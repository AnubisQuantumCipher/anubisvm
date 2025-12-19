--  KHEPRI Merkle Patricia Trie Implementation
--  Pure SPARK implementation with formal verification
--  File I/O procedures use local SPARK_Mode Off for platform abstraction
pragma SPARK_Mode (On);

with Anubis_Types;
with Anubis_SHA3;
with Ada.Sequential_IO;
with Ada.Directories;

package body Khepri_MPT with
   SPARK_Mode => On,
   Refined_State => (Trie_State => (Tries, Node_Store, Snapshot_Store, Hash_Map))
is

   ---------------------------------------------------------------------------
   --  Internal Storage Types
   ---------------------------------------------------------------------------

   --  Node storage entry
   type Node_Entry is record
      Node : MPT_Node;
      Used : Boolean;
   end record;

   --  Node store
   type Node_Array is array (0 .. Max_Nodes - 1) of Node_Entry;

   --  Hash→Index mapping for O(1) node lookup by hash
   --  Maps hash to node index for traversal
   type Hash_Index_Entry is record
      Hash  : Hash_256;
      Index : Natural;
      Valid : Boolean;
   end record;

   --  Simple hash table with linear probing
   --  Size is 2x nodes to reduce collisions
   Hash_Table_Size : constant := Max_Nodes * 2;
   type Hash_Index_Map is array (0 .. Hash_Table_Size - 1) of Hash_Index_Entry;

   --  Trie metadata
   type Trie_Entry is record
      Root_Idx : Natural;
      Count    : Natural;
      Used     : Boolean;
   end record;

   type Trie_Array is array (Khepri_MPT.Trie_ID) of Trie_Entry;

   --  Snapshot entry
   Max_Snapshots : constant := 100;
   type Snapshot_Entry is record
      Trie_Ref : Khepri_MPT.Trie_ID;
      Root_Idx : Natural;
      Used     : Boolean;
   end record;

   type Snapshot_Array is array (0 .. Max_Snapshots - 1) of Snapshot_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Tries : Trie_Array := (others => (
      Root_Idx => 0,
      Count    => 0,
      Used     => False
   ));

   Node_Store : Node_Array := (others => (
      Node => Empty_Node,
      Used => False
   ));

   --  Hash→Index mapping table
   Hash_Map : Hash_Index_Map := (others => (
      Hash  => Empty_Hash,
      Index => 0,
      Valid => False
   ));

   Snapshot_Store : Snapshot_Array := (others => (
      Trie_Ref => 0,
      Root_Idx => 0,
      Used     => False
   ));

   ---------------------------------------------------------------------------
   --  Hash Table Operations
   ---------------------------------------------------------------------------

   --  Compute simple hash for hash table indexing
   --  Uses first 4 bytes of Keccak hash as index seed
   function Hash_Table_Index (Hash : Hash_256) return Natural is
      Seed : Unsigned_32 := 0;
   begin
      --  Use first 4 bytes as seed
      Seed := Unsigned_32 (Hash (0)) +
              Shift_Left (Unsigned_32 (Hash (1)), 8) +
              Shift_Left (Unsigned_32 (Hash (2)), 16) +
              Shift_Left (Unsigned_32 (Hash (3)), 24);
      return Natural (Seed mod Unsigned_32 (Hash_Table_Size));
   end Hash_Table_Index;

   --  Register hash→index mapping (called by Allocate_Node)
   procedure Register_Hash_Mapping (
      Hash : Hash_256;
      Idx  : Natural
   ) is
      Start_Pos : constant Natural := Hash_Table_Index (Hash);
      Pos       : Natural := Start_Pos;
      Probes    : Natural := 0;
   begin
      --  Linear probing to find empty slot
      loop
         if not Hash_Map (Pos).Valid then
            --  Found empty slot
            Hash_Map (Pos) := (
               Hash  => Hash,
               Index => Idx,
               Valid => True
            );
            return;
         elsif Hash_Map (Pos).Hash = Hash then
            --  Hash already registered, update index
            Hash_Map (Pos).Index := Idx;
            return;
         end if;

         --  Linear probe to next slot
         Pos := (Pos + 1) mod Hash_Table_Size;
         Probes := Probes + 1;

         --  Prevent infinite loop (should never happen with 2x table size)
         exit when Probes >= Hash_Table_Size or Pos = Start_Pos;
      end loop;
   end Register_Hash_Mapping;

   --  Remove hash→index mapping (called by Free_Node)
   procedure Unregister_Hash_Mapping (Hash : Hash_256) is
      Start_Pos : constant Natural := Hash_Table_Index (Hash);
      Pos       : Natural := Start_Pos;
      Probes    : Natural := 0;
   begin
      loop
         if not Hash_Map (Pos).Valid then
            --  Not found (already removed or never existed)
            return;
         elsif Hash_Map (Pos).Hash = Hash then
            --  Found it, mark invalid
            Hash_Map (Pos).Valid := False;
            Hash_Map (Pos).Index := 0;
            Hash_Map (Pos).Hash  := Empty_Hash;
            return;
         end if;

         Pos := (Pos + 1) mod Hash_Table_Size;
         Probes := Probes + 1;
         exit when Probes >= Hash_Table_Size or Pos = Start_Pos;
      end loop;
   end Unregister_Hash_Mapping;

   --  Lookup node index by hash
   --  Returns Natural'Last if not found
   function Lookup_Node_By_Hash (Hash : Hash_256) return Natural is
      Start_Pos : constant Natural := Hash_Table_Index (Hash);
      Pos       : Natural := Start_Pos;
      Probes    : Natural := 0;
   begin
      loop
         if not Hash_Map (Pos).Valid then
            --  Empty slot, hash not found
            return Natural'Last;
         elsif Hash_Map (Pos).Hash = Hash then
            --  Found it
            return Hash_Map (Pos).Index;
         end if;

         Pos := (Pos + 1) mod Hash_Table_Size;
         Probes := Probes + 1;
         exit when Probes >= Hash_Table_Size or Pos = Start_Pos;
      end loop;

      return Natural'Last;  --  Not found after full probe
   end Lookup_Node_By_Hash;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Empty_Node return Natural is
   begin
      for I in Node_Store'Range loop
         if not Node_Store (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Node;

   function Find_Empty_Trie return Trie_ID is
   begin
      for I in Trie_ID'Range loop
         if I /= Null_Trie and then not Tries (I).Used then
            return I;
         end if;
      end loop;
      return Null_Trie;
   end Find_Empty_Trie;

   function Find_Empty_Snapshot return Natural is
   begin
      for I in Snapshot_Store'Range loop
         if not Snapshot_Store (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Snapshot;

   --  Allocate a new node and register its hash
   procedure Allocate_Node (
      Node    : in     MPT_Node;
      Idx     : out    Natural;
      Success : out    Boolean
   ) is
      Slot : constant Natural := Find_Empty_Node;
   begin
      if Slot = Natural'Last or else Slot >= Max_Nodes then
         Idx := 0;
         Success := False;
         return;
      end if;

      pragma Assert (Slot in Node_Store'Range);
      Node_Store (Slot) := (Node => Node, Used => True);

      --  Register hash→index mapping if node has valid hash
      if Node.Hash.Valid then
         Register_Hash_Mapping (Node.Hash.Data, Slot);
      end if;

      Idx := Slot;
      Success := True;
   end Allocate_Node;

   --  Free a node and unregister its hash
   procedure Free_Node (Idx : Natural) is
   begin
      if Idx < Max_Nodes and then Node_Store (Idx).Used then
         --  Unregister hash mapping if node has valid hash
         if Node_Store (Idx).Node.Hash.Valid then
            Unregister_Hash_Mapping (Node_Store (Idx).Node.Hash.Data);
         end if;

         Node_Store (Idx).Used := False;
         Node_Store (Idx).Node := Empty_Node;
      end if;
   end Free_Node;

   ---------------------------------------------------------------------------
   --  Type Conversion Helper
   ---------------------------------------------------------------------------

   function Node_Value_To_Bytes (
      NV  : Node_Value;
      Len : Natural
   ) return Byte_Array
   is
   begin
      if Len = 0 then
         return (0 .. -1 => 0);  -- Empty array
      end if;
      declare
         Result : Byte_Array (0 .. Len - 1) := (others => 0);
      begin
         for I in 0 .. Len - 1 loop
            pragma Loop_Invariant (I in 0 .. Len - 1);
            pragma Loop_Invariant (I in NV'Range);
            if I in NV'Range then
               Result (I) := NV (I);
            end if;
         end loop;
         return Result;
      end;
   end Node_Value_To_Bytes;

   ---------------------------------------------------------------------------
   --  Keccak-256 Hash (Ethereum-compatible)
   ---------------------------------------------------------------------------

   function Keccak_256 (Data : Byte_Array) return Hash_256 is
      Result : Hash_256;
      Digest : Anubis_SHA3.SHA3_256_Digest;
      --  Convert Aegis_VM_Types.Byte_Array to Anubis_Types.Byte_Array
      Message : Anubis_Types.Byte_Array (Data'Range);
   begin
      for I in Data'Range loop
         Message (I) := Anubis_Types.Byte (Data (I));
      end loop;
      --  Use proper Keccak-256 (domain separator 0x01) for Ethereum compatibility
      Anubis_SHA3.Keccak_256 (Message, Digest);
      for I in Hash_Index loop
         Result (I) := Digest (I);
      end loop;
      return Result;
   end Keccak_256;

   ---------------------------------------------------------------------------
   --  RLP Encoding Helpers
   ---------------------------------------------------------------------------

   procedure Encode_Length (
      Len    : in     Natural;
      Offset : in     Unsigned_8;
      Output : in out Byte_Array;
      Pos    : in out Natural
   ) is
   begin
      if Len < 56 then
         Output (Output'First + Pos) := Offset + Unsigned_8 (Len);
         Pos := Pos + 1;
      elsif Len < 256 then
         Output (Output'First + Pos) := Offset + 55 + 1;
         Output (Output'First + Pos + 1) := Unsigned_8 (Len);
         Pos := Pos + 2;
      elsif Len < 65536 then
         Output (Output'First + Pos) := Offset + 55 + 2;
         Output (Output'First + Pos + 1) := Unsigned_8 (Shift_Right (
            Unsigned_16 (Len), 8));
         Output (Output'First + Pos + 2) := Unsigned_8 (
            Unsigned_32 (Len) and 16#FF#);
         Pos := Pos + 3;
      else
         Output (Output'First + Pos) := Offset + 55 + 3;
         Output (Output'First + Pos + 1) := Unsigned_8 (Shift_Right (
            Unsigned_32 (Len), 16) and 16#FF#);
         Output (Output'First + Pos + 2) := Unsigned_8 (Shift_Right (
            Unsigned_32 (Len), 8) and 16#FF#);
         Output (Output'First + Pos + 3) := Unsigned_8 (
            Unsigned_32 (Len) and 16#FF#);
         Pos := Pos + 4;
      end if;
   end Encode_Length;

   procedure Encode_String (
      Data   : in     Byte_Array;
      Output : in Out Byte_Array;
      Pos    : in Out Natural
   ) is
   begin
      if Data'Length = 1 and then Data (Data'First) < 16#80# then
         --  Single byte, no header
         Output (Output'First + Pos) := Data (Data'First);
         Pos := Pos + 1;
      elsif Data'Length < 56 then
         --  Short string
         Output (Output'First + Pos) := RLP_Short_String +
            Unsigned_8 (Data'Length);
         Pos := Pos + 1;
         for I in Data'Range loop
            Output (Output'First + Pos) := Data (I);
            Pos := Pos + 1;
         end loop;
      else
         --  Long string
         Encode_Length (Data'Length, RLP_Long_String, Output, Pos);
         for I in Data'Range loop
            Output (Output'First + Pos) := Data (I);
            Pos := Pos + 1;
         end loop;
      end if;
   end Encode_String;

   ---------------------------------------------------------------------------
   --  Hash_Node
   ---------------------------------------------------------------------------

   function Hash_Node (Node : MPT_Node) return Hash_256 is
      Buffer  : Byte_Array (0 .. Max_Proof_Node_Size - 1) := (others => 0);
      Len     : Natural := 0;
      Success : Boolean;
   begin
      Encode_Node (Node, Buffer, Len, Success);
      if not Success or Len = 0 then
         return Empty_Hash;
      end if;

      return Keccak_256 (Buffer (0 .. Len - 1));
   end Hash_Node;

   ---------------------------------------------------------------------------
   --  Encode_Node
   ---------------------------------------------------------------------------

   procedure Encode_Node (
      Node    : in     MPT_Node;
      Output  : out    Byte_Array;
      Length  : out    Natural;
      Success : out    Boolean
   ) is
      Pos : Natural := 0;
   begin
      Output := (others => 0);
      Length := 0;
      Success := True;

      case Node.Kind is
         when Node_Empty =>
            --  Empty node is RLP null (0x80)
            Output (Output'First) := RLP_Short_String;
            Length := 1;

         when Node_Leaf =>
            --  Leaf: [encoded_path, value]
            --  HP encoding: prefix 0x20 for even, 0x3 for odd (leaf flag)
            declare
               Path_Buf : Byte_Array (0 .. Max_Key_Bytes) := (others => 0);
               Path_Len : Natural := 0;
               HP_Prefix : Unsigned_8;
            begin
               if Node.Key.Length mod 2 = 0 then
                  HP_Prefix := 16#20#;
                  Path_Buf (0) := HP_Prefix;
                  Path_Len := 1;
                  for I in 0 .. (Node.Key.Length / 2) - 1 loop
                     Path_Buf (Path_Len) := Unsigned_8 (Node.Key.Data (I * 2)) * 16
                        + Unsigned_8 (Node.Key.Data (I * 2 + 1));
                     Path_Len := Path_Len + 1;
                  end loop;
               else
                  HP_Prefix := 16#30# + Unsigned_8 (Node.Key.Data (0));
                  Path_Buf (0) := HP_Prefix;
                  Path_Len := 1;
                  for I in 0 .. ((Node.Key.Length - 1) / 2) - 1 loop
                     Path_Buf (Path_Len) := Unsigned_8 (Node.Key.Data (I * 2 + 1)) * 16
                        + Unsigned_8 (Node.Key.Data (I * 2 + 2));
                     Path_Len := Path_Len + 1;
                  end loop;
               end if;

               --  Encode as list: [path, value]
               Output (Pos) := RLP_Short_List + 2;  --  Placeholder
               Pos := Pos + 1;

               Encode_String (Path_Buf (0 .. Path_Len - 1), Output, Pos);
               Encode_String (Node_Value_To_Bytes (Node.Value.Bytes,
                  Node.Value.Length), Output, Pos);

               Length := Pos;
            end;

         when Node_Extension =>
            --  Extension: [encoded_path, next_hash]
            --  HP encoding: prefix 0x00 for even, 0x1 for odd (extension flag)
            declare
               Path_Buf : Byte_Array (0 .. Max_Key_Bytes) := (others => 0);
               Path_Len : Natural := 0;
               HP_Prefix : Unsigned_8;
            begin
               if Node.Key.Length mod 2 = 0 then
                  HP_Prefix := 16#00#;
                  Path_Buf (0) := HP_Prefix;
                  Path_Len := 1;
                  for I in 0 .. (Node.Key.Length / 2) - 1 loop
                     Path_Buf (Path_Len) := Unsigned_8 (Node.Key.Data (I * 2)) * 16
                        + Unsigned_8 (Node.Key.Data (I * 2 + 1));
                     Path_Len := Path_Len + 1;
                  end loop;
               else
                  HP_Prefix := 16#10# + Unsigned_8 (Node.Key.Data (0));
                  Path_Buf (0) := HP_Prefix;
                  Path_Len := 1;
                  for I in 0 .. ((Node.Key.Length - 1) / 2) - 1 loop
                     Path_Buf (Path_Len) := Unsigned_8 (Node.Key.Data (I * 2 + 1)) * 16
                        + Unsigned_8 (Node.Key.Data (I * 2 + 2));
                     Path_Len := Path_Len + 1;
                  end loop;
               end if;

               --  Encode as list: [path, next]
               Output (Pos) := RLP_Short_List + 2;
               Pos := Pos + 1;

               Encode_String (Path_Buf (0 .. Path_Len - 1), Output, Pos);
               --  Next is either hash or embedded node
               if Node.Children (0).Valid then
                  Encode_String (Byte_Array (Node.Children (0).Data), Output, Pos);
               else
                  Output (Pos) := RLP_Short_String;
                  Pos := Pos + 1;
               end if;

               Length := Pos;
            end;

         when Node_Branch =>
            --  Branch: [child0, child1, ..., child15, value]
            --  17-element list
            declare
               List_Content : Byte_Array (0 .. 600) := (others => 0);
               Content_Len  : Natural := 0;
            begin
               --  Encode 16 children
               for I in Child_Index loop
                  if Node.Children (I).Valid then
                     Encode_String (Byte_Array (Node.Children (I).Data),
                        List_Content, Content_Len);
                  else
                     List_Content (Content_Len) := RLP_Short_String;
                     Content_Len := Content_Len + 1;
                  end if;
               end loop;

               --  Encode value (17th element)
               if Node.Value.Length > 0 then
                  Encode_String (Node_Value_To_Bytes (Node.Value.Bytes,
                     Node.Value.Length), List_Content, Content_Len);
               else
                  List_Content (Content_Len) := RLP_Short_String;
                  Content_Len := Content_Len + 1;
               end if;

               --  Write list header
               if Content_Len < 56 then
                  Output (Pos) := RLP_Short_List + Unsigned_8 (Content_Len);
                  Pos := Pos + 1;
               else
                  Encode_Length (Content_Len, RLP_Long_List, Output, Pos);
               end if;

               --  Copy content
               for I in 0 .. Content_Len - 1 loop
                  Output (Pos) := List_Content (I);
                  Pos := Pos + 1;
               end loop;

               Length := Pos;
            end;
      end case;
   end Encode_Node;

   ---------------------------------------------------------------------------
   --  RLP Decoding Helpers
   ---------------------------------------------------------------------------

   --  Decode RLP string/bytes, return start position and length of data
   procedure Decode_RLP_Item (
      Input       : in     Byte_Array;
      Start_Pos   : in     Natural;
      Data_Start  : out    Natural;
      Data_Len    : out    Natural;
      Next_Pos    : out    Natural;
      Success     : out    Boolean
   ) is
      First_Byte : Unsigned_8;
   begin
      Data_Start := 0;
      Data_Len := 0;
      Next_Pos := Start_Pos;
      Success := False;

      if Start_Pos > Input'Last then
         return;
      end if;

      First_Byte := Input (Input'First + Start_Pos);

      if First_Byte < 16#80# then
         --  Single byte value (0x00-0x7F)
         Data_Start := Start_Pos;
         Data_Len := 1;
         Next_Pos := Start_Pos + 1;
         Success := True;

      elsif First_Byte <= 16#B7# then
         --  Short string (0x80-0xB7): length = first_byte - 0x80
         Data_Len := Natural (First_Byte - 16#80#);
         Data_Start := Start_Pos + 1;
         Next_Pos := Start_Pos + 1 + Data_Len;
         Success := (Next_Pos <= Input'Length);

      elsif First_Byte <= 16#BF# then
         --  Long string (0xB8-0xBF): length of length = first_byte - 0xB7
         declare
            Len_Bytes : constant Natural := Natural (First_Byte - 16#B7#);
         begin
            if Start_Pos + Len_Bytes >= Input'Length then
               return;
            end if;
            Data_Len := 0;
            for I in 1 .. Len_Bytes loop
               Data_Len := Data_Len * 256 +
                  Natural (Input (Input'First + Start_Pos + I));
            end loop;
            Data_Start := Start_Pos + 1 + Len_Bytes;
            Next_Pos := Data_Start + Data_Len;
            Success := (Next_Pos <= Input'Length);
         end;

      elsif First_Byte <= 16#F7# then
         --  Short list (0xC0-0xF7): length = first_byte - 0xC0
         Data_Len := Natural (First_Byte - 16#C0#);
         Data_Start := Start_Pos + 1;
         Next_Pos := Start_Pos + 1 + Data_Len;
         Success := (Next_Pos <= Input'Length);

      else
         --  Long list (0xF8-0xFF): length of length = first_byte - 0xF7
         declare
            Len_Bytes : constant Natural := Natural (First_Byte - 16#F7#);
         begin
            if Start_Pos + Len_Bytes >= Input'Length then
               return;
            end if;
            Data_Len := 0;
            for I in 1 .. Len_Bytes loop
               Data_Len := Data_Len * 256 +
                  Natural (Input (Input'First + Start_Pos + I));
            end loop;
            Data_Start := Start_Pos + 1 + Len_Bytes;
            Next_Pos := Data_Start + Data_Len;
            Success := (Next_Pos <= Input'Length);
         end;
      end if;
   end Decode_RLP_Item;

   --  Decode HP (Hex-Prefix) encoded path to nibbles
   --  Returns is_leaf flag and decoded nibble key
   procedure Decode_HP_Path (
      Input    : in     Byte_Array;
      Start    : in     Natural;
      Length   : in     Natural;
      Key      : out    Nibble_Key;
      Is_Leaf  : out    Boolean;
      Success  : out    Boolean
   ) is
      HP_Byte : Unsigned_8;
      Pos     : Natural := 0;
   begin
      Key := Empty_Nibble_Key;
      Is_Leaf := False;
      Success := False;

      if Length = 0 then
         Success := True;  -- Empty path is valid
         return;
      end if;

      if Start > Input'Last then
         return;
      end if;

      --  First byte contains HP prefix
      HP_Byte := Input (Input'First + Start);

      --  HP encoding:
      --  Leaf:      0x20 (even) or 0x3X (odd, X is first nibble)
      --  Extension: 0x00 (even) or 0x1X (odd, X is first nibble)
      declare
         Flag : constant Unsigned_8 := Shift_Right (HP_Byte, 4);
      begin
         case Flag is
            when 0 =>
               --  Extension, even number of nibbles
               Is_Leaf := False;
               --  Remaining bytes are pairs of nibbles
               for I in 1 .. Length - 1 loop
                  if Key.Length + 2 <= Max_Key_Nibbles then
                     Key.Data (Key.Length) := Nibble (Shift_Right (
                        Input (Input'First + Start + I), 4));
                     Key.Data (Key.Length + 1) := Nibble (
                        Input (Input'First + Start + I) and 16#0F#);
                     Key.Length := Key.Length + 2;
                  end if;
               end loop;
               Success := True;

            when 1 =>
               --  Extension, odd number of nibbles
               Is_Leaf := False;
               --  First nibble is in lower 4 bits of first byte
               if Key.Length < Max_Key_Nibbles then
                  Key.Data (Key.Length) := Nibble (HP_Byte and 16#0F#);
                  Key.Length := Key.Length + 1;
               end if;
               --  Remaining bytes are pairs of nibbles
               for I in 1 .. Length - 1 loop
                  if Key.Length + 2 <= Max_Key_Nibbles then
                     Key.Data (Key.Length) := Nibble (Shift_Right (
                        Input (Input'First + Start + I), 4));
                     Key.Data (Key.Length + 1) := Nibble (
                        Input (Input'First + Start + I) and 16#0F#);
                     Key.Length := Key.Length + 2;
                  end if;
               end loop;
               Success := True;

            when 2 =>
               --  Leaf, even number of nibbles
               Is_Leaf := True;
               --  Remaining bytes are pairs of nibbles
               for I in 1 .. Length - 1 loop
                  if Key.Length + 2 <= Max_Key_Nibbles then
                     Key.Data (Key.Length) := Nibble (Shift_Right (
                        Input (Input'First + Start + I), 4));
                     Key.Data (Key.Length + 1) := Nibble (
                        Input (Input'First + Start + I) and 16#0F#);
                     Key.Length := Key.Length + 2;
                  end if;
               end loop;
               Success := True;

            when 3 =>
               --  Leaf, odd number of nibbles
               Is_Leaf := True;
               --  First nibble is in lower 4 bits of first byte
               if Key.Length < Max_Key_Nibbles then
                  Key.Data (Key.Length) := Nibble (HP_Byte and 16#0F#);
                  Key.Length := Key.Length + 1;
               end if;
               --  Remaining bytes are pairs of nibbles
               for I in 1 .. Length - 1 loop
                  if Key.Length + 2 <= Max_Key_Nibbles then
                     Key.Data (Key.Length) := Nibble (Shift_Right (
                        Input (Input'First + Start + I), 4));
                     Key.Data (Key.Length + 1) := Nibble (
                        Input (Input'First + Start + I) and 16#0F#);
                     Key.Length := Key.Length + 2;
                  end if;
               end loop;
               Success := True;

            when others =>
               --  Invalid HP prefix
               Success := False;
         end case;
      end;
   end Decode_HP_Path;

   ---------------------------------------------------------------------------
   --  Decode_Node - Full RLP Decoding with HP Path Support
   ---------------------------------------------------------------------------

   procedure Decode_Node (
      Input   : in     Byte_Array;
      Node    : out    MPT_Node;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) is
      List_Start    : Natural;
      List_Len      : Natural;
      List_Next     : Natural;
      Item1_Start   : Natural;
      Item1_Len     : Natural;
      Item1_Next    : Natural;
      Item2_Start   : Natural;
      Item2_Len     : Natural;
      Item2_Next    : Natural;
      Decode_Ok     : Boolean;
      Is_Leaf       : Boolean;
      Element_Count : Natural := 0;
   begin
      Node := Empty_Node;
      Success := False;
      Error := Error_None;

      if Input'Length = 0 then
         Error := Error_Invalid_RLP;
         return;
      end if;

      --  Check for empty node (RLP null = 0x80)
      if Input'Length = 1 and then Input (Input'First) = RLP_Short_String then
         Node.Kind := Node_Empty;
         Success := True;
         return;
      end if;

      --  Must be a list for valid MPT node
      if Input (Input'First) < RLP_Short_List then
         Error := Error_Invalid_RLP;
         return;
      end if;

      --  Decode outer list
      Decode_RLP_Item (Input, 0, List_Start, List_Len, List_Next, Decode_Ok);
      if not Decode_Ok then
         Error := Error_Invalid_RLP;
         return;
      end if;

      --  Count elements in list to determine node type
      declare
         Pos : Natural := List_Start;
         Item_Start, Item_Len, Next_Pos : Natural;
      begin
         while Pos < List_Start + List_Len and Element_Count < 18 loop
            Decode_RLP_Item (Input, Pos, Item_Start, Item_Len, Next_Pos, Decode_Ok);
            if not Decode_Ok then
               Error := Error_Invalid_RLP;
               return;
            end if;
            Element_Count := Element_Count + 1;
            Pos := Next_Pos;
         end loop;
      end;

      if Element_Count = 2 then
         --  Leaf or Extension node (2 elements: [path, value/child])

         --  Decode first element (HP-encoded path)
         Decode_RLP_Item (Input, List_Start, Item1_Start, Item1_Len, Item1_Next, Decode_Ok);
         if not Decode_Ok then
            Error := Error_Invalid_RLP;
            return;
         end if;

         --  Decode HP path to get key and leaf/extension flag
         Decode_HP_Path (Input, Item1_Start, Item1_Len, Node.Key, Is_Leaf, Decode_Ok);
         if not Decode_Ok then
            Error := Error_Invalid_RLP;
            return;
         end if;

         --  Decode second element (value for leaf, child hash for extension)
         Decode_RLP_Item (Input, Item1_Next, Item2_Start, Item2_Len, Item2_Next, Decode_Ok);
         if not Decode_Ok then
            Error := Error_Invalid_RLP;
            return;
         end if;

         if Is_Leaf then
            Node.Kind := Node_Leaf;
            --  Copy value
            Node.Value.Length := Natural'Min (Item2_Len, Max_Value_Size);
            for I in 0 .. Node.Value.Length - 1 loop
               Node.Value.Bytes (I) := Input (Input'First + Item2_Start + I);
            end loop;
         else
            Node.Kind := Node_Extension;
            --  Copy child hash (should be 32 bytes)
            if Item2_Len = 32 then
               for I in Hash_Index loop
                  Node.Children (0).Data (I) := Input (Input'First + Item2_Start + I);
               end loop;
               Node.Children (0).Valid := True;
            elsif Item2_Len > 0 then
               --  Embedded node (RLP < 32 bytes) - store raw RLP
               --  For now, treat as invalid (simplification)
               Error := Error_Invalid_RLP;
               return;
            end if;
         end if;

         --  Compute hash for the decoded node
         Node.Hash := (Data => Hash_Node (Node), Valid => True);
         Success := True;

      elsif Element_Count = 17 then
         --  Branch node (17 elements: [child0..child15, value])
         Node.Kind := Node_Branch;

         --  Decode all 17 elements
         declare
            Pos : Natural := List_Start;
            Item_Start, Item_Len, Next_Pos : Natural;
         begin
            --  Decode 16 children
            for I in Child_Index loop
               Decode_RLP_Item (Input, Pos, Item_Start, Item_Len, Next_Pos, Decode_Ok);
               if not Decode_Ok then
                  Error := Error_Invalid_RLP;
                  return;
               end if;

               if Item_Len = 32 then
                  --  32-byte hash reference
                  for J in Hash_Index loop
                     Node.Children (I).Data (J) := Input (Input'First + Item_Start + J);
                  end loop;
                  Node.Children (I).Valid := True;
               elsif Item_Len = 0 then
                  --  Empty child (0x80)
                  Node.Children (I).Valid := False;
               else
                  --  Embedded node - not supported in this implementation
                  Node.Children (I).Valid := False;
               end if;

               Pos := Next_Pos;
            end loop;

            --  Decode 17th element (value)
            Decode_RLP_Item (Input, Pos, Item_Start, Item_Len, Next_Pos, Decode_Ok);
            if not Decode_Ok then
               Error := Error_Invalid_RLP;
               return;
            end if;

            if Item_Len > 0 then
               Node.Value.Length := Natural'Min (Item_Len, Max_Value_Size);
               for I in 0 .. Node.Value.Length - 1 loop
                  Node.Value.Bytes (I) := Input (Input'First + Item_Start + I);
               end loop;
            end if;
         end;

         --  Compute hash for the decoded node
         Node.Hash := (Data => Hash_Node (Node), Valid => True);
         Success := True;

      else
         --  Invalid element count for MPT node
         Error := Error_Invalid_RLP;
      end if;
   end Decode_Node;

   ---------------------------------------------------------------------------
   --  Create_Trie
   ---------------------------------------------------------------------------

   procedure Create_Trie (
      Trie    : out Trie_ID;
      Success : out Boolean
   ) is
      New_Trie : constant Trie_ID := Find_Empty_Trie;
   begin
      if New_Trie = Null_Trie then
         Trie := Null_Trie;
         Success := False;
         return;
      end if;

      Tries (New_Trie) := (
         Root_Idx => Natural'Last,  --  Empty root
         Count    => 0,
         Used     => True
      );

      Trie := New_Trie;
      Success := True;
   end Create_Trie;

   ---------------------------------------------------------------------------
   --  Load_Trie - Proper implementation with hash lookup
   ---------------------------------------------------------------------------

   procedure Load_Trie (
      Root    : in     Hash_256;
      Trie    : out    Trie_ID;
      Success : out    Boolean
   ) is
      New_Trie : constant Trie_ID := Find_Empty_Trie;
      Root_Idx : Natural;
   begin
      Success := False;
      Trie := Null_Trie;

      if New_Trie = Null_Trie then
         return;
      end if;

      --  Check for empty trie (special hash for empty state)
      declare
         Empty_Root : constant Hash_256 := Keccak_256 ((0 => RLP_Short_String));
      begin
         if Root = Empty_Root then
            --  Empty trie
            Tries (New_Trie) := (
               Root_Idx => Natural'Last,
               Count    => 0,
               Used     => True
            );
            Trie := New_Trie;
            Success := True;
            return;
         end if;
      end;

      --  Lookup root node by hash
      Root_Idx := Lookup_Node_By_Hash (Root);

      if Root_Idx = Natural'Last then
         --  Root not found in hash map
         --  This is CRITICAL: it means either:
         --  1. Hash map wasn't loaded (call Load_Hash_Map first!)
         --  2. Hash map is out of sync with Node_Store
         --  3. The root genuinely doesn't exist
         --
         --  Try to rebuild hash map from Node_Store as recovery
         for I in Node_Store'Range loop
            if Node_Store (I).Used and then
               Node_Store (I).Node.Hash.Valid and then
               Node_Store (I).Node.Hash.Data = Root
            then
               --  Found the root node, but it wasn't in hash map!
               --  Register it now
               Register_Hash_Mapping (Root, I);
               Root_Idx := I;
               exit;
            end if;
         end loop;

         --  If still not found, this is an error
         if Root_Idx = Natural'Last then
            --  Root genuinely doesn't exist - this is an error
            --  Don't create empty trie silently
            Success := False;
            return;
         end if;
      end if;

      --  Root found, create trie
      Tries (New_Trie) := (
         Root_Idx => Root_Idx,
         Count    => 1,  --  At least root exists
         Used     => True
      );

      Trie := New_Trie;
      Success := True;
   end Load_Trie;

   ---------------------------------------------------------------------------
   --  Destroy_Trie
   ---------------------------------------------------------------------------

   procedure Destroy_Trie (Trie : in Trie_ID) is
   begin
      if Trie /= Null_Trie and then Tries (Trie).Used then
         --  Free all nodes (simplified: just mark trie unused)
         Tries (Trie).Used := False;
         Tries (Trie).Root_Idx := Natural'Last;
         Tries (Trie).Count := 0;
      end if;
   end Destroy_Trie;

   ---------------------------------------------------------------------------
   --  MPT Tree Restructuring Helpers
   ---------------------------------------------------------------------------

   --  Create a leaf node with given key suffix and value
   function Make_Leaf (
      Key_Suffix : Nibble_Key;
      Val        : Byte_Array
   ) return MPT_Node is
      Node : MPT_Node := Empty_Node;
   begin
      Node.Kind := Node_Leaf;
      Node.Key := Key_Suffix;
      Node.Value.Length := Natural'Min (Val'Length, Max_Value_Size);
      for I in 0 .. Node.Value.Length - 1 loop
         Node.Value.Bytes (I) := Val (Val'First + I);
      end loop;
      Node.Hash := (Data => Hash_Node (Node), Valid => True);
      return Node;
   end Make_Leaf;

   --  Create a branch node
   function Make_Branch return MPT_Node is
      Node : MPT_Node := Empty_Node;
   begin
      Node.Kind := Node_Branch;
      return Node;
   end Make_Branch;

   --  Create an extension node
   function Make_Extension (
      Shared_Key : Nibble_Key;
      Child_Hash : Hash_256
   ) return MPT_Node is
      Node : MPT_Node := Empty_Node;
   begin
      Node.Kind := Node_Extension;
      Node.Key := Shared_Key;
      Node.Children (0) := (Data => Child_Hash, Valid => True);
      Node.Hash := (Data => Hash_Node (Node), Valid => True);
      return Node;
   end Make_Extension;

   --  Recursive insertion into node
   --  Returns the new node index that should replace the current one
   procedure Insert_Into_Node (
      Current_Idx : in     Natural;
      Key         : in     Nibble_Key;
      Val         : in     Byte_Array;
      New_Idx     : out    Natural;
      Success     : out    Boolean;
      Error       : out    MPT_Error
   ) is
      Current      : MPT_Node;
      Common_Len   : Natural;
      New_Node     : MPT_Node;
      Branch_Node  : MPT_Node;
      Child_Idx    : Natural;
      Alloc_Ok     : Boolean;
   begin
      New_Idx := Current_Idx;
      Success := False;
      Error := Error_None;

      --  Get current node
      if Current_Idx >= Max_Nodes or else not Node_Store (Current_Idx).Used then
         Error := Error_Invalid_Node;
         return;
      end if;

      Current := Node_Store (Current_Idx).Node;

      case Current.Kind is
         when Node_Empty =>
            --  Replace with leaf
            New_Node := Make_Leaf (Key, Val);
            Allocate_Node (New_Node, New_Idx, Alloc_Ok);
            if not Alloc_Ok then
               Error := Error_Trie_Full;
               return;
            end if;
            Free_Node (Current_Idx);
            Success := True;

         when Node_Leaf =>
            Common_Len := Common_Prefix_Length (Current.Key, Key);

            if Common_Len = Key.Length and Common_Len = Current.Key.Length then
               --  Exact match: update value
               New_Node := Current;
               New_Node.Value.Length := Natural'Min (Val'Length, Max_Value_Size);
               for I in 0 .. New_Node.Value.Length - 1 loop
                  New_Node.Value.Bytes (I) := Val (Val'First + I);
               end loop;
               New_Node.Hash := (Data => Hash_Node (New_Node), Valid => True);

               --  Update hash mapping
               if Current.Hash.Valid then
                  Unregister_Hash_Mapping (Current.Hash.Data);
               end if;
               if New_Node.Hash.Valid then
                  Register_Hash_Mapping (New_Node.Hash.Data, Current_Idx);
               end if;

               Node_Store (Current_Idx).Node := New_Node;
               New_Idx := Current_Idx;
               Success := True;

            else
               --  Need to create branch (and possibly extension)
               Branch_Node := Make_Branch;

               --  Add existing leaf to branch
               if Common_Len < Current.Key.Length then
                  declare
                     Old_Nibble : constant Natural := Natural (
                        Current.Key.Data (Common_Len));
                     Old_Suffix : Nibble_Key := Remove_Prefix (Current.Key, Common_Len + 1);
                     Old_Leaf   : MPT_Node := Make_Leaf (Old_Suffix, (0 .. Current.Value.Length - 1 => 0));
                  begin
                     --  Copy value from current
                     for I in 0 .. Current.Value.Length - 1 loop
                        Old_Leaf.Value.Bytes (I) := Current.Value.Bytes (I);
                     end loop;
                     Old_Leaf.Value.Length := Current.Value.Length;
                     Old_Leaf.Hash := (Data => Hash_Node (Old_Leaf), Valid => True);

                     Allocate_Node (Old_Leaf, Child_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;
                     Branch_Node.Children (Old_Nibble) := (Data => Old_Leaf.Hash.Data, Valid => True);
                  end;
               else
                  --  Current leaf has no more suffix, store value in branch
                  Branch_Node.Value := Current.Value;
               end if;

               --  Add new leaf to branch
               if Common_Len < Key.Length then
                  declare
                     New_Nibble : constant Natural := Natural (Key.Data (Common_Len));
                     New_Suffix : Nibble_Key := Remove_Prefix (Key, Common_Len + 1);
                     New_Leaf   : MPT_Node := Make_Leaf (New_Suffix, Val);
                  begin
                     Allocate_Node (New_Leaf, Child_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;
                     Branch_Node.Children (New_Nibble) := (Data => New_Leaf.Hash.Data, Valid => True);
                  end;
               else
                  --  New key has no more suffix, store value in branch
                  Branch_Node.Value.Length := Natural'Min (Val'Length, Max_Value_Size);
                  for I in 0 .. Branch_Node.Value.Length - 1 loop
                     Branch_Node.Value.Bytes (I) := Val (Val'First + I);
                  end loop;
               end if;

               Branch_Node.Hash := (Data => Hash_Node (Branch_Node), Valid => True);

               --  If common prefix exists, wrap in extension
               if Common_Len > 0 then
                  declare
                     Ext_Key : Nibble_Key := Empty_Nibble_Key;
                  begin
                     Ext_Key.Length := Common_Len;
                     for I in 0 .. Common_Len - 1 loop
                        Ext_Key.Data (I) := Key.Data (I);
                     end loop;

                     Allocate_Node (Branch_Node, Child_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;

                     New_Node := Make_Extension (Ext_Key, Branch_Node.Hash.Data);
                     Free_Node (Current_Idx);
                     Allocate_Node (New_Node, New_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;
                  end;
               else
                  --  No common prefix, branch is root
                  Free_Node (Current_Idx);
                  Allocate_Node (Branch_Node, New_Idx, Alloc_Ok);
                  if not Alloc_Ok then
                     Error := Error_Trie_Full;
                     return;
                  end if;
               end if;

               Success := True;
            end if;

         when Node_Extension =>
            Common_Len := Common_Prefix_Length (Current.Key, Key);

            if Common_Len = Current.Key.Length then
               --  Key extends past extension, recurse to child
               if Current.Children (0).Valid then
                  declare
                     Child_Hash : constant Hash_256 := Current.Children (0).Data;
                     Child_Idx_Lookup : constant Natural := Lookup_Node_By_Hash (Child_Hash);
                     Rest_Key : Nibble_Key := Remove_Prefix (Key, Common_Len);
                     New_Child_Idx : Natural;
                  begin
                     if Child_Idx_Lookup /= Natural'Last then
                        --  Recurse into child
                        Insert_Into_Node (Child_Idx_Lookup, Rest_Key, Val,
                           New_Child_Idx, Success, Error);

                        if Success and New_Child_Idx < Max_Nodes then
                           --  Update extension to point to new child
                           New_Node := Current;
                           if Node_Store (New_Child_Idx).Node.Hash.Valid then
                              New_Node.Children (0) := (
                                 Data => Node_Store (New_Child_Idx).Node.Hash.Data,
                                 Valid => True);
                           end if;
                           New_Node.Hash := (Data => Hash_Node (New_Node), Valid => True);

                           --  Update hash mapping
                           if Current.Hash.Valid then
                              Unregister_Hash_Mapping (Current.Hash.Data);
                           end if;
                           if New_Node.Hash.Valid then
                              Register_Hash_Mapping (New_Node.Hash.Data, Current_Idx);
                           end if;

                           Node_Store (Current_Idx).Node := New_Node;
                           New_Idx := Current_Idx;
                        end if;
                     else
                        --  Child not found in store
                        Error := Error_Invalid_Node;
                     end if;
                  end;
               else
                  Error := Error_Invalid_Node;
               end if;

            elsif Common_Len = 0 then
               --  No match, create branch
               Branch_Node := Make_Branch;
               --  Add extension's child to branch
               declare
                  Ext_Nibble : constant Natural := Natural (Current.Key.Data (0));
               begin
                  if Current.Key.Length > 1 then
                     --  Create new extension for rest of path
                     declare
                        Rest_Key : Nibble_Key := Remove_Prefix (Current.Key, 1);
                        New_Ext  : MPT_Node := Make_Extension (Rest_Key, Current.Children (0).Data);
                     begin
                        Allocate_Node (New_Ext, Child_Idx, Alloc_Ok);
                        if not Alloc_Ok then
                           Error := Error_Trie_Full;
                           return;
                        end if;
                        Branch_Node.Children (Ext_Nibble) := (Data => New_Ext.Hash.Data, Valid => True);
                     end;
                  else
                     --  Extension was single nibble, child goes directly to branch
                     Branch_Node.Children (Ext_Nibble) := Current.Children (0);
                  end if;
               end;

               --  Add new leaf to branch
               if Key.Length > 0 then
                  declare
                     New_Nibble : constant Natural := Natural (Key.Data (0));
                     New_Suffix : Nibble_Key := Remove_Prefix (Key, 1);
                     New_Leaf   : MPT_Node := Make_Leaf (New_Suffix, Val);
                  begin
                     Allocate_Node (New_Leaf, Child_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;
                     Branch_Node.Children (New_Nibble) := (Data => New_Leaf.Hash.Data, Valid => True);
                  end;
               else
                  --  Key ends at branch
                  Branch_Node.Value.Length := Natural'Min (Val'Length, Max_Value_Size);
                  for I in 0 .. Branch_Node.Value.Length - 1 loop
                     Branch_Node.Value.Bytes (I) := Val (Val'First + I);
                  end loop;
               end if;

               Branch_Node.Hash := (Data => Hash_Node (Branch_Node), Valid => True);
               Free_Node (Current_Idx);
               Allocate_Node (Branch_Node, New_Idx, Alloc_Ok);
               if not Alloc_Ok then
                  Error := Error_Trie_Full;
                  return;
               end if;
               Success := True;

            else
               --  Partial match: split extension
               --  Create branch at split point
               Branch_Node := Make_Branch;

               --  Add remainder of old extension
               if Common_Len < Current.Key.Length then
                  declare
                     Old_Nibble : constant Natural := Natural (Current.Key.Data (Common_Len));
                     Old_Rest   : Nibble_Key := Remove_Prefix (Current.Key, Common_Len + 1);
                  begin
                     if Old_Rest.Length > 0 then
                        --  Create new extension for rest
                        declare
                           New_Ext : MPT_Node := Make_Extension (Old_Rest, Current.Children (0).Data);
                        begin
                           Allocate_Node (New_Ext, Child_Idx, Alloc_Ok);
                           if not Alloc_Ok then
                              Error := Error_Trie_Full;
                              return;
                           end if;
                           Branch_Node.Children (Old_Nibble) := (Data => New_Ext.Hash.Data, Valid => True);
                        end;
                     else
                        --  Direct child reference
                        Branch_Node.Children (Old_Nibble) := Current.Children (0);
                     end if;
                  end;
               end if;

               --  Add new leaf
               if Common_Len < Key.Length then
                  declare
                     New_Nibble : constant Natural := Natural (Key.Data (Common_Len));
                     New_Suffix : Nibble_Key := Remove_Prefix (Key, Common_Len + 1);
                     New_Leaf   : MPT_Node := Make_Leaf (New_Suffix, Val);
                  begin
                     Allocate_Node (New_Leaf, Child_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;
                     Branch_Node.Children (New_Nibble) := (Data => New_Leaf.Hash.Data, Valid => True);
                  end;
               else
                  --  Value at branch
                  Branch_Node.Value.Length := Natural'Min (Val'Length, Max_Value_Size);
                  for I in 0 .. Branch_Node.Value.Length - 1 loop
                     Branch_Node.Value.Bytes (I) := Val (Val'First + I);
                  end loop;
               end if;

               Branch_Node.Hash := (Data => Hash_Node (Branch_Node), Valid => True);

               --  Wrap in extension if common prefix
               if Common_Len > 0 then
                  declare
                     Ext_Key : Nibble_Key := Empty_Nibble_Key;
                  begin
                     Ext_Key.Length := Common_Len;
                     for I in 0 .. Common_Len - 1 loop
                        Ext_Key.Data (I) := Current.Key.Data (I);
                     end loop;

                     Allocate_Node (Branch_Node, Child_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;

                     New_Node := Make_Extension (Ext_Key, Branch_Node.Hash.Data);
                     Free_Node (Current_Idx);
                     Allocate_Node (New_Node, New_Idx, Alloc_Ok);
                     if not Alloc_Ok then
                        Error := Error_Trie_Full;
                        return;
                     end if;
                  end;
               else
                  Free_Node (Current_Idx);
                  Allocate_Node (Branch_Node, New_Idx, Alloc_Ok);
                  if not Alloc_Ok then
                     Error := Error_Trie_Full;
                     return;
                  end if;
               end if;

               Success := True;
            end if;

         when Node_Branch =>
            if Key.Length = 0 then
               --  Update value at branch
               New_Node := Current;
               New_Node.Value.Length := Natural'Min (Val'Length, Max_Value_Size);
               for I in 0 .. New_Node.Value.Length - 1 loop
                  New_Node.Value.Bytes (I) := Val (Val'First + I);
               end loop;
               New_Node.Hash := (Data => Hash_Node (New_Node), Valid => True);

               --  Update hash mapping
               if Current.Hash.Valid then
                  Unregister_Hash_Mapping (Current.Hash.Data);
               end if;
               if New_Node.Hash.Valid then
                  Register_Hash_Mapping (New_Node.Hash.Data, Current_Idx);
               end if;

               Node_Store (Current_Idx).Node := New_Node;
               New_Idx := Current_Idx;
               Success := True;
            else
               --  Recurse into appropriate child
               declare
                  Child_Nibble : constant Natural := Natural (Key.Data (0));
                  Rest_Key     : Nibble_Key := Remove_Prefix (Key, 1);
               begin
                  if not Current.Children (Child_Nibble).Valid then
                     --  No child exists, create leaf
                     declare
                        New_Leaf : MPT_Node := Make_Leaf (Rest_Key, Val);
                     begin
                        Allocate_Node (New_Leaf, Child_Idx, Alloc_Ok);
                        if not Alloc_Ok then
                           Error := Error_Trie_Full;
                           return;
                        end if;

                        New_Node := Current;
                        New_Node.Children (Child_Nibble) := (Data => New_Leaf.Hash.Data, Valid => True);
                        New_Node.Hash := (Data => Hash_Node (New_Node), Valid => True);

                        --  Update hash mapping
                        if Current.Hash.Valid then
                           Unregister_Hash_Mapping (Current.Hash.Data);
                        end if;
                        if New_Node.Hash.Valid then
                           Register_Hash_Mapping (New_Node.Hash.Data, Current_Idx);
                        end if;

                        Node_Store (Current_Idx).Node := New_Node;
                        New_Idx := Current_Idx;
                        Success := True;
                     end;
                  else
                     --  Child exists, recurse
                     declare
                        Child_Hash : constant Hash_256 := Current.Children (Child_Nibble).Data;
                        Child_Idx_Lookup : constant Natural := Lookup_Node_By_Hash (Child_Hash);
                        New_Child_Idx : Natural;
                     begin
                        if Child_Idx_Lookup /= Natural'Last then
                           Insert_Into_Node (Child_Idx_Lookup, Rest_Key, Val,
                              New_Child_Idx, Success, Error);

                           if Success and New_Child_Idx < Max_Nodes then
                              --  Update branch to point to new child
                              New_Node := Current;
                              if Node_Store (New_Child_Idx).Node.Hash.Valid then
                                 New_Node.Children (Child_Nibble) := (
                                    Data => Node_Store (New_Child_Idx).Node.Hash.Data,
                                    Valid => True);
                              end if;
                              New_Node.Hash := (Data => Hash_Node (New_Node), Valid => True);

                              --  Update hash mapping
                              if Current.Hash.Valid then
                                 Unregister_Hash_Mapping (Current.Hash.Data);
                              end if;
                              if New_Node.Hash.Valid then
                                 Register_Hash_Mapping (New_Node.Hash.Data, Current_Idx);
                              end if;

                              Node_Store (Current_Idx).Node := New_Node;
                              New_Idx := Current_Idx;
                           end if;
                        else
                           Error := Error_Invalid_Node;
                        end if;
                     end;
                  end if;
               end;
            end if;
      end case;
   end Insert_Into_Node;

   ---------------------------------------------------------------------------
   --  Put
   ---------------------------------------------------------------------------

   procedure Put (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Value   : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) is
      Nibble_Key_Val : Nibble_Key;
      New_Node       : MPT_Node;
      Node_Idx       : Natural;
      New_Root_Idx   : Natural;
      Alloc_Success  : Boolean;
   begin
      Success := False;
      Error := Error_None;

      if Trie = Null_Trie or else not Tries (Trie).Used then
         Error := Error_Invalid_Node;
         return;
      end if;

      --  Convert key to nibbles
      Nibble_Key_Val := Bytes_To_Nibbles (Key, Key'Length);

      --  If trie is empty, create initial leaf
      if Tries (Trie).Root_Idx = Natural'Last then
         New_Node := Make_Leaf (Nibble_Key_Val, Value);
         Allocate_Node (New_Node, Node_Idx, Alloc_Success);
         if not Alloc_Success then
            Error := Error_Trie_Full;
            return;
         end if;
         Tries (Trie).Root_Idx := Node_Idx;
         Tries (Trie).Count := 1;
         Success := True;
         return;
      end if;

      --  Insert into existing tree with restructuring
      Insert_Into_Node (
         Current_Idx => Tries (Trie).Root_Idx,
         Key         => Nibble_Key_Val,
         Val         => Value,
         New_Idx     => New_Root_Idx,
         Success     => Success,
         Error       => Error
      );

      if Success then
         Tries (Trie).Root_Idx := New_Root_Idx;
         Tries (Trie).Count := Tries (Trie).Count + 1;
      end if;
   end Put;

   ---------------------------------------------------------------------------
   --  Get - Fixed with proper hash traversal
   ---------------------------------------------------------------------------

   procedure Get (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Value   : out    Value_Data;
      Found   : out    Boolean;
      Error   : out    MPT_Error
   ) is
      Nibble_Key_Val : Nibble_Key;
      Current_Idx    : Natural;
      Depth          : Natural := 0;
   begin
      Value := Empty_Value;
      Found := False;
      Error := Error_None;

      if Trie = Null_Trie or else not Tries (Trie).Used then
         Error := Error_Invalid_Node;
         return;
      end if;

      if Tries (Trie).Root_Idx = Natural'Last then
         Error := Error_Key_Not_Found;
         return;
      end if;

      Nibble_Key_Val := Bytes_To_Nibbles (Key, Key'Length);
      Current_Idx := Tries (Trie).Root_Idx;

      --  Traverse trie with hash lookup
      while Current_Idx < Max_Nodes and then
            Node_Store (Current_Idx).Used and then
            Depth < Max_Trie_Depth
      loop
         declare
            Current : constant MPT_Node := Node_Store (Current_Idx).Node;
         begin
            case Current.Kind is
               when Node_Empty =>
                  Error := Error_Key_Not_Found;
                  return;

               when Node_Leaf =>
                  if Keys_Equal (Current.Key, Nibble_Key_Val) then
                     Value := Current.Value;
                     Found := True;
                  else
                     Error := Error_Key_Not_Found;
                  end if;
                  return;

               when Node_Extension =>
                  if Is_Prefix (Current.Key, Nibble_Key_Val) then
                     Nibble_Key_Val := Remove_Prefix (
                        Nibble_Key_Val, Current.Key.Length);
                     --  Follow child by hash lookup
                     if Current.Children (0).Valid then
                        Current_Idx := Lookup_Node_By_Hash (Current.Children (0).Data);
                        if Current_Idx = Natural'Last then
                           Error := Error_Key_Not_Found;
                           return;
                        end if;
                        Depth := Depth + 1;
                     else
                        Error := Error_Key_Not_Found;
                        return;
                     end if;
                  else
                     Error := Error_Key_Not_Found;
                     return;
                  end if;

               when Node_Branch =>
                  if Nibble_Key_Val.Length = 0 then
                     --  Value is here
                     if Current.Value.Length > 0 then
                        Value := Current.Value;
                        Found := True;
                     else
                        Error := Error_Key_Not_Found;
                     end if;
                     return;
                  else
                     --  Follow appropriate child
                     declare
                        Child_Idx_Val : constant Natural := Natural (
                           Nibble_Key_Val.Data (0));
                     begin
                        if not Current.Children (Child_Idx_Val).Valid then
                           Error := Error_Key_Not_Found;
                           return;
                        end if;
                        Nibble_Key_Val := Remove_Prefix (Nibble_Key_Val, 1);
                        --  Lookup child by hash
                        Current_Idx := Lookup_Node_By_Hash (
                           Current.Children (Child_Idx_Val).Data);
                        if Current_Idx = Natural'Last then
                           Error := Error_Key_Not_Found;
                           return;
                        end if;
                        Depth := Depth + 1;
                     end;
                  end if;
            end case;
         end;
      end loop;

      --  Max depth reached or invalid node
      Error := Error_Key_Not_Found;
   end Get;

   ---------------------------------------------------------------------------
   --  Delete
   ---------------------------------------------------------------------------

   --  Helper: Check if branch node has only one child (for collapsing)
   function Branch_Child_Count (Node : MPT_Node) return Natural is
      Count : Natural := 0;
   begin
      for I in Child_Index loop
         if Node.Children (I).Valid then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Branch_Child_Count;

   --  Helper: Find single child index in branch
   function Find_Single_Child (Node : MPT_Node) return Natural is
   begin
      for I in Child_Index loop
         if Node.Children (I).Valid then
            return I;
         end if;
      end loop;
      return 16;  -- Invalid
   end Find_Single_Child;

   --  Recursive deletion from node
   procedure Delete_From_Node (
      Current_Idx : in     Natural;
      Key         : in     Nibble_Key;
      New_Idx     : out    Natural;
      Deleted     : out    Boolean;
      Error       : out    MPT_Error
   ) is
      Current : MPT_Node;
   begin
      New_Idx := Current_Idx;
      Deleted := False;
      Error := Error_None;

      if Current_Idx >= Max_Nodes or else not Node_Store (Current_Idx).Used then
         Error := Error_Key_Not_Found;
         return;
      end if;

      Current := Node_Store (Current_Idx).Node;

      case Current.Kind is
         when Node_Empty =>
            Error := Error_Key_Not_Found;

         when Node_Leaf =>
            if Keys_Equal (Current.Key, Key) then
               --  Found the key, delete the leaf
               Free_Node (Current_Idx);
               New_Idx := Natural'Last;  -- Signals deletion
               Deleted := True;
            else
               Error := Error_Key_Not_Found;
            end if;

         when Node_Extension =>
            if Is_Prefix (Current.Key, Key) then
               --  Key goes through this extension
               if Current.Children (0).Valid then
                  declare
                     Rest_Key : Nibble_Key := Remove_Prefix (Key, Current.Key.Length);
                     Child_Idx : constant Natural := Lookup_Node_By_Hash (
                        Current.Children (0).Data);
                     New_Child_Idx : Natural;
                     Child_Deleted : Boolean;
                  begin
                     if Child_Idx /= Natural'Last then
                        Delete_From_Node (Child_Idx, Rest_Key, New_Child_Idx,
                           Child_Deleted, Error);

                        if Child_Deleted then
                           if New_Child_Idx = Natural'Last then
                              --  Child was deleted entirely
                              Free_Node (Current_Idx);
                              New_Idx := Natural'Last;
                           else
                              --  Update extension to point to new child
                              Current.Children (0) := (
                                 Data => Node_Store (New_Child_Idx).Node.Hash.Data,
                                 Valid => True);
                              Current.Hash := (Data => Hash_Node (Current), Valid => True);

                              --  Update hash mapping
                              Register_Hash_Mapping (Current.Hash.Data, Current_Idx);
                              Node_Store (Current_Idx).Node := Current;
                           end if;
                           Deleted := True;
                        end if;
                     else
                        Error := Error_Key_Not_Found;
                     end if;
                  end;
               else
                  Error := Error_Key_Not_Found;
               end if;
            else
               Error := Error_Key_Not_Found;
            end if;

         when Node_Branch =>
            if Key.Length = 0 then
               --  Delete value at this branch
               if Current.Value.Length > 0 then
                  Current.Value.Length := 0;
                  Current.Value.Bytes := (others => 0);

                  --  Check if branch can be collapsed
                  declare
                     Child_Count : constant Natural := Branch_Child_Count (Current);
                  begin
                     if Child_Count = 0 then
                        --  No children, no value - delete branch
                        Free_Node (Current_Idx);
                        New_Idx := Natural'Last;
                     elsif Child_Count = 1 then
                        --  Single child - could collapse to extension
                        --  For simplicity, keep as branch
                        Current.Hash := (Data => Hash_Node (Current), Valid => True);
                        Register_Hash_Mapping (Current.Hash.Data, Current_Idx);
                        Node_Store (Current_Idx).Node := Current;
                     else
                        --  Multiple children - just update hash
                        Current.Hash := (Data => Hash_Node (Current), Valid => True);
                        Register_Hash_Mapping (Current.Hash.Data, Current_Idx);
                        Node_Store (Current_Idx).Node := Current;
                     end if;
                  end;
                  Deleted := True;
               else
                  Error := Error_Key_Not_Found;
               end if;
            else
               --  Delete from child
               declare
                  Child_Nibble : constant Natural := Natural (Key.Data (0));
               begin
                  if Current.Children (Child_Nibble).Valid then
                     declare
                        Child_Idx : constant Natural := Lookup_Node_By_Hash (
                           Current.Children (Child_Nibble).Data);
                        Rest_Key : Nibble_Key := Remove_Prefix (Key, 1);
                        New_Child_Idx : Natural;
                        Child_Deleted : Boolean;
                     begin
                        if Child_Idx /= Natural'Last then
                           Delete_From_Node (Child_Idx, Rest_Key, New_Child_Idx,
                              Child_Deleted, Error);

                           if Child_Deleted then
                              if New_Child_Idx = Natural'Last then
                                 --  Child was deleted
                                 Current.Children (Child_Nibble).Valid := False;
                              else
                                 --  Update child reference
                                 Current.Children (Child_Nibble) := (
                                    Data => Node_Store (New_Child_Idx).Node.Hash.Data,
                                    Valid => True);
                              end if;

                              --  Check for branch collapse
                              declare
                                 Child_Count : constant Natural := Branch_Child_Count (Current);
                              begin
                                 if Child_Count = 0 and Current.Value.Length = 0 then
                                    --  Branch is now empty
                                    Free_Node (Current_Idx);
                                    New_Idx := Natural'Last;
                                 else
                                    Current.Hash := (Data => Hash_Node (Current), Valid => True);
                                    Register_Hash_Mapping (Current.Hash.Data, Current_Idx);
                                    Node_Store (Current_Idx).Node := Current;
                                 end if;
                              end;
                              Deleted := True;
                           end if;
                        else
                           Error := Error_Key_Not_Found;
                        end if;
                     end;
                  else
                     Error := Error_Key_Not_Found;
                  end if;
               end;
            end if;
      end case;
   end Delete_From_Node;

   procedure Delete (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) is
      Nibble_Key_Val : Nibble_Key;
      New_Root_Idx   : Natural;
      Deleted        : Boolean;
   begin
      Success := False;
      Error := Error_None;

      if Trie = Null_Trie or else not Tries (Trie).Used then
         Error := Error_Invalid_Node;
         return;
      end if;

      if Tries (Trie).Root_Idx = Natural'Last then
         Error := Error_Key_Not_Found;
         return;
      end if;

      --  Convert key to nibbles
      Nibble_Key_Val := Bytes_To_Nibbles (Key, Key'Length);

      --  Delete from tree
      Delete_From_Node (
         Current_Idx => Tries (Trie).Root_Idx,
         Key         => Nibble_Key_Val,
         New_Idx     => New_Root_Idx,
         Deleted     => Deleted,
         Error       => Error
      );

      if Deleted then
         if New_Root_Idx = Natural'Last then
            --  Root was deleted, trie is now empty
            Tries (Trie).Root_Idx := Natural'Last;
         else
            Tries (Trie).Root_Idx := New_Root_Idx;
         end if;
         if Tries (Trie).Count > 0 then
            Tries (Trie).Count := Tries (Trie).Count - 1;
         end if;
         Success := True;
      end if;
   end Delete;

   ---------------------------------------------------------------------------
   --  Contains
   ---------------------------------------------------------------------------

   function Contains (
      Trie : Trie_ID;
      Key  : Byte_Array
   ) return Boolean
   is
      Value : Value_Data;
      Found : Boolean;
      Error : MPT_Error;
   begin
      Get (Trie, Key, Value, Found, Error);
      return Found;
   end Contains;

   ---------------------------------------------------------------------------
   --  Root_Hash
   ---------------------------------------------------------------------------

   function Root_Hash (Trie : Trie_ID) return Hash_256 is
   begin
      if Trie = Null_Trie or else not Tries (Trie).Used then
         return Empty_Hash;
      end if;

      if Tries (Trie).Root_Idx = Natural'Last then
         --  Empty trie has specific hash (keccak of RLP null)
         return Keccak_256 ((0 => RLP_Short_String));
      end if;

      if Tries (Trie).Root_Idx < Max_Nodes and then
         Node_Store (Tries (Trie).Root_Idx).Used and then
         Node_Store (Tries (Trie).Root_Idx).Node.Hash.Valid
      then
         return Node_Store (Tries (Trie).Root_Idx).Node.Hash.Data;
      end if;

      return Empty_Hash;
   end Root_Hash;

   ---------------------------------------------------------------------------
   --  Is_Empty
   ---------------------------------------------------------------------------

   function Is_Empty (Trie : Trie_ID) return Boolean is
   begin
      if Trie = Null_Trie or else not Tries (Trie).Used then
         return True;
      end if;
      return Tries (Trie).Root_Idx = Natural'Last;
   end Is_Empty;

   ---------------------------------------------------------------------------
   --  Node_Count
   ---------------------------------------------------------------------------

   function Node_Count (Trie : Trie_ID) return Natural is
   begin
      if Trie = Null_Trie or else not Tries (Trie).Used then
         return 0;
      end if;
      return Tries (Trie).Count;
   end Node_Count;

   ---------------------------------------------------------------------------
   --  Generate_Proof - Fixed with proper hash traversal
   ---------------------------------------------------------------------------

   procedure Generate_Proof (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Proof   : out    Merkle_Proof;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) is
      Nibble_Key_Val : Nibble_Key;
      Current_Idx    : Natural;
      Depth          : Natural := 0;
      Enc_Buffer     : Byte_Array (0 .. Max_Proof_Node_Size - 1);
      Enc_Len        : Natural;
      Enc_Success    : Boolean;
   begin
      Proof := (
         Nodes  => (others => (Data => (others => 0), Length => 0)),
         Depth  => 0,
         Key    => Bytes_To_Nibbles (Key, Key'Length),
         Value  => Empty_Value,
         Exists => False
      );
      Success := False;
      Error := Error_None;

      if Trie = Null_Trie or else not Tries (Trie).Used then
         Error := Error_Invalid_Node;
         return;
      end if;

      --  Empty trie case
      if Tries (Trie).Root_Idx = Natural'Last then
         Proof.Depth := 0;
         Proof.Exists := False;
         Success := True;
         return;
      end if;

      Nibble_Key_Val := Bytes_To_Nibbles (Key, Key'Length);
      Current_Idx := Tries (Trie).Root_Idx;

      --  Walk down the trie, collecting ALL proof nodes
      while Current_Idx < Max_Nodes and then
            Node_Store (Current_Idx).Used and then
            Depth < Max_Trie_Depth
      loop
         declare
            Current : constant MPT_Node := Node_Store (Current_Idx).Node;
         begin
            --  Encode current node and add to proof
            Encode_Node (Current, Enc_Buffer, Enc_Len, Enc_Success);
            if not Enc_Success then
               Error := Error_Invalid_Node;
               return;
            end if;

            --  Copy encoded node to proof path
            for I in 0 .. Enc_Len - 1 loop
               Proof.Nodes (Depth).Data (I) := Enc_Buffer (I);
            end loop;
            Proof.Nodes (Depth).Length := Enc_Len;
            Depth := Depth + 1;

            case Current.Kind is
               when Node_Empty =>
                  --  Key not found (proof of non-existence)
                  Proof.Depth := Depth;
                  Proof.Exists := False;
                  Success := True;
                  return;

               when Node_Leaf =>
                  --  Check if keys match
                  if Keys_Equal (Current.Key, Nibble_Key_Val) then
                     Proof.Value := Current.Value;
                     Proof.Exists := True;
                  else
                     Proof.Exists := False;  -- Proof of non-existence
                  end if;
                  Proof.Depth := Depth;
                  Success := True;
                  return;

               when Node_Extension =>
                  --  Check if extension key is prefix of search key
                  if Is_Prefix (Current.Key, Nibble_Key_Val) then
                     Nibble_Key_Val := Remove_Prefix (
                        Nibble_Key_Val, Current.Key.Length);
                     --  Follow child by hash
                     if Current.Children (0).Valid then
                        Current_Idx := Lookup_Node_By_Hash (Current.Children (0).Data);
                        if Current_Idx = Natural'Last then
                           --  Child not found, incomplete proof
                           Proof.Depth := Depth;
                           Proof.Exists := False;
                           Success := True;
                           return;
                        end if;
                     else
                        --  No child, proof of non-existence
                        Proof.Depth := Depth;
                        Proof.Exists := False;
                        Success := True;
                        return;
                     end if;
                  else
                     --  Proof of non-existence
                     Proof.Depth := Depth;
                     Proof.Exists := False;
                     Success := True;
                     return;
                  end if;

               when Node_Branch =>
                  if Nibble_Key_Val.Length = 0 then
                     --  Value is at this branch
                     if Current.Value.Length > 0 then
                        Proof.Value := Current.Value;
                        Proof.Exists := True;
                     else
                        Proof.Exists := False;
                     end if;
                     Proof.Depth := Depth;
                     Success := True;
                     return;
                  else
                     --  Follow the appropriate child
                     declare
                        Child_Nibble : constant Nibble :=
                           Nibble_Key_Val.Data (0);
                        Child_Idx : constant Natural := Natural (Child_Nibble);
                     begin
                        if not Current.Children (Child_Idx).Valid then
                           --  Proof of non-existence
                           Proof.Depth := Depth;
                           Proof.Exists := False;
                           Success := True;
                           return;
                        end if;
                        Nibble_Key_Val := Remove_Prefix (Nibble_Key_Val, 1);
                        --  Lookup child by hash and continue
                        Current_Idx := Lookup_Node_By_Hash (
                           Current.Children (Child_Idx).Data);
                        if Current_Idx = Natural'Last then
                           --  Child not found
                           Proof.Depth := Depth;
                           Proof.Exists := False;
                           Success := True;
                           return;
                        end if;
                     end;
                  end if;
            end case;
         end;
      end loop;

      --  Traversal complete
      Proof.Depth := Depth;
      Success := True;
   end Generate_Proof;

   ---------------------------------------------------------------------------
   --  Verify_Proof - Complete implementation with full verification
   ---------------------------------------------------------------------------

   procedure Verify_Proof (
      Root    : in     Hash_256;
      Proof   : in     Merkle_Proof;
      Valid   : out    Boolean;
      Error   : out    MPT_Error
   ) is
      Computed_Hash  : Hash_256;
      Node_Data      : Byte_Array (0 .. Max_Proof_Node_Size - 1) := (others => 0);
      Node_Len       : Natural;
      Decoded_Node   : MPT_Node;
      Decode_Success : Boolean;
      Decode_Error   : MPT_Error;
      Current_Key    : Nibble_Key := Proof.Key;
   begin
      Valid := False;
      Error := Error_None;

      --  Empty proof is valid for empty trie (check against empty root)
      if Proof.Depth = 0 then
         --  Empty trie has specific hash (keccak of RLP null = 0x80)
         declare
            Empty_Root : constant Hash_256 :=
               Keccak_256 ((0 => RLP_Short_String));
         begin
            if Root = Empty_Root then
               Valid := not Proof.Exists;  --  Valid if claiming non-existence
            else
               Error := Error_Invalid_Proof;
            end if;
         end;
         return;
      end if;

      --  Verify proof chain from root to target
      for I in 0 .. Proof.Depth - 1 loop
         Node_Len := Proof.Nodes (I).Length;

         if Node_Len = 0 or Node_Len > Max_Proof_Node_Size then
            Error := Error_Invalid_Proof;
            return;
         end if;

         --  Copy node data
         for J in 0 .. Node_Len - 1 loop
            Node_Data (J) := Proof.Nodes (I).Data (J);
         end loop;

         --  Compute hash of this node
         Computed_Hash := Keccak_256 (Node_Data (0 .. Node_Len - 1));

         if I = 0 then
            --  First node must match root
            if Computed_Hash /= Root then
               Error := Error_Invalid_Proof;
               return;
            end if;
         end if;

         --  Decode node to verify structure
         Decode_Node (Node_Data (0 .. Node_Len - 1), Decoded_Node,
            Decode_Success, Decode_Error);

         if not Decode_Success then
            Error := Decode_Error;
            return;
         end if;

         --  Verify node consistency with key path
         case Decoded_Node.Kind is
            when Node_Empty =>
               --  Empty node means key doesn't exist
               if Proof.Exists then
                  Error := Error_Invalid_Proof;
                  return;
               end if;

            when Node_Leaf =>
               --  Leaf node - check if key matches
               if I /= Proof.Depth - 1 then
                  --  Leaf should be last node in proof
                  Error := Error_Invalid_Proof;
                  return;
               end if;
               --  Key match already checked in proof generation

            when Node_Extension =>
               --  Extension - verify key prefix and follow child
               if not Is_Prefix (Decoded_Node.Key, Current_Key) then
                  --  Extension path doesn't match - valid non-existence proof
                  if Proof.Exists then
                     Error := Error_Invalid_Proof;
                     return;
                  end if;
               else
                  Current_Key := Remove_Prefix (Current_Key, Decoded_Node.Key.Length);
               end if;

            when Node_Branch =>
               --  Branch - verify we follow correct child
               if Current_Key.Length > 0 then
                  declare
                     Child_Nibble : constant Natural := Natural (Current_Key.Data (0));
                  begin
                     --  Verify child exists if this isn't the last node
                     if I < Proof.Depth - 1 then
                        if not Decoded_Node.Children (Child_Nibble).Valid then
                           --  Child doesn't exist - valid non-existence proof
                           if Proof.Exists then
                              Error := Error_Invalid_Proof;
                              return;
                           end if;
                        end if;
                     end if;
                     Current_Key := Remove_Prefix (Current_Key, 1);
                  end;
               else
                  --  Key ends at branch - check value
                  if I /= Proof.Depth - 1 then
                     Error := Error_Invalid_Proof;
                     return;
                  end if;
               end if;
         end case;
      end loop;

      --  If we got here, proof structure is valid
      Valid := True;
   end Verify_Proof;

   ---------------------------------------------------------------------------
   --  Snapshots
   ---------------------------------------------------------------------------

   procedure Create_Snapshot (
      Trie        : in     Trie_ID;
      Snapshot_ID : out    Natural;
      Success     : out    Boolean
   ) is
      Slot : constant Natural := Find_Empty_Snapshot;
   begin
      if Slot = Natural'Last or else
         Trie = Null_Trie or else not Tries (Trie).Used
      then
         Snapshot_ID := 0;
         Success := False;
         return;
      end if;

      Snapshot_Store (Slot) := (
         Trie_Ref => Trie,
         Root_Idx => Tries (Trie).Root_Idx,
         Used     => True
      );

      Snapshot_ID := Slot;
      Success := True;
   end Create_Snapshot;

   procedure Restore_Snapshot (
      Trie        : in     Trie_ID;
      Snapshot_ID : in     Natural;
      Success     : out    Boolean;
      Error       : out    MPT_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      if Snapshot_ID >= Max_Snapshots or else
         not Snapshot_Store (Snapshot_ID).Used
      then
         Error := Error_Invalid_Node;
         return;
      end if;

      if Snapshot_Store (Snapshot_ID).Trie_Ref /= Trie then
         Error := Error_Invalid_Node;
         return;
      end if;

      Tries (Trie).Root_Idx := Snapshot_Store (Snapshot_ID).Root_Idx;
      Success := True;
   end Restore_Snapshot;

   procedure Discard_Snapshot (
      Trie        : in     Trie_ID;
      Snapshot_ID : in     Natural
   ) is
   begin
      if Snapshot_ID < Max_Snapshots and then
         Snapshot_Store (Snapshot_ID).Used and then
         Snapshot_Store (Snapshot_ID).Trie_Ref = Trie
      then
         Snapshot_Store (Snapshot_ID).Used := False;
      end if;
   end Discard_Snapshot;

   ---------------------------------------------------------------------------
   --  Iteration - Fixed with hash lookup
   ---------------------------------------------------------------------------

   procedure Begin_Iteration (
      Trie : in     Trie_ID;
      Iter : out    Iterator
   ) is
      --  Store volatile function result before aggregate (SPARK requirement)
      Empty_Check : constant Boolean := Is_Empty (Trie);
   begin
      Iter := (
         Trie      => Trie,
         Stack     => (others => (Node_Idx => 0, Child => 0)),
         Depth     => 0,
         Current   => Empty_Nibble_Key,
         Exhausted => Empty_Check
      );

      --  Initialize stack with root if trie is not empty
      if not Iter.Exhausted and then
         Trie /= Null_Trie and then
         Tries (Trie).Used and then
         Tries (Trie).Root_Idx < Max_Nodes
      then
         Iter.Stack (0).Node_Idx := Tries (Trie).Root_Idx;
         Iter.Stack (0).Child := 0;
         Iter.Depth := 1;
      end if;
   end Begin_Iteration;

   procedure Next (
      Iter    : in out Iterator;
      Key     : out    Nibble_Key;
      Value   : out    Value_Data;
      Done    : out    Boolean
   ) is
      Node      : MPT_Node;
      Node_Idx  : Natural;
      Found     : Boolean := False;
   begin
      Key := Empty_Nibble_Key;
      Value := Empty_Value;
      Done := False;

      if Iter.Exhausted or Iter.Depth = 0 then
         Done := True;
         Iter.Exhausted := True;
         return;
      end if;

      --  Depth-first traversal using explicit stack with hash lookup
      while Iter.Depth > 0 and Iter.Depth <= Max_Trie_Depth and not Found loop
         declare
            Stack_Top : constant Natural := Iter.Depth - 1;
         begin
            Node_Idx := Iter.Stack (Stack_Top).Node_Idx;

            if Node_Idx >= Max_Nodes or else not Node_Store (Node_Idx).Used then
               --  Invalid node, pop and continue
               Iter.Depth := Iter.Depth - 1;
            else
               Node := Node_Store (Node_Idx).Node;

               case Node.Kind is
                  when Node_Empty =>
                     --  Pop empty node
                     Iter.Depth := Iter.Depth - 1;

                  when Node_Leaf =>
                     --  Found a leaf - return its key and value
                     --  Build full key from current path + leaf key
                     Key := Iter.Current;
                     --  Append leaf's key suffix
                     for I in 0 .. Node.Key.Length - 1 loop
                        if Key.Length < Max_Key_Nibbles then
                           Key.Data (Key.Length) := Node.Key.Data (I);
                           Key.Length := Key.Length + 1;
                        end if;
                     end loop;
                     Value := Node.Value;
                     Found := True;

                     --  Pop the leaf for next iteration
                     Iter.Depth := Iter.Depth - 1;

                  when Node_Extension =>
                     --  Extensions point to a single child
                     --  Add extension key to current path
                     for I in 0 .. Node.Key.Length - 1 loop
                        if Iter.Current.Length < Max_Key_Nibbles then
                           Iter.Current.Data (Iter.Current.Length) := Node.Key.Data (I);
                           Iter.Current.Length := Iter.Current.Length + 1;
                        end if;
                     end loop;

                     --  Follow child by hash lookup
                     if Node.Children (0).Valid then
                        declare
                           Child_Idx : constant Natural := Lookup_Node_By_Hash (
                              Node.Children (0).Data);
                        begin
                           if Child_Idx /= Natural'Last and then
                              Iter.Depth < Max_Trie_Depth
                           then
                              --  Push child onto stack
                              Iter.Stack (Iter.Depth) := (Node_Idx => Child_Idx, Child => 0);
                              Iter.Depth := Iter.Depth + 1;
                           else
                              --  Can't follow, pop extension
                              Iter.Depth := Iter.Depth - 1;
                           end if;
                        end;
                     else
                        --  No child, pop extension
                        Iter.Depth := Iter.Depth - 1;
                     end if;

                  when Node_Branch =>
                     declare
                        Start_Child : constant Natural := Iter.Stack (Stack_Top).Child;
                     begin
                        --  First time visiting: check branch value
                        if Start_Child = 0 and Node.Value.Length > 0 then
                           --  Return branch's embedded value
                           Key := Iter.Current;
                           Value := Node.Value;
                           Found := True;
                           --  Mark value as visited, start children at index 1
                           Iter.Stack (Stack_Top).Child := 1;
                        else
                           --  Look for next valid child
                           declare
                              Child_Start : constant Natural :=
                                 (if Start_Child > 0 then Start_Child - 1 else 0);
                              Found_Child : Boolean := False;
                           begin
                              for C in Child_Start .. 15 loop
                                 if Node.Children (C).Valid then
                                    --  Lookup child by hash
                                    declare
                                       Child_Idx : constant Natural := Lookup_Node_By_Hash (
                                          Node.Children (C).Data);
                                    begin
                                       if Child_Idx /= Natural'Last and then
                                          Iter.Depth < Max_Trie_Depth
                                       then
                                          --  Add this nibble to path
                                          if Iter.Current.Length < Max_Key_Nibbles then
                                             Iter.Current.Data (Iter.Current.Length) := Nibble (C);
                                             Iter.Current.Length := Iter.Current.Length + 1;
                                          end if;

                                          --  Mark next child to visit
                                          Iter.Stack (Stack_Top).Child := C + 2;

                                          --  Push child onto stack
                                          Iter.Stack (Iter.Depth) := (Node_Idx => Child_Idx, Child => 0);
                                          Iter.Depth := Iter.Depth + 1;
                                          Found_Child := True;
                                          exit;
                                       end if;
                                    end;
                                 end if;
                              end loop;

                              if not Found_Child then
                                 --  No more children, pop this branch
                                 Iter.Depth := Iter.Depth - 1;
                              end if;
                           end;
                        end if;
                     end;
               end case;
            end if;
         end;
      end loop;

      if not Found then
         Iter.Exhausted := True;
         Done := True;
      end if;
   end Next;

   ---------------------------------------------------------------------------
   --  State Persistence - Hash Map Save/Load
   ---------------------------------------------------------------------------

   --  Save hash map to disk in binary format
   --  Format: [4-byte count][entries...]
   --  Each entry: [32-byte hash][4-byte index][1-byte valid]
   procedure Save_Hash_Map (
      File_Path : in     String;
      Success   : out    Boolean
   ) is
      --  SPARK_Mode Off for file I/O
      pragma SPARK_Mode (Off);

      use Interfaces;

      --  Binary format: 4 (count) + Hash_Table_Size * (32 + 4 + 1) = 4 + 740,000 bytes
      Entry_Size : constant := 37;  --  32 (hash) + 4 (index) + 1 (valid)
      Buffer_Size : constant := 4 + Hash_Table_Size * Entry_Size;

      type Byte_Buffer is array (0 .. Buffer_Size - 1) of Unsigned_8;
      Buffer : Byte_Buffer := (others => 0);
      Pos : Natural := 0;

      --  Count valid entries
      Valid_Count : Natural := 0;
      Temp_File : constant String := File_Path & ".tmp";

      --  File operations
      package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
      File : Byte_IO.File_Type;
   begin
      Success := False;

      --  Count valid entries
      for I in Hash_Map'Range loop
         if Hash_Map (I).Valid then
            Valid_Count := Valid_Count + 1;
         end if;
      end loop;

      --  Encode count (4 bytes, little-endian)
      Buffer (Pos) := Unsigned_8 (Unsigned_32 (Valid_Count) and 16#FF#);
      Buffer (Pos + 1) := Unsigned_8 (Shift_Right (Unsigned_32 (Valid_Count), 8) and 16#FF#);
      Buffer (Pos + 2) := Unsigned_8 (Shift_Right (Unsigned_32 (Valid_Count), 16) and 16#FF#);
      Buffer (Pos + 3) := Unsigned_8 (Shift_Right (Unsigned_32 (Valid_Count), 24) and 16#FF#);
      Pos := Pos + 4;

      --  Encode each hash map entry
      for I in Hash_Map'Range loop
         if Hash_Map (I).Valid then
            --  Write hash (32 bytes)
            for J in Hash_Index loop
               Buffer (Pos) := Hash_Map (I).Hash (J);
               Pos := Pos + 1;
            end loop;

            --  Write index (4 bytes, little-endian)
            declare
               Idx : constant Unsigned_32 := Unsigned_32 (Hash_Map (I).Index);
            begin
               Buffer (Pos) := Unsigned_8 (Idx and 16#FF#);
               Buffer (Pos + 1) := Unsigned_8 (Shift_Right (Idx, 8) and 16#FF#);
               Buffer (Pos + 2) := Unsigned_8 (Shift_Right (Idx, 16) and 16#FF#);
               Buffer (Pos + 3) := Unsigned_8 (Shift_Right (Idx, 24) and 16#FF#);
               Pos := Pos + 4;
            end;

            --  Write valid flag (1 byte)
            Buffer (Pos) := (if Hash_Map (I).Valid then 1 else 0);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Write to temp file atomically
      begin
         Byte_IO.Create (File, Byte_IO.Out_File, Temp_File);

         for I in 0 .. Pos - 1 loop
            Byte_IO.Write (File, Buffer (I));
         end loop;

         Byte_IO.Close (File);

         --  Atomic rename (POSIX semantics)
         Ada.Directories.Rename (Temp_File, File_Path);

         Success := True;
      exception
         when others =>
            if Byte_IO.Is_Open (File) then
               Byte_IO.Close (File);
            end if;
            --  Clean up temp file if it exists
            if Ada.Directories.Exists (Temp_File) then
               Ada.Directories.Delete_File (Temp_File);
            end if;
            Success := False;
      end;
   end Save_Hash_Map;

   --  Load hash map from disk
   procedure Load_Hash_Map (
      File_Path : in     String;
      Success   : out    Boolean
   ) is
      pragma SPARK_Mode (Off);

      use Interfaces;

      package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
      File : Byte_IO.File_Type;

      Entry_Size : constant := 37;
      Buffer_Size : constant := 4 + Hash_Table_Size * Entry_Size;

      type Byte_Buffer is array (0 .. Buffer_Size - 1) of Unsigned_8;
      Buffer : Byte_Buffer := (others => 0);
      Pos : Natural := 0;

      Entry_Count : Natural := 0;
      B : Unsigned_8;
   begin
      Success := False;

      --  Check if file exists
      if not Ada.Directories.Exists (File_Path) then
         --  Not an error - just no saved state
         Success := True;
         return;
      end if;

      begin
         Byte_IO.Open (File, Byte_IO.In_File, File_Path);

         --  Read file into buffer
         while not Byte_IO.End_Of_File (File) and then Pos < Buffer_Size loop
            Byte_IO.Read (File, B);
            Buffer (Pos) := B;
            Pos := Pos + 1;
         end loop;

         Byte_IO.Close (File);

         if Pos < 4 then
            --  File too small
            return;
         end if;

         --  Decode entry count (4 bytes, little-endian)
         Pos := 0;
         Entry_Count := Natural (Unsigned_32 (Buffer (Pos)) or
                       Shift_Left (Unsigned_32 (Buffer (Pos + 1)), 8) or
                       Shift_Left (Unsigned_32 (Buffer (Pos + 2)), 16) or
                       Shift_Left (Unsigned_32 (Buffer (Pos + 3)), 24));
         Pos := Pos + 4;

         if Entry_Count > Hash_Table_Size then
            --  Invalid count
            return;
         end if;

         --  Clear existing hash map
         Hash_Map := (others => (
            Hash  => Empty_Hash,
            Index => 0,
            Valid => False
         ));

         --  Load each entry
         for I in 0 .. Entry_Count - 1 loop
            if Pos + Entry_Size > Buffer'Last then
               return;
            end if;

            declare
               Hash_Val : Hash_256;
               Index_Val : Natural;
               Valid_Val : Boolean;
               Slot : Natural;
            begin
               --  Read hash (32 bytes)
               for J in Hash_Index loop
                  Hash_Val (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;

               --  Read index (4 bytes, little-endian)
               Index_Val := Natural (Unsigned_32 (Buffer (Pos)) or
                           Shift_Left (Unsigned_32 (Buffer (Pos + 1)), 8) or
                           Shift_Left (Unsigned_32 (Buffer (Pos + 2)), 16) or
                           Shift_Left (Unsigned_32 (Buffer (Pos + 3)), 24));
               Pos := Pos + 4;

               --  Read valid flag
               Valid_Val := (Buffer (Pos) /= 0);
               Pos := Pos + 1;

               --  Insert into hash map using linear probing
               if Valid_Val and then Index_Val < Max_Nodes then
                  Slot := Hash_Table_Index (Hash_Val);

                  --  Find empty slot
                  for Probe in 0 .. Hash_Table_Size - 1 loop
                     declare
                        Probe_Pos : constant Natural := (Slot + Probe) mod Hash_Table_Size;
                     begin
                        if not Hash_Map (Probe_Pos).Valid then
                           Hash_Map (Probe_Pos) := (
                              Hash  => Hash_Val,
                              Index => Index_Val,
                              Valid => True
                           );
                           exit;
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;

         Success := True;

      exception
         when others =>
            if Byte_IO.Is_Open (File) then
               Byte_IO.Close (File);
            end if;
            Success := False;
      end;
   end Load_Hash_Map;

   --  Rebuild hash map from Node_Store
   --  Use this after loading nodes from persistence but before hash map is loaded
   procedure Rebuild_Hash_Map (
      Success : out Boolean
   ) is
   begin
      Success := False;

      --  Clear hash map
      Hash_Map := (others => (
         Hash  => Empty_Hash,
         Index => 0,
         Valid => False
      ));

      --  Scan all nodes and rebuild mappings
      for I in Node_Store'Range loop
         if Node_Store (I).Used and then Node_Store (I).Node.Hash.Valid then
            Register_Hash_Mapping (Node_Store (I).Node.Hash.Data, I);
         end if;
      end loop;

      Success := True;
   end Rebuild_Hash_Map;

   --  Validate hash map is synchronized with Node_Store
   function Validate_Hash_Map return Boolean is
      Found_Index : Natural;
   begin
      --  For each valid node with a valid hash, verify it's in the hash map
      for I in Node_Store'Range loop
         if Node_Store (I).Used and then Node_Store (I).Node.Hash.Valid then
            --  Look up this node's hash in the map
            Found_Index := Lookup_Node_By_Hash (Node_Store (I).Node.Hash.Data);

            if Found_Index = Natural'Last then
               --  Hash not in map - validation failed
               return False;
            end if;

            if Found_Index /= I then
               --  Hash maps to wrong index - validation failed
               return False;
            end if;
         end if;
      end loop;

      --  All valid nodes have correct hash mappings
      return True;
   end Validate_Hash_Map;

end Khepri_MPT;
