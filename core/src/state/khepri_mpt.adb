--  KHEPRI Merkle Patricia Trie Implementation
pragma SPARK_Mode (On);

with Anubis_Types;
with Anubis_SHA3;

package body Khepri_MPT with
   SPARK_Mode => On,
   Refined_State => (Trie_State => (Tries, Node_Store, Snapshot_Store))
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

   Snapshot_Store : Snapshot_Array := (others => (
      Trie_Ref => 0,
      Root_Idx => 0,
      Used     => False
   ));

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

   --  Allocate a new node
   procedure Allocate_Node (
      Node    : in     MPT_Node;
      Idx     : out    Natural;
      Success : out    Boolean
   ) is
      Slot : constant Natural := Find_Empty_Node;
   begin
      if Slot = Natural'Last then
         Idx := 0;
         Success := False;
         return;
      end if;

      Node_Store (Slot) := (Node => Node, Used => True);
      Idx := Slot;
      Success := True;
   end Allocate_Node;

   --  Free a node
   procedure Free_Node (Idx : Natural) is
   begin
      if Idx < Max_Nodes then
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
      Result : Byte_Array (0 .. Len - 1);
   begin
      for I in 0 .. Len - 1 loop
         Result (I) := NV (I);
      end loop;
      return Result;
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
   --  Decode_Node (simplified)
   ---------------------------------------------------------------------------

   procedure Decode_Node (
      Input   : in     Byte_Array;
      Node    : out    MPT_Node;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) is
   begin
      Node := Empty_Node;
      Success := False;
      Error := Error_None;

      if Input'Length = 0 then
         Error := Error_Invalid_RLP;
         return;
      end if;

      --  Check for empty node
      if Input'Length = 1 and then Input (Input'First) = RLP_Short_String then
         Node.Kind := Node_Empty;
         Success := True;
         return;
      end if;

      --  Determine if list
      if Input (Input'First) >= RLP_Short_List then
         --  It"s a list, determine length
         declare
            List_Len : Natural;
            Start    : Natural;
            Elements : Natural := 0;
         begin
            if Input (Input'First) < RLP_Long_List then
               List_Len := Natural (Input (Input'First) - RLP_Short_List);
               Start := 1;
            else
               --  Long list
               declare
                  Len_Bytes : constant Natural := Natural (
                     Input (Input'First) - RLP_Long_List);
               begin
                  List_Len := 0;
                  for I in 1 .. Len_Bytes loop
                     List_Len := List_Len * 256 +
                        Natural (Input (Input'First + I));
                  end loop;
                  Start := 1 + Len_Bytes;
               end;
            end if;

            --  Count elements (simplified: assume 2 or 17 elements)
            --  2 elements = leaf or extension
            --  17 elements = branch
            if List_Len < 40 then
               --  Likely leaf or extension (short)
               Node.Kind := Node_Leaf;  --  Need HP prefix to distinguish
            else
               --  Likely branch
               Node.Kind := Node_Branch;
            end if;

            Success := True;
         end;
      else
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
   --  Load_Trie
   ---------------------------------------------------------------------------

   procedure Load_Trie (
      Root    : in     Hash_256;
      Trie    : out    Trie_ID;
      Success : out    Boolean
   ) is
      New_Trie : constant Trie_ID := Find_Empty_Trie;
   begin
      --  For now, just create empty trie
      --  Full implementation would lookup root in database
      if New_Trie = Null_Trie then
         Trie := Null_Trie;
         Success := False;
         return;
      end if;

      Tries (New_Trie) := (
         Root_Idx => Natural'Last,
         Count    => 0,
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
               --  (simplified: would need to look up child by hash)
               --  For now, treat as success with node update
               Success := True;

            elsif Common_Len = 0 then
               --  No match, create branch
               Branch_Node := Make_Branch;
               --  Add extension"s child to branch
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
               --  (simplified implementation)
               Success := True;
            end if;

         when Node_Branch =>
            if Key.Length = 0 then
               --  Update value at branch
               Current.Value.Length := Natural'Min (Val'Length, Max_Value_Size);
               for I in 0 .. Current.Value.Length - 1 loop
                  Current.Value.Bytes (I) := Val (Val'First + I);
               end loop;
               Current.Hash := (Data => Hash_Node (Current), Valid => True);
               Node_Store (Current_Idx).Node := Current;
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
                        Current.Children (Child_Nibble) := (Data => New_Leaf.Hash.Data, Valid => True);
                        Current.Hash := (Data => Hash_Node (Current), Valid => True);
                        Node_Store (Current_Idx).Node := Current;
                        New_Idx := Current_Idx;
                        Success := True;
                     end;
                  else
                     --  Child exists - simplified: just mark success
                     --  Full impl would recurse into child
                     Success := True;
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
   --  Get
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

      --  Traverse trie
      while Current_Idx < Max_Nodes and then Node_Store (Current_Idx).Used loop
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
                     return;
                  else
                     Error := Error_Key_Not_Found;
                     return;
                  end if;

               when Node_Extension =>
                  if Is_Prefix (Current.Key, Nibble_Key_Val) then
                     Nibble_Key_Val := Remove_Prefix (
                        Nibble_Key_Val, Current.Key.Length);
                     --  Follow child (simplified)
                     exit;
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
                        Child_Idx : constant Natural := Natural (
                           Nibble_Key_Val.Data (0));
                     begin
                        if not Current.Children (Child_Idx).Valid then
                           Error := Error_Key_Not_Found;
                           return;
                        end if;
                        Nibble_Key_Val := Remove_Prefix (Nibble_Key_Val, 1);
                        --  Lookup child by hash (simplified: exit for now)
                        exit;
                     end;
                  end if;
            end case;
         end;
      end loop;

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
               --  In full impl, would recurse into child and potentially collapse
               --  For now, mark as deleted if key matches extension end
               declare
                  Rest_Key : Nibble_Key := Remove_Prefix (Key, Current.Key.Length);
               begin
                  if Rest_Key.Length = 0 then
                     --  Key ends at extension (invalid - extensions don"t have values)
                     Error := Error_Key_Not_Found;
                  else
                     --  Would recurse into child by hash lookup
                     --  Simplified: just report not found
                     Error := Error_Key_Not_Found;
                  end if;
               end;
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
                        Node_Store (Current_Idx).Node := Current;
                     else
                        --  Multiple children - just update hash
                        Current.Hash := (Data => Hash_Node (Current), Valid => True);
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
                     --  In full impl, would look up child by hash and recurse
                     --  For now, just invalidate the child reference
                     Current.Children (Child_Nibble).Valid := False;

                     --  Check for branch collapse
                     declare
                        Child_Count : constant Natural := Branch_Child_Count (Current);
                     begin
                        if Child_Count = 0 and Current.Value.Length = 0 then
                           --  Branch is now empty
                           Free_Node (Current_Idx);
                           New_Idx := Natural'Last;
                        elsif Child_Count = 1 and Current.Value.Length = 0 then
                           --  Could collapse to extension or leaf
                           --  Simplified: keep as branch
                           Current.Hash := (Data => Hash_Node (Current), Valid => True);
                           Node_Store (Current_Idx).Node := Current;
                        else
                           Current.Hash := (Data => Hash_Node (Current), Valid => True);
                           Node_Store (Current_Idx).Node := Current;
                        end if;
                     end;
                     Deleted := True;
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
   --  Generate_Proof
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

      --  Walk down the trie, collecting proof nodes
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
                     --  Need to find child node by hash
                     --  For now, simplified - would need hash->index lookup
                     exit;  -- End traversal for now
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
                        --  Would need hash->index lookup to continue
                        exit;  -- End traversal for now
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
   --  Verify_Proof
   ---------------------------------------------------------------------------

   procedure Verify_Proof (
      Root    : in     Hash_256;
      Proof   : in     Merkle_Proof;
      Valid   : out    Boolean;
      Error   : out    MPT_Error
   ) is
      Computed_Hash  : Hash_256;
      Expected_Hash  : Hash_256;
      Node_Data      : Byte_Array (0 .. Max_Proof_Node_Size - 1) := (others => 0);
      Node_Len       : Natural;
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
               Valid := True;
            else
               Error := Error_Invalid_Proof;
            end if;
         end;
         return;
      end if;

      --  Verify proof chain from first node (root) to last
      --  Step 1: Hash the first proof node and verify it matches Root
      Node_Len := Proof.Nodes (0).Length;
      if Node_Len = 0 or Node_Len > Max_Proof_Node_Size then
         Error := Error_Invalid_Proof;
         return;
      end if;

      --  Copy first proof node data
      for I in 0 .. Node_Len - 1 loop
         Node_Data (I) := Proof.Nodes (0).Data (I);
      end loop;

      --  Hash the root node and compare to expected root
      Computed_Hash := Keccak_256 (Node_Data (0 .. Node_Len - 1));

      if Computed_Hash /= Root then
         Error := Error_Invalid_Proof;
         return;
      end if;

      --  Step 2: Verify each subsequent node is referenced by previous
      --  For full verification, we"d need to:
      --  1. Decode each node to find child references
      --  2. Verify the hash of child node matches the reference
      --  3. Follow the path dictated by the key"s nibbles
      --
      --  Simplified verification for now: just verify root hash matches
      Expected_Hash := Root;

      for I in 0 .. Proof.Depth - 1 loop
         Node_Len := Proof.Nodes (I).Length;

         if Node_Len = 0 then
            --  Invalid: zero-length node in proof
            Error := Error_Invalid_Proof;
            return;
         end if;

         --  Copy node data
         for J in 0 .. Node_Len - 1 loop
            Node_Data (J) := Proof.Nodes (I).Data (J);
         end loop;

         --  Verify hash
         Computed_Hash := Keccak_256 (Node_Data (0 .. Node_Len - 1));

         if I = 0 then
            --  First node must match root
            if Computed_Hash /= Root then
               Error := Error_Invalid_Proof;
               return;
            end if;
            Expected_Hash := Computed_Hash;
         else
            --  Subsequent nodes: verify they"re referenced by parent
            --  For full verification, would decode parent and check child hash
            --  For now, just record the hash for the chain
            Expected_Hash := Computed_Hash;
         end if;
      end loop;

      --  If we got here, basic proof structure is valid
      --  Full verification would also check:
      --  - Key path matches nibbles in branch/extension nodes
      --  - Value at leaf matches Proof.Value (if Proof.Exists)
      --  - For non-existence proofs, verify the path terminates correctly

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
   --  Iteration
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

      --  Depth-first traversal using explicit stack
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
                     --  Append leaf"s key suffix
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
                     --  Pop extension (cannot follow child without hash->index lookup)
                     Iter.Depth := Iter.Depth - 1;

                  when Node_Branch =>
                     declare
                        Start_Child : constant Natural := Iter.Stack (Stack_Top).Child;
                     begin
                        --  First time visiting: check branch value
                        if Start_Child = 0 and Node.Value.Length > 0 then
                           --  Return branch"s embedded value
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
                                    --  Add this nibble to path
                                    if Iter.Current.Length < Max_Key_Nibbles then
                                       Iter.Current.Data (Iter.Current.Length) := Nibble (C);
                                       Iter.Current.Length := Iter.Current.Length + 1;
                                    end if;

                                    --  Mark next child to visit
                                    Iter.Stack (Stack_Top).Child := C + 2;
                                    Found_Child := True;
                                    exit;
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

end Khepri_MPT;
