--  KHEPRI Storage Trie Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_Storage_Trie with
   SPARK_Mode => On,
   Refined_State => (Storage_Tries => (Contract_Map, Storage_Array))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Storage_Entry is record
      Contract : Contract_Address;
      Trie     : Khepri_MPT.Trie_ID;
      Used     : Boolean;
   end record;

   type Storage_Array_Type is array (Storage_ID) of Storage_Entry;

   --  Contract to storage mapping
   type Contract_Entry is record
      Contract    : Contract_Address;
      Storage_Ref : Storage_ID;
      Used        : Boolean;
   end record;

   type Contract_Map_Type is array (0 .. Max_Contracts - 1) of Contract_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Storage_Array : Storage_Array_Type := (others => (
      Contract => Address_Zero,
      Trie     => Khepri_MPT.Null_Trie,
      Used     => False
   ));

   Contract_Map : Contract_Map_Type := (others => (
      Contract    => Address_Zero,
      Storage_Ref => Null_Storage,
      Used        => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Empty_Storage return Storage_ID is
   begin
      for I in Storage_ID'Range loop
         if I /= Null_Storage and then not Storage_Array (I).Used then
            return I;
         end if;
      end loop;
      return Null_Storage;
   end Find_Empty_Storage;

   function Find_Contract_Entry (Contract : Contract_Address) return Natural is
   begin
      for I in Contract_Map'Range loop
         if Contract_Map (I).Used and then
            Contract_Map (I).Contract = Contract
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Contract_Entry;

   function Find_Empty_Contract_Entry return Natural is
   begin
      for I in Contract_Map'Range loop
         if not Contract_Map (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Contract_Entry;

   --  Convert U256 to 32-byte key
   procedure U256_To_Key (
      Value : in     U256;
      Key   : out    Byte_Array
   ) is
      Hash : constant Hash256 := To_Bytes_BE (Value);
   begin
      --  Initialize all elements first
      Key := (others => 0);
      --  Then copy hash bytes (up to 32 or Key'Length, whichever is smaller)
      for I in 0 .. Natural'Min (31, Key'Length - 1) loop
         Key (Key'First + I) := Hash (I);
      end loop;
   end U256_To_Key;

   --  Convert 32-byte value to U256
   function Bytes_To_U256 (Data : Byte_Array) return U256 is
      Result : U256 := U256_Zero;
      Max_Idx : constant Natural := (if Data'Length > 0 then
                                        Natural'Min (31, Data'Length - 1)
                                     else 0);
   begin
      if Data'Length = 0 then
         return U256_Zero;
      end if;
      for I in 0 .. Max_Idx loop
         Result := Shift_Left (Result, 8);
         Result := Bit_Or (Result,
            From_Word64 (Word64 (Data (Data'First + I))));
      end loop;
      return Result;
   end Bytes_To_U256;

   --  Convert Node_Value to Byte_Array
   function Node_Value_To_Bytes (
      V      : Node_Value;
      Length : Natural
   ) return Byte_Array is
      Result : Byte_Array (0 .. Length - 1) := (others => 0);
   begin
      for I in 0 .. Length - 1 loop
         if I <= V'Last then
            Result (I) := V (I);
         end if;
      end loop;
      return Result;
   end Node_Value_To_Bytes;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Create_Storage (
      Contract : in     Address;
      Storage  : out    Storage_ID;
      Success  : out    Boolean
   ) is
      New_Storage   : Storage_ID;
      New_Trie      : Trie_ID;
      Trie_Success  : Boolean;
      Map_Slot      : Natural;
      Existing_Slot : Natural;
   begin
      Storage := Null_Storage;
      Success := False;

      --  Check if storage already exists
      Existing_Slot := Find_Contract_Entry (Contract);
      if Existing_Slot /= Natural'Last and then Existing_Slot in Contract_Map'Range then
         Storage := Contract_Map (Existing_Slot).Storage_Ref;
         Success := True;
         return;
      end if;

      --  Find empty slots
      New_Storage := Find_Empty_Storage;
      if New_Storage = Null_Storage then
         return;
      end if;

      Map_Slot := Find_Empty_Contract_Entry;
      if Map_Slot = Natural'Last then
         return;
      end if;

      --  Create trie
      Create_Trie (New_Trie, Trie_Success);
      if not Trie_Success then
         return;
      end if;

      --  Initialize storage entry
      Storage_Array (New_Storage) := (
         Contract => Contract,
         Trie     => New_Trie,
         Used     => True
      );

      --  Update contract map
      Contract_Map (Map_Slot) := (
         Contract   => Contract,
         Storage_Ref => New_Storage,
         Used       => True
      );

      Storage := New_Storage;
      Success := True;
   end Create_Storage;

   procedure Load_Storage (
      Contract     : in     Address;
      Storage_Root : in     Hash_256;
      Storage      : out    Storage_ID;
      Success      : out    Boolean
   ) is
      New_Storage  : Storage_ID;
      New_Trie     : Trie_ID;
      Trie_Success : Boolean;
      Map_Slot     : Natural;
   begin
      Storage := Null_Storage;
      Success := False;

      --  Find empty slots
      New_Storage := Find_Empty_Storage;
      if New_Storage = Null_Storage then
         return;
      end if;

      Map_Slot := Find_Empty_Contract_Entry;
      if Map_Slot = Natural'Last then
         return;
      end if;

      --  Load trie from root
      Load_Trie (Storage_Root, New_Trie, Trie_Success);
      if not Trie_Success then
         return;
      end if;

      --  Initialize storage entry
      Storage_Array (New_Storage) := (
         Contract => Contract,
         Trie     => New_Trie,
         Used     => True
      );

      --  Update contract map
      Contract_Map (Map_Slot) := (
         Contract   => Contract,
         Storage_Ref => New_Storage,
         Used       => True
      );

      Storage := New_Storage;
      Success := True;
   end Load_Storage;

   procedure Destroy_Storage (Storage : in Storage_ID) is
   begin
      if Storage /= Null_Storage and then Storage_Array (Storage).Used then
         --  Destroy trie
         Destroy_Trie (Storage_Array (Storage).Trie);

         --  Clear contract map entry
         for I in Contract_Map'Range loop
            if Contract_Map (I).Used and then
               Contract_Map (I).Storage_Ref = Storage
            then
               Contract_Map (I).Used := False;
               exit;
            end if;
         end loop;

         --  Clear storage entry
         Storage_Array (Storage).Used := False;
         Storage_Array (Storage).Trie := Khepri_MPT.Null_Trie;
      end if;
   end Destroy_Storage;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   procedure SLoad (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Value   : out    U256;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) is
      Key       : Byte_Array (0 .. 31) := (others => 0);
      Val_Data  : Value_Data;
      Found     : Boolean;
      MPT_Error : Khepri_MPT_Types.MPT_Error;
   begin
      Value := U256_Zero;
      Success := False;
      Error := Error_None;

      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         Error := Error_Invalid;
         return;
      end if;

      U256_To_Key (Slot, Key);

      Get (Storage_Array (Storage).Trie, Key, Val_Data, Found, MPT_Error);

      if MPT_Error /= Khepri_MPT_Types.Error_None and then
         MPT_Error /= Khepri_MPT_Types.Error_Key_Not_Found
      then
         Error := Error_Trie_Error;
         return;
      end if;

      if Found then
         declare
            Val_Bytes : constant Byte_Array :=
               Node_Value_To_Bytes (Val_Data.Bytes, Val_Data.Length);
         begin
            Value := Bytes_To_U256 (Val_Bytes);
         end;
      else
         Value := U256_Zero;
      end if;

      Success := True;
   end SLoad;

   procedure SStore (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Value   : in     U256;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) is
      Key       : Byte_Array (0 .. 31) := (others => 0);
      Val       : Byte_Array (0 .. 31) := (others => 0);
      MPT_Error : Khepri_MPT_Types.MPT_Error;
   begin
      Success := False;
      Error := Error_None;

      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         Error := Error_Invalid;
         return;
      end if;

      U256_To_Key (Slot, Key);
      U256_To_Key (Value, Val);

      --  If value is zero, delete the slot
      if Equal (Value, U256_Zero) then
         Delete (Storage_Array (Storage).Trie, Key, Success, MPT_Error);
         if MPT_Error = Khepri_MPT_Types.Error_Key_Not_Found then
            Success := True;  --  Deleting non-existent is OK
         elsif MPT_Error /= Khepri_MPT_Types.Error_None then
            Error := Error_Trie_Error;
         end if;
      else
         Put (Storage_Array (Storage).Trie, Key, Val, Success, MPT_Error);
         if MPT_Error /= Khepri_MPT_Types.Error_None then
            Error := Error_Trie_Error;
         end if;
      end if;
   end SStore;

   procedure SDelete (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) is
      Key       : Byte_Array (0 .. 31) := (others => 0);
      MPT_Error : Khepri_MPT_Types.MPT_Error;
   begin
      Success := False;
      Error := Error_None;

      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         Error := Error_Invalid;
         return;
      end if;

      U256_To_Key (Slot, Key);

      Delete (Storage_Array (Storage).Trie, Key, Success, MPT_Error);

      if MPT_Error = Khepri_MPT_Types.Error_Key_Not_Found then
         Success := True;
      elsif MPT_Error /= Khepri_MPT_Types.Error_None then
         Error := Error_Trie_Error;
      end if;
   end SDelete;

   function Slot_Exists (
      Storage : Storage_ID;
      Slot    : U256
   ) return Boolean
   is
      Key : Byte_Array (0 .. 31) := (others => 0);
   begin
      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         return False;
      end if;

      U256_To_Key (Slot, Key);
      return Contains (Storage_Array (Storage).Trie, Key);
   end Slot_Exists;

   ---------------------------------------------------------------------------
   --  Storage Root
   ---------------------------------------------------------------------------

   function Get_Root (Storage : Storage_ID) return Hash_256 is
   begin
      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         return Empty_Hash;
      end if;
      return Root_Hash (Storage_Array (Storage).Trie);
   end Get_Root;

   function Is_Empty (Storage : Storage_ID) return Boolean is
   begin
      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         return True;
      end if;
      return Khepri_MPT.Is_Empty (Storage_Array (Storage).Trie);
   end Is_Empty;

   function Slot_Count (Storage : Storage_ID) return Natural is
   begin
      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         return 0;
      end if;
      return Node_Count (Storage_Array (Storage).Trie);
   end Slot_Count;

   ---------------------------------------------------------------------------
   --  Batch Operations
   ---------------------------------------------------------------------------

   procedure Clear_Storage (
      Storage : in     Storage_ID;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) is
      New_Trie     : Trie_ID;
      Trie_Success : Boolean;
   begin
      Success := False;
      Error := Error_None;

      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         Error := Error_Invalid;
         return;
      end if;

      --  Destroy old trie and create new empty one
      Destroy_Trie (Storage_Array (Storage).Trie);
      Create_Trie (New_Trie, Trie_Success);

      if not Trie_Success then
         Error := Error_Trie_Error;
         return;
      end if;

      Storage_Array (Storage).Trie := New_Trie;
      Success := True;
   end Clear_Storage;

   ---------------------------------------------------------------------------
   --  Snapshots
   ---------------------------------------------------------------------------

   procedure Create_Snapshot (
      Storage     : in     Storage_ID;
      Snapshot_ID : out    Natural;
      Success     : out    Boolean
   ) is
   begin
      Snapshot_ID := 0;
      Success := False;

      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         return;
      end if;

      Khepri_MPT.Create_Snapshot (
         Storage_Array (Storage).Trie, Snapshot_ID, Success);
   end Create_Snapshot;

   procedure Revert_Snapshot (
      Storage     : in     Storage_ID;
      Snapshot_ID : in     Natural;
      Success     : out    Boolean
   ) is
      Error : Khepri_MPT_Types.MPT_Error;
   begin
      Success := False;

      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         return;
      end if;

      Restore_Snapshot (
         Storage_Array (Storage).Trie, Snapshot_ID, Success, Error);
   end Revert_Snapshot;

   procedure Discard_Snapshot (
      Storage     : in     Storage_ID;
      Snapshot_ID : in     Natural
   ) is
   begin
      if Storage /= Null_Storage and then Storage_Array (Storage).Used then
         Khepri_MPT.Discard_Snapshot (
            Storage_Array (Storage).Trie, Snapshot_ID);
      end if;
   end Discard_Snapshot;

   ---------------------------------------------------------------------------
   --  Proofs
   ---------------------------------------------------------------------------

   procedure Generate_Storage_Proof (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Proof   : out    Merkle_Proof;
      Success : out    Boolean
   ) is
      Key   : Byte_Array (0 .. 31) := (others => 0);
      Error : Khepri_MPT_Types.MPT_Error;
   begin
      Proof := (
         Nodes  => (others => (Data => (others => 0), Length => 0)),
         Depth  => 0,
         Key    => Empty_Nibble_Key,
         Value  => Empty_Value,
         Exists => False
      );
      Success := False;

      if Storage = Null_Storage or else not Storage_Array (Storage).Used then
         return;
      end if;

      U256_To_Key (Slot, Key);
      Generate_Proof (Storage_Array (Storage).Trie, Key, Proof, Success, Error);
   end Generate_Storage_Proof;

   procedure Verify_Storage_Proof (
      Root    : in     Hash_256;
      Slot    : in     U256;
      Proof   : in     Merkle_Proof;
      Value   : out    U256;
      Valid   : out    Boolean
   ) is
      Error : Khepri_MPT_Types.MPT_Error;
   begin
      Value := U256_Zero;

      Verify_Proof (Root, Proof, Valid, Error);

      if Valid and then Proof.Exists then
         declare
            Val_Bytes : constant Byte_Array :=
               Node_Value_To_Bytes (Proof.Value.Bytes, Proof.Value.Length);
         begin
            Value := Bytes_To_U256 (Val_Bytes);
         end;
      end if;
   end Verify_Storage_Proof;

   ---------------------------------------------------------------------------
   --  Lookup
   ---------------------------------------------------------------------------

   function Find_Storage (Contract : Address) return Storage_ID is
      Idx : constant Natural := Find_Contract_Entry (Contract);
   begin
      if Idx /= Natural'Last and then Idx in Contract_Map'Range then
         return Contract_Map (Idx).Storage_Ref;
      else
         return Null_Storage;
      end if;
   end Find_Storage;

end Khepri_Storage_Trie;
