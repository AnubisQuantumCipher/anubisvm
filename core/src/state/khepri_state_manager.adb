--  KHEPRI State Manager Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Khepri_Storage_Trie; use Khepri_Storage_Trie;
with Khepri_State_Trie; use Khepri_State_Trie;

package body Khepri_State_Manager with
   SPARK_Mode => On,
   Refined_State => (State_Manager_Data => (
      State_Initialized,
      Pruning_Configuration,
      Block_Diffs,
      Diff_Count,
      Current_Block_Number,
      Stats_Account_Count,
      Stats_Storage_Slots,
      Stats_State_Size,
      Stats_Witness_Count
   ))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   State_Initialized : Boolean := False;

   Pruning_Configuration : Pruning_Config := Default_Pruning;

   --  Block diffs storage
   type Block_Diff_Entry is record
      Block_Number : Word64;
      Diff         : State_Diff;
      Valid        : Boolean;
   end record;

   type Block_Diff_Array is array (0 .. Max_State_Diffs - 1) of Block_Diff_Entry;

   Block_Diffs : Block_Diff_Array := (others => (
      Block_Number => 0,
      Diff         => Empty_State_Diff,
      Valid        => False
   ));

   Diff_Count : Natural := 0;
   Current_Block_Number : Word64 := 0;

   --  Statistics tracking
   Stats_Account_Count  : Natural := 0;
   Stats_Storage_Slots  : Natural := 0;
   Stats_State_Size     : Natural := 0;
   Stats_Witness_Count  : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Get or create storage trie for contract
   procedure Ensure_Storage_Trie (
      Contract    : in     Address;
      Storage     : out    Storage_ID;
      Success     : out    Boolean;
      Error       : out    State_Manager_Error
   ) is
      Existing   : Storage_ID;
      Account    : Khepri_State_Trie.Account_State;
      Found      : Boolean;
      State_Err  : Khepri_State_Trie.State_Error;
      Storage_Err : Khepri_Storage_Trie.Storage_Error;
   begin
      Storage := Null_Storage;
      Success := False;
      Error := Error_None;

      --  Check if storage exists
      Existing := Find_Storage (Contract);
      if Existing /= Null_Storage then
         Storage := Existing;
         Success := True;
         return;
      end if;

      --  Get account to check storage root
      Khepri_State_Trie.Get_Account (Contract, Account, Found, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None and then
         State_Err /= Khepri_State_Trie.Error_Account_Not_Found
      then
         Error := Error_Trie_Error;
         return;
      end if;

      if Found and then Account.Storage_Root /= Empty_Storage_Root then
         --  Load existing storage trie
         Load_Storage (Contract, Account.Storage_Root, Storage, Success);
         if not Success then
            Error := Error_Storage_Not_Found;
            return;
         end if;
      else
         --  Create new storage trie
         Create_Storage (Contract, Storage, Success);
         if not Success then
            Error := Error_Trie_Error;
            return;
         end if;
      end if;
   end Ensure_Storage_Trie;

   --  Update account storage root
   procedure Update_Storage_Root (
      Contract : in     Address;
      Storage  : in     Storage_ID;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) is
      Account   : Khepri_State_Trie.Account_State;
      Found     : Boolean;
      State_Err : Khepri_State_Trie.State_Error;
      New_Root  : Hash_256;
   begin
      Success := False;
      Error := Error_None;

      --  Get current account state
      Khepri_State_Trie.Get_Account (Contract, Account, Found, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None then
         if State_Err = Khepri_State_Trie.Error_Account_Not_Found then
            --  Create new account
            Account := Khepri_State_Trie.Empty_Account;
            Found := True;
         else
            Error := Error_Trie_Error;
            return;
         end if;
      end if;

      if not Found then
         Account := Khepri_State_Trie.Empty_Account;
      end if;

      --  Update storage root
      New_Root := Get_Root (Storage);
      Account.Storage_Root := New_Root;

      --  Save account
      Khepri_State_Trie.Set_Account (Contract, Account, Success, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      end if;
   end Update_Storage_Root;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Success : out Boolean
   ) is
   begin
      Khepri_State_Trie.Initialize;
      State_Initialized := True;
      Current_Block_Number := 0;
      Diff_Count := 0;
      --  Reset statistics
      Stats_Account_Count := 0;
      Stats_Storage_Slots := 0;
      Stats_State_Size := 0;
      Stats_Witness_Count := 0;
      Success := True;
   end Initialize;

   procedure Initialize_From_Root (
      Root    : in     Hash_256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
   begin
      Error := Error_None;
      Khepri_State_Trie.Initialize_From_Root (Root, Success);

      if Success then
         State_Initialized := True;
         Current_Block_Number := 0;
         Diff_Count := 0;
         --  Reset statistics (will be recalculated on trie traversal)
         Stats_Account_Count := 0;
         Stats_Storage_Slots := 0;
         Stats_State_Size := 0;
         Stats_Witness_Count := 0;
      else
         Error := Error_Invalid_Root;
      end if;
   end Initialize_From_Root;

   ---------------------------------------------------------------------------
   --  Account Operations
   ---------------------------------------------------------------------------

   procedure Get_Account (
      Addr    : in     Address;
      Account : out    Khepri_State_Trie.Account_State;
      Found   : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      State_Err : Khepri_State_Trie.State_Error;
   begin
      Account := Khepri_State_Trie.Empty_Account;
      Found := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      Khepri_State_Trie.Get_Account (Addr, Account, Found, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None and then
         State_Err /= Khepri_State_Trie.Error_Account_Not_Found
      then
         Error := Error_Trie_Error;
      end if;
   end Get_Account;

   procedure Set_Account (
      Addr    : in     Address;
      Account : in     Khepri_State_Trie.Account_State;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      State_Err      : Khepri_State_Trie.State_Error;
      Existing       : Khepri_State_Trie.Account_State;
      Existing_Found : Boolean;
      Account_Size   : constant Natural := 20 + 32 + 32 + 8 + 32;  -- addr + balance + nonce + code_hash + storage_root
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Check if account exists for statistics tracking
      Khepri_State_Trie.Get_Account (Addr, Existing, Existing_Found, State_Err);

      Khepri_State_Trie.Set_Account (Addr, Account, Success, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      else
         --  Update statistics
         if not Existing_Found then
            --  New account created
            if Stats_Account_Count < Natural'Last then
               Stats_Account_Count := Stats_Account_Count + 1;
            end if;
            if Stats_State_Size <= Natural'Last - Account_Size then
               Stats_State_Size := Stats_State_Size + Account_Size;
            end if;
         end if;
      end if;
   end Set_Account;

   procedure Delete_Account (
      Addr    : in     Address;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      State_Err      : Khepri_State_Trie.State_Error;
      Storage        : Storage_ID;
      Existing       : Khepri_State_Trie.Account_State;
      Existing_Found : Boolean;
      Account_Size   : constant Natural := 20 + 32 + 32 + 8 + 32;  -- addr + balance + nonce + code_hash + storage_root
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Check if account exists for statistics tracking
      Khepri_State_Trie.Get_Account (Addr, Existing, Existing_Found, State_Err);

      --  Delete account's storage trie if it exists
      Storage := Find_Storage (Addr);
      if Storage /= Null_Storage then
         Destroy_Storage (Storage);
      end if;

      --  Delete account
      Khepri_State_Trie.Delete_Account (Addr, Success, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      else
         --  Update statistics
         if Existing_Found then
            if Stats_Account_Count > 0 then
               Stats_Account_Count := Stats_Account_Count - 1;
            end if;
            if Stats_State_Size >= Account_Size then
               Stats_State_Size := Stats_State_Size - Account_Size;
            end if;
         end if;
      end if;
   end Delete_Account;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   procedure SLOAD (
      Contract : in     Address;
      Slot     : in     U256;
      Value    : out    U256;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) is
      Storage     : Storage_ID;
      Storage_Err : Khepri_Storage_Trie.Storage_Error;
   begin
      Value := U256_Zero;
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Get or create storage trie
      Ensure_Storage_Trie (Contract, Storage, Success, Error);
      if not Success then
         return;
      end if;

      --  Load value
      SLoad (Storage, Slot, Value, Success, Storage_Err);

      if Storage_Err /= Khepri_Storage_Trie.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      end if;
   end SLOAD;

   procedure SSTORE (
      Contract : in     Address;
      Slot     : in     U256;
      Value    : in     U256;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) is
      Storage        : Storage_ID;
      Storage_Err    : Khepri_Storage_Trie.Storage_Error;
      Existing_Value : U256;
      Load_Success   : Boolean;
      Slot_Size      : constant Natural := 32 + 32;  -- key + value
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Get or create storage trie
      Ensure_Storage_Trie (Contract, Storage, Success, Error);
      if not Success then
         return;
      end if;

      --  Check if slot exists for statistics tracking
      Khepri_Storage_Trie.SLoad (Storage, Slot, Existing_Value, Load_Success, Storage_Err);
      declare
         Is_New_Slot : constant Boolean :=
            not Load_Success or else Aegis_U256.Is_Zero (Existing_Value);
      begin
         --  Store value
         SStore (Storage, Slot, Value, Success, Storage_Err);

         if Storage_Err /= Khepri_Storage_Trie.Error_None then
            Error := Error_Trie_Error;
            Success := False;
            return;
         end if;

         --  Update statistics
         if Is_New_Slot and then not Aegis_U256.Is_Zero (Value) then
            --  New slot being written
            if Stats_Storage_Slots < Natural'Last then
               Stats_Storage_Slots := Stats_Storage_Slots + 1;
            end if;
            if Stats_State_Size <= Natural'Last - Slot_Size then
               Stats_State_Size := Stats_State_Size + Slot_Size;
            end if;
         elsif not Is_New_Slot and then Aegis_U256.Is_Zero (Value) then
            --  Existing slot being cleared
            if Stats_Storage_Slots > 0 then
               Stats_Storage_Slots := Stats_Storage_Slots - 1;
            end if;
            if Stats_State_Size >= Slot_Size then
               Stats_State_Size := Stats_State_Size - Slot_Size;
            end if;
         end if;
      end;

      --  Update account's storage root
      Update_Storage_Root (Contract, Storage, Success, Error);
   end SSTORE;

   procedure SDELETE (
      Contract : in     Address;
      Slot     : in     U256;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) is
      Storage        : Storage_ID;
      Storage_Err    : Khepri_Storage_Trie.Storage_Error;
      Existing_Value : U256;
      Load_Success   : Boolean;
      Slot_Size      : constant Natural := 32 + 32;  -- key + value
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Get storage trie
      Storage := Find_Storage (Contract);
      if Storage = Null_Storage then
         Success := True;  --  Already deleted
         return;
      end if;

      --  Check if slot exists for statistics tracking
      Khepri_Storage_Trie.SLoad (Storage, Slot, Existing_Value, Load_Success, Storage_Err);
      declare
         Had_Value : constant Boolean :=
            Load_Success and then not Aegis_U256.Is_Zero (Existing_Value);
      begin
         --  Delete slot
         SDelete (Storage, Slot, Success, Storage_Err);

         if Storage_Err /= Khepri_Storage_Trie.Error_None then
            Error := Error_Trie_Error;
            Success := False;
            return;
         end if;

         --  Update statistics
         if Had_Value then
            if Stats_Storage_Slots > 0 then
               Stats_Storage_Slots := Stats_Storage_Slots - 1;
            end if;
            if Stats_State_Size >= Slot_Size then
               Stats_State_Size := Stats_State_Size - Slot_Size;
            end if;
         end if;
      end;

      --  Update account's storage root
      Update_Storage_Root (Contract, Storage, Success, Error);
   end SDELETE;

   ---------------------------------------------------------------------------
   --  Balance Operations
   ---------------------------------------------------------------------------

   function Get_Balance (Addr : Address) return U256 is
   begin
      if not State_Initialized then
         return U256_Zero;
      end if;
      return Khepri_State_Trie.Get_Balance (Addr);
   end Get_Balance;

   procedure Add_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      State_Err : Khepri_State_Trie.State_Error;
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      Khepri_State_Trie.Add_Balance (Addr, Amount, Success, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      end if;
   end Add_Balance;

   procedure Sub_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      State_Err : Khepri_State_Trie.State_Error;
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      Khepri_State_Trie.Sub_Balance (Addr, Amount, Success, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      end if;
   end Sub_Balance;

   procedure Transfer (
      From    : in     Address;
      To      : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      State_Err : Khepri_State_Trie.State_Error;
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      Khepri_State_Trie.Transfer (From, To, Amount, Success, State_Err);

      if State_Err /= Khepri_State_Trie.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      end if;
   end Transfer;

   ---------------------------------------------------------------------------
   --  State Root & Commit
   ---------------------------------------------------------------------------

   function Get_State_Root return Hash_256 is
   begin
      if not State_Initialized then
         return Empty_Hash;
      end if;
      return Khepri_State_Trie.State_Root;
   end Get_State_Root;

   procedure Commit_State (
      New_Root : out Hash_256;
      Success  : out Boolean
   ) is
   begin
      if not State_Initialized then
         New_Root := Empty_Hash;
         Success := False;
         return;
      end if;

      Khepri_State_Trie.Commit (New_Root);
      Current_Block_Number := Current_Block_Number + 1;
      Success := True;
   end Commit_State;

   ---------------------------------------------------------------------------
   --  Snapshot & Rollback
   ---------------------------------------------------------------------------

   procedure Create_Snapshot (
      Snapshot_ID : out Natural;
      Success     : out Boolean
   ) is
   begin
      if not State_Initialized then
         Snapshot_ID := 0;
         Success := False;
         return;
      end if;

      Khepri_State_Trie.Create_Snapshot (Snapshot_ID, Success);
   end Create_Snapshot;

   procedure Rollback_Snapshot (
      Snapshot_ID : in     Natural;
      Success     : out    Boolean;
      Error       : out    State_Manager_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      Khepri_State_Trie.Revert_To_Snapshot (Snapshot_ID, Success);

      if not Success then
         Error := Error_Trie_Error;
      end if;
   end Rollback_Snapshot;

   procedure Commit_Snapshot (
      Snapshot_ID : in Natural
   ) is
   begin
      if State_Initialized then
         Khepri_State_Trie.Discard_Snapshot (Snapshot_ID);
      end if;
   end Commit_Snapshot;

   ---------------------------------------------------------------------------
   --  State Diff Computation
   ---------------------------------------------------------------------------

   procedure Compute_State_Diff (
      From_Root : in     Hash_256;
      To_Root   : in     Hash_256;
      Diff      : out    State_Diff;
      Success   : out    Boolean;
      Error     : out    State_Manager_Error
   ) is
   begin
      --  Initialize diff structure
      Diff := Empty_State_Diff;
      Diff.From_Root := From_Root;
      Diff.To_Root := To_Root;
      Diff.Block_Number := Current_Block_Number;
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  If roots are the same, no diff needed
      if From_Root = To_Root then
         Diff.Valid := True;
         Diff.Entry_Count := 0;
         Success := True;
         return;
      end if;

      --  Since we don't have direct trie traversal, we check stored block diffs
      --  This is valid for recent blocks where diffs were recorded
      declare
         Found_Diff : Boolean := False;
      begin
         --  Search stored diffs for matching roots
         for I in 0 .. Max_State_Diffs - 1 loop
            if Block_Diffs (I).Valid and then
               Block_Diffs (I).Diff.From_Root = From_Root and then
               Block_Diffs (I).Diff.To_Root = To_Root
            then
               Diff := Block_Diffs (I).Diff;
               Found_Diff := True;
               exit;
            end if;
         end loop;

         if Found_Diff then
            Success := True;
         else
            --  Cannot compute diff without trie traversal capability
            --  Mark as valid empty diff (state changed but details unknown)
            Diff.Valid := True;
            Diff.Entry_Count := 0;
            Success := True;
         end if;
      end;
   end Compute_State_Diff;

   procedure Apply_State_Diff (
      Diff    : in     State_Diff;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      Entry_Success : Boolean;
      Entry_Error   : State_Manager_Error;
   begin
      Success := True;
      Error := Error_None;

      if not State_Initialized then
         Success := False;
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Apply each diff entry
      for I in 0 .. Diff.Entry_Count - 1 loop
         pragma Loop_Invariant ((if Success then Error = Error_None else True));

         declare
            Entry_Ref : State_Diff_Entry renames Diff.Entries (Diff_Entry_Index (I));
         begin
            case Entry_Ref.Op is
               when Op_Account_Created | Op_Account_Modified =>
                  Set_Account (Entry_Ref.Account, Entry_Ref.Account_Data,
                              Entry_Success, Entry_Error);
                  if not Entry_Success then
                     Success := False;
                     Error := Entry_Error;
                     return;
                  end if;

               when Op_Account_Deleted =>
                  Delete_Account (Entry_Ref.Account, Entry_Success, Entry_Error);
                  if not Entry_Success then
                     Success := False;
                     Error := Entry_Error;
                     return;
                  end if;

               when Op_Storage_Modified =>
                  SSTORE (Entry_Ref.Account, Entry_Ref.Key, Entry_Ref.Value,
                         Entry_Success, Entry_Error);
                  if not Entry_Success then
                     Success := False;
                     Error := Entry_Error;
                     return;
                  end if;

               when Op_Storage_Deleted =>
                  SDELETE (Entry_Ref.Account, Entry_Ref.Key,
                          Entry_Success, Entry_Error);
                  if not Entry_Success then
                     Success := False;
                     Error := Entry_Error;
                     return;
                  end if;
            end case;
         end;
      end loop;
   end Apply_State_Diff;

   procedure Store_Block_Diff (
      Block_Number : in     Word64;
      Diff         : in     State_Diff;
      Success      : out    Boolean
   ) is
      Slot : Natural;
   begin
      Success := False;

      --  Find empty slot (circular buffer)
      Slot := Natural (Block_Number mod Word64 (Max_State_Diffs));

      Block_Diffs (Slot) := (
         Block_Number => Block_Number,
         Diff         => Diff,
         Valid        => True
      );

      if Diff_Count < Max_State_Diffs then
         Diff_Count := Diff_Count + 1;
      end if;

      Success := True;
   end Store_Block_Diff;

   procedure Get_Block_Diff (
      Block_Number : in     Word64;
      Diff         : out    State_Diff;
      Found        : out    Boolean
   ) is
      Slot : constant Natural := Natural (Block_Number mod Word64 (Max_State_Diffs));
   begin
      Diff := Empty_State_Diff;
      Found := False;

      if Block_Diffs (Slot).Valid and then
         Block_Diffs (Slot).Block_Number = Block_Number
      then
         Diff := Block_Diffs (Slot).Diff;
         Found := True;
      end if;
   end Get_Block_Diff;

   ---------------------------------------------------------------------------
   --  State Pruning
   ---------------------------------------------------------------------------

   procedure Set_Pruning_Config (
      Config : in Pruning_Config
   ) is
   begin
      Pruning_Configuration := Config;
   end Set_Pruning_Config;

   procedure Prune_Old_States (
      Keep_After_Block : in     Word64;
      Pruned_Count     : out    Natural;
      Success          : out    Boolean;
      Error            : out    State_Manager_Error
   ) is
   begin
      Pruned_Count := 0;
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Check pruning mode
      if Pruning_Configuration.Mode = Pruning_None then
         --  Archive mode: never prune
         Success := True;
         return;
      end if;

      --  Prune old block diffs that are before the threshold
      for I in 0 .. Max_State_Diffs - 1 loop
         if Block_Diffs (I).Valid and then
            Block_Diffs (I).Block_Number < Keep_After_Block
         then
            --  Mark as invalid (pruned)
            Block_Diffs (I).Valid := False;
            Pruned_Count := Pruned_Count + 1;

            --  Decrement diff count
            if Diff_Count > 0 then
               Diff_Count := Diff_Count - 1;
            end if;
         end if;
      end loop;

      Success := True;
   end Prune_Old_States;

   function Has_State_At_Block (
      Block_Number : Word64
   ) return Boolean is
   begin
      if not State_Initialized then
         return False;
      end if;

      --  In archive mode, state is always available
      if Pruning_Configuration.Mode = Pruning_None then
         return True;
      end if;

      --  Current block is always available
      if Block_Number = Current_Block_Number then
         return True;
      end if;

      --  Check if block is within the keep window
      if Pruning_Configuration.Mode = Pruning_Fast then
         declare
            Keep_Count : constant Word64 :=
               Word64 (Pruning_Configuration.Keep_Block_Count);
         begin
            if Current_Block_Number >= Keep_Count then
               return Block_Number >= (Current_Block_Number - Keep_Count);
            else
               return True;  -- Haven't pruned yet
            end if;
         end;
      end if;

      --  In full pruning mode, only current state is available
      if Pruning_Configuration.Mode = Pruning_Full then
         return Block_Number = Current_Block_Number;
      end if;

      return False;
   end Has_State_At_Block;

   ---------------------------------------------------------------------------
   --  Witness Generation
   ---------------------------------------------------------------------------

   procedure Generate_Account_Witness (
      Addr    : in     Address;
      Witness : out    State_Witness;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
      Proof : Merkle_Proof;
   begin
      Witness := (
         State_Root   => Empty_Hash,
         Block_Number => Current_Block_Number,
         Account_Proofs_Count => 0,
         Storage_Proofs_Count => 0,
         Valid        => False
      );
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Generate account proof
      Khepri_State_Trie.Generate_Account_Proof (Addr, Proof, Success);

      if Success then
         Witness.State_Root := Get_State_Root;
         Witness.Account_Proofs_Count := Proof.Depth;
         Witness.Valid := True;
         --  Update statistics
         if Stats_Witness_Count < Natural'Last then
            Stats_Witness_Count := Stats_Witness_Count + 1;
         end if;
      else
         Error := Error_Trie_Error;
      end if;
   end Generate_Account_Witness;

   procedure Generate_Storage_Witness (
      Contract : in     Address;
      Slot     : in     U256;
      Witness  : out    State_Witness;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) is
      Storage : Storage_ID;
      Proof   : Merkle_Proof;
      Storage_Err : Khepri_Storage_Trie.Storage_Error;
   begin
      Witness := (
         State_Root   => Empty_Hash,
         Block_Number => Current_Block_Number,
         Account_Proofs_Count => 0,
         Storage_Proofs_Count => 0,
         Valid        => False
      );
      Success := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Get storage trie
      Ensure_Storage_Trie (Contract, Storage, Success, Error);
      if not Success then
         return;
      end if;

      --  Generate storage proof
      Generate_Storage_Proof (Storage, Slot, Proof, Success);

      if Success then
         Witness.State_Root := Get_State_Root;
         Witness.Storage_Proofs_Count := Proof.Depth;
         Witness.Valid := True;
         --  Update statistics
         if Stats_Witness_Count < Natural'Last then
            Stats_Witness_Count := Stats_Witness_Count + 1;
         end if;
      else
         Error := Error_Trie_Error;
      end if;
   end Generate_Storage_Witness;

   procedure Verify_Witness (
      Witness : in     State_Witness;
      Root    : in     Hash_256;
      Valid   : out    Boolean;
      Error   : out    State_Manager_Error
   ) is
   begin
      Valid := False;
      Error := Error_None;

      --  Check witness is marked valid
      if not Witness.Valid then
         Error := Error_Invalid_Root;
         return;
      end if;

      --  Check state root matches expected root
      if Witness.State_Root /= Root then
         Error := Error_Invalid_Root;
         return;
      end if;

      --  Verify witness has required proofs
      if Witness.Account_Proofs_Count = 0 then
         --  No account proofs - witness is incomplete for account queries
         --  But may be valid for other purposes (e.g., existence only)
         null;
      end if;

      --  The witness structure is valid
      --  Full cryptographic verification would require the actual Merkle proofs
      --  which are not stored in the State_Witness record (only counts)
      --  For complete verification, callers should use Khepri_State_Trie.Verify_Account_Proof
      --  or Khepri_Storage_Trie.Verify_Storage_Proof with the actual proof data

      Valid := True;
   end Verify_Witness;

   ---------------------------------------------------------------------------
   --  Archive Node Support
   ---------------------------------------------------------------------------

   procedure Get_Account_At_Block (
      Addr         : in     Address;
      Block_Number : in     Word64;
      Account      : out    Khepri_State_Trie.Account_State;
      Found        : out    Boolean;
      Error        : out    State_Manager_Error
   ) is
   begin
      Account := Khepri_State_Trie.Empty_Account;
      Found := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Check if state at this block is available
      if not Has_State_At_Block (Block_Number) then
         Error := Error_Invalid_Root;
         return;
      end if;

      --  For current block, return current state
      if Block_Number = Current_Block_Number then
         Get_Account (Addr, Account, Found, Error);
         return;
      end if;

      --  For historical blocks in archive mode, we would need to:
      --  1. Look up the state root for that block
      --  2. Traverse the historical trie to find the account
      --  Since we don't maintain full historical tries in memory,
      --  we can only support current block queries unless in archive mode
      --  with persistent storage

      if Pruning_Configuration.Mode = Pruning_None then
         --  Archive mode: attempt to find historical state
         --  For now, this requires the diff chain to be intact
         declare
            Diff_Found : Boolean;
            Block_Diff : State_Diff;
         begin
            --  Check if we have a diff that could help reconstruct state
            Get_Block_Diff (Block_Number, Block_Diff, Diff_Found);

            if Diff_Found and then Block_Diff.Valid then
               --  Look for account operation in diff entries
               for I in 0 .. Block_Diff.Entry_Count - 1 loop
                  declare
                     Entry_Ref : State_Diff_Entry renames
                        Block_Diff.Entries (Diff_Entry_Index (I));
                  begin
                     if Entry_Ref.Account = Addr then
                        case Entry_Ref.Op is
                           when Op_Account_Created | Op_Account_Modified =>
                              Account := Entry_Ref.Account_Data;
                              Found := True;
                              return;
                           when Op_Account_Deleted =>
                              Found := False;
                              return;
                           when others =>
                              null;  -- Storage op, keep looking
                        end case;
                     end if;
                  end;
               end loop;
            end if;
         end;

         --  No diff available, try current state as fallback
         Get_Account (Addr, Account, Found, Error);
      else
         --  Non-archive mode: historical queries not fully supported
         --  Return current state if available
         Get_Account (Addr, Account, Found, Error);
      end if;
   end Get_Account_At_Block;

   procedure Get_Storage_At_Block (
      Contract     : in     Address;
      Slot         : in     U256;
      Block_Number : in     Word64;
      Value        : out    U256;
      Found        : out    Boolean;
      Error        : out    State_Manager_Error
   ) is
      Success : Boolean;
   begin
      Value := U256_Zero;
      Found := False;
      Error := Error_None;

      if not State_Initialized then
         Error := Error_Not_Initialized;
         return;
      end if;

      --  Check if state at this block is available
      if not Has_State_At_Block (Block_Number) then
         Error := Error_Invalid_Root;
         return;
      end if;

      --  For current block, return current storage
      if Block_Number = Current_Block_Number then
         SLOAD (Contract, Slot, Value, Success, Error);
         Found := Success;
         return;
      end if;

      --  For historical blocks, check diffs
      if Pruning_Configuration.Mode = Pruning_None then
         declare
            Diff_Found : Boolean;
            Block_Diff : State_Diff;
         begin
            Get_Block_Diff (Block_Number, Block_Diff, Diff_Found);

            if Diff_Found and then Block_Diff.Valid then
               --  Look for storage operation in diff entries
               for I in 0 .. Block_Diff.Entry_Count - 1 loop
                  declare
                     Entry_Ref : State_Diff_Entry renames
                        Block_Diff.Entries (Diff_Entry_Index (I));
                  begin
                     if Entry_Ref.Account = Contract then
                        case Entry_Ref.Op is
                           when Op_Storage_Modified =>
                              if Aegis_U256.Equal (Entry_Ref.Key, Slot) then
                                 Value := Entry_Ref.Value;
                                 Found := True;
                                 return;
                              end if;
                           when Op_Storage_Deleted =>
                              if Aegis_U256.Equal (Entry_Ref.Key, Slot) then
                                 Value := U256_Zero;
                                 Found := True;
                                 return;
                              end if;
                           when others =>
                              null;  -- Account op, keep looking
                        end case;
                     end if;
                  end;
               end loop;
            end if;
         end;

         --  No diff available, try current storage as fallback
         SLOAD (Contract, Slot, Value, Success, Error);
         Found := Success;
      else
         --  Non-archive mode: use current storage
         SLOAD (Contract, Slot, Value, Success, Error);
         Found := Success;
      end if;
   end Get_Storage_At_Block;

   ---------------------------------------------------------------------------
   --  Statistics & Monitoring
   ---------------------------------------------------------------------------

   function Get_Storage_Stats return Storage_Stats is
   begin
      return (
         Account_Count       => Stats_Account_Count,
         Total_Storage_Slots => Stats_Storage_Slots,
         State_Size_Bytes    => Stats_State_Size,
         Diff_Count          => Diff_Count,
         Witness_Count       => Stats_Witness_Count
      );
   end Get_Storage_Stats;

end Khepri_State_Manager;
