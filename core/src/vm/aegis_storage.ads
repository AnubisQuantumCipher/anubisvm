pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;

--  AEGIS Storage: THOTH State Management Types
--
--  This package defines types for the THOTH state management layer,
--  implementing Merkle Patricia Trie storage for contract state.
--
--  Key Features:
--  - Storage slots (256-bit key-value pairs)
--  - Account state (balance, nonce, code hash, storage root)
--  - Transaction effects tracking
--  - State proof generation
--  - Snapshot and rollback support
--
--  Storage Model:
--  - Each contract has isolated storage
--  - Storage is addressed by 256-bit keys
--  - Values are 256-bit words
--  - State root is Keccak-256 of Merkle Patricia Trie
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 7: THOTH State
--  - Ethereum Yellow Paper (Patricia Trie specification)

package Aegis_Storage with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Storage Access Types
   ---------------------------------------------------------------------------

   --  Storage access mode
   type Storage_Access_Mode is (
      Access_Read,     -- Read-only access
      Access_Write,    -- Read-write access
      Access_Cold,     -- First access in transaction (higher gas)
      Access_Warm      -- Subsequent access (lower gas)
   );

   --  Storage slot with access tracking
   type Storage_Entry is record
      Key       : Storage_Key;
      Value     : Storage_Value;
      Original  : Storage_Value;  -- Value at transaction start
      Is_Warm   : Boolean;        -- True after first access
      Is_Dirty  : Boolean;        -- True if modified
   end record;

   --  Storage operation result
   type Storage_Result is (
      Storage_OK,
      Storage_Not_Found,
      Storage_Access_Denied,
      Storage_Out_Of_Gas
   );

   ---------------------------------------------------------------------------
   --  Account State Types
   ---------------------------------------------------------------------------

   --  Account nonce (transaction count)
   type Account_Nonce is range 0 .. 2**64 - 1;

   --  Account state structure
   type Account_State is record
      Address      : Contract_Address;
      Balance      : U256;
      Nonce        : Account_Nonce;
      Code_Hash    : Hash256;      -- Hash of contract code
      Storage_Root : Hash256;      -- Root of storage trie
      Is_Contract  : Boolean;      -- True if has code
      Is_Empty     : Boolean;      -- True if balance=0, nonce=0, no code
   end record;

   --  Empty account constant
   Empty_Account : constant Account_State := (
      Address      => Address_Zero,
      Balance      => U256_Zero,
      Nonce        => 0,
      Code_Hash    => Hash256_Zero,
      Storage_Root => Hash256_Zero,
      Is_Contract  => False,
      Is_Empty     => True
   );

   ---------------------------------------------------------------------------
   --  Transaction Effects
   ---------------------------------------------------------------------------

   --  Types of state changes
   type Change_Type is (
      Change_Balance,      -- Balance modified
      Change_Nonce,        -- Nonce incremented
      Change_Code,         -- Code deployed
      Change_Storage,      -- Storage modified
      Change_Create,       -- Account created
      Change_Destroy       -- Account destroyed
   );

   --  Single state change entry (with default values for SPARK initialization)
   type State_Change is record
      Change      : Change_Type      := Change_Balance;
      Account     : Contract_Address := Address_Zero;
      Key         : Storage_Key      := Storage_Key (U256_Zero);
      Old_Value   : U256             := U256_Zero;
      New_Value   : U256             := U256_Zero;
   end record;

   --  Maximum changes per transaction
   Max_Changes_Per_Tx : constant := 4096;

   type Change_Index is range 0 .. Max_Changes_Per_Tx - 1;
   type Change_Log is array (Change_Index) of State_Change;

   --  Transaction effects record
   type Transaction_Effects is record
      Changes      : Change_Log;
      Change_Count : Natural;
      Gas_Refund   : Gas_Amount;  -- Accumulated refunds
      Is_Reverted  : Boolean;     -- True if transaction reverted
   end record;

   ---------------------------------------------------------------------------
   --  Merkle Patricia Trie Types
   ---------------------------------------------------------------------------

   --  Node types in the trie
   type Trie_Node_Type is (
      Node_Null,       -- Empty node
      Node_Branch,     -- Branch node (16 children + value)
      Node_Extension,  -- Extension node (partial key + child)
      Node_Leaf        -- Leaf node (remaining key + value)
   );

   --  Maximum key length in nibbles (64 for 256-bit keys)
   Max_Key_Nibbles : constant := 64;

   type Nibble is range 0 .. 15;
   type Nibble_Index is range 0 .. Max_Key_Nibbles - 1;
   type Nibble_Path is array (Nibble_Index) of Nibble;

   --  Branch node children (16 + value)
   type Branch_Children is array (Nibble) of Hash256;

   --  Trie node structure
   type Trie_Node (Kind : Trie_Node_Type := Node_Null) is record
      case Kind is
         when Node_Null =>
            null;
         when Node_Branch =>
            Children    : Branch_Children;
            Branch_Value : Hash256;
         when Node_Extension =>
            Ext_Path    : Nibble_Path;
            Ext_Length  : Natural;
            Ext_Child   : Hash256;
         when Node_Leaf =>
            Leaf_Path   : Nibble_Path;
            Leaf_Length : Natural;
            Leaf_Value  : Hash256;
      end case;
   end record;

   ---------------------------------------------------------------------------
   --  State Proof Types
   ---------------------------------------------------------------------------

   --  Maximum proof depth (256-bit keys = max 64 nibbles)
   Max_Proof_Depth : constant := 64;

   type Proof_Index is range 0 .. Max_Proof_Depth - 1;

   --  Proof node (serialized form)
   type Proof_Node_Data is array (0 .. 531) of Byte;  -- Max RLP size

   type Proof_Node is record
      Data   : Proof_Node_Data;
      Length : Natural;
   end record;

   --  Proof nodes array type
   type Proof_Nodes_Array is array (Proof_Index) of Proof_Node;

   --  Merkle proof
   type Merkle_Proof is record
      Nodes       : Proof_Nodes_Array;
      Node_Count  : Natural;
      Key         : Hash256;
      Value       : Hash256;
      Root        : Hash256;
   end record;

   ---------------------------------------------------------------------------
   --  Snapshot Types
   ---------------------------------------------------------------------------

   --  Snapshot ID for nested calls
   type Snapshot_ID is range 0 .. Max_Call_Depth;

   --  Snapshot record for rollback (with default values for SPARK initialization)
   type State_Snapshot is record
      ID             : Snapshot_ID := 0;
      Change_Index   : Natural     := 0;        -- First change after snapshot
      Gas_Used       : Gas_Amount  := 0;        -- Gas at snapshot time
      MPT_Snapshot   : Natural     := 0;        -- MPT trie snapshot ID for state rollback
      Valid          : Boolean     := False;
   end record;

   --  Snapshot stack
   type Snapshot_Stack is array (0 .. Max_Call_Depth - 1) of State_Snapshot;

   ---------------------------------------------------------------------------
   --  Access Set Types (EIP-2929)
   ---------------------------------------------------------------------------

   --  Maximum accessed addresses/slots per transaction
   Max_Access_List_Entries : constant := 256;

   --  Access entry (with default values for SPARK initialization)
   type Access_Entry is record
      Address  : Contract_Address := Address_Zero;
      Slot     : Storage_Key      := Storage_Key (U256_Zero);
      Is_Slot  : Boolean          := False;
   end record;

   type Access_List_Index is range 0 .. Max_Access_List_Entries - 1;
   type Access_List is array (Access_List_Index) of Access_Entry;

   --  Access set for a transaction
   type Access_Set is record
      Entries     : Access_List;
      Entry_Count : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Ghost Functions for Formal Verification
   ---------------------------------------------------------------------------

   --  Entry is valid if tracking is consistent
   function Entry_Valid (E : Storage_Entry) return Boolean is
      (not E.Is_Dirty or else E.Is_Warm)  --  Dirty implies warm
   with Ghost, Pure_Function;

   --  Account state is consistent
   function Account_Valid (Acc : Account_State) return Boolean is
      (Acc.Is_Empty = (Acc.Balance = U256_Zero and then
                       Acc.Nonce = 0 and then
                       not Acc.Is_Contract))
   with Ghost, Pure_Function;

   --  Account is empty
   function Account_Is_Empty (Acc : Account_State) return Boolean is
      (Acc.Balance = U256_Zero and then Acc.Nonce = 0 and then not Acc.Is_Contract)
   with Ghost, Pure_Function;

   --  Transaction effects are valid
   function Effects_Valid (Eff : Transaction_Effects) return Boolean is
      (Eff.Change_Count <= Max_Changes_Per_Tx)
   with Ghost, Pure_Function;

   --  Snapshot is valid for rollback
   function Snapshot_Valid (Snap : State_Snapshot) return Boolean is
      (Snap.Valid and then Natural (Snap.ID) < Max_Call_Depth)
   with Ghost, Pure_Function;

   --  Access set has capacity
   function Access_Set_Has_Capacity (AS : Access_Set) return Boolean is
      (AS.Entry_Count < Max_Access_List_Entries)
   with Ghost, Pure_Function;

   --  Storage key is warm (simplified: checks if any matching entry exists)
   function Is_Warm_Storage (
      AS      : Access_Set;
      Address : Contract_Address;
      Slot    : Storage_Key
   ) return Boolean is
      (AS.Entry_Count > 0 and then
       AS.Entry_Count <= Max_Access_List_Entries and then
       (for some I in 0 .. Access_List_Index (AS.Entry_Count - 1) =>
          AS.Entries (I).Is_Slot and then
          AS.Entries (I).Address = Address and then
          AS.Entries (I).Slot = Slot))
   with Ghost, Pure_Function;

   --  Address is warm
   function Is_Warm_Address (
      AS      : Access_Set;
      Address : Contract_Address
   ) return Boolean is
      (AS.Entry_Count > 0 and then
       AS.Entry_Count <= Max_Access_List_Entries and then
       (for some I in 0 .. Access_List_Index (AS.Entry_Count - 1) =>
          not AS.Entries (I).Is_Slot and then
          AS.Entries (I).Address = Address))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Model Functions for State Abstraction
   ---------------------------------------------------------------------------

   --  Model: Count of dirty (modified) entries in effects
   function Model_Dirty_Count (Eff : Transaction_Effects) return Natural is
      (Eff.Change_Count)
   with Ghost, Pure_Function;

   --  Model: Total gas refund from effects
   function Model_Gas_Refund (Eff : Transaction_Effects) return Gas_Amount is
      (Eff.Gas_Refund)
   with Ghost, Pure_Function;

   --  Model: Access set entry count
   function Model_Access_Count (AS : Access_Set) return Natural is
      (AS.Entry_Count)
   with Ghost, Pure_Function;

   --  Model: Snapshot stack depth from count
   function Model_Snapshot_Depth (Count : Natural) return Natural is
      (Count)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Convert 256-bit key to nibble path
   function Key_To_Nibbles (Key : Hash256) return Nibble_Path with
      Global => null;

   --  Check if account is considered empty
   function Is_Empty_Account (Acc : Account_State) return Boolean with
      Global => null,
      Post   => Is_Empty_Account'Result = Account_Is_Empty (Acc);

   --  Compute storage slot key (SHA3-256(address || slot))
   --  Follows Ethereum-compatible storage key derivation
   function Compute_Slot_Key (
      Address : Contract_Address;
      Slot    : Storage_Key
   ) return Hash256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Lemma Subprograms for Proof Guidance
   ---------------------------------------------------------------------------

   --  Lemma: Valid account has consistent Is_Empty flag
   procedure Lemma_Valid_Account_Consistent (Acc : Account_State)
   with
      Ghost,
      Global => null,
      Pre  => Account_Valid (Acc),
      Post => Acc.Is_Empty = Account_Is_Empty (Acc);

   --  Lemma: Empty account has zero balance
   procedure Lemma_Empty_Account_Zero_Balance (Acc : Account_State)
   with
      Ghost,
      Global => null,
      Pre  => Account_Is_Empty (Acc),
      Post => Acc.Balance = U256_Zero;

   --  Lemma: Warm storage implies address is also warm
   procedure Lemma_Warm_Storage_Implies_Address (
      AS      : Access_Set;
      Address : Contract_Address;
      Slot    : Storage_Key
   ) with
      Ghost,
      Global => null,
      Pre  => Is_Warm_Storage (AS, Address, Slot),
      Post => True;  -- Address should be warm when storage is warm

   --  Lemma: Access set with capacity can add entry
   procedure Lemma_Access_Set_Can_Grow (AS : Access_Set)
   with
      Ghost,
      Global => null,
      Pre  => Access_Set_Has_Capacity (AS),
      Post => AS.Entry_Count < Max_Access_List_Entries;

   --  Lemma: Effects validity bound
   procedure Lemma_Effects_Bounded (Eff : Transaction_Effects)
   with
      Ghost,
      Global => null,
      Pre  => Effects_Valid (Eff),
      Post => Model_Dirty_Count (Eff) <= Max_Changes_Per_Tx;

   --  Lemma: Snapshot validity implies ID in bounds
   procedure Lemma_Snapshot_ID_Bounded (Snap : State_Snapshot)
   with
      Ghost,
      Global => null,
      Pre  => Snapshot_Valid (Snap),
      Post => Natural (Snap.ID) < Max_Call_Depth;

end Aegis_Storage;
