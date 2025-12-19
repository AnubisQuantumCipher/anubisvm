--  KHEPRI State Manager
--
--  Complete state management system integrating:
--  - Account state trie (Khepri_State_Trie)
--  - Per-contract storage tries (Khepri_Storage_Trie)
--  - State diffs for synchronization
--  - State pruning for efficient storage
--  - Witness generation for light clients
--  - Archive node support
--
--  Certification Target: GOLD
pragma SPARK_Mode (On);

with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_U256;         use Aegis_U256;
with Khepri_Types;       use Khepri_Types;
with Khepri_MPT_Types;   use Khepri_MPT_Types;
with Khepri_State_Trie;
with Khepri_Storage_Trie;

package Khepri_State_Manager with
   SPARK_Mode => On,
   Abstract_State => State_Manager_Data,
   Initializes => State_Manager_Data
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_State_Diffs : constant := 1024;  -- Keep diffs for recent blocks
   Max_Witnesses   : constant := 100;   -- Concurrent witness requests

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type State_Manager_Error is (
      Error_None,
      Error_Not_Initialized,
      Error_Account_Not_Found,
      Error_Storage_Not_Found,
      Error_Trie_Error,
      Error_Diff_Full,
      Error_Witness_Full,
      Error_Invalid_Root,
      Error_Pruning_Failed
   );

   ---------------------------------------------------------------------------
   --  State Diff Types (for state sync)
   ---------------------------------------------------------------------------

   --  Types of state operations
   type State_Operation is (
      Op_Account_Created,
      Op_Account_Modified,
      Op_Account_Deleted,
      Op_Storage_Modified,
      Op_Storage_Deleted
   );

   --  Single state change for syncing
   type State_Diff_Entry is record
      Op           : State_Operation;
      Account      : Address;
      Key          : U256;           -- For storage operations
      Value        : U256;           -- New value
      Old_Value    : U256;           -- Previous value
      Account_Data : Khepri_State_Trie.Account_State;  -- For account ops
   end record;

   --  Diff index
   type Diff_Entry_Index is range 0 .. Max_State_Diffs - 1;
   type State_Diff_Entries is array (Diff_Entry_Index) of State_Diff_Entry;

   --  Complete state diff (changes between two state roots)
   type State_Diff is record
      From_Root    : Hash_256;
      To_Root      : Hash_256;
      Block_Number : Word64;
      Entries      : State_Diff_Entries;
      Entry_Count  : Natural;
      Valid        : Boolean;
   end record;

   Empty_State_Diff : constant State_Diff := (
      From_Root    => Empty_Hash,
      To_Root      => Empty_Hash,
      Block_Number => 0,
      Entries      => (others => (
         Op           => Op_Account_Created,
         Account      => Address_Zero,
         Key          => U256_Zero,
         Value        => U256_Zero,
         Old_Value    => U256_Zero,
         Account_Data => Khepri_State_Trie.Empty_Account
      )),
      Entry_Count  => 0,
      Valid        => False
   );

   ---------------------------------------------------------------------------
   --  Witness Types (for light clients)
   ---------------------------------------------------------------------------

   --  Witness contains all proofs needed to verify a state transition
   type State_Witness is record
      State_Root   : Hash_256;
      Block_Number : Word64;
      Account_Proofs_Count : Natural;
      Storage_Proofs_Count : Natural;
      Valid        : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Pruning Configuration
   ---------------------------------------------------------------------------

   type Pruning_Mode is (
      Pruning_None,      -- Archive node (keep all states)
      Pruning_Fast,      -- Keep only recent states
      Pruning_Full       -- Keep only current state
   );

   type Pruning_Config is record
      Mode              : Pruning_Mode;
      Keep_Block_Count  : Natural;  -- Blocks to keep in Fast mode
      Min_Free_Space_MB : Natural;  -- Trigger pruning when space low
   end record;

   Default_Pruning : constant Pruning_Config := (
      Mode              => Pruning_Fast,
      Keep_Block_Count  => 128,
      Min_Free_Space_MB => 1024
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize state manager with empty state
   procedure Initialize (
      Success : out Boolean
   ) with
      Global => (Output => State_Manager_Data);

   --  Initialize from existing state root
   procedure Initialize_From_Root (
      Root    : in     Hash_256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (Output => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Account Operations
   ---------------------------------------------------------------------------

   --  Get account state
   procedure Get_Account (
      Addr    : in     Address;
      Account : out    Khepri_State_Trie.Account_State;
      Found   : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (Input => State_Manager_Data);

   --  Set account state
   procedure Set_Account (
      Addr    : in     Address;
      Account : in     Khepri_State_Trie.Account_State;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Delete account
   procedure Delete_Account (
      Addr    : in     Address;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Storage Operations (integrated with storage tries)
   ---------------------------------------------------------------------------

   --  Load storage slot
   procedure SLOAD (
      Contract : in     Address;
      Slot     : in     U256;
      Value    : out    U256;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Store storage slot
   procedure SSTORE (
      Contract : in     Address;
      Slot     : in     U256;
      Value    : in     U256;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Delete storage slot
   procedure SDELETE (
      Contract : in     Address;
      Slot     : in     U256;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Balance Operations
   ---------------------------------------------------------------------------

   --  Get balance
   function Get_Balance (Addr : Address) return U256 with
      Global => (Input => State_Manager_Data);

   --  Add balance
   procedure Add_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Subtract balance
   procedure Sub_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Transfer balance
   procedure Transfer (
      From    : in     Address;
      To      : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  State Root & Commit
   ---------------------------------------------------------------------------

   --  Get current state root
   function Get_State_Root return Hash_256 with
      Global => (Input => State_Manager_Data),
      Volatile_Function;

   --  Commit current state and return new root
   procedure Commit_State (
      New_Root : out Hash_256;
      Success  : out Boolean
   ) with
      Global => (In_Out => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Snapshot & Rollback
   ---------------------------------------------------------------------------

   --  Create snapshot for potential rollback
   procedure Create_Snapshot (
      Snapshot_ID : out Natural;
      Success     : out Boolean
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Rollback to snapshot
   procedure Rollback_Snapshot (
      Snapshot_ID : in     Natural;
      Success     : out    Boolean;
      Error       : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Commit snapshot (discard rollback capability)
   procedure Commit_Snapshot (
      Snapshot_ID : in Natural
   ) with
      Global => (In_Out => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  State Diff Computation (for state sync)
   ---------------------------------------------------------------------------

   --  Compute diff between two state roots
   --  This walks both tries and identifies changes
   procedure Compute_State_Diff (
      From_Root : in     Hash_256;
      To_Root   : in     Hash_256;
      Diff      : out    State_Diff;
      Success   : out    Boolean;
      Error     : out    State_Manager_Error
   ) with
      Global => (Input => State_Manager_Data);

   --  Apply state diff (for fast sync)
   procedure Apply_State_Diff (
      Diff    : in     State_Diff;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Store diff for recent block
   procedure Store_Block_Diff (
      Block_Number : in     Word64;
      Diff         : in     State_Diff;
      Success      : out    Boolean
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Retrieve diff for block
   procedure Get_Block_Diff (
      Block_Number : in     Word64;
      Diff         : out    State_Diff;
      Found        : out    Boolean
   ) with
      Global => (Input => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  State Pruning (for efficient storage)
   ---------------------------------------------------------------------------

   --  Configure pruning mode
   procedure Set_Pruning_Config (
      Config : in Pruning_Config
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Prune old states (blocks before threshold)
   procedure Prune_Old_States (
      Keep_After_Block : in     Word64;
      Pruned_Count     : out    Natural;
      Success          : out    Boolean;
      Error            : out    State_Manager_Error
   ) with
      Global => (In_Out => State_Manager_Data);

   --  Check if state is available (for archive queries)
   function Has_State_At_Block (
      Block_Number : Word64
   ) return Boolean with
      Global => (Input => State_Manager_Data),
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Witness Generation (for light clients)
   ---------------------------------------------------------------------------

   --  Generate witness for account state
   procedure Generate_Account_Witness (
      Addr    : in     Address;
      Witness : out    State_Witness;
      Success : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => (Input => State_Manager_Data);

   --  Generate witness for storage slot
   procedure Generate_Storage_Witness (
      Contract : in     Address;
      Slot     : in     U256;
      Witness  : out    State_Witness;
      Success  : out    Boolean;
      Error    : out    State_Manager_Error
   ) with
      Global => (Input => State_Manager_Data);

   --  Verify witness against state root
   procedure Verify_Witness (
      Witness : in     State_Witness;
      Root    : in     Hash_256;
      Valid   : out    Boolean;
      Error   : out    State_Manager_Error
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Archive Node Support
   ---------------------------------------------------------------------------

   --  Get account state at specific block
   procedure Get_Account_At_Block (
      Addr         : in     Address;
      Block_Number : in     Word64;
      Account      : out    Khepri_State_Trie.Account_State;
      Found        : out    Boolean;
      Error        : out    State_Manager_Error
   ) with
      Global => (Input => State_Manager_Data);

   --  Get storage at specific block
   procedure Get_Storage_At_Block (
      Contract     : in     Address;
      Slot         : in     U256;
      Block_Number : in     Word64;
      Value        : out    U256;
      Found        : out    Boolean;
      Error        : out    State_Manager_Error
   ) with
      Global => (Input => State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Statistics & Monitoring
   ---------------------------------------------------------------------------

   --  Get storage statistics
   type Storage_Stats is record
      Account_Count       : Natural;
      Total_Storage_Slots : Natural;
      State_Size_Bytes    : Natural;
      Diff_Count          : Natural;
      Witness_Count       : Natural;
   end record;

   function Get_Storage_Stats return Storage_Stats with
      Global => (Input => State_Manager_Data),
      Volatile_Function;

end Khepri_State_Manager;
