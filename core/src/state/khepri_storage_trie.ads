--  KHEPRI Storage Trie
--
--  Per-contract storage using Merkle Patricia Trie.
--  Each contract has its own storage trie keyed by 256-bit slots.
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_U256;         use Aegis_U256;
with Khepri_Types;       use Khepri_Types;
with Khepri_MPT_Types;   use Khepri_MPT_Types;
with Khepri_MPT;         use Khepri_MPT;

package Khepri_Storage_Trie with
   SPARK_Mode => On,
   Abstract_State => (Storage_Tries with External => Async_Writers),
   Initializes => Storage_Tries
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_Contracts : constant := 10_000;

   ---------------------------------------------------------------------------
   --  Storage Handle
   ---------------------------------------------------------------------------

   type Storage_ID is new Natural range 0 .. Max_Contracts;
   Null_Storage : constant Storage_ID := 0;

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type Storage_Error is (
      Error_None,
      Error_Not_Found,
      Error_Full,
      Error_Invalid,
      Error_Trie_Error
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Create new empty storage for a contract
   procedure Create_Storage (
      Contract : in     Address;
      Storage  : out    Storage_ID;
      Success  : out    Boolean
   ) with
      Global => (In_Out => (Storage_Tries, Khepri_MPT.Trie_State));

   --  Load storage from existing root
   procedure Load_Storage (
      Contract     : in     Address;
      Storage_Root : in     Hash_256;
      Storage      : out    Storage_ID;
      Success      : out    Boolean
   ) with
      Global => (In_Out => (Storage_Tries, Khepri_MPT.Trie_State));

   --  Destroy storage
   procedure Destroy_Storage (
      Storage : in Storage_ID
   ) with
      Global => (In_Out => (Storage_Tries, Khepri_MPT.Trie_State));

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   --  Read storage slot
   procedure SLoad (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Value   : out    U256;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) with
      Global => (Input => (Storage_Tries, Khepri_MPT.Trie_State));

   --  Write storage slot
   procedure SStore (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Value   : in     U256;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) with
      Global => (Input  => Storage_Tries,
                 In_Out => Khepri_MPT.Trie_State);

   --  Delete storage slot
   procedure SDelete (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) with
      Global => (Input  => Storage_Tries,
                 In_Out => Khepri_MPT.Trie_State);

   --  Check if slot exists
   function Slot_Exists (
      Storage : Storage_ID;
      Slot    : U256
   ) return Boolean with
      Global => (Input => (Storage_Tries, Khepri_MPT.Trie_State)),
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Storage Root
   ---------------------------------------------------------------------------

   --  Get storage root hash
   function Get_Root (Storage : Storage_ID) return Hash_256 with
      Global => (Input => (Storage_Tries, Khepri_MPT.Trie_State)),
      Volatile_Function;

   --  Check if storage is empty
   function Is_Empty (Storage : Storage_ID) return Boolean with
      Global => (Input => (Storage_Tries, Khepri_MPT.Trie_State)),
      Volatile_Function;

   --  Get slot count
   function Slot_Count (Storage : Storage_ID) return Natural with
      Global => (Input => (Storage_Tries, Khepri_MPT.Trie_State)),
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Batch Operations
   ---------------------------------------------------------------------------

   --  Clear all storage (selfdestruct)
   procedure Clear_Storage (
      Storage : in     Storage_ID;
      Success : out    Boolean;
      Error   : out    Storage_Error
   ) with
      Global => (In_Out => (Storage_Tries, Khepri_MPT.Trie_State));

   ---------------------------------------------------------------------------
   --  Snapshots
   ---------------------------------------------------------------------------

   --  Create snapshot
   procedure Create_Snapshot (
      Storage     : in     Storage_ID;
      Snapshot_ID : out    Natural;
      Success     : out    Boolean
   ) with
      Global => (Input  => Storage_Tries,
                 In_Out => Khepri_MPT.Trie_State);

   --  Revert to snapshot
   procedure Revert_Snapshot (
      Storage     : in     Storage_ID;
      Snapshot_ID : in     Natural;
      Success     : out    Boolean
   ) with
      Global => (Input  => Storage_Tries,
                 In_Out => Khepri_MPT.Trie_State);

   --  Discard snapshot
   procedure Discard_Snapshot (
      Storage     : in     Storage_ID;
      Snapshot_ID : in     Natural
   ) with
      Global => (Input  => Storage_Tries,
                 In_Out => Khepri_MPT.Trie_State);

   ---------------------------------------------------------------------------
   --  Proofs
   ---------------------------------------------------------------------------

   --  Generate storage proof
   procedure Generate_Storage_Proof (
      Storage : in     Storage_ID;
      Slot    : in     U256;
      Proof   : out    Merkle_Proof;
      Success : out    Boolean
   ) with
      Global => (Input => (Storage_Tries, Khepri_MPT.Trie_State));

   --  Verify storage proof
   procedure Verify_Storage_Proof (
      Root    : in     Hash_256;
      Slot    : in     U256;
      Proof   : in     Merkle_Proof;
      Value   : out    U256;
      Valid   : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Lookup
   ---------------------------------------------------------------------------

   --  Find storage by contract address
   function Find_Storage (Contract : Address) return Storage_ID with
      Global => Storage_Tries,
      Volatile_Function;

end Khepri_Storage_Trie;
