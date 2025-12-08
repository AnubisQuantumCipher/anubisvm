--  KHEPRI State Trie
--
--  High-level state management using Merkle Patricia Trie.
--  Stores account state: balance, nonce, code hash, storage root.
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_U256;         use Aegis_U256;
with Khepri_Types;       use Khepri_Types;
with Khepri_MPT_Types;   use Khepri_MPT_Types;
with Khepri_MPT;         use Khepri_MPT;

package Khepri_State_Trie with
   SPARK_Mode => On,
   Abstract_State => (State_Trie_Data with External => Async_Writers),
   Initializes => State_Trie_Data
is

   ---------------------------------------------------------------------------
   --  Account State
   ---------------------------------------------------------------------------

   type Account_State is record
      Nonce        : Word64;           --  Transaction count
      Balance      : U256;             --  Account balance
      Storage_Root : Hash_256;         --  Root of storage trie
      Code_Hash    : Hash_256;         --  Hash of contract code
   end record;

   Empty_Code_Hash : constant Hash_256;  --  Keccak256 of empty

   Empty_Account : constant Account_State;

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type State_Error is (
      Error_None,
      Error_Account_Not_Found,
      Error_Invalid_State,
      Error_Trie_Error,
      Error_Overflow
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize with empty state root
   procedure Initialize with
      Global => (Output => State_Trie_Data);

   --  Initialize from existing root
   procedure Initialize_From_Root (
      Root    : in     Hash_256;
      Success : out    Boolean
   ) with
      Global => (Output => State_Trie_Data);

   ---------------------------------------------------------------------------
   --  Account Operations
   ---------------------------------------------------------------------------

   --  Get account state
   procedure Get_Account (
      Addr    : in     Address;
      Account : out    Account_State;
      Found   : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => State_Trie_Data;

   --  Set account state
   procedure Set_Account (
      Addr    : in     Address;
      Account : in     Account_State;
      Success : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Delete account
   procedure Delete_Account (
      Addr    : in     Address;
      Success : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Check if account exists
   function Account_Exists (Addr : Address) return Boolean with
      Global => State_Trie_Data;

   ---------------------------------------------------------------------------
   --  Balance Operations
   ---------------------------------------------------------------------------

   --  Get balance
   function Get_Balance (Addr : Address) return U256 with
      Global => State_Trie_Data;

   --  Add to balance (mint)
   procedure Add_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Subtract from balance (burn/transfer)
   procedure Sub_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Transfer balance
   procedure Transfer (
      From    : in     Address;
      To      : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   ---------------------------------------------------------------------------
   --  Nonce Operations
   ---------------------------------------------------------------------------

   --  Get nonce
   function Get_Nonce (Addr : Address) return Word64 with
      Global => State_Trie_Data;

   --  Increment nonce
   procedure Increment_Nonce (
      Addr    : in     Address;
      Success : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Set nonce
   procedure Set_Nonce (
      Addr    : in     Address;
      Nonce   : in     Word64;
      Success : out    Boolean;
      Error   : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   ---------------------------------------------------------------------------
   --  Code Operations
   ---------------------------------------------------------------------------

   --  Get code hash
   function Get_Code_Hash (Addr : Address) return Hash_256 with
      Global => State_Trie_Data;

   --  Set code hash (deploy contract)
   procedure Set_Code_Hash (
      Addr      : in     Address;
      Code_Hash : in     Hash_256;
      Success   : out    Boolean;
      Error     : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Check if address is contract
   function Is_Contract (Addr : Address) return Boolean with
      Global => State_Trie_Data;

   ---------------------------------------------------------------------------
   --  Storage Root Operations
   ---------------------------------------------------------------------------

   --  Get storage root
   function Get_Storage_Root (Addr : Address) return Hash_256 with
      Global => State_Trie_Data;

   --  Set storage root
   procedure Set_Storage_Root (
      Addr         : in     Address;
      Storage_Root : in     Hash_256;
      Success      : out    Boolean;
      Error        : out    State_Error
   ) with
      Global => (In_Out => State_Trie_Data);

   ---------------------------------------------------------------------------
   --  State Root
   ---------------------------------------------------------------------------

   --  Get current state root
   function State_Root return Hash_256 with
      Global => State_Trie_Data;

   --  Commit pending changes and return new root
   procedure Commit (
      New_Root : out Hash_256
   ) with
      Global => (In_Out => State_Trie_Data);

   ---------------------------------------------------------------------------
   --  Snapshots
   ---------------------------------------------------------------------------

   --  Create snapshot for transaction rollback
   procedure Create_Snapshot (
      Snapshot_ID : out Natural;
      Success     : out Boolean
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Revert to snapshot
   procedure Revert_To_Snapshot (
      Snapshot_ID : in     Natural;
      Success     : out    Boolean
   ) with
      Global => (In_Out => State_Trie_Data);

   --  Discard snapshot
   procedure Discard_Snapshot (
      Snapshot_ID : in Natural
   ) with
      Global => (In_Out => State_Trie_Data);

   ---------------------------------------------------------------------------
   --  Proofs
   ---------------------------------------------------------------------------

   --  Generate account proof
   procedure Generate_Account_Proof (
      Addr    : in     Address;
      Proof   : out    Merkle_Proof;
      Success : out    Boolean
   ) with
      Global => State_Trie_Data;

   --  Verify account proof against root
   procedure Verify_Account_Proof (
      Root    : in     Hash_256;
      Addr    : in     Address;
      Proof   : in     Merkle_Proof;
      Account : out    Account_State;
      Valid   : out    Boolean
   ) with
      Global => null;

private

   --  Empty code hash (keccak256 of empty byte array)
   Empty_Code_Hash : constant Hash_256 := (
      16#C5#, 16#D2#, 16#46#, 16#01#, 16#86#, 16#F7#, 16#23#, 16#3C#,
      16#92#, 16#7E#, 16#7D#, 16#B2#, 16#DC#, 16#C7#, 16#03#, 16#C0#,
      16#E5#, 16#00#, 16#B6#, 16#53#, 16#CA#, 16#82#, 16#27#, 16#3B#,
      16#7B#, 16#FA#, 16#D8#, 16#04#, 16#5D#, 16#85#, 16#A4#, 16#70#
   );

   --  Empty storage root (keccak256 of RLP null)
   Empty_Storage_Root : constant Hash_256 := (
      16#56#, 16#E8#, 16#1F#, 16#17#, 16#1B#, 16#CC#, 16#55#, 16#A6#,
      16#FF#, 16#83#, 16#45#, 16#E6#, 16#92#, 16#C0#, 16#F8#, 16#6E#,
      16#5B#, 16#48#, 16#E0#, 16#1B#, 16#99#, 16#6C#, 16#AD#, 16#C0#,
      16#01#, 16#62#, 16#2F#, 16#B5#, 16#E3#, 16#63#, 16#B4#, 16#21#
   );

   Empty_Account : constant Account_State := (
      Nonce        => 0,
      Balance      => U256_Zero,
      Storage_Root => Empty_Storage_Root,
      Code_Hash    => Empty_Code_Hash
   );

end Khepri_State_Trie;
