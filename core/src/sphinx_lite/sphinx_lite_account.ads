--  SPHINX Lite Account: Account State Proof Verification
--
--  Implements Merkle proof verification for account balances
--  with minimal bandwidth (~2KB per proof).
--
--  Key Features:
--  - Merkle Patricia Trie proof verification
--  - Account state extraction
--  - Balance verification
--  - ~2KB proof size for IoT devices
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.4: Account Proofs
--  - SCARAB v2.0 Immortal Edition

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;
with Sphinx_Lite_Types; use Sphinx_Lite_Types;

package Sphinx_Lite_Account with
   SPARK_Mode => On,
   Abstract_State => Account_State,
   Initializes => Account_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum account proof size for IoT bandwidth
   Max_Account_Proof_Size : constant := 2 * 1024;  -- 2 KB

   --  Account storage slots
   Max_Storage_Slots : constant := 16;

   --  RLP encoding limits
   Max_RLP_Item_Length : constant := 256;

   ---------------------------------------------------------------------------
   --  Account State Types
   ---------------------------------------------------------------------------

   type Account_State_Data is record
      Address      : Contract_Address;
      Balance      : U256;
      Nonce        : Unsigned_64;
      Code_Hash    : Hash256;
      Storage_Root : Hash256;
      Exists       : Boolean;
   end record;

   Null_Account_State : constant Account_State_Data := (
      Address      => (others => 0),
      Balance      => U256_Zero,
      Nonce        => 0,
      Code_Hash    => (others => 0),
      Storage_Root => (others => 0),
      Exists       => False
   );

   ---------------------------------------------------------------------------
   --  Storage Proof Types
   ---------------------------------------------------------------------------

   type Storage_Slot is record
      Key   : Hash256;
      Value : Hash256;
      Valid : Boolean;
   end record;

   Null_Storage_Slot : constant Storage_Slot := (
      Key   => (others => 0),
      Value => (others => 0),
      Valid => False
   );

   type Storage_Slots is array (0 .. Max_Storage_Slots - 1) of Storage_Slot;

   ---------------------------------------------------------------------------
   --  Account Proof Types
   ---------------------------------------------------------------------------

   --  Compact account proof for IoT transmission
   type Account_Proof is record
      Account       : Account_State_Data;
      State_Root    : Hash256;          -- Expected state root
      Block_Number  : Unsigned_64;      -- Block of proof
      Proof         : Merkle_Proof;     -- Merkle branch
      Storage_Proof : Merkle_Proof;     -- Storage Merkle branch (if needed)
      Valid         : Boolean;
   end record;

   Null_Account_Proof : constant Account_Proof := (
      Account       => Null_Account_State,
      State_Root    => (others => 0),
      Block_Number  => 0,
      Proof         => Null_Merkle_Proof,
      Storage_Proof => Null_Merkle_Proof,
      Valid         => False
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize account proof system with trusted state root
   procedure Initialize (
      Trusted_Root : in Hash256;
      Block_Number : in Unsigned_64
   ) with
      Global => (Output => Account_State);

   --  Check if initialized
   function Is_Initialized return Boolean with
      Global => Account_State;

   ---------------------------------------------------------------------------
   --  Account Proof Verification
   ---------------------------------------------------------------------------

   --  Verify account exists at given state root
   procedure Verify_Account_Existence (
      Address     : in  Contract_Address;
      State_Root  : in  Hash256;
      Proof       : in  Merkle_Proof;
      Exists      : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) with
      Global => null,
      Pre    => Proof.Depth <= Max_Proof_Depth;

   --  Verify account state against proof
   procedure Verify_Account_State (
      Account     : in  Account_State_Data;
      State_Root  : in  Hash256;
      Proof       : in  Merkle_Proof;
      Result      : out Verification_Result
   ) with
      Global => null,
      Pre    => Account.Exists and then Proof.Depth <= Max_Proof_Depth;

   --  Verify account balance
   procedure Verify_Balance (
      Address     : in  Contract_Address;
      Balance     : in  U256;
      State_Root  : in  Hash256;
      Proof       : in  Merkle_Proof;
      Valid       : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) with
      Global => null,
      Pre    => Proof.Depth <= Max_Proof_Depth;

   --  Complete account proof verification
   procedure Verify_Account_Proof (
      Proof   : in  Account_Proof;
      Result  : out Verification_Result
   ) with
      Global => (In_Out => Account_State);

   ---------------------------------------------------------------------------
   --  Storage Proof Verification
   ---------------------------------------------------------------------------

   --  Verify storage slot value
   procedure Verify_Storage_Slot (
      Account      : in  Account_State_Data;
      Slot_Key     : in  Hash256;
      Slot_Value   : in  Hash256;
      Storage_Proof: in  Merkle_Proof;
      Valid        : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) with
      Global => null,
      Pre    => Account.Exists and then Storage_Proof.Depth <= Max_Proof_Depth;

   --  Verify multiple storage slots
   procedure Verify_Storage_Slots (
      Account      : in  Account_State_Data;
      Slots        : in  Storage_Slots;
      Storage_Root : in  Hash256;
      Proof        : in  Merkle_Proof;
      Valid_Count  : out Natural;
      Error        : out Sphinx_Lite_Error
   ) with
      Global => null,
      Post   => Valid_Count <= Max_Storage_Slots;

   ---------------------------------------------------------------------------
   --  Account State Cache
   ---------------------------------------------------------------------------

   --  Update trusted state root (after checkpoint finality)
   procedure Update_Trusted_Root (
      New_Root     : in  Hash256;
      Block_Number : in  Unsigned_64;
      Success      : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) with
      Global => (In_Out => Account_State);

   --  Get current trusted state root
   function Get_Trusted_Root return Hash256 with
      Global => Account_State;

   --  Get block number of trusted root
   function Get_Trusted_Block return Unsigned_64 with
      Global => Account_State;

   ---------------------------------------------------------------------------
   --  Account Proof Construction (for full nodes serving light clients)
   ---------------------------------------------------------------------------

   --  Create account proof for address
   procedure Create_Account_Proof (
      Address      : in  Contract_Address;
      State_Root   : in  Hash256;
      Block_Number : in  Unsigned_64;
      Proof        : out Account_Proof;
      Success      : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Merkle Proof Utilities
   ---------------------------------------------------------------------------

   --  Compute account key (Keccak256 of address)
   function Compute_Account_Key (Address : Contract_Address) return Hash256 with
      Global => null;

   --  Compute storage key (Keccak256 of slot key)
   function Compute_Storage_Key (Slot_Key : Hash256) return Hash256 with
      Global => null;

   --  Verify Merkle branch
   procedure Verify_Merkle_Branch (
      Leaf       : in  Hash256;
      Root       : in  Hash256;
      Proof      : in  Merkle_Proof;
      Key        : in  Hash256;
      Valid      : out Boolean
   ) with
      Global => null,
      Pre    => Proof.Depth <= Max_Proof_Depth;

   ---------------------------------------------------------------------------
   --  Serialization (for network transmission)
   ---------------------------------------------------------------------------

   --  Serialized account proof size
   Account_Proof_Header_Size : constant := 32 + 32 + 8 + 8 + 32 + 1;  -- ~113 bytes
   Max_Serialized_Proof_Size : constant := Account_Proof_Header_Size + Max_Account_Proof_Size;

   type Serialized_Account_Proof is array (0 .. Max_Serialized_Proof_Size - 1) of Unsigned_8;

   --  Serialize account proof
   procedure Serialize_Account_Proof (
      Proof : in  Account_Proof;
      Bytes : out Serialized_Account_Proof;
      Size  : out Natural
   ) with
      Global => null;

   --  Deserialize account proof
   procedure Deserialize_Account_Proof (
      Bytes   : in  Serialized_Account_Proof;
      Size    : in  Natural;
      Proof   : out Account_Proof;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Size <= Max_Serialized_Proof_Size;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   --  Get total proofs verified
   function Total_Proofs_Verified return Natural with
      Global => Account_State;

   --  Get successful verifications
   function Successful_Verifications return Natural with
      Global => Account_State;

   --  Get average proof size
   function Average_Proof_Size return Natural with
      Global => Account_State,
      Post   => Average_Proof_Size'Result <= Max_Account_Proof_Size;

end Sphinx_Lite_Account;
