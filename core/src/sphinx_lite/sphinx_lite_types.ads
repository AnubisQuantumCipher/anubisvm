--  SPHINX Lite Types: Ultra-Light Client Types for IoT
--
--  Defines core types for the SPHINX Lite checkpoint protocol
--  enabling IoT devices with minimal resources to verify blockchain state.
--
--  Key Features:
--  - 21-member checkpoint committee
--  - 14-of-21 threshold signatures
--  - ~100 bytes trusted state
--  - ~2KB bandwidth per update
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8: SPHINX Lite
--  - SCARAB v2.0 Immortal Edition

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;

package Sphinx_Lite_Types with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Committee Constants
   ---------------------------------------------------------------------------

   --  Committee size (21 members for BFT safety)
   Committee_Size : constant := 21;

   --  Threshold for checkpoint validity (14-of-21 = 2/3 + 1)
   Threshold : constant := 14;

   --  Maximum proof depth for Merkle proofs
   Max_Proof_Depth : constant := 32;

   --  Maximum checkpoint age (in blocks) before refresh required
   Max_Checkpoint_Age : constant := 1000;

   ---------------------------------------------------------------------------
   --  Index Types
   ---------------------------------------------------------------------------

   subtype Committee_Index is Natural range 0 .. Committee_Size - 1;
   subtype Proof_Depth_Index is Natural range 0 .. Max_Proof_Depth - 1;
   subtype Signature_Count is Natural range 0 .. Committee_Size;

   ---------------------------------------------------------------------------
   --  Committee Member
   ---------------------------------------------------------------------------

   type Committee_Member is record
      Address    : Contract_Address;     --  Member"s address (20 bytes)
      Public_Key : Hash512;              --  ML-DSA-87 public key hash
      Weight     : Natural;              --  Voting weight (usually 1)
      Active     : Boolean;              --  Whether member is active
   end record;

   Null_Member : constant Committee_Member := (
      Address    => (others => 0),
      Public_Key => (others => 0),
      Weight     => 0,
      Active     => False
   );

   type Committee_Array is array (Committee_Index) of Committee_Member;

   ---------------------------------------------------------------------------
   --  Checkpoint Types
   ---------------------------------------------------------------------------

   --  Checkpoint represents a verified blockchain state snapshot
   type Checkpoint is record
      --  Block identification
      Block_Number   : Unsigned_64;       --  Block height
      Block_Hash     : Hash256;           --  Block hash
      State_Root     : Hash256;           --  State trie root

      --  Timestamp
      Timestamp      : Unsigned_64;       --  Unix timestamp

      --  Signature aggregation
      Sig_Bitmap     : Unsigned_32;       --  Bitmask of signers (21 bits used)
      Sig_Count      : Signature_Count;   --  Number of valid signatures

      --  Validity
      Valid          : Boolean;           --  Whether checkpoint is valid
   end record;

   Null_Checkpoint : constant Checkpoint := (
      Block_Number   => 0,
      Block_Hash     => (others => 0),
      State_Root     => (others => 0),
      Timestamp      => 0,
      Sig_Bitmap     => 0,
      Sig_Count      => 0,
      Valid          => False
   );

   ---------------------------------------------------------------------------
   --  Checkpoint Signature
   ---------------------------------------------------------------------------

   type Checkpoint_Signature is record
      Member_Index   : Committee_Index;   --  Index in committee
      Signature      : Hash512;           --  ML-DSA-87 signature
      Valid          : Boolean;           --  Verification result
   end record;

   Null_Signature : constant Checkpoint_Signature := (
      Member_Index   => 0,
      Signature      => (others => 0),
      Valid          => False
   );

   type Signature_Array is array (Committee_Index) of Checkpoint_Signature;

   ---------------------------------------------------------------------------
   --  Merkle Proof Types
   ---------------------------------------------------------------------------

   --  Single proof node (sibling hash)
   type Proof_Node is record
      Hash     : Hash256;                 --  Sibling hash
      Is_Left  : Boolean;                 --  True if sibling is left child
   end record;

   Null_Proof_Node : constant Proof_Node := (
      Hash     => (others => 0),
      Is_Left  => False
   );

   type Proof_Path is array (Proof_Depth_Index) of Proof_Node;

   --  Complete Merkle proof
   type Merkle_Proof is record
      Path       : Proof_Path;            --  Proof path (siblings)
      Depth      : Natural;               --  Actual depth (0..32)
      Leaf_Hash  : Hash256;               --  Hash of leaf data
      Root       : Hash256;               --  Expected root hash
      Valid      : Boolean;               --  Whether proof is valid
   end record;

   Null_Merkle_Proof : constant Merkle_Proof := (
      Path       => (others => Null_Proof_Node),
      Depth      => 0,
      Leaf_Hash  => (others => 0),
      Root       => (others => 0),
      Valid      => False
   );

   ---------------------------------------------------------------------------
   --  Account State Types (for IoT verification)
   ---------------------------------------------------------------------------

   --  Minimal account state
   type Account_Summary is record
      Address       : Contract_Address;   --  Account address
      Balance       : U256;               --  Account balance
      Nonce         : Unsigned_64;        --  Transaction nonce
      Storage_Root  : Hash256;            --  Storage trie root (if contract)
      Code_Hash     : Hash256;            --  Code hash (if contract)
   end record;

   Null_Account_Summary : constant Account_Summary := (
      Address       => (others => 0),
      Balance       => U256_Zero,
      Nonce         => 0,
      Storage_Root  => (others => 0),
      Code_Hash     => (others => 0)
   );

   --  Account proof (account state + Merkle proof)
   type Account_Proof is record
      Account       : Account_Summary;    --  Account data
      Proof         : Merkle_Proof;       --  Merkle inclusion proof
      Checkpoint_Ref : Unsigned_64;       --  Block number of checkpoint
      Valid         : Boolean;            --  Overall validity
   end record;

   Null_Account_Proof : constant Account_Proof := (
      Account       => Null_Account_Summary,
      Proof         => Null_Merkle_Proof,
      Checkpoint_Ref => 0,
      Valid         => False
   );

   ---------------------------------------------------------------------------
   --  IoT Client State (minimal trusted state ~100 bytes)
   ---------------------------------------------------------------------------

   type IoT_Client_State is record
      --  Latest verified checkpoint (64 bytes)
      Last_Checkpoint    : Checkpoint;

      --  Current committee root (32 bytes)
      Committee_Root     : Hash256;

      --  Client configuration
      Chain_ID           : Unsigned_32;
      Initialized        : Boolean;
   end record;

   Null_IoT_State : constant IoT_Client_State := (
      Last_Checkpoint    => Null_Checkpoint,
      Committee_Root     => (others => 0),
      Chain_ID           => 0,
      Initialized        => False
   );

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type Sphinx_Lite_Error is (
      Error_None,
      Error_Not_Initialized,
      Error_Invalid_Checkpoint,
      Error_Insufficient_Signatures,
      Error_Invalid_Proof,
      Error_Stale_Checkpoint,
      Error_Committee_Mismatch,
      Error_Committee_Changed,
      Error_Invalid_Member,
      Error_Duplicate_Signature,
      Error_Invalid_Block_Number,
      Error_Invalid_State_Root,
      Error_State_Root_Mismatch
   );

   ---------------------------------------------------------------------------
   --  Result Types
   ---------------------------------------------------------------------------

   type Verification_Result is record
      Success  : Boolean;
      Error    : Sphinx_Lite_Error;
   end record;

   Success_Result : constant Verification_Result := (
      Success  => True,
      Error    => Error_None
   );

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Count set bits in signature bitmap
   function Count_Signatures (Bitmap : Unsigned_32) return Natural with
      Global => null,
      Post   => Count_Signatures'Result <= Committee_Size;

   --  Check if member signed (bitmap check)
   function Has_Signed (
      Bitmap : Unsigned_32;
      Index  : Committee_Index
   ) return Boolean with
      Global => null;

   --  Set signature bit
   function Set_Signature (
      Bitmap : Unsigned_32;
      Index  : Committee_Index
   ) return Unsigned_32 with
      Global => null;

   --  Check if threshold reached
   function Threshold_Reached (Sig_Count : Signature_Count) return Boolean with
      Global => null,
      Post   => Threshold_Reached'Result = (Sig_Count >= Threshold);

end Sphinx_Lite_Types;
