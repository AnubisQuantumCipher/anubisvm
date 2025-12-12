pragma SPARK_Mode (On);

with Anubis_MLDSA_Types;
with Anubis_MLDSA_Config;
with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  Quantum_Transaction_Auth: ML-DSA-87 Transaction Authentication
--
--  NIST Level 5 Quantum-Resistant Transaction Signing
--
--  Every state-changing operation on AnubisVM MUST be authenticated with
--  an ML-DSA-87 signature. This ensures that:
--
--  1. ALL transactions are quantum-resistant by default
--  2. No classical signature schemes (ECDSA, Ed25519) are supported
--  3. The sender's identity is derived from their ML-DSA-87 public key
--  4. Contract invocations cannot be forged even by quantum computers
--
--  Transaction Format:
--  +------------------+------------------+------------------+
--  | Transaction Body | ML-DSA-87 Sig    | Public Key Hash  |
--  | (variable)       | (4627 bytes)     | (32 bytes)       |
--  +------------------+------------------+------------------+
--
--  Signature covers: SHA3-256(domain || nonce || to || entry || args || gas || value)
--  Domain separator: "aegis-v1-tx-mldsa87"
--
--  Security Level: NIST Level 5 (equivalent to AES-256)
--  - Public Key:  2592 bytes (ML-DSA-87)
--  - Signature:   4627 bytes (ML-DSA-87)
--  - Security:    ~256-bit post-quantum
--
--  References:
--  - NIST FIPS 204 (ML-DSA)
--  - AAS-001 v3.1 (Aegis Address Standard)

package Quantum_Transaction_Auth with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Domain separator for transaction signing
   --  This ensures signatures cannot be replayed across different contexts
   Domain_Separator : constant String := "aegis-v1-tx-mldsa87";

   --  ML-DSA-87 key and signature sizes from FIPS 204
   MLDSA87_Public_Key_Size : constant := Anubis_MLDSA_Config.Public_Key_Bytes;  -- 2592
   MLDSA87_Secret_Key_Size : constant := Anubis_MLDSA_Config.Secret_Key_Bytes;  -- 4896
   MLDSA87_Signature_Size  : constant := Anubis_MLDSA_Config.Signature_Bytes;   -- 4627

   --  Maximum transaction body size (raw, unsigned)
   Max_Tx_Body_Size : constant := 16 * 1024;  -- 16 KB

   ---------------------------------------------------------------------------
   --  Transaction Types
   ---------------------------------------------------------------------------

   --  Transaction type enumeration
   type Transaction_Type is (
      Tx_Deploy,      --  Contract deployment
      Tx_Invoke,      --  Contract invocation (state change)
      Tx_Transfer     --  Native token transfer
   );

   --  Transaction body (the data that gets signed)
   subtype Tx_Body_Index is Natural range 0 .. Max_Tx_Body_Size - 1;
   type Tx_Body_Buffer is array (Tx_Body_Index) of Byte;

   --  Signed transaction structure
   type Signed_Transaction is record
      --  Transaction metadata
      Tx_Type      : Transaction_Type;
      Nonce        : U256;
      Chain_ID     : U256;

      --  Transaction payload
      From         : Contract_Address;   --  Sender (derived from public key)
      To           : Contract_Address;   --  Recipient/contract
      Entry_Point  : Entry_Name;
      Entry_Len    : Natural;
      Args         : Args_Buffer;
      Args_Size    : Natural;
      Gas_Limit    : Gas_Amount;
      Value        : U256;

      --  Signature (ML-DSA-87 ONLY)
      Signature    : Anubis_MLDSA_Types.Signature;
      Sig_Valid    : Boolean;

      --  Public key hash (for sender address recovery)
      --  The full 2592-byte public key is not stored in the transaction;
      --  it must be looked up from the account registry
      PubKey_Hash  : Hash256;
   end record;

   ---------------------------------------------------------------------------
   --  Transaction Verification
   ---------------------------------------------------------------------------

   --  Verification result
   type Verify_Result is record
      Valid        : Boolean;
      Error_Code   : Natural;
      Error_Msg    : Error_Message;
      Error_Len    : Natural;
      Gas_Cost     : Gas_Amount;
   end record;

   --  Error codes
   Error_OK                 : constant := 0;
   Error_Invalid_Signature  : constant := 1;
   Error_Invalid_Nonce      : constant := 2;
   Error_Invalid_Chain_ID   : constant := 3;
   Error_Insufficient_Gas   : constant := 4;
   Error_Invalid_Sender     : constant := 5;
   Error_Expired            : constant := 6;
   Error_Replay_Attack      : constant := 7;

   --  Gas cost for signature verification (ML-DSA-87 is computationally intensive)
   Sig_Verify_Gas_Cost : constant Gas_Amount := 50_000;

   --  Verify a signed transaction
   --
   --  This function:
   --  1. Reconstructs the transaction body from the signed transaction
   --  2. Hashes it with SHA3-256 using the domain separator
   --  3. Verifies the ML-DSA-87 signature against the public key
   --  4. Verifies that the sender address matches the public key
   --  5. Checks replay protection (nonce, chain ID)
   --
   --  Parameters:
   --  - Tx: The signed transaction to verify
   --  - Public_Key: The sender's ML-DSA-87 public key (2592 bytes)
   --  - Expected_Nonce: The expected nonce for replay protection
   --  - Current_Chain_ID: The current chain ID
   --
   --  Returns: Verification result with success/failure and gas used
   procedure Verify_Transaction (
      Tx              : in     Signed_Transaction;
      Public_Key      : in     Anubis_MLDSA_Types.Public_Key;
      Expected_Nonce  : in     U256;
      Current_Chain_ID : in    U256;
      Result          : out    Verify_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Transaction Signing (for client-side use)
   ---------------------------------------------------------------------------

   --  Sign a transaction with ML-DSA-87
   --
   --  Parameters:
   --  - Tx_Type: Type of transaction
   --  - To: Recipient/contract address
   --  - Entry_Point: Entry point name (for invoke)
   --  - Entry_Len: Length of entry point name
   --  - Args: Call arguments
   --  - Args_Size: Size of arguments
   --  - Gas_Limit: Maximum gas for execution
   --  - Value: Native token value
   --  - Nonce: Sender's nonce (for replay protection)
   --  - Chain_ID: Chain ID (for cross-chain replay protection)
   --  - Secret_Key: Sender's ML-DSA-87 secret key (4896 bytes)
   --  - Signed_Tx: Output signed transaction
   --  - Success: True if signing succeeded
   procedure Sign_Transaction (
      Tx_Type      : in     Transaction_Type;
      To           : in     Contract_Address;
      Entry_Point  : in     Entry_Name;
      Entry_Len    : in     Natural;
      Args         : in     Args_Buffer;
      Args_Size    : in     Natural;
      Gas_Limit    : in     Gas_Amount;
      Value        : in     U256;
      Nonce        : in     U256;
      Chain_ID     : in     U256;
      Secret_Key   : in     Anubis_MLDSA_Types.Secret_Key;
      Public_Key   : in     Anubis_MLDSA_Types.Public_Key;
      Signed_Tx    : out    Signed_Transaction;
      Success      : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Derive sender address from ML-DSA-87 public key
   --  Address = SHA3-256(domain_separator || public_key)[0..31]
   procedure Derive_Sender_Address (
      Public_Key : in     Anubis_MLDSA_Types.Public_Key;
      Sender     : out    Contract_Address
   ) with
      Global => null;

   --  Compute transaction hash for signing
   --  Hash = SHA3-256(domain || nonce || chain_id || to || entry || args || gas || value)
   procedure Compute_Tx_Hash (
      Tx   : in     Signed_Transaction;
      Hash : out    Hash256
   ) with
      Global => null;

   --  Check if a public key hash matches a public key
   function Verify_PubKey_Hash (
      Public_Key  : Anubis_MLDSA_Types.Public_Key;
      Hash        : Hash256
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Execution Policy
   ---------------------------------------------------------------------------

   --  Transaction execution policy
   --  This determines when signature verification is required
   type Execution_Policy is (
      Policy_Strict,      --  ALL transactions require signatures (production)
      Policy_Development, --  Signatures optional for read-only calls
      Policy_Testing      --  No signatures required (testing only)
   );

   --  Current policy (should be Policy_Strict in production)
   Current_Policy : constant Execution_Policy := Policy_Strict;

   --  Check if signature is required for a given transaction type
   function Requires_Signature (
      Tx_Type : Transaction_Type;
      Policy  : Execution_Policy
   ) return Boolean is
      (case Policy is
         when Policy_Strict      => True,  -- Always required
         when Policy_Development =>
            (case Tx_Type is
               when Tx_Deploy   => True,   -- Deploy always requires sig
               when Tx_Invoke   => True,   -- Invoke always requires sig
               when Tx_Transfer => True),  -- Transfer always requires sig
         when Policy_Testing     => False) -- Never required in testing
   with
      Global => null;

end Quantum_Transaction_Auth;
