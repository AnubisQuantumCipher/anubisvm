pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

--  TEE_Keys: Post-Quantum Key Management for Trusted Execution Environment
--
--  This package manages the TEE's cryptographic keys:
--  - Master Seal Key (MSK): Protects all persistent data
--  - Attestation Keys: ML-DSA-87 for remote attestation
--  - Session Keys: Derived from MSK for specific purposes
--
--  Key Hierarchy:
--  1. MSK (256-bit) - Stored in secure enclave / hardware
--  2. Derived Keys - Via KMAC256-based KDF
--     - Storage Key: Encrypts all persistent state
--     - Attestation Signing Key: Signs attestation reports
--     - Session Keys: Per-CVM and per-connection
--
--  All operations are designed for constant-time execution to
--  prevent timing side-channel attacks.

package TEE_Keys with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Key Types
   ---------------------------------------------------------------------------

   --  Master Seal Key (256-bit entropy)
   Key_Size : constant := 32;
   subtype Master_Seal_Key is Byte_Array (0 .. Key_Size - 1);

   --  Storage encryption key
   subtype Storage_Key is Byte_Array (0 .. Key_Size - 1);

   --  Session key for CVM operations
   subtype Session_Key is Byte_Array (0 .. Key_Size - 1);

   --  ML-DSA-87 key sizes (NIST FIPS 204)
   DSA_PK_Size : constant := 2592;  -- Public key
   DSA_SK_Size : constant := 4032;  -- Secret key
   DSA_SIG_Size : constant := 4627; -- Signature

   subtype DSA_Public_Key is Byte_Array (0 .. DSA_PK_Size - 1);
   subtype DSA_Secret_Key is Byte_Array (0 .. DSA_SK_Size - 1);
   subtype DSA_Signature is Byte_Array (0 .. DSA_SIG_Size - 1);

   --  ML-KEM-1024 key sizes (NIST FIPS 203)
   KEM_EK_Size : constant := 1568;  -- Encapsulation key (public)
   KEM_DK_Size : constant := 3168;  -- Decapsulation key (secret)
   KEM_CT_Size : constant := 1568;  -- Ciphertext
   KEM_SS_Size : constant := 32;    -- Shared secret

   subtype KEM_Encaps_Key is Byte_Array (0 .. KEM_EK_Size - 1);
   subtype KEM_Decaps_Key is Byte_Array (0 .. KEM_DK_Size - 1);
   subtype KEM_Ciphertext is Byte_Array (0 .. KEM_CT_Size - 1);
   subtype KEM_Shared_Secret is Byte_Array (0 .. KEM_SS_Size - 1);

   ---------------------------------------------------------------------------
   --  Key Bundle
   ---------------------------------------------------------------------------

   --  TEE Key Bundle - all keys needed for operation
   type TEE_Key_Bundle is record
      --  Master seal key (root of key hierarchy)
      MSK : Master_Seal_Key;

      --  Storage encryption key (derived from MSK)
      Storage : Storage_Key;

      --  Attestation signing keys (ML-DSA-87)
      Attest_PK : DSA_Public_Key;
      Attest_SK : DSA_Secret_Key;

      --  Key exchange keys (ML-KEM-1024)
      KEM_EK : KEM_Encaps_Key;
      KEM_DK : KEM_Decaps_Key;

      --  Keys are initialized
      Initialized : Boolean;
   end record;

   --  Empty bundle
   Empty_Bundle : constant TEE_Key_Bundle := (
      MSK         => (others => 0),
      Storage     => (others => 0),
      Attest_PK   => (others => 0),
      Attest_SK   => (others => 0),
      KEM_EK      => (others => 0),
      KEM_DK      => (others => 0),
      Initialized => False
   );

   ---------------------------------------------------------------------------
   --  Key Generation
   ---------------------------------------------------------------------------

   --  Initialize TEE with fresh keys
   --
   --  Entropy  : 256 bits of true random entropy
   --  Bundle   : Output key bundle
   --  Success  : True if initialization succeeded
   --
   --  This should only be called once during initial TEE provisioning.
   procedure Initialize_Keys (
      Entropy : Byte_Array;
      Bundle  : out TEE_Key_Bundle;
      Success : out Boolean
   ) with
      Global => null,
      Pre => Entropy'Length = 64,  -- 512 bits of entropy
      Post => (if Success then Bundle.Initialized);

   --  Derive all keys from MSK
   --
   --  MSK    : Master seal key
   --  Bundle : Output key bundle with derived keys
   procedure Derive_All_Keys (
      MSK    : Master_Seal_Key;
      Bundle : out TEE_Key_Bundle
   ) with
      Global => null,
      Post => Bundle.Initialized;

   --  Generate new attestation keypair
   procedure Generate_Attestation_Keys (
      Seed     : Byte_Array;
      PK       : out DSA_Public_Key;
      SK       : out DSA_Secret_Key;
      Success  : out Boolean
   ) with
      Global => null,
      Pre => Seed'Length = 32;

   --  Generate new KEM keypair
   procedure Generate_KEM_Keys (
      Seed     : Byte_Array;
      EK       : out KEM_Encaps_Key;
      DK       : out KEM_Decaps_Key;
      Success  : out Boolean
   ) with
      Global => null,
      Pre => Seed'Length = 64;

   ---------------------------------------------------------------------------
   --  Key Derivation
   ---------------------------------------------------------------------------

   --  Derive storage key from MSK
   procedure Derive_Storage_Key (
      MSK : Master_Seal_Key;
      Key : out Storage_Key
   ) with
      Global => null;

   --  Derive session key for CVM
   --
   --  MSK    : Master seal key
   --  CVM_ID : CVM address (32 bytes)
   --  Key    : Output session key
   procedure Derive_CVM_Key (
      MSK    : Master_Seal_Key;
      CVM_ID : Byte_Array;
      Key    : out Session_Key
   ) with
      Global => null,
      Pre => CVM_ID'Length = 32;

   --  Derive session key for connection
   --
   --  MSK        : Master seal key
   --  Session_ID : Unique session identifier
   --  Key        : Output session key
   procedure Derive_Connection_Key (
      MSK        : Master_Seal_Key;
      Session_ID : Byte_Array;
      Key        : out Session_Key
   ) with
      Global => null,
      Pre => Session_ID'Length = 32;

   ---------------------------------------------------------------------------
   --  Key Operations
   ---------------------------------------------------------------------------

   --  Seal data (encrypt with storage key)
   --
   --  Key       : Storage encryption key
   --  Plaintext : Data to encrypt
   --  Nonce     : Unique nonce (24 bytes)
   --  Ciphertext: Output encrypted data
   --  Tag       : Authentication tag (32 bytes)
   --  Success   : True if sealing succeeded
   procedure Seal_Data (
      Key        : Storage_Key;
      Plaintext  : Byte_Array;
      Nonce      : Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Byte_Array;
      Success    : out Boolean
   ) with
      Global => null,
      Pre => Nonce'Length = 24 and then
             Ciphertext'Length = Plaintext'Length and then
             Tag'Length = 32 and then
             Plaintext'Length <= 1024 * 1024;  -- 1 MB max

   --  Unseal data (decrypt with storage key)
   --
   --  Key        : Storage encryption key
   --  Ciphertext : Encrypted data
   --  Nonce      : Nonce used for encryption
   --  Tag        : Expected authentication tag
   --  Plaintext  : Output decrypted data
   --  Success    : True if unsealing and verification succeeded
   procedure Unseal_Data (
      Key        : Storage_Key;
      Ciphertext : Byte_Array;
      Nonce      : Byte_Array;
      Tag        : Byte_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean
   ) with
      Global => null,
      Pre => Nonce'Length = 24 and then
             Plaintext'Length = Ciphertext'Length and then
             Tag'Length = 32 and then
             Ciphertext'Length <= 1024 * 1024;

   ---------------------------------------------------------------------------
   --  Key Exchange
   ---------------------------------------------------------------------------

   --  Establish shared secret with remote party
   --
   --  DK      : Our decapsulation key
   --  CT      : Ciphertext from remote
   --  SS      : Output shared secret
   procedure Decapsulate (
      DK : KEM_Decaps_Key;
      CT : KEM_Ciphertext;
      SS : out KEM_Shared_Secret
   ) with
      Global => null;

   --  Create ciphertext for remote party
   --
   --  EK      : Remote's encapsulation key
   --  Random  : Randomness for encapsulation
   --  CT      : Output ciphertext
   --  SS      : Output shared secret
   procedure Encapsulate (
      EK     : KEM_Encaps_Key;
      Random : Byte_Array;
      CT     : out KEM_Ciphertext;
      SS     : out KEM_Shared_Secret
   ) with
      Global => null,
      Pre => Random'Length = 32;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   --  Securely clear key bundle
   procedure Zeroize_Bundle (Bundle : in Out TEE_Key_Bundle) with
      Global => null,
      Post => not Bundle.Initialized and then
              (for all I in Bundle.MSK'Range => Bundle.MSK (I) = 0) and then
              (for all I in Bundle.Storage'Range => Bundle.Storage (I) = 0);

   --  Securely clear master key
   procedure Zeroize_MSK (Key : in Out Master_Seal_Key) with
      Global => null,
      Post => (for all I in Key'Range => Key (I) = 0);

   --  Securely clear session key
   procedure Zeroize_Session (Key : in Out Session_Key) with
      Global => null,
      Post => (for all I in Key'Range => Key (I) = 0);

   --  Securely clear DSA secret key
   procedure Zeroize_DSA_SK (Key : in Out DSA_Secret_Key) with
      Global => null,
      Post => (for all I in Key'Range => Key (I) = 0);

   --  Securely clear KEM decapsulation key
   procedure Zeroize_KEM_DK (Key : in Out KEM_Decaps_Key) with
      Global => null,
      Post => (for all I in Key'Range => Key (I) = 0);

end TEE_Keys;
