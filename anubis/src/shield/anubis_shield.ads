--  ANUBIS Shield: Encrypted State Storage
--
--  Provides encrypted private state where:
--  - Data is encrypted using ML-KEM-1024 key encapsulation
--  - AEAD (ChaCha20-Poly1305 style) protects confidentiality and integrity
--  - Ajtai commitments enable ZK proofs about encrypted data
--
--  Flow:
--  1. Contract encrypts value V for user U:
--     - Generate random session key via ML-KEM Encapsulation
--     - Encrypt V with session key using AEAD
--     - Create Ajtai commitment to V for ZK proofs
--  2. User decrypts:
--     - Use ML-KEM Decapsulation to recover session key
--     - Decrypt ciphertext with AEAD
--
--  References:
--  - FIPS 203 (ML-KEM-1024)
--  - KHEPRI Blueprint v1.0, Anubis Privacy Layer Spec

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_Ajtai; use Anubis_Ajtai;

package Anubis_Shield with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  ML-KEM Types (from existing AegisVM core)
   ---------------------------------------------------------------------------

   --  ML-KEM-1024 key sizes
   Public_Key_Size  : constant := 1568;
   Secret_Key_Size  : constant := 3168;
   Ciphertext_Size  : constant := 1568;
   Shared_Secret_Size : constant := 32;

   subtype ML_KEM_Public_Key is Byte_Array (0 .. Public_Key_Size - 1);
   subtype ML_KEM_Secret_Key is Byte_Array (0 .. Secret_Key_Size - 1);
   subtype ML_KEM_Ciphertext is Byte_Array (0 .. Ciphertext_Size - 1);
   subtype Shared_Secret is Byte_Array (0 .. Shared_Secret_Size - 1);

   ---------------------------------------------------------------------------
   --  Private Entry Types
   ---------------------------------------------------------------------------

   --  Encrypted private state entry
   type Private_Entry is record
      --  AEAD ciphertext (encrypted payload)
      Ciphertext  : Byte_Array (0 .. Max_Entry_Size + AEAD_Overhead - 1);
      CT_Length   : Natural;

      --  ML-KEM encapsulated key
      Encapsulated : ML_KEM_Ciphertext;

      --  AEAD nonce (unique per encryption)
      Nonce : Byte_Array (0 .. 23);  --  192-bit nonce for XChaCha20

      --  AEAD authentication tag
      Tag : Byte_Array (0 .. 15);    --  128-bit Poly1305 tag
   end record;

   --  Entry commitment (for ZK proofs)
   type Entry_Commitment is record
      --  Ajtai commitment to plaintext value
      Commitment : Ajtai_Commitment;

      --  Commitment opening (kept by owner)
      Opening : Commitment_Opening;
   end record;

   --  Empty entry constant
   Empty_Entry : constant Private_Entry := (
      Ciphertext   => (others => 0),
      CT_Length    => 0,
      Encapsulated => (others => 0),
      Nonce        => (others => 0),
      Tag          => (others => 0)
   );

   ---------------------------------------------------------------------------
   --  User Key Bundle
   ---------------------------------------------------------------------------

   --  Complete key bundle for a user's private state
   type User_Key_Bundle is record
      --  ML-KEM-1024 keypair for receiving encrypted state
      KEM_Public : ML_KEM_Public_Key;
      KEM_Secret : ML_KEM_Secret_Key;

      --  Viewing key (can be shared for auditing)
      Viewing_Key : Byte_Array (0 .. 31);

      --  Commitment randomness seed
      Commit_Seed : Byte_Array (0 .. 31);
   end record;

   ---------------------------------------------------------------------------
   --  Encryption Operations
   ---------------------------------------------------------------------------

   --  Encrypt private state for a user
   --
   --  Creates an encrypted entry that only the user can decrypt.
   --  Also creates a commitment for ZK proofs.
   procedure Encrypt_State (
      Plaintext   : Byte_Array;
      User_PK     : ML_KEM_Public_Key;
      Randomness  : Byte_Array;
      Commit_Key  : Commitment_Key;
      Entry       : out Private_Entry;
      Commitment  : out Entry_Commitment;
      Success     : out Boolean
   ) with
      Global => null,
      Pre    => Plaintext'Length <= Max_Entry_Size
                and Randomness'Length >= 96;  --  64 for KEM + 32 for commit

   --  Decrypt private state (user-side)
   procedure Decrypt_State (
      Entry     : Private_Entry;
      User_SK   : ML_KEM_Secret_Key;
      Plaintext : out Byte_Array;
      Length    : out Natural;
      Success   : out Boolean
   ) with
      Global => null,
      Pre    => Plaintext'Length >= Max_Entry_Size;

   --  Verify commitment matches decrypted value
   function Verify_Commitment (
      Plaintext  : Byte_Array;
      Commitment : Entry_Commitment;
      Commit_Key : Commitment_Key
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Re-encryption (for transfers)
   ---------------------------------------------------------------------------

   --  Re-encrypt entry for a different user
   --
   --  Used when transferring ownership of private state.
   --  Requires the old owner's secret key to decrypt first.
   procedure Re_Encrypt (
      Entry          : Private_Entry;
      Old_SK         : ML_KEM_Secret_Key;
      New_PK         : ML_KEM_Public_Key;
      New_Randomness : Byte_Array;
      New_Entry      : out Private_Entry;
      Success        : out Boolean
   ) with
      Global => null,
      Pre    => New_Randomness'Length >= 64;

   ---------------------------------------------------------------------------
   --  Viewing Key Operations
   ---------------------------------------------------------------------------

   --  Derive viewing key from master secret
   procedure Derive_Viewing_Key (
      Master_SK   : ML_KEM_Secret_Key;
      Scope       : Disclosure_Scope;
      Viewing_Key : out Byte_Array
   ) with
      Global => null,
      Pre    => Viewing_Key'Length = 32;

   --  View state using viewing key (read-only access)
   procedure View_State (
      Entry       : Private_Entry;
      Viewing_Key : Byte_Array;
      Plaintext   : out Byte_Array;
      Length      : out Natural;
      Success     : out Boolean
   ) with
      Global => null,
      Pre    => Viewing_Key'Length = 32
                and Plaintext'Length >= Max_Entry_Size;

   ---------------------------------------------------------------------------
   --  Key Generation
   ---------------------------------------------------------------------------

   --  Generate a new user key bundle
   procedure Generate_Key_Bundle (
      Seed   : Byte_Array;
      Bundle : out User_Key_Bundle
   ) with
      Global => null,
      Pre    => Seed'Length = 64;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize private entry for storage
   procedure Serialize_Entry (
      Entry  : Private_Entry;
      Output : out Byte_Array;
      Length : out Natural
   ) with
      Global => null,
      Pre    => Output'Length >= Max_Entry_Size + AEAD_Overhead +
                Ciphertext_Size + 24 + 16 + 8;

   --  Deserialize private entry
   procedure Deserialize_Entry (
      Data    : Byte_Array;
      Entry   : out Private_Entry;
      Success : out Boolean
   ) with
      Global => null;

private

   ---------------------------------------------------------------------------
   --  AEAD Operations (XChaCha20-Poly1305 style)
   ---------------------------------------------------------------------------

   --  Encrypt with AEAD
   procedure AEAD_Encrypt (
      Key        : Byte_Array;
      Nonce      : Byte_Array;
      Plaintext  : Byte_Array;
      AAD        : Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Byte_Array
   ) with
      Global => null,
      Pre    => Key'Length = 32 and Nonce'Length = 24
                and Tag'Length = 16;

   --  Decrypt with AEAD
   procedure AEAD_Decrypt (
      Key        : Byte_Array;
      Nonce      : Byte_Array;
      Ciphertext : Byte_Array;
      Tag        : Byte_Array;
      AAD        : Byte_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean
   ) with
      Global => null,
      Pre    => Key'Length = 32 and Nonce'Length = 24
                and Tag'Length = 16;

   ---------------------------------------------------------------------------
   --  KDF Operations
   ---------------------------------------------------------------------------

   --  Derive AEAD key from shared secret
   procedure Derive_AEAD_Key (
      Shared_Secret : Byte_Array;
      Context       : Byte_Array;
      Key           : out Byte_Array
   ) with
      Global => null,
      Pre    => Shared_Secret'Length = 32 and Key'Length = 32;

end Anubis_Shield;
