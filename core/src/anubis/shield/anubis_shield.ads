-------------------------------------------------------------------------------
--  ANUBIS SHIELD - Encrypted Private State
--  Part of the Anubis Privacy Layer for AegisVM
--
--  Provides ML-KEM encrypted state storage where private contract data
--  is encrypted at rest and can only be decrypted by authorized parties.
--
--  SPDX-License-Identifier: Apache-2.0
--  Author: Sicarii
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types;     use Anubis_Types;
with Anubis_Bytes;     use Anubis_Bytes;
with Anubis_ChaCha20_Poly1305;

package Anubis_Shield with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum private state entry size (4KB)
   Max_Entry_Size : constant := 4096;

   --  AEAD overhead (ChaCha20-Poly1305: 12-byte nonce + 16-byte tag)
   AEAD_Nonce_Size : constant := Anubis_ChaCha20_Poly1305.Nonce_Size;  --  12
   AEAD_Tag_Size   : constant := Anubis_ChaCha20_Poly1305.Tag_Size;    --  16
   AEAD_Overhead   : constant := AEAD_Nonce_Size + AEAD_Tag_Size;

   --  ML-KEM-1024 ciphertext size
   KEM_Ciphertext_Size : constant := 1568;

   --  Ajtai commitment size (64 bytes)
   Commitment_Size : constant := 64;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Private state entry (encrypted)
   subtype Entry_Ciphertext is Byte_Array (0 .. Max_Entry_Size + AEAD_Overhead - 1);
   subtype Entry_Nonce is Byte_Array (0 .. AEAD_Nonce_Size - 1);
   subtype Entry_Tag is Byte_Array (0 .. AEAD_Tag_Size - 1);
   subtype KEM_Ciphertext is Byte_Array (0 .. KEM_Ciphertext_Size - 1);
   subtype Commitment_Value is Byte_Array (0 .. Commitment_Size - 1);

   type Private_Entry is record
      --  Encrypted payload
      Ciphertext     : Entry_Ciphertext;
      CT_Length      : Natural;

      --  ML-KEM encapsulated key (for the state owner)
      Encapsulated   : KEM_Ciphertext;

      --  Nonce for AEAD (unique per entry)
      Nonce          : Entry_Nonce;

      --  Authentication tag
      Tag            : Entry_Tag;
   end record;

   --  Ajtai commitment for ZK proofs
   type Entry_Commitment is record
      Value : Commitment_Value;
   end record;

   --  User"s key bundle for private state
   subtype Viewing_Key is Byte_Array (0 .. 31);
   subtype Commitment_Seed is Byte_Array (0 .. 31);

   type User_Key_Bundle is record
      --  ML-KEM-1024 public key for receiving encrypted state
      KEM_Public     : Byte_Array (0 .. 1567);  -- ML-KEM-1024 public key

      --  ML-DSA-87 public key for authenticated disclosure
      DSA_Public     : Byte_Array (0 .. 2591);  -- ML-DSA-87 public key

      --  Viewing key (derived, can be shared for auditing)
      View_Key       : Viewing_Key;

      --  Commitment randomness seed
      Commit_Random  : Commitment_Seed;
   end record;

   ---------------------------------------------------------------------------
   --  Encryption Operations
   ---------------------------------------------------------------------------

   --  Encrypt private state for a user
   procedure Encrypt_State (
      Plaintext      : Byte_Array;
      User_KEM_PK    : Byte_Array;
      Randomness     : Byte_Array;
      Priv_Entry     : out Private_Entry;
      Commitment     : out Entry_Commitment;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Plaintext'Length > 0
             and then Plaintext'Length <= Max_Entry_Size
             and then Plaintext'First = 0
             and then Randomness'Length = 64
             and then Randomness'First = 0
             and then User_KEM_PK'Length = 1568,
      Post => (if Success then Priv_Entry.CT_Length <= Max_Entry_Size + AEAD_Overhead);

   --  Decrypt private state (user-side)
   procedure Decrypt_State (
      Priv_Entry     : Private_Entry;
      User_KEM_SK    : Byte_Array;
      Plaintext      : out Byte_Array;
      PT_Length      : out Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => User_KEM_SK'Length = 3168  -- ML-KEM-1024 secret key size
             and then Plaintext'Length >= Max_Entry_Size
             and then Plaintext'First = 0
             and then Priv_Entry.CT_Length <= Max_Entry_Size,
      Post => (if Success then PT_Length <= Max_Entry_Size);

   --  Verify commitment matches decrypted value
   function Verify_Commitment (
      Plaintext      : Byte_Array;
      Commitment     : Entry_Commitment;
      Opening        : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Opening'Length = 32
             and then Opening'First = 0
             and then Plaintext'Length <= Max_Entry_Size
             and then Plaintext'First = 0;

   --  Re-encrypt for a different user (for transfers)
   procedure Re_Encrypt (
      Priv_Entry     : Private_Entry;
      Old_KEM_SK     : Byte_Array;
      New_KEM_PK     : Byte_Array;
      New_Randomness : Byte_Array;
      New_Entry      : out Private_Entry;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Old_KEM_SK'Length = 3168
             and then New_KEM_PK'Length = 1568
             and then New_Randomness'Length = 64
             and then New_Randomness'First = 0
             and then Priv_Entry.CT_Length <= Max_Entry_Size;

   ---------------------------------------------------------------------------
   --  Key Derivation
   ---------------------------------------------------------------------------

   --  Derive viewing key from master seed
   procedure Derive_Viewing_Key (
      Master_Seed    : Byte_Array;
      View_Key       : out Viewing_Key
   ) with
      Global => null,
      Pre => Master_Seed'Length = 32;

   --  Derive commitment seed from master seed
   procedure Derive_Commit_Seed (
      Master_Seed    : Byte_Array;
      Seed           : out Commitment_Seed
   ) with
      Global => null,
      Pre => Master_Seed'Length = 32;

   ---------------------------------------------------------------------------
   --  Commitment Operations
   ---------------------------------------------------------------------------

   --  Create Ajtai commitment to plaintext
   procedure Create_Commitment (
      Plaintext      : Byte_Array;
      Randomness     : Byte_Array;
      Commitment     : out Entry_Commitment
   ) with
      Global => null,
      Always_Terminates => True,
      Pre => Randomness'Length = 32
             and then Randomness'First = 0
             and then Plaintext'Length <= Max_Entry_Size
             and then Plaintext'First = 0;

   --  Zero out sensitive data
   procedure Zeroize_Entry (Priv_Entry : in out Private_Entry) with
      Global => null,
      Post => Priv_Entry.CT_Length = 0;

   procedure Zeroize_Bundle (Bundle : in out User_Key_Bundle) with
      Global => null;

end Anubis_Shield;
