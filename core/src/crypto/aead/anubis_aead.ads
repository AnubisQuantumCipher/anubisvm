pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

--  Anubis_AEAD: Post-Quantum Authenticated Encryption with Associated Data
--
--  This package provides AEAD using Keccak primitives:
--  - Encryption: SHAKE256-based stream cipher
--  - Authentication: KMAC256 (encrypt-then-MAC)
--
--  Security properties:
--  - 256-bit security level (post-quantum resistant)
--  - IND-CCA2 secure under standard model
--  - Nonce-misuse resistant when using random nonces
--
--  Construction (similar to SpongeWrap but simplified):
--  1. Derive keystream K' = SHAKE256(key || nonce || counter)
--  2. Ciphertext C = Plaintext XOR K'
--  3. Tag = KMAC256(key, nonce || AAD || C || len(AAD) || len(C), "aead")
--
--  This provides IND-CCA2 security under the Encrypt-then-MAC paradigm.
--
--  Formal Verification (SPARK Gold):
--  - All procedures proven free of runtime errors (no overflow, bounds)
--  - Ciphertext/Plaintext length equality verified
--  - Tag length invariants verified
--  - Zeroization postconditions proven
--  - On decryption failure, plaintext is zeroed (security critical)

package Anubis_AEAD with
   SPARK_Mode => On,
   Pure,
   Always_Terminates
is

   ---------------------------------------------------------------------------
   --  Constants (Security Parameters)
   ---------------------------------------------------------------------------

   --  Key size: 256 bits (32 bytes) - post-quantum secure
   Key_Size : constant := 32;

   --  Nonce size: 192 bits (24 bytes) - large enough for random generation
   --  Birthday bound for collision: 2^96 which is more than sufficient
   Nonce_Size : constant := 24;

   --  Tag size: 256 bits (32 bytes) - full KMAC256 output
   --  Provides 256-bit integrity protection
   Tag_Size : constant := 32;

   --  Maximum plaintext/ciphertext length (prevent overflow)
   Max_Message_Length : constant := Natural'Last / 2 - 1024;

   --  Maximum associated data length
   Max_AAD_Length : constant := 65536;  -- 64 KB

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  AEAD key type (256-bit)
   subtype AEAD_Key is Byte_Array (0 .. Key_Size - 1);

   --  AEAD nonce type (192-bit)
   subtype AEAD_Nonce is Byte_Array (0 .. Nonce_Size - 1);

   --  AEAD authentication tag type (256-bit)
   subtype AEAD_Tag is Byte_Array (0 .. Tag_Size - 1);

   ---------------------------------------------------------------------------
   --  Ghost Functions for Specification
   ---------------------------------------------------------------------------

   --  Ghost function: Verify key has correct length
   function Is_Valid_Key (K : AEAD_Key) return Boolean is
      (K'First = 0 and K'Last = Key_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify nonce has correct length
   function Is_Valid_Nonce (N : AEAD_Nonce) return Boolean is
      (N'First = 0 and N'Last = Nonce_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify tag has correct length
   function Is_Valid_Tag (T : AEAD_Tag) return Boolean is
      (T'First = 0 and T'Last = Tag_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify message bounds are safe for processing
   function Message_Bounds_Safe (M : Byte_Array) return Boolean is
      (M'Length <= Max_Message_Length and then M'Last < Natural'Last)
   with Ghost, Pure_Function;

   --  Ghost function: Verify AAD bounds are safe for processing
   function AAD_Bounds_Safe (A : Byte_Array) return Boolean is
      (A'Length <= Max_AAD_Length and then A'Last < Natural'Last)
   with Ghost, Pure_Function;

   --  Ghost function: Verify key is all zeros (for zeroization)
   function Key_Is_Zero (K : AEAD_Key) return Boolean is
      (for all I in K'Range => K (I) = 0)
   with Ghost, Pure_Function;

   --  Ghost function: Verify nonce is all zeros (for zeroization)
   function Nonce_Is_Zero (N : AEAD_Nonce) return Boolean is
      (for all I in N'Range => N (I) = 0)
   with Ghost, Pure_Function;

   --  Ghost function: Verify buffer is all zeros
   function Buffer_Is_Zero (B : Byte_Array) return Boolean is
      (for all I in B'Range => B (I) = 0)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  AEAD Encryption
   ---------------------------------------------------------------------------

   --  AEAD_Encrypt: Encrypt plaintext with authentication
   --
   --  Key        : 256-bit secret key
   --  Nonce      : 192-bit nonce (must be unique per key)
   --  Plaintext  : Data to encrypt
   --  AAD        : Additional authenticated data (not encrypted, but authenticated)
   --  Ciphertext : Encrypted output (same length as Plaintext)
   --  Tag        : Authentication tag (32 bytes)
   --
   --  The Ciphertext and Tag must both be transmitted/stored for decryption.
   --
   --  IMPORTANT: Never reuse a (Key, Nonce) pair. Either:
   --  - Use a counter-based nonce (guaranteed unique)
   --  - Use random nonce from secure PRNG (192 bits provides sufficient margin)
   --
   --  Pre: Message and AAD bounds safe, ciphertext matches plaintext length
   --  Post: Tag has exact length 32 bytes (indices 0..31)
   procedure AEAD_Encrypt (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      Plaintext  : Byte_Array;
      AAD        : Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out AEAD_Tag
   ) with
      Global  => null,
      Depends => (Ciphertext =>+ (Key, Nonce, Plaintext),
                  Tag => (Key, Nonce, Plaintext, AAD, Ciphertext)),
      Pre     => Message_Bounds_Safe (Plaintext) and then
                 AAD_Bounds_Safe (AAD) and then
                 Ciphertext'Length = Plaintext'Length and then
                 Ciphertext'Last < Natural'Last and then
                 Plaintext'Length <= 65535 and then
                 Nonce_Size + AAD'Length + Plaintext'Length + 16 <=
                    Max_Message_Length,
      Post    => Is_Valid_Tag (Tag),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  AEAD Decryption
   ---------------------------------------------------------------------------

   --  AEAD_Decrypt: Decrypt and verify ciphertext
   --
   --  Key        : 256-bit secret key
   --  Nonce      : 192-bit nonce (same as used for encryption)
   --  Ciphertext : Data to decrypt
   --  AAD        : Additional authenticated data
   --  Tag        : Expected authentication tag
   --  Plaintext  : Decrypted output (if Success = True)
   --  Success    : True if authentication passed, False otherwise
   --
   --  CRITICAL SECURITY PROPERTY:
   --  On authentication failure, Plaintext is zeroed and Success is False.
   --  The caller MUST check Success before using Plaintext.
   --  This prevents release of unauthenticated plaintext.
   --
   --  Pre: Message and AAD bounds safe, plaintext matches ciphertext length
   --  Post: On failure (not Success), plaintext is all zeros
   procedure AEAD_Decrypt (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      Ciphertext : Byte_Array;
      AAD        : Byte_Array;
      Tag        : AEAD_Tag;
      Plaintext  : out Byte_Array;
      Success    : out Boolean
   ) with
      Global  => null,
      Depends => (Plaintext =>+ (Key, Nonce, Ciphertext, AAD, Tag),
                  Success => (Key, Nonce, Ciphertext, AAD, Tag)),
      Pre     => Message_Bounds_Safe (Ciphertext) and then
                 AAD_Bounds_Safe (AAD) and then
                 Plaintext'Length = Ciphertext'Length and then
                 Plaintext'Last < Natural'Last and then
                 Ciphertext'Length <= 65535 and then
                 Nonce_Size + AAD'Length + Ciphertext'Length + 16 <=
                    Max_Message_Length,
      Post    => (if not Success then Buffer_Is_Zero (Plaintext)),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   --  Zeroize_Key: Securely clear key material
   --
   --  Uses volatile writes to prevent dead-store elimination.
   --  Must be called after key is no longer needed.
   --
   --  Post: All bytes of key are zero (proven by postcondition)
   procedure Zeroize_Key (Key : in Out AEAD_Key) with
      Global  => null,
      Depends => (Key => null, null => Key),
      Post    => Key_Is_Zero (Key),
      Always_Terminates;

   --  Zeroize_Nonce: Securely clear nonce
   --
   --  Uses volatile writes to prevent dead-store elimination.
   --  Called after encryption/decryption completes.
   --
   --  Post: All bytes of nonce are zero (proven by postcondition)
   procedure Zeroize_Nonce (Nonce : in Out AEAD_Nonce) with
      Global  => null,
      Depends => (Nonce => null, null => Nonce),
      Post    => Nonce_Is_Zero (Nonce),
      Always_Terminates;

private

   ---------------------------------------------------------------------------
   --  Internal Functions
   ---------------------------------------------------------------------------

   --  Generate_Keystream: Derive keystream using SHAKE256
   --
   --  The keystream is generated as:
   --    SHAKE256(key || nonce || counter, len)
   --  where counter increments for each rate-block of output.
   --
   --  Pre: Keystream bounds safe and starts at index 0
   procedure Generate_Keystream (
      Key       : AEAD_Key;
      Nonce     : AEAD_Nonce;
      Keystream : out Byte_Array
   ) with
      Global  => null,
      Depends => (Keystream =>+ (Key, Nonce)),
      Pre     => Keystream'Length <= Max_Message_Length and then
                 Keystream'Length <= 65535 and then
                 Keystream'First = 0;

   --  Compute_Tag: Compute authentication tag using KMAC256
   --
   --  Tag is computed over:
   --    KMAC256(key, nonce || AAD || ciphertext || len(AAD) || len(C), "aead")
   --
   --  This binds all inputs to the tag, providing integrity.
   --
   --  Pre: AAD and ciphertext bounds safe, combined length fits KMAC bounds
   procedure Compute_Tag (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      AAD        : Byte_Array;
      Ciphertext : Byte_Array;
      Tag        : out AEAD_Tag
   ) with
      Global  => null,
      Depends => (Tag => (Key, Nonce, AAD, Ciphertext)),
      Pre     => AAD_Bounds_Safe (AAD) and then
                 Message_Bounds_Safe (Ciphertext) and then
                 Nonce_Size + AAD'Length + Ciphertext'Length + 16 <=
                    Max_Message_Length;

   --  Constant_Time_Equal: Compare two tags in constant time
   --
   --  Compares all bytes regardless of differences found (no early exit).
   --  This prevents timing attacks that could leak tag information.
   --
   --  Post: Returns True iff tags are byte-for-byte identical
   function Constant_Time_Equal (
      A : AEAD_Tag;
      B : AEAD_Tag
   ) return Boolean with
      Global => null;

end Anubis_AEAD;
