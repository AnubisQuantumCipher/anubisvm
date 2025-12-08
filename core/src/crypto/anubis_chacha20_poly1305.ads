-------------------------------------------------------------------------------
--  ANUBIS ChaCha20-Poly1305 AEAD (RFC 8439)
--
--  Pure SPARK implementation following SPARKNaCl platinum-level patterns.
--  Combines ChaCha20 stream cipher with Poly1305 MAC for authenticated
--  encryption with associated data (AEAD).
--
--  References:
--  - RFC 8439: ChaCha20 and Poly1305 for IETF Protocols
--  - SPARKNaCl by Rod Chapman (platinum-level formal verification)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_ChaCha20; use Anubis_ChaCha20;
with Anubis_Poly1305; use Anubis_Poly1305;

package Anubis_ChaCha20_Poly1305 with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Key_Size   : constant := 32;   --  256-bit key
   Nonce_Size : constant := 12;   --  96-bit nonce
   Tag_Size   : constant := 16;   --  128-bit authentication tag

   --  Maximum plaintext/ciphertext length
   Max_Message_Length : constant := 2**32 - 64;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype AEAD_Key is ChaCha20_Key;
   subtype AEAD_Nonce is ChaCha20_Nonce;
   subtype AEAD_Tag is Poly1305_Tag;

   ---------------------------------------------------------------------------
   --  AEAD Encryption
   --
   --  Encrypts plaintext and computes authentication tag over AAD + ciphertext.
   --  Per RFC 8439:
   --  1. Generate one-time Poly1305 key using ChaCha20 block 0
   --  2. Encrypt plaintext with ChaCha20 starting at counter 1
   --  3. Compute tag = Poly1305(AAD || pad || ciphertext || pad || lengths)
   ---------------------------------------------------------------------------

   procedure AEAD_Encrypt (
      Ciphertext :    out Byte_Array;
      Tag        :    out AEAD_Tag;
      Plaintext  : in     Byte_Array;
      AAD        : in     Byte_Array;
      Nonce      : in     AEAD_Nonce;
      Key        : in     AEAD_Key
   ) with
      Global => null,
      Pre    => Plaintext'First = 0
                and then Ciphertext'First = 0
                and then AAD'First = 0
                and then Ciphertext'Last = Plaintext'Last
                and then Plaintext'Length <= Max_Message_Length,
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  AEAD Decryption
   --
   --  Verifies tag and decrypts ciphertext if valid.
   --  Returns True if authentication succeeds, False otherwise.
   --  On failure, Plaintext is zeroed for safety.
   ---------------------------------------------------------------------------

   procedure AEAD_Decrypt (
      Plaintext  :    out Byte_Array;
      Success    :    out Boolean;
      Ciphertext : in     Byte_Array;
      Tag        : in     AEAD_Tag;
      AAD        : in     Byte_Array;
      Nonce      : in     AEAD_Nonce;
      Key        : in     AEAD_Key
   ) with
      Global => null,
      Pre    => Ciphertext'First = 0
                and then Plaintext'First = 0
                and then AAD'First = 0
                and then Plaintext'Last = Ciphertext'Last
                and then Ciphertext'Length <= Max_Message_Length,
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Sanitization
   ---------------------------------------------------------------------------

   procedure Sanitize (Key : out AEAD_Key) with
      Global => null,
      Post   => (for all I in Key'Range => Key (I) = 0);

end Anubis_ChaCha20_Poly1305;
