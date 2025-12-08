-------------------------------------------------------------------------------
--  ANUBIS ChaCha20 Stream Cipher (RFC 8439)
--
--  Pure SPARK implementation following SPARKNaCl platinum-level patterns.
--  Designed for proof-friendly contracts and complete type safety.
--
--  References:
--  - RFC 8439: ChaCha20 and Poly1305 for IETF Protocols
--  - SPARKNaCl by Rod Chapman (platinum-level formal verification)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_ChaCha20 with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Key_Size   : constant := 32;   --  256-bit key
   Nonce_Size : constant := 12;   --  96-bit nonce (IETF variant)
   Block_Size : constant := 64;   --  512-bit block output

   ---------------------------------------------------------------------------
   --  Types (following SPARKNaCl patterns)
   ---------------------------------------------------------------------------

   --  Bounded index types for proof-friendly indexing
   subtype Key_Index is Natural range 0 .. Key_Size - 1;
   subtype Nonce_Index is Natural range 0 .. Nonce_Size - 1;
   subtype Block_Index is Natural range 0 .. Block_Size - 1;
   subtype State_Index is Natural range 0 .. 15;

   --  Key and nonce types
   type ChaCha20_Key is array (Key_Index) of Byte;
   type ChaCha20_Nonce is array (Nonce_Index) of Byte;
   type ChaCha20_Block is array (Block_Index) of Byte;

   --  ChaCha20 state: 4x4 matrix of 32-bit words
   type State_Matrix is array (State_Index) of Unsigned_32;

   --  Maximum message length (2^38 bytes as per RFC)
   Max_Message_Length : constant := 2**32 - 1;  --  Practical limit

   ---------------------------------------------------------------------------
   --  Core Functions
   ---------------------------------------------------------------------------

   --  Quarter round function (operates on 4 state words)
   --  Uses modular arithmetic - no overflow possible
   procedure Quarter_Round (
      A, B, C, D : in Out Unsigned_32
   ) with
      Global  => null,
      Inline,
      Always_Terminates;

   --  ChaCha20 block function: generates one 64-byte keystream block
   procedure Block_Function (
      Key     : in     ChaCha20_Key;
      Counter : in     Unsigned_32;
      Nonce   : in     ChaCha20_Nonce;
      Output  :    out ChaCha20_Block
   ) with
      Global => null,
      Always_Terminates;

   --  Generate keystream only (no XOR with message)
   procedure ChaCha20 (
      Output  :    out Byte_Array;
      Nonce   : in     ChaCha20_Nonce;
      Key     : in     ChaCha20_Key;
      Counter : in     Unsigned_32
   ) with
      Global => null,
      Pre    => Output'First = 0
                and then Output'Length <= Max_Message_Length,
      Always_Terminates;

   --  Encrypt/decrypt via XOR with keystream
   procedure ChaCha20_XOR (
      Output  :    out Byte_Array;
      Input   : in     Byte_Array;
      Nonce   : in     ChaCha20_Nonce;
      Key     : in     ChaCha20_Key;
      Counter : in     Unsigned_32
   ) with
      Global => null,
      Pre    => Input'First = 0
                and then Output'First = 0
                and then Output'Last = Input'Last
                and then Input'Length <= Max_Message_Length,
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Sanitization
   ---------------------------------------------------------------------------

   procedure Sanitize (Key : out ChaCha20_Key) with
      Global => null,
      Post   => (for all I in Key'Range => Key (I) = 0);

   procedure Sanitize_Block (Block : out ChaCha20_Block) with
      Global => null,
      Post   => (for all I in Block'Range => Block (I) = 0);

end Anubis_ChaCha20;
