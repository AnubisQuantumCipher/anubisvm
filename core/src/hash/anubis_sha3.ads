pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_Keccak; use Anubis_Keccak;

--  Anubis_SHA3: SHA3-256, SHA3-384, SHA3-512 hash functions and SHAKE XOFs
--
--  Implements FIPS 202 SHA-3 fixed-output hash functions and SHAKE
--  extensible-output functions (XOFs) using the Keccak sponge construction.
--
--  Security parameters (SHA3):
--  - SHA3-256: 128-bit security, 256-bit output, rate=1088 bits (136 bytes)
--  - SHA3-384: 192-bit security, 384-bit output, rate=832 bits (104 bytes)
--  - SHA3-512: 256-bit security, 512-bit output, rate=576 bits (72 bytes)
--
--  Security parameters (SHAKE):
--  - SHAKE128: 128-bit security, arbitrary output, rate=1344 bits (168 bytes)
--  - SHAKE256: 256-bit security, arbitrary output, rate=1088 bits (136 bytes)
--
--  The sponge construction:
--  1. Pad message with 10*1 padding (domain separator varies: 0x06=SHA3, 0x1F=SHAKE)
--  2. Absorb: XOR rate-sized blocks into state, apply Keccak-f
--  3. Squeeze: Extract rate-sized blocks from state (can repeat for XOF)
--
--  All operations are constant-time with respect to message content.

package Anubis_SHA3 with
   SPARK_Mode => On
is

   --  SHA3-256: 256-bit output (32 bytes)
   subtype SHA3_256_Digest is Byte_Array (0 .. 31);

   --  SHA3-384: 384-bit output (48 bytes)
   subtype SHA3_384_Digest is Byte_Array (0 .. 47);

   --  SHA3-512: 512-bit output (64 bytes)
   subtype SHA3_512_Digest is Byte_Array (0 .. 63);

   --  Rate parameters (in bytes) for each variant
   Rate_256 : constant := 136;  -- (1600 - 2*256) / 8
   Rate_384 : constant := 104;  -- (1600 - 2*384) / 8
   Rate_512 : constant :=  72;  -- (1600 - 2*512) / 8
   Rate_SHAKE128 : constant := 168;  -- (1600 - 2*128) / 8
   Rate_SHAKE256 : constant := 136;  -- (1600 - 2*256) / 8

   --  Domain separators for padding
   SHA3_Suffix   : constant Byte := 16#06#;  -- SHA3 fixed-output
   SHAKE_Suffix  : constant Byte := 16#1F#;  -- SHAKE extensible-output
   Keccak_Suffix : constant Byte := 16#01#;  -- Keccak (pre-FIPS, Ethereum)

   --  Maximum message length for safe processing (prevents overflow)
   --  Must be less than Natural'Last to ensure 'Length attribute doesn"t overflow
   Max_Message_Length : constant := Natural'Last / 2;

   --  SHA3-256: Hash arbitrary-length message
   procedure SHA3_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) with
      Global => null,
      Always_Terminates => True,
      Pre => Message'Last < Natural'Last,
      Post => Digest'First = 0 and Digest'Last = 31;

   --  SHA3-384: Hash arbitrary-length message
   procedure SHA3_384 (
      Message : Byte_Array;
      Digest  : out SHA3_384_Digest
   ) with
      Global => null,
      Pre => Message'Last < Natural'Last,
      Post => Digest'First = 0 and Digest'Last = 47;

   --  SHA3-512: Hash arbitrary-length message
   procedure SHA3_512 (
      Message : Byte_Array;
      Digest  : out SHA3_512_Digest
   ) with
      Global => null,
      Pre => Message'Last < Natural'Last,
      Post => Digest'First = 0 and Digest'Last = 63;

   --  Keccak-256: Ethereum-compatible hash (uses 0x01 domain separator)
   --  This is the pre-FIPS version used by Ethereum and other systems.
   --  NOT the same as SHA3-256 (which uses 0x06).
   procedure Keccak_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) with
      Global => null,
      Pre => Message'Last < Natural'Last,
      Post => Digest'First = 0 and Digest'Last = 31;

   --  SHAKE128: Extensible-output function (arbitrary length)
   --  Produces Output_Length bytes of output (can be any positive value)
   procedure SHAKE128 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) with
      Global => null,
      Pre => Output'Last < Natural'Last and then Message'Last < Natural'Last and then
             Output'Length = Output_Length and then Output_Length <= 65535,
      Post => Output'First = Output'First'Old and Output'Last = Output'Last'Old;

   --  SHAKE256: Extensible-output function (arbitrary length)
   --  Produces Output_Length bytes of output (can be any positive value)
   procedure SHAKE256 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) with
      Global => null,
      Always_Terminates => True,
      Pre => Output'Last < Natural'Last and then Message'Last < Natural'Last and then
             Output'Length = Output_Length and then Output_Length <= 65535,
      Post => Output'First = Output'First'Old and Output'Last = Output'Last'Old;

   --  Internal: Absorb one rate-sized block into Keccak state
   --  XORs block into first Rate bytes of state (as lanes), then applies Keccak-f
   procedure Absorb_Block (
      State : in Out State_Array;
      Block : Byte_Array;
      Rate  : Positive
   ) with
      Global => null,
      Pre => Rate >= 8 and then Rate <= 200 and then Rate mod 8 = 0
             and then Block'Length >= 8 and then Block'Length <= 200
             and then Block'Length = Rate;

   --  Internal: Squeeze Rate bytes from Keccak state into output
   --  Extracts first Rate bytes of state (as lanes) into output buffer
   procedure Squeeze_Block (
      State  : State_Array;
      Output : out Byte_Array;
      Rate   : Positive
   ) with
      Global => null,
      Pre => Output'Length >= 8 and then Output'Length <= 200
             and then Rate >= 8 and then Rate <= 200 and then Rate mod 8 = 0
             and then Output'Length = Rate,
      Post => Output'First = Output'First'Old and Output'Last = Output'Last'Old;

private

   --  Convert 8 bytes (little-endian) to Lane
   function Bytes_To_Lane (B : Byte_Array) return Lane with
      Global => null,
      Pre => B'Length = 8;

   --  Convert Lane to 8 bytes (little-endian)
   procedure Lane_To_Bytes (L : Lane; B : out Byte_Array) with
      Global => null,
      Pre => B'Length = 8,
      Post => B'First = B'First'Old and B'Last = B'Last'Old;

end Anubis_SHA3;
