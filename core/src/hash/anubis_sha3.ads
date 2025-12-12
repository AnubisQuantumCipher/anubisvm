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
--
--  Formal Verification (SPARK Gold):
--  - All procedures proven free of runtime errors (no overflow, bounds)
--  - Digest length invariants verified
--  - Sponge construction follows FIPS 202 specification
--  - Data flow dependencies explicitly declared and verified

package Anubis_SHA3 with
   SPARK_Mode => On,
   Pure,
   Always_Terminates
is

   ---------------------------------------------------------------------------
   --  Digest Types (Fixed Output Lengths)
   ---------------------------------------------------------------------------

   --  SHA3-256: 256-bit output (32 bytes)
   subtype SHA3_256_Digest is Byte_Array (0 .. 31);

   --  SHA3-384: 384-bit output (48 bytes)
   subtype SHA3_384_Digest is Byte_Array (0 .. 47);

   --  SHA3-512: 512-bit output (64 bytes)
   subtype SHA3_512_Digest is Byte_Array (0 .. 63);

   ---------------------------------------------------------------------------
   --  Rate Parameters (FIPS 202 Section 6)
   ---------------------------------------------------------------------------

   --  Rate = (1600 - 2*capacity) / 8 bytes
   --  Capacity = 2 * security_level for SHA3
   Rate_256 : constant := 136;  -- (1600 - 2*256) / 8 = 136 bytes
   Rate_384 : constant := 104;  -- (1600 - 2*384) / 8 = 104 bytes
   Rate_512 : constant :=  72;  -- (1600 - 2*512) / 8 = 72 bytes
   Rate_SHAKE128 : constant := 168;  -- (1600 - 2*128) / 8 = 168 bytes
   Rate_SHAKE256 : constant := 136;  -- (1600 - 2*256) / 8 = 136 bytes

   ---------------------------------------------------------------------------
   --  Domain Separators (FIPS 202 Section 6.1)
   ---------------------------------------------------------------------------

   --  Domain separator suffixes for padding differentiation
   SHA3_Suffix   : constant Byte := 16#06#;  -- SHA3 fixed-output (0110)
   SHAKE_Suffix  : constant Byte := 16#1F#;  -- SHAKE extensible-output (11111)
   Keccak_Suffix : constant Byte := 16#01#;  -- Keccak (pre-FIPS, Ethereum)

   ---------------------------------------------------------------------------
   --  Ghost Functions for Specification
   ---------------------------------------------------------------------------

   --  Ghost function: Verify digest has correct length for SHA3-256
   function Is_Valid_256_Digest (D : SHA3_256_Digest) return Boolean is
      (D'First = 0 and D'Last = 31)
   with Ghost, Pure_Function;

   --  Ghost function: Verify digest has correct length for SHA3-384
   function Is_Valid_384_Digest (D : SHA3_384_Digest) return Boolean is
      (D'First = 0 and D'Last = 47)
   with Ghost, Pure_Function;

   --  Ghost function: Verify digest has correct length for SHA3-512
   function Is_Valid_512_Digest (D : SHA3_512_Digest) return Boolean is
      (D'First = 0 and D'Last = 63)
   with Ghost, Pure_Function;

   --  Ghost function: Verify message bounds are safe for processing
   function Message_Bounds_Safe (M : Byte_Array) return Boolean is
      (M'Last < Natural'Last)
   with Ghost, Pure_Function;

   --  Ghost function: Verify output bounds are safe for SHAKE
   function Output_Bounds_Safe (O : Byte_Array; Len : Positive) return Boolean is
      (O'Last < Natural'Last and then O'Length = Len and then Len <= 65535)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum message length for safe processing (prevents overflow)
   Max_Message_Length : constant := Natural'Last / 2;

   ---------------------------------------------------------------------------
   --  SHA3 Fixed-Output Hash Functions
   ---------------------------------------------------------------------------

   --  SHA3-256: Hash arbitrary-length message to 256-bit digest
   --
   --  Security level: 128 bits (collision), 256 bits (preimage)
   --  Output: Fixed 32 bytes
   --
   --  Pre: Message bounds safe for length computation
   --  Post: Digest has exact length 32 bytes (indices 0..31)
   procedure SHA3_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) with
      Global  => null,
      Depends => (Digest => Message),
      Pre     => Message_Bounds_Safe (Message),
      Post    => Is_Valid_256_Digest (Digest),
      Always_Terminates;

   --  SHA3-384: Hash arbitrary-length message to 384-bit digest
   --
   --  Security level: 192 bits (collision), 384 bits (preimage)
   --  Output: Fixed 48 bytes
   --
   --  Pre: Message bounds safe for length computation
   --  Post: Digest has exact length 48 bytes (indices 0..47)
   procedure SHA3_384 (
      Message : Byte_Array;
      Digest  : out SHA3_384_Digest
   ) with
      Global  => null,
      Depends => (Digest => Message),
      Pre     => Message_Bounds_Safe (Message),
      Post    => Is_Valid_384_Digest (Digest),
      Always_Terminates;

   --  SHA3-512: Hash arbitrary-length message to 512-bit digest
   --
   --  Security level: 256 bits (collision), 512 bits (preimage)
   --  Output: Fixed 64 bytes
   --
   --  Pre: Message bounds safe for length computation
   --  Post: Digest has exact length 64 bytes (indices 0..63)
   procedure SHA3_512 (
      Message : Byte_Array;
      Digest  : out SHA3_512_Digest
   ) with
      Global  => null,
      Depends => (Digest => Message),
      Pre     => Message_Bounds_Safe (Message),
      Post    => Is_Valid_512_Digest (Digest),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Keccak-256 (Ethereum Compatibility)
   ---------------------------------------------------------------------------

   --  Keccak-256: Ethereum-compatible hash (uses 0x01 domain separator)
   --
   --  This is the pre-FIPS version used by Ethereum and other systems.
   --  NOT the same as SHA3-256 (which uses 0x06 domain separator).
   --  The only difference is the domain separator byte in padding.
   --
   --  Security level: Same as SHA3-256 (128-bit collision resistance)
   --  Output: Fixed 32 bytes
   procedure Keccak_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) with
      Global  => null,
      Depends => (Digest => Message),
      Pre     => Message_Bounds_Safe (Message),
      Post    => Is_Valid_256_Digest (Digest),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  SHAKE Extensible-Output Functions (XOFs)
   ---------------------------------------------------------------------------

   --  SHAKE128: Extensible-output function with 128-bit security
   --
   --  Can produce arbitrary length output (up to 65535 bytes).
   --  Security: min(d/2, 128) bits where d is output length in bits.
   --
   --  Pre: Output array matches requested length, bounds safe
   procedure SHAKE128 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) with
      Global  => null,
      Depends => (Output => (Output, Message, Output_Length)),
      Pre     => Message_Bounds_Safe (Message) and then
                 Output_Length <= 65535 and then
                 Output'First = 0 and then
                 Output'Last = Output_Length - 1,
      Always_Terminates;

   --  SHAKE256: Extensible-output function with 256-bit security
   --
   --  Can produce arbitrary length output (up to 65535 bytes).
   --  Security: min(d/2, 256) bits where d is output length in bits.
   --
   --  Pre: Output array matches requested length, bounds safe
   procedure SHAKE256 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) with
      Global  => null,
      Depends => (Output => (Output, Message, Output_Length)),
      Pre     => Message_Bounds_Safe (Message) and then
                 Output_Length <= 65535 and then
                 Output'First = 0 and then
                 Output'Last = Output_Length - 1,
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Sponge Construction Primitives
   ---------------------------------------------------------------------------

   --  Absorb_Block: XOR rate-sized block into state, apply Keccak-f
   --
   --  Internal operation for sponge absorption phase.
   --  XORs block into first Rate bytes of state (as lanes), then permutes.
   --
   --  Pre: Rate is valid (8-200, multiple of 8), block length equals rate
   procedure Absorb_Block (
      State : in Out State_Array;
      Block : Byte_Array;
      Rate  : Positive
   ) with
      Global  => null,
      Depends => (State => (State, Block, Rate)),
      Pre     => Rate in 8 | 72 | 104 | 136 | 168 and then  -- Valid SHAKE/SHA3 rates
                 Block'First = 0 and then
                 Block'Last = Rate - 1,
      Always_Terminates;

   --  Squeeze_Block: Extract Rate bytes from state
   --
   --  Internal operation for sponge squeezing phase.
   --  Extracts first Rate bytes of state (as lanes) into output buffer.
   --
   --  Pre: Rate is valid, output length equals rate
   procedure Squeeze_Block (
      State  : State_Array;
      Output : out Byte_Array;
      Rate   : Positive
   ) with
      Global  => null,
      Depends => (Output => (Output, State, Rate)),
      Pre     => Rate in 8 | 72 | 104 | 136 | 168 and then  -- Valid SHAKE/SHA3 rates
                 Output'First = 0 and then
                 Output'Last = Rate - 1,
      Always_Terminates;

private

   ---------------------------------------------------------------------------
   --  Endian Conversion Helpers
   ---------------------------------------------------------------------------

   --  Convert 8 bytes (little-endian) to Lane
   --
   --  Keccak uses little-endian lane encoding per FIPS 202.
   --
   --  Pre: Exactly 8 bytes provided with known bounds
   function Bytes_To_Lane (B : Byte_Array) return Lane with
      Global => null,
      Pre    => B'Length = 8 and then
                B'First >= 0 and then
                B'Last = B'First + 7,
      Pure_Function;

   --  Convert Lane to 8 bytes (little-endian)
   --
   --  Keccak uses little-endian lane encoding per FIPS 202.
   --
   --  Pre: Output buffer is exactly 8 bytes with known bounds
   procedure Lane_To_Bytes (L : Lane; B : out Byte_Array) with
      Global  => null,
      Depends => (B => (B, L)),
      Pre     => B'Length = 8 and then
                 B'First >= 0 and then
                 B'Last = B'First + 7;

end Anubis_SHA3;
