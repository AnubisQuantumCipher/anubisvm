pragma SPARK_Mode (On);

with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_Types; use Anubis_Types;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;

--  Anubis_MLKEM_Encoding: Byte encoding/decoding for ML-KEM
--
--  Implements ByteEncode and ByteDecode as specified in FIPS 203 Section 4.2.
--  These functions convert between polynomial coefficients and byte arrays.
--
--  ByteEncode_d(F): Encode polynomial F with d bits per coefficient
--  ByteDecode_d(B): Decode byte array B to polynomial with d-bit coefficients
--
--  For ML-KEM-1024:
--  - d=12: Full coefficient encoding (mod q=3329)
--  - d=11: Compressed ciphertext u
--  - d=5:  Compressed ciphertext v
--  - d=1:  Message encoding (1 bit per coefficient)
--
--  References:
--  - NIST FIPS 203 Section 4.2.1 (ByteEncode)
--  - NIST FIPS 203 Section 4.2.2 (ByteDecode)

package Anubis_MLKEM_Encoding with
   SPARK_Mode => On
is

   --  ByteEncode_12: Encode polynomial with 12 bits per coefficient
   --  Output: 384 bytes (256 coefficients × 12 bits / 8)
   procedure ByteEncode_12 (
      F      : in  Polynomial;
      Output : out Encoded_Poly
   ) with
      Global => null,
      Relaxed_Initialization => Output,
      Post => (for all I in Encoded_Poly'Range => Output (I)'Initialized);

   --  ByteDecode_12: Decode 384 bytes to polynomial (mod q)
   --  Coefficients are reduced mod q after decoding
   procedure ByteDecode_12 (
      Input : in  Encoded_Poly;
      F     : out Polynomial
   ) with
      Global => null,
      Relaxed_Initialization => F,
      Post => (for all I in Poly_Index => F (I)'Initialized and F (I) < Q);

   --  ByteEncode_11: Encode polynomial with 11 bits per coefficient
   --  Output: 352 bytes (256 coefficients × 11 bits / 8)
   procedure ByteEncode_11 (
      F      : in  Polynomial;
      Output : out Compressed_Poly_Du
   ) with
      Global => null,
      Relaxed_Initialization => Output,
      Post => Output'Initialized;

   --  ByteDecode_11: Decode 352 bytes to polynomial
   --  Coefficients are bounded by 2^11 = 2048
   procedure ByteDecode_11 (
      Input : in  Compressed_Poly_Du;
      F     : out Polynomial
   ) with
      Global => null,
      Relaxed_Initialization => F,
      Post => (for all I in Poly_Index => F (I)'Initialized and F (I) < 2**Du);

   --  ByteEncode_5: Encode polynomial with 5 bits per coefficient
   --  Output: 160 bytes (256 coefficients × 5 bits / 8)
   procedure ByteEncode_5 (
      F      : in  Polynomial;
      Output : out Compressed_Poly_Dv
   ) with
      Global => null,
      Relaxed_Initialization => Output,
      Post => Output'Initialized;

   --  ByteDecode_5: Decode 160 bytes to polynomial
   --  Coefficients are bounded by 2^5 = 32
   procedure ByteDecode_5 (
      Input : in  Compressed_Poly_Dv;
      F     : out Polynomial
   ) with
      Global => null,
      Relaxed_Initialization => F,
      Post => (for all I in Poly_Index => F (I)'Initialized and F (I) < 2**Dv);

   --  ByteEncode_1: Encode message bits (1 bit per coefficient)
   --  Output: 32 bytes (256 bits)
   procedure ByteEncode_1 (
      F      : in  Polynomial;
      Output : out Message
   ) with
      Global => null;

   --  ByteDecode_1: Decode 32 bytes to polynomial (coefficients 0 or 1)
   procedure ByteDecode_1 (
      Input : in  Message;
      F     : out Polynomial
   ) with
      Global => null,
      Relaxed_Initialization => F,
      Post => (for all I in Poly_Index => F (I)'Initialized and F (I) in 0 | 1);

   --  Encode/decode polynomial vectors
   procedure Encode_Vector_12 (
      V      : in  Poly_Vector;
      Output : out Encoded_Vector
   ) with
      Global => null,
      Relaxed_Initialization => Output,
      Post => Output'Initialized;

   procedure Decode_Vector_12 (
      Input : in  Encoded_Vector;
      V     : out Poly_Vector
   ) with
      Global => null;

end Anubis_MLKEM_Encoding;
