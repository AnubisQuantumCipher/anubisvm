pragma SPARK_Mode (On);

with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;

--  Anubis_MLKEM_Compress: Compression operations for ML-KEM
--
--  Implements Compress_d and Decompress_d as specified in FIPS 203 Section 4.2.
--  Compression reduces coefficient size for ciphertext transmission.
--
--  Compress_d(x) = round(2^d / q * x) mod 2^d
--  Decompress_d(y) = round(q / 2^d * y)
--
--  For ML-KEM-1024:
--  - d=11 for u (ciphertext component)
--  - d=5 for v (ciphertext component)
--
--  References:
--  - NIST FIPS 203 Section 4.2.1

package Anubis_MLKEM_Compress with
   SPARK_Mode => On
is

   --  Compress polynomial coefficients from [0, q-1] to [0, 2^d - 1]
   --  Compress_d(x) = round((2^d / q) * x) mod 2^d

   procedure Compress_Du (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => Result (I) < 2**Du);

   procedure Compress_Dv (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => Result (I) < 2**Dv);

   --  Decompress polynomial coefficients from [0, 2^d - 1] to [0, q-1]
   --  Decompress_d(y) = round((q / 2^d) * y)

   --  Single polynomial decompress with proven bound: result < Q always
   --  For input y < 2^11=2048: (y * 3329 + 1024) / 2048 < 3329
   procedure Decompress_Du (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) with
      Global => null,
      Pre => (for all I in Poly_Index => F (I) < 2**Du),
      Post => (for all I in Poly_Index => Result (I) < Q);

   procedure Decompress_Dv (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) with
      Global => null,
      Pre => (for all I in Poly_Index => F (I) < 2**Dv),
      Post => (for all I in Poly_Index => Result (I) < Q);

   --  Compress/decompress vectors
   procedure Compress_Vector_Du (
      V      : in     Poly_Vector;
      Result : out    Poly_Vector
   ) with
      Global => null,
      Post => (for all J in Vec_Index =>
                  (for all I in Poly_Index => Result (J) (I) < 2**Du));

   procedure Decompress_Vector_Du (
      V      : in     Poly_Vector;
      Result : out    Poly_Vector
   ) with
      Global => null,
      Pre => (for all J in Vec_Index =>
                 (for all I in Poly_Index => V (J) (I) < 2**Du)),
      Post => (for all J in Vec_Index =>
                  (for all I in Poly_Index => Result (J) (I) < Q));

end Anubis_MLKEM_Compress;
