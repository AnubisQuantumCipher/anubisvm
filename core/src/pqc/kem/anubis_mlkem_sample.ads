pragma SPARK_Mode (On);

with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_Types; use Anubis_Types;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;

--  Anubis_MLKEM_Sample: Sampling operations for ML-KEM
--
--  Implements SampleNTT and matrix generation as specified in FIPS 203.
--  Uses SHAKE128 to expand seeds into pseudorandom polynomials.
--
--  SampleNTT: Sample polynomial in NTT domain from XOF output
--  - Uses rejection sampling to ensure uniform distribution mod q
--  - Coefficients are in range [0, q-1]
--
--  Matrix generation: Generate k×k matrix A from seed ρ
--  - A[i,j] = SampleNTT(XOF(ρ ‖ j ‖ i))
--
--  References:
--  - NIST FIPS 203 Section 4.2.2

package Anubis_MLKEM_Sample with
   SPARK_Mode => On
is

   --  Sample polynomial in NTT domain from XOF (SHAKE128) output
   --  Uses rejection sampling: accept coefficients < q, reject >= q
   procedure Sample_NTT (
      XOF_Input : in  Byte_Array;
      Poly      : out Polynomial
   ) with
      Global => null,
      Pre => XOF_Input'Length = 34 and then XOF_Input'Last < Natural'Last,
      Post => (for all I in Poly_Index => Poly (I) < Q);

   --  Generate matrix A from seed ρ (in NTT domain)
   --  A[i,j] = SampleNTT(XOF(ρ ‖ j ‖ i))
   procedure Generate_Matrix (
      Rho    : in  Seed;
      Matrix : out Poly_Matrix
   ) with
      Global => null;

   --  Generate matrix A^T (transpose) from seed ρ
   --  Useful for decapsulation where we need A^T
   procedure Generate_Matrix_Transpose (
      Rho    : in  Seed;
      Matrix : out Poly_Matrix
   ) with
      Global => null;

   --  Sample noise polynomial using CBD_2 from PRF output
   --  Uses SHAKE256(σ ‖ N) to generate CBD input
   procedure Sample_Poly_CBD (
      Sigma  : in  Seed;
      Nonce  : in  Byte;
      Poly   : out Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => Poly (I) < Q);

   --  Sample noise vector (k polynomials) using CBD
   procedure Sample_Vector_CBD (
      Sigma  : in  Seed;
      Offset : in  Natural;
      Vec    : out Poly_Vector
   ) with
      Global => null,
      Pre => Offset <= 256 - K,  --  Rewritten to avoid overflow
      Post => (for all J in Vec_Index =>
                 (for all I in Poly_Index => Vec (J) (I) < Q));

end Anubis_MLKEM_Sample;
