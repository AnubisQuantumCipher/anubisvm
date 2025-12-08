pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

--  Anubis_MLDSA_Sample: Sampling operations for ML-DSA
--
--  Provides deterministic generation of matrix A, secret vectors s1/s2,
--  and masking polynomial y using SHAKE128/SHAKE256

package Anubis_MLDSA_Sample with
   SPARK_Mode => On
is

   --  ExpandA: Generate matrix A from seed ρ
   --  A[i,j] = RejNTTPoly(SHAKE128(ρ ‖ j ‖ i))
   procedure ExpandA (
      Rho    : in  Seed;
      A_Hat  : out Poly_Matrix
   ) with
      Global => null,
      Always_Terminates => True;
   pragma Annotate (GNATprove, Terminating, ExpandA);

   --  ExpandS: Generate secret vectors s1, s2 from seed ρ"
   --  s1[i] = RejBoundedPoly(SHAKE256(ρ" ‖ i))
   --  s2[i] = RejBoundedPoly(SHAKE256(ρ" ‖ l + i))
   procedure ExpandS (
      Rho_Prime : in  Seed;
      S1        : out Poly_Vector_L;
      S2        : out Poly_Vector_K
   ) with
      Global => null;

   --  Maximum Kappa value to prevent overflow when adding L-1
   Max_Kappa : constant := 2**20;  --  Generous bound, more than needed

   --  ExpandMask: Generate masking vector y from seed ρ"" and counter κ
   --  y[i] = SampleInBall(SHAKE256(ρ"" ‖ κ ‖ i))
   procedure ExpandMask (
      Rho_Prime : in  Seed;
      Kappa     : in  Natural;
      Y         : out Poly_Vector_L
   ) with
      Global => null,
      Pre => Kappa <= Max_Kappa;

   --  SampleInBall: Generate challenge polynomial c with τ ±1 coefficients
   --  Uses SHAKE256(μ ‖ w1_bytes)
   procedure SampleInBall (
      Seed_Bytes : in  Byte_Array;
      C          : out Polynomial
   ) with
      Global => null,
      Pre => Seed_Bytes'Last < Natural'Last,
      Always_Terminates => True;
   pragma Annotate (GNATprove, Terminating, SampleInBall);

   --  RejNTTPoly: Sample polynomial in NTT domain using rejection sampling
   procedure RejNTTPoly (
      Seed_Bytes : in  Byte_Array;
      Poly       : out Polynomial
   ) with
      Global => null,
      Pre => Seed_Bytes'Last < Natural'Last,
      Always_Terminates => True;
   pragma Annotate (GNATprove, Terminating, RejNTTPoly);

   --  RejBoundedPoly: Sample polynomial with coefficients in [-η, η]
   procedure RejBoundedPoly (
      Seed_Bytes : in  Byte_Array;
      Poly       : out Polynomial
   ) with
      Global => null,
      Pre => Seed_Bytes'Last < Natural'Last,
      Always_Terminates => True;
   pragma Annotate (GNATprove, Terminating, RejBoundedPoly);

end Anubis_MLDSA_Sample;
