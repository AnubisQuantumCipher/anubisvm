pragma SPARK_Mode (On);

with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

--  Anubis_MLDSA_Poly: Polynomial operations for ML-DSA
--
--  Provides polynomial and vector arithmetic operations

package Anubis_MLDSA_Poly with
   SPARK_Mode => On
is

   --  Polynomial addition: result = a + b
   --  Each coefficient of Result is in [0, Q-1]
   procedure Poly_Add (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => Result (I) < Q);

   --  Polynomial subtraction: result = a - b
   --  Each coefficient of Result is in [0, Q-1]
   procedure Poly_Sub (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => Result (I) < Q);

   --  Polynomial coefficient-wise reduction
   --  Each coefficient is reduced to [0, Q-1]
   procedure Poly_Reduce (F : in out Polynomial) with
      Global => null,
      Post => (for all I in Poly_Index => F (I) < Q);

   --  Convert polynomial from Montgomery form to standard form
   procedure Poly_From_Mont (F : in out Polynomial) with
      Global => null;

   --  Polynomial shift by constant: result = a << c
   --  Shift must be <= 63 to avoid overflow in 64-bit multiplication
   procedure Poly_ShiftL (
      A      : in  Polynomial;
      Shift  : in  Natural;
      Result : out Polynomial
   ) with
      Global => null,
      Pre => Shift <= 63,
      Post => (for all I in Poly_Index => Result (I) < Q);

   --  Check if polynomial coefficients are in bounds
   function Poly_Chk_Norm (F : Polynomial; Bound : Natural) return Boolean with
      Global => null;

   --  L-vector addition
   procedure Vec_Add_L (
      A, B   : in  Poly_Vector_L;
      Result : out Poly_Vector_L
   ) with
      Global => null;

   --  L-vector subtraction
   procedure Vec_Sub_L (
      A, B   : in  Poly_Vector_L;
      Result : out Poly_Vector_L
   ) with
      Global => null;

   --  K-vector addition
   procedure Vec_Add_K (
      A, B   : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) with
      Global => null;

   --  K-vector subtraction
   procedure Vec_Sub_K (
      A, B   : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) with
      Global => null;

   --  K-vector coefficient reduction
   procedure Vec_Reduce_K (V : in Out Poly_Vector_K) with
      Global => null;

   --  L-vector coefficient reduction
   procedure Vec_Reduce_L (V : in Out Poly_Vector_L) with
      Global => null;

   --  Convert K-vector from Montgomery form to standard form
   procedure Vec_From_Mont_K (V : in out Poly_Vector_K) with
      Global => null;

   --  Convert L-vector from Montgomery form to standard form
   procedure Vec_From_Mont_L (V : in out Poly_Vector_L) with
      Global => null;

   --  Center K-vector coefficients to [-Q/2, Q/2)
   procedure Vec_Center_K (V : in out Poly_Vector_K) with
      Global => null;

   --  Center L-vector coefficients to [-Q/2, Q/2)
   procedure Vec_Center_L (V : in out Poly_Vector_L) with
      Global => null;

   --  Check L-vector norm bound
   function Vec_Chk_Norm_L (V : Poly_Vector_L; Bound : Natural) return Boolean with
      Global => null;

   --  Check K-vector norm bound
   function Vec_Chk_Norm_K (V : Poly_Vector_K; Bound : Natural) return Boolean with
      Global => null;

   --  Apply Power2Round to all coefficients of a polynomial
   procedure Poly_Power2Round (
      A  : in  Polynomial;
      A1 : out Polynomial;
      A0 : out Polynomial
   ) with
      Global => null;

   --  Apply Power2Round to K-vector
   procedure Vec_Power2Round_K (
      V  : in  Poly_Vector_K;
      V1 : out Poly_Vector_K;
      V0 : out Poly_Vector_K
   ) with
      Global => null;

   --  Apply Decompose to polynomial
   procedure Poly_Decompose (
      A  : in  Polynomial;
      A1 : out Polynomial;
      A0 : out Polynomial
   ) with
      Global => null;

   --  Compute HighBits of K-vector
   procedure Vec_HighBits_K (
      V      : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) with
      Global => null;

   --  Compute LowBits of K-vector
   procedure Vec_LowBits_K (
      V      : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) with
      Global => null;

   --  Make hint for K-vector pair
   procedure Vec_MakeHint_K (
      Z, R   : in  Poly_Vector_K;
      Hint   : out Poly_Vector_K;
      Count  : out Natural
   ) with
      Global => null;

   --  Use hint for K-vector
   procedure Vec_UseHint_K (
      Hint, R : in  Poly_Vector_K;
      Result  : out Poly_Vector_K
   ) with
      Global => null;

end Anubis_MLDSA_Poly;
