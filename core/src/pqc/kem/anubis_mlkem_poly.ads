pragma SPARK_Mode (On);

with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;

--  Anubis_MLKEM_Poly: Polynomial arithmetic for ML-KEM
--
--  Implements polynomial and vector arithmetic operations over Z_q[X]/(X^n + 1)
--  where q = 3329 and n = 256.
--
--  Operations:
--  - Polynomial addition, subtraction
--  - NTT/INTT transforms
--  - Point-wise multiplication in NTT domain
--  - Vector operations (dot product, add)
--  - Matrix-vector multiplication
--
--  References:
--  - NIST FIPS 203 Section 4.3

package Anubis_MLKEM_Poly with
   SPARK_Mode => On
is

   --  Polynomial addition: c = a + b (mod q)
   procedure Poly_Add (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => Result (I) < Q);

   --  Polynomial subtraction: c = a - b (mod q)
   procedure Poly_Sub (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => Result (I) < Q);

   --  Forward NTT transform
   procedure Poly_NTT (
      F : in out Polynomial
   ) with
      Global => null,
      Pre => (for all I in Poly_Index => F (I) < Q);

   --  Inverse NTT transform
   procedure Poly_INTT (
      F : in Out Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => F (I) < Q);

   --  Point-wise multiplication in NTT domain: c = a * b
   --  Inputs must be in NTT domain (coefficients < 2*Q from NTT output)
   procedure Poly_Mul_NTT (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) with
      Global => null,
      Pre => (for all I in Poly_Index => A (I) < 2 * Q and B (I) < 2 * Q);

   --  Reduce polynomial coefficients to [0, q-1]
   procedure Poly_Reduce (
      F : in Out Polynomial
   ) with
      Global => null,
      Post => (for all I in Poly_Index => F (I) < Q);

   --  Convert message to polynomial
   --  m_i -> round(q/2) * m_i for decryption comparison
   procedure Msg_To_Poly (
      Msg    : in  Message;
      Result : out Polynomial
   ) with
      Global => null;

   --  Vector addition: c = a + b
   procedure Vec_Add (
      A, B   : in  Poly_Vector;
      Result : out Poly_Vector
   ) with
      Global => null;

   --  Vector subtraction: c = a - b
   procedure Vec_Sub (
      A, B   : in  Poly_Vector;
      Result : out Poly_Vector
   ) with
      Global => null;

   --  Forward NTT on vector
   --  All polynomial coefficients must be < Q
   procedure Vec_NTT (
      V : in Out Poly_Vector
   ) with
      Global => null,
      Pre => (for all J in Vec_Index =>
                 (for all I in Poly_Index => V (J) (I) < Q));

   --  Inverse NTT on vector
   procedure Vec_INTT (
      V : in Out Poly_Vector
   ) with
      Global => null;

   --  Dot product of two vectors in NTT domain
   --  Result = sum(a[i] * b[i]) for i in 0..k-1
   procedure Vec_Dot_Product_NTT (
      A, B   : in  Poly_Vector;
      Result : out Polynomial
   ) with
      Global => null;

   --  Matrix-vector multiplication in NTT domain
   --  Result[i] = sum(M[i,j] * V[j]) for j in 0..k-1
   procedure Matrix_Vec_Mul_NTT (
      M      : in  Poly_Matrix;
      V      : in  Poly_Vector;
      Result : out Poly_Vector
   ) with
      Global => null;

   --  Transpose matrix-vector multiplication in NTT domain
   --  Result[i] = sum(M[j,i] * V[j]) for j in 0..k-1 (using M^T)
   procedure Matrix_T_Vec_Mul_NTT (
      M      : in  Poly_Matrix;
      V      : in  Poly_Vector;
      Result : out Poly_Vector
   ) with
      Global => null;

end Anubis_MLKEM_Poly;
