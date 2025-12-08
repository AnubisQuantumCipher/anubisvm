-------------------------------------------------------------------------------
--  ANUBIS VEIL - STARK Polynomial Operations
--  FFT, IFFT, polynomial arithmetic for STARK proofs
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces;         use Interfaces;
with Anubis_STARK_Field; use Anubis_STARK_Field;
with Anubis_Types;

package Anubis_STARK_Poly with
   SPARK_Mode => On
is
   --  Re-export Byte_Array_32 (defined first for use in function signatures)
   subtype Byte_Array_32 is Anubis_Types.Byte_Array (0 .. 31);
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum polynomial degree (2^20 for practical limits)
   Max_Log_Degree : constant := 20;
   Max_Degree     : constant := 2 ** Max_Log_Degree;

   --  Blowup factor for Reed-Solomon encoding
   Default_Blowup : constant := 8;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Poly_Degree is Natural range 0 .. Max_Degree - 1;

   type Polynomial is record
      Coeffs : Field_Array (0 .. Max_Degree - 1);
      Degree : Poly_Degree;
   end record;

   --  Evaluation domain
   type Domain is record
      Size       : Natural;
      Log_Size   : Natural;
      Offset     : Field_Element;  -- Coset offset (generator^k)
      Root       : Field_Element;  -- Root of unity
   end record;

   --  Evaluations over a domain
   type Evaluations is record
      Values : Field_Array (0 .. Max_Degree - 1);
      Count  : Natural;
      Dom    : Domain;
   end record;

   ---------------------------------------------------------------------------
   --  Domain Operations
   ---------------------------------------------------------------------------

   --  Create standard domain (powers of omega)
   function Create_Domain (Log_Size : Natural) return Domain with
      Global => null,
      Pre => Log_Size <= Max_Log_Degree;

   --  Create coset domain (offset * powers of omega)
   function Create_Coset (
      Log_Size : Natural;
      Offset   : Field_Element
   ) return Domain with
      Global => null,
      Pre => Log_Size <= Max_Log_Degree;

   --  Get domain element at index
   function Domain_Element (D : Domain; Index : Natural) return Field_Element with
      Global => null,
      Pre => Index < D.Size;

   ---------------------------------------------------------------------------
   --  FFT Operations (Number Theoretic Transform)
   ---------------------------------------------------------------------------

   --  Forward FFT: Coefficient form -> Evaluation form
   procedure FFT (
      Coeffs : Field_Array;
      Dom    : Domain;
      Evals  : out Field_Array
   ) with
      Global => null,
      Pre => Coeffs'Length = Dom.Size and Evals'Length = Dom.Size;

   --  Inverse FFT: Evaluation form -> Coefficient form
   procedure IFFT (
      Evals  : Field_Array;
      Dom    : Domain;
      Coeffs : out Field_Array
   ) with
      Global => null,
      Pre => Evals'Length = Dom.Size and Coeffs'Length = Dom.Size;

   --  Coset FFT: Evaluate on coset domain
   procedure Coset_FFT (
      Coeffs : Field_Array;
      Dom    : Domain;
      Evals  : out Field_Array
   ) with
      Global => null,
      Pre => Coeffs'Length <= Dom.Size and Evals'Length = Dom.Size;

   --  Coset IFFT: Interpolate from coset evaluations
   procedure Coset_IFFT (
      Evals  : Field_Array;
      Dom    : Domain;
      Coeffs : out Field_Array
   ) with
      Global => null,
      Pre => Evals'Length = Dom.Size and Coeffs'Length = Dom.Size;

   ---------------------------------------------------------------------------
   --  Polynomial Arithmetic
   ---------------------------------------------------------------------------

   --  Create polynomial from coefficients
   function From_Coeffs (Coeffs : Field_Array) return Polynomial with
      Global => null,
      Pre => Coeffs'Length <= Max_Degree;

   --  Create zero polynomial
   function Zero_Poly return Polynomial with
      Global => null,
      Post => Zero_Poly'Result.Degree = 0;

   --  Add two polynomials
   function Poly_Add (A, B : Polynomial) return Polynomial with
      Global => null;

   --  Subtract two polynomials
   function Poly_Sub (A, B : Polynomial) return Polynomial with
      Global => null;

   --  Multiply two polynomials (FFT-based for large polynomials)
   function Poly_Mul (A, B : Polynomial) return Polynomial with
      Global => null,
      Pre => Natural (A.Degree) + Natural (B.Degree) < Max_Degree;

   --  Scalar multiplication
   function Poly_Scale (P : Polynomial; S : Field_Element) return Polynomial with
      Global => null;

   --  Polynomial division with remainder: A = Q * B + R
   procedure Poly_Div (
      A        : Polynomial;
      B        : Polynomial;
      Quotient : out Polynomial;
      Remainder: out Polynomial
   ) with
      Global => null,
      Pre => B.Degree > 0 or B.Coeffs (0) /= Zero;

   --  Evaluate polynomial at a point
   function Poly_Eval (P : Polynomial; X : Field_Element) return Field_Element with
      Global => null;

   --  Evaluate polynomial at multiple points
   procedure Poly_Multi_Eval (
      P      : Polynomial;
      Points : Field_Array;
      Values : out Field_Array
   ) with
      Global => null,
      Pre => Points'Length = Values'Length;

   ---------------------------------------------------------------------------
   --  Interpolation
   ---------------------------------------------------------------------------

   --  Lagrange interpolation from evaluations
   function Interpolate (Evals : Evaluations) return Polynomial with
      Global => null;

   --  Interpolate from points and values
   function Interpolate_Points (
      Points : Field_Array;
      Values : Field_Array
   ) return Polynomial with
      Global => null,
      Pre => Points'Length = Values'Length and Points'Length <= Max_Degree;

   ---------------------------------------------------------------------------
   --  Reed-Solomon Encoding
   ---------------------------------------------------------------------------

   --  Low-degree extend: Interpolate then evaluate on larger domain
   procedure LDE (
      Evals     : Field_Array;
      Blowup    : Positive;
      Extended  : out Field_Array
   ) with
      Global => null,
      Pre => Extended'Length = Evals'Length * Blowup;

   --  Compute quotient polynomial for composition
   --  Q(x) = (P(x) - P(z)) / (x - z)
   function Compute_Quotient (
      P : Polynomial;
      Z : Field_Element
   ) return Polynomial with
      Global => null;

   ---------------------------------------------------------------------------
   --  Vanishing Polynomial
   ---------------------------------------------------------------------------

   --  Compute Z_H(x) = x^n - 1 for domain of size n
   function Vanishing_Poly (Dom : Domain) return Polynomial with
      Global => null;

   --  Evaluate vanishing polynomial at point
   function Eval_Vanishing (Dom : Domain; X : Field_Element) return Field_Element with
      Global => null;

   ---------------------------------------------------------------------------
   --  Commitment Helpers
   ---------------------------------------------------------------------------

   --  Merkle commit to polynomial evaluations
   --  (Returns root hash - actual Merkle tree built separately)
   function Commit_Evaluations (
      Evals : Evaluations
   ) return Byte_Array_32 with
      Global => null;

end Anubis_STARK_Poly;
