-------------------------------------------------------------------------------
--  ANUBIS VEIL - STARK Field Arithmetic
--  Prime field operations for zk-STARK proofs
--
--  Field: Goldilocks prime p = 2^64 - 2^32 + 1
--  This field is optimal for 64-bit CPUs and STARK operations
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_STARK_Field with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  Field Parameters
   ---------------------------------------------------------------------------

   --  Goldilocks prime: p = 2^64 - 2^32 + 1 = 18446744069414584321
   --  Note: This exceeds Ada"s mod type limit (2^32-1), so we use
   --  explicit modular arithmetic on Unsigned_64
   P : constant Unsigned_64 := 18446744069414584321;

   --  Field element as a subtype of Unsigned_64 with modular semantics
   --  All elements must be < P after operations
   subtype Field_Element is Unsigned_64;

   --  Constants
   Zero : constant Field_Element := 0;
   One  : constant Field_Element := 1;

   --  Generator for multiplicative group
   Generator : constant Field_Element := 7;

   --  2-adicity: largest k such that 2^k divides p-1
   --  p-1 = 2^32 * 4294967295, so Two_Adicity = 32
   Two_Adicity : constant := 32;

   --  Root of unity: omega such that omega^(2^32) = 1
   Two_Adic_Root : constant Field_Element := 1753635133440165772;

   ---------------------------------------------------------------------------
   --  Basic Arithmetic
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Ghost Functions (for specification)
   ---------------------------------------------------------------------------

   --  True if element is in canonical form (< P)
   function Is_Canonical (E : Field_Element) return Boolean is
      (E < P)
   with Ghost;

   ---------------------------------------------------------------------------
   --  Basic Arithmetic
   ---------------------------------------------------------------------------

   --  Addition (modular)
   function Add (A, B : Field_Element) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B),
      Post => Is_Canonical (Add'Result);

   --  Subtraction (modular)
   function Sub (A, B : Field_Element) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B),
      Post => Is_Canonical (Sub'Result);

   --  Negation
   function Neg (A : Field_Element) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A),
      Post => Is_Canonical (Neg'Result)
             and then (if A = Zero then Neg'Result = Zero);

   --  Multiplication (modular)
   function Mul (A, B : Field_Element) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B),
      Post => Is_Canonical (Mul'Result);

   --  Squaring (optimized)
   function Sqr (A : Field_Element) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A),
      Post => Is_Canonical (Sqr'Result);

   --  Division (modular, requires B /= 0)
   function Div_F (A, B : Field_Element) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B) and B /= Zero,
      Post => Is_Canonical (Div_F'Result);

   --  Multiplicative inverse (Fermat"s little theorem)
   function Inv (A : Field_Element) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A) and A /= Zero,
      Post => Is_Canonical (Inv'Result) and Inv'Result /= Zero;

   --  Exponentiation (binary method)
   function Exp (Base : Field_Element; Exponent : Unsigned_64) return Field_Element with
      Global => null,
      Pre => Is_Canonical (Base),
      Post => Is_Canonical (Exp'Result)
             and then (if Exponent = 0 then Exp'Result = One);

   ---------------------------------------------------------------------------
   --  Operator Overloads
   --  NOTE: Operator renames removed to avoid ambiguity with Interfaces
   --  when Field_Element is a subtype of Unsigned_64. Use explicit
   --  function calls: Add, Sub, Mul, Div_F, Exp instead.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Roots of Unity
   ---------------------------------------------------------------------------

   --  Get primitive n-th root of unity (n must be power of 2, n <= 2^32)
   function Get_Root_Of_Unity (Log_N : Natural) return Field_Element with
      Global => null,
      Pre => Log_N <= Two_Adicity,
      Post => Is_Canonical (Get_Root_Of_Unity'Result);

   --  Compute omega^i for FFT twiddle factors
   function Get_Twiddle (Root : Field_Element; I : Natural) return Field_Element with
      Global => null,
      Pre => Is_Canonical (Root),
      Post => Is_Canonical (Get_Twiddle'Result);

   ---------------------------------------------------------------------------
   --  Batch Operations (for FFT efficiency)
   ---------------------------------------------------------------------------

   type Field_Array is array (Natural range <>) of Field_Element;

   --  Ghost: All elements in array are canonical
   function All_Canonical (Arr : Field_Array) return Boolean is
      (for all I in Arr'Range => Is_Canonical (Arr (I)))
   with Ghost;

   --  Ghost: All elements are non-zero
   function All_Nonzero (Arr : Field_Array) return Boolean is
      (for all I in Arr'Range => Arr (I) /= Zero)
   with Ghost;

   --  Batch addition: C[i] = A[i] + B[i]
   procedure Batch_Add (
      A, B   : Field_Array;
      Result : out Field_Array
   ) with
      Global => null,
      Pre => A'Length = B'Length
             and then A'Length = Result'Length
             and then All_Canonical (A)
             and then All_Canonical (B),
      Post => All_Canonical (Result),
      Always_Terminates;

   --  Batch multiplication: C[i] = A[i] * B[i]
   procedure Batch_Mul (
      A, B   : Field_Array;
      Result : out Field_Array
   ) with
      Global => null,
      Pre => A'Length = B'Length
             and then A'Length = Result'Length
             and then All_Canonical (A)
             and then All_Canonical (B),
      Post => All_Canonical (Result),
      Always_Terminates;

   --  Batch inverse: Result[i] = 1/A[i] (Montgomery batch inversion)
   procedure Batch_Inv (
      A      : Field_Array;
      Result : out Field_Array
   ) with
      Global => null,
      Pre => A'Length = Result'Length
             and then All_Canonical (A)
             and then All_Nonzero (A),
      Post => All_Canonical (Result) and All_Nonzero (Result),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Reduction and Conversion
   ---------------------------------------------------------------------------

   --  Byte array type for conversion (defined before use)
   subtype Byte_Array_8 is Byte_Array (0 .. 7);

   --  Reduce arbitrary 128-bit value to field element
   function Reduce_128 (High, Low : Unsigned_64) return Field_Element with
      Global => null,
      Post => Is_Canonical (Reduce_128'Result);

   --  Convert to/from bytes (little-endian)
   function From_Bytes (Data : Byte_Array_8) return Field_Element with
      Global => null,
      Post => Is_Canonical (From_Bytes'Result);

   function To_Bytes (E : Field_Element) return Byte_Array_8 with
      Global => null,
      Pre => Is_Canonical (E);

   ---------------------------------------------------------------------------
   --  Legendre Symbol and Square Root
   ---------------------------------------------------------------------------

   --  Legendre symbol: returns 1 if A is QR, -1 if NQR, 0 if A=0
   function Legendre (A : Field_Element) return Integer with
      Global => null,
      Pre => Is_Canonical (A),
      Post => Legendre'Result in -1 .. 1
             and then (if A = Zero then Legendre'Result = 0);

   --  Square root (if exists)
   procedure Sqrt (
      A       : Field_Element;
      Root    : out Field_Element;
      Exists  : out Boolean
   ) with
      Global => null,
      Pre => Is_Canonical (A),
      Post => Is_Canonical (Root)
             and then (if A = Zero then Exists and Root = Zero),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Interpolation Helper
   ---------------------------------------------------------------------------

   --  Evaluate polynomial at point (Horner"s method)
   function Poly_Eval (
      Coeffs : Field_Array;
      Point  : Field_Element
   ) return Field_Element with
      Global => null,
      Pre => All_Canonical (Coeffs) and Is_Canonical (Point),
      Post => Is_Canonical (Poly_Eval'Result);

private
   --  Internal reduction for Goldilocks
   function Reduce_Goldilocks (High, Low : Unsigned_64) return Field_Element with
      Global => null,
      Post => Is_Canonical (Reduce_Goldilocks'Result);

end Anubis_STARK_Field;
