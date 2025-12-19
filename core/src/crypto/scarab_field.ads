-------------------------------------------------------------------------------
--  SCARAB FIELD - Finite Field Arithmetic for SCARAB Proofs
--  Prime field operations for ML-DSA/SCARAB zero-knowledge proofs
--
--  Field: Z_q where q = 8380417 (23-bit prime used by ML-DSA)
--  This package provides complete field arithmetic including:
--  - Basic operations (add, sub, mul, neg, inv, pow)
--  - Polynomial operations (add, sub, mul, eval, interpolation)
--  - Barrett and Montgomery reduction for efficiency
--  - Full SPARK contracts for formal verification
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Scarab_Field with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  Field Parameters
   ---------------------------------------------------------------------------

   --  ML-DSA prime modulus: q = 8380417 (23-bit prime)
   --  Binary: 0x7FE001 = 2^23 - 2^13 - 2^12 - 2^11 - 2^10 - 2^9 + 1
   Q : constant := 8380417;

   --  Field element type (32-bit to handle intermediate computations)
   type Field_Element is mod 2**32 with Size => 32;

   --  Valid field elements are in [0, Q-1]
   subtype Valid_Field is Field_Element range 0 .. Q - 1;

   --  Centered representation: [-Q/2, Q/2] for coefficient bounds
   subtype Centered_Coef is Integer range -(Q / 2) .. (Q / 2);

   --  Constants
   Zero : constant Valid_Field := 0;
   One  : constant Valid_Field := 1;

   ---------------------------------------------------------------------------
   --  Polynomial Types (ML-DSA uses 256-coefficient polynomials)
   ---------------------------------------------------------------------------

   --  Polynomial with 256 coefficients (standard ML-DSA degree)
   type Poly_256 is array (0 .. 255) of Field_Element;

   --  Variable-length polynomial array
   type Poly_Array is array (Natural range <>) of Field_Element;

   --  Polynomial vector (for matrix-vector operations)
   type Poly_Vector is array (Natural range <>) of Poly_256;

   --  Point for interpolation (x, y coordinate)
   type Field_Point is record
      X : Valid_Field;
      Y : Valid_Field;
   end record;

   type Point_Array is array (Natural range <>) of Field_Point;

   ---------------------------------------------------------------------------
   --  Barrett Reduction Constants
   ---------------------------------------------------------------------------

   --  Barrett constant: m = floor(2^48 / Q) for 48-bit Barrett
   --  2^48 / 8380417 ~ 33584992.77
   Barrett_Const : constant := 33584992;
   Barrett_Shift : constant := 48;

   ---------------------------------------------------------------------------
   --  Montgomery Constants (R = 2^32)
   ---------------------------------------------------------------------------

   --  R^2 mod Q = (2^32)^2 mod 8380417
   Mont_R_Sq : constant := 2365951;

   --  -Q^(-1) mod R (for Montgomery REDC algorithm)
   Mont_Q_Inv : constant := 4236238847;

   ---------------------------------------------------------------------------
   --  Ghost Functions (for specification and proof)
   ---------------------------------------------------------------------------

   --  True if element is in canonical form (< Q)
   function Is_Canonical (E : Field_Element) return Boolean is
      (E < Q)
   with Ghost;

   --  True if all elements in array are canonical
   function All_Canonical (Arr : Poly_Array) return Boolean is
      (for all I in Arr'Range => Is_Canonical (Arr (I)))
   with Ghost;

   --  True if polynomial has all canonical coefficients
   function Poly_Is_Canonical (P : Poly_256) return Boolean is
      (for all I in P'Range => Is_Canonical (P (I)))
   with Ghost;

   ---------------------------------------------------------------------------
   --  Basic Field Arithmetic
   ---------------------------------------------------------------------------

   --  Reduce to canonical form [0, Q-1]
   function Reduce (A : Field_Element) return Valid_Field with
      Global => null,
      Post => Is_Canonical (Reduce'Result);

   --  Barrett reduction for 64-bit intermediate values
   --  Efficient modular reduction without division
   function Barrett_Reduce (A : Unsigned_64) return Valid_Field with
      Global => null,
      Post => Is_Canonical (Barrett_Reduce'Result);

   --  Field addition: (A + B) mod Q
   function Add (A, B : Valid_Field) return Valid_Field with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B),
      Post => Is_Canonical (Add'Result);

   --  Field subtraction: (A - B) mod Q
   function Sub (A, B : Valid_Field) return Valid_Field with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B),
      Post => Is_Canonical (Sub'Result);

   --  Field multiplication: (A * B) mod Q
   function Mul (A, B : Valid_Field) return Valid_Field with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B),
      Post => Is_Canonical (Mul'Result);

   --  Field negation: -A mod Q
   function Neg (A : Valid_Field) return Valid_Field with
      Global => null,
      Pre => Is_Canonical (A),
      Post => Is_Canonical (Neg'Result)
             and then (if A = Zero then Neg'Result = Zero);

   --  Field multiplicative inverse: A^(-1) mod Q (requires A /= 0)
   --  Uses extended Euclidean algorithm
   function Inv (A : Valid_Field) return Valid_Field with
      Global => null,
      Pre => Is_Canonical (A) and A /= Zero,
      Post => Is_Canonical (Inv'Result)
             and Inv'Result /= Zero;

   --  Field exponentiation: Base^Exp mod Q
   --  Uses binary exponentiation (square-and-multiply)
   function Pow (Base : Valid_Field; Exp : Natural) return Valid_Field with
      Global => null,
      Pre => Is_Canonical (Base),
      Post => Is_Canonical (Pow'Result)
             and then (if Exp = 0 then Pow'Result = One)
             and then (if Exp = 1 then Pow'Result = Base);

   --  Field division: A / B = A * B^(-1) mod Q
   function Div_F (A, B : Valid_Field) return Valid_Field with
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B) and B /= Zero,
      Post => Is_Canonical (Div_F'Result);

   ---------------------------------------------------------------------------
   --  Montgomery Form Operations (for NTT and fast multiplication)
   ---------------------------------------------------------------------------

   --  Montgomery reduction: (A * R^(-1)) mod Q
   --  Result is guaranteed to be in [0, Q-1]
   function Mont_Reduce (A : Unsigned_64) return Field_Element with
      Global => null,
      Post => Is_Canonical (Mont_Reduce'Result);

   --  Montgomery multiplication: (A * B * R^(-1)) mod Q
   function Mont_Mul (A, B : Field_Element) return Field_Element with
      Global => null,
      Post => Is_Canonical (Mont_Mul'Result);

   --  Convert to Montgomery form: (A * R) mod Q
   function To_Mont (A : Valid_Field) return Field_Element with
      Global => null,
      Pre => Is_Canonical (A),
      Post => Is_Canonical (To_Mont'Result);

   --  Convert from Montgomery form: (A * R^(-1)) mod Q
   function From_Mont (A : Field_Element) return Valid_Field with
      Global => null,
      Post => Is_Canonical (From_Mont'Result);

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Freeze to canonical form (final reduction step)
   function Freeze (A : Field_Element) return Valid_Field with
      Global => null,
      Post => Is_Canonical (Freeze'Result);

   --  Convert to centered representation [-Q/2, Q/2]
   function Center (A : Valid_Field) return Integer with
      Global => null,
      Pre => Is_Canonical (A),
      Post => Center'Result in -(Q / 2) .. (Q / 2);

   ---------------------------------------------------------------------------
   --  Polynomial Arithmetic (coefficientwise operations)
   ---------------------------------------------------------------------------

   --  Polynomial addition: C[i] = (A[i] + B[i]) mod Q
   procedure Poly_Add (
      A, B   : Poly_256;
      Result : out Poly_256
   ) with
      Global => null,
      Pre => Poly_Is_Canonical (A) and Poly_Is_Canonical (B),
      Post => Poly_Is_Canonical (Result),
      Always_Terminates;

   --  Polynomial subtraction: C[i] = (A[i] - B[i]) mod Q
   procedure Poly_Sub (
      A, B   : Poly_256;
      Result : out Poly_256
   ) with
      Global => null,
      Pre => Poly_Is_Canonical (A) and Poly_Is_Canonical (B),
      Post => Poly_Is_Canonical (Result),
      Always_Terminates;

   --  Polynomial multiplication (schoolbook, coefficient domain)
   --  Result polynomial has degree < 512, so truncate to 256 coefficients
   --  For NTT-based multiplication, use NTT package instead
   procedure Poly_Mul (
      A, B   : Poly_256;
      Result : out Poly_256
   ) with
      Global => null,
      Pre => Poly_Is_Canonical (A) and Poly_Is_Canonical (B),
      Post => Poly_Is_Canonical (Result),
      Always_Terminates;

   --  Polynomial negation: C[i] = -A[i] mod Q
   procedure Poly_Neg (
      A      : Poly_256;
      Result : out Poly_256
   ) with
      Global => null,
      Pre => Poly_Is_Canonical (A),
      Post => Poly_Is_Canonical (Result),
      Always_Terminates;

   --  Scalar multiplication: C[i] = (Scalar * A[i]) mod Q
   procedure Poly_Scale (
      A      : Poly_256;
      Scalar : Valid_Field;
      Result : out Poly_256
   ) with
      Global => null,
      Pre => Poly_Is_Canonical (A) and Is_Canonical (Scalar),
      Post => Poly_Is_Canonical (Result),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Polynomial Evaluation and Interpolation
   ---------------------------------------------------------------------------

   --  Evaluate polynomial at point X using Horner's method
   --  P(X) = a_0 + a_1*X + a_2*X^2 + ... + a_n*X^n
   function Poly_Eval (
      Coeffs : Poly_256;
      X      : Valid_Field
   ) return Valid_Field with
      Global => null,
      Pre => Poly_Is_Canonical (Coeffs) and Is_Canonical (X),
      Post => Is_Canonical (Poly_Eval'Result);

   --  Lagrange interpolation: given N points, find polynomial of degree < N
   --  that passes through all points
   --  Returns coefficients of interpolated polynomial
   --  Requires: all points have distinct X coordinates
   procedure Poly_Interpolate (
      Points : Point_Array;
      Result : out Poly_Array
   ) with
      Global => null,
      Pre => Points'Length > 0
             and then Result'Length = Points'Length
             and then (for all I in Points'Range =>
                       Is_Canonical (Points (I).X)
                       and Is_Canonical (Points (I).Y)),
      Post => All_Canonical (Result),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Batch Operations (for proof system efficiency)
   ---------------------------------------------------------------------------

   --  Batch modular reduction
   procedure Batch_Reduce (
      A      : Poly_Array;
      Result : out Poly_Array
   ) with
      Global => null,
      Pre => A'Length = Result'Length,
      Post => All_Canonical (Result),
      Always_Terminates;

   --  Batch addition
   procedure Batch_Add (
      A, B   : Poly_Array;
      Result : out Poly_Array
   ) with
      Global => null,
      Pre => A'Length = B'Length
             and then A'Length = Result'Length
             and then All_Canonical (A)
             and then All_Canonical (B),
      Post => All_Canonical (Result),
      Always_Terminates;

   --  Batch multiplication
   procedure Batch_Mul (
      A, B   : Poly_Array;
      Result : out Poly_Array
   ) with
      Global => null,
      Pre => A'Length = B'Length
             and then A'Length = Result'Length
             and then All_Canonical (A)
             and then All_Canonical (B),
      Post => All_Canonical (Result),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Conversion Functions
   ---------------------------------------------------------------------------

   --  Convert from 64-bit unsigned to field element
   function From_U64 (Val : Unsigned_64) return Valid_Field with
      Global => null,
      Post => Is_Canonical (From_U64'Result);

   --  Convert to 64-bit unsigned (zero-extended)
   function To_U64 (Val : Valid_Field) return Unsigned_64 with
      Global => null,
      Pre => Is_Canonical (Val),
      Post => To_U64'Result < Q;

   --  Convert from 32-bit word
   function From_Word32 (Val : Word32) return Valid_Field with
      Global => null,
      Post => Is_Canonical (From_Word32'Result);

   --  Convert to 32-bit word
   function To_Word32 (Val : Valid_Field) return Word32 with
      Global => null,
      Pre => Is_Canonical (Val),
      Post => Word32 (To_Word32'Result) < Q;

   ---------------------------------------------------------------------------
   --  Extended GCD Helper (for computing modular inverse)
   ---------------------------------------------------------------------------

   --  Extended Euclidean algorithm: find (gcd, x, y) such that:
   --  gcd = A*x + B*y
   --  Used internally by Inv function
   procedure Extended_GCD (
      A      : Natural;
      B      : Natural;
      GCD    : out Natural;
      X      : out Integer;
      Y      : out Integer
   ) with
      Global => null,
      Pre => A > 0 and B > 0,
      Post => GCD > 0 and GCD <= A and GCD <= B,
      Always_Terminates;

private

   --  Internal helper: reduce 128-bit product to field element
   function Reduce_128 (High, Low : Unsigned_64) return Valid_Field with
      Global => null,
      Post => Is_Canonical (Reduce_128'Result);

end Scarab_Field;
