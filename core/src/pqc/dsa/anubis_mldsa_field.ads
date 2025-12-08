pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;

--  Anubis_MLDSA_Field: Field arithmetic for Z_q where q = 8380417
--
--  ML-DSA uses a 23-bit prime modulus, requiring 32-bit field elements.
--  This package provides:
--  - Barrett reduction for efficient modular reduction
--  - Montgomery form for fast NTT multiplication
--  - Constant-time operations for side-channel resistance

package Anubis_MLDSA_Field with
   Pure,
   SPARK_Mode => On
is

   --  Field element: 32-bit value modulo Q
   type Field_Element is mod 2**32 with Size => 32;

   --  Valid field elements are in [0, Q-1]
   subtype Valid_Field is Field_Element range 0 .. Q - 1;

   --  Centered representation: [-Q/2, Q/2]
   --  Used for coefficient bounds checking
   subtype Centered_Coef is Integer range -(Q / 2) .. (Q / 2);

   type Field_Array is array (Natural range <>) of Field_Element;

   --  Barrett reduction constant for q = 8380417
   --  m = floor(2^48 / Q) for 48-bit Barrett
   Barrett_Const : constant := 33584992;  --  floor(2^48 / 8380417) = 33584992
   Barrett_Shift : constant := 48;

   --  Montgomery constants for NTT (R = 2^32)
   Mont_R_Sq : constant := 2365951;  --  R^2 mod Q = (2^32)^2 mod Q
   Mont_Q_Inv : constant := 4236238847;  --  -Q^(-1) mod R (for REDC)

   --  === Basic Field Operations ===

   --  Reduce to [0, Q-1] using conditional subtraction
   function Reduce (A : Field_Element) return Valid_Field with
      Inline_Always,
      Post => Reduce'Result < Q;

   --  Barrett reduction for 64-bit intermediate values
   function Barrett_Reduce (A : Unsigned_64) return Valid_Field with
      Inline_Always,
      Post => Barrett_Reduce'Result < Q;

   --  Field addition: (A + B) mod Q
   function Add (A, B : Valid_Field) return Valid_Field with
      Inline_Always,
      Post => Add'Result < Q;

   --  Field subtraction: (A - B) mod Q
   function Sub (A, B : Valid_Field) return Valid_Field with
      Inline_Always,
      Post => Sub'Result < Q;

   --  Field multiplication: (A * B) mod Q
   function Mul (A, B : Valid_Field) return Valid_Field with
      Inline_Always,
      Post => Mul'Result < Q;

   --  === Montgomery Form Operations ===

   --  Montgomery reduction: (A * R^(-1)) mod Q
   --  Result is guaranteed to be in [0, Q-1]
   function Mont_Reduce (A : Unsigned_64) return Field_Element with
      Inline_Always,
      Post => Mont_Reduce'Result < Q;

   --  Montgomery multiplication: (A * B * R^(-1)) mod Q
   --  Result is always < Q due to Mont_Reduce postcondition
   function Mont_Mul (A, B : Field_Element) return Field_Element with
      Inline_Always,
      Post => Mont_Mul'Result < Q;

   --  Convert to Montgomery form: (A * R) mod Q
   --  Result is always < Q due to Mont_Reduce postcondition
   function To_Mont (A : Valid_Field) return Field_Element with
      Inline_Always,
      Post => To_Mont'Result < Q;

   --  Convert from Montgomery form: (A * R^(-1)) mod Q
   function From_Mont (A : Field_Element) return Valid_Field with
      Inline_Always,
      Post => From_Mont'Result < Q;

   --  === Utility Functions ===

   --  Freeze: reduce to canonical [0, Q-1] range
   function Freeze (A : Field_Element) return Valid_Field with
      Inline_Always,
      Post => Freeze'Result < Q;

   --  Center: convert to centered representation
   function Center (A : Valid_Field) return Integer with
      Inline_Always,
      Post => Center'Result in -(Q / 2) .. (Q / 2);

   --  Power2Round: decompose a into (a1, a0) where a = a1*2^d + a0
   procedure Power2Round (
      A  : in  Valid_Field;
      A1 : out Valid_Field;
      A0 : out Integer
   ) with
      Post => A0 in -(2**(D - 1)) .. 2**(D - 1);

   --  Decompose: compute high and low bits for rounding
   procedure Decompose (
      A      : in  Valid_Field;
      A1     : out Valid_Field;
      A0     : out Integer
   ) with
      Post => A0 in -Gamma2 .. Gamma2;

   --  HighBits: extract high-order bits
   function HighBits (A : Valid_Field) return Valid_Field with
      Inline_Always;

   --  LowBits: extract low-order bits (centered)
   function LowBits (A : Valid_Field) return Integer with
      Inline_Always,
      Post => LowBits'Result in -Gamma2 .. Gamma2;

   --  MakeHint: compute hint bit for rounding correction
   function MakeHint (Z, R : Valid_Field) return Boolean with
      Inline_Always;

   --  UseHint: apply hint to recover high bits
   function UseHint (Hint : Boolean; R : Valid_Field) return Valid_Field with
      Inline_Always;

end Anubis_MLDSA_Field;
