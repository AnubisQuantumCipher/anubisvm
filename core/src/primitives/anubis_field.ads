pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Config; use Anubis_Config;

package Anubis_Field with
   Pure,
   SPARK_Mode => On
is
   --  Field Arithmetic for Z_q where q = 3329
   --
   --  ML-KEM and ML-DSA use the field Z_3329
   --  All polynomial coefficients are in [0, Q-1]
   --
   --  This package provides:
   --  - Barrett reduction for efficient modular reduction
   --  - Montgomery form for fast modular multiplication (used in NTT)
   --  - Constant-time field operations

   --  Field element: value modulo Q
   type Field_Element is mod 2**16 with Size => 16;

   --  Valid field elements are in [0, Q-1]
   subtype Valid_Field is Field_Element range 0 .. Q - 1;

   type Field_Array is array (Natural range <>) of Field_Element;

   --  Barrett reduction constant: floor(2^26 / Q) = 20158
   --  Used for fast reduction without division
   Barrett_Const : constant := 20158;
   Barrett_Shift : constant := 26;

   --  Montgomery constants for NTT
   --  R = 2^16 mod Q
   Mont_R : constant := 2285;

   --  R^(-1) mod Q
   Mont_R_Inv : constant := 169;

   --  -Q^(-1) mod R (for Montgomery reduction)
   Mont_Q_Inv : constant := 3327;

   --  === Basic Field Operations ===

   --  Reduce to [0, Q-1] using conditional subtraction
   function Reduce (A : Field_Element) return Valid_Field with
      Inline_Always,
      Pre => A < 2 * Q,  -- Must be less than 2*Q for single conditional subtract
      Post => Reduce'Result < Q;

   --  Barrett reduction for 32-bit intermediate values
   --  Input must be < 2^24 for correctness
   function Barrett_Reduce (A : Unsigned_32) return Valid_Field with
      Inline_Always,
      Pre => A < 2**24,
      Post => Barrett_Reduce'Result < Q;

   --  Field addition: (A + B) mod Q
   function Add (A, B : Valid_Field) return Valid_Field with
      Inline_Always,
      Post => Add'Result < Q;

   --  Field subtraction: (A - B) mod Q
   function Sub (A, B : Valid_Field) return Valid_Field with
      Inline_Always,
      Post => Sub'Result < Q;

   --  Field multiplication: (A * B) mod Q using Barrett reduction
   function Mul (A, B : Valid_Field) return Valid_Field with
      Inline_Always,
      Post => Mul'Result < Q;

   --  === Montgomery Form Operations (for NTT) ===

   --  Convert to Montgomery form: (A * R) mod Q
   function To_Mont (A : Valid_Field) return Field_Element with
      Inline_Always;

   --  Convert from Montgomery form: (A * R^(-1)) mod Q
   function From_Mont (A : Field_Element) return Valid_Field with
      Inline_Always,
      Post => From_Mont'Result < Q;

   --  Montgomery multiplication: (A * B * R^(-1)) mod Q
   --  Used in NTT for fast modular multiplication
   function Mont_Mul (A, B : Field_Element) return Field_Element with
      Inline_Always;

   --  Montgomery reduction: (A * R^(-1)) mod Q
   --  Converts from Montgomery form implicitly during multiplication
   function Mont_Reduce (A : Unsigned_32) return Field_Element with
      Inline_Always;

   --  === Utility Functions ===

   --  Constant-time conditional selection
   --  Returns A if Cond=1, B if Cond=0
   function CT_Select (Cond : Field_Element; A, B : Field_Element) return Field_Element with
      Inline_Always,
      Pre => Cond in 0 | 1;

end Anubis_Field;
