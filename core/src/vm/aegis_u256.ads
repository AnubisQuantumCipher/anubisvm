pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Interfaces; use Interfaces;

--  AEGIS U256: 256-bit Unsigned Integer Arithmetic
--
--  Provides formally verified 256-bit unsigned integer operations
--  for the KHEPRI VM. All operations are proven to be free of
--  overflow and runtime errors.
--
--  Implementation:
--  - Little-endian limb order (limb 0 is least significant)
--  - Operations use carry/borrow chains for correctness
--  - All postconditions proven by GNATprove
--
--  References:
--  - KHEPRI Blueprint v1.0
--  - EVM Yellow Paper (256-bit semantics)

package Aegis_U256 with
   Pure,
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Comparison Operations
   ---------------------------------------------------------------------------

   --  Equality
   function Equal (A, B : U256) return Boolean with
      Global => null,
      Post   => Equal'Result = (A.Limbs = B.Limbs);

   --  Less than
   function Less_Than (A, B : U256) return Boolean with
      Global => null;

   --  Less than or equal
   function Less_Than_Or_Equal (A, B : U256) return Boolean with
      Global => null,
      Post   => Less_Than_Or_Equal'Result = (Less_Than (A, B) or Equal (A, B));

   --  Greater than
   function Greater_Than (A, B : U256) return Boolean with
      Global => null,
      Post   => Greater_Than'Result = Less_Than (B, A);

   --  Greater than or equal
   function Greater_Than_Or_Equal (A, B : U256) return Boolean with
      Global => null,
      Post   => Greater_Than_Or_Equal'Result = (Greater_Than (A, B) or Equal (A, B));

   --  Is zero
   function Is_Zero (A : U256) return Boolean with
      Global => null,
      Post   => Is_Zero'Result = Equal (A, U256_Zero);

   ---------------------------------------------------------------------------
   --  Arithmetic Operations (with overflow detection)
   ---------------------------------------------------------------------------

   --  Addition with carry out
   procedure Add (
      A, B     : in  U256;
      Result   : out U256;
      Overflow : out Boolean
   ) with
      Global => null;

   --  Addition (modular, wraps on overflow)
   function Add_Mod (A, B : U256) return U256 with
      Global => null;

   --  Subtraction with borrow out
   procedure Sub (
      A, B      : in  U256;
      Result    : out U256;
      Underflow : out Boolean
   ) with
      Global => null;

   --  Subtraction (modular, wraps on underflow)
   function Sub_Mod (A, B : U256) return U256 with
      Global => null;

   --  Multiplication with overflow detection
   --  Result is 512 bits (High, Low)
   procedure Mul (
      A, B : in  U256;
      High : out U256;
      Low  : out U256
   ) with
      Global => null;

   --  Multiplication (modular, truncates to 256 bits)
   function Mul_Mod (A, B : U256) return U256 with
      Global => null;

   --  Division and modulo
   --  Pre: B /= 0
   procedure Div_Mod (
      A, B      : in  U256;
      Quotient  : out U256;
      Remainder : out U256
   ) with
      Global => null,
      Pre    => not Is_Zero (B);

   --  Division only
   function Div (A, B : U256) return U256 with
      Global => null,
      Pre    => not Is_Zero (B);

   --  Modulo only
   function Mod_Op (A, B : U256) return U256 with
      Global => null,
      Pre    => not Is_Zero (B);

   ---------------------------------------------------------------------------
   --  Bitwise Operations
   ---------------------------------------------------------------------------

   --  Bitwise AND
   function Bit_And (A, B : U256) return U256 with
      Global => null;

   --  Bitwise OR
   function Bit_Or (A, B : U256) return U256 with
      Global => null;

   --  Bitwise XOR
   function Bit_Xor (A, B : U256) return U256 with
      Global => null;

   --  Bitwise NOT
   function Bit_Not (A : U256) return U256 with
      Global => null;

   --  Left shift by N bits (N < 256)
   function Shift_Left (A : U256; N : Natural) return U256 with
      Global => null,
      Pre    => N < 256;

   --  Right shift by N bits (N < 256)
   function Shift_Right (A : U256; N : Natural) return U256 with
      Global => null,
      Pre    => N < 256;

   ---------------------------------------------------------------------------
   --  Conversion Operations
   ---------------------------------------------------------------------------

   --  From Word64
   function From_Word64 (V : Word64) return U256 with
      Global => null,
      Post   => From_Word64'Result.Limbs (0) = V and
                From_Word64'Result.Limbs (1) = 0 and
                From_Word64'Result.Limbs (2) = 0 and
                From_Word64'Result.Limbs (3) = 0;

   --  To Word64 (truncates)
   function To_Word64 (V : U256) return Word64 with
      Global => null,
      Post   => To_Word64'Result = V.Limbs (0);

   --  From bytes (big-endian, 32 bytes)
   function From_Bytes_BE (Data : Hash256) return U256 with
      Global => null;

   --  To bytes (big-endian, 32 bytes)
   function To_Bytes_BE (V : U256) return Hash256 with
      Global => null;

   --  From bytes (little-endian, 32 bytes)
   function From_Bytes_LE (Data : Hash256) return U256 with
      Global => null;

   --  To bytes (little-endian, 32 bytes)
   function To_Bytes_LE (V : U256) return Hash256 with
      Global => null;

   --  Alias for From_Word64 (convenience)
   function U64_To_U256 (V : Word64) return U256 with
      Global => null,
      Post   => U64_To_U256'Result = From_Word64 (V);

   --  Alias for To_Word64 (convenience)
   function U256_To_U64 (V : U256) return Word64 with
      Global => null,
      Post   => U256_To_U64'Result = V.Limbs (0);

   --  From Gas_Amount (convenience)
   function Gas_To_U256 (G : Gas_Amount) return U256 with
      Global => null;

   --  Address to U256 (addresses are 32 bytes = 256 bits)
   function Address_To_U256 (Addr : Contract_Address) return U256 with
      Global => null;

   --  U256 to Address
   function U256_To_Address (V : U256) return Contract_Address with
      Global => null;

   ---------------------------------------------------------------------------
   --  Utility Operations
   ---------------------------------------------------------------------------

   --  Count leading zeros
   function CLZ (A : U256) return Natural with
      Global => null,
      Post   => CLZ'Result <= 256;

   --  Count trailing zeros
   function CTZ (A : U256) return Natural with
      Global => null,
      Post   => CTZ'Result <= 256;

   --  Bit length (256 - CLZ)
   function Bit_Length (A : U256) return Natural with
      Global => null,
      Post   => Bit_Length'Result <= 256;

   --  Byte length (ceiling of Bit_Length / 8)
   function Byte_Length (A : U256) return Natural with
      Global => null,
      Post   => Byte_Length'Result <= 32;

   ---------------------------------------------------------------------------
   --  Exponentiation
   ---------------------------------------------------------------------------

   --  Modular exponentiation: (Base ^ Exp) mod Modulus
   --  Pre: Modulus /= 0
   function Mod_Exp (
      Base    : U256;
      Exp     : U256;
      Modulus : U256
   ) return U256 with
      Global => null,
      Pre    => not Is_Zero (Modulus);

private

   --  Internal helpers for carry propagation

   --  Add two 64-bit values with carry in, produce result and carry out
   procedure Add64_With_Carry (
      A, B     : in  Word64;
      Carry_In : in  Boolean;
      Result   : out Word64;
      Carry_Out : out Boolean
   ) with
      Global => null,
      Inline;

   --  Subtract two 64-bit values with borrow in, produce result and borrow out
   procedure Sub64_With_Borrow (
      A, B       : in  Word64;
      Borrow_In  : in  Boolean;
      Result     : out Word64;
      Borrow_Out : out Boolean
   ) with
      Global => null,
      Inline;

   --  Multiply two 64-bit values, produce 128-bit result
   procedure Mul64 (
      A, B : in  Word64;
      High : out Word64;
      Low  : out Word64
   ) with
      Global => null,
      Inline;

end Aegis_U256;
