pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_MLDSA_Field with
   SPARK_Mode => On
is

   --  Reduce: Modular reduction to [0, Q-1]
   --  Uses mod Q for provable correctness
   function Reduce (A : Field_Element) return Valid_Field is
      --  Compute in Unsigned_64 for provable bounds
      R : constant Unsigned_64 := Unsigned_64 (A) mod Unsigned_64 (Q);
   begin
      --  R is guaranteed to be in 0 .. Q-1 by definition of mod
      pragma Assert (R < Unsigned_64 (Q));
      return Valid_Field (R);
   end Reduce;

   --  Barrett reduction for 64-bit values
   --  Computes A mod Q for A < Q^2
   function Barrett_Reduce (A : Unsigned_64) return Valid_Field is
      --  Simple modulo reduction - result is always < Q
      R : constant Unsigned_64 := A mod Unsigned_64 (Q);
   begin
      --  R is guaranteed to be in 0 .. Q-1 by definition of mod
      pragma Assert (R < Unsigned_64 (Q));
      return Valid_Field (R);
   end Barrett_Reduce;

   --  Field addition
   function Add (A, B : Valid_Field) return Valid_Field is
      Sum : constant Unsigned_64 := Unsigned_64 (A) + Unsigned_64 (B);
   begin
      if Sum >= Q then
         return Valid_Field (Sum - Q);
      else
         return Valid_Field (Sum);
      end if;
   end Add;

   --  Field subtraction
   function Sub (A, B : Valid_Field) return Valid_Field is
   begin
      if A >= B then
         return Valid_Field (A - B);
      else
         return Valid_Field (Q + A - B);
      end if;
   end Sub;

   --  Field multiplication using Barrett
   function Mul (A, B : Valid_Field) return Valid_Field is
      Product : constant Unsigned_64 := Unsigned_64 (A) * Unsigned_64 (B);
   begin
      return Barrett_Reduce (Product);
   end Mul;

   --  Montgomery reduction: compute A * R^(-1) mod Q
   --  Using SPARK ML-DSA formula: result = (T + M*Q) >> 32
   --  where M = (T mod 2^32) * QInv mod 2^32
   --  Output is guaranteed to be in [0, Q-1]
   function Mont_Reduce (A : Unsigned_64) return Field_Element is
      T_Low : constant Unsigned_32 := Unsigned_32 (A and 16#FFFFFFFF#);
      M_Tmp : constant Unsigned_64 := Unsigned_64 (T_Low) * Unsigned_64 (Mont_Q_Inv);
      M     : constant Unsigned_32 := Unsigned_32 (M_Tmp and 16#FFFFFFFF#);
      U     : constant Unsigned_64 := A + Unsigned_64 (M) * Unsigned_64 (Q);
      Rr    : constant Unsigned_64 := Shift_Right (U, 32);
      --  Final reduction using mod Q for provable correctness
      Result : constant Unsigned_64 := Rr mod Unsigned_64 (Q);
   begin
      --  Result is guaranteed to be in 0 .. Q-1 by definition of mod
      pragma Assert (Result < Unsigned_64 (Q));
      return Field_Element (Result);
   end Mont_Reduce;

   --  Montgomery multiplication
   function Mont_Mul (A, B : Field_Element) return Field_Element is
      Product : constant Unsigned_64 := Unsigned_64 (A) * Unsigned_64 (B);
   begin
      return Mont_Reduce (Product);
   end Mont_Mul;

   --  Convert to Montgomery form
   function To_Mont (A : Valid_Field) return Field_Element is
      Product : constant Unsigned_64 := Unsigned_64 (A) * Unsigned_64 (Mont_R_Sq);
   begin
      return Mont_Reduce (Product);
   end To_Mont;

   --  Convert from Montgomery form: (A * R^(-1)) mod Q
   --  Uses plain modular multiplication (like SPARK reference)
   --  This is the final step after INTT to convert back to standard form
   function From_Mont (A : Field_Element) return Valid_Field is
      Result : constant Field_Element := Mont_Reduce (Unsigned_64 (A));
   begin
      --  Mont_Reduce guarantees result < Q via its postcondition
      pragma Assert (Result < Q);
      return Valid_Field (Result);
   end From_Mont;

   --  Freeze to canonical form using mod Q for provable correctness
   function Freeze (A : Field_Element) return Valid_Field is
      --  Compute in Unsigned_64 for provable bounds
      R : constant Unsigned_64 := Unsigned_64 (A) mod Unsigned_64 (Q);
   begin
      --  R is guaranteed to be in 0 .. Q-1 by definition of mod
      pragma Assert (R < Unsigned_64 (Q));
      return Valid_Field (R);
   end Freeze;

   --  Center: convert to signed centered representation
   function Center (A : Valid_Field) return Integer is
      Half_Q : constant := (Q - 1) / 2;
   begin
      if A > Half_Q then
         return Integer (A) - Integer (Q);
      else
         return Integer (A);
      end if;
   end Center;

   --  Power2Round: a = a1 * 2^d + a0 with |a0| <= 2^(d-1)
   procedure Power2Round (
      A  : in  Valid_Field;
      A1 : out Valid_Field;
      A0 : out Integer
   ) is
      Two_D : constant := 2**D;  --  8192 for D=13
      Half_D : constant := 2**(D - 1);  --  4096

      A_Int : constant Natural := Natural (A);
      A0_Unsigned : Natural;
   begin
      --  a0 = a mod 2^d (centered)
      A0_Unsigned := A_Int mod Two_D;

      --  Center a0 to [-2^(d-1), 2^(d-1)]
      if A0_Unsigned > Half_D then
         A0 := Integer (A0_Unsigned) - Two_D;
      else
         A0 := Integer (A0_Unsigned);
      end if;

      --  a1 = (a - a0) / 2^d
      if A0 >= 0 then
         A1 := Valid_Field ((A_Int - A0) / Two_D);
      else
         A1 := Valid_Field ((A_Int + (-A0)) / Two_D);
      end if;
   end Power2Round;

   --  Decompose: compute high and low bits
   --  a = a1 * (2 * gamma2) + a0 with |a0| <= gamma2
   procedure Decompose (
      A      : in  Valid_Field;
      A1     : out Valid_Field;
      A0     : out Integer
   ) is
      Two_Gamma2 : constant := 2 * Gamma2;  --  523776

      A_Plus : Natural;
      A0_Centered : Integer;
      A1_Temp : Natural;
   begin
      --  a+ = a mod^+ q (already in [0, q-1])
      A_Plus := Natural (A);

      --  a0 = a+ mod^± (2*gamma2)
      A0_Centered := A_Plus mod Two_Gamma2;
      if A0_Centered > Integer (Gamma2) then
         A0_Centered := A0_Centered - Two_Gamma2;
      end if;

      --  Handle the case when a+ - a0 = q-1
      if A_Plus - A0_Centered = Q - 1 then
         A1 := 0;
         A0 := A0_Centered - 1;
      else
         A1_Temp := (A_Plus - A0_Centered) / Two_Gamma2;
         A1 := Valid_Field (A1_Temp);
         A0 := A0_Centered;
      end if;
   end Decompose;

   --  HighBits: extract high-order bits
   function HighBits (A : Valid_Field) return Valid_Field is
      A1 : Valid_Field;
      A0 : Integer;
   begin
      Decompose (A, A1, A0);
      return A1;
   end HighBits;

   --  LowBits: extract low-order bits
   function LowBits (A : Valid_Field) return Integer is
      A1 : Valid_Field;
      A0 : Integer;
   begin
      Decompose (A, A1, A0);
      return A0;
   end LowBits;

   --  MakeHint: compute hint bit
   --  FIPS 204: MakeHint(z, r) returns true if HighBits(r) != HighBits(r + z)
   function MakeHint (Z, R : Valid_Field) return Boolean is
      R_High : constant Valid_Field := HighBits (R);  --  HighBits of r
      RZ_Sum : Valid_Field;
      RZ_High : Valid_Field;
   begin
      --  Compute (r + z) mod q
      if Unsigned_64 (Z) + Unsigned_64 (R) >= Q then
         RZ_Sum := Valid_Field (Unsigned_64 (Z) + Unsigned_64 (R) - Q);
      else
         RZ_Sum := Valid_Field (Z + R);
      end if;

      RZ_High := HighBits (RZ_Sum);

      return R_High /= RZ_High;
   end MakeHint;

   --  UseHint: apply hint to recover high bits of r + z
   function UseHint (Hint : Boolean; R : Valid_Field) return Valid_Field is
      R1 : Valid_Field;
      R0 : Integer;
      M : constant := (Q - 1) / (2 * Gamma2);  --  Number of high bits values = 16
   begin
      Decompose (R, R1, R0);

      if not Hint then
         return R1;
      end if;

      --  Apply hint correction
      --  R1 from Decompose is in range 0 .. M-1 = 0 .. 15
      --  So R1 + 1 is at most 16, well within Valid_Field range (< Q = 8380417)
      if R0 > 0 then
         if R1 = Valid_Field (M - 1) then
            return 0;
         else
            --  R1 < M - 1 = 15, so R1 + 1 <= 15 which is far below Q
            --  Use Add function which handles field arithmetic safely
            return Add (R1, 1);
         end if;
      else
         if R1 = 0 then
            return Valid_Field (M - 1);
         else
            --  R1 > 0, so R1 - 1 >= 0
            return Sub (R1, 1);
         end if;
      end if;
   end UseHint;

end Anubis_MLDSA_Field;
