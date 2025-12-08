pragma SPARK_Mode (On);

package body Aegis_U256 with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Add64_With_Carry (
      A, B      : in  Word64;
      Carry_In  : in  Boolean;
      Result    : out Word64;
      Carry_Out : out Boolean
   ) is
      Sum : Word64;
      C   : constant Word64 := (if Carry_In then 1 else 0);
   begin
      Sum := A + B;
      Carry_Out := Sum < A;
      Result := Sum + C;
      if Result < Sum then
         Carry_Out := True;
      end if;
   end Add64_With_Carry;

   procedure Sub64_With_Borrow (
      A, B       : in  Word64;
      Borrow_In  : in  Boolean;
      Result     : out Word64;
      Borrow_Out : out Boolean
   ) is
      Diff : Word64;
      B_Adj : constant Word64 := (if Borrow_In then 1 else 0);
   begin
      Diff := A - B;
      Borrow_Out := A < B;
      Result := Diff - B_Adj;
      if Diff < B_Adj then
         Borrow_Out := True;
      end if;
   end Sub64_With_Borrow;

   procedure Mul64 (
      A, B : in  Word64;
      High : out Word64;
      Low  : out Word64
   ) is
      --  Split into 32-bit halves to avoid overflow
      A_Lo : constant Word64 := A and 16#FFFFFFFF#;
      A_Hi : constant Word64 := Shift_Right (A, 32);
      B_Lo : constant Word64 := B and 16#FFFFFFFF#;
      B_Hi : constant Word64 := Shift_Right (B, 32);

      --  Partial products
      P0 : constant Word64 := A_Lo * B_Lo;
      P1 : constant Word64 := A_Lo * B_Hi;
      P2 : constant Word64 := A_Hi * B_Lo;
      P3 : constant Word64 := A_Hi * B_Hi;

      --  Combine with carries
      Mid : Word64;
      Carry : Word64;
   begin
      Low := P0;
      Mid := P1 + P2;
      Carry := (if Mid < P1 then Shift_Left (Word64'(1), 32) else 0);

      --  Add middle contribution to low
      declare
         Mid_Lo : constant Word64 := Shift_Left (Mid and 16#FFFFFFFF#, 32);
         New_Low : constant Word64 := Low + Mid_Lo;
         Low_Carry : constant Word64 := (if New_Low < Low then 1 else 0);
      begin
         Low := New_Low;
         High := P3 + Shift_Right (Mid, 32) + Carry + Low_Carry;
      end;
   end Mul64;

   ---------------------------------------------------------------------------
   --  Comparison Operations
   ---------------------------------------------------------------------------

   function Equal (A, B : U256) return Boolean is
   begin
      return A.Limbs = B.Limbs;
   end Equal;

   function Less_Than (A, B : U256) return Boolean is
   begin
      --  Compare from most significant limb to least
      for I in reverse 0 .. 3 loop
         if A.Limbs (I) < B.Limbs (I) then
            return True;
         elsif A.Limbs (I) > B.Limbs (I) then
            return False;
         end if;
      end loop;
      return False;  -- Equal
   end Less_Than;

   function Less_Than_Or_Equal (A, B : U256) return Boolean is
   begin
      return Less_Than (A, B) or Equal (A, B);
   end Less_Than_Or_Equal;

   function Greater_Than (A, B : U256) return Boolean is
   begin
      return Less_Than (B, A);
   end Greater_Than;

   function Greater_Than_Or_Equal (A, B : U256) return Boolean is
   begin
      return Greater_Than (A, B) or Equal (A, B);
   end Greater_Than_Or_Equal;

   function Is_Zero (A : U256) return Boolean is
   begin
      return Equal (A, U256_Zero);
   end Is_Zero;

   ---------------------------------------------------------------------------
   --  Arithmetic Operations
   ---------------------------------------------------------------------------

   procedure Add (
      A, B     : in  U256;
      Result   : out U256;
      Overflow : out Boolean
   ) is
      Carry : Boolean := False;
   begin
      for I in 0 .. 3 loop
         Add64_With_Carry (A.Limbs (I), B.Limbs (I), Carry,
                          Result.Limbs (I), Carry);
      end loop;
      Overflow := Carry;
   end Add;

   function Add_Mod (A, B : U256) return U256 is
      Result : U256;
      Overflow : Boolean;
   begin
      Add (A, B, Result, Overflow);
      return Result;
   end Add_Mod;

   procedure Sub (
      A, B      : in  U256;
      Result    : out U256;
      Underflow : out Boolean
   ) is
      Borrow : Boolean := False;
   begin
      for I in 0 .. 3 loop
         Sub64_With_Borrow (A.Limbs (I), B.Limbs (I), Borrow,
                           Result.Limbs (I), Borrow);
      end loop;
      Underflow := Borrow;
   end Sub;

   function Sub_Mod (A, B : U256) return U256 is
      Result : U256;
      Underflow : Boolean;
   begin
      Sub (A, B, Result, Underflow);
      return Result;
   end Sub_Mod;

   procedure Mul (
      A, B : in  U256;
      High : out U256;
      Low  : out U256
   ) is
      --  512-bit accumulator as 8 limbs
      Acc : array (0 .. 7) of Word64 := (others => 0);
      H, L : Word64;
      Sum : Word64;
      Carry : Word64;
   begin
      --  Schoolbook multiplication
      for I in 0 .. 3 loop
         Carry := 0;
         for J in 0 .. 3 loop
            Mul64 (A.Limbs (I), B.Limbs (J), H, L);
            --  Add to accumulator at position I+J
            Sum := Acc (I + J) + L + Carry;
            Carry := (if Sum < Acc (I + J) or Sum < L then 1 else 0);
            Acc (I + J) := Sum;
            --  Propagate high part
            Sum := Acc (I + J + 1) + H + Carry;
            Carry := (if Sum < Acc (I + J + 1) or Sum < H then 1 else 0);
            Acc (I + J + 1) := Sum;
         end loop;
         --  Propagate remaining carry
         if I + 4 <= 7 then
            Acc (I + 4) := Acc (I + 4) + Carry;
         end if;
      end loop;

      Low := (Limbs => (Acc (0), Acc (1), Acc (2), Acc (3)));
      High := (Limbs => (Acc (4), Acc (5), Acc (6), Acc (7)));
   end Mul;

   function Mul_Mod (A, B : U256) return U256 is
      High, Low : U256;
   begin
      Mul (A, B, High, Low);
      return Low;
   end Mul_Mod;

   procedure Div_Mod (
      A, B      : in  U256;
      Quotient  : out U256;
      Remainder : out U256
   ) is
      --  Binary long division
      Temp_Rem : U256 := U256_Zero;
      Temp_Quot : U256 := U256_Zero;
   begin
      --  Simple implementation using binary long division
      --  Process bit by bit from MSB to LSB
      for Bit in reverse 0 .. 255 loop
         --  Shift remainder left by 1
         Temp_Rem := Shift_Left (Temp_Rem, 1);

         --  Bring down next bit from dividend
         declare
            Limb_Idx : constant Natural := Bit / 64;
            Bit_Idx : constant Natural := Bit mod 64;
            Bit_Val : constant Word64 :=
               Shift_Right (A.Limbs (Limb_Idx), Bit_Idx) and 1;
         begin
            Temp_Rem.Limbs (0) := Temp_Rem.Limbs (0) or Bit_Val;
         end;

         --  If remainder >= divisor, subtract and set quotient bit
         if Greater_Than_Or_Equal (Temp_Rem, B) then
            declare
               Sub_Result : U256;
               Underflow : Boolean;
            begin
               Sub (Temp_Rem, B, Sub_Result, Underflow);
               Temp_Rem := Sub_Result;  -- Avoid aliasing Temp_Rem as input and output
            end;
            --  Set bit in quotient
            declare
               Limb_Idx : constant Natural := Bit / 64;
               Bit_Idx : constant Natural := Bit mod 64;
            begin
               Temp_Quot.Limbs (Limb_Idx) :=
                  Temp_Quot.Limbs (Limb_Idx) or Shift_Left (Word64'(1), Bit_Idx);
            end;
         end if;
      end loop;

      Quotient := Temp_Quot;
      Remainder := Temp_Rem;
   end Div_Mod;

   function Div (A, B : U256) return U256 is
      Q, R : U256;
   begin
      Div_Mod (A, B, Q, R);
      return Q;
   end Div;

   function Mod_Op (A, B : U256) return U256 is
      Q, R : U256;
   begin
      Div_Mod (A, B, Q, R);
      return R;
   end Mod_Op;

   ---------------------------------------------------------------------------
   --  Bitwise Operations
   ---------------------------------------------------------------------------

   function Bit_And (A, B : U256) return U256 is
      Result : U256;
   begin
      for I in 0 .. 3 loop
         Result.Limbs (I) := A.Limbs (I) and B.Limbs (I);
      end loop;
      return Result;
   end Bit_And;

   function Bit_Or (A, B : U256) return U256 is
      Result : U256;
   begin
      for I in 0 .. 3 loop
         Result.Limbs (I) := A.Limbs (I) or B.Limbs (I);
      end loop;
      return Result;
   end Bit_Or;

   function Bit_Xor (A, B : U256) return U256 is
      Result : U256;
   begin
      for I in 0 .. 3 loop
         Result.Limbs (I) := A.Limbs (I) xor B.Limbs (I);
      end loop;
      return Result;
   end Bit_Xor;

   function Bit_Not (A : U256) return U256 is
      Result : U256;
   begin
      for I in 0 .. 3 loop
         Result.Limbs (I) := not A.Limbs (I);
      end loop;
      return Result;
   end Bit_Not;

   function Shift_Left (A : U256; N : Natural) return U256 is
      Result : U256 := U256_Zero;
      Limb_Shift : constant Natural := N / 64;
      Bit_Shift : constant Natural := N mod 64;
   begin
      if N = 0 then
         return A;
      end if;

      if Limb_Shift >= 4 then
         return U256_Zero;
      end if;

      if Bit_Shift = 0 then
         --  Simple limb shift
         for I in Limb_Shift .. 3 loop
            Result.Limbs (I) := A.Limbs (I - Limb_Shift);
         end loop;
      else
         --  Combined limb and bit shift
         for I in Limb_Shift .. 3 loop
            Result.Limbs (I) := Interfaces.Shift_Left (A.Limbs (I - Limb_Shift), Bit_Shift);
            if I > Limb_Shift then
               Result.Limbs (I) := Result.Limbs (I) or
                  Interfaces.Shift_Right (A.Limbs (I - Limb_Shift - 1), 64 - Bit_Shift);
            end if;
         end loop;
      end if;

      return Result;
   end Shift_Left;

   function Shift_Right (A : U256; N : Natural) return U256 is
      Result : U256 := U256_Zero;
      Limb_Shift : constant Natural := N / 64;
      Bit_Shift : constant Natural := N mod 64;
   begin
      if N = 0 then
         return A;
      end if;

      if Limb_Shift >= 4 then
         return U256_Zero;
      end if;

      if Bit_Shift = 0 then
         --  Simple limb shift
         for I in 0 .. 3 - Limb_Shift loop
            Result.Limbs (I) := A.Limbs (I + Limb_Shift);
         end loop;
      else
         --  Combined limb and bit shift
         for I in 0 .. 3 - Limb_Shift loop
            Result.Limbs (I) := Interfaces.Shift_Right (A.Limbs (I + Limb_Shift), Bit_Shift);
            if I + Limb_Shift < 3 then
               Result.Limbs (I) := Result.Limbs (I) or
                  Interfaces.Shift_Left (A.Limbs (I + Limb_Shift + 1), 64 - Bit_Shift);
            end if;
         end loop;
      end if;

      return Result;
   end Shift_Right;

   ---------------------------------------------------------------------------
   --  Conversion Operations
   ---------------------------------------------------------------------------

   function From_Word64 (V : Word64) return U256 is
   begin
      return (Limbs => (V, 0, 0, 0));
   end From_Word64;

   function To_Word64 (V : U256) return Word64 is
   begin
      return V.Limbs (0);
   end To_Word64;

   function From_Bytes_BE (Data : Hash256) return U256 is
      Result : U256;
   begin
      --  Big-endian: first byte is MSB
      for Limb in 0 .. 3 loop
         declare
            Base : constant Natural := (3 - Limb) * 8;  -- 24, 16, 8, 0
            Val : Word64 := 0;
         begin
            for B in 0 .. 7 loop
               Val := Interfaces.Shift_Left (Val, 8) or Word64 (Data (Base + B));
            end loop;
            Result.Limbs (Limb) := Val;
         end;
      end loop;
      return Result;
   end From_Bytes_BE;

   function To_Bytes_BE (V : U256) return Hash256 is
      Result : Hash256 := (others => 0);  -- Initialize to ensure all bytes set
   begin
      for Limb in 0 .. 3 loop
         declare
            Base : constant Natural := (3 - Limb) * 8;
            Val : Word64 := V.Limbs (Limb);
         begin
            for B in reverse 0 .. 7 loop
               Result (Base + B) := Byte (Val and 16#FF#);
               Val := Interfaces.Shift_Right (Val, 8);
            end loop;
         end;
      end loop;
      return Result;
   end To_Bytes_BE;

   function From_Bytes_LE (Data : Hash256) return U256 is
      Result : U256;
   begin
      --  Little-endian: first byte is LSB
      for Limb in 0 .. 3 loop
         declare
            Base : constant Natural := Limb * 8;
            Val : Word64 := 0;
         begin
            for B in reverse 0 .. 7 loop
               Val := Interfaces.Shift_Left (Val, 8) or Word64 (Data (Base + B));
            end loop;
            Result.Limbs (Limb) := Val;
         end;
      end loop;
      return Result;
   end From_Bytes_LE;

   function To_Bytes_LE (V : U256) return Hash256 is
      Result : Hash256 := (others => 0);  -- Initialize to ensure all bytes set
   begin
      for Limb in 0 .. 3 loop
         declare
            Base : constant Natural := Limb * 8;
            Val : Word64 := V.Limbs (Limb);
         begin
            for B in 0 .. 7 loop
               Result (Base + B) := Byte (Val and 16#FF#);
               Val := Interfaces.Shift_Right (Val, 8);
            end loop;
         end;
      end loop;
      return Result;
   end To_Bytes_LE;

   --  Convenience aliases and additional conversions
   function U64_To_U256 (V : Word64) return U256 is
   begin
      return From_Word64 (V);
   end U64_To_U256;

   function U256_To_U64 (V : U256) return Word64 is
   begin
      return V.Limbs (0);
   end U256_To_U64;

   function Gas_To_U256 (G : Gas_Amount) return U256 is
   begin
      return From_Word64 (Word64 (G));
   end Gas_To_U256;

   function Address_To_U256 (Addr : Contract_Address) return U256 is
      Bytes : Hash256;
   begin
      for I in 0 .. 31 loop
         Bytes (I) := Addr (I);
      end loop;
      return From_Bytes_BE (Bytes);
   end Address_To_U256;

   function U256_To_Address (V : U256) return Contract_Address is
      Bytes : constant Hash256 := To_Bytes_BE (V);
      Result : Contract_Address;
   begin
      for I in 0 .. 31 loop
         Result (I) := Bytes (I);
      end loop;
      return Result;
   end U256_To_Address;

   ---------------------------------------------------------------------------
   --  Utility Operations
   ---------------------------------------------------------------------------

   function CLZ (A : U256) return Natural is
      Count : Natural := 0;
      Val : Word64;
   begin
      --  Check from most significant limb
      for I in reverse 0 .. 3 loop
         pragma Loop_Invariant (Count = Natural (3 - I) * 64);
         if A.Limbs (I) /= 0 then
            Val := A.Limbs (I);
            --  Count leading zeros in this limb (max 63 iterations)
            --  Loop terminates because Val is shifted left, eventually MSB becomes 1
            for Bit in 0 .. 63 loop
               pragma Loop_Invariant (Count <= Natural (3 - I) * 64 + Bit);
               pragma Loop_Invariant (Count <= 255);
               exit when (Val and 16#8000_0000_0000_0000#) /= 0;
               Count := Count + 1;
               Val := Interfaces.Shift_Left (Val, 1);
            end loop;
            return Count;
         end if;
         Count := Count + 64;
      end loop;
      return 256;  -- All zeros
   end CLZ;

   function CTZ (A : U256) return Natural is
      Count : Natural := 0;
      Val : Word64;
   begin
      --  Check from least significant limb
      for I in 0 .. 3 loop
         pragma Loop_Invariant (Count = Natural (I) * 64);
         if A.Limbs (I) /= 0 then
            Val := A.Limbs (I);
            --  Count trailing zeros in this limb (max 63 iterations since Val /= 0)
            --  Loop terminates because Val is shifted right, eventually LSB becomes 1
            for Bit in 0 .. 63 loop
               pragma Loop_Invariant (Count <= Natural (I) * 64 + Bit);
               pragma Loop_Invariant (Count <= 255);
               exit when (Val and 1) /= 0;
               Count := Count + 1;
               Val := Interfaces.Shift_Right (Val, 1);
            end loop;
            return Count;
         end if;
         Count := Count + 64;
      end loop;
      return 256;  -- All zeros
   end CTZ;

   function Bit_Length (A : U256) return Natural is
   begin
      return 256 - CLZ (A);
   end Bit_Length;

   function Byte_Length (A : U256) return Natural is
      Bits : constant Natural := Bit_Length (A);
   begin
      return (Bits + 7) / 8;
   end Byte_Length;

   ---------------------------------------------------------------------------
   --  Modular Exponentiation
   ---------------------------------------------------------------------------

   function Mod_Exp (
      Base    : U256;
      Exp     : U256;
      Modulus : U256
   ) return U256 is
      Result : U256 := U256_One;
      B : U256 := Mod_Op (Base, Modulus);
      E : U256 := Exp;
   begin
      --  Square-and-multiply algorithm
      --  Loop terminates because E is shifted right each iteration
      --  At most 256 iterations (number of bits in U256)
      for Bit in 0 .. 255 loop
         exit when Is_Zero (E);
         if (E.Limbs (0) and 1) = 1 then
            --  Result := (Result * B) mod Modulus
            Result := Mod_Op (Mul_Mod (Result, B), Modulus);
         end if;
         --  B := (B * B) mod Modulus
         B := Mod_Op (Mul_Mod (B, B), Modulus);
         --  E := E >> 1
         E := Shift_Right (E, 1);
      end loop;
      return Result;
   end Mod_Exp;

end Aegis_U256;
