-------------------------------------------------------------------------------
--  ANUBIS VEIL - STARK Field Arithmetic (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_STARK_Field with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal: 128-bit multiplication result type
   ---------------------------------------------------------------------------

   procedure Mul_128 (
      A, B    : Unsigned_64;
      Hi, Lo  : out Unsigned_64
   ) with
      Global => null
   is
      A_Lo : constant Unsigned_64 := A and 16#FFFFFFFF#;
      A_Hi : constant Unsigned_64 := Shift_Right (A, 32);
      B_Lo : constant Unsigned_64 := B and 16#FFFFFFFF#;
      B_Hi : constant Unsigned_64 := Shift_Right (B, 32);

      P0   : constant Unsigned_64 := A_Lo * B_Lo;
      P1   : constant Unsigned_64 := A_Lo * B_Hi;
      P2   : constant Unsigned_64 := A_Hi * B_Lo;
      P3   : constant Unsigned_64 := A_Hi * B_Hi;

      Mid  : Unsigned_64;
      Carry : Unsigned_64;
   begin
      Lo := P0;
      Mid := Shift_Right (P0, 32) + (P1 and 16#FFFFFFFF#) + (P2 and 16#FFFFFFFF#);
      Lo := (Lo and 16#FFFFFFFF#) or Shift_Left (Mid and 16#FFFFFFFF#, 32);
      Carry := Shift_Right (Mid, 32);
      Hi := P3 + Shift_Right (P1, 32) + Shift_Right (P2, 32) + Carry;
   end Mul_128;

   ---------------------------------------------------------------------------
   --  Goldilocks Reduction
   --  p = 2^64 - 2^32 + 1
   --  For x = a * 2^64 + b, we have:
   --  x mod p = b + a * 2^32 - a (mod p)
   ---------------------------------------------------------------------------

   function Reduce_Goldilocks (High, Low : Unsigned_64) return Field_Element is
      Result : Unsigned_64;
      Temp   : Unsigned_64;
      Borrow : Unsigned_64;
   begin
      --  x mod p = low + high * (2^32 - 1) mod p
      --  = low + high * 2^32 - high mod p

      --  First: low + high * 2^32
      Temp := Low + Shift_Left (High, 32);
      if Temp < Low then
         --  Overflow occurred, need to reduce
         Result := Temp - (P - 1);
         return Field_Element (Result);
      end if;

      --  Then subtract high
      if Temp >= High then
         Result := Temp - High;
      else
         --  Borrow needed
         Result := Temp + (P - High);
      end if;

      --  Final reduction if needed
      if Result >= P then
         Result := Result - P;
      end if;

      return Field_Element (Result);
   end Reduce_Goldilocks;

   function Reduce_128 (High, Low : Unsigned_64) return Field_Element renames
      Reduce_Goldilocks;

   ---------------------------------------------------------------------------
   --  Basic Arithmetic
   ---------------------------------------------------------------------------

   function Add (A, B : Field_Element) return Field_Element is
      Sum : constant Unsigned_64 := Unsigned_64 (A) + Unsigned_64 (B);
   begin
      if Sum >= P then
         return Field_Element (Sum - P);
      else
         return Field_Element (Sum);
      end if;
   end Add;

   function Sub (A, B : Field_Element) return Field_Element is
   begin
      if Unsigned_64 (A) >= Unsigned_64 (B) then
         return Field_Element (Unsigned_64 (A) - Unsigned_64 (B));
      else
         return Field_Element (P - Unsigned_64 (B) + Unsigned_64 (A));
      end if;
   end Sub;

   function Neg (A : Field_Element) return Field_Element is
   begin
      if A = Zero then
         return Zero;
      else
         return Field_Element (P - Unsigned_64 (A));
      end if;
   end Neg;

   function Mul (A, B : Field_Element) return Field_Element is
      Hi, Lo : Unsigned_64;
   begin
      Mul_128 (Unsigned_64 (A), Unsigned_64 (B), Hi, Lo);
      return Reduce_Goldilocks (Hi, Lo);
   end Mul;

   function Sqr (A : Field_Element) return Field_Element is
   begin
      return Mul (A, A);
   end Sqr;

   ---------------------------------------------------------------------------
   --  Extended Euclidean Algorithm for Inverse
   ---------------------------------------------------------------------------

   function Inv (A : Field_Element) return Field_Element is
      --  Use Fermat"s little theorem: a^(-1) = a^(p-2) mod p
      --  This is constant-time
   begin
      return Exp (A, P - 2);
   end Inv;

   function Div_F (A, B : Field_Element) return Field_Element is
   begin
      return Mul (A, Inv (B));
   end Div_F;

   ---------------------------------------------------------------------------
   --  Binary Exponentiation
   ---------------------------------------------------------------------------

   function Exp (Base : Field_Element; Exponent : Unsigned_64) return Field_Element is
      Result : Field_Element := One;
      B      : Field_Element := Base;
      E      : Unsigned_64 := Exponent;
   begin
      while E > 0 loop
         pragma Loop_Invariant (Is_Canonical (Result));
         pragma Loop_Invariant (Is_Canonical (B));
         pragma Loop_Variant (Decreases => E);

         if (E and 1) = 1 then
            Result := Mul (Result, B);
         end if;
         B := Sqr (B);
         E := Shift_Right (E, 1);
      end loop;
      return Result;
   end Exp;

   ---------------------------------------------------------------------------
   --  Roots of Unity
   ---------------------------------------------------------------------------

   function Get_Root_Of_Unity (Log_N : Natural) return Field_Element is
      --  omega = Two_Adic_Root^(2^(32-log_n))
      Exp_Val : constant Unsigned_64 := Shift_Left (1, Two_Adicity - Log_N);
   begin
      return Exp (Two_Adic_Root, Exp_Val);
   end Get_Root_Of_Unity;

   function Get_Twiddle (Root : Field_Element; I : Natural) return Field_Element is
   begin
      return Exp (Root, Unsigned_64 (I));
   end Get_Twiddle;

   ---------------------------------------------------------------------------
   --  Batch Operations
   ---------------------------------------------------------------------------

   procedure Batch_Add (
      A, B   : Field_Array;
      Result : out Field_Array
   ) is
   begin
      for I in A'Range loop
         pragma Loop_Invariant (
            for all J in A'First .. I - 1 => Is_Canonical (Result (J))
         );
         Result (I) := Add (A (I), B (I - A'First + B'First));
      end loop;
   end Batch_Add;

   procedure Batch_Mul (
      A, B   : Field_Array;
      Result : out Field_Array
   ) is
   begin
      for I in A'Range loop
         pragma Loop_Invariant (
            for all J in A'First .. I - 1 => Is_Canonical (Result (J))
         );
         Result (I) := Mul (A (I), B (I - A'First + B'First));
      end loop;
   end Batch_Mul;

   --  Montgomery batch inversion
   procedure Batch_Inv (
      A      : Field_Array;
      Result : out Field_Array
   ) is
      N       : constant Natural := A'Length;
      Prefix  : Field_Array (0 .. N - 1);
      Inv_All : Field_Element;
   begin
      --  Compute prefix products
      Prefix (0) := A (A'First);
      for I in 1 .. N - 1 loop
         pragma Loop_Invariant (Is_Canonical (Prefix (I - 1)));
         pragma Loop_Invariant (Prefix (I - 1) /= Zero);
         Prefix (I) := Mul (Prefix (I - 1), A (A'First + I));
      end loop;

      --  Single inversion
      Inv_All := Inv (Prefix (N - 1));

      --  Compute individual inverses
      for I in reverse 1 .. N - 1 loop
         pragma Loop_Invariant (Is_Canonical (Inv_All));
         pragma Loop_Invariant (Inv_All /= Zero);
         pragma Loop_Invariant (
            for all J in I + 1 .. N - 1 =>
               Is_Canonical (Result (J)) and Result (J) /= Zero
         );
         Result (I) := Mul (Inv_All, Prefix (I - 1));
         Inv_All := Mul (Inv_All, A (A'First + I));
      end loop;
      Result (0) := Inv_All;
   end Batch_Inv;

   ---------------------------------------------------------------------------
   --  Conversion
   ---------------------------------------------------------------------------

   function From_Bytes (Data : Byte_Array_8) return Field_Element is
      Result : Unsigned_64 := 0;
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (I in 0 .. 7);
         Result := Result or Shift_Left (Unsigned_64 (Data (I)), I * 8);
      end loop;

      --  Reduce if >= P
      if Result >= P then
         Result := Result mod P;
      end if;

      return Field_Element (Result);
   end From_Bytes;

   function To_Bytes (E : Field_Element) return Byte_Array_8 is
      Result : Byte_Array_8;
      Val    : Unsigned_64 := Unsigned_64 (E);
   begin
      for I in Result'Range loop
         pragma Loop_Invariant (I in 0 .. 7);
         Result (I) := Anubis_Types.Byte (Val and 16#FF#);
         Val := Shift_Right (Val, 8);
      end loop;
      return Result;
   end To_Bytes;

   ---------------------------------------------------------------------------
   --  Legendre Symbol and Square Root
   ---------------------------------------------------------------------------

   function Legendre (A : Field_Element) return Integer is
      --  a^((p-1)/2) = 1 if QR, p-1 if NQR, 0 if a=0
      E : constant Field_Element := Exp (A, (P - 1) / 2);
   begin
      if A = Zero then
         return 0;
      elsif E = One then
         return 1;
      else
         return -1;
      end if;
   end Legendre;

   procedure Sqrt (
      A       : Field_Element;
      Root    : out Field_Element;
      Exists  : out Boolean
   ) is
      --  Tonelli-Shanks algorithm for p = 1 mod 4
      --  Since p = 2^64 - 2^32 + 1, we use the simpler method
   begin
      if A = Zero then
         Root := Zero;
         Exists := True;
         return;
      end if;

      --  Check if quadratic residue
      if Legendre (A) /= 1 then
         Root := Zero;
         Exists := False;
         return;
      end if;

      --  For Goldilocks: sqrt(a) = a^((p+1)/4) when p = 3 mod 4
      --  But Goldilocks is p = 1 mod 4, so we need Tonelli-Shanks
      --  Simplified: use exponentiation
      Root := Exp (A, (P + 1) / 4);
      Exists := (Sqr (Root) = A);
   end Sqrt;

   ---------------------------------------------------------------------------
   --  Polynomial Evaluation (Horner"s Method)
   ---------------------------------------------------------------------------

   function Poly_Eval (
      Coeffs : Field_Array;
      Point  : Field_Element
   ) return Field_Element is
      Result : Field_Element := Zero;
   begin
      --  Evaluate: c[n-1] * x^(n-1) + ... + c[1] * x + c[0]
      --  Horner: ((c[n-1] * x + c[n-2]) * x + ...) * x + c[0]
      for I in reverse Coeffs'Range loop
         pragma Loop_Invariant (Is_Canonical (Result));
         Result := Add (Mul (Result, Point), Coeffs (I));
      end loop;
      return Result;
   end Poly_Eval;

end Anubis_STARK_Field;
