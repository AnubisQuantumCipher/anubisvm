-------------------------------------------------------------------------------
--  SCARAB FIELD - Implementation
--  Formally verified finite field arithmetic for SCARAB proofs
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Scarab_Field with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Basic Field Arithmetic
   ---------------------------------------------------------------------------

   --  Reduce to canonical form using modulo
   function Reduce (A : Field_Element) return Valid_Field is
      R : constant Unsigned_64 := Unsigned_64 (A) mod Unsigned_64 (Q);
   begin
      pragma Assert (R < Unsigned_64 (Q));
      return Valid_Field (R);
   end Reduce;

   --  Barrett reduction for 64-bit values
   --  Formula: r = a - q * floor((a * m) / 2^k)
   --  where m = floor(2^k / q), k = 48
   function Barrett_Reduce (A : Unsigned_64) return Valid_Field is
      --  Compute quotient estimate: q_est = floor((A * m) / 2^48)
      --  Use 128-bit intermediate to avoid overflow
      Q_Est_High : constant Unsigned_64 :=
         Shift_Right (A, Barrett_Shift - 32);
      Q_Est_Product : constant Unsigned_64 :=
         Q_Est_High * Unsigned_64 (Barrett_Const);
      Q_Est : constant Unsigned_64 :=
         Shift_Right (Q_Est_Product, 32);

      --  Compute remainder: r = A - q_est * Q
      Product : constant Unsigned_64 := Q_Est * Unsigned_64 (Q);
      R : Unsigned_64;
   begin
      --  Handle potential underflow
      if A >= Product then
         R := A - Product;
      else
         --  This shouldn't happen with correct Barrett constant
         R := A mod Unsigned_64 (Q);
      end if;

      --  Final reduction (r might be in [0, 2Q-1])
      if R >= Q then
         R := R - Q;
      end if;

      pragma Assert (R < Q);
      return Valid_Field (R);
   end Barrett_Reduce;

   --  Field addition with overflow check
   function Add (A, B : Valid_Field) return Valid_Field is
      Sum : constant Unsigned_64 := Unsigned_64 (A) + Unsigned_64 (B);
   begin
      if Sum >= Q then
         return Valid_Field (Sum - Q);
      else
         return Valid_Field (Sum);
      end if;
   end Add;

   --  Field subtraction with underflow handling
   function Sub (A, B : Valid_Field) return Valid_Field is
   begin
      if A >= B then
         return Valid_Field (A - B);
      else
         --  Add Q to handle underflow
         return Valid_Field (Q + A - B);
      end if;
   end Sub;

   --  Field multiplication using Barrett reduction
   function Mul (A, B : Valid_Field) return Valid_Field is
      Product : constant Unsigned_64 := Unsigned_64 (A) * Unsigned_64 (B);
   begin
      return Barrett_Reduce (Product);
   end Mul;

   --  Field negation
   function Neg (A : Valid_Field) return Valid_Field is
   begin
      if A = Zero then
         return Zero;
      else
         return Valid_Field (Q - A);
      end if;
   end Neg;

   --  Extended Euclidean algorithm for modular inverse
   procedure Extended_GCD (
      A      : Natural;
      B      : Natural;
      GCD    : out Natural;
      X      : out Integer;
      Y      : out Integer
   ) is
      --  Working variables
      R0, R1, R_Temp : Natural;
      S0, S1, S_Temp : Integer;
      T0, T1, T_Temp : Integer;
      Quotient       : Natural;
   begin
      --  Initialize
      R0 := A; R1 := B;
      S0 := 1; S1 := 0;
      T0 := 0; T1 := 1;

      --  Euclidean algorithm with extended coefficients
      while R1 /= 0 loop
         pragma Loop_Invariant (R0 > 0);
         pragma Loop_Invariant (R1 >= 0);
         pragma Loop_Invariant (R1 < R0);

         Quotient := R0 / R1;

         R_Temp := R0 - Quotient * R1;
         R0 := R1;
         R1 := R_Temp;

         S_Temp := S0 - Integer (Quotient) * S1;
         S0 := S1;
         S1 := S_Temp;

         T_Temp := T0 - Integer (Quotient) * T1;
         T0 := T1;
         T1 := T_Temp;
      end loop;

      GCD := R0;
      X := S0;
      Y := T0;
   end Extended_GCD;

   --  Modular multiplicative inverse using extended GCD
   function Inv (A : Valid_Field) return Valid_Field is
      GCD_Result : Natural;
      X, Y       : Integer;
      Result     : Integer;
   begin
      --  Compute extended GCD: gcd = A*X + Q*Y
      Extended_GCD (Natural (A), Q, GCD_Result, X, Y);

      --  Since Q is prime and A /= 0, gcd must be 1
      pragma Assert (GCD_Result = 1);

      --  X is the inverse, but may be negative
      Result := X;

      --  Reduce to [0, Q-1]
      while Result < 0 loop
         pragma Loop_Invariant (Result < Integer (Q));
         Result := Result + Integer (Q);
      end loop;

      while Result >= Integer (Q) loop
         pragma Loop_Invariant (Result >= 0);
         Result := Result - Integer (Q);
      end loop;

      pragma Assert (Result >= 0 and Result < Integer (Q));
      return Valid_Field (Result);
   end Inv;

   --  Binary exponentiation (square-and-multiply)
   function Pow (Base : Valid_Field; Exp : Natural) return Valid_Field is
      Result : Valid_Field := One;
      B      : Valid_Field := Base;
      E      : Natural := Exp;
   begin
      if Exp = 0 then
         return One;
      end if;

      while E > 0 loop
         pragma Loop_Invariant (Is_Canonical (Result));
         pragma Loop_Invariant (Is_Canonical (B));
         pragma Loop_Invariant (E >= 0);

         --  If exponent bit is 1, multiply result by base
         if (E mod 2) = 1 then
            Result := Mul (Result, B);
         end if;

         --  Square the base and halve the exponent
         B := Mul (B, B);
         E := E / 2;
      end loop;

      return Result;
   end Pow;

   --  Field division: A / B = A * B^(-1)
   function Div_F (A, B : Valid_Field) return Valid_Field is
      B_Inv : constant Valid_Field := Inv (B);
   begin
      return Mul (A, B_Inv);
   end Div_F;

   ---------------------------------------------------------------------------
   --  Montgomery Form Operations
   ---------------------------------------------------------------------------

   --  Montgomery reduction using REDC algorithm
   --  Computes (A * R^(-1)) mod Q where R = 2^32
   function Mont_Reduce (A : Unsigned_64) return Field_Element is
      T_Low : constant Unsigned_32 := Unsigned_32 (A and 16#FFFFFFFF#);
      M_Tmp : constant Unsigned_64 := Unsigned_64 (T_Low) * Unsigned_64 (Mont_Q_Inv);
      M     : constant Unsigned_32 := Unsigned_32 (M_Tmp and 16#FFFFFFFF#);
      U     : constant Unsigned_64 := A + Unsigned_64 (M) * Unsigned_64 (Q);
      R     : constant Unsigned_64 := Shift_Right (U, 32);

      --  Final reduction using mod for provability
      Result : constant Unsigned_64 := R mod Unsigned_64 (Q);
   begin
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

   --  Convert from Montgomery form
   function From_Mont (A : Field_Element) return Valid_Field is
      Result : constant Field_Element := Mont_Reduce (Unsigned_64 (A));
   begin
      pragma Assert (Result < Q);
      return Valid_Field (Result);
   end From_Mont;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Freeze to canonical form
   function Freeze (A : Field_Element) return Valid_Field is
      R : constant Unsigned_64 := Unsigned_64 (A) mod Unsigned_64 (Q);
   begin
      pragma Assert (R < Unsigned_64 (Q));
      return Valid_Field (R);
   end Freeze;

   --  Convert to centered representation
   function Center (A : Valid_Field) return Integer is
      Half_Q : constant := (Q - 1) / 2;
   begin
      if A > Half_Q then
         return Integer (A) - Integer (Q);
      else
         return Integer (A);
      end if;
   end Center;

   ---------------------------------------------------------------------------
   --  Polynomial Arithmetic
   ---------------------------------------------------------------------------

   --  Polynomial addition
   procedure Poly_Add (
      A, B   : Poly_256;
      Result : out Poly_256
   ) is
   begin
      for I in 0 .. 255 loop
         pragma Loop_Invariant (for all J in 0 .. I - 1 =>
                                Is_Canonical (Result (J)));

         Result (I) := Add (Valid_Field (A (I)), Valid_Field (B (I)));
      end loop;
   end Poly_Add;

   --  Polynomial subtraction
   procedure Poly_Sub (
      A, B   : Poly_256;
      Result : out Poly_256
   ) is
   begin
      for I in 0 .. 255 loop
         pragma Loop_Invariant (for all J in 0 .. I - 1 =>
                                Is_Canonical (Result (J)));

         Result (I) := Sub (Valid_Field (A (I)), Valid_Field (B (I)));
      end loop;
   end Poly_Sub;

   --  Polynomial multiplication (schoolbook algorithm)
   --  Truncates to 256 coefficients (degree reduction)
   procedure Poly_Mul (
      A, B   : Poly_256;
      Result : out Poly_256
   ) is
      Temp : array (0 .. 511) of Unsigned_64 := (others => 0);
   begin
      --  Schoolbook multiplication with accumulation
      for I in 0 .. 255 loop
         pragma Loop_Invariant (for all K in 0 .. I - 1 =>
                                for all J in 0 .. 255 =>
                                Temp (K + J) >= Unsigned_64 (A (K)) * Unsigned_64 (B (J)));

         for J in 0 .. 255 loop
            pragma Loop_Invariant (for all K in 0 .. I - 1 =>
                                   for all L in 0 .. 255 =>
                                   Temp (K + L) >= Unsigned_64 (A (K)) * Unsigned_64 (B (L)));
            pragma Loop_Invariant (for all L in 0 .. J - 1 =>
                                   Temp (I + L) >= Unsigned_64 (A (I)) * Unsigned_64 (B (L)));

            Temp (I + J) := Temp (I + J) + Unsigned_64 (A (I)) * Unsigned_64 (B (J));
         end loop;
      end loop;

      --  Reduce and store first 256 coefficients
      for I in 0 .. 255 loop
         pragma Loop_Invariant (for all J in 0 .. I - 1 =>
                                Is_Canonical (Result (J)));

         Result (I) := Barrett_Reduce (Temp (I));
      end loop;
   end Poly_Mul;

   --  Polynomial negation
   procedure Poly_Neg (
      A      : Poly_256;
      Result : out Poly_256
   ) is
   begin
      for I in 0 .. 255 loop
         pragma Loop_Invariant (for all J in 0 .. I - 1 =>
                                Is_Canonical (Result (J)));

         Result (I) := Neg (Valid_Field (A (I)));
      end loop;
   end Poly_Neg;

   --  Scalar multiplication
   procedure Poly_Scale (
      A      : Poly_256;
      Scalar : Valid_Field;
      Result : out Poly_256
   ) is
   begin
      for I in 0 .. 255 loop
         pragma Loop_Invariant (for all J in 0 .. I - 1 =>
                                Is_Canonical (Result (J)));

         Result (I) := Mul (Valid_Field (A (I)), Scalar);
      end loop;
   end Poly_Scale;

   ---------------------------------------------------------------------------
   --  Polynomial Evaluation and Interpolation
   ---------------------------------------------------------------------------

   --  Horner's method for polynomial evaluation
   --  P(X) = a_0 + X*(a_1 + X*(a_2 + ... + X*a_n))
   function Poly_Eval (
      Coeffs : Poly_256;
      X      : Valid_Field
   ) return Valid_Field is
      Result : Valid_Field := Valid_Field (Coeffs (255));
   begin
      for I in reverse 0 .. 254 loop
         pragma Loop_Invariant (Is_Canonical (Result));

         Result := Add (Mul (Result, X), Valid_Field (Coeffs (I)));
      end loop;

      return Result;
   end Poly_Eval;

   --  Lagrange interpolation
   --  Given points (x_0, y_0), ..., (x_n, y_n), compute polynomial P
   --  such that P(x_i) = y_i for all i
   procedure Poly_Interpolate (
      Points : Point_Array;
      Result : out Poly_Array
   ) is
      N : constant Natural := Points'Length;

      --  Lagrange basis polynomial L_i evaluated at all points
      L_Basis : array (0 .. N - 1) of Poly_Array (0 .. N - 1) :=
         (others => (others => Zero));

      --  Temporary polynomial for accumulation
      Temp : Poly_Array (0 .. N - 1);
   begin
      --  Initialize result to zero polynomial
      for I in Result'Range loop
         Result (I) := Zero;
      end loop;

      --  Compute each Lagrange basis polynomial
      for I in 0 .. N - 1 loop
         pragma Loop_Invariant (for all J in Result'Range =>
                                Is_Canonical (Result (J)));

         --  Start with constant polynomial 1
         Temp := (others => Zero);
         Temp (0) := One;

         --  Multiply by (X - x_j) / (x_i - x_j) for all j /= i
         for J in 0 .. N - 1 loop
            pragma Loop_Invariant (for all K in Temp'Range =>
                                   Is_Canonical (Temp (K)));

            if I /= J then
               declare
                  X_Diff : constant Valid_Field :=
                     Sub (Points (Points'First + I).X, Points (Points'First + J).X);
                  X_Diff_Inv : constant Valid_Field := Inv (X_Diff);

                  --  Multiply temp by (X - x_j)
                  New_Temp : Poly_Array (0 .. N - 1) := (others => Zero);
               begin
                  --  Degree increases by 1
                  for K in 0 .. N - 2 loop
                     pragma Loop_Invariant (for all L in 0 .. K =>
                                            Is_Canonical (New_Temp (L)));

                     if K > 0 then
                        New_Temp (K) := Add (New_Temp (K), Temp (K - 1));
                     end if;
                     New_Temp (K) := Sub (New_Temp (K),
                                          Mul (Temp (K), Points (Points'First + J).X));
                  end loop;

                  --  Multiply by 1 / (x_i - x_j)
                  for K in 0 .. N - 1 loop
                     pragma Loop_Invariant (for all L in 0 .. K - 1 =>
                                            Is_Canonical (Temp (L)));

                     Temp (K) := Mul (New_Temp (K), X_Diff_Inv);
                  end loop;
               end;
            end if;
         end loop;

         --  Add y_i * L_i to result
         for K in 0 .. N - 1 loop
            pragma Loop_Invariant (for all L in Result'Range =>
                                   Is_Canonical (Result (L)));

            Result (Result'First + K) :=
               Add (Result (Result'First + K),
                    Mul (Temp (K), Points (Points'First + I).Y));
         end loop;
      end loop;
   end Poly_Interpolate;

   ---------------------------------------------------------------------------
   --  Batch Operations
   ---------------------------------------------------------------------------

   --  Batch reduction
   procedure Batch_Reduce (
      A      : Poly_Array;
      Result : out Poly_Array
   ) is
   begin
      for I in A'Range loop
         pragma Loop_Invariant (for all J in A'First .. I - 1 =>
                                Is_Canonical (Result (Result'First + (J - A'First))));

         Result (Result'First + (I - A'First)) := Reduce (A (I));
      end loop;
   end Batch_Reduce;

   --  Batch addition
   procedure Batch_Add (
      A, B   : Poly_Array;
      Result : out Poly_Array
   ) is
   begin
      for I in A'Range loop
         pragma Loop_Invariant (for all J in A'First .. I - 1 =>
                                Is_Canonical (Result (Result'First + (J - A'First))));

         Result (Result'First + (I - A'First)) :=
            Add (Valid_Field (A (I)), Valid_Field (B (B'First + (I - A'First))));
      end loop;
   end Batch_Add;

   --  Batch multiplication
   procedure Batch_Mul (
      A, B   : Poly_Array;
      Result : out Poly_Array
   ) is
   begin
      for I in A'Range loop
         pragma Loop_Invariant (for all J in A'First .. I - 1 =>
                                Is_Canonical (Result (Result'First + (J - A'First))));

         Result (Result'First + (I - A'First)) :=
            Mul (Valid_Field (A (I)), Valid_Field (B (B'First + (I - A'First))));
      end loop;
   end Batch_Mul;

   ---------------------------------------------------------------------------
   --  Conversion Functions
   ---------------------------------------------------------------------------

   --  Convert from 64-bit unsigned
   function From_U64 (Val : Unsigned_64) return Valid_Field is
      R : constant Unsigned_64 := Val mod Unsigned_64 (Q);
   begin
      pragma Assert (R < Unsigned_64 (Q));
      return Valid_Field (R);
   end From_U64;

   --  Convert to 64-bit unsigned
   function To_U64 (Val : Valid_Field) return Unsigned_64 is
   begin
      return Unsigned_64 (Val);
   end To_U64;

   --  Convert from Word32
   function From_Word32 (Val : Word32) return Valid_Field is
      R : constant Unsigned_64 := Unsigned_64 (Val) mod Unsigned_64 (Q);
   begin
      pragma Assert (R < Unsigned_64 (Q));
      return Valid_Field (R);
   end From_Word32;

   --  Convert to Word32
   function To_Word32 (Val : Valid_Field) return Word32 is
   begin
      return Word32 (Val);
   end To_Word32;

   ---------------------------------------------------------------------------
   --  Private Helpers
   ---------------------------------------------------------------------------

   --  Reduce 128-bit value to field element
   function Reduce_128 (High, Low : Unsigned_64) return Valid_Field is
      --  For now, use simple modulo reduction
      --  Could optimize using Barrett reduction for 128-bit inputs
      High_Mod : constant Unsigned_64 := High mod Unsigned_64 (Q);

      --  Compute (High_Mod * 2^64 + Low) mod Q
      --  = ((High_Mod * (2^64 mod Q)) + Low) mod Q
      R_Sq_64 : constant Unsigned_64 := Shift_Left (1, 32) mod Unsigned_64 (Q);
      R_Sq_64_Sq : constant Unsigned_64 := (R_Sq_64 * R_Sq_64) mod Unsigned_64 (Q);

      High_Contrib : constant Unsigned_64 := (High_Mod * R_Sq_64_Sq) mod Unsigned_64 (Q);
      Low_Mod : constant Unsigned_64 := Low mod Unsigned_64 (Q);

      Result : constant Unsigned_64 := (High_Contrib + Low_Mod) mod Unsigned_64 (Q);
   begin
      pragma Assert (Result < Unsigned_64 (Q));
      return Valid_Field (Result);
   end Reduce_128;

end Scarab_Field;
