-------------------------------------------------------------------------------
--  Test: Field Arithmetic for SCARAB Proofs
--  Demonstrates finite field operations over Z_8380417
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Test code doesn't require SPARK mode

with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Field; use Anubis_Field;
with Interfaces; use Interfaces;

procedure Test_Field_Arithmetic is

   procedure Test_Basic_Arithmetic is
      A : constant Valid_Field := 12345;
      B : constant Valid_Field := 67890;
      Sum, Diff, Prod, Quot, Inv_A : Valid_Field;
   begin
      Put_Line ("=== Basic Field Arithmetic ===");

      --  Test addition
      Sum := Add (A, B);
      Put_Line ("Add(12345, 67890) = " & Valid_Field'Image (Sum));

      --  Test subtraction
      Diff := Sub (A, B);
      Put_Line ("Sub(12345, 67890) = " & Valid_Field'Image (Diff));

      --  Test multiplication
      Prod := Mul (A, B);
      Put_Line ("Mul(12345, 67890) = " & Valid_Field'Image (Prod));

      --  Test inverse
      Inv_A := Inv (A);
      Put_Line ("Inv(12345) = " & Valid_Field'Image (Inv_A));

      --  Verify: A * Inv(A) = 1
      declare
         Check : constant Valid_Field := Mul (A, Inv_A);
      begin
         if Check = One then
            Put_Line ("  Verified: A * Inv(A) = 1 ✓");
         else
            Put_Line ("  ERROR: A * Inv(A) = " & Valid_Field'Image (Check));
         end if;
      end;

      --  Test division
      Quot := Div_F (A, B);
      Put_Line ("Div(12345, 67890) = " & Valid_Field'Image (Quot));

      --  Test negation
      declare
         Neg_A : constant Valid_Field := Neg (A);
         Check : constant Valid_Field := Add (A, Neg_A);
      begin
         Put_Line ("Neg(12345) = " & Valid_Field'Image (Neg_A));
         if Check = Zero then
            Put_Line ("  Verified: A + Neg(A) = 0 ✓");
         else
            Put_Line ("  ERROR: A + Neg(A) = " & Valid_Field'Image (Check));
         end if;
      end;

      New_Line;
   end Test_Basic_Arithmetic;

   procedure Test_Exponentiation is
      Base : constant Valid_Field := 7;
      Exp2, Exp3, Exp10 : Valid_Field;
   begin
      Put_Line ("=== Exponentiation ===");

      Exp2 := Pow (Base, 2);
      Put_Line ("Pow(7, 2) = " & Valid_Field'Image (Exp2));
      Put_Line ("  Expected: 49 = " & Valid_Field'Image (49));

      Exp3 := Pow (Base, 3);
      Put_Line ("Pow(7, 3) = " & Valid_Field'Image (Exp3));
      Put_Line ("  Expected: 343 = " & Valid_Field'Image (343));

      Exp10 := Pow (Base, 10);
      Put_Line ("Pow(7, 10) = " & Valid_Field'Image (Exp10));

      --  Verify Pow(7, 0) = 1
      declare
         Exp0 : constant Valid_Field := Pow (Base, 0);
      begin
         if Exp0 = One then
            Put_Line ("  Verified: Pow(7, 0) = 1 ✓");
         else
            Put_Line ("  ERROR: Pow(7, 0) = " & Valid_Field'Image (Exp0));
         end if;
      end;

      New_Line;
   end Test_Exponentiation;

   procedure Test_Polynomial_Operations is
      P1, P2, P_Sum, P_Diff, P_Prod, P_Scaled : Poly_256;
      X : constant Valid_Field := 5;
      Y : Valid_Field;
   begin
      Put_Line ("=== Polynomial Operations ===");

      --  Initialize simple polynomials: P1 = 1 + 2x + 3x^2
      for I in P1'Range loop
         if I <= 2 then
            P1 (I) := Field_Element (I + 1);
         else
            P1 (I) := Zero;
         end if;
      end loop;

      --  P2 = 4 + 5x
      for I in P2'Range loop
         if I = 0 then
            P2 (I) := 4;
         elsif I = 1 then
            P2 (I) := 5;
         else
            P2 (I) := Zero;
         end if;
      end loop;

      --  Test polynomial addition
      Poly_Add (P1, P2, P_Sum);
      Put_Line ("Poly_Add: First 3 coeffs = " &
                Valid_Field'Image (Valid_Field (P_Sum (0))) & ", " &
                Valid_Field'Image (Valid_Field (P_Sum (1))) & ", " &
                Valid_Field'Image (Valid_Field (P_Sum (2))));
      Put_Line ("  Expected: 5, 7, 3");

      --  Test polynomial subtraction
      Poly_Sub (P1, P2, P_Diff);
      Put_Line ("Poly_Sub: First 3 coeffs = " &
                Valid_Field'Image (Valid_Field (P_Diff (0))) & ", " &
                Valid_Field'Image (Valid_Field (P_Diff (1))) & ", " &
                Valid_Field'Image (Valid_Field (P_Diff (2))));

      --  Test scalar multiplication
      Poly_Scale (P1, 10, P_Scaled);
      Put_Line ("Poly_Scale(P1, 10): First 3 coeffs = " &
                Valid_Field'Image (Valid_Field (P_Scaled (0))) & ", " &
                Valid_Field'Image (Valid_Field (P_Scaled (1))) & ", " &
                Valid_Field'Image (Valid_Field (P_Scaled (2))));
      Put_Line ("  Expected: 10, 20, 30");

      --  Test polynomial evaluation: P1(5) = 1 + 2*5 + 3*25 = 1 + 10 + 75 = 86
      Y := Poly_Eval (P1, X);
      Put_Line ("Poly_Eval(P1, 5) = " & Valid_Field'Image (Y));
      Put_Line ("  Expected: 86");

      --  Test polynomial multiplication (simple case)
      Poly_Mul (P1, P2, P_Prod);
      Put_Line ("Poly_Mul(P1, P2): First 4 coeffs = " &
                Valid_Field'Image (Valid_Field (P_Prod (0))) & ", " &
                Valid_Field'Image (Valid_Field (P_Prod (1))) & ", " &
                Valid_Field'Image (Valid_Field (P_Prod (2))) & ", " &
                Valid_Field'Image (Valid_Field (P_Prod (3))));
      --  (1 + 2x + 3x^2) * (4 + 5x) = 4 + 5x + 8x + 10x^2 + 12x^2 + 15x^3
      --                              = 4 + 13x + 22x^2 + 15x^3
      Put_Line ("  Expected: 4, 13, 22, 15");

      New_Line;
   end Test_Polynomial_Operations;

   procedure Test_Lagrange_Interpolation is
      Points : Point_Array (0 .. 2);
      Coeffs : Poly_Array (0 .. 2);
      Test_X, Test_Y : Valid_Field;
   begin
      Put_Line ("=== Lagrange Interpolation ===");

      --  Define 3 points: (1, 5), (2, 8), (3, 13)
      --  These lie on the line y = 2 + 3x (polynomial of degree 1)
      Points (0) := (X => 1, Y => 5);
      Points (1) := (X => 2, Y => 8);
      Points (2) := (X => 3, Y => 13);

      Put_Line ("Points: (1, 5), (2, 8), (3, 13)");
      Put_Line ("Expected polynomial: 2 + 3x");

      --  Interpolate
      Poly_Interpolate (Points, Coeffs);

      Put_Line ("Interpolated coefficients:");
      for I in Coeffs'Range loop
         Put_Line ("  c[" & Natural'Image (I) & "] = " &
                   Valid_Field'Image (Valid_Field (Coeffs (I))));
      end loop;

      --  Verify: evaluate at x=4, should give y = 2 + 3*4 = 14
      declare
         P_Full : Poly_256 := (others => Zero);
      begin
         for I in Coeffs'Range loop
            P_Full (I) := Coeffs (I);
         end loop;

         Test_X := 4;
         Test_Y := Poly_Eval (P_Full, Test_X);
         Put_Line ("Verification: P(4) = " & Valid_Field'Image (Test_Y));
         Put_Line ("  Expected: 14");
      end;

      New_Line;
   end Test_Lagrange_Interpolation;

   procedure Test_Montgomery_Arithmetic is
      A : constant Valid_Field := 12345;
      B : constant Valid_Field := 67890;
      A_Mont, B_Mont, Prod_Mont, Prod_Normal : Field_Element;
   begin
      Put_Line ("=== Montgomery Arithmetic ===");

      --  Convert to Montgomery form
      A_Mont := To_Mont (A);
      B_Mont := To_Mont (B);

      Put_Line ("To_Mont(12345) = " & Field_Element'Image (A_Mont));
      Put_Line ("To_Mont(67890) = " & Field_Element'Image (B_Mont));

      --  Montgomery multiplication
      Prod_Mont := Mont_Mul (A_Mont, B_Mont);
      Put_Line ("Mont_Mul(A_Mont, B_Mont) = " & Field_Element'Image (Prod_Mont));

      --  Convert back and verify
      Prod_Normal := Field_Element (From_Mont (Prod_Mont));
      declare
         Expected : constant Valid_Field := Mul (A, B);
      begin
         Put_Line ("From_Mont(Product) = " & Field_Element'Image (Prod_Normal));
         Put_Line ("Expected (A*B mod Q) = " & Valid_Field'Image (Expected));

         if Valid_Field (Prod_Normal) = Expected then
            Put_Line ("  Verified: Montgomery multiplication correct ✓");
         else
            Put_Line ("  ERROR: Montgomery result mismatch!");
         end if;
      end;

      New_Line;
   end Test_Montgomery_Arithmetic;

   procedure Test_Conversions is
      Val32 : constant Anubis_Field.Word32 := 1234567;
      Val64 : constant Unsigned_64 := 9876543210;
      F32, F64 : Valid_Field;
   begin
      Put_Line ("=== Type Conversions ===");

      F32 := From_Word32 (Val32);
      Put_Line ("From_Word32(1234567) = " & Valid_Field'Image (F32));

      F64 := From_U64 (Val64);
      Put_Line ("From_U64(9876543210) = " & Valid_Field'Image (F64));

      declare
         Back32 : constant Anubis_Field.Word32 := To_Word32 (F32);
         Back64 : constant Unsigned_64 := To_U64 (F64);
      begin
         Put_Line ("Round-trip Word32: " &
                   Boolean'Image (Back32 = Val32) & " ✓");
         Put_Line ("Round-trip U64: " &
                   Boolean'Image (Back64 = F64) & " (reduced)");
      end;

      New_Line;
   end Test_Conversions;

   procedure Test_Field_Properties is
      A : constant Valid_Field := 999;
      B : constant Valid_Field := 777;
   begin
      Put_Line ("=== Field Properties ===");

      --  Commutativity: A + B = B + A
      declare
         Sum1 : constant Valid_Field := Add (A, B);
         Sum2 : constant Valid_Field := Add (B, A);
      begin
         Put ("Commutativity (addition): ");
         if Sum1 = Sum2 then
            Put_Line ("PASS ✓");
         else
            Put_Line ("FAIL");
         end if;
      end;

      --  Associativity: (A + B) + C = A + (B + C)
      declare
         C : constant Valid_Field := 555;
         Sum1 : constant Valid_Field := Add (Add (A, B), C);
         Sum2 : constant Valid_Field := Add (A, Add (B, C));
      begin
         Put ("Associativity (addition): ");
         if Sum1 = Sum2 then
            Put_Line ("PASS ✓");
         else
            Put_Line ("FAIL");
         end if;
      end;

      --  Multiplicative identity: A * 1 = A
      declare
         Prod : constant Valid_Field := Mul (A, One);
      begin
         Put ("Multiplicative identity: ");
         if Prod = A then
            Put_Line ("PASS ✓");
         else
            Put_Line ("FAIL");
         end if;
      end;

      --  Additive identity: A + 0 = A
      declare
         Sum : constant Valid_Field := Add (A, Zero);
      begin
         Put ("Additive identity: ");
         if Sum = A then
            Put_Line ("PASS ✓");
         else
            Put_Line ("FAIL");
         end if;
      end;

      --  Additive inverse: A + (-A) = 0
      declare
         Sum : constant Valid_Field := Add (A, Neg (A));
      begin
         Put ("Additive inverse: ");
         if Sum = Zero then
            Put_Line ("PASS ✓");
         else
            Put_Line ("FAIL");
         end if;
      end;

      --  Multiplicative inverse: A * A^(-1) = 1
      declare
         Prod : constant Valid_Field := Mul (A, Inv (A));
      begin
         Put ("Multiplicative inverse: ");
         if Prod = One then
            Put_Line ("PASS ✓");
         else
            Put_Line ("FAIL");
         end if;
      end;

      --  Distributivity: A * (B + C) = A*B + A*C
      declare
         C : constant Valid_Field := 333;
         Left : constant Valid_Field := Mul (A, Add (B, C));
         Right : constant Valid_Field := Add (Mul (A, B), Mul (A, C));
      begin
         Put ("Distributivity: ");
         if Left = Right then
            Put_Line ("PASS ✓");
         else
            Put_Line ("FAIL");
         end if;
      end;

      New_Line;
   end Test_Field_Properties;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  AnubisVM Field Arithmetic Test Suite                     ║");
   Put_Line ("║  Prime Field Z_8380417 for SCARAB Proofs                  ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("Field modulus Q = " & Natural'Image (Q));
   Put_Line ("Field element size: 32 bits");
   New_Line;

   Test_Basic_Arithmetic;
   Test_Exponentiation;
   Test_Polynomial_Operations;
   Test_Lagrange_Interpolation;
   Test_Montgomery_Arithmetic;
   Test_Conversions;
   Test_Field_Properties;

   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  All tests completed                                       ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");

end Test_Field_Arithmetic;
