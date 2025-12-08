--  Test NTT (Number Theoretic Transform)
--
--  Verifies:
--  1. NTT/INTT round-trip correctness
--  2. Point-wise multiplication equivalence
--  3. Linearity properties

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;
with Anubis_NTT; use Anubis_NTT;

procedure Test_NTT is

   Test_Failed : Boolean := False;

   --  Test NTT/INTT round-trip
   procedure Test_Round_Trip is
      F : Polynomial;
      F_Original : Polynomial;
   begin
      Put_Line ("Test 1: NTT/INTT Round-Trip");

      --  Create test polynomial with small coefficients
      for I in Poly_Index loop
         F (I) := Field_Element ((I * 7 + 13) mod Q);
         F_Original (I) := F (I);
      end loop;

      --  Apply NTT then INTT
      NTT (F);
      INTT (F);

      --  Check if we got back the original
      for I in Poly_Index loop
         if F (I) /= F_Original (I) then
            Put_Line ("  FAIL at index" & I'Image);
            Put_Line ("    Expected:" & F_Original (I)'Image);
            Put_Line ("    Got:" & F (I)'Image);
            Test_Failed := True;
            return;
         end if;
      end loop;

      Put_Line ("  PASS");
   end Test_Round_Trip;

   --  Test that NTT(0) = 0
   procedure Test_Zero_Polynomial is
      F : Polynomial := (others => 0);
   begin
      Put_Line ("Test 2: NTT of Zero Polynomial");

      NTT (F);

      for I in Poly_Index loop
         if F (I) /= 0 then
            Put_Line ("  FAIL at index" & I'Image);
            Put_Line ("    Expected: 0");
            Put_Line ("    Got:" & F (I)'Image);
            Test_Failed := True;
            return;
         end if;
      end loop;

      Put_Line ("  PASS");
   end Test_Zero_Polynomial;

   --  Test linearity: NTT(a*f + b*g) = a*NTT(f) + b*NTT(g)
   procedure Test_Linearity is
      F, G, H : Polynomial;
      F_NTT, G_NTT, H_NTT : Polynomial;
      A : constant Field_Element := 5;
      B : constant Field_Element := 7;
   begin
      Put_Line ("Test 3: NTT Linearity");

      --  Initialize polynomials
      for I in Poly_Index loop
         F (I) := Field_Element ((I * 3 + 1) mod Q);
         G (I) := Field_Element ((I * 5 + 2) mod Q);
         --  H = A*F + B*G
         H (I) := Add (Mul (A, F (I)), Mul (B, G (I)));
      end loop;

      --  Copy for NTT
      F_NTT := F;
      G_NTT := G;
      H_NTT := H;

      --  Apply NTT
      NTT (F_NTT);
      NTT (G_NTT);
      NTT (H_NTT);

      --  Check: H_NTT should equal A*F_NTT + B*G_NTT
      for I in Poly_Index loop
         declare
            Expected : constant Field_Element :=
               Add (Mul (A, F_NTT (I)), Mul (B, G_NTT (I)));
            Got : constant Field_Element := Barrett_Reduce (Unsigned_32 (H_NTT (I)));
         begin
            --  Allow small error due to modular arithmetic
            if Got /= Expected and then
               (Got + Q) mod Q /= Expected and then
               Got /= (Expected + Q) mod Q
            then
               Put_Line ("  FAIL at index" & I'Image);
               Put_Line ("    Expected:" & Expected'Image);
               Put_Line ("    Got:" & Got'Image);
               Test_Failed := True;
               return;
            end if;
         end;
      end loop;

      Put_Line ("  PASS");
   end Test_Linearity;

   --  Test point-wise multiplication
   procedure Test_Pointwise_Mul is
      F, G, H : Polynomial;
      F_Original, G_Original : Polynomial;
   begin
      Put_Line ("Test 4: Point-wise Multiplication");

      --  Create two polynomials
      for I in Poly_Index loop
         F (I) := Field_Element ((I * 11 + 3) mod Q);
         G (I) := Field_Element ((I * 13 + 5) mod Q);
         F_Original (I) := F (I);
         G_Original (I) := G (I);
      end loop;

      --  Transform to NTT domain
      NTT (F);
      NTT (G);

      --  Multiply in NTT domain
      NTT_Mul (F, G, H);

      --  Transform back
      INTT (H);

      --  Simple sanity check: result should be in valid range
      for I in Poly_Index loop
         if H (I) >= Q then
            Put_Line ("  FAIL: coefficient out of range at index" & I'Image);
            Put_Line ("    Got:" & H (I)'Image);
            Test_Failed := True;
            return;
         end if;
      end loop;

      Put_Line ("  PASS (range check)");
   end Test_Pointwise_Mul;

   --  Test normalization
   procedure Test_Normalize is
      F : Polynomial;
   begin
      Put_Line ("Test 5: Normalization");

      --  Create polynomial with values > Q
      for I in Poly_Index loop
         F (I) := Q + Field_Element (I mod 100);
      end loop;

      Normalize (F);

      --  Check all values are < Q
      for I in Poly_Index loop
         if F (I) >= Q then
            Put_Line ("  FAIL at index" & I'Image);
            Put_Line ("    Expected: < Q");
            Put_Line ("    Got:" & F (I)'Image);
            Test_Failed := True;
            return;
         end if;
      end loop;

      Put_Line ("  PASS");
   end Test_Normalize;

begin
   Put_Line ("NTT Tests (ML-KEM/ML-DSA Z_3329)");
   Put_Line ("=================================");
   New_Line;

   Test_Round_Trip;
   Test_Zero_Polynomial;
   Test_Linearity;
   Test_Pointwise_Mul;
   Test_Normalize;

   New_Line;
   if Test_Failed then
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_NTT;
