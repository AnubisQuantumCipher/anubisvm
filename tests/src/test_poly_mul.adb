--  Test polynomial multiplication via NTT

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;
with Anubis_NTT;

procedure Test_Poly_Mul is

   procedure Print_Poly_Sample (Name : String; P : Anubis_NTT.Polynomial) is
   begin
      Put (Name & ": [");
      for I in 0 .. 7 loop
         Put (Field_Element'Image (P (I)));
         if I < 7 then Put (","); end if;
      end loop;
      Put_Line ("...]");
   end Print_Poly_Sample;

   --  Test 1: Multiply by 1 (identity)
   --  f = 1 (constant polynomial), g = 1
   --  Result should be 1
   procedure Test_Mul_Identity is
      F, G, H : Anubis_NTT.Polynomial;
   begin
      Put_Line ("Test 1: Multiply 1 * 1");

      F := (0 => 1, others => 0);
      G := (0 => 1, others => 0);

      Print_Poly_Sample ("  F (before NTT)", F);
      Anubis_NTT.NTT (F);
      Print_Poly_Sample ("  F (after NTT)", F);

      Anubis_NTT.NTT (G);
      Print_Poly_Sample ("  G (after NTT)", G);

      Anubis_NTT.NTT_Mul (F, G, H);
      Print_Poly_Sample ("  H = F*G (NTT domain)", H);

      Anubis_NTT.INTT (H);
      Print_Poly_Sample ("  H (after INTT)", H);

      if H (0) = 1 and (for all I in 1 .. 255 => H (I) = 0) then
         Put_Line ("  PASS: 1*1 = 1");
      else
         Put_Line ("  FAIL: Expected [1, 0, 0, ...]");
      end if;
      New_Line;
   end Test_Mul_Identity;

   --  Test 2: Multiply by X
   --  f = X (coefficient 1 at index 1), g = 1
   --  Result should be X
   procedure Test_Mul_By_X is
      F, G, H : Anubis_NTT.Polynomial;
   begin
      Put_Line ("Test 2: Multiply X * 1");

      F := (1 => 1, others => 0);  -- f = X
      G := (0 => 1, others => 0);  -- g = 1

      Anubis_NTT.NTT (F);
      Anubis_NTT.NTT (G);
      Anubis_NTT.NTT_Mul (F, G, H);
      Anubis_NTT.INTT (H);

      Print_Poly_Sample ("  H = X*1", H);

      if H (1) = 1 and H (0) = 0 and (for all I in 2 .. 255 => H (I) = 0) then
         Put_Line ("  PASS: X*1 = X");
      else
         Put_Line ("  FAIL: Expected [0, 1, 0, 0, ...]");
      end if;
      New_Line;
   end Test_Mul_By_X;

   --  Test 3: X * X = X^2
   procedure Test_X_Squared is
      F, G, H : Anubis_NTT.Polynomial;
   begin
      Put_Line ("Test 3: Multiply X * X");

      F := (1 => 1, others => 0);  -- f = X
      G := (1 => 1, others => 0);  -- g = X

      Anubis_NTT.NTT (F);
      Anubis_NTT.NTT (G);
      Anubis_NTT.NTT_Mul (F, G, H);
      Anubis_NTT.INTT (H);

      Print_Poly_Sample ("  H = X*X = X^2", H);

      if H (2) = 1 and H (0) = 0 and H (1) = 0 and (for all I in 3 .. 255 => H (I) = 0) then
         Put_Line ("  PASS: X*X = X^2");
      else
         Put_Line ("  FAIL: Expected [0, 0, 1, 0, ...]");
      end if;
      New_Line;
   end Test_X_Squared;

   --  Test 4: X^128 * X^128 = X^256 = -1 (mod X^256+1)
   procedure Test_X_256 is
      F, G, H : Anubis_NTT.Polynomial;
   begin
      Put_Line ("Test 4: X^128 * X^128 = X^256 = -1 (mod X^256+1)");

      F := (128 => 1, others => 0);  -- f = X^128
      G := (128 => 1, others => 0);  -- g = X^128

      Anubis_NTT.NTT (F);
      Anubis_NTT.NTT (G);
      Anubis_NTT.NTT_Mul (F, G, H);
      Anubis_NTT.INTT (H);

      Print_Poly_Sample ("  H = X^256 mod (X^256+1)", H);

      --  X^256 = -1 mod (X^256+1), so H(0) should be Q-1 = 3328
      if H (0) = Q - 1 and (for all I in 1 .. 255 => H (I) = 0) then
         Put_Line ("  PASS: X^256 = -1");
      else
         Put_Line ("  FAIL: Expected [3328, 0, 0, ...]");
      end if;
      New_Line;
   end Test_X_256;

   --  Test 5: (1 + X) * (1 - X) = 1 - X^2
   procedure Test_Difference_Of_Squares is
      F, G, H : Anubis_NTT.Polynomial;
   begin
      Put_Line ("Test 5: (1 + X) * (1 - X) = 1 - X^2");

      F := (0 => 1, 1 => 1, others => 0);      -- f = 1 + X
      G := (0 => 1, 1 => Q - 1, others => 0);  -- g = 1 - X

      Anubis_NTT.NTT (F);
      Anubis_NTT.NTT (G);
      Anubis_NTT.NTT_Mul (F, G, H);
      Anubis_NTT.INTT (H);

      Print_Poly_Sample ("  H = 1 - X^2", H);

      --  Expected: 1 at coef 0, -1 (=Q-1) at coef 2
      if H (0) = 1 and H (1) = 0 and H (2) = Q - 1
         and (for all I in 3 .. 255 => H (I) = 0) then
         Put_Line ("  PASS: (1+X)*(1-X) = 1 - X^2");
      else
         Put_Line ("  FAIL: Expected [1, 0, 3328, 0, ...]");
      end if;
      New_Line;
   end Test_Difference_Of_Squares;

begin
   Put_Line ("Polynomial Multiplication Tests");
   Put_Line ("================================");
   Put_Line ("Q = " & Natural'Image (Q));
   New_Line;

   Test_Mul_Identity;
   Test_Mul_By_X;
   Test_X_Squared;
   Test_X_256;
   Test_Difference_Of_Squares;

   Put_Line ("Done.");
end Test_Poly_Mul;
