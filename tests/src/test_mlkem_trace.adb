--  Trace test for ML-KEM-1024 KeyGen - comparing intermediate values

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_Field; use Anubis_Field;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_MLKEM_Sample; use Anubis_MLKEM_Sample;
with Anubis_MLKEM_Poly; use Anubis_MLKEM_Poly;

procedure Test_MLKEM_Trace is

   --  Hex to byte conversion
   function Hex_To_Byte (C1, C2 : Character) return Byte is
      function Hex_Digit (C : Character) return Byte is
      begin
         case C is
            when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
            when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
            when others => raise Constraint_Error;
         end case;
      end Hex_Digit;
   begin
      return Shift_Left (Hex_Digit (C1), 4) or Hex_Digit (C2);
   end Hex_To_Byte;

   --  Convert hex string to byte array
   function Hex_To_Bytes (Hex : String) return Byte_Array is
      Result : Byte_Array (0 .. Hex'Length / 2 - 1);
   begin
      for I in Result'Range loop
         Result (I) := Hex_To_Byte (
            Hex (Hex'First + I * 2),
            Hex (Hex'First + I * 2 + 1)
         );
      end loop;
      return Result;
   end Hex_To_Bytes;

   --  Test data from NIST KAT
   D_Hex : constant String := "6dbbc4375136df3b07f7c70e639e223e177e7fd53b161b3f4d57791794f12624";
   D : constant Byte_Array := Hex_To_Bytes (D_Hex);

   --  G hash output
   G_Output : SHA3_512_Digest;
   Rho : Seed;
   Sigma : Seed;

   --  Matrix and vectors
   A : Poly_Matrix;
   S : Poly_Vector;
   E : Poly_Vector;

begin
   Put_Line ("ML-KEM-1024 KeyGen Trace");
   Put_Line ("========================");
   New_Line;

   --  Step 1: G(d) = SHA3-512(d)
   SHA3_512 (D, G_Output);

   --  Extract rho and sigma
   for I in 0 .. 31 loop
      Rho (I) := G_Output (I);
      Sigma (I) := G_Output (32 + I);
   end loop;

   Put_Line ("rho/sigma extracted from G(d)");
   New_Line;

   --  Step 2: Generate matrix A
   Put_Line ("Step 2: Generate matrix A from rho");
   Generate_Matrix (Rho, A);

   Put_Line ("A[0,0] coefficients (first 20):");
   for I in 0 .. 19 loop
      Put (Field_Element'Image (A (0, 0) (I)));
   end loop;
   New_Line;
   Put_Line ("Expected (Python): 1662 2718 800 452 895 659 1842 3189 1218 398 3072 2028 2002 1590 2893 2519 3148 2619 2279 2200");
   New_Line;

   Put_Line ("A[0,0] coefficients (last 20):");
   for I in 236 .. 255 loop
      Put (Field_Element'Image (A (0, 0) (I)));
   end loop;
   New_Line;
   New_Line;

   --  Step 3: Sample secret vector s
   Put_Line ("Step 3: Sample secret vector s using CBD_2");
   Sample_Vector_CBD (Sigma, 0, S);

   Put_Line ("s[0] coefficients (first 20):");
   for I in 0 .. 19 loop
      Put (Field_Element'Image (S (0) (I)));
   end loop;
   New_Line;
   Put_Line ("Expected (Python): 1 0 0 0 0 0 0 1 0 1 1 3328 0 3328 0 3327 0 1 3328 3328");
   New_Line;

   --  Step 4: Sample error vector e
   Put_Line ("Step 4: Sample error vector e using CBD_2");
   Sample_Vector_CBD (Sigma, K, E);

   Put_Line ("e[0] coefficients (first 20):");
   for I in 0 .. 19 loop
      Put (Field_Element'Image (E (0) (I)));
   end loop;
   New_Line;
   Put_Line ("Expected (Python): 2 3327 0 1 0 1 0 0 1 3328 0 0 1 3327 1 0 3328 3327 1 0");
   New_Line;

   --  Step 5: Apply NTT to s
   Put_Line ("Step 5: Apply NTT to s[0]");
   declare
      S_Copy : Polynomial := S (0);
   begin
      Poly_NTT (S_Copy);
      Put_Line ("NTT(s[0]) coefficients (first 20):");
      for I in 0 .. 19 loop
         Put (Field_Element'Image (S_Copy (I)));
      end loop;
      New_Line;
      Put_Line ("Expected (Python): 660 2124 1384 1920 174 2028 3011 3090 1093 2858 656 782 3208 2023 424 2313 2813 1216 1296 2117");
   end;
   New_Line;

   --  Step 6: Apply NTT to e
   Put_Line ("Step 6: Apply NTT to e[0]");
   declare
      E_Copy : Polynomial := E (0);
   begin
      Poly_NTT (E_Copy);
      Put_Line ("NTT(e[0]) coefficients (first 20):");
      for I in 0 .. 19 loop
         Put (Field_Element'Image (E_Copy (I)));
      end loop;
      New_Line;
      Put_Line ("Expected (Python): 2817 2012 2932 1508 2816 2019 3047 1575 2889 1960 2915 1539 2836 2034 3033 1500 2827 2046 3031 1475");
   end;
   New_Line;

   Put_Line ("Done.");

end Test_MLKEM_Trace;
