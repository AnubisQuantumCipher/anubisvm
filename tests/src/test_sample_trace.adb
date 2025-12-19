--  Trace test for Sample_NTT - detailed byte-by-byte analysis

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_Field; use Anubis_Field;
with Anubis_Config; use Anubis_Config;

procedure Test_Sample_Trace is

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

   function Bytes_To_Hex (Bytes : Byte_Array) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Result : String (1 .. Bytes'Length * 2);
      Pos : Natural := 1;
   begin
      for B of Bytes loop
         Result (Pos) := Hex_Chars (Natural (Shift_Right (B, 4)) + 1);
         Result (Pos + 1) := Hex_Chars (Natural (B and 16#0F#) + 1);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Bytes_To_Hex;

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
   Rho : Byte_Array (0 .. 31);

   --  XOF input and output for A[0,0]
   XOF_Input : Byte_Array (0 .. 33);
   XOF_Output : Byte_Array (0 .. 167);  -- 168 bytes = 56 * 3 for first 50+ coeffs

   --  Q = 3329
   Coef_Idx : Natural := 0;
   Buf_Idx : Natural := 0;
   D1, D2 : Unsigned_16;

begin
   Put_Line ("Sample_NTT Detailed Trace");
   Put_Line ("=========================");
   New_Line;

   --  Step 1: G(d) = SHA3-512(d)
   SHA3_512 (D, G_Output);

   --  Extract rho
   for I in 0 .. 31 loop
      Rho (I) := G_Output (I);
   end loop;

   Put_Line ("rho = " & Bytes_To_Hex (Rho));
   New_Line;

   --  Build XOF input for A[0,0]: rho || j=0 || i=0
   for I in 0 .. 31 loop
      XOF_Input (I) := Rho (I);
   end loop;
   XOF_Input (32) := 0;  -- j
   XOF_Input (33) := 0;  -- i

   Put_Line ("XOF input for A[0,0]: " & Bytes_To_Hex (XOF_Input));
   New_Line;

   --  Generate XOF output
   SHAKE128 (XOF_Input, XOF_Output, 168);

   Put_Line ("XOF output (first 60 bytes):");
   Put_Line (Bytes_To_Hex (XOF_Output (0 .. 59)));
   New_Line;

   --  Manually do rejection sampling and trace each step
   Put_Line ("Rejection sampling trace:");
   Put_Line ("  Buf_Idx  Bytes           D1     D2     Accept?    Coef[idx]");
   Put_Line ("  ------  --------------  -----  -----  ---------  ----------");

   Buf_Idx := 0;
   Coef_Idx := 0;

   while Coef_Idx < 25 and Buf_Idx + 2 < 168 loop
      --  Extract two 12-bit values
      D1 := Unsigned_16 (XOF_Output (Buf_Idx)) or
            Shift_Left (Unsigned_16 (XOF_Output (Buf_Idx + 1)) and 16#0F#, 8);
      D2 := Shift_Right (Unsigned_16 (XOF_Output (Buf_Idx + 1)), 4) or
            Shift_Left (Unsigned_16 (XOF_Output (Buf_Idx + 2)), 4);

      Put ("  " & Natural'Image (Buf_Idx));
      Put ("    " & Bytes_To_Hex (XOF_Output (Buf_Idx .. Buf_Idx + 2)));
      Put ("   " & Unsigned_16'Image (D1));
      Put (" " & Unsigned_16'Image (D2));

      Buf_Idx := Buf_Idx + 3;

      --  Accept D1 if < q
      if D1 < Q then
         Put ("   D1 accept ");
         Put ("   coef[" & Natural'Image (Coef_Idx) & "] =" & Unsigned_16'Image (D1));
         Coef_Idx := Coef_Idx + 1;
      else
         Put ("   D1 reject ");
         Put ("             ");
      end if;
      New_Line;

      --  Accept D2 if < q
      if D2 < Q and Coef_Idx < 25 then
         Put ("                            ");
         Put ("   D2 accept ");
         Put ("   coef[" & Natural'Image (Coef_Idx) & "] =" & Unsigned_16'Image (D2));
         Coef_Idx := Coef_Idx + 1;
         New_Line;
      elsif D2 >= Q then
         Put ("                            ");
         Put ("   D2 reject ");
         New_Line;
      end if;
   end loop;

   New_Line;
   Put_Line ("Expected Python coefficients:");
   Put_Line ("  [0..9]:  1662 2718 800 452 895 659 1842 3189 1218 398");
   Put_Line ("  [10..19]: 3072 2028 2002 1590 2893 2519 3148 2619 2279 2200");

end Test_Sample_Trace;
