--  Debug test for ML-KEM-1024 KeyGen

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_MLKEM; use Anubis_MLKEM;

procedure Test_MLKEM_Debug is

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

   --  Convert byte array to hex string
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

   --  Test data from NIST KAT
   D_Hex : constant String := "6dbbc4375136df3b07f7c70e639e223e177e7fd53b161b3f4d57791794f12624";
   D : constant Byte_Array := Hex_To_Bytes (D_Hex);

   --  G hash output
   G_Output : SHA3_512_Digest;
   Rho : Byte_Array (0 .. 31);
   Sigma : Byte_Array (0 .. 31);

begin
   Put_Line ("ML-KEM-1024 KeyGen Debug");
   Put_Line ("========================");
   New_Line;

   Put_Line ("Input d seed: " & D_Hex);
   New_Line;

   --  Step 1: G(d) = SHA3-512(d)
   SHA3_512 (D, G_Output);

   --  Extract rho and sigma
   for I in 0 .. 31 loop
      Rho (I) := G_Output (I);
      Sigma (I) := G_Output (32 + I);
   end loop;

   Put_Line ("Step 1: G(d) = SHA3-512(d)");
   Put_Line ("  rho   = " & Bytes_To_Hex (Rho));
   Put_Line ("  sigma = " & Bytes_To_Hex (Sigma));
   New_Line;

   --  Expected from Python:
   --  rho   = 09488e697b6f4a41d755f36acd4f57f9b543de0da8a597eefe1c37aecaffce62
   --  sigma = 02a25d76bd26b14ccca2007cff4a672711afcf0f9d496076918c78f6b68d7c5b

   Put_Line ("Expected (Python):");
   Put_Line ("  rho   = 09488e697b6f4a41d755f36acd4f57f9b543de0da8a597eefe1c37aecaffce62");
   Put_Line ("  sigma = 02a25d76bd26b14ccca2007cff4a672711afcf0f9d496076918c78f6b68d7c5b");
   New_Line;

   if Bytes_To_Hex (Rho) = "09488e697b6f4a41d755f36acd4f57f9b543de0da8a597eefe1c37aecaffce62" then
      Put_Line ("rho: MATCH");
   else
      Put_Line ("rho: MISMATCH!");
   end if;

   if Bytes_To_Hex (Sigma) = "02a25d76bd26b14ccca2007cff4a672711afcf0f9d496076918c78f6b68d7c5b" then
      Put_Line ("sigma: MATCH");
   else
      Put_Line ("sigma: MISMATCH!");
   end if;

   New_Line;
   Put_Line ("Step 2: SHAKE128 for A[0,0]");

   declare
      XOF_Input : Byte_Array (0 .. 33);
      XOF_Output : Byte_Array (0 .. 503);
   begin
      --  Build XOF input: rho || j=0 || i=0
      for I in 0 .. 31 loop
         XOF_Input (I) := Rho (I);
      end loop;
      XOF_Input (32) := 0;  -- j
      XOF_Input (33) := 0;  -- i

      Put_Line ("  XOF input: " & Bytes_To_Hex (XOF_Input));

      SHAKE128 (XOF_Input, XOF_Output, 504);

      Put_Line ("  XOF output (first 20 bytes): " & Bytes_To_Hex (XOF_Output (0 .. 19)));

      --  Expected from Python:
      --  XOF output (first 20 bytes): 7ee6a920431c7f43f193227375ecdbc2e418b1ca

      if Bytes_To_Hex (XOF_Output (0 .. 19)) = "7ee6a920431c7f43f193227375ecdbc2e418b1ca" then
         Put_Line ("  XOF output: MATCH");
      else
         Put_Line ("  XOF output: MISMATCH!");
      end if;
   end;

   New_Line;
   Put_Line ("Step 3: SHAKE256 for PRF (sigma, 0)");

   declare
      PRF_Input : Byte_Array (0 .. 32);
      PRF_Output : Byte_Array (0 .. 127);
   begin
      --  Build PRF input: sigma || N=0
      for I in 0 .. 31 loop
         PRF_Input (I) := Sigma (I);
      end loop;
      PRF_Input (32) := 0;

      Put_Line ("  PRF input: " & Bytes_To_Hex (PRF_Input));

      SHAKE256 (PRF_Input, PRF_Output, 128);

      Put_Line ("  PRF output (first 20 bytes): " & Bytes_To_Hex (PRF_Output (0 .. 19)));

      --  Expected from Python:
      --  PRF output (first 20 bytes): 576f65702581eac02176fa7de70fe219520a2665

      if Bytes_To_Hex (PRF_Output (0 .. 19)) = "576f65702581eac02176fa7de70fe219520a2665" then
         Put_Line ("  PRF output: MATCH");
      else
         Put_Line ("  PRF output: MISMATCH!");
      end if;
   end;

   New_Line;
   Put_Line ("Step 4: Full KeyGen test");

   declare
      Random_D : Seed;
      Random_Z : Seed;
      EK : Encapsulation_Key;
      DK : Decapsulation_Key;

      D_Bytes : constant Byte_Array := Hex_To_Bytes (D_Hex);
      Z_Hex : constant String := "f696484048ec21f96cf50a56d0759c448f3779752f0383d37449690694cf7a68";
      Z_Bytes : constant Byte_Array := Hex_To_Bytes (Z_Hex);

      Expected_EK_Start : constant String := "a8122f376b3f5d35";
   begin
      --  Copy seeds
      for I in 0 .. 31 loop
         Random_D (I) := D_Bytes (I);
         Random_Z (I) := Z_Bytes (I);
      end loop;

      Put_Line ("  Random_D: " & Bytes_To_Hex (Byte_Array (Random_D)));
      Put_Line ("  Random_Z: " & Bytes_To_Hex (Byte_Array (Random_Z)));

      KeyGen (Random_D, Random_Z, EK, DK);

      Put_Line ("  EK (first 8 bytes): " & Bytes_To_Hex (Byte_Array (EK (0 .. 7))));
      Put_Line ("  Expected:           " & Expected_EK_Start);

      if Bytes_To_Hex (Byte_Array (EK (0 .. 7))) = Expected_EK_Start then
         Put_Line ("  KeyGen: PASS");
      else
         Put_Line ("  KeyGen: FAIL");
      end if;
   end;

end Test_MLKEM_Debug;
