--  Test SHAKE128/256 Known Answer Tests (NIST Test Vectors)
--
--  This test validates the SHAKE XOF implementation against official NIST test vectors
--  from FIPS 202 and the SHAKE Validation System.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_SHA3; use Anubis_SHA3;

procedure Test_SHAKE_KAT is

   Test_Failed : Boolean := False;

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

   --  Test SHAKE128
   procedure Test_SHAKE128_Vector (
      Msg           : String;
      Output_Length : Positive;
      Expected      : String;
      Label         : String
   ) is
      Message_Bytes : constant Byte_Array :=
         (if Msg'Length = 0 then Byte_Array'(1 .. 0 => 0) else Hex_To_Bytes (Msg));
      Expected_Bytes : constant Byte_Array := Hex_To_Bytes (Expected);
      Output : Byte_Array (0 .. Output_Length - 1);
   begin
      Put_Line ("  Testing SHAKE128: " & Label);
      SHAKE128 (Message_Bytes, Output, Output_Length);

      if Output /= Expected_Bytes then
         Put_Line ("    FAIL!");
         Put_Line ("    Expected: " & Expected);
         Put_Line ("    Got:      " & Bytes_To_Hex (Output));
         Test_Failed := True;
      else
         Put_Line ("    PASS");
      end if;
   end Test_SHAKE128_Vector;

   --  Test SHAKE256
   procedure Test_SHAKE256_Vector (
      Msg           : String;
      Output_Length : Positive;
      Expected      : String;
      Label         : String
   ) is
      Message_Bytes : constant Byte_Array :=
         (if Msg'Length = 0 then Byte_Array'(1 .. 0 => 0) else Hex_To_Bytes (Msg));
      Expected_Bytes : constant Byte_Array := Hex_To_Bytes (Expected);
      Output : Byte_Array (0 .. Output_Length - 1);
   begin
      Put_Line ("  Testing SHAKE256: " & Label);
      SHAKE256 (Message_Bytes, Output, Output_Length);

      if Output /= Expected_Bytes then
         Put_Line ("    FAIL!");
         Put_Line ("    Expected: " & Expected);
         Put_Line ("    Got:      " & Bytes_To_Hex (Output));
         Test_Failed := True;
      else
         Put_Line ("    PASS");
      end if;
   end Test_SHAKE256_Vector;

begin
   Put_Line ("SHAKE128/256 Known Answer Tests (NIST FIPS 202)");
   Put_Line ("===============================================");
   New_Line;

   --  SHAKE128 Test Vectors
   Put_Line ("SHAKE128 Tests:");

   --  Empty message, 256 bits output
   Test_SHAKE128_Vector (
      Msg           => "",
      Output_Length => 32,
      Expected      => "7f9c2ba4e88f827d616045507605853e" &
                       "d73b8093f6efbc88eb1a6eacfa66ef26",
      Label         => "Empty message, 256-bit output"
   );

   --  "abc", 256 bits output
   Test_SHAKE128_Vector (
      Msg           => "616263",
      Output_Length => 32,
      Expected      => "5881092dd818bf5cf8a3ddb793fbcba7" &
                       "4097d5c526a6d35f97b83351940f2cc8",
      Label         => "Message 'abc', 256-bit output"
   );

   --  "abc", 512 bits output
   Test_SHAKE128_Vector (
      Msg           => "616263",
      Output_Length => 64,
      Expected      => "5881092dd818bf5cf8a3ddb793fbcba7" &
                       "4097d5c526a6d35f97b83351940f2cc8" &
                       "44c50af32acd3f2cdd066568706f509b" &
                       "c1bdde58295dae3f891a9a0fca578378",
      Label         => "Message 'abc', 512-bit output"
   );

   New_Line;
   Put_Line ("SHAKE256 Tests:");

   --  Empty message, 512 bits output
   Test_SHAKE256_Vector (
      Msg           => "",
      Output_Length => 64,
      Expected      => "46b9dd2b0ba88d13233b3feb743eeb24" &
                       "3fcd52ea62b81b82b50c27646ed5762f" &
                       "d75dc4ddd8c0f200cb05019d67b592f6" &
                       "fc821c49479ab48640292eacb3b7c4be",
      Label         => "Empty message, 512-bit output"
   );

   --  "abc", 512 bits output
   Test_SHAKE256_Vector (
      Msg           => "616263",
      Output_Length => 64,
      Expected      => "483366601360a8771c6863080cc4114d" &
                       "8db44530f8f1e1ee4f94ea37e78b5739" &
                       "d5a15bef186a5386c75744c0527e1faa" &
                       "9f8726e462a12a4feb06bd8801e751e4",
      Label         => "Message 'abc', 512-bit output"
   );

   --  "abc", 32 bytes output
   Test_SHAKE256_Vector (
      Msg           => "616263",
      Output_Length => 32,
      Expected      => "483366601360a8771c6863080cc4114d" &
                       "8db44530f8f1e1ee4f94ea37e78b5739",
      Label         => "Message 'abc', 256-bit output"
   );

   New_Line;

   --  ML-DSA-87 KAT Seed Expansion Diagnostic
   Put_Line ("ML-DSA-87 Seed Expansion Diagnostic:");
   declare
      KAT_Seed : constant Byte_Array := Hex_To_Bytes (
         "7c9935a0b07694aa0c6d10e4db6b1add2fd81a25ccb148032dcd739936737f2d");
      Expanded : Byte_Array (0 .. 127);
      --  Domain-separated seed (old Dilithium: seed || K || L)
      Seed_With_Domain : Byte_Array (0 .. 33);
   begin
      --  Test 1: FIPS 204 style (raw seed)
      SHAKE256 (KAT_Seed, Expanded, 128);
      Put_Line ("  KAT seed: 7c9935a0b07694aa0c6d10e4db6b1add...");
      New_Line;
      Put_Line ("  Method 1: FIPS 204 - SHAKE256(seed, 128):");
      Put_Line ("    rho [0-31]: " & Bytes_To_Hex (Expanded (0 .. 31)));

      --  Test 2: Old Dilithium style (seed || K || L where K=8, L=7)
      for I in KAT_Seed'Range loop
         Seed_With_Domain (I) := KAT_Seed (I);
      end loop;
      Seed_With_Domain (32) := 8;  --  K for ML-DSA-87
      Seed_With_Domain (33) := 7;  --  L for ML-DSA-87
      SHAKE256 (Seed_With_Domain, Expanded, 128);
      New_Line;
      Put_Line ("  Method 2: Dilithium - SHAKE256(seed || K || L, 128):");
      Put_Line ("    rho [0-31]: " & Bytes_To_Hex (Expanded (0 .. 31)));

      New_Line;
      Put_Line ("  Expected pkey prefix from KAT: 903efbf16cd1f779...");
      if Bytes_To_Hex (Expanded (0 .. 15)) = "903efbf16cd1f779825106f76de12df4" then
         Put_Line ("  ** Method 2 MATCHES! KAT uses old Dilithium domain separation **");
      else
         Put_Line ("  Neither method matches - deeper investigation needed.");
      end if;
   end;

   New_Line;
   if Test_Failed then
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_SHAKE_KAT;
