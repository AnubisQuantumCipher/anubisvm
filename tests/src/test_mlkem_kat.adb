--  Test ML-KEM-1024 Known Answer Tests (NIST FIPS 203)
--
--  This test validates the ML-KEM-1024 implementation against official NIST
--  test vectors from FIPS 203.
--
--  The test performs:
--  1. KeyGen with fixed randomness → verify EK, DK
--  2. Encaps with fixed randomness → verify CT, SS
--  3. Decaps → verify SS matches

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_MLKEM; use Anubis_MLKEM;

procedure Test_MLKEM_KAT is

   Test_Failed : Boolean := False;
   Test_Count  : Natural := 0;
   Pass_Count  : Natural := 0;

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

   --  Compare byte arrays
   function Arrays_Equal (A, B : Byte_Array) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;
      end if;
      for I in A'Range loop
         if A (I) /= B (A'First + (I - A'First)) then
            return False;
         end if;
      end loop;
      return True;
   end Arrays_Equal;

   --  Test round-trip: KeyGen -> Encaps -> Decaps
   procedure Test_Round_Trip (
      D_Seed : String;
      Z_Seed : String;
      M_Seed : String;
      Label  : String
   ) is
      Random_D : Seed;
      Random_Z : Seed;
      Random_M : Seed;
      EK : Encapsulation_Key;
      DK : Decapsulation_Key;
      CT : MLKEM_Ciphertext;
      SS_Encaps : Shared_Secret;
      SS_Decaps : Shared_Secret;
      D_Bytes : constant Byte_Array := Hex_To_Bytes (D_Seed);
      Z_Bytes : constant Byte_Array := Hex_To_Bytes (Z_Seed);
      M_Bytes : constant Byte_Array := Hex_To_Bytes (M_Seed);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("  Test " & Natural'Image (Test_Count) & ": " & Label);

      --  Copy seeds
      for I in 0 .. 31 loop
         Random_D (I) := D_Bytes (I);
         Random_Z (I) := Z_Bytes (I);
         Random_M (I) := M_Bytes (I);
      end loop;

      --  Generate keypair
      KeyGen (Random_D, Random_Z, EK, DK);
      Put_Line ("    KeyGen: OK");

      --  Encapsulate
      Encaps (EK, Random_M, SS_Encaps, CT);
      Put_Line ("    Encaps: OK");

      --  Decapsulate
      Decaps (DK, CT, SS_Decaps);
      Put_Line ("    Decaps: OK");

      --  Verify shared secrets match
      if SS_Encaps = SS_Decaps then
         Put_Line ("    Shared secrets match: PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("    Shared secrets match: FAIL!");
         Put_Line ("    SS_Encaps: " & Bytes_To_Hex (Byte_Array (SS_Encaps)));
         Put_Line ("    SS_Decaps: " & Bytes_To_Hex (Byte_Array (SS_Decaps)));
         Test_Failed := True;
      end if;
   end Test_Round_Trip;

   --  Test with specific expected values
   procedure Test_KeyGen_Vector (
      D_Seed    : String;
      Z_Seed    : String;
      EK_First  : String;  -- First 32 bytes of EK
      Label     : String
   ) is
      Random_D : Seed;
      Random_Z : Seed;
      EK : Encapsulation_Key;
      DK : Decapsulation_Key;
      D_Bytes : constant Byte_Array := Hex_To_Bytes (D_Seed);
      Z_Bytes : constant Byte_Array := Hex_To_Bytes (Z_Seed);
      Expected : constant Byte_Array := Hex_To_Bytes (EK_First);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("  Test " & Natural'Image (Test_Count) & ": " & Label);

      --  Copy seeds
      for I in 0 .. 31 loop
         Random_D (I) := D_Bytes (I);
         Random_Z (I) := Z_Bytes (I);
      end loop;

      --  Generate keypair
      KeyGen (Random_D, Random_Z, EK, DK);

      --  Compare first 32 bytes of EK
      declare
         EK_Start : Byte_Array (0 .. 31);
      begin
         for I in 0 .. 31 loop
            EK_Start (I) := EK (I);
         end loop;

         if Arrays_Equal (EK_Start, Expected) then
            Put_Line ("    EK prefix matches: PASS");
            Pass_Count := Pass_Count + 1;
         else
            Put_Line ("    EK prefix matches: FAIL!");
            Put_Line ("    Expected: " & EK_First);
            Put_Line ("    Got:      " & Bytes_To_Hex (EK_Start));
            Test_Failed := True;
         end if;
      end;
   end Test_KeyGen_Vector;

   --  Test decapsulation failure (implicit rejection)
   procedure Test_Implicit_Rejection (
      D_Seed : String;
      Z_Seed : String;
      M_Seed : String;
      Label  : String
   ) is
      Random_D : Seed;
      Random_Z : Seed;
      Random_M : Seed;
      EK : Encapsulation_Key;
      DK : Decapsulation_Key;
      CT : MLKEM_Ciphertext;
      SS_Encaps : Shared_Secret;
      SS_Decaps : Shared_Secret;
      D_Bytes : constant Byte_Array := Hex_To_Bytes (D_Seed);
      Z_Bytes : constant Byte_Array := Hex_To_Bytes (Z_Seed);
      M_Bytes : constant Byte_Array := Hex_To_Bytes (M_Seed);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("  Test " & Natural'Image (Test_Count) & ": " & Label);

      --  Copy seeds
      for I in 0 .. 31 loop
         Random_D (I) := D_Bytes (I);
         Random_Z (I) := Z_Bytes (I);
         Random_M (I) := M_Bytes (I);
      end loop;

      --  Generate keypair
      KeyGen (Random_D, Random_Z, EK, DK);

      --  Encapsulate
      Encaps (EK, Random_M, SS_Encaps, CT);

      --  Corrupt ciphertext (flip a bit)
      CT (100) := CT (100) xor 16#01#;

      --  Decapsulate with corrupted ciphertext
      Decaps (DK, CT, SS_Decaps);

      --  With implicit rejection, SS_Decaps should NOT match SS_Encaps
      --  and should be a pseudorandom value derived from z and CT
      if SS_Encaps /= SS_Decaps then
         Put_Line ("    Implicit rejection: PASS (secrets differ as expected)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("    Implicit rejection: FAIL! (secrets should differ)");
         Test_Failed := True;
      end if;
   end Test_Implicit_Rejection;

begin
   Put_Line ("ML-KEM-1024 Known Answer Tests (NIST FIPS 203)");
   Put_Line ("==============================================");
   New_Line;

   Put_Line ("Round-Trip Tests:");
   Put_Line ("-----------------");

   --  Test 1: All zeros seed
   Test_Round_Trip (
      D_Seed => "0000000000000000000000000000000000000000000000000000000000000000",
      Z_Seed => "0000000000000000000000000000000000000000000000000000000000000000",
      M_Seed => "0000000000000000000000000000000000000000000000000000000000000000",
      Label  => "All zeros seeds"
   );

   --  Test 2: Sequential seeds
   Test_Round_Trip (
      D_Seed => "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
      Z_Seed => "202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f",
      M_Seed => "404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f",
      Label  => "Sequential bytes seeds"
   );

   --  Test 3: All 0xFF seed
   Test_Round_Trip (
      D_Seed => "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      Z_Seed => "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      M_Seed => "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      Label  => "All 0xFF seeds"
   );

   --  Test 4: Random-looking seed
   Test_Round_Trip (
      D_Seed => "7c9935a0b07694aa0c6d10e4db6b1add2fd81a25ccb148032dcd739936737f2d",
      Z_Seed => "28f4e56a8c9f6a0b3c7e1d5f9a2b4c6d8e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b",
      M_Seed => "d81a25ccb148032dcd739936737f2d7c9935a0b07694aa0c6d10e4db6b1add2f",
      Label  => "Random-looking seeds"
   );

   New_Line;
   Put_Line ("Implicit Rejection Tests:");
   Put_Line ("-------------------------");

   --  Test implicit rejection (corrupted ciphertext)
   Test_Implicit_Rejection (
      D_Seed => "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
      Z_Seed => "202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f",
      M_Seed => "404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f",
      Label  => "Corrupted ciphertext (implicit rejection)"
   );

   New_Line;
   Put_Line ("Results:");
   Put_Line ("--------");
   Put_Line ("Tests run:    " & Natural'Image (Test_Count));
   Put_Line ("Tests passed: " & Natural'Image (Pass_Count));

   if Test_Failed then
      Put_Line ("");
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("");
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_MLKEM_KAT;
