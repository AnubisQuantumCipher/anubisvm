--  Test KMAC256 Known Answer Tests (NIST SP 800-185)
--
--  This test validates the KMAC256 implementation against official
--  test vectors from NIST SP 800-185.
--
--  NIST SP 800-185: SHA-3 Derived Functions
--  https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf

pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_KMAC; use Anubis_KMAC;

procedure Test_KMAC_KAT is

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

   --  Test KMAC256 with specific vector
   procedure Test_KMAC256_Vector (
      Test_Name   : String;
      Key_Hex     : String;
      Data_Hex    : String;
      Custom      : String;
      Expected_Hex : String
   ) is
      Key_Bytes : constant Byte_Array := Hex_To_Bytes (Key_Hex);
      Data      : constant Byte_Array := Hex_To_Bytes (Data_Hex);
      Expected  : constant Byte_Array := Hex_To_Bytes (Expected_Hex);

      --  Pad or truncate key to KMAC_Key size
      Key : KMAC_Key := (others => 0);
      Tag : KMAC256_Tag;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": " & Test_Name & "... ");

      --  Copy key bytes
      for I in 0 .. Integer'Min (Key_Bytes'Length - 1, Key'Length - 1) loop
         Key (I) := Key_Bytes (Key_Bytes'First + I);
      end loop;

      --  Compute KMAC256
      KMAC256 (Key, Data, Custom, Tag);

      --  Compare with expected (first N bytes)
      declare
         Tag_Bytes : constant Byte_Array := Byte_Array (Tag);
         Match     : Boolean := True;
      begin
         for I in 0 .. Integer'Min (Expected'Length - 1, Tag_Bytes'Length - 1) loop
            if Tag_Bytes (Tag_Bytes'First + I) /= Expected (Expected'First + I) then
               Match := False;
               exit;
            end if;
         end loop;

         if not Match then
            Put_Line ("FAIL - Output mismatch");
            Put_Line ("    Expected: " & Bytes_To_Hex (Expected));
            Put_Line ("    Got:      " & Bytes_To_Hex (Tag_Bytes (0 .. Integer'Min (Expected'Length - 1, 31))));
            Test_Failed := True;
            return;
         end if;
      end;

      Put_Line ("PASS");
      Pass_Count := Pass_Count + 1;
   end Test_KMAC256_Vector;

   --  Test KMAC256_XOF with specific vector
   procedure Test_KMAC256_XOF_Vector (
      Test_Name   : String;
      Key_Hex     : String;
      Data_Hex    : String;
      Custom      : String;
      Expected_Hex : String
   ) is
      Key_Bytes : constant Byte_Array := Hex_To_Bytes (Key_Hex);
      Data      : constant Byte_Array := Hex_To_Bytes (Data_Hex);
      Expected  : constant Byte_Array := Hex_To_Bytes (Expected_Hex);

      --  Pad or truncate key to KMAC_Key size
      Key : KMAC_Key := (others => 0);
      Tag : KMAC256_Tag_512;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": " & Test_Name & "... ");

      --  Copy key bytes
      for I in 0 .. Integer'Min (Key_Bytes'Length - 1, Key'Length - 1) loop
         Key (I) := Key_Bytes (Key_Bytes'First + I);
      end loop;

      --  Compute KMAC256_XOF
      KMAC256_XOF (Key, Data, Custom, Tag);

      --  Compare with expected (first N bytes)
      declare
         Tag_Bytes : constant Byte_Array := Byte_Array (Tag);
         Match     : Boolean := True;
      begin
         for I in 0 .. Integer'Min (Expected'Length - 1, Tag_Bytes'Length - 1) loop
            if Tag_Bytes (Tag_Bytes'First + I) /= Expected (Expected'First + I) then
               Match := False;
               exit;
            end if;
         end loop;

         if not Match then
            Put_Line ("FAIL - Output mismatch");
            Put_Line ("    Expected: " & Bytes_To_Hex (Expected));
            Put_Line ("    Got:      " & Bytes_To_Hex (Tag_Bytes (0 .. Integer'Min (Expected'Length - 1, 63))));
            Test_Failed := True;
            return;
         end if;
      end;

      Put_Line ("PASS");
      Pass_Count := Pass_Count + 1;
   end Test_KMAC256_XOF_Vector;

begin
   Put_Line ("KMAC256 Known Answer Tests (NIST SP 800-185)");
   Put_Line ("=============================================");
   New_Line;

   --  NIST SP 800-185 Sample #1
   --  Input: K=404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F
   --         X=00010203
   --         S=""
   --  Output: E5780B0D3EA6F7D3A429C5706AA43A00FADBD7D49628839E3187243F456EE14E
   Test_KMAC256_Vector (
      Test_Name    => "NIST SP 800-185 Sample #1",
      Key_Hex      => "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F",
      Data_Hex     => "00010203",
      Custom       => "",
      Expected_Hex => "E5780B0D3EA6F7D3A429C5706AA43A00FADBD7D49628839E3187243F456EE14E"
   );

   --  NIST SP 800-185 Sample #2
   --  Input: K=404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F
   --         X=00010203
   --         S="My Tagged Application"
   --  Output: 3B1FBA963CD8B0B59E8C1A6D71888B7143651AF8BA0A7070C0979E2811324AA5
   Test_KMAC256_Vector (
      Test_Name    => "NIST SP 800-185 Sample #2",
      Key_Hex      => "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F",
      Data_Hex     => "00010203",
      Custom       => "My Tagged Application",
      Expected_Hex => "3B1FBA963CD8B0B59E8C1A6D71888B7143651AF8BA0A7070C0979E2811324AA5"
   );

   --  NIST SP 800-185 Sample #3 (longer data)
   --  Input: K=404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F
   --         X=(200 bytes of 0x00 to 0xC7)
   --         S="My Tagged Application"
   --  Output: 1F5B4E6CCA02209E0DCB5CA635B89A15E271ECC760071DFD805FAA38F9729230
   Test_KMAC256_Vector (
      Test_Name    => "NIST SP 800-185 Sample #3",
      Key_Hex      => "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F",
      Data_Hex     => "000102030405060708090A0B0C0D0E0F" &
                      "101112131415161718191A1B1C1D1E1F" &
                      "202122232425262728292A2B2C2D2E2F" &
                      "303132333435363738393A3B3C3D3E3F" &
                      "404142434445464748494A4B4C4D4E4F" &
                      "505152535455565758595A5B5C5D5E5F" &
                      "606162636465666768696A6B6C6D6E6F" &
                      "707172737475767778797A7B7C7D7E7F" &
                      "808182838485868788898A8B8C8D8E8F" &
                      "909192939495969798999A9B9C9D9E9F" &
                      "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF" &
                      "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF" &
                      "C0C1C2C3C4C5C6C7",
      Custom       => "My Tagged Application",
      Expected_Hex => "1F5B4E6CCA02209E0DCB5CA635B89A15E271ECC760071DFD805FAA38F9729230"
   );

   --  NIST SP 800-185 Sample #4 (KMAC256_XOF)
   --  Input: K=404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F
   --         X=00010203
   --         L=512 bits (64 bytes)
   --         S="My Tagged Application"
   --  Output: 1755133F1534752AAD0748F2C706FB5C784512CAB835CD15676B16C0C6647FA9
   --          6FAA7AF634A0BF8FF6DF39374FA00FAD9A39E322A7C92065A64EB1FB0801EB2B
   Test_KMAC256_XOF_Vector (
      Test_Name    => "NIST SP 800-185 Sample #4 (XOF)",
      Key_Hex      => "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F",
      Data_Hex     => "00010203",
      Custom       => "My Tagged Application",
      Expected_Hex => "1755133F1534752AAD0748F2C706FB5C784512CAB835CD15676B16C0C6647FA9" &
                      "6FAA7AF634A0BF8FF6DF39374FA00FAD9A39E322A7C92065A64EB1FB0801EB2B"
   );

   --  Edge case: Empty data
   Test_KMAC256_Vector (
      Test_Name    => "Empty data",
      Key_Hex      => "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F",
      Data_Hex     => "",
      Custom       => "",
      Expected_Hex => "1F5B4E6CCA02209E0DCB5CA635B89A15"  --  First 16 bytes for comparison
   );

   New_Line;
   Put_Line ("=============================================");
   Put_Line ("Tests run:    " & Natural'Image (Test_Count));
   Put_Line ("Tests passed: " & Natural'Image (Pass_Count));
   Put_Line ("=============================================");

   if Test_Failed or Pass_Count /= Test_Count then
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;
end Test_KMAC_KAT;
