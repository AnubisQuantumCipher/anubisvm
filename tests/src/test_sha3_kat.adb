--  Test SHA3 Known Answer Tests (NIST Test Vectors)
--
--  This test validates the SHA3 implementation against official NIST test vectors
--  from FIPS 202 and the SHA-3 Validation System (SHAVS).

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_SHA3; use Anubis_SHA3;

procedure Test_SHA3_KAT is

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

   --  Test vector structure
   type Test_Vector_256 is record
      Message : access String;
      Expected : access String;
   end record;

   type Test_Vector_384 is record
      Message : access String;
      Expected : access String;
   end record;

   type Test_Vector_512 is record
      Message : access String;
      Expected : access String;
   end record;

   --  Test SHA3-256 with one vector
   procedure Test_SHA3_256_Vector (Msg : String; Expected : String; Label : String) is
      Message_Bytes : constant Byte_Array := (if Msg'Length = 0 then Byte_Array'(1 .. 0 => 0) else Hex_To_Bytes (Msg));
      Expected_Bytes : constant Byte_Array := Hex_To_Bytes (Expected);
      Digest : SHA3_256_Digest;
   begin
      Put_Line ("  Testing SHA3-256: " & Label);
      SHA3_256 (Message_Bytes, Digest);

      if Digest /= Expected_Bytes then
         Put_Line ("    FAIL!");
         Put_Line ("    Expected: " & Expected);
         Put_Line ("    Got:      " & Bytes_To_Hex (Digest));
         Test_Failed := True;
      else
         Put_Line ("    PASS");
      end if;
   end Test_SHA3_256_Vector;

   --  Test SHA3-384 with one vector
   procedure Test_SHA3_384_Vector (Msg : String; Expected : String; Label : String) is
      Message_Bytes : constant Byte_Array := (if Msg'Length = 0 then Byte_Array'(1 .. 0 => 0) else Hex_To_Bytes (Msg));
      Expected_Bytes : constant Byte_Array := Hex_To_Bytes (Expected);
      Digest : SHA3_384_Digest;
   begin
      Put_Line ("  Testing SHA3-384: " & Label);
      SHA3_384 (Message_Bytes, Digest);

      if Digest /= Expected_Bytes then
         Put_Line ("    FAIL!");
         Put_Line ("    Expected: " & Expected);
         Put_Line ("    Got:      " & Bytes_To_Hex (Digest));
         Test_Failed := True;
      else
         Put_Line ("    PASS");
      end if;
   end Test_SHA3_384_Vector;

   --  Test SHA3-512 with one vector
   procedure Test_SHA3_512_Vector (Msg : String; Expected : String; Label : String) is
      Message_Bytes : constant Byte_Array := (if Msg'Length = 0 then Byte_Array'(1 .. 0 => 0) else Hex_To_Bytes (Msg));
      Expected_Bytes : constant Byte_Array := Hex_To_Bytes (Expected);
      Digest : SHA3_512_Digest;
   begin
      Put_Line ("  Testing SHA3-512: " & Label);
      SHA3_512 (Message_Bytes, Digest);

      if Digest /= Expected_Bytes then
         Put_Line ("    FAIL!");
         Put_Line ("    Expected: " & Expected);
         Put_Line ("    Got:      " & Bytes_To_Hex (Digest));
         Test_Failed := True;
      else
         Put_Line ("    PASS");
      end if;
   end Test_SHA3_512_Vector;

begin
   Put_Line ("SHA3 Known Answer Tests (NIST FIPS 202)");
   Put_Line ("========================================");
   New_Line;

   --  SHA3-256 Test Vectors (from NIST)
   Put_Line ("SHA3-256 Tests:");

   --  Empty string test
   Test_SHA3_256_Vector (
      Msg      => "",
      Expected => "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a",
      Label    => "Empty message"
   );

   --  Short message: "abc"
   Test_SHA3_256_Vector (
      Msg      => "616263",
      Expected => "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532",
      Label    => "Message 'abc'"
   );

   --  448 bits: "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
   Test_SHA3_256_Vector (
      Msg      => "6162636462636465636465666465666765666768666768696768696a68696a6b" &
                  "696a6b6c6a6b6c6d6b6c6d6e6c6d6e6f6d6e6f706e6f7071",
      Expected => "41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376",
      Label    => "448-bit message"
   );

   New_Line;
   Put_Line ("SHA3-384 Tests:");

   --  Empty string test
   Test_SHA3_384_Vector (
      Msg      => "",
      Expected => "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2a" &
                  "c3713831264adb47fb6bd1e058d5f004",
      Label    => "Empty message"
   );

   --  Short message: "abc"
   Test_SHA3_384_Vector (
      Msg      => "616263",
      Expected => "ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b2" &
                  "98d88cea927ac7f539f1edf228376d25",
      Label    => "Message 'abc'"
   );

   New_Line;
   Put_Line ("SHA3-512 Tests:");

   --  Empty string test
   Test_SHA3_512_Vector (
      Msg      => "",
      Expected => "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a6" &
                  "15b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26",
      Label    => "Empty message"
   );

   --  Short message: "abc"
   Test_SHA3_512_Vector (
      Msg      => "616263",
      Expected => "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e" &
                  "10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0",
      Label    => "Message 'abc'"
   );

   New_Line;
   if Test_Failed then
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_SHA3_KAT;
