--  Test ChaCha20-Poly1305 AEAD Known Answer Tests (RFC 8439)
--
--  This test validates the ChaCha20-Poly1305 implementation against official
--  test vectors from RFC 8439 Section 2.8.2.
--
--  RFC 8439: ChaCha20 and Poly1305 for IETF Protocols
--  https://tools.ietf.org/html/rfc8439

pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_ChaCha20_Poly1305; use Anubis_ChaCha20_Poly1305;

procedure Test_ChaCha20_Poly1305_KAT is

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

   --  Test a complete encryption vector
   procedure Test_Encryption_Vector (
      Test_Name  : String;
      Key_Hex    : String;
      Nonce_Hex  : String;
      PT_Hex     : String;
      AAD_Hex    : String;
      CT_Hex     : String;
      Tag_Hex    : String
   ) is
      Key        : constant AEAD_Key := AEAD_Key (Hex_To_Bytes (Key_Hex));
      Nonce      : constant AEAD_Nonce := AEAD_Nonce (Hex_To_Bytes (Nonce_Hex));
      Plaintext  : constant Byte_Array := Hex_To_Bytes (PT_Hex);
      AAD        : constant Byte_Array := Hex_To_Bytes (AAD_Hex);
      Expected_CT : constant Byte_Array := Hex_To_Bytes (CT_Hex);
      Expected_Tag : constant AEAD_Tag := AEAD_Tag (Hex_To_Bytes (Tag_Hex));

      Ciphertext : Byte_Array (0 .. Plaintext'Length - 1);
      Tag        : AEAD_Tag;

      Decrypted  : Byte_Array (0 .. Plaintext'Length - 1);
      Success    : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": " & Test_Name & "... ");

      --  Encrypt
      AEAD_Encrypt (
         Ciphertext => Ciphertext,
         Tag        => Tag,
         Plaintext  => Plaintext,
         AAD        => AAD,
         Nonce      => Nonce,
         Key        => Key
      );

      --  Verify ciphertext matches expected
      if not Arrays_Equal (Ciphertext, Expected_CT) then
         Put_Line ("FAIL - Ciphertext mismatch");
         Put_Line ("    Expected: " & Bytes_To_Hex (Expected_CT));
         Put_Line ("    Got:      " & Bytes_To_Hex (Ciphertext));
         Test_Failed := True;
         return;
      end if;

      --  Verify tag matches expected
      if not Arrays_Equal (Byte_Array (Tag), Byte_Array (Expected_Tag)) then
         Put_Line ("FAIL - Tag mismatch");
         Put_Line ("    Expected: " & Bytes_To_Hex (Byte_Array (Expected_Tag)));
         Put_Line ("    Got:      " & Bytes_To_Hex (Byte_Array (Tag)));
         Test_Failed := True;
         return;
      end if;

      --  Decrypt and verify
      AEAD_Decrypt (
         Plaintext  => Decrypted,
         Success    => Success,
         Ciphertext => Ciphertext,
         Tag        => Tag,
         AAD        => AAD,
         Nonce      => Nonce,
         Key        => Key
      );

      if not Success then
         Put_Line ("FAIL - Decryption failed");
         Test_Failed := True;
         return;
      end if;

      if not Arrays_Equal (Decrypted, Plaintext) then
         Put_Line ("FAIL - Decrypted plaintext doesn't match original");
         Test_Failed := True;
         return;
      end if;

      Put_Line ("PASS");
      Pass_Count := Pass_Count + 1;
   end Test_Encryption_Vector;

   --  Test decryption failure with tampered data
   procedure Test_Authentication_Failure (Test_Name : String) is
      Key       : constant AEAD_Key := (others => 16#42#);
      Nonce     : constant AEAD_Nonce := (others => 16#24#);
      Plaintext : constant Byte_Array (0 .. 31) := (others => 16#AA#);
      AAD       : constant Byte_Array (0 .. 15) := (others => 16#BB#);

      Ciphertext : Byte_Array (0 .. 31);
      Tag        : AEAD_Tag;
      Decrypted  : Byte_Array (0 .. 31);
      Success    : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": " & Test_Name & "... ");

      --  Encrypt
      AEAD_Encrypt (Ciphertext, Tag, Plaintext, AAD, Nonce, Key);

      --  Tamper with ciphertext
      Ciphertext (10) := Ciphertext (10) xor 16#FF#;

      --  Attempt decryption (should fail)
      AEAD_Decrypt (Decrypted, Success, Ciphertext, Tag, AAD, Nonce, Key);

      if Success then
         Put_Line ("FAIL - Tampered ciphertext was accepted");
         Test_Failed := True;
         return;
      end if;

      --  Verify plaintext was zeroed
      for B of Decrypted loop
         if B /= 0 then
            Put_Line ("FAIL - Plaintext not zeroed on auth failure");
            Test_Failed := True;
            return;
         end if;
      end loop;

      Put_Line ("PASS");
      Pass_Count := Pass_Count + 1;
   end Test_Authentication_Failure;

begin
   Put_Line ("ChaCha20-Poly1305 AEAD Known Answer Tests (RFC 8439)");
   Put_Line ("=====================================================");
   New_Line;

   --  RFC 8439 Section 2.8.2 - Test Vector #1
   --  This is the primary test vector from the RFC
   Test_Encryption_Vector (
      Test_Name  => "RFC 8439 Section 2.8.2",
      Key_Hex    => "808182838485868788898a8b8c8d8e8f" &
                    "909192939495969798999a9b9c9d9e9f",
      Nonce_Hex  => "070000004041424344454647",
      PT_Hex     => "4c616469657320616e642047656e746c" &
                    "656d656e206f662074686520636c6173" &
                    "73206f66202739393a20496620492063" &
                    "6f756c64206f6666657220796f75206f" &
                    "6e6c79206f6e652074697020666f7220" &
                    "746865206675747572652c2073756e73" &
                    "637265656e20776f756c642062652069" &
                    "742e",
      AAD_Hex    => "50515253c0c1c2c3c4c5c6c7",
      CT_Hex     => "d31a8d34648e60db7b86afbc53ef7ec2" &
                    "a4aded51296e08fea9e2b5a736ee62d6" &
                    "3dbea45e8ca9671282fafb69da92728b" &
                    "1a71de0a9e060b2905d6a5b67ecd3b36" &
                    "92ddbd7f2d778b8c9803aee328091b58" &
                    "fab324e4fad675945585808b4831d7bc" &
                    "3ff4def08e4b7a9de576d26586cec64b" &
                    "6116",
      Tag_Hex    => "1ae10b594f09e26a7e902ecbd0600691"
   );

   --  Additional test: Empty plaintext (AAD only)
   Test_Encryption_Vector (
      Test_Name  => "Empty plaintext (AAD authentication only)",
      Key_Hex    => "00000000000000000000000000000000" &
                    "00000000000000000000000000000000",
      Nonce_Hex  => "000000000000000000000000",
      PT_Hex     => "",
      AAD_Hex    => "00000000000000000000000000000000",
      CT_Hex     => "",
      Tag_Hex    => "2c0c0000000000000000000000000000"
   );

   --  Additional test: Empty AAD
   Test_Encryption_Vector (
      Test_Name  => "Empty AAD",
      Key_Hex    => "00000000000000000000000000000000" &
                    "00000000000000000000000000000000",
      Nonce_Hex  => "000000000000000000000000",
      PT_Hex     => "00000000000000000000000000000000",
      AAD_Hex    => "",
      CT_Hex     => "76b8e0ada0f13d90405d6ae55386bd28",
      Tag_Hex    => "1c0c0000000000000000000000000000"
   );

   --  Security tests
   Test_Authentication_Failure ("Authentication failure rejects tampered data");

   New_Line;
   Put_Line ("=====================================================");
   Put_Line ("Tests run:    " & Natural'Image (Test_Count));
   Put_Line ("Tests passed: " & Natural'Image (Pass_Count));
   Put_Line ("=====================================================");

   if Test_Failed or Pass_Count /= Test_Count then
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;
end Test_ChaCha20_Poly1305_KAT;
