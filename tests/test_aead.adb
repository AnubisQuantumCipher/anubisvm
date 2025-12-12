pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces; use Interfaces;

with Anubis_Types; use Anubis_Types;
with Anubis_AEAD; use Anubis_AEAD;

--  Test_AEAD: Test suite for Authenticated Encryption with Associated Data
--
--  Tests:
--  1. Basic encrypt/decrypt round-trip
--  2. Ciphertext differs from plaintext
--  3. Decrypt with correct tag succeeds
--  4. Decrypt with tampered ciphertext fails (integrity)
--  5. Decrypt with tampered AAD fails (integrity)
--  6. Decrypt with wrong key fails
--  7. Decrypt with wrong nonce fails
--  8. Plaintext zeroed on authentication failure
--  9. Different nonces produce different ciphertexts
--  10. Zeroization

procedure Test_AEAD is

   --  Test counter
   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;

   --  Report test result
   procedure Report_Test (Name : String; Passed : Boolean) is
   begin
      Tests_Run := Tests_Run + 1;
      if Passed then
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Report_Test;

   --  Convert byte to hex string
   function Byte_To_Hex (B : Byte) return String is
      Hex : constant String := "0123456789abcdef";
      Hi  : constant Natural := Natural (B / 16);
      Lo  : constant Natural := Natural (B mod 16);
   begin
      return Hex (Hi + 1) & Hex (Lo + 1);
   end Byte_To_Hex;

   --  Print first N bytes of array as hex
   procedure Print_Hex (Label : String; Data : Byte_Array; N : Natural) is
   begin
      Put (Label);
      for I in Data'First .. Natural'Min (Data'First + N - 1, Data'Last) loop
         Put (Byte_To_Hex (Data (I)));
      end loop;
      if Data'Length > N then
         Put ("...");
      end if;
      New_Line;
   end Print_Hex;

   --  Check if two byte arrays are equal
   function Arrays_Equal (A, B : Byte_Array) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;
      end if;
      for I in 0 .. A'Length - 1 loop
         if A (A'First + I) /= B (B'First + I) then
            return False;
         end if;
      end loop;
      return True;
   end Arrays_Equal;

   --  Check if array is all zeros
   function Is_All_Zero (A : Byte_Array) return Boolean is
   begin
      for I in A'Range loop
         if A (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_All_Zero;

   --  Test 1: Basic encrypt/decrypt round-trip
   procedure Test_Round_Trip is
      Key        : AEAD_Key := (others => 16#42#);
      Nonce      : AEAD_Nonce := (others => 16#24#);
      Plaintext  : Byte_Array (0 .. 31) := (16#48#, 16#65#, 16#6C#, 16#6C#,  -- "Hell"
                                            16#6F#, 16#20#, 16#57#, 16#6F#,  -- "o Wo"
                                            16#72#, 16#6C#, 16#64#, 16#21#,  -- "rld!"
                                            others => 16#00#);
      AAD        : Byte_Array (0 .. 7) := (others => 16#AA#);
      Ciphertext : Byte_Array (0 .. 31);
      Decrypted  : Byte_Array (0 .. 31);
      Tag        : AEAD_Tag;
      Success    : Boolean;
   begin
      --  Encrypt
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Decrypt
      AEAD_Decrypt (Key, Nonce, Ciphertext, AAD, Tag, Decrypted, Success);

      Print_Hex ("  Plaintext:  ", Plaintext, 12);
      Print_Hex ("  Ciphertext: ", Ciphertext, 12);
      Print_Hex ("  Decrypted:  ", Decrypted, 12);

      Report_Test ("Basic encrypt/decrypt round-trip",
                   Success and Arrays_Equal (Plaintext, Decrypted));
   end Test_Round_Trip;

   --  Test 2: Ciphertext differs from plaintext
   procedure Test_Ciphertext_Different is
      Key        : AEAD_Key := (others => 16#11#);
      Nonce      : AEAD_Nonce := (others => 16#22#);
      Plaintext  : Byte_Array (0 .. 15) := (others => 16#33#);
      AAD        : Byte_Array (0 .. 3) := (others => 16#00#);
      Ciphertext : Byte_Array (0 .. 15);
      Tag        : AEAD_Tag;
      Different  : Boolean := False;
   begin
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Ciphertext should differ from plaintext
      for I in Plaintext'Range loop
         if Plaintext (I) /= Ciphertext (I) then
            Different := True;
            exit;
         end if;
      end loop;

      Report_Test ("Ciphertext differs from plaintext", Different);
   end Test_Ciphertext_Different;

   --  Test 3: Tampered ciphertext fails
   procedure Test_Tampered_Ciphertext is
      Key        : AEAD_Key := (others => 16#55#);
      Nonce      : AEAD_Nonce := (others => 16#66#);
      Plaintext  : Byte_Array (0 .. 15) := (others => 16#77#);
      AAD        : Byte_Array (0 .. 3) := (others => 16#88#);
      Ciphertext : Byte_Array (0 .. 15);
      Decrypted  : Byte_Array (0 .. 15);
      Tag        : AEAD_Tag;
      Success    : Boolean;
   begin
      --  Encrypt
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Tamper with ciphertext
      Ciphertext (7) := Ciphertext (7) xor 16#FF#;

      --  Decrypt should fail
      AEAD_Decrypt (Key, Nonce, Ciphertext, AAD, Tag, Decrypted, Success);

      Report_Test ("Tampered ciphertext rejected", not Success);
   end Test_Tampered_Ciphertext;

   --  Test 4: Tampered AAD fails
   procedure Test_Tampered_AAD is
      Key        : AEAD_Key := (others => 16#AA#);
      Nonce      : AEAD_Nonce := (others => 16#BB#);
      Plaintext  : Byte_Array (0 .. 15) := (others => 16#CC#);
      AAD        : Byte_Array (0 .. 7) := (others => 16#DD#);
      Tampered_AAD : Byte_Array (0 .. 7) := (others => 16#DD#);
      Ciphertext : Byte_Array (0 .. 15);
      Decrypted  : Byte_Array (0 .. 15);
      Tag        : AEAD_Tag;
      Success    : Boolean;
   begin
      --  Encrypt with original AAD
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Tamper with AAD
      Tampered_AAD (3) := 16#00#;

      --  Decrypt with tampered AAD should fail
      AEAD_Decrypt (Key, Nonce, Ciphertext, Tampered_AAD, Tag, Decrypted, Success);

      Report_Test ("Tampered AAD rejected", not Success);
   end Test_Tampered_AAD;

   --  Test 5: Wrong key fails
   procedure Test_Wrong_Key is
      Key1       : AEAD_Key := (others => 16#11#);
      Key2       : AEAD_Key := (others => 16#22#);
      Nonce      : AEAD_Nonce := (others => 16#33#);
      Plaintext  : Byte_Array (0 .. 15) := (others => 16#44#);
      AAD        : Byte_Array (0 .. 3) := (others => 16#55#);
      Ciphertext : Byte_Array (0 .. 15);
      Decrypted  : Byte_Array (0 .. 15);
      Tag        : AEAD_Tag;
      Success    : Boolean;
   begin
      --  Encrypt with key1
      AEAD_Encrypt (Key1, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Decrypt with key2 should fail
      AEAD_Decrypt (Key2, Nonce, Ciphertext, AAD, Tag, Decrypted, Success);

      Report_Test ("Wrong key rejected", not Success);
   end Test_Wrong_Key;

   --  Test 6: Wrong nonce fails
   procedure Test_Wrong_Nonce is
      Key        : AEAD_Key := (others => 16#66#);
      Nonce1     : AEAD_Nonce := (others => 16#77#);
      Nonce2     : AEAD_Nonce := (others => 16#88#);
      Plaintext  : Byte_Array (0 .. 15) := (others => 16#99#);
      AAD        : Byte_Array (0 .. 3) := (others => 16#AA#);
      Ciphertext : Byte_Array (0 .. 15);
      Decrypted  : Byte_Array (0 .. 15);
      Tag        : AEAD_Tag;
      Success    : Boolean;
   begin
      --  Encrypt with nonce1
      AEAD_Encrypt (Key, Nonce1, Plaintext, AAD, Ciphertext, Tag);

      --  Decrypt with nonce2 should fail
      AEAD_Decrypt (Key, Nonce2, Ciphertext, AAD, Tag, Decrypted, Success);

      Report_Test ("Wrong nonce rejected", not Success);
   end Test_Wrong_Nonce;

   --  Test 7: Plaintext zeroed on failure
   procedure Test_Plaintext_Zeroed_On_Failure is
      Key        : AEAD_Key := (others => 16#BB#);
      Nonce      : AEAD_Nonce := (others => 16#CC#);
      Plaintext  : Byte_Array (0 .. 15) := (others => 16#DD#);
      AAD        : Byte_Array (0 .. 3) := (others => 16#EE#);
      Ciphertext : Byte_Array (0 .. 15);
      Decrypted  : Byte_Array (0 .. 15) := (others => 16#FF#);  -- Pre-fill with non-zero
      Tag        : AEAD_Tag;
      Success    : Boolean;
   begin
      --  Encrypt
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Tamper with tag
      Tag (0) := Tag (0) xor 16#01#;

      --  Decrypt should fail and zero plaintext
      AEAD_Decrypt (Key, Nonce, Ciphertext, AAD, Tag, Decrypted, Success);

      Report_Test ("Plaintext zeroed on auth failure",
                   not Success and Is_All_Zero (Decrypted));
   end Test_Plaintext_Zeroed_On_Failure;

   --  Test 8: Different nonces produce different ciphertexts
   procedure Test_Nonce_Uniqueness is
      Key        : AEAD_Key := (others => 16#12#);
      Nonce1     : AEAD_Nonce := (others => 16#34#);
      Nonce2     : AEAD_Nonce := (others => 16#56#);
      Plaintext  : Byte_Array (0 .. 15) := (others => 16#78#);
      AAD        : Byte_Array (0 .. 3) := (others => 16#9A#);
      Ciphertext1 : Byte_Array (0 .. 15);
      Ciphertext2 : Byte_Array (0 .. 15);
      Tag1       : AEAD_Tag;
      Tag2       : AEAD_Tag;
      Different  : Boolean := False;
   begin
      --  Encrypt same plaintext with different nonces
      AEAD_Encrypt (Key, Nonce1, Plaintext, AAD, Ciphertext1, Tag1);
      AEAD_Encrypt (Key, Nonce2, Plaintext, AAD, Ciphertext2, Tag2);

      --  Ciphertexts should differ
      for I in Ciphertext1'Range loop
         if Ciphertext1 (I) /= Ciphertext2 (I) then
            Different := True;
            exit;
         end if;
      end loop;

      Report_Test ("Different nonces produce different ciphertexts", Different);
   end Test_Nonce_Uniqueness;

   --  Test 9: Empty plaintext
   procedure Test_Empty_Plaintext is
      Key        : AEAD_Key := (others => 16#AB#);
      Nonce      : AEAD_Nonce := (others => 16#CD#);
      Plaintext  : Byte_Array (1 .. 0);  -- Empty
      AAD        : Byte_Array (0 .. 7) := (others => 16#EF#);
      Ciphertext : Byte_Array (1 .. 0);  -- Empty
      Decrypted  : Byte_Array (1 .. 0);  -- Empty
      Tag        : AEAD_Tag;
      Success    : Boolean;
      Tag_Non_Zero : Boolean := False;
   begin
      --  Encrypt empty plaintext (AAD-only authentication)
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Tag should still be computed
      for I in Tag'Range loop
         if Tag (I) /= 0 then
            Tag_Non_Zero := True;
            exit;
         end if;
      end loop;

      --  Decrypt should succeed
      AEAD_Decrypt (Key, Nonce, Ciphertext, AAD, Tag, Decrypted, Success);

      Report_Test ("Empty plaintext (AAD-only) works",
                   Tag_Non_Zero and Success);
   end Test_Empty_Plaintext;

   --  Test 10: Zeroization
   procedure Test_Zeroization is
      Key   : AEAD_Key := (others => 16#FF#);
      Nonce : AEAD_Nonce := (others => 16#EE#);
   begin
      Zeroize_Key (Key);
      Zeroize_Nonce (Nonce);

      Report_Test ("Zeroization clears key and nonce",
                   Is_All_Zero (Key) and Is_All_Zero (Nonce));
   end Test_Zeroization;

begin
   Put_Line ("AEAD Test Suite (SHAKE256 + KMAC256)");
   Put_Line ("====================================");
   New_Line;

   Test_Round_Trip;
   Test_Ciphertext_Different;
   Test_Tampered_Ciphertext;
   Test_Tampered_AAD;
   Test_Wrong_Key;
   Test_Wrong_Nonce;
   Test_Plaintext_Zeroed_On_Failure;
   Test_Nonce_Uniqueness;
   Test_Empty_Plaintext;
   Test_Zeroization;

   New_Line;
   Put_Line ("====================================");
   Put ("Tests run: ");
   Put (Tests_Run, Width => 0);
   New_Line;
   Put ("Tests passed: ");
   Put (Tests_Passed, Width => 0);
   New_Line;
   Put_Line ("====================================");

   if Tests_Passed = Tests_Run then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;
end Test_AEAD;
