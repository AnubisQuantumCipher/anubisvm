pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces; use Interfaces;

with Anubis_Types; use Anubis_Types;
with Anubis_KMAC; use Anubis_KMAC;

--  Test_KMAC: Test suite for KMAC256 (NIST SP 800-185)
--
--  Tests:
--  1. Basic KMAC256 computation
--  2. KMAC256 with customization string
--  3. KMAC256_XOF (extended output)
--  4. Verify_KMAC256 (constant-time verification)
--  5. Tamper detection (modified message should fail)
--  6. Zeroization
--
--  Reference: NIST SP 800-185 test vectors

procedure Test_KMAC is

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

   --  Test 1: Basic KMAC256 computation
   procedure Test_Basic_KMAC256 is
      Key     : KMAC_Key := (others => 16#42#);
      Message : Byte_Array (0 .. 31) := (others => 16#00#);
      Tag     : KMAC256_Tag;
   begin
      KMAC256 (Key, Message, "", Tag);

      --  Verify tag is not all zeros (sanity check)
      declare
         Non_Zero : Boolean := False;
      begin
         for I in Tag'Range loop
            if Tag (I) /= 0 then
               Non_Zero := True;
               exit;
            end if;
         end loop;

         Print_Hex ("  Tag: ", Tag, 16);
         Report_Test ("Basic KMAC256 computation", Non_Zero);
      end;
   end Test_Basic_KMAC256;

   --  Test 2: KMAC256 with customization string
   procedure Test_KMAC256_Custom is
      Key      : KMAC_Key := (others => 16#AA#);
      Message  : Byte_Array (0 .. 15) := (others => 16#BB#);
      Tag1     : KMAC256_Tag;
      Tag2     : KMAC256_Tag;
      Different : Boolean := False;
   begin
      --  Same key/message, different customization strings
      KMAC256 (Key, Message, "context1", Tag1);
      KMAC256 (Key, Message, "context2", Tag2);

      --  Tags should be different due to domain separation
      for I in Tag1'Range loop
         if Tag1 (I) /= Tag2 (I) then
            Different := True;
            exit;
         end if;
      end loop;

      Print_Hex ("  Tag (context1): ", Tag1, 8);
      Print_Hex ("  Tag (context2): ", Tag2, 8);
      Report_Test ("KMAC256 domain separation", Different);
   end Test_KMAC256_Custom;

   --  Test 3: KMAC256 determinism
   procedure Test_KMAC256_Determinism is
      Key     : KMAC_Key := (16#01#, 16#02#, 16#03#, 16#04#, others => 0);
      Message : Byte_Array (0 .. 7) := (16#DE#, 16#AD#, 16#BE#, 16#EF#,
                                         16#CA#, 16#FE#, 16#BA#, 16#BE#);
      Tag1    : KMAC256_Tag;
      Tag2    : KMAC256_Tag;
      Match   : Boolean := True;
   begin
      KMAC256 (Key, Message, "test", Tag1);
      KMAC256 (Key, Message, "test", Tag2);

      for I in Tag1'Range loop
         if Tag1 (I) /= Tag2 (I) then
            Match := False;
            exit;
         end if;
      end loop;

      Report_Test ("KMAC256 determinism (same inputs = same output)", Match);
   end Test_KMAC256_Determinism;

   --  Test 4: KMAC256_XOF (extended output)
   procedure Test_KMAC256_XOF is
      Key  : KMAC_Key := (others => 16#55#);
      Msg  : Byte_Array (0 .. 3) := (16#01#, 16#02#, 16#03#, 16#04#);
      Tag  : KMAC256_Tag_512;
      Non_Zero : Boolean := False;
   begin
      KMAC256_XOF (Key, Msg, "xof_test", Tag);

      --  Verify output is 64 bytes and not all zeros
      for I in Tag'Range loop
         if Tag (I) /= 0 then
            Non_Zero := True;
            exit;
         end if;
      end loop;

      Print_Hex ("  XOF Tag (64 bytes): ", Tag, 16);
      Report_Test ("KMAC256_XOF extended output",
                   Tag'Length = 64 and Non_Zero);
   end Test_KMAC256_XOF;

   --  Test 5: Verify_KMAC256 (correct tag)
   procedure Test_Verify_Correct is
      Key     : KMAC_Key := (others => 16#77#);
      Message : Byte_Array (0 .. 19) := (others => 16#88#);
      Tag     : KMAC256_Tag;
      Valid   : Boolean;
   begin
      KMAC256 (Key, Message, "verify_test", Tag);
      Valid := Verify_KMAC256 (Key, Message, "verify_test", Tag);

      Report_Test ("Verify_KMAC256 accepts correct tag", Valid);
   end Test_Verify_Correct;

   --  Test 6: Verify_KMAC256 (tampered message)
   procedure Test_Verify_Tampered is
      Key         : KMAC_Key := (others => 16#99#);
      Message     : Byte_Array (0 .. 15) := (others => 16#AA#);
      Tampered    : Byte_Array (0 .. 15) := (others => 16#AA#);
      Tag         : KMAC256_Tag;
      Should_Fail : Boolean;
   begin
      --  Compute tag on original message
      KMAC256 (Key, Message, "tamper_test", Tag);

      --  Tamper with message
      Tampered (7) := 16#BB#;

      --  Verify should fail on tampered message
      Should_Fail := not Verify_KMAC256 (Key, Tampered, "tamper_test", Tag);

      Report_Test ("Verify_KMAC256 rejects tampered message", Should_Fail);
   end Test_Verify_Tampered;

   --  Test 7: Verify_KMAC256 (wrong key)
   procedure Test_Verify_Wrong_Key is
      Key1        : KMAC_Key := (others => 16#11#);
      Key2        : KMAC_Key := (others => 16#22#);
      Message     : Byte_Array (0 .. 7) := (others => 16#33#);
      Tag         : KMAC256_Tag;
      Should_Fail : Boolean;
   begin
      --  Compute tag with key1
      KMAC256 (Key1, Message, "", Tag);

      --  Verify with key2 should fail
      Should_Fail := not Verify_KMAC256 (Key2, Message, "", Tag);

      Report_Test ("Verify_KMAC256 rejects wrong key", Should_Fail);
   end Test_Verify_Wrong_Key;

   --  Test 8: Zeroization
   procedure Test_Zeroization is
      Key     : KMAC_Key := (others => 16#FF#);
      All_Zero : Boolean := True;
   begin
      Zeroize_Key (Key);

      for I in Key'Range loop
         if Key (I) /= 0 then
            All_Zero := False;
            exit;
         end if;
      end loop;

      Report_Test ("Zeroize_Key clears all bytes", All_Zero);
   end Test_Zeroization;

   --  Test 9: Empty message
   procedure Test_Empty_Message is
      Key     : KMAC_Key := (others => 16#CC#);
      Empty   : Byte_Array (1 .. 0);  -- Empty array
      Tag     : KMAC256_Tag;
      Non_Zero : Boolean := False;
   begin
      KMAC256 (Key, Empty, "empty", Tag);

      for I in Tag'Range loop
         if Tag (I) /= 0 then
            Non_Zero := True;
            exit;
         end if;
      end loop;

      Print_Hex ("  Empty message tag: ", Tag, 8);
      Report_Test ("KMAC256 handles empty message", Non_Zero);
   end Test_Empty_Message;

begin
   Put_Line ("KMAC256 Test Suite (NIST SP 800-185)");
   Put_Line ("====================================");
   New_Line;

   Test_Basic_KMAC256;
   Test_KMAC256_Custom;
   Test_KMAC256_Determinism;
   Test_KMAC256_XOF;
   Test_Verify_Correct;
   Test_Verify_Tampered;
   Test_Verify_Wrong_Key;
   Test_Zeroization;
   Test_Empty_Message;

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
end Test_KMAC;
