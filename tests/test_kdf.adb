pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces; use Interfaces;

with Anubis_Types; use Anubis_Types;
with Anubis_KDF; use Anubis_KDF;

--  Test_KDF: Test suite for Key Derivation Function
--
--  Tests:
--  1. Basic Derive_Key
--  2. Extract/Expand separation
--  3. Different salts produce different keys
--  4. Different contexts produce different keys (domain separation)
--  5. Different info produce different keys
--  6. Determinism (same inputs = same output)
--  7. Derive_Shield_Key
--  8. Derive_Eye_Key
--  9. Derive_Gate_Key
--  10. Derive_Whisper_Key
--  11. Zeroization

procedure Test_KDF is

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

   --  Check if array has non-zero bytes
   function Has_Non_Zero (A : Byte_Array) return Boolean is
   begin
      for I in A'Range loop
         if A (I) /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Has_Non_Zero;

   --  Test 1: Basic Derive_Key
   procedure Test_Basic_Derive is
      Master : Master_Key := (others => 16#42#);
      Salt   : KDF_Salt := (others => 16#24#);
      Info   : Byte_Array (0 .. 7) := (others => 16#11#);
      Key    : Derived_Key;
   begin
      Derive_Key (Master, Salt, Info, Context_Master, Key);

      Print_Hex ("  Derived key: ", Key, 16);
      Report_Test ("Basic Derive_Key produces non-zero output",
                   Has_Non_Zero (Key));
   end Test_Basic_Derive;

   --  Test 2: Extract/Expand separation
   procedure Test_Extract_Expand is
      IKM  : Byte_Array (0 .. 31) := (others => 16#55#);
      Salt : KDF_Salt := (others => 16#66#);
      Info : Byte_Array (0 .. 7) := (others => 16#77#);
      Prk  : KDF_PRK;
      OKM  : Byte_Array (0 .. 63);  -- 64 bytes output
   begin
      --  Extract
      Extract (IKM, Salt, Prk);

      --  Expand
      Expand (Prk, Info, "test", OKM);

      Print_Hex ("  PRK: ", Prk, 8);
      Print_Hex ("  OKM: ", OKM, 16);

      Report_Test ("Extract/Expand produces distinct non-zero outputs",
                   Has_Non_Zero (Prk) and Has_Non_Zero (OKM) and
                   not Arrays_Equal (Prk, OKM (0 .. 31)));
   end Test_Extract_Expand;

   --  Test 3: Different salts produce different keys
   procedure Test_Salt_Independence is
      Master : Master_Key := (others => 16#AA#);
      Salt1  : KDF_Salt := (others => 16#11#);
      Salt2  : KDF_Salt := (others => 16#22#);
      Info   : Byte_Array (0 .. 3) := (others => 16#00#);
      Key1   : Derived_Key;
      Key2   : Derived_Key;
   begin
      Derive_Key (Master, Salt1, Info, Context_Master, Key1);
      Derive_Key (Master, Salt2, Info, Context_Master, Key2);

      Print_Hex ("  Key (salt1): ", Key1, 8);
      Print_Hex ("  Key (salt2): ", Key2, 8);

      Report_Test ("Different salts produce different keys",
                   not Arrays_Equal (Key1, Key2));
   end Test_Salt_Independence;

   --  Test 4: Different contexts produce different keys (domain separation)
   procedure Test_Context_Separation is
      Master : Master_Key := (others => 16#BB#);
      Salt   : KDF_Salt := (others => 16#CC#);
      Info   : Byte_Array (0 .. 3) := (others => 16#DD#);
      Key1   : Derived_Key;
      Key2   : Derived_Key;
   begin
      Derive_Key (Master, Salt, Info, Context_Shield, Key1);
      Derive_Key (Master, Salt, Info, Context_Eye, Key2);

      Print_Hex ("  Key (shield): ", Key1, 8);
      Print_Hex ("  Key (eye):    ", Key2, 8);

      Report_Test ("Different contexts produce different keys (domain separation)",
                   not Arrays_Equal (Key1, Key2));
   end Test_Context_Separation;

   --  Test 5: Different info produce different keys
   procedure Test_Info_Independence is
      Master : Master_Key := (others => 16#EE#);
      Salt   : KDF_Salt := (others => 16#FF#);
      Info1  : Byte_Array (0 .. 7) := (others => 16#11#);
      Info2  : Byte_Array (0 .. 7) := (others => 16#22#);
      Key1   : Derived_Key;
      Key2   : Derived_Key;
   begin
      Derive_Key (Master, Salt, Info1, Context_Gate, Key1);
      Derive_Key (Master, Salt, Info2, Context_Gate, Key2);

      Report_Test ("Different info produces different keys",
                   not Arrays_Equal (Key1, Key2));
   end Test_Info_Independence;

   --  Test 6: Determinism
   procedure Test_Determinism is
      Master : Master_Key := (16#01#, 16#02#, 16#03#, 16#04#, others => 0);
      Salt   : KDF_Salt := (16#05#, 16#06#, 16#07#, 16#08#, others => 0);
      Info   : Byte_Array (0 .. 3) := (16#09#, 16#0A#, 16#0B#, 16#0C#);
      Key1   : Derived_Key;
      Key2   : Derived_Key;
   begin
      Derive_Key (Master, Salt, Info, Context_Whisper, Key1);
      Derive_Key (Master, Salt, Info, Context_Whisper, Key2);

      Report_Test ("Determinism (same inputs = same output)",
                   Arrays_Equal (Key1, Key2));
   end Test_Determinism;

   --  Test 7: Derive_Shield_Key
   procedure Test_Derive_Shield_Key is
      Master : Master_Key := (others => 16#12#);
      Salt   : KDF_Salt := (others => 16#34#);
      Key0   : Derived_Key;
      Key1   : Derived_Key;
   begin
      Derive_Shield_Key (Master, Salt, 0, Key0);
      Derive_Shield_Key (Master, Salt, 1, Key1);

      Print_Hex ("  Shield key (index 0): ", Key0, 8);
      Print_Hex ("  Shield key (index 1): ", Key1, 8);

      Report_Test ("Derive_Shield_Key: different indices produce different keys",
                   Has_Non_Zero (Key0) and Has_Non_Zero (Key1) and
                   not Arrays_Equal (Key0, Key1));
   end Test_Derive_Shield_Key;

   --  Test 8: Derive_Eye_Key
   procedure Test_Derive_Eye_Key is
      Master  : Master_Key := (others => 16#56#);
      Salt    : KDF_Salt := (others => 16#78#);
      Viewer1 : Byte_Array (0 .. 31) := (others => 16#AA#);
      Viewer2 : Byte_Array (0 .. 31) := (others => 16#BB#);
      Key1    : Derived_Key;
      Key2    : Derived_Key;
   begin
      Derive_Eye_Key (Master, Salt, Viewer1, Key1);
      Derive_Eye_Key (Master, Salt, Viewer2, Key2);

      Print_Hex ("  Eye key (viewer1): ", Key1, 8);
      Print_Hex ("  Eye key (viewer2): ", Key2, 8);

      Report_Test ("Derive_Eye_Key: different viewers produce different keys",
                   Has_Non_Zero (Key1) and Has_Non_Zero (Key2) and
                   not Arrays_Equal (Key1, Key2));
   end Test_Derive_Eye_Key;

   --  Test 9: Derive_Gate_Key
   procedure Test_Derive_Gate_Key is
      Master     : Master_Key := (others => 16#9A#);
      Salt       : KDF_Salt := (others => 16#BC#);
      Session1   : Byte_Array (0 .. 31) := (others => 16#11#);
      Session2   : Byte_Array (0 .. 31) := (others => 16#22#);
      Key1       : Derived_Key;
      Key2       : Derived_Key;
   begin
      Derive_Gate_Key (Master, Salt, Session1, Key1);
      Derive_Gate_Key (Master, Salt, Session2, Key2);

      Print_Hex ("  Gate key (session1): ", Key1, 8);
      Print_Hex ("  Gate key (session2): ", Key2, 8);

      Report_Test ("Derive_Gate_Key: different sessions produce different keys",
                   Has_Non_Zero (Key1) and Has_Non_Zero (Key2) and
                   not Arrays_Equal (Key1, Key2));
   end Test_Derive_Gate_Key;

   --  Test 10: Derive_Whisper_Key
   procedure Test_Derive_Whisper_Key is
      Master   : Master_Key := (others => 16#DE#);
      Salt     : KDF_Salt := (others => 16#F0#);
      Value1   : Byte_Array (0 .. 31) := (others => 16#33#);
      Value2   : Byte_Array (0 .. 31) := (others => 16#44#);
      Key1     : Derived_Key;
      Key2     : Derived_Key;
   begin
      Derive_Whisper_Key (Master, Salt, Value1, Key1);
      Derive_Whisper_Key (Master, Salt, Value2, Key2);

      Print_Hex ("  Whisper key (value1): ", Key1, 8);
      Print_Hex ("  Whisper key (value2): ", Key2, 8);

      Report_Test ("Derive_Whisper_Key: different values produce different keys",
                   Has_Non_Zero (Key1) and Has_Non_Zero (Key2) and
                   not Arrays_Equal (Key1, Key2));
   end Test_Derive_Whisper_Key;

   --  Test 11: Zero_Salt produces deterministic output
   procedure Test_Zero_Salt is
      Master : Master_Key := (others => 16#AB#);
      Info   : Byte_Array (0 .. 3) := (others => 16#CD#);
      Key1   : Derived_Key;
      Key2   : Derived_Key;
   begin
      Derive_Key (Master, Zero_Salt, Info, Context_Master, Key1);
      Derive_Key (Master, Zero_Salt, Info, Context_Master, Key2);

      Report_Test ("Zero_Salt produces deterministic output",
                   Arrays_Equal (Key1, Key2));
   end Test_Zero_Salt;

   --  Test 12: Zeroization
   procedure Test_Zeroization is
      Key    : Derived_Key := (others => 16#FF#);
      Master : Master_Key := (others => 16#EE#);
   begin
      Zeroize_Key (Key);
      Zeroize_Master (Master);

      Report_Test ("Zeroization clears keys",
                   Is_All_Zero (Key) and Is_All_Zero (Master));
   end Test_Zeroization;

begin
   Put_Line ("KDF Test Suite (KMAC256-based HKDF)");
   Put_Line ("===================================");
   New_Line;

   Test_Basic_Derive;
   Test_Extract_Expand;
   Test_Salt_Independence;
   Test_Context_Separation;
   Test_Info_Independence;
   Test_Determinism;
   Test_Derive_Shield_Key;
   Test_Derive_Eye_Key;
   Test_Derive_Gate_Key;
   Test_Derive_Whisper_Key;
   Test_Zero_Salt;
   Test_Zeroization;

   New_Line;
   Put_Line ("===================================");
   Put ("Tests run: ");
   Put (Tests_Run, Width => 0);
   New_Line;
   Put ("Tests passed: ");
   Put (Tests_Passed, Width => 0);
   New_Line;
   Put_Line ("===================================");

   if Tests_Passed = Tests_Run then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;
end Test_KDF;
