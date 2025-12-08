--  Basic test for ML-DSA-87

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA; use Anubis_MLDSA;

procedure Test_MLDSA is

   function Bytes_To_Hex (Bytes : Byte_Array; Max_Len : Natural := 16) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Actual_Len : constant Natural := Natural'Min (Bytes'Length, Max_Len);
      Result : String (1 .. Actual_Len * 2 + 3);
      Pos : Natural := 1;
   begin
      for I in 0 .. Actual_Len - 1 loop
         Result (Pos) := Hex_Chars (Natural (Bytes (Bytes'First + I) / 16) + 1);
         Result (Pos + 1) := Hex_Chars (Natural (Bytes (Bytes'First + I) mod 16) + 1);
         Pos := Pos + 2;
      end loop;
      if Bytes'Length > Max_Len then
         Result (Pos .. Pos + 2) := "...";
      else
         Result (Pos .. Pos + 2) := "   ";
      end if;
      return Result;
   end Bytes_To_Hex;

   Tests_Run : Natural := 0;
   Tests_Passed : Natural := 0;

   --  Test 1: Basic key generation and sign/verify
   procedure Test_Basic_Sign_Verify is
      Seed_KG : Seed := (others => 0);
      Random_Sign : Seed := (others => 0);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Success : Boolean;
      Msg : constant Byte_Array (0 .. 31) := (others => 16#42#);
   begin
      Put_Line ("Test 1: Basic sign/verify");
      Tests_Run := Tests_Run + 1;

      --  Set up seed
      for I in 0 .. 31 loop
         Seed_KG (I) := Byte (I);
      end loop;

      Put ("  KeyGen...");
      KeyGen (Seed_KG, PK, SK);
      Put_Line (" done");

      Put_Line ("  PK[0..15]: " & Bytes_To_Hex (Byte_Array (PK)));
      Put_Line ("  SK[0..15]: " & Bytes_To_Hex (Byte_Array (SK)));

      Put ("  Sign...");
      Sign (SK, Msg, Random_Sign, Sig, Success);
      if Success then
         Put_Line (" done");
         Put_Line ("  Sig[0..15]: " & Bytes_To_Hex (Byte_Array (Sig)));
      else
         Put_Line (" FAILED!");
         return;
      end if;

      Put ("  Verify...");
      if Verify (PK, Msg, Sig) then
         Put_Line (" PASS");
         Tests_Passed := Tests_Passed + 1;
      else
         Put_Line (" FAIL");
      end if;
   end Test_Basic_Sign_Verify;

   --  Test 2: Verify with wrong message should fail
   procedure Test_Wrong_Message is
      Seed_KG : Seed := (others => 0);
      Random_Sign : Seed := (others => 0);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Success : Boolean;
      Msg1 : constant Byte_Array (0 .. 31) := (others => 16#42#);
      Msg2 : constant Byte_Array (0 .. 31) := (others => 16#43#);
   begin
      Put_Line ("Test 2: Wrong message should fail");
      Tests_Run := Tests_Run + 1;

      for I in 0 .. 31 loop
         Seed_KG (I) := Byte (I + 100);
      end loop;

      KeyGen (Seed_KG, PK, SK);
      Sign (SK, Msg1, Random_Sign, Sig, Success);

      if not Success then
         Put_Line ("  Sign failed unexpectedly");
         return;
      end if;

      --  Verify with different message should fail
      if Verify (PK, Msg2, Sig) then
         Put_Line ("  FAIL: Signature verified with wrong message!");
      else
         Put_Line ("  PASS: Signature correctly rejected");
         Tests_Passed := Tests_Passed + 1;
      end if;
   end Test_Wrong_Message;

   --  Test 3: Different seeds give different keys
   procedure Test_Different_Seeds is
      Seed1, Seed2 : Seed;
      Random_Sign : Seed := (others => 0);
      PK1, PK2 : Public_Key;
      SK1, SK2 : Secret_Key;
      Same : Boolean := True;
   begin
      Put_Line ("Test 3: Different seeds give different keys");
      Tests_Run := Tests_Run + 1;

      Seed1 := (others => 0);
      Seed2 := (others => 16#FF#);

      KeyGen (Seed1, PK1, SK1);
      KeyGen (Seed2, PK2, SK2);

      --  Check that public keys differ
      for I in PK1'Range loop
         if PK1 (I) /= PK2 (I) then
            Same := False;
            exit;
         end if;
      end loop;

      if Same then
         Put_Line ("  FAIL: Keys should differ!");
      else
         Put_Line ("  PASS: Different seeds produce different keys");
         Tests_Passed := Tests_Passed + 1;
      end if;
   end Test_Different_Seeds;

begin
   Put_Line ("ML-DSA-87 Basic Tests");
   Put_Line ("=====================");
   New_Line;

   Test_Basic_Sign_Verify;
   New_Line;

   Test_Wrong_Message;
   New_Line;

   Test_Different_Seeds;
   New_Line;

   Put_Line ("Results:");
   Put_Line ("  Tests run: " & Natural'Image (Tests_Run));
   Put_Line ("  Passed:    " & Natural'Image (Tests_Passed));
   New_Line;

   if Tests_Passed = Tests_Run then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;
end Test_MLDSA;
