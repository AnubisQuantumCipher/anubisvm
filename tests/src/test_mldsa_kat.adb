--  Test ML-DSA-87 Known Answer Tests (NIST FIPS 204)
--
--  This test validates the ML-DSA-87 implementation against test vectors
--  from FIPS 204.

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA; use Anubis_MLDSA;

procedure Test_MLDSA_KAT is

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

   --  Test KeyGen determinism
   procedure Test_KeyGen_Determinism is
      Seed1 : constant Seed := (others => 16#01#);
      Seed2 : constant Seed := (others => 16#01#);
      PK1, PK2 : Public_Key;
      SK1, SK2 : Secret_Key;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": KeyGen determinism... ");

      KeyGen (Seed1, PK1, SK1);
      KeyGen (Seed2, PK2, SK2);

      if Byte_Array (PK1) = Byte_Array (PK2) and then
         Byte_Array (SK1) = Byte_Array (SK2)
      then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Same seed produces different keys");
         Test_Failed := True;
      end if;
   end Test_KeyGen_Determinism;

   --  Test Sign/Verify round-trip
   procedure Test_Sign_Verify_Roundtrip is
      Seed_Bytes : constant Seed := (others => 16#42#);
      Msg : constant Byte_Array := (16#48#, 16#65#, 16#6C#, 16#6C#, 16#6F#);  -- "Hello"
      Random_Seed : constant Seed := (others => 16#AA#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Sign/Verify round-trip... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg, Random_Seed, Sig, Sign_Ok);

      if not Sign_Ok then
         Put_Line ("FAIL - Signing failed");
         Test_Failed := True;
         return;
      end if;

      Verify_Ok := Verify (PK, Msg, Sig);

      if Verify_Ok then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Verification failed");
         Test_Failed := True;
      end if;
   end Test_Sign_Verify_Roundtrip;

   --  Test signature rejection on wrong message
   procedure Test_Wrong_Message_Rejected is
      Seed_Bytes : constant Seed := (others => 16#55#);
      Msg1 : constant Byte_Array := (16#41#, 16#42#, 16#43#);  -- "ABC"
      Msg2 : constant Byte_Array := (16#58#, 16#59#, 16#5A#);  -- "XYZ"
      Random_Seed : constant Seed := (others => 16#BB#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Wrong message rejected... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg1, Random_Seed, Sig, Sign_Ok);

      if not Sign_Ok then
         Put_Line ("FAIL - Signing failed");
         Test_Failed := True;
         return;
      end if;

      --  Verify with wrong message
      Verify_Ok := Verify (PK, Msg2, Sig);

      if not Verify_Ok then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Wrong message accepted");
         Test_Failed := True;
      end if;
   end Test_Wrong_Message_Rejected;

   --  Test KAT vector 1 - KeyGen only
   procedure Test_KAT_Vector_1_KeyGen is
      --  Seed from FIPS 204 test vectors
      --  Public key prefix = rho = first 32 bytes of SHAKE256(seed || domain)
      KAT_Seed_Hex : constant String :=
         "7c9935a0b07694aa0c6d10e4db6b1add2fd81a25ccb148032dcd739936737f2d";
      KAT_PK_Prefix : constant String :=
         "1c0ee1111b08003f28e65e8b3bdeb037cf8f221dfcdaf5950edb38d506d85bef";

      KAT_Seed : Seed;
      PK : Public_Key;
      SK : Secret_Key;
      Seed_Bytes : constant Byte_Array := Hex_To_Bytes (KAT_Seed_Hex);
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": KAT Vector 1 KeyGen... ");

      --  Copy seed
      for I in Seed_Bytes'Range loop
         KAT_Seed (I) := Seed_Bytes (I);
      end loop;

      KeyGen (KAT_Seed, PK, SK);

      --  Compare first 32 bytes of public key
      declare
         PK_Prefix : constant String := Bytes_To_Hex (Byte_Array (PK (0 .. 31)));
      begin
         if PK_Prefix = KAT_PK_Prefix then
            Put_Line ("PASS");
            Pass_Count := Pass_Count + 1;
            Put_Line ("    PK prefix matches: " & PK_Prefix (1 .. 32) & "...");
         else
            Put_Line ("FAIL");
            Put_Line ("    Expected: " & KAT_PK_Prefix);
            Put_Line ("    Got:      " & PK_Prefix);
            Test_Failed := True;
         end if;
      end;
   end Test_KAT_Vector_1_KeyGen;

   --  Test deterministic signing
   procedure Test_Sign_Determinism is
      Seed_Bytes : constant Seed := (others => 16#77#);
      Msg : constant Byte_Array := (16#54#, 16#65#, 16#73#, 16#74#);  -- "Test"
      Random_Seed : constant Seed := (others => 16#CC#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig1, Sig2 : Signature;
      Sign_Ok1, Sign_Ok2 : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Signing determinism... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg, Random_Seed, Sig1, Sign_Ok1);
      Sign (SK, Msg, Random_Seed, Sig2, Sign_Ok2);

      if not (Sign_Ok1 and Sign_Ok2) then
         Put_Line ("FAIL - Signing failed");
         Test_Failed := True;
         return;
      end if;

      if Arrays_Equal (Byte_Array (Sig1), Byte_Array (Sig2)) then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Same inputs produce different signatures");
         Test_Failed := True;
      end if;
   end Test_Sign_Determinism;

   --  Test multiple messages
   procedure Test_Multiple_Messages is
      Seed_Bytes : constant Seed := (others => 16#99#);
      Random_Seed : constant Seed := (others => 16#11#);
      PK : Public_Key;
      SK : Secret_Key;
      All_Passed : Boolean := True;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Multiple messages... ");

      KeyGen (Seed_Bytes, PK, SK);

      --  Test messages of different lengths
      for Len in 1 .. 10 loop
         declare
            Msg : constant Byte_Array (0 .. Len - 1) := (others => Byte (Len));
            Sig : Signature;
            Sign_Ok : Boolean;
            Verify_Ok : Boolean;
         begin
            Sign (SK, Msg, Random_Seed, Sig, Sign_Ok);
            if Sign_Ok then
               Verify_Ok := Verify (PK, Msg, Sig);
               if not Verify_Ok then
                  All_Passed := False;
                  exit;
               end if;
            else
               All_Passed := False;
               exit;
            end if;
         end;
      end loop;

      if All_Passed then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL");
         Test_Failed := True;
      end if;
   end Test_Multiple_Messages;

begin
   Put_Line ("ML-DSA-87 KAT Tests");
   Put_Line ("===================");
   New_Line;

   Test_KeyGen_Determinism;
   Test_Sign_Verify_Roundtrip;
   Test_Wrong_Message_Rejected;
   Test_KAT_Vector_1_KeyGen;
   Test_Sign_Determinism;
   Test_Multiple_Messages;

   New_Line;
   Put_Line ("Results:");
   Put_Line ("  Tests run:  " & Natural'Image (Test_Count));
   Put_Line ("  Passed:     " & Natural'Image (Pass_Count));
   New_Line;

   if Test_Failed then
      Put_Line ("RESULT: SOME TESTS FAILED");
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
   end if;

end Test_MLDSA_KAT;
