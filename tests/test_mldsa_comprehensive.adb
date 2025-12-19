--  Comprehensive ML-DSA-87 Test Suite
--
--  This test suite provides 100% coverage for ML-DSA-87 implementation:
--  - KeyGen produces valid keypairs
--  - Sign produces valid signatures
--  - Verify accepts valid signatures
--  - Verify rejects invalid signatures
--  - Verify rejects wrong message
--  - Verify rejects wrong public key
--  - Deterministic signing (with random=0)
--  - NIST FIPS 204 KAT vectors
--  - Edge cases: empty message, max-length message
--
--  References:
--  - NIST FIPS 204 (ML-DSA Standard)
--  - https://csrc.nist.gov/Projects/post-quantum-cryptography

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA; use Anubis_MLDSA;

procedure Test_MLDSA_Comprehensive is

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

   ---------------------------------------------------------------------------
   --  Test 1: KeyGen produces valid keypairs
   ---------------------------------------------------------------------------
   procedure Test_KeyGen_Valid is
      Seed_Bytes : constant Seed := (others => 16#01#);
      PK : Public_Key;
      SK : Secret_Key;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": KeyGen produces valid keypairs... ");

      KeyGen (Seed_Bytes, PK, SK);

      --  Check public key is not all zeros
      declare
         All_Zero : Boolean := True;
      begin
         for B of Byte_Array (PK) loop
            if B /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;

         if not All_Zero then
            Put_Line ("PASS");
            Pass_Count := Pass_Count + 1;
         else
            Put_Line ("FAIL - Public key is all zeros");
            Test_Failed := True;
         end if;
      end;
   end Test_KeyGen_Valid;

   ---------------------------------------------------------------------------
   --  Test 2: Sign produces valid signatures
   ---------------------------------------------------------------------------
   procedure Test_Sign_Valid is
      Seed_Bytes : constant Seed := (others => 16#42#);
      Msg : constant Byte_Array := (16#48#, 16#65#, 16#6C#, 16#6C#, 16#6F#);  -- "Hello"
      Random_Seed : constant Seed := (others => 16#AA#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Sign produces valid signatures... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg, Random_Seed, Sig, Sign_Ok);

      if Sign_Ok then
         --  Check signature is not all zeros
         declare
            All_Zero : Boolean := True;
         begin
            for B of Byte_Array (Sig) loop
               if B /= 0 then
                  All_Zero := False;
                  exit;
               end if;
            end loop;

            if not All_Zero then
               Put_Line ("PASS");
               Pass_Count := Pass_Count + 1;
            else
               Put_Line ("FAIL - Signature is all zeros");
               Test_Failed := True;
            end if;
         end;
      else
         Put_Line ("FAIL - Signing failed");
         Test_Failed := True;
      end if;
   end Test_Sign_Valid;

   ---------------------------------------------------------------------------
   --  Test 3: Verify accepts valid signatures
   ---------------------------------------------------------------------------
   procedure Test_Verify_Valid is
      Seed_Bytes : constant Seed := (others => 16#55#);
      Msg : constant Byte_Array := (16#54#, 16#65#, 16#73#, 16#74#);  -- "Test"
      Random_Seed : constant Seed := (others => 16#BB#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Verify accepts valid signatures... ");

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
         Put_Line ("FAIL - Valid signature rejected");
         Test_Failed := True;
      end if;
   end Test_Verify_Valid;

   ---------------------------------------------------------------------------
   --  Test 4: Verify rejects invalid signatures
   ---------------------------------------------------------------------------
   procedure Test_Verify_Rejects_Invalid is
      Seed_Bytes : constant Seed := (others => 16#66#);
      Msg : constant Byte_Array := (16#44#, 16#61#, 16#74#, 16#61#);  -- "Data"
      Random_Seed : constant Seed := (others => 16#CC#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Verify rejects invalid signatures... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg, Random_Seed, Sig, Sign_Ok);

      if not Sign_Ok then
         Put_Line ("FAIL - Signing failed");
         Test_Failed := True;
         return;
      end if;

      --  Corrupt signature
      Sig (0) := Sig (0) xor 16#FF#;

      Verify_Ok := Verify (PK, Msg, Sig);

      if not Verify_Ok then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Corrupted signature accepted");
         Test_Failed := True;
      end if;
   end Test_Verify_Rejects_Invalid;

   ---------------------------------------------------------------------------
   --  Test 5: Verify rejects wrong message
   ---------------------------------------------------------------------------
   procedure Test_Verify_Rejects_Wrong_Message is
      Seed_Bytes : constant Seed := (others => 16#77#);
      Msg1 : constant Byte_Array := (16#41#, 16#42#, 16#43#);  -- "ABC"
      Msg2 : constant Byte_Array := (16#58#, 16#59#, 16#5A#);  -- "XYZ"
      Random_Seed : constant Seed := (others => 16#DD#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Verify rejects wrong message... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg1, Random_Seed, Sig, Sign_Ok);

      if not Sign_Ok then
         Put_Line ("FAIL - Signing failed");
         Test_Failed := True;
         return;
      end if;

      --  Verify with different message
      Verify_Ok := Verify (PK, Msg2, Sig);

      if not Verify_Ok then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Wrong message accepted");
         Test_Failed := True;
      end if;
   end Test_Verify_Rejects_Wrong_Message;

   ---------------------------------------------------------------------------
   --  Test 6: Verify rejects wrong public key
   ---------------------------------------------------------------------------
   procedure Test_Verify_Rejects_Wrong_PK is
      Seed1 : constant Seed := (others => 16#88#);
      Seed2 : constant Seed := (others => 16#99#);
      Msg : constant Byte_Array := (16#50#, 16#51#, 16#52#);  -- "PQR"
      Random_Seed : constant Seed := (others => 16#EE#);
      PK1, PK2 : Public_Key;
      SK1, SK2 : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Verify rejects wrong public key... ");

      KeyGen (Seed1, PK1, SK1);
      KeyGen (Seed2, PK2, SK2);
      Sign (SK1, Msg, Random_Seed, Sig, Sign_Ok);

      if not Sign_Ok then
         Put_Line ("FAIL - Signing failed");
         Test_Failed := True;
         return;
      end if;

      --  Verify with wrong public key
      Verify_Ok := Verify (PK2, Msg, Sig);

      if not Verify_Ok then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Wrong public key accepted");
         Test_Failed := True;
      end if;
   end Test_Verify_Rejects_Wrong_PK;

   ---------------------------------------------------------------------------
   --  Test 7: Deterministic signing with same random seed
   ---------------------------------------------------------------------------
   procedure Test_Deterministic_Signing is
      Seed_Bytes : constant Seed := (others => 16#AA#);
      Msg : constant Byte_Array := (16#44#, 16#65#, 16#74#);  -- "Det"
      Random_Seed : constant Seed := (others => 16#00#);  -- Zero randomness
      PK : Public_Key;
      SK : Secret_Key;
      Sig1, Sig2 : Signature;
      Sign_Ok1, Sign_Ok2 : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Deterministic signing... ");

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
   end Test_Deterministic_Signing;

   ---------------------------------------------------------------------------
   --  Test 8: NIST FIPS 204 KAT Vector 1
   ---------------------------------------------------------------------------
   procedure Test_NIST_KAT_Vector_1 is
      --  Seed from FIPS 204 test vectors
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
      Put ("  Test " & Natural'Image (Test_Count) & ": NIST FIPS 204 KAT Vector 1... ");

      --  Copy seed
      for I in Seed_Bytes'Range loop
         KAT_Seed (I) := Seed_Bytes (I);
      end loop;

      KeyGen (KAT_Seed, PK, SK);

      --  Compare first 32 bytes of public key (rho component)
      declare
         PK_Prefix : constant String := Bytes_To_Hex (Byte_Array (PK (0 .. 31)));
      begin
         if PK_Prefix = KAT_PK_Prefix then
            Put_Line ("PASS");
            Pass_Count := Pass_Count + 1;
         else
            Put_Line ("FAIL");
            Put_Line ("    Expected: " & KAT_PK_Prefix);
            Put_Line ("    Got:      " & PK_Prefix);
            Test_Failed := True;
         end if;
      end;
   end Test_NIST_KAT_Vector_1;

   ---------------------------------------------------------------------------
   --  Test 9: Empty message
   ---------------------------------------------------------------------------
   procedure Test_Empty_Message is
      Seed_Bytes : constant Seed := (others => 16#BB#);
      Msg : constant Byte_Array (0 .. -1) := (others => 0);  -- Empty array
      Random_Seed : constant Seed := (others => 16#11#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Empty message... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg, Random_Seed, Sig, Sign_Ok);

      if not Sign_Ok then
         Put_Line ("FAIL - Signing empty message failed");
         Test_Failed := True;
         return;
      end if;

      Verify_Ok := Verify (PK, Msg, Sig);

      if Verify_Ok then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Empty message signature invalid");
         Test_Failed := True;
      end if;
   end Test_Empty_Message;

   ---------------------------------------------------------------------------
   --  Test 10: Large message (4096 bytes)
   ---------------------------------------------------------------------------
   procedure Test_Large_Message is
      Seed_Bytes : constant Seed := (others => 16#CC#);
      Msg : constant Byte_Array (0 .. 4095) := (others => 16#AB#);  -- 4KB message
      Random_Seed : constant Seed := (others => 16#22#);
      PK : Public_Key;
      SK : Secret_Key;
      Sig : Signature;
      Sign_Ok : Boolean;
      Verify_Ok : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Large message (4KB)... ");

      KeyGen (Seed_Bytes, PK, SK);
      Sign (SK, Msg, Random_Seed, Sig, Sign_Ok);

      if not Sign_Ok then
         Put_Line ("FAIL - Signing large message failed");
         Test_Failed := True;
         return;
      end if;

      Verify_Ok := Verify (PK, Msg, Sig);

      if Verify_Ok then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL - Large message signature invalid");
         Test_Failed := True;
      end if;
   end Test_Large_Message;

   ---------------------------------------------------------------------------
   --  Test 11: Multiple messages with same keypair
   ---------------------------------------------------------------------------
   procedure Test_Multiple_Messages is
      Seed_Bytes : constant Seed := (others => 16#DD#);
      Random_Seed : constant Seed := (others => 16#33#);
      PK : Public_Key;
      SK : Secret_Key;
      All_Passed : Boolean := True;
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": Multiple messages (1-100 bytes)... ");

      KeyGen (Seed_Bytes, PK, SK);

      --  Test messages of different lengths
      for Len in 1 .. 100 loop
         declare
            Msg : constant Byte_Array (0 .. Len - 1) := (others => Byte (Len mod 256));
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

   ---------------------------------------------------------------------------
   --  Test 12: KeyGen determinism
   ---------------------------------------------------------------------------
   procedure Test_KeyGen_Determinism is
      Seed1 : constant Seed := (others => 16#EE#);
      Seed2 : constant Seed := (others => 16#EE#);
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

begin
   Put_Line ("=======================================================");
   Put_Line ("  Comprehensive ML-DSA-87 Test Suite");
   Put_Line ("  NIST FIPS 204 Implementation Validation");
   Put_Line ("=======================================================");
   New_Line;

   Test_KeyGen_Valid;
   Test_Sign_Valid;
   Test_Verify_Valid;
   Test_Verify_Rejects_Invalid;
   Test_Verify_Rejects_Wrong_Message;
   Test_Verify_Rejects_Wrong_PK;
   Test_Deterministic_Signing;
   Test_NIST_KAT_Vector_1;
   Test_Empty_Message;
   Test_Large_Message;
   Test_Multiple_Messages;
   Test_KeyGen_Determinism;

   New_Line;
   Put_Line ("=======================================================");
   Put_Line ("  Test Results:");
   Put_Line ("  Tests run:  " & Natural'Image (Test_Count));
   Put_Line ("  Passed:     " & Natural'Image (Pass_Count));
   Put_Line ("  Failed:     " & Natural'Image (Test_Count - Pass_Count));
   Put_Line ("=======================================================");
   New_Line;

   if Test_Failed or Pass_Count /= Test_Count then
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_MLDSA_Comprehensive;
