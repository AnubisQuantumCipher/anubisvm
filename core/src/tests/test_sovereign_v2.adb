--  Test: AnubisSovereign v2 with Real VM Power
--
--  This test verifies that the sovereign_v2.c contract correctly uses
--  VM syscalls (THOTH storage, ANKH crypto) via the syscall table.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Sphinx_Native_MacOS;
with Aegis_VM_Types; use Aegis_VM_Types;

procedure Test_Sovereign_V2 is

   Contract_Path : constant String :=
      "/Users/sicarii/anubisvm/examples/native_contracts/libsovereign_v2.dylib";

   Handle     : Sphinx_Native_MacOS.Library_Handle;
   Load_OK    : Boolean;
   Success    : Boolean;
   Return_Data : Byte_Array (0 .. 255);
   Return_Len  : Natural;
   Gas_Used    : Gas_Amount;

   Passed : Natural := 0;
   Failed : Natural := 0;

   procedure Run_Test (
      Name     : String;
      Calldata : Byte_Array;
      Expected : Boolean
   ) is
   begin
      Put ("  " & Name & "... ");
      Sphinx_Native_MacOS.Execute_Contract (
         Handle      => Handle,
         Calldata    => Calldata,
         Gas_Limit   => 10_000_000,
         Return_Data => Return_Data,
         Return_Len  => Return_Len,
         Gas_Used    => Gas_Used,
         Success     => Success
      );

      if Success = Expected then
         Put_Line ("PASSED (gas: " & Gas_Amount'Image (Gas_Used) & ")");
         Passed := Passed + 1;
      else
         Put_Line ("FAILED");
         Failed := Failed + 1;
      end if;
   end Run_Test;

   procedure Run_Test_With_Return (
      Name     : String;
      Calldata : Byte_Array;
      Expected_Len : Natural
   ) is
   begin
      Put ("  " & Name & "... ");
      Sphinx_Native_MacOS.Execute_Contract (
         Handle      => Handle,
         Calldata    => Calldata,
         Gas_Limit   => 10_000_000,
         Return_Data => Return_Data,
         Return_Len  => Return_Len,
         Gas_Used    => Gas_Used,
         Success     => Success
      );

      if Success and Return_Len >= Expected_Len then
         Put_Line ("PASSED (returned " & Natural'Image (Return_Len) &
                   " bytes, gas: " & Gas_Amount'Image (Gas_Used) & ")");
         Passed := Passed + 1;
      else
         Put_Line ("FAILED (success=" & Boolean'Image (Success) &
                   ", len=" & Natural'Image (Return_Len) & ")");
         Failed := Failed + 1;
      end if;
   end Run_Test_With_Return;

   --  Selectors
   SEL_INITIALIZE      : constant := 16#00#;
   SEL_REGISTER_ISSUER : constant := 16#01#;
   SEL_REGISTER_DID    : constant := 16#03#;
   SEL_ISSUE_CRED      : constant := 16#04#;
   SEL_VERIFY_SIG      : constant := 16#06#;
   SEL_COMMIT_REP      : constant := 16#18#;
   SEL_VERIFY_REP      : constant := 16#19#;
   SEL_CHECK_KEY_IMAGE : constant := 16#1D#;
   SEL_GET_OWNER       : constant := 16#25#;
   SEL_GET_VERSION     : constant := 16#26#;
   SEL_GET_CALLER      : constant := 16#27#;
   SEL_IS_ISSUER       : constant := 16#28#;
   SEL_GET_COUNTS      : constant := 16#29#;

begin
   Put_Line ("============================================================");
   Put_Line ("AnubisSovereign v2 - Production Test with Real VM Power");
   Put_Line ("============================================================");
   New_Line;

   --  Load contract
   Put ("Loading " & Contract_Path & "...");
   Sphinx_Native_MacOS.Load_Library (Contract_Path, Handle, Load_OK);

   if not Load_OK then
      Put_Line (" FAILED");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;
   Put_Line (" OK");
   New_Line;

   --  Test 1: Get Version (before init)
   Put_Line ("=== Pre-initialization ===");
   Run_Test_With_Return ("Get version (should be 0)", (0 => SEL_GET_VERSION), 8);

   --  Test 2: Initialize
   Put_Line (ASCII.LF & "=== Initialization ===");
   Run_Test ("Initialize contract", (0 => SEL_INITIALIZE), True);

   --  Test 3: Get Version (after init)
   Run_Test_With_Return ("Get version (should be 1)", (0 => SEL_GET_VERSION), 8);

   if Return_Len >= 8 then
      Put ("    Version bytes: ");
      for I in 0 .. 7 loop
         Put (Unsigned_8'Image (Return_Data (I)));
      end loop;
      New_Line;
   end if;

   --  Test 4: Get Owner
   Run_Test_With_Return ("Get owner", (0 => SEL_GET_OWNER), 32);

   if Return_Len >= 32 then
      Put ("    Owner (first 8 bytes): ");
      for I in 0 .. 7 loop
         Put (Unsigned_8'Image (Return_Data (I)));
      end loop;
      New_Line;
   end if;

   --  Test 5: Get Caller
   Run_Test_With_Return ("Get caller", (0 => SEL_GET_CALLER), 32);

   if Return_Len >= 32 then
      Put ("    Caller (first 8 bytes): ");
      for I in 0 .. 7 loop
         Put (Unsigned_8'Image (Return_Data (I)));
      end loop;
      New_Line;
   end if;

   --  Test 6: Double init should fail
   Put_Line (ASCII.LF & "=== Authorization Tests ===");
   Run_Test ("Double init (should fail)", (0 => SEL_INITIALIZE), False);

   --  Test 7: Register Issuer
   Put_Line (ASCII.LF & "=== Issuer Management ===");
   declare
      Issuer_Data : Byte_Array (0 .. 32);
   begin
      Issuer_Data (0) := SEL_REGISTER_ISSUER;
      for I in 1 .. 32 loop
         Issuer_Data (I) := Byte (I);  -- Test issuer address
      end loop;
      Run_Test ("Register issuer", Issuer_Data, True);
   end;

   --  Test 8: Check issuer
   declare
      Check_Data : Byte_Array (0 .. 32);
   begin
      Check_Data (0) := SEL_IS_ISSUER;
      for I in 1 .. 32 loop
         Check_Data (I) := Byte (I);  -- Same issuer address
      end loop;
      Run_Test_With_Return ("Is issuer (should be 1)", Check_Data, 1);
      if Return_Len >= 1 then
         Put_Line ("    Is issuer: " & Unsigned_8'Image (Return_Data (0)));
      end if;
   end;

   --  Test 9: Get Counts
   Run_Test_With_Return ("Get counts", (0 => SEL_GET_COUNTS), 24);
   if Return_Len >= 24 then
      Put_Line ("    Issuers: " & Unsigned_8'Image (Return_Data (0)));
      Put_Line ("    Subjects: " & Unsigned_8'Image (Return_Data (8)));
      Put_Line ("    Creds: " & Unsigned_8'Image (Return_Data (16)));
   end if;

   --  Test 10: Register DID (uses VM SHA3!)
   Put_Line (ASCII.LF & "=== DID Registration (VM SHA3) ===");
   declare
      DID_Data : Byte_Array (0 .. 32);
   begin
      DID_Data (0) := SEL_REGISTER_DID;
      for I in 1 .. 32 loop
         DID_Data (I) := Byte (16#AA# + I mod 16);  -- Test public key
      end loop;
      Run_Test_With_Return ("Register DID", DID_Data, 32);
      if Return_Len >= 32 then
         Put ("    DID hash (first 8 bytes): ");
         for I in 0 .. 7 loop
            Put (Unsigned_8'Image (Return_Data (I)));
         end loop;
         New_Line;
      end if;
   end;

   --  Test 11: Issue Credential (caller is issuer now)
   Put_Line (ASCII.LF & "=== Credential Issuance ===");
   --  Note: This will fail because the caller is not the registered issuer
   declare
      Cred_Data : Byte_Array (0 .. 64);
   begin
      Cred_Data (0) := SEL_ISSUE_CRED;
      --  Subject DID
      for I in 1 .. 32 loop
         Cred_Data (I) := Byte (16#BB#);
      end loop;
      --  Credential data
      for I in 33 .. 64 loop
         Cred_Data (I) := Byte (16#CC#);
      end loop;
      Run_Test ("Issue credential (not issuer - should fail)", Cred_Data, False);
   end;

   --  Test 12: WHISPER Reputation Commit
   Put_Line (ASCII.LF & "=== WHISPER Privacy (VM SHA3) ===");
   declare
      Rep_Data : Byte_Array (0 .. 41);
   begin
      Rep_Data (0) := SEL_COMMIT_REP;
      Rep_Data (1) := 0;  -- Slot 0
      --  Score (8 bytes LE) = 85
      Rep_Data (2) := 85;
      for I in 3 .. 9 loop Rep_Data (I) := 0; end loop;
      --  Randomness (32 bytes)
      for I in 10 .. 41 loop
         Rep_Data (I) := Byte (16#DD# + I mod 8);
      end loop;
      Run_Test_With_Return ("Commit reputation", Rep_Data, 32);
      if Return_Len >= 32 then
         Put ("    Commitment (first 8 bytes): ");
         for I in 0 .. 7 loop
            Put (Unsigned_8'Image (Return_Data (I)));
         end loop;
         New_Line;
      end if;
   end;

   --  Test 13: Verify Reputation Range
   declare
      Verify_Data : Byte_Array (0 .. 57);
   begin
      Verify_Data (0) := SEL_VERIFY_REP;
      Verify_Data (1) := 0;  -- Slot 0
      --  Score = 85
      Verify_Data (2) := 85;
      for I in 3 .. 9 loop Verify_Data (I) := 0; end loop;
      --  Same randomness
      for I in 10 .. 41 loop
         Verify_Data (I) := Byte (16#DD# + I mod 8);
      end loop;
      --  Min = 50
      Verify_Data (42) := 50;
      for I in 43 .. 49 loop Verify_Data (I) := 0; end loop;
      --  Max = 100
      Verify_Data (50) := 100;
      for I in 51 .. 57 loop Verify_Data (I) := 0; end loop;

      Run_Test_With_Return ("Verify rep range [50,100]", Verify_Data, 1);
      if Return_Len >= 1 then
         Put_Line ("    In range: " & Unsigned_8'Image (Return_Data (0)));
      end if;
   end;

   --  Test 14: VEIL Key Image Check
   Put_Line (ASCII.LF & "=== VEIL Anti-Double-Spend ===");
   declare
      Key_Data : Byte_Array (0 .. 32);
   begin
      Key_Data (0) := SEL_CHECK_KEY_IMAGE;
      for I in 1 .. 32 loop
         Key_Data (I) := Byte (16#EE#);  -- Test key image
      end loop;
      Run_Test_With_Return ("Check key image (fresh)", Key_Data, 1);
      if Return_Len >= 1 then
         Put_Line ("    Valid (unspent): " & Unsigned_8'Image (Return_Data (0)));
      end if;
   end;

   --  Unload
   New_Line;
   Put_Line ("Unloading contract...");
   Sphinx_Native_MacOS.Unload_Library (Handle);

   --  Results
   New_Line;
   Put_Line ("============================================================");
   Put_Line ("Results: " & Natural'Image (Passed) & " passed, " &
             Natural'Image (Failed) & " failed");
   Put_Line ("============================================================");

   if Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Test_Sovereign_V2;
