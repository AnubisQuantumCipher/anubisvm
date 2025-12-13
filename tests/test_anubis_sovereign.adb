--  Test AnubisSovereign - Quantum-Resistant Self-Sovereign Identity Contract
--
--  Comprehensive test suite for the AnubisSovereign native contract.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_Native_MacOS;

procedure Test_Anubis_Sovereign is

   use Sphinx_Native_MacOS;

   --  Sample storage callbacks
   procedure Test_Storage_Load (
      Slot  : Hash256;
      Value : out Hash256
   ) is
      pragma Unreferenced (Slot);
   begin
      Value := (others => 0);
   end Test_Storage_Load;

   procedure Test_Storage_Store (
      Slot  : Hash256;
      Value : Hash256
   ) is
      pragma Unreferenced (Slot, Value);
   begin
      null;
   end Test_Storage_Store;

   --  Test state
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   procedure Check (Condition : Boolean; Test_Name : String) is
   begin
      if Condition then
         Put_Line ("  [PASS] " & Test_Name);
         Tests_Passed := Tests_Passed + 1;
      else
         Put_Line ("  [FAIL] " & Test_Name);
         Tests_Failed := Tests_Failed + 1;
      end if;
   end Check;

   --  Entry point selectors
   SEL_INITIALIZE      : constant Byte := 16#00#;
   SEL_REGISTER_ISSUER : constant Byte := 16#01#;
   SEL_REGISTER_DID    : constant Byte := 16#03#;
   SEL_ISSUE_CRED      : constant Byte := 16#04#;
   SEL_COMMIT_REP      : constant Byte := 16#18#;
   SEL_VERIFY_REP      : constant Byte := 16#19#;
   SEL_CHECK_KEY_IMAGE : constant Byte := 16#1D#;
   SEL_GET_ISSUER_CNT  : constant Byte := 16#20#;
   SEL_GET_SUBJECT_CNT : constant Byte := 16#21#;
   SEL_GET_CRED_CNT    : constant Byte := 16#22#;
   SEL_GET_VERSION     : constant Byte := 16#26#;
   SEL_GET_OWNER       : constant Byte := 16#25#;
   SEL_IS_ISSUER       : constant Byte := 16#23#;

   function Get_Dylib_Path return String is
   begin
      if Ada.Command_Line.Argument_Count > 0 then
         return Ada.Command_Line.Argument (1);
      else
         return "/Users/sicarii/anubisvm/examples/native_contracts/libanubis_sovereign.dylib";
      end if;
   end Get_Dylib_Path;

   Handle      : Library_Handle;
   Load_OK     : Boolean;
   Exec_OK     : Boolean;
   Dylib_Path  : constant String := Get_Dylib_Path;

   --  Buffers
   Calldata    : Byte_Array (0 .. 127) := (others => 0);
   Return_Data : Byte_Array (0 .. 255) := (others => 0);
   Return_Len  : Natural;
   Gas_Used    : Gas_Amount;

   --  Helper to read LE64
   function Read_LE64 (Buf : Byte_Array; Offset : Natural) return Unsigned_64 is
   begin
      return Unsigned_64 (Buf (Offset)) or
             Shift_Left (Unsigned_64 (Buf (Offset + 1)), 8) or
             Shift_Left (Unsigned_64 (Buf (Offset + 2)), 16) or
             Shift_Left (Unsigned_64 (Buf (Offset + 3)), 24) or
             Shift_Left (Unsigned_64 (Buf (Offset + 4)), 32) or
             Shift_Left (Unsigned_64 (Buf (Offset + 5)), 40) or
             Shift_Left (Unsigned_64 (Buf (Offset + 6)), 48) or
             Shift_Left (Unsigned_64 (Buf (Offset + 7)), 56);
   end Read_LE64;

begin
   Put_Line ("AnubisSovereign Contract Tests");
   Put_Line ("==============================");
   Put_Line ("Quantum-Resistant Self-Sovereign Identity");
   Put_Line ("");
   Put_Line ("Dylib path: " & Dylib_Path);
   Put_Line ("");

   --  Set storage callbacks
   Set_Storage_Callbacks (
      Load_Fn  => Test_Storage_Load'Unrestricted_Access,
      Store_Fn => Test_Storage_Store'Unrestricted_Access
   );

   --  Test 1: Load library
   Put_Line ("Test 1: Load AnubisSovereign contract");
   Load_Library (Dylib_Path, Handle, Load_OK);
   Check (Load_OK, "Library loaded successfully");
   Check (Handle /= Null_Handle, "Handle is valid");

   if not Load_OK then
      Put_Line ("Cannot continue - library load failed");
      return;
   end if;

   --  Test 2: Get version (should be 0 before initialization)
   Put_Line ("Test 2: Get initial version");
   Calldata (0) := SEL_GET_VERSION;
   Execute_Contract (Handle, Calldata (0 .. 0), 100_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Get version succeeded");
   Check (Return_Len = 8, "Returns 8 bytes");
   declare
      Version : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Version:" & Unsigned_64'Image (Version));
      Check (Version = 0, "Version is 0 (uninitialized)");
   end;

   --  Test 3: Initialize contract
   Put_Line ("Test 3: Initialize contract");
   Calldata (0) := SEL_INITIALIZE;
   --  Owner address (32 bytes of 0xAB)
   for I in 1 .. 32 loop
      Calldata (I) := 16#AB#;
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 32), 1_000_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Initialize succeeded");
   Put_Line ("  Gas used:" & Gas_Amount'Image (Gas_Used));

   --  Test 4: Verify version after init
   Put_Line ("Test 4: Verify version after initialization");
   Calldata (0) := SEL_GET_VERSION;
   Execute_Contract (Handle, Calldata (0 .. 0), 100_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Get version succeeded");
   declare
      Version : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Version:" & Unsigned_64'Image (Version));
      Check (Version = 1, "Version is now 1");
   end;

   --  Test 5: Get owner
   Put_Line ("Test 5: Get owner address");
   Calldata (0) := SEL_GET_OWNER;
   Execute_Contract (Handle, Calldata (0 .. 0), 100_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Get owner succeeded");
   Check (Return_Len = 32, "Returns 32 bytes");
   Check (Return_Data (0) = 16#AB#, "Owner first byte is 0xAB");

   --  Test 6: Register an issuer
   Put_Line ("Test 6: Register issuer");
   Calldata (0) := SEL_REGISTER_ISSUER;
   for I in 1 .. 32 loop
      Calldata (I) := 16#11#;
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 32), 500_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Register issuer succeeded");
   declare
      Issuer_Idx : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Issuer index:" & Unsigned_64'Image (Issuer_Idx));
      Check (Issuer_Idx = 0, "First issuer has index 0");
   end;
   Put_Line ("  Gas used:" & Gas_Amount'Image (Gas_Used));

   --  Test 7: Get issuer count
   Put_Line ("Test 7: Get issuer count");
   Calldata (0) := SEL_GET_ISSUER_CNT;
   Execute_Contract (Handle, Calldata (0 .. 0), 100_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Get issuer count succeeded");
   declare
      Count : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Issuer count:" & Unsigned_64'Image (Count));
      Check (Count = 1, "Issuer count is 1");
   end;

   --  Test 8: Check if issuer is registered
   Put_Line ("Test 8: Check if issuer is registered");
   Calldata (0) := SEL_IS_ISSUER;
   for I in 1 .. 32 loop
      Calldata (I) := 16#11#;
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 32), 500_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Is issuer check succeeded");
   declare
      Is_Issuer : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Is issuer:" & Unsigned_64'Image (Is_Issuer));
      Check (Is_Issuer = 1, "Issuer is registered");
   end;

   --  Test 9: Register a DID (SHA3 hash)
   Put_Line ("Test 9: Register DID (SHA3 hash of public key)");
   Calldata (0) := SEL_REGISTER_DID;
   for I in 1 .. 32 loop
      Calldata (I) := 16#22#;
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 32), 500_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Register DID succeeded");
   Check (Return_Len = 32, "Returns 32-byte DID");
   Put_Line ("  DID (first 4 bytes):" &
      Byte'Image (Return_Data (0)) &
      Byte'Image (Return_Data (1)) &
      Byte'Image (Return_Data (2)) &
      Byte'Image (Return_Data (3)));
   Put_Line ("  Gas used:" & Gas_Amount'Image (Gas_Used));

   --  Test 10: Get subject count
   Put_Line ("Test 10: Get subject count");
   Calldata (0) := SEL_GET_SUBJECT_CNT;
   Execute_Contract (Handle, Calldata (0 .. 0), 100_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Get subject count succeeded");
   declare
      Count : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Subject count:" & Unsigned_64'Image (Count));
      Check (Count = 1, "Subject count is 1");
   end;

   --  Test 11: Issue a credential
   Put_Line ("Test 11: Issue credential");
   Calldata (0) := SEL_ISSUE_CRED;
   for I in 1 .. 64 loop
      Calldata (I) := Byte (I mod 256);
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 64), 500_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Issue credential succeeded");
   Check (Return_Len = 32, "Returns 32-byte credential hash");
   Put_Line ("  Gas used:" & Gas_Amount'Image (Gas_Used));

   --  Test 12: Get credential count
   Put_Line ("Test 12: Get credential count");
   Calldata (0) := SEL_GET_CRED_CNT;
   Execute_Contract (Handle, Calldata (0 .. 0), 100_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Get credential count succeeded");
   declare
      Count : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Credential count:" & Unsigned_64'Image (Count));
      Check (Count = 1, "Credential count is 1");
   end;

   --  Test 13: WHISPER - Commit reputation
   Put_Line ("Test 13: WHISPER - Commit reputation (Ajtai commitment)");
   Calldata (0) := SEL_COMMIT_REP;
   Calldata (1) := 0;  -- Subject index
   --  Score = 85
   Calldata (2) := 85;
   for I in 3 .. 9 loop
      Calldata (I) := 0;
   end loop;
   --  Randomness
   for I in 10 .. 41 loop
      Calldata (I) := 16#33#;
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 41), 500_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Commit reputation succeeded");
   Check (Return_Len = 32, "Returns 32-byte commitment");
   Put_Line ("  Gas used:" & Gas_Amount'Image (Gas_Used));

   --  Test 14: WHISPER - Verify range
   Put_Line ("Test 14: WHISPER - Verify reputation in range [50, 100]");
   Calldata (0) := SEL_VERIFY_REP;
   Calldata (1) := 0;
   Calldata (2) := 85;
   for I in 3 .. 9 loop
      Calldata (I) := 0;
   end loop;
   for I in 10 .. 41 loop
      Calldata (I) := 16#33#;
   end loop;
   --  Min = 50
   Calldata (42) := 50;
   for I in 43 .. 49 loop
      Calldata (I) := 0;
   end loop;
   --  Max = 100
   Calldata (50) := 100;
   for I in 51 .. 57 loop
      Calldata (I) := 0;
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 57), 500_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Verify range succeeded");
   declare
      In_Range : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  In range:" & Unsigned_64'Image (In_Range));
      Check (In_Range = 1, "Score 85 is in range [50, 100]");
   end;

   --  Test 15: VEIL - Check key image
   Put_Line ("Test 15: VEIL - Check key image (double-spend prevention)");
   Calldata (0) := SEL_CHECK_KEY_IMAGE;
   for I in 1 .. 32 loop
      Calldata (I) := 16#44#;
   end loop;
   Execute_Contract (Handle, Calldata (0 .. 32), 500_000, Return_Data, Return_Len, Gas_Used, Exec_OK);
   Check (Exec_OK, "Check key image succeeded");
   declare
      Is_Valid : constant Unsigned_64 := Read_LE64 (Return_Data, 0);
   begin
      Put_Line ("  Is valid (unspent):" & Unsigned_64'Image (Is_Valid));
      Check (Is_Valid = 1, "Key image is unspent");
   end;

   --  Test 16: Unload library
   Put_Line ("Test 16: Unload library");
   Unload_Library (Handle);
   Check (Handle = Null_Handle, "Handle cleared after unload");

   --  Summary
   Put_Line ("");
   Put_Line ("==============================");
   Put_Line ("Tests Passed:" & Natural'Image (Tests_Passed));
   Put_Line ("Tests Failed:" & Natural'Image (Tests_Failed));
   Put_Line ("");
   Put_Line ("Features Tested:");
   Put_Line ("  - THOTH: Storage slots (issuers, subjects, credentials)");
   Put_Line ("  - ANKH: SHA3-256 hashing (DID generation, credential hashing)");
   Put_Line ("  - WHISPER: Ajtai commitments (confidential reputation)");
   Put_Line ("  - VEIL: Key image checking (double-spend prevention)");
   Put_Line ("");

   if Tests_Failed = 0 then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;

end Test_Anubis_Sovereign;
