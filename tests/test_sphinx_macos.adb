--  Test SPHINX Native macOS Execution
--
--  Tests dylib loading and contract execution.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_Native_MacOS;

procedure Test_Sphinx_MacOS is

   use Sphinx_Native_MacOS;

   --  Sample storage callbacks (for testing)
   procedure Test_Storage_Load (
      Slot  : Hash256;
      Value : out Hash256
   ) is
      pragma Unreferenced (Slot);
   begin
      --  Return zeros for test
      Value := (others => 0);
   end Test_Storage_Load;

   procedure Test_Storage_Store (
      Slot  : Hash256;
      Value : Hash256
   ) is
      pragma Unreferenced (Slot, Value);
   begin
      --  No-op for test
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

   --  Get dylib path from command line or use default
   function Get_Dylib_Path return String is
   begin
      if Ada.Command_Line.Argument_Count > 0 then
         return Ada.Command_Line.Argument (1);
      else
         return "/Users/sicarii/anubisvm/examples/native_contracts/libcounter.dylib";
      end if;
   end Get_Dylib_Path;

   Handle      : Library_Handle;
   Load_OK     : Boolean;
   Exec_OK     : Boolean;
   Dylib_Path  : constant String := Get_Dylib_Path;

   --  Calldata and return buffers
   Calldata    : Byte_Array (0 .. 15) := (others => 0);
   Return_Data : Byte_Array (0 .. 255) := (others => 0);
   Return_Len  : Natural;
   Gas_Used    : Gas_Amount;

begin
   Put_Line ("SPHINX Native macOS Execution Tests");
   Put_Line ("====================================");
   Put_Line ("");
   Put_Line ("Dylib path: " & Dylib_Path);
   Put_Line ("");

   --  Set storage callbacks
   Set_Storage_Callbacks (
      Load_Fn  => Test_Storage_Load'Unrestricted_Access,
      Store_Fn => Test_Storage_Store'Unrestricted_Access
   );

   --  Test 1: Load library
   Put_Line ("Test 1: Load contract dylib");
   Load_Library (Dylib_Path, Handle, Load_OK);
   Check (Load_OK, "Library loaded successfully");
   Check (Handle /= Null_Handle, "Handle is valid");

   if not Load_OK then
      Put_Line ("Cannot continue - library load failed");
      Put_Line ("");
      Put_Line ("Tests Passed:" & Natural'Image (Tests_Passed));
      Put_Line ("Tests Failed:" & Natural'Image (Tests_Failed));
      return;
   end if;

   --  Test 2: Get counter value (entry point 0x00)
   Put_Line ("Test 2: Get initial counter value");
   Calldata (0) := 16#00#;  -- Get counter
   Execute_Contract (
      Handle      => Handle,
      Calldata    => Calldata (0 .. 0),
      Gas_Limit   => 10_000,
      Return_Data => Return_Data,
      Return_Len  => Return_Len,
      Gas_Used    => Gas_Used,
      Success     => Exec_OK
   );
   Check (Exec_OK, "Get counter succeeded");
   Check (Return_Len = 8, "Return length is 8 bytes");
   Check (Gas_Used > 0, "Gas was consumed");
   Put_Line ("  Gas used:" & Gas_Amount'Image (Gas_Used));
   Put_Line ("  Return value: " &
      Unsigned_8'Image (Return_Data (0)) & " " &
      Unsigned_8'Image (Return_Data (1)) & " " &
      Unsigned_8'Image (Return_Data (2)) & " " &
      Unsigned_8'Image (Return_Data (3)));

   --  Test 3: Increment counter (entry point 0x01)
   Put_Line ("Test 3: Increment counter");
   Calldata (0) := 16#01#;  -- Increment
   Execute_Contract (
      Handle      => Handle,
      Calldata    => Calldata (0 .. 0),
      Gas_Limit   => 10_000,
      Return_Data => Return_Data,
      Return_Len  => Return_Len,
      Gas_Used    => Gas_Used,
      Success     => Exec_OK
   );
   Check (Exec_OK, "Increment succeeded");
   Check (Return_Data (0) = 1, "Counter is now 1");
   Put_Line ("  Gas used:" & Gas_Amount'Image (Gas_Used));
   Put_Line ("  New value:" & Unsigned_8'Image (Return_Data (0)));

   --  Test 4: Increment again
   Put_Line ("Test 4: Increment again");
   Execute_Contract (
      Handle      => Handle,
      Calldata    => Calldata (0 .. 0),
      Gas_Limit   => 10_000,
      Return_Data => Return_Data,
      Return_Len  => Return_Len,
      Gas_Used    => Gas_Used,
      Success     => Exec_OK
   );
   Check (Exec_OK, "Second increment succeeded");
   Check (Return_Data (0) = 2, "Counter is now 2");
   Put_Line ("  New value:" & Unsigned_8'Image (Return_Data (0)));

   --  Test 5: Set counter to specific value (entry point 0x02)
   Put_Line ("Test 5: Set counter to 42");
   Calldata (0) := 16#02#;  -- Set counter
   Calldata (1) := 42;       -- Value (LE32)
   Calldata (2) := 0;
   Calldata (3) := 0;
   Calldata (4) := 0;
   Execute_Contract (
      Handle      => Handle,
      Calldata    => Calldata (0 .. 4),
      Gas_Limit   => 10_000,
      Return_Data => Return_Data,
      Return_Len  => Return_Len,
      Gas_Used    => Gas_Used,
      Success     => Exec_OK
   );
   Check (Exec_OK, "Set counter succeeded");
   Check (Return_Data (0) = 42, "Counter is now 42");
   Put_Line ("  New value:" & Unsigned_8'Image (Return_Data (0)));

   --  Test 6: Verify value persists
   Put_Line ("Test 6: Verify counter value");
   Calldata (0) := 16#00#;  -- Get counter
   Execute_Contract (
      Handle      => Handle,
      Calldata    => Calldata (0 .. 0),
      Gas_Limit   => 10_000,
      Return_Data => Return_Data,
      Return_Len  => Return_Len,
      Gas_Used    => Gas_Used,
      Success     => Exec_OK
   );
   Check (Exec_OK, "Get counter succeeded");
   Check (Return_Data (0) = 42, "Counter is still 42");

   --  Test 7: Invalid entry point
   Put_Line ("Test 7: Invalid entry point");
   Calldata (0) := 16#FF#;  -- Invalid
   Execute_Contract (
      Handle      => Handle,
      Calldata    => Calldata (0 .. 0),
      Gas_Limit   => 10_000,
      Return_Data => Return_Data,
      Return_Len  => Return_Len,
      Gas_Used    => Gas_Used,
      Success     => Exec_OK
   );
   Check (not Exec_OK, "Invalid entry point rejected");

   --  Test 8: Unload library
   Put_Line ("Test 8: Unload library");
   Unload_Library (Handle);
   Check (Handle = Null_Handle, "Handle cleared after unload");

   --  Summary
   Put_Line ("");
   Put_Line ("====================================");
   Put_Line ("Tests Passed:" & Natural'Image (Tests_Passed));
   Put_Line ("Tests Failed:" & Natural'Image (Tests_Failed));

   if Tests_Failed = 0 then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;

end Test_Sphinx_MacOS;
