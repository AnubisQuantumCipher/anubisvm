--  AegisVM FFI Integration Tests
--
--  Tests for the C FFI interface to verify correct behavior
--  of VM creation, execution, state management, and cryptographic
--  functions.

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;

with Aegis_FFI;             use Aegis_FFI;

procedure Test_Aegis_FFI is

   ---------------------------------------------------------------------------
   --  Test Result Tracking
   ---------------------------------------------------------------------------

   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;

   procedure Report (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Failed_Tests := Failed_Tests + 1;
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Report;

   ---------------------------------------------------------------------------
   --  Library Initialization Tests
   ---------------------------------------------------------------------------

   procedure Test_Library_Init is
      Result : Result_Code;
   begin
      Put_Line ("Testing library initialization...");

      --  Test aegis_init
      Result := Aegis_Init;
      Report ("aegis_init returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));

      --  Test double init (should still succeed)
      Result := Aegis_Init;
      Report ("aegis_init idempotent",
              Result = Result_Code (AEGIS_OK));

      --  Test version
      declare
         Version : constant Interfaces.C.Strings.chars_ptr := Aegis_Version;
      begin
         Report ("aegis_version returns non-null",
                 Version /= Interfaces.C.Strings.Null_Ptr);
      end;
   end Test_Library_Init;

   ---------------------------------------------------------------------------
   --  VM Context Tests
   ---------------------------------------------------------------------------

   procedure Test_VM_Context is
      Handle : aliased VM_Handle;
      Result : Result_Code;
   begin
      Put_Line ("Testing VM context management...");

      --  Test simple create
      Result := Aegis_VM_Create (Handle'Access);
      Report ("aegis_vm_create returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));
      Report ("aegis_vm_create sets handle",
              Handle /= null);

      --  Test destroy
      if Handle /= null then
         Result := Aegis_VM_Destroy (Handle);
         Report ("aegis_vm_destroy returns AEGIS_OK",
                 Result = Result_Code (AEGIS_OK));
      end if;

      --  Test create with options
      Result := Aegis_VM_Create_With_Options (
         Handle'Access,
         10_000_000,  -- 10M gas
         Cert_Level (AEGIS_CERT_GOLD));
      Report ("aegis_vm_create_with_options returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));

      --  Test gas remaining
      if Handle /= null then
         declare
            Gas : aliased Interfaces.C.unsigned_long;
         begin
            Result := Aegis_VM_Gas_Remaining (Handle, Gas'Access);
            Report ("aegis_vm_gas_remaining returns AEGIS_OK",
                    Result = Result_Code (AEGIS_OK));
            Report ("aegis_vm_gas_remaining returns 10M",
                    Gas = 10_000_000);
         end;

         --  Test reset
         Result := Aegis_VM_Reset (Handle);
         Report ("aegis_vm_reset returns AEGIS_OK",
                 Result = Result_Code (AEGIS_OK));

         --  Cleanup
         Result := Aegis_VM_Destroy (Handle);
      end if;
   end Test_VM_Context;

   ---------------------------------------------------------------------------
   --  Bytecode Loading Tests
   ---------------------------------------------------------------------------

   procedure Test_Bytecode_Loading is
      Handle : aliased VM_Handle;
      Result : Result_Code;
      Sample_Code : aliased constant Byte_Array (0 .. 15) :=
        (16#00#, 16#01#, 16#02#, 16#03#,
         16#04#, 16#05#, 16#06#, 16#07#,
         16#08#, 16#09#, 16#0A#, 16#0B#,
         16#0C#, 16#0D#, 16#0E#, 16#0F#);
   begin
      Put_Line ("Testing bytecode loading...");

      --  Create VM
      Result := Aegis_VM_Create (Handle'Access);
      if Result /= Result_Code (AEGIS_OK) or Handle = null then
         Report ("Failed to create VM for bytecode test", False);
         return;
      end if;

      --  Test validate bytecode
      Result := Aegis_Validate_Bytecode (
         Sample_Code'Address,
         Sample_Code'Length);
      Report ("aegis_validate_bytecode returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));

      --  Test load bytecode
      Result := Aegis_VM_Load_Code (
         Handle,
         Sample_Code'Address,
         Sample_Code'Length);
      Report ("aegis_vm_load_code returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));

      --  Test empty bytecode (should fail)
      Result := Aegis_VM_Load_Code (
         Handle,
         System.Null_Address,
         0);
      Report ("aegis_vm_load_code rejects empty code",
              Result = Result_Code (AEGIS_ERROR_INVALID_INPUT));

      --  Cleanup
      Result := Aegis_VM_Destroy (Handle);
   end Test_Bytecode_Loading;

   ---------------------------------------------------------------------------
   --  State Management Tests
   ---------------------------------------------------------------------------

   procedure Test_State_Management is
      Handle : aliased VM_Handle;
      Result : Result_Code;
      Test_Key   : aliased constant Byte_Array (0 .. 3) := (16#01#, 16#02#, 16#03#, 16#04#);
      Test_Value : aliased constant Byte_Array (0 .. 7) := (16#AA#, 16#BB#, 16#CC#, 16#DD#,
                                                            16#EE#, 16#FF#, 16#00#, 16#11#);
      Read_Value : aliased Byte_Array (0 .. 31) := (others => 0);
      Value_Len  : aliased Interfaces.C.size_t;
   begin
      Put_Line ("Testing state management...");

      --  Create VM
      Result := Aegis_VM_Create (Handle'Access);
      if Result /= Result_Code (AEGIS_OK) or Handle = null then
         Report ("Failed to create VM for state test", False);
         return;
      end if;

      --  Test set state
      Result := Aegis_VM_Set_State (
         Handle,
         Test_Key'Address,
         Test_Key'Length,
         Test_Value'Address,
         Test_Value'Length);
      Report ("aegis_vm_set_state returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));

      --  Test get state
      Result := Aegis_VM_Get_State (
         Handle,
         Test_Key'Address,
         Test_Key'Length,
         Read_Value'Address,
         Value_Len'Access);
      Report ("aegis_vm_get_state returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));
      Report ("aegis_vm_get_state returns correct length",
              Value_Len = Test_Value'Length);

      --  Verify value
      declare
         Match : Boolean := True;
      begin
         for I in 0 .. Natural (Value_Len) - 1 loop
            if Read_Value (I) /= Test_Value (I) then
               Match := False;
               exit;
            end if;
         end loop;
         Report ("aegis_vm_get_state returns correct value", Match);
      end;

      --  Test get non-existent key
      declare
         Other_Key : aliased constant Byte_Array (0 .. 3) := (16#FF#, 16#FF#, 16#FF#, 16#FF#);
      begin
         Result := Aegis_VM_Get_State (
            Handle,
            Other_Key'Address,
            Other_Key'Length,
            Read_Value'Address,
            Value_Len'Access);
         Report ("aegis_vm_get_state returns AEGIS_OK for missing key",
                 Result = Result_Code (AEGIS_OK));
         Report ("aegis_vm_get_state returns zero length for missing key",
                 Value_Len = 0);
      end;

      --  Cleanup
      Result := Aegis_VM_Destroy (Handle);
   end Test_State_Management;

   ---------------------------------------------------------------------------
   --  Gas Estimation Tests
   ---------------------------------------------------------------------------

   procedure Test_Gas_Estimation is
      Sample_Code : aliased constant Byte_Array (0 .. 99) := (others => 16#00#);
      Estimate    : aliased Interfaces.C.unsigned_long;
      Result      : Result_Code;
   begin
      Put_Line ("Testing gas estimation...");

      Result := Aegis_Estimate_Gas (
         Sample_Code'Address,
         Sample_Code'Length,
         System.Null_Address,
         0,
         System.Null_Address,
         0,
         Estimate'Access);
      Report ("aegis_estimate_gas returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));
      Report ("aegis_estimate_gas returns non-zero estimate",
              Estimate > 0);

      --  Expected: 21000 + (100 * 16) = 22600
      Report ("aegis_estimate_gas returns expected value",
              Estimate = 22600);
   end Test_Gas_Estimation;

   ---------------------------------------------------------------------------
   --  Hash Function Tests
   ---------------------------------------------------------------------------

   procedure Test_Hash_Functions is
      Input  : aliased constant Byte_Array (0 .. 4) := (16#48#, 16#65#, 16#6C#, 16#6C#, 16#6F#);  -- "Hello"
      Output256 : aliased Hash256 := (others => 0);
      Output512 : aliased Hash512 := (others => 0);
      Result : Result_Code;
   begin
      Put_Line ("Testing hash functions...");

      --  Test SHA3-256
      Result := Aegis_SHA3_256 (Output256'Address, Input'Address, Input'Length);
      Report ("aegis_sha3_256 returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));

      --  Verify output is not all zeros
      declare
         All_Zero : Boolean := True;
      begin
         for I in Output256'Range loop
            if Output256 (I) /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;
         Report ("aegis_sha3_256 produces non-zero output", not All_Zero);
      end;

      --  Test SHA3-512
      Result := Aegis_SHA3_512 (Output512'Address, Input'Address, Input'Length);
      Report ("aegis_sha3_512 returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));

      --  Verify output is not all zeros
      declare
         All_Zero : Boolean := True;
      begin
         for I in Output512'Range loop
            if Output512 (I) /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;
         Report ("aegis_sha3_512 produces non-zero output", not All_Zero);
      end;

      --  Test Keccak-256
      Result := Aegis_Keccak256 (Output256'Address, Input'Address, Input'Length);
      Report ("aegis_keccak256 returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));
   end Test_Hash_Functions;

   ---------------------------------------------------------------------------
   --  Gas Discount Tests
   ---------------------------------------------------------------------------

   procedure Test_Gas_Discount is
      Base_Gas : constant Interfaces.C.unsigned_long := 100_000;
      Result_Gas : aliased Interfaces.C.unsigned_long;
      Result     : Result_Code;
   begin
      Put_Line ("Testing gas discount...");

      --  Test Bronze (0% discount)
      Result := Aegis_Apply_Gas_Discount (Base_Gas, Cert_Level (AEGIS_CERT_BRONZE), Result_Gas'Access);
      Report ("aegis_apply_gas_discount bronze returns AEGIS_OK",
              Result = Result_Code (AEGIS_OK));
      Report ("aegis_apply_gas_discount bronze = 100%",
              Result_Gas = 100_000);

      --  Test Silver (10% discount)
      Result := Aegis_Apply_Gas_Discount (Base_Gas, Cert_Level (AEGIS_CERT_SILVER), Result_Gas'Access);
      Report ("aegis_apply_gas_discount silver = 90%",
              Result_Gas = 90_000);

      --  Test Gold (20% discount)
      Result := Aegis_Apply_Gas_Discount (Base_Gas, Cert_Level (AEGIS_CERT_GOLD), Result_Gas'Access);
      Report ("aegis_apply_gas_discount gold = 80%",
              Result_Gas = 80_000);

      --  Test Platinum (30% discount)
      Result := Aegis_Apply_Gas_Discount (Base_Gas, Cert_Level (AEGIS_CERT_PLATINUM), Result_Gas'Access);
      Report ("aegis_apply_gas_discount platinum = 70%",
              Result_Gas = 70_000);
   end Test_Gas_Discount;

   ---------------------------------------------------------------------------
   --  Utility Function Tests
   ---------------------------------------------------------------------------

   procedure Test_Utility_Functions is
      Buf_A : aliased Byte_Array (0 .. 7) := (16#01#, 16#02#, 16#03#, 16#04#,
                                              16#05#, 16#06#, 16#07#, 16#08#);
      Buf_B : aliased Byte_Array (0 .. 7) := (16#01#, 16#02#, 16#03#, 16#04#,
                                              16#05#, 16#06#, 16#07#, 16#08#);
      Buf_C : aliased Byte_Array (0 .. 7) := (16#FF#, 16#FF#, 16#FF#, 16#FF#,
                                              16#FF#, 16#FF#, 16#FF#, 16#FF#);
      Cmp_Result : Interfaces.C.int;
   begin
      Put_Line ("Testing utility functions...");

      --  Test constant-time compare (equal)
      Cmp_Result := Aegis_Constant_Time_Compare (Buf_A'Address, Buf_B'Address, Buf_A'Length);
      Report ("aegis_constant_time_compare equal returns 0",
              Cmp_Result = 0);

      --  Test constant-time compare (not equal)
      Cmp_Result := Aegis_Constant_Time_Compare (Buf_A'Address, Buf_C'Address, Buf_A'Length);
      Report ("aegis_constant_time_compare not equal returns non-zero",
              Cmp_Result /= 0);

      --  Test secure zero
      Aegis_Secure_Zero (Buf_A'Address, Buf_A'Length);
      declare
         All_Zero : Boolean := True;
      begin
         for I in Buf_A'Range loop
            if Buf_A (I) /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;
         Report ("aegis_secure_zero clears buffer", All_Zero);
      end;
   end Test_Utility_Functions;

   ---------------------------------------------------------------------------
   --  Main
   ---------------------------------------------------------------------------

begin
   Put_Line ("===========================================");
   Put_Line ("  AegisVM FFI Integration Tests");
   Put_Line ("===========================================");
   New_Line;

   --  Run all tests
   Test_Library_Init;
   New_Line;

   Test_VM_Context;
   New_Line;

   Test_Bytecode_Loading;
   New_Line;

   Test_State_Management;
   New_Line;

   Test_Gas_Estimation;
   New_Line;

   Test_Hash_Functions;
   New_Line;

   Test_Gas_Discount;
   New_Line;

   Test_Utility_Functions;
   New_Line;

   --  Cleanup
   Aegis_Cleanup;

   --  Summary
   Put_Line ("===========================================");
   Put_Line ("  Test Summary");
   Put_Line ("===========================================");
   Put_Line ("  Total:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed: " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed: " & Natural'Image (Failed_Tests));
   New_Line;

   if Failed_Tests > 0 then
      Put_Line ("  SOME TESTS FAILED!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("  ALL TESTS PASSED!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_Aegis_FFI;
