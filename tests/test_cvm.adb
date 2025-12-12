pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces; use Interfaces;

with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;
with CVM_Registry; use CVM_Registry;
with CVM_Dispatch; use CVM_Dispatch;

--  Test_CVM: Integration test suite for CVM Runtime
--
--  Tests CVM types, interfaces, and dispatch without allocating
--  large State_Array or Registry_Array on stack (>64MB each).
--
--  Focuses on:
--  - Type definitions and constants
--  - Method selector computation
--  - Capability checks
--  - Exec result handling
--  - Pack/Unpack serialization

procedure Test_CVM is

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

   --  Test 1: Address comparison (constant-time)
   procedure Test_Address_Compare is
      Addr1 : constant CVM_Address := (others => 16#42#);
      Addr2 : constant CVM_Address := (others => 16#42#);
      Addr3 : constant CVM_Address := (others => 16#24#);
   begin
      Report_Test ("Address equality (same addresses)",
                   Address_Equal (Addr1, Addr2));
      Report_Test ("Address inequality (different addresses)",
                   not Address_Equal (Addr1, Addr3));
   end Test_Address_Compare;

   --  Test 2: Code hash comparison (constant-time)
   procedure Test_Code_Compare is
      Code1 : constant Code_Hash := (others => 16#AA#);
      Code2 : constant Code_Hash := (others => 16#AA#);
      Code3 : constant Code_Hash := (others => 16#BB#);
   begin
      Report_Test ("Code hash equality (same hashes)",
                   Code_Equal (Code1, Code2));
      Report_Test ("Code hash inequality (different hashes)",
                   not Code_Equal (Code1, Code3));
   end Test_Code_Compare;

   --  Test 3: Method selector computation
   procedure Test_Selector_Computation is
      Selector1 : Method_Selector;
      Selector2 : Method_Selector;
      Selector3 : Method_Selector;
   begin
      Compute_Selector ("transfer", Selector1);
      Compute_Selector ("transfer", Selector2);
      Compute_Selector ("balance", Selector3);

      Print_Hex ("  transfer selector: ", Selector1, 8);
      Print_Hex ("  balance selector:  ", Selector3, 8);

      Report_Test ("Selector determinism (same name = same selector)",
                   Arrays_Equal (Selector1, Selector2));
      Report_Test ("Selector uniqueness (different names = different selectors)",
                   not Arrays_Equal (Selector1, Selector3));
   end Test_Selector_Computation;

   --  Test 4: Call context structure
   procedure Test_Context_Structure is
      Context : Call_Context;
   begin
      Context := (
         Caller      => (others => 16#11#),
         Target      => (others => 16#22#),
         Entry_Point => (others => 16#33#),
         Param_Len   => 100,
         Params      => (others => 0),
         Height      => 12345,
         Internal    => True
      );

      Report_Test ("Context Param_Len stores correctly",
                   Context.Param_Len = 100);
      Report_Test ("Context Height stores correctly",
                   Context.Height = 12345);
      Report_Test ("Context Internal flag stores correctly",
                   Context.Internal);
   end Test_Context_Structure;

   --  Test 5: Empty context defaults
   procedure Test_Empty_Context is
   begin
      Report_Test ("Empty_Context has zero param length",
                   Empty_Context.Param_Len = 0);
      Report_Test ("Empty_Context has zero height",
                   Empty_Context.Height = 0);
      Report_Test ("Empty_Context is not internal",
                   not Empty_Context.Internal);
   end Test_Empty_Context;

   --  Test 6: Exec status values
   procedure Test_Exec_Status is
   begin
      Report_Test ("Success status equals Success",
                   Success = Success);
      Report_Test ("Invalid_Caller is not Success",
                   Invalid_Caller /= Success);
      Report_Test ("Unknown_Method is not Success",
                   Unknown_Method /= Success);
   end Test_Exec_Status;

   --  Test 7: Error result creation
   procedure Test_Error_Result is
      Err : Exec_Result;
   begin
      Err := Error_Result (Invalid_Caller);

      Report_Test ("Error_Result has error status",
                   Err.Status = Invalid_Caller);
      Report_Test ("Error_Result has zero return length",
                   Err.Return_Len = 0);
   end Test_Error_Result;

   --  Test 8: Empty result defaults
   procedure Test_Empty_Result is
   begin
      Report_Test ("Empty_Result has Success status",
                   Empty_Result.Status = Success);
      Report_Test ("Empty_Result has zero return length",
                   Empty_Result.Return_Len = 0);
   end Test_Empty_Result;

   --  Test 9: Success result creation
   procedure Test_Success_Result is
      Data : constant Byte_Array (0 .. 3) := (16#DE#, 16#AD#, 16#BE#, 16#EF#);
      Res  : Exec_Result;
   begin
      Res := Success_Result (Data, 4);

      Report_Test ("Success_Result has Success status",
                   Res.Status = Success);
      Report_Test ("Success_Result has correct length",
                   Res.Return_Len = 4);
      Report_Test ("Success_Result first byte is correct",
                   Res.Return_Data (0) = 16#DE#);
      Report_Test ("Success_Result last byte is correct",
                   Res.Return_Data (3) = 16#EF#);
   end Test_Success_Result;

   --  Test 10: State slot defaults
   procedure Test_State_Slot_Defaults is
   begin
      Report_Test ("Empty_Slot length is zero",
                   Empty_Slot.Length = 0);
      Report_Test ("Empty_Slot index is zero",
                   Empty_Slot.Index = 0);
      Report_Test ("Empty_Slot is not modified",
                   not Empty_Slot.Modified);
   end Test_State_Slot_Defaults;

   --  Test 11: Dispatch state initialization
   procedure Test_Dispatch_Init is
   begin
      Report_Test ("Initial dispatch depth is zero",
                   Current_Depth (Initial_Dispatch_State) = 0);
      Report_Test ("Initial dispatch is not active",
                   not Is_Dispatching (Initial_Dispatch_State));
   end Test_Dispatch_Init;

   --  Test 12: CVM capabilities - defaults
   procedure Test_Default_Capabilities is
   begin
      Report_Test ("Default has Cap_Read_State",
                   Has_Capability (Default_Capabilities, Cap_Read_State));
      Report_Test ("Default has Cap_Write_State",
                   Has_Capability (Default_Capabilities, Cap_Write_State));
      Report_Test ("Default has Cap_Call_CVM",
                   Has_Capability (Default_Capabilities, Cap_Call_CVM));
      Report_Test ("Default lacks Cap_Shield_State",
                   not Has_Capability (Default_Capabilities, Cap_Shield_State));
   end Test_Default_Capabilities;

   --  Test 13: CVM capabilities - privacy
   procedure Test_Privacy_Capabilities is
   begin
      Report_Test ("Privacy has Cap_Shield_State",
                   Has_Capability (Privacy_Capabilities, Cap_Shield_State));
      Report_Test ("Privacy has Cap_Eye_View",
                   Has_Capability (Privacy_Capabilities, Cap_Eye_View));
      Report_Test ("Privacy has Cap_Veil_Proof",
                   Has_Capability (Privacy_Capabilities, Cap_Veil_Proof));
   end Test_Privacy_Capabilities;

   --  Test 14: CVM info defaults
   procedure Test_CVM_Info_Defaults is
   begin
      Report_Test ("Empty_Info is not active",
                   not Empty_Info.Active);
      Report_Test ("Empty_Info has default capabilities",
                   Has_Capability (Empty_Info.Caps, Cap_Read_State));
   end Test_CVM_Info_Defaults;

   --  Test 15: Descriptor defaults
   procedure Test_Descriptor_Defaults is
   begin
      Report_Test ("Empty_Descriptor has zero entries",
                   Empty_Descriptor.Entry_Count = 0);
   end Test_Descriptor_Defaults;

   --  Test 16: Entry point defaults
   procedure Test_Entry_Point_Defaults is
   begin
      Report_Test ("Empty_Entry_Point has null handler",
                   Empty_Entry_Point.Handler = null);
      Report_Test ("Empty_Entry_Point is not public",
                   not Empty_Entry_Point.Public);
      Report_Test ("Empty_Entry_Point is read-only",
                   Empty_Entry_Point.ReadOnly);
   end Test_Entry_Point_Defaults;

   --  Test 17: Registration defaults
   procedure Test_Registration_Defaults is
   begin
      Report_Test ("Empty_Registration has null descriptor getter",
                   Empty_Registration.Get_Descriptor = null);
      Report_Test ("Empty_Registration has null init",
                   Empty_Registration.Init = null);
      Report_Test ("Empty_Registration has null execute",
                   Empty_Registration.Execute = null);
   end Test_Registration_Defaults;

   --  Test 18: Registry entry defaults
   procedure Test_Registry_Entry_Defaults is
   begin
      Report_Test ("Empty_Entry is not occupied",
                   not Empty_Entry.Occupied);
      Report_Test ("Empty_Entry has zero exec count",
                   Empty_Entry.Exec_Count = 0);
   end Test_Registry_Entry_Defaults;

   --  Test 19: Pack/Unpack natural serialization
   procedure Test_Pack_Unpack is
      Buffer : Byte_Array (0 .. 7) := (others => 0);
      Value1 : constant Natural := 12345678;
      Value2 : constant Natural := 0;
      Value3 : constant Natural := 255;
      Result : Natural;
   begin
      Pack_Natural (Value1, Buffer, 0);
      Result := Unpack_Natural (Buffer, 0);
      Report_Test ("Pack/Unpack large value",
                   Result = Value1);

      Pack_Natural (Value2, Buffer, 0);
      Result := Unpack_Natural (Buffer, 0);
      Report_Test ("Pack/Unpack zero",
                   Result = Value2);

      Pack_Natural (Value3, Buffer, 0);
      Result := Unpack_Natural (Buffer, 0);
      Report_Test ("Pack/Unpack small value",
                   Result = Value3);
   end Test_Pack_Unpack;

   --  Test 20: Max constants
   procedure Test_Max_Constants is
   begin
      Report_Test ("Max_Call_Depth is 16",
                   Max_Call_Depth = 16);
      Report_Test ("Max_Entry_Points is 32",
                   Max_Entry_Points = 32);
      Report_Test ("Max_CVMs is 64",
                   Max_CVMs = 64);
      Report_Test ("Max_Param_Size is 4096",
                   Max_Param_Size = 4096);
      Report_Test ("Max_Return_Size is 4096",
                   Max_Return_Size = 4096);
      Report_Test ("Max_State_Slots is 256",
                   Max_State_Slots = 256);
   end Test_Max_Constants;

   --  Test 21: Find entry in empty descriptor
   procedure Test_Find_Entry_Empty is
      Selector : constant Method_Selector := (others => 16#AA#);
      Index    : Natural;
      Found    : Boolean;
   begin
      Find_Entry_Point (Empty_Descriptor, Selector, Index, Found);
      Report_Test ("Find entry in empty descriptor fails",
                   not Found);
   end Test_Find_Entry_Empty;

begin
   Put_Line ("CVM Runtime Test Suite");
   Put_Line ("======================");
   New_Line;

   Test_Address_Compare;
   Test_Code_Compare;
   Test_Selector_Computation;
   Test_Context_Structure;
   Test_Empty_Context;
   Test_Exec_Status;
   Test_Error_Result;
   Test_Empty_Result;
   Test_Success_Result;
   Test_State_Slot_Defaults;
   Test_Dispatch_Init;
   Test_Default_Capabilities;
   Test_Privacy_Capabilities;
   Test_CVM_Info_Defaults;
   Test_Descriptor_Defaults;
   Test_Entry_Point_Defaults;
   Test_Registration_Defaults;
   Test_Registry_Entry_Defaults;
   Test_Pack_Unpack;
   Test_Max_Constants;
   Test_Find_Entry_Empty;

   New_Line;
   Put_Line ("======================");
   Put ("Tests run: ");
   Put (Tests_Run, Width => 0);
   New_Line;
   Put ("Tests passed: ");
   Put (Tests_Passed, Width => 0);
   New_Line;
   Put_Line ("======================");

   if Tests_Passed = Tests_Run then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;
end Test_CVM;
