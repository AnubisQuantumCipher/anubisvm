pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Test_Runner; use Khepri_Test_Runner;

--  KHEPRI Test Assertions: Test Assertion Library
--
--  This package provides a comprehensive set of assertion functions
--  for testing KHEPRI smart contracts. All assertions return a
--  Test_Result for integration with the test runner.
--
--  Assertion Categories:
--  - Basic equality/inequality
--  - Numeric comparisons
--  - Array/byte comparisons
--  - Contract state assertions
--  - Event assertions
--  - Revert assertions
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.2: Test Assertions

package Khepri_Test_Assertions with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Basic Assertions
   ---------------------------------------------------------------------------

   --  Assert boolean condition
   function Assert_True (
      Condition : Boolean;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_False (
      Condition : Boolean;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Equality Assertions
   ---------------------------------------------------------------------------

   --  Assert equal (natural)
   function Assert_Equal (
      Expected : Natural;
      Actual   : Natural;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert equal (Word64)
   function Assert_Equal_U64 (
      Expected : Word64;
      Actual   : Word64;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert equal (Uint256)
   function Assert_Equal_U256 (
      Expected : Uint256;
      Actual   : Uint256;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert equal (Address)
   function Assert_Equal_Address (
      Expected : Khepri_Types.Address;
      Actual   : Khepri_Types.Address;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert equal (Hash256)
   function Assert_Equal_Hash (
      Expected : Hash256;
      Actual   : Hash256;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert equal (Byte arrays)
   function Assert_Equal_Bytes (
      Expected : Byte_Array;
      Actual   : Byte_Array;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Inequality Assertions
   ---------------------------------------------------------------------------

   function Assert_Not_Equal (
      Expected : Natural;
      Actual   : Natural;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_Not_Equal_U256 (
      Expected : Uint256;
      Actual   : Uint256;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Numeric Comparisons
   ---------------------------------------------------------------------------

   function Assert_Greater_Than (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_Greater_Equal (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_Less_Than (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_Less_Equal (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert value is in range
   function Assert_In_Range (
      Value : Natural;
      Min   : Natural;
      Max   : Natural;
      Message : String := ""
   ) return Test_Result with
      Global => null,
      Pre    => Min <= Max;

   ---------------------------------------------------------------------------
   --  Uint256 Comparisons
   ---------------------------------------------------------------------------

   function Assert_U256_Greater (
      Value     : Uint256;
      Threshold : Uint256;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_U256_Less (
      Value     : Uint256;
      Threshold : Uint256;
      Message   : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_U256_Zero (
      Value   : Uint256;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_U256_Not_Zero (
      Value   : Uint256;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Address Assertions
   ---------------------------------------------------------------------------

   function Assert_Address_Zero (
      Addr    : Khepri_Types.Address;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   function Assert_Address_Not_Zero (
      Addr    : Khepri_Types.Address;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Call Result Assertions
   ---------------------------------------------------------------------------

   --  Assert call succeeded
   function Assert_Call_Success (
      Result  : Call_Result;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert call failed/reverted
   function Assert_Call_Reverted (
      Result  : Call_Result;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert call reverted with specific message
   function Assert_Revert_Message (
      Result          : Call_Result;
      Expected_Message: String;
      Message         : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert return data equals expected
   function Assert_Return_Data (
      Result   : Call_Result;
      Expected : Byte_Array;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Gas Assertions
   ---------------------------------------------------------------------------

   --  Assert gas used is within limit
   function Assert_Gas_Used (
      Used    : Word64;
      Max_Gas : Word64;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert gas used is in range
   function Assert_Gas_Range (
      Used    : Word64;
      Min_Gas : Word64;
      Max_Gas : Word64;
      Message : String := ""
   ) return Test_Result with
      Global => null,
      Pre    => Min_Gas <= Max_Gas;

   ---------------------------------------------------------------------------
   --  Storage Assertions
   ---------------------------------------------------------------------------

   --  Assert storage slot equals value
   function Assert_Storage (
      Session  : Test_Session;
      Fixture  : Fixture_State;
      Slot     : Hash256;
      Expected : Uint256;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Balance Assertions
   ---------------------------------------------------------------------------

   --  Assert balance equals expected
   function Assert_Balance (
      Session  : Test_Session;
      Address  : Khepri_Types.Address;
      Expected : Uint256;
      Message  : String := ""
   ) return Test_Result with
      Global => null;

   --  Assert balance changed by amount
   function Assert_Balance_Change (
      Before  : Uint256;
      After   : Uint256;
      Amount  : Uint256;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Event Assertions (Placeholder for future)
   ---------------------------------------------------------------------------

   --  Event check type
   type Event_Check is record
      Topic0   : Hash256;  --  Event signature
      Has_Data : Boolean;
      Data     : Byte_Array (0 .. 255);
      Data_Len : Natural;
   end record;

   --  Assert event was emitted
   function Assert_Event_Emitted (
      Session : Test_Session;
      Event   : Event_Check;
      Message : String := ""
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Combination Assertions
   ---------------------------------------------------------------------------

   --  Combine multiple results (all must pass)
   function Assert_All (
      Results : Test_Array;
      Count   : Natural
   ) return Test_Result with
      Global => null;

   --  Combine multiple results (at least one must pass)
   function Assert_Any (
      Results : Test_Array;
      Count   : Natural
   ) return Test_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Create failure result
   function Fail (
      Message : String := ""
   ) return Test_Result with
      Global => null;

   --  Create skip result
   function Skip (
      Reason : String := ""
   ) return Test_Result with
      Global => null;

   --  Check if result is passing
   function Is_Passing (
      Result : Test_Result
   ) return Boolean with
      Global => null;

end Khepri_Test_Assertions;
