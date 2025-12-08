pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;

--  KHEPRI Test Runner: Contract Testing Framework
--
--  This package provides the core testing infrastructure for KHEPRI
--  smart contracts. It supports:
--  - Unit tests for contract functions
--  - Integration tests for contract interactions
--  - Property-based testing
--  - Gas consumption tracking
--  - Test fixtures and setup/teardown
--
--  Test Structure:
--  - Test Suite: Collection of related tests
--  - Test Case: Individual test with setup/execution/teardown
--  - Test Assertion: Verification of expected behavior
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.1: Testing Framework

package Khepri_Test_Runner with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Test Result Types
   ---------------------------------------------------------------------------

   --  Test outcome
   type Test_Outcome is (
      Outcome_Passed,     --  Test passed
      Outcome_Failed,     --  Assertion failed
      Outcome_Error,      --  Unexpected error
      Outcome_Skipped,    --  Test skipped
      Outcome_Timeout     --  Test timed out
   );

   --  Test result with details
   type Test_Result is record
      Outcome       : Test_Outcome;
      Duration_Ms   : Natural;
      Gas_Used      : Word64;
      Message       : Bounded_String;
      Expected      : Bounded_String;
      Actual        : Bounded_String;
      Location      : Bounded_String;  --  File:line
   end record;

   Passed_Result : constant Test_Result := (
      Outcome       => Outcome_Passed,
      Duration_Ms   => 0,
      Gas_Used      => 0,
      Message       => Empty_String,
      Expected      => Empty_String,
      Actual        => Empty_String,
      Location      => Empty_String
   );

   ---------------------------------------------------------------------------
   --  Test Case Types
   ---------------------------------------------------------------------------

   --  Test case identifier
   subtype Test_Name is Bounded_String;

   --  Test case metadata
   type Test_Case_Info is record
      Name          : Test_Name;
      Description   : Bounded_String;
      Tags          : Bounded_String;  --  Comma-separated tags
      Timeout_Ms    : Natural;
      Gas_Limit     : Word64;
      Is_Async      : Boolean;
      Skip_Reason   : Bounded_String;  --  Empty if not skipped
   end record;

   Default_Test_Info : constant Test_Case_Info := (
      Name          => Empty_String,
      Description   => Empty_String,
      Tags          => Empty_String,
      Timeout_Ms    => 30000,
      Gas_Limit     => 10_000_000,
      Is_Async      => False,
      Skip_Reason   => Empty_String
   );

   --  Test case with result
   type Test_Case is record
      Info   : Test_Case_Info;
      Result : Test_Result;
      Is_Run : Boolean;
   end record;

   --  Maximum tests per suite
   Max_Tests : constant := 1024;
   type Test_Index is range 0 .. Max_Tests - 1;
   type Test_Array is array (Test_Index) of Test_Case;

   ---------------------------------------------------------------------------
   --  Test Suite Types
   ---------------------------------------------------------------------------

   --  Test suite metadata
   type Suite_Info is record
      Name          : Bounded_String;
      Description   : Bounded_String;
      Contract_Path : Bounded_String;
      Setup_Gas     : Word64;
   end record;

   --  Suite statistics
   type Suite_Stats is record
      Total_Tests   : Natural;
      Passed        : Natural;
      Failed        : Natural;
      Errors        : Natural;
      Skipped       : Natural;
      Timeouts      : Natural;
      Total_Gas     : Word64;
      Total_Time_Ms : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Test Runner Session
   ---------------------------------------------------------------------------

   type Test_Session is private;

   --  Runner configuration
   type Runner_Config is record
      --  Execution options
      Parallel       : Boolean;   --  Run tests in parallel
      Max_Parallel   : Natural;   --  Max parallel tests
      Stop_On_Failure: Boolean;   --  Stop at first failure
      Timeout_Ms     : Natural;   --  Default timeout

      --  Filtering
      Tag_Filter     : Bounded_String;  --  Run only matching tags
      Name_Filter    : Bounded_String;  --  Run only matching names

      --  Output
      Verbose        : Boolean;
      Report_Format  : Natural;   --  0=text, 1=JSON, 2=JUnit XML

      --  Gas tracking
      Track_Gas      : Boolean;
      Gas_Report     : Boolean;
   end record;

   Default_Runner_Config : constant Runner_Config := (
      Parallel       => False,
      Max_Parallel   => 4,
      Stop_On_Failure=> False,
      Timeout_Ms     => 30000,
      Tag_Filter     => Empty_String,
      Name_Filter    => Empty_String,
      Verbose        => True,
      Report_Format  => 0,
      Track_Gas      => True,
      Gas_Report     => True
   );

   --  Create test session
   function Create_Session (
      Config : Runner_Config
   ) return Test_Session with
      Global => null;

   --  Add test suite
   procedure Add_Suite (
      Session : in Out Test_Session;
      Info    : in     Suite_Info;
      Success : out    Boolean
   ) with
      Global => null;

   --  Add test case to current suite
   procedure Add_Test (
      Session   : in Out Test_Session;
      Test_Info : in     Test_Case_Info;
      Success   : out    Boolean
   ) with
      Global => null;

   --  Run all tests
   procedure Run_All (
      Session : in Out Test_Session;
      Stats   : out    Suite_Stats
   ) with
      Global => null;

   --  Run single test by index
   procedure Run_Test (
      Session    : in Out Test_Session;
      Test_Idx   : in     Test_Index;
      Result     : out    Test_Result
   ) with
      Global => null;

   --  Get test results
   procedure Get_Results (
      Session    : in  Test_Session;
      Tests      : out Test_Array;
      Test_Count : out Natural
   ) with
      Global => null;

   --  Get suite statistics
   function Get_Stats (
      Session : Test_Session
   ) return Suite_Stats with
      Global => null;

   ---------------------------------------------------------------------------
   --  Test Fixtures
   ---------------------------------------------------------------------------

   --  Fixture state
   type Fixture_State is record
      Contract_Address : Khepri_Types.Address;
      Deployer_Address : Khepri_Types.Address;
      Initial_Balance  : Uint256;
      Block_Number     : Word64;
      Timestamp        : Word64;
      Is_Setup         : Boolean;
   end record;

   Null_Fixture : constant Fixture_State := (
      Contract_Address => (others => 0),
      Deployer_Address => (others => 0),
      Initial_Balance  => U256_Zero,
      Block_Number     => 0,
      Timestamp        => 0,
      Is_Setup         => False
   );

   --  Setup fixture for test
   procedure Setup_Fixture (
      Session : in Out Test_Session;
      Fixture : out    Fixture_State;
      Success : out    Boolean
   ) with
      Global => null;

   --  Teardown fixture after test
   procedure Teardown_Fixture (
      Session : in Out Test_Session;
      Fixture : in Out Fixture_State
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Contract Interaction
   ---------------------------------------------------------------------------

   --  Call result
   type Call_Result is record
      Success    : Boolean;
      Return_Data: Byte_Array (0 .. 4095);
      Data_Size  : Natural;
      Gas_Used   : Word64;
      Revert_Msg : Bounded_String;
   end record;

   --  Call contract function
   procedure Call_Contract (
      Session  : in     Test_Session;
      Fixture  : in     Fixture_State;
      Selector : in     Bytes4;
      Args     : in     Byte_Array;
      Result   : out    Call_Result
   ) with
      Global => null;

   --  Send transaction to contract
   procedure Send_Transaction (
      Session  : in     Test_Session;
      Fixture  : in     Fixture_State;
      Selector : in     Bytes4;
      Args     : in     Byte_Array;
      Value    : in     Uint256;
      Result   : out    Call_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  State Manipulation (for testing)
   ---------------------------------------------------------------------------

   --  Set storage value
   procedure Set_Storage (
      Session : in Out Test_Session;
      Fixture : in     Fixture_State;
      Slot    : in     Hash256;
      Value   : in     Uint256;
      Success : out    Boolean
   ) with
      Global => null;

   --  Get storage value
   function Get_Storage (
      Session : Test_Session;
      Fixture : Fixture_State;
      Slot    : Hash256
   ) return Uint256 with
      Global => null;

   --  Set account balance (for testing)
   procedure Set_Balance (
      Session : in Out Test_Session;
      Address : in     Khepri_Types.Address;
      Balance : in     Uint256;
      Success : out    Boolean
   ) with
      Global => null;

   --  Advance block
   procedure Mine_Block (
      Session : in Out Test_Session;
      Count   : in     Natural := 1
   ) with
      Global => null;

   --  Set timestamp
   procedure Set_Timestamp (
      Session   : in Out Test_Session;
      Timestamp : in     Word64
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   --  Report format
   type Report_Format is (
      Format_Text,
      Format_JSON,
      Format_JUnit,
      Format_TAP
   );

   --  Generate test report
   procedure Generate_Report (
      Session : in     Test_Session;
      Format  : in     Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 1048576;

   --  Print summary to console
   function Summary_Line (
      Session : Test_Session
   ) return String with
      Global => null;

private

   Max_Suites : constant := 64;
   type Suite_Index is range 0 .. Max_Suites - 1;

   type Test_Suite is record
      Info       : Suite_Info;
      Tests      : Test_Array;
      Test_Count : Natural;
      Stats      : Suite_Stats;
      Fixture    : Fixture_State;
   end record;

   type Suite_Array is array (Suite_Index) of Test_Suite;

   type Test_Session is record
      Config       : Runner_Config;
      Suites       : Suite_Array;
      Suite_Count  : Natural;
      Current_Suite: Natural;
      Total_Stats  : Suite_Stats;
      Is_Running   : Boolean;
   end record;

end Khepri_Test_Runner;
