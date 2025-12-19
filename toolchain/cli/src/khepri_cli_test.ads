-------------------------------------------------------------------------------
--  KHEPRI CLI - Test Command (Test Runner Integration)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Khepri_CLI; use Khepri_CLI;

package Khepri_CLI_Test with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Test Command Options
   ---------------------------------------------------------------------------

   type Test_Options is record
      Project_File   : String (1 .. 256);
      File_Length    : Natural;
      Test_Filter    : String (1 .. 128);  --  Filter pattern (e.g., "test_*")
      Filter_Length  : Natural;
      Parallel       : Boolean;            --  Run tests in parallel
      Coverage       : Boolean;            --  Collect coverage data
      Verbose        : Boolean;
      Fail_Fast      : Boolean;            --  Stop on first failure
      Timeout        : Natural;            --  Per-test timeout (seconds)
   end record;

   Default_Test_Options : constant Test_Options := (
      Project_File  => (others => ' '),
      File_Length   => 0,
      Test_Filter   => (others => ' '),
      Filter_Length => 0,
      Parallel      => False,
      Coverage      => False,
      Verbose       => False,
      Fail_Fast     => False,
      Timeout       => 60
   );

   ---------------------------------------------------------------------------
   --  Test Results
   ---------------------------------------------------------------------------

   type Test_Result is record
      Total_Tests    : Natural;
      Passed_Tests   : Natural;
      Failed_Tests   : Natural;
      Skipped_Tests  : Natural;
      Total_Time_Ms  : Natural;
      Coverage_Pct   : Natural;  --  0-100
      Success        : Boolean;
      Error_Message  : String (1 .. 256);
      Error_Length   : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Command Execution
   ---------------------------------------------------------------------------

   --  Execute 'khepri test' command
   procedure Execute_Test (
      Options : Test_Options;
      Result  : out Test_Result;
      Output  : out CLI_Output
   ) with
      Global => null;

   --  Discover test files in project
   procedure Discover_Tests (
      Project_Dir  : String;
      Test_Files   : out Test_File_Array;
      Test_Count   : out Natural;
      Success      : out Boolean
   ) with
      Global => null,
      Pre    => Project_Dir'Length > 0;

   ---------------------------------------------------------------------------
   --  Test File Management
   ---------------------------------------------------------------------------

   Max_Test_Files : constant := 64;

   type Test_File_Info is record
      Path      : String (1 .. 512);
      Length    : Natural;
      Test_Count: Natural;
   end record;

   type Test_File_Array is array (Natural range 0 .. Max_Test_Files - 1)
      of Test_File_Info;

end Khepri_CLI_Test;
