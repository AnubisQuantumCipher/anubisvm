-------------------------------------------------------------------------------
--  KHEPRI CLI - Test Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  File I/O and process execution

with Ada.Text_IO;
with Ada.Directories;
with Ada.Calendar;
with GNAT.OS_Lib;

package body Khepri_CLI_Test is

   use Ada.Text_IO;
   use Ada.Directories;
   use Ada.Calendar;

   ---------------------------------------------------------------------------
   --  Test Discovery
   ---------------------------------------------------------------------------

   procedure Discover_Tests (
      Project_Dir  : String;
      Test_Files   : out Test_File_Array;
      Test_Count   : out Natural;
      Success      : out Boolean
   ) is
      Test_Dir : constant String := Project_Dir & "/tests";
      Search   : Search_Type;
      Item     : Directory_Entry_Type;
   begin
      Test_Files := (others => (
         Path       => (others => ' '),
         Length     => 0,
         Test_Count => 0
      ));
      Test_Count := 0;
      Success := False;

      if not Exists (Test_Dir) then
         return;
      end if;

      --  Search for test_*.adb files
      begin
         Start_Search (Search, Test_Dir, "test_*.adb");

         while More_Entries (Search) and Test_Count < Max_Test_Files loop
            Get_Next_Entry (Search, Item);

            declare
               Full_Path : constant String := Full_Name (Item);
            begin
               if Full_Path'Length <= 512 then
                  Test_Files (Test_Count).Path (1 .. Full_Path'Length) := Full_Path;
                  Test_Files (Test_Count).Length := Full_Path'Length;
                  Test_Files (Test_Count).Test_Count := 1;  --  Simplified
                  Test_Count := Test_Count + 1;
               end if;
            end;
         end loop;

         End_Search (Search);
         Success := True;

      exception
         when others =>
            if Is_Open (Search) then
               End_Search (Search);
            end if;
            Success := False;
      end;
   end Discover_Tests;

   ---------------------------------------------------------------------------
   --  Test Execution
   ---------------------------------------------------------------------------

   procedure Run_Test_File (
      Test_Path : String;
      Timeout   : Natural;
      Passed    : out Boolean;
      Time_Ms   : out Natural
   ) is
      use GNAT.OS_Lib;

      Start_Time : constant Time := Clock;
      Executable : constant String := Test_Path (Test_Path'First .. Test_Path'Last - 4);  --  Remove .adb
      Cmd_Path   : String_Access;
      Args       : Argument_List (1 .. 0);
      Status     : aliased Integer;
      Pid        : Process_Id;
   begin
      Passed := False;
      Time_Ms := 0;

      --  Build test executable first (simplified - would use gprbuild)
      --  For now, assume it's already built

      --  Locate executable
      Cmd_Path := Locate_Exec_On_Path (Simple_Name (Executable));

      if Cmd_Path = null then
         --  Try in obj directory
         declare
            Obj_Exec : constant String := "obj/" & Simple_Name (Executable);
         begin
            if Exists (Obj_Exec) then
               Cmd_Path := new String'(Obj_Exec);
            else
               return;
            end if;
         end;
      end if;

      --  Execute test
      Pid := Non_Blocking_Spawn (
         Program_Name => Cmd_Path.all,
         Args         => Args
      );

      Wait_Process (Pid, Status);

      declare
         End_Time : constant Time := Clock;
         Duration_Sec : constant Duration := End_Time - Start_Time;
      begin
         Time_Ms := Natural (Float'Floor (Float (Duration_Sec) * 1000.0));
      end;

      Passed := (Status = 0);

      Free (Cmd_Path);

   exception
      when others =>
         if Cmd_Path /= null then
            Free (Cmd_Path);
         end if;
         Passed := False;
   end Run_Test_File;

   ---------------------------------------------------------------------------
   --  Command Execution
   ---------------------------------------------------------------------------

   procedure Execute_Test (
      Options : Test_Options;
      Result  : out Test_Result;
      Output  : out CLI_Output
   ) is
      Project_Dir : constant String := ".";  --  Would parse from project file
      Test_Files  : Test_File_Array;
      Test_Count  : Natural;
      Discover_Success : Boolean;
      Total_Passed : Natural := 0;
      Total_Failed : Natural := 0;
      Total_Time   : Natural := 0;
   begin
      --  Initialize result
      Result := (
         Total_Tests   => 0,
         Passed_Tests  => 0,
         Failed_Tests  => 0,
         Skipped_Tests => 0,
         Total_Time_Ms => 0,
         Coverage_Pct  => 0,
         Success       => False,
         Error_Message => (others => ' '),
         Error_Length  => 0
      );

      --  Discover tests
      Discover_Tests (Project_Dir, Test_Files, Test_Count, Discover_Success);

      if not Discover_Success or Test_Count = 0 then
         Output.Status := Result_Not_Found;
         Output.Exit_Code := 1;
         declare
            Msg : constant String := "No test files found";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
            Result.Error_Message (1 .. Msg'Length) := Msg;
            Result.Error_Length := Msg'Length;
         end;
         return;
      end if;

      --  Run each test
      Put_Line ("Running " & Natural'Image (Test_Count) & " test file(s)...");
      Put_Line ("");

      for I in 0 .. Test_Count - 1 loop
         declare
            Test_Path : constant String :=
               Test_Files (I).Path (1 .. Test_Files (I).Length);
            Test_Name : constant String := Simple_Name (Test_Path);
            Passed    : Boolean;
            Time_Ms   : Natural;
         begin
            Put (Test_Name & "... ");

            Run_Test_File (Test_Path, Options.Timeout, Passed, Time_Ms);

            if Passed then
               Put_Line ("PASS (" & Natural'Image (Time_Ms) & " ms)");
               Total_Passed := Total_Passed + 1;
            else
               Put_Line ("FAIL");
               Total_Failed := Total_Failed + 1;

               if Options.Fail_Fast then
                  Put_Line ("Stopping on first failure (--fail-fast)");
                  exit;
               end if;
            end if;

            Total_Time := Total_Time + Time_Ms;
         end;
      end loop;

      Put_Line ("");
      Put_Line ("Test Results:");
      Put_Line ("  Total:  " & Natural'Image (Test_Count));
      Put_Line ("  Passed: " & Natural'Image (Total_Passed));
      Put_Line ("  Failed: " & Natural'Image (Total_Failed));
      Put_Line ("  Time:   " & Natural'Image (Total_Time) & " ms");

      --  Set result
      Result.Total_Tests := Test_Count;
      Result.Passed_Tests := Total_Passed;
      Result.Failed_Tests := Total_Failed;
      Result.Total_Time_Ms := Total_Time;
      Result.Success := (Total_Failed = 0);

      if Result.Success then
         Output.Status := Result_Success;
         Output.Exit_Code := 0;
         declare
            Msg : constant String := "All tests passed";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      else
         Output.Status := Result_Error;
         Output.Exit_Code := 1;
         declare
            Msg : constant String :=
               Natural'Image (Total_Failed) & " test(s) failed";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      end if;

   exception
      when E : others =>
         Output.Status := Result_Error;
         Output.Exit_Code := 1;
         declare
            Msg : constant String := "Error running tests";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
            Result.Error_Message (1 .. Msg'Length) := Msg;
            Result.Error_Length := Msg'Length;
         end;
   end Execute_Test;

end Khepri_CLI_Test;
