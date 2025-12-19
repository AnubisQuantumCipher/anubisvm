-------------------------------------------------------------------------------
--  KHEPRI CLI - Prove Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Text_IO;
with GNAT.OS_Lib;

package body Khepri_CLI_Prove with
   SPARK_Mode => Off  -- OS operations not in SPARK
is
   ---------------------------------------------------------------------------
   --  Proof Execution
   ---------------------------------------------------------------------------

   procedure Execute_Prove (
      Options  : Proof_Options;
      Results  : out Proof_Results;
      Output   : out CLI_Output
   ) is
      GNATProve_Path : constant String := "gnatprove";
      Args : GNAT.OS_Lib.Argument_List (1 .. 20);
      Arg_Count : Natural := 0;
      Success : Boolean;
   begin
      --  Initialize results
      Results.Total_VCs := 0;
      Results.VCs_Total := 0;
      Results.Proved := 0;
      Results.VCs_Proven := 0;
      Results.Not_Proved := 0;
      Results.VCs_Unproven := 0;
      Results.Timeouts := 0;
      Results.Errors := 0;
      Results.VCs_Error := 0;
      Results.Flow_Errors := 0;
      Results.Time_Total := 0.0;
      Results.Elapsed_Seconds := 0.0;
      Results.Success := False;
      Results.Target_Met := False;
      Results.Level_Achieved := Proof_Flow;

      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Build argument list
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'("-P");
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'(Trim (Options.Project_File));

      --  Add level
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'("--level=" &
         (case Options.Level is
            when Proof_Flow   => "0",
            when Proof_Bronze => "1",
            when Proof_Silver => "2",
            when Proof_Gold   => "3",
            when Proof_Max    => "4"));

      --  Add timeout
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'("--timeout=" &
         Natural'Image (Options.Timeout));

      --  Add parallel jobs
      if Options.Parallel > 0 then
         Arg_Count := Arg_Count + 1;
         Args (Arg_Count) := new String'("-j" &
            Natural'Image (Options.Parallel));
      end if;

      --  Add prover selection
      if Options.Provers (1) /= ' ' then
         Arg_Count := Arg_Count + 1;
         Args (Arg_Count) := new String'("--prover=" &
            Trim (Options.Provers));
      end if;

      --  Add report mode
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'("--report=" &
         (case Options.Report is
            when Report_Fail       => "fail",
            when Report_All        => "all",
            when Report_Statistics => "statistics"));

      --  Execute gnatprove
      GNAT.OS_Lib.Spawn (
         Program_Name => GNATProve_Path,
         Args => Args (1 .. Arg_Count),
         Success => Success
      );

      --  Free argument strings
      for I in 1 .. Arg_Count loop
         GNAT.OS_Lib.Free (Args (I));
      end loop;

      if Success then
         Output.Status := Result_Success;
         Output.Exit_Code := 0;
         Results.Level_Achieved := Options.Level;
         Results.Target_Met := True;
         declare
            Msg : constant String := "Proof completed successfully";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      else
         declare
            Msg : constant String := "Proof failed or has unproven VCs";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      end if;

   exception
      when others =>
         declare
            Msg : constant String := "Proof execution error";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_Prove;

   ---------------------------------------------------------------------------
   --  Result Parsing
   ---------------------------------------------------------------------------

   procedure Parse_Proof_Output (
      Output_File : String;
      Results     : out Proof_Results
   ) is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 256);
      Last : Natural;
   begin
      Results.VCs_Total := 0;
      Results.VCs_Proven := 0;
      Results.VCs_Unproven := 0;
      Results.VCs_Error := 0;
      Results.Elapsed_Seconds := 0;
      Results.Level_Achieved := Proof_Flow;
      Results.Target_Met := False;

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Output_File);

      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Line, Last);

         --  Parse summary lines
         --  Format: "Total: X VCs, Y proven, Z unproven"
         if Last >= 10 and then Line (1 .. 6) = "Total:" then
            --  Parse VCs (simplified)
            null;
         end if;
      end loop;

      Ada.Text_IO.Close (File);

      --  Determine achieved level
      if Results.VCs_Unproven = 0 and Results.VCs_Error = 0 then
         Results.Target_Met := True;
         if Results.VCs_Total > 0 then
            Results.Level_Achieved := Proof_Silver;
         end if;
      end if;

   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
   end Parse_Proof_Output;

   function Get_Level_From_Results (
      Results : Proof_Results
   ) return Proof_Target is
   begin
      if Results.VCs_Total = 0 then
         return Proof_Flow;
      elsif Results.VCs_Unproven > 0 or Results.VCs_Error > 0 then
         return Proof_Bronze;
      elsif Results.VCs_Proven = Results.VCs_Total then
         return Proof_Gold;
      else
         return Proof_Silver;
      end if;
   end Get_Level_From_Results;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   procedure Generate_Proof_Report (
      Results     : Proof_Results;
      Output_Path : String;
      Success     : out Boolean
   ) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Output_Path);

      Ada.Text_IO.Put_Line (File, "KHEPRI Proof Report");
      Ada.Text_IO.Put_Line (File, "===================");
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line (File, "Summary:");
      Ada.Text_IO.Put_Line (File, "  Total VCs:    " &
         Natural'Image (Results.VCs_Total));
      Ada.Text_IO.Put_Line (File, "  Proven:       " &
         Natural'Image (Results.VCs_Proven));
      Ada.Text_IO.Put_Line (File, "  Unproven:     " &
         Natural'Image (Results.VCs_Unproven));
      Ada.Text_IO.Put_Line (File, "  Errors:       " &
         Natural'Image (Results.VCs_Error));
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line (File, "Level Achieved: " &
         (case Results.Level_Achieved is
            when Proof_Flow   => "Flow",
            when Proof_Bronze => "Bronze",
            when Proof_Silver => "Silver",
            when Proof_Gold   => "Gold",
            when Proof_Max    => "Max"));

      Ada.Text_IO.Put_Line (File, "Target Met:     " &
         Boolean'Image (Results.Target_Met));

      Ada.Text_IO.Close (File);
      Success := True;

   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         Success := False;
   end Generate_Proof_Report;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Trim (S : String) return String is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      while First <= Last and then S (First) = ' ' loop
         First := First + 1;
      end loop;
      while Last >= First and then S (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if First > Last then
         return "";
      else
         return S (First .. Last);
      end if;
   end Trim;

end Khepri_CLI_Prove;
