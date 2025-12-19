-------------------------------------------------------------------------------
--  KHEPRI CLI - Prove Command
--  Runs GNATprove for formal verification
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Khepri_CLI; use Khepri_CLI;

package Khepri_CLI_Prove with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Proof Configuration
   ---------------------------------------------------------------------------

   type Proof_Options is record
      Project_File   : String (1 .. 256);
      Level          : Proof_Level;
      Timeout        : Natural;       -- Per VC timeout in seconds
      Steps          : Natural;       -- Max proof steps
      Parallel       : Natural;       -- Parallel jobs (0 = auto)
      Prover         : Prover_Choice;
      Provers        : String (1 .. 64);  -- Prover name string
      Report_Mode    : Report_Mode;
      Report         : Report_Mode;   -- Alias for Report_Mode
      Output_Format  : String (1 .. 32);
      Warnings       : Warning_Mode;
      Warnings_As_Errors : Boolean;
      Debug_Mode     : Boolean;
   end record;

   type Prover_Choice is (
      Prover_Auto,      -- Let GNATprove decide
      Prover_CVC5,      -- CVC5 only
      Prover_Z3,        -- Z3 only
      Prover_Alt_Ergo,  -- Alt-Ergo only
      Prover_All        -- Try all provers
   );

   type Report_Mode is (
      Report_Fail,      -- Only failed VCs
      Report_All,       -- All VCs
      Report_Statistics -- Summary statistics
   );

   type Warning_Mode is (
      Warnings_Off,
      Warnings_Continue,
      Warnings_Error
   );

   Default_Proof_Options : constant Proof_Options := (
      Project_File   => (others => ' '),
      Level          => Level_Silver,
      Timeout        => 60,
      Steps          => 0,  -- Default
      Parallel       => 0,  -- Auto
      Prover         => Prover_All,
      Provers        => (others => ' '),
      Report_Mode    => Report_All,
      Report         => Report_All,
      Output_Format  => (others => ' '),
      Warnings       => Warnings_Continue,
      Warnings_As_Errors => False,
      Debug_Mode     => False
   );

   ---------------------------------------------------------------------------
   --  Proof Execution
   ---------------------------------------------------------------------------

   --  Execute proof analysis
   procedure Execute_Prove (
      Options  : Proof_Options;
      Results  : out Proof_Results;
      Output   : out CLI_Output
   ) with
      Global => null;

   --  Check if GNATprove is available
   procedure Check_GNATprove (
      Available : out Boolean;
      Version   : out String
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Proof Results
   ---------------------------------------------------------------------------

   type VC_Status is (
      VC_Proved,
      VC_Not_Proved,
      VC_Timeout,
      VC_Error
   );

   type VC_Result is record
      File_Name      : String (1 .. 128);
      Line           : Natural;
      Column         : Natural;
      Check_Type     : String (1 .. 64);
      Status         : VC_Status;
      Prover_Used    : String (1 .. 32);
      Time_Taken     : Float;
   end record;

   type Proof_Results is record
      Total_VCs      : Natural;
      VCs_Total      : Natural;  -- Alias
      Proved         : Natural;
      VCs_Proven     : Natural;  -- Alias
      Not_Proved     : Natural;
      VCs_Unproven   : Natural;  -- Alias
      Timeouts       : Natural;
      Errors         : Natural;
      VCs_Error      : Natural;  -- Alias
      Flow_Errors    : Natural;
      Time_Total     : Float;
      Elapsed_Seconds : Float;  -- Alias
      Success        : Boolean;
      Target_Met     : Boolean;  -- Alias
      Level_Achieved : Proof_Level;
   end record;

   ---------------------------------------------------------------------------
   --  GNATprove Invocation
   ---------------------------------------------------------------------------

   --  Build command line for gnatprove
   function Build_GNATprove_Command (
      Options : Proof_Options
   ) return String with
      Global => null;

   --  Map proof level to --level=N
   function Level_To_Number (L : Proof_Level) return Natural with
      Global => null,
      Post => Level_To_Number'Result in 0 .. 4;

   --  Run gnatprove and capture output
   procedure Run_GNATprove (
      Options   : Proof_Options;
      Exit_Code : out Integer;
      Output    : out String;
      Output_Len: out Natural
   ) with
      Global => null;

   --  Parse gnatprove JSON output
   procedure Parse_Proof_Output (
      Output    : String;
      Results   : out Proof_Results
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Analysis
   ---------------------------------------------------------------------------

   --  Analyze proof failures
   procedure Analyze_Failures (
      Results   : Proof_Results;
      Analysis  : out Failure_Analysis
   ) with
      Global => null;

   type Failure_Analysis is record
      Common_Issues  : String (1 .. 512);
      Suggestions    : String (1 .. 512);
      Worst_File     : String (1 .. 128);
      Worst_Count    : Natural;
   end record;

   --  Check if contract meets certification level
   function Meets_Certification (
      Results : Proof_Results;
      Target  : Cert_Target
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Progress Tracking
   ---------------------------------------------------------------------------

   type Proof_Progress is record
      Current_File   : String (1 .. 128);
      Current_VC     : Natural;
      Total_VCs      : Natural;
      Elapsed_Time   : Float;
   end record;

   --  Callback type for progress updates
   type Progress_Callback is access procedure (P : Proof_Progress);

   --  Set progress callback
   procedure Set_Progress_Callback (Callback : Progress_Callback) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Incremental Proofs
   ---------------------------------------------------------------------------

   --  Only prove changed files
   procedure Prove_Incremental (
      Options   : Proof_Options;
      Changed   : String;  -- Comma-separated file list
      Results   : out Proof_Results;
      Output    : out CLI_Output
   ) with
      Global => null;

   --  Clear proof cache
   procedure Clear_Cache (
      Project_Dir : String;
      Success     : out Boolean
   ) with
      Global => null;

end Khepri_CLI_Prove;
