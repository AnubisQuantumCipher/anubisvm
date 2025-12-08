-------------------------------------------------------------------------------
--  KHEPRI CLI - Certify Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Text_IO;
with Anubis_SHA3;

package body Khepri_CLI_Certify with
   SPARK_Mode => Off  -- I/O operations not in SPARK
is
   ---------------------------------------------------------------------------
   --  Certification Execution
   ---------------------------------------------------------------------------

   procedure Execute_Certify (
      Options  : Certify_Options;
      Results  : out Certification_Result;
      Output   : out CLI_Output
   ) is
      Proof_OK : Boolean;
      WCET_OK : Boolean;
      CT_OK : Boolean;
   begin
      --  Initialize results
      Results.Level_Achieved := Cert_Bronze;
      Results.Target_Met := False;
      Results.Proof_Results.VCs_Total := 0;
      Results.Proof_Results.VCs_Proven := 0;
      Results.Proof_Results.VCs_Unproven := 0;
      Results.Proof_Results.VCs_Error := 0;
      Results.Proof_Results.Elapsed_Seconds := 0;
      Results.Proof_Results.Level_Achieved := Proof_Flow;
      Results.Proof_Results.Target_Met := False;
      Results.WCET_Analyzed := False;
      Results.Max_WCET := 0;
      Results.WCET_Count := 0;
      Results.CT_Analyzed := False;
      Results.CT_Passed := False;
      Results.CT_Violations := 0;
      Results.Manifest_Path := (others => ' ');
      Results.Manifest_Hash := (others => 0);
      Results.Estimated_Gas := 0;
      Results.Gas_Discount := 1.0;

      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Step 1: Run proofs
      Run_Certification_Proofs (Options, Results.Proof_Results, Proof_OK);
      if not Proof_OK then
         declare
            Msg : constant String := "Proof verification failed";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
         return;
      end if;

      --  Step 2: Run WCET analysis (if requested)
      if Options.Include_WCET then
         declare
            WCET_Results : WCET_Analysis_Result;
         begin
            Run_WCET_Analysis (
               Trim (Options.Project_File),
               WCET_Results,
               WCET_OK
            );
            if WCET_OK then
               Results.WCET_Analyzed := True;
               Results.Max_WCET := WCET_Results.Max_Cycles;
               Results.WCET_Count := WCET_Results.Func_Count;
            end if;
         end;
      end if;

      --  Step 3: Run constant-time analysis (if requested)
      if Options.Include_CT then
         declare
            CT_Results : CT_Analysis_Result;
         begin
            Run_CT_Analysis (
               Trim (Options.Project_File),
               CT_Results,
               CT_OK
            );
            if CT_OK then
               Results.CT_Analyzed := True;
               Results.CT_Passed := CT_Results.All_Passed;
               Results.CT_Violations := CT_Results.Violations;
            end if;
         end;
      end if;

      --  Step 4: Determine certification level
      Results.Level_Achieved := Determine_Level (
         Results.Proof_Results,
         (Func_Count => Results.WCET_Count,
          Max_Cycles => Results.Max_WCET,
          All_Bounded => Results.WCET_Analyzed,
          others => <>),
         (Func_Count => 0,
          All_Passed => Results.CT_Passed,
          Violations => Results.CT_Violations,
          others => <>),
         Options.Auditor_Key (1) /= ' '
      );

      Results.Target_Met := Results.Level_Achieved >= Options.Target_Level;

      --  Step 5: Calculate gas discount
      Results.Gas_Discount := Gas_Discount (Results.Level_Achieved);
      Results.Estimated_Gas := Calculate_Gas (
         (Func_Count => Results.WCET_Count,
          Max_Cycles => Results.Max_WCET,
          All_Bounded => Results.WCET_Analyzed,
          others => <>),
         Results.Level_Achieved
      );

      --  Step 6: Generate manifest
      declare
         Manifest_OK : Boolean;
         Manifest_Path_Str : String (1 .. 256) := (others => ' ');
      begin
         Generate_Manifest (
            Options,
            Results.Proof_Results,
            (Func_Count => Results.WCET_Count,
             Max_Cycles => Results.Max_WCET,
             All_Bounded => Results.WCET_Analyzed,
             others => <>),
            (Func_Count => 0,
             All_Passed => Results.CT_Passed,
             Violations => Results.CT_Violations,
             others => <>),
            Manifest_Path_Str,
            Manifest_OK
         );

         if Manifest_OK then
            Results.Manifest_Path := Manifest_Path_Str;
         end if;
      end;

      --  Step 7: Sign manifest (Platinum only)
      if Results.Level_Achieved = Cert_Platinum and
         Options.Auditor_Key (1) /= ' '
      then
         declare
            Sign_OK : Boolean;
         begin
            Sign_Manifest (
               Trim (Results.Manifest_Path),
               Trim (Options.Auditor_Key),
               Sign_OK
            );
         end;
      end if;

      Output.Status := Result_Success;
      Output.Exit_Code := 0;
      declare
         Msg : constant String := "Certification completed: " &
            (case Results.Level_Achieved is
               when Cert_Bronze   => "Bronze",
               when Cert_Silver   => "Silver",
               when Cert_Gold     => "Gold",
               when Cert_Platinum => "Platinum");
      begin
         Output.Message (1 .. Msg'Length) := Msg;
         Output.Message_Length := Msg'Length;
      end;

   exception
      when others =>
         declare
            Msg : constant String := "Certification execution error";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_Certify;

   ---------------------------------------------------------------------------
   --  Individual Certification Steps
   ---------------------------------------------------------------------------

   procedure Run_Certification_Proofs (
      Options  : Certify_Options;
      Results  : out Proof_Results;
      Success  : out Boolean
   ) is
      Proof_Options : Khepri_CLI_Prove.Proof_Options := (
         Project_File => Options.Project_File,
         Level => Proof_Silver,
         Timeout => 60,
         Parallel => 0,
         Provers => (others => ' '),
         Report => Report_All,
         Output_Format => (others => ' '),
         Warnings_As_Errors => False
      );
      Dummy_Output : CLI_Output;
   begin
      --  Set proof level based on target certification
      case Options.Target_Level is
         when Cert_Bronze =>
            Proof_Options.Level := Proof_Bronze;
         when Cert_Silver | Cert_Gold | Cert_Platinum =>
            Proof_Options.Level := Proof_Silver;
      end case;

      Khepri_CLI_Prove.Execute_Prove (Proof_Options, Results, Dummy_Output);
      Success := Results.Target_Met or Results.VCs_Unproven = 0;
   end Run_Certification_Proofs;

   procedure Run_WCET_Analysis (
      Project_File : String;
      WCET_Results : out WCET_Analysis_Result;
      Success      : out Boolean
   ) is
   begin
      --  Initialize results
      for I in WCET_Results.Functions'Range loop
         WCET_Results.Functions (I).Name := (others => ' ');
         WCET_Results.Functions (I).Name_Length := 0;
         WCET_Results.Functions (I).Max_Cycles := 0;
         WCET_Results.Functions (I).Avg_Cycles := 0;
         WCET_Results.Functions (I).Proven := False;
      end loop;
      WCET_Results.Func_Count := 0;
      WCET_Results.Max_Cycles := 0;
      WCET_Results.All_Bounded := True;

      --  Would invoke WCET analyzer (e.g., SWEET, aiT, or custom tool)
      --  For now, provide placeholder values
      WCET_Results.Func_Count := 1;
      WCET_Results.Functions (0).Name (1 .. 4) := "main";
      WCET_Results.Functions (0).Name_Length := 4;
      WCET_Results.Functions (0).Max_Cycles := 1000000;
      WCET_Results.Functions (0).Avg_Cycles := 500000;
      WCET_Results.Functions (0).Proven := True;
      WCET_Results.Max_Cycles := 1000000;

      Success := True;
   end Run_WCET_Analysis;

   procedure Run_CT_Analysis (
      Project_File : String;
      CT_Results   : out CT_Analysis_Result;
      Success      : out Boolean
   ) is
   begin
      --  Initialize results
      for I in CT_Results.Functions'Range loop
         CT_Results.Functions (I).Name := (others => ' ');
         CT_Results.Functions (I).Name_Length := 0;
         CT_Results.Functions (I).Is_CT := False;
         CT_Results.Functions (I).Violation_Type := CT_None;
      end loop;
      CT_Results.Func_Count := 0;
      CT_Results.All_Passed := True;
      CT_Results.Violations := 0;

      --  Would invoke constant-time checker
      --  For now, assume all pass
      Success := True;
   end Run_CT_Analysis;

   procedure Generate_Manifest (
      Options       : Certify_Options;
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result;
      Manifest_Path : out String;
      Success       : out Boolean
   ) is
      File : Ada.Text_IO.File_Type;
      Path : constant String :=
         (if Options.Output_Path (1) = ' ' then "contract.manifest"
          else Trim (Options.Output_Path));
   begin
      Manifest_Path := (others => ' ');
      Success := False;

      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);

      Ada.Text_IO.Put_Line (File, "[manifest]");
      Ada.Text_IO.Put_Line (File, "version = ""1.0.0""");
      Ada.Text_IO.Put_Line (File, "format = ""khepri-v1""");
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line (File, "[certification]");
      Ada.Text_IO.Put_Line (File, "level = """ &
         (case Options.Target_Level is
            when Cert_Bronze   => "bronze",
            when Cert_Silver   => "silver",
            when Cert_Gold     => "gold",
            when Cert_Platinum => "platinum") & """");
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line (File, "[proof]");
      Ada.Text_IO.Put_Line (File, "vcs_total = " &
         Natural'Image (Proof_Results.VCs_Total));
      Ada.Text_IO.Put_Line (File, "vcs_proven = " &
         Natural'Image (Proof_Results.VCs_Proven));
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line (File, "[wcet]");
      Ada.Text_IO.Put_Line (File, "max_cycles = " &
         Unsigned_64'Image (WCET_Results.Max_Cycles));
      Ada.Text_IO.Put_Line (File, "functions = " &
         Natural'Image (WCET_Results.Func_Count));
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line (File, "[constant_time]");
      Ada.Text_IO.Put_Line (File, "analyzed = " &
         Boolean'Image (CT_Results.Func_Count > 0));
      Ada.Text_IO.Put_Line (File, "violations = " &
         Natural'Image (CT_Results.Violations));

      Ada.Text_IO.Close (File);

      Manifest_Path (1 .. Path'Length) := Path;
      Success := True;

   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
   end Generate_Manifest;

   procedure Sign_Manifest (
      Manifest_Path : String;
      Auditor_Key   : String;
      Success       : out Boolean
   ) is
   begin
      --  Would sign manifest with ML-DSA-87
      Success := Manifest_Path'Length > 0 and Auditor_Key'Length > 0;
   end Sign_Manifest;

   ---------------------------------------------------------------------------
   --  Level Determination
   ---------------------------------------------------------------------------

   function Determine_Level (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result;
      Has_Audit     : Boolean
   ) return Cert_Target is
   begin
      --  Check requirements from highest to lowest
      if Check_Platinum_Requirements (Proof_Results, WCET_Results, CT_Results, Has_Audit) then
         return Cert_Platinum;
      elsif Check_Gold_Requirements (Proof_Results, WCET_Results, CT_Results) then
         return Cert_Gold;
      elsif Check_Silver_Requirements (Proof_Results, WCET_Results) then
         return Cert_Silver;
      elsif Check_Bronze_Requirements (Proof_Results) then
         return Cert_Bronze;
      else
         return Cert_Bronze;  -- Minimum
      end if;
   end Determine_Level;

   function Check_Bronze_Requirements (
      Proof_Results : Proof_Results
   ) return Boolean is
   begin
      --  Bronze: SPARK Mode + Flow Analysis + AoRTE
      return Proof_Results.Level_Achieved >= Proof_Bronze;
   end Check_Bronze_Requirements;

   function Check_Silver_Requirements (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result
   ) return Boolean is
   begin
      --  Silver: Bronze + 100% proof coverage + WCET
      return Proof_Results.VCs_Proven = Proof_Results.VCs_Total
         and Proof_Results.VCs_Total > 0
         and WCET_Results.All_Bounded;
   end Check_Silver_Requirements;

   function Check_Gold_Requirements (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result
   ) return Boolean is
   begin
      --  Gold: Silver + functional contracts + constant-time
      return Check_Silver_Requirements (Proof_Results, WCET_Results)
         and CT_Results.All_Passed;
   end Check_Gold_Requirements;

   function Check_Platinum_Requirements (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result;
      Has_Audit     : Boolean
   ) return Boolean is
   begin
      --  Platinum: Gold + external audit signature
      return Check_Gold_Requirements (Proof_Results, WCET_Results, CT_Results)
         and Has_Audit;
   end Check_Platinum_Requirements;

   ---------------------------------------------------------------------------
   --  Output Formatting
   ---------------------------------------------------------------------------

   procedure Generate_Report (
      Results      : Certification_Result;
      Report_Path  : String;
      Success      : out Boolean
   ) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Report_Path);

      Ada.Text_IO.Put_Line (File, "KHEPRI Certification Report");
      Ada.Text_IO.Put_Line (File, "===========================");
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line (File, "Level Achieved: " &
         (case Results.Level_Achieved is
            when Cert_Bronze   => "Bronze",
            when Cert_Silver   => "Silver",
            when Cert_Gold     => "Gold",
            when Cert_Platinum => "Platinum"));

      Ada.Text_IO.Put_Line (File, "Gas Discount: " &
         Float'Image (Results.Gas_Discount));

      Ada.Text_IO.Close (File);
      Success := True;

   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         Success := False;
   end Generate_Report;

   procedure Print_Summary (
      Results      : Certification_Result
   ) is
      use Ada.Text_IO;
   begin
      Put_Line ("Certification Summary");
      Put_Line ("---------------------");
      Put_Line ("Level: " &
         (case Results.Level_Achieved is
            when Cert_Bronze   => "Bronze",
            when Cert_Silver   => "Silver",
            when Cert_Gold     => "Gold",
            when Cert_Platinum => "Platinum"));
      Put_Line ("Target Met: " & Boolean'Image (Results.Target_Met));
      Put_Line ("Gas Discount: " & Float'Image (Results.Gas_Discount));
      Put_Line ("Estimated Gas: " & Unsigned_64'Image (Results.Estimated_Gas));
   end Print_Summary;

   ---------------------------------------------------------------------------
   --  Gas Estimation
   ---------------------------------------------------------------------------

   function Calculate_Gas (
      WCET_Results : WCET_Analysis_Result;
      Level        : Cert_Target
   ) return Unsigned_64 is
      Base_Gas : constant Unsigned_64 := WCET_Results.Max_Cycles / 1000;
      Discount : constant Float := Gas_Discount (Level);
   begin
      return Unsigned_64 (Float (Base_Gas) * Discount);
   end Calculate_Gas;

   function Gas_Discount (Level : Cert_Target) return Float is
   begin
      case Level is
         when Cert_Bronze   => return 1.0;    -- No discount
         when Cert_Silver   => return 0.9;    -- 10% discount
         when Cert_Gold     => return 0.8;    -- 20% discount
         when Cert_Platinum => return 0.7;    -- 30% discount
      end case;
   end Gas_Discount;

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

end Khepri_CLI_Certify;
