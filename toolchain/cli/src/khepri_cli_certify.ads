-------------------------------------------------------------------------------
--  KHEPRI CLI - Certify Command
--  Generates certification manifests for verified contracts
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Khepri_CLI; use Khepri_CLI;
with Khepri_CLI_Prove; use Khepri_CLI_Prove;

package Khepri_CLI_Certify with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Certification Configuration
   ---------------------------------------------------------------------------

   type Certify_Options is record
      Project_File   : String (1 .. 256);
      Target_Level   : Cert_Target;
      Output_Path    : String (1 .. 256);
      Include_WCET   : Boolean;
      Include_CT     : Boolean;   -- Constant-time analysis
      Auditor_Key    : String (1 .. 256);  -- Path to auditor key (Platinum)
      Force          : Boolean;   -- Skip cache check
   end record;

   Default_Certify_Options : constant Certify_Options := (
      Project_File   => (others => ' '),
      Target_Level   => Cert_Gold,
      Output_Path    => (others => ' '),
      Include_WCET   => True,
      Include_CT     => True,
      Auditor_Key    => (others => ' '),
      Force          => False
   );

   ---------------------------------------------------------------------------
   --  Certification Execution
   ---------------------------------------------------------------------------

   --  Execute full certification flow
   procedure Execute_Certify (
      Options  : Certify_Options;
      Results  : out Certification_Result;
      Output   : out CLI_Output
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Result
   ---------------------------------------------------------------------------

   type Certification_Result is record
      Level_Achieved : Cert_Target;
      Target_Met     : Boolean;

      --  Proof results
      Proof_Results  : Proof_Results;

      --  WCET results
      WCET_Analyzed  : Boolean;
      Max_WCET       : Unsigned_64;  -- cycles
      WCET_Count     : Natural;

      --  Constant-time analysis
      CT_Analyzed    : Boolean;
      CT_Passed      : Boolean;
      CT_Violations  : Natural;

      --  Manifest
      Manifest_Path  : String (1 .. 256);
      Manifest_Hash  : Byte_Array (0 .. 31);

      --  Gas estimate
      Estimated_Gas  : Unsigned_64;
      Gas_Discount   : Float;
   end record;

   ---------------------------------------------------------------------------
   --  Individual Certification Steps
   ---------------------------------------------------------------------------

   --  Step 1: Run proofs
   procedure Run_Certification_Proofs (
      Options  : Certify_Options;
      Results  : out Proof_Results;
      Success  : out Boolean
   ) with
      Global => null;

   --  Step 2: Run WCET analysis
   procedure Run_WCET_Analysis (
      Project_File : String;
      WCET_Results : out WCET_Analysis_Result;
      Success      : out Boolean
   ) with
      Global => null;

   type WCET_Analysis_Result is record
      Functions      : array (0 .. 63) of Function_WCET;
      Func_Count     : Natural;
      Max_Cycles     : Unsigned_64;
      All_Bounded    : Boolean;
   end record;

   type Function_WCET is record
      Name           : String (1 .. 64);
      Name_Length    : Natural;
      Max_Cycles     : Unsigned_64;
      Avg_Cycles     : Unsigned_64;
      Proven         : Boolean;
   end record;

   --  Step 3: Run constant-time analysis
   procedure Run_CT_Analysis (
      Project_File : String;
      CT_Results   : out CT_Analysis_Result;
      Success      : out Boolean
   ) with
      Global => null;

   type CT_Analysis_Result is record
      Functions      : array (0 .. 63) of Function_CT;
      Func_Count     : Natural;
      All_Passed     : Boolean;
      Violations     : Natural;
   end record;

   type Function_CT is record
      Name           : String (1 .. 64);
      Name_Length    : Natural;
      Is_CT          : Boolean;
      Violation_Type : CT_Violation;
   end record;

   type CT_Violation is (
      CT_None,
      CT_Data_Branch,      -- Branch on secret data
      CT_Data_Index,       -- Array index depends on secret
      CT_Variable_Loop,    -- Loop bounds depend on secret
      CT_Early_Exit       -- Early return based on secret
   );

   --  Step 4: Generate manifest
   procedure Generate_Manifest (
      Options       : Certify_Options;
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result;
      Manifest_Path : out String;
      Success       : out Boolean
   ) with
      Global => null;

   --  Step 5: Sign manifest (Platinum only)
   procedure Sign_Manifest (
      Manifest_Path : String;
      Auditor_Key   : String;
      Success       : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Level Determination
   ---------------------------------------------------------------------------

   --  Determine achievable level from results
   function Determine_Level (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result;
      Has_Audit     : Boolean
   ) return Cert_Target with
      Global => null;

   --  Check specific level requirements
   function Check_Bronze_Requirements (
      Proof_Results : Proof_Results
   ) return Boolean with
      Global => null;

   function Check_Silver_Requirements (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result
   ) return Boolean with
      Global => null;

   function Check_Gold_Requirements (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result
   ) return Boolean with
      Global => null;

   function Check_Platinum_Requirements (
      Proof_Results : Proof_Results;
      WCET_Results  : WCET_Analysis_Result;
      CT_Results    : CT_Analysis_Result;
      Has_Audit     : Boolean
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Output Formatting
   ---------------------------------------------------------------------------

   --  Generate certification report
   procedure Generate_Report (
      Results      : Certification_Result;
      Report_Path  : String;
      Success      : out Boolean
   ) with
      Global => null;

   --  Print certification summary to console
   procedure Print_Summary (
      Results      : Certification_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Gas Estimation
   ---------------------------------------------------------------------------

   --  Calculate gas from WCET
   function Calculate_Gas (
      WCET_Results : WCET_Analysis_Result;
      Level        : Cert_Target
   ) return Unsigned_64 with
      Global => null;

   --  Calculate discount factor
   function Gas_Discount (Level : Cert_Target) return Float with
      Global => null,
      Post => Gas_Discount'Result in 0.7 .. 1.0;

end Khepri_CLI_Certify;
