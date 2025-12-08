pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;

--  KHEPRI Prover: GNATprove Integration
--
--  This package provides the interface between KHEPRI certification
--  and the GNATprove formal verification tool. It handles:
--  - Running automated proof sessions
--  - Categorizing verification conditions (VCs)
--  - Caching proof results
--  - Generating proof reports
--
--  Proof Levels:
--  - Level 0: Fast checks only
--  - Level 1: Basic proofs with small timeout
--  - Level 2: Moderate proofs with medium timeout
--  - Level 3: Advanced proofs with longer timeout
--  - Level 4: Full proofs with extended timeout
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6.3: GNATprove Integration
--  - SPARK User"s Guide

package Khepri_Prover with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Prover Configuration
   ---------------------------------------------------------------------------

   --  Proof effort levels
   type Proof_Level is (
      Level_0,  --  Fast checks, 1s timeout
      Level_1,  --  Basic, 10s timeout
      Level_2,  --  Moderate, 30s timeout
      Level_3,  --  Advanced, 60s timeout
      Level_4   --  Full, 120s timeout
   );

   --  Timeout in seconds for each level
   function Get_Timeout (Level : Proof_Level) return Natural with
      Global => null;

   --  Prover configuration
   type Prover_Config is record
      Level           : Proof_Level;
      Parallel_Jobs   : Natural;      --  Number of parallel provers
      Memlimit        : Natural;      --  Memory limit in MB
      Steps           : Natural;      --  Max proof steps
      Timeout         : Natural;      --  Override timeout
      Warnings_As_Errors : Boolean;   --  Treat warnings as errors
      Report_All      : Boolean;      --  Report all VCs
      Cache_Enabled   : Boolean;      --  Use proof cache
   end record;

   Default_Config : constant Prover_Config := (
      Level           => Level_2,
      Parallel_Jobs   => 4,
      Memlimit        => 2000,
      Steps           => 10000,
      Timeout         => 0,  --  Use level default
      Warnings_As_Errors => False,
      Report_All      => True,
      Cache_Enabled   => True
   );

   Silver_Config : constant Prover_Config := (
      Level           => Level_4,
      Parallel_Jobs   => 8,
      Memlimit        => 4000,
      Steps           => 50000,
      Timeout         => 120,
      Warnings_As_Errors => True,
      Report_All      => True,
      Cache_Enabled   => True
   );

   ---------------------------------------------------------------------------
   --  Verification Condition Types
   ---------------------------------------------------------------------------

   --  VC categories
   type VC_Category is (
      --  Run-time checks
      VC_Division_Check,
      VC_Index_Check,
      VC_Overflow_Check,
      VC_Range_Check,
      VC_Length_Check,
      VC_Discriminant_Check,
      VC_Tag_Check,

      --  Assertions
      VC_Precondition,
      VC_Postcondition,
      VC_Contract_Case,
      VC_Loop_Invariant,
      VC_Loop_Variant,
      VC_Assert,

      --  Flow analysis
      VC_Initialization,
      VC_Aliasing,

      --  Other
      VC_Unknown
   );

   --  VC status
   type VC_Status is (
      VC_Proved,
      VC_Not_Proved,
      VC_Timeout,
      VC_Error,
      VC_Skipped
   );

   --  Individual VC result
   type VC_Result is record
      Category    : VC_Category;
      Status      : VC_Status;
      File        : Bounded_String;
      Line        : Natural;
      Column      : Natural;
      Message     : Bounded_String;
      Prover_Time : Natural;  --  Milliseconds
   end record;

   --  Maximum VCs per contract
   Max_VCs : constant := 4096;
   type VC_Index is range 0 .. Max_VCs - 1;
   type VC_Array is array (VC_Index) of VC_Result;

   ---------------------------------------------------------------------------
   --  Proof Session
   ---------------------------------------------------------------------------

   --  Proof session state
   type Proof_Session is private;

   --  Session statistics
   type Session_Stats is record
      Total_VCs      : Natural;
      Proved_VCs     : Natural;
      Unproved_VCs   : Natural;
      Timeout_VCs    : Natural;
      Error_VCs      : Natural;
      Skipped_VCs    : Natural;
      Total_Time     : Natural;  --  Milliseconds
      Max_Time       : Natural;  --  Longest single proof
   end record;

   --  Create new proof session
   function Create_Session (
      Config : Prover_Config
   ) return Proof_Session with
      Global => null;

   --  Run proofs on a contract
   procedure Run_Proofs (
      Session      : in Out Proof_Session;
      Contract_Path: in     String;
      Success      : out    Boolean
   ) with
      Global => null,
      Pre    => Contract_Path'Length <= 256;

   --  Get session statistics
   function Get_Stats (
      Session : Proof_Session
   ) return Session_Stats with
      Global => null;

   --  Get all VC results
   procedure Get_VCs (
      Session  : in  Proof_Session;
      VCs      : out VC_Array;
      VC_Count : out Natural
   ) with
      Global => null;

   --  Check if all VCs proved
   function All_Proved (Session : Proof_Session) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Proof Cache
   ---------------------------------------------------------------------------

   --  Cache entry
   type Cache_Entry is record
      Contract_Hash : Hash256;
      Config_Hash   : Hash256;
      Stats         : Session_Stats;
      Timestamp     : Word64;
      Is_Valid      : Boolean;
   end record;

   --  Check if cached result exists
   function Has_Cached_Result (
      Contract_Hash : Hash256;
      Config        : Prover_Config
   ) return Boolean with
      Global => null;

   --  Get cached result
   function Get_Cached_Result (
      Contract_Hash : Hash256;
      Config        : Prover_Config
   ) return Session_Stats with
      Global => null;

   --  Store result in cache
   procedure Cache_Result (
      Contract_Hash : in Hash256;
      Config        : in Prover_Config;
      Stats         : in Session_Stats
   ) with
      Global => null;

   --  Invalidate cache entry
   procedure Invalidate_Cache (
      Contract_Hash : in Hash256
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Proof Report Generation
   ---------------------------------------------------------------------------

   --  Report format
   type Report_Format is (
      Format_Text,    --  Plain text
      Format_JSON,    --  JSON format
      Format_HTML,    --  HTML report
      Format_SARIF    --  SARIF (Static Analysis Results Interchange Format)
   );

   --  Generate proof report
   procedure Generate_Report (
      Session : in     Proof_Session;
      Format  : in     Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 65536;

   --  Generate summary line
   function Summary_Line (Session : Proof_Session) return String with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Integration
   ---------------------------------------------------------------------------

   --  Check if proof results meet Bronze requirements
   function Meets_Bronze_Proofs (
      Session : Proof_Session
   ) return Boolean with
      Global => null;

   --  Check if proof results meet Silver requirements
   function Meets_Silver_Proofs (
      Session : Proof_Session
   ) return Boolean with
      Global => null;

   --  Generate proof artifact hash for certification
   function Generate_Proof_Hash (
      Session : Proof_Session
   ) return Hash256 with
      Global => null;

   --  Verify proof artifact integrity
   function Verify_Proof_Artifact (
      Contract_Hash : Hash256;
      Proof_Hash    : Hash256
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  WCET Analysis Integration
   ---------------------------------------------------------------------------

   --  WCET analysis result
   type WCET_Analysis is record
      Function_Name  : Bounded_String;
      Selector       : Bytes4;
      Cycles         : Natural;
      Gas_Bound      : Gas_Amount;
      Proven         : Boolean;
      Path_Count     : Natural;
      Max_Loop_Bound : Natural;
   end record;

   Max_WCET_Entries : constant := 128;
   type WCET_Index is range 0 .. Max_WCET_Entries - 1;
   type WCET_Array is array (WCET_Index) of WCET_Analysis;

   --  Run WCET analysis
   procedure Analyze_WCET (
      Session      : in Out Proof_Session;
      Contract_Path: in     String;
      Results      : out    WCET_Array;
      Result_Count : out    Natural;
      Success      : out    Boolean
   ) with
      Global => null,
      Pre    => Contract_Path'Length <= 256;

   --  Check if all functions have proven WCET bounds
   function All_WCET_Bounded (
      Results      : WCET_Array;
      Result_Count : Natural
   ) return Boolean with
      Global => null,
      Pre    => Result_Count <= Max_WCET_Entries;

private

   type Proof_Session is record
      Config      : Prover_Config;
      Stats       : Session_Stats;
      VCs         : VC_Array;
      VC_Count    : Natural;
      Is_Complete : Boolean;
      Error_Msg   : Bounded_String;
   end record;

end Khepri_Prover;
