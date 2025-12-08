pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;

--  KHEPRI Compiler: SPARK Contract Compiler
--
--  This package provides the compilation infrastructure for KHEPRI
--  smart contracts written in SPARK. It handles:
--  - Source parsing and validation
--  - SPARK subset enforcement
--  - Bytecode generation
--  - Manifest generation
--  - Certification artifact collection
--
--  Compilation Pipeline:
--  1. Parse SPARK source
--  2. Validate SPARK subset compliance
--  3. Run GNATprove for proofs
--  4. Generate AVM bytecode
--  5. Collect certification artifacts
--  6. Generate deployment manifest
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 7.1: Compiler

package Khepri_Compiler with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Compilation Error Types
   ---------------------------------------------------------------------------

   --  Error severity
   type Error_Severity is (
      Severity_Hint,      --  Style suggestion
      Severity_Warning,   --  Potential issue
      Severity_Error,     --  Must fix to compile
      Severity_Fatal      --  Cannot continue
   );

   --  Error category
   type Error_Category is (
      Cat_Syntax,         --  Syntax error
      Cat_Semantic,       --  Semantic error
      Cat_SPARK,          --  SPARK subset violation
      Cat_Type,           --  Type error
      Cat_Proof,          --  Proof failure
      Cat_Resource,       --  Resource limit exceeded
      Cat_Internal        --  Internal compiler error
   );

   --  Compiler error entry
   type Compiler_Error is record
      Severity    : Error_Severity;
      Category    : Error_Category;
      File        : Bounded_String;
      Line        : Natural;
      Column      : Natural;
      Message     : Bounded_String;
      Suggestion  : Bounded_String;  --  How to fix
   end record;

   --  Maximum errors per compilation
   Max_Errors : constant := 256;
   type Error_Index is range 0 .. Max_Errors - 1;
   type Error_Array is array (Error_Index) of Compiler_Error;

   ---------------------------------------------------------------------------
   --  Compiler Configuration
   ---------------------------------------------------------------------------

   --  Optimization level
   type Optimization_Level is (
      Opt_None,       --  No optimization (-O0)
      Opt_Debug,      --  Debug-friendly (-O1)
      Opt_Standard,   --  Standard optimization (-O2)
      Opt_Size,       --  Size optimization (-Os)
      Opt_Full        --  Full optimization (-O3)
   );

   --  Target certification level to aim for
   type Target_Cert_Level is (
      Target_None,    --  No certification checks
      Target_Bronze,  --  Run flow analysis
      Target_Silver,  --  Full proofs + WCET
      Target_Gold,    --  Silver + CT analysis
      Target_Platinum --  Gold + audit prep
   );

   --  Compiler configuration
   type Compiler_Config is record
      --  Output options
      Output_Dir         : Bounded_String;
      Manifest_Name      : Bounded_String;

      --  Optimization
      Opt_Level          : Optimization_Level;

      --  Certification
      Target_Level       : Target_Cert_Level;
      Run_Proofs         : Boolean;
      Proof_Timeout      : Natural;  --  Seconds per VC

      --  Validation
      Strict_SPARK       : Boolean;  --  Reject any non-SPARK
      Check_Style        : Boolean;  --  Check coding style
      Max_Complexity     : Natural;  --  Max cyclomatic complexity

      --  Gas estimation
      Estimate_Gas       : Boolean;
      Max_Function_Gas   : Word64;

      --  Debug
      Debug_Info         : Boolean;
      Verbose            : Boolean;
   end record;

   Default_Config : constant Compiler_Config := (
      Output_Dir         => Empty_String,
      Manifest_Name      => Empty_String,
      Opt_Level          => Opt_Standard,
      Target_Level       => Target_Silver,
      Run_Proofs         => True,
      Proof_Timeout      => 60,
      Strict_SPARK       => True,
      Check_Style        => True,
      Max_Complexity     => 50,
      Estimate_Gas       => True,
      Max_Function_Gas   => 10_000_000,
      Debug_Info         => False,
      Verbose            => False
   );

   ---------------------------------------------------------------------------
   --  Compilation Session
   ---------------------------------------------------------------------------

   type Compilation_Session is private;

   --  Compilation statistics
   type Compilation_Stats is record
      Files_Processed    : Natural;
      Lines_Compiled     : Natural;
      Functions_Compiled : Natural;
      Bytecode_Size      : Natural;
      Proof_VCs_Total    : Natural;
      Proof_VCs_Passed   : Natural;
      Compile_Time_Ms    : Natural;
      Proof_Time_Ms      : Natural;
   end record;

   --  Compilation result
   type Compilation_Result is (
      Result_Success,       --  All good
      Result_Warnings,      --  Success with warnings
      Result_Errors,        --  Compilation failed
      Result_Proof_Failed,  --  Proofs didn"t pass
      Result_Internal_Error --  Compiler bug
   );

   --  Create compilation session
   function Create_Session (
      Config : Compiler_Config
   ) return Compilation_Session with
      Global => null;

   --  Add source file to compilation
   procedure Add_Source (
      Session     : in out Compilation_Session;
      Source_Path : in     String;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => Source_Path'Length <= 256;

   --  Compile all sources
   procedure Compile (
      Session : in Out Compilation_Session;
      Result  : out    Compilation_Result
   ) with
      Global => null;

   --  Get compilation statistics
   function Get_Stats (
      Session : Compilation_Session
   ) return Compilation_Stats with
      Global => null;

   --  Get compilation errors
   procedure Get_Errors (
      Session     : in  Compilation_Session;
      Errors      : out Error_Array;
      Error_Count : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Source Validation
   ---------------------------------------------------------------------------

   --  SPARK subset compliance check
   type SPARK_Compliance is (
      Full_SPARK,       --  100% SPARK compliant
      Partial_SPARK,    --  Some SPARK_Mode Off sections
      Non_SPARK,        --  Not SPARK at all
      Unknown           --  Not yet analyzed
   );

   --  Source file info
   type Source_Info is record
      Path              : Bounded_String;
      Hash              : Hash256;
      Lines             : Natural;
      Functions         : Natural;
      Compliance        : SPARK_Compliance;
      Has_Globals       : Boolean;
      Has_Proof_Aspects : Boolean;
   end record;

   --  Validate source file
   procedure Validate_Source (
      Session     : in     Compilation_Session;
      Source_Path : in     String;
      Info        : out    Source_Info;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => Source_Path'Length <= 256;

   ---------------------------------------------------------------------------
   --  Bytecode Generation
   ---------------------------------------------------------------------------

   --  Generated bytecode info
   type Bytecode_Info is record
      Size           : Natural;
      Function_Count : Natural;
      Entry_Points   : Natural;
      Gas_Estimate   : Word64;
      Is_Valid       : Boolean;
   end record;

   --  Generate bytecode from compiled session
   procedure Generate_Bytecode (
      Session  : in     Compilation_Session;
      Bytecode : out    Byte_Array;
      Size     : out    Natural;
      Info     : out    Bytecode_Info;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre    => Bytecode'Length >= 1048576;  --  1MB max

   ---------------------------------------------------------------------------
   --  Manifest Generation
   ---------------------------------------------------------------------------

   --  Generate deployment manifest
   procedure Generate_Manifest (
      Session      : in     Compilation_Session;
      Manifest     : out    Byte_Array;
      Manifest_Size: out    Natural;
      Success      : out    Boolean
   ) with
      Global => null,
      Pre    => Manifest'Length >= 65536;

   ---------------------------------------------------------------------------
   --  Function Table
   ---------------------------------------------------------------------------

   --  Compiled function entry
   type Function_Entry is record
      Name           : Bounded_String;
      Selector       : Bytes4;
      Bytecode_Start : Natural;
      Bytecode_Size  : Natural;
      Stack_Depth    : Natural;
      Local_Count    : Natural;
      Gas_Estimate   : Word64;
      Is_External    : Boolean;
      Is_Payable     : Boolean;
      Is_View        : Boolean;
   end record;

   Max_Functions : constant := 256;
   type Function_Index is range 0 .. Max_Functions - 1;
   type Function_Table is array (Function_Index) of Function_Entry;

   --  Get function table from compilation
   procedure Get_Functions (
      Session        : in  Compilation_Session;
      Functions      : out Function_Table;
      Function_Count : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Gas Estimation
   ---------------------------------------------------------------------------

   --  Gas estimate for a function
   type Gas_Estimate is record
      Min_Gas        : Word64;
      Max_Gas        : Word64;
      Average_Gas    : Word64;
      Proven_Bound   : Boolean;  --  Is max proven via WCET?
   end record;

   --  Estimate gas for a function
   function Estimate_Function_Gas (
      Session  : Compilation_Session;
      Selector : Bytes4
   ) return Gas_Estimate with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Artifacts
   ---------------------------------------------------------------------------

   --  Collected certification artifacts
   type Cert_Artifacts is record
      Proof_Report_Hash    : Hash256;
      WCET_Report_Hash     : Hash256;
      CT_Analysis_Hash     : Hash256;
      Coverage_Report_Hash : Hash256;
      Source_Hash          : Hash256;
      Achieved_Level       : Khepri_Certification.Certification_Level;
   end record;

   --  Collect certification artifacts
   procedure Collect_Artifacts (
      Session   : in     Compilation_Session;
      Artifacts : out    Cert_Artifacts;
      Success   : out    Boolean
   ) with
      Global => null;

private

   Max_Sources : constant := 64;
   type Source_Index is range 0 .. Max_Sources - 1;
   type Source_Array is array (Source_Index) of Source_Info;

   type Compilation_Session is record
      Config         : Compiler_Config;
      Sources        : Source_Array;
      Source_Count   : Natural;
      Functions      : Function_Table;
      Function_Count : Natural;
      Errors         : Error_Array;
      Error_Count    : Natural;
      Stats          : Compilation_Stats;
      Is_Compiled    : Boolean;
      Result         : Compilation_Result;
   end record;

end Khepri_Compiler;
