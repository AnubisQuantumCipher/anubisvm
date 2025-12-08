pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;

--  KHEPRI Constant-Time Analysis
--
--  This package provides constant-time analysis for KHEPRI contracts.
--  Constant-time execution is critical for cryptographic operations to
--  prevent timing side-channel attacks.
--
--  Analysis Checks:
--  - No secret-dependent branches
--  - No secret-dependent memory access
--  - No secret-dependent loop bounds
--  - No early termination based on secrets
--  - Uniform execution paths
--
--  Annotation System:
--  - CT_Secret: Mark variables as secret
--  - CT_Public: Mark variables as public
--  - CT_Safe: Mark functions as constant-time safe
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6.4: Constant-Time Analysis
--  - "Verifying Constant-Time Implementations" (Barthe et al.)

package Khepri_CT_Analysis with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Security Classification
   ---------------------------------------------------------------------------

   --  Data sensitivity classification
   type Sensitivity is (
      Public,     --  Can be used freely
      Secret,     --  Must not influence timing
      Tainted,    --  Derived from secret
      Unknown     --  Not yet classified
   );

   --  Classification for a variable/expression
   type Classification is record
      Level       : Sensitivity;
      Source      : Bounded_String;  --  Where classification came from
      Line        : Natural;
      Column      : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  CT Violation Types
   ---------------------------------------------------------------------------

   --  Types of constant-time violations
   type Violation_Type is (
      --  Control flow violations
      V_Secret_Branch,        --  Branch condition depends on secret
      V_Secret_Loop,          --  Loop bound depends on secret
      V_Secret_Return,        --  Early return based on secret

      --  Memory access violations
      V_Secret_Index,         --  Array index depends on secret
      V_Secret_Pointer,       --  Pointer value depends on secret

      --  Timing violations
      V_Variable_Time_Op,     --  Operation with variable timing
      V_Cache_Timing,         --  Potential cache timing leak

      --  Other
      V_Unknown
   );

   --  Severity level
   type Severity is (
      Sev_Info,       --  Informational
      Sev_Warning,    --  Potential issue
      Sev_Error,      --  Definite violation
      Sev_Critical    --  Critical security issue
   );

   --  CT violation entry
   type CT_Violation is record
      Vtype       : Violation_Type;
      Sev         : Severity;
      File        : Bounded_String;
      Line        : Natural;
      Column      : Natural;
      Message     : Bounded_String;
      Suggestion  : Bounded_String;  --  How to fix
   end record;

   --  Maximum violations per analysis
   Max_Violations : constant := 1024;
   type Violation_Index is range 0 .. Max_Violations - 1;
   type Violation_Array is array (Violation_Index) of CT_Violation;

   ---------------------------------------------------------------------------
   --  CT Annotations
   ---------------------------------------------------------------------------

   --  Annotation types
   type Annotation_Type is (
      Ann_Secret,       --  Mark as secret
      Ann_Public,       --  Mark as public
      Ann_CT_Safe,      --  Mark function as CT-safe
      Ann_CT_Unsafe,    --  Mark function as intentionally CT-unsafe
      Ann_Declassify    --  Intentional declassification
   );

   --  Annotation entry
   type CT_Annotation is record
      Ann_Type   : Annotation_Type;
      Target     : Bounded_String;  --  Variable/function name
      File       : Bounded_String;
      Line       : Natural;
      Reason     : Bounded_String;  --  Why this annotation
   end record;

   Max_Annotations : constant := 512;
   type Annotation_Index is range 0 .. Max_Annotations - 1;
   type Annotation_Array is array (Annotation_Index) of CT_Annotation;

   ---------------------------------------------------------------------------
   --  Analysis Configuration
   ---------------------------------------------------------------------------

   type CT_Config is record
      --  Analysis options
      Check_Branches    : Boolean;  --  Check branch conditions
      Check_Memory      : Boolean;  --  Check memory access
      Check_Loops       : Boolean;  --  Check loop bounds
      Check_Cache       : Boolean;  --  Check cache timing
      Check_Operations  : Boolean;  --  Check operation timing

      --  Strictness
      Strict_Mode       : Boolean;  --  Treat warnings as errors
      Require_Annotations : Boolean;  --  All secrets must be annotated

      --  Output
      Generate_Report   : Boolean;
      Report_Path       : Bounded_String;
   end record;

   Default_CT_Config : constant CT_Config := (
      Check_Branches    => True,
      Check_Memory      => True,
      Check_Loops       => True,
      Check_Cache       => True,
      Check_Operations  => True,
      Strict_Mode       => False,
      Require_Annotations => False,
      Generate_Report   => True,
      Report_Path       => Empty_String
   );

   Gold_CT_Config : constant CT_Config := (
      Check_Branches    => True,
      Check_Memory      => True,
      Check_Loops       => True,
      Check_Cache       => True,
      Check_Operations  => True,
      Strict_Mode       => True,
      Require_Annotations => True,
      Generate_Report   => True,
      Report_Path       => Empty_String
   );

   ---------------------------------------------------------------------------
   --  Analysis Session
   ---------------------------------------------------------------------------

   type CT_Session is private;

   --  Analysis statistics
   type CT_Stats is record
      Functions_Analyzed : Natural;
      Functions_Safe     : Natural;
      Functions_Unsafe   : Natural;
      Total_Violations   : Natural;
      Critical_Count     : Natural;
      Error_Count        : Natural;
      Warning_Count      : Natural;
      Info_Count         : Natural;
      Analysis_Time      : Natural;  --  Milliseconds
   end record;

   --  Create analysis session
   function Create_Session (
      Config : CT_Config
   ) return CT_Session with
      Global => null;

   --  Run CT analysis
   procedure Run_Analysis (
      Session       : in Out CT_Session;
      Contract_Path : in     String;
      Success       : out    Boolean
   ) with
      Global => null,
      Pre    => Contract_Path'Length <= 256;

   --  Get analysis statistics
   function Get_Stats (Session : CT_Session) return CT_Stats with
      Global => null;

   --  Get violations
   procedure Get_Violations (
      Session         : in  CT_Session;
      Violations      : out Violation_Array;
      Violation_Count : out Natural
   ) with
      Global => null;

   --  Check if analysis passed for Gold level
   function Passes_Gold (Session : CT_Session) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Function Analysis
   ---------------------------------------------------------------------------

   --  Function CT status
   type Function_CT_Status is record
      Name           : Bounded_String;
      Selector       : Bytes4;
      Is_CT_Safe     : Boolean;
      Violation_Count: Natural;
      Has_Annotation : Boolean;
      Annotation     : Annotation_Type;
   end record;

   Max_Functions : constant := 128;
   type Function_Index is range 0 .. Max_Functions - 1;
   type Function_Status_Array is array (Function_Index) of Function_CT_Status;

   --  Get per-function CT status
   procedure Get_Function_Status (
      Session        : in  CT_Session;
      Functions      : out Function_Status_Array;
      Function_Count : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Cryptographic Function Analysis
   ---------------------------------------------------------------------------

   --  Known crypto operations that should be CT
   type Crypto_Op is (
      Op_Hash,          --  Hash function
      Op_Sign,          --  Signature
      Op_Verify,        --  Signature verification
      Op_Encrypt,       --  Encryption
      Op_Decrypt,       --  Decryption
      Op_Key_Gen,       --  Key generation
      Op_Key_Derive,    --  Key derivation
      Op_Compare,       --  Secret comparison
      Op_Other
   );

   --  Crypto operation analysis
   type Crypto_Analysis is record
      Op_Type       : Crypto_Op;
      Function_Name : Bounded_String;
      Is_CT_Safe    : Boolean;
      Risk_Level    : Severity;
      Notes         : Bounded_String;
   end record;

   Max_Crypto_Ops : constant := 64;
   type Crypto_Index is range 0 .. Max_Crypto_Ops - 1;
   type Crypto_Array is array (Crypto_Index) of Crypto_Analysis;

   --  Analyze cryptographic operations
   procedure Analyze_Crypto (
      Session      : in Out CT_Session;
      Results      : out    Crypto_Array;
      Result_Count : out    Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   --  Report format
   type CT_Report_Format is (
      Format_Text,
      Format_JSON,
      Format_HTML,
      Format_SARIF
   );

   --  Generate CT analysis report
   procedure Generate_Report (
      Session : in     CT_Session;
      Format  : in     CT_Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 65536;

   --  Generate summary
   function Summary_Line (Session : CT_Session) return String with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Integration
   ---------------------------------------------------------------------------

   --  Generate CT analysis hash for certification
   function Generate_CT_Hash (Session : CT_Session) return Hash256 with
      Global => null;

   --  Check if CT analysis meets Gold requirements
   function Meets_Gold_CT (Session : CT_Session) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  CT-Safe Patterns
   ---------------------------------------------------------------------------

   --  Constant-time comparison (should use this pattern)
   function CT_Equal (
      A, B   : Byte_Array;
      Length : Natural
   ) return Boolean with
      Global => null,
      Pre    => A'Length >= Length and B'Length >= Length;

   --  Constant-time select (if condition then A else B)
   function CT_Select (
      Condition : Boolean;
      A, B      : Byte
   ) return Byte with
      Global => null;

   --  Constant-time conditional copy
   procedure CT_Conditional_Copy (
      Condition : in     Boolean;
      Src       : in     Byte_Array;
      Dst       : in Out Byte_Array
   ) with
      Global => null,
      Pre    => Src'Length = Dst'Length;

private

   type CT_Session is record
      Config          : CT_Config;
      Stats           : CT_Stats;
      Violations      : Violation_Array;
      Violation_Count : Natural;
      Annotations     : Annotation_Array;
      Annotation_Count: Natural;
      Is_Complete     : Boolean;
      Error_Msg       : Bounded_String;
   end record;

end Khepri_CT_Analysis;
