pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;

--  KHEPRI Verifier: Contract Verification Toolchain
--
--  This package provides comprehensive verification for KHEPRI
--  smart contracts. It performs:
--  - Bytecode validation
--  - Manifest verification
--  - Signature verification (ML-DSA-87)
--  - Certification level verification
--  - Dependency verification
--  - Runtime checks validation
--
--  Verification Pipeline:
--  1. Parse manifest
--  2. Verify bytecode integrity
--  3. Check certification artifacts
--  4. Validate signatures
--  5. Verify dependencies
--  6. Issue verification report
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 7.2: Verifier

package Khepri_Verifier with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Verification Result Types
   ---------------------------------------------------------------------------

   --  Verification check type
   type Check_Type is (
      Check_Manifest_Format,     --  Manifest structure
      Check_Bytecode_Integrity,  --  Bytecode hash matches
      Check_Certification,       --  Cert level valid
      Check_Proof_Artifacts,     --  Proof reports valid
      Check_Signatures,          --  Signatures verify
      Check_Dependencies,        --  Deps available/valid
      Check_ABI_Consistency,     --  ABI matches bytecode
      Check_Gas_Bounds,          --  Gas estimates valid
      Check_Security_Policy      --  Security requirements met
   );

   --  Check result
   type Check_Result is (
      Check_Passed,      --  Check successful
      Check_Warning,     --  Passed with warnings
      Check_Failed,      --  Check failed
      Check_Skipped,     --  Check not applicable
      Check_Error        --  Error during check
   );

   --  Individual verification check
   type Verification_Check is record
      Check_Id    : Check_Type;
      Result      : Check_Result;
      Message     : Bounded_String;
      Details     : Bounded_String;
   end record;

   --  Maximum checks per verification
   Max_Checks : constant := 64;
   type Check_Index is range 0 .. Max_Checks - 1;
   type Check_Array is array (Check_Index) of Verification_Check;

   ---------------------------------------------------------------------------
   --  Verification Configuration
   ---------------------------------------------------------------------------

   --  Verification strictness
   type Strictness_Level is (
      Strict_Minimal,    --  Basic checks only
      Strict_Standard,   --  Standard verification
      Strict_Full,       --  All checks including optional
      Strict_Paranoid    --  Extra security checks
   );

   --  Verification configuration
   type Verifier_Config is record
      --  Strictness
      Strictness         : Strictness_Level;

      --  What to verify
      Verify_Bytecode    : Boolean;
      Verify_Signatures  : Boolean;
      Verify_Proofs      : Boolean;
      Verify_Dependencies: Boolean;
      Verify_Gas         : Boolean;

      --  Certification
      Min_Cert_Level     : Khepri_Certification.Certification_Level;

      --  Output
      Generate_Report    : Boolean;
      Report_Path        : Bounded_String;

      --  Network
      Fetch_Dependencies : Boolean;  --  Fetch deps from network
      Network_Timeout    : Natural;  --  Seconds
   end record;

   Default_Verifier_Config : constant Verifier_Config := (
      Strictness         => Strict_Standard,
      Verify_Bytecode    => True,
      Verify_Signatures  => True,
      Verify_Proofs      => True,
      Verify_Dependencies=> True,
      Verify_Gas         => True,
      Min_Cert_Level     => Level_Bronze,
      Generate_Report    => True,
      Report_Path        => Empty_String,
      Fetch_Dependencies => False,
      Network_Timeout    => 30
   );

   ---------------------------------------------------------------------------
   --  Verification Session
   ---------------------------------------------------------------------------

   type Verification_Session is private;

   --  Overall verification result
   type Verification_Result is (
      Verified_Full,      --  All checks passed
      Verified_Partial,   --  Some warnings
      Verification_Failed,--  Critical check failed
      Verification_Error  --  Error during verification
   );

   --  Verification statistics
   type Verification_Stats is record
      Checks_Passed     : Natural;
      Checks_Warnings   : Natural;
      Checks_Failed     : Natural;
      Checks_Skipped    : Natural;
      Checks_Error      : Natural;
      Total_Time_Ms     : Natural;
   end record;

   --  Create verification session
   function Create_Session (
      Config : Verifier_Config
   ) return Verification_Session with
      Global => null;

   --  Verify contract from manifest
   procedure Verify_Manifest (
      Session      : in Out Verification_Session;
      Manifest     : in     Byte_Array;
      Result       : out    Verification_Result
   ) with
      Global => null;

   --  Verify contract from files
   procedure Verify_Contract (
      Session       : in Out Verification_Session;
      Manifest_Path : in     String;
      Bytecode_Path : in     String;
      Result        : out    Verification_Result
   ) with
      Global => null,
      Pre    => Manifest_Path'Length <= 256 and Bytecode_Path'Length <= 256;

   --  Get verification statistics
   function Get_Stats (
      Session : Verification_Session
   ) return Verification_Stats with
      Global => null;

   --  Get individual check results
   procedure Get_Checks (
      Session     : in  Verification_Session;
      Checks      : out Check_Array;
      Check_Count : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Bytecode Verification
   ---------------------------------------------------------------------------

   --  Bytecode validation result
   type Bytecode_Validation is record
      Is_Valid       : Boolean;
      Size_Valid     : Boolean;
      Header_Valid   : Boolean;
      Opcodes_Valid  : Boolean;
      Jumps_Valid    : Boolean;
      Stack_Balanced : Boolean;
      Error_Message  : Bounded_String;
   end record;

   --  Validate bytecode structure
   procedure Validate_Bytecode (
      Session    : in     Verification_Session;
      Bytecode   : in     Byte_Array;
      Validation : out    Bytecode_Validation
   ) with
      Global => null;

   --  Verify bytecode hash
   function Verify_Bytecode_Hash (
      Bytecode      : Byte_Array;
      Expected_Hash : Hash256
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Signature Verification
   ---------------------------------------------------------------------------

   --  Signature verification result
   type Signature_Validation is record
      Is_Valid          : Boolean;
      Signer_Address    : Khepri_Types.Address;
      Signer_Verified   : Boolean;  --  Known/trusted signer
      Algorithm_Valid   : Boolean;  --  ML-DSA-87
      Timestamp_Valid   : Boolean;
      Error_Message     : Bounded_String;
   end record;

   --  Verify manifest signature
   procedure Verify_Signature (
      Session    : in     Verification_Session;
      Manifest   : in     Byte_Array;
      Validation : out    Signature_Validation
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Verification
   ---------------------------------------------------------------------------

   --  Certification validation result
   type Cert_Validation is record
      Claimed_Level     : Khepri_Certification.Certification_Level;
      Verified_Level    : Khepri_Certification.Certification_Level;
      Level_Matches     : Boolean;
      Proofs_Valid      : Boolean;
      WCET_Valid        : Boolean;
      CT_Analysis_Valid : Boolean;
      Audit_Valid       : Boolean;
      Error_Message     : Bounded_String;
   end record;

   --  Verify certification claims
   procedure Verify_Certification (
      Session    : in     Verification_Session;
      Manifest   : in     Byte_Array;
      Validation : out    Cert_Validation
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Dependency Verification
   ---------------------------------------------------------------------------

   --  Dependency status
   type Dependency_Status is (
      Dep_Verified,     --  Dependency valid
      Dep_Missing,      --  Dependency not found
      Dep_Invalid,      --  Dependency invalid
      Dep_Untrusted,    --  Dependency not trusted
      Dep_Version_Mismatch  --  Wrong version
   );

   --  Dependency validation entry
   type Dependency_Validation is record
      Name          : Bounded_String;
      Address       : Khepri_Types.Address;
      Status        : Dependency_Status;
      Required_Cert : Khepri_Certification.Certification_Level;
      Actual_Cert   : Khepri_Certification.Certification_Level;
   end record;

   Max_Dependencies : constant := 32;
   type Dep_Index is range 0 .. Max_Dependencies - 1;
   type Dep_Validation_Array is array (Dep_Index) of Dependency_Validation;

   --  Verify all dependencies
   procedure Verify_Dependencies (
      Session     : in     Verification_Session;
      Manifest    : in     Byte_Array;
      Deps        : out    Dep_Validation_Array;
      Dep_Count   : out    Natural;
      All_Valid   : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   --  Report format
   type Report_Format is (
      Format_Text,
      Format_JSON,
      Format_HTML,
      Format_SARIF
   );

   --  Generate verification report
   procedure Generate_Report (
      Session : in     Verification_Session;
      Format  : in     Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 65536;

   --  Generate summary line
   function Summary_Line (
      Session : Verification_Session
   ) return String with
      Global => null;

   ---------------------------------------------------------------------------
   --  Quick Verification
   ---------------------------------------------------------------------------

   --  Quick verification for deployment
   function Quick_Verify (
      Manifest : Byte_Array;
      Min_Cert : Khepri_Certification.Certification_Level
   ) return Boolean with
      Global => null;

   --  Verify contract is deployable
   function Is_Deployable (
      Session : Verification_Session
   ) return Boolean with
      Global => null;

private

   type Verification_Session is record
      Config      : Verifier_Config;
      Checks      : Check_Array;
      Check_Count : Natural;
      Stats       : Verification_Stats;
      Result      : Verification_Result;
      Is_Complete : Boolean;
   end record;

end Khepri_Verifier;
