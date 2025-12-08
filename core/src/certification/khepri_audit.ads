pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;
with Khepri_Crypto; use Khepri_Crypto;

--  KHEPRI Audit: Third-Party Security Audit Support
--
--  This package provides the infrastructure for third-party security
--  audits required for Platinum certification. It handles:
--  - Audit package generation
--  - Auditor identity verification (via ML-DSA-87)
--  - Audit report registration
--  - Audit registry management
--  - Dispute resolution
--
--  Audit Process:
--  1. Developer generates audit package
--  2. Auditor reviews code, proofs, and artifacts
--  3. Auditor signs audit report with ML-DSA-87
--  4. Report is registered on-chain
--  5. Contract receives Platinum certification
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6.5: Audit Support

package Khepri_Audit with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Auditor Types
   ---------------------------------------------------------------------------

   --  Auditor status
   type Auditor_Status is (
      Status_Active,      --  Can perform audits
      Status_Suspended,   --  Temporarily suspended
      Status_Revoked,     --  Permanently revoked
      Status_Pending      --  Awaiting approval
   );

   --  Auditor tier (determines what contracts they can audit)
   type Auditor_Tier is (
      Tier_1,    --  Basic contracts (< $1M TVL)
      Tier_2,    --  Standard contracts (< $10M TVL)
      Tier_3,    --  High-value contracts (< $100M TVL)
      Tier_4     --  Critical infrastructure (unlimited)
   );

   --  Auditor profile
   type Auditor_Profile is record
      --  Identity
      Address          : Khepri_Types.Address;
      Public_Key       : MLDSA_Public_Key;
      Name             : Bounded_String;
      Organization     : Bounded_String;

      --  Status
      Status           : Auditor_Status;
      Tier             : Auditor_Tier;

      --  Statistics
      Audits_Completed : Natural;
      Audits_Disputed  : Natural;
      Registration_Time: Word64;
      Last_Audit_Time  : Word64;

      --  Verification
      Identity_Verified: Boolean;
      KYC_Complete     : Boolean;
   end record;

   --  Empty auditor
   Null_Auditor : constant Auditor_Profile := (
      Address          => (others => 0),
      Public_Key       => (others => 0),
      Name             => Empty_String,
      Organization     => Empty_String,
      Status           => Status_Pending,
      Tier             => Tier_1,
      Audits_Completed => 0,
      Audits_Disputed  => 0,
      Registration_Time=> 0,
      Last_Audit_Time  => 0,
      Identity_Verified=> False,
      KYC_Complete     => False
   );

   ---------------------------------------------------------------------------
   --  Audit Report Types
   ---------------------------------------------------------------------------

   --  Audit finding severity
   type Finding_Severity is (
      Finding_Info,       --  Informational
      Finding_Low,        --  Low severity
      Finding_Medium,     --  Medium severity
      Finding_High,       --  High severity
      Finding_Critical    --  Critical vulnerability
   );

   --  Audit finding status
   type Finding_Status is (
      Status_Open,        --  Not yet addressed
      Status_Acknowledged,--  Developer aware
      Status_Fixed,       --  Issue fixed
      Status_Wont_Fix,    --  Will not fix (with reason)
      Status_Disputed     --  Developer disputes finding
   );

   --  Individual audit finding
   type Audit_Finding is record
      ID          : Natural;
      Severity    : Finding_Severity;
      Status      : Finding_Status;
      Title       : Bounded_String;
      Description : Bounded_String;
      Location    : Bounded_String;  --  File:line
      Recommendation : Bounded_String;
      Response    : Bounded_String;  --  Developer response
   end record;

   --  Maximum findings per audit
   Max_Findings : constant := 256;
   type Finding_Index is range 0 .. Max_Findings - 1;
   type Finding_Array is array (Finding_Index) of Audit_Finding;

   --  Audit verdict
   type Audit_Verdict is (
      Verdict_Pass,           --  No critical/high issues
      Verdict_Pass_With_Notes,--  Pass with observations
      Verdict_Fail,           --  Critical issues found
      Verdict_Incomplete      --  Audit not finished
   );

   --  Complete audit report
   type Audit_Report is record
      --  Identity
      Report_ID        : Hash256;
      Contract_Address : Khepri_Types.Address;
      Contract_Hash    : Hash256;
      Auditor          : Khepri_Types.Address;

      --  Timestamps
      Audit_Start      : Word64;
      Audit_End        : Word64;
      Report_Time      : Word64;

      --  Scope
      Version_Audited  : Bounded_String;
      Commit_Hash      : Hash256;
      Scope_Description: Bounded_String;

      --  Findings
      Findings         : Finding_Array;
      Finding_Count    : Natural;
      Critical_Count   : Natural;
      High_Count       : Natural;
      Medium_Count     : Natural;
      Low_Count        : Natural;
      Info_Count       : Natural;

      --  Verdict
      Verdict          : Audit_Verdict;
      Summary          : Bounded_String;

      --  Signature
      Signature        : MLDSA_Signature;
      Is_Signed        : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Audit Package Generation
   ---------------------------------------------------------------------------

   --  Audit package contents
   type Audit_Package is record
      --  Contract info
      Contract_Name    : Bounded_String;
      Contract_Version : Bounded_String;
      Contract_Address : Khepri_Types.Address;
      Contract_Hash    : Hash256;

      --  Source code hash
      Source_Hash      : Hash256;

      --  Proof artifacts
      Proof_Hash       : Hash256;
      WCET_Hash        : Hash256;
      CT_Analysis_Hash : Hash256;

      --  Certification status
      Current_Level    : Khepri_Certification.Certification_Level;

      --  Package metadata
      Generated_At     : Word64;
      Expires_At       : Word64;
      Package_Hash     : Hash256;
   end record;

   --  Generate audit package for a contract
   procedure Generate_Audit_Package (
      Contract_Path : in     String;
      Package_Out   : out    Audit_Package;
      Success       : out    Boolean
   ) with
      Global => null,
      Pre    => Contract_Path'Length <= 256;

   --  Serialize audit package to bytes
   procedure Serialize_Package (
      Package_In : in  Audit_Package;
      Data       : out Byte_Array;
      Size       : out Natural;
      Success    : out Boolean
   ) with
      Global => null,
      Pre    => Data'Length >= 16384;

   --  Parse audit package from bytes
   procedure Parse_Package (
      Data       : in  Byte_Array;
      Package_Out: out Audit_Package;
      Success    : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Audit Report Operations
   ---------------------------------------------------------------------------

   --  Create new audit report
   function Create_Report (
      Contract  : Khepri_Types.Address;
      Auditor   : Khepri_Types.Address
   ) return Audit_Report with
      Global => null;

   --  Add finding to report
   procedure Add_Finding (
      Report  : in Out Audit_Report;
      Finding : in     Audit_Finding;
      Success : out    Boolean
   ) with
      Global => null;

   --  Set verdict
   procedure Set_Verdict (
      Report  : in Out Audit_Report;
      Verdict : in     Audit_Verdict;
      Summary : in     Bounded_String
   ) with
      Global => null;

   --  Sign report with auditor"s ML-DSA-87 key
   procedure Sign_Report (
      Report     : in Out Audit_Report;
      Secret_Key : in     MLDSA_Secret_Key;
      Success    : out    Boolean
   ) with
      Global => null;

   --  Verify report signature
   function Verify_Report (
      Report     : Audit_Report;
      Public_Key : MLDSA_Public_Key
   ) return Boolean with
      Global => null;

   --  Calculate report hash
   function Report_Hash (Report : Audit_Report) return Hash256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Auditor Registry
   ---------------------------------------------------------------------------

   --  Maximum registered auditors
   Max_Auditors : constant := 1024;
   type Auditor_Index is range 0 .. Max_Auditors - 1;
   type Auditor_Array is array (Auditor_Index) of Auditor_Profile;

   --  Register new auditor
   procedure Register_Auditor (
      Profile : in     Auditor_Profile;
      Success : out    Boolean
   ) with
      Global => null;

   --  Get auditor profile
   function Get_Auditor (
      Auditor_Address : Khepri_Types.Address
   ) return Auditor_Profile with
      Global => null;

   --  Check if address is registered auditor
   function Is_Registered_Auditor (
      Auditor_Address : Khepri_Types.Address
   ) return Boolean with
      Global => null;

   --  Check if auditor can audit given contract value
   function Can_Audit (
      Auditor_Address : Khepri_Types.Address;
      Contract_Value  : Uint256
   ) return Boolean with
      Global => null;

   --  Update auditor status
   procedure Update_Auditor_Status (
      Auditor_Address : in     Khepri_Types.Address;
      New_Status      : in     Auditor_Status;
      Success         : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  On-Chain Audit Registry
   ---------------------------------------------------------------------------

   --  Registry entry for completed audit
   type Registry_Audit_Entry is record
      Contract_Address : Khepri_Types.Address;
      Report_Hash      : Hash256;
      Auditor          : Khepri_Types.Address;
      Verdict          : Audit_Verdict;
      Timestamp        : Word64;
      Is_Valid         : Boolean;
      Is_Disputed      : Boolean;
   end record;

   --  Maximum audits in registry
   Max_Registry_Audits : constant := 65536;
   type Registry_Audit_Index is range 0 .. Max_Registry_Audits - 1;

   --  Register completed audit on-chain
   procedure Register_Audit (
      Report  : in     Audit_Report;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Report.Is_Signed;

   --  Get audit for contract
   function Get_Contract_Audit (
      Contract : Khepri_Types.Address
   ) return Registry_Audit_Entry with
      Global => null;

   --  Check if contract has valid audit
   function Has_Valid_Audit (
      Contract : Khepri_Types.Address
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Dispute Resolution
   ---------------------------------------------------------------------------

   --  Dispute status
   type Dispute_Status is (
      Dispute_Open,       --  Under review
      Dispute_Resolved,   --  Resolved
      Dispute_Rejected,   --  Dispute rejected
      Dispute_Upheld      --  Dispute upheld (audit invalidated)
   );

   --  Dispute entry
   type Dispute_Entry is record
      ID              : Hash256;
      Contract        : Khepri_Types.Address;
      Report_Hash     : Hash256;
      Disputer        : Khepri_Types.Address;
      Reason          : Bounded_String;
      Evidence_Hash   : Hash256;
      Status          : Dispute_Status;
      Filed_At        : Word64;
      Resolved_At     : Word64;
      Resolution      : Bounded_String;
   end record;

   --  File a dispute against an audit
   procedure File_Dispute (
      Report_Hash   : in     Hash256;
      Reason        : in     Bounded_String;
      Evidence_Hash : in     Hash256;
      Disputer      : in     Khepri_Types.Address;
      Success       : out    Boolean
   ) with
      Global => null;

   --  Resolve a dispute
   procedure Resolve_Dispute (
      Dispute_ID  : in     Hash256;
      Resolution  : in     Dispute_Status;
      Notes       : in     Bounded_String;
      Success     : out    Boolean
   ) with
      Global => null;

   --  Get dispute status
   function Get_Dispute (
      Dispute_ID : Hash256
   ) return Dispute_Entry with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Integration
   ---------------------------------------------------------------------------

   --  Check if contract meets Platinum audit requirements
   function Meets_Platinum_Audit (
      Contract : Khepri_Types.Address
   ) return Boolean with
      Global => null;

   --  Get Platinum certification status
   function Get_Platinum_Status (
      Contract : Khepri_Types.Address
   ) return Platinum_Requirements with
      Global => null;

end Khepri_Audit;
