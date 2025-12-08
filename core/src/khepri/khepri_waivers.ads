-------------------------------------------------------------------------------
--  KHEPRI - Proof Waiver Management System
--  Manages SPARK proof waivers with on-chain auditability
--
--  Waivers allow controlled exceptions to formal verification requirements
--  when proper justification and mitigation are provided.
--
--  Key Features:
--  - Structured waiver records with full provenance
--  - On-chain recording for immutable audit trail
--  - Automatic expiration enforcement
--  - Integration with gnatprove workflow
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Khepri_Waivers with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum active waivers in the system
   Max_Active_Waivers    : constant := 100;

   --  Maximum justification length
   Max_Justification_Len : constant := 4096;

   --  Maximum mitigation length
   Max_Mitigation_Len    : constant := 2048;

   --  Waiver ID format: WAIVER-YYYYMMDD-NNNN (20 chars)
   Waiver_ID_Length      : constant := 20;

   --  Unit name maximum length
   Max_Unit_Name_Len     : constant := 256;

   --  VC ID maximum length
   Max_VC_ID_Len         : constant := 128;

   --  Hash size (SHA3-256)
   Hash_Size             : constant := 32;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Waiver category
   type Waiver_Category is (
      Timing_Sensitive,     --  Constant-time operations
      External_FFI,         --  Foreign function interface
      Platform_Dependent,   --  Platform-specific code
      Performance_Critical, --  Performance optimizations
      Legacy_Interface      --  Legacy system interfaces
   );

   --  Waiver status
   type Waiver_Status is (
      Active,      --  Currently in effect
      Expired,     --  Past expiration date
      Revoked,     --  Manually revoked
      Superseded   --  Replaced by newer waiver
   );

   --  Severity level
   type Severity_Level is (Low, Medium, High, Critical);

   --  Date representation (days since 2020-01-01)
   subtype Waiver_Date is Natural range 0 .. 36500;  -- ~100 years

   --  Waiver index
   subtype Waiver_Index is Natural range 0 .. Max_Active_Waivers - 1;

   --  String types for waiver fields
   subtype Waiver_ID_String is String (1 .. Waiver_ID_Length);
   subtype Unit_Name_String is String (1 .. Max_Unit_Name_Len);

   --  Waiver record
   type Waiver_Record is record
      ID             : Waiver_ID_String;
      Category       : Waiver_Category;
      Status         : Waiver_Status;
      Severity       : Severity_Level;

      --  Target information
      Unit_Name      : Unit_Name_String;
      Unit_Name_Len  : Natural;
      VC_ID          : String (1 .. Max_VC_ID_Len);
      VC_ID_Len      : Natural;

      --  Documentation
      Justification  : String (1 .. Max_Justification_Len);
      Just_Len       : Natural;
      Mitigation     : String (1 .. Max_Mitigation_Len);
      Mit_Len        : Natural;

      --  Approval chain
      Reviewer_Hash  : Byte_Array (0 .. Hash_Size - 1);
      Approved_Date  : Waiver_Date;
      Expires_Date   : Waiver_Date;

      --  On-chain recording
      On_Chain_Hash  : Byte_Array (0 .. Hash_Size - 1);
      Chain_Block    : Unsigned_64;
      Recorded       : Boolean;

      --  Validity flag
      Valid          : Boolean;
   end record;

   type Waiver_Array is array (Waiver_Index) of Waiver_Record;

   --  Waiver registry state
   type Waiver_Registry is record
      Waivers        : Waiver_Array;
      Count          : Natural;
      Last_Updated   : Unsigned_64;

      --  Statistics
      Active_Count   : Natural;
      Expired_Count  : Natural;
      High_Sev_Count : Natural;
   end record;

   --  Validation result
   type Validation_Result is (
      Valid,
      Invalid_ID,
      Invalid_Category,
      Invalid_Dates,
      Missing_Justification,
      Missing_Mitigation,
      Expired,
      Not_Recorded,
      Hash_Mismatch
   );

   --  Query result
   type Query_Result is record
      Found          : Boolean;
      Index          : Waiver_Index;
      Waiver         : Waiver_Record;
   end record;

   --  Waiver statistics
   type Waiver_Stats is record
      Total_Waivers     : Natural;
      Active_Waivers    : Natural;
      Expired_Waivers   : Natural;
      High_Severity     : Natural;
      Expiring_Soon     : Natural;  -- Within 30 days
      Coverage_Percent  : Natural;  -- Proved VCs / Total VCs
   end record;

   ---------------------------------------------------------------------------
   --  Registry Management
   ---------------------------------------------------------------------------

   --  Initialize empty registry
   procedure Init_Registry (
      Registry       : out Waiver_Registry
   ) with
      Global => null,
      Post => Registry.Count = 0
              and Registry.Active_Count = 0
              and Registry.Expired_Count = 0;

   --  Add waiver to registry
   procedure Add_Waiver (
      Registry       : in out Waiver_Registry;
      Waiver         : Waiver_Record;
      Index          : out Waiver_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Registry.Count < Max_Active_Waivers,
      Post => (if Success then Registry.Count = Registry.Count'Old + 1);

   --  Remove waiver from registry (marks as revoked)
   procedure Remove_Waiver (
      Registry       : in Out Waiver_Registry;
      Index          : Waiver_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Index < Registry.Count;

   --  Update waiver status
   procedure Update_Status (
      Registry       : in Out Waiver_Registry;
      Index          : Waiver_Index;
      New_Status     : Waiver_Status
   ) with
      Global => null,
      Pre => Index < Registry.Count;

   --  Process expirations based on current date
   procedure Process_Expirations (
      Registry       : in Out Waiver_Registry;
      Current_Date   : Waiver_Date;
      Expired_Count  : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Waiver Creation
   ---------------------------------------------------------------------------

   --  Create new waiver record
   procedure Create_Waiver (
      ID             : Waiver_ID_String;
      Category       : Waiver_Category;
      Unit_Name      : String;
      VC_ID          : String;
      Justification  : String;
      Mitigation     : String;
      Approved_Date  : Waiver_Date;
      Expires_Date   : Waiver_Date;
      Waiver         : out Waiver_Record;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Unit_Name'Length <= Max_Unit_Name_Len
             and VC_ID'Length <= Max_VC_ID_Len
             and Justification'Length <= Max_Justification_Len
             and Mitigation'Length <= Max_Mitigation_Len
             and Approved_Date < Expires_Date,
      Post => (if Success then Waiver.Valid and Waiver.Status = Active);

   --  Set reviewer for waiver
   procedure Set_Reviewer (
      Waiver         : in Out Waiver_Record;
      Reviewer_Hash  : Byte_Array
   ) with
      Global => null,
      Pre => Reviewer_Hash'Length = Hash_Size and Waiver.Valid;

   ---------------------------------------------------------------------------
   --  Validation
   ---------------------------------------------------------------------------

   --  Validate waiver record
   function Validate_Waiver (
      Waiver         : Waiver_Record;
      Current_Date   : Waiver_Date
   ) return Validation_Result with
      Global => null;

   --  Check if waiver is currently active
   function Is_Active (
      Waiver         : Waiver_Record;
      Current_Date   : Waiver_Date
   ) return Boolean with
      Global => null,
      Post => Is_Active'Result = (Waiver.Valid
                                  and Waiver.Status = Active
                                  and Current_Date <= Waiver.Expires_Date);

   --  Check if unit is covered by waiver
   function Is_Unit_Waived (
      Registry       : Waiver_Registry;
      Unit_Name      : String;
      Current_Date   : Waiver_Date
   ) return Boolean with
      Global => null;

   --  Check if specific VC is waived
   function Is_VC_Waived (
      Registry       : Waiver_Registry;
      Unit_Name      : String;
      VC_ID          : String;
      Current_Date   : Waiver_Date
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Query Operations
   ---------------------------------------------------------------------------

   --  Find waiver by ID
   procedure Find_By_ID (
      Registry       : Waiver_Registry;
      ID             : Waiver_ID_String;
      Result         : out Query_Result
   ) with
      Global => null;

   --  Find waivers by unit name
   procedure Find_By_Unit (
      Registry       : Waiver_Registry;
      Unit_Name      : String;
      Indices        : out Waiver_Array;
      Count          : out Natural
   ) with
      Global => null,
      Pre => Unit_Name'Length <= Max_Unit_Name_Len;

   --  Find waivers by category
   procedure Find_By_Category (
      Registry       : Waiver_Registry;
      Category       : Waiver_Category;
      Indices        : out Waiver_Array;
      Count          : out Natural
   ) with
      Global => null;

   --  Get expiring waivers (within N days)
   procedure Find_Expiring (
      Registry       : Waiver_Registry;
      Current_Date   : Waiver_Date;
      Days_Ahead     : Natural;
      Indices        : out Waiver_Array;
      Count          : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  On-Chain Recording
   ---------------------------------------------------------------------------

   --  Compute waiver hash for on-chain recording
   procedure Compute_Waiver_Hash (
      Waiver         : Waiver_Record;
      Hash           : out Byte_Array
   ) with
      Global => null,
      Pre => Hash'Length = Hash_Size;

   --  Record waiver on chain (prepares the transaction)
   procedure Prepare_Chain_Record (
      Waiver         : Waiver_Record;
      TX_Data        : out Byte_Array;
      TX_Length      : out Natural
   ) with
      Global => null,
      Pre => TX_Data'Length >= 256;

   --  Mark waiver as recorded on chain
   procedure Mark_Recorded (
      Waiver         : in Out Waiver_Record;
      Block_Number   : Unsigned_64;
      TX_Hash        : Byte_Array
   ) with
      Global => null,
      Pre => TX_Hash'Length = Hash_Size,
      Post => Waiver.Recorded;

   --  Verify on-chain record
   function Verify_Chain_Record (
      Waiver         : Waiver_Record;
      Chain_Hash     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Chain_Hash'Length = Hash_Size;

   ---------------------------------------------------------------------------
   --  Statistics and Reporting
   ---------------------------------------------------------------------------

   --  Get registry statistics
   function Get_Stats (
      Registry       : Waiver_Registry;
      Current_Date   : Waiver_Date
   ) return Waiver_Stats with
      Global => null;

   --  Check if registry exceeds thresholds
   function Check_Thresholds (
      Stats          : Waiver_Stats;
      Max_Active     : Natural;
      Max_High_Sev   : Natural
   ) return Boolean with
      Global => null,
      Post => Check_Thresholds'Result = (Stats.Active_Waivers <= Max_Active
                                         and Stats.High_Severity <= Max_High_Sev);

   --  Get severity for category
   function Category_Severity (Category : Waiver_Category) return Severity_Level with
      Global => null;

   --  Get max duration for category (in days)
   function Category_Max_Duration (Category : Waiver_Category) return Natural with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize waiver to bytes
   procedure Serialize_Waiver (
      Waiver         : Waiver_Record;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 8192;

   --  Deserialize waiver from bytes
   procedure Deserialize_Waiver (
      Input          : Byte_Array;
      Waiver         : out Waiver_Record;
      Success        : out Boolean
   ) with
      Global => null;

   --  Serialize registry to bytes
   procedure Serialize_Registry (
      Registry       : Waiver_Registry;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 1024 * 1024;  -- 1MB max

   --  Deserialize registry from bytes
   procedure Deserialize_Registry (
      Input          : Byte_Array;
      Registry       : out Waiver_Registry;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Date Utilities
   ---------------------------------------------------------------------------

   --  Convert date components to Waiver_Date
   function Make_Date (
      Year           : Natural;
      Month          : Natural;
      Day            : Natural
   ) return Waiver_Date with
      Global => null,
      Pre => Year >= 2020 and Year <= 2120
             and Month in 1 .. 12
             and Day in 1 .. 31;

   --  Get days until expiration
   function Days_Until_Expiry (
      Waiver         : Waiver_Record;
      Current_Date   : Waiver_Date
   ) return Integer with
      Global => null;

   ---------------------------------------------------------------------------
   --  CI/CD Integration
   ---------------------------------------------------------------------------

   --  Generate gnatprove exclusion list
   procedure Generate_Exclusions (
      Registry       : Waiver_Registry;
      Current_Date   : Waiver_Date;
      Output         : out String;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 4096;

   --  Validate that exclusions match active waivers
   function Validate_Exclusions (
      Registry       : Waiver_Registry;
      Exclusions     : String;
      Current_Date   : Waiver_Date
   ) return Boolean with
      Global => null;

end Khepri_Waivers;
