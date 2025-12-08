pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;

--  KHEPRI Manifest: Contract Manifest Format
--
--  This package defines the manifest format for KHEPRI contracts.
--  The manifest contains metadata about the contract, its certification
--  status, ABI definition, and proof artifacts.
--
--  Manifest Structure:
--  1. Header (version, magic, sizes)
--  2. Contract metadata (name, version, author)
--  3. ABI specification (functions, events, types)
--  4. Certification data (level, proofs, WCET)
--  5. Dependencies (imported contracts)
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6.2: Manifest Format

package Khepri_Manifest with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Manifest Header
   ---------------------------------------------------------------------------

   --  Magic number for manifest files
   Manifest_Magic : constant Word32 := 16#4B485052#;  --  "KHPR"

   --  Current manifest version
   Manifest_Version : constant Word32 := 1;

   --  Manifest header structure
   type Manifest_Header is record
      Magic           : Word32;    --  Must be Manifest_Magic
      Version         : Word32;    --  Manifest format version
      Flags           : Word32;    --  Feature flags
      Header_Size     : Word32;    --  Size of header
      Metadata_Offset : Word32;    --  Offset to metadata section
      ABI_Offset      : Word32;    --  Offset to ABI section
      Cert_Offset     : Word32;    --  Offset to certification section
      Deps_Offset     : Word32;    --  Offset to dependencies section
      Total_Size      : Word32;    --  Total manifest size
      Checksum        : Hash256;   --  SHA3-256 of manifest content
   end record;

   --  Header flags
   Has_Certification : constant Word32 := 16#0001#;
   Has_WCET_Bounds   : constant Word32 := 16#0002#;
   Has_CT_Analysis   : constant Word32 := 16#0004#;
   Has_Audit_Report  : constant Word32 := 16#0008#;
   Is_Upgradeable    : constant Word32 := 16#0010#;
   Is_Library        : constant Word32 := 16#0020#;

   ---------------------------------------------------------------------------
   --  Contract Metadata
   ---------------------------------------------------------------------------

   --  Maximum string lengths
   Max_Name_Length    : constant := 64;
   Max_Version_Length : constant := 32;
   Max_Author_Length  : constant := 128;
   Max_License_Length : constant := 32;
   Max_URL_Length     : constant := 256;

   --  String types for metadata
   subtype Name_String is String (1 .. Max_Name_Length);
   subtype Version_String is String (1 .. Max_Version_Length);
   subtype Author_String is String (1 .. Max_Author_Length);
   subtype License_String is String (1 .. Max_License_Length);
   subtype URL_String is String (1 .. Max_URL_Length);

   --  Contract metadata structure
   type Contract_Metadata is record
      --  Identity
      Name           : Bounded_String;
      Symbol         : Bounded_String;  --  Short symbol (e.g., "MTK")
      Version_Major  : Natural;
      Version_Minor  : Natural;
      Version_Patch  : Natural;

      --  Authorship
      Author         : Bounded_String;
      License        : Bounded_String;  --  SPDX identifier
      Homepage       : Bounded_String;
      Repository     : Bounded_String;

      --  Technical
      Compiler_Version : Bounded_String;
      SPARK_Version    : Bounded_String;
      Target_Arch      : Bounded_String;

      --  Timestamps
      Created_At     : Word64;
      Updated_At     : Word64;
   end record;

   Empty_Metadata : constant Contract_Metadata := (
      Name           => Empty_String,
      Symbol         => Empty_String,
      Version_Major  => 0,
      Version_Minor  => 0,
      Version_Patch  => 0,
      Author         => Empty_String,
      License        => Empty_String,
      Homepage       => Empty_String,
      Repository     => Empty_String,
      Compiler_Version => Empty_String,
      SPARK_Version    => Empty_String,
      Target_Arch      => Empty_String,
      Created_At     => 0,
      Updated_At     => 0
   );

   ---------------------------------------------------------------------------
   --  ABI Specification
   ---------------------------------------------------------------------------

   --  Function mutability
   type Function_Mutability is (
      Mutability_Pure,       --  No state read/write
      Mutability_View,       --  State read only
      Mutability_Nonpayable, --  State read/write, no value
      Mutability_Payable     --  State read/write, accepts value
   );

   --  Parameter type
   type Param_Type is (
      Type_U256,
      Type_Address,
      Type_Bool,
      Type_Bytes32,
      Type_Bytes,
      Type_String,
      Type_Array
   );

   --  Function parameter
   type Function_Param is record
      Name       : Bounded_String;
      Param_Type : Khepri_Manifest.Param_Type;
      Indexed    : Boolean;  --  For events
   end record;

   --  Maximum parameters per function
   Max_Params : constant := 16;
   type Param_Index is range 0 .. Max_Params - 1;
   type Param_Array is array (Param_Index) of Function_Param;

   --  Function ABI entry
   type Function_ABI is record
      Name        : Bounded_String;
      Selector    : Bytes4;
      Mutability  : Function_Mutability;
      Inputs      : Param_Array;
      Input_Count : Natural;
      Outputs     : Param_Array;
      Output_Count: Natural;
      Gas_Estimate: Gas_Amount;  --  Estimated gas cost
      WCET_Bound  : Gas_Amount;  --  Proven WCET (if certified)
   end record;

   --  Event ABI entry
   type Event_ABI is record
      Name        : Bounded_String;
      Signature   : Hash256;     --  Keccak256 of signature
      Params      : Param_Array;
      Param_Count : Natural;
      Anonymous   : Boolean;
   end record;

   --  Error ABI entry
   type Error_ABI is record
      Name        : Bounded_String;
      Selector    : Bytes4;
      Params      : Param_Array;
      Param_Count : Natural;
   end record;

   --  Maximum ABI entries
   Max_Functions : constant := 128;
   Max_Events    : constant := 64;
   Max_Errors    : constant := 64;

   type Function_Index is range 0 .. Max_Functions - 1;
   type Event_Index is range 0 .. Max_Events - 1;
   type Error_Index is range 0 .. Max_Errors - 1;

   type Function_Array is array (Function_Index) of Function_ABI;
   type Event_Array is array (Event_Index) of Event_ABI;
   type Error_Array is array (Error_Index) of Error_ABI;

   --  Complete ABI specification
   type ABI_Specification is record
      Functions      : Function_Array;
      Function_Count : Natural;
      Events         : Event_Array;
      Event_Count    : Natural;
      Errors         : Error_Array;
      Error_Count    : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Certification Data
   ---------------------------------------------------------------------------

   --  WCET bound entry
   type WCET_Entry is record
      Function_Selector : Bytes4;
      Cycles           : Natural;
      Gas_Bound        : Gas_Amount;
      Proven           : Boolean;
   end record;

   type WCET_Index is range 0 .. Max_Functions - 1;
   type WCET_Array is array (WCET_Index) of WCET_Entry;

   --  Certification data section
   type Certification_Data is record
      Status           : Certification_Status;
      WCET_Bounds      : WCET_Array;
      WCET_Count       : Natural;
      Proof_Hash       : Hash256;    --  Hash of GNATprove output
      CT_Analysis_Hash : Hash256;    --  Hash of CT analysis
      Audit_Hash       : Hash256;    --  Hash of audit report (if any)
   end record;

   ---------------------------------------------------------------------------
   --  Dependencies
   ---------------------------------------------------------------------------

   --  Dependency entry
   type Dependency_Entry is record
      Name              : Bounded_String;
      Contract_Address  : Address;
      Version_Required  : Bounded_String;
      Min_Cert_Level    : Khepri_Certification.Certification_Level;
   end record;

   Max_Dependencies : constant := 32;
   type Dep_Index is range 0 .. Max_Dependencies - 1;
   type Dependency_Array is array (Dep_Index) of Dependency_Entry;

   --  Dependencies section
   type Dependencies_Section is record
      Deps       : Dependency_Array;
      Dep_Count  : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Complete Manifest
   ---------------------------------------------------------------------------

   type Contract_Manifest is record
      Header       : Manifest_Header;
      Metadata     : Contract_Metadata;
      ABI          : ABI_Specification;
      Certification: Certification_Data;
      Dependencies : Dependencies_Section;
   end record;

   ---------------------------------------------------------------------------
   --  Manifest Operations
   ---------------------------------------------------------------------------

   --  Parse manifest from bytes
   procedure Parse_Manifest (
      Data     : in  Byte_Array;
      Manifest : out Contract_Manifest;
      Success  : out Boolean
   ) with
      Global => null;

   --  Serialize manifest to bytes
   procedure Serialize_Manifest (
      Manifest : in  Contract_Manifest;
      Data     : out Byte_Array;
      Size     : out Natural;
      Success  : out Boolean
   ) with
      Global => null,
      Pre    => Data'Length >= 65536;

   --  Validate manifest structure
   function Validate_Manifest (
      Manifest : Contract_Manifest
   ) return Boolean with
      Global => null;

   --  Calculate manifest checksum
   function Calculate_Checksum (
      Manifest : Contract_Manifest
   ) return Hash256 with
      Global => null;

   --  Get function ABI by selector
   function Get_Function (
      Manifest : Contract_Manifest;
      Selector : Bytes4
   ) return Function_ABI with
      Global => null;

   --  Get event ABI by signature
   function Get_Event (
      Manifest  : Contract_Manifest;
      Signature : Hash256
   ) return Event_ABI with
      Global => null;

   --  Get WCET bound for function
   function Get_WCET_Bound (
      Manifest : Contract_Manifest;
      Selector : Bytes4
   ) return Gas_Amount with
      Global => null;

   ---------------------------------------------------------------------------
   --  Manifest Builder
   ---------------------------------------------------------------------------

   --  Builder state for constructing manifests
   type Manifest_Builder is private;

   --  Initialize builder
   function Create_Builder return Manifest_Builder with
      Global => null;

   --  Set metadata
   procedure Set_Metadata (
      Builder  : in out Manifest_Builder;
      Metadata : in     Contract_Metadata
   ) with
      Global => null;

   --  Add function to ABI
   procedure Add_Function (
      Builder : in Out Manifest_Builder;
      Func    : in     Function_ABI;
      Success : out    Boolean
   ) with
      Global => null;

   --  Add event to ABI
   procedure Add_Event (
      Builder : in Out Manifest_Builder;
      Event   : in     Event_ABI;
      Success : out    Boolean
   ) with
      Global => null;

   --  Set certification data
   procedure Set_Certification (
      Builder : in Out Manifest_Builder;
      Cert    : in     Certification_Data
   ) with
      Global => null;

   --  Add dependency
   procedure Add_Dependency (
      Builder : in Out Manifest_Builder;
      Dep     : in     Dependency_Entry;
      Success : out    Boolean
   ) with
      Global => null;

   --  Build final manifest
   function Build (Builder : Manifest_Builder) return Contract_Manifest with
      Global => null;

private

   type Manifest_Builder is record
      Manifest : Contract_Manifest;
      Is_Valid : Boolean;
   end record;

end Khepri_Manifest;
