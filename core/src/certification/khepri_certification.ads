pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;

--  KHEPRI Certification: Contract Certification System
--
--  This package defines the KHEPRI certification levels and verification
--  procedures. Contracts can achieve Bronze, Silver, Gold, or Platinum
--  certification based on their formal verification status.
--
--  Certification Levels:
--  - BRONZE:   Flow analysis clean (no uninitialized reads, no aliasing)
--  - SILVER:   All SPARK proofs discharged + WCET bounds proven
--  - GOLD:     Silver + constant-time analysis + security properties
--  - PLATINUM: Gold + third-party security audit
--
--  Benefits:
--  - Bronze:   Base gas pricing
--  - Silver:   10% gas discount
--  - Gold:     20% gas discount
--  - Platinum: 30% gas discount
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6: Certification System

package Khepri_Certification with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Certification Levels
   ---------------------------------------------------------------------------

   type Certification_Level is (
      Level_None,      --  No certification (uncertified contract)
      Level_Bronze,    --  Flow analysis clean
      Level_Silver,    --  All proofs discharged + WCET
      Level_Gold,      --  Silver + CT analysis + security
      Level_Platinum   --  Gold + third-party audit
   );

   --  Gas discount percentages (in basis points, 100 = 1%)
   Bronze_Discount   : constant := 0;     --  0%
   Silver_Discount   : constant := 1000;  --  10%
   Gold_Discount     : constant := 2000;  --  20%
   Platinum_Discount : constant := 3000;  --  30%

   --  Get gas discount for certification level
   function Get_Discount (Level : Certification_Level) return Natural with
      Global => null,
      Post   => Get_Discount'Result <= 3000;

   ---------------------------------------------------------------------------
   --  Certification Requirements
   ---------------------------------------------------------------------------

   --  Bronze requirements
   type Bronze_Requirements is record
      SPARK_Mode_Enabled    : Boolean;  --  pragma SPARK_Mode (On) present
      No_Runtime_Errors     : Boolean;  --  Flow analysis clean
      No_Uninitialized_Reads: Boolean;  --  All variables initialized
      No_Illegal_Aliasing   : Boolean;  --  No aliasing violations
      Compiles_Clean        : Boolean;  --  No warnings/errors
   end record;

   --  Silver requirements (includes Bronze)
   type Silver_Requirements is record
      Bronze_Met            : Boolean;  --  Bronze requirements satisfied
      All_Proofs_Discharged : Boolean;  --  GNATprove level 4 clean
      WCET_Analyzed         : Boolean;  --  WCET analysis complete
      WCET_Bounded          : Boolean;  --  All functions have gas bounds
      No_Unchecked_Unions   : Boolean;  --  No unchecked unions
   end record;

   --  Gold requirements (includes Silver)
   type Gold_Requirements is record
      Silver_Met              : Boolean;  --  Silver requirements satisfied
      Constant_Time_Verified  : Boolean;  --  CT analysis passed
      Security_Props_Proven   : Boolean;  --  Security properties verified
      No_Timing_Channels      : Boolean;  --  No timing side channels
      Functional_Correctness  : Boolean;  --  Key properties proven
   end record;

   --  Platinum requirements (includes Gold)
   type Platinum_Requirements is record
      Gold_Met              : Boolean;  --  Gold requirements satisfied
      Audit_Complete        : Boolean;  --  Third-party audit done
      Auditor_Verified      : Boolean;  --  Auditor identity verified
      Audit_Report_Hash     : Hash256;  --  Hash of audit report
      Audit_Timestamp       : Word64;   --  When audit was completed
   end record;

   ---------------------------------------------------------------------------
   --  Certification Status
   ---------------------------------------------------------------------------

   --  Full certification status for a contract
   type Certification_Status is record
      Level           : Certification_Level;
      Bronze_Reqs     : Bronze_Requirements;
      Silver_Reqs     : Silver_Requirements;
      Gold_Reqs       : Gold_Requirements;
      Platinum_Reqs   : Platinum_Requirements;
      Certified_At    : Word64;           --  Timestamp of certification
      Certified_By    : Address;          --  Certifier address (or null)
      Contract_Hash   : Hash256;          --  Hash of certified bytecode
      Proof_Artifact  : Hash256;          --  Hash of proof artifacts
   end record;

   --  Empty/uncertified status
   Uncertified_Status : constant Certification_Status := (
      Level         => Level_None,
      Bronze_Reqs   => (others => False),
      Silver_Reqs   => (others => False),
      Gold_Reqs     => (others => False),
      Platinum_Reqs => (
         Gold_Met          => False,
         Audit_Complete    => False,
         Auditor_Verified  => False,
         Audit_Report_Hash => (others => 0),
         Audit_Timestamp   => 0
      ),
      Certified_At  => 0,
      Certified_By  => (others => 0),
      Contract_Hash => (others => 0),
      Proof_Artifact => (others => 0)
   );

   ---------------------------------------------------------------------------
   --  Verification Results
   ---------------------------------------------------------------------------

   --  Individual verification check result
   type Check_Result is record
      Passed   : Boolean;
      Message  : Bounded_String;
      Location : Bounded_String;  --  File:line if applicable
   end record;

   --  Maximum checks per verification run
   Max_Checks : constant := 256;
   type Check_Index is range 0 .. Max_Checks - 1;
   type Check_Array is array (Check_Index) of Check_Result;

   --  Verification summary
   type Verification_Result is record
      Success      : Boolean;
      Level_Achieved : Certification_Level;
      Checks       : Check_Array;
      Check_Count  : Natural;
      Total_Time   : Natural;  --  Milliseconds
   end record;

   ---------------------------------------------------------------------------
   --  Certification Verification
   ---------------------------------------------------------------------------

   --  Check if Bronze requirements are met
   function Check_Bronze (
      Contract_Code : Byte_Array
   ) return Bronze_Requirements with
      Global => null,
      Pre    => Contract_Code'Length <= Max_Binary_Size;

   --  Check if Silver requirements are met
   function Check_Silver (
      Contract_Code : Byte_Array;
      Bronze_Status : Bronze_Requirements
   ) return Silver_Requirements with
      Global => null,
      Pre    => Contract_Code'Length <= Max_Binary_Size;

   --  Check if Gold requirements are met
   function Check_Gold (
      Contract_Code : Byte_Array;
      Silver_Status : Silver_Requirements
   ) return Gold_Requirements with
      Global => null,
      Pre    => Contract_Code'Length <= Max_Binary_Size;

   --  Verify complete certification status
   function Verify_Certification (
      Contract_Code : Byte_Array
   ) return Certification_Status with
      Global => null,
      Pre    => Contract_Code'Length <= Max_Binary_Size;

   --  Calculate final certification level from requirements
   function Calculate_Level (
      Bronze : Bronze_Requirements;
      Silver : Silver_Requirements;
      Gold   : Gold_Requirements;
      Platinum : Platinum_Requirements
   ) return Certification_Level with
      Global => null;

   ---------------------------------------------------------------------------
   --  Gas Calculation
   ---------------------------------------------------------------------------

   --  Apply certification discount to gas amount
   function Apply_Discount (
      Base_Gas : Gas_Amount;
      Level    : Certification_Level
   ) return Gas_Amount with
      Global => null,
      Post   => Apply_Discount'Result <= Base_Gas;

   --  Calculate effective gas price with discount
   function Effective_Gas_Price (
      Base_Price : Uint256;
      Level      : Certification_Level
   ) return Uint256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Registry (On-Chain)
   ---------------------------------------------------------------------------

   --  Registry entry for a certified contract
   type Registry_Entry is record
      Contract_Address : Address;
      Status           : Certification_Status;
      Version          : Natural;         --  Certification version
      Is_Active        : Boolean;         --  Currently active
      Revoked          : Boolean;         --  Has been revoked
      Revoked_Reason   : Bounded_String;  --  Why revoked (if applicable)
   end record;

   --  Maximum registry entries (for bounded storage)
   Max_Registry_Entries : constant := 65536;
   type Registry_Index is range 0 .. Max_Registry_Entries - 1;

   --  Register a new certification
   procedure Register_Certification (
      Contract  : in Address;
      Status    : in Certification_Status;
      Success   : out Boolean
   ) with
      Global => null;

   --  Get certification status for a contract
   function Get_Certification (
      Contract : Address
   ) return Certification_Status with
      Global => null;

   --  Check if contract has minimum certification level
   function Has_Minimum_Level (
      Contract : Address;
      Minimum  : Certification_Level
   ) return Boolean with
      Global => null;

   --  Revoke a certification
   procedure Revoke_Certification (
      Contract : in Address;
      Reason   : in Bounded_String;
      Success  : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Badges (Visual Identifiers)
   ---------------------------------------------------------------------------

   --  Badge types for UI display
   type Badge_Type is (
      Badge_None,
      Badge_Bronze,   --  🥉
      Badge_Silver,   --  🥈
      Badge_Gold,     --  🥇
      Badge_Platinum  --  💎
   );

   --  Get badge for certification level
   function Get_Badge (Level : Certification_Level) return Badge_Type with
      Global => null;

   --  Get badge description
   function Badge_Description (Badge : Badge_Type) return String with
      Global => null;

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum binary size for certification
   Max_Binary_Size : constant := 24 * 1024;  --  24 KB

   --  Certification version
   Certification_Version : constant := 1;

private

   --  Internal helpers for requirement checking
   function All_Bronze_Met (Reqs : Bronze_Requirements) return Boolean is
      (Reqs.SPARK_Mode_Enabled and
       Reqs.No_Runtime_Errors and
       Reqs.No_Uninitialized_Reads and
       Reqs.No_Illegal_Aliasing and
       Reqs.Compiles_Clean);

   function All_Silver_Met (Reqs : Silver_Requirements) return Boolean is
      (Reqs.Bronze_Met and
       Reqs.All_Proofs_Discharged and
       Reqs.WCET_Analyzed and
       Reqs.WCET_Bounded and
       Reqs.No_Unchecked_Unions);

   function All_Gold_Met (Reqs : Gold_Requirements) return Boolean is
      (Reqs.Silver_Met and
       Reqs.Constant_Time_Verified and
       Reqs.Security_Props_Proven and
       Reqs.No_Timing_Channels and
       Reqs.Functional_Correctness);

   function All_Platinum_Met (Reqs : Platinum_Requirements) return Boolean is
      (Reqs.Gold_Met and
       Reqs.Audit_Complete and
       Reqs.Auditor_Verified);

end Khepri_Certification;
