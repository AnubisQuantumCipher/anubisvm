-------------------------------------------------------------------------------
--  ANUBIS Certification Gas Discounts
--  Bronze to Platinum Tiers Based on SPARK Verification Level
--
--  Contracts with SPARK verification get gas discounts:
--
--  Level    | Discount | Requirements                  | Deposit
--  ---------|----------|-------------------------------|----------------
--  Bronze   | 0%       | SPARK Mode enabled            | 1,000 ANUBIS
--  Silver   | 10%      | + All VCs proven              | 10,000 ANUBIS
--  Gold     | 20%      | + Constant-time verified      | 50,000 ANUBIS
--  Platinum | 30%      | + Third-party audit           | 100,000 ANUBIS
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Certification with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Certification levels
   type Cert_Level is (None, Bronze, Silver, Gold, Platinum);

   --  Discount rates (basis points, 10000 = 100%)
   Level_Discounts : constant array (Cert_Level) of Natural := (
      None     => 0,
      Bronze   => 0,
      Silver   => 1000,   -- 10%
      Gold     => 2000,   -- 20%
      Platinum => 3000    -- 30%
   );

   --  Deposit requirements (in raw tokens)
   Level_Deposits : constant array (Cert_Level) of Unsigned_64 := (
      None     => 0,
      Bronze   => 1_000,
      Silver   => 10_000,
      Gold     => 50_000,
      Platinum => 100_000
   );

   --  Verification requirements
   type Verification_Requirement is (
      None_Required,
      SPARK_Mode_Enabled,
      All_VCs_Proven,
      Constant_Time_Verified,
      Third_Party_Audited
   );

   Level_Requirements : constant array (Cert_Level) of Verification_Requirement := (
      None     => None_Required,
      Bronze   => SPARK_Mode_Enabled,
      Silver   => All_VCs_Proven,
      Gold     => Constant_Time_Verified,
      Platinum => Third_Party_Audited
   );

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Certification_Status is (
      Not_Applied,
      Pending_Review,
      Active,
      Suspended,
      Revoked
   );

   type Contract_Certification is record
      Contract_Address : Byte_Array (0 .. 31);
      Level            : Cert_Level;
      Status           : Certification_Status;

      --  Verification data
      GNATprove_Hash   : Byte_Array (0 .. 31);  -- Proof output hash
      VC_Count         : Unsigned_32;           -- Total VCs
      VCs_Proven       : Unsigned_32;           -- Proven VCs
      CT_Analysis_Hash : Byte_Array (0 .. 31);  -- Constant-time analysis
      Audit_Report     : Byte_Array (0 .. 31);  -- Audit report hash
      Auditor_PK       : Byte_Array (0 .. 31);  -- Auditor public key

      --  Deposit
      Deposit_Amount   : Unsigned_64;
      Deposit_Locked   : Boolean;

      --  Timing
      Applied_At       : Unsigned_64;
      Certified_At     : Unsigned_64;
      Expires_At       : Unsigned_64;  -- Annual renewal required
      Last_Verified    : Unsigned_64;

      --  Statistics
      Gas_Saved        : Unsigned_64;
      TX_Count         : Unsigned_64;
   end record;

   type Certification_Array is array (Natural range <>) of Contract_Certification;

   --  Level counts array type (for SPARK compatibility)
   type Level_Count_Array is array (Cert_Level) of Unsigned_32;

   type Certification_State is record
      Total_Certified    : Unsigned_64;
      Total_Deposits     : Unsigned_64;
      Total_Gas_Saved    : Unsigned_64;
      Level_Counts       : Level_Count_Array;
      Genesis_Block      : Unsigned_64;
      Current_Block      : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Apply_Result is (
      Applied,
      Already_Certified,
      Insufficient_Deposit,
      Invalid_Contract,
      Invalid_Level
   );

   type Verify_Result is (
      Verified,
      Verification_Failed,
      Missing_Proofs,
      Audit_Required,
      Expired
   );

   type Upgrade_Result is (
      Upgraded,
      Requirements_Not_Met,
      Insufficient_Deposit,
      Invalid_Upgrade_Path
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Certification_State (
      State          : out Certification_State;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Certification Application
   ---------------------------------------------------------------------------

   procedure Apply_Certification (
      State          : in Out Certification_State;
      Contract_Addr  : Byte_Array;
      Level          : Cert_Level;
      Deposit        : Unsigned_64;
      GNATprove_Hash : Byte_Array;
      Cert           : out Contract_Certification;
      Result         : out Apply_Result
   ) with
      Global => null,
      Pre => Contract_Addr'Length = 32
             and GNATprove_Hash'Length = 32
             and Level /= None;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   procedure Verify_Certification (
      Cert           : in Out Contract_Certification;
      Verifier       : Byte_Array;
      Proof_Hash     : Byte_Array;
      CT_Hash        : Byte_Array;
      Audit_Hash     : Byte_Array;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => Verifier'Length = 32
             and Proof_Hash'Length = 32;

   function Check_VC_Coverage (
      VC_Count       : Unsigned_32;
      VCs_Proven     : Unsigned_32
   ) return Boolean with
      Global => null,
      Post => Check_VC_Coverage'Result = (VCs_Proven = VC_Count);

   ---------------------------------------------------------------------------
   --  Level Management
   ---------------------------------------------------------------------------

   procedure Upgrade_Level (
      Cert           : in Out Contract_Certification;
      New_Level      : Cert_Level;
      Additional_Deposit : Unsigned_64;
      Additional_Proof   : Byte_Array;
      Result         : out Upgrade_Result
   ) with
      Global => null,
      Pre => Additional_Proof'Length = 32
             and New_Level > Cert.Level;

   procedure Downgrade_Level (
      Cert           : in Out Contract_Certification;
      New_Level      : Cert_Level;
      Refund         : out Unsigned_64
   ) with
      Global => null,
      Pre => New_Level < Cert.Level;

   procedure Revoke_Certification (
      State          : in Out Certification_State;
      Cert           : in Out Contract_Certification;
      Reason_Hash    : Byte_Array;
      Slash          : Boolean;
      Slashed_Amount : out Unsigned_64
   ) with
      Global => null,
      Pre => Reason_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Gas Discount Calculation
   ---------------------------------------------------------------------------

   function Get_Discount_Rate (
      Level          : Cert_Level
   ) return Natural with
      Global => null,
      Post => Get_Discount_Rate'Result <= 3000;

   function Calculate_Discounted_Gas (
      Base_Gas       : Unsigned_64;
      Level          : Cert_Level
   ) return Unsigned_64 with
      Global => null;

   procedure Apply_Gas_Discount (
      Cert           : in Out Contract_Certification;
      Base_Gas       : Unsigned_64;
      Final_Gas      : out Unsigned_64
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Renewal
   ---------------------------------------------------------------------------

   procedure Renew_Certification (
      Cert           : in Out Contract_Certification;
      New_Proof_Hash : Byte_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => New_Proof_Hash'Length = 32;

   function Is_Expired (
      Cert           : Contract_Certification;
      Current_Block  : Unsigned_64
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Level (Cert : Contract_Certification) return Cert_Level with
      Global => null;

   function Get_Deposit (Level : Cert_Level) return Unsigned_64 with
      Global => null;

   function Get_Total_Certified (State : Certification_State) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Certification_State (State : out Certification_State) with
      Global => null;

   procedure Zeroize_Contract_Cert (Cert : out Contract_Certification) with
      Global => null;

end Anubis_Certification;
