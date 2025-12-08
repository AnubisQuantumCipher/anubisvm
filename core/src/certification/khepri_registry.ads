--  KHEPRI Contract Registry
--
--  On-chain registry for contract certification, verification status,
--  and metadata. Enables gas discounts for certified contracts and
--  provides trust anchors for cross-contract calls.
--
--  Features:
--  - Contract registration with certification level
--  - Proof artifact storage references
--  - Auditor attestations (Platinum level)
--  - Gas discount calculation based on level
--  - Revocation support for compromised contracts
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6: MAAT Certification
--  - KHEPRI Blueprint v1.0, Section 12: Registry
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;

package Khepri_Registry with
   SPARK_Mode => On,
   Abstract_State => (Registry_State with External => Async_Writers),
   Initializes => Registry_State
is

   ---------------------------------------------------------------------------
   --  Certification Levels
   ---------------------------------------------------------------------------

   type Certification_Level is (
      Level_None,      --  No certification (default)
      Level_Bronze,    --  Basic checks (compilation, static analysis)
      Level_Silver,    --  SPARK mode enabled, partial proofs
      Level_Gold,      --  Full SPARK proofs, WCET bounds
      Level_Platinum   --  Gold + independent audit
   );

   --  Gas discount percentages per level
   Gas_Discount_None     : constant := 0;
   Gas_Discount_Bronze   : constant := 5;
   Gas_Discount_Silver   : constant := 15;
   Gas_Discount_Gold     : constant := 25;
   Gas_Discount_Platinum : constant := 35;

   ---------------------------------------------------------------------------
   --  Registry Entry Types
   ---------------------------------------------------------------------------

   --  Maximum registered contracts
   Max_Contracts : constant := 10_000;

   --  Maximum auditors per contract
   Max_Auditors : constant := 5;

   --  Auditor signature
   type Auditor_Attestation is record
      Auditor     : Contract_Address;    --  Auditor"s address
      Signature   : Hash512;             --  ML-DSA-87 signature
      Timestamp   : Unsigned_64;         --  Attestation time
      Valid       : Boolean;             --  Whether attestation is valid
   end record;

   type Auditor_Array is array (0 .. Max_Auditors - 1) of Auditor_Attestation;

   --  Contract registration entry
   type Registry_Entry is record
      --  Identity
      Contract_Addr   : Contract_Address;
      Code_Hash       : Hash256;           --  Hash of deployed code
      Deployer        : Contract_Address;

      --  Certification
      Level           : Certification_Level;
      Proof_Hash      : Hash256;           --  Hash of proof artifacts
      WCET_Bound      : U256;              --  Maximum gas usage

      --  Audit (for Platinum)
      Auditors        : Auditor_Array;
      Auditor_Count   : Natural;

      --  Status
      Registered_At   : Unsigned_64;       --  Registration timestamp
      Updated_At      : Unsigned_64;       --  Last update timestamp
      Revoked         : Boolean;           --  Whether revoked
      Revoked_Reason  : Hash256;           --  Hash of revocation reason

      --  Usage
      Call_Count      : U256;              --  Total calls
      Gas_Saved       : U256;              --  Total gas saved from discounts
   end record;

   --  Empty entry
   Null_Entry : constant Registry_Entry := (
      Contract_Addr   => (others => 0),
      Code_Hash       => (others => 0),
      Deployer        => (others => 0),
      Level           => Level_None,
      Proof_Hash      => (others => 0),
      WCET_Bound      => U256_Zero,
      Auditors        => (others => (
         Auditor   => (others => 0),
         Signature => (others => 0),
         Timestamp => 0,
         Valid     => False
      )),
      Auditor_Count   => 0,
      Registered_At   => 0,
      Updated_At      => 0,
      Revoked         => False,
      Revoked_Reason  => (others => 0),
      Call_Count      => U256_Zero,
      Gas_Saved       => U256_Zero
   );

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type Registry_Error is (
      Error_None,
      Error_Already_Registered,
      Error_Not_Registered,
      Error_Unauthorized,
      Error_Invalid_Proof,
      Error_Invalid_Level,
      Error_Already_Revoked,
      Error_Auditor_Required,
      Error_Invalid_Signature,
      Error_Registry_Full
   );

   ---------------------------------------------------------------------------
   --  Registration Operations
   ---------------------------------------------------------------------------

   --  Register a new contract
   procedure Register (
      Contract_Addr : in     Contract_Address;
      Code_Hash     : in     Hash256;
      Deployer      : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Update certification level
   procedure Set_Certification (
      Contract_Addr : in     Contract_Address;
      Level         : in     Certification_Level;
      Proof_Hash    : in     Hash256;
      WCET_Bound    : in     U256;
      Caller        : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State),
      Pre    => Level /= Level_None;

   --  Add auditor attestation (for Platinum)
   procedure Add_Auditor (
      Contract_Addr : in     Contract_Address;
      Auditor       : in     Contract_Address;
      Signature     : in     Hash512;
      Caller        : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Revoke certification
   procedure Revoke (
      Contract_Addr : in     Contract_Address;
      Reason        : in     Hash256;
      Caller        : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   ---------------------------------------------------------------------------
   --  Query Operations
   ---------------------------------------------------------------------------

   --  Check if contract is registered
   function Is_Registered (Contract_Addr : Contract_Address) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   --  Get certification level
   function Get_Level (Contract_Addr : Contract_Address) return Certification_Level with
      Global => Registry_State,
      Volatile_Function;

   --  Get full entry
   procedure Get_Registry_Entry (
      Contract_Addr : in     Contract_Address;
      Reg_Entry     : out    Registry_Entry;
      Found         : out    Boolean
   ) with
      Global => Registry_State;

   --  Check if revoked
   function Is_Revoked (Contract_Addr : Contract_Address) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   --  Get WCET bound
   function Get_WCET_Bound (Contract_Addr : Contract_Address) return U256 with
      Global => Registry_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Gas Discount Calculation
   ---------------------------------------------------------------------------

   --  Get gas discount percentage for contract
   function Get_Gas_Discount (Contract_Addr : Contract_Address) return Natural with
      Global => Registry_State,
      Volatile_Function,
      Post   => Get_Gas_Discount'Result <= 100;

   --  Calculate discounted gas
   function Apply_Discount (
      Gas_Amount    : U256;
      Contract_Addr : Contract_Address
   ) return U256 with
      Global => Registry_State,
      Volatile_Function;

   --  Record gas usage (for statistics)
   procedure Record_Gas_Usage (
      Contract_Addr : in Contract_Address;
      Gas_Used      : in U256;
      Gas_Saved     : in U256
   ) with
      Global => (In_Out => Registry_State);

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   --  Verify proof artifacts match registered hash
   function Verify_Proof (
      Contract_Addr : Contract_Address;
      Proof_Data    : Hash256
   ) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   --  Verify auditor signature
   function Verify_Auditor (
      Contract_Addr : Contract_Address;
      Auditor       : Contract_Address
   ) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   --  Check if contract has minimum certification level
   function Meets_Level (
      Contract_Addr : Contract_Address;
      Min_Level     : Certification_Level
   ) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   --  Get total registered contracts
   function Total_Registered return Natural with
      Global => Registry_State,
      Volatile_Function;

   --  Get count by level
   function Count_By_Level (Level : Certification_Level) return Natural with
      Global => Registry_State,
      Volatile_Function;

   --  Get total gas saved across all contracts
   function Total_Gas_Saved return U256 with
      Global => Registry_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Administrative
   ---------------------------------------------------------------------------

   --  Initialize registry (called once at genesis)
   procedure Initialize with
      Global => (Output => Registry_State);

   --  Check if caller is authorized certifier
   function Is_Certifier (Addr : Contract_Address) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   --  Add authorized certifier
   procedure Add_Certifier (
      Addr    : in     Contract_Address;
      Caller  : in     Contract_Address;
      Success : out    Boolean
   ) with
      Global => (In_Out => Registry_State);

   --  Remove authorized certifier
   procedure Remove_Certifier (
      Addr    : in     Contract_Address;
      Caller  : in     Contract_Address;
      Success : out    Boolean
   ) with
      Global => (In_Out => Registry_State);

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------

   type Registration_Event is record
      Contract    : Contract_Address;
      Code_Hash   : Hash256;
      Deployer    : Contract_Address;
      Timestamp   : Unsigned_64;
   end record;

   type Certification_Event is record
      Contract    : Contract_Address;
      Old_Level   : Certification_Level;
      New_Level   : Certification_Level;
      Proof_Hash  : Hash256;
      Timestamp   : Unsigned_64;
   end record;

   type Revocation_Event is record
      Contract    : Contract_Address;
      Reason      : Hash256;
      Revoker     : Contract_Address;
      Timestamp   : Unsigned_64;
   end record;

   type Audit_Event is record
      Contract    : Contract_Address;
      Auditor     : Contract_Address;
      Timestamp   : Unsigned_64;
   end record;

end Khepri_Registry;
