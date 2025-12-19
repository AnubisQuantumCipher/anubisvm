pragma SPARK_Mode (On);

with Khepri_Types; use Khepri_Types;
with Khepri_Crypto; use Khepri_Crypto;
with Aegis_VM_Types; use Aegis_VM_Types;

--  Certification Verifier V2: Simplified Contract State Model
--
--  This is a WORKING implementation that uses in-memory state rather than
--  the incomplete Khepri_Runtime API. For a production contract, you would
--  use Khepri_State.SLoad/SStore for persistent storage.
--
--  This contract provides certification management for SPARK contracts:
--  - Register certifications (Bronze/Silver/Gold/Platinum)
--  - Query certification levels
--  - Verify proof artifacts
--  - Calculate gas discounts
--
--  Certification levels:
--  - Level_None (0):     No certification
--  - Level_Bronze (1):   Flow analysis clean
--  - Level_Silver (2):   All proofs discharged
--  - Level_Gold (3):     Gold + WCET + CT-safe
--  - Level_Platinum (4): Gold + third-party audit
--
--  Gas discounts:
--  - Bronze:   10% (1000 basis points)
--  - Silver:   15% (1500 basis points)
--  - Gold:     20% (2000 basis points)
--  - Platinum: 30% (3000 basis points)

package Certification_Verifier_V2 with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Types and Constants
   ---------------------------------------------------------------------------

   --  Certification levels
   subtype Cert_Level is Uint256;
   Level_None     : constant Cert_Level := From_Natural (0);
   Level_Bronze   : constant Cert_Level := From_Natural (1);
   Level_Silver   : constant Cert_Level := From_Natural (2);
   Level_Gold     : constant Cert_Level := From_Natural (3);
   Level_Platinum : constant Cert_Level := From_Natural (4);

   --  Gas discount basis points (100 = 1%)
   Discount_None     : constant Uint256 := From_Natural (0);
   Discount_Bronze   : constant Uint256 := From_Natural (1000);   --  10%
   Discount_Silver   : constant Uint256 := From_Natural (1500);   --  15%
   Discount_Gold     : constant Uint256 := From_Natural (2000);   --  20%
   Discount_Platinum : constant Uint256 := From_Natural (3000);   --  30%

   --  Maximum registered contracts
   Max_Contracts : constant := 256;
   subtype Contract_Index is Natural range 0 .. Max_Contracts - 1;

   --  Certification entry
   type Cert_Entry is record
      Contract_Addr : Address;
      Level         : Cert_Level;
      Proof_Hash    : Hash_256;
      Code_Hash     : Hash_256;
      Timestamp     : Uint256;
      Certifier     : Address;
      Is_Revoked    : Boolean;
      Is_Valid      : Boolean;  -- Entry in use
   end record;

   Empty_Entry : constant Cert_Entry := (
      Contract_Addr => Null_Address,
      Level         => Level_None,
      Proof_Hash    => (others => 0),
      Code_Hash     => (others => 0),
      Timestamp     => Zero,
      Certifier     => Null_Address,
      Is_Revoked    => False,
      Is_Valid      => False
   );

   --  Certification entries array
   type Cert_Entries is array (Contract_Index) of Cert_Entry;

   --  Contract state
   type Contract_State is record
      Admin             : Address;
      Total_Contracts   : Natural;
      Entries           : Cert_Entries;
      Initialized       : Boolean;
   end record;

   Empty_State : constant Contract_State := (
      Admin           => Null_Address,
      Total_Contracts => 0,
      Entries         => (others => Empty_Entry),
      Initialized     => False
   );

   ---------------------------------------------------------------------------
   --  Contract Lifecycle
   ---------------------------------------------------------------------------

   --  Initialize contract with admin address
   procedure Initialize (
      State : out Contract_State;
      Admin : Address
   ) with
      Global => null,
      Pre    => Is_Valid_Address (Admin),
      Post   => State.Initialized and State.Admin = Admin;

   ---------------------------------------------------------------------------
   --  Administrative Functions
   ---------------------------------------------------------------------------

   --  Register a new certification
   procedure Register_Certification (
      State         : in out Contract_State;
      Contract_Addr : in     Address;
      Level         : in     Cert_Level;
      Proof_Hash    : in     Hash_256;
      Code_Hash     : in     Hash_256;
      Certifier     : in     Address;
      Timestamp     : in     Uint256;
      Status        : out    Error_Code
   ) with
      Global => null,
      Pre    => State.Initialized and
                Is_Valid_Address (Contract_Addr) and
                Level <= Level_Platinum;

   --  Revoke a certification
   procedure Revoke_Certification (
      State         : in out Contract_State;
      Contract_Addr : in     Address;
      Caller        : in     Address;
      Status        : out    Error_Code
   ) with
      Global => null,
      Pre    => State.Initialized and Is_Valid_Address (Contract_Addr);

   --  Update admin address (only current admin)
   procedure Set_Admin (
      State     : in out Contract_State;
      New_Admin : in     Address;
      Caller    : in     Address;
      Status    : out    Error_Code
   ) with
      Global => null,
      Pre    => State.Initialized and Is_Valid_Address (New_Admin);

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Get certification level for a contract
   function Get_Level (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Cert_Level with
      Global => null,
      Pre    => State.Initialized;

   --  Get proof hash for a contract
   function Get_Proof_Hash (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Hash_256 with
      Global => null,
      Pre    => State.Initialized;

   --  Get code hash for a contract
   function Get_Code_Hash (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Hash_256 with
      Global => null,
      Pre    => State.Initialized;

   --  Get gas discount for a contract (in basis points)
   function Get_Discount (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Uint256 with
      Global => null,
      Pre    => State.Initialized,
      Post   => Get_Discount'Result <= Discount_Platinum;

   --  Check if contract has minimum certification level
   function Has_Minimum_Level (
      State         : Contract_State;
      Contract_Addr : Address;
      Min_Level     : Cert_Level
   ) return Boolean with
      Global => null,
      Pre    => State.Initialized and Min_Level <= Level_Platinum;

   --  Verify proof artifact matches registered hash
   function Verify_Proof (
      State         : Contract_State;
      Contract_Addr : Address;
      Proof_Data    : Hash_256
   ) return Boolean with
      Global => null,
      Pre    => State.Initialized;

   --  Get admin address
   function Get_Admin (State : Contract_State) return Address with
      Global => null,
      Pre    => State.Initialized;

   --  Get total registered contracts
   function Get_Total_Contracts (State : Contract_State) return Natural with
      Global => null,
      Pre    => State.Initialized;

private

   --  Internal helper: find entry by contract address
   function Find_Entry (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Contract_Index with
      Global => null;

   --  Internal helper: map level to discount
   function Level_To_Discount (Level : Cert_Level) return Uint256 with
      Global => null,
      Pre    => Level <= Level_Platinum,
      Post   => Level_To_Discount'Result <= Discount_Platinum;

end Certification_Verifier_V2;
