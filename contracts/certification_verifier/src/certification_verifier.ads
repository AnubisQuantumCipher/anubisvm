pragma SPARK_Mode (On);

with Khepri_Types; use Khepri_Types;
with Khepri_State; use Khepri_State;
with Khepri_Runtime; use Khepri_Runtime;
with Khepri_Events; use Khepri_Events;
with Khepri_Crypto; use Khepri_Crypto;

--  Certification Verifier: On-Chain Smart Contract
--
--  This contract provides on-chain verification of SPARK certification
--  status for deployed contracts. It enables:
--  - Querying certification levels (Bronze/Silver/Gold/Platinum)
--  - Verifying proof artifacts match deployed bytecode
--  - Granting gas discounts based on certification
--  - Storing and retrieving certification metadata
--
--  Certification levels:
--  - Level_None (0):     No certification
--  - Level_Bronze (1):   Flow analysis clean
--  - Level_Silver (2):   All proofs discharged + WCET
--  - Level_Gold (3):     CT-safe + security properties
--  - Level_Platinum (4): Gold + third-party audit
--
--  Gas discounts:
--  - Bronze:   10%
--  - Silver:   15%
--  - Gold:     20%
--  - Platinum: 30%
--
--  Usage:
--    certification_verifier.register_cert(contract_addr, proof_hash, level)
--    certification_verifier.get_level(contract_addr) -> level
--    certification_verifier.verify_proof(contract_addr, proof_data) -> bool
--    certification_verifier.get_discount(contract_addr) -> discount_bps
--
--  Certification Target: GOLD
--  - All functions fully proven with GNATprove level 4
--  - WCET bounds computed for all entry points
--  - Constant-time operations where applicable
--  - Security properties formally verified

package Certification_Verifier with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  State Layout
   ---------------------------------------------------------------------------

   --  Storage slots:
   --  0x00: Total registered contracts
   --  0x01: Admin address
   --  0x02-0x31: Reserved
   --
   --  Contract entries (starting at 0x1000):
   --  For contract address H(addr):
   --    slot = keccak256(addr || 0x1000)
   --    slot+0: Certification level (0-4)
   --    slot+1: Proof artifact hash (bytes32)
   --    slot+2: Contract code hash (bytes32)
   --    slot+3: Timestamp registered
   --    slot+4: Certifier address
   --    slot+5: Flags (revoked, audited, etc.)

   --  Storage keys
   Slot_Total_Contracts : constant Uint256 := From_Natural (0);
   Slot_Admin_Address   : constant Uint256 := From_Natural (1);
   Slot_Entry_Base      : constant Uint256 := From_Natural (16#1000#);

   --  Certification levels (matches Khepri_Certification)
   Level_None     : constant Uint256 := From_Natural (0);
   Level_Bronze   : constant Uint256 := From_Natural (1);
   Level_Silver   : constant Uint256 := From_Natural (2);
   Level_Gold     : constant Uint256 := From_Natural (3);
   Level_Platinum : constant Uint256 := From_Natural (4);

   --  Gas discount basis points (100 = 1%)
   Discount_None     : constant Uint256 := From_Natural (0);
   Discount_Bronze   : constant Uint256 := From_Natural (1000);   --  10%
   Discount_Silver   : constant Uint256 := From_Natural (1500);   --  15%
   Discount_Gold     : constant Uint256 := From_Natural (2000);   --  20%
   Discount_Platinum : constant Uint256 := From_Natural (3000);   --  30%

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------

   --  Event: CertificationRegistered(address indexed contract, uint256 level, bytes32 proofHash)
   Event_Certification_Registered : constant Hash_256 := SHA3_256_Bytes32 ((
      16#CE#, 16#12#, 16#34#, 16#56#, 16#78#, 16#9A#, 16#BC#, 16#DE#,
      16#F0#, 16#12#, 16#34#, 16#56#, 16#78#, 16#9A#, 16#BC#, 16#DE#,
      16#F0#, 16#12#, 16#34#, 16#56#, 16#78#, 16#9A#, 16#BC#, 16#DE#,
      16#F0#, 16#12#, 16#34#, 16#56#, 16#78#, 16#9A#, 16#BC#, 16#DE#
   ));

   --  Event: CertificationRevoked(address indexed contract, string reason)
   Event_Certification_Revoked : constant Hash_256 := SHA3_256_Bytes32 ((
      16#CE#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#,
      16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#,
      16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#,
      16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#
   ));

   ---------------------------------------------------------------------------
   --  Constructor
   ---------------------------------------------------------------------------

   procedure Constructor with
      Global => (In_Out => (Contract_State, Event_Log)),
      Pre    => Is_Constructor_Call,
      Post   => Is_Initialized;

   ---------------------------------------------------------------------------
   --  Administrative Functions
   ---------------------------------------------------------------------------

   --  Register a new certification for a contract
   --  Only callable by admin or authorized certifiers
   procedure Register_Certification (
      Contract_Addr : in Address;
      Proof_Hash    : in Hash_256;
      Code_Hash     : in Hash_256;
      Level         : in Uint256
   ) with
      Global => (In_Out => (Contract_State, Event_Log)),
      Pre    => Is_Initialized and Level <= Level_Platinum,
      Post   => Is_Initialized;

   --  Revoke a certification
   procedure Revoke_Certification (
      Contract_Addr : in Address
   ) with
      Global => (In_Out => (Contract_State, Event_Log)),
      Pre    => Is_Initialized,
      Post   => Is_Initialized;

   --  Update admin address
   procedure Set_Admin (
      New_Admin : in Address
   ) with
      Global => (In_Out => Contract_State),
      Pre    => Is_Initialized and Caller = Get_Admin,
      Post   => Is_Initialized and Get_Admin = New_Admin;

   ---------------------------------------------------------------------------
   --  Query Functions (View)
   ---------------------------------------------------------------------------

   --  Get certification level for a contract
   function Get_Level (
      Contract_Addr : Address
   ) return Uint256 with
      Global => Contract_State,
      Pre    => Is_Initialized;

   --  Get proof artifact hash for a contract
   function Get_Proof_Hash (
      Contract_Addr : Address
   ) return Hash_256 with
      Global => Contract_State,
      Pre    => Is_Initialized;

   --  Get contract code hash
   function Get_Code_Hash (
      Contract_Addr : Address
   ) return Hash_256 with
      Global => Contract_State,
      Pre    => Is_Initialized;

   --  Get gas discount for a contract (in basis points)
   function Get_Discount (
      Contract_Addr : Address
   ) return Uint256 with
      Global => Contract_State,
      Pre    => Is_Initialized,
      Post   => Get_Discount'Result <= Discount_Platinum;

   --  Check if contract has minimum certification level
   function Has_Minimum_Level (
      Contract_Addr : Address;
      Min_Level     : Uint256
   ) return Boolean with
      Global => Contract_State,
      Pre    => Is_Initialized and Min_Level <= Level_Platinum;

   --  Get total number of certified contracts
   function Get_Total_Contracts return Uint256 with
      Global => Contract_State,
      Pre    => Is_Initialized;

   --  Get admin address
   function Get_Admin return Address with
      Global => Contract_State,
      Pre    => Is_Initialized;

   ---------------------------------------------------------------------------
   --  Verification Functions
   ---------------------------------------------------------------------------

   --  Verify that proof artifact matches registered hash
   function Verify_Proof (
      Contract_Addr : Address;
      Proof_Data    : Hash_256
   ) return Boolean with
      Global => Contract_State,
      Pre    => Is_Initialized;

   --  Calculate storage slot for contract entry
   function Get_Contract_Slot (
      Contract_Addr : Address;
      Offset        : Uint256
   ) return Uint256 with
      Global => null,
      Pre    => Offset <= From_Natural (10);

private

   --  Internal helpers
   function Level_To_Discount (Level : Uint256) return Uint256 with
      Global => null,
      Pre    => Level <= Level_Platinum,
      Post   => Level_To_Discount'Result <= Discount_Platinum;

   function Is_Initialized return Boolean with
      Global => Contract_State;

   function Is_Constructor_Call return Boolean with
      Global => null;

end Certification_Verifier;
