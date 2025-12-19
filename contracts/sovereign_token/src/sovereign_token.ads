--  Sovereign_Token - AnubisVM SPARK Contract
--  Features: KHEPRI SDK, THOTH Storage, ANKH Crypto
--
--  This implements a full ERC20-compatible token with:
--  - Per-address balance tracking via Khepri_State
--  - Transfer between any addresses
--  - Approve/TransferFrom allowance system
--  - Total supply tracking
pragma SPARK_Mode (On);

with Interfaces;      use Interfaces;
with Khepri_Types;    use Khepri_Types;
with Khepri_Crypto;   use Khepri_Crypto;
with Khepri_State;
with Aegis_VM_Types;

package Sovereign_Token with SPARK_Mode => On is

   --  Contract version (AAS-001 compliant)
   Contract_Version : constant Uint256 := One;

   --  Storage slot constants (ERC20-compatible layout)
   --  Slot 0: Total supply
   --  Slot 1: Owner address
   --  Slot 2: Balances mapping base
   --  Slot 3: Allowances mapping base
   Slot_Total_Supply : constant Uint256 := Zero;
   Slot_Owner        : constant Uint256 := One;
   Slot_Balances     : constant Uint256 := From_Natural (2);
   Slot_Allowances   : constant Uint256 := From_Natural (3);

   --  State record (for compatibility - actual storage is via Khepri_State)
   type Contract_State is record
      Version      : Uint256;
      Owner        : Address;
      Total_Supply : Balance;
      Initialized  : Boolean;
   end record;

   Empty_State : constant Contract_State := (
      Version      => Zero,
      Owner        => Null_Address,
      Total_Supply => Zero,
      Initialized  => False
   );

   --  Initialize contract with owner (ML-DSA-87 derived address)
   --  Owner receives the initial supply
   procedure Initialize (
      State  : out Contract_State;
      Owner  : Address;
      Supply : Balance
   ) with
      Global => (In_Out => Khepri_State.Storage_State),
      Pre    => Is_Valid_Address (Owner),
      Post   => State.Initialized and State.Version = One;

   --  Check if caller is owner
   function Is_Owner (State : Contract_State; Caller : Address)
      return Boolean with Global => null;

   ---------------------------------------------------------------------------
   --  ERC20-Compatible Interface
   ---------------------------------------------------------------------------

   --  Get balance of an account
   function BalanceOf (Account : Address) return Balance with
      Global => Khepri_State.Storage_State;

   --  Get total supply
   function TotalSupply return Balance with
      Global => Khepri_State.Storage_State;

   --  Transfer tokens from caller to recipient
   procedure Transfer (
      State  : in out Contract_State;
      To     : Address;
      Amount : Balance;
      Caller : Address;
      Status : out Error_Code
   ) with
      Global => (In_Out => Khepri_State.Storage_State),
      Pre    => State.Initialized and Is_Valid_Address (To);

   --  Approve spender to transfer up to amount from caller's account
   procedure Approve (
      Owner   : Address;
      Spender : Address;
      Amount  : Balance;
      Status  : out Error_Code
   ) with
      Global => (In_Out => Khepri_State.Storage_State),
      Pre    => Is_Valid_Address (Owner) and Is_Valid_Address (Spender);

   --  Get allowance: how much spender can transfer from owner
   function Allowance (
      Owner   : Address;
      Spender : Address
   ) return Balance with
      Global => Khepri_State.Storage_State;

   --  Transfer tokens from one account to another (requires allowance)
   procedure TransferFrom (
      State   : in Out Contract_State;
      From    : Address;
      To      : Address;
      Amount  : Balance;
      Caller  : Address;
      Status  : out Error_Code
   ) with
      Global => (In_Out => Khepri_State.Storage_State),
      Pre    => State.Initialized and Is_Valid_Address (From) and Is_Valid_Address (To);

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Query total supply (legacy interface)
   function Get_Supply (State : Contract_State) return Balance
      with Global => null;

   --  Query owner address
   function Get_Owner (State : Contract_State) return Address
      with Global => null;

   --  Compute state hash (SHA3-256)
   function State_Hash (State : Contract_State) return Hash_256
      with Global => null;

end Sovereign_Token;
