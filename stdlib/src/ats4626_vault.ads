--  ATS-4626: KHEPRI Tokenized Vault Standard
--
--  This package defines the tokenized vault interface for KHEPRI contracts,
--  equivalent to ERC-4626 on Ethereum. Provides a standard API for
--  yield-bearing vaults.
--
--  Certification Target: PLATINUM

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Khepri_Types;    use Khepri_Types;

package ATS4626_Vault with
   SPARK_Mode => On,
   Abstract_State => (Vault_State with External => Async_Writers),
   Initializes => Vault_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Zero_Address : constant Address := Null_Address;

   Max_Depositors : constant := 10000;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Asset amount (underlying token)
   subtype Asset_Amount is U256;

   --  Share amount (vault token)
   subtype Share_Amount is U256;

   ---------------------------------------------------------------------------
   --  Vault Configuration
   ---------------------------------------------------------------------------

   subtype Name_Length is Natural range 0 .. 64;
   type Vault_Name is record
      Data   : String (1 .. 64);
      Length : Name_Length;
   end record;

   subtype Symbol_Length is Natural range 0 .. 16;
   type Vault_Symbol is record
      Data   : String (1 .. 16);
      Length : Symbol_Length;
   end record;

   type Vault_Config is record
      Name         : Vault_Name;
      Symbol       : Vault_Symbol;
      Asset_Token  : Address;
      Decimals     : Natural;
   end record;

   Default_Config : constant Vault_Config := (
      Name        => (Data => (others => ' '), Length => 0),
      Symbol      => (Data => (others => ' '), Length => 0),
      Asset_Token => Zero_Address,
      Decimals    => 18
   );

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Vault_Error is (
      Error_None,
      Error_Zero_Shares,
      Error_Zero_Assets,
      Error_Insufficient_Balance,
      Error_Insufficient_Allowance,
      Error_Deposit_Limit_Exceeded,
      Error_Withdraw_Limit_Exceeded,
      Error_Not_Owner,
      Error_Zero_Address,
      Error_Overflow
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Vault_Config;
      Owner   : in Address;
      Success : out Boolean
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Owner /= Zero_Address
                and Config.Asset_Token /= Zero_Address;

   ---------------------------------------------------------------------------
   --  ERC-20 Compatible Functions (Share Token)
   ---------------------------------------------------------------------------

   function Name return Vault_Name with
      Global => Vault_State,
      Volatile_Function;

   function Symbol return Vault_Symbol with
      Global => Vault_State,
      Volatile_Function;

   function Decimals return Natural with
      Global => Vault_State,
      Volatile_Function;

   function Total_Supply return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   function Balance_Of (Account : Address) return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   function Allowance (Owner_Addr, Spender : Address) return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   procedure Transfer (
      Caller    : in     Address;
      Recipient : in     Address;
      Amount    : in     Share_Amount;
      Success   : out    Boolean;
      Error     : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Recipient /= Zero_Address;

   procedure Approve (
      Caller  : in     Address;
      Spender : in     Address;
      Amount  : in     Share_Amount;
      Success : out    Boolean;
      Error   : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Spender /= Zero_Address;

   procedure Transfer_From (
      Caller    : in     Address;
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Amount    : in     Share_Amount;
      Success   : out    Boolean;
      Error     : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => To_Addr /= Zero_Address;

   ---------------------------------------------------------------------------
   --  ERC-4626 Asset Functions
   ---------------------------------------------------------------------------

   function Asset return Address with
      Global => Vault_State,
      Volatile_Function;

   function Total_Assets return Asset_Amount with
      Global => Vault_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  ERC-4626 Conversion Functions
   ---------------------------------------------------------------------------

   function Convert_To_Shares (Assets : Asset_Amount) return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   function Convert_To_Assets (Shares : Share_Amount) return Asset_Amount with
      Global => Vault_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  ERC-4626 Deposit/Mint Functions
   ---------------------------------------------------------------------------

   function Max_Deposit (Receiver : Address) return Asset_Amount with
      Global => Vault_State,
      Volatile_Function;

   function Preview_Deposit (Assets : Asset_Amount) return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   procedure Deposit (
      Caller   : in     Address;
      Assets   : in     Asset_Amount;
      Receiver : in     Address;
      Shares   : out    Share_Amount;
      Success  : out    Boolean;
      Error    : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Receiver /= Zero_Address;

   function Max_Mint (Receiver : Address) return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   function Preview_Mint (Shares : Share_Amount) return Asset_Amount with
      Global => Vault_State,
      Volatile_Function;

   procedure Mint (
      Caller   : in     Address;
      Shares   : in     Share_Amount;
      Receiver : in     Address;
      Assets   : out    Asset_Amount;
      Success  : out    Boolean;
      Error    : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Receiver /= Zero_Address;

   ---------------------------------------------------------------------------
   --  ERC-4626 Withdraw/Redeem Functions
   ---------------------------------------------------------------------------

   function Max_Withdraw (Owner_Addr : Address) return Asset_Amount with
      Global => Vault_State,
      Volatile_Function;

   function Preview_Withdraw (Assets : Asset_Amount) return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   procedure Withdraw (
      Caller   : in     Address;
      Assets   : in     Asset_Amount;
      Receiver : in     Address;
      Owner_Addr : in   Address;
      Shares   : out    Share_Amount;
      Success  : out    Boolean;
      Error    : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Receiver /= Zero_Address;

   function Max_Redeem (Owner_Addr : Address) return Share_Amount with
      Global => Vault_State,
      Volatile_Function;

   function Preview_Redeem (Shares : Share_Amount) return Asset_Amount with
      Global => Vault_State,
      Volatile_Function;

   procedure Redeem (
      Caller     : in     Address;
      Shares     : in     Share_Amount;
      Receiver   : in     Address;
      Owner_Addr : in     Address;
      Assets     : out    Asset_Amount;
      Success    : out    Boolean;
      Error      : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Receiver /= Zero_Address;

   ---------------------------------------------------------------------------
   --  Vault Management
   ---------------------------------------------------------------------------

   function Contract_Owner return Address with
      Global => Vault_State,
      Volatile_Function;

   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Vault_Error
   ) with
      Global => (In_Out => Vault_State),
      Pre    => New_Owner /= Zero_Address;

   ---------------------------------------------------------------------------
   --  Interface Support
   ---------------------------------------------------------------------------

   function Supports_Interface (Interface_ID : Bytes4) return Boolean with
      Global => null;

private

   ERC4626_Interface_ID : constant Bytes4 := (16#a2#, 16#19#, 16#a0#, 16#25#);

end ATS4626_Vault;
