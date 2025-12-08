--  ATS-20: KHEPRI Standard Token Interface
--
--  This package defines the standard token interface for KHEPRI contracts,
--  equivalent to ERC-20 on Ethereum. Simplified version for compilation.
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Khepri_Types;    use Khepri_Types;

package ATS20_Token with
   SPARK_Mode => On,
   Abstract_State => (Token_State with External => Async_Writers),
   Initializes => Token_State
is

   --  Zero address constant (alias)
   Zero_Address : constant Address := Null_Address;

   --  Token amount (256-bit unsigned integer)
   subtype Token_Amount is Uint256;

   --  Error Codes
   type Token_Error is (
      Error_None,
      Error_Insufficient_Balance,
      Error_Insufficient_Allowance,
      Error_Zero_Address,
      Error_Not_Owner,
      Error_Overflow,
      Error_Paused,
      Error_Cap_Exceeded
   );

   --  Initialization
   procedure Initialize (
      Name           : in String;
      Symbol         : in String;
      Decimals       : in Natural;
      Initial_Supply : in Token_Amount;
      Owner          : in Address;
      Success        : out Boolean
   ) with
      Global => (In_Out => Token_State),
      Pre    => Owner /= Zero_Address and Name'Length <= 64 and Symbol'Length <= 16;

   --  View Functions
   function Total_Supply return Token_Amount with
      Global => Token_State,
      Volatile_Function;

   function Balance_Of (Account : Address) return Token_Amount with
      Global => Token_State,
      Volatile_Function;

   function Allowance (Owner_Addr, Spender : Address) return Token_Amount with
      Global => Token_State,
      Volatile_Function;

   function Owner return Address with
      Global => Token_State,
      Volatile_Function;

   function Is_Paused return Boolean with
      Global => Token_State,
      Volatile_Function;

   function Get_Decimals return Natural with
      Global => Token_State,
      Volatile_Function;

   --  Transfer Functions
   procedure Transfer (
      Caller    : in     Address;
      Recipient : in     Address;
      Amount    : in     Token_Amount;
      Success   : out    Boolean;
      Error     : out    Token_Error
   ) with
      Global => (In_Out => Token_State),
      Pre    => Recipient /= Zero_Address;

   procedure Approve (
      Caller  : in     Address;
      Spender : in     Address;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Token_Error
   ) with
      Global => (In_Out => Token_State),
      Pre    => Spender /= Zero_Address;

   procedure Transfer_From (
      Caller    : in     Address;
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Amount    : in     Token_Amount;
      Success   : out    Boolean;
      Error     : out    Token_Error
   ) with
      Global => (In_Out => Token_State),
      Pre    => To_Addr /= Zero_Address;

   --  Mint and Burn (Owner Only)
   procedure Mint (
      Caller  : in     Address;
      To      : in     Address;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Token_Error
   ) with
      Global => (In_Out => Token_State),
      Pre    => To /= Zero_Address;

   procedure Burn (
      Caller  : in     Address;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Token_Error
   ) with
      Global => (In_Out => Token_State);

   --  Ownership
   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Token_Error
   ) with
      Global => (In_Out => Token_State),
      Pre    => New_Owner /= Zero_Address;

   --  Utility
   function Is_Owner (Account : Address) return Boolean with
      Global => Token_State,
      Volatile_Function;

end ATS20_Token;
