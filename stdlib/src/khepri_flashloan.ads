--  KHEPRI Flash Loan
--
--  Flash loan implementation for uncollateralized borrowing within a single
--  transaction. Borrowed funds must be returned with fee before transaction ends.
--
--  Features:
--  - Multi-token flash loans
--  - Configurable fee per token
--  - Callback-based execution
--  - Reentrancy protection
--  - Protocol fee collection
--
--  Security:
--  - Atomicity guaranteed (loan + repayment in single tx)
--  - Balance verification before and after callback
--  - Reentrancy guard prevents nested flash loans
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_U256;         use Aegis_U256;
with Khepri_Types;       use Khepri_Types;

package Khepri_FlashLoan with
   SPARK_Mode => On,
   Abstract_State => (FlashLoan_State with External => Async_Writers),
   Initializes => FlashLoan_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Fee denominator (10000 = 100%, 9 = 0.09%)
   Fee_Denominator : constant := 10_000;

   --  Default fee (9 basis points = 0.09%)
   Default_Fee : constant := 9;

   --  Maximum supported tokens
   Max_Tokens : constant := 100;

   --  Maximum flash loan amount per token
   Max_Loan_Per_Token : constant U256 := U256_Max;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Token configuration for flash loans
   type Token_Config is record
      Token      : Address;     --  Token address
      Fee        : Natural;     --  Fee in basis points
      Available  : U256;        --  Available liquidity
      Borrowed   : U256;        --  Currently borrowed amount
      Enabled    : Boolean;     --  Whether flash loans enabled
   end record;

   --  Flash loan request
   type Loan_Request is record
      Token     : Address;
      Amount    : U256;
      Receiver  : Address;      --  Contract receiving the loan
      Callback  : Hash256;      --  Callback function selector
      Data      : Hash256;      --  Hash of callback data
   end record;

   --  Multi-token loan request
   Max_Loans_Per_Tx : constant := 10;
   type Loan_Index is range 0 .. Max_Loans_Per_Tx - 1;
   type Loan_Request_Array is array (Loan_Index) of Loan_Request;

   --  Flash loan result
   type Loan_Result is record
      Success      : Boolean;
      Fee_Paid     : U256;
      Error_Code   : Natural;
   end record;

   --  Error codes
   type FlashLoan_Error is (
      Error_None,
      Error_Token_Not_Supported,
      Error_Insufficient_Liquidity,
      Error_Zero_Amount,
      Error_Reentrancy,
      Error_Repayment_Failed,
      Error_Fee_Too_High,
      Error_Callback_Failed,
      Error_Unauthorized,
      Error_Disabled
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize flash loan contract
   procedure Initialize (
      Owner : in Address
   ) with
      Global => (Output => FlashLoan_State);

   ---------------------------------------------------------------------------
   --  Flash Loan Operations
   ---------------------------------------------------------------------------

   --  Execute single-token flash loan
   procedure Flash_Loan (
      Token    : in     Address;
      Amount   : in     U256;
      Receiver : in     Address;
      Callback : in     Hash256;
      Data     : in     Hash256;
      Success  : out    Boolean;
      Error    : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State),
      Pre    => not Equal (Amount, U256_Zero);

   --  Execute multi-token flash loan
   procedure Flash_Loan_Multi (
      Loans     : in     Loan_Request_Array;
      Count     : in     Natural;
      Receiver  : in     Address;
      Callback  : in     Hash256;
      Data      : in     Hash256;
      Success   : out    Boolean;
      Error     : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State),
      Pre    => Count <= Max_Loans_Per_Tx;

   ---------------------------------------------------------------------------
   --  Liquidity Management
   ---------------------------------------------------------------------------

   --  Deposit liquidity for flash loans
   procedure Deposit (
      Token   : in     Address;
      Amount  : in     U256;
      From    : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State),
      Pre    => not Equal (Amount, U256_Zero);

   --  Withdraw liquidity
   procedure Withdraw (
      Token   : in     Address;
      Amount  : in     U256;
      To      : in     Address;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State),
      Pre    => not Equal (Amount, U256_Zero);

   ---------------------------------------------------------------------------
   --  Fee Calculation
   ---------------------------------------------------------------------------

   --  Calculate fee for loan amount
   function Calculate_Fee (
      Amount : U256;
      Fee_BP : Natural
   ) return U256 with
      Global => null,
      Pre    => Fee_BP < Fee_Denominator;

   --  Get total repayment amount (principal + fee)
   function Get_Repayment_Amount (
      Amount : U256;
      Fee_BP : Natural
   ) return U256 with
      Global => null,
      Pre    => Fee_BP < Fee_Denominator;

   ---------------------------------------------------------------------------
   --  Token Management
   ---------------------------------------------------------------------------

   --  Add token for flash loans
   procedure Add_Token (
      Token   : in     Address;
      Fee     : in     Natural;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State),
      Pre    => Fee < Fee_Denominator;

   --  Remove token from flash loans
   procedure Remove_Token (
      Token   : in     Address;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State);

   --  Update token fee
   procedure Set_Token_Fee (
      Token   : in     Address;
      New_Fee : in     Natural;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State),
      Pre    => New_Fee < Fee_Denominator;

   --  Enable/disable flash loans for token
   procedure Set_Token_Enabled (
      Token   : in     Address;
      Enabled : in     Boolean;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State);

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   --  Check if token is supported
   function Is_Token_Supported (Token : Address) return Boolean with
      Global => FlashLoan_State,
      Volatile_Function;

   --  Get available liquidity for token
   function Get_Available_Liquidity (Token : Address) return U256 with
      Global => FlashLoan_State,
      Volatile_Function;

   --  Get token fee
   function Get_Token_Fee (Token : Address) return Natural with
      Global => FlashLoan_State,
      Volatile_Function;

   --  Check if currently in flash loan (reentrancy check)
   function Is_Flash_Loan_Active return Boolean with
      Global => FlashLoan_State,
      Volatile_Function;

   --  Get total fees collected for token
   function Get_Fees_Collected (Token : Address) return U256 with
      Global => FlashLoan_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   --  Collect accumulated fees
   procedure Collect_Fees (
      Token   : in     Address;
      To      : in     Address;
      Caller  : in     Address;
      Amount  : out    U256;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) with
      Global => (In_Out => FlashLoan_State);

   --  Transfer ownership
   procedure Transfer_Ownership (
      New_Owner : in     Address;
      Caller    : in     Address;
      Success   : out    Boolean
   ) with
      Global => (In_Out => FlashLoan_State);

   --  Emergency pause
   procedure Pause (
      Caller  : in     Address;
      Success : out    Boolean
   ) with
      Global => (In_Out => FlashLoan_State);

   --  Unpause
   procedure Unpause (
      Caller  : in     Address;
      Success : out    Boolean
   ) with
      Global => (In_Out => FlashLoan_State);

   --  Check if paused
   function Is_Paused return Boolean with
      Global => FlashLoan_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------

   type FlashLoan_Event is record
      Initiator : Address;
      Token     : Address;
      Amount    : U256;
      Fee       : U256;
      Receiver  : Address;
   end record;

   type Deposit_Event is record
      Token  : Address;
      Amount : U256;
      From   : Address;
   end record;

   type Withdraw_Event is record
      Token  : Address;
      Amount : U256;
      To     : Address;
   end record;

   type FeeCollected_Event is record
      Token  : Address;
      Amount : U256;
      To     : Address;
   end record;

   ---------------------------------------------------------------------------
   --  Callback Interface
   ---------------------------------------------------------------------------

   --  Receiver must implement this callback
   --  procedure Execute_Operation (
   --     Token      : Address;
   --     Amount     : U256;
   --     Fee        : U256;
   --     Initiator  : Address;
   --     Data       : Byte_Array;
   --     Success    : out Boolean
   --  );
   --
   --  Return True if operation succeeded and repayment is ready

end Khepri_FlashLoan;
