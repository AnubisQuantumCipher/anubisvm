--  KHEPRI AMM (Automated Market Maker)
--
--  Constant Product AMM implementation (x * y = k) for decentralized trading.
--  Supports token swaps, liquidity provision, and fee collection.
--
--  Features:
--  - Constant product formula (Uniswap v2 style)
--  - LP token minting for liquidity providers
--  - Configurable swap fee (default 0.3%)
--  - Flash swap support
--  - Price oracle integration
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_U256;         use Aegis_U256;
with Khepri_Types;       use Khepri_Types;

package Khepri_AMM with
   SPARK_Mode => On,
   Abstract_State => (Pool_State with External => Async_Writers),
   Initializes => Pool_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Fee denominator (10000 = 100%)
   Fee_Denominator : constant := 10_000;

   --  Default swap fee (30 = 0.3%)
   Default_Swap_Fee : constant := 30;

   --  Minimum liquidity (locked forever to prevent manipulation)
   Minimum_Liquidity : constant := 1_000;

   --  Maximum pools per AMM
   Max_Pools : constant := 1_000;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Pool_ID is new Natural range 0 .. Max_Pools;
   Null_Pool : constant Pool_ID := 0;

   --  Pool reserves
   type Pool_Reserves is record
      Reserve0   : U256;     --  Token0 reserve
      Reserve1   : U256;     --  Token1 reserve
      K_Last     : U256;     --  Last k value (for fee calculation)
      Block_Last : Word64;   --  Last update block
   end record;

   --  Pool configuration
   type Pool_Config is record
      Token0     : Address;     --  Token0 address
      Token1     : Address;     --  Token1 address
      LP_Token   : Address;     --  LP token address
      Fee        : Natural;     --  Swap fee (basis points)
      Owner      : Address;     --  Pool owner
   end record;

   --  Swap direction
   type Swap_Direction is (Token0_To_Token1, Token1_To_Token0);

   --  Error types
   type AMM_Error is (
      Error_None,
      Error_Pool_Not_Found,
      Error_Pool_Already_Exists,
      Error_Insufficient_Liquidity,
      Error_Insufficient_Input,
      Error_Insufficient_Output,
      Error_Slippage_Exceeded,
      Error_Zero_Amount,
      Error_Invalid_Token,
      Error_Overflow,
      Error_K_Invariant_Violated,
      Error_Flash_Callback_Failed
   );

   ---------------------------------------------------------------------------
   --  Pool Management
   ---------------------------------------------------------------------------

   --  Create new trading pool
   procedure Create_Pool (
      Token0  : in     Address;
      Token1  : in     Address;
      Fee     : in     Natural;
      Pool    : out    Pool_ID;
      Success : out    Boolean;
      Error   : out    AMM_Error
   ) with
      Global => (In_Out => Pool_State),
      Pre    => Fee < Fee_Denominator;

   --  Get pool by token pair
   function Get_Pool (
      Token0 : Address;
      Token1 : Address
   ) return Pool_ID with
      Global => Pool_State,
      Volatile_Function;

   --  Get pool reserves
   procedure Get_Reserves (
      Pool     : in     Pool_ID;
      Reserve0 : out    U256;
      Reserve1 : out    U256;
      Success  : out    Boolean
   ) with
      Global => Pool_State;

   --  Get pool configuration
   procedure Get_Config (
      Pool    : in     Pool_ID;
      Config  : out    Pool_Config;
      Success : out    Boolean
   ) with
      Global => Pool_State;

   ---------------------------------------------------------------------------
   --  Liquidity Operations
   ---------------------------------------------------------------------------

   --  Add liquidity to pool
   --  Returns: LP tokens minted
   procedure Add_Liquidity (
      Pool          : in     Pool_ID;
      Amount0       : in     U256;
      Amount1       : in     U256;
      Amount0_Min   : in     U256;
      Amount1_Min   : in     U256;
      To            : in     Address;
      LP_Minted     : out    U256;
      Actual0       : out    U256;
      Actual1       : out    U256;
      Success       : out    Boolean;
      Error         : out    AMM_Error
   ) with
      Global => (In_Out => Pool_State),
      Pre    => not Equal (Amount0, U256_Zero) and
                not Equal (Amount1, U256_Zero);

   --  Remove liquidity from pool
   --  Returns: Token amounts received
   procedure Remove_Liquidity (
      Pool        : in     Pool_ID;
      LP_Amount   : in     U256;
      Amount0_Min : in     U256;
      Amount1_Min : in     U256;
      To          : in     Address;
      Amount0     : out    U256;
      Amount1     : out    U256;
      Success     : out    Boolean;
      Error       : out    AMM_Error
   ) with
      Global => (In_Out => Pool_State),
      Pre    => not Equal (LP_Amount, U256_Zero);

   ---------------------------------------------------------------------------
   --  Swap Operations
   ---------------------------------------------------------------------------

   --  Swap exact input for output
   procedure Swap_Exact_Input (
      Pool          : in     Pool_ID;
      Amount_In     : in     U256;
      Amount_Out_Min : in    U256;
      Direction     : in     Swap_Direction;
      To            : in     Address;
      Amount_Out    : out    U256;
      Success       : out    Boolean;
      Error         : out    AMM_Error
   ) with
      Global => (In_Out => Pool_State),
      Pre    => not Equal (Amount_In, U256_Zero);

   --  Swap input for exact output
   procedure Swap_Exact_Output (
      Pool          : in     Pool_ID;
      Amount_Out    : in     U256;
      Amount_In_Max : in     U256;
      Direction     : in     Swap_Direction;
      To            : in     Address;
      Amount_In     : out    U256;
      Success       : out    Boolean;
      Error         : out    AMM_Error
   ) with
      Global => (In_Out => Pool_State),
      Pre    => not Equal (Amount_Out, U256_Zero);

   ---------------------------------------------------------------------------
   --  Flash Swap
   ---------------------------------------------------------------------------

   --  Execute flash swap (borrow tokens, must repay in same transaction)
   procedure Flash_Swap (
      Pool       : in     Pool_ID;
      Amount0    : in     U256;
      Amount1    : in     U256;
      Recipient  : in     Address;
      Callback   : in     Address;
      Data       : in     Hash256;
      Success    : out    Boolean;
      Error      : out    AMM_Error
   ) with
      Global => (In_Out => Pool_State);

   ---------------------------------------------------------------------------
   --  Price Functions
   ---------------------------------------------------------------------------

   --  Get amount out for given input
   function Get_Amount_Out (
      Amount_In   : U256;
      Reserve_In  : U256;
      Reserve_Out : U256;
      Fee         : Natural
   ) return U256 with
      Global => null,
      Pre    => not Equal (Reserve_In, U256_Zero) and
                not Equal (Reserve_Out, U256_Zero) and
                Fee < Fee_Denominator;

   --  Get amount in for desired output
   function Get_Amount_In (
      Amount_Out  : U256;
      Reserve_In  : U256;
      Reserve_Out : U256;
      Fee         : Natural
   ) return U256 with
      Global => null,
      Pre    => not Equal (Reserve_In, U256_Zero) and
                not Equal (Reserve_Out, U256_Zero) and
                Fee < Fee_Denominator and
                Less_Than (Amount_Out, Reserve_Out);

   --  Get spot price (Token1 per Token0)
   function Get_Spot_Price (
      Reserve0 : U256;
      Reserve1 : U256
   ) return U256 with
      Global => null,
      Pre    => not Equal (Reserve0, U256_Zero);

   --  Get TWAP (Time-Weighted Average Price)
   procedure Get_TWAP (
      Pool    : in     Pool_ID;
      Window  : in     Natural;    --  Time window in blocks
      Price0  : out    U256;       --  Average price of Token0 in Token1
      Price1  : out    U256;       --  Average price of Token1 in Token0
      Success : out    Boolean
   ) with
      Global => Pool_State;

   ---------------------------------------------------------------------------
   --  LP Token Operations
   ---------------------------------------------------------------------------

   --  Get total LP supply for pool
   function Get_LP_Supply (Pool : Pool_ID) return U256 with
      Global => Pool_State,
      Volatile_Function;

   --  Get LP balance for address
   function Get_LP_Balance (
      Pool    : Pool_ID;
      Account : Address
   ) return U256 with
      Global => Pool_State,
      Volatile_Function;

   --  Calculate LP tokens for liquidity
   function Quote_Liquidity (
      Amount0    : U256;
      Reserve0   : U256;
      Reserve1   : U256;
      LP_Supply  : U256
   ) return U256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Pool Statistics
   ---------------------------------------------------------------------------

   --  Get total value locked in pool
   procedure Get_TVL (
      Pool    : in     Pool_ID;
      TVL0    : out    U256;
      TVL1    : out    U256;
      Success : out    Boolean
   ) with
      Global => Pool_State;

   --  Get 24h volume (requires oracle updates)
   procedure Get_Volume_24h (
      Pool    : in     Pool_ID;
      Volume0 : out    U256;
      Volume1 : out    U256;
      Success : out    Boolean
   ) with
      Global => Pool_State;

   --  Get accumulated fees
   procedure Get_Fees (
      Pool    : in     Pool_ID;
      Fees0   : out    U256;
      Fees1   : out    U256;
      Success : out    Boolean
   ) with
      Global => Pool_State;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   --  Set swap fee (pool owner only)
   procedure Set_Fee (
      Pool    : in     Pool_ID;
      New_Fee : in     Natural;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    AMM_Error
   ) with
      Global => (In_Out => Pool_State),
      Pre    => New_Fee < Fee_Denominator;

   --  Transfer pool ownership
   procedure Transfer_Ownership (
      Pool      : in     Pool_ID;
      New_Owner : in     Address;
      Caller    : in     Address;
      Success   : out    Boolean
   ) with
      Global => (In_Out => Pool_State);

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------

   type Swap_Event is record
      Pool       : Pool_ID;
      Sender     : Address;
      Amount0_In : U256;
      Amount1_In : U256;
      Amount0_Out : U256;
      Amount1_Out : U256;
      To         : Address;
   end record;

   type Mint_Event is record
      Pool    : Pool_ID;
      Sender  : Address;
      Amount0 : U256;
      Amount1 : U256;
   end record;

   type Burn_Event is record
      Pool    : Pool_ID;
      Sender  : Address;
      Amount0 : U256;
      Amount1 : U256;
      To      : Address;
   end record;

   type Sync_Event is record
      Pool     : Pool_ID;
      Reserve0 : U256;
      Reserve1 : U256;
   end record;

end Khepri_AMM;
