--  DEX Trading Contract
--  A decentralized exchange with order book and matching engine
--
--  Features:
--  - Limit orders (buy/sell at specific price)
--  - Market orders (immediate execution at best price)
--  - Order cancellation
--  - Fee system (0.3% trading fee)
--  - Balance tracking
--  - Order matching

pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;

package DEX_Trading with
   Abstract_State => Trading_State,
   Initializes => Trading_State
is

   --  Order types
   type Order_Side is (Buy, Sell);
   type Order_Status is (Open, Filled, Cancelled);
   
   --  Order ID type
   type Order_ID is new Natural range 0 .. 999;
   
   --  Price and amount are in base units (e.g., wei)
   subtype Price is U256;
   subtype Amount is U256;
   
   --  Order record
   type Order_Record is record
      ID       : Order_ID;
      Trader   : Contract_Address;
      Side     : Order_Side;
      Price    : U256;  -- Price per token
      Amount   : U256;  -- Amount of tokens
      Filled   : U256;  -- Amount filled so far
      Status   : Order_Status;
      Valid    : Boolean;
   end record;
   
   --  Initialize the DEX
   procedure Initialize (
      Owner   : in Contract_Address;
      Success : out Boolean
   ) with
      Global => (Output => Trading_State),
      Pre => Owner /= Address_Zero,
      Post => Success;
   
   --  Deposit tokens into the DEX
   procedure Deposit (
      Trader  : in Contract_Address;
      Token   : in Contract_Address;
      Amount  : in U256;
      Success : out Boolean
   ) with
      Global => (In_Out => Trading_State),
      Pre => Trader /= Address_Zero and then
             Token /= Address_Zero and then
             Amount /= U256_Zero;
   
   --  Withdraw tokens from the DEX
   procedure Withdraw (
      Trader  : in Contract_Address;
      Token   : in Contract_Address;
      Amount  : in U256;
      Success : out Boolean
   ) with
      Global => (In_Out => Trading_State),
      Pre => Trader /= Address_Zero and then
             Token /= Address_Zero and then
             Amount /= U256_Zero;
   
   --  Place a limit order
   procedure Place_Limit_Order (
      Trader     : in Contract_Address;
      Side       : in Order_Side;
      Order_Price : in U256;
      Order_Amount : in U256;
      Order_Out  : out Order_ID;
      Success    : out Boolean
   ) with
      Global => (In_Out => Trading_State),
      Pre => Trader /= Address_Zero and then
             Order_Price /= U256_Zero and then
             Order_Amount /= U256_Zero;
   
   --  Execute a market order (immediate execution at best available price)
   procedure Execute_Market_Order (
      Trader       : in Contract_Address;
      Side         : in Order_Side;
      Order_Amount : in U256;
      Filled_Amount : out U256;
      Success      : out Boolean
   ) with
      Global => (In_Out => Trading_State),
      Pre => Trader /= Address_Zero and then
             Order_Amount /= U256_Zero;
   
   --  Cancel an order
   procedure Cancel_Order (
      Trader  : in Contract_Address;
      Order   : in Order_ID;
      Success : out Boolean
   ) with
      Global => (In_Out => Trading_State),
      Pre => Trader /= Address_Zero;
   
   --  Get balance of a trader for a specific token
   function Get_Balance (
      Trader : Contract_Address;
      Token  : Contract_Address
   ) return U256 with
      Global => (Input => Trading_State),
      Pre => Trader /= Address_Zero and then Token /= Address_Zero;
   
   --  Get order details
   function Get_Order (
      Order : Order_ID
   ) return Order_Record with
      Global => (Input => Trading_State);
   
   --  Get total number of open orders
   function Get_Order_Count return Natural with
      Global => (Input => Trading_State);
   
   --  Get total trading volume
   function Get_Total_Volume return U256 with
      Global => (Input => Trading_State);
   
   --  Get collected fees
   function Get_Collected_Fees return U256 with
      Global => (Input => Trading_State);

end DEX_Trading;

