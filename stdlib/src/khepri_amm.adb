--  KHEPRI AMM Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_AMM with
   SPARK_Mode => On,
   Refined_State => (Pool_State => (Pools, Pool_Configs, LP_Balances, Pool_Count))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Pool_Entry is record
      Reserves : Pool_Reserves;
      Used     : Boolean;
   end record;

   type Pool_Array is array (Pool_ID) of Pool_Entry;
   type Config_Array is array (Pool_ID) of Pool_Config;

   --  LP balance per pool per address
   Max_LP_Holders : constant := 10_000;
   type LP_Entry is record
      Pool    : Pool_ID;
      Account : Address;
      Balance : U256;
      Used    : Boolean;
   end record;

   type LP_Array is array (0 .. Max_LP_Holders - 1) of LP_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Pools       : Pool_Array := (others => (
      Reserves => (
         Reserve0   => U256_Zero,
         Reserve1   => U256_Zero,
         K_Last     => U256_Zero,
         Block_Last => 0
      ),
      Used => False
   ));

   Pool_Configs : Config_Array := (others => (
      Token0   => Null_Address,
      Token1   => Null_Address,
      LP_Token => Null_Address,
      Fee      => Default_Swap_Fee,
      Owner    => Null_Address
   ));

   LP_Balances : LP_Array := (others => (
      Pool    => Null_Pool,
      Account => Null_Address,
      Balance => U256_Zero,
      Used    => False
   ));

   Pool_Count : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_LP_Entry (
      Pool    : Pool_ID;
      Account : Address
   ) return Natural is
   begin
      for I in LP_Balances'Range loop
         if LP_Balances (I).Used and then
            LP_Balances (I).Pool = Pool and then
            LP_Balances (I).Account = Account
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_LP_Entry;

   function Find_Empty_LP_Entry return Natural is
   begin
      for I in LP_Balances'Range loop
         if not LP_Balances (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_LP_Entry;

   --  Order tokens (Token0 < Token1)
   procedure Order_Tokens (
      TokenA : in     Address;
      TokenB : in     Address;
      Token0 : out    Address;
      Token1 : out    Address
   ) is
   begin
      --  Simple comparison (first byte that differs)
      for I in Address'Range loop
         if TokenA (I) < TokenB (I) then
            Token0 := TokenA;
            Token1 := TokenB;
            return;
         elsif TokenA (I) > TokenB (I) then
            Token0 := TokenB;
            Token1 := TokenA;
            return;
         end if;
      end loop;
      --  Equal (shouldn't happen)
      Token0 := TokenA;
      Token1 := TokenB;
   end Order_Tokens;

   --  Calculate square root (integer)
   function Sqrt (Value : U256) return U256 is
      X      : U256 := Value;
      Y      : U256;
      Temp   : U256;
      Oflow  : Boolean;
   begin
      if Equal (Value, U256_Zero) then
         return U256_Zero;
      end if;

      --  Newton's method
      Y := From_Word64 (1);
      for I in 1 .. 256 loop
         --  Y = (X + Value / X) / 2
         if not Equal (X, U256_Zero) then
            Temp := Div (Value, X);
            Add (X, Temp, Y, Oflow);
            Y := Shift_Right (Y, 1);

            if Less_Than_Or_Equal (Y, X) and then
               not Equal (Y, X)
            then
               X := Y;
            else
               return X;
            end if;
         else
            return U256_Zero;
         end if;
      end loop;
      return X;
   end Sqrt;

   ---------------------------------------------------------------------------
   --  Pool Management
   ---------------------------------------------------------------------------

   procedure Create_Pool (
      Token0  : in     Address;
      Token1  : in     Address;
      Fee     : in     Natural;
      Pool    : out    Pool_ID;
      Success : out    Boolean;
      Error   : out    AMM_Error
   ) is
      Ordered0, Ordered1 : Address;
   begin
      Pool := Null_Pool;
      Success := False;
      Error := Error_None;

      --  Order tokens
      Order_Tokens (Token0, Token1, Ordered0, Ordered1);

      --  Check if pool exists
      if Get_Pool (Ordered0, Ordered1) /= Null_Pool then
         Error := Error_Pool_Already_Exists;
         return;
      end if;

      --  Find empty pool slot
      for I in Pool_ID range 1 .. Pool_ID'Last loop
         if not Pools (I).Used then
            --  Initialize pool
            Pools (I) := (
               Reserves => (
                  Reserve0   => U256_Zero,
                  Reserve1   => U256_Zero,
                  K_Last     => U256_Zero,
                  Block_Last => 0
               ),
               Used => True
            );

            Pool_Configs (I) := (
               Token0   => Ordered0,
               Token1   => Ordered1,
               LP_Token => Null_Address,  --  LP token address TBD
               Fee      => Fee,
               Owner    => Null_Address   --  Set by caller
            );

            Pool := I;
            Pool_Count := Pool_Count + 1;
            Success := True;
            return;
         end if;
      end loop;

      Error := Error_Pool_Not_Found;  --  No slots available
   end Create_Pool;

   function Get_Pool (
      Token0 : Address;
      Token1 : Address
   ) return Pool_ID is
      Ordered0, Ordered1 : Address;
   begin
      Order_Tokens (Token0, Token1, Ordered0, Ordered1);

      for I in Pool_ID range 1 .. Pool_ID'Last loop
         if Pools (I).Used and then
            Pool_Configs (I).Token0 = Ordered0 and then
            Pool_Configs (I).Token1 = Ordered1
         then
            return I;
         end if;
      end loop;

      return Null_Pool;
   end Get_Pool;

   procedure Get_Reserves (
      Pool     : in     Pool_ID;
      Reserve0 : out    U256;
      Reserve1 : out    U256;
      Success  : out    Boolean
   ) is
   begin
      if Pool = Null_Pool or else not Pools (Pool).Used then
         Reserve0 := U256_Zero;
         Reserve1 := U256_Zero;
         Success := False;
         return;
      end if;

      Reserve0 := Pools (Pool).Reserves.Reserve0;
      Reserve1 := Pools (Pool).Reserves.Reserve1;
      Success := True;
   end Get_Reserves;

   procedure Get_Config (
      Pool    : in     Pool_ID;
      Config  : out    Pool_Config;
      Success : out    Boolean
   ) is
   begin
      if Pool = Null_Pool or else not Pools (Pool).Used then
         Config := (
            Token0   => Null_Address,
            Token1   => Null_Address,
            LP_Token => Null_Address,
            Fee      => 0,
            Owner    => Null_Address
         );
         Success := False;
         return;
      end if;

      Config := Pool_Configs (Pool);
      Success := True;
   end Get_Config;

   ---------------------------------------------------------------------------
   --  Liquidity Operations
   ---------------------------------------------------------------------------

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
   ) is
      Reserve0    : U256;
      Reserve1    : U256;
      LP_Supply   : U256;
      Optimal1    : U256;
      Optimal0    : U256;
      Temp        : U256;
      High        : U256;
      Oflow       : Boolean;
      LP_Idx      : Natural;
   begin
      LP_Minted := U256_Zero;
      Actual0 := U256_Zero;
      Actual1 := U256_Zero;
      Success := False;
      Error := Error_None;

      if Pool = Null_Pool or else not Pools (Pool).Used then
         Error := Error_Pool_Not_Found;
         return;
      end if;

      Reserve0 := Pools (Pool).Reserves.Reserve0;
      Reserve1 := Pools (Pool).Reserves.Reserve1;
      LP_Supply := Get_LP_Supply (Pool);

      if Equal (Reserve0, U256_Zero) and then Equal (Reserve1, U256_Zero) then
         --  First liquidity: LP tokens = sqrt(amount0 * amount1) - MINIMUM_LIQUIDITY
         Mul (Amount0, Amount1, High, Temp);
         LP_Minted := Sqrt (Temp);

         --  Lock minimum liquidity
         Sub (LP_Minted, From_Word64 (Word64 (Minimum_Liquidity)), Temp, Oflow);
         if Oflow then
            Error := Error_Insufficient_Liquidity;
            return;
         end if;
         LP_Minted := Temp;

         Actual0 := Amount0;
         Actual1 := Amount1;
      else
         --  Calculate optimal amounts
         --  optimal1 = amount0 * reserve1 / reserve0
         Mul (Amount0, Reserve1, High, Temp);
         Optimal1 := Div (Temp, Reserve0);

         if Less_Than_Or_Equal (Optimal1, Amount1) then
            if Less_Than (Optimal1, Amount1_Min) then
               Error := Error_Slippage_Exceeded;
               return;
            end if;
            Actual0 := Amount0;
            Actual1 := Optimal1;
         else
            --  optimal0 = amount1 * reserve0 / reserve1
            Mul (Amount1, Reserve0, High, Temp);
            Optimal0 := Div (Temp, Reserve1);

            if Less_Than (Optimal0, Amount0_Min) then
               Error := Error_Slippage_Exceeded;
               return;
            end if;
            Actual0 := Optimal0;
            Actual1 := Amount1;
         end if;

         --  LP tokens: min(amount0 * supply / reserve0, amount1 * supply / reserve1)
         Mul (Actual0, LP_Supply, High, Temp);
         declare
            LP0 : constant U256 := Div (Temp, Reserve0);
            LP1 : U256;
         begin
            Mul (Actual1, LP_Supply, High, Temp);
            LP1 := Div (Temp, Reserve1);
            if Less_Than (LP0, LP1) then
               LP_Minted := LP0;
            else
               LP_Minted := LP1;
            end if;
         end;
      end if;

      if Equal (LP_Minted, U256_Zero) then
         Error := Error_Insufficient_Liquidity;
         return;
      end if;

      --  Update reserves
      Add (Reserve0, Actual0, Temp, Oflow);
      if Oflow then
         Error := Error_Overflow;
         return;
      end if;
      Pools (Pool).Reserves.Reserve0 := Temp;

      Add (Reserve1, Actual1, Temp, Oflow);
      if Oflow then
         Error := Error_Overflow;
         return;
      end if;
      Pools (Pool).Reserves.Reserve1 := Temp;

      --  Mint LP tokens
      LP_Idx := Find_LP_Entry (Pool, To);
      if LP_Idx = Natural'Last then
         LP_Idx := Find_Empty_LP_Entry;
         if LP_Idx = Natural'Last then
            Error := Error_Overflow;
            return;
         end if;
         LP_Balances (LP_Idx) := (
            Pool    => Pool,
            Account => To,
            Balance => LP_Minted,
            Used    => True
         );
      else
         Add (LP_Balances (LP_Idx).Balance, LP_Minted, Temp, Oflow);
         if Oflow then
            Error := Error_Overflow;
            return;
         end if;
         LP_Balances (LP_Idx).Balance := Temp;
      end if;

      Success := True;
   end Add_Liquidity;

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
   ) is
      Reserve0  : U256;
      Reserve1  : U256;
      LP_Supply : U256;
      Temp      : U256;
      High      : U256;
      Uflow     : Boolean;
      LP_Idx    : Natural;
   begin
      Amount0 := U256_Zero;
      Amount1 := U256_Zero;
      Success := False;
      Error := Error_None;

      if Pool = Null_Pool or else not Pools (Pool).Used then
         Error := Error_Pool_Not_Found;
         return;
      end if;

      Reserve0 := Pools (Pool).Reserves.Reserve0;
      Reserve1 := Pools (Pool).Reserves.Reserve1;
      LP_Supply := Get_LP_Supply (Pool);

      if Equal (LP_Supply, U256_Zero) then
         Error := Error_Insufficient_Liquidity;
         return;
      end if;

      --  Calculate amounts: amount = lp * reserve / supply
      Mul (LP_Amount, Reserve0, High, Temp);
      Amount0 := Div (Temp, LP_Supply);

      Mul (LP_Amount, Reserve1, High, Temp);
      Amount1 := Div (Temp, LP_Supply);

      --  Check minimums
      if Less_Than (Amount0, Amount0_Min) or else
         Less_Than (Amount1, Amount1_Min)
      then
         Error := Error_Slippage_Exceeded;
         Amount0 := U256_Zero;
         Amount1 := U256_Zero;
         return;
      end if;

      --  Burn LP tokens
      LP_Idx := Find_LP_Entry (Pool, To);
      if LP_Idx = Natural'Last then
         Error := Error_Insufficient_Liquidity;
         Amount0 := U256_Zero;
         Amount1 := U256_Zero;
         return;
      end if;

      if Less_Than (LP_Balances (LP_Idx).Balance, LP_Amount) then
         Error := Error_Insufficient_Liquidity;
         Amount0 := U256_Zero;
         Amount1 := U256_Zero;
         return;
      end if;

      Sub (LP_Balances (LP_Idx).Balance, LP_Amount, Temp, Uflow);
      LP_Balances (LP_Idx).Balance := Temp;

      --  Update reserves
      Sub (Reserve0, Amount0, Temp, Uflow);
      Pools (Pool).Reserves.Reserve0 := Temp;

      Sub (Reserve1, Amount1, Temp, Uflow);
      Pools (Pool).Reserves.Reserve1 := Temp;

      Success := True;
   end Remove_Liquidity;

   ---------------------------------------------------------------------------
   --  Swap Operations
   ---------------------------------------------------------------------------

   procedure Swap_Exact_Input (
      Pool           : in     Pool_ID;
      Amount_In      : in     U256;
      Amount_Out_Min : in     U256;
      Direction      : in     Swap_Direction;
      To             : in     Address;
      Amount_Out     : out    U256;
      Success        : out    Boolean;
      Error          : out    AMM_Error
   ) is
      Reserve0   : U256;
      Reserve1   : U256;
      Reserve_In : U256;
      Reserve_Out : U256;
      Fee        : Natural;
      New_Res_In : U256;
      New_Res_Out : U256;
      Temp       : U256;
      Oflow      : Boolean;
   begin
      Amount_Out := U256_Zero;
      Success := False;
      Error := Error_None;

      if Pool = Null_Pool or else not Pools (Pool).Used then
         Error := Error_Pool_Not_Found;
         return;
      end if;

      Reserve0 := Pools (Pool).Reserves.Reserve0;
      Reserve1 := Pools (Pool).Reserves.Reserve1;
      Fee := Pool_Configs (Pool).Fee;

      case Direction is
         when Token0_To_Token1 =>
            Reserve_In := Reserve0;
            Reserve_Out := Reserve1;
         when Token1_To_Token0 =>
            Reserve_In := Reserve1;
            Reserve_Out := Reserve0;
      end case;

      Amount_Out := Get_Amount_Out (Amount_In, Reserve_In, Reserve_Out, Fee);

      if Less_Than (Amount_Out, Amount_Out_Min) then
         Error := Error_Slippage_Exceeded;
         Amount_Out := U256_Zero;
         return;
      end if;

      --  Update reserves
      Add (Reserve_In, Amount_In, New_Res_In, Oflow);
      if Oflow then
         Error := Error_Overflow;
         Amount_Out := U256_Zero;
         return;
      end if;

      Sub (Reserve_Out, Amount_Out, New_Res_Out, Oflow);
      if Oflow then
         Error := Error_Insufficient_Liquidity;
         Amount_Out := U256_Zero;
         return;
      end if;

      case Direction is
         when Token0_To_Token1 =>
            Pools (Pool).Reserves.Reserve0 := New_Res_In;
            Pools (Pool).Reserves.Reserve1 := New_Res_Out;
         when Token1_To_Token0 =>
            Pools (Pool).Reserves.Reserve1 := New_Res_In;
            Pools (Pool).Reserves.Reserve0 := New_Res_Out;
      end case;

      Success := True;
   end Swap_Exact_Input;

   procedure Swap_Exact_Output (
      Pool          : in     Pool_ID;
      Amount_Out    : in     U256;
      Amount_In_Max : in     U256;
      Direction     : in     Swap_Direction;
      To            : in     Address;
      Amount_In     : out    U256;
      Success       : out    Boolean;
      Error         : out    AMM_Error
   ) is
      Reserve0    : U256;
      Reserve1    : U256;
      Reserve_In  : U256;
      Reserve_Out : U256;
      Fee         : Natural;
      New_Res_In  : U256;
      New_Res_Out : U256;
      Oflow       : Boolean;
   begin
      Amount_In := U256_Zero;
      Success := False;
      Error := Error_None;

      if Pool = Null_Pool or else not Pools (Pool).Used then
         Error := Error_Pool_Not_Found;
         return;
      end if;

      Reserve0 := Pools (Pool).Reserves.Reserve0;
      Reserve1 := Pools (Pool).Reserves.Reserve1;
      Fee := Pool_Configs (Pool).Fee;

      case Direction is
         when Token0_To_Token1 =>
            Reserve_In := Reserve0;
            Reserve_Out := Reserve1;
         when Token1_To_Token0 =>
            Reserve_In := Reserve1;
            Reserve_Out := Reserve0;
      end case;

      if Greater_Than_Or_Equal (Amount_Out, Reserve_Out) then
         Error := Error_Insufficient_Liquidity;
         return;
      end if;

      Amount_In := Get_Amount_In (Amount_Out, Reserve_In, Reserve_Out, Fee);

      if Greater_Than (Amount_In, Amount_In_Max) then
         Error := Error_Slippage_Exceeded;
         Amount_In := U256_Zero;
         return;
      end if;

      --  Update reserves
      Add (Reserve_In, Amount_In, New_Res_In, Oflow);
      if Oflow then
         Error := Error_Overflow;
         Amount_In := U256_Zero;
         return;
      end if;

      Sub (Reserve_Out, Amount_Out, New_Res_Out, Oflow);

      case Direction is
         when Token0_To_Token1 =>
            Pools (Pool).Reserves.Reserve0 := New_Res_In;
            Pools (Pool).Reserves.Reserve1 := New_Res_Out;
         when Token1_To_Token0 =>
            Pools (Pool).Reserves.Reserve1 := New_Res_In;
            Pools (Pool).Reserves.Reserve0 := New_Res_Out;
      end case;

      Success := True;
   end Swap_Exact_Output;

   ---------------------------------------------------------------------------
   --  Flash Swap
   ---------------------------------------------------------------------------

   procedure Flash_Swap (
      Pool       : in     Pool_ID;
      Amount0    : in     U256;
      Amount1    : in     U256;
      Recipient  : in     Address;
      Callback   : in     Address;
      Data       : in     Hash256;
      Success    : out    Boolean;
      Error      : out    AMM_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      if Pool = Null_Pool or else not Pools (Pool).Used then
         Error := Error_Pool_Not_Found;
         return;
      end if;

      --  Flash swap implementation would call the callback contract
      --  and verify k invariant is maintained after callback returns
      --  For now, this is a placeholder
      Error := Error_Flash_Callback_Failed;
   end Flash_Swap;

   ---------------------------------------------------------------------------
   --  Price Functions
   ---------------------------------------------------------------------------

   function Get_Amount_Out (
      Amount_In   : U256;
      Reserve_In  : U256;
      Reserve_Out : U256;
      Fee         : Natural
   ) return U256 is
      Amount_In_With_Fee : U256;
      Numerator          : U256;
      Denominator        : U256;
      Temp               : U256;
      High               : U256;
      Oflow              : Boolean;
   begin
      --  amount_out = (amount_in * (10000 - fee) * reserve_out) /
      --               (reserve_in * 10000 + amount_in * (10000 - fee))

      --  Amount with fee applied
      Mul (Amount_In, From_Word64 (Word64 (Fee_Denominator - Fee)), High, Temp);
      Amount_In_With_Fee := Temp;

      --  Numerator: amount_in_with_fee * reserve_out
      Mul (Amount_In_With_Fee, Reserve_Out, High, Numerator);

      --  Denominator: reserve_in * 10000 + amount_in_with_fee
      Mul (Reserve_In, From_Word64 (Word64 (Fee_Denominator)), High, Temp);
      Add (Temp, Amount_In_With_Fee, Denominator, Oflow);

      if Oflow or else Equal (Denominator, U256_Zero) then
         return U256_Zero;
      end if;

      return Div (Numerator, Denominator);
   end Get_Amount_Out;

   function Get_Amount_In (
      Amount_Out  : U256;
      Reserve_In  : U256;
      Reserve_Out : U256;
      Fee         : Natural
   ) return U256 is
      Numerator   : U256;
      Denominator : U256;
      Temp        : U256;
      High        : U256;
      Oflow       : Boolean;
      Uflow       : Boolean;
   begin
      --  amount_in = (reserve_in * amount_out * 10000) /
      --              ((reserve_out - amount_out) * (10000 - fee)) + 1

      --  Numerator: reserve_in * amount_out * 10000
      Mul (Reserve_In, Amount_Out, High, Temp);
      Mul (Temp, From_Word64 (Word64 (Fee_Denominator)), High, Numerator);

      --  Denominator: (reserve_out - amount_out) * (10000 - fee)
      Sub (Reserve_Out, Amount_Out, Temp, Uflow);
      if Uflow then
         return U256_Max;
      end if;
      Mul (Temp, From_Word64 (Word64 (Fee_Denominator - Fee)), High, Denominator);

      if Equal (Denominator, U256_Zero) then
         return U256_Max;
      end if;

      --  Add 1 to round up
      Temp := Div (Numerator, Denominator);
      Add (Temp, U256_One, Numerator, Oflow);
      if Oflow then
         return U256_Max;
      end if;

      return Numerator;
   end Get_Amount_In;

   function Get_Spot_Price (
      Reserve0 : U256;
      Reserve1 : U256
   ) return U256 is
      High : U256;
      Temp : U256;
   begin
      --  Price = Reserve1 * 10^18 / Reserve0 (scaled by 10^18)
      Mul (Reserve1, From_Word64 (10**18), High, Temp);
      return Div (Temp, Reserve0);
   end Get_Spot_Price;

   procedure Get_TWAP (
      Pool    : in     Pool_ID;
      Window  : in     Natural;
      Price0  : out    U256;
      Price1  : out    U256;
      Success : out    Boolean
   ) is
   begin
      --  TWAP requires cumulative price tracking across blocks
      --  Placeholder implementation
      Price0 := U256_Zero;
      Price1 := U256_Zero;
      Success := False;

      if Pool /= Null_Pool and then Pools (Pool).Used then
         --  Simple spot price for now (not true TWAP)
         Price0 := Get_Spot_Price (
            Pools (Pool).Reserves.Reserve0,
            Pools (Pool).Reserves.Reserve1
         );
         Price1 := Get_Spot_Price (
            Pools (Pool).Reserves.Reserve1,
            Pools (Pool).Reserves.Reserve0
         );
         Success := True;
      end if;
   end Get_TWAP;

   ---------------------------------------------------------------------------
   --  LP Token Operations
   ---------------------------------------------------------------------------

   function Get_LP_Supply (Pool : Pool_ID) return U256 is
      Total : U256 := U256_Zero;
      Temp  : U256;
      Oflow : Boolean;
   begin
      if Pool = Null_Pool or else not Pools (Pool).Used then
         return U256_Zero;
      end if;

      for I in LP_Balances'Range loop
         if LP_Balances (I).Used and then LP_Balances (I).Pool = Pool then
            Add (Total, LP_Balances (I).Balance, Temp, Oflow);
            if not Oflow then
               Total := Temp;
            end if;
         end if;
      end loop;

      return Total;
   end Get_LP_Supply;

   function Get_LP_Balance (
      Pool    : Pool_ID;
      Account : Address
   ) return U256 is
      Idx : constant Natural := Find_LP_Entry (Pool, Account);
   begin
      if Idx = Natural'Last then
         return U256_Zero;
      end if;
      return LP_Balances (Idx).Balance;
   end Get_LP_Balance;

   function Quote_Liquidity (
      Amount0   : U256;
      Reserve0  : U256;
      Reserve1  : U256;
      LP_Supply : U256
   ) return U256 is
      High : U256;
      Temp : U256;
   begin
      if Equal (LP_Supply, U256_Zero) then
         --  First liquidity
         Mul (Amount0, Reserve1, High, Temp);
         return Sqrt (Temp);
      else
         Mul (Amount0, LP_Supply, High, Temp);
         return Div (Temp, Reserve0);
      end if;
   end Quote_Liquidity;

   ---------------------------------------------------------------------------
   --  Pool Statistics
   ---------------------------------------------------------------------------

   procedure Get_TVL (
      Pool    : in     Pool_ID;
      TVL0    : out    U256;
      TVL1    : out    U256;
      Success : out    Boolean
   ) is
   begin
      if Pool = Null_Pool or else not Pools (Pool).Used then
         TVL0 := U256_Zero;
         TVL1 := U256_Zero;
         Success := False;
         return;
      end if;

      TVL0 := Pools (Pool).Reserves.Reserve0;
      TVL1 := Pools (Pool).Reserves.Reserve1;
      Success := True;
   end Get_TVL;

   procedure Get_Volume_24h (
      Pool    : in     Pool_ID;
      Volume0 : out    U256;
      Volume1 : out    U256;
      Success : out    Boolean
   ) is
   begin
      --  Would require tracking swaps over time
      Volume0 := U256_Zero;
      Volume1 := U256_Zero;
      Success := False;
   end Get_Volume_24h;

   procedure Get_Fees (
      Pool    : in     Pool_ID;
      Fees0   : out    U256;
      Fees1   : out    U256;
      Success : out    Boolean
   ) is
   begin
      --  Would require tracking accumulated fees
      Fees0 := U256_Zero;
      Fees1 := U256_Zero;
      Success := False;
   end Get_Fees;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   procedure Set_Fee (
      Pool    : in     Pool_ID;
      New_Fee : in     Natural;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    AMM_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      if Pool = Null_Pool or else not Pools (Pool).Used then
         Error := Error_Pool_Not_Found;
         return;
      end if;

      if Pool_Configs (Pool).Owner /= Caller then
         Error := Error_Invalid_Token;  --  Not authorized
         return;
      end if;

      Pool_Configs (Pool).Fee := New_Fee;
      Success := True;
   end Set_Fee;

   procedure Transfer_Ownership (
      Pool      : in     Pool_ID;
      New_Owner : in     Address;
      Caller    : in     Address;
      Success   : out    Boolean
   ) is
   begin
      Success := False;

      if Pool = Null_Pool or else not Pools (Pool).Used then
         return;
      end if;

      if Pool_Configs (Pool).Owner /= Caller then
         return;
      end if;

      Pool_Configs (Pool).Owner := New_Owner;
      Success := True;
   end Transfer_Ownership;

end Khepri_AMM;
