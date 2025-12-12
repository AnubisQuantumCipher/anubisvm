pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_U256; use Aegis_U256;

package body DEX_Trading with
   Refined_State => (Trading_State => (Owner_Address, Order_Book, Balances,
                                       Order_Counter, Total_Volume, Collected_Fees))
is

   --  Maximum orders in the book
   Max_Orders : constant := 1000;
   
   --  Trading fee (30 basis points = 0.3%)
   Fee_Basis_Points : constant := 30;
   Fee_Denominator  : constant := 10_000;
   
   --  State variables
   Owner_Address : Contract_Address := Address_Zero;
   Order_Book : array (Order_ID) of Order_Record := (others => (
      ID      => 0,
      Trader  => Address_Zero,
      Side    => Buy,
      Price   => U256_Zero,
      Amount  => U256_Zero,
      Filled  => U256_Zero,
      Status  => Open,
      Valid   => False
   ));
   
   --  Balance tracking: Simple array for demo (real impl would use mapping)
   --  Index 0-499: Token A balances, 500-999: Token B balances
   type Balance_Index is range 0 .. 999;
   type Balance_Array is array (Balance_Index) of U256;
   Balances : Balance_Array := (others => U256_Zero);
   
   --  Order counter
   Order_Counter : Natural := 0;
   
   --  Trading stats
   Total_Volume    : U256 := U256_Zero;
   Collected_Fees  : U256 := U256_Zero;
   
   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   --  Get balance index for (Trader, Token). For demo purposes we just hash
   --  the address bytes into a small range and use 0..499 for one token,
   --  500..999 for another.
   function Get_Balance_Index (
      Trader : Contract_Address;
      Token  : Contract_Address
   ) return Balance_Index is
      Hash : Unsigned_64 := 0;
   begin
      --  Very simple hash: sum of bytes
      for I in Trader'Range loop
         Hash := Hash + Unsigned_64 (Trader (I));
      end loop;

      for I in Token'Range loop
         Hash := Hash + Unsigned_64 (Token (I));
      end loop;

      return Balance_Index (Hash mod 1000);
   end Get_Balance_Index;

   --  Calculate trading fee: amount * Fee_Basis_Points / Fee_Denominator
   function Calculate_Fee (Amount : U256) return U256 is
      Fee_Numer  : U256;
      Fee_Denom  : U256 := From_Word64 (Fee_Denominator);
      Quotient   : U256;
      Remainder  : U256;
   begin
      Fee_Numer := Mul_Mod (Amount, From_Word64 (Fee_Basis_Points));
      Div_Mod
        (Fee_Numer, Fee_Denom,
         Quotient  => Quotient,
         Remainder => Remainder);
      return Quotient;
   end Calculate_Fee;

   ---------------------------------------------------------------------------
   --  API Implementation
   ---------------------------------------------------------------------------

   procedure Initialize (
      Owner   : in Contract_Address;
      Success : out Boolean
   ) is
   begin
      Owner_Address := Owner;
      Order_Counter := 0;
      Total_Volume  := U256_Zero;
      Collected_Fees := U256_Zero;
      Order_Book := (others => (
                      ID      => 0,
                      Trader  => Address_Zero,
                      Side    => Buy,
                      Price   => U256_Zero,
                      Amount  => U256_Zero,
                      Filled  => U256_Zero,
                      Status  => Open,
                      Valid   => False
                    ));
      Balances := (others => U256_Zero);
      Success := True;
   end Initialize;
   
   --  Deposit
   procedure Deposit (
      Trader  : in Contract_Address;
      Token   : in Contract_Address;
      Amount  : in U256;
      Success : out Boolean
   ) is
      Idx         : constant Balance_Index := Get_Balance_Index (Trader, Token);
      New_Balance : U256;
   begin
      --  Add to balance (with overflow check)
      New_Balance := Add_Mod (Balances (Idx), Amount);
      
      --  Check for overflow
      if Less_Than (New_Balance, Balances (Idx)) then
         Success := False;
         return;
      end if;
      
      Balances (Idx) := New_Balance;
      Success := True;
   end Deposit;
   
   --  Withdraw
   procedure Withdraw (
      Trader  : in Contract_Address;
      Token   : in Contract_Address;
      Amount  : in U256;
      Success : out Boolean
   ) is
      Idx : constant Balance_Index := Get_Balance_Index (Trader, Token);
   begin
      --  Check sufficient balance
      if Less_Than (Balances (Idx), Amount) then
         Success := False;
         return;
      end if;
      
      Balances (Idx) := Sub_Mod (Balances (Idx), Amount);
      Success := True;
   end Withdraw;
   
   --  Place limit order
   procedure Place_Limit_Order (
      Trader       : in Contract_Address;
      Side         : in Order_Side;
      Order_Price  : in U256;
      Order_Amount : in U256;
      Order_Out    : out Order_ID;
      Success      : out Boolean
   ) is
      New_Order : Order_Record;
      ID        : Order_ID;
   begin
      --  Check if we have space
      if Order_Counter >= Max_Orders then
         Order_Out := 0;
         Success   := False;
         return;
      end if;
      
      --  Create new order
      ID := Order_ID (Order_Counter);
      New_Order := (
         ID      => ID,
         Trader  => Trader,
         Side    => Side,
         Price   => Order_Price,
         Amount  => Order_Amount,
         Filled  => U256_Zero,
         Status  => Open,
         Valid   => True
      );
      
      --  Add to order book
      Order_Book (ID) := New_Order;
      Order_Counter   := Order_Counter + 1;
      
      Order_Out := ID;
      Success   := True;
   end Place_Limit_Order;
   
   --  Execute market order
   procedure Execute_Market_Order (
      Trader        : in Contract_Address;
      Side          : in Order_Side;
      Order_Amount  : in U256;
      Filled_Amount : out U256;
      Success       : out Boolean
   ) is
      Remaining     : U256 := Order_Amount;
      Matched       : U256;
      Fee           : U256;
      Opposite_Side : constant Order_Side :=
        (if Side = Buy then Sell else Buy);
   begin
      pragma Unreferenced (Trader);

      Filled_Amount := U256_Zero;
      
      --  Try to match with existing orders
      for I in Order_Book'Range loop
         exit when Remaining = U256_Zero;
         
         if Order_Book (I).Valid and then
            Order_Book (I).Status = Open and then
            Order_Book (I).Side   = Opposite_Side
         then
            --  Calculate how much we can fill
            declare
               Available : constant U256 :=
                 Sub_Mod (Order_Book (I).Amount, Order_Book (I).Filled);
            begin
               if Less_Than (Remaining, Available) then
                  Matched := Remaining;
               else
                  Matched := Available;
               end if;
               
               --  Update order
               Order_Book (I).Filled :=
                 Add_Mod (Order_Book (I).Filled, Matched);
               if Order_Book (I).Filled = Order_Book (I).Amount then
                  Order_Book (I).Status := Filled;
               end if;
               
               --  Calculate and collect fee
               Fee := Calculate_Fee (Matched);
               Collected_Fees := Add_Mod (Collected_Fees, Fee);
               
               --  Update volume
               Total_Volume := Add_Mod (Total_Volume, Matched);
               
               --  Update remaining
               Remaining     := Sub_Mod (Remaining, Matched);
               Filled_Amount := Add_Mod (Filled_Amount, Matched);
            end;
         end if;
      end loop;
      
      Success := (Filled_Amount /= U256_Zero);
   end Execute_Market_Order;
   
   --  Cancel order
   procedure Cancel_Order (
      Trader  : in Contract_Address;
      Order   : in Order_ID;
      Success : out Boolean
   ) is
   begin
      --  Check order exists and belongs to trader
      if not Order_Book (Order).Valid or else
         Order_Book (Order).Trader /= Trader or else
         Order_Book (Order).Status /= Open
      then
         Success := False;
         return;
      end if;
      
      --  Cancel the order
      Order_Book (Order).Status := Cancelled;
      Success := True;
   end Cancel_Order;
   
   --  Get balance
   function Get_Balance (
      Trader : Contract_Address;
      Token  : Contract_Address
   ) return U256 is
      Idx : constant Balance_Index := Get_Balance_Index (Trader, Token);
   begin
      return Balances (Idx);
   end Get_Balance;
   
   --  Get order
   function Get_Order (
      Order : Order_ID
   ) return Order_Record is
   begin
      if Order_Book (Order).Valid then
         return Order_Book (Order);
      else
         return (
            ID      => 0,
            Trader  => Address_Zero,
            Side    => Buy,
            Price   => U256_Zero,
            Amount  => U256_Zero,
            Filled  => U256_Zero,
            Status  => Open,
            Valid   => False
         );
      end if;
   end Get_Order;
   
   --  Get order count
   function Get_Order_Count return Natural is
   begin
      return Order_Counter;
   end Get_Order_Count;
   
   --  Get total volume
   function Get_Total_Volume return U256 is
   begin
      return Total_Volume;
   end Get_Total_Volume;
   
   --  Get collected fees
   function Get_Collected_Fees return U256 is
   begin
      return Collected_Fees;
   end Get_Collected_Fees;

end DEX_Trading;
