--  KHEPRI Flash Loan Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_FlashLoan with
   SPARK_Mode => On,
   Refined_State => (FlashLoan_State => (
      Tokens, Token_Count, Owner_Addr, Paused_Flag, Active_Loan, Fees_Collected))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Token_Entry is record
      Config : Token_Config;
      Used   : Boolean;
   end record;

   type Token_Array is array (0 .. Max_Tokens - 1) of Token_Entry;
   type Fees_Array is array (0 .. Max_Tokens - 1) of U256;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Tokens         : Token_Array := (others => (
      Config => (
         Token     => Null_Address,
         Fee       => Default_Fee,
         Available => U256_Zero,
         Borrowed  => U256_Zero,
         Enabled   => False
      ),
      Used => False
   ));

   Token_Count    : Natural := 0;
   Owner_Addr     : Address := Null_Address;
   Paused_Flag    : Boolean := False;
   Active_Loan    : Boolean := False;
   Fees_Collected : Fees_Array := (others => U256_Zero);

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Token (Token : Address) return Natural is
   begin
      for I in Tokens'Range loop
         if Tokens (I).Used and then Tokens (I).Config.Token = Token then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Token;

   function Find_Empty_Slot return Natural is
   begin
      for I in Tokens'Range loop
         if not Tokens (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Slot;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (Owner : in Address) is
   begin
      Owner_Addr := Owner;
      Paused_Flag := False;
      Active_Loan := False;
      Token_Count := 0;
      Tokens := (others => (
         Config => (
            Token     => Null_Address,
            Fee       => Default_Fee,
            Available => U256_Zero,
            Borrowed  => U256_Zero,
            Enabled   => False
         ),
         Used => False
      ));
      Fees_Collected := (others => U256_Zero);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Flash Loan Operations
   ---------------------------------------------------------------------------

   procedure Flash_Loan (
      Token    : in     Address;
      Amount   : in     U256;
      Receiver : in     Address;
      Callback : in     Hash256;
      Data     : in     Hash256;
      Success  : out    Boolean;
      Error    : out    FlashLoan_Error
   ) is
      Idx           : Natural;
      Fee           : U256;
      Repayment     : U256;
      Balance_Before : U256;
      Balance_After : U256;
      Temp          : U256;
      Oflow         : Boolean;
   begin
      Success := False;
      Error := Error_None;

      --  Check reentrancy
      if Active_Loan then
         Error := Error_Reentrancy;
         return;
      end if;

      --  Check paused
      if Paused_Flag then
         Error := Error_Disabled;
         return;
      end if;

      --  Find token
      Idx := Find_Token (Token);
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;
         return;
      end if;

      --  Check enabled
      if not Tokens (Idx).Config.Enabled then
         Error := Error_Disabled;
         return;
      end if;

      --  Check liquidity
      if Less_Than (Tokens (Idx).Config.Available, Amount) then
         Error := Error_Insufficient_Liquidity;
         return;
      end if;

      --  Calculate fee and repayment
      Fee := Calculate_Fee (Amount, Tokens (Idx).Config.Fee);
      Repayment := Get_Repayment_Amount (Amount, Tokens (Idx).Config.Fee);

      --  Record balance before
      Balance_Before := Tokens (Idx).Config.Available;

      --  Mark loan active (reentrancy guard)
      Active_Loan := True;

      --  Update borrowed amount
      Add (Tokens (Idx).Config.Borrowed, Amount, Temp, Oflow);
      if Oflow then
         Active_Loan := False;
         Error := Error_Insufficient_Liquidity;
         return;
      end if;
      Tokens (Idx).Config.Borrowed := Temp;

      --  Decrease available (tokens sent to receiver)
      Sub (Tokens (Idx).Config.Available, Amount, Temp, Oflow);
      Tokens (Idx).Config.Available := Temp;

      --  Here we would call the receiver's callback
      --  The callback should:
      --  1. Use the borrowed tokens
      --  2. Ensure repayment amount is available
      --  3. Approve the flash loan contract to pull repayment
      --
      --  For now, we simulate successful callback

      --  Verify repayment (check balance increased by at least repayment amount)
      --  In real implementation, would pull tokens from receiver
      Add (Balance_Before, Fee, Balance_After, Oflow);
      if Oflow then
         --  Rollback
         Tokens (Idx).Config.Available := Balance_Before;
         Tokens (Idx).Config.Borrowed := U256_Zero;
         Active_Loan := False;
         Error := Error_Repayment_Failed;
         return;
      end if;

      --  Simulate successful repayment: restore principal + add fee
      Tokens (Idx).Config.Available := Balance_After;

      --  Clear borrowed
      Sub (Tokens (Idx).Config.Borrowed, Amount, Temp, Oflow);
      Tokens (Idx).Config.Borrowed := Temp;

      --  Accumulate fees
      Add (Fees_Collected (Idx), Fee, Temp, Oflow);
      if not Oflow then
         Fees_Collected (Idx) := Temp;
      end if;

      --  Clear reentrancy guard
      Active_Loan := False;

      Success := True;
   end Flash_Loan;

   procedure Flash_Loan_Multi (
      Loans     : in     Loan_Request_Array;
      Count     : in     Natural;
      Receiver  : in     Address;
      Callback  : in     Hash256;
      Data      : in     Hash256;
      Success   : out    Boolean;
      Error     : out    FlashLoan_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      --  Check reentrancy
      if Active_Loan then
         Error := Error_Reentrancy;
         return;
      end if;

      --  Check paused
      if Paused_Flag then
         Error := Error_Disabled;
         return;
      end if;

      if Count = 0 then
         Error := Error_Zero_Amount;
         return;
      end if;

      --  Mark loan active
      Active_Loan := True;

      --  Process each loan
      for I in 0 .. Count - 1 loop
         declare
            Loan_Error : FlashLoan_Error;
            Loan_Success : Boolean;
         begin
            Flash_Loan (
               Token    => Loans (Loan_Index (I)).Token,
               Amount   => Loans (Loan_Index (I)).Amount,
               Receiver => Receiver,
               Callback => Callback,
               Data     => Data,
               Success  => Loan_Success,
               Error    => Loan_Error
            );

            if not Loan_Success then
               Active_Loan := False;
               Error := Loan_Error;
               return;
            end if;
         end;
      end loop;

      Active_Loan := False;
      Success := True;
   end Flash_Loan_Multi;

   ---------------------------------------------------------------------------
   --  Liquidity Management
   ---------------------------------------------------------------------------

   procedure Deposit (
      Token   : in     Address;
      Amount  : in     U256;
      From    : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) is
      Idx   : Natural;
      Temp  : U256;
      Oflow : Boolean;
   begin
      Success := False;
      Error := Error_None;

      Idx := Find_Token (Token);
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;
         return;
      end if;

      Add (Tokens (Idx).Config.Available, Amount, Temp, Oflow);
      if Oflow then
         Error := Error_Insufficient_Liquidity;  --  Overflow
         return;
      end if;

      Tokens (Idx).Config.Available := Temp;
      Success := True;
   end Deposit;

   procedure Withdraw (
      Token   : in     Address;
      Amount  : in     U256;
      To      : in     Address;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) is
      Idx   : Natural;
      Temp  : U256;
      Uflow : Boolean;
   begin
      Success := False;
      Error := Error_None;

      --  Only owner can withdraw
      if Caller /= Owner_Addr then
         Error := Error_Unauthorized;
         return;
      end if;

      Idx := Find_Token (Token);
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;
         return;
      end if;

      --  Check available (excluding borrowed)
      if Less_Than (Tokens (Idx).Config.Available, Amount) then
         Error := Error_Insufficient_Liquidity;
         return;
      end if;

      Sub (Tokens (Idx).Config.Available, Amount, Temp, Uflow);
      Tokens (Idx).Config.Available := Temp;

      Success := True;
   end Withdraw;

   ---------------------------------------------------------------------------
   --  Fee Calculation
   ---------------------------------------------------------------------------

   function Calculate_Fee (
      Amount : U256;
      Fee_BP : Natural
   ) return U256 is
      High : U256;
      Temp : U256;
   begin
      --  Fee = Amount * Fee_BP / 10000
      Mul (Amount, From_Word64 (Word64 (Fee_BP)), High, Temp);
      return Div (Temp, From_Word64 (Word64 (Fee_Denominator)));
   end Calculate_Fee;

   function Get_Repayment_Amount (
      Amount : U256;
      Fee_BP : Natural
   ) return U256 is
      Fee   : U256;
      Temp  : U256;
      Oflow : Boolean;
   begin
      Fee := Calculate_Fee (Amount, Fee_BP);
      Add (Amount, Fee, Temp, Oflow);
      if Oflow then
         return U256_Max;
      end if;
      return Temp;
   end Get_Repayment_Amount;

   ---------------------------------------------------------------------------
   --  Token Management
   ---------------------------------------------------------------------------

   procedure Add_Token (
      Token   : in     Address;
      Fee     : in     Natural;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) is
      Idx : Natural;
   begin
      Success := False;
      Error := Error_None;

      if Caller /= Owner_Addr then
         Error := Error_Unauthorized;
         return;
      end if;

      --  Check if already exists
      if Find_Token (Token) /= Natural'Last then
         Error := Error_Token_Not_Supported;  --  Already exists
         return;
      end if;

      Idx := Find_Empty_Slot;
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;  --  No slots
         return;
      end if;

      Tokens (Idx) := (
         Config => (
            Token     => Token,
            Fee       => Fee,
            Available => U256_Zero,
            Borrowed  => U256_Zero,
            Enabled   => True
         ),
         Used => True
      );

      Token_Count := Token_Count + 1;
      Success := True;
   end Add_Token;

   procedure Remove_Token (
      Token   : in     Address;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) is
      Idx : Natural;
   begin
      Success := False;
      Error := Error_None;

      if Caller /= Owner_Addr then
         Error := Error_Unauthorized;
         return;
      end if;

      Idx := Find_Token (Token);
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;
         return;
      end if;

      --  Can't remove if there's borrowed amount
      if not Equal (Tokens (Idx).Config.Borrowed, U256_Zero) then
         Error := Error_Insufficient_Liquidity;
         return;
      end if;

      Tokens (Idx).Used := False;
      Token_Count := Token_Count - 1;
      Success := True;
   end Remove_Token;

   procedure Set_Token_Fee (
      Token   : in     Address;
      New_Fee : in     Natural;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) is
      Idx : Natural;
   begin
      Success := False;
      Error := Error_None;

      if Caller /= Owner_Addr then
         Error := Error_Unauthorized;
         return;
      end if;

      Idx := Find_Token (Token);
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;
         return;
      end if;

      Tokens (Idx).Config.Fee := New_Fee;
      Success := True;
   end Set_Token_Fee;

   procedure Set_Token_Enabled (
      Token   : in     Address;
      Enabled : in     Boolean;
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) is
      Idx : Natural;
   begin
      Success := False;
      Error := Error_None;

      if Caller /= Owner_Addr then
         Error := Error_Unauthorized;
         return;
      end if;

      Idx := Find_Token (Token);
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;
         return;
      end if;

      Tokens (Idx).Config.Enabled := Enabled;
      Success := True;
   end Set_Token_Enabled;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Is_Token_Supported (Token : Address) return Boolean is
   begin
      return Find_Token (Token) /= Natural'Last;
   end Is_Token_Supported;

   function Get_Available_Liquidity (Token : Address) return U256 is
      Idx : constant Natural := Find_Token (Token);
   begin
      if Idx = Natural'Last then
         return U256_Zero;
      end if;
      return Tokens (Idx).Config.Available;
   end Get_Available_Liquidity;

   function Get_Token_Fee (Token : Address) return Natural is
      Idx : constant Natural := Find_Token (Token);
   begin
      if Idx = Natural'Last then
         return 0;
      end if;
      return Tokens (Idx).Config.Fee;
   end Get_Token_Fee;

   function Is_Flash_Loan_Active return Boolean is
   begin
      return Active_Loan;
   end Is_Flash_Loan_Active;

   function Get_Fees_Collected (Token : Address) return U256 is
      Idx : constant Natural := Find_Token (Token);
   begin
      if Idx = Natural'Last then
         return U256_Zero;
      end if;
      return Fees_Collected (Idx);
   end Get_Fees_Collected;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   procedure Collect_Fees (
      Token   : in     Address;
      To      : in     Address;
      Caller  : in     Address;
      Amount  : out    U256;
      Success : out    Boolean;
      Error   : out    FlashLoan_Error
   ) is
      Idx : Natural;
   begin
      Amount := U256_Zero;
      Success := False;
      Error := Error_None;

      if Caller /= Owner_Addr then
         Error := Error_Unauthorized;
         return;
      end if;

      Idx := Find_Token (Token);
      if Idx = Natural'Last then
         Error := Error_Token_Not_Supported;
         return;
      end if;

      Amount := Fees_Collected (Idx);
      Fees_Collected (Idx) := U256_Zero;

      --  Subtract from available (fees are part of available)
      declare
         Temp  : U256;
         Uflow : Boolean;
      begin
         Sub (Tokens (Idx).Config.Available, Amount, Temp, Uflow);
         if not Uflow then
            Tokens (Idx).Config.Available := Temp;
         end if;
      end;

      Success := True;
   end Collect_Fees;

   procedure Transfer_Ownership (
      New_Owner : in     Address;
      Caller    : in     Address;
      Success   : out    Boolean
   ) is
   begin
      Success := False;

      if Caller /= Owner_Addr then
         return;
      end if;

      Owner_Addr := New_Owner;
      Success := True;
   end Transfer_Ownership;

   procedure Pause (
      Caller  : in     Address;
      Success : out    Boolean
   ) is
   begin
      Success := False;

      if Caller /= Owner_Addr then
         return;
      end if;

      Paused_Flag := True;
      Success := True;
   end Pause;

   procedure Unpause (
      Caller  : in     Address;
      Success : out    Boolean
   ) is
   begin
      Success := False;

      if Caller /= Owner_Addr then
         return;
      end if;

      Paused_Flag := False;
      Success := True;
   end Unpause;

   function Is_Paused return Boolean is
   begin
      return Paused_Flag;
   end Is_Paused;

end Khepri_FlashLoan;
