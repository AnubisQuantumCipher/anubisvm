--  KHEPRI Staking Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_Staking with
   SPARK_Mode => On,
   Refined_State => (Staking_State =>
      (Config_Store, State_Store, Stakes, Validators, Admin_Addr,
       Block_Counter, Paused))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Stake_Array is array (0 .. Max_Stakers - 1) of Stake_Info;
   type Validator_Array is array (0 .. Max_Validators - 1) of Validator_Info;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Config_Store : Pool_Config := Default_Pool_Config;
   Admin_Addr   : Address := Zero_Address;
   Block_Counter: Block_Number := 0;
   Paused       : Boolean := False;

   State_Store : Pool_State := (
      Total_Staked         => U256_Zero,
      Acc_Reward_Per_Share => U256_Zero,
      Last_Reward_Block    => 0,
      Total_Rewards        => U256_Zero,
      Distributed          => U256_Zero
   );

   Stakes : Stake_Array := (others => (
      Account        => Zero_Address,
      Amount         => U256_Zero,
      Reward_Debt    => U256_Zero,
      Pending_Reward => U256_Zero,
      Stake_Time     => 0,
      Lock_End       => 0,
      Used           => False
   ));

   Validators : Validator_Array := (others => (
      Operator       => Zero_Address,
      Stake          => U256_Zero,
      Commission_BPS => 0,
      Status         => Status_Inactive,
      Jailed_Until   => 0,
      Used           => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Stake (Account : Address) return Natural is
   begin
      for I in Stakes'Range loop
         if Stakes (I).Used and then Stakes (I).Account = Account then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Stake;

   function Find_Or_Create_Stake (Account : Address) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Stakes'Range loop
         if Stakes (I).Used then
            if Stakes (I).Account = Account then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Stake;

   function Find_Validator (Operator : Address) return Natural is
   begin
      for I in Validators'Range loop
         if Validators (I).Used and then Validators (I).Operator = Operator then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Validator;

   function Find_Or_Create_Validator (Operator : Address) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Validators'Range loop
         if Validators (I).Used then
            if Validators (I).Operator = Operator then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Validator;

   procedure Update_Pool is
      Blocks_Elapsed  : Word64;
      Reward          : U256;
      New_Acc         : U256;
      Overflow        : Boolean;
   begin
      if Block_Counter <= State_Store.Last_Reward_Block then
         return;
      end if;

      if Equal (State_Store.Total_Staked, U256_Zero) then
         State_Store.Last_Reward_Block := Block_Counter;
         return;
      end if;

      Blocks_Elapsed := Block_Counter - State_Store.Last_Reward_Block;

      --  reward = blocks * reward_rate
      Reward := Mul_Mod (From_Word64 (Blocks_Elapsed), Config_Store.Reward_Rate);

      --  acc += (reward * precision) / total_staked
      declare
         Scaled : U256;
         Quotient, Remainder : U256;
      begin
         Scaled := Mul_Mod (Reward, From_Word64 (Word64 (Precision_Factor)));
         Div_Mod (Scaled, State_Store.Total_Staked, Quotient, Remainder);
         Add (State_Store.Acc_Reward_Per_Share, Quotient, New_Acc, Overflow);
         if not Overflow then
            State_Store.Acc_Reward_Per_Share := New_Acc;
         end if;
      end;

      State_Store.Last_Reward_Block := Block_Counter;
   end Update_Pool;

   function Calculate_Pending (Slot : Natural) return Reward_Amount is
      Acc_Reward          : U256;
      Pending             : U256;
      Overflow            : Boolean;
      Quotient, Remainder : U256;
   begin
      if Slot >= Max_Stakers or else not Stakes (Slot).Used then
         return U256_Zero;
      end if;

      --  pending = (amount * acc_per_share / precision) - reward_debt + pending_reward
      Acc_Reward := Mul_Mod (Stakes (Slot).Amount, State_Store.Acc_Reward_Per_Share);
      Div_Mod (Acc_Reward, From_Word64 (Word64 (Precision_Factor)), Quotient, Remainder);

      if Less_Than (Quotient, Stakes (Slot).Reward_Debt) then
         return Stakes (Slot).Pending_Reward;
      end if;

      Pending := Sub_Mod (Quotient, Stakes (Slot).Reward_Debt);
      Add (Pending, Stakes (Slot).Pending_Reward, Acc_Reward, Overflow);
      if Overflow then
         return U256_Max;
      end if;

      return Acc_Reward;
   end Calculate_Pending;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Pool_Config;
      Admin   : in Address;
      Success : out Boolean
   ) is
   begin
      if Admin_Addr /= Zero_Address then
         Success := False;
         return;
      end if;

      Config_Store := Config;
      Admin_Addr := Admin;
      State_Store.Last_Reward_Block := Config.Start_Block;
      Paused := False;
      Success := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Get_Pool_Config return Pool_Config is
   begin
      return Config_Store;
   end Get_Pool_Config;

   function Get_Pool_State return Pool_State is
   begin
      return State_Store;
   end Get_Pool_State;

   function Get_Stake_Info (Account : Address) return Stake_Info is
      Slot : constant Natural := Find_Stake (Account);
   begin
      if Slot /= Natural'Last and Slot < Max_Stakers then
         return Stakes (Slot);
      else
         return (
            Account        => Zero_Address,
            Amount         => U256_Zero,
            Reward_Debt    => U256_Zero,
            Pending_Reward => U256_Zero,
            Stake_Time     => 0,
            Lock_End       => 0,
            Used           => False
         );
      end if;
   end Get_Stake_Info;

   function Staked_Balance (Account : Address) return Stake_Amount is
      Slot : constant Natural := Find_Stake (Account);
   begin
      if Slot /= Natural'Last and Slot < Max_Stakers then
         return Stakes (Slot).Amount;
      else
         return U256_Zero;
      end if;
   end Staked_Balance;

   function Pending_Rewards (Account : Address) return Reward_Amount is
      Slot : constant Natural := Find_Stake (Account);
   begin
      if Slot /= Natural'Last then
         return Calculate_Pending (Slot);
      else
         return U256_Zero;
      end if;
   end Pending_Rewards;

   function Total_Staked return Stake_Amount is
   begin
      return State_Store.Total_Staked;
   end Total_Staked;

   function Is_Locked (Account : Address) return Boolean is
      Slot : constant Natural := Find_Stake (Account);
   begin
      if Slot /= Natural'Last and Slot < Max_Stakers then
         return Block_Counter < Stakes (Slot).Lock_End;
      else
         return False;
      end if;
   end Is_Locked;

   function Current_Block return Block_Number is
   begin
      return Block_Counter;
   end Current_Block;

   function APR return Natural is
   begin
      --  Simplified APR calculation
      --  Real implementation would use actual block times
      if Equal (State_Store.Total_Staked, U256_Zero) then
         return 0;
      else
         return 1000;  --  Placeholder: 10%
      end if;
   end APR;

   ---------------------------------------------------------------------------
   --  Staking Functions
   ---------------------------------------------------------------------------

   procedure Stake (
      Caller  : in     Address;
      Amount  : in     Stake_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) is
      Slot              : Natural;
      New_Amount        : U256;
      New_Total         : U256;
      Overflow          : Boolean;
      Quotient, Remaind : U256;
   begin
      if Paused then
         Success := False;
         Error := Error_Pool_Not_Active;
         return;
      end if;

      if Less_Than (Amount, Config_Store.Min_Stake) then
         Success := False;
         Error := Error_Below_Minimum;
         return;
      end if;

      Update_Pool;

      Slot := Find_Or_Create_Stake (Caller);
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      --  If already staked, claim pending first
      if Stakes (Slot).Used then
         Stakes (Slot).Pending_Reward := Calculate_Pending (Slot);
      end if;

      --  Update stake
      Add (Stakes (Slot).Amount, Amount, New_Amount, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Add (State_Store.Total_Staked, Amount, New_Total, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Stakes (Slot).Account := Caller;
      Stakes (Slot).Amount := New_Amount;
      Stakes (Slot).Stake_Time := Block_Counter;
      Stakes (Slot).Lock_End := Block_Counter + Config_Store.Lock_Period;
      Stakes (Slot).Used := True;

      --  Update reward debt
      declare
         Scaled : U256;
      begin
         Scaled := Mul_Mod (New_Amount, State_Store.Acc_Reward_Per_Share);
         Div_Mod (Scaled, From_Word64 (Word64 (Precision_Factor)), Quotient, Remaind);
         Stakes (Slot).Reward_Debt := Quotient;
      end;

      State_Store.Total_Staked := New_Total;

      Success := True;
      Error := Error_None;
   end Stake;

   procedure Unstake (
      Caller  : in     Address;
      Amount  : in     Stake_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) is
      Slot              : constant Natural := Find_Stake (Caller);
      Pending           : Reward_Amount;
      Quotient, Remaind : U256;
   begin
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Not_Staked;
         return;
      end if;

      if Less_Than (Stakes (Slot).Amount, Amount) then
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      if Block_Counter < Stakes (Slot).Lock_End then
         Success := False;
         Error := Error_Lock_Not_Expired;
         return;
      end if;

      Update_Pool;

      --  Calculate and store pending rewards
      Pending := Calculate_Pending (Slot);
      Stakes (Slot).Pending_Reward := Pending;

      --  Update stake
      Stakes (Slot).Amount := Sub_Mod (Stakes (Slot).Amount, Amount);
      State_Store.Total_Staked := Sub_Mod (State_Store.Total_Staked, Amount);

      --  Update reward debt
      declare
         Scaled : U256;
      begin
         Scaled := Mul_Mod (Stakes (Slot).Amount, State_Store.Acc_Reward_Per_Share);
         Div_Mod (Scaled, From_Word64 (Word64 (Precision_Factor)), Quotient, Remaind);
         Stakes (Slot).Reward_Debt := Quotient;
      end;

      Success := True;
      Error := Error_None;
   end Unstake;

   procedure Claim_Rewards (
      Caller  : in     Address;
      Claimed : out    Reward_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) is
      Slot              : constant Natural := Find_Stake (Caller);
      Quotient, Remaind : U256;
   begin
      if Slot = Natural'Last then
         Claimed := U256_Zero;
         Success := False;
         Error := Error_Not_Staked;
         return;
      end if;

      Update_Pool;

      Claimed := Calculate_Pending (Slot);
      Stakes (Slot).Pending_Reward := U256_Zero;

      --  Update reward debt
      declare
         Scaled : U256;
      begin
         Scaled := Mul_Mod (Stakes (Slot).Amount, State_Store.Acc_Reward_Per_Share);
         Div_Mod (Scaled, From_Word64 (Word64 (Precision_Factor)), Quotient, Remaind);
         Stakes (Slot).Reward_Debt := Quotient;
      end;

      --  Update distributed
      declare
         New_Dist : U256;
         Overflow : Boolean;
      begin
         Add (State_Store.Distributed, Claimed, New_Dist, Overflow);
         if not Overflow then
            State_Store.Distributed := New_Dist;
         end if;
      end;

      Success := True;
      Error := Error_None;
   end Claim_Rewards;

   procedure Compound (
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) is
      Pending : Reward_Amount;
      Claimed : Reward_Amount;
   begin
      --  Claim rewards
      Claim_Rewards (Caller, Claimed, Success, Error);
      if not Success then
         return;
      end if;

      Pending := Claimed;
      if Equal (Pending, U256_Zero) then
         Success := True;
         Error := Error_None;
         return;
      end if;

      --  Stake the rewards (assuming same token)
      Stake (Caller, Pending, Success, Error);
   end Compound;

   procedure Emergency_Withdraw (
      Caller   : in     Address;
      Received : out    Stake_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) is
      Slot : constant Natural := Find_Stake (Caller);
   begin
      if Slot = Natural'Last then
         Received := U256_Zero;
         Success := False;
         Error := Error_Not_Staked;
         return;
      end if;

      Received := Stakes (Slot).Amount;

      --  Clear stake (forfeit rewards)
      State_Store.Total_Staked := Sub_Mod (State_Store.Total_Staked, Received);
      Stakes (Slot).Amount := U256_Zero;
      Stakes (Slot).Pending_Reward := U256_Zero;
      Stakes (Slot).Reward_Debt := U256_Zero;

      Success := True;
      Error := Error_None;
   end Emergency_Withdraw;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   procedure Set_Reward_Rate (
      Caller   : in     Address;
      New_Rate : in     Reward_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Update_Pool;
      Config_Store.Reward_Rate := New_Rate;
      Success := True;
      Error := Error_None;
   end Set_Reward_Rate;

   procedure Add_Rewards (
      Caller  : in     Address;
      Amount  : in     Reward_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) is
      New_Total : U256;
      Overflow  : Boolean;
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Add (State_Store.Total_Rewards, Amount, New_Total, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      State_Store.Total_Rewards := New_Total;
      Success := True;
      Error := Error_None;
   end Add_Rewards;

   procedure Set_Lock_Period (
      Caller     : in     Address;
      New_Period : in     Block_Number;
      Success    : out    Boolean;
      Error      : out    Staking_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Config_Store.Lock_Period := New_Period;
      Success := True;
      Error := Error_None;
   end Set_Lock_Period;

   procedure Set_Penalty (
      Caller      : in     Address;
      New_Penalty : in     Natural;
      Success     : out    Boolean;
      Error       : out    Staking_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Config_Store.Penalty_BPS := New_Penalty;
      Success := True;
      Error := Error_None;
   end Set_Penalty;

   procedure Pause_Pool (
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Update_Pool;
      Paused := True;
      Success := True;
      Error := Error_None;
   end Pause_Pool;

   procedure Resume_Pool (
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Paused := False;
      State_Store.Last_Reward_Block := Block_Counter;
      Success := True;
      Error := Error_None;
   end Resume_Pool;

   ---------------------------------------------------------------------------
   --  Validator Functions
   ---------------------------------------------------------------------------

   function Get_Validator (Operator : Address) return Validator_Info is
      Slot : constant Natural := Find_Validator (Operator);
   begin
      if Slot /= Natural'Last and Slot < Max_Validators then
         return Validators (Slot);
      else
         return (
            Operator       => Zero_Address,
            Stake          => U256_Zero,
            Commission_BPS => 0,
            Status         => Status_Inactive,
            Jailed_Until   => 0,
            Used           => False
         );
      end if;
   end Get_Validator;

   function Validator_Count return Natural is
      Count : Natural := 0;
   begin
      for I in Validators'Range loop
         if Validators (I).Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Validator_Count;

   procedure Register_Validator (
      Caller     : in     Address;
      Commission : in     Natural;
      Success    : out    Boolean;
      Error      : out    Staking_Error
   ) is
      Slot : constant Natural := Find_Or_Create_Validator (Caller);
   begin
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      if Commission > 10000 then
         Success := False;
         Error := Error_Invalid_Commission;
         return;
      end if;

      Validators (Slot) := (
         Operator       => Caller,
         Stake          => U256_Zero,
         Commission_BPS => Commission,
         Status         => Status_Active,
         Jailed_Until   => 0,
         Used           => True
      );

      Success := True;
      Error := Error_None;
   end Register_Validator;

   procedure Delegate (
      Caller   : in     Address;
      Operator : in     Address;
      Amount   : in     Stake_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) is
      pragma Unreferenced (Caller);
      Slot       : constant Natural := Find_Validator (Operator);
      New_Stake  : U256;
      Overflow   : Boolean;
   begin
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Not_Staked;
         return;
      end if;

      if Validators (Slot).Status = Status_Jailed then
         Success := False;
         Error := Error_Validator_Jailed;
         return;
      end if;

      Add (Validators (Slot).Stake, Amount, New_Stake, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Validators (Slot).Stake := New_Stake;
      Success := True;
      Error := Error_None;
   end Delegate;

   procedure Undelegate (
      Caller   : in     Address;
      Operator : in     Address;
      Amount   : in     Stake_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) is
      pragma Unreferenced (Caller);
      Slot : constant Natural := Find_Validator (Operator);
   begin
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Not_Staked;
         return;
      end if;

      if Less_Than (Validators (Slot).Stake, Amount) then
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      Validators (Slot).Stake := Sub_Mod (Validators (Slot).Stake, Amount);
      Success := True;
      Error := Error_None;
   end Undelegate;

   procedure Slash (
      Caller     : in     Address;
      Operator   : in     Address;
      Slash_BPS  : in     Natural;
      Jail_Until : in     Block_Number;
      Success    : out    Boolean;
      Error      : out    Staking_Error
   ) is
      Slot       : constant Natural := Find_Validator (Operator);
      Slash_Amt  : U256;
      Quotient   : U256;
      Remainder  : U256;
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      if Slot = Natural'Last then
         Success := False;
         Error := Error_Not_Staked;
         return;
      end if;

      --  Calculate slash amount
      Slash_Amt := Mul_Mod (Validators (Slot).Stake, From_Word64 (Word64 (Slash_BPS)));
      Div_Mod (Slash_Amt, From_Word64 (10000), Quotient, Remainder);

      --  Apply slash
      Validators (Slot).Stake := Sub_Mod (Validators (Slot).Stake, Quotient);
      Validators (Slot).Status := Status_Jailed;
      Validators (Slot).Jailed_Until := Jail_Until;

      Success := True;
      Error := Error_None;
   end Slash;

end Khepri_Staking;
