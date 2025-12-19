pragma SPARK_Mode (On);

package body Anubis_Staking with SPARK_Mode => On is

   procedure Initialize (State : in Out Contract_State) is
   begin
      State := (
         Initialized => True,
         Total_Staked => 0,
         Total_Rewards => 0,
         Stake_Count => 0,
         Current_Block => 0,
         User_Stake => Empty_Stake);
   end Initialize;

   procedure Stake (
      State : in Out Contract_State;
      Amount : Unsigned_64;
      Block : Unsigned_64) is
   begin
      State.User_Stake := (
         Amount => Amount,
         Start_Block => Block,
         Rewards_Claimed => 0,
         Active => True);
      State.Total_Staked := State.Total_Staked + Amount;
      State.Stake_Count := State.Stake_Count + 1;
      State.Current_Block := Block;
   end Stake;

   procedure Unstake (State : in Out Contract_State) is
   begin
      State.Total_Staked := State.Total_Staked - State.User_Stake.Amount;
      State.User_Stake := Empty_Stake;
   end Unstake;

   function Calculate_Rewards (
      Stake : Stake_Record;
      Current_Block : Unsigned_64) return Unsigned_64 is
      Blocks_Staked : Unsigned_64;
   begin
      Blocks_Staked := Current_Block - Stake.Start_Block;
      return (Stake.Amount * Reward_Rate * Blocks_Staked) / 100_000;
   end Calculate_Rewards;

   function Get_Total_Staked (State : Contract_State) return Unsigned_64 is
   begin
      return State.Total_Staked;
   end Get_Total_Staked;

   function Get_User_Stake (State : Contract_State) return Unsigned_64 is
   begin
      return State.User_Stake.Amount;
   end Get_User_Stake;

   function Is_Staking (State : Contract_State) return Boolean is
   begin
      return State.User_Stake.Active;
   end Is_Staking;

end Anubis_Staking;
