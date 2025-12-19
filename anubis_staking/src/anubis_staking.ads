--  Anubis Staking Contract
--  Post-Quantum Secure Staking with Rewards
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package Anubis_Staking with SPARK_Mode => On is

   Contract_Name : constant String := "AnubisStaking";

   --  Staking parameters
   Min_Stake : constant Unsigned_64 := 1_000;
   Max_Stake : constant Unsigned_64 := 1_000_000_000;
   Reward_Rate : constant Unsigned_64 := 5;  -- 5% APY

   type Stake_Record is record
      Amount : Unsigned_64;
      Start_Block : Unsigned_64;
      Rewards_Claimed : Unsigned_64;
      Active : Boolean;
   end record;

   Empty_Stake : constant Stake_Record := (0, 0, 0, False);

   type Contract_State is record
      Initialized : Boolean;
      Total_Staked : Unsigned_64;
      Total_Rewards : Unsigned_64;
      Stake_Count : Unsigned_64;
      Current_Block : Unsigned_64;
      User_Stake : Stake_Record;
   end record;

   Empty_State : constant Contract_State := (
      False, 0, 0, 0, 0, Empty_Stake);

   procedure Initialize (State : in out Contract_State)
      with Global => null,
           Pre  => not State.Initialized,
           Post => State.Initialized and State.Total_Staked = 0;

   procedure Stake (
      State : in Out Contract_State;
      Amount : Unsigned_64;
      Block : Unsigned_64)
      with Global => null,
           Pre  => State.Initialized
                   and not State.User_Stake.Active
                   and Amount >= Min_Stake
                   and Amount <= Max_Stake
                   and State.Total_Staked <= Unsigned_64'Last - Amount,
           Post => State.User_Stake.Active
                   and State.User_Stake.Amount = Amount
                   and State.Total_Staked = State.Total_Staked'Old + Amount;

   procedure Unstake (State : in Out Contract_State)
      with Global => null,
           Pre  => State.Initialized and State.User_Stake.Active,
           Post => not State.User_Stake.Active
                   and State.User_Stake.Amount = 0;

   function Calculate_Rewards (
      Stake : Stake_Record;
      Current_Block : Unsigned_64) return Unsigned_64
      with Global => null,
           Pre => Stake.Active and Current_Block >= Stake.Start_Block;

   function Get_Total_Staked (State : Contract_State) return Unsigned_64
      with Global => null;

   function Get_User_Stake (State : Contract_State) return Unsigned_64
      with Global => null;

   function Is_Staking (State : Contract_State) return Boolean
      with Global => null;

end Anubis_Staking;
