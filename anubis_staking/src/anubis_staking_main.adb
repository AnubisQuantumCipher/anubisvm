pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Staking; use Anubis_Staking;

procedure Anubis_Staking_Main is
   State : Contract_State := Empty_State;
   Rewards : Unsigned_64;
   Tests_Passed : Natural := 0;
   Tests_Total : Natural := 0;

   procedure Check (Name : String; Condition : Boolean) is
   begin
      Tests_Total := Tests_Total + 1;
      if Condition then
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Check;

begin
   Put_Line ("AnubisVM Staking Contract Test");
   Put_Line ("==============================");
   Put_Line ("");

   -- Test 1: Initialize
   Put_Line ("--- Initialization ---");
   Initialize (State);
   Check ("Contract initialized", State.Initialized);
   Check ("Total staked is zero", Get_Total_Staked (State) = 0);
   Check ("User not staking", not Is_Staking (State));

   -- Test 2: Stake tokens
   Put_Line ("");
   Put_Line ("--- Staking 100,000 tokens at block 1000 ---");
   Stake (State, 100_000, 1000);
   Check ("User is now staking", Is_Staking (State));
   Check ("User stake is 100,000", Get_User_Stake (State) = 100_000);
   Check ("Total staked is 100,000", Get_Total_Staked (State) = 100_000);

   -- Test 3: Calculate rewards after 10,000 blocks
   Put_Line ("");
   Put_Line ("--- Rewards after 10,000 blocks ---");
   Rewards := Calculate_Rewards (State.User_Stake, 11_000);
   Put_Line ("  Rewards earned:" & Unsigned_64'Image (Rewards));
   Check ("Rewards calculated correctly", Rewards = 500);

   -- Test 4: Unstake
   Put_Line ("");
   Put_Line ("--- Unstaking ---");
   Unstake (State);
   Check ("User no longer staking", not Is_Staking (State));
   Check ("User stake is zero", Get_User_Stake (State) = 0);
   Check ("Total staked is zero", Get_Total_Staked (State) = 0);

   -- Test 5: Re-stake with different amount
   Put_Line ("");
   Put_Line ("--- Re-staking 500,000 tokens ---");
   Stake (State, 500_000, 20_000);
   Check ("User staking again", Is_Staking (State));
   Check ("New stake amount correct", Get_User_Stake (State) = 500_000);

   -- Test 6: Rewards on larger stake
   Put_Line ("");
   Put_Line ("--- Rewards on 500k stake after 50,000 blocks ---");
   Rewards := Calculate_Rewards (State.User_Stake, 70_000);
   Put_Line ("  Rewards earned:" & Unsigned_64'Image (Rewards));
   Check ("Large stake rewards correct", Rewards = 12_500);

   -- Summary
   Put_Line ("");
   Put_Line ("==============================");
   Put_Line ("Tests passed:" & Natural'Image (Tests_Passed) &
             " /" & Natural'Image (Tests_Total));

   if Tests_Passed = Tests_Total then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;

end Anubis_Staking_Main;
