pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Multisig_Wallet; use Multisig_Wallet;

procedure Multisig_Wallet_Main is
   State : Wallet_State := Empty_Wallet;
   Pass : Natural := 0;
   Total : Natural := 0;

   procedure Test (Name : String; Ok : Boolean) is
   begin
      Total := Total + 1;
      if Ok then
         Pass := Pass + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Test;

begin
   Put_Line ("Multi-Sig Wallet Contract Test");
   Put_Line ("==============================");
   Put_Line ("");

   Put_Line ("--- Setup: 3-of-5 Wallet ---");
   Initialize (State, Owners => 5, Required => 3);
   Test ("Wallet initialized", State.Initialized);
   Test ("Balance is 0", Get_Balance (State) = 0);
   Test ("Required sigs is 3", State.Required_Sigs = 3);
   Test ("Owner count is 5", State.Owner_Count = 5);

   Put_Line ("");
   Put_Line ("--- Deposit 1,000,000 tokens ---");
   Deposit (State, 1_000_000);
   Test ("Balance is 1,000,000", Get_Balance (State) = 1_000_000);

   Put_Line ("");
   Put_Line ("--- Propose withdrawal of 250,000 ---");
   Propose_Withdrawal (State, 250_000);
   Test ("Pending is 250,000", Get_Pending (State) = 250_000);
   Test ("Sig count is 0", Get_Sig_Count (State) = 0);
   Test ("Not ready yet", not Is_Ready (State));

   Put_Line ("");
   Put_Line ("--- Owner 1 signs ---");
   Sign_Withdrawal (State, 1);
   Test ("Sig count is 1", Get_Sig_Count (State) = 1);
   Test ("Still not ready", not Is_Ready (State));

   Put_Line ("");
   Put_Line ("--- Owner 3 signs ---");
   Sign_Withdrawal (State, 3);
   Test ("Sig count is 2", Get_Sig_Count (State) = 2);
   Test ("Still not ready", not Is_Ready (State));

   Put_Line ("");
   Put_Line ("--- Owner 5 signs ---");
   Sign_Withdrawal (State, 5);
   Test ("Sig count is 3", Get_Sig_Count (State) = 3);
   Test ("Now ready (3 of 3)", Is_Ready (State));

   Put_Line ("");
   Put_Line ("--- Execute withdrawal ---");
   Execute_Withdrawal (State);
   Test ("Balance is 750,000", Get_Balance (State) = 750_000);
   Test ("Pending cleared", Get_Pending (State) = 0);
   Test ("Sigs cleared", Get_Sig_Count (State) = 0);

   Put_Line ("");
   Put_Line ("--- New proposal: 100,000 ---");
   Propose_Withdrawal (State, 100_000);
   Sign_Withdrawal (State, 2);
   Test ("1 signature", Get_Sig_Count (State) = 1);

   Put_Line ("");
   Put_Line ("--- Cancel withdrawal ---");
   Cancel_Withdrawal (State);
   Test ("Pending cleared", Get_Pending (State) = 0);
   Test ("Balance unchanged", Get_Balance (State) = 750_000);

   Put_Line ("");
   Put_Line ("==============================");
   Put_Line ("Passed:" & Natural'Image (Pass) & " /" & Natural'Image (Total));
   if Pass = Total then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;
end Multisig_Wallet_Main;
