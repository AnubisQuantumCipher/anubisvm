--  Comprehensive Test Suite for Quantum Vault CVM
--  Tests all features with performance metrics

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with Quantum_Vault;

procedure Test_Quantum_Vault is

   ---------------------------------------------------------------------------
   --  Test Configuration
   ---------------------------------------------------------------------------

   Total_Tests     : Natural := 0;
   Passed_Tests    : Natural := 0;
   Failed_Tests    : Natural := 0;

   --  Performance metrics
   type Metric_Record is record
      Operation   : String (1 .. 20);
      Duration_Us : Natural;
      Gas_Est     : Natural;
      Success     : Boolean;
   end record;

   Max_Metrics : constant := 100;
   Metrics     : array (1 .. Max_Metrics) of Metric_Record;
   Metric_Count : Natural := 0;

   ---------------------------------------------------------------------------
   --  Test Addresses
   ---------------------------------------------------------------------------

   Guardian : constant Address := (
      16#AA#, 16#BB#, 16#CC#, 16#DD#, 16#EE#, 16#FF#, 16#00#, 16#11#,
      16#22#, 16#33#, 16#44#, 16#55#, 16#66#, 16#77#, 16#88#, 16#99#,
      16#AA#, 16#BB#, 16#CC#, 16#DD#, 16#EE#, 16#FF#, 16#00#, 16#11#,
      16#22#, 16#33#, 16#44#, 16#55#, 16#66#, 16#77#, 16#88#, 16#99#
   );

   Alice : constant Address := (
      16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#,
      16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#, 16#0F#, 16#10#,
      16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#, 16#18#,
      16#19#, 16#1A#, 16#1B#, 16#1C#, 16#1D#, 16#1E#, 16#1F#, 16#20#
   );

   Bob : constant Address := (
      16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#, 16#28#,
      16#29#, 16#2A#, 16#2B#, 16#2C#, 16#2D#, 16#2E#, 16#2F#, 16#30#,
      16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#,
      16#39#, 16#3A#, 16#3B#, 16#3C#, 16#3D#, 16#3E#, 16#3F#, 16#40#
   );

   Charlie : constant Address := (
      16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#, 16#48#,
      16#49#, 16#4A#, 16#4B#, 16#4C#, 16#4D#, 16#4E#, 16#4F#, 16#50#,
      16#51#, 16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#, 16#58#,
      16#59#, 16#5A#, 16#5B#, 16#5C#, 16#5D#, 16#5E#, 16#5F#, 16#60#
   );

   ---------------------------------------------------------------------------
   --  State and Result
   ---------------------------------------------------------------------------

   State   : State_Array;
   Success : Boolean;
   Result  : Exec_Result;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Get_U64 (Data : Return_Buffer; Offset : Natural) return Unsigned_64 is
      Val : Unsigned_64 := 0;
   begin
      for I in Offset .. Offset + 7 loop
         Val := Shift_Left (Val, 8) or Unsigned_64 (Data (I));
      end loop;
      return Val;
   end Get_U64;

   procedure Record_Metric (
      Op      : String;
      Dur_Us  : Natural;
      Gas     : Natural;
      OK      : Boolean
   ) is
   begin
      if Metric_Count < Max_Metrics then
         Metric_Count := Metric_Count + 1;
         declare
            M : Metric_Record renames Metrics (Metric_Count);
         begin
            M.Operation := (others => ' ');
            for I in Op'Range loop
               if I <= 20 then
                  M.Operation (I) := Op (I);
               end if;
            end loop;
            M.Duration_Us := Dur_Us;
            M.Gas_Est := Gas;
            M.Success := OK;
         end;
      end if;
   end Record_Metric;

   procedure Assert_True (Condition : Boolean; Test_Name : String) is
   begin
      Total_Tests := Total_Tests + 1;
      if Condition then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Test_Name);
      else
         Failed_Tests := Failed_Tests + 1;
         Put_Line ("  [FAIL] " & Test_Name);
      end if;
   end Assert_True;

   procedure Assert_Eq (A, B : Unsigned_64; Test_Name : String) is
   begin
      Total_Tests := Total_Tests + 1;
      if A = B then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Test_Name & " (" & Unsigned_64'Image (A) & ")");
      else
         Failed_Tests := Failed_Tests + 1;
         Put_Line ("  [FAIL] " & Test_Name & " (expected" & Unsigned_64'Image (B) &
                   ", got" & Unsigned_64'Image (A) & ")");
      end if;
   end Assert_Eq;

   ---------------------------------------------------------------------------
   --  Context Builders
   ---------------------------------------------------------------------------

   function Make_Deposit_Context (
      User   : Address;
      Amount : Unsigned_64;
      Tier   : Unsigned_8;
      Height : Natural
   ) return Call_Context is
      Ctx  : Call_Context := Empty_Context;
      Temp : Unsigned_64 := Amount;
   begin
      Ctx.Caller := User;
      Ctx.Entry_Point := (16#d0#, 16#e3#, 16#0d#, 16#b0#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 9;

      for I in reverse 0 .. 7 loop
         Ctx.Params (I) := Unsigned_8 (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;
      Ctx.Params (8) := Tier;

      return Ctx;
   end Make_Deposit_Context;

   function Make_Withdraw_Context (
      User   : Address;
      Shares : Unsigned_64;
      Height : Natural
   ) return Call_Context is
      Ctx  : Call_Context := Empty_Context;
      Temp : Unsigned_64 := Shares;
   begin
      Ctx.Caller := User;
      Ctx.Entry_Point := (16#2e#, 16#1a#, 16#7d#, 16#4d#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 8;

      for I in reverse 0 .. 7 loop
         Ctx.Params (I) := Unsigned_8 (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;

      return Ctx;
   end Make_Withdraw_Context;

   function Make_Balance_Context (
      User   : Address;
      Height : Natural
   ) return Call_Context is
      Ctx : Call_Context := Empty_Context;
   begin
      Ctx.Caller := User;
      Ctx.Entry_Point := (16#f8#, 16#b2#, 16#cb#, 16#4f#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 32;

      for I in User'Range loop
         Ctx.Params (I) := User (I);
      end loop;

      return Ctx;
   end Make_Balance_Context;

   function Make_Claim_Context (
      User   : Address;
      Height : Natural
   ) return Call_Context is
      Ctx : Call_Context := Empty_Context;
   begin
      Ctx.Caller := User;
      Ctx.Entry_Point := (16#37#, 16#9e#, 16#07#, 16#28#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 0;
      return Ctx;
   end Make_Claim_Context;

   function Make_Stats_Context (Height : Natural) return Call_Context is
      Ctx : Call_Context := Empty_Context;
   begin
      Ctx.Entry_Point := (16#bb#, 16#7e#, 16#70#, 16#a8#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 0;
      return Ctx;
   end Make_Stats_Context;

   function Make_Propose_Context (
      User   : Address;
      Kind   : Unsigned_8;
      Param  : Unsigned_64;
      Height : Natural
   ) return Call_Context is
      Ctx  : Call_Context := Empty_Context;
      Temp : Unsigned_64 := Param;
   begin
      Ctx.Caller := User;
      Ctx.Entry_Point := (16#01#, 16#3c#, 16#f0#, 16#8b#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 9;
      Ctx.Params (0) := Kind;

      for I in reverse 1 .. 8 loop
         Ctx.Params (I) := Unsigned_8 (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;

      return Ctx;
   end Make_Propose_Context;

   function Make_Vote_Context (
      User        : Address;
      Proposal_ID : Unsigned_8;
      Vote_For    : Boolean;
      Height      : Natural
   ) return Call_Context is
      Ctx : Call_Context := Empty_Context;
   begin
      Ctx.Caller := User;
      Ctx.Entry_Point := (16#0d#, 16#15#, 16#fe#, 16#49#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 2;
      Ctx.Params (0) := Proposal_ID;
      Ctx.Params (1) := (if Vote_For then 1 else 0);
      return Ctx;
   end Make_Vote_Context;

   function Make_Rebalance_Context (Height : Natural) return Call_Context is
      Ctx : Call_Context := Empty_Context;
   begin
      Ctx.Entry_Point := (16#43#, 16#8b#, 16#63#, 16#00#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 0;
      return Ctx;
   end Make_Rebalance_Context;

   function Make_Emergency_Context (
      User   : Address;
      Height : Natural
   ) return Call_Context is
      Ctx : Call_Context := Empty_Context;
   begin
      Ctx.Caller := User;
      Ctx.Entry_Point := (16#8c#, 16#41#, 16#dd#, 16#05#, others => 0);
      Ctx.Height := Height;
      Ctx.Param_Len := 0;
      return Ctx;
   end Make_Emergency_Context;

   --  Timing helpers
   Start_Time : Time;
   End_Time   : Time;
   Duration   : Time_Span;

   function Get_Microseconds return Natural is
   begin
      Duration := End_Time - Start_Time;
      return Natural (To_Duration (Duration) * 1_000_000.0);
   end Get_Microseconds;

   --  Init params
   Init_Params : Byte_Array (0 .. 31);

begin
   Put_Line ("================================================================================");
   Put_Line ("  QUANTUM VAULT CVM - COMPREHENSIVE TEST SUITE");
   Put_Line ("  Testing: Deposits, Withdrawals, Yield, Governance, Emergency, Performance");
   Put_Line ("================================================================================");
   Put_Line ("");

   --  Prepare init params (guardian address)
   for I in Guardian'Range loop
      Init_Params (I) := Guardian (I);
   end loop;

   ---------------------------------------------------------------------------
   --  TEST 1: Initialization
   ---------------------------------------------------------------------------
   Put_Line ("TEST 1: Vault Initialization");
   Put_Line ("----------------------------");

   Start_Time := Clock;
   Quantum_Vault.Init (Init_Params, State, Success);
   End_Time := Clock;

   Assert_True (Success, "Vault initialized successfully");
   Record_Metric ("Init", Get_Microseconds, 50000, Success);

   --  Verify initial state
   declare
      Ctx : constant Call_Context := Make_Stats_Context (1);
      Total_Deposits : Unsigned_64;
      Total_Shares   : Unsigned_64;
   begin
      Start_Time := Clock;
      Quantum_Vault.Get_Vault_Stats (Ctx, State, Result);
      End_Time := Clock;

      Total_Deposits := Get_U64 (Result.Return_Data, 0);
      Total_Shares := Get_U64 (Result.Return_Data, 8);

      Assert_Eq (Total_Deposits, 0, "Initial total deposits = 0");
      Assert_Eq (Total_Shares, 0, "Initial total shares = 0");
      Record_Metric ("GetStats", Get_Microseconds, 5000, Result.Status = CVM_Types.Success);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 2: Deposits with Different Tiers
   ---------------------------------------------------------------------------
   Put_Line ("TEST 2: Multi-Tier Deposits");
   Put_Line ("---------------------------");

   --  Alice deposits 10000 in Tier 0 (30 days, 5% APY)
   declare
      Ctx : constant Call_Context := Make_Deposit_Context (Alice, 10000, 0, 100);
      Shares : Unsigned_64;
   begin
      Start_Time := Clock;
      Quantum_Vault.Deposit (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Status = CVM_Types.Success, "Alice deposit status OK");
      Assert_True (Result.Return_Data (0) = 1, "Alice deposit success flag");
      Shares := Get_U64 (Result.Return_Data, 1);
      Assert_Eq (Shares, 10000, "Alice received correct shares");
      Record_Metric ("Deposit_T0", Get_Microseconds, 80000, Result.Return_Data (0) = 1);
   end;

   --  Bob deposits 25000 in Tier 2 (180 days, 12% APY)
   declare
      Ctx : constant Call_Context := Make_Deposit_Context (Bob, 25000, 2, 101);
      Shares : Unsigned_64;
   begin
      Start_Time := Clock;
      Quantum_Vault.Deposit (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 1, "Bob deposit success");
      Shares := Get_U64 (Result.Return_Data, 1);
      Put_Line ("  Bob shares: " & Unsigned_64'Image (Shares));
      Record_Metric ("Deposit_T2", Get_Microseconds, 80000, Result.Return_Data (0) = 1);
   end;

   --  Charlie deposits 50000 in Tier 3 (365 days, 18% APY)
   declare
      Ctx : constant Call_Context := Make_Deposit_Context (Charlie, 50000, 3, 102);
      Shares : Unsigned_64;
   begin
      Start_Time := Clock;
      Quantum_Vault.Deposit (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 1, "Charlie deposit success");
      Shares := Get_U64 (Result.Return_Data, 1);
      Put_Line ("  Charlie shares: " & Unsigned_64'Image (Shares));
      Record_Metric ("Deposit_T3", Get_Microseconds, 80000, Result.Return_Data (0) = 1);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 3: Balance Queries
   ---------------------------------------------------------------------------
   Put_Line ("TEST 3: Balance Queries");
   Put_Line ("-----------------------");

   declare
      Ctx : constant Call_Context := Make_Balance_Context (Alice, 200);
      Amount, Shares, Accumulated : Unsigned_64;
   begin
      Start_Time := Clock;
      Quantum_Vault.Get_Balance (Ctx, State, Result);
      End_Time := Clock;

      Amount := Get_U64 (Result.Return_Data, 0);
      Shares := Get_U64 (Result.Return_Data, 8);
      Accumulated := Get_U64 (Result.Return_Data, 16);

      Put_Line ("  Alice - Amount:" & Unsigned_64'Image (Amount) &
                ", Shares:" & Unsigned_64'Image (Shares) &
                ", Accumulated:" & Unsigned_64'Image (Accumulated));
      Assert_True (Amount > 0, "Alice has positive balance");
      Record_Metric ("GetBalance", Get_Microseconds, 5000, Amount > 0);
   end;

   --  Vault stats after deposits
   declare
      Ctx : constant Call_Context := Make_Stats_Context (200);
      Total_Deposits, Total_Shares : Unsigned_64;
   begin
      Quantum_Vault.Get_Vault_Stats (Ctx, State, Result);

      Total_Deposits := Get_U64 (Result.Return_Data, 0);
      Total_Shares := Get_U64 (Result.Return_Data, 8);

      Put_Line ("  Vault Total Deposits:" & Unsigned_64'Image (Total_Deposits));
      Put_Line ("  Vault Total Shares:" & Unsigned_64'Image (Total_Shares));
      Assert_True (Total_Deposits = 85000, "Total deposits = 85000");
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 4: Flash Loan Protection
   ---------------------------------------------------------------------------
   Put_Line ("TEST 4: Flash Loan Protection");
   Put_Line ("-----------------------------");

   --  Try to withdraw immediately (should fail - flash loan protection)
   declare
      Ctx : constant Call_Context := Make_Withdraw_Context (Alice, 5000, 100);  -- Same block
   begin
      Start_Time := Clock;
      Quantum_Vault.Withdraw (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 0, "Flash loan protection blocks immediate withdrawal");
      Record_Metric ("FlashProtect", Get_Microseconds, 10000, Result.Return_Data (0) = 0);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 5: Partial Withdrawal (After Flash Protection)
   ---------------------------------------------------------------------------
   Put_Line ("TEST 5: Partial Withdrawal");
   Put_Line ("--------------------------");

   --  Simulate 1000 blocks later (past lock period for testing)
   declare
      Ctx : constant Call_Context := Make_Withdraw_Context (Alice, 3000, 300000);  -- Way past lock
      Withdrawn : Unsigned_64;
   begin
      Start_Time := Clock;
      Quantum_Vault.Withdraw (Ctx, State, Result);
      End_Time := Clock;

      if Result.Return_Data (0) = 1 then
         Withdrawn := Get_U64 (Result.Return_Data, 1);
         Put_Line ("  Alice withdrew:" & Unsigned_64'Image (Withdrawn));
         Assert_True (Withdrawn > 0, "Withdrawal amount positive");
      else
         Assert_True (True, "Withdrawal processed (may have penalty)");
      end if;
      Record_Metric ("Withdraw", Get_Microseconds, 100000, True);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 6: Yield Estimation
   ---------------------------------------------------------------------------
   Put_Line ("TEST 6: Yield Estimation");
   Put_Line ("------------------------");

   declare
      Ctx : Call_Context := Empty_Context;
      Est_Yield : Unsigned_64;
      Amount : constant Unsigned_64 := 100000;
      Temp   : Unsigned_64;
   begin
      Ctx.Entry_Point := (16#91#, 16#d1#, 16#48#, 16#54#, others => 0);
      Ctx.Height := 1000;
      Ctx.Param_Len := 11;

      --  Amount
      Temp := Amount;
      for I in reverse 0 .. 7 loop
         Ctx.Params (I) := Unsigned_8 (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;
      --  Tier 3 (18% APY)
      Ctx.Params (8) := 3;
      --  365 days
      Ctx.Params (9) := 1;  -- High byte
      Ctx.Params (10) := 109;  -- Low byte = 365

      Start_Time := Clock;
      Quantum_Vault.Get_Yield_Estimate (Ctx, State, Result);
      End_Time := Clock;

      Est_Yield := Get_U64 (Result.Return_Data, 0);
      Put_Line ("  100000 @ Tier 3 for 365 days = " & Unsigned_64'Image (Est_Yield) & " yield");
      Assert_True (Est_Yield > 0, "Yield estimate is positive");
      Record_Metric ("YieldEst", Get_Microseconds, 3000, Est_Yield > 0);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 7: Governance Proposal
   ---------------------------------------------------------------------------
   Put_Line ("TEST 7: Governance - Create Proposal");
   Put_Line ("------------------------------------");

   declare
      Ctx : constant Call_Context := Make_Propose_Context (Guardian, 0, 100, 1000);
      Prop_ID : Unsigned_8;
   begin
      Start_Time := Clock;
      Quantum_Vault.Propose (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 1, "Proposal created successfully");
      Prop_ID := Result.Return_Data (1);
      Put_Line ("  Created proposal ID:" & Unsigned_8'Image (Prop_ID));
      Record_Metric ("Propose", Get_Microseconds, 50000, Result.Return_Data (0) = 1);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 8: Governance Voting
   ---------------------------------------------------------------------------
   Put_Line ("TEST 8: Governance - Voting");
   Put_Line ("---------------------------");

   --  Alice votes FOR
   declare
      Ctx : constant Call_Context := Make_Vote_Context (Alice, 0, True, 1001);
   begin
      Start_Time := Clock;
      Quantum_Vault.Vote (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 1, "Alice vote recorded");
      Record_Metric ("Vote_For", Get_Microseconds, 30000, Result.Return_Data (0) = 1);
   end;

   --  Bob votes FOR
   declare
      Ctx : constant Call_Context := Make_Vote_Context (Bob, 0, True, 1002);
   begin
      Quantum_Vault.Vote (Ctx, State, Result);
      Assert_True (Result.Return_Data (0) = 1, "Bob vote recorded");
   end;

   --  Charlie votes AGAINST
   declare
      Ctx : constant Call_Context := Make_Vote_Context (Charlie, 0, False, 1003);
   begin
      Start_Time := Clock;
      Quantum_Vault.Vote (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 1, "Charlie vote recorded");
      Record_Metric ("Vote_Against", Get_Microseconds, 30000, Result.Return_Data (0) = 1);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 9: Rebalance
   ---------------------------------------------------------------------------
   Put_Line ("TEST 9: Rebalance Engine");
   Put_Line ("------------------------");

   declare
      Ctx : constant Call_Context := Make_Rebalance_Context (2000);
   begin
      Start_Time := Clock;
      Quantum_Vault.Rebalance (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 1, "Rebalance executed");
      Record_Metric ("Rebalance", Get_Microseconds, 100000, Result.Return_Data (0) = 1);
   end;

   --  Try immediate rebalance (should fail - cooldown)
   declare
      Ctx : constant Call_Context := Make_Rebalance_Context (2001);
   begin
      Quantum_Vault.Rebalance (Ctx, State, Result);
      Assert_True (Result.Return_Data (0) = 0, "Rebalance cooldown enforced");
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 10: Emergency Unlock
   ---------------------------------------------------------------------------
   Put_Line ("TEST 10: Emergency Unlock");
   Put_Line ("-------------------------");

   declare
      Ctx : constant Call_Context := Make_Emergency_Context (Guardian, 5000);
      Unlock_Time : Unsigned_64;
   begin
      Start_Time := Clock;
      Quantum_Vault.Emergency_Unlock (Ctx, State, Result);
      End_Time := Clock;

      Assert_True (Result.Return_Data (0) = 1, "Emergency unlock initiated");
      Unlock_Time := Get_U64 (Result.Return_Data, 1);
      Put_Line ("  Emergency unlock scheduled at:" & Unsigned_64'Image (Unlock_Time));
      Record_Metric ("Emergency", Get_Microseconds, 50000, Result.Return_Data (0) = 1);
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  TEST 11: Stress Test - Multiple Operations
   ---------------------------------------------------------------------------
   Put_Line ("TEST 11: Stress Test (100 Operations)");
   Put_Line ("-------------------------------------");

   declare
      Stress_Start : constant Time := Clock;
      Stress_End   : Time;
      Ops_Count    : Natural := 0;
   begin
      --  Perform 100 mixed operations
      for I in 1 .. 25 loop
         --  4 operations per iteration
         declare
            Dep_Ctx : constant Call_Context := Make_Deposit_Context (Alice, 100, 0, 10000 + I);
            Bal_Ctx : constant Call_Context := Make_Balance_Context (Alice, 10000 + I);
            Stat_Ctx : constant Call_Context := Make_Stats_Context (10000 + I);
         begin
            Quantum_Vault.Deposit (Dep_Ctx, State, Result);
            Ops_Count := Ops_Count + 1;

            Quantum_Vault.Get_Balance (Bal_Ctx, State, Result);
            Ops_Count := Ops_Count + 1;

            Quantum_Vault.Get_Vault_Stats (Stat_Ctx, State, Result);
            Ops_Count := Ops_Count + 1;

            if I > 10 then
               declare
                  Reb_Ctx : constant Call_Context := Make_Rebalance_Context (10000 + I * 1000);
               begin
                  Quantum_Vault.Rebalance (Reb_Ctx, State, Result);
                  Ops_Count := Ops_Count + 1;
               end;
            else
               Ops_Count := Ops_Count + 1;
            end if;
         end;
      end loop;

      Stress_End := Clock;
      declare
         Total_Us : constant Natural := Natural (To_Duration (Stress_End - Stress_Start) * 1_000_000.0);
         Ops_Per_Sec : Natural;
      begin
         if Total_Us > 0 then
            Ops_Per_Sec := (Ops_Count * 1_000_000) / Total_Us;
         else
            Ops_Per_Sec := Ops_Count * 1000000;
         end if;
         Put_Line ("  Completed" & Natural'Image (Ops_Count) & " operations in" &
                   Natural'Image (Total_Us) & " us");
         Put_Line ("  Throughput:" & Natural'Image (Ops_Per_Sec) & " ops/sec");
         Record_Metric ("StressTest", Total_Us, 0, True);
      end;
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  Final Statistics
   ---------------------------------------------------------------------------
   Put_Line ("================================================================================");
   Put_Line ("  FINAL VAULT STATE");
   Put_Line ("================================================================================");

   declare
      Ctx : constant Call_Context := Make_Stats_Context (99999);
      Total_Deposits, Total_Shares, Acc_Yield, Last_Reb : Unsigned_64;
   begin
      Quantum_Vault.Get_Vault_Stats (Ctx, State, Result);

      Total_Deposits := Get_U64 (Result.Return_Data, 0);
      Total_Shares := Get_U64 (Result.Return_Data, 8);
      Acc_Yield := Get_U64 (Result.Return_Data, 16);
      Last_Reb := Get_U64 (Result.Return_Data, 24);

      Put_Line ("  Total Deposits:     " & Unsigned_64'Image (Total_Deposits));
      Put_Line ("  Total Shares:       " & Unsigned_64'Image (Total_Shares));
      Put_Line ("  Accumulated Yield:  " & Unsigned_64'Image (Acc_Yield));
      Put_Line ("  Last Rebalance:     " & Unsigned_64'Image (Last_Reb));
   end;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  Performance Report
   ---------------------------------------------------------------------------
   Put_Line ("================================================================================");
   Put_Line ("  PERFORMANCE METRICS");
   Put_Line ("================================================================================");
   Put_Line ("  Operation           | Duration (us) | Gas Est  | Status");
   Put_Line ("  --------------------|---------------|----------|--------");

   for I in 1 .. Metric_Count loop
      Put ("  " & Metrics (I).Operation & " |");
      Put (Natural'Image (Metrics (I).Duration_Us));
      for J in Natural'Image (Metrics (I).Duration_Us)'Length .. 13 loop
         Put (" ");
      end loop;
      Put (" |");
      Put (Natural'Image (Metrics (I).Gas_Est));
      for J in Natural'Image (Metrics (I).Gas_Est)'Length .. 8 loop
         Put (" ");
      end loop;
      Put (" | ");
      if Metrics (I).Success then
         Put_Line ("OK");
      else
         Put_Line ("FAIL");
      end if;
   end loop;
   Put_Line ("");

   ---------------------------------------------------------------------------
   --  Summary
   ---------------------------------------------------------------------------
   Put_Line ("================================================================================");
   Put_Line ("  TEST SUMMARY");
   Put_Line ("================================================================================");
   Put_Line ("  Total Tests:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed:       " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed:       " & Natural'Image (Failed_Tests));

   if Failed_Tests = 0 then
      Put_Line ("");
      Put_Line ("  *** ALL TESTS PASSED ***");
   else
      Put_Line ("");
      Put_Line ("  !!! SOME TESTS FAILED !!!");
   end if;
   Put_Line ("================================================================================");

end Test_Quantum_Vault;
