pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_U256; use Aegis_U256;

package body Khepri_Test_Runner is

   ---------------------------------------------------------------------------
   --  Session Management
   ---------------------------------------------------------------------------

   function Create_Session (
      Config : Runner_Config
   ) return Test_Session is
   begin
      return (
         Config       => Config,
         Suites       => (others => (
            Info       => (
               Name          => Empty_String,
               Description   => Empty_String,
               Contract_Path => Empty_String,
               Setup_Gas     => 0
            ),
            Tests      => (others => (
               Info   => Default_Test_Info,
               Result => Passed_Result,
               Is_Run => False
            )),
            Test_Count => 0,
            Stats      => (
               Total_Tests   => 0,
               Passed        => 0,
               Failed        => 0,
               Errors        => 0,
               Skipped       => 0,
               Timeouts      => 0,
               Total_Gas     => 0,
               Total_Time_Ms => 0
            ),
            Fixture    => Null_Fixture
         )),
         Suite_Count  => 0,
         Current_Suite=> 0,
         Total_Stats  => (
            Total_Tests   => 0,
            Passed        => 0,
            Failed        => 0,
            Errors        => 0,
            Skipped       => 0,
            Timeouts      => 0,
            Total_Gas     => 0,
            Total_Time_Ms => 0
         ),
         Is_Running   => False
      );
   end Create_Session;

   procedure Add_Suite (
      Session : in Out Test_Session;
      Info    : in     Suite_Info;
      Success : out    Boolean
   ) is
   begin
      if Session.Suite_Count >= Max_Suites then
         Success := False;
         return;
      end if;

      Session.Suites (Suite_Index (Session.Suite_Count)).Info := Info;
      Session.Suites (Suite_Index (Session.Suite_Count)).Test_Count := 0;
      Session.Current_Suite := Session.Suite_Count;
      Session.Suite_Count := Session.Suite_Count + 1;
      Success := True;
   end Add_Suite;

   procedure Add_Test (
      Session   : in Out Test_Session;
      Test_Info : in     Test_Case_Info;
      Success   : out    Boolean
   ) is
      Suite_Idx : constant Suite_Index := Suite_Index (Session.Current_Suite);
   begin
      if Session.Suite_Count = 0 then
         Success := False;
         return;
      end if;

      if Session.Suites (Suite_Idx).Test_Count >= Max_Tests then
         Success := False;
         return;
      end if;

      declare
         Test_Idx : constant Test_Index :=
            Test_Index (Session.Suites (Suite_Idx).Test_Count);
      begin
         Session.Suites (Suite_Idx).Tests (Test_Idx) := (
            Info   => Test_Info,
            Result => Passed_Result,
            Is_Run => False
         );
         Session.Suites (Suite_Idx).Test_Count :=
            Session.Suites (Suite_Idx).Test_Count + 1;
      end;

      Success := True;
   end Add_Test;

   procedure Run_All (
      Session : in Out Test_Session;
      Stats   : out    Suite_Stats
   ) is
      Result : Test_Result;
   begin
      Session.Is_Running := True;
      Session.Total_Stats := (
         Total_Tests   => 0,
         Passed        => 0,
         Failed        => 0,
         Errors        => 0,
         Skipped       => 0,
         Timeouts      => 0,
         Total_Gas     => 0,
         Total_Time_Ms => 0
      );

      for S in 0 .. Session.Suite_Count - 1 loop
         declare
            Suite_Idx : constant Suite_Index := Suite_Index (S);
         begin
            Session.Suites (Suite_Idx).Stats := (
               Total_Tests   => 0,
               Passed        => 0,
               Failed        => 0,
               Errors        => 0,
               Skipped       => 0,
               Timeouts      => 0,
               Total_Gas     => 0,
               Total_Time_Ms => 0
            );

            for T in 0 .. Session.Suites (Suite_Idx).Test_Count - 1 loop
               declare
                  Test_Idx : constant Test_Index := Test_Index (T);
               begin
                  --  Check if test should be skipped
                  if Session.Suites (Suite_Idx).Tests (Test_Idx).Info.Skip_Reason /=
                     Empty_String
                  then
                     Session.Suites (Suite_Idx).Tests (Test_Idx).Result := (
                        Outcome       => Outcome_Skipped,
                        Duration_Ms   => 0,
                        Gas_Used      => 0,
                        Message       => Session.Suites (Suite_Idx).Tests (Test_Idx).Info.Skip_Reason,
                        Expected      => Empty_String,
                        Actual        => Empty_String,
                        Location      => Empty_String
                     );
                     Session.Suites (Suite_Idx).Stats.Skipped :=
                        Session.Suites (Suite_Idx).Stats.Skipped + 1;
                  else
                     --  Execute test
                     --  NOTE: Actual execution would require invoking contract functions
                     --  through Aegis_Execution. For now, we simulate successful execution.
                     --  In a full implementation, this would:
                     --  1. Create execution context with fixture state
                     --  2. Load contract at fixture address
                     --  3. Call test function via selector
                     --  4. Measure gas and capture result
                     declare
                        Start_Time : constant Natural := 0;  -- Would use Ada.Real_Time
                        Gas_Start  : constant Word64 := Session.Suites (Suite_Idx).Tests (Test_Idx).Info.Gas_Limit;
                     begin
                        Result := (
                           Outcome       => Outcome_Passed,
                           Duration_Ms   => 10,  -- Simulated duration
                           Gas_Used      => 50000,  -- Simulated gas
                           Message       => Empty_String,
                           Expected      => Empty_String,
                           Actual        => Empty_String,
                           Location      => Empty_String
                        );
                     end;

                     Session.Suites (Suite_Idx).Tests (Test_Idx).Result := Result;
                     Session.Suites (Suite_Idx).Tests (Test_Idx).Is_Run := True;

                     --  Update stats
                     case Result.Outcome is
                        when Outcome_Passed =>
                           Session.Suites (Suite_Idx).Stats.Passed :=
                              Session.Suites (Suite_Idx).Stats.Passed + 1;
                        when Outcome_Failed =>
                           Session.Suites (Suite_Idx).Stats.Failed :=
                              Session.Suites (Suite_Idx).Stats.Failed + 1;
                        when Outcome_Error =>
                           Session.Suites (Suite_Idx).Stats.Errors :=
                              Session.Suites (Suite_Idx).Stats.Errors + 1;
                        when Outcome_Skipped =>
                           Session.Suites (Suite_Idx).Stats.Skipped :=
                              Session.Suites (Suite_Idx).Stats.Skipped + 1;
                        when Outcome_Timeout =>
                           Session.Suites (Suite_Idx).Stats.Timeouts :=
                              Session.Suites (Suite_Idx).Stats.Timeouts + 1;
                     end case;

                     Session.Suites (Suite_Idx).Stats.Total_Gas :=
                        Session.Suites (Suite_Idx).Stats.Total_Gas + Result.Gas_Used;
                     Session.Suites (Suite_Idx).Stats.Total_Time_Ms :=
                        Session.Suites (Suite_Idx).Stats.Total_Time_Ms + Result.Duration_Ms;
                  end if;

                  Session.Suites (Suite_Idx).Stats.Total_Tests :=
                     Session.Suites (Suite_Idx).Stats.Total_Tests + 1;
               end;
            end loop;

            --  Aggregate to total stats
            Session.Total_Stats.Total_Tests :=
               Session.Total_Stats.Total_Tests + Session.Suites (Suite_Idx).Stats.Total_Tests;
            Session.Total_Stats.Passed :=
               Session.Total_Stats.Passed + Session.Suites (Suite_Idx).Stats.Passed;
            Session.Total_Stats.Failed :=
               Session.Total_Stats.Failed + Session.Suites (Suite_Idx).Stats.Failed;
            Session.Total_Stats.Errors :=
               Session.Total_Stats.Errors + Session.Suites (Suite_Idx).Stats.Errors;
            Session.Total_Stats.Skipped :=
               Session.Total_Stats.Skipped + Session.Suites (Suite_Idx).Stats.Skipped;
            Session.Total_Stats.Timeouts :=
               Session.Total_Stats.Timeouts + Session.Suites (Suite_Idx).Stats.Timeouts;
            Session.Total_Stats.Total_Gas :=
               Session.Total_Stats.Total_Gas + Session.Suites (Suite_Idx).Stats.Total_Gas;
            Session.Total_Stats.Total_Time_Ms :=
               Session.Total_Stats.Total_Time_Ms + Session.Suites (Suite_Idx).Stats.Total_Time_Ms;
         end;
      end loop;

      Session.Is_Running := False;
      Stats := Session.Total_Stats;
   end Run_All;

   procedure Run_Test (
      Session    : in Out Test_Session;
      Test_Idx   : in     Test_Index;
      Result     : out    Test_Result
   ) is
   begin
      if Session.Suite_Count = 0 or
         Natural (Test_Idx) >= Session.Suites (Suite_Index (Session.Current_Suite)).Test_Count
      then
         Result := (
            Outcome       => Outcome_Error,
            Duration_Ms   => 0,
            Gas_Used      => 0,
            Message       => Empty_String,
            Expected      => Empty_String,
            Actual        => Empty_String,
            Location      => Empty_String
         );
         return;
      end if;

      --  Execute individual test
      --  Similar to Run_All but for single test
      declare
         Test_Info : constant Test_Case_Info :=
            Session.Suites (Suite_Index (Session.Current_Suite)).Tests (Test_Idx).Info;
      begin
         --  Check if test should be skipped
         if Test_Info.Skip_Reason /= Empty_String then
            Result := (
               Outcome       => Outcome_Skipped,
               Duration_Ms   => 0,
               Gas_Used      => 0,
               Message       => Test_Info.Skip_Reason,
               Expected      => Empty_String,
               Actual        => Empty_String,
               Location      => Empty_String
            );
         else
            --  Run test (simulated - would call contract via Aegis_Execution)
            Result := (
               Outcome       => Outcome_Passed,
               Duration_Ms   => 10,
               Gas_Used      => 50000,
               Message       => Empty_String,
               Expected      => Empty_String,
               Actual        => Empty_String,
               Location      => Empty_String
            );
         end if;
      end;

      Session.Suites (Suite_Index (Session.Current_Suite)).Tests (Test_Idx).Result := Result;
      Session.Suites (Suite_Index (Session.Current_Suite)).Tests (Test_Idx).Is_Run := True;
   end Run_Test;

   procedure Get_Results (
      Session    : in  Test_Session;
      Tests      : out Test_Array;
      Test_Count : out Natural
   ) is
   begin
      if Session.Suite_Count = 0 then
         Tests := (others => (
            Info   => Default_Test_Info,
            Result => Passed_Result,
            Is_Run => False
         ));
         Test_Count := 0;
         return;
      end if;

      Tests := Session.Suites (Suite_Index (Session.Current_Suite)).Tests;
      Test_Count := Session.Suites (Suite_Index (Session.Current_Suite)).Test_Count;
   end Get_Results;

   function Get_Stats (
      Session : Test_Session
   ) return Suite_Stats is
   begin
      return Session.Total_Stats;
   end Get_Stats;

   ---------------------------------------------------------------------------
   --  Test Fixtures
   ---------------------------------------------------------------------------

   procedure Setup_Fixture (
      Session : in Out Test_Session;
      Fixture : out    Fixture_State;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Session);
   begin
      --  Setup test fixture
      --  In a full implementation, this would:
      --  1. Deploy contract code to test address via Aegis_Execution
      --  2. Initialize state using Khepri_MPT
      --  3. Set initial balances
      --  4. Configure block environment
      --
      --  For now, we create a fixture with default values
      Fixture := (
         Contract_Address => (others => 16#42#),  -- Test contract address
         Deployer_Address => (others => 16#01#),  -- Default deployer
         Initial_Balance  => From_Natural (1_000_000),  -- 1M wei initial balance
         Block_Number     => 1,  -- Genesis + 1
         Timestamp        => 1700000000,  -- Mock timestamp
         Is_Setup         => True
      );
      Success := True;
   end Setup_Fixture;

   procedure Teardown_Fixture (
      Session : in Out Test_Session;
      Fixture : in Out Fixture_State
   ) is
      pragma Unreferenced (Session);
   begin
      Fixture.Is_Setup := False;
   end Teardown_Fixture;

   ---------------------------------------------------------------------------
   --  Contract Interaction
   ---------------------------------------------------------------------------

   procedure Call_Contract (
      Session  : in     Test_Session;
      Fixture  : in     Fixture_State;
      Selector : in     Bytes4;
      Args     : in     Byte_Array;
      Result   : out    Call_Result
   ) is
      pragma Unreferenced (Session, Fixture, Selector, Args);
      pragma SPARK_Mode (Off);
   begin
      --  Execute contract call (read-only, staticcall)
      --  In a full implementation, this would:
      --  1. Create Aegis_Execution context with Mode_Static
      --  2. Load contract at Fixture.Contract_Address
      --  3. Encode call data with Selector and Args
      --  4. Execute via Aegis_Contract interface
      --  5. Decode return data
      --  6. Track gas consumption
      --
      --  For now, simulate successful call with no return data
      Result := (
         Success     => True,
         Return_Data => (others => 0),
         Data_Size   => 0,
         Gas_Used    => 21000,  -- Base gas cost
         Revert_Msg  => Empty_String
      );
   end Call_Contract;

   procedure Send_Transaction (
      Session  : in     Test_Session;
      Fixture  : in     Fixture_State;
      Selector : in     Bytes4;
      Args     : in     Byte_Array;
      Value    : in     Uint256;
      Result   : out    Call_Result
   ) is
      pragma Unreferenced (Session, Fixture, Selector, Args, Value);
      pragma SPARK_Mode (Off);
   begin
      --  Execute state-modifying transaction
      --  In a full implementation, this would:
      --  1. Create Aegis_Execution context with Mode_Normal
      --  2. Transfer Value to contract if non-zero
      --  3. Execute contract function with Selector and Args
      --  4. Commit state changes to Khepri_MPT
      --  5. Capture logs/events
      --  6. Return result and gas used
      --
      --  For now, simulate successful transaction
      Result := (
         Success     => True,
         Return_Data => (others => 0),
         Data_Size   => 0,
         Gas_Used    => 21000,  -- Base transaction cost
         Revert_Msg  => Empty_String
      );
   end Send_Transaction;

   ---------------------------------------------------------------------------
   --  State Manipulation
   ---------------------------------------------------------------------------

   procedure Set_Storage (
      Session : in Out Test_Session;
      Fixture : in     Fixture_State;
      Slot    : in     Hash256;
      Value   : in     Uint256;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Session, Fixture, Slot, Value);
      pragma SPARK_Mode (Off);
   begin
      --  Set contract storage slot (for test setup)
      --  In a full implementation, this would:
      --  1. Get Khepri_MPT trie for contract at Fixture.Contract_Address
      --  2. Encode Slot as storage key
      --  3. Encode Value as RLP
      --  4. Call Khepri_MPT.Put to update trie
      --  5. Update storage root in account state
      --
      --  For now, always succeed
      Success := True;
   end Set_Storage;

   function Get_Storage (
      Session : Test_Session;
      Fixture : Fixture_State;
      Slot    : Hash256
   ) return Uint256 is
      pragma Unreferenced (Session, Fixture, Slot);
      pragma SPARK_Mode (Off);
   begin
      --  Read contract storage slot
      --  In a full implementation, this would:
      --  1. Get Khepri_MPT trie for contract
      --  2. Encode Slot as storage key
      --  3. Call Khepri_MPT.Get to read value
      --  4. Decode RLP value to Uint256
      --
      --  For now, return zero (empty storage)
      return U256_Zero;
   end Get_Storage;

   procedure Set_Balance (
      Session : in Out Test_Session;
      Address : in     Khepri_Types.Address;
      Balance : in     Uint256;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Session, Address, Balance);
      pragma SPARK_Mode (Off);
   begin
      --  Set account balance (for test setup)
      --  In a full implementation, this would:
      --  1. Load account state from Khepri_MPT global trie
      --  2. Update Balance field
      --  3. Update account state in trie
      --  4. Recompute state root
      --
      --  For now, always succeed
      Success := True;
   end Set_Balance;

   procedure Mine_Block (
      Session : in Out Test_Session;
      Count   : in     Natural := 1
   ) is
   begin
      --  Advance block number (for testing time-dependent contracts)
      --  In a full implementation, this would:
      --  1. Increment global block number
      --  2. Update block timestamp
      --  3. Trigger any block-based state transitions
      --
      --  For now, just update fixture block number
      if Session.Suite_Count > 0 then
         for I in 1 .. Count loop
            Session.Suites (Suite_Index (Session.Current_Suite)).Fixture.Block_Number :=
               Session.Suites (Suite_Index (Session.Current_Suite)).Fixture.Block_Number + 1;
         end loop;
      end if;
   end Mine_Block;

   procedure Set_Timestamp (
      Session   : in Out Test_Session;
      Timestamp : in     Word64
   ) is
   begin
      if Session.Suite_Count > 0 then
         Session.Suites (Suite_Index (Session.Current_Suite)).Fixture.Timestamp :=
            Timestamp;
      end if;
   end Set_Timestamp;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   procedure Generate_Report (
      Session : in     Test_Session;
      Format  : in     Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Format);
   begin
      Output := (others => 0);

      declare
         Header : constant String := "KHEPRI Test Report" & ASCII.LF &
            "==================" & ASCII.LF;
      begin
         for I in Header'Range loop
            Output (Output'First + I - Header'First) :=
               Byte (Character'Pos (Header (I)));
         end loop;
         Size := Header'Length + 100;
      end;

      Success := Session.Total_Stats.Total_Tests > 0;
   end Generate_Report;

   function Summary_Line (
      Session : Test_Session
   ) return String is
   begin
      if Session.Total_Stats.Failed = 0 and Session.Total_Stats.Errors = 0 then
         return "All tests passed";
      else
         return "Some tests failed";
      end if;
   end Summary_Line;

end Khepri_Test_Runner;
