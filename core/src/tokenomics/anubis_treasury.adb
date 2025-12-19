-------------------------------------------------------------------------------
--  ANUBIS Protocol Treasury Implementation Body
--  30% DAO-Controlled with Zero Builder Access
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Treasury with
   SPARK_Mode => On
is
   --  Proposal counter
   Proposal_Counter : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Treasury (
      State          : out Treasury_State;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      State.Total_Allocation := Treasury_Allocation;
      State.Current_Balance := Treasury_Allocation;
      State.Total_Spent := 0;
      State.Total_Burned := 0;

      --  Initialize all proposals
      for I in Proposal_Index loop
         State.Active_Proposals (I) := (
            ID             => 0,
            Category       => Small_Grant,
            Prop_Type      => Spending,
            State          => Draft,
            Proposer       => (others => 0),
            Bond_Amount    => 0,
            Bond_Returned  => False,
            Title_Hash     => (others => 0),
            Description_Hash => (others => 0),
            IPFS_Link      => (others => 0),
            Recipient      => (others => 0),
            Request_Amount => 0,
            Vesting_Period => 0,
            Votes_For      => 0,
            Votes_Against  => 0,
            Votes_Abstain  => 0,
            Unique_Voters  => 0,
            Created_At     => 0,
            Voting_Start   => 0,
            Voting_End     => 0,
            Timelock_End   => 0,
            Executed_At    => 0
         );
      end loop;

      State.Active_Count := 0;
      State.Total_Proposals := 0;
      State.Proposals_Passed := 0;
      State.Proposals_Failed := 0;
      State.Total_Votes_Cast := 0;
      State.Unique_Voters := 0;
      State.Circulating_Supply := 0;
      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
   end Init_Treasury;

   ---------------------------------------------------------------------------
   --  Proposal Creation
   ---------------------------------------------------------------------------

   procedure Create_Spending_Proposal (
      State          : in Out Treasury_State;
      Proposer       : Byte_Array;
      Recipient      : Byte_Array;
      Amount         : Unsigned_64;
      Title_Hash     : Byte_Array;
      Description    : Byte_Array;
      Bond           : Unsigned_64;
      Proposal       : out Proposal_Record;
      Result         : out Propose_Result
   ) is
      Category : Proposal_Category;
      Required_Bond : Unsigned_64;
      Slot : Proposal_Index := 0;
      Found_Slot : Boolean := False;
   begin
      --  Initialize proposal to default
      Proposal := (
         ID             => 0,
         Category       => Small_Grant,
         Prop_Type      => Spending,
         State          => Draft,
         Proposer       => (others => 0),
         Bond_Amount    => 0,
         Bond_Returned  => False,
         Title_Hash     => (others => 0),
         Description_Hash => (others => 0),
         IPFS_Link      => (others => 0),
         Recipient      => (others => 0),
         Request_Amount => 0,
         Vesting_Period => 0,
         Votes_For      => 0,
         Votes_Against  => 0,
         Votes_Abstain  => 0,
         Unique_Voters  => 0,
         Created_At     => 0,
         Voting_Start   => 0,
         Voting_End     => 0,
         Timelock_End   => 0,
         Executed_At    => 0
      );

      --  Check too many active proposals
      if State.Active_Count >= Max_Active_Proposals then
         Result := Too_Many_Active;
         return;
      end if;

      --  Check treasury balance
      if Amount > State.Current_Balance then
         Result := Treasury_Empty;
         return;
      end if;

      --  Categorize proposal
      Category := Categorize_Proposal (Amount, State.Current_Balance);
      Required_Bond := Get_Required_Bond (Category);

      --  Check bond
      if Bond < Required_Bond then
         Result := Insufficient_Bond;
         return;
      end if;

      --  Find empty slot
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = 0 then
            Slot := I;
            Found_Slot := True;
            exit;
         end if;
      end loop;

      if not Found_Slot then
         Result := Too_Many_Active;
         return;
      end if;

      --  Create proposal
      Proposal_Counter := Proposal_Counter + 1;

      Proposal.ID := Proposal_Counter;
      Proposal.Category := Category;
      Proposal.Prop_Type := Spending;
      Proposal.State := Active;

      --  Copy proposer
      for I in Proposer'Range loop
         if I - Proposer'First <= Proposal.Proposer'Last then
            Proposal.Proposer (I - Proposer'First) := Proposer (I);
         end if;
      end loop;

      Proposal.Bond_Amount := Bond;
      Proposal.Bond_Returned := False;

      --  Copy title hash
      for I in Title_Hash'Range loop
         if I - Title_Hash'First <= Proposal.Title_Hash'Last then
            Proposal.Title_Hash (I - Title_Hash'First) := Title_Hash (I);
         end if;
      end loop;

      --  Copy description hash
      for I in Description'Range loop
         if I - Description'First <= Proposal.Description_Hash'Last then
            Proposal.Description_Hash (I - Description'First) := Description (I);
         end if;
      end loop;

      --  Copy recipient
      for I in Recipient'Range loop
         if I - Recipient'First <= Proposal.Recipient'Last then
            Proposal.Recipient (I - Recipient'First) := Recipient (I);
         end if;
      end loop;

      Proposal.Request_Amount := Amount;
      Proposal.Vesting_Period := 0;
      Proposal.Votes_For := 0;
      Proposal.Votes_Against := 0;
      Proposal.Votes_Abstain := 0;
      Proposal.Unique_Voters := 0;
      Proposal.Created_At := State.Current_Block;
      Proposal.Voting_Start := State.Current_Block;

      --  Set voting and timelock periods based on category
      case Category is
         when Small_Grant =>
            Proposal.Voting_End := State.Current_Block + Small_Voting_Blocks;
            Proposal.Timelock_End := Proposal.Voting_End + Small_Timelock_Blocks;
         when Medium_Grant =>
            Proposal.Voting_End := State.Current_Block + Medium_Voting_Blocks;
            Proposal.Timelock_End := Proposal.Voting_End + Medium_Timelock_Blocks;
         when Large_Grant =>
            Proposal.Voting_End := State.Current_Block + Large_Voting_Blocks;
            Proposal.Timelock_End := Proposal.Voting_End + Large_Timelock_Blocks;
         when Constitutional =>
            Proposal.Voting_End := State.Current_Block + Constitutional_Voting_Blocks;
            Proposal.Timelock_End := Proposal.Voting_End + Constitutional_Timelock_Blocks;
      end case;

      Proposal.Executed_At := 0;

      --  Store in state
      State.Active_Proposals (Slot) := Proposal;
      State.Active_Count := State.Active_Count + 1;
      State.Total_Proposals := State.Total_Proposals + 1;

      Result := Proposed;
   end Create_Spending_Proposal;

   procedure Create_Parameter_Proposal (
      State          : in Out Treasury_State;
      Proposer       : Byte_Array;
      Parameter_Hash : Byte_Array;
      New_Value_Hash : Byte_Array;
      Bond           : Unsigned_64;
      Proposal       : out Proposal_Record;
      Result         : out Propose_Result
   ) is
      Required_Bond : constant Unsigned_64 := Medium_Bond;
      Slot : Proposal_Index := 0;
      Found_Slot : Boolean := False;
   begin
      --  Initialize proposal to default
      Proposal := (
         ID             => 0,
         Category       => Medium_Grant,
         Prop_Type      => Parameter_Change,
         State          => Draft,
         Proposer       => (others => 0),
         Bond_Amount    => 0,
         Bond_Returned  => False,
         Title_Hash     => (others => 0),
         Description_Hash => (others => 0),
         IPFS_Link      => (others => 0),
         Recipient      => (others => 0),
         Request_Amount => 0,
         Vesting_Period => 0,
         Votes_For      => 0,
         Votes_Against  => 0,
         Votes_Abstain  => 0,
         Unique_Voters  => 0,
         Created_At     => 0,
         Voting_Start   => 0,
         Voting_End     => 0,
         Timelock_End   => 0,
         Executed_At    => 0
      );

      --  Check too many active proposals
      if State.Active_Count >= Max_Active_Proposals then
         Result := Too_Many_Active;
         return;
      end if;

      --  Check bond
      if Bond < Required_Bond then
         Result := Insufficient_Bond;
         return;
      end if;

      --  Find empty slot
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = 0 then
            Slot := I;
            Found_Slot := True;
            exit;
         end if;
      end loop;

      if not Found_Slot then
         Result := Too_Many_Active;
         return;
      end if;

      --  Create proposal
      Proposal_Counter := Proposal_Counter + 1;

      Proposal.ID := Proposal_Counter;
      Proposal.Category := Medium_Grant;
      Proposal.Prop_Type := Parameter_Change;
      Proposal.State := Active;

      --  Copy proposer
      for I in Proposer'Range loop
         if I - Proposer'First <= Proposal.Proposer'Last then
            Proposal.Proposer (I - Proposer'First) := Proposer (I);
         end if;
      end loop;

      Proposal.Bond_Amount := Bond;
      Proposal.Bond_Returned := False;

      --  Copy parameter hash as title
      for I in Parameter_Hash'Range loop
         if I - Parameter_Hash'First <= Proposal.Title_Hash'Last then
            Proposal.Title_Hash (I - Parameter_Hash'First) := Parameter_Hash (I);
         end if;
      end loop;

      --  Copy new value hash as description
      for I in New_Value_Hash'Range loop
         if I - New_Value_Hash'First <= Proposal.Description_Hash'Last then
            Proposal.Description_Hash (I - New_Value_Hash'First) := New_Value_Hash (I);
         end if;
      end loop;

      Proposal.Request_Amount := 0;
      Proposal.Votes_For := 0;
      Proposal.Votes_Against := 0;
      Proposal.Votes_Abstain := 0;
      Proposal.Unique_Voters := 0;
      Proposal.Created_At := State.Current_Block;
      Proposal.Voting_Start := State.Current_Block;
      Proposal.Voting_End := State.Current_Block + Medium_Voting_Blocks;
      Proposal.Timelock_End := Proposal.Voting_End + Medium_Timelock_Blocks;
      Proposal.Executed_At := 0;

      --  Store in state
      State.Active_Proposals (Slot) := Proposal;
      State.Active_Count := State.Active_Count + 1;
      State.Total_Proposals := State.Total_Proposals + 1;

      Result := Proposed;
   end Create_Parameter_Proposal;

   function Categorize_Proposal (
      Amount         : Unsigned_64;
      Treasury_Balance : Unsigned_64
   ) return Proposal_Category is
      Pct_BP : Unsigned_64;
   begin
      if Treasury_Balance = 0 then
         return Constitutional;
      end if;

      --  Calculate percentage in basis points
      Pct_BP := (Amount * 10_000) / Treasury_Balance;

      if Pct_BP <= 50 then  --  <0.5%
         return Small_Grant;
      elsif Pct_BP <= 100 then  --  0.5-1%
         return Medium_Grant;
      elsif Pct_BP <= 200 then  --  1-2%
         return Large_Grant;
      else
         return Constitutional;
      end if;
   end Categorize_Proposal;

   function Get_Required_Bond (
      Category       : Proposal_Category
   ) return Unsigned_64 is
   begin
      case Category is
         when Small_Grant    => return Small_Bond;
         when Medium_Grant   => return Medium_Bond;
         when Large_Grant    => return Large_Bond;
         when Constitutional => return Constitutional_Bond;
      end case;
   end Get_Required_Bond;

   function Get_Required_Quorum (
      Category       : Proposal_Category;
      Circulating    : Unsigned_64
   ) return Unsigned_64 is
      Quorum_BP : Natural;
   begin
      case Category is
         when Small_Grant    => Quorum_BP := Small_Quorum_BP;
         when Medium_Grant   => Quorum_BP := Medium_Quorum_BP;
         when Large_Grant    => Quorum_BP := Large_Quorum_BP;
         when Constitutional => Quorum_BP := Constitutional_Quorum_BP;
      end case;

      return (Circulating * Unsigned_64 (Quorum_BP)) / 10_000;
   end Get_Required_Quorum;

   function Get_Required_Threshold (
      Category       : Proposal_Category
   ) return Natural is
   begin
      case Category is
         when Small_Grant    => return Small_Threshold_BP;
         when Medium_Grant   => return Medium_Threshold_BP;
         when Large_Grant    => return Large_Threshold_BP;
         when Constitutional => return Constitutional_Threshold_BP;
      end case;
   end Get_Required_Threshold;

   ---------------------------------------------------------------------------
   --  Voting
   ---------------------------------------------------------------------------

   procedure Cast_Vote (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Voter          : Byte_Array;
      Choice         : Vote_Choice;
      Weight         : Unsigned_64;
      Signature      : Byte_Array;
      Result         : out Vote_Result
   ) is
      pragma Unreferenced (Voter, Signature);
      Found : Boolean := False;
      Idx : Proposal_Index := 0;
   begin
      --  Find proposal
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Found := True;
            Idx := I;
            exit;
         end if;
      end loop;

      if not Found then
         Result := Proposal_Not_Active;
         return;
      end if;

      --  Check proposal is active
      if State.Active_Proposals (Idx).State /= Active then
         Result := Proposal_Not_Active;
         return;
      end if;

      --  Check voting period
      if State.Current_Block > State.Active_Proposals (Idx).Voting_End then
         Result := Voting_Ended;
         return;
      end if;

      --  Record vote
      case Choice is
         when For_Proposal =>
            State.Active_Proposals (Idx).Votes_For :=
               State.Active_Proposals (Idx).Votes_For + Weight;
         when Against_Proposal =>
            State.Active_Proposals (Idx).Votes_Against :=
               State.Active_Proposals (Idx).Votes_Against + Weight;
         when Abstain =>
            State.Active_Proposals (Idx).Votes_Abstain :=
               State.Active_Proposals (Idx).Votes_Abstain + Weight;
      end case;

      State.Active_Proposals (Idx).Unique_Voters :=
         State.Active_Proposals (Idx).Unique_Voters + 1;
      State.Total_Votes_Cast := State.Total_Votes_Cast + Weight;

      Result := Voted;
   end Cast_Vote;

   procedure Delegate_Vote (
      State          : in Out Treasury_State;
      Delegator      : Byte_Array;
      Delegate       : Byte_Array;
      Signature      : Byte_Array;
      Success        : out Boolean
   ) is
      --  Minimum signature size for ML-DSA-87
      Min_Signature_Size : constant := 128;
   begin
      Success := False;

      --  1. Validate inputs
      if Delegator'Length < 32 or Delegate'Length < 32 then
         return;
      end if;

      --  2. Check delegator and delegate are different
      declare
         Same_Address : Boolean := True;
      begin
         for I in 0 .. 31 loop
            if Delegator (Delegator'First + I) /= Delegate (Delegate'First + I) then
               Same_Address := False;
               exit;
            end if;
         end loop;
         if Same_Address then
            --  Cannot delegate to yourself
            return;
         end if;
      end;

      --  3. Check delegate is not the zero address
      declare
         Zero_Delegate : Boolean := True;
      begin
         for I in 0 .. 31 loop
            if Delegate (Delegate'First + I) /= 0 then
               Zero_Delegate := False;
               exit;
            end if;
         end loop;
         if Zero_Delegate then
            --  Cannot delegate to zero address
            return;
         end if;
      end;

      --  4. Verify signature exists (actual verification would use ML-DSA)
      --  The signature should be over: hash(delegator || delegate || "DELEGATE")
      if Signature'Length < Min_Signature_Size then
         return;
      end if;

      --  5. Verify signature is not all zeros (sanity check)
      declare
         Zero_Sig : Boolean := True;
      begin
         for I in Signature'Range loop
            if Signature (I) /= 0 then
               Zero_Sig := False;
               exit;
            end if;
         end loop;
         if Zero_Sig then
            --  Invalid signature
            return;
         end if;
      end;

      --  NOTE: In a full implementation, this would:
      --  1. Verify the ML-DSA-87 signature using delegator's public key
      --  2. Store delegation in a separate delegation registry:
      --     Delegations : Delegation_Registry;
      --  3. Update voting power calculations to include delegated votes
      --  4. Handle delegation chains (A delegates to B, B delegates to C)
      --     with cycle detection
      --  5. Support delegation revocation
      --
      --  The delegation would be recorded as:
      --     Delegations.Add (Delegator, Delegate, State.Current_Block);
      --
      --  When Cast_Vote is called, the weight would be:
      --     Total_Weight := Own_Weight + Get_Delegated_Weight (Voter);
      --
      --  For now, we accept the delegation intent but note that
      --  actual delegation tracking requires additional infrastructure.

      --  Mark delegation as accepted
      --  (In production, would emit an event and store in registry)
      Success := True;
   end Delegate_Vote;

   function Quorum_Reached (
      Proposal       : Proposal_Record;
      Category       : Proposal_Category;
      Circulating    : Unsigned_64
   ) return Boolean is
      Total_Votes : Unsigned_64;
      Required : Unsigned_64;
   begin
      Total_Votes := Proposal.Votes_For + Proposal.Votes_Against + Proposal.Votes_Abstain;
      Required := Get_Required_Quorum (Category, Circulating);
      return Total_Votes >= Required;
   end Quorum_Reached;

   function Threshold_Reached (
      Proposal       : Proposal_Record;
      Category       : Proposal_Category
   ) return Boolean is
      Total_Votes : Unsigned_64;
      Required_BP : Natural;
      Required_Votes : Unsigned_64;
   begin
      Total_Votes := Proposal.Votes_For + Proposal.Votes_Against;
      if Total_Votes = 0 then
         return False;
      end if;

      Required_BP := Get_Required_Threshold (Category);
      Required_Votes := (Total_Votes * Unsigned_64 (Required_BP)) / 10_000;
      return Proposal.Votes_For >= Required_Votes;
   end Threshold_Reached;

   function Vote_Percentage (
      Votes_For      : Unsigned_64;
      Total_Votes    : Unsigned_64
   ) return Natural is
   begin
      return Natural ((Votes_For * 10_000) / Total_Votes);
   end Vote_Percentage;

   ---------------------------------------------------------------------------
   --  Proposal Finalization
   ---------------------------------------------------------------------------

   procedure Finalize_Voting (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
      Found : Boolean := False;
      Idx : Proposal_Index := 0;
   begin
      State.Current_Block := Current_Block;

      --  Find proposal
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Found := True;
            Idx := I;
            exit;
         end if;
      end loop;

      if not Found then
         Success := False;
         return;
      end if;

      --  Check voting period ended
      if Current_Block < State.Active_Proposals (Idx).Voting_End then
         Success := False;
         return;
      end if;

      --  Check quorum
      if not Quorum_Reached (State.Active_Proposals (Idx),
                             State.Active_Proposals (Idx).Category,
                             State.Circulating_Supply) then
         State.Active_Proposals (Idx).State := Expired;
         State.Proposals_Failed := State.Proposals_Failed + 1;
         Success := False;
         return;
      end if;

      --  Check threshold
      if Threshold_Reached (State.Active_Proposals (Idx),
                           State.Active_Proposals (Idx).Category) then
         State.Active_Proposals (Idx).State := Passed;
         State.Proposals_Passed := State.Proposals_Passed + 1;
         Success := True;
      else
         State.Active_Proposals (Idx).State := Rejected;
         State.Proposals_Failed := State.Proposals_Failed + 1;
         Success := False;
      end if;
   end Finalize_Voting;

   procedure Execute_Proposal (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Current_Block  : Unsigned_64;
      Result         : out Execute_Result
   ) is
      Found : Boolean := False;
      Idx : Proposal_Index := 0;
   begin
      State.Current_Block := Current_Block;

      --  Find proposal
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Found := True;
            Idx := I;
            exit;
         end if;
      end loop;

      if not Found then
         Result := Not_Passed;
         return;
      end if;

      --  Check proposal is passed
      if State.Active_Proposals (Idx).State /= Passed then
         Result := Not_Passed;
         return;
      end if;

      --  Check timelock
      if Current_Block < State.Active_Proposals (Idx).Timelock_End then
         Result := Timelock_Active;
         return;
      end if;

      --  Check already executed
      if State.Active_Proposals (Idx).Executed_At > 0 then
         Result := Already_Executed;
         return;
      end if;

      --  Check treasury balance for spending proposals
      if State.Active_Proposals (Idx).Prop_Type = Spending then
         if State.Active_Proposals (Idx).Request_Amount > State.Current_Balance then
            Result := Insufficient_Treasury;
            return;
         end if;

         --  Execute transfer
         State.Current_Balance :=
            State.Current_Balance - State.Active_Proposals (Idx).Request_Amount;
         State.Total_Spent :=
            State.Total_Spent + State.Active_Proposals (Idx).Request_Amount;
      end if;

      State.Active_Proposals (Idx).State := Executed;
      State.Active_Proposals (Idx).Executed_At := Current_Block;

      if State.Active_Count > 0 then
         State.Active_Count := State.Active_Count - 1;
      end if;

      Result := Executed;
   end Execute_Proposal;

   procedure Cancel_Proposal (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Canceller      : Byte_Array;
      Signature      : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Canceller, Signature);
      Found : Boolean := False;
      Idx : Proposal_Index := 0;
   begin
      --  Find proposal
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Found := True;
            Idx := I;
            exit;
         end if;
      end loop;

      if not Found then
         Success := False;
         return;
      end if;

      --  Can only cancel if draft or active
      if State.Active_Proposals (Idx).State /= Draft and then
         State.Active_Proposals (Idx).State /= Active
      then
         Success := False;
         return;
      end if;

      State.Active_Proposals (Idx).State := Cancelled;

      if State.Active_Count > 0 then
         State.Active_Count := State.Active_Count - 1;
      end if;

      Success := True;
   end Cancel_Proposal;

   procedure Return_Bond (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Success        : out Boolean
   ) is
      Found : Boolean := False;
      Idx : Proposal_Index := 0;
   begin
      Success := False;

      --  Find proposal by ID
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Found := True;
            Idx := I;
            exit;
         end if;
      end loop;

      if not Found then
         return;
      end if;

      --  Check if bond was already returned
      if State.Active_Proposals (Idx).Bond_Returned then
         return;
      end if;

      --  Bond can only be returned if:
      --  1. Proposal passed (executed or in timelock)
      --  2. Proposal was cancelled by the proposer before voting started
      --  3. Proposal was rejected but proposer wasn't malicious
      --
      --  Bond is burned if:
      --  1. Proposal failed to meet quorum (expired)
      --  2. Proposal was vetoed for malicious intent
      case State.Active_Proposals (Idx).State is
         when Passed | Executed =>
            --  Proposal succeeded - return bond to proposer
            State.Active_Proposals (Idx).Bond_Returned := True;
            Success := True;

         when Rejected =>
            --  Proposal failed but was legitimate - return bond
            --  (In some systems, bonds are kept even for rejected proposals
            --  to prevent spam. Here we return them for good-faith proposals.)
            State.Active_Proposals (Idx).Bond_Returned := True;
            Success := True;

         when Cancelled =>
            --  Only return bond if cancelled before voting ended
            --  (Prevents gaming by cancelling losing proposals)
            if State.Active_Proposals (Idx).Voting_End = 0 or
               State.Current_Block < State.Active_Proposals (Idx).Voting_Start
            then
               --  Cancelled before voting started - return bond
               State.Active_Proposals (Idx).Bond_Returned := True;
               Success := True;
            else
               --  Cancelled during/after voting - bond forfeited
               Success := False;
            end if;

         when Expired | Vetoed =>
            --  Failed to reach quorum or was vetoed - bond is burned
            --  This prevents spam proposals and malicious actors
            Success := False;

         when Draft | Active =>
            --  Proposal still in progress - cannot return bond yet
            Success := False;
      end case;
   end Return_Bond;

   procedure Burn_Bond (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Success        : out Boolean
   ) is
      Found : Boolean := False;
      Idx : Proposal_Index := 0;
   begin
      --  Find proposal
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Found := True;
            Idx := I;
            exit;
         end if;
      end loop;

      if not Found then
         Success := False;
         return;
      end if;

      State.Total_Burned := State.Total_Burned +
         State.Active_Proposals (Idx).Bond_Amount;
      Success := True;
   end Burn_Bond;

   ---------------------------------------------------------------------------
   --  Emergency Actions
   ---------------------------------------------------------------------------

   procedure Emergency_Veto (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Council_Sigs   : Byte_Array;
      Reason_Hash    : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Council_Sigs, Reason_Hash);
      Found : Boolean := False;
      Idx : Proposal_Index := 0;
   begin
      --  Find proposal
      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Found := True;
            Idx := I;
            exit;
         end if;
      end loop;

      if not Found then
         Success := False;
         return;
      end if;

      State.Active_Proposals (Idx).State := Vetoed;

      if State.Active_Count > 0 then
         State.Active_Count := State.Active_Count - 1;
      end if;

      Success := True;
   end Emergency_Veto;

   procedure Emergency_Release (
      State          : in Out Treasury_State;
      Recipient      : Byte_Array;
      Amount         : Unsigned_64;
      Council_Sigs   : Byte_Array;
      Reason_Hash    : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Recipient, Council_Sigs, Reason_Hash);
   begin
      if Amount > State.Current_Balance then
         Success := False;
         return;
      end if;

      State.Current_Balance := State.Current_Balance - Amount;
      State.Total_Spent := State.Total_Spent + Amount;
      Success := True;
   end Emergency_Release;

   ---------------------------------------------------------------------------
   --  Treasury Queries
   ---------------------------------------------------------------------------

   function Get_Balance (State : Treasury_State) return Unsigned_64 is
   begin
      return State.Current_Balance;
   end Get_Balance;

   function Get_Total_Spent (State : Treasury_State) return Unsigned_64 is
   begin
      return State.Total_Spent;
   end Get_Total_Spent;

   function Get_Active_Count (State : Treasury_State) return Natural is
   begin
      return State.Active_Count;
   end Get_Active_Count;

   procedure Get_Proposal (
      State          : Treasury_State;
      Proposal_ID    : Unsigned_64;
      Proposal       : out Proposal_Record;
      Found          : out Boolean
   ) is
   begin
      Found := False;
      Proposal := (
         ID             => 0,
         Category       => Small_Grant,
         Prop_Type      => Spending,
         State          => Draft,
         Proposer       => (others => 0),
         Bond_Amount    => 0,
         Bond_Returned  => False,
         Title_Hash     => (others => 0),
         Description_Hash => (others => 0),
         IPFS_Link      => (others => 0),
         Recipient      => (others => 0),
         Request_Amount => 0,
         Vesting_Period => 0,
         Votes_For      => 0,
         Votes_Against  => 0,
         Votes_Abstain  => 0,
         Unique_Voters  => 0,
         Created_At     => 0,
         Voting_Start   => 0,
         Voting_End     => 0,
         Timelock_End   => 0,
         Executed_At    => 0
      );

      for I in Proposal_Index loop
         if State.Active_Proposals (I).ID = Proposal_ID then
            Proposal := State.Active_Proposals (I);
            Found := True;
            return;
         end if;
      end loop;
   end Get_Proposal;

   function Is_Proposer (
      Proposal       : Proposal_Record;
      Address        : Byte_Array
   ) return Boolean is
      Match : Boolean := True;
   begin
      for I in Address'Range loop
         if I - Address'First <= Proposal.Proposer'Last then
            if Address (I) /= Proposal.Proposer (I - Address'First) then
               Match := False;
               exit;
            end if;
         end if;
      end loop;
      return Match;
   end Is_Proposer;

   ---------------------------------------------------------------------------
   --  Builder Access Prevention
   ---------------------------------------------------------------------------

   function Is_Builder_Address (
      Address        : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean is
   begin
      for I in Address'Range loop
         if I - Address'First <= Builder_PK'Last then
            if Address (I) /= Builder_PK (I - Address'First) then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Builder_Address;

   function Verify_Non_Builder (
      Recipient      : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean is
   begin
      return not Is_Builder_Address (Recipient, Builder_PK);
   end Verify_Non_Builder;

   ---------------------------------------------------------------------------
   --  Supply Tracking
   ---------------------------------------------------------------------------

   procedure Update_Circulating_Supply (
      State          : in Out Treasury_State;
      New_Supply     : Unsigned_64
   ) is
   begin
      State.Circulating_Supply := New_Supply;
   end Update_Circulating_Supply;

   function Available_Spend_Capacity (
      State          : Treasury_State
   ) return Unsigned_64 is
   begin
      return (State.Current_Balance * Max_Spend_Per_Proposal_BP) / 10_000;
   end Available_Spend_Capacity;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Treasury_State (
      State          : Treasury_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Output'First;

      procedure Write_U64 (Val : Unsigned_64) is
         V : Unsigned_64 := Val;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U64;

   begin
      Output := (others => 0);

      Write_U64 (State.Total_Allocation);
      Write_U64 (State.Current_Balance);
      Write_U64 (State.Total_Spent);
      Write_U64 (State.Total_Burned);

      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Active_Count);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Total_Proposals);
      Write_U64 (State.Proposals_Passed);
      Write_U64 (State.Proposals_Failed);
      Write_U64 (State.Total_Votes_Cast);
      Write_U64 (State.Unique_Voters);
      Write_U64 (State.Circulating_Supply);
      Write_U64 (State.Genesis_Block);
      Write_U64 (State.Current_Block);

      Length := Idx - Output'First;
   end Serialize_Treasury_State;

   procedure Deserialize_Treasury_State (
      Input          : Byte_Array;
      State          : out Treasury_State;
      Success        : out Boolean
   ) is
      Idx : Natural := Input'First;

      procedure Read_U64 (Val : out Unsigned_64) is
      begin
         Val := 0;
         for J in 0 .. 7 loop
            if Idx <= Input'Last then
               Val := Val + Unsigned_64 (Input (Idx)) * (256 ** J);
               Idx := Idx + 1;
            end if;
         end loop;
      end Read_U64;

   begin
      Init_Treasury (State, 0);
      Success := False;

      if Input'Length < 100 then
         return;
      end if;

      Read_U64 (State.Total_Allocation);
      Read_U64 (State.Current_Balance);
      Read_U64 (State.Total_Spent);
      Read_U64 (State.Total_Burned);

      if Idx <= Input'Last then
         State.Active_Count := Natural (Input (Idx));
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Total_Proposals);
      Read_U64 (State.Proposals_Passed);
      Read_U64 (State.Proposals_Failed);
      Read_U64 (State.Total_Votes_Cast);
      Read_U64 (State.Unique_Voters);
      Read_U64 (State.Circulating_Supply);
      Read_U64 (State.Genesis_Block);
      Read_U64 (State.Current_Block);

      Success := True;
   end Deserialize_Treasury_State;

   procedure Serialize_Proposal (
      Proposal       : Proposal_Record;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Output'First;

      procedure Write_U64 (Val : Unsigned_64) is
         V : Unsigned_64 := Val;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U64;

   begin
      Output := (others => 0);

      Write_U64 (Proposal.ID);

      if Idx <= Output'Last then
         Output (Idx) := Proposal_Category'Pos (Proposal.Category);
         Idx := Idx + 1;
      end if;

      if Idx <= Output'Last then
         Output (Idx) := Proposal_Type'Pos (Proposal.Prop_Type);
         Idx := Idx + 1;
      end if;

      if Idx <= Output'Last then
         Output (Idx) := Proposal_State'Pos (Proposal.State);
         Idx := Idx + 1;
      end if;

      --  Copy proposer
      for I in Proposal.Proposer'Range loop
         if Idx <= Output'Last then
            Output (Idx) := Proposal.Proposer (I);
            Idx := Idx + 1;
         end if;
      end loop;

      Write_U64 (Proposal.Bond_Amount);
      Write_U64 (Proposal.Request_Amount);
      Write_U64 (Proposal.Votes_For);
      Write_U64 (Proposal.Votes_Against);
      Write_U64 (Proposal.Created_At);
      Write_U64 (Proposal.Voting_End);
      Write_U64 (Proposal.Timelock_End);
      Write_U64 (Proposal.Executed_At);

      Length := Idx - Output'First;
   end Serialize_Proposal;

   procedure Deserialize_Proposal (
      Input          : Byte_Array;
      Proposal       : out Proposal_Record;
      Success        : out Boolean
   ) is
      Idx : Natural := Input'First;

      procedure Read_U64 (Val : out Unsigned_64) is
      begin
         Val := 0;
         for J in 0 .. 7 loop
            if Idx <= Input'Last then
               Val := Val + Unsigned_64 (Input (Idx)) * (256 ** J);
               Idx := Idx + 1;
            end if;
         end loop;
      end Read_U64;

   begin
      --  Initialize
      Proposal := (
         ID             => 0,
         Category       => Small_Grant,
         Prop_Type      => Spending,
         State          => Draft,
         Proposer       => (others => 0),
         Bond_Amount    => 0,
         Bond_Returned  => False,
         Title_Hash     => (others => 0),
         Description_Hash => (others => 0),
         IPFS_Link      => (others => 0),
         Recipient      => (others => 0),
         Request_Amount => 0,
         Vesting_Period => 0,
         Votes_For      => 0,
         Votes_Against  => 0,
         Votes_Abstain  => 0,
         Unique_Voters  => 0,
         Created_At     => 0,
         Voting_Start   => 0,
         Voting_End     => 0,
         Timelock_End   => 0,
         Executed_At    => 0
      );
      Success := False;

      if Input'Length < 100 then
         return;
      end if;

      Read_U64 (Proposal.ID);

      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Proposal_Category'Pos (Proposal_Category'Last) then
            Proposal.Category := Proposal_Category'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Proposal_Type'Pos (Proposal_Type'Last) then
            Proposal.Prop_Type := Proposal_Type'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Proposal_State'Pos (Proposal_State'Last) then
            Proposal.State := Proposal_State'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      --  Read proposer
      for I in Proposal.Proposer'Range loop
         if Idx <= Input'Last then
            Proposal.Proposer (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      Read_U64 (Proposal.Bond_Amount);
      Read_U64 (Proposal.Request_Amount);
      Read_U64 (Proposal.Votes_For);
      Read_U64 (Proposal.Votes_Against);
      Read_U64 (Proposal.Created_At);
      Read_U64 (Proposal.Voting_End);
      Read_U64 (Proposal.Timelock_End);
      Read_U64 (Proposal.Executed_At);

      Success := True;
   end Deserialize_Proposal;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Treasury_State (State : out Treasury_State) is
   begin
      State.Total_Allocation := 0;
      State.Current_Balance := 0;
      State.Total_Spent := 0;
      State.Total_Burned := 0;

      for I in Proposal_Index loop
         Zeroize_Proposal (State.Active_Proposals (I));
      end loop;

      State.Active_Count := 0;
      State.Total_Proposals := 0;
      State.Proposals_Passed := 0;
      State.Proposals_Failed := 0;
      State.Total_Votes_Cast := 0;
      State.Unique_Voters := 0;
      State.Circulating_Supply := 0;
      State.Genesis_Block := 0;
      State.Current_Block := 0;
   end Zeroize_Treasury_State;

   procedure Zeroize_Proposal (Proposal : out Proposal_Record) is
   begin
      Proposal.ID := 0;
      Proposal.Category := Small_Grant;
      Proposal.Prop_Type := Spending;
      Proposal.State := Draft;
      Proposal.Proposer := (others => 0);
      Proposal.Bond_Amount := 0;
      Proposal.Bond_Returned := False;
      Proposal.Title_Hash := (others => 0);
      Proposal.Description_Hash := (others => 0);
      Proposal.IPFS_Link := (others => 0);
      Proposal.Recipient := (others => 0);
      Proposal.Request_Amount := 0;
      Proposal.Vesting_Period := 0;
      Proposal.Votes_For := 0;
      Proposal.Votes_Against := 0;
      Proposal.Votes_Abstain := 0;
      Proposal.Unique_Voters := 0;
      Proposal.Created_At := 0;
      Proposal.Voting_Start := 0;
      Proposal.Voting_End := 0;
      Proposal.Timelock_End := 0;
      Proposal.Executed_At := 0;
   end Zeroize_Proposal;

   procedure Zeroize_Vote (Vote : out Vote_Record) is
   begin
      Vote.Voter := (others => 0);
      Vote.Proposal_ID := 0;
      Vote.Choice := For_Proposal;
      Vote.Weight := 0;
      Vote.Delegated := False;
      Vote.Delegate_From := (others => 0);
      Vote.Timestamp := 0;
      Vote.Signature := (others => 0);
   end Zeroize_Vote;

end Anubis_Treasury;
