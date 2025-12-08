-------------------------------------------------------------------------------
--  ANUBIS Developer Ecosystem Rewards Implementation Body
--  7% Allocation (70M ANUBIS) for Code Mining
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Developer_Rewards with
   SPARK_Mode => On,
   Refined_State => (Counter_State => Contribution_Counter)
is
   --  Contribution counter
   Contribution_Counter : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Developer_Ecosystem (
      State          : out Developer_Ecosystem_State;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      State.Total_Distributed := 0;
      State.Remaining_Pool := Developer_Allocation;
      State.Total_Contributions := 0;
      State.Active_Contributors := 0;
      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
   end Init_Developer_Ecosystem;

   ---------------------------------------------------------------------------
   --  Contribution Submission
   ---------------------------------------------------------------------------

   procedure Submit_Contribution (
      State          : in out Developer_Ecosystem_State;
      Contributor    : Byte_Array;
      Cont_Type      : Contribution_Type;
      Commit_Hash    : Byte_Array;
      Proof_Hash     : Byte_Array;
      Lines_Added    : Unsigned_32;
      VCs_Proven     : Unsigned_32;
      Contribution   : out Contribution_Record;
      Result         : out Submit_Result
   ) is
   begin
      --  Check pool not exhausted
      if State.Remaining_Pool = 0 then
         Result := Pool_Exhausted;
         Contribution := (
            ID             => 0,
            Contributor    => (others => 0),
            Cont_Type      => Core_Protocol_PR,
            Status         => Rejected,
            Commit_Hash    => (others => 0),
            Proof_Hash     => (others => 0),
            PR_Link_Hash   => (others => 0),
            IPFS_Link      => (others => 0),
            Lines_Added    => 0,
            Lines_Removed  => 0,
            VCs_Proven     => 0,
            Test_Coverage_Pct => 0,
            Proposed_Reward => 0,
            Final_Reward    => 0,
            Votes_For       => 0,
            Votes_Against   => 0,
            Submitted_At    => 0,
            Reviewed_At     => 0,
            Paid_At         => 0
         );
         return;
      end if;

      --  Create contribution
      Contribution_Counter := Contribution_Counter + 1;

      Contribution.ID := Contribution_Counter;

      --  Copy contributor address
      Contribution.Contributor := (others => 0);
      for I in Contributor'Range loop
         if I - Contributor'First <= Contribution.Contributor'Last then
            Contribution.Contributor (I - Contributor'First) :=
               Contributor (I);
         end if;
      end loop;

      Contribution.Cont_Type := Cont_Type;
      Contribution.Status := Submitted;

      --  Copy commit hash
      Contribution.Commit_Hash := (others => 0);
      for I in Commit_Hash'Range loop
         if I - Commit_Hash'First <= Contribution.Commit_Hash'Last then
            Contribution.Commit_Hash (I - Commit_Hash'First) :=
               Commit_Hash (I);
         end if;
      end loop;

      --  Copy proof hash
      Contribution.Proof_Hash := (others => 0);
      for I in Proof_Hash'Range loop
         if I - Proof_Hash'First <= Contribution.Proof_Hash'Last then
            Contribution.Proof_Hash (I - Proof_Hash'First) :=
               Proof_Hash (I);
         end if;
      end loop;

      Contribution.PR_Link_Hash := (others => 0);
      Contribution.IPFS_Link := (others => 0);
      Contribution.Lines_Added := Lines_Added;
      Contribution.Lines_Removed := 0;
      Contribution.VCs_Proven := VCs_Proven;
      Contribution.Test_Coverage_Pct := 0;

      --  Calculate proposed reward
      Contribution.Proposed_Reward := Calculate_Reward (
         Cont_Type, Lines_Added, VCs_Proven, 0
      );
      Contribution.Final_Reward := 0;
      Contribution.Votes_For := 0;
      Contribution.Votes_Against := 0;
      Contribution.Submitted_At := State.Current_Block;
      Contribution.Reviewed_At := 0;
      Contribution.Paid_At := 0;

      State.Total_Contributions := State.Total_Contributions + 1;

      Result := Submit_Result'(Submitted);
   end Submit_Contribution;

   ---------------------------------------------------------------------------
   --  Review and Verification
   ---------------------------------------------------------------------------

   procedure Review_Contribution (
      Contribution   : in Out Contribution_Record;
      Reviewer       : Byte_Array;
      Approved       : Boolean;
      Final_Reward   : Unsigned_64;
      Notes_Hash     : Byte_Array;
      Result         : out Review_Result
   ) is
      pragma Unreferenced (Reviewer, Notes_Hash);
   begin
      --  Check contribution is under review
      if Contribution.Status /= Submitted and then
         Contribution.Status /= Under_Review
      then
         Result := Rejected;
         return;
      end if;

      if Approved then
         Contribution.Status := Contribution_Status'(Contribution_Status'(Anubis_Developer_Rewards.Approved));
         Contribution.Final_Reward := Final_Reward;
         Result := Review_Result'(Anubis_Developer_Rewards.Approved);
      else
         Contribution.Status := Anubis_Developer_Rewards.Rejected;
         Contribution.Final_Reward := 0;
         Result := Anubis_Developer_Rewards.Rejected;
      end if;
   end Review_Contribution;

   procedure Community_Vote (
      Contribution   : in Out Contribution_Record;
      Voter          : Byte_Array;
      Vote_For       : Boolean;
      Weight         : Unsigned_32
   ) is
      pragma Unreferenced (Voter);
   begin
      if Vote_For then
         Contribution.Votes_For := Contribution.Votes_For + Weight;
      else
         Contribution.Votes_Against := Contribution.Votes_Against + Weight;
      end if;
   end Community_Vote;

   function Calculate_Reward (
      Cont_Type      : Contribution_Type;
      Lines_Added    : Unsigned_32;
      VCs_Proven     : Unsigned_32;
      Test_Coverage  : Natural
   ) return Unsigned_64 is
      Base_Min : constant Unsigned_64 :=
         Contribution_Rewards (Cont_Type).Min_Reward;
      Base_Max : constant Unsigned_64 :=
         Contribution_Rewards (Cont_Type).Max_Reward;
      Range_Size : Unsigned_64;
      Score_Factor : Unsigned_64;
      Reward : Unsigned_64;

      --  Use Unsigned_64 to avoid overflow in intermediate calculations
      Lines_Score : constant Unsigned_64 := Unsigned_64 (Lines_Added) / 10;
      VCs_Score   : constant Unsigned_64 := Unsigned_64 (VCs_Proven);
      Test_Score  : constant Unsigned_64 := Unsigned_64 (Test_Coverage);
   begin
      --  Calculate range
      Range_Size := Base_Max - Base_Min;

      --  Calculate score factor (average of metrics, 0-100)
      --  Using Unsigned_64 throughout to avoid overflow
      Score_Factor := (Lines_Score + VCs_Score + Test_Score) / 3;

      --  Cap at 100
      if Score_Factor > 100 then
         Score_Factor := 100;
      end if;

      --  Calculate reward: min + (range * score / 100)
      Reward := Base_Min + (Range_Size * Score_Factor) / 100;

      --  Cap at max
      if Reward > Base_Max then
         Reward := Base_Max;
      end if;

      return Reward;
   end Calculate_Reward;

   ---------------------------------------------------------------------------
   --  Reward Distribution
   ---------------------------------------------------------------------------

   procedure Distribute_Reward (
      State          : in Out Developer_Ecosystem_State;
      Contribution   : in Out Contribution_Record;
      Success        : out Boolean
   ) is
   begin
      --  Check contribution is approved
      if Contribution.Status /= Contribution_Status'(Approved) then
         Success := False;
         return;
      end if;

      --  Check sufficient pool
      if Contribution.Final_Reward > State.Remaining_Pool then
         Success := False;
         return;
      end if;

      --  Distribute
      State.Remaining_Pool := State.Remaining_Pool - Contribution.Final_Reward;
      State.Total_Distributed := State.Total_Distributed +
         Contribution.Final_Reward;

      Contribution.Status := Paid;
      Contribution.Paid_At := State.Current_Block;

      Success := True;
   end Distribute_Reward;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Remaining_Pool (
      State          : Developer_Ecosystem_State
   ) return Unsigned_64 is
   begin
      return State.Remaining_Pool;
   end Get_Remaining_Pool;

   function Get_Reward_Range (
      Cont_Type      : Contribution_Type
   ) return Reward_Range is
   begin
      return Contribution_Rewards (Cont_Type);
   end Get_Reward_Range;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Ecosystem_State (State : out Developer_Ecosystem_State) is
   begin
      State.Total_Distributed := 0;
      State.Remaining_Pool := 0;
      State.Total_Contributions := 0;
      State.Active_Contributors := 0;
      State.Genesis_Block := 0;
      State.Current_Block := 0;
   end Zeroize_Ecosystem_State;

   procedure Zeroize_Contribution (C : out Contribution_Record) is
   begin
      C.ID := 0;
      C.Contributor := (others => 0);
      C.Cont_Type := Core_Protocol_PR;
      C.Status := Rejected;
      C.Commit_Hash := (others => 0);
      C.Proof_Hash := (others => 0);
      C.PR_Link_Hash := (others => 0);
      C.IPFS_Link := (others => 0);
      C.Lines_Added := 0;
      C.Lines_Removed := 0;
      C.VCs_Proven := 0;
      C.Test_Coverage_Pct := 0;
      C.Proposed_Reward := 0;
      C.Final_Reward := 0;
      C.Votes_For := 0;
      C.Votes_Against := 0;
      C.Submitted_At := 0;
      C.Reviewed_At := 0;
      C.Paid_At := 0;
   end Zeroize_Contribution;

end Anubis_Developer_Rewards;
