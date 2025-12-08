-------------------------------------------------------------------------------
--  ANUBIS Developer Ecosystem Code Mining
--  7% Allocation (70M ANUBIS) Earned via Verified Commits
--
--  Developers earn ANUBIS by contributing verified code to the ecosystem.
--  All rewards are based on GNATprove verification and community review.
--
--  Reward Scale:
--  - Core Protocol PR:       1,000 - 50,000 ANUBIS (GNATprove + review)
--  - KHEPRI Contract Template: 500 - 10,000 ANUBIS (Full SPARK proof)
--  - SDK/Tooling:              200 - 5,000  ANUBIS (Test coverage + review)
--  - Documentation:             50 - 500    ANUBIS (Community vote)
--  - Tutorial/Education:       100 - 1,000  ANUBIS (Community vote)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Developer_Rewards with
   SPARK_Mode => On,
   Abstract_State => Counter_State,
   Initializes => Counter_State
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Developer_Allocation       : constant := 70_000_000;  -- 7% of 1B

   --  Contribution types and reward ranges
   type Contribution_Type is (
      Core_Protocol_PR,
      KHEPRI_Contract,
      SDK_Tooling,
      Documentation,
      Tutorial_Education,
      Bug_Report,
      Security_Audit
   );

   --  Reward ranges (min, max) in raw tokens
   type Reward_Range is record
      Min_Reward : Unsigned_64;
      Max_Reward : Unsigned_64;
   end record;

   Contribution_Rewards : constant array (Contribution_Type) of Reward_Range := (
      Core_Protocol_PR    => (1_000, 50_000),
      KHEPRI_Contract     => (500, 10_000),
      SDK_Tooling         => (200, 5_000),
      Documentation       => (50, 500),
      Tutorial_Education  => (100, 1_000),
      Bug_Report          => (100, 5_000),
      Security_Audit      => (1_000, 25_000)
   );

   --  Verification requirements
   type Verification_Type is (
      GNATprove_Review,    -- GNATprove + builder review
      Full_SPARK_Proof,    -- Complete SPARK verification
      Test_Coverage_Review, -- Test coverage + community review
      Community_Vote,      -- Community vote only
      Audit_Verification   -- External audit
   );

   Required_Verification : constant array (Contribution_Type) of Verification_Type := (
      Core_Protocol_PR    => GNATprove_Review,
      KHEPRI_Contract     => Full_SPARK_Proof,
      SDK_Tooling         => Test_Coverage_Review,
      Documentation       => Community_Vote,
      Tutorial_Education  => Community_Vote,
      Bug_Report          => Community_Vote,
      Security_Audit      => Audit_Verification
   );

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Contribution_Status is (
      Submitted,
      Under_Review,
      Verification_Pending,
      Approved,
      Rejected,
      Paid
   );

   type Contribution_Record is record
      ID                 : Unsigned_64;
      Contributor        : Byte_Array (0 .. 31);
      Cont_Type          : Contribution_Type;
      Status             : Contribution_Status;

      --  Content references
      Commit_Hash        : Byte_Array (0 .. 31);
      Proof_Hash         : Byte_Array (0 .. 31);
      PR_Link_Hash       : Byte_Array (0 .. 31);
      IPFS_Link          : Byte_Array (0 .. 45);

      --  Metrics
      Lines_Added        : Unsigned_32;
      Lines_Removed      : Unsigned_32;
      VCs_Proven         : Unsigned_32;
      Test_Coverage_Pct  : Natural;

      --  Reward
      Proposed_Reward    : Unsigned_64;
      Final_Reward       : Unsigned_64;
      Votes_For          : Unsigned_32;
      Votes_Against      : Unsigned_32;

      --  Timing
      Submitted_At       : Unsigned_64;
      Reviewed_At        : Unsigned_64;
      Paid_At            : Unsigned_64;
   end record;

   type Contribution_Array is array (Natural range <>) of Contribution_Record;

   type Developer_Ecosystem_State is record
      Total_Distributed  : Unsigned_64;
      Remaining_Pool     : Unsigned_64;
      Total_Contributions: Unsigned_64;
      Active_Contributors: Unsigned_32;
      Genesis_Block      : Unsigned_64;
      Current_Block      : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Submit_Result is (
      Submitted,
      Invalid_Type,
      Missing_Proof,
      Duplicate_Commit,
      Pool_Exhausted
   );

   type Review_Result is (
      Approved,
      Rejected,
      Needs_Revision,
      Escalated
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Developer_Ecosystem (
      State          : out Developer_Ecosystem_State;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Post => State.Remaining_Pool = Developer_Allocation
              and State.Total_Distributed = 0;

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
   ) with
      Global => (In_Out => Counter_State),
      Pre => Contributor'Length = 32
             and Commit_Hash'Length = 32
             and Proof_Hash'Length = 32;

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
   ) with
      Global => null,
      Pre => Reviewer'Length = 32
             and Notes_Hash'Length = 32;

   procedure Community_Vote (
      Contribution   : in Out Contribution_Record;
      Voter          : Byte_Array;
      Vote_For       : Boolean;
      Weight         : Unsigned_32
   ) with
      Global => null,
      Pre => Voter'Length = 32
             and Weight > 0;

   function Calculate_Reward (
      Cont_Type      : Contribution_Type;
      Lines_Added    : Unsigned_32;
      VCs_Proven     : Unsigned_32;
      Test_Coverage  : Natural
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Reward Distribution
   ---------------------------------------------------------------------------

   procedure Distribute_Reward (
      State          : in Out Developer_Ecosystem_State;
      Contribution   : in Out Contribution_Record;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Contribution.Status = Approved
             and Contribution.Final_Reward > 0;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Remaining_Pool (
      State          : Developer_Ecosystem_State
   ) return Unsigned_64 with
      Global => null;

   function Get_Reward_Range (
      Cont_Type      : Contribution_Type
   ) return Reward_Range with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Ecosystem_State (State : out Developer_Ecosystem_State) with
      Global => null;

   procedure Zeroize_Contribution (C : out Contribution_Record) with
      Global => null;

end Anubis_Developer_Rewards;
