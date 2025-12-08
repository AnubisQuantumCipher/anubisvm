-------------------------------------------------------------------------------
--  ANUBIS Protocol Treasury
--  30% DAO-Controlled Treasury with Zero Builder Access
--
--  CRITICAL: Builder has ZERO access to the treasury from day 1.
--  The 30% treasury is controlled by DAO governance.
--
--  Treasury Parameters:
--  - Allocation: 300,000,000 ANUBIS (30%)
--  - Builder Access: NONE — zero tokens, zero control
--  - Control: DAO governance from block 0
--  - Spend Cap: 2% per proposal
--  - Quorum: 10% of circulating supply
--  - Approval: 60% majority
--
--  Proposal Types and Requirements:
--  - Small Grant (<0.5%):   Bond 5,000   | Quorum 7%  | Threshold 55% | 7 days
--  - Medium Grant (0.5-1%): Bond 15,000  | Quorum 10% | Threshold 60% | 14 days
--  - Large Grant (1-2%):    Bond 30,000  | Quorum 12% | Threshold 65% | 21 days
--  - Strategic (>2%):       Constitutional proposal required
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Treasury with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Treasury allocation
   Treasury_Allocation        : constant := 300_000_000;  -- 30% of 1B

   --  Spending limits (basis points of treasury)
   Max_Spend_Per_Proposal_BP  : constant := 200;   -- 2% max per proposal
   Strategic_Threshold_BP     : constant := 200;   -- >2% requires constitutional

   --  Quorum requirements (basis points of circulating supply)
   Small_Quorum_BP            : constant := 700;   -- 7%
   Medium_Quorum_BP           : constant := 1000;  -- 10%
   Large_Quorum_BP            : constant := 1200;  -- 12%
   Constitutional_Quorum_BP   : constant := 2000;  -- 20%

   --  Approval thresholds (basis points)
   Small_Threshold_BP         : constant := 5500;  -- 55%
   Medium_Threshold_BP        : constant := 6000;  -- 60%
   Large_Threshold_BP         : constant := 6500;  -- 65%
   Constitutional_Threshold_BP: constant := 7500;  -- 75%

   --  Bond requirements (in raw tokens)
   Small_Bond                 : constant := 5_000;
   Medium_Bond                : constant := 15_000;
   Large_Bond                 : constant := 30_000;
   Constitutional_Bond        : constant := 100_000;

   --  Timelock periods (in blocks, 6-second blocks)
   Small_Timelock_Blocks      : constant := 100_800;   -- 7 days
   Medium_Timelock_Blocks     : constant := 201_600;   -- 14 days
   Large_Timelock_Blocks      : constant := 302_400;   -- 21 days
   Constitutional_Timelock_Blocks : constant := 604_800; -- 42 days

   --  Voting periods (in blocks)
   Small_Voting_Blocks        : constant := 72_000;    -- 5 days
   Medium_Voting_Blocks       : constant := 100_800;   -- 7 days
   Large_Voting_Blocks        : constant := 144_000;   -- 10 days
   Constitutional_Voting_Blocks : constant := 201_600; -- 14 days

   --  Maximum concurrent proposals
   Max_Active_Proposals       : constant := 10;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Proposal categories
   type Proposal_Category is (
      Small_Grant,        -- <0.5% of treasury
      Medium_Grant,       -- 0.5-1% of treasury
      Large_Grant,        -- 1-2% of treasury
      Constitutional      -- >2% or fundamental changes
   );

   --  Proposal types
   type Proposal_Type is (
      Spending,           -- Request treasury funds
      Parameter_Change,   -- Modify protocol parameters
      Upgrade,            -- Protocol upgrade
      Emergency,          -- Emergency action
      Constitutional_Amendment  -- Change governance rules
   );

   --  Proposal states
   type Proposal_State is (
      Draft,              -- Being prepared
      Active,             -- Open for voting
      Passed,             -- Approved, in timelock
      Rejected,           -- Did not meet threshold
      Executed,           -- Funds transferred
      Cancelled,          -- Cancelled by proposer
      Expired,            -- Voting period ended without quorum
      Vetoed              -- Vetoed by council (emergency only)
   );

   subtype Proposal_Index is Natural range 0 .. Max_Active_Proposals - 1;

   ---------------------------------------------------------------------------
   --  Proposal Record
   ---------------------------------------------------------------------------

   type Proposal_Record is record
      ID               : Unsigned_64;
      Category         : Proposal_Category;
      Prop_Type        : Proposal_Type;
      State            : Proposal_State;

      --  Proposer info
      Proposer         : Byte_Array (0 .. 31);
      Bond_Amount      : Unsigned_64;
      Bond_Returned    : Boolean;

      --  Proposal details
      Title_Hash       : Byte_Array (0 .. 31);
      Description_Hash : Byte_Array (0 .. 31);
      IPFS_Link        : Byte_Array (0 .. 45);

      --  Spending details (if applicable)
      Recipient        : Byte_Array (0 .. 31);
      Request_Amount   : Unsigned_64;
      Vesting_Period   : Unsigned_64;  -- Optional vesting for recipient

      --  Voting
      Votes_For        : Unsigned_64;
      Votes_Against    : Unsigned_64;
      Votes_Abstain    : Unsigned_64;
      Unique_Voters    : Unsigned_32;

      --  Timing
      Created_At       : Unsigned_64;
      Voting_Start     : Unsigned_64;
      Voting_End       : Unsigned_64;
      Timelock_End     : Unsigned_64;
      Executed_At      : Unsigned_64;
   end record;

   type Proposal_Array is array (Proposal_Index) of Proposal_Record;

   ---------------------------------------------------------------------------
   --  Vote Record
   ---------------------------------------------------------------------------

   type Vote_Choice is (For_Proposal, Against_Proposal, Abstain);

   type Vote_Record is record
      Voter            : Byte_Array (0 .. 31);
      Proposal_ID      : Unsigned_64;
      Choice           : Vote_Choice;
      Weight           : Unsigned_64;
      Delegated        : Boolean;
      Delegate_From    : Byte_Array (0 .. 31);
      Timestamp        : Unsigned_64;
      Signature        : Byte_Array (0 .. 127);
   end record;

   type Vote_Array is array (Natural range <>) of Vote_Record;

   ---------------------------------------------------------------------------
   --  Treasury State
   ---------------------------------------------------------------------------

   type Treasury_State is record
      --  Balances
      Total_Allocation : Unsigned_64;
      Current_Balance  : Unsigned_64;
      Total_Spent      : Unsigned_64;
      Total_Burned     : Unsigned_64;  -- From failed proposals

      --  Proposals
      Active_Proposals : Proposal_Array;
      Active_Count     : Natural;
      Total_Proposals  : Unsigned_64;
      Proposals_Passed : Unsigned_64;
      Proposals_Failed : Unsigned_64;

      --  Governance stats
      Total_Votes_Cast : Unsigned_64;
      Unique_Voters    : Unsigned_64;

      --  Supply tracking (for quorum calculation)
      Circulating_Supply : Unsigned_64;

      --  Timing
      Genesis_Block    : Unsigned_64;
      Current_Block    : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Propose_Result is (
      Proposed,
      Insufficient_Bond,
      Treasury_Empty,
      Too_Many_Active,
      Invalid_Amount,
      Invalid_Recipient,
      Proposer_Banned,
      Category_Mismatch
   );

   type Vote_Result is (
      Voted,
      Already_Voted,
      Proposal_Not_Active,
      Insufficient_Balance,
      Invalid_Signature,
      Voting_Ended
   );

   type Execute_Result is (
      Executed,
      Not_Passed,
      Timelock_Active,
      Already_Executed,
      Insufficient_Treasury,
      Execution_Failed
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize treasury
   procedure Init_Treasury (
      State          : out Treasury_State;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Post => State.Total_Allocation = Treasury_Allocation
              and State.Current_Balance = Treasury_Allocation
              and State.Active_Count = 0;

   ---------------------------------------------------------------------------
   --  Proposal Creation
   ---------------------------------------------------------------------------

   --  Create spending proposal
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
   ) with
      Global => null,
      Pre => Proposer'Length = 32
             and Recipient'Length = 32
             and Title_Hash'Length = 32
             and Description'Length = 32
             and Amount > 0;

   --  Create parameter change proposal
   procedure Create_Parameter_Proposal (
      State          : in Out Treasury_State;
      Proposer       : Byte_Array;
      Parameter_Hash : Byte_Array;
      New_Value_Hash : Byte_Array;
      Bond           : Unsigned_64;
      Proposal       : out Proposal_Record;
      Result         : out Propose_Result
   ) with
      Global => null,
      Pre => Proposer'Length = 32
             and Parameter_Hash'Length = 32
             and New_Value_Hash'Length = 32;

   --  Determine proposal category based on amount
   function Categorize_Proposal (
      Amount         : Unsigned_64;
      Treasury_Balance : Unsigned_64
   ) return Proposal_Category with
      Global => null;

   --  Get required bond for category
   function Get_Required_Bond (
      Category       : Proposal_Category
   ) return Unsigned_64 with
      Global => null;

   --  Get required quorum for category
   function Get_Required_Quorum (
      Category       : Proposal_Category;
      Circulating    : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   --  Get required threshold for category
   function Get_Required_Threshold (
      Category       : Proposal_Category
   ) return Natural with
      Global => null,
      Post => Get_Required_Threshold'Result <= 10000;

   ---------------------------------------------------------------------------
   --  Voting
   ---------------------------------------------------------------------------

   --  Cast vote
   procedure Cast_Vote (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Voter          : Byte_Array;
      Choice         : Vote_Choice;
      Weight         : Unsigned_64;
      Signature      : Byte_Array;
      Result         : out Vote_Result
   ) with
      Global => null,
      Pre => Voter'Length = 32
             and Signature'Length >= 64
             and Weight > 0;

   --  Delegate voting power
   procedure Delegate_Vote (
      State          : in Out Treasury_State;
      Delegator      : Byte_Array;
      Delegate       : Byte_Array;
      Signature      : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Delegator'Length = 32
             and Delegate'Length = 32
             and Signature'Length >= 64;

   --  Check if quorum reached
   function Quorum_Reached (
      Proposal       : Proposal_Record;
      Category       : Proposal_Category;
      Circulating    : Unsigned_64
   ) return Boolean with
      Global => null;

   --  Check if threshold reached
   function Threshold_Reached (
      Proposal       : Proposal_Record;
      Category       : Proposal_Category
   ) return Boolean with
      Global => null;

   --  Calculate vote percentage
   function Vote_Percentage (
      Votes_For      : Unsigned_64;
      Total_Votes    : Unsigned_64
   ) return Natural with
      Global => null,
      Pre => Total_Votes > 0,
      Post => Vote_Percentage'Result <= 10000;

   ---------------------------------------------------------------------------
   --  Proposal Finalization
   ---------------------------------------------------------------------------

   --  Finalize voting (called after voting period ends)
   procedure Finalize_Voting (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   --  Execute passed proposal (called after timelock)
   procedure Execute_Proposal (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Current_Block  : Unsigned_64;
      Result         : out Execute_Result
   ) with
      Global => null;

   --  Cancel proposal (by proposer, before voting ends)
   procedure Cancel_Proposal (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Canceller      : Byte_Array;
      Signature      : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Canceller'Length = 32
             and Signature'Length >= 64;

   --  Return bond (after execution or cancellation)
   procedure Return_Bond (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   --  Burn bond (on failed quorum)
   procedure Burn_Bond (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Emergency Actions
   ---------------------------------------------------------------------------

   --  Emergency veto (council only)
   procedure Emergency_Veto (
      State          : in Out Treasury_State;
      Proposal_ID    : Unsigned_64;
      Council_Sigs   : Byte_Array;
      Reason_Hash    : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Reason_Hash'Length = 32;

   --  Emergency fund release (for critical bugs)
   procedure Emergency_Release (
      State          : in Out Treasury_State;
      Recipient      : Byte_Array;
      Amount         : Unsigned_64;
      Council_Sigs   : Byte_Array;
      Reason_Hash    : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Recipient'Length = 32
             and Reason_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Treasury Queries
   ---------------------------------------------------------------------------

   --  Get treasury balance
   function Get_Balance (State : Treasury_State) return Unsigned_64 with
      Global => null,
      Post => Get_Balance'Result = State.Current_Balance;

   --  Get total spent
   function Get_Total_Spent (State : Treasury_State) return Unsigned_64 with
      Global => null,
      Post => Get_Total_Spent'Result = State.Total_Spent;

   --  Get active proposal count
   function Get_Active_Count (State : Treasury_State) return Natural with
      Global => null,
      Post => Get_Active_Count'Result = State.Active_Count;

   --  Get proposal by ID
   procedure Get_Proposal (
      State          : Treasury_State;
      Proposal_ID    : Unsigned_64;
      Proposal       : out Proposal_Record;
      Found          : out Boolean
   ) with
      Global => null;

   --  Check if address is proposer
   function Is_Proposer (
      Proposal       : Proposal_Record;
      Address        : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Address'Length = 32;

   ---------------------------------------------------------------------------
   --  Builder Access Prevention
   ---------------------------------------------------------------------------

   --  CRITICAL: These functions always return false for builder addresses
   --  Hardcoded check ensures builder can NEVER access treasury

   --  Check if address is builder (for blocking)
   function Is_Builder_Address (
      Address        : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Address'Length = 32
             and Builder_PK'Length = 32;

   --  Verify non-builder access
   function Verify_Non_Builder (
      Recipient      : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Recipient'Length = 32
             and Builder_PK'Length = 32,
      Post => Verify_Non_Builder'Result =
              (not Is_Builder_Address (Recipient, Builder_PK));

   ---------------------------------------------------------------------------
   --  Supply Tracking
   ---------------------------------------------------------------------------

   --  Update circulating supply
   procedure Update_Circulating_Supply (
      State          : in Out Treasury_State;
      New_Supply     : Unsigned_64
   ) with
      Global => null,
      Post => State.Circulating_Supply = New_Supply;

   --  Calculate available spend capacity
   function Available_Spend_Capacity (
      State          : Treasury_State
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Treasury_State (
      State          : Treasury_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 4096;

   procedure Deserialize_Treasury_State (
      Input          : Byte_Array;
      State          : out Treasury_State;
      Success        : out Boolean
   ) with
      Global => null;

   procedure Serialize_Proposal (
      Proposal       : Proposal_Record;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 512;

   procedure Deserialize_Proposal (
      Input          : Byte_Array;
      Proposal       : out Proposal_Record;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Treasury_State (State : out Treasury_State) with
      Global => null;

   procedure Zeroize_Proposal (Proposal : out Proposal_Record) with
      Global => null;

   procedure Zeroize_Vote (Vote : out Vote_Record) with
      Global => null;

end Anubis_Treasury;
