-------------------------------------------------------------------------------
--  ANUBIS Milestone-Based Vesting System
--  Solo Builder 30% Allocation with Verified Milestone Unlocks
--
--  CRITICAL INNOVATION: Tokens don"t vest on time alone - they vest when
--  verified milestones are delivered. GNATprove + audits + oracle attestation.
--
--  Milestones (from Tokenomics v4.0):
--  1. ML-DSA-87 SPARK implementation       - 5% (50M) - GNATprove + audit
--  2. ML-KEM-1024 SPARK implementation     - 5% (50M) - GNATprove + audit
--  3. Core VM with WCET gas model          - 5% (50M) - Test suite + audit
--  4. Cosmos SDK PQ integration            - 5% (50M) - Testnet launch
--  5. zk-STARK prover (basic)              - 3% (30M) - Proof verification
--  6. Privacy layer (Shield, Whisper)      - 3% (30M) - Testnet demo
--  7. Mainnet launch                       - 2% (20M) - Block 1 produced
--  8. 1 year mainnet stability             - 1% (10M) - 99.9% uptime
--  9. 2 year mainnet stability             - 1% (10M) - No critical bugs
--
--  Post-Milestone Linear Vesting: After each milestone unlocks, tokens
--  vest linearly over 12 months. You can"t dump.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Vesting with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Total builder allocation (30% of 1B)
   Total_Builder_Allocation : constant := 300_000_000;

   --  Number of milestones
   Num_Milestones           : constant := 9;

   --  Linear vesting period after milestone (12 months in blocks)
   --  Assuming 6-second blocks: 12 * 30 * 24 * 60 * 10 = 5,184,000 blocks
   Vesting_Period_Blocks    : constant := 5_184_000;

   --  Blocks per month (for calculations)
   Blocks_Per_Month         : constant := 432_000;

   ---------------------------------------------------------------------------
   --  Milestone Definitions
   ---------------------------------------------------------------------------

   type Milestone_ID is range 1 .. Num_Milestones;

   type Milestone_Status is (
      Pending,        -- Not yet completed
      Submitted,      -- Work submitted, awaiting verification
      Verifying,      -- Under audit/verification
      Unlocked,       -- Milestone verified, vesting begins
      Vesting,        -- Linear vesting in progress
      Fully_Vested    -- All tokens released
   );

   type Verification_Type is (
      GNATprove_Only,     -- Automated SPARK verification
      GNATprove_Audit,    -- SPARK + third-party audit
      Test_Suite_Audit,   -- Test coverage + audit
      Testnet_Demo,       -- Testnet demonstration
      Mainnet_Block,      -- Block production verified
      Uptime_Oracle       -- Uptime monitoring oracle
   );

   --  Milestone allocation amounts (in raw tokens)
   Milestone_Amounts : constant array (Milestone_ID) of Unsigned_64 := (
      1 => 50_000_000,   -- ML-DSA-87 SPARK
      2 => 50_000_000,   -- ML-KEM-1024 SPARK
      3 => 50_000_000,   -- Core VM + WCET
      4 => 50_000_000,   -- Cosmos SDK PQ
      5 => 30_000_000,   -- zk-STARK prover
      6 => 30_000_000,   -- Privacy layer
      7 => 20_000_000,   -- Mainnet launch
      8 => 10_000_000,   -- 1 year stability
      9 => 10_000_000    -- 2 year stability
   );

   --  Milestone verification types
   Milestone_Verification : constant array (Milestone_ID) of Verification_Type := (
      1 => GNATprove_Audit,
      2 => GNATprove_Audit,
      3 => Test_Suite_Audit,
      4 => Testnet_Demo,
      5 => GNATprove_Audit,
      6 => Testnet_Demo,
      7 => Mainnet_Block,
      8 => Uptime_Oracle,
      9 => Uptime_Oracle
   );

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Milestone record
   type Milestone_Record is record
      ID               : Milestone_ID;
      Status           : Milestone_Status;
      Allocation       : Unsigned_64;
      Unlocked_At      : Unsigned_64;    -- Block number when unlocked
      Vested_Amount    : Unsigned_64;    -- Amount already vested
      Released_Amount  : Unsigned_64;    -- Amount already released to builder
      --  Verification data
      Commit_Hash      : Byte_Array (0 .. 31);  -- Git commit hash
      Proof_Hash       : Byte_Array (0 .. 31);  -- GNATprove verification hash
      Audit_Hash       : Byte_Array (0 .. 31);  -- Audit report hash
      Oracle_Attestation : Byte_Array (0 .. 63); -- Oracle signature
      Verifier_PK      : Byte_Array (0 .. 31);  -- Verifier"s public key hash
   end record;

   type Milestone_Array is array (Milestone_ID) of Milestone_Record;

   --  Vesting schedule state
   type Vesting_State is record
      Builder_Address  : Byte_Array (0 .. 31);
      Milestones       : Milestone_Array;
      Total_Unlocked   : Unsigned_64;    -- Sum of unlocked milestone amounts
      Total_Vested     : Unsigned_64;    -- Sum of vested (claimable) amounts
      Total_Released   : Unsigned_64;    -- Sum of released to builder
      Current_Block    : Unsigned_64;
      Initialized      : Boolean;
   end record;

   --  Submission for milestone verification
   type Milestone_Submission is record
      Milestone        : Milestone_ID;
      Commit_Hash      : Byte_Array (0 .. 31);
      Proof_Hash       : Byte_Array (0 .. 31);  -- GNATprove output hash
      Lines_Verified   : Unsigned_32;           -- Lines with VCs proven
      Test_Coverage    : Natural;               -- Percentage (0-100)
      IPFS_Link        : Byte_Array (0 .. 45);  -- CID to full report
      Builder_Sig      : Byte_Array (0 .. 127); -- ML-DSA signature
   end record;

   --  Oracle attestation
   type Oracle_Attestation is record
      Milestone        : Milestone_ID;
      Timestamp        : Unsigned_64;
      Block_Number     : Unsigned_64;
      Verification_Result : Boolean;
      Details_Hash     : Byte_Array (0 .. 31);
      Oracle_Sig       : Byte_Array (0 .. 127);
   end record;

   --  Results
   type Submit_Result is (
      Accepted,
      Invalid_Milestone,
      Already_Submitted,
      Invalid_Signature,
      Previous_Incomplete,
      Submission_Failed
   );

   type Verify_Result is (
      Verified,
      Verification_Failed,
      Invalid_Proof,
      Audit_Required,
      Oracle_Rejected,
      Timeout
   );

   type Release_Result is (
      Released,
      Nothing_Vested,
      Not_Unlocked,
      Already_Released,
      Dead_Man_Switch_Active
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize vesting state
   procedure Init_Vesting (
      State          : out Vesting_State;
      Builder_Addr   : Byte_Array
   ) with
      Global => null,
      Pre => Builder_Addr'Length = 32,
      Post => State.Initialized
              and State.Total_Unlocked = 0
              and State.Total_Vested = 0
              and State.Total_Released = 0;

   ---------------------------------------------------------------------------
   --  Milestone Submission
   ---------------------------------------------------------------------------

   --  Submit milestone for verification
   procedure Submit_Milestone (
      State          : in Out Vesting_State;
      Submission     : Milestone_Submission;
      Result         : out Submit_Result
   ) with
      Global => null,
      Pre => State.Initialized,
      Post => (if Result = Accepted then
                  State.Milestones (Submission.Milestone).Status = Submitted);

   --  Verify milestone (called by oracle/verifier)
   procedure Verify_Milestone (
      State          : in Out Vesting_State;
      Attestation    : Oracle_Attestation;
      Auditor_Sig    : Byte_Array;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => Auditor_Sig'Length >= 64,
      Post => (if Result = Verified then
                  State.Milestones (Attestation.Milestone).Status = Unlocked);

   --  Check if previous milestones are complete (enforces order)
   function Previous_Milestones_Complete (
      State          : Vesting_State;
      Milestone      : Milestone_ID
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Vesting Calculations
   ---------------------------------------------------------------------------

   --  Calculate currently vested amount for a milestone
   function Calculate_Vested (
      Milestone      : Milestone_Record;
      Current_Block  : Unsigned_64
   ) return Unsigned_64 with
      Global => null,
      Post => Calculate_Vested'Result <= Milestone.Allocation;

   --  Calculate total vested across all milestones
   function Total_Vested_Amount (
      State          : Vesting_State
   ) return Unsigned_64 with
      Global => null,
      Post => Total_Vested_Amount'Result <= Total_Builder_Allocation;

   --  Calculate claimable (vested but not released)
   function Claimable_Amount (
      State          : Vesting_State
   ) return Unsigned_64 with
      Global => null;

   --  Update vesting state (call each block)
   procedure Update_Vesting (
      State          : in Out Vesting_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => State.Initialized,
      Post => State.Current_Block = Current_Block;

   ---------------------------------------------------------------------------
   --  Token Release
   ---------------------------------------------------------------------------

   --  Release vested tokens to builder
   procedure Release_Vested (
      State          : in Out Vesting_State;
      Amount         : Unsigned_64;
      Recipient      : Byte_Array;
      Result         : out Release_Result
   ) with
      Global => null,
      Pre => Recipient'Length = 32
             and Amount > 0,
      Post => (if Result = Released then
                  State.Total_Released = State.Total_Released'Old + Amount);

   --  Release all available vested tokens
   procedure Release_All_Vested (
      State          : in Out Vesting_State;
      Recipient      : Byte_Array;
      Released       : out Unsigned_64;
      Result         : out Release_Result
   ) with
      Global => null,
      Pre => Recipient'Length = 32;

   ---------------------------------------------------------------------------
   --  Milestone Queries
   ---------------------------------------------------------------------------

   --  Get milestone status
   function Get_Milestone_Status (
      State          : Vesting_State;
      Milestone      : Milestone_ID
   ) return Milestone_Status with
      Global => null;

   --  Get milestone allocation
   function Get_Milestone_Allocation (
      Milestone      : Milestone_ID
   ) return Unsigned_64 with
      Global => null,
      Post => Get_Milestone_Allocation'Result = Milestone_Amounts (Milestone);

   --  Get milestone vesting progress (0-100%)
   function Get_Vesting_Progress (
      State          : Vesting_State;
      Milestone      : Milestone_ID
   ) return Natural with
      Global => null,
      Post => Get_Vesting_Progress'Result <= 100;

   --  Get overall vesting progress (0-100%)
   function Get_Overall_Progress (
      State          : Vesting_State
   ) return Natural with
      Global => null,
      Post => Get_Overall_Progress'Result <= 100;

   --  Count completed milestones
   function Completed_Milestones (
      State          : Vesting_State
   ) return Natural with
      Global => null,
      Post => Completed_Milestones'Result <= Num_Milestones;

   ---------------------------------------------------------------------------
   --  Verification Helpers
   ---------------------------------------------------------------------------

   --  Verify GNATprove output hash
   function Verify_GNATprove_Hash (
      Expected       : Byte_Array;
      Proof_Output   : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Expected'Length = 32;

   --  Verify audit signature
   function Verify_Audit_Signature (
      Milestone      : Milestone_ID;
      Audit_Report   : Byte_Array;
      Auditor_PK     : Byte_Array;
      Signature      : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Auditor_PK'Length = 2592
             and Signature'Length >= 64;

   --  Verify oracle attestation
   function Verify_Oracle_Attestation (
      Attestation    : Oracle_Attestation;
      Oracle_PK      : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Oracle_PK'Length = 2592;

   ---------------------------------------------------------------------------
   --  Public Build Protocol Integration
   ---------------------------------------------------------------------------

   --  Record weekly update (required for continued vesting)
   type Weekly_Update is record
      Week_Number    : Unsigned_32;
      Commits_Count  : Unsigned_32;
      Lines_Verified : Unsigned_32;
      Milestone_Progress : Natural;  -- Percentage
      Blockers       : Byte_Array (0 .. 255);  -- Hash of blocker description
      IPFS_Link      : Byte_Array (0 .. 45);
      Timestamp      : Unsigned_64;
      Builder_Sig    : Byte_Array (0 .. 127);
   end record;

   --  Weekly update tracking
   type Update_Status is (
      On_Track,           -- Updates on schedule
      Warning,            -- 1 week missed
      Slowdown,           -- 2 weeks missed (10% slowdown)
      Paused,             -- 4 weeks missed
      Dead_Man_Warning    -- 8 weeks missed
   );

   --  Record weekly update
   procedure Record_Weekly_Update (
      State          : in Out Vesting_State;
      Update         : Weekly_Update;
      Success        : out Boolean
   ) with
      Global => null;

   --  Get current update status
   function Get_Update_Status (
      State          : Vesting_State;
      Current_Block  : Unsigned_64
   ) return Update_Status with
      Global => null;

   --  Get vesting slowdown multiplier (100 = normal, 90 = 10% slower)
   function Get_Vesting_Rate (
      State          : Vesting_State;
      Current_Block  : Unsigned_64
   ) return Natural with
      Global => null,
      Post => Get_Vesting_Rate'Result <= 100;

   ---------------------------------------------------------------------------
   --  Proof-of-Build NFT Integration
   ---------------------------------------------------------------------------

   --  Proof-of-Build NFT data
   type Proof_Of_Build is record
      Token_ID       : Unsigned_64;
      Milestone      : Milestone_ID;
      Commit_Hash    : Byte_Array (0 .. 31);
      Proof_Hash     : Byte_Array (0 .. 31);
      Lines_Added    : Unsigned_32;
      Timestamp      : Unsigned_64;
      IPFS_Report    : Byte_Array (0 .. 45);
   end record;

   --  Mint Proof-of-Build NFT
   procedure Mint_Proof_Of_Build (
      Milestone      : Milestone_ID;
      Commit_Hash    : Byte_Array;
      Proof_Hash     : Byte_Array;
      Lines_Added    : Unsigned_32;
      NFT            : out Proof_Of_Build;
      Token_ID       : out Unsigned_64
   ) with
      Global => null,
      Pre => Commit_Hash'Length = 32
             and Proof_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize vesting state
   procedure Serialize_Vesting_State (
      State          : Vesting_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 2048;

   --  Deserialize vesting state
   procedure Deserialize_Vesting_State (
      Input          : Byte_Array;
      State          : out Vesting_State;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Vesting_State (State : out Vesting_State) with
      Global => null;

   procedure Zeroize_Milestone (Milestone : out Milestone_Record) with
      Global => null;

   procedure Zeroize_Submission (Submission : out Milestone_Submission) with
      Global => null;

end Anubis_Vesting;
