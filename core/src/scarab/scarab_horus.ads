-------------------------------------------------------------------------------
--  SCARAB - HORUS Pipeline Prover
--  Hierarchical Optimistic Rapid Universal Sequencing
--
--  Eliminates STARK proof latency through pipelining:
--  - Prove block N while validating block N+1
--  - 6-second blocks with 4-second proof generation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Scarab_Horus with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Pipeline Parameters
   ---------------------------------------------------------------------------

   --  Block time target (seconds)
   Block_Time      : constant := 6;

   --  Proof generation budget (seconds)
   Proof_Budget    : constant := 4;

   --  Pipeline depth (blocks in flight)
   Pipeline_Depth  : constant := 3;

   --  Maximum batch size
   Max_Batch_Size  : constant := 4096;

   --  Prover tiers
   type Prover_Tier is (
      Light,       -- 16GB RAM, small batches
      Standard,    -- 64GB RAM, medium batches
      Heavy,       -- 256GB RAM, large batches
      Enterprise   -- 1TB+ RAM, full blocks
   );

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Transaction execution modes
   type TX_Mode is (
      Standard,     -- Signature verified by network, proof async
      Pre_Proven,   -- User provides their own STARK proof (instant)
      Batched       -- Goes to batch aggregator (cheapest)
   );

   --  Transaction with signature
   type Transaction is record
      From_Addr      : Byte_Array (0 .. 31);
      To_Addr        : Byte_Array (0 .. 31);
      Value          : Unsigned_64;
      Nonce          : Unsigned_64;
      Gas_Limit      : Unsigned_64;
      Gas_Price      : Unsigned_64;
      Data           : Byte_Array (0 .. 4095);
      Data_Length    : Natural;
      Signature      : Byte_Array (0 .. 4626);  -- ML-DSA-87
      Mode           : TX_Mode;
   end record;

   --  Pre-proven transaction (user generated proof)
   type PreProven_TX is record
      TX             : Transaction;
      Mini_Proof     : Byte_Array (0 .. 1023);  -- Small STARK proof
      Proof_Length   : Natural;
   end record;

   --  Transaction array type
   type TX_Array is array (0 .. Max_Batch_Size - 1) of Transaction;

   --  Batch of transactions awaiting proof
   type TX_Batch is record
      Transactions   : TX_Array;
      Count          : Natural;
      Block_Height   : Unsigned_64;
      Batch_Hash     : Byte_Array (0 .. 31);
   end record;

   --  Job status enumeration (defined before Proof_Job which uses it)
   type Job_Status is (
      Posted,        -- Available for claiming
      Claimed,       -- Prover working on it
      Submitted,     -- Proof submitted
      Verified,      -- Proof verified and accepted
      Expired        -- Deadline passed
   );

   --  Proof job for prover market
   type Proof_Job is record
      Job_ID         : Byte_Array (0 .. 31);
      Batch          : TX_Batch;
      Deadline       : Unsigned_64;  -- Block height deadline
      Reward         : Unsigned_64;  -- ANUBIS reward
      Claimed_By     : Byte_Array (0 .. 31);  -- Prover address
      Status         : Job_Status;
   end record;

   --  Proof submission
   type Proof_Submission is record
      Job_ID         : Byte_Array (0 .. 31);
      Prover         : Byte_Array (0 .. 31);
      Proof          : Byte_Array (0 .. 163839);  -- Up to 160KB
      Proof_Length   : Natural;
      Timestamp      : Unsigned_64;
   end record;

   --  Block with proof
   type Proven_Block is record
      Height         : Unsigned_64;
      Parent_Hash    : Byte_Array (0 .. 31);
      State_Root     : Byte_Array (0 .. 31);
      TX_Root        : Byte_Array (0 .. 31);
      Batch_Proof    : Byte_Array (0 .. 163839);
      Proof_Length   : Natural;
      Proposer       : Byte_Array (0 .. 31);
   end record;

   --  Stage types
   type Stage_Type is (
      Collecting,    -- Collecting transactions
      Executing,     -- Executing transactions
      Proving,       -- Generating STARK proof
      Finalizing     -- Committing to chain
   );

   --  Pipeline stage
   type Pipeline_Stage is record
      Block_Height   : Unsigned_64;
      Stage          : Stage_Type;
      Started_At     : Unsigned_64;
      Data           : Byte_Array (0 .. 65535);
      Data_Length    : Natural;
   end record;

   --  Stage array type
   type Stage_Array is array (0 .. Pipeline_Depth - 1) of Pipeline_Stage;

   --  Full pipeline state
   type Pipeline_State is record
      Stages         : Stage_Array;
      Current_Height : Unsigned_64;
      Head_Proven    : Unsigned_64;  -- Last proven block
   end record;

   ---------------------------------------------------------------------------
   --  Prover Registration
   ---------------------------------------------------------------------------

   Minimum_Prover_Stake : constant := 10_000;  -- ANUBIS

   type Prover_Info is record
      Address        : Byte_Array (0 .. 31);
      Stake          : Unsigned_64;
      Tier           : Prover_Tier;
      Jobs_Completed : Unsigned_64;
      Reputation     : Unsigned_64;  -- 0-1000
      Active         : Boolean;
   end record;

   --  Register as prover
   procedure Register_Prover (
      Address        : Byte_Array;
      Stake          : Unsigned_64;
      Tier           : Prover_Tier;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Stake >= Minimum_Prover_Stake and Address'Length = 32;

   --  Unregister prover (with unbonding period)
   procedure Unregister_Prover (
      Address        : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Job Market
   ---------------------------------------------------------------------------

   --  Post proof job (called by validators)
   procedure Post_Proof_Job (
      Batch          : TX_Batch;
      Deadline       : Unsigned_64;
      Reward         : Unsigned_64;
      Job            : out Proof_Job
   ) with
      Global => null;

   --  Claim job (called by prover)
   procedure Claim_Job (
      Prover         : Byte_Array;
      Job_ID         : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Prover'Length = 32 and Job_ID'Length = 32;

   --  Submit proof
   procedure Submit_Proof (
      Submission     : Proof_Submission;
      Success        : out Boolean
   ) with
      Global => null;

   --  Verify and accept proof
   procedure Accept_Proof (
      Job            : in Out Proof_Job;
      Submission     : Proof_Submission;
      Valid          : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Pipeline Operations
   ---------------------------------------------------------------------------

   --  Initialize pipeline
   procedure Init_Pipeline (
      State          : out Pipeline_State;
      Start_Height   : Unsigned_64
   ) with
      Global => null;

   --  Add transaction to current collecting stage
   procedure Add_Transaction (
      State          : in Out Pipeline_State;
      TX             : Transaction;
      Success        : out Boolean
   ) with
      Global => null;

   --  Advance pipeline (called each block)
   procedure Advance_Pipeline (
      State          : in Out Pipeline_State;
      New_Block      : out Proven_Block;
      Has_Block      : out Boolean
   ) with
      Global => null;

   --  Get pipeline status
   function Get_Pipeline_Status (
      State          : Pipeline_State
   ) return Pipeline_Stage with
      Global => null;

   ---------------------------------------------------------------------------
   --  Transaction Modes
   ---------------------------------------------------------------------------

   --  Standard: Verify signature, queue for batch proof
   procedure Process_Standard_TX (
      TX             : Transaction;
      State          : in Out Pipeline_State;
      Accepted       : out Boolean
   ) with
      Global => null;

   --  Pre-proven: Verify mini-proof immediately
   procedure Process_PreProven_TX (
      TX             : PreProven_TX;
      State          : in Out Pipeline_State;
      Accepted       : out Boolean
   ) with
      Global => null;

   --  Batched: Add to batch aggregator
   procedure Process_Batched_TX (
      TX             : Transaction;
      State          : in Out Pipeline_State;
      Accepted       : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Batch Aggregation
   ---------------------------------------------------------------------------

   --  Aggregate batch of signatures into single proof
   procedure Aggregate_Batch (
      Batch          : TX_Batch;
      Proof          : out Byte_Array;
      Proof_Length   : out Natural;
      Success        : out Boolean
   ) with
      Global => null;

   --  Verify batch proof
   function Verify_Batch_Proof (
      Batch          : TX_Batch;
      Proof          : Byte_Array
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Finality
   ---------------------------------------------------------------------------

   type Finality_Status is (
      Soft,          -- Executed but not proven
      Hard,          -- Proven
      Final          -- Proven + N confirmations
   );

   --  Get finality status of block
   function Get_Finality (
      Height         : Unsigned_64;
      State          : Pipeline_State
   ) return Finality_Status with
      Global => null;

   --  Get finality delay (blocks until hard finality)
   function Finality_Delay (
      Mode           : TX_Mode
   ) return Natural with
      Global => null,
      Post => Finality_Delay'Result <= Pipeline_Depth + 1;

   ---------------------------------------------------------------------------
   --  Rewards and Slashing
   ---------------------------------------------------------------------------

   --  Calculate prover reward
   function Calculate_Reward (
      Job            : Proof_Job;
      Submission_Time: Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   --  Slash prover for missed deadline
   procedure Slash_Prover (
      Prover         : in Out Prover_Info;
      Job            : Proof_Job;
      Slash_Amount   : out Unsigned_64
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Metrics
   ---------------------------------------------------------------------------

   type Pipeline_Metrics is record
      Avg_Proof_Time : Unsigned_64;  -- ms
      Avg_Block_Time : Unsigned_64;  -- ms
      Proofs_Per_Hour: Unsigned_64;
      Active_Provers : Natural;
      Pending_Jobs   : Natural;
   end record;

   function Get_Metrics (State : Pipeline_State) return Pipeline_Metrics with
      Global => null;

end Scarab_Horus;
