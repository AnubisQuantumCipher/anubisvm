-------------------------------------------------------------------------------
--  SCARAB - Decentralized Prover Market
--  Economic Layer for ZK Proof Generation Network
--
--  Implements a decentralized marketplace for ZK-STARK proof generation:
--  - Dutch auction pricing mechanism
--  - Prover staking and slashing
--  - Quality scoring and reputation
--  - Anti-centralization measures
--  - Geographic/hardware diversity incentives
--
--  Key Features:
--  - No single point of failure for proof generation
--  - Market-driven pricing ensures efficient resource allocation
--  - Slashing deters malicious or unreliable provers
--  - Diversity requirements prevent centralization
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Scarab_Prover_Market with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Market Constants
   ---------------------------------------------------------------------------

   --  Maximum registered provers
   Max_Provers           : constant := 1024;

   --  Maximum concurrent proof requests
   Max_Pending_Requests  : constant := 10_000;

   --  Auction parameters
   Auction_Duration_Ms   : constant := 3_000;   -- 3 second Dutch auction
   Price_Decay_Steps     : constant := 30;      -- Price drops every 100ms

   --  Staking requirements (in smallest token units)
   Min_Prover_Stake      : constant := 10_000_000_000;  -- 10,000 tokens
   Max_Prover_Stake_Pct  : constant := 10;              -- Max 10% of total stake

   --  Slashing percentages (basis points, 10000 = 100%)
   Slash_Invalid_Proof   : constant := 10_000;  -- 100% for fraud
   Slash_Missed_Deadline : constant := 1_000;   -- 10% for timeout
   Slash_Repeated_Failure: constant := 2_500;   -- 25% for pattern

   --  Quality thresholds
   Min_Success_Rate      : constant := 95;      -- 95% minimum
   Min_Uptime_Pct        : constant := 99;      -- 99% uptime

   --  Diversity requirements
   Max_Single_Region_Pct : constant := 33;      -- No region > 33%
   Max_Single_Hardware_Pct: constant := 50;     -- No hardware type > 50%

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Prover_Index is Natural range 0 .. Max_Provers - 1;
   subtype Request_Index is Natural range 0 .. Max_Pending_Requests - 1;

   --  Prover status
   type Prover_Status is (
      Active,        -- Available for proof requests
      Busy,          -- Currently generating a proof
      Suspended,     -- Temporarily suspended (slashing)
      Banned,        -- Permanently banned
      Offline        -- Not responding
   );

   --  Hardware type
   type Hardware_Type is (
      GPU_Nvidia,    -- NVIDIA GPUs
      GPU_AMD,       -- AMD GPUs
      FPGA,          -- FPGAs
      CPU,           -- CPU-only
      ASIC,          -- Custom ASICs
      Cloud,         -- Cloud instances
      Unknown
   );

   --  Geographic region
   type Geographic_Region is (
      North_America,
      South_America,
      Europe,
      Asia_Pacific,
      Middle_East,
      Africa,
      Unknown_Region
   );

   --  Named array types for SPARK
   type Region_Count_Array is array (Geographic_Region) of Natural;
   type Hardware_Count_Array is array (Hardware_Type) of Natural;

   --  Proof complexity tier
   type Proof_Complexity is (
      Tier_1_Simple,     -- Range proofs, simple transfers
      Tier_2_Standard,   -- Standard contract execution
      Tier_3_Complex,    -- Complex DeFi operations
      Tier_4_Heavy       -- Large batch proofs
   );

   --  Request priority
   type Request_Priority is (
      Low,           -- Best effort
      Normal,        -- Standard processing
      High,          -- Priority processing
      Urgent         -- Immediate processing
   );

   --  Prover information
   type Prover_Info is record
      --  Identity
      Index          : Prover_Index;
      Address        : Byte_Array (0 .. 31);
      PK_Hash        : Byte_Array (0 .. 31);

      --  Staking
      Stake_Amount   : Unsigned_64;
      Locked_Until   : Unsigned_64;

      --  Status
      Status         : Prover_Status;
      Last_Active    : Unsigned_64;
      Registered_At  : Unsigned_64;

      --  Hardware/Location
      Hardware       : Hardware_Type;
      Region         : Geographic_Region;
      Capacity       : Natural;  -- Proofs per hour

      --  Performance
      Total_Proofs   : Unsigned_64;
      Failed_Proofs  : Unsigned_64;
      Avg_Time_Ms    : Unsigned_64;

      --  Quality Score (0-10000 = 0-100%)
      Quality_Score  : Natural;
      Reputation     : Natural;
   end record;

   type Prover_Array is array (Prover_Index range <>) of Prover_Info;

   --  Proof request
   type Proof_Request is record
      --  Identity
      Request_ID     : Byte_Array (0 .. 31);
      Requester      : Byte_Array (0 .. 31);

      --  Proof specification
      Circuit_Hash   : Byte_Array (0 .. 31);
      Witness_Hash   : Byte_Array (0 .. 31);
      Complexity     : Proof_Complexity;
      Priority       : Request_Priority;

      --  Pricing
      Max_Fee        : Unsigned_64;
      Current_Price  : Unsigned_64;
      Final_Price    : Unsigned_64;

      --  Timing
      Submitted_At   : Unsigned_64;
      Deadline       : Unsigned_64;
      Auction_Ends   : Unsigned_64;

      --  Assignment
      Assigned_Prover: Prover_Index;
      Assigned       : Boolean;
      Completed      : Boolean;
      Success        : Boolean;
   end record;

   type Request_Array is array (Request_Index range <>) of Proof_Request;

   --  Auction state
   type Auction_State is record
      Request_ID     : Byte_Array (0 .. 31);
      Start_Price    : Unsigned_64;
      Current_Price  : Unsigned_64;
      Reserve_Price  : Unsigned_64;
      Started_At     : Unsigned_64;
      Ends_At        : Unsigned_64;
      Winner         : Prover_Index;
      Has_Winner     : Boolean;
      Finalized      : Boolean;
   end record;

   --  Market state
   type Market_State is record
      Provers        : Prover_Array (0 .. Max_Provers - 1);
      Num_Provers    : Natural;
      Total_Stake    : Unsigned_64;

      Requests       : Request_Array (0 .. Max_Pending_Requests - 1);
      Num_Pending    : Natural;

      --  Statistics
      Total_Requests : Unsigned_64;
      Total_Completed: Unsigned_64;
      Total_Failed   : Unsigned_64;
      Total_Fees     : Unsigned_64;

      --  Diversity metrics
      Region_Counts  : Region_Count_Array;
      Hardware_Counts: Hardware_Count_Array;
   end record;

   --  Bid result
   type Bid_Result is (
      Accepted,
      Outbid,
      Auction_Ended,
      Insufficient_Stake,
      Prover_Suspended,
      Invalid_Bid
   );

   --  Assignment result
   type Assignment_Result is (
      Assigned,
      No_Available_Prover,
      Auction_In_Progress,
      Request_Expired,
      Already_Assigned
   );

   --  Completion result
   type Completion_Result is (
      Verified,
      Invalid_Proof,
      Wrong_Prover,
      Deadline_Missed,
      Request_Not_Found
   );

   --  Market metrics
   type Market_Metrics is record
      Active_Provers    : Natural;
      Pending_Requests  : Natural;
      Avg_Price         : Unsigned_64;
      Avg_Time_Ms       : Unsigned_64;
      Success_Rate      : Natural;     -- Percentage
      Utilization       : Natural;     -- Percentage
      Diversity_Score   : Natural;     -- 0-100
   end record;

   ---------------------------------------------------------------------------
   --  Prover Registration
   ---------------------------------------------------------------------------

   --  Register new prover
   procedure Register_Prover (
      Market         : in Out Market_State;
      Address        : Byte_Array;
      PK_Hash        : Byte_Array;
      Stake_Amount   : Unsigned_64;
      Hardware       : Hardware_Type;
      Region         : Geographic_Region;
      Capacity       : Natural;
      Index          : out Prover_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Address'Length = 32
             and PK_Hash'Length = 32
             and Stake_Amount >= Min_Prover_Stake
             and Market.Num_Provers < Max_Provers,
      Post => (if Success then Market.Num_Provers = Market.Num_Provers'Old + 1);

   --  Increase prover stake
   procedure Increase_Stake (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Additional     : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Initiate stake withdrawal (with timelock)
   procedure Request_Withdrawal (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : Unsigned_64;
      Unlock_Time    : out Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Update prover status
   procedure Update_Prover_Status (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      New_Status     : Prover_Status
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Update prover hardware/region
   procedure Update_Prover_Info (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Hardware       : Hardware_Type;
      Region         : Geographic_Region;
      Capacity       : Natural
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   ---------------------------------------------------------------------------
   --  Request Management
   ---------------------------------------------------------------------------

   --  Submit proof request
   procedure Submit_Request (
      Market         : in Out Market_State;
      Requester      : Byte_Array;
      Circuit_Hash   : Byte_Array;
      Witness_Hash   : Byte_Array;
      Complexity     : Proof_Complexity;
      Priority       : Request_Priority;
      Max_Fee        : Unsigned_64;
      Deadline       : Unsigned_64;
      Request_ID     : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Requester'Length = 32
             and Circuit_Hash'Length = 32
             and Witness_Hash'Length = 32
             and Request_ID'Length = 32
             and Market.Num_Pending < Max_Pending_Requests;

   --  Cancel proof request (before assignment)
   procedure Cancel_Request (
      Market         : in Out Market_State;
      Request_ID     : Byte_Array;
      Requester      : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Request_ID'Length = 32 and Requester'Length = 32;

   --  Get request status
   procedure Get_Request_Status (
      Market         : Market_State;
      Request_ID     : Byte_Array;
      Request        : out Proof_Request;
      Found          : out Boolean
   ) with
      Global => null,
      Pre => Request_ID'Length = 32;

   ---------------------------------------------------------------------------
   --  Auction Mechanism (Dutch Auction)
   ---------------------------------------------------------------------------

   --  Start auction for request
   procedure Start_Auction (
      Market         : in Out Market_State;
      Request_Idx    : Request_Index;
      Start_Price    : Unsigned_64;
      Reserve_Price  : Unsigned_64;
      Auction        : out Auction_State
   ) with
      Global => null,
      Pre => Request_Idx < Market.Num_Pending;

   --  Place bid in auction
   procedure Place_Bid (
      Market         : in Out Market_State;
      Auction        : in Out Auction_State;
      Prover_Idx     : Prover_Index;
      Bid_Price      : Unsigned_64;
      Current_Time   : Unsigned_64;
      Result         : out Bid_Result
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers
             and not Auction.Finalized;

   --  Update auction price (called periodically)
   procedure Update_Auction_Price (
      Auction        : in Out Auction_State;
      Current_Time   : Unsigned_64
   ) with
      Global => null,
      Pre => not Auction.Finalized;

   --  Finalize auction
   procedure Finalize_Auction (
      Market         : in Out Market_State;
      Auction        : in Out Auction_State;
      Result         : out Assignment_Result
   ) with
      Global => null,
      Post => Auction.Finalized;

   --  Calculate current auction price
   function Current_Auction_Price (
      Auction        : Auction_State;
      Current_Time   : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Proof Assignment and Completion
   ---------------------------------------------------------------------------

   --  Direct assignment (skip auction for urgent)
   procedure Direct_Assign (
      Market         : in Out Market_State;
      Request_Idx    : Request_Index;
      Prover_Idx     : Prover_Index;
      Price          : Unsigned_64;
      Result         : out Assignment_Result
   ) with
      Global => null,
      Pre => Request_Idx < Market.Num_Pending
             and Prover_Idx < Market.Num_Provers;

   --  Find best available prover
   procedure Find_Best_Prover (
      Market         : Market_State;
      Complexity     : Proof_Complexity;
      Prover_Idx     : out Prover_Index;
      Found          : out Boolean
   ) with
      Global => null;

   --  Submit proof completion
   procedure Submit_Completion (
      Market         : in Out Market_State;
      Request_ID     : Byte_Array;
      Prover_Idx     : Prover_Index;
      Proof_Hash     : Byte_Array;
      Proof_Valid    : Boolean;
      Completion_Time: Unsigned_64;
      Result         : out Completion_Result
   ) with
      Global => null,
      Pre => Request_ID'Length = 32
             and Proof_Hash'Length = 32
             and Prover_Idx < Market.Num_Provers;

   --  Handle proof timeout
   procedure Handle_Timeout (
      Market         : in Out Market_State;
      Request_Idx    : Request_Index;
      Current_Time   : Unsigned_64
   ) with
      Global => null,
      Pre => Request_Idx < Market.Num_Pending;

   ---------------------------------------------------------------------------
   --  Slashing and Rewards
   ---------------------------------------------------------------------------

   --  Slash prover for invalid proof
   procedure Slash_Invalid (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : out Unsigned_64
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Slash prover for missed deadline
   procedure Slash_Timeout (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : out Unsigned_64
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Distribute reward to prover
   procedure Distribute_Reward (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : Unsigned_64
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Calculate prover earnings
   function Calculate_Earnings (
      Prover         : Prover_Info;
      Period_Start   : Unsigned_64;
      Period_End     : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Quality and Reputation
   ---------------------------------------------------------------------------

   --  Update prover quality score
   procedure Update_Quality_Score (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Calculate reputation based on history
   function Calculate_Reputation (
      Prover         : Prover_Info
   ) return Natural with
      Global => null,
      Post => Calculate_Reputation'Result <= 10000;

   --  Check if prover meets minimum quality
   function Meets_Quality_Threshold (
      Prover         : Prover_Info
   ) return Boolean with
      Global => null;

   --  Get prover ranking
   function Get_Prover_Rank (
      Market         : Market_State;
      Prover_Idx     : Prover_Index
   ) return Natural with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   ---------------------------------------------------------------------------
   --  Diversity Management
   ---------------------------------------------------------------------------

   --  Check regional diversity
   function Check_Regional_Diversity (
      Market         : Market_State
   ) return Boolean with
      Global => null;

   --  Check hardware diversity
   function Check_Hardware_Diversity (
      Market         : Market_State
   ) return Boolean with
      Global => null;

   --  Get diversity score (0-100)
   function Get_Diversity_Score (
      Market         : Market_State
   ) return Natural with
      Global => null,
      Post => Get_Diversity_Score'Result <= 100;

   --  Apply diversity bonus to bid
   function Apply_Diversity_Bonus (
      Market         : Market_State;
      Prover_Idx     : Prover_Index;
      Base_Score     : Natural
   ) return Natural with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Check if new prover would help diversity
   function Would_Improve_Diversity (
      Market         : Market_State;
      Region         : Geographic_Region;
      Hardware       : Hardware_Type
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Pricing
   ---------------------------------------------------------------------------

   --  Estimate proof price
   function Estimate_Price (
      Market         : Market_State;
      Complexity     : Proof_Complexity;
      Priority       : Request_Priority
   ) return Unsigned_64 with
      Global => null;

   --  Get complexity multiplier
   function Complexity_Multiplier (
      Complexity     : Proof_Complexity
   ) return Natural with
      Global => null;

   --  Get priority multiplier
   function Priority_Multiplier (
      Priority       : Request_Priority
   ) return Natural with
      Global => null;

   --  Calculate dynamic base price
   function Dynamic_Base_Price (
      Market         : Market_State
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Emergency Procedures
   ---------------------------------------------------------------------------

   --  Emergency pause market
   procedure Emergency_Pause (
      Market         : in Out Market_State;
      Authority_Sig  : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Authority_Sig'Length = 4627;

   --  Activate lite proof mode
   procedure Activate_Lite_Mode (
      Market         : in Out Market_State
   ) with
      Global => null;

   --  Fallback to validator proving
   procedure Enable_Validator_Fallback (
      Market         : in Out Market_State
   ) with
      Global => null;

   --  Check network health
   function Check_Network_Health (
      Market         : Market_State
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Statistics and Metrics
   ---------------------------------------------------------------------------

   --  Get market metrics
   function Get_Metrics (
      Market         : Market_State
   ) return Market_Metrics with
      Global => null;

   --  Get prover performance summary
   procedure Get_Prover_Stats (
      Market         : Market_State;
      Prover_Idx     : Prover_Index;
      Total_Proofs   : out Unsigned_64;
      Success_Rate   : out Natural;
      Avg_Time       : out Unsigned_64;
      Total_Earned   : out Unsigned_64
   ) with
      Global => null,
      Pre => Prover_Idx < Market.Num_Provers;

   --  Get market utilization
   function Get_Utilization (
      Market         : Market_State
   ) return Natural with
      Global => null,
      Post => Get_Utilization'Result <= 100;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize market state
   procedure Serialize_Market (
      Market         : Market_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 1024 * 1024;

   --  Deserialize market state
   procedure Deserialize_Market (
      Input          : Byte_Array;
      Market         : out Market_State;
      Success        : out Boolean
   ) with
      Global => null;

   --  Serialize prover info
   procedure Serialize_Prover (
      Prover         : Prover_Info;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 512;

end Scarab_Prover_Market;
