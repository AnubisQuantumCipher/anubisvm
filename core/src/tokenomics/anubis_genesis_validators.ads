-------------------------------------------------------------------------------
--  ANUBIS Genesis Validators
--  15% Allocation (150M ANUBIS) Earned via Block Production
--
--  Genesis Validators are the initial 100 validators selected via public
--  application before mainnet launch. They earn tokens through block rewards
--  over 3 years, not through grants.
--
--  Parameters:
--  - Allocation: 150,000,000 ANUBIS (15%)
--  - Distribution: Block rewards over 3 years
--  - Genesis Set: First 100 validators (application-based)
--  - Open Entry: After 6 months (stake-based)
--  - Minimum Stake: 25,000 ANUBIS (can be earned or bought)
--
--  Selection Criteria:
--  - Hardware Proof: Screenshots of server specs meeting requirements
--  - Uptime History: Track record on other chains
--  - Geographic Distribution: Priority for underserved regions
--  - Technical Competence: Basic SPARK/Ada familiarity preferred
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Genesis_Validators with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Allocation
   Validator_Allocation       : constant := 150_000_000;  -- 15% of 1B
   Distribution_Period_Blocks : constant := 15_768_000;   -- 3 years

   --  Genesis set limits
   Genesis_Validator_Count    : constant := 100;
   Open_Entry_Block           : constant := 2_628_000;    -- 6 months

   --  Stake requirements
   Min_Stake                  : constant := 25_000;
   Max_Stake                  : constant := 5_000_000;    -- 0.5% cap
   Max_Stake_Percentage_BP    : constant := 50;           -- 0.5%

   --  Block timing
   Block_Time_Seconds         : constant := 6;
   Blocks_Per_Day             : constant := 14_400;
   Blocks_Per_Year            : constant := 5_256_000;

   --  Initial block reward
   Initial_Block_Reward       : constant := 5;            -- 5 ANUBIS
   Reward_Decay_Rate_BP       : constant := 1000;         -- 10% per year

   --  Unbonding period
   Unbonding_Period_Blocks    : constant := 201_600;      -- 14 days

   --  Slashing rates (basis points)
   Slash_Double_Sign_BP       : constant := 500;          -- 5%
   Slash_Downtime_BP          : constant := 10;           -- 0.1%
   Slash_Safety_Violation_BP  : constant := 1000;         -- 10%

   --  Uptime requirements
   Min_Uptime_Percentage_BP   : constant := 9500;         -- 95%
   Blocks_Per_Epoch           : constant := 100;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Validator status
   type Validator_Status is (
      Pending_Application,  -- Application submitted
      Approved,             -- Application approved
      Active,               -- Producing blocks
      Jailed,               -- Temporarily jailed
      Unbonding,            -- Withdrawing stake
      Slashed,              -- Slashed for misconduct
      Removed               -- Permanently removed
   );

   --  Application status
   type Application_Status is (
      Submitted,
      Under_Review,
      Approved,
      Rejected,
      Waitlisted
   );

   --  Geographic regions (for distribution)
   type Geographic_Region is (
      North_America,
      South_America,
      Europe,
      Africa,
      Middle_East,
      Central_Asia,
      South_Asia,
      East_Asia,
      Southeast_Asia,
      Oceania
   );

   subtype Validator_Index is Natural range 0 .. Genesis_Validator_Count - 1;

   ---------------------------------------------------------------------------
   --  Hardware Requirements
   ---------------------------------------------------------------------------

   type Hardware_Tier is (
      Minimum,        -- Bare minimum specs
      Recommended,    -- Recommended specs
      Professional    -- High-performance
   );

   type Hardware_Specs is record
      CPU_Cores      : Natural;
      RAM_GB         : Natural;
      Storage_TB     : Natural;
      Network_Mbps   : Natural;
      Is_Dedicated   : Boolean;
      Has_UPS        : Boolean;
      Tier           : Hardware_Tier;
   end record;

   --  Minimum requirements
   Min_CPU_Cores              : constant := 8;
   Min_RAM_GB                 : constant := 32;
   Min_Storage_TB             : constant := 2;
   Min_Network_Mbps           : constant := 100;

   ---------------------------------------------------------------------------
   --  Validator Records
   ---------------------------------------------------------------------------

   type Validator_Record is record
      --  Identity
      Index              : Validator_Index;
      Address            : Byte_Array (0 .. 31);
      PK_Hash            : Byte_Array (0 .. 31);  -- ML-DSA public key hash
      Consensus_PK       : Byte_Array (0 .. 63);  -- Consensus signing key

      --  Status
      Status             : Validator_Status;
      Is_Genesis         : Boolean;
      Region             : Geographic_Region;

      --  Stake
      Self_Stake         : Unsigned_64;
      Delegated_Stake    : Unsigned_64;
      Total_Stake        : Unsigned_64;

      --  Rewards
      Blocks_Produced    : Unsigned_64;
      Blocks_Missed      : Unsigned_64;
      Total_Rewards      : Unsigned_64;
      Pending_Rewards    : Unsigned_64;
      Commission_Rate_BP : Natural;

      --  Slashing
      Times_Slashed      : Natural;
      Total_Slashed      : Unsigned_64;
      Jail_End_Block     : Unsigned_64;

      --  Timing
      Registered_At      : Unsigned_64;
      Active_Since       : Unsigned_64;
      Last_Block         : Unsigned_64;
      Unbonding_End      : Unsigned_64;
   end record;

   type Validator_Array is array (Validator_Index) of Validator_Record;

   ---------------------------------------------------------------------------
   --  Application Record
   ---------------------------------------------------------------------------

   type Validator_Application is record
      Applicant          : Byte_Array (0 .. 31);
      Status             : Application_Status;
      Region             : Geographic_Region;
      Hardware           : Hardware_Specs;

      --  Verification
      Hardware_Proof_Hash: Byte_Array (0 .. 31);
      Uptime_Proof_Hash  : Byte_Array (0 .. 31);
      Experience_Hash    : Byte_Array (0 .. 31);
      IPFS_Application   : Byte_Array (0 .. 45);

      --  Stake commitment
      Committed_Stake    : Unsigned_64;
      Stake_Source       : Byte_Array (0 .. 31);  -- Address funding stake

      --  Review
      Submitted_At       : Unsigned_64;
      Reviewed_At        : Unsigned_64;
      Reviewer           : Byte_Array (0 .. 31);
      Review_Notes_Hash  : Byte_Array (0 .. 31);
   end record;

   type Application_Array is array (Natural range <>) of Validator_Application;

   ---------------------------------------------------------------------------
   --  Validator Set State
   ---------------------------------------------------------------------------

   type Validator_Set_State is record
      --  Active set
      Validators         : Validator_Array;
      Active_Count       : Natural;
      Genesis_Count      : Natural;
      Total_Stake        : Unsigned_64;

      --  Rewards distribution
      Total_Distributed  : Unsigned_64;
      Remaining_Pool     : Unsigned_64;
      Current_Block_Reward : Unsigned_64;

      --  Timing
      Genesis_Block      : Unsigned_64;
      Current_Block      : Unsigned_64;
      Current_Epoch      : Unsigned_64;

      --  Open entry tracking
      Open_Entry_Active  : Boolean;
      Post_Genesis_Count : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Apply_Result is (
      Application_Submitted,
      Insufficient_Specs,
      Already_Applied,
      Genesis_Period_Closed,
      Invalid_Region,
      Invalid_Stake
   );

   type Register_Result is (
      Registered,
      Not_Approved,
      Insufficient_Stake,
      Stake_Cap_Exceeded,
      Invalid_Signature
   );

   type Slash_Result is (
      Slashed,
      Already_Jailed,
      Invalid_Evidence,
      Not_Validator
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize validator set
   procedure Init_Validator_Set (
      State          : out Validator_Set_State;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Post => State.Active_Count = 0
              and State.Remaining_Pool = Validator_Allocation
              and not State.Open_Entry_Active;

   ---------------------------------------------------------------------------
   --  Application Process
   ---------------------------------------------------------------------------

   --  Submit genesis validator application
   procedure Submit_Application (
      Application    : Validator_Application;
      Current_Block  : Unsigned_64;
      Result         : out Apply_Result
   ) with
      Global => null;

   --  Review application
   procedure Review_Application (
      Application    : in Out Validator_Application;
      Reviewer       : Byte_Array;
      Approved       : Boolean;
      Notes_Hash     : Byte_Array;
      Result         : out Application_Status
   ) with
      Global => null,
      Pre => Reviewer'Length = 32
             and Notes_Hash'Length = 32;

   --  Verify hardware meets minimum requirements
   function Verify_Hardware (
      Specs          : Hardware_Specs
   ) return Boolean with
      Global => null,
      Post => Verify_Hardware'Result =
              (Specs.CPU_Cores >= Min_CPU_Cores
               and Specs.RAM_GB >= Min_RAM_GB
               and Specs.Storage_TB >= Min_Storage_TB
               and Specs.Network_Mbps >= Min_Network_Mbps);

   --  Check regional distribution
   function Check_Regional_Balance (
      State          : Validator_Set_State;
      Region         : Geographic_Region
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Validator Registration
   ---------------------------------------------------------------------------

   --  Register approved validator
   procedure Register_Validator (
      State          : in Out Validator_Set_State;
      Application    : Validator_Application;
      Stake          : Unsigned_64;
      Consensus_PK   : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Register_Result
   ) with
      Global => null,
      Pre => Consensus_PK'Length = 64
             and Signature'Length >= 64
             and Stake >= Min_Stake;

   --  Add stake to validator
   procedure Add_Stake (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Amount         : Unsigned_64;
      Is_Self        : Boolean;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Amount > 0;

   --  Begin unstaking
   procedure Begin_Unstake (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Amount         : Unsigned_64;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Amount > 0;

   --  Complete unstaking (after unbonding period)
   procedure Complete_Unstake (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Current_Block  : Unsigned_64;
      Released       : out Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Block Production
   ---------------------------------------------------------------------------

   --  Record block produced
   procedure Record_Block_Produced (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Block_Number   : Unsigned_64
   ) with
      Global => null;

   --  Record missed block
   procedure Record_Block_Missed (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Block_Number   : Unsigned_64
   ) with
      Global => null;

   --  Calculate block reward
   function Calculate_Block_Reward (
      State          : Validator_Set_State
   ) return Unsigned_64 with
      Global => null;

   --  Distribute block reward
   procedure Distribute_Block_Reward (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Block_Reward   : Unsigned_64
   ) with
      Global => null;

   --  Claim pending rewards
   procedure Claim_Rewards (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Claimed        : out Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Slashing
   ---------------------------------------------------------------------------

   --  Slash for double signing
   procedure Slash_Double_Sign (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Evidence_Hash  : Byte_Array;
      Result         : out Slash_Result
   ) with
      Global => null,
      Pre => Evidence_Hash'Length = 32;

   --  Slash for downtime
   procedure Slash_Downtime (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Missed_Blocks  : Natural;
      Result         : out Slash_Result
   ) with
      Global => null;

   --  Jail validator
   procedure Jail_Validator (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Jail_Duration  : Unsigned_64
   ) with
      Global => null;

   --  Unjail validator
   procedure Unjail_Validator (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Validator Queries
   ---------------------------------------------------------------------------

   --  Get validator by index
   function Get_Validator (
      State          : Validator_Set_State;
      Index          : Validator_Index
   ) return Validator_Record with
      Global => null;

   --  Get validator by address
   procedure Find_Validator (
      State          : Validator_Set_State;
      Address        : Byte_Array;
      Index          : out Validator_Index;
      Found          : out Boolean
   ) with
      Global => null,
      Pre => Address'Length = 32;

   --  Get active validator count
   function Get_Active_Count (
      State          : Validator_Set_State
   ) return Natural with
      Global => null,
      Post => Get_Active_Count'Result = State.Active_Count;

   --  Get total stake
   function Get_Total_Stake (
      State          : Validator_Set_State
   ) return Unsigned_64 with
      Global => null,
      Post => Get_Total_Stake'Result = State.Total_Stake;

   --  Calculate validator uptime
   function Calculate_Uptime (
      Validator      : Validator_Record
   ) return Natural with
      Global => null,
      Post => Calculate_Uptime'Result <= 10000;

   --  Check if validator meets uptime requirement
   function Meets_Uptime_Requirement (
      Validator      : Validator_Record
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Open Entry (Post-Genesis)
   ---------------------------------------------------------------------------

   --  Enable open entry
   procedure Enable_Open_Entry (
      State          : in Out Validator_Set_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => Current_Block >= Open_Entry_Block,
      Post => State.Open_Entry_Active;

   --  Check if open entry is active
   function Is_Open_Entry (
      State          : Validator_Set_State;
      Current_Block  : Unsigned_64
   ) return Boolean with
      Global => null;

   --  Register via open entry (stake-based)
   procedure Register_Open_Entry (
      State          : in Out Validator_Set_State;
      Address        : Byte_Array;
      Stake          : Unsigned_64;
      Consensus_PK   : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Register_Result
   ) with
      Global => null,
      Pre => Address'Length = 32
             and Consensus_PK'Length = 64
             and Signature'Length >= 64
             and Stake >= Min_Stake;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Validator_Set (
      State          : Validator_Set_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 65536;

   procedure Deserialize_Validator_Set (
      Input          : Byte_Array;
      State          : out Validator_Set_State;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Validator_Set (State : out Validator_Set_State) with
      Global => null;

   procedure Zeroize_Validator (V : out Validator_Record) with
      Global => null;

   procedure Zeroize_Application (A : out Validator_Application) with
      Global => null;

end Anubis_Genesis_Validators;
