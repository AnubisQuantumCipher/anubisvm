-------------------------------------------------------------------------------
--  ANUBIS Genesis Provers
--  8% Allocation (80M ANUBIS) Earned via Proof Generation
--
--  Genesis Provers are the initial 50 provers selected via application.
--  They earn tokens through proof rewards over 3 years.
--
--  Parameters:
--  - Allocation: 80,000,000 ANUBIS (8%)
--  - Distribution: Proof rewards over 3 years
--  - Genesis Set: First 50 provers (application-based)
--  - Open Entry: After 6 months (stake-based)
--  - Tiered Hardware: Light (16GB) to Enterprise (128GB)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Genesis_Provers with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Prover_Allocation          : constant := 80_000_000;   -- 8% of 1B
   Genesis_Prover_Count       : constant := 50;
   Open_Entry_Block           : constant := 2_628_000;    -- 6 months
   Distribution_Period_Blocks : constant := 15_768_000;   -- 3 years

   --  Hardware tiers
   type Hardware_Tier is (
      Light,        -- 16GB RAM - basic proofs
      Standard,     -- 32GB RAM - standard proofs
      Heavy,        -- 64GB RAM - complex proofs
      Enterprise    -- 128GB RAM - full capability
   );

   Tier_RAM_Requirements : constant array (Hardware_Tier) of Natural := (
      Light      => 16,
      Standard   => 32,
      Heavy      => 64,
      Enterprise => 128
   );

   --  Reward multipliers per tier (basis points, 10000 = 1x)
   Tier_Reward_Multiplier : constant array (Hardware_Tier) of Natural := (
      Light      => 5000,   -- 0.5x
      Standard   => 10000,  -- 1.0x
      Heavy      => 15000,  -- 1.5x
      Enterprise => 20000   -- 2.0x
   );

   subtype Prover_Index is Natural range 0 .. Genesis_Prover_Count - 1;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Prover_Status is (
      Pending_Application,
      Approved,
      Active,
      Busy,
      Suspended,
      Slashed,
      Removed
   );

   type Prover_Record is record
      Index              : Prover_Index;
      Address            : Byte_Array (0 .. 31);
      PK_Hash            : Byte_Array (0 .. 31);
      Status             : Prover_Status;
      Tier               : Hardware_Tier;
      Is_Genesis         : Boolean;

      --  Performance
      Proofs_Generated   : Unsigned_64;
      Proofs_Failed      : Unsigned_64;
      Avg_Proof_Time_Ms  : Unsigned_32;
      Total_Rewards      : Unsigned_64;
      Pending_Rewards    : Unsigned_64;

      --  Stake
      Stake_Amount       : Unsigned_64;
      Times_Slashed      : Natural;
      Total_Slashed      : Unsigned_64;

      --  Timing
      Registered_At      : Unsigned_64;
      Active_Since       : Unsigned_64;
      Last_Proof_Block   : Unsigned_64;
   end record;

   type Prover_Array is array (Prover_Index) of Prover_Record;

   type Prover_Set_State is record
      Provers            : Prover_Array;
      Active_Count       : Natural;
      Genesis_Count      : Natural;
      Total_Distributed  : Unsigned_64;
      Remaining_Pool     : Unsigned_64;
      Open_Entry_Active  : Boolean;
      Genesis_Block      : Unsigned_64;
      Current_Block      : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Register_Result is (
      Registered,
      Not_Approved,
      Insufficient_Stake,
      Invalid_Hardware,
      Invalid_Signature
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Prover_Set (
      State          : out Prover_Set_State;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Post => State.Active_Count = 0
              and State.Remaining_Pool = Prover_Allocation;

   ---------------------------------------------------------------------------
   --  Prover Registration
   ---------------------------------------------------------------------------

   procedure Register_Prover (
      State          : in Out Prover_Set_State;
      Address        : Byte_Array;
      PK_Hash        : Byte_Array;
      Tier           : Hardware_Tier;
      Stake          : Unsigned_64;
      Is_Genesis     : Boolean;
      Result         : out Register_Result
   ) with
      Global => null,
      Pre => Address'Length = 32
             and PK_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Proof Recording
   ---------------------------------------------------------------------------

   procedure Record_Proof (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Proof_Size     : Unsigned_32;
      Proof_Time_Ms  : Unsigned_32;
      Block_Number   : Unsigned_64
   ) with
      Global => null;

   procedure Record_Proof_Failed (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Reason_Hash    : Byte_Array
   ) with
      Global => null,
      Pre => Reason_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Rewards
   ---------------------------------------------------------------------------

   function Calculate_Proof_Reward (
      State          : Prover_Set_State;
      Tier           : Hardware_Tier;
      Proof_Size     : Unsigned_32
   ) return Unsigned_64 with
      Global => null;

   procedure Distribute_Proof_Reward (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Reward         : Unsigned_64
   ) with
      Global => null;

   procedure Claim_Rewards (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Claimed        : out Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Slashing
   ---------------------------------------------------------------------------

   procedure Slash_Prover (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Slash_Rate_BP  : Natural;
      Reason_Hash    : Byte_Array
   ) with
      Global => null,
      Pre => Reason_Hash'Length = 32
             and Slash_Rate_BP <= 10000;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Prover (
      State          : Prover_Set_State;
      Index          : Prover_Index
   ) return Prover_Record with
      Global => null;

   function Get_Active_Count (
      State          : Prover_Set_State
   ) return Natural with
      Global => null;

   ---------------------------------------------------------------------------
   --  Open Entry
   ---------------------------------------------------------------------------

   procedure Enable_Open_Entry (
      State          : in Out Prover_Set_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => Current_Block >= Open_Entry_Block;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Prover_Set (State : out Prover_Set_State) with
      Global => null;

   procedure Zeroize_Prover (P : out Prover_Record) with
      Global => null;

end Anubis_Genesis_Provers;
