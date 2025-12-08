-------------------------------------------------------------------------------
--  ANUBIS Dead Man"s Switch Governance
--  Survival Mechanism for Solo Builder Project
--
--  The biggest risk of a solo builder project: what happens if you get hit
--  by a bus? The Dead Man"s Switch ensures ANUBIS survives you.
--
--  The Heartbeat System:
--  - Heartbeat Requirement: On-chain transaction every 30 days
--  - Grace Period: 30 additional days after missed heartbeat
--  - Total Absence Allowed: 60 days before trigger
--  - Trigger Action: Full control transfers to DAO
--
--  Builder Privileges (Pre-Handoff):
--  - Protocol upgrades: Until Year 2 -> DAO vote required
--  - Emergency pause (72h max): Until Year 2 -> DAO 5-of-9 council
--  - Parameter tuning (±20%): Until Year 1 -> DAO vote required
--  - Bug bounty approval: Until Year 1 -> DAO vote required
--  - Validator removal: Until Year 1 -> DAO vote required
--
--  Recovery Mechanism: If builder returns after trigger, they can prove
--  identity via ML-DSA-87 signature and DAO votes (67%) to restore.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Dead_Man_Switch with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Timing (in blocks, assuming 6-second block time)
   Heartbeat_Period_Blocks    : constant := 432_000;   -- 30 days
   Grace_Period_Blocks        : constant := 432_000;   -- 30 days
   Total_Absence_Blocks       : constant := 864_000;   -- 60 days total

   --  Emergency pause limits
   Max_Emergency_Pause_Blocks : constant := 43_200;    -- 72 hours

   --  Parameter tuning limits (basis points)
   Max_Parameter_Change_BP    : constant := 2000;      -- ±20%

   --  Governance phase boundaries (in blocks)
   --  Assuming 6-second blocks: 1 year ≈ 5,256,000 blocks
   Phase1_End_Block           : constant := 5_256_000;    -- Year 1
   Phase2_End_Block           : constant := 10_512_000;   -- Year 2
   Phase3_End_Block           : constant := 15_768_000;   -- Year 3

   --  Council requirements
   Council_Size               : constant := 9;
   Council_Threshold          : constant := 5;         -- 5-of-9 for emergency

   --  Recovery vote requirements (basis points)
   Recovery_Vote_Threshold_BP : constant := 6700;      -- 67%
   Recovery_Quorum_BP         : constant := 1500;      -- 15%

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Builder privilege types
   type Privilege_Type is (
      Protocol_Upgrade,
      Emergency_Pause,
      Parameter_Tuning,
      Bug_Bounty_Approval,
      Validator_Removal,
      Treasury_Access  -- Never granted to builder
   );

   --  Governance phases
   type Governance_Phase is (
      Phase1_Builder_Control,    -- Year 0-1
      Phase2_Shared_Control,     -- Year 1-2
      Phase3_DAO_Control,        -- Year 2-3
      Phase4_Full_Autonomy       -- Year 3+
   );

   --  Dead Man"s Switch states
   type Switch_State is (
      Active,           -- Builder is active, heartbeat current
      Warning,          -- Heartbeat missed, in grace period
      Triggered,        -- Switch triggered, DAO takeover complete
      Recovered         -- Builder returned and was restored
   );

   --  Emergency pause state
   type Pause_State is (
      Not_Paused,
      Paused_By_Builder,
      Paused_By_Council,
      Paused_By_DAO
   );

   --  Recovery proposal state
   type Recovery_State is (
      None,
      Proposed,
      Voting,
      Approved,
      Rejected,
      Executed
   );

   subtype Council_Index is Natural range 0 .. Council_Size - 1;

   ---------------------------------------------------------------------------
   --  Heartbeat Record
   ---------------------------------------------------------------------------

   type Heartbeat_Record is record
      Builder_Address  : Byte_Array (0 .. 31);
      Last_Heartbeat   : Unsigned_64;        -- Block number of last heartbeat
      Heartbeat_Count  : Unsigned_64;        -- Total heartbeats recorded
      Signature        : Byte_Array (0 .. 127); -- ML-DSA signature
      Message_Hash     : Byte_Array (0 .. 31);  -- Hash of heartbeat message
   end record;

   ---------------------------------------------------------------------------
   --  Switch System State
   ---------------------------------------------------------------------------

   type Dead_Man_State is record
      --  Core state
      Builder_PK_Hash     : Byte_Array (0 .. 31);  -- Hash of builder"s ML-DSA PK
      State               : Switch_State;
      Current_Phase       : Governance_Phase;
      Current_Block       : Unsigned_64;
      Genesis_Block       : Unsigned_64;

      --  Heartbeat tracking
      Last_Heartbeat      : Unsigned_64;
      Grace_Period_Start  : Unsigned_64;
      Trigger_Block       : Unsigned_64;

      --  Emergency state
      Current_Pause       : Pause_State;
      Pause_Start         : Unsigned_64;
      Pause_End           : Unsigned_64;
      Pause_Reason        : Byte_Array (0 .. 255);

      --  Recovery tracking
      Current_Recovery    : Recovery_State;
      Recovery_Votes_For  : Unsigned_64;
      Recovery_Votes_Against : Unsigned_64;
      Recovery_Deadline   : Unsigned_64;

      --  Statistics
      Total_Heartbeats    : Unsigned_64;
      Warnings_Issued     : Unsigned_32;
      Times_Recovered     : Unsigned_32;
   end record;

   ---------------------------------------------------------------------------
   --  Council State
   ---------------------------------------------------------------------------

   type Council_Member is record
      Address         : Byte_Array (0 .. 31);
      PK_Hash         : Byte_Array (0 .. 31);
      Elected_At      : Unsigned_64;
      Active          : Boolean;
      Votes_Cast      : Unsigned_32;
   end record;

   type Council_Array is array (Council_Index) of Council_Member;

   type Council_State is record
      Members         : Council_Array;
      Active_Count    : Natural;
      Formation_Block : Unsigned_64;
      Last_Election   : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Privilege Tracking
   ---------------------------------------------------------------------------

   type Privilege_Record is record
      Privilege       : Privilege_Type;
      Holder          : Byte_Array (0 .. 31);  -- Current holder address
      Expires_At      : Unsigned_64;           -- Block when privilege expires
      Transferred     : Boolean;
      Transfer_Block  : Unsigned_64;
   end record;

   type Privilege_Array is array (Privilege_Type) of Privilege_Record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Heartbeat_Result is (
      Recorded,
      Invalid_Signature,
      Not_Builder,
      Switch_Already_Triggered
   );

   type Pause_Result is (
      Paused,
      Already_Paused,
      Unauthorized,
      Duration_Exceeded,
      Phase_Not_Allowed
   );

   type Recovery_Result is (
      Recovery_Initiated,
      Identity_Verified,
      Vote_Recorded,
      Approved,
      Rejected,
      Invalid_Proof,
      Already_Active,
      Vote_Ended
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize dead man"s switch
   procedure Init_Switch (
      State          : out Dead_Man_State;
      Builder_PK     : Byte_Array;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => Builder_PK'Length = 32,
      Post => State.State = Active
              and State.Current_Phase = Phase1_Builder_Control
              and State.Last_Heartbeat = Genesis_Block;

   --  Initialize council
   procedure Init_Council (
      Council        : out Council_State;
      Formation_Block: Unsigned_64
   ) with
      Global => null,
      Post => Council.Active_Count = 0;

   ---------------------------------------------------------------------------
   --  Heartbeat Operations
   ---------------------------------------------------------------------------

   --  Record builder heartbeat
   procedure Record_Heartbeat (
      State          : in Out Dead_Man_State;
      Signature      : Byte_Array;
      Message        : Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Heartbeat_Result
   ) with
      Global => null,
      Pre => Signature'Length >= 64
             and Message'Length >= 32,
      Post => (if Result = Recorded then
                  State.Last_Heartbeat = Current_Block
                  and State.State = Active);

   --  Check heartbeat status
   procedure Check_Heartbeat_Status (
      State          : in Out Dead_Man_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null;

   --  Get blocks until next required heartbeat
   function Blocks_Until_Heartbeat (
      State          : Dead_Man_State;
      Current_Block  : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   --  Check if heartbeat is overdue
   function Is_Heartbeat_Overdue (
      State          : Dead_Man_State;
      Current_Block  : Unsigned_64
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Switch Trigger
   ---------------------------------------------------------------------------

   --  Trigger the dead man"s switch (automatic)
   procedure Trigger_Switch (
      State          : in Out Dead_Man_State;
      Privileges     : in Out Privilege_Array;
      Current_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => State.State = Warning,
      Post => State.State = Triggered
              and State.Trigger_Block = Current_Block;

   --  Check if switch should trigger
   function Should_Trigger (
      State          : Dead_Man_State;
      Current_Block  : Unsigned_64
   ) return Boolean with
      Global => null,
      Post => Should_Trigger'Result =
              (State.State = Warning
               and Current_Block >= State.Grace_Period_Start + Grace_Period_Blocks);

   --  Transfer all privileges to DAO
   procedure Transfer_All_Privileges (
      Privileges     : in Out Privilege_Array;
      DAO_Address    : Byte_Array;
      Current_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => DAO_Address'Length = 32;

   ---------------------------------------------------------------------------
   --  Builder Recovery
   ---------------------------------------------------------------------------

   --  Initiate recovery (builder proves identity)
   procedure Initiate_Recovery (
      State          : in Out Dead_Man_State;
      Identity_Proof : Byte_Array;
      Builder_Sig    : Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Recovery_Result
   ) with
      Global => null,
      Pre => Identity_Proof'Length >= 64
             and Builder_Sig'Length >= 64,
      Post => (if Result = Recovery_Initiated then
                  State.Current_Recovery = Voting);

   --  Cast recovery vote
   procedure Vote_Recovery (
      State          : in Out Dead_Man_State;
      Voter          : Byte_Array;
      Vote_Amount    : Unsigned_64;
      In_Favor       : Boolean;
      Current_Block  : Unsigned_64;
      Result         : out Recovery_Result
   ) with
      Global => null,
      Pre => Voter'Length = 32
             and Vote_Amount > 0
             and State.Current_Recovery = Voting;

   --  Finalize recovery vote
   procedure Finalize_Recovery (
      State          : in Out Dead_Man_State;
      Privileges     : in Out Privilege_Array;
      Builder_Address: Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Recovery_Result
   ) with
      Global => null,
      Pre => Builder_Address'Length = 32
             and State.Current_Recovery = Voting;

   ---------------------------------------------------------------------------
   --  Governance Phase Management
   ---------------------------------------------------------------------------

   --  Update governance phase
   procedure Update_Phase (
      State          : in Out Dead_Man_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null;

   --  Get current governance phase
   function Get_Phase (
      State          : Dead_Man_State
   ) return Governance_Phase with
      Global => null,
      Post => Get_Phase'Result = State.Current_Phase;

   --  Check if privilege is available in current phase
   function Privilege_Available (
      State          : Dead_Man_State;
      Privilege      : Privilege_Type
   ) return Boolean with
      Global => null;

   --  Get privilege expiration block
   function Privilege_Expires_At (
      Privilege      : Privilege_Type
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Emergency Pause
   ---------------------------------------------------------------------------

   --  Builder emergency pause
   procedure Builder_Emergency_Pause (
      State          : in Out Dead_Man_State;
      Reason         : Byte_Array;
      Duration       : Unsigned_64;
      Current_Block  : Unsigned_64;
      Result         : out Pause_Result
   ) with
      Global => null,
      Pre => Reason'Length <= 256
             and Duration <= Max_Emergency_Pause_Blocks;

   --  Council emergency pause
   procedure Council_Emergency_Pause (
      State          : in Out Dead_Man_State;
      Council        : Council_State;
      Signatures     : Byte_Array;
      Reason         : Byte_Array;
      Duration       : Unsigned_64;
      Current_Block  : Unsigned_64;
      Result         : out Pause_Result
   ) with
      Global => null,
      Pre => Reason'Length <= 256
             and Duration <= Max_Emergency_Pause_Blocks;

   --  Unpause
   procedure Unpause (
      State          : in Out Dead_Man_State;
      Authorizer     : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Pause_Result
   ) with
      Global => null,
      Pre => Authorizer'Length = 32
             and Signature'Length >= 64;

   --  Check pause status
   function Is_Paused (State : Dead_Man_State) return Boolean with
      Global => null,
      Post => Is_Paused'Result = (State.Current_Pause /= Not_Paused);

   ---------------------------------------------------------------------------
   --  Parameter Tuning
   ---------------------------------------------------------------------------

   type Parameter_ID is (
      Block_Gas_Limit,
      Min_Stake,
      Slash_Rate,
      Unbonding_Period,
      Proof_Timeout,
      Privacy_Fee
   );

   --  Tune protocol parameter (builder only, Year 1)
   procedure Tune_Parameter (
      State          : Dead_Man_State;
      Parameter      : Parameter_ID;
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64;
      Builder_Sig    : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Builder_Sig'Length >= 64;

   --  Check if parameter change is within limits
   function Is_Valid_Change (
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Council Management
   ---------------------------------------------------------------------------

   --  Add council member
   procedure Add_Council_Member (
      Council        : in Out Council_State;
      Member_Address : Byte_Array;
      Member_PK      : Byte_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Member_Address'Length = 32
             and Member_PK'Length = 32
             and Council.Active_Count < Council_Size;

   --  Remove council member
   procedure Remove_Council_Member (
      Council        : in Out Council_State;
      Index          : Council_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Council.Members (Index).Active;

   --  Check council threshold
   function Check_Council_Threshold (
      Council        : Council_State;
      Signers        : Natural
   ) return Boolean with
      Global => null,
      Post => Check_Council_Threshold'Result = (Signers >= Council_Threshold);

   ---------------------------------------------------------------------------
   --  Privilege Queries
   ---------------------------------------------------------------------------

   --  Get privilege holder
   function Get_Privilege_Holder (
      Privileges     : Privilege_Array;
      Privilege      : Privilege_Type
   ) return Byte_Array with
      Global => null;

   --  Check if address has privilege
   function Has_Privilege (
      Privileges     : Privilege_Array;
      Privilege      : Privilege_Type;
      Address        : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Address'Length = 32;

   ---------------------------------------------------------------------------
   --  Automatic Handoff Schedule (from Tokenomics v4.0)
   ---------------------------------------------------------------------------

   --  Block 0:        Genesis, builder control     -> Full protocol control
   --  Block 2,628,000: Parameter control → council -> Reduced powers
   --  Block 5,256,000: Upgrades need council       -> Proposer only
   --  Block 7,884,000: Emergency powers → council  -> Equal to council
   --  Block 10,512,000: All builder privileges expire -> Token holder only
   --  Block 15,768,000: Full autonomy              -> Community member

   Handoff_Parameter_Control  : constant := 2_628_000;
   Handoff_Upgrade_Control    : constant := 5_256_000;
   Handoff_Emergency_Control  : constant := 7_884_000;
   Handoff_All_Privileges     : constant := 10_512_000;
   Handoff_Full_Autonomy      : constant := 15_768_000;

   --  Check automatic handoff status
   procedure Check_Handoff (
      State          : in Out Dead_Man_State;
      Privileges     : in Out Privilege_Array;
      Current_Block  : Unsigned_64
   ) with
      Global => null;

   --  Get next handoff block
   function Next_Handoff_Block (
      Current_Block  : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Switch_State (
      State          : Dead_Man_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 1024;

   procedure Deserialize_Switch_State (
      Input          : Byte_Array;
      State          : out Dead_Man_State;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Switch_State (State : out Dead_Man_State) with
      Global => null;

   procedure Zeroize_Council_State (Council : out Council_State) with
      Global => null;

   procedure Zeroize_Heartbeat (Heartbeat : out Heartbeat_Record) with
      Global => null;

end Anubis_Dead_Man_Switch;
