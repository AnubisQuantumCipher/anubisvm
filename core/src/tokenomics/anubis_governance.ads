-------------------------------------------------------------------------------
--  ANUBIS Governance Timeline
--  4-Phase Automatic Handoff from Builder to DAO
--
--  The path from solo builder control to full DAO autonomy is hard-coded
--  into smart contracts. The builder cannot extend their own power.
--
--  Governance Phases:
--  - Phase 1 (Year 0-1): Builder Control - Limited by constraints
--  - Phase 2 (Year 1-2): Shared Control - Council approval required
--  - Phase 3 (Year 2-3): DAO Control - All privileges expire
--  - Phase 4 (Year 3+):  Full Autonomy - No emergency powers remain
--
--  Automatic Handoff Schedule:
--  - Block 0:          Genesis, builder control
--  - Block 2,628,000:  Parameter control → council
--  - Block 5,256,000:  Upgrades need council approval
--  - Block 7,884,000:  Emergency powers require council
--  - Block 10,512,000: All builder privileges expire
--  - Block 15,768,000: Full autonomy achieved
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Governance with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants - Handoff Schedule (IMMUTABLE)
   ---------------------------------------------------------------------------

   --  Phase boundaries (in blocks, 6-second block time)
   Handoff_Parameter_Control  : constant := 2_628_000;   -- ~6 months
   Handoff_Upgrade_Control    : constant := 5_256_000;   -- ~1 year
   Handoff_Emergency_Control  : constant := 7_884_000;   -- ~1.5 years
   Handoff_All_Privileges     : constant := 10_512_000;  -- ~2 years
   Handoff_Full_Autonomy      : constant := 15_768_000;  -- ~3 years

   --  Phase definitions
   type Governance_Phase is (
      Phase1_Builder_Control,    -- Year 0-1
      Phase2_Shared_Control,     -- Year 1-2
      Phase3_DAO_Control,        -- Year 2-3
      Phase4_Full_Autonomy       -- Year 3+
   );

   --  Privilege types
   type Privilege_Type is (
      Protocol_Upgrade,
      Emergency_Pause,
      Parameter_Tuning,
      Bug_Bounty_Approval,
      Validator_Removal,
      Treasury_Access  -- Builder NEVER has this
   );

   --  Privilege holder
   type Privilege_Holder is (
      Builder,
      Council,
      DAO,
      None
   );

   --  Council size
   Council_Size               : constant := 9;
   Council_Threshold          : constant := 5;  -- 5-of-9

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Privilege record
   type Privilege_Record is record
      Privilege       : Privilege_Type;
      Current_Holder  : Privilege_Holder;
      Expires_At      : Unsigned_64;
      Transferred_At  : Unsigned_64;
      Transfer_To     : Privilege_Holder;
   end record;

   type Privilege_Array is array (Privilege_Type) of Privilege_Record;

   --  Phase 1 limitations
   Phase1_Max_Parameter_Change_BP : constant := 2000;  -- ±20%
   Phase1_Max_Pause_Blocks        : constant := 43_200; -- 72 hours

   --  Council member
   type Council_Member is record
      Address         : Byte_Array (0 .. 31);
      PK_Hash         : Byte_Array (0 .. 31);
      Elected_At      : Unsigned_64;
      Term_Ends       : Unsigned_64;
      Active          : Boolean;
   end record;

   subtype Council_Index is Natural range 0 .. Council_Size - 1;
   type Council_Array is array (Council_Index) of Council_Member;

   --  Governance state
   type Governance_State is record
      Current_Phase    : Governance_Phase;
      Current_Block    : Unsigned_64;
      Genesis_Block    : Unsigned_64;

      --  Privileges
      Privileges       : Privilege_Array;

      --  Council
      Council          : Council_Array;
      Council_Active   : Natural;
      Council_Formed   : Boolean;

      --  Statistics
      Upgrades_Proposed: Unsigned_64;
      Upgrades_Approved: Unsigned_64;
      Pauses_Invoked   : Unsigned_32;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Action_Result is (
      Approved,
      Phase_Not_Allowed,
      Privilege_Expired,
      Council_Required,
      DAO_Vote_Required,
      Invalid_Signature,
      Exceeds_Limits
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Governance (
      State          : out Governance_State;
      Builder_PK     : Byte_Array;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => Builder_PK'Length = 32,
      Post => State.Current_Phase = Phase1_Builder_Control
              and not State.Council_Formed;

   ---------------------------------------------------------------------------
   --  Phase Management
   ---------------------------------------------------------------------------

   --  Update governance phase based on current block
   procedure Update_Phase (
      State          : in Out Governance_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null;

   --  Get current governance phase
   function Get_Current_Phase (
      State          : Governance_State
   ) return Governance_Phase with
      Global => null;

   --  Calculate phase from block number
   function Calculate_Phase (
      Current_Block  : Unsigned_64
   ) return Governance_Phase with
      Global => null;

   --  Get blocks until next phase transition
   function Blocks_Until_Next_Phase (
      State          : Governance_State
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Privilege Checking
   ---------------------------------------------------------------------------

   --  Check if privilege is available to holder in current phase
   function Has_Privilege (
      State          : Governance_State;
      Privilege      : Privilege_Type;
      Holder         : Privilege_Holder
   ) return Boolean with
      Global => null;

   --  Get current holder of privilege
   function Get_Privilege_Holder (
      State          : Governance_State;
      Privilege      : Privilege_Type
   ) return Privilege_Holder with
      Global => null;

   --  Check if privilege has expired
   function Privilege_Expired (
      State          : Governance_State;
      Privilege      : Privilege_Type
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Privilege Actions
   ---------------------------------------------------------------------------

   --  Execute protocol upgrade
   procedure Execute_Upgrade (
      State          : in Out Governance_State;
      Proposer       : Byte_Array;
      Upgrade_Hash   : Byte_Array;
      Council_Sigs   : Byte_Array;  -- Required in Phase 2+
      DAO_Vote_ID    : Unsigned_64; -- Required in Phase 3+
      Result         : out Action_Result
   ) with
      Global => null,
      Pre => Proposer'Length = 32
             and Upgrade_Hash'Length = 32;

   --  Execute emergency pause
   procedure Execute_Pause (
      State          : in Out Governance_State;
      Initiator      : Byte_Array;
      Duration       : Unsigned_64;
      Reason_Hash    : Byte_Array;
      Council_Sigs   : Byte_Array;  -- Required in Phase 2+
      Result         : out Action_Result
   ) with
      Global => null,
      Pre => Initiator'Length = 32
             and Reason_Hash'Length = 32;

   --  Tune protocol parameter
   procedure Tune_Parameter (
      State          : in Governance_State;
      Initiator      : Byte_Array;
      Parameter_ID   : Natural;
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64;
      Council_Sigs   : Byte_Array;  -- Required in Phase 2+
      Result         : out Action_Result
   ) with
      Global => null,
      Pre => Initiator'Length = 32;

   --  Validate parameter change is within limits
   function Is_Valid_Parameter_Change (
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64;
      Max_Change_BP  : Natural
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Council Management
   ---------------------------------------------------------------------------

   --  Form initial council (at Phase 2 start)
   procedure Form_Council (
      State          : in Out Governance_State;
      Members        : Council_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => not State.Council_Formed,
      Post => (if Success then State.Council_Formed);

   --  Elect council member
   procedure Elect_Council_Member (
      State          : in Out Governance_State;
      Index          : Council_Index;
      Member         : Council_Member;
      Votes          : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null;

   --  Remove council member
   procedure Remove_Council_Member (
      State          : in Out Governance_State;
      Index          : Council_Index;
      Reason_Hash    : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Reason_Hash'Length = 32;

   --  Verify council signatures (5-of-9)
   function Verify_Council_Signatures (
      State          : Governance_State;
      Message_Hash   : Byte_Array;
      Signatures     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Message_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Automatic Handoff
   ---------------------------------------------------------------------------

   --  Process automatic privilege transfers
   procedure Process_Handoffs (
      State          : in Out Governance_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null;

   --  Check if handoff is due
   function Handoff_Due (
      State          : Governance_State;
      Current_Block  : Unsigned_64
   ) return Boolean with
      Global => null;

   --  Get next handoff block
   function Next_Handoff_Block (
      Current_Block  : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  DAO Integration
   ---------------------------------------------------------------------------

   --  Require DAO vote for action
   function Requires_DAO_Vote (
      State          : Governance_State;
      Privilege      : Privilege_Type
   ) return Boolean with
      Global => null;

   --  Verify DAO vote passed
   function Verify_DAO_Vote (
      Vote_ID        : Unsigned_64;
      Required_Quorum: Unsigned_64;
      Required_Threshold: Natural
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   --  Get builder status in current phase
   function Builder_Status (
      State          : Governance_State
   ) return String with
      Global => null;

   --  Check if builder has any remaining privileges
   function Builder_Has_Privileges (
      State          : Governance_State
   ) return Boolean with
      Global => null;

   --  Get phase description
   function Phase_Description (
      Phase          : Governance_Phase
   ) return String with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Governance_State (
      State          : Governance_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 2048;

   procedure Deserialize_Governance_State (
      Input          : Byte_Array;
      State          : out Governance_State;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Governance_State (State : out Governance_State) with
      Global => null;

   procedure Zeroize_Council_Member (M : out Council_Member) with
      Global => null;

end Anubis_Governance;
