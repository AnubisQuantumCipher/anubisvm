-------------------------------------------------------------------------------
--  ANUBIS Quantum Insurance Reserve
--  5% Allocation (50M ANUBIS) Immutably Locked for Cryptographic Migration
--
--  This reserve exists to fund emergency cryptographic migration if/when
--  a cryptographically relevant quantum computer (CRQC) becomes a reality.
--
--  Parameters:
--  - Allocation: 50,000,000 ANUBIS (5%)
--  - Builder Access: NONE — immutable smart contract
--  - Trigger: Oracle attestation of quantum threat
--  - Release: DAO vote (67% + 25% quorum)
--  - Sunset: 20 years
--
--  The reserve is IMMUTABLY LOCKED. Not even the builder can touch it.
--  Only a verified quantum threat + DAO supermajority can release funds.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Quantum_Insurance with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Allocation
   Insurance_Allocation       : constant := 50_000_000;  -- 5% of 1B

   --  Access control
   Builder_Access             : constant Boolean := False;  -- NEVER

   --  Release requirements (basis points)
   Release_Vote_Threshold_BP  : constant := 6700;   -- 67%
   Release_Quorum_BP          : constant := 2500;   -- 25%

   --  Timing
   Sunset_Blocks              : constant := 105_120_000;  -- 20 years
   Voting_Period_Blocks       : constant := 604_800;      -- 42 days
   Execution_Timelock_Blocks  : constant := 201_600;      -- 14 days

   --  Threat levels
   type Quantum_Threat_Level is (
      None,           -- No credible threat
      Research,       -- Academic progress noted
      Development,    -- Nation-state development programs
      Imminent,       -- CRQC expected within 5 years
      Active          -- CRQC confirmed operational
   );

   --  Required threat level for release
   Min_Release_Threat_Level   : constant Quantum_Threat_Level := Imminent;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Oracle_Attestation is record
      Timestamp       : Unsigned_64;
      Block_Number    : Unsigned_64;
      Threat_Level    : Quantum_Threat_Level;
      Evidence_Hash   : Byte_Array (0 .. 31);  -- Hash of evidence document
      Oracle_PK       : Byte_Array (0 .. 31);
      Signature       : Byte_Array (0 .. 127);
      IPFS_Evidence   : Byte_Array (0 .. 45);
   end record;

   type Release_Proposal is record
      ID              : Unsigned_64;
      Proposer        : Byte_Array (0 .. 31);
      Attestation     : Oracle_Attestation;
      Amount          : Unsigned_64;
      Recipient       : Byte_Array (0 .. 31);  -- Migration contract address
      Purpose_Hash    : Byte_Array (0 .. 31);

      --  Voting
      Votes_For       : Unsigned_64;
      Votes_Against   : Unsigned_64;
      Unique_Voters   : Unsigned_32;

      --  Timing
      Created_At      : Unsigned_64;
      Voting_End      : Unsigned_64;
      Execution_After : Unsigned_64;

      --  Status
      Passed          : Boolean;
      Executed        : Boolean;
      Expired         : Boolean;
   end record;

   type Insurance_State is record
      --  Balance
      Total_Allocation   : Unsigned_64;
      Current_Balance    : Unsigned_64;
      Total_Released     : Unsigned_64;

      --  Current threat assessment
      Current_Threat     : Quantum_Threat_Level;
      Last_Attestation   : Oracle_Attestation;

      --  Active proposal
      Has_Active_Proposal: Boolean;
      Active_Proposal    : Release_Proposal;

      --  Timing
      Genesis_Block      : Unsigned_64;
      Current_Block      : Unsigned_64;
      Sunset_Block       : Unsigned_64;

      --  Security
      Lock_Hash          : Byte_Array (0 .. 31);  -- Proves immutability
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Attest_Result is (
      Attestation_Recorded,
      Invalid_Oracle,
      Invalid_Signature,
      Lower_Than_Current,
      Too_Recent
   );

   type Propose_Result is (
      Proposed,
      Threat_Level_Insufficient,
      Active_Proposal_Exists,
      Insurance_Exhausted,
      Invalid_Recipient,
      Sunset_Reached
   );

   type Release_Result is (
      Released,
      Vote_Not_Passed,
      Timelock_Active,
      Already_Executed,
      Insufficient_Balance
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize quantum insurance
   --  CRITICAL: This sets up the IMMUTABLE lock
   procedure Init_Quantum_Insurance (
      State          : out Insurance_State;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Post => State.Total_Allocation = Insurance_Allocation
              and State.Current_Balance = Insurance_Allocation
              and State.Current_Threat = None
              and State.Sunset_Block = Genesis_Block + Sunset_Blocks;

   ---------------------------------------------------------------------------
   --  Threat Attestation
   ---------------------------------------------------------------------------

   --  Record oracle threat attestation
   procedure Record_Attestation (
      State          : in Out Insurance_State;
      Attestation    : Oracle_Attestation;
      Result         : out Attest_Result
   ) with
      Global => null;

   --  Verify oracle signature
   function Verify_Oracle_Signature (
      Attestation    : Oracle_Attestation;
      Oracle_PK      : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Oracle_PK'Length = 2592;

   --  Get current threat level
   function Get_Threat_Level (
      State          : Insurance_State
   ) return Quantum_Threat_Level with
      Global => null,
      Post => Get_Threat_Level'Result = State.Current_Threat;

   ---------------------------------------------------------------------------
   --  Release Proposals
   ---------------------------------------------------------------------------

   --  Propose release of funds
   procedure Propose_Release (
      State          : in Out Insurance_State;
      Proposer       : Byte_Array;
      Amount         : Unsigned_64;
      Recipient      : Byte_Array;
      Purpose_Hash   : Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Propose_Result
   ) with
      Global => null,
      Pre => Proposer'Length = 32
             and Recipient'Length = 32
             and Purpose_Hash'Length = 32
             and Amount > 0;

   --  Cast vote on release proposal
   procedure Vote_Release (
      State          : in Out Insurance_State;
      Voter          : Byte_Array;
      Vote_Amount    : Unsigned_64;
      In_Favor       : Boolean;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Voter'Length = 32
             and Vote_Amount > 0
             and State.Has_Active_Proposal;

   --  Finalize voting
   procedure Finalize_Vote (
      State          : in Out Insurance_State;
      Circulating_Supply : Unsigned_64;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Has_Active_Proposal;

   --  Execute release (after timelock)
   procedure Execute_Release (
      State          : in Out Insurance_State;
      Current_Block  : Unsigned_64;
      Result         : out Release_Result
   ) with
      Global => null,
      Pre => State.Has_Active_Proposal
             and State.Active_Proposal.Passed;

   ---------------------------------------------------------------------------
   --  Access Control Verification
   ---------------------------------------------------------------------------

   --  CRITICAL: These functions enforce immutability
   --  They ALWAYS return false for builder addresses

   --  Check if address is builder (for blocking)
   function Is_Builder (
      Address        : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Address'Length = 32
             and Builder_PK'Length = 32;

   --  Verify release is to valid migration contract (not builder)
   function Is_Valid_Recipient (
      Recipient      : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Recipient'Length = 32
             and Builder_PK'Length = 32,
      Post => Is_Valid_Recipient'Result =
              (not Is_Builder (Recipient, Builder_PK));

   --  Verify lock integrity
   function Verify_Lock_Integrity (
      State          : Insurance_State
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Sunset Handling
   ---------------------------------------------------------------------------

   --  Check if sunset has been reached
   function Is_Sunset (
      State          : Insurance_State;
      Current_Block  : Unsigned_64
   ) return Boolean with
      Global => null,
      Post => Is_Sunset'Result = (Current_Block >= State.Sunset_Block);

   --  Handle sunset (transfer to DAO treasury)
   procedure Handle_Sunset (
      State          : in Out Insurance_State;
      DAO_Treasury   : Byte_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => DAO_Treasury'Length = 32
             and Current_Block >= State.Sunset_Block;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   --  Get current balance
   function Get_Balance (
      State          : Insurance_State
   ) return Unsigned_64 with
      Global => null,
      Post => Get_Balance'Result = State.Current_Balance;

   --  Get total released
   function Get_Total_Released (
      State          : Insurance_State
   ) return Unsigned_64 with
      Global => null,
      Post => Get_Total_Released'Result = State.Total_Released;

   --  Get blocks until sunset
   function Blocks_Until_Sunset (
      State          : Insurance_State;
      Current_Block  : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   --  Check if release is possible
   function Can_Release (
      State          : Insurance_State
   ) return Boolean with
      Global => null,
      Post => Can_Release'Result =
              (State.Current_Threat >= Min_Release_Threat_Level);

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Insurance_State (
      State          : Insurance_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 1024;

   procedure Deserialize_Insurance_State (
      Input          : Byte_Array;
      State          : out Insurance_State;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Insurance_State (State : out Insurance_State) with
      Global => null;

   procedure Zeroize_Attestation (A : out Oracle_Attestation) with
      Global => null;

   procedure Zeroize_Proposal (P : out Release_Proposal) with
      Global => null;

end Anubis_Quantum_Insurance;
