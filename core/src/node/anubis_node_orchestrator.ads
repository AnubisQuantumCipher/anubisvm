-------------------------------------------------------------------------------
--  Anubis Node Orchestrator: Complete Node Integration
--
--  This package orchestrates all node components:
--  - Block production and validation
--  - Transaction pool management
--  - State synchronization
--  - Consensus integration
--  - Chain reorganization handling
--  - P2P networking
--  - RPC server
--  - Governance
--
--  Architecture:
--  +------------------+
--  | RPC Server       | <-- External requests
--  +------------------+
--          |
--  +------------------+
--  | Orchestrator     | <-- Coordinates all subsystems
--  +------------------+
--     |   |   |   |
--     v   v   v   v
--  +----+----+----+----+
--  |Pool|Sync|Cons|Gov |
--  +----+----+----+----+
--
--  Block Production Flow:
--  1. Mempool → Select priority transactions
--  2. Builder → Execute and build block
--  3. Consensus → Propose and vote
--  4. Finalize → Commit to state
--  5. Broadcast → Share with peers
--
--  State Sync Flow:
--  1. Detect gap (local < peer height)
--  2. Request headers + blocks
--  3. Validate signatures + state roots
--  4. Execute transactions
--  5. Update local state
--  6. Broadcast sync progress
--
--  Chain Reorg Flow:
--  1. Detect competing chain (higher weight)
--  2. Find common ancestor
--  3. Rollback to ancestor
--  4. Replay new chain
--  5. Update finalized state
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Node_Types; use Anubis_Node_Types;
with Anubis_Node;
with Anubis_RPC_Server;
with Mempool;
with Block_Builder;
with Block_Sync;
with Consensus;
with P2P_Network;
with Anubis_Governance;

package Anubis_Node_Orchestrator with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Orchestrator Configuration
   ---------------------------------------------------------------------------

   type Orchestrator_Config is record
      --  Node identity
      Node_Name        : String (1 .. 64);
      Node_Name_Len    : Natural;

      --  Network
      P2P_Port         : Unsigned_16;
      RPC_Port         : Port_Number;
      Enable_P2P       : Boolean;
      Enable_RPC       : Boolean;

      --  Block production
      Enable_Mining    : Boolean;
      Block_Time       : Unsigned_64;  -- Target block time (seconds)
      Max_Block_Size   : Natural;

      --  Sync configuration
      Sync_Interval    : Unsigned_64;  -- Seconds between sync checks
      Max_Peers        : Natural;
      Fast_Sync        : Boolean;      -- Enable fast sync mode

      --  Governance
      Enable_Governance : Boolean;
      Genesis_Block     : Unsigned_64;
   end record;

   Default_Orchestrator_Config : constant Orchestrator_Config := (
      Node_Name       => (others => ' '),
      Node_Name_Len   => 0,
      P2P_Port        => 26656,
      RPC_Port        => 26659,
      Enable_P2P      => True,
      Enable_RPC      => True,
      Enable_Mining   => True,
      Block_Time      => 6,
      Max_Block_Size  => 1_048_576,  -- 1 MB
      Sync_Interval   => 30,
      Max_Peers       => 50,
      Fast_Sync       => True,
      Enable_Governance => True,
      Genesis_Block   => 0
   );

   ---------------------------------------------------------------------------
   --  Orchestrator State
   ---------------------------------------------------------------------------

   type Sync_Status is (
      Sync_NotStarted,
      Sync_Discovering,
      Sync_Downloading,
      Sync_Validating,
      Sync_Complete,
      Sync_Error
   );

   type Orchestrator_State is record
      Is_Initialized  : Boolean;
      Config          : Orchestrator_Config;

      --  Component states
      VM              : Anubis_Node.VM_Instance;
      RPC             : Anubis_RPC_Server.RPC_Server_State;
      Pool            : Mempool.Mempool_State;
      Builder         : Block_Builder.Builder_State;
      Sync_Mgr        : Block_Sync.Sync_Manager;
      Consensus       : Consensus.Consensus_State;
      Network         : P2P_Network.Network_State;
      Governance      : Anubis_Governance.Governance_State;

      --  Current state
      Latest_Block_Num : U256;
      Latest_Block_Hash : Hash256;
      Sync_State       : Sync_Status;
      Is_Validator     : Boolean;
      Is_Syncing       : Boolean;

      --  Statistics
      Blocks_Produced  : Unsigned_64;
      Blocks_Validated : Unsigned_64;
      TX_Processed     : Unsigned_64;
      Reorgs_Handled   : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Orchestrator : out Orchestrator_State;
      Config       : in  Orchestrator_Config
   ) with
      Global => null,
      Post   => Orchestrator.Is_Initialized;

   procedure Start (
      Orchestrator : in Out Orchestrator_State
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   procedure Stop (
      Orchestrator : in Out Orchestrator_State
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Block Production
   ---------------------------------------------------------------------------

   type Production_Result is (
      Prod_Success,
      Prod_No_Transactions,
      Prod_Not_Validator,
      Prod_Already_Building,
      Prod_Consensus_Failed,
      Prod_Broadcast_Failed
   );

   --  Produce a new block (validator only)
   procedure Produce_Block (
      Orchestrator : in Out Orchestrator_State;
      Result       : out    Production_Result
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   --  Validate received block
   procedure Validate_Block (
      Orchestrator : in Out Orchestrator_State;
      Block_Data   : in     Block_Builder.Block;
      Valid        : out    Boolean;
      Error_Msg    : out    String
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   --  Finalize block after consensus
   procedure Finalize_Block (
      Orchestrator : in Out Orchestrator_State;
      Block_Num    : in     U256;
      Block_Hash   : in     Hash256
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   ---------------------------------------------------------------------------
   --  State Synchronization
   ---------------------------------------------------------------------------

   type Sync_Result is (
      Sync_OK,
      Sync_No_Peers,
      Sync_Failed,
      Sync_Already_Syncing,
      Sync_Complete_Already
   );

   --  Start state synchronization with peers
   procedure Start_Sync (
      Orchestrator : in Out Orchestrator_State;
      Result       : out    Sync_Result
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   --  Process incoming block during sync
   procedure Process_Sync_Block (
      Orchestrator : in Out Orchestrator_State;
      Block_Data   : in     Block_Builder.Block;
      Result       : out    Sync_Result
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   --  Check sync progress
   procedure Check_Sync_Progress (
      Orchestrator : in Out Orchestrator_State
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Chain Reorganization
   ---------------------------------------------------------------------------

   type Reorg_Result is (
      Reorg_Applied,
      Reorg_No_Ancestor,
      Reorg_Invalid_Chain,
      Reorg_Execution_Failed,
      Reorg_Not_Better
   );

   --  Handle chain reorganization
   procedure Handle_Reorg (
      Orchestrator  : in Out Orchestrator_State;
      New_Chain_Tip : in     Hash256;
      New_Chain_Num : in     U256;
      Result        : out    Reorg_Result
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   --  Find common ancestor between two chains
   procedure Find_Common_Ancestor (
      Orchestrator : in     Orchestrator_State;
      Chain_A_Hash : in     Hash256;
      Chain_B_Hash : in     Hash256;
      Ancestor     : out    Hash256;
      Found        : out    Boolean
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Transaction Processing
   ---------------------------------------------------------------------------

   --  Submit transaction to mempool
   procedure Submit_Transaction (
      Orchestrator : in Out Orchestrator_State;
      TX_Data      : in     Byte_Array;
      TX_Hash      : out    Hash256;
      Success      : out    Boolean
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   --  Process pending transactions from mempool
   procedure Process_Pending_Transactions (
      Orchestrator : in Out Orchestrator_State;
      Max_TX       : in     Natural;
      Processed    : out    Natural
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Governance Integration
   ---------------------------------------------------------------------------

   --  Update governance phase based on current block
   procedure Update_Governance (
      Orchestrator : in Out Orchestrator_State
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   --  Execute governance action
   procedure Execute_Governance_Action (
      Orchestrator : in Out Orchestrator_State;
      Action_Type  : in     Anubis_Governance.Privilege_Type;
      Success      : out    Boolean
   ) with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Block_Number (Orchestrator : Orchestrator_State) return U256 with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   function Get_Sync_Status (Orchestrator : Orchestrator_State) return Sync_Status with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   function Is_Synced (Orchestrator : Orchestrator_State) return Boolean with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   function Get_Peer_Count (Orchestrator : Orchestrator_State) return Natural with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

   function Get_Mempool_Size (Orchestrator : Orchestrator_State) return Natural with
      Global => null,
      Pre    => Orchestrator.Is_Initialized;

end Anubis_Node_Orchestrator;
