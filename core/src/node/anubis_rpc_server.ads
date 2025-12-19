-------------------------------------------------------------------------------
--  Anubis RPC Server: Complete JSON-RPC 2.0 + WebSocket Implementation
--
--  This package provides comprehensive RPC functionality:
--
--  1. JSON-RPC 2.0 over TCP (eth_* compatible)
--  2. Anubis-specific methods (anubis_*)
--  3. Debug and admin methods (debug_*, admin_*)
--  4. WebSocket subscriptions for real-time events
--
--  Supported Methods:
--  ------------------
--  Standard Ethereum Compatibility:
--    - eth_blockNumber, eth_getBlockByNumber, eth_getBlockByHash
--    - eth_getBalance, eth_getCode, eth_getStorageAt
--    - eth_getTransactionCount, eth_getTransactionByHash
--    - eth_getTransactionReceipt, eth_sendRawTransaction
--    - eth_call, eth_estimateGas, eth_gasPrice
--    - eth_chainId, net_version, web3_clientVersion
--
--  Anubis Custom Methods:
--    - anubis_getNodeInfo, anubis_getValidatorSet
--    - anubis_getConsensusState, anubis_getPeers
--    - anubis_getMempoolStatus, anubis_getBlockStats
--    - anubis_verifyMLDSA, anubis_verifyMLKEM
--
--  Debug Methods (non-production):
--    - debug_traceTransaction, debug_traceBlock
--    - debug_getState, debug_dumpMempool
--
--  Admin Methods (restricted):
--    - admin_addPeer, admin_removePeer
--    - admin_nodeInfo, admin_datadir
--
--  WebSocket Subscriptions:
--    - newHeads (new block headers)
--    - newPendingTransactions (mempool updates)
--    - logs (contract event logs)
--    - syncing (sync status changes)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Node_Types; use Anubis_Node_Types;
with Anubis_Node;
with Mempool;
with Block_Builder;
with Consensus;

package Anubis_RPC_Server with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  RPC Configuration
   ---------------------------------------------------------------------------

   type RPC_Config is record
      Port                : Port_Number;
      Enable_WebSocket    : Boolean;
      Enable_Debug_API    : Boolean;
      Enable_Admin_API    : Boolean;
      Request_Timeout_MS  : Natural;
      Max_Connections     : Natural;
   end record;

   Default_RPC_Config : constant RPC_Config := (
      Port               => 26659,
      Enable_WebSocket   => True,
      Enable_Debug_API   => False,  -- Only in testnet
      Enable_Admin_API   => False,  -- Only with authentication
      Request_Timeout_MS => 30_000,
      Max_Connections    => 100
   );

   ---------------------------------------------------------------------------
   --  WebSocket Subscription Types
   ---------------------------------------------------------------------------

   type Subscription_Type is (
      Sub_NewHeads,                 -- New block headers
      Sub_NewPendingTransactions,   -- Mempool transactions
      Sub_Logs,                     -- Contract event logs
      Sub_Syncing                   -- Sync status
   );

   type Subscription_ID is new Natural range 0 .. 65535;

   type Subscription_Filter is record
      Filter_Type : Subscription_Type;
      Addresses   : array (0 .. 15) of Contract_Address;  -- Log filters
      Address_Count : Natural;
      Topics      : array (0 .. 3) of Hash256;             -- Log topics
      Topic_Count : Natural;
   end record;

   type Subscription_Entry is record
      Is_Active   : Boolean;
      Sub_ID      : Subscription_ID;
      Filter      : Subscription_Filter;
      Client_Addr : Contract_Address;  -- Identifies client
   end record;

   Max_Subscriptions : constant := 1024;
   subtype Sub_Index is Natural range 0 .. Max_Subscriptions - 1;
   type Subscription_Array is array (Sub_Index) of Subscription_Entry;

   ---------------------------------------------------------------------------
   --  RPC Server State
   ---------------------------------------------------------------------------

   type RPC_Server_State is record
      Is_Initialized : Boolean;
      Config         : RPC_Config;

      --  Node state references
      VM_Inst        : Anubis_Node.VM_Instance;
      Mempool_Inst   : Mempool.Mempool_State;
      Builder_Inst   : Block_Builder.Builder_State;
      Consensus_Inst : Consensus.Consensus_State;

      --  WebSocket subscriptions
      Subscriptions  : Subscription_Array;
      Sub_Count      : Natural;
      Next_Sub_ID    : Subscription_ID;

      --  Statistics
      Requests_Served : Unsigned_64;
      Errors_Occurred : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Server   : out RPC_Server_State;
      Config   : in  RPC_Config;
      VM       : in  Anubis_Node.VM_Instance;
      Pool     : in  Mempool.Mempool_State;
      Builder  : in  Block_Builder.Builder_State;
      Cons     : in  Consensus.Consensus_State
   ) with
      Global => null,
      Post   => Server.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Standard Ethereum RPC Methods
   ---------------------------------------------------------------------------

   --  eth_blockNumber - Returns current block number
   procedure ETH_BlockNumber (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  eth_getBalance - Get account balance
   procedure ETH_GetBalance (
      Server  : in     RPC_Server_State;
      Address : in     Contract_Address;
      Block_Num : in   U256;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  eth_getBlockByNumber - Get block by number
   procedure ETH_GetBlockByNumber (
      Server      : in     RPC_Server_State;
      Block_Num   : in     U256;
      Full_TX     : in     Boolean;
      Result      : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  eth_getCode - Get contract bytecode
   procedure ETH_GetCode (
      Server    : in     RPC_Server_State;
      Address   : in     Contract_Address;
      Block_Num : in     U256;
      Result    : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  eth_getStorageAt - Get contract storage slot
   procedure ETH_GetStorageAt (
      Server    : in     RPC_Server_State;
      Address   : in     Contract_Address;
      Slot      : in     Storage_Key;
      Block_Num : in     U256;
      Result    : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  eth_call - Execute read-only call
   procedure ETH_Call (
      Server    : in     RPC_Server_State;
      From      : in     Contract_Address;
      To        : in     Contract_Address;
      Data      : in     Params_Buffer;
      Data_Size : in     Natural;
      Gas_Limit : in     Gas_Amount;
      Result    : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Data_Size <= Max_Params_Size;

   --  eth_estimateGas - Estimate gas for transaction
   procedure ETH_EstimateGas (
      Server    : in     RPC_Server_State;
      From      : in     Contract_Address;
      To        : in     Contract_Address;
      Data      : in     Params_Buffer;
      Data_Size : in     Natural;
      Result    : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Data_Size <= Max_Params_Size;

   --  eth_gasPrice - Get current gas price
   procedure ETH_GasPrice (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  eth_chainId - Get chain ID
   procedure ETH_ChainId (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Anubis Custom Methods
   ---------------------------------------------------------------------------

   --  anubis_getNodeInfo - Get comprehensive node information
   procedure Anubis_GetNodeInfo (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  anubis_getValidatorSet - Get current validator set
   procedure Anubis_GetValidatorSet (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  anubis_getConsensusState - Get consensus state
   procedure Anubis_GetConsensusState (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  anubis_getMempoolStatus - Get mempool status
   procedure Anubis_GetMempoolStatus (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   --  anubis_verifyMLDSA - Verify ML-DSA-87 signature
   procedure Anubis_VerifyMLDSA (
      Server    : in     RPC_Server_State;
      Message   : in     Hash256;
      Signature : in     Byte_Array;
      PublicKey : in     Byte_Array;
      Result    : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Debug Methods (non-production)
   ---------------------------------------------------------------------------

   --  debug_traceTransaction - Trace transaction execution
   procedure Debug_TraceTransaction (
      Server  : in     RPC_Server_State;
      TX_Hash : in     Hash256;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Server.Config.Enable_Debug_API;

   --  debug_dumpMempool - Dump mempool contents
   procedure Debug_DumpMempool (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Server.Config.Enable_Debug_API;

   ---------------------------------------------------------------------------
   --  Admin Methods (restricted)
   ---------------------------------------------------------------------------

   --  admin_addPeer - Add peer to network
   procedure Admin_AddPeer (
      Server   : in     RPC_Server_State;
      Peer_URL : in     String;
      Result   : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Server.Config.Enable_Admin_API;

   --  admin_nodeInfo - Get detailed node info
   procedure Admin_NodeInfo (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Server.Config.Enable_Admin_API;

   ---------------------------------------------------------------------------
   --  WebSocket Subscriptions
   ---------------------------------------------------------------------------

   --  Subscribe to event stream
   procedure WS_Subscribe (
      Server   : in Out RPC_Server_State;
      Filter   : in     Subscription_Filter;
      Client   : in     Contract_Address;
      Sub_ID   : out    Subscription_ID;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Server.Config.Enable_WebSocket;

   --  Unsubscribe from event stream
   procedure WS_Unsubscribe (
      Server   : in Out RPC_Server_State;
      Sub_ID   : in     Subscription_ID;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre    => Server.Is_Initialized and Server.Config.Enable_WebSocket;

   --  Check if any subscriptions match an event
   function Has_Matching_Subscription (
      Server  : RPC_Server_State;
      Filter  : Subscription_Filter
   ) return Boolean with
      Global => null,
      Pre    => Server.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Request Dispatcher
   ---------------------------------------------------------------------------

   --  Dispatch RPC request to appropriate handler
   procedure Process_RPC_Request (
      Server   : in Out RPC_Server_State;
      Request  : in     RPC_Request;
      Response : out    RPC_Response
   ) with
      Global => null,
      Pre    => Server.Is_Initialized;

end Anubis_RPC_Server;
