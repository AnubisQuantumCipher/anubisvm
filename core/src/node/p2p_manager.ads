-------------------------------------------------------------------------------
--  P2P_Manager: Comprehensive P2P Network Management
--
--  This package provides the main interface for P2P networking in AnubisVM.
--  It coordinates:
--  - Peer discovery and connection management
--  - Secure ML-KEM-1024 encrypted channels
--  - Transaction and block gossip
--  - Peer reputation scoring
--  - Concurrent peer handling
--  - Graceful shutdown and reconnection
--
--  Architecture:
--  +------------------+
--  |   Anubis Node    |
--  +------------------+
--           |
--           v
--  +------------------+
--  |   P2P Manager    |  <--- This package
--  +------------------+
--     |    |     |
--     v    v     v
--  +------+------+------+
--  | Disc | Net  | Sec  |  Discovery, Network, Secure Channel
--  +------+------+------+
--
--  Usage:
--  1. Initialize with node identity and configuration
--  2. Add bootstrap nodes
--  3. Start network (begins accepting connections)
--  4. Call Process_Events regularly in main loop
--  5. Use Gossip_TX/Gossip_Block to propagate data
--  6. Call Shutdown when stopping node
--
--  SPARK Verification Level: Gold
--  ==============================
--  - NRTE proven for all operations
--  - Bounds checking on all arrays
--  - No heap allocation
--  - Resource cleanup proven
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_MLDSA_Types;
with P2P_Network; use P2P_Network;
with P2P_Discovery; use P2P_Discovery;

package P2P_Manager with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  P2P Manager state
   type Manager_State is record
      Net            : Network_State;
      Discovery      : Discovery_State;
      Node_SK        : Anubis_MLDSA_Types.ML_DSA_Secret_Key;
      Is_Initialized : Boolean;
      Is_Running     : Boolean;
   end record;

   --  Result codes
   type Manager_Result is (
      Manager_OK,
      Manager_Not_Initialized,
      Manager_Already_Running,
      Manager_Network_Error,
      Manager_Discovery_Error,
      Manager_Peer_Error
   );

   ---------------------------------------------------------------------------
   --  Initialization and Lifecycle
   ---------------------------------------------------------------------------

   --  Initialize P2P manager
   procedure Initialize (
      Mgr       : out Manager_State;
      Node_ID   : in  Hash256;
      Node_Key  : in  Anubis_MLDSA_Types.Public_Key;
      Node_SK   : in  Anubis_MLDSA_Types.ML_DSA_Secret_Key;
      Chain_ID  : in  U256;
      Port      : in  Unsigned_16
   ) with
      Global => null,
      Post   => Mgr.Is_Initialized and not Mgr.Is_Running;

   --  Start P2P networking
   procedure Start (
      Mgr    : in out Manager_State;
      Result : out    Manager_Result
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and not Mgr.Is_Running,
      Post   => (Result = Manager_OK and then Mgr.Is_Running)
                or (Result /= Manager_OK and then not Mgr.Is_Running);

   --  Stop P2P networking
   procedure Stop (
      Mgr : in out Manager_State
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized,
      Post   => not Mgr.Is_Running;

   --  Shutdown P2P manager
   procedure Shutdown (
      Mgr : in out Manager_State
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized,
      Post   => not Mgr.Is_Initialized and not Mgr.Is_Running;

   ---------------------------------------------------------------------------
   --  Bootstrap Configuration
   ---------------------------------------------------------------------------

   --  Add a bootstrap node
   procedure Add_Bootstrap (
      Mgr        : in out Manager_State;
      Address_Str : in     String;
      Port       : in     Unsigned_16;
      Result     : out    Manager_Result
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and
                Address_Str'Length > 0 and
                Address_Str'Length <= Max_Address_Len;

   ---------------------------------------------------------------------------
   --  Connection Management
   ---------------------------------------------------------------------------

   --  Connect to a peer
   procedure Connect_To_Peer (
      Mgr        : in out Manager_State;
      Address_Str : in     String;
      Port       : in     Unsigned_16;
      Result     : out    Manager_Result
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and Mgr.Is_Running and
                Address_Str'Length > 0 and
                Address_Str'Length <= Max_Address_Len;

   --  Disconnect from a peer
   procedure Disconnect_From_Peer (
      Mgr     : in out Manager_State;
      Node_ID : in     Hash256;
      Result  : out    Manager_Result
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Data Propagation
   ---------------------------------------------------------------------------

   --  Gossip a transaction to peers
   procedure Gossip_Transaction (
      Mgr     : in out Manager_State;
      TX_Hash : in     Hash256;
      Sent    : out    Natural
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and Mgr.Is_Running;

   --  Gossip a block to peers
   procedure Gossip_Block (
      Mgr        : in out Manager_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      Sent       : out    Natural
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and Mgr.Is_Running;

   --  Request a transaction from peers
   procedure Request_Transaction (
      Mgr     : in out Manager_State;
      TX_Hash : in     Hash256;
      Result  : out    Manager_Result
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and Mgr.Is_Running;

   --  Request a block from peers
   procedure Request_Block (
      Mgr        : in out Manager_State;
      Block_Hash : in     Hash256;
      Result     : out    Manager_Result
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and Mgr.Is_Running;

   ---------------------------------------------------------------------------
   --  Event Processing
   ---------------------------------------------------------------------------

   --  Process pending network events (call regularly in main loop)
   procedure Process_Events (
      Mgr          : in out Manager_State;
      Current_Time : in     Unsigned_64
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and Mgr.Is_Running;

   --  Perform periodic maintenance (keepalives, peer cleanup, discovery)
   procedure Perform_Maintenance (
      Mgr          : in out Manager_State;
      Current_Time : in     Unsigned_64
   ) with
      Global => null,
      Pre    => Mgr.Is_Initialized and Mgr.Is_Running;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Get number of connected peers
   function Get_Peer_Count (Mgr : Manager_State) return Natural with
      Global => null,
      Pre    => Mgr.Is_Initialized;

   --  Get number of discovered peers
   function Get_Discovered_Count (Mgr : Manager_State) return Natural with
      Global => null,
      Pre    => Mgr.Is_Initialized;

   --  Check if peer is connected
   function Is_Peer_Connected (
      Mgr     : Manager_State;
      Node_ID : Hash256
   ) return Boolean with
      Global => null,
      Pre    => Mgr.Is_Initialized;

   --  Get network statistics
   function Get_Network_Stats (Mgr : Manager_State) return Network_Stats with
      Global => null,
      Pre    => Mgr.Is_Initialized;

   --  Check if manager is running
   function Is_Running (Mgr : Manager_State) return Boolean is
      (Mgr.Is_Running)
   with
      Global => null;

end P2P_Manager;
