-------------------------------------------------------------------------------
--  P2P_Discovery: Peer Discovery and Bootstrap Mechanism
--
--  This package implements peer discovery for the AnubisVM P2P network using:
--  - Static bootstrap nodes (seed list)
--  - Peer exchange protocol (PEX)
--  - DHT-inspired kademlia-like routing (simplified)
--
--  Discovery Flow:
--  1. Connect to bootstrap nodes from static seed list
--  2. Request peer lists from connected peers (PEX)
--  3. Maintain routing table of known peers by node ID distance
--  4. Periodically refresh peer list to discover new nodes
--
--  Peer Selection Strategy:
--  - Prefer geographically diverse peers
--  - Maintain balance of inbound/outbound connections
--  - Prioritize peers with good reputation
--  - Ensure connectivity to peers with latest chain state
--
--  SPARK Verification Level: Gold
--  ==============================
--  - NRTE proven for all operations
--  - Bounds checking on all arrays and indices
--  - No heap allocation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with P2P_Network; use P2P_Network;

package P2P_Discovery with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum bootstrap nodes
   Max_Bootstrap_Nodes : constant := 16;

   --  Maximum discovered peers
   Max_Discovered_Peers : constant := 256;

   --  Peer refresh interval (seconds)
   Peer_Refresh_Interval : constant := 300;  --  5 minutes

   --  Minimum peers before discovery
   Min_Peers_Threshold : constant := 4;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Bootstrap node entry
   type Bootstrap_Node is record
      Is_Valid : Boolean;
      Address  : Peer_Address;
      Node_ID  : Hash256;
   end record;

   subtype Bootstrap_Index is Natural range 0 .. Max_Bootstrap_Nodes - 1;
   type Bootstrap_List is array (Bootstrap_Index) of Bootstrap_Node;

   --  Discovered peer entry
   type Discovered_Peer is record
      Is_Valid      : Boolean;
      Address       : Peer_Address;
      Node_ID       : Hash256;
      Discovered_At : Unsigned_64;
      Last_Seen     : Unsigned_64;
      Attempts      : Natural;
      Connected     : Boolean;
   end record;

   subtype Discovery_Index is Natural range 0 .. Max_Discovered_Peers - 1;
   type Discovery_Storage is array (Discovery_Index) of Discovered_Peer;

   --  Discovery state
   type Discovery_State is record
      Bootstrap_Nodes    : Bootstrap_List;
      Bootstrap_Count    : Natural;
      Discovered_Peers   : Discovery_Storage;
      Discovered_Count   : Natural;
      Last_Refresh       : Unsigned_64;
      Is_Initialized     : Boolean;
   end record;

   --  Result codes
   type Discovery_Result is (
      Discovery_OK,
      Discovery_Not_Initialized,
      Discovery_Bootstrap_Full,
      Discovery_Peer_Full,
      Discovery_Already_Known,
      Discovery_Invalid_Address,
      Discovery_Not_Found
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize discovery system
   procedure Initialize (
      State : out Discovery_State
   ) with
      Global => null,
      Post   => State.Is_Initialized and
                State.Bootstrap_Count = 0 and
                State.Discovered_Count = 0;

   ---------------------------------------------------------------------------
   --  Bootstrap Management
   ---------------------------------------------------------------------------

   --  Add a bootstrap node
   procedure Add_Bootstrap_Node (
      State   : in out Discovery_State;
      Address : in     Peer_Address;
      Node_ID : in     Hash256;
      Result  : out    Discovery_Result
   ) with
      Global => null,
      Pre    => State.Is_Initialized,
      Post   => (Result = Discovery_OK and then
                   State.Bootstrap_Count = State.Bootstrap_Count'Old + 1 and
                   State.Bootstrap_Count <= Max_Bootstrap_Nodes)
                or (Result /= Discovery_OK and then
                   State.Bootstrap_Count = State.Bootstrap_Count'Old);

   --  Get all bootstrap nodes
   procedure Get_Bootstrap_Nodes (
      State : in     Discovery_State;
      Nodes : out    Bootstrap_List;
      Count : out    Natural
   ) with
      Global => null,
      Pre    => State.Is_Initialized,
      Post   => Count <= Max_Bootstrap_Nodes;

   ---------------------------------------------------------------------------
   --  Peer Discovery
   ---------------------------------------------------------------------------

   --  Add a discovered peer
   procedure Add_Discovered_Peer (
      State       : in out Discovery_State;
      Address     : in     Peer_Address;
      Node_ID     : in     Hash256;
      Current_Time : in     Unsigned_64;
      Result      : out    Discovery_Result
   ) with
      Global => null,
      Pre    => State.Is_Initialized,
      Post   => (Result = Discovery_OK and then
                   State.Discovered_Count <= Max_Discovered_Peers)
                or (Result /= Discovery_OK and then
                   State.Discovered_Count = State.Discovered_Count'Old);

   --  Mark peer as connected
   procedure Mark_Peer_Connected (
      State   : in out Discovery_State;
      Node_ID : in     Hash256;
      Result  : out    Discovery_Result
   ) with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Update peer last seen time
   procedure Update_Peer_Seen (
      State       : in out Discovery_State;
      Node_ID     : in     Hash256;
      Current_Time : in     Unsigned_64;
      Result      : out    Discovery_Result
   ) with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Get next peer to connect
   procedure Get_Next_Peer (
      State       : in     Discovery_State;
      Current_Time : in     Unsigned_64;
      Address     : out    Peer_Address;
      Node_ID     : out    Hash256;
      Found       : out    Boolean
   ) with
      Global => null,
      Pre    => State.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Get number of discovered peers
   function Get_Discovered_Count (State : Discovery_State) return Natural is
      (State.Discovered_Count)
   with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Check if peer is known
   function Is_Peer_Known (
      State   : Discovery_State;
      Node_ID : Hash256
   ) return Boolean with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Check if refresh is needed
   function Need_Refresh (
      State        : Discovery_State;
      Current_Time : Unsigned_64
   ) return Boolean is
      (Current_Time - State.Last_Refresh >= Peer_Refresh_Interval)
   with
      Global => null,
      Pre    => State.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Maintenance
   ---------------------------------------------------------------------------

   --  Remove stale peers
   procedure Remove_Stale_Peers (
      State        : in out Discovery_State;
      Current_Time : in     Unsigned_64;
      Timeout      : in     Unsigned_64;
      Removed      : out    Natural
   ) with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Trigger peer refresh
   procedure Refresh_Peers (
      State        : in out Discovery_State;
      Current_Time : in     Unsigned_64
   ) with
      Global => null,
      Pre    => State.Is_Initialized,
      Post   => State.Last_Refresh = Current_Time;

end P2P_Discovery;
