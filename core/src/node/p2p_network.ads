-------------------------------------------------------------------------------
--  P2P_Network: Peer-to-Peer Networking for AnubisVM
--
--  This package implements the P2P networking layer for node communication.
--  Key features:
--
--  - Peer discovery and management
--  - Transaction and block gossiping
--  - Message serialization and deserialization
--  - Connection state tracking
--  - ML-DSA-87 authenticated peer handshakes
--
--  Network Architecture:
--  +-------------+     +---------------+     +---------------+
--  | Local Node  | <-> | Peer Manager  | <-> | Remote Peers  |
--  | (mempool)   |     | (connections) |     | (gossip)      |
--  +-------------+     +---------------+     +---------------+
--
--  Message Types:
--  - HANDSHAKE: Initial peer connection with chain/version info
--  - PING/PONG: Keepalive and latency measurement
--  - TX_ANNOUNCE: Announce new transactions (hash only)
--  - TX_REQUEST: Request full transaction by hash
--  - TX_RESPONSE: Full transaction data
--  - BLOCK_ANNOUNCE: Announce new block (hash + height)
--  - BLOCK_REQUEST: Request block by hash or height
--  - BLOCK_RESPONSE: Full block data
--  - PEER_REQUEST: Request peer list
--  - PEER_RESPONSE: Share known peer addresses
--
--  Security:
--  - All messages authenticated with ML-DSA-87
--  - Peer reputation tracking
--  - Rate limiting per peer
--  - Ban list for malicious peers
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_MLDSA_Types;
with P2P_Secure_Channel;

package P2P_Network with
   SPARK_Mode => On
is

   --  Forward declaration for socket handle
   --  Note: Actual sockets are managed in P2P_Sockets package (SPARK_Mode Off)

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum number of connected peers
   Max_Peers : constant := 64;

   --  Maximum pending connections
   Max_Pending_Connections : constant := 32;

   --  Peer discovery limit per request
   Max_Peers_Per_Discovery : constant := 16;

   --  Message size limits
   Max_Message_Size : constant := 1024 * 1024;  -- 1 MB max message
   Max_TX_Per_Message : constant := 64;
   Max_Blocks_Per_Message : constant := 8;

   --  Timeouts (in seconds)
   Handshake_Timeout : constant := 30;
   Ping_Interval : constant := 60;
   Peer_Timeout : constant := 180;

   --  Protocol version
   Protocol_Version : constant := 1;

   --  Network magic (identifies AnubisVM network)
   Network_Magic : constant Unsigned_32 := 16#A0B1C501#;

   --  Default P2P port
   Default_P2P_Port : constant := 26656;

   ---------------------------------------------------------------------------
   --  Peer Address Type
   ---------------------------------------------------------------------------

   --  Maximum address string length
   Max_Address_Len : constant := 256;

   --  Peer address (IP or hostname + port)
   type Peer_Address is record
      Is_Valid   : Boolean;
      Address    : String (1 .. Max_Address_Len);
      Addr_Len   : Natural;
      Port       : Unsigned_16;
   end record;

   ---------------------------------------------------------------------------
   --  Message Types
   ---------------------------------------------------------------------------

   --  P2P message type
   type Message_Type is (
      Msg_Handshake,
      Msg_Handshake_Ack,
      Msg_Ping,
      Msg_Pong,
      Msg_TX_Announce,
      Msg_TX_Request,
      Msg_TX_Response,
      Msg_Block_Announce,
      Msg_Block_Request,
      Msg_Block_Response,
      Msg_Peer_Request,
      Msg_Peer_Response,
      Msg_Disconnect
   );

   --  Message header
   type Message_Header is record
      Magic        : Unsigned_32;
      Version      : Unsigned_16;
      Msg_Type     : Message_Type;
      Payload_Len  : Unsigned_32;
      Timestamp    : Unsigned_64;
      Sender_ID    : Hash256;  -- Node ID (pubkey hash)
   end record;

   --  Maximum payload size (for static arrays)
   Max_Payload_Len : constant := 65536;  -- 64 KB payload limit
   subtype Payload_Index is Natural range 0 .. Max_Payload_Len - 1;
   type Payload_Buffer is array (Payload_Index) of Aegis_VM_Types.Byte;

   --  Message structure
   type P2P_Message is record
      Header       : Message_Header;
      Payload      : Payload_Buffer;
      Payload_Size : Natural;
      Is_Valid     : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Peer State Types
   ---------------------------------------------------------------------------

   --  Peer connection state
   type Peer_State is (
      Peer_Disconnected,
      Peer_Connecting,
      Peer_Handshaking,
      Peer_Connected,
      Peer_Syncing,
      Peer_Banned
   );

   --  Peer information
   type Peer_Info is record
      Is_Valid        : Boolean;
      Node_ID         : Hash256;
      Address         : Peer_Address;
      State           : Peer_State;
      Connected_Time  : Unsigned_64;
      Last_Seen       : Unsigned_64;
      Last_Ping       : Unsigned_64;
      Ping_Latency_Ms : Natural;
      Chain_Height    : U256;
      Best_Block      : Hash256;
      TX_Received     : Natural;
      TX_Sent         : Natural;
      Blocks_Received : Natural;
      Blocks_Sent     : Natural;
      Reputation      : Integer;  -- -100 to 100
      Is_Inbound      : Boolean;
      Public_Key      : Anubis_MLDSA_Types.Public_Key;
      Socket_FD       : Integer;  -- Socket file descriptor (-1 if not connected)
      Secure_Channel  : P2P_Secure_Channel.Secure_Channel;  -- Post-quantum encrypted channel
   end record;

   --  Peer storage
   subtype Peer_Index is Natural range 0 .. Max_Peers - 1;
   type Peer_Storage is array (Peer_Index) of Peer_Info;

   ---------------------------------------------------------------------------
   --  Ban List Types
   ---------------------------------------------------------------------------

   --  Maximum banned peers
   Max_Banned_Peers : constant := 256;
   subtype Ban_Index is Natural range 0 .. Max_Banned_Peers - 1;

   type Ban_Entry is record
      Is_Valid    : Boolean;
      Node_ID     : Hash256;
      Ban_Time    : Unsigned_64;
      Ban_Until   : Unsigned_64;
      Reason      : Natural;  -- Ban reason code
   end record;

   type Ban_Storage is array (Ban_Index) of Ban_Entry;

   ---------------------------------------------------------------------------
   --  Network Statistics
   ---------------------------------------------------------------------------

   type Network_Stats is record
      Peers_Connected      : Natural;
      Peers_Connecting     : Natural;
      Peers_Banned         : Natural;
      Messages_Sent        : Natural;
      Messages_Received    : Natural;
      TX_Gossiped          : Natural;
      Blocks_Gossiped      : Natural;
      Bytes_Sent           : Unsigned_64;
      Bytes_Received       : Unsigned_64;
      Connections_Received : Natural;
      Last_Update_Time     : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Network State
   ---------------------------------------------------------------------------

   --  Result codes
   type Network_Result is (
      Network_OK,
      Network_Peer_Full,
      Network_Peer_Banned,
      Network_Invalid_Message,
      Network_Timeout,
      Network_Connection_Failed,
      Network_Already_Connected,
      Network_Not_Found,
      Network_Not_Connected,
      Network_Protocol_Error,
      Network_Not_Initialized
   );

   --  Network manager state
   type Network_State is record
      Peers           : Peer_Storage;
      Peer_Count      : Natural;
      Ban_List        : Ban_Storage;
      Ban_Count       : Natural;
      Node_ID         : Hash256;
      Node_Key        : Anubis_MLDSA_Types.Public_Key;
      Listen_Port     : Unsigned_16;
      Stats           : Network_Stats;
      Chain_ID        : U256;
      Current_Height  : U256;
      Best_Block      : Hash256;
      Is_Initialized  : Boolean;
      Is_Running      : Boolean;
      Listener_FD     : Integer;  -- Listening socket FD (-1 if not listening)
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize network layer
   procedure Initialize (
      Net       : out Network_State;
      Node_ID   : in  Hash256;
      Node_Key  : in  Anubis_MLDSA_Types.Public_Key;
      Chain_ID  : in  U256;
      Port      : in  Unsigned_16
   ) with
      Global => null,
      Post   => Net.Is_Initialized and not Net.Is_Running;

   --  Start network (begin accepting connections)
   procedure Start (
      Net    : in Out Network_State;
      Result : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Stop network (close all connections)
   procedure Stop (
      Net : in Out Network_State
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Peer Management
   ---------------------------------------------------------------------------

   --  Connect to a peer
   procedure Connect_Peer (
      Net     : in Out Network_State;
      Address : in     Peer_Address;
      Result  : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Disconnect from a peer
   procedure Disconnect_Peer (
      Net     : in Out Network_State;
      Node_ID : in     Hash256;
      Result  : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Ban a peer
   procedure Ban_Peer (
      Net      : in Out Network_State;
      Node_ID  : in     Hash256;
      Duration : in     Unsigned_64;  -- Ban duration in seconds
      Reason   : in     Natural;
      Result   : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Unban a peer
   procedure Unban_Peer (
      Net     : in Out Network_State;
      Node_ID : in     Hash256;
      Result  : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Get peer info
   procedure Get_Peer (
      Net     : in     Network_State;
      Node_ID : in     Hash256;
      Info    : out    Peer_Info;
      Found   : out    Boolean
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Update peer chain info
   procedure Update_Peer_Chain (
      Net      : in Out Network_State;
      Node_ID  : in     Hash256;
      Height   : in     U256;
      Best     : in     Hash256;
      Result   : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Message Operations
   ---------------------------------------------------------------------------

   --  Send message to specific peer
   procedure Send_To_Peer (
      Net     : in Out Network_State;
      Node_ID : in     Hash256;
      Msg     : in     P2P_Message;
      Result  : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Broadcast message to all connected peers
   procedure Broadcast (
      Net     : in Out Network_State;
      Msg     : in     P2P_Message;
      Sent    : out    Natural
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Broadcast to peers except specified node
   procedure Broadcast_Except (
      Net       : in Out Network_State;
      Msg       : in     P2P_Message;
      Except_ID : in     Hash256;
      Sent      : out    Natural
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   ---------------------------------------------------------------------------
   --  Gossip Operations
   ---------------------------------------------------------------------------

   --  Announce a new transaction to peers
   procedure Gossip_TX (
      Net     : in Out Network_State;
      TX_Hash : in     Hash256;
      Sent    : out    Natural
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Announce a new block to peers
   procedure Gossip_Block (
      Net        : in Out Network_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      Sent       : out    Natural
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Request a transaction from peers
   procedure Request_TX (
      Net     : in Out Network_State;
      TX_Hash : in     Hash256;
      Result  : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Request a block from peers
   procedure Request_Block (
      Net        : in Out Network_State;
      Block_Hash : in     Hash256;
      Result     : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Request block by height
   procedure Request_Block_By_Height (
      Net    : in Out Network_State;
      Height : in     U256;
      Result : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   ---------------------------------------------------------------------------
   --  Message Construction
   ---------------------------------------------------------------------------

   --  Create handshake message
   procedure Create_Handshake (
      Net : in     Network_State;
      Msg : out    P2P_Message
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Create ping message
   procedure Create_Ping (
      Net : in     Network_State;
      Msg : out    P2P_Message
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Create pong message (response to ping)
   procedure Create_Pong (
      Net      : in     Network_State;
      Ping_Msg : in     P2P_Message;
      Msg      : out    P2P_Message
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Create TX announce message
   procedure Create_TX_Announce (
      Net      : in     Network_State;
      TX_Hash  : in     Hash256;
      Msg      : out    P2P_Message
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Create block announce message
   procedure Create_Block_Announce (
      Net        : in     Network_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      Msg        : out    P2P_Message
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Get number of connected peers
   function Get_Peer_Count (Net : Network_State) return Natural with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Check if peer is connected
   function Is_Peer_Connected (
      Net     : Network_State;
      Node_ID : Hash256
   ) return Boolean with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Check if peer is banned
   function Is_Peer_Banned (
      Net     : Network_State;
      Node_ID : Hash256
   ) return Boolean with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Get network statistics
   function Get_Stats (Net : Network_State) return Network_Stats with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Get best known chain height from peers
   function Get_Best_Peer_Height (Net : Network_State) return U256 with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Check if network is running
   function Is_Running (Net : Network_State) return Boolean with
      Global => null,
      Pre    => Net.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Peer Discovery
   ---------------------------------------------------------------------------

   --  Request peer list from connected peers
   procedure Request_Peers (
      Net    : in Out Network_State;
      Result : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Add discovered peer address
   procedure Add_Discovered_Peer (
      Net     : in Out Network_State;
      Address : in     Peer_Address;
      Result  : out    Network_Result
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Maintenance Operations
   ---------------------------------------------------------------------------

   --  Process pending network events
   procedure Process_Events (
      Net : in Out Network_State
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Send keepalive pings to peers
   procedure Send_Keepalives (
      Net          : in Out Network_State;
      Current_Time : in     Unsigned_64
   ) with
      Global => null,
      Pre    => Net.Is_Initialized and Net.Is_Running;

   --  Expire stale peers
   procedure Expire_Stale_Peers (
      Net          : in Out Network_State;
      Current_Time : in     Unsigned_64;
      Expired      : out    Natural
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

   --  Clean up expired bans
   procedure Cleanup_Bans (
      Net          : in Out Network_State;
      Current_Time : in     Unsigned_64;
      Removed      : out    Natural
   ) with
      Global => null,
      Pre    => Net.Is_Initialized;

end P2P_Network;
