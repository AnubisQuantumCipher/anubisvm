-------------------------------------------------------------------------------
--  Block_Sync: Block Synchronization for AnubisVM
--
--  This package implements block chain synchronization between nodes.
--  Key features:
--
--  - Initial sync: Download blocks from genesis to tip
--  - Live sync: Stay in sync with network as new blocks arrive
--  - Fork detection and resolution
--  - Block validation before acceptance
--  - Parallel block downloading
--  - Progress tracking and reporting
--
--  Sync Modes:
--  - INITIAL: Downloading historical blocks from scratch
--  - CATCHING_UP: Behind the network, fast-syncing
--  - SYNCED: At tip, receiving new blocks in real-time
--  - REORG: Detected fork, reorganizing chain
--
--  Sync Flow:
--  +-------------+     +---------------+     +---------------+     +--------+
--  | Peer Height | --> | Request Blocks| --> | Validate      | --> | Apply  |
--  | Discovery   |     | (parallel)    |     | (sig + state) |     | Block  |
--  +-------------+     +---------------+     +---------------+     +--------+
--
--  Security:
--  - All block headers validated before body download
--  - Block signatures (ML-DSA-87) verified
--  - State roots verified after transaction replay
--  - Checkpoints for fast sync (trusted block hashes)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Block_Builder;
with P2P_Network;

package Block_Sync with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum blocks to request in parallel
   Max_Parallel_Requests : constant := 16;

   --  Maximum blocks to buffer before applying
   Max_Block_Buffer : constant := 256;

   --  Maximum reorg depth (blocks we'll roll back)
   Max_Reorg_Depth : constant := 64;

   --  Sync timeout (seconds)
   Sync_Timeout : constant := 30;

   --  Block request batch size
   Block_Batch_Size : constant := 16;

   --  Minimum peers needed for sync
   Min_Sync_Peers : constant := 1;

   ---------------------------------------------------------------------------
   --  Sync State Types
   ---------------------------------------------------------------------------

   --  Sync mode
   type Sync_Mode is (
      Sync_Initial,      -- First sync from genesis
      Sync_Catching_Up,  -- Behind network, fast syncing
      Sync_Synced,       -- At tip, live sync
      Sync_Reorg         -- Reorganizing chain after fork
   );

   --  Sync status
   type Sync_Status is (
      Status_Idle,
      Status_Discovering,    -- Finding best peer height
      Status_Downloading,    -- Downloading blocks
      Status_Validating,     -- Validating downloaded blocks
      Status_Applying,       -- Applying blocks to state
      Status_Synced,         -- Fully synchronized
      Status_Error           -- Sync error occurred
   );

   --  Result codes
   type Sync_Result is (
      Sync_OK,
      Sync_No_Peers,
      Sync_Invalid_Block,
      Sync_Invalid_Parent,
      Sync_Invalid_State_Root,
      Sync_Timed_Out,
      Sync_Fork_Detected,
      Sync_Reorg_Too_Deep,
      Sync_Already_Syncing,
      Sync_Not_Initialized
   );

   ---------------------------------------------------------------------------
   --  Block Request Types
   ---------------------------------------------------------------------------

   --  Block request status
   type Request_Status is (
      Request_Pending,
      Request_Sent,
      Request_Received,
      Request_Failed,
      Request_Timeout
   );

   --  Block request tracking
   type Block_Request is record
      Is_Valid      : Boolean;
      Height        : U256;
      Block_Hash    : Hash256;  -- Known hash (for validation)
      Peer_ID       : Hash256;
      Status        : Request_Status;
      Request_Time  : Unsigned_64;
      Attempts      : Natural;
   end record;

   --  Request storage
   subtype Request_Index is Natural range 0 .. Max_Parallel_Requests - 1;
   type Request_Storage is array (Request_Index) of Block_Request;

   ---------------------------------------------------------------------------
   --  Block Buffer Types
   ---------------------------------------------------------------------------

   --  Buffered block entry
   type Buffered_Block is record
      Is_Valid      : Boolean;
      Block         : Block_Builder.Block;
      Height        : U256;
      From_Peer     : Hash256;
      Received_Time : Unsigned_64;
   end record;

   --  Block buffer storage
   subtype Buffer_Index is Natural range 0 .. Max_Block_Buffer - 1;
   type Block_Buffer_Storage is array (Buffer_Index) of Buffered_Block;

   ---------------------------------------------------------------------------
   --  Sync Statistics
   ---------------------------------------------------------------------------

   type Sync_Stats is record
      Blocks_Downloaded   : Natural;
      Blocks_Applied      : Natural;
      Blocks_Rejected     : Natural;
      Reorgs_Performed    : Natural;
      Bytes_Downloaded    : Unsigned_64;
      Sync_Start_Time     : Unsigned_64;
      Last_Block_Time     : Unsigned_64;
      Current_Height      : U256;
      Target_Height       : U256;
   end record;

   ---------------------------------------------------------------------------
   --  Checkpoint Types
   ---------------------------------------------------------------------------

   --  Maximum checkpoints
   Max_Checkpoints : constant := 64;
   subtype Checkpoint_Index is Natural range 0 .. Max_Checkpoints - 1;

   --  Trusted checkpoint (for fast sync)
   type Checkpoint is record
      Is_Valid    : Boolean;
      Height      : U256;
      Block_Hash  : Hash256;
   end record;

   type Checkpoint_Storage is array (Checkpoint_Index) of Checkpoint;

   ---------------------------------------------------------------------------
   --  Sync State
   ---------------------------------------------------------------------------

   type Sync_State is record
      --  Current sync status
      Mode          : Sync_Mode;
      Status        : Sync_Status;

      --  Chain state
      Local_Height  : U256;
      Local_Best    : Hash256;
      Target_Height : U256;
      Target_Best   : Hash256;

      --  Block requests
      Requests      : Request_Storage;
      Request_Count : Natural;

      --  Block buffer (pending application)
      Buffer        : Block_Buffer_Storage;
      Buffer_Count  : Natural;

      --  Checkpoints
      Checkpoints   : Checkpoint_Storage;
      Checkpoint_Count : Natural;

      --  Statistics
      Stats         : Sync_Stats;

      --  State flags
      Is_Initialized : Boolean;
      Is_Syncing     : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize sync module
   procedure Initialize (
      Sync         : out Sync_State;
      Local_Height : in  U256;
      Local_Best   : in  Hash256
   ) with
      Global => null,
      Post   => Sync.Is_Initialized and not Sync.Is_Syncing;

   --  Add a trusted checkpoint
   procedure Add_Checkpoint (
      Sync       : in Out Sync_State;
      Height     : in     U256;
      Block_Hash : in     Hash256;
      Result     : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Sync Control
   ---------------------------------------------------------------------------

   --  Start synchronization
   procedure Start_Sync (
      Sync   : in Out Sync_State;
      Net    : in Out P2P_Network.Network_State;
      Result : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized and Net.Is_Initialized;

   --  Stop synchronization
   procedure Stop_Sync (
      Sync : in Out Sync_State
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Process sync events (call periodically)
   procedure Process_Sync (
      Sync         : in Out Sync_State;
      Net          : in Out P2P_Network.Network_State;
      Current_Time : in     Unsigned_64;
      Result       : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized and Net.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Block Reception
   ---------------------------------------------------------------------------

   --  Handle received block from peer
   procedure Receive_Block (
      Sync         : in Out Sync_State;
      Block        : in     Block_Builder.Block;
      From_Peer    : in     Hash256;
      Current_Time : in     Unsigned_64;
      Result       : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Handle block announcement from peer
   procedure Handle_Block_Announce (
      Sync       : in Out Sync_State;
      Net        : in Out P2P_Network.Network_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      From_Peer  : in     Hash256;
      Result     : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized and Net.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Block Validation
   ---------------------------------------------------------------------------

   --  Validate block header
   procedure Validate_Header (
      Sync   : in     Sync_State;
      Block  : in     Block_Builder.Block;
      Result : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Validate full block (header + transactions + state)
   procedure Validate_Block (
      Sync   : in     Sync_State;
      Block  : in     Block_Builder.Block;
      Result : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Check if block is at checkpoint
   function Is_Checkpoint (
      Sync       : Sync_State;
      Height     : U256;
      Block_Hash : Hash256
   ) return Boolean with
      Global => null,
      Pre    => Sync.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Chain Management
   ---------------------------------------------------------------------------

   --  Apply validated block to chain
   procedure Apply_Block (
      Sync   : in Out Sync_State;
      Block  : in     Block_Builder.Block;
      Result : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Detect if we need to reorg
   procedure Detect_Reorg (
      Sync        : in     Sync_State;
      New_Block   : in     Block_Builder.Block;
      Need_Reorg  : out    Boolean;
      Reorg_Depth : out    Natural
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Perform chain reorganization
   procedure Perform_Reorg (
      Sync        : in Out Sync_State;
      Fork_Point  : in     U256;
      New_Blocks  : in     Block_Buffer_Storage;
      Block_Count : in     Natural;
      Result      : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Request Management
   ---------------------------------------------------------------------------

   --  Request blocks from peers
   procedure Request_Blocks (
      Sync       : in Out Sync_State;
      Net        : in Out P2P_Network.Network_State;
      Start_Height : in   U256;
      Count      : in     Natural;
      Result     : out    Sync_Result
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized and Net.Is_Initialized;

   --  Handle request timeout
   procedure Handle_Timeout (
      Sync         : in Out Sync_State;
      Net          : in Out P2P_Network.Network_State;
      Current_Time : in     Unsigned_64
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized and Net.Is_Initialized;

   --  Cancel pending request
   procedure Cancel_Request (
      Sync   : in Out Sync_State;
      Height : in     U256
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Get current sync mode
   function Get_Mode (Sync : Sync_State) return Sync_Mode with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Get current sync status
   function Get_Status (Sync : Sync_State) return Sync_Status with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Get sync statistics
   function Get_Stats (Sync : Sync_State) return Sync_Stats with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Get sync progress (0-100)
   function Get_Progress (Sync : Sync_State) return Natural with
      Global => null,
      Pre    => Sync.Is_Initialized,
      Post   => Get_Progress'Result <= 100;

   --  Check if fully synced
   function Is_Synced (Sync : Sync_State) return Boolean with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Get local chain height
   function Get_Local_Height (Sync : Sync_State) return U256 with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Get target chain height
   function Get_Target_Height (Sync : Sync_State) return U256 with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Get number of pending blocks in buffer
   function Get_Buffer_Count (Sync : Sync_State) return Natural with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Check if syncing
   function Is_Syncing (Sync : Sync_State) return Boolean with
      Global => null,
      Pre    => Sync.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Update local chain state after block application
   procedure Update_Local_State (
      Sync       : in Out Sync_State;
      New_Height : in     U256;
      New_Best   : in     Hash256
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Update target based on peer announcements
   procedure Update_Target (
      Sync         : in Out Sync_State;
      Peer_Height  : in     U256;
      Peer_Best    : in     Hash256
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

   --  Clear block buffer
   procedure Clear_Buffer (
      Sync : in Out Sync_State
   ) with
      Global => null,
      Pre    => Sync.Is_Initialized;

end Block_Sync;
