--  SPHINX Lite IoT: Minimal Light Client for Constrained Devices
--
--  Ultra-minimal client for IoT devices with severe resource constraints.
--  Designed for microcontrollers with ~100 bytes RAM for trusted state.
--
--  Key Features:
--  - ~100 bytes trusted state (checkpoint hash + block number)
--  - ~2KB bandwidth per sync operation
--  - Battery-optimized sync intervals
--  - Checkpoint-only verification (no full block processing)
--
--  Resource Budget:
--  - RAM: ~100 bytes trusted state
--  - Flash: ~10KB code (excluding crypto primitives)
--  - Bandwidth: ~2KB per sync
--  - CPU: Minimal (hash verification only)
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.5: IoT Protocol
--  - SCARAB v2.0 Immortal Edition

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;
with Sphinx_Lite_Types; use Sphinx_Lite_Types;

package Sphinx_Lite_IoT with
   SPARK_Mode => On,
   Abstract_State => IoT_State,
   Initializes => IoT_State
is

   ---------------------------------------------------------------------------
   --  Constants (designed for minimal footprint)
   ---------------------------------------------------------------------------

   --  Trusted state size in bytes (~100 bytes)
   Trusted_State_Size : constant := 32 + 8 + 4 + 4 + 32 + 1;  -- 81 bytes

   --  Sync message size (~2KB max)
   Max_Sync_Message_Size : constant := 2 * 1024;

   --  Default sync interval in seconds (15 minutes)
   Default_Sync_Interval : constant := 15 * 60;

   --  Maximum checkpoint age before forcing sync (1 hour)
   Max_Checkpoint_Age_Seconds : constant := 60 * 60;

   --  Battery threshold for reduced sync (percentage)
   Low_Battery_Threshold : constant := 20;

   ---------------------------------------------------------------------------
   --  IoT Client State (minimal ~100 bytes)
   ---------------------------------------------------------------------------

   type IoT_Trusted_State is record
      --  Core trusted state (~72 bytes)
      Checkpoint_Hash  : Hash256;        -- 32 bytes
      Block_Number     : Unsigned_64;    -- 8 bytes
      Chain_ID         : Unsigned_32;    -- 4 bytes
      Committee_Epoch  : Unsigned_32;    -- 4 bytes

      --  Optional committee root (~32 bytes)
      Committee_Root   : Hash256;        -- 32 bytes

      --  Status flags (1 byte)
      Initialized      : Boolean;
   end record;

   Null_IoT_State : constant IoT_Trusted_State := (
      Checkpoint_Hash  => (others => 0),
      Block_Number     => 0,
      Chain_ID         => 0,
      Committee_Epoch  => 0,
      Committee_Root   => (others => 0),
      Initialized      => False
   );

   ---------------------------------------------------------------------------
   --  Sync Request/Response Types
   ---------------------------------------------------------------------------

   type Sync_Request is record
      Client_Block     : Unsigned_64;      -- Last known block
      Client_Epoch     : Unsigned_32;      -- Last known epoch
      Request_Proof    : Boolean;          -- Request account proof?
      Proof_Address    : Contract_Address; -- Address to prove (if requested)
   end record;

   type Sync_Response_Status is (
      Sync_OK,
      Sync_Already_Current,
      Sync_Checkpoint_Available,
      Sync_Committee_Rotation,
      Sync_Error_Invalid,
      Sync_Error_Stale,
      Sync_Error_No_Data
   );

   type Sync_Response is record
      Status           : Sync_Response_Status;
      New_Checkpoint   : Checkpoint;
      Has_Account_Proof: Boolean;
      Account_Balance  : U256;
      Proof_Valid      : Boolean;
   end record;

   Null_Sync_Response : constant Sync_Response := (
      Status           => Sync_Error_No_Data,
      New_Checkpoint   => Null_Checkpoint,
      Has_Account_Proof=> False,
      Account_Balance  => U256_Zero,
      Proof_Valid      => False
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize IoT client with genesis state
   procedure Initialize (
      Genesis_Checkpoint : in Checkpoint;
      Chain_ID           : in Unsigned_32;
      Committee_Root     : in Hash256
   ) with
      Global => (Output => IoT_State),
      Pre    => Genesis_Checkpoint.Valid;

   --  Check if client is initialized
   function Is_Initialized return Boolean with
      Global => IoT_State;

   --  Reset client to uninitialized state
   procedure Reset with
      Global => (Output => IoT_State);

   ---------------------------------------------------------------------------
   --  State Access
   ---------------------------------------------------------------------------

   --  Get current trusted state
   function Get_Trusted_State return IoT_Trusted_State with
      Global => IoT_State;

   --  Get last known block number
   function Get_Block_Number return Unsigned_64 with
      Global => IoT_State;

   --  Get checkpoint hash
   function Get_Checkpoint_Hash return Hash256 with
      Global => IoT_State;

   --  Get chain ID
   function Get_Chain_ID return Unsigned_32 with
      Global => IoT_State;

   ---------------------------------------------------------------------------
   --  Sync Operations (bandwidth-optimized)
   ---------------------------------------------------------------------------

   --  Create sync request message
   procedure Create_Sync_Request (
      Request_Proof : in  Boolean;
      Proof_Address : in  Contract_Address;
      Request       : out Sync_Request
   ) with
      Global => IoT_State;

   --  Process sync response and update state
   procedure Process_Sync_Response (
      Response : in  Sync_Response;
      Success  : out Boolean;
      Error    : out Sphinx_Lite_Error
   ) with
      Global => (In_Out => IoT_State);

   --  Quick check if sync is needed
   function Needs_Sync (Current_Block : Unsigned_64) return Boolean with
      Global => IoT_State;

   ---------------------------------------------------------------------------
   --  Checkpoint Verification (minimal)
   ---------------------------------------------------------------------------

   --  Verify checkpoint update is valid
   procedure Verify_Checkpoint_Update (
      New_Checkpoint   : in  Checkpoint;
      Expected_Root    : in  Hash256;
      Valid            : out Boolean;
      Error            : out Sphinx_Lite_Error
   ) with
      Global => IoT_State,
      Pre    => New_Checkpoint.Valid;

   --  Apply verified checkpoint (update trusted state)
   procedure Apply_Checkpoint (
      New_Checkpoint : in  Checkpoint;
      Success        : out Boolean;
      Error          : out Sphinx_Lite_Error
   ) with
      Global => (In_Out => IoT_State),
      Pre    => New_Checkpoint.Valid;

   ---------------------------------------------------------------------------
   --  Committee Rotation (for epoch changes)
   ---------------------------------------------------------------------------

   --  Check if committee rotation needed
   function Needs_Committee_Update (New_Epoch : Unsigned_32) return Boolean with
      Global => IoT_State;

   --  Update committee root (on epoch change)
   procedure Update_Committee_Root (
      New_Root    : in  Hash256;
      New_Epoch   : in  Unsigned_32;
      Success     : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) with
      Global => (In_Out => IoT_State);

   ---------------------------------------------------------------------------
   --  Power Management (battery optimization)
   ---------------------------------------------------------------------------

   type Power_Mode is (
      Power_Normal,       -- Regular sync interval
      Power_Low,          -- Extended sync interval
      Power_Critical,     -- Minimal syncing
      Power_Sleep         -- No syncing until wake
   );

   --  Get recommended sync interval based on power mode
   function Get_Sync_Interval (Mode : Power_Mode) return Unsigned_32 with
      Global => null,
      Post   => Get_Sync_Interval'Result >= 60;  -- At least 60 seconds

   --  Check if sync should be skipped (battery saving)
   function Should_Skip_Sync (
      Mode           : Power_Mode;
      Seconds_Since  : Unsigned_32
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization (for persistent storage)
   ---------------------------------------------------------------------------

   type Serialized_IoT_State is array (0 .. Trusted_State_Size - 1) of Unsigned_8;

   --  Serialize trusted state for flash storage
   procedure Serialize_State (
      State : in  IoT_Trusted_State;
      Bytes : out Serialized_IoT_State
   ) with
      Global => null;

   --  Deserialize trusted state from flash
   procedure Deserialize_State (
      Bytes   : in  Serialized_IoT_State;
      State   : out IoT_Trusted_State;
      Success : out Boolean
   ) with
      Global => null;

   --  Save current state to bytes
   procedure Save_State (
      Bytes : out Serialized_IoT_State
   ) with
      Global => IoT_State;

   --  Load state from bytes
   procedure Load_State (
      Bytes   : in  Serialized_IoT_State;
      Success : out Boolean
   ) with
      Global => (Output => IoT_State);

   ---------------------------------------------------------------------------
   --  Statistics (optional, for debugging)
   ---------------------------------------------------------------------------

   --  Get total sync operations performed
   function Total_Syncs return Natural with
      Global => IoT_State;

   --  Get bytes received since init
   function Bytes_Received return Unsigned_64 with
      Global => IoT_State;

   --  Get checkpoint update count
   function Checkpoint_Updates return Natural with
      Global => IoT_State;

end Sphinx_Lite_IoT;
