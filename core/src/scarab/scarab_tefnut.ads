-------------------------------------------------------------------------------
--  SCARAB - TEFNUT Light Client Protocol
--  Tiny Efficient Framework for Navigating Unified Transactions
--
--  Ultra-lightweight client protocol designed for extreme resource constraints:
--  - 8KB RAM footprint
--  - 50ms verification time
--  - Post-quantum secure using lattice proofs
--
--  Key Features:
--  - Streaming proof verification (constant memory)
--  - Incremental state sync via Merkle proofs
--  - Compressed header format (128 bytes)
--  - Compatible with MAAT aggregated proofs
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Scarab_Tefnut with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Resource Constraints
   ---------------------------------------------------------------------------

   --  Maximum RAM footprint (bytes)
   Max_RAM_Footprint     : constant := 8192;  -- 8KB

   --  Target verification time (microseconds)
   Target_Verify_Time    : constant := 50_000;  -- 50ms

   --  Compressed header size
   Compact_Header_Size   : constant := 136;

   --  Maximum proof chunk size for streaming
   Max_Chunk_Size        : constant := 1024;  -- 1KB

   --  Merkle proof depth (for 2^32 blocks)
   Max_Merkle_Depth      : constant := 32;

   --  State root size
   State_Root_Size       : constant := 32;

   --  Block hash size
   Block_Hash_Size       : constant := 32;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Merkle_Depth is Natural range 0 .. Max_Merkle_Depth;

   --  Compact block header (128 bytes)
   type Compact_Header is record
      --  Core fields (64 bytes)
      Block_Height       : Unsigned_64;
      Parent_Hash        : Byte_Array (0 .. 31);
      State_Root         : Byte_Array (0 .. 31);

      --  Proof commitment (32 bytes)
      Proof_Commitment   : Byte_Array (0 .. 31);

      --  Validator signature (32 bytes - aggregated)
      Validator_Sig      : Byte_Array (0 .. 31);
   end record with Size => Compact_Header_Size * 8;

   --  Light client state (fits in 8KB)
   type Light_Client_State is record
      --  Current finalized header (128 bytes)
      Finalized_Header   : Compact_Header;

      --  Pending header being verified (128 bytes)
      Pending_Header     : Compact_Header;
      Has_Pending        : Boolean;

      --  Trusted validator set root (32 bytes)
      Validator_Root     : Byte_Array (0 .. 31);

      --  Current epoch
      Current_Epoch      : Unsigned_64;

      --  Verification state
      Verify_Progress    : Natural;
      Verify_Complete    : Boolean;

      --  Streaming proof buffer (1KB)
      Proof_Buffer       : Byte_Array (0 .. Max_Chunk_Size - 1);
      Buffer_Used        : Natural;

      --  Merkle path cache (for incremental updates)
      Path_Cache         : Byte_Array (0 .. Max_Merkle_Depth * 32 - 1);
      Path_Depth         : Merkle_Depth;
   end record;

   --  Sync status
   type Sync_Status is (
      Synced,           -- Fully synchronized
      Syncing,          -- Sync in progress
      Behind,           -- Significantly behind head
      Disconnected,     -- No recent updates
      Error_State       -- Verification failed
   );

   --  Header validation result
   type Header_Result is (
      Valid,            -- Header is valid
      Invalid_Proof,    -- Proof verification failed
      Invalid_Sig,      -- Signature verification failed
      Invalid_Parent,   -- Parent hash mismatch
      Invalid_Height,   -- Height not sequential
      Stale             -- Header too old
   );

   --  State proof for inclusion verification
   type State_Proof is record
      Key            : Byte_Array (0 .. 31);
      Value          : Byte_Array (0 .. 255);
      Value_Length   : Natural;
      Path           : Byte_Array (0 .. Max_Merkle_Depth * 32 - 1);
      Path_Length    : Merkle_Depth;
      Siblings       : Byte_Array (0 .. Max_Merkle_Depth * 32 - 1);
   end record;

   --  Streaming proof chunk
   type Proof_Chunk is record
      Data           : Byte_Array (0 .. Max_Chunk_Size - 1);
      Length         : Natural;
      Chunk_Index    : Natural;
      Total_Chunks   : Natural;
      Is_Final       : Boolean;
   end record;

   --  Validator info (compact)
   type Validator_Info is record
      PK_Hash        : Byte_Array (0 .. 31);  -- Hash of full ML-DSA public key
      Weight         : Unsigned_64;
      Index          : Natural;
   end record;

   --  Epoch transition proof
   type Epoch_Proof is record
      Old_Epoch      : Unsigned_64;
      New_Epoch      : Unsigned_64;
      Old_Val_Root   : Byte_Array (0 .. 31);
      New_Val_Root   : Byte_Array (0 .. 31);
      Transition_Sig : Byte_Array (0 .. 63);
   end record;

   --  Verification metrics
   type Verify_Metrics is record
      Verify_Time_Us : Unsigned_64;
      RAM_Used       : Natural;
      Chunks_Processed : Natural;
      Proofs_Verified : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize light client from trusted checkpoint
   procedure Init_From_Checkpoint (
      State          : out Light_Client_State;
      Checkpoint     : Compact_Header;
      Validator_Root : Byte_Array
   ) with
      Global => null,
      Pre => Validator_Root'Length = 32,
      Post => State.Finalized_Header = Checkpoint
              and not State.Has_Pending;

   --  Initialize light client from genesis
   procedure Init_From_Genesis (
      State          : out Light_Client_State;
      Genesis        : Compact_Header
   ) with
      Global => null,
      Post => State.Finalized_Header = Genesis
              and State.Current_Epoch = 0;

   --  Reset client state (keep trusted roots)
   procedure Reset_State (
      State          : in Out Light_Client_State
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Header Verification (Core Protocol)
   ---------------------------------------------------------------------------

   --  Begin verification of new header
   procedure Begin_Verify (
      State          : in Out Light_Client_State;
      Header         : Compact_Header;
      Result         : out Header_Result
   ) with
      Global => null;

   --  Process streaming proof chunk
   procedure Process_Proof_Chunk (
      State          : in Out Light_Client_State;
      Chunk          : Proof_Chunk;
      Progress       : out Natural;  -- 0-100 percentage
      Complete       : out Boolean
   ) with
      Global => null,
      Pre => Chunk.Length <= Max_Chunk_Size;

   --  Finalize header verification
   procedure Finalize_Verify (
      State          : in Out Light_Client_State;
      Result         : out Header_Result
   ) with
      Global => null,
      Post => (if Result = Valid then
               State.Finalized_Header = State.Pending_Header
               and not State.Has_Pending);

   --  Single-shot verification (if proof fits in buffer)
   procedure Verify_Header_Immediate (
      State          : in Out Light_Client_State;
      Header         : Compact_Header;
      Proof          : Byte_Array;
      Result         : out Header_Result
   ) with
      Global => null,
      Pre => Proof'Length <= Max_Chunk_Size;

   ---------------------------------------------------------------------------
   --  State Verification
   ---------------------------------------------------------------------------

   --  Verify state inclusion proof
   function Verify_State_Proof (
      State          : Light_Client_State;
      Proof          : State_Proof
   ) return Boolean with
      Global => null;

   --  Verify account balance
   procedure Verify_Balance (
      State          : Light_Client_State;
      Address        : Byte_Array;
      Balance        : Unsigned_64;
      Proof          : State_Proof;
      Valid          : out Boolean
   ) with
      Global => null,
      Pre => Address'Length = 32;

   --  Verify transaction inclusion
   procedure Verify_TX_Inclusion (
      State          : Light_Client_State;
      TX_Hash        : Byte_Array;
      Block_Height   : Unsigned_64;
      Proof          : State_Proof;
      Valid          : out Boolean
   ) with
      Global => null,
      Pre => TX_Hash'Length = 32;

   --  Verify contract storage
   procedure Verify_Storage (
      State          : Light_Client_State;
      Contract       : Byte_Array;
      Key            : Byte_Array;
      Value          : out Byte_Array;
      Value_Length   : out Natural;
      Proof          : State_Proof;
      Valid          : out Boolean
   ) with
      Global => null,
      Pre => Contract'Length = 32 and Key'Length = 32
             and Value'Length >= 256;

   ---------------------------------------------------------------------------
   --  Validator Set Management
   ---------------------------------------------------------------------------

   --  Verify validator set transition
   procedure Verify_Epoch_Transition (
      State          : in Out Light_Client_State;
      Proof          : Epoch_Proof;
      Valid          : out Boolean
   ) with
      Global => null,
      Post => (if Valid then State.Current_Epoch = Proof.New_Epoch);

   --  Verify validator in current set
   function Verify_Validator (
      State          : Light_Client_State;
      Validator      : Validator_Info;
      Proof          : Byte_Array
   ) return Boolean with
      Global => null;

   --  Get current validator set root
   function Get_Validator_Root (
      State          : Light_Client_State
   ) return Byte_Array with
      Global => null,
      Post => Get_Validator_Root'Result'Length = 32;

   ---------------------------------------------------------------------------
   --  Sync Protocol
   ---------------------------------------------------------------------------

   --  Get current sync status
   function Get_Sync_Status (
      State          : Light_Client_State;
      Network_Height : Unsigned_64
   ) return Sync_Status with
      Global => null;

   --  Get blocks needed to sync
   function Blocks_Behind (
      State          : Light_Client_State;
      Network_Height : Unsigned_64
   ) return Unsigned_64 with
      Global => null;

   --  Request sync headers (returns range to fetch)
   procedure Request_Sync_Range (
      State          : Light_Client_State;
      Network_Height : Unsigned_64;
      Start_Height   : out Unsigned_64;
      End_Height     : out Unsigned_64
   ) with
      Global => null;

   --  Fast-forward with skip proof (for catching up)
   procedure Skip_Forward (
      State          : in Out Light_Client_State;
      Target_Header  : Compact_Header;
      Skip_Proof     : Byte_Array;
      Valid          : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Compact Proof Verification
   ---------------------------------------------------------------------------

   --  Verify MAAT aggregated proof (constant time)
   function Verify_Aggregated_Proof (
      State          : Light_Client_State;
      Proof_Root     : Byte_Array;
      Num_Proofs     : Natural
   ) return Boolean with
      Global => null,
      Pre => Proof_Root'Length = 32;

   --  Verify KHNUM signature aggregate
   function Verify_Sig_Aggregate (
      State          : Light_Client_State;
      Agg_Sig        : Byte_Array;
      Message_Root   : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Agg_Sig'Length = 4096 and Message_Root'Length = 32;

   ---------------------------------------------------------------------------
   --  Merkle Operations (Memory-Efficient)
   ---------------------------------------------------------------------------

   --  Verify Merkle proof incrementally (streaming)
   procedure Verify_Merkle_Streaming (
      Root           : Byte_Array;
      Leaf           : Byte_Array;
      Sibling        : Byte_Array;
      Is_Left        : Boolean;
      Depth          : Merkle_Depth;
      State          : in Out Light_Client_State;
      Continue       : out Boolean;
      Valid          : out Boolean
   ) with
      Global => null,
      Pre => Root'Length = 32 and Leaf'Length = 32 and Sibling'Length = 32;

   --  Reset Merkle verification state
   procedure Reset_Merkle_State (
      State          : in Out Light_Client_State
   ) with
      Global => null;

   --  Finalize Merkle verification
   function Finalize_Merkle (
      State          : Light_Client_State;
      Expected_Root  : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Expected_Root'Length = 32;

   ---------------------------------------------------------------------------
   --  Header Compression
   ---------------------------------------------------------------------------

   --  Compress full header to compact format
   procedure Compress_Header (
      Full_Header    : Byte_Array;
      Compact        : out Compact_Header;
      Success        : out Boolean
   ) with
      Global => null;

   --  Compute header hash
   procedure Hash_Header (
      Header         : Compact_Header;
      Hash           : out Byte_Array
   ) with
      Global => null,
      Pre => Hash'Length = 32;

   --  Verify header chain (multiple headers)
   procedure Verify_Header_Chain (
      State          : in Out Light_Client_State;
      Headers        : Byte_Array;
      Num_Headers    : Natural;
      Valid          : out Boolean
   ) with
      Global => null,
      Pre => Headers'Length = Num_Headers * Compact_Header_Size;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize light client state
   procedure Serialize_State (
      State          : Light_Client_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= Max_RAM_Footprint;

   --  Deserialize light client state
   procedure Deserialize_State (
      Input          : Byte_Array;
      State          : out Light_Client_State;
      Success        : out Boolean
   ) with
      Global => null;

   --  Serialize compact header
   procedure Serialize_Header (
      Header         : Compact_Header;
      Output         : out Byte_Array
   ) with
      Global => null,
      Pre => Output'Length >= Compact_Header_Size;

   --  Deserialize compact header
   procedure Deserialize_Header (
      Input          : Byte_Array;
      Header         : out Compact_Header;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Input'Length >= Compact_Header_Size;

   ---------------------------------------------------------------------------
   --  Metrics and Diagnostics
   ---------------------------------------------------------------------------

   --  Get verification metrics
   function Get_Metrics (
      State          : Light_Client_State
   ) return Verify_Metrics with
      Global => null;

   --  Get RAM usage
   function Get_RAM_Usage (
      State          : Light_Client_State
   ) return Natural with
      Global => null,
      Post => Get_RAM_Usage'Result <= Max_RAM_Footprint;

   --  Self-test (verify constraints are met)
   function Self_Test return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Security Checks
   ---------------------------------------------------------------------------

   --  Verify proof is within time bounds
   function Is_Timely (
      State          : Light_Client_State;
      Header         : Compact_Header;
      Current_Time   : Unsigned_64
   ) return Boolean with
      Global => null;

   --  Check for eclipse attack indicators
   function Detect_Eclipse (
      State          : Light_Client_State;
      Network_Height : Unsigned_64
   ) return Boolean with
      Global => null;

   --  Verify finality depth
   function Has_Finality (
      State          : Light_Client_State;
      Block_Height   : Unsigned_64;
      Confirmations  : Natural
   ) return Boolean with
      Global => null;

end Scarab_Tefnut;
