--  SPHINX Lite Checkpoint: Checkpoint Protocol Implementation
--
--  Implements the checkpoint signing and verification protocol
--  for ultra-light clients with ~100KB proof sizes.
--
--  Key Features:
--  - 14-of-21 threshold checkpoint signing
--  - Checkpoint chain validation
--  - Signature aggregation
--  - ~100KB checkpoint proof
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.3: Checkpoint Protocol
--  - SCARAB v2.0 Immortal Edition

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_Lite_Types; use Sphinx_Lite_Types;

package Sphinx_Lite_Checkpoint with
   SPARK_Mode => On,
   Abstract_State => Checkpoint_State,
   Initializes => Checkpoint_State
is

   ---------------------------------------------------------------------------
   --  Checkpoint Management
   ---------------------------------------------------------------------------

   --  Initialize checkpoint system
   procedure Initialize (
      Genesis_Checkpoint : in Checkpoint;
      Committee_Root     : in Hash256
   ) with
      Global => (Output => Checkpoint_State),
      Pre    => Genesis_Checkpoint.Valid;

   --  Check if initialized
   function Is_Initialized return Boolean with
      Global => Checkpoint_State;

   --  Get latest checkpoint
   function Get_Latest_Checkpoint return Checkpoint with
      Global => Checkpoint_State;

   --  Get checkpoint count
   function Get_Checkpoint_Count return Natural with
      Global => Checkpoint_State;

   ---------------------------------------------------------------------------
   --  Checkpoint Creation
   ---------------------------------------------------------------------------

   --  Create new checkpoint proposal
   procedure Create_Checkpoint (
      Block_Number : in  Unsigned_64;
      Block_Hash   : in  Hash256;
      State_Root   : in  Hash256;
      Timestamp    : in  Unsigned_64;
      Checkpoint   : out Sphinx_Lite_Types.Checkpoint;
      Success      : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) with
      Global => Checkpoint_State;

   --  Add signature to checkpoint
   procedure Add_Signature (
      Checkpoint   : in out Sphinx_Lite_Types.Checkpoint;
      Member_Index : in     Committee_Index;
      Signature    : in     Hash512;
      Success      : out    Boolean;
      Error        : out    Sphinx_Lite_Error
   ) with
      Global => null,
      Pre    => Member_Index < Committee_Size;

   --  Check if checkpoint has enough signatures
   function Is_Finalized (CP : Checkpoint) return Boolean with
      Global => null,
      Post   => Is_Finalized'Result = (CP.Sig_Count >= Threshold and then CP.Valid);

   ---------------------------------------------------------------------------
   --  Checkpoint Verification
   ---------------------------------------------------------------------------

   --  Verify checkpoint validity
   procedure Verify_Checkpoint (
      CP     : in  Checkpoint;
      Result : out Verification_Result
   ) with
      Global => Checkpoint_State;

   --  Verify checkpoint chain (connects to known checkpoint)
   procedure Verify_Chain (
      New_Checkpoint : in  Checkpoint;
      Result         : out Verification_Result
   ) with
      Global => Checkpoint_State;

   --  Apply verified checkpoint (update state)
   procedure Apply_Checkpoint (
      CP      : in  Checkpoint;
      Success : out Boolean;
      Error   : out Sphinx_Lite_Error
   ) with
      Global => (In_Out => Checkpoint_State),
      Pre    => Is_Finalized (CP);

   ---------------------------------------------------------------------------
   --  Checkpoint Queries
   ---------------------------------------------------------------------------

   --  Get checkpoint by block number
   procedure Get_Checkpoint_At (
      Block_Number : in  Unsigned_64;
      CP           : out Checkpoint;
      Found        : out Boolean
   ) with
      Global => Checkpoint_State;

   --  Check if block is checkpointed
   function Is_Checkpointed (Block_Number : Unsigned_64) return Boolean with
      Global => Checkpoint_State;

   --  Get distance to latest checkpoint
   function Blocks_Since_Checkpoint (
      Block_Number : Unsigned_64
   ) return Unsigned_64 with
      Global => Checkpoint_State;

   ---------------------------------------------------------------------------
   --  Checkpoint Serialization (for network transmission)
   ---------------------------------------------------------------------------

   --  Checkpoint size in bytes (for IoT bandwidth planning)
   Checkpoint_Size : constant := 32 + 32 + 32 + 8 + 4 + 4 + 1;  -- ~113 bytes

   --  Maximum serialized checkpoint proof size
   Max_Proof_Size : constant := 100 * 1024;  -- 100 KB

   type Checkpoint_Bytes is array (0 .. Checkpoint_Size - 1) of Unsigned_8;

   --  Serialize checkpoint to bytes
   procedure Serialize_Checkpoint (
      CP    : in  Checkpoint;
      Bytes : out Checkpoint_Bytes
   ) with
      Global => null;

   --  Deserialize checkpoint from bytes
   procedure Deserialize_Checkpoint (
      Bytes   : in  Checkpoint_Bytes;
      CP      : out Checkpoint;
      Success : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   --  Get total checkpoints processed
   function Total_Checkpoints return Natural with
      Global => Checkpoint_State;

   --  Get average signatures per checkpoint
   function Average_Signatures return Natural with
      Global => Checkpoint_State;

end Sphinx_Lite_Checkpoint;
