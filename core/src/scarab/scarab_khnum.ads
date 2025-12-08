-------------------------------------------------------------------------------
--  SCARAB - KHNUM TX Aggregation Layer
--  Key Homomorphic Numerically Unified Multiplexing
--
--  Aggregates multiple ML-DSA-87 signatures into a single compact proof,
--  enabling massive block compression while maintaining post-quantum security.
--
--  Key Features:
--  - Batch aggregation of up to 4096 signatures per block
--  - Lattice-based homomorphic signature combination
--  - Constant-size aggregate (4KB regardless of batch size)
--  - Compatible with MAAT hierarchical proof aggregation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Scarab_Khnum with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Aggregation Parameters
   ---------------------------------------------------------------------------

   --  Maximum signatures per aggregation batch
   Max_Sigs_Per_Batch    : constant := 4096;

   --  Maximum signatures per micro-batch (for parallelization)
   Max_Sigs_Per_Micro    : constant := 64;

   --  Aggregated signature size (constant regardless of batch size)
   Aggregated_Sig_Size   : constant := 4096;  -- 4KB

   --  Individual ML-DSA-87 signature size
   MLDSA_Sig_Size        : constant := 4627;

   --  ML-DSA-87 public key size
   MLDSA_PK_Size         : constant := 2592;

   --  Message hash size
   Message_Hash_Size     : constant := 64;

   --  Domain separator size
   Domain_Sep_Size       : constant := 32;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Sig_Index is Natural range 0 .. Max_Sigs_Per_Batch - 1;
   subtype Micro_Index is Natural range 0 .. Max_Sigs_Per_Micro - 1;

   --  Individual signature entry
   type Signature_Entry is record
      Signer_PK      : Byte_Array (0 .. MLDSA_PK_Size - 1);
      Message_Hash   : Byte_Array (0 .. Message_Hash_Size - 1);
      Signature      : Byte_Array (0 .. MLDSA_Sig_Size - 1);
      Verified       : Boolean;
   end record;

   type Signature_Entry_Array is array (Sig_Index) of Signature_Entry;

   --  Aggregation batch
   type Aggregation_Batch is record
      Entries        : Signature_Entry_Array;
      Count          : Natural;
      Domain_Sep     : Byte_Array (0 .. Domain_Sep_Size - 1);
      Block_Height   : Unsigned_64;
      Finalized      : Boolean;
   end record;

   --  Aggregated signature (constant size)
   type Aggregated_Signature is record
      --  Combined signature data
      Aggregate      : Byte_Array (0 .. Aggregated_Sig_Size - 1);

      --  Metadata
      Num_Sigs       : Natural;
      Block_Height   : Unsigned_64;

      --  Merkle root of all message hashes
      Message_Root   : Byte_Array (0 .. 31);

      --  Merkle root of all public keys
      Signer_Root    : Byte_Array (0 .. 31);

      --  Commitment to batch (for verification)
      Batch_Commit   : Byte_Array (0 .. 63);

      --  Domain separator used
      Domain_Sep     : Byte_Array (0 .. Domain_Sep_Size - 1);
   end record;

   --  Micro-batch for parallel aggregation
   type Micro_Batch is record
      Entries        : Signature_Entry_Array;
      Count          : Natural;
      Start_Index    : Natural;
      Partial_Agg    : Byte_Array (0 .. Aggregated_Sig_Size - 1);
      Aggregated     : Boolean;
   end record;

   --  Parallelization hint
   type Parallel_Config is record
      Num_Micros     : Natural;
      Sigs_Per_Micro : Natural;
      Num_Threads    : Natural;
   end record;

   --  Verification result
   type Verify_Result is record
      Valid          : Boolean;
      Num_Verified   : Natural;
      Failed_Index   : Natural;
      Error_Code     : Natural;
   end record;

   --  Aggregation statistics
   type Aggregation_Stats is record
      Num_Signatures    : Natural;
      Original_Size     : Natural;  -- Sum of individual sigs
      Aggregated_Size   : Natural;  -- Constant 4KB
      Compression_Ratio : Natural;  -- Percentage saved
      Aggregation_Time  : Unsigned_64;  -- Microseconds
   end record;

   ---------------------------------------------------------------------------
   --  Batch Management
   ---------------------------------------------------------------------------

   --  Initialize empty batch
   procedure Init_Batch (
      Batch          : out Aggregation_Batch;
      Domain_Sep     : Byte_Array;
      Block_Height   : Unsigned_64
   ) with
      Global => null,
      Pre => Domain_Sep'Length = Domain_Sep_Size,
      Post => Batch.Count = 0 and not Batch.Finalized;

   --  Add signature to batch
   procedure Add_Signature (
      Batch          : in out Aggregation_Batch;
      Signer_PK      : Byte_Array;
      Message_Hash   : Byte_Array;
      Signature      : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => not Batch.Finalized
             and Signer_PK'Length = MLDSA_PK_Size
             and Message_Hash'Length = Message_Hash_Size
             and Signature'Length = MLDSA_Sig_Size,
      Post => (if Success then Batch.Count = Batch.Count'Old + 1);

   --  Remove signature from batch (by index)
   procedure Remove_Signature (
      Batch          : in Out Aggregation_Batch;
      Index          : Sig_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => not Batch.Finalized and Index < Batch.Count;

   --  Get current signature count
   function Signature_Count (Batch : Aggregation_Batch) return Natural with
      Global => null;

   --  Check if batch is full
   function Is_Full (Batch : Aggregation_Batch) return Boolean with
      Global => null,
      Post => Is_Full'Result = (Batch.Count >= Max_Sigs_Per_Batch);

   ---------------------------------------------------------------------------
   --  Individual Verification (Pre-Aggregation)
   ---------------------------------------------------------------------------

   --  Verify single signature before aggregation
   procedure Verify_Single (
      Batch          : in Out Aggregation_Batch;
      Index          : Sig_Index;
      Valid          : out Boolean
   ) with
      Global => null,
      Pre => Index < Batch.Count;

   --  Verify all signatures in batch (sequential)
   procedure Verify_All_Sequential (
      Batch          : in Out Aggregation_Batch;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => Batch.Count > 0;

   --  Verify all signatures in batch (parallel hint)
   procedure Verify_All_Parallel (
      Batch          : in Out Aggregation_Batch;
      Num_Threads    : Natural;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => Batch.Count > 0 and Num_Threads in 1 .. 64;

   ---------------------------------------------------------------------------
   --  Aggregation Core
   ---------------------------------------------------------------------------

   --  Aggregate all verified signatures into constant-size proof
   procedure Aggregate (
      Batch          : in Out Aggregation_Batch;
      Agg_Sig        : out Aggregated_Signature;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Batch.Count > 0,
      Post => (if Success then Batch.Finalized
               and Agg_Sig.Num_Sigs = Batch.Count);

   --  Aggregate using micro-batches (for parallelization)
   procedure Aggregate_Parallel (
      Batch          : in Out Aggregation_Batch;
      Config         : Parallel_Config;
      Agg_Sig        : out Aggregated_Signature;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Batch.Count > 0 and Config.Num_Threads in 1 .. 64;

   --  Combine two aggregated signatures (for hierarchical aggregation)
   procedure Combine_Aggregates (
      Agg1           : Aggregated_Signature;
      Agg2           : Aggregated_Signature;
      Combined       : out Aggregated_Signature;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Agg1.Num_Sigs > 0 and Agg2.Num_Sigs > 0,
      Post => (if Success then
               Combined.Num_Sigs = Agg1.Num_Sigs + Agg2.Num_Sigs);

   ---------------------------------------------------------------------------
   --  Verification of Aggregated Signatures
   ---------------------------------------------------------------------------

   --  Verify aggregated signature (constant time regardless of batch size)
   function Verify_Aggregated (
      Agg_Sig        : Aggregated_Signature
   ) return Boolean with
      Global => null;

   --  Verify with public key set (for external verifiers)
   function Verify_With_Keys (
      Agg_Sig        : Aggregated_Signature;
      PK_Root        : Byte_Array;
      Msg_Root       : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => PK_Root'Length = 32 and Msg_Root'Length = 32;

   --  Batch verify multiple aggregated signatures
   procedure Batch_Verify_Aggregated (
      Agg_Sigs       : Aggregated_Signature;
      Results        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Merkle Tree Operations
   ---------------------------------------------------------------------------

   --  Build Merkle tree of message hashes
   procedure Build_Message_Tree (
      Batch          : Aggregation_Batch;
      Root           : out Byte_Array
   ) with
      Global => null,
      Pre => Batch.Count > 0 and Root'Length = 32;

   --  Build Merkle tree of public keys
   procedure Build_Signer_Tree (
      Batch          : Aggregation_Batch;
      Root           : out Byte_Array
   ) with
      Global => null,
      Pre => Batch.Count > 0 and Root'Length = 32;

   --  Generate inclusion proof for specific signature
   procedure Generate_Inclusion_Proof (
      Batch          : Aggregation_Batch;
      Index          : Sig_Index;
      Proof          : out Byte_Array;
      Proof_Length   : out Natural
   ) with
      Global => null,
      Pre => Index < Batch.Count and Proof'Length >= 512;

   --  Verify inclusion proof
   function Verify_Inclusion (
      Agg_Sig        : Aggregated_Signature;
      Message_Hash   : Byte_Array;
      Signer_PK      : Byte_Array;
      Proof          : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Message_Hash'Length = Message_Hash_Size
             and Signer_PK'Length = MLDSA_PK_Size;

   ---------------------------------------------------------------------------
   --  Homomorphic Operations (Lattice-Based)
   ---------------------------------------------------------------------------

   --  Combine signatures homomorphically
   procedure Homomorphic_Combine (
      Sig1           : Byte_Array;
      Sig2           : Byte_Array;
      Challenge      : Byte_Array;
      Combined       : out Byte_Array
   ) with
      Global => null,
      Pre => Sig1'Length = MLDSA_Sig_Size
             and Sig2'Length = MLDSA_Sig_Size
             and Challenge'Length = 64
             and Combined'Length = MLDSA_Sig_Size;

   --  Compute aggregation challenge (Fiat-Shamir)
   procedure Compute_Challenge (
      Batch_Commit   : Byte_Array;
      Domain_Sep     : Byte_Array;
      Challenge      : out Byte_Array
   ) with
      Global => null,
      Pre => Batch_Commit'Length = 64
             and Domain_Sep'Length = Domain_Sep_Size
             and Challenge'Length = 64;

   --  Compress combined signature to constant size
   procedure Compress_Aggregate (
      Combined       : Byte_Array;
      Num_Sigs       : Natural;
      Compressed     : out Byte_Array
   ) with
      Global => null,
      Pre => Compressed'Length = Aggregated_Sig_Size;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize aggregated signature
   procedure Serialize (
      Agg_Sig        : Aggregated_Signature;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= Aggregated_Sig_Size + 256;

   --  Deserialize aggregated signature
   procedure Deserialize (
      Input          : Byte_Array;
      Agg_Sig        : out Aggregated_Signature;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Integration with MAAT
   ---------------------------------------------------------------------------

   --  Convert aggregated signature to MAAT-compatible proof reference
   procedure To_MAAT_Proof (
      Agg_Sig        : Aggregated_Signature;
      Proof_Hash     : out Byte_Array;
      FRI_Root       : out Byte_Array;
      Public_Inputs  : out Byte_Array
   ) with
      Global => null,
      Pre => Proof_Hash'Length = 32
             and FRI_Root'Length = 32
             and Public_Inputs'Length >= 128;

   ---------------------------------------------------------------------------
   --  Statistics and Metrics
   ---------------------------------------------------------------------------

   --  Get aggregation statistics
   function Get_Stats (
      Batch          : Aggregation_Batch;
      Agg_Sig        : Aggregated_Signature
   ) return Aggregation_Stats with
      Global => null;

   --  Estimate compression ratio
   function Estimate_Compression (
      Num_Signatures : Natural
   ) return Natural with
      Global => null,
      Pre => Num_Signatures > 0,
      Post => Estimate_Compression'Result <= 100;

   --  Get parallelization configuration hint
   function Get_Parallel_Config (
      Num_Sigs       : Natural;
      Num_Cores      : Natural
   ) return Parallel_Config with
      Global => null,
      Pre => Num_Sigs > 0 and Num_Cores in 1 .. 64;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Batch (Batch : in Out Aggregation_Batch) with
      Global => null;

   procedure Zeroize_Signature (Sig : in Out Aggregated_Signature) with
      Global => null;

end Scarab_Khnum;
