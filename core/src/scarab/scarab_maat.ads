-------------------------------------------------------------------------------
--  SCARAB - MAAT Hierarchical Non-Recursive Aggregation
--  Merkle Aggregation of Algebraic Transcripts
--
--  Eliminates recursive STARK complexity through hierarchical aggregation:
--  - No "STARK of STARKs" recursion
--  - Merkle tree of FRI commitments
--  - Batch verification of multiple proofs
--  - Constant-size aggregated proof
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;
with Anubis_STARK_Field; use Anubis_STARK_Field;

package Scarab_Maat with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Aggregation Parameters
   ---------------------------------------------------------------------------

   --  Maximum proofs to aggregate
   Max_Proofs_Per_Batch : constant := 1024;

   --  Maximum aggregation tree depth
   Max_Tree_Depth : constant := 10;  -- 2^10 = 1024 leaves

   --  Commitment hash size
   Hash_Size : constant := 32;

   --  Maximum proof size (160KB)
   Max_Proof_Size : constant := 163840;

   --  Aggregated proof overhead
   Agg_Proof_Overhead : constant := 4096;  -- Merkle paths, metadata

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Hash_Value is Byte_Array (0 .. Hash_Size - 1);
   subtype Proof_Bytes is Byte_Array (0 .. Max_Proof_Size - 1);

   --  Individual STARK proof reference
   type Proof_Reference is record
      Proof_Hash     : Hash_Value;        -- Hash of full proof
      FRI_Root       : Hash_Value;        -- FRI commitment root
      Trace_Root     : Hash_Value;        -- Trace commitment
      Public_Inputs  : Byte_Array (0 .. 255);
      Input_Length   : Natural;
      Proof_Size     : Natural;
      Verified       : Boolean;
   end record;

   type Proof_Ref_Array is array (0 .. Max_Proofs_Per_Batch - 1) of Proof_Reference;

   --  Merkle tree node
   type Tree_Node is record
      Hash           : Hash_Value;
      Left_Child     : Natural;           -- Index or 0 if leaf
      Right_Child    : Natural;
      Is_Leaf        : Boolean;
      Leaf_Index     : Natural;           -- Index in proof array
   end record;

   Max_Tree_Nodes : constant := 2 * Max_Proofs_Per_Batch;
   type Tree_Node_Array is array (0 .. Max_Tree_Nodes - 1) of Tree_Node;

   --  Aggregation tree
   type Aggregation_Tree is record
      Nodes          : Tree_Node_Array;
      Node_Count     : Natural;
      Leaf_Count     : Natural;
      Root_Index     : Natural;
      Depth          : Natural;
   end record;

   --  Merkle authentication path
   type Auth_Path_Node is record
      Hash           : Hash_Value;
      Is_Left        : Boolean;  -- True if sibling is on left
   end record;

   type Auth_Path is array (0 .. Max_Tree_Depth - 1) of Auth_Path_Node;

   --  Decommitment for single proof
   type Proof_Decommitment is record
      Proof_Index    : Natural;
      Path           : Auth_Path;
      Path_Length    : Natural;
   end record;

   type Decommitment_Array is array (0 .. Max_Proofs_Per_Batch - 1) of Proof_Decommitment;

   --  Aggregated proof
   type Aggregated_Proof is record
      --  Aggregation metadata
      Num_Proofs     : Natural;
      Tree_Root      : Hash_Value;

      --  Combined FRI data (deduplication)
      Combined_FRI   : Proof_Bytes;
      FRI_Length     : Natural;

      --  Public input commitments
      Input_Root     : Hash_Value;

      --  Batch randomness (Fiat-Shamir)
      Batch_Challenge: Hash_Value;

      --  Decommitments for spot checks
      Decommitments  : Decommitment_Array;
      Num_Decommits  : Natural;

      --  Proof data
      Proof_Data     : Byte_Array (0 .. Max_Proof_Size + Agg_Proof_Overhead - 1);
      Proof_Length   : Natural;
   end record;

   --  Aggregation batch
   type Aggregation_Batch is record
      Proofs         : Proof_Ref_Array;
      Num_Proofs     : Natural;
      Tree           : Aggregation_Tree;
      Tree_Built     : Boolean;
   end record;

   --  Arrays for batch operations
   type Aggregated_Proof_Array is array (Natural range <>) of Aggregated_Proof;
   type Boolean_Array is array (Natural range <>) of Boolean;

   ---------------------------------------------------------------------------
   --  Batch Management
   ---------------------------------------------------------------------------

   --  Initialize empty batch
   procedure Init_Batch (
      Batch          : out Aggregation_Batch
   ) with
      Global => null,
      Post => Batch.Num_Proofs = 0 and not Batch.Tree_Built;

   --  Add proof to batch
   procedure Add_Proof (
      Batch          : in Out Aggregation_Batch;
      Proof_Hash     : Hash_Value;
      FRI_Root       : Hash_Value;
      Trace_Root     : Hash_Value;
      Public_Inputs  : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Public_Inputs'Length <= 256,
      Post => (if Success then Batch.Num_Proofs = Batch.Num_Proofs'Old + 1);

   --  Remove proof from batch
   procedure Remove_Proof (
      Batch          : in Out Aggregation_Batch;
      Index          : Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Index < Batch.Num_Proofs;

   --  Get proof count
   function Proof_Count (Batch : Aggregation_Batch) return Natural with
      Global => null;

   ---------------------------------------------------------------------------
   --  Tree Construction
   ---------------------------------------------------------------------------

   --  Build Merkle tree from proofs
   procedure Build_Tree (
      Batch          : in Out Aggregation_Batch;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Batch.Num_Proofs > 0,
      Post => (if Success then Batch.Tree_Built);

   --  Get tree root
   function Get_Root (Batch : Aggregation_Batch) return Hash_Value with
      Global => null,
      Pre => Batch.Tree_Built;

   --  Generate authentication path for proof
   procedure Get_Auth_Path (
      Batch          : Aggregation_Batch;
      Proof_Index    : Natural;
      Path           : out Auth_Path;
      Path_Length    : out Natural
   ) with
      Global => null,
      Pre => Batch.Tree_Built and Proof_Index < Batch.Num_Proofs,
      Post => Path_Length <= Max_Tree_Depth;

   --  Verify authentication path
   function Verify_Path (
      Leaf_Hash      : Hash_Value;
      Path           : Auth_Path;
      Path_Length    : Natural;
      Root           : Hash_Value
   ) return Boolean with
      Global => null,
      Pre => Path_Length <= Max_Tree_Depth;

   ---------------------------------------------------------------------------
   --  Aggregation
   ---------------------------------------------------------------------------

   --  Aggregate batch into single proof
   procedure Aggregate_Proofs (
      Batch          : Aggregation_Batch;
      Agg_Proof      : out Aggregated_Proof;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Batch.Tree_Built and Batch.Num_Proofs > 0;

   --  Add spot check decommitments based on Fiat-Shamir challenge
   procedure Add_Spot_Checks (
      Batch          : Aggregation_Batch;
      Agg_Proof      : in Out Aggregated_Proof;
      Num_Checks     : Natural
   ) with
      Global => null,
      Pre => Batch.Tree_Built and Num_Checks <= Batch.Num_Proofs;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   --  Verify aggregated proof
   function Verify_Aggregated (
      Agg_Proof      : Aggregated_Proof
   ) return Boolean with
      Global => null;

   --  Verify single proof within aggregation
   function Verify_Single_In_Agg (
      Agg_Proof      : Aggregated_Proof;
      Proof_Index    : Natural
   ) return Boolean with
      Global => null,
      Pre => Proof_Index < Agg_Proof.Num_Proofs;

   --  Extract public inputs for proof
   procedure Extract_Public_Inputs (
      Agg_Proof      : Aggregated_Proof;
      Proof_Index    : Natural;
      Inputs         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Proof_Index < Agg_Proof.Num_Proofs and Inputs'Length >= 256;

   ---------------------------------------------------------------------------
   --  FRI Combination (Key Innovation)
   ---------------------------------------------------------------------------

   --  Combine FRI commitments from multiple proofs
   --  This avoids recursive STARK by:
   --  1. Hashing all FRI roots into tree
   --  2. Using single random challenge for batch
   --  3. Combining query responses
   procedure Combine_FRI (
      Batch          : Aggregation_Batch;
      Challenge      : Hash_Value;
      Combined       : out Proof_Bytes;
      Combined_Len   : out Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Batch.Tree_Built;

   --  Verify combined FRI
   function Verify_Combined_FRI (
      Combined       : Proof_Bytes;
      Combined_Len   : Natural;
      Tree_Root      : Hash_Value;
      Challenge      : Hash_Value
   ) return Boolean with
      Global => null,
      Pre => Combined_Len <= Max_Proof_Size;

   ---------------------------------------------------------------------------
   --  Batch Verification
   ---------------------------------------------------------------------------

   --  Batch verify multiple aggregated proofs
   procedure Batch_Verify (
      Proofs         : Aggregated_Proof_Array;
      Results        : out Boolean_Array
   ) with
      Global => null,
      Pre => Proofs'Length = Results'Length;

   --  Parallel batch verification hint
   function Parallelism_Hint (
      Num_Proofs     : Natural
   ) return Natural with
      Global => null,
      Post => Parallelism_Hint'Result <= 64;

   ---------------------------------------------------------------------------
   --  Compression
   ---------------------------------------------------------------------------

   --  Compress aggregated proof for transmission
   procedure Compress_Proof (
      Agg_Proof      : Aggregated_Proof;
      Compressed     : out Byte_Array;
      Comp_Length    : out Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Compressed'Length >= Max_Proof_Size;

   --  Decompress aggregated proof
   procedure Decompress_Proof (
      Compressed     : Byte_Array;
      Agg_Proof      : out Aggregated_Proof;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   type Aggregation_Stats is record
      Num_Proofs        : Natural;
      Tree_Depth        : Natural;
      Original_Size     : Natural;  -- Sum of individual proof sizes
      Aggregated_Size   : Natural;  -- Aggregated proof size
      Compression_Ratio : Natural;  -- Percentage saved
      Verification_Ops  : Natural;  -- Estimated verification cost
   end record;

   function Get_Stats (Agg_Proof : Aggregated_Proof) return Aggregation_Stats with
      Global => null;

   --  Estimate aggregated proof size
   function Estimate_Agg_Size (
      Num_Proofs     : Natural;
      Avg_Proof_Size : Natural
   ) return Natural with
      Global => null,
      Post => Estimate_Agg_Size'Result <= Max_Proof_Size + Agg_Proof_Overhead;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize aggregated proof
   procedure Serialize_Agg_Proof (
      Agg_Proof      : Aggregated_Proof;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= Max_Proof_Size + Agg_Proof_Overhead;

   --  Deserialize aggregated proof
   procedure Deserialize_Agg_Proof (
      Input          : Byte_Array;
      Agg_Proof      : out Aggregated_Proof;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Hash Utilities
   ---------------------------------------------------------------------------

   --  Hash two children to parent
   function Hash_Pair (Left, Right : Hash_Value) return Hash_Value with
      Global => null;

   --  Hash proof reference to leaf
   function Hash_Proof_Ref (Ref : Proof_Reference) return Hash_Value with
      Global => null;

   --  Compute Fiat-Shamir challenge from tree root
   function Compute_Challenge (
      Tree_Root      : Hash_Value;
      Input_Root     : Hash_Value;
      Domain_Sep     : Byte_Array
   ) return Hash_Value with
      Global => null,
      Pre => Domain_Sep'Length <= 64;

end Scarab_Maat;
