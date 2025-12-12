pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types;
with Anubis_MLDSA_Types;
with Aegis_VM_Types; use Aegis_VM_Types;
with Quantum_Transaction_Auth; use Quantum_Transaction_Auth;
with Scarab_Khnum;
with Scarab_Horus;

--  SCARAB TX Pipeline: Batch ML-DSA-87 Signature Verification
--
--  This package integrates the quantum-resistant transaction authentication
--  with SCARAB's batch signature aggregation for efficient block processing.
--
--  Key Features:
--  - Batch verification of up to 4096 ML-DSA-87 signatures per block
--  - STARK proof generation via HORUS pipeline
--  - KHNUM signature aggregation (4KB constant-size aggregate)
--  - Three TX modes: Standard, Pre-Proven, Batched
--
--  Block Processing Flow:
--  +-------------+     +------------+     +-------------+     +-----------+
--  | Collect TXs | --> | Add to     | --> | Generate    | --> | Include   |
--  | (mempool)   |     | KHNUM      |     | STARK proof |     | in block  |
--  +-------------+     | batch      |     | (HORUS)     |     +-----------+
--                      +------------+     +-------------+
--
--  Gas Savings (via batch verification):
--  - Individual verify: 50,000 gas per signature
--  - Batched verify:    5,000 gas per signature (10x reduction)
--  - Pre-proven:        1,000 gas (user provides proof)
--
--  SPDX-License-Identifier: Apache-2.0

package Scarab_TX_Pipeline with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum transactions per batch
   Max_TX_Per_Batch       : constant := 4096;

   --  Maximum transactions per block
   Max_TX_Per_Block       : constant := 1024;

   --  Gas costs
   Individual_Verify_Gas  : constant Gas_Amount := 50_000;  -- Single ML-DSA-87 verify
   Batched_Verify_Gas     : constant Gas_Amount := 5_000;   -- Amortized in batch
   Pre_Proven_Gas         : constant Gas_Amount := 1_000;   -- User provides proof
   Proof_Submit_Gas       : constant Gas_Amount := 21_000;  -- Base TX cost for proof

   --  Block timing
   Block_Time_Ms          : constant := 6_000;   -- 6 second blocks
   Proof_Budget_Ms        : constant := 4_000;   -- 4 seconds for proof generation

   --  Signature sizes
   Sig_Size               : constant := 4627;    -- ML-DSA-87 signature
   PK_Size                : constant := 2592;    -- ML-DSA-87 public key
   Msg_Hash_Size          : constant := 64;      -- TX hash size

   ---------------------------------------------------------------------------
   --  Local Type Definitions (to avoid ambiguity)
   ---------------------------------------------------------------------------

   subtype TX_Byte_Array is Anubis_Types.Byte_Array;

   subtype TX_Hash_Buffer is TX_Byte_Array (0 .. Msg_Hash_Size - 1);
   subtype TX_Sig_Buffer is TX_Byte_Array (0 .. Sig_Size - 1);
   subtype TX_PK_Buffer is TX_Byte_Array (0 .. PK_Size - 1);
   subtype TX_Hash32 is TX_Byte_Array (0 .. 31);
   subtype TX_Proof_Buffer is TX_Byte_Array (0 .. 163839);  -- Up to 160KB
   subtype TX_Pre_Proof_Buffer is TX_Byte_Array (0 .. 4095);

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype TX_Index is Natural range 0 .. Max_TX_Per_Batch - 1;

   --  Transaction verification mode
   type TX_Mode is (
      Mode_Standard,    -- Verify individually, batch proof async
      Mode_Pre_Proven,  -- User provides their own STARK proof
      Mode_Batched      -- Goes to batch aggregator
   );

   --  Transaction item for batch processing
   type TX_Item is record
      TX_Hash       : TX_Hash_Buffer;
      Signature     : TX_Sig_Buffer;
      Public_Key    : TX_PK_Buffer;
      Mode          : TX_Mode;
      Gas_Used      : Gas_Amount;
      Verified      : Boolean;
      In_Batch      : Boolean;
      Batch_Index   : Natural;
      Block_Height  : Unsigned_64;
   end record;

   type TX_Item_Array is array (TX_Index) of TX_Item;

   --  Batch verification state
   type TX_Batch is record
      Items         : TX_Item_Array;
      Count         : Natural;
      Block_Height  : Unsigned_64;
      Finalized     : Boolean;
      KHNUM_Batch   : Scarab_Khnum.Aggregation_Batch;
      Has_Proof     : Boolean;
   end record;

   --  Block proof (generated after block finalization)
   type Block_Proof is record
      Block_Height  : Unsigned_64;
      TX_Count      : Natural;
      TX_Root       : TX_Hash32;
      Sig_Root      : TX_Hash32;
      Proof_Data    : TX_Proof_Buffer;
      Proof_Len     : Natural;
      FRI_Root      : TX_Hash32;
      Valid         : Boolean;
   end record;

   --  Pre-proven transaction proof
   type Pre_Proof is record
      TX_Hash       : TX_Hash_Buffer;
      Proof_Data    : TX_Pre_Proof_Buffer;
      Proof_Len     : Natural;
      Valid         : Boolean;
   end record;

   --  Pipeline statistics
   type Pipeline_Stats is record
      TX_Processed      : Natural;
      TX_Batched        : Natural;
      TX_Pre_Proven     : Natural;
      TX_Standard       : Natural;
      Total_Gas_Saved   : Unsigned_64;
      Avg_Verify_Time   : Unsigned_64;  -- Microseconds
      Blocks_Processed  : Natural;
   end record;

   --  Pipeline state
   type Pipeline_State is record
      Current_Batch     : TX_Batch;
      Pending_Proofs    : Natural;
      Stats             : Pipeline_Stats;
      Is_Initialized    : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize the TX pipeline
   procedure Initialize (
      State          : out Pipeline_State
   ) with
      Global => null,
      Post => State.Is_Initialized;

   --  Reset pipeline for new block
   procedure Reset_For_Block (
      State          : in Out Pipeline_State;
      Block_Height   : Unsigned_64
   ) with
      Global => null,
      Pre => State.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Transaction Submission
   ---------------------------------------------------------------------------

   --  Submit a standard transaction for batch verification
   procedure Submit_TX (
      State          : in Out Pipeline_State;
      TX_Hash        : TX_Hash_Buffer;
      Signature      : TX_Sig_Buffer;
      Public_Key     : TX_PK_Buffer;
      Mode           : TX_Mode;
      Gas_Cost       : out Gas_Amount;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Is_Initialized;

   --  Submit pre-proven transaction
   procedure Submit_Pre_Proven (
      State          : in Out Pipeline_State;
      TX_Hash        : TX_Hash_Buffer;
      Signature      : TX_Sig_Buffer;
      Public_Key     : TX_PK_Buffer;
      Proof          : Pre_Proof;
      Gas_Cost       : out Gas_Amount;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Is_Initialized;

   --  Submit from Signed_Transaction type
   procedure Submit_Signed_TX (
      State          : in Out Pipeline_State;
      Signed_TX      : Signed_Transaction;
      Public_Key     : Anubis_MLDSA_Types.Public_Key;
      Mode           : TX_Mode;
      Gas_Cost       : out Gas_Amount;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Batch Processing
   ---------------------------------------------------------------------------

   --  Add verified transaction to KHNUM batch
   procedure Add_To_Batch (
      State          : in Out Pipeline_State;
      Item_Index     : Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Is_Initialized and Item_Index < State.Current_Batch.Count;

   --  Process all pending transactions
   procedure Process_Pending (
      State          : in Out Pipeline_State;
      Processed      : out Natural;
      Failed         : out Natural
   ) with
      Global => null,
      Pre => State.Is_Initialized;

   --  Finalize batch and generate KHNUM aggregate
   procedure Finalize_Batch (
      State          : in Out Pipeline_State;
      Aggregate      : out Scarab_Khnum.Aggregated_Signature;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Is_Initialized and State.Current_Batch.Count > 0;

   ---------------------------------------------------------------------------
   --  Proof Generation (HORUS Pipeline)
   ---------------------------------------------------------------------------

   --  Generate STARK proof for batch
   procedure Generate_Block_Proof (
      State          : in     Pipeline_State;
      Block_Height   : Unsigned_64;
      Proof          : out    Block_Proof
   ) with
      Global => null,
      Pre => State.Is_Initialized and State.Current_Batch.Finalized;

   --  Verify pre-provided STARK proof
   function Verify_Pre_Proof (
      Proof          : Pre_Proof;
      TX_Hash        : TX_Hash_Buffer;
      Public_Key     : TX_PK_Buffer
   ) return Boolean with
      Global => null;

   --  Submit block proof to MAAT aggregation
   procedure Submit_To_MAAT (
      Proof          : Block_Proof;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Verification Operations
   ---------------------------------------------------------------------------

   --  Verify individual ML-DSA-87 signature
   function Verify_Single (
      TX_Hash        : TX_Hash_Buffer;
      Signature      : TX_Sig_Buffer;
      Public_Key     : TX_PK_Buffer
   ) return Boolean with
      Global => null;

   --  Verify batch using KHNUM aggregate
   function Verify_Batch (
      Aggregate      : Scarab_Khnum.Aggregated_Signature
   ) return Boolean with
      Global => null;

   --  Verify block proof
   function Verify_Block_Proof (
      Proof          : Block_Proof
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Gas Calculation
   ---------------------------------------------------------------------------

   --  Calculate gas cost based on verification mode
   function Calculate_Gas (
      Mode           : TX_Mode;
      Batch_Size     : Natural
   ) return Gas_Amount with
      Global => null;

   --  Calculate total gas savings for batch
   function Calculate_Savings (
      Batch_Size     : Natural
   ) return Unsigned_64 with
      Global => null,
      Post => Calculate_Savings'Result =
              Unsigned_64 (Batch_Size) *
              (Unsigned_64 (Individual_Verify_Gas) - Unsigned_64 (Batched_Verify_Gas));

   ---------------------------------------------------------------------------
   --  Statistics and Metrics
   ---------------------------------------------------------------------------

   --  Get pipeline statistics
   function Get_Stats (
      State          : Pipeline_State
   ) return Pipeline_Stats with
      Global => null;

   --  Get current batch count
   function Get_Batch_Count (
      State          : Pipeline_State
   ) return Natural with
      Global => null,
      Post => Get_Batch_Count'Result <= Max_TX_Per_Batch;

   --  Check if batch is full
   function Is_Batch_Full (
      State          : Pipeline_State
   ) return Boolean with
      Global => null,
      Post => Is_Batch_Full'Result = (State.Current_Batch.Count >= Max_TX_Per_Batch);

   ---------------------------------------------------------------------------
   --  HORUS Pipeline Integration
   ---------------------------------------------------------------------------

   --  Register as HORUS prover
   procedure Register_Prover (
      Tier           : Scarab_Horus.Prover_Tier;
      Success        : out Boolean
   ) with
      Global => null;

   --  Start HORUS pipeline for block
   procedure Start_Pipeline (
      State          : in Out Pipeline_State;
      Block_Height   : Unsigned_64;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Is_Initialized;

   --  Check pipeline status
   function Pipeline_Ready (
      State          : Pipeline_State
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Batch (
      State          : in Out Pipeline_State
   ) with
      Global => null;

   procedure Zeroize_Item (
      Item           : in Out TX_Item
   ) with
      Global => null;

   procedure Zeroize_Proof (
      Proof          : in Out Block_Proof
   ) with
      Global => null;

end Scarab_TX_Pipeline;
