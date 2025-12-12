-------------------------------------------------------------------------------
--  Consensus: Proof-of-Authority Consensus Engine for AnubisVM
--
--  This package implements a Proof-of-Authority (PoA) consensus mechanism
--  for AnubisVM devnet/testnet. Key features:
--
--  - Authority-based block production
--  - Round-robin validator selection
--  - ML-DSA-87 signed blocks
--  - Slot-based timing
--  - Finality tracking
--
--  Consensus Flow:
--  +-------------+     +---------------+     +---------------+     +--------+
--  | Slot Timer  | --> | Check if      | --> | Build Block   | --> | Sign & |
--  | (6 sec)     |     | Our Turn      |     | (from mempool)|     | Propose|
--  +-------------+     +---------------+     +---------------+     +--------+
--
--  Authority Management:
--  - Authorities are identified by ML-DSA-87 public key hashes
--  - Round-robin selection: slot % num_authorities
--  - Authorities can be added/removed via governance
--
--  Finality:
--  - Block is finalized after 2/3 authorities have built on top
--  - Simple immediate finality for single-authority devnet
--
--  Security:
--  - All blocks must be signed by the scheduled authority
--  - Invalid signatures result in block rejection
--  - Equivocation (double-signing) is detectable
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_MLDSA_Types;
with Block_Builder;
with Mempool;
with P2P_Network;

package Consensus with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Slot duration (seconds)
   Slot_Duration : constant := 6;

   --  Maximum authorities
   Max_Authorities : constant := 21;

   --  Blocks per epoch
   Blocks_Per_Epoch : constant := 100;

   --  Finality threshold (2/3 of authorities)
   Finality_Threshold : constant := 14;  -- ceil(21 * 2/3)

   --  Genesis slot timestamp (Unix epoch of genesis block)
   Genesis_Timestamp : constant := 0;  -- Set at network launch

   ---------------------------------------------------------------------------
   --  Authority Types
   ---------------------------------------------------------------------------

   --  Authority entry
   type Authority is record
      Is_Valid     : Boolean;
      Public_Key   : Anubis_MLDSA_Types.Public_Key;
      Key_Hash     : Hash256;  -- SHA3-256(public_key)
      Name         : String (1 .. 32);
      Name_Len     : Natural;
      Blocks_Produced : Natural;
      Last_Block   : U256;
      Is_Active    : Boolean;
   end record;

   --  Authority storage
   subtype Authority_Index is Natural range 0 .. Max_Authorities - 1;
   type Authority_Storage is array (Authority_Index) of Authority;

   ---------------------------------------------------------------------------
   --  Slot Types
   ---------------------------------------------------------------------------

   --  Slot number
   subtype Slot_Number is Unsigned_64;

   --  Slot info
   type Slot_Info is record
      Slot          : Slot_Number;
      Epoch         : Natural;
      Slot_In_Epoch : Natural;
      Authority_Idx : Authority_Index;
      Timestamp     : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Consensus State Types
   ---------------------------------------------------------------------------

   --  Consensus result codes
   type Consensus_Result is (
      Consensus_OK,
      Consensus_Not_Our_Turn,
      Consensus_Invalid_Signature,
      Consensus_Invalid_Authority,
      Consensus_Invalid_Slot,
      Consensus_Already_Proposed,
      Consensus_No_Transactions,
      Consensus_Block_Build_Failed,
      Consensus_Not_Initialized,
      Consensus_Not_Running
   );

   --  Consensus mode
   type Consensus_Mode is (
      Mode_Single_Authority,   -- Single validator (devnet)
      Mode_Round_Robin,        -- Round-robin PoA
      Mode_Delegated           -- Delegated PoS (future)
   );

   --  Consensus statistics
   type Consensus_Stats is record
      Blocks_Proposed     : Natural;
      Blocks_Validated    : Natural;
      Blocks_Rejected     : Natural;
      Slots_Missed        : Natural;
      Current_Slot        : Slot_Number;
      Current_Epoch       : Natural;
   end record;

   --  Consensus state
   type Consensus_State is record
      --  Authority list
      Authorities     : Authority_Storage;
      Authority_Count : Natural;

      --  Our authority info
      Our_Index       : Authority_Index;
      Our_Key         : Anubis_MLDSA_Types.Public_Key;
      Our_Secret_Key  : Anubis_MLDSA_Types.Secret_Key;  -- For block signing
      Our_Key_Hash    : Hash256;
      Is_Authority    : Boolean;

      --  Current slot tracking
      Current_Slot    : Slot_Number;
      Last_Slot_Time  : Unsigned_64;

      --  Chain state
      Chain_Height    : U256;
      Last_Block_Hash : Hash256;
      Last_Block_Slot : Slot_Number;

      --  Mode and stats
      Mode            : Consensus_Mode;
      Stats           : Consensus_Stats;

      --  Flags
      Is_Initialized  : Boolean;
      Is_Running      : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize consensus engine
   procedure Initialize (
      Cons          : out Consensus_State;
      Mode          : in  Consensus_Mode;
      Our_Key       : in  Anubis_MLDSA_Types.Public_Key;
      Our_Secret    : in  Anubis_MLDSA_Types.Secret_Key
   ) with
      Global => null,
      Post   => Cons.Is_Initialized and not Cons.Is_Running;

   --  Add an authority
   procedure Add_Authority (
      Cons     : in Out Consensus_State;
      Pub_Key  : in     Anubis_MLDSA_Types.Public_Key;
      Name     : in     String;
      Name_Len : in     Natural;
      Result   : out    Consensus_Result
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized and Name_Len <= 32;

   --  Remove an authority
   procedure Remove_Authority (
      Cons     : in Out Consensus_State;
      Key_Hash : in     Hash256;
      Result   : out    Consensus_Result
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Consensus Control
   ---------------------------------------------------------------------------

   --  Start consensus engine
   procedure Start (
      Cons   : in Out Consensus_State;
      Result : out    Consensus_Result
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Stop consensus engine
   procedure Stop (
      Cons : in Out Consensus_State
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Process a slot tick (call every Slot_Duration seconds)
   procedure Process_Slot (
      Cons         : in Out Consensus_State;
      Builder      : in Out Block_Builder.Builder_State;
      Pool         : in Out Mempool.Mempool_State;
      Net          : in Out P2P_Network.Network_State;
      Current_Time : in     Unsigned_64;
      Result       : out    Consensus_Result
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized and Cons.Is_Running;

   ---------------------------------------------------------------------------
   --  Block Production
   ---------------------------------------------------------------------------

   --  Check if it's our turn to produce a block
   function Is_Our_Turn (
      Cons : Consensus_State;
      Slot : Slot_Number
   ) return Boolean with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Get the scheduled authority for a slot
   function Get_Slot_Authority (
      Cons : Consensus_State;
      Slot : Slot_Number
   ) return Authority_Index with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Produce a block for the current slot
   procedure Produce_Block (
      Cons    : in Out Consensus_State;
      Builder : in Out Block_Builder.Builder_State;
      Pool    : in Out Mempool.Mempool_State;
      Net     : in Out P2P_Network.Network_State;
      Block   : out    Block_Builder.Block;
      Result  : out    Consensus_Result
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized and Cons.Is_Running;

   ---------------------------------------------------------------------------
   --  Block Validation
   ---------------------------------------------------------------------------

   --  Validate a proposed block
   procedure Validate_Block (
      Cons   : in     Consensus_State;
      Block  : in     Block_Builder.Block;
      Result : out    Consensus_Result
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Verify block signature
   procedure Verify_Block_Signature (
      Cons   : in     Consensus_State;
      Block  : in     Block_Builder.Block;
      Valid  : out    Boolean
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Check if block is from valid authority
   function Is_Valid_Authority (
      Cons     : Consensus_State;
      Key_Hash : Hash256
   ) return Boolean with
      Global => null,
      Pre    => Cons.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Finality
   ---------------------------------------------------------------------------

   --  Check if a block is finalized
   function Is_Finalized (
      Cons         : Consensus_State;
      Block_Height : U256
   ) return Boolean with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Get latest finalized block height
   function Get_Finalized_Height (
      Cons : Consensus_State
   ) return U256 with
      Global => null,
      Pre    => Cons.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Get current slot
   function Get_Current_Slot (Cons : Consensus_State) return Slot_Number with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Get current epoch
   function Get_Current_Epoch (Cons : Consensus_State) return Natural with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Get slot info
   procedure Get_Slot_Info (
      Cons         : in     Consensus_State;
      Slot         : in     Slot_Number;
      Info         : out    Slot_Info
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Get authority count
   function Get_Authority_Count (Cons : Consensus_State) return Natural with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Get authority by index
   procedure Get_Authority (
      Cons  : in     Consensus_State;
      Index : in     Authority_Index;
      Auth  : out    Authority;
      Found : out    Boolean
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Get consensus statistics
   function Get_Stats (Cons : Consensus_State) return Consensus_Stats with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Check if we are an authority
   function Is_Authority (Cons : Consensus_State) return Boolean with
      Global => null,
      Pre    => Cons.Is_Initialized;

   --  Check if consensus is running
   function Is_Running (Cons : Consensus_State) return Boolean with
      Global => null,
      Pre    => Cons.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Calculate slot from timestamp
   function Slot_From_Timestamp (
      Timestamp : Unsigned_64
   ) return Slot_Number with
      Global => null;

   --  Calculate timestamp from slot
   function Timestamp_From_Slot (
      Slot : Slot_Number
   ) return Unsigned_64 with
      Global => null;

   --  Update chain state after block application
   procedure Update_Chain_State (
      Cons        : in Out Consensus_State;
      Block_Hash  : in     Hash256;
      Block_Slot  : in     Slot_Number;
      Height      : in     U256
   ) with
      Global => null,
      Pre    => Cons.Is_Initialized;

end Consensus;
