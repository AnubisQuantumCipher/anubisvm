-------------------------------------------------------------------------------
--  Block_Builder: Block Construction for AnubisVM
--
--  This package implements block building from pending transactions in the
--  mempool. Key features:
--
--  - Transaction selection based on gas price priority
--  - State root computation after transaction execution
--  - Block header construction with Merkle roots
--  - Gas accounting and limit enforcement
--  - Integration with SCARAB batch verification
--
--  Block Structure:
--  +-------------------+
--  | Block Header      |  <- Hash, height, timestamp, roots
--  +-------------------+
--  | Transaction List  |  <- Ordered by priority, up to gas limit
--  +-------------------+
--  | Receipts Root     |  <- Execution results
--  +-------------------+
--
--  Flow:
--  +-------------+     +---------------+     +---------------+     +--------+
--  | Mempool     | --> | Select TX     | --> | Execute TX    | --> | Seal   |
--  | (pending)   |     | (by priority) |     | (update state)|     | Block  |
--  +-------------+     +---------------+     +---------------+     +--------+
--
--  Security:
--  - All transactions have ML-DSA-87 signatures
--  - State roots use SHA3-256 Merkle trees
--  - Gas limits prevent DoS attacks
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Node_Types; use Anubis_Node_Types;
with Quantum_Transaction_Auth; use Quantum_Transaction_Auth;
with Mempool;
with Anubis_MLDSA_Types;
with Node_Contract_Registry;
with Node_Contract_Executor;

package Block_Builder with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum transactions per block
   Max_TX_Per_Block : constant := 1024;

   --  Block gas limit (matches mempool)
   Block_Gas_Limit : constant Gas_Amount := 100_000_000;  -- 100M gas

   --  Block time target (seconds)
   Block_Time_Target : constant := 6;  -- 6 second blocks

   --  Minimum block interval (seconds)
   Min_Block_Interval : constant := 1;

   ---------------------------------------------------------------------------
   --  Block Header Types
   ---------------------------------------------------------------------------

   --  Block header containing metadata and roots
   type Block_Header is record
      --  Block identification
      Number         : U256;
      Parent_Hash    : Hash256;
      Timestamp      : Unsigned_64;

      --  State roots
      State_Root     : Hash256;  -- State trie root after execution
      TX_Root        : Hash256;  -- Transaction Merkle root
      Receipts_Root  : Hash256;  -- Receipt Merkle root

      --  Block producer
      Proposer       : Contract_Address;
      Proposer_Sig   : Anubis_MLDSA_Types.Signature;

      --  Gas accounting
      Gas_Used       : Gas_Amount;
      Gas_Limit      : Gas_Amount;

      --  Extra data (for consensus)
      Extra_Data     : Hash256;  -- Used by consensus layer
   end record;

   ---------------------------------------------------------------------------
   --  Transaction Receipt Types
   ---------------------------------------------------------------------------

   --  Execution result for a single transaction
   type TX_Receipt is record
      Is_Valid      : Boolean;
      TX_Hash       : Hash256;
      Success       : Boolean;
      Gas_Used      : Gas_Amount;
      Return_Data   : Args_Buffer;
      Return_Size   : Natural;
      Error_Code    : Natural;
   end record;

   --  Storage for transaction receipts
   subtype TX_Receipt_Index is Natural range 0 .. Max_TX_Per_Block - 1;
   type TX_Receipt_Storage is array (TX_Receipt_Index) of TX_Receipt;

   ---------------------------------------------------------------------------
   --  Block Body Types
   ---------------------------------------------------------------------------

   --  Transaction reference in block (hash only, full TX in separate storage)
   type Block_TX_Entry is record
      Is_Valid      : Boolean;
      TX_Hash       : Hash256;
      TX            : Signed_Transaction;
      Public_Key    : Anubis_MLDSA_Types.Public_Key;
   end record;

   --  Storage for block transactions
   subtype Block_TX_Index is Natural range 0 .. Max_TX_Per_Block - 1;
   type Block_TX_Storage is array (Block_TX_Index) of Block_TX_Entry;

   --  Complete block structure
   type Block is record
      Header        : Block_Header;
      TX_Count      : Natural;
      Transactions  : Block_TX_Storage;
      Receipts      : TX_Receipt_Storage;
      Is_Valid      : Boolean;
      Is_Finalized  : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Builder State
   ---------------------------------------------------------------------------

   --  Builder result codes
   type Builder_Result is (
      Builder_OK,
      Builder_No_Transactions,
      Builder_Gas_Limit_Exceeded,
      Builder_Invalid_Parent,
      Builder_Execution_Failed,
      Builder_State_Root_Failed,
      Builder_Already_Building
   );

   --  Builder statistics
   type Builder_Stats is record
      Blocks_Built  : Natural;
      TX_Included   : Natural;
      TX_Failed     : Natural;
      Gas_Used      : Gas_Amount;
   end record;

   --  Builder state
   type Builder_State is record
      Is_Building     : Boolean;
      Current_Block   : Block;
      Parent_Hash     : Hash256;
      Parent_Number   : U256;
      Start_Time      : Unsigned_64;
      Stats           : Builder_Stats;
      Is_Initialized  : Boolean;
      --  VM execution state
      Exec            : Node_Contract_Executor.Executor_State;
      Registry        : Node_Contract_Registry.Registry_State;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize block builder
   procedure Initialize (Builder : out Builder_State) with
      Global => null,
      Post   => Builder.Is_Initialized and not Builder.Is_Building;

   ---------------------------------------------------------------------------
   --  Block Building
   ---------------------------------------------------------------------------

   --  Start building a new block
   procedure Start_Block (
      Builder      : in Out Builder_State;
      Parent_Hash  : in     Hash256;
      Parent_Num   : in     U256;
      Timestamp    : in     Unsigned_64;
      Proposer     : in     Contract_Address;
      Result       : out    Builder_Result
   ) with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Add transactions from mempool to block
   procedure Add_Transactions (
      Builder      : in Out Builder_State;
      Pool         : in Out Mempool.Mempool_State;
      Max_TX       : in     Natural;
      Added        : out    Natural;
      Result       : out    Builder_Result
   ) with
      Global => null,
      Pre    => Builder.Is_Initialized and Builder.Is_Building and
                Pool.Is_Initialized;

   --  Execute a single transaction and add to block
   procedure Execute_Transaction (
      Builder      : in Out Builder_State;
      TX           : in     Signed_Transaction;
      Public_Key   : in     Anubis_MLDSA_Types.Public_Key;
      TX_Hash      : in     Hash256;
      Receipt      : out    TX_Receipt;
      Result       : out    Builder_Result
   ) with
      Global => null,
      Pre    => Builder.Is_Initialized and Builder.Is_Building;

   --  Finalize and seal the block
   procedure Seal_Block (
      Builder      : in Out Builder_State;
      State_Root   : in     Hash256;
      Result       : out    Builder_Result;
      Sealed_Block : out    Block
   ) with
      Global => null,
      Pre    => Builder.Is_Initialized and Builder.Is_Building;

   --  Abort current block building
   procedure Abort_Block (
      Builder : in Out Builder_State
   ) with
      Global => null,
      Pre    => Builder.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Root Computation
   ---------------------------------------------------------------------------

   --  Compute transaction Merkle root
   procedure Compute_TX_Root (
      Builder  : in     Builder_State;
      TX_Root  : out    Hash256
   ) with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Compute receipts Merkle root
   procedure Compute_Receipts_Root (
      Builder       : in     Builder_State;
      Receipts_Root : out    Hash256
   ) with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Compute block hash
   procedure Compute_Block_Hash (
      Header     : in     Block_Header;
      Block_Hash : out    Hash256
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Check if builder is currently building
   function Is_Building (Builder : Builder_State) return Boolean with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Get current block transaction count
   function Get_TX_Count (Builder : Builder_State) return Natural with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Get current block gas used
   function Get_Gas_Used (Builder : Builder_State) return Gas_Amount with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Get remaining gas capacity
   function Get_Gas_Remaining (Builder : Builder_State) return Gas_Amount with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Get builder statistics
   function Get_Stats (Builder : Builder_State) return Builder_Stats with
      Global => null,
      Pre    => Builder.Is_Initialized;

   --  Check if block can accept more transactions
   function Can_Add_TX (
      Builder   : Builder_State;
      Gas_Limit : Gas_Amount
   ) return Boolean with
      Global => null,
      Pre    => Builder.Is_Initialized;

end Block_Builder;
