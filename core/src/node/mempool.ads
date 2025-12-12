-------------------------------------------------------------------------------
--  Mempool: Transaction Pool for AnubisVM
--
--  This package implements a transaction mempool that holds pending transactions
--  waiting to be included in a block. Key features:
--
--  - Priority queue based on gas price and nonce
--  - Nonce tracking per sender for sequential transaction ordering
--  - Eviction policy for when pool is full (lowest gas price first)
--  - Duplicate detection by transaction hash
--  - Gas limit enforcement (total pending gas cannot exceed block limit)
--
--  Transaction Flow:
--  +-------------+     +---------------+     +---------------+     +--------+
--  | RPC Submit  | --> | Validate TX   | --> | Add to Pool   | --> | Block  |
--  | (signed)    |     | (sig, nonce)  |     | (by gas price)|     | Builder|
--  +-------------+     +---------------+     +---------------+     +--------+
--
--  Configuration:
--  - Max 4096 pending transactions (matches SCARAB batch limit)
--  - Max 100M gas total pending (matches block gas limit)
--  - Transactions expire after 1 hour without inclusion
--
--  Security:
--  - All transactions require ML-DSA-87 signatures
--  - Nonce prevents replay attacks
--  - Gas price prevents spam (minimum gas price enforced)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Node_Types; use Anubis_Node_Types;
with Quantum_Transaction_Auth; use Quantum_Transaction_Auth;
with Anubis_MLDSA_Types;

package Mempool with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum pending transactions in pool
   Max_Pending_TX : constant := 4096;

   --  Maximum total gas for all pending transactions
   Max_Pending_Gas : constant Gas_Amount := 100_000_000;  -- 100M gas

   --  Minimum gas price (in wei-equivalent units)
   Min_Gas_Price : constant := 1;

   --  Maximum gas price (prevent overflow)
   Max_Gas_Price : constant := 1_000_000_000_000;  -- 1T

   --  Transaction expiration time (in seconds)
   TX_Expiration_Seconds : constant := 3600;  -- 1 hour

   --  Maximum transactions per sender
   Max_TX_Per_Sender : constant := 64;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype TX_Pool_Index is Natural range 0 .. Max_Pending_TX - 1;

   --  Gas price type (for priority ordering)
   subtype Gas_Price is Unsigned_64 range Min_Gas_Price .. Max_Gas_Price;

   --  Mempool entry wrapping a signed transaction
   type Mempool_Entry is record
      Is_Valid      : Boolean;
      TX            : Signed_Transaction;
      Public_Key    : Anubis_MLDSA_Types.Public_Key;
      TX_Hash       : Hash256;
      Gas_Price_Val : Gas_Price;
      Received_Time : Unsigned_64;  -- Unix timestamp
      Priority      : Natural;      -- Higher = more priority
   end record;

   --  Pool storage
   type TX_Pool_Storage is array (TX_Pool_Index) of Mempool_Entry;

   --  Per-sender nonce tracking
   Max_Tracked_Senders : constant := 1024;
   subtype Sender_Index is Natural range 0 .. Max_Tracked_Senders - 1;

   type Sender_Nonce_Entry is record
      Is_Valid      : Boolean;
      Sender        : Contract_Address;
      Pending_Nonce : U256;   -- Next expected nonce
      TX_Count      : Natural;
   end record;

   type Sender_Nonce_Table is array (Sender_Index) of Sender_Nonce_Entry;

   --  Mempool statistics
   type Mempool_Stats is record
      TX_Count      : Natural;
      Total_Gas     : Gas_Amount;
      Senders       : Natural;
      Evictions     : Natural;
      Expired       : Natural;
      Duplicates    : Natural;
   end record;

   --  Mempool result codes
   type Mempool_Result is (
      Mempool_OK,
      Mempool_Full,
      Mempool_Duplicate,
      Mempool_Invalid_Signature,
      Mempool_Invalid_Nonce,
      Mempool_Gas_Too_Low,
      Mempool_Gas_Price_Too_Low,
      Mempool_Sender_Limit,
      Mempool_Expired,
      Mempool_Not_Found
   );

   --  Mempool state
   type Mempool_State is record
      Pool          : TX_Pool_Storage;
      Senders       : Sender_Nonce_Table;
      TX_Count      : Natural;
      Total_Gas     : Gas_Amount;
      Sender_Count  : Natural;
      Stats         : Mempool_Stats;
      Is_Initialized : Boolean;
      Current_Time  : Unsigned_64;  -- Updated externally
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize empty mempool
   procedure Initialize (Pool : out Mempool_State) with
      Global => null,
      Post   => Pool.Is_Initialized and Pool.TX_Count = 0;

   --  Reset mempool (clear all pending transactions)
   procedure Reset (Pool : in Out Mempool_State) with
      Global => null,
      Pre    => Pool.Is_Initialized,
      Post   => Pool.TX_Count = 0;

   ---------------------------------------------------------------------------
   --  Transaction Submission
   ---------------------------------------------------------------------------

   --  Submit a signed transaction to the mempool
   --  Validates signature and nonce before adding
   procedure Submit_TX (
      Pool          : in Out Mempool_State;
      TX            : in     Signed_Transaction;
      Public_Key    : in     Anubis_MLDSA_Types.Public_Key;
      Gas_Price_Val : in     Gas_Price;
      Timestamp     : in     Unsigned_64;
      Expected_Nonce : in    U256;
      Chain_ID      : in     U256;
      Result        : out    Mempool_Result;
      TX_Hash       : out    Hash256
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Submit without signature verification (for testing)
   procedure Submit_TX_Unchecked (
      Pool          : in Out Mempool_State;
      TX            : in     Signed_Transaction;
      Public_Key    : in     Anubis_MLDSA_Types.Public_Key;
      Gas_Price_Val : in     Gas_Price;
      Timestamp     : in     Unsigned_64;
      Result        : out    Mempool_Result;
      TX_Hash       : out    Hash256
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Transaction Retrieval
   ---------------------------------------------------------------------------

   --  Get highest priority transaction for block building
   procedure Get_Best_TX (
      Pool    : in     Mempool_State;
      Index   : out    TX_Pool_Index;
      Found   : out    Boolean
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Get transaction by hash
   procedure Get_TX_By_Hash (
      Pool    : in     Mempool_State;
      TX_Hash : in     Hash256;
      Index   : out    TX_Pool_Index;
      Found   : out    Boolean
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Get all transactions for a sender (sorted by nonce)
   procedure Get_TX_For_Sender (
      Pool         : in     Mempool_State;
      Sender       : in     Contract_Address;
      Indices      : out    TX_Pool_Storage;
      Count        : out    Natural
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Get up to N highest priority transactions for block building
   procedure Get_Best_N_TX (
      Pool    : in     Mempool_State;
      N       : in     Natural;
      Indices : out    TX_Pool_Storage;
      Count   : out    Natural
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized and N <= Max_Pending_TX;

   ---------------------------------------------------------------------------
   --  Transaction Removal
   ---------------------------------------------------------------------------

   --  Remove transaction after inclusion in a block
   procedure Remove_TX (
      Pool    : in Out Mempool_State;
      TX_Hash : in     Hash256;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Remove transaction by index
   procedure Remove_TX_By_Index (
      Pool    : in Out Mempool_State;
      Index   : in     TX_Pool_Index;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized and Index < Max_Pending_TX;

   --  Remove all transactions from a sender
   procedure Remove_Sender_TX (
      Pool    : in Out Mempool_State;
      Sender  : in     Contract_Address;
      Removed : out    Natural
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Maintenance Operations
   ---------------------------------------------------------------------------

   --  Expire old transactions
   procedure Expire_Old_TX (
      Pool         : in Out Mempool_State;
      Current_Time : in     Unsigned_64;
      Expired      : out    Natural
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Evict lowest priority transactions when pool is full
   procedure Evict_Lowest_Priority (
      Pool    : in Out Mempool_State;
      Count   : in     Natural;
      Evicted : out    Natural
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Update sender nonce after block inclusion
   procedure Update_Sender_Nonce (
      Pool       : in Out Mempool_State;
      Sender     : in     Contract_Address;
      New_Nonce  : in     U256
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Check if transaction exists by hash
   function TX_Exists (
      Pool    : Mempool_State;
      TX_Hash : Hash256
   ) return Boolean with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Get pending nonce for sender
   function Get_Pending_Nonce (
      Pool   : Mempool_State;
      Sender : Contract_Address
   ) return U256 with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Get pool transaction count
   function Get_TX_Count (Pool : Mempool_State) return Natural with
      Global => null,
      Pre    => Pool.Is_Initialized,
      Post   => Get_TX_Count'Result <= Max_Pending_TX;

   --  Get total pending gas
   function Get_Total_Gas (Pool : Mempool_State) return Gas_Amount with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Get mempool statistics
   function Get_Stats (Pool : Mempool_State) return Mempool_Stats with
      Global => null,
      Pre    => Pool.Is_Initialized;

   --  Check if pool is full
   function Is_Full (Pool : Mempool_State) return Boolean with
      Global => null,
      Pre    => Pool.Is_Initialized,
      Post   => Is_Full'Result = (Pool.TX_Count >= Max_Pending_TX);

   --  Check if pool can accept more gas
   function Has_Gas_Capacity (
      Pool      : Mempool_State;
      Gas_Limit : Gas_Amount
   ) return Boolean with
      Global => null,
      Pre    => Pool.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Priority Calculation
   ---------------------------------------------------------------------------

   --  Calculate transaction priority (higher = more priority)
   --  Based on: gas_price * urgency_factor
   function Calculate_Priority (
      Gas_Price_Val : Gas_Price;
      Received_Time : Unsigned_64;
      Current_Time  : Unsigned_64
   ) return Natural with
      Global => null;

   --  Compare two transactions for ordering
   --  Returns True if A should come before B
   function Compare_Priority (
      A, B : Mempool_Entry
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Validation
   ---------------------------------------------------------------------------

   --  Validate transaction before adding to pool
   procedure Validate_TX (
      Pool    : in     Mempool_State;
      TX      : in     Signed_Transaction;
      Gas_Val : in     Gas_Price;
      Result  : out    Mempool_Result
   ) with
      Global => null,
      Pre    => Pool.Is_Initialized;

end Mempool;
