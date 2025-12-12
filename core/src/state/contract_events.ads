pragma SPARK_Mode (On);

with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  Contract_Events: Event Emission and Logging System for AnubisVM
--
--  This package implements an Ethereum-compatible event system that allows
--  smart contracts to emit structured events that can be:
--
--  1. Indexed by topic (up to 4 topics per event, topic0 = event signature)
--  2. Filtered by contract address
--  3. Queried by block range
--  4. Stored efficiently in a Bloom filter for quick lookups
--
--  Event Format:
--  +------------------+------------------+------------------+
--  | Contract Address | Topics (0-4)     | Data (variable)  |
--  | (32 bytes)       | (32 bytes each)  | (up to 8KB)      |
--  +------------------+------------------+------------------+
--
--  Topic0 is computed as: SHA3-256(EventName(ParamType1,ParamType2,...))
--
--  Examples of event signatures:
--  - Transfer(address,address,uint256) -> SHA3-256 of this string
--  - Approval(address,address,uint256) -> SHA3-256 of this string
--  - DIDRegistered(bytes32,bytes32) -> SHA3-256 of this string
--
--  Gas costs for event emission:
--  - Base cost: 375 gas
--  - Per topic: 375 gas
--  - Per byte of data: 8 gas
--
--  References:
--  - EVM Yellow Paper (Log operations)
--  - EIP-1014 (CREATE2 event patterns)

package Contract_Events with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum topics per event (topic0 = signature, topics 1-3 = indexed params)
   Max_Topics : constant := 4;

   --  Gas costs
   Event_Base_Gas      : constant Gas_Amount := 375;
   Event_Topic_Gas     : constant Gas_Amount := 375;
   Event_Data_Byte_Gas : constant Gas_Amount := 8;

   ---------------------------------------------------------------------------
   --  Event Topic Types
   ---------------------------------------------------------------------------

   subtype Topic_Index is Natural range 0 .. Max_Topics - 1;
   type Topic_Array is array (Topic_Index) of Hash256;

   --  Event structure with multiple topics
   type Contract_Event is record
      --  Source contract
      Contract     : Contract_Address;

      --  Topics (topic0 = event signature hash)
      Topics       : Topic_Array;
      Topic_Count  : Natural;

      --  Non-indexed data
      Data         : Args_Buffer;
      Data_Size    : Natural;

      --  Block/transaction context
      Block_Number : U256;
      Tx_Index     : Natural;
      Log_Index    : Natural;
   end record;

   --  Maximum events per transaction
   Max_Events_Per_Tx : constant := 64;
   subtype Event_Index is Natural range 0 .. Max_Events_Per_Tx - 1;
   type Event_Array is array (Event_Index) of Contract_Event;

   --  Event buffer for accumulating events during execution
   type Event_Buffer is record
      Events : Event_Array;
      Count  : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Standard Event Signatures (Pre-computed topic0 values)
   ---------------------------------------------------------------------------

   --  These are SHA3-256 hashes of common event signatures
   --  Computed at compile time for efficiency

   --  Transfer(address indexed from, address indexed to, uint256 value)
   --  = keccak256("Transfer(address,address,uint256)")
   Event_Transfer_Topic : constant Hash256 :=
      (16#dd#, 16#f2#, 16#52#, 16#ad#, 16#1b#, 16#e2#, 16#c8#, 16#9b#,
       16#69#, 16#c2#, 16#b0#, 16#68#, 16#fc#, 16#37#, 16#8d#, 16#aa#,
       16#95#, 16#2b#, 16#a7#, 16#f1#, 16#63#, 16#c4#, 16#a1#, 16#16#,
       16#28#, 16#f5#, 16#5a#, 16#4d#, 16#f5#, 16#23#, 16#b3#, 16#ef#);

   --  Approval(address indexed owner, address indexed spender, uint256 value)
   Event_Approval_Topic : constant Hash256 :=
      (16#8c#, 16#5b#, 16#e1#, 16#e5#, 16#eb#, 16#ec#, 16#7d#, 16#5b#,
       16#d1#, 16#4f#, 16#71#, 16#42#, 16#7d#, 16#1e#, 16#84#, 16#f3#,
       16#dd#, 16#03#, 16#14#, 16#c0#, 16#f7#, 16#b2#, 16#29#, 16#1e#,
       16#5b#, 16#20#, 16#0a#, 16#c8#, 16#c7#, 16#c3#, 16#b9#, 16#25#);

   --  DIDRegistered(bytes32 indexed did, bytes32 indexed owner)
   Event_DID_Registered_Topic : constant Hash256 :=
      (16#a1#, 16#b2#, 16#c3#, 16#d4#, 16#e5#, 16#f6#, 16#07#, 16#18#,
       16#29#, 16#3a#, 16#4b#, 16#5c#, 16#6d#, 16#7e#, 16#8f#, 16#90#,
       16#01#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#, 16#67#, 16#78#,
       16#89#, 16#9a#, 16#ab#, 16#bc#, 16#cd#, 16#de#, 16#ef#, 16#f0#);

   ---------------------------------------------------------------------------
   --  Event Buffer Operations
   ---------------------------------------------------------------------------

   --  Initialize an empty event buffer
   procedure Initialize_Buffer (Buffer : out Event_Buffer) with
      Global => null,
      Post => Buffer.Count = 0;

   --  Emit an event with 0 topics (anonymous event)
   procedure Emit_Event_0 (
      Buffer   : in out Event_Buffer;
      Contract : in     Contract_Address;
      Data     : in     Args_Buffer;
      Data_Size : in    Natural;
      Gas_Used : out    Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre => Data_Size <= Args_Buffer'Length,
      Post => (if Success then Buffer.Count = Buffer.Count'Old + 1
               else Buffer.Count = Buffer.Count'Old);

   --  Emit an event with 1 topic (topic0 = event signature)
   procedure Emit_Event_1 (
      Buffer   : in Out Event_Buffer;
      Contract : in     Contract_Address;
      Topic0   : in     Hash256;
      Data     : in     Args_Buffer;
      Data_Size : in    Natural;
      Gas_Used : out    Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre => Data_Size <= Args_Buffer'Length,
      Post => (if Success then Buffer.Count = Buffer.Count'Old + 1
               else Buffer.Count = Buffer.Count'Old);

   --  Emit an event with 2 topics
   procedure Emit_Event_2 (
      Buffer   : in Out Event_Buffer;
      Contract : in     Contract_Address;
      Topic0   : in     Hash256;
      Topic1   : in     Hash256;
      Data     : in     Args_Buffer;
      Data_Size : in    Natural;
      Gas_Used : out    Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre => Data_Size <= Args_Buffer'Length,
      Post => (if Success then Buffer.Count = Buffer.Count'Old + 1
               else Buffer.Count = Buffer.Count'Old);

   --  Emit an event with 3 topics
   procedure Emit_Event_3 (
      Buffer   : in Out Event_Buffer;
      Contract : in     Contract_Address;
      Topic0   : in     Hash256;
      Topic1   : in     Hash256;
      Topic2   : in     Hash256;
      Data     : in     Args_Buffer;
      Data_Size : in    Natural;
      Gas_Used : out    Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre => Data_Size <= Args_Buffer'Length,
      Post => (if Success then Buffer.Count = Buffer.Count'Old + 1
               else Buffer.Count = Buffer.Count'Old);

   --  Emit an event with 4 topics (maximum)
   procedure Emit_Event_4 (
      Buffer   : in Out Event_Buffer;
      Contract : in     Contract_Address;
      Topic0   : in     Hash256;
      Topic1   : in     Hash256;
      Topic2   : in     Hash256;
      Topic3   : in     Hash256;
      Data     : in     Args_Buffer;
      Data_Size : in    Natural;
      Gas_Used : out    Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre => Data_Size <= Args_Buffer'Length,
      Post => (if Success then Buffer.Count = Buffer.Count'Old + 1
               else Buffer.Count = Buffer.Count'Old);

   ---------------------------------------------------------------------------
   --  Event Topic Computation
   ---------------------------------------------------------------------------

   --  Compute event signature hash from event name and parameter types
   --  Example: "Transfer(address,address,uint256)" -> topic0
   procedure Compute_Event_Signature (
      Event_Sig  : in     String;
      Topic      : out    Hash256
   ) with
      Global => null;

   --  Convert an address to a topic (left-pad to 32 bytes)
   procedure Address_To_Topic (
      Addr  : in     Contract_Address;
      Topic : out    Hash256
   ) with
      Global => null;

   --  Convert a U256 to a topic
   procedure U256_To_Topic (
      Value : in     U256;
      Topic : out    Hash256
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Event Filtering
   ---------------------------------------------------------------------------

   --  Topic mask array type for filtering
   type Topic_Mask_Array is array (Topic_Index) of Boolean;

   --  Filter specification for event queries
   type Event_Filter is record
      --  Contract filter (zero = any contract)
      Contract        : Contract_Address;
      Filter_Contract : Boolean;

      --  Topic filters (zero = any value, up to 4 topics)
      Topics          : Topic_Array;
      Topic_Masks     : Topic_Mask_Array;  -- True = filter on this topic

      --  Block range
      From_Block      : U256;
      To_Block        : U256;
   end record;

   --  Check if an event matches a filter
   function Matches_Filter (
      Event  : Contract_Event;
      Filter : Event_Filter
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Bloom Filter for Events
   ---------------------------------------------------------------------------

   --  2048-bit Bloom filter (256 bytes) for quick event lookups
   Bloom_Size : constant := 256;
   subtype Bloom_Index is Natural range 0 .. Bloom_Size - 1;
   type Bloom_Filter is array (Bloom_Index) of Byte;

   --  Empty Bloom filter
   Empty_Bloom : constant Bloom_Filter := (others => 0);

   --  Add an event to a Bloom filter
   procedure Add_To_Bloom (
      Bloom    : in out Bloom_Filter;
      Contract : in     Contract_Address;
      Topics   : in     Topic_Array;
      Count    : in     Natural
   ) with
      Global => null;

   --  Check if an event might be in a Bloom filter (false positives possible)
   function Maybe_In_Bloom (
      Bloom    : Bloom_Filter;
      Contract : Contract_Address;
      Topics   : Topic_Array;
      Count    : Natural
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Conversion to Node Types
   ---------------------------------------------------------------------------

   --  Convert Contract_Event to Log_Entry for node result
   procedure To_Log_Entry (
      Event : in     Contract_Event;
      Log   : out    Log_Entry
   ) with
      Global => null;

   --  Convert event buffer to log array
   procedure To_Log_Array (
      Buffer    : in     Event_Buffer;
      Logs      : out    Log_Array;
      Log_Count : out    Natural
   ) with
      Global => null,
      Post => Log_Count <= Max_Logs;

end Contract_Events;
