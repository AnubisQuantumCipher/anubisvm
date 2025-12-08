pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;

--  KHEPRI Types: Contract SDK Core Type Definitions
--
--  This package provides the foundational types for KHEPRI smart contract
--  development. Contracts use these types to interact with the blockchain.
--
--  Key Components:
--  - U256: 256-bit unsigned integer (re-exported from Aegis_U256)
--  - Address: Contract/account address type
--  - Storage_Map: Key-value storage abstraction
--  - Balance: Token balance management
--  - Result: Contract execution result type
--
--  All types are SPARK-compatible and formally verified.
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 3: Contract Specification
--  - EVM Yellow Paper (type semantics)

package Khepri_Types with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Re-exported Core Types
   ---------------------------------------------------------------------------

   --  256-bit unsigned integer (from Aegis_U256)
   subtype Uint256 is U256;

   --  Common U256 constants (inline definitions for SPARK compatibility)
   Zero : constant Uint256 := (Limbs => (0, 0, 0, 0));
   One  : constant Uint256 := (Limbs => (1, 0, 0, 0));
   Max  : constant Uint256 := (Limbs => (Word64'Last, Word64'Last,
                                          Word64'Last, Word64'Last));

   --  Contract address (32 bytes, derived from ML-DSA-87 public key)
   subtype Address is Contract_Address;

   --  Zero address constant
   Null_Address : constant Address := (others => 0);

   --  Hash types
   subtype Bytes32 is Hash256;
   subtype Bytes64 is Hash512;

   Bytes32_Zero : constant Bytes32 := (others => 0);

   ---------------------------------------------------------------------------
   --  Integer Conversion Helpers
   ---------------------------------------------------------------------------

   --  Convert Word64 to Uint256
   function To_U256 (V : Word64) return Uint256
      renames From_Word64;

   --  Convert Uint256 to Word64 (truncates)
   function To_U64 (V : Uint256) return Word64
      renames To_Word64;

   --  From natural (up to 64 bits)
   function From_Natural (N : Natural) return Uint256 with
      Global => null,
      Post   => From_Natural'Result = From_Word64 (Word64 (N));

   ---------------------------------------------------------------------------
   --  Arithmetic Operations (convenience wrappers)
   ---------------------------------------------------------------------------

   --  Addition (modular)
   function "+" (Left, Right : Uint256) return Uint256 with
      Global => null;

   --  Subtraction (modular)
   function "-" (Left, Right : Uint256) return Uint256 with
      Global => null;

   --  Multiplication (modular)
   function "*" (Left, Right : Uint256) return Uint256 with
      Global => null;

   --  Division (pre: divisor /= 0)
   function "/" (Left, Right : Uint256) return Uint256 with
      Global => null,
      Pre    => not Is_Zero (Right);

   --  Modulo (pre: divisor /= 0)
   function "mod" (Left, Right : Uint256) return Uint256 with
      Global => null,
      Pre    => not Is_Zero (Right);

   ---------------------------------------------------------------------------
   --  Comparison Operations (convenience wrappers)
   ---------------------------------------------------------------------------

   function "=" (Left, Right : Uint256) return Boolean
      renames Equal;

   function "<" (Left, Right : Uint256) return Boolean
      renames Less_Than;

   function "<=" (Left, Right : Uint256) return Boolean
      renames Less_Than_Or_Equal;

   function ">" (Left, Right : Uint256) return Boolean
      renames Greater_Than;

   function ">=" (Left, Right : Uint256) return Boolean
      renames Greater_Than_Or_Equal;

   ---------------------------------------------------------------------------
   --  Bitwise Operations (convenience wrappers)
   ---------------------------------------------------------------------------

   function "and" (Left, Right : Uint256) return Uint256
      renames Bit_And;

   function "or" (Left, Right : Uint256) return Uint256
      renames Bit_Or;

   function "xor" (Left, Right : Uint256) return Uint256
      renames Bit_Xor;

   function "not" (Value : Uint256) return Uint256
      renames Bit_Not;

   ---------------------------------------------------------------------------
   --  Storage Map Types
   ---------------------------------------------------------------------------

   --  Maximum entries per storage map instance
   Max_Map_Entries : constant := 256;

   --  Map entry (key-value pair)
   type Map_Entry is record
      Key   : Uint256;
      Value : Uint256;
      Valid : Boolean;
   end record;

   --  Map entry index
   subtype Map_Index is Natural range 0 .. Max_Map_Entries - 1;

   --  Storage map (fixed-size hash map)
   type Storage_Map is array (Map_Index) of Map_Entry;

   --  Empty map constant
   Empty_Map : constant Storage_Map := (others => (
      Key   => (Limbs => (0, 0, 0, 0)),
      Value => (Limbs => (0, 0, 0, 0)),
      Valid => False
   ));

   ---------------------------------------------------------------------------
   --  Address Map Types (for balances, allowances, etc.)
   ---------------------------------------------------------------------------

   --  Address-to-U256 map entry
   type Address_Entry is record
      Addr  : Address;
      Value : Uint256;
      Valid : Boolean;
   end record;

   --  Address map
   type Address_Map is array (Map_Index) of Address_Entry;

   --  Empty address map
   Empty_Address_Map : constant Address_Map := (others => (
      Addr  => (others => 0),
      Value => (Limbs => (0, 0, 0, 0)),
      Valid => False
   ));

   ---------------------------------------------------------------------------
   --  Balance and Token Types
   ---------------------------------------------------------------------------

   --  Token balance (alias for Uint256)
   subtype Balance is Uint256;

   --  Wei (smallest unit)
   subtype Wei is Uint256;

   --  Token ID for NFTs
   subtype Token_ID is Uint256;

   ---------------------------------------------------------------------------
   --  Contract Result Types
   ---------------------------------------------------------------------------

   --  Contract execution error codes
   type Error_Code is (
      No_Error,              -- Success
      Insufficient_Balance,  -- Not enough tokens
      Unauthorized,          -- Caller not authorized
      Invalid_Input,         -- Bad parameters
      Overflow,              -- Arithmetic overflow
      Underflow,             -- Arithmetic underflow
      Not_Found,             -- Key/entry not found
      Already_Exists,        -- Entry already exists
      Out_Of_Gas,            -- Gas exhausted
      Contract_Error         -- Generic error
   );

   --  Result type for operations that can fail
   type Result (Success : Boolean := False) is record
      case Success is
         when True =>
            Value : Uint256;
         when False =>
            Error : Error_Code;
      end case;
   end record;

   --  Convenience constructors
   function Ok (Value : Uint256) return Result with
      Global => null,
      Post   => Ok'Result.Success and Ok'Result.Value = Value;

   function Err (Code : Error_Code) return Result with
      Global => null,
      Pre    => Code /= No_Error,
      Post   => not Err'Result.Success and Err'Result.Error = Code;

   ---------------------------------------------------------------------------
   --  Boolean Result Type
   ---------------------------------------------------------------------------

   type Bool_Result (Success : Boolean := False) is record
      case Success is
         when True =>
            Value : Boolean;
         when False =>
            Error : Error_Code;
      end case;
   end record;

   function Ok_Bool (Value : Boolean) return Bool_Result with
      Global => null,
      Post   => Ok_Bool'Result.Success and Ok_Bool'Result.Value = Value;

   ---------------------------------------------------------------------------
   --  Event Types
   ---------------------------------------------------------------------------

   --  Maximum topics per event
   Max_Event_Topics : constant := 4;

   --  Event topic index
   subtype Topic_Index_T is Natural range 0 .. Max_Event_Topics - 1;

   --  Event topics array
   type Event_Topics is array (Topic_Index_T) of Bytes32;

   --  Maximum event data size
   Max_Event_Data : constant := 256;

   --  Event data type
   subtype Event_Data_Index is Natural range 0 .. Max_Event_Data - 1;
   type Event_Data is array (Event_Data_Index) of Byte;

   --  Event descriptor
   type Event is record
      Topics      : Event_Topics;
      Topic_Count : Natural;
      Data        : Event_Data;
      Data_Length : Natural;
   end record;

   --  Empty event
   Empty_Event : constant Event := (
      Topics      => (others => (others => 0)),
      Topic_Count => 0,
      Data        => (others => 0),
      Data_Length => 0
   );

   ---------------------------------------------------------------------------
   --  Call Context Types
   ---------------------------------------------------------------------------

   --  Message sender context
   type Msg_Context is record
      Sender : Address;      -- Transaction origin
      Value  : Wei;          -- Value sent with call
      Data   : Bytes32;      -- Hash of call data
   end record;

   --  Block context
   type Block_Context is record
      Number     : Uint256;    -- Block number
      Timestamp  : Uint256;    -- Unix timestamp
      Coinbase   : Address;    -- Block producer
      Gas_Limit  : Uint256;    -- Block gas limit
      Base_Fee   : Uint256;    -- Base fee per gas
      Chain_ID   : Uint256;    -- Chain identifier
   end record;

   --  Transaction context
   type Tx_Context is record
      Origin    : Address;    -- Original sender
      Gas_Price : Uint256;    -- Gas price in wei
      Gas_Limit : Uint256;    -- Gas limit for tx
   end record;

   ---------------------------------------------------------------------------
   --  Array Types for Common Patterns
   ---------------------------------------------------------------------------

   --  Fixed-size byte arrays
   type Bytes4 is array (0 .. 3) of Byte;
   type Bytes8 is array (0 .. 7) of Byte;
   type Bytes20 is array (0 .. 19) of Byte;

   --  Function selector (4 bytes)
   subtype Selector is Bytes4;

   ---------------------------------------------------------------------------
   --  String Types (Limited)
   ---------------------------------------------------------------------------

   --  Maximum string length for contract names/symbols
   Max_String_Length : constant := 32;

   subtype String_Index is Natural range 0 .. Max_String_Length - 1;
   type Contract_String is array (String_Index) of Character;

   --  String with length
   type Bounded_String is record
      Data   : Contract_String;
      Length : Natural;
   end record;

   Empty_String : constant Bounded_String := (
      Data   => (others => ASCII.NUL),
      Length => 0
   );

end Khepri_Types;
