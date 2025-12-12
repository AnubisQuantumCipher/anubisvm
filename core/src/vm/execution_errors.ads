pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;

--  Execution_Errors: Comprehensive Error Handling for AnubisVM
--
--  This package provides detailed error codes, error categories, and
--  error handling utilities for all VM execution scenarios.
--
--  Error Categories:
--  - Transaction validation errors (signature, nonce, gas)
--  - Execution errors (out of gas, stack overflow, invalid opcode)
--  - State errors (storage access, memory bounds)
--  - Contract errors (revert, panic, require failure)
--  - Cryptographic errors (verification failures)
--
--  References:
--  - JSON-RPC 2.0 Error Codes
--  - EIP-658 (Status field for receipts)

package Execution_Errors with
   SPARK_Mode => On,
   Pure
is

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   --  Detailed error codes for all failure scenarios
   type VM_Error_Code is (
      --  Success (no error)
      Err_None,

      --  Transaction validation errors (1xx)
      Err_Invalid_Signature,       -- 101: ML-DSA-87 signature invalid
      Err_Invalid_Nonce,           -- 102: Nonce mismatch (replay protection)
      Err_Invalid_Chain_ID,        -- 103: Wrong chain ID
      Err_Insufficient_Balance,    -- 104: Sender lacks funds
      Err_Intrinsic_Gas_Too_Low,   -- 105: Gas < intrinsic gas
      Err_Gas_Limit_Exceeded,      -- 106: Gas > block gas limit
      Err_Invalid_Sender,          -- 107: Sender address mismatch
      Err_Tx_Size_Exceeded,        -- 108: Transaction too large
      Err_Invalid_Recipient,       -- 109: Invalid 'to' address

      --  Execution errors (2xx)
      Err_Out_Of_Gas,              -- 201: Gas exhausted during execution
      Err_Stack_Overflow,          -- 202: Call stack depth exceeded
      Err_Stack_Underflow,         -- 203: Stack underflow
      Err_Invalid_Opcode,          -- 204: Unknown/unsupported opcode
      Err_Invalid_Jump,            -- 205: Jump to invalid destination
      Err_Write_Protection,        -- 206: Write in static context
      Err_Call_Depth_Exceeded,     -- 207: Max call depth reached

      --  Memory errors (3xx)
      Err_Memory_Access,           -- 301: Out of bounds memory access
      Err_Memory_Limit,            -- 302: Memory expansion limit
      Err_Invalid_Offset,          -- 303: Invalid memory offset
      Err_Return_Data_OOB,         -- 304: Return data copy out of bounds

      --  Storage errors (4xx)
      Err_Storage_Access,          -- 401: Storage slot access denied
      Err_Storage_Read_Only,       -- 402: Cannot write to storage
      Err_Storage_Key_Invalid,     -- 403: Invalid storage key

      --  Contract errors (5xx)
      Err_Contract_Not_Found,      -- 501: Target contract doesn't exist
      Err_Entry_Point_Not_Found,   -- 502: Entry point not in contract
      Err_Contract_Reverted,       -- 503: Explicit revert() called
      Err_Contract_Panic,          -- 504: Panic (assert failure)
      Err_Require_Failed,          -- 505: require() condition false
      Err_Division_By_Zero,        -- 506: Division/modulo by zero
      Err_Invalid_Args,            -- 507: Invalid function arguments
      Err_Return_Type_Mismatch,    -- 508: Unexpected return type

      --  Cryptographic errors (6xx)
      Err_Crypto_Signature,        -- 601: Signature operation failed
      Err_Crypto_Hash,             -- 602: Hash operation failed
      Err_Crypto_Encryption,       -- 603: Encryption operation failed
      Err_Crypto_Decryption,       -- 604: Decryption operation failed
      Err_Crypto_Key_Invalid,      -- 605: Invalid cryptographic key
      Err_PQ_Verify_Failed,        -- 606: Post-quantum verification failed

      --  Resource errors (7xx)
      Err_Code_Size_Exceeded,      -- 701: Contract code too large
      Err_Init_Code_Exceeded,      -- 702: Init code too large
      Err_Max_Contracts,           -- 703: Contract registry full
      Err_Max_Events,              -- 704: Too many events in tx
      Err_Max_Logs,                -- 705: Log limit exceeded

      --  System errors (9xx)
      Err_Internal,                -- 901: Internal error
      Err_Not_Implemented,         -- 902: Feature not implemented
      Err_State_Corruption         -- 903: State integrity failure
   );

   ---------------------------------------------------------------------------
   --  Error Categories
   ---------------------------------------------------------------------------

   type Error_Category is (
      Cat_None,           -- No error
      Cat_Validation,     -- Transaction validation
      Cat_Execution,      -- Runtime execution
      Cat_Memory,         -- Memory access
      Cat_Storage,        -- Storage access
      Cat_Contract,       -- Contract-specific
      Cat_Crypto,         -- Cryptographic
      Cat_Resource,       -- Resource limits
      Cat_System          -- System/internal
   );

   --  Get category for an error code
   function Get_Category (Code : VM_Error_Code) return Error_Category with
      Post => (if Code = Err_None then Get_Category'Result = Cat_None);

   ---------------------------------------------------------------------------
   --  JSON-RPC Error Mapping
   ---------------------------------------------------------------------------

   --  Standard JSON-RPC error codes
   JSONRPC_Parse_Error       : constant := -32700;
   JSONRPC_Invalid_Request   : constant := -32600;
   JSONRPC_Method_Not_Found  : constant := -32601;
   JSONRPC_Invalid_Params    : constant := -32602;
   JSONRPC_Internal_Error    : constant := -32603;

   --  Server error range: -32000 to -32099
   JSONRPC_Execution_Error   : constant := -32000;
   JSONRPC_Out_Of_Gas        : constant := -32001;
   JSONRPC_Contract_Revert   : constant := -32002;
   JSONRPC_Invalid_Signature : constant := -32003;
   JSONRPC_Nonce_Too_Low     : constant := -32004;
   JSONRPC_Nonce_Too_High    : constant := -32005;
   JSONRPC_Insufficient_Funds: constant := -32006;
   JSONRPC_Contract_Not_Found: constant := -32007;

   --  Map VM error to JSON-RPC error code
   function To_JSONRPC_Code (Code : VM_Error_Code) return Integer with
      Post => To_JSONRPC_Code'Result <= 0 or To_JSONRPC_Code'Result = 0;

   ---------------------------------------------------------------------------
   --  Error Context
   ---------------------------------------------------------------------------

   --  Maximum error message length
   Max_Error_Msg_Len : constant := 256;
   subtype Error_Msg_Index is Natural range 1 .. Max_Error_Msg_Len;
   subtype Error_Msg_String is String (Error_Msg_Index);

   --  Maximum additional data size
   Max_Error_Data_Len : constant := 256;
   subtype Error_Data_Index is Natural range 0 .. Max_Error_Data_Len - 1;
   type Error_Data_Buffer is array (Error_Data_Index) of Byte;

   --  Full error context with message and data
   type Error_Context is record
      Code       : VM_Error_Code;
      Message    : Error_Msg_String;
      Msg_Len    : Natural;
      Data       : Error_Data_Buffer;
      Data_Len   : Natural;
      Gas_Used   : Gas_Amount;
      PC         : Natural;            -- Program counter at error
      Call_Depth : Natural;            -- Call stack depth at error
   end record;

   --  Empty/success context
   Empty_Error_Context : constant Error_Context := (
      Code       => Err_None,
      Message    => (others => ' '),
      Msg_Len    => 0,
      Data       => (others => 0),
      Data_Len   => 0,
      Gas_Used   => 0,
      PC         => 0,
      Call_Depth => 0
   );

   ---------------------------------------------------------------------------
   --  Error Construction
   ---------------------------------------------------------------------------

   --  Create error context from code
   function Make_Error (
      Code     : VM_Error_Code;
      Gas_Used : Gas_Amount
   ) return Error_Context with
      Post => Make_Error'Result.Code = Code;

   --  Create error context with message
   procedure Make_Error_With_Msg (
      Code     : in     VM_Error_Code;
      Msg      : in     String;
      Gas_Used : in     Gas_Amount;
      Context  : out    Error_Context
   ) with
      Pre => Msg'Length <= Max_Error_Msg_Len,
      Post => Context.Code = Code and Context.Msg_Len = Msg'Length;

   --  Create error context with revert data (for contract reverts)
   procedure Make_Revert_Error (
      Data     : in     Error_Data_Buffer;
      Data_Len : in     Natural;
      Gas_Used : in     Gas_Amount;
      Context  : out    Error_Context
   ) with
      Pre => Data_Len <= Max_Error_Data_Len,
      Post => Context.Code = Err_Contract_Reverted and Context.Data_Len = Data_Len;

   ---------------------------------------------------------------------------
   --  Error Checking
   ---------------------------------------------------------------------------

   --  Check if error code indicates failure
   function Is_Error (Code : VM_Error_Code) return Boolean is
      (Code /= Err_None) with Inline;

   --  Check if error is recoverable (state can be rolled back)
   function Is_Recoverable (Code : VM_Error_Code) return Boolean;

   --  Check if error should consume all gas
   function Consumes_All_Gas (Code : VM_Error_Code) return Boolean;

   ---------------------------------------------------------------------------
   --  Standard Error Messages
   ---------------------------------------------------------------------------

   --  Get default message for error code
   function Default_Message (Code : VM_Error_Code) return String with
      Post => Default_Message'Result'Length <= Max_Error_Msg_Len;

end Execution_Errors;
