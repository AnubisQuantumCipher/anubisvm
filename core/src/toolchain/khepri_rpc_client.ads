pragma SPARK_Mode (Off);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;

--  KHEPRI RPC Client: JSON-RPC 2.0 Network Layer
--
--  This package provides HTTP JSON-RPC 2.0 client functionality for
--  interacting with AnubisVM nodes. SPARK_Mode is Off due to network I/O,
--  but all interfaces are designed for safe integration with SPARK code.
--
--  Supported RPC Methods:
--  - anubis_sendRawTransaction: Submit signed transaction
--  - anubis_getTransactionReceipt: Get transaction receipt
--  - anubis_getCode: Get deployed contract bytecode
--  - anubis_blockNumber: Get current block number
--  - anubis_getBalance: Get account balance
--
--  References:
--  - JSON-RPC 2.0 Specification
--  - Ethereum JSON-RPC API (adapted for AnubisVM)

package Khepri_RPC_Client is

   ---------------------------------------------------------------------------
   --  RPC Connection Types
   ---------------------------------------------------------------------------

   --  RPC endpoint configuration
   type RPC_Endpoint is record
      Host     : Bounded_String;  --  e.g., "localhost" or "mainnet.anubis.io"
      Port     : Natural;          --  e.g., 8545
      Use_TLS  : Boolean;          --  HTTPS vs HTTP
      Timeout  : Natural;          --  Seconds
   end record;

   Default_Endpoint : constant RPC_Endpoint := (
      Host     => Empty_String,
      Port     => 8545,
      Use_TLS  => False,
      Timeout  => 30
   );

   --  Parse endpoint from URL string
   function Parse_Endpoint (
      URL : String
   ) return RPC_Endpoint;

   ---------------------------------------------------------------------------
   --  RPC Error Codes (JSON-RPC 2.0)
   ---------------------------------------------------------------------------

   type RPC_Error_Code is (
      RPC_Success,            --  No error
      RPC_Parse_Error,        --  -32700: Invalid JSON
      RPC_Invalid_Request,    --  -32600: Invalid request object
      RPC_Method_Not_Found,   --  -32601: Method does not exist
      RPC_Invalid_Params,     --  -32602: Invalid method parameters
      RPC_Internal_Error,     --  -32603: Internal JSON-RPC error
      RPC_Server_Error,       --  -32000: Generic server error
      RPC_Tx_Underpriced,     --  -32001: Transaction underpriced
      RPC_Tx_Already_Known,   --  -32002: Already known transaction
      RPC_Tx_Pool_Full,       --  -32003: Transaction pool full
      RPC_Network_Error,      --  Network/connection error
      RPC_Timeout_Error,      --  Request timeout
      RPC_Unknown_Error       --  Unknown error
   );

   --  RPC error information
   type RPC_Error is record
      Code    : RPC_Error_Code;
      Message : Bounded_String;
   end record;

   No_Error : constant RPC_Error := (
      Code    => RPC_Success,
      Message => Empty_String
   );

   ---------------------------------------------------------------------------
   --  Transaction Receipt
   ---------------------------------------------------------------------------

   --  Transaction receipt from blockchain
   type Transaction_Receipt is record
      Transaction_Hash    : Hash256;
      Block_Number        : Word64;
      Block_Hash          : Hash256;
      Contract_Address    : Khepri_Types.Address;  --  Non-zero for contract creation
      Gas_Used            : Word64;
      Effective_Gas_Price : Word64;
      Status              : Boolean;  --  True = success, False = reverted
      Cumulative_Gas_Used : Word64;
      Logs_Count          : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  RPC Methods
   ---------------------------------------------------------------------------

   --  Send raw signed transaction
   --  Method: anubis_sendRawTransaction
   --  Params: ["0x<hex_encoded_tx>"]
   --  Returns: Transaction hash
   procedure Send_Raw_Transaction (
      Endpoint    : in     RPC_Endpoint;
      Signed_Tx   : in     Byte_Array;
      Tx_Hash     : out    Hash256;
      Error       : out    RPC_Error;
      Success     : out    Boolean
   );

   --  Get transaction receipt
   --  Method: anubis_getTransactionReceipt
   --  Params: ["0x<tx_hash>"]
   --  Returns: Receipt object or null
   procedure Get_Transaction_Receipt (
      Endpoint : in     RPC_Endpoint;
      Tx_Hash  : in     Hash256;
      Receipt  : out    Transaction_Receipt;
      Found    : out    Boolean;
      Error    : out    RPC_Error;
      Success  : out    Boolean
   );

   --  Get contract code
   --  Method: anubis_getCode
   --  Params: ["0x<address>", "latest"]
   --  Returns: Bytecode as hex string
   procedure Get_Code (
      Endpoint      : in     RPC_Endpoint;
      Contract_Addr : in     Khepri_Types.Address;
      Code          : out    Byte_Array;
      Code_Size     : out    Natural;
      Error         : out    RPC_Error;
      Success       : out    Boolean
   );

   --  Get current block number
   --  Method: anubis_blockNumber
   --  Returns: Block number as hex
   procedure Get_Block_Number (
      Endpoint     : in     RPC_Endpoint;
      Block_Number : out    Word64;
      Error        : out    RPC_Error;
      Success      : out    Boolean
   );

   --  Get account balance
   --  Method: anubis_getBalance
   --  Params: ["0x<address>", "latest"]
   --  Returns: Balance in wei
   procedure Get_Balance (
      Endpoint : in     RPC_Endpoint;
      Address  : in     Khepri_Types.Address;
      Balance  : out    Uint256;
      Error    : out    RPC_Error;
      Success  : out    Boolean
   );

   ---------------------------------------------------------------------------
   --  Polling Helpers
   ---------------------------------------------------------------------------

   --  Poll for transaction receipt with retry logic
   --  Polls every Poll_Interval_Seconds until receipt found or timeout
   procedure Poll_Transaction_Receipt (
      Endpoint              : in     RPC_Endpoint;
      Tx_Hash               : in     Hash256;
      Max_Attempts          : in     Natural;
      Poll_Interval_Seconds : in     Natural;
      Receipt               : out    Transaction_Receipt;
      Found                 : out    Boolean;
      Error                 : out    RPC_Error;
      Success               : out    Boolean
   );

   --  Wait for N confirmations
   --  Returns when (current_block - receipt.block_number) >= Confirmations
   procedure Wait_Confirmations (
      Endpoint       : in     RPC_Endpoint;
      Tx_Hash        : in     Hash256;
      Confirmations  : in     Natural;
      Max_Wait_Seconds : in   Natural;
      Receipt        : out    Transaction_Receipt;
      Error          : out    RPC_Error;
      Success        : out    Boolean
   );

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Convert byte array to hex string with "0x" prefix
   function To_Hex_String (Data : Byte_Array) return String;

   --  Convert hash to hex string
   function Hash_To_Hex (Hash : Hash256) return String;

   --  Convert address to hex string
   function Address_To_Hex (Addr : Khepri_Types.Address) return String;

   --  Parse hex string (with or without "0x" prefix) to bytes
   procedure Parse_Hex (
      Hex_Str : in     String;
      Data    : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   );

end Khepri_RPC_Client;
