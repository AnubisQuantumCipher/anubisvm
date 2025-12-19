pragma SPARK_Mode (Off);  -- RPC involves I/O and external communication

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;
with Khepri_Transaction; use Khepri_Transaction;
with Khepri_Contract; use Khepri_Contract;
with Aegis_VM_Types; use Aegis_VM_Types;

--  KHEPRI RPC: JSON-RPC Client for AnubisVM
--
--  This package provides a JSON-RPC client for interacting with AnubisVM
--  nodes. It implements the standard Ethereum-style JSON-RPC interface
--  with AnubisVM-specific extensions.
--
--  Key Features:
--  - Standard JSON-RPC 2.0 protocol
--  - Transaction submission
--  - State queries
--  - Event log retrieval
--  - Block and chain info
--
--  RPC Methods:
--  - anubis_chainId: Get chain ID
--  - anubis_getBalance: Get account balance
--  - anubis_getTransactionCount: Get nonce
--  - anubis_sendTransaction: Submit transaction
--  - anubis_call: Execute read-only call
--  - anubis_getLogs: Get event logs
--  - anubis_getBlockNumber: Get latest block
--
--  Connection:
--  - HTTP/HTTPS transport
--  - WebSocket support (future)
--  - Batch requests
--
--  References:
--  - JSON-RPC 2.0 Specification
--  - Ethereum JSON-RPC API
--  - AnubisVM RPC Extensions

package Khepri_RPC is

   ---------------------------------------------------------------------------
   --  RPC Configuration
   ---------------------------------------------------------------------------

   --  Maximum URL length
   Max_URL_Length : constant := 256;

   --  Maximum response size
   Max_Response_Size : constant := 65536;  -- 64 KB

   --  RPC timeout (milliseconds)
   Default_Timeout : constant := 30_000;  -- 30 seconds

   ---------------------------------------------------------------------------
   --  RPC Client
   ---------------------------------------------------------------------------

   --  RPC client connection
   type RPC_Client is private;

   --  Connection result
   type RPC_Result is (
      RPC_Success,
      RPC_Connection_Error,
      RPC_Timeout,
      RPC_Invalid_Response,
      RPC_Server_Error,
      RPC_Network_Error
   );

   --  Create new RPC client
   procedure New_Client (
      URL    : in  String;
      Client : out RPC_Client;
      Result : out RPC_Result
   ) with
      Pre => URL'Length > 0 and URL'Length <= Max_URL_Length;

   --  Close RPC client connection
   procedure Close_Client (Client : in out RPC_Client);

   --  Check if client is connected
   function Is_Connected (Client : RPC_Client) return Boolean;

   ---------------------------------------------------------------------------
   --  Chain Information
   ---------------------------------------------------------------------------

   --  Get chain ID
   procedure Get_Chain_ID (
      Client   : in  RPC_Client;
      Chain_ID : out Uint256;
      Result   : out RPC_Result
   );

   --  Get latest block number
   procedure Get_Block_Number (
      Client       : in  RPC_Client;
      Block_Number : out Uint256;
      Result       : out RPC_Result
   );

   --  Get gas price
   procedure Get_Gas_Price (
      Client    : in  RPC_Client;
      Gas_Price : out Uint256;
      Result    : out RPC_Result
   );

   ---------------------------------------------------------------------------
   --  Account Queries
   ---------------------------------------------------------------------------

   --  Get account balance
   procedure Get_Balance (
      Client  : in  RPC_Client;
      Account : in  Address;
      Balance : out Uint256;
      Result  : out RPC_Result
   );

   --  Get account nonce (transaction count)
   procedure Get_Transaction_Count (
      Client : in  RPC_Client;
      Account : in  Address;
      Nonce  : out Uint256;
      Result : out RPC_Result
   );

   --  Get account code hash
   procedure Get_Code_Hash (
      Client    : in  RPC_Client;
      Account   : in  Address;
      Code_Hash : out Bytes32;
      Result    : out RPC_Result
   );

   ---------------------------------------------------------------------------
   --  Transaction Submission
   ---------------------------------------------------------------------------

   --  Send signed transaction
   procedure Send_Transaction (
      Client    : in  RPC_Client;
      Signed_TX : in  Signed_Transaction;
      TX_Hash   : out Bytes32;
      Result    : out RPC_Result
   );

   --  Send raw transaction bytes
   procedure Send_Raw_Transaction (
      Client   : in  RPC_Client;
      Raw_TX   : in  Byte_Array;
      TX_Size  : in  Natural;
      TX_Hash  : out Bytes32;
      Result   : out RPC_Result
   ) with
      Pre => TX_Size <= Raw_TX'Length and TX_Size <= Max_Tx_Size;

   --  Get transaction receipt
   type TX_Receipt is record
      TX_Hash          : Bytes32;
      Block_Number     : Uint256;
      Block_Hash       : Bytes32;
      Transaction_Index : Natural;
      From_Addr        : Address;
      To_Addr          : Address;
      Gas_Used         : Uint256;
      Status           : Boolean;  -- True = success, False = reverted
      Contract_Address : Address;  -- For contract creation
      Logs_Count       : Natural;
      Valid            : Boolean;
   end record;

   Null_Receipt : constant TX_Receipt := (
      TX_Hash          => Bytes32_Zero,
      Block_Number     => Zero,
      Block_Hash       => Bytes32_Zero,
      Transaction_Index => 0,
      From_Addr        => Null_Address,
      To_Addr          => Null_Address,
      Gas_Used         => Zero,
      Status           => False,
      Contract_Address => Null_Address,
      Logs_Count       => 0,
      Valid            => False
   );

   --  Get transaction receipt by hash
   procedure Get_Transaction_Receipt (
      Client  : in  RPC_Client;
      TX_Hash : in  Bytes32;
      Receipt : out TX_Receipt;
      Result  : out RPC_Result
   );

   --  Wait for transaction confirmation
   procedure Wait_For_Transaction (
      Client        : in  RPC_Client;
      TX_Hash       : in  Bytes32;
      Confirmations : in  Natural;
      Receipt       : out TX_Receipt;
      Result        : out RPC_Result
   );

   ---------------------------------------------------------------------------
   --  Contract Calls
   ---------------------------------------------------------------------------

   --  Execute read-only contract call
   procedure Call_Contract (
      Client       : in  RPC_Client;
      Contract_Addr : in  Address;
      Calldata     : in  Byte_Array;
      Data_Size    : in  Natural;
      Call_Result  : out Khepri_Contract.Call_Result;
      Result       : out RPC_Result
   ) with
      Pre => Data_Size <= Calldata'Length;

   --  Estimate gas for call
   procedure Estimate_Gas (
      Client    : in  RPC_Client;
      TX        : in  Transaction;
      Gas_Estimate : out Uint256;
      Result    : out RPC_Result
   );

   ---------------------------------------------------------------------------
   --  Event Logs
   ---------------------------------------------------------------------------

   --  Maximum logs per query
   Max_Logs_Per_Query : constant := 1000;

   --  Log entry
   type Log_Entry is record
      Contract_Addr : Address;
      Topics        : Event_Topics;
      Topic_Count   : Natural;
      Data          : Byte_Array (0 .. Max_Event_Data - 1);
      Data_Size     : Natural;
      Block_Number  : Uint256;
      Block_Hash    : Bytes32;
      TX_Hash       : Bytes32;
      TX_Index      : Natural;
      Log_Index     : Natural;
      Removed       : Boolean;
   end record;

   --  Log query result
   type Log_Array_Index is range 0 .. Max_Logs_Per_Query - 1;
   type Log_Array is array (Log_Array_Index) of Log_Entry;

   type Log_Query_Result is record
      Logs  : Log_Array;
      Count : Natural;
   end record;

   --  Get logs matching filter
   procedure Get_Logs (
      Client      : in  RPC_Client;
      Filter      : in  Event_Filter;
      Log_Result  : out Log_Query_Result;
      Result      : out RPC_Result
   );

   ---------------------------------------------------------------------------
   --  Block Queries
   ---------------------------------------------------------------------------

   --  Block information
   type Block_Info is record
      Number       : Uint256;
      Hash         : Bytes32;
      Parent_Hash  : Bytes32;
      Timestamp    : Uint256;
      Gas_Limit    : Uint256;
      Gas_Used     : Uint256;
      Miner        : Address;
      TX_Count     : Natural;
      Valid        : Boolean;
   end record;

   Null_Block : constant Block_Info := (
      Number      => Zero,
      Hash        => Bytes32_Zero,
      Parent_Hash => Bytes32_Zero,
      Timestamp   => Zero,
      Gas_Limit   => Zero,
      Gas_Used    => Zero,
      Miner       => Null_Address,
      TX_Count    => 0,
      Valid       => False
   );

   --  Get block by number
   procedure Get_Block_By_Number (
      Client       : in  RPC_Client;
      Block_Number : in  Uint256;
      Block        : out Block_Info;
      Result       : out RPC_Result
   );

   --  Get block by hash
   procedure Get_Block_By_Hash (
      Client     : in  RPC_Client;
      Block_Hash : in  Bytes32;
      Block      : out Block_Info;
      Result     : out RPC_Result
   );

   ---------------------------------------------------------------------------
   --  Batch Requests
   ---------------------------------------------------------------------------

   --  Maximum batch requests
   Max_Batch_Requests : constant := 32;

   --  Batch request handle
   type Batch_Request is private;

   --  Create new batch
   function New_Batch return Batch_Request;

   --  Add get_balance to batch
   procedure Batch_Add_Get_Balance (
      Batch   : in out Batch_Request;
      Account : in     Address;
      Success : out    Boolean
   );

   --  Add get_nonce to batch
   procedure Batch_Add_Get_Nonce (
      Batch   : in out Batch_Request;
      Account : in     Address;
      Success : out    Boolean
   );

   --  Execute batch
   procedure Execute_Batch (
      Client : in  RPC_Client;
      Batch  : in  Batch_Request;
      Result : out RPC_Result
   );

   --  Get batch result (by index)
   procedure Get_Batch_Result (
      Batch   : in  Batch_Request;
      Index   : in  Natural;
      Data    : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   );

   ---------------------------------------------------------------------------
   --  Subscription (WebSocket only)
   ---------------------------------------------------------------------------

   --  Subscription types
   type Subscription_Type is (
      Sub_NewHeads,         -- New block headers
      Sub_Logs,             -- Event logs
      Sub_NewPendingTxs     -- Pending transactions
   );

   --  Subscription handle
   type Subscription is private;

   --  Subscribe to events
   procedure Subscribe (
      Client   : in  RPC_Client;
      Sub_Type : in  Subscription_Type;
      Filter   : in  Event_Filter;
      Sub      : out Subscription;
      Result   : out RPC_Result
   );

   --  Unsubscribe
   procedure Unsubscribe (
      Client : in  RPC_Client;
      Sub    : in  Subscription;
      Result : out RPC_Result
   );

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Parse hex string to bytes
   procedure Parse_Hex (
      Hex_Str : in  String;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   );

   --  Encode bytes to hex string
   procedure Encode_Hex (
      Data     : in  Byte_Array;
      Size     : in  Natural;
      Hex_Str  : out String;
      Str_Size : out Natural;
      Success  : out Boolean
   ) with
      Pre => Size <= Data'Length;

   --  Create JSON-RPC request
   procedure Create_JSON_Request (
      Method  : in  String;
      Params  : in  String;
      Request : out String;
      Size    : out Natural;
      Success : out Boolean
   ) with
      Pre => Method'Length > 0 and Method'Length <= 64;

private

   --  RPC client internal state
   type RPC_Client is record
      URL        : String (1 .. Max_URL_Length);
      URL_Length : Natural;
      Connected  : Boolean;
      Request_ID : Natural;
   end record;

   --  Empty client
   Null_Client : constant RPC_Client := (
      URL        => (others => ' '),
      URL_Length => 0,
      Connected  => False,
      Request_ID => 0
   );

   --  Batch request state
   type Batch_Request_Entry is record
      Method     : String (1 .. 64);
      Method_Len : Natural;
      Params     : String (1 .. 512);
      Params_Len : Natural;
      Valid      : Boolean;
   end record;

   type Batch_Entry_Array is array (0 .. Max_Batch_Requests - 1) of Batch_Request_Entry;

   type Batch_Request is record
      Entries : Batch_Entry_Array;
      Count   : Natural;
   end record;

   --  Subscription state
   type Subscription is record
      ID        : Natural;
      Sub_Type  : Subscription_Type;
      Active    : Boolean;
   end record;

   Null_Subscription : constant Subscription := (
      ID       => 0,
      Sub_Type => Sub_NewHeads,
      Active   => False
   );

end Khepri_RPC;
