--  KHEPRI JSON-RPC Provider Layer
--
--  Provides JSON-RPC 2.0 interface for blockchain connectivity.
--  Supports method calls, batch requests, and error handling.
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Khepri_Types;    use Khepri_Types;

package Khepri_JSONRPC with
   SPARK_Mode => On,
   Abstract_State => (RPC_State with External => Async_Writers),
   Initializes => RPC_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_Method_Length   : constant := 64;
   Max_Params_Length   : constant := 4096;
   Max_Result_Length   : constant := 8192;
   Max_Error_Length    : constant := 256;
   Max_Batch_Size      : constant := 100;

   ---------------------------------------------------------------------------
   --  JSON-RPC Version
   ---------------------------------------------------------------------------

   JSONRPC_Version : constant String := "2.0";

   ---------------------------------------------------------------------------
   --  Request/Response Types
   ---------------------------------------------------------------------------

   --  Request ID (can be string or integer, we use integer)
   subtype Request_ID is Natural;

   --  Method name
   subtype Method_Length is Natural range 0 .. Max_Method_Length;
   type RPC_Method is record
      Data   : String (1 .. Max_Method_Length);
      Length : Method_Length;
   end record;

   --  JSON Parameters (serialized)
   subtype Params_Length is Natural range 0 .. Max_Params_Length;
   type RPC_Params is record
      Data   : String (1 .. Max_Params_Length);
      Length : Params_Length;
   end record;

   --  JSON Result (serialized)
   subtype Result_Length is Natural range 0 .. Max_Result_Length;
   type RPC_Result is record
      Data   : String (1 .. Max_Result_Length);
      Length : Result_Length;
   end record;

   --  Error message
   subtype Error_Msg_Length is Natural range 0 .. Max_Error_Length;
   type RPC_Error_Msg is record
      Data   : String (1 .. Max_Error_Length);
      Length : Error_Msg_Length;
   end record;

   ---------------------------------------------------------------------------
   --  Error Codes (JSON-RPC 2.0 Standard)
   ---------------------------------------------------------------------------

   type RPC_Error_Code is new Integer range -32768 .. 32767;

   --  Standard JSON-RPC errors
   Parse_Error          : constant RPC_Error_Code := -32700;
   Invalid_Request      : constant RPC_Error_Code := -32600;
   Method_Not_Found     : constant RPC_Error_Code := -32601;
   Invalid_Params       : constant RPC_Error_Code := -32602;
   Internal_Error       : constant RPC_Error_Code := -32603;

   --  Server errors (-32000 to -32099)
   Server_Error_Start   : constant RPC_Error_Code := -32000;
   Server_Error_End     : constant RPC_Error_Code := -32099;

   --  Application errors
   Execution_Error      : constant RPC_Error_Code := 3;
   Invalid_Input        : constant RPC_Error_Code := -32000;
   Resource_Not_Found   : constant RPC_Error_Code := -32001;
   Resource_Unavailable : constant RPC_Error_Code := -32002;
   Transaction_Rejected : constant RPC_Error_Code := -32003;
   Method_Not_Supported : constant RPC_Error_Code := -32004;
   Limit_Exceeded       : constant RPC_Error_Code := -32005;

   ---------------------------------------------------------------------------
   --  RPC Error Type
   ---------------------------------------------------------------------------

   type RPC_Error is record
      Code    : RPC_Error_Code;
      Message : RPC_Error_Msg;
      Has_Data : Boolean;
      Data    : RPC_Result;  --  Optional error data
   end record;

   No_Error : constant RPC_Error := (
      Code     => 0,
      Message  => (Data => (others => ' '), Length => 0),
      Has_Data => False,
      Data     => (Data => (others => ' '), Length => 0)
   );

   ---------------------------------------------------------------------------
   --  Request Type
   ---------------------------------------------------------------------------

   type RPC_Request is record
      ID     : Request_ID;
      Method : RPC_Method;
      Params : RPC_Params;
   end record;

   ---------------------------------------------------------------------------
   --  Response Type
   ---------------------------------------------------------------------------

   type RPC_Response is record
      ID       : Request_ID;
      Success  : Boolean;
      Result   : RPC_Result;
      Error    : RPC_Error;
   end record;

   ---------------------------------------------------------------------------
   --  Provider Configuration
   ---------------------------------------------------------------------------

   subtype URL_Length is Natural range 0 .. 256;
   type Provider_URL is record
      Data   : String (1 .. 256);
      Length : URL_Length;
   end record;

   type Provider_Config is record
      Endpoint     : Provider_URL;
      Timeout_MS   : Natural;
      Retry_Count  : Natural;
      Chain_ID     : Natural;
   end record;

   Default_Config : constant Provider_Config := (
      Endpoint    => (Data => (others => ' '), Length => 0),
      Timeout_MS  => 30000,
      Retry_Count => 3,
      Chain_ID    => 1
   );

   ---------------------------------------------------------------------------
   --  Provider Status
   ---------------------------------------------------------------------------

   type Provider_Status is (
      Status_Disconnected,
      Status_Connecting,
      Status_Connected,
      Status_Error
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Provider_Config;
      Success : out Boolean
   ) with
      Global => (In_Out => RPC_State);

   procedure Shutdown with
      Global => (In_Out => RPC_State);

   ---------------------------------------------------------------------------
   --  Status Functions
   ---------------------------------------------------------------------------

   function Get_Status return Provider_Status with
      Global => RPC_State,
      Volatile_Function;

   function Is_Connected return Boolean with
      Global => RPC_State,
      Volatile_Function;

   function Get_Chain_ID return Natural with
      Global => RPC_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Core RPC Functions
   ---------------------------------------------------------------------------

   --  Execute a single RPC call
   procedure Call (
      Request  : in     RPC_Request;
      Response : out    RPC_Response;
      Success  : out    Boolean
   ) with
      Global => RPC_State;

   --  Execute RPC call with method and params strings
   procedure Call_Method (
      Method   : in     String;
      Params   : in     String;
      Result   : out    RPC_Result;
      Error    : out    RPC_Error;
      Success  : out    Boolean
   ) with
      Global => (In_Out => RPC_State),
      Pre    => Method'Length <= Max_Method_Length
                and Params'Length <= Max_Params_Length;

   ---------------------------------------------------------------------------
   --  Common Ethereum JSON-RPC Methods
   ---------------------------------------------------------------------------

   --  eth_chainId - Get the chain ID
   procedure ETH_Chain_ID (
      Chain_ID : out Natural;
      Success  : out Boolean
   ) with
      Global => (In_Out => RPC_State);

   --  eth_blockNumber - Get latest block number
   procedure ETH_Block_Number (
      Block_Num : out Word64;
      Success   : out Boolean
   ) with
      Global => (In_Out => RPC_State);

   --  eth_getBalance - Get account balance
   procedure ETH_Get_Balance (
      Account : in     Address;
      Block   : in     String;
      Balance : out    U256;
      Success : out    Boolean
   ) with
      Global => (In_Out => RPC_State),
      Pre    => Block'Length <= 32;

   --  eth_getTransactionCount - Get nonce
   procedure ETH_Get_Transaction_Count (
      Account : in     Address;
      Block   : in     String;
      Nonce   : out    Word64;
      Success : out    Boolean
   ) with
      Global => (In_Out => RPC_State),
      Pre    => Block'Length <= 32;

   --  eth_gasPrice - Get current gas price
   procedure ETH_Gas_Price (
      Price   : out U256;
      Success : out Boolean
   ) with
      Global => (In_Out => RPC_State);

   --  eth_estimateGas - Estimate gas for a call
   procedure ETH_Estimate_Gas (
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Data      : in     Byte_Array;
      Gas       : out    Word64;
      Success   : out    Boolean
   ) with
      Global => (In_Out => RPC_State);

   --  eth_sendRawTransaction - Send signed transaction
   procedure ETH_Send_Raw_Transaction (
      Signed_Tx : in     Byte_Array;
      Tx_Hash   : out    Hash256;
      Success   : out    Boolean
   ) with
      Global => (In_Out => RPC_State);

   --  eth_call - Execute a call (no state change)
   procedure ETH_Call (
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Data      : in     Byte_Array;
      Block     : in     String;
      Result    : out    Byte_Array;
      Success   : out    Boolean
   ) with
      Global => (In_Out => RPC_State),
      Pre    => Block'Length <= 32;

   --  eth_getTransactionReceipt - Get transaction receipt
   procedure ETH_Get_Transaction_Receipt (
      Tx_Hash  : in     Hash256;
      Found    : out    Boolean;
      Status   : out    Boolean;
      Gas_Used : out    Word64;
      Success  : out    Boolean
   ) with
      Global => (In_Out => RPC_State);

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Create method from string
   function Make_Method (S : String) return RPC_Method with
      Pre => S'Length <= Max_Method_Length;

   --  Create params from string
   function Make_Params (S : String) return RPC_Params with
      Pre => S'Length <= Max_Params_Length;

   --  Check if response is an error
   function Is_Error (Response : RPC_Response) return Boolean;

   --  Get error message as string
   function Get_Error_Message (Error : RPC_Error) return String with
      Post => Get_Error_Message'Result'Length <= Max_Error_Length;

end Khepri_JSONRPC;
