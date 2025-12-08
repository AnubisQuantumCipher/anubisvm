pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;

--  Anubis Node Types: Core type definitions for the standalone node
--
--  This package defines types for the Anubis TEE node that runs
--  without any Go/Rust contamination - pure Ada/SPARK from entry point
--  to syscall.
--
--  Architecture:
--  - Entry point is Ada main procedure
--  - TCP server via GNAT.Sockets with SPARK contracts at boundaries
--  - JSON-RPC parsing in pure SPARK
--  - All crypto operations formally verified

package Anubis_Node_Types with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Node Configuration
   ---------------------------------------------------------------------------

   --  Maximum port range
   subtype Port_Number is Natural range 1 .. 65535;

   --  Default ports
   Default_RPC_Port     : constant Port_Number := 26659;
   Default_P2P_Port     : constant Port_Number := 26656;
   Default_Metrics_Port : constant Port_Number := 26660;

   --  Maximum path length for config
   Max_Path_Length : constant := 256;
   subtype Path_String is String (1 .. Max_Path_Length);

   --  Node configuration
   type Node_Configuration is record
      --  Network settings
      RPC_Port     : Port_Number;
      P2P_Port     : Port_Number;
      Metrics_Port : Port_Number;

      --  Paths
      Data_Dir     : Path_String;
      Data_Dir_Len : Natural;
      Contract_Dir : Path_String;
      Contract_Dir_Len : Natural;

      --  Execution settings
      Max_Gas_Per_Request : Gas_Amount;
      Block_Time_Ms       : Natural;

      --  Identity (ML-DSA-87 public key hash, first 20 bytes)
      Node_Address : Contract_Address;
   end record;

   --  Default configuration
   Default_Configuration : constant Node_Configuration := (
      RPC_Port             => Default_RPC_Port,
      P2P_Port             => Default_P2P_Port,
      Metrics_Port         => Default_Metrics_Port,
      Data_Dir             => (others => ' '),
      Data_Dir_Len         => 0,
      Contract_Dir         => (others => ' '),
      Contract_Dir_Len     => 0,
      Max_Gas_Per_Request  => Max_Gas_Per_Tx,
      Block_Time_Ms        => 5000,
      Node_Address         => (others => 0)
   );

   ---------------------------------------------------------------------------
   --  RPC Request/Response Types
   ---------------------------------------------------------------------------

   --  JSON-RPC version
   subtype RPC_Version is String (1 .. 3);
   RPC_Version_2_0 : constant RPC_Version := "2.0";

   --  Maximum method name length
   Max_Method_Length : constant := 64;
   subtype Method_Name is String (1 .. Max_Method_Length);

   --  RPC Methods
   type RPC_Method is (
      Method_Unknown,
      --  Execution methods
      Method_Execute,          -- Execute contract call
      Method_Query,            -- Read-only query
      Method_Deploy,           -- Deploy contract

      --  VM Contract methods (primary API)
      Method_VM_Deploy,        -- vm_deployContract: Deploy KHEPRI contract
      Method_VM_Invoke,        -- vm_invoke: Call contract method (state change)
      Method_VM_Call,          -- vm_call: Read-only contract call
      Method_VM_GetState,      -- vm_getState: Raw state query

      --  State methods
      Method_GetBalance,       -- Get account balance
      Method_GetNonce,         -- Get account nonce
      Method_GetCode,          -- Get contract code
      Method_GetStorage,       -- Get storage value

      --  Block methods
      Method_GetBlockNumber,   -- Get latest block
      Method_GetBlockByNumber, -- Get block by number

      --  Proof methods
      Method_GetProof,         -- Get Merkle proof

      --  Node methods
      Method_GetNodeInfo,      -- Get node information
      Method_Health            -- Health check
   );

   --  Maximum request ID length
   Max_Request_ID_Length : constant := 32;
   subtype Request_ID is String (1 .. Max_Request_ID_Length);

   --  Maximum parameter data size (32 KB)
   Max_Params_Size : constant := 32 * 1024;
   subtype Params_Index is Natural range 0 .. Max_Params_Size - 1;
   type Params_Buffer is array (Params_Index) of Byte;

   --  RPC Request
   type RPC_Request is record
      Version      : RPC_Version;
      Method       : RPC_Method;
      Method_Str   : Method_Name;
      Method_Len   : Natural;
      ID           : Request_ID;
      ID_Len       : Natural;
      Params       : Params_Buffer;
      Params_Size  : Natural;
   end record;

   --  Maximum result data size (64 KB)
   Max_Result_Size : constant := 64 * 1024;
   subtype Result_Index is Natural range 0 .. Max_Result_Size - 1;
   type Result_Buffer is array (Result_Index) of Byte;

   --  RPC Error codes (JSON-RPC 2.0)
   type RPC_Error_Code is (
      Error_None,              -- No error
      Error_Parse,             -- -32700: Parse error
      Error_Invalid_Request,   -- -32600: Invalid request
      Error_Method_Not_Found,  -- -32601: Method not found
      Error_Invalid_Params,    -- -32602: Invalid params
      Error_Internal,          -- -32603: Internal error
      Error_Execution_Failed,  -- -32000: Execution failed
      Error_Out_Of_Gas,        -- -32001: Out of gas
      Error_Contract_Revert    -- -32002: Contract reverted
   );

   --  Maximum error message length
   Max_Error_Message_Length : constant := 256;
   subtype Error_Message is String (1 .. Max_Error_Message_Length);

   --  RPC Response
   type RPC_Response is record
      Version      : RPC_Version;
      ID           : Request_ID;
      ID_Len       : Natural;
      Has_Result   : Boolean;
      Result       : Result_Buffer;
      Result_Size  : Natural;
      Error_Code   : RPC_Error_Code;
      Error_Msg    : Error_Message;
      Error_Msg_Len : Natural;
   end record;

   --  Empty response
   Empty_Response : constant RPC_Response := (
      Version      => RPC_Version_2_0,
      ID           => (others => ' '),
      ID_Len       => 0,
      Has_Result   => False,
      Result       => (others => 0),
      Result_Size  => 0,
      Error_Code   => Error_None,
      Error_Msg    => (others => ' '),
      Error_Msg_Len => 0
   );

   ---------------------------------------------------------------------------
   --  Node State Types
   ---------------------------------------------------------------------------

   --  Node status
   type Node_Status is (
      Status_Starting,      -- Node is initializing
      Status_Running,       -- Node is operational
      Status_Syncing,       -- Node is syncing state
      Status_Stopping,      -- Node is shutting down
      Status_Stopped,       -- Node is stopped
      Status_Error          -- Node encountered error
   );

   --  Block state
   type Block_State is record
      Number     : U256;
      Timestamp  : U256;
      State_Root : Hash256;
      Tx_Count   : Natural;
   end record;

   --  Node state
   type Node_State is record
      Status        : Node_Status;
      Latest_Block  : Block_State;
      Pending_Txs   : Natural;
      Connections   : Natural;
   end record;

   --  Initial node state
   Initial_State : constant Node_State := (
      Status       => Status_Starting,
      Latest_Block => (
         Number     => (Limbs => (0, 0, 0, 0)),
         Timestamp  => (Limbs => (0, 0, 0, 0)),
         State_Root => (others => 0),
         Tx_Count   => 0
      ),
      Pending_Txs  => 0,
      Connections  => 0
   );

   ---------------------------------------------------------------------------
   --  Contract Deployment Types
   ---------------------------------------------------------------------------

   --  Maximum contract name length
   Max_Contract_Name_Length : constant := 64;
   subtype Contract_Name is String (1 .. Max_Contract_Name_Length);

   --  Contract certification level
   type Cert_Level is (
      Cert_None,     -- No verification
      Cert_Bronze,   -- Basic type safety
      Cert_Silver,   -- Partial proofs
      Cert_Gold,     -- Full SPARK proofs
      Cert_Platinum  -- With side-channel analysis
   );

   --  Contract manifest (metadata for deployment)
   type Node_Contract_Manifest is record
      Name          : Contract_Name;
      Name_Len      : Natural;
      Version_Major : Natural;
      Version_Minor : Natural;
      Version_Patch : Natural;
      Cert          : Cert_Level;
   end record;

   --  Maximum ELF code size (1 MB) - Node specific
   Node_Max_Code_Size : constant := 1024 * 1024;
   subtype Node_Code_Index is Natural range 0 .. Node_Max_Code_Size - 1;
   type Node_Code_Buffer is array (Node_Code_Index) of Byte;

   --  Deployment result
   type Deploy_Result is record
      Success       : Boolean;
      Contract_ID   : Contract_Address;
      Code_Hash     : Hash256;
      Gas_Used      : Gas_Amount;
      Error_Code    : RPC_Error_Code;
      Error_Msg     : Error_Message;
      Error_Msg_Len : Natural;
   end record;

   --  Empty deploy result
   Empty_Deploy_Result : constant Deploy_Result := (
      Success       => False,
      Contract_ID   => (others => 0),
      Code_Hash     => (others => 0),
      Gas_Used      => 0,
      Error_Code    => Error_None,
      Error_Msg     => (others => ' '),
      Error_Msg_Len => 0
   );

   ---------------------------------------------------------------------------
   --  Contract Invocation Types
   ---------------------------------------------------------------------------

   --  Maximum entry point name length
   Max_Entry_Name_Length : constant := 64;
   subtype Entry_Name is String (1 .. Max_Entry_Name_Length);

   --  Maximum arguments size (8 KB)
   Max_Args_Size : constant := 8 * 1024;
   subtype Args_Index is Natural range 0 .. Max_Args_Size - 1;
   type Args_Buffer is array (Args_Index) of Byte;

   --  Invoke request
   type Invoke_Request is record
      From         : Contract_Address;
      To           : Contract_Address;
      Entry_Point  : Entry_Name;    -- 'Entry' is reserved, use Entry_Point
      Entry_Len    : Natural;
      Args         : Args_Buffer;
      Args_Size    : Natural;
      Gas_Limit    : Gas_Amount;
      Value        : U256;
   end record;

   --  Maximum return data size (32 KB)
   Max_Return_Size : constant := 32 * 1024;
   subtype Return_Index is Natural range 0 .. Max_Return_Size - 1;
   type Return_Buffer is array (Return_Index) of Byte;

   --  Maximum log entries
   Max_Logs : constant := 64;
   subtype Log_Index is Natural range 0 .. Max_Logs - 1;

   --  Log entry (event)
   type Log_Entry is record
      Contract    : Contract_Address;
      Topic_Hash  : Hash256;
      Data        : Args_Buffer;
      Data_Size   : Natural;
   end record;

   type Log_Array is array (Log_Index) of Log_Entry;

   --  Invoke result
   type Invoke_Result is record
      Success      : Boolean;
      Gas_Used     : Gas_Amount;
      Return_Data  : Return_Buffer;
      Return_Size  : Natural;
      Logs         : Log_Array;
      Log_Count    : Natural;
      Error_Code   : RPC_Error_Code;
      Error_Msg    : Error_Message;
      Error_Msg_Len : Natural;
   end record;

   --  Empty invoke result
   Empty_Invoke_Result : constant Invoke_Result := (
      Success      => False,
      Gas_Used     => 0,
      Return_Data  => (others => 0),
      Return_Size  => 0,
      Logs         => (others => (
         Contract   => (others => 0),
         Topic_Hash => (others => 0),
         Data       => (others => 0),
         Data_Size  => 0
      )),
      Log_Count    => 0,
      Error_Code   => Error_None,
      Error_Msg    => (others => ' '),
      Error_Msg_Len => 0
   );

end Anubis_Node_Types;
