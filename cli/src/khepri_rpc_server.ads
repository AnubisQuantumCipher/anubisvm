--  KHEPRI JSON-RPC Server
--
--  Simple JSON-RPC 2.0 server for IoT clients.
--  Listens on a TCP port and dispatches calls to the local executor.
--
--  Methods:
--    vm_call     - Execute contract entry point
--    vm_balance  - Get vault balance
--    vm_tokens   - Get token balance/supply
--    vm_counter  - Get/increment counter
--    vm_status   - Get server status

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Local_Executor;

package Khepri_RPC_Server is

   --  Default port for RPC server
   Default_Port : constant := 8545;

   --  Server status
   type Server_Status is (Stopped, Running, Error);

   --  Start the RPC server on specified port
   procedure Start (Port : Positive := Default_Port);

   --  Stop the RPC server
   procedure Stop;

   --  Check if server is running
   function Is_Running return Boolean;

   --  Get current status
   function Get_Status return Server_Status;

   --  Process a single JSON-RPC request (for testing)
   procedure Process_Request (
      Request_JSON  : String;
      Response_JSON : out Unbounded_String
   );

   --  Run server loop (blocking)
   procedure Run_Server;

private

   --  Parse JSON-RPC request and extract method/params
   procedure Parse_Request (
      JSON       : String;
      ID         : out Natural;
      Method     : out Unbounded_String;
      Params     : out Unbounded_String;
      Success    : out Boolean
   );

   --  Format JSON-RPC response
   function Format_Response (
      ID      : Natural;
      Result  : String;
      Is_Error : Boolean := False;
      Error_Code : Integer := 0;
      Error_Msg  : String := ""
   ) return String;

   --  Dispatch RPC method to local executor
   procedure Dispatch_Method (
      Method  : String;
      Params  : String;
      Result  : out Unbounded_String;
      Success : out Boolean
   );

end Khepri_RPC_Server;
