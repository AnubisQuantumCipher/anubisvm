--  KHEPRI RPC Server Main
--
--  Starts a JSON-RPC server for IoT clients to connect to.
--
--  Usage: khepri-rpc [--port PORT]

with Ada.Command_Line;
with Ada.Text_IO;
with Khepri_RPC_Server;

procedure Khepri_RPC_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;

   Port : Positive := Khepri_RPC_Server.Default_Port;
begin
   Put_Line ("AnubisVM JSON-RPC Server v0.1.0");
   Put_Line ("================================");

   --  Parse arguments
   if Argument_Count >= 2 then
      if Argument (1) = "--port" then
         begin
            Port := Positive'Value (Argument (2));
         exception
            when others =>
               Put_Line ("Invalid port number, using default " &
                  Positive'Image (Khepri_RPC_Server.Default_Port));
         end;
      elsif Argument (1) = "--help" or Argument (1) = "-h" then
         Put_Line ("Usage: khepri-rpc [--port PORT]");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --port PORT    Listen on specified port (default: 8545)");
         Put_Line ("");
         Put_Line ("RPC Methods:");
         Put_Line ("  vm_call      Execute contract: {contract, entry, args}");
         Put_Line ("  vm_balance   Get vault total deposits");
         Put_Line ("  vm_tokens    Get token total supply");
         Put_Line ("  vm_counter   Get counter value");
         Put_Line ("  vm_deposit   Deposit to vault: {amount}");
         Put_Line ("  vm_withdraw  Withdraw from vault: {amount}");
         Put_Line ("  vm_status    Get server status");
         Put_Line ("");
         Put_Line ("Example curl:");
         Put_Line ("  curl -X POST http://localhost:8545 -d \\");
         Put_Line ("    '{""jsonrpc"":""2.0"",""method"":""vm_counter"",""id"":1}'");
         return;
      end if;
   end if;

   --  Start server
   Khepri_RPC_Server.Start (Port);

   if Khepri_RPC_Server.Is_Running then
      Put_Line ("");
      Put_Line ("Listening for JSON-RPC requests...");
      Put_Line ("Press Ctrl+C to stop.");
      Put_Line ("");

      --  Run server loop (blocking)
      Khepri_RPC_Server.Run_Server;
   else
      Put_Line ("Failed to start server");
      Set_Exit_Status (1);
   end if;

exception
   when others =>
      Khepri_RPC_Server.Stop;
      raise;
end Khepri_RPC_Main;
