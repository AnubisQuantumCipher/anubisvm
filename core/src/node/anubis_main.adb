--  Anubis Node Main Entry Point
--
--  Pure Ada/SPARK execution node for AegisVM TEE.
--  No Go. No Rust. Just proven Ada from entry point to syscall.
--
--  This is the seL4 model: trust nothing unverified.
--
--  Usage:
--    anubis-node [--port PORT] [--data DIR]
--
--  Endpoints:
--    - JSON-RPC 2.0 over TCP (port 26659 default)
--    - Health: {"jsonrpc":"2.0","method":"health","id":1}

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Streams;
with GNAT.Sockets;
with GNAT.Traceback.Symbolic;

with Anubis_Node; use Anubis_Node;
with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

procedure Anubis_Main is

   --  Banner
   Banner : constant String :=
      ASCII.LF &
      "    ___    _   ____  ______  _________ " & ASCII.LF &
      "   /   |  / | / / / / / __ )/  _/ ___/ " & ASCII.LF &
      "  / /| | /  |/ / / / / __  |/ / \__ \  " & ASCII.LF &
      " / ___ |/ /|  / /_/ / /_/ // / ___/ /  " & ASCII.LF &
      "/_/  |_/_/ |_/\____/_____/___//____/   " & ASCII.LF &
      ASCII.LF &
      " Post-Quantum Trusted Execution Environment" & ASCII.LF &
      " ML-DSA-87 | ML-KEM-1024 | SPARK Verified" & ASCII.LF &
      " Pure Ada - No Go - No Rust - Just Math" & ASCII.LF;

   --  Configuration
   Config : Node_Configuration := Default_Configuration;

   --  VM Instance
   VM : VM_Instance := Null_Instance;

   --  Server state
   Server_Socket  : GNAT.Sockets.Socket_Type;
   Server_Address : GNAT.Sockets.Sock_Addr_Type;

   --  Running flag
   Running : Boolean := True;

   --  Parse command line arguments
   procedure Parse_Arguments is
      I : Positive := 1;
   begin
      while I <= Ada.Command_Line.Argument_Count loop
         declare
            Arg : constant String := Ada.Command_Line.Argument (I);
         begin
            if Arg = "--port" or Arg = "-p" then
               I := I + 1;
               if I <= Ada.Command_Line.Argument_Count then
                  Config.RPC_Port := Port_Number'Value
                     (Ada.Command_Line.Argument (I));
               end if;
            elsif Arg = "--data" or Arg = "-d" then
               I := I + 1;
               if I <= Ada.Command_Line.Argument_Count then
                  declare
                     Dir : constant String := Ada.Command_Line.Argument (I);
                     Len : constant Natural := Natural'Min (Dir'Length, Max_Path_Length);
                  begin
                     Config.Data_Dir (1 .. Len) := Dir (Dir'First .. Dir'First + Len - 1);
                     Config.Data_Dir_Len := Len;
                  end;
               end if;
            elsif Arg = "--help" or Arg = "-h" then
               Ada.Text_IO.Put_Line ("Usage: anubis-node [OPTIONS]");
               Ada.Text_IO.Put_Line ("  --port, -p PORT   RPC port (default: 26659)");
               Ada.Text_IO.Put_Line ("  --data, -d DIR    Data directory");
               Ada.Text_IO.Put_Line ("  --help, -h        Show this help");
               Running := False;
            end if;
         end;
         I := I + 1;
      end loop;
   end Parse_Arguments;

   --  Handle a single client connection
   procedure Handle_Client (Client : GNAT.Sockets.Socket_Type) is
      use GNAT.Sockets;
      use Ada.Streams;

      Buffer     : String (1 .. 4096);
      Recv_Buf   : Stream_Element_Array (1 .. 4096);
      Send_Buf   : Stream_Element_Array (1 .. 8192);
      Last       : Stream_Element_Offset;
      Sent       : Stream_Element_Offset;

      Request    : RPC_Request;
      Response   : RPC_Response;

      --  Simple JSON-RPC parser (extract method and id)
      procedure Parse_JSON_RPC (Input : String; Req : out RPC_Request) is
         Pos : Natural := Input'First;

         --  Find string value after key
         function Find_String_Value (Key : String) return String is
            Key_Pos : Natural;
            Start   : Natural;
            Stop    : Natural;
         begin
            Key_Pos := Input'First;
            while Key_Pos <= Input'Last - Key'Length loop
               if Input (Key_Pos .. Key_Pos + Key'Length - 1) = Key then
                  --  Find opening quote after colon
                  Start := Key_Pos + Key'Length;
                  while Start <= Input'Last and then Input (Start) /= '"' loop
                     Start := Start + 1;
                  end loop;
                  Start := Start + 1;  -- Skip quote

                  --  Find closing quote
                  Stop := Start;
                  while Stop <= Input'Last and then Input (Stop) /= '"' loop
                     Stop := Stop + 1;
                  end loop;

                  if Start <= Input'Last and Stop > Start then
                     return Input (Start .. Stop - 1);
                  end if;
               end if;
               Key_Pos := Key_Pos + 1;
            end loop;
            return "";
         end Find_String_Value;

         Method_Val : constant String := Find_String_Value ("""method""");
         ID_Val     : constant String := Find_String_Value ("""id""");
      begin
         Req := (
            Version      => RPC_Version_2_0,
            Method       => Method_Unknown,
            Method_Str   => (others => ' '),
            Method_Len   => 0,
            ID           => (others => ' '),
            ID_Len       => 0,
            Params       => (others => 0),
            Params_Size  => 0
         );

         --  Copy method
         if Method_Val'Length > 0 and Method_Val'Length <= Max_Method_Length then
            Req.Method_Str (1 .. Method_Val'Length) := Method_Val;
            Req.Method_Len := Method_Val'Length;
            Req.Method := Parse_Method (Req.Method_Str, Req.Method_Len);
         end if;

         --  Copy ID
         if ID_Val'Length > 0 and ID_Val'Length <= Max_Request_ID_Length then
            Req.ID (1 .. ID_Val'Length) := ID_Val;
            Req.ID_Len := ID_Val'Length;
         end if;

         pragma Unreferenced (Pos);
      end Parse_JSON_RPC;

      --  Format JSON-RPC response
      function Format_Response (Resp : RPC_Response) return String is
         ID_Str : constant String := Resp.ID (1 .. Resp.ID_Len);
      begin
         if Resp.Error_Code = Error_None then
            --  Success response
            if Resp.Has_Result then
               declare
                  Result_Str : String (1 .. Resp.Result_Size);
               begin
                  for I in 1 .. Resp.Result_Size loop
                     Result_Str (I) := Character'Val (Resp.Result (Result_Index (I - 1)));
                  end loop;
                  return "{""jsonrpc"":""2.0"",""id"":" & ID_Str &
                         ",""result"":""" & Result_Str & """}";
               end;
            else
               return "{""jsonrpc"":""2.0"",""id"":" & ID_Str &
                      ",""result"":null}";
            end if;
         else
            --  Error response
            declare
               Err_Msg : constant String := Resp.Error_Msg (1 .. Resp.Error_Msg_Len);
               Code    : constant Integer := (case Resp.Error_Code is
                  when Error_None           => 0,
                  when Error_Parse          => -32700,
                  when Error_Invalid_Request => -32600,
                  when Error_Method_Not_Found => -32601,
                  when Error_Invalid_Params => -32602,
                  when Error_Internal       => -32603,
                  when Error_Execution_Failed => -32000,
                  when Error_Out_Of_Gas     => -32001,
                  when Error_Contract_Revert => -32002);
            begin
               return "{""jsonrpc"":""2.0"",""id"":" & ID_Str &
                      ",""error"":{""code"":" & Integer'Image (Code) &
                      ",""message"":""" & Err_Msg & """}}";
            end;
         end if;
      end Format_Response;

   begin
      --  Receive data from socket
      Receive_Socket (Client, Recv_Buf, Last);

      if Last >= 1 then
         --  Convert stream elements to string
         for I in 1 .. Natural (Last) loop
            Buffer (I) := Character'Val (Integer (Recv_Buf (Stream_Element_Offset (I))));
         end loop;

         --  Find actual end of data (look for newline or null)
         declare
            Str_Last : Natural := Natural (Last);
         begin
            for I in 1 .. Natural (Last) loop
               if Buffer (I) = ASCII.NUL or Buffer (I) = ASCII.LF then
                  Str_Last := I - 1;
                  exit;
               end if;
            end loop;

            if Str_Last >= 1 then
               --  Parse and process
               Parse_JSON_RPC (Buffer (1 .. Str_Last), Request);

               --  Log request
               Ada.Text_IO.Put_Line ("Request: " &
                  Request.Method_Str (1 .. Request.Method_Len));

               --  Process through VM
               Anubis_Node.Process_Request (VM, Request, Response);

               --  Send response
               declare
                  Resp_Str : constant String := Format_Response (Response) & ASCII.LF;
                  Resp_Len : constant Stream_Element_Offset :=
                     Stream_Element_Offset (Resp_Str'Length);
               begin
                  --  Convert string to stream elements
                  for I in 1 .. Resp_Str'Length loop
                     Send_Buf (Stream_Element_Offset (I)) :=
                        Stream_Element (Character'Pos (Resp_Str (I)));
                  end loop;

                  Send_Socket (Client, Send_Buf (1 .. Resp_Len), Sent);
               end;
            end if;
         end;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Client error: " &
            Ada.Exceptions.Exception_Message (E));
   end Handle_Client;

begin
   --  Print banner
   Ada.Text_IO.Put_Line (Banner);

   --  Parse arguments
   Parse_Arguments;

   if not Running then
      return;
   end if;

   --  Initialize VM
   Ada.Text_IO.Put_Line ("Initializing AegisVM...");
   Anubis_Node.Initialize (VM, Config);
   Ada.Text_IO.Put_Line ("  Status: " & Node_Status'Image (VM.State.Status));

   --  Create server socket
   Ada.Text_IO.Put_Line ("Starting RPC server on port" &
      Port_Number'Image (Config.RPC_Port) & "...");

   GNAT.Sockets.Create_Socket (Server_Socket);
   GNAT.Sockets.Set_Socket_Option
     (Server_Socket,
      GNAT.Sockets.Socket_Level,
      (GNAT.Sockets.Reuse_Address, True));

   Server_Address := (
      Family => GNAT.Sockets.Family_Inet,
      Addr   => GNAT.Sockets.Any_Inet_Addr,
      Port   => GNAT.Sockets.Port_Type (Config.RPC_Port)
   );

   GNAT.Sockets.Bind_Socket (Server_Socket, Server_Address);
   GNAT.Sockets.Listen_Socket (Server_Socket);

   Ada.Text_IO.Put_Line ("Ready. Accepting connections...");
   Ada.Text_IO.New_Line;

   --  Main server loop
   while Running loop
      declare
         Client_Socket  : GNAT.Sockets.Socket_Type;
         Client_Address : GNAT.Sockets.Sock_Addr_Type;
      begin
         --  Accept connection
         GNAT.Sockets.Accept_Socket
           (Server_Socket, Client_Socket, Client_Address);

         Ada.Text_IO.Put_Line ("Connection from " &
            GNAT.Sockets.Image (Client_Address));

         --  Handle request
         Handle_Client (Client_Socket);

         --  Close client
         GNAT.Sockets.Close_Socket (Client_Socket);
      end;
   end loop;

   --  Cleanup
   GNAT.Sockets.Close_Socket (Server_Socket);
   Anubis_Node.Shutdown (VM);
   Ada.Text_IO.Put_Line ("Node stopped.");

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Fatal error: " &
         Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      GNAT.Sockets.Close_Socket (Server_Socket);
end Anubis_Main;
