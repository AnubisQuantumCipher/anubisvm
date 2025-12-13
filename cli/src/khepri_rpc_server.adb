with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.Sockets;
with Ada.Streams;

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;

package body Khepri_RPC_Server is

   use GNAT.Sockets;
   use Ada.Streams;

   --  Server state
   Server_Socket : Socket_Type;
   Server_Address : Sock_Addr_Type;
   Current_Status : Server_Status := Stopped;
   Server_Port : Positive := Default_Port;
   Stop_Requested : Boolean := False;

   --  Simple JSON string extraction (finds "key": "value" or "key": number)
   function Extract_JSON_String (JSON : String; Key : String) return String is
      Search : constant String := """" & Key & """:";
      Pos : Natural;
      Start_Pos, End_Pos : Natural;
   begin
      Pos := JSON'First;
      while Pos <= JSON'Last - Search'Length loop
         if JSON (Pos .. Pos + Search'Length - 1) = Search then
            --  Found key, now find value
            Start_Pos := Pos + Search'Length;
            --  Skip whitespace
            while Start_Pos <= JSON'Last and then
                  (JSON (Start_Pos) = ' ' or JSON (Start_Pos) = ASCII.HT) loop
               Start_Pos := Start_Pos + 1;
            end loop;

            if Start_Pos <= JSON'Last then
               if JSON (Start_Pos) = '"' then
                  --  String value
                  Start_Pos := Start_Pos + 1;
                  End_Pos := Start_Pos;
                  while End_Pos <= JSON'Last and then JSON (End_Pos) /= '"' loop
                     End_Pos := End_Pos + 1;
                  end loop;
                  return JSON (Start_Pos .. End_Pos - 1);
               elsif JSON (Start_Pos) in '0' .. '9' or JSON (Start_Pos) = '-' then
                  --  Number value
                  End_Pos := Start_Pos;
                  while End_Pos <= JSON'Last and then
                        JSON (End_Pos) in '0' .. '9' loop
                     End_Pos := End_Pos + 1;
                  end loop;
                  return JSON (Start_Pos .. End_Pos - 1);
               elsif JSON (Start_Pos) = '{' or JSON (Start_Pos) = '[' then
                  --  Object or array - find matching bracket
                  declare
                     Depth : Natural := 1;
                     Open_Bracket : constant Character := JSON (Start_Pos);
                     Close_Bracket : constant Character :=
                       (if Open_Bracket = '{' then '}' else ']');
                  begin
                     End_Pos := Start_Pos + 1;
                     while End_Pos <= JSON'Last and then Depth > 0 loop
                        if JSON (End_Pos) = Open_Bracket then
                           Depth := Depth + 1;
                        elsif JSON (End_Pos) = Close_Bracket then
                           Depth := Depth - 1;
                        end if;
                        End_Pos := End_Pos + 1;
                     end loop;
                     return JSON (Start_Pos .. End_Pos - 1);
                  end;
               end if;
            end if;
         end if;
         Pos := Pos + 1;
      end loop;
      return "";
   end Extract_JSON_String;

   procedure Parse_Request (
      JSON       : String;
      ID         : out Natural;
      Method     : out Unbounded_String;
      Params     : out Unbounded_String;
      Success    : out Boolean
   ) is
      ID_Str : constant String := Extract_JSON_String (JSON, "id");
      Method_Str : constant String := Extract_JSON_String (JSON, "method");
      Params_Str : constant String := Extract_JSON_String (JSON, "params");
   begin
      Success := False;
      ID := 0;
      Method := Null_Unbounded_String;
      Params := Null_Unbounded_String;

      if Method_Str = "" then
         return;
      end if;

      --  Parse ID
      if ID_Str /= "" then
         begin
            ID := Natural'Value (ID_Str);
         exception
            when others => ID := 0;
         end;
      end if;

      Method := To_Unbounded_String (Method_Str);
      Params := To_Unbounded_String (Params_Str);
      Success := True;
   end Parse_Request;

   function Format_Response (
      ID      : Natural;
      Result  : String;
      Is_Error : Boolean := False;
      Error_Code : Integer := 0;
      Error_Msg  : String := ""
   ) return String is
   begin
      if Is_Error then
         return "{""jsonrpc"":""2.0"",""id"":" & Natural'Image (ID) &
                ",""error"":{""code"":" & Integer'Image (Error_Code) &
                ",""message"":""" & Error_Msg & """}}";
      else
         return "{""jsonrpc"":""2.0"",""id"":" & Natural'Image (ID) &
                ",""result"":" & Result & "}";
      end if;
   end Format_Response;

   procedure Dispatch_Method (
      Method  : String;
      Params  : String;
      Result  : out Unbounded_String;
      Success : out Boolean
   ) is
      Res : Local_Executor.Exec_Result;
      From_Zero : constant Contract_Address := (others => 0);
      Empty_Args : Local_Executor.Arg_Array (0 .. -1);
   begin
      Success := False;
      Result := Null_Unbounded_String;

      --  vm_call: {"contract":"...", "entry":"...", "args":["..."]}
      if Method = "vm_call" then
         declare
            Contract_Name : constant String := Extract_JSON_String (Params, "contract");
            Entry_Name : constant String := Extract_JSON_String (Params, "entry");
            Args_Str : constant String := Extract_JSON_String (Params, "args");
         begin
            if Contract_Name = "" or Entry_Name = "" then
               Result := To_Unbounded_String ("""Invalid params: missing contract or entry""");
               return;
            end if;

            --  Simple case: no args
            if Args_Str = "" or Args_Str = "[]" then
               Local_Executor.Execute_Local
                 (From_Zero, Contract_Name, Entry_Name, Empty_Args,
                  1_000_000, U256_Zero, Res);
            else
               --  Parse args array - simplified: just extract first arg
               declare
                  Arg1 : constant String := Extract_JSON_String (Args_Str, "0");
                  Args : Local_Executor.Arg_Array (0 .. 0);
               begin
                  if Arg1 /= "" then
                     Args (0) := To_Unbounded_String (Arg1);
                     Local_Executor.Execute_Local
                       (From_Zero, Contract_Name, Entry_Name, Args,
                        1_000_000, U256_Zero, Res);
                  else
                     Local_Executor.Execute_Local
                       (From_Zero, Contract_Name, Entry_Name, Empty_Args,
                        1_000_000, U256_Zero, Res);
                  end if;
               end;
            end if;

            if Res.Success then
               Result := To_Unbounded_String (
                  "{""success"":true,""gas_used"":" &
                  Gas_Amount'Image (Res.Gas_Used) &
                  ",""return"":""0x" & To_String (Res.Return_Hex) & """}");
               Success := True;
            else
               Result := To_Unbounded_String (
                  "{""success"":false,""error"":""" & To_String (Res.Error) & """}");
               Success := True;  -- RPC succeeded, execution failed
            end if;
         end;

      --  vm_balance: Get vault total
      elsif Method = "vm_balance" then
         Local_Executor.Execute_Local
           (From_Zero, "SimpleVault", "TotalDeposits", Empty_Args,
            1_000_000, U256_Zero, Res);
         if Res.Success then
            Result := To_Unbounded_String ("""0x" & To_String (Res.Return_Hex) & """");
            Success := True;
         end if;

      --  vm_tokens: Get token supply
      elsif Method = "vm_tokens" then
         Local_Executor.Execute_Local
           (From_Zero, "SimpleToken", "TotalSupply", Empty_Args,
            1_000_000, U256_Zero, Res);
         if Res.Success then
            Result := To_Unbounded_String ("""0x" & To_String (Res.Return_Hex) & """");
            Success := True;
         end if;

      --  vm_counter: Get counter
      elsif Method = "vm_counter" then
         Local_Executor.Execute_Local
           (From_Zero, "HelloCounter", "Get_Count", Empty_Args,
            1_000_000, U256_Zero, Res);
         if Res.Success then
            Result := To_Unbounded_String ("""0x" & To_String (Res.Return_Hex) & """");
            Success := True;
         end if;

      --  vm_status: Server status
      elsif Method = "vm_status" then
         Result := To_Unbounded_String (
            "{""status"":""running"",""version"":""0.1.0"",""contracts"":6}");
         Success := True;

      --  vm_deposit: Deposit to vault
      elsif Method = "vm_deposit" then
         declare
            Amount : constant String := Extract_JSON_String (Params, "amount");
            Args : Local_Executor.Arg_Array (0 .. 0);
         begin
            if Amount /= "" then
               Args (0) := To_Unbounded_String ("0x" & Amount);
               Local_Executor.Execute_Local
                 (From_Zero, "SimpleVault", "Deposit", Args,
                  1_000_000, U256_Zero, Res);
               if Res.Success then
                  Result := To_Unbounded_String (
                     "{""success"":true,""total"":""0x" &
                     To_String (Res.Return_Hex) & """}");
                  Success := True;
               end if;
            end if;
         end;

      --  vm_withdraw: Withdraw from vault
      elsif Method = "vm_withdraw" then
         declare
            Amount : constant String := Extract_JSON_String (Params, "amount");
            Args : Local_Executor.Arg_Array (0 .. 0);
         begin
            if Amount /= "" then
               Args (0) := To_Unbounded_String ("0x" & Amount);
               Local_Executor.Execute_Local
                 (From_Zero, "SimpleVault", "Withdraw", Args,
                  1_000_000, U256_Zero, Res);
               if Res.Success then
                  Result := To_Unbounded_String (
                     "{""success"":true,""withdrawn"":""0x" &
                     To_String (Res.Return_Hex) & """}");
                  Success := True;
               end if;
            end if;
         end;

      else
         Result := To_Unbounded_String ("""Method not found: " & Method & """");
      end if;
   end Dispatch_Method;

   procedure Process_Request (
      Request_JSON  : String;
      Response_JSON : out Unbounded_String
   ) is
      ID : Natural;
      Method, Params : Unbounded_String;
      Parse_OK : Boolean;
      Result : Unbounded_String;
      Dispatch_OK : Boolean;
   begin
      Parse_Request (Request_JSON, ID, Method, Params, Parse_OK);

      if not Parse_OK then
         Response_JSON := To_Unbounded_String (
            Format_Response (0, "", True, -32600, "Invalid Request"));
         return;
      end if;

      Dispatch_Method (To_String (Method), To_String (Params), Result, Dispatch_OK);

      if Dispatch_OK then
         Response_JSON := To_Unbounded_String (
            Format_Response (ID, To_String (Result)));
      else
         Response_JSON := To_Unbounded_String (
            Format_Response (ID, "", True, -32601, "Method not found"));
      end if;
   end Process_Request;

   procedure Start (Port : Positive := Default_Port) is
   begin
      Server_Port := Port;
      Stop_Requested := False;

      --  Create socket
      Create_Socket (Server_Socket, Family_Inet, Socket_Stream);

      --  Allow address reuse
      Set_Socket_Option (Server_Socket, Socket_Level, (Reuse_Address, True));

      --  Bind to port
      Server_Address.Addr := Any_Inet_Addr;
      Server_Address.Port := Port_Type (Port);
      Bind_Socket (Server_Socket, Server_Address);

      --  Listen
      Listen_Socket (Server_Socket, 5);

      Current_Status := Running;
      Ada.Text_IO.Put_Line ("RPC Server started on port" & Positive'Image (Port));

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Failed to start server: " &
            Ada.Exceptions.Exception_Message (E));
         Current_Status := Error;
   end Start;

   procedure Stop is
   begin
      Stop_Requested := True;
      if Current_Status = Running then
         Close_Socket (Server_Socket);
         Current_Status := Stopped;
         Ada.Text_IO.Put_Line ("RPC Server stopped");
      end if;
   end Stop;

   function Is_Running return Boolean is
   begin
      return Current_Status = Running;
   end Is_Running;

   function Get_Status return Server_Status is
   begin
      return Current_Status;
   end Get_Status;

   procedure Run_Server is
      Client_Socket : Socket_Type;
      Client_Address : Sock_Addr_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 8192);
      Last : Ada.Streams.Stream_Element_Offset;

      function To_String (Data : Ada.Streams.Stream_Element_Array;
                         Len : Ada.Streams.Stream_Element_Offset) return String is
         Result : String (1 .. Natural (Len));
      begin
         for I in 1 .. Natural (Len) loop
            Result (I) := Character'Val (Data (Ada.Streams.Stream_Element_Offset (I)));
         end loop;
         return Result;
      end To_String;

      function To_Stream (S : String) return Ada.Streams.Stream_Element_Array is
         Result : Ada.Streams.Stream_Element_Array (1 .. S'Length);
      begin
         for I in S'Range loop
            Result (Ada.Streams.Stream_Element_Offset (I - S'First + 1)) :=
              Ada.Streams.Stream_Element (Character'Pos (S (I)));
         end loop;
         return Result;
      end To_Stream;

   begin
      while not Stop_Requested loop
         --  Accept connection
         Accept_Socket (Server_Socket, Client_Socket, Client_Address);

         --  Read request
         Receive_Socket (Client_Socket, Buffer, Last);

         if Last > 0 then
            declare
               Request : constant String := To_String (Buffer, Last);
               --  Find JSON body after HTTP headers
               Body_Start : Natural := 0;
               Response_JSON : Unbounded_String;
            begin
               --  Find double CRLF marking end of headers
               for I in Request'First .. Request'Last - 3 loop
                  if Request (I .. I + 3) = ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF then
                     Body_Start := I + 4;
                     exit;
                  end if;
               end loop;

               if Body_Start > 0 and Body_Start <= Request'Last then
                  Process_Request (Request (Body_Start .. Request'Last), Response_JSON);
               else
                  --  No headers, treat whole thing as JSON
                  Process_Request (Request, Response_JSON);
               end if;

               --  Send HTTP response
               declare
                  Response_Body : constant String := To_String (Response_JSON);
                  HTTP_Response : constant String :=
                     "HTTP/1.1 200 OK" & ASCII.CR & ASCII.LF &
                     "Content-Type: application/json" & ASCII.CR & ASCII.LF &
                     "Content-Length:" & Natural'Image (Response_Body'Length) & ASCII.CR & ASCII.LF &
                     "Access-Control-Allow-Origin: *" & ASCII.CR & ASCII.LF &
                     ASCII.CR & ASCII.LF &
                     Response_Body;
                  Send_Buffer : constant Ada.Streams.Stream_Element_Array :=
                     To_Stream (HTTP_Response);
                  Send_Last : Ada.Streams.Stream_Element_Offset;
               begin
                  Send_Socket (Client_Socket, Send_Buffer, Send_Last);
               end;
            end;
         end if;

         Close_Socket (Client_Socket);
      end loop;

   exception
      when E : others =>
         if not Stop_Requested then
            Ada.Text_IO.Put_Line ("Server error: " &
               Ada.Exceptions.Exception_Message (E));
         end if;
   end Run_Server;

end Khepri_RPC_Server;
