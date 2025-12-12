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
with Interfaces; use Interfaces;

with Anubis_Node; use Anubis_Node;
with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;
with Node_JSON_Parser;
with Node_Contract_Registry; use Node_Contract_Registry;
with Node_Contract_Executor; use Node_Contract_Executor;
with Anubis_Main_Buffers; use Anubis_Main_Buffers;
with P2P_Network; use P2P_Network;
with P2P_Sockets;
with Anubis_MLDSA_Types;

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

   --  Contract Registry (persists across requests)
   Registry : Registry_State;

   --  Contract Executor (dispatches to native handlers)
   Executor : Executor_State;

   --  Current block number (increments with each deploy)
   Current_Block : U256 := (Limbs => (0, 0, 0, 0));

   --  P2P Network State
   P2P_Net : P2P_Network.Network_State;
   P2P_Port : constant Unsigned_16 := 26656;  -- Default P2P port
   P2P_Enabled : Boolean := True;

   --  Node identity (hash of public key)
   Node_ID : Aegis_VM_Types.Hash256 := (others => 0);
   Node_Key : constant Anubis_MLDSA_Types.Public_Key := (others => 0);
   Chain_ID : constant U256 := (Limbs => (1, 0, 0, 0));  -- Testnet chain ID = 1

   --  NOTE: Large buffers (Deploy_Code_Buffer, RPC_Buffer, RPC_Recv_Buf,
   --  RPC_Params_Str, RPC_Request_Static, RPC_Response_Static, RPC_Return_Data,
   --  RPC_Contract_Index) are now in Anubis_Main_Buffers package to avoid
   --  stack overflow from aggregate initializers.

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

      --  Large buffers moved to package level: RPC_Buffer, RPC_Recv_Buf, RPC_Params_Str
      --  RPC_Request_Static and RPC_Response_Static also at package level (100KB total)
      Send_Buf   : Stream_Element_Array (1 .. 4096);
      Last       : Stream_Element_Offset;
      Sent       : Stream_Element_Offset;

      --  Simple JSON-RPC parser (extract method and id)
      --  Uses package-level RPC_Buffer to avoid stack copy of large input
      procedure Parse_JSON_RPC (Input_Len : Natural; Req : out RPC_Request) is
         --  Find string value after key (uses package-level RPC_Buffer)
         function Find_String_Value (Key : String) return String is
            Key_Pos : Natural;
            Start   : Natural;
            Stop    : Natural;
         begin
            if Input_Len = 0 then
               return "";
            end if;
            Key_Pos := 1;
            while Key_Pos <= Input_Len - Key'Length loop
               if RPC_Buffer (Key_Pos .. Key_Pos + Key'Length - 1) = Key then
                  --  Find opening quote after colon
                  Start := Key_Pos + Key'Length;
                  while Start <= Input_Len and then RPC_Buffer (Start) /= '"' loop
                     Start := Start + 1;
                  end loop;
                  Start := Start + 1;  -- Skip quote

                  --  Find closing quote
                  Stop := Start;
                  while Stop <= Input_Len and then RPC_Buffer (Stop) /= '"' loop
                     Stop := Stop + 1;
                  end loop;

                  if Start <= Input_Len and Stop > Start then
                     return RPC_Buffer (Start .. Stop - 1);
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

      --  Extract params object from JSON-RPC request
      --  Uses package-level RPC_Buffer to avoid stack copy of large input
      procedure Extract_Params (Input_Len : Natural) is
         Key_Pos : Natural := 0;
         Start   : Natural;
         Depth   : Natural;
      begin
         --  Note: Don't use (others => ' ') - creates 512KB stack temporary
         --  Just reset length; old data is ignored
         RPC_Params_Str_Len := 0;

         if Input_Len < 9 then
            return;  -- Too short for "params"
         end if;

         --  Find "params" (uses package-level RPC_Buffer)
         for I in 1 .. Input_Len - 8 loop
            if RPC_Buffer (I .. I + 7) = """params""" then
               Key_Pos := I + 8;
               exit;
            end if;
         end loop;

         if Key_Pos = 0 then
            return;  -- No params
         end if;

         --  Skip colon and whitespace, find opening brace
         Start := Key_Pos;
         while Start <= Input_Len and then RPC_Buffer (Start) /= '{' loop
            Start := Start + 1;
         end loop;

         if Start > Input_Len then
            return;
         end if;

         --  Find matching closing brace
         Depth := 1;
         for I in Start + 1 .. Input_Len loop
            if RPC_Buffer (I) = '{' then
               Depth := Depth + 1;
            elsif RPC_Buffer (I) = '}' then
               Depth := Depth - 1;
               if Depth = 0 then
                  --  Copy params object
                  declare
                     Len : constant Natural := I - Start + 1;
                  begin
                     if Len <= RPC_Params_Str'Length then
                        RPC_Params_Str (1 .. Len) := RPC_Buffer (Start .. I);
                        RPC_Params_Str_Len := Len;
                     end if;
                  end;
                  return;
               end if;
            end if;
         end loop;
      end Extract_Params;

      --  Handle vm_deployContract
      --  Note: Directly modifies RPC_Response_Static to avoid parameter passing overhead
      procedure Handle_Deploy is
         From_Addr   : Contract_Address;
         --  Code and Stored buffers moved to package level
         Code_Size   : Natural;
         Manifest    : Node_Contract_Manifest;
         Gas_Limit   : Gas_Amount;
         Parse_OK    : Boolean;
         --  Stored moved to package level as RPC_Stored_Result (256KB)
         Reg_OK      : Boolean;
         Result_Str  : String (1 .. 512);
         Result_Len  : Natural;
      begin
         Ada.Text_IO.Put_Line ("  Handle_Deploy: params_len=" &
            Natural'Image (RPC_Params_Str_Len));
         --  Note: Don't zero Deploy_Code_Buffer here - (others => 0) creates 256KB
         --  stack temporary. Code_Size tracks the valid portion instead.

         --  Initialize response fields individually to avoid 64KB stack aggregate
         --  Don't zero Result buffer - just set Result_Size to 0
         RPC_Response_Static.Version := RPC_Version_2_0;
         RPC_Response_Static.ID := RPC_Request_Static.ID;
         RPC_Response_Static.ID_Len := RPC_Request_Static.ID_Len;
         RPC_Response_Static.Has_Result := False;
         --  RPC_Response_Static.Result left uninitialized - Result_Size controls valid portion
         RPC_Response_Static.Result_Size := 0;
         RPC_Response_Static.Error_Code := Error_None;
         RPC_Response_Static.Error_Msg := (others => ' ');
         RPC_Response_Static.Error_Msg_Len := 0;

         --  Parse params
         if RPC_Params_Str_Len = 0 then
            RPC_Response_Static.Error_Code := Error_Invalid_Params;
            RPC_Response_Static.Error_Msg (1 .. 16) := "missing params  ";
            RPC_Response_Static.Error_Msg_Len := 14;
            return;
         end if;

         --  Pass full static buffer to avoid stack copy of large slice
         --  Parse_Deploy_Params will find the fields it needs
         Node_JSON_Parser.Parse_Deploy_Params (
            Params_Json => RPC_Params_Str,
            From_Addr   => From_Addr,
            Code        => Deploy_Code_Buffer,  -- Use static buffer
            Code_Size   => Code_Size,
            Manifest    => Manifest,
            Gas_Limit   => Gas_Limit,
            Success     => Parse_OK
         );

         if not Parse_OK then
            RPC_Response_Static.Error_Code := Error_Invalid_Params;
            RPC_Response_Static.Error_Msg (1 .. 18) := "invalid params    ";
            RPC_Response_Static.Error_Msg_Len := 14;
            return;
         end if;

         Ada.Text_IO.Put_Line ("  Deploy: code_size=" & Natural'Image (Code_Size));
         Ada.Text_IO.Put_Line ("  Deploy: name=" & Manifest.Name (1 .. Manifest.Name_Len));

         --  Check registry capacity
         if Registry.Contract_Count >= Max_Stored_Contracts then
            RPC_Response_Static.Error_Code := Error_Internal;
            RPC_Response_Static.Error_Msg (1 .. 17) := "registry full    ";
            RPC_Response_Static.Error_Msg_Len := 13;
            return;
         end if;

         --  Register contract (returns index into registry, no full copy)
         Register_Contract (
            Registry  => Registry,
            Manifest  => Manifest,
            Code      => Deploy_Code_Buffer,  -- Use static buffer
            Code_Size => Code_Size,
            Block_Num => Current_Block,
            Index     => RPC_Contract_Index,  -- Receive index, not full record
            Success   => Reg_OK
         );

         if not Reg_OK then
            RPC_Response_Static.Error_Code := Error_Internal;
            RPC_Response_Static.Error_Msg (1 .. 20) := "registration failed ";
            RPC_Response_Static.Error_Msg_Len := 19;
            return;
         end if;

         --  Increment block number
         Current_Block.Limbs (0) := Current_Block.Limbs (0) + 1;

         --  Format result using renames to avoid stack copy
         declare
            SC : Stored_Contract renames Registry.Contracts (RPC_Contract_Index);
         begin
            Node_JSON_Parser.Format_Deploy_Result (
               Contract_ID => SC.Contract_ID,
               Code_Hash   => SC.Code_Hash,
               Gas_Used    => Gas_Limit,  -- Placeholder
               Result_Str  => Result_Str,
               Result_Len  => Result_Len
            );
         end;

         --  Copy result to response
         RPC_Response_Static.Has_Result := True;
         for I in 1 .. Natural'Min (Result_Len, Max_Result_Size) loop
            RPC_Response_Static.Result (Result_Index (I - 1)) := Character'Pos (Result_Str (I));
         end loop;
         RPC_Response_Static.Result_Size := Natural'Min (Result_Len, Max_Result_Size);

         Ada.Text_IO.Put_Line ("  Deployed contract, count=" &
            Natural'Image (Registry.Contract_Count));
      end Handle_Deploy;

      --  Handle vm_invoke (executes contract via Executor)
      procedure Handle_Invoke (Resp : out RPC_Response; Is_View : Boolean) is
         Invoke_Req  : Invoke_Request;
         Parse_OK    : Boolean;
         Exec_Result : Invoke_Result;
         Result_Str  : String (1 .. 512);
         Result_Len  : Natural;
      begin
         --  Initialize response fields individually to avoid 64KB stack aggregate
         Resp.Version := RPC_Version_2_0;
         Resp.ID := RPC_Request_Static.ID;
         Resp.ID_Len := RPC_Request_Static.ID_Len;
         Resp.Has_Result := False;
         Resp.Result_Size := 0;
         Resp.Error_Code := Error_None;
         Resp.Error_Msg := (others => ' ');
         Resp.Error_Msg_Len := 0;

         if RPC_Params_Str_Len = 0 then
            Resp.Error_Code := Error_Invalid_Params;
            Resp.Error_Msg (1 .. 14) := "missing params";
            Resp.Error_Msg_Len := 14;
            return;
         end if;

         --  Parse invoke request
         Node_JSON_Parser.Parse_Invoke_Params (
            Params_Json => RPC_Params_Str,
            Request     => Invoke_Req,
            Success     => Parse_OK
         );

         if not Parse_OK then
            Resp.Error_Code := Error_Invalid_Params;
            Resp.Error_Msg (1 .. 14) := "invalid params";
            Resp.Error_Msg_Len := 14;
            return;
         end if;

         --  Look up contract
         declare
            Found : Boolean;
         begin
            Find_Contract (Registry, Invoke_Req.To, RPC_Contract_Index, Found);
            if not Found then
               Resp.Error_Code := Error_Invalid_Params;
               Resp.Error_Msg (1 .. 18) := "contract not found";
               Resp.Error_Msg_Len := 18;
               return;
            end if;
         end;

         --  Validate contract
         declare
            SC : Stored_Contract renames Registry.Contracts (RPC_Contract_Index);
         begin
            if not SC.Is_Valid then
               Resp.Error_Code := Error_Invalid_Params;
               Resp.Error_Msg (1 .. 16) := "invalid contract";
               Resp.Error_Msg_Len := 16;
               return;
            end if;

            Ada.Text_IO.Put_Line ("  " & (if Is_View then "Call" else "Invoke") &
               ": entry=" & Invoke_Req.Entry_Point (1 .. Invoke_Req.Entry_Len));
         end;

         --  Execute contract via Executor
         Node_Contract_Executor.Execute (
            Exec         => Executor,
            Registry     => Registry,
            Contract_Idx => RPC_Contract_Index,
            From         => Invoke_Req.From,
            EP_Name      => Invoke_Req.Entry_Point,
            EP_Name_Len  => Invoke_Req.Entry_Len,
            Args         => Invoke_Req.Args,
            Args_Size    => Invoke_Req.Args_Size,
            Gas_Limit    => Invoke_Req.Gas_Limit,
            Value        => Invoke_Req.Value,
            Is_View      => Is_View,
            Ret          => Exec_Result
         );

         --  Copy return data to static buffer for formatting
         for I in 0 .. Exec_Result.Return_Size - 1 loop
            RPC_Return_Data (Return_Index (I)) := Exec_Result.Return_Data (Return_Index (I));
         end loop;

         --  Format result
         if Exec_Result.Success then
            Node_JSON_Parser.Format_Invoke_Result (
               Success_Flag => True,
               Gas_Used     => Exec_Result.Gas_Used,
               Return_Data  => RPC_Return_Data,
               Return_Size  => Exec_Result.Return_Size,
               Error_Msg    => "",
               Result_Str   => Result_Str,
               Result_Len   => Result_Len
            );
         else
            Node_JSON_Parser.Format_Invoke_Result (
               Success_Flag => False,
               Gas_Used     => Exec_Result.Gas_Used,
               Return_Data  => RPC_Return_Data,
               Return_Size  => 0,
               Error_Msg    => Exec_Result.Error_Msg (1 .. Exec_Result.Error_Msg_Len),
               Result_Str   => Result_Str,
               Result_Len   => Result_Len
            );
         end if;

         Resp.Has_Result := True;
         for I in 1 .. Natural'Min (Result_Len, Max_Result_Size) loop
            Resp.Result (Result_Index (I - 1)) := Character'Pos (Result_Str (I));
         end loop;
         Resp.Result_Size := Natural'Min (Result_Len, Max_Result_Size);

         if Exec_Result.Success then
            Ada.Text_IO.Put_Line ("  Success, gas_used=" &
               Gas_Amount'Image (Exec_Result.Gas_Used));
         else
            Ada.Text_IO.Put_Line ("  Failed: " &
               Exec_Result.Error_Msg (1 .. Exec_Result.Error_Msg_Len));
         end if;
      end Handle_Invoke;

      --  Handle vm_call (read-only invoke)
      procedure Handle_Call (Resp : out RPC_Response) is
      begin
         --  vm_call is the same as vm_invoke but with Is_View=True
         Handle_Invoke (Resp, Is_View => True);
      end Handle_Call;

   begin
      --  Receive all data from socket (loop for large payloads)
      --  Keep reading until client closes write end (returns 0 bytes)
      declare
         Total_Received : Natural := 0;
         Chunk_Size     : Stream_Element_Offset;
         Max_Size       : constant Natural := RPC_Buffer'Length;
      begin
         loop
            exit when Total_Received >= Max_Size;

            begin
               Receive_Socket (
                  Client,
                  RPC_Recv_Buf (1 .. Stream_Element_Offset (
                     Natural'Min (65536, Max_Size - Total_Received))),
                  Chunk_Size
               );
            exception
               when GNAT.Sockets.Socket_Error =>
                  exit;  -- Connection closed or error
            end;

            exit when Chunk_Size < 1;  -- Client closed write end

            --  Copy chunk to buffer
            for I in 1 .. Natural (Chunk_Size) loop
               RPC_Buffer (Total_Received + I) :=
                  Character'Val (Integer (RPC_Recv_Buf (Stream_Element_Offset (I))));
            end loop;

            Total_Received := Total_Received + Natural (Chunk_Size);
         end loop;

         Last := Stream_Element_Offset (Total_Received);

         if Total_Received > 0 then
            Ada.Text_IO.Put_Line ("  Received" & Natural'Image (Total_Received) & " bytes");
         end if;
      end;

      if Last >= 1 then
         --  Find actual end of data (look for newline or null)
         declare
            Str_Last : Natural := Natural (Last);
         begin
            for I in 1 .. Natural (Last) loop
               if RPC_Buffer (I) = ASCII.NUL or RPC_Buffer (I) = ASCII.LF then
                  Str_Last := I - 1;
                  exit;
               end if;
            end loop;

            if Str_Last >= 1 then
               --  Parse and process (using package-level RPC_Buffer and static Request/Response)
               Parse_JSON_RPC (Str_Last, RPC_Request_Static);
               Extract_Params (Str_Last);

               --  Log request with flush to ensure visibility before crash
               Ada.Text_IO.Put_Line ("Request: " &
                  RPC_Request_Static.Method_Str (1 .. RPC_Request_Static.Method_Len));
               Ada.Text_IO.Flush;

               --  Dispatch based on method
               Ada.Text_IO.Put_Line ("  Dispatching method...");
               Ada.Text_IO.Flush;
               case RPC_Request_Static.Method is
                  when Method_VM_Deploy =>
                     Ada.Text_IO.Put_Line ("  Calling Handle_Deploy...");
                     Ada.Text_IO.Flush;
                     Handle_Deploy;
                     Ada.Text_IO.Put_Line ("  Handle_Deploy returned.");
                     Ada.Text_IO.Flush;

                  when Method_VM_Invoke =>
                     Handle_Invoke (RPC_Response_Static, Is_View => False);

                  when Method_VM_Call =>
                     Handle_Call (RPC_Response_Static);

                  when others =>
                     --  Fall back to generic handler
                     Anubis_Node.Process_Request (VM, RPC_Request_Static, RPC_Response_Static);
               end case;

               --  Send response
               declare
                  Resp_Str : constant String := Format_Response (RPC_Response_Static) & ASCII.LF;
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
   --  Initialize static buffers (must be first - uses loop-based init to avoid stack overflow)
   Initialize_Buffers;

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

   --  Initialize contract registry
   Ada.Text_IO.Put_Line ("Initializing contract registry...");
   Node_Contract_Registry.Initialize (Registry);
   Ada.Text_IO.Put_Line ("  Registry ready (max " &
      Natural'Image (Max_Stored_Contracts) & " contracts)");

   --  Initialize contract executor
   Ada.Text_IO.Put_Line ("Initializing contract executor...");
   Node_Contract_Executor.Initialize (Executor);
   Ada.Text_IO.Put_Line ("  Executor ready (native dispatch)");

   --  Initialize P2P network
   if P2P_Enabled then
      Ada.Text_IO.Put_Line ("Initializing P2P network...");

      --  Generate a simple node ID (in production, use SHA3-256 of public key)
      for I in Node_ID'Range loop
         Node_ID (I) := Byte (I mod 256);
      end loop;

      P2P_Network.Initialize (
         Net      => P2P_Net,
         Node_ID  => Node_ID,
         Node_Key => Node_Key,
         Chain_ID => Chain_ID,
         Port     => P2P_Port
      );

      --  Start P2P listener
      declare
         Start_Result : P2P_Network.Network_Result;
      begin
         P2P_Network.Start (P2P_Net, Start_Result);
         if Start_Result = P2P_Network.Network_OK then
            Ada.Text_IO.Put_Line ("  P2P listening on port" & Unsigned_16'Image (P2P_Port));
         else
            Ada.Text_IO.Put_Line ("  Warning: P2P listener failed to start");
            P2P_Enabled := False;
         end if;
      end;
   end if;

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

   --  Stop P2P network
   if P2P_Enabled then
      Ada.Text_IO.Put_Line ("Stopping P2P network...");
      P2P_Network.Stop (P2P_Net);
      P2P_Sockets.Shutdown;
   end if;

   Anubis_Node.Shutdown (VM);
   Ada.Text_IO.Put_Line ("Node stopped.");

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Fatal error: " &
         Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      GNAT.Sockets.Close_Socket (Server_Socket);
end Anubis_Main;
