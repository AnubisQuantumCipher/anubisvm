pragma SPARK_Mode (Off);

with Ada.Streams;
use type Ada.Streams.Stream_Element_Offset;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Real_Time;
with GNAT.Sockets; use GNAT.Sockets;
with Interfaces; use Interfaces;

package body Khepri_RPC_Client is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   Hex_Chars : constant String := "0123456789abcdef";

   function Byte_To_Hex (B : Byte) return String is
   begin
      return (1 => Hex_Chars (Natural (B / 16) + 1),
              2 => Hex_Chars (Natural (B mod 16) + 1));
   end Byte_To_Hex;

   function Hex_To_Byte (C : Character) return Byte is
   begin
      case C is
         when '0' .. '9' => return Byte (Character'Pos (C) - Character'Pos ('0'));
         when 'a' .. 'f' => return Byte (Character'Pos (C) - Character'Pos ('a') + 10);
         when 'A' .. 'F' => return Byte (Character'Pos (C) - Character'Pos ('A') + 10);
         when others => return 0;
      end case;
   end Hex_To_Byte;

   ---------------------------------------------------------------------------
   --  Hex Conversion
   ---------------------------------------------------------------------------

   function To_Hex_String (Data : Byte_Array) return String is
      Result : String (1 .. 2 + Data'Length * 2);
      Offset : Natural := 3;
   begin
      Result (1 .. 2) := "0x";
      for I in Data'Range loop
         Result (Offset .. Offset + 1) := Byte_To_Hex (Data (I));
         Offset := Offset + 2;
      end loop;
      return Result;
   end To_Hex_String;

   function Hash_To_Hex (Hash : Hash256) return String is
   begin
      return To_Hex_String (Byte_Array (Hash));
   end Hash_To_Hex;

   function Address_To_Hex (Addr : Khepri_Types.Address) return String is
   begin
      return To_Hex_String (Byte_Array (Addr));
   end Address_To_Hex;

   procedure Parse_Hex (
      Hex_Str : in     String;
      Data    : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
      Start : Natural := Hex_Str'First;
      Idx   : Natural := Data'First;
   begin
      Data := (others => 0);
      Size := 0;
      Success := False;

      --  Skip "0x" prefix if present
      if Hex_Str'Length >= 2 and then Hex_Str (Hex_Str'First .. Hex_Str'First + 1) = "0x" then
         Start := Hex_Str'First + 2;
      end if;

      --  Parse hex pairs
      for I in Start .. Hex_Str'Last - 1 loop
         if I mod 2 = Start mod 2 then
            if Idx <= Data'Last then
               Data (Idx) := Hex_To_Byte (Hex_Str (I)) * 16 +
                             Hex_To_Byte (Hex_Str (I + 1));
               Idx := Idx + 1;
            else
               return;
            end if;
         end if;
      end loop;

      Size := Idx - Data'First;
      Success := True;
   end Parse_Hex;

   ---------------------------------------------------------------------------
   --  JSON Building/Parsing Helpers
   ---------------------------------------------------------------------------

   RPC_ID : Natural := 1;

   function Build_JSON_RPC_Request (
      Method : String;
      Params : String
   ) return String is
      ID_Str : constant String := Natural'Image (RPC_ID);
   begin
      RPC_ID := RPC_ID + 1;
      return "{""jsonrpc"":""2.0""," &
             """method"":""" & Method & """," &
             """params"":[" & Params & "]," &
             """id"":" & ID_Str (2 .. ID_Str'Last) & "}";
   end Build_JSON_RPC_Request;

   --  Simple JSON field extractor (no full parser, just pattern matching)
   function Extract_JSON_String (
      JSON  : String;
      Field : String
   ) return String is
      Pattern : constant String := """" & Field & """:""";
      Start_Pos : Natural;
      End_Pos : Natural;
   begin
      Start_Pos := Index (JSON, Pattern);
      if Start_Pos = 0 then
         return "";
      end if;
      Start_Pos := Start_Pos + Pattern'Length;
      End_Pos := Index (JSON (Start_Pos .. JSON'Last), """");
      if End_Pos = 0 then
         return "";
      end if;
      return JSON (Start_Pos .. Start_Pos + End_Pos - 2);
   end Extract_JSON_String;

   function Extract_JSON_Number (
      JSON  : String;
      Field : String
   ) return Word64 is
      Pattern : constant String := """" & Field & """:";
      Hex_Pattern : constant String := """" & Field & """:""0x";
      Start_Pos : Natural;
      End_Pos : Natural;
      Hex_Str : String (1 .. 20);
      Result : Word64 := 0;
   begin
      --  Check for hex string format
      Start_Pos := Index (JSON, Hex_Pattern);
      if Start_Pos > 0 then
         Start_Pos := Start_Pos + Hex_Pattern'Length;
         End_Pos := Index (JSON (Start_Pos .. JSON'Last), """");
         if End_Pos > 0 then
            declare
               Hex_Value : constant String := JSON (Start_Pos .. Start_Pos + End_Pos - 2);
            begin
               for C of Hex_Value loop
                  Result := Result * 16 + Word64 (Hex_To_Byte (C));
               end loop;
               return Result;
            end;
         end if;
      end if;

      --  Check for numeric format
      Start_Pos := Index (JSON, Pattern);
      if Start_Pos = 0 then
         return 0;
      end if;
      Start_Pos := Start_Pos + Pattern'Length;

      --  Skip whitespace
      while Start_Pos <= JSON'Last and then JSON (Start_Pos) = ' ' loop
         Start_Pos := Start_Pos + 1;
      end loop;

      --  Find end of number
      End_Pos := Start_Pos;
      while End_Pos <= JSON'Last and then
            (JSON (End_Pos) in '0' .. '9' or JSON (End_Pos) = 'x') loop
         End_Pos := End_Pos + 1;
      end loop;

      if Start_Pos < End_Pos then
         declare
            Num_Str : constant String := JSON (Start_Pos .. End_Pos - 1);
         begin
            if Num_Str'Length > 2 and then Num_Str (1 .. 2) = "0x" then
               for I in 3 .. Num_Str'Last loop
                  Result := Result * 16 + Word64 (Hex_To_Byte (Num_Str (I)));
               end loop;
            else
               Result := Word64'Value (Num_Str);
            end if;
         end;
      end if;

      return Result;
   exception
      when others =>
         return 0;
   end Extract_JSON_Number;

   function Extract_JSON_Boolean (
      JSON  : String;
      Field : String
   ) return Boolean is
      Pattern : constant String := """" & Field & """:";
      Start_Pos : Natural;
   begin
      Start_Pos := Index (JSON, Pattern);
      if Start_Pos = 0 then
         return False;
      end if;
      Start_Pos := Start_Pos + Pattern'Length;
      return Index (JSON (Start_Pos .. JSON'Last), "true") = Start_Pos or
             Index (JSON (Start_Pos .. JSON'Last), "1") = Start_Pos;
   end Extract_JSON_Boolean;

   function Has_JSON_Error (JSON : String) return Boolean is
   begin
      return Index (JSON, """error"":") > 0;
   end Has_JSON_Error;

   function Parse_RPC_Error (JSON : String) return RPC_Error is
      Code_Str : constant String := Extract_JSON_Number (JSON, "code")'Image;
      Message  : constant String := Extract_JSON_String (JSON, "message");
      Code_Val : Integer;
      Result   : RPC_Error;
   begin
      begin
         Code_Val := Integer'Value (Code_Str);
      exception
         when others =>
            Code_Val := -32603;
      end;

      --  Map JSON-RPC error codes to our enumeration
      Result.Code := (case Code_Val is
         when -32700 => RPC_Parse_Error,
         when -32600 => RPC_Invalid_Request,
         when -32601 => RPC_Method_Not_Found,
         when -32602 => RPC_Invalid_Params,
         when -32603 => RPC_Internal_Error,
         when -32000 => RPC_Server_Error,
         when -32001 => RPC_Tx_Underpriced,
         when -32002 => RPC_Tx_Already_Known,
         when -32003 => RPC_Tx_Pool_Full,
         when others => RPC_Unknown_Error);

      --  Copy message (truncate if needed)
      declare
         Len : constant Natural := Natural'Min (Message'Length, 32);
      begin
         for I in 0 .. Len - 1 loop
            Result.Message.Data (I) := Message (Message'First + I);
         end loop;
         Result.Message.Length := Len;
      end;

      return Result;
   end Parse_RPC_Error;

   ---------------------------------------------------------------------------
   --  HTTP Communication
   ---------------------------------------------------------------------------

   procedure HTTP_POST (
      Endpoint    : in     RPC_Endpoint;
      JSON_Body   : in     String;
      Response    : out    Unbounded_String;
      Error       : out    RPC_Error;
      Success     : out    Boolean
   ) is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;
      Host_Str : String (1 .. 256);
      Host_Len : Natural;
   begin
      Success := False;
      Response := Null_Unbounded_String;
      Error := No_Error;

      --  Extract host string
      Host_Len := Natural'Min (Endpoint.Host.Length, Host_Str'Length);
      for I in 0 .. Host_Len - 1 loop
         Host_Str (I + 1) := Endpoint.Host.Data (I);
      end loop;

      begin
         --  Resolve hostname
         Address.Addr := Addresses (Get_Host_By_Name (Host_Str (1 .. Host_Len)), 1);
         Address.Port := Port_Type (Endpoint.Port);

         --  Create socket
         Create_Socket (Socket);
         Set_Socket_Option (Socket, Socket_Level, (Send_Timeout, Timeout => Endpoint.Timeout * 1.0));
         Set_Socket_Option (Socket, Socket_Level, (Receive_Timeout, Timeout => Endpoint.Timeout * 1.0));

         --  Connect
         Connect_Socket (Socket, Address);
         Channel := Stream (Socket);

         --  Build HTTP request
         declare
            HTTP_Request : constant String :=
               "POST / HTTP/1.1" & ASCII.CR & ASCII.LF &
               "Host: " & Host_Str (1 .. Host_Len) & ASCII.CR & ASCII.LF &
               "Content-Type: application/json" & ASCII.CR & ASCII.LF &
               "Content-Length:" & Natural'Image (JSON_Body'Length) & ASCII.CR & ASCII.LF &
               "User-Agent: AnubisVM-Deployer/1.0" & ASCII.CR & ASCII.LF &
               "Connection: close" & ASCII.CR & ASCII.LF &
               ASCII.CR & ASCII.LF &
               JSON_Body;
         begin
            --  Send request
            String'Write (Channel, HTTP_Request);
         end;

         --  Read response
         declare
            Buffer : Ada.Streams.Stream_Element_Array (1 .. 8192);
            Last   : Ada.Streams.Stream_Element_Offset;
            In_Body : Boolean := False;

            function To_String (Data : Ada.Streams.Stream_Element_Array) return String is
               Result : String (1 .. Data'Length);
            begin
               for I in Data'Range loop
                  Result (Integer (I - Data'First + 1)) :=
                     Character'Val (Data (I));
               end loop;
               return Result;
            end To_String;
         begin
            loop
               begin
                  GNAT.Sockets.Receive_Socket (Socket, Buffer, Last);
                  exit when Last < Buffer'First;

                  declare
                     Received : constant String := To_String (Buffer (Buffer'First .. Last));
                  begin
                     --  Look for end of headers (double CRLF)
                     if not In_Body then
                        declare
                           Body_Start : Natural := Index (Received,
                                                         ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);
                        begin
                           if Body_Start > 0 then
                              In_Body := True;
                              Body_Start := Body_Start + 4;
                              Response := Response & Received (Body_Start .. Received'Last);
                           end if;
                        end;
                     else
                        Response := Response & Received;
                     end if;
                  end;
               exception
                  when Socket_Error =>
                     exit;
               end;
            end loop;
         end;

         --  Close socket
         Close_Socket (Socket);
         Success := True;

      exception
         when E : Socket_Error =>
            Error := (Code => RPC_Network_Error,
                     Message => Empty_String);
            begin
               Close_Socket (Socket);
            exception
               when others => null;
            end;
         when E : others =>
            Error := (Code => RPC_Unknown_Error,
                     Message => Empty_String);
            begin
               Close_Socket (Socket);
            exception
               when others => null;
            end;
      end;
   end HTTP_POST;

   ---------------------------------------------------------------------------
   --  Endpoint Parsing
   ---------------------------------------------------------------------------

   function Parse_Endpoint (URL : String) return RPC_Endpoint is
      Result : RPC_Endpoint := Default_Endpoint;
      Start_Pos : Natural := URL'First;
      Colon_Pos : Natural;
      Slash_Pos : Natural;
      Host_Start : Natural;
      Host_End : Natural;
   begin
      --  Parse protocol
      if URL'Length >= 8 and then URL (URL'First .. URL'First + 7) = "https://" then
         Result.Use_TLS := True;
         Start_Pos := URL'First + 8;
      elsif URL'Length >= 7 and then URL (URL'First .. URL'First + 6) = "http://" then
         Result.Use_TLS := False;
         Start_Pos := URL'First + 7;
      end if;

      --  Find host and port
      Host_Start := Start_Pos;
      Colon_Pos := Index (URL (Start_Pos .. URL'Last), ":");
      Slash_Pos := Index (URL (Start_Pos .. URL'Last), "/");

      if Colon_Pos > 0 and (Slash_Pos = 0 or Colon_Pos < Slash_Pos) then
         --  Port specified
         Host_End := Colon_Pos - 1;
         declare
            Port_Start : constant Natural := Colon_Pos + 1;
            Port_End : Natural := (if Slash_Pos > 0 then Slash_Pos - 1 else URL'Last);
            Port_Str : constant String := URL (Port_Start .. Port_End);
         begin
            begin
               Result.Port := Natural'Value (Port_Str);
            exception
               when others =>
                  Result.Port := 8545;
            end;
         end;
      elsif Slash_Pos > 0 then
         Host_End := Slash_Pos - 1;
      else
         Host_End := URL'Last;
      end if;

      --  Copy host
      declare
         Host_Str : constant String := URL (Host_Start .. Host_End);
         Len : constant Natural := Natural'Min (Host_Str'Length, 32);
      begin
         for I in 0 .. Len - 1 loop
            Result.Host.Data (I) := Host_Str (Host_Str'First + I);
         end loop;
         Result.Host.Length := Len;
      end;

      return Result;
   end Parse_Endpoint;

   ---------------------------------------------------------------------------
   --  RPC Methods Implementation
   ---------------------------------------------------------------------------

   procedure Send_Raw_Transaction (
      Endpoint    : in     RPC_Endpoint;
      Signed_Tx   : in     Byte_Array;
      Tx_Hash     : out    Hash256;
      Error       : out    RPC_Error;
      Success     : out    Boolean
   ) is
      Hex_Tx : constant String := To_Hex_String (Signed_Tx);
      JSON_Request : constant String := Build_JSON_RPC_Request (
         "anubis_sendRawTransaction",
         """" & Hex_Tx & """");
      Response : Unbounded_String;
      RPC_Success : Boolean;
   begin
      Success := False;
      Tx_Hash := (others => 0);
      Error := No_Error;

      HTTP_POST (Endpoint, JSON_Request, Response, Error, RPC_Success);

      if not RPC_Success then
         return;
      end if;

      declare
         JSON : constant String := To_String (Response);
      begin
         if Has_JSON_Error (JSON) then
            Error := Parse_RPC_Error (JSON);
            return;
         end if;

         --  Extract result (transaction hash)
         declare
            Hash_Hex : constant String := Extract_JSON_String (JSON, "result");
            Hash_Bytes : Byte_Array (0 .. 31);
            Hash_Size : Natural;
            Parse_Success : Boolean;
         begin
            Parse_Hex (Hash_Hex, Hash_Bytes, Hash_Size, Parse_Success);
            if Parse_Success and Hash_Size = 32 then
               for I in Hash_Bytes'Range loop
                  Tx_Hash (I) := Hash_Bytes (I);
               end loop;
               Success := True;
            else
               Error := (Code => RPC_Parse_Error, Message => Empty_String);
            end if;
         end;
      end;
   end Send_Raw_Transaction;

   procedure Get_Transaction_Receipt (
      Endpoint : in     RPC_Endpoint;
      Tx_Hash  : in     Hash256;
      Receipt  : out    Transaction_Receipt;
      Found    : out    Boolean;
      Error    : out    RPC_Error;
      Success  : out    Boolean
   ) is
      Hash_Hex : constant String := Hash_To_Hex (Tx_Hash);
      JSON_Request : constant String := Build_JSON_RPC_Request (
         "anubis_getTransactionReceipt",
         """" & Hash_Hex & """");
      Response : Unbounded_String;
      RPC_Success : Boolean;
   begin
      Success := False;
      Found := False;
      Error := No_Error;
      Receipt := (
         Transaction_Hash    => (others => 0),
         Block_Number        => 0,
         Block_Hash          => (others => 0),
         Contract_Address    => (others => 0),
         Gas_Used            => 0,
         Effective_Gas_Price => 0,
         Status              => False,
         Cumulative_Gas_Used => 0,
         Logs_Count          => 0
      );

      HTTP_POST (Endpoint, JSON_Request, Response, Error, RPC_Success);

      if not RPC_Success then
         return;
      end if;

      declare
         JSON : constant String := To_String (Response);
      begin
         if Has_JSON_Error (JSON) then
            Error := Parse_RPC_Error (JSON);
            return;
         end if;

         --  Check if result is null (transaction not mined yet)
         if Index (JSON, """result"":null") > 0 then
            Success := True;
            Found := False;
            return;
         end if;

         --  Parse receipt fields
         Receipt.Transaction_Hash := Tx_Hash;
         Receipt.Block_Number := Extract_JSON_Number (JSON, "blockNumber");
         Receipt.Gas_Used := Extract_JSON_Number (JSON, "gasUsed");
         Receipt.Effective_Gas_Price := Extract_JSON_Number (JSON, "effectiveGasPrice");
         Receipt.Status := Extract_JSON_Boolean (JSON, "status");
         Receipt.Cumulative_Gas_Used := Extract_JSON_Number (JSON, "cumulativeGasUsed");

         --  Parse contract address if present
         declare
            Addr_Hex : constant String := Extract_JSON_String (JSON, "contractAddress");
            Addr_Bytes : Byte_Array (0 .. 31);
            Addr_Size : Natural;
            Parse_Success : Boolean;
         begin
            if Addr_Hex'Length > 0 then
               Parse_Hex (Addr_Hex, Addr_Bytes, Addr_Size, Parse_Success);
               if Parse_Success then
                  for I in 0 .. Natural'Min (Addr_Size - 1, 31) loop
                     Receipt.Contract_Address (I) := Addr_Bytes (I);
                  end loop;
               end if;
            end if;
         end;

         --  Parse block hash
         declare
            Block_Hex : constant String := Extract_JSON_String (JSON, "blockHash");
            Block_Bytes : Byte_Array (0 .. 31);
            Block_Size : Natural;
            Parse_Success : Boolean;
         begin
            if Block_Hex'Length > 0 then
               Parse_Hex (Block_Hex, Block_Bytes, Block_Size, Parse_Success);
               if Parse_Success and Block_Size = 32 then
                  for I in Block_Bytes'Range loop
                     Receipt.Block_Hash (I) := Block_Bytes (I);
                  end loop;
               end if;
            end if;
         end;

         Success := True;
         Found := True;
      end;
   end Get_Transaction_Receipt;

   procedure Get_Code (
      Endpoint      : in     RPC_Endpoint;
      Contract_Addr : in     Khepri_Types.Address;
      Code          : out    Byte_Array;
      Code_Size     : out    Natural;
      Error         : out    RPC_Error;
      Success       : out    Boolean
   ) is
      Addr_Hex : constant String := Address_To_Hex (Contract_Addr);
      JSON_Request : constant String := Build_JSON_RPC_Request (
         "anubis_getCode",
         """" & Addr_Hex & """,""latest""");
      Response : Unbounded_String;
      RPC_Success : Boolean;
   begin
      Success := False;
      Code := (others => 0);
      Code_Size := 0;
      Error := No_Error;

      HTTP_POST (Endpoint, JSON_Request, Response, Error, RPC_Success);

      if not RPC_Success then
         return;
      end if;

      declare
         JSON : constant String := To_String (Response);
      begin
         if Has_JSON_Error (JSON) then
            Error := Parse_RPC_Error (JSON);
            return;
         end if;

         --  Extract code hex
         declare
            Code_Hex : constant String := Extract_JSON_String (JSON, "result");
            Parse_Success : Boolean;
         begin
            Parse_Hex (Code_Hex, Code, Code_Size, Parse_Success);
            Success := Parse_Success;
         end;
      end;
   end Get_Code;

   procedure Get_Block_Number (
      Endpoint     : in     RPC_Endpoint;
      Block_Number : out    Word64;
      Error        : out    RPC_Error;
      Success      : out    Boolean
   ) is
      JSON_Request : constant String := Build_JSON_RPC_Request (
         "anubis_blockNumber", "");
      Response : Unbounded_String;
      RPC_Success : Boolean;
   begin
      Success := False;
      Block_Number := 0;
      Error := No_Error;

      HTTP_POST (Endpoint, JSON_Request, Response, Error, RPC_Success);

      if not RPC_Success then
         return;
      end if;

      declare
         JSON : constant String := To_String (Response);
      begin
         if Has_JSON_Error (JSON) then
            Error := Parse_RPC_Error (JSON);
            return;
         end if;

         Block_Number := Extract_JSON_Number (JSON, "result");
         Success := True;
      end;
   end Get_Block_Number;

   procedure Get_Balance (
      Endpoint : in     RPC_Endpoint;
      Address  : in     Khepri_Types.Address;
      Balance  : out    Uint256;
      Error    : out    RPC_Error;
      Success  : out    Boolean
   ) is
      Addr_Hex : constant String := Address_To_Hex (Address);
      JSON_Request : constant String := Build_JSON_RPC_Request (
         "anubis_getBalance",
         """" & Addr_Hex & """,""latest""");
      Response : Unbounded_String;
      RPC_Success : Boolean;
   begin
      Success := False;
      Balance := (Limbs => (0, 0, 0, 0));
      Error := No_Error;

      HTTP_POST (Endpoint, JSON_Request, Response, Error, RPC_Success);

      if not RPC_Success then
         return;
      end if;

      declare
         JSON : constant String := To_String (Response);
      begin
         if Has_JSON_Error (JSON) then
            Error := Parse_RPC_Error (JSON);
            return;
         end if;

         --  Parse balance (hex string)
         declare
            Balance_Hex : constant String := Extract_JSON_String (JSON, "result");
            Balance_Bytes : Byte_Array (0 .. 31) := (others => 0);
            Balance_Size : Natural;
            Parse_Success : Boolean;
         begin
            Parse_Hex (Balance_Hex, Balance_Bytes, Balance_Size, Parse_Success);
            if Parse_Success then
               --  Convert bytes to U256 (big-endian to little-endian limbs)
               for I in 0 .. 3 loop
                  Balance.Limbs (I) := 0;
                  for J in 0 .. 7 loop
                     declare
                        Byte_Idx : constant Natural := 31 - (I * 8 + J);
                     begin
                        if Byte_Idx < Balance_Size then
                           Balance.Limbs (I) := Balance.Limbs (I) or
                              Shift_Left (Word64 (Balance_Bytes (Byte_Idx)), J * 8);
                        end if;
                     end;
                  end loop;
               end loop;
               Success := True;
            end if;
         end;
      end;
   end Get_Balance;

   ---------------------------------------------------------------------------
   --  Polling Helpers
   ---------------------------------------------------------------------------

   procedure Poll_Transaction_Receipt (
      Endpoint              : in     RPC_Endpoint;
      Tx_Hash               : in     Hash256;
      Max_Attempts          : in     Natural;
      Poll_Interval_Seconds : in     Natural;
      Receipt               : out    Transaction_Receipt;
      Found                 : out    Boolean;
      Error                 : out    RPC_Error;
      Success               : out    Boolean
   ) is
      RPC_Success : Boolean;
   begin
      Success := False;
      Found := False;
      Error := No_Error;

      for Attempt in 1 .. Max_Attempts loop
         Get_Transaction_Receipt (
            Endpoint => Endpoint,
            Tx_Hash  => Tx_Hash,
            Receipt  => Receipt,
            Found    => Found,
            Error    => Error,
            Success  => RPC_Success
         );

         if not RPC_Success then
            Success := False;
            return;
         end if;

         if Found then
            Success := True;
            return;
         end if;

         --  Wait before next attempt
         if Attempt < Max_Attempts then
            delay Duration (Poll_Interval_Seconds);
         end if;
      end loop;

      --  Timeout
      Success := True;
      Found := False;
      Error := (Code => RPC_Timeout_Error, Message => Empty_String);
   end Poll_Transaction_Receipt;

   procedure Wait_Confirmations (
      Endpoint         : in     RPC_Endpoint;
      Tx_Hash          : in     Hash256;
      Confirmations    : in     Natural;
      Max_Wait_Seconds : in     Natural;
      Receipt          : out    Transaction_Receipt;
      Error            : out    RPC_Error;
      Success          : out    Boolean
   ) is
      use Ada.Real_Time;
      Found : Boolean;
      Current_Block : Word64;
      Start_Time : constant Time := Clock;
   begin
      Success := False;
      Error := No_Error;

      --  First, get the receipt
      Poll_Transaction_Receipt (
         Endpoint              => Endpoint,
         Tx_Hash               => Tx_Hash,
         Max_Attempts          => Max_Wait_Seconds / 2,
         Poll_Interval_Seconds => 2,
         Receipt               => Receipt,
         Found                 => Found,
         Error                 => Error,
         Success               => Success
      );

      if not Success or not Found then
         return;
      end if;

      --  Now wait for confirmations
      loop
         declare
            Block_Success : Boolean;
         begin
            Get_Block_Number (Endpoint, Current_Block, Error, Block_Success);

            if not Block_Success then
               Success := False;
               return;
            end if;

            if Current_Block >= Receipt.Block_Number + Word64 (Confirmations) then
               Success := True;
               return;
            end if;
         end;

         --  Check timeout
         if To_Duration (Clock - Start_Time) > Duration (Max_Wait_Seconds) then
            Error := (Code => RPC_Timeout_Error, Message => Empty_String);
            Success := False;
            return;
         end if;

         delay 2.0;
      end loop;
   end Wait_Confirmations;

end Khepri_RPC_Client;
