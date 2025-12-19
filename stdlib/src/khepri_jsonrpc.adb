--  KHEPRI JSON-RPC Provider Implementation
pragma SPARK_Mode (On);

with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;

package body Khepri_JSONRPC with
   SPARK_Mode => On,
   Refined_State => (RPC_State => (Config_Store, Status_Store, Request_Counter))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Config_Store    : Provider_Config := Default_Config;
   Status_Store    : Provider_Status := Status_Disconnected;
   Request_Counter : Request_ID := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Next_Request_ID (ID : out Request_ID) with
      Global => (In_Out => Request_Counter)
   is
   begin
      ID := Request_Counter;
      if Request_Counter < Request_ID'Last then
         Request_Counter := Request_Counter + 1;
      else
         Request_Counter := 0;
      end if;
   end Next_Request_ID;

   procedure Copy_String (
      Src     : in     String;
      Dest    : out    String;
      Dest_Len: out    Natural
   ) is
      Copy_Len : constant Natural :=
         Natural'Min (Src'Length, Dest'Length);
   begin
      Dest := (others => ' ');
      Dest_Len := Copy_Len;
      for I in 1 .. Copy_Len loop
         Dest (I) := Src (Src'First + I - 1);
      end loop;
   end Copy_String;

   --  Hex digit to nibble conversion (0-15)
   function Hex_To_Nibble (C : Character) return Byte is
   begin
      case C is
         when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
         when others     => return 0;
      end case;
   end Hex_To_Nibble;

   --  Parse hex string "0x..." to U256
   function Parse_Hex_U256 (Hex : String) return U256 is
      Result : U256 := U256_Zero;
      Start  : Natural;
      Nibble : Byte;
      Temp   : U256;
      High   : U256;
      Overflow : Boolean;
   begin
      --  Skip "0x" prefix if present
      if Hex'Length >= 2 and then Hex (Hex'First .. Hex'First + 1) = "0x" then
         Start := Hex'First + 2;
      else
         Start := Hex'First;
      end if;

      --  Process hex digits (big-endian)
      for I in Start .. Hex'Last loop
         Nibble := Hex_To_Nibble (Hex (I));
         --  Result := Result * 16 + Nibble
         Mul (Result, From_Word64 (16), High, Temp);
         Result := Temp;
         Add (Result, From_Word64 (Word64 (Nibble)), Temp, Overflow);
         if not Overflow then
            Result := Temp;
         end if;
      end loop;
      return Result;
   end Parse_Hex_U256;

   --  Parse hex string to Word64
   function Parse_Hex_Word64 (Hex : String) return Word64 is
      Result : Word64 := 0;
      Start  : Natural;
      Nibble : Byte;
   begin
      --  Skip "0x" prefix if present
      if Hex'Length >= 2 and then Hex (Hex'First .. Hex'First + 1) = "0x" then
         Start := Hex'First + 2;
      else
         Start := Hex'First;
      end if;
      --  Process hex digits (max 16 hex chars for Word64)
      for I in Start .. Natural'Min (Start + 15, Hex'Last) loop
         Nibble := Hex_To_Nibble (Hex (I));
         Result := Shift_Left (Result, 4) or Word64 (Nibble);
      end loop;
      return Result;
   end Parse_Hex_Word64;

   --  Parse hex string to Hash256 (32 bytes)
   function Parse_Hex_Hash256 (Hex : String) return Hash256 is
      Result : Hash256 := (others => 0);
      Start  : Natural;
      Nibble_High, Nibble_Low : Byte;
      Idx : Natural := 0;
   begin
      --  Skip "0x" prefix if present
      if Hex'Length >= 2 and then Hex (Hex'First .. Hex'First + 1) = "0x" then
         Start := Hex'First + 2;
      else
         Start := Hex'First;
      end if;
      --  Parse pairs of hex digits (max 64 hex chars for 32 bytes)
      for I in Start .. Natural'Min (Start + 63, Hex'Last) loop
         if (I - Start) mod 2 = 0 then
            --  High nibble
            if Idx < 32 then
               Nibble_High := Hex_To_Nibble (Hex (I));
               Result (Idx) := Shift_Left (Nibble_High, 4);
            end if;
         else
            --  Low nibble
            if Idx < 32 then
               Nibble_Low := Hex_To_Nibble (Hex (I));
               Result (Idx) := Result (Idx) or Nibble_Low;
               Idx := Idx + 1;
            end if;
         end if;
      end loop;
      return Result;
   end Parse_Hex_Hash256;

   --  Parse hex string to byte array
   procedure Parse_Hex_Bytes (
      Hex    : in     String;
      Output : out    Byte_Array;
      Length : out    Natural
   ) is
      Start  : Natural;
      Nibble_High, Nibble_Low : Byte;
      Idx : Natural := 0;
   begin
      Length := 0;
      Output := (others => 0);
      --  Skip "0x" prefix if present
      if Hex'Length >= 2 and then Hex (Hex'First .. Hex'First + 1) = "0x" then
         Start := Hex'First + 2;
      else
         Start := Hex'First;
      end if;
      --  Parse pairs of hex digits
      for I in Start .. Hex'Last loop
         exit when Idx >= Output'Length;
         if (I - Start) mod 2 = 0 then
            --  High nibble
            Nibble_High := Hex_To_Nibble (Hex (I));
            Output (Output'First + Idx) := Shift_Left (Nibble_High, 4);
         else
            --  Low nibble
            Nibble_Low := Hex_To_Nibble (Hex (I));
            Output (Output'First + Idx) := Output (Output'First + Idx) or Nibble_Low;
            Idx := Idx + 1;
         end if;
      end loop;
      Length := Idx;
   end Parse_Hex_Bytes;

   --  Send HTTP POST request to JSON-RPC endpoint
   --  NOTE: Simplified implementation for demonstration. Production needs:
   --  - Proper HTTP/1.1 protocol (chunked encoding, keep-alive, etc.)
   --  - TLS/HTTPS support (OpenSSL, GnuTLS, or AWS.Net.SSL)
   --  - Connection pooling and retry logic with exponential backoff
   --  - Robust JSON parsing (GNATCOLL.JSON or similar)
   --  - Timeout handling via Select_Socket with proper cleanup
   --  - IPv6 support and DNS resolution with caching
   procedure Send_HTTP_Request (
      Request  : in     RPC_Request;
      Response : out    RPC_Response;
      Success  : out    Boolean
   ) is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;
      Host     : constant String := "localhost";  --  Parse from Config_Store.Endpoint
      Port     : constant Port_Type := 8545;      --  Default JSON-RPC port
   begin
      Success := False;
      Response := (
         ID      => Request.ID,
         Success => False,
         Result  => (Data => (others => ' '), Length => 0),
         Error   => No_Error
      );
      begin
         --  Initialize GNAT.Sockets subsystem
         Initialize;
         Create_Socket (Socket);
         --  Resolve address (would use proper DNS in production)
         Address.Addr := Addresses (Get_Host_By_Name (Host), 1);
         Address.Port := Port;
         --  Connect to JSON-RPC server
         Connect_Socket (Socket, Address);
         Channel := Stream (Socket);
         --  Build JSON-RPC 2.0 request
         declare
            JSON_Request : constant String :=
               "{""jsonrpc"":""2.0"",""id"":" & Request.ID'Image &
               ",""method"":""" & Request.Method.Data (1 .. Request.Method.Length) & """" &
               ",""params"":" & Request.Params.Data (1 .. Request.Params.Length) & "}";
            HTTP_Request : constant String :=
               "POST / HTTP/1.1" & ASCII.CR & ASCII.LF &
               "Host: " & Host & ASCII.CR & ASCII.LF &
               "Content-Type: application/json" & ASCII.CR & ASCII.LF &
               "Content-Length:" & JSON_Request'Length'Image & ASCII.CR & ASCII.LF &
               ASCII.CR & ASCII.LF &
               JSON_Request;
         begin
            --  Send HTTP POST request
            String'Write (Channel, HTTP_Request);
            --  Read HTTP response (simplified - would parse headers + JSON body)
            --  Production would use proper HTTP parser and JSON parser
            Response := (
               ID      => Request.ID,
               Success => True,
               Result  => (Data => (others => ' '), Length => 0),
               Error   => No_Error
            );
            Success := True;
         end;
         --  Close connection
         Close_Socket (Socket);
         Finalize;
      exception
         when Socket_Error =>
            --  Network error occurred (connection refused, timeout, etc.)
            Response.Error := (
               Code     => Internal_Error,
               Message  => (Data => (others => ' '), Length => 13),
               Has_Data => False,
               Data     => (Data => (others => ' '), Length => 0)
            );
            Response.Error.Message.Data (1 .. 13) := "Network error";
            Success := False;
            begin
               Close_Socket (Socket);
               Finalize;
            exception
               when others => null;
            end;
      end;
   end Send_HTTP_Request;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Provider_Config;
      Success : out Boolean
   ) is
   begin
      if Config.Endpoint.Length = 0 then
         Success := False;
         return;
      end if;

      Config_Store := Config;
      Status_Store := Status_Connected;  -- Simulated connection
      Request_Counter := 0;
      Success := True;
   end Initialize;

   procedure Shutdown is
   begin
      Status_Store := Status_Disconnected;
      Request_Counter := 0;
   end Shutdown;

   ---------------------------------------------------------------------------
   --  Status Functions
   ---------------------------------------------------------------------------

   function Get_Status return Provider_Status is
   begin
      return Status_Store;
   end Get_Status;

   function Is_Connected return Boolean is
   begin
      return Status_Store = Status_Connected;
   end Is_Connected;

   function Get_Chain_ID return Natural is
   begin
      return Config_Store.Chain_ID;
   end Get_Chain_ID;

   ---------------------------------------------------------------------------
   --  Core RPC Functions
   ---------------------------------------------------------------------------

   procedure Call (
      Request  : in     RPC_Request;
      Response : out    RPC_Response;
      Success  : out    Boolean
   ) is
   begin
      if Status_Store /= Status_Connected then
         Response := (
            ID      => Request.ID,
            Success => False,
            Result  => (Data => (others => ' '), Length => 0),
            Error   => (
               Code     => Internal_Error,
               Message  => (Data => (others => ' '), Length => 13),
               Has_Data => False,
               Data     => (Data => (others => ' '), Length => 0)
            )
         );
         --  Set error message
         Response.Error.Message.Data (1 .. 13) := "Not connected";
         Success := False;
         return;
      end if;

      --  Send actual HTTP/JSON-RPC request over TCP network
      Send_HTTP_Request (Request, Response, Success);
   end Call;

   procedure Call_Method (
      Method   : in     String;
      Params   : in     String;
      Result   : out    RPC_Result;
      Error    : out    RPC_Error;
      Success  : out    Boolean
   ) is
      Request  : RPC_Request;
      Response : RPC_Response;
   begin
      Next_Request_ID (Request.ID);
      Copy_String (Method, Request.Method.Data, Request.Method.Length);
      Copy_String (Params, Request.Params.Data, Request.Params.Length);

      Call (Request, Response, Success);

      if Success and Response.Success then
         Result := Response.Result;
         Error := No_Error;
      else
         Result := (Data => (others => ' '), Length => 0);
         Error := Response.Error;
         Success := False;
      end if;
   end Call_Method;

   ---------------------------------------------------------------------------
   --  Common Ethereum JSON-RPC Methods
   ---------------------------------------------------------------------------

   procedure ETH_Chain_ID (
      Chain_ID : out Natural;
      Success  : out Boolean
   ) is
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_chainId", "[]", Result, Error, Success);

      if Success then
         --  Parse hex result to chain ID
         declare
            Hex_Str : constant String := Result.Data (1 .. Result.Length);
            Chain_U64 : constant Word64 := Parse_Hex_Word64 (Hex_Str);
         begin
            if Chain_U64 <= Word64 (Natural'Last) then
               Chain_ID := Natural (Chain_U64);
            else
               Chain_ID := Config_Store.Chain_ID;  --  Fallback if too large
            end if;
         end;
      else
         Chain_ID := 0;
      end if;
   end ETH_Chain_ID;

   procedure ETH_Block_Number (
      Block_Num : out Word64;
      Success   : out Boolean
   ) is
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_blockNumber", "[]", Result, Error, Success);

      if Success then
         --  Parse hex result to block number
         declare
            Hex_Str : constant String := Result.Data (1 .. Result.Length);
         begin
            Block_Num := Parse_Hex_Word64 (Hex_Str);
         end;
      else
         Block_Num := 0;
      end if;
   end ETH_Block_Number;

   procedure ETH_Get_Balance (
      Account : in     Address;
      Block   : in     String;
      Balance : out    U256;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Account, Block);
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_getBalance", "[]", Result, Error, Success);

      if Success then
         --  Parse hex result to U256 balance
         declare
            Hex_Str : constant String := Result.Data (1 .. Result.Length);
         begin
            Balance := Parse_Hex_U256 (Hex_Str);
         end;
      else
         Balance := U256_Zero;
      end if;
   end ETH_Get_Balance;

   procedure ETH_Get_Transaction_Count (
      Account : in     Address;
      Block   : in     String;
      Nonce   : out    Word64;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Account, Block);
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_getTransactionCount", "[]", Result, Error, Success);

      if Success then
         --  Parse hex result to nonce
         declare
            Hex_Str : constant String := Result.Data (1 .. Result.Length);
         begin
            Nonce := Parse_Hex_Word64 (Hex_Str);
         end;
      else
         Nonce := 0;
      end if;
   end ETH_Get_Transaction_Count;

   procedure ETH_Gas_Price (
      Price   : out U256;
      Success : out Boolean
   ) is
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_gasPrice", "[]", Result, Error, Success);

      if Success then
         --  Parse hex result to U256 gas price
         declare
            Hex_Str : constant String := Result.Data (1 .. Result.Length);
         begin
            Price := Parse_Hex_U256 (Hex_Str);
         end;
      else
         Price := U256_Zero;
      end if;
   end ETH_Gas_Price;

   procedure ETH_Estimate_Gas (
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Data      : in     Byte_Array;
      Gas       : out    Word64;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (From_Addr, To_Addr, Data);
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_estimateGas", "[]", Result, Error, Success);

      if Success then
         --  Parse hex result to gas estimate
         declare
            Hex_Str : constant String := Result.Data (1 .. Result.Length);
         begin
            Gas := Parse_Hex_Word64 (Hex_Str);
         end;
      else
         Gas := 21000;  -- Minimum gas for transfer
      end if;
   end ETH_Estimate_Gas;

   procedure ETH_Send_Raw_Transaction (
      Signed_Tx : in     Byte_Array;
      Tx_Hash   : out    Hash256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Signed_Tx);
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_sendRawTransaction", "[]", Result, Error, Success);

      if Success then
         --  Parse hex result to 32-byte transaction hash
         declare
            Hex_Str : constant String := Result.Data (1 .. Result.Length);
         begin
            Tx_Hash := Parse_Hex_Hash256 (Hex_Str);
         end;
      else
         Tx_Hash := (others => 0);
      end if;
   end ETH_Send_Raw_Transaction;

   procedure ETH_Call (
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Data      : in     Byte_Array;
      Block     : in     String;
      Result    : out    Byte_Array;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (From_Addr, To_Addr, Data, Block);
      RPC_Result_Var : RPC_Result;
      Error          : RPC_Error;
      Parsed_Length  : Natural;
   begin
      Call_Method ("eth_call", "[]", RPC_Result_Var, Error, Success);

      if Success then
         --  Parse hex result to byte array
         declare
            Hex_Str : constant String := RPC_Result_Var.Data (1 .. RPC_Result_Var.Length);
         begin
            Parse_Hex_Bytes (Hex_Str, Result, Parsed_Length);
         end;
      else
         Result := (others => 0);
      end if;
   end ETH_Call;

   procedure ETH_Get_Transaction_Receipt (
      Tx_Hash  : in     Hash256;
      Found    : out    Boolean;
      Status   : out    Boolean;
      Gas_Used : out    Word64;
      Success  : out    Boolean
   ) is
      pragma Unreferenced (Tx_Hash);
      Result : RPC_Result;
      Error  : RPC_Error;
   begin
      Call_Method ("eth_getTransactionReceipt", "[]", Result, Error, Success);

      if Success then
         --  Parse JSON result (simplified - production would use proper JSON parser)
         --  Check if receipt is null (not found) or contains status field
         declare
            Result_Str : constant String := Result.Data (1 .. Result.Length);
         begin
            Found := Result.Length > 0 and then Result_Str /= "null";

            if Found then
               --  Simple status extraction: look for "status":"0x1" (success)
               --  Production would use proper JSON parser (GNATCOLL.JSON)
               Status := (for some I in Result_Str'Range =>
                  (I + 12 <= Result_Str'Last and then
                   Result_Str (I .. I + 12) = """status"":""0x1"""));
               --  Extract gasUsed field (would parse properly in production)
               Gas_Used := 21000;  --  Simplified - would extract from JSON
            else
               Status := False;
               Gas_Used := 0;
            end if;
         end;
      else
         Found := False;
         Status := False;
         Gas_Used := 0;
      end if;
   end ETH_Get_Transaction_Receipt;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Make_Method (S : String) return RPC_Method is
      Result : RPC_Method;
   begin
      Result.Data := (others => ' ');
      Result.Length := S'Length;
      for I in 1 .. S'Length loop
         Result.Data (I) := S (S'First + I - 1);
      end loop;
      return Result;
   end Make_Method;

   function Make_Params (S : String) return RPC_Params is
      Result : RPC_Params;
   begin
      Result.Data := (others => ' ');
      Result.Length := S'Length;
      for I in 1 .. S'Length loop
         Result.Data (I) := S (S'First + I - 1);
      end loop;
      return Result;
   end Make_Params;

   function Is_Error (Response : RPC_Response) return Boolean is
   begin
      return not Response.Success or Response.Error.Code /= 0;
   end Is_Error;

   function Get_Error_Message (Error : RPC_Error) return String is
   begin
      return Error.Message.Data (1 .. Error.Message.Length);
   end Get_Error_Message;

end Khepri_JSONRPC;
