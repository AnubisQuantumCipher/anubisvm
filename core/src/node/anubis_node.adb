pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types;
with Anubis_SHA3;

package body Anubis_Node with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper Procedures
   ---------------------------------------------------------------------------

   --  Set error message in response (handles string length properly)
   procedure Set_Error_Msg (
      Response : in Out RPC_Response;
      Msg      : String
   ) with
      Global => null,
      Pre    => Msg'Length <= Max_Error_Message_Length
   is
      Len : constant Natural := Msg'Length;
   begin
      Response.Error_Msg := (others => ' ');
      for I in 1 .. Len loop
         Response.Error_Msg (I) := Msg (Msg'First + I - 1);
      end loop;
      Response.Error_Msg_Len := Len;
   end Set_Error_Msg;

   ---------------------------------------------------------------------------
   --  VM Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      VM     : in Out VM_Instance;
      Config : in     Node_Configuration
   ) is
   begin
      VM.Is_Initialized := True;
      VM.Config := Config;
      VM.State := Initial_State;
      VM.State.Status := Status_Running;
      VM.Contract_Count := 0;

      --  Initialize contract registry
      for I in Contract_Index loop
         VM.Contracts (I) := (
            Address   => Address_Zero,
            Code_Hash => Hash256_Zero,
            Is_Loaded => False
         );
      end loop;
   end Initialize;

   procedure Shutdown (
      VM : in Out VM_Instance
   ) is
   begin
      VM.State.Status := Status_Stopping;

      --  Clear contract registry
      for I in Contract_Index loop
         VM.Contracts (I).Is_Loaded := False;
      end loop;
      VM.Contract_Count := 0;

      VM.State.Status := Status_Stopped;
      VM.Is_Initialized := False;
   end Shutdown;

   ---------------------------------------------------------------------------
   --  Contract Loading
   ---------------------------------------------------------------------------

   procedure Load_Contract (
      VM        : in Out VM_Instance;
      Path      : in     Path_String;
      Path_Len  : in     Natural;
      Address   : out    Contract_Address;
      Success   : out    Boolean
   ) is
      --  Contract slot index
      Slot : Contract_Index;
      Code_Hash  : Hash256;
   begin
      Address := Address_Zero;
      Success := False;

      --  Check capacity
      if VM.Contract_Count >= Max_Contracts or Path_Len = 0 then
         return;
      end if;

      --  Hash the path to create a deterministic address
      --  In production, this would load the ELF and compute code hash
      declare
         Path_Bytes : Anubis_Types.Byte_Array (0 .. Path_Len - 1);
         Digest     : Anubis_SHA3.SHA3_256_Digest;
      begin
         --  Convert path to bytes for hashing
         for I in 0 .. Path_Len - 1 loop
            Path_Bytes (I) := Anubis_Types.Byte (Character'Pos (Path (I + 1)));
         end loop;

         Anubis_SHA3.SHA3_256 (Path_Bytes, Digest);

         --  Copy digest to code hash
         for I in 0 .. 31 loop
            Code_Hash (I) := Byte (Digest (I));
         end loop;
      end;

      --  Generate contract address from code hash
      for I in 0 .. 31 loop
         Address (I) := Code_Hash (I);
      end loop;

      --  Find empty slot
      Slot := Contract_Index (VM.Contract_Count);

      --  Register contract
      VM.Contracts (Slot) := (
         Address   => Address,
         Code_Hash => Code_Hash,
         Is_Loaded => True
      );

      VM.Contract_Count := VM.Contract_Count + 1;
      Success := True;
   end Load_Contract;

   function Is_Contract_Loaded (
      VM      : VM_Instance;
      Address : Contract_Address
   ) return Boolean is
   begin
      for I in 0 .. Contract_Index (VM.Contract_Count - 1) loop
         if VM.Contracts (I).Is_Loaded and then
            VM.Contracts (I).Address = Address then
            return True;
         end if;
      end loop;
      return False;
   end Is_Contract_Loaded;

   ---------------------------------------------------------------------------
   --  Request Processing
   ---------------------------------------------------------------------------

   procedure Process_Request (
      VM       : in Out VM_Instance;
      Request  : in     RPC_Request;
      Response : out    RPC_Response
   ) is
   begin
      --  Initialize response
      Response := Empty_Response;
      Response.ID := Request.ID;
      Response.ID_Len := Request.ID_Len;

      case Request.Method is
         when Method_Health =>
            --  Health check - return "ok"
            Response.Has_Result := True;
            Response.Result (0) := Character'Pos ('o');
            Response.Result (1) := Character'Pos ('k');
            Response.Result_Size := 2;

         when Method_GetBlockNumber =>
            --  Return latest block number as bytes (little-endian)
            Response.Has_Result := True;
            for I in 0 .. 31 loop
               declare
                  Limb_Idx : constant Natural := I / 8;
                  Byte_Idx : constant Natural := I mod 8;
                  Limb_Val : constant Word64 := VM.State.Latest_Block.Number.Limbs (Limb_Idx);
                  Shifted  : constant Word64 := Shift_Right (Limb_Val, Byte_Idx * 8);
               begin
                  Response.Result (I) := Byte (Shifted and 16#FF#);
               end;
            end loop;
            Response.Result_Size := 32;

         when Method_GetNodeInfo =>
            --  Return basic node info
            Response.Has_Result := True;
            --  TODO: Serialize node info to JSON
            Response.Result_Size := 0;

         when Method_Execute | Method_Query | Method_Deploy =>
            --  Legacy execution methods - redirect to VM methods
            Response.Has_Result := False;
            Response.Error_Code := Error_Invalid_Params;
            Set_Error_Msg (Response, "Use vm_deployContract or vm_invoke");

         when Method_VM_Deploy =>
            --  Contract deployment - params should contain hex-encoded ELF
            --  For now, return "ready" to indicate the endpoint exists
            Response.Has_Result := True;
            Response.Result (0) := Character'Pos ('r');
            Response.Result (1) := Character'Pos ('e');
            Response.Result (2) := Character'Pos ('a');
            Response.Result (3) := Character'Pos ('d');
            Response.Result (4) := Character'Pos ('y');
            Response.Result_Size := 5;

         when Method_VM_Invoke =>
            --  Contract invocation - params should contain to, entry, args
            Response.Has_Result := True;
            Response.Result (0) := Character'Pos ('r');
            Response.Result (1) := Character'Pos ('e');
            Response.Result (2) := Character'Pos ('a');
            Response.Result (3) := Character'Pos ('d');
            Response.Result (4) := Character'Pos ('y');
            Response.Result_Size := 5;

         when Method_VM_Call =>
            --  Read-only contract call
            Response.Has_Result := True;
            Response.Result (0) := Character'Pos ('r');
            Response.Result (1) := Character'Pos ('e');
            Response.Result (2) := Character'Pos ('a');
            Response.Result (3) := Character'Pos ('d');
            Response.Result (4) := Character'Pos ('y');
            Response.Result_Size := 5;

         when Method_VM_GetState =>
            --  Raw state query
            Response.Has_Result := False;
            Response.Error_Code := Error_Invalid_Params;
            Set_Error_Msg (Response, "Params required");

         when Method_GetBalance | Method_GetNonce |
              Method_GetCode | Method_GetStorage =>
            --  State queries
            Response.Has_Result := False;
            Response.Error_Code := Error_Invalid_Params;
            Set_Error_Msg (Response, "Not implemented");

         when Method_GetBlockByNumber | Method_GetProof =>
            --  Block/proof queries
            Response.Has_Result := False;
            Response.Error_Code := Error_Invalid_Params;
            Set_Error_Msg (Response, "Not implemented");

         when Method_Unknown =>
            Response.Has_Result := False;
            Response.Error_Code := Error_Method_Not_Found;
            Set_Error_Msg (Response, "Unknown method");
      end case;
   end Process_Request;

   procedure Execute_Call (
      VM        : in Out VM_Instance;
      From      : in     Contract_Address;
      To        : in     Contract_Address;
      Value     : in     U256;
      Gas_Limit : in     Gas_Amount;
      Data      : in     Params_Buffer;
      Data_Size : in     Natural;
      Result    : out    Execution_Result
   ) is
      pragma Unreferenced (From, To, Value, Data, Data_Size);
   begin
      --  Basic execution stub
      --  In production, this would:
      --  1. Create execution context
      --  2. Load contract code
      --  3. Execute with gas metering
      --  4. Return results

      Result := (
         Status      => Success,
         Gas_Used    => Gas_Limit / 10,  -- Placeholder
         Return_Data => Hash256_Zero
      );
   end Execute_Call;

   procedure Query_State (
      VM        : in     VM_Instance;
      Address   : in     Contract_Address;
      Key       : in     Storage_Key;
      Value     : out    Storage_Value;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Address, Key);
   begin
      --  State query stub
      --  In production, this would query Khepri MPT
      if VM.Is_Initialized then
         Value := Storage_Value (U256_Zero);
         Success := True;
      else
         Value := Storage_Value (U256_Zero);
         Success := False;
      end if;
   end Query_State;

   ---------------------------------------------------------------------------
   --  State Access
   ---------------------------------------------------------------------------

   function Get_Balance (
      VM      : VM_Instance;
      Address : Contract_Address
   ) return U256 is
      pragma Unreferenced (Address);
   begin
      --  Balance query stub
      if VM.Is_Initialized then
         return U256_Zero;
      else
         return U256_Zero;
      end if;
   end Get_Balance;

   function Get_Block_Number (VM : VM_Instance) return U256 is
   begin
      return VM.State.Latest_Block.Number;
   end Get_Block_Number;

   function Get_Status (VM : VM_Instance) return Node_Status is
   begin
      return VM.State.Status;
   end Get_Status;

   ---------------------------------------------------------------------------
   --  Method Dispatch
   ---------------------------------------------------------------------------

   function Parse_Method (
      Method_Str : Method_Name;
      Method_Len : Natural
   ) return RPC_Method is
      Str : constant String := Method_Str (1 .. Method_Len);
   begin
      --  Match method names to enum
      if Str = "execute" or Str = "eth_call" then
         return Method_Execute;
      elsif Str = "query" then
         return Method_Query;
      elsif Str = "deploy" or Str = "eth_sendTransaction" then
         return Method_Deploy;
      elsif Str = "getBalance" or Str = "eth_getBalance" then
         return Method_GetBalance;
      elsif Str = "getNonce" or Str = "eth_getTransactionCount" then
         return Method_GetNonce;
      elsif Str = "getCode" or Str = "eth_getCode" then
         return Method_GetCode;
      elsif Str = "getStorage" or Str = "eth_getStorageAt" then
         return Method_GetStorage;
      elsif Str = "getBlockNumber" or Str = "eth_blockNumber" then
         return Method_GetBlockNumber;
      elsif Str = "getBlockByNumber" or Str = "eth_getBlockByNumber" then
         return Method_GetBlockByNumber;
      elsif Str = "getProof" or Str = "eth_getProof" then
         return Method_GetProof;
      elsif Str = "getNodeInfo" or Str = "web3_clientVersion" then
         return Method_GetNodeInfo;
      elsif Str = "health" or Str = "net_version" then
         return Method_Health;
      elsif Str = "vm_deployContract" or Str = "vm_deploy" then
         return Method_VM_Deploy;
      elsif Str = "vm_invoke" then
         return Method_VM_Invoke;
      elsif Str = "vm_call" then
         return Method_VM_Call;
      elsif Str = "vm_getState" then
         return Method_VM_GetState;
      else
         return Method_Unknown;
      end if;
   end Parse_Method;

   ---------------------------------------------------------------------------
   --  Contract Deployment
   ---------------------------------------------------------------------------

   procedure Set_Deploy_Error (
      Result : in out Deploy_Result;
      Code   : RPC_Error_Code;
      Msg    : String
   ) with
      Global => null,
      Pre    => Msg'Length <= Max_Error_Message_Length
   is
      Len : constant Natural := Msg'Length;
   begin
      Result.Success := False;
      Result.Error_Code := Code;
      Result.Error_Msg := (others => ' ');
      for I in 1 .. Len loop
         Result.Error_Msg (I) := Msg (Msg'First + I - 1);
      end loop;
      Result.Error_Msg_Len := Len;
   end Set_Deploy_Error;

   procedure Deploy_Contract (
      VM       : in Out VM_Instance;
      From     : in     Contract_Address;
      Code     : in     Node_Code_Buffer;
      Code_Size : in    Natural;
      Manifest : in     Node_Contract_Manifest;
      Gas_Limit : in    Gas_Amount;
      Result   : out    Deploy_Result
   ) is
      pragma Unreferenced (From, Gas_Limit);
      Code_Hash    : Hash256;
      Contract_ID  : Contract_Address;
      Slot         : Contract_Index;
   begin
      Result := Empty_Deploy_Result;

      --  Hash the code to create contract ID
      declare
         Code_Bytes : Anubis_Types.Byte_Array (0 .. Code_Size - 1);
         Digest     : Anubis_SHA3.SHA3_256_Digest;
      begin
         --  Convert code buffer to byte array for hashing
         for I in 0 .. Code_Size - 1 loop
            Code_Bytes (I) := Anubis_Types.Byte (Code (Node_Code_Index (I)));
         end loop;

         Anubis_SHA3.SHA3_256 (Code_Bytes, Digest);

         --  Copy digest to code hash
         for I in 0 .. 31 loop
            Code_Hash (I) := Byte (Digest (I));
         end loop;
      end;

      --  Generate contract address from code hash
      for I in 0 .. 31 loop
         Contract_ID (I) := Code_Hash (I);
      end loop;

      --  Find empty slot
      Slot := Contract_Index (VM.Contract_Count);

      --  Register contract
      VM.Contracts (Slot) := (
         Address   => Contract_ID,
         Code_Hash => Code_Hash,
         Is_Loaded => True
      );

      VM.Contract_Count := VM.Contract_Count + 1;

      --  Set success result
      Result.Success := True;
      Result.Contract_ID := Contract_ID;
      Result.Code_Hash := Code_Hash;
      Result.Gas_Used := Gas_Amount (Code_Size * 200);  -- Base gas cost

      --  Log deployment
      --  In production, this would store the manifest and code

      pragma Unreferenced (Manifest);
   end Deploy_Contract;

   ---------------------------------------------------------------------------
   --  Contract Invocation
   ---------------------------------------------------------------------------

   procedure Set_Invoke_Error (
      Result : in Out Invoke_Result;
      Code   : RPC_Error_Code;
      Msg    : String
   ) with
      Global => null,
      Pre    => Msg'Length <= Max_Error_Message_Length
   is
      Len : constant Natural := Msg'Length;
   begin
      Result.Success := False;
      Result.Error_Code := Code;
      Result.Error_Msg := (others => ' ');
      for I in 1 .. Len loop
         Result.Error_Msg (I) := Msg (Msg'First + I - 1);
      end loop;
      Result.Error_Msg_Len := Len;
   end Set_Invoke_Error;

   procedure Invoke_Contract (
      VM       : in Out VM_Instance;
      Request  : in     Invoke_Request;
      Result   : out    Invoke_Result
   ) is
      Found : Boolean := False;
   begin
      Result := Empty_Invoke_Result;

      --  Check if contract exists
      for I in 0 .. Contract_Index (VM.Contract_Count - 1) loop
         if VM.Contracts (I).Is_Loaded and then
            VM.Contracts (I).Address = Request.To then
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Set_Invoke_Error (Result, Error_Invalid_Params, "Contract not found");
         return;
      end if;

      --  Check gas limit
      if Request.Gas_Limit = 0 then
         Set_Invoke_Error (Result, Error_Invalid_Params, "Gas limit required");
         return;
      end if;

      --  Execute contract (stub - would invoke AegisVM here)
      --  In production:
      --  1. Load contract code from storage
      --  2. Create execution context
      --  3. Execute entry point with args
      --  4. Capture return data and logs

      Result.Success := True;
      Result.Gas_Used := Request.Gas_Limit / 10;  -- Placeholder
      Result.Return_Size := 0;
      Result.Log_Count := 0;

      --  Note: Request fields Entry_Point, Entry_Len, Args, Args_Size,
      --  Value are parsed from params and used by AegisVM execution
   end Invoke_Contract;

   procedure Call_Contract (
      VM       : in     VM_Instance;
      Request  : in     Invoke_Request;
      Result   : out    Invoke_Result
   ) is
      Found : Boolean := False;
   begin
      Result := Empty_Invoke_Result;

      --  Check if contract exists
      for I in 0 .. Contract_Index (VM.Contract_Count - 1) loop
         if VM.Contracts (I).Is_Loaded and then
            VM.Contracts (I).Address = Request.To then
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Set_Invoke_Error (Result, Error_Invalid_Params, "Contract not found");
         return;
      end if;

      --  Execute read-only call (stub)
      --  Same as Invoke but without state changes

      Result.Success := True;
      Result.Gas_Used := 21000;  -- Base call cost
      Result.Return_Size := 0;
      Result.Log_Count := 0;

      --  Note: Request fields used by AegisVM read-only execution
   end Call_Contract;

end Anubis_Node;
