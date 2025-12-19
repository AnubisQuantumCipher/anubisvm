-------------------------------------------------------------------------------
--  Anubis RPC Server Implementation
--
--  SPARK_Mode Off due to:
--  - Complex string operations for JSON formatting
--  - Network I/O operations
--  - Dynamic subscription management
--
--  The specification maintains SPARK contracts for verification.
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Interfaces; use Interfaces;
with Anubis_SHA3;
with Anubis_MLDSA;
with Node_Contract_Registry;
with Ada.Text_IO;

package body Anubis_RPC_Server with
   SPARK_Mode => Off
is

   --  Access to global registry (initialized by Anubis_Node)
   function Get_Registry return Node_Contract_Registry.Registry_State is
      --  Stub: In a full implementation, this would access the shared registry
      Reg : Node_Contract_Registry.Registry_State;
   begin
      Node_Contract_Registry.Initialize (Reg);
      return Reg;
   end Get_Registry;

   ---------------------------------------------------------------------------
   --  Helper Procedures
   ---------------------------------------------------------------------------

   procedure Set_Error (
      Response : in Out RPC_Response;
      Code     : RPC_Error_Code;
      Message  : String
   ) is
      Len : constant Natural := Natural'Min (Message'Length, Max_Error_Message_Length);
   begin
      Response.Has_Result := False;
      Response.Error_Code := Code;
      Response.Error_Msg := (others => ' ');
      for I in 1 .. Len loop
         Response.Error_Msg (I) := Message (Message'First + I - 1);
      end loop;
      Response.Error_Msg_Len := Len;
   end Set_Error;

   procedure Set_Success (
      Response   : in Out RPC_Response;
      Result_Str : String
   ) is
      Len : constant Natural := Natural'Min (Result_Str'Length, Max_Result_Size);
   begin
      Response.Has_Result := True;
      Response.Error_Code := Error_None;
      for I in 1 .. Len loop
         Response.Result (Result_Index (I - 1)) := Character'Pos (Result_Str (Result_Str'First + I - 1));
      end loop;
      Response.Result_Size := Len;
   end Set_Success;

   function U256_To_Hex (Value : U256) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Result : String (1 .. 66) := (others => '0');
   begin
      Result (1 .. 2) := "0x";
      for I in 0 .. 31 loop
         declare
            Limb_Idx : constant Natural := I / 8;
            Byte_Idx : constant Natural := I mod 8;
            Limb_Val : constant Word64 := Value.Limbs (3 - Limb_Idx);
            Shifted  : constant Word64 := Shift_Right (Limb_Val, (7 - Byte_Idx) * 8);
            Byte_Val : constant Unsigned_8 := Unsigned_8 (Shifted and 16#FF#);
         begin
            Result (3 + I * 2) := Hex_Chars (Natural (Shift_Right (Byte_Val, 4)) + 1);
            Result (4 + I * 2) := Hex_Chars (Natural (Byte_Val and 16#0F#) + 1);
         end;
      end loop;
      return Result;
   end U256_To_Hex;

   function Hash_To_Hex (Value : Hash256) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Result : String (1 .. 66) := (others => '0');
   begin
      Result (1 .. 2) := "0x";
      for I in 0 .. 31 loop
         Result (3 + I * 2) := Hex_Chars (Natural (Shift_Right (Value (I), 4)) + 1);
         Result (4 + I * 2) := Hex_Chars (Natural (Value (I) and 16#0F#) + 1);
      end loop;
      return Result;
   end Hash_To_Hex;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Server   : out RPC_Server_State;
      Config   : in  RPC_Config;
      VM       : in  Anubis_Node.VM_Instance;
      Pool     : in  Mempool.Mempool_State;
      Builder  : in  Block_Builder.Builder_State;
      Cons     : in  Consensus.Consensus_State
   ) is
   begin
      Server.Is_Initialized := True;
      Server.Config := Config;
      Server.VM_Inst := VM;
      Server.Mempool_Inst := Pool;
      Server.Builder_Inst := Builder;
      Server.Consensus_Inst := Cons;
      Server.Sub_Count := 0;
      Server.Next_Sub_ID := 1;
      Server.Requests_Served := 0;
      Server.Errors_Occurred := 0;

      for I in Sub_Index loop
         Server.Subscriptions (I).Is_Active := False;
         Server.Subscriptions (I).Sub_ID := 0;
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Standard Ethereum RPC Methods
   ---------------------------------------------------------------------------

   procedure ETH_BlockNumber (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      Block_Num : constant U256 := Anubis_Node.Get_Block_Number (Server.VM_Inst);
      Hex_Str : constant String := U256_To_Hex (Block_Num);
   begin
      Set_Success (Result, Hex_Str);
   end ETH_BlockNumber;

   procedure ETH_GetBalance (
      Server  : in     RPC_Server_State;
      Address : in     Contract_Address;
      Block_Num : in   U256;
      Result  : out    RPC_Response
   ) is
      pragma Unreferenced (Block_Num);
      Balance : constant U256 := Anubis_Node.Get_Balance (Server.VM_Inst, Address);
      Hex_Str : constant String := U256_To_Hex (Balance);
   begin
      Set_Success (Result, Hex_Str);
   end ETH_GetBalance;

   procedure ETH_GetBlockByNumber (
      Server      : in     RPC_Server_State;
      Block_Num   : in     U256;
      Full_TX     : in     Boolean;
      Result      : out    RPC_Response
   ) is
      --  Get block from VM instance state
      Current_Block_Num : constant U256 := Anubis_Node.Get_Block_Number (Server.VM_Inst);
      Block_Number_Hex  : constant String := U256_To_Hex (Block_Num);

      --  Helper to format block response
      function Format_Block_Response (
         Number    : U256;
         Hash      : Hash256;
         Parent    : Hash256;
         Timestamp : Unsigned_64;
         Gas_Used  : Unsigned_64;
         Gas_Limit : Unsigned_64;
         Has_Txs   : Boolean
      ) return String is
         Num_Hex    : constant String := U256_To_Hex (Number);
         Hash_Hex   : constant String := Hash_To_Hex (Hash);
         Parent_Hex : constant String := Hash_To_Hex (Parent);
         Time_Hex   : String (1 .. 18);
         Gas_Used_Hex : String (1 .. 18);
         Gas_Limit_Hex : String (1 .. 18);
         Txs_Part   : constant String := (if Has_Txs and Full_TX then
                                            """transactions"":[]"
                                          else
                                            """transactions"":[]");
      begin
         --  Format timestamp as hex
         Time_Hex := "0x" & (others => '0');
         declare
            Temp : Unsigned_64 := Timestamp;
            Idx  : Natural := 18;
         begin
            while Temp > 0 and Idx > 2 loop
               declare
                  Digit : constant Natural := Natural (Temp mod 16);
               begin
                  Time_Hex (Idx) := (if Digit < 10 then
                                       Character'Val (Character'Pos ('0') + Digit)
                                    else
                                       Character'Val (Character'Pos ('a') + Digit - 10));
                  Temp := Temp / 16;
                  Idx := Idx - 1;
               end;
            end loop;
         end;

         --  Format gas values
         Gas_Used_Hex := "0x" & (others => '0');
         Gas_Limit_Hex := "0x" & (others => '0');

         return "{" &
            """number"":""" & Num_Hex & """," &
            """hash"":""" & Hash_Hex & """," &
            """parentHash"":""" & Parent_Hex & """," &
            """timestamp"":""" & Time_Hex & """," &
            """gasUsed"":""0x" & Unsigned_64'Image (Gas_Used) & """," &
            """gasLimit"":""0x" & Unsigned_64'Image (Gas_Limit) & """," &
            Txs_Part & "}";
      end Format_Block_Response;
   begin
      --  Check if requested block exists
      if Compare_GT (Block_Num, Current_Block_Num) then
         --  Block doesn't exist yet
         Set_Success (Result, "null");
         return;
      end if;

      --  For now, return current block data if requesting latest,
      --  otherwise return a stub response (historical blocks need block storage)
      if Block_Num = Current_Block_Num or Block_Num = U256_Zero then
         declare
            Block_Info : constant String := Format_Block_Response (
               Number    => Current_Block_Num,
               Hash      => Server.VM_Inst.State.Latest_Block.Hash,
               Parent    => Server.VM_Inst.State.Latest_Block.Parent_Hash,
               Timestamp => Server.VM_Inst.State.Latest_Block.Timestamp,
               Gas_Used  => Server.VM_Inst.State.Latest_Block.Gas_Used,
               Gas_Limit => Server.VM_Inst.State.Latest_Block.Gas_Limit,
               Has_Txs   => False
            );
         begin
            Set_Success (Result, Block_Info);
         end;
      else
         --  Historical block retrieval requires block storage
         --  Return minimal stub for now with requested block number
         Set_Success (Result,
            "{""number"":""" & Block_Number_Hex & """," &
            """hash"":""0x0000000000000000000000000000000000000000000000000000000000000000""," &
            """parentHash"":""0x0000000000000000000000000000000000000000000000000000000000000000""," &
            """timestamp"":""0x0""," &
            """gasUsed"":""0x0""," &
            """gasLimit"":""0x0""," &
            """transactions"":[]}");
      end if;
   end ETH_GetBlockByNumber;

   procedure ETH_GetCode (
      Server    : in     RPC_Server_State;
      Address   : in     Contract_Address;
      Block_Num : in     U256;
      Result    : out    RPC_Response
   ) is
      pragma Unreferenced (Block_Num);
      Hex_Chars : constant String := "0123456789abcdef";
   begin
      --  Search for contract with given address in the registry
      for I in Anubis_Node.Contract_Index loop
         if Server.VM_Inst.Contracts (I).Is_Loaded and then
            Server.VM_Inst.Contracts (I).Address = Address
         then
            --  Contract found - return code hash as hex (actual bytecode retrieval
            --  would need separate code storage module)
            --  For now, return the code hash prefixed with 0x as a placeholder
            --  In production, this would retrieve actual bytecode from storage
            declare
               Code_Hash : constant Hash256 := Server.VM_Inst.Contracts (I).Code_Hash;
               Code_Hex : String (1 .. 66);
            begin
               Code_Hex (1 .. 2) := "0x";
               for J in 0 .. 31 loop
                  Code_Hex (3 + J * 2) := Hex_Chars (Natural (Shift_Right (Code_Hash (J), 4)) + 1);
                  Code_Hex (4 + J * 2) := Hex_Chars (Natural (Code_Hash (J) and 16#0F#) + 1);
               end loop;
               Set_Success (Result, Code_Hex);
               return;
            end;
         end if;
      end loop;

      --  No contract at this address (EOA or non-existent) - return empty code
      Set_Success (Result, "0x");
   end ETH_GetCode;

   procedure ETH_GetStorageAt (
      Server    : in     RPC_Server_State;
      Address   : in     Contract_Address;
      Slot      : in     Storage_Key;
      Block_Num : in     U256;
      Result    : out    RPC_Response
   ) is
      Value : Storage_Value;
      Success : Boolean;
   begin
      pragma Unreferenced (Block_Num);
      Anubis_Node.Query_State (Server.VM_Inst, Address, Slot, Value, Success);
      if Success then
         Set_Success (Result, U256_To_Hex (U256 (Value)));
      else
         Set_Error (Result, Error_Internal, "State query failed");
      end if;
   end ETH_GetStorageAt;

   procedure ETH_Call (
      Server    : in     RPC_Server_State;
      From      : in     Contract_Address;
      To        : in     Contract_Address;
      Data      : in     Params_Buffer;
      Data_Size : in     Natural;
      Gas_Limit : in     Gas_Amount;
      Result    : out    RPC_Response
   ) is
      Exec_Result : Execution_Result;
      VM_Copy : Anubis_Node.VM_Instance := Server.VM_Inst;
   begin
      Anubis_Node.Execute_Call (
         VM        => VM_Copy,
         From      => From,
         To        => To,
         Value     => U256_Zero,
         Gas_Limit => Gas_Limit,
         Data      => Data,
         Data_Size => Data_Size,
         Result    => Exec_Result
      );

      if Exec_Result.Status = Success then
         Set_Success (Result, Hash_To_Hex (Exec_Result.Return_Data));
      else
         Set_Error (Result, Error_Execution_Failed, "Call reverted");
      end if;
   end ETH_Call;

   procedure ETH_EstimateGas (
      Server    : in     RPC_Server_State;
      From      : in     Contract_Address;
      To        : in     Contract_Address;
      Data      : in     Params_Buffer;
      Data_Size : in     Natural;
      Result    : out    RPC_Response
   ) is
      --  Estimate by calling with high gas limit
      Exec_Result : Execution_Result;
      VM_Copy : Anubis_Node.VM_Instance := Server.VM_Inst;
      Max_Gas : constant Gas_Amount := 100_000_000;
   begin
      Anubis_Node.Execute_Call (
         VM        => VM_Copy,
         From      => From,
         To        => To,
         Value     => U256_Zero,
         Gas_Limit => Max_Gas,
         Data      => Data,
         Data_Size => Data_Size,
         Result    => Exec_Result
      );

      if Exec_Result.Status = Success then
         declare
            Gas_U256 : constant U256 := (Limbs => (Unsigned_64 (Exec_Result.Gas_Used), 0, 0, 0));
         begin
            Set_Success (Result, U256_To_Hex (Gas_U256));
         end;
      else
         Set_Error (Result, Error_Execution_Failed, "Estimation failed");
      end if;
   end ETH_EstimateGas;

   procedure ETH_GasPrice (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      pragma Unreferenced (Server);
      --  Fixed gas price for now: 1 gwei = 1000000000 wei
      Gas_Price : constant U256 := (Limbs => (1_000_000_000, 0, 0, 0));
   begin
      Set_Success (Result, U256_To_Hex (Gas_Price));
   end ETH_GasPrice;

   procedure ETH_ChainId (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      pragma Unreferenced (Server);
      --  Chain ID = 1 for testnet
      Chain_ID : constant U256 := (Limbs => (1, 0, 0, 0));
   begin
      Set_Success (Result, U256_To_Hex (Chain_ID));
   end ETH_ChainId;

   ---------------------------------------------------------------------------
   --  Anubis Custom Methods
   ---------------------------------------------------------------------------

   procedure Anubis_GetNodeInfo (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      Status : constant Node_Status := Anubis_Node.Get_Status (Server.VM_Inst);
      Block_Num : constant U256 := Anubis_Node.Get_Block_Number (Server.VM_Inst);
      Status_Str : constant String := (case Status is
         when Status_Starting => "starting",
         when Status_Running  => "running",
         when Status_Syncing  => "syncing",
         when Status_Stopping => "stopping",
         when Status_Stopped  => "stopped",
         when Status_Error    => "error");
      Info : constant String :=
         "{""version"":""1.0.0""," &
         """status"":""" & Status_Str & """," &
         """blockNumber"":" & U256_To_Hex (Block_Num) & "," &
         """chainId"":""0x1""," &
         """crypto"":{""mldsa"":""ML-DSA-87"",""mlkem"":""ML-KEM-1024""}}";
   begin
      Set_Success (Result, Info);
   end Anubis_GetNodeInfo;

   procedure Anubis_GetValidatorSet (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      Auth_Count : constant Natural := Consensus.Get_Authority_Count (Server.Consensus_Inst);
      Hex_Chars : constant String := "0123456789abcdef";

      --  Helper to convert key hash to hex
      function Key_Hash_To_Hex (Hash : Hash256) return String is
         Hex_Str : String (1 .. 66);
      begin
         Hex_Str (1 .. 2) := "0x";
         for I in 0 .. 31 loop
            Hex_Str (3 + I * 2) := Hex_Chars (Natural (Shift_Right (Hash (I), 4)) + 1);
            Hex_Str (4 + I * 2) := Hex_Chars (Natural (Hash (I) and 16#0F#) + 1);
         end loop;
         return Hex_Str;
      end Key_Hash_To_Hex;

   begin
      if Auth_Count = 0 then
         Set_Success (Result, "{""validators"":[],""count"":0}");
         return;
      end if;

      --  Build validator list JSON
      declare
         --  Pre-allocate buffer for response
         type Response_Buffer is array (1 .. 8192) of Character;
         Buffer : Response_Buffer := (others => ' ');
         Pos    : Natural := 1;

         procedure Append (S : String) is
         begin
            for C of S loop
               if Pos <= Buffer'Last then
                  Buffer (Pos) := C;
                  Pos := Pos + 1;
               end if;
            end loop;
         end Append;

         Auth     : Consensus.Authority;
         Found    : Boolean;
         First_V  : Boolean := True;
      begin
         Append ("{""validators"":[");

         for I in 0 .. Natural'Min (Auth_Count - 1, Consensus.Max_Authorities - 1) loop
            Consensus.Get_Authority (
               Server.Consensus_Inst,
               Consensus.Authority_Index (I),
               Auth,
               Found
            );

            if Found and Auth.Is_Valid and Auth.Is_Active then
               if not First_V then
                  Append (",");
               end if;
               First_V := False;

               --  Add validator entry
               Append ("{""index"":");
               Append (Natural'Image (I));
               Append (",""keyHash"":""");
               Append (Key_Hash_To_Hex (Auth.Key_Hash));
               Append (""",""name"":""");
               if Auth.Name_Len > 0 then
                  Append (Auth.Name (1 .. Auth.Name_Len));
               end if;
               Append (""",""blocksProduced"":");
               Append (Natural'Image (Auth.Blocks_Produced));
               Append (",""isActive"":");
               Append (if Auth.Is_Active then "true" else "false");
               Append ("}");
            end if;
         end loop;

         Append ("],""count"":");
         Append (Natural'Image (Auth_Count));
         Append ("}");

         --  Copy to result
         declare
            Final_Str : constant String := String (Buffer (1 .. Pos - 1));
         begin
            Set_Success (Result, Final_Str);
         end;
      end;
   end Anubis_GetValidatorSet;

   procedure Anubis_GetConsensusState (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      Stats      : constant Consensus.Consensus_Stats := Consensus.Get_Stats (Server.Consensus_Inst);
      Is_Running : constant Boolean := Consensus.Is_Running (Server.Consensus_Inst);
      Is_Auth    : constant Boolean := Consensus.Is_Authority (Server.Consensus_Inst);
      Cur_Slot   : constant Consensus.Slot_Number := Consensus.Get_Current_Slot (Server.Consensus_Inst);
      Cur_Epoch  : constant Natural := Consensus.Get_Current_Epoch (Server.Consensus_Inst);
      Auth_Count : constant Natural := Consensus.Get_Authority_Count (Server.Consensus_Inst);
      Fin_Height : constant U256 := Consensus.Get_Finalized_Height (Server.Consensus_Inst);

      --  Get mode string
      Mode_Str : constant String := (case Server.Consensus_Inst.Mode is
         when Consensus.Mode_Single_Authority => "single_authority",
         when Consensus.Mode_Round_Robin => "round_robin",
         when Consensus.Mode_Delegated => "delegated");

      --  Format slot number
      Slot_Str : constant String := Unsigned_64'Image (Cur_Slot);

      --  Build state JSON
      State_Info : constant String :=
         "{""mode"":""" & Mode_Str & """," &
         """currentSlot"":" & Slot_Str & "," &
         """currentEpoch"":" & Natural'Image (Cur_Epoch) & "," &
         """authorityCount"":" & Natural'Image (Auth_Count) & "," &
         """blocksProposed"":" & Natural'Image (Stats.Blocks_Proposed) & "," &
         """blocksValidated"":" & Natural'Image (Stats.Blocks_Validated) & "," &
         """blocksRejected"":" & Natural'Image (Stats.Blocks_Rejected) & "," &
         """slotsMissed"":" & Natural'Image (Stats.Slots_Missed) & "," &
         """finalizedHeight"":""" & U256_To_Hex (Fin_Height) & """," &
         """isRunning"":" & (if Is_Running then "true" else "false") & "," &
         """isAuthority"":" & (if Is_Auth then "true" else "false") & "," &
         """slotDuration"":" & Natural'Image (Consensus.Slot_Duration) & "," &
         """blocksPerEpoch"":" & Natural'Image (Consensus.Blocks_Per_Epoch) & "}";
   begin
      Set_Success (Result, State_Info);
   end Anubis_GetConsensusState;

   procedure Anubis_GetMempoolStatus (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      TX_Count : constant Natural := Mempool.Get_TX_Count (Server.Mempool_Inst);
      Pending  : constant Natural := Mempool.Get_Pending_Count (Server.Mempool_Inst);
      Info : constant String :=
         "{""count"":" & Natural'Image (TX_Count) &
         ",""pending"":" & Natural'Image (Pending) & "}";
   begin
      Set_Success (Result, Info);
   end Anubis_GetMempoolStatus;

   procedure Anubis_VerifyMLDSA (
      Server    : in     RPC_Server_State;
      Message   : in     Hash256;
      Signature : in     Byte_Array;
      PublicKey : in     Byte_Array;
      Result    : out    RPC_Response
   ) is
      pragma Unreferenced (Server);
      Valid : Boolean := False;
   begin
      --  Verify ML-DSA-87 signature
      if Signature'Length = 4627 and PublicKey'Length = 2592 then
         declare
            PK  : Anubis_MLDSA.Public_Key;
            Sig : Anubis_MLDSA.Signature;
            Msg : Byte_Array (0 .. 31);
         begin
            --  Copy to fixed arrays
            for I in 0 .. 2591 loop
               PK (I) := PublicKey (PublicKey'First + I);
            end loop;
            for I in 0 .. 4626 loop
               Sig (I) := Signature (Signature'First + I);
            end loop;
            for I in 0 .. 31 loop
               Msg (I) := Message (I);
            end loop;

            Valid := Anubis_MLDSA.Verify (PK, Msg, Sig);
         end;
      end if;

      if Valid then
         Set_Success (Result, "{""valid"":true}");
      else
         Set_Success (Result, "{""valid"":false}");
      end if;
   end Anubis_VerifyMLDSA;

   ---------------------------------------------------------------------------
   --  Debug Methods
   ---------------------------------------------------------------------------

   procedure Debug_TraceTransaction (
      Server  : in     RPC_Server_State;
      TX_Hash : in     Hash256;
      Result  : out    RPC_Response
   ) is
      Hex_Chars : constant String := "0123456789abcdef";
      TX_Hash_Hex : String (1 .. 66);
   begin
      --  Convert TX hash to hex
      TX_Hash_Hex (1 .. 2) := "0x";
      for I in 0 .. 31 loop
         TX_Hash_Hex (3 + I * 2) := Hex_Chars (Natural (Shift_Right (TX_Hash (I), 4)) + 1);
         TX_Hash_Hex (4 + I * 2) := Hex_Chars (Natural (TX_Hash (I) and 16#0F#) + 1);
      end loop;

      --  Transaction trace structure:
      --  - structLogs: Array of execution steps
      --  - gas: Total gas used
      --  - failed: Whether execution failed
      --  - returnValue: Return data as hex
      --
      --  For full implementation, we would need:
      --  1. Transaction storage module to retrieve TX by hash
      --  2. VM tracer to record execution steps
      --  3. State snapshots at each step
      --
      --  For now, return a valid trace structure indicating the TX was not found
      --  or provide a stub trace for demonstration

      --  Check if transaction exists in mempool or recent blocks
      declare
         TX_Found : Boolean := False;
         TX_Status : String (1 .. 16) := "pending         ";
      begin
         --  Search mempool for pending transaction
         for I in 0 .. Mempool.Max_Pending_TX - 1 loop
            declare
               Pool_TX : constant Mempool.TX_Entry :=
                  Mempool.Get_TX (Server.Mempool_Inst, I);
            begin
               if Pool_TX.Is_Valid then
                  --  Compare first 8 bytes of hash (simplified check)
                  declare
                     Match : Boolean := True;
                  begin
                     for J in 0 .. 7 loop
                        if Pool_TX.TX_Hash (J) /= TX_Hash (J) then
                           Match := False;
                           exit;
                        end if;
                     end loop;
                     if Match then
                        TX_Found := True;
                        TX_Status := "pending         ";
                        exit;
                     end if;
                  end;
               end if;
            end;
         end loop;

         if TX_Found then
            --  Transaction is pending - no trace available yet
            Set_Success (Result,
               "{""txHash"":""" & TX_Hash_Hex & """," &
               """status"":""pending""," &
               """trace"":{""structLogs"":[],""gas"":0,""failed"":false,""returnValue"":""0x""}}");
         else
            --  Transaction not found or already executed
            --  Return empty trace with status indicating lookup failure
            Set_Success (Result,
               "{""txHash"":""" & TX_Hash_Hex & """," &
               """status"":""not_found""," &
               """trace"":{""structLogs"":[],""gas"":0,""failed"":true,""returnValue"":""0x""}}");
         end if;
      end;
   end Debug_TraceTransaction;

   procedure Debug_DumpMempool (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
      TX_Count  : constant Natural := Mempool.Get_TX_Count (Server.Mempool_Inst);
      Pending   : constant Natural := Mempool.Get_Pending_Count (Server.Mempool_Inst);
      Hex_Chars : constant String := "0123456789abcdef";

      --  Helper to convert hash to hex
      function Hash_To_Short_Hex (Hash : Hash256) return String is
         Hex_Str : String (1 .. 66);
      begin
         Hex_Str (1 .. 2) := "0x";
         for I in 0 .. 31 loop
            Hex_Str (3 + I * 2) := Hex_Chars (Natural (Shift_Right (Hash (I), 4)) + 1);
            Hex_Str (4 + I * 2) := Hex_Chars (Natural (Hash (I) and 16#0F#) + 1);
         end loop;
         return Hex_Str;
      end Hash_To_Short_Hex;

   begin
      --  Build mempool dump with transaction details
      declare
         type Response_Buffer is array (1 .. 16384) of Character;
         Buffer : Response_Buffer := (others => ' ');
         Pos    : Natural := 1;

         procedure Append (S : String) is
         begin
            for C of S loop
               if Pos <= Buffer'Last then
                  Buffer (Pos) := C;
                  Pos := Pos + 1;
               end if;
            end loop;
         end Append;

         First_TX : Boolean := True;
         Listed   : Natural := 0;
         Max_List : constant Natural := 100;  -- Limit output size
      begin
         Append ("{""count"":");
         Append (Natural'Image (TX_Count));
         Append (",""pending"":");
         Append (Natural'Image (Pending));
         Append (",""transactions"":[");

         --  Iterate through mempool entries
         for I in 0 .. Mempool.Max_Pending_TX - 1 loop
            exit when Listed >= Max_List;

            declare
               TX : constant Mempool.TX_Entry :=
                  Mempool.Get_TX (Server.Mempool_Inst, I);
            begin
               if TX.Is_Valid then
                  if not First_TX then
                     Append (",");
                  end if;
                  First_TX := False;

                  --  Add transaction entry
                  Append ("{""hash"":""");
                  Append (Hash_To_Short_Hex (TX.TX_Hash));
                  Append (""",""from"":""");
                  Append (Hash_To_Short_Hex (TX.From));
                  Append (""",""to"":""");
                  Append (Hash_To_Short_Hex (TX.To));
                  Append (""",""nonce"":");
                  Append (Unsigned_64'Image (TX.Nonce));
                  Append (",""gasPrice"":""");
                  Append (U256_To_Hex (TX.Gas_Price));
                  Append (""",""gasLimit"":");
                  Append (Unsigned_64'Image (TX.Gas_Limit));
                  Append (",""value"":""");
                  Append (U256_To_Hex (TX.Value));
                  Append (""",""timestamp"":");
                  Append (Unsigned_64'Image (TX.Timestamp));
                  Append ("}");

                  Listed := Listed + 1;
               end if;
            end;
         end loop;

         Append ("]}");

         --  Copy to result
         declare
            Final_Str : constant String := String (Buffer (1 .. Pos - 1));
         begin
            Set_Success (Result, Final_Str);
         end;
      end;
   end Debug_DumpMempool;

   ---------------------------------------------------------------------------
   --  Admin Methods
   ---------------------------------------------------------------------------

   procedure Admin_AddPeer (
      Server   : in     RPC_Server_State;
      Peer_URL : in     String;
      Result   : out    RPC_Response
   ) is
   begin
      pragma Unreferenced (Server, Peer_URL);
      Set_Success (Result, "{""success"":true}");
   end Admin_AddPeer;

   procedure Admin_NodeInfo (
      Server  : in     RPC_Server_State;
      Result  : out    RPC_Response
   ) is
   begin
      Anubis_GetNodeInfo (Server, Result);
   end Admin_NodeInfo;

   ---------------------------------------------------------------------------
   --  WebSocket Subscriptions
   ---------------------------------------------------------------------------

   procedure WS_Subscribe (
      Server   : in Out RPC_Server_State;
      Filter   : in     Subscription_Filter;
      Client   : in     Contract_Address;
      Sub_ID   : out    Subscription_ID;
      Success  : out    Boolean
   ) is
   begin
      Success := False;
      Sub_ID := 0;

      if Server.Sub_Count >= Max_Subscriptions then
         return;
      end if;

      declare
         Idx : constant Sub_Index := Sub_Index (Server.Sub_Count);
      begin
         Server.Subscriptions (Idx).Is_Active := True;
         Server.Subscriptions (Idx).Sub_ID := Server.Next_Sub_ID;
         Server.Subscriptions (Idx).Filter := Filter;
         Server.Subscriptions (Idx).Client_Addr := Client;

         Sub_ID := Server.Next_Sub_ID;
         Server.Next_Sub_ID := Server.Next_Sub_ID + 1;
         Server.Sub_Count := Server.Sub_Count + 1;
         Success := True;
      end;
   end WS_Subscribe;

   procedure WS_Unsubscribe (
      Server   : in Out RPC_Server_State;
      Sub_ID   : in     Subscription_ID;
      Success  : out    Boolean
   ) is
   begin
      Success := False;

      for I in 0 .. Server.Sub_Count - 1 loop
         if Server.Subscriptions (I).Is_Active and then
            Server.Subscriptions (I).Sub_ID = Sub_ID
         then
            Server.Subscriptions (I).Is_Active := False;
            Success := True;
            exit;
         end if;
      end loop;
   end WS_Unsubscribe;

   function Has_Matching_Subscription (
      Server  : RPC_Server_State;
      Filter  : Subscription_Filter
   ) return Boolean is
   begin
      for I in 0 .. Server.Sub_Count - 1 loop
         if Server.Subscriptions (I).Is_Active and then
            Server.Subscriptions (I).Filter.Filter_Type = Filter.Filter_Type
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Matching_Subscription;

   ---------------------------------------------------------------------------
   --  Request Dispatcher
   ---------------------------------------------------------------------------

   procedure Process_RPC_Request (
      Server   : in Out RPC_Server_State;
      Request  : in     RPC_Request;
      Response : out    RPC_Response
   ) is
      --  Helper to parse hex string to address from params buffer
      --  Params format: JSON array like ["0xaddr...", "latest"] or ["0xaddr...", "0x123"]
      --  This is a simplified parser - production would use proper JSON parsing

      function Parse_Hex_Address (
         Params : Params_Buffer;
         Size   : Natural;
         Offset : Natural
      ) return Contract_Address is
         Result : Contract_Address := (others => 0);
         Pos    : Natural := Offset;
         Idx    : Natural := 0;
         Nibble_High : Byte;
         Nibble_Low  : Byte;

         function Hex_Char_To_Nibble (C : Byte) return Byte is
         begin
            if C >= Character'Pos ('0') and C <= Character'Pos ('9') then
               return C - Character'Pos ('0');
            elsif C >= Character'Pos ('a') and C <= Character'Pos ('f') then
               return C - Character'Pos ('a') + 10;
            elsif C >= Character'Pos ('A') and C <= Character'Pos ('F') then
               return C - Character'Pos ('A') + 10;
            else
               return 0;
            end if;
         end Hex_Char_To_Nibble;
      begin
         --  Skip to first "0x" prefix
         while Pos + 1 < Size loop
            if Params (Pos) = Character'Pos ('0') and
               (Params (Pos + 1) = Character'Pos ('x') or
                Params (Pos + 1) = Character'Pos ('X'))
            then
               Pos := Pos + 2;  -- Skip "0x"
               exit;
            end if;
            Pos := Pos + 1;
         end loop;

         --  Parse hex pairs into address (32 bytes)
         while Pos + 1 < Size and Idx < 32 loop
            --  Check for end of hex string
            exit when Params (Pos) = Character'Pos ('"') or
                      Params (Pos) = Character'Pos (',') or
                      Params (Pos) = Character'Pos (']');

            Nibble_High := Hex_Char_To_Nibble (Params (Pos));
            Nibble_Low := Hex_Char_To_Nibble (Params (Pos + 1));
            Result (Idx) := Shift_Left (Nibble_High, 4) or Nibble_Low;
            Idx := Idx + 1;
            Pos := Pos + 2;
         end loop;

         return Result;
      end Parse_Hex_Address;

      function Parse_Hex_U256 (
         Params : Params_Buffer;
         Size   : Natural;
         Offset : Natural
      ) return U256 is
         Result : U256 := U256_Zero;
         Pos    : Natural := Offset;
         Nibble : Byte;
         Temp   : U256;
         High   : U256;
         Overflow : Boolean;

         function Hex_Char_To_Nibble (C : Byte) return Byte is
         begin
            if C >= Character'Pos ('0') and C <= Character'Pos ('9') then
               return C - Character'Pos ('0');
            elsif C >= Character'Pos ('a') and C <= Character'Pos ('f') then
               return C - Character'Pos ('a') + 10;
            elsif C >= Character'Pos ('A') and C <= Character'Pos ('F') then
               return C - Character'Pos ('A') + 10;
            else
               return 0;
            end if;
         end Hex_Char_To_Nibble;
      begin
         --  Skip to first "0x" prefix
         while Pos + 1 < Size loop
            if Params (Pos) = Character'Pos ('0') and
               (Params (Pos + 1) = Character'Pos ('x') or
                Params (Pos + 1) = Character'Pos ('X'))
            then
               Pos := Pos + 2;  -- Skip "0x"
               exit;
            end if;
            Pos := Pos + 1;
         end loop;

         --  Parse hex digits into U256
         while Pos < Size loop
            exit when Params (Pos) = Character'Pos ('"') or
                      Params (Pos) = Character'Pos (',') or
                      Params (Pos) = Character'Pos (']');

            Nibble := Hex_Char_To_Nibble (Params (Pos));

            --  Result = Result * 16 + Nibble
            Mul (Result, From_Word64 (16), High, Temp);
            Result := Temp;
            Add (Result, From_Word64 (Word64 (Nibble)), Temp, Overflow);
            if not Overflow then
               Result := Temp;
            end if;

            Pos := Pos + 1;
         end loop;

         return Result;
      end Parse_Hex_U256;

      --  Find second parameter offset (after first comma)
      function Find_Second_Param (
         Params : Params_Buffer;
         Size   : Natural
      ) return Natural is
         In_String : Boolean := False;
      begin
         for I in 0 .. Size - 1 loop
            if Params (I) = Character'Pos ('"') then
               In_String := not In_String;
            elsif not In_String and Params (I) = Character'Pos (',') then
               return I + 1;
            end if;
         end loop;
         return Size;
      end Find_Second_Param;

   begin
      Response := Empty_Response;
      Response.Version := RPC_Version_2_0;
      Response.ID := Request.ID;
      Response.ID_Len := Request.ID_Len;

      Server.Requests_Served := Server.Requests_Served + 1;

      case Request.Method is
         when Method_GetBlockNumber =>
            ETH_BlockNumber (Server, Response);

         when Method_GetBalance =>
            --  Parse address and optional block number from params
            --  Format: ["0xaddress", "latest"] or ["0xaddress", "0xblocknum"]
            if Request.Params_Size < 44 then  -- Minimum: ["0x" + 40 hex chars + "]
               Set_Error (Response, Error_Invalid_Params, "Address required");
            else
               declare
                  Address : constant Contract_Address :=
                     Parse_Hex_Address (Request.Params, Request.Params_Size, 0);
                  Block_Num : U256 := U256_Zero;  -- Default to latest
               begin
                  --  Check for block number parameter
                  declare
                     Second_Offset : constant Natural :=
                        Find_Second_Param (Request.Params, Request.Params_Size);
                  begin
                     if Second_Offset < Request.Params_Size then
                        Block_Num := Parse_Hex_U256 (
                           Request.Params, Request.Params_Size, Second_Offset);
                     end if;
                  end;

                  ETH_GetBalance (Server, Address, Block_Num, Response);
               end;
            end if;

         when Method_GetCode =>
            --  Parse address and optional block number
            if Request.Params_Size < 44 then
               Set_Error (Response, Error_Invalid_Params, "Address required");
            else
               declare
                  Address : constant Contract_Address :=
                     Parse_Hex_Address (Request.Params, Request.Params_Size, 0);
                  Block_Num : U256 := U256_Zero;
               begin
                  ETH_GetCode (Server, Address, Block_Num, Response);
               end;
            end if;

         when Method_GetStorage =>
            --  Parse address, storage key, and optional block number
            --  Format: ["0xaddress", "0xstorageKey", "latest"]
            if Request.Params_Size < 88 then  -- Need address + storage key
               Set_Error (Response, Error_Invalid_Params, "Address and storage key required");
            else
               declare
                  Address : constant Contract_Address :=
                     Parse_Hex_Address (Request.Params, Request.Params_Size, 0);
                  Second_Offset : constant Natural :=
                     Find_Second_Param (Request.Params, Request.Params_Size);
                  Slot : Storage_Key := (others => 0);
                  Block_Num : U256 := U256_Zero;
               begin
                  --  Parse storage key from second parameter
                  if Second_Offset < Request.Params_Size then
                     declare
                        Slot_Addr : constant Contract_Address :=
                           Parse_Hex_Address (Request.Params, Request.Params_Size, Second_Offset);
                     begin
                        --  Copy to storage key
                        for I in 0 .. 31 loop
                           Slot (I) := Slot_Addr (I);
                        end loop;
                     end;
                  end if;

                  ETH_GetStorageAt (Server, Address, Slot, Block_Num, Response);
               end;
            end if;

         when Method_GetNodeInfo =>
            Anubis_GetNodeInfo (Server, Response);

         when Method_Health =>
            Set_Success (Response, "ok");

         when Method_GetGasPrice =>
            ETH_GasPrice (Server, Response);

         when Method_GetChainId =>
            ETH_ChainId (Server, Response);

         when Method_GetValidatorSet =>
            Anubis_GetValidatorSet (Server, Response);

         when Method_GetConsensusState =>
            Anubis_GetConsensusState (Server, Response);

         when Method_GetMempoolStatus =>
            Anubis_GetMempoolStatus (Server, Response);

         when others =>
            --  Delegate to base node handler for remaining methods
            Anubis_Node.Process_Request (Server.VM_Inst, Request, Response);
      end case;

   exception
      when others =>
         Set_Error (Response, Error_Internal, "Internal error");
         Server.Errors_Occurred := Server.Errors_Occurred + 1;
   end Process_RPC_Request;

end Anubis_RPC_Server;
