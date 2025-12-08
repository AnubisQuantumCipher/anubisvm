pragma SPARK_Mode (On);

with Aegis_Gas; use Aegis_Gas;
with Aegis_Storage; use Aegis_Storage;
with Aegis_U256; use Aegis_U256;

package body Aegis_Syscall with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Main Dispatcher
   ---------------------------------------------------------------------------

   procedure Dispatch (
      Ctx       : in Out Execution_Context;
      Syscall   : in     Syscall_Number;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) is
      Caps : constant Capability_Mask := Ctx.Sandbox.Current_Frame.Capabilities;
   begin
      --  Check if syscall is allowed
      if not Is_Syscall_Allowed (Caps, Syscall) then
         Result := (
            Success    => False,
            Gas_Used   => 0,
            Return_Val => U256_Zero,
            Error_Code => Error_Permission
         );
         return;
      end if;

      --  Dispatch based on syscall number
      case Syscall is
         when Sys_SLoad =>
            if Arg_Count >= 1 then
               Sys_Handle_SLoad (Ctx, Args (0), Result);
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_SStore =>
            if Arg_Count >= 2 then
               Sys_Handle_SStore (Ctx, Args (0), Args (1), Result);
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_SHA3 =>
            declare
               Data_Hash : Hash256 := Hash256_Zero;
               Size : Natural := 0;
               Raw_Size : Unsigned_64;
            begin
               if Arg_Count >= 1 then
                  Raw_Size := U256_To_U64 (Args (0));
                  --  Clamp to Natural range and Max_Hash_Input
                  if Raw_Size > Unsigned_64 (Max_Hash_Input) then
                     Size := Max_Hash_Input;
                  else
                     Size := Natural (Raw_Size);
                  end if;
               end if;
               Sys_Handle_SHA3 (Ctx, Data_Hash, Size, Result);
            end;

         when Sys_Keccak256 =>
            declare
               Data_Hash : Hash256 := Hash256_Zero;
               Size : Natural := 0;
               Raw_Size : Unsigned_64;
            begin
               if Arg_Count >= 1 then
                  Raw_Size := U256_To_U64 (Args (0));
                  --  Clamp to Natural range and Max_Hash_Input
                  if Raw_Size > Unsigned_64 (Max_Hash_Input) then
                     Size := Max_Hash_Input;
                  else
                     Size := Natural (Raw_Size);
                  end if;
               end if;
               Sys_Handle_Keccak256 (Ctx, Data_Hash, Size, Result);
            end;

         when Sys_MLDSA_Verify =>
            Sys_Handle_MLDSA_Verify (
               Ctx, Hash256_Zero, 0, Hash256_Zero, Hash256_Zero, Result);

         when Sys_MLKEM_Decaps =>
            Sys_Handle_MLKEM_Decaps (
               Ctx, Hash256_Zero, Hash256_Zero, Result);

         when Sys_Caller =>
            Sys_Handle_Caller (Ctx, Result);

         when Sys_Address =>
            Sys_Handle_Address (Ctx, Result);

         when Sys_CallValue =>
            Sys_Handle_CallValue (Ctx, Result);

         when Sys_CallData | Sys_CallDataSize =>
            --  Return zero for now (placeholder)
            Result := (True, 3, U256_Zero, Error_None);

         when Sys_BlockNumber =>
            Sys_Handle_BlockNumber (Ctx, Result);

         when Sys_Timestamp =>
            Sys_Handle_Timestamp (Ctx, Result);

         when Sys_GasPrice =>
            Sys_Handle_GasPrice (Ctx, Result);

         when Sys_GasRemaining =>
            Sys_Handle_GasRemaining (Ctx, Result);

         when Sys_Balance =>
            Sys_Handle_Balance (Ctx, Address_Zero, Result);

         when Sys_SelfBalance =>
            Sys_Handle_SelfBalance (Ctx, Result);

         when Sys_Call | Sys_StaticCall | Sys_DelegateCall |
              Sys_Create | Sys_Create2 =>
            --  Complex calls - placeholder
            Result := (False, 0, U256_Zero, Error_Call_Depth);

         when Sys_Return =>
            Sys_Handle_Return (Ctx, Empty_Return, Result);

         when Sys_Revert =>
            Sys_Handle_Revert (Ctx, Hash256_Zero, Result);

         when Sys_Stop =>
            Sys_Handle_Stop (Ctx, Result);

         when Sys_SelfDestruct =>
            if Can_Modify_State (Ctx) then
               Result := (True, Gas_SelfDestruct, U256_Zero, Error_None);
            else
               Result := (False, 0, U256_Zero, Error_State_Modify);
            end if;

         when Sys_Log0 | Sys_Log1 | Sys_Log2 | Sys_Log3 | Sys_Log4 =>
            Sys_Handle_Log (Ctx, (others => Event_Topic (Hash256_Zero)), 0,
                            Hash256_Zero, 0, Result);
      end case;
   end Dispatch;

   ---------------------------------------------------------------------------
   --  Storage Syscalls (THOTH)
   ---------------------------------------------------------------------------

   procedure Sys_Handle_SLoad (
      Ctx     : in Out Execution_Context;
      Key     : in     U256;
      Result  : out    Syscall_Return
   ) is
      pragma SPARK_Mode (Off);
      Value   : Storage_Value;
      Success : Boolean;
      SK      : constant Storage_Key := Storage_Key (Key);
   begin
      Storage_Load (Ctx, Get_Address (Ctx), SK, Value, Success);
      if Success then
         Result := (
            Success    => True,
            Gas_Used   => Gas_SLoad,
            Return_Val => U256 (Value),
            Error_Code => Error_None
         );
      else
         Result := (False, Gas_SLoad, U256_Zero, Error_Out_Of_Gas);
      end if;
   end Sys_Handle_SLoad;

   procedure Sys_Handle_SStore (
      Ctx     : in Out Execution_Context;
      Key     : in     U256;
      Value   : in     U256;
      Result  : out    Syscall_Return
   ) is
      pragma SPARK_Mode (Off);
      Success : Boolean;
      SK      : constant Storage_Key := Storage_Key (Key);
      SV      : constant Storage_Value := Storage_Value (Value);
   begin
      if not Can_Modify_State (Ctx) then
         Result := (False, 0, U256_Zero, Error_State_Modify);
         return;
      end if;

      Storage_Store (Ctx, Get_Address (Ctx), SK, SV, Success);
      if Success then
         Result := (True, Gas_SStore_Set, U256_Zero, Error_None);
      else
         Result := (False, Gas_SStore_Set, U256_Zero, Error_Out_Of_Gas);
      end if;
   end Sys_Handle_SStore;

   ---------------------------------------------------------------------------
   --  Cryptographic Syscalls (ANKH)
   ---------------------------------------------------------------------------

   procedure Sys_Handle_SHA3 (
      Ctx        : in Out Execution_Context;
      Data_Hash  : in     Hash256;
      Data_Size  : in     Natural;
      Result     : out    Syscall_Return
   ) is
      pragma Unreferenced (Data_Hash);
      Output    : Hash256;
      Input     : Hash_Input_Buffer := (others => 0);
      Crypto_Res : Crypto_Result;
   begin
      SHA3_256_Hash (Ctx, Input, Data_Size, Output, Crypto_Res);
      if Crypto_Res = Crypto_OK then
         Result := (True, Gas_Hash (Data_Size), U256_Zero, Error_None);
      else
         Result := (False, 0, U256_Zero, Error_Crypto);
      end if;
   end Sys_Handle_SHA3;

   procedure Sys_Handle_Keccak256 (
      Ctx        : in Out Execution_Context;
      Data_Hash  : in     Hash256;
      Data_Size  : in     Natural;
      Result     : out    Syscall_Return
   ) is
      pragma Unreferenced (Data_Hash);
      Output    : Hash256;
      Input     : Hash_Input_Buffer := (others => 0);
      Crypto_Res : Crypto_Result;
   begin
      Keccak256_Hash (Ctx, Input, Data_Size, Output, Crypto_Res);
      if Crypto_Res = Crypto_OK then
         Result := (True, Gas_Hash (Data_Size), U256_Zero, Error_None);
      else
         Result := (False, 0, U256_Zero, Error_Crypto);
      end if;
   end Sys_Handle_Keccak256;

   procedure Sys_Handle_MLDSA_Verify (
      Ctx       : in Out Execution_Context;
      Msg_Hash  : in     Hash256;
      Msg_Len   : in     Natural;
      Sig_Hash  : in     Hash256;
      PK_Hash   : in     Hash256;
      Result    : out    Syscall_Return
   ) is
      pragma Unreferenced (Msg_Hash, Sig_Hash, PK_Hash);
      Valid      : Boolean;
      Input      : Hash_Input_Buffer := (others => 0);
      Signature  : MLDSA87_Signature := (others => 0);
      Public_Key : MLDSA87_Public_Key := (others => 0);
      Crypto_Res : Crypto_Result;
   begin
      MLDSA87_Verify (Ctx, Input, Msg_Len, Signature, Public_Key,
                      Valid, Crypto_Res);
      if Crypto_Res = Crypto_OK then
         if Valid then
            Result := (True, Gas_MLDSA_Verify, U256_One, Error_None);
         else
            Result := (True, Gas_MLDSA_Verify, U256_Zero, Error_None);
         end if;
      else
         Result := (False, 0, U256_Zero, Error_Crypto);
      end if;
   end Sys_Handle_MLDSA_Verify;

   procedure Sys_Handle_MLKEM_Decaps (
      Ctx       : in Out Execution_Context;
      CT_Hash   : in     Hash256;
      DK_Hash   : in     Hash256;
      Result    : out    Syscall_Return
   ) is
      pragma Unreferenced (CT_Hash, DK_Hash);
      Shared     : MLKEM1024_Shared_Secret;
      Ciphertext : MLKEM1024_Ciphertext := (others => 0);
      Decaps_Key : MLKEM1024_Decaps_Key := (others => 0);
      Crypto_Res : Crypto_Result;
   begin
      MLKEM1024_Decaps (Ctx, Ciphertext, Decaps_Key, Shared, Crypto_Res);
      if Crypto_Res = Crypto_OK then
         Result := (True, Gas_MLKEM_Decaps, U256_One, Error_None);
         --  Zeroize the shared secret
         Zeroize_Secret (Shared);
      else
         Result := (False, 0, U256_Zero, Error_Crypto);
      end if;
   end Sys_Handle_MLKEM_Decaps;

   ---------------------------------------------------------------------------
   --  Environment Syscalls
   ---------------------------------------------------------------------------

   procedure Sys_Handle_Caller (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
      Caller_Addr : constant Contract_Address := Get_Caller (Ctx);
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Address_To_U256 (Caller_Addr),
         Error_Code => Error_None
      );
   end Sys_Handle_Caller;

   procedure Sys_Handle_Address (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
      Self_Addr : constant Contract_Address := Get_Address (Ctx);
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Address_To_U256 (Self_Addr),
         Error_Code => Error_None
      );
   end Sys_Handle_Address;

   procedure Sys_Handle_CallValue (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Get_Call_Value (Ctx),
         Error_Code => Error_None
      );
   end Sys_Handle_CallValue;

   procedure Sys_Handle_BlockNumber (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Get_Block_Number (Ctx),
         Error_Code => Error_None
      );
   end Sys_Handle_BlockNumber;

   procedure Sys_Handle_Timestamp (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Get_Timestamp (Ctx),
         Error_Code => Error_None
      );
   end Sys_Handle_Timestamp;

   procedure Sys_Handle_ChainID (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Get_Chain_ID (Ctx),
         Error_Code => Error_None
      );
   end Sys_Handle_ChainID;

   procedure Sys_Handle_GasPrice (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Ctx.Gas_Price,
         Error_Code => Error_None
      );
   end Sys_Handle_GasPrice;

   procedure Sys_Handle_GasRemaining (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Base,
         Return_Val => Gas_To_U256 (Gas_Remaining (Ctx)),
         Error_Code => Error_None
      );
   end Sys_Handle_GasRemaining;

   ---------------------------------------------------------------------------
   --  Balance Syscalls
   ---------------------------------------------------------------------------

   procedure Sys_Handle_Balance (
      Ctx     : in     Execution_Context;
      Address : in     Contract_Address;
      Result  : out    Syscall_Return
   ) is
      pragma SPARK_Mode (Off);
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Balance,
         Return_Val => Get_Balance (Ctx, Address),
         Error_Code => Error_None
      );
   end Sys_Handle_Balance;

   procedure Sys_Handle_SelfBalance (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
      pragma SPARK_Mode (Off);
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_SelfBalance,
         Return_Val => Get_Balance (Ctx, Get_Address (Ctx)),
         Error_Code => Error_None
      );
   end Sys_Handle_SelfBalance;

   ---------------------------------------------------------------------------
   --  Call Syscalls
   ---------------------------------------------------------------------------

   procedure Sys_Handle_Call (
      Ctx       : in Out Execution_Context;
      Target    : in     Contract_Address;
      Value     : in     U256;
      Gas_Limit : in     Gas_Amount;
      Data_Hash : in     Hash256;
      Data_Size : in     Natural;
      Result    : out    Syscall_Return
   ) is
      pragma Unreferenced (Target, Value, Gas_Limit, Data_Hash, Data_Size);
      Success : Boolean;
   begin
      --  Check call depth
      if Current_Depth (Ctx) >= Max_Call_Depth then
         Result := (False, 0, U256_Zero, Error_Call_Depth);
         return;
      end if;

      --  Placeholder: actual implementation would execute contract
      Enter_Call (Ctx, Get_Address (Ctx), Address_Zero, U256_Zero,
                  0, Call, Success);
      if Success then
         Result := (True, Gas_Call, U256_One, Error_None);
      else
         Result := (False, 0, U256_Zero, Error_Call_Depth);
      end if;
   end Sys_Handle_Call;

   procedure Sys_Handle_StaticCall (
      Ctx       : in Out Execution_Context;
      Target    : in     Contract_Address;
      Gas_Limit : in     Gas_Amount;
      Data_Hash : in     Hash256;
      Data_Size : in     Natural;
      Result    : out    Syscall_Return
   ) is
      pragma Unreferenced (Target, Gas_Limit, Data_Hash, Data_Size);
      Success : Boolean;
   begin
      if Current_Depth (Ctx) >= Max_Call_Depth then
         Result := (False, 0, U256_Zero, Error_Call_Depth);
         return;
      end if;

      Enter_Call (Ctx, Get_Address (Ctx), Address_Zero, U256_Zero,
                  0, Static_Call, Success);
      if Success then
         Result := (True, Gas_StaticCall, U256_One, Error_None);
      else
         Result := (False, 0, U256_Zero, Error_Call_Depth);
      end if;
   end Sys_Handle_StaticCall;

   procedure Sys_Handle_DelegateCall (
      Ctx       : in Out Execution_Context;
      Target    : in     Contract_Address;
      Gas_Limit : in     Gas_Amount;
      Data_Hash : in     Hash256;
      Data_Size : in     Natural;
      Result    : out    Syscall_Return
   ) is
      pragma Unreferenced (Target, Gas_Limit, Data_Hash, Data_Size);
      Success : Boolean;
   begin
      if Current_Depth (Ctx) >= Max_Call_Depth then
         Result := (False, 0, U256_Zero, Error_Call_Depth);
         return;
      end if;

      Enter_Call (Ctx, Get_Address (Ctx), Address_Zero, U256_Zero,
                  0, Delegate_Call, Success);
      if Success then
         Result := (True, Gas_DelegateCall, U256_One, Error_None);
      else
         Result := (False, 0, U256_Zero, Error_Call_Depth);
      end if;
   end Sys_Handle_DelegateCall;

   ---------------------------------------------------------------------------
   --  Control Syscalls
   ---------------------------------------------------------------------------

   procedure Sys_Handle_Return (
      Ctx         : in Out Execution_Context;
      Return_Data : in     Aegis_Contract.Return_Data;
      Result      : out    Syscall_Return
   ) is
      Exec_Result : Execution_Result;
   begin
      Finalize_Success (Ctx, Return_Data, Exec_Result);
      Result := (
         Success    => True,
         Gas_Used   => Exec_Result.Gas_Used,
         Return_Val => U256_Zero,
         Error_Code => Error_None
      );
   end Sys_Handle_Return;

   procedure Sys_Handle_Revert (
      Ctx         : in Out Execution_Context;
      Revert_Data : in     Hash256;
      Result      : out    Syscall_Return
   ) is
      Exec_Result : Execution_Result;
   begin
      Finalize_Revert (Ctx, Revert_Data, Exec_Result);
      Result := (
         Success    => False,
         Gas_Used   => Exec_Result.Gas_Used,
         Return_Val => U256_Zero,
         Error_Code => Error_Revert
      );
   end Sys_Handle_Revert;

   procedure Sys_Handle_Stop (
      Ctx    : in Out Execution_Context;
      Result : out    Syscall_Return
   ) is
      Exec_Result : Execution_Result;
   begin
      Finalize_Success (Ctx, Empty_Return, Exec_Result);
      Result := (
         Success    => True,
         Gas_Used   => Exec_Result.Gas_Used,
         Return_Val => U256_Zero,
         Error_Code => Error_None
      );
   end Sys_Handle_Stop;

   ---------------------------------------------------------------------------
   --  Event Syscalls
   ---------------------------------------------------------------------------

   procedure Sys_Handle_Log (
      Ctx         : in Out Execution_Context;
      Topics      : in     Topic_Array;
      Topic_Count : in     Natural;
      Data_Hash   : in     Hash256;
      Data_Size   : in     Natural;
      Result      : out    Syscall_Return
   ) is
      pragma Unreferenced (Data_Hash);
      Success : Boolean;
      Log_Data : Log_Data_Buffer := (others => 0);
   begin
      if not Can_Modify_State (Ctx) then
         Result := (False, 0, U256_Zero, Error_State_Modify);
         return;
      end if;

      --  Validate Gas_Log preconditions before calling
      if Topic_Count > 4 or Data_Size > Max_Log_Data_Length then
         Result := (False, 0, U256_Zero, Error_Invalid_Args);
         return;
      end if;

      Emit_Log (Ctx, Topics, Topic_Count, Log_Data, Data_Size, Success);
      if Success then
         --  Precondition satisfied: Topic_Count <= 4 and Data_Size <= Max_Log_Data_Length
         Result := (True, Gas_Log (Topic_Count, Data_Size), U256_Zero, Error_None);
      else
         Result := (False, 0, U256_Zero, Error_Out_Of_Gas);
      end if;
   end Sys_Handle_Log;

   ---------------------------------------------------------------------------
   --  Capability Checking
   ---------------------------------------------------------------------------

   function Is_Syscall_Allowed (
      Capabilities : Capability_Mask;
      Syscall      : Syscall_Number
   ) return Boolean is
   begin
      case Syscall is
         --  Read operations always allowed
         when Sys_Caller | Sys_Address | Sys_CallValue | Sys_CallData |
              Sys_CallDataSize | Sys_BlockNumber | Sys_Timestamp |
              Sys_GasPrice | Sys_GasRemaining | Sys_Balance | Sys_SelfBalance =>
            return True;

         --  Storage read
         when Sys_SLoad =>
            return Capabilities (Cap_Read_Storage);

         --  Storage write
         when Sys_SStore =>
            return Capabilities (Cap_Write_Storage);

         --  Crypto operations
         when Sys_SHA3 | Sys_Keccak256 | Sys_MLDSA_Verify | Sys_MLKEM_Decaps =>
            return Capabilities (Cap_Crypto);

         --  External calls
         when Sys_Call | Sys_StaticCall | Sys_DelegateCall =>
            return Capabilities (Cap_Call);

         --  Contract creation
         when Sys_Create | Sys_Create2 =>
            return Capabilities (Cap_Create);

         --  Control flow
         when Sys_Return | Sys_Revert | Sys_Stop =>
            return True;

         --  Destruction
         when Sys_SelfDestruct =>
            return Capabilities (Cap_Self_Destruct);

         --  Events
         when Sys_Log0 | Sys_Log1 | Sys_Log2 | Sys_Log3 | Sys_Log4 =>
            return Capabilities (Cap_Event);
      end case;
   end Is_Syscall_Allowed;

   function Can_Modify_State (
      Ctx : Execution_Context
   ) return Boolean is
   begin
      return Ctx.Mode /= Mode_Static and
             Ctx.Sandbox.Current_Frame.Capabilities (Cap_Write_Storage);
   end Can_Modify_State;

   function Can_Transfer_Value (
      Ctx : Execution_Context
   ) return Boolean is
   begin
      return Ctx.Mode /= Mode_Static and
             Ctx.Sandbox.Current_Frame.Capabilities (Cap_Transfer);
   end Can_Transfer_Value;

end Aegis_Syscall;
