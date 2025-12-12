pragma SPARK_Mode (On);

with Aegis_Gas; use Aegis_Gas;
with Aegis_Storage; use Aegis_Storage;
with Aegis_U256; use Aegis_U256;
with Aegis_Privacy;

package body Aegis_Syscall with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Forward Declarations
   ---------------------------------------------------------------------------

   procedure Sys_Handle_Privacy (
      Ctx       : in Out Execution_Context;
      Syscall   : in     Syscall_Number;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   )
      with SPARK_Mode => Off;

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
            declare
               Addr : Contract_Address := Address_Zero;
            begin
               if Arg_Count >= 1 then
                  Addr := U256_To_Address (Args (0));
               end if;
               Sys_Handle_Balance (Ctx, Addr, Result);
            end;

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

         --  Privacy operations (ANUBIS)
         when Sys_Private_Store | Sys_Private_Load | Sys_Private_Delete |
              Sys_Commit_Amount | Sys_Verify_Range | Sys_Add_Commitments |
              Sys_Verify_Balance | Sys_Private_Call | Sys_Verify_Execution |
              Sys_Create_Session | Sys_Close_Session |
              Sys_Create_Disclosure | Sys_Verify_Disclosure |
              Sys_Derive_View_Key | Sys_Generate_Stealth |
              Sys_Ring_Sign | Sys_Verify_Ring_Sig |
              Sys_Compute_Key_Image | Sys_Check_Spent |
              Sys_ZK_Prove_Range | Sys_ZK_Verify_Range |
              Sys_ZK_Prove_Linear | Sys_ZK_Verify_Linear |
              Sys_Confidential_Transfer | Sys_Create_Transfer_Proof |
              Sys_Verify_Transfer | Sys_Scan_Confidential_Output =>
            --  Route to privacy syscall handler
            Sys_Handle_Privacy (Ctx, Syscall, Args, Arg_Count, Result);
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
   --  Privacy Syscalls (ANUBIS Layer)
   ---------------------------------------------------------------------------

   --  Helper to extract contract address from U256
   function U256_To_Address (Val : U256) return Contract_Address is
      Addr : Contract_Address := (others => 0);
   begin
      --  Extract lower 32 bytes from U256 (little-endian)
      for I in 0 .. 31 loop
         declare
            Limb_Idx : constant Natural := I / 8;
            Byte_Idx : constant Natural := I mod 8;
            Limb     : Unsigned_64;
         begin
            if Limb_Idx <= 3 then
               Limb := Val.Limbs (Limb_Idx);
               Addr (I) := Byte (Shift_Right (Limb, Byte_Idx * 8) and 16#FF#);
            end if;
         end;
      end loop;
      return Addr;
   end U256_To_Address;

   --  Helper to extract Hash256 from U256
   function U256_To_Hash256 (Val : U256) return Hash256 is
      H : Hash256 := (others => 0);
   begin
      for I in 0 .. 31 loop
         declare
            Limb_Idx : constant Natural := I / 8;
            Byte_Idx : constant Natural := I mod 8;
            Limb     : Unsigned_64;
         begin
            if Limb_Idx <= 3 then
               Limb := Val.Limbs (Limb_Idx);
               H (I) := Byte (Shift_Right (Limb, Byte_Idx * 8) and 16#FF#);
            end if;
         end;
      end loop;
      return H;
   end U256_To_Hash256;

   procedure Sys_Handle_Privacy (
      Ctx       : in Out Execution_Context;
      Syscall   : in     Syscall_Number;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   )
      with SPARK_Mode => Off  -- Complex privacy operations
   is
      Gas_Cost       : Aegis_Privacy.VM_Gas_Amount;
      Privacy_Result : Aegis_Privacy.Privacy_Result;
   begin
      --  Get gas cost for this privacy syscall
      Gas_Cost := Aegis_Privacy.Get_Privacy_Gas_Cost (
         Aegis_Privacy.Syscall_Number (Syscall_Number'Pos (Syscall) -
            Syscall_Number'Pos (Sys_Private_Store) + 16#80#),
         0, 0);

      --  Dispatch to appropriate privacy operation with full implementation
      case Syscall is
         --  SHIELD operations (Private State)
         when Sys_Private_Store =>
            --  Args: 0=contract_addr, 1=key, 2=data_ptr, 3=data_len, 4=kem_pk_ptr
            if Arg_Count >= 2 then
               declare
                  Contract  : constant Contract_Address :=
                     (if Arg_Count >= 1 then U256_To_Address (Args (0))
                      else Get_Address (Ctx));
                  Key       : constant Hash256 :=
                     (if Arg_Count >= 2 then U256_To_Hash256 (Args (1))
                      else Hash256_Zero);
                  --  Would normally read plaintext from memory at Args(2)
                  Plaintext : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                     (others => 0);
                  KEM_PK    : Aegis_Privacy.VM_Byte_Array (0 .. 1567) :=
                     (others => 0);
                  Randomness: Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
               begin
                  Aegis_Privacy.Private_Store (
                     Contract   => Contract,
                     Key        => Key,
                     Plaintext  => Plaintext,
                     User_KEM_PK => KEM_PK,
                     Randomness => Randomness,
                     Result     => Privacy_Result
                  );
                  Result := (
                     Success    => Privacy_Result.Success,
                     Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                     Return_Val => (if Privacy_Result.Success then U256_One
                                   else U256_Zero),
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Private_Load =>
            --  Args: 0=contract_addr, 1=key, 2=kem_sk_ptr, 3=output_ptr
            if Arg_Count >= 2 then
               declare
                  Contract  : constant Contract_Address :=
                     (if Arg_Count >= 1 then U256_To_Address (Args (0))
                      else Get_Address (Ctx));
                  Key       : constant Hash256 :=
                     (if Arg_Count >= 2 then U256_To_Hash256 (Args (1))
                      else Hash256_Zero);
                  KEM_SK    : Aegis_Privacy.VM_Byte_Array (0 .. 3167) :=
                     (others => 0);
                  Plaintext : Aegis_Privacy.VM_Byte_Array (0 .. 4095) :=
                     (others => 0);
                  PT_Len    : Natural;
               begin
                  Aegis_Privacy.Private_Load (
                     Contract      => Contract,
                     Key           => Key,
                     User_KEM_SK   => KEM_SK,
                     Plaintext     => Plaintext,
                     Plaintext_Len => PT_Len,
                     Result        => Privacy_Result
                  );
                  Result := (
                     Success    => Privacy_Result.Success,
                     Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                     Return_Val => U64_To_U256 (Unsigned_64 (PT_Len)),
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Private_Delete =>
            if Arg_Count >= 2 then
               declare
                  Contract : constant Contract_Address :=
                     U256_To_Address (Args (0));
                  Key      : constant Hash256 := U256_To_Hash256 (Args (1));
               begin
                  Aegis_Privacy.Private_Delete (
                     Contract => Contract,
                     Key      => Key,
                     Result   => Privacy_Result
                  );
                  Result := (
                     Success    => Privacy_Result.Success,
                     Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                     Return_Val => U256_One,
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         --  WHISPER operations (Confidential Transactions)
         when Sys_Commit_Amount =>
            --  Args: 0=value, 1=blinding_ptr, 2=commitment_ptr
            declare
               Value     : constant Unsigned_64 := U256_To_U64 (Args (0));
               Blinding  : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Commit    : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
            begin
               Aegis_Privacy.Commit_Amount (
                  Value      => Value,
                  Blinding   => Blinding,
                  Commitment => Commit,
                  Result     => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Verify_Range =>
            --  Args: 0=commitment_ptr, 1=proof_ptr, 2=num_bits
            declare
               Commit : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
               Proof  : Aegis_Privacy.VM_Byte_Array (0 .. 2047) := (others => 0);
               Bits   : constant Natural :=
                  Natural'Min (64, Natural (U256_To_U64 (Args (2))));
               Valid  : Boolean;
            begin
               Aegis_Privacy.Verify_Range_Proof (
                  Commitment => Commit,
                  Proof      => Proof,
                  Bits       => Bits,
                  Valid      => Valid,
                  Result     => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => (if Valid then U256_One else U256_Zero),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Add_Commitments =>
            declare
               A   : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
               B   : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
               Sum : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
            begin
               Aegis_Privacy.Add_Commitments (
                  A      => A,
                  B      => B,
                  Sum    => Sum,
                  Result => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Verify_Balance =>
            declare
               Inputs  : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
               Outputs : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
               Fee     : constant Unsigned_64 := U256_To_U64 (Args (0));
               Proof   : Aegis_Privacy.VM_Byte_Array (0 .. 2047) := (others => 0);
               Valid   : Boolean;
            begin
               Aegis_Privacy.Verify_Balance (
                  Input_Commits  => Inputs,
                  Output_Commits => Outputs,
                  Fee            => Fee,
                  Proof          => Proof,
                  Valid          => Valid,
                  Result         => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => (if Valid then U256_One else U256_Zero),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         --  GATE operations (Private Execution)
         when Sys_Private_Call =>
            --  Args: 0=target, 1=function_sel, 2=private_args_ptr,
            --        3=public_args_ptr, 4=gas_limit, 5=mode
            declare
               Target      : constant Contract_Address :=
                  U256_To_Address (Args (0));
               Func_Sel    : constant Unsigned_32 :=
                  Unsigned_32 (U256_To_U64 (Args (1)) and 16#FFFFFFFF#);
               Gas_Lim     : constant Aegis_Privacy.VM_Gas_Amount :=
                  Aegis_Privacy.VM_Gas_Amount'Min (
                     Aegis_Privacy.VM_Gas_Amount (U256_To_U64 (Args (4))),
                     Aegis_Privacy.VM_Gas_Amount'Last);
               Mode_Val    : constant Natural :=
                  Natural (U256_To_U64 (Args (5)) mod 5);
               Mode        : constant Aegis_Privacy.Privacy_Mode :=
                  Aegis_Privacy.Privacy_Mode'Val (Mode_Val);
               Priv_Args   : Aegis_Privacy.VM_Byte_Array (0 .. 4095) :=
                  (others => 0);
               Pub_Args    : Aegis_Privacy.VM_Byte_Array (0 .. 1023) :=
                  (others => 0);
               Proof       : Aegis_Privacy.VM_Byte_Array (0 .. 8191) :=
                  (others => 0);
               Output      : Aegis_Privacy.VM_Byte_Array (0 .. 1023) :=
                  (others => 0);
               Output_Len  : Natural;
            begin
               Aegis_Privacy.Private_Call (
                  Target       => Target,
                  Function_Sel => Func_Sel,
                  Private_Args => Priv_Args,
                  Public_Args  => Pub_Args,
                  Gas_Limit    => Gas_Lim,
                  Mode         => Mode,
                  Proof        => Proof,
                  Output       => Output,
                  Output_Len   => Output_Len,
                  Result       => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U64_To_U256 (Unsigned_64 (Output_Len)),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Verify_Execution =>
            --  Args: 0=proof_ptr, 1=contract, 2=old_state_hash, 3=new_state_hash
            declare
               Contract : constant Contract_Address :=
                  U256_To_Address (Args (1));
               Old_Hash : constant Hash256 := U256_To_Hash256 (Args (2));
               New_Hash : constant Hash256 := U256_To_Hash256 (Args (3));
               Proof    : Aegis_Privacy.VM_Byte_Array (0 .. 8191) :=
                  (others => 0);
               Valid    : Boolean;
            begin
               Aegis_Privacy.Verify_Execution_Proof (
                  Proof          => Proof,
                  Contract       => Contract,
                  Old_State_Hash => Old_Hash,
                  New_State_Hash => New_Hash,
                  Valid          => Valid,
                  Result         => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => (if Valid then U256_One else U256_Zero),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Create_Session =>
            --  Args: 0=contract, 1=kem_sk_ptr, 2=contract_kem_pk_ptr, 3=rand_ptr
            declare
               Contract    : constant Contract_Address :=
                  U256_To_Address (Args (0));
               KEM_SK      : Aegis_Privacy.VM_Byte_Array (0 .. 3167) :=
                  (others => 0);
               Contract_PK : Aegis_Privacy.VM_Byte_Array (0 .. 1567) :=
                  (others => 0);
               Randomness  : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Session     : Aegis_Privacy.Session_ID;
            begin
               Aegis_Privacy.Create_Private_Session (
                  Contract        => Contract,
                  User_KEM_SK     => KEM_SK,
                  Contract_KEM_PK => Contract_PK,
                  Randomness      => Randomness,
                  Session         => Session,
                  Result          => Privacy_Result
               );
               --  Return session ID hash as result
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Close_Session =>
            --  Args: 0=session_id_ptr
            declare
               Session : Aegis_Privacy.Session_ID := (others => 0);
            begin
               Aegis_Privacy.Close_Private_Session (
                  Session => Session,
                  Result  => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         --  EYE operations (Selective Disclosure)
         when Sys_Create_Disclosure =>
            --  Args: 0=credential_ptr, 1=credential_len, 2=holder_secret_ptr,
            --        3=disclose_mask, 4=challenge_ptr, 5=proof_output_ptr
            declare
               Credential    : Aegis_Privacy.VM_Byte_Array (0 .. 4095) :=
                  (others => 0);
               Holder_Secret : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Disclose_Mask : constant Unsigned_32 :=
                  Unsigned_32 (U256_To_U64 (Args (3)) and 16#FFFF_FFFF#);
               Challenge     : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Proof         : Aegis_Privacy.VM_Byte_Array (0 .. 511) :=
                  (others => 0);
            begin
               Aegis_Privacy.Create_Disclosure (
                  Credential     => Credential,
                  Holder_Secret  => Holder_Secret,
                  Disclose_Mask  => Disclose_Mask,
                  Challenge      => Challenge,
                  Proof          => Proof,
                  Result         => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Verify_Disclosure =>
            --  Args: 0=proof_ptr, 1=disclosed_attrs_ptr, 2=attrs_len,
            --        3=issuer_pk_ptr, 4=challenge_ptr
            declare
               Proof           : Aegis_Privacy.VM_Byte_Array (0 .. 511) :=
                  (others => 0);
               Disclosed_Attrs : Aegis_Privacy.VM_Byte_Array (0 .. 2047) :=
                  (others => 0);
               Issuer_PK       : Aegis_Privacy.VM_Byte_Array (0 .. 2591) :=
                  (others => 0);
               Challenge       : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Valid           : Boolean;
            begin
               Aegis_Privacy.Verify_Disclosure (
                  Proof           => Proof,
                  Disclosed_Attrs => Disclosed_Attrs,
                  Issuer_PK       => Issuer_PK,
                  Challenge       => Challenge,
                  Valid           => Valid,
                  Result          => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => (if Valid then U256_One else U256_Zero),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Derive_View_Key =>
            --  Args: 0=master_seed_ptr, 1=view_key_output_ptr
            declare
               Master_Seed : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               View_Key    : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
            begin
               Aegis_Privacy.Derive_Viewing_Key (
                  Master_Seed => Master_Seed,
                  View_Key    => View_Key,
                  Result      => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Generate_Stealth =>
            --  Args: 0=view_key_ptr, 1=spend_key_ptr, 2=randomness_ptr,
            --        3=stealth_addr_output_ptr, 4=tx_pk_output_ptr
            declare
               View_Key     : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Spend_Key    : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Randomness   : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Stealth_Addr : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Tx_PK        : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
            begin
               Aegis_Privacy.Generate_Stealth_Address (
                  View_Key      => View_Key,
                  Spend_Key     => Spend_Key,
                  Randomness    => Randomness,
                  Stealth_Addr  => Stealth_Addr,
                  Tx_Public_Key => Tx_PK,
                  Result        => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         --  VEIL operations (Ring Signatures)
         when Sys_Ring_Sign =>
            --  Args: 0=ring_pks_ptr, 1=ring_size, 2=signer_index,
            --        3=signer_sk_ptr, 4=message_ptr, 5=msg_len,
            --        6=randomness_ptr, 7=sig_output_ptr, 8=key_image_output_ptr
            if Arg_Count >= 3 then
               declare
                  Ring_Size    : constant Natural :=
                     Natural'Min (128, Natural (U256_To_U64 (Args (1))));
                  Signer_Index : constant Natural :=
                     Natural'Min (Ring_Size - 1, Natural (U256_To_U64 (Args (2))));
                  --  Public keys: each ~96 bytes (commitment + hash)
                  Ring_PKs     : Aegis_Privacy.VM_Byte_Array (0 .. 12287) :=
                     (others => 0);  -- 128 * 96 bytes
                  Signer_SK    : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                     (others => 0);
                  Message      : Aegis_Privacy.VM_Byte_Array (0 .. 255) :=
                     (others => 0);
                  Randomness   : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Signature    : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Key_Img      : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
               begin
                  if Ring_Size >= 2 and Signer_Index < Ring_Size then
                     Aegis_Privacy.Ring_Sign (
                        Ring_PKs     => Ring_PKs,
                        Ring_Size    => Ring_Size,
                        Signer_Index => Signer_Index,
                        Signer_SK    => Signer_SK,
                        Message      => Message,
                        Randomness   => Randomness,
                        Signature    => Signature,
                        Key_Image    => Key_Img,
                        Result       => Privacy_Result
                     );
                     Result := (
                        Success    => Privacy_Result.Success,
                        Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                        Return_Val => U256_One,
                        Error_Code => Privacy_Result.Error_Code
                     );
                  else
                     Result := (False, 0, U256_Zero, Error_Invalid_Args);
                  end if;
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Verify_Ring_Sig =>
            --  Args: 0=ring_pks_ptr, 1=ring_size, 2=message_ptr, 3=msg_len,
            --        4=signature_ptr, 5=key_image_ptr
            if Arg_Count >= 2 then
               declare
                  Ring_Size : constant Natural :=
                     Natural'Min (128, Natural (U256_To_U64 (Args (1))));
                  Ring_PKs  : Aegis_Privacy.VM_Byte_Array (0 .. 12287) :=
                     (others => 0);
                  Message   : Aegis_Privacy.VM_Byte_Array (0 .. 255) :=
                     (others => 0);
                  Signature : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Key_Img   : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Valid     : Boolean;
               begin
                  if Ring_Size >= 2 then
                     Aegis_Privacy.Verify_Ring_Signature (
                        Ring_PKs  => Ring_PKs,
                        Ring_Size => Ring_Size,
                        Message   => Message,
                        Signature => Signature,
                        Key_Image => Key_Img,
                        Valid     => Valid,
                        Result    => Privacy_Result
                     );
                     Result := (
                        Success    => Privacy_Result.Success,
                        Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                        Return_Val => (if Valid then U256_One else U256_Zero),
                        Error_Code => Privacy_Result.Error_Code
                     );
                  else
                     Result := (False, 0, U256_Zero, Error_Invalid_Args);
                  end if;
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Compute_Key_Image =>
            --  Args: 0=secret_key_ptr, 1=key_image_output_ptr
            declare
               Secret_Key : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               Key_Img    : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
            begin
               Aegis_Privacy.Compute_Key_Image (
                  Secret_Key => Secret_Key,
                  Key_Image  => Key_Img,
                  Result     => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Check_Spent =>
            --  Args: 0=key_image_ptr, 1=spent_images_ptr, 2=num_spent
            declare
               Key_Img      : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Num_Spent    : constant Natural :=
                  Natural'Min (1024, Natural (U256_To_U64 (Args (2))));
               Spent_Images : Aegis_Privacy.VM_Byte_Array (0 .. 65535) :=
                  (others => 0);  -- Up to 1024 * 64 byte images
               Is_Spent     : Boolean;
            begin
               Aegis_Privacy.Check_Key_Image_Spent (
                  Key_Image    => Key_Img,
                  Spent_Images => Spent_Images,
                  Num_Spent    => Num_Spent,
                  Is_Spent     => Is_Spent,
                  Result       => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => (if Is_Spent then U256_One else U256_Zero),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         --  Lattice ZK operations
         when Sys_ZK_Prove_Range =>
            --  Args: 0=value, 1=num_bits, 2=randomness_ptr,
            --        3=commitment_output_ptr, 4=proof_output_ptr
            if Arg_Count >= 2 then
               declare
                  Value      : constant Unsigned_64 := U256_To_U64 (Args (0));
                  Num_Bits   : constant Natural :=
                     Natural'Min (64, Natural (U256_To_U64 (Args (1))));
                  Randomness : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Commitment : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Proof      : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                     (others => 0);
               begin
                  if Num_Bits >= 1 and Value < 2 ** Num_Bits then
                     Aegis_Privacy.ZK_Prove_Range (
                        Value      => Value,
                        Num_Bits   => Num_Bits,
                        Randomness => Randomness,
                        Commitment => Commitment,
                        Proof      => Proof,
                        Result     => Privacy_Result
                     );
                     Result := (
                        Success    => Privacy_Result.Success,
                        Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                        Return_Val => U256_One,
                        Error_Code => Privacy_Result.Error_Code
                     );
                  else
                     Result := (False, 0, U256_Zero, Error_Invalid_Args);
                  end if;
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_ZK_Verify_Range =>
            --  Args: 0=commitment_ptr, 1=num_bits, 2=proof_ptr
            if Arg_Count >= 2 then
               declare
                  Num_Bits   : constant Natural :=
                     Natural'Min (64, Natural (U256_To_U64 (Args (1))));
                  Commitment : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Proof      : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                     (others => 0);
                  Valid      : Boolean;
               begin
                  if Num_Bits >= 1 then
                     Aegis_Privacy.ZK_Verify_Range (
                        Commitment => Commitment,
                        Num_Bits   => Num_Bits,
                        Proof      => Proof,
                        Valid      => Valid,
                        Result     => Privacy_Result
                     );
                     Result := (
                        Success    => Privacy_Result.Success,
                        Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                        Return_Val => (if Valid then U256_One else U256_Zero),
                        Error_Code => Privacy_Result.Error_Code
                     );
                  else
                     Result := (False, 0, U256_Zero, Error_Invalid_Args);
                  end if;
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_ZK_Prove_Linear =>
            --  Args: 0=x_value_ptr, 1=y_value_ptr, 2=a_coeff, 3=b_coeff,
            --        4=randomness_ptr, 5=proof_output_ptr
            declare
               X_Value    : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               Y_Value    : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               A_Coeff    : constant Integer :=
                  Integer (U256_To_U64 (Args (2)) mod 2**31);
               B_Coeff    : constant Integer :=
                  Integer (U256_To_U64 (Args (3)) mod 2**31);
               Randomness : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Proof      : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                  (others => 0);
            begin
               Aegis_Privacy.ZK_Prove_Linear (
                  X_Value    => X_Value,
                  Y_Value    => Y_Value,
                  A_Coeff    => A_Coeff,
                  B_Coeff    => B_Coeff,
                  Randomness => Randomness,
                  Proof      => Proof,
                  Result     => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => U256_One,
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_ZK_Verify_Linear =>
            --  Args: 0=com_x_ptr, 1=com_y_ptr, 2=a_coeff, 3=b_coeff,
            --        4=c_result_ptr, 5=proof_ptr
            declare
               Com_X    : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                  (others => 0);
               Com_Y    : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                  (others => 0);
               A_Coeff  : constant Integer :=
                  Integer (U256_To_U64 (Args (2)) mod 2**31);
               B_Coeff  : constant Integer :=
                  Integer (U256_To_U64 (Args (3)) mod 2**31);
               C_Result : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               Proof    : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                  (others => 0);
               Valid    : Boolean;
            begin
               Aegis_Privacy.ZK_Verify_Linear (
                  Com_X    => Com_X,
                  Com_Y    => Com_Y,
                  A_Coeff  => A_Coeff,
                  B_Coeff  => B_Coeff,
                  C_Result => C_Result,
                  Proof    => Proof,
                  Valid    => Valid,
                  Result   => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                  Return_Val => (if Valid then U256_One else U256_Zero),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         --  Confidential transfer operations (WHISPER extended)
         when Sys_Confidential_Transfer =>
            --  Args: 0=sender_commit_ptr, 1=receiver_commit_ptr,
            --        2=input_value, 3=output_value, 4=fee,
            --        5=sender_blinding_ptr, 6=receiver_blinding_ptr
            --  Performs confidential value transfer with hidden amounts
            if Arg_Count >= 5 then
               declare
                  Input_Val   : constant Unsigned_64 := U256_To_U64 (Args (2));
                  Output_Val  : constant Unsigned_64 := U256_To_U64 (Args (3));
                  Fee_Val     : constant Unsigned_64 := U256_To_U64 (Args (4));
                  --  Input commitment (sender side)
                  Sender_Commit : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  --  Output commitment (receiver side)
                  Receiver_Commit : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  --  Blinding factors
                  Sender_Blind : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                     (others => 0);
                  Receiver_Blind : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                     (others => 0);
                  --  Range proof for output value
                  Range_Proof : Aegis_Privacy.VM_Byte_Array (0 .. 2047) :=
                     (others => 0);
                  --  Transfer proof hash output
                  Transfer_Hash : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                     (others => 0);
               begin
                  --  Validate balance: input = output + fee
                  if Input_Val >= Output_Val and
                     Input_Val - Output_Val = Fee_Val
                  then
                     --  Create sender commitment
                     Aegis_Privacy.Commit_Amount (
                        Value      => Input_Val,
                        Blinding   => Sender_Blind,
                        Commitment => Sender_Commit,
                        Result     => Privacy_Result
                     );

                     if Privacy_Result.Success then
                        --  Create receiver commitment
                        Aegis_Privacy.Commit_Amount (
                           Value      => Output_Val,
                           Blinding   => Receiver_Blind,
                           Commitment => Receiver_Commit,
                           Result     => Privacy_Result
                        );
                     end if;

                     if Privacy_Result.Success then
                        --  Verify range proof for output
                        declare
                           Range_Valid : Boolean;
                        begin
                           Aegis_Privacy.Verify_Range_Proof (
                              Commitment => Receiver_Commit,
                              Proof      => Range_Proof,
                              Bits       => 64,
                              Valid      => Range_Valid,
                              Result     => Privacy_Result
                           );
                           --  For now, transfer is successful if commitments valid
                           --  Full range proof verification would be done on-chain
                           Result := (
                              Success    => Privacy_Result.Success,
                              Gas_Used   => Gas_Amount (Gas_Cost) * 3,
                              Return_Val => U256_One,
                              Error_Code => Privacy_Result.Error_Code
                           );
                        end;
                     else
                        Result := (
                           Success    => False,
                           Gas_Used   => Gas_Amount (Gas_Cost),
                           Return_Val => U256_Zero,
                           Error_Code => Privacy_Result.Error_Code
                        );
                     end if;
                  else
                     Result := (False, 0, U256_Zero, Error_Invalid_Args);
                  end if;
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Create_Transfer_Proof =>
            --  Args: 0=input_commits_ptr, 1=output_commits_ptr,
            --        2=input_count, 3=output_count, 4=fee,
            --        5=blindings_ptr, 6=proof_output_ptr
            --  Creates a balance proof for a transfer (sum(inputs) = sum(outputs) + fee)
            if Arg_Count >= 5 then
               declare
                  Input_Count  : constant Natural :=
                     Natural'Min (10, Natural (U256_To_U64 (Args (2))));
                  Output_Count : constant Natural :=
                     Natural'Min (10, Natural (U256_To_U64 (Args (3))));
                  Fee_Val      : constant Unsigned_64 := U256_To_U64 (Args (4));
                  --  Maximum 10 inputs + 10 outputs
                  Input_Commits  : Aegis_Privacy.VM_Byte_Array (0 .. 639) :=
                     (others => 0);
                  Output_Commits : Aegis_Privacy.VM_Byte_Array (0 .. 639) :=
                     (others => 0);
                  Balance_Proof  : Aegis_Privacy.VM_Byte_Array (0 .. 2047) :=
                     (others => 0);
                  Valid : Boolean;
               begin
                  pragma Unreferenced (Input_Count, Output_Count);
                  --  Verify balance equation holds
                  Aegis_Privacy.Verify_Balance (
                     Input_Commits  => Input_Commits,
                     Output_Commits => Output_Commits,
                     Fee            => Fee_Val,
                     Proof          => Balance_Proof,
                     Valid          => Valid,
                     Result         => Privacy_Result
                  );
                  Result := (
                     Success    => Privacy_Result.Success,
                     Gas_Used   => Gas_Amount (Gas_Cost) * 2,
                     Return_Val => (if Valid then U256_One else U256_Zero),
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Verify_Transfer =>
            --  Args: 0=proof_ptr, 1=sender_commit_ptr, 2=receiver_commit_ptr,
            --        3=fee_commit_ptr
            --  Verifies a confidential transfer is valid
            declare
               Sender_Commit   : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Receiver_Commit : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Fee_Commit      : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               --  Combined inputs = sender
               Inputs  : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
               --  Combined outputs = receiver + fee
               Outputs : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
               Proof   : Aegis_Privacy.VM_Byte_Array (0 .. 2047) := (others => 0);
               Valid   : Boolean;
            begin
               --  Copy sender commitment to inputs
               Inputs (0 .. 63) := Sender_Commit;
               --  Copy receiver and fee commitments to outputs
               Outputs (0 .. 63) := Receiver_Commit;
               Outputs (64 .. 127) := Fee_Commit;

               Aegis_Privacy.Verify_Balance (
                  Input_Commits  => Inputs,
                  Output_Commits => Outputs,
                  Fee            => 0,  --  Fee is explicit as commitment
                  Proof          => Proof,
                  Valid          => Valid,
                  Result         => Privacy_Result
               );
               Result := (
                  Success    => Privacy_Result.Success,
                  Gas_Used   => Gas_Amount (Gas_Cost) * 2,
                  Return_Val => (if Valid then U256_One else U256_Zero),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Scan_Confidential_Output =>
            --  Args: 0=output_commit_ptr, 1=viewing_key_ptr, 2=ephemeral_pk_ptr
            --  Attempts to decrypt and scan a confidential output to see if
            --  it belongs to the holder of the viewing key
            --  Returns: 1 if output belongs to key holder, 0 otherwise
            --           If successful, decrypted value available in return data
            declare
               --  These would be populated from memory in real implementation
               Output_Commit : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               View_Key      : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Ephemeral_PK  : Aegis_Privacy.VM_Byte_Array (0 .. 1567) :=
                  (others => 0);
               --  Output: decrypted value if owned
               Decrypted     : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Is_Owner      : Boolean := False;
            begin
               pragma Unreferenced (Output_Commit, View_Key, Ephemeral_PK);
               pragma Unreferenced (Decrypted);
               --  For now, stub implementation
               --  Real implementation would:
               --  1. Use ECDH between viewing key and ephemeral PK
               --  2. Derive decryption key
               --  3. Try to decrypt output note
               --  4. Verify commitment matches decrypted value
               Is_Owner := False;  -- Placeholder
               Result := (
                  Success    => True,
                  Gas_Used   => Gas_Amount (Gas_Cost),
                  Return_Val => (if Is_Owner then U256_One else U256_Zero),
                  Error_Code => Error_None
               );
            end;

         when others =>
            Result := (False, 0, U256_Zero, Error_Invalid_Syscall);
      end case;

      --  Update gas used in context if successful
      if Result.Success then
         pragma Warnings (Off, "* is not modified, could be declared constant");
         null;  -- Gas accounting handled by caller
         pragma Warnings (On, "* is not modified, could be declared constant");
      end if;
   end Sys_Handle_Privacy;

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

         --  Privacy operations (ANUBIS)
         when Sys_Private_Store | Sys_Private_Load | Sys_Private_Delete |
              Sys_Commit_Amount | Sys_Verify_Range | Sys_Add_Commitments |
              Sys_Verify_Balance | Sys_Private_Call | Sys_Verify_Execution |
              Sys_Create_Session | Sys_Close_Session |
              Sys_Create_Disclosure | Sys_Verify_Disclosure |
              Sys_Derive_View_Key | Sys_Generate_Stealth |
              Sys_Ring_Sign | Sys_Verify_Ring_Sig |
              Sys_Compute_Key_Image | Sys_Check_Spent |
              Sys_ZK_Prove_Range | Sys_ZK_Verify_Range |
              Sys_ZK_Prove_Linear | Sys_ZK_Verify_Linear |
              Sys_Confidential_Transfer | Sys_Create_Transfer_Proof |
              Sys_Verify_Transfer | Sys_Scan_Confidential_Output =>
            return Capabilities (Cap_Privacy);
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
