pragma SPARK_Mode (On);

with Aegis_Gas; use Aegis_Gas;
with Aegis_Storage; use Aegis_Storage;
with Aegis_U256; use Aegis_U256;
with Aegis_Privacy;
with Sphinx_Runtime;
with Anubis_Address_Types;

package body Aegis_Syscall with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Forward Declarations
   ---------------------------------------------------------------------------

   --  Helper to convert Boolean to U256 (avoids if-expression in aggregates)
   function Bool_To_U256 (B : Boolean) return U256 is
      (if B then U256_One else U256_Zero);

   --  Helper to extract Hash256 from U256
   function U256_To_Hash256 (Val : U256) return Hash256;

   --  Internal wrapper for CALL with Args array
   procedure Sys_Handle_Call (
      Ctx       : in Out Execution_Context;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => (In_Out => Sphinx_Runtime.Current_Runtime);

   --  Internal wrapper for STATICCALL with Args array
   procedure Sys_Handle_Static_Call (
      Ctx       : in Out Execution_Context;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => (In_Out => Sphinx_Runtime.Current_Runtime);

   --  Internal wrapper for DELEGATECALL with Args array
   procedure Sys_Handle_Delegate_Call (
      Ctx       : in Out Execution_Context;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => (In_Out => Sphinx_Runtime.Current_Runtime);

   --  Internal wrapper for CREATE/CREATE2 with Args array
   procedure Sys_Handle_Create (
      Ctx        : in Out Execution_Context;
      Args       : in     Syscall_Args;
      Arg_Count  : in     Natural;
      Is_Create2 : in     Boolean;
      Result     : out    Syscall_Return
   ) with
      Global => (In_Out => Sphinx_Runtime.Current_Runtime);

   procedure Sys_Handle_Privacy (
      Ctx       : in Out Execution_Context;
      Syscall   : in     Syscall_Number;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => (Input => Sphinx_Runtime.Current_Runtime);

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
            --  Args: 0=data_offset, 1=data_size
            --  Load data from calldata and compute SHA3-256 hash
            declare
               use Sphinx_Runtime;

               Data_Offset : Natural := 0;
               Data_Size   : Natural := 0;
               Raw_Size    : Unsigned_64;

               --  Buffer for data to hash
               Input     : Hash_Input_Buffer := (others => 0);
               Output    : Hash256;
               Temp_Buf  : Aegis_VM_Types.Byte_Array (0 .. Max_Hash_Input - 1)
                  := (others => 0);
               Load_OK   : Boolean;
               Crypto_Res : Crypto_Result;
            begin
               --  Parse arguments
               if Arg_Count >= 1 then
                  Data_Offset := Natural (U256_To_U64 (Args (0)) mod 2**16);
               end if;
               if Arg_Count >= 2 then
                  Raw_Size := U256_To_U64 (Args (1));
                  --  Clamp to Natural range and Max_Hash_Input
                  if Raw_Size > Unsigned_64 (Max_Hash_Input) then
                     Data_Size := Max_Hash_Input;
                  else
                     Data_Size := Natural (Raw_Size);
                  end if;
               end if;

               --  Load data from calldata
               Load_Calldata (Data_Offset, Data_Size, Temp_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. Natural'Min (Data_Size - 1, Input'Last) loop
                     Input (I) := Byte (Temp_Buf (I));
                  end loop;
               end if;

               --  Compute SHA3-256 hash
               SHA3_256_Hash (Ctx, Input, Data_Size, Output, Crypto_Res);
               if Crypto_Res = Crypto_OK then
                  Result := (True, Gas_Hash (Data_Size), U256_Zero, Error_None);
               else
                  Result := (False, 0, U256_Zero, Error_Crypto);
               end if;
            end;

         when Sys_Keccak256 =>
            --  Args: 0=data_offset, 1=data_size
            --  Load data from calldata and compute Keccak-256 hash
            declare
               use Sphinx_Runtime;

               Data_Offset : Natural := 0;
               Data_Size   : Natural := 0;
               Raw_Size    : Unsigned_64;

               --  Buffer for data to hash
               Input     : Hash_Input_Buffer := (others => 0);
               Output    : Hash256;
               Temp_Buf  : Aegis_VM_Types.Byte_Array (0 .. Max_Hash_Input - 1)
                  := (others => 0);
               Load_OK   : Boolean;
               Crypto_Res : Crypto_Result;
            begin
               --  Parse arguments
               if Arg_Count >= 1 then
                  Data_Offset := Natural (U256_To_U64 (Args (0)) mod 2**16);
               end if;
               if Arg_Count >= 2 then
                  Raw_Size := U256_To_U64 (Args (1));
                  --  Clamp to Natural range and Max_Hash_Input
                  if Raw_Size > Unsigned_64 (Max_Hash_Input) then
                     Data_Size := Max_Hash_Input;
                  else
                     Data_Size := Natural (Raw_Size);
                  end if;
               end if;

               --  Load data from calldata
               Load_Calldata (Data_Offset, Data_Size, Temp_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. Natural'Min (Data_Size - 1, Input'Last) loop
                     Input (I) := Byte (Temp_Buf (I));
                  end loop;
               end if;

               --  Compute Keccak-256 hash
               Keccak256_Hash (Ctx, Input, Data_Size, Output, Crypto_Res);
               if Crypto_Res = Crypto_OK then
                  Result := (True, Gas_Hash (Data_Size), U256_Zero, Error_None);
               else
                  Result := (False, 0, U256_Zero, Error_Crypto);
               end if;
            end;

         when Sys_MLDSA_Verify =>
            --  Args: 0=msg_offset, 1=msg_len, 2=sig_offset, 3=pk_offset
            --  Load message, signature, and public key from calldata
            declare
               use Sphinx_Runtime;

               Msg_Offset : Natural := 0;
               Msg_Len    : Natural := 0;
               Sig_Offset : Natural := 0;
               PK_Offset  : Natural := 0;

               --  Buffers to load from calldata
               Msg_Buf    : Hash_Input_Buffer := (others => 0);
               Sig_Buf    : MLDSA87_Signature := (others => 0);
               PK_Buf     : MLDSA87_Public_Key := (others => 0);

               --  Temp buffers for loading
               Temp_Sig   : Aegis_VM_Types.Byte_Array (0 .. 4626) := (others => 0);
               Temp_PK    : Aegis_VM_Types.Byte_Array (0 .. 2591) := (others => 0);
               Temp_Msg   : Aegis_VM_Types.Byte_Array (0 .. 255) := (others => 0);

               Load_OK    : Boolean;
               Valid      : Boolean;
               Crypto_Res : Crypto_Result;
            begin
               --  Parse arguments
               if Arg_Count >= 1 then
                  Msg_Offset := Natural (U256_To_U64 (Args (0)) mod 2**16);
               end if;
               if Arg_Count >= 2 then
                  Msg_Len := Natural'Min (256,
                     Natural (U256_To_U64 (Args (1))));
               end if;
               if Arg_Count >= 3 then
                  Sig_Offset := Natural (U256_To_U64 (Args (2)) mod 2**16);
               end if;
               if Arg_Count >= 4 then
                  PK_Offset := Natural (U256_To_U64 (Args (3)) mod 2**16);
               end if;

               --  Load message from calldata
               Load_Calldata (Msg_Offset, Msg_Len, Temp_Msg, Load_OK);
               if Load_OK then
                  for I in 0 .. Natural'Min (Msg_Len - 1, Msg_Buf'Last) loop
                     Msg_Buf (I) := Byte (Temp_Msg (I));
                  end loop;
               end if;

               --  Load signature from calldata (4627 bytes)
               Load_Calldata (Sig_Offset, 4627, Temp_Sig, Load_OK);
               if Load_OK then
                  for I in Sig_Buf'Range loop
                     Sig_Buf (I) := Byte (Temp_Sig (I));
                  end loop;
               end if;

               --  Load public key from calldata (2592 bytes)
               Load_Calldata (PK_Offset, 2592, Temp_PK, Load_OK);
               if Load_OK then
                  for I in PK_Buf'Range loop
                     PK_Buf (I) := Byte (Temp_PK (I));
                  end loop;
               end if;

               --  Now verify with real data
               MLDSA87_Verify (Ctx, Msg_Buf, Msg_Len, Sig_Buf, PK_Buf,
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
            end;

         when Sys_MLKEM_Decaps =>
            --  Args: 0=ciphertext_offset, 1=decaps_key_offset
            --  Load ciphertext and decapsulation key from calldata
            declare
               use Sphinx_Runtime;

               CT_Offset : Natural := 0;
               DK_Offset : Natural := 0;

               --  Buffers to load from calldata
               CT_Buf    : MLKEM1024_Ciphertext := (others => 0);
               DK_Buf    : MLKEM1024_Decaps_Key := (others => 0);
               Shared    : MLKEM1024_Shared_Secret;

               --  Temp buffers for loading
               Temp_CT   : Aegis_VM_Types.Byte_Array (0 .. 1567) := (others => 0);
               Temp_DK   : Aegis_VM_Types.Byte_Array (0 .. 3167) := (others => 0);

               Load_OK    : Boolean;
               Crypto_Res : Crypto_Result;
            begin
               --  Parse arguments
               if Arg_Count >= 1 then
                  CT_Offset := Natural (U256_To_U64 (Args (0)) mod 2**16);
               end if;
               if Arg_Count >= 2 then
                  DK_Offset := Natural (U256_To_U64 (Args (1)) mod 2**16);
               end if;

               --  Load ciphertext from calldata (1568 bytes)
               Load_Calldata (CT_Offset, 1568, Temp_CT, Load_OK);
               if Load_OK then
                  for I in CT_Buf'Range loop
                     CT_Buf (I) := Byte (Temp_CT (I));
                  end loop;
               end if;

               --  Load decapsulation key from calldata (3168 bytes)
               Load_Calldata (DK_Offset, 3168, Temp_DK, Load_OK);
               if Load_OK then
                  for I in DK_Buf'Range loop
                     DK_Buf (I) := Byte (Temp_DK (I));
                  end loop;
               end if;

               --  Now decapsulate with real data
               MLKEM1024_Decaps (Ctx, CT_Buf, DK_Buf, Shared, Crypto_Res);
               if Crypto_Res = Crypto_OK then
                  Result := (True, Gas_MLKEM_Decaps, U256_One, Error_None);
                  --  Zeroize the shared secret
                  Zeroize_Secret (Shared);
               else
                  Result := (False, 0, U256_Zero, Error_Crypto);
               end if;
            end;

         when Sys_Caller =>
            Sys_Handle_Caller (Ctx, Result);

         when Sys_Address =>
            Sys_Handle_Address (Ctx, Result);

         when Sys_CallValue =>
            Sys_Handle_CallValue (Ctx, Result);

         when Sys_CallDataSize =>
            --  Get real calldata size from runtime
            declare
               CD_Size : constant Natural := Sphinx_Runtime.Get_Calldata_Size;
            begin
               Result := (
                  Success    => True,
                  Gas_Used   => 2,  --  CALLDATASIZE gas
                  Return_Val => U64_To_U256 (Unsigned_64 (CD_Size)),
                  Error_Code => Error_None
               );
            end;

         when Sys_CallData =>
            --  Load calldata bytes from runtime
            declare
               Offset    : Natural := 0;
               Load_Size : Natural := 32;  --  Default to 32 bytes
               Buffer    : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Success   : Boolean;
            begin
               if Arg_Count >= 1 then
                  Offset := Natural (U256_To_U64 (Args (0)) mod 2**16);
               end if;
               if Arg_Count >= 2 then
                  Load_Size := Natural'Min (32,
                     Natural (U256_To_U64 (Args (1)) mod 33));
               end if;

               Sphinx_Runtime.Load_Calldata (Offset, Load_Size, Buffer, Success);

               if Success then
                  --  Convert buffer to U256
                  declare
                     Val : U256 := U256_Zero;
                  begin
                     for I in 0 .. Natural'Min (31, Load_Size - 1) loop
                        Val.Limbs (I / 8) := Val.Limbs (I / 8) or
                           Shift_Left (Word64 (Buffer (I)), (I mod 8) * 8);
                     end loop;
                     Result := (True, 3, Val, Error_None);
                  end;
               else
                  Result := (False, 3, U256_Zero, Error_Invalid_Args);
               end if;
            end;

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

         when Sys_Call =>
            --  Execute CALL to another contract
            Sys_Handle_Call (Ctx, Args, Arg_Count, Result);

         when Sys_StaticCall =>
            --  Execute STATICCALL (read-only)
            Sys_Handle_Static_Call (Ctx, Args, Arg_Count, Result);

         when Sys_DelegateCall =>
            --  Execute DELEGATECALL (preserve context)
            Sys_Handle_Delegate_Call (Ctx, Args, Arg_Count, Result);

         when Sys_Create =>
            --  Execute CREATE (deploy new contract)
            Sys_Handle_Create (Ctx, Args, Arg_Count, False, Result);

         when Sys_Create2 =>
            --  Execute CREATE2 (deterministic deployment)
            Sys_Handle_Create (Ctx, Args, Arg_Count, True, Result);

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
      --  LEGACY: This procedure is kept for API compatibility but is no longer
      --  called from Dispatch. SHA3 syscalls are now handled inline in Dispatch
      --  with proper calldata loading. See the when Sys_SHA3 case.
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
      --  LEGACY: This procedure is kept for API compatibility but is no longer
      --  called from Dispatch. Keccak256 syscalls are now handled inline in
      --  Dispatch with proper calldata loading. See the when Sys_Keccak256 case.
      Keccak256_Hash (Ctx, Input, Data_Size, Output, Crypto_Res);
      if Crypto_Res = Crypto_OK then
         Result := (True, Gas_Hash (Data_Size), U256_Zero, Error_None);
      else
         Result := (False, 0, U256_Zero, Error_Crypto);
      end if;
   end Sys_Handle_Keccak256;

   --  Note: SHA3, Keccak256, MLDSA_Verify and MLKEM_Decaps are all handled
   --  inline in Dispatch, loading real data from calldata via
   --  Sphinx_Runtime.Load_Calldata. See the corresponding when cases.

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
      --  Call volatile function outside aggregate to satisfy SPARK rules
      Balance : constant U256 := Get_Balance (Ctx, Address);
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_Balance,
         Return_Val => Balance,
         Error_Code => Error_None
      );
   end Sys_Handle_Balance;

   procedure Sys_Handle_SelfBalance (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) is
      --  Call volatile function outside aggregate to satisfy SPARK rules
      Self_Balance : constant U256 := Get_Balance (Ctx, Get_Address (Ctx));
   begin
      Result := (
         Success    => True,
         Gas_Used   => Gas_SelfBalance,
         Return_Val => Self_Balance,
         Error_Code => Error_None
      );
   end Sys_Handle_SelfBalance;

   ---------------------------------------------------------------------------
   --  Cross-Contract Calls via Sphinx Runtime
   ---------------------------------------------------------------------------

   --  CALL via Sphinx_Runtime with full calldata support
   procedure Sys_Handle_Call (
      Ctx       : in Out Execution_Context;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) is
      use Sphinx_Runtime;
      use Anubis_Address_Types;

      Target      : Account_ID := (others => 0);
      Value       : Unsigned_64 := 0;
      Gas_Lim     : Gas_Amount := 0;
      Calldata    : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
      CD_Size     : Natural := 0;
      Return_Buf  : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
      Return_Size : Natural := 0;
      Gas_Used    : Gas_Amount := 0;
      Success     : Boolean;
   begin
      --  Check call depth
      if Current_Depth (Ctx) >= Aegis_VM_Types.Max_Call_Depth then
         Result := (False, 0, U256_Zero, Error_Call_Depth);
         return;
      end if;

      --  Parse arguments: Args(0)=target, Args(1)=value, Args(2)=gas, Args(3)=cd_size
      if Arg_Count >= 1 then
         declare
            Addr : constant Contract_Address := U256_To_Address (Args (0));
         begin
            for I in 0 .. 31 loop
               Target (I) := Addr (I);
            end loop;
         end;
      end if;

      if Arg_Count >= 2 then
         Value := U256_To_U64 (Args (1));
      end if;

      if Arg_Count >= 3 then
         Gas_Lim := Gas_Amount'Min (
            Gas_Amount (U256_To_U64 (Args (2))),
            Gas_Remaining (Ctx));
      else
         Gas_Lim := Gas_Remaining (Ctx);
      end if;

      if Arg_Count >= 4 then
         CD_Size := Natural'Min (1024, Natural (U256_To_U64 (Args (3))));
      end if;

      --  Execute call via Sphinx_Runtime
      Sphinx_Runtime.Execute_Call (
         Target      => Target,
         Value       => Value,
         Calldata    => Calldata (0 .. CD_Size - 1),
         Gas_Limit   => Gas_Lim,
         Return_Data => Return_Buf,
         Return_Size => Return_Size,
         Gas_Used    => Gas_Used,
         Success     => Success
      );

      if Success then
         Result := (True, Gas_Used + Gas_Call, U256_One, Error_None);
      else
         Result := (False, Gas_Used + Gas_Call, U256_Zero, Error_Call_Depth);
      end if;
   end Sys_Handle_Call;

   --  STATICCALL via Sphinx_Runtime
   procedure Sys_Handle_Static_Call (
      Ctx       : in Out Execution_Context;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) is
      use Sphinx_Runtime;
      use Anubis_Address_Types;

      Target      : Account_ID := (others => 0);
      Gas_Lim     : Gas_Amount := 0;
      Calldata    : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
      CD_Size     : Natural := 0;
      Return_Buf  : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
      Return_Size : Natural := 0;
      Gas_Used    : Gas_Amount := 0;
      Success     : Boolean;
   begin
      if Current_Depth (Ctx) >= Aegis_VM_Types.Max_Call_Depth then
         Result := (False, 0, U256_Zero, Error_Call_Depth);
         return;
      end if;

      --  Parse arguments: Args(0)=target, Args(1)=gas, Args(2)=cd_size
      if Arg_Count >= 1 then
         declare
            Addr : constant Contract_Address := U256_To_Address (Args (0));
         begin
            for I in 0 .. 31 loop
               Target (I) := Addr (I);
            end loop;
         end;
      end if;

      if Arg_Count >= 2 then
         Gas_Lim := Gas_Amount'Min (
            Gas_Amount (U256_To_U64 (Args (1))),
            Gas_Remaining (Ctx));
      else
         Gas_Lim := Gas_Remaining (Ctx);
      end if;

      if Arg_Count >= 3 then
         CD_Size := Natural'Min (1024, Natural (U256_To_U64 (Args (2))));
      end if;

      Sphinx_Runtime.Execute_Static_Call (
         Target      => Target,
         Calldata    => Calldata (0 .. CD_Size - 1),
         Gas_Limit   => Gas_Lim,
         Return_Data => Return_Buf,
         Return_Size => Return_Size,
         Gas_Used    => Gas_Used,
         Success     => Success
      );

      if Success then
         Result := (True, Gas_Used + Gas_StaticCall, U256_One, Error_None);
      else
         Result := (False, Gas_Used + Gas_StaticCall, U256_Zero, Error_Call_Depth);
      end if;
   end Sys_Handle_Static_Call;

   --  DELEGATECALL via Sphinx_Runtime
   procedure Sys_Handle_Delegate_Call (
      Ctx       : in Out Execution_Context;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) is
      use Sphinx_Runtime;
      use Anubis_Address_Types;

      Target      : Account_ID := (others => 0);
      Gas_Lim     : Gas_Amount := 0;
      Calldata    : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
      CD_Size     : Natural := 0;
      Return_Buf  : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
      Return_Size : Natural := 0;
      Gas_Used    : Gas_Amount := 0;
      Success     : Boolean;
   begin
      if Current_Depth (Ctx) >= Aegis_VM_Types.Max_Call_Depth then
         Result := (False, 0, U256_Zero, Error_Call_Depth);
         return;
      end if;

      --  Parse arguments: Args(0)=target, Args(1)=gas, Args(2)=cd_size
      if Arg_Count >= 1 then
         declare
            Addr : constant Contract_Address := U256_To_Address (Args (0));
         begin
            for I in 0 .. 31 loop
               Target (I) := Addr (I);
            end loop;
         end;
      end if;

      if Arg_Count >= 2 then
         Gas_Lim := Gas_Amount'Min (
            Gas_Amount (U256_To_U64 (Args (1))),
            Gas_Remaining (Ctx));
      else
         Gas_Lim := Gas_Remaining (Ctx);
      end if;

      if Arg_Count >= 3 then
         CD_Size := Natural'Min (1024, Natural (U256_To_U64 (Args (2))));
      end if;

      Sphinx_Runtime.Execute_Delegate_Call (
         Target      => Target,
         Calldata    => Calldata (0 .. CD_Size - 1),
         Gas_Limit   => Gas_Lim,
         Return_Data => Return_Buf,
         Return_Size => Return_Size,
         Gas_Used    => Gas_Used,
         Success     => Success
      );

      if Success then
         Result := (True, Gas_Used + Gas_DelegateCall, U256_One, Error_None);
      else
         Result := (False, Gas_Used + Gas_DelegateCall, U256_Zero, Error_Call_Depth);
      end if;
   end Sys_Handle_Delegate_Call;

   --  CREATE/CREATE2 via Sphinx_Runtime
   procedure Sys_Handle_Create (
      Ctx        : in Out Execution_Context;
      Args       : in     Syscall_Args;
      Arg_Count  : in     Natural;
      Is_Create2 : in     Boolean;
      Result     : out    Syscall_Return
   ) is
      use Sphinx_Runtime;
      use Anubis_Address_Types;

      Value       : Unsigned_64 := 0;
      Gas_Lim     : Gas_Amount := 0;
      Init_Code   : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
      Code_Size   : Natural := 0;
      Salt        : Hash256 := Hash256_Zero;
      New_Address : Account_ID := (others => 0);
      Gas_Used    : Gas_Amount := 0;
      Success     : Boolean;
   begin
      if Current_Depth (Ctx) >= Aegis_VM_Types.Max_Call_Depth then
         Result := (False, 0, U256_Zero, Error_Call_Depth);
         return;
      end if;

      if not Can_Modify_State (Ctx) then
         Result := (False, 0, U256_Zero, Error_State_Modify);
         return;
      end if;

      --  Parse arguments: Args(0)=value, Args(1)=code_size, Args(2)=salt (CREATE2 only)
      if Arg_Count >= 1 then
         Value := U256_To_U64 (Args (0));
      end if;

      if Arg_Count >= 2 then
         Code_Size := Natural'Min (1024, Natural (U256_To_U64 (Args (1))));
      end if;

      Gas_Lim := Gas_Remaining (Ctx);

      if Is_Create2 then
         if Arg_Count >= 3 then
            Salt := U256_To_Hash256 (Args (2));
         end if;

         Sphinx_Runtime.Execute_Create2 (
            Init_Code   => Init_Code (0 .. Code_Size - 1),
            Salt        => Salt,
            Value       => Value,
            Gas_Limit   => Gas_Lim,
            New_Address => New_Address,
            Gas_Used    => Gas_Used,
            Success     => Success
         );
      else
         Sphinx_Runtime.Execute_Create (
            Init_Code   => Init_Code (0 .. Code_Size - 1),
            Value       => Value,
            Gas_Limit   => Gas_Lim,
            New_Address => New_Address,
            Gas_Used    => Gas_Used,
            Success     => Success
         );
      end if;

      if Success then
         --  Convert new address to U256
         declare
            Addr_U256 : U256 := U256_Zero;
         begin
            for I in 0 .. 31 loop
               Addr_U256.Limbs (I / 8) := Addr_U256.Limbs (I / 8) or
                  Shift_Left (Word64 (New_Address (I)), (I mod 8) * 8);
            end loop;
            Result := (True, Gas_Used + Gas_Create, Addr_U256, Error_None);
         end;
      else
         Result := (False, Gas_Used + Gas_Create, U256_Zero, Error_Call_Depth);
      end if;
   end Sys_Handle_Create;

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
   ) is
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
            if Arg_Count >= 4 then
               declare
                  Contract  : constant Contract_Address :=
                     U256_To_Address (Args (0));
                  Key       : constant Hash256 :=
                     U256_To_Hash256 (Args (1));
                  Data_Ptr  : constant Natural :=
                     Natural (U256_To_U64 (Args (2)) mod 2**16);
                  Data_Len  : constant Natural :=
                     Natural'Min (32, Natural (U256_To_U64 (Args (3))));
                  --  Read plaintext from memory via Sphinx_Runtime
                  Plaintext : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                     (others => 0);
                  Temp_Buf  : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
                  KEM_PK    : Aegis_Privacy.VM_Byte_Array (0 .. 1567) :=
                     (others => 0);
                  Randomness: Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Load_OK   : Boolean;
               begin
                  --  Load plaintext from calldata at specified offset
                  Sphinx_Runtime.Load_Calldata (Data_Ptr, Data_Len, Temp_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. Data_Len - 1 loop
                        Plaintext (I) := Aegis_Privacy.VM_Byte (Temp_Buf (I));
                     end loop;
                  end if;

                  --  If KEM_PK pointer provided, load it too
                  if Arg_Count >= 5 then
                     declare
                        PK_Ptr   : constant Natural :=
                           Natural (U256_To_U64 (Args (4)) mod 2**16);
                        PK_Buf   : Aegis_VM_Types.Byte_Array (0 .. 1567) := (others => 0);
                        PK_OK    : Boolean;
                     begin
                        Sphinx_Runtime.Load_Calldata (PK_Ptr, 1568, PK_Buf, PK_OK);
                        if PK_OK then
                           for I in 0 .. 1567 loop
                              KEM_PK (I) := Aegis_Privacy.VM_Byte (PK_Buf (I));
                           end loop;
                        end if;
                     end;
                  end if;

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
                     Return_Val => Bool_To_U256 (Privacy_Result.Success),
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Private_Load =>
            --  Args: 0=contract_addr, 1=key, 2=kem_sk_ptr, 3=output_ptr
            if Arg_Count >= 4 then
               declare
                  Contract  : constant Contract_Address :=
                     U256_To_Address (Args (0));
                  Key       : constant Hash256 :=
                     U256_To_Hash256 (Args (1));
                  SK_Ptr    : constant Natural :=
                     Natural (U256_To_U64 (Args (2)) mod 2**16);
                  --  Load KEM secret key from calldata
                  KEM_SK    : Aegis_Privacy.VM_Byte_Array (0 .. 3167) :=
                     (others => 0);
                  SK_Buf    : Aegis_VM_Types.Byte_Array (0 .. 3167) := (others => 0);
                  Plaintext : Aegis_Privacy.VM_Byte_Array (0 .. 4095) :=
                     (others => 0);
                  PT_Len    : Natural;
                  Load_OK   : Boolean;
               begin
                  --  Load KEM secret key from calldata
                  Sphinx_Runtime.Load_Calldata (SK_Ptr, 3168, SK_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 3167 loop
                        KEM_SK (I) := Aegis_Privacy.VM_Byte (SK_Buf (I));
                     end loop;
                  end if;

                  Aegis_Privacy.Private_Load (
                     Contract      => Contract,
                     Key           => Key,
                     User_KEM_SK   => KEM_SK,
                     Plaintext     => Plaintext,
                     Plaintext_Len => PT_Len,
                     Result        => Privacy_Result
                  );

                  --  Return the plaintext length; caller would read output via
                  --  separate memory read syscall
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
            if Arg_Count >= 2 then
               declare
                  Value       : constant Unsigned_64 := U256_To_U64 (Args (0));
                  Blind_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (1)) mod 2**16);
                  Blinding    : Aegis_Privacy.VM_Byte_Array (0 .. 31) := (others => 0);
                  Blind_Buf   : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
                  Commit      : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
                  Load_OK     : Boolean;
               begin
                  --  Load blinding factor from calldata
                  Sphinx_Runtime.Load_Calldata (Blind_Ptr, 32, Blind_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 31 loop
                        Blinding (I) := Aegis_Privacy.VM_Byte (Blind_Buf (I));
                     end loop;
                  end if;

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
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Verify_Range =>
            --  Args: 0=commitment_ptr, 1=proof_ptr, 2=num_bits
            if Arg_Count >= 3 then
               declare
                  Commit_Ptr : constant Natural :=
                     Natural (U256_To_U64 (Args (0)) mod 2**16);
                  Proof_Ptr  : constant Natural :=
                     Natural (U256_To_U64 (Args (1)) mod 2**16);
                  Bits       : constant Natural :=
                     Natural'Min (64, Natural (U256_To_U64 (Args (2))));
                  Commit     : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
                  Proof      : Aegis_Privacy.VM_Byte_Array (0 .. 2047) := (others => 0);
                  Commit_Buf : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
                  Proof_Buf  : Aegis_VM_Types.Byte_Array (0 .. 2047) := (others => 0);
                  Valid      : Boolean;
                  Load_OK    : Boolean;
               begin
                  --  Load commitment from calldata
                  Sphinx_Runtime.Load_Calldata (Commit_Ptr, 64, Commit_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 63 loop
                        Commit (I) := Aegis_Privacy.VM_Byte (Commit_Buf (I));
                     end loop;
                  end if;

                  --  Load proof from calldata
                  Sphinx_Runtime.Load_Calldata (Proof_Ptr, 2048, Proof_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 2047 loop
                        Proof (I) := Aegis_Privacy.VM_Byte (Proof_Buf (I));
                     end loop;
                  end if;

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
                     Return_Val => Bool_To_U256 (Valid),
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Add_Commitments =>
            --  Args: 0=a_ptr, 1=b_ptr, 2=sum_ptr
            if Arg_Count >= 2 then
               declare
                  A_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (0)) mod 2**16);
                  B_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (1)) mod 2**16);
                  A       : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
                  B       : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
                  Sum     : Aegis_Privacy.VM_Byte_Array (0 .. 63) := (others => 0);
                  A_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
                  B_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
                  Load_OK : Boolean;
               begin
                  --  Load commitment A from calldata
                  Sphinx_Runtime.Load_Calldata (A_Ptr, 64, A_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 63 loop
                        A (I) := Aegis_Privacy.VM_Byte (A_Buf (I));
                     end loop;
                  end if;

                  --  Load commitment B from calldata
                  Sphinx_Runtime.Load_Calldata (B_Ptr, 64, B_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 63 loop
                        B (I) := Aegis_Privacy.VM_Byte (B_Buf (I));
                     end loop;
                  end if;

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
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         when Sys_Verify_Balance =>
            --  Args: 0=inputs_ptr, 1=outputs_ptr, 2=fee, 3=proof_ptr
            if Arg_Count >= 4 then
               declare
                  Inputs_Ptr  : constant Natural :=
                     Natural (U256_To_U64 (Args (0)) mod 2**16);
                  Outputs_Ptr : constant Natural :=
                     Natural (U256_To_U64 (Args (1)) mod 2**16);
                  Fee         : constant Unsigned_64 := U256_To_U64 (Args (2));
                  Proof_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (3)) mod 2**16);
                  Inputs      : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
                  Outputs     : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
                  Proof       : Aegis_Privacy.VM_Byte_Array (0 .. 2047) := (others => 0);
                  In_Buf      : Aegis_VM_Types.Byte_Array (0 .. 639) := (others => 0);
                  Out_Buf     : Aegis_VM_Types.Byte_Array (0 .. 639) := (others => 0);
                  Proof_Buf   : Aegis_VM_Types.Byte_Array (0 .. 2047) := (others => 0);
                  Valid       : Boolean;
                  Load_OK     : Boolean;
               begin
                  --  Load inputs from calldata
                  Sphinx_Runtime.Load_Calldata (Inputs_Ptr, 640, In_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 639 loop
                        Inputs (I) := Aegis_Privacy.VM_Byte (In_Buf (I));
                     end loop;
                  end if;

                  --  Load outputs from calldata
                  Sphinx_Runtime.Load_Calldata (Outputs_Ptr, 640, Out_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 639 loop
                        Outputs (I) := Aegis_Privacy.VM_Byte (Out_Buf (I));
                     end loop;
                  end if;

                  --  Load proof from calldata
                  Sphinx_Runtime.Load_Calldata (Proof_Ptr, 2048, Proof_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 2047 loop
                        Proof (I) := Aegis_Privacy.VM_Byte (Proof_Buf (I));
                     end loop;
                  end if;

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
                     Return_Val => Bool_To_U256 (Valid),
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
            else
               Result := (False, 0, U256_Zero, Error_Invalid_Args);
            end if;

         --  GATE operations (Private Execution)
         when Sys_Private_Call =>
            --  Args: 0=target, 1=function_sel, 2=private_args_ptr,
            --        3=public_args_ptr, 4=gas_limit, 5=mode
            declare
               Target      : constant Contract_Address :=
                  U256_To_Address (Args (0));
               Func_Sel    : constant Unsigned_32 :=
                  Unsigned_32 (U256_To_U64 (Args (1)) and 16#FFFFFFFF#);
               Priv_Ptr    : constant Natural :=
                  Natural (U256_To_U64 (Args (2)) mod 2**16);
               Pub_Ptr     : constant Natural :=
                  Natural (U256_To_U64 (Args (3)) mod 2**16);
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
               Priv_Buf    : Aegis_VM_Types.Byte_Array (0 .. 4095) := (others => 0);
               Pub_Buf     : Aegis_VM_Types.Byte_Array (0 .. 1023) := (others => 0);
               Proof       : Aegis_Privacy.VM_Byte_Array (0 .. 8191) :=
                  (others => 0);
               Output      : Aegis_Privacy.VM_Byte_Array (0 .. 1023) :=
                  (others => 0);
               Output_Len  : Natural;
               Load_OK     : Boolean;
            begin
               --  Load private args from calldata
               Sphinx_Runtime.Load_Calldata (Priv_Ptr, 4096, Priv_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 4095 loop
                     Priv_Args (I) := Aegis_Privacy.VM_Byte (Priv_Buf (I));
                  end loop;
               end if;

               --  Load public args from calldata
               Sphinx_Runtime.Load_Calldata (Pub_Ptr, 1024, Pub_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 1023 loop
                     Pub_Args (I) := Aegis_Privacy.VM_Byte (Pub_Buf (I));
                  end loop;
               end if;

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
               Proof_Ptr : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Contract  : constant Contract_Address :=
                  U256_To_Address (Args (1));
               Old_Hash  : constant Hash256 := U256_To_Hash256 (Args (2));
               New_Hash  : constant Hash256 := U256_To_Hash256 (Args (3));
               Proof     : Aegis_Privacy.VM_Byte_Array (0 .. 8191) :=
                  (others => 0);
               Proof_Buf : Aegis_VM_Types.Byte_Array (0 .. 8191) := (others => 0);
               Valid     : Boolean;
               Load_OK   : Boolean;
            begin
               --  Load proof from calldata
               Sphinx_Runtime.Load_Calldata (Proof_Ptr, 8192, Proof_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 8191 loop
                     Proof (I) := Aegis_Privacy.VM_Byte (Proof_Buf (I));
                  end loop;
               end if;

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
                  Return_Val => Bool_To_U256 (Valid),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Create_Session =>
            --  Args: 0=contract, 1=kem_sk_ptr, 2=contract_kem_pk_ptr, 3=rand_ptr
            declare
               Contract    : constant Contract_Address :=
                  U256_To_Address (Args (0));
               SK_Ptr      : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               PK_Ptr      : constant Natural :=
                  Natural (U256_To_U64 (Args (2)) mod 2**16);
               Rand_Ptr    : constant Natural :=
                  Natural (U256_To_U64 (Args (3)) mod 2**16);
               KEM_SK      : Aegis_Privacy.VM_Byte_Array (0 .. 3167) :=
                  (others => 0);
               Contract_PK : Aegis_Privacy.VM_Byte_Array (0 .. 1567) :=
                  (others => 0);
               Randomness  : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               SK_Buf      : Aegis_VM_Types.Byte_Array (0 .. 3167) := (others => 0);
               PK_Buf      : Aegis_VM_Types.Byte_Array (0 .. 1567) := (others => 0);
               Rand_Buf    : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
               Session     : Aegis_Privacy.Session_ID;
               Load_OK     : Boolean;
            begin
               --  Load KEM secret key from calldata
               Sphinx_Runtime.Load_Calldata (SK_Ptr, 3168, SK_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 3167 loop
                     KEM_SK (I) := Aegis_Privacy.VM_Byte (SK_Buf (I));
                  end loop;
               end if;

               --  Load contract KEM public key from calldata
               Sphinx_Runtime.Load_Calldata (PK_Ptr, 1568, PK_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 1567 loop
                     Contract_PK (I) := Aegis_Privacy.VM_Byte (PK_Buf (I));
                  end loop;
               end if;

               --  Load randomness from calldata
               Sphinx_Runtime.Load_Calldata (Rand_Ptr, 64, Rand_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 63 loop
                     Randomness (I) := Aegis_Privacy.VM_Byte (Rand_Buf (I));
                  end loop;
               end if;

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
               Sess_Ptr   : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Session    : Aegis_Privacy.Session_ID := (others => 0);
               Sess_Buf   : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Load_OK    : Boolean;
            begin
               --  Load session ID from calldata
               Sphinx_Runtime.Load_Calldata (Sess_Ptr, 32, Sess_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     Session (I) := Aegis_Privacy.VM_Byte (Sess_Buf (I));
                  end loop;
               end if;

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
               Cred_Ptr      : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Cred_Len      : constant Natural :=
                  Natural'Min (4096, Natural (U256_To_U64 (Args (1))));
               Secret_Ptr    : constant Natural :=
                  Natural (U256_To_U64 (Args (2)) mod 2**16);
               Disclose_Mask : constant Unsigned_32 :=
                  Unsigned_32 (U256_To_U64 (Args (3)) and 16#FFFF_FFFF#);
               Challenge_Ptr : constant Natural :=
                  Natural (U256_To_U64 (Args (4)) mod 2**16);
               Credential    : Aegis_Privacy.VM_Byte_Array (0 .. 4095) :=
                  (others => 0);
               Holder_Secret : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Challenge     : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Cred_Buf      : Aegis_VM_Types.Byte_Array (0 .. 4095) := (others => 0);
               Secret_Buf    : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Chall_Buf     : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Proof         : Aegis_Privacy.VM_Byte_Array (0 .. 511) :=
                  (others => 0);
               Load_OK       : Boolean;
            begin
               --  Load credential from calldata
               Sphinx_Runtime.Load_Calldata (Cred_Ptr, Cred_Len, Cred_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. Natural'Min (4095, Cred_Len - 1) loop
                     Credential (I) := Aegis_Privacy.VM_Byte (Cred_Buf (I));
                  end loop;
               end if;

               --  Load holder secret from calldata
               Sphinx_Runtime.Load_Calldata (Secret_Ptr, 32, Secret_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     Holder_Secret (I) := Aegis_Privacy.VM_Byte (Secret_Buf (I));
                  end loop;
               end if;

               --  Load challenge from calldata
               Sphinx_Runtime.Load_Calldata (Challenge_Ptr, 32, Chall_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     Challenge (I) := Aegis_Privacy.VM_Byte (Chall_Buf (I));
                  end loop;
               end if;

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
               Proof_Ptr       : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Attrs_Ptr       : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               Attrs_Len       : constant Natural :=
                  Natural'Min (2048, Natural (U256_To_U64 (Args (2))));
               PK_Ptr          : constant Natural :=
                  Natural (U256_To_U64 (Args (3)) mod 2**16);
               Challenge_Ptr   : constant Natural :=
                  Natural (U256_To_U64 (Args (4)) mod 2**16);
               Proof           : Aegis_Privacy.VM_Byte_Array (0 .. 511) :=
                  (others => 0);
               Disclosed_Attrs : Aegis_Privacy.VM_Byte_Array (0 .. 2047) :=
                  (others => 0);
               Issuer_PK       : Aegis_Privacy.VM_Byte_Array (0 .. 2591) :=
                  (others => 0);
               Challenge       : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Proof_Buf       : Aegis_VM_Types.Byte_Array (0 .. 511) := (others => 0);
               Attrs_Buf       : Aegis_VM_Types.Byte_Array (0 .. 2047) := (others => 0);
               PK_Buf          : Aegis_VM_Types.Byte_Array (0 .. 2591) := (others => 0);
               Chall_Buf       : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Valid           : Boolean;
               Load_OK         : Boolean;
            begin
               --  Load proof from calldata
               Sphinx_Runtime.Load_Calldata (Proof_Ptr, 512, Proof_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 511 loop
                     Proof (I) := Aegis_Privacy.VM_Byte (Proof_Buf (I));
                  end loop;
               end if;

               --  Load disclosed attrs from calldata
               Sphinx_Runtime.Load_Calldata (Attrs_Ptr, Attrs_Len, Attrs_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. Natural'Min (2047, Attrs_Len - 1) loop
                     Disclosed_Attrs (I) := Aegis_Privacy.VM_Byte (Attrs_Buf (I));
                  end loop;
               end if;

               --  Load issuer public key from calldata
               Sphinx_Runtime.Load_Calldata (PK_Ptr, 2592, PK_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 2591 loop
                     Issuer_PK (I) := Aegis_Privacy.VM_Byte (PK_Buf (I));
                  end loop;
               end if;

               --  Load challenge from calldata
               Sphinx_Runtime.Load_Calldata (Challenge_Ptr, 32, Chall_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     Challenge (I) := Aegis_Privacy.VM_Byte (Chall_Buf (I));
                  end loop;
               end if;

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
                  Return_Val => Bool_To_U256 (Valid),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         when Sys_Derive_View_Key =>
            --  Args: 0=master_seed_ptr, 1=view_key_output_ptr
            declare
               Seed_Ptr    : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Master_Seed : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Seed_Buf    : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               View_Key    : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Load_OK     : Boolean;
            begin
               --  Load master seed from calldata
               Sphinx_Runtime.Load_Calldata (Seed_Ptr, 32, Seed_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     Master_Seed (I) := Aegis_Privacy.VM_Byte (Seed_Buf (I));
                  end loop;
               end if;

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
               View_Ptr     : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Spend_Ptr    : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               Rand_Ptr     : constant Natural :=
                  Natural (U256_To_U64 (Args (2)) mod 2**16);
               View_Key     : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Spend_Key    : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Randomness   : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               View_Buf     : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Spend_Buf    : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Rand_Buf     : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Stealth_Addr : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Tx_PK        : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Load_OK      : Boolean;
            begin
               --  Load view key from calldata
               Sphinx_Runtime.Load_Calldata (View_Ptr, 32, View_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     View_Key (I) := Aegis_Privacy.VM_Byte (View_Buf (I));
                  end loop;
               end if;

               --  Load spend key from calldata
               Sphinx_Runtime.Load_Calldata (Spend_Ptr, 32, Spend_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     Spend_Key (I) := Aegis_Privacy.VM_Byte (Spend_Buf (I));
                  end loop;
               end if;

               --  Load randomness from calldata
               Sphinx_Runtime.Load_Calldata (Rand_Ptr, 32, Rand_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     Randomness (I) := Aegis_Privacy.VM_Byte (Rand_Buf (I));
                  end loop;
               end if;

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
            if Arg_Count >= 7 then
               declare
                  Ring_Ptr     : constant Natural :=
                     Natural (U256_To_U64 (Args (0)) mod 2**16);
                  Ring_Size    : constant Natural :=
                     Natural'Min (128, Natural (U256_To_U64 (Args (1))));
                  Signer_Index : constant Natural :=
                     Natural'Min (Ring_Size - 1, Natural (U256_To_U64 (Args (2))));
                  SK_Ptr       : constant Natural :=
                     Natural (U256_To_U64 (Args (3)) mod 2**16);
                  Msg_Ptr      : constant Natural :=
                     Natural (U256_To_U64 (Args (4)) mod 2**16);
                  Msg_Len      : constant Natural :=
                     Natural'Min (256, Natural (U256_To_U64 (Args (5))));
                  Rand_Ptr     : constant Natural :=
                     Natural (U256_To_U64 (Args (6)) mod 2**16);
                  --  Public keys: each ~96 bytes (commitment + hash)
                  Ring_PKs     : Aegis_Privacy.VM_Byte_Array (0 .. 12287) :=
                     (others => 0);  -- 128 * 96 bytes
                  Signer_SK    : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                     (others => 0);
                  Message      : Aegis_Privacy.VM_Byte_Array (0 .. 255) :=
                     (others => 0);
                  Randomness   : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Ring_Buf     : Aegis_VM_Types.Byte_Array (0 .. 12287) := (others => 0);
                  SK_Buf       : Aegis_VM_Types.Byte_Array (0 .. 127) := (others => 0);
                  Msg_Buf      : Aegis_VM_Types.Byte_Array (0 .. 255) := (others => 0);
                  Rand_Buf     : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
                  Signature    : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Key_Img      : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Load_OK      : Boolean;
               begin
                  if Ring_Size >= 2 and Signer_Index < Ring_Size then
                     --  Load ring public keys from calldata
                     Sphinx_Runtime.Load_Calldata (Ring_Ptr, Ring_Size * 96, Ring_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. Natural'Min (12287, Ring_Size * 96 - 1) loop
                           Ring_PKs (I) := Aegis_Privacy.VM_Byte (Ring_Buf (I));
                        end loop;
                     end if;

                     --  Load signer secret key from calldata
                     Sphinx_Runtime.Load_Calldata (SK_Ptr, 128, SK_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 127 loop
                           Signer_SK (I) := Aegis_Privacy.VM_Byte (SK_Buf (I));
                        end loop;
                     end if;

                     --  Load message from calldata
                     Sphinx_Runtime.Load_Calldata (Msg_Ptr, Msg_Len, Msg_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. Natural'Min (255, Msg_Len - 1) loop
                           Message (I) := Aegis_Privacy.VM_Byte (Msg_Buf (I));
                        end loop;
                     end if;

                     --  Load randomness from calldata
                     Sphinx_Runtime.Load_Calldata (Rand_Ptr, 64, Rand_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 63 loop
                           Randomness (I) := Aegis_Privacy.VM_Byte (Rand_Buf (I));
                        end loop;
                     end if;

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
            if Arg_Count >= 6 then
               declare
                  Ring_Ptr  : constant Natural :=
                     Natural (U256_To_U64 (Args (0)) mod 2**16);
                  Ring_Size : constant Natural :=
                     Natural'Min (128, Natural (U256_To_U64 (Args (1))));
                  Msg_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (2)) mod 2**16);
                  Msg_Len   : constant Natural :=
                     Natural'Min (256, Natural (U256_To_U64 (Args (3))));
                  Sig_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (4)) mod 2**16);
                  Key_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (5)) mod 2**16);
                  Ring_PKs  : Aegis_Privacy.VM_Byte_Array (0 .. 12287) :=
                     (others => 0);
                  Message   : Aegis_Privacy.VM_Byte_Array (0 .. 255) :=
                     (others => 0);
                  Signature : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Key_Img   : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Ring_Buf  : Aegis_VM_Types.Byte_Array (0 .. 12287) := (others => 0);
                  Msg_Buf   : Aegis_VM_Types.Byte_Array (0 .. 255) := (others => 0);
                  Sig_Buf   : Aegis_VM_Types.Byte_Array (0 .. 16383) := (others => 0);
                  Key_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
                  Valid     : Boolean;
                  Load_OK   : Boolean;
               begin
                  if Ring_Size >= 2 then
                     --  Load ring public keys from calldata
                     Sphinx_Runtime.Load_Calldata (Ring_Ptr, Ring_Size * 96, Ring_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. Natural'Min (12287, Ring_Size * 96 - 1) loop
                           Ring_PKs (I) := Aegis_Privacy.VM_Byte (Ring_Buf (I));
                        end loop;
                     end if;

                     --  Load message from calldata
                     Sphinx_Runtime.Load_Calldata (Msg_Ptr, Msg_Len, Msg_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. Natural'Min (255, Msg_Len - 1) loop
                           Message (I) := Aegis_Privacy.VM_Byte (Msg_Buf (I));
                        end loop;
                     end if;

                     --  Load signature from calldata (size varies with ring size)
                     Sphinx_Runtime.Load_Calldata (Sig_Ptr, Ring_Size * 128, Sig_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. Natural'Min (16383, Ring_Size * 128 - 1) loop
                           Signature (I) := Aegis_Privacy.VM_Byte (Sig_Buf (I));
                        end loop;
                     end if;

                     --  Load key image from calldata
                     Sphinx_Runtime.Load_Calldata (Key_Ptr, 64, Key_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 63 loop
                           Key_Img (I) := Aegis_Privacy.VM_Byte (Key_Buf (I));
                        end loop;
                     end if;

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
                        Return_Val => Bool_To_U256 (Valid),
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
               SK_Ptr     : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Secret_Key : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               SK_Buf     : Aegis_VM_Types.Byte_Array (0 .. 127) := (others => 0);
               Key_Img    : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Load_OK    : Boolean;
            begin
               --  Load secret key from calldata
               Sphinx_Runtime.Load_Calldata (SK_Ptr, 128, SK_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 127 loop
                     Secret_Key (I) := Aegis_Privacy.VM_Byte (SK_Buf (I));
                  end loop;
               end if;

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
               Key_Ptr      : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Spent_Ptr    : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               Num_Spent    : constant Natural :=
                  Natural'Min (1024, Natural (U256_To_U64 (Args (2))));
               Key_Img      : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Spent_Images : Aegis_Privacy.VM_Byte_Array (0 .. 65535) :=
                  (others => 0);  -- Up to 1024 * 64 byte images
               Key_Buf      : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
               Spent_Buf    : Aegis_VM_Types.Byte_Array (0 .. 65535) := (others => 0);
               Is_Spent     : Boolean;
               Load_OK      : Boolean;
            begin
               --  Load key image from calldata
               Sphinx_Runtime.Load_Calldata (Key_Ptr, 64, Key_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 63 loop
                     Key_Img (I) := Aegis_Privacy.VM_Byte (Key_Buf (I));
                  end loop;
               end if;

               --  Load spent images from calldata
               if Num_Spent > 0 then
                  Sphinx_Runtime.Load_Calldata (Spent_Ptr, Num_Spent * 64, Spent_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. Natural'Min (65535, Num_Spent * 64 - 1) loop
                        Spent_Images (I) := Aegis_Privacy.VM_Byte (Spent_Buf (I));
                     end loop;
                  end if;
               end if;

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
                  Return_Val => Bool_To_U256 (Is_Spent),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         --  Lattice ZK operations
         when Sys_ZK_Prove_Range =>
            --  Args: 0=value, 1=num_bits, 2=randomness_ptr,
            --        3=commitment_output_ptr, 4=proof_output_ptr
            if Arg_Count >= 3 then
               declare
                  Value      : constant Unsigned_64 := U256_To_U64 (Args (0));
                  Num_Bits   : constant Natural :=
                     Natural'Min (64, Natural (U256_To_U64 (Args (1))));
                  Rand_Ptr   : constant Natural :=
                     Natural (U256_To_U64 (Args (2)) mod 2**16);
                  Randomness : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                     (others => 0);
                  Rand_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
                  Commitment : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Proof      : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                     (others => 0);
                  Load_OK    : Boolean;
               begin
                  if Num_Bits >= 1 and Value < 2 ** Num_Bits then
                     --  Load randomness from calldata
                     Sphinx_Runtime.Load_Calldata (Rand_Ptr, 64, Rand_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 63 loop
                           Randomness (I) := Aegis_Privacy.VM_Byte (Rand_Buf (I));
                        end loop;
                     end if;

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
            if Arg_Count >= 3 then
               declare
                  Commit_Ptr : constant Natural :=
                     Natural (U256_To_U64 (Args (0)) mod 2**16);
                  Num_Bits   : constant Natural :=
                     Natural'Min (64, Natural (U256_To_U64 (Args (1))));
                  Proof_Ptr  : constant Natural :=
                     Natural (U256_To_U64 (Args (2)) mod 2**16);
                  Commitment : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                     (others => 0);
                  Proof      : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                     (others => 0);
                  Commit_Buf : Aegis_VM_Types.Byte_Array (0 .. 16383) := (others => 0);
                  Proof_Buf  : Aegis_VM_Types.Byte_Array (0 .. 32767) := (others => 0);
                  Valid      : Boolean;
                  Load_OK    : Boolean;
               begin
                  if Num_Bits >= 1 then
                     --  Load commitment from calldata
                     Sphinx_Runtime.Load_Calldata (Commit_Ptr, 16384, Commit_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 16383 loop
                           Commitment (I) := Aegis_Privacy.VM_Byte (Commit_Buf (I));
                        end loop;
                     end if;

                     --  Load proof from calldata
                     Sphinx_Runtime.Load_Calldata (Proof_Ptr, 32768, Proof_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 32767 loop
                           Proof (I) := Aegis_Privacy.VM_Byte (Proof_Buf (I));
                        end loop;
                     end if;

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
                        Return_Val => Bool_To_U256 (Valid),
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
               X_Ptr      : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Y_Ptr      : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               A_Coeff    : constant Integer :=
                  Integer (U256_To_U64 (Args (2)) mod 2**31);
               B_Coeff    : constant Integer :=
                  Integer (U256_To_U64 (Args (3)) mod 2**31);
               Rand_Ptr   : constant Natural :=
                  Natural (U256_To_U64 (Args (4)) mod 2**16);
               X_Value    : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               Y_Value    : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               Randomness : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               X_Buf      : Aegis_VM_Types.Byte_Array (0 .. 127) := (others => 0);
               Y_Buf      : Aegis_VM_Types.Byte_Array (0 .. 127) := (others => 0);
               Rand_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
               Proof      : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                  (others => 0);
               Load_OK    : Boolean;
            begin
               --  Load X value from calldata
               Sphinx_Runtime.Load_Calldata (X_Ptr, 128, X_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 127 loop
                     X_Value (I) := Aegis_Privacy.VM_Byte (X_Buf (I));
                  end loop;
               end if;

               --  Load Y value from calldata
               Sphinx_Runtime.Load_Calldata (Y_Ptr, 128, Y_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 127 loop
                     Y_Value (I) := Aegis_Privacy.VM_Byte (Y_Buf (I));
                  end loop;
               end if;

               --  Load randomness from calldata
               Sphinx_Runtime.Load_Calldata (Rand_Ptr, 64, Rand_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 63 loop
                     Randomness (I) := Aegis_Privacy.VM_Byte (Rand_Buf (I));
                  end loop;
               end if;

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
               ComX_Ptr : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               ComY_Ptr : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               A_Coeff  : constant Integer :=
                  Integer (U256_To_U64 (Args (2)) mod 2**31);
               B_Coeff  : constant Integer :=
                  Integer (U256_To_U64 (Args (3)) mod 2**31);
               CRes_Ptr : constant Natural :=
                  Natural (U256_To_U64 (Args (4)) mod 2**16);
               Proof_Ptr : constant Natural :=
                  Natural (U256_To_U64 (Args (5)) mod 2**16);
               Com_X    : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                  (others => 0);
               Com_Y    : Aegis_Privacy.VM_Byte_Array (0 .. 16383) :=
                  (others => 0);
               C_Result : Aegis_Privacy.VM_Byte_Array (0 .. 127) :=
                  (others => 0);
               Proof    : Aegis_Privacy.VM_Byte_Array (0 .. 32767) :=
                  (others => 0);
               ComX_Buf : Aegis_VM_Types.Byte_Array (0 .. 16383) := (others => 0);
               ComY_Buf : Aegis_VM_Types.Byte_Array (0 .. 16383) := (others => 0);
               CRes_Buf : Aegis_VM_Types.Byte_Array (0 .. 127) := (others => 0);
               Proof_Buf : Aegis_VM_Types.Byte_Array (0 .. 32767) := (others => 0);
               Valid    : Boolean;
               Load_OK  : Boolean;
            begin
               --  Load commitment X from calldata
               Sphinx_Runtime.Load_Calldata (ComX_Ptr, 16384, ComX_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 16383 loop
                     Com_X (I) := Aegis_Privacy.VM_Byte (ComX_Buf (I));
                  end loop;
               end if;

               --  Load commitment Y from calldata
               Sphinx_Runtime.Load_Calldata (ComY_Ptr, 16384, ComY_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 16383 loop
                     Com_Y (I) := Aegis_Privacy.VM_Byte (ComY_Buf (I));
                  end loop;
               end if;

               --  Load C result from calldata
               Sphinx_Runtime.Load_Calldata (CRes_Ptr, 128, CRes_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 127 loop
                     C_Result (I) := Aegis_Privacy.VM_Byte (CRes_Buf (I));
                  end loop;
               end if;

               --  Load proof from calldata
               Sphinx_Runtime.Load_Calldata (Proof_Ptr, 32768, Proof_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 32767 loop
                     Proof (I) := Aegis_Privacy.VM_Byte (Proof_Buf (I));
                  end loop;
               end if;

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
                  Return_Val => Bool_To_U256 (Valid),
                  Error_Code => Privacy_Result.Error_Code
               );
            end;

         --  Confidential transfer operations (WHISPER extended)
         when Sys_Confidential_Transfer =>
            --  Args: 0=sender_commit_ptr, 1=receiver_commit_ptr,
            --        2=input_value, 3=output_value, 4=fee,
            --        5=sender_blinding_ptr, 6=receiver_blinding_ptr
            --  Performs confidential value transfer with hidden amounts
            if Arg_Count >= 7 then
               declare
                  Input_Val   : constant Unsigned_64 := U256_To_U64 (Args (2));
                  Output_Val  : constant Unsigned_64 := U256_To_U64 (Args (3));
                  Fee_Val     : constant Unsigned_64 := U256_To_U64 (Args (4));
                  SBlind_Ptr  : constant Natural :=
                     Natural (U256_To_U64 (Args (5)) mod 2**16);
                  RBlind_Ptr  : constant Natural :=
                     Natural (U256_To_U64 (Args (6)) mod 2**16);
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
                  SBlind_Buf : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
                  RBlind_Buf : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
                  --  Range proof for output value
                  Range_Proof : Aegis_Privacy.VM_Byte_Array (0 .. 2047) :=
                     (others => 0);
                  --  Transfer proof hash output
                  Transfer_Hash : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                     (others => 0);
                  Load_OK : Boolean;
               begin
                  pragma Unreferenced (Transfer_Hash);
                  --  Validate balance: input = output + fee
                  if Input_Val >= Output_Val and
                     Input_Val - Output_Val = Fee_Val
                  then
                     --  Load sender blinding factor from calldata
                     Sphinx_Runtime.Load_Calldata (SBlind_Ptr, 32, SBlind_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 31 loop
                           Sender_Blind (I) := Aegis_Privacy.VM_Byte (SBlind_Buf (I));
                        end loop;
                     end if;

                     --  Load receiver blinding factor from calldata
                     Sphinx_Runtime.Load_Calldata (RBlind_Ptr, 32, RBlind_Buf, Load_OK);
                     if Load_OK then
                        for I in 0 .. 31 loop
                           Receiver_Blind (I) := Aegis_Privacy.VM_Byte (RBlind_Buf (I));
                        end loop;
                     end if;

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
                  In_Ptr       : constant Natural :=
                     Natural (U256_To_U64 (Args (0)) mod 2**16);
                  Out_Ptr      : constant Natural :=
                     Natural (U256_To_U64 (Args (1)) mod 2**16);
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
                  In_Buf         : Aegis_VM_Types.Byte_Array (0 .. 639) := (others => 0);
                  Out_Buf        : Aegis_VM_Types.Byte_Array (0 .. 639) := (others => 0);
                  Balance_Proof  : Aegis_Privacy.VM_Byte_Array (0 .. 2047) :=
                     (others => 0);
                  Valid   : Boolean;
                  Load_OK : Boolean;
               begin
                  pragma Unreferenced (Input_Count, Output_Count);
                  --  Load input commitments from calldata
                  Sphinx_Runtime.Load_Calldata (In_Ptr, 640, In_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 639 loop
                        Input_Commits (I) := Aegis_Privacy.VM_Byte (In_Buf (I));
                     end loop;
                  end if;

                  --  Load output commitments from calldata
                  Sphinx_Runtime.Load_Calldata (Out_Ptr, 640, Out_Buf, Load_OK);
                  if Load_OK then
                     for I in 0 .. 639 loop
                        Output_Commits (I) := Aegis_Privacy.VM_Byte (Out_Buf (I));
                     end loop;
                  end if;

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
                     Return_Val => Bool_To_U256 (Valid),
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
               Proof_Ptr  : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               Send_Ptr   : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               Recv_Ptr   : constant Natural :=
                  Natural (U256_To_U64 (Args (2)) mod 2**16);
               Fee_Ptr    : constant Natural :=
                  Natural (U256_To_U64 (Args (3)) mod 2**16);
               Sender_Commit   : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Receiver_Commit : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Fee_Commit      : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               Send_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
               Recv_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
               Fee_Buf    : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
               Proof_Buf  : Aegis_VM_Types.Byte_Array (0 .. 2047) := (others => 0);
               --  Combined inputs = sender
               Inputs  : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
               --  Combined outputs = receiver + fee
               Outputs : Aegis_Privacy.VM_Byte_Array (0 .. 639) := (others => 0);
               Proof   : Aegis_Privacy.VM_Byte_Array (0 .. 2047) := (others => 0);
               Valid   : Boolean;
               Load_OK : Boolean;
            begin
               --  Load proof from calldata
               Sphinx_Runtime.Load_Calldata (Proof_Ptr, 2048, Proof_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 2047 loop
                     Proof (I) := Aegis_Privacy.VM_Byte (Proof_Buf (I));
                  end loop;
               end if;

               --  Load sender commitment from calldata
               Sphinx_Runtime.Load_Calldata (Send_Ptr, 64, Send_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 63 loop
                     Sender_Commit (I) := Aegis_Privacy.VM_Byte (Send_Buf (I));
                  end loop;
               end if;

               --  Load receiver commitment from calldata
               Sphinx_Runtime.Load_Calldata (Recv_Ptr, 64, Recv_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 63 loop
                     Receiver_Commit (I) := Aegis_Privacy.VM_Byte (Recv_Buf (I));
                  end loop;
               end if;

               --  Load fee commitment from calldata
               Sphinx_Runtime.Load_Calldata (Fee_Ptr, 64, Fee_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 63 loop
                     Fee_Commit (I) := Aegis_Privacy.VM_Byte (Fee_Buf (I));
                  end loop;
               end if;

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
                  Return_Val => Bool_To_U256 (Valid),
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
               Commit_Ptr   : constant Natural :=
                  Natural (U256_To_U64 (Args (0)) mod 2**16);
               View_Ptr     : constant Natural :=
                  Natural (U256_To_U64 (Args (1)) mod 2**16);
               Eph_Ptr      : constant Natural :=
                  Natural (U256_To_U64 (Args (2)) mod 2**16);
               Output_Commit : Aegis_Privacy.VM_Byte_Array (0 .. 63) :=
                  (others => 0);
               View_Key      : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Ephemeral_PK  : Aegis_Privacy.VM_Byte_Array (0 .. 1567) :=
                  (others => 0);
               Commit_Buf   : Aegis_VM_Types.Byte_Array (0 .. 63) := (others => 0);
               View_Buf     : Aegis_VM_Types.Byte_Array (0 .. 31) := (others => 0);
               Eph_Buf      : Aegis_VM_Types.Byte_Array (0 .. 1567) := (others => 0);
               --  Output: decrypted value if owned
               Decrypted     : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                  (others => 0);
               Is_Owner      : Boolean := False;
               Load_OK       : Boolean;
            begin
               --  Load output commitment from calldata
               Sphinx_Runtime.Load_Calldata (Commit_Ptr, 64, Commit_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 63 loop
                     Output_Commit (I) := Aegis_Privacy.VM_Byte (Commit_Buf (I));
                  end loop;
               end if;

               --  Load viewing key from calldata
               Sphinx_Runtime.Load_Calldata (View_Ptr, 32, View_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 31 loop
                     View_Key (I) := Aegis_Privacy.VM_Byte (View_Buf (I));
                  end loop;
               end if;

               --  Load ephemeral public key from calldata
               Sphinx_Runtime.Load_Calldata (Eph_Ptr, 1568, Eph_Buf, Load_OK);
               if Load_OK then
                  for I in 0 .. 1567 loop
                     Ephemeral_PK (I) := Aegis_Privacy.VM_Byte (Eph_Buf (I));
                  end loop;
               end if;

               --  Call real privacy implementation
               declare
                  Privacy_Result : Aegis_Privacy.Privacy_Result;
                  Decrypted_Val  : Interfaces.Unsigned_64;
                  Spend_Key      : Aegis_Privacy.VM_Byte_Array (0 .. 31) :=
                     (others => 0);  --  Not used for scan, only for spend
               begin
                  Aegis_Privacy.Scan_Confidential_Output (
                     Tx_Public_Key   => Ephemeral_PK (0 .. 31),
                     Output_Commit   => Output_Commit,
                     View_Key        => View_Key,
                     Spend_Key       => Spend_Key,
                     Is_Owned        => Is_Owner,
                     Decrypted_Value => Decrypted_Val,
                     Result          => Privacy_Result
                  );

                  --  Store decrypted value in return data if owned
                  if Is_Owner then
                     for I in 0 .. 7 loop
                        Decrypted (I) := Aegis_Privacy.VM_Byte (
                           Interfaces.Unsigned_8 ((Decrypted_Val / (256 ** I)) mod 256));
                     end loop;
                  end if;

                  Result := (
                     Success    => Privacy_Result.Success,
                     Gas_Used   => Gas_Amount (Privacy_Result.Gas_Used),
                     Return_Val => Bool_To_U256 (Is_Owner),
                     Error_Code => Privacy_Result.Error_Code
                  );
               end;
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
