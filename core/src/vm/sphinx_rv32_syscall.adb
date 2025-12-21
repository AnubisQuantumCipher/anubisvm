--  SPHINX RV32 Syscall: Implementation

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Sphinx_RV32_Memory; use Sphinx_RV32_Memory;
with Sphinx_RV32_Execute; use Sphinx_RV32_Execute;
with Sphinx_RV32_Storage; use Sphinx_RV32_Storage;
with Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;

package body Sphinx_RV32_Syscall with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper: Read arguments from registers
   ---------------------------------------------------------------------------

   function Get_A0 (Regs : Register_File) return Word is (Regs (A0));
   function Get_A1 (Regs : Register_File) return Word is (Regs (A1));
   function Get_A2 (Regs : Register_File) return Word is (Regs (A2));
   function Get_A3 (Regs : Register_File) return Word is (Regs (A3));
   function Get_A4 (Regs : Register_File) return Word is (Regs (A4));
   function Get_A5 (Regs : Register_File) return Word is (Regs (A5));
   function Get_A7 (Regs : Register_File) return Word is (Regs (A7));

   ---------------------------------------------------------------------------
   --  Syscall Validation
   ---------------------------------------------------------------------------

   function Is_Valid_Syscall (Num : Word) return Boolean is
   begin
      --  Only list actually implemented syscalls
      return Num in
         Syscall_SLoad | Syscall_SStore |
         Syscall_SHA3 | Syscall_Keccak256 |
         Syscall_MLDSA_Verify | Syscall_MLKEM_Decaps |
         Syscall_Caller | Syscall_Address |
         Syscall_CallValue | Syscall_CallData |
         Syscall_CallDataSize | Syscall_BlockNumber |
         Syscall_Timestamp | Syscall_GasPrice |
         Syscall_GasRemaining |
         Syscall_Return | Syscall_Revert |
         Syscall_ReturnData | Syscall_ReturnDataSize;
      --  Note: Call/StaticCall/DelegateCall/Create/Create2 are NOT implemented
      --  and will correctly trigger Syscall_Error if invoked
   end Is_Valid_Syscall;

   function Is_Valid_Memory_Range (
      Addr   : Word;
      Length : Word;
      State  : Interpreter_State
   ) return Boolean
   is
      pragma Unreferenced (State);
   begin
      --  Check bounds: addr + length <= max memory
      if Length = 0 then
         return True;
      end if;
      return Natural (Addr) <= Max_Memory_Size - Natural (Length);
   end Is_Valid_Memory_Range;

   ---------------------------------------------------------------------------
   --  Syscall Gas Calculation
   ---------------------------------------------------------------------------

   function Get_Syscall_Gas (
      Syscall_Num : Word;
      Data_Length : Natural := 0
   ) return Gas_Amount
   is
      Words : constant Natural := (Data_Length + 31) / 32;
   begin
      case Syscall_Num is
         when Syscall_SLoad =>
            return Gas_Syscall_SLoad;

         when Syscall_SStore =>
            return Gas_Syscall_SStore_Set;  -- Conservative estimate

         when Syscall_SHA3 | Syscall_Keccak256 =>
            return Gas_Syscall_SHA3_Base +
                   Gas_Amount (Words) * Gas_Syscall_SHA3_Per_Word;

         when Syscall_MLDSA_Verify =>
            return Gas_Syscall_MLDSA_Verify;

         when Syscall_MLKEM_Decaps =>
            return Gas_Syscall_MLKEM_Decaps;

         when Syscall_Caller | Syscall_Address |
              Syscall_CallValue | Syscall_CallDataSize |
              Syscall_BlockNumber | Syscall_Timestamp |
              Syscall_GasPrice | Syscall_GasRemaining =>
            return 2;  -- Cheap environment reads

         when Syscall_CallData =>
            return Gas_Amount (3 + Words);  -- Base + per-word copy

         when Syscall_Call | Syscall_StaticCall | Syscall_DelegateCall =>
            return Gas_Syscall_Call;

         when Syscall_Create | Syscall_Create2 =>
            return Gas_Syscall_Create;

         when Syscall_Return | Syscall_Revert =>
            return 0;  -- No gas for termination

         when Syscall_ReturnData | Syscall_ReturnDataSize =>
            return 2;  -- Cheap reads

         when others =>
            return 100;  -- Default for unknown syscalls
      end case;
   end Get_Syscall_Gas;

   ---------------------------------------------------------------------------
   --  THOTH: Storage Load
   ---------------------------------------------------------------------------

   procedure Handle_SLoad (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Slot_Ptr  : constant Word := Get_A0 (State.CPU.Regs);
      Value_Ptr : constant Word := Get_A1 (State.CPU.Regs);
   begin
      --  Validate memory ranges
      if not Is_Valid_Memory_Range (Slot_Ptr, 32, State) or
         not Is_Valid_Memory_Range (Value_Ptr, 32, State)
      then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Syscall_SLoad, Should_Halt => False);
         return;
      end if;

      --  Read key from memory and load value from storage
      declare
         Key        : Storage_Key_Bytes;
         Value      : Storage_Value_Bytes;
         Found      : Boolean;
         Write_OK   : Boolean;
         Output     : Byte_Array (0 .. 31);
      begin
         --  Read 32-byte key from contract memory
         for I in 0 .. 31 loop
            if Natural (Slot_Ptr) + I < Max_Memory_Size then
               Key (I) := State.Memory (Natural (Slot_Ptr) + I);
            else
               Key (I) := 0;
            end if;
         end loop;

         --  Load value from storage (returns Zero_Value if not found)
         SLoad (Contract_Storage, Key, Value, Found);

         --  Convert to output byte array
         for I in 0 .. 31 loop
            Output (I) := Byte (Value (I));
         end loop;

         --  Write value to contract memory
         Memory_Write_Block (State.Memory, Value_Ptr, Output, Write_OK);

         if Write_OK then
            Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                       Gas_Cost => Gas_Syscall_SLoad, Should_Halt => False);
         else
            Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                       Gas_Cost => Gas_Syscall_SLoad, Should_Halt => False);
         end if;
      end;
   end Handle_SLoad;

   ---------------------------------------------------------------------------
   --  THOTH: Storage Store
   ---------------------------------------------------------------------------

   procedure Handle_SStore (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Slot_Ptr  : constant Word := Get_A0 (State.CPU.Regs);
      Value_Ptr : constant Word := Get_A1 (State.CPU.Regs);
   begin
      --  Check for static call (no state changes allowed)
      if State.Context.Is_Static then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Syscall_SStore_Set, Should_Halt => False);
         return;
      end if;

      --  Validate memory ranges
      if not Is_Valid_Memory_Range (Slot_Ptr, 32, State) or
         not Is_Valid_Memory_Range (Value_Ptr, 32, State)
      then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Syscall_SStore_Set, Should_Halt => False);
         return;
      end if;

      --  Read key and value from memory and store to storage
      declare
         Key      : Storage_Key_Bytes;
         Value    : Storage_Value_Bytes;
         Store_OK : Boolean;
      begin
         --  Read 32-byte key from contract memory
         for I in 0 .. 31 loop
            if Natural (Slot_Ptr) + I < Max_Memory_Size then
               Key (I) := State.Memory (Natural (Slot_Ptr) + I);
            else
               Key (I) := 0;
            end if;
         end loop;

         --  Read 32-byte value from contract memory
         for I in 0 .. 31 loop
            if Natural (Value_Ptr) + I < Max_Memory_Size then
               Value (I) := State.Memory (Natural (Value_Ptr) + I);
            else
               Value (I) := 0;
            end if;
         end loop;

         --  Store to contract storage
         SStore (Contract_Storage, Key, Value, Store_OK);

         if Store_OK then
            Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                       Gas_Cost => Gas_Syscall_SStore_Set, Should_Halt => False);
         else
            --  Storage is full
            Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                       Gas_Cost => Gas_Syscall_SStore_Set, Should_Halt => False);
         end if;
      end;
   end Handle_SStore;

   ---------------------------------------------------------------------------
   --  ANKH: SHA3 Hash
   ---------------------------------------------------------------------------

   procedure Handle_SHA3 (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Input_Ptr  : constant Word := Get_A0 (State.CPU.Regs);
      Input_Len  : constant Word := Get_A1 (State.CPU.Regs);
      Output_Ptr : constant Word := Get_A2 (State.CPU.Regs);
      Gas_Cost   : Gas_Amount;
   begin
      Gas_Cost := Get_Syscall_Gas (Syscall_SHA3, Natural (Input_Len));

      --  Validate memory ranges
      if not Is_Valid_Memory_Range (Input_Ptr, Input_Len, State) or
         not Is_Valid_Memory_Range (Output_Ptr, 32, State)
      then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Cost, Should_Halt => False);
         return;
      end if;

      --  Compute SHA3-256 hash of input and write to output
      declare
         --  Maximum input size for stack allocation (64KB)
         Max_Input : constant := 65536;
         Input_Size : constant Natural := Natural'Min (Natural (Input_Len), Max_Input);

         --  Type conversions between Aegis_VM_Types and Anubis_Types
         Crypto_Input  : Anubis_Types.Byte_Array (0 .. Input_Size - 1);
         Crypto_Digest : Anubis_SHA3.SHA3_256_Digest;
         Output_Bytes  : Byte_Array (0 .. 31);
         Success       : Boolean;
      begin
         --  Read input from memory into crypto buffer
         for I in 0 .. Input_Size - 1 loop
            if Natural (Input_Ptr) + I < Max_Memory_Size then
               Crypto_Input (I) := Anubis_Types.Byte (
                  State.Memory (Natural (Input_Ptr) + I));
            else
               Crypto_Input (I) := 0;
            end if;
         end loop;

         --  Compute SHA3-256 hash
         Anubis_SHA3.SHA3_256 (Crypto_Input, Crypto_Digest);

         --  Convert crypto output to VM byte array
         for I in 0 .. 31 loop
            Output_Bytes (I) := Byte (Crypto_Digest (I));
         end loop;

         --  Write hash to memory
         Memory_Write_Block (State.Memory, Output_Ptr, Output_Bytes, Success);
         if Success then
            Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                       Gas_Cost => Gas_Cost, Should_Halt => False);
         else
            Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                       Gas_Cost => Gas_Cost, Should_Halt => False);
         end if;
      end;
   end Handle_SHA3;

   ---------------------------------------------------------------------------
   --  ANKH: ML-DSA-87 Verification
   ---------------------------------------------------------------------------

   procedure Handle_MLDSA_Verify (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      --  Register layout: a0=msg_ptr, a1=msg_len, a2=sig_ptr, a3=pubkey_ptr
      --  Note: Sig_Len is fixed at 4627 bytes, PK_Len is fixed at 2592 bytes
      Msg_Ptr    : constant Word := Get_A0 (State.CPU.Regs);
      Msg_Len    : constant Word := Get_A1 (State.CPU.Regs);
      Sig_Ptr    : constant Word := Get_A2 (State.CPU.Regs);
      Pubkey_Ptr : constant Word := Get_A3 (State.CPU.Regs);

      --  ML-DSA-87 sizes from FIPS 204
      PK_Size  : constant := Anubis_MLDSA.Public_Key_Bytes;   -- 2592
      Sig_Size : constant := Anubis_MLDSA.Signature_Bytes;    -- 4627
      Max_Msg  : constant := 65536;  -- Maximum message size
   begin
      --  Validate sizes
      if Msg_Len > Word (Max_Msg) then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Syscall_MLDSA_Verify, Should_Halt => False);
         return;
      end if;

      --  Validate memory ranges
      if not Is_Valid_Memory_Range (Msg_Ptr, Msg_Len, State) or
         not Is_Valid_Memory_Range (Sig_Ptr, Word (Sig_Size), State) or
         not Is_Valid_Memory_Range (Pubkey_Ptr, Word (PK_Size), State)
      then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Syscall_MLDSA_Verify, Should_Halt => False);
         return;
      end if;

      --  Read data from memory and verify signature
      declare
         use Anubis_MLDSA_Types;
         Msg_Size : constant Natural := Natural'Min (Natural (Msg_Len), Max_Msg);

         --  Crypto types for ML-DSA-87
         Crypto_PK  : Public_Key;
         Crypto_Sig : Signature;
         Crypto_Msg : Anubis_Types.Byte_Array (0 .. Msg_Size - 1);
         Valid      : Boolean;
      begin
         --  Read public key from memory
         for I in 0 .. PK_Size - 1 loop
            if Natural (Pubkey_Ptr) + I < Max_Memory_Size then
               Crypto_PK (I) := Anubis_Types.Byte (
                  State.Memory (Natural (Pubkey_Ptr) + I));
            else
               Crypto_PK (I) := 0;
            end if;
         end loop;

         --  Read signature from memory
         for I in 0 .. Sig_Size - 1 loop
            if Natural (Sig_Ptr) + I < Max_Memory_Size then
               Crypto_Sig (I) := Anubis_Types.Byte (
                  State.Memory (Natural (Sig_Ptr) + I));
            else
               Crypto_Sig (I) := 0;
            end if;
         end loop;

         --  Read message from memory
         for I in 0 .. Msg_Size - 1 loop
            if Natural (Msg_Ptr) + I < Max_Memory_Size then
               Crypto_Msg (I) := Anubis_Types.Byte (
                  State.Memory (Natural (Msg_Ptr) + I));
            else
               Crypto_Msg (I) := 0;
            end if;
         end loop;

         --  Call real ML-DSA-87 verification
         Valid := Anubis_MLDSA.Verify (Crypto_PK, Crypto_Msg, Crypto_Sig);

         --  Return result: 0 = valid, 1 = invalid
         if Valid then
            Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                       Gas_Cost => Gas_Syscall_MLDSA_Verify, Should_Halt => False);
         else
            Result := (Success => True, Return_Val => 1, Return_Val2 => 0,
                       Gas_Cost => Gas_Syscall_MLDSA_Verify, Should_Halt => False);
         end if;
      end;
   end Handle_MLDSA_Verify;

   ---------------------------------------------------------------------------
   --  ANKH: ML-KEM-1024 Decapsulation
   ---------------------------------------------------------------------------

   procedure Handle_MLKEM_Decaps (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      --  Register layout: a0=ct_ptr, a1=sk_ptr, a2=ss_ptr
      --  CT = ciphertext (1568 bytes), SK = secret key (3168 bytes), SS = shared secret (32 bytes)
      CT_Ptr : constant Word := Get_A0 (State.CPU.Regs);
      SK_Ptr : constant Word := Get_A1 (State.CPU.Regs);
      SS_Ptr : constant Word := Get_A2 (State.CPU.Regs);

      --  ML-KEM-1024 sizes from FIPS 203
      CT_Size : constant := Anubis_MLKEM.Ciphertext_Bytes;        -- 1568
      SK_Size : constant := Anubis_MLKEM.Decapsulation_Key_Bytes; -- 3168
      SS_Size : constant := Anubis_MLKEM.Shared_Secret_Bytes;     -- 32
   begin
      --  Validate memory ranges
      if not Is_Valid_Memory_Range (CT_Ptr, Word (CT_Size), State) or
         not Is_Valid_Memory_Range (SK_Ptr, Word (SK_Size), State) or
         not Is_Valid_Memory_Range (SS_Ptr, Word (SS_Size), State)
      then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Syscall_MLKEM_Decaps, Should_Halt => False);
         return;
      end if;

      --  Read data from memory and perform decapsulation
      declare
         use Anubis_MLKEM_Types;

         --  Crypto types for ML-KEM-1024
         Crypto_CT : MLKEM_Ciphertext;
         Crypto_SK : Decapsulation_Key;
         Crypto_SS : Shared_Secret;
         Output_SS : Byte_Array (0 .. SS_Size - 1);
      begin
         --  Read ciphertext from memory
         for I in 0 .. CT_Size - 1 loop
            if Natural (CT_Ptr) + I < Max_Memory_Size then
               Crypto_CT (I) := Anubis_Types.Byte (
                  State.Memory (Natural (CT_Ptr) + I));
            else
               Crypto_CT (I) := 0;
            end if;
         end loop;

         --  Read secret key from memory
         for I in 0 .. SK_Size - 1 loop
            if Natural (SK_Ptr) + I < Max_Memory_Size then
               Crypto_SK (I) := Anubis_Types.Byte (
                  State.Memory (Natural (SK_Ptr) + I));
            else
               Crypto_SK (I) := 0;
            end if;
         end loop;

         --  Call real ML-KEM-1024 decapsulation
         Anubis_MLKEM.Decaps (Crypto_SK, Crypto_CT, Crypto_SS);

         --  Convert crypto output to VM byte array
         for I in 0 .. SS_Size - 1 loop
            Output_SS (I) := Byte (Crypto_SS (I));
         end loop;

         --  Write shared secret to memory
         declare
            Success : Boolean;
         begin
            Memory_Write_Block (State.Memory, SS_Ptr, Output_SS, Success);
            if Success then
               Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                          Gas_Cost => Gas_Syscall_MLKEM_Decaps, Should_Halt => False);
            else
               Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                          Gas_Cost => Gas_Syscall_MLKEM_Decaps, Should_Halt => False);
            end if;
         end;
      end;
   end Handle_MLKEM_Decaps;

   ---------------------------------------------------------------------------
   --  Environment: Get Caller
   ---------------------------------------------------------------------------

   procedure Handle_Caller (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Addr_Ptr : constant Word := Get_A0 (State.CPU.Regs);
   begin
      if not Is_Valid_Memory_Range (Addr_Ptr, 32, State) then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => 2, Should_Halt => False);
         return;
      end if;

      --  Write caller address from embedded context (no pointer!)
      declare
         Caller_Bytes : Byte_Array (0 .. 31);
         Success      : Boolean;
      begin
         --  Copy from Contract_Address to Byte_Array
         for I in 0 .. 31 loop
            Caller_Bytes (I) := State.Context.Caller (I);
         end loop;

         Memory_Write_Block (State.Memory, Addr_Ptr, Caller_Bytes, Success);
         if Success then
            Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                       Gas_Cost => 2, Should_Halt => False);
         else
            Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                       Gas_Cost => 2, Should_Halt => False);
         end if;
      end;
   end Handle_Caller;

   ---------------------------------------------------------------------------
   --  Environment: Get Self Address
   ---------------------------------------------------------------------------

   procedure Handle_Address (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Addr_Ptr : constant Word := Get_A0 (State.CPU.Regs);
   begin
      if not Is_Valid_Memory_Range (Addr_Ptr, 32, State) then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => 2, Should_Halt => False);
         return;
      end if;

      --  Write self address from embedded context (no pointer!)
      declare
         Self_Bytes : Byte_Array (0 .. 31);
         Success    : Boolean;
      begin
         --  Copy from Contract_Address to Byte_Array
         for I in 0 .. 31 loop
            Self_Bytes (I) := State.Context.Self (I);
         end loop;

         Memory_Write_Block (State.Memory, Addr_Ptr, Self_Bytes, Success);
         if Success then
            Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                       Gas_Cost => 2, Should_Halt => False);
         else
            Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                       Gas_Cost => 2, Should_Halt => False);
         end if;
      end;
   end Handle_Address;

   ---------------------------------------------------------------------------
   --  Environment: Get Calldata Size
   ---------------------------------------------------------------------------

   procedure Handle_CallDataSize (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
   begin
      --  Return calldata size from embedded context (no pointer!)
      Result := (Success     => True,
                 Return_Val  => Word (State.Context.Calldata_Len),
                 Return_Val2 => 0,
                 Gas_Cost    => 2,
                 Should_Halt => False);
   end Handle_CallDataSize;

   ---------------------------------------------------------------------------
   --  Environment: Copy Calldata
   ---------------------------------------------------------------------------

   procedure Handle_CallData (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Dest_Ptr : constant Word := Get_A0 (State.CPU.Regs);
      Offset   : constant Word := Get_A1 (State.CPU.Regs);
      Length   : constant Word := Get_A2 (State.CPU.Regs);
      Gas_Cost : Gas_Amount;
      Nat_Offset : Natural;
      Nat_Length : Natural;
   begin
      Gas_Cost := Get_Syscall_Gas (Syscall_CallData, Natural (Length));

      if not Is_Valid_Memory_Range (Dest_Ptr, Length, State) then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Cost, Should_Halt => False);
         return;
      end if;

      --  Copy calldata from embedded context (no pointer!)
      Nat_Offset := Natural (Offset);
      Nat_Length := Natural (Length);

      --  Bounds check against actual calldata
      if Nat_Offset >= State.Context.Calldata_Len or else
         Nat_Length > State.Context.Calldata_Len - Nat_Offset
      then
         --  Request exceeds available calldata - zero-fill the destination
         declare
            Success : Boolean;
         begin
            Zero_Memory (State.Memory, Dest_Ptr, Nat_Length, Success);
            Result := (Success => Success, Return_Val => (if Success then 0 else 1),
                       Return_Val2 => 0, Gas_Cost => Gas_Cost, Should_Halt => False);
         end;
         return;
      end if;

      --  Copy calldata byte-by-byte to memory (SPARK-compatible: no variable-length arrays)
      declare
         Dest_Idx : Natural;
         Src_Idx  : Natural;
      begin
         for I in 0 .. Nat_Length - 1 loop
            Dest_Idx := Natural (Dest_Ptr) + I;
            Src_Idx := Nat_Offset + I;

            --  Bounds already checked above, so direct assignment is safe
            if Dest_Idx < Max_Memory_Size and then Src_Idx < Max_Calldata_Size then
               State.Memory (Dest_Idx) := State.Context.Calldata (Src_Idx);
            end if;

            pragma Loop_Invariant (I < Nat_Length);
         end loop;

         Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                    Gas_Cost => Gas_Cost, Should_Halt => False);
      end;
   end Handle_CallData;

   ---------------------------------------------------------------------------
   --  Environment: Get Block Number
   ---------------------------------------------------------------------------

   procedure Handle_BlockNumber (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      --  Extract lower 32 bits of block number from embedded context
      Block_Low : constant Word := Word (State.Context.Block_Number.Limbs (0) and 16#FFFF_FFFF#);
   begin
      Result := (Success     => True,
                 Return_Val  => Block_Low,
                 Return_Val2 => 0,
                 Gas_Cost    => 2,
                 Should_Halt => False);
   end Handle_BlockNumber;

   ---------------------------------------------------------------------------
   --  Environment: Get Timestamp
   ---------------------------------------------------------------------------

   procedure Handle_Timestamp (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      --  Extract lower 32 bits of timestamp from embedded context
      Timestamp_Low : constant Word := Word (State.Context.Timestamp.Limbs (0) and 16#FFFF_FFFF#);
   begin
      Result := (Success     => True,
                 Return_Val  => Timestamp_Low,
                 Return_Val2 => 0,
                 Gas_Cost    => 2,
                 Should_Halt => False);
   end Handle_Timestamp;

   ---------------------------------------------------------------------------
   --  Environment: Get Remaining Gas
   ---------------------------------------------------------------------------

   procedure Handle_GasRemaining (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Remaining : Gas_Amount;
   begin
      if State.CPU.Gas_Used <= State.CPU.Gas_Limit then
         Remaining := State.CPU.Gas_Limit - State.CPU.Gas_Used;
      else
         Remaining := 0;
      end if;

      --  Return lower 32 bits of remaining gas
      Result := (Success => True,
                 Return_Val => Word (Remaining mod 2**32),
                 Return_Val2 => 0,
                 Gas_Cost => 2,
                 Should_Halt => False);
   end Handle_GasRemaining;

   ---------------------------------------------------------------------------
   --  Environment: Get Call Value
   ---------------------------------------------------------------------------

   procedure Handle_CallValue (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Value_Ptr : constant Word := Get_A0 (State.CPU.Regs);
   begin
      --  Validate memory range (32 bytes for U256)
      if not Is_Valid_Memory_Range (Value_Ptr, 32, State) then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => 2, Should_Halt => False);
         return;
      end if;

      --  Write call value from embedded context to memory (little-endian U256)
      declare
         Value_Bytes : Byte_Array (0 .. 31);
         Success     : Boolean;
         Limb_Val    : Unsigned_64;
      begin
         --  Convert U256 (4 x 64-bit limbs) to 32-byte little-endian array
         for L in 0 .. 3 loop
            Limb_Val := State.Context.Call_Value.Limbs (L);
            for B in 0 .. 7 loop
               Value_Bytes (L * 8 + B) := Byte (Limb_Val and 16#FF#);
               Limb_Val := Shift_Right (Limb_Val, 8);
            end loop;
         end loop;

         Memory_Write_Block (State.Memory, Value_Ptr, Value_Bytes, Success);
         if Success then
            Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                       Gas_Cost => 2, Should_Halt => False);
         else
            Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                       Gas_Cost => 2, Should_Halt => False);
         end if;
      end;
   end Handle_CallValue;

   ---------------------------------------------------------------------------
   --  Environment: Get Gas Price
   ---------------------------------------------------------------------------

   procedure Handle_GasPrice (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      --  Extract lower 32 bits of gas price from embedded context
      GasPrice_Low : constant Word := Word (State.Context.Gas_Price.Limbs (0) and 16#FFFF_FFFF#);
   begin
      Result := (Success     => True,
                 Return_Val  => GasPrice_Low,
                 Return_Val2 => 0,
                 Gas_Cost    => 2,
                 Should_Halt => False);
   end Handle_GasPrice;

   ---------------------------------------------------------------------------
   --  Return Data: Get Return Data Size
   ---------------------------------------------------------------------------

   procedure Handle_ReturnDataSize (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
   begin
      --  Return size of return data from previous call
      Result := (Success     => True,
                 Return_Val  => Word (State.Context.Return_Data_Len),
                 Return_Val2 => 0,
                 Gas_Cost    => 2,
                 Should_Halt => False);
   end Handle_ReturnDataSize;

   ---------------------------------------------------------------------------
   --  Return Data: Copy Return Data
   ---------------------------------------------------------------------------

   procedure Handle_ReturnData (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Dest_Ptr : constant Word := Get_A0 (State.CPU.Regs);
      Offset   : constant Word := Get_A1 (State.CPU.Regs);
      Length   : constant Word := Get_A2 (State.CPU.Regs);
      Gas_Cost : constant Gas_Amount := Get_Syscall_Gas (Syscall_ReturnData, Natural (Length));
      Nat_Offset : Natural;
      Nat_Length : Natural;
   begin
      if not Is_Valid_Memory_Range (Dest_Ptr, Length, State) then
         Result := (Success => False, Return_Val => 1, Return_Val2 => 0,
                    Gas_Cost => Gas_Cost, Should_Halt => False);
         return;
      end if;

      Nat_Offset := Natural (Offset);
      Nat_Length := Natural (Length);

      --  Bounds check against actual return data
      if Nat_Offset >= State.Context.Return_Data_Len or else
         Nat_Length > State.Context.Return_Data_Len - Nat_Offset
      then
         --  Request exceeds available return data - zero-fill the destination
         declare
            Success : Boolean;
         begin
            Zero_Memory (State.Memory, Dest_Ptr, Nat_Length, Success);
            Result := (Success => Success, Return_Val => (if Success then 0 else 1),
                       Return_Val2 => 0, Gas_Cost => Gas_Cost, Should_Halt => False);
         end;
         return;
      end if;

      --  Copy return data byte-by-byte to memory
      declare
         Dest_Idx : Natural;
         Src_Idx  : Natural;
      begin
         for I in 0 .. Nat_Length - 1 loop
            Dest_Idx := Natural (Dest_Ptr) + I;
            Src_Idx := Nat_Offset + I;

            if Dest_Idx < Max_Memory_Size and then Src_Idx < Max_Return_Data_Size then
               State.Memory (Dest_Idx) := Byte (State.Context.Return_Data (Src_Idx));
            end if;
         end loop;

         Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                    Gas_Cost => Gas_Cost, Should_Halt => False);
      end;
   end Handle_ReturnData;

   ---------------------------------------------------------------------------
   --  Return: Normal contract return
   ---------------------------------------------------------------------------

   procedure Handle_Return (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Data_Ptr : constant Word := Get_A0 (State.CPU.Regs);
      Data_Len : constant Word := Get_A1 (State.CPU.Regs);
      Copy_Len : Natural;
   begin
      --  Copy return data from memory to context buffer
      if Data_Len > 0 then
         --  Clamp to maximum return data size
         Copy_Len := Natural'Min (Natural (Data_Len), Max_Return_Data_Size);

         --  Validate memory range
         if Is_Valid_Memory_Range (Data_Ptr, Word (Copy_Len), State) then
            --  Copy bytes from contract memory to return buffer
            for I in 0 .. Copy_Len - 1 loop
               if Natural (Data_Ptr) + I < Max_Memory_Size then
                  State.Context.Return_Data (I) :=
                     RV_Byte (State.Memory (Natural (Data_Ptr) + I));
               else
                  State.Context.Return_Data (I) := 0;
               end if;
            end loop;
            State.Context.Return_Data_Len := Copy_Len;
         else
            --  Invalid memory range - set empty return
            State.Context.Return_Data_Len := 0;
         end if;
      else
         State.Context.Return_Data_Len := 0;
      end if;

      --  Set return trap
      State.CPU.Trap := Trap_Return;
      State.CPU.Halted := True;

      Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                 Gas_Cost => 0, Should_Halt => True);
   end Handle_Return;

   ---------------------------------------------------------------------------
   --  Revert: Revert execution
   ---------------------------------------------------------------------------

   procedure Handle_Revert (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   )
   is
      Data_Ptr : constant Word := Get_A0 (State.CPU.Regs);
      Data_Len : constant Word := Get_A1 (State.CPU.Regs);
      Copy_Len : Natural;
   begin
      --  Copy revert data from memory to context buffer (same as return data)
      if Data_Len > 0 then
         --  Clamp to maximum return data size
         Copy_Len := Natural'Min (Natural (Data_Len), Max_Return_Data_Size);

         --  Validate memory range
         if Is_Valid_Memory_Range (Data_Ptr, Word (Copy_Len), State) then
            --  Copy bytes from contract memory to return buffer
            for I in 0 .. Copy_Len - 1 loop
               if Natural (Data_Ptr) + I < Max_Memory_Size then
                  State.Context.Return_Data (I) :=
                     RV_Byte (State.Memory (Natural (Data_Ptr) + I));
               else
                  State.Context.Return_Data (I) := 0;
               end if;
            end loop;
            State.Context.Return_Data_Len := Copy_Len;
         else
            --  Invalid memory range - set empty return
            State.Context.Return_Data_Len := 0;
         end if;
      else
         State.Context.Return_Data_Len := 0;
      end if;

      --  Set revert trap
      State.CPU.Trap := Trap_Revert;
      State.CPU.Halted := True;

      Result := (Success => True, Return_Val => 0, Return_Val2 => 0,
                 Gas_Cost => 0, Should_Halt => True);
   end Handle_Revert;

   ---------------------------------------------------------------------------
   --  Main Syscall Handler
   ---------------------------------------------------------------------------

   procedure Handle_Syscall (
      State : in Out Interpreter_State
   )
   is
      Syscall_Num : constant Word := Get_A7 (State.CPU.Regs);
      Result      : Syscall_Result;
   begin
      --  Clear the ECALL trap first
      State.CPU.Trap := Trap_None;

      --  Validate syscall number
      if not Is_Valid_Syscall (Syscall_Num) then
         State.CPU.Trap := Trap_Syscall_Error;
         State.CPU.Trap_Value := Syscall_Num;
         State.CPU.Halted := True;
         return;
      end if;

      --  Dispatch to appropriate handler
      case Syscall_Num is
         when Syscall_SLoad =>
            Handle_SLoad (State, Result);

         when Syscall_SStore =>
            Handle_SStore (State, Result);

         when Syscall_SHA3 | Syscall_Keccak256 =>
            Handle_SHA3 (State, Result);

         when Syscall_MLDSA_Verify =>
            Handle_MLDSA_Verify (State, Result);

         when Syscall_MLKEM_Decaps =>
            Handle_MLKEM_Decaps (State, Result);

         when Syscall_Caller =>
            Handle_Caller (State, Result);

         when Syscall_Address =>
            Handle_Address (State, Result);

         when Syscall_CallDataSize =>
            Handle_CallDataSize (State, Result);

         when Syscall_CallData =>
            Handle_CallData (State, Result);

         when Syscall_BlockNumber =>
            Handle_BlockNumber (State, Result);

         when Syscall_Timestamp =>
            Handle_Timestamp (State, Result);

         when Syscall_GasRemaining =>
            Handle_GasRemaining (State, Result);

         when Syscall_CallValue =>
            Handle_CallValue (State, Result);

         when Syscall_GasPrice =>
            Handle_GasPrice (State, Result);

         when Syscall_ReturnDataSize =>
            Handle_ReturnDataSize (State, Result);

         when Syscall_ReturnData =>
            Handle_ReturnData (State, Result);

         when Syscall_Return =>
            Handle_Return (State, Result);

         when Syscall_Revert =>
            Handle_Revert (State, Result);

         when others =>
            --  Unimplemented syscall (Call/Create etc.)
            State.CPU.Trap := Trap_Syscall_Error;
            State.CPU.Trap_Value := Syscall_Num;
            State.CPU.Halted := True;
            return;
      end case;

      --  Charge gas for the syscall
      if State.CPU.Gas_Used > State.CPU.Gas_Limit - Result.Gas_Cost then
         State.CPU.Trap := Trap_Out_Of_Gas;
         State.CPU.Halted := True;
         return;
      end if;
      State.CPU.Gas_Used := State.CPU.Gas_Used + Result.Gas_Cost;

      --  Set return value in a0
      Write_Reg (State.CPU.Regs, A0, Result.Return_Val);

      --  Set secondary return in a1 if applicable
      if Result.Return_Val2 /= 0 then
         Write_Reg (State.CPU.Regs, A1, Result.Return_Val2);
      end if;

      --  Advance PC (unless halting)
      if not Result.Should_Halt and State.CPU.Trap = Trap_None then
         State.CPU.PC := State.CPU.PC + 4;
      end if;

      --  Handle syscall error
      if not Result.Success and not Result.Should_Halt then
         --  Non-fatal error: just set error return value (already done above)
         null;
      end if;
   end Handle_Syscall;

   ---------------------------------------------------------------------------
   --  Storage Management
   ---------------------------------------------------------------------------

   procedure Reset_Storage is
   begin
      Contract_Storage := Empty_Storage;
   end Reset_Storage;

   function Get_Storage return Storage_State is
   begin
      return Contract_Storage;
   end Get_Storage;

end Sphinx_RV32_Syscall;
