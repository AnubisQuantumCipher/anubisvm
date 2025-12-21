--  SPHINX RV32 Runner: Implementation

pragma SPARK_Mode (Off);

with Ada.Unchecked_Deallocation;
with Sphinx_RV32_Execute; use Sphinx_RV32_Execute;
with Sphinx_RV32_Memory; use Sphinx_RV32_Memory;
with Sphinx_RV32_Context; use Sphinx_RV32_Context;

package body Sphinx_RV32_Runner is

   ---------------------------------------------------------------------------
   --  Helper: Copy from pointer-based context to embedded context
   ---------------------------------------------------------------------------

   procedure Sync_Context_To_Embedded (
      Ctx   : in     Interpreter_Context_Ptr;
      State : in Out Interpreter_State
   ) is
   begin
      if Ctx = null then
         State.Context := Empty_Context;
         return;
      end if;

      --  Copy fields from pointer-based context to embedded context
      State.Context.Caller := Ctx.Caller;
      State.Context.Self := Ctx.Self;
      State.Context.Origin := Ctx.Origin;
      State.Context.Call_Value := Ctx.Call_Value;
      State.Context.Block_Number := Ctx.Block_Number;
      State.Context.Timestamp := Ctx.Timestamp;
      State.Context.Chain_ID := Ctx.Chain_ID;
      State.Context.Gas_Price := Ctx.Gas_Price;
      State.Context.Is_Static := Ctx.Is_Static;
      State.Context.Is_Valid := Ctx.Is_Valid;

      --  Copy calldata (up to embedded buffer limit)
      State.Context.Calldata_Len := Natural'Min (
         Ctx.Calldata_Length,
         Max_Calldata_Size
      );
      for I in 0 .. State.Context.Calldata_Len - 1 loop
         State.Context.Calldata (I) := Ctx.Calldata (I);
      end loop;

      --  Copy return data
      State.Context.Return_Data_Len := Natural'Min (
         Ctx.Return_Data_Length,
         Max_Return_Data_Size
      );
      for I in 0 .. State.Context.Return_Data_Len - 1 loop
         State.Context.Return_Data (I) := Ctx.Return_Data (I);
      end loop;
   end Sync_Context_To_Embedded;

   ---------------------------------------------------------------------------
   --  Memory Management
   ---------------------------------------------------------------------------

   procedure Free_Runner_State is new Ada.Unchecked_Deallocation
     (Object => Runner_State, Name => Runner_State_Ptr);

   ---------------------------------------------------------------------------
   --  Runner Lifecycle
   ---------------------------------------------------------------------------

   function Create_Runner return VM_Runner is
      State : Runner_State_Ptr;
   begin
      State := new Runner_State'(
         Interp   => (
            CPU    => (
               Regs => (others => 0),
               PC => 0,
               Trap => Trap_None,
               Trap_Value => 0,
               Gas_Limit => 0,
               Gas_Used => 0,
               Halted => False,
               Instruction_Count => 0
            ),
            Memory => (others => 0),
            Bounds => Default_Bounds,
            Context => Empty_Context
         ),
         Context  => Create_Context,
         Loaded   => False,
         Load_Res => Failed_Load
      );
      return (State => State);
   end Create_Runner;

   function Is_Valid (Runner : VM_Runner) return Boolean is
   begin
      return Runner.State /= null;
   end Is_Valid;

   procedure Destroy_Runner (Runner : in Out VM_Runner) is
   begin
      if Runner.State /= null then
         if Runner.State.Context /= null then
            Free_Context (Runner.State.Context);
         end if;
         Free_Runner_State (Runner.State);
      end if;
   end Destroy_Runner;

   ---------------------------------------------------------------------------
   --  Contract Loading
   ---------------------------------------------------------------------------

   procedure Load_Contract (
      Runner    : in Out VM_Runner;
      ELF_Data  : in     Byte_Array;
      Gas_Limit : in     Gas_Amount;
      Success   : out    Boolean
   ) is
   begin
      if Runner.State = null then
         Success := False;
         return;
      end if;

      --  Load ELF into interpreter state
      Sphinx_RV32_Loader.Load_ELF (
         ELF_Data  => ELF_Data,
         State     => Runner.State.Interp,
         Gas_Limit => Gas_Limit,
         Result    => Runner.State.Load_Res
      );

      Runner.State.Loaded := Runner.State.Load_Res.Status = Load_Success;

      --  Sync pointer-based context to embedded context
      if Runner.State.Loaded then
         Sync_Context_To_Embedded (Runner.State.Context, Runner.State.Interp);
      end if;

      Success := Runner.State.Loaded;
   end Load_Contract;

   function Get_Load_Status (Runner : VM_Runner) return Load_Status is
   begin
      if Runner.State = null then
         return Load_Invalid_Magic;
      end if;
      return Runner.State.Load_Res.Status;
   end Get_Load_Status;

   ---------------------------------------------------------------------------
   --  Context Setup
   ---------------------------------------------------------------------------

   procedure Initialize_Context (
      Runner       : in Out VM_Runner;
      Caller       : in     Contract_Address;
      Self         : in     Contract_Address;
      Origin       : in     Contract_Address;
      Call_Value   : in     U256;
      Block_Number : in     U256;
      Timestamp    : in     U256;
      Chain_ID     : in     U256;
      Gas_Price    : in     U256;
      Is_Static    : in     Boolean := False
   ) is
   begin
      if Runner.State = null or else Runner.State.Context = null then
         return;
      end if;

      Sphinx_RV32_Context.Initialize_Context (
         Ctx          => Runner.State.Context,
         Caller       => Caller,
         Self         => Self,
         Origin       => Origin,
         Call_Value   => Call_Value,
         Block_Number => Block_Number,
         Timestamp    => Timestamp,
         Chain_ID     => Chain_ID,
         Gas_Price    => Gas_Price,
         Is_Static    => Is_Static
      );

      --  Sync updated context to embedded context
      Sync_Context_To_Embedded (Runner.State.Context, Runner.State.Interp);
   end Initialize_Context;

   procedure Set_Calldata (
      Runner : in Out VM_Runner;
      Data   : in     Byte_Array
   ) is
   begin
      if Runner.State = null or else Runner.State.Context = null then
         return;
      end if;
      Sphinx_RV32_Context.Set_Calldata (Runner.State.Context, Data);
      --  Sync to embedded context
      Sync_Context_To_Embedded (Runner.State.Context, Runner.State.Interp);
   end Set_Calldata;

   procedure Set_Storage_Callbacks (
      Runner : in Out VM_Runner;
      Load   : in     Storage_Load_Proc;
      Store  : in     Storage_Store_Proc
   ) is
   begin
      if Runner.State = null or else Runner.State.Context = null then
         return;
      end if;
      Runner.State.Context.Storage_Load_Callback := Load.all'Address;
      Runner.State.Context.Storage_Store_Callback := Store.all'Address;
   end Set_Storage_Callbacks;

   ---------------------------------------------------------------------------
   --  Execution
   ---------------------------------------------------------------------------

   function Map_Trap_To_Status (T : Trap_Cause) return Execution_Status is
   begin
      case T is
         when Trap_None =>
            return Exec_Success;
         when Trap_Return =>
            return Exec_Success;
         when Trap_Revert =>
            return Exec_Reverted;
         when Trap_Out_Of_Gas =>
            return Exec_Out_Of_Gas;
         when Trap_Illegal_Instruction =>
            return Exec_Invalid_Opcode;
         when Trap_Memory_Fault =>
            return Exec_Memory_Fault;
         when Trap_Alignment_Fault =>
            return Exec_Alignment_Fault;
         when Trap_Syscall_Error =>
            return Exec_Syscall_Error;
         when Trap_Stack_Overflow | Trap_Stack_Underflow =>
            return Exec_Stack_Overflow;
         when Trap_Invalid_Syscall =>
            return Exec_Syscall_Error;
         when Trap_Division_By_Zero =>
            return Exec_Internal_Error;
      end case;
   end Map_Trap_To_Status;

   procedure Build_Result (
      Runner : VM_Runner;
      Result : out Execution_Result
   ) is
      Interp : Interpreter_State renames Runner.State.Interp;
   begin
      Result := Default_Result;
      Result.Gas_Used := Interp.CPU.Gas_Used;
      Result.Gas_Remaining := Interp.CPU.Gas_Limit - Interp.CPU.Gas_Used;
      Result.Instructions := Interp.CPU.Instruction_Count;
      Result.Trap_Cause := Interp.CPU.Trap;
      Result.Trap_Value := Interp.CPU.Trap_Value;
      Result.Status := Map_Trap_To_Status (Interp.CPU.Trap);

      --  Copy return data from embedded context (set by syscalls)
      if Interp.Context.Return_Data_Len > 0 then
         Result.Return_Length := Interp.Context.Return_Data_Len;
         for I in 0 .. Interp.Context.Return_Data_Len - 1 loop
            Result.Return_Data (I) := Interp.Context.Return_Data (I);
         end loop;
      end if;
   end Build_Result;

   procedure Run_Contract (
      Runner    : in Out VM_Runner;
      Max_Steps : in     Natural := Natural'Last;
      Result    : out    Execution_Result
   ) is
   begin
      Result := Default_Result;

      if Runner.State = null then
         Result.Status := Exec_Internal_Error;
         return;
      end if;

      if not Runner.State.Loaded then
         Result.Status := Exec_Load_Failed;
         return;
      end if;

      --  Run the interpreter
      Sphinx_RV32_Execute.Run (Runner.State.Interp, Max_Steps);

      --  Check for max steps
      if Runner.State.Interp.CPU.Instruction_Count >= Max_Steps and
         Runner.State.Interp.CPU.Trap = Trap_None
      then
         Result.Status := Exec_Max_Steps;
         Result.Gas_Used := Runner.State.Interp.CPU.Gas_Used;
         Result.Gas_Remaining := Runner.State.Interp.CPU.Gas_Limit -
                                  Runner.State.Interp.CPU.Gas_Used;
         Result.Instructions := Runner.State.Interp.CPU.Instruction_Count;
         return;
      end if;

      Build_Result (Runner, Result);
   end Run_Contract;

   procedure Step (
      Runner : in Out VM_Runner;
      Result : out    Execution_Result
   ) is
   begin
      Result := Default_Result;

      if Runner.State = null then
         Result.Status := Exec_Internal_Error;
         return;
      end if;

      if not Runner.State.Loaded then
         Result.Status := Exec_Load_Failed;
         return;
      end if;

      --  Execute single instruction
      Sphinx_RV32_Execute.Execute_Instruction (Runner.State.Interp);

      Build_Result (Runner, Result);
   end Step;

   ---------------------------------------------------------------------------
   --  State Inspection
   ---------------------------------------------------------------------------

   function Get_Register (
      Runner : VM_Runner;
      Reg    : Register_Index
   ) return Word is
   begin
      if Runner.State = null then
         return 0;
      end if;
      return Read_Reg (Runner.State.Interp.CPU, Reg);
   end Get_Register;

   function Get_PC (Runner : VM_Runner) return Word is
   begin
      if Runner.State = null then
         return 0;
      end if;
      return Runner.State.Interp.CPU.PC;
   end Get_PC;

   function Get_Gas_Remaining (Runner : VM_Runner) return Gas_Amount is
   begin
      if Runner.State = null then
         return 0;
      end if;
      return Runner.State.Interp.CPU.Gas_Limit -
             Runner.State.Interp.CPU.Gas_Used;
   end Get_Gas_Remaining;

   function Get_Instruction_Count (Runner : VM_Runner) return Natural is
   begin
      if Runner.State = null then
         return 0;
      end if;
      return Runner.State.Interp.CPU.Instruction_Count;
   end Get_Instruction_Count;

   function Get_Trap (Runner : VM_Runner) return Trap_Cause is
   begin
      if Runner.State = null then
         return Trap_None;
      end if;
      return Runner.State.Interp.CPU.Trap;
   end Get_Trap;

   function Read_Memory (
      Runner : VM_Runner;
      Addr   : Word;
      Size   : Natural
   ) return Byte_Array is
      Result : Byte_Array (0 .. Size - 1) := (others => 0);
   begin
      if Runner.State = null then
         return Result;
      end if;

      for I in 0 .. Size - 1 loop
         if Natural (Addr) + I < Max_Memory_Size then
            Result (I) := Runner.State.Interp.Memory (Natural (Addr) + I);
         end if;
      end loop;

      return Result;
   end Read_Memory;

end Sphinx_RV32_Runner;
