--  SPHINX RV32 Runner: Complete VM Integration Layer
--
--  This package provides the high-level interface for running RISC-V
--  contracts in the AnubisVM. It integrates:
--  - ELF loading (sphinx_rv32_loader)
--  - Instruction execution (sphinx_rv32_execute)
--  - Syscall handling with real crypto (sphinx_rv32_context)
--  - Gas metering and trapping
--
--  Usage:
--  1. Create a runner with Create_Runner
--  2. Load contract with Load_Contract
--  3. Set up context with Initialize_Context
--  4. Execute with Run_Contract
--  5. Get results and clean up
--
--  SPARK Mode: Off (uses heap allocation for context)

pragma SPARK_Mode (Off);

with System;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;
with Sphinx_RV32_Loader; use Sphinx_RV32_Loader;
with Sphinx_RV32_Context; use Sphinx_RV32_Context;

package Sphinx_RV32_Runner is

   ---------------------------------------------------------------------------
   --  Execution Result
   ---------------------------------------------------------------------------

   type Execution_Status is (
      Exec_Success,           -- Contract returned successfully
      Exec_Reverted,          -- Contract reverted
      Exec_Out_Of_Gas,        -- Gas exhausted
      Exec_Invalid_Opcode,    -- Invalid instruction
      Exec_Memory_Fault,      -- Memory access violation
      Exec_Alignment_Fault,   -- Misaligned memory access
      Exec_Syscall_Error,     -- Syscall failed
      Exec_Stack_Overflow,    -- Stack overflow
      Exec_Load_Failed,       -- ELF loading failed
      Exec_Max_Steps,         -- Hit maximum step limit
      Exec_Internal_Error     -- Internal VM error
   );

   type Execution_Result is record
      Status          : Execution_Status;
      Gas_Used        : Gas_Amount;
      Gas_Remaining   : Gas_Amount;
      Instructions    : Natural;
      Return_Data     : Byte_Array (0 .. Max_Return_Data_Size - 1);
      Return_Length   : Natural;
      Trap_Cause      : Trap_Cause;
      Trap_Value      : Word;
   end record;

   Default_Result : constant Execution_Result := (
      Status          => Exec_Internal_Error,
      Gas_Used        => 0,
      Gas_Remaining   => 0,
      Instructions    => 0,
      Return_Data     => (others => 0),
      Return_Length   => 0,
      Trap_Cause      => Trap_None,
      Trap_Value      => 0
   );

   ---------------------------------------------------------------------------
   --  VM Runner (Opaque Handle)
   ---------------------------------------------------------------------------

   type VM_Runner is private;

   ---------------------------------------------------------------------------
   --  Runner Lifecycle
   ---------------------------------------------------------------------------

   --  Create a new VM runner
   function Create_Runner return VM_Runner;

   --  Check if runner is valid
   function Is_Valid (Runner : VM_Runner) return Boolean;

   --  Destroy runner and free resources
   procedure Destroy_Runner (Runner : in out VM_Runner);

   ---------------------------------------------------------------------------
   --  Contract Loading
   ---------------------------------------------------------------------------

   --  Load ELF contract into runner
   procedure Load_Contract (
      Runner    : in Out VM_Runner;
      ELF_Data  : in     Byte_Array;
      Gas_Limit : in     Gas_Amount;
      Success   : out    Boolean
   );

   --  Get load status if loading failed
   function Get_Load_Status (Runner : VM_Runner) return Load_Status;

   ---------------------------------------------------------------------------
   --  Context Setup
   ---------------------------------------------------------------------------

   --  Initialize execution context
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
   );

   --  Set calldata for contract call
   procedure Set_Calldata (
      Runner : in Out VM_Runner;
      Data   : in     Byte_Array
   );

   --  Set storage callbacks for THOTH integration
   procedure Set_Storage_Callbacks (
      Runner : in Out VM_Runner;
      Load   : in     Storage_Load_Proc;
      Store  : in     Storage_Store_Proc
   );

   ---------------------------------------------------------------------------
   --  Execution
   ---------------------------------------------------------------------------

   --  Run contract to completion or trap
   procedure Run_Contract (
      Runner    : in Out VM_Runner;
      Max_Steps : in     Natural := Natural'Last;
      Result    : out    Execution_Result
   );

   --  Step through a single instruction (for debugging)
   procedure Step (
      Runner : in Out VM_Runner;
      Result : out    Execution_Result
   );

   ---------------------------------------------------------------------------
   --  State Inspection (for debugging/testing)
   ---------------------------------------------------------------------------

   --  Get register value
   function Get_Register (
      Runner : VM_Runner;
      Reg    : Register_Index
   ) return Word;

   --  Get program counter
   function Get_PC (Runner : VM_Runner) return Word;

   --  Get gas remaining
   function Get_Gas_Remaining (Runner : VM_Runner) return Gas_Amount;

   --  Get instruction count
   function Get_Instruction_Count (Runner : VM_Runner) return Natural;

   --  Get trap cause
   function Get_Trap (Runner : VM_Runner) return Trap_Cause;

   --  Read memory at address
   function Read_Memory (
      Runner : VM_Runner;
      Addr   : Word;
      Size   : Natural
   ) return Byte_Array;

private

   type Runner_State is record
      Interp   : Interpreter_State;
      Context  : Interpreter_Context_Ptr;
      Loaded   : Boolean;
      Load_Res : Load_Result;
   end record;

   type Runner_State_Ptr is access Runner_State;

   type VM_Runner is record
      State : Runner_State_Ptr;
   end record;

end Sphinx_RV32_Runner;
