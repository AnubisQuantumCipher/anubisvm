--  SPHINX RV32 Execute: Instruction Execution Engine
--
--  This package implements the instruction execution logic for RV32IM.
--  Each instruction is decoded, gas is charged, and the operation is performed.
--
--  SPARK Verification Properties:
--  - Register x0 always reads as 0
--  - Gas is charged before execution (no execution without gas)
--  - All arithmetic follows RISC-V spec (wrapping, sign extension)
--  - Memory operations are bounds-checked via Sphinx_RV32_Memory
--  - Traps are set correctly for all error conditions
--
--  Execution Model:
--  1. Fetch instruction at PC
--  2. Decode instruction
--  3. Charge gas (trap if insufficient)
--  4. Execute instruction
--  5. Update PC (normally PC+4, or branch/jump target)
--  6. Repeat until trap or halt
--
--  References:
--  - RISC-V Unprivileged ISA Specification v20191213

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types;  use Sphinx_RV32_Types;
with Sphinx_RV32_Decode; use Sphinx_RV32_Decode;
with Sphinx_RV32_Syscall;

package Sphinx_RV32_Execute with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Register File Operations (PLATINUM: Proves x0=0 invariant)
   ---------------------------------------------------------------------------

   --  Read register (x0 always returns 0)
   --  PLATINUM: Proves register read matches semantic model exactly
   function Read_Reg (
      Regs : Register_File;
      Idx  : Register_Index
   ) return Word with
      Global => null,
      Post   => Read_Reg'Result = Model_Read_Reg (Regs, Idx) and then
                (if Idx = 0 then Read_Reg'Result = 0);

   --  Write register (writes to x0 are ignored)
   --  PLATINUM: Proves register write matches semantic model exactly
   procedure Write_Reg (
      Regs  : in Out Register_File;
      Idx   : in     Register_Index;
      Value : in     Word
   ) with
      Global => null,
      Post   => Regs = Model_Write_Reg (Regs'Old, Idx, Value) and then
                Regs (0) = 0 and then  -- x0 always zero
                (if Idx /= 0 then Regs (Idx) = Value);

   ---------------------------------------------------------------------------
   --  Gas Management
   ---------------------------------------------------------------------------

   --  Charge gas, set trap if insufficient
   --  Pre: Trap must be None (we only charge gas when not already trapped)
   --  Post: Either gas is charged and Trap remains None,
   --        or insufficient gas and Trap becomes Out_Of_Gas
   --  Note: Instruction_Count, Regs, and PC are always preserved
   procedure Charge_Gas (
      CPU    : in Out CPU_State;
      Amount : in     Gas_Amount
   ) with
      Global => null,
      Pre    => CPU.Trap = Trap_None and then
                Amount <= Gas_Amount'Last - CPU.Gas_Used and then
                CPU.Gas_Used <= CPU.Gas_Limit,
      Post   => (if Has_Gas (CPU'Old, Amount) then
                    CPU.Gas_Used = CPU'Old.Gas_Used + Amount and
                    CPU.Trap = Trap_None
                 else
                    CPU.Trap = Trap_Out_Of_Gas and
                    CPU.Halted = True) and then
                --  Gas_Used <= Gas_Limit is always maintained
                CPU.Gas_Used <= CPU.Gas_Limit and then
                --  These fields are always preserved
                CPU.Instruction_Count = CPU'Old.Instruction_Count and then
                CPU.Regs = CPU'Old.Regs and then
                CPU.PC = CPU'Old.PC and then
                CPU.Gas_Limit = CPU'Old.Gas_Limit;

   --  Check if sufficient gas remains
   function Has_Gas (CPU : CPU_State; Amount : Gas_Amount) return Boolean is
      (CPU.Gas_Used <= CPU.Gas_Limit and then
       CPU.Gas_Limit - CPU.Gas_Used >= Amount)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  ALU Operations (PLATINUM: Postconditions prove RISC-V ISA semantics)
   ---------------------------------------------------------------------------

   --  ADD (with wrapping) - PLATINUM: Proves modular addition correctness
   function ALU_Add (A, B : Word) return Word with
      Global => null,
      Post   => ALU_Add'Result = Model_ADD (A, B);

   --  SUB (with wrapping) - PLATINUM: Proves modular subtraction correctness
   function ALU_Sub (A, B : Word) return Word with
      Global => null,
      Post   => ALU_Sub'Result = Model_SUB (A, B);

   --  AND - PLATINUM: Proves bitwise AND correctness
   function ALU_And (A, B : Word) return Word with
      Global => null,
      Post   => ALU_And'Result = Model_AND (A, B);

   --  OR - PLATINUM: Proves bitwise OR correctness
   function ALU_Or (A, B : Word) return Word with
      Global => null,
      Post   => ALU_Or'Result = Model_OR (A, B);

   --  XOR - PLATINUM: Proves bitwise XOR correctness
   function ALU_Xor (A, B : Word) return Word with
      Global => null,
      Post   => ALU_Xor'Result = Model_XOR (A, B);

   --  SLT (signed less than) - PLATINUM: Proves signed comparison correctness
   function ALU_SLT (A, B : Word) return Word with
      Global => null,
      Post   => ALU_SLT'Result in 0 | 1 and then
                ALU_SLT'Result = Model_SLT (A, B);

   --  SLTU (unsigned less than) - PLATINUM: Proves unsigned comparison correctness
   function ALU_SLTU (A, B : Word) return Word with
      Global => null,
      Post   => ALU_SLTU'Result in 0 | 1 and then
                ALU_SLTU'Result = Model_SLTU (A, B);

   --  SLL (shift left logical) - PLATINUM: Proves left shift correctness
   function ALU_SLL (A, B : Word) return Word with
      Global => null,
      Post   => ALU_SLL'Result = Model_SLL (A, B);

   --  SRL (shift right logical) - PLATINUM: Proves logical right shift correctness
   function ALU_SRL (A, B : Word) return Word with
      Global => null,
      Post   => ALU_SRL'Result = Model_SRL (A, B);

   --  SRA (shift right arithmetic) - PLATINUM: Proves arithmetic right shift
   function ALU_SRA (A, B : Word) return Word with
      Global => null,
      Post   => ALU_SRA'Result = Model_SRA (A, B);

   ---------------------------------------------------------------------------
   --  M Extension Operations (PLATINUM: Proves RISC-V M-extension semantics)
   ---------------------------------------------------------------------------

   --  MUL (lower 32 bits of product) - PLATINUM: Proves multiplication low bits
   function ALU_MUL (A, B : Word) return Word with
      Global => null,
      Post   => ALU_MUL'Result = Model_MUL (A, B);

   --  MULH (upper 32 bits of signed*signed product) - PLATINUM: Proves MULH
   function ALU_MULH (A, B : Word) return Word with
      Global => null,
      Post   => ALU_MULH'Result = Model_MULH (A, B);

   --  MULHU (upper 32 bits of unsigned*unsigned product) - PLATINUM: Proves MULHU
   function ALU_MULHU (A, B : Word) return Word with
      Global => null,
      Post   => ALU_MULHU'Result = Model_MULHU (A, B);

   --  MULHSU (upper 32 bits of signed*unsigned product) - PLATINUM: Proves MULHSU
   function ALU_MULHSU (A, B : Word) return Word with
      Global => null,
      Post   => ALU_MULHSU'Result = Model_MULHSU (A, B);

   --  DIV (signed division) - PLATINUM: Proves signed division with edge cases
   --  Division by zero: returns -1
   --  Overflow (MIN_INT / -1): returns MIN_INT
   function ALU_DIV (A, B : Word) return Word with
      Global => null,
      Post   => ALU_DIV'Result = Model_DIV (A, B);

   --  DIVU (unsigned division) - PLATINUM: Proves unsigned division
   --  Division by zero: returns MAX_UINT
   function ALU_DIVU (A, B : Word) return Word with
      Global => null,
      Post   => ALU_DIVU'Result = Model_DIVU (A, B);

   --  REM (signed remainder) - PLATINUM: Proves signed remainder with edge cases
   --  Division by zero: returns dividend
   --  Overflow (MIN_INT % -1): returns 0
   function ALU_REM (A, B : Word) return Word with
      Global => null,
      Post   => ALU_REM'Result = Model_REM (A, B);

   --  REMU (unsigned remainder) - PLATINUM: Proves unsigned remainder
   --  Division by zero: returns dividend
   function ALU_REMU (A, B : Word) return Word with
      Global => null,
      Post   => ALU_REMU'Result = Model_REMU (A, B);

   ---------------------------------------------------------------------------
   --  Branch Condition Evaluation
   ---------------------------------------------------------------------------

   --  Evaluate branch condition
   function Branch_Taken (
      Rs1_Val : Word;
      Rs2_Val : Word;
      F3      : Funct3
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Single Instruction Execution
   ---------------------------------------------------------------------------

   --  Execute one instruction on the interpreter state
   --
   --  This is the core execution function. It:
   --  1. Fetches the instruction at PC
   --  2. Decodes it
   --  3. Charges appropriate gas
   --  4. Executes the operation
   --  5. Updates PC and registers
   --
   --  On error, sets CPU.Trap and halts.
   --
   procedure Execute_Instruction (
      State : in Out Interpreter_State
   ) with
      Global => null,
      Pre    => Interpreter_State_Valid (State) and then
                not State.CPU.Halted and then
                State.CPU.Trap = Trap_None and then
                State.CPU.Instruction_Count < Natural'Last,
      Post   => State.CPU.Instruction_Count >= State'Old.CPU.Instruction_Count and then
                (State.CPU.Trap /= Trap_None or
                 State.CPU.Instruction_Count = State'Old.CPU.Instruction_Count + 1) and then
                --  State validity preserved when no trap and not halted
                (State.CPU.Trap /= Trap_None or State.CPU.Halted or
                 Interpreter_State_Valid (State));

   ---------------------------------------------------------------------------
   --  Main Execution Loop
   ---------------------------------------------------------------------------

   --  Run interpreter until trap or gas exhaustion
   --
   --  This is the main entry point for contract execution.
   --  It repeatedly calls Execute_Instruction until:
   --  - A terminal trap occurs (return, revert, error)
   --  - Gas is exhausted
   --  - Maximum instruction count is reached (anti-DoS)
   --
   --  Returns the final interpreter state with execution result.
   --
   procedure Run (
      State     : in Out Interpreter_State;
      Max_Steps : in     Natural := Natural'Last
   ) with
      Global => (In_Out => Sphinx_RV32_Syscall.Contract_Storage),
      Always_Terminates,
      Pre    => Interpreter_State_Valid (State) and then
                State.CPU.Trap = Trap_None,
      Post   => State.CPU.Halted or State.CPU.Trap /= Trap_None;

   ---------------------------------------------------------------------------
   --  Convenience Entry Points
   ---------------------------------------------------------------------------

   --  Execute contract from initial state
   procedure Execute_Contract (
      Initial_State : in     Interpreter_State;
      Max_Steps     : in     Natural := Natural'Last;
      Result        : out    Interpreter_Result
   ) with
      Global => (In_Out => Sphinx_RV32_Syscall.Contract_Storage),
      Pre    => Interpreter_State_Valid (Initial_State) and then
                Initial_State.CPU.Trap = Trap_None,
      Post   => Result_Consistent (Result);

end Sphinx_RV32_Execute;
