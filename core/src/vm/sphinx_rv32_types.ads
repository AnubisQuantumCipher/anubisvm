--  SPHINX RV32 Types: Core Type Definitions for RISC-V Interpreter
--
--  This package defines the foundational types for the SPARK-verified
--  RISC-V interpreter. All types are designed for formal verification.
--
--  Target ISA: RV32IM (32-bit base integer + multiply/divide extension)
--
--  Key Design Decisions:
--  1. Pure interpreter - no native code execution
--  2. All memory accesses through bounds-checked functions
--  3. Deterministic gas metering per instruction
--  4. ECALL is the ONLY capability boundary
--  5. All state transitions are SPARK-provable
--
--  Security Properties (formally verified):
--  - No out-of-bounds memory access
--  - Gas exhaustion traps correctly
--  - Register x0 is always zero
--  - PC alignment enforced (4-byte)
--  - Integer overflow handled per RISC-V spec
--
--  References:
--  - RISC-V Unprivileged ISA Specification v20191213
--  - KHEPRI Blueprint v1.0
--  - AnubisVM Security Model

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;

package Sphinx_RV32_Types with
   Pure,
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  RV32 Word Types
   ---------------------------------------------------------------------------

   --  RV32 uses 32-bit words
   subtype Word is Unsigned_32;
   subtype SWord is Integer_32;  -- Signed word for signed operations

   --  Byte type (8-bit)
   subtype RV_Byte is Unsigned_8;

   --  Half-word (16-bit) for compressed instructions (future)
   subtype Half is Unsigned_16;

   ---------------------------------------------------------------------------
   --  Register File
   ---------------------------------------------------------------------------

   --  32 integer registers: x0-x31
   --  x0 is hardwired to 0 (writes are ignored, reads return 0)
   --  x1 = ra (return address)
   --  x2 = sp (stack pointer)
   --  x8 = s0/fp (frame pointer)
   --  x10-x17 = a0-a7 (function arguments/return values)

   type Register_Index is range 0 .. 31;

   --  Named register aliases
   X0  : constant Register_Index := 0;   -- Zero register
   RA  : constant Register_Index := 1;   -- Return address
   SP  : constant Register_Index := 2;   -- Stack pointer
   GP  : constant Register_Index := 3;   -- Global pointer
   TP  : constant Register_Index := 4;   -- Thread pointer
   T0  : constant Register_Index := 5;   -- Temporary
   T1  : constant Register_Index := 6;   -- Temporary
   T2  : constant Register_Index := 7;   -- Temporary
   S0  : constant Register_Index := 8;   -- Saved/frame pointer
   FP  : constant Register_Index := 8;   -- Frame pointer (alias for S0)
   S1  : constant Register_Index := 9;   -- Saved register
   A0  : constant Register_Index := 10;  -- Argument/return value
   A1  : constant Register_Index := 11;  -- Argument/return value
   A2  : constant Register_Index := 12;  -- Argument
   A3  : constant Register_Index := 13;  -- Argument
   A4  : constant Register_Index := 14;  -- Argument
   A5  : constant Register_Index := 15;  -- Argument
   A6  : constant Register_Index := 16;  -- Argument
   A7  : constant Register_Index := 17;  -- Argument/syscall number
   S2  : constant Register_Index := 18;  -- Saved register
   S3  : constant Register_Index := 19;  -- Saved register
   S4  : constant Register_Index := 20;  -- Saved register
   S5  : constant Register_Index := 21;  -- Saved register
   S6  : constant Register_Index := 22;  -- Saved register
   S7  : constant Register_Index := 23;  -- Saved register
   S8  : constant Register_Index := 24;  -- Saved register
   S9  : constant Register_Index := 25;  -- Saved register
   S10 : constant Register_Index := 26;  -- Saved register
   S11 : constant Register_Index := 27;  -- Saved register
   T3  : constant Register_Index := 28;  -- Temporary
   T4  : constant Register_Index := 29;  -- Temporary
   T5  : constant Register_Index := 30;  -- Temporary
   T6  : constant Register_Index := 31;  -- Temporary

   --  Register file (x0 always returns 0)
   type Register_File is array (Register_Index) of Word;

   --  Initial register state (all zeros)
   Zero_Registers : constant Register_File := (others => 0);

   ---------------------------------------------------------------------------
   --  Program Counter
   ---------------------------------------------------------------------------

   --  PC must be 4-byte aligned (IALIGN=32 for RV32)
   --  PC is a full 32-bit address in the contract's virtual memory
   subtype Program_Counter is Word;

   --  Ghost: PC is aligned
   function PC_Aligned (PC : Program_Counter) return Boolean is
      (PC mod 4 = 0)
   with Ghost, Pure_Function;

   --  Entry point for contracts (after ELF loading)
   Default_Entry_Point : constant Program_Counter := 16#0000_1000#;

   ---------------------------------------------------------------------------
   --  Memory Model
   ---------------------------------------------------------------------------

   --  Contract memory is a linear array of bytes
   --  Size is configurable but bounded
   --  Note: Reduced to 256KB for stack-based allocation in tests
   --  Production systems should use heap allocation for larger memory
   Max_Memory_Size : constant := 256 * 1024;  -- 256 KB

   subtype Memory_Index is Natural range 0 .. Max_Memory_Size - 1;
   type Contract_Memory is array (Memory_Index) of RV_Byte;

   --  Ghost: Address is in bounds (handles Word > Natural'Last case)
   function Address_In_Bounds (Addr : Word; Size : Positive) return Boolean is
      (Size <= Max_Memory_Size and then Addr <= Word (Max_Memory_Size - Size))
   with Ghost, Pure_Function;

   --  Memory region for bounds tracking
   type Memory_Bounds is record
      Code_Start   : Word;
      Code_End     : Word;
      Data_Start   : Word;
      Data_End     : Word;
      Heap_Start   : Word;
      Heap_End     : Word;
      Stack_Start  : Word;
      Stack_End    : Word;
   end record;

   --  Default bounds (will be set by ELF loader)
   Default_Bounds : constant Memory_Bounds := (
      Code_Start   => 16#0000_1000#,
      Code_End     => 16#0000_FFFF#,
      Data_Start   => 16#0001_0000#,
      Data_End     => 16#000F_FFFF#,
      Heap_Start   => 16#0010_0000#,
      Heap_End     => 16#007F_FFFF#,
      Stack_Start  => 16#0080_0000#,
      Stack_End    => 16#00FF_FFFF#
   );

   ---------------------------------------------------------------------------
   --  Trap Types (Exceptions)
   ---------------------------------------------------------------------------

   --  Trap causes for interpreter state machine
   type Trap_Cause is (
      --  No trap (normal execution)
      Trap_None,

      --  Instruction traps
      Trap_Illegal_Instruction,  -- Unknown or malformed instruction
      Trap_Misaligned_Fetch,     -- PC not 4-byte aligned
      Trap_Breakpoint,           -- EBREAK instruction

      --  Memory traps
      Trap_Load_Misaligned,      -- Misaligned load address
      Trap_Load_Access_Fault,    -- Load from invalid/protected address
      Trap_Store_Misaligned,     -- Misaligned store address
      Trap_Store_Access_Fault,   -- Store to invalid/protected address

      --  Environment traps (ECALL results)
      Trap_Ecall,                -- ECALL instruction (syscall)
      Trap_Syscall_Error,        -- Syscall returned error

      --  Gas traps
      Trap_Out_Of_Gas,           -- Gas exhausted

      --  Arithmetic traps (optional, RISC-V doesn't trap these by default)
      Trap_Division_By_Zero,     -- DIV/REM by zero (we choose to trap)

      --  Control flow
      Trap_Return,               -- Contract returned normally
      Trap_Revert,               -- Contract reverted
      Trap_Halt                  -- Halt execution
   );

   --  Ghost: Trap is an error condition
   function Is_Error_Trap (T : Trap_Cause) return Boolean is
      (T in Trap_Illegal_Instruction | Trap_Misaligned_Fetch |
            Trap_Load_Misaligned | Trap_Load_Access_Fault |
            Trap_Store_Misaligned | Trap_Store_Access_Fault |
            Trap_Syscall_Error | Trap_Out_Of_Gas | Trap_Division_By_Zero)
   with Ghost, Pure_Function;

   --  Ghost: Trap terminates execution
   function Is_Terminal_Trap (T : Trap_Cause) return Boolean is
      (T in Trap_Return | Trap_Revert | Trap_Halt | Trap_Out_Of_Gas |
            Trap_Illegal_Instruction | Trap_Load_Access_Fault |
            Trap_Store_Access_Fault)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Gas Metering (Per-Instruction)
   ---------------------------------------------------------------------------

   --  Gas cost for each instruction category
   --  These are deliberately higher than native execution to account for
   --  interpretation overhead and ensure determinism

   --  Basic arithmetic (ADD, SUB, AND, OR, XOR, SLT, etc.)
   Gas_ALU_Basic       : constant Gas_Amount := 1;

   --  Shifts (SLL, SRL, SRA)
   Gas_Shift           : constant Gas_Amount := 1;

   --  Multiply (MUL, MULH, MULHU, MULHSU)
   Gas_Multiply        : constant Gas_Amount := 3;

   --  Divide (DIV, DIVU, REM, REMU)
   Gas_Divide          : constant Gas_Amount := 10;

   --  Memory load (LB, LH, LW, LBU, LHU)
   Gas_Load            : constant Gas_Amount := 3;

   --  Memory store (SB, SH, SW)
   Gas_Store           : constant Gas_Amount := 3;

   --  Branch (BEQ, BNE, BLT, BGE, BLTU, BGEU)
   Gas_Branch          : constant Gas_Amount := 2;

   --  Jump (JAL, JALR)
   Gas_Jump            : constant Gas_Amount := 2;

   --  Upper immediate (LUI, AUIPC)
   Gas_Upper_Imm       : constant Gas_Amount := 1;

   --  System (ECALL, EBREAK)
   --  ECALL cost is the base; actual syscall adds more
   Gas_Ecall_Base      : constant Gas_Amount := 10;

   --  Fence (FENCE, FENCE.I) - essentially no-ops in interpreter
   Gas_Fence           : constant Gas_Amount := 1;

   ---------------------------------------------------------------------------
   --  Syscall Gas Costs (Added on top of Gas_Ecall_Base)
   ---------------------------------------------------------------------------

   --  These match aegis_gas.ads costs
   Gas_Syscall_SLoad         : constant Gas_Amount := 200;
   Gas_Syscall_SStore_Set    : constant Gas_Amount := 20_000;
   Gas_Syscall_SStore_Reset  : constant Gas_Amount := 5_000;
   Gas_Syscall_SHA3_Base     : constant Gas_Amount := 30;
   Gas_Syscall_SHA3_Per_Word : constant Gas_Amount := 6;
   Gas_Syscall_MLDSA_Verify  : constant Gas_Amount := 30_000;
   Gas_Syscall_MLKEM_Decaps  : constant Gas_Amount := 25_000;
   Gas_Syscall_Call          : constant Gas_Amount := 700;
   Gas_Syscall_Create        : constant Gas_Amount := 32_000;

   ---------------------------------------------------------------------------
   --  Execution Context Types (SPARK-Compatible, No Pointers)
   --
   --  These types embed the execution context directly in Interpreter_State,
   --  eliminating the need for heap allocation or pointer-based callbacks.
   --  This enables full SPARK verification of the syscall path.
   ---------------------------------------------------------------------------

   --  Calldata buffer: maximum 64KB for contract call input
   Max_Calldata_Size : constant := 65536;
   subtype Calldata_Size is Natural range 0 .. Max_Calldata_Size;
   subtype Calldata_Index is Natural range 0 .. Max_Calldata_Size - 1;
   type Calldata_Buffer is array (Calldata_Index) of RV_Byte;

   --  Return data buffer: maximum 64KB for call return data
   Max_Return_Data_Size : constant := 65536;
   subtype Return_Data_Size is Natural range 0 .. Max_Return_Data_Size;
   subtype Return_Data_Index is Natural range 0 .. Max_Return_Data_Size - 1;
   type Return_Data_Buffer is array (Return_Data_Index) of RV_Byte;

   --  Execution Context: Embedded in Interpreter_State (no heap, no pointers)
   --
   --  This record contains all information needed by syscalls:
   --  - Transaction info (caller, self, origin, value)
   --  - Block info (number, timestamp, chain ID, gas price)
   --  - Calldata (embedded buffer, no pointer)
   --  - Return data (embedded buffer, no pointer)
   --  - Execution mode flags
   --
   --  Security Property: All data is value-semantics, no aliasing possible.
   type Execution_Context is record
      --  Transaction/Call Info
      Caller      : Contract_Address;  -- msg.sender
      Self        : Contract_Address;  -- address(this)
      Origin      : Contract_Address;  -- tx.origin
      Call_Value  : U256;              -- msg.value

      --  Block Info
      Block_Number : U256;
      Timestamp    : U256;
      Chain_ID     : U256;
      Gas_Price    : U256;

      --  Calldata (embedded buffer)
      Calldata     : Calldata_Buffer;
      Calldata_Len : Calldata_Size;

      --  Return Data (for previous call results)
      Return_Data     : Return_Data_Buffer;
      Return_Data_Len : Return_Data_Size;

      --  Execution mode flags
      Is_Static : Boolean;  -- True if in staticcall (no state changes)
      Is_Valid  : Boolean;  -- True if context is properly initialized
   end record;

   --  Empty context constant (using inline values for Pure package compatibility)
   Empty_Context : constant Execution_Context := (
      Caller          => (others => 0),
      Self            => (others => 0),
      Origin          => (others => 0),
      Call_Value      => (Limbs => (0, 0, 0, 0)),
      Block_Number    => (Limbs => (0, 0, 0, 0)),
      Timestamp       => (Limbs => (0, 0, 0, 0)),
      Chain_ID        => (Limbs => (0, 0, 0, 0)),
      Gas_Price       => (Limbs => (0, 0, 0, 0)),
      Calldata        => (others => 0),
      Calldata_Len    => 0,
      Return_Data     => (others => 0),
      Return_Data_Len => 0,
      Is_Static       => False,
      Is_Valid        => False
   );

   --  Ghost: Context buffers are within bounds (structural validity)
   --  Note: Is_Valid field indicates whether context data is meaningful for
   --  syscalls - checked at syscall time, not instruction execution time.
   function Context_Valid (Ctx : Execution_Context) return Boolean is
      (Ctx.Calldata_Len <= Max_Calldata_Size and then
       Ctx.Return_Data_Len <= Max_Return_Data_Size)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Instruction Encoding Types
   ---------------------------------------------------------------------------

   --  Opcode field (bits 6:0)
   subtype Opcode is Word range 0 .. 127;

   --  Register field (5 bits)
   subtype Reg_Field is Word range 0 .. 31;

   --  Function codes
   subtype Funct3 is Word range 0 .. 7;
   subtype Funct7 is Word range 0 .. 127;

   --  Immediate value types
   subtype Imm12 is Integer_32 range -2048 .. 2047;
   subtype Imm20 is Integer_32 range -524288 .. 524287;

   --  RV32 opcodes (bits 6:0)
   OP_LUI    : constant Opcode := 2#0110111#;  -- U-type
   OP_AUIPC  : constant Opcode := 2#0010111#;  -- U-type
   OP_JAL    : constant Opcode := 2#1101111#;  -- J-type
   OP_JALR   : constant Opcode := 2#1100111#;  -- I-type
   OP_BRANCH : constant Opcode := 2#1100011#;  -- B-type
   OP_LOAD   : constant Opcode := 2#0000011#;  -- I-type
   OP_STORE  : constant Opcode := 2#0100011#;  -- S-type
   OP_IMM    : constant Opcode := 2#0010011#;  -- I-type
   OP_REG    : constant Opcode := 2#0110011#;  -- R-type
   OP_FENCE  : constant Opcode := 2#0001111#;  -- I-type
   OP_SYSTEM : constant Opcode := 2#1110011#;  -- I-type

   --  Funct3 codes for branches
   F3_BEQ  : constant Funct3 := 2#000#;
   F3_BNE  : constant Funct3 := 2#001#;
   F3_BLT  : constant Funct3 := 2#100#;
   F3_BGE  : constant Funct3 := 2#101#;
   F3_BLTU : constant Funct3 := 2#110#;
   F3_BGEU : constant Funct3 := 2#111#;

   --  Funct3 codes for loads
   F3_LB  : constant Funct3 := 2#000#;
   F3_LH  : constant Funct3 := 2#001#;
   F3_LW  : constant Funct3 := 2#010#;
   F3_LBU : constant Funct3 := 2#100#;
   F3_LHU : constant Funct3 := 2#101#;

   --  Funct3 codes for stores
   F3_SB : constant Funct3 := 2#000#;
   F3_SH : constant Funct3 := 2#001#;
   F3_SW : constant Funct3 := 2#010#;

   --  Funct3 codes for ALU immediate
   F3_ADDI  : constant Funct3 := 2#000#;
   F3_SLTI  : constant Funct3 := 2#010#;
   F3_SLTIU : constant Funct3 := 2#011#;
   F3_XORI  : constant Funct3 := 2#100#;
   F3_ORI   : constant Funct3 := 2#110#;
   F3_ANDI  : constant Funct3 := 2#111#;
   F3_SLLI  : constant Funct3 := 2#001#;
   F3_SRLI_SRAI : constant Funct3 := 2#101#;

   --  Funct3 codes for ALU register
   F3_ADD_SUB : constant Funct3 := 2#000#;
   F3_SLL     : constant Funct3 := 2#001#;
   F3_SLT     : constant Funct3 := 2#010#;
   F3_SLTU    : constant Funct3 := 2#011#;
   F3_XOR     : constant Funct3 := 2#100#;
   F3_SRL_SRA : constant Funct3 := 2#101#;
   F3_OR      : constant Funct3 := 2#110#;
   F3_AND     : constant Funct3 := 2#111#;

   --  Funct3 codes for M extension (multiply/divide)
   F3_MUL    : constant Funct3 := 2#000#;
   F3_MULH   : constant Funct3 := 2#001#;
   F3_MULHSU : constant Funct3 := 2#010#;
   F3_MULHU  : constant Funct3 := 2#011#;
   F3_DIV    : constant Funct3 := 2#100#;
   F3_DIVU   : constant Funct3 := 2#101#;
   F3_REM    : constant Funct3 := 2#110#;
   F3_REMU   : constant Funct3 := 2#111#;

   --  Funct7 codes
   F7_ZERO    : constant Funct7 := 2#0000000#;  -- ADD, SRL, etc.
   F7_ALT     : constant Funct7 := 2#0100000#;  -- SUB, SRA
   F7_MULDIV  : constant Funct7 := 2#0000001#;  -- M extension

   ---------------------------------------------------------------------------
   --  Decoded Instruction
   ---------------------------------------------------------------------------

   --  Instruction format types
   type Instruction_Format is (
      Format_R,   -- Register-register
      Format_I,   -- Immediate
      Format_S,   -- Store
      Format_B,   -- Branch
      Format_U,   -- Upper immediate
      Format_J,   -- Jump
      Format_Unknown
   );

   --  Decoded instruction record
   type Decoded_Instruction is record
      Format  : Instruction_Format;
      Opcode  : Sphinx_RV32_Types.Opcode;
      Rd      : Register_Index;
      Rs1     : Register_Index;
      Rs2     : Register_Index;
      Funct3  : Sphinx_RV32_Types.Funct3;
      Funct7  : Sphinx_RV32_Types.Funct7;
      Imm     : Integer_32;  -- Sign-extended immediate
      Valid   : Boolean;
   end record;

   --  Invalid instruction constant
   Invalid_Instruction : constant Decoded_Instruction := (
      Format  => Format_Unknown,
      Opcode  => 0,
      Rd      => 0,
      Rs1     => 0,
      Rs2     => 0,
      Funct3  => 0,
      Funct7  => 0,
      Imm     => 0,
      Valid   => False
   );

   ---------------------------------------------------------------------------
   --  CPU State
   ---------------------------------------------------------------------------

   --  Complete CPU state for interpreter
   type CPU_State is record
      --  Registers
      Regs : Register_File;
      PC   : Program_Counter;

      --  Trap state
      Trap       : Trap_Cause;
      Trap_Value : Word;  -- Additional trap information

      --  Gas metering
      Gas_Limit     : Gas_Amount;
      Gas_Used      : Gas_Amount;

      --  Execution state
      Halted : Boolean;

      --  Statistics (for debugging/profiling)
      Instruction_Count : Natural;
   end record;

   --  Ghost: CPU state is valid
   function CPU_State_Valid (S : CPU_State) return Boolean is
      (S.Gas_Used <= S.Gas_Limit and then
       S.Regs (0) = 0 and then            -- x0 is always zero
       PC_Aligned (S.PC))
   with Ghost, Pure_Function;

   --  Initial CPU state
   function Initial_CPU_State (
      Entry_Point : Program_Counter;
      Gas_Limit   : Gas_Amount;
      Stack_Ptr   : Word
   ) return CPU_State is
      ((Regs => (SP => Stack_Ptr, others => 0),
        PC => Entry_Point,
        Trap => Trap_None,
        Trap_Value => 0,
        Gas_Limit => Gas_Limit,
        Gas_Used => 0,
        Halted => False,
        Instruction_Count => 0))
   with
      Pre  => PC_Aligned (Entry_Point),
      Post => CPU_State_Valid (Initial_CPU_State'Result) and then
              Initial_CPU_State'Result.PC = Entry_Point and then
              Initial_CPU_State'Result.Gas_Used = 0 and then
              not Initial_CPU_State'Result.Halted;

   ---------------------------------------------------------------------------
   --  Interpreter State (CPU + Memory + Context)
   --
   --  SPARK PURIST DESIGN: All state is embedded, no pointers.
   --  This enables complete formal verification of execution.
   ---------------------------------------------------------------------------

   --  Full interpreter state (value semantics, no heap)
   type Interpreter_State is record
      CPU     : CPU_State;
      Memory  : Contract_Memory;
      Bounds  : Memory_Bounds;
      Context : Execution_Context;  -- Embedded context (no pointer!)
   end record;

   --  Ghost: Interpreter state is valid
   function Interpreter_State_Valid (S : Interpreter_State) return Boolean is
      (CPU_State_Valid (S.CPU) and then Context_Valid (S.Context))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Execution Result
   ---------------------------------------------------------------------------

   --  Result from running the interpreter
   type Interpreter_Result is record
      Success       : Boolean;
      Final_State   : CPU_State;
      Trap          : Trap_Cause;
      Gas_Used      : Gas_Amount;
      Return_Data   : Hash256;
      Return_Length : Natural;
   end record;

   --  Ghost: Result is consistent
   function Result_Consistent (R : Interpreter_Result) return Boolean is
      (R.Gas_Used = R.Final_State.Gas_Used and then
       R.Trap = R.Final_State.Trap)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  PLATINUM LEVEL: Ghost Model Types for Semantic Verification
   --  These types and functions provide mathematical models for proving
   --  complete functional correctness of instruction execution.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Ghost: 64-bit Arithmetic for Overflow-Free Proofs
   --  Using 64-bit allows proving 32-bit overflow behavior
   ---------------------------------------------------------------------------

   --  Ghost: Convert Word to unsigned 64-bit for overflow-free arithmetic
   function To_U64 (W : Word) return Unsigned_64 is
      (Unsigned_64 (W))
   with Ghost, Pure_Function;

   --  Ghost: Convert signed 32-bit to signed 64-bit
   function To_S64 (W : Word) return Integer_64 is
      (if W < 16#8000_0000# then Integer_64 (W)
       else Integer_64 (W) - 16#1_0000_0000#)
   with Ghost, Pure_Function;

   --  Ghost: Low 32 bits of 64-bit value (modular truncation)
   function Low32 (V : Unsigned_64) return Word is
      (Word (V and 16#FFFF_FFFF#))
   with Ghost, Pure_Function;

   --  Ghost: High 32 bits of 64-bit value
   function High32 (V : Unsigned_64) return Word is
      (Word (Shift_Right (V, 32) and 16#FFFF_FFFF#))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Ghost: Mathematical Models for ALU Operations (RISC-V Specification)
   ---------------------------------------------------------------------------

   --  Model: ADD is modular addition (wrapping)
   function Model_ADD (A, B : Word) return Word is
      (Low32 (To_U64 (A) + To_U64 (B)))
   with Ghost, Pure_Function;

   --  Model: SUB is modular subtraction (wrapping)
   function Model_SUB (A, B : Word) return Word is
      (A - B)  -- Modular subtraction
   with Ghost, Pure_Function;

   --  Model: AND is bitwise AND
   function Model_AND (A, B : Word) return Word is
      (A and B)
   with Ghost, Pure_Function;

   --  Model: OR is bitwise OR
   function Model_OR (A, B : Word) return Word is
      (A or B)
   with Ghost, Pure_Function;

   --  Model: XOR is bitwise XOR
   function Model_XOR (A, B : Word) return Word is
      (A xor B)
   with Ghost, Pure_Function;

   --  Model: SLT - signed less than comparison
   --  Uses direct bit-level signed comparison logic matching implementation
   function Model_SLT (A, B : Word) return Word is
      (if (A >= 16#8000_0000# and B < 16#8000_0000#) then
         --  A is negative, B is non-negative: A < B
         1
       elsif (A < 16#8000_0000# and B >= 16#8000_0000#) then
         --  A is non-negative, B is negative: A >= B
         0
       elsif A < B then
         --  Same sign: compare as unsigned
         1
       else
         0)
   with Ghost, Pure_Function;

   --  Model: SLTU - unsigned less than comparison
   function Model_SLTU (A, B : Word) return Word is
      (if A < B then 1 else 0)
   with Ghost, Pure_Function;

   --  Model: SLL - shift left logical (lower 5 bits of B)
   function Model_SLL (A, B : Word) return Word is
      (Shift_Left (A, Natural (B and 16#1F#)))
   with Ghost, Pure_Function;

   --  Model: SRL - shift right logical (lower 5 bits of B)
   function Model_SRL (A, B : Word) return Word is
      (Shift_Right (A, Natural (B and 16#1F#)))
   with Ghost, Pure_Function;

   --  Model: SRA - shift right arithmetic (lower 5 bits of B)
   --  Sign bit is replicated into vacated positions
   function Model_SRA (A, B : Word) return Word is
      (Shift_Right_Arithmetic (A, Natural (B and 16#1F#)))
   with Ghost, Pure_Function;

   --  Model: MUL - lower 32 bits of product
   function Model_MUL (A, B : Word) return Word is
      (Low32 (To_U64 (A) * To_U64 (B)))
   with Ghost, Pure_Function;

   --  Model: MULHU - upper 32 bits of unsigned*unsigned product
   function Model_MULHU (A, B : Word) return Word is
      (High32 (To_U64 (A) * To_U64 (B)))
   with Ghost, Pure_Function;

   --  Model: MULH - upper 32 bits of signed*signed product
   --  Uses signed 64-bit arithmetic, modular conversion for two's complement
   function Model_MULH (A, B : Word) return Word is
      (High32 (Unsigned_64'Mod (To_S64 (A) * To_S64 (B))))
   with Ghost, Pure_Function;

   --  Model: MULHSU - upper 32 bits of signed*unsigned product
   --  A is sign-extended, B is zero-extended, multiply as signed, take high bits
   --  Uses modular conversion for two's complement representation
   function Model_MULHSU (A, B : Word) return Word is
      (High32 (Unsigned_64'Mod (To_S64 (A) * Integer_64 (B))))
   with Ghost, Pure_Function;

   --  Model: DIVU - unsigned division (returns MAX for div-by-zero)
   function Model_DIVU (A, B : Word) return Word is
      (if B = 0 then Word'Last else A / B)
   with Ghost, Pure_Function;

   --  Model: REMU - unsigned remainder (returns dividend for div-by-zero)
   function Model_REMU (A, B : Word) return Word is
      (if B = 0 then A else A rem B)
   with Ghost, Pure_Function;

   --  Model: DIV - signed division per RISC-V spec
   --  Division by zero: returns -1 (all ones)
   --  Overflow (MIN_INT / -1): returns MIN_INT (dividend unchanged)
   function Model_DIV (A, B : Word) return Word is
      (if B = 0 then
         Word'Last  -- -1 in two's complement
       elsif A = 16#8000_0000# and B = Word'Last then
         16#8000_0000#  -- Overflow: return MIN_INT
       else
         --  Normal signed division
         Word (Unsigned_32'Mod (To_S64 (A) / To_S64 (B))))
   with Ghost, Pure_Function;

   --  Model: REM - signed remainder per RISC-V spec
   --  Division by zero: returns dividend
   --  Overflow (MIN_INT % -1): returns 0
   function Model_REM (A, B : Word) return Word is
      (if B = 0 then
         A  -- Division by zero: return dividend
       elsif A = 16#8000_0000# and B = Word'Last then
         0  -- Overflow: remainder is 0
       else
         --  Normal signed remainder
         Word (Unsigned_32'Mod (To_S64 (A) rem To_S64 (B))))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Ghost: Register File Semantic Model
   ---------------------------------------------------------------------------

   --  Model: Read register - x0 always returns 0
   function Model_Read_Reg (Regs : Register_File; Idx : Register_Index)
      return Word is
      (if Idx = 0 then 0 else Regs (Idx))
   with Ghost, Pure_Function;

   --  Model: Write register - update, x0 stays 0
   function Model_Write_Reg (
      Regs  : Register_File;
      Idx   : Register_Index;
      Value : Word
   ) return Register_File is
      (if Idx = 0 then Regs
       else Regs'Update (Idx => Value))
   with Ghost, Pure_Function,
        Post => Model_Write_Reg'Result (0) = 0 and then
                (if Idx /= 0 then Model_Write_Reg'Result (Idx) = Value);

   ---------------------------------------------------------------------------
   --  Ghost: Memory Semantic Model (Little-Endian)
   ---------------------------------------------------------------------------

   --  Model: Load byte from memory at address
   function Model_Load_Byte (Mem : Contract_Memory; Addr : Memory_Index)
      return RV_Byte is
      (Mem (Addr))
   with Ghost, Pure_Function;

   --  Model: Load word from memory (little-endian)
   --  Word = Mem[Addr] | Mem[Addr+1]<<8 | Mem[Addr+2]<<16 | Mem[Addr+3]<<24
   function Model_Load_Word (Mem : Contract_Memory; Addr : Memory_Index)
      return Word is
      (Word (Mem (Addr)) or
       Shift_Left (Word (Mem (Addr + 1)), 8) or
       Shift_Left (Word (Mem (Addr + 2)), 16) or
       Shift_Left (Word (Mem (Addr + 3)), 24))
   with Ghost, Pure_Function,
        Pre => Addr + 3 <= Memory_Index'Last;

   ---------------------------------------------------------------------------
   --  Ghost: Instruction Execution Semantic Models
   ---------------------------------------------------------------------------

   --  Model: PC advancement for non-branch instructions
   function Model_PC_Next (PC : Program_Counter) return Program_Counter is
      (PC + 4)
   with Ghost, Pure_Function;

   --  Model: Branch target calculation
   function Model_Branch_Target (PC : Program_Counter; Offset : SWord)
      return Program_Counter is
      (if Offset >= 0 then PC + Word (Offset)
       elsif Offset = SWord'First then PC - 16#8000_0000#
       else PC - Word (-Offset - 1) - 1)
   with Ghost, Pure_Function;

   --  Model: LUI instruction result (load upper immediate)
   function Model_LUI (Imm : Integer_32) return Word is
      (if Imm >= 0 then Word (Imm)
       elsif Imm = Integer_32'First then 16#8000_0000#
       else Word (Unsigned_32'Last) - Word (-(Imm + 1)))
   with Ghost, Pure_Function;

   --  Model: AUIPC instruction result (add upper immediate to PC)
   function Model_AUIPC (PC : Program_Counter; Imm : Integer_32) return Word is
      (PC + Model_LUI (Imm))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Ghost: Gas Accounting Model
   --  (Note: Category-specific costs are defined in Sphinx_RV32_Decode)
   ---------------------------------------------------------------------------

   --  Model: Total gas after charging
   function Model_Gas_After (Used, Limit, Cost : Gas_Amount) return Gas_Amount is
      (if Limit - Used >= Cost then Used + Cost else Used)
   with Ghost, Pure_Function,
        Pre => Used <= Limit;

   --  Model: Trap after charging gas
   function Model_Gas_Trap (Used, Limit, Cost : Gas_Amount) return Boolean is
      (Limit - Used < Cost)
   with Ghost, Pure_Function,
        Pre => Used <= Limit;

end Sphinx_RV32_Types;
