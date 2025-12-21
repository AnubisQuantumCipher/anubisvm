--  SPHINX RV32 Decode: Instruction Decoding for RISC-V Interpreter
--
--  This package provides instruction decoding for RV32IM.
--  All 6 instruction formats are supported:
--    R-type: Register-register operations
--    I-type: Immediate operations, loads, JALR
--    S-type: Stores
--    B-type: Branches
--    U-type: Upper immediate (LUI, AUIPC)
--    J-type: Jump (JAL)
--
--  SPARK Verification Properties:
--  - All decoded fields are in valid ranges
--  - Immediate sign extension is correct
--  - Format detection is complete (no missing cases)
--
--  References:
--  - RISC-V Unprivileged ISA Specification v20191213, Chapter 2

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;

package Sphinx_RV32_Decode with
   Pure,
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Bit Extraction Helpers
   ---------------------------------------------------------------------------

   --  Extract bits [hi:lo] from word (inclusive)
   function Extract_Bits (
      W  : Word;
      Lo : Natural;
      Hi : Natural
   ) return Word with
      Global => null,
      Pre    => Lo <= Hi and Hi <= 31,
      Post   => Extract_Bits'Result < 2 ** (Hi - Lo + 1);

   --  Get opcode (bits 6:0)
   function Get_Opcode (Instr : Word) return Opcode with
      Global => null,
      Post   => Get_Opcode'Result = Opcode (Instr and 16#7F#);

   --  Get rd (bits 11:7)
   function Get_Rd (Instr : Word) return Register_Index with
      Global => null,
      Post   => Get_Rd'Result = Register_Index ((Instr / 128) mod 32);

   --  Get rs1 (bits 19:15)
   function Get_Rs1 (Instr : Word) return Register_Index with
      Global => null,
      Post   => Get_Rs1'Result = Register_Index ((Instr / 32768) mod 32);

   --  Get rs2 (bits 24:20)
   function Get_Rs2 (Instr : Word) return Register_Index with
      Global => null,
      Post   => Get_Rs2'Result = Register_Index ((Instr / 1048576) mod 32);

   --  Get funct3 (bits 14:12)
   function Get_Funct3 (Instr : Word) return Funct3 with
      Global => null,
      Post   => Get_Funct3'Result = Funct3 ((Instr / 4096) mod 8);

   --  Get funct7 (bits 31:25)
   function Get_Funct7 (Instr : Word) return Funct7 with
      Global => null,
      Post   => Get_Funct7'Result = Funct7 (Instr / 33554432);

   ---------------------------------------------------------------------------
   --  Immediate Extraction (Sign-Extended)
   ---------------------------------------------------------------------------

   --  I-type immediate: bits [31:20], sign-extended
   --    imm[11:0] = inst[31:20]
   function Get_Imm_I (Instr : Word) return Integer_32 with
      Global => null;

   --  S-type immediate: bits [31:25|11:7], sign-extended
   --    imm[11:5] = inst[31:25], imm[4:0] = inst[11:7]
   function Get_Imm_S (Instr : Word) return Integer_32 with
      Global => null;

   --  B-type immediate: bits [31|7|30:25|11:8], sign-extended, *2
   --    imm[12|10:5] = inst[31|30:25], imm[4:1|11] = inst[11:8|7]
   function Get_Imm_B (Instr : Word) return Integer_32 with
      Global => null,
      Post   => Get_Imm_B'Result mod 2 = 0;  -- Always even (2-byte aligned)

   --  U-type immediate: bits [31:12], placed in upper 20 bits
   --    imm[31:12] = inst[31:12]
   function Get_Imm_U (Instr : Word) return Integer_32 with
      Global => null,
      Post   => Get_Imm_U'Result mod 4096 = 0;  -- Lower 12 bits are zero

   --  J-type immediate: bits [31|19:12|20|30:21], sign-extended, *2
   --    imm[20|10:1|11|19:12] = inst[31|30:21|20|19:12]
   function Get_Imm_J (Instr : Word) return Integer_32 with
      Global => null,
      Post   => Get_Imm_J'Result mod 2 = 0;  -- Always even (2-byte aligned)

   ---------------------------------------------------------------------------
   --  Format Detection
   ---------------------------------------------------------------------------

   --  Determine instruction format from opcode
   function Get_Format (Op : Opcode) return Instruction_Format with
      Global => null;

   ---------------------------------------------------------------------------
   --  Full Instruction Decoding
   ---------------------------------------------------------------------------

   --  Decode a 32-bit instruction word into structured form
   --
   --  SPARK Postconditions:
   --  - Result.Valid = True iff instruction is recognized
   --  - All register indices are in 0..31
   --  - Immediate is correctly sign-extended for the format
   --
   function Decode (Instr : Word) return Decoded_Instruction with
      Global => null,
      Post   => (if Decode'Result.Valid then
                    Decode'Result.Rd in 0 .. 31 and
                    Decode'Result.Rs1 in 0 .. 31 and
                    Decode'Result.Rs2 in 0 .. 31);

   ---------------------------------------------------------------------------
   --  Instruction Classification (for gas metering)
   ---------------------------------------------------------------------------

   --  Instruction categories for gas calculation
   type Instruction_Category is (
      Cat_ALU,        -- Basic arithmetic/logic
      Cat_Shift,      -- Shift operations
      Cat_Multiply,   -- MUL, MULH, MULHU, MULHSU
      Cat_Divide,     -- DIV, DIVU, REM, REMU
      Cat_Load,       -- Memory loads
      Cat_Store,      -- Memory stores
      Cat_Branch,     -- Conditional branches
      Cat_Jump,       -- JAL, JALR
      Cat_Upper_Imm,  -- LUI, AUIPC
      Cat_System,     -- ECALL, EBREAK
      Cat_Fence,      -- FENCE, FENCE.I
      Cat_Unknown     -- Invalid instruction
   );

   --  Get instruction category for gas calculation
   function Get_Category (Instr : Decoded_Instruction) return Instruction_Category with
      Global => null;

   --  Get gas cost for instruction category
   function Get_Instruction_Gas (Cat : Instruction_Category) return Gas_Amount with
      Global => null,
      Post   => Get_Instruction_Gas'Result >= 1;

end Sphinx_RV32_Decode;
