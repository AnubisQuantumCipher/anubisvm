--  SPHINX RV32 Decode: Implementation

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Sphinx_RV32_Decode with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Bit Extraction Helpers
   ---------------------------------------------------------------------------

   function Extract_Bits (
      W  : Word;
      Lo : Natural;
      Hi : Natural
   ) return Word
   is
      Mask : constant Word := 2 ** (Hi - Lo + 1) - 1;
   begin
      return (W / (2 ** Lo)) and Mask;
   end Extract_Bits;

   function Get_Opcode (Instr : Word) return Opcode is
   begin
      return Opcode (Instr and 16#7F#);
   end Get_Opcode;

   function Get_Rd (Instr : Word) return Register_Index is
   begin
      return Register_Index ((Instr / 128) mod 32);
   end Get_Rd;

   function Get_Rs1 (Instr : Word) return Register_Index is
   begin
      return Register_Index ((Instr / 32768) mod 32);
   end Get_Rs1;

   function Get_Rs2 (Instr : Word) return Register_Index is
   begin
      return Register_Index ((Instr / 1048576) mod 32);
   end Get_Rs2;

   function Get_Funct3 (Instr : Word) return Funct3 is
   begin
      return Funct3 ((Instr / 4096) mod 8);
   end Get_Funct3;

   function Get_Funct7 (Instr : Word) return Funct7 is
   begin
      return Funct7 (Instr / 33554432);
   end Get_Funct7;

   ---------------------------------------------------------------------------
   --  Immediate Extraction
   ---------------------------------------------------------------------------

   function Get_Imm_I (Instr : Word) return Integer_32 is
      --  I-type: imm[11:0] = inst[31:20]
      Raw : constant Word := Instr / 1048576;  -- bits 31:20
      Imm : Integer_32;
   begin
      --  Sign extend from bit 11
      if Raw >= 2048 then
         --  Negative: extend sign
         Imm := Integer_32 (Raw) - 4096;
      else
         Imm := Integer_32 (Raw);
      end if;
      return Imm;
   end Get_Imm_I;

   function Get_Imm_S (Instr : Word) return Integer_32 is
      --  S-type: imm[11:5] = inst[31:25], imm[4:0] = inst[11:7]
      Hi  : constant Word := (Instr / 33554432) and 16#7F#;  -- bits 31:25
      Lo  : constant Word := (Instr / 128) and 16#1F#;       -- bits 11:7
      Raw : constant Word := (Hi * 32) or Lo;
      Imm : Integer_32;
   begin
      --  Sign extend from bit 11
      if Raw >= 2048 then
         Imm := Integer_32 (Raw) - 4096;
      else
         Imm := Integer_32 (Raw);
      end if;
      return Imm;
   end Get_Imm_S;

   function Get_Imm_B (Instr : Word) return Integer_32 is
      --  B-type: imm[12|10:5|4:1|11]
      --  inst[31]    -> imm[12]
      --  inst[30:25] -> imm[10:5]
      --  inst[11:8]  -> imm[4:1]
      --  inst[7]     -> imm[11]
      Bit_12 : constant Word := (Instr / 2147483648) and 1;  -- bit 31
      Bit_11 : constant Word := (Instr / 128) and 1;          -- bit 7
      Bits_10_5 : constant Word := (Instr / 33554432) and 16#3F#;  -- bits 30:25
      Bits_4_1  : constant Word := (Instr / 256) and 16#F#;   -- bits 11:8

      Raw : Word;
      Imm : Integer_32;
   begin
      Raw := (Bit_12 * 4096) or (Bit_11 * 2048) or (Bits_10_5 * 32) or (Bits_4_1 * 2);

      --  Sign extend from bit 12
      if Bit_12 = 1 then
         Imm := Integer_32 (Raw) - 8192;
      else
         Imm := Integer_32 (Raw);
      end if;
      return Imm;
   end Get_Imm_B;

   function Get_Imm_U (Instr : Word) return Integer_32 is
      --  U-type: imm[31:12] = inst[31:12], lower 12 bits are 0
      Raw : constant Word := Instr and 16#FFFFF000#;
   begin
      return Integer_32 (Raw);
   end Get_Imm_U;

   function Get_Imm_J (Instr : Word) return Integer_32 is
      --  J-type: imm[20|10:1|11|19:12]
      --  inst[31]    -> imm[20]
      --  inst[30:21] -> imm[10:1]
      --  inst[20]    -> imm[11]
      --  inst[19:12] -> imm[19:12]
      Bit_20 : constant Word := (Instr / 2147483648) and 1;      -- bit 31
      Bits_19_12 : constant Word := (Instr / 4096) and 16#FF#;   -- bits 19:12
      Bit_11 : constant Word := (Instr / 1048576) and 1;         -- bit 20
      Bits_10_1 : constant Word := (Instr / 2097152) and 16#3FF#;  -- bits 30:21

      Raw : Word;
      Imm : Integer_32;
   begin
      Raw := (Bit_20 * 1048576) or (Bits_19_12 * 4096) or
             (Bit_11 * 2048) or (Bits_10_1 * 2);

      --  Sign extend from bit 20
      if Bit_20 = 1 then
         Imm := Integer_32 (Raw) - 2097152;
      else
         Imm := Integer_32 (Raw);
      end if;
      return Imm;
   end Get_Imm_J;

   ---------------------------------------------------------------------------
   --  Format Detection
   ---------------------------------------------------------------------------

   function Get_Format (Op : Opcode) return Instruction_Format is
   begin
      case Op is
         when OP_LUI | OP_AUIPC =>
            return Format_U;

         when OP_JAL =>
            return Format_J;

         when OP_JALR | OP_LOAD | OP_IMM | OP_FENCE | OP_SYSTEM =>
            return Format_I;

         when OP_BRANCH =>
            return Format_B;

         when OP_STORE =>
            return Format_S;

         when OP_REG =>
            return Format_R;

         when others =>
            return Format_Unknown;
      end case;
   end Get_Format;

   ---------------------------------------------------------------------------
   --  Full Instruction Decoding
   ---------------------------------------------------------------------------

   function Decode (Instr : Word) return Decoded_Instruction is
      Result : Decoded_Instruction;
      Op     : constant Opcode := Get_Opcode (Instr);
      Format : constant Instruction_Format := Get_Format (Op);
   begin
      Result.Opcode := Op;
      Result.Format := Format;
      Result.Rd     := Get_Rd (Instr);
      Result.Rs1    := Get_Rs1 (Instr);
      Result.Rs2    := Get_Rs2 (Instr);
      Result.Funct3 := Get_Funct3 (Instr);
      Result.Funct7 := Get_Funct7 (Instr);
      Result.Valid  := True;

      --  Extract immediate based on format
      case Format is
         when Format_R =>
            Result.Imm := 0;  -- R-type has no immediate

         when Format_I =>
            Result.Imm := Get_Imm_I (Instr);

         when Format_S =>
            Result.Imm := Get_Imm_S (Instr);

         when Format_B =>
            Result.Imm := Get_Imm_B (Instr);

         when Format_U =>
            Result.Imm := Get_Imm_U (Instr);

         when Format_J =>
            Result.Imm := Get_Imm_J (Instr);

         when Format_Unknown =>
            Result.Imm := 0;
            Result.Valid := False;
      end case;

      return Result;
   end Decode;

   ---------------------------------------------------------------------------
   --  Instruction Classification
   ---------------------------------------------------------------------------

   function Get_Category (Instr : Decoded_Instruction) return Instruction_Category is
   begin
      if not Instr.Valid then
         return Cat_Unknown;
      end if;

      case Instr.Opcode is
         when OP_LUI | OP_AUIPC =>
            return Cat_Upper_Imm;

         when OP_JAL | OP_JALR =>
            return Cat_Jump;

         when OP_BRANCH =>
            return Cat_Branch;

         when OP_LOAD =>
            return Cat_Load;

         when OP_STORE =>
            return Cat_Store;

         when OP_IMM =>
            --  Check for shift operations
            if Instr.Funct3 = F3_SLLI or Instr.Funct3 = F3_SRLI_SRAI then
               return Cat_Shift;
            else
               return Cat_ALU;
            end if;

         when OP_REG =>
            --  Check for M extension (multiply/divide)
            if Instr.Funct7 = F7_MULDIV then
               if Instr.Funct3 in F3_MUL | F3_MULH | F3_MULHSU | F3_MULHU then
                  return Cat_Multiply;
               else
                  return Cat_Divide;
               end if;
            --  Check for shift operations
            elsif Instr.Funct3 = F3_SLL or Instr.Funct3 = F3_SRL_SRA then
               return Cat_Shift;
            else
               return Cat_ALU;
            end if;

         when OP_FENCE =>
            return Cat_Fence;

         when OP_SYSTEM =>
            return Cat_System;

         when others =>
            return Cat_Unknown;
      end case;
   end Get_Category;

   function Get_Instruction_Gas (Cat : Instruction_Category) return Gas_Amount is
   begin
      case Cat is
         when Cat_ALU       => return Gas_ALU_Basic;
         when Cat_Shift     => return Gas_Shift;
         when Cat_Multiply  => return Gas_Multiply;
         when Cat_Divide    => return Gas_Divide;
         when Cat_Load      => return Gas_Load;
         when Cat_Store     => return Gas_Store;
         when Cat_Branch    => return Gas_Branch;
         when Cat_Jump      => return Gas_Jump;
         when Cat_Upper_Imm => return Gas_Upper_Imm;
         when Cat_System    => return Gas_Ecall_Base;
         when Cat_Fence     => return Gas_Fence;
         when Cat_Unknown   => return 1;  -- Minimum for invalid instruction
      end case;
   end Get_Instruction_Gas;

end Sphinx_RV32_Decode;
