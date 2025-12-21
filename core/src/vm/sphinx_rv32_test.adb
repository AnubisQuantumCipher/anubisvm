--  SPHINX RV32 Test: Test Harness for RISC-V Interpreter
--
--  This test program verifies the RISC-V interpreter implementation
--  by running simple instruction sequences and checking results.

pragma SPARK_Mode (Off);  -- Test harness uses I/O

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;
with Sphinx_RV32_Decode; use Sphinx_RV32_Decode;
with Sphinx_RV32_Memory; use Sphinx_RV32_Memory;
with Sphinx_RV32_Execute; use Sphinx_RV32_Execute;

procedure Sphinx_RV32_Test is

   --  Helper: Print test result
   procedure Print_Result (Name : String; Passed : Boolean) is
   begin
      if Passed then
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Print_Result;

   --  Helper: Create test state with code at default entry point
   function Make_Test_State (
      Code      : Byte_Array;
      Gas_Limit : Gas_Amount := 1000
   ) return Interpreter_State
   is
      State : Interpreter_State;
      Entry_Addr : constant Word := 16#1000#;
   begin
      --  Initialize CPU
      State.CPU := (
         Regs => (SP => 16#0003_FF00#, others => 0),  -- Stack near top of 256KB
         PC => Entry_Addr,
         Trap => Trap_None,
         Trap_Value => 0,
         Gas_Limit => Gas_Limit,
         Gas_Used => 0,
         Halted => False,
         Instruction_Count => 0
      );

      --  Initialize memory with code
      State.Memory := (others => 0);
      for I in Code'Range loop
         State.Memory (Natural (Entry_Addr) + I - Code'First) := Code (I);
      end loop;

      --  Set bounds (fitting within 256KB)
      State.Bounds := (
         Code_Start  => Entry_Addr,
         Code_End    => Entry_Addr + Word (Code'Length),
         Data_Start  => 16#2000#,
         Data_End    => 16#3000#,
         Heap_Start  => 16#3000#,
         Heap_End    => 16#0003_0000#,
         Stack_Start => 16#0003_0000#,
         Stack_End   => 16#0003_FFFF#
      );

      --  Initialize context with empty values
      State.Context := Empty_Context;

      return State;
   end Make_Test_State;

   --  Helper: Encode ADDI instruction
   --  addi rd, rs1, imm
   function Encode_ADDI (Rd, Rs1 : Register_Index; Imm : Integer_32) return Word is
      --  Handle negative immediates via two's complement
      Imm_Bits : constant Word := (if Imm >= 0
                                   then Word (Imm) and 16#FFF#
                                   else Word (16#1000# + Imm) and 16#FFF#);
   begin
      return Imm_Bits * 1048576 +  -- imm[11:0] at bits 31:20
             Word (Rs1) * 32768 +  -- rs1 at bits 19:15
             0 * 4096 +            -- funct3 = 000 at bits 14:12
             Word (Rd) * 128 +     -- rd at bits 11:7
             OP_IMM;               -- opcode at bits 6:0
   end Encode_ADDI;

   --  Helper: Encode ADD instruction
   --  add rd, rs1, rs2
   function Encode_ADD (Rd, Rs1, Rs2 : Register_Index) return Word is
   begin
      return 0 * 33554432 +        -- funct7 = 0000000 at bits 31:25
             Word (Rs2) * 1048576 + -- rs2 at bits 24:20
             Word (Rs1) * 32768 +   -- rs1 at bits 19:15
             0 * 4096 +             -- funct3 = 000 at bits 14:12
             Word (Rd) * 128 +      -- rd at bits 11:7
             OP_REG;                -- opcode at bits 6:0
   end Encode_ADD;

   --  Helper: Encode SUB instruction
   function Encode_SUB (Rd, Rs1, Rs2 : Register_Index) return Word is
   begin
      return 32 * 33554432 +       -- funct7 = 0100000 at bits 31:25
             Word (Rs2) * 1048576 + -- rs2 at bits 24:20
             Word (Rs1) * 32768 +   -- rs1 at bits 19:15
             0 * 4096 +             -- funct3 = 000 at bits 14:12
             Word (Rd) * 128 +      -- rd at bits 11:7
             OP_REG;                -- opcode at bits 6:0
   end Encode_SUB;

   --  Helper: Encode LUI instruction
   --  lui rd, imm
   function Encode_LUI (Rd : Register_Index; Imm : Word) return Word is
   begin
      return (Imm and 16#FFFFF000#) +  -- imm[31:12] at bits 31:12
             Word (Rd) * 128 +          -- rd at bits 11:7
             OP_LUI;                    -- opcode at bits 6:0
   end Encode_LUI;

   --  Helper: Encode ECALL instruction
   function Encode_ECALL return Word is
   begin
      return OP_SYSTEM;  -- ecall is system opcode with imm=0
   end Encode_ECALL;

   --  Helper: Encode JAL instruction
   --  jal rd, offset
   function Encode_JAL (Rd : Register_Index; Offset : Integer_32) return Word is
      Off : constant Word := Word (Offset);
      Bit_20   : constant Word := (Off / 1048576) and 1;
      Bits_10_1 : constant Word := (Off / 2) and 16#3FF#;
      Bit_11   : constant Word := (Off / 2048) and 1;
      Bits_19_12 : constant Word := (Off / 4096) and 16#FF#;
   begin
      return Bit_20 * 2147483648 +     -- imm[20] at bit 31
             Bits_10_1 * 2097152 +     -- imm[10:1] at bits 30:21
             Bit_11 * 1048576 +        -- imm[11] at bit 20
             Bits_19_12 * 4096 +       -- imm[19:12] at bits 19:12
             Word (Rd) * 128 +         -- rd at bits 11:7
             OP_JAL;                   -- opcode at bits 6:0
   end Encode_JAL;

   --  Helper: Encode JALR instruction
   --  jalr rd, rs1, offset
   function Encode_JALR (Rd, Rs1 : Register_Index; Offset : Integer_32) return Word is
      Imm_Bits : constant Word := (if Offset >= 0
                                   then Word (Offset) and 16#FFF#
                                   else Word (16#1000# + Offset) and 16#FFF#);
   begin
      return Imm_Bits * 1048576 +       -- imm[11:0] at bits 31:20
             Word (Rs1) * 32768 +       -- rs1 at bits 19:15
             0 * 4096 +                 -- funct3 = 000 at bits 14:12
             Word (Rd) * 128 +          -- rd at bits 11:7
             OP_JALR;                   -- opcode at bits 6:0
   end Encode_JALR;

   --  Helper: Encode BEQ instruction
   --  beq rs1, rs2, offset
   function Encode_BEQ (Rs1, Rs2 : Register_Index; Offset : Integer_32) return Word is
      Off : constant Word := (if Offset >= 0
                              then Word (Offset)
                              else Word (16#2000# + Offset) and 16#1FFF#);
      Bit_12   : constant Word := (Off / 4096) and 1;
      Bits_10_5 : constant Word := (Off / 32) and 16#3F#;
      Bits_4_1 : constant Word := (Off / 2) and 16#F#;
      Bit_11   : constant Word := (Off / 2048) and 1;
   begin
      return Bit_12 * 2147483648 +      -- imm[12] at bit 31
             Bits_10_5 * 33554432 +     -- imm[10:5] at bits 30:25
             Word (Rs2) * 1048576 +     -- rs2 at bits 24:20
             Word (Rs1) * 32768 +       -- rs1 at bits 19:15
             0 * 4096 +                 -- funct3 = 000 at bits 14:12
             Bits_4_1 * 256 +           -- imm[4:1] at bits 11:8
             Bit_11 * 128 +             -- imm[11] at bit 7
             OP_BRANCH;                 -- opcode at bits 6:0
   end Encode_BEQ;

   --  Helper: Encode BNE instruction
   --  bne rs1, rs2, offset
   function Encode_BNE (Rs1, Rs2 : Register_Index; Offset : Integer_32) return Word is
      Off : constant Word := (if Offset >= 0
                              then Word (Offset)
                              else Word (16#2000# + Offset) and 16#1FFF#);
      Bit_12   : constant Word := (Off / 4096) and 1;
      Bits_10_5 : constant Word := (Off / 32) and 16#3F#;
      Bits_4_1 : constant Word := (Off / 2) and 16#F#;
      Bit_11   : constant Word := (Off / 2048) and 1;
   begin
      return Bit_12 * 2147483648 +      -- imm[12] at bit 31
             Bits_10_5 * 33554432 +     -- imm[10:5] at bits 30:25
             Word (Rs2) * 1048576 +     -- rs2 at bits 24:20
             Word (Rs1) * 32768 +       -- rs1 at bits 19:15
             1 * 4096 +                 -- funct3 = 001 at bits 14:12
             Bits_4_1 * 256 +           -- imm[4:1] at bits 11:8
             Bit_11 * 128 +             -- imm[11] at bit 7
             OP_BRANCH;                 -- opcode at bits 6:0
   end Encode_BNE;

   --  Helper: Encode SW (store word) instruction
   --  sw rs2, offset(rs1)
   function Encode_SW (Rs1, Rs2 : Register_Index; Offset : Integer_32) return Word is
      Imm : constant Word := (if Offset >= 0
                              then Word (Offset) and 16#FFF#
                              else Word (16#1000# + Offset) and 16#FFF#);
      Imm_11_5 : constant Word := (Imm / 32) and 16#7F#;
      Imm_4_0  : constant Word := Imm and 16#1F#;
   begin
      return Imm_11_5 * 33554432 +    -- imm[11:5] at bits 31:25
             Word (Rs2) * 1048576 +   -- rs2 at bits 24:20
             Word (Rs1) * 32768 +     -- rs1 at bits 19:15
             2 * 4096 +               -- funct3 = 010 at bits 14:12
             Imm_4_0 * 128 +          -- imm[4:0] at bits 11:7
             OP_STORE;                -- opcode at bits 6:0
   end Encode_SW;

   --  Helper: Encode LW (load word) instruction
   --  lw rd, offset(rs1)
   function Encode_LW (Rd, Rs1 : Register_Index; Offset : Integer_32) return Word is
      Imm_Bits : constant Word := (if Offset >= 0
                                   then Word (Offset) and 16#FFF#
                                   else Word (16#1000# + Offset) and 16#FFF#);
   begin
      return Imm_Bits * 1048576 +     -- imm[11:0] at bits 31:20
             Word (Rs1) * 32768 +     -- rs1 at bits 19:15
             2 * 4096 +               -- funct3 = 010 at bits 14:12
             Word (Rd) * 128 +        -- rd at bits 11:7
             OP_LOAD;                 -- opcode at bits 6:0
   end Encode_LW;

   --  Helper: Convert Word to 4 bytes (little-endian)
   function Word_To_Bytes (W : Word) return Byte_Array is
      Result : Byte_Array (0 .. 3);
   begin
      Result (0) := RV_Byte (W and 16#FF#);
      Result (1) := RV_Byte ((W / 256) and 16#FF#);
      Result (2) := RV_Byte ((W / 65536) and 16#FF#);
      Result (3) := RV_Byte ((W / 16777216) and 16#FF#);
      return Result;
   end Word_To_Bytes;

   --  Test variables
   All_Passed : Boolean := True;

   --  Test 1: ADDI instruction
   procedure Test_ADDI is
      --  addi x1, x0, 42  ; x1 = 42
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 42)) &
                                     Word_To_Bytes (Encode_ECALL);  -- syscall to return
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);
      Print_Result ("ADDI: x1 = 42", State.CPU.Regs (1) = 42);
      All_Passed := All_Passed and State.CPU.Regs (1) = 42;
   end Test_ADDI;

   --  Test 2: ADD instruction
   procedure Test_ADD is
      --  addi x1, x0, 10  ; x1 = 10
      --  addi x2, x0, 20  ; x2 = 20
      --  add x3, x1, x2   ; x3 = 30
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 10)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 20)) &
                                     Word_To_Bytes (Encode_ADD (3, 1, 2));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);
      Execute_Instruction (State);
      Execute_Instruction (State);
      Print_Result ("ADD: 10 + 20 = 30", State.CPU.Regs (3) = 30);
      All_Passed := All_Passed and State.CPU.Regs (3) = 30;
   end Test_ADD;

   --  Test 3: SUB instruction
   procedure Test_SUB is
      --  addi x1, x0, 100 ; x1 = 100
      --  addi x2, x0, 37  ; x2 = 37
      --  sub x3, x1, x2   ; x3 = 63
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 100)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 37)) &
                                     Word_To_Bytes (Encode_SUB (3, 1, 2));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);
      Execute_Instruction (State);
      Execute_Instruction (State);
      Print_Result ("SUB: 100 - 37 = 63", State.CPU.Regs (3) = 63);
      All_Passed := All_Passed and State.CPU.Regs (3) = 63;
   end Test_SUB;

   --  Test 4: LUI instruction
   procedure Test_LUI is
      --  lui x1, 0x12345  ; x1 = 0x12345000
      Code  : constant Byte_Array := Word_To_Bytes (Encode_LUI (1, 16#12345000#));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);
      Print_Result ("LUI: x1 = 0x12345000", State.CPU.Regs (1) = 16#12345000#);
      All_Passed := All_Passed and State.CPU.Regs (1) = 16#12345000#;
   end Test_LUI;

   --  Test 5: x0 is always zero
   procedure Test_X0_Zero is
      --  addi x0, x0, 42  ; attempt to write to x0
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (0, 0, 42));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);
      Print_Result ("x0 is always zero", State.CPU.Regs (0) = 0);
      All_Passed := All_Passed and State.CPU.Regs (0) = 0;
   end Test_X0_Zero;

   --  Test 6: Gas metering
   procedure Test_Gas_Metering is
      --  Execute 3 instructions, check gas used
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 1)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 2)) &
                                     Word_To_Bytes (Encode_ADDI (3, 0, 3));
      State : Interpreter_State := Make_Test_State (Code, 1000);
      Initial_Gas : constant Gas_Amount := State.CPU.Gas_Used;
   begin
      Execute_Instruction (State);
      Execute_Instruction (State);
      Execute_Instruction (State);
      --  Each ALU instruction costs Gas_ALU_Basic (1)
      Print_Result ("Gas metering: 3 instructions", State.CPU.Gas_Used = Initial_Gas + 3);
      All_Passed := All_Passed and State.CPU.Gas_Used = Initial_Gas + 3;
   end Test_Gas_Metering;

   --  Test 7: Out of gas trap
   procedure Test_Out_Of_Gas is
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 1)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 2)) &
                                     Word_To_Bytes (Encode_ADDI (3, 0, 3));
      State : Interpreter_State := Make_Test_State (Code, 2);  -- Only 2 gas
   begin
      Run (State);
      Print_Result ("Out of gas trap", State.CPU.Trap = Trap_Out_Of_Gas);
      All_Passed := All_Passed and State.CPU.Trap = Trap_Out_Of_Gas;
   end Test_Out_Of_Gas;

   --  Test 8: Decode all instruction formats
   procedure Test_Decode is
      --  R-type: add x1, x2, x3
      R_Type : constant Word := Encode_ADD (1, 2, 3);
      R_Dec  : constant Decoded_Instruction := Decode (R_Type);

      --  I-type: addi x1, x2, 100
      I_Type : constant Word := Encode_ADDI (1, 2, 100);
      I_Dec  : constant Decoded_Instruction := Decode (I_Type);

      --  U-type: lui x1, 0x12345
      U_Type : constant Word := Encode_LUI (1, 16#12345000#);
      U_Dec  : constant Decoded_Instruction := Decode (U_Type);

      R_OK : constant Boolean := R_Dec.Valid and R_Dec.Format = Format_R and
                                 R_Dec.Rd = 1 and R_Dec.Rs1 = 2 and R_Dec.Rs2 = 3;
      I_OK : constant Boolean := I_Dec.Valid and I_Dec.Format = Format_I and
                                 I_Dec.Rd = 1 and I_Dec.Rs1 = 2 and I_Dec.Imm = 100;
      U_OK : constant Boolean := U_Dec.Valid and U_Dec.Format = Format_U and
                                 U_Dec.Rd = 1;
   begin
      Print_Result ("Decode R-type", R_OK);
      Print_Result ("Decode I-type", I_OK);
      Print_Result ("Decode U-type", U_OK);
      All_Passed := All_Passed and R_OK and I_OK and U_OK;
   end Test_Decode;

   --  Test 9: Memory load/store
   procedure Test_Memory is
      Mem     : Contract_Memory := (others => 0);
      Value   : Word;
      Success : Boolean;
   begin
      --  Store word
      Store_Word (Mem, 100, 16#DEADBEEF#, Success);
      if not Success then
         Print_Result ("Memory store word", False);
         All_Passed := False;
         return;
      end if;

      --  Load word
      Load_Word (Mem, 100, Value, Success);
      Print_Result ("Memory store/load word", Success and Value = 16#DEADBEEF#);
      All_Passed := All_Passed and Success and Value = 16#DEADBEEF#;
   end Test_Memory;

   --  Test 10: Signed arithmetic
   procedure Test_Signed is
      --  addi x1, x0, -10  ; x1 = -10 (0xFFFFFFF6)
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, -10));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);
      --  -10 in two's complement is 0xFFFFFFF6
      Print_Result ("Signed ADDI: x1 = -10", State.CPU.Regs (1) = 16#FFFF_FFF6#);
      All_Passed := All_Passed and State.CPU.Regs (1) = 16#FFFF_FFF6#;
   end Test_Signed;

   --  Test 11: JAL (jump and link)
   procedure Test_JAL is
      --  jal x1, 8         ; jump forward 8 bytes (2 instructions), link in x1
      --  addi x5, x0, 99   ; skip this (at PC+4) - use x5 which is init to 0
      --  addi x6, x0, 42   ; land here (at PC+8)
      --  Note: x2 is SP and initialized non-zero, so we use x5/x6 instead
      Code  : constant Byte_Array := Word_To_Bytes (Encode_JAL (1, 8)) &
                                     Word_To_Bytes (Encode_ADDI (5, 0, 99)) &
                                     Word_To_Bytes (Encode_ADDI (6, 0, 42));
      State : Interpreter_State := Make_Test_State (Code);
      Start_PC : constant Word := State.CPU.PC;
   begin
      Execute_Instruction (State);  -- JAL
      --  x1 should be return address (start + 4)
      --  PC should be start + 8
      Print_Result ("JAL: link address", State.CPU.Regs (1) = Start_PC + 4);
      All_Passed := All_Passed and State.CPU.Regs (1) = Start_PC + 4;

      Print_Result ("JAL: jumped to target", State.CPU.PC = Start_PC + 8);
      All_Passed := All_Passed and State.CPU.PC = Start_PC + 8;

      --  Now execute the instruction at target (should be addi x6, x0, 42)
      Execute_Instruction (State);
      --  Check x6 got set (we landed and executed correctly)
      Print_Result ("JAL: executed target", State.CPU.Regs (6) = 42);
      All_Passed := All_Passed and State.CPU.Regs (6) = 42;
      --  Check x5 was skipped (should still be 0)
      Print_Result ("JAL: skipped middle", State.CPU.Regs (5) = 0);
      All_Passed := All_Passed and State.CPU.Regs (5) = 0;
   end Test_JAL;

   --  Test 12: BEQ (branch if equal) - taken
   procedure Test_BEQ_Taken is
      --  addi x1, x0, 10   ; x1 = 10
      --  addi x2, x0, 10   ; x2 = 10
      --  beq x1, x2, 8     ; branch forward 8 bytes (taken because x1=x2)
      --  addi x3, x0, 99   ; skip this
      --  addi x4, x0, 42   ; land here
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 10)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 10)) &
                                     Word_To_Bytes (Encode_BEQ (1, 2, 8)) &
                                     Word_To_Bytes (Encode_ADDI (3, 0, 99)) &
                                     Word_To_Bytes (Encode_ADDI (4, 0, 42));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);  -- addi x1
      Execute_Instruction (State);  -- addi x2
      Execute_Instruction (State);  -- beq (should branch)
      Execute_Instruction (State);  -- addi x4 (landed here)
      Print_Result ("BEQ taken: skipped x3, got x4", State.CPU.Regs (3) = 0 and State.CPU.Regs (4) = 42);
      All_Passed := All_Passed and State.CPU.Regs (3) = 0 and State.CPU.Regs (4) = 42;
   end Test_BEQ_Taken;

   --  Test 13: BEQ (branch if equal) - not taken
   procedure Test_BEQ_Not_Taken is
      --  addi x1, x0, 10   ; x1 = 10
      --  addi x2, x0, 20   ; x2 = 20
      --  beq x1, x2, 8     ; branch forward (NOT taken because x1/=x2)
      --  addi x3, x0, 42   ; execute this
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 10)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 20)) &
                                     Word_To_Bytes (Encode_BEQ (1, 2, 8)) &
                                     Word_To_Bytes (Encode_ADDI (3, 0, 42));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);  -- addi x1
      Execute_Instruction (State);  -- addi x2
      Execute_Instruction (State);  -- beq (not taken)
      Execute_Instruction (State);  -- addi x3
      Print_Result ("BEQ not taken: executed x3", State.CPU.Regs (3) = 42);
      All_Passed := All_Passed and State.CPU.Regs (3) = 42;
   end Test_BEQ_Not_Taken;

   --  Test 14: BNE (branch if not equal)
   procedure Test_BNE is
      --  addi x1, x0, 10   ; x1 = 10
      --  addi x2, x0, 20   ; x2 = 20
      --  bne x1, x2, 8     ; branch forward (taken because x1/=x2)
      --  addi x3, x0, 99   ; skip this
      --  addi x4, x0, 42   ; land here
      Code  : constant Byte_Array := Word_To_Bytes (Encode_ADDI (1, 0, 10)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 20)) &
                                     Word_To_Bytes (Encode_BNE (1, 2, 8)) &
                                     Word_To_Bytes (Encode_ADDI (3, 0, 99)) &
                                     Word_To_Bytes (Encode_ADDI (4, 0, 42));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);  -- addi x1
      Execute_Instruction (State);  -- addi x2
      Execute_Instruction (State);  -- bne (taken)
      Execute_Instruction (State);  -- addi x4
      Print_Result ("BNE taken: skipped x3, got x4", State.CPU.Regs (3) = 0 and State.CPU.Regs (4) = 42);
      All_Passed := All_Passed and State.CPU.Regs (3) = 0 and State.CPU.Regs (4) = 42;
   end Test_BNE;

   --  Test 15: Load/Store word via interpreter
   procedure Test_LW_SW is
      --  addi x1, x0, 0x2100  ; address for store (in data segment)
      --  lui x1, 0x2000       ; set high bits
      --  addi x1, x1, 0x100   ; x1 = 0x2100
      --  addi x2, x0, 0x42    ; value = 0x42
      --  sw x2, 0(x1)         ; store x2 to mem[x1]
      --  lw x3, 0(x1)         ; load x3 from mem[x1]
      Code  : constant Byte_Array := Word_To_Bytes (Encode_LUI (1, 16#2000#)) &
                                     Word_To_Bytes (Encode_ADDI (1, 1, 16#100#)) &
                                     Word_To_Bytes (Encode_ADDI (2, 0, 16#42#)) &
                                     Word_To_Bytes (Encode_SW (1, 2, 0)) &
                                     Word_To_Bytes (Encode_LW (3, 1, 0));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);  -- lui x1
      Execute_Instruction (State);  -- addi x1
      Execute_Instruction (State);  -- addi x2
      Execute_Instruction (State);  -- sw
      Execute_Instruction (State);  -- lw
      Print_Result ("LW/SW: stored and loaded 0x42", State.CPU.Regs (3) = 16#42#);
      All_Passed := All_Passed and State.CPU.Regs (3) = 16#42#;
   end Test_LW_SW;

   --  Test 16: JALR (jump and link register)
   procedure Test_JALR is
      --  lui x1, 0x1000       ; load base address
      --  addi x1, x1, 16      ; x1 = 0x1010 (target: 4th instruction)
      --  jalr x2, x1, 0       ; jump to x1, link in x2
      --  addi x3, x0, 99      ; skip this
      --  addi x4, x0, 42      ; land here (at 0x1010)
      Code  : constant Byte_Array := Word_To_Bytes (Encode_LUI (1, 16#1000#)) &
                                     Word_To_Bytes (Encode_ADDI (1, 1, 16)) &
                                     Word_To_Bytes (Encode_JALR (2, 1, 0)) &
                                     Word_To_Bytes (Encode_ADDI (3, 0, 99)) &
                                     Word_To_Bytes (Encode_ADDI (4, 0, 42));
      State : Interpreter_State := Make_Test_State (Code);
   begin
      Execute_Instruction (State);  -- lui x1
      Execute_Instruction (State);  -- addi x1
      Execute_Instruction (State);  -- jalr
      --  PC should now be at 0x1010 (4th instruction)
      Print_Result ("JALR: jumped to register value", State.CPU.PC = 16#1010#);
      All_Passed := All_Passed and State.CPU.PC = 16#1010#;

      Execute_Instruction (State);  -- addi x4
      Print_Result ("JALR: skipped x3, got x4", State.CPU.Regs (3) = 0 and State.CPU.Regs (4) = 42);
      All_Passed := All_Passed and State.CPU.Regs (3) = 0 and State.CPU.Regs (4) = 42;
   end Test_JALR;

begin
   Put_Line ("============================================");
   Put_Line ("SPHINX RV32 Interpreter Test Suite");
   Put_Line ("============================================");
   Put_Line ("");

   Test_ADDI;
   Test_ADD;
   Test_SUB;
   Test_LUI;
   Test_X0_Zero;
   Test_Gas_Metering;
   Test_Out_Of_Gas;
   Test_Decode;
   Test_Memory;
   Test_Signed;
   Test_JAL;
   Test_BEQ_Taken;
   Test_BEQ_Not_Taken;
   Test_BNE;
   Test_LW_SW;
   Test_JALR;

   Put_Line ("");
   Put_Line ("============================================");
   if All_Passed then
      Put_Line ("All tests PASSED");
   else
      Put_Line ("Some tests FAILED");
   end if;
   Put_Line ("============================================");
end Sphinx_RV32_Test;
