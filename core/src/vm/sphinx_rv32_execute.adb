--  SPHINX RV32 Execute: Implementation

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Sphinx_RV32_Memory; use Sphinx_RV32_Memory;
with Sphinx_RV32_Syscall; use Sphinx_RV32_Syscall;

package body Sphinx_RV32_Execute with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Ghost Predicates for State Validity
   ---------------------------------------------------------------------------

   --  Check if registers are valid (x0 = 0)
   function Regs_Valid (R : Register_File) return Boolean is
      (R (0) = 0)
   with Ghost, Pure_Function;

   --  Check if CPU state maintains key invariants
   function CPU_Invariant (C : CPU_State) return Boolean is
      (Regs_Valid (C.Regs) and then
       C.Gas_Used <= C.Gas_Limit)
   with Ghost, Pure_Function;

   --  CPU invariant is maintained OR a trap was set
   function CPU_Invariant_Or_Trap (C : CPU_State) return Boolean is
      (CPU_Invariant (C) or else C.Trap /= Trap_None)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Two's Complement Conversion Helpers
   ---------------------------------------------------------------------------

   --  Half of maximum unsigned 32-bit value (2^31 - 1)
   Max_Positive : constant Word := 16#7FFF_FFFF#;

   --  Convert unsigned Word to signed SWord using two's complement
   --  This is provably safe because:
   --  - If W <= Max_Positive, W fits in SWord directly
   --  - If W > Max_Positive, the result is negative and within SWord range
   function To_Signed (W : Word) return SWord with
      SPARK_Mode => On,
      Post => (if W <= Max_Positive then To_Signed'Result = SWord (W)
               else To_Signed'Result < 0)
   is
      Result : SWord;
   begin
      if W <= Max_Positive then
         Result := SWord (W);
      else
         --  For values > 2^31-1, compute negative equivalent
         --  W in range [2^31, 2^32-1] maps to [-2^31, -1]
         --  We compute: -(2^32 - W) = -(not W + 1) = -(complement + 1)
         --  Since not W gives us the bit-flip, and +1 gives two's complement
         declare
            Complement : constant Word := not W;  -- Bit flip
            Negated    : constant Word := Complement + 1;  -- Two's complement
         begin
            --  Negated is in range [1, 2^31], which fits in SWord as negative
            --  Special case: when W = 2^31, Negated = 2^31, which is Integer_32'First
            if Negated <= Max_Positive then
               Result := -SWord (Negated);
            else
               --  This only happens when W = 2^31 (0x8000_0000)
               --  Negated = 2^31, so result is Integer_32'First = -2^31
               Result := SWord'First;  -- -2147483648
            end if;
         end;
      end if;
      return Result;
   end To_Signed;

   --  Convert signed SWord back to unsigned Word
   function To_Unsigned (S : SWord) return Word with
      SPARK_Mode => On
   is
   begin
      if S >= 0 then
         return Word (S);
      else
         --  For negative values, compute two's complement unsigned
         --  S in range [-2^31, -1] maps to [2^31, 2^32-1]
         if S = SWord'First then
            return 16#8000_0000#;  -- -2^31 -> 2^31
         else
            return Word (not Word (-(S + 1)));  -- Two's complement
         end if;
      end if;
   end To_Unsigned;

   ---------------------------------------------------------------------------
   --  Register File Operations
   ---------------------------------------------------------------------------

   function Read_Reg (
      Regs : Register_File;
      Idx  : Register_Index
   ) return Word is
   begin
      if Idx = 0 then
         return 0;  -- x0 is always zero
      else
         return Regs (Idx);
      end if;
   end Read_Reg;

   procedure Write_Reg (
      Regs  : in Out Register_File;
      Idx   : in     Register_Index;
      Value : in     Word
   ) is
   begin
      if Idx /= 0 then
         Regs (Idx) := Value;
      end if;
      --  Ensure x0 stays zero
      Regs (0) := 0;
   end Write_Reg;

   ---------------------------------------------------------------------------
   --  Gas Management
   ---------------------------------------------------------------------------

   procedure Charge_Gas (
      CPU    : in Out CPU_State;
      Amount : in     Gas_Amount
   ) is
   begin
      --  Check if we have enough gas remaining
      --  Use Has_Gas to ensure consistency with the postcondition
      if CPU.Gas_Used <= CPU.Gas_Limit and then
         CPU.Gas_Limit - CPU.Gas_Used >= Amount
      then
         CPU.Gas_Used := CPU.Gas_Used + Amount;
      else
         CPU.Trap := Trap_Out_Of_Gas;
         CPU.Halted := True;
      end if;
   end Charge_Gas;

   ---------------------------------------------------------------------------
   --  ALU Operations
   ---------------------------------------------------------------------------

   function ALU_Add (A, B : Word) return Word is
   begin
      return A + B;  -- Wrapping addition
   end ALU_Add;

   function ALU_Sub (A, B : Word) return Word is
   begin
      return A - B;  -- Wrapping subtraction
   end ALU_Sub;

   function ALU_And (A, B : Word) return Word is
   begin
      return A and B;
   end ALU_And;

   function ALU_Or (A, B : Word) return Word is
   begin
      return A or B;
   end ALU_Or;

   function ALU_Xor (A, B : Word) return Word is
   begin
      return A xor B;
   end ALU_Xor;

   --  ALU_SLT: Uses direct bit-level signed comparison for Platinum provability
   --  Sign bit determines negative: A >= 16#8000_0000# means A is negative
   function ALU_SLT (A, B : Word) return Word is
   begin
      if (A >= 16#8000_0000# and B < 16#8000_0000#) then
         --  A is negative, B is non-negative: A < B
         return 1;
      elsif (A < 16#8000_0000# and B >= 16#8000_0000#) then
         --  A is non-negative, B is negative: A >= B
         return 0;
      elsif A < B then
         --  Same sign: compare as unsigned
         return 1;
      else
         return 0;
      end if;
   end ALU_SLT;

   function ALU_SLTU (A, B : Word) return Word is
   begin
      if A < B then
         return 1;
      else
         return 0;
      end if;
   end ALU_SLTU;

   function ALU_SLL (A, B : Word) return Word is
      Shift_Amt : constant Natural := Natural (B and 16#1F#);  -- Lower 5 bits
   begin
      return Shift_Left (A, Shift_Amt);
   end ALU_SLL;

   function ALU_SRL (A, B : Word) return Word is
      Shift_Amt : constant Natural := Natural (B and 16#1F#);
   begin
      return Shift_Right (A, Shift_Amt);
   end ALU_SRL;

   --  ALU_SRA: Uses built-in Shift_Right_Arithmetic for Platinum provability
   function ALU_SRA (A, B : Word) return Word is
      Shift_Amt : constant Natural := Natural (B and 16#1F#);
   begin
      return Shift_Right_Arithmetic (A, Shift_Amt);
   end ALU_SRA;

   ---------------------------------------------------------------------------
   --  M Extension Operations
   ---------------------------------------------------------------------------

   function ALU_MUL (A, B : Word) return Word is
      Prod : constant Unsigned_64 := Unsigned_64 (A) * Unsigned_64 (B);
   begin
      return Word (Prod and 16#FFFF_FFFF#);
   end ALU_MUL;

   --  Helper: Get absolute value of a signed 32-bit number as Unsigned_64
   --  Handles the MIN_INT case where -MIN_INT would overflow
   function Abs_As_U64 (W : Word) return Unsigned_64 is
   begin
      if W < 16#8000_0000# then
         --  Positive value, return as-is
         return Unsigned_64 (W);
      elsif W = 16#8000_0000# then
         --  MIN_INT = -2^31, absolute value = 2^31
         return 16#8000_0000#;
      else
         --  Negative value: 2^32 - W = |value|
         return Unsigned_64 (16#1_0000_0000# - Unsigned_64 (W));
      end if;
   end Abs_As_U64;

   --  ALU_MULH: Uses direct 64-bit signed multiplication for Platinum provability
   --  Converts to signed 64-bit, multiplies, converts product to unsigned, takes high bits
   function ALU_MULH (A, B : Word) return Word is
      --  Convert to signed 64-bit using same logic as To_S64
      SA : constant Integer_64 :=
         (if A < 16#8000_0000# then Integer_64 (A)
          else Integer_64 (A) - 16#1_0000_0000#);
      SB : constant Integer_64 :=
         (if B < 16#8000_0000# then Integer_64 (B)
          else Integer_64 (B) - 16#1_0000_0000#);
      --  Product fits in 64-bit signed: max |product| ≈ 2^62
      Prod : constant Integer_64 := SA * SB;
      --  Convert to unsigned for bit extraction using modular conversion
      --  This correctly handles negative values (two's complement)
      UProd : constant Unsigned_64 := Unsigned_64'Mod (Prod);
   begin
      --  Return upper 32 bits
      return Word (Shift_Right (UProd, 32) and 16#FFFF_FFFF#);
   end ALU_MULH;

   function ALU_MULHU (A, B : Word) return Word is
      Prod : constant Unsigned_64 := Unsigned_64 (A) * Unsigned_64 (B);
   begin
      return Word (Prod / 16#1_0000_0000#);
   end ALU_MULHU;

   --  ALU_MULHSU: Uses direct 64-bit signed multiplication for Platinum provability
   --  A is sign-extended, B is zero-extended (as signed), multiply, take high bits
   function ALU_MULHSU (A, B : Word) return Word is
      --  Sign-extend A to 64-bit signed using same logic as To_S64
      SA : constant Integer_64 :=
         (if A < 16#8000_0000# then Integer_64 (A)
          else Integer_64 (A) - 16#1_0000_0000#);
      --  Zero-extend B to 64-bit (fits in signed since B < 2^32 < 2^63)
      SB : constant Integer_64 := Integer_64 (B);
      --  Product as signed 64-bit
      Prod : constant Integer_64 := SA * SB;
      --  Convert to unsigned for bit extraction using modular conversion
      --  This correctly handles negative values (two's complement)
      UProd : constant Unsigned_64 := Unsigned_64'Mod (Prod);
   begin
      --  Return upper 32 bits
      return Word (Shift_Right (UProd, 32) and 16#FFFF_FFFF#);
   end ALU_MULHSU;

   --  ALU_DIV: Uses 64-bit signed arithmetic for Platinum provability
   function ALU_DIV (A, B : Word) return Word is
      --  Sign-extend to 64-bit using same logic as To_S64
      SA : constant Integer_64 :=
         (if A < 16#8000_0000# then Integer_64 (A)
          else Integer_64 (A) - 16#1_0000_0000#);
      SB : constant Integer_64 :=
         (if B < 16#8000_0000# then Integer_64 (B)
          else Integer_64 (B) - 16#1_0000_0000#);
      Result : Integer_64;
   begin
      --  Division by zero: return -1
      if B = 0 then
         return Word'Last;  -- -1 in two's complement
      end if;

      --  Overflow case: MIN_INT / -1 = MIN_INT (can't represent in 32-bit signed)
      if A = 16#8000_0000# and B = Word'Last then
         return 16#8000_0000#;  -- MIN_INT in two's complement
      end if;

      --  Normal signed division
      Result := SA / SB;
      --  Convert to unsigned 32-bit via modular truncation
      return Word (Unsigned_32'Mod (Result));
   end ALU_DIV;

   function ALU_DIVU (A, B : Word) return Word is
   begin
      if B = 0 then
         return Word'Last;  -- Maximum unsigned for div by zero
      end if;
      return A / B;
   end ALU_DIVU;

   --  ALU_REM: Uses 64-bit signed arithmetic for Platinum provability
   function ALU_REM (A, B : Word) return Word is
      --  Sign-extend to 64-bit using same logic as To_S64
      SA : constant Integer_64 :=
         (if A < 16#8000_0000# then Integer_64 (A)
          else Integer_64 (A) - 16#1_0000_0000#);
      SB : constant Integer_64 :=
         (if B < 16#8000_0000# then Integer_64 (B)
          else Integer_64 (B) - 16#1_0000_0000#);
      Result : Integer_64;
   begin
      --  Remainder with zero divisor: return dividend
      if B = 0 then
         return A;
      end if;

      --  Overflow case: MIN_INT % -1 = 0
      if A = 16#8000_0000# and B = Word'Last then
         return 0;
      end if;

      --  Normal signed remainder
      Result := SA rem SB;
      --  Convert to unsigned 32-bit via modular truncation
      return Word (Unsigned_32'Mod (Result));
   end ALU_REM;

   function ALU_REMU (A, B : Word) return Word is
   begin
      if B = 0 then
         return A;  -- Return dividend for div by zero
      end if;
      return A rem B;
   end ALU_REMU;

   ---------------------------------------------------------------------------
   --  Branch Condition Evaluation
   ---------------------------------------------------------------------------

   function Branch_Taken (
      Rs1_Val : Word;
      Rs2_Val : Word;
      F3      : Funct3
   ) return Boolean is
      S1 : constant SWord := To_Signed (Rs1_Val);
      S2 : constant SWord := To_Signed (Rs2_Val);
   begin
      case F3 is
         when F3_BEQ  => return Rs1_Val = Rs2_Val;
         when F3_BNE  => return Rs1_Val /= Rs2_Val;
         when F3_BLT  => return S1 < S2;
         when F3_BGE  => return S1 >= S2;
         when F3_BLTU => return Rs1_Val < Rs2_Val;
         when F3_BGEU => return Rs1_Val >= Rs2_Val;
         when others  => return False;
      end case;
   end Branch_Taken;

   ---------------------------------------------------------------------------
   --  Forward Declarations with Postconditions
   --  These help the prover understand what properties are maintained
   ---------------------------------------------------------------------------

   procedure Execute_R_Type (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              (CPU.Trap /= Trap_None or CPU.PC mod 4 = 0);

   procedure Execute_I_Type_ALU (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              (CPU.Trap /= Trap_None or CPU.PC mod 4 = 0);

   procedure Execute_Load (
      State : in Out Interpreter_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => State.CPU.Regs (0) = 0 and State.CPU.PC mod 4 = 0,
      Post => State.CPU.Regs (0) = 0 and then
              State.CPU.Gas_Used = State'Old.CPU.Gas_Used and then
              State.CPU.Gas_Limit = State'Old.CPU.Gas_Limit and then
              State.CPU.Instruction_Count = State'Old.CPU.Instruction_Count and then
              (State.CPU.Trap /= Trap_None or State.CPU.PC mod 4 = 0);

   procedure Execute_Store (
      State : in Out Interpreter_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => State.CPU.Regs (0) = 0 and State.CPU.PC mod 4 = 0,
      Post => State.CPU.Regs (0) = 0 and then
              State.CPU.Gas_Used = State'Old.CPU.Gas_Used and then
              State.CPU.Gas_Limit = State'Old.CPU.Gas_Limit and then
              State.CPU.Instruction_Count = State'Old.CPU.Instruction_Count and then
              (State.CPU.Trap /= Trap_None or State.CPU.PC mod 4 = 0);

   procedure Execute_Branch (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              (CPU.Trap /= Trap_None or CPU.PC mod 4 = 0);

   procedure Execute_JAL (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              (CPU.Trap /= Trap_None or CPU.PC mod 4 = 0);

   procedure Execute_JALR (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              (CPU.Trap /= Trap_None or CPU.PC mod 4 = 0);

   procedure Execute_LUI (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              CPU.PC mod 4 = 0;  -- LUI always succeeds

   procedure Execute_AUIPC (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              CPU.PC mod 4 = 0;  -- AUIPC always succeeds

   procedure Execute_System (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              CPU.PC = CPU'Old.PC;  -- System calls don't advance PC

   procedure Execute_Fence (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) with
      Pre  => CPU.Regs (0) = 0 and CPU.PC mod 4 = 0,
      Post => CPU.Regs (0) = 0 and then
              CPU.Gas_Used = CPU'Old.Gas_Used and then
              CPU.Gas_Limit = CPU'Old.Gas_Limit and then
              CPU.Instruction_Count = CPU'Old.Instruction_Count and then
              CPU.PC mod 4 = 0;  -- FENCE always succeeds

   ---------------------------------------------------------------------------
   --  Execute R-Type Instructions
   ---------------------------------------------------------------------------

   procedure Execute_R_Type (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
      Rs1_Val : constant Word := Read_Reg (CPU.Regs, Instr.Rs1);
      Rs2_Val : constant Word := Read_Reg (CPU.Regs, Instr.Rs2);
      Result  : Word := 0;
   begin
      --  Check for M extension (multiply/divide)
      if Instr.Funct7 = F7_MULDIV then
         case Instr.Funct3 is
            when F3_MUL    => Result := ALU_MUL (Rs1_Val, Rs2_Val);
            when F3_MULH   => Result := ALU_MULH (Rs1_Val, Rs2_Val);
            when F3_MULHSU => Result := ALU_MULHSU (Rs1_Val, Rs2_Val);
            when F3_MULHU  => Result := ALU_MULHU (Rs1_Val, Rs2_Val);
            when F3_DIV    => Result := ALU_DIV (Rs1_Val, Rs2_Val);
            when F3_DIVU   => Result := ALU_DIVU (Rs1_Val, Rs2_Val);
            when F3_REM    => Result := ALU_REM (Rs1_Val, Rs2_Val);
            when F3_REMU   => Result := ALU_REMU (Rs1_Val, Rs2_Val);
            when others    => CPU.Trap := Trap_Illegal_Instruction;
         end case;
      else
         --  Standard R-type instructions
         case Instr.Funct3 is
            when F3_ADD_SUB =>
               if Instr.Funct7 = F7_ZERO then
                  Result := ALU_Add (Rs1_Val, Rs2_Val);
               elsif Instr.Funct7 = F7_ALT then
                  Result := ALU_Sub (Rs1_Val, Rs2_Val);
               else
                  CPU.Trap := Trap_Illegal_Instruction;
               end if;

            when F3_SLL =>
               Result := ALU_SLL (Rs1_Val, Rs2_Val);

            when F3_SLT =>
               Result := ALU_SLT (Rs1_Val, Rs2_Val);

            when F3_SLTU =>
               Result := ALU_SLTU (Rs1_Val, Rs2_Val);

            when F3_XOR =>
               Result := ALU_Xor (Rs1_Val, Rs2_Val);

            when F3_SRL_SRA =>
               if Instr.Funct7 = F7_ZERO then
                  Result := ALU_SRL (Rs1_Val, Rs2_Val);
               elsif Instr.Funct7 = F7_ALT then
                  Result := ALU_SRA (Rs1_Val, Rs2_Val);
               else
                  CPU.Trap := Trap_Illegal_Instruction;
               end if;

            when F3_OR =>
               Result := ALU_Or (Rs1_Val, Rs2_Val);

            when F3_AND =>
               Result := ALU_And (Rs1_Val, Rs2_Val);

            when others =>
               CPU.Trap := Trap_Illegal_Instruction;
         end case;
      end if;

      if CPU.Trap = Trap_None then
         Write_Reg (CPU.Regs, Instr.Rd, Result);
         CPU.PC := CPU.PC + 4;
      end if;
   end Execute_R_Type;

   ---------------------------------------------------------------------------
   --  Execute I-Type Instructions (ALU immediate)
   ---------------------------------------------------------------------------

   --  Helper to convert signed immediate to Word (two's complement)
   function Signed_To_Word (S : Integer_32) return Word is
   begin
      if S >= 0 then
         return Word (S);
      elsif S = Integer_32'First then
         --  Special case: MIN_INT (-2147483648) maps to 0x80000000
         return 16#8000_0000#;
      else
         --  Two's complement: -N = 2^32 - N = 0xFFFFFFFF - N + 1
         return Word (Unsigned_32'Last) - Word (-(S + 1));
      end if;
   end Signed_To_Word;

   procedure Execute_I_Type_ALU (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
      Rs1_Val : constant Word := Read_Reg (CPU.Regs, Instr.Rs1);
      Imm_Val : constant Word := Signed_To_Word (Instr.Imm);
      Result  : Word := 0;
      Shamt   : Natural;
   begin
      case Instr.Funct3 is
         when F3_ADDI =>
            Result := ALU_Add (Rs1_Val, Imm_Val);

         when F3_SLTI =>
            Result := ALU_SLT (Rs1_Val, Imm_Val);

         when F3_SLTIU =>
            Result := ALU_SLTU (Rs1_Val, Imm_Val);

         when F3_XORI =>
            Result := ALU_Xor (Rs1_Val, Imm_Val);

         when F3_ORI =>
            Result := ALU_Or (Rs1_Val, Imm_Val);

         when F3_ANDI =>
            Result := ALU_And (Rs1_Val, Imm_Val);

         when F3_SLLI =>
            Shamt := Natural (Imm_Val and 16#1F#);
            Result := ALU_SLL (Rs1_Val, Word (Shamt));

         when F3_SRLI_SRAI =>
            Shamt := Natural (Imm_Val and 16#1F#);
            if (Instr.Funct7 and 16#20#) = 0 then
               Result := ALU_SRL (Rs1_Val, Word (Shamt));
            else
               Result := ALU_SRA (Rs1_Val, Word (Shamt));
            end if;

         when others =>
            CPU.Trap := Trap_Illegal_Instruction;
      end case;

      if CPU.Trap = Trap_None then
         Write_Reg (CPU.Regs, Instr.Rd, Result);
         CPU.PC := CPU.PC + 4;
      end if;
   end Execute_I_Type_ALU;

   ---------------------------------------------------------------------------
   --  Execute Load Instructions
   ---------------------------------------------------------------------------

   procedure Execute_Load (
      State : in Out Interpreter_State;
      Instr : in     Decoded_Instruction
   ) is
      Rs1_Val : constant Word := Read_Reg (State.CPU.Regs, Instr.Rs1);
      Addr    : constant Word := Rs1_Val + Signed_To_Word (Instr.Imm);
      Value   : Word := 0;  -- Initialize to prevent uninitialized use
      SValue  : SWord;
      Success : Boolean := False;  -- Initialize for safety
   begin
      case Instr.Funct3 is
         when F3_LB =>
            Load_Byte_Signed (State.Memory, Addr, SValue, Success);
            if Success then
               Value := To_Unsigned (SValue);
            end if;

         when F3_LH =>
            Load_Half_Signed (State.Memory, Addr, SValue, Success);
            if Success then
               Value := To_Unsigned (SValue);
            else
               State.CPU.Trap := Trap_Load_Misaligned;
            end if;

         when F3_LW =>
            Load_Word (State.Memory, Addr, Value, Success);
            if not Success then
               State.CPU.Trap := Trap_Load_Misaligned;
            end if;

         when F3_LBU =>
            Load_Byte (State.Memory, Addr, Value, Success);

         when F3_LHU =>
            Load_Half (State.Memory, Addr, Value, Success);
            if not Success then
               State.CPU.Trap := Trap_Load_Misaligned;
            end if;

         when others =>
            State.CPU.Trap := Trap_Illegal_Instruction;
            Success := False;
      end case;

      if State.CPU.Trap = Trap_None then
         if Success then
            Write_Reg (State.CPU.Regs, Instr.Rd, Value);
            State.CPU.PC := State.CPU.PC + 4;
         else
            State.CPU.Trap := Trap_Load_Access_Fault;
            State.CPU.Trap_Value := Addr;
         end if;
      end if;
   end Execute_Load;

   ---------------------------------------------------------------------------
   --  Execute Store Instructions
   ---------------------------------------------------------------------------

   procedure Execute_Store (
      State : in Out Interpreter_State;
      Instr : in     Decoded_Instruction
   ) is
      Rs1_Val : constant Word := Read_Reg (State.CPU.Regs, Instr.Rs1);
      Rs2_Val : constant Word := Read_Reg (State.CPU.Regs, Instr.Rs2);
      Addr    : constant Word := Rs1_Val + Signed_To_Word (Instr.Imm);
      Success : Boolean;
   begin
      case Instr.Funct3 is
         when F3_SB =>
            Store_Byte (State.Memory, Addr, Rs2_Val, Success);

         when F3_SH =>
            Store_Half (State.Memory, Addr, Rs2_Val, Success);
            if not Success then
               State.CPU.Trap := Trap_Store_Misaligned;
            end if;

         when F3_SW =>
            Store_Word (State.Memory, Addr, Rs2_Val, Success);
            if not Success then
               State.CPU.Trap := Trap_Store_Misaligned;
            end if;

         when others =>
            State.CPU.Trap := Trap_Illegal_Instruction;
            Success := False;
      end case;

      if State.CPU.Trap = Trap_None then
         if Success then
            State.CPU.PC := State.CPU.PC + 4;
         else
            State.CPU.Trap := Trap_Store_Access_Fault;
            State.CPU.Trap_Value := Addr;
         end if;
      end if;
   end Execute_Store;

   ---------------------------------------------------------------------------
   --  Execute Branch Instructions
   ---------------------------------------------------------------------------

   procedure Execute_Branch (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
      Rs1_Val : constant Word := Read_Reg (CPU.Regs, Instr.Rs1);
      Rs2_Val : constant Word := Read_Reg (CPU.Regs, Instr.Rs2);
      Target  : Word;
   begin
      if Branch_Taken (Rs1_Val, Rs2_Val, Instr.Funct3) then
         Target := CPU.PC + Signed_To_Word (Instr.Imm);
         --  Check alignment
         if Target mod 4 /= 0 then
            CPU.Trap := Trap_Misaligned_Fetch;
            CPU.Trap_Value := Target;
         else
            CPU.PC := Target;
         end if;
      else
         CPU.PC := CPU.PC + 4;
      end if;
   end Execute_Branch;

   ---------------------------------------------------------------------------
   --  Execute JAL (Jump and Link)
   ---------------------------------------------------------------------------

   procedure Execute_JAL (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
      Return_Addr : constant Word := CPU.PC + 4;
      Target      : constant Word := CPU.PC + Signed_To_Word (Instr.Imm);
   begin
      if Target mod 4 /= 0 then
         CPU.Trap := Trap_Misaligned_Fetch;
         CPU.Trap_Value := Target;
      else
         Write_Reg (CPU.Regs, Instr.Rd, Return_Addr);
         CPU.PC := Target;
      end if;
   end Execute_JAL;

   ---------------------------------------------------------------------------
   --  Execute JALR (Jump and Link Register)
   ---------------------------------------------------------------------------

   procedure Execute_JALR (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
      Rs1_Val     : constant Word := Read_Reg (CPU.Regs, Instr.Rs1);
      Return_Addr : constant Word := CPU.PC + 4;
      Target      : Word;
   begin
      --  Target = (rs1 + imm) with LSB cleared
      Target := (Rs1_Val + Signed_To_Word (Instr.Imm)) and 16#FFFF_FFFE#;

      if Target mod 4 /= 0 then
         CPU.Trap := Trap_Misaligned_Fetch;
         CPU.Trap_Value := Target;
      else
         Write_Reg (CPU.Regs, Instr.Rd, Return_Addr);
         CPU.PC := Target;
      end if;
   end Execute_JALR;

   ---------------------------------------------------------------------------
   --  Execute LUI (Load Upper Immediate)
   ---------------------------------------------------------------------------

   procedure Execute_LUI (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
   begin
      Write_Reg (CPU.Regs, Instr.Rd, Signed_To_Word (Instr.Imm));
      CPU.PC := CPU.PC + 4;
   end Execute_LUI;

   ---------------------------------------------------------------------------
   --  Execute AUIPC (Add Upper Immediate to PC)
   ---------------------------------------------------------------------------

   procedure Execute_AUIPC (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
      Result : constant Word := CPU.PC + Signed_To_Word (Instr.Imm);
   begin
      Write_Reg (CPU.Regs, Instr.Rd, Result);
      CPU.PC := CPU.PC + 4;
   end Execute_AUIPC;

   ---------------------------------------------------------------------------
   --  Execute SYSTEM Instructions (ECALL, EBREAK)
   ---------------------------------------------------------------------------

   procedure Execute_System (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
   begin
      --  ECALL: imm = 0, EBREAK: imm = 1
      if Instr.Imm = 0 then
         --  ECALL - system call
         CPU.Trap := Trap_Ecall;
         --  Don't advance PC; syscall handler will do that
      elsif Instr.Imm = 1 then
         --  EBREAK - breakpoint
         CPU.Trap := Trap_Breakpoint;
         CPU.Halted := True;
      else
         CPU.Trap := Trap_Illegal_Instruction;
      end if;
   end Execute_System;

   ---------------------------------------------------------------------------
   --  Execute FENCE Instructions
   ---------------------------------------------------------------------------

   procedure Execute_Fence (
      CPU   : in Out CPU_State;
      Instr : in     Decoded_Instruction
   ) is
      pragma Unreferenced (Instr);
   begin
      --  FENCE is a no-op in our interpreter (single-threaded, no caches)
      CPU.PC := CPU.PC + 4;
   end Execute_Fence;

   ---------------------------------------------------------------------------
   --  Single Instruction Execution
   ---------------------------------------------------------------------------

   procedure Execute_Instruction (
      State : in Out Interpreter_State
   ) is
      --  Ghost variable to track initial instruction count
      Initial_Count : constant Natural := State.CPU.Instruction_Count with Ghost;

      Instr_Word : Word;
      Instr      : Decoded_Instruction;
      Category   : Instruction_Category;
      Gas_Cost   : Gas_Amount;
      Fetch_OK   : Boolean;
   begin
      --  Fetch instruction
      Fetch_Instruction (
         State.Memory,
         State.CPU.PC,
         State.Bounds,
         Instr_Word,
         Fetch_OK
      );

      if not Fetch_OK then
         State.CPU.Trap := Trap_Misaligned_Fetch;
         State.CPU.Trap_Value := State.CPU.PC;
         State.CPU.Halted := True;
         pragma Assert (State.CPU.Trap /= Trap_None);
         pragma Assert (State.CPU.Instruction_Count = Initial_Count);
         return;
      end if;

      --  Decode instruction
      Instr := Decode (Instr_Word);

      if not Instr.Valid then
         State.CPU.Trap := Trap_Illegal_Instruction;
         State.CPU.Trap_Value := Instr_Word;
         State.CPU.Halted := True;
         pragma Assert (State.CPU.Trap /= Trap_None);
         pragma Assert (State.CPU.Instruction_Count = Initial_Count);
         return;
      end if;

      --  Calculate and charge gas
      Category := Get_Category (Instr);
      Gas_Cost := Get_Instruction_Gas (Category);

      --  Check for potential gas overflow (defensive check for prover)
      if Gas_Cost > Gas_Amount'Last - State.CPU.Gas_Used then
         State.CPU.Trap := Trap_Out_Of_Gas;
         State.CPU.Halted := True;
         pragma Assert (State.CPU.Trap /= Trap_None);
         pragma Assert (State.CPU.Instruction_Count = Initial_Count);
         return;
      end if;

      Charge_Gas (State.CPU, Gas_Cost);

      if State.CPU.Trap /= Trap_None then
         pragma Assert (State.CPU.Trap /= Trap_None);
         pragma Assert (State.CPU.Instruction_Count = Initial_Count);
         return;  -- Out of gas
      end if;

      pragma Assert (State.CPU.Instruction_Count = Initial_Count);

      --  Execute instruction based on opcode
      case Instr.Opcode is
         when OP_LUI =>
            Execute_LUI (State.CPU, Instr);

         when OP_AUIPC =>
            Execute_AUIPC (State.CPU, Instr);

         when OP_JAL =>
            Execute_JAL (State.CPU, Instr);

         when OP_JALR =>
            Execute_JALR (State.CPU, Instr);

         when OP_BRANCH =>
            Execute_Branch (State.CPU, Instr);

         when OP_LOAD =>
            Execute_Load (State, Instr);

         when OP_STORE =>
            Execute_Store (State, Instr);

         when OP_IMM =>
            Execute_I_Type_ALU (State.CPU, Instr);

         when OP_REG =>
            Execute_R_Type (State.CPU, Instr);

         when OP_FENCE =>
            Execute_Fence (State.CPU, Instr);

         when OP_SYSTEM =>
            Execute_System (State.CPU, Instr);

         when others =>
            State.CPU.Trap := Trap_Illegal_Instruction;
            State.CPU.Halted := True;
      end case;

      --  At this point, instruction count hasn't changed yet
      pragma Assert (State.CPU.Instruction_Count = Initial_Count);

      --  State validity assertions for postcondition proof:
      --  1. Regs(0) is always zero (Write_Reg maintains this)
      pragma Assert (State.CPU.Regs (0) = 0);
      --  2. Gas_Used <= Gas_Limit (Charge_Gas maintains this or sets trap)
      pragma Assert (State.CPU.Trap /= Trap_None or
                     State.CPU.Gas_Used <= State.CPU.Gas_Limit);
      --  3. PC is aligned when no trap (branch/jump set trap if misaligned,
      --     other instructions add 4 which preserves alignment)
      pragma Assert (State.CPU.Trap /= Trap_None or
                     State.CPU.PC mod 4 = 0);

      --  Increment instruction count
      if State.CPU.Instruction_Count < Natural'Last then
         State.CPU.Instruction_Count := State.CPU.Instruction_Count + 1;
         pragma Assert (State.CPU.Instruction_Count = Initial_Count + 1);
      end if;

      --  Final assertion: either count increased or there's a trap
      pragma Assert (State.CPU.Instruction_Count >= Initial_Count);
      pragma Assert (State.CPU.Trap /= Trap_None or
                     State.CPU.Instruction_Count = Initial_Count + 1);

      --  Halt on terminal trap
      if State.CPU.Trap in Trap_Return | Trap_Revert | Trap_Halt |
                           Trap_Out_Of_Gas | Trap_Illegal_Instruction |
                           Trap_Load_Access_Fault | Trap_Store_Access_Fault
      then
         State.CPU.Halted := True;
      end if;

      --  Final state validity assertion (combines the above)
      pragma Assert (State.CPU.Trap /= Trap_None or State.CPU.Halted or
                     Interpreter_State_Valid (State));
   end Execute_Instruction;

   ---------------------------------------------------------------------------
   --  Main Execution Loop
   ---------------------------------------------------------------------------

   procedure Run (
      State     : in Out Interpreter_State;
      Max_Steps : in     Natural := Natural'Last
   ) is
      Steps : Natural := 0;
   begin
      while not State.CPU.Halted and then
            Steps < Max_Steps and then
            State.CPU.Instruction_Count < Natural'Last
      loop
         pragma Loop_Invariant (Steps < Max_Steps);
         pragma Loop_Invariant (not State.CPU.Halted);
         pragma Loop_Invariant (State.CPU.Instruction_Count < Natural'Last);
         pragma Loop_Variant (Decreases => Max_Steps - Steps);

         --  Handle pending syscall (ECALL) if any
         if State.CPU.Trap = Trap_Ecall then
            Handle_Syscall (State);
            --  If syscall set a terminal trap or halted, exit
            if State.CPU.Halted or else
               State.CPU.Trap in Trap_Return | Trap_Revert | Trap_Out_Of_Gas |
                                 Trap_Syscall_Error
            then
               exit;
            end if;
         elsif State.CPU.Trap /= Trap_None then
            --  Non-ECALL trap - exit loop
            exit;
         end if;

         --  Execute next instruction (only if no pending trap)
         if State.CPU.Trap = Trap_None and then not State.CPU.Halted then
            Execute_Instruction (State);
         end if;

         Steps := Steps + 1;
      end loop;

      --  If we exited without halt or trap, set halt
      --  (either hit max steps or instruction count limit)
      if not State.CPU.Halted and State.CPU.Trap = Trap_None then
         State.CPU.Halted := True;
      end if;
   end Run;

   ---------------------------------------------------------------------------
   --  Convenience Entry Point
   ---------------------------------------------------------------------------

   procedure Execute_Contract (
      Initial_State : in     Interpreter_State;
      Max_Steps     : in     Natural := Natural'Last;
      Result        : out    Interpreter_Result
   )
   is
      State : Interpreter_State := Initial_State;
   begin
      Run (State, Max_Steps);

      Result.Final_State   := State.CPU;
      Result.Trap          := State.CPU.Trap;
      Result.Gas_Used      := State.CPU.Gas_Used;
      Result.Success       := State.CPU.Trap in Trap_None | Trap_Return;
      Result.Return_Data   := Hash256_Zero;  -- Set by syscall handler
      Result.Return_Length := 0;
   end Execute_Contract;

end Sphinx_RV32_Execute;
