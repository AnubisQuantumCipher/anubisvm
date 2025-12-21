--  SPHINX RV32 Loader: Implementation

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Sphinx_RV32_Memory; use Sphinx_RV32_Memory;

package body Sphinx_RV32_Loader with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Little-Endian Reading Helpers
   ---------------------------------------------------------------------------

   function Read_LE16 (Data : Byte_Array; Offset : Natural) return Half is
      Base : constant Natural := Data'First + Offset;
   begin
      return Half (Data (Base)) or
             (Half (Data (Base + 1)) * 256);
   end Read_LE16;

   function Read_LE32 (Data : Byte_Array; Offset : Natural) return Word is
      Base : constant Natural := Data'First + Offset;
   begin
      return Word (Data (Base)) or
             (Word (Data (Base + 1)) * 256) or
             (Word (Data (Base + 2)) * 65536) or
             (Word (Data (Base + 3)) * 16777216);
   end Read_LE32;

   ---------------------------------------------------------------------------
   --  Header Parsing
   ---------------------------------------------------------------------------

   function Parse_ELF_Header (
      Data : Byte_Array
   ) return ELF32_Header
   is
      H : ELF32_Header;
   begin
      H.E_Ident_Magic   := Read_LE32 (Data, 0);
      H.E_Ident_Class   := Data (Data'First + 4);
      H.E_Ident_Data    := Data (Data'First + 5);
      H.E_Ident_Version := Data (Data'First + 6);
      H.E_Ident_OSABI   := Data (Data'First + 7);
      H.E_Ident_Pad     := 0;  -- Not used

      H.E_Type      := Read_LE16 (Data, 16);
      H.E_Machine   := Read_LE16 (Data, 18);
      H.E_Version   := Read_LE32 (Data, 20);
      H.E_Entry     := Read_LE32 (Data, 24);
      H.E_Phoff     := Read_LE32 (Data, 28);
      H.E_Shoff     := Read_LE32 (Data, 32);
      H.E_Flags     := Read_LE32 (Data, 36);
      H.E_Ehsize    := Read_LE16 (Data, 40);
      H.E_Phentsize := Read_LE16 (Data, 42);
      H.E_Phnum     := Read_LE16 (Data, 44);
      H.E_Shentsize := Read_LE16 (Data, 46);
      H.E_Shnum     := Read_LE16 (Data, 48);
      H.E_Shstrndx  := Read_LE16 (Data, 50);

      return H;
   end Parse_ELF_Header;

   function Parse_Phdr (
      Data   : Byte_Array;
      Offset : Natural
   ) return ELF32_Phdr
   is
      P : ELF32_Phdr;
   begin
      P.P_Type   := Read_LE32 (Data, Offset);
      P.P_Offset := Read_LE32 (Data, Offset + 4);
      P.P_Vaddr  := Read_LE32 (Data, Offset + 8);
      P.P_Paddr  := Read_LE32 (Data, Offset + 12);
      P.P_Filesz := Read_LE32 (Data, Offset + 16);
      P.P_Memsz  := Read_LE32 (Data, Offset + 20);
      P.P_Flags  := Read_LE32 (Data, Offset + 24);
      P.P_Align  := Read_LE32 (Data, Offset + 28);

      return P;
   end Parse_Phdr;

   ---------------------------------------------------------------------------
   --  ELF Validation
   ---------------------------------------------------------------------------

   function Is_Valid_ELF (Data : Byte_Array) return Boolean is
   begin
      if Data'Length < ELF32_Header_Size then
         return False;
      end if;
      return Read_LE32 (Data, 0) = ELF_MAGIC;
   end Is_Valid_ELF;

   function Validate_ELF_Header (
      Data : Byte_Array
   ) return Load_Status
   is
      H : constant ELF32_Header := Parse_ELF_Header (Data);
   begin
      --  Check magic number
      if H.E_Ident_Magic /= ELF_MAGIC then
         return Load_Invalid_Magic;
      end if;

      --  Check 32-bit class
      if H.E_Ident_Class /= ELFCLASS32 then
         return Load_Invalid_Class;
      end if;

      --  Check little-endian
      if H.E_Ident_Data /= ELFDATA2LSB then
         return Load_Invalid_Endian;
      end if;

      --  Check RISC-V machine type
      if H.E_Machine /= EM_RISCV then
         return Load_Invalid_Machine;
      end if;

      --  Check executable type (allow both ET_EXEC and ET_DYN)
      if H.E_Type /= ET_EXEC and H.E_Type /= ET_DYN then
         return Load_Invalid_Type;
      end if;

      --  Check entry point alignment
      if H.E_Entry mod 4 /= 0 then
         return Load_Invalid_Entry;
      end if;

      --  Check program header offset is valid
      if Natural (H.E_Phoff) + Natural (H.E_Phnum) * ELF32_Phdr_Size > Data'Length then
         return Load_Invalid_Phoff;
      end if;

      return Load_Success;
   end Validate_ELF_Header;

   ---------------------------------------------------------------------------
   --  ELF Loading
   ---------------------------------------------------------------------------

   procedure Load_ELF (
      ELF_Data  : in     Byte_Array;
      State     : out    Interpreter_State;
      Gas_Limit : in     Gas_Amount;
      Result    : out    Load_Result
   )
   is
      H           : ELF32_Header;
      Status      : Load_Status;
      Code_Found  : Boolean := False;
      Data_Found  : Boolean := False;
      Code_Start  : Word := 0;
      Code_End    : Word := 0;
      Data_Start  : Word := 0;
      Data_End    : Word := 0;
      Stack_Top   : constant Word := 16#00FF_FFF0#;  -- Top of stack, 16-byte aligned
   begin
      --  Initialize state with zeros
      State.CPU := (
         Regs => (others => 0),
         PC => 0,
         Trap => Trap_None,
         Trap_Value => 0,
         Gas_Limit => Gas_Limit,
         Gas_Used => 0,
         Halted => False,
         Instruction_Count => 0
      );
      State.Memory := (others => 0);
      State.Bounds := Default_Bounds;
      State.Context := Empty_Context;

      Result := Failed_Load;

      --  Validate ELF header
      Status := Validate_ELF_Header (ELF_Data);
      if Status /= Load_Success then
         Result.Status := Status;
         return;
      end if;

      H := Parse_ELF_Header (ELF_Data);

      --  Process program headers
      for I in 0 .. Natural (H.E_Phnum) - 1 loop
         declare
            Phdr_Offset : constant Natural := Natural (H.E_Phoff) + I * ELF32_Phdr_Size;
            P           : ELF32_Phdr;
            Seg_End     : Word;
         begin
            --  Check bounds for program header
            if Phdr_Offset + ELF32_Phdr_Size > ELF_Data'Length then
               Result.Status := Load_File_Truncated;
               return;
            end if;

            P := Parse_Phdr (ELF_Data, Phdr_Offset);

            --  Only process PT_LOAD segments
            if P.P_Type = PT_LOAD and P.P_Memsz > 0 then
               --  Check segment fits in our memory
               if P.P_Vaddr > Word (Max_Memory_Size) or
                  P.P_Memsz > Word (Max_Memory_Size) or
                  P.P_Vaddr > Word (Max_Memory_Size) - P.P_Memsz
               then
                  Result.Status := Load_Segment_Too_Large;
                  return;
               end if;

               Seg_End := P.P_Vaddr + P.P_Memsz;

               --  Check file data is available
               if Natural (P.P_Offset) + Natural (P.P_Filesz) > ELF_Data'Length then
                  Result.Status := Load_File_Truncated;
                  return;
               end if;

               --  Copy segment data to memory
               if P.P_Filesz > 0 then
                  for J in 0 .. Natural (P.P_Filesz) - 1 loop
                     State.Memory (Natural (P.P_Vaddr) + J) :=
                        ELF_Data (ELF_Data'First + Natural (P.P_Offset) + J);
                  end loop;
               end if;

               --  Zero BSS portion (memsz > filesz)
               if P.P_Memsz > P.P_Filesz then
                  for J in Natural (P.P_Filesz) .. Natural (P.P_Memsz) - 1 loop
                     State.Memory (Natural (P.P_Vaddr) + J) := 0;
                  end loop;
               end if;

               --  Track code and data sections based on flags
               if (P.P_Flags and PF_X) /= 0 then
                  --  Executable segment (code)
                  if not Code_Found or P.P_Vaddr < Code_Start then
                     Code_Start := P.P_Vaddr;
                  end if;
                  if not Code_Found or Seg_End > Code_End then
                     Code_End := Seg_End;
                  end if;
                  Code_Found := True;
               elsif (P.P_Flags and PF_W) /= 0 then
                  --  Writable segment (data)
                  if not Data_Found or P.P_Vaddr < Data_Start then
                     Data_Start := P.P_Vaddr;
                  end if;
                  if not Data_Found or Seg_End > Data_End then
                     Data_End := Seg_End;
                  end if;
                  Data_Found := True;
               end if;
            end if;
         end;
      end loop;

      --  Verify we found a code section
      if not Code_Found then
         Result.Status := Load_No_Code_Section;
         return;
      end if;

      --  Set up memory bounds
      State.Bounds.Code_Start := Code_Start;
      State.Bounds.Code_End := Code_End;

      if Data_Found then
         State.Bounds.Data_Start := Data_Start;
         State.Bounds.Data_End := Data_End;
         State.Bounds.Heap_Start := Data_End;
      else
         --  No data section; heap starts after code
         State.Bounds.Data_Start := Code_End;
         State.Bounds.Data_End := Code_End;
         State.Bounds.Heap_Start := Code_End;
      end if;

      State.Bounds.Heap_End := Stack_Top - 4096;  -- Leave 4KB guard page
      State.Bounds.Stack_Start := Stack_Top - 1024 * 1024;  -- 1MB stack
      State.Bounds.Stack_End := Stack_Top;

      --  Set up initial CPU state
      State.CPU.PC := H.E_Entry;
      State.CPU.Regs (SP) := Stack_Top;  -- Stack pointer at top
      State.CPU.Regs (0) := 0;  -- Ensure x0 is zero

      --  Return success
      Result := (
         Status      => Load_Success,
         Entry_Point => H.E_Entry,
         Code_Start  => Code_Start,
         Code_End    => Code_End,
         Data_Start  => Data_Start,
         Data_End    => Data_End,
         Stack_Top   => Stack_Top
      );
   end Load_ELF;

   ---------------------------------------------------------------------------
   --  Convenience Entry Point
   ---------------------------------------------------------------------------

   procedure Load_Contract (
      ELF_Data  : in     Byte_Array;
      Gas_Limit : in     Gas_Amount;
      State     : out    Interpreter_State;
      Success   : out    Boolean
   )
   is
      Result : Load_Result;
   begin
      Load_ELF (ELF_Data, State, Gas_Limit, Result);
      Success := Result.Status = Load_Success;
   end Load_Contract;

end Sphinx_RV32_Loader;
