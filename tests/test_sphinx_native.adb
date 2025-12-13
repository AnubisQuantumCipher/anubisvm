--  Test SPHINX Native ELF Loading
--
--  Tests ELF binary parsing and loading functions.

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_Native; use Sphinx_Native;

procedure Test_Sphinx_Native is

   --  Create a minimal valid ELF64 header for ARM64
   function Create_Minimal_ELF return Binary_Buffer is
      ELF : Binary_Buffer := (others => 0);

      --  Write little-endian 16-bit
      procedure Write_LE16 (Offset : Natural; Val : Word32) is
      begin
         ELF (Binary_Index (Offset)) := Byte (Val and 16#FF#);
         ELF (Binary_Index (Offset + 1)) := Byte (Shift_Right (Val, 8) and 16#FF#);
      end Write_LE16;

      --  Write little-endian 32-bit
      procedure Write_LE32 (Offset : Natural; Val : Word32) is
      begin
         ELF (Binary_Index (Offset)) := Byte (Val and 16#FF#);
         ELF (Binary_Index (Offset + 1)) := Byte (Shift_Right (Val, 8) and 16#FF#);
         ELF (Binary_Index (Offset + 2)) := Byte (Shift_Right (Val, 16) and 16#FF#);
         ELF (Binary_Index (Offset + 3)) := Byte (Shift_Right (Val, 24) and 16#FF#);
      end Write_LE32;

      --  Write little-endian 64-bit
      procedure Write_LE64 (Offset : Natural; Val : Word64) is
      begin
         Write_LE32 (Offset, Word32 (Val and 16#FFFFFFFF#));
         Write_LE32 (Offset + 4, Word32 (Shift_Right (Val, 32) and 16#FFFFFFFF#));
      end Write_LE64;

   begin
      --  ELF Magic (bytes 0-3)
      ELF (0) := 16#7F#;
      ELF (1) := 16#45#;  -- 'E'
      ELF (2) := 16#4C#;  -- 'L'
      ELF (3) := 16#46#;  -- 'F'

      --  ELF Class: ELFCLASS64 (byte 4)
      ELF (4) := 2;

      --  ELF Data: Little endian (byte 5)
      ELF (5) := 1;

      --  ELF Version (byte 6)
      ELF (6) := 1;

      --  OS/ABI (byte 7)
      ELF (7) := 0;  -- ELFOSABI_NONE

      --  e_type: ET_EXEC (offset 16)
      Write_LE16 (16, 2);  -- Executable

      --  e_machine: EM_AARCH64 (offset 18)
      Write_LE16 (18, 16#B7#);  -- ARM64

      --  e_version (offset 20)
      Write_LE32 (20, 1);

      --  e_entry: Entry point (offset 24)
      Write_LE64 (24, 16#400000#);  -- Typical entry point

      --  e_phoff: Program header offset (offset 32)
      Write_LE64 (32, 64);  -- Right after ELF header

      --  e_shoff: Section header offset (offset 40)
      Write_LE64 (40, 128);  -- After program headers

      --  e_flags (offset 48)
      Write_LE32 (48, 0);

      --  e_ehsize: ELF header size (offset 52)
      Write_LE16 (52, 64);

      --  e_phentsize: Program header entry size (offset 54)
      Write_LE16 (54, 56);

      --  e_phnum: Number of program headers (offset 56)
      Write_LE16 (56, 1);

      --  e_shentsize: Section header entry size (offset 58)
      Write_LE16 (58, 64);

      --  e_shnum: Number of section headers (offset 60)
      Write_LE16 (60, 3);  -- null, .text, .data

      --  e_shstrndx: Section name string table index (offset 62)
      Write_LE16 (62, 2);

      --  Section Header 0 (offset 128): Null section
      --  (all zeros, already set)

      --  Section Header 1 (offset 192): .text section
      Write_LE32 (192, 1);        -- sh_name (offset into shstrtab)
      Write_LE32 (196, 1);        -- sh_type = SHT_PROGBITS
      Write_LE64 (200, 6);        -- sh_flags = SHF_ALLOC | SHF_EXECINSTR
      Write_LE64 (208, 16#400000#); -- sh_addr
      Write_LE64 (216, 512);      -- sh_offset
      Write_LE64 (224, 256);      -- sh_size

      --  Section Header 2 (offset 256): .data section
      Write_LE32 (256, 7);        -- sh_name
      Write_LE32 (260, 1);        -- sh_type = SHT_PROGBITS
      Write_LE64 (264, 3);        -- sh_flags = SHF_ALLOC | SHF_WRITE
      Write_LE64 (272, 16#410000#); -- sh_addr
      Write_LE64 (280, 768);      -- sh_offset
      Write_LE64 (288, 128);      -- sh_size

      --  Add some fake code at offset 512
      ELF (512) := 16#1F#;  -- NOP (ARM64)
      ELF (513) := 16#20#;
      ELF (514) := 16#03#;
      ELF (515) := 16#D5#;

      return ELF;
   end Create_Minimal_ELF;

   ELF_Binary : Binary_Buffer;
   Result : Load_Result;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   procedure Check (Condition : Boolean; Test_Name : String) is
   begin
      if Condition then
         Put_Line ("  [PASS] " & Test_Name);
         Tests_Passed := Tests_Passed + 1;
      else
         Put_Line ("  [FAIL] " & Test_Name);
         Tests_Failed := Tests_Failed + 1;
      end if;
   end Check;

begin
   Put_Line ("SPHINX Native ELF Loader Tests");
   Put_Line ("==============================");
   Put_Line ("");

   --  Test 1: Invalid magic
   Put_Line ("Test 1: Invalid ELF magic");
   declare
      Bad_ELF : Binary_Buffer := (others => 0);
   begin
      Result := Load_ELF (Bad_ELF, 100);
      Check (Result.Error = Load_Invalid_Magic, "Rejects non-ELF data");
   end;

   --  Test 2: Too small
   Put_Line ("Test 2: ELF too small");
   declare
      Small_ELF : Binary_Buffer := (others => 0);
   begin
      Small_ELF (0) := 16#7F#;
      Small_ELF (1) := 16#45#;
      Small_ELF (2) := 16#4C#;
      Small_ELF (3) := 16#46#;
      Result := Load_ELF (Small_ELF, 32);  -- Too small for header
      Check (Result.Error = Load_Invalid_Magic, "Rejects too-small ELF");
   end;

   --  Test 3: Wrong class (32-bit)
   Put_Line ("Test 3: Wrong ELF class (32-bit)");
   declare
      ELF32 : Binary_Buffer := (others => 0);
   begin
      ELF32 (0) := 16#7F#;
      ELF32 (1) := 16#45#;
      ELF32 (2) := 16#4C#;
      ELF32 (3) := 16#46#;
      ELF32 (4) := 1;  -- ELFCLASS32
      Result := Load_ELF (ELF32, 100);
      Check (Result.Error = Load_Unsupported_Class, "Rejects 32-bit ELF");
   end;

   --  Test 4: Valid minimal ELF
   Put_Line ("Test 4: Valid minimal ELF64");
   ELF_Binary := Create_Minimal_ELF;
   Result := Load_ELF (ELF_Binary, 1024);
   Check (Result.Error = Load_Success, "Loads valid ELF");
   Check (Result.Contract.Is_Valid, "Contract marked valid");
   Check (Result.Contract.Arch = Arch_ARM64, "Correct architecture");
   Check (Result.Contract.Entry_Addr = 16#400000#, "Correct entry point");
   Check (Result.Contract.Code_Base = 16#400000#, "Code base extracted");
   Check (Result.Contract.Code_Size = 256, "Code size extracted");
   Check (Result.Contract.Data_Base = 16#410000#, "Data base extracted");
   Check (Result.Contract.Data_Size = 128, "Data size extracted");

   --  Test 5: Header validation
   Put_Line ("Test 5: Header validation");
   Check (Validate_Header (ELF_Binary, 1024) = Load_Success, "Header validates");
   Check (Get_Entry_Point (ELF_Binary, 1024) = 16#400000#, "Entry point correct");
   Check (Get_Architecture (ELF_Binary) = Arch_ARM64, "Architecture correct");

   --  Test 6: Memory layout creation
   Put_Line ("Test 6: Memory layout");
   declare
      Layout : Region_Layout;
   begin
      Layout := Create_Layout (Result.Contract);
      Check (Layout (0).Reg_Type = Region_Code, "Code region created");
      Check (Layout (0).Executable, "Code is executable");
      Check (not Layout (0).Writable, "Code is not writable");
      Check (Layout (1).Reg_Type = Region_Data, "Data region created");
      Check (Layout (1).Writable, "Data is writable");
      Check (not Layout (1).Executable, "Data is not executable");
   end;

   --  Test 7: Security validation
   Put_Line ("Test 7: Security validation");
   declare
      Security : Security_Result;
   begin
      Security := Validate_Security (Result.Contract);
      Check (Is_Secure (Security), "Security checks pass");
   end;

   --  Summary
   Put_Line ("");
   Put_Line ("==============================");
   Put_Line ("Tests Passed:" & Natural'Image (Tests_Passed));
   Put_Line ("Tests Failed:" & Natural'Image (Tests_Failed));

   if Tests_Failed = 0 then
      Put_Line ("RESULT: ALL TESTS PASSED");
   else
      Put_Line ("RESULT: SOME TESTS FAILED");
   end if;

end Test_Sphinx_Native;
