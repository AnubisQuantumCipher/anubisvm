--  SPHINX RV32 Loader: ELF Loading for RISC-V Interpreter
--
--  This package loads RV32 ELF binaries into the interpreter state.
--  It parses the ELF header and program headers to extract:
--  - Code section (PT_LOAD with PF_X)
--  - Data section (PT_LOAD with PF_W)
--  - Entry point
--  - Memory bounds
--
--  SPARK Verification Properties:
--  - ELF header is validated before loading
--  - Memory bounds are checked before copying
--  - Entry point is validated for alignment
--
--  Supported ELF Features:
--  - ELF32 little-endian (ELFCLASS32, ELFDATA2LSB)
--  - RISC-V machine type (EM_RISCV = 243)
--  - Executable type (ET_EXEC)
--  - Program headers (PT_LOAD segments)
--
--  References:
--  - ELF Specification v1.2
--  - RISC-V ELF psABI

pragma SPARK_Mode (On);

with Sphinx_RV32_Types; use Sphinx_RV32_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

package Sphinx_RV32_Loader with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  ELF Constants
   ---------------------------------------------------------------------------

   --  ELF Magic Number
   ELF_MAGIC : constant Word := 16#464C457F#;  -- 0x7F 'E' 'L' 'F'

   --  ELF Class
   ELFCLASS32 : constant := 1;
   ELFCLASS64 : constant := 2;

   --  ELF Data Encoding
   ELFDATA2LSB : constant := 1;  -- Little-endian
   ELFDATA2MSB : constant := 2;  -- Big-endian

   --  ELF Type
   ET_EXEC : constant := 2;  -- Executable
   ET_DYN  : constant := 3;  -- Shared object

   --  ELF Machine
   EM_RISCV : constant := 243;

   --  Program Header Type
   PT_NULL    : constant := 0;
   PT_LOAD    : constant := 1;
   PT_DYNAMIC : constant := 2;
   PT_INTERP  : constant := 3;

   --  Program Header Flags
   PF_X : constant := 1;  -- Execute
   PF_W : constant := 2;  -- Write
   PF_R : constant := 4;  -- Read

   ---------------------------------------------------------------------------
   --  ELF Header Structure (32-bit)
   ---------------------------------------------------------------------------

   type ELF32_Header is record
      E_Ident_Magic    : Word;      -- Magic number (0x7F, 'E', 'L', 'F')
      E_Ident_Class    : RV_Byte;   -- 32-bit = 1
      E_Ident_Data     : RV_Byte;   -- Little-endian = 1
      E_Ident_Version  : RV_Byte;   -- ELF version = 1
      E_Ident_OSABI    : RV_Byte;   -- OS ABI
      E_Ident_Pad      : Word64;    -- Padding (8 bytes)
      E_Type           : Half;      -- ET_EXEC = 2
      E_Machine        : Half;      -- EM_RISCV = 243
      E_Version        : Word;      -- ELF version = 1
      E_Entry          : Word;      -- Entry point
      E_Phoff          : Word;      -- Program header offset
      E_Shoff          : Word;      -- Section header offset
      E_Flags          : Word;      -- Flags
      E_Ehsize         : Half;      -- ELF header size
      E_Phentsize      : Half;      -- Program header entry size
      E_Phnum          : Half;      -- Number of program headers
      E_Shentsize      : Half;      -- Section header entry size
      E_Shnum          : Half;      -- Number of section headers
      E_Shstrndx       : Half;      -- Section name string table index
   end record;

   ELF32_Header_Size : constant := 52;

   ---------------------------------------------------------------------------
   --  Program Header Structure (32-bit)
   ---------------------------------------------------------------------------

   type ELF32_Phdr is record
      P_Type   : Word;  -- Segment type
      P_Offset : Word;  -- File offset
      P_Vaddr  : Word;  -- Virtual address
      P_Paddr  : Word;  -- Physical address
      P_Filesz : Word;  -- Size in file
      P_Memsz  : Word;  -- Size in memory
      P_Flags  : Word;  -- Flags (PF_X, PF_W, PF_R)
      P_Align  : Word;  -- Alignment
   end record;

   ELF32_Phdr_Size : constant := 32;

   ---------------------------------------------------------------------------
   --  Load Result
   ---------------------------------------------------------------------------

   type Load_Status is (
      Load_Success,
      Load_Invalid_Magic,
      Load_Invalid_Class,
      Load_Invalid_Endian,
      Load_Invalid_Machine,
      Load_Invalid_Type,
      Load_Invalid_Entry,
      Load_Invalid_Phoff,
      Load_Segment_Too_Large,
      Load_Segment_Overlap,
      Load_Memory_Error,
      Load_No_Code_Section,
      Load_File_Truncated
   );

   type Load_Result is record
      Status      : Load_Status;
      Entry_Point : Program_Counter;
      Code_Start  : Word;
      Code_End    : Word;
      Data_Start  : Word;
      Data_End    : Word;
      Stack_Top   : Word;
   end record;

   Failed_Load : constant Load_Result := (
      Status      => Load_Invalid_Magic,
      Entry_Point => 0,
      Code_Start  => 0,
      Code_End    => 0,
      Data_Start  => 0,
      Data_End    => 0,
      Stack_Top   => 0
   );

   ---------------------------------------------------------------------------
   --  ELF Validation
   ---------------------------------------------------------------------------

   --  Validate ELF header for RV32
   function Validate_ELF_Header (
      Data : Byte_Array
   ) return Load_Status with
      Global => null,
      Pre    => Data'Length >= ELF32_Header_Size;

   --  Check if data looks like a valid ELF file
   function Is_Valid_ELF (Data : Byte_Array) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  ELF Loading
   ---------------------------------------------------------------------------

   --  Load ELF binary into interpreter state
   --
   --  This function:
   --  1. Validates the ELF header
   --  2. Parses program headers
   --  3. Loads PT_LOAD segments into memory
   --  4. Sets up memory bounds
   --  5. Returns entry point and bounds
   --
   procedure Load_ELF (
      ELF_Data  : in     Byte_Array;
      State     : out    Interpreter_State;
      Gas_Limit : in     Gas_Amount;
      Result    : out    Load_Result
   ) with
      Global => null,
      Pre    => ELF_Data'Length >= ELF32_Header_Size,
      Post   => (if Result.Status = Load_Success then
                    PC_Aligned (Result.Entry_Point));

   --  Load ELF and create initial CPU state
   procedure Load_Contract (
      ELF_Data  : in     Byte_Array;
      Gas_Limit : in     Gas_Amount;
      State     : out    Interpreter_State;
      Success   : out    Boolean
   ) with
      Global => null,
      Pre    => ELF_Data'Length >= ELF32_Header_Size;

   ---------------------------------------------------------------------------
   --  Header Parsing Helpers
   ---------------------------------------------------------------------------

   --  Parse ELF32 header from byte array
   function Parse_ELF_Header (
      Data : Byte_Array
   ) return ELF32_Header with
      Global => null,
      Pre    => Data'Length >= ELF32_Header_Size;

   --  Parse program header from byte array
   function Parse_Phdr (
      Data   : Byte_Array;
      Offset : Natural
   ) return ELF32_Phdr with
      Global => null,
      Pre    => Offset + ELF32_Phdr_Size <= Data'Length;

   --  Read little-endian 32-bit word from byte array
   function Read_LE32 (Data : Byte_Array; Offset : Natural) return Word with
      Global => null,
      Pre    => Offset + 4 <= Data'Length;

   --  Read little-endian 16-bit half-word from byte array
   function Read_LE16 (Data : Byte_Array; Offset : Natural) return Half with
      Global => null,
      Pre    => Offset + 2 <= Data'Length;

end Sphinx_RV32_Loader;
