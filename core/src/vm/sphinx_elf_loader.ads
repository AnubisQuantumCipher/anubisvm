pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with System;

--  SPHINX ELF Loader: True ELF Execution Engine
--
--  This package implements direct execution of ELF binaries without
--  relying on dlopen() or the system dynamic linker. It provides:
--
--  - mmap-based section loading into isolated memory regions
--  - Full relocation processing (R_X86_64_*, R_AARCH64_*, R_RISCV_*)
--  - PLT/GOT setup for position-independent code
--  - Page permission management (RX for code, RW for data)
--  - Direct entry point invocation with proper ABI
--
--  Architecture Support:
--  - x86-64 (AMD64) - System V ABI
--  - AArch64 (ARM64) - ARM64 ABI
--  - RISC-V 64 - RISC-V ABI
--
--  Security Model:
--  - W^X enforcement via mprotect
--  - ASLR support for position-independent executables
--  - No system dynamic linker dependencies
--  - Isolated memory regions per contract
--
--  References:
--  - ELF Specification (Tool Interface Standard)
--  - System V ABI x86-64 Supplement
--  - ARM64 ELF ABI
--  - RISC-V ELF psABI

package Sphinx_ELF_Loader with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  ELF Constants
   ---------------------------------------------------------------------------

   --  ELF Header offsets (ELF64)
   ELF_MAGIC_OFFSET     : constant := 0;
   ELF_CLASS_OFFSET     : constant := 4;
   ELF_DATA_OFFSET      : constant := 5;
   ELF_VERSION_OFFSET   : constant := 6;
   ELF_TYPE_OFFSET      : constant := 16;
   ELF_MACHINE_OFFSET   : constant := 18;
   ELF_ENTRY_OFFSET     : constant := 24;
   ELF_PHOFF_OFFSET     : constant := 32;
   ELF_SHOFF_OFFSET     : constant := 40;
   ELF_PHENTSIZE_OFFSET : constant := 54;
   ELF_PHNUM_OFFSET     : constant := 56;
   ELF_SHENTSIZE_OFFSET : constant := 58;
   ELF_SHNUM_OFFSET     : constant := 60;
   ELF_SHSTRNDX_OFFSET  : constant := 62;

   --  ELF Class
   ELFCLASS64 : constant := 2;

   --  ELF Data encoding
   ELFDATA2LSB : constant := 1;  --  Little-endian
   ELFDATA2MSB : constant := 2;  --  Big-endian

   --  ELF Type
   ET_EXEC : constant := 2;  --  Executable file
   ET_DYN  : constant := 3;  --  Shared object (position-independent)

   --  ELF Machine types
   EM_X86_64  : constant := 16#3E#;   --  AMD x86-64
   EM_AARCH64 : constant := 16#B7#;   --  ARM AArch64
   EM_RISCV   : constant := 16#F3#;   --  RISC-V

   --  Program header types
   PT_NULL    : constant := 0;
   PT_LOAD    : constant := 1;  --  Loadable segment
   PT_DYNAMIC : constant := 2;  --  Dynamic linking info
   PT_INTERP  : constant := 3;  --  Interpreter path
   PT_NOTE    : constant := 4;  --  Auxiliary info

   --  Program header flags
   PF_X : constant := 1;  --  Execute
   PF_W : constant := 2;  --  Write
   PF_R : constant := 4;  --  Read

   --  Section header types
   SHT_NULL     : constant := 0;
   SHT_PROGBITS : constant := 1;  --  Program data
   SHT_SYMTAB   : constant := 2;  --  Symbol table
   SHT_STRTAB   : constant := 3;  --  String table
   SHT_RELA     : constant := 4;  --  Relocation with addend
   SHT_HASH     : constant := 5;  --  Symbol hash table
   SHT_DYNAMIC  : constant := 6;  --  Dynamic linking info
   SHT_NOTE     : constant := 7;  --  Notes
   SHT_NOBITS   : constant := 8;  --  BSS
   SHT_REL      : constant := 9;  --  Relocation without addend

   --  Section header flags
   SHF_WRITE     : constant := 16#01#;  --  Writable
   SHF_ALLOC     : constant := 16#02#;  --  Occupies memory
   SHF_EXECINSTR : constant := 16#04#;  --  Executable

   --  Dynamic table tags
   DT_NULL     : constant := 0;
   DT_NEEDED   : constant := 1;   --  Name of needed library
   DT_PLTRELSZ : constant := 2;   --  Size of PLT relocations
   DT_PLTGOT   : constant := 3;   --  Address of PLT/GOT
   DT_HASH     : constant := 4;   --  Address of symbol hash table
   DT_STRTAB   : constant := 5;   --  Address of string table
   DT_SYMTAB   : constant := 6;   --  Address of symbol table
   DT_RELA     : constant := 7;   --  Address of Rela relocations
   DT_RELASZ   : constant := 8;   --  Size of Rela relocations
   DT_RELAENT  : constant := 9;   --  Size of one Rela entry
   DT_STRSZ    : constant := 10;  --  Size of string table
   DT_SYMENT   : constant := 11;  --  Size of symbol table entry
   DT_INIT     : constant := 12;  --  Address of init function
   DT_FINI     : constant := 13;  --  Address of fini function
   DT_SONAME   : constant := 14;  --  Name of shared object
   DT_RPATH    : constant := 15;  --  Library search path
   DT_REL      : constant := 17;  --  Address of Rel relocations
   DT_RELSZ    : constant := 18;  --  Size of Rel relocations
   DT_RELENT   : constant := 19;  --  Size of one Rel entry
   DT_PLTREL   : constant := 20;  --  Type of reloc in PLT
   DT_JMPREL   : constant := 23;  --  Address of PLT relocations

   ---------------------------------------------------------------------------
   --  Relocation Types
   ---------------------------------------------------------------------------

   --  x86-64 relocation types
   R_X86_64_NONE         : constant := 0;
   R_X86_64_64           : constant := 1;   --  Direct 64-bit
   R_X86_64_PC32         : constant := 2;   --  PC relative 32-bit signed
   R_X86_64_GOT32        : constant := 3;   --  32-bit GOT entry
   R_X86_64_PLT32        : constant := 4;   --  32-bit PLT address
   R_X86_64_COPY         : constant := 5;   --  Copy symbol at runtime
   R_X86_64_GLOB_DAT     : constant := 6;   --  Create GOT entry
   R_X86_64_JUMP_SLOT    : constant := 7;   --  Create PLT entry
   R_X86_64_RELATIVE     : constant := 8;   --  Adjust by program base
   R_X86_64_GOTPCREL     : constant := 9;   --  32-bit signed PC relative to GOT
   R_X86_64_32           : constant := 10;  --  Direct 32-bit zero extended
   R_X86_64_32S          : constant := 11;  --  Direct 32-bit sign extended
   R_X86_64_16           : constant := 12;  --  Direct 16-bit zero extended
   R_X86_64_PC16         : constant := 13;  --  16-bit sign extended PC relative
   R_X86_64_8            : constant := 14;  --  Direct 8-bit sign extended
   R_X86_64_PC8          : constant := 15;  --  8-bit sign extended PC relative

   --  AArch64 relocation types
   R_AARCH64_NONE        : constant := 0;
   R_AARCH64_ABS64       : constant := 257;  --  Direct 64-bit
   R_AARCH64_ABS32       : constant := 258;  --  Direct 32-bit
   R_AARCH64_ABS16       : constant := 259;  --  Direct 16-bit
   R_AARCH64_PREL64      : constant := 260;  --  PC-relative 64-bit
   R_AARCH64_PREL32      : constant := 261;  --  PC-relative 32-bit
   R_AARCH64_PREL16      : constant := 262;  --  PC-relative 16-bit
   R_AARCH64_GLOB_DAT    : constant := 1025; --  Create GOT entry
   R_AARCH64_JUMP_SLOT   : constant := 1026; --  Create PLT entry
   R_AARCH64_RELATIVE    : constant := 1027; --  Adjust by program base
   R_AARCH64_CALL26      : constant := 283;  --  PC-relative 26-bit call
   R_AARCH64_JUMP26      : constant := 282;  --  PC-relative 26-bit jump
   R_AARCH64_ADR_PREL_PG_HI21 : constant := 275;  --  Page offset 21 bits

   --  RISC-V relocation types
   R_RISCV_NONE          : constant := 0;
   R_RISCV_64            : constant := 2;   --  Direct 64-bit
   R_RISCV_RELATIVE      : constant := 3;   --  Adjust by program base
   R_RISCV_JUMP_SLOT     : constant := 5;   --  Create PLT entry
   R_RISCV_CALL          : constant := 18;  --  PC-relative call
   R_RISCV_CALL_PLT      : constant := 19;  --  PC-relative call to PLT
   R_RISCV_GOT_HI20      : constant := 20;  --  PC-relative GOT reference
   R_RISCV_PCREL_HI20    : constant := 23;  --  PC-relative high 20 bits
   R_RISCV_PCREL_LO12_I  : constant := 24;  --  PC-relative low 12 bits
   R_RISCV_HI20          : constant := 26;  --  High 20 bits
   R_RISCV_LO12_I        : constant := 27;  --  Low 12 bits

   ---------------------------------------------------------------------------
   --  Loaded ELF Image
   ---------------------------------------------------------------------------

   --  Maximum loadable segments
   Max_Segments : constant := 16;
   type Segment_Index is range 0 .. Max_Segments - 1;

   --  Memory segment descriptor
   type Memory_Segment is record
      Virtual_Addr : Word64;      --  Virtual address
      Physical_Addr : Word64;     --  Physical address (usually = virtual)
      File_Offset  : Word64;      --  Offset in ELF file
      File_Size    : Word64;      --  Size in file
      Memory_Size  : Word64;      --  Size in memory (may be > file size)
      Flags        : Word32;      --  PF_R | PF_W | PF_X
      Alignment    : Word64;      --  Required alignment
      Mapped_Addr  : System.Address;  --  Actual mmap address (runtime)
      Is_Loaded    : Boolean;     --  Successfully mapped
   end record;

   Invalid_Segment : constant Memory_Segment := (
      Virtual_Addr  => 0,
      Physical_Addr => 0,
      File_Offset   => 0,
      File_Size     => 0,
      Memory_Size   => 0,
      Flags         => 0,
      Alignment     => 0,
      Mapped_Addr   => System.Null_Address,
      Is_Loaded     => False
   );

   type Segment_Array is array (Segment_Index) of Memory_Segment;

   --  Loaded ELF image
   type ELF_Image is record
      Entry_Point   : Word64;             --  Program entry point
      Base_Address  : Word64;             --  Load base (for PIE)
      Machine       : Word32;             --  Architecture (EM_*)
      Segments      : Segment_Array;      --  Loaded segments
      Segment_Count : Natural;            --  Number of segments
      Is_PIE        : Boolean;            --  Position-independent executable
      GOT_Address   : Word64;             --  Global Offset Table address
      PLT_Address   : Word64;             --  Procedure Linkage Table address
      Dynamic_Addr  : Word64;             --  Dynamic section address
      Is_Valid      : Boolean;            --  Successfully loaded
   end record;

   Invalid_ELF_Image : constant ELF_Image := (
      Entry_Point   => 0,
      Base_Address  => 0,
      Machine       => 0,
      Segments      => (others => Invalid_Segment),
      Segment_Count => 0,
      Is_PIE        => False,
      GOT_Address   => 0,
      PLT_Address   => 0,
      Dynamic_Addr  => 0,
      Is_Valid      => False
   );

   ---------------------------------------------------------------------------
   --  Load and Execute
   ---------------------------------------------------------------------------

   --  Load error codes
   type ELF_Load_Error is (
      ELF_Success,
      ELF_Invalid_Magic,
      ELF_Unsupported_Class,
      ELF_Unsupported_Machine,
      ELF_Invalid_Type,
      ELF_Missing_Segments,
      ELF_Mmap_Failed,
      ELF_Mprotect_Failed,
      ELF_Relocation_Failed,
      ELF_Symbol_Not_Found,
      ELF_Security_Violation
   );

   type ELF_Load_Result is record
      Error : ELF_Load_Error;
      Image : ELF_Image;
   end record;

   --  Load ELF binary from buffer into memory
   --  This performs:
   --  1. Parse ELF headers and program headers
   --  2. mmap segments with appropriate permissions
   --  3. Process relocations
   --  4. Set up GOT/PLT
   --  5. Apply page protections (W^X enforcement)
   function Load_ELF_Binary (
      Binary : access constant Byte_Array;
      Size   : Natural
   ) return ELF_Load_Result with
      Global => null;

   --  Execute loaded ELF at entry point
   --  This:
   --  1. Sets up stack and execution context
   --  2. Jumps to entry point with proper ABI
   --  3. Handles return value and cleanup
   type Execution_Result is record
      Exit_Code   : Integer;
      Gas_Used    : Gas_Amount;
      Return_Data : Hash256;
      Success     : Boolean;
   end record;

   function Execute_ELF (
      Image     : ELF_Image;
      Calldata  : access constant Byte_Array;
      Gas_Limit : Gas_Amount
   ) return Execution_Result with
      Global => null;

   --  Unload ELF image and free memory
   procedure Unload_ELF (Image : in out ELF_Image);

   ---------------------------------------------------------------------------
   --  Relocation Processing
   ---------------------------------------------------------------------------

   --  Process all relocations for loaded image
   --  This handles:
   --  - R_*_RELATIVE: Adjust by base address
   --  - R_*_GLOB_DAT: Resolve global symbols
   --  - R_*_JUMP_SLOT: Create PLT entries
   --  - R_*_64/32/16: Direct relocations
   procedure Process_Relocations (
      Image   : in out ELF_Image;
      Binary  : access constant Byte_Array;
      Success : out Boolean
   ) with
      Global => null;

   --  Resolve symbol and return its address
   function Resolve_Symbol (
      Image       : ELF_Image;
      Binary      : access constant Byte_Array;
      Symbol_Name : String
   ) return Word64 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Memory Protection
   ---------------------------------------------------------------------------

   --  Set page protections based on segment flags
   --  Enforces W^X: no page can be both writable and executable
   function Apply_Page_Protections (Image : ELF_Image) return Boolean with
      Global => null;

   --  Check if address is within loaded segments
   function Is_Valid_Address (
      Image   : ELF_Image;
      Address : Word64
   ) return Boolean with
      Global => null;

private

   ---------------------------------------------------------------------------
   --  Internal Helpers (Implementation in body with SPARK_Mode Off)
   ---------------------------------------------------------------------------

   --  These functions require system calls and are implemented in the body

end Sphinx_ELF_Loader;
