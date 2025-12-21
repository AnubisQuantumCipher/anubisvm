--  SPHINX RV32 Memory: Bounds-Checked Memory Access
--
--  This package provides memory operations for the RISC-V interpreter.
--  All accesses are bounds-checked and alignment-verified.
--
--  SPARK Verification Properties:
--  - No out-of-bounds access (proven)
--  - Alignment requirements enforced
--  - Load/store operations are sound
--  - Little-endian byte order (RISC-V native)
--
--  Memory Layout:
--  - Code section: Read-only executable
--  - Data section: Read-write
--  - Heap: Growable, read-write
--  - Stack: Read-write, grows downward
--
--  References:
--  - RISC-V Unprivileged ISA Specification v20191213, Chapter 1.4

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;

package Sphinx_RV32_Memory with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Memory Access Checks (Ghost Functions)
   ---------------------------------------------------------------------------

   --  Check if address range is valid for memory access
   function Is_Valid_Address (
      Addr   : Word;
      Size   : Positive;
      Memory : Contract_Memory
   ) return Boolean is
      (Natural (Addr) <= Max_Memory_Size - Size)
   with Ghost, Pure_Function;

   --  Check if address is aligned for access size
   function Is_Aligned (Addr : Word; Size : Positive) return Boolean is
      (case Size is
         when 1 => True,
         when 2 => Addr mod 2 = 0,
         when 4 => Addr mod 4 = 0,
         when others => False)
   with Ghost, Pure_Function;

   --  Check if address is in code section
   function In_Code_Section (Addr : Word; Bounds : Memory_Bounds) return Boolean is
      (Addr >= Bounds.Code_Start and Addr < Bounds.Code_End)
   with Ghost, Pure_Function;

   --  Check if address is in writable section
   function Is_Writable (Addr : Word; Bounds : Memory_Bounds) return Boolean is
      (Addr >= Bounds.Data_Start and Addr < Bounds.Stack_End and
       not In_Code_Section (Addr, Bounds))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Byte Load Operations (Always Aligned)
   ---------------------------------------------------------------------------

   --  Load byte (unsigned)
   procedure Load_Byte (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    Word;
      Success : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then Value < 256);

   --  Load byte (signed, sign-extended to 32 bits)
   procedure Load_Byte_Signed (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    SWord;
      Success : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then Value in -128 .. 127);

   ---------------------------------------------------------------------------
   --  Half-Word (16-bit) Load Operations
   ---------------------------------------------------------------------------

   --  Load half-word (unsigned, little-endian)
   procedure Load_Half (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    Word;
      Success : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then Value < 65536);

   --  Load half-word (signed, sign-extended to 32 bits)
   procedure Load_Half_Signed (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    SWord;
      Success : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then Value in -32768 .. 32767);

   ---------------------------------------------------------------------------
   --  Word (32-bit) Load Operations
   ---------------------------------------------------------------------------

   --  Load word (little-endian)
   procedure Load_Word (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    Word;
      Success : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Byte Store Operations
   ---------------------------------------------------------------------------

   --  Store byte
   procedure Store_Byte (
      Memory  : in out Contract_Memory;
      Addr    : in     Word;
      Value   : in     Word;
      Success : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then
                    Memory (Natural (Addr)) = RV_Byte (Value and 16#FF#));

   ---------------------------------------------------------------------------
   --  Half-Word (16-bit) Store Operations
   ---------------------------------------------------------------------------

   --  Store half-word (little-endian)
   procedure Store_Half (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Value   : in     Word;
      Success : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Word (32-bit) Store Operations
   ---------------------------------------------------------------------------

   --  Store word (little-endian)
   procedure Store_Word (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Value   : in     Word;
      Success : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Instruction Fetch
   ---------------------------------------------------------------------------

   --  Fetch instruction at PC (4-byte aligned read from code section)
   procedure Fetch_Instruction (
      Memory  : in     Contract_Memory;
      PC      : in     Program_Counter;
      Bounds  : in     Memory_Bounds;
      Instr   : out    Word;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => PC mod 4 = 0,
      Post   => Success = (Natural (PC) + 4 <= Max_Memory_Size and
                           PC >= Bounds.Code_Start and PC < Bounds.Code_End);

   ---------------------------------------------------------------------------
   --  Block Memory Operations
   ---------------------------------------------------------------------------

   --  Copy bytes from memory to buffer
   procedure Memory_Read_Block (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Buffer'Length > 0,
      Post   => (if Success then
                    Natural (Addr) + Buffer'Length <= Max_Memory_Size);

   --  Copy bytes from buffer to memory
   procedure Memory_Write_Block (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Buffer  : in     Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Buffer'Length > 0,
      Post   => (if Success then
                    Natural (Addr) + Buffer'Length <= Max_Memory_Size);

   ---------------------------------------------------------------------------
   --  Memory Initialization
   ---------------------------------------------------------------------------

   --  Zero out memory region
   procedure Zero_Memory (
      Memory : in Out Contract_Memory;
      Addr   : in     Word;
      Size   : in     Natural;
      Success : out   Boolean
   ) with
      Global => null,
      Post   => (if Success then
                    (for all I in Natural (Addr) .. Natural (Addr) + Size - 1 =>
                        Memory (I) = 0));

   --  Initialize memory from byte array (for ELF loading)
   procedure Initialize_Memory (
      Memory  : out    Contract_Memory;
      Code    : in     Byte_Array;
      Code_Base : in   Word;
      Data    : in     Byte_Array;
      Data_Base : in   Word;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Code'Length > 0 and Data'Length > 0;

   ---------------------------------------------------------------------------
   --  Stack Operations
   ---------------------------------------------------------------------------

   --  Push word onto stack (decrement SP, then store)
   procedure Push (
      Memory  : in Out Contract_Memory;
      SP      : in Out Word;
      Value   : in     Word;
      Bounds  : in     Memory_Bounds;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => SP >= 4,  -- Room for decrement
      Post   => (if Success then SP = SP'Old - 4);

   --  Pop word from stack (load, then increment SP)
   procedure Pop (
      Memory  : in     Contract_Memory;
      SP      : in Out Word;
      Value   : out    Word;
      Bounds  : in     Memory_Bounds;
      Success : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then SP = SP'Old + 4);

end Sphinx_RV32_Memory;
