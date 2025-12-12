pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Sandbox;
with Anubis_Types;
with Anubis_SHA3;

--  SPHINX Native: Native ELF Execution for KHEPRI Contracts
--
--  This package implements the native execution layer for KHEPRI smart
--  contracts. Unlike traditional VMs that interpret bytecode, KHEPRI
--  contracts are compiled to native ARM/x86 ELF binaries and executed
--  directly with hardware-level sandboxing.
--
--  Key Features:
--  - ELF binary loading and validation
--  - Memory region isolation (stack, heap, code)
--  - Syscall interception and metering
--  - WCET-based gas accounting
--  - Hardware-assisted sandboxing (when available)
--
--  Security Model:
--  - W^X enforcement (write XOR execute)
--  - Stack canaries
--  - Address space layout randomization (ASLR)
--  - Capability-based syscall filtering
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8: SPHINX Native Execution
--  - ELF Specification (Tool Interface Standard)
--  - ARM/x86 ABI Specifications

package Sphinx_Native with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  ELF Binary Types
   ---------------------------------------------------------------------------

   --  Maximum ELF binary size (24 KB per KHEPRI spec)
   Max_Binary_Size : constant := 24 * 1024;

   --  ELF header magic
   ELF_Magic : constant array (0 .. 3) of Byte := (16#7F#, 16#45#, 16#4C#, 16#46#);

   --  ELF class (32 or 64 bit)
   type ELF_Class is (ELF_32, ELF_64);

   --  Target architecture
   type Target_Arch is (
      Arch_ARM64,   -- AArch64
      Arch_X86_64,  -- AMD64
      Arch_RISCV64  -- RISC-V 64
   );

   --  Binary buffer
   type Binary_Index is range 0 .. Max_Binary_Size - 1;
   type Binary_Buffer is array (Binary_Index) of Byte;

   --  ELF section types
   type Section_Type is (
      Section_Null,
      Section_Progbits,   -- Code/data
      Section_Symtab,     -- Symbol table
      Section_Strtab,     -- String table
      Section_Rela,       -- Relocations
      Section_Nobits,     -- BSS
      Section_Note,       -- Notes
      Section_Unknown
   );

   --  Section descriptor
   type Section_Desc is record
      Name_Offset : Word32;
      Sec_Type    : Section_Type;
      Flags       : Word64;
      Address     : Word64;
      Offset      : Word64;
      Size        : Word64;
      Align       : Word64;
   end record;

   --  Maximum sections per ELF
   Max_Sections : constant := 32;
   type Section_Index is range 0 .. Max_Sections - 1;
   type Section_Table is array (Section_Index) of Section_Desc;

   ---------------------------------------------------------------------------
   --  Loaded Contract Types
   ---------------------------------------------------------------------------

   --  Contract entry point type
   type Entry_Point is new Word64;

   --  Loaded contract descriptor
   type Loaded_Contract is record
      --  Binary info
      Code_Base    : Word64;          -- Base address of code section
      Code_Size    : Word64;          -- Size of code section
      Data_Base    : Word64;          -- Base address of data section
      Data_Size    : Word64;          -- Size of data section
      BSS_Base     : Word64;          -- Base of uninitialized data
      BSS_Size     : Word64;          -- Size of BSS

      --  Entry point
      Entry_Addr   : Entry_Point;     -- Contract entry function

      --  Architecture
      Arch         : Target_Arch;     -- Target architecture
      Class        : ELF_Class;       -- 32 or 64 bit

      --  Validation
      Is_Valid     : Boolean;         -- Successfully loaded
      Has_Proof    : Boolean;         -- SPARK proof artifact present
      WCET_Bound   : Gas_Amount;      -- Proven worst-case gas
   end record;

   --  Empty/invalid contract
   Invalid_Contract : constant Loaded_Contract := (
      Code_Base  => 0,
      Code_Size  => 0,
      Data_Base  => 0,
      Data_Size  => 0,
      BSS_Base   => 0,
      BSS_Size   => 0,
      Entry_Addr => 0,
      Arch       => Arch_ARM64,
      Class      => ELF_64,
      Is_Valid   => False,
      Has_Proof  => False,
      WCET_Bound => 0
   );

   ---------------------------------------------------------------------------
   --  Load Result Types
   ---------------------------------------------------------------------------

   type Load_Error is (
      Load_Success,
      Load_Invalid_Magic,         -- Not an ELF file
      Load_Unsupported_Class,     -- Wrong ELF class
      Load_Unsupported_Arch,      -- Unsupported architecture
      Load_Too_Large,             -- Exceeds Max_Binary_Size
      Load_Invalid_Sections,      -- Malformed section headers
      Load_Missing_Entry,         -- No entry point found
      Load_Security_Violation,    -- W^X or other security issue
      Load_Memory_Error           -- Failed to allocate memory
   );

   type Load_Result is record
      Error    : Load_Error;
      Contract : Loaded_Contract;
   end record;

   ---------------------------------------------------------------------------
   --  ELF Loading
   ---------------------------------------------------------------------------

   --  Load ELF binary from buffer
   function Load_ELF (
      Binary : Binary_Buffer;
      Size   : Natural
   ) return Load_Result with
      Global => null,
      Pre    => Size <= Max_Binary_Size;

   --  Validate ELF header
   function Validate_Header (
      Binary : Binary_Buffer;
      Size   : Natural
   ) return Load_Error with
      Global => null;

   --  Extract entry point from ELF
   function Get_Entry_Point (
      Binary : Binary_Buffer;
      Size   : Natural
   ) return Entry_Point with
      Global => null;

   --  Get target architecture from ELF
   function Get_Architecture (
      Binary : Binary_Buffer
   ) return Target_Arch with
      Global => null;

   ---------------------------------------------------------------------------
   --  Contract Execution
   ---------------------------------------------------------------------------

   --  Execution result
   type Exec_Status is (
      Exec_Success,
      Exec_Reverted,
      Exec_Out_Of_Gas,
      Exec_Security_Violation,
      Exec_Invalid_Syscall,
      Exec_Stack_Overflow,
      Exec_Segmentation_Fault,
      Exec_Illegal_Instruction,
      Exec_Timeout
   );

   type Exec_Result is record
      Status       : Exec_Status;
      Gas_Used     : Gas_Amount;
      Return_Data  : Hash256;   -- Hash of return data
   end record;

   --  Execute loaded contract
   function Execute (
      Contract    : Loaded_Contract;
      Sandbox     : Aegis_Sandbox.Sandbox_Context;
      Calldata    : Byte_Array;
      Gas_Limit   : Gas_Amount
   ) return Exec_Result with
      Global => null,
      Pre    => Contract.Is_Valid and Calldata'Length <= 65536;

   --  Execute with specific function selector
   function Execute_Function (
      Contract    : Loaded_Contract;
      Sandbox     : Aegis_Sandbox.Sandbox_Context;
      Selector    : Word32;
      Args        : Byte_Array;
      Gas_Limit   : Gas_Amount
   ) return Exec_Result with
      Global => null,
      Pre    => Contract.Is_Valid and Args'Length <= 65536;

   ---------------------------------------------------------------------------
   --  Security Validation
   ---------------------------------------------------------------------------

   --  Validate contract security properties
   type Security_Check is (
      Check_WXE,           -- Write XOR Execute
      Check_Stack_Guard,   -- Stack canary present
      Check_No_Absolute,   -- No absolute addresses
      Check_Bounded_Stack, -- Stack usage bounded
      Check_No_Syscalls,   -- No direct syscalls (all via runtime)
      Check_Valid_GOT,     -- Global offset table valid
      Check_Valid_PLT      -- Procedure linkage table valid
   );

   type Security_Result is array (Security_Check) of Boolean;

   function Validate_Security (
      Contract : Loaded_Contract
   ) return Security_Result with
      Global => null;

   --  Check if all security checks pass
   function Is_Secure (Checks : Security_Result) return Boolean with
      Global => null,
      Post   => Is_Secure'Result = (for all C in Security_Check => Checks (C));

   ---------------------------------------------------------------------------
   --  WCET Integration
   ---------------------------------------------------------------------------

   --  WCET analysis result
   type WCET_Result is record
      Valid      : Boolean;      -- Analysis succeeded
      Cycles     : Natural;      -- Worst-case cycles
      Gas_Bound  : Gas_Amount;   -- Derived gas bound
      Call_Depth : Natural;      -- Maximum call depth
   end record;

   --  Analyze WCET for function
   function Analyze_WCET (
      Contract : Loaded_Contract;
      Function_Entry : Entry_Point
   ) return WCET_Result with
      Global => null;

   --  Get WCET annotation from proof artifact
   function Get_WCET_Annotation (
      Contract : Loaded_Contract
   ) return WCET_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Memory Layout
   ---------------------------------------------------------------------------

   --  Memory region types for contract execution
   type Region_Type is (
      Region_Code,     -- Executable code (R-X)
      Region_Data,     -- Initialized data (RW-)
      Region_BSS,      -- Uninitialized data (RW-)
      Region_Stack,    -- Contract stack (RW-)
      Region_Heap,     -- Contract heap (RW-)
      Region_Guard     -- Guard page (---)
   );

   --  Memory region descriptor
   type Memory_Region_Desc is record
      Base       : Word64;
      Size       : Word64;
      Reg_Type   : Region_Type;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean;
   end record;

   --  Maximum regions per contract
   Max_Regions : constant := 16;
   type Region_Index is range 0 .. Max_Regions - 1;
   type Region_Layout is array (Region_Index) of Memory_Region_Desc;

   --  Create memory layout for contract
   function Create_Layout (
      Contract : Loaded_Contract
   ) return Region_Layout with
      Global => null;

   ---------------------------------------------------------------------------
   --  Debug Support
   ---------------------------------------------------------------------------

   --  Symbol table entry (for debugging)
   type Symbol_Entry is record
      Name_Offset : Word32;
      Value       : Word64;
      Size        : Word64;
      Sym_Type    : Byte;
      Binding     : Byte;
   end record;

   --  Get symbol by address (for stack traces)
   function Lookup_Symbol (
      Contract : Loaded_Contract;
      Address  : Word64
   ) return Symbol_Entry with
      Global => null;

   --  Disassemble instruction at address
   function Disassemble (
      Contract : Loaded_Contract;
      Address  : Word64
   ) return String with
      Global => null;

private

   --  Internal ELF parsing helpers

   --  Read little-endian 16-bit value
   function Read_LE16 (
      Data   : Binary_Buffer;
      Offset : Natural
   ) return Word32 with
      Global => null,
      Pre    => Offset < Max_Binary_Size - 1;

   --  Read little-endian 32-bit value
   function Read_LE32 (
      Data   : Binary_Buffer;
      Offset : Natural
   ) return Word32 with
      Global => null,
      Pre    => Offset < Max_Binary_Size - 3;

   --  Read little-endian 64-bit value
   function Read_LE64 (
      Data   : Binary_Buffer;
      Offset : Natural
   ) return Word64 with
      Global => null,
      Pre    => Offset < Max_Binary_Size - 7;

end Sphinx_Native;
