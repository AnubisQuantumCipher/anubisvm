pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Sandbox;
with Anubis_Types;
with Anubis_SHA3;
with Anubis_Address_Types;

--  SPHINX Native: Native ELF Execution Engine
--
--  EXECUTION MODEL:
--  ================
--  This package implements native contract execution using ELF binaries.
--  Contracts are loaded via mmap and executed directly on the host CPU.
--
--  Supported Architectures:
--  ------------------------
--    - ARM64 (AArch64) - Full native execution support
--    - x86-64 (AMD64) - Full native execution support
--
--  Execution Flow:
--  ---------------
--    1. Parse ELF headers and validate structure
--    2. Map segments with appropriate permissions (mmap/mprotect)
--    3. Apply relocations for position-independent code
--    4. Execute entry point with runtime context
--    5. Handle syscalls via Aegis_Syscall dispatcher
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8: SPHINX Execution Model
--  - ELF Specification (Tool Interface Standard)

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

   --  Target architecture for native ELF execution
   type Target_Arch is (
      Arch_ARM64,   -- AArch64 (native execution)
      Arch_X86_64   -- AMD64 (native execution)
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
      Arch       => Arch_ARM64,    -- Default to ARM64
      Class      => ELF_64,        -- Default to 64-bit ELF
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
      Load_Unsupported_Class,     -- Wrong ELF class (must be ELF32 for RV32)
      Load_Unsupported_Arch,      -- Not RISC-V 32-bit
      Load_Too_Large,             -- Exceeds Max_Binary_Size
      Load_Invalid_Sections,      -- Malformed section headers
      Load_Missing_Entry,         -- No entry point found
      Load_Security_Violation,    -- Malformed binary
      Load_Memory_Error,          -- Failed to allocate memory
      Load_Not_RISCV32            -- Binary is not RISC-V 32-bit (required)
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
   --  Contract Execution (via RISC-V Interpreter)
   ---------------------------------------------------------------------------

   --  Execution result
   --  NOTE: Execution uses Sphinx_RISCV interpreter, NOT native host execution
   type Exec_Status is (
      Exec_Success,           -- Contract halted cleanly (EBREAK)
      Exec_Reverted,          -- Contract called Sys_Revert
      Exec_Out_Of_Gas,        -- Gas limit exceeded
      Exec_Security_Violation, -- Invalid syscall or capability violation
      Exec_Invalid_Syscall,   -- Unknown syscall number
      Exec_Stack_Overflow,    -- Guest stack overflow
      Exec_Segmentation_Fault, -- Guest memory access violation
      Exec_Illegal_Instruction, -- Invalid RISC-V instruction
      Exec_Timeout            -- Execution took too long (if timeout enabled)
   );

   type Exec_Result is record
      Status       : Exec_Status;
      Gas_Used     : Gas_Amount;
      Return_Data  : Hash256;   -- Hash of return data
   end record;

   --  Execute loaded contract via RISC-V interpreter
   --  SECURITY: Contract code is executed by Sphinx_RISCV interpreter.
   --  No native host execution occurs. All instructions are interpreted.
   function Execute (
      Contract    : Loaded_Contract;
      Sandbox     : Aegis_Sandbox.Sandbox_Context;
      Calldata    : Byte_Array;
      Gas_Limit   : Gas_Amount;
      Gas_Price   : Word64;
      Block_Num   : Word64;
      Timestamp   : Word64;
      Chain_ID    : Word64
   ) return Exec_Result with
      Global => null,
      Pre    => Contract.Is_Valid
                and then (Contract.Arch = Arch_ARM64 or Contract.Arch = Arch_X86_64)
                and then Calldata'Length <= 65536;

   --  Execute with specific function selector via native execution
   function Execute_Function (
      Contract    : Loaded_Contract;
      Sandbox     : Aegis_Sandbox.Sandbox_Context;
      Selector    : Word32;
      Args        : Byte_Array;
      Gas_Limit   : Gas_Amount;
      Gas_Price   : Word64;
      Block_Num   : Word64;
      Timestamp   : Word64;
      Chain_ID    : Word64
   ) return Exec_Result with
      Global => null,
      Pre    => Contract.Is_Valid
                and then (Contract.Arch = Arch_ARM64 or Contract.Arch = Arch_X86_64)
                and then Args'Length <= 65536;

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

   ---------------------------------------------------------------------------
   --  Contract Library Management (for native execution)
   ---------------------------------------------------------------------------

   --  Load and register a contract library from path
   procedure Load_Contract_From_Path (
      Path     : in     String;
      Code_ID  : in     Word64;
      Success  : out    Boolean
   );

   --  Execute a registered contract by code ID
   procedure Execute_Registered (
      Code_ID     : in     Word64;
      Calldata    : in     Byte_Array;
      Gas_Limit   : in     Gas_Amount;
      Return_Data :    out Byte_Array;
      Return_Len  :    out Natural;
      Gas_Used    :    out Gas_Amount;
      Status      :    out Exec_Status
   );

   ---------------------------------------------------------------------------
   --  Runtime Configuration (Block Context)
   ---------------------------------------------------------------------------
   --
   --  CRITICAL SECURITY: Node operators MUST call Set_Block_Context before
   --  executing any contracts. Failure to do so will result in execution
   --  failure and security violations.
   --
   --  The runtime configuration includes:
   --  - Gas price (for transaction cost calculation)
   --  - Block number and timestamp (for time-dependent contracts)
   --  - Chain ID (prevents replay attacks across chains)
   --  - Coinbase, gas limit, base fee (EIP-1559 support)

   type Runtime_Config is record
      Gas_Price   : Word64;  --  Wei per gas unit
      Block_Num   : Word64;  --  Current block number
      Timestamp   : Word64;  --  Block timestamp (Unix epoch)
      Chain_ID    : Word64;  --  Chain identifier (prevents replay attacks)
      Coinbase    : Anubis_Address_Types.Account_ID;  --  Block miner/validator
      Gas_Limit   : Word64;  --  Block gas limit
      Base_Fee    : Word64;  --  EIP-1559 base fee per gas
      Difficulty  : Word64;  --  Block difficulty (PoW) or 0 (PoS)
   end record;

   --  Set block context before contract execution
   --  MUST be called by node with each new block or before transaction execution
   procedure Set_Block_Context (Config : Runtime_Config);

   --  Get current block context
   function Get_Block_Context return Runtime_Config;

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
