pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;

--  AEGIS VM Memory: Contract Memory Management
--
--  This package provides the memory model for contract execution.
--  Each contract gets an isolated memory space with:
--  - Code segment (R-X)
--  - Data segment (RW-)
--  - Stack (RW-)
--  - Heap (RW-)
--  - Calldata buffer (R--)
--  - Return data buffer (RW-)
--
--  Memory Layout (per contract):
--  +------------------+ 0x0000_0000
--  |    Code (R-X)    |
--  +------------------+ code_end
--  |    Data (RW-)    |
--  +------------------+ data_end
--  |    BSS (RW-)     |
--  +------------------+ bss_end
--  |   Guard Page     |
--  +------------------+
--  |   Stack (RW-)    | (grows down)
--  +------------------+ stack_base
--  |   Guard Page     |
--  +------------------+
--  |   Heap (RW-)     | (grows up)
--  +------------------+ heap_limit
--  |  Calldata (R--)  |
--  +------------------+
--  | Return Data(RW-) |
--  +------------------+
--
--  SPARK Verification: Gold
--  - All memory access bounds checked
--  - No buffer overflows
--  - W^X enforcement proven

package Aegis_VM_Memory with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Memory Constants
   ---------------------------------------------------------------------------

   --  Maximum contract memory size (1 MB per contract)
   Max_Contract_Memory : constant := 1024 * 1024;

   --  Maximum code size (24 KB per KHEPRI spec)
   Max_Code_Size : constant := 24 * 1024;

   --  Maximum stack size (64 KB)
   Max_Stack_Size : constant := 64 * 1024;

   --  Maximum heap size (256 KB)
   Max_Heap_Size : constant := 256 * 1024;

   --  Maximum calldata size (64 KB)
   Max_Calldata_Size : constant := 64 * 1024;

   --  Maximum return data size (64 KB)
   Max_Return_Data_Size : constant := 64 * 1024;

   --  Guard page size
   Guard_Page_Size : constant := 4096;

   --  Word size for memory operations
   Word_Size : constant := 32;

   ---------------------------------------------------------------------------
   --  Memory Types
   ---------------------------------------------------------------------------

   --  Memory address within contract
   subtype VM_Address is Word64 range 0 .. Word64 (Max_Contract_Memory - 1);

   --  Memory region permissions
   type Memory_Perm is record
      Read    : Boolean;
      Write   : Boolean;
      Execute : Boolean;
   end record;

   Perm_None : constant Memory_Perm := (False, False, False);
   Perm_RX   : constant Memory_Perm := (True, False, True);   -- Code
   Perm_RW   : constant Memory_Perm := (True, True, False);   -- Data/Stack/Heap
   Perm_R    : constant Memory_Perm := (True, False, False);  -- Calldata

   --  Memory region descriptor
   type Memory_Region is record
      Base  : VM_Address;
      Size  : Natural;
      Perm  : Memory_Perm;
      Valid : Boolean;
   end record;

   --  Invalid region
   Invalid_Region : constant Memory_Region := (
      Base  => 0,
      Size  => 0,
      Perm  => Perm_None,
      Valid => False
   );

   --  Memory region types
   type Region_Kind is (
      Region_Code,
      Region_Data,
      Region_BSS,
      Region_Stack,
      Region_Heap,
      Region_Calldata,
      Region_Return_Data,
      Region_Guard
   );

   --  Number of regions per contract
   Num_Regions : constant := 8;

   type Region_Index is range 0 .. Num_Regions - 1;
   type Region_Array is array (Region_Kind) of Memory_Region;

   ---------------------------------------------------------------------------
   --  Memory Buffer Types
   ---------------------------------------------------------------------------

   --  Memory buffer for contract
   subtype Memory_Index is Natural range 0 .. Max_Contract_Memory - 1;
   type Memory_Buffer is array (Memory_Index) of Byte;

   --  Calldata buffer
   subtype Calldata_Index is Natural range 0 .. Max_Calldata_Size - 1;
   type Calldata_Buffer is array (Calldata_Index) of Byte;

   --  Return data buffer
   subtype Return_Index is Natural range 0 .. Max_Return_Data_Size - 1;
   type Return_Buffer is array (Return_Index) of Byte;

   ---------------------------------------------------------------------------
   --  VM Memory Context
   ---------------------------------------------------------------------------

   --  Contract memory state
   type VM_Memory is record
      --  Memory buffer (allocated externally)
      Memory_Valid : Boolean;

      --  Regions
      Regions : Region_Array;

      --  Stack pointer (grows down from stack top)
      Stack_Pointer : VM_Address;

      --  Heap pointer (grows up from heap base)
      Heap_Pointer : VM_Address;

      --  Calldata
      Calldata      : Calldata_Buffer;
      Calldata_Size : Natural;

      --  Return data from last call
      Return_Data      : Return_Buffer;
      Return_Data_Size : Natural;

      --  Memory expansion tracking
      Memory_Size : Natural;
   end record;

   --  Empty memory
   Empty_Memory : constant VM_Memory;

   ---------------------------------------------------------------------------
   --  Memory Initialization
   ---------------------------------------------------------------------------

   --  Initialize memory for contract execution
   procedure Initialize_Memory (
      Mem        : out VM_Memory;
      Code_Size  : in  Natural;
      Data_Size  : in  Natural;
      BSS_Size   : in  Natural;
      Success    : out Boolean
   ) with
      Global => null,
      Pre    => Code_Size <= Max_Code_Size and
                Data_Size <= Max_Contract_Memory - Max_Code_Size and
                BSS_Size <= Max_Contract_Memory - Max_Code_Size - Data_Size,
      Post   => (if Success then Mem.Memory_Valid);

   --  Free memory
   procedure Free_Memory (
      Mem : in Out VM_Memory
   ) with
      Global => null,
      Post   => not Mem.Memory_Valid;

   ---------------------------------------------------------------------------
   --  Memory Access (SPARK verified bounds)
   ---------------------------------------------------------------------------

   --  Load byte from memory
   function Load_Byte (
      Mem     : VM_Memory;
      Address : VM_Address
   ) return Byte with
      Global => null,
      Pre    => Mem.Memory_Valid and Address < VM_Address (Mem.Memory_Size);

   --  Store byte to memory
   procedure Store_Byte (
      Mem     : in Out VM_Memory;
      Address : in     VM_Address;
      Value   : in     Byte;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid;

   --  Load word (32 bytes) from memory
   function Load_Word (
      Mem     : VM_Memory;
      Address : VM_Address
   ) return U256 with
      Global => null,
      Pre    => Mem.Memory_Valid and
                Address <= VM_Address (Mem.Memory_Size - Word_Size);

   --  Store word (32 bytes) to memory
   procedure Store_Word (
      Mem     : in Out VM_Memory;
      Address : in     VM_Address;
      Value   : in     U256;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid;

   --  Load bytes from memory
   procedure Load_Bytes (
      Mem     : in     VM_Memory;
      Address : in     VM_Address;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid and
                Buffer'Length >= Size and
                Size <= Max_Contract_Memory;

   --  Store bytes to memory
   procedure Store_Bytes (
      Mem     : in Out VM_Memory;
      Address : in     VM_Address;
      Data    : in     Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid and
                Data'Length <= Max_Contract_Memory;

   ---------------------------------------------------------------------------
   --  Calldata Operations
   ---------------------------------------------------------------------------

   --  Set calldata for contract execution
   procedure Set_Calldata (
      Mem  : in Out VM_Memory;
      Data : in     Byte_Array
   ) with
      Global => null,
      Pre    => Data'Length <= Max_Calldata_Size,
      Post   => Mem.Calldata_Size = Data'Length;

   --  Get calldata size
   function Get_Calldata_Size (Mem : VM_Memory) return Natural with
      Global => null,
      Post   => Get_Calldata_Size'Result <= Max_Calldata_Size;

   --  Load calldata bytes
   procedure Load_Calldata (
      Mem     : in     VM_Memory;
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Buffer'Length >= Size;

   --  Copy calldata to memory
   procedure Calldata_Copy (
      Mem         : in Out VM_Memory;
      Dest_Offset : in     VM_Address;
      Data_Offset : in     Natural;
      Size        : in     Natural;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid;

   ---------------------------------------------------------------------------
   --  Return Data Operations
   ---------------------------------------------------------------------------

   --  Set return data from last call
   procedure Set_Return_Data (
      Mem  : in Out VM_Memory;
      Data : in     Byte_Array
   ) with
      Global => null,
      Pre    => Data'Length <= Max_Return_Data_Size,
      Post   => Mem.Return_Data_Size = Data'Length;

   --  Get return data size
   function Get_Return_Data_Size (Mem : VM_Memory) return Natural with
      Global => null,
      Post   => Get_Return_Data_Size'Result <= Max_Return_Data_Size;

   --  Load return data bytes
   procedure Load_Return_Data (
      Mem     : in     VM_Memory;
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Buffer'Length >= Size;

   --  Copy return data to memory
   procedure Return_Data_Copy (
      Mem         : in Out VM_Memory;
      Dest_Offset : in     VM_Address;
      Data_Offset : in     Natural;
      Size        : in     Natural;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid;

   ---------------------------------------------------------------------------
   --  Stack Operations
   ---------------------------------------------------------------------------

   --  Push bytes to stack
   procedure Stack_Push (
      Mem     : in Out VM_Memory;
      Data    : in     Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid and Data'Length <= Max_Stack_Size;

   --  Pop bytes from stack
   procedure Stack_Pop (
      Mem     : in Out VM_Memory;
      Size    : in     Natural;
      Data    : out    Byte_Array;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid and Data'Length >= Size;

   --  Get current stack usage
   function Stack_Usage (Mem : VM_Memory) return Natural with
      Global => null;

   ---------------------------------------------------------------------------
   --  Heap Operations
   ---------------------------------------------------------------------------

   --  Allocate heap memory
   procedure Heap_Alloc (
      Mem     : in Out VM_Memory;
      Size    : in     Natural;
      Address : out    VM_Address;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid and Size <= Max_Heap_Size;

   --  Get current heap usage
   function Heap_Usage (Mem : VM_Memory) return Natural with
      Global => null;

   ---------------------------------------------------------------------------
   --  Memory Expansion
   ---------------------------------------------------------------------------

   --  Expand memory to accommodate address (gas cost calculation)
   procedure Expand_Memory (
      Mem      : in Out VM_Memory;
      Address  : in     VM_Address;
      Size     : in     Natural;
      Gas_Cost : out    Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre    => Mem.Memory_Valid;

   --  Calculate memory expansion gas cost
   function Memory_Gas_Cost (
      Current_Size : Natural;
      Target_Size  : Natural
   ) return Gas_Amount with
      Global => null;

   ---------------------------------------------------------------------------
   --  Permission Checking
   ---------------------------------------------------------------------------

   --  Check if address is readable
   function Is_Readable (
      Mem     : VM_Memory;
      Address : VM_Address;
      Size    : Natural
   ) return Boolean with
      Global => null;

   --  Check if address is writable
   function Is_Writable (
      Mem     : VM_Memory;
      Address : VM_Address;
      Size    : Natural
   ) return Boolean with
      Global => null;

   --  Check if address is executable
   function Is_Executable (
      Mem     : VM_Memory;
      Address : VM_Address
   ) return Boolean with
      Global => null;

   --  Validate W^X (write XOR execute)
   function WXE_Valid (Mem : VM_Memory) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Memory Hashing (for state commitments)
   ---------------------------------------------------------------------------

   --  Hash memory region
   function Hash_Region (
      Mem   : VM_Memory;
      Start : VM_Address;
      Size  : Natural
   ) return Hash256 with
      Global => null,
      Pre    => Mem.Memory_Valid;

   --  Hash all writable memory
   function Hash_State (Mem : VM_Memory) return Hash256 with
      Global => null,
      Pre    => Mem.Memory_Valid;

private

   Empty_Memory : constant VM_Memory := (
      Memory_Valid     => False,
      Regions          => (others => Invalid_Region),
      Stack_Pointer    => 0,
      Heap_Pointer     => 0,
      Calldata         => (others => 0),
      Calldata_Size    => 0,
      Return_Data      => (others => 0),
      Return_Data_Size => 0,
      Memory_Size      => 0
   );

end Aegis_VM_Memory;
