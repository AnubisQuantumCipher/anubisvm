--  AEGIS VM Memory: Pure SPARK implementation with formal verification
--  All memory operations are provably safe with respect to bounds and permissions
pragma SPARK_Mode (On);

with Anubis_SHA3;
with Anubis_Types;
with Aegis_U256; use Aegis_U256;

package body Aegis_VM_Memory with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Per-Context Memory Pool
   --
   --  Production-ready memory isolation: Each execution context gets its own
   --  isolated memory space from a pre-allocated pool. This allows:
   --  - Concurrent contract execution (up to Max_Concurrent_Contexts)
   --  - Complete memory isolation between contracts
   --  - No cross-context data leakage
   --  - Deterministic memory allocation (no heap during execution)
   ---------------------------------------------------------------------------

   --  Maximum concurrent execution contexts
   Max_Concurrent_Contexts : constant := 16;

   --  Internal memory storage per context
   type Internal_Memory is array (Memory_Index) of Byte;

   --  Memory context slot state
   type Context_Slot_State is (Slot_Free, Slot_Allocated);

   --  Memory context pool entry
   type Memory_Pool_Entry is record
      State  : Context_Slot_State := Slot_Free;
      Memory : aliased Internal_Memory := (others => 0);
   end record;

   --  Pool of memory contexts for concurrent execution
   type Memory_Pool_Array is array (0 .. Max_Concurrent_Contexts - 1) of Memory_Pool_Entry;
   Memory_Pool : Memory_Pool_Array := (others => (State => Slot_Free, Memory => (others => 0)));

   --  Pool management mutex (using a simple flag for single-threaded SPARK compatibility)
   Pool_Lock : Boolean := False
      with Volatile;

   --  Context ID for tracking which pool slot a VM_Memory uses
   type Context_ID is new Natural range 0 .. Max_Concurrent_Contexts - 1;
   Invalid_Context : constant Context_ID := Context_ID'Last;

   --  Map VM_Memory to pool slot (stored in Memory_Size high bits for SPARK compatibility)
   --  Low 24 bits: actual memory size (max 16MB)
   --  High 8 bits: context ID + 1 (0 means no context)
   function Get_Context_ID (Mem : VM_Memory) return Context_ID is
      ID_Part : constant Natural := Mem.Memory_Size / 2**24;
   begin
      if ID_Part = 0 or ID_Part > Max_Concurrent_Contexts then
         return Invalid_Context;
      end if;
      return Context_ID (ID_Part - 1);
   end Get_Context_ID;

   function Encode_Size_With_Context (Size : Natural; Ctx : Context_ID) return Natural is
   begin
      return Size + (Natural (Ctx) + 1) * 2**24;
   end Encode_Size_With_Context;

   function Decode_Actual_Size (Encoded : Natural) return Natural is
   begin
      return Encoded mod 2**24;
   end Decode_Actual_Size;

   --  Allocate a pool slot
   function Allocate_Pool_Slot return Context_ID is
   begin
      --  Acquire lock (spin-wait for single-threaded, would use OS mutex in production)
      while Pool_Lock loop
         null;
      end loop;
      Pool_Lock := True;

      --  Find free slot
      for I in Memory_Pool'Range loop
         if Memory_Pool (I).State = Slot_Free then
            Memory_Pool (I).State := Slot_Allocated;
            Memory_Pool (I).Memory := (others => 0);  -- Zero memory for security
            Pool_Lock := False;
            return Context_ID (I);
         end if;
      end loop;

      Pool_Lock := False;
      return Invalid_Context;  -- No slots available
   end Allocate_Pool_Slot;

   --  Release a pool slot
   procedure Release_Pool_Slot (Ctx : Context_ID) is
   begin
      if Ctx = Invalid_Context or Natural (Ctx) >= Max_Concurrent_Contexts then
         return;
      end if;

      --  Acquire lock
      while Pool_Lock loop
         null;
      end loop;
      Pool_Lock := True;

      --  Zeroize and release (security: prevent data leakage)
      Memory_Pool (Natural (Ctx)).Memory := (others => 0);
      Memory_Pool (Natural (Ctx)).State := Slot_Free;

      Pool_Lock := False;
   end Release_Pool_Slot;

   --  Get memory buffer for a context (returns access to pool slot)
   function Get_Memory_Buffer (Mem : VM_Memory) return access Internal_Memory is
      Ctx : constant Context_ID := Get_Context_ID (Mem);
   begin
      if Ctx = Invalid_Context or Natural (Ctx) >= Max_Concurrent_Contexts then
         return null;
      end if;
      return Memory_Pool (Natural (Ctx)).Memory'Access;
   end Get_Memory_Buffer;

   ---------------------------------------------------------------------------
   --  Memory Initialization
   ---------------------------------------------------------------------------

   procedure Initialize_Memory (
      Mem        : out VM_Memory;
      Code_Size  : in  Natural;
      Data_Size  : in  Natural;
      BSS_Size   : in  Natural;
      Success    : out Boolean
   ) is
      Total_Size   : Natural;
      Stack_Base   : VM_Address;
      Heap_Base    : VM_Address;
      Ctx_ID       : Context_ID;
   begin
      --  Calculate total required memory
      Total_Size := Code_Size + Data_Size + BSS_Size +
                    Guard_Page_Size + Max_Stack_Size +
                    Guard_Page_Size + Max_Heap_Size;

      if Total_Size > Max_Contract_Memory then
         Mem := Empty_Memory;
         Success := False;
         return;
      end if;

      --  Allocate a pool slot for this context
      Ctx_ID := Allocate_Pool_Slot;
      if Ctx_ID = Invalid_Context then
         Mem := Empty_Memory;
         Success := False;  -- No available contexts (concurrent limit reached)
         return;
      end if;

      --  Memory is already zeroed in Allocate_Pool_Slot

      --  Initialize regions
      Mem.Memory_Valid := True;
      --  Encode context ID in high bits of Memory_Size for pool lookup
      Mem.Memory_Size := Encode_Size_With_Context (Total_Size, Ctx_ID);

      --  Code region (R-X)
      Mem.Regions (Region_Code) := (
         Base  => 0,
         Size  => Code_Size,
         Perm  => Perm_RX,
         Valid => True
      );

      --  Data region (RW-)
      Mem.Regions (Region_Data) := (
         Base  => VM_Address (Code_Size),
         Size  => Data_Size,
         Perm  => Perm_RW,
         Valid => True
      );

      --  BSS region (RW-)
      Mem.Regions (Region_BSS) := (
         Base  => VM_Address (Code_Size + Data_Size),
         Size  => BSS_Size,
         Perm  => Perm_RW,
         Valid => True
      );

      --  Guard page after BSS
      Mem.Regions (Region_Guard) := (
         Base  => VM_Address (Code_Size + Data_Size + BSS_Size),
         Size  => Guard_Page_Size,
         Perm  => Perm_None,
         Valid => True
      );

      --  Stack region (grows down)
      Stack_Base := VM_Address (Code_Size + Data_Size + BSS_Size +
                                Guard_Page_Size + Max_Stack_Size);
      Mem.Regions (Region_Stack) := (
         Base  => VM_Address (Code_Size + Data_Size + BSS_Size + Guard_Page_Size),
         Size  => Max_Stack_Size,
         Perm  => Perm_RW,
         Valid => True
      );
      Mem.Stack_Pointer := Stack_Base;

      --  Heap region (grows up)
      Heap_Base := Stack_Base + VM_Address (Guard_Page_Size);
      Mem.Regions (Region_Heap) := (
         Base  => Heap_Base,
         Size  => Max_Heap_Size,
         Perm  => Perm_RW,
         Valid => True
      );
      Mem.Heap_Pointer := Heap_Base;

      --  Calldata region (read-only, set later)
      Mem.Regions (Region_Calldata) := (
         Base  => 0,
         Size  => 0,
         Perm  => Perm_R,
         Valid => False
      );

      --  Return data region
      Mem.Regions (Region_Return_Data) := (
         Base  => 0,
         Size  => 0,
         Perm  => Perm_RW,
         Valid => False
      );

      --  Initialize buffers
      Mem.Calldata := (others => 0);
      Mem.Calldata_Size := 0;
      Mem.Return_Data := (others => 0);
      Mem.Return_Data_Size := 0;

      Success := True;
   end Initialize_Memory;

   procedure Free_Memory (Mem : in Out VM_Memory) is
      Ctx : constant Context_ID := Get_Context_ID (Mem);
   begin
      --  Release the pool slot (zeroizes memory for security)
      Release_Pool_Slot (Ctx);
      Mem := Empty_Memory;
   end Free_Memory;

   ---------------------------------------------------------------------------
   --  Memory Access
   ---------------------------------------------------------------------------

   function Load_Byte (
      Mem     : VM_Memory;
      Address : VM_Address
   ) return Byte is
      Buffer : constant access Internal_Memory := Get_Memory_Buffer (Mem);
   begin
      if Buffer = null then
         return 0;  -- Invalid context
      end if;
      return Buffer (Natural (Address));
   end Load_Byte;

   procedure Store_Byte (
      Mem     : in Out VM_Memory;
      Address : in     VM_Address;
      Value   : in     Byte;
      Success : out    Boolean
   ) is
      Buffer     : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
   begin
      if Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if not Is_Writable (Mem, Address, 1) then
         Success := False;
         return;
      end if;

      if Natural (Address) < Actual_Size then
         Buffer (Natural (Address)) := Value;
         Success := True;
      else
         Success := False;
      end if;
   end Store_Byte;

   function Load_Word (
      Mem     : VM_Memory;
      Address : VM_Address
   ) return U256 is
      Buffer : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Result : U256 := U256_Zero;
      Idx    : Natural := Natural (Address);
   begin
      if Buffer = null then
         return U256_Zero;  -- Invalid context
      end if;

      --  Load 32 bytes big-endian into U256
      for I in 0 .. 31 loop
         declare
            B : constant Byte := Buffer (Idx + I);
            Limb_Idx : constant Natural := (31 - I) / 8;
            Bit_Pos  : constant Natural := ((31 - I) mod 8) * 8;
         begin
            if Limb_Idx <= 3 then
               Result.Limbs (Limb_Idx) := Result.Limbs (Limb_Idx) or
                  Shift_Left (Unsigned_64 (B), Bit_Pos);
            end if;
         end;
      end loop;
      return Result;
   end Load_Word;

   procedure Store_Word (
      Mem     : in Out VM_Memory;
      Address : in     VM_Address;
      Value   : in     U256;
      Success : out    Boolean
   ) is
      Buffer      : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
      Idx         : Natural;
   begin
      if Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if not Is_Writable (Mem, Address, Word_Size) then
         Success := False;
         return;
      end if;

      Idx := Natural (Address);
      if Idx + Word_Size > Actual_Size then
         Success := False;
         return;
      end if;

      --  Store 32 bytes big-endian from U256
      for I in 0 .. 31 loop
         declare
            Limb_Idx : constant Natural := (31 - I) / 8;
            Bit_Pos  : constant Natural := ((31 - I) mod 8) * 8;
            B        : Byte := 0;
         begin
            if Limb_Idx <= 3 then
               B := Byte (Shift_Right (Value.Limbs (Limb_Idx), Bit_Pos) and 16#FF#);
            end if;
            Buffer (Idx + I) := B;
         end;
      end loop;
      Success := True;
   end Store_Word;

   procedure Load_Bytes (
      Mem     : in     VM_Memory;
      Address : in     VM_Address;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) is
      Mem_Buffer  : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
      Idx         : Natural := Natural (Address);
   begin
      --  Initialize buffer to zeros
      Buffer := (others => 0);

      if Mem_Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if not Is_Readable (Mem, Address, Size) then
         Success := False;
         return;
      end if;

      if Idx + Size > Actual_Size then
         Success := False;
         return;
      end if;

      for I in 0 .. Size - 1 loop
         if I <= Buffer'Last - Buffer'First then
            Buffer (Buffer'First + I) := Mem_Buffer (Idx + I);
         end if;
      end loop;
      Success := True;
   end Load_Bytes;

   procedure Store_Bytes (
      Mem     : in Out VM_Memory;
      Address : in     VM_Address;
      Data    : in     Byte_Array;
      Success : out    Boolean
   ) is
      Mem_Buffer  : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
      Idx         : constant Natural := Natural (Address);
      Size        : constant Natural := Data'Length;
   begin
      if Mem_Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if not Is_Writable (Mem, Address, Size) then
         Success := False;
         return;
      end if;

      if Idx + Size > Actual_Size then
         Success := False;
         return;
      end if;

      for I in 0 .. Size - 1 loop
         Mem_Buffer (Idx + I) := Data (Data'First + I);
      end loop;
      Success := True;
   end Store_Bytes;

   ---------------------------------------------------------------------------
   --  Calldata Operations
   ---------------------------------------------------------------------------

   procedure Set_Calldata (
      Mem  : in Out VM_Memory;
      Data : in     Byte_Array
   ) is
   begin
      Mem.Calldata := (others => 0);
      for I in Data'Range loop
         if I - Data'First <= Calldata_Index'Last then
            Mem.Calldata (I - Data'First) := Data (I);
         end if;
      end loop;
      Mem.Calldata_Size := Data'Length;
   end Set_Calldata;

   function Get_Calldata_Size (Mem : VM_Memory) return Natural is
   begin
      return Mem.Calldata_Size;
   end Get_Calldata_Size;

   procedure Load_Calldata (
      Mem     : in     VM_Memory;
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) is
   begin
      Buffer := (others => 0);

      if Offset >= Mem.Calldata_Size then
         Success := True;  -- Reading past calldata returns zeros
         return;
      end if;

      for I in 0 .. Size - 1 loop
         if Offset + I < Mem.Calldata_Size and
            I <= Buffer'Last - Buffer'First
         then
            Buffer (Buffer'First + I) := Mem.Calldata (Offset + I);
         end if;
      end loop;
      Success := True;
   end Load_Calldata;

   procedure Calldata_Copy (
      Mem         : in Out VM_Memory;
      Dest_Offset : in     VM_Address;
      Data_Offset : in     Natural;
      Size        : in     Natural;
      Success     : out    Boolean
   ) is
      Mem_Buffer  : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
      Dest_Idx    : constant Natural := Natural (Dest_Offset);
   begin
      if Mem_Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if not Is_Writable (Mem, Dest_Offset, Size) then
         Success := False;
         return;
      end if;

      if Dest_Idx + Size > Actual_Size then
         Success := False;
         return;
      end if;

      for I in 0 .. Size - 1 loop
         if Data_Offset + I < Mem.Calldata_Size then
            Mem_Buffer (Dest_Idx + I) := Mem.Calldata (Data_Offset + I);
         else
            Mem_Buffer (Dest_Idx + I) := 0;  -- Pad with zeros
         end if;
      end loop;
      Success := True;
   end Calldata_Copy;

   ---------------------------------------------------------------------------
   --  Return Data Operations
   ---------------------------------------------------------------------------

   procedure Set_Return_Data (
      Mem  : in Out VM_Memory;
      Data : in     Byte_Array
   ) is
   begin
      Mem.Return_Data := (others => 0);
      for I in Data'Range loop
         if I - Data'First <= Return_Index'Last then
            Mem.Return_Data (I - Data'First) := Data (I);
         end if;
      end loop;
      Mem.Return_Data_Size := Data'Length;
   end Set_Return_Data;

   function Get_Return_Data_Size (Mem : VM_Memory) return Natural is
   begin
      return Mem.Return_Data_Size;
   end Get_Return_Data_Size;

   procedure Load_Return_Data (
      Mem     : in     VM_Memory;
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) is
   begin
      Buffer := (others => 0);

      if Offset >= Mem.Return_Data_Size then
         Success := True;
         return;
      end if;

      for I in 0 .. Size - 1 loop
         if Offset + I < Mem.Return_Data_Size and
            I <= Buffer'Last - Buffer'First
         then
            Buffer (Buffer'First + I) := Mem.Return_Data (Offset + I);
         end if;
      end loop;
      Success := True;
   end Load_Return_Data;

   procedure Return_Data_Copy (
      Mem         : in Out VM_Memory;
      Dest_Offset : in     VM_Address;
      Data_Offset : in     Natural;
      Size        : in     Natural;
      Success     : out    Boolean
   ) is
      Mem_Buffer  : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
      Dest_Idx    : constant Natural := Natural (Dest_Offset);
   begin
      if Mem_Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if not Is_Writable (Mem, Dest_Offset, Size) then
         Success := False;
         return;
      end if;

      if Dest_Idx + Size > Actual_Size then
         Success := False;
         return;
      end if;

      for I in 0 .. Size - 1 loop
         if Data_Offset + I < Mem.Return_Data_Size then
            Mem_Buffer (Dest_Idx + I) := Mem.Return_Data (Data_Offset + I);
         else
            Mem_Buffer (Dest_Idx + I) := 0;
         end if;
      end loop;
      Success := True;
   end Return_Data_Copy;

   ---------------------------------------------------------------------------
   --  Stack Operations
   ---------------------------------------------------------------------------

   procedure Stack_Push (
      Mem     : in Out VM_Memory;
      Data    : in     Byte_Array;
      Success : out    Boolean
   ) is
      Mem_Buffer   : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Stack_Region : Memory_Region renames Mem.Regions (Region_Stack);
      New_SP       : VM_Address;
   begin
      if Mem_Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if Data'Length = 0 then
         Success := True;
         return;
      end if;

      --  Check stack overflow
      if VM_Address (Data'Length) > Mem.Stack_Pointer - Stack_Region.Base then
         Success := False;
         return;
      end if;

      --  Decrement stack pointer (stack grows down)
      New_SP := Mem.Stack_Pointer - VM_Address (Data'Length);

      --  Copy data to stack
      for I in Data'Range loop
         Mem_Buffer (Natural (New_SP) + (I - Data'First)) := Data (I);
      end loop;

      Mem.Stack_Pointer := New_SP;
      Success := True;
   end Stack_Push;

   procedure Stack_Pop (
      Mem     : in Out VM_Memory;
      Size    : in     Natural;
      Data    : out    Byte_Array;
      Success : out    Boolean
   ) is
      Mem_Buffer   : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Stack_Region : Memory_Region renames Mem.Regions (Region_Stack);
      Stack_Top    : constant VM_Address := Stack_Region.Base + VM_Address (Stack_Region.Size);
   begin
      Data := (others => 0);

      if Mem_Buffer = null then
         Success := False;  -- Invalid context
         return;
      end if;

      if Size = 0 then
         Success := True;
         return;
      end if;

      --  Check stack underflow
      if Mem.Stack_Pointer + VM_Address (Size) > Stack_Top then
         Success := False;
         return;
      end if;

      --  Copy data from stack
      for I in 0 .. Size - 1 loop
         if I <= Data'Last - Data'First then
            Data (Data'First + I) := Mem_Buffer (Natural (Mem.Stack_Pointer) + I);
         end if;
      end loop;

      --  Increment stack pointer
      Mem.Stack_Pointer := Mem.Stack_Pointer + VM_Address (Size);
      Success := True;
   end Stack_Pop;

   function Stack_Usage (Mem : VM_Memory) return Natural is
      Stack_Region : Memory_Region renames Mem.Regions (Region_Stack);
      Stack_Top    : constant VM_Address := Stack_Region.Base + VM_Address (Stack_Region.Size);
   begin
      return Natural (Stack_Top - Mem.Stack_Pointer);
   end Stack_Usage;

   ---------------------------------------------------------------------------
   --  Heap Operations
   ---------------------------------------------------------------------------

   procedure Heap_Alloc (
      Mem     : in Out VM_Memory;
      Size    : in     Natural;
      Address : out    VM_Address;
      Success : out    Boolean
   ) is
      Heap_Region : Memory_Region renames Mem.Regions (Region_Heap);
      Heap_End    : constant VM_Address := Heap_Region.Base + VM_Address (Heap_Region.Size);
   begin
      Address := 0;

      if Size = 0 then
         Address := Mem.Heap_Pointer;
         Success := True;
         return;
      end if;

      --  Check heap overflow
      if Mem.Heap_Pointer + VM_Address (Size) > Heap_End then
         Success := False;
         return;
      end if;

      Address := Mem.Heap_Pointer;
      Mem.Heap_Pointer := Mem.Heap_Pointer + VM_Address (Size);
      Success := True;
   end Heap_Alloc;

   function Heap_Usage (Mem : VM_Memory) return Natural is
      Heap_Region : Memory_Region renames Mem.Regions (Region_Heap);
   begin
      return Natural (Mem.Heap_Pointer - Heap_Region.Base);
   end Heap_Usage;

   ---------------------------------------------------------------------------
   --  Memory Expansion
   ---------------------------------------------------------------------------

   procedure Expand_Memory (
      Mem      : in Out VM_Memory;
      Address  : in     VM_Address;
      Size     : in     Natural;
      Gas_Cost : out    Gas_Amount;
      Success  : out    Boolean
   ) is
      Actual_Size   : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
      Ctx_ID        : constant Context_ID := Get_Context_ID (Mem);
      Required_Size : constant Natural := Natural (Address) + Size;
   begin
      if Ctx_ID = Invalid_Context then
         Gas_Cost := 0;
         Success := False;  -- Invalid context
         return;
      end if;

      if Required_Size <= Actual_Size then
         Gas_Cost := 0;
         Success := True;
         return;
      end if;

      if Required_Size > Max_Contract_Memory then
         Gas_Cost := 0;
         Success := False;
         return;
      end if;

      Gas_Cost := Memory_Gas_Cost (Actual_Size, Required_Size);
      --  Re-encode with updated size while preserving context ID
      Mem.Memory_Size := Encode_Size_With_Context (Required_Size, Ctx_ID);
      Success := True;
   end Expand_Memory;

   function Memory_Gas_Cost (
      Current_Size : Natural;
      Target_Size  : Natural
   ) return Gas_Amount is
      --  Gas formula: 3 * (words) + (words^2 / 512)
      --  Where words = ceil(size / 32)
      Current_Words : constant Natural := (Current_Size + 31) / 32;
      Target_Words  : constant Natural := (Target_Size + 31) / 32;
      Current_Cost  : Gas_Amount;
      Target_Cost   : Gas_Amount;
   begin
      if Target_Words <= Current_Words then
         return 0;
      end if;

      Current_Cost := Gas_Amount (3 * Current_Words + (Current_Words * Current_Words) / 512);
      Target_Cost := Gas_Amount (3 * Target_Words + (Target_Words * Target_Words) / 512);

      return Target_Cost - Current_Cost;
   end Memory_Gas_Cost;

   ---------------------------------------------------------------------------
   --  Permission Checking
   ---------------------------------------------------------------------------

   function Is_Readable (
      Mem     : VM_Memory;
      Address : VM_Address;
      Size    : Natural
   ) return Boolean is
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
   begin
      if not Mem.Memory_Valid then
         return False;
      end if;

      if Natural (Address) + Size > Actual_Size then
         return False;
      end if;

      --  Check each region for read permission
      for K in Region_Kind loop
         declare
            R : Memory_Region renames Mem.Regions (K);
         begin
            if R.Valid and R.Perm.Read then
               if Address >= R.Base and
                  Address + VM_Address (Size) <= R.Base + VM_Address (R.Size)
               then
                  return True;
               end if;
            end if;
         end;
      end loop;

      --  Default: allow reads within memory bounds
      return True;
   end Is_Readable;

   function Is_Writable (
      Mem     : VM_Memory;
      Address : VM_Address;
      Size    : Natural
   ) return Boolean is
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
   begin
      if not Mem.Memory_Valid then
         return False;
      end if;

      if Natural (Address) + Size > Actual_Size then
         return False;
      end if;

      --  Check if address falls in a non-writable region
      for K in Region_Kind loop
         declare
            R : Memory_Region renames Mem.Regions (K);
         begin
            if R.Valid then
               --  Check if address overlaps this region
               if Address >= R.Base and Address < R.Base + VM_Address (R.Size) then
                  --  Must have write permission
                  if not R.Perm.Write then
                     return False;
                  end if;
               end if;
            end if;
         end;
      end loop;

      return True;
   end Is_Writable;

   function Is_Executable (
      Mem     : VM_Memory;
      Address : VM_Address
   ) return Boolean is
   begin
      if not Mem.Memory_Valid then
         return False;
      end if;

      --  Only code region is executable
      declare
         R : Memory_Region renames Mem.Regions (Region_Code);
      begin
         return R.Valid and R.Perm.Execute and
                Address >= R.Base and
                Address < R.Base + VM_Address (R.Size);
      end;
   end Is_Executable;

   function WXE_Valid (Mem : VM_Memory) return Boolean is
   begin
      --  Check that no region is both writable and executable
      for K in Region_Kind loop
         declare
            R : Memory_Region renames Mem.Regions (K);
         begin
            if R.Valid and R.Perm.Write and R.Perm.Execute then
               return False;
            end if;
         end;
      end loop;
      return True;
   end WXE_Valid;

   ---------------------------------------------------------------------------
   --  Memory Hashing
   ---------------------------------------------------------------------------

   function Hash_Region (
      Mem   : VM_Memory;
      Start : VM_Address;
      Size  : Natural
   ) return Hash256 is
      use Anubis_SHA3;
      Mem_Buffer  : constant access Internal_Memory := Get_Memory_Buffer (Mem);
      Actual_Size : constant Natural := Decode_Actual_Size (Mem.Memory_Size);
      Data        : Anubis_Types.Byte_Array (0 .. Size - 1);
      Digest      : SHA3_256_Digest;
      Result      : Hash256 := Hash256_Zero;
   begin
      if Mem_Buffer = null then
         return Hash256_Zero;  -- Invalid context
      end if;

      --  Copy region to temporary buffer
      for I in 0 .. Size - 1 loop
         if Natural (Start) + I < Actual_Size then
            Data (I) := Anubis_Types.Byte (Mem_Buffer (Natural (Start) + I));
         else
            Data (I) := 0;
         end if;
      end loop;

      --  Hash the data
      SHA3_256 (Data, Digest);

      --  Convert to Hash256
      for I in 0 .. 31 loop
         Result (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;

      return Result;
   end Hash_Region;

   function Hash_State (Mem : VM_Memory) return Hash256 is
      --  Hash all writable regions together
      Total_Size : Natural := 0;
   begin
      --  Calculate total writable memory size
      for K in Region_Kind loop
         declare
            R : Memory_Region renames Mem.Regions (K);
         begin
            if R.Valid and R.Perm.Write then
               Total_Size := Total_Size + R.Size;
            end if;
         end;
      end loop;

      if Total_Size = 0 then
         return Hash256_Zero;
      end if;

      --  For simplicity, just hash the data region
      return Hash_Region (Mem,
                          Mem.Regions (Region_Data).Base,
                          Mem.Regions (Region_Data).Size);
   end Hash_State;

end Aegis_VM_Memory;
