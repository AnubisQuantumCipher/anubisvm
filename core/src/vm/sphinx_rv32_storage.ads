--  SPHINX RV32 Storage: SPARK-Compatible Storage Interface
--
--  This package provides a SPARK-verifiable storage interface for the
--  RV32 interpreter. It uses embedded fixed-size arrays instead of
--  heap allocation or callbacks.
--
--  SPARK Verification Properties:
--  - All storage operations are bounds-checked
--  - No heap allocation or pointers
--  - Deterministic behavior
--  - Full SPARK Mode On verification
--
--  Design:
--  - Storage uses 32-byte keys and 32-byte values (matching EVM)
--  - Fixed-capacity storage slots (for bounded verification)
--  - Simple linear search (can be optimized later without changing interface)

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;

package Sphinx_RV32_Storage with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Storage Types
   ---------------------------------------------------------------------------

   --  Storage key: 32 bytes (256 bits)
   subtype Storage_Key_Bytes is Byte_Array (0 .. 31);

   --  Storage value: 32 bytes (256 bits)
   subtype Storage_Value_Bytes is Byte_Array (0 .. 31);

   --  Zero values
   Zero_Key   : constant Storage_Key_Bytes := (others => 0);
   Zero_Value : constant Storage_Value_Bytes := (others => 0);

   --  Maximum storage slots per contract execution
   --  This bounds the verification complexity
   Max_Storage_Slots : constant := 4096;

   subtype Storage_Slot_Index is Natural range 0 .. Max_Storage_Slots - 1;
   subtype Storage_Count is Natural range 0 .. Max_Storage_Slots;

   ---------------------------------------------------------------------------
   --  Storage Slot Entry
   ---------------------------------------------------------------------------

   type Storage_Entry is record
      Key     : Storage_Key_Bytes;
      Value   : Storage_Value_Bytes;
      Written : Boolean;  -- True if this slot has been written
   end record;

   Empty_Entry : constant Storage_Entry := (
      Key     => Zero_Key,
      Value   => Zero_Value,
      Written => False
   );

   ---------------------------------------------------------------------------
   --  Storage State (Embedded, No Heap)
   ---------------------------------------------------------------------------

   type Storage_Entries is array (Storage_Slot_Index) of Storage_Entry;

   --  Storage state: embedded fixed-size array
   --  No pointers, no heap, fully SPARK-verifiable
   type Storage_State is record
      Entries : Storage_Entries;
      Count   : Storage_Count;  -- Number of slots in use
   end record;

   --  Empty storage state
   Empty_Storage : constant Storage_State := (
      Entries => (others => Empty_Entry),
      Count   => 0
   );

   --  Ghost: Storage state is valid
   function Storage_Valid (S : Storage_State) return Boolean is
      (S.Count <= Max_Storage_Slots)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Storage Operations (SPARK Mode On)
   ---------------------------------------------------------------------------

   --  Load value from storage
   --  Returns Zero_Value if key not found
   procedure SLoad (
      S       : in     Storage_State;
      Key     : in     Storage_Key_Bytes;
      Value   : out    Storage_Value_Bytes;
      Found   : out    Boolean
   ) with
      Global => null,
      Pre    => Storage_Valid (S),
      Post   => (if not Found then Value = Zero_Value);

   --  Store value to storage
   --  Returns Success = False if storage is full
   procedure SStore (
      S       : in Out Storage_State;
      Key     : in     Storage_Key_Bytes;
      Value   : in     Storage_Value_Bytes;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Storage_Valid (S),
      Post   => Storage_Valid (S) and then
                (if Success then S.Count >= S'Old.Count);

   --  Check if key exists in storage
   function Contains (
      S   : Storage_State;
      Key : Storage_Key_Bytes
   ) return Boolean with
      Global => null,
      Pre    => Storage_Valid (S);

   --  Get current storage slot count
   function Slot_Count (S : Storage_State) return Storage_Count is
      (S.Count)
   with Global => null;

   --  Check if storage is full
   function Is_Full (S : Storage_State) return Boolean is
      (S.Count >= Max_Storage_Slots)
   with Global => null;

   ---------------------------------------------------------------------------
   --  Storage Diff (for transaction commit/rollback)
   ---------------------------------------------------------------------------

   --  A storage modification record
   type Storage_Diff_Entry is record
      Key       : Storage_Key_Bytes;
      Old_Value : Storage_Value_Bytes;
      New_Value : Storage_Value_Bytes;
      Was_Set   : Boolean;  -- True if key existed before
   end record;

   --  Maximum diffs per transaction
   Max_Storage_Diffs : constant := 1024;
   subtype Storage_Diff_Index is Natural range 0 .. Max_Storage_Diffs - 1;
   subtype Storage_Diff_Count is Natural range 0 .. Max_Storage_Diffs;

   type Storage_Diffs is array (Storage_Diff_Index) of Storage_Diff_Entry;

   --  Storage diff tracking for rollback support
   type Storage_Diff_State is record
      Diffs : Storage_Diffs;
      Count : Storage_Diff_Count;
   end record;

   Empty_Diff_State : constant Storage_Diff_State := (
      Diffs => (others => (
         Key       => Zero_Key,
         Old_Value => Zero_Value,
         New_Value => Zero_Value,
         Was_Set   => False
      )),
      Count => 0
   );

end Sphinx_RV32_Storage;
