--  SPHINX Native macOS: Platform-specific native execution
--
--  This package provides actual native code execution on macOS ARM64.
--  Uses mmap/mprotect for memory isolation and function pointers for
--  calling loaded contract code.
--
--  Security Model:
--  - Contracts are compiled Ada/SPARK libraries (.dylib)
--  - They export a standard entry point: contract_execute
--  - All storage/crypto ops go through callback functions
--  - Memory is isolated per contract invocation
--
--  For true ELF position-independent execution, we'd need:
--  - Full ELF relocation
--  - PLT/GOT setup for dynamic linking
--  - seccomp-like syscall filtering (not available on macOS)
--
--  This implementation uses the simpler "contract as dylib" approach
--  which is more practical for a personal VM.

with System;
with Interfaces; use Interfaces;
with Interfaces.C;
with Aegis_VM_Types; use Aegis_VM_Types;

package Sphinx_Native_MacOS is

   ---------------------------------------------------------------------------
   --  Contract Function Types
   ---------------------------------------------------------------------------

   --  Standard contract entry point signature (C-compatible)
   --  Matches: int contract_execute(const uint8_t*, size_t, uint8_t*, size_t*,
   --                                uint64_t, uint64_t*)
   --  Returns: 0 = success, non-zero = error code
   type Contract_Entry_Fn is access function (
      Calldata     : System.Address;       --  const uint8_t*
      Calldata_Len : Interfaces.C.size_t;  --  size_t
      Return_Buf   : System.Address;       --  uint8_t*
      Return_Len   : System.Address;       --  size_t*
      Gas_Limit    : Unsigned_64;          --  uint64_t
      Gas_Used     : System.Address        --  uint64_t*
   ) return Interfaces.C.int
   with Convention => C;

   --  Storage callback type (contract calls this for SLoad/SStore)
   --  Note: These are Ada callbacks, not C callbacks
   type Storage_Load_Fn is access procedure (
      Slot  : Hash256;
      Value : out Hash256
   );

   type Storage_Store_Fn is access procedure (
      Slot  : Hash256;
      Value : Hash256
   );

   ---------------------------------------------------------------------------
   --  Loaded Library Handle
   ---------------------------------------------------------------------------

   type Library_Handle is private;
   Null_Handle : constant Library_Handle;

   ---------------------------------------------------------------------------
   --  Library Loading
   ---------------------------------------------------------------------------

   --  Load a contract library (.dylib)
   procedure Load_Library (
      Path    : String;
      Handle  : out Library_Handle;
      Success : out Boolean
   );

   --  Get the contract entry point from loaded library
   procedure Get_Entry_Point (
      Handle  : Library_Handle;
      Fn_Ptr  : out Contract_Entry_Fn;
      Success : out Boolean
   );

   --  Unload library
   procedure Unload_Library (Handle : in out Library_Handle);

   ---------------------------------------------------------------------------
   --  Contract Execution
   ---------------------------------------------------------------------------

   --  Execute a loaded contract
   --  This is the real execution path - actually calls native code
   procedure Execute_Contract (
      Handle      : Library_Handle;
      Calldata    : Byte_Array;
      Gas_Limit   : Gas_Amount;
      Return_Data : out Byte_Array;
      Return_Len  : out Natural;
      Gas_Used    : out Gas_Amount;
      Success     : out Boolean
   );

   ---------------------------------------------------------------------------
   --  Runtime Callbacks
   ---------------------------------------------------------------------------

   --  Set storage callbacks (must be called before Execute_Contract)
   procedure Set_Storage_Callbacks (
      Load_Fn  : Storage_Load_Fn;
      Store_Fn : Storage_Store_Fn
   );

private

   type Library_Handle is new System.Address;
   Null_Handle : constant Library_Handle := Library_Handle (System.Null_Address);

end Sphinx_Native_MacOS;
