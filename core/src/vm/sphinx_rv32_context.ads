--  SPHINX RV32 Context: Bridge between Interpreter and VM Infrastructure
--
--  This package provides the execution context that connects the
--  RISC-V interpreter syscalls to the real AnubisVM infrastructure:
--  - THOTH storage (via Aegis_Execution)
--  - ANKH crypto (via Anubis_SHA3, Anubis_MLDSA, Anubis_MLKEM)
--  - Environment info (caller, address, block, etc.)
--
--  The context is passed to the interpreter via Syscall_Context pointer
--  in Interpreter_State, allowing syscall handlers to access:
--  - Storage operations (sload/sstore)
--  - Cryptographic operations (sha3, mldsa_verify, mlkem_decaps)
--  - Transaction environment (caller, block number, timestamp, etc.)
--  - Calldata buffer for input parameters
--
--  SPARK Mode: Off for this bridge (uses access types for context passing)

pragma SPARK_Mode (Off);

with System;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;

package Sphinx_RV32_Context is

   ---------------------------------------------------------------------------
   --  Calldata Buffer
   ---------------------------------------------------------------------------

   --  Maximum calldata size (64KB should be plenty for contract calls)
   Max_Calldata_Size : constant := 65536;

   subtype Calldata_Size is Natural range 0 .. Max_Calldata_Size;
   subtype Calldata_Buffer is Byte_Array (0 .. Max_Calldata_Size - 1);

   ---------------------------------------------------------------------------
   --  Return Data Buffer
   ---------------------------------------------------------------------------

   --  Maximum return data size
   Max_Return_Data_Size : constant := 65536;

   subtype Return_Data_Size is Natural range 0 .. Max_Return_Data_Size;
   subtype Return_Data_Buffer is Byte_Array (0 .. Max_Return_Data_Size - 1);

   ---------------------------------------------------------------------------
   --  Execution Context for Interpreter
   ---------------------------------------------------------------------------

   --  This context is allocated by the VM and passed to the interpreter
   --  via the Syscall_Context pointer in Interpreter_State.
   type Interpreter_Context is record
      --  Transaction/Call Info
      Caller      : Contract_Address;  -- msg.sender
      Self        : Contract_Address;  -- address(this)
      Origin      : Contract_Address;  -- tx.origin
      Call_Value  : U256;              -- msg.value

      --  Block Info
      Block_Number : U256;
      Timestamp    : U256;
      Chain_ID     : U256;
      Gas_Price    : U256;

      --  Calldata
      Calldata        : Calldata_Buffer;
      Calldata_Length : Calldata_Size;

      --  Return Data (set by previous call or return syscall)
      Return_Data        : Return_Data_Buffer;
      Return_Data_Length : Return_Data_Size;

      --  Storage callback pointers (set by VM)
      --  These allow the interpreter to call back into the VM for storage
      Storage_Load_Callback  : System.Address;
      Storage_Store_Callback : System.Address;

      --  Execution mode flags
      Is_Static : Boolean;  -- True if in staticcall (no state changes)
      Is_Valid  : Boolean;  -- True if context is properly initialized
   end record;

   --  Access type for context (passed via System.Address)
   type Interpreter_Context_Ptr is access all Interpreter_Context;

   ---------------------------------------------------------------------------
   --  Context Conversion
   ---------------------------------------------------------------------------

   --  Convert System.Address to context pointer
   function To_Context_Ptr (Addr : System.Address) return Interpreter_Context_Ptr;

   --  Check if context pointer is valid
   function Is_Valid_Context (Addr : System.Address) return Boolean;

   ---------------------------------------------------------------------------
   --  Storage Callback Types
   ---------------------------------------------------------------------------

   --  Storage key/value are 32 bytes (256 bits)
   subtype Storage_Key_Bytes is Byte_Array (0 .. 31);
   subtype Storage_Value_Bytes is Byte_Array (0 .. 31);

   --  Callback procedure types for storage operations
   type Storage_Load_Proc is access procedure (
      Key     : in     Storage_Key_Bytes;
      Value   : out    Storage_Value_Bytes;
      Success : out    Boolean
   );

   type Storage_Store_Proc is access procedure (
      Key     : in     Storage_Key_Bytes;
      Value   : in     Storage_Value_Bytes;
      Success : out    Boolean
   );

   ---------------------------------------------------------------------------
   --  Context-Aware Syscall Helpers
   ---------------------------------------------------------------------------

   --  Storage: Load 32-byte value from storage
   procedure Context_SLoad (
      Ctx     : in     Interpreter_Context_Ptr;
      Key     : in     Storage_Key_Bytes;
      Value   : out    Storage_Value_Bytes;
      Success : out    Boolean
   );

   --  Storage: Store 32-byte value to storage
   procedure Context_SStore (
      Ctx     : in     Interpreter_Context_Ptr;
      Key     : in     Storage_Key_Bytes;
      Value   : in     Storage_Value_Bytes;
      Success : out    Boolean
   );

   --  Crypto: SHA3-256 hash
   procedure Context_SHA3 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Pre => Output'Length = 32;

   --  Crypto: Keccak-256 hash (Ethereum compatible)
   procedure Context_Keccak256 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Pre => Output'Length = 32;

   --  Crypto: ML-DSA-87 signature verification
   procedure Context_MLDSA_Verify (
      Public_Key : in     Byte_Array;
      Message    : in     Byte_Array;
      Signature  : in     Byte_Array;
      Valid      : out    Boolean;
      Success    : out    Boolean
   ) with
      Pre => Public_Key'Length = 2592 and
             Signature'Length = 4627;

   --  Crypto: ML-KEM-1024 decapsulation
   procedure Context_MLKEM_Decaps (
      Secret_Key    : in     Byte_Array;
      Ciphertext    : in     Byte_Array;
      Shared_Secret : out    Byte_Array;
      Success       : out    Boolean
   ) with
      Pre => Secret_Key'Length = 3168 and
             Ciphertext'Length = 1568 and
             Shared_Secret'Length = 32;

   ---------------------------------------------------------------------------
   --  Environment Access
   ---------------------------------------------------------------------------

   --  Get caller address as 32-byte array
   procedure Get_Caller_Bytes (
      Ctx   : in     Interpreter_Context_Ptr;
      Bytes : out    Byte_Array
   ) with
      Pre => Bytes'Length = 32;

   --  Get self address as 32-byte array
   procedure Get_Address_Bytes (
      Ctx   : in     Interpreter_Context_Ptr;
      Bytes : out    Byte_Array
   ) with
      Pre => Bytes'Length = 32;

   --  Get calldata slice
   procedure Get_Calldata (
      Ctx     : in     Interpreter_Context_Ptr;
      Offset  : in     Natural;
      Length  : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Pre => Buffer'Length >= Length;

   --  Get calldata size
   function Get_Calldata_Size (
      Ctx : Interpreter_Context_Ptr
   ) return Natural;

   --  Get block number (lower 32 bits for RV32)
   function Get_Block_Number_32 (
      Ctx : Interpreter_Context_Ptr
   ) return Word;

   --  Get timestamp (lower 32 bits for RV32)
   function Get_Timestamp_32 (
      Ctx : Interpreter_Context_Ptr
   ) return Word;

   ---------------------------------------------------------------------------
   --  Context Creation (for VM to use)
   ---------------------------------------------------------------------------

   --  Create a new interpreter context
   function Create_Context return Interpreter_Context_Ptr;

   --  Initialize context with transaction info
   procedure Initialize_Context (
      Ctx          : in out Interpreter_Context_Ptr;
      Caller       : in     Contract_Address;
      Self         : in     Contract_Address;
      Origin       : in     Contract_Address;
      Call_Value   : in     U256;
      Block_Number : in     U256;
      Timestamp    : in     U256;
      Chain_ID     : in     U256;
      Gas_Price    : in     U256;
      Is_Static    : in     Boolean
   );

   --  Set calldata from byte array
   procedure Set_Calldata (
      Ctx  : in out Interpreter_Context_Ptr;
      Data : in     Byte_Array
   ) with
      Pre => Data'Length <= Max_Calldata_Size;

   --  Free context (cleanup)
   procedure Free_Context (Ctx : in out Interpreter_Context_Ptr);

end Sphinx_RV32_Context;
