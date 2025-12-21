--  SPHINX RV32 Syscall Implementation: Real Crypto Operations
--
--  This package provides the actual crypto implementations for syscalls.
--  It bridges the SPARK syscall handlers to the real crypto libraries.
--
--  SPARK Mode: Off (uses context and crypto libraries)

pragma SPARK_Mode (Off);

with System;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;

package Sphinx_RV32_Syscall_Impl is

   ---------------------------------------------------------------------------
   --  Storage Operations (via Context Callbacks)
   ---------------------------------------------------------------------------

   --  Load 32-byte value from storage
   procedure Storage_Load (
      Context   : in     System.Address;
      Slot_Data : in     Byte_Array;
      Value     : out    Byte_Array;
      Success   : out    Boolean
   ) with
      Pre => Slot_Data'Length = 32 and Value'Length = 32;

   --  Store 32-byte value to storage
   procedure Storage_Store (
      Context    : in     System.Address;
      Slot_Data  : in     Byte_Array;
      Value_Data : in     Byte_Array;
      Success    : out    Boolean
   ) with
      Pre => Slot_Data'Length = 32 and Value_Data'Length = 32;

   ---------------------------------------------------------------------------
   --  Crypto: SHA3-256
   ---------------------------------------------------------------------------

   --  Compute SHA3-256 hash
   procedure Compute_SHA3_256 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Pre => Output'Length = 32;

   --  Compute Keccak-256 hash (Ethereum compatible)
   procedure Compute_Keccak_256 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Pre => Output'Length = 32;

   ---------------------------------------------------------------------------
   --  Crypto: ML-DSA-87 Signature Verification
   ---------------------------------------------------------------------------

   --  Verify ML-DSA-87 signature
   procedure Verify_MLDSA_Signature (
      Public_Key : in     Byte_Array;
      Message    : in     Byte_Array;
      Signature  : in     Byte_Array;
      Valid      : out    Boolean;
      Success    : out    Boolean
   ) with
      Pre => Public_Key'Length = 2592 and Signature'Length = 4627;

   ---------------------------------------------------------------------------
   --  Crypto: ML-KEM-1024 Decapsulation
   ---------------------------------------------------------------------------

   --  Decapsulate shared secret
   procedure Decapsulate_MLKEM (
      Secret_Key    : in     Byte_Array;
      Ciphertext    : in     Byte_Array;
      Shared_Secret : out    Byte_Array;
      Success       : out    Boolean
   ) with
      Pre => Secret_Key'Length = 3168 and
             Ciphertext'Length = 1568 and
             Shared_Secret'Length = 32;

   ---------------------------------------------------------------------------
   --  Environment Access (via Context)
   ---------------------------------------------------------------------------

   --  Get caller address (32 bytes)
   procedure Get_Caller (
      Context : in     System.Address;
      Address : out    Byte_Array;
      Success : out    Boolean
   ) with
      Pre => Address'Length = 32;

   --  Get self address (32 bytes)
   procedure Get_Self_Address (
      Context : in     System.Address;
      Address : out    Byte_Array;
      Success : out    Boolean
   ) with
      Pre => Address'Length = 32;

   --  Get calldata
   procedure Get_Calldata (
      Context  : in     System.Address;
      Offset   : in     Natural;
      Length   : in     Natural;
      Buffer   : out    Byte_Array;
      Actual   : out    Natural;
      Success  : out    Boolean
   ) with
      Pre => Buffer'Length >= Length;

   --  Get calldata size
   procedure Get_Calldata_Size (
      Context : in     System.Address;
      Size    : out    Natural;
      Success : out    Boolean
   );

   --  Get block number (lower 32 bits)
   procedure Get_Block_Number (
      Context : in     System.Address;
      Value   : out    Word;
      Success : out    Boolean
   );

   --  Get timestamp (lower 32 bits)
   procedure Get_Timestamp (
      Context : in     System.Address;
      Value   : out    Word;
      Success : out    Boolean
   );

end Sphinx_RV32_Syscall_Impl;
