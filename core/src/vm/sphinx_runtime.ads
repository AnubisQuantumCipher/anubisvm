--  SPHINX Runtime: VM Syscall Interface for Native Contracts
--
--  This package provides the syscall table that native contracts use
--  to access VM services. All operations flow through these syscalls:
--  - THOTH: Storage (sload, sstore)
--  - ANKH: Crypto (sha3, mldsa_verify, mlkem_decaps)
--  - Environment: caller, self, timestamp, block_number
--  - Privacy: SHIELD, WHISPER, VEIL, EYE, GATE
--
--  The VM sets up these callbacks before executing a native contract.

with System;
with Interfaces; use Interfaces;
with Interfaces.C;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Address_Types; use Anubis_Address_Types;

package Sphinx_Runtime is

   ---------------------------------------------------------------------------
   --  Syscall Numbers (must match anubis_vm.h)
   ---------------------------------------------------------------------------

   --  THOTH Storage
   Sys_SLoad  : constant := 16#01#;
   Sys_SStore : constant := 16#02#;

   --  ANKH Crypto
   Sys_SHA3         : constant := 16#10#;
   Sys_Keccak256    : constant := 16#11#;
   Sys_MLDSA_Verify : constant := 16#12#;
   Sys_MLKEM_Decaps : constant := 16#13#;

   --  Environment
   Sys_Caller       : constant := 16#20#;
   Sys_Address      : constant := 16#21#;
   Sys_CallValue    : constant := 16#22#;
   Sys_BlockNumber  : constant := 16#25#;
   Sys_Timestamp    : constant := 16#26#;
   Sys_GasRemaining : constant := 16#28#;

   ---------------------------------------------------------------------------
   --  Syscall Function Types (C-compatible)
   ---------------------------------------------------------------------------

   --  Storage: sload(slot) -> value
   type SLoad_Fn is access procedure (
      Slot  : System.Address;  --  Hash256*
      Value : System.Address   --  Hash256* (out)
   ) with Convention => C;

   --  Storage: sstore(slot, value)
   type SStore_Fn is access procedure (
      Slot  : System.Address;  --  Hash256*
      Value : System.Address   --  Hash256*
   ) with Convention => C;

   --  Crypto: sha3(input, input_len, output)
   type SHA3_Fn is access procedure (
      Input     : System.Address;  --  uint8_t*
      Input_Len : Interfaces.C.size_t;
      Output    : System.Address   --  Hash256* (out)
   ) with Convention => C;

   --  Crypto: mldsa_verify(message, msg_len, signature, sig_len, pubkey) -> bool
   type MLDSA_Verify_Fn is access function (
      Message    : System.Address;
      Msg_Len    : Interfaces.C.size_t;
      Signature  : System.Address;
      Sig_Len    : Interfaces.C.size_t;
      Public_Key : System.Address
   ) return Interfaces.C.int
   with Convention => C;

   --  Environment: get_caller(addr_out)
   type Get_Caller_Fn is access procedure (
      Addr_Out : System.Address  --  Address* (out)
   ) with Convention => C;

   --  Environment: get_self(addr_out)
   type Get_Self_Fn is access procedure (
      Addr_Out : System.Address  --  Address* (out)
   ) with Convention => C;

   --  Environment: get_timestamp() -> uint64
   type Get_Timestamp_Fn is access function return Unsigned_64
   with Convention => C;

   --  Environment: get_block_number() -> uint64
   type Get_Block_Number_Fn is access function return Unsigned_64
   with Convention => C;

   --  Environment: get_gas_remaining() -> uint64
   type Get_Gas_Remaining_Fn is access function return Unsigned_64
   with Convention => C;

   ---------------------------------------------------------------------------
   --  Syscall Table (set by VM before execution)
   ---------------------------------------------------------------------------

   type Syscall_Table is record
      --  THOTH Storage
      SLoad  : SLoad_Fn;
      SStore : SStore_Fn;

      --  ANKH Crypto
      SHA3         : SHA3_Fn;
      MLDSA_Verify : MLDSA_Verify_Fn;

      --  Environment
      Get_Caller       : Get_Caller_Fn;
      Get_Self         : Get_Self_Fn;
      Get_Timestamp    : Get_Timestamp_Fn;
      Get_Block_Number : Get_Block_Number_Fn;
      Get_Gas_Remaining : Get_Gas_Remaining_Fn;
   end record
   with Convention => C;

   type Syscall_Table_Access is access all Syscall_Table;

   ---------------------------------------------------------------------------
   --  Execution Context
   ---------------------------------------------------------------------------

   type Execution_Context is record
      Caller        : Account_ID;
      Self          : Account_ID;
      Call_Value    : Unsigned_64;
      Gas_Limit     : Gas_Amount;
      Gas_Used      : Gas_Amount;
      Block_Number  : Unsigned_64;
      Timestamp     : Unsigned_64;
      Syscalls      : Syscall_Table_Access;
   end record
   with Convention => C;

   type Context_Access is access all Execution_Context;

   ---------------------------------------------------------------------------
   --  Runtime State
   ---------------------------------------------------------------------------

   --  Current execution context (set before contract call)
   Current_Context : Context_Access := null;

   --  Set the execution context
   procedure Set_Context (Ctx : Context_Access);

   --  Get current context
   function Get_Context return Context_Access;

   ---------------------------------------------------------------------------
   --  High-Level Syscall Wrappers (Ada-friendly)
   ---------------------------------------------------------------------------

   --  Storage operations
   procedure VM_SLoad (Slot : Hash256; Value : out Hash256);
   procedure VM_SStore (Slot : Hash256; Value : Hash256);

   --  Crypto operations
   procedure VM_SHA3 (Input : Byte_Array; Output : out Hash256);
   function VM_MLDSA_Verify (
      Message   : Byte_Array;
      Signature : Byte_Array;
      Pub_Key   : Byte_Array
   ) return Boolean;

   --  Environment operations
   procedure VM_Get_Caller (Addr : out Account_ID);
   procedure VM_Get_Self (Addr : out Account_ID);
   function VM_Get_Timestamp return Unsigned_64;
   function VM_Get_Block_Number return Unsigned_64;
   function VM_Get_Gas_Remaining return Unsigned_64;

end Sphinx_Runtime;
