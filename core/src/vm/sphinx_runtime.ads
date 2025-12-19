--  SPHINX Runtime: VM Syscall Interface for Native Contracts
--
--  This package provides the syscall table that native contracts use
--  to access VM services. All operations flow through these syscalls:
--  - THOTH: Storage (sload, sstore)
--  - ANKH: Crypto (sha3, mldsa_verify, mlkem_decaps)
--  - Environment: caller, self, timestamp, block_number
--  - Calldata: calldata, calldatasize, calldatacopy
--  - Calls: call, staticcall, delegatecall, create, create2
--  - Privacy: SHIELD, WHISPER, VEIL, EYE, GATE
--
--  The VM sets up these callbacks before executing a native contract.
--
--  SPARK Boundary:
--  This package forms the boundary between SPARK-verified contracts
--  and the non-SPARK VM runtime. Global state is explicitly declared
--  for all operations that access Current_Runtime.

pragma SPARK_Mode (On);

with System;
with Interfaces; use Interfaces;
with Interfaces.C;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Address_Types; use Anubis_Address_Types;

package Sphinx_Runtime with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Runtime Constants
   ---------------------------------------------------------------------------

   Max_Call_Depth     : constant := 1024;
   Max_Calldata_Size  : constant := 65536;  --  64 KB
   Max_Return_Size    : constant := 65536;  --  64 KB
   Max_Memory_Size    : constant := 1024 * 1024;  --  1 MB

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
   Sys_CallData     : constant := 16#23#;
   Sys_CallDataSize : constant := 16#24#;
   Sys_BlockNumber  : constant := 16#25#;
   Sys_Timestamp    : constant := 16#26#;
   Sys_GasPrice     : constant := 16#27#;
   Sys_GasRemaining : constant := 16#28#;

   --  Cross-Contract Calls
   Sys_Call         : constant := 16#30#;
   Sys_StaticCall   : constant := 16#31#;
   Sys_DelegateCall : constant := 16#32#;
   Sys_Create       : constant := 16#33#;
   Sys_Create2      : constant := 16#34#;

   --  Return Data
   Sys_Return       : constant := 16#40#;
   Sys_Revert       : constant := 16#41#;
   Sys_ReturnData   : constant := 16#42#;
   Sys_ReturnDataSize : constant := 16#43#;

   ---------------------------------------------------------------------------
   --  Calldata Buffer Type
   ---------------------------------------------------------------------------

   subtype Calldata_Index is Natural range 0 .. Max_Calldata_Size - 1;
   type Calldata_Buffer is array (Calldata_Index) of Byte;

   subtype Return_Index is Natural range 0 .. Max_Return_Size - 1;
   type Return_Buffer is array (Return_Index) of Byte;

   ---------------------------------------------------------------------------
   --  Call Frame (for nested calls)
   ---------------------------------------------------------------------------

   type Call_Kind is (
      Kind_Call,           --  Normal CALL
      Kind_Static_Call,    --  STATICCALL (read-only)
      Kind_Delegate_Call,  --  DELEGATECALL (preserve caller/value)
      Kind_Create,         --  CREATE
      Kind_Create2         --  CREATE2
   );

   type Call_Frame is record
      --  Contract addresses
      Contract_Addr : Account_ID;   --  Current contract being executed
      Code_Addr     : Account_ID;   --  Address of code (for delegatecall)
      Caller_Addr   : Account_ID;   --  msg.sender
      Origin_Addr   : Account_ID;   --  tx.origin

      --  Call context
      Kind          : Call_Kind;
      Value         : Unsigned_64;  --  msg.value

      --  Calldata for this frame
      Calldata      : Calldata_Buffer;
      Calldata_Size : Natural;

      --  Return data from subcall
      Return_Data   : Return_Buffer;
      Return_Size   : Natural;

      --  Gas allocation
      Gas_Limit     : Gas_Amount;
      Gas_Used      : Gas_Amount;

      --  State snapshot for rollback
      Snapshot_ID   : Natural;

      --  Frame status
      Active        : Boolean;
      Depth         : Natural;
   end record;

   Empty_Frame : constant Call_Frame := (
      Contract_Addr => (others => 0),
      Code_Addr     => (others => 0),
      Caller_Addr   => (others => 0),
      Origin_Addr   => (others => 0),
      Kind          => Kind_Call,
      Value         => 0,
      Calldata      => (others => 0),
      Calldata_Size => 0,
      Return_Data   => (others => 0),
      Return_Size   => 0,
      Gas_Limit     => 0,
      Gas_Used      => 0,
      Snapshot_ID   => 0,
      Active        => False,
      Depth         => 0
   );

   ---------------------------------------------------------------------------
   --  Call Stack
   ---------------------------------------------------------------------------

   subtype Frame_Index is Natural range 0 .. Max_Call_Depth - 1;
   type Frame_Stack is array (Frame_Index) of Call_Frame;

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
   --  Full Runtime Context
   ---------------------------------------------------------------------------

   type Runtime_Context is record
      --  Call stack
      Frames        : Frame_Stack;
      Current_Depth : Natural;

      --  Transaction-level info
      Origin        : Account_ID;
      Gas_Price     : Unsigned_64;
      Block_Number  : Unsigned_64;
      Timestamp     : Unsigned_64;
      Chain_ID      : Unsigned_64;

      --  External syscalls
      Syscalls      : Syscall_Table_Access;

      --  Runtime state
      Initialized   : Boolean;
      Next_Snapshot : Natural;
   end record;

   type Runtime_Context_Access is access all Runtime_Context;

   --  Legacy execution context for compatibility
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

   --  Current runtime context (full call stack support)
   Current_Runtime : Runtime_Context_Access := null;

   --  Legacy context for backward compatibility
   Current_Context : Context_Access := null;

   ---------------------------------------------------------------------------
   --  Runtime Initialization
   ---------------------------------------------------------------------------

   --  Initialize runtime for new transaction
   procedure Initialize_Runtime (
      Runtime     : out Runtime_Context;
      Origin      : in  Account_ID;
      Gas_Limit   : in  Gas_Amount;
      Gas_Price   : in  Unsigned_64;
      Block_Num   : in  Unsigned_64;
      Timestamp   : in  Unsigned_64;
      Chain_ID    : in  Unsigned_64;
      Syscalls    : in  Syscall_Table_Access;
      Success     : out Boolean
   );

   --  Finalize runtime after transaction
   procedure Finalize_Runtime (Runtime : in out Runtime_Context);

   --  Set current runtime context
   procedure Set_Runtime (Runtime : Runtime_Context_Access);

   --  Get current runtime context
   function Get_Runtime return Runtime_Context_Access;

   ---------------------------------------------------------------------------
   --  Call Frame Management
   ---------------------------------------------------------------------------

   --  Push a new call frame
   procedure Push_Frame (
      Runtime   : in out Runtime_Context;
      Target    : in     Account_ID;
      Caller    : in     Account_ID;
      Value     : in     Unsigned_64;
      Calldata  : in     Byte_Array;
      Gas_Limit : in     Gas_Amount;
      Kind      : in     Call_Kind;
      Success   : out    Boolean
   );

   --  Pop call frame on return/revert
   procedure Pop_Frame (
      Runtime     : in Out Runtime_Context;
      Success     : in     Boolean;
      Return_Data : in     Byte_Array;
      Gas_Refund  : out    Gas_Amount
   );

   --  Get current call frame
   function Current_Frame (Runtime : Runtime_Context) return Call_Frame;

   ---------------------------------------------------------------------------
   --  Calldata Operations
   ---------------------------------------------------------------------------

   --  Get calldata size for current frame
   function Get_Calldata_Size return Natural;

   --  Load calldata bytes
   procedure Load_Calldata (
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) with
      Global => (Input => Current_Runtime),
      Pre    => Buffer'Length > 0 and Size <= Buffer'Length;

   --  Copy calldata to memory (for CALLDATACOPY)
   procedure Copy_Calldata (
      Data_Offset : in     Natural;
      Size        : in     Natural;
      Buffer      : out    Byte_Array;
      Success     : out    Boolean
   );

   ---------------------------------------------------------------------------
   --  Return Data Operations
   ---------------------------------------------------------------------------

   --  Get return data size from last subcall
   function Get_Return_Data_Size return Natural;

   --  Load return data bytes
   procedure Load_Return_Data (
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   );

   ---------------------------------------------------------------------------
   --  Cross-Contract Call Operations
   ---------------------------------------------------------------------------

   --  Execute CALL to another contract
   procedure Execute_Call (
      Target      : in     Account_ID;
      Value       : in     Unsigned_64;
      Calldata    : in     Byte_Array;
      Gas_Limit   : in     Gas_Amount;
      Return_Data : out    Byte_Array;
      Return_Size : out    Natural;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with
      Global => (In_Out => Current_Runtime),
      Pre    => Return_Data'Length > 0 and Gas_Limit > 0,
      Post   => Return_Size <= Return_Data'Length and
                Gas_Used <= Gas_Limit;

   --  Execute STATICCALL (read-only)
   procedure Execute_Static_Call (
      Target      : in     Account_ID;
      Calldata    : in     Byte_Array;
      Gas_Limit   : in     Gas_Amount;
      Return_Data : out    Byte_Array;
      Return_Size : out    Natural;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with
      Global => (In_Out => Current_Runtime),
      Pre    => Return_Data'Length > 0 and Gas_Limit > 0,
      Post   => Return_Size <= Return_Data'Length and
                Gas_Used <= Gas_Limit;

   --  Execute DELEGATECALL (preserve context)
   procedure Execute_Delegate_Call (
      Target      : in     Account_ID;
      Calldata    : in     Byte_Array;
      Gas_Limit   : in     Gas_Amount;
      Return_Data : out    Byte_Array;
      Return_Size : out    Natural;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with
      Global => (In_Out => Current_Runtime),
      Pre    => Return_Data'Length > 0 and Gas_Limit > 0,
      Post   => Return_Size <= Return_Data'Length and
                Gas_Used <= Gas_Limit;

   --  Execute CREATE (deploy new contract)
   procedure Execute_Create (
      Init_Code   : in     Byte_Array;
      Value       : in     Unsigned_64;
      Gas_Limit   : in     Gas_Amount;
      New_Address : out    Account_ID;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with
      Global => (In_Out => Current_Runtime),
      Pre    => Gas_Limit > 0,
      Post   => Gas_Used <= Gas_Limit;

   --  Execute CREATE2 (deterministic deployment)
   procedure Execute_Create2 (
      Init_Code   : in     Byte_Array;
      Salt        : in     Hash256;
      Value       : in     Unsigned_64;
      Gas_Limit   : in     Gas_Amount;
      New_Address : out    Account_ID;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with
      Global => (In_Out => Current_Runtime),
      Pre    => Gas_Limit > 0,
      Post   => Gas_Used <= Gas_Limit;

   ---------------------------------------------------------------------------
   --  Gas Management
   ---------------------------------------------------------------------------

   --  Use gas from current frame
   procedure Use_Gas (
      Amount  : in     Gas_Amount;
      Success : out    Boolean
   );

   --  Get remaining gas
   function Gas_Remaining return Gas_Amount;

   --  Refund gas
   procedure Refund_Gas (Amount : in Gas_Amount);

   ---------------------------------------------------------------------------
   --  Legacy Context Management (backward compatibility)
   ---------------------------------------------------------------------------

   procedure Set_Context (Ctx : Context_Access);
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
