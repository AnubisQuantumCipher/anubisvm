pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Sandbox; use Aegis_Sandbox;
with Aegis_Execution; use Aegis_Execution;
with Aegis_Contract; use Aegis_Contract;
with Aegis_Crypto_API; use Aegis_Crypto_API;

--  AEGIS Syscall: SPHINX Syscall Dispatcher
--
--  This package provides the syscall dispatcher for the SPHINX sandbox.
--  It validates syscall parameters, checks capabilities, charges gas,
--  and routes calls to the appropriate handlers.
--
--  Syscall Categories:
--  - Storage: SLOAD, SSTORE (THOTH interface)
--  - Crypto: SHA3, ML-DSA, ML-KEM (ANKH interface)
--  - Environment: CALLER, ADDRESS, TIMESTAMP, etc.
--  - Balance: BALANCE, SELFBALANCE
--  - Calls: CALL, STATICCALL, DELEGATECALL
--  - Control: RETURN, REVERT, STOP
--  - Events: LOG0-LOG4
--
--  Security:
--  - All syscalls validate capabilities before execution
--  - Gas is charged before operations
--  - State modifications blocked in static context
--  - Return values validated before passing to contracts
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 5.2: SPHINX Syscalls

package Aegis_Syscall with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Syscall Argument Types
   ---------------------------------------------------------------------------

   --  Maximum syscall arguments
   Max_Syscall_Args : constant := 8;

   type Syscall_Arg_Index is range 0 .. Max_Syscall_Args - 1;
   type Syscall_Args is array (Syscall_Arg_Index) of U256;

   --  Syscall return values
   type Syscall_Return is record
      Success    : Boolean;
      Gas_Used   : Gas_Amount;
      Return_Val : U256;
      Error_Code : Natural;
   end record;

   --  Zero return (for errors)
   Syscall_Return_Zero : constant Syscall_Return := (
      Success    => False,
      Gas_Used   => 0,
      Return_Val => U256_Zero,
      Error_Code => 0
   );

   --  Success return
   Syscall_Return_OK : constant Syscall_Return := (
      Success    => True,
      Gas_Used   => 0,
      Return_Val => U256_Zero,
      Error_Code => 0
   );

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   --  Syscall error codes (matching KHEPRI spec)
   Error_None           : constant := 0;
   Error_Invalid_Syscall : constant := 1;
   Error_Permission     : constant := 2;
   Error_Out_Of_Gas     : constant := 3;
   Error_Invalid_Args   : constant := 4;
   Error_Stack_Overflow : constant := 5;
   Error_Stack_Underflow : constant := 6;
   Error_State_Modify   : constant := 7;
   Error_Call_Depth     : constant := 8;
   Error_Value_Transfer : constant := 9;
   Error_Invalid_Jump   : constant := 10;
   Error_Memory_Access  : constant := 11;
   Error_Revert         : constant := 12;
   Error_Crypto         : constant := 13;

   ---------------------------------------------------------------------------
   --  Main Dispatcher
   ---------------------------------------------------------------------------

   --  Dispatch syscall to appropriate handler
   procedure Dispatch (
      Ctx       : in out Execution_Context;
      Syscall   : in     Syscall_Number;
      Args      : in     Syscall_Args;
      Arg_Count : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => null,
      Pre    => Arg_Count <= Max_Syscall_Args;

   ---------------------------------------------------------------------------
   --  Storage Syscalls (THOTH)
   ---------------------------------------------------------------------------

   --  SLOAD: Load storage value
   procedure Sys_Handle_SLoad (
      Ctx     : in Out Execution_Context;
      Key     : in     U256;
      Result  : out    Syscall_Return
   ) with
      Global => null;

   --  SSTORE: Store storage value
   procedure Sys_Handle_SStore (
      Ctx     : in Out Execution_Context;
      Key     : in     U256;
      Value   : in     U256;
      Result  : out    Syscall_Return
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Cryptographic Syscalls (ANKH)
   ---------------------------------------------------------------------------

   --  SHA3: Compute SHA3-256 hash
   procedure Sys_Handle_SHA3 (
      Ctx        : in Out Execution_Context;
      Data_Hash  : in     Hash256;  -- Hash of data in memory
      Data_Size  : in     Natural;
      Result     : out    Syscall_Return
   ) with
      Global => null,
      Pre    => Data_Size <= Max_Hash_Input;

   --  KECCAK256: Compute Keccak-256 hash (Ethereum-compatible)
   procedure Sys_Handle_Keccak256 (
      Ctx        : in Out Execution_Context;
      Data_Hash  : in     Hash256;
      Data_Size  : in     Natural;
      Result     : out    Syscall_Return
   ) with
      Global => null,
      Pre    => Data_Size <= Max_Hash_Input;

   --  MLDSA_VERIFY: ML-DSA-87 signature verification
   procedure Sys_Handle_MLDSA_Verify (
      Ctx       : in Out Execution_Context;
      Msg_Hash  : in     Hash256;
      Msg_Len   : in     Natural;
      Sig_Hash  : in     Hash256;  -- Hash of signature data
      PK_Hash   : in     Hash256;  -- Hash of public key
      Result    : out    Syscall_Return
   ) with
      Global => null,
      Pre    => Msg_Len <= Max_Hash_Input;

   --  MLKEM_DECAPS: ML-KEM-1024 decapsulation
   procedure Sys_Handle_MLKEM_Decaps (
      Ctx       : in Out Execution_Context;
      CT_Hash   : in     Hash256;  -- Hash of ciphertext
      DK_Hash   : in     Hash256;  -- Hash of decapsulation key
      Result    : out    Syscall_Return
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Environment Syscalls
   ---------------------------------------------------------------------------

   --  CALLER: Get msg.sender
   procedure Sys_Handle_Caller (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   --  ADDRESS: Get self address
   procedure Sys_Handle_Address (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   --  CALLVALUE: Get msg.value
   procedure Sys_Handle_CallValue (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   --  BLOCKNUMBER: Get block number
   procedure Sys_Handle_BlockNumber (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   --  TIMESTAMP: Get block timestamp
   procedure Sys_Handle_Timestamp (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   --  CHAINID: Get chain ID
   procedure Sys_Handle_ChainID (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   --  GASPRICE: Get gas price
   procedure Sys_Handle_GasPrice (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   --  GASREMAINING: Get remaining gas
   procedure Sys_Handle_GasRemaining (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Balance Syscalls
   ---------------------------------------------------------------------------

   --  BALANCE: Get account balance
   procedure Sys_Handle_Balance (
      Ctx     : in     Execution_Context;
      Address : in     Contract_Address;
      Result  : out    Syscall_Return
   ) with
      Global => null;

   --  SELFBALANCE: Get self balance
   procedure Sys_Handle_SelfBalance (
      Ctx    : in     Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Call Syscalls
   ---------------------------------------------------------------------------

   --  CALL: Call another contract
   procedure Sys_Handle_Call (
      Ctx       : in Out Execution_Context;
      Target    : in     Contract_Address;
      Value     : in     U256;
      Gas_Limit : in     Gas_Amount;
      Data_Hash : in     Hash256;
      Data_Size : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => null;

   --  STATICCALL: Read-only call
   procedure Sys_Handle_StaticCall (
      Ctx       : in Out Execution_Context;
      Target    : in     Contract_Address;
      Gas_Limit : in     Gas_Amount;
      Data_Hash : in     Hash256;
      Data_Size : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => null;

   --  DELEGATECALL: Delegate call (preserve context)
   procedure Sys_Handle_DelegateCall (
      Ctx       : in Out Execution_Context;
      Target    : in     Contract_Address;
      Gas_Limit : in     Gas_Amount;
      Data_Hash : in     Hash256;
      Data_Size : in     Natural;
      Result    : out    Syscall_Return
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Control Syscalls
   ---------------------------------------------------------------------------

   --  RETURN: Return with data
   procedure Sys_Handle_Return (
      Ctx         : in Out Execution_Context;
      Return_Data : in     Aegis_Contract.Return_Data;
      Result      : out    Syscall_Return
   ) with
      Global => null;

   --  REVERT: Revert execution
   procedure Sys_Handle_Revert (
      Ctx         : in Out Execution_Context;
      Revert_Data : in     Hash256;
      Result      : out    Syscall_Return
   ) with
      Global => null;

   --  STOP: Stop execution
   procedure Sys_Handle_Stop (
      Ctx    : in Out Execution_Context;
      Result : out    Syscall_Return
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Event Syscalls
   ---------------------------------------------------------------------------

   --  LOG: Emit log with topics
   procedure Sys_Handle_Log (
      Ctx         : in Out Execution_Context;
      Topics      : in     Topic_Array;
      Topic_Count : in     Natural;
      Data_Hash   : in     Hash256;
      Data_Size   : in     Natural;
      Result      : out    Syscall_Return
   ) with
      Global => null,
      Pre    => Topic_Count <= Max_Topics;

   ---------------------------------------------------------------------------
   --  Capability Checking
   ---------------------------------------------------------------------------

   --  Check if syscall is allowed given current capabilities
   function Is_Syscall_Allowed (
      Capabilities : Capability_Mask;
      Syscall      : Syscall_Number
   ) return Boolean with
      Global => null;

   --  Check if state modification is allowed
   function Can_Modify_State (
      Ctx : Execution_Context
   ) return Boolean with
      Global => null;

   --  Check if value transfer is allowed
   function Can_Transfer_Value (
      Ctx : Execution_Context
   ) return Boolean with
      Global => null;

end Aegis_Syscall;
