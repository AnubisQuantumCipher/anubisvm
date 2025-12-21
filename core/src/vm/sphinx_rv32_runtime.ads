--  SPHINX RV32 Runtime: Complete Contract Execution Pipeline
--
--  This package provides the top-level interface for executing RV32 contracts.
--  It integrates:
--  - ELF loading (sphinx_rv32_loader)
--  - Instruction execution (sphinx_rv32_execute)
--  - Storage callbacks (sphinx_rv32_context -> aegis_execution)
--  - Crypto operations (sphinx_rv32_context -> ankh crypto)
--
--  This is the main entry point for running contracts in the AnubisVM.
--
--  SPARK Mode: Off (uses context/callback infrastructure)

pragma SPARK_Mode (Off);

with System;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;
with Sphinx_RV32_Context; use Sphinx_RV32_Context;

package Sphinx_RV32_Runtime is

   ---------------------------------------------------------------------------
   --  Runtime Configuration
   ---------------------------------------------------------------------------

   --  Maximum steps before forced termination (anti-DoS)
   Default_Max_Steps : constant := 10_000_000;

   ---------------------------------------------------------------------------
   --  Contract Execution Result
   ---------------------------------------------------------------------------

   type Execution_Status is (
      Status_Success,        -- Contract returned normally
      Status_Revert,         -- Contract explicitly reverted
      Status_Out_Of_Gas,     -- Gas exhausted
      Status_Invalid_ELF,    -- ELF loading failed
      Status_Trap,           -- Runtime trap (illegal instruction, etc.)
      Status_Timeout         -- Max steps exceeded
   );

   type Contract_Result is record
      Status      : Execution_Status;
      Gas_Used    : Gas_Amount;
      Return_Data : Hash256;
      Return_Size : Natural;
      Trap        : Trap_Cause;
      Exit_Code   : Integer_32;
   end record;

   Failed_Result : constant Contract_Result := (
      Status      => Status_Invalid_ELF,
      Gas_Used    => 0,
      Return_Data => (others => 0),
      Return_Size => 0,
      Trap        => Trap_None,
      Exit_Code   => -1
   );

   ---------------------------------------------------------------------------
   --  Storage Callback Setup
   ---------------------------------------------------------------------------

   --  Callback type for storage operations
   type Storage_Callback_State is record
      Target_Address : Aegis_VM_Types.Contract_Address;
      Load_Callback  : System.Address;
      Store_Callback : System.Address;
   end record;

   --  Create default storage callbacks (in-memory for testing)
   function Create_Test_Storage_Callbacks return Storage_Callback_State;

   ---------------------------------------------------------------------------
   --  Contract Execution
   ---------------------------------------------------------------------------

   --  Execute contract from ELF binary
   --
   --  This is the main entry point for contract execution. It:
   --  1. Loads the ELF binary into interpreter memory
   --  2. Sets up the execution context (caller, value, etc.)
   --  3. Configures storage callbacks
   --  4. Runs the interpreter until completion or trap
   --  5. Returns the execution result
   --
   function Execute_Contract (
      ELF_Binary : Byte_Array;
      Calldata   : Byte_Array;
      Caller     : Contract_Address;
      Self       : Contract_Address;
      Call_Value : U256;
      Gas_Limit  : Gas_Amount;
      Storage    : Storage_Callback_State;
      Max_Steps  : Natural := Default_Max_Steps
   ) return Contract_Result;

   --  Simplified execution for testing (no storage callbacks)
   function Execute_Contract_Simple (
      ELF_Binary : Byte_Array;
      Calldata   : Byte_Array;
      Gas_Limit  : Gas_Amount
   ) return Contract_Result;

   ---------------------------------------------------------------------------
   --  State Access (for testing/debugging)
   ---------------------------------------------------------------------------

   --  Get the last interpreter state (for inspection after execution)
   function Get_Last_State return Interpreter_State;

   --  Get the last context (for inspection)
   function Get_Last_Context return Interpreter_Context_Ptr;

private

   --  Internal storage for last execution state
   Last_State   : Interpreter_State;
   Last_Context : Interpreter_Context_Ptr := null;

end Sphinx_RV32_Runtime;
