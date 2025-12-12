pragma SPARK_Mode (Off);

with Ada.Strings.Unbounded;
with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types;    use Aegis_VM_Types;
with Aegis_U256;        use Aegis_U256;

--  Local_Executor: Minimal in-process executor for personal AegisVM use
--
--  This package wraps Node_Contract_Executor and Node_Contract_Registry
--  to provide a simple, single-node execution path for built-in contracts
--  (HelloCounter, SimpleToken, SimpleVault, QuantumDID, Staking, Governance)
--  without going through the full JSON-RPC / network stack.
--
--  It is intended for a trusted single-operator environment on macOS,
--  allowing direct execution of SPARK contracts against the local state.

package Local_Executor is

   --  Simple argument array type for CLI-style invocation
   type Arg_Array is array (Natural range <>)
     of Ada.Strings.Unbounded.Unbounded_String;

   --  High-level execution result for local use
   type Exec_Result is record
      Success    : Boolean;
      Gas_Used   : Gas_Amount;
      Return_Hex : Ada.Strings.Unbounded.Unbounded_String;
      Error      : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Execute a built-in contract entry point locally.
   --
   --  From_Address  : 32-byte contract address of the caller.
   --  Contract_Name : Logical contract name (e.g., "HelloCounter",
   --                  "SimpleToken", "SimpleVault", "QuantumDID",
   --                  "Staking", "Governance").
   --  Entry_Point   : Entry point name, e.g. "Increment", "Initialize".
   --  Args          : Optional string arguments (currently interpreted
   --                  as 32-byte hex-encoded values when present).
   --  Gas_Limit     : Maximum gas allowed for the call.
   --  Value         : Call value (U256) for contracts that depend on it.
   --
   --  Result        : Execution outcome, with hex-encoded return data
   --                  and any error message in Error when Success = False.
   procedure Execute_Local
     (From_Address  : Contract_Address;
      Contract_Name : String;
      Entry_Point   : String;
      Args          : Arg_Array;
      Gas_Limit     : Gas_Amount;
      Value         : U256;
      Result        : out Exec_Result);

   ---------------------------------------------------------------------------
   --  State Management
   ---------------------------------------------------------------------------

   --  Manually save state to disk (normally happens automatically)
   procedure Save_State;

   --  Reset executor state (for testing or re-initialization)
   procedure Reset_State;

   --  Get the state directory path being used
   function Get_State_Directory return String;

end Local_Executor;
