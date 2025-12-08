pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Storage; use Aegis_Storage;
with Aegis_Gas; use Aegis_Gas;
with Aegis_Sandbox; use Aegis_Sandbox;
with Aegis_Contract; use Aegis_Contract;

--  AEGIS Execution: Contract Execution Context
--
--  This package manages the execution context for KHEPRI native contracts.
--  It coordinates between the sandbox, gas metering, storage, and contract
--  interface to execute transactions.
--
--  Key Features:
--  - Transaction execution management
--  - Call stack management
--  - State transition handling
--  - Gas accounting with certification discounts
--  - Snapshot/rollback for nested calls
--
--  Execution Flow:
--  1. Begin transaction (create context)
--  2. Execute contract call (with gas metering)
--  3. Handle nested calls (push/pop frames)
--  4. Commit or rollback state
--  5. Return execution result
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6: Runtime

package Aegis_Execution with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Execution Context Types
   ---------------------------------------------------------------------------

   --  Execution mode
   type Execution_Mode is (
      Mode_Normal,      -- Standard execution
      Mode_Static,      -- Read-only (staticcall)
      Mode_Delegate,    -- Delegate context
      Mode_Create       -- Contract creation
   );

   --  Full execution context
   type Execution_Context is record
      --  Transaction info
      Origin       : Contract_Address;
      Gas_Price    : U256;
      Block_Number : U256;
      Timestamp    : U256;
      Chain_ID     : U256;

      --  Current execution
      Mode         : Execution_Mode;
      Sandbox      : Sandbox_Context;

      --  State management
      Effects      : Transaction_Effects;
      Snapshots    : Snapshot_Stack;
      Snapshot_Depth : Natural;

      --  Certification
      Certification : Certification_Level;
   end record;

   ---------------------------------------------------------------------------
   --  Context Management
   ---------------------------------------------------------------------------

   --  Create initial execution context for transaction
   function Create_Context (
      Origin       : Contract_Address;
      Gas_Limit    : Gas_Amount;
      Gas_Price    : U256;
      Block_Number : U256;
      Timestamp    : U256;
      Chain_ID     : U256;
      Certification : Certification_Level
   ) return Execution_Context with
      Global => null;

   --  Enter a new call frame
   procedure Enter_Call (
      Ctx       : in out Execution_Context;
      Caller    : in     Contract_Address;
      Callee    : in     Contract_Address;
      Value     : in     U256;
      Gas_Limit : in     Gas_Amount;
      Call_Kind : in     Call_Type;
      Success   : out    Boolean
   ) with
      Global => null;

   --  Exit current call frame
   procedure Exit_Call (
      Ctx       : in Out Execution_Context;
      Success   : in     Boolean;
      Gas_Used  : out    Gas_Amount
   ) with
      Global => null;

   --  Get current call depth
   function Current_Depth (Ctx : Execution_Context) return Call_Depth with
      Global => null;

   ---------------------------------------------------------------------------
   --  Snapshot Management
   ---------------------------------------------------------------------------

   --  Take a state snapshot before nested call
   procedure Take_Snapshot (
      Ctx     : in Out Execution_Context;
      ID      : out    Snapshot_ID;
      Success : out    Boolean
   ) with
      Global => null;

   --  Commit changes up to snapshot
   procedure Commit_Snapshot (
      Ctx : in Out Execution_Context;
      ID  : in     Snapshot_ID
   ) with
      Global => null;

   --  Rollback to snapshot
   procedure Rollback_To_Snapshot (
      Ctx : in Out Execution_Context;
      ID  : in     Snapshot_ID
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Gas Management
   ---------------------------------------------------------------------------

   --  Consume gas with certification discount
   procedure Use_Gas (
      Ctx     : in Out Execution_Context;
      Amount  : in     Gas_Amount;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Amount <= Aegis_Gas.Max_Safe_Base_Gas;

   --  Get remaining gas
   function Gas_Remaining (Ctx : Execution_Context) return Gas_Amount with
      Global => null;

   --  Refund gas (for storage clears, etc.)
   procedure Refund (
      Ctx    : in Out Execution_Context;
      Amount : in     Gas_Amount
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  State Access
   ---------------------------------------------------------------------------

   --  Load storage value (uses external state trie)
   procedure Storage_Load (
      Ctx     : in Out Execution_Context;
      Address : in     Contract_Address;
      Key     : in     Storage_Key;
      Value   : out    Storage_Value;
      Success : out    Boolean
   ) with
      SPARK_Mode => Off;

   --  Store storage value (uses external state trie)
   procedure Storage_Store (
      Ctx     : in Out Execution_Context;
      Address : in     Contract_Address;
      Key     : in     Storage_Key;
      Value   : in     Storage_Value;
      Success : out    Boolean
   ) with
      SPARK_Mode => Off;

   --  Get account balance (uses external state trie)
   function Get_Balance (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return U256 with
      SPARK_Mode => Off;

   --  Transfer value between accounts (uses external state trie)
   procedure Transfer_Value (
      Ctx     : in Out Execution_Context;
      From    : in     Contract_Address;
      To      : in     Contract_Address;
      Amount  : in     U256;
      Success : out    Boolean
   ) with
      SPARK_Mode => Off;

   ---------------------------------------------------------------------------
   --  Environment Access
   ---------------------------------------------------------------------------

   --  Get caller address (msg.sender)
   function Get_Caller (Ctx : Execution_Context) return Contract_Address with
      Global => null;

   --  Get current contract address (address(this))
   function Get_Address (Ctx : Execution_Context) return Contract_Address with
      Global => null;

   --  Get call value (msg.value)
   function Get_Call_Value (Ctx : Execution_Context) return U256 with
      Global => null;

   --  Get transaction origin (tx.origin)
   function Get_Origin (Ctx : Execution_Context) return Contract_Address with
      Global => null;

   --  Get block number
   function Get_Block_Number (Ctx : Execution_Context) return U256 with
      Global => null;

   --  Get block timestamp
   function Get_Timestamp (Ctx : Execution_Context) return U256 with
      Global => null;

   --  Get chain ID
   function Get_Chain_ID (Ctx : Execution_Context) return U256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Event Emission
   ---------------------------------------------------------------------------

   --  Emit log with topics
   procedure Emit_Log (
      Ctx        : in Out Execution_Context;
      Topics     : in     Topic_Array;
      Topic_Count : in    Natural;
      Data       : in     Log_Data_Buffer;
      Data_Size  : in     Natural;
      Success    : out    Boolean
   ) with
      Global => null,
      Pre    => Topic_Count <= Max_Topics;

   ---------------------------------------------------------------------------
   --  Execution Finalization
   ---------------------------------------------------------------------------

   --  Finalize successful execution
   procedure Finalize_Success (
      Ctx         : in Out Execution_Context;
      Return_Data : in     Aegis_Contract.Return_Data;
      Result      : out    Execution_Result
   ) with
      Global => null;

   --  Finalize reverted execution
   procedure Finalize_Revert (
      Ctx         : in Out Execution_Context;
      Revert_Data : in     Hash256;
      Result      : out    Execution_Result
   ) with
      Global => null;

   --  Finalize failed execution (out of gas, etc.)
   procedure Finalize_Failure (
      Ctx    : in Out Execution_Context;
      Status : in     Execution_Status;
      Result : out    Execution_Result
   ) with
      Global => null,
      Pre    => Status /= Success;

end Aegis_Execution;
