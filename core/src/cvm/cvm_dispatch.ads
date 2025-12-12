pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;
with CVM_Registry; use CVM_Registry;

--  CVM_Dispatch: Execution dispatcher for Cryptographically Verified Modules
--
--  This package handles the dispatch of calls to CVMs:
--  1. Validate caller authorization
--  2. Find target CVM in registry
--  3. Locate entry point by selector
--  4. Execute with state management
--  5. Handle cross-CVM calls
--
--  Security properties:
--  - Authorization checked before execution
--  - State changes are atomic (all-or-nothing)
--  - Cross-CVM calls maintain call stack integrity
--  - Deterministic execution path

package CVM_Dispatch with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Dispatch Configuration
   ---------------------------------------------------------------------------

   --  Maximum call depth for cross-CVM calls
   Max_Call_Depth : constant := 16;

   --  Call stack entry
   type Call_Stack_Entry is record
      --  CVM registry index
      CVM_Index : Registry_Index;

      --  Caller context
      Context : Call_Context;

      --  Pre-call state (for rollback on failure)
      Saved_State : State_Array;
   end record;

   --  Call stack
   type Call_Stack is array (0 .. Max_Call_Depth - 1) of Call_Stack_Entry;

   --  Dispatch state
   type Dispatch_State is record
      --  Call stack
      Stack : Call_Stack;

      --  Current stack depth
      Depth : Natural;

      --  Is dispatch in progress?
      Active : Boolean;
   end record;

   --  Initial dispatch state
   Initial_Dispatch_State : constant Dispatch_State := (
      Stack  => (others => (
         CVM_Index   => 0,
         Context     => Empty_Context,
         Saved_State => Empty_State
      )),
      Depth  => 0,
      Active => False
   );

   ---------------------------------------------------------------------------
   --  Dispatch Operations
   ---------------------------------------------------------------------------

   --  Execute a CVM call
   --
   --  This is the main entry point for executing CVM transactions.
   --
   --  Registry : CVM registry
   --  DState   : Dispatch state (for nested calls)
   --  Context  : Call context
   --  Result   : Execution result
   --
   --  The function:
   --  1. Looks up target CVM
   --  2. Verifies authorization
   --  3. Finds entry point
   --  4. Executes with state management
   --  5. Commits or rolls back state
   procedure Execute_Call (
      Registry : in Out Registry_Array;
      DState   : in Out Dispatch_State;
      Context  : Call_Context;
      Result   : out Exec_Result
   ) with
      Global => null;

   --  Execute internal call (CVM-to-CVM)
   --
   --  Called when one CVM calls another.
   --  Automatically marks the call as internal.
   --
   --  Registry     : CVM registry
   --  DState       : Dispatch state
   --  From_CVM     : Calling CVM address
   --  To_CVM       : Target CVM address
   --  Entry_Point  : Method selector
   --  Params       : Call parameters
   --  Param_Len    : Parameter length
   --  Result       : Execution result
   procedure Execute_Internal_Call (
      Registry    : in Out Registry_Array;
      DState      : in Out Dispatch_State;
      From_CVM    : CVM_Address;
      To_CVM      : CVM_Address;
      Entry_Point : Method_Selector;
      Params      : Param_Buffer;
      Param_Len   : Natural;
      Result      : out Exec_Result
   ) with
      Global => null,
      Pre => DState.Depth < Max_Call_Depth;

   --  Validate call context
   --
   --  Checks that the call context is valid:
   --  - Target CVM exists and is active
   --  - Entry point exists
   --  - Caller is authorized
   function Validate_Context (
      Registry : Registry_Array;
      Context  : Call_Context
   ) return Exec_Status with
      Global => null;

   --  Get current call depth
   function Current_Depth (
      DState : Dispatch_State
   ) return Natural is (DState.Depth) with
      Global => null;

   --  Check if dispatch is active
   function Is_Dispatching (
      DState : Dispatch_State
   ) return Boolean is (DState.Active) with
      Global => null;

   ---------------------------------------------------------------------------
   --  State Management
   ---------------------------------------------------------------------------

   --  Save state for rollback
   procedure Push_State (
      DState    : in Out Dispatch_State;
      CVM_Index : Registry_Index;
      Context   : Call_Context;
      State     : State_Array
   ) with
      Global => null,
      Pre => DState.Depth < Max_Call_Depth,
      Post => DState.Depth = DState.Depth'Old + 1;

   --  Restore state on failure
   procedure Pop_And_Rollback (
      DState   : in Out Dispatch_State;
      Registry : in Out Registry_Array
   ) with
      Global => null,
      Pre => DState.Depth > 0,
      Post => DState.Depth = DState.Depth'Old - 1;

   --  Commit state changes
   procedure Pop_And_Commit (
      DState : in Out Dispatch_State
   ) with
      Global => null,
      Pre => DState.Depth > 0,
      Post => DState.Depth = DState.Depth'Old - 1;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Compute method selector from name
   --
   --  Name     : Method name string
   --  Selector : Output selector (SHA3-256 of name)
   procedure Compute_Selector (
      Name     : String;
      Selector : out Method_Selector
   ) with
      Global => null,
      Pre => Name'Length > 0 and Name'Length <= 256;

end CVM_Dispatch;
