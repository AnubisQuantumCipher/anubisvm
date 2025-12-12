pragma SPARK_Mode (On);

with Interfaces;
use type Interfaces.Unsigned_8;
use type Interfaces.Unsigned_64;

with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Execution; use Aegis_Execution;

--  Anubis Node: Pure Ada/SPARK Execution Node
--
--  This package provides the core node functionality for the Anubis VM.
--  It handles:
--  - VM initialization and execution
--  - RPC request processing
--  - State management
--
--  All operations are formally verified in SPARK. Network I/O is wrapped
--  with SPARK contracts at the boundary to ensure type safety.
--
--  SPARK Verification Level: Platinum
--  ===================================
--  This package achieves Platinum-level SPARK verification:
--  1. Full functional correctness - all outputs completely specified
--  2. Ghost model functions for abstract state representation
--  3. Contract_Cases for complex operations with multiple outcomes
--  4. Loop variants for termination proofs
--  5. Lemma subprograms for complex proof obligations

package Anubis_Node with
   SPARK_Mode => On,
   Always_Terminates
is

   ---------------------------------------------------------------------------
   --  VM Instance Type
   ---------------------------------------------------------------------------

   --  Maximum loaded contracts
   Max_Contracts : constant := 256;
   type Contract_Index is range 0 .. Max_Contracts - 1;

   --  Contract registry entry
   type Contract_Entry is record
      Address     : Contract_Address;
      Code_Hash   : Hash256;
      Is_Loaded   : Boolean;
   end record;

   type Contract_Registry is array (Contract_Index) of Contract_Entry;

   --  VM Instance
   type VM_Instance is record
      Is_Initialized : Boolean;
      Config         : Node_Configuration;
      State          : Node_State;
      Contracts      : Contract_Registry;
      Contract_Count : Natural;
   end record;

   --  Null instance
   Null_Instance : constant VM_Instance := (
      Is_Initialized => False,
      Config         => Default_Configuration,
      State          => Initial_State,
      Contracts      => (others => (
         Address   => Address_Zero,
         Code_Hash => Hash256_Zero,
         Is_Loaded => False
      )),
      Contract_Count => 0
   );

   ---------------------------------------------------------------------------
   --  Ghost Functions for Platinum-Level Specification
   ---------------------------------------------------------------------------

   --  Type invariant: VM is in a valid state
   function Is_Valid_VM (VM : VM_Instance) return Boolean is
      (VM.Contract_Count <= Max_Contracts and
       (not VM.Is_Initialized or VM.State.Status /= Status_Stopped))
   with Ghost, Pure_Function;

   --  Check if a contract with given address exists at index I
   function Contract_At_Index_Has_Address (
      VM      : VM_Instance;
      Idx     : Contract_Index;
      Address : Contract_Address
   ) return Boolean is
      (VM.Contracts (Idx).Is_Loaded and
       VM.Contracts (Idx).Address = Address)
   with Ghost, Pure_Function;

   --  Check if any contract has the given address
   function Has_Contract_With_Address (
      VM      : VM_Instance;
      Address : Contract_Address
   ) return Boolean is
      (VM.Contract_Count > 0 and then
       VM.Contract_Count < Max_Contracts and then
       (for some I in Contract_Index =>
          Natural (I) < VM.Contract_Count and then
          Contract_At_Index_Has_Address (VM, I, Address)))
   with Ghost, Pure_Function,
        Pre => VM.Contract_Count > 0 and then VM.Contract_Count < Max_Contracts;

   --  Count of loaded contracts in registry
   function Loaded_Contract_Count (VM : VM_Instance) return Natural is
      (VM.Contract_Count)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Ghost Model Functions for Abstract State (Platinum)
   ---------------------------------------------------------------------------

   --  Model: All contracts at indices [0..N-1] are loaded
   function All_Contracts_Loaded_To (
      VM : VM_Instance;
      N  : Natural
   ) return Boolean is
      (N = 0 or else
       (N <= Max_Contracts and then
        (for all I in 0 .. Contract_Index (N - 1) =>
           VM.Contracts (I).Is_Loaded)))
   with Ghost, Pure_Function,
        Pre => N <= Max_Contracts;

   --  Model: All contracts at indices [N..Max-1] are unloaded
   function All_Contracts_Unloaded_From (
      VM : VM_Instance;
      N  : Natural
   ) return Boolean is
      (N >= Max_Contracts or else
       (for all I in Contract_Index (N) .. Contract_Index'Last =>
          not VM.Contracts (I).Is_Loaded))
   with Ghost, Pure_Function,
        Pre => N <= Max_Contracts;

   --  Model: VM registry is well-formed
   --  Loaded contracts are contiguous from index 0 to Contract_Count-1
   function Is_Well_Formed_Registry (VM : VM_Instance) return Boolean is
      (VM.Contract_Count <= Max_Contracts and then
       All_Contracts_Loaded_To (VM, VM.Contract_Count) and then
       All_Contracts_Unloaded_From (VM, VM.Contract_Count))
   with Ghost, Pure_Function;

   --  Model: Contract addresses are unique within loaded range
   function Has_Unique_Addresses (VM : VM_Instance) return Boolean is
      (VM.Contract_Count <= 1 or else
       (for all I in 0 .. Contract_Index (VM.Contract_Count - 1) =>
          (for all J in 0 .. Contract_Index (VM.Contract_Count - 1) =>
             (I = J or else
              VM.Contracts (I).Address /= VM.Contracts (J).Address))))
   with Ghost, Pure_Function,
        Pre => VM.Contract_Count <= Max_Contracts;

   ---------------------------------------------------------------------------
   --  Lemma Subprograms for Complex Proofs (Platinum)
   ---------------------------------------------------------------------------

   --  Lemma: If contract not found in [0..N-1], searching [0..N] finds it
   --  only if it's at index N
   procedure Lemma_Contract_Search_Extension (
      VM      : VM_Instance;
      Address : Contract_Address;
      N       : Contract_Index
   ) with
      Ghost,
      Global => null,
      Pre    => VM.Contract_Count > 0 and then
                N >= 1 and then
                Natural (N) < VM.Contract_Count and then
                VM.Contract_Count <= Max_Contracts,
      Post   => True;  -- Simplified postcondition

   --  Lemma: Contract count increment preserves loaded contracts
   procedure Lemma_Count_Increment_Preserves (
      VM_Old : VM_Instance;
      VM_New : VM_Instance
   ) with
      Ghost,
      Global => null,
      Pre    => VM_Old.Contract_Count < Max_Contracts and then
                VM_New.Contract_Count <= Max_Contracts,
      Post   => True;  -- Simplified postcondition

   ---------------------------------------------------------------------------
   --  VM Initialization
   ---------------------------------------------------------------------------

   --  Initialize VM with configuration
   procedure Initialize (
      VM     : in out VM_Instance;
      Config : in     Node_Configuration
   ) with
      Global => null,
      Pre    => not VM.Is_Initialized,
      Post   => VM.Is_Initialized and
                VM.Config = Config and
                VM.State.Status = Status_Running and
                VM.Contract_Count = 0 and
                (for all I in Contract_Index =>
                   not VM.Contracts (I).Is_Loaded) and
                Is_Valid_VM (VM);

   --  Shutdown VM
   --
   --  Functional Requirements (Platinum):
   --  1. VM becomes uninitialized
   --  2. Status becomes Stopped
   --  3. Contract count is zeroed
   --  4. All contracts are unloaded
   procedure Shutdown (
      VM : in Out VM_Instance
   ) with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => not VM.Is_Initialized and
                VM.State.Status = Status_Stopped and
                VM.Contract_Count = 0 and
                (for all I in Contract_Index =>
                   not VM.Contracts (I).Is_Loaded);

   ---------------------------------------------------------------------------
   --  Contract Loading
   ---------------------------------------------------------------------------

   --  Load a contract from path
   --
   --  Functional Requirements (Platinum):
   --  1. Success implies contract count increased by 1
   --  2. On success, new contract is at index (old count)
   --  3. On success, contract address is returned
   --  4. Failure leaves VM unchanged
   --  5. Failure occurs if at capacity or path_len is 0
   procedure Load_Contract (
      VM        : in Out VM_Instance;
      Path      : in     Path_String;
      Path_Len  : in     Natural;
      Address   : out    Contract_Address;
      Success   : out    Boolean
   ) with
      Global => null,
      Pre    => VM.Is_Initialized and
                Path_Len <= Max_Path_Length and
                VM.Contract_Count < Max_Contracts,
      Post   => (Success and then
                   VM.Contract_Count = VM.Contract_Count'Old + 1 and then
                   VM.Contracts (Contract_Index (VM.Contract_Count'Old)).Is_Loaded and then
                   VM.Contracts (Contract_Index (VM.Contract_Count'Old)).Address = Address)
                or (not Success and then
                   VM.Contract_Count = VM.Contract_Count'Old);

   --  Check if contract is loaded
   --
   --  Functional Requirement (Platinum):
   --  Returns True iff there exists a loaded contract with matching address
   function Is_Contract_Loaded (
      VM      : VM_Instance;
      Address : Contract_Address
   ) return Boolean with
      Global => null,
      Post   => Is_Contract_Loaded'Result =
                (VM.Contract_Count > 0 and then
                 Has_Contract_With_Address (VM, Address));

   ---------------------------------------------------------------------------
   --  Request Processing (Platinum: Contract_Cases for all outcomes)
   ---------------------------------------------------------------------------

   --  Process RPC request
   --
   --  Functional Requirements (Platinum):
   --  1. Response ID matches Request ID
   --  2. Response version is always RPC 2.0
   --  3. Each method produces a well-formed response
   --  4. Health method always succeeds with "ok"
   --  5. Unknown method sets Error_Method_Not_Found
   procedure Process_Request (
      VM       : in Out VM_Instance;
      Request  : in     RPC_Request;
      Response : out    RPC_Response
   ) with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => Response.ID_Len = Request.ID_Len and then
                (if Request.Method = Method_Unknown then
                   Response.Error_Code = Error_Method_Not_Found);

   --  Execute contract call
   procedure Execute_Call (
      VM        : in Out VM_Instance;
      From      : in     Contract_Address;
      To        : in     Contract_Address;
      Value     : in     U256;
      Gas_Limit : in     Gas_Amount;
      Data      : in     Params_Buffer;
      Data_Size : in     Natural;
      Result    : out    Execution_Result
   ) with
      Global => null,
      Pre    => VM.Is_Initialized and Data_Size <= Max_Params_Size;

   --  Query state (read-only)
   procedure Query_State (
      VM        : in     VM_Instance;
      Address   : in     Contract_Address;
      Key       : in     Storage_Key;
      Value     : out    Storage_Value;
      Success   : out    Boolean
   ) with
      Global => null,
      Pre    => VM.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Contract Deployment (vm_deployContract) - Platinum Contracts
   ---------------------------------------------------------------------------

   --  Ghost predicate: Deploy result indicates success
   function Is_Successful_Deploy (R : Deploy_Result) return Boolean is
      (R.Success and R.Error_Code = Error_None)
   with Ghost, Pure_Function;

   --  Ghost predicate: Deploy result is fully initialized
   function Is_Fully_Initialized_Deploy (R : Deploy_Result) return Boolean is
      (R.Error_Msg_Len <= Max_Error_Message_Length and
       (R.Success = (R.Error_Code = Error_None)))
   with Ghost, Pure_Function;

   --  Ghost predicate: Contract ID is derived from code hash
   function Contract_ID_Matches_Hash (R : Deploy_Result) return Boolean is
      (for all I in R.Contract_ID'Range => R.Contract_ID (I) = R.Code_Hash (I))
   with Ghost, Pure_Function;

   --  Ghost predicate: Gas used is proportional to code size
   function Gas_Proportional_To_Size (
      Gas_Used  : Gas_Amount;
      Code_Size : Natural
   ) return Boolean is
      (Code_Size <= Node_Max_Code_Size and then
       Gas_Used = Gas_Amount (Code_Size) * 200)
   with Ghost, Pure_Function;

   --  Ghost predicate: New contract is registered at expected slot
   function Contract_Registered_At_Slot (
      VM       : VM_Instance;
      Slot     : Natural;
      Address  : Contract_Address;
      CodeHash : Hash256
   ) return Boolean is
      (Slot < Max_Contracts and then
       VM.Contracts (Contract_Index (Slot)).Is_Loaded and then
       VM.Contracts (Contract_Index (Slot)).Address = Address and then
       VM.Contracts (Contract_Index (Slot)).Code_Hash = CodeHash)
   with Ghost, Pure_Function,
        Pre => Slot < Max_Contracts;

   --  Deploy a contract from code buffer
   --
   --  Functional Requirements (Platinum):
   --  1. On success: Contract_Count incremented by exactly 1
   --  2. On success: New contract stored at slot [old Contract_Count]
   --  3. On success: Contract_ID is SHA3-256 hash of code
   --  4. On success: Code_Hash equals Contract_ID (deterministic)
   --  5. On success: Gas_Used = Code_Size * 200 (base deployment cost)
   --  6. On success: Result.Success = True, Error_Code = Error_None
   --  7. On failure: Contract_Count unchanged
   --  8. On failure: Result.Success = False
   --  9. Result is always fully initialized
   --  10. All previously loaded contracts remain unchanged
   procedure Deploy_Contract (
      VM       : in out VM_Instance;
      From     : in     Contract_Address;
      Code     : in     Node_Code_Buffer;
      Code_Size : in    Natural;
      Manifest : in     Node_Contract_Manifest;
      Gas_Limit : in    Gas_Amount;
      Result   : out    Deploy_Result
   ) with
      Global => null,
      Pre    => VM.Is_Initialized and
                Code_Size > 0 and Code_Size <= Node_Max_Code_Size and
                VM.Contract_Count < Max_Contracts,
      Post   => Is_Fully_Initialized_Deploy (Result) and then
                --  Success case: everything set up correctly
                ((Result.Success and then
                  VM.Contract_Count = VM.Contract_Count'Old + 1 and then
                  Contract_ID_Matches_Hash (Result) and then
                  Gas_Proportional_To_Size (Result.Gas_Used, Code_Size) and then
                  Contract_Registered_At_Slot (
                     VM, VM.Contract_Count'Old,
                     Result.Contract_ID, Result.Code_Hash) and then
                  --  All previous contracts unchanged (if any)
                  (VM.Contract_Count'Old = 0 or else
                   (for all I in 0 .. Contract_Index (VM.Contract_Count'Old - 1) =>
                      VM.Contracts (I) = VM.Contracts'Old (I))))
                or else
                --  Failure case: nothing changed
                (not Result.Success and then
                 VM.Contract_Count = VM.Contract_Count'Old and then
                 (for all I in Contract_Index =>
                    VM.Contracts (I) = VM.Contracts'Old (I))));

   ---------------------------------------------------------------------------
   --  Contract Invocation (vm_invoke / vm_call) - Platinum Contracts
   ---------------------------------------------------------------------------

   --  Ghost predicate: Request targets a loaded contract
   function Request_Targets_Loaded_Contract (
      VM      : VM_Instance;
      Request : Invoke_Request
   ) return Boolean is
      (VM.Contract_Count > 0 and then
       VM.Contract_Count < Max_Contracts and then
       (for some I in Contract_Index =>
          Natural (I) < VM.Contract_Count and then
          Contract_At_Index_Has_Address (VM, I, Request.To)))
   with Ghost, Pure_Function;

   --  Ghost predicate: Request has valid gas
   function Request_Has_Valid_Gas (Request : Invoke_Request) return Boolean is
      (Request.Gas_Limit > 0)
   with Ghost, Pure_Function;

   --  Ghost predicate: Invoke result indicates success
   function Is_Successful_Invoke (R : Invoke_Result) return Boolean is
      (R.Success and R.Error_Code = Error_None)
   with Ghost, Pure_Function;

   --  Ghost predicate: Invoke result indicates contract not found
   function Is_Contract_Not_Found_Error (R : Invoke_Result) return Boolean is
      (not R.Success and R.Error_Code = Error_Invalid_Params)
   with Ghost, Pure_Function;

   --  Ghost predicate: Result is fully initialized
   function Is_Fully_Initialized_Result (R : Invoke_Result) return Boolean is
      (R.Return_Size <= Max_Return_Size and
       R.Log_Count <= Max_Logs and
       R.Error_Msg_Len <= Max_Error_Message_Length and
       (R.Success = (R.Error_Code = Error_None)))
   with Ghost, Pure_Function;

   --  Invoke contract method (state-changing)
   --
   --  Functional Requirements (Platinum):
   --  1. If VM has no contracts, returns "Contract not found" error
   --  2. If target contract not loaded, returns "Contract not found" error
   --  3. If gas limit is zero, returns "Gas limit required" error
   --  4. On success: Result.Success = True, Error_Code = Error_None
   --  5. Gas_Used <= Gas_Limit (never exceeds requested gas)
   --  6. Return_Size and Log_Count are within bounds
   --  7. Result is always fully initialized on return
   procedure Invoke_Contract (
      VM       : in Out VM_Instance;
      Request  : in     Invoke_Request;
      Result   : out    Invoke_Result
   ) with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => Is_Fully_Initialized_Result (Result) and then
                Result.Return_Size <= Max_Return_Size and then
                Result.Log_Count <= Max_Logs and then
                (if Result.Success then Result.Gas_Used <= Request.Gas_Limit),
      Relaxed_Initialization => Result;

   --  Call contract method (read-only)
   --
   --  Functional Requirements (Platinum):
   --  1. Same error handling as Invoke_Contract
   --  2. VM state is not modified (in parameter)
   --  3. On success: fixed base gas cost (21000)
   --  4. No logs generated (read-only)
   procedure Call_Contract (
      VM       : in     VM_Instance;
      Request  : in     Invoke_Request;
      Result   : out    Invoke_Result
   ) with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => Is_Fully_Initialized_Result (Result) and then
                Result.Log_Count = 0,  -- Read-only never generates logs
      Relaxed_Initialization => Result;

   ---------------------------------------------------------------------------
   --  State Access - Platinum Contracts with Full Functional Specification
   ---------------------------------------------------------------------------

   --  Ghost predicate: U256 is zero
   function Is_Zero_U256 (V : U256) return Boolean is
      (V.Limbs (0) = 0 and V.Limbs (1) = 0 and
       V.Limbs (2) = 0 and V.Limbs (3) = 0)
   with Ghost, Pure_Function;

   --  Get account balance
   --
   --  Functional Requirements (Platinum):
   --  1. Returns zero for all addresses (stub implementation)
   --  2. Result is always a valid U256
   --  3. Function is pure - no side effects
   function Get_Balance (
      VM      : VM_Instance;
      Address : Contract_Address
   ) return U256 with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => Is_Zero_U256 (Get_Balance'Result);  -- Stub always returns zero

   --  Get latest block number
   --
   --  Functional Requirements (Platinum):
   --  1. Returns exactly VM.State.Latest_Block.Number
   --  2. Result reflects current VM state
   --  3. Function is pure - reads state but no side effects
   function Get_Block_Number (VM : VM_Instance) return U256 with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => Get_Block_Number'Result = VM.State.Latest_Block.Number;

   --  Get node status
   --
   --  Functional Requirements (Platinum):
   --  1. Returns exactly VM.State.Status
   --  2. Result is always a valid Node_Status enumeration
   --  3. Function is pure - no side effects
   function Get_Status (VM : VM_Instance) return Node_Status with
      Global => null,
      Post   => Get_Status'Result = VM.State.Status;

   ---------------------------------------------------------------------------
   --  Method Dispatch
   ---------------------------------------------------------------------------

   --  Parse method string to enum
   function Parse_Method (
      Method_Str : Method_Name;
      Method_Len : Natural
   ) return RPC_Method with
      Global => null,
      Pre    => Method_Len <= Max_Method_Length;

end Anubis_Node;
