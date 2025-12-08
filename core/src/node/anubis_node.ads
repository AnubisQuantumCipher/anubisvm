pragma SPARK_Mode (On);

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

package Anubis_Node with
   SPARK_Mode => On
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
                VM.State.Status = Status_Running;

   --  Shutdown VM
   procedure Shutdown (
      VM : in Out VM_Instance
   ) with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => not VM.Is_Initialized and
                VM.State.Status = Status_Stopped;

   ---------------------------------------------------------------------------
   --  Contract Loading
   ---------------------------------------------------------------------------

   --  Load a contract from path
   procedure Load_Contract (
      VM        : in Out VM_Instance;
      Path      : in     Path_String;
      Path_Len  : in     Natural;
      Address   : out    Contract_Address;
      Success   : out    Boolean
   ) with
      Global => null,
      Pre    => VM.Is_Initialized and Path_Len <= Max_Path_Length,
      Post   => (Success and then VM.Contract_Count = VM.Contract_Count'Old + 1)
                or (not Success and then VM.Contract_Count = VM.Contract_Count'Old);

   --  Check if contract is loaded
   function Is_Contract_Loaded (
      VM      : VM_Instance;
      Address : Contract_Address
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Request Processing
   ---------------------------------------------------------------------------

   --  Process RPC request
   procedure Process_Request (
      VM       : in Out VM_Instance;
      Request  : in     RPC_Request;
      Response : out    RPC_Response
   ) with
      Global => null,
      Pre    => VM.Is_Initialized,
      Post   => Response.Version = RPC_Version_2_0;

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
   --  State Access
   ---------------------------------------------------------------------------

   --  Get account balance
   function Get_Balance (
      VM      : VM_Instance;
      Address : Contract_Address
   ) return U256 with
      Global => null,
      Pre    => VM.Is_Initialized;

   --  Get latest block number
   function Get_Block_Number (VM : VM_Instance) return U256 with
      Global => null,
      Pre    => VM.Is_Initialized;

   --  Get node status
   function Get_Status (VM : VM_Instance) return Node_Status with
      Global => null;

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
