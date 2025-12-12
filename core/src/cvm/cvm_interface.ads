pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;

--  CVM_Interface: Abstract interface for Cryptographically Verified Modules
--
--  All CVMs must implement this interface to be callable from the TEE runtime.
--  The interface is designed for formal verification:
--
--  1. Pure functions where possible
--  2. Explicit state threading (no hidden globals)
--  3. Strong contracts (Pre/Post) for all operations
--  4. Deterministic execution
--
--  CVM Lifecycle:
--  1. Registration: CVM code is compiled and linked into TEE
--  2. Initialization: CVM_Init called to set up initial state
--  3. Execution: CVM_Execute called for each transaction
--  4. State: Read/Write via State_Get/State_Set callbacks
--
--  Formal Verification (SPARK Gold):
--  - Entry point dispatch is proven safe (index in bounds)
--  - Authorization checks are complete
--  - State modifications are tracked
--  - Return data is properly bounded

package CVM_Interface with
   SPARK_Mode => On,
   Pure,
   Always_Terminates
is

   ---------------------------------------------------------------------------
   --  CVM Entry Point Type
   ---------------------------------------------------------------------------

   --  Entry point procedure type that all CVM methods must conform to
   --
   --  Context : Call context with caller, params, etc.
   --  State   : Current CVM state (in/out for modifications)
   --  Result  : Execution result with return data
   type CVM_Entry_Point is access procedure (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   );

   ---------------------------------------------------------------------------
   --  CVM Descriptor
   ---------------------------------------------------------------------------

   --  Maximum number of entry points per CVM
   Max_Entry_Points : constant := 32;

   --  Entry point index type (bounded for safety)
   subtype Entry_Point_Index is Natural range 0 .. Max_Entry_Points - 1;

   --  Entry point descriptor
   type Entry_Point_Desc is record
      --  Method selector (SHA3-256 of method name)
      Selector : Method_Selector;

      --  Entry point handler (null if not registered)
      Handler  : CVM_Entry_Point;

      --  Is this entry point callable externally?
      Public   : Boolean;

      --  Is this entry point read-only (no state modification)?
      ReadOnly : Boolean;
   end record;

   --  Ghost function: Verify entry point is registered
   function Is_Registered (EP : Entry_Point_Desc) return Boolean is
      (EP.Handler /= null)
   with Ghost, Pure_Function;

   --  Ghost function: Verify entry point is callable externally
   function Is_Public_Entry (EP : Entry_Point_Desc) return Boolean is
      (EP.Public)
   with Ghost, Pure_Function;

   --  Empty entry point (unregistered)
   Empty_Entry_Point : constant Entry_Point_Desc := (
      Selector => (others => 0),
      Handler  => null,
      Public   => False,
      ReadOnly => True
   );

   --  Entry point table
   type Entry_Point_Table is array (Entry_Point_Index) of Entry_Point_Desc;

   --  CVM descriptor (returned by Get_Descriptor)
   type CVM_Descriptor is record
      --  CVM information
      Info : CVM_Info;

      --  Number of registered entry points (0..Max_Entry_Points)
      Entry_Count : Natural;

      --  Entry point table (only first Entry_Count entries are valid)
      Entries : Entry_Point_Table;
   end record;

   --  Ghost function: Verify descriptor is well-formed
   function Is_Valid_Descriptor (D : CVM_Descriptor) return Boolean is
      (D.Entry_Count <= Max_Entry_Points and then
       Is_Valid_CVM_Info (D.Info))
   with Ghost, Pure_Function;

   --  Ghost function: Verify entry point index is valid
   function Valid_Entry_Index (D : CVM_Descriptor; Idx : Natural) return Boolean is
      (Idx < D.Entry_Count)
   with Ghost, Pure_Function;

   --  Empty descriptor
   Empty_Descriptor : constant CVM_Descriptor := (
      Info        => Empty_Info,
      Entry_Count => 0,
      Entries     => (others => Empty_Entry_Point)
   );

   ---------------------------------------------------------------------------
   --  CVM Interface Procedures
   ---------------------------------------------------------------------------

   --  Initialize CVM state
   --
   --  Called once when CVM is first deployed. Sets up initial state.
   --  Init_Params may contain constructor arguments.
   --
   --  Init_Params : Initialization parameters
   --  State       : Output initial state
   --  Success     : True if initialization succeeded
   type CVM_Init_Proc is access procedure (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   );

   --  Execute CVM entry point
   --
   --  Called for each transaction targeting this CVM.
   --  Dispatches to appropriate entry point based on selector.
   --
   --  Context : Call context (caller, params, etc.)
   --  State   : Current state (modified if write operation)
   --  Result  : Execution result
   type CVM_Execute_Proc is access procedure (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   );

   --  Get CVM descriptor
   --
   --  Returns CVM metadata and entry point table.
   --  Used by runtime for dispatch and capability checking.
   type CVM_Get_Descriptor_Func is access function return CVM_Descriptor;

   ---------------------------------------------------------------------------
   --  CVM Registration Record
   ---------------------------------------------------------------------------

   --  Complete CVM registration (all required interface components)
   type CVM_Registration is record
      --  Descriptor getter (returns metadata and entry points)
      Get_Descriptor : CVM_Get_Descriptor_Func;

      --  Initialization procedure (called on deployment)
      Init : CVM_Init_Proc;

      --  Execution procedure (called per transaction)
      Execute : CVM_Execute_Proc;
   end record;

   --  Ghost function: Verify registration is complete
   function Is_Valid_Registration (R : CVM_Registration) return Boolean is
      (R.Get_Descriptor /= null and then
       R.Init /= null and then
       R.Execute /= null)
   with Ghost, Pure_Function;

   --  Empty registration (not yet registered)
   Empty_Registration : constant CVM_Registration := (
      Get_Descriptor => null,
      Init           => null,
      Execute        => null
   );

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Find_Entry_Point: Find entry point by selector
   --
   --  Searches the entry point table for a matching method selector.
   --
   --  Desc     : CVM descriptor
   --  Selector : Method selector to find
   --  Index    : Output index if found (valid only if Found = True)
   --  Found    : True if entry point exists
   --
   --  Pre: Descriptor is well-formed
   --  Post: If found, index is valid for descriptor's entry count
   procedure Find_Entry_Point (
      Desc     : CVM_Descriptor;
      Selector : Method_Selector;
      Index    : out Natural;
      Found    : out Boolean
   ) with
      Global  => null,
      Depends => ((Index, Found) => (Desc, Selector)),
      Pre     => Is_Valid_Descriptor (Desc),
      Post    => (if Found then Valid_Entry_Index (Desc, Index)),
      Always_Terminates;

   --  Has_Capability: Check if capability set includes a capability
   --
   --  Caps : Capability set to check
   --  Cap  : Capability to look for
   --
   --  Returns True iff the capability is enabled
   function Has_Capability (
      Caps : Capability_Set;
      Cap  : CVM_Capability
   ) return Boolean is (Caps (Cap))
   with
      Global => null,
      Pure_Function;

   --  Is_Authorized: Verify caller authorization for entry point
   --
   --  Checks if the caller (from context) is authorized to call
   --  the specified entry point.
   --
   --  Authorization rules:
   --  - Public entry points: Always callable
   --  - Private entry points: Only callable by CVMs in same trust domain
   --  - Trust domain: First 8 bytes of address match
   --
   --  Context  : Current call context
   --  EP_Desc  : Target entry point descriptor
   --
   --  Returns True if caller is authorized
   function Is_Authorized (
      Context : Call_Context;
      EP_Desc : Entry_Point_Desc
   ) return Boolean with
      Global => null,
      Pre    => Is_Valid_Context (Context);

   --  Success_Result: Create success result with return data
   --
   --  Creates an Exec_Result with Success status and the provided
   --  return data copied into the return buffer.
   --
   --  Data : Return data to include
   --  Len  : Actual length of return data
   --
   --  Pre: Length is within bounds and data array has sufficient length
   --  Post: Result has Success status
   function Success_Result (
      Data : Byte_Array;
      Len  : Natural
   ) return Exec_Result with
      Global => null,
      Pre    => Len <= Max_Return_Size and then Data'Length >= Len,
      Post   => Result_Is_Success (Success_Result'Result) and then
                Success_Result'Result.Return_Len = Len;

   ---------------------------------------------------------------------------
   --  Serialization Helpers
   ---------------------------------------------------------------------------

   --  Pack_Natural: Serialize natural to big-endian bytes
   --
   --  Writes a natural value as 4 big-endian bytes to the buffer
   --  at the specified offset.
   --
   --  Value  : Natural value to serialize
   --  Buffer : Output buffer
   --  Offset : Starting offset in buffer
   --
   --  Pre: Buffer has at least Offset + 4 bytes
   procedure Pack_Natural (
      Value  : Natural;
      Buffer : out Byte_Array;
      Offset : Natural
   ) with
      Global  => null,
      Depends => (Buffer => (Value, Offset)),
      Pre     => Buffer'Length >= Offset + 4 and then
                 Buffer'Last < Natural'Last,
      Always_Terminates;

   --  Unpack_Natural: Deserialize natural from big-endian bytes
   --
   --  Reads 4 big-endian bytes from the buffer at the specified
   --  offset and returns the natural value.
   --
   --  Buffer : Input buffer
   --  Offset : Starting offset in buffer
   --
   --  Pre: Buffer has at least Offset + 4 bytes
   function Unpack_Natural (
      Buffer : Byte_Array;
      Offset : Natural
   ) return Natural with
      Global => null,
      Pre    => Buffer'Length >= Offset + 4 and then
                Buffer'Last < Natural'Last;

end CVM_Interface;
