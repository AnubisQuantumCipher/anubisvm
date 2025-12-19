pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;

--  CVM_Registry: Registry for Cryptographically Verified Modules
--
--  This package manages the registration and lookup of CVMs in the TEE.
--  Since CVMs are statically linked SPARK packages (not dynamic bytecode),
--  the registry is populated at compile time.
--
--  Registry operations:
--  1. Register: Add a CVM to the registry (compile-time)
--  2. Lookup: Find CVM by address (runtime)
--  3. Enumerate: List all registered CVMs
--
--  The registry maintains:
--  - CVM descriptor (info, capabilities, entry points)
--  - CVM state (persistent encrypted storage)
--  - Execution statistics

package CVM_Registry with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Registry Configuration
   ---------------------------------------------------------------------------

   --  Maximum number of registered CVMs
   Max_CVMs : constant := 64;

   --  Registry slot index
   subtype Registry_Index is Natural range 0 .. Max_CVMs - 1;

   ---------------------------------------------------------------------------
   --  Registry Entry
   ---------------------------------------------------------------------------

   --  Complete registry entry for a CVM
   type Registry_Entry is record
      --  CVM registration (handlers)
      Registration : CVM_Registration;

      --  CVM descriptor (cached)
      Descriptor : CVM_Descriptor;

      --  CVM state
      State : State_Array;

      --  Is this slot occupied?
      Occupied : Boolean;

      --  Execution count
      Exec_Count : Natural;
   end record;

   --  Empty registry entry
   Empty_Entry : constant Registry_Entry := (
      Registration => Empty_Registration,
      Descriptor   => Empty_Descriptor,
      State        => Empty_State,
      Occupied     => False,
      Exec_Count   => 0
   );

   ---------------------------------------------------------------------------
   --  Registry State
   ---------------------------------------------------------------------------

   --  The global registry (package state)
   --  Note: In SPARK, global state must be carefully managed

   --  Registry array type
   type Registry_Array is array (Registry_Index) of Registry_Entry;

   ---------------------------------------------------------------------------
   --  Registry Operations
   ---------------------------------------------------------------------------

   --  Initialize the registry
   --
   --  Must be called once at TEE startup.
   --  Clears all registry entries.
   procedure Initialize (
      Registry : out Registry_Array
   ) with
      Global => null,
      Post => (for all I in Registry'Range => not Registry (I).Occupied);

   --  Register a CVM
   --
   --  Registry : Registry to modify
   --  Reg      : CVM registration info
   --  Success  : True if registration succeeded
   --
   --  Fails if:
   --  - Registry is full
   --  - CVM with same address already exists
   procedure Register_CVM (
      Registry : in out Registry_Array;
      Reg      : CVM_Registration;
      Success  : out Boolean
   ) with
      Global => null;

   --  Lookup CVM by address
   --
   --  Registry : Registry to search
   --  Addr     : CVM address to find
   --  Index    : Output registry index if found
   --  Found    : True if CVM exists
   procedure Lookup_By_Address (
      Registry : Registry_Array;
      Addr     : CVM_Address;
      Index    : out Registry_Index;
      Found    : out Boolean
   ) with
      Global => null,
      Post => (if Found then Registry (Index).Occupied);

   --  Lookup CVM by code hash
   --
   --  Registry : Registry to search
   --  Code     : Code hash to find
   --  Index    : Output registry index if found
   --  Found    : True if CVM exists
   procedure Lookup_By_Code (
      Registry : Registry_Array;
      Code     : Code_Hash;
      Index    : out Registry_Index;
      Found    : out Boolean
   ) with
      Global => null,
      Post => (if Found then Registry (Index).Occupied);

   --  Get CVM descriptor
   --
   --  Registry : Registry to query
   --  Index    : Registry index
   --  Desc     : Output descriptor
   procedure Get_Descriptor (
      Registry : Registry_Array;
      Index    : Registry_Index;
      Desc     : out CVM_Descriptor
   ) with
      Global => null,
      Pre => Registry (Index).Occupied;

   --  Get CVM state
   --
   --  Registry : Registry to query
   --  Index    : Registry index
   --  State    : Output state
   procedure Get_State (
      Registry : Registry_Array;
      Index    : Registry_Index;
      State    : out State_Array
   ) with
      Global => null,
      Pre => Registry (Index).Occupied;

   --  Set CVM state
   --
   --  Registry : Registry to modify
   --  Index    : Registry index
   --  State    : New state
   procedure Set_State (
      Registry : in Out Registry_Array;
      Index    : Registry_Index;
      State    : State_Array
   ) with
      Global => null,
      Pre => Registry (Index).Occupied,
      Post => Registry (Index).Occupied;

   --  Count registered CVMs
   function CVM_Count (
      Registry : Registry_Array
   ) return Natural with
      Global => null,
      Post => CVM_Count'Result <= Max_CVMs;

   --  Check if CVM is active
   function Is_Active (
      Registry : Registry_Array;
      Index    : Registry_Index
   ) return Boolean with
      Global => null,
      Pre => Registry (Index).Occupied;

   --  Activate/deactivate CVM
   procedure Set_Active (
      Registry : in Out Registry_Array;
      Index    : Registry_Index;
      Active   : Boolean
   ) with
      Global => null,
      Pre => Registry (Index).Occupied;

   --  Increment execution counter
   procedure Inc_Exec_Count (
      Registry : in Out Registry_Array;
      Index    : Registry_Index
   ) with
      Global => null,
      Pre => Registry (Index).Occupied;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Compare addresses (constant-time)
   function Address_Equal (
      A : CVM_Address;
      B : CVM_Address
   ) return Boolean with
      Global => null;

   --  Compare code hashes (constant-time)
   function Code_Equal (
      A : Code_Hash;
      B : Code_Hash
   ) return Boolean with
      Global => null;

end CVM_Registry;
