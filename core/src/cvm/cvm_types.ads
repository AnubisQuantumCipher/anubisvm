pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

--  CVM_Types: Core types for Cryptographically Verified Modules
--
--  CVMs are native SPARK packages that are mathematically proven correct
--  at compile time. Unlike bytecode VMs, CVMs are statically linked into
--  the TEE binary and execute with full hardware speed.
--
--  Key properties:
--  - Type-safe: All operations are type-checked at compile time
--  - Memory-safe: No buffer overflows, use-after-free, etc.
--  - Deterministic: Same inputs always produce same outputs
--  - Post-quantum: All crypto uses ML-DSA-87, ML-KEM-1024, SHA3
--
--  CVM Address Format (AAS-001 v3.1):
--  - Entity type: Contract (c)
--  - Payload: SHA3-256("aegis-v1-mldsa87-c" || code_hash)
--  - Full format: mldsa87:network:c:chunked_payload-checksum
--
--  Formal Verification (SPARK Gold):
--  - All types have bounded ranges
--  - Array indices proven in bounds
--  - State slot operations proven safe
--  - Execution results well-formed

package CVM_Types with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  CVM Identifiers
   ---------------------------------------------------------------------------

   --  CVM code hash (SHA3-256 of the compiled SPARK package)
   subtype Code_Hash is Hash256;

   --  CVM instance address (derived from code hash per AAS-001)
   --  Uses Anubis_Types.Address (32 bytes)
   subtype CVM_Address is Anubis_Types.Address;

   --  Caller address (user or another CVM)
   subtype Caller_Address is Anubis_Types.Address;

   ---------------------------------------------------------------------------
   --  Execution Context Constants
   ---------------------------------------------------------------------------

   --  Maximum parameter size for CVM calls (4 KB)
   Max_Param_Size : constant := 4096;

   --  Maximum return value size (4 KB)
   Max_Return_Size : constant := 4096;

   --  Maximum state slot value size (1 KB per slot)
   Max_State_Size : constant := 1024;

   --  Maximum number of state slots per CVM
   Max_State_Slots : constant := 256;

   ---------------------------------------------------------------------------
   --  Buffer Types
   ---------------------------------------------------------------------------

   --  Parameter buffer for CVM calls
   subtype Param_Buffer is Byte_Array (0 .. Max_Param_Size - 1);

   --  Return value buffer
   subtype Return_Buffer is Byte_Array (0 .. Max_Return_Size - 1);

   --  State slot value
   subtype State_Value is Byte_Array (0 .. Max_State_Size - 1);

   --  State slot index (bounded type for safety)
   subtype State_Index is Natural range 0 .. Max_State_Slots - 1;

   ---------------------------------------------------------------------------
   --  Ghost Functions for Specification
   ---------------------------------------------------------------------------

   --  Ghost function: Verify address is well-formed
   function Is_Valid_Address (Addr : CVM_Address) return Boolean is
      (Addr'First = 0 and Addr'Last = 31)
   with Ghost, Pure_Function;

   --  Ghost function: Verify param buffer is well-formed
   function Is_Valid_Param_Buffer (P : Param_Buffer) return Boolean is
      (P'First = 0 and P'Last = Max_Param_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify return buffer is well-formed
   function Is_Valid_Return_Buffer (R : Return_Buffer) return Boolean is
      (R'First = 0 and R'Last = Max_Return_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify state value is well-formed
   function Is_Valid_State_Value (S : State_Value) return Boolean is
      (S'First = 0 and S'Last = Max_State_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify buffer is zeroed
   function Buffer_Is_Zero (B : Byte_Array) return Boolean is
      (for all I in B'Range => B (I) = 0)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Execution Status
   ---------------------------------------------------------------------------

   --  Result of CVM execution
   type Exec_Status is (
      Success,           -- Execution completed successfully
      Invalid_Caller,    -- Caller not authorized
      Invalid_Params,    -- Parameters failed validation
      Invalid_State,     -- State inconsistency detected
      Out_Of_Gas,        -- Resource limit exceeded (optional)
      Proof_Failed,      -- Runtime assertion failed (should never happen)
      Unknown_Method,    -- Entry point not found
      Internal_Error     -- Unexpected internal error
   );

   --  Ghost function: Verify status indicates success
   function Is_Success (S : Exec_Status) return Boolean is
      (S = Success)
   with Ghost, Pure_Function;

   --  Ghost function: Verify status indicates error
   function Is_Error (S : Exec_Status) return Boolean is
      (S /= Success)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Execution Result
   ---------------------------------------------------------------------------

   --  Execution result with return data
   type Exec_Result is record
      Status      : Exec_Status;
      Return_Len  : Natural;  -- Actual length of return data
      Return_Data : Return_Buffer;
   end record;

   --  Ghost function: Verify exec result is well-formed
   function Is_Valid_Exec_Result (R : Exec_Result) return Boolean is
      (R.Return_Len <= Max_Return_Size and then
       Is_Valid_Return_Buffer (R.Return_Data))
   with Ghost, Pure_Function;

   --  Ghost function: Verify result is successful
   function Result_Is_Success (R : Exec_Result) return Boolean is
      (Is_Success (R.Status))
   with Ghost, Pure_Function;

   --  Default empty result
   Empty_Result : constant Exec_Result := (
      Status      => Success,
      Return_Len  => 0,
      Return_Data => (others => 0)
   );

   --  Error result constructor
   --
   --  Creates a result indicating the specified error status.
   --  Return data is zeroed.
   --
   --  Pre: Status must be an error (not Success)
   --  Post: Result has error status and zero return length
   function Error_Result (Status : Exec_Status) return Exec_Result is
      (Status      => Status,
       Return_Len  => 0,
       Return_Data => (others => 0))
   with
      Pre  => Is_Error (Status),
      Post => Is_Error (Error_Result'Result.Status) and then
              Error_Result'Result.Return_Len = 0;

   ---------------------------------------------------------------------------
   --  Call Context
   ---------------------------------------------------------------------------

   --  Information about the current CVM call
   type Call_Context is record
      --  Caller's address (user or CVM)
      Caller     : Caller_Address;

      --  Target CVM address
      Target     : CVM_Address;

      --  Entry point identifier (method selector)
      Entry_Point : Hash256;

      --  Parameter length (actual length of valid data in Params)
      Param_Len  : Natural;

      --  Parameters (only first Param_Len bytes are valid)
      Params     : Param_Buffer;

      --  Block height (for time-dependent logic)
      Height     : Natural;

      --  Is this an internal call (from another CVM)?
      Internal   : Boolean;
   end record;

   --  Ghost function: Verify call context is well-formed
   function Is_Valid_Context (C : Call_Context) return Boolean is
      (Is_Valid_Address (C.Caller) and then
       Is_Valid_Address (C.Target) and then
       C.Param_Len <= Max_Param_Size and then
       Is_Valid_Param_Buffer (C.Params))
   with Ghost, Pure_Function;

   --  Default empty context
   Empty_Context : constant Call_Context := (
      Caller      => (others => 0),
      Target      => (others => 0),
      Entry_Point => (others => 0),
      Param_Len   => 0,
      Params      => (others => 0),
      Height      => 0,
      Internal    => False
   );

   ---------------------------------------------------------------------------
   --  State Management
   ---------------------------------------------------------------------------

   --  State slot descriptor
   type State_Slot is record
      Index    : State_Index;
      Length   : Natural;  -- Actual value length (0 = empty)
      Value    : State_Value;
      Modified : Boolean;  -- True if modified in current execution
   end record;

   --  Ghost function: Verify state slot is well-formed
   function Is_Valid_Slot (S : State_Slot) return Boolean is
      (S.Length <= Max_State_Size and then
       Is_Valid_State_Value (S.Value))
   with Ghost, Pure_Function;

   --  Ghost function: Verify slot is empty
   function Slot_Is_Empty (S : State_Slot) return Boolean is
      (S.Length = 0)
   with Ghost, Pure_Function;

   --  Empty slot constant
   Empty_Slot : constant State_Slot := (
      Index    => 0,
      Length   => 0,
      Value    => (others => 0),
      Modified => False
   );

   --  State array type (all slots for a CVM)
   type State_Array is array (State_Index) of State_Slot;

   --  Ghost function: Verify state array is well-formed
   function Is_Valid_State_Array (S : State_Array) return Boolean is
      (for all I in State_Index => Is_Valid_Slot (S (I)))
   with Ghost, Pure_Function;

   --  Empty state
   Empty_State : constant State_Array := (others => Empty_Slot);

   ---------------------------------------------------------------------------
   --  Method Selector
   ---------------------------------------------------------------------------

   --  Method selector: SHA3-256(method_name)
   --  Used to identify which entry point to call
   subtype Method_Selector is Hash256;

   ---------------------------------------------------------------------------
   --  CVM Capabilities
   ---------------------------------------------------------------------------

   --  Capabilities define what a CVM can do
   type CVM_Capability is (
      Cap_Read_State,      -- Can read own state
      Cap_Write_State,     -- Can modify own state
      Cap_Call_CVM,        -- Can call other CVMs
      Cap_Shield_State,    -- Can use encrypted state (TEE feature)
      Cap_Eye_View,        -- Can create viewing keys (TEE feature)
      Cap_Gate_Session,    -- Can create private sessions (TEE feature)
      Cap_Whisper_Value,   -- Can use confidential values (TEE feature)
      Cap_Veil_Proof       -- Can generate ZK proofs (TEE feature)
   );

   --  Capability set
   type Capability_Set is array (CVM_Capability) of Boolean;

   --  Ghost function: Check if capability is enabled
   function Has_Cap (Caps : Capability_Set; Cap : CVM_Capability) return Boolean is
      (Caps (Cap))
   with Ghost, Pure_Function;

   --  Default capabilities (read/write state, call other CVMs)
   Default_Capabilities : constant Capability_Set := (
      Cap_Read_State    => True,
      Cap_Write_State   => True,
      Cap_Call_CVM      => True,
      Cap_Shield_State  => False,
      Cap_Eye_View      => False,
      Cap_Gate_Session  => False,
      Cap_Whisper_Value => False,
      Cap_Veil_Proof    => False
   );

   --  Full privacy capabilities (all features enabled)
   Privacy_Capabilities : constant Capability_Set := (others => True);

   ---------------------------------------------------------------------------
   --  CVM Metadata
   ---------------------------------------------------------------------------

   --  Maximum length for CVM name/description
   Max_Name_Length : constant := 64;

   subtype CVM_Name is String (1 .. Max_Name_Length);

   --  CVM registration info
   type CVM_Info is record
      --  CVM address (derived from code hash)
      Addr : CVM_Address;

      --  Code hash (SHA3-256 of SPARK package)
      Code : Code_Hash;

      --  Human-readable name (padded with spaces)
      Name : CVM_Name;

      --  Capability set
      Caps : Capability_Set;

      --  Is CVM active?
      Active : Boolean;
   end record;

   --  Ghost function: Verify CVM info is well-formed
   function Is_Valid_CVM_Info (I : CVM_Info) return Boolean is
      (Is_Valid_Address (I.Addr))
   with Ghost, Pure_Function;

   --  Empty CVM info
   Empty_Info : constant CVM_Info := (
      Addr   => (others => 0),
      Code   => (others => 0),
      Name   => (others => ' '),
      Caps   => Default_Capabilities,
      Active => False
   );

end CVM_Types;
