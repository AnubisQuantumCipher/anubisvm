pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Storage; use Aegis_Storage;

--  AEGIS Contract: Native SPARK Contract Interface
--
--  This package defines the interface for KHEPRI native contracts.
--  Unlike bytecode VMs, KHEPRI contracts are compiled SPARK/Ada code
--  that runs natively with formal verification guarantees.
--
--  Key Features:
--  - Native ELF binary contracts
--  - SPARK-verified entry points
--  - Manifest-based metadata
--  - Certification level tracking
--  - ABI encoding/decoding
--
--  Contract Structure:
--  - Each contract has a manifest with metadata
--  - Entry points are SPARK procedures with contracts
--  - Gas bounds are proven via WCET analysis
--  - State is isolated per SPHINX sandbox
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 3: Contract Specification

package Aegis_Contract with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Function Selector Types
   ---------------------------------------------------------------------------

   --  Function selector: first 4 bytes of SHA3-256(signature)
   type Function_Selector is array (0 .. 3) of Byte;

   --  Null selector for fallback/receive
   Null_Selector : constant Function_Selector := (0, 0, 0, 0);

   --  Maximum function parameters
   Max_Parameters : constant := 16;

   --  Parameter slot (32 bytes, U256-aligned)
   type Parameter_Slot is array (0 .. 31) of Byte;

   --  Function parameters array
   type Parameter_Index is range 0 .. Max_Parameters - 1;
   type Parameters_Array is array (Parameter_Index) of Parameter_Slot;

   --  Function call data
   type Call_Data is record
      Selector    : Function_Selector;
      Parameters  : Parameters_Array;
      Param_Count : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Return Data Types
   ---------------------------------------------------------------------------

   --  Maximum return slots
   Max_Return_Slots : constant := 16;

   type Return_Index is range 0 .. Max_Return_Slots - 1;
   type Return_Array is array (Return_Index) of Parameter_Slot;

   --  Function return data
   type Return_Data is record
      Values       : Return_Array;
      Value_Count  : Natural;
      Success      : Boolean;
      Revert_Reason : Hash256;  -- Hash of revert message if any
   end record;

   --  Empty return data
   Empty_Return : constant Return_Data := (
      Values       => (others => (others => 0)),
      Value_Count  => 0,
      Success      => True,
      Revert_Reason => Hash256_Zero
   );

   ---------------------------------------------------------------------------
   --  Contract Function Types
   ---------------------------------------------------------------------------

   --  Function mutability (solidity-style)
   type Function_Mutability is (
      Mut_Pure,        -- No state access
      Mut_View,        -- Read-only state access
      Mut_Nonpayable,  -- State modification, no value
      Mut_Payable      -- State modification with value
   );

   --  Function visibility
   type Function_Visibility is (
      Vis_Public,      -- Callable externally
      Vis_External,    -- Only callable externally
      Vis_Internal,    -- Only within contract/inheritance
      Vis_Private      -- Only within contract
   );

   --  Function signature entry
   type Function_Entry is record
      Selector    : Function_Selector;
      Name_Hash   : Hash256;
      Param_Count : Natural;
      Return_Count : Natural;
      Mutability  : Function_Mutability;
      Visibility  : Function_Visibility;
      Gas_Bound   : Gas_Amount;  -- Proven WCET bound
   end record;

   --  Maximum functions per contract
   Max_Functions : constant := 256;

   type Function_Index is range 0 .. Max_Functions - 1;
   type Function_Table is array (Function_Index) of Function_Entry;

   ---------------------------------------------------------------------------
   --  Event Types
   ---------------------------------------------------------------------------

   --  Event topic
   type Event_Topic is new Hash256;

   --  Maximum topics per event
   Max_Topics : constant := 4;

   type Topic_Index is range 0 .. Max_Topics - 1;
   type Topic_Array is array (Topic_Index) of Event_Topic;

   --  Indexed flags for event topics
   type Indexed_Flags is array (Topic_Index) of Boolean;

   --  Event definition
   type Event_Entry is record
      Signature   : Hash256;   -- SHA3(name(param_types))
      Topic_Count : Natural;
      Indexed     : Indexed_Flags;
   end record;

   --  Maximum events per contract
   Max_Events : constant := 64;

   type Event_Index is range 0 .. Max_Events - 1;
   type Event_Table is array (Event_Index) of Event_Entry;

   ---------------------------------------------------------------------------
   --  Contract Manifest
   ---------------------------------------------------------------------------

   --  Contract name (fixed size for proof)
   Max_Name_Length : constant := 64;
   subtype Contract_Name is String (1 .. Max_Name_Length);

   --  Contract manifest (on-chain metadata)
   type Contract_Manifest is record
      --  Identity
      Address       : Contract_Address;
      Code_Hash     : Hash256;
      Name          : Contract_Name;
      Name_Length   : Natural;

      --  Verification
      Proof_Hash    : Hash256;       -- Hash of SPARK proof artifacts
      Certification : Certification_Level;
      WCET_Bound    : Gas_Amount;    -- Maximum proven gas for any function

      --  Version
      Major_Version : Natural;
      Minor_Version : Natural;
      Patch_Version : Natural;

      --  Interface
      Functions     : Function_Table;
      Function_Count : Natural;
      Events        : Event_Table;
      Event_Count   : Natural;

      --  Deployment
      Deployer      : Contract_Address;
      Deploy_Block  : U256;
      Deploy_Time   : U256;
   end record;

   ---------------------------------------------------------------------------
   --  Contract State Types
   ---------------------------------------------------------------------------

   --  Contract runtime state
   type Contract_State is record
      Manifest      : Contract_Manifest;
      Account       : Account_State;
      Is_Deployed   : Boolean;
      Is_Destroyed  : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  ABI Encoding/Decoding
   ---------------------------------------------------------------------------

   --  Encode U256 to parameter slot (big-endian)
   function Encode_U256 (Value : U256) return Parameter_Slot with
      Global => null;

   --  Decode parameter slot to U256 (big-endian)
   function Decode_U256 (Slot : Parameter_Slot) return U256 with
      Global => null;

   --  Encode address to parameter slot (right-padded)
   function Encode_Address (Addr : Contract_Address) return Parameter_Slot with
      Global => null;

   --  Decode parameter slot to address
   function Decode_Address (Slot : Parameter_Slot) return Contract_Address with
      Global => null;

   --  Encode boolean to parameter slot
   function Encode_Bool (Value : Boolean) return Parameter_Slot with
      Global => null;

   --  Decode parameter slot to boolean
   function Decode_Bool (Slot : Parameter_Slot) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Function Selector Computation
   ---------------------------------------------------------------------------

   --  Compute selector from function signature
   --  Selector = SHA3-256(signature)[0:4]
   function Compute_Selector (Signature_Hash : Hash256) return Function_Selector with
      Global => null;

   --  Match selector in function table
   function Find_Function (
      Table    : Function_Table;
      Count    : Natural;
      Selector : Function_Selector
   ) return Natural with
      Global => null,
      Pre    => Count <= Max_Functions,
      Post   => Find_Function'Result <= Count;

   ---------------------------------------------------------------------------
   --  Contract Validation
   ---------------------------------------------------------------------------

   --  Validate manifest consistency
   function Validate_Manifest (M : Contract_Manifest) return Boolean with
      Global => null;

   --  Check if function is callable with given context
   function Can_Call_Function (
      Func      : Function_Entry;
      Is_Static : Boolean;
      Has_Value : Boolean
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Contract Entry Point Type
   ---------------------------------------------------------------------------

   --  Entry point procedure signature for native contracts
   --  All contract functions conform to this interface
   type Entry_Point_Result is record
      Success     : Boolean;
      Gas_Used    : Gas_Amount;
      Return_Data : Aegis_Contract.Return_Data;
   end record;

end Aegis_Contract;
