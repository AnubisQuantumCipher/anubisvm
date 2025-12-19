pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface;
with CVM_Registry; use CVM_Registry;
with CVM_Dispatch; use CVM_Dispatch;
with TEE_Keys; use TEE_Keys;
with TEE_Attestation; use TEE_Attestation;

--  TEE_Runtime: Main Trusted Execution Environment Runtime
--
--  This package is the main entry point for the AnubisVM TEE.
--  It integrates all components:
--  - CVM Registry and Dispatch
--  - TEE Key Management
--  - Remote Attestation
--  - State Management (Khepri MPT)
--  - Privacy Layer (Shield, Eye, Gate, Whisper, Veil)
--
--  The TEE provides:
--  - Post-quantum cryptographic security (ML-DSA-87, ML-KEM-1024, SHA3)
--  - Mathematically proven code execution (SPARK verification)
--  - Hardware-grade isolation (planned: Apple Secure Enclave integration)
--  - Remote attestation for trust verification

package TEE_Runtime with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  TEE State
   ---------------------------------------------------------------------------

   --  TEE execution status
   type TEE_Status is (
      Uninitialized,    -- TEE not yet initialized
      Initializing,     -- Initialization in progress
      Running,          -- TEE is operational
      Attesting,        -- Attestation in progress
      Error,            -- Error state
      Shutdown          -- TEE is shutting down
   );

   --  TEE runtime state
   type TEE_State is record
      --  Current status
      Status : TEE_Status;

      --  Key bundle
      Keys : TEE_Key_Bundle;

      --  CVM registry
      Registry : Registry_Array;

      --  Dispatch state
      Dispatch : Dispatch_State;

      --  TEE measurements
      Code_Hash   : Measurement;  -- Hash of TEE binary
      Config_Hash : Measurement;  -- Hash of configuration

      --  State root from Khepri MPT (updated after block commits)
      State_Root : Measurement;

      --  Current timestamp (Unix seconds, updated by block processing)
      Timestamp : Word64;

      --  Block height (for time-based logic)
      Height : Natural;

      --  Error message (if Status = Error)
      Error_Code : Natural;
   end record;

   --  Initial state
   Initial_State : constant TEE_State := (
      Status      => Uninitialized,
      Keys        => Empty_Bundle,
      Registry    => (others => Empty_Entry),
      Dispatch    => Initial_Dispatch_State,
      Code_Hash   => (others => 0),
      Config_Hash => (others => 0),
      State_Root  => (others => 0),
      Timestamp   => 0,
      Height      => 0,
      Error_Code  => 0
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize TEE with entropy
   --
   --  Entropy     : 512 bits of true random entropy
   --  Code_Hash   : SHA3-256 hash of TEE binary
   --  Config_Hash : SHA3-256 hash of configuration
   --  State       : Output TEE state
   --  Success     : True if initialization succeeded
   procedure Initialize (
      Entropy     : Byte_Array;
      Code_Hash   : Measurement;
      Config_Hash : Measurement;
      State       : out TEE_State;
      Success     : out Boolean
   ) with
      Global => null,
      Pre => Entropy'Length = 64,
      Post => (if Success then State.Status = Running);

   --  Shutdown TEE (securely zeroize all keys)
   procedure Shutdown (
      State : in Out TEE_State
   ) with
      Global => null,
      Post => State.Status = Shutdown;

   ---------------------------------------------------------------------------
   --  CVM Operations
   ---------------------------------------------------------------------------

   --  Register a CVM
   --
   --  State   : TEE state
   --  Reg     : CVM registration
   --  Success : True if registration succeeded
   procedure Register_CVM (
      State   : in Out TEE_State;
      Reg     : CVM_Interface.CVM_Registration;
      Success : out Boolean
   ) with
      Global => null,
      Pre => State.Status = Running;

   --  Execute CVM call
   --
   --  State   : TEE state
   --  Context : Call context
   --  Result  : Execution result
   procedure Execute (
      State   : in Out TEE_State;
      Context : Call_Context;
      Result  : out Exec_Result
   ) with
      Global => null,
      Pre => State.Status = Running;

   --  Get CVM count
   function Get_CVM_Count (
      State : TEE_State
   ) return Natural with
      Global => null;

   ---------------------------------------------------------------------------
   --  Attestation
   ---------------------------------------------------------------------------

   --  Generate attestation quote
   --
   --  State : TEE state
   --  Nonce : Verifier-provided nonce (32 bytes)
   --  Quote : Output attestation quote
   --  Success : True if quote generation succeeded
   procedure Generate_Attestation (
      State   : TEE_State;
      Nonce   : Byte_Array;
      Quote   : out Attestation_Quote;
      Success : out Boolean
   ) with
      Global => null,
      Pre => State.Status = Running and then Nonce'Length = 32;

   --  Get attestation public key
   function Get_Attestation_PK (
      State : TEE_State
   ) return DSA_Public_Key with
      Global => null,
      Pre => State.Status = Running;

   ---------------------------------------------------------------------------
   --  Key Exchange
   ---------------------------------------------------------------------------

   --  Get KEM encapsulation key (for establishing secure channel)
   function Get_KEM_Key (
      State : TEE_State
   ) return KEM_Encaps_Key with
      Global => null,
      Pre => State.Status = Running;

   --  Establish session key with remote party
   --
   --  State      : TEE state
   --  Ciphertext : KEM ciphertext from remote
   --  Session_ID : Output session identifier
   --  Key        : Output session key
   --  Success    : True if key exchange succeeded
   procedure Establish_Session (
      State      : TEE_State;
      Ciphertext : KEM_Ciphertext;
      Session_ID : out Byte_Array;
      Key        : out Session_Key;
      Success    : out Boolean
   ) with
      Global => null,
      Pre => State.Status = Running and then Session_ID'Length = 32;

   ---------------------------------------------------------------------------
   --  State Management
   ---------------------------------------------------------------------------

   --  Get current state root hash
   function Get_State_Root (
      State : TEE_State
   ) return Measurement with
      Global => null,
      Pre => State.Status = Running;

   --  Update state root (called after Khepri MPT commits)
   --  This integrates with Khepri_State_Trie.State_Root externally
   procedure Set_State_Root (
      State     : in Out TEE_State;
      New_Root  : in     Measurement
   ) with
      Global => null,
      Pre => State.Status = Running,
      Post => Get_State_Root (State) = New_Root;

   --  Get current timestamp
   function Get_Timestamp (
      State : TEE_State
   ) return Word64 with
      Global => null;

   --  Update timestamp (called by block processing)
   procedure Set_Timestamp (
      State         : in Out TEE_State;
      New_Timestamp : in     Word64
   ) with
      Global => null,
      Post => Get_Timestamp (State) = New_Timestamp;

   --  Advance block height
   procedure Advance_Height (
      State : in Out TEE_State
   ) with
      Global => null,
      Pre => State.Status = Running,
      Post => State.Height = State.Height'Old + 1;

   --  Get current height
   function Get_Height (
      State : TEE_State
   ) return Natural is (State.Height) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Memory Isolation Verification
   ---------------------------------------------------------------------------

   --  Memory region type
   type Memory_Region is record
      Base_Address : Word64;     --  Base address of region
      Size         : Word64;     --  Size in bytes
      Read_Only    : Boolean;    --  Region is read-only
      Executable   : Boolean;    --  Region is executable
      Isolated     : Boolean;    --  Region is isolated from host
      Valid        : Boolean;    --  Region is valid
   end record;

   --  Zero memory region
   Zero_Region : constant Memory_Region := (
      Base_Address => 0,
      Size         => 0,
      Read_Only    => False,
      Executable   => False,
      Isolated     => False,
      Valid        => False
   );

   --  Maximum memory regions
   Max_Memory_Regions : constant := 16;

   --  Memory region array
   type Memory_Region_Array is array (0 .. Max_Memory_Regions - 1) of Memory_Region;

   --  Memory isolation state
   type Memory_Isolation_State is record
      Regions       : Memory_Region_Array;
      Region_Count  : Natural;
      Verified      : Boolean;  --  Isolation verified
   end record;

   --  Initial isolation state
   Initial_Isolation : constant Memory_Isolation_State := (
      Regions      => (others => Zero_Region),
      Region_Count => 0,
      Verified     => False
   );

   --  Verify memory isolation
   --
   --  Checks that all memory regions are properly isolated:
   --  1. No overlapping regions
   --  2. Code regions are read-only and executable
   --  3. Data regions are not executable
   --  4. All regions are marked as isolated from host
   --
   --  Isolation : Memory isolation state to verify
   --
   --  Returns: True if all regions are properly isolated
   function Verify_Memory_Isolation (
      Isolation : Memory_Isolation_State
   ) return Boolean with
      Global => null,
      Pre => Isolation.Region_Count <= Max_Memory_Regions;

   --  Add memory region to isolation state
   --
   --  State   : Memory isolation state
   --  Region  : Memory region to add
   --  Success : True if region added successfully
   procedure Add_Memory_Region (
      Isolation : in out Memory_Isolation_State;
      Region    : Memory_Region;
      Success   : out Boolean
   ) with
      Global => null,
      Pre => Isolation.Region_Count <= Max_Memory_Regions and Region.Valid,
      Post => (if Success then Isolation.Region_Count = Isolation.Region_Count'Old + 1);

   --  Check if two memory regions overlap
   function Regions_Overlap (
      A : Memory_Region;
      B : Memory_Region
   ) return Boolean with
      Global => null,
      Pre => A.Valid and B.Valid;

   ---------------------------------------------------------------------------
   --  Enclave Lifecycle Management
   ---------------------------------------------------------------------------

   --  Enclave lifecycle state
   type Enclave_Lifecycle is (
      Uninitialized,  --  Enclave not created
      Created,        --  Enclave created but not initialized
      Initialized,    --  Enclave initialized and ready
      Attested,       --  Enclave has been attested
      Running,        --  Enclave is running and processing
      Suspended,      --  Enclave is temporarily suspended
      Terminating,    --  Enclave is shutting down
      Destroyed       --  Enclave has been destroyed
   );

   --  Enclave metadata
   type Enclave_Metadata is record
      Lifecycle     : Enclave_Lifecycle;
      Creation_Time : Word64;            --  When enclave was created
      Last_Attest   : Word64;            --  Last attestation time
      Measurement   : TEE_Attestation.Measurement;  --  Current measurement
      Sealed        : Boolean;           --  State is sealed
      Valid         : Boolean;           --  Metadata is valid
   end record;

   --  Initial enclave metadata
   Initial_Enclave : constant Enclave_Metadata := (
      Lifecycle     => Uninitialized,
      Creation_Time => 0,
      Last_Attest   => 0,
      Measurement   => (others => 0),
      Sealed        => False,
      Valid         => False
   );

   --  Verify enclave lifecycle state transition is valid
   --
   --  Checks that state transitions follow valid paths:
   --  Uninitialized -> Created -> Initialized -> Attested -> Running
   --  Running <-> Suspended
   --  Any state -> Terminating -> Destroyed
   --
   --  Current : Current lifecycle state
   --  Next    : Proposed next state
   --
   --  Returns: True if transition is valid
   function Valid_Lifecycle_Transition (
      Current : Enclave_Lifecycle;
      Next    : Enclave_Lifecycle
   ) return Boolean with
      Global => null;

   --  Update enclave lifecycle state
   --
   --  Metadata   : Enclave metadata
   --  New_State  : New lifecycle state
   --  Timestamp  : Current timestamp
   --  Success    : True if state updated successfully
   procedure Update_Enclave_State (
      Metadata   : in out Enclave_Metadata;
      New_State  : Enclave_Lifecycle;
      Timestamp  : Word64;
      Success    : out Boolean
   ) with
      Global => null,
      Pre => Metadata.Valid,
      Post => (if Success then
                 Metadata.Lifecycle = New_State and
                 Metadata.Valid);

   --  Verify enclave measurement matches expected
   --
   --  Metadata  : Enclave metadata
   --  Expected  : Expected measurement
   --
   --  Returns: True if measurement matches
   function Verify_Enclave_Measurement (
      Metadata : Enclave_Metadata;
      Expected : Measurement
   ) return Boolean with
      Global => null,
      Pre => Metadata.Valid;

   --  Seal enclave state
   --
   --  Marks enclave state as sealed (persistent storage)
   --
   --  Metadata : Enclave metadata
   procedure Seal_Enclave_State (
      Metadata : in out Enclave_Metadata
   ) with
      Global => null,
      Pre => Metadata.Valid and Metadata.Lifecycle = Running,
      Post => Metadata.Sealed;

   --  Unseal enclave state
   --
   --  Marks enclave state as unsealed (loaded from storage)
   --
   --  Metadata : Enclave metadata
   procedure Unseal_Enclave_State (
      Metadata : in out Enclave_Metadata
   ) with
      Global => null,
      Pre => Metadata.Valid and Metadata.Sealed,
      Post => not Metadata.Sealed;

   ---------------------------------------------------------------------------
   --  Error Handling
   ---------------------------------------------------------------------------

   --  Error codes
   Not_Initialized      : constant := 1;
   Key_Generation_Fail  : constant := 2;
   CVM_Register_Fail    : constant := 3;
   Attestation_Fail     : constant := 4;
   Execution_Fail       : constant := 5;
   Memory_Isolation_Fail: constant := 6;
   Lifecycle_Error      : constant := 7;

   --  Get error code
   function Get_Error (
      State : TEE_State
   ) return Natural is (State.Error_Code) with
      Global => null;

   --  Clear error
   procedure Clear_Error (
      State : in Out TEE_State
   ) with
      Global => null,
      Post => State.Error_Code = 0;

end TEE_Runtime;
