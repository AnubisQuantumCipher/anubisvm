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
   --  Error Handling
   ---------------------------------------------------------------------------

   --  Error codes
   Not_Initialized     : constant := 1;
   Key_Generation_Fail : constant := 2;
   CVM_Register_Fail   : constant := 3;
   Attestation_Fail    : constant := 4;
   Execution_Fail      : constant := 5;

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
