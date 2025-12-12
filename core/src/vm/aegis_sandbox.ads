pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Storage; use Aegis_Storage;

--  AEGIS Sandbox: SPHINX Security Isolation
--
--  This package defines types for the SPHINX sandbox that provides
--  security isolation for native SPARK contract execution.
--
--  Key Features:
--  - Memory isolation with capability tokens
--  - Native code execution boundaries
--  - Resource limits enforcement
--  - Syscall interface definition
--  - Inter-contract call security
--
--  Security Model:
--  - Each contract runs in isolated memory space
--  - Capabilities control access to resources
--  - All syscalls are validated before execution
--  - Call depth and gas are strictly limited
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 5: SPHINX Sandbox

package Aegis_Sandbox with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Syscall Types
   ---------------------------------------------------------------------------

   --  Syscall numbers (matching KHEPRI spec)
   type Syscall_Number is (
      --  Storage operations (THOTH)
      Sys_SLoad,           -- 0x01: Load storage slot
      Sys_SStore,          -- 0x02: Store storage slot

      --  Cryptographic operations (ANKH)
      Sys_SHA3,            -- 0x10: SHA3-256 hash
      Sys_Keccak256,       -- 0x11: Keccak-256 hash
      Sys_MLDSA_Verify,    -- 0x12: ML-DSA-87 signature verification
      Sys_MLKEM_Decaps,    -- 0x13: ML-KEM-1024 decapsulation

      --  Environment operations
      Sys_Caller,          -- 0x20: Get caller address
      Sys_Address,         -- 0x21: Get self address
      Sys_CallValue,       -- 0x22: Get call value
      Sys_CallData,        -- 0x23: Get call data
      Sys_CallDataSize,    -- 0x24: Get call data size
      Sys_BlockNumber,     -- 0x25: Get block number
      Sys_Timestamp,       -- 0x26: Get block timestamp
      Sys_GasPrice,        -- 0x27: Get gas price
      Sys_GasRemaining,    -- 0x28: Get remaining gas

      --  Balance operations
      Sys_Balance,         -- 0x30: Get account balance
      Sys_SelfBalance,     -- 0x31: Get self balance

      --  Call operations
      Sys_Call,            -- 0x40: Call another contract
      Sys_StaticCall,      -- 0x41: Read-only call
      Sys_DelegateCall,    -- 0x42: Delegate call
      Sys_Create,          -- 0x43: Create contract
      Sys_Create2,         -- 0x44: Deterministic create

      --  Control operations
      Sys_Return,          -- 0x50: Return data
      Sys_Revert,          -- 0x51: Revert with data
      Sys_Stop,            -- 0x52: Stop execution
      Sys_SelfDestruct,    -- 0x53: Destroy contract

      --  Event operations
      Sys_Log0,            -- 0x60: Log with 0 topics
      Sys_Log1,            -- 0x61: Log with 1 topic
      Sys_Log2,            -- 0x62: Log with 2 topics
      Sys_Log3,            -- 0x63: Log with 3 topics
      Sys_Log4,            -- 0x64: Log with 4 topics

      --  Privacy operations (ANUBIS)
      Sys_Private_Store,        -- 0x80: SHIELD encrypted store
      Sys_Private_Load,         -- 0x81: SHIELD encrypted load
      Sys_Private_Delete,       -- 0x82: SHIELD delete entry
      Sys_Commit_Amount,        -- 0x83: WHISPER Ajtai commitment
      Sys_Verify_Range,         -- 0x84: WHISPER range proof verify
      Sys_Add_Commitments,      -- 0x85: WHISPER homomorphic add
      Sys_Verify_Balance,       -- 0x86: WHISPER balance proof
      Sys_Private_Call,         -- 0x87: GATE private execution
      Sys_Verify_Execution,     -- 0x88: GATE verify proof
      Sys_Create_Session,       -- 0x89: GATE create session
      Sys_Close_Session,        -- 0x8A: GATE close session
      Sys_Create_Disclosure,    -- 0x8B: EYE selective disclosure
      Sys_Verify_Disclosure,    -- 0x8C: EYE verify disclosure
      Sys_Derive_View_Key,      -- 0x8D: EYE derive viewing key
      Sys_Generate_Stealth,     -- 0x8E: EYE stealth address
      Sys_Ring_Sign,            -- 0x8F: VEIL ring signature
      Sys_Verify_Ring_Sig,      -- 0x90: VEIL verify ring sig
      Sys_Compute_Key_Image,    -- 0x91: VEIL compute key image
      Sys_Check_Spent,          -- 0x92: VEIL check double-spend
      Sys_ZK_Prove_Range,       -- 0x93: Lattice ZK range proof
      Sys_ZK_Verify_Range,      -- 0x94: Lattice ZK verify range
      Sys_ZK_Prove_Linear,      -- 0x95: Lattice ZK linear proof
      Sys_ZK_Verify_Linear,     -- 0x96: Lattice ZK verify linear

      --  Confidential transfer operations (WHISPER extended)
      Sys_Confidential_Transfer,    -- 0x97: Confidential value transfer
      Sys_Create_Transfer_Proof,    -- 0x98: Create transfer balance proof
      Sys_Verify_Transfer,          -- 0x99: Verify confidential transfer
      Sys_Scan_Confidential_Output  -- 0x9A: Scan for owned outputs
   );

   --  Syscall result
   type Syscall_Result is (
      Syscall_OK,
      Syscall_Denied,       -- Capability check failed
      Syscall_Out_Of_Gas,   -- Insufficient gas
      Syscall_Error,        -- Execution error
      Syscall_Invalid       -- Invalid syscall
   );

   ---------------------------------------------------------------------------
   --  Sandbox State Types
   ---------------------------------------------------------------------------

   --  Sandbox execution status
   type Sandbox_Status is (
      Sandbox_Running,      -- Actively executing
      Sandbox_Stopped,      -- Normal stop
      Sandbox_Returned,     -- Returned with data
      Sandbox_Reverted,     -- Reverted with data
      Sandbox_Error         -- Fatal error
   );

   --  Call frame information (with default values for SPARK initialization)
   type Call_Frame is record
      Caller       : Contract_Address := Address_Zero;
      Callee       : Contract_Address := Address_Zero;
      Value        : U256             := U256_Zero;
      Gas_Limit    : Gas_Amount       := 0;
      Gas_Used     : Gas_Amount       := 0;
      Call_Data    : Hash256          := Hash256_Zero;
      Return_Data  : Hash256          := Hash256_Zero;
      Depth        : Call_Depth       := 0;
      Call_Kind    : Call_Type        := Call;
      Capabilities : Capability_Mask  := Full_Capabilities;
      Is_Static    : Boolean          := False;
   end record;

   --  Call stack index (0 to Max_Call_Depth-1)
   subtype Call_Frame_Index is Call_Depth range 0 .. Max_Call_Depth - 1;

   --  Call stack
   type Call_Stack is array (Call_Frame_Index) of Call_Frame;

   --  Sandbox context
   type Sandbox_Context is record
      --  Current execution state
      Status       : Sandbox_Status;
      Current_Frame : Call_Frame;

      --  Call stack
      Frames       : Call_Stack;
      Depth        : Call_Depth;

      --  Memory state
      Memory_Size  : Natural;       -- Current memory in words

      --  Gas tracking
      Gas          : Gas_Context;

      --  Storage access set
      Access_Set   : Aegis_Storage.Access_Set;

      --  Snapshots for rollback
      Snapshots    : Snapshot_Stack;
      Snapshot_Count : Natural;

      --  Transaction effects
      Effects      : Transaction_Effects;
   end record;

   ---------------------------------------------------------------------------
   --  Environment Data Types
   ---------------------------------------------------------------------------

   --  Block information
   type Block_Info is record
      Number       : U256;
      Timestamp    : U256;
      Gas_Limit    : Gas_Amount;
      Coinbase     : Contract_Address;
      Difficulty   : U256;
      Base_Fee     : U256;
      Chain_ID     : U256;
   end record;

   --  Transaction information
   type Transaction_Info is record
      Origin       : Contract_Address;  -- Original sender
      Gas_Price    : U256;
      Value        : U256;
      Data_Hash    : Hash256;
      Data_Size    : Natural;
   end record;

   --  Complete execution environment
   type Execution_Environment is record
      Block        : Block_Info;
      Transaction  : Transaction_Info;
      Certification : Certification_Level;
   end record;

   ---------------------------------------------------------------------------
   --  Memory Management Types
   ---------------------------------------------------------------------------

   --  Memory operation
   type Memory_Op is (Mem_Read, Mem_Write, Mem_Copy);

   --  Memory access record
   type Memory_Access_Record is record
      Operation   : Memory_Op;
      Offset      : Natural;
      Size        : Natural;
      Success     : Boolean;
   end record;

   --  Memory expansion result
   type Memory_Expansion_Result is record
      New_Size    : Natural;
      Gas_Cost    : Gas_Amount;
      Success     : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Return/Revert Types
   ---------------------------------------------------------------------------

   --  Maximum return data size
   Max_Return_Data_Size : constant := 24 * 1024;  -- 24 KB

   type Return_Data_Index is range 0 .. Max_Return_Data_Size - 1;
   type Return_Data_Buffer is array (Return_Data_Index) of Byte;

   --  Return data descriptor
   type Return_Data is record
      Buffer : Return_Data_Buffer;
      Size   : Natural;
      Valid  : Boolean;
   end record;

   Empty_Return_Data : constant Return_Data := (
      Buffer => (others => 0),
      Size   => 0,
      Valid  => False
   );

   ---------------------------------------------------------------------------
   --  Event/Log Types
   ---------------------------------------------------------------------------

   --  Maximum log data size
   Max_Log_Data_Size : constant := 4 * 1024;  -- 4 KB

   type Log_Data_Index is range 0 .. Max_Log_Data_Size - 1;
   type Log_Data_Buffer is array (Log_Data_Index) of Byte;

   --  Log topics
   type Topic_Index is range 0 .. 3;
   type Log_Topics is array (Topic_Index) of Hash256;

   --  Log entry
   type Log_Entry is record
      Address     : Contract_Address;
      Topics      : Log_Topics;
      Topic_Count : Natural;
      Data        : Log_Data_Buffer;
      Data_Size   : Natural;
   end record;

   --  Maximum logs per transaction
   Max_Logs_Per_Tx : constant := 256;

   type Log_Index is range 0 .. Max_Logs_Per_Tx - 1;
   type Log_List is array (Log_Index) of Log_Entry;

   --  Transaction logs
   type Transaction_Logs is record
      Logs      : Log_List;
      Log_Count : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Capability Checking
   ---------------------------------------------------------------------------

   --  Check if operation is allowed
   function Has_Capability (
      Mask : Capability_Mask;
      Cap  : Capability_Type
   ) return Boolean with
      Global => null,
      Post   => Has_Capability'Result = Mask (Cap);

   --  Check if call is allowed from static context
   function Is_Static_Safe (Syscall : Syscall_Number) return Boolean with
      Global => null;

   --  Get required capabilities for syscall
   function Required_Capabilities (
      Syscall : Syscall_Number
   ) return Capability_Mask with
      Global => null;

   ---------------------------------------------------------------------------
   --  Sandbox Validation
   ---------------------------------------------------------------------------

   --  Validate syscall against context
   function Validate_Syscall (
      Context : Sandbox_Context;
      Syscall : Syscall_Number
   ) return Syscall_Result with
      Global => null;

   --  Check if memory access is valid
   function Validate_Memory_Access (
      Context : Sandbox_Context;
      Offset  : Natural;
      Size    : Natural;
      Mode    : Memory_Access
   ) return Boolean with
      Global => null,
      Pre    => Offset <= Natural'Last - Size;

end Aegis_Sandbox;
