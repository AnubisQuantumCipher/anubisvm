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
   SPARK_Mode => On,
   Always_Terminates
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
   --  Ghost Functions for Formal Verification
   ---------------------------------------------------------------------------

   --  Ghost: Syscall is state-modifying (not safe in static context)
   function Is_State_Modifying (Syscall : Syscall_Number) return Boolean is
      (Syscall in Sys_SStore | Sys_Call | Sys_DelegateCall |
                 Sys_Create | Sys_Create2 | Sys_SelfDestruct |
                 Sys_Log0 | Sys_Log1 | Sys_Log2 | Sys_Log3 | Sys_Log4 |
                 Sys_Private_Store | Sys_Private_Delete |
                 Sys_Confidential_Transfer)
   with Ghost, Pure_Function;

   --  Ghost: Syscall requires specific capability
   function Requires_Capability (
      Syscall : Syscall_Number;
      Cap     : Capability_Type
   ) return Boolean is
      (case Syscall is
         when Sys_SLoad | Sys_Private_Load => Cap = Cap_Read_Storage,
         when Sys_SStore | Sys_Private_Store | Sys_Private_Delete =>
            Cap = Cap_Write_Storage,
         when Sys_Call | Sys_StaticCall | Sys_DelegateCall | Sys_Private_Call =>
            Cap = Cap_Call,
         when Sys_Create | Sys_Create2 => Cap = Cap_Create,
         when Sys_SelfDestruct => Cap = Cap_Self_Destruct,
         when Sys_Log0 | Sys_Log1 | Sys_Log2 | Sys_Log3 | Sys_Log4 =>
            Cap = Cap_Event,
         when Sys_SHA3 | Sys_Keccak256 | Sys_MLDSA_Verify | Sys_MLKEM_Decaps =>
            Cap = Cap_Crypto,
         when Sys_Commit_Amount | Sys_Verify_Range | Sys_Add_Commitments |
              Sys_Verify_Balance | Sys_Verify_Execution |
              Sys_Create_Session | Sys_Close_Session |
              Sys_Create_Disclosure | Sys_Verify_Disclosure |
              Sys_Derive_View_Key | Sys_Generate_Stealth |
              Sys_Ring_Sign | Sys_Verify_Ring_Sig |
              Sys_Compute_Key_Image | Sys_Check_Spent |
              Sys_ZK_Prove_Range | Sys_ZK_Verify_Range |
              Sys_ZK_Prove_Linear | Sys_ZK_Verify_Linear |
              Sys_Confidential_Transfer | Sys_Create_Transfer_Proof |
              Sys_Verify_Transfer | Sys_Scan_Confidential_Output =>
            Cap = Cap_Privacy,
         when others => False)
   with Ghost, Pure_Function;

   --  Ghost: Call frame validity - gas bounds, depth bounds
   function Frame_Valid (Frame : Call_Frame) return Boolean is
      (Frame.Gas_Used <= Frame.Gas_Limit and then
       Frame.Depth < Max_Call_Depth and then
       Frame.Capabilities (Cap_None) = False)
   with Ghost, Pure_Function;

   --  Ghost: Gas context within sandbox is valid
   function Sandbox_Gas_Valid (Ctx : Sandbox_Context) return Boolean is
      (Ctx.Gas.Gas_Used <= Ctx.Gas.Gas_Limit and then
       Ctx.Gas.Gas_Limit <= Max_Gas_Per_Tx)
   with Ghost, Pure_Function;

   --  Ghost: Sandbox depth within bounds
   function Sandbox_Depth_Valid (Ctx : Sandbox_Context) return Boolean is
      (Ctx.Depth < Max_Call_Depth and then
       Ctx.Snapshot_Count <= Natural (Max_Call_Depth))
   with Ghost, Pure_Function;

   --  Ghost: Sandbox memory within bounds
   function Sandbox_Memory_Valid (Ctx : Sandbox_Context) return Boolean is
      (Ctx.Memory_Size <= Max_Heap_Size / 8)
   with Ghost, Pure_Function;

   --  Ghost: Sandbox status is consistent with frame
   function Sandbox_Status_Consistent (Ctx : Sandbox_Context) return Boolean is
      (Ctx.Status /= Sandbox_Running or else Frame_Valid (Ctx.Current_Frame))
   with Ghost, Pure_Function;

   --  Ghost: Sandbox context is in valid state - all invariants hold
   function Sandbox_Valid (Ctx : Sandbox_Context) return Boolean is
      (Sandbox_Gas_Valid (Ctx) and then
       Sandbox_Depth_Valid (Ctx) and then
       Sandbox_Memory_Valid (Ctx) and then
       Sandbox_Status_Consistent (Ctx))
   with Ghost, Pure_Function;

   --  Ghost: All required capabilities are present
   function Has_Required_Capabilities (
      Frame   : Call_Frame;
      Syscall : Syscall_Number
   ) return Boolean is
      (for all Cap in Capability_Type =>
         (if Requires_Capability (Syscall, Cap) then Frame.Capabilities (Cap)))
   with Ghost, Pure_Function;

   --  Ghost: Static context allows syscall
   function Static_Context_Allows (
      Frame   : Call_Frame;
      Syscall : Syscall_Number
   ) return Boolean is
      (not Frame.Is_Static or else not Is_State_Modifying (Syscall))
   with Ghost, Pure_Function;

   --  Ghost: Syscall can be validated successfully
   function Syscall_Can_Succeed (
      Ctx     : Sandbox_Context;
      Syscall : Syscall_Number
   ) return Boolean is
      (Static_Context_Allows (Ctx.Current_Frame, Syscall) and then
       Has_Required_Capabilities (Ctx.Current_Frame, Syscall) and then
       Ctx.Gas.Gas_Used < Ctx.Gas.Gas_Limit)
   with Ghost, Pure_Function;

   --  Ghost: Memory access is within bounds
   function Memory_Access_In_Bounds (
      Ctx    : Sandbox_Context;
      Offset : Natural;
      Size   : Natural
   ) return Boolean is
      (Offset <= Natural'Last - Size and then
       Offset + Size <= Ctx.Memory_Size)
   with Ghost, Pure_Function;

   --  Ghost: Call stack is consistent
   function Stack_Consistent (Ctx : Sandbox_Context) return Boolean is
      (Ctx.Depth = 0 or else
       (for all I in 0 .. Call_Depth'Min (Ctx.Depth - 1, Max_Call_Depth - 1) =>
          Frame_Valid (Ctx.Frames (Call_Frame_Index (I)))))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Model Functions for State Abstraction
   ---------------------------------------------------------------------------

   --  Model: Total gas consumed
   function Model_Total_Gas_Used (Ctx : Sandbox_Context) return Gas_Amount is
      (Ctx.Gas.Gas_Used)
   with Ghost, Pure_Function;

   --  Model: Effective capability mask
   function Model_Effective_Capabilities (
      Ctx : Sandbox_Context
   ) return Capability_Mask is
      (Ctx.Current_Frame.Capabilities)
   with Ghost, Pure_Function;

   --  Model: Current execution is in static mode
   function Model_Is_Static_Execution (Ctx : Sandbox_Context) return Boolean is
      (Ctx.Current_Frame.Is_Static)
   with Ghost, Pure_Function;

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
      Global => null,
      Post   => Is_Static_Safe'Result = not Is_State_Modifying (Syscall);

   --  Get required capabilities for syscall
   function Required_Capabilities (
      Syscall : Syscall_Number
   ) return Capability_Mask with
      Global => null,
      Post   => (for all Cap in Capability_Type =>
                   Required_Capabilities'Result (Cap) =
                      Requires_Capability (Syscall, Cap));

   ---------------------------------------------------------------------------
   --  Sandbox Validation
   ---------------------------------------------------------------------------

   --  Validate syscall against context
   --
   --  Returns Syscall_OK only when all validation checks pass:
   --  - Static context allows the syscall
   --  - Required capabilities are present
   --  - Sufficient gas remains
   function Validate_Syscall (
      Context : Sandbox_Context;
      Syscall : Syscall_Number
   ) return Syscall_Result with
      Global => null,
      Post   => (Validate_Syscall'Result = Syscall_OK) =
                   Syscall_Can_Succeed (Context, Syscall);

   --  Check if memory access is valid
   function Validate_Memory_Access (
      Context : Sandbox_Context;
      Offset  : Natural;
      Size    : Natural;
      Mode    : Memory_Access
   ) return Boolean with
      Global => null,
      Pre    => Offset <= Natural'Last - Size,
      Post   => (if Validate_Memory_Access'Result then
                    Memory_Access_In_Bounds (Context, Offset, Size));

   ---------------------------------------------------------------------------
   --  Lemma Subprograms for Proof Guidance
   ---------------------------------------------------------------------------

   --  Lemma: Static call denies state-modifying syscalls
   procedure Lemma_Static_Denies_Modifying (
      Frame   : Call_Frame;
      Syscall : Syscall_Number
   ) with
      Ghost,
      Global => null,
      Pre  => Frame.Is_Static and then Is_State_Modifying (Syscall),
      Post => not Static_Context_Allows (Frame, Syscall);

   --  Lemma: Capability check is sound
   procedure Lemma_Capability_Sound (
      Frame   : Call_Frame;
      Syscall : Syscall_Number;
      Cap     : Capability_Type
   ) with
      Ghost,
      Global => null,
      Pre  => Requires_Capability (Syscall, Cap) and then not Frame.Capabilities (Cap),
      Post => not Has_Required_Capabilities (Frame, Syscall);

   --  Lemma: Valid sandbox preserves gas bounds
   procedure Lemma_Valid_Sandbox_Gas_Bounds (
      Ctx : Sandbox_Context
   ) with
      Ghost,
      Global => null,
      Pre  => Sandbox_Valid (Ctx),
      Post => Ctx.Gas.Gas_Used <= Ctx.Gas.Gas_Limit and then
              Ctx.Gas.Gas_Limit <= Max_Gas_Per_Tx;

   --  Lemma: Memory access validity is decidable
   procedure Lemma_Memory_Access_Decidable (
      Ctx    : Sandbox_Context;
      Offset : Natural;
      Size   : Natural
   ) with
      Ghost,
      Global => null,
      Pre  => Offset <= Natural'Last - Size,
      Post => Memory_Access_In_Bounds (Ctx, Offset, Size) or else
              not Memory_Access_In_Bounds (Ctx, Offset, Size);

   --  Lemma: Stack depth preserves frame validity
   procedure Lemma_Stack_Frame_Valid (
      Ctx : Sandbox_Context;
      Idx : Call_Frame_Index
   ) with
      Ghost,
      Global => null,
      Pre  => Stack_Consistent (Ctx) and then Natural (Idx) < Natural (Ctx.Depth),
      Post => Frame_Valid (Ctx.Frames (Idx));

end Aegis_Sandbox;
