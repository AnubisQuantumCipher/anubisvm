--  SPHINX RV32 Syscall: System Call Handling for RISC-V Interpreter
--
--  This package handles ECALL instructions by mapping them to the
--  AnubisVM syscall interface (THOTH/ANKH/Environment).
--
--  RISC-V ECALL ABI:
--  - a7 (x17): System call number
--  - a0-a5 (x10-x15): Arguments
--  - a0 (x10): Return value
--  - a1 (x11): Secondary return value (if applicable)
--
--  System Call Numbers (matching sphinx_runtime.ads):
--  - 0x01: SLoad (storage read)
--  - 0x02: SStore (storage write)
--  - 0x10: SHA3 (hash)
--  - 0x11: Keccak256 (hash)
--  - 0x12: MLDSA_Verify (signature verification)
--  - 0x13: MLKEM_Decaps (key decapsulation)
--  - 0x20: Caller (get msg.sender)
--  - 0x21: Address (get self address)
--  - 0x22: CallValue (get msg.value)
--  - 0x23: CallData (get calldata pointer)
--  - 0x24: CallDataSize (get calldata length)
--  - 0x25: BlockNumber (get block number)
--  - 0x26: Timestamp (get block timestamp)
--  - 0x27: GasPrice (get gas price)
--  - 0x28: GasRemaining (get remaining gas)
--  - 0x30: Call (cross-contract call)
--  - 0x31: StaticCall (read-only call)
--  - 0x32: DelegateCall (delegate call)
--  - 0x33: Create (deploy contract)
--  - 0x34: Create2 (deterministic deploy)
--  - 0x40: Return (normal return)
--  - 0x41: Revert (revert execution)
--
--  SPARK Verification Properties:
--  - All syscall numbers are validated
--  - Memory addresses passed as arguments are bounds-checked
--  - Gas is charged appropriately for each syscall
--  - Return values are properly set in registers

pragma SPARK_Mode (On);

with Sphinx_RV32_Types; use Sphinx_RV32_Types;
with Sphinx_RV32_Storage; use Sphinx_RV32_Storage;
with Aegis_VM_Types; use Aegis_VM_Types;

package Sphinx_RV32_Syscall with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Package-Level Storage State
   --
   --  This provides persistent storage for the contract execution session.
   --  Storage is maintained across syscalls within a single execution.
   ---------------------------------------------------------------------------

   Contract_Storage : Storage_State := Empty_Storage;

   ---------------------------------------------------------------------------
   --  Syscall Numbers (must match sphinx_runtime.ads)
   ---------------------------------------------------------------------------

   --  THOTH Storage
   Syscall_SLoad  : constant := 16#01#;
   Syscall_SStore : constant := 16#02#;

   --  ANKH Crypto
   Syscall_SHA3         : constant := 16#10#;
   Syscall_Keccak256    : constant := 16#11#;
   Syscall_MLDSA_Verify : constant := 16#12#;
   Syscall_MLKEM_Decaps : constant := 16#13#;

   --  Environment
   Syscall_Caller        : constant := 16#20#;
   Syscall_Address       : constant := 16#21#;
   Syscall_CallValue     : constant := 16#22#;
   Syscall_CallData      : constant := 16#23#;
   Syscall_CallDataSize  : constant := 16#24#;
   Syscall_BlockNumber   : constant := 16#25#;
   Syscall_Timestamp     : constant := 16#26#;
   Syscall_GasPrice      : constant := 16#27#;
   Syscall_GasRemaining  : constant := 16#28#;

   --  Cross-Contract Calls
   Syscall_Call         : constant := 16#30#;
   Syscall_StaticCall   : constant := 16#31#;
   Syscall_DelegateCall : constant := 16#32#;
   Syscall_Create       : constant := 16#33#;
   Syscall_Create2      : constant := 16#34#;

   --  Return Data
   Syscall_Return         : constant := 16#40#;
   Syscall_Revert         : constant := 16#41#;
   Syscall_ReturnData     : constant := 16#42#;
   Syscall_ReturnDataSize : constant := 16#43#;

   ---------------------------------------------------------------------------
   --  Syscall Result Type
   ---------------------------------------------------------------------------

   type Syscall_Result is record
      Success     : Boolean;
      Return_Val  : Word;       -- Value for a0
      Return_Val2 : Word;       -- Value for a1 (secondary return)
      Gas_Cost    : Gas_Amount;
      Should_Halt : Boolean;    -- True for return/revert
   end record;

   Failed_Syscall : constant Syscall_Result := (
      Success     => False,
      Return_Val  => 0,
      Return_Val2 => 0,
      Gas_Cost    => 0,
      Should_Halt => False
   );

   ---------------------------------------------------------------------------
   --  Syscall Handler
   ---------------------------------------------------------------------------

   --  Handle an ECALL instruction
   --
   --  This function:
   --  1. Reads syscall number from a7 (x17)
   --  2. Reads arguments from a0-a5 (x10-x15)
   --  3. Validates memory addresses
   --  4. Charges appropriate gas
   --  5. Dispatches to the appropriate handler
   --  6. Sets return value in a0/a1
   --  7. Advances PC if successful
   --
   procedure Handle_Syscall (
      State : in Out Interpreter_State
   ) with
      Global => (In_Out => Contract_Storage),
      Pre    => State.CPU.Trap = Trap_Ecall,
      Post   => State.CPU.Trap /= Trap_Ecall or else
                State.CPU.Halted;

   ---------------------------------------------------------------------------
   --  Individual Syscall Handlers
   ---------------------------------------------------------------------------

   --  THOTH: Storage Load (sload)
   --  a0: slot_ptr (pointer to 32-byte slot key in memory)
   --  a1: value_ptr (pointer to 32-byte output buffer)
   --  Returns: 0 on success, 1 on error
   procedure Handle_SLoad (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => (Input => Contract_Storage);

   --  THOTH: Storage Store (sstore)
   --  a0: slot_ptr (pointer to 32-byte slot key in memory)
   --  a1: value_ptr (pointer to 32-byte value in memory)
   --  Returns: 0 on success, 1 on error
   procedure Handle_SStore (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => (In_Out => Contract_Storage);

   --  ANKH: SHA3 Hash
   --  a0: input_ptr (pointer to input data)
   --  a1: input_len (length of input data)
   --  a2: output_ptr (pointer to 32-byte output buffer)
   --  Returns: 0 on success, 1 on error
   procedure Handle_SHA3 (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  ANKH: ML-DSA-87 Signature Verification
   --  a0: msg_ptr (pointer to message)
   --  a1: msg_len (message length)
   --  a2: sig_ptr (pointer to signature)
   --  a3: sig_len (signature length)
   --  a4: pubkey_ptr (pointer to public key)
   --  Returns: 0 if valid, 1 if invalid/error
   procedure Handle_MLDSA_Verify (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Environment: Get Caller
   --  a0: addr_ptr (pointer to 32-byte output buffer)
   --  Returns: 0 on success
   procedure Handle_Caller (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Environment: Get Self Address
   --  a0: addr_ptr (pointer to 32-byte output buffer)
   --  Returns: 0 on success
   procedure Handle_Address (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Environment: Get Calldata Size
   --  Returns: calldata length in a0
   procedure Handle_CallDataSize (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Environment: Copy Calldata
   --  a0: dest_ptr (pointer to destination buffer)
   --  a1: offset (offset in calldata)
   --  a2: length (bytes to copy)
   --  Returns: 0 on success, 1 on error
   procedure Handle_CallData (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Environment: Get Block Number
   --  Returns: block number in a0
   procedure Handle_BlockNumber (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Environment: Get Timestamp
   --  Returns: timestamp in a0
   procedure Handle_Timestamp (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Environment: Get Remaining Gas
   --  Returns: remaining gas in a0
   procedure Handle_GasRemaining (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null;

   --  Return: Normal contract return
   --  a0: data_ptr (pointer to return data)
   --  a1: data_len (length of return data)
   procedure Handle_Return (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null,
      Post   => Result.Should_Halt;

   --  Revert: Revert execution
   --  a0: data_ptr (pointer to revert data)
   --  a1: data_len (length of revert data)
   procedure Handle_Revert (
      State  : in Out Interpreter_State;
      Result : out    Syscall_Result
   ) with
      Global => null,
      Post   => Result.Should_Halt;

   ---------------------------------------------------------------------------
   --  Syscall Gas Calculation
   ---------------------------------------------------------------------------

   --  Get gas cost for a syscall
   function Get_Syscall_Gas (
      Syscall_Num : Word;
      Data_Length : Natural := 0
   ) return Gas_Amount with
      Global => null;

   ---------------------------------------------------------------------------
   --  Syscall Validation
   ---------------------------------------------------------------------------

   --  Check if syscall number is valid
   function Is_Valid_Syscall (Num : Word) return Boolean with
      Global => null;

   --  Check if memory range is valid for syscall
   function Is_Valid_Memory_Range (
      Addr   : Word;
      Length : Word;
      State  : Interpreter_State
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Storage Management
   ---------------------------------------------------------------------------

   --  Reset storage to empty state (call before each contract execution)
   procedure Reset_Storage with
      Global => (Output => Contract_Storage),
      Post   => Contract_Storage = Empty_Storage;

   --  Get current storage state (for testing/debugging)
   function Get_Storage return Storage_State with
      Global => (Input => Contract_Storage);

end Sphinx_RV32_Syscall;
