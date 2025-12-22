pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with System;

--  SPHINX Subprocess Sandbox: Secure Contract Execution via Process Isolation
--
--  This package implements a subprocess-based sandbox for executing smart
--  contracts with strong OS-level isolation. Key security properties:
--
--  1. PROCESS ISOLATION
--     - Each contract runs in a separate child process (fork)
--     - Memory space is fully isolated (copy-on-write)
--     - Parent cannot be corrupted by malicious child code
--     - Child crash does not affect parent VM
--
--  2. RESOURCE LIMITS (setrlimit)
--     - CPU time limit (RLIMIT_CPU) enforced by kernel
--     - Memory limit (RLIMIT_AS) prevents heap exhaustion
--     - Stack size limit (RLIMIT_STACK) prevents stack overflow
--     - File descriptor limit (RLIMIT_NOFILE) prevents resource exhaustion
--     - Core dump disabled (RLIMIT_CORE) prevents secrets leaking
--
--  3. PLATFORM SANDBOX
--     - macOS: Seatbelt sandbox (sandbox-exec) with restricted profile
--     - Linux: seccomp-bpf syscall filter (future)
--     - BSD: pledge/capsicum (future)
--
--  4. HARD TIMEOUT
--     - Parent monitors child via waitpid with timeout
--     - SIGKILL sent on timeout (cannot be caught/blocked)
--     - Zombie process cleanup guaranteed
--
--  5. IPC CHANNEL
--     - Pipe-based communication for return data
--     - Parent reads gas usage, return data, exit status
--     - No shared memory between parent and child
--
--  Security Model:
--     SANDBOX_NONE     - No isolation (development only, NOT SECURE)
--     SANDBOX_RLIMIT   - Resource limits only (basic protection)
--     SANDBOX_SEATBELT - macOS Seatbelt + rlimits (recommended for macOS)
--     SANDBOX_SECCOMP  - Linux seccomp-bpf + rlimits (recommended for Linux)
--
--  References:
--  - Apple Sandbox Guide (sandbox-exec, sandbox.sb profiles)
--  - Linux seccomp(2) and prctl(2) man pages
--  - POSIX setrlimit(2) and fork(2)

package Sphinx_Subprocess with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Sandbox Configuration
   ---------------------------------------------------------------------------

   --  Sandbox security levels
   type Sandbox_Level is (
      Sandbox_None,      --  DANGER: No isolation - for testing only
      Sandbox_Rlimit,    --  Resource limits only
      Sandbox_Seatbelt,  --  macOS Seatbelt sandbox (recommended)
      Sandbox_Seccomp    --  Linux seccomp-bpf (future)
   );

   --  Resource limits configuration
   type Resource_Limits is record
      CPU_Seconds    : Natural;   --  Max CPU time (RLIMIT_CPU)
      Memory_Bytes   : Word64;    --  Max virtual memory (RLIMIT_AS)
      Stack_Bytes    : Word64;    --  Max stack size (RLIMIT_STACK)
      Max_FDs        : Natural;   --  Max file descriptors (RLIMIT_NOFILE)
      Core_Allowed   : Boolean;   --  Allow core dumps (RLIMIT_CORE)
   end record;

   --  Default resource limits (conservative)
   Default_Limits : constant Resource_Limits := (
      CPU_Seconds   => 30,          --  30 seconds max CPU
      Memory_Bytes  => 64 * 1024 * 1024,  --  64 MB max memory
      Stack_Bytes   => 1 * 1024 * 1024,   --  1 MB stack
      Max_FDs       => 8,           --  Minimal file descriptors
      Core_Allowed  => False        --  No core dumps
   );

   --  Strict limits for production
   Strict_Limits : constant Resource_Limits := (
      CPU_Seconds   => 10,          --  10 seconds max CPU
      Memory_Bytes  => 16 * 1024 * 1024,  --  16 MB max memory
      Stack_Bytes   => 512 * 1024,  --  512 KB stack
      Max_FDs       => 4,           --  Minimal file descriptors
      Core_Allowed  => False        --  No core dumps
   );

   ---------------------------------------------------------------------------
   --  Subprocess Execution Result
   ---------------------------------------------------------------------------

   --  Exit reasons for subprocess
   type Exit_Reason is (
      Exit_Success,       --  Exited normally with code 0
      Exit_Failure,       --  Exited normally with non-zero code
      Exit_Timeout,       --  Killed by timeout (SIGKILL from parent)
      Exit_CPU_Limit,     --  Killed by CPU limit (SIGXCPU from kernel)
      Exit_Memory_Limit,  --  Killed by memory limit (segfault)
      Exit_Crashed,       --  Crashed (SIGSEGV, SIGBUS, SIGFPE, etc.)
      Exit_Sandbox_Deny,  --  Sandbox denied operation (SIGKILL from sandbox)
      Exit_IPC_Error,     --  Failed to communicate with child
      Exit_Fork_Failed,   --  Failed to fork subprocess
      Exit_Unknown        --  Unknown exit reason
   );

   --  Result from subprocess execution
   type Subprocess_Result is record
      Reason        : Exit_Reason;   --  Why the subprocess exited
      Exit_Code     : Integer;       --  Exit code if normal exit
      Signal_Number : Integer;       --  Signal number if killed
      Gas_Used      : Gas_Amount;    --  Gas consumed
      Return_Data   : Hash256;       --  SHA3 hash of return data
      Return_Length : Natural;       --  Actual return data length
      Success       : Boolean;       --  True if execution succeeded
      CPU_Time_Ms   : Natural;       --  Actual CPU time used (ms)
      Wall_Time_Ms  : Natural;       --  Actual wall time (ms)
   end record;

   --  Failed result constructor
   Failed_Result : constant Subprocess_Result := (
      Reason        => Exit_Fork_Failed,
      Exit_Code     => -1,
      Signal_Number => 0,
      Gas_Used      => 0,
      Return_Data   => (others => 0),
      Return_Length => 0,
      Success       => False,
      CPU_Time_Ms   => 0,
      Wall_Time_Ms  => 0
   );

   ---------------------------------------------------------------------------
   --  Subprocess Execution
   ---------------------------------------------------------------------------

   --  Execute contract in sandboxed subprocess
   --
   --  This is the main entry point for secure contract execution:
   --  1. Fork child process
   --  2. Child: Apply resource limits and sandbox
   --  3. Child: Execute contract entry point
   --  4. Child: Write result to IPC pipe
   --  5. Parent: Wait for child with timeout
   --  6. Parent: Kill child if timeout exceeded
   --  7. Parent: Read result from IPC pipe
   --  8. Parent: Return result to caller
   --
   --  Security guarantees:
   --  - Parent process is never at risk from child
   --  - Child cannot exceed resource limits
   --  - Child cannot perform unauthorized syscalls (with sandbox)
   --  - Timeout is enforced externally by parent
   --
   function Execute_Sandboxed (
      Entry_Point   : System.Address;       --  Contract entry address
      Calldata      : access constant Byte_Array;  --  Input data
      Gas_Limit     : Gas_Amount;           --  Gas limit
      Timeout_Ms    : Natural;              --  Timeout in milliseconds
      Sandbox       : Sandbox_Level;        --  Sandbox level
      Limits        : Resource_Limits       --  Resource limits
   ) return Subprocess_Result with
      Global => null;

   --  Execute with default settings (strict sandbox)
   function Execute_Sandboxed_Default (
      Entry_Point : System.Address;
      Calldata    : access constant Byte_Array;
      Gas_Limit   : Gas_Amount
   ) return Subprocess_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Sandbox Profiles
   ---------------------------------------------------------------------------

   --  Maximum Seatbelt profile size
   Max_Profile_Size : constant := 4096;

   subtype Profile_Index is Natural range 0 .. Max_Profile_Size - 1;
   type Seatbelt_Profile is array (Profile_Index) of Character;

   --  Get Seatbelt profile for contract execution
   --  This returns a highly restrictive sandbox profile that:
   --  - Denies all network access
   --  - Denies all file read/write except /dev/null
   --  - Denies process creation (no fork/exec from contract)
   --  - Denies signal sending
   --  - Allows only memory operations and syscalls needed for execution
   function Get_Contract_Seatbelt_Profile return Seatbelt_Profile with
      Global => null;

   ---------------------------------------------------------------------------
   --  Platform Detection
   ---------------------------------------------------------------------------

   --  Check if current platform supports Seatbelt (macOS)
   function Is_Seatbelt_Available return Boolean with
      Global => null;

   --  Check if current platform supports seccomp (Linux)
   function Is_Seccomp_Available return Boolean with
      Global => null;

   --  Get recommended sandbox level for current platform
   function Recommended_Sandbox_Level return Sandbox_Level with
      Global => null;

   ---------------------------------------------------------------------------
   --  IPC Protocol
   ---------------------------------------------------------------------------

   --  Maximum return data size (matches contract ABI)
   Max_Return_Data : constant := 24 * 1024;  --  24 KB

   --  IPC message header (sent from child to parent)
   type IPC_Header is record
      Magic        : Word32;      --  0x50515047 "PQPG" for validation
      Version      : Word32;      --  Protocol version
      Exit_Code    : Integer;     --  Contract exit code
      Gas_Used     : Word64;      --  Gas consumed
      Return_Len   : Word32;      --  Return data length
      Checksum     : Word32;      --  CRC32 of return data
   end record with
      Convention => C;

   IPC_Magic   : constant Word32 := 16#5051_5047#;  --  "PQPG"
   IPC_Version : constant Word32 := 1;

   ---------------------------------------------------------------------------
   --  Syscall IPC Protocol
   ---------------------------------------------------------------------------
   --
   --  For out-of-process (sandboxed) execution, syscalls must be proxied
   --  from the child process to the parent. This is done via bidirectional
   --  pipes:
   --
   --    Child Process                Parent Process
   --    =============                ==============
   --    Contract calls VM_SLoad  -->  Receives syscall request
   --    Blocks waiting...             Executes SLOAD in storage
   --    Receives response         <--  Sends response data
   --    Returns to contract           Continues monitoring
   --
   --  Syscall Request (Child -> Parent):
   --    Header: Magic, syscall number, data length
   --    Data:   Syscall-specific arguments (e.g., storage key for SLOAD)
   --
   --  Syscall Response (Parent -> Child):
   --    Header: Magic, status, data length
   --    Data:   Syscall result (e.g., storage value for SLOAD)

   --  Syscall numbers (must match aegis_syscall.ads)
   Syscall_SLoad       : constant Word32 := 16#01#;  --  Storage load
   Syscall_SStore      : constant Word32 := 16#02#;  --  Storage store
   Syscall_SHA3        : constant Word32 := 16#10#;  --  SHA3-256 hash
   Syscall_ML_DSA_Sign : constant Word32 := 16#20#;  --  ML-DSA-87 sign
   Syscall_ML_DSA_Vrfy : constant Word32 := 16#21#;  --  ML-DSA-87 verify
   Syscall_ML_KEM_Enc  : constant Word32 := 16#30#;  --  ML-KEM-1024 encapsulate
   Syscall_ML_KEM_Dec  : constant Word32 := 16#31#;  --  ML-KEM-1024 decapsulate
   Syscall_Log         : constant Word32 := 16#40#;  --  Emit event log
   Syscall_Return      : constant Word32 := 16#51#;  --  Successful return with data
   Syscall_Revert      : constant Word32 := 16#FF#;  --  Revert execution

   --  Maximum syscall data size
   Max_Syscall_Data : constant := 8 * 1024;  --  8 KB max per syscall

   --  Syscall request header (Child -> Parent)
   type Syscall_Request_Header is record
      Magic        : Word32;      --  0x53595343 "SYSC" for validation
      Syscall_Num  : Word32;      --  Syscall number
      Data_Length  : Word32;      --  Length of following data
      Gas_Budget   : Word64;      --  Gas available for this syscall
   end record with
      Convention => C;

   Syscall_Request_Magic : constant Word32 := 16#5359_5343#;  --  "SYSC"

   --  Syscall response status
   type Syscall_Status is (
      Syscall_OK,              --  Success
      Syscall_Error,           --  General error
      Syscall_Out_Of_Gas,      --  Not enough gas
      Syscall_Invalid_Args,    --  Bad arguments
      Syscall_Not_Found,       --  Key not found (SLOAD)
      Syscall_Denied           --  Operation denied by sandbox
   );
   for Syscall_Status use (
      Syscall_OK           => 0,
      Syscall_Error        => 1,
      Syscall_Out_Of_Gas   => 2,
      Syscall_Invalid_Args => 3,
      Syscall_Not_Found    => 4,
      Syscall_Denied       => 5
   );

   --  Syscall response header (Parent -> Child)
   type Syscall_Response_Header is record
      Magic        : Word32;      --  0x52455350 "RESP" for validation
      Status       : Word32;      --  Syscall_Status as Word32
      Data_Length  : Word32;      --  Length of following data
      Gas_Used     : Word64;      --  Gas consumed by this syscall
   end record with
      Convention => C;

   Syscall_Response_Magic : constant Word32 := 16#5245_5350#;  --  "RESP"

   ---------------------------------------------------------------------------
   --  Syscall-Enabled Subprocess Execution
   ---------------------------------------------------------------------------

   --  Execute contract with syscall proxying support
   --
   --  This is the recommended function for production use. It:
   --  1. Creates bidirectional pipes for syscall IPC
   --  2. Forks child with sandbox and resource limits
   --  3. Child sets up syscall handlers to use IPC
   --  4. Parent handles syscall requests in event loop
   --  5. Returns final result when child exits
   --
   --  NOTE: This function is more complex than Execute_Sandboxed but
   --  supports contracts that use storage, crypto, and logging syscalls.
   --
   function Execute_With_Syscalls (
      Entry_Point   : System.Address;
      Calldata      : access constant Byte_Array;
      Gas_Limit     : Gas_Amount;
      Timeout_Ms    : Natural;
      Sandbox       : Sandbox_Level;
      Limits        : Resource_Limits
   ) return Subprocess_Result with
      Global => null;

private

   ---------------------------------------------------------------------------
   --  Internal Implementation (SPARK_Mode Off in body)
   ---------------------------------------------------------------------------

   --  These functions use system calls and are implemented in the body

end Sphinx_Subprocess;
