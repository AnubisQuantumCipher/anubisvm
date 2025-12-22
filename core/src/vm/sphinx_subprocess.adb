--  SPHINX Subprocess Sandbox Implementation
--
--  Security Architecture:
--  ----------------------
--  Parent Process:
--    1. Create pipe for IPC
--    2. Fork child process
--    3. Close write end of pipe
--    4. Wait for child with timeout (poll-based)
--    5. If timeout: SIGKILL child
--    6. Read result from pipe
--    7. Reap zombie (waitpid)
--
--  Child Process:
--    1. Close read end of pipe
--    2. Drop to minimal file descriptors
--    3. Apply resource limits (setrlimit)
--    4. Enter sandbox (Seatbelt on macOS)
--    5. Execute contract entry point
--    6. Write result to pipe
--    7. Exit
--
--  The key security property: the parent NEVER executes untrusted code.
--  All contract code runs in the isolated child process.

pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;
use type Interfaces.C.int;
use type Interfaces.C.size_t;
use type Interfaces.C.unsigned_long;
use type Interfaces.C.long;
use type Interfaces.C.short;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;

package body Sphinx_Subprocess is

   ---------------------------------------------------------------------------
   --  POSIX System Calls
   ---------------------------------------------------------------------------

   --  Process control
   function C_fork return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "fork";

   function C_waitpid (
      Pid     : Interfaces.C.int;
      Status  : access Interfaces.C.int;
      Options : Interfaces.C.int
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "waitpid";

   function C_kill (
      Pid    : Interfaces.C.int;
      Signal : Interfaces.C.int
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "kill";

   procedure C_exit (Status : Interfaces.C.int)
   with Import => True, Convention => C, External_Name => "_exit";

   --  Pipe operations
   function C_pipe (Fds : System.Address) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "pipe";

   function C_close (Fd : Interfaces.C.int) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "close";

   function C_read (
      Fd    : Interfaces.C.int;
      Buf   : System.Address;
      Count : Interfaces.C.size_t
   ) return Interfaces.C.long
   with Import => True, Convention => C, External_Name => "read";

   function C_write (
      Fd    : Interfaces.C.int;
      Buf   : System.Address;
      Count : Interfaces.C.size_t
   ) return Interfaces.C.long
   with Import => True, Convention => C, External_Name => "write";

   --  Poll for non-blocking wait
   type Pollfd is record
      Fd      : Interfaces.C.int;
      Events  : Interfaces.C.short;
      Revents : Interfaces.C.short;
   end record with Convention => C;

   function C_poll (
      Fds     : System.Address;
      Nfds    : Interfaces.C.unsigned_long;
      Timeout : Interfaces.C.int
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "poll";

   POLLIN  : constant Interfaces.C.short := 1;

   --  Resource limits
   type Rlimit is record
      Rlim_Cur : Interfaces.C.unsigned_long;  --  Soft limit
      Rlim_Max : Interfaces.C.unsigned_long;  --  Hard limit
   end record with Convention => C;

   function C_setrlimit (
      Resource : Interfaces.C.int;
      Rlim     : access constant Rlimit
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "setrlimit";

   --  Resource limit constants (macOS values)
   RLIMIT_CPU    : constant Interfaces.C.int := 0;   --  CPU time
   RLIMIT_FSIZE  : constant Interfaces.C.int := 1;   --  File size
   RLIMIT_DATA   : constant Interfaces.C.int := 2;   --  Data segment
   RLIMIT_STACK  : constant Interfaces.C.int := 3;   --  Stack size
   RLIMIT_CORE   : constant Interfaces.C.int := 4;   --  Core file size
   RLIMIT_AS     : constant Interfaces.C.int := 5;   --  Address space
   RLIMIT_NOFILE : constant Interfaces.C.int := 8;   --  Open files

   --  Wait options
   WNOHANG : constant Interfaces.C.int := 1;

   --  Signals
   SIGKILL : constant Interfaces.C.int := 9;
   SIGTERM : constant Interfaces.C.int := 15;

   --  Timing
   type Timespec is record
      Tv_Sec  : Interfaces.C.long;
      Tv_Nsec : Interfaces.C.long;
   end record with Convention => C;

   CLOCK_MONOTONIC : constant Interfaces.C.int := 1;

   function C_clock_gettime (
      Clock_ID : Interfaces.C.int;
      Ts       : access Timespec
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "clock_gettime";

   ---------------------------------------------------------------------------
   --  macOS Seatbelt Sandbox
   ---------------------------------------------------------------------------

   --  sandbox_init on macOS
   --  Returns 0 on success, -1 on failure
   function C_sandbox_init (
      Profile   : System.Address;  --  Sandbox profile string (NUL-terminated)
      Flags     : Word64;          --  Flags (0 = no flags)
      Error_Buf : access System.Address  --  Error message buffer pointer
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "sandbox_init";

   --  Sandbox profile flags
   SANDBOX_NAMED : constant Word64 := 1;  --  Use named profile

   --  Contract execution sandbox profile (Seatbelt SBPL)
   --  This profile is EXTREMELY restrictive:
   --  - No network access
   --  - No file I/O (except /dev/null for safety)
   --  STRICT SEATBELT PROFILE FOR SMART CONTRACTS
   --
   --  Security policy: DENY-BY-DEFAULT with minimal exceptions
   --  This is the most restrictive profile possible while still allowing
   --  the contract to execute and communicate with the parent via IPC.
   --
   --  DENIED operations:
   --  - process-exec: No launching new processes
   --  - process-fork: No spawning child processes
   --  - network*: No network access (inbound or outbound)
   --  - file-read*/file-write*: No filesystem access (except IPC pipes)
   --  - signal: No sending signals
   --  - ipc-*: No System V IPC
   --  - mach-*: No Mach services (except minimal required)
   --  - system-*: No system modifications
   --
   --  ALLOWED operations:
   --  - Memory operations (mmap, mprotect) for execution
   --  - IPC pipe read/write (inherited file descriptors only)
   --  - Process exit
   --
   Contract_Seatbelt_Profile_Str : constant String :=
      "(version 1)" & ASCII.LF &
      "" & ASCII.LF &
      "; =================================================================" & ASCII.LF &
      "; AnubisVM Smart Contract Sandbox - STRICT DENY-BY-DEFAULT" & ASCII.LF &
      "; =================================================================" & ASCII.LF &
      "" & ASCII.LF &
      "; Start with deny-all baseline" & ASCII.LF &
      "(deny default)" & ASCII.LF &
      "" & ASCII.LF &
      "; ---- EXPLICITLY DENY HIGH-RISK OPERATIONS ----" & ASCII.LF &
      "" & ASCII.LF &
      "; No process creation or execution" & ASCII.LF &
      "(deny process-exec)" & ASCII.LF &
      "(deny process-fork)" & ASCII.LF &
      "" & ASCII.LF &
      "; No network access" & ASCII.LF &
      "(deny network*)" & ASCII.LF &
      "" & ASCII.LF &
      "; No signal sending (prevent signaling parent or other processes)" & ASCII.LF &
      "(deny signal)" & ASCII.LF &
      "" & ASCII.LF &
      "; No IPC except our pipes" & ASCII.LF &
      "(deny ipc-posix*)" & ASCII.LF &
      "(deny ipc-sysv*)" & ASCII.LF &
      "" & ASCII.LF &
      "; No system modifications" & ASCII.LF &
      "(deny system*)" & ASCII.LF &
      "(deny sysctl-write)" & ASCII.LF &
      "" & ASCII.LF &
      "; ---- MINIMAL REQUIRED PERMISSIONS ----" & ASCII.LF &
      "" & ASCII.LF &
      "; Allow reading sysctl for CPU/memory info (needed by runtime)" & ASCII.LF &
      "(allow sysctl-read)" & ASCII.LF &
      "" & ASCII.LF &
      "; Allow reading /dev/urandom for DRBG seeding (if needed)" & ASCII.LF &
      "(allow file-read* (literal ""/dev/urandom""))" & ASCII.LF &
      "(allow file-read* (literal ""/dev/random""))" & ASCII.LF &
      "" & ASCII.LF &
      "; Allow /dev/null for discarding output" & ASCII.LF &
      "(allow file-read* (literal ""/dev/null""))" & ASCII.LF &
      "(allow file-write* (literal ""/dev/null""))" & ASCII.LF &
      "" & ASCII.LF &
      "; Allow pipe operations on inherited FDs (our IPC channel)" & ASCII.LF &
      "; Pipes are anonymous and inherited, not opened by path" & ASCII.LF &
      "(allow file-read* (pipe))" & ASCII.LF &
      "(allow file-write* (pipe))" & ASCII.LF &
      "" & ASCII.LF &
      "; Allow process to exit cleanly" & ASCII.LF &
      "(allow process-exit)" & ASCII.LF &
      "" & ASCII.LF &
      "; ---- END OF PROFILE ----" & ASCII.LF &
      ASCII.NUL;

   ---------------------------------------------------------------------------
   --  Signal Decoding Helpers
   ---------------------------------------------------------------------------

   --  Decode wait status to exit reason
   procedure Decode_Wait_Status (
      Status : Interfaces.C.int;
      Reason : out Exit_Reason;
      Code   : out Integer;
      Signal : out Integer
   ) is
      --  WIFEXITED: (status & 0x7F) == 0
      --  WEXITSTATUS: (status >> 8) & 0xFF
      --  WIFSIGNALED: ((status & 0x7F) + 1) >> 1 > 0
      --  WTERMSIG: status & 0x7F
      Status_U32 : constant Unsigned_32 := Unsigned_32 (Status);
      Low7 : constant Unsigned_32 := Status_U32 and 16#7F#;
   begin
      Code := 0;
      Signal := 0;

      if Low7 = 0 then
         --  Normal exit
         Code := Integer ((Shift_Right (Status_U32, 8)) and 16#FF#);
         if Code = 0 then
            Reason := Exit_Success;
         else
            Reason := Exit_Failure;
         end if;
      else
         --  Killed by signal
         Signal := Integer (Low7);
         case Signal is
            when 9 =>   --  SIGKILL
               Reason := Exit_Timeout;  --  We sent SIGKILL on timeout
            when 24 =>  --  SIGXCPU
               Reason := Exit_CPU_Limit;
            when 11 =>  --  SIGSEGV
               Reason := Exit_Memory_Limit;
            when 10 =>  --  SIGBUS
               Reason := Exit_Crashed;
            when 8 =>   --  SIGFPE
               Reason := Exit_Crashed;
            when 6 =>   --  SIGABRT
               Reason := Exit_Crashed;
            when 4 =>   --  SIGILL
               Reason := Exit_Crashed;
            when others =>
               Reason := Exit_Unknown;
         end case;
      end if;
   end Decode_Wait_Status;

   ---------------------------------------------------------------------------
   --  Apply Resource Limits (Child Process)
   ---------------------------------------------------------------------------

   procedure Apply_Resource_Limits (Limits : Resource_Limits) is
      Res : Interfaces.C.int;
      Lim : aliased Rlimit;
      pragma Unreferenced (Res);
   begin
      Ada.Text_IO.Put_Line ("    [Sandbox] Applying resource limits...");

      --  CPU time limit
      Lim := (Rlim_Cur => Interfaces.C.unsigned_long (Limits.CPU_Seconds),
              Rlim_Max => Interfaces.C.unsigned_long (Limits.CPU_Seconds));
      Res := C_setrlimit (RLIMIT_CPU, Lim'Access);
      Ada.Text_IO.Put_Line ("    [Sandbox]   CPU: " &
         Natural'Image (Limits.CPU_Seconds) & " seconds");

      --  Memory limit (virtual address space)
      Lim := (Rlim_Cur => Interfaces.C.unsigned_long (Limits.Memory_Bytes),
              Rlim_Max => Interfaces.C.unsigned_long (Limits.Memory_Bytes));
      Res := C_setrlimit (RLIMIT_AS, Lim'Access);
      Ada.Text_IO.Put_Line ("    [Sandbox]   Memory: " &
         Word64'Image (Limits.Memory_Bytes / 1024 / 1024) & " MB");

      --  Stack size limit
      Lim := (Rlim_Cur => Interfaces.C.unsigned_long (Limits.Stack_Bytes),
              Rlim_Max => Interfaces.C.unsigned_long (Limits.Stack_Bytes));
      Res := C_setrlimit (RLIMIT_STACK, Lim'Access);
      Ada.Text_IO.Put_Line ("    [Sandbox]   Stack: " &
         Word64'Image (Limits.Stack_Bytes / 1024) & " KB");

      --  File descriptor limit
      Lim := (Rlim_Cur => Interfaces.C.unsigned_long (Limits.Max_FDs),
              Rlim_Max => Interfaces.C.unsigned_long (Limits.Max_FDs));
      Res := C_setrlimit (RLIMIT_NOFILE, Lim'Access);
      Ada.Text_IO.Put_Line ("    [Sandbox]   Max FDs: " &
         Natural'Image (Limits.Max_FDs));

      --  Core dump (disable unless explicitly allowed)
      if not Limits.Core_Allowed then
         Lim := (Rlim_Cur => 0, Rlim_Max => 0);
         Res := C_setrlimit (RLIMIT_CORE, Lim'Access);
         Ada.Text_IO.Put_Line ("    [Sandbox]   Core dumps: disabled");
      end if;

      --  File size limit (prevent file creation)
      Lim := (Rlim_Cur => 0, Rlim_Max => 0);
      Res := C_setrlimit (RLIMIT_FSIZE, Lim'Access);
      Ada.Text_IO.Put_Line ("    [Sandbox]   File creation: disabled");

      Ada.Text_IO.Put_Line ("    [Sandbox] Resource limits applied");
   end Apply_Resource_Limits;

   ---------------------------------------------------------------------------
   --  Apply Seatbelt Sandbox (Child Process, macOS only)
   ---------------------------------------------------------------------------

   function Apply_Seatbelt_Sandbox return Boolean is
      Profile_Bytes : constant String := Contract_Seatbelt_Profile_Str;
      Error_Ptr     : aliased System.Address := System.Null_Address;
      Res           : Interfaces.C.int;
   begin
      Ada.Text_IO.Put_Line ("    [Sandbox] Entering Seatbelt sandbox...");

      Res := C_sandbox_init (
         Profile   => Profile_Bytes (Profile_Bytes'First)'Address,
         Flags     => 0,  --  Use inline profile, not named
         Error_Buf => Error_Ptr'Access
      );

      if Res /= 0 then
         Ada.Text_IO.Put_Line ("    [Sandbox] ERROR: sandbox_init failed!");
         return False;
      end if;

      Ada.Text_IO.Put_Line ("    [Sandbox] Seatbelt sandbox active");
      return True;
   end Apply_Seatbelt_Sandbox;

   ---------------------------------------------------------------------------
   --  Execute Contract in Child
   ---------------------------------------------------------------------------

   procedure Execute_In_Child (
      Entry_Point : System.Address;
      Calldata    : access constant Byte_Array;
      Gas_Limit   : Gas_Amount;
      Write_Fd    : Interfaces.C.int;
      Sandbox     : Sandbox_Level;
      Limits      : Resource_Limits
   ) is
      --  Contract entry function type
      type Entry_Function is access function (
         Calldata_Ptr : System.Address;
         Calldata_Len : Interfaces.C.size_t;
         Return_Ptr   : System.Address;
         Return_Len   : access Interfaces.C.size_t;
         Gas_Limit_In : Unsigned_64;
         Gas_Used_Out : access Unsigned_64
      ) return Interfaces.C.int
      with Convention => C;

      function To_Entry_Fn is new Ada.Unchecked_Conversion (
         System.Address, Entry_Function);

      Entry_Fn      : constant Entry_Function := To_Entry_Fn (Entry_Point);
      Return_Buf    : aliased Byte_Array (0 .. Max_Return_Data - 1) := (others => 0);
      Return_Len    : aliased Interfaces.C.size_t := 0;
      Gas_Used_Val  : aliased Unsigned_64 := 0;
      Exit_Code     : Interfaces.C.int;

      --  IPC result
      Header : aliased IPC_Header;
      Written : Interfaces.C.long;
      Res : Interfaces.C.int;
      pragma Unreferenced (Written, Res);
   begin
      Ada.Text_IO.Put_Line ("  [Child] Starting contract execution...");

      --  Step 1: Apply resource limits
      Apply_Resource_Limits (Limits);

      --  Step 2: Apply sandbox (if requested)
      case Sandbox is
         when Sandbox_None =>
            Ada.Text_IO.Put_Line ("  [Child] WARNING: No sandbox active!");
         when Sandbox_Rlimit =>
            Ada.Text_IO.Put_Line ("  [Child] Resource limits only");
         when Sandbox_Seatbelt =>
            if not Apply_Seatbelt_Sandbox then
               Ada.Text_IO.Put_Line ("  [Child] Seatbelt failed, aborting");
               Header := (
                  Magic       => IPC_Magic,
                  Version     => IPC_Version,
                  Exit_Code   => -100,
                  Gas_Used    => 0,
                  Return_Len  => 0,
                  Checksum    => 0
               );
               Written := C_write (Write_Fd, Header'Address,
                  Interfaces.C.size_t (Header'Size / 8));
               C_exit (Interfaces.C.int (1));
               return;
            end if;
         when Sandbox_Seccomp =>
            Ada.Text_IO.Put_Line ("  [Child] Seccomp not yet implemented");
      end case;

      --  Step 3: Execute contract
      Ada.Text_IO.Put_Line ("  [Child] Calling entry point...");

      Exit_Code := Entry_Fn (
         Calldata_Ptr => (if Calldata'Length > 0 then
                           Calldata (Calldata'First)'Address
                        else System.Null_Address),
         Calldata_Len => Interfaces.C.size_t (Calldata'Length),
         Return_Ptr   => Return_Buf (Return_Buf'First)'Address,
         Return_Len   => Return_Len'Access,
         Gas_Limit_In => Unsigned_64 (Gas_Limit),
         Gas_Used_Out => Gas_Used_Val'Access
      );

      Ada.Text_IO.Put_Line ("  [Child] Contract returned: " &
         Interfaces.C.int'Image (Exit_Code));
      Ada.Text_IO.Put_Line ("  [Child] Gas used: " &
         Unsigned_64'Image (Gas_Used_Val));
      Ada.Text_IO.Put_Line ("  [Child] Return length: " &
         Interfaces.C.size_t'Image (Return_Len));

      --  Step 4: Write result to IPC pipe
      Header := (
         Magic       => IPC_Magic,
         Version     => IPC_Version,
         Exit_Code   => Integer (Exit_Code),
         Gas_Used    => Gas_Used_Val,
         Return_Len  => Word32 (Return_Len),
         Checksum    => 0  --  TODO: Add CRC32
      );

      --  Write header
      Written := C_write (Write_Fd, Header'Address,
         Interfaces.C.size_t (Header'Size / 8));

      --  Write return data if any
      if Return_Len > 0 then
         Written := C_write (Write_Fd, Return_Buf (Return_Buf'First)'Address,
            Return_Len);
      end if;

      --  Close pipe and exit
      Res := C_close (Write_Fd);
      Ada.Text_IO.Put_Line ("  [Child] Exiting normally");
      C_exit (Exit_Code);

   exception
      when others =>
         Ada.Text_IO.Put_Line ("  [Child] EXCEPTION during execution");
         Header := (
            Magic       => IPC_Magic,
            Version     => IPC_Version,
            Exit_Code   => -99,
            Gas_Used    => Unsigned_64 (Gas_Limit),  --  Charge full gas on crash
            Return_Len  => 0,
            Checksum    => 0
         );
         Written := C_write (Write_Fd, Header'Address,
            Interfaces.C.size_t (Header'Size / 8));
         Res := C_close (Write_Fd);
         C_exit (Interfaces.C.int (99));
   end Execute_In_Child;

   ---------------------------------------------------------------------------
   --  Execute Sandboxed (Main Entry Point)
   ---------------------------------------------------------------------------

   function Execute_Sandboxed (
      Entry_Point   : System.Address;
      Calldata      : access constant Byte_Array;
      Gas_Limit     : Gas_Amount;
      Timeout_Ms    : Natural;
      Sandbox       : Sandbox_Level;
      Limits        : Resource_Limits
   ) return Subprocess_Result
   is
      Result : Subprocess_Result := Failed_Result;

      --  Pipe file descriptors: [0] = read, [1] = write
      Pipe_Fds  : aliased array (0 .. 1) of Interfaces.C.int := (others => -1);
      Read_Fd   : Interfaces.C.int;
      Write_Fd  : Interfaces.C.int;

      --  Process tracking
      Child_Pid : Interfaces.C.int;
      Wait_Status : aliased Interfaces.C.int := 0;
      Wait_Result : Interfaces.C.int;

      --  Timing
      Start_Time  : aliased Timespec;
      Current_Time : aliased Timespec;
      Elapsed_Ms  : Natural;
      Clock_Res   : Interfaces.C.int;
      pragma Unreferenced (Clock_Res);

      --  Poll for reading
      Pfd : aliased Pollfd;
      Poll_Res : Interfaces.C.int;

      --  IPC data
      Header : aliased IPC_Header;
      Bytes_Read : Interfaces.C.long;
      Return_Buf : aliased Byte_Array (0 .. Max_Return_Data - 1) := (others => 0);

      Res : Interfaces.C.int;
      pragma Unreferenced (Res);
   begin
      Ada.Text_IO.Put_Line ("  [Subprocess] Starting sandboxed execution...");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Gas limit: " & Gas_Amount'Image (Gas_Limit));
      Ada.Text_IO.Put_Line ("  [Subprocess]   Timeout: " & Natural'Image (Timeout_Ms) & " ms");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Sandbox: " & Sandbox_Level'Image (Sandbox));

      --  Record start time
      Clock_Res := C_clock_gettime (CLOCK_MONOTONIC, Start_Time'Access);

      --  Step 1: Create pipe for IPC
      if C_pipe (Pipe_Fds (0)'Address) /= 0 then
         Ada.Text_IO.Put_Line ("  [Subprocess] ERROR: pipe() failed");
         Result.Reason := Exit_IPC_Error;
         return Result;
      end if;

      Read_Fd := Pipe_Fds (0);
      Write_Fd := Pipe_Fds (1);
      Ada.Text_IO.Put_Line ("  [Subprocess] Pipe created: read=" &
         Interfaces.C.int'Image (Read_Fd) & " write=" &
         Interfaces.C.int'Image (Write_Fd));

      --  Step 2: Fork child process
      Child_Pid := C_fork;

      if Child_Pid < 0 then
         --  Fork failed
         Ada.Text_IO.Put_Line ("  [Subprocess] ERROR: fork() failed");
         Res := C_close (Read_Fd);
         Res := C_close (Write_Fd);
         Result.Reason := Exit_Fork_Failed;
         return Result;

      elsif Child_Pid = 0 then
         --  ========== CHILD PROCESS ==========
         --  Close read end (we only write)
         Res := C_close (Read_Fd);

         --  Execute contract in sandbox
         Execute_In_Child (Entry_Point, Calldata, Gas_Limit, Write_Fd,
            Sandbox, Limits);

         --  Should never reach here (Execute_In_Child calls _exit)
         C_exit (Interfaces.C.int (127));
         return Result;  --  Unreachable
      end if;

      --  ========== PARENT PROCESS ==========
      Ada.Text_IO.Put_Line ("  [Subprocess] Forked child PID: " &
         Interfaces.C.int'Image (Child_Pid));

      --  Close write end (we only read)
      Res := C_close (Write_Fd);

      --  Step 3: Wait for child with timeout
      --  We poll the pipe and waitpid in a loop
      loop
         --  Check elapsed time
         Clock_Res := C_clock_gettime (CLOCK_MONOTONIC, Current_Time'Access);
         Elapsed_Ms := Natural (
            (Current_Time.Tv_Sec - Start_Time.Tv_Sec) * 1000 +
            (Current_Time.Tv_Nsec - Start_Time.Tv_Nsec) / 1_000_000);

         if Elapsed_Ms >= Timeout_Ms then
            --  Timeout! Kill the child
            Ada.Text_IO.Put_Line ("  [Subprocess] TIMEOUT after " &
               Natural'Image (Elapsed_Ms) & " ms");
            Ada.Text_IO.Put_Line ("  [Subprocess] Sending SIGKILL to PID " &
               Interfaces.C.int'Image (Child_Pid));

            Res := C_kill (Child_Pid, SIGKILL);

            --  Wait for child to die
            Wait_Result := C_waitpid (Child_Pid, Wait_Status'Access, 0);

            Result.Reason := Exit_Timeout;
            Result.Gas_Used := Gas_Limit;  --  Charge full gas on timeout
            Result.Wall_Time_Ms := Elapsed_Ms;
            Result.Success := False;

            Res := C_close (Read_Fd);
            return Result;
         end if;

         --  Check if child is done (non-blocking)
         Wait_Result := C_waitpid (Child_Pid, Wait_Status'Access, WNOHANG);

         if Wait_Result > 0 then
            --  Child exited
            Ada.Text_IO.Put_Line ("  [Subprocess] Child exited, status: " &
               Interfaces.C.int'Image (Wait_Status));
            exit;
         elsif Wait_Result < 0 then
            --  Error in waitpid
            Ada.Text_IO.Put_Line ("  [Subprocess] ERROR in waitpid");
            Result.Reason := Exit_Unknown;
            Result.Gas_Used := Gas_Limit;
            Res := C_close (Read_Fd);
            return Result;
         end if;

         --  Child still running, poll for 100ms
         Pfd := (Fd => Read_Fd, Events => POLLIN, Revents => 0);
         Poll_Res := C_poll (Pfd'Address, 1, 100);

         if Poll_Res > 0 and then
            (Unsigned_16 (Pfd.Revents) and Unsigned_16 (POLLIN)) /= 0
         then
            --  Data available on pipe - child may be done
            null;  --  Continue loop to check waitpid
         end if;
      end loop;

      --  Step 4: Decode exit status
      declare
         Exit_Reason : Sphinx_Subprocess.Exit_Reason;
         Exit_Code   : Integer;
         Signal_Num  : Integer;
      begin
         Decode_Wait_Status (Wait_Status, Exit_Reason, Exit_Code, Signal_Num);
         Result.Reason := Exit_Reason;
         Result.Exit_Code := Exit_Code;
         Result.Signal_Number := Signal_Num;

         Ada.Text_IO.Put_Line ("  [Subprocess] Exit reason: " &
            Sphinx_Subprocess.Exit_Reason'Image (Exit_Reason));
      end;

      --  Step 5: Read result from IPC pipe
      Bytes_Read := C_read (Read_Fd, Header'Address,
         Interfaces.C.size_t (Header'Size / 8));

      if Bytes_Read = Interfaces.C.long (Header'Size / 8) then
         --  Validate magic
         if Header.Magic = IPC_Magic and Header.Version = IPC_Version then
            Ada.Text_IO.Put_Line ("  [Subprocess] IPC header valid");
            Ada.Text_IO.Put_Line ("  [Subprocess]   Exit code: " &
               Integer'Image (Header.Exit_Code));
            Ada.Text_IO.Put_Line ("  [Subprocess]   Gas used: " &
               Word64'Image (Header.Gas_Used));
            Ada.Text_IO.Put_Line ("  [Subprocess]   Return len: " &
               Word32'Image (Header.Return_Len));

            Result.Gas_Used := Gas_Amount (Header.Gas_Used);
            Result.Return_Length := Natural (Header.Return_Len);

            --  Read return data if any
            if Header.Return_Len > 0 and
               Natural (Header.Return_Len) <= Max_Return_Data
            then
               Bytes_Read := C_read (Read_Fd, Return_Buf (Return_Buf'First)'Address,
                  Interfaces.C.size_t (Header.Return_Len));

               if Bytes_Read = Interfaces.C.long (Header.Return_Len) then
                  --  Hash return data
                  declare
                     use Anubis_Types;
                     use Anubis_SHA3;
                     Data : Anubis_Types.Byte_Array (0 .. Natural (Header.Return_Len) - 1);
                     Digest : SHA3_256_Digest;
                  begin
                     for I in 0 .. Natural (Header.Return_Len) - 1 loop
                        Data (I) := Anubis_Types.Byte (Return_Buf (I));
                     end loop;
                     SHA3_256 (Data, Digest);
                     for I in 0 .. 31 loop
                        Result.Return_Data (I) := Aegis_VM_Types.Byte (Digest (I));
                     end loop;
                  end;
               end if;
            end if;

            --  Determine success based on exit code
            if Result.Reason = Exit_Success or
               (Result.Reason = Exit_Failure and Header.Exit_Code = 0)
            then
               Result.Success := True;
               Result.Reason := Exit_Success;
            else
               Result.Success := False;
            end if;
         else
            Ada.Text_IO.Put_Line ("  [Subprocess] IPC header invalid!");
            Result.Reason := Exit_IPC_Error;
            Result.Gas_Used := Gas_Limit;
         end if;
      else
         Ada.Text_IO.Put_Line ("  [Subprocess] Failed to read IPC header");
         --  Child may have crashed before writing result
         Result.Gas_Used := Gas_Limit;  --  Charge full gas
      end if;

      --  Calculate timing
      Clock_Res := C_clock_gettime (CLOCK_MONOTONIC, Current_Time'Access);
      Result.Wall_Time_Ms := Natural (
         (Current_Time.Tv_Sec - Start_Time.Tv_Sec) * 1000 +
         (Current_Time.Tv_Nsec - Start_Time.Tv_Nsec) / 1_000_000);

      --  Close read end
      Res := C_close (Read_Fd);

      Ada.Text_IO.Put_Line ("  [Subprocess] Execution complete");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Wall time: " &
         Natural'Image (Result.Wall_Time_Ms) & " ms");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Gas charged: " &
         Gas_Amount'Image (Result.Gas_Used));
      Ada.Text_IO.Put_Line ("  [Subprocess]   Success: " & Boolean'Image (Result.Success));

      return Result;
   end Execute_Sandboxed;

   ---------------------------------------------------------------------------
   --  Execute with Default Settings
   ---------------------------------------------------------------------------

   function Execute_Sandboxed_Default (
      Entry_Point : System.Address;
      Calldata    : access constant Byte_Array;
      Gas_Limit   : Gas_Amount
   ) return Subprocess_Result
   is
      --  Calculate timeout from gas limit
      --  ~1M gas = 1 second, with minimum 5 seconds
      Timeout_Ms : Natural := Natural (Gas_Limit / 1000);
   begin
      if Timeout_Ms < 5000 then
         Timeout_Ms := 5000;
      elsif Timeout_Ms > 300_000 then
         Timeout_Ms := 300_000;  --  Max 5 minutes
      end if;

      return Execute_Sandboxed (
         Entry_Point => Entry_Point,
         Calldata    => Calldata,
         Gas_Limit   => Gas_Limit,
         Timeout_Ms  => Timeout_Ms,
         Sandbox     => Recommended_Sandbox_Level,
         Limits      => Strict_Limits
      );
   end Execute_Sandboxed_Default;

   ---------------------------------------------------------------------------
   --  Platform Detection
   ---------------------------------------------------------------------------

   function Is_Seatbelt_Available return Boolean is
   begin
      --  Check if we're on macOS (Darwin)
      --  This is a compile-time constant effectively
      return True;  --  Assume macOS for now
   end Is_Seatbelt_Available;

   function Is_Seccomp_Available return Boolean is
   begin
      --  Linux only
      return False;  --  Not implemented yet
   end Is_Seccomp_Available;

   function Recommended_Sandbox_Level return Sandbox_Level is
   begin
      if Is_Seatbelt_Available then
         return Sandbox_Seatbelt;
      elsif Is_Seccomp_Available then
         return Sandbox_Seccomp;
      else
         return Sandbox_Rlimit;
      end if;
   end Recommended_Sandbox_Level;

   ---------------------------------------------------------------------------
   --  Get Seatbelt Profile
   ---------------------------------------------------------------------------

   function Get_Contract_Seatbelt_Profile return Seatbelt_Profile is
      Profile : Seatbelt_Profile := (others => ' ');
   begin
      for I in Contract_Seatbelt_Profile_Str'Range loop
         if I - Contract_Seatbelt_Profile_Str'First < Max_Profile_Size then
            Profile (I - Contract_Seatbelt_Profile_Str'First) :=
               Contract_Seatbelt_Profile_Str (I);
         end if;
      end loop;
      return Profile;
   end Get_Contract_Seatbelt_Profile;

   ---------------------------------------------------------------------------
   --  Syscall IPC Global State (for child process)
   ---------------------------------------------------------------------------
   --
   --  These globals are used by the child process to communicate syscalls
   --  to the parent. They are set after fork() in the child.

   Syscall_Request_Fd  : Interfaces.C.int := -1;
   Syscall_Response_Fd : Interfaces.C.int := -1;

   ---------------------------------------------------------------------------
   --  Send Syscall Request (Child -> Parent)
   ---------------------------------------------------------------------------

   procedure Send_Syscall_Request (
      Syscall_Num : Word32;
      Data        : Byte_Array;
      Gas_Budget  : Gas_Amount;
      Response    : out Byte_Array;
      Resp_Len    : out Natural;
      Status      : out Syscall_Status;
      Gas_Used    : out Gas_Amount
   )
   is
      Request_Header : aliased Syscall_Request_Header;
      Response_Header : aliased Syscall_Response_Header;
      Written : Interfaces.C.long;
      Bytes_Read : Interfaces.C.long;
   begin
      --  Initialize outputs
      Resp_Len := 0;
      Status := Syscall_Error;
      Gas_Used := 0;
      Response := (others => 0);

      --  Check if syscall IPC is available
      if Syscall_Request_Fd < 0 or Syscall_Response_Fd < 0 then
         Ada.Text_IO.Put_Line ("  [Syscall] ERROR: IPC not initialized");
         return;
      end if;

      --  Build request header
      Request_Header := (
         Magic       => Syscall_Request_Magic,
         Syscall_Num => Syscall_Num,
         Data_Length => Word32 (Data'Length),
         Gas_Budget  => Word64 (Gas_Budget)
      );

      --  Send request header
      Written := C_write (Syscall_Request_Fd, Request_Header'Address,
         Interfaces.C.size_t (Request_Header'Size / 8));

      if Written /= Interfaces.C.long (Request_Header'Size / 8) then
         Ada.Text_IO.Put_Line ("  [Syscall] ERROR: Failed to write request header");
         return;
      end if;

      --  Send request data
      if Data'Length > 0 then
         Written := C_write (Syscall_Request_Fd, Data (Data'First)'Address,
            Interfaces.C.size_t (Data'Length));
         if Written /= Interfaces.C.long (Data'Length) then
            Ada.Text_IO.Put_Line ("  [Syscall] ERROR: Failed to write request data");
            return;
         end if;
      end if;

      --  Wait for response header
      Bytes_Read := C_read (Syscall_Response_Fd, Response_Header'Address,
         Interfaces.C.size_t (Response_Header'Size / 8));

      if Bytes_Read /= Interfaces.C.long (Response_Header'Size / 8) then
         Ada.Text_IO.Put_Line ("  [Syscall] ERROR: Failed to read response header");
         return;
      end if;

      --  Validate response
      if Response_Header.Magic /= Syscall_Response_Magic then
         Ada.Text_IO.Put_Line ("  [Syscall] ERROR: Invalid response magic");
         return;
      end if;

      --  Extract status and gas
      Status := Syscall_Status'Val (Integer (Response_Header.Status));
      Gas_Used := Gas_Amount (Response_Header.Gas_Used);

      --  Read response data
      if Response_Header.Data_Length > 0 then
         declare
            Data_Len : constant Natural := Natural (Response_Header.Data_Length);
         begin
            if Data_Len <= Response'Length then
               Bytes_Read := C_read (Syscall_Response_Fd, Response (Response'First)'Address,
                  Interfaces.C.size_t (Data_Len));
               if Bytes_Read = Interfaces.C.long (Data_Len) then
                  Resp_Len := Data_Len;
               end if;
            else
               Ada.Text_IO.Put_Line ("  [Syscall] ERROR: Response too large");
            end if;
         end;
      end if;
   end Send_Syscall_Request;

   ---------------------------------------------------------------------------
   --  Execute Contract with Syscall Support in Child
   ---------------------------------------------------------------------------

   procedure Execute_With_Syscalls_Child (
      Entry_Point   : System.Address;
      Calldata      : access constant Byte_Array;
      Gas_Limit     : Gas_Amount;
      Result_Fd     : Interfaces.C.int;
      Request_Fd    : Interfaces.C.int;
      Response_Fd   : Interfaces.C.int;
      Sandbox       : Sandbox_Level;
      Limits        : Resource_Limits
   )
   is
      --  Contract entry function type
      type Entry_Function is access function (
         Calldata_Ptr : System.Address;
         Calldata_Len : Interfaces.C.size_t;
         Return_Ptr   : System.Address;
         Return_Len   : access Interfaces.C.size_t;
         Gas_Limit_In : Unsigned_64;
         Gas_Used_Out : access Unsigned_64
      ) return Interfaces.C.int
      with Convention => C;

      function To_Entry_Fn is new Ada.Unchecked_Conversion (
         System.Address, Entry_Function);

      Entry_Fn      : constant Entry_Function := To_Entry_Fn (Entry_Point);
      Return_Buf    : aliased Byte_Array (0 .. Max_Return_Data - 1) := (others => 0);
      Return_Len    : aliased Interfaces.C.size_t := 0;
      Gas_Used_Val  : aliased Unsigned_64 := 0;
      Exit_Code     : Interfaces.C.int;

      --  IPC result
      Header : aliased IPC_Header;
      Written : Interfaces.C.long;
      Res : Interfaces.C.int;
      pragma Unreferenced (Written, Res);
   begin
      Ada.Text_IO.Put_Line ("  [Child] Starting syscall-enabled execution...");

      --  Set up global syscall IPC file descriptors
      Syscall_Request_Fd := Request_Fd;
      Syscall_Response_Fd := Response_Fd;

      --  Step 1: Apply resource limits
      Apply_Resource_Limits (Limits);

      --  Step 2: Apply sandbox (if requested)
      case Sandbox is
         when Sandbox_None =>
            Ada.Text_IO.Put_Line ("  [Child] WARNING: No sandbox active!");
         when Sandbox_Rlimit =>
            Ada.Text_IO.Put_Line ("  [Child] Resource limits only");
         when Sandbox_Seatbelt =>
            if not Apply_Seatbelt_Sandbox then
               Ada.Text_IO.Put_Line ("  [Child] Seatbelt failed, aborting");
               Header := (
                  Magic       => IPC_Magic,
                  Version     => IPC_Version,
                  Exit_Code   => -100,
                  Gas_Used    => 0,
                  Return_Len  => 0,
                  Checksum    => 0
               );
               Written := C_write (Result_Fd, Header'Address,
                  Interfaces.C.size_t (Header'Size / 8));
               C_exit (Interfaces.C.int (1));
               return;
            end if;
         when Sandbox_Seccomp =>
            Ada.Text_IO.Put_Line ("  [Child] Seccomp not yet implemented");
      end case;

      --  Step 3: Execute contract
      --  NOTE: The contract's syscall handlers should use Send_Syscall_Request
      --  to proxy syscalls to the parent. This requires the contract runtime
      --  to be aware of the subprocess execution mode.
      Ada.Text_IO.Put_Line ("  [Child] Calling entry point (syscall IPC enabled)...");

      Exit_Code := Entry_Fn (
         Calldata_Ptr => (if Calldata'Length > 0 then
                           Calldata (Calldata'First)'Address
                        else System.Null_Address),
         Calldata_Len => Interfaces.C.size_t (Calldata'Length),
         Return_Ptr   => Return_Buf (Return_Buf'First)'Address,
         Return_Len   => Return_Len'Access,
         Gas_Limit_In => Unsigned_64 (Gas_Limit),
         Gas_Used_Out => Gas_Used_Val'Access
      );

      Ada.Text_IO.Put_Line ("  [Child] Contract returned: " &
         Interfaces.C.int'Image (Exit_Code));
      Ada.Text_IO.Put_Line ("  [Child] Gas used: " &
         Unsigned_64'Image (Gas_Used_Val));

      --  Step 4: Write result to IPC pipe
      Header := (
         Magic       => IPC_Magic,
         Version     => IPC_Version,
         Exit_Code   => Integer (Exit_Code),
         Gas_Used    => Gas_Used_Val,
         Return_Len  => Word32 (Return_Len),
         Checksum    => 0
      );

      Written := C_write (Result_Fd, Header'Address,
         Interfaces.C.size_t (Header'Size / 8));

      if Return_Len > 0 then
         Written := C_write (Result_Fd, Return_Buf (Return_Buf'First)'Address,
            Return_Len);
      end if;

      --  Close pipes and exit
      Res := C_close (Result_Fd);
      Res := C_close (Request_Fd);
      Res := C_close (Response_Fd);
      Ada.Text_IO.Put_Line ("  [Child] Exiting normally");
      C_exit (Exit_Code);

   exception
      when others =>
         Ada.Text_IO.Put_Line ("  [Child] EXCEPTION during execution");
         Header := (
            Magic       => IPC_Magic,
            Version     => IPC_Version,
            Exit_Code   => -99,
            Gas_Used    => Unsigned_64 (Gas_Limit),
            Return_Len  => 0,
            Checksum    => 0
         );
         Written := C_write (Result_Fd, Header'Address,
            Interfaces.C.size_t (Header'Size / 8));
         Res := C_close (Result_Fd);
         Res := C_close (Request_Fd);
         Res := C_close (Response_Fd);
         C_exit (Interfaces.C.int (99));
   end Execute_With_Syscalls_Child;

   ---------------------------------------------------------------------------
   --  Handle Syscall Request (Parent Process)
   ---------------------------------------------------------------------------

   procedure Handle_Syscall_Request (
      Request_Fd  : Interfaces.C.int;
      Response_Fd : Interfaces.C.int;
      Total_Gas   : in out Gas_Amount;
      Success     : out Boolean
   )
   is
      Request_Header : aliased Syscall_Request_Header;
      Response_Header : aliased Syscall_Response_Header;
      Request_Data : Byte_Array (0 .. Max_Syscall_Data - 1) := (others => 0);
      Response_Data : Byte_Array (0 .. Max_Syscall_Data - 1) := (others => 0);
      Bytes_Read : Interfaces.C.long;
      Written : Interfaces.C.long;
      Syscall_Gas : Gas_Amount := 0;
      pragma Unreferenced (Written);
   begin
      Success := False;

      --  Read request header
      Bytes_Read := C_read (Request_Fd, Request_Header'Address,
         Interfaces.C.size_t (Request_Header'Size / 8));

      if Bytes_Read /= Interfaces.C.long (Request_Header'Size / 8) then
         Ada.Text_IO.Put_Line ("  [Parent] Failed to read syscall request header");
         return;
      end if;

      --  Validate magic
      if Request_Header.Magic /= Syscall_Request_Magic then
         Ada.Text_IO.Put_Line ("  [Parent] Invalid syscall request magic");
         return;
      end if;

      Ada.Text_IO.Put_Line ("  [Parent] Syscall request: " &
         Word32'Image (Request_Header.Syscall_Num));

      --  Read request data
      if Request_Header.Data_Length > 0 and
         Natural (Request_Header.Data_Length) <= Max_Syscall_Data
      then
         Bytes_Read := C_read (Request_Fd, Request_Data (Request_Data'First)'Address,
            Interfaces.C.size_t (Request_Header.Data_Length));
         if Bytes_Read /= Interfaces.C.long (Request_Header.Data_Length) then
            Ada.Text_IO.Put_Line ("  [Parent] Failed to read syscall request data");
            return;
         end if;
      end if;

      --  Process syscall
      --  NOTE: This is where we would dispatch to the actual syscall handlers
      --  (Aegis_Syscall, storage, crypto, etc.) and get the result.
      --  For now, we return a "not implemented" response.

      declare
         Resp_Status : Syscall_Status := Syscall_OK;
         Resp_Len : Word32 := 0;
      begin
         case Request_Header.Syscall_Num is
            when Syscall_SLoad =>
               --  TODO: Call actual storage load
               Ada.Text_IO.Put_Line ("  [Parent] SLOAD syscall - returning zeros");
               Syscall_Gas := 200;  --  Base SLOAD cost
               Resp_Len := 32;  --  32-byte value
               Response_Data (0 .. 31) := (others => 0);

            when Syscall_SStore =>
               --  TODO: Call actual storage store
               Ada.Text_IO.Put_Line ("  [Parent] SSTORE syscall - storing");
               Syscall_Gas := 20000;  --  Base SSTORE cost
               Resp_Len := 0;

            when Syscall_SHA3 =>
               --  TODO: Call actual SHA3 hash
               Ada.Text_IO.Put_Line ("  [Parent] SHA3 syscall");
               Syscall_Gas := 30 + Gas_Amount (Request_Header.Data_Length) * 6;
               Resp_Len := 32;  --  32-byte hash
               --  Compute hash using Anubis_SHA3
               declare
                  use Anubis_Types;
                  use Anubis_SHA3;
                  Input : Anubis_Types.Byte_Array (0 .. Natural (Request_Header.Data_Length) - 1);
                  Digest : SHA3_256_Digest;
               begin
                  for I in Input'Range loop
                     Input (I) := Anubis_Types.Byte (Request_Data (I));
                  end loop;
                  SHA3_256 (Input, Digest);
                  for I in 0 .. 31 loop
                     Response_Data (I) := Aegis_VM_Types.Byte (Digest (I));
                  end loop;
               end;

            when Syscall_Log =>
               --  Emit event log
               Ada.Text_IO.Put_Line ("  [Parent] LOG syscall");
               Syscall_Gas := 375 + Gas_Amount (Request_Header.Data_Length) * 8;
               Resp_Len := 0;

            when Syscall_ML_DSA_Sign =>
               --  ML-DSA-87 Sign syscall
               --  Request format: [SK (4896 bytes)] [Random (32 bytes)] [Msg (variable)]
               --  Response: [Sig (4627 bytes)] [Success (1 byte)]
               Ada.Text_IO.Put_Line ("  [Parent] ML-DSA-87 SIGN syscall");
               declare
                  use Anubis_MLDSA;
                  use Anubis_MLDSA_Types;

                  SK_Size    : constant := Secret_Key_Bytes;    -- 4896
                  Seed_Size  : constant := Seed_Bytes;           -- 32
                  Sig_Size   : constant := Signature_Bytes;      -- 4627
                  Min_Req    : constant := SK_Size + Seed_Size;  -- 4928
                  Data_Len   : constant Natural := Natural (Request_Header.Data_Length);
               begin
                  if Data_Len < Min_Req then
                     Ada.Text_IO.Put_Line ("  [Parent]   ERROR: Insufficient data for ML-DSA Sign");
                     Resp_Status := Syscall_Invalid_Args;
                     Resp_Len := 0;
                  else
                     declare
                        SK : Secret_Key;
                        Random_Seed : Seed;
                        Msg_Len : constant Natural := Data_Len - Min_Req;
                        Msg : Anubis_Types.Byte_Array (0 .. (if Msg_Len > 0 then Msg_Len - 1 else 0));
                        Sig : Signature;
                        Sign_Success : Boolean;
                     begin
                        --  Extract SK
                        for I in 0 .. SK_Size - 1 loop
                           SK (I) := Anubis_Types.Byte (Request_Data (I));
                        end loop;

                        --  Extract Random seed
                        for I in 0 .. Seed_Size - 1 loop
                           Random_Seed (I) := Anubis_Types.Byte (Request_Data (SK_Size + I));
                        end loop;

                        --  Extract message
                        for I in Msg'Range loop
                           Msg (I) := Anubis_Types.Byte (Request_Data (Min_Req + I));
                        end loop;

                        --  Sign the message
                        Anubis_MLDSA.Sign (
                           SK      => SK,
                           Msg     => Msg,
                           Random  => Random_Seed,
                           Sig     => Sig,
                           Success => Sign_Success
                        );

                        if Sign_Success then
                           --  Copy signature to response
                           for I in 0 .. Sig_Size - 1 loop
                              Response_Data (I) := Aegis_VM_Types.Byte (Sig (I));
                           end loop;
                           Response_Data (Sig_Size) := 1;  --  Success = 1
                           Resp_Len := Word32 (Sig_Size + 1);
                           Ada.Text_IO.Put_Line ("  [Parent]   Sign SUCCESS");
                        else
                           Response_Data (0) := 0;  --  Success = 0
                           Resp_Len := 1;
                           Resp_Status := Syscall_Error;
                           Ada.Text_IO.Put_Line ("  [Parent]   Sign FAILED (rejection sampling)");
                        end if;

                        --  Gas: Base cost + per-byte message cost
                        Syscall_Gas := 50000 + Gas_Amount (Msg_Len) * 10;
                     end;
                  end if;
               end;

            when Syscall_ML_DSA_Vrfy =>
               --  ML-DSA-87 Verify syscall
               --  Request format: [PK (2592 bytes)] [Sig (4627 bytes)] [Msg (variable)]
               --  Response: [Valid (1 byte)]
               Ada.Text_IO.Put_Line ("  [Parent] ML-DSA-87 VERIFY syscall");
               declare
                  use Anubis_MLDSA;
                  use Anubis_MLDSA_Types;

                  PK_Size   : constant := Public_Key_Bytes;     -- 2592
                  Sig_Size  : constant := Signature_Bytes;       -- 4627
                  Min_Req   : constant := PK_Size + Sig_Size;   -- 7219
                  Data_Len  : constant Natural := Natural (Request_Header.Data_Length);
               begin
                  if Data_Len < Min_Req then
                     Ada.Text_IO.Put_Line ("  [Parent]   ERROR: Insufficient data for ML-DSA Verify");
                     Resp_Status := Syscall_Invalid_Args;
                     Resp_Len := 0;
                  else
                     declare
                        PK : Public_Key;
                        Sig : Signature;
                        Msg_Len : constant Natural := Data_Len - Min_Req;
                        Msg : Anubis_Types.Byte_Array (0 .. (if Msg_Len > 0 then Msg_Len - 1 else 0));
                        Valid : Boolean;
                     begin
                        --  Extract PK
                        for I in 0 .. PK_Size - 1 loop
                           PK (I) := Anubis_Types.Byte (Request_Data (I));
                        end loop;

                        --  Extract Signature
                        for I in 0 .. Sig_Size - 1 loop
                           Sig (I) := Anubis_Types.Byte (Request_Data (PK_Size + I));
                        end loop;

                        --  Extract message
                        for I in Msg'Range loop
                           Msg (I) := Anubis_Types.Byte (Request_Data (Min_Req + I));
                        end loop;

                        --  Verify the signature
                        Valid := Anubis_MLDSA.Verify (
                           PK  => PK,
                           Msg => Msg,
                           Sig => Sig
                        );

                        if Valid then
                           Response_Data (0) := 1;  --  Valid = 1
                           Ada.Text_IO.Put_Line ("  [Parent]   Verify: VALID");
                        else
                           Response_Data (0) := 0;  --  Valid = 0
                           Ada.Text_IO.Put_Line ("  [Parent]   Verify: INVALID");
                        end if;
                        Resp_Len := 1;

                        --  Gas: Base cost + per-byte message cost
                        Syscall_Gas := 30000 + Gas_Amount (Msg_Len) * 5;
                     end;
                  end if;
               end;

            when Syscall_ML_KEM_Enc =>
               --  ML-KEM-1024 Encapsulate syscall
               --  Request format: [EK (1568 bytes)] [Random (32 bytes)]
               --  Response: [SS (32 bytes)] [CT (1568 bytes)]
               Ada.Text_IO.Put_Line ("  [Parent] ML-KEM-1024 ENCAPSULATE syscall");
               declare
                  use Anubis_MLKEM_Types;

                  EK_Size   : constant := Anubis_MLKEM.Encapsulation_Key_Bytes;  -- 1568
                  Rand_Size : constant := Anubis_MLKEM.Seed_Bytes;                -- 32
                  SS_Size   : constant := Anubis_MLKEM.Shared_Secret_Bytes;       -- 32
                  CT_Size   : constant := Anubis_MLKEM.Ciphertext_Bytes;          -- 1568
                  Req_Size  : constant := EK_Size + Rand_Size;                    -- 1600
                  Data_Len  : constant Natural := Natural (Request_Header.Data_Length);
               begin
                  if Data_Len < Req_Size then
                     Ada.Text_IO.Put_Line ("  [Parent]   ERROR: Insufficient data for ML-KEM Encaps");
                     Resp_Status := Syscall_Invalid_Args;
                     Resp_Len := 0;
                  else
                     declare
                        EK : Encapsulation_Key;
                        Random_M : Anubis_MLKEM_Types.Seed;
                        SS : Shared_Secret;
                        CT : MLKEM_Ciphertext;
                     begin
                        --  Extract EK
                        for I in 0 .. EK_Size - 1 loop
                           EK (I) := Anubis_Types.Byte (Request_Data (I));
                        end loop;

                        --  Extract Random
                        for I in 0 .. Rand_Size - 1 loop
                           Random_M (I) := Anubis_Types.Byte (Request_Data (EK_Size + I));
                        end loop;

                        --  Encapsulate
                        Anubis_MLKEM.Encaps (
                           EK       => EK,
                           Random_M => Random_M,
                           SS       => SS,
                           CT       => CT
                        );

                        --  Copy SS to response (first 32 bytes)
                        for I in 0 .. SS_Size - 1 loop
                           Response_Data (I) := Aegis_VM_Types.Byte (SS (I));
                        end loop;

                        --  Copy CT to response (next 1568 bytes)
                        for I in 0 .. CT_Size - 1 loop
                           Response_Data (SS_Size + I) := Aegis_VM_Types.Byte (CT (I));
                        end loop;

                        Resp_Len := Word32 (SS_Size + CT_Size);  --  1600 bytes
                        Ada.Text_IO.Put_Line ("  [Parent]   Encapsulate SUCCESS");

                        --  Gas cost for ML-KEM encapsulation
                        Syscall_Gas := 25000;
                     end;
                  end if;
               end;

            when Syscall_ML_KEM_Dec =>
               --  ML-KEM-1024 Decapsulate syscall
               --  Request format: [DK (3168 bytes)] [CT (1568 bytes)]
               --  Response: [SS (32 bytes)]
               Ada.Text_IO.Put_Line ("  [Parent] ML-KEM-1024 DECAPSULATE syscall");
               declare
                  use Anubis_MLKEM_Types;

                  DK_Size   : constant := Anubis_MLKEM.Decapsulation_Key_Bytes;  -- 3168
                  CT_Size   : constant := Anubis_MLKEM.Ciphertext_Bytes;          -- 1568
                  SS_Size   : constant := Anubis_MLKEM.Shared_Secret_Bytes;       -- 32
                  Req_Size  : constant := DK_Size + CT_Size;                      -- 4736
                  Data_Len  : constant Natural := Natural (Request_Header.Data_Length);
               begin
                  if Data_Len < Req_Size then
                     Ada.Text_IO.Put_Line ("  [Parent]   ERROR: Insufficient data for ML-KEM Decaps");
                     Resp_Status := Syscall_Invalid_Args;
                     Resp_Len := 0;
                  else
                     declare
                        DK : Decapsulation_Key;
                        CT : MLKEM_Ciphertext;
                        SS : Shared_Secret;
                     begin
                        --  Extract DK
                        for I in 0 .. DK_Size - 1 loop
                           DK (I) := Anubis_Types.Byte (Request_Data (I));
                        end loop;

                        --  Extract CT
                        for I in 0 .. CT_Size - 1 loop
                           CT (I) := Anubis_Types.Byte (Request_Data (DK_Size + I));
                        end loop;

                        --  Decapsulate
                        Anubis_MLKEM.Decaps (
                           DK => DK,
                           CT => CT,
                           SS => SS
                        );

                        --  Copy SS to response
                        for I in 0 .. SS_Size - 1 loop
                           Response_Data (I) := Aegis_VM_Types.Byte (SS (I));
                        end loop;

                        Resp_Len := Word32 (SS_Size);  --  32 bytes
                        Ada.Text_IO.Put_Line ("  [Parent]   Decapsulate SUCCESS");

                        --  Gas cost for ML-KEM decapsulation
                        Syscall_Gas := 30000;
                     end;
                  end if;
               end;

            when Syscall_Return =>
               --  Contract is returning successfully with data
               --  Request: [Return data (variable length)]
               --  Response: [Acknowledged (1 byte)]
               --
               --  The return data is passed directly - we just acknowledge receipt.
               --  The actual return data handling is done in the Execute loop.
               Ada.Text_IO.Put_Line ("  [Parent] RETURN syscall");
               Ada.Text_IO.Put_Line ("  [Parent]   Return data length: " &
                  Word32'Image (Request_Header.Data_Length));

               --  Copy return data for later use
               if Request_Header.Data_Length > 0 and
                  Request_Header.Data_Length <= Word32 (Max_Syscall_Data)
               then
                  declare
                     Return_Len : constant Natural := Natural (Request_Header.Data_Length);
                  begin
                     --  Return data is in Request_Data, already read by caller
                     --  We just acknowledge - the Execute loop handles the data
                     Ada.Text_IO.Put_Line ("  [Parent]   Return data received OK");
                  end;
               end if;

               Resp_Status := Syscall_OK;
               Resp_Len := 1;
               Response_Data (Response_Data'First) := 1;  --  Acknowledged

               --  Minimal gas for return syscall
               Syscall_Gas := 100;

            --  =================================================================
            --  BLOCKCHAIN CONTEXT SYSCALLS
            --  =================================================================
            --  These syscalls provide read-only access to blockchain state.
            --  Values come from the execution context set by the node.
            --
            --  NOTE: In subprocess execution, these values must be passed from
            --  the parent process. For now, we return placeholder values.
            --  TODO: Wire these to actual block context from parent.

            when Syscall_Timestamp =>
               --  Get current block timestamp (Unix seconds)
               --  Response: [timestamp (8 bytes, little-endian)]
               Ada.Text_IO.Put_Line ("  [Parent] TIMESTAMP syscall");

               --  TODO: Get actual timestamp from execution context
               --  For now, return current Unix time as placeholder
               declare
                  --  Placeholder: would come from block context
                  Timestamp : constant Word64 := 1734879600;  --  Example timestamp
               begin
                  Response_Data (0) := Byte (Timestamp and 16#FF#);
                  Response_Data (1) := Byte (Shift_Right (Timestamp, 8) and 16#FF#);
                  Response_Data (2) := Byte (Shift_Right (Timestamp, 16) and 16#FF#);
                  Response_Data (3) := Byte (Shift_Right (Timestamp, 24) and 16#FF#);
                  Response_Data (4) := Byte (Shift_Right (Timestamp, 32) and 16#FF#);
                  Response_Data (5) := Byte (Shift_Right (Timestamp, 40) and 16#FF#);
                  Response_Data (6) := Byte (Shift_Right (Timestamp, 48) and 16#FF#);
                  Response_Data (7) := Byte (Shift_Right (Timestamp, 56) and 16#FF#);
               end;

               Resp_Status := Syscall_OK;
               Resp_Len := 8;
               Syscall_Gas := 2;  --  Very cheap read

            when Syscall_BlockNumber =>
               --  Get current block number
               --  Response: [block_number (8 bytes, little-endian)]
               Ada.Text_IO.Put_Line ("  [Parent] BLOCKNUMBER syscall");

               declare
                  --  Placeholder: would come from block context
                  Block_Num : constant Word64 := 1000000;  --  Example block number
               begin
                  Response_Data (0) := Byte (Block_Num and 16#FF#);
                  Response_Data (1) := Byte (Shift_Right (Block_Num, 8) and 16#FF#);
                  Response_Data (2) := Byte (Shift_Right (Block_Num, 16) and 16#FF#);
                  Response_Data (3) := Byte (Shift_Right (Block_Num, 24) and 16#FF#);
                  Response_Data (4) := Byte (Shift_Right (Block_Num, 32) and 16#FF#);
                  Response_Data (5) := Byte (Shift_Right (Block_Num, 40) and 16#FF#);
                  Response_Data (6) := Byte (Shift_Right (Block_Num, 48) and 16#FF#);
                  Response_Data (7) := Byte (Shift_Right (Block_Num, 56) and 16#FF#);
               end;

               Resp_Status := Syscall_OK;
               Resp_Len := 8;
               Syscall_Gas := 2;  --  Very cheap read

            when Syscall_ChainID =>
               --  Get chain ID (for replay protection)
               --  Response: [chain_id (8 bytes, little-endian)]
               Ada.Text_IO.Put_Line ("  [Parent] CHAINID syscall");

               declare
                  --  AnubisVM mainnet chain ID: 0x414E5542 ("ANUB" in hex)
                  Chain_ID : constant Word64 := 16#414E5542#;
               begin
                  Response_Data (0) := Byte (Chain_ID and 16#FF#);
                  Response_Data (1) := Byte (Shift_Right (Chain_ID, 8) and 16#FF#);
                  Response_Data (2) := Byte (Shift_Right (Chain_ID, 16) and 16#FF#);
                  Response_Data (3) := Byte (Shift_Right (Chain_ID, 24) and 16#FF#);
                  Response_Data (4) := Byte (Shift_Right (Chain_ID, 32) and 16#FF#);
                  Response_Data (5) := Byte (Shift_Right (Chain_ID, 40) and 16#FF#);
                  Response_Data (6) := Byte (Shift_Right (Chain_ID, 48) and 16#FF#);
                  Response_Data (7) := Byte (Shift_Right (Chain_ID, 56) and 16#FF#);
               end;

               Resp_Status := Syscall_OK;
               Resp_Len := 8;
               Syscall_Gas := 2;  --  Very cheap read

            when Syscall_Balance =>
               --  Get account balance
               --  Request: [address (32 bytes)]
               --  Response: [balance (32 bytes, U256 little-endian)]
               Ada.Text_IO.Put_Line ("  [Parent] BALANCE syscall");

               if Request_Header.Data_Length >= 32 then
                  --  TODO: Look up actual balance from state
                  --  For now, return 0 balance as placeholder
                  Ada.Text_IO.Put_Line ("  [Parent]   Address query received");

                  for I in 0 .. 31 loop
                     Response_Data (I) := 0;
                  end loop;

                  Resp_Status := Syscall_OK;
                  Resp_Len := 32;
                  Syscall_Gas := 100;  --  Cold storage access
               else
                  Ada.Text_IO.Put_Line ("  [Parent]   Invalid address length");
                  Resp_Status := Syscall_Invalid_Args;
                  Resp_Len := 0;
                  Syscall_Gas := 0;
               end if;

            when Syscall_SelfBalance =>
               --  Get own contract balance (cheaper than BALANCE)
               --  Request: (none)
               --  Response: [balance (32 bytes, U256 little-endian)]
               Ada.Text_IO.Put_Line ("  [Parent] SELFBALANCE syscall");

               --  TODO: Look up contract's own balance
               --  For now, return 0 as placeholder
               for I in 0 .. 31 loop
                  Response_Data (I) := 0;
               end loop;

               Resp_Status := Syscall_OK;
               Resp_Len := 32;
               Syscall_Gas := 5;  --  Cheaper than BALANCE (warm access)

            when Syscall_BlockHash =>
               --  Get block hash for recent block (last 256 blocks only)
               --  Request: [block_number (8 bytes)]
               --  Response: [block_hash (32 bytes)] or error if too old
               Ada.Text_IO.Put_Line ("  [Parent] BLOCKHASH syscall");

               if Request_Header.Data_Length >= 8 then
                  --  TODO: Look up actual block hash from chain
                  --  For now, return zeros (indicates block not available)
                  Ada.Text_IO.Put_Line ("  [Parent]   Block number query received");

                  for I in 0 .. 31 loop
                     Response_Data (I) := 0;
                  end loop;

                  Resp_Status := Syscall_OK;
                  Resp_Len := 32;
                  Syscall_Gas := 20;  --  Relatively cheap
               else
                  Ada.Text_IO.Put_Line ("  [Parent]   Invalid block number length");
                  Resp_Status := Syscall_Invalid_Args;
                  Resp_Len := 0;
                  Syscall_Gas := 0;
               end if;

            when Syscall_GasPrice =>
               --  Get current gas price
               --  Response: [gas_price (8 bytes, little-endian)]
               Ada.Text_IO.Put_Line ("  [Parent] GASPRICE syscall");

               declare
                  --  Placeholder: 1 gwei = 10^9 wei
                  Gas_Price_Val : constant Word64 := 1_000_000_000;
               begin
                  Response_Data (0) := Byte (Gas_Price_Val and 16#FF#);
                  Response_Data (1) := Byte (Shift_Right (Gas_Price_Val, 8) and 16#FF#);
                  Response_Data (2) := Byte (Shift_Right (Gas_Price_Val, 16) and 16#FF#);
                  Response_Data (3) := Byte (Shift_Right (Gas_Price_Val, 24) and 16#FF#);
                  Response_Data (4) := Byte (Shift_Right (Gas_Price_Val, 32) and 16#FF#);
                  Response_Data (5) := Byte (Shift_Right (Gas_Price_Val, 40) and 16#FF#);
                  Response_Data (6) := Byte (Shift_Right (Gas_Price_Val, 48) and 16#FF#);
                  Response_Data (7) := Byte (Shift_Right (Gas_Price_Val, 56) and 16#FF#);
               end;

               Resp_Status := Syscall_OK;
               Resp_Len := 8;
               Syscall_Gas := 2;

            when Syscall_GasLimit =>
               --  Get block gas limit
               --  Response: [gas_limit (8 bytes, little-endian)]
               Ada.Text_IO.Put_Line ("  [Parent] GASLIMIT syscall");

               declare
                  --  Placeholder: 30M gas limit (similar to Ethereum)
                  Gas_Lim : constant Word64 := 30_000_000;
               begin
                  Response_Data (0) := Byte (Gas_Lim and 16#FF#);
                  Response_Data (1) := Byte (Shift_Right (Gas_Lim, 8) and 16#FF#);
                  Response_Data (2) := Byte (Shift_Right (Gas_Lim, 16) and 16#FF#);
                  Response_Data (3) := Byte (Shift_Right (Gas_Lim, 24) and 16#FF#);
                  Response_Data (4) := Byte (Shift_Right (Gas_Lim, 32) and 16#FF#);
                  Response_Data (5) := Byte (Shift_Right (Gas_Lim, 40) and 16#FF#);
                  Response_Data (6) := Byte (Shift_Right (Gas_Lim, 48) and 16#FF#);
                  Response_Data (7) := Byte (Shift_Right (Gas_Lim, 56) and 16#FF#);
               end;

               Resp_Status := Syscall_OK;
               Resp_Len := 8;
               Syscall_Gas := 2;

            when Syscall_Coinbase =>
               --  Get block proposer/validator address
               --  Response: [address (32 bytes)]
               Ada.Text_IO.Put_Line ("  [Parent] COINBASE syscall");

               --  TODO: Get actual coinbase from block context
               --  For now, return zeros as placeholder
               for I in 0 .. 31 loop
                  Response_Data (I) := 0;
               end loop;

               Resp_Status := Syscall_OK;
               Resp_Len := 32;
               Syscall_Gas := 2;

            when Syscall_Revert =>
               --  Contract is reverting
               Ada.Text_IO.Put_Line ("  [Parent] REVERT syscall");
               Resp_Status := Syscall_Error;
               Resp_Len := 0;

            when others =>
               Ada.Text_IO.Put_Line ("  [Parent] Unknown syscall: " &
                  Word32'Image (Request_Header.Syscall_Num));
               Resp_Status := Syscall_Denied;
               Syscall_Gas := 0;
               Resp_Len := 0;
         end case;

         --  Charge gas
         Total_Gas := Total_Gas + Syscall_Gas;

         --  Build response
         Response_Header := (
            Magic       => Syscall_Response_Magic,
            Status      => Word32 (Syscall_Status'Pos (Resp_Status)),
            Data_Length => Resp_Len,
            Gas_Used    => Word64 (Syscall_Gas)
         );

         --  Send response header
         Written := C_write (Response_Fd, Response_Header'Address,
            Interfaces.C.size_t (Response_Header'Size / 8));

         --  Send response data
         if Resp_Len > 0 then
            Written := C_write (Response_Fd, Response_Data (Response_Data'First)'Address,
               Interfaces.C.size_t (Resp_Len));
         end if;

         Success := True;
      end;
   end Handle_Syscall_Request;

   ---------------------------------------------------------------------------
   --  Execute with Syscall Support (Main Entry Point)
   ---------------------------------------------------------------------------

   function Execute_With_Syscalls (
      Entry_Point   : System.Address;
      Calldata      : access constant Byte_Array;
      Gas_Limit     : Gas_Amount;
      Timeout_Ms    : Natural;
      Sandbox       : Sandbox_Level;
      Limits        : Resource_Limits
   ) return Subprocess_Result
   is
      Result : Subprocess_Result := Failed_Result;

      --  Result pipe: child -> parent (for final execution result)
      Result_Pipe : aliased array (0 .. 1) of Interfaces.C.int := (others => -1);

      --  Syscall request pipe: child -> parent (for syscall requests)
      Request_Pipe : aliased array (0 .. 1) of Interfaces.C.int := (others => -1);

      --  Syscall response pipe: parent -> child (for syscall responses)
      Response_Pipe : aliased array (0 .. 1) of Interfaces.C.int := (others => -1);

      --  Process tracking
      Child_Pid : Interfaces.C.int;
      Wait_Status : aliased Interfaces.C.int := 0;
      Wait_Result : Interfaces.C.int;

      --  Timing
      Start_Time   : aliased Timespec;
      Current_Time : aliased Timespec;
      Elapsed_Ms   : Natural;
      Clock_Res    : Interfaces.C.int;
      pragma Unreferenced (Clock_Res);

      --  Poll for syscall requests
      Pfds : aliased array (0 .. 1) of Pollfd;
      Poll_Res : Interfaces.C.int;

      --  IPC data
      Header : aliased IPC_Header;
      Bytes_Read : Interfaces.C.long;
      Return_Buf : aliased Byte_Array (0 .. Max_Return_Data - 1) := (others => 0);

      --  Syscall tracking
      Total_Syscall_Gas : Gas_Amount := 0;
      Syscall_OK : Boolean;

      Res : Interfaces.C.int;
      pragma Unreferenced (Res);
   begin
      Ada.Text_IO.Put_Line ("  [Subprocess] Starting syscall-enabled execution...");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Gas limit: " & Gas_Amount'Image (Gas_Limit));
      Ada.Text_IO.Put_Line ("  [Subprocess]   Timeout: " & Natural'Image (Timeout_Ms) & " ms");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Syscall IPC: ENABLED");

      --  Record start time
      Clock_Res := C_clock_gettime (CLOCK_MONOTONIC, Start_Time'Access);

      --  Create pipes
      if C_pipe (Result_Pipe (0)'Address) /= 0 then
         Ada.Text_IO.Put_Line ("  [Subprocess] ERROR: result pipe() failed");
         Result.Reason := Exit_IPC_Error;
         return Result;
      end if;

      if C_pipe (Request_Pipe (0)'Address) /= 0 then
         Ada.Text_IO.Put_Line ("  [Subprocess] ERROR: request pipe() failed");
         Res := C_close (Result_Pipe (0));
         Res := C_close (Result_Pipe (1));
         Result.Reason := Exit_IPC_Error;
         return Result;
      end if;

      if C_pipe (Response_Pipe (0)'Address) /= 0 then
         Ada.Text_IO.Put_Line ("  [Subprocess] ERROR: response pipe() failed");
         Res := C_close (Result_Pipe (0));
         Res := C_close (Result_Pipe (1));
         Res := C_close (Request_Pipe (0));
         Res := C_close (Request_Pipe (1));
         Result.Reason := Exit_IPC_Error;
         return Result;
      end if;

      Ada.Text_IO.Put_Line ("  [Subprocess] Pipes created:");
      Ada.Text_IO.Put_Line ("    Result:   read=" &
         Interfaces.C.int'Image (Result_Pipe (0)) & " write=" &
         Interfaces.C.int'Image (Result_Pipe (1)));
      Ada.Text_IO.Put_Line ("    Request:  read=" &
         Interfaces.C.int'Image (Request_Pipe (0)) & " write=" &
         Interfaces.C.int'Image (Request_Pipe (1)));
      Ada.Text_IO.Put_Line ("    Response: read=" &
         Interfaces.C.int'Image (Response_Pipe (0)) & " write=" &
         Interfaces.C.int'Image (Response_Pipe (1)));

      --  Fork child process
      Child_Pid := C_fork;

      if Child_Pid < 0 then
         --  Fork failed
         Ada.Text_IO.Put_Line ("  [Subprocess] ERROR: fork() failed");
         Res := C_close (Result_Pipe (0));
         Res := C_close (Result_Pipe (1));
         Res := C_close (Request_Pipe (0));
         Res := C_close (Request_Pipe (1));
         Res := C_close (Response_Pipe (0));
         Res := C_close (Response_Pipe (1));
         Result.Reason := Exit_Fork_Failed;
         return Result;

      elsif Child_Pid = 0 then
         --  ========== CHILD PROCESS ==========
         --  Close parent ends of pipes
         Res := C_close (Result_Pipe (0));    --  Parent reads result
         Res := C_close (Request_Pipe (0));   --  Parent reads requests
         Res := C_close (Response_Pipe (1));  --  Parent writes responses

         --  Execute contract in sandbox with syscall IPC
         Execute_With_Syscalls_Child (
            Entry_Point   => Entry_Point,
            Calldata      => Calldata,
            Gas_Limit     => Gas_Limit,
            Result_Fd     => Result_Pipe (1),
            Request_Fd    => Request_Pipe (1),
            Response_Fd   => Response_Pipe (0),
            Sandbox       => Sandbox,
            Limits        => Limits
         );

         --  Should never reach here
         C_exit (Interfaces.C.int (127));
         return Result;
      end if;

      --  ========== PARENT PROCESS ==========
      Ada.Text_IO.Put_Line ("  [Subprocess] Forked child PID: " &
         Interfaces.C.int'Image (Child_Pid));

      --  Close child ends of pipes
      Res := C_close (Result_Pipe (1));    --  Child writes result
      Res := C_close (Request_Pipe (1));   --  Child writes requests
      Res := C_close (Response_Pipe (0));  --  Child reads responses

      --  Event loop: handle syscall requests and monitor child
      loop
         --  Check elapsed time
         Clock_Res := C_clock_gettime (CLOCK_MONOTONIC, Current_Time'Access);
         Elapsed_Ms := Natural (
            (Current_Time.Tv_Sec - Start_Time.Tv_Sec) * 1000 +
            (Current_Time.Tv_Nsec - Start_Time.Tv_Nsec) / 1_000_000);

         if Elapsed_Ms >= Timeout_Ms then
            --  Timeout! Kill the child
            Ada.Text_IO.Put_Line ("  [Subprocess] TIMEOUT after " &
               Natural'Image (Elapsed_Ms) & " ms");
            Res := C_kill (Child_Pid, SIGKILL);
            Wait_Result := C_waitpid (Child_Pid, Wait_Status'Access, 0);
            --  Wait_Result intentionally ignored here

            Result.Reason := Exit_Timeout;
            Result.Gas_Used := Gas_Limit;
            Result.Wall_Time_Ms := Elapsed_Ms;
            Result.Success := False;

            Res := C_close (Result_Pipe (0));
            Res := C_close (Request_Pipe (0));
            Res := C_close (Response_Pipe (1));
            return Result;
         end if;

         --  Check if child is done (non-blocking)
         Wait_Result := C_waitpid (Child_Pid, Wait_Status'Access, WNOHANG);

         if Wait_Result > 0 then
            --  Child exited
            Ada.Text_IO.Put_Line ("  [Subprocess] Child exited, status: " &
               Interfaces.C.int'Image (Wait_Status));
            exit;
         elsif Wait_Result < 0 then
            --  Error in waitpid
            Ada.Text_IO.Put_Line ("  [Subprocess] ERROR in waitpid");
            Result.Reason := Exit_Unknown;
            Result.Gas_Used := Gas_Limit;
            Res := C_close (Result_Pipe (0));
            Res := C_close (Request_Pipe (0));
            Res := C_close (Response_Pipe (1));
            return Result;
         end if;

         --  Poll for syscall requests and result (100ms timeout)
         Pfds (0) := (Fd => Result_Pipe (0), Events => POLLIN, Revents => 0);
         Pfds (1) := (Fd => Request_Pipe (0), Events => POLLIN, Revents => 0);
         Poll_Res := C_poll (Pfds (0)'Address, 2, 100);

         if Poll_Res > 0 then
            --  Check for syscall request
            if (Unsigned_16 (Pfds (1).Revents) and Unsigned_16 (POLLIN)) /= 0 then
               Ada.Text_IO.Put_Line ("  [Subprocess] Handling syscall request...");
               Handle_Syscall_Request (
                  Request_Fd  => Request_Pipe (0),
                  Response_Fd => Response_Pipe (1),
                  Total_Gas   => Total_Syscall_Gas,
                  Success     => Syscall_OK
               );
               if not Syscall_OK then
                  Ada.Text_IO.Put_Line ("  [Subprocess] Syscall handling failed");
               end if;
            end if;

            --  Result pipe data will be read after loop
         end if;
      end loop;

      --  Decode exit status
      declare
         Exit_Reason : Sphinx_Subprocess.Exit_Reason;
         Exit_Code   : Integer;
         Signal_Num  : Integer;
      begin
         Decode_Wait_Status (Wait_Status, Exit_Reason, Exit_Code, Signal_Num);
         Result.Reason := Exit_Reason;
         Result.Exit_Code := Exit_Code;
         Result.Signal_Number := Signal_Num;
      end;

      --  Read final result from IPC pipe
      Bytes_Read := C_read (Result_Pipe (0), Header'Address,
         Interfaces.C.size_t (Header'Size / 8));

      if Bytes_Read = Interfaces.C.long (Header'Size / 8) then
         if Header.Magic = IPC_Magic and Header.Version = IPC_Version then
            Ada.Text_IO.Put_Line ("  [Subprocess] IPC header valid");
            Ada.Text_IO.Put_Line ("  [Subprocess]   Contract gas: " &
               Word64'Image (Header.Gas_Used));
            Ada.Text_IO.Put_Line ("  [Subprocess]   Syscall gas: " &
               Gas_Amount'Image (Total_Syscall_Gas));

            --  Total gas = contract gas + syscall gas
            Result.Gas_Used := Gas_Amount (Header.Gas_Used) + Total_Syscall_Gas;
            Result.Return_Length := Natural (Header.Return_Len);

            --  Read return data if any
            if Header.Return_Len > 0 and
               Natural (Header.Return_Len) <= Max_Return_Data
            then
               Bytes_Read := C_read (Result_Pipe (0), Return_Buf (Return_Buf'First)'Address,
                  Interfaces.C.size_t (Header.Return_Len));

               if Bytes_Read = Interfaces.C.long (Header.Return_Len) then
                  declare
                     use Anubis_Types;
                     use Anubis_SHA3;
                     Data : Anubis_Types.Byte_Array (0 .. Natural (Header.Return_Len) - 1);
                     Digest : SHA3_256_Digest;
                  begin
                     for I in 0 .. Natural (Header.Return_Len) - 1 loop
                        Data (I) := Anubis_Types.Byte (Return_Buf (I));
                     end loop;
                     SHA3_256 (Data, Digest);
                     for I in 0 .. 31 loop
                        Result.Return_Data (I) := Aegis_VM_Types.Byte (Digest (I));
                     end loop;
                  end;
               end if;
            end if;

            if Result.Reason = Exit_Success or
               (Result.Reason = Exit_Failure and Header.Exit_Code = 0)
            then
               Result.Success := True;
               Result.Reason := Exit_Success;
            else
               Result.Success := False;
            end if;
         else
            Ada.Text_IO.Put_Line ("  [Subprocess] IPC header invalid!");
            Result.Reason := Exit_IPC_Error;
            Result.Gas_Used := Gas_Limit;
         end if;
      else
         Ada.Text_IO.Put_Line ("  [Subprocess] Failed to read IPC header");
         Result.Gas_Used := Gas_Limit;
      end if;

      --  Calculate wall time
      Clock_Res := C_clock_gettime (CLOCK_MONOTONIC, Current_Time'Access);
      Result.Wall_Time_Ms := Natural (
         (Current_Time.Tv_Sec - Start_Time.Tv_Sec) * 1000 +
         (Current_Time.Tv_Nsec - Start_Time.Tv_Nsec) / 1_000_000);

      --  Close pipes
      Res := C_close (Result_Pipe (0));
      Res := C_close (Request_Pipe (0));
      Res := C_close (Response_Pipe (1));

      Ada.Text_IO.Put_Line ("  [Subprocess] Syscall-enabled execution complete");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Wall time: " &
         Natural'Image (Result.Wall_Time_Ms) & " ms");
      Ada.Text_IO.Put_Line ("  [Subprocess]   Total gas: " &
         Gas_Amount'Image (Result.Gas_Used));
      Ada.Text_IO.Put_Line ("  [Subprocess]   Success: " & Boolean'Image (Result.Success));

      return Result;
   end Execute_With_Syscalls;

end Sphinx_Subprocess;
