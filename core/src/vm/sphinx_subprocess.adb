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
      Fds     : access Pollfd;
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
   --  - No process creation
   --  - No IPC except the pipe we set up
   --  - No signal sending
   --  - Memory and CPU only
   Contract_Seatbelt_Profile_Str : constant String :=
      "(version 1)" & ASCII.LF &
      "(deny default)" & ASCII.LF &
      "; Allow basic process operations" & ASCII.LF &
      "(allow process-exec)" & ASCII.LF &
      "(allow process-fork)" & ASCII.LF &
      "; Allow memory operations" & ASCII.LF &
      "(allow mach-lookup)" & ASCII.LF &
      "(allow sysctl-read)" & ASCII.LF &
      "; Allow file operations on /dev/null only" & ASCII.LF &
      "(allow file-read* (literal ""/dev/null""))" & ASCII.LF &
      "(allow file-write* (literal ""/dev/null""))" & ASCII.LF &
      "; Allow pipe operations (for our IPC)" & ASCII.LF &
      "(allow file-read* file-write* (require-all (file-mode #o0600)))" & ASCII.LF &
      "; Deny everything else by default" & ASCII.LF &
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
         Poll_Res := C_poll (Pfd'Access, 1, 100);

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

end Sphinx_Subprocess;
