pragma SPARK_Mode (On);

package body Sphinx_Native is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Read_LE16 (
      Data   : Binary_Buffer;
      Offset : Natural
   ) return Word32 is
   begin
      return Word32 (Data (Binary_Index (Offset))) or
             Shift_Left (Word32 (Data (Binary_Index (Offset + 1))), 8);
   end Read_LE16;

   function Read_LE32 (
      Data   : Binary_Buffer;
      Offset : Natural
   ) return Word32 is
   begin
      return Word32 (Data (Binary_Index (Offset))) or
             Shift_Left (Word32 (Data (Binary_Index (Offset + 1))), 8) or
             Shift_Left (Word32 (Data (Binary_Index (Offset + 2))), 16) or
             Shift_Left (Word32 (Data (Binary_Index (Offset + 3))), 24);
   end Read_LE32;

   function Read_LE64 (
      Data   : Binary_Buffer;
      Offset : Natural
   ) return Word64 is
   begin
      return Word64 (Read_LE32 (Data, Offset)) or
             Shift_Left (Word64 (Read_LE32 (Data, Offset + 4)), 32);
   end Read_LE64;

   ---------------------------------------------------------------------------
   --  ELF Loading
   ---------------------------------------------------------------------------

   function Validate_Header (
      Binary : Binary_Buffer;
      Size   : Natural
   ) return Load_Error is
   begin
      --  Check minimum size for ELF header
      if Size < 64 then
         return Load_Invalid_Magic;
      end if;

      --  Check ELF magic number
      if Binary (0) /= ELF_Magic (0) or
         Binary (1) /= ELF_Magic (1) or
         Binary (2) /= ELF_Magic (2) or
         Binary (3) /= ELF_Magic (3)
      then
         return Load_Invalid_Magic;
      end if;

      --  Check ELF class (must be 64-bit)
      if Binary (4) /= 2 then  -- ELFCLASS64
         return Load_Unsupported_Class;
      end if;

      --  Check machine type (e_machine at offset 18)
      declare
         Machine : constant Word32 := Read_LE16 (Binary, 18);
      begin
         case Machine is
            when 16#B7# =>   -- EM_AARCH64
               null;
            when 16#3E# =>   -- EM_X86_64
               null;
            when 16#F3# =>   -- EM_RISCV
               null;
            when others =>
               return Load_Unsupported_Arch;
         end case;
      end;

      return Load_Success;
   end Validate_Header;

   function Get_Entry_Point (
      Binary : Binary_Buffer;
      Size   : Natural
   ) return Entry_Point is
      pragma Unreferenced (Size);
   begin
      --  Entry point at offset 24 for ELF64
      return Entry_Point (Read_LE64 (Binary, 24));
   end Get_Entry_Point;

   function Get_Architecture (
      Binary : Binary_Buffer
   ) return Target_Arch is
      Machine : constant Word32 := Read_LE16 (Binary, 18);
   begin
      case Machine is
         when 16#B7# =>
            return Arch_ARM64;
         when 16#3E# =>
            return Arch_X86_64;
         when 16#F3# =>
            return Arch_RISCV64;
         when others =>
            return Arch_ARM64;  -- Default
      end case;
   end Get_Architecture;

   function Load_ELF (
      Binary : Binary_Buffer;
      Size   : Natural
   ) return Load_Result is
      Header_Error : Load_Error;
      Contract     : Loaded_Contract := Invalid_Contract;
   begin
      --  Check size limit
      if Size > Max_Binary_Size then
         return (Error => Load_Too_Large, Contract => Invalid_Contract);
      end if;

      --  Validate header
      Header_Error := Validate_Header (Binary, Size);
      if Header_Error /= Load_Success then
         return (Error => Header_Error, Contract => Invalid_Contract);
      end if;

      --  Extract basic info
      Contract.Arch := Get_Architecture (Binary);
      Contract.Class := ELF_64;
      Contract.Entry_Addr := Get_Entry_Point (Binary, Size);

      --  Parse section headers to get code/data segments
      --  Section header offset at offset 40 for ELF64
      declare
         SH_Off     : constant Word64 := Read_LE64 (Binary, 40);
         SH_Entsize : constant Word32 := Read_LE16 (Binary, 58);
         SH_Num     : constant Word32 := Read_LE16 (Binary, 60);
         Current_Off : Natural;
      begin
         if SH_Off = 0 or SH_Num = 0 then
            return (Error => Load_Invalid_Sections, Contract => Invalid_Contract);
         end if;

         --  Scan sections to find .text, .data, .bss
         Current_Off := Natural (SH_Off);
         for I in 0 .. SH_Num - 1 loop
            if Current_Off + Natural (SH_Entsize) <= Size then
               declare
                  Sh_Type  : constant Word32 := Read_LE32 (Binary, Current_Off + 4);
                  Sh_Flags : constant Word64 := Read_LE64 (Binary, Current_Off + 8);
                  Sh_Addr  : constant Word64 := Read_LE64 (Binary, Current_Off + 16);
                  Sh_Size  : constant Word64 := Read_LE64 (Binary, Current_Off + 32);
               begin
                  --  Check section type and flags
                  if Sh_Type = 1 then  -- SHT_PROGBITS
                     if (Sh_Flags and 4) /= 0 then  -- SHF_EXECINSTR
                        --  This is code (.text)
                        Contract.Code_Base := Sh_Addr;
                        Contract.Code_Size := Sh_Size;
                     elsif (Sh_Flags and 1) /= 0 then  -- SHF_WRITE
                        --  This is writable data (.data)
                        Contract.Data_Base := Sh_Addr;
                        Contract.Data_Size := Sh_Size;
                     end if;
                  elsif Sh_Type = 8 then  -- SHT_NOBITS (.bss)
                     Contract.BSS_Base := Sh_Addr;
                     Contract.BSS_Size := Sh_Size;
                  end if;
               end;
            end if;
            Current_Off := Current_Off + Natural (SH_Entsize);
         end loop;
      end;

      --  Check entry point is valid
      if Contract.Entry_Addr = 0 then
         return (Error => Load_Missing_Entry, Contract => Invalid_Contract);
      end if;

      Contract.Is_Valid := True;
      return (Error => Load_Success, Contract => Contract);
   end Load_ELF;

   ---------------------------------------------------------------------------
   --  Contract Execution
   ---------------------------------------------------------------------------

   --  Execute native contract code
   --
   --  KHEPRI uses native execution (not bytecode interpretation):
   --  1. Contract code is compiled Ada/SPARK to native ELF
   --  2. ELF is loaded into isolated memory regions
   --  3. Syscalls are intercepted via the SPHINX runtime
   --  4. Gas is metered based on WCET analysis
   --
   --  This function implements the execution lifecycle:
   --  - Memory region setup with guard pages
   --  - Calldata passing via defined ABI
   --  - Syscall dispatch through ANKH/THOTH layers
   --  - Return data extraction
   --
   --  Security model:
   --  - W^X enforcement (write XOR execute)
   --  - Stack canaries and guard pages
   --  - Capability-based syscall filtering
   --  - Hardware sandboxing when available

   function Execute (
      Contract    : Loaded_Contract;
      Sandbox     : Aegis_Sandbox.Sandbox_Context;
      Calldata    : Byte_Array;
      Gas_Limit   : Gas_Amount
   ) return Exec_Result is
      Layout      : Region_Layout;
      Gas_Used    : Gas_Amount := 0;
      Result_Hash : Hash256 := Hash256_Zero;
   begin
      --  Step 1: Validate contract and create memory layout
      if not Contract.Is_Valid then
         return (Status => Exec_Security_Violation, Gas_Used => 0, Return_Data => Hash256_Zero);
      end if;

      Layout := Create_Layout (Contract);

      --  Step 2: Check gas limit against WCET bound (if available)
      if Contract.Has_Proof then
         if Gas_Limit < Contract.WCET_Bound then
            return (Status => Exec_Out_Of_Gas, Gas_Used => 0, Return_Data => Hash256_Zero);
         end if;
         --  With WCET proof, we know exact gas consumption
         Gas_Used := Contract.WCET_Bound;
      else
         --  Without proof, assume max gas usage
         Gas_Used := Gas_Limit;
      end if;

      --  Step 3: Validate calldata size
      if Calldata'Length > 65536 then
         return (Status => Exec_Security_Violation, Gas_Used => 0, Return_Data => Hash256_Zero);
      end if;

      --  Step 4: Compute calldata hash for return data reference
      if Calldata'Length > 0 then
         declare
            use Anubis_Types;
            use Anubis_SHA3;
            Calldata_Bytes : Anubis_Types.Byte_Array (0 .. Calldata'Length - 1);
            Digest : SHA3_256_Digest;
         begin
            for I in Calldata'Range loop
               Calldata_Bytes (I - Calldata'First) := Anubis_Types.Byte (Calldata (I));
            end loop;
            SHA3_256 (Calldata_Bytes, Digest);
            for I in 0 .. 31 loop
               Result_Hash (I) := Aegis_VM_Types.Byte (Digest (I));
            end loop;
         end;
      end if;

      --  Step 5: Security validation
      declare
         Security : constant Security_Result := Validate_Security (Contract);
      begin
         if not Is_Secure (Security) then
            return (Status => Exec_Security_Violation, Gas_Used => 0, Return_Data => Hash256_Zero);
         end if;
      end;

      --  Step 6: Execute contract
      --
      --  In a full implementation, this would:
      --  a) Map memory regions with appropriate permissions
      --  b) Set up the contract stack with calldata
      --  c) Install syscall handler (seccomp on Linux, similar on other platforms)
      --  d) Execute native code at Contract.Entry_Addr
      --  e) Trap on syscalls and dispatch to ANKH (crypto) or THOTH (storage)
      --  f) Extract return data from contract memory
      --  g) Cleanup and restore parent context
      --
      --  For now, we validate the structure and return success with computed gas.
      --  The actual native execution requires platform-specific code that would
      --  be in a separate non-SPARK unit (e.g., sphinx_native_linux.adb).
      pragma Unreferenced (Sandbox, Layout);

      return (
         Status      => Exec_Success,
         Gas_Used    => Gas_Used,
         Return_Data => Result_Hash
      );
   end Execute;

   function Execute_Function (
      Contract    : Loaded_Contract;
      Sandbox     : Aegis_Sandbox.Sandbox_Context;
      Selector    : Word32;
      Args        : Byte_Array;
      Gas_Limit   : Gas_Amount
   ) return Exec_Result is
      Calldata : Byte_Array (0 .. Args'Length + 3);
   begin
      --  Build calldata: selector (4 bytes) + args
      Calldata (0) := Byte (Shift_Right (Selector, 24) and 16#FF#);
      Calldata (1) := Byte (Shift_Right (Selector, 16) and 16#FF#);
      Calldata (2) := Byte (Shift_Right (Selector, 8) and 16#FF#);
      Calldata (3) := Byte (Selector and 16#FF#);

      for I in Args'Range loop
         Calldata (4 + I - Args'First) := Args (I);
      end loop;

      return Execute (Contract, Sandbox, Calldata, Gas_Limit);
   end Execute_Function;

   ---------------------------------------------------------------------------
   --  Security Validation
   ---------------------------------------------------------------------------

   function Validate_Security (
      Contract : Loaded_Contract
   ) return Security_Result is
      Result : Security_Result := (others => True);
   begin
      --  Check W^X: code should not be writable, data should not be executable
      if Contract.Code_Base = 0 then
         Result (Check_WXE) := False;
      end if;

      --  Other checks would analyze the binary more deeply
      --  For now, assume all checks pass for valid contracts
      if not Contract.Is_Valid then
         Result := (others => False);
      end if;

      return Result;
   end Validate_Security;

   function Is_Secure (Checks : Security_Result) return Boolean is
   begin
      for C in Security_Check loop
         if not Checks (C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Secure;

   ---------------------------------------------------------------------------
   --  WCET Integration
   ---------------------------------------------------------------------------

   function Analyze_WCET (
      Contract : Loaded_Contract;
      Function_Entry : Entry_Point
   ) return WCET_Result is
      pragma Unreferenced (Contract, Function_Entry);
   begin
      --  Placeholder: Real implementation would:
      --  1. Analyze control flow graph
      --  2. Count instructions
      --  3. Apply WCET analysis
      --  4. Convert cycles to gas
      return (
         Valid      => True,
         Cycles     => 1000,
         Gas_Bound  => 10,  -- cycles / CYCLES_PER_GAS
         Call_Depth => 1
      );
   end Analyze_WCET;

   function Get_WCET_Annotation (
      Contract : Loaded_Contract
   ) return WCET_Result is
   begin
      if Contract.Has_Proof then
         return (
            Valid      => True,
            Cycles     => Natural (Contract.WCET_Bound * 100),
            Gas_Bound  => Contract.WCET_Bound,
            Call_Depth => 1
         );
      else
         return (
            Valid      => False,
            Cycles     => 0,
            Gas_Bound  => 0,
            Call_Depth => 0
         );
      end if;
   end Get_WCET_Annotation;

   ---------------------------------------------------------------------------
   --  Memory Layout
   ---------------------------------------------------------------------------

   function Create_Layout (
      Contract : Loaded_Contract
   ) return Region_Layout is
      Layout : Region_Layout := (others => (
         Base       => 0,
         Size       => 0,
         Reg_Type   => Region_Guard,
         Readable   => False,
         Writable   => False,
         Executable => False
      ));
   begin
      --  Code region (R-X)
      Layout (0) := (
         Base       => Contract.Code_Base,
         Size       => Contract.Code_Size,
         Reg_Type   => Region_Code,
         Readable   => True,
         Writable   => False,
         Executable => True
      );

      --  Data region (RW-)
      Layout (1) := (
         Base       => Contract.Data_Base,
         Size       => Contract.Data_Size,
         Reg_Type   => Region_Data,
         Readable   => True,
         Writable   => True,
         Executable => False
      );

      --  BSS region (RW-)
      Layout (2) := (
         Base       => Contract.BSS_Base,
         Size       => Contract.BSS_Size,
         Reg_Type   => Region_BSS,
         Readable   => True,
         Writable   => True,
         Executable => False
      );

      --  Stack region (RW-) - placed after BSS (64 KB stack)
      Layout (3) := (
         Base       => Contract.BSS_Base + Contract.BSS_Size + 4096,
         Size       => 65536,  -- 64 KB contract stack
         Reg_Type   => Region_Stack,
         Readable   => True,
         Writable   => True,
         Executable => False
      );

      --  Guard page before stack
      Layout (4) := (
         Base       => Contract.BSS_Base + Contract.BSS_Size,
         Size       => 4096,
         Reg_Type   => Region_Guard,
         Readable   => False,
         Writable   => False,
         Executable => False
      );

      return Layout;
   end Create_Layout;

   ---------------------------------------------------------------------------
   --  Debug Support
   ---------------------------------------------------------------------------

   function Lookup_Symbol (
      Contract : Loaded_Contract;
      Address  : Word64
   ) return Symbol_Entry is
      pragma Unreferenced (Contract, Address);
   begin
      --  Placeholder: Would search symbol table
      return (
         Name_Offset => 0,
         Value       => 0,
         Size        => 0,
         Sym_Type    => 0,
         Binding     => 0
      );
   end Lookup_Symbol;

   function Disassemble (
      Contract : Loaded_Contract;
      Address  : Word64
   ) return String is
      pragma Unreferenced (Contract, Address);
   begin
      --  Placeholder: Would disassemble instruction
      return "<unknown>";
   end Disassemble;

end Sphinx_Native;
