pragma SPARK_Mode (Off);

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Directories;

with Anubis_Node_Types;       use Anubis_Node_Types;
with Node_Contract_Registry;  use Node_Contract_Registry;
with Node_Contract_Executor;  use Node_Contract_Executor;
with State_Persistence;
with Khepri_State;
with Khepri_Types;
pragma Unreferenced (Khepri_Types);

package body Local_Executor is

   use Ada.Characters.Handling;
   use Ada.Strings;
   use Ada.Strings.Unbounded;

   ---------------------------------------------------------------------------
   --  State Directory Management
   ---------------------------------------------------------------------------

   Default_State_Dir : constant String := ".anubisvm";
   Registry_Filename : constant String := "registry.anubis";
   Storage_Filename  : constant String := "storage.anubis";

   function Get_Home_Dir return String is
   begin
      if Ada.Environment_Variables.Exists ("HOME") then
         return Ada.Environment_Variables.Value ("HOME");
      elsif Ada.Environment_Variables.Exists ("USERPROFILE") then
         return Ada.Environment_Variables.Value ("USERPROFILE");
      else
         return ".";
      end if;
   end Get_Home_Dir;

   function Get_State_Dir return String is
   begin
      --  Check for ANUBISVM_STATE environment variable first
      if Ada.Environment_Variables.Exists ("ANUBISVM_STATE") then
         return Ada.Environment_Variables.Value ("ANUBISVM_STATE");
      end if;
      --  Default to ~/.anubisvm/state
      return Get_Home_Dir & "/" & Default_State_Dir & "/state";
   end Get_State_Dir;

   function Get_Registry_Path return String is
   begin
      return Get_State_Dir & "/" & Registry_Filename;
   end Get_Registry_Path;

   function Get_Storage_Path return String is
   begin
      return Get_State_Dir & "/" & Storage_Filename;
   end Get_Storage_Path;

   procedure Ensure_State_Directory is
      State_Dir : constant String := Get_State_Dir;
   begin
      if not Ada.Directories.Exists (State_Dir) then
         Ada.Directories.Create_Path (State_Dir);
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Warning: Could not create state directory: " & State_Dir);
   end Ensure_State_Directory;

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Initialized : Boolean := False;
   State_Dirty : Boolean := False;  --  Track if state needs saving

   Exec  : Executor_State;
   Reg   : Registry_State;

   --  Mapping from logical contract name to stored index
   type Contract_Entry is record
      Name  : Unbounded_String;
      Index : Stored_Contract_Index;
   end record;

   Max_Local_Contracts : constant := 6;
   subtype Local_Index is Natural range 0 .. Max_Local_Contracts - 1;

   Local_Contracts : array (Local_Index) of Contract_Entry;
   Local_Count     : Natural := 0;

   --  Forward declaration for case-insensitive comparison helper
   function Equal_CI (A, B : String) return Boolean;

   ---------------------------------------------------------------------------
   --  Quantum Vault Helper (TE-only, no node)
   ---------------------------------------------------------------------------

   procedure Execute_Quantum_Vault
     (From_Address  : Contract_Address;
      Entry_Point   : String;
      Args          : Arg_Array;
      Gas_Limit     : Gas_Amount;
      Value         : U256;
      Result        : out Exec_Result) is
      pragma Unreferenced (From_Address, Args, Gas_Limit, Value);
   begin
      --  Quantum Vault CVM now uses the new CVM interface
      --  This local executor stub is for backwards compatibility
      if Equal_CI (Entry_Point, "Initialize") or else
         Equal_CI (Entry_Point, "Deposit") or else
         Equal_CI (Entry_Point, "Withdraw") or else
         Equal_CI (Entry_Point, "GetBalance") or else
         Equal_CI (Entry_Point, "GetStats")
      then
         Result.Success := True;
         Result.Gas_Used := 0;
         Result.Error := Null_Unbounded_String;
         Result.Return_Hex := Null_Unbounded_String;
      else
         Result.Success := False;
         Result.Gas_Used := 0;
         Result.Error := To_Unbounded_String ("Use CVM interface");
         Result.Return_Hex := Null_Unbounded_String;
      end if;
   end Execute_Quantum_Vault;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   function To_Hex (Data : Return_Buffer; Size : Natural) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Len       : constant Natural := Size * 2;
      Result    : String (1 .. Len);
      Pos       : Natural := 1;
   begin
      for I in 0 .. Size - 1 loop
         declare
            B : constant Byte := Data (Return_Index (I));
            Hi : constant Natural := Integer (B) / 16;
            Lo : constant Natural := Integer (B) mod 16;
         begin
            Result (Pos)     := Hex_Chars (Hi + 1);
            Result (Pos + 1) := Hex_Chars (Lo + 1);
            Pos := Pos + 2;
         end;
      end loop;
      return Result;
   end To_Hex;

   function Equal_CI (A, B : String) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;
      end if;
      for I in 1 .. A'Length loop
         if To_Lower (A (A'First + I - 1)) /=
            To_Lower (B (B'First + I - 1))
         then
            return False;
         end if;
      end loop;
      return True;
   end Equal_CI;

   function Hex_Digit (C : Character) return Integer is
   begin
      if C in '0' .. '9' then
         return Character'Pos (C) - Character'Pos ('0');
      elsif C in 'a' .. 'f' then
         return 10 + Character'Pos (C) - Character'Pos ('a');
      elsif C in 'A' .. 'F' then
         return 10 + Character'Pos (C) - Character'Pos ('A');
      else
         return -1;
      end if;
   end Hex_Digit;

   --  Parse a single 32-byte (64 hex chars) argument into Args_Buffer at Offset
   procedure Encode_Single_U256_Arg
     (Arg       : in  String;
      Args_Out  : in out Args_Buffer;
      Offset    : in  Natural) is
      Clean : constant String := (if Arg'Length >= 2
                                  and then Arg (Arg'First) = '0'
                                  and then (Arg (Arg'First + 1) = 'x'
                                            or else Arg (Arg'First + 1) = 'X')
                                  then Arg (Arg'First + 2 .. Arg'Last)
                                  else Arg);
      Hex_Len : Natural := Clean'Length;
      Ptr     : Natural := Hex_Len;
      Byte_Pos : Natural := 31;
   begin
      --  Fill from right to left so shorter hex strings are right-aligned
      while Ptr > 0 and Byte_Pos < 32 loop
         declare
            Lo_Char : constant Character := Clean (Clean'First + Ptr - 1);
            Lo      : constant Integer   := Hex_Digit (Lo_Char);
            Hi      : Integer := 0;
         begin
            if Lo < 0 then
               exit;
            end if;
            Ptr := Ptr - 1;
            if Ptr > 0 then
               Hi := Hex_Digit (Clean (Clean'First + Ptr - 1));
               if Hi < 0 then
                  Hi := 0;
               else
                  Ptr := Ptr - 1;
               end if;
            end if;
            Args_Out (Args_Buffer'First + Offset + Byte_Pos) :=
              Byte (Hi * 16 + Lo);
            if Byte_Pos = 0 then
               exit;
            end if;
            Byte_Pos := Byte_Pos - 1;
         end;
      end loop;
   end Encode_Single_U256_Arg;

   ---------------------------------------------------------------------------
   --  State Persistence Operations
   ---------------------------------------------------------------------------

   procedure Load_State_From_Disk is
      Registry_Path : constant String := Get_Registry_Path;
      Storage_Path  : constant String := Get_Storage_Path;
      Path_Buffer   : State_Persistence.Path_String := (others => ' ');
      Load_Result   : State_Persistence.Persist_Result;
      Storage_Result : Khepri_State.Persist_Result;
   begin
      --  Check if registry file exists
      if not Ada.Directories.Exists (Registry_Path) then
         Ada.Text_IO.Put_Line ("  [State] No existing state found, starting fresh");
         return;
      end if;

      --  Prepare path buffer
      if Registry_Path'Length <= State_Persistence.Max_Path_Len then
         Path_Buffer (1 .. Registry_Path'Length) := Registry_Path;

         State_Persistence.Load_Registry (
            File_Path => Path_Buffer,
            Path_Len  => Registry_Path'Length,
            Registry  => Reg,
            Result    => Load_Result
         );

         if Load_Result.Success then
            Ada.Text_IO.Put_Line ("  [State] Loaded " &
               Natural'Image (Node_Contract_Registry.Count (Reg)) &
               " contracts from " & Registry_Path);
         else
            Ada.Text_IO.Put_Line ("  [State] Warning: Failed to load state (" &
               State_Persistence.Persist_Error'Image (Load_Result.Error) & ")");
            --  Re-initialize to clean state
            Node_Contract_Registry.Initialize (Reg);
         end if;
      else
         Ada.Text_IO.Put_Line ("  [State] Warning: Path too long, starting fresh");
      end if;

      --  Load contract storage state if file exists
      if Ada.Directories.Exists (Storage_Path) then
         Khepri_State.Load_Storage_State (Storage_Path, Storage_Result);
         case Storage_Result is
            when Khepri_State.Persist_OK =>
               Ada.Text_IO.Put_Line ("  [State] Loaded storage state (" &
                  Natural'Image (Khepri_State.Get_Storage_Count) &
                  " entries) from " & Storage_Path);
            when others =>
               Ada.Text_IO.Put_Line ("  [State] Warning: Failed to load storage (" &
                  Khepri_State.Persist_Result'Image (Storage_Result) & ")");
         end case;
      end if;
   end Load_State_From_Disk;

   procedure Save_State_To_Disk is
      Registry_Path : constant String := Get_Registry_Path;
      Storage_Path  : constant String := Get_Storage_Path;
      Path_Buffer   : State_Persistence.Path_String := (others => ' ');
      Save_Result   : State_Persistence.Persist_Result;
      Storage_Result : Khepri_State.Persist_Result;
   begin
      if not State_Dirty then
         return;  --  Nothing changed, skip save
      end if;

      Ensure_State_Directory;

      --  Save contract registry
      if Registry_Path'Length <= State_Persistence.Max_Path_Len then
         Path_Buffer (1 .. Registry_Path'Length) := Registry_Path;

         State_Persistence.Save_Registry (
            Registry  => Reg,
            File_Path => Path_Buffer,
            Path_Len  => Registry_Path'Length,
            Result    => Save_Result
         );

         if Save_Result.Success then
            Ada.Text_IO.Put_Line ("  [State] Saved registry to " & Registry_Path &
               " (" & Natural'Image (Save_Result.Bytes_Written) & " bytes)");
         else
            Ada.Text_IO.Put_Line ("  [State] Warning: Failed to save registry (" &
               State_Persistence.Persist_Error'Image (Save_Result.Error) & ")");
         end if;
      end if;

      --  Save contract storage state
      Khepri_State.Save_Storage_State (Storage_Path, Storage_Result);
      case Storage_Result is
         when Khepri_State.Persist_OK =>
            Ada.Text_IO.Put_Line ("  [State] Saved storage to " & Storage_Path &
               " (" & Natural'Image (Khepri_State.Get_Storage_Count) & " entries)");
         when others =>
            Ada.Text_IO.Put_Line ("  [State] Warning: Failed to save storage (" &
               Khepri_State.Persist_Result'Image (Storage_Result) & ")");
      end case;

      State_Dirty := False;
   end Save_State_To_Disk;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_If_Needed is
      Contract_Code : Node_Code_Buffer := (others => 0);
      Manifest   : Node_Contract_Manifest;
      Block_Zero : constant U256 := (Limbs => (0, 0, 0, 0));
      Index      : Stored_Contract_Index;
      Ok         : Boolean;

      procedure Register_Local (Name : String) is
      begin
         --  Create unique code for each contract by embedding the name
         --  This ensures unique code hashes for contract dispatch
         Contract_Code := (others => 0);
         for I in 1 .. Natural'Min (Name'Length, Contract_Code'Length) loop
            Contract_Code (Contract_Code'First + I - 1) := Byte (Character'Pos (Name (Name'First + I - 1)));
         end loop;

         Manifest.Name      := (others => ' ');
         Manifest.Name_Len  := Name'Length;
         Manifest.Version_Major := 1;
         Manifest.Version_Minor := 0;
         Manifest.Version_Patch := 0;
         Manifest.Cert      := Cert_None;
         for I in 1 .. Name'Length loop
            Manifest.Name (I) := Name (Name'First + I - 1);
         end loop;

         Register_Contract
           (Registry  => Reg,
            Manifest  => Manifest,
            Code      => Contract_Code,
            Code_Size => Name'Length,
            Block_Num => Block_Zero,
            Index     => Index,
            Success   => Ok);

         if Ok and then Local_Count < Max_Local_Contracts then
            Local_Contracts (Local_Index (Local_Count)).Name  :=
              To_Unbounded_String (Name);
            Local_Contracts (Local_Index (Local_Count)).Index := Index;
            Local_Count := Local_Count + 1;
         end if;
      end Register_Local;

   begin
      if Initialized then
         return;
      end if;

      Ada.Text_IO.Put_Line ("  Initializing AnubisVM Local Executor...");

      --  Initialize base state
      Node_Contract_Registry.Initialize (Reg);
      Node_Contract_Executor.Initialize (Exec);

      --  Try to load existing state from disk
      Load_State_From_Disk;

      --  Register known built-in contracts by manifest name
      --  These will be added if not already present from loaded state
      if Node_Contract_Registry.Count (Reg) = 0 then
         Ada.Text_IO.Put_Line ("  Registering built-in contracts...");
         Register_Local ("HelloCounter");
         Register_Local ("SimpleToken");
         Register_Local ("SimpleVault");
         Register_Local ("QuantumDID");
         Register_Local ("Staking");
         Register_Local ("Governance");
         State_Dirty := True;  --  Mark as needing save
      else
         --  Populate Local_Contracts from loaded registry
         --  This maps contract names to their stored indices
         for I in 0 .. Node_Contract_Registry.Count (Reg) - 1 loop
            exit when Local_Count >= Max_Local_Contracts;
            declare
               Stored_Idx : constant Stored_Contract_Index := Stored_Contract_Index (I);
               Name_Len : constant Natural := Reg.Contracts (Stored_Idx).Manifest.Name_Len;
               Name_Str : String (1 .. Name_Len);
            begin
               for J in 1 .. Name_Len loop
                  Name_Str (J) := Reg.Contracts (Stored_Idx).Manifest.Name (J);
               end loop;
               Local_Contracts (Local_Index (Local_Count)).Name := To_Unbounded_String (Name_Str);
               Local_Contracts (Local_Index (Local_Count)).Index := Stored_Idx;
               Local_Count := Local_Count + 1;
            end;
         end loop;
      end if;

      Initialized := True;
      Ada.Text_IO.Put_Line ("  Ready.");
   end Init_If_Needed;

   function Find_Contract_Index (Name : String) return Stored_Contract_Index is
   begin
      for I in 0 .. Local_Count - 1 loop
         declare
            Candidate : constant Contract_Entry :=
              Local_Contracts (Local_Index (I));
         begin
            if Equal_CI (To_String (Candidate.Name), Name) then
               return Candidate.Index;
            end if;
         end;
      end loop;
      return 0;
   end Find_Contract_Index;

   ---------------------------------------------------------------------------
   --  Execute_Local
   ---------------------------------------------------------------------------

   procedure Execute_Local
     (From_Address  : Contract_Address;
      Contract_Name : String;
      Entry_Point   : String;
      Args          : Arg_Array;
      Gas_Limit     : Gas_Amount;
      Value         : U256;
      Result        : out Exec_Result) is

      Contract_Idx : Stored_Contract_Index;
      EP_Name      : Entry_Name := (others => ' ');
      EP_Name_Len  : Natural := Entry_Point'Length;
      Args_Storage : Args_Buffer := (others => 0);
      Args_Size    : Natural := 0;
      Invoke       : Invoke_Result;
   begin
      Init_If_Needed;

      --  TE-only Quantum Vault path (bypasses node executor)
      if Equal_CI (Contract_Name, "QuantumVault")
        or else Equal_CI (Contract_Name, "Quantum_Vault")
      then
         Execute_Quantum_Vault
           (From_Address => From_Address,
            Entry_Point  => Entry_Point,
            Args         => Args,
            Gas_Limit    => Gas_Limit,
            Value        => Value,
            Result       => Result);
         return;
      end if;

      --  Locate contract slot
      Contract_Idx := Find_Contract_Index (Contract_Name);

      --  Prepare entry point name
      if EP_Name_Len > Max_Entry_Name_Length then
         EP_Name_Len := Max_Entry_Name_Length;
      end if;
      for I in 1 .. EP_Name_Len loop
         EP_Name (I) := Entry_Point (Entry_Point'First + I - 1);
      end loop;

      --  Encode a single hex U256 argument if present, otherwise no args
      if Args'Length = 0 then
         Args_Size := 0;
      else
         --  Encode each argument as a 32-byte big-endian word
         Args_Size := Args'Length * 32;
         if Args_Size > Args_Storage'Length then
            Args_Size := Args_Storage'Length;
         end if;

         declare
            Offset : Natural := 0;
         begin
            for I in Args'Range loop
               exit when Offset + 32 > Args_Storage'Length;
               Encode_Single_U256_Arg
                 (To_String (Args (I)), Args_Storage, Offset);
               Offset := Offset + 32;
            end loop;
         end;
      end if;

      --  Execute via built-in executor
      Node_Contract_Executor.Execute
        (Exec         => Exec,
         Registry     => Reg,
         Contract_Idx => Contract_Idx,
         From         => From_Address,
         EP_Name      => EP_Name,
         EP_Name_Len  => EP_Name_Len,
         Args         => Args_Storage,
         Args_Size    => Args_Size,
         Gas_Limit    => Gas_Limit,
         Value        => Value,
         Is_View      => False,
         Ret          => Invoke);

      if Invoke.Success then
         Result.Success    := True;
         Result.Gas_Used   := Invoke.Gas_Used;
         Result.Error      := Null_Unbounded_String;
         if Invoke.Return_Size > 0 then
            Result.Return_Hex :=
              To_Unbounded_String (To_Hex (Invoke.Return_Data, Invoke.Return_Size));
         else
            Result.Return_Hex := Null_Unbounded_String;
         end if;

         --  Mark state as dirty and save to disk after successful write operations
         State_Dirty := True;
         Save_State_To_Disk;
      else
         Result.Success  := False;
         Result.Gas_Used := Invoke.Gas_Used;

         if Invoke.Error_Msg_Len > 0 then
            Result.Error := To_Unbounded_String (
              Invoke.Error_Msg (1 .. Invoke.Error_Msg_Len));
         else
            Result.Error := To_Unbounded_String ("Execution failed");
         end if;

         Result.Return_Hex := Null_Unbounded_String;
      end if;
   end Execute_Local;

   ---------------------------------------------------------------------------
   --  Public State Management
   ---------------------------------------------------------------------------

   procedure Save_State is
   begin
      State_Dirty := True;
      Save_State_To_Disk;
   end Save_State;

   procedure Reset_State is
   begin
      Initialized := False;
      State_Dirty := False;
      Local_Count := 0;
   end Reset_State;

   function Get_State_Directory return String is
   begin
      return Get_State_Dir;
   end Get_State_Directory;

end Local_Executor;
