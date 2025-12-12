pragma SPARK_Mode (Off);

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Anubis_Node_Types;       use Anubis_Node_Types;
with Node_Contract_Registry;  use Node_Contract_Registry;
with Node_Contract_Executor;  use Node_Contract_Executor;
with Khepri_Types;
with Quantum_Vault;

package body Local_Executor is

   use Ada.Characters.Handling;
   use Ada.Strings;
   use Ada.Strings.Unbounded;

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Initialized : Boolean := False;

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
      pragma Unreferenced (Args, Gas_Limit, Value);

      Caller : Khepri_Types.Address := From_Address;

      Success : Boolean := False;
      Err     : Quantum_Vault.Error_Code := Quantum_Vault.Error_Internal;
   begin
      if Equal_CI (Entry_Point, "Initialize") then
         declare
            Signers : Quantum_Vault.Address_Array (0 .. 0);
            Weights : Quantum_Vault.Natural_Array (0 .. 0);
         begin
            Signers (0) := Caller;
            Weights (0) := 1;

            Quantum_Vault.Initialize
              (Signers   => Signers,
               Weights   => Weights,
               Threshold => 1,
               Success   => Success,
               Error     => Err);
         end;

      elsif Equal_CI (Entry_Point, "Get_Config") then
         declare
            Cfg : constant Quantum_Vault.Vault_Config :=
              Quantum_Vault.Get_Config;
            pragma Unreferenced (Cfg);
         begin
            Success := True;
            Err := Quantum_Vault.Error_None;
         end;

      else
         Success := False;
         Err := Quantum_Vault.Error_Invalid_Transaction;
      end if;

      Result.Success  := Success;
      Result.Gas_Used := 0;

      if Success then
         Result.Error      := Null_Unbounded_String;
      else
         Result.Error :=
           To_Unbounded_String (Quantum_Vault.Error_Code'Image (Err));
      end if;

      Result.Return_Hex := Null_Unbounded_String;
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

   procedure Init_If_Needed is
      Dummy_Code : Node_Code_Buffer := (others => 0);
      Manifest   : Node_Contract_Manifest;
      Block_Zero : constant U256 := (Limbs => (0, 0, 0, 0));
      Index      : Stored_Contract_Index;
      Ok         : Boolean;

      procedure Register_Local (Name : String) is
      begin
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
            Code      => Dummy_Code,
            Code_Size => 1,
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

      Node_Contract_Registry.Initialize (Reg);
      Node_Contract_Executor.Initialize (Exec);

      --  Register known built-in contracts by manifest name
      Register_Local ("HelloCounter");
      Register_Local ("SimpleToken");
      Register_Local ("SimpleVault");
      Register_Local ("QuantumDID");
      Register_Local ("Staking");
      Register_Local ("Governance");

      Initialized := True;
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

end Local_Executor;
