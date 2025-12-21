--  SPHINX RV32 Runtime: Implementation

pragma SPARK_Mode (Off);

with Sphinx_RV32_Loader; use Sphinx_RV32_Loader;
with Sphinx_RV32_Execute; use Sphinx_RV32_Execute;
with Sphinx_RV32_Types; use Sphinx_RV32_Types;

package body Sphinx_RV32_Runtime is

   ---------------------------------------------------------------------------
   --  Helper: Copy from pointer-based context to embedded context
   ---------------------------------------------------------------------------

   procedure Sync_Context_To_Embedded (
      Ctx   : in     Interpreter_Context_Ptr;
      State : in Out Interpreter_State
   ) is
   begin
      if Ctx = null then
         State.Context := Empty_Context;
         return;
      end if;

      --  Copy fields from pointer-based context to embedded context
      State.Context.Caller := Ctx.Caller;
      State.Context.Self := Ctx.Self;
      State.Context.Origin := Ctx.Origin;
      State.Context.Call_Value := Ctx.Call_Value;
      State.Context.Block_Number := Ctx.Block_Number;
      State.Context.Timestamp := Ctx.Timestamp;
      State.Context.Chain_ID := Ctx.Chain_ID;
      State.Context.Gas_Price := Ctx.Gas_Price;
      State.Context.Is_Static := Ctx.Is_Static;
      State.Context.Is_Valid := Ctx.Is_Valid;

      --  Copy calldata (up to embedded buffer limit)
      State.Context.Calldata_Len := Natural'Min (
         Ctx.Calldata_Length,
         Max_Calldata_Size
      );
      for I in 0 .. State.Context.Calldata_Len - 1 loop
         State.Context.Calldata (I) := Ctx.Calldata (I);
      end loop;

      --  Copy return data
      State.Context.Return_Data_Len := Natural'Min (
         Ctx.Return_Data_Length,
         Max_Return_Data_Size
      );
      for I in 0 .. State.Context.Return_Data_Len - 1 loop
         State.Context.Return_Data (I) := Ctx.Return_Data (I);
      end loop;
   end Sync_Context_To_Embedded;

   ---------------------------------------------------------------------------
   --  Test Storage Implementation (In-Memory)
   ---------------------------------------------------------------------------

   --  Simple in-memory storage for testing (256 slots)
   Max_Storage_Slots : constant := 256;
   type Storage_Entry is record
      Key   : Storage_Key_Bytes;
      Value : Storage_Value_Bytes;
      Used  : Boolean;
   end record;

   type Storage_Array is array (0 .. Max_Storage_Slots - 1) of Storage_Entry;

   Test_Storage : Storage_Array := (others => (
      Key   => (others => 0),
      Value => (others => 0),
      Used  => False
   ));

   procedure Test_Storage_Load (
      Key     : in     Storage_Key_Bytes;
      Value   : out    Storage_Value_Bytes;
      Success : out    Boolean
   ) is
   begin
      Value := (others => 0);
      Success := True;

      --  Search for key
      for I in Test_Storage'Range loop
         if Test_Storage (I).Used then
            declare
               Match : Boolean := True;
            begin
               for J in Key'Range loop
                  if Test_Storage (I).Key (J) /= Key (J) then
                     Match := False;
                     exit;
                  end if;
               end loop;

               if Match then
                  Value := Test_Storage (I).Value;
                  return;
               end if;
            end;
         end if;
      end loop;

      --  Key not found, return zeros (success with default value)
   end Test_Storage_Load;

   procedure Test_Storage_Store (
      Key     : in     Storage_Key_Bytes;
      Value   : in     Storage_Value_Bytes;
      Success : out    Boolean
   ) is
   begin
      Success := False;

      --  Search for existing key or empty slot
      for I in Test_Storage'Range loop
         if Test_Storage (I).Used then
            declare
               Match : Boolean := True;
            begin
               for J in Key'Range loop
                  if Test_Storage (I).Key (J) /= Key (J) then
                     Match := False;
                     exit;
                  end if;
               end loop;

               if Match then
                  --  Update existing entry
                  Test_Storage (I).Value := Value;
                  Success := True;
                  return;
               end if;
            end;
         else
            --  Found empty slot
            Test_Storage (I).Key := Key;
            Test_Storage (I).Value := Value;
            Test_Storage (I).Used := True;
            Success := True;
            return;
         end if;
      end loop;

      --  No space available
      Success := False;
   end Test_Storage_Store;

   ---------------------------------------------------------------------------
   --  Storage Callback Setup
   ---------------------------------------------------------------------------

   function Create_Test_Storage_Callbacks return Storage_Callback_State is
   begin
      return (
         Target_Address => (others => 0),
         Load_Callback  => Test_Storage_Load'Address,
         Store_Callback => Test_Storage_Store'Address
      );
   end Create_Test_Storage_Callbacks;

   ---------------------------------------------------------------------------
   --  Contract Execution
   ---------------------------------------------------------------------------

   function Execute_Contract (
      ELF_Binary : Byte_Array;
      Calldata   : Byte_Array;
      Caller     : Contract_Address;
      Self       : Contract_Address;
      Call_Value : U256;
      Gas_Limit  : Gas_Amount;
      Storage    : Storage_Callback_State;
      Max_Steps  : Natural := Default_Max_Steps
   ) return Contract_Result
   is
      Load_Result : Sphinx_RV32_Loader.Load_Result;
      Ctx         : Interpreter_Context_Ptr;
      Exec_Result : Interpreter_Result;
      Result      : Contract_Result := Failed_Result;
   begin
      --  Check minimum ELF size
      if ELF_Binary'Length < ELF32_Header_Size then
         return Failed_Result;
      end if;

      --  Load ELF into interpreter state
      Load_ELF (
         ELF_Data  => ELF_Binary,
         State     => Last_State,
         Gas_Limit => Gas_Limit,
         Result    => Load_Result
      );

      if Load_Result.Status /= Load_Success then
         Result.Status := Status_Invalid_ELF;
         return Result;
      end if;

      --  Create and initialize context
      Ctx := Create_Context;
      if Ctx = null then
         Result.Status := Status_Trap;
         Result.Trap := Trap_Syscall_Error;
         return Result;
      end if;

      Initialize_Context (
         Ctx          => Ctx,
         Caller       => Caller,
         Self         => Self,
         Origin       => Caller,  -- For direct calls, origin = caller
         Call_Value   => Call_Value,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_Zero,
         Gas_Price    => U256_Zero,
         Is_Static    => False
      );

      --  Set up calldata
      if Calldata'Length > 0 then
         Set_Calldata (Ctx, Calldata);
      end if;

      --  Set up storage callbacks
      Ctx.Storage_Load_Callback := Storage.Load_Callback;
      Ctx.Storage_Store_Callback := Storage.Store_Callback;

      --  Sync context to embedded context in interpreter state
      Sync_Context_To_Embedded (Ctx, Last_State);
      Last_Context := Ctx;

      --  Run the interpreter
      Execute_Contract (Last_State, Max_Steps, Exec_Result);

      --  Convert result
      Result.Gas_Used := Exec_Result.Gas_Used;
      Result.Return_Data := Exec_Result.Return_Data;
      Result.Return_Size := Exec_Result.Return_Length;
      Result.Trap := Exec_Result.Trap;
      Result.Exit_Code := Integer_32 (Last_State.CPU.Regs (A0) and 16#FFFF_FFFF#);

      case Exec_Result.Trap is
         when Trap_Return =>
            Result.Status := Status_Success;
         when Trap_Revert =>
            Result.Status := Status_Revert;
         when Trap_Out_Of_Gas =>
            Result.Status := Status_Out_Of_Gas;
         when Trap_Halt =>
            if Exec_Result.Success then
               Result.Status := Status_Success;
            else
               Result.Status := Status_Trap;
            end if;
         when others =>
            Result.Status := Status_Trap;
      end case;

      return Result;
   end Execute_Contract;

   ---------------------------------------------------------------------------
   --  Simplified Execution
   ---------------------------------------------------------------------------

   function Execute_Contract_Simple (
      ELF_Binary : Byte_Array;
      Calldata   : Byte_Array;
      Gas_Limit  : Gas_Amount
   ) return Contract_Result
   is
      Storage : constant Storage_Callback_State := Create_Test_Storage_Callbacks;
   begin
      return Execute_Contract (
         ELF_Binary => ELF_Binary,
         Calldata   => Calldata,
         Caller     => (others => 0),
         Self       => (others => 0),
         Call_Value => U256_Zero,
         Gas_Limit  => Gas_Limit,
         Storage    => Storage
      );
   end Execute_Contract_Simple;

   ---------------------------------------------------------------------------
   --  State Access
   ---------------------------------------------------------------------------

   function Get_Last_State return Interpreter_State is
   begin
      return Last_State;
   end Get_Last_State;

   function Get_Last_Context return Interpreter_Context_Ptr is
   begin
      return Last_Context;
   end Get_Last_Context;

end Sphinx_RV32_Runtime;
