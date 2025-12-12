pragma SPARK_Mode (On);

with Anubis_SHA3; use Anubis_SHA3;

package body CVM_Dispatch with
   SPARK_Mode => On
is

   --  Push state onto call stack
   procedure Push_State (
      DState    : in Out Dispatch_State;
      CVM_Index : Registry_Index;
      Context   : Call_Context;
      State     : State_Array
   ) is
   begin
      DState.Stack (DState.Depth) := (
         CVM_Index   => CVM_Index,
         Context     => Context,
         Saved_State => State
      );
      DState.Depth := DState.Depth + 1;
   end Push_State;

   --  Pop and rollback state
   procedure Pop_And_Rollback (
      DState   : in Out Dispatch_State;
      Registry : in Out Registry_Array
   ) is
      Entry_Ref : Call_Stack_Entry;
   begin
      DState.Depth := DState.Depth - 1;
      Entry_Ref := DState.Stack (DState.Depth);

      --  Restore saved state
      Set_State (Registry, Entry_Ref.CVM_Index, Entry_Ref.Saved_State);
   end Pop_And_Rollback;

   --  Pop and commit (state already updated)
   procedure Pop_And_Commit (
      DState : in Out Dispatch_State
   ) is
   begin
      DState.Depth := DState.Depth - 1;
   end Pop_And_Commit;

   --  Validate call context
   function Validate_Context (
      Registry : Registry_Array;
      Context  : Call_Context
   ) return Exec_Status is
      CVM_Index : Registry_Index;
      Found : Boolean;
      Desc : CVM_Descriptor;
      Entry_Index : Natural;
      Entry_Found : Boolean;
   begin
      --  Look up target CVM
      Lookup_By_Address (Registry, Context.Target, CVM_Index, Found);

      if not Found then
         return Unknown_Method;  -- CVM not found
      end if;

      --  Check if CVM is active
      if not Is_Active (Registry, CVM_Index) then
         return Internal_Error;  -- CVM not active
      end if;

      --  Get descriptor
      Get_Descriptor (Registry, CVM_Index, Desc);

      --  Find entry point
      Find_Entry_Point (Desc, Context.Entry_Point, Entry_Index, Entry_Found);

      if not Entry_Found then
         return Unknown_Method;
      end if;

      --  Check authorization
      if not Is_Authorized (Context, Desc.Entries (Entry_Index)) then
         return Invalid_Caller;
      end if;

      return Success;
   end Validate_Context;

   --  Execute a CVM call
   procedure Execute_Call (
      Registry : in Out Registry_Array;
      DState   : in Out Dispatch_State;
      Context  : Call_Context;
      Result   : out Exec_Result
   ) is
      CVM_Index : Registry_Index;
      Found : Boolean;
      Current_State : State_Array;
      Desc : CVM_Descriptor;
      Entry_Index : Natural;
      Entry_Found : Boolean;
      Validation_Status : Exec_Status;
   begin
      Result := Empty_Result;

      --  Check call depth
      if DState.Depth >= Max_Call_Depth then
         Result := Error_Result (Internal_Error);
         return;
      end if;

      --  Mark dispatch as active
      DState.Active := True;

      --  Validate context
      Validation_Status := Validate_Context (Registry, Context);
      if Validation_Status /= Success then
         Result := Error_Result (Validation_Status);
         DState.Active := DState.Depth > 0;
         return;
      end if;

      --  Look up CVM (we know it exists from validation)
      Lookup_By_Address (Registry, Context.Target, CVM_Index, Found);

      if not Found then
         Result := Error_Result (Unknown_Method);
         DState.Active := DState.Depth > 0;
         return;
      end if;

      --  Get current state and save for rollback
      Get_State (Registry, CVM_Index, Current_State);

      --  Push onto call stack
      Push_State (DState, CVM_Index, Context, Current_State);

      --  Get descriptor and find entry point
      Get_Descriptor (Registry, CVM_Index, Desc);
      Find_Entry_Point (Desc, Context.Entry_Point, Entry_Index, Entry_Found);

      --  Execute the entry point
      if Registry (CVM_Index).Registration.Execute /= null then
         Registry (CVM_Index).Registration.Execute.all (
            Context,
            Current_State,
            Result
         );
      else
         Result := Error_Result (Internal_Error);
      end if;

      --  Handle result
      if Result.Status = Success then
         --  Commit state changes
         Set_State (Registry, CVM_Index, Current_State);
         Pop_And_Commit (DState);
         Inc_Exec_Count (Registry, CVM_Index);
      else
         --  Rollback on failure
         Pop_And_Rollback (DState, Registry);
      end if;

      --  Update active status
      DState.Active := DState.Depth > 0;
   end Execute_Call;

   --  Execute internal call (CVM-to-CVM)
   procedure Execute_Internal_Call (
      Registry    : in Out Registry_Array;
      DState      : in Out Dispatch_State;
      From_CVM    : CVM_Address;
      To_CVM      : CVM_Address;
      Entry_Point : Method_Selector;
      Params      : Param_Buffer;
      Param_Len   : Natural;
      Result      : out Exec_Result
   ) is
      Internal_Context : Call_Context;
   begin
      --  Build internal call context
      Internal_Context := (
         Caller      => From_CVM,
         Target      => To_CVM,
         Entry_Point => Entry_Point,
         Param_Len   => Param_Len,
         Params      => Params,
         Height      => 0,  -- Inherited from parent context
         Internal    => True
      );

      --  Execute the call
      Execute_Call (Registry, DState, Internal_Context, Result);
   end Execute_Internal_Call;

   --  Compute method selector from name
   procedure Compute_Selector (
      Name     : String;
      Selector : out Method_Selector
   ) is
      Name_Bytes : Byte_Array (0 .. Name'Length - 1);
   begin
      --  Convert string to bytes
      for I in Name'Range loop
         pragma Loop_Invariant (I >= Name'First);
         Name_Bytes (I - Name'First) := Byte (Character'Pos (Name (I)));
      end loop;

      --  Hash with SHA3-256
      SHA3_256 (Name_Bytes, Selector);
   end Compute_Selector;

end CVM_Dispatch;
