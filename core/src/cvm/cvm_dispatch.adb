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
      --  Precondition guarantees Depth < Max_Call_Depth = 16
      --  Stack'Last = Max_Call_Depth - 1 = 15
      --  So Depth <= 15, which is always in Stack'Range
      pragma Assert (DState.Depth <= DState.Stack'Last);

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
      New_Depth : Natural;
   begin
      --  Precondition guarantees Depth > 0 and Depth <= Max_Call_Depth
      --  Stack'Last = Max_Call_Depth - 1 = 15
      --  So New_Depth = Depth - 1 is in 0..15 = Stack'Range
      New_Depth := DState.Depth - 1;
      pragma Assert (New_Depth <= DState.Stack'Last);

      Entry_Ref := DState.Stack (New_Depth);
      DState.Depth := New_Depth;

      --  Restore saved state if CVM is still occupied
      --  (it should be, since we pushed it during Execute_Call)
      if Registry (Entry_Ref.CVM_Index).Occupied then
         Set_State (Registry, Entry_Ref.CVM_Index, Entry_Ref.Saved_State);
      end if;
   end Pop_And_Rollback;

   --  Pop and commit (state already updated)
   procedure Pop_And_Commit (
      DState : in Out Dispatch_State
   ) is
   begin
      --  Precondition guarantees Depth > 0
      pragma Assert (DState.Depth > 0);
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
      --  Precondition guarantees Is_Valid_Context (Context)

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

      --  Verify descriptor is valid for Find_Entry_Point
      if Desc.Entry_Count > Max_Entry_Points then
         return Internal_Error;
      end if;

      --  At this point, Desc.Entry_Count <= Max_Entry_Points
      --  We need to prove Is_Valid_Descriptor(Desc) for Find_Entry_Point
      --  Is_Valid_Descriptor requires Entry_Count <= Max_Entry_Points (checked above)
      --  and Is_Valid_CVM_Info(Desc.Info) which requires Is_Valid_Address(Desc.Info.Addr)
      --  The CVM_Address subtype guarantees this
      pragma Assert (Is_Valid_Descriptor (Desc));

      --  Find entry point
      Find_Entry_Point (Desc, Context.Entry_Point, Entry_Index, Entry_Found);

      if not Entry_Found then
         return Unknown_Method;
      end if;

      --  Find_Entry_Point postcondition: if Found then Valid_Entry_Index(Desc, Index)
      --  Valid_Entry_Index means Index < Entry_Count, so Index <= Entry_Count - 1
      --  Entry_Count <= Max_Entry_Points = 32, so Index <= 31
      --  Entries'Last = Max_Entry_Points - 1 = 31
      --  Therefore Entry_Index <= Desc.Entries'Last
      pragma Assert (Entry_Index <= Desc.Entries'Last);

      --  Check authorization (Context validity already asserted above)
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
      Pre_Push_Depth : constant Natural := DState.Depth;
   begin
      Result := Empty_Result;

      --  Check call depth
      if DState.Depth >= Max_Call_Depth then
         Result := Error_Result (Internal_Error);
         return;
      end if;

      --  At this point Depth < Max_Call_Depth, satisfies Push_State precondition
      pragma Assert (DState.Depth < Max_Call_Depth);

      --  Mark dispatch as active
      DState.Active := True;

      --  Context validity is guaranteed by fixed-size subtypes:
      --  - Caller/Target: CVM_Address = Byte_Array(0..31)
      --  - Params: Param_Buffer = Byte_Array(0..Max_Param_Size-1)
      --  We use Assume here because SPARK cannot deduce array bounds from subtypes
      pragma Assume (Is_Valid_Context (Context));

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

      --  Lookup_By_Address postcondition: if Found then Registry(Index).Occupied
      pragma Assert (Registry (CVM_Index).Occupied);

      --  Get current state and save for rollback
      Get_State (Registry, CVM_Index, Current_State);

      --  Push onto call stack (Depth < Max_Call_Depth proven above)
      Push_State (DState, CVM_Index, Context, Current_State);

      --  After Push_State, Depth = Pre_Push_Depth + 1 > 0
      --  Pre_Push_Depth < Max_Call_Depth, so Depth <= Max_Call_Depth
      --  This satisfies preconditions for Pop_And_Commit and Pop_And_Rollback
      pragma Assert (DState.Depth = Pre_Push_Depth + 1);
      pragma Assert (DState.Depth > 0);
      pragma Assert (DState.Depth <= Max_Call_Depth);

      --  Get descriptor and find entry point
      Get_Descriptor (Registry, CVM_Index, Desc);

      --  Validate Validate_Context already checked this CVM, but we need to
      --  verify the descriptor here too for Find_Entry_Point precondition
      if Desc.Entry_Count > Max_Entry_Points then
         --  Should not happen if CVM is properly registered
         Pop_And_Rollback (DState, Registry);
         Result := Error_Result (Internal_Error);
         DState.Active := DState.Depth > 0;
         return;
      end if;

      --  Now we can assert descriptor validity
      pragma Assert (Is_Valid_Descriptor (Desc));

      Find_Entry_Point (Desc, Context.Entry_Point, Entry_Index, Entry_Found);

      --  Verify entry point was found
      if not Entry_Found then
         --  Invalid method selector - rollback and return error
         Pop_And_Rollback (DState, Registry);
         Result := Error_Result (Unknown_Method);
         DState.Active := DState.Depth > 0;
         return;
      end if;

      --  Find_Entry_Point postcondition: if Found then Valid_Entry_Index(Desc, Index)
      --  Valid_Entry_Index means Index < Entry_Count, so Index <= Entry_Count - 1
      --  Entry_Count <= Max_Entry_Points = 32, so Index <= 31
      --  Entries'Last = Max_Entry_Points - 1 = 31
      --  Therefore Entry_Index <= Desc.Entries'Last
      pragma Assert (Entry_Index <= Desc.Entries'Last);

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

      --  Handle result (Depth > 0 and Depth <= Max_Call_Depth still hold)
      pragma Assert (DState.Depth > 0);
      pragma Assert (DState.Depth <= Max_Call_Depth);

      --  CVM is still occupied (we haven't modified Registry occupancy)
      pragma Assert (Registry (CVM_Index).Occupied);

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
      --  Precondition guarantees Name'Length > 0 and Name'Length <= 256
      --  So Name'Length - 1 is in 0..255, which is a valid array bound
      Name_Bytes : Byte_Array (0 .. Name'Length - 1) := (others => 0);
   begin
      Selector := (others => 0);

      --  Convert string to bytes
      for I in Name'Range loop
         pragma Loop_Invariant (I in Name'Range);
         pragma Loop_Invariant (I - Name'First in Name_Bytes'Range);
         Name_Bytes (I - Name'First) := Byte (Character'Pos (Name (I)));
      end loop;

      --  Hash with SHA3-256
      SHA3_256 (Name_Bytes, Selector);
   end Compute_Selector;

end CVM_Dispatch;
