pragma SPARK_Mode (On);

with Anubis_MLDSA;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

package body MultiSig_Wallet with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal Helper Implementations
   ---------------------------------------------------------------------------

   function Read_U64 (Slot : State_Slot) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      if Slot.Length >= 8 then
         for I in 0 .. 7 loop
            Result := Shift_Left (Result, 8) or Unsigned_64 (Slot.Value (I));
         end loop;
      end if;
      return Result;
   end Read_U64;

   procedure Write_U64 (
      Slot  : in Out State_Slot;
      Value : Unsigned_64
   ) is
      V : Unsigned_64 := Value;
   begin
      for I in reverse 0 .. 7 loop
         Slot.Value (I) := Unsigned_8 (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
      Slot.Length := 8;
      Slot.Modified := True;
   end Write_U64;

   function Match_Selector (A, B : Hash256) return Boolean is
   begin
      return A (0) = B (0) and then
             A (1) = B (1) and then
             A (2) = B (2) and then
             A (3) = B (3);
   end Match_Selector;

   function Is_Owner (
      State : State_Array;
      Addr  : Address
   ) return Boolean is
      Owner_Addr : Address;
      Match      : Boolean;
   begin
      for I in 0 .. Max_Owners - 1 loop
         if State (State_Index (I + Natural (Owner_Slots_Base))).Length >= 32 then
            for J in 0 .. 31 loop
               Owner_Addr (J) := State (State_Index (I + Natural (Owner_Slots_Base))).Value (J);
            end loop;

            Match := True;
            for J in 0 .. 31 loop
               if Owner_Addr (J) /= Addr (J) then
                  Match := False;
               end if;
            end loop;

            if Match then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Is_Owner;

   function Get_Owner_Count (State : State_Array) return Natural is
      Count : Natural := 0;
   begin
      for I in 0 .. Max_Owners - 1 loop
         if State (State_Index (I + Natural (Owner_Slots_Base))).Length >= 32 then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Get_Owner_Count;

   function Verify_Signature (
      Message   : Byte_Array;
      Signature : Byte_Array;
      PubKey    : Address
   ) return Boolean is
      PK        : Public_Key := (others => 0);
      Sig       : Anubis_MLDSA_Types.Signature := (others => 0);
      Local_Msg : Byte_Array (0 .. Message'Length - 1);
   begin
      if Message'Length = 0 or Message'Length > 4096 or Signature'Length /= 4627 then
         return False;
      end if;

      for I in Message'Range loop
         pragma Loop_Invariant (I in Message'Range);
         pragma Loop_Invariant (I - Message'First <= Local_Msg'Last);
         Local_Msg (I - Message'First) := Message (I);
      end loop;

      for I in 0 .. 31 loop
         PK (I) := PubKey (I);
      end loop;

      for I in 32 .. PK'Last loop
         PK (I) := Unsigned_8 (I mod 256);
      end loop;

      for I in Sig'Range loop
         pragma Loop_Invariant (I in Sig'Range);
         Sig (I) := Signature (Signature'First + I);
      end loop;

      return Anubis_MLDSA.Verify (PK, Local_Msg, Sig);
   end Verify_Signature;

   function Is_Frozen (State : State_Array) return Boolean is
   begin
      if State (Config_Slot).Length > 2 then
         return State (Config_Slot).Value (2) = 1;
      end if;
      return False;
   end Is_Frozen;

   function Check_Daily_Limit (
      State        : in Out State_Array;
      Amount       : Unsigned_64;
      Current_Time : Unsigned_64
   ) return Boolean is
      Daily_Limit  : constant Unsigned_64 := Read_U64 (State (Daily_Limit_Slot));
      Last_Reset   : constant Unsigned_64 := Read_U64 (State (Last_Reset_Slot));
      Daily_Spent  : Unsigned_64 := Read_U64 (State (Daily_Spent_Slot));
      Day_Seconds  : constant := 86400;
   begin
      if Daily_Limit = 0 then
         return True;
      end if;

      if Current_Time >= Last_Reset + Day_Seconds then
         Daily_Spent := 0;
         Write_U64 (State (Last_Reset_Slot), Current_Time);
      end if;

      if Daily_Spent + Amount > Daily_Limit then
         return False;
      end if;

      Write_U64 (State (Daily_Spent_Slot), Daily_Spent + Amount);
      return True;
   end Check_Daily_Limit;

   ---------------------------------------------------------------------------
   --  Core Operations
   ---------------------------------------------------------------------------

   procedure Propose_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      To           : Address;
      Amount       : Unsigned_64 := 0;
      Tx_Count     : Unsigned_64;
      Tx_Slot      : State_Index;
      Current_Time : Unsigned_64;
      M_Value      : Unsigned_8;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      if Is_Frozen (State) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      for I in 0 .. 31 loop
         To (I) := Context.Params (I);
      end loop;

      for I in 32 .. 39 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      Tx_Count := Read_U64 (State (Tx_Count_Slot));
      Tx_Slot := State_Index (Natural (Tx_Count mod Max_Pending_Txs) + Natural (Pending_Tx_Slots_Base));

      M_Value := State (Config_Slot).Value (0);
      Current_Time := Unsigned_64 (Context.Height) * 12;

      declare
         Offset : Natural := 0;
         Temp   : Unsigned_64;
      begin
         for I in 0 .. 31 loop
            State (Tx_Slot).Value (Offset + I) := To (I);
         end loop;
         Offset := 32;

         Temp := Amount;
         for I in reverse Offset .. Offset + 7 loop
            State (Tx_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := 40;

         State (Tx_Slot).Value (Offset) := 1;
         State (Tx_Slot).Value (Offset + 1) := M_Value;
         State (Tx_Slot).Value (Offset + 2) := 0;
         State (Tx_Slot).Value (Offset + 3) := 0;
         Offset := 44;

         for I in 0 .. 31 loop
            State (Tx_Slot).Value (Offset + I) := Context.Caller (I);
         end loop;
         Offset := 76;

         Temp := Current_Time;
         for I in reverse Offset .. Offset + 7 loop
            State (Tx_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;

         State (Tx_Slot).Length := 84;
         State (Tx_Slot).Modified := True;
      end;

      Write_U64 (State (Tx_Count_Slot), Tx_Count + 1);

      Return_Data (0) := 1;
      Return_Data (1) := Unsigned_8 (Tx_Count mod 256);
      Result := (Status => CVM_Types.Success, Return_Len => 2, Return_Data => Return_Data);
   end Propose_Transaction;

   procedure Approve_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Tx_ID       : Unsigned_8;
      Tx_Slot     : State_Index;
      Approvals   : Unsigned_8;
      Return_Data : Return_Buffer := (others => 0);
   begin
      if Is_Frozen (State) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 4628 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      Tx_ID := Context.Params (0);
      Tx_Slot := State_Index (Natural (Tx_ID mod Max_Pending_Txs) + Natural (Pending_Tx_Slots_Base));

      if State (Tx_Slot).Length < 84 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if State (Tx_Slot).Value (42) = 1 or State (Tx_Slot).Value (43) = 1 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      declare
         Sig_Bytes : Byte_Array (0 .. 4626);
         Msg_Bytes : Byte_Array (0 .. 0);
         Sig_Valid : Boolean;
      begin
         for I in 0 .. 4626 loop
            Sig_Bytes (I) := Context.Params (1 + I);
         end loop;

         Msg_Bytes (0) := Tx_ID;
         Sig_Valid := Verify_Signature (Msg_Bytes, Sig_Bytes, Context.Caller);

         if not Sig_Valid then
            Return_Data (0) := 0;
            Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
            return;
         end if;
      end;

      Approvals := State (Tx_Slot).Value (40);
      State (Tx_Slot).Value (40) := Approvals + 1;
      State (Tx_Slot).Modified := True;

      Return_Data (0) := 1;
      Return_Data (1) := Approvals + 1;
      Result := (Status => CVM_Types.Success, Return_Len => 2, Return_Data => Return_Data);
   end Approve_Transaction;

   procedure Execute_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Tx_ID        : Unsigned_8;
      Tx_Slot      : State_Index;
      Approvals    : Unsigned_8;
      Required     : Unsigned_8;
      Amount       : Unsigned_64 := 0;
      Current_Time : Unsigned_64;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      if Is_Frozen (State) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 1 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      Tx_ID := Context.Params (0);
      Tx_Slot := State_Index (Natural (Tx_ID mod Max_Pending_Txs) + Natural (Pending_Tx_Slots_Base));

      if State (Tx_Slot).Length < 84 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      Approvals := State (Tx_Slot).Value (40);
      Required := State (Tx_Slot).Value (41);

      if Approvals < Required then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if State (Tx_Slot).Value (42) = 1 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      for I in 32 .. 39 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (State (Tx_Slot).Value (I));
      end loop;

      Current_Time := Unsigned_64 (Context.Height) * 12;

      if not Check_Daily_Limit (State, Amount, Current_Time) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      State (Tx_Slot).Value (42) := 1;
      State (Tx_Slot).Modified := True;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Execute_Transaction;

   procedure Revoke_Approval (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Tx_ID       : Unsigned_8;
      Tx_Slot     : State_Index;
      Approvals   : Unsigned_8;
      Return_Data : Return_Buffer := (others => 0);
   begin
      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 1 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      Tx_ID := Context.Params (0);
      Tx_Slot := State_Index (Natural (Tx_ID mod Max_Pending_Txs) + Natural (Pending_Tx_Slots_Base));

      if State (Tx_Slot).Length < 84 or State (Tx_Slot).Value (40) = 0 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      Approvals := State (Tx_Slot).Value (40);
      State (Tx_Slot).Value (40) := Approvals - 1;
      State (Tx_Slot).Modified := True;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Revoke_Approval;

   procedure Add_Owner (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      New_Owner   : Address;
      Owner_Count : Natural;
      Return_Data : Return_Buffer := (others => 0);
   begin
      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 32 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      for I in 0 .. 31 loop
         New_Owner (I) := Context.Params (I);
      end loop;

      Owner_Count := Get_Owner_Count (State);

      if Owner_Count >= Max_Owners then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      declare
         Slot_Idx : constant State_Index := State_Index (Owner_Count + Natural (Owner_Slots_Base));
      begin
         for I in 0 .. 31 loop
            State (Slot_Idx).Value (I) := New_Owner (I);
         end loop;
         State (Slot_Idx).Length := 32;
         State (Slot_Idx).Modified := True;
      end;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Add_Owner;

   procedure Remove_Owner (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Owner_To_Remove : Address;
      Owner_Count     : Natural;
      M_Value         : Unsigned_8;
      Found_Index     : Natural := Max_Owners;
      Return_Data     : Return_Buffer := (others => 0);
   begin
      --  Only owners can remove owners
      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 32 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract owner address to remove
      for I in 0 .. 31 loop
         Owner_To_Remove (I) := Context.Params (I);
      end loop;

      Owner_Count := Get_Owner_Count (State);
      M_Value := State (Config_Slot).Value (0);

      --  Cannot remove if it would make M > N
      if Owner_Count <= Natural (M_Value) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Cannot remove if only one owner left
      if Owner_Count <= 1 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Find the owner index
      for I in 0 .. Max_Owners - 1 loop
         if State (State_Index (I + Natural (Owner_Slots_Base))).Length >= 32 then
            declare
               Match : Boolean := True;
               Slot_Idx : constant State_Index :=
                  State_Index (I + Natural (Owner_Slots_Base));
            begin
               for J in 0 .. 31 loop
                  if State (Slot_Idx).Value (J) /= Owner_To_Remove (J) then
                     Match := False;
                     exit;
                  end if;
               end loop;

               if Match then
                  Found_Index := I;
                  exit;
               end if;
            end;
         end if;
      end loop;

      --  Owner not found
      if Found_Index = Max_Owners then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Remove owner by clearing the slot and compacting
      declare
         Remove_Slot : constant State_Index :=
            State_Index (Found_Index + Natural (Owner_Slots_Base));
      begin
         --  Clear the removed owner's slot
         for I in 0 .. 31 loop
            State (Remove_Slot).Value (I) := 0;
         end loop;
         State (Remove_Slot).Length := 0;
         State (Remove_Slot).Modified := True;

         --  Shift remaining owners to compact the array
         for I in Found_Index .. Max_Owners - 2 loop
            declare
               Curr_Slot : constant State_Index :=
                  State_Index (I + Natural (Owner_Slots_Base));
               Next_Slot : constant State_Index :=
                  State_Index (I + 1 + Natural (Owner_Slots_Base));
            begin
               if State (Next_Slot).Length >= 32 then
                  for J in 0 .. 31 loop
                     State (Curr_Slot).Value (J) := State (Next_Slot).Value (J);
                  end loop;
                  State (Curr_Slot).Length := 32;
                  State (Curr_Slot).Modified := True;

                  --  Clear the source slot after copy
                  for J in 0 .. 31 loop
                     State (Next_Slot).Value (J) := 0;
                  end loop;
                  State (Next_Slot).Length := 0;
                  State (Next_Slot).Modified := True;
               end if;
            end;
         end loop;
      end;

      --  Update N in config
      State (Config_Slot).Value (1) := Unsigned_8 (Owner_Count - 1);
      State (Config_Slot).Modified := True;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Remove_Owner;

   procedure Change_Threshold (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      New_M       : Unsigned_8;
      Return_Data : Return_Buffer := (others => 0);
   begin
      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 1 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      New_M := Context.Params (0);

      if New_M = 0 or New_M > Unsigned_8 (Get_Owner_Count (State)) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      State (Config_Slot).Value (0) := New_M;
      State (Config_Slot).Modified := True;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Change_Threshold;

   procedure Set_Daily_Limit (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      New_Limit   : Unsigned_64 := 0;
      Return_Data : Return_Buffer := (others => 0);
   begin
      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      if Context.Param_Len < 8 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      for I in 0 .. 7 loop
         New_Limit := Shift_Left (New_Limit, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      Write_U64 (State (Daily_Limit_Slot), New_Limit);

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Set_Daily_Limit;

   procedure Freeze (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Return_Data : Return_Buffer := (others => 0);
   begin
      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      State (Config_Slot).Value (2) := 1;
      State (Config_Slot).Modified := True;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Freeze;

   procedure Unfreeze (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Return_Data : Return_Buffer := (others => 0);
   begin
      if not Is_Owner (State, Context.Caller) then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      State (Config_Slot).Value (2) := 0;
      State (Config_Slot).Modified := True;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Unfreeze;

   procedure Get_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Tx_ID       : Unsigned_8;
      Tx_Slot     : State_Index;
      Return_Data : Return_Buffer := (others => 0);
      Offset      : Natural := 0;
   begin
      --  Check parameters
      if Context.Param_Len < 1 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      Tx_ID := Context.Params (0);
      Tx_Slot := State_Index (Natural (Tx_ID mod Max_Pending_Txs) + Natural (Pending_Tx_Slots_Base));

      --  Check if transaction exists
      if State (Tx_Slot).Length < 84 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Return transaction data:
      --  [0]: success (1)
      --  [1-32]: to address (32 bytes)
      --  [33-40]: amount (8 bytes)
      --  [41]: approvals count
      --  [42]: required approvals
      --  [43]: executed flag
      --  [44]: revoked flag
      --  [45-76]: proposer address (32 bytes)
      --  [77-84]: propose time (8 bytes)
      --  Total: 85 bytes

      Return_Data (0) := 1;  -- Success
      Offset := 1;

      --  Copy to address (32 bytes)
      for I in 0 .. 31 loop
         Return_Data (Offset + I) := State (Tx_Slot).Value (I);
      end loop;
      Offset := 33;

      --  Copy amount (8 bytes)
      for I in 32 .. 39 loop
         Return_Data (Offset + I - 32) := State (Tx_Slot).Value (I);
      end loop;
      Offset := 41;

      --  Approvals count
      Return_Data (Offset) := State (Tx_Slot).Value (40);
      Offset := 42;

      --  Required approvals
      Return_Data (Offset) := State (Tx_Slot).Value (41);
      Offset := 43;

      --  Executed flag
      Return_Data (Offset) := State (Tx_Slot).Value (42);
      Offset := 44;

      --  Revoked flag
      Return_Data (Offset) := State (Tx_Slot).Value (43);
      Offset := 45;

      --  Copy proposer address (32 bytes)
      for I in 44 .. 75 loop
         Return_Data (Offset + I - 44) := State (Tx_Slot).Value (I);
      end loop;
      Offset := 77;

      --  Copy propose time (8 bytes)
      for I in 76 .. 83 loop
         Return_Data (Offset + I - 76) := State (Tx_Slot).Value (I);
      end loop;

      Result := (Status => CVM_Types.Success, Return_Len => 85, Return_Data => Return_Data);
   end Get_Transaction;

   procedure Get_Owners (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      Owner_Count : constant Natural := Get_Owner_Count (State);
      Return_Data : Return_Buffer := (others => 0);
   begin
      Return_Data (0) := Unsigned_8 (Owner_Count);
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Get_Owners;

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor is
      Desc : CVM_Descriptor := Empty_Descriptor;
   begin
      Desc.Info.Name := "MultiSig Wallet - M-of-N Multi-Signature with ML-DSA-87          ";
      Desc.Info.Caps := Default_Capabilities;
      Desc.Info.Active := True;
      Desc.Entry_Count := 12;

      Desc.Entries (0) := (Selector => Propose_Transaction_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (1) := (Selector => Approve_Transaction_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (2) := (Selector => Execute_Transaction_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (3) := (Selector => Revoke_Approval_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (4) := (Selector => Add_Owner_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (5) := (Selector => Remove_Owner_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (6) := (Selector => Change_Threshold_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (7) := (Selector => Set_Daily_Limit_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (8) := (Selector => Freeze_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (9) := (Selector => Unfreeze_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (10) := (Selector => Get_Transaction_Selector, Handler => null, Public => True, ReadOnly => True);
      Desc.Entries (11) := (Selector => Get_Owners_Selector, Handler => null, Public => True, ReadOnly => True);

      return Desc;
   end Get_Descriptor;

   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) is
      M, N        : Unsigned_8;
      Daily_Limit : Unsigned_64 := 0;
      Idx         : Natural;
   begin
      State := Empty_State;

      if Init_Params'Length < 10 then
         Success := False;
         return;
      end if;

      M := Init_Params (Init_Params'First);
      N := Init_Params (Init_Params'First + 1);

      if M = 0 or N = 0 or M > N or N > Max_Owners then
         Success := False;
         return;
      end if;

      for I in 0 .. 7 loop
         Daily_Limit := Shift_Left (Daily_Limit, 8) or
                        Unsigned_64 (Init_Params (Init_Params'First + 2 + I));
      end loop;

      State (Config_Slot).Value (0) := M;
      State (Config_Slot).Value (1) := N;
      State (Config_Slot).Value (2) := 0;
      State (Config_Slot).Length := 3;

      Write_U64 (State (Daily_Limit_Slot), Daily_Limit);
      Write_U64 (State (Tx_Count_Slot), 0);

      Idx := Init_Params'First + 10;
      for I in 0 .. Natural (N) - 1 loop
         exit when Idx + 32 > Init_Params'Last + 1;

         for J in 0 .. 31 loop
            State (State_Index (I + Natural (Owner_Slots_Base))).Value (J) :=
               Init_Params (Idx + J);
         end loop;
         State (State_Index (I + Natural (Owner_Slots_Base))).Length := 32;

         Idx := Idx + 32;
      end loop;

      Success := True;
   end Init;

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
   begin
      if Match_Selector (Context.Entry_Point, Propose_Transaction_Selector) then
         Propose_Transaction (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Approve_Transaction_Selector) then
         Approve_Transaction (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Execute_Transaction_Selector) then
         Execute_Transaction (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Revoke_Approval_Selector) then
         Revoke_Approval (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Add_Owner_Selector) then
         Add_Owner (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Remove_Owner_Selector) then
         Remove_Owner (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Change_Threshold_Selector) then
         Change_Threshold (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Set_Daily_Limit_Selector) then
         Set_Daily_Limit (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Freeze_Selector) then
         Freeze (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Unfreeze_Selector) then
         Unfreeze (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Get_Transaction_Selector) then
         Get_Transaction (Context, State, Result);
      elsif Match_Selector (Context.Entry_Point, Get_Owners_Selector) then
         Get_Owners (Context, State, Result);
      else
         Result := Error_Result (Unknown_Method);
      end if;
   end Execute;

end MultiSig_Wallet;
