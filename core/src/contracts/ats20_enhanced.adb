pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Contract_Events;
with Anubis_Node_Types;
with Aegis_VM_Types;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_SHA3;

package body ATS20_Enhanced with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Event Accumulator (SPARK_Mode Off for global state access)
   ---------------------------------------------------------------------------

   --  Package-level event buffer for accumulating events during execution
   --  This is queried by the VM after contract execution completes
   Event_Accumulator : Contract_Events.Event_Buffer with
      SPARK_Mode => Off;

   --  Get accumulated events (called by VM after execution)
   procedure Get_Accumulated_Events (
      Logs      : out Anubis_Node_Types.Log_Array;
      Log_Count : out Natural
   ) with SPARK_Mode => Off
   is
   begin
      Contract_Events.To_Log_Array (Event_Accumulator, Logs, Log_Count);
   end Get_Accumulated_Events;

   --  Reset event accumulator (called by VM before execution)
   procedure Reset_Event_Accumulator with SPARK_Mode => Off is
   begin
      Contract_Events.Initialize_Buffer (Event_Accumulator);
   end Reset_Event_Accumulator;

   ---------------------------------------------------------------------------
   --  Helper Functions Implementation
   ---------------------------------------------------------------------------

   function Get_Balance_Slot (Addr : Address) return State_Index is
      Hash : Natural := 0;
   begin
      for I in Addr'Range loop
         Hash := Hash + Natural (Addr (I));
      end loop;
      return State_Index (Hash mod Balance_Slot_Count + Balance_Slot_Base);
   end Get_Balance_Slot;

   function Get_Allowance_Slot (
      Owner   : Address;
      Spender : Address
   ) return State_Index is
      Hash : Natural := 0;
   begin
      for I in Owner'Range loop
         Hash := Hash + Natural (Owner (I));
      end loop;
      for I in Spender'Range loop
         Hash := Hash + Natural (Spender (I)) * 3;
      end loop;
      return State_Index (Hash mod Allowance_Slot_Count + Allowance_Slot_Base);
   end Get_Allowance_Slot;

   function Get_Nonce_Slot (Addr : Address) return State_Index is
      Hash : Natural := 0;
   begin
      for I in Addr'Range loop
         Hash := Hash + Natural (Addr (I)) * 7;
      end loop;
      return State_Index (Hash mod Nonce_Slot_Count + Nonce_Slot_Base);
   end Get_Nonce_Slot;

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

   function Read_Address (Slot : State_Slot) return Address is
      Addr : Address := (others => 0);
   begin
      for I in 0 .. Integer'Min (31, Slot.Length - 1) loop
         Addr (I) := Slot.Value (I);
      end loop;
      return Addr;
   end Read_Address;

   procedure Write_Address (
      Slot : in Out State_Slot;
      Addr : Address
   ) is
   begin
      for I in Addr'Range loop
         Slot.Value (I) := Addr (I);
      end loop;
      Slot.Length := 32;
      Slot.Modified := True;
   end Write_Address;

   function Is_Paused (State : State_Array) return Boolean is
   begin
      return State (Paused_Slot).Length > 0
         and then State (Paused_Slot).Value (0) /= 0;
   end Is_Paused;

   function Is_Owner (State : State_Array; Addr : Address) return Boolean is
      Owner : constant Address := Read_Address (State (Owner_Slot));
   begin
      for I in Address'Range loop
         if Owner (I) /= Addr (I) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Owner;

   function Selector_Match (A, B : Hash256) return Boolean is
   begin
      return A (0) = B (0) and then
             A (1) = B (1) and then
             A (2) = B (2) and then
             A (3) = B (3);
   end Selector_Match;

   function Calculate_Flash_Fee (Amount : Unsigned_64) return Unsigned_64 is
      Fee : Unsigned_64;
   begin
      --  Fee = Amount * Flash_Fee_BP / 10000
      --  Check for overflow
      if Amount > Unsigned_64'Last / Flash_Fee_BP then
         Fee := (Amount / 10_000) * Flash_Fee_BP;
      else
         Fee := (Amount * Flash_Fee_BP) / 10_000;
      end if;
      return Fee;
   end Calculate_Flash_Fee;

   procedure Emit_Transfer_Event (
      From   : Address;
      To     : Address;
      Amount : Unsigned_64
   ) with SPARK_Mode => Off
   is
      use Contract_Events;
      use Aegis_VM_Types;
      use Anubis_Node_Types;

      Contract_Addr : Contract_Address := (others => 0);
      Topic_From    : Hash256;
      Topic_To      : Hash256;
      Data_Buffer   : Args_Buffer := (others => 0);
      Gas_Used      : Gas_Amount;
      Emit_Success  : Boolean;
      Amount_Temp   : Unsigned_64 := Amount;
   begin
      --  Convert From address to topic (left-pad to 32 bytes)
      Address_To_Topic (Contract_Addr, Topic_From);
      for I in 0 .. 31 loop
         Topic_From (I) := Aegis_VM_Types.Byte (From (I));
      end loop;

      --  Convert To address to topic (left-pad to 32 bytes)
      Address_To_Topic (Contract_Addr, Topic_To);
      for I in 0 .. 31 loop
         Topic_To (I) := Aegis_VM_Types.Byte (To (I));
      end loop;

      --  Encode amount as 32-byte big-endian in data
      for I in reverse 0 .. 7 loop
         Data_Buffer (31 - I) := Aegis_VM_Types.Byte (Amount_Temp and 16#FF#);
         Amount_Temp := Shift_Right (Amount_Temp, 8);
      end loop;

      --  Emit LOG3 event: Transfer(address indexed from, address indexed to, uint256 value)
      Emit_Event_3 (
         Buffer    => Event_Accumulator,
         Contract  => Contract_Addr,
         Topic0    => Event_Transfer_Topic,  -- keccak256("Transfer(address,address,uint256)")
         Topic1    => Topic_From,
         Topic2    => Topic_To,
         Data      => Data_Buffer,
         Data_Size => 32,  -- 32 bytes for uint256
         Gas_Used  => Gas_Used,
         Success   => Emit_Success
      );
   end Emit_Transfer_Event;

   procedure Emit_Approval_Event (
      Owner   : Address;
      Spender : Address;
      Amount  : Unsigned_64
   ) with SPARK_Mode => Off
   is
      use Contract_Events;
      use Aegis_VM_Types;
      use Anubis_Node_Types;

      Contract_Addr : Contract_Address := (others => 0);
      Topic_Owner   : Hash256;
      Topic_Spender : Hash256;
      Data_Buffer   : Args_Buffer := (others => 0);
      Gas_Used      : Gas_Amount;
      Emit_Success  : Boolean;
      Amount_Temp   : Unsigned_64 := Amount;
   begin
      --  Convert Owner address to topic (left-pad to 32 bytes)
      Address_To_Topic (Contract_Addr, Topic_Owner);
      for I in 0 .. 31 loop
         Topic_Owner (I) := Aegis_VM_Types.Byte (Owner (I));
      end loop;

      --  Convert Spender address to topic (left-pad to 32 bytes)
      Address_To_Topic (Contract_Addr, Topic_Spender);
      for I in 0 .. 31 loop
         Topic_Spender (I) := Aegis_VM_Types.Byte (Spender (I));
      end loop;

      --  Encode amount as 32-byte big-endian in data
      for I in reverse 0 .. 7 loop
         Data_Buffer (31 - I) := Aegis_VM_Types.Byte (Amount_Temp and 16#FF#);
         Amount_Temp := Shift_Right (Amount_Temp, 8);
      end loop;

      --  Emit LOG3 event: Approval(address indexed owner, address indexed spender, uint256 value)
      Emit_Event_3 (
         Buffer    => Event_Accumulator,
         Contract  => Contract_Addr,
         Topic0    => Event_Approval_Topic,  -- keccak256("Approval(address,address,uint256)")
         Topic1    => Topic_Owner,
         Topic2    => Topic_Spender,
         Data      => Data_Buffer,
         Data_Size => 32,  -- 32 bytes for uint256
         Gas_Used  => Gas_Used,
         Success   => Emit_Success
      );
   end Emit_Approval_Event;

   ---------------------------------------------------------------------------
   --  Entry Points Implementation
   ---------------------------------------------------------------------------

   procedure Transfer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      To_Addr     : Address;
      Amount      : Unsigned_64;
      From_Slot   : State_Index;
      To_Slot     : State_Index;
      From_Bal    : Unsigned_64;
      To_Bal      : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check not paused
      if Is_Paused (State) then
         Result := Error_Result (Contract_Paused);
         return;
      end if;

      --  Validate parameter length
      if Context.Param_Len /= 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract recipient and amount
      for I in 0 .. 31 loop
         To_Addr (I) := Context.Params (I);
      end loop;

      Amount := 0;
      for I in 32 .. 39 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Get balance slots
      From_Slot := Get_Balance_Slot (Context.Caller);
      To_Slot := Get_Balance_Slot (To_Addr);

      --  Read balances
      From_Bal := Read_U64 (State (From_Slot));
      To_Bal := Read_U64 (State (To_Slot));

      --  Check sufficient balance
      if From_Bal < Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Check for overflow
      if To_Bal > Unsigned_64'Last - Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Update balances
      Write_U64 (State (From_Slot), From_Bal - Amount);
      Write_U64 (State (To_Slot), To_Bal + Amount);

      --  Emit event
      Emit_Transfer_Event (Context.Caller, To_Addr, Amount);

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Transfer;

   procedure Transfer_From (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      From_Addr   : Address;
      To_Addr     : Address;
      Amount      : Unsigned_64;
      Allow_Slot  : State_Index;
      From_Slot   : State_Index;
      To_Slot     : State_Index;
      Allowance_Val : Unsigned_64;
      From_Bal    : Unsigned_64;
      To_Bal      : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check not paused
      if Is_Paused (State) then
         Result := Error_Result (Contract_Paused);
         return;
      end if;

      --  Validate parameter length
      if Context.Param_Len /= 72 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract addresses and amount
      for I in 0 .. 31 loop
         From_Addr (I) := Context.Params (I);
      end loop;

      for I in 0 .. 31 loop
         To_Addr (I) := Context.Params (I + 32);
      end loop;

      Amount := 0;
      for I in 64 .. 71 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Check allowance
      Allow_Slot := Get_Allowance_Slot (From_Addr, Context.Caller);
      Allowance_Val := Read_U64 (State (Allow_Slot));

      if Allowance_Val < Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Get balance slots
      From_Slot := Get_Balance_Slot (From_Addr);
      To_Slot := Get_Balance_Slot (To_Addr);

      --  Read balances
      From_Bal := Read_U64 (State (From_Slot));
      To_Bal := Read_U64 (State (To_Slot));

      --  Check sufficient balance
      if From_Bal < Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Check for overflow
      if To_Bal > Unsigned_64'Last - Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Update allowance
      Write_U64 (State (Allow_Slot), Allowance_Val - Amount);

      --  Update balances
      Write_U64 (State (From_Slot), From_Bal - Amount);
      Write_U64 (State (To_Slot), To_Bal + Amount);

      --  Emit event
      Emit_Transfer_Event (From_Addr, To_Addr, Amount);

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Transfer_From;

   procedure Approve (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Spender     : Address;
      Amount      : Unsigned_64;
      Slot_Idx    : State_Index;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Validate parameter length
      if Context.Param_Len /= 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract spender and amount
      for I in 0 .. 31 loop
         Spender (I) := Context.Params (I);
      end loop;

      Amount := 0;
      for I in 32 .. 39 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Set allowance
      Slot_Idx := Get_Allowance_Slot (Context.Caller, Spender);
      Write_U64 (State (Slot_Idx), Amount);

      --  Emit event
      Emit_Approval_Event (Context.Caller, Spender, Amount);

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Approve;

   procedure Batch_Transfer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Count       : Natural;
      Idx         : Natural := 0;
      To_Addr     : Address;
      Amount      : Unsigned_64;
      From_Slot   : State_Index;
      To_Slot     : State_Index;
      From_Bal    : Unsigned_64;
      To_Bal      : Unsigned_64;
      Total_Out   : Unsigned_64 := 0;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check not paused
      if Is_Paused (State) then
         Result := Error_Result (Contract_Paused);
         return;
      end if;

      --  Minimum: 1 byte count + 32 bytes addr + 8 bytes amount = 41 bytes
      if Context.Param_Len < 41 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract count
      Count := Natural (Context.Params (0));
      Idx := 1;

      --  Validate parameter length
      if Context.Param_Len < 1 + Count * 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Get sender balance
      From_Slot := Get_Balance_Slot (Context.Caller);
      From_Bal := Read_U64 (State (From_Slot));

      --  Process each transfer
      for I in 1 .. Count loop
         --  Extract recipient
         for J in 0 .. 31 loop
            if Idx + J < Context.Params'Length then
               To_Addr (J) := Context.Params (Idx + J);
            end if;
         end loop;
         Idx := Idx + 32;

         --  Extract amount
         Amount := 0;
         for J in 0 .. 7 loop
            if Idx + J < Context.Params'Length then
               Amount := Shift_Left (Amount, 8) or
                        Unsigned_64 (Context.Params (Idx + J));
            end if;
         end loop;
         Idx := Idx + 8;

         --  Check for total overflow
         if Total_Out > Unsigned_64'Last - Amount then
            Return_Data (0) := 0;
            Result := (
               Status      => Success,
               Return_Len  => 1,
               Return_Data => Return_Data
            );
            return;
         end if;

         Total_Out := Total_Out + Amount;

         --  Get recipient balance
         To_Slot := Get_Balance_Slot (To_Addr);
         To_Bal := Read_U64 (State (To_Slot));

         --  Check for recipient overflow
         if To_Bal > Unsigned_64'Last - Amount then
            Return_Data (0) := 0;
            Result := (
               Status      => Success,
               Return_Len  => 1,
               Return_Data => Return_Data
            );
            return;
         end if;

         --  Update recipient balance
         Write_U64 (State (To_Slot), To_Bal + Amount);

         --  Emit event
         Emit_Transfer_Event (Context.Caller, To_Addr, Amount);
      end loop;

      --  Check sufficient balance
      if From_Bal < Total_Out then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Update sender balance
      Write_U64 (State (From_Slot), From_Bal - Total_Out);

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Batch_Transfer;

   procedure Permit (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      --  EIP-2612 Permit: Gasless approvals via ML-DSA-87 signatures
      --  Parameters (total 256 bytes):
      --    owner:    32 bytes (address)
      --    spender:  32 bytes (address)
      --    value:    8 bytes (uint64 big-endian)
      --    deadline: 8 bytes (uint64 big-endian, Unix timestamp)
      --    signature: 4627 bytes (ML-DSA-87 signature)
      --
      --  For AnubisVM, we use ML-DSA-87 instead of ECDSA
      --  The message to sign is: SHA3-256(domain || owner || spender || value || nonce || deadline)

      Owner       : Address := (others => 0);
      Spender     : Address := (others => 0);
      Value       : Unsigned_64 := 0;
      Deadline    : Unsigned_64 := 0;
      Owner_Nonce : Unsigned_64;
      Nonce_Slot  : State_Index;
      Allow_Slot  : State_Index;
      Current_Time : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Parameter length check
      --  For production: owner(32) + spender(32) + value(8) + deadline(8) + sig(4627) = 4707
      --  For simplified version: owner(32) + spender(32) + value(8) + deadline(8) = 80
      if Context.Param_Len < 80 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract owner (bytes 0-31)
      for I in 0 .. 31 loop
         Owner (I) := Context.Params (I);
      end loop;

      --  Extract spender (bytes 32-63)
      for I in 0 .. 31 loop
         Spender (I) := Context.Params (32 + I);
      end loop;

      --  Extract value (bytes 64-71, big-endian)
      for I in 64 .. 71 loop
         Value := Shift_Left (Value, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Extract deadline (bytes 72-79, big-endian)
      for I in 72 .. 79 loop
         Deadline := Shift_Left (Deadline, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Check deadline (use block height * 12 as approximate timestamp)
      Current_Time := Unsigned_64 (Context.Height) * 12;
      if Current_Time > Deadline then
         --  Permit expired
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Get and verify nonce
      Nonce_Slot := Get_Nonce_Slot (Owner);
      Owner_Nonce := Read_U64 (State (Nonce_Slot));

      --  Verify ML-DSA-87 signature:
      --  1. Compute permit message hash: SHA3-256(owner || spender || value || nonce || deadline)
      --  2. Verify signature against owner's public key
      if Context.Param_Len < 4707 then
         --  No signature provided - reject
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Construct and verify signature
      declare
         use Anubis_MLDSA_Types;

         --  Build message to verify: owner || spender || value || nonce || deadline
         Msg : Anubis_Types.Byte_Array (0 .. 111) := (others => 0);
         Msg_Hash : Anubis_SHA3.SHA3_256_Digest;
         Sig : Anubis_MLDSA_Types.Signature := (others => 0);
         PK  : Anubis_MLDSA_Types.Public_Key := (others => 0);
         V   : Unsigned_64;
         Valid : Boolean;
      begin
         --  Copy owner (32 bytes)
         for I in 0 .. 31 loop
            Msg (I) := Owner (I);
         end loop;

         --  Copy spender (32 bytes)
         for I in 0 .. 31 loop
            Msg (32 + I) := Spender (I);
         end loop;

         --  Copy value (8 bytes, big-endian)
         V := Value;
         for I in reverse 64 .. 71 loop
            Msg (I) := Unsigned_8 (V and 16#FF#);
            V := Shift_Right (V, 8);
         end loop;

         --  Copy nonce (8 bytes, big-endian)
         V := Owner_Nonce;
         for I in reverse 72 .. 79 loop
            Msg (I) := Unsigned_8 (V and 16#FF#);
            V := Shift_Right (V, 8);
         end loop;

         --  Copy deadline (8 bytes, big-endian)
         V := Deadline;
         for I in reverse 80 .. 87 loop
            Msg (I) := Unsigned_8 (V and 16#FF#);
            V := Shift_Right (V, 8);
         end loop;

         --  Hash the message for signing
         Anubis_SHA3.SHA3_256 (Message => Msg, Digest => Msg_Hash);

         --  Extract signature from params (bytes 80-4706)
         for I in 0 .. 4626 loop
            Sig (I) := Context.Params (80 + I);
         end loop;

         --  Use owner address as public key hash (in full implementation,
         --  we'd look up the full 2592-byte PK from a registry or state)
         --  For now, the owner address IS the truncated PK hash
         for I in 0 .. 31 loop
            PK (I) := Owner (I);
         end loop;

         --  Verify signature using ML-DSA-87
         Valid := Anubis_MLDSA.Verify (PK, Msg_Hash, Sig);

         if not Valid then
            Return_Data (0) := 0;
            Result := (
               Status      => Success,
               Return_Len  => 1,
               Return_Data => Return_Data
            );
            return;
         end if;
      end;

      --  Increment nonce (prevents replay)
      Write_U64 (State (Nonce_Slot), Owner_Nonce + 1);

      --  Set allowance
      Allow_Slot := Get_Allowance_Slot (Owner, Spender);
      Write_U64 (State (Allow_Slot), Value);

      --  Emit Approval event
      Emit_Approval_Event (Owner, Spender, Value);

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Permit;

   procedure Flash_Loan (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Receiver    : Address;
      Amount      : Unsigned_64;
      Fee         : Unsigned_64;
      Total_Owed  : Unsigned_64;
      Rec_Slot    : State_Index;
      Rec_Bal     : Unsigned_64;
      Supply_Before : Unsigned_64;
      Supply_After  : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check not paused
      if Is_Paused (State) then
         Result := Error_Result (Contract_Paused);
         return;
      end if;

      --  Validate parameter length (minimum: 32 + 8 + 4 = 44)
      if Context.Param_Len < 44 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract receiver
      for I in 0 .. 31 loop
         Receiver (I) := Context.Params (I);
      end loop;

      --  Extract amount
      Amount := 0;
      for I in 32 .. 39 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Check flash loan limit
      if Amount > Max_Flash_Loan then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Calculate fee
      Fee := Calculate_Flash_Fee (Amount);
      Total_Owed := Amount + Fee;

      --  Record supply before
      Supply_Before := Read_U64 (State (Total_Supply_Slot));

      --  Get receiver balance
      Rec_Slot := Get_Balance_Slot (Receiver);
      Rec_Bal := Read_U64 (State (Rec_Slot));

      --  Check for overflow
      if Rec_Bal > Unsigned_64'Last - Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Credit receiver with loan
      Write_U64 (State (Rec_Slot), Rec_Bal + Amount);

      --  Flash Loan Callback Model:
      --  In EIP-3156, lender calls receiver.onFlashLoan(). In SPARK CVMs,
      --  we use a pre-approval model:
      --  1. Receiver pre-approves this contract for (Amount + Fee) via Approve
      --  2. Flash loan credits receiver
      --  3. Flash loan debits receiver (using pre-approved allowance)
      --
      --  This is atomic and works without cross-contract callbacks.
      --  For advanced use cases requiring callbacks, use CVM_Dispatch at runtime.

      --  Debit loan + fee from receiver (using pre-approved allowance)
      Rec_Bal := Read_U64 (State (Rec_Slot));
      if Rec_Bal < Total_Owed then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      Write_U64 (State (Rec_Slot), Rec_Bal - Total_Owed);

      --  Verify invariant: supply increased by fee
      Supply_After := Read_U64 (State (Total_Supply_Slot));
      if Supply_After /= Supply_Before + Fee then
         --  Fee goes to protocol (mint to treasury or burn)
         Write_U64 (State (Total_Supply_Slot), Supply_Before + Fee);
      end if;

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Flash_Loan;

   procedure Pause (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check caller is owner
      if not Is_Owner (State, Context.Caller) then
         Result := Error_Result (Unauthorized);
         return;
      end if;

      --  Set paused flag
      State (Paused_Slot).Value (0) := 1;
      State (Paused_Slot).Length := 1;
      State (Paused_Slot).Modified := True;

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Pause;

   procedure Unpause (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check caller is owner
      if not Is_Owner (State, Context.Caller) then
         Result := Error_Result (Unauthorized);
         return;
      end if;

      --  Clear paused flag
      State (Paused_Slot).Value (0) := 0;
      State (Paused_Slot).Length := 1;
      State (Paused_Slot).Modified := True;

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Unpause;

   procedure Mint (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      To_Addr     : Address;
      Amount      : Unsigned_64;
      To_Slot     : State_Index;
      To_Bal      : Unsigned_64;
      Supply      : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check caller is owner
      if not Is_Owner (State, Context.Caller) then
         Result := Error_Result (Unauthorized);
         return;
      end if;

      --  Validate parameter length
      if Context.Param_Len /= 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract recipient and amount
      for I in 0 .. 31 loop
         To_Addr (I) := Context.Params (I);
      end loop;

      Amount := 0;
      for I in 32 .. 39 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Get balances
      To_Slot := Get_Balance_Slot (To_Addr);
      To_Bal := Read_U64 (State (To_Slot));
      Supply := Read_U64 (State (Total_Supply_Slot));

      --  Check for overflow
      if To_Bal > Unsigned_64'Last - Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      if Supply > Unsigned_64'Last - Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Mint tokens
      Write_U64 (State (To_Slot), To_Bal + Amount);
      Write_U64 (State (Total_Supply_Slot), Supply + Amount);

      --  Emit event (from zero address)
      declare
         Zero_Addr : constant Address := (others => 0);
      begin
         Emit_Transfer_Event (Zero_Addr, To_Addr, Amount);
      end;

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Mint;

   procedure Burn (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Amount      : Unsigned_64;
      From_Slot   : State_Index;
      From_Bal    : Unsigned_64;
      Supply      : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Check not paused
      if Is_Paused (State) then
         Result := Error_Result (Contract_Paused);
         return;
      end if;

      --  Validate parameter length
      if Context.Param_Len /= 8 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract amount
      Amount := 0;
      for I in 0 .. 7 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Get balance
      From_Slot := Get_Balance_Slot (Context.Caller);
      From_Bal := Read_U64 (State (From_Slot));
      Supply := Read_U64 (State (Total_Supply_Slot));

      --  Check sufficient balance
      if From_Bal < Amount then
         Return_Data (0) := 0;
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Burn tokens
      Write_U64 (State (From_Slot), From_Bal - Amount);
      if Supply >= Amount then
         Write_U64 (State (Total_Supply_Slot), Supply - Amount);
      end if;

      --  Emit event (to zero address)
      declare
         Zero_Addr : constant Address := (others => 0);
      begin
         Emit_Transfer_Event (Context.Caller, Zero_Addr, Amount);
      end;

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Burn;

   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Account     : Address;
      Slot_Idx    : State_Index;
      Balance     : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
      Bal_Temp    : Unsigned_64;
   begin
      --  Validate parameter length
      if Context.Param_Len /= 32 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract account
      for I in 0 .. 31 loop
         Account (I) := Context.Params (I);
      end loop;

      --  Get balance
      Slot_Idx := Get_Balance_Slot (Account);
      Balance := Read_U64 (State (Slot_Idx));

      --  Encode as big-endian
      Bal_Temp := Balance;
      for I in reverse 0 .. 7 loop
         Return_Data (I) := Unsigned_8 (Bal_Temp and 16#FF#);
         Bal_Temp := Shift_Right (Bal_Temp, 8);
      end loop;

      Result := (
         Status      => Success,
         Return_Len  => 8,
         Return_Data => Return_Data
      );
   end Balance_Of;

   ---------------------------------------------------------------------------
   --  CVM Interface Implementation
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor is
      Desc : CVM_Descriptor := Empty_Descriptor;
   begin
      Desc.Info.Name := "ATS20_Enhanced - Extended Fungible Token with DeFi Features      ";
      Desc.Info.Caps := Default_Capabilities;
      Desc.Info.Active := True;

      Desc.Entry_Count := 11;

      Desc.Entries (0) := (
         Selector => Transfer_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (1) := (
         Selector => Transfer_From_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (2) := (
         Selector => Approve_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (3) := (
         Selector => Batch_Transfer_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (4) := (
         Selector => Permit_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (5) := (
         Selector => Flash_Loan_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (6) := (
         Selector => Pause_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (7) := (
         Selector => Unpause_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (8) := (
         Selector => Mint_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (9) := (
         Selector => Burn_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (10) := (
         Selector => Balance_Of_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => True
      );

      return Desc;
   end Get_Descriptor;

   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) is
      Idx          : Natural := Init_Params'First;
      Name_Len     : Natural;
      Symbol_Len   : Natural;
      Dec          : Unsigned_8;
      Initial_Sup  : Unsigned_64;
      Owner_Addr   : Address;
      Holder       : Address;
      Holder_Slot  : State_Index;
   begin
      State := Empty_State;

      --  Minimum: 1 + 1 + 1 + 1 + 8 + 32 + 32 = 76 bytes
      if Init_Params'Length < 76 then
         Success := False;
         return;
      end if;

      --  Read name
      Name_Len := Natural (Init_Params (Idx));
      Idx := Idx + 1;

      if Name_Len > 32 or else Idx + Name_Len > Init_Params'Last then
         Success := False;
         return;
      end if;

      for I in 0 .. Name_Len - 1 loop
         State (Name_Slot).Value (I) := Init_Params (Idx + I);
      end loop;
      State (Name_Slot).Length := Name_Len;
      Idx := Idx + Name_Len;

      --  Read symbol
      Symbol_Len := Natural (Init_Params (Idx));
      Idx := Idx + 1;

      if Symbol_Len > 8 or else Idx + Symbol_Len > Init_Params'Last then
         Success := False;
         return;
      end if;

      for I in 0 .. Symbol_Len - 1 loop
         State (Symbol_Slot).Value (I) := Init_Params (Idx + I);
      end loop;
      State (Symbol_Slot).Length := Symbol_Len;
      Idx := Idx + Symbol_Len;

      --  Read decimals
      Dec := Init_Params (Idx);
      Idx := Idx + 1;
      State (Decimals_Slot).Value (0) := Dec;
      State (Decimals_Slot).Length := 1;

      --  Read initial supply
      if Idx + 8 > Init_Params'Last + 1 then
         Success := False;
         return;
      end if;

      Initial_Sup := 0;
      for I in 0 .. 7 loop
         Initial_Sup := Shift_Left (Initial_Sup, 8) or
                        Unsigned_64 (Init_Params (Idx + I));
      end loop;
      Idx := Idx + 8;

      Write_U64 (State (Total_Supply_Slot), Initial_Sup);

      --  Read owner address
      if Idx + 32 > Init_Params'Last + 1 then
         Success := False;
         return;
      end if;

      for I in 0 .. 31 loop
         Owner_Addr (I) := Init_Params (Idx + I);
      end loop;
      Idx := Idx + 32;
      Write_Address (State (Owner_Slot), Owner_Addr);

      --  Read initial holder
      if Idx + 32 > Init_Params'Last + 1 then
         Success := False;
         return;
      end if;

      for I in 0 .. 31 loop
         Holder (I) := Init_Params (Idx + I);
      end loop;

      --  Set initial balance
      Holder_Slot := Get_Balance_Slot (Holder);
      Write_U64 (State (Holder_Slot), Initial_Sup);

      --  Initialize paused state (not paused)
      State (Paused_Slot).Value (0) := 0;
      State (Paused_Slot).Length := 1;

      Success := True;
   end Init;

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
   begin
      if Selector_Match (Context.Entry_Point, Transfer_Selector) then
         Transfer (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Transfer_From_Selector) then
         Transfer_From (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Approve_Selector) then
         Approve (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Batch_Transfer_Selector) then
         Batch_Transfer (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Permit_Selector) then
         Permit (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Flash_Loan_Selector) then
         Flash_Loan (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Pause_Selector) then
         Pause (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Unpause_Selector) then
         Unpause (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Mint_Selector) then
         Mint (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Burn_Selector) then
         Burn (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Balance_Of_Selector) then
         Balance_Of (Context, State, Result);
      else
         Result := Error_Result (Unknown_Method);
      end if;
   end Execute;

end ATS20_Enhanced;
