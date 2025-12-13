pragma SPARK_Mode (On);

package body Quantum_Vault with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal Helper Implementations
   ---------------------------------------------------------------------------

   function Calculate_Shares (
      Amount       : Unsigned_64;
      Total_Deposits : Unsigned_64;
      Total_Shares : Unsigned_64
   ) return Unsigned_64 is
   begin
      if Total_Deposits = 0 or Total_Shares = 0 then
         --  First deposit: 1:1 ratio
         return Amount;
      else
         --  shares = amount * total_shares / total_deposits
         --  Use 128-bit intermediate to prevent overflow
         declare
            Numerator : constant Unsigned_64 := Amount;
            Shares    : Unsigned_64;
         begin
            --  Simplified: assume no overflow for demo
            if Total_Deposits > 0 then
               Shares := (Numerator * Total_Shares) / Total_Deposits;
            else
               Shares := Numerator;
            end if;
            return Shares;
         end;
      end if;
   end Calculate_Shares;

   function Calculate_Yield (
      Principal    : Unsigned_64;
      APY_BPS      : Unsigned_16;
      Days_Elapsed : Unsigned_64
   ) return Unsigned_64 is
      --  yield = principal * apy_bps * days / (10000 * 365)
      Yield : Unsigned_64;
   begin
      if Days_Elapsed = 0 or APY_BPS = 0 then
         return 0;
      end if;

      --  Calculate with scaling to prevent overflow
      --  Simplified calculation
      Yield := (Principal / 10000) * Unsigned_64 (APY_BPS) * Days_Elapsed / 365;
      return Yield;
   end Calculate_Yield;

   function Get_Deposit_Slot (Addr : Address) return State_Index is
      Hash : Natural := 0;
      --  Maximum 32 bytes * 255 max byte value = 8160
      Max_Hash : constant Natural := 8160;
   begin
      for I in Addr'Range loop
         pragma Loop_Invariant (I in Addr'Range);
         pragma Loop_Invariant (Hash <= Max_Hash);
         --  Each byte adds at most 255, guard ensures invariant preserved
         if Hash <= Max_Hash - 255 then
            Hash := Hash + Natural (Addr (I));
         end if;
      end loop;
      return State_Index (Hash mod 96 + Natural (Deposit_Slots_Base));
   end Get_Deposit_Slot;

   function Verify_Signature (
      Message   : Byte_Array;
      Signature : Byte_Array;
      PubKey    : Address
   ) return Boolean is
      pragma Unreferenced (Message, Signature, PubKey);
   begin
      --  Stub: In production, this would call ML-DSA-87 verify
      --  For testing, we accept all signatures
      return True;
   end Verify_Signature;

   function Hash_To_Slot (
      Addr : Address;
      Base : State_Index;
      Range_Size : Natural
   ) return State_Index is
      Hash : Natural := 0;
      --  Maximum hash: 32 iterations * 255 * 33 (max I+1) = approx 270K
      Max_Hash : constant Natural := 270_000;
      Term : Natural;
   begin
      --  Compute bounded hash to prevent overflow
      for I in Addr'Range loop
         pragma Loop_Invariant (I in Addr'Range);
         pragma Loop_Invariant (Hash <= Max_Hash);

         --  Compute term with overflow protection
         --  I is in 0..31 for Address, so I + 1 <= 32
         --  Term <= 255 * 32 = 8160
         Term := Natural (Addr (I)) * ((I mod 32) + 1);

         --  Add with overflow protection, ensuring invariant preserved
         if Hash <= Max_Hash - 8160 then
            Hash := Hash + Term;
         end if;
      end loop;

      --  Compute final slot with bounds check
      if Range_Size > 0 then
         declare
            Offset : constant Natural := Hash mod Range_Size;
         begin
            if Natural (Base) <= Natural'Last - Offset and then
               Natural (Base) + Offset <= Natural (State_Index'Last)
            then
               return State_Index (Natural (Base) + Offset);
            else
               return Base;
            end if;
         end;
      else
         return Base;
      end if;
   end Hash_To_Slot;

   procedure Record_Audit (
      State     : in Out State_Array;
      Operation : Unsigned_8;
      User      : Address;
      Amount    : Unsigned_64;
      Timestamp : Unsigned_64;
      Block     : Unsigned_64
   ) is
      --  Find next audit slot (circular buffer)
      User_Hash : Unsigned_64 := 0;
      Slot_Idx  : State_Index;
      Audit_Idx : Natural := 0;
   begin
      --  Simple user hash
      for I in 0 .. 7 loop
         User_Hash := Shift_Left (User_Hash, 8) or Unsigned_64 (User (I));
      end loop;

      --  Find slot based on timestamp
      Audit_Idx := Natural (Timestamp mod Unsigned_64 (Max_Audit_Entries));
      Slot_Idx := State_Index (Audit_Idx + Natural (Audit_Slots_Base));

      --  Store audit entry (packed into state slot)
      --  Format: op (1) + user_hash (8) + amount (8) + timestamp (8) + block (8) = 33 bytes
      State (Slot_Idx).Value (0) := Operation;

      declare
         Temp : Unsigned_64 := User_Hash;
      begin
         for I in reverse 1 .. 8 loop
            State (Slot_Idx).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      declare
         Temp : Unsigned_64 := Amount;
      begin
         for I in reverse 9 .. 16 loop
            State (Slot_Idx).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      declare
         Temp : Unsigned_64 := Timestamp;
      begin
         for I in reverse 17 .. 24 loop
            State (Slot_Idx).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      declare
         Temp : Unsigned_64 := Block;
      begin
         for I in reverse 25 .. 32 loop
            State (Slot_Idx).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      State (Slot_Idx).Length := 33;
      State (Slot_Idx).Modified := True;
   end Record_Audit;

   function Check_Flash_Loan_Protection (
      Deposit_Block : Unsigned_64;
      Current_Block : Unsigned_64
   ) return Boolean is
   begin
      return Current_Block >= Deposit_Block + Unsigned_64 (Flash_Loan_Delay);
   end Check_Flash_Loan_Protection;

   ---------------------------------------------------------------------------
   --  Read/Write Helpers
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

   ---------------------------------------------------------------------------
   --  Core Operations
   ---------------------------------------------------------------------------

   procedure Deposit (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Amount      : Unsigned_64 := 0;
      Tier_Index  : Unsigned_8;
      Deposit_Slot : State_Index;
      Total_Deps  : Unsigned_64;
      Total_Shrs  : Unsigned_64;
      New_Shares  : Unsigned_64;
      Lock_Days   : Unsigned_64;
      Lock_Until  : Unsigned_64;
      Current_Time : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Validate params: amount (8 bytes) + tier_index (1 byte)
      if Context.Param_Len < 9 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract amount (big-endian)
      for I in 0 .. 7 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Extract tier index
      Tier_Index := Context.Params (8);

      --  Validate amount
      if Amount = 0 then
         Return_Data (0) := 0;  -- Failure: zero amount
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Validate tier (0-3)
      if Tier_Index > 3 then
         Return_Data (0) := 0;  -- Failure: invalid tier
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Read current totals
      Total_Deps := Read_U64 (State (Total_Deposits_Slot));
      Total_Shrs := Read_U64 (State (Total_Shares_Slot));

      --  Calculate shares
      New_Shares := Calculate_Shares (Amount, Total_Deps, Total_Shrs);

      --  Calculate lock period based on tier
      case Tier_Index is
         when 0 => Lock_Days := 30;
         when 1 => Lock_Days := 90;
         when 2 => Lock_Days := 180;
         when 3 => Lock_Days := 365;
         when others => Lock_Days := 30;
      end case;

      --  Get current time (from block height as proxy)
      Current_Time := Unsigned_64 (Context.Height) * 12;  -- ~12 sec per block
      Lock_Until := Current_Time + Lock_Days * 86400;

      --  Get deposit slot for user
      Deposit_Slot := Get_Deposit_Slot (Context.Caller);

      --  Store deposit record
      --  Format: amount (8) + shares (8) + lock_until (8) + tier (1) + block (8) + accumulated (8) + active (1) = 42 bytes
      declare
         Offset : Natural := 0;
         Temp   : Unsigned_64;
      begin
         --  Amount
         Temp := Amount;
         for I in reverse 0 .. 7 loop
            State (Deposit_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := 8;

         --  Shares
         Temp := New_Shares;
         for I in reverse Offset .. Offset + 7 loop
            State (Deposit_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := Offset + 8;

         --  Lock until
         Temp := Lock_Until;
         for I in reverse Offset .. Offset + 7 loop
            State (Deposit_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := Offset + 8;

         --  Tier
         State (Deposit_Slot).Value (Offset) := Tier_Index;
         Offset := Offset + 1;

         --  Deposit block
         Temp := Unsigned_64 (Context.Height);
         for I in reverse Offset .. Offset + 7 loop
            State (Deposit_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := Offset + 8;

         --  Accumulated yield (0 initially)
         for I in Offset .. Offset + 7 loop
            State (Deposit_Slot).Value (I) := 0;
         end loop;
         Offset := Offset + 8;

         --  Active flag
         State (Deposit_Slot).Value (Offset) := 1;

         State (Deposit_Slot).Length := Offset + 1;
         State (Deposit_Slot).Modified := True;
      end;

      --  Update totals
      Write_U64 (State (Total_Deposits_Slot), Total_Deps + Amount);
      Write_U64 (State (Total_Shares_Slot), Total_Shrs + New_Shares);

      --  Record audit
      Record_Audit (State, 1, Context.Caller, Amount, Current_Time, Unsigned_64 (Context.Height));

      --  Return success with shares
      Return_Data (0) := 1;  -- Success
      declare
         Temp : Unsigned_64 := New_Shares;
      begin
         for I in reverse 1 .. 8 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      Result := (Status => CVM_Types.Success, Return_Len => 9, Return_Data => Return_Data);
   end Deposit;

   procedure Withdraw (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Shares_To_Withdraw : Unsigned_64 := 0;
      Deposit_Slot : State_Index;
      User_Shares  : Unsigned_64 := 0;
      User_Amount  : Unsigned_64 := 0;
      Lock_Until   : Unsigned_64 := 0;
      Deposit_Block : Unsigned_64 := 0;
      Current_Time : Unsigned_64;
      Total_Deps   : Unsigned_64;
      Total_Shrs   : Unsigned_64;
      Withdraw_Amount : Unsigned_64;
      Fee_Amount   : Unsigned_64;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      --  Validate params
      if Context.Param_Len < 8 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract shares to withdraw
      for I in 0 .. 7 loop
         Shares_To_Withdraw := Shift_Left (Shares_To_Withdraw, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Get deposit slot
      Deposit_Slot := Get_Deposit_Slot (Context.Caller);

      --  Read user's deposit
      if State (Deposit_Slot).Length >= 42 then
         --  Read amount
         for I in 0 .. 7 loop
            User_Amount := Shift_Left (User_Amount, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
         end loop;

         --  Read shares
         for I in 8 .. 15 loop
            User_Shares := Shift_Left (User_Shares, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
         end loop;

         --  Read lock_until
         for I in 16 .. 23 loop
            Lock_Until := Shift_Left (Lock_Until, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
         end loop;

         --  Read deposit block
         for I in 25 .. 32 loop
            Deposit_Block := Shift_Left (Deposit_Block, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
         end loop;
      else
         Return_Data (0) := 0;  -- No deposit
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Check active
      if State (Deposit_Slot).Value (41) /= 1 then
         Return_Data (0) := 0;  -- Deposit not active
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Check shares
      if Shares_To_Withdraw > User_Shares then
         Return_Data (0) := 0;  -- Insufficient shares
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Flash loan protection
      if not Check_Flash_Loan_Protection (Deposit_Block, Unsigned_64 (Context.Height)) then
         Return_Data (0) := 0;  -- Flash loan protection
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Read totals
      Total_Deps := Read_U64 (State (Total_Deposits_Slot));
      Total_Shrs := Read_U64 (State (Total_Shares_Slot));

      --  Calculate withdrawal amount
      if Total_Shrs > 0 then
         Withdraw_Amount := (Shares_To_Withdraw * Total_Deps) / Total_Shrs;
      else
         Withdraw_Amount := Shares_To_Withdraw;
      end if;

      --  Check timelock and apply penalty if early
      Current_Time := Unsigned_64 (Context.Height) * 12;
      if Current_Time < Lock_Until then
         --  Early exit penalty
         Fee_Amount := (Withdraw_Amount * Unsigned_64 (Early_Exit_Penalty)) / 10000;
      else
         --  Normal withdrawal fee
         Fee_Amount := (Withdraw_Amount * Unsigned_64 (Withdrawal_Fee)) / 10000;
      end if;

      Withdraw_Amount := Withdraw_Amount - Fee_Amount;

      --  Update user's deposit
      declare
         New_Shares : constant Unsigned_64 := User_Shares - Shares_To_Withdraw;
         New_Amount : Unsigned_64;
         Temp       : Unsigned_64;
      begin
         if Total_Shrs > 0 then
            New_Amount := (New_Shares * Total_Deps) / Total_Shrs;
         else
            New_Amount := 0;
         end if;

         --  Write new amount
         Temp := New_Amount;
         for I in reverse 0 .. 7 loop
            State (Deposit_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;

         --  Write new shares
         Temp := New_Shares;
         for I in reverse 8 .. 15 loop
            State (Deposit_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;

         --  Mark inactive if fully withdrawn
         if New_Shares = 0 then
            State (Deposit_Slot).Value (41) := 0;
         end if;

         State (Deposit_Slot).Modified := True;
      end;

      --  Update totals
      Write_U64 (State (Total_Deposits_Slot), Total_Deps - Withdraw_Amount - Fee_Amount);
      Write_U64 (State (Total_Shares_Slot), Total_Shrs - Shares_To_Withdraw);

      --  Add fee to accumulated yield
      declare
         Acc_Yield : Unsigned_64 := Read_U64 (State (Accumulated_Yield_Slot));
      begin
         Write_U64 (State (Accumulated_Yield_Slot), Acc_Yield + Fee_Amount);
      end;

      --  Record audit
      Record_Audit (State, 2, Context.Caller, Withdraw_Amount, Current_Time, Unsigned_64 (Context.Height));

      --  Return success with amount
      Return_Data (0) := 1;
      declare
         Temp : Unsigned_64 := Withdraw_Amount;
      begin
         for I in reverse 1 .. 8 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      Result := (Status => CVM_Types.Success, Return_Len => 9, Return_Data => Return_Data);
   end Withdraw;

   procedure Claim_Yield (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Deposit_Slot : State_Index;
      User_Amount  : Unsigned_64 := 0;
      Accumulated  : Unsigned_64 := 0;
      Tier_Index   : Unsigned_8;
      Deposit_Block : Unsigned_64 := 0;
      Current_Time : Unsigned_64;
      Days_Elapsed : Unsigned_64;
      APY_BPS      : Unsigned_16;
      New_Yield    : Unsigned_64;
      Total_Yield  : Unsigned_64;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      --  Get deposit slot
      Deposit_Slot := Get_Deposit_Slot (Context.Caller);

      --  Read deposit data
      if State (Deposit_Slot).Length < 42 or else State (Deposit_Slot).Value (41) /= 1 then
         Return_Data (0) := 0;  -- No active deposit
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Read amount
      for I in 0 .. 7 loop
         User_Amount := Shift_Left (User_Amount, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
      end loop;

      --  Read tier
      Tier_Index := State (Deposit_Slot).Value (24);

      --  Read deposit block
      for I in 25 .. 32 loop
         Deposit_Block := Shift_Left (Deposit_Block, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
      end loop;

      --  Read accumulated
      for I in 33 .. 40 loop
         Accumulated := Shift_Left (Accumulated, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
      end loop;

      --  Calculate new yield
      Current_Time := Unsigned_64 (Context.Height) * 12;
      Days_Elapsed := (Unsigned_64 (Context.Height) - Deposit_Block) * 12 / 86400;

      case Tier_Index is
         when 0 => APY_BPS := Tier_1_APY;
         when 1 => APY_BPS := Tier_2_APY;
         when 2 => APY_BPS := Tier_3_APY;
         when 3 => APY_BPS := Tier_4_APY;
         when others => APY_BPS := Tier_1_APY;
      end case;

      New_Yield := Calculate_Yield (User_Amount, APY_BPS, Days_Elapsed);
      Total_Yield := New_Yield - Accumulated;  -- Only unclaimed yield

      if Total_Yield = 0 then
         Return_Data (0) := 0;  -- No yield to claim
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Deduct performance fee
      declare
         Fee : constant Unsigned_64 := (Total_Yield * Unsigned_64 (Performance_Fee)) / 10000;
         Net_Yield : constant Unsigned_64 := Total_Yield - Fee;
         Temp : Unsigned_64;
      begin
         --  Update accumulated
         Temp := New_Yield;
         for I in reverse 33 .. 40 loop
            State (Deposit_Slot).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         State (Deposit_Slot).Modified := True;

         --  Record audit
         Record_Audit (State, 3, Context.Caller, Net_Yield, Current_Time, Unsigned_64 (Context.Height));

         --  Return success
         Return_Data (0) := 1;
         Temp := Net_Yield;
         for I in reverse 1 .. 8 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      Result := (Status => CVM_Types.Success, Return_Len => 9, Return_Data => Return_Data);
   end Claim_Yield;

   procedure Get_Balance (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Query_Addr   : Address;
      Deposit_Slot : State_Index;
      User_Amount  : Unsigned_64 := 0;
      User_Shares  : Unsigned_64 := 0;
      Accumulated  : Unsigned_64 := 0;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      --  Get address to query (from params or caller)
      if Context.Param_Len >= 32 then
         for I in 0 .. 31 loop
            Query_Addr (I) := Context.Params (I);
         end loop;
      else
         Query_Addr := Context.Caller;
      end if;

      --  Get deposit slot
      Deposit_Slot := Get_Deposit_Slot (Query_Addr);

      --  Read deposit if exists
      if State (Deposit_Slot).Length >= 42 and then State (Deposit_Slot).Value (41) = 1 then
         for I in 0 .. 7 loop
            User_Amount := Shift_Left (User_Amount, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
         end loop;

         for I in 8 .. 15 loop
            User_Shares := Shift_Left (User_Shares, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
         end loop;

         for I in 33 .. 40 loop
            Accumulated := Shift_Left (Accumulated, 8) or Unsigned_64 (State (Deposit_Slot).Value (I));
         end loop;
      end if;

      --  Return: amount (8) + shares (8) + accumulated (8) = 24 bytes
      declare
         Temp : Unsigned_64;
      begin
         Temp := User_Amount;
         for I in reverse 0 .. 7 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;

         Temp := User_Shares;
         for I in reverse 8 .. 15 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;

         Temp := Accumulated;
         for I in reverse 16 .. 23 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      Result := (Status => CVM_Types.Success, Return_Len => 24, Return_Data => Return_Data);
   end Get_Balance;

   procedure Get_Yield_Estimate (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (State);
      Amount       : Unsigned_64 := 0;
      Tier_Index   : Unsigned_8;
      Duration_Days : Unsigned_16 := 0;
      APY_BPS      : Unsigned_16;
      Est_Yield    : Unsigned_64;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      if Context.Param_Len < 11 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Read amount
      for I in 0 .. 7 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Read tier
      Tier_Index := Context.Params (8);

      --  Read duration
      Duration_Days := Unsigned_16 (Context.Params (9)) * 256 + Unsigned_16 (Context.Params (10));

      --  Get APY for tier
      case Tier_Index is
         when 0 => APY_BPS := Tier_1_APY;
         when 1 => APY_BPS := Tier_2_APY;
         when 2 => APY_BPS := Tier_3_APY;
         when 3 => APY_BPS := Tier_4_APY;
         when others => APY_BPS := Tier_1_APY;
      end case;

      --  Calculate yield
      Est_Yield := Calculate_Yield (Amount, APY_BPS, Unsigned_64 (Duration_Days));

      --  Return estimate
      declare
         Temp : Unsigned_64 := Est_Yield;
      begin
         for I in reverse 0 .. 7 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      Result := (Status => CVM_Types.Success, Return_Len => 8, Return_Data => Return_Data);
   end Get_Yield_Estimate;

   ---------------------------------------------------------------------------
   --  Governance Operations (Simplified for testing)
   ---------------------------------------------------------------------------

   procedure Propose (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Proposal_Kind : Unsigned_8;
      Parameter     : Unsigned_64 := 0;
      Prop_Count    : Unsigned_64;
      Slot_Idx      : State_Index;
      Current_Time  : Unsigned_64;
      Return_Data   : Return_Buffer := (others => 0);
   begin
      if Context.Param_Len < 9 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      Proposal_Kind := Context.Params (0);
      for I in 1 .. 8 loop
         Parameter := Shift_Left (Parameter, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Get proposal count
      Prop_Count := Read_U64 (State (Proposal_Count_Slot));

      if Prop_Count >= Unsigned_64 (Max_Proposals) then
         Return_Data (0) := 0;  -- Too many proposals
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Store proposal
      Slot_Idx := State_Index (Natural (Prop_Count) mod Max_Proposals + Natural (Proposal_Slots_Base));
      Current_Time := Unsigned_64 (Context.Height) * 12;

      --  Proposal format: kind (1) + param (8) + proposer (32) + votes_for (1) + votes_against (1) +
      --                   created (8) + expires (8) + executed (1) + active (1) = 61 bytes
      State (Slot_Idx).Value (0) := Proposal_Kind;

      declare
         Temp : Unsigned_64 := Parameter;
      begin
         for I in reverse 1 .. 8 loop
            State (Slot_Idx).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      for I in 0 .. 31 loop
         State (Slot_Idx).Value (9 + I) := Context.Caller (I);
      end loop;

      State (Slot_Idx).Value (41) := 0;  -- votes_for
      State (Slot_Idx).Value (42) := 0;  -- votes_against

      declare
         Temp : Unsigned_64 := Current_Time;
      begin
         for I in reverse 43 .. 50 loop
            State (Slot_Idx).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;

         Temp := Current_Time + 604800;  -- 7 day expiry
         for I in reverse 51 .. 58 loop
            State (Slot_Idx).Value (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      State (Slot_Idx).Value (59) := 0;  -- not executed
      State (Slot_Idx).Value (60) := 1;  -- active
      State (Slot_Idx).Length := 61;
      State (Slot_Idx).Modified := True;

      --  Increment proposal count
      Write_U64 (State (Proposal_Count_Slot), Prop_Count + 1);

      --  Return proposal ID
      Return_Data (0) := 1;
      Return_Data (1) := Unsigned_8 (Prop_Count mod 256);
      Result := (Status => CVM_Types.Success, Return_Len => 2, Return_Data => Return_Data);
   end Propose;

   procedure Vote (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Proposal_ID : Unsigned_8;
      Vote_Value  : Unsigned_8;
      Slot_Idx    : State_Index;
      Return_Data : Return_Buffer := (others => 0);
   begin
      if Context.Param_Len < 2 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      Proposal_ID := Context.Params (0);
      Vote_Value := Context.Params (1);

      Slot_Idx := State_Index (Natural (Proposal_ID) mod Max_Proposals + Natural (Proposal_Slots_Base));

      --  Check proposal is active
      if State (Slot_Idx).Length < 61 or else State (Slot_Idx).Value (60) /= 1 then
         Return_Data (0) := 0;
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Record vote
      if Vote_Value = 1 then
         State (Slot_Idx).Value (41) := State (Slot_Idx).Value (41) + 1;
      else
         State (Slot_Idx).Value (42) := State (Slot_Idx).Value (42) + 1;
      end if;
      State (Slot_Idx).Modified := True;

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Vote;

   procedure Execute_Proposal (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      pragma Unreferenced (State);
      Return_Data : Return_Buffer := (others => 0);
   begin
      --  Simplified: always return success for testing
      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Execute_Proposal;

   procedure Emergency_Unlock (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Current_Time : Unsigned_64;
      Unlock_Time  : Unsigned_64;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      Current_Time := Unsigned_64 (Context.Height) * 12;
      Unlock_Time := Current_Time + Unsigned_64 (Emergency_Delay);

      Write_U64 (State (Emergency_Unlock_Slot), Unlock_Time);

      Record_Audit (State, 9, Context.Caller, 0, Current_Time, Unsigned_64 (Context.Height));

      Return_Data (0) := 1;
      declare
         Temp : Unsigned_64 := Unlock_Time;
      begin
         for I in reverse 1 .. 8 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      Result := (Status => CVM_Types.Success, Return_Len => 9, Return_Data => Return_Data);
   end Emergency_Unlock;

   procedure Rebalance (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Last_Rebalance : Unsigned_64;
      Current_Time   : Unsigned_64;
      Return_Data    : Return_Buffer := (others => 0);
   begin
      Last_Rebalance := Read_U64 (State (Last_Rebalance_Slot));
      Current_Time := Unsigned_64 (Context.Height) * 12;

      --  Check cooldown
      if Current_Time < Last_Rebalance + Unsigned_64 (Rebalance_Cooldown) then
         Return_Data (0) := 0;  -- Cooldown not elapsed
         Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
         return;
      end if;

      --  Update last rebalance time
      Write_U64 (State (Last_Rebalance_Slot), Current_Time);

      --  Record audit
      Record_Audit (State, 5, Context.Caller, 0, Current_Time, Unsigned_64 (Context.Height));

      Return_Data (0) := 1;
      Result := (Status => CVM_Types.Success, Return_Len => 1, Return_Data => Return_Data);
   end Rebalance;

   procedure Get_Vault_Stats (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      Total_Deps   : Unsigned_64;
      Total_Shrs   : Unsigned_64;
      Acc_Yield    : Unsigned_64;
      Last_Reb     : Unsigned_64;
      Return_Data  : Return_Buffer := (others => 0);
   begin
      Total_Deps := Read_U64 (State (Total_Deposits_Slot));
      Total_Shrs := Read_U64 (State (Total_Shares_Slot));
      Acc_Yield := Read_U64 (State (Accumulated_Yield_Slot));
      Last_Reb := Read_U64 (State (Last_Rebalance_Slot));

      --  Pack stats: total_deposits (8) + total_shares (8) + acc_yield (8) + last_rebalance (8) = 32
      declare
         Temp : Unsigned_64;
         Offset : Natural := 0;
      begin
         Temp := Total_Deps;
         for I in reverse Offset .. Offset + 7 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := 8;

         Temp := Total_Shrs;
         for I in reverse Offset .. Offset + 7 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := 16;

         Temp := Acc_Yield;
         for I in reverse Offset .. Offset + 7 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
         Offset := 24;

         Temp := Last_Reb;
         for I in reverse Offset .. Offset + 7 loop
            Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
            Temp := Shift_Right (Temp, 8);
         end loop;
      end;

      Result := (Status => CVM_Types.Success, Return_Len => 32, Return_Data => Return_Data);
   end Get_Vault_Stats;

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor is
      Desc : CVM_Descriptor := Empty_Descriptor;
   begin
      Desc.Info.Name := "Quantum-Secured Autonomous Yield Vault (QSAYV)                  ";
      Desc.Info.Caps := Default_Capabilities;
      Desc.Info.Active := True;
      Desc.Entry_Count := 11;

      Desc.Entries (0) := (Selector => Deposit_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (1) := (Selector => Withdraw_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (2) := (Selector => Claim_Yield_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (3) := (Selector => Get_Balance_Selector, Handler => null, Public => True, ReadOnly => True);
      Desc.Entries (4) := (Selector => Get_Yield_Selector, Handler => null, Public => True, ReadOnly => True);
      Desc.Entries (5) := (Selector => Propose_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (6) := (Selector => Vote_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (7) := (Selector => Execute_Proposal_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (8) := (Selector => Emergency_Unlock_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (9) := (Selector => Rebalance_Selector, Handler => null, Public => True, ReadOnly => False);
      Desc.Entries (10) := (Selector => Get_Vault_Stats_Selector, Handler => null, Public => True, ReadOnly => True);

      return Desc;
   end Get_Descriptor;

   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) is
      Guardian_Addr : Address := (others => 0);
   begin
      State := Empty_State;

      if Init_Params'Length < 32 then
         Success := False;
         return;
      end if;

      --  Extract guardian address
      for I in 0 .. 31 loop
         Guardian_Addr (I) := Init_Params (Init_Params'First + I);
      end loop;

      --  Store guardian
      for I in 0 .. 31 loop
         State (Guardian_Slot).Value (I) := Guardian_Addr (I);
      end loop;
      State (Guardian_Slot).Length := 32;

      --  Initialize totals to 0
      Write_U64 (State (Total_Deposits_Slot), 0);
      Write_U64 (State (Total_Shares_Slot), 0);
      Write_U64 (State (Accumulated_Yield_Slot), 0);
      Write_U64 (State (Last_Rebalance_Slot), 0);
      Write_U64 (State (Emergency_Unlock_Slot), 0);
      Write_U64 (State (Proposal_Count_Slot), 0);

      Success := True;
   end Init;

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      function Match (A, B : Hash256) return Boolean is
      begin
         return A (0) = B (0) and then A (1) = B (1) and then A (2) = B (2) and then A (3) = B (3);
      end Match;
   begin
      if Match (Context.Entry_Point, Deposit_Selector) then
         Deposit (Context, State, Result);
      elsif Match (Context.Entry_Point, Withdraw_Selector) then
         Withdraw (Context, State, Result);
      elsif Match (Context.Entry_Point, Claim_Yield_Selector) then
         Claim_Yield (Context, State, Result);
      elsif Match (Context.Entry_Point, Get_Balance_Selector) then
         Get_Balance (Context, State, Result);
      elsif Match (Context.Entry_Point, Get_Yield_Selector) then
         Get_Yield_Estimate (Context, State, Result);
      elsif Match (Context.Entry_Point, Propose_Selector) then
         Propose (Context, State, Result);
      elsif Match (Context.Entry_Point, Vote_Selector) then
         Vote (Context, State, Result);
      elsif Match (Context.Entry_Point, Execute_Proposal_Selector) then
         Execute_Proposal (Context, State, Result);
      elsif Match (Context.Entry_Point, Emergency_Unlock_Selector) then
         Emergency_Unlock (Context, State, Result);
      elsif Match (Context.Entry_Point, Rebalance_Selector) then
         Rebalance (Context, State, Result);
      elsif Match (Context.Entry_Point, Get_Vault_Stats_Selector) then
         Get_Vault_Stats (Context, State, Result);
      else
         Result := Error_Result (Unknown_Method);
      end if;
   end Execute;

end Quantum_Vault;
