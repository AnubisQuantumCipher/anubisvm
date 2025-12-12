pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body ATS20_Token with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper Functions Implementation
   ---------------------------------------------------------------------------

   function Get_Balance_Slot (Addr : Address) return State_Index is
      Hash : Natural := 0;
   begin
      --  Simple hash: sum of address bytes
      for I in Addr'Range loop
         Hash := Hash + Natural (Addr (I));
      end loop;

      --  Map to balance slot range
      return State_Index (Hash mod Balance_Slot_Count + Balance_Slot_Base);
   end Get_Balance_Slot;

   function Get_Allowance_Slot (
      Owner   : Address;
      Spender : Address
   ) return State_Index is
      Hash : Natural := 0;
   begin
      --  Simple hash: sum of owner and spender bytes with offset
      for I in Owner'Range loop
         Hash := Hash + Natural (Owner (I));
      end loop;

      for I in Spender'Range loop
         Hash := Hash + Natural (Spender (I)) * 3;  -- Different weight
      end loop;

      --  Map to allowance slot range
      return State_Index (Hash mod Allowance_Slot_Count + Allowance_Slot_Base);
   end Get_Allowance_Slot;

   function Read_U64 (Slot : State_Slot) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      if Slot.Length >= 8 then
         --  Big-endian read
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
      --  Big-endian write
      for I in reverse 0 .. 7 loop
         Slot.Value (I) := Unsigned_8 (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
      Slot.Length := 8;
      Slot.Modified := True;
   end Write_U64;

   function Selector_Match (A, B : Hash256) return Boolean is
   begin
      --  Compare first 4 bytes only (method selector)
      return A (0) = B (0) and then
             A (1) = B (1) and then
             A (2) = B (2) and then
             A (3) = B (3);
   end Selector_Match;

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
      --  Validate parameter length: 32 (address) + 8 (amount) = 40
      if Context.Param_Len /= 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract recipient address
      for I in 0 .. 31 loop
         To_Addr (I) := Context.Params (I);
      end loop;

      --  Extract amount (big-endian)
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
         Return_Data (0) := 0;  -- Failure
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Check for overflow on recipient
      if To_Bal > Unsigned_64'Last - Amount then
         Return_Data (0) := 0;  -- Failure
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

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Transfer;

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
      --  Validate parameter length: 32 bytes (address)
      if Context.Param_Len /= 32 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract account address
      for I in 0 .. 31 loop
         Account (I) := Context.Params (I);
      end loop;

      --  Get balance slot and read
      Slot_Idx := Get_Balance_Slot (Account);
      Balance := Read_U64 (State (Slot_Idx));

      --  Encode balance as big-endian in return data
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
      --  Validate parameter length: 32 (address) + 8 (amount) = 40
      if Context.Param_Len /= 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract spender address
      for I in 0 .. 31 loop
         Spender (I) := Context.Params (I);
      end loop;

      --  Extract amount (big-endian)
      Amount := 0;
      for I in 32 .. 39 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Get allowance slot and write
      Slot_Idx := Get_Allowance_Slot (Context.Caller, Spender);
      Write_U64 (State (Slot_Idx), Amount);

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Approve;

   procedure Allowance (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Owner       : Address;
      Spender     : Address;
      Slot_Idx    : State_Index;
      Allow_Val   : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
      Temp        : Unsigned_64;
   begin
      --  Validate parameter length: 32 + 32 = 64 bytes
      if Context.Param_Len /= 64 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract owner address
      for I in 0 .. 31 loop
         Owner (I) := Context.Params (I);
      end loop;

      --  Extract spender address
      for I in 0 .. 31 loop
         Spender (I) := Context.Params (I + 32);
      end loop;

      --  Get allowance
      Slot_Idx := Get_Allowance_Slot (Owner, Spender);
      Allow_Val := Read_U64 (State (Slot_Idx));

      --  Encode as big-endian
      Temp := Allow_Val;
      for I in reverse 0 .. 7 loop
         Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;

      Result := (
         Status      => Success,
         Return_Len  => 8,
         Return_Data => Return_Data
      );
   end Allowance;

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
      --  Validate parameter length: 32 + 32 + 8 = 72 bytes
      if Context.Param_Len /= 72 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract from address
      for I in 0 .. 31 loop
         From_Addr (I) := Context.Params (I);
      end loop;

      --  Extract to address
      for I in 0 .. 31 loop
         To_Addr (I) := Context.Params (I + 32);
      end loop;

      --  Extract amount (big-endian)
      Amount := 0;
      for I in 64 .. 71 loop
         Amount := Shift_Left (Amount, 8) or Unsigned_64 (Context.Params (I));
      end loop;

      --  Check allowance
      Allow_Slot := Get_Allowance_Slot (From_Addr, Context.Caller);
      Allowance_Val := Read_U64 (State (Allow_Slot));

      if Allowance_Val < Amount then
         Return_Data (0) := 0;  -- Insufficient allowance
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
         Return_Data (0) := 0;  -- Insufficient balance
         Result := (
            Status      => Success,
            Return_Len  => 1,
            Return_Data => Return_Data
         );
         return;
      end if;

      --  Check for overflow
      if To_Bal > Unsigned_64'Last - Amount then
         Return_Data (0) := 0;  -- Overflow
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

      --  Return success
      Return_Data (0) := 1;
      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Transfer_From;

   procedure Total_Supply (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      Supply      : Unsigned_64;
      Return_Data : Return_Buffer := (others => 0);
      Temp        : Unsigned_64;
   begin
      Supply := Read_U64 (State (Total_Supply_Slot));

      --  Encode as big-endian
      Temp := Supply;
      for I in reverse 0 .. 7 loop
         Return_Data (I) := Unsigned_8 (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;

      Result := (
         Status      => Success,
         Return_Len  => 8,
         Return_Data => Return_Data
      );
   end Total_Supply;

   procedure Name (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      Return_Data : Return_Buffer := (others => 0);
      Len         : Natural;
   begin
      Len := State (Name_Slot).Length;
      if Len > 32 then
         Len := 32;
      end if;

      for I in 0 .. Len - 1 loop
         Return_Data (I) := State (Name_Slot).Value (I);
      end loop;

      Result := (
         Status      => Success,
         Return_Len  => Len,
         Return_Data => Return_Data
      );
   end Name;

   procedure Symbol (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      Return_Data : Return_Buffer := (others => 0);
      Len         : Natural;
   begin
      Len := State (Symbol_Slot).Length;
      if Len > 8 then
         Len := 8;
      end if;

      for I in 0 .. Len - 1 loop
         Return_Data (I) := State (Symbol_Slot).Value (I);
      end loop;

      Result := (
         Status      => Success,
         Return_Len  => Len,
         Return_Data => Return_Data
      );
   end Symbol;

   procedure Decimals (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      Return_Data : Return_Buffer := (others => 0);
   begin
      if State (Decimals_Slot).Length > 0 then
         Return_Data (0) := State (Decimals_Slot).Value (0);
      else
         Return_Data (0) := 18;  -- Default decimals
      end if;

      Result := (
         Status      => Success,
         Return_Len  => 1,
         Return_Data => Return_Data
      );
   end Decimals;

   ---------------------------------------------------------------------------
   --  CVM Interface Implementation
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor is
      Desc : CVM_Descriptor := Empty_Descriptor;
   begin
      --  Set CVM info (name must be exactly 64 characters)
      Desc.Info.Name := "ATS20_Token - Standard Fungible Token                           ";
      Desc.Info.Caps := Default_Capabilities;
      Desc.Info.Active := True;

      --  Register entry points
      Desc.Entry_Count := 9;

      Desc.Entries (0) := (
         Selector => Transfer_Selector,
         Handler  => null,  -- Filled at runtime
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (1) := (
         Selector => Balance_Of_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => True
      );

      Desc.Entries (2) := (
         Selector => Approve_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (3) := (
         Selector => Allowance_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => True
      );

      Desc.Entries (4) := (
         Selector => Transfer_From_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => False
      );

      Desc.Entries (5) := (
         Selector => Total_Supply_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => True
      );

      Desc.Entries (6) := (
         Selector => Name_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => True
      );

      Desc.Entries (7) := (
         Selector => Symbol_Selector,
         Handler  => null,
         Public   => True,
         ReadOnly => True
      );

      Desc.Entries (8) := (
         Selector => Decimals_Selector,
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
      Holder       : Address;
      Holder_Slot  : State_Index;
   begin
      --  Initialize all slots to empty
      State := Empty_State;

      --  Minimum params: 1 + 1 + 1 + 1 + 8 + 32 = 44 bytes
      if Init_Params'Length < 44 then
         Success := False;
         return;
      end if;

      --  Read name length
      Name_Len := Natural (Init_Params (Idx));
      Idx := Idx + 1;

      if Name_Len > 32 or else Idx + Name_Len > Init_Params'Last then
         Success := False;
         return;
      end if;

      --  Copy name to state
      for I in 0 .. Name_Len - 1 loop
         State (Name_Slot).Value (I) := Init_Params (Idx + I);
      end loop;
      State (Name_Slot).Length := Name_Len;
      Idx := Idx + Name_Len;

      --  Read symbol length
      Symbol_Len := Natural (Init_Params (Idx));
      Idx := Idx + 1;

      if Symbol_Len > 8 or else Idx + Symbol_Len > Init_Params'Last then
         Success := False;
         return;
      end if;

      --  Copy symbol to state
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

      --  Read initial supply (big-endian)
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

      --  Write total supply
      Write_U64 (State (Total_Supply_Slot), Initial_Sup);

      --  Read initial holder address
      if Idx + 32 > Init_Params'Last + 1 then
         Success := False;
         return;
      end if;

      for I in 0 .. 31 loop
         Holder (I) := Init_Params (Idx + I);
      end loop;

      --  Set initial holder balance
      Holder_Slot := Get_Balance_Slot (Holder);
      Write_U64 (State (Holder_Slot), Initial_Sup);

      Success := True;
   end Init;

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
   begin
      --  Dispatch based on method selector
      if Selector_Match (Context.Entry_Point, Transfer_Selector) then
         Transfer (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Balance_Of_Selector) then
         Balance_Of (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Approve_Selector) then
         Approve (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Allowance_Selector) then
         Allowance (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Transfer_From_Selector) then
         Transfer_From (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Total_Supply_Selector) then
         Total_Supply (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Name_Selector) then
         Name (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Symbol_Selector) then
         Symbol (Context, State, Result);
      elsif Selector_Match (Context.Entry_Point, Decimals_Selector) then
         Decimals (Context, State, Result);
      else
         Result := Error_Result (Unknown_Method);
      end if;
   end Execute;

end ATS20_Token;
