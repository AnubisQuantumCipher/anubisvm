-------------------------------------------------------------------------------
--  ANUBIS Token Core Implementation Body
--  Native Token of the AnubisVM/KHEPRI Ecosystem
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Token with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Token Operations
   ---------------------------------------------------------------------------

   procedure Transfer (
      From           : in out Account_State;
      To             : in Out Account_State;
      Amount         : Token_Amount;
      Result         : out Transfer_Result
   ) is
   begin
      --  Check sender not frozen
      if From.Frozen then
         Result := Sender_Frozen;
         return;
      end if;

      --  Check recipient not frozen
      if To.Frozen then
         Result := Recipient_Frozen;
         return;
      end if;

      --  Check sufficient balance
      if From.Balance < Amount then
         Result := Insufficient_Balance;
         return;
      end if;

      --  Check for overflow
      if To.Balance > Token_Amount'Last - Amount then
         Result := Amount_Overflow;
         return;
      end if;

      --  Execute transfer
      From.Balance := From.Balance - Amount;
      To.Balance := To.Balance + Amount;
      Result := Success;
   end Transfer;

   procedure Burn (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Amount         : Token_Amount;
      Result         : out Burn_Result
   ) is
   begin
      --  Check sufficient balance
      if Account.Balance < Amount then
         Result := Insufficient_Balance;
         return;
      end if;

      --  Execute burn
      Account.Balance := Account.Balance - Amount;
      State.Total_Burned := State.Total_Burned + Amount;
      State.Total_Supply := State.Total_Supply - Amount;
      Result := Burned;
   end Burn;

   procedure Genesis_Mint (
      State          : in Out Token_State;
      Category       : Allocation_Category;
      Recipient      : in Out Account_State;
      Amount         : Token_Amount;
      Success        : out Boolean
   ) is
      Remaining : Token_Amount;
   begin
      --  Verify genesis block
      if State.Current_Block /= 0 then
         Success := False;
         return;
      end if;

      --  Get remaining allocation for category
      Remaining := Get_Remaining (State, Category);

      if Amount > Remaining then
         Success := False;
         return;
      end if;

      --  Check for overflow
      if Recipient.Balance > Token_Amount'Last - Amount then
         Success := False;
         return;
      end if;

      --  Credit recipient
      Recipient.Balance := Recipient.Balance + Amount;

      --  Update allocation tracking
      case Category is
         when Solo_Builder =>
            State.Builder_Vested := State.Builder_Vested + Amount;
         when Protocol_Treasury =>
            State.Treasury_Released := State.Treasury_Released + Amount;
         when Genesis_Validators =>
            State.Validators_Earned := State.Validators_Earned + Amount;
         when Genesis_Provers =>
            State.Provers_Earned := State.Provers_Earned + Amount;
         when Developer_Ecosystem =>
            State.Developers_Earned := State.Developers_Earned + Amount;
         when Quantum_Insurance =>
            State.Insurance_Locked := State.Insurance_Locked + Amount;
         when Bug_Bounties =>
            State.Bounties_Paid := State.Bounties_Paid + Amount;
      end case;

      Success := True;
   end Genesis_Mint;

   function Balance_Of (Account : Account_State) return Token_Amount is
   begin
      return Account.Balance;
   end Balance_Of;

   function Get_Total_Supply (State : Token_State) return Token_Amount is
   begin
      return State.Total_Supply;
   end Get_Total_Supply;

   function Get_Total_Burned (State : Token_State) return Token_Amount is
   begin
      return State.Total_Burned;
   end Get_Total_Burned;

   ---------------------------------------------------------------------------
   --  Certification Operations
   ---------------------------------------------------------------------------

   procedure Apply_Certification (
      Account        : in Out Account_State;
      Level          : Certification_Level;
      Deposit        : Token_Amount;
      Success        : out Boolean
   ) is
      Required : constant Token_Amount := Get_Required_Deposit (Level);
   begin
      --  Must not have existing certification
      if Account.Cert_Level /= None then
         Success := False;
         return;
      end if;

      --  Check deposit meets requirement
      if Deposit < Required then
         Success := False;
         return;
      end if;

      --  Check sufficient balance
      if Account.Balance < Deposit then
         Success := False;
         return;
      end if;

      --  Lock deposit
      Account.Balance := Account.Balance - Deposit;
      Account.Cert_Deposit := Deposit;
      Account.Cert_Level := Level;
      Success := True;
   end Apply_Certification;

   procedure Upgrade_Certification (
      Account        : in Out Account_State;
      New_Level      : Certification_Level;
      Additional_Deposit : Token_Amount;
      Success        : out Boolean
   ) is
      Required_New : constant Token_Amount := Get_Required_Deposit (New_Level);
      Total_Deposit : Token_Amount;
   begin
      --  Check new level is higher
      if New_Level <= Account.Cert_Level then
         Success := False;
         return;
      end if;

      --  Calculate total deposit needed
      Total_Deposit := Account.Cert_Deposit + Additional_Deposit;

      if Total_Deposit < Required_New then
         Success := False;
         return;
      end if;

      --  Check sufficient balance for additional deposit
      if Account.Balance < Additional_Deposit then
         Success := False;
         return;
      end if;

      --  Lock additional deposit
      Account.Balance := Account.Balance - Additional_Deposit;
      Account.Cert_Deposit := Total_Deposit;
      Account.Cert_Level := New_Level;
      Success := True;
   end Upgrade_Certification;

   procedure Revoke_Certification (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Slash          : Boolean;
      Success        : out Boolean
   ) is
      Deposit : constant Token_Amount := Account.Cert_Deposit;
      Burn_Amount : Token_Amount;
   begin
      if Account.Cert_Level = None then
         Success := False;
         return;
      end if;

      if Slash then
         --  Burn 50% of deposit on slash
         Burn_Amount := Deposit / 2;
         State.Total_Burned := State.Total_Burned + Burn_Amount;
         State.Total_Supply := State.Total_Supply - Burn_Amount;
         --  Return remaining 50%
         Account.Balance := Account.Balance + (Deposit - Burn_Amount);
      else
         --  Return full deposit
         Account.Balance := Account.Balance + Deposit;
      end if;

      Account.Cert_Deposit := 0;
      Account.Cert_Level := None;
      Success := True;
   end Revoke_Certification;

   function Get_Gas_Discount (Level : Certification_Level) return Natural is
   begin
      case Level is
         when None    => return 0;
         when Bronze  => return Discount_Bronze;
         when Silver  => return Discount_Silver;
         when Gold    => return Discount_Gold;
         when Platinum => return Discount_Platinum;
      end case;
   end Get_Gas_Discount;

   function Get_Required_Deposit (Level : Certification_Level) return Token_Amount is
   begin
      case Level is
         when None    => return 0;
         when Bronze  => return Deposit_Bronze;
         when Silver  => return Deposit_Silver;
         when Gold    => return Deposit_Gold;
         when Platinum => return Deposit_Platinum;
      end case;
   end Get_Required_Deposit;

   ---------------------------------------------------------------------------
   --  Burn Mechanisms
   ---------------------------------------------------------------------------

   procedure Burn_Base_Fee (
      State          : in Out Token_State;
      Fee_Amount     : Token_Amount
   ) is
      Burn_Amount : constant Token_Amount :=
         (Fee_Amount * Token_Amount (Base_Fee_Burn_BP)) / 10_000;
   begin
      if Burn_Amount > 0 and then Burn_Amount <= State.Total_Supply then
         State.Total_Burned := State.Total_Burned + Burn_Amount;
         State.Total_Supply := State.Total_Supply - Burn_Amount;
      end if;
   end Burn_Base_Fee;

   procedure Burn_Proposal_Bond (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Bond_Amount    : Token_Amount
   ) is
      Burn_Amount : constant Token_Amount :=
         (Bond_Amount * Token_Amount (Failed_Proposal_Burn_BP)) / 10_000;
   begin
      if Burn_Amount > 0 and then Burn_Amount <= Account.Balance then
         Account.Balance := Account.Balance - Burn_Amount;
         State.Total_Burned := State.Total_Burned + Burn_Amount;
         State.Total_Supply := State.Total_Supply - Burn_Amount;
      end if;
   end Burn_Proposal_Bond;

   procedure Burn_Slashing (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Slash_Amount   : Token_Amount
   ) is
      Burn_Amount : constant Token_Amount :=
         (Slash_Amount * Token_Amount (Slashing_Burn_BP)) / 10_000;
   begin
      if Burn_Amount > 0 and then Burn_Amount <= Account.Balance then
         Account.Balance := Account.Balance - Burn_Amount;
         State.Total_Burned := State.Total_Burned + Burn_Amount;
         State.Total_Supply := State.Total_Supply - Burn_Amount;
      end if;
   end Burn_Slashing;

   procedure Burn_Privacy_Fee (
      State          : in Out Token_State;
      Fee_Amount     : Token_Amount
   ) is
      Burn_Amount : constant Token_Amount :=
         (Fee_Amount * Token_Amount (Privacy_Fee_Burn_BP)) / 10_000;
   begin
      if Burn_Amount > 0 and then Burn_Amount <= State.Total_Supply then
         State.Total_Burned := State.Total_Burned + Burn_Amount;
         State.Total_Supply := State.Total_Supply - Burn_Amount;
      end if;
   end Burn_Privacy_Fee;

   ---------------------------------------------------------------------------
   --  Allocation Queries
   ---------------------------------------------------------------------------

   function Get_Allocation (Category : Allocation_Category) return Token_Amount is
   begin
      case Category is
         when Solo_Builder       => return Alloc_Solo_Builder;
         when Protocol_Treasury  => return Alloc_Protocol_Treasury;
         when Genesis_Validators => return Alloc_Genesis_Validators;
         when Genesis_Provers    => return Alloc_Genesis_Provers;
         when Developer_Ecosystem => return Alloc_Developer_Ecosystem;
         when Quantum_Insurance  => return Alloc_Quantum_Insurance;
         when Bug_Bounties       => return Alloc_Bug_Bounties;
      end case;
   end Get_Allocation;

   function Get_Remaining (
      State          : Token_State;
      Category       : Allocation_Category
   ) return Token_Amount is
      Alloc : constant Token_Amount := Get_Allocation (Category);
      Used  : Token_Amount;
   begin
      case Category is
         when Solo_Builder       => Used := State.Builder_Released;
         when Protocol_Treasury  => Used := State.Treasury_Released;
         when Genesis_Validators => Used := State.Validators_Earned;
         when Genesis_Provers    => Used := State.Provers_Earned;
         when Developer_Ecosystem => Used := State.Developers_Earned;
         when Quantum_Insurance  => Used := State.Insurance_Locked;
         when Bug_Bounties       => Used := State.Bounties_Paid;
      end case;

      if Used >= Alloc then
         return 0;
      else
         return Alloc - Used;
      end if;
   end Get_Remaining;

   function Is_Exhausted (
      State          : Token_State;
      Category       : Allocation_Category
   ) return Boolean is
   begin
      return Get_Remaining (State, Category) = 0;
   end Is_Exhausted;

   ---------------------------------------------------------------------------
   --  Token Utilities
   ---------------------------------------------------------------------------

   procedure To_Display_String (
      Amount         : Token_Amount;
      Output         : out String;
      Length         : out Natural
   ) is
      Temp : Token_Amount := Amount;
      Idx  : Natural := Output'Last;
      Decimals_Placed : Natural := 0;
   begin
      --  Initialize output
      Output := (others => ' ');
      Length := 0;

      --  Handle zero case
      if Temp = 0 then
         Output (Output'First) := '0';
         Length := 1;
         return;
      end if;

      --  Build string from right to left
      while Temp > 0 and then Idx >= Output'First loop
         --  Insert decimal point after 18 digits from right
         if Decimals_Placed = 18 then
            Output (Idx) := '.';
            Idx := Idx - 1;
            if Idx < Output'First then
               exit;
            end if;
         end if;

         Output (Idx) := Character'Val (48 + Natural (Temp mod 10));
         Temp := Temp / 10;
         Idx := Idx - 1;
         Decimals_Placed := Decimals_Placed + 1;
      end loop;

      --  Calculate length
      Length := Output'Last - Idx;
   end To_Display_String;

   procedure From_Display_String (
      Input          : String;
      Amount         : out Token_Amount;
      Success        : out Boolean
   ) is
      Temp : Token_Amount := 0;
      Decimal_Pos : Natural := 0;
      Decimals : Natural := 0;
   begin
      Amount := 0;
      Success := False;

      --  Find decimal point
      for I in Input'Range loop
         if Input (I) = '.' then
            Decimal_Pos := I;
            exit;
         end if;
      end loop;

      --  Parse integer part
      for I in Input'Range loop
         if Input (I) = '.' then
            null;  --  Skip decimal
         elsif Input (I) in '0' .. '9' then
            if Temp > Token_Amount'Last / 10 then
               return;  --  Overflow
            end if;
            Temp := Temp * 10 + Token_Amount (Character'Pos (Input (I)) - 48);
            if Decimal_Pos > 0 and then I > Decimal_Pos then
               Decimals := Decimals + 1;
            end if;
         elsif Input (I) /= ' ' then
            return;  --  Invalid character
         end if;
      end loop;

      --  Adjust for remaining decimals (18 total)
      while Decimals < 18 loop
         if Temp > Token_Amount'Last / 10 then
            return;  --  Overflow
         end if;
         Temp := Temp * 10;
         Decimals := Decimals + 1;
      end loop;

      Amount := Temp;
      Success := True;
   end From_Display_String;

   function Is_Dust (Amount : Token_Amount) return Boolean is
   begin
      return Amount < Min_Transfer_Amount;
   end Is_Dust;

   ---------------------------------------------------------------------------
   --  Account Operations
   ---------------------------------------------------------------------------

   procedure Init_Account (
      Account        : out Account_State;
      Address        : Byte_Array
   ) is
   begin
      Account.Address := (others => 0);
      for I in Address'Range loop
         if I - Address'First <= Account.Address'Last then
            Account.Address (I - Address'First) := Address (I);
         end if;
      end loop;
      Account.Balance := 0;
      Account.Nonce := 0;
      Account.Frozen := False;
      Account.Cert_Level := None;
      Account.Cert_Deposit := 0;
      Account.Last_Activity := 0;
   end Init_Account;

   procedure Freeze_Account (
      Account        : in Out Account_State
   ) is
   begin
      Account.Frozen := True;
   end Freeze_Account;

   procedure Unfreeze_Account (
      Account        : in Out Account_State
   ) is
   begin
      Account.Frozen := False;
   end Unfreeze_Account;

   procedure Increment_Nonce (
      Account        : in Out Account_State
   ) is
   begin
      Account.Nonce := Account.Nonce + 1;
   end Increment_Nonce;

   ---------------------------------------------------------------------------
   --  State Initialization
   ---------------------------------------------------------------------------

   procedure Init_Token_State (
      State          : out Token_State;
      Genesis_Time   : Unsigned_64
   ) is
   begin
      State.Total_Supply := Total_Supply_Raw * Wei_Per_Token;
      State.Total_Burned := 0;
      State.Genesis_Block := Genesis_Time;
      State.Current_Block := 0;
      State.Builder_Vested := 0;
      State.Builder_Released := 0;
      State.Treasury_Released := 0;
      State.Validators_Earned := 0;
      State.Provers_Earned := 0;
      State.Developers_Earned := 0;
      State.Insurance_Locked := 0;
      State.Bounties_Paid := 0;
   end Init_Token_State;

   procedure Advance_Block (
      State          : in Out Token_State
   ) is
   begin
      State.Current_Block := State.Current_Block + 1;
   end Advance_Block;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Account (
      Account        : Account_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Output'First;
   begin
      Output := (others => 0);

      --  Address (32 bytes)
      for I in Account.Address'Range loop
         if Idx <= Output'Last then
            Output (Idx) := Account.Address (I);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Balance (8 bytes, little-endian)
      declare
         Bal : Token_Amount := Account.Balance;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (Bal mod 256);
               Bal := Bal / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end;

      --  Nonce (8 bytes)
      declare
         N : Unsigned_64 := Account.Nonce;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (N mod 256);
               N := N / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end;

      --  Frozen (1 byte)
      if Idx <= Output'Last then
         Output (Idx) := (if Account.Frozen then 1 else 0);
         Idx := Idx + 1;
      end if;

      --  Cert level (1 byte)
      if Idx <= Output'Last then
         Output (Idx) := Certification_Level'Pos (Account.Cert_Level);
         Idx := Idx + 1;
      end if;

      --  Cert deposit (8 bytes)
      declare
         D : Token_Amount := Account.Cert_Deposit;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (D mod 256);
               D := D / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end;

      Length := Idx - Output'First;
   end Serialize_Account;

   procedure Deserialize_Account (
      Input          : Byte_Array;
      Account        : out Account_State;
      Success        : out Boolean
   ) is
      Idx : Natural := Input'First;
   begin
      Account := (
         Address => (others => 0),
         Balance => 0,
         Nonce => 0,
         Frozen => False,
         Cert_Level => None,
         Cert_Deposit => 0,
         Last_Activity => 0
      );
      Success := False;

      if Input'Length < 58 then  --  Minimum size
         return;
      end if;

      --  Address
      for I in Account.Address'Range loop
         if Idx <= Input'Last then
            Account.Address (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Balance
      for J in 0 .. 7 loop
         if Idx <= Input'Last then
            Account.Balance := Account.Balance +
               Token_Amount (Input (Idx)) * (256 ** J);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Nonce
      for J in 0 .. 7 loop
         if Idx <= Input'Last then
            Account.Nonce := Account.Nonce +
               Unsigned_64 (Input (Idx)) * (256 ** J);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Frozen
      if Idx <= Input'Last then
         Account.Frozen := Input (Idx) /= 0;
         Idx := Idx + 1;
      end if;

      --  Cert level
      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Certification_Level'Pos (Certification_Level'Last) then
            Account.Cert_Level := Certification_Level'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      --  Cert deposit
      for J in 0 .. 7 loop
         if Idx <= Input'Last then
            Account.Cert_Deposit := Account.Cert_Deposit +
               Token_Amount (Input (Idx)) * (256 ** J);
            Idx := Idx + 1;
         end if;
      end loop;

      Success := True;
   end Deserialize_Account;

   procedure Serialize_Token_State (
      State          : Token_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Output'First;

      procedure Write_U64 (Val : Unsigned_64) is
         V : Unsigned_64 := Val;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U64;

   begin
      Output := (others => 0);

      Write_U64 (State.Total_Supply);
      Write_U64 (State.Total_Burned);
      Write_U64 (State.Genesis_Block);
      Write_U64 (State.Current_Block);
      Write_U64 (State.Builder_Vested);
      Write_U64 (State.Builder_Released);
      Write_U64 (State.Treasury_Released);
      Write_U64 (State.Validators_Earned);
      Write_U64 (State.Provers_Earned);
      Write_U64 (State.Developers_Earned);
      Write_U64 (State.Insurance_Locked);
      Write_U64 (State.Bounties_Paid);

      Length := Idx - Output'First;
   end Serialize_Token_State;

   procedure Deserialize_Token_State (
      Input          : Byte_Array;
      State          : out Token_State;
      Success        : out Boolean
   ) is
      Idx : Natural := Input'First;

      procedure Read_U64 (Val : out Unsigned_64) is
      begin
         Val := 0;
         for J in 0 .. 7 loop
            if Idx <= Input'Last then
               Val := Val + Unsigned_64 (Input (Idx)) * (256 ** J);
               Idx := Idx + 1;
            end if;
         end loop;
      end Read_U64;

   begin
      State := (
         Total_Supply => 0,
         Total_Burned => 0,
         Genesis_Block => 0,
         Current_Block => 0,
         Builder_Vested => 0,
         Builder_Released => 0,
         Treasury_Released => 0,
         Validators_Earned => 0,
         Provers_Earned => 0,
         Developers_Earned => 0,
         Insurance_Locked => 0,
         Bounties_Paid => 0
      );
      Success := False;

      if Input'Length < 96 then  --  12 * 8 bytes
         return;
      end if;

      Read_U64 (State.Total_Supply);
      Read_U64 (State.Total_Burned);
      Read_U64 (State.Genesis_Block);
      Read_U64 (State.Current_Block);
      Read_U64 (State.Builder_Vested);
      Read_U64 (State.Builder_Released);
      Read_U64 (State.Treasury_Released);
      Read_U64 (State.Validators_Earned);
      Read_U64 (State.Provers_Earned);
      Read_U64 (State.Developers_Earned);
      Read_U64 (State.Insurance_Locked);
      Read_U64 (State.Bounties_Paid);

      Success := True;
   end Deserialize_Token_State;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Account (Account : out Account_State) is
   begin
      Account.Address := (others => 0);
      Account.Balance := 0;
      Account.Nonce := 0;
      Account.Frozen := False;
      Account.Cert_Level := None;
      Account.Cert_Deposit := 0;
      Account.Last_Activity := 0;
   end Zeroize_Account;

   procedure Zeroize_Token_State (State : out Token_State) is
   begin
      State.Total_Supply := 0;
      State.Total_Burned := 0;
      State.Genesis_Block := 0;
      State.Current_Block := 0;
      State.Builder_Vested := 0;
      State.Builder_Released := 0;
      State.Treasury_Released := 0;
      State.Validators_Earned := 0;
      State.Provers_Earned := 0;
      State.Developers_Earned := 0;
      State.Insurance_Locked := 0;
      State.Bounties_Paid := 0;
   end Zeroize_Token_State;

end Anubis_Token;
