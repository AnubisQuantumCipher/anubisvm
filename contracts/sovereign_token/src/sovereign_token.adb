--  Sovereign_Token - AnubisVM Contract Implementation
--  Full ERC20-compatible token with per-address balances
pragma SPARK_Mode (On);

package body Sovereign_Token with SPARK_Mode => On is

   procedure Initialize (
      State  : out Contract_State;
      Owner  : Address;
      Supply : Balance
   ) is
   begin
      --  Set in-memory state
      State := (
         Version      => One,
         Owner        => Owner,
         Total_Supply => Supply,
         Initialized  => True
      );

      --  Store total supply in persistent storage
      Khepri_State.SStore (Slot_Total_Supply, Supply);

      --  Store owner address in persistent storage
      Khepri_State.SStore (Slot_Owner, Address_To_U256 (Owner));

      --  Credit owner with initial supply
      Khepri_State.Set_Balance (Slot_Balances, Owner, Supply);
   end Initialize;

   function Is_Owner (State : Contract_State; Caller : Address)
      return Boolean is
   begin
      return Address_Equal (State.Owner, Caller);
   end Is_Owner;

   ---------------------------------------------------------------------------
   --  ERC20-Compatible Implementation
   ---------------------------------------------------------------------------

   function BalanceOf (Account : Address) return Balance is
   begin
      return Khepri_State.Balance_Of (Slot_Balances, Account);
   end BalanceOf;

   function TotalSupply return Balance is
      Value : Uint256;
   begin
      Khepri_State.SLoad (Slot_Total_Supply, Value);
      return Value;
   end TotalSupply;

   procedure Transfer (
      State  : in Out Contract_State;
      To     : Address;
      Amount : Balance;
      Caller : Address;
      Status : out Error_Code
   ) is
      Sender_Balance   : Balance;
      Recipient_Balance : Balance;
   begin
      --  Validate recipient address (don't send to null address)
      if not Is_Valid_Address (To) then
         Status := Invalid_Input;
         return;
      end if;

      --  Cannot transfer to self
      if Address_Equal (Caller, To) then
         Status := Invalid_Input;
         return;
      end if;

      --  Get sender's balance
      Sender_Balance := Khepri_State.Balance_Of (Slot_Balances, Caller);

      --  Check sufficient balance
      if Sender_Balance < Amount then
         Status := Insufficient_Balance;
         return;
      end if;

      --  Get recipient's balance
      Recipient_Balance := Khepri_State.Balance_Of (Slot_Balances, To);

      --  Check for overflow (paranoid check)
      if Recipient_Balance > Sub (Max_Uint256, Amount) then
         Status := Overflow;
         return;
      end if;

      --  Update balances: debit sender, credit recipient
      Khepri_State.Set_Balance (Slot_Balances, Caller, Sub (Sender_Balance, Amount));
      Khepri_State.Set_Balance (Slot_Balances, To, Add (Recipient_Balance, Amount));

      Status := No_Error;
   end Transfer;

   procedure Approve (
      Owner   : Address;
      Spender : Address;
      Amount  : Balance;
      Status  : out Error_Code
   ) is
   begin
      --  Cannot approve null address
      if not Is_Valid_Address (Spender) then
         Status := Invalid_Input;
         return;
      end if;

      --  Set allowance in storage
      Khepri_State.Set_Allowance (Slot_Allowances, Owner, Spender, Amount);

      Status := No_Error;
   end Approve;

   function Allowance (
      Owner   : Address;
      Spender : Address
   ) return Balance is
   begin
      return Khepri_State.Get_Allowance (Slot_Allowances, Owner, Spender);
   end Allowance;

   procedure TransferFrom (
      State   : in Out Contract_State;
      From    : Address;
      To      : Address;
      Amount  : Balance;
      Caller  : Address;
      Status  : out Error_Code
   ) is
      Current_Allowance : Balance;
      Sender_Balance    : Balance;
      Recipient_Balance : Balance;
   begin
      --  Validate addresses
      if not Is_Valid_Address (From) or not Is_Valid_Address (To) then
         Status := Invalid_Input;
         return;
      end if;

      --  Cannot transfer to self
      if Address_Equal (From, To) then
         Status := Invalid_Input;
         return;
      end if;

      --  Check allowance (caller spending from owner's account)
      Current_Allowance := Khepri_State.Get_Allowance (Slot_Allowances, From, Caller);

      if Current_Allowance < Amount then
         Status := Unauthorized;
         return;
      end if;

      --  Get sender's balance
      Sender_Balance := Khepri_State.Balance_Of (Slot_Balances, From);

      --  Check sufficient balance
      if Sender_Balance < Amount then
         Status := Insufficient_Balance;
         return;
      end if;

      --  Get recipient's balance
      Recipient_Balance := Khepri_State.Balance_Of (Slot_Balances, To);

      --  Check for overflow
      if Recipient_Balance > Sub (Max_Uint256, Amount) then
         Status := Overflow;
         return;
      end if;

      --  Update balances: debit sender, credit recipient
      Khepri_State.Set_Balance (Slot_Balances, From, Sub (Sender_Balance, Amount));
      Khepri_State.Set_Balance (Slot_Balances, To, Add (Recipient_Balance, Amount));

      --  Reduce allowance
      Khepri_State.Set_Allowance (
         Slot_Allowances, From, Caller,
         Sub (Current_Allowance, Amount)
      );

      Status := No_Error;
   end TransferFrom;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Supply (State : Contract_State) return Balance is
      (State.Total_Supply);

   function Get_Owner (State : Contract_State) return Address is
      (State.Owner);

   function State_Hash (State : Contract_State) return Hash_256 is
      --  Serialize state fields into a canonical byte representation
      --  Format: Version (32) || Owner (32) || Total_Supply (32) || Initialized (1)
      --  Total: 97 bytes
      Buffer : Aegis_VM_Types.Byte_Array (0 .. 96);
      Version_Bytes : constant Bytes32 := U256_To_Bytes_BE (State.Version);
      Owner_Bytes : constant Bytes32 := Address_To_Bytes (State.Owner);
      Supply_Bytes : constant Bytes32 := U256_To_Bytes_BE (State.Total_Supply);
   begin
      --  Pack version (bytes 0..31)
      for I in Bytes32'Range loop
         Buffer (Natural (I)) := Version_Bytes (I);
      end loop;

      --  Pack owner (bytes 32..63)
      for I in Bytes32'Range loop
         Buffer (32 + Natural (I)) := Owner_Bytes (I);
      end loop;

      --  Pack total supply (bytes 64..95)
      for I in Bytes32'Range loop
         Buffer (64 + Natural (I)) := Supply_Bytes (I);
      end loop;

      --  Pack initialized flag (byte 96)
      Buffer (96) := (if State.Initialized then 1 else 0);

      --  Hash the serialized state
      return SHA3_256 (Buffer);
   end State_Hash;

end Sovereign_Token;
