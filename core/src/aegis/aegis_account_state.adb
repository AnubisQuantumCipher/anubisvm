pragma SPARK_Mode (On);

package body Aegis_Account_State with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Runtime Predicate Functions
   ---------------------------------------------------------------------------

   function Is_EOA (Acc : Account_State) return Boolean is
   begin
      if not Acc.Exists then
         return False;
      end if;
      for I in Hash256'Range loop
         pragma Loop_Invariant (I >= Hash256'First);
         if Acc.Code_Hash (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_EOA;

   function Is_Contract (Acc : Account_State) return Boolean is
   begin
      if not Acc.Exists then
         return False;
      end if;
      for I in Hash256'Range loop
         pragma Loop_Invariant (I >= Hash256'First);
         if Acc.Code_Hash (I) /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Is_Contract;

   ---------------------------------------------------------------------------
   --  Account Creation and Initialization
   ---------------------------------------------------------------------------

   --  Create new account with given address
   function Create_Account (
      Address : Account_ID
   ) return Account_State is
      Result : Account_State;
   begin
      Result.Address      := Address;
      Result.Balance      := U256_Zero;
      Result.Nonce        := 0;
      Result.Code_Hash    := Hash256_Zero;
      Result.Storage_Root := Hash256_Zero;
      Result.Exists       := True;

      return Result;
   end Create_Account;

   --  Initialize contract account with code hash
   procedure Initialize_Contract (
      Account   : in out Account_State;
      Code_Hash : in     Hash256;
      Success   : out    Boolean;
      Error     : out    Account_Error
   ) is
   begin
      --  Verify account is an EOA (not already a contract)
      if not Is_EOA (Account) then
         Success := False;
         Error := Contract_Already_Deployed;
         return;
      end if;

      --  Verify code hash is non-zero
      declare
         Has_Nonzero : Boolean := False;
      begin
         for I in Hash256'Range loop
            pragma Loop_Invariant (I >= Hash256'First);
            if Code_Hash (I) /= 0 then
               Has_Nonzero := True;
               exit;
            end if;
         end loop;

         if not Has_Nonzero then
            Success := False;
            Error := Invalid_Transfer;
            return;
         end if;
      end;

      --  Set code hash to make this a contract
      Account.Code_Hash := Code_Hash;
      Success := True;
      Error := Account_Error'First;  --  Success
   end Initialize_Contract;

   ---------------------------------------------------------------------------
   --  Nonce Management
   ---------------------------------------------------------------------------

   --  Get current nonce
   function Get_Nonce (Account : Account_State) return Unsigned_64 is
   begin
      return Account.Nonce;
   end Get_Nonce;

   --  Increment nonce
   procedure Increment_Nonce (
      Account : in out Account_State;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) is
   begin
      --  Check for overflow
      if Account.Nonce >= Unsigned_64'Last then
         Success := False;
         Error := Nonce_Overflow;
         return;
      end if;

      --  Increment nonce
      Account.Nonce := Account.Nonce + 1;
      Success := True;
      Error := Account_Error'First;  --  Success
   end Increment_Nonce;

   --  Set nonce
   procedure Set_Nonce (
      Account : in out Account_State;
      Nonce   : in     Unsigned_64
   ) is
   begin
      Account.Nonce := Nonce;
   end Set_Nonce;

   ---------------------------------------------------------------------------
   --  Balance Management
   ---------------------------------------------------------------------------

   --  Get current balance
   function Get_Balance (Account : Account_State) return U256 is
   begin
      return Account.Balance;
   end Get_Balance;

   --  Add amount to balance
   procedure Add_Balance (
      Account : in out Account_State;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) is
      Result   : U256;
      Overflow : Boolean;
   begin
      --  Check for overflow
      Add (Account.Balance, Amount, Result, Overflow);

      if Overflow then
         Success := False;
         Error := Balance_Overflow;
         return;
      end if;

      --  Update balance
      Account.Balance := Result;
      Success := True;
      Error := Account_Error'First;  --  Success
   end Add_Balance;

   --  Subtract amount from balance
   procedure Sub_Balance (
      Account : in out Account_State;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) is
      Result    : U256;
      Underflow : Boolean;
   begin
      --  Check for underflow (insufficient balance)
      Sub (Account.Balance, Amount, Result, Underflow);

      if Underflow then
         Success := False;
         Error := Insufficient_Balance;
         return;
      end if;

      --  Update balance
      Account.Balance := Result;
      Success := True;
      Error := Account_Error'First;  --  Success
   end Sub_Balance;

   --  Set balance directly
   procedure Set_Balance (
      Account : in out Account_State;
      Balance : in     U256
   ) is
   begin
      Account.Balance := Balance;
   end Set_Balance;

   --  Transfer amount between accounts
   procedure Transfer (
      From    : in out Account_State;
      To      : in out Account_State;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) is
      Sub_Success : Boolean;
      Add_Success : Boolean;
      Sub_Error   : Account_Error;
      Add_Error   : Account_Error;
      From_Balance_Backup : U256;
   begin
      --  Backup from balance for rollback
      From_Balance_Backup := From.Balance;

      --  Subtract from sender
      Sub_Balance (From, Amount, Sub_Success, Sub_Error);
      if not Sub_Success then
         Success := False;
         Error := Sub_Error;
         return;
      end if;

      --  Add to receiver
      Add_Balance (To, Amount, Add_Success, Add_Error);
      if not Add_Success then
         --  Rollback: restore sender's balance
         From.Balance := From_Balance_Backup;
         Success := False;
         Error := Add_Error;
         return;
      end if;

      Success := True;
      Error := Account_Error'First;  --  Success
   end Transfer;

   ---------------------------------------------------------------------------
   --  Account Metadata
   ---------------------------------------------------------------------------

   --  Check if account exists
   function Account_Exists (Account : Account_State) return Boolean is
   begin
      return Account.Exists;
   end Account_Exists;

   --  Get code hash
   function Get_Code_Hash (Account : Account_State) return Hash256 is
   begin
      return Account.Code_Hash;
   end Get_Code_Hash;

   --  Set storage root
   procedure Set_Storage_Root (
      Account      : in out Account_State;
      Storage_Root : in     Hash256
   ) is
   begin
      Account.Storage_Root := Storage_Root;
   end Set_Storage_Root;

   --  Get storage root
   function Get_Storage_Root (Account : Account_State) return Hash256 is
   begin
      return Account.Storage_Root;
   end Get_Storage_Root;

   ---------------------------------------------------------------------------
   --  Account Validation
   ---------------------------------------------------------------------------

   --  Validate account state consistency
   function Validate_Account (Account : Account_State) return Boolean is
   begin
      --  Account must exist
      if not Account.Exists then
         return False;
      end if;

      --  Check if EOA or contract
      if not (Is_EOA (Account) or Is_Contract (Account)) then
         return False;
      end if;

      return True;
   end Validate_Account;

end Aegis_Account_State;
