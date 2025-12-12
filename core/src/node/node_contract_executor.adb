pragma SPARK_Mode (Off);

--  SPARK_Mode disabled for this package body because it uses
--  access-to-subprogram types with global effects, which is
--  intentionally outside SPARK for the native contract dispatch
--  mechanism. The core cryptographic and CVM modules remain
--  under full SPARK verification.

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;
with Khepri_State;
with Sphinx_Native; use Sphinx_Native;
with Aegis_Sandbox;
with Ada.Text_IO;

package body Node_Contract_Executor with
   SPARK_Mode => Off
is

   ---------------------------------------------------------------------------
   --  Global Execution Context (accessible by handlers)
   ---------------------------------------------------------------------------

   Ctx_Sender : Contract_Address := (others => 0);
   Ctx_Self   : Contract_Address := (others => 0);
   Ctx_Value  : U256 := U256_Zero;

   ---------------------------------------------------------------------------
   --  Storage Slot Constants (per contract type)
   ---------------------------------------------------------------------------

   --  HelloCounter slots
   Slot_HC_Count : constant Khepri_Types.Uint256 := Khepri_Types.Zero;
   Slot_HC_Owner : constant Khepri_Types.Uint256 := Khepri_Types.One;

   --  SimpleToken slots
   Slot_Token_TotalSupply : constant Khepri_Types.Uint256 := (Limbs => (0, 0, 0, 0));
   Slot_Token_Balances    : constant Khepri_Types.Uint256 := (Limbs => (1, 0, 0, 0));
   Slot_Token_Allowances  : constant Khepri_Types.Uint256 := (Limbs => (2, 0, 0, 0));
   Slot_Token_Name        : constant Khepri_Types.Uint256 := (Limbs => (3, 0, 0, 0));
   Slot_Token_Symbol      : constant Khepri_Types.Uint256 := (Limbs => (4, 0, 0, 0));
   Slot_Token_Decimals    : constant Khepri_Types.Uint256 := (Limbs => (5, 0, 0, 0));

   --  SimpleVault slots
   Slot_Vault_TotalDeposits : constant Khepri_Types.Uint256 := (Limbs => (0, 0, 0, 0));
   Slot_Vault_Balances      : constant Khepri_Types.Uint256 := (Limbs => (1, 0, 0, 0));
   Slot_Vault_Owner         : constant Khepri_Types.Uint256 := (Limbs => (2, 0, 0, 0));

   --  QuantumDID slots (post-quantum decentralized identity)
   Slot_DID_Count           : constant Khepri_Types.Uint256 := (Limbs => (16#100#, 0, 0, 0));
   Slot_DID_Identities      : constant Khepri_Types.Uint256 := (Limbs => (16#101#, 0, 0, 0));
   Slot_DID_Credentials     : constant Khepri_Types.Uint256 := (Limbs => (16#102#, 0, 0, 0));
   Slot_DID_Cred_Count      : constant Khepri_Types.Uint256 := (Limbs => (16#103#, 0, 0, 0));

   --  Staking contract slots (proof-of-stake)
   Slot_Stake_TotalStaked   : constant Khepri_Types.Uint256 := (Limbs => (16#200#, 0, 0, 0));
   Slot_Stake_Stakes        : constant Khepri_Types.Uint256 := (Limbs => (16#201#, 0, 0, 0));
   Slot_Stake_Rewards       : constant Khepri_Types.Uint256 := (Limbs => (16#202#, 0, 0, 0));
   Slot_Stake_ValidatorCount : constant Khepri_Types.Uint256 := (Limbs => (16#203#, 0, 0, 0));
   Slot_Stake_MinStake      : constant Khepri_Types.Uint256 := (Limbs => (16#204#, 0, 0, 0));

   --  Governance contract slots (on-chain voting)
   Slot_Gov_ProposalCount   : constant Khepri_Types.Uint256 := (Limbs => (16#300#, 0, 0, 0));
   Slot_Gov_Proposals       : constant Khepri_Types.Uint256 := (Limbs => (16#301#, 0, 0, 0));
   Slot_Gov_Votes           : constant Khepri_Types.Uint256 := (Limbs => (16#302#, 0, 0, 0));
   Slot_Gov_Quorum          : constant Khepri_Types.Uint256 := (Limbs => (16#303#, 0, 0, 0));
   Slot_Gov_VotingPeriod    : constant Khepri_Types.Uint256 := (Limbs => (16#304#, 0, 0, 0));

   ---------------------------------------------------------------------------
   --  Context Accessors
   ---------------------------------------------------------------------------

   function Get_Current_Sender return Contract_Address is
   begin
      return Ctx_Sender;
   end Get_Current_Sender;

   function Get_Current_Self return Contract_Address is
   begin
      return Ctx_Self;
   end Get_Current_Self;

   function Get_Current_Value return U256 is
   begin
      return Ctx_Value;
   end Get_Current_Value;

   procedure Set_Storage_Context (
      Contract_Addr : in Contract_Address
   ) is
      Addr : Khepri_Types.Address;
   begin
      for I in 0 .. 31 loop
         Addr (I) := Aegis_VM_Types.Byte (Contract_Addr (I));
      end loop;
      --  Note: Khepri_State internally tracks current contract
      --  This is a placeholder for proper context switching
      null;
   end Set_Storage_Context;

   ---------------------------------------------------------------------------
   --  Helper: Parse U256 from args (big-endian, 32 bytes)
   ---------------------------------------------------------------------------

   procedure Parse_U256 (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Offset    : in     Natural;
      Value     : out    U256;
      Success   : out    Boolean
   ) is
   begin
      Value := U256_Zero;
      Success := False;

      if Offset + 32 <= Args_Size then
         for I in 0 .. 3 loop
            declare
               Limb : Word64 := 0;
            begin
               for J in 0 .. 7 loop
                  Limb := Limb or Shift_Left (
                     Word64 (Args (Offset + I * 8 + J)),
                     (7 - J) * 8
                  );
               end loop;
               Value.Limbs (3 - I) := Limb;
            end;
         end loop;
         Success := True;
      end if;
   end Parse_U256;

   procedure Parse_Address (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Offset    : in     Natural;
      Addr      : out    Contract_Address;
      Success   : out    Boolean
   ) is
   begin
      Addr := (others => 0);
      Success := False;

      if Offset + 32 <= Args_Size then
         for I in 0 .. 31 loop
            Addr (I) := Args (Offset + I);
         end loop;
         Success := True;
      end if;
   end Parse_Address;

   ---------------------------------------------------------------------------
   --  HelloCounter Handlers (using Khepri_State)
   ---------------------------------------------------------------------------

   procedure HC_Initialize (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
   begin
      --  Store initial count = 0
      Khepri_State.SStore (Slot_HC_Count, Khepri_Types.Zero);
      --  Store owner = 1 (placeholder)
      Khepri_State.SStore (Slot_HC_Owner, Khepri_Types.One);
      Ret := U256_Zero;
      Success := True;
   end HC_Initialize;

   procedure HC_Increment (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Current : Khepri_Types.Uint256;
      New_Val : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_HC_Count, Current);
      New_Val := Khepri_Types."+" (Current, Khepri_Types.One);
      Khepri_State.SStore (Slot_HC_Count, New_Val);
      Ret := (Limbs => New_Val.Limbs);
      Success := True;
   end HC_Increment;

   procedure HC_Increment_By (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Amount  : U256;
      Current : Khepri_Types.Uint256;
      New_Val : Khepri_Types.Uint256;
      Parse_OK : Boolean;
   begin
      Parse_U256 (Args, Args_Size, 0, Amount, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Khepri_State.SLoad (Slot_HC_Count, Current);
      declare
         Amt : constant Khepri_Types.Uint256 := (Limbs => Amount.Limbs);
      begin
         New_Val := Khepri_Types."+" (Current, Amt);
      end;
      Khepri_State.SStore (Slot_HC_Count, New_Val);
      Ret := (Limbs => New_Val.Limbs);
      Success := True;
   end HC_Increment_By;

   procedure HC_Reset (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
   begin
      Khepri_State.SStore (Slot_HC_Count, Khepri_Types.Zero);
      Ret := U256_Zero;
      Success := True;
   end HC_Reset;

   procedure HC_Get_Count (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Current : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_HC_Count, Current);
      Ret := (Limbs => Current.Limbs);
      Success := True;
   end HC_Get_Count;

   procedure HC_Get_Owner (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Owner : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_HC_Owner, Owner);
      Ret := (Limbs => Owner.Limbs);
      Success := True;
   end HC_Get_Owner;

   ---------------------------------------------------------------------------
   --  SimpleToken Handlers (ERC20-style)
   ---------------------------------------------------------------------------

   procedure Token_Initialize (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Initial_Supply : U256;
      Parse_OK : Boolean;
      Sender_Key : Khepri_Types.Uint256;
   begin
      --  Args: initial_supply (32 bytes)
      Parse_U256 (Args, Args_Size, 0, Initial_Supply, Parse_OK);
      if not Parse_OK then
         --  Default to 1 million tokens (with 18 decimals)
         Initial_Supply := (Limbs => (16#DE0B6B3A7640000#, 0, 0, 0)); -- 10^18
      end if;

      --  Set total supply
      Khepri_State.SStore (Slot_Token_TotalSupply, (Limbs => Initial_Supply.Limbs));

      --  Set decimals = 18
      Khepri_State.SStore (Slot_Token_Decimals, (Limbs => (18, 0, 0, 0)));

      --  Give all tokens to sender
      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Khepri_State.Map_Set (Slot_Token_Balances, Sender_Key, (Limbs => Initial_Supply.Limbs));

      Ret := Initial_Supply;
      Success := True;
   end Token_Initialize;

   procedure Token_Transfer (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      To_Addr   : Contract_Address;
      Amount    : U256;
      Parse_OK  : Boolean;
      Sender_Bal : Khepri_Types.Uint256;
      To_Bal     : Khepri_Types.Uint256;
      Sender_Key : Khepri_Types.Uint256;
      To_Key     : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      --  Args: to (32 bytes) || amount (32 bytes)
      Parse_Address (Args, Args_Size, 0, To_Addr, Parse_OK);
      if not Parse_OK then return; end if;

      Parse_U256 (Args, Args_Size, 32, Amount, Parse_OK);
      if not Parse_OK then return; end if;

      --  Get sender balance
      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Sender_Bal := Khepri_State.Balance_Of (Slot_Token_Balances, Ctx_Sender);

      --  Check sufficient balance
      if Khepri_Types."<" (Sender_Bal, (Limbs => Amount.Limbs)) then
         return;  -- Insufficient balance
      end if;

      --  Subtract from sender
      declare
         New_Sender_Bal : Khepri_Types.Uint256;
      begin
         New_Sender_Bal := Khepri_Types."-" (Sender_Bal, (Limbs => Amount.Limbs));
         Khepri_State.Set_Balance (Slot_Token_Balances, Ctx_Sender, New_Sender_Bal);
      end;

      --  Add to recipient
      To_Key := Khepri_State.Address_Key (To_Addr);
      To_Bal := Khepri_State.Balance_Of (Slot_Token_Balances, To_Addr);
      declare
         New_To_Bal : Khepri_Types.Uint256;
      begin
         New_To_Bal := Khepri_Types."+" (To_Bal, (Limbs => Amount.Limbs));
         Khepri_State.Set_Balance (Slot_Token_Balances, To_Addr, New_To_Bal);
      end;

      Ret := (Limbs => (1, 0, 0, 0));  -- true
      Success := True;
   end Token_Transfer;

   procedure Token_BalanceOf (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Account  : Contract_Address;
      Parse_OK : Boolean;
      Balance  : Khepri_Types.Uint256;
   begin
      Parse_Address (Args, Args_Size, 0, Account, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Balance := Khepri_State.Balance_Of (Slot_Token_Balances, Account);
      Ret := (Limbs => Balance.Limbs);
      Success := True;
   end Token_BalanceOf;

   procedure Token_TotalSupply (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Supply : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_Token_TotalSupply, Supply);
      Ret := (Limbs => Supply.Limbs);
      Success := True;
   end Token_TotalSupply;

   procedure Token_Approve (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Spender  : Contract_Address;
      Amount   : U256;
      Parse_OK : Boolean;
   begin
      Ret := U256_Zero;
      Success := False;

      --  Args: spender (32 bytes) || amount (32 bytes)
      Parse_Address (Args, Args_Size, 0, Spender, Parse_OK);
      if not Parse_OK then return; end if;

      Parse_U256 (Args, Args_Size, 32, Amount, Parse_OK);
      if not Parse_OK then return; end if;

      --  Set allowance
      Khepri_State.Set_Allowance (
         Slot_Token_Allowances,
         Ctx_Sender,
         Spender,
         (Limbs => Amount.Limbs)
      );

      Ret := (Limbs => (1, 0, 0, 0));  -- true
      Success := True;
   end Token_Approve;

   procedure Token_Allowance (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Owner    : Contract_Address;
      Spender  : Contract_Address;
      Parse_OK : Boolean;
      Allow    : Khepri_Types.Uint256;
   begin
      --  Args: owner (32 bytes) || spender (32 bytes)
      Parse_Address (Args, Args_Size, 0, Owner, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Parse_Address (Args, Args_Size, 32, Spender, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Allow := Khepri_State.Get_Allowance (Slot_Token_Allowances, Owner, Spender);
      Ret := (Limbs => Allow.Limbs);
      Success := True;
   end Token_Allowance;

   ---------------------------------------------------------------------------
   --  SimpleVault Handlers
   ---------------------------------------------------------------------------

   procedure Vault_Initialize (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Sender_Key : Khepri_Types.Uint256;
   begin
      --  Set total deposits = 0
      Khepri_State.SStore (Slot_Vault_TotalDeposits, Khepri_Types.Zero);

      --  Set owner = sender
      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Khepri_State.SStore (Slot_Vault_Owner, Sender_Key);

      Ret := U256_Zero;
      Success := True;
   end Vault_Initialize;

   procedure Vault_Deposit (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Amount   : U256;
      Parse_OK : Boolean;
      Current_Bal : Khepri_Types.Uint256;
      Total_Dep   : Khepri_Types.Uint256;
   begin
      --  Args: amount (32 bytes)
      Parse_U256 (Args, Args_Size, 0, Amount, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      --  Add to sender's balance
      Current_Bal := Khepri_State.Balance_Of (Slot_Vault_Balances, Ctx_Sender);
      declare
         New_Bal : Khepri_Types.Uint256;
      begin
         New_Bal := Khepri_Types."+" (Current_Bal, (Limbs => Amount.Limbs));
         Khepri_State.Set_Balance (Slot_Vault_Balances, Ctx_Sender, New_Bal);
      end;

      --  Update total deposits
      Khepri_State.SLoad (Slot_Vault_TotalDeposits, Total_Dep);
      declare
         New_Total : Khepri_Types.Uint256;
      begin
         New_Total := Khepri_Types."+" (Total_Dep, (Limbs => Amount.Limbs));
         Khepri_State.SStore (Slot_Vault_TotalDeposits, New_Total);
         Ret := (Limbs => New_Total.Limbs);
      end;

      Success := True;
   end Vault_Deposit;

   procedure Vault_Withdraw (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Amount   : U256;
      Parse_OK : Boolean;
      Current_Bal : Khepri_Types.Uint256;
      Total_Dep   : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      --  Args: amount (32 bytes)
      Parse_U256 (Args, Args_Size, 0, Amount, Parse_OK);
      if not Parse_OK then return; end if;

      --  Check balance
      Current_Bal := Khepri_State.Balance_Of (Slot_Vault_Balances, Ctx_Sender);
      if Khepri_Types."<" (Current_Bal, (Limbs => Amount.Limbs)) then
         return;  -- Insufficient balance
      end if;

      --  Subtract from balance
      declare
         New_Bal : Khepri_Types.Uint256;
      begin
         New_Bal := Khepri_Types."-" (Current_Bal, (Limbs => Amount.Limbs));
         Khepri_State.Set_Balance (Slot_Vault_Balances, Ctx_Sender, New_Bal);
      end;

      --  Update total deposits
      Khepri_State.SLoad (Slot_Vault_TotalDeposits, Total_Dep);
      declare
         New_Total : Khepri_Types.Uint256;
      begin
         New_Total := Khepri_Types."-" (Total_Dep, (Limbs => Amount.Limbs));
         Khepri_State.SStore (Slot_Vault_TotalDeposits, New_Total);
      end;

      Ret := Amount;
      Success := True;
   end Vault_Withdraw;

   procedure Vault_BalanceOf (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Account  : Contract_Address;
      Parse_OK : Boolean;
      Balance  : Khepri_Types.Uint256;
   begin
      Parse_Address (Args, Args_Size, 0, Account, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Balance := Khepri_State.Balance_Of (Slot_Vault_Balances, Account);
      Ret := (Limbs => Balance.Limbs);
      Success := True;
   end Vault_BalanceOf;

   procedure Vault_TotalDeposits (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Total : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_Vault_TotalDeposits, Total);
      Ret := (Limbs => Total.Limbs);
      Success := True;
   end Vault_TotalDeposits;

   ---------------------------------------------------------------------------
   --  QuantumDID Handlers (Post-Quantum Decentralized Identity)
   ---------------------------------------------------------------------------

   --  Register a new identity with ML-DSA-87 public key
   procedure DID_Register (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: public_key (32 bytes - key hash, full ML-DSA-87 key would be 2592 bytes)
      Parse_OK : Boolean;
      Sender_Key : Khepri_Types.Uint256;
      Existing : Khepri_Types.Uint256;
      Count : Khepri_Types.Uint256;
      PubKey_Hash : U256;
   begin
      Ret := U256_Zero;
      Success := False;

      --  Parse public key hash from args
      Parse_U256 (Args, Args_Size, 0, PubKey_Hash, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      --  Check if already registered
      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Existing := Khepri_State.Map_Get (Slot_DID_Identities, Sender_Key);
      if Existing.Limbs (0) /= 0 or Existing.Limbs (1) /= 0 or
         Existing.Limbs (2) /= 0 or Existing.Limbs (3) /= 0
      then
         --  Already registered
         return;
      end if;

      --  Store identity (maps sender address -> pubkey hash)
      Khepri_State.Map_Set (Slot_DID_Identities, Sender_Key, (Limbs => PubKey_Hash.Limbs));

      --  Initialize credential count for this identity
      Khepri_State.Map_Set (Slot_DID_Cred_Count, Sender_Key, Khepri_Types.Zero);

      --  Increment global DID count
      Khepri_State.SLoad (Slot_DID_Count, Count);
      Count := Khepri_Types."+" (Count, Khepri_Types.One);
      Khepri_State.SStore (Slot_DID_Count, Count);

      Ret := (Limbs => (1, 0, 0, 0));  -- success = 1
      Success := True;
   end DID_Register;

   --  Add a credential to an identity
   procedure DID_AddCredential (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: credential_hash (32 bytes)
      Parse_OK : Boolean;
      Sender_Key : Khepri_Types.Uint256;
      Existing : Khepri_Types.Uint256;
      Cred_Count : Khepri_Types.Uint256;
      Cred_Hash : U256;
      Cred_Key : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      --  Parse credential hash
      Parse_U256 (Args, Args_Size, 0, Cred_Hash, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      --  Check if identity is registered
      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Existing := Khepri_State.Map_Get (Slot_DID_Identities, Sender_Key);
      if Existing.Limbs (0) = 0 and Existing.Limbs (1) = 0 and
         Existing.Limbs (2) = 0 and Existing.Limbs (3) = 0
      then
         --  Not registered
         return;
      end if;

      --  Get current credential count
      Cred_Count := Khepri_State.Map_Get (Slot_DID_Cred_Count, Sender_Key);

      --  Create credential key: hash(sender || cred_index)
      --  For simplicity, use cred_count as key offset
      Cred_Key := Khepri_Types."+" (Sender_Key, Cred_Count);

      --  Store credential
      Khepri_State.Map_Set (Slot_DID_Credentials, Cred_Key, (Limbs => Cred_Hash.Limbs));

      --  Increment credential count
      Cred_Count := Khepri_Types."+" (Cred_Count, Khepri_Types.One);
      Khepri_State.Map_Set (Slot_DID_Cred_Count, Sender_Key, Cred_Count);

      Ret := (Limbs => Cred_Count.Limbs);  -- Return new credential count
      Success := True;
   end DID_AddCredential;

   --  Revoke a credential (mark as revoked by setting to 1)
   procedure DID_RevokeCredential (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: credential_index (32 bytes as U256)
      Parse_OK : Boolean;
      Sender_Key : Khepri_Types.Uint256;
      Cred_Index : U256;
      Cred_Key : Khepri_Types.Uint256;
      Revoked_Marker : constant Khepri_Types.Uint256 := Khepri_Types.One;
   begin
      Ret := U256_Zero;
      Success := False;

      --  Parse credential index
      Parse_U256 (Args, Args_Size, 0, Cred_Index, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Cred_Key := Khepri_Types."+" (Sender_Key, (Limbs => Cred_Index.Limbs));

      --  Mark as revoked (store 1)
      Khepri_State.Map_Set (Slot_DID_Credentials, Cred_Key, Revoked_Marker);

      Ret := (Limbs => (1, 0, 0, 0));  -- success
      Success := True;
   end DID_RevokeCredential;

   --  Get identity public key hash for an address
   procedure DID_GetIdentity (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: address (32 bytes)
      Parse_OK : Boolean;
      Account : Contract_Address;
      Account_Key : Khepri_Types.Uint256;
      Identity : Khepri_Types.Uint256;
   begin
      Parse_Address (Args, Args_Size, 0, Account, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Account_Key := Khepri_State.Address_Key (Account);
      Identity := Khepri_State.Map_Get (Slot_DID_Identities, Account_Key);
      Ret := (Limbs => Identity.Limbs);
      Success := True;
   end DID_GetIdentity;

   --  Check if an address is registered
   procedure DID_IsRegistered (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: address (32 bytes)
      Parse_OK : Boolean;
      Account : Contract_Address;
      Account_Key : Khepri_Types.Uint256;
      Identity : Khepri_Types.Uint256;
   begin
      Parse_Address (Args, Args_Size, 0, Account, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Account_Key := Khepri_State.Address_Key (Account);
      Identity := Khepri_State.Map_Get (Slot_DID_Identities, Account_Key);

      if Identity.Limbs (0) /= 0 or Identity.Limbs (1) /= 0 or
         Identity.Limbs (2) /= 0 or Identity.Limbs (3) /= 0
      then
         Ret := (Limbs => (1, 0, 0, 0));  -- true
      else
         Ret := U256_Zero;  -- false
      end if;
      Success := True;
   end DID_IsRegistered;

   --  Get credential count for an address
   procedure DID_GetCredentialCount (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: address (32 bytes)
      Parse_OK : Boolean;
      Account : Contract_Address;
      Account_Key : Khepri_Types.Uint256;
      Count : Khepri_Types.Uint256;
   begin
      Parse_Address (Args, Args_Size, 0, Account, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Account_Key := Khepri_State.Address_Key (Account);
      Count := Khepri_State.Map_Get (Slot_DID_Cred_Count, Account_Key);
      Ret := (Limbs => Count.Limbs);
      Success := True;
   end DID_GetCredentialCount;

   ---------------------------------------------------------------------------
   --  Staking Contract Handlers (Proof-of-Stake)
   ---------------------------------------------------------------------------

   --  Initialize staking contract
   procedure Staking_Initialize (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: min_stake (32 bytes)
      Parse_OK : Boolean;
      Min_Stake : U256;
   begin
      Parse_U256 (Args, Args_Size, 0, Min_Stake, Parse_OK);
      if not Parse_OK then
         --  Default minimum stake: 1000 tokens
         Min_Stake := (Limbs => (1000, 0, 0, 0));
      end if;

      --  Initialize state
      Khepri_State.SStore (Slot_Stake_TotalStaked, Khepri_Types.Zero);
      Khepri_State.SStore (Slot_Stake_ValidatorCount, Khepri_Types.Zero);
      Khepri_State.SStore (Slot_Stake_MinStake, (Limbs => Min_Stake.Limbs));

      Ret := Min_Stake;
      Success := True;
   end Staking_Initialize;

   --  Stake tokens (become a validator)
   procedure Staking_Stake (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: amount (32 bytes)
      Parse_OK : Boolean;
      Amount : U256;
      Min_Stake : Khepri_Types.Uint256;
      Current_Stake : Khepri_Types.Uint256;
      Total_Staked : Khepri_Types.Uint256;
      Val_Count : Khepri_Types.Uint256;
      Sender_Key : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      Parse_U256 (Args, Args_Size, 0, Amount, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      --  Check minimum stake
      Khepri_State.SLoad (Slot_Stake_MinStake, Min_Stake);
      if Khepri_Types."<" ((Limbs => Amount.Limbs), Min_Stake) then
         return;  -- Below minimum
      end if;

      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);

      --  Get current stake
      Current_Stake := Khepri_State.Map_Get (Slot_Stake_Stakes, Sender_Key);
      declare
         Was_Validator : constant Boolean :=
            Current_Stake.Limbs (0) /= 0 or Current_Stake.Limbs (1) /= 0 or
            Current_Stake.Limbs (2) /= 0 or Current_Stake.Limbs (3) /= 0;
         New_Stake : Khepri_Types.Uint256;
      begin
         --  Add to stake
         New_Stake := Khepri_Types."+" (Current_Stake, (Limbs => Amount.Limbs));
         Khepri_State.Map_Set (Slot_Stake_Stakes, Sender_Key, New_Stake);

         --  Update total staked
         Khepri_State.SLoad (Slot_Stake_TotalStaked, Total_Staked);
         Total_Staked := Khepri_Types."+" (Total_Staked, (Limbs => Amount.Limbs));
         Khepri_State.SStore (Slot_Stake_TotalStaked, Total_Staked);

         --  Increment validator count if new validator
         if not Was_Validator then
            Khepri_State.SLoad (Slot_Stake_ValidatorCount, Val_Count);
            Val_Count := Khepri_Types."+" (Val_Count, Khepri_Types.One);
            Khepri_State.SStore (Slot_Stake_ValidatorCount, Val_Count);
         end if;

         Ret := (Limbs => New_Stake.Limbs);
      end;
      Success := True;
   end Staking_Stake;

   --  Unstake tokens
   procedure Staking_Unstake (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: amount (32 bytes)
      Parse_OK : Boolean;
      Amount : U256;
      Current_Stake : Khepri_Types.Uint256;
      Total_Staked : Khepri_Types.Uint256;
      Val_Count : Khepri_Types.Uint256;
      Sender_Key : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      Parse_U256 (Args, Args_Size, 0, Amount, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Current_Stake := Khepri_State.Map_Get (Slot_Stake_Stakes, Sender_Key);

      --  Check sufficient stake
      if Khepri_Types."<" (Current_Stake, (Limbs => Amount.Limbs)) then
         return;  -- Insufficient stake
      end if;

      declare
         New_Stake : Khepri_Types.Uint256;
         Is_Zero : Boolean;
      begin
         New_Stake := Khepri_Types."-" (Current_Stake, (Limbs => Amount.Limbs));
         Khepri_State.Map_Set (Slot_Stake_Stakes, Sender_Key, New_Stake);

         --  Update total staked
         Khepri_State.SLoad (Slot_Stake_TotalStaked, Total_Staked);
         Total_Staked := Khepri_Types."-" (Total_Staked, (Limbs => Amount.Limbs));
         Khepri_State.SStore (Slot_Stake_TotalStaked, Total_Staked);

         --  Decrement validator count if stake is now zero
         Is_Zero := New_Stake.Limbs (0) = 0 and New_Stake.Limbs (1) = 0 and
                    New_Stake.Limbs (2) = 0 and New_Stake.Limbs (3) = 0;
         if Is_Zero then
            Khepri_State.SLoad (Slot_Stake_ValidatorCount, Val_Count);
            if Val_Count.Limbs (0) > 0 then
               Val_Count := Khepri_Types."-" (Val_Count, Khepri_Types.One);
               Khepri_State.SStore (Slot_Stake_ValidatorCount, Val_Count);
            end if;
         end if;

         Ret := Amount;
      end;
      Success := True;
   end Staking_Unstake;

   --  Claim staking rewards
   procedure Staking_ClaimRewards (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Sender_Key : Khepri_Types.Uint256;
      Rewards : Khepri_Types.Uint256;
   begin
      Sender_Key := Khepri_State.Address_Key (Ctx_Sender);
      Rewards := Khepri_State.Map_Get (Slot_Stake_Rewards, Sender_Key);

      --  Clear rewards after claiming
      Khepri_State.Map_Set (Slot_Stake_Rewards, Sender_Key, Khepri_Types.Zero);

      Ret := (Limbs => Rewards.Limbs);
      Success := True;
   end Staking_ClaimRewards;

   --  Get stake for address
   procedure Staking_GetStake (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Parse_OK : Boolean;
      Account : Contract_Address;
      Account_Key : Khepri_Types.Uint256;
      Stake : Khepri_Types.Uint256;
   begin
      Parse_Address (Args, Args_Size, 0, Account, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Account_Key := Khepri_State.Address_Key (Account);
      Stake := Khepri_State.Map_Get (Slot_Stake_Stakes, Account_Key);
      Ret := (Limbs => Stake.Limbs);
      Success := True;
   end Staking_GetStake;

   --  Get total staked
   procedure Staking_GetTotalStaked (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Total : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_Stake_TotalStaked, Total);
      Ret := (Limbs => Total.Limbs);
      Success := True;
   end Staking_GetTotalStaked;

   --  Get rewards for address
   procedure Staking_GetRewards (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Parse_OK : Boolean;
      Account : Contract_Address;
      Account_Key : Khepri_Types.Uint256;
      Rewards : Khepri_Types.Uint256;
   begin
      Parse_Address (Args, Args_Size, 0, Account, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Account_Key := Khepri_State.Address_Key (Account);
      Rewards := Khepri_State.Map_Get (Slot_Stake_Rewards, Account_Key);
      Ret := (Limbs => Rewards.Limbs);
      Success := True;
   end Staking_GetRewards;

   --  Get validator count
   procedure Staking_GetValidatorCount (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Count : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_Stake_ValidatorCount, Count);
      Ret := (Limbs => Count.Limbs);
      Success := True;
   end Staking_GetValidatorCount;

   ---------------------------------------------------------------------------
   --  Governance Contract Handlers (On-chain Voting)
   ---------------------------------------------------------------------------

   --  Initialize governance contract
   procedure Gov_Initialize (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: quorum (32 bytes), voting_period (32 bytes)
      Parse_OK : Boolean;
      Quorum : U256;
      Voting_Period : U256;
   begin
      Parse_U256 (Args, Args_Size, 0, Quorum, Parse_OK);
      if not Parse_OK then
         --  Default quorum: 50%
         Quorum := (Limbs => (50, 0, 0, 0));
      end if;

      Parse_U256 (Args, Args_Size, 32, Voting_Period, Parse_OK);
      if not Parse_OK then
         --  Default: 100 blocks
         Voting_Period := (Limbs => (100, 0, 0, 0));
      end if;

      --  Initialize state
      Khepri_State.SStore (Slot_Gov_ProposalCount, Khepri_Types.Zero);
      Khepri_State.SStore (Slot_Gov_Quorum, (Limbs => Quorum.Limbs));
      Khepri_State.SStore (Slot_Gov_VotingPeriod, (Limbs => Voting_Period.Limbs));

      Ret := (Limbs => (1, 0, 0, 0));  -- success
      Success := True;
   end Gov_Initialize;

   --  Create a new proposal
   procedure Gov_CreateProposal (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: proposal_hash (32 bytes) - hash of proposal details
      Parse_OK : Boolean;
      Proposal_Hash : U256;
      Proposal_Count : Khepri_Types.Uint256;
      Proposal_Key : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      Parse_U256 (Args, Args_Size, 0, Proposal_Hash, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      --  Get next proposal ID
      Khepri_State.SLoad (Slot_Gov_ProposalCount, Proposal_Count);

      --  Store proposal (key = proposal_id, value = proposal_hash)
      Proposal_Key := Proposal_Count;
      Khepri_State.Map_Set (Slot_Gov_Proposals, Proposal_Key, (Limbs => Proposal_Hash.Limbs));

      --  Initialize vote count to 0
      Khepri_State.Map_Set (Slot_Gov_Votes, Proposal_Key, Khepri_Types.Zero);

      --  Increment proposal count
      Proposal_Count := Khepri_Types."+" (Proposal_Count, Khepri_Types.One);
      Khepri_State.SStore (Slot_Gov_ProposalCount, Proposal_Count);

      --  Return proposal ID (previous count)
      Ret := (Limbs => Khepri_Types."-" (Proposal_Count, Khepri_Types.One).Limbs);
      Success := True;
   end Gov_CreateProposal;

   --  Vote on a proposal
   procedure Gov_Vote (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: proposal_id (32 bytes), vote_power (32 bytes)
      --  In production, vote_power would come from staking
      Parse_OK : Boolean;
      Proposal_ID : U256;
      Vote_Power : U256;
      Current_Votes : Khepri_Types.Uint256;
      Proposal_Key : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      Parse_U256 (Args, Args_Size, 0, Proposal_ID, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      Parse_U256 (Args, Args_Size, 32, Vote_Power, Parse_OK);
      if not Parse_OK then
         --  Default: 1 vote
         Vote_Power := (Limbs => (1, 0, 0, 0));
      end if;

      Proposal_Key := (Limbs => Proposal_ID.Limbs);

      --  Add votes
      Current_Votes := Khepri_State.Map_Get (Slot_Gov_Votes, Proposal_Key);
      Current_Votes := Khepri_Types."+" (Current_Votes, (Limbs => Vote_Power.Limbs));
      Khepri_State.Map_Set (Slot_Gov_Votes, Proposal_Key, Current_Votes);

      Ret := (Limbs => Current_Votes.Limbs);
      Success := True;
   end Gov_Vote;

   --  Execute a proposal (if quorum reached)
   procedure Gov_Execute (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      --  Args: proposal_id (32 bytes)
      Parse_OK : Boolean;
      Proposal_ID : U256;
      Votes : Khepri_Types.Uint256;
      Quorum : Khepri_Types.Uint256;
      Proposal_Key : Khepri_Types.Uint256;
   begin
      Ret := U256_Zero;
      Success := False;

      Parse_U256 (Args, Args_Size, 0, Proposal_ID, Parse_OK);
      if not Parse_OK then
         return;
      end if;

      Proposal_Key := (Limbs => Proposal_ID.Limbs);

      --  Check if quorum reached
      Votes := Khepri_State.Map_Get (Slot_Gov_Votes, Proposal_Key);
      Khepri_State.SLoad (Slot_Gov_Quorum, Quorum);

      --  Simple check: votes >= quorum (would need total supply comparison in production)
      if not Khepri_Types."<" (Votes, Quorum) then
         --  Mark proposal as executed by setting to max value
         Khepri_State.Map_Set (Slot_Gov_Proposals, Proposal_Key,
            (Limbs => (16#FFFFFFFFFFFFFFFF#, 16#FFFFFFFFFFFFFFFF#,
                       16#FFFFFFFFFFFFFFFF#, 16#FFFFFFFFFFFFFFFF#)));
         Ret := (Limbs => (1, 0, 0, 0));  -- success
         Success := True;
      end if;
   end Gov_Execute;

   --  Get proposal hash
   procedure Gov_GetProposal (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Parse_OK : Boolean;
      Proposal_ID : U256;
      Proposal_Key : Khepri_Types.Uint256;
      Proposal_Hash : Khepri_Types.Uint256;
   begin
      Parse_U256 (Args, Args_Size, 0, Proposal_ID, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Proposal_Key := (Limbs => Proposal_ID.Limbs);
      Proposal_Hash := Khepri_State.Map_Get (Slot_Gov_Proposals, Proposal_Key);
      Ret := (Limbs => Proposal_Hash.Limbs);
      Success := True;
   end Gov_GetProposal;

   --  Get vote count for proposal
   procedure Gov_GetVotes (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      Parse_OK : Boolean;
      Proposal_ID : U256;
      Proposal_Key : Khepri_Types.Uint256;
      Votes : Khepri_Types.Uint256;
   begin
      Parse_U256 (Args, Args_Size, 0, Proposal_ID, Parse_OK);
      if not Parse_OK then
         Ret := U256_Zero;
         Success := False;
         return;
      end if;

      Proposal_Key := (Limbs => Proposal_ID.Limbs);
      Votes := Khepri_State.Map_Get (Slot_Gov_Votes, Proposal_Key);
      Ret := (Limbs => Votes.Limbs);
      Success := True;
   end Gov_GetVotes;

   --  Get total proposal count
   procedure Gov_GetProposalCount (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Count : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_Gov_ProposalCount, Count);
      Ret := (Limbs => Count.Limbs);
      Success := True;
   end Gov_GetProposalCount;

   --  Get quorum threshold
   procedure Gov_GetQuorum (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   ) is
      pragma Unreferenced (Args, Args_Size);
      Quorum : Khepri_Types.Uint256;
   begin
      Khepri_State.SLoad (Slot_Gov_Quorum, Quorum);
      Ret := (Limbs => Quorum.Limbs);
      Success := True;
   end Gov_GetQuorum;

   ---------------------------------------------------------------------------
   --  Entry Table Builders
   ---------------------------------------------------------------------------

   procedure Init_Entry (
      E       : out Entry_Point_Def;
      Name    : String;
      Handler : EP_Handler;
      View    : Boolean
   ) is
   begin
      E.Name := (others => ' ');
      E.Name_Len := Name'Length;
      E.Handler := Handler;
      E.Is_View := View;
      for I in 1 .. Name'Length loop
         E.Name (I) := Name (Name'First + I - 1);
      end loop;
   end Init_Entry;

   procedure Get_HelloCounter_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) is
   begin
      for I in EP_Index loop
         Entries (I) := (Name => (others => ' '), Name_Len => 0, Handler => null, Is_View => False);
      end loop;

      Init_Entry (Entries (0), "Initialize", HC_Initialize'Access, False);
      Init_Entry (Entries (1), "Increment", HC_Increment'Access, False);
      Init_Entry (Entries (2), "Increment_By", HC_Increment_By'Access, False);
      Init_Entry (Entries (3), "Reset", HC_Reset'Access, False);
      Init_Entry (Entries (4), "Get_Count", HC_Get_Count'Access, True);
      Init_Entry (Entries (5), "Get", HC_Get_Count'Access, True);
      Init_Entry (Entries (6), "Get_Owner", HC_Get_Owner'Access, True);

      Entry_Count := 7;
   end Get_HelloCounter_Entries;

   procedure Get_SimpleToken_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) is
   begin
      for I in EP_Index loop
         Entries (I) := (Name => (others => ' '), Name_Len => 0, Handler => null, Is_View => False);
      end loop;

      Init_Entry (Entries (0), "Initialize", Token_Initialize'Access, False);
      Init_Entry (Entries (1), "Transfer", Token_Transfer'Access, False);
      Init_Entry (Entries (2), "Approve", Token_Approve'Access, False);
      Init_Entry (Entries (3), "BalanceOf", Token_BalanceOf'Access, True);
      Init_Entry (Entries (4), "TotalSupply", Token_TotalSupply'Access, True);
      Init_Entry (Entries (5), "Allowance", Token_Allowance'Access, True);

      Entry_Count := 6;
   end Get_SimpleToken_Entries;

   procedure Get_SimpleVault_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) is
   begin
      for I in EP_Index loop
         Entries (I) := (Name => (others => ' '), Name_Len => 0, Handler => null, Is_View => False);
      end loop;

      Init_Entry (Entries (0), "Initialize", Vault_Initialize'Access, False);
      Init_Entry (Entries (1), "Deposit", Vault_Deposit'Access, False);
      Init_Entry (Entries (2), "Withdraw", Vault_Withdraw'Access, False);
      Init_Entry (Entries (3), "BalanceOf", Vault_BalanceOf'Access, True);
      Init_Entry (Entries (4), "TotalDeposits", Vault_TotalDeposits'Access, True);

      Entry_Count := 5;
   end Get_SimpleVault_Entries;

   procedure Get_QuantumDID_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) is
   begin
      for I in EP_Index loop
         Entries (I) := (Name => (others => ' '), Name_Len => 0, Handler => null, Is_View => False);
      end loop;

      Init_Entry (Entries (0), "Register", DID_Register'Access, False);
      Init_Entry (Entries (1), "AddCredential", DID_AddCredential'Access, False);
      Init_Entry (Entries (2), "RevokeCredential", DID_RevokeCredential'Access, False);
      Init_Entry (Entries (3), "GetIdentity", DID_GetIdentity'Access, True);
      Init_Entry (Entries (4), "IsRegistered", DID_IsRegistered'Access, True);
      Init_Entry (Entries (5), "GetCredentialCount", DID_GetCredentialCount'Access, True);

      Entry_Count := 6;
   end Get_QuantumDID_Entries;

   procedure Get_Staking_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) is
   begin
      for I in EP_Index loop
         Entries (I) := (Name => (others => ' '), Name_Len => 0, Handler => null, Is_View => False);
      end loop;

      Init_Entry (Entries (0), "Initialize", Staking_Initialize'Access, False);
      Init_Entry (Entries (1), "Stake", Staking_Stake'Access, False);
      Init_Entry (Entries (2), "Unstake", Staking_Unstake'Access, False);
      Init_Entry (Entries (3), "ClaimRewards", Staking_ClaimRewards'Access, False);
      Init_Entry (Entries (4), "GetStake", Staking_GetStake'Access, True);
      Init_Entry (Entries (5), "GetTotalStaked", Staking_GetTotalStaked'Access, True);
      Init_Entry (Entries (6), "GetRewards", Staking_GetRewards'Access, True);
      Init_Entry (Entries (7), "GetValidatorCount", Staking_GetValidatorCount'Access, True);

      Entry_Count := 8;
   end Get_Staking_Entries;

   procedure Get_Governance_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) is
   begin
      for I in EP_Index loop
         Entries (I) := (Name => (others => ' '), Name_Len => 0, Handler => null, Is_View => False);
      end loop;

      Init_Entry (Entries (0), "Initialize", Gov_Initialize'Access, False);
      Init_Entry (Entries (1), "CreateProposal", Gov_CreateProposal'Access, False);
      Init_Entry (Entries (2), "Vote", Gov_Vote'Access, False);
      Init_Entry (Entries (3), "Execute", Gov_Execute'Access, False);
      Init_Entry (Entries (4), "GetProposal", Gov_GetProposal'Access, True);
      Init_Entry (Entries (5), "GetVotes", Gov_GetVotes'Access, True);
      Init_Entry (Entries (6), "GetProposalCount", Gov_GetProposalCount'Access, True);
      Init_Entry (Entries (7), "GetQuorum", Gov_GetQuorum'Access, True);

      Entry_Count := 8;
   end Get_Governance_Entries;

   ---------------------------------------------------------------------------
   --  Contract Type Detection
   ---------------------------------------------------------------------------

   function Detect_Contract_Type (
      Manifest_Name : String
   ) return Contract_Type is
      --  Helper to check if a substring exists in the name (case-insensitive)
      function Contains (Haystack, Needle : String) return Boolean is
      begin
         if Needle'Length > Haystack'Length then
            return False;
         end if;
         for I in Haystack'First .. Haystack'Last - Needle'Length + 1 loop
            declare
               Match : Boolean := True;
            begin
               for J in 0 .. Needle'Length - 1 loop
                  declare
                     H : Character := Haystack (I + J);
                     N : Character := Needle (Needle'First + J);
                  begin
                     --  Simple case-insensitive comparison
                     if H >= 'A' and H <= 'Z' then
                        H := Character'Val (Character'Pos (H) + 32);
                     end if;
                     if N >= 'A' and N <= 'Z' then
                        N := Character'Val (Character'Pos (N) + 32);
                     end if;
                     if H /= N then
                        Match := False;
                        exit;
                     end if;
                  end;
               end loop;
               if Match then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Contains;
   begin
      --  Check for known contract type names (anywhere in manifest name)
      --  Order matters - check most specific first
      if Contains (Manifest_Name, "DID") or Contains (Manifest_Name, "Identity") then
         return Contract_QuantumDID;
      end if;

      if Contains (Manifest_Name, "Staking") or Contains (Manifest_Name, "Stake") then
         return Contract_Staking;
      end if;

      if Contains (Manifest_Name, "Governance") or Contains (Manifest_Name, "Vote") then
         return Contract_Governance;
      end if;

      if Contains (Manifest_Name, "Token") then
         return Contract_SimpleToken;
      end if;

      if Contains (Manifest_Name, "Vault") then
         return Contract_SimpleVault;
      end if;

      if Contains (Manifest_Name, "Counter") then
         return Contract_HelloCounter;
      end if;

      --  Default to Unknown (will fail if invoked)
      return Contract_Unknown;
   end Detect_Contract_Type;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (Exec : out Executor_State) is
   begin
      Exec.Is_Initialized := True;
      Exec.Native_Count := 0;
      Exec.Current_Sender := (others => 0);
      Exec.Current_Self := (others => 0);
      Exec.Current_Value := U256_Zero;
      Exec.Current_Gas := 0;

      for I in Native_Index loop
         Exec.Natives (I).Is_Registered := False;
         Exec.Natives (I).Entry_Count := 0;
         Exec.Natives (I).Code_Hash_Val := (others => 0);
         Exec.Natives (I).Contract_Addr := (others => 0);
         Exec.Natives (I).Kind := Contract_Unknown;
         Exec.Natives (I).ELF_Contract := Sphinx_Native.Invalid_Contract;
         Exec.Natives (I).Is_ELF_Valid := False;
         for J in EP_Index loop
            Exec.Natives (I).Entries (J) := (
               Name     => (others => ' '),
               Name_Len => 0,
               Handler  => null,
               Is_View  => False
            );
         end loop;
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Native Contract Registration
   ---------------------------------------------------------------------------

   procedure Register_Native (
      Exec        : in Out Executor_State;
      Code_Hash   : in     Hash256;
      Entries     : in     EP_Table;
      Entry_Count : in     Natural;
      Success     : out    Boolean
   ) is
      Slot : Native_Index;
   begin
      Success := False;

      if Exec.Native_Count >= Max_Native_Contracts then
         return;
      end if;

      Slot := Native_Index (Exec.Native_Count);

      Exec.Natives (Slot).Code_Hash_Val := Code_Hash;
      Exec.Natives (Slot).Entry_Count := Entry_Count;
      Exec.Natives (Slot).Is_Registered := True;

      for I in 0 .. Entry_Count - 1 loop
         Exec.Natives (Slot).Entries (EP_Index (I)) := Entries (EP_Index (I));
      end loop;

      Exec.Native_Count := Exec.Native_Count + 1;
      Success := True;
   end Register_Native;

   ---------------------------------------------------------------------------
   --  Execution
   ---------------------------------------------------------------------------

   procedure Set_Ret_Error (
      Ret : in Out Invoke_Result;
      Msg : String
   ) is
      Len : constant Natural := Natural'Min (Msg'Length, Max_Error_Message_Length);
   begin
      Ret.Success := False;
      Ret.Error_Code := Error_Execution_Failed;
      Ret.Error_Msg := (others => ' ');
      for I in 1 .. Len loop
         Ret.Error_Msg (I) := Msg (Msg'First + I - 1);
      end loop;
      Ret.Error_Msg_Len := Len;
   end Set_Ret_Error;

   procedure Execute (
      Exec         : in Out Executor_State;
      Registry     : in     Registry_State;
      Contract_Idx : in     Stored_Contract_Index;
      From         : in     Contract_Address;
      EP_Name      : in     Entry_Name;
      EP_Name_Len  : in     Natural;
      Args         : in     Args_Buffer;
      Args_Size    : in     Natural;
      Gas_Limit    : in     Gas_Amount;
      Value        : in     U256;
      Is_View      : in     Boolean;
      Ret          : out    Invoke_Result
   ) is
      Stored_Hash  : Hash256;
      Found_Native : Boolean := False;
      Native_Idx   : Native_Index := 0;
      Found_EP     : Boolean := False;
      EP_Idx       : EP_Index := 0;
      Exec_Ret     : U256;
      Exec_OK      : Boolean;
      pragma Unreferenced (Is_View);
   begin
      --  Initialize result
      Ret.Success := False;
      Ret.Gas_Used := 0;
      Ret.Return_Size := 0;
      Ret.Log_Count := 0;
      Ret.Error_Code := Error_None;
      Ret.Error_Msg := (others => ' ');
      Ret.Error_Msg_Len := 0;
      for I in Return_Index loop
         Ret.Return_Data (I) := 0;
      end loop;
      for L in Log_Index loop
         Ret.Logs (L).Contract := (others => 0);
         Ret.Logs (L).Topic_Hash := (others => 0);
         Ret.Logs (L).Data := (others => 0);
         Ret.Logs (L).Data_Size := 0;
      end loop;

      --  Get contract info
      Stored_Hash := Registry.Contracts (Contract_Idx).Code_Hash;

      --  Set execution context
      Ctx_Sender := From;
      Ctx_Self := Registry.Contracts (Contract_Idx).Contract_ID;
      Ctx_Value := Value;

      Exec.Current_Sender := From;
      Exec.Current_Self := Registry.Contracts (Contract_Idx).Contract_ID;
      Exec.Current_Value := Value;
      Exec.Current_Gas := Gas_Limit;

      --  Find matching native contract by code hash
      for I in 0 .. Native_Index'Min (Native_Index (Exec.Native_Count), Native_Index'Last) loop
         exit when I >= Native_Index (Exec.Native_Count);
         if Exec.Natives (I).Is_Registered and then
            Exec.Natives (I).Code_Hash_Val = Stored_Hash
         then
            Found_Native := True;
            Native_Idx := I;
            exit;
         end if;
      end loop;

      if not Found_Native then
         --  Auto-register contract
         declare
            Manifest_Name_Str : String (1 .. 32);
            Manifest_Len : constant Natural := Registry.Contracts (Contract_Idx).Manifest.Name_Len;
            Kind : Contract_Type;
            New_Entries : EP_Table;
            New_Count : Natural;
            Code_Ref : Node_Code_Buffer renames Registry.Contracts (Contract_Idx).Code;
            Code_Len : constant Natural := Registry.Contracts (Contract_Idx).Code_Size;
            ELF_Contract : Sphinx_Native.Loaded_Contract;
            ELF_Load_OK : Boolean;
         begin
            --  PRIORITY 1: Check if code is a native ELF binary
            if Is_ELF_Binary (Code_Ref, Code_Len) then
               Ada.Text_IO.Put_Line ("  Detected ELF binary, attempting native load...");

               Try_Load_ELF (Code_Ref, Code_Len, ELF_Contract, ELF_Load_OK);

               if ELF_Load_OK then
                  --  Register as native ELF contract
                  if Exec.Native_Count < Max_Native_Contracts then
                     Native_Idx := Native_Index (Exec.Native_Count);
                     Exec.Natives (Native_Idx).Code_Hash_Val := Stored_Hash;
                     Exec.Natives (Native_Idx).Contract_Addr := Ctx_Self;
                     Exec.Natives (Native_Idx).Kind := Contract_NativeELF;
                     Exec.Natives (Native_Idx).Entry_Count := 0;  -- ELF uses symbol table
                     Exec.Natives (Native_Idx).Is_Registered := True;
                     Exec.Natives (Native_Idx).ELF_Contract := ELF_Contract;
                     Exec.Natives (Native_Idx).Is_ELF_Valid := True;
                     Exec.Native_Count := Exec.Native_Count + 1;
                     Found_Native := True;
                     Ada.Text_IO.Put_Line ("  Registered as Contract_NativeELF");
                  end if;
               else
                  Ada.Text_IO.Put_Line ("  ELF load failed, falling back to manifest detection");
               end if;
            end if;

            --  PRIORITY 2: Fall back to manifest-based built-in contracts
            if not Found_Native then
               --  Extract manifest name
               for I in 1 .. Manifest_Len loop
                  Manifest_Name_Str (I) := Registry.Contracts (Contract_Idx).Manifest.Name (I);
               end loop;
               for I in Manifest_Len + 1 .. 32 loop
                  Manifest_Name_Str (I) := ' ';
               end loop;

               Kind := Detect_Contract_Type (Manifest_Name_Str (1 .. Manifest_Len));

               --  Get entries for this contract type
               case Kind is
                  when Contract_SimpleToken =>
                     Get_SimpleToken_Entries (New_Entries, New_Count);
                  when Contract_SimpleVault =>
                     Get_SimpleVault_Entries (New_Entries, New_Count);
                  when Contract_QuantumDID =>
                     Get_QuantumDID_Entries (New_Entries, New_Count);
                  when Contract_Staking =>
                     Get_Staking_Entries (New_Entries, New_Count);
                  when Contract_Governance =>
                     Get_Governance_Entries (New_Entries, New_Count);
                  when Contract_NativeELF =>
                     --  Already handled above
                     New_Count := 0;
                  when Contract_HelloCounter =>
                     Get_HelloCounter_Entries (New_Entries, New_Count);
                  when Contract_Unknown =>
                     New_Count := 0;  -- Will fail on invoke
               end case;

               --  Register it
               if Exec.Native_Count < Max_Native_Contracts then
                  Native_Idx := Native_Index (Exec.Native_Count);
                  Exec.Natives (Native_Idx).Code_Hash_Val := Stored_Hash;
                  Exec.Natives (Native_Idx).Contract_Addr := Ctx_Self;
                  Exec.Natives (Native_Idx).Kind := Kind;
                  Exec.Natives (Native_Idx).Entry_Count := New_Count;
                  Exec.Natives (Native_Idx).Is_Registered := True;
                  Exec.Natives (Native_Idx).Is_ELF_Valid := False;
                  for J in 0 .. New_Count - 1 loop
                     Exec.Natives (Native_Idx).Entries (EP_Index (J)) := New_Entries (EP_Index (J));
                  end loop;
                  Exec.Native_Count := Exec.Native_Count + 1;
                  Found_Native := True;
               end if;
            end if;
         end;
      end if;

      if not Found_Native then
         Set_Ret_Error (Ret, "No native handler");
         return;
      end if;

      --  DISPATCH: Route to appropriate execution path based on contract kind
      if Exec.Natives (Native_Idx).Kind = Contract_NativeELF and then
         Exec.Natives (Native_Idx).Is_ELF_Valid
      then
         --  Execute via native ELF loader (symbol table lookup)
         Ada.Text_IO.Put_Line ("  Executing via SPHINX native ELF loader...");
         Execute_ELF (
            Exec       => Exec,
            Native_Idx => Native_Idx,
            EP_Name    => EP_Name,
            EP_Name_Len => EP_Name_Len,
            Args       => Args,
            Args_Size  => Args_Size,
            Gas_Limit  => Gas_Limit,
            Ret        => Ret
         );
         return;
      end if;

      --  BUILT-IN CONTRACTS: Use EP_Table lookup
      --  Find matching entry point
      for I in 0 .. EP_Index'Min (EP_Index (Exec.Natives (Native_Idx).Entry_Count), EP_Index'Last) loop
         exit when I >= EP_Index (Exec.Natives (Native_Idx).Entry_Count);
         declare
            E : Entry_Point_Def renames Exec.Natives (Native_Idx).Entries (I);
         begin
            if E.Name_Len = EP_Name_Len and then
               E.Name (1 .. E.Name_Len) = EP_Name (1 .. EP_Name_Len)
            then
               Found_EP := True;
               EP_Idx := I;
               exit;
            end if;
         end;
      end loop;

      if not Found_EP then
         Set_Ret_Error (Ret, "Entry not found");
         return;
      end if;

      --  Execute the handler
      declare
         E : Entry_Point_Def renames Exec.Natives (Native_Idx).Entries (EP_Idx);
      begin
         if E.Handler /= null then
            E.Handler.all (Args, Args_Size, Exec_Ret, Exec_OK);

            if Exec_OK then
               Ret.Success := True;
               Ret.Gas_Used := 1000;

               --  Encode U256 result as 32 bytes (big-endian)
               for I in 0 .. 3 loop
                  declare
                     Limb : constant Word64 := Exec_Ret.Limbs (3 - I);
                  begin
                     for J in 0 .. 7 loop
                        Ret.Return_Data (Return_Index (I * 8 + J)) :=
                           Byte (Shift_Right (Limb, (7 - J) * 8) and 16#FF#);
                     end loop;
                  end;
               end loop;
               Ret.Return_Size := 32;
            else
               Set_Ret_Error (Ret, "Handler failed");
            end if;
         else
            Set_Ret_Error (Ret, "Null handler");
         end if;
      end;
   end Execute;

   ---------------------------------------------------------------------------
   --  Native ELF Contract Loading
   ---------------------------------------------------------------------------

   function Is_ELF_Binary (
      Code      : Node_Code_Buffer;
      Code_Size : Natural
   ) return Boolean is
   begin
      --  ELF magic: 0x7F 'E' 'L' 'F'
      if Code_Size < 4 then
         return False;
      end if;

      return Code (0) = 16#7F# and
             Code (1) = 16#45# and  -- 'E'
             Code (2) = 16#4C# and  -- 'L'
             Code (3) = 16#46#;     -- 'F'
   end Is_ELF_Binary;

   procedure Try_Load_ELF (
      Code      : in     Node_Code_Buffer;
      Code_Size : in     Natural;
      Contract  :    out Sphinx_Native.Loaded_Contract;
      Success   :    out Boolean
   ) is
      Binary : Sphinx_Native.Binary_Buffer;
      Result : Sphinx_Native.Load_Result;
   begin
      Contract := Sphinx_Native.Invalid_Contract;
      Success := False;

      --  Check if it looks like ELF
      if not Is_ELF_Binary (Code, Code_Size) then
         return;
      end if;

      --  Check size limit
      if Code_Size > Sphinx_Native.Max_Binary_Size then
         Ada.Text_IO.Put_Line ("  ELF too large: " & Code_Size'Image &
            " > " & Sphinx_Native.Max_Binary_Size'Image);
         return;
      end if;

      --  Copy code to binary buffer
      for I in 0 .. Code_Size - 1 loop
         Binary (Sphinx_Native.Binary_Index (I)) := Code (Node_Code_Index (I));
      end loop;
      --  Zero the rest
      for I in Code_Size .. Sphinx_Native.Max_Binary_Size - 1 loop
         Binary (Sphinx_Native.Binary_Index (I)) := 0;
      end loop;

      --  Try to load the ELF
      Result := Sphinx_Native.Load_ELF (Binary, Code_Size);

      if Result.Error = Sphinx_Native.Load_Success then
         Contract := Result.Contract;
         Ada.Text_IO.Put_Line ("  ELF loaded successfully:");
         Ada.Text_IO.Put_Line ("    Architecture: " & Result.Contract.Arch'Image);
         Ada.Text_IO.Put_Line ("    Entry: 0x" & Word64'Image (Word64 (Result.Contract.Entry_Addr)));
         Ada.Text_IO.Put_Line ("    Code size: " & Result.Contract.Code_Size'Image);
         Success := True;
      else
         Ada.Text_IO.Put_Line ("  ELF load failed: " & Result.Error'Image);
      end if;
   end Try_Load_ELF;

   ---------------------------------------------------------------------------
   --  Native ELF Contract Execution
   ---------------------------------------------------------------------------

   procedure Execute_ELF (
      Exec         : in Out Executor_State;
      Native_Idx   : in     Native_Index;
      EP_Name      : in     Entry_Name;
      EP_Name_Len  : in     Natural;
      Args         : in     Args_Buffer;
      Args_Size    : in     Natural;
      Gas_Limit    : in     Gas_Amount;
      Ret          : out    Invoke_Result
   ) is
      pragma Unreferenced (EP_Name, EP_Name_Len);
      Contract  : Sphinx_Native.Loaded_Contract renames Exec.Natives (Native_Idx).ELF_Contract;
      Sandbox   : Aegis_Sandbox.Sandbox_Context;
      Calldata  : Byte_Array (0 .. Args_Size - 1);
      Result    : Sphinx_Native.Exec_Result;
   begin
      --  Initialize result
      Ret.Success := False;
      Ret.Gas_Used := 0;
      Ret.Return_Size := 0;
      Ret.Log_Count := 0;
      Ret.Error_Code := Error_None;
      Ret.Error_Msg := (others => ' ');
      Ret.Error_Msg_Len := 0;
      for I in Return_Index loop
         Ret.Return_Data (I) := 0;
      end loop;
      for L in Log_Index loop
         Ret.Logs (L).Contract := (others => 0);
         Ret.Logs (L).Topic_Hash := (others => 0);
         Ret.Logs (L).Data := (others => 0);
         Ret.Logs (L).Data_Size := 0;
      end loop;

      --  Copy args to calldata
      for I in 0 .. Args_Size - 1 loop
         Calldata (I) := Args (I);
      end loop;

      --  Initialize sandbox
      --  Note: Full sandbox initialization would include proper call frame,
      --  storage access set, snapshots, etc. For now we use defaults.
      Sandbox.Status := Aegis_Sandbox.Sandbox_Running;
      Sandbox.Depth := 0;
      Sandbox.Memory_Size := 0;
      Sandbox.Snapshot_Count := 0;
      Sandbox.Gas.Gas_Used := 0;
      Sandbox.Gas.Gas_Limit := Gas_Limit;
      Sandbox.Current_Frame.Caller := Ctx_Sender;
      Sandbox.Current_Frame.Callee := Ctx_Self;
      Sandbox.Current_Frame.Gas_Limit := Gas_Limit;
      Sandbox.Current_Frame.Gas_Used := 0;
      Sandbox.Current_Frame.Depth := 0;
      Sandbox.Current_Frame.Is_Static := False;

      Ada.Text_IO.Put_Line ("  Executing native ELF contract...");
      Ada.Text_IO.Put_Line ("    Entry: 0x" & Word64'Image (Word64 (Contract.Entry_Addr)));
      Ada.Text_IO.Put_Line ("    Args size: " & Args_Size'Image);
      Ada.Text_IO.Put_Line ("    Gas limit: " & Gas_Limit'Image);

      --  Execute the contract
      Result := Sphinx_Native.Execute (Contract, Sandbox, Calldata, Gas_Limit);

      case Result.Status is
         when Sphinx_Native.Exec_Success =>
            Ret.Success := True;
            Ret.Gas_Used := Result.Gas_Used;
            --  Copy return data hash to return buffer
            for I in 0 .. 31 loop
               Ret.Return_Data (Return_Index (I)) := Result.Return_Data (I);
            end loop;
            Ret.Return_Size := 32;
            Ada.Text_IO.Put_Line ("    Execution success, gas used: " & Result.Gas_Used'Image);

         when Sphinx_Native.Exec_Out_Of_Gas =>
            Set_Ret_Error (Ret, "Out of gas");
            Ada.Text_IO.Put_Line ("    Execution failed: Out of gas");

         when Sphinx_Native.Exec_Reverted =>
            Set_Ret_Error (Ret, "Reverted");
            Ada.Text_IO.Put_Line ("    Execution failed: Reverted");

         when Sphinx_Native.Exec_Security_Violation =>
            Set_Ret_Error (Ret, "Security violation");
            Ada.Text_IO.Put_Line ("    Execution failed: Security violation");

         when others =>
            Set_Ret_Error (Ret, "Execution error");
            Ada.Text_IO.Put_Line ("    Execution failed: " & Result.Status'Image);
      end case;
   end Execute_ELF;

end Node_Contract_Executor;
