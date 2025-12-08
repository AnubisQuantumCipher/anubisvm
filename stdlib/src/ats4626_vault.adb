--  ATS-4626: KHEPRI Tokenized Vault Implementation
pragma SPARK_Mode (On);

package body ATS4626_Vault with
   SPARK_Mode => On,
   Refined_State => (Vault_State =>
      (Config_Store, Owner_Addr, Total_Shares, Total_Asset_Balance,
       Share_Balances, Share_Allowances))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   Max_Allowances : constant := 2048;

   type Balance_Entry is record
      Account : Address;
      Amount  : Share_Amount;
      Used    : Boolean;
   end record;

   type Balance_Array is array (0 .. Max_Depositors - 1) of Balance_Entry;

   type Allowance_Entry is record
      Owner_Acct : Address;
      Spender    : Address;
      Amount     : Share_Amount;
      Used       : Boolean;
   end record;

   type Allowance_Array is array (0 .. Max_Allowances - 1) of Allowance_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Config_Store       : Vault_Config := Default_Config;
   Owner_Addr         : Address := Zero_Address;
   Total_Shares       : Share_Amount := U256_Zero;
   Total_Asset_Balance: Asset_Amount := U256_Zero;

   Share_Balances : Balance_Array := (others => (
      Account => Zero_Address,
      Amount  => U256_Zero,
      Used    => False
   ));

   Share_Allowances : Allowance_Array := (others => (
      Owner_Acct => Zero_Address,
      Spender    => Zero_Address,
      Amount     => U256_Zero,
      Used       => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Balance (Account : Address) return Natural is
   begin
      for I in Share_Balances'Range loop
         if Share_Balances (I).Used
            and then Share_Balances (I).Account = Account
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Balance;

   function Find_Or_Create_Balance (Account : Address) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Share_Balances'Range loop
         if Share_Balances (I).Used then
            if Share_Balances (I).Account = Account then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Balance;

   function Find_Allowance (Owner_Acct, Spender : Address) return Natural is
   begin
      for I in Share_Allowances'Range loop
         if Share_Allowances (I).Used
            and then Share_Allowances (I).Owner_Acct = Owner_Acct
            and then Share_Allowances (I).Spender = Spender
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Allowance;

   function Find_Or_Create_Allowance (
      Owner_Acct, Spender : Address
   ) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Share_Allowances'Range loop
         if Share_Allowances (I).Used then
            if Share_Allowances (I).Owner_Acct = Owner_Acct
               and then Share_Allowances (I).Spender = Spender
            then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Allowance;

   --  Convert assets to shares with rounding down
   function Assets_To_Shares (Assets : Asset_Amount) return Share_Amount is
      Quotient, Remainder : U256;
   begin
      if Equal (Total_Shares, U256_Zero) or Equal (Total_Asset_Balance, U256_Zero) then
         return Assets;  --  1:1 for empty vault
      end if;

      --  shares = (assets * totalShares) / totalAssets
      declare
         Product : U256;
      begin
         Product := Mul_Mod (Assets, Total_Shares);
         Div_Mod (Product, Total_Asset_Balance, Quotient, Remainder);
         return Quotient;
      end;
   end Assets_To_Shares;

   --  Convert shares to assets with rounding down
   function Shares_To_Assets (Shares : Share_Amount) return Asset_Amount is
      Quotient, Remainder : U256;
   begin
      if Equal (Total_Shares, U256_Zero) then
         return U256_Zero;
      end if;

      --  assets = (shares * totalAssets) / totalShares
      declare
         Product : U256;
      begin
         Product := Mul_Mod (Shares, Total_Asset_Balance);
         Div_Mod (Product, Total_Shares, Quotient, Remainder);
         return Quotient;
      end;
   end Shares_To_Assets;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Vault_Config;
      Owner   : in Address;
      Success : out Boolean
   ) is
   begin
      if Owner_Addr /= Zero_Address then
         Success := False;
         return;
      end if;

      Config_Store := Config;
      Owner_Addr := Owner;
      Total_Shares := U256_Zero;
      Total_Asset_Balance := U256_Zero;
      Success := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  ERC-20 Compatible Functions
   ---------------------------------------------------------------------------

   function Name return Vault_Name is
   begin
      return Config_Store.Name;
   end Name;

   function Symbol return Vault_Symbol is
   begin
      return Config_Store.Symbol;
   end Symbol;

   function Decimals return Natural is
   begin
      return Config_Store.Decimals;
   end Decimals;

   function Total_Supply return Share_Amount is
   begin
      return Total_Shares;
   end Total_Supply;

   function Balance_Of (Account : Address) return Share_Amount is
      Slot : constant Natural := Find_Balance (Account);
   begin
      if Slot < Max_Depositors then
         return Share_Balances (Slot).Amount;
      else
         return U256_Zero;
      end if;
   end Balance_Of;

   function Allowance (Owner_Addr, Spender : Address) return Share_Amount is
      Slot : constant Natural := Find_Allowance (Owner_Addr, Spender);
   begin
      if Slot < Max_Allowances then
         return Share_Allowances (Slot).Amount;
      else
         return U256_Zero;
      end if;
   end Allowance;

   procedure Transfer (
      Caller    : in     Address;
      Recipient : in     Address;
      Amount    : in     Share_Amount;
      Success   : out    Boolean;
      Error     : out    Vault_Error
   ) is
      Sender_Bal  : Share_Amount;
      Recv_Bal    : Share_Amount;
      New_Recv    : U256;
      Overflow    : Boolean;
      Sender_Slot : Natural;
      Recv_Slot   : Natural;
   begin
      Sender_Bal := Balance_Of (Caller);
      if Less_Than (Sender_Bal, Amount) then
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      Recv_Bal := Balance_Of (Recipient);
      Add (Recv_Bal, Amount, New_Recv, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Sender_Slot := Find_Or_Create_Balance (Caller);
      Recv_Slot := Find_Or_Create_Balance (Recipient);

      if Sender_Slot < Max_Depositors then
         Share_Balances (Sender_Slot).Amount := Sub_Mod (Sender_Bal, Amount);
      end if;

      if Recv_Slot < Max_Depositors then
         Share_Balances (Recv_Slot) := (
            Account => Recipient,
            Amount  => New_Recv,
            Used    => True
         );
      end if;

      Success := True;
      Error := Error_None;
   end Transfer;

   procedure Approve (
      Caller  : in     Address;
      Spender : in     Address;
      Amount  : in     Share_Amount;
      Success : out    Boolean;
      Error   : out    Vault_Error
   ) is
      Slot : constant Natural := Find_Or_Create_Allowance (Caller, Spender);
   begin
      if Slot < Max_Allowances then
         Share_Allowances (Slot) := (
            Owner_Acct => Caller,
            Spender    => Spender,
            Amount     => Amount,
            Used       => True
         );
         Success := True;
         Error := Error_None;
      else
         Success := False;
         Error := Error_Overflow;
      end if;
   end Approve;

   procedure Transfer_From (
      Caller    : in     Address;
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Amount    : in     Share_Amount;
      Success   : out    Boolean;
      Error     : out    Vault_Error
   ) is
      From_Bal    : Share_Amount;
      Allow       : Share_Amount;
      To_Bal      : Share_Amount;
      New_To      : U256;
      Overflow    : Boolean;
      From_Slot   : Natural;
      To_Slot     : Natural;
      Allow_Slot  : Natural;
   begin
      From_Bal := Balance_Of (From_Addr);
      if Less_Than (From_Bal, Amount) then
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      Allow := Allowance (From_Addr, Caller);
      if Less_Than (Allow, Amount) then
         Success := False;
         Error := Error_Insufficient_Allowance;
         return;
      end if;

      To_Bal := Balance_Of (To_Addr);
      Add (To_Bal, Amount, New_To, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      From_Slot := Find_Or_Create_Balance (From_Addr);
      To_Slot := Find_Or_Create_Balance (To_Addr);
      Allow_Slot := Find_Allowance (From_Addr, Caller);

      if From_Slot < Max_Depositors then
         Share_Balances (From_Slot).Amount := Sub_Mod (From_Bal, Amount);
      end if;

      if To_Slot < Max_Depositors then
         Share_Balances (To_Slot) := (
            Account => To_Addr,
            Amount  => New_To,
            Used    => True
         );
      end if;

      if Allow_Slot < Max_Allowances then
         Share_Allowances (Allow_Slot).Amount := Sub_Mod (Allow, Amount);
      end if;

      Success := True;
      Error := Error_None;
   end Transfer_From;

   ---------------------------------------------------------------------------
   --  ERC-4626 Asset Functions
   ---------------------------------------------------------------------------

   function Asset return Address is
   begin
      return Config_Store.Asset_Token;
   end Asset;

   function Total_Assets return Asset_Amount is
   begin
      return Total_Asset_Balance;
   end Total_Assets;

   ---------------------------------------------------------------------------
   --  ERC-4626 Conversion Functions
   ---------------------------------------------------------------------------

   function Convert_To_Shares (Assets : Asset_Amount) return Share_Amount is
   begin
      return Assets_To_Shares (Assets);
   end Convert_To_Shares;

   function Convert_To_Assets (Shares : Share_Amount) return Asset_Amount is
   begin
      return Shares_To_Assets (Shares);
   end Convert_To_Assets;

   ---------------------------------------------------------------------------
   --  ERC-4626 Deposit/Mint Functions
   ---------------------------------------------------------------------------

   function Max_Deposit (Receiver : Address) return Asset_Amount is
      pragma Unreferenced (Receiver);
   begin
      return U256_Max;
   end Max_Deposit;

   function Preview_Deposit (Assets : Asset_Amount) return Share_Amount is
   begin
      return Assets_To_Shares (Assets);
   end Preview_Deposit;

   procedure Deposit (
      Caller   : in     Address;
      Assets   : in     Asset_Amount;
      Receiver : in     Address;
      Shares   : out    Share_Amount;
      Success  : out    Boolean;
      Error    : out    Vault_Error
   ) is
      pragma Unreferenced (Caller);
      New_Bal      : U256;
      New_Total    : U256;
      New_Assets   : U256;
      Overflow     : Boolean;
      Recv_Slot    : Natural;
   begin
      if Equal (Assets, U256_Zero) then
         Shares := U256_Zero;
         Success := False;
         Error := Error_Zero_Assets;
         return;
      end if;

      Shares := Assets_To_Shares (Assets);

      if Equal (Shares, U256_Zero) then
         Success := False;
         Error := Error_Zero_Shares;
         return;
      end if;

      --  Update receiver balance
      Recv_Slot := Find_Or_Create_Balance (Receiver);
      if Recv_Slot = Natural'Last then
         Shares := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Add (Share_Balances (Recv_Slot).Amount, Shares, New_Bal, Overflow);
      if Overflow then
         Shares := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Add (Total_Shares, Shares, New_Total, Overflow);
      if Overflow then
         Shares := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Add (Total_Asset_Balance, Assets, New_Assets, Overflow);
      if Overflow then
         Shares := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Share_Balances (Recv_Slot) := (
         Account => Receiver,
         Amount  => New_Bal,
         Used    => True
      );
      Total_Shares := New_Total;
      Total_Asset_Balance := New_Assets;

      Success := True;
      Error := Error_None;
   end Deposit;

   function Max_Mint (Receiver : Address) return Share_Amount is
      pragma Unreferenced (Receiver);
   begin
      return U256_Max;
   end Max_Mint;

   function Preview_Mint (Shares : Share_Amount) return Asset_Amount is
      Quotient, Remainder : U256;
   begin
      if Equal (Total_Shares, U256_Zero) then
         return Shares;
      end if;

      --  Round up: assets = (shares * totalAssets + totalShares - 1) / totalShares
      declare
         Product   : U256;
         Numerator : U256;
         Overflow  : Boolean;
      begin
         Product := Mul_Mod (Shares, Total_Asset_Balance);
         Add (Product, Total_Shares, Numerator, Overflow);
         if Overflow then
            return U256_Max;
         end if;
         Numerator := Sub_Mod (Numerator, From_Word64 (1));
         Div_Mod (Numerator, Total_Shares, Quotient, Remainder);
         return Quotient;
      end;
   end Preview_Mint;

   procedure Mint (
      Caller   : in     Address;
      Shares   : in     Share_Amount;
      Receiver : in     Address;
      Assets   : out    Asset_Amount;
      Success  : out    Boolean;
      Error    : out    Vault_Error
   ) is
      pragma Unreferenced (Caller);
      New_Bal      : U256;
      New_Total    : U256;
      New_Assets   : U256;
      Overflow     : Boolean;
      Recv_Slot    : Natural;
   begin
      if Equal (Shares, U256_Zero) then
         Assets := U256_Zero;
         Success := False;
         Error := Error_Zero_Shares;
         return;
      end if;

      Assets := Preview_Mint (Shares);

      Recv_Slot := Find_Or_Create_Balance (Receiver);
      if Recv_Slot = Natural'Last then
         Assets := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Add (Share_Balances (Recv_Slot).Amount, Shares, New_Bal, Overflow);
      if Overflow then
         Assets := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Add (Total_Shares, Shares, New_Total, Overflow);
      if Overflow then
         Assets := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Add (Total_Asset_Balance, Assets, New_Assets, Overflow);
      if Overflow then
         Assets := U256_Zero;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Share_Balances (Recv_Slot) := (
         Account => Receiver,
         Amount  => New_Bal,
         Used    => True
      );
      Total_Shares := New_Total;
      Total_Asset_Balance := New_Assets;

      Success := True;
      Error := Error_None;
   end Mint;

   ---------------------------------------------------------------------------
   --  ERC-4626 Withdraw/Redeem Functions
   ---------------------------------------------------------------------------

   function Max_Withdraw (Owner_Addr : Address) return Asset_Amount is
   begin
      return Shares_To_Assets (Balance_Of (Owner_Addr));
   end Max_Withdraw;

   function Preview_Withdraw (Assets : Asset_Amount) return Share_Amount is
      Quotient, Remainder : U256;
   begin
      if Equal (Total_Asset_Balance, U256_Zero) then
         return U256_Zero;
      end if;

      --  Round up: shares = (assets * totalShares + totalAssets - 1) / totalAssets
      declare
         Product   : U256;
         Numerator : U256;
         Overflow  : Boolean;
      begin
         Product := Mul_Mod (Assets, Total_Shares);
         Add (Product, Total_Asset_Balance, Numerator, Overflow);
         if Overflow then
            return U256_Max;
         end if;
         Numerator := Sub_Mod (Numerator, From_Word64 (1));
         Div_Mod (Numerator, Total_Asset_Balance, Quotient, Remainder);
         return Quotient;
      end;
   end Preview_Withdraw;

   procedure Withdraw (
      Caller     : in     Address;
      Assets     : in     Asset_Amount;
      Receiver   : in     Address;
      Owner_Addr : in     Address;
      Shares     : out    Share_Amount;
      Success    : out    Boolean;
      Error      : out    Vault_Error
   ) is
      pragma Unreferenced (Receiver);
      Owner_Bal    : Share_Amount;
      Allow        : Share_Amount;
      Owner_Slot   : Natural;
      Allow_Slot   : Natural;
   begin
      if Equal (Assets, U256_Zero) then
         Shares := U256_Zero;
         Success := False;
         Error := Error_Zero_Assets;
         return;
      end if;

      Shares := Preview_Withdraw (Assets);

      Owner_Bal := Balance_Of (Owner_Addr);
      if Less_Than (Owner_Bal, Shares) then
         Shares := U256_Zero;
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      --  Check allowance if caller is not owner
      if Caller /= Owner_Addr then
         Allow := Allowance (Owner_Addr, Caller);
         if Less_Than (Allow, Shares) then
            Shares := U256_Zero;
            Success := False;
            Error := Error_Insufficient_Allowance;
            return;
         end if;

         Allow_Slot := Find_Allowance (Owner_Addr, Caller);
         if Allow_Slot < Max_Allowances then
            Share_Allowances (Allow_Slot).Amount := Sub_Mod (Allow, Shares);
         end if;
      end if;

      Owner_Slot := Find_Balance (Owner_Addr);
      if Owner_Slot < Max_Depositors then
         Share_Balances (Owner_Slot).Amount := Sub_Mod (Owner_Bal, Shares);
      end if;

      Total_Shares := Sub_Mod (Total_Shares, Shares);
      Total_Asset_Balance := Sub_Mod (Total_Asset_Balance, Assets);

      Success := True;
      Error := Error_None;
   end Withdraw;

   function Max_Redeem (Owner_Addr : Address) return Share_Amount is
   begin
      return Balance_Of (Owner_Addr);
   end Max_Redeem;

   function Preview_Redeem (Shares : Share_Amount) return Asset_Amount is
   begin
      return Shares_To_Assets (Shares);
   end Preview_Redeem;

   procedure Redeem (
      Caller     : in     Address;
      Shares     : in     Share_Amount;
      Receiver   : in     Address;
      Owner_Addr : in     Address;
      Assets     : out    Asset_Amount;
      Success    : out    Boolean;
      Error      : out    Vault_Error
   ) is
      pragma Unreferenced (Receiver);
      Owner_Bal    : Share_Amount;
      Allow        : Share_Amount;
      Owner_Slot   : Natural;
      Allow_Slot   : Natural;
   begin
      if Equal (Shares, U256_Zero) then
         Assets := U256_Zero;
         Success := False;
         Error := Error_Zero_Shares;
         return;
      end if;

      Owner_Bal := Balance_Of (Owner_Addr);
      if Less_Than (Owner_Bal, Shares) then
         Assets := U256_Zero;
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      Assets := Shares_To_Assets (Shares);

      --  Check allowance if caller is not owner
      if Caller /= Owner_Addr then
         Allow := Allowance (Owner_Addr, Caller);
         if Less_Than (Allow, Shares) then
            Assets := U256_Zero;
            Success := False;
            Error := Error_Insufficient_Allowance;
            return;
         end if;

         Allow_Slot := Find_Allowance (Owner_Addr, Caller);
         if Allow_Slot < Max_Allowances then
            Share_Allowances (Allow_Slot).Amount := Sub_Mod (Allow, Shares);
         end if;
      end if;

      Owner_Slot := Find_Balance (Owner_Addr);
      if Owner_Slot < Max_Depositors then
         Share_Balances (Owner_Slot).Amount := Sub_Mod (Owner_Bal, Shares);
      end if;

      Total_Shares := Sub_Mod (Total_Shares, Shares);
      Total_Asset_Balance := Sub_Mod (Total_Asset_Balance, Assets);

      Success := True;
      Error := Error_None;
   end Redeem;

   ---------------------------------------------------------------------------
   --  Vault Management
   ---------------------------------------------------------------------------

   function Contract_Owner return Address is
   begin
      return Owner_Addr;
   end Contract_Owner;

   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Vault_Error
   ) is
   begin
      if Caller /= Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Owner_Addr := New_Owner;
      Success := True;
      Error := Error_None;
   end Transfer_Ownership;

   ---------------------------------------------------------------------------
   --  Interface Support
   ---------------------------------------------------------------------------

   function Supports_Interface (Interface_ID : Bytes4) return Boolean is
   begin
      return Interface_ID = ERC4626_Interface_ID;
   end Supports_Interface;

end ATS4626_Vault;
