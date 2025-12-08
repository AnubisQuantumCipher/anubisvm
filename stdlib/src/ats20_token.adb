--  ATS-20: KHEPRI Standard Token Implementation
pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;

package body ATS20_Token with
   SPARK_Mode => On,
   Refined_State => (Token_State => (Storage, Balances, Allowances))
is

   --  Constants
   Max_Accounts : constant := 1024;
   Max_Allowances : constant := 2048;

   --  Storage record
   type Token_Storage is record
      Name        : String (1 .. 64);
      Name_Len    : Natural;
      Symbol      : String (1 .. 16);
      Symbol_Len  : Natural;
      Decimals    : Natural;
      Total       : Token_Amount;
      Owner_Addr  : Address;
      Paused      : Boolean;
      Initialized : Boolean;
   end record;

   --  Balance entry
   type Balance_Entry is record
      Account : Address;
      Amount  : Token_Amount;
      Used    : Boolean;
   end record;
   type Balance_Array is array (0 .. Max_Accounts - 1) of Balance_Entry;

   --  Allowance entry
   type Allowance_Entry is record
      Owner_Addr : Address;
      Spender    : Address;
      Amount     : Token_Amount;
      Used       : Boolean;
   end record;
   type Allowance_Array is array (0 .. Max_Allowances - 1) of Allowance_Entry;

   --  State variables
   Storage : Token_Storage := (
      Name        => (others => ' '),
      Name_Len    => 0,
      Symbol      => (others => ' '),
      Symbol_Len  => 0,
      Decimals    => 18,
      Total       => U256_Zero,
      Owner_Addr  => Null_Address,
      Paused      => False,
      Initialized => False
   );

   Balances : Balance_Array := (others => (
      Account => Null_Address,
      Amount  => U256_Zero,
      Used    => False
   ));

   Allowances : Allowance_Array := (others => (
      Owner_Addr => Null_Address,
      Spender    => Null_Address,
      Amount     => U256_Zero,
      Used       => False
   ));

   --  Helper: Find balance slot
   function Find_Balance (Account : Address) return Natural is
   begin
      for I in Balances'Range loop
         if Balances (I).Used and then Balances (I).Account = Account then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Balance;

   --  Helper: Find or create balance slot
   function Find_Or_Create_Balance (Account : Address) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Balances'Range loop
         if Balances (I).Used then
            if Balances (I).Account = Account then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Balance;

   --  Helper: Find allowance slot
   function Find_Allow (Owner_A, Spender_A : Address) return Natural is
   begin
      for I in Allowances'Range loop
         if Allowances (I).Used
            and then Allowances (I).Owner_Addr = Owner_A
            and then Allowances (I).Spender = Spender_A
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Allow;

   --  Helper: Find or create allowance slot
   function Find_Or_Create_Allow (Owner_A, Spender_A : Address) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Allowances'Range loop
         if Allowances (I).Used then
            if Allowances (I).Owner_Addr = Owner_A
               and then Allowances (I).Spender = Spender_A
            then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Allow;

   --  Initialize
   procedure Initialize (
      Name           : in String;
      Symbol         : in String;
      Decimals       : in Natural;
      Initial_Supply : in Token_Amount;
      Owner          : in Address;
      Success        : out Boolean
   ) is
      Slot : Natural;
   begin
      if Storage.Initialized then
         Success := False;
         return;
      end if;

      --  Set name
      Storage.Name_Len := Name'Length;
      for I in 1 .. Name'Length loop
         Storage.Name (I) := Name (Name'First + I - 1);
      end loop;

      --  Set symbol
      Storage.Symbol_Len := Symbol'Length;
      for I in 1 .. Symbol'Length loop
         Storage.Symbol (I) := Symbol (Symbol'First + I - 1);
      end loop;

      Storage.Decimals := Decimals;
      Storage.Total := Initial_Supply;
      Storage.Owner_Addr := Owner;
      Storage.Paused := False;
      Storage.Initialized := True;

      --  Assign initial supply to owner
      Slot := Find_Or_Create_Balance (Owner);
      if Slot < Max_Accounts then
         Balances (Slot) := (Account => Owner, Amount => Initial_Supply, Used => True);
      end if;

      Success := True;
   end Initialize;

   --  View functions
   function Total_Supply return Token_Amount is
   begin
      return Storage.Total;
   end Total_Supply;

   function Balance_Of (Account : Address) return Token_Amount is
      Slot : constant Natural := Find_Balance (Account);
   begin
      if Slot < Max_Accounts then
         return Balances (Slot).Amount;
      else
         return U256_Zero;
      end if;
   end Balance_Of;

   function Allowance (Owner_Addr, Spender : Address) return Token_Amount is
      Slot : constant Natural := Find_Allow (Owner_Addr, Spender);
   begin
      if Slot < Max_Allowances then
         return Allowances (Slot).Amount;
      else
         return U256_Zero;
      end if;
   end Allowance;

   function Owner return Address is
   begin
      return Storage.Owner_Addr;
   end Owner;

   function Is_Paused return Boolean is
   begin
      return Storage.Paused;
   end Is_Paused;

   function Get_Decimals return Natural is
   begin
      return Storage.Decimals;
   end Get_Decimals;

   --  Transfer
   procedure Transfer (
      Caller    : in     Address;
      Recipient : in     Address;
      Amount    : in     Token_Amount;
      Success   : out    Boolean;
      Error     : out    Token_Error
   ) is
      Sender_Bal : Token_Amount;
      Recv_Bal   : Token_Amount;
      New_Recv   : U256;
      Overflow   : Boolean;
      Sender_Slot : Natural;
      Recv_Slot   : Natural;
   begin
      if Storage.Paused then
         Success := False;
         Error := Error_Paused;
         return;
      end if;

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

      --  Update balances
      Sender_Slot := Find_Or_Create_Balance (Caller);
      Recv_Slot := Find_Or_Create_Balance (Recipient);

      if Sender_Slot < Max_Accounts then
         Balances (Sender_Slot).Amount := Sub_Mod (Sender_Bal, Amount);
      end if;

      if Recv_Slot < Max_Accounts then
         Balances (Recv_Slot) := (Account => Recipient, Amount => New_Recv, Used => True);
      end if;

      Success := True;
      Error := Error_None;
   end Transfer;

   --  Approve
   procedure Approve (
      Caller  : in     Address;
      Spender : in     Address;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Token_Error
   ) is
      Slot : constant Natural := Find_Or_Create_Allow (Caller, Spender);
   begin
      if Slot < Max_Allowances then
         Allowances (Slot) := (
            Owner_Addr => Caller,
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

   --  Transfer_From
   procedure Transfer_From (
      Caller    : in     Address;
      From_Addr : in     Address;
      To_Addr   : in     Address;
      Amount    : in     Token_Amount;
      Success   : out    Boolean;
      Error     : out    Token_Error
   ) is
      From_Bal   : Token_Amount;
      To_Bal     : Token_Amount;
      Allow      : Token_Amount;
      New_To     : U256;
      Overflow   : Boolean;
      From_Slot  : Natural;
      To_Slot    : Natural;
      Allow_Slot : Natural;
   begin
      if Storage.Paused then
         Success := False;
         Error := Error_Paused;
         return;
      end if;

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

      --  Update state
      From_Slot := Find_Or_Create_Balance (From_Addr);
      To_Slot := Find_Or_Create_Balance (To_Addr);
      Allow_Slot := Find_Allow (From_Addr, Caller);

      if From_Slot < Max_Accounts then
         Balances (From_Slot).Amount := Sub_Mod (From_Bal, Amount);
      end if;

      if To_Slot < Max_Accounts then
         Balances (To_Slot) := (Account => To_Addr, Amount => New_To, Used => True);
      end if;

      if Allow_Slot < Max_Allowances then
         Allowances (Allow_Slot).Amount := Sub_Mod (Allow, Amount);
      end if;

      Success := True;
      Error := Error_None;
   end Transfer_From;

   --  Mint
   procedure Mint (
      Caller  : in     Address;
      To      : in     Address;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Token_Error
   ) is
      New_Total : U256;
      To_Bal    : Token_Amount;
      New_Bal   : U256;
      Overflow  : Boolean;
      Slot      : Natural;
   begin
      if Caller /= Storage.Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Add (Storage.Total, Amount, New_Total, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      To_Bal := Balance_Of (To);
      Add (To_Bal, Amount, New_Bal, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Storage.Total := New_Total;

      Slot := Find_Or_Create_Balance (To);
      if Slot < Max_Accounts then
         Balances (Slot) := (Account => To, Amount => New_Bal, Used => True);
      end if;

      Success := True;
      Error := Error_None;
   end Mint;

   --  Burn
   procedure Burn (
      Caller  : in     Address;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Token_Error
   ) is
      Caller_Bal : Token_Amount;
      Slot       : Natural;
   begin
      Caller_Bal := Balance_Of (Caller);

      if Less_Than (Caller_Bal, Amount) then
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      Storage.Total := Sub_Mod (Storage.Total, Amount);

      Slot := Find_Balance (Caller);
      if Slot < Max_Accounts then
         Balances (Slot).Amount := Sub_Mod (Caller_Bal, Amount);
      end if;

      Success := True;
      Error := Error_None;
   end Burn;

   --  Transfer_Ownership
   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Token_Error
   ) is
   begin
      if Caller /= Storage.Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Storage.Owner_Addr := New_Owner;
      Success := True;
      Error := Error_None;
   end Transfer_Ownership;

   --  Is_Owner
   function Is_Owner (Account : Address) return Boolean is
   begin
      return Account = Storage.Owner_Addr;
   end Is_Owner;

end ATS20_Token;
