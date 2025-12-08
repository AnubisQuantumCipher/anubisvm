--  ATS-1155: KHEPRI Multi-Token Implementation
pragma SPARK_Mode (On);

package body ATS1155_Multi with
   SPARK_Mode => On,
   Refined_State => (Multi_State => (Storage, Balances, Operators, Token_URIs, Supplies))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Config_Storage is record
      URI_Base     : Token_URI;
      Contract_URI : Token_URI;
      Owner        : Address;
      Initialized  : Boolean;
   end record;

   --  Balance entry: (account, token_id) -> amount
   type Balance_Entry is record
      Account : Address;
      ID      : Token_ID;
      Amount  : Token_Amount;
      Used    : Boolean;
   end record;

   type Balance_Array is array (0 .. Max_Accounts - 1) of Balance_Entry;

   --  Operator approval entry
   type Operator_Entry is record
      Account  : Address;
      Operator : Address;
      Approved : Boolean;
      Used     : Boolean;
   end record;

   type Operator_Array is array (0 .. Max_Operators - 1) of Operator_Entry;

   --  Token URI entry
   type URI_Entry is record
      ID   : Token_ID;
      URI  : Token_URI;
      Used : Boolean;
   end record;

   type URI_Array is array (0 .. Max_Token_Types - 1) of URI_Entry;

   --  Token supply tracking
   type Supply_Entry is record
      ID     : Token_ID;
      Supply : Token_Amount;
      Used   : Boolean;
   end record;

   type Supply_Array is array (0 .. Max_Token_Types - 1) of Supply_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Storage : Config_Storage := (
      URI_Base     => (Data => (others => ' '), Length => 0),
      Contract_URI => (Data => (others => ' '), Length => 0),
      Owner        => Zero_Address,
      Initialized  => False
   );

   Balances : Balance_Array := (others => (
      Account => Zero_Address,
      ID      => U256_Zero,
      Amount  => U256_Zero,
      Used    => False
   ));

   Operators : Operator_Array := (others => (
      Account  => Zero_Address,
      Operator => Zero_Address,
      Approved => False,
      Used     => False
   ));

   Token_URIs : URI_Array := (others => (
      ID   => U256_Zero,
      URI  => (Data => (others => ' '), Length => 0),
      Used => False
   ));

   Supplies : Supply_Array := (others => (
      ID     => U256_Zero,
      Supply => U256_Zero,
      Used   => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Balance (Account : Address; ID : Token_ID) return Natural is
   begin
      for I in Balances'Range loop
         if Balances (I).Used
            and then Balances (I).Account = Account
            and then Equal (Balances (I).ID, ID)
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Balance;

   function Find_Or_Create_Balance (Account : Address; ID : Token_ID) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Balances'Range loop
         if Balances (I).Used then
            if Balances (I).Account = Account
               and then Equal (Balances (I).ID, ID)
            then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Balance;

   function Find_Operator (Account, Operator : Address) return Natural is
   begin
      for I in Operators'Range loop
         if Operators (I).Used
            and then Operators (I).Account = Account
            and then Operators (I).Operator = Operator
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Operator;

   function Find_Or_Create_Operator (Account, Operator : Address) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Operators'Range loop
         if Operators (I).Used then
            if Operators (I).Account = Account
               and then Operators (I).Operator = Operator
            then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Operator;

   function Find_Supply (ID : Token_ID) return Natural is
   begin
      for I in Supplies'Range loop
         if Supplies (I).Used and then Equal (Supplies (I).ID, ID) then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Supply;

   function Find_Or_Create_Supply (ID : Token_ID) return Natural is
      Empty : Natural := Natural'Last;
   begin
      for I in Supplies'Range loop
         if Supplies (I).Used then
            if Equal (Supplies (I).ID, ID) then
               return I;
            end if;
         elsif Empty = Natural'Last then
            Empty := I;
         end if;
      end loop;
      return Empty;
   end Find_Or_Create_Supply;

   function Find_URI (ID : Token_ID) return Natural is
   begin
      for I in Token_URIs'Range loop
         if Token_URIs (I).Used and then Equal (Token_URIs (I).ID, ID) then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_URI;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Multi_Config;
      Success : out Boolean
   ) is
   begin
      if Storage.Initialized then
         Success := False;
         return;
      end if;

      Storage := (
         URI_Base     => Config.URI_Base,
         Contract_URI => Config.Contract_URI,
         Owner        => Config.Owner,
         Initialized  => True
      );

      Success := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Balance_Of (
      Account : Address;
      ID      : Token_ID
   ) return Token_Amount is
      Slot : constant Natural := Find_Balance (Account, ID);
   begin
      if Slot < Max_Accounts then
         return Balances (Slot).Amount;
      else
         return U256_Zero;
      end if;
   end Balance_Of;

   procedure Balance_Of_Batch (
      Accounts : in     Address_Array;
      IDs      : in     Token_ID_Array;
      Count    : in     Natural;
      Balances : out    Amount_Array;
      Success  : out    Boolean
   ) is
   begin
      Balances := (others => U256_Zero);

      for I in 0 .. Count - 1 loop
         if Accounts (I) = Zero_Address then
            Success := False;
            return;
         end if;
         Balances (I) := Balance_Of (Accounts (I), IDs (I));
      end loop;

      Success := True;
   end Balance_Of_Batch;

   function Is_Approved_For_All (
      Account  : Address;
      Operator : Address
   ) return Boolean is
      Slot : constant Natural := Find_Operator (Account, Operator);
   begin
      if Slot < Max_Operators then
         return Operators (Slot).Approved;
      else
         return False;
      end if;
   end Is_Approved_For_All;

   function URI (ID : Token_ID) return Token_URI is
      Slot : constant Natural := Find_URI (ID);
   begin
      if Slot < Max_Token_Types then
         return Token_URIs (Slot).URI;
      else
         --  Return base URI if no specific URI set
         return Storage.URI_Base;
      end if;
   end URI;

   function Contract_Owner return Address is
   begin
      return Storage.Owner;
   end Contract_Owner;

   function Total_Supply (ID : Token_ID) return Token_Amount is
      Slot : constant Natural := Find_Supply (ID);
   begin
      if Slot < Max_Token_Types then
         return Supplies (Slot).Supply;
      else
         return U256_Zero;
      end if;
   end Total_Supply;

   function Exists (ID : Token_ID) return Boolean is
      Slot : constant Natural := Find_Supply (ID);
   begin
      return Slot < Max_Token_Types and then not Equal (Supplies (Slot).Supply, U256_Zero);
   end Exists;

   ---------------------------------------------------------------------------
   --  Transfer Functions
   ---------------------------------------------------------------------------

   procedure Safe_Transfer_From (
      Caller  : in     Address;
      From    : in     Address;
      To      : in     Address;
      ID      : in     Token_ID;
      Amount  : in     Token_Amount;
      Data    : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
      pragma Unreferenced (Data);
      From_Bal   : Token_Amount;
      To_Bal     : Token_Amount;
      New_To_Bal : U256;
      Overflow   : Boolean;
      From_Slot  : Natural;
      To_Slot    : Natural;
   begin
      --  Check authorization
      if Caller /= From and then not Is_Approved_For_All (From, Caller) then
         Success := False;
         Error := Error_Not_Approved;
         return;
      end if;

      From_Bal := Balance_Of (From, ID);
      if Less_Than (From_Bal, Amount) then
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      To_Bal := Balance_Of (To, ID);
      Add (To_Bal, Amount, New_To_Bal, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      --  Update balances
      From_Slot := Find_Or_Create_Balance (From, ID);
      To_Slot := Find_Or_Create_Balance (To, ID);

      if From_Slot < Max_Accounts then
         Balances (From_Slot).Amount := Sub_Mod (From_Bal, Amount);
      end if;

      if To_Slot < Max_Accounts then
         Balances (To_Slot) := (
            Account => To,
            ID      => ID,
            Amount  => New_To_Bal,
            Used    => True
         );
      end if;

      Success := True;
      Error := Error_None;
   end Safe_Transfer_From;

   procedure Safe_Batch_Transfer_From (
      Caller  : in     Address;
      From    : in     Address;
      To      : in     Address;
      IDs     : in     Token_ID_Array;
      Amounts : in     Amount_Array;
      Count   : in     Natural;
      Data    : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
      Single_Success : Boolean;
      Single_Error   : Multi_Error;
   begin
      --  Check authorization once
      if Caller /= From and then not Is_Approved_For_All (From, Caller) then
         Success := False;
         Error := Error_Not_Approved;
         return;
      end if;

      --  Process each transfer
      for I in 0 .. Count - 1 loop
         Safe_Transfer_From (Caller, From, To, IDs (I), Amounts (I), Data,
                            Single_Success, Single_Error);
         if not Single_Success then
            Success := False;
            Error := Single_Error;
            return;
         end if;
      end loop;

      Success := True;
      Error := Error_None;
   end Safe_Batch_Transfer_From;

   ---------------------------------------------------------------------------
   --  Approval Functions
   ---------------------------------------------------------------------------

   procedure Set_Approval_For_All (
      Caller   : in     Address;
      Operator : in     Address;
      Approved : in     Boolean;
      Success  : out    Boolean;
      Error    : out    Multi_Error
   ) is
      Slot : constant Natural := Find_Or_Create_Operator (Caller, Operator);
   begin
      if Slot = Natural'Last or Slot >= Max_Operators then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Operators (Slot) := (
         Account  => Caller,
         Operator => Operator,
         Approved => Approved,
         Used     => True
      );

      Success := True;
      Error := Error_None;
   end Set_Approval_For_All;

   ---------------------------------------------------------------------------
   --  Minting and Burning
   ---------------------------------------------------------------------------

   procedure Mint (
      Caller  : in     Address;
      To      : in     Address;
      ID      : in     Token_ID;
      Amount  : in     Token_Amount;
      Data    : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
      pragma Unreferenced (Data);
      To_Bal     : Token_Amount;
      New_Bal    : U256;
      Supply     : Token_Amount;
      New_Supply : U256;
      Overflow   : Boolean;
      Bal_Slot   : Natural;
      Sup_Slot   : Natural;
   begin
      --  Check caller is owner
      if Caller /= Storage.Owner then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      --  Update balance
      To_Bal := Balance_Of (To, ID);
      Add (To_Bal, Amount, New_Bal, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      --  Update supply
      Supply := Total_Supply (ID);
      Add (Supply, Amount, New_Supply, Overflow);
      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Bal_Slot := Find_Or_Create_Balance (To, ID);
      Sup_Slot := Find_Or_Create_Supply (ID);

      if Bal_Slot < Max_Accounts then
         Balances (Bal_Slot) := (
            Account => To,
            ID      => ID,
            Amount  => New_Bal,
            Used    => True
         );
      end if;

      if Sup_Slot < Max_Token_Types then
         Supplies (Sup_Slot) := (
            ID     => ID,
            Supply => New_Supply,
            Used   => True
         );
      end if;

      Success := True;
      Error := Error_None;
   end Mint;

   procedure Mint_Batch (
      Caller  : in     Address;
      To      : in     Address;
      IDs     : in     Token_ID_Array;
      Amounts : in     Amount_Array;
      Count   : in     Natural;
      Data    : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
      Single_Success : Boolean;
      Single_Error   : Multi_Error;
   begin
      --  Check caller is owner once
      if Caller /= Storage.Owner then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      for I in 0 .. Count - 1 loop
         Mint (Caller, To, IDs (I), Amounts (I), Data, Single_Success, Single_Error);
         if not Single_Success then
            Success := False;
            Error := Single_Error;
            return;
         end if;
      end loop;

      Success := True;
      Error := Error_None;
   end Mint_Batch;

   procedure Burn (
      Caller  : in     Address;
      From    : in     Address;
      ID      : in     Token_ID;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
      From_Bal : Token_Amount;
      Supply   : Token_Amount;
      Bal_Slot : Natural;
      Sup_Slot : Natural;
   begin
      --  Check authorization
      if Caller /= From and then not Is_Approved_For_All (From, Caller) then
         Success := False;
         Error := Error_Not_Approved;
         return;
      end if;

      From_Bal := Balance_Of (From, ID);
      if Less_Than (From_Bal, Amount) then
         Success := False;
         Error := Error_Insufficient_Balance;
         return;
      end if;

      Supply := Total_Supply (ID);

      Bal_Slot := Find_Balance (From, ID);
      Sup_Slot := Find_Supply (ID);

      if Bal_Slot < Max_Accounts then
         Balances (Bal_Slot).Amount := Sub_Mod (From_Bal, Amount);
      end if;

      if Sup_Slot < Max_Token_Types then
         Supplies (Sup_Slot).Supply := Sub_Mod (Supply, Amount);
      end if;

      Success := True;
      Error := Error_None;
   end Burn;

   procedure Burn_Batch (
      Caller  : in     Address;
      From    : in     Address;
      IDs     : in     Token_ID_Array;
      Amounts : in     Amount_Array;
      Count   : in     Natural;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
      Single_Success : Boolean;
      Single_Error   : Multi_Error;
   begin
      for I in 0 .. Count - 1 loop
         Burn (Caller, From, IDs (I), Amounts (I), Single_Success, Single_Error);
         if not Single_Success then
            Success := False;
            Error := Single_Error;
            return;
         end if;
      end loop;

      Success := True;
      Error := Error_None;
   end Burn_Batch;

   ---------------------------------------------------------------------------
   --  Metadata Functions
   ---------------------------------------------------------------------------

   procedure Set_URI (
      Caller  : in     Address;
      ID      : in     Token_ID;
      New_URI : in     Token_URI;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
      Slot : Natural;
   begin
      if Caller /= Storage.Owner then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      --  Find or create URI slot
      Slot := Find_URI (ID);
      if Slot = Natural'Last then
         --  Find empty slot
         for I in Token_URIs'Range loop
            if not Token_URIs (I).Used then
               Slot := I;
               exit;
            end if;
         end loop;
      end if;

      if Slot < Max_Token_Types then
         Token_URIs (Slot) := (
            ID   => ID,
            URI  => New_URI,
            Used => True
         );
         Success := True;
         Error := Error_None;
      else
         Success := False;
         Error := Error_Overflow;
      end if;
   end Set_URI;

   procedure Set_Base_URI (
      Caller  : in     Address;
      New_URI : in     Token_URI;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) is
   begin
      if Caller /= Storage.Owner then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Storage.URI_Base := New_URI;
      Success := True;
      Error := Error_None;
   end Set_Base_URI;

   ---------------------------------------------------------------------------
   --  Ownership Functions
   ---------------------------------------------------------------------------

   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Multi_Error
   ) is
   begin
      if Caller /= Storage.Owner then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Storage.Owner := New_Owner;
      Success := True;
      Error := Error_None;
   end Transfer_Ownership;

   ---------------------------------------------------------------------------
   --  Interface Support
   ---------------------------------------------------------------------------

   function Supports_Interface (Interface_ID : Bytes4) return Boolean is
   begin
      return Interface_ID = ERC1155_Interface_ID
         or else Interface_ID = ERC1155_Metadata_ID
         or else Interface_ID = ERC1155_TokenReceiver_ID;
   end Supports_Interface;

end ATS1155_Multi;
