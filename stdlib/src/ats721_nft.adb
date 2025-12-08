--  ATS-721: KHEPRI Standard NFT Implementation
pragma SPARK_Mode (On);

package body ATS721_NFT with
   SPARK_Mode => On,
   Refined_State => (NFT_State => (Storage, Tokens, Approvals, Operator_Approvals))
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_Tokens : constant := 10000;
   Max_Operators : constant := 1024;

   ---------------------------------------------------------------------------
   --  Internal Storage Types
   ---------------------------------------------------------------------------

   type NFT_Storage is record
      Config         : NFT_Config;
      Owner_Addr     : Address;
      Total_Minted   : Natural;
      Initialized    : Boolean;
   end record;

   type Token_Entry is record
      Token_Id   : NFT_Token_ID;
      Owner      : Address;
      Approved   : Address;
      URI        : Token_URI;
      Used       : Boolean;
   end record;

   type Token_Array is array (0 .. Max_Tokens - 1) of Token_Entry;

   type Operator_Entry is record
      Owner    : Address;
      Operator : Address;
      Approved : Boolean;
      Used     : Boolean;
   end record;

   type Operator_Array is array (0 .. Max_Operators - 1) of Operator_Entry;

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Storage : NFT_Storage := (
      Config       => Default_NFT_Config,
      Owner_Addr   => Zero_Address,
      Total_Minted => 0,
      Initialized  => False
   );

   Tokens : Token_Array := (others => (
      Token_Id  => U256_Zero,
      Owner     => Zero_Address,
      Approved  => Zero_Address,
      URI       => (Data => (others => ' '), Length => 0),
      Used      => False
   ));

   Approvals : Token_Array := (others => (
      Token_Id  => U256_Zero,
      Owner     => Zero_Address,
      Approved  => Zero_Address,
      URI       => (Data => (others => ' '), Length => 0),
      Used      => False
   ));

   Operator_Approvals : Operator_Array := (others => (
      Owner    => Zero_Address,
      Operator => Zero_Address,
      Approved => False,
      Used     => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Token_Slot (Token_Id : NFT_Token_ID) return Natural is
   begin
      for I in Tokens'Range loop
         if Tokens (I).Used and then Equal (Tokens (I).Token_Id, Token_Id) then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Token_Slot;

   function Find_Empty_Token_Slot return Natural is
   begin
      for I in Tokens'Range loop
         if not Tokens (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Token_Slot;

   function Find_Operator_Slot (Owner, Operator : Address) return Natural is
   begin
      for I in Operator_Approvals'Range loop
         if Operator_Approvals (I).Used
            and then Operator_Approvals (I).Owner = Owner
            and then Operator_Approvals (I).Operator = Operator
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Operator_Slot;

   function Find_Or_Create_Operator_Slot (Owner, Operator : Address) return Natural is
      Empty_Slot : Natural := Natural'Last;
   begin
      for I in Operator_Approvals'Range loop
         if Operator_Approvals (I).Used then
            if Operator_Approvals (I).Owner = Owner
               and then Operator_Approvals (I).Operator = Operator
            then
               return I;
            end if;
         elsif Empty_Slot = Natural'Last then
            Empty_Slot := I;
         end if;
      end loop;
      return Empty_Slot;
   end Find_Or_Create_Operator_Slot;

   function Count_Tokens_For_Owner (Owner : Address) return Natural is
      Count : Natural := 0;
   begin
      for I in Tokens'Range loop
         if Tokens (I).Used and then Tokens (I).Owner = Owner then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Tokens_For_Owner;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in NFT_Config;
      Owner   : in Address;
      Success : out Boolean
   ) is
   begin
      if Storage.Initialized then
         Success := False;
         return;
      end if;

      Storage := (
         Config       => Config,
         Owner_Addr   => Owner,
         Total_Minted => 0,
         Initialized  => True
      );

      Success := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Get_Name return Collection_Name is
   begin
      return Storage.Config.Name;
   end Get_Name;

   function Get_Symbol return Collection_Symbol is
   begin
      return Storage.Config.Symbol;
   end Get_Symbol;

   function Get_Base_URI return Base_URI is
   begin
      return Storage.Config.URI_Base;
   end Get_Base_URI;

   function Total_Supply return Natural is
   begin
      return Storage.Total_Minted;
   end Total_Supply;

   function Max_Supply return Natural is
   begin
      return Storage.Config.Max_Supply;
   end Max_Supply;

   function Balance_Of (Owner : Address) return Natural is
   begin
      return Count_Tokens_For_Owner (Owner);
   end Balance_Of;

   function Owner_Of (Token_Id : NFT_Token_ID) return Address is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
   begin
      if Slot /= Natural'Last and Slot < Max_Tokens then
         return Tokens (Slot).Owner;
      else
         return Zero_Address;
      end if;
   end Owner_Of;

   function Token_Exists (Token_Id : NFT_Token_ID) return Boolean is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
   begin
      return Slot /= Natural'Last and Slot < Max_Tokens;
   end Token_Exists;

   function Get_Token_URI (Token_Id : NFT_Token_ID) return Token_URI is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
   begin
      if Slot /= Natural'Last and Slot < Max_Tokens then
         return Tokens (Slot).URI;
      else
         return (Data => (others => ' '), Length => 0);
      end if;
   end Get_Token_URI;

   function Get_Approved (Token_Id : NFT_Token_ID) return Address is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
   begin
      if Slot /= Natural'Last and Slot < Max_Tokens then
         return Tokens (Slot).Approved;
      else
         return Zero_Address;
      end if;
   end Get_Approved;

   function Is_Approved_For_All (Owner, Operator : Address) return Boolean is
      Slot : constant Natural := Find_Operator_Slot (Owner, Operator);
   begin
      if Slot /= Natural'Last and Slot < Max_Operators then
         return Operator_Approvals (Slot).Approved;
      else
         return False;
      end if;
   end Is_Approved_For_All;

   function Contract_Owner return Address is
   begin
      return Storage.Owner_Addr;
   end Contract_Owner;

   ---------------------------------------------------------------------------
   --  Enumerable Extension
   ---------------------------------------------------------------------------

   function Token_Of_Owner_By_Index (
      Owner : Address;
      Index : Natural
   ) return NFT_Token_ID is
      Count : Natural := 0;
   begin
      for I in Tokens'Range loop
         if Tokens (I).Used and then Tokens (I).Owner = Owner then
            if Count = Index then
               return Tokens (I).Token_Id;
            end if;
            Count := Count + 1;
         end if;
      end loop;
      return Invalid_Token_ID;
   end Token_Of_Owner_By_Index;

   function Token_By_Index (Index : Natural) return NFT_Token_ID is
      Count : Natural := 0;
   begin
      for I in Tokens'Range loop
         if Tokens (I).Used then
            if Count = Index then
               return Tokens (I).Token_Id;
            end if;
            Count := Count + 1;
         end if;
      end loop;
      return Invalid_Token_ID;
   end Token_By_Index;

   ---------------------------------------------------------------------------
   --  Transfer Functions
   ---------------------------------------------------------------------------

   procedure Transfer_From (
      Caller   : in     Address;
      From     : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
   begin
      --  Check token exists
      if Slot = Natural'Last or Slot >= Max_Tokens then
         Success := False;
         Error := Error_Token_Not_Exists;
         return;
      end if;

      --  Check ownership
      if Tokens (Slot).Owner /= From then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      --  Check authorization
      if not Is_Approved_Or_Owner (Caller, Token_Id) then
         Success := False;
         Error := Error_Not_Approved;
         return;
      end if;

      --  Check destination
      if To = Zero_Address then
         Success := False;
         Error := Error_Zero_Address;
         return;
      end if;

      --  Perform transfer
      Tokens (Slot).Owner := To;
      Tokens (Slot).Approved := Zero_Address;  --  Clear approval

      Success := True;
      Error := Error_None;
   end Transfer_From;

   procedure Safe_Transfer_From (
      Caller   : in     Address;
      From     : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Data     : in     Byte_Array;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      pragma Unreferenced (Data);
   begin
      --  For now, same as Transfer_From
      --  In full implementation, would check if To is a contract
      --  and call onERC721Received
      Transfer_From (Caller, From, To, Token_Id, Success, Error);
   end Safe_Transfer_From;

   ---------------------------------------------------------------------------
   --  Approval Functions
   ---------------------------------------------------------------------------

   procedure Approve (
      Caller   : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
      Token_Owner : Address;
   begin
      --  Check token exists
      if Slot = Natural'Last or Slot >= Max_Tokens then
         Success := False;
         Error := Error_Token_Not_Exists;
         return;
      end if;

      Token_Owner := Tokens (Slot).Owner;

      --  Check caller is owner or approved for all
      if Caller /= Token_Owner
         and then not Is_Approved_For_All (Token_Owner, Caller)
      then
         Success := False;
         Error := Error_Not_Approved;
         return;
      end if;

      --  Check not self approval
      if To = Token_Owner then
         Success := False;
         Error := Error_Self_Approval;
         return;
      end if;

      --  Set approval
      Tokens (Slot).Approved := To;

      Success := True;
      Error := Error_None;
   end Approve;

   procedure Set_Approval_For_All (
      Caller   : in     Address;
      Operator : in     Address;
      Approved : in     Boolean;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      Slot : constant Natural := Find_Or_Create_Operator_Slot (Caller, Operator);
   begin
      if Operator = Zero_Address then
         Success := False;
         Error := Error_Zero_Address;
         return;
      end if;

      if Operator = Caller then
         Success := False;
         Error := Error_Self_Approval;
         return;
      end if;

      if Slot = Natural'Last or Slot >= Max_Operators then
         Success := False;
         Error := Error_Max_Supply_Reached;
         return;
      end if;

      Operator_Approvals (Slot) := (
         Owner    => Caller,
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
      Caller   : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      Slot : Natural;
   begin
      --  Check caller is owner
      if Caller /= Storage.Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      --  Check max supply
      if Storage.Total_Minted >= Storage.Config.Max_Supply then
         Success := False;
         Error := Error_Max_Supply_Reached;
         return;
      end if;

      --  Check token doesn't exist
      if Token_Exists (Token_Id) then
         Success := False;
         Error := Error_Already_Minted;
         return;
      end if;

      --  Check destination
      if To = Zero_Address then
         Success := False;
         Error := Error_Zero_Address;
         return;
      end if;

      --  Find empty slot
      Slot := Find_Empty_Token_Slot;
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Max_Supply_Reached;
         return;
      end if;

      --  Mint token
      Tokens (Slot) := (
         Token_Id  => Token_Id,
         Owner     => To,
         Approved  => Zero_Address,
         URI       => (Data => (others => ' '), Length => 0),
         Used      => True
      );

      Storage.Total_Minted := Storage.Total_Minted + 1;

      Success := True;
      Error := Error_None;
   end Mint;

   procedure Safe_Mint (
      Caller   : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Data     : in     Byte_Array;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      pragma Unreferenced (Data);
   begin
      --  For now, same as Mint
      Mint (Caller, To, Token_Id, Success, Error);
   end Safe_Mint;

   procedure Burn (
      Caller   : in     Address;
      Token_Id : in     NFT_Token_ID;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
   begin
      --  Check token exists
      if Slot = Natural'Last or Slot >= Max_Tokens then
         Success := False;
         Error := Error_Token_Not_Exists;
         return;
      end if;

      --  Check authorization
      if not Is_Approved_Or_Owner (Caller, Token_Id) then
         Success := False;
         Error := Error_Not_Approved;
         return;
      end if;

      --  Burn token
      Tokens (Slot).Used := False;
      Tokens (Slot).Owner := Zero_Address;
      Tokens (Slot).Approved := Zero_Address;

      if Storage.Total_Minted > 0 then
         Storage.Total_Minted := Storage.Total_Minted - 1;
      end if;

      Success := True;
      Error := Error_None;
   end Burn;

   ---------------------------------------------------------------------------
   --  Metadata Functions
   ---------------------------------------------------------------------------

   procedure Set_Token_URI (
      Caller   : in     Address;
      Token_Id : in     NFT_Token_ID;
      URI      : in     Token_URI;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) is
      Slot : constant Natural := Find_Token_Slot (Token_Id);
   begin
      --  Check caller is owner
      if Caller /= Storage.Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      --  Check token exists
      if Slot = Natural'Last or Slot >= Max_Tokens then
         Success := False;
         Error := Error_Token_Not_Exists;
         return;
      end if;

      Tokens (Slot).URI := URI;

      Success := True;
      Error := Error_None;
   end Set_Token_URI;

   procedure Set_Base_URI (
      Caller  : in     Address;
      URI     : in     Base_URI;
      Success : out    Boolean;
      Error   : out    NFT_Error
   ) is
   begin
      if Caller /= Storage.Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Storage.Config.URI_Base := URI;

      Success := True;
      Error := Error_None;
   end Set_Base_URI;

   ---------------------------------------------------------------------------
   --  Royalty Functions
   ---------------------------------------------------------------------------

   procedure Royalty_Info (
      Token_Id     : in     NFT_Token_ID;
      Sale_Price   : in     U256;
      Receiver     : out    Address;
      Royalty_Amt  : out    U256
   ) is
      pragma Unreferenced (Token_Id);
      BPS : constant Natural := Storage.Config.Royalty_BPS;
   begin
      Receiver := Storage.Config.Royalty_Recv;

      if BPS = 0 or else Receiver = Zero_Address then
         Royalty_Amt := U256_Zero;
      else
         --  Royalty = Sale_Price * BPS / 10000
         declare
            Multiplied : U256;
            Remainder  : U256;
            BPS_U256   : constant U256 := From_Word64 (Word64 (BPS));
            Divisor    : constant U256 := From_Word64 (10000);
         begin
            --  Use modular multiplication (truncates to 256 bits)
            Multiplied := Mul_Mod (Sale_Price, BPS_U256);
            --  Divide by 10000 to get royalty amount
            Div_Mod (Multiplied, Divisor, Royalty_Amt, Remainder);
         end;
      end if;
   end Royalty_Info;

   procedure Set_Default_Royalty (
      Caller       : in     Address;
      Receiver     : in     Address;
      Fee_Numerator: in     Natural;
      Success      : out    Boolean;
      Error        : out    NFT_Error
   ) is
   begin
      if Caller /= Storage.Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Storage.Config.Royalty_BPS := Fee_Numerator;
      Storage.Config.Royalty_Recv := Receiver;

      Success := True;
      Error := Error_None;
   end Set_Default_Royalty;

   ---------------------------------------------------------------------------
   --  Ownership Functions
   ---------------------------------------------------------------------------

   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    NFT_Error
   ) is
   begin
      if Caller /= Storage.Owner_Addr then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if New_Owner = Zero_Address then
         Success := False;
         Error := Error_Zero_Address;
         return;
      end if;

      Storage.Owner_Addr := New_Owner;

      Success := True;
      Error := Error_None;
   end Transfer_Ownership;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Is_Approved_Or_Owner (
      Spender  : Address;
      Token_Id : NFT_Token_ID
   ) return Boolean is
      Token_Owner : constant Address := Owner_Of (Token_Id);
   begin
      return Spender = Token_Owner
         or else Get_Approved (Token_Id) = Spender
         or else Is_Approved_For_All (Token_Owner, Spender);
   end Is_Approved_Or_Owner;

   function Supports_Interface (Interface_ID : Bytes4) return Boolean is
   begin
      return Interface_ID = ERC721_Interface_ID
         or else Interface_ID = ERC721_Metadata_ID
         or else Interface_ID = ERC721_Enumerable_ID
         or else Interface_ID = ERC2981_Interface_ID;
   end Supports_Interface;

end ATS721_NFT;
