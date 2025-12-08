--  ATS-721: KHEPRI Standard Non-Fungible Token Interface
--
--  This package defines the standard NFT interface for KHEPRI contracts,
--  equivalent to ERC-721 on Ethereum. Each token has a unique ID and
--  can have associated metadata.
--
--  Certification Target: PLATINUM
--
--  Features:
--    - Unique token IDs (256-bit)
--    - Ownership tracking
--    - Transfer and approval mechanisms
--    - Metadata URI support
--    - Enumerable extension
--    - Safe transfer with receiver checks

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Khepri_Types;    use Khepri_Types;

package ATS721_NFT with
   SPARK_Mode => On,
   Abstract_State => (NFT_State with External => Async_Writers),
   Initializes => NFT_State
is

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Zero address constant (alias)
   Zero_Address : constant Address := Null_Address;

   --  Token ID (256-bit unique identifier)
   subtype NFT_Token_ID is U256;

   --  Invalid token ID constant
   Invalid_Token_ID : constant NFT_Token_ID := U256_Max;

   ---------------------------------------------------------------------------
   --  Collection Metadata
   ---------------------------------------------------------------------------

   --  Collection name (max 64 characters)
   subtype Collection_Name_Length is Natural range 0 .. 64;
   type Collection_Name is record
      Data   : String (1 .. 64);
      Length : Collection_Name_Length;
   end record;

   --  Collection symbol (max 16 characters)
   subtype Collection_Symbol_Length is Natural range 0 .. 16;
   type Collection_Symbol is record
      Data   : String (1 .. 16);
      Length : Collection_Symbol_Length;
   end record;

   --  Base URI for metadata (max 256 characters)
   subtype URI_Length is Natural range 0 .. 256;
   type Base_URI is record
      Data   : String (1 .. 256);
      Length : URI_Length;
   end record;

   --  Token URI (max 512 characters)
   subtype Token_URI_Length is Natural range 0 .. 512;
   type Token_URI is record
      Data   : String (1 .. 512);
      Length : Token_URI_Length;
   end record;

   ---------------------------------------------------------------------------
   --  Collection Configuration
   ---------------------------------------------------------------------------

   type NFT_Config is record
      Name          : Collection_Name;
      Symbol        : Collection_Symbol;
      URI_Base      : Base_URI;
      Max_Supply    : Natural;
      Royalty_BPS   : Natural;   --  Basis points (100 = 1%)
      Royalty_Recv  : Address;
   end record;

   Default_NFT_Config : constant NFT_Config := (
      Name         => (Data => (others => ' '), Length => 0),
      Symbol       => (Data => (others => ' '), Length => 0),
      URI_Base     => (Data => (others => ' '), Length => 0),
      Max_Supply   => Natural'Last,
      Royalty_BPS  => 0,
      Royalty_Recv => Zero_Address
   );

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------

   type Transfer_Event is record
      From     : Address;
      To       : Address;
      Token_Id : NFT_Token_ID;
   end record;

   type Approval_Event is record
      Owner    : Address;
      Approved : Address;
      Token_Id : NFT_Token_ID;
   end record;

   type Approval_For_All_Event is record
      Owner    : Address;
      Operator : Address;
      Approved : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type NFT_Error is (
      Error_None,
      Error_Token_Not_Exists,
      Error_Not_Owner,
      Error_Not_Approved,
      Error_Already_Minted,
      Error_Zero_Address,
      Error_Self_Approval,
      Error_Max_Supply_Reached,
      Error_Invalid_NFT_Token_ID
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in NFT_Config;
      Owner   : in Address;
      Success : out Boolean
   ) with
      Global => (In_Out => NFT_State),
      Pre    => Owner /= Zero_Address;

   ---------------------------------------------------------------------------
   --  View Functions (Read-Only)
   ---------------------------------------------------------------------------

   function Get_Name return Collection_Name with
      Global => NFT_State,
      Volatile_Function;

   function Get_Symbol return Collection_Symbol with
      Global => NFT_State,
      Volatile_Function;

   function Get_Base_URI return Base_URI with
      Global => NFT_State,
      Volatile_Function;

   function Total_Supply return Natural with
      Global => NFT_State,
      Volatile_Function;

   function Max_Supply return Natural with
      Global => NFT_State,
      Volatile_Function;

   function Balance_Of (Owner : Address) return Natural with
      Global => NFT_State,
      Volatile_Function,
      Pre    => Owner /= Zero_Address;

   function Owner_Of (Token_Id : NFT_Token_ID) return Address with
      Global => NFT_State,
      Volatile_Function;

   function Token_Exists (Token_Id : NFT_Token_ID) return Boolean with
      Global => NFT_State,
      Volatile_Function;

   function Get_Token_URI (Token_Id : NFT_Token_ID) return Token_URI with
      Global => NFT_State,
      Volatile_Function,
      Pre    => Token_Exists (Token_Id);

   function Get_Approved (Token_Id : NFT_Token_ID) return Address with
      Global => NFT_State,
      Volatile_Function,
      Pre    => Token_Exists (Token_Id);

   function Is_Approved_For_All (Owner, Operator : Address) return Boolean with
      Global => NFT_State,
      Volatile_Function;

   function Contract_Owner return Address with
      Global => NFT_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Enumerable Extension
   ---------------------------------------------------------------------------

   function Token_Of_Owner_By_Index (
      Owner : Address;
      Index : Natural
   ) return NFT_Token_ID with
      Global => NFT_State,
      Volatile_Function,
      Pre    => Owner /= Zero_Address and Index < Balance_Of (Owner);

   function Token_By_Index (Index : Natural) return NFT_Token_ID with
      Global => NFT_State,
      Volatile_Function,
      Pre    => Index < Total_Supply;

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
   ) with
      Global => (In_Out => NFT_State),
      Pre    => To /= Zero_Address,
      Post   => (if Success then Owner_Of (Token_Id) = To);

   procedure Safe_Transfer_From (
      Caller   : in     Address;
      From     : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Data     : in     Byte_Array;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State),
      Pre    => To /= Zero_Address;

   ---------------------------------------------------------------------------
   --  Approval Functions
   ---------------------------------------------------------------------------

   procedure Approve (
      Caller   : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State),
      Post   => (if Success then Get_Approved (Token_Id) = To);

   procedure Set_Approval_For_All (
      Caller   : in     Address;
      Operator : in     Address;
      Approved : in     Boolean;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State),
      Pre    => Operator /= Zero_Address and Operator /= Caller,
      Post   => (if Success then Is_Approved_For_All (Caller, Operator) = Approved);

   ---------------------------------------------------------------------------
   --  Minting and Burning
   ---------------------------------------------------------------------------

   procedure Mint (
      Caller   : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State),
      Pre    => To /= Zero_Address,
      Post   => (if Success then
                    Owner_Of (Token_Id) = To
                    and Token_Exists (Token_Id));

   procedure Safe_Mint (
      Caller   : in     Address;
      To       : in     Address;
      Token_Id : in     NFT_Token_ID;
      Data     : in     Byte_Array;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State),
      Pre    => To /= Zero_Address;

   procedure Burn (
      Caller   : in     Address;
      Token_Id : in     NFT_Token_ID;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State),
      Post   => (if Success then not Token_Exists (Token_Id));

   ---------------------------------------------------------------------------
   --  Metadata Functions
   ---------------------------------------------------------------------------

   procedure Set_Token_URI (
      Caller   : in     Address;
      Token_Id : in     NFT_Token_ID;
      URI      : in     Token_URI;
      Success  : out    Boolean;
      Error    : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State);

   procedure Set_Base_URI (
      Caller  : in     Address;
      URI     : in     Base_URI;
      Success : out    Boolean;
      Error   : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State);

   ---------------------------------------------------------------------------
   --  Royalty Functions (EIP-2981 compatible)
   ---------------------------------------------------------------------------

   procedure Royalty_Info (
      Token_Id     : in     NFT_Token_ID;
      Sale_Price   : in     U256;
      Receiver     : out    Address;
      Royalty_Amt  : out    U256
   ) with
      Global => NFT_State;

   procedure Set_Default_Royalty (
      Caller       : in     Address;
      Receiver     : in     Address;
      Fee_Numerator: in     Natural;
      Success      : out    Boolean;
      Error        : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State);

   ---------------------------------------------------------------------------
   --  Ownership Functions
   ---------------------------------------------------------------------------

   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    NFT_Error
   ) with
      Global => (In_Out => NFT_State),
      Pre    => New_Owner /= Zero_Address,
      Post   => (if Success then Contract_Owner = New_Owner);

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Is_Approved_Or_Owner (
      Spender  : Address;
      Token_Id : NFT_Token_ID
   ) return Boolean with
      Global => NFT_State,
      Volatile_Function;

   function Supports_Interface (Interface_ID : Bytes4) return Boolean with
      Global => null;

private

   --  Interface IDs
   ERC721_Interface_ID     : constant Bytes4 := (16#80#, 16#ac#, 16#58#, 16#cd#);
   ERC721_Metadata_ID      : constant Bytes4 := (16#5b#, 16#5e#, 16#13#, 16#9f#);
   ERC721_Enumerable_ID    : constant Bytes4 := (16#78#, 16#0e#, 16#9d#, 16#63#);
   ERC2981_Interface_ID    : constant Bytes4 := (16#2a#, 16#55#, 16#20#, 16#5a#);

end ATS721_NFT;
