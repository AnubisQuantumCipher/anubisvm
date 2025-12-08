--  ATS-1155: KHEPRI Multi-Token Standard Interface
--
--  This package defines the multi-token interface for KHEPRI contracts,
--  equivalent to ERC-1155 on Ethereum. Supports both fungible and
--  non-fungible tokens in a single contract.
--
--  Certification Target: PLATINUM

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Khepri_Types;    use Khepri_Types;

package ATS1155_Multi with
   SPARK_Mode => On,
   Abstract_State => (Multi_State with External => Async_Writers),
   Initializes => Multi_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Zero_Address : constant Address := Null_Address;

   Max_Token_Types  : constant := 10000;
   Max_Accounts     : constant := 10000;
   Max_Operators    : constant := 1024;
   Max_Batch_Size   : constant := 100;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Token ID (256-bit)
   subtype Token_ID is U256;

   --  Token amount (256-bit)
   subtype Token_Amount is U256;

   --  URI for metadata (max 512 characters)
   subtype URI_Length is Natural range 0 .. 512;
   type Token_URI is record
      Data   : String (1 .. 512);
      Length : URI_Length;
   end record;

   ---------------------------------------------------------------------------
   --  Batch Operation Types
   ---------------------------------------------------------------------------

   subtype Batch_Index is Natural range 0 .. Max_Batch_Size - 1;

   type Token_ID_Array is array (Batch_Index) of Token_ID;
   type Amount_Array is array (Batch_Index) of Token_Amount;
   type Address_Array is array (Batch_Index) of Address;

   type Batch_Transfer is record
      IDs     : Token_ID_Array;
      Amounts : Amount_Array;
      Count   : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Multi_Error is (
      Error_None,
      Error_Insufficient_Balance,
      Error_Not_Approved,
      Error_Zero_Address,
      Error_Self_Approval,
      Error_Length_Mismatch,
      Error_Overflow,
      Error_Not_Owner,
      Error_Token_Not_Exists,
      Error_Batch_Too_Large
   );

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------

   type Transfer_Single_Event is record
      Operator : Address;
      From     : Address;
      To       : Address;
      ID       : Token_ID;
      Value    : Token_Amount;
   end record;

   type Transfer_Batch_Event is record
      Operator : Address;
      From     : Address;
      To       : Address;
      IDs      : Token_ID_Array;
      Values   : Amount_Array;
      Count    : Natural;
   end record;

   type Approval_For_All_Event is record
      Account  : Address;
      Operator : Address;
      Approved : Boolean;
   end record;

   type URI_Event is record
      Value : Token_URI;
      ID    : Token_ID;
   end record;

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------

   type Multi_Config is record
      URI_Base     : Token_URI;
      Contract_URI : Token_URI;
      Owner        : Address;
   end record;

   Default_Config : constant Multi_Config := (
      URI_Base     => (Data => (others => ' '), Length => 0),
      Contract_URI => (Data => (others => ' '), Length => 0),
      Owner        => Zero_Address
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Multi_Config;
      Success : out Boolean
   ) with
      Global => (In_Out => Multi_State),
      Pre    => Config.Owner /= Zero_Address;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Balance_Of (
      Account : Address;
      ID      : Token_ID
   ) return Token_Amount with
      Global => Multi_State,
      Volatile_Function,
      Pre    => Account /= Zero_Address;

   procedure Balance_Of_Batch (
      Accounts : in     Address_Array;
      IDs      : in     Token_ID_Array;
      Count    : in     Natural;
      Balances : out    Amount_Array;
      Success  : out    Boolean
   ) with
      Global => Multi_State,
      Pre    => Count <= Max_Batch_Size;

   function Is_Approved_For_All (
      Account  : Address;
      Operator : Address
   ) return Boolean with
      Global => Multi_State,
      Volatile_Function;

   function URI (ID : Token_ID) return Token_URI with
      Global => Multi_State,
      Volatile_Function;

   function Contract_Owner return Address with
      Global => Multi_State,
      Volatile_Function;

   function Total_Supply (ID : Token_ID) return Token_Amount with
      Global => Multi_State,
      Volatile_Function;

   function Exists (ID : Token_ID) return Boolean with
      Global => Multi_State,
      Volatile_Function;

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
   ) with
      Global => (In_Out => Multi_State),
      Pre    => To /= Zero_Address;

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
   ) with
      Global => (In_Out => Multi_State),
      Pre    => To /= Zero_Address and Count <= Max_Batch_Size;

   ---------------------------------------------------------------------------
   --  Approval Functions
   ---------------------------------------------------------------------------

   procedure Set_Approval_For_All (
      Caller   : in     Address;
      Operator : in     Address;
      Approved : in     Boolean;
      Success  : out    Boolean;
      Error    : out    Multi_Error
   ) with
      Global => (In_Out => Multi_State),
      Pre    => Operator /= Zero_Address and Operator /= Caller;

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
   ) with
      Global => (In_Out => Multi_State),
      Pre    => To /= Zero_Address;

   procedure Mint_Batch (
      Caller  : in     Address;
      To      : in     Address;
      IDs     : in     Token_ID_Array;
      Amounts : in     Amount_Array;
      Count   : in     Natural;
      Data    : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) with
      Global => (In_Out => Multi_State),
      Pre    => To /= Zero_Address and Count <= Max_Batch_Size;

   procedure Burn (
      Caller  : in     Address;
      From    : in     Address;
      ID      : in     Token_ID;
      Amount  : in     Token_Amount;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) with
      Global => (In_Out => Multi_State);

   procedure Burn_Batch (
      Caller  : in     Address;
      From    : in     Address;
      IDs     : in     Token_ID_Array;
      Amounts : in     Amount_Array;
      Count   : in     Natural;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) with
      Global => (In_Out => Multi_State),
      Pre    => Count <= Max_Batch_Size;

   ---------------------------------------------------------------------------
   --  Metadata Functions
   ---------------------------------------------------------------------------

   procedure Set_URI (
      Caller  : in     Address;
      ID      : in     Token_ID;
      New_URI : in     Token_URI;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) with
      Global => (In_Out => Multi_State);

   procedure Set_Base_URI (
      Caller  : in     Address;
      New_URI : in     Token_URI;
      Success : out    Boolean;
      Error   : out    Multi_Error
   ) with
      Global => (In_Out => Multi_State);

   ---------------------------------------------------------------------------
   --  Ownership Functions
   ---------------------------------------------------------------------------

   procedure Transfer_Ownership (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Multi_Error
   ) with
      Global => (In_Out => Multi_State),
      Pre    => New_Owner /= Zero_Address;

   ---------------------------------------------------------------------------
   --  Interface Support
   ---------------------------------------------------------------------------

   function Supports_Interface (Interface_ID : Bytes4) return Boolean with
      Global => null;

private

   --  Interface IDs
   ERC1155_Interface_ID       : constant Bytes4 := (16#d9#, 16#b6#, 16#7a#, 16#26#);
   ERC1155_Metadata_ID        : constant Bytes4 := (16#0e#, 16#89#, 16#34#, 16#1c#);
   ERC1155_TokenReceiver_ID   : constant Bytes4 := (16#4e#, 16#23#, 16#12#, 16#e0#);

end ATS1155_Multi;
