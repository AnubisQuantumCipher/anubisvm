--  KHEPRI Oracle Module
--
--  Price feed oracle for KHEPRI contracts, similar to Chainlink.
--  Provides decentralized price data with multiple data sources.
--
--  Certification Target: PLATINUM

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Khepri_Types;    use Khepri_Types;

package Khepri_Oracle with
   SPARK_Mode => On,
   Abstract_State => (Oracle_State with External => Async_Writers),
   Initializes => Oracle_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Zero_Address : constant Address := Null_Address;

   Max_Feeds       : constant := 100;
   Max_Reporters   : constant := 50;
   Max_Rounds      : constant := 1000;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Price value (256-bit, scaled by decimals)
   subtype Price_Value is U256;

   --  Round ID
   subtype Round_ID is Word64;

   --  Timestamp
   subtype Timestamp is Word64;

   ---------------------------------------------------------------------------
   --  Feed Identifier
   ---------------------------------------------------------------------------

   subtype Feed_Name_Length is Natural range 0 .. 32;
   type Feed_Name is record
      Data   : String (1 .. 32);
      Length : Feed_Name_Length;
   end record;

   ---------------------------------------------------------------------------
   --  Price Data
   ---------------------------------------------------------------------------

   type Price_Data is record
      Round_Num    : Round_ID;
      Answer       : Price_Value;
      Started_At   : Timestamp;
      Updated_At   : Timestamp;
      Answered_In  : Round_ID;
   end record;

   Null_Price_Data : constant Price_Data := (
      Round_Num   => 0,
      Answer      => U256_Zero,
      Started_At  => 0,
      Updated_At  => 0,
      Answered_In => 0
   );

   ---------------------------------------------------------------------------
   --  Feed Configuration
   ---------------------------------------------------------------------------

   type Feed_Config is record
      Name           : Feed_Name;
      Base_Token     : Address;
      Quote_Token    : Address;
      Decimals       : Natural;
      Heartbeat      : Timestamp;  --  Max seconds between updates
      Deviation_BPS  : Natural;    --  Price change threshold (basis points)
      Min_Reporters  : Natural;    --  Minimum reporters for valid price
   end record;

   Default_Feed_Config : constant Feed_Config := (
      Name          => (Data => (others => ' '), Length => 0),
      Base_Token    => Zero_Address,
      Quote_Token   => Zero_Address,
      Decimals      => 8,
      Heartbeat     => 3600,
      Deviation_BPS => 100,
      Min_Reporters => 1
   );

   ---------------------------------------------------------------------------
   --  Reporter Status
   ---------------------------------------------------------------------------

   type Reporter_Status is (
      Status_Inactive,
      Status_Active,
      Status_Suspended
   );

   type Reporter_Info is record
      Reporter_Addr : Address;
      Status        : Reporter_Status;
      Last_Report   : Timestamp;
      Report_Count  : Natural;
      Used          : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Oracle_Error is (
      Error_None,
      Error_Not_Authorized,
      Error_Feed_Not_Found,
      Error_Feed_Exists,
      Error_Reporter_Not_Found,
      Error_Reporter_Suspended,
      Error_Stale_Price,
      Error_Invalid_Price,
      Error_Insufficient_Reporters,
      Error_Overflow
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Admin   : in Address;
      Success : out Boolean
   ) with
      Global => (In_Out => Oracle_State),
      Pre    => Admin /= Zero_Address;

   ---------------------------------------------------------------------------
   --  Feed Management
   ---------------------------------------------------------------------------

   procedure Create_Feed (
      Caller  : in     Address;
      Config  : in     Feed_Config;
      Feed_Id : out    Natural;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State);

   procedure Update_Feed_Config (
      Caller  : in     Address;
      Feed_Id : in     Natural;
      Config  : in     Feed_Config;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State);

   procedure Disable_Feed (
      Caller  : in     Address;
      Feed_Id : in     Natural;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State);

   ---------------------------------------------------------------------------
   --  Reporter Management
   ---------------------------------------------------------------------------

   procedure Add_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State),
      Pre    => Reporter /= Zero_Address;

   procedure Remove_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State);

   procedure Suspend_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State);

   procedure Reinstate_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State);

   ---------------------------------------------------------------------------
   --  Price Submission
   ---------------------------------------------------------------------------

   procedure Submit_Price (
      Caller  : in     Address;
      Feed_Id : in     Natural;
      Price   : in     Price_Value;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State);

   ---------------------------------------------------------------------------
   --  Price Query (Chainlink-compatible)
   ---------------------------------------------------------------------------

   function Latest_Round_Data (Feed_Id : Natural) return Price_Data with
      Global => Oracle_State,
      Volatile_Function;

   function Get_Round_Data (
      Feed_Id   : Natural;
      Round_Num : Round_ID
   ) return Price_Data with
      Global => Oracle_State,
      Volatile_Function;

   function Latest_Answer (Feed_Id : Natural) return Price_Value with
      Global => Oracle_State,
      Volatile_Function;

   function Latest_Timestamp (Feed_Id : Natural) return Timestamp with
      Global => Oracle_State,
      Volatile_Function;

   function Latest_Round (Feed_Id : Natural) return Round_ID with
      Global => Oracle_State,
      Volatile_Function;

   function Get_Answer (
      Feed_Id   : Natural;
      Round_Num : Round_ID
   ) return Price_Value with
      Global => Oracle_State,
      Volatile_Function;

   function Get_Timestamp (
      Feed_Id   : Natural;
      Round_Num : Round_ID
   ) return Timestamp with
      Global => Oracle_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Feed Information
   ---------------------------------------------------------------------------

   function Get_Feed_Config (Feed_Id : Natural) return Feed_Config with
      Global => Oracle_State,
      Volatile_Function;

   function Feed_Decimals (Feed_Id : Natural) return Natural with
      Global => Oracle_State,
      Volatile_Function;

   function Feed_Description (Feed_Id : Natural) return Feed_Name with
      Global => Oracle_State,
      Volatile_Function;

   function Feed_Count return Natural with
      Global => Oracle_State,
      Volatile_Function;

   function Is_Feed_Active (Feed_Id : Natural) return Boolean with
      Global => Oracle_State,
      Volatile_Function;

   function Is_Price_Stale (Feed_Id : Natural) return Boolean with
      Global => Oracle_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Reporter Information
   ---------------------------------------------------------------------------

   function Get_Reporter_Info (Reporter : Address) return Reporter_Info with
      Global => Oracle_State,
      Volatile_Function;

   function Is_Reporter (Reporter : Address) return Boolean with
      Global => Oracle_State,
      Volatile_Function;

   function Reporter_Count return Natural with
      Global => Oracle_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   function Contract_Admin return Address with
      Global => Oracle_State,
      Volatile_Function;

   procedure Transfer_Admin (
      Caller    : in     Address;
      New_Admin : in     Address;
      Success   : out    Boolean;
      Error     : out    Oracle_Error
   ) with
      Global => (In_Out => Oracle_State),
      Pre    => New_Admin /= Zero_Address;

end Khepri_Oracle;
