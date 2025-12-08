--  KHEPRI Multisig Wallet Module
--
--  Multi-signature wallet for KHEPRI contracts.
--  Requires M-of-N signatures for transaction execution.
--
--  Certification Target: PLATINUM

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Khepri_Types;    use Khepri_Types;

package Khepri_Multisig with
   SPARK_Mode => On,
   Abstract_State => (Multisig_State with External => Async_Writers),
   Initializes => Multisig_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Zero_Address : constant Address := Null_Address;

   Max_Owners       : constant := 50;
   Max_Transactions : constant := 1000;
   Max_Data_Size    : constant := 4096;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Transaction ID
   subtype Transaction_ID is Natural;

   --  Owner address array
   type Owner_Address_Array is array (0 .. Max_Owners - 1) of Address;

   --  Timestamp
   subtype Timestamp is Word64;

   ---------------------------------------------------------------------------
   --  Transaction Data
   ---------------------------------------------------------------------------

   subtype Data_Length is Natural range 0 .. Max_Data_Size;
   type Tx_Data is record
      Bytes  : Byte_Array (0 .. Max_Data_Size - 1);
      Length : Data_Length;
   end record;

   ---------------------------------------------------------------------------
   --  Transaction Status
   ---------------------------------------------------------------------------

   type Tx_Status is (
      Status_Pending,
      Status_Executed,
      Status_Cancelled,
      Status_Expired
   );

   ---------------------------------------------------------------------------
   --  Transaction Record
   ---------------------------------------------------------------------------

   type Transaction is record
      ID              : Transaction_ID;
      Target          : Address;
      Value           : U256;
      Data            : Tx_Data;
      Proposer        : Address;
      Confirmations   : Natural;
      Status          : Tx_Status;
      Created_At      : Timestamp;
      Executed_At     : Timestamp;
      Used            : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Wallet Configuration
   ---------------------------------------------------------------------------

   type Multisig_Config is record
      Required_Sigs   : Natural;      --  M signatures required
      Expiry_Period   : Timestamp;    --  Tx expiry in seconds (0 = never)
      Daily_Limit     : U256;         --  Daily spending limit (0 = no limit)
   end record;

   Default_Config : constant Multisig_Config := (
      Required_Sigs => 2,
      Expiry_Period => 0,
      Daily_Limit   => U256_Zero
   );

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Multisig_Error is (
      Error_None,
      Error_Not_Owner,
      Error_Not_Authorized,
      Error_Already_Owner,
      Error_Not_Enough_Owners,
      Error_Too_Many_Owners,
      Error_Tx_Not_Found,
      Error_Already_Confirmed,
      Error_Not_Confirmed,
      Error_Already_Executed,
      Error_Insufficient_Confirmations,
      Error_Tx_Expired,
      Error_Execution_Failed,
      Error_Daily_Limit_Exceeded,
      Error_Invalid_Threshold,
      Error_Overflow
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Owners        : in Owner_Address_Array;
      Owner_Count   : in Natural;
      Required_Sigs : in Natural;
      Success       : out Boolean
   ) with
      Global => (In_Out => Multisig_State),
      Pre    => Owner_Count >= 1
                and Owner_Count <= Max_Owners
                and Required_Sigs >= 1
                and Required_Sigs <= Owner_Count;

   ---------------------------------------------------------------------------
   --  Owner Management
   ---------------------------------------------------------------------------

   function Is_Owner (Account : Address) return Boolean with
      Global => Multisig_State,
      Volatile_Function;

   function Owner_Count return Natural with
      Global => Multisig_State,
      Volatile_Function;

   function Required_Confirmations return Natural with
      Global => Multisig_State,
      Volatile_Function;

   function Get_Owner (Index : Natural) return Address with
      Global => Multisig_State,
      Volatile_Function,
      Pre    => Index < Owner_Count;

   procedure Add_Owner (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State),
      Pre    => New_Owner /= Zero_Address;

   procedure Remove_Owner (
      Caller      : in     Address;
      Owner_Addr  : in     Address;
      Success     : out    Boolean;
      Error       : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   procedure Replace_Owner (
      Caller    : in     Address;
      Old_Owner : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State),
      Pre    => New_Owner /= Zero_Address;

   procedure Change_Requirement (
      Caller       : in     Address;
      New_Required : in     Natural;
      Success      : out    Boolean;
      Error        : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   ---------------------------------------------------------------------------
   --  Transaction Management
   ---------------------------------------------------------------------------

   procedure Submit_Transaction (
      Caller  : in     Address;
      Target  : in     Address;
      Value   : in     U256;
      Data    : in     Tx_Data;
      Tx_Id   : out    Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   procedure Confirm_Transaction (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   procedure Revoke_Confirmation (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   procedure Execute_Transaction (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   procedure Cancel_Transaction (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   ---------------------------------------------------------------------------
   --  Transaction Query
   ---------------------------------------------------------------------------

   function Get_Transaction (Tx_Id : Transaction_ID) return Transaction with
      Global => Multisig_State,
      Volatile_Function;

   function Transaction_Count return Natural with
      Global => Multisig_State,
      Volatile_Function;

   function Pending_Transaction_Count return Natural with
      Global => Multisig_State,
      Volatile_Function;

   function Is_Confirmed (Tx_Id : Transaction_ID; Owner : Address) return Boolean with
      Global => Multisig_State,
      Volatile_Function;

   function Confirmation_Count (Tx_Id : Transaction_ID) return Natural with
      Global => Multisig_State,
      Volatile_Function;

   function Is_Executable (Tx_Id : Transaction_ID) return Boolean with
      Global => Multisig_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Configuration Query
   ---------------------------------------------------------------------------

   function Get_Config return Multisig_Config with
      Global => Multisig_State,
      Volatile_Function;

   function Daily_Spent return U256 with
      Global => Multisig_State,
      Volatile_Function;

   function Current_Time return Timestamp with
      Global => Multisig_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Configuration Update (requires multisig)
   ---------------------------------------------------------------------------

   procedure Set_Daily_Limit (
      Caller    : in     Address;
      New_Limit : in     U256;
      Success   : out    Boolean;
      Error     : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

   procedure Set_Expiry_Period (
      Caller     : in     Address;
      New_Expiry : in     Timestamp;
      Success    : out    Boolean;
      Error      : out    Multisig_Error
   ) with
      Global => (In_Out => Multisig_State);

end Khepri_Multisig;
