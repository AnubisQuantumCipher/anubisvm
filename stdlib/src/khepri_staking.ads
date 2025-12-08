--  KHEPRI Staking Module
--
--  Token staking with rewards distribution for KHEPRI contracts.
--  Supports flexible staking periods, reward calculation, and slashing.
--
--  Certification Target: PLATINUM

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Khepri_Types;    use Khepri_Types;

package Khepri_Staking with
   SPARK_Mode => On,
   Abstract_State => (Staking_State with External => Async_Writers),
   Initializes => Staking_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Zero_Address : constant Address := Null_Address;

   Max_Stakers      : constant := 10000;
   Max_Validators   : constant := 1000;
   Precision_Factor : constant := 1_000_000_000_000;  --  1e12 for precision

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Staked amount (256-bit)
   subtype Stake_Amount is U256;

   --  Reward amount (256-bit)
   subtype Reward_Amount is U256;

   --  Block/time stamp
   subtype Block_Number is Word64;

   ---------------------------------------------------------------------------
   --  Staking Info
   ---------------------------------------------------------------------------

   type Stake_Info is record
      Account        : Address;
      Amount         : Stake_Amount;
      Reward_Debt    : Reward_Amount;
      Pending_Reward : Reward_Amount;
      Stake_Time     : Block_Number;
      Lock_End       : Block_Number;
      Used           : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Pool Configuration
   ---------------------------------------------------------------------------

   type Pool_Config is record
      --  Token to stake
      Stake_Token    : Address;
      --  Token for rewards
      Reward_Token   : Address;
      --  Reward per block (scaled by Precision_Factor)
      Reward_Rate    : Reward_Amount;
      --  Minimum stake amount
      Min_Stake      : Stake_Amount;
      --  Lock period in blocks
      Lock_Period    : Block_Number;
      --  Early withdrawal penalty (basis points, 100 = 1%)
      Penalty_BPS    : Natural;
      --  Pool start block
      Start_Block    : Block_Number;
      --  Pool end block (0 = no end)
      End_Block      : Block_Number;
   end record;

   Default_Pool_Config : constant Pool_Config := (
      Stake_Token  => Zero_Address,
      Reward_Token => Zero_Address,
      Reward_Rate  => U256_Zero,
      Min_Stake    => U256_Zero,
      Lock_Period  => 0,
      Penalty_BPS  => 0,
      Start_Block  => 0,
      End_Block    => 0
   );

   ---------------------------------------------------------------------------
   --  Pool State
   ---------------------------------------------------------------------------

   type Pool_State is record
      Total_Staked      : Stake_Amount;
      Acc_Reward_Per_Share : Reward_Amount;
      Last_Reward_Block : Block_Number;
      Total_Rewards     : Reward_Amount;
      Distributed       : Reward_Amount;
   end record;

   ---------------------------------------------------------------------------
   --  Validator Info (for PoS networks)
   ---------------------------------------------------------------------------

   type Validator_Status is (
      Status_Inactive,
      Status_Active,
      Status_Jailed,
      Status_Unbonding
   );

   type Validator_Info is record
      Operator       : Address;
      Stake          : Stake_Amount;
      Commission_BPS : Natural;
      Status         : Validator_Status;
      Jailed_Until   : Block_Number;
      Used           : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Staking_Error is (
      Error_None,
      Error_Not_Authorized,
      Error_Insufficient_Balance,
      Error_Below_Minimum,
      Error_Lock_Not_Expired,
      Error_Pool_Not_Active,
      Error_Pool_Ended,
      Error_Already_Staked,
      Error_Not_Staked,
      Error_Overflow,
      Error_Validator_Jailed,
      Error_Invalid_Commission
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Pool_Config;
      Admin   : in Address;
      Success : out Boolean
   ) with
      Global => (In_Out => Staking_State),
      Pre    => Admin /= Zero_Address;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Get_Pool_Config return Pool_Config with
      Global => Staking_State,
      Volatile_Function;

   function Get_Pool_State return Pool_State with
      Global => Staking_State,
      Volatile_Function;

   function Get_Stake_Info (Account : Address) return Stake_Info with
      Global => Staking_State,
      Volatile_Function;

   function Staked_Balance (Account : Address) return Stake_Amount with
      Global => Staking_State,
      Volatile_Function;

   function Pending_Rewards (Account : Address) return Reward_Amount with
      Global => Staking_State,
      Volatile_Function;

   function Total_Staked return Stake_Amount with
      Global => Staking_State,
      Volatile_Function;

   function Is_Locked (Account : Address) return Boolean with
      Global => Staking_State,
      Volatile_Function;

   function Current_Block return Block_Number with
      Global => Staking_State,
      Volatile_Function;

   function APR return Natural with
      Global => Staking_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Staking Functions
   ---------------------------------------------------------------------------

   procedure Stake (
      Caller  : in     Address;
      Amount  : in     Stake_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Unstake (
      Caller  : in     Address;
      Amount  : in     Stake_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Claim_Rewards (
      Caller  : in     Address;
      Claimed : out    Reward_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Compound (
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Emergency_Withdraw (
      Caller   : in     Address;
      Received : out    Stake_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   procedure Set_Reward_Rate (
      Caller   : in     Address;
      New_Rate : in     Reward_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Add_Rewards (
      Caller  : in     Address;
      Amount  : in     Reward_Amount;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Set_Lock_Period (
      Caller     : in     Address;
      New_Period : in     Block_Number;
      Success    : out    Boolean;
      Error      : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Set_Penalty (
      Caller      : in     Address;
      New_Penalty : in     Natural;
      Success     : out    Boolean;
      Error       : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State),
      Pre    => New_Penalty <= 10000;

   procedure Pause_Pool (
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Resume_Pool (
      Caller  : in     Address;
      Success : out    Boolean;
      Error   : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   ---------------------------------------------------------------------------
   --  Validator Functions (for PoS)
   ---------------------------------------------------------------------------

   function Get_Validator (Operator : Address) return Validator_Info with
      Global => Staking_State,
      Volatile_Function;

   function Validator_Count return Natural with
      Global => Staking_State,
      Volatile_Function;

   procedure Register_Validator (
      Caller     : in     Address;
      Commission : in     Natural;
      Success    : out    Boolean;
      Error      : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State),
      Pre    => Commission <= 10000;

   procedure Delegate (
      Caller   : in     Address;
      Operator : in     Address;
      Amount   : in     Stake_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Undelegate (
      Caller   : in     Address;
      Operator : in     Address;
      Amount   : in     Stake_Amount;
      Success  : out    Boolean;
      Error    : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State);

   procedure Slash (
      Caller     : in     Address;
      Operator   : in     Address;
      Slash_BPS  : in     Natural;
      Jail_Until : in     Block_Number;
      Success    : out    Boolean;
      Error      : out    Staking_Error
   ) with
      Global => (In_Out => Staking_State),
      Pre    => Slash_BPS <= 10000;

end Khepri_Staking;
