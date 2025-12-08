--  KHEPRI Governance Module
--
--  On-chain governance for KHEPRI contracts, similar to OpenZeppelin Governor.
--  Supports proposals, voting, and execution with time-locks.
--
--  Certification Target: PLATINUM

pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Khepri_Types;    use Khepri_Types;

package Khepri_Governance with
   SPARK_Mode => On,
   Abstract_State => (Gov_State with External => Async_Writers),
   Initializes => Gov_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Zero_Address : constant Address := Null_Address;

   Max_Proposals    : constant := 1000;
   Max_Voters       : constant := 10000;
   Max_Targets      : constant := 10;
   Max_Description  : constant := 1024;

   ---------------------------------------------------------------------------
   --  Proposal States
   ---------------------------------------------------------------------------

   type Proposal_State is (
      State_Pending,      --  Waiting for voting to start
      State_Active,       --  Voting is open
      State_Canceled,     --  Proposal was canceled
      State_Defeated,     --  Proposal did not reach quorum or majority
      State_Succeeded,    --  Proposal passed
      State_Queued,       --  Proposal is in timelock
      State_Expired,      --  Proposal expired in queue
      State_Executed      --  Proposal was executed
   );

   ---------------------------------------------------------------------------
   --  Vote Types
   ---------------------------------------------------------------------------

   type Vote_Type is (
      Vote_Against,
      Vote_For,
      Vote_Abstain
   );

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Proposal ID (256-bit hash)
   subtype Proposal_ID is U256;

   --  Voting power (256-bit)
   subtype Voting_Power is U256;

   --  Block number for timing
   subtype Block_Number is Word64;

   --  Description string
   subtype Desc_Length is Natural range 0 .. Max_Description;
   type Description is record
      Data   : String (1 .. Max_Description);
      Length : Desc_Length;
   end record;

   ---------------------------------------------------------------------------
   --  Proposal Targets (actions to execute)
   ---------------------------------------------------------------------------

   subtype Target_Index is Natural range 0 .. Max_Targets - 1;

   type Target_Entry is record
      Target_Addr : Address;
      Value       : U256;
      Calldata    : Byte_Array (0 .. 255);
      Data_Length : Natural;
      Used        : Boolean;
   end record;

   type Target_Array is array (Target_Index) of Target_Entry;

   ---------------------------------------------------------------------------
   --  Vote Receipt
   ---------------------------------------------------------------------------

   type Vote_Receipt is record
      Has_Voted : Boolean;
      Support   : Vote_Type;
      Weight    : Voting_Power;
   end record;

   ---------------------------------------------------------------------------
   --  Proposal Core Data
   ---------------------------------------------------------------------------

   type Proposal_Core is record
      ID             : Proposal_ID;
      Proposer       : Address;
      Targets        : Target_Array;
      Target_Count   : Natural;
      Vote_Start     : Block_Number;
      Vote_End       : Block_Number;
      Executed       : Boolean;
      Canceled       : Boolean;
      For_Votes      : Voting_Power;
      Against_Votes  : Voting_Power;
      Abstain_Votes  : Voting_Power;
      Desc           : Description;
   end record;

   ---------------------------------------------------------------------------
   --  Governance Configuration
   ---------------------------------------------------------------------------

   type Gov_Config is record
      --  Voting token contract address
      Token_Addr        : Address;
      --  Delay before voting starts (in blocks)
      Voting_Delay      : Block_Number;
      --  Duration of voting (in blocks)
      Voting_Period     : Block_Number;
      --  Minimum tokens to create proposal
      Proposal_Threshold: Voting_Power;
      --  Minimum votes for quorum (as percentage * 100, e.g., 400 = 4%)
      Quorum_Numerator  : Natural;
      --  Timelock delay (in blocks)
      Timelock_Delay    : Block_Number;
   end record;

   Default_Config : constant Gov_Config := (
      Token_Addr         => Zero_Address,
      Voting_Delay       => 1,
      Voting_Period      => 45818,  -- ~1 week at 12s blocks
      Proposal_Threshold => U256_Zero,
      Quorum_Numerator   => 400,    -- 4%
      Timelock_Delay     => 172800  -- ~2 days at 12s blocks
   );

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Gov_Error is (
      Error_None,
      Error_Not_Authorized,
      Error_Invalid_State,
      Error_Already_Voted,
      Error_Proposal_Not_Found,
      Error_Below_Threshold,
      Error_Voting_Closed,
      Error_Not_Succeeded,
      Error_Already_Executed,
      Error_Timelock_Not_Ready,
      Error_Invalid_Proposal,
      Error_Quorum_Not_Reached
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Gov_Config;
      Admin   : in Address;
      Success : out Boolean
   ) with
      Global => (In_Out => Gov_State),
      Pre    => Admin /= Zero_Address;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Get_Config return Gov_Config with
      Global => Gov_State,
      Volatile_Function;

   function Get_Proposal_State (ID : Proposal_ID) return Proposal_State with
      Global => Gov_State,
      Volatile_Function;

   function Get_Proposal (ID : Proposal_ID) return Proposal_Core with
      Global => Gov_State,
      Volatile_Function;

   function Has_Voted (ID : Proposal_ID; Voter : Address) return Boolean with
      Global => Gov_State,
      Volatile_Function;

   function Get_Vote_Receipt (
      ID    : Proposal_ID;
      Voter : Address
   ) return Vote_Receipt with
      Global => Gov_State,
      Volatile_Function;

   function Get_Votes (
      Account      : Address;
      Block_Num    : Block_Number
   ) return Voting_Power with
      Global => Gov_State,
      Volatile_Function;

   function Proposal_Count return Natural with
      Global => Gov_State,
      Volatile_Function;

   function Current_Block return Block_Number with
      Global => Gov_State,
      Volatile_Function;

   function Quorum (Block_Num : Block_Number) return Voting_Power with
      Global => Gov_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Proposal Functions
   ---------------------------------------------------------------------------

   procedure Propose (
      Caller       : in     Address;
      Targets      : in     Target_Array;
      Target_Count : in     Natural;
      Desc         : in     Description;
      Result_ID    : out    Proposal_ID;
      Success      : out    Boolean;
      Error        : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State),
      Pre    => Target_Count <= Max_Targets;

   procedure Cancel (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   ---------------------------------------------------------------------------
   --  Voting Functions
   ---------------------------------------------------------------------------

   procedure Cast_Vote (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Support : in     Vote_Type;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   procedure Cast_Vote_With_Reason (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Support : in     Vote_Type;
      Reason  : in     Description;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   ---------------------------------------------------------------------------
   --  Execution Functions
   ---------------------------------------------------------------------------

   procedure Queue (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   procedure Execute (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   procedure Set_Voting_Delay (
      Caller    : in     Address;
      New_Delay : in     Block_Number;
      Success   : out    Boolean;
      Error     : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   procedure Set_Voting_Period (
      Caller     : in     Address;
      New_Period : in     Block_Number;
      Success    : out    Boolean;
      Error      : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   procedure Set_Proposal_Threshold (
      Caller        : in     Address;
      New_Threshold : in     Voting_Power;
      Success       : out    Boolean;
      Error         : out    Gov_Error
   ) with
      Global => (In_Out => Gov_State);

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Hash_Proposal (
      Targets      : Target_Array;
      Target_Count : Natural;
      Desc         : Description
   ) return Proposal_ID with
      Global => null;

end Khepri_Governance;
