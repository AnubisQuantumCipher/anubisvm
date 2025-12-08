--  KHEPRI Governance Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_Governance with
   SPARK_Mode => On,
   Refined_State => (Gov_State => (Config_Store, Proposals, Votes, Block_Counter, Admin_Addr))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Proposal_Entry is record
      Core : Proposal_Core;
      Queue_Time : Block_Number;
      Used : Boolean;
   end record;

   type Proposal_Array is array (0 .. Max_Proposals - 1) of Proposal_Entry;

   type Vote_Entry is record
      Prop_ID : Proposal_ID;
      Voter   : Address;
      Receipt : Vote_Receipt;
      Used    : Boolean;
   end record;

   type Vote_Array is array (0 .. Max_Voters - 1) of Vote_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Config_Store  : Gov_Config := Default_Config;
   Admin_Addr    : Address := Zero_Address;
   Block_Counter : Block_Number := 0;

   Proposals : Proposal_Array := (others => (
      Core => (
         ID            => U256_Zero,
         Proposer      => Zero_Address,
         Targets       => (others => (
            Target_Addr => Zero_Address,
            Value       => U256_Zero,
            Calldata    => (others => 0),
            Data_Length => 0,
            Used        => False
         )),
         Target_Count  => 0,
         Vote_Start    => 0,
         Vote_End      => 0,
         Executed      => False,
         Canceled      => False,
         For_Votes     => U256_Zero,
         Against_Votes => U256_Zero,
         Abstain_Votes => U256_Zero,
         Desc          => (Data => (others => ' '), Length => 0)
      ),
      Queue_Time => 0,
      Used       => False
   ));

   Votes : Vote_Array := (others => (
      Prop_ID => U256_Zero,
      Voter   => Zero_Address,
      Receipt => (Has_Voted => False, Support => Vote_Against, Weight => U256_Zero),
      Used    => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Proposal (ID : Proposal_ID) return Natural is
   begin
      for I in Proposals'Range loop
         if Proposals (I).Used and then Equal (Proposals (I).Core.ID, ID) then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Proposal;

   function Find_Empty_Proposal return Natural is
   begin
      for I in Proposals'Range loop
         if not Proposals (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Proposal;

   function Find_Vote (ID : Proposal_ID; Voter : Address) return Natural is
   begin
      for I in Votes'Range loop
         if Votes (I).Used
            and then Equal (Votes (I).Prop_ID, ID)
            and then Votes (I).Voter = Voter
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Vote;

   function Find_Empty_Vote return Natural is
   begin
      for I in Votes'Range loop
         if not Votes (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Vote;

   function Compute_State (Slot : Natural) return Proposal_State is
      P : Proposal_Core;
   begin
      if Slot >= Max_Proposals or else not Proposals (Slot).Used then
         return State_Pending;
      end if;

      P := Proposals (Slot).Core;

      if P.Canceled then
         return State_Canceled;
      end if;

      if P.Executed then
         return State_Executed;
      end if;

      if Block_Counter < P.Vote_Start then
         return State_Pending;
      end if;

      if Block_Counter <= P.Vote_End then
         return State_Active;
      end if;

      --  Voting ended - check result
      if Less_Than (P.For_Votes, P.Against_Votes) then
         return State_Defeated;
      end if;

      --  Check if queued
      if Proposals (Slot).Queue_Time > 0 then
         if Block_Counter >= Proposals (Slot).Queue_Time + Config_Store.Timelock_Delay then
            return State_Queued;  -- Ready for execution
         else
            return State_Queued;  -- Still in timelock
         end if;
      end if;

      return State_Succeeded;
   end Compute_State;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Config  : in Gov_Config;
      Admin   : in Address;
      Success : out Boolean
   ) is
   begin
      if Admin_Addr /= Zero_Address then
         Success := False;
         return;
      end if;

      Config_Store := Config;
      Admin_Addr := Admin;
      Block_Counter := 0;
      Success := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Get_Config return Gov_Config is
   begin
      return Config_Store;
   end Get_Config;

   function Get_Proposal_State (ID : Proposal_ID) return Proposal_State is
      Slot : constant Natural := Find_Proposal (ID);
   begin
      if Slot = Natural'Last then
         return State_Pending;
      end if;
      return Compute_State (Slot);
   end Get_Proposal_State;

   function Get_Proposal (ID : Proposal_ID) return Proposal_Core is
      Slot : constant Natural := Find_Proposal (ID);
      Empty : constant Proposal_Core := (
         ID            => U256_Zero,
         Proposer      => Zero_Address,
         Targets       => (others => (
            Target_Addr => Zero_Address,
            Value       => U256_Zero,
            Calldata    => (others => 0),
            Data_Length => 0,
            Used        => False
         )),
         Target_Count  => 0,
         Vote_Start    => 0,
         Vote_End      => 0,
         Executed      => False,
         Canceled      => False,
         For_Votes     => U256_Zero,
         Against_Votes => U256_Zero,
         Abstain_Votes => U256_Zero,
         Desc          => (Data => (others => ' '), Length => 0)
      );
   begin
      if Slot /= Natural'Last and Slot < Max_Proposals then
         return Proposals (Slot).Core;
      else
         return Empty;
      end if;
   end Get_Proposal;

   function Has_Voted (ID : Proposal_ID; Voter : Address) return Boolean is
      Slot : constant Natural := Find_Vote (ID, Voter);
   begin
      return Slot /= Natural'Last and then Votes (Slot).Receipt.Has_Voted;
   end Has_Voted;

   function Get_Vote_Receipt (
      ID    : Proposal_ID;
      Voter : Address
   ) return Vote_Receipt is
      Slot : constant Natural := Find_Vote (ID, Voter);
   begin
      if Slot /= Natural'Last and Slot < Max_Voters then
         return Votes (Slot).Receipt;
      else
         return (Has_Voted => False, Support => Vote_Against, Weight => U256_Zero);
      end if;
   end Get_Vote_Receipt;

   function Get_Votes (
      Account      : Address;
      Block_Num    : Block_Number
   ) return Voting_Power is
      pragma Unreferenced (Account, Block_Num);
   begin
      --  Placeholder: In real implementation, would query token contract
      --  for voting power at the given block
      return From_Word64 (1000);
   end Get_Votes;

   function Proposal_Count return Natural is
      Count : Natural := 0;
   begin
      for I in Proposals'Range loop
         if Proposals (I).Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Proposal_Count;

   function Current_Block return Block_Number is
   begin
      return Block_Counter;
   end Current_Block;

   function Quorum (Block_Num : Block_Number) return Voting_Power is
      pragma Unreferenced (Block_Num);
   begin
      --  Placeholder: Calculate quorum based on total supply
      return From_Word64 (1000000);
   end Quorum;

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
   ) is
      Slot : Natural;
      Voter_Power : Voting_Power;
   begin
      --  Check proposer has enough tokens
      Voter_Power := Get_Votes (Caller, Block_Counter);
      if Less_Than (Voter_Power, Config_Store.Proposal_Threshold) then
         Result_ID := U256_Zero;
         Success := False;
         Error := Error_Below_Threshold;
         return;
      end if;

      --  Find empty slot
      Slot := Find_Empty_Proposal;
      if Slot = Natural'Last then
         Result_ID := U256_Zero;
         Success := False;
         Error := Error_Invalid_Proposal;
         return;
      end if;

      --  Compute proposal ID
      Result_ID := Hash_Proposal (Targets, Target_Count, Desc);

      --  Create proposal
      Proposals (Slot) := (
         Core => (
            ID            => Result_ID,
            Proposer      => Caller,
            Targets       => Targets,
            Target_Count  => Target_Count,
            Vote_Start    => Block_Counter + Config_Store.Voting_Delay,
            Vote_End      => Block_Counter + Config_Store.Voting_Delay + Config_Store.Voting_Period,
            Executed      => False,
            Canceled      => False,
            For_Votes     => U256_Zero,
            Against_Votes => U256_Zero,
            Abstain_Votes => U256_Zero,
            Desc          => Desc
         ),
         Queue_Time => 0,
         Used       => True
      );

      Success := True;
      Error := Error_None;
   end Propose;

   procedure Cancel (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) is
      Slot : constant Natural := Find_Proposal (ID);
   begin
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Proposal_Not_Found;
         return;
      end if;

      --  Only proposer or admin can cancel
      if Caller /= Proposals (Slot).Core.Proposer
         and then Caller /= Admin_Addr
      then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Proposals (Slot).Core.Canceled := True;
      Success := True;
      Error := Error_None;
   end Cancel;

   ---------------------------------------------------------------------------
   --  Voting Functions
   ---------------------------------------------------------------------------

   procedure Cast_Vote (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Support : in     Vote_Type;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) is
      Prop_Slot : constant Natural := Find_Proposal (ID);
      Vote_Slot : Natural;
      Weight    : Voting_Power;
      New_Total : U256;
      Overflow  : Boolean;
   begin
      if Prop_Slot = Natural'Last then
         Success := False;
         Error := Error_Proposal_Not_Found;
         return;
      end if;

      --  Check voting is active
      if Compute_State (Prop_Slot) /= State_Active then
         Success := False;
         Error := Error_Voting_Closed;
         return;
      end if;

      --  Check not already voted
      if Has_Voted (ID, Caller) then
         Success := False;
         Error := Error_Already_Voted;
         return;
      end if;

      --  Get voting weight
      Weight := Get_Votes (Caller, Proposals (Prop_Slot).Core.Vote_Start);

      --  Record vote
      Vote_Slot := Find_Empty_Vote;
      if Vote_Slot = Natural'Last then
         Success := False;
         Error := Error_Invalid_State;
         return;
      end if;

      Votes (Vote_Slot) := (
         Prop_ID => ID,
         Voter   => Caller,
         Receipt => (Has_Voted => True, Support => Support, Weight => Weight),
         Used    => True
      );

      --  Update vote totals
      case Support is
         when Vote_For =>
            Add (Proposals (Prop_Slot).Core.For_Votes, Weight, New_Total, Overflow);
            if not Overflow then
               Proposals (Prop_Slot).Core.For_Votes := New_Total;
            end if;
         when Vote_Against =>
            Add (Proposals (Prop_Slot).Core.Against_Votes, Weight, New_Total, Overflow);
            if not Overflow then
               Proposals (Prop_Slot).Core.Against_Votes := New_Total;
            end if;
         when Vote_Abstain =>
            Add (Proposals (Prop_Slot).Core.Abstain_Votes, Weight, New_Total, Overflow);
            if not Overflow then
               Proposals (Prop_Slot).Core.Abstain_Votes := New_Total;
            end if;
      end case;

      Success := True;
      Error := Error_None;
   end Cast_Vote;

   procedure Cast_Vote_With_Reason (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Support : in     Vote_Type;
      Reason  : in     Description;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) is
      pragma Unreferenced (Reason);
   begin
      --  Reason is for events/logging only
      Cast_Vote (Caller, ID, Support, Success, Error);
   end Cast_Vote_With_Reason;

   ---------------------------------------------------------------------------
   --  Execution Functions
   ---------------------------------------------------------------------------

   procedure Queue (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) is
      pragma Unreferenced (Caller);
      Slot : constant Natural := Find_Proposal (ID);
   begin
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Proposal_Not_Found;
         return;
      end if;

      if Compute_State (Slot) /= State_Succeeded then
         Success := False;
         Error := Error_Not_Succeeded;
         return;
      end if;

      Proposals (Slot).Queue_Time := Block_Counter;
      Success := True;
      Error := Error_None;
   end Queue;

   procedure Execute (
      Caller  : in     Address;
      ID      : in     Proposal_ID;
      Success : out    Boolean;
      Error   : out    Gov_Error
   ) is
      pragma Unreferenced (Caller);
      Slot : constant Natural := Find_Proposal (ID);
   begin
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Proposal_Not_Found;
         return;
      end if;

      if Proposals (Slot).Core.Executed then
         Success := False;
         Error := Error_Already_Executed;
         return;
      end if;

      if Compute_State (Slot) /= State_Queued then
         Success := False;
         Error := Error_Timelock_Not_Ready;
         return;
      end if;

      --  Check timelock has passed
      if Block_Counter < Proposals (Slot).Queue_Time + Config_Store.Timelock_Delay then
         Success := False;
         Error := Error_Timelock_Not_Ready;
         return;
      end if;

      --  Execute targets (placeholder - would call target contracts)
      Proposals (Slot).Core.Executed := True;
      Success := True;
      Error := Error_None;
   end Execute;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   procedure Set_Voting_Delay (
      Caller    : in     Address;
      New_Delay : in     Block_Number;
      Success   : out    Boolean;
      Error     : out    Gov_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Config_Store.Voting_Delay := New_Delay;
      Success := True;
      Error := Error_None;
   end Set_Voting_Delay;

   procedure Set_Voting_Period (
      Caller     : in     Address;
      New_Period : in     Block_Number;
      Success    : out    Boolean;
      Error      : out    Gov_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Config_Store.Voting_Period := New_Period;
      Success := True;
      Error := Error_None;
   end Set_Voting_Period;

   procedure Set_Proposal_Threshold (
      Caller        : in     Address;
      New_Threshold : in     Voting_Power;
      Success       : out    Boolean;
      Error         : out    Gov_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Config_Store.Proposal_Threshold := New_Threshold;
      Success := True;
      Error := Error_None;
   end Set_Proposal_Threshold;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Hash_Proposal (
      Targets      : Target_Array;
      Target_Count : Natural;
      Desc         : Description
   ) return Proposal_ID is
      pragma Unreferenced (Targets, Target_Count, Desc);
   begin
      --  Placeholder: Would compute keccak256 hash of proposal data
      --  For now, return a simple incrementing ID
      return From_Word64 (Word64 (Proposal_Count + 1));
   end Hash_Proposal;

end Khepri_Governance;
