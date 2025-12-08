-------------------------------------------------------------------------------
--  ANUBIS Dead Man"s Switch Implementation Body
--  30-Day Heartbeat + 30-Day Grace = 60 Days Maximum Absence
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Dead_Man_Switch with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Switch (
      State          : out Dead_Man_State;
      Builder_PK     : Byte_Array;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      --  Copy builder public key hash
      State.Builder_PK_Hash := (others => 0);
      for I in Builder_PK'Range loop
         if I - Builder_PK'First <= State.Builder_PK_Hash'Last then
            State.Builder_PK_Hash (I - Builder_PK'First) := Builder_PK (I);
         end if;
      end loop;

      State.State := Active;
      State.Current_Phase := Phase1_Builder_Control;
      State.Current_Block := Genesis_Block;
      State.Genesis_Block := Genesis_Block;

      State.Last_Heartbeat := Genesis_Block;
      State.Grace_Period_Start := 0;
      State.Trigger_Block := 0;

      State.Current_Pause := Not_Paused;
      State.Pause_Start := 0;
      State.Pause_End := 0;
      State.Pause_Reason := (others => 0);

      State.Current_Recovery :=None;
      State.Recovery_Votes_For := 0;
      State.Recovery_Votes_Against := 0;
      State.Recovery_Deadline := 0;

      State.Total_Heartbeats := 0;
      State.Warnings_Issued := 0;
      State.Times_Recovered := 0;
   end Init_Switch;

   procedure Init_Council (
      Council        : out Council_State;
      Formation_Block: Unsigned_64
   ) is
   begin
      for I in Council_Index loop
         Council.Members (I) := (
            Address    => (others => 0),
            PK_Hash    => (others => 0),
            Elected_At => 0,
            Active     => False,
            Votes_Cast => 0
         );
      end loop;

      Council.Active_Count := 0;
      Council.Formation_Block := Formation_Block;
      Council.Last_Election := Formation_Block;
   end Init_Council;

   ---------------------------------------------------------------------------
   --  Heartbeat Operations
   ---------------------------------------------------------------------------

   procedure Record_Heartbeat (
      State          : in Out Dead_Man_State;
      Signature      : Byte_Array;
      Message        : Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Heartbeat_Result
   ) is
      pragma Unreferenced (Signature, Message);
   begin
      --  Check if switch already triggered
      if State.State = Triggered then
         Result := Switch_Already_Triggered;
         return;
      end if;

      --  Record heartbeat
      State.Last_Heartbeat := Current_Block;
      State.Current_Block := Current_Block;
      State.Total_Heartbeats := State.Total_Heartbeats + 1;

      --  Reset to active state if in warning
      if State.State = Warning then
         State.State := Active;
         State.Grace_Period_Start := 0;
      end if;

      Result := Recorded;
   end Record_Heartbeat;

   procedure Check_Heartbeat_Status (
      State          : in Out Dead_Man_State;
      Current_Block  : Unsigned_64
   ) is
      Time_Since : Unsigned_64;
   begin
      State.Current_Block := Current_Block;

      --  Skip if already triggered or recovered
      if State.State = Triggered or else State.State = Recovered then
         return;
      end if;

      --  Calculate time since last heartbeat
      if Current_Block > State.Last_Heartbeat then
         Time_Since := Current_Block - State.Last_Heartbeat;
      else
         Time_Since := 0;
      end if;

      --  Enter warning state if heartbeat period exceeded
      if State.State = Active and then
         Time_Since >= Heartbeat_Period_Blocks
      then
         State.State := Warning;
         State.Grace_Period_Start := Current_Block;
         State.Warnings_Issued := State.Warnings_Issued + 1;
      end if;
   end Check_Heartbeat_Status;

   function Blocks_Until_Heartbeat (
      State          : Dead_Man_State;
      Current_Block  : Unsigned_64
   ) return Unsigned_64 is
      Time_Since : Unsigned_64;
   begin
      if Current_Block > State.Last_Heartbeat then
         Time_Since := Current_Block - State.Last_Heartbeat;
      else
         return Heartbeat_Period_Blocks;
      end if;

      if Time_Since >= Heartbeat_Period_Blocks then
         return 0;
      end if;

      return Heartbeat_Period_Blocks - Time_Since;
   end Blocks_Until_Heartbeat;

   function Is_Heartbeat_Overdue (
      State          : Dead_Man_State;
      Current_Block  : Unsigned_64
   ) return Boolean is
   begin
      return Blocks_Until_Heartbeat (State, Current_Block) = 0;
   end Is_Heartbeat_Overdue;

   ---------------------------------------------------------------------------
   --  Switch Trigger
   ---------------------------------------------------------------------------

   procedure Trigger_Switch (
      State          : in Out Dead_Man_State;
      Privileges     : in Out Privilege_Array;
      Current_Block  : Unsigned_64
   ) is
      DAO_Address : constant Byte_Array (0 .. 31) := (others => 0);
   begin
      State.State := Triggered;
      State.Trigger_Block := Current_Block;

      --  Transfer all privileges to DAO
      Transfer_All_Privileges (Privileges, DAO_Address, Current_Block);
   end Trigger_Switch;

   function Should_Trigger (
      State          : Dead_Man_State;
      Current_Block  : Unsigned_64
   ) return Boolean is
   begin
      return State.State = Warning and then
             Current_Block >= State.Grace_Period_Start + Grace_Period_Blocks;
   end Should_Trigger;

   procedure Transfer_All_Privileges (
      Privileges     : in Out Privilege_Array;
      DAO_Address    : Byte_Array;
      Current_Block  : Unsigned_64
   ) is
   begin
      for P in Privilege_Type loop
         --  Copy DAO address to holder
         for I in DAO_Address'Range loop
            if I - DAO_Address'First <= Privileges (P).Holder'Last then
               Privileges (P).Holder (I - DAO_Address'First) :=
                  DAO_Address (I);
            end if;
         end loop;

         Privileges (P).Transferred := True;
         Privileges (P).Transfer_Block := Current_Block;
      end loop;
   end Transfer_All_Privileges;

   ---------------------------------------------------------------------------
   --  Builder Recovery
   ---------------------------------------------------------------------------

   procedure Initiate_Recovery (
      State          : in Out Dead_Man_State;
      Identity_Proof : Byte_Array;
      Builder_Sig    : Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Recovery_Result
   ) is
      pragma Unreferenced (Identity_Proof, Builder_Sig);
      Voting_Period : constant := 604_800;  --  7 days in blocks
   begin
      --  Can only recover if triggered
      if State.State /= Triggered then
         Result := Already_Active;
         return;
      end if;

      State.Current_Recovery :=Voting;
      State.Recovery_Votes_For := 0;
      State.Recovery_Votes_Against := 0;
      State.Recovery_Deadline := Current_Block + Voting_Period;

      Result := Recovery_Initiated;
   end Initiate_Recovery;

   procedure Vote_Recovery (
      State          : in Out Dead_Man_State;
      Voter          : Byte_Array;
      Vote_Amount    : Unsigned_64;
      In_Favor       : Boolean;
      Current_Block  : Unsigned_64;
      Result         : out Recovery_Result
   ) is
      pragma Unreferenced (Voter);
   begin
      --  Check voting is active
      if State.Current_Recovery /= Voting then
         Result := Vote_Ended;
         return;
      end if;

      --  Check deadline
      if Current_Block > State.Recovery_Deadline then
         Result := Vote_Ended;
         return;
      end if;

      --  Record vote
      if In_Favor then
         State.Recovery_Votes_For :=
            State.Recovery_Votes_For + Vote_Amount;
      else
         State.Recovery_Votes_Against :=
            State.Recovery_Votes_Against + Vote_Amount;
      end if;

      Result := Vote_Recorded;
   end Vote_Recovery;

   procedure Finalize_Recovery (
      State          : in Out Dead_Man_State;
      Privileges     : in Out Privilege_Array;
      Builder_Address: Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Recovery_Result
   ) is
      Total_Votes : Unsigned_64;
      Threshold_Votes : Unsigned_64;
   begin
      --  Check voting is complete
      if State.Current_Recovery /= Voting then
         Result := Vote_Ended;
         return;
      end if;

      Total_Votes := State.Recovery_Votes_For + State.Recovery_Votes_Against;

      if Total_Votes = 0 then
         State.Current_Recovery :=Anubis_Dead_Man_Switch.Rejected;
         Result := Recovery_Result'(Anubis_Dead_Man_Switch.Rejected);
         return;
      end if;

      --  Calculate threshold (67%)
      Threshold_Votes := (Total_Votes * Recovery_Vote_Threshold_BP) / 10_000;

      if State.Recovery_Votes_For >= Threshold_Votes then
         --  Recovery approved
         State.State := Recovered;
         State.Current_Recovery :=Anubis_Dead_Man_Switch.Approved;
         State.Times_Recovered := State.Times_Recovered + 1;
         State.Last_Heartbeat := Current_Block;

         --  Transfer privileges back to builder
         for P in Privilege_Type loop
            for I in Builder_Address'Range loop
               if I - Builder_Address'First <= Privileges (P).Holder'Last then
                  Privileges (P).Holder (I - Builder_Address'First) :=
                     Builder_Address (I);
               end if;
            end loop;
            Privileges (P).Transferred := False;
         end loop;

         Result := Recovery_Result'(Anubis_Dead_Man_Switch.Approved);
      else
         State.Current_Recovery :=Anubis_Dead_Man_Switch.Rejected;
         Result := Recovery_Result'(Anubis_Dead_Man_Switch.Rejected);
      end if;
   end Finalize_Recovery;

   ---------------------------------------------------------------------------
   --  Governance Phase Management
   ---------------------------------------------------------------------------

   procedure Update_Phase (
      State          : in Out Dead_Man_State;
      Current_Block  : Unsigned_64
   ) is
   begin
      State.Current_Block := Current_Block;

      if Current_Block < Phase1_End_Block then
         State.Current_Phase := Phase1_Builder_Control;
      elsif Current_Block < Phase2_End_Block then
         State.Current_Phase := Phase2_Shared_Control;
      elsif Current_Block < Phase3_End_Block then
         State.Current_Phase := Phase3_DAO_Control;
      else
         State.Current_Phase := Phase4_Full_Autonomy;
      end if;
   end Update_Phase;

   function Get_Phase (
      State          : Dead_Man_State
   ) return Governance_Phase is
   begin
      return State.Current_Phase;
   end Get_Phase;

   function Privilege_Available (
      State          : Dead_Man_State;
      Privilege      : Privilege_Type
   ) return Boolean is
   begin
      case State.Current_Phase is
         when Phase1_Builder_Control =>
            return Privilege /= Treasury_Access;

         when Phase2_Shared_Control =>
            return Privilege = Protocol_Upgrade or else
                   Privilege = Emergency_Pause;

         when Phase3_DAO_Control =>
            return Privilege = Emergency_Pause;

         when Phase4_Full_Autonomy =>
            return False;
      end case;
   end Privilege_Available;

   function Privilege_Expires_At (
      Privilege      : Privilege_Type
   ) return Unsigned_64 is
   begin
      case Privilege is
         when Protocol_Upgrade =>
            return Phase2_End_Block;
         when Emergency_Pause =>
            return Phase3_End_Block;
         when Parameter_Tuning | Bug_Bounty_Approval | Validator_Removal =>
            return Phase1_End_Block;
         when Treasury_Access =>
            return 0;  --  Never available to builder
      end case;
   end Privilege_Expires_At;

   ---------------------------------------------------------------------------
   --  Emergency Pause
   ---------------------------------------------------------------------------

   procedure Builder_Emergency_Pause (
      State          : in Out Dead_Man_State;
      Reason         : Byte_Array;
      Duration       : Unsigned_64;
      Current_Block  : Unsigned_64;
      Result         : out Pause_Result
   ) is
   begin
      --  Check phase allows builder pause
      if State.Current_Phase = Phase4_Full_Autonomy then
         Result := Phase_Not_Allowed;
         return;
      end if;

      --  Check not already paused
      if State.Current_Pause /= Not_Paused then
         Result := Already_Paused;
         return;
      end if;

      --  Check duration
      if Duration > Max_Emergency_Pause_Blocks then
         Result := Duration_Exceeded;
         return;
      end if;

      --  Execute pause
      State.Current_Pause := Paused_By_Builder;
      State.Pause_Start := Current_Block;
      State.Pause_End := Current_Block + Duration;

      --  Copy reason
      State.Pause_Reason := (others => 0);
      for I in Reason'Range loop
         if I - Reason'First <= State.Pause_Reason'Last then
            State.Pause_Reason (I - Reason'First) := Reason (I);
         end if;
      end loop;

      Result := Paused;
   end Builder_Emergency_Pause;

   procedure Council_Emergency_Pause (
      State          : in Out Dead_Man_State;
      Council        : Council_State;
      Signatures     : Byte_Array;
      Reason         : Byte_Array;
      Duration       : Unsigned_64;
      Current_Block  : Unsigned_64;
      Result         : out Pause_Result
   ) is
      pragma Unreferenced (Signatures);
   begin
      --  Check council threshold
      if Council.Active_Count < Council_Threshold then
         Result := Unauthorized;
         return;
      end if;

      --  Check not already paused
      if State.Current_Pause /= Not_Paused then
         Result := Already_Paused;
         return;
      end if;

      --  Check duration
      if Duration > Max_Emergency_Pause_Blocks then
         Result := Duration_Exceeded;
         return;
      end if;

      --  Execute pause
      State.Current_Pause := Paused_By_Council;
      State.Pause_Start := Current_Block;
      State.Pause_End := Current_Block + Duration;

      --  Copy reason
      State.Pause_Reason := (others => 0);
      for I in Reason'Range loop
         if I - Reason'First <= State.Pause_Reason'Last then
            State.Pause_Reason (I - Reason'First) := Reason (I);
         end if;
      end loop;

      Result := Paused;
   end Council_Emergency_Pause;

   procedure Unpause (
      State          : in Out Dead_Man_State;
      Authorizer     : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Pause_Result
   ) is
      pragma Unreferenced (Authorizer, Signature);
   begin
      if State.Current_Pause = Not_Paused then
         Result := Already_Paused;
         return;
      end if;

      State.Current_Pause := Not_Paused;
      State.Pause_Start := 0;
      State.Pause_End := 0;
      State.Pause_Reason := (others => 0);

      Result := Paused;
   end Unpause;

   function Is_Paused (State : Dead_Man_State) return Boolean is
   begin
      return State.Current_Pause /= Not_Paused;
   end Is_Paused;

   ---------------------------------------------------------------------------
   --  Parameter Tuning
   ---------------------------------------------------------------------------

   procedure Tune_Parameter (
      State          : Dead_Man_State;
      Parameter      : Parameter_ID;
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64;
      Builder_Sig    : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Parameter, Builder_Sig);
   begin
      --  Check phase allows parameter tuning
      if State.Current_Phase /= Phase1_Builder_Control then
         Success := False;
         return;
      end if;

      --  Check change is within limits
      if not Is_Valid_Change (Current_Value, New_Value) then
         Success := False;
         return;
      end if;

      Success := True;
   end Tune_Parameter;

   function Is_Valid_Change (
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64
   ) return Boolean is
      Max_Change : Unsigned_64;
      Difference : Unsigned_64;
   begin
      --  Calculate 20% of current value
      Max_Change := (Current_Value * Max_Parameter_Change_BP) / 10_000;

      --  Calculate absolute difference
      if New_Value > Current_Value then
         Difference := New_Value - Current_Value;
      else
         Difference := Current_Value - New_Value;
      end if;

      return Difference <= Max_Change;
   end Is_Valid_Change;

   ---------------------------------------------------------------------------
   --  Council Management
   ---------------------------------------------------------------------------

   procedure Add_Council_Member (
      Council        : in Out Council_State;
      Member_Address : Byte_Array;
      Member_PK      : Byte_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      --  Find empty slot
      for I in Council_Index loop
         if not Council.Members (I).Active then
            --  Copy address
            for J in Member_Address'Range loop
               if J - Member_Address'First <= Council.Members (I).Address'Last then
                  Council.Members (I).Address (J - Member_Address'First) :=
                     Member_Address (J);
               end if;
            end loop;

            --  Copy PK hash
            for J in Member_PK'Range loop
               if J - Member_PK'First <= Council.Members (I).PK_Hash'Last then
                  Council.Members (I).PK_Hash (J - Member_PK'First) :=
                     Member_PK (J);
               end if;
            end loop;

            Council.Members (I).Elected_At := Current_Block;
            Council.Members (I).Active := True;
            Council.Members (I).Votes_Cast := 0;

            Council.Active_Count := Council.Active_Count + 1;
            Success := True;
            return;
         end if;
      end loop;

      Success := False;
   end Add_Council_Member;

   procedure Remove_Council_Member (
      Council        : in Out Council_State;
      Index          : Council_Index;
      Success        : out Boolean
   ) is
   begin
      if not Council.Members (Index).Active then
         Success := False;
         return;
      end if;

      Council.Members (Index).Address := (others => 0);
      Council.Members (Index).PK_Hash := (others => 0);
      Council.Members (Index).Elected_At := 0;
      Council.Members (Index).Active := False;
      Council.Members (Index).Votes_Cast := 0;

      if Council.Active_Count > 0 then
         Council.Active_Count := Council.Active_Count - 1;
      end if;

      Success := True;
   end Remove_Council_Member;

   function Check_Council_Threshold (
      Council        : Council_State;
      Signers        : Natural
   ) return Boolean is
      pragma Unreferenced (Council);
   begin
      return Signers >= Council_Threshold;
   end Check_Council_Threshold;

   ---------------------------------------------------------------------------
   --  Privilege Queries
   ---------------------------------------------------------------------------

   function Get_Privilege_Holder (
      Privileges     : Privilege_Array;
      Privilege      : Privilege_Type
   ) return Byte_Array is
   begin
      return Privileges (Privilege).Holder;
   end Get_Privilege_Holder;

   function Has_Privilege (
      Privileges     : Privilege_Array;
      Privilege      : Privilege_Type;
      Address        : Byte_Array
   ) return Boolean is
      Match : Boolean := True;
   begin
      for I in Address'Range loop
         if I - Address'First <= Privileges (Privilege).Holder'Last then
            if Address (I) /=
               Privileges (Privilege).Holder (I - Address'First)
            then
               Match := False;
               exit;
            end if;
         end if;
      end loop;

      return Match and then not Privileges (Privilege).Transferred;
   end Has_Privilege;

   ---------------------------------------------------------------------------
   --  Automatic Handoff Schedule
   ---------------------------------------------------------------------------

   procedure Check_Handoff (
      State          : in Out Dead_Man_State;
      Privileges     : in Out Privilege_Array;
      Current_Block  : Unsigned_64
   ) is
   begin
      State.Current_Block := Current_Block;

      --  Update governance phase
      Update_Phase (State, Current_Block);

      --  Expire privileges based on schedule
      for P in Privilege_Type loop
         if Current_Block >= Privilege_Expires_At (P) then
            Privileges (P).Expires_At := Privilege_Expires_At (P);
         end if;
      end loop;

      --  Check if should trigger switch due to missed heartbeat
      if Should_Trigger (State, Current_Block) then
         Trigger_Switch (State, Privileges, Current_Block);
      end if;
   end Check_Handoff;

   function Next_Handoff_Block (
      Current_Block  : Unsigned_64
   ) return Unsigned_64 is
   begin
      if Current_Block < Handoff_Parameter_Control then
         return Handoff_Parameter_Control;
      elsif Current_Block < Handoff_Upgrade_Control then
         return Handoff_Upgrade_Control;
      elsif Current_Block < Handoff_Emergency_Control then
         return Handoff_Emergency_Control;
      elsif Current_Block < Handoff_All_Privileges then
         return Handoff_All_Privileges;
      elsif Current_Block < Handoff_Full_Autonomy then
         return Handoff_Full_Autonomy;
      else
         return 0;  --  All handoffs complete
      end if;
   end Next_Handoff_Block;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Switch_State (
      State          : Dead_Man_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Output'First;

      procedure Write_U64 (Val : Unsigned_64) is
         V : Unsigned_64 := Val;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U64;

   begin
      Output := (others => 0);

      --  Builder PK Hash
      for I in State.Builder_PK_Hash'Range loop
         if Idx <= Output'Last then
            Output (Idx) := State.Builder_PK_Hash (I);
            Idx := Idx + 1;
         end if;
      end loop;

      --  State values
      if Idx <= Output'Last then
         Output (Idx) := Switch_State'Pos (State.State);
         Idx := Idx + 1;
      end if;

      if Idx <= Output'Last then
         Output (Idx) := Governance_Phase'Pos (State.Current_Phase);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Current_Block);
      Write_U64 (State.Genesis_Block);
      Write_U64 (State.Last_Heartbeat);
      Write_U64 (State.Grace_Period_Start);
      Write_U64 (State.Trigger_Block);

      --  Pause state
      if Idx <= Output'Last then
         Output (Idx) := Pause_State'Pos (State.Current_Pause);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Pause_Start);
      Write_U64 (State.Pause_End);

      --  Recovery state
      if Idx <= Output'Last then
         Output (Idx) := Recovery_State'Pos (State.Current_Recovery);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Recovery_Votes_For);
      Write_U64 (State.Recovery_Votes_Against);
      Write_U64 (State.Recovery_Deadline);

      --  Statistics
      Write_U64 (State.Total_Heartbeats);

      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Warnings_Issued mod 256);
         Idx := Idx + 1;
      end if;

      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Times_Recovered mod 256);
         Idx := Idx + 1;
      end if;

      Length := Idx - Output'First;
   end Serialize_Switch_State;

   procedure Deserialize_Switch_State (
      Input          : Byte_Array;
      State          : out Dead_Man_State;
      Success        : out Boolean
   ) is
      Idx : Natural := Input'First;

      procedure Read_U64 (Val : out Unsigned_64) is
      begin
         Val := 0;
         for J in 0 .. 7 loop
            if Idx <= Input'Last then
               Val := Val + Unsigned_64 (Input (Idx)) * (256 ** J);
               Idx := Idx + 1;
            end if;
         end loop;
      end Read_U64;

   begin
      --  Initialize
      Init_Switch (State, (0 .. 31 => 0), 0);
      Success := False;

      if Input'Length < 100 then
         return;
      end if;

      --  Builder PK Hash
      for I in State.Builder_PK_Hash'Range loop
         if Idx <= Input'Last then
            State.Builder_PK_Hash (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      --  State values
      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Switch_State'Pos (Switch_State'Last) then
            State.State := Switch_State'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Governance_Phase'Pos (Governance_Phase'Last) then
            State.Current_Phase := Governance_Phase'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Current_Block);
      Read_U64 (State.Genesis_Block);
      Read_U64 (State.Last_Heartbeat);
      Read_U64 (State.Grace_Period_Start);
      Read_U64 (State.Trigger_Block);

      --  Pause state
      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Pause_State'Pos (Pause_State'Last) then
            State.Current_Pause := Pause_State'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Pause_Start);
      Read_U64 (State.Pause_End);

      --  Recovery state
      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Recovery_State'Pos (Recovery_State'Last) then
            State.Current_Recovery :=Recovery_State'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Recovery_Votes_For);
      Read_U64 (State.Recovery_Votes_Against);
      Read_U64 (State.Recovery_Deadline);

      --  Statistics
      Read_U64 (State.Total_Heartbeats);

      if Idx <= Input'Last then
         State.Warnings_Issued := Unsigned_32 (Input (Idx));
         Idx := Idx + 1;
      end if;

      if Idx <= Input'Last then
         State.Times_Recovered := Unsigned_32 (Input (Idx));
         Idx := Idx + 1;
      end if;

      Success := True;
   end Deserialize_Switch_State;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Switch_State (State : out Dead_Man_State) is
   begin
      State.Builder_PK_Hash := (others => 0);
      State.State := Active;
      State.Current_Phase := Phase1_Builder_Control;
      State.Current_Block := 0;
      State.Genesis_Block := 0;
      State.Last_Heartbeat := 0;
      State.Grace_Period_Start := 0;
      State.Trigger_Block := 0;
      State.Current_Pause := Not_Paused;
      State.Pause_Start := 0;
      State.Pause_End := 0;
      State.Pause_Reason := (others => 0);
      State.Current_Recovery :=None;
      State.Recovery_Votes_For := 0;
      State.Recovery_Votes_Against := 0;
      State.Recovery_Deadline := 0;
      State.Total_Heartbeats := 0;
      State.Warnings_Issued := 0;
      State.Times_Recovered := 0;
   end Zeroize_Switch_State;

   procedure Zeroize_Council_State (Council : out Council_State) is
   begin
      for I in Council_Index loop
         Council.Members (I).Address := (others => 0);
         Council.Members (I).PK_Hash := (others => 0);
         Council.Members (I).Elected_At := 0;
         Council.Members (I).Active := False;
         Council.Members (I).Votes_Cast := 0;
      end loop;

      Council.Active_Count := 0;
      Council.Formation_Block := 0;
      Council.Last_Election := 0;
   end Zeroize_Council_State;

   procedure Zeroize_Heartbeat (Heartbeat : out Heartbeat_Record) is
   begin
      Heartbeat.Builder_Address := (others => 0);
      Heartbeat.Last_Heartbeat := 0;
      Heartbeat.Heartbeat_Count := 0;
      Heartbeat.Signature := (others => 0);
      Heartbeat.Message_Hash := (others => 0);
   end Zeroize_Heartbeat;

end Anubis_Dead_Man_Switch;
