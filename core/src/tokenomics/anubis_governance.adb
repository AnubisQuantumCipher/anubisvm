-------------------------------------------------------------------------------
--  ANUBIS Governance Timeline Implementation Body
--  4-Phase Automatic Handoff from Builder to DAO
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Governance with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Governance (
      State          : out Governance_State;
      Builder_PK     : Byte_Array;
      Genesis_Block  : Unsigned_64
   ) is
      pragma Unreferenced (Builder_PK);
   begin
      State.Current_Phase := Phase1_Builder_Control;
      State.Current_Block := Genesis_Block;
      State.Genesis_Block := Genesis_Block;

      --  Initialize privileges - Builder has limited control in Phase 1
      for P in Privilege_Type loop
         State.Privileges (P) := (
            Privilege => P,
            Current_Holder => (if P = Treasury_Access then None else Builder),
            Expires_At => Genesis_Block + Handoff_All_Privileges,
            Transferred_At => 0,
            Transfer_To => DAO
         );
      end loop;

      --  Treasury_Access is NEVER available to builder
      State.Privileges (Treasury_Access).Current_Holder := None;
      State.Privileges (Treasury_Access).Expires_At := 0;

      --  Initialize empty council
      for I in Council_Index loop
         State.Council (I) := (
            Address => (others => 0),
            PK_Hash => (others => 0),
            Elected_At => 0,
            Term_Ends => 0,
            Active => False
         );
      end loop;

      State.Council_Active := 0;
      State.Council_Formed := False;
      State.Upgrades_Proposed := 0;
      State.Upgrades_Approved := 0;
      State.Pauses_Invoked := 0;
   end Init_Governance;

   ---------------------------------------------------------------------------
   --  Phase Management
   ---------------------------------------------------------------------------

   procedure Update_Phase (
      State          : in Out Governance_State;
      Current_Block  : Unsigned_64
   ) is
      New_Phase : Governance_Phase;
   begin
      State.Current_Block := Current_Block;
      New_Phase := Calculate_Phase (Current_Block);

      if New_Phase /= State.Current_Phase then
         State.Current_Phase := New_Phase;

         --  Update privilege holders based on phase
         case New_Phase is
            when Phase1_Builder_Control =>
               null;  --  Already set

            when Phase2_Shared_Control =>
               --  Parameter control moves to council
               State.Privileges (Parameter_Tuning).Current_Holder := Council;

            when Phase3_DAO_Control =>
               --  All privileges require council or DAO
               for P in Privilege_Type loop
                  if P /= Treasury_Access then
                     State.Privileges (P).Current_Holder := Council;
                  end if;
               end loop;

            when Phase4_Full_Autonomy =>
               --  All privileges to DAO
               for P in Privilege_Type loop
                  if P /= Treasury_Access then
                     State.Privileges (P).Current_Holder := DAO;
                  end if;
               end loop;
         end case;
      end if;
   end Update_Phase;

   function Get_Current_Phase (
      State          : Governance_State
   ) return Governance_Phase is
   begin
      return State.Current_Phase;
   end Get_Current_Phase;

   function Calculate_Phase (
      Current_Block  : Unsigned_64
   ) return Governance_Phase is
   begin
      if Current_Block >= Handoff_Full_Autonomy then
         return Phase4_Full_Autonomy;
      elsif Current_Block >= Handoff_All_Privileges then
         return Phase3_DAO_Control;
      elsif Current_Block >= Handoff_Parameter_Control then
         return Phase2_Shared_Control;
      else
         return Phase1_Builder_Control;
      end if;
   end Calculate_Phase;

   function Blocks_Until_Next_Phase (
      State          : Governance_State
   ) return Unsigned_64 is
      Next_Handoff : Unsigned_64;
   begin
      case State.Current_Phase is
         when Phase1_Builder_Control =>
            Next_Handoff := Handoff_Parameter_Control;
         when Phase2_Shared_Control =>
            Next_Handoff := Handoff_All_Privileges;
         when Phase3_DAO_Control =>
            Next_Handoff := Handoff_Full_Autonomy;
         when Phase4_Full_Autonomy =>
            return 0;  --  No more phases
      end case;

      if State.Current_Block >= Next_Handoff then
         return 0;
      end if;

      return Next_Handoff - State.Current_Block;
   end Blocks_Until_Next_Phase;

   ---------------------------------------------------------------------------
   --  Privilege Checking
   ---------------------------------------------------------------------------

   function Has_Privilege (
      State          : Governance_State;
      Privilege      : Privilege_Type;
      Holder         : Privilege_Holder
   ) return Boolean is
   begin
      --  Treasury is NEVER accessible to builder
      if Privilege = Treasury_Access and then Holder = Builder then
         return False;
      end if;

      --  Check if privilege has expired
      if Privilege_Expired (State, Privilege) then
         return False;
      end if;

      return State.Privileges (Privilege).Current_Holder = Holder;
   end Has_Privilege;

   function Get_Privilege_Holder (
      State          : Governance_State;
      Privilege      : Privilege_Type
   ) return Privilege_Holder is
   begin
      return State.Privileges (Privilege).Current_Holder;
   end Get_Privilege_Holder;

   function Privilege_Expired (
      State          : Governance_State;
      Privilege      : Privilege_Type
   ) return Boolean is
   begin
      --  Treasury access is always None for builder
      if Privilege = Treasury_Access then
         return True;  --  Builder never has this
      end if;

      --  Check expiry
      if State.Privileges (Privilege).Expires_At = 0 then
         return False;  --  Never expires (DAO/Council)
      end if;

      return State.Current_Block >= State.Privileges (Privilege).Expires_At;
   end Privilege_Expired;

   ---------------------------------------------------------------------------
   --  Privilege Actions
   ---------------------------------------------------------------------------

   procedure Execute_Upgrade (
      State          : in Out Governance_State;
      Proposer       : Byte_Array;
      Upgrade_Hash   : Byte_Array;
      Council_Sigs   : Byte_Array;
      DAO_Vote_ID    : Unsigned_64;
      Result         : out Action_Result
   ) is
      pragma Unreferenced (Proposer, Upgrade_Hash);
   begin
      State.Upgrades_Proposed := State.Upgrades_Proposed + 1;

      --  Check phase requirements
      case State.Current_Phase is
         when Phase1_Builder_Control =>
            --  Builder can upgrade alone (limited)
            Result := Approved;

         when Phase2_Shared_Control =>
            --  Need council approval
            if not Verify_Council_Signatures (State,
                     (0 .. 31 => 0), Council_Sigs)
            then
               Result := Council_Required;
               return;
            end if;
            Result := Approved;

         when Phase3_DAO_Control | Phase4_Full_Autonomy =>
            --  Need DAO vote
            if not Verify_DAO_Vote (DAO_Vote_ID, 1_000_000, 5100) then
               Result := DAO_Vote_Required;
               return;
            end if;
            Result := Approved;
      end case;

      if Result = Approved then
         State.Upgrades_Approved := State.Upgrades_Approved + 1;
      end if;
   end Execute_Upgrade;

   procedure Execute_Pause (
      State          : in Out Governance_State;
      Initiator      : Byte_Array;
      Duration       : Unsigned_64;
      Reason_Hash    : Byte_Array;
      Council_Sigs   : Byte_Array;
      Result         : out Action_Result
   ) is
      pragma Unreferenced (Initiator, Reason_Hash);
   begin
      --  Check phase requirements
      case State.Current_Phase is
         when Phase1_Builder_Control =>
            --  Builder can pause but with time limit
            if Duration > Phase1_Max_Pause_Blocks then
               Result := Exceeds_Limits;
               return;
            end if;
            Result := Approved;

         when Phase2_Shared_Control | Phase3_DAO_Control |
              Phase4_Full_Autonomy =>
            --  Need council approval
            if not Verify_Council_Signatures (State,
                     (0 .. 31 => 0), Council_Sigs)
            then
               Result := Council_Required;
               return;
            end if;
            Result := Approved;
      end case;

      if Result = Approved then
         State.Pauses_Invoked := State.Pauses_Invoked + 1;
      end if;
   end Execute_Pause;

   procedure Tune_Parameter (
      State          : in Governance_State;
      Initiator      : Byte_Array;
      Parameter_ID   : Natural;
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64;
      Council_Sigs   : Byte_Array;
      Result         : out Action_Result
   ) is
      pragma Unreferenced (Initiator, Parameter_ID);
   begin
      --  Check phase requirements
      case State.Current_Phase is
         when Phase1_Builder_Control =>
            --  Builder can tune but within limits
            if not Is_Valid_Parameter_Change (Current_Value, New_Value,
                     Phase1_Max_Parameter_Change_BP)
            then
               Result := Exceeds_Limits;
               return;
            end if;
            Result := Approved;

         when Phase2_Shared_Control | Phase3_DAO_Control |
              Phase4_Full_Autonomy =>
            --  Need council approval
            if not Verify_Council_Signatures (State,
                     (0 .. 31 => 0), Council_Sigs)
            then
               Result := Council_Required;
               return;
            end if;
            Result := Approved;
      end case;
   end Tune_Parameter;

   function Is_Valid_Parameter_Change (
      Current_Value  : Unsigned_64;
      New_Value      : Unsigned_64;
      Max_Change_BP  : Natural
   ) return Boolean is
      Max_Increase : Unsigned_64;
      Max_Decrease : Unsigned_64;
   begin
      --  Calculate maximum allowed change
      Max_Increase := Current_Value +
         (Current_Value * Unsigned_64 (Max_Change_BP)) / 10_000;
      Max_Decrease := Current_Value -
         (Current_Value * Unsigned_64 (Max_Change_BP)) / 10_000;

      return New_Value <= Max_Increase and then New_Value >= Max_Decrease;
   end Is_Valid_Parameter_Change;

   ---------------------------------------------------------------------------
   --  Council Management
   ---------------------------------------------------------------------------

   procedure Form_Council (
      State          : in Out Governance_State;
      Members        : Council_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
      Active_Count : Natural := 0;
   begin
      --  Must be in Phase 2 or later
      if State.Current_Phase = Phase1_Builder_Control then
         Success := False;
         return;
      end if;

      --  Copy members and count active
      for I in Council_Index loop
         State.Council (I) := Members (I);
         if Members (I).Active then
            Active_Count := Active_Count + 1;
         end if;
      end loop;

      --  Need at least 5 active members
      if Active_Count < Council_Threshold then
         Success := False;
         return;
      end if;

      State.Council_Active := Active_Count;
      State.Council_Formed := True;

      --  Update block
      State.Current_Block := Current_Block;

      Success := True;
   end Form_Council;

   procedure Elect_Council_Member (
      State          : in Out Governance_State;
      Index          : Council_Index;
      Member         : Council_Member;
      Votes          : Unsigned_64;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Votes);
   begin
      --  Replace member at index
      State.Council (Index) := Member;

      --  Update active count
      State.Council_Active := 0;
      for I in Council_Index loop
         if State.Council (I).Active then
            State.Council_Active := State.Council_Active + 1;
         end if;
      end loop;

      Success := True;
   end Elect_Council_Member;

   procedure Remove_Council_Member (
      State          : in Out Governance_State;
      Index          : Council_Index;
      Reason_Hash    : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Reason_Hash);
   begin
      --  Deactivate member
      State.Council (Index).Active := False;

      --  Update active count
      State.Council_Active := 0;
      for I in Council_Index loop
         if State.Council (I).Active then
            State.Council_Active := State.Council_Active + 1;
         end if;
      end loop;

      Success := True;
   end Remove_Council_Member;

   function Verify_Council_Signatures (
      State          : Governance_State;
      Message_Hash   : Byte_Array;
      Signatures     : Byte_Array
   ) return Boolean is
      pragma Unreferenced (Message_Hash, Signatures);
      Valid_Sigs : Natural := 0;
   begin
      --  In production, would verify actual signatures
      --  For now, check we have enough active members
      for I in Council_Index loop
         if State.Council (I).Active then
            Valid_Sigs := Valid_Sigs + 1;
         end if;
      end loop;

      return Valid_Sigs >= Council_Threshold;
   end Verify_Council_Signatures;

   ---------------------------------------------------------------------------
   --  Automatic Handoff
   ---------------------------------------------------------------------------

   procedure Process_Handoffs (
      State          : in Out Governance_State;
      Current_Block  : Unsigned_64
   ) is
   begin
      Update_Phase (State, Current_Block);

      --  Process privilege transfers based on block number
      if Current_Block >= Handoff_Parameter_Control and then
         State.Privileges (Parameter_Tuning).Current_Holder = Builder
      then
         State.Privileges (Parameter_Tuning).Current_Holder := Council;
         State.Privileges (Parameter_Tuning).Transferred_At := Current_Block;
      end if;

      if Current_Block >= Handoff_Upgrade_Control and then
         State.Privileges (Protocol_Upgrade).Current_Holder = Builder
      then
         State.Privileges (Protocol_Upgrade).Current_Holder := Council;
         State.Privileges (Protocol_Upgrade).Transferred_At := Current_Block;
      end if;

      if Current_Block >= Handoff_Emergency_Control and then
         State.Privileges (Emergency_Pause).Current_Holder = Builder
      then
         State.Privileges (Emergency_Pause).Current_Holder := Council;
         State.Privileges (Emergency_Pause).Transferred_At := Current_Block;
      end if;

      if Current_Block >= Handoff_All_Privileges then
         for P in Privilege_Type loop
            if P /= Treasury_Access and then
               State.Privileges (P).Current_Holder = Builder
            then
               State.Privileges (P).Current_Holder := DAO;
               State.Privileges (P).Transferred_At := Current_Block;
            end if;
         end loop;
      end if;
   end Process_Handoffs;

   function Handoff_Due (
      State          : Governance_State;
      Current_Block  : Unsigned_64
   ) return Boolean is
   begin
      return Current_Block >= Next_Handoff_Block (State.Current_Block);
   end Handoff_Due;

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
   --  DAO Integration
   ---------------------------------------------------------------------------

   function Requires_DAO_Vote (
      State          : Governance_State;
      Privilege      : Privilege_Type
   ) return Boolean is
   begin
      return State.Current_Phase = Phase3_DAO_Control or else
             State.Current_Phase = Phase4_Full_Autonomy or else
             State.Privileges (Privilege).Current_Holder = DAO;
   end Requires_DAO_Vote;

   function Verify_DAO_Vote (
      Vote_ID        : Unsigned_64;
      Required_Quorum: Unsigned_64;
      Required_Threshold: Natural
   ) return Boolean is
      pragma Unreferenced (Vote_ID, Required_Quorum, Required_Threshold);
   begin
      --  In production, would query on-chain vote result
      --  Placeholder returns True
      return True;
   end Verify_DAO_Vote;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Builder_Status (
      State          : Governance_State
   ) return String is
   begin
      case State.Current_Phase is
         when Phase1_Builder_Control =>
            return "Active - Limited Control";
         when Phase2_Shared_Control =>
            return "Active - Shared with Council";
         when Phase3_DAO_Control =>
            return "Limited - Most Privileges Transferred";
         when Phase4_Full_Autonomy =>
            return "Expired - DAO Autonomous";
      end case;
   end Builder_Status;

   function Builder_Has_Privileges (
      State          : Governance_State
   ) return Boolean is
   begin
      for P in Privilege_Type loop
         if State.Privileges (P).Current_Holder = Builder then
            return True;
         end if;
      end loop;

      return False;
   end Builder_Has_Privileges;

   function Phase_Description (
      Phase          : Governance_Phase
   ) return String is
   begin
      case Phase is
         when Phase1_Builder_Control =>
            return "Builder Control (Year 0-1)";
         when Phase2_Shared_Control =>
            return "Shared Control (Year 1-2)";
         when Phase3_DAO_Control =>
            return "DAO Control (Year 2-3)";
         when Phase4_Full_Autonomy =>
            return "Full Autonomy (Year 3+)";
      end case;
   end Phase_Description;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Governance_State (
      State          : Governance_State;
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

      --  Phase
      if Idx <= Output'Last then
         Output (Idx) := Governance_Phase'Pos (State.Current_Phase);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Current_Block);
      Write_U64 (State.Genesis_Block);

      --  Flags
      if Idx <= Output'Last then
         Output (Idx) := (if State.Council_Formed then 1 else 0);
         Idx := Idx + 1;
      end if;

      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Council_Active);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Upgrades_Proposed);
      Write_U64 (State.Upgrades_Approved);

      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Pauses_Invoked mod 256);
         Idx := Idx + 1;
      end if;

      Length := Idx - Output'First;
   end Serialize_Governance_State;

   procedure Deserialize_Governance_State (
      Input          : Byte_Array;
      State          : out Governance_State;
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
      Init_Governance (State, (0 .. 31 => 0), 0);
      Success := False;

      if Input'Length < 40 then
         return;
      end if;

      --  Phase
      if Idx <= Input'Last then
         if Natural (Input (Idx)) <=
            Governance_Phase'Pos (Governance_Phase'Last)
         then
            State.Current_Phase := Governance_Phase'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Current_Block);
      Read_U64 (State.Genesis_Block);

      --  Flags
      if Idx <= Input'Last then
         State.Council_Formed := Input (Idx) /= 0;
         Idx := Idx + 1;
      end if;

      if Idx <= Input'Last then
         State.Council_Active := Natural (Input (Idx));
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Upgrades_Proposed);
      Read_U64 (State.Upgrades_Approved);

      if Idx <= Input'Last then
         State.Pauses_Invoked := Unsigned_32 (Input (Idx));
         Idx := Idx + 1;
      end if;

      Success := True;
   end Deserialize_Governance_State;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Governance_State (State : out Governance_State) is
   begin
      State.Current_Phase := Phase1_Builder_Control;
      State.Current_Block := 0;
      State.Genesis_Block := 0;

      for P in Privilege_Type loop
         State.Privileges (P) := (
            Privilege => P,
            Current_Holder => None,
            Expires_At => 0,
            Transferred_At => 0,
            Transfer_To => None
         );
      end loop;

      for I in Council_Index loop
         Zeroize_Council_Member (State.Council (I));
      end loop;

      State.Council_Active := 0;
      State.Council_Formed := False;
      State.Upgrades_Proposed := 0;
      State.Upgrades_Approved := 0;
      State.Pauses_Invoked := 0;
   end Zeroize_Governance_State;

   procedure Zeroize_Council_Member (M : out Council_Member) is
   begin
      M.Address := (others => 0);
      M.PK_Hash := (others => 0);
      M.Elected_At := 0;
      M.Term_Ends := 0;
      M.Active := False;
   end Zeroize_Council_Member;

end Anubis_Governance;
