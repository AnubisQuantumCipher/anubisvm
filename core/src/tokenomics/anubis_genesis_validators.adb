-------------------------------------------------------------------------------
--  ANUBIS Genesis Validators Implementation Body
--  15% Allocation (150M ANUBIS) for Genesis Validators
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Genesis_Validators with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Validator_Set (
      State          : out Validator_Set_State;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      --  Initialize all validators
      for I in Validator_Index loop
         State.Validators (I) := (
            Index             => I,
            Address           => (others => 0),
            PK_Hash           => (others => 0),
            Consensus_PK      => (others => 0),
            Status            => Pending_Application,
            Is_Genesis        => False,
            Region            => North_America,
            Self_Stake        => 0,
            Delegated_Stake   => 0,
            Total_Stake       => 0,
            Blocks_Produced   => 0,
            Blocks_Missed     => 0,
            Total_Rewards     => 0,
            Pending_Rewards   => 0,
            Commission_Rate_BP => 0,
            Times_Slashed     => 0,
            Total_Slashed     => 0,
            Jail_End_Block    => 0,
            Registered_At     => 0,
            Active_Since      => 0,
            Last_Block        => 0,
            Unbonding_End     => 0
         );
      end loop;

      State.Active_Count := 0;
      State.Genesis_Count := 0;
      State.Total_Stake := 0;
      State.Total_Distributed := 0;
      State.Remaining_Pool := Validator_Allocation;
      State.Current_Block_Reward := Initial_Block_Reward;
      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
      State.Current_Epoch := 0;
      State.Open_Entry_Active := False;
      State.Post_Genesis_Count := 0;
   end Init_Validator_Set;

   ---------------------------------------------------------------------------
   --  Application Process
   ---------------------------------------------------------------------------

   procedure Submit_Application (
      Application    : Validator_Application;
      Current_Block  : Unsigned_64;
      Result         : out Apply_Result
   ) is
      pragma Unreferenced (Current_Block);
   begin
      --  Check hardware requirements
      if not Verify_Hardware (Application.Hardware) then
         Result := Insufficient_Specs;
         return;
      end if;

      --  Check minimum stake commitment
      if Application.Committed_Stake < Min_Stake then
         Result := Invalid_Stake;
         return;
      end if;

      Result := Application_Submitted;
   end Submit_Application;

   procedure Review_Application (
      Application    : in Out Validator_Application;
      Reviewer       : Byte_Array;
      Approved       : Boolean;
      Notes_Hash     : Byte_Array;
      Result         : out Application_Status
   ) is
   begin
      --  Copy reviewer address
      for I in Reviewer'Range loop
         if I - Reviewer'First <= Application.Reviewer'Last then
            Application.Reviewer (I - Reviewer'First) := Reviewer (I);
         end if;
      end loop;

      --  Copy notes hash
      for I in Notes_Hash'Range loop
         if I - Notes_Hash'First <= Application.Review_Notes_Hash'Last then
            Application.Review_Notes_Hash (I - Notes_Hash'First) := Notes_Hash (I);
         end if;
      end loop;

      if Approved then
         Application.Status := Application_Status'(Anubis_Genesis_Validators.Approved);
         Result := Application_Status'(Anubis_Genesis_Validators.Approved);
      else
         Application.Status := Application_Status'(Rejected);
         Result := Application_Status'(Rejected);
      end if;
   end Review_Application;

   function Verify_Hardware (
      Specs          : Hardware_Specs
   ) return Boolean is
   begin
      return Specs.CPU_Cores >= Min_CPU_Cores
             and then Specs.RAM_GB >= Min_RAM_GB
             and then Specs.Storage_TB >= Min_Storage_TB
             and then Specs.Network_Mbps >= Min_Network_Mbps;
   end Verify_Hardware;

   function Check_Regional_Balance (
      State          : Validator_Set_State;
      Region         : Geographic_Region
   ) return Boolean is
      Count : Natural := 0;
      Max_Per_Region : constant Natural := Genesis_Validator_Count / 5;
   begin
      for I in Validator_Index loop
         if State.Validators (I).Status = Active and then
            State.Validators (I).Region = Region
         then
            Count := Count + 1;
         end if;
      end loop;

      return Count < Max_Per_Region;
   end Check_Regional_Balance;

   ---------------------------------------------------------------------------
   --  Validator Registration
   ---------------------------------------------------------------------------

   procedure Register_Validator (
      State          : in Out Validator_Set_State;
      Application    : Validator_Application;
      Stake          : Unsigned_64;
      Consensus_PK   : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Register_Result
   ) is
      pragma Unreferenced (Signature);
      Slot : Validator_Index := 0;
      Found_Slot : Boolean := False;
   begin
      --  Check application status
      if Application.Status /= Application_Status'(Anubis_Genesis_Validators.Approved) then
         Result := Not_Approved;
         return;
      end if;

      --  Check stake
      if Stake < Min_Stake then
         Result := Insufficient_Stake;
         return;
      end if;

      if Stake > Max_Stake then
         Result := Stake_Cap_Exceeded;
         return;
      end if;

      --  Find empty slot
      for I in Validator_Index loop
         if State.Validators (I).Status = Pending_Application and then
            State.Validators (I).Registered_At = 0
         then
            Slot := I;
            Found_Slot := True;
            exit;
         end if;
      end loop;

      if not Found_Slot then
         Result := Not_Approved;
         return;
      end if;

      --  Copy applicant address (both arrays are 0..31)
      for I in Application.Applicant'Range loop
         State.Validators (Slot).Address (I) := Application.Applicant (I);
      end loop;

      --  Copy consensus PK (both arrays are 0..31)
      for I in Consensus_PK'Range loop
         State.Validators (Slot).Consensus_PK (I) := Consensus_PK (I);
      end loop;

      State.Validators (Slot).Index := Slot;
      State.Validators (Slot).Status := Validator_Status'(Anubis_Genesis_Validators.Approved);
      State.Validators (Slot).Is_Genesis := True;
      State.Validators (Slot).Region := Application.Region;
      State.Validators (Slot).Self_Stake := Stake;
      State.Validators (Slot).Total_Stake := Stake;
      State.Validators (Slot).Registered_At := State.Current_Block;

      State.Genesis_Count := State.Genesis_Count + 1;
      State.Total_Stake := State.Total_Stake + Stake;

      Result := Registered;
   end Register_Validator;

   procedure Add_Stake (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Amount         : Unsigned_64;
      Is_Self        : Boolean;
      Success        : out Boolean
   ) is
   begin
      if State.Validators (Validator_Idx).Status = Pending_Application or else
         State.Validators (Validator_Idx).Status = Removed
      then
         Success := False;
         return;
      end if;

      --  Check stake cap
      if State.Validators (Validator_Idx).Total_Stake + Amount > Max_Stake then
         Success := False;
         return;
      end if;

      if Is_Self then
         State.Validators (Validator_Idx).Self_Stake :=
            State.Validators (Validator_Idx).Self_Stake + Amount;
      else
         State.Validators (Validator_Idx).Delegated_Stake :=
            State.Validators (Validator_Idx).Delegated_Stake + Amount;
      end if;

      State.Validators (Validator_Idx).Total_Stake :=
         State.Validators (Validator_Idx).Total_Stake + Amount;
      State.Total_Stake := State.Total_Stake + Amount;

      Success := True;
   end Add_Stake;

   procedure Begin_Unstake (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Amount         : Unsigned_64;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      if Amount > State.Validators (Validator_Idx).Self_Stake then
         Success := False;
         return;
      end if;

      --  Check minimum stake after unstake
      if State.Validators (Validator_Idx).Self_Stake - Amount < Min_Stake and then
         State.Validators (Validator_Idx).Self_Stake - Amount > 0
      then
         Success := False;
         return;
      end if;

      State.Validators (Validator_Idx).Status := Unbonding;
      State.Validators (Validator_Idx).Unbonding_End :=
         Current_Block + Unbonding_Period_Blocks;

      Success := True;
   end Begin_Unstake;

   procedure Complete_Unstake (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Current_Block  : Unsigned_64;
      Released       : out Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      Released := 0;

      if State.Validators (Validator_Idx).Status /= Unbonding then
         Success := False;
         return;
      end if;

      if Current_Block < State.Validators (Validator_Idx).Unbonding_End then
         Success := False;
         return;
      end if;

      Released := State.Validators (Validator_Idx).Self_Stake;
      State.Total_Stake := State.Total_Stake - Released;
      State.Validators (Validator_Idx).Self_Stake := 0;
      State.Validators (Validator_Idx).Total_Stake :=
         State.Validators (Validator_Idx).Delegated_Stake;
      State.Validators (Validator_Idx).Status := Removed;

      if State.Active_Count > 0 then
         State.Active_Count := State.Active_Count - 1;
      end if;

      Success := True;
   end Complete_Unstake;

   ---------------------------------------------------------------------------
   --  Block Production
   ---------------------------------------------------------------------------

   procedure Record_Block_Produced (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Block_Number   : Unsigned_64
   ) is
   begin
      if State.Validators (Validator_Idx).Status = Active then
         State.Validators (Validator_Idx).Blocks_Produced :=
            State.Validators (Validator_Idx).Blocks_Produced + 1;
         State.Validators (Validator_Idx).Last_Block := Block_Number;
      end if;
   end Record_Block_Produced;

   procedure Record_Block_Missed (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Block_Number   : Unsigned_64
   ) is
      pragma Unreferenced (Block_Number);
   begin
      if State.Validators (Validator_Idx).Status = Active then
         State.Validators (Validator_Idx).Blocks_Missed :=
            State.Validators (Validator_Idx).Blocks_Missed + 1;
      end if;
   end Record_Block_Missed;

   function Calculate_Block_Reward (
      State          : Validator_Set_State
   ) return Unsigned_64 is
   begin
      return State.Current_Block_Reward;
   end Calculate_Block_Reward;

   procedure Distribute_Block_Reward (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Block_Reward   : Unsigned_64
   ) is
   begin
      if Block_Reward > State.Remaining_Pool then
         return;
      end if;

      State.Validators (Validator_Idx).Pending_Rewards :=
         State.Validators (Validator_Idx).Pending_Rewards + Block_Reward;
      State.Remaining_Pool := State.Remaining_Pool - Block_Reward;
      State.Total_Distributed := State.Total_Distributed + Block_Reward;
   end Distribute_Block_Reward;

   procedure Claim_Rewards (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Claimed        : out Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      Claimed := State.Validators (Validator_Idx).Pending_Rewards;

      if Claimed = 0 then
         Success := False;
         return;
      end if;

      State.Validators (Validator_Idx).Total_Rewards :=
         State.Validators (Validator_Idx).Total_Rewards + Claimed;
      State.Validators (Validator_Idx).Pending_Rewards := 0;

      Success := True;
   end Claim_Rewards;

   ---------------------------------------------------------------------------
   --  Slashing
   ---------------------------------------------------------------------------

   procedure Slash_Double_Sign (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Evidence_Hash  : Byte_Array;
      Result         : out Slash_Result
   ) is
      pragma Unreferenced (Evidence_Hash);
      Slash_Amount : Unsigned_64;
   begin
      if State.Validators (Validator_Idx).Status = Pending_Application or else
         State.Validators (Validator_Idx).Status = Removed
      then
         Result := Not_Validator;
         return;
      end if;

      if State.Validators (Validator_Idx).Status = Jailed then
         Result := Already_Jailed;
         return;
      end if;

      --  Calculate slash amount
      Slash_Amount :=
         (State.Validators (Validator_Idx).Total_Stake * Slash_Double_Sign_BP) / 10_000;

      --  Apply slash
      if Slash_Amount >= State.Validators (Validator_Idx).Total_Stake then
         State.Validators (Validator_Idx).Total_Stake := 0;
         State.Validators (Validator_Idx).Self_Stake := 0;
      else
         State.Validators (Validator_Idx).Total_Stake :=
            State.Validators (Validator_Idx).Total_Stake - Slash_Amount;
         if Slash_Amount >= State.Validators (Validator_Idx).Self_Stake then
            State.Validators (Validator_Idx).Self_Stake := 0;
         else
            State.Validators (Validator_Idx).Self_Stake :=
               State.Validators (Validator_Idx).Self_Stake - Slash_Amount;
         end if;
      end if;

      State.Validators (Validator_Idx).Total_Slashed :=
         State.Validators (Validator_Idx).Total_Slashed + Slash_Amount;
      State.Validators (Validator_Idx).Times_Slashed :=
         State.Validators (Validator_Idx).Times_Slashed + 1;
      State.Validators (Validator_Idx).Status := Slashed;
      State.Total_Stake := State.Total_Stake - Slash_Amount;

      Result := Slash_Result'(Anubis_Genesis_Validators.Slashed);
   end Slash_Double_Sign;

   procedure Slash_Downtime (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Missed_Blocks  : Natural;
      Result         : out Slash_Result
   ) is
      pragma Unreferenced (Missed_Blocks);
      Slash_Amount : Unsigned_64;
   begin
      if State.Validators (Validator_Idx).Status = Pending_Application or else
         State.Validators (Validator_Idx).Status = Removed
      then
         Result := Not_Validator;
         return;
      end if;

      if State.Validators (Validator_Idx).Status = Jailed then
         Result := Already_Jailed;
         return;
      end if;

      --  Calculate slash amount for downtime
      Slash_Amount :=
         (State.Validators (Validator_Idx).Total_Stake * Slash_Downtime_BP) / 10_000;

      --  Apply slash
      if Slash_Amount >= State.Validators (Validator_Idx).Total_Stake then
         State.Validators (Validator_Idx).Total_Stake := 0;
      else
         State.Validators (Validator_Idx).Total_Stake :=
            State.Validators (Validator_Idx).Total_Stake - Slash_Amount;
      end if;

      State.Validators (Validator_Idx).Total_Slashed :=
         State.Validators (Validator_Idx).Total_Slashed + Slash_Amount;
      State.Validators (Validator_Idx).Times_Slashed :=
         State.Validators (Validator_Idx).Times_Slashed + 1;
      State.Total_Stake := State.Total_Stake - Slash_Amount;

      Result := Slash_Result'(Anubis_Genesis_Validators.Slashed);
   end Slash_Downtime;

   procedure Jail_Validator (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Jail_Duration  : Unsigned_64
   ) is
   begin
      if State.Validators (Validator_Idx).Status = Active then
         State.Validators (Validator_Idx).Status := Jailed;
         State.Validators (Validator_Idx).Jail_End_Block :=
            State.Current_Block + Jail_Duration;

         if State.Active_Count > 0 then
            State.Active_Count := State.Active_Count - 1;
         end if;
      end if;
   end Jail_Validator;

   procedure Unjail_Validator (
      State          : in Out Validator_Set_State;
      Validator_Idx  : Validator_Index;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      if State.Validators (Validator_Idx).Status /= Jailed then
         Success := False;
         return;
      end if;

      if Current_Block < State.Validators (Validator_Idx).Jail_End_Block then
         Success := False;
         return;
      end if;

      State.Validators (Validator_Idx).Status := Active;
      State.Active_Count := State.Active_Count + 1;

      Success := True;
   end Unjail_Validator;

   ---------------------------------------------------------------------------
   --  Validator Queries
   ---------------------------------------------------------------------------

   function Get_Validator (
      State          : Validator_Set_State;
      Index          : Validator_Index
   ) return Validator_Record is
   begin
      return State.Validators (Index);
   end Get_Validator;

   procedure Find_Validator (
      State          : Validator_Set_State;
      Address        : Byte_Array;
      Index          : out Validator_Index;
      Found          : out Boolean
   ) is
   begin
      Index := 0;
      Found := False;

      for I in Validator_Index loop
         declare
            Match : Boolean := True;
         begin
            for J in Address'Range loop
               if J - Address'First <= State.Validators (I).Address'Last then
                  if Address (J) /=
                     State.Validators (I).Address (J - Address'First)
                  then
                     Match := False;
                     exit;
                  end if;
               end if;
            end loop;

            if Match and then State.Validators (I).Status /= Pending_Application then
               Index := I;
               Found := True;
               return;
            end if;
         end;
      end loop;
   end Find_Validator;

   function Get_Active_Count (
      State          : Validator_Set_State
   ) return Natural is
   begin
      return State.Active_Count;
   end Get_Active_Count;

   function Get_Total_Stake (
      State          : Validator_Set_State
   ) return Unsigned_64 is
   begin
      return State.Total_Stake;
   end Get_Total_Stake;

   function Calculate_Uptime (
      Validator      : Validator_Record
   ) return Natural is
      Total : Unsigned_64;
   begin
      Total := Validator.Blocks_Produced + Validator.Blocks_Missed;

      if Total = 0 then
         return 10000;  -- 100% if no blocks yet
      end if;

      return Natural ((Validator.Blocks_Produced * 10000) / Total);
   end Calculate_Uptime;

   function Meets_Uptime_Requirement (
      Validator      : Validator_Record
   ) return Boolean is
   begin
      return Calculate_Uptime (Validator) >= Min_Uptime_Percentage_BP;
   end Meets_Uptime_Requirement;

   ---------------------------------------------------------------------------
   --  Open Entry (Post-Genesis)
   ---------------------------------------------------------------------------

   procedure Enable_Open_Entry (
      State          : in Out Validator_Set_State;
      Current_Block  : Unsigned_64
   ) is
      pragma Unreferenced (Current_Block);
   begin
      State.Open_Entry_Active := True;
   end Enable_Open_Entry;

   function Is_Open_Entry (
      State          : Validator_Set_State;
      Current_Block  : Unsigned_64
   ) return Boolean is
   begin
      return State.Open_Entry_Active or else
             Current_Block >= Open_Entry_Block;
   end Is_Open_Entry;

   procedure Register_Open_Entry (
      State          : in Out Validator_Set_State;
      Address        : Byte_Array;
      Stake          : Unsigned_64;
      Consensus_PK   : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Register_Result
   ) is
      pragma Unreferenced (Signature);
      Slot : Validator_Index := 0;
      Found_Slot : Boolean := False;
   begin
      if not State.Open_Entry_Active then
         Result := Not_Approved;
         return;
      end if;

      if Stake < Min_Stake then
         Result := Insufficient_Stake;
         return;
      end if;

      if Stake > Max_Stake then
         Result := Stake_Cap_Exceeded;
         return;
      end if;

      --  Find empty slot
      for I in Validator_Index loop
         if State.Validators (I).Status = Pending_Application and then
            State.Validators (I).Registered_At = 0
         then
            Slot := I;
            Found_Slot := True;
            exit;
         end if;
      end loop;

      if not Found_Slot then
         Result := Not_Approved;
         return;
      end if;

      --  Copy address
      for I in Address'Range loop
         if I - Address'First <= State.Validators (Slot).Address'Last then
            State.Validators (Slot).Address (I - Address'First) := Address (I);
         end if;
      end loop;

      --  Copy consensus PK
      for I in Consensus_PK'Range loop
         if I - Consensus_PK'First <= State.Validators (Slot).Consensus_PK'Last then
            State.Validators (Slot).Consensus_PK (I - Consensus_PK'First) :=
               Consensus_PK (I);
         end if;
      end loop;

      State.Validators (Slot).Index := Slot;
      State.Validators (Slot).Status := Validator_Status'(Anubis_Genesis_Validators.Approved);
      State.Validators (Slot).Is_Genesis := False;
      State.Validators (Slot).Self_Stake := Stake;
      State.Validators (Slot).Total_Stake := Stake;
      State.Validators (Slot).Registered_At := State.Current_Block;

      State.Post_Genesis_Count := State.Post_Genesis_Count + 1;
      State.Total_Stake := State.Total_Stake + Stake;

      Result := Registered;
   end Register_Open_Entry;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Validator_Set (
      State          : Validator_Set_State;
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

      --  Write counts
      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Active_Count mod 256);
         Idx := Idx + 1;
      end if;

      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Genesis_Count mod 256);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Total_Stake);
      Write_U64 (State.Total_Distributed);
      Write_U64 (State.Remaining_Pool);
      Write_U64 (State.Current_Block_Reward);
      Write_U64 (State.Genesis_Block);
      Write_U64 (State.Current_Block);
      Write_U64 (State.Current_Epoch);

      if Idx <= Output'Last then
         Output (Idx) := (if State.Open_Entry_Active then 1 else 0);
         Idx := Idx + 1;
      end if;

      if Idx <= Output'Last then
         Output (Idx) := Unsigned_8 (State.Post_Genesis_Count mod 256);
         Idx := Idx + 1;
      end if;

      Length := Idx - Output'First;
   end Serialize_Validator_Set;

   procedure Deserialize_Validator_Set (
      Input          : Byte_Array;
      State          : out Validator_Set_State;
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
      Init_Validator_Set (State, 0);
      Success := False;

      if Input'Length < 80 then
         return;
      end if;

      if Idx <= Input'Last then
         State.Active_Count := Natural (Input (Idx));
         Idx := Idx + 1;
      end if;

      if Idx <= Input'Last then
         State.Genesis_Count := Natural (Input (Idx));
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Total_Stake);
      Read_U64 (State.Total_Distributed);
      Read_U64 (State.Remaining_Pool);
      Read_U64 (State.Current_Block_Reward);
      Read_U64 (State.Genesis_Block);
      Read_U64 (State.Current_Block);
      Read_U64 (State.Current_Epoch);

      if Idx <= Input'Last then
         State.Open_Entry_Active := Input (Idx) /= 0;
         Idx := Idx + 1;
      end if;

      if Idx <= Input'Last then
         State.Post_Genesis_Count := Natural (Input (Idx));
         Idx := Idx + 1;
      end if;

      Success := True;
   end Deserialize_Validator_Set;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Validator_Set (State : out Validator_Set_State) is
   begin
      for I in Validator_Index loop
         Zeroize_Validator (State.Validators (I));
      end loop;

      State.Active_Count := 0;
      State.Genesis_Count := 0;
      State.Total_Stake := 0;
      State.Total_Distributed := 0;
      State.Remaining_Pool := 0;
      State.Current_Block_Reward := 0;
      State.Genesis_Block := 0;
      State.Current_Block := 0;
      State.Current_Epoch := 0;
      State.Open_Entry_Active := False;
      State.Post_Genesis_Count := 0;
   end Zeroize_Validator_Set;

   procedure Zeroize_Validator (V : out Validator_Record) is
   begin
      V.Index := 0;
      V.Address := (others => 0);
      V.PK_Hash := (others => 0);
      V.Consensus_PK := (others => 0);
      V.Status := Pending_Application;
      V.Is_Genesis := False;
      V.Region := North_America;
      V.Self_Stake := 0;
      V.Delegated_Stake := 0;
      V.Total_Stake := 0;
      V.Blocks_Produced := 0;
      V.Blocks_Missed := 0;
      V.Total_Rewards := 0;
      V.Pending_Rewards := 0;
      V.Commission_Rate_BP := 0;
      V.Times_Slashed := 0;
      V.Total_Slashed := 0;
      V.Jail_End_Block := 0;
      V.Registered_At := 0;
      V.Active_Since := 0;
      V.Last_Block := 0;
      V.Unbonding_End := 0;
   end Zeroize_Validator;

   procedure Zeroize_Application (A : out Validator_Application) is
   begin
      A.Applicant := (others => 0);
      A.Status := Submitted;
      A.Region := North_America;
      A.Hardware := (
         CPU_Cores    => 0,
         RAM_GB       => 0,
         Storage_TB   => 0,
         Network_Mbps => 0,
         Is_Dedicated => False,
         Has_UPS      => False,
         Tier         => Minimum
      );
      A.Hardware_Proof_Hash := (others => 0);
      A.Uptime_Proof_Hash := (others => 0);
      A.Experience_Hash := (others => 0);
      A.IPFS_Application := (others => 0);
      A.Committed_Stake := 0;
      A.Stake_Source := (others => 0);
      A.Submitted_At := 0;
      A.Reviewed_At := 0;
      A.Reviewer := (others => 0);
      A.Review_Notes_Hash := (others => 0);
   end Zeroize_Application;

end Anubis_Genesis_Validators;
