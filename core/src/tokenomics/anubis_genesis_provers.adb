-------------------------------------------------------------------------------
--  ANUBIS Genesis Provers Implementation Body
--  8% Allocation (80M ANUBIS) for Proof Generation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Genesis_Provers with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Prover_Set (
      State          : out Prover_Set_State;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      --  Initialize all provers
      for I in Prover_Index loop
         State.Provers (I) := (
            Index             => I,
            Address           => (others => 0),
            PK_Hash           => (others => 0),
            Status            => Pending_Application,
            Tier              => Light,
            Is_Genesis        => False,
            Proofs_Generated  => 0,
            Proofs_Failed     => 0,
            Avg_Proof_Time_Ms => 0,
            Total_Rewards     => 0,
            Pending_Rewards   => 0,
            Stake_Amount      => 0,
            Times_Slashed     => 0,
            Total_Slashed     => 0,
            Registered_At     => 0,
            Active_Since      => 0,
            Last_Proof_Block  => 0
         );
      end loop;

      State.Active_Count := 0;
      State.Genesis_Count := 0;
      State.Total_Distributed := 0;
      State.Remaining_Pool := Prover_Allocation;
      State.Open_Entry_Active := False;
      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
   end Init_Prover_Set;

   ---------------------------------------------------------------------------
   --  Prover Registration
   ---------------------------------------------------------------------------

   procedure Register_Prover (
      State          : in Out Prover_Set_State;
      Address        : Byte_Array;
      PK_Hash        : Byte_Array;
      Tier           : Hardware_Tier;
      Stake          : Unsigned_64;
      Is_Genesis     : Boolean;
      Result         : out Register_Result
   ) is
      Slot : Prover_Index := 0;
      Found_Slot : Boolean := False;
   begin
      --  Find first available slot
      for I in Prover_Index loop
         if State.Provers (I).Status = Pending_Application and then
            State.Provers (I).Registered_At = 0
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

      --  Check genesis limit
      if Is_Genesis and then State.Genesis_Count >= Genesis_Prover_Count then
         Result := Not_Approved;
         return;
      end if;

      --  Register prover
      State.Provers (Slot).Index := Slot;

      --  Copy address
      for I in Address'Range loop
         if I - Address'First <= State.Provers (Slot).Address'Last then
            State.Provers (Slot).Address (I - Address'First) := Address (I);
         end if;
      end loop;

      --  Copy PK hash
      for I in PK_Hash'Range loop
         if I - PK_Hash'First <= State.Provers (Slot).PK_Hash'Last then
            State.Provers (Slot).PK_Hash (I - PK_Hash'First) := PK_Hash (I);
         end if;
      end loop;

      State.Provers (Slot).Status := Approved;
      State.Provers (Slot).Tier := Tier;
      State.Provers (Slot).Is_Genesis := Is_Genesis;
      State.Provers (Slot).Stake_Amount := Stake;
      State.Provers (Slot).Registered_At := State.Current_Block;

      if Is_Genesis then
         State.Genesis_Count := State.Genesis_Count + 1;
      end if;

      Result := Registered;
   end Register_Prover;

   ---------------------------------------------------------------------------
   --  Proof Recording
   ---------------------------------------------------------------------------

   procedure Record_Proof (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Proof_Size     : Unsigned_32;
      Proof_Time_Ms  : Unsigned_32;
      Block_Number   : Unsigned_64
   ) is
      pragma Unreferenced (Proof_Size);
   begin
      --  Update prover stats
      if State.Provers (Prover_Idx).Status = Active or else
         State.Provers (Prover_Idx).Status = Busy
      then
         State.Provers (Prover_Idx).Proofs_Generated :=
            State.Provers (Prover_Idx).Proofs_Generated + 1;

         --  Update average proof time (simple moving average)
         if State.Provers (Prover_Idx).Avg_Proof_Time_Ms = 0 then
            State.Provers (Prover_Idx).Avg_Proof_Time_Ms := Proof_Time_Ms;
         else
            State.Provers (Prover_Idx).Avg_Proof_Time_Ms :=
               (State.Provers (Prover_Idx).Avg_Proof_Time_Ms + Proof_Time_Ms) / 2;
         end if;

         State.Provers (Prover_Idx).Last_Proof_Block := Block_Number;
         State.Provers (Prover_Idx).Status := Active;
      end if;
   end Record_Proof;

   procedure Record_Proof_Failed (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Reason_Hash    : Byte_Array
   ) is
      pragma Unreferenced (Reason_Hash);
   begin
      if State.Provers (Prover_Idx).Status = Active or else
         State.Provers (Prover_Idx).Status = Busy
      then
         State.Provers (Prover_Idx).Proofs_Failed :=
            State.Provers (Prover_Idx).Proofs_Failed + 1;
      end if;
   end Record_Proof_Failed;

   ---------------------------------------------------------------------------
   --  Rewards
   ---------------------------------------------------------------------------

   function Calculate_Proof_Reward (
      State          : Prover_Set_State;
      Tier           : Hardware_Tier;
      Proof_Size     : Unsigned_32
   ) return Unsigned_64 is
      Base_Reward : Unsigned_64 := 100;  -- Base reward per proof
      Multiplier : constant Natural := Tier_Reward_Multiplier (Tier);
      Size_Bonus : Unsigned_64;
   begin
      --  Size bonus (larger proofs = more work)
      Size_Bonus := Unsigned_64 (Proof_Size / 1024);  -- 1 token per KB

      --  Apply tier multiplier
      Base_Reward := (Base_Reward * Unsigned_64 (Multiplier)) / 10_000;

      --  Cap based on remaining pool
      if Base_Reward + Size_Bonus > State.Remaining_Pool then
         return State.Remaining_Pool;
      end if;

      return Base_Reward + Size_Bonus;
   end Calculate_Proof_Reward;

   procedure Distribute_Proof_Reward (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Reward         : Unsigned_64
   ) is
   begin
      if Reward > State.Remaining_Pool then
         return;
      end if;

      State.Remaining_Pool := State.Remaining_Pool - Reward;
      State.Total_Distributed := State.Total_Distributed + Reward;
      State.Provers (Prover_Idx).Pending_Rewards :=
         State.Provers (Prover_Idx).Pending_Rewards + Reward;
   end Distribute_Proof_Reward;

   procedure Claim_Rewards (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Claimed        : out Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      Claimed := State.Provers (Prover_Idx).Pending_Rewards;

      if Claimed = 0 then
         Success := False;
         return;
      end if;

      State.Provers (Prover_Idx).Total_Rewards :=
         State.Provers (Prover_Idx).Total_Rewards + Claimed;
      State.Provers (Prover_Idx).Pending_Rewards := 0;

      Success := True;
   end Claim_Rewards;

   ---------------------------------------------------------------------------
   --  Slashing
   ---------------------------------------------------------------------------

   procedure Slash_Prover (
      State          : in Out Prover_Set_State;
      Prover_Idx     : Prover_Index;
      Slash_Rate_BP  : Natural;
      Reason_Hash    : Byte_Array
   ) is
      pragma Unreferenced (Reason_Hash);
      Slash_Amount : Unsigned_64;
   begin
      --  Calculate slash amount
      Slash_Amount := (State.Provers (Prover_Idx).Stake_Amount *
                       Unsigned_64 (Slash_Rate_BP)) / 10_000;

      --  Apply slash
      if Slash_Amount >= State.Provers (Prover_Idx).Stake_Amount then
         State.Provers (Prover_Idx).Stake_Amount := 0;
      else
         State.Provers (Prover_Idx).Stake_Amount :=
            State.Provers (Prover_Idx).Stake_Amount - Slash_Amount;
      end if;

      State.Provers (Prover_Idx).Times_Slashed :=
         State.Provers (Prover_Idx).Times_Slashed + 1;
      State.Provers (Prover_Idx).Total_Slashed :=
         State.Provers (Prover_Idx).Total_Slashed + Slash_Amount;

      --  If fully slashed, mark as slashed
      if State.Provers (Prover_Idx).Stake_Amount = 0 then
         State.Provers (Prover_Idx).Status := Slashed;
         if State.Active_Count > 0 then
            State.Active_Count := State.Active_Count - 1;
         end if;
      end if;
   end Slash_Prover;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Prover (
      State          : Prover_Set_State;
      Index          : Prover_Index
   ) return Prover_Record is
   begin
      return State.Provers (Index);
   end Get_Prover;

   function Get_Active_Count (
      State          : Prover_Set_State
   ) return Natural is
   begin
      return State.Active_Count;
   end Get_Active_Count;

   ---------------------------------------------------------------------------
   --  Open Entry
   ---------------------------------------------------------------------------

   procedure Enable_Open_Entry (
      State          : in Out Prover_Set_State;
      Current_Block  : Unsigned_64
   ) is
      pragma Unreferenced (Current_Block);
   begin
      State.Open_Entry_Active := True;
   end Enable_Open_Entry;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Prover_Set (State : out Prover_Set_State) is
   begin
      for I in Prover_Index loop
         Zeroize_Prover (State.Provers (I));
      end loop;

      State.Active_Count := 0;
      State.Genesis_Count := 0;
      State.Total_Distributed := 0;
      State.Remaining_Pool := 0;
      State.Open_Entry_Active := False;
      State.Genesis_Block := 0;
      State.Current_Block := 0;
   end Zeroize_Prover_Set;

   procedure Zeroize_Prover (P : out Prover_Record) is
   begin
      P.Index := 0;
      P.Address := (others => 0);
      P.PK_Hash := (others => 0);
      P.Status := Pending_Application;
      P.Tier := Light;
      P.Is_Genesis := False;
      P.Proofs_Generated := 0;
      P.Proofs_Failed := 0;
      P.Avg_Proof_Time_Ms := 0;
      P.Total_Rewards := 0;
      P.Pending_Rewards := 0;
      P.Stake_Amount := 0;
      P.Times_Slashed := 0;
      P.Total_Slashed := 0;
      P.Registered_At := 0;
      P.Active_Since := 0;
      P.Last_Proof_Block := 0;
   end Zeroize_Prover;

end Anubis_Genesis_Provers;
