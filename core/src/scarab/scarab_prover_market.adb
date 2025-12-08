-------------------------------------------------------------------------------
--  SCARAB - Decentralized Prover Market Implementation
--  Economic Layer for ZK Proof Generation Network
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Anubis_SHA3;

package body Scarab_Prover_Market with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Hash_Request_ID (
      Requester    : Byte_Array;
      Circuit_Hash : Byte_Array;
      Witness_Hash : Byte_Array;
      Timestamp    : Unsigned_64
   ) return Byte_Array is
      Input : Byte_Array (0 .. 103);
      Output : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Input (I) := Requester (I);
         Input (32 + I) := Circuit_Hash (I);
         Input (64 + I) := Witness_Hash (I);
      end loop;
      Input (96) := Unsigned_8 (Timestamp and 16#FF#);
      Input (97) := Unsigned_8 (Shift_Right (Timestamp, 8) and 16#FF#);
      Input (98) := Unsigned_8 (Shift_Right (Timestamp, 16) and 16#FF#);
      Input (99) := Unsigned_8 (Shift_Right (Timestamp, 24) and 16#FF#);
      Input (100) := Unsigned_8 (Shift_Right (Timestamp, 32) and 16#FF#);
      Input (101) := Unsigned_8 (Shift_Right (Timestamp, 40) and 16#FF#);
      Input (102) := Unsigned_8 (Shift_Right (Timestamp, 48) and 16#FF#);
      Input (103) := Unsigned_8 (Shift_Right (Timestamp, 56) and 16#FF#);
      Anubis_SHA3.SHA3_256 (Input, Output);
      return Output;
   end Hash_Request_ID;

   ---------------------------------------------------------------------------
   --  Prover Registration
   ---------------------------------------------------------------------------

   procedure Register_Prover (
      Market         : in Out Market_State;
      Address        : Byte_Array;
      PK_Hash        : Byte_Array;
      Stake_Amount   : Unsigned_64;
      Hardware       : Hardware_Type;
      Region         : Geographic_Region;
      Capacity       : Natural;
      Index          : out Prover_Index;
      Success        : out Boolean
   ) is
   begin
      Index := Prover_Index (Market.Num_Provers);

      Market.Provers (Index) := (
         Index          => Index,
         Address        => Address,
         PK_Hash        => PK_Hash,
         Stake_Amount   => Stake_Amount,
         Locked_Until   => 0,
         Status         => Active,
         Last_Active    => 0,
         Registered_At  => 0,
         Hardware       => Hardware,
         Region         => Region,
         Capacity       => Capacity,
         Total_Proofs   => 0,
         Failed_Proofs  => 0,
         Avg_Time_Ms    => 0,
         Quality_Score  => 8000,  -- Start at 80%
         Reputation     => 5000   -- Start at 50%
      );

      Market.Num_Provers := Market.Num_Provers + 1;
      Market.Total_Stake := Market.Total_Stake + Stake_Amount;
      Market.Region_Counts (Region) := Market.Region_Counts (Region) + 1;
      Market.Hardware_Counts (Hardware) := Market.Hardware_Counts (Hardware) + 1;
      Success := True;
   end Register_Prover;

   procedure Increase_Stake (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Additional     : Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      Market.Provers (Prover_Idx).Stake_Amount :=
         Market.Provers (Prover_Idx).Stake_Amount + Additional;
      Market.Total_Stake := Market.Total_Stake + Additional;
      Success := True;
   end Increase_Stake;

   procedure Request_Withdrawal (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : Unsigned_64;
      Unlock_Time    : out Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      if Amount > Market.Provers (Prover_Idx).Stake_Amount then
         Unlock_Time := 0;
         Success := False;
         return;
      end if;

      --  7 day unlock period (in milliseconds)
      Unlock_Time := Market.Provers (Prover_Idx).Last_Active + 604_800_000;
      Market.Provers (Prover_Idx).Locked_Until := Unlock_Time;
      Success := True;
   end Request_Withdrawal;

   procedure Update_Prover_Status (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      New_Status     : Prover_Status
   ) is
   begin
      Market.Provers (Prover_Idx).Status := New_Status;
   end Update_Prover_Status;

   procedure Update_Prover_Info (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Hardware       : Hardware_Type;
      Region         : Geographic_Region;
      Capacity       : Natural
   ) is
      Old_Hardware : constant Hardware_Type :=
         Market.Provers (Prover_Idx).Hardware;
      Old_Region : constant Geographic_Region :=
         Market.Provers (Prover_Idx).Region;
   begin
      --  Update counts
      Market.Hardware_Counts (Old_Hardware) :=
         Market.Hardware_Counts (Old_Hardware) - 1;
      Market.Region_Counts (Old_Region) :=
         Market.Region_Counts (Old_Region) - 1;

      Market.Provers (Prover_Idx).Hardware := Hardware;
      Market.Provers (Prover_Idx).Region := Region;
      Market.Provers (Prover_Idx).Capacity := Capacity;

      Market.Hardware_Counts (Hardware) :=
         Market.Hardware_Counts (Hardware) + 1;
      Market.Region_Counts (Region) :=
         Market.Region_Counts (Region) + 1;
   end Update_Prover_Info;

   ---------------------------------------------------------------------------
   --  Request Management
   ---------------------------------------------------------------------------

   procedure Submit_Request (
      Market         : in Out Market_State;
      Requester      : Byte_Array;
      Circuit_Hash   : Byte_Array;
      Witness_Hash   : Byte_Array;
      Complexity     : Proof_Complexity;
      Priority       : Request_Priority;
      Max_Fee        : Unsigned_64;
      Deadline       : Unsigned_64;
      Request_ID     : out Byte_Array;
      Success        : out Boolean
   ) is
      Idx : constant Request_Index := Request_Index (Market.Num_Pending);
      ID : constant Byte_Array := Hash_Request_ID (
         Requester, Circuit_Hash, Witness_Hash, Deadline);
   begin
      for I in Request_ID'Range loop
         Request_ID (I) := ID (I - Request_ID'First);
      end loop;

      Market.Requests (Idx) := (
         Request_ID     => ID,
         Requester      => Requester,
         Circuit_Hash   => Circuit_Hash,
         Witness_Hash   => Witness_Hash,
         Complexity     => Complexity,
         Priority       => Priority,
         Max_Fee        => Max_Fee,
         Current_Price  => Max_Fee,
         Final_Price    => 0,
         Submitted_At   => 0,
         Deadline       => Deadline,
         Auction_Ends   => 0,
         Assigned_Prover=> 0,
         Assigned       => False,
         Completed      => False,
         Success        => False
      );

      Market.Num_Pending := Market.Num_Pending + 1;
      Market.Total_Requests := Market.Total_Requests + 1;
      Success := True;
   end Submit_Request;

   procedure Cancel_Request (
      Market         : in Out Market_State;
      Request_ID     : Byte_Array;
      Requester      : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Requester);
   begin
      for I in 0 .. Market.Num_Pending - 1 loop
         pragma Loop_Invariant (I < Market.Num_Pending);
         if Market.Requests (I).Request_ID = Request_ID then
            if not Market.Requests (I).Assigned then
               --  Shift remaining requests
               for J in I .. Market.Num_Pending - 2 loop
                  pragma Loop_Invariant (J >= I and J < Market.Num_Pending - 1);
                  Market.Requests (J) := Market.Requests (J + 1);
               end loop;
               Market.Num_Pending := Market.Num_Pending - 1;
               Success := True;
               return;
            end if;
         end if;
      end loop;
      Success := False;
   end Cancel_Request;

   procedure Get_Request_Status (
      Market         : Market_State;
      Request_ID     : Byte_Array;
      Request        : out Proof_Request;
      Found          : out Boolean
   ) is
   begin
      for I in 0 .. Market.Num_Pending - 1 loop
         pragma Loop_Invariant (I < Market.Num_Pending);
         if Market.Requests (I).Request_ID = Request_ID then
            Request := Market.Requests (I);
            Found := True;
            return;
         end if;
      end loop;

      Request := (
         Request_ID     => (others => 0),
         Requester      => (others => 0),
         Circuit_Hash   => (others => 0),
         Witness_Hash   => (others => 0),
         Complexity     => Tier_1_Simple,
         Priority       => Normal,
         Max_Fee        => 0,
         Current_Price  => 0,
         Final_Price    => 0,
         Submitted_At   => 0,
         Deadline       => 0,
         Auction_Ends   => 0,
         Assigned_Prover=> 0,
         Assigned       => False,
         Completed      => False,
         Success        => False
      );
      Found := False;
   end Get_Request_Status;

   ---------------------------------------------------------------------------
   --  Auction Mechanism
   ---------------------------------------------------------------------------

   procedure Start_Auction (
      Market         : in Out Market_State;
      Request_Idx    : Request_Index;
      Start_Price    : Unsigned_64;
      Reserve_Price  : Unsigned_64;
      Auction        : out Auction_State
   ) is
      pragma Unreferenced (Market);
   begin
      Auction := (
         Request_ID    => (others => 0),
         Start_Price   => Start_Price,
         Current_Price => Start_Price,
         Reserve_Price => Reserve_Price,
         Started_At    => 0,
         Ends_At       => Unsigned_64 (Auction_Duration_Ms),
         Winner        => 0,
         Has_Winner    => False,
         Finalized     => False
      );

      --  Copy request ID
      for I in Auction.Request_ID'Range loop
         Auction.Request_ID (I) :=
            Market.Requests (Request_Idx).Request_ID (I);
      end loop;
   end Start_Auction;

   procedure Place_Bid (
      Market         : in Out Market_State;
      Auction        : in Out Auction_State;
      Prover_Idx     : Prover_Index;
      Bid_Price      : Unsigned_64;
      Current_Time   : Unsigned_64;
      Result         : out Bid_Result
   ) is
   begin
      --  Check auction hasn't ended
      if Current_Time > Auction.Ends_At then
         Result := Auction_Ended;
         return;
      end if;

      --  Check prover status
      if Market.Provers (Prover_Idx).Status /= Active then
         Result := Prover_Suspended;
         return;
      end if;

      --  Check stake
      if Market.Provers (Prover_Idx).Stake_Amount < Min_Prover_Stake then
         Result := Insufficient_Stake;
         return;
      end if;

      --  Check bid is at or below current price
      if Bid_Price > Auction.Current_Price then
         Result := Invalid_Bid;
         return;
      end if;

      --  Accept bid
      Auction.Winner := Prover_Idx;
      Auction.Has_Winner := True;
      Auction.Current_Price := Bid_Price;
      Result := Accepted;
   end Place_Bid;

   procedure Update_Auction_Price (
      Auction        : in Out Auction_State;
      Current_Time   : Unsigned_64
   ) is
      Elapsed : Unsigned_64;
      Price_Drop : Unsigned_64;
   begin
      if Current_Time > Auction.Started_At then
         Elapsed := Current_Time - Auction.Started_At;
      else
         Elapsed := 0;
      end if;

      --  Calculate price decay
      if Elapsed > 0 and Auction.Start_Price > Auction.Reserve_Price then
         Price_Drop := ((Auction.Start_Price - Auction.Reserve_Price) * Elapsed) /
            Unsigned_64 (Auction_Duration_Ms);

         if Auction.Start_Price - Price_Drop > Auction.Reserve_Price then
            Auction.Current_Price := Auction.Start_Price - Price_Drop;
         else
            Auction.Current_Price := Auction.Reserve_Price;
         end if;
      end if;
   end Update_Auction_Price;

   procedure Finalize_Auction (
      Market         : in Out Market_State;
      Auction        : in Out Auction_State;
      Result         : out Assignment_Result
   ) is
   begin
      Auction.Finalized := True;

      if not Auction.Has_Winner then
         Result := No_Available_Prover;
         return;
      end if;

      --  Find and assign request
      for I in 0 .. Market.Num_Pending - 1 loop
         pragma Loop_Invariant (I < Market.Num_Pending);
         if Market.Requests (I).Request_ID = Auction.Request_ID then
            Market.Requests (I).Assigned_Prover := Auction.Winner;
            Market.Requests (I).Assigned := True;
            Market.Requests (I).Final_Price := Auction.Current_Price;
            Market.Provers (Auction.Winner).Status := Busy;
            Result := Assigned;
            return;
         end if;
      end loop;

      Result := Request_Expired;
   end Finalize_Auction;

   function Current_Auction_Price (
      Auction        : Auction_State;
      Current_Time   : Unsigned_64
   ) return Unsigned_64 is
      Elapsed : Unsigned_64;
      Price_Drop : Unsigned_64;
   begin
      if Auction.Finalized then
         return Auction.Current_Price;
      end if;

      if Current_Time > Auction.Started_At then
         Elapsed := Current_Time - Auction.Started_At;
      else
         return Auction.Start_Price;
      end if;

      if Elapsed >= Unsigned_64 (Auction_Duration_Ms) then
         return Auction.Reserve_Price;
      end if;

      if Auction.Start_Price > Auction.Reserve_Price then
         Price_Drop := ((Auction.Start_Price - Auction.Reserve_Price) * Elapsed) /
            Unsigned_64 (Auction_Duration_Ms);

         if Auction.Start_Price - Price_Drop > Auction.Reserve_Price then
            return Auction.Start_Price - Price_Drop;
         else
            return Auction.Reserve_Price;
         end if;
      end if;

      return Auction.Current_Price;
   end Current_Auction_Price;

   ---------------------------------------------------------------------------
   --  Assignment and Completion
   ---------------------------------------------------------------------------

   procedure Direct_Assign (
      Market         : in Out Market_State;
      Request_Idx    : Request_Index;
      Prover_Idx     : Prover_Index;
      Price          : Unsigned_64;
      Result         : out Assignment_Result
   ) is
   begin
      if Market.Requests (Request_Idx).Assigned then
         Result := Already_Assigned;
         return;
      end if;

      if Market.Provers (Prover_Idx).Status /= Active then
         Result := No_Available_Prover;
         return;
      end if;

      Market.Requests (Request_Idx).Assigned_Prover := Prover_Idx;
      Market.Requests (Request_Idx).Assigned := True;
      Market.Requests (Request_Idx).Final_Price := Price;
      Market.Provers (Prover_Idx).Status := Busy;
      Result := Assigned;
   end Direct_Assign;

   procedure Find_Best_Prover (
      Market         : Market_State;
      Complexity     : Proof_Complexity;
      Prover_Idx     : out Prover_Index;
      Found          : out Boolean
   ) is
      Best_Score : Natural := 0;
      Best_Idx : Prover_Index := 0;
      Score : Natural;
      pragma Unreferenced (Complexity);
   begin
      Found := False;
      Prover_Idx := 0;

      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         if Market.Provers (Prover_Index (I)).Status = Active then
            Score := Market.Provers (Prover_Index (I)).Quality_Score +
                     Market.Provers (Prover_Index (I)).Reputation;
            if Score > Best_Score then
               Best_Score := Score;
               Best_Idx := Prover_Index (I);
               Found := True;
            end if;
         end if;
      end loop;

      Prover_Idx := Best_Idx;
   end Find_Best_Prover;

   procedure Submit_Completion (
      Market         : in Out Market_State;
      Request_ID     : Byte_Array;
      Prover_Idx     : Prover_Index;
      Proof_Hash     : Byte_Array;
      Proof_Valid    : Boolean;
      Completion_Time: Unsigned_64;
      Result         : out Completion_Result
   ) is
      pragma Unreferenced (Proof_Hash, Completion_Time);
   begin
      for I in 0 .. Market.Num_Pending - 1 loop
         pragma Loop_Invariant (I < Market.Num_Pending);
         if Market.Requests (I).Request_ID = Request_ID then
            if Market.Requests (I).Assigned_Prover /= Prover_Idx then
               Result := Wrong_Prover;
               return;
            end if;

            Market.Requests (I).Completed := True;
            Market.Requests (I).Success := Proof_Valid;
            Market.Provers (Prover_Idx).Status := Active;
            Market.Provers (Prover_Idx).Total_Proofs :=
               Market.Provers (Prover_Idx).Total_Proofs + 1;

            if Proof_Valid then
               Market.Total_Completed := Market.Total_Completed + 1;
               Market.Total_Fees := Market.Total_Fees +
                  Market.Requests (I).Final_Price;
               Result := Verified;
            else
               Market.Provers (Prover_Idx).Failed_Proofs :=
                  Market.Provers (Prover_Idx).Failed_Proofs + 1;
               Market.Total_Failed := Market.Total_Failed + 1;
               Result := Invalid_Proof;
            end if;
            return;
         end if;
      end loop;

      Result := Request_Not_Found;
   end Submit_Completion;

   procedure Handle_Timeout (
      Market         : in Out Market_State;
      Request_Idx    : Request_Index;
      Current_Time   : Unsigned_64
   ) is
      Prover_Idx : Prover_Index;
      Slash_Amt : Unsigned_64;
   begin
      if Market.Requests (Request_Idx).Assigned and
         Current_Time > Market.Requests (Request_Idx).Deadline
      then
         Prover_Idx := Market.Requests (Request_Idx).Assigned_Prover;
         Slash_Timeout (Market, Prover_Idx, Slash_Amt);
         Market.Requests (Request_Idx).Assigned := False;
         Market.Provers (Prover_Idx).Status := Active;
      end if;
   end Handle_Timeout;

   ---------------------------------------------------------------------------
   --  Slashing and Rewards
   ---------------------------------------------------------------------------

   procedure Slash_Invalid (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : out Unsigned_64
   ) is
   begin
      Amount := (Market.Provers (Prover_Idx).Stake_Amount *
         Unsigned_64 (Slash_Invalid_Proof)) / 10_000;
      Market.Provers (Prover_Idx).Stake_Amount :=
         Market.Provers (Prover_Idx).Stake_Amount - Amount;
      Market.Total_Stake := Market.Total_Stake - Amount;
      Market.Provers (Prover_Idx).Status := Banned;
   end Slash_Invalid;

   procedure Slash_Timeout (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : out Unsigned_64
   ) is
   begin
      Amount := (Market.Provers (Prover_Idx).Stake_Amount *
         Unsigned_64 (Slash_Missed_Deadline)) / 10_000;
      Market.Provers (Prover_Idx).Stake_Amount :=
         Market.Provers (Prover_Idx).Stake_Amount - Amount;
      Market.Total_Stake := Market.Total_Stake - Amount;
      Market.Provers (Prover_Idx).Status := Suspended;
   end Slash_Timeout;

   procedure Distribute_Reward (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index;
      Amount         : Unsigned_64
   ) is
   begin
      Market.Provers (Prover_Idx).Stake_Amount :=
         Market.Provers (Prover_Idx).Stake_Amount + Amount;
      Market.Total_Stake := Market.Total_Stake + Amount;
   end Distribute_Reward;

   function Calculate_Earnings (
      Prover         : Prover_Info;
      Period_Start   : Unsigned_64;
      Period_End     : Unsigned_64
   ) return Unsigned_64 is
      pragma Unreferenced (Period_Start, Period_End);
   begin
      --  Simplified: return stake amount as proxy for earnings potential
      return Prover.Stake_Amount;
   end Calculate_Earnings;

   ---------------------------------------------------------------------------
   --  Quality and Reputation
   ---------------------------------------------------------------------------

   procedure Update_Quality_Score (
      Market         : in Out Market_State;
      Prover_Idx     : Prover_Index
   ) is
      P : Prover_Info renames Market.Provers (Prover_Idx);
      Success_Rate : Natural;
   begin
      if P.Total_Proofs > 0 then
         Success_Rate := Natural (
            ((P.Total_Proofs - P.Failed_Proofs) * 10_000) / P.Total_Proofs);
         P.Quality_Score := Success_Rate;
      end if;
   end Update_Quality_Score;

   function Calculate_Reputation (
      Prover         : Prover_Info
   ) return Natural is
      Base_Rep : Natural := 5000;
   begin
      --  Increase for high success rate
      if Prover.Quality_Score > 9500 then
         Base_Rep := Base_Rep + 2000;
      elsif Prover.Quality_Score > 9000 then
         Base_Rep := Base_Rep + 1000;
      end if;

      --  Increase for volume
      if Prover.Total_Proofs > 10000 then
         Base_Rep := Base_Rep + 2000;
      elsif Prover.Total_Proofs > 1000 then
         Base_Rep := Base_Rep + 1000;
      end if;

      if Base_Rep > 10000 then
         return 10000;
      else
         return Base_Rep;
      end if;
   end Calculate_Reputation;

   function Meets_Quality_Threshold (
      Prover         : Prover_Info
   ) return Boolean is
   begin
      return Prover.Quality_Score >= Natural (Min_Success_Rate) * 100;
   end Meets_Quality_Threshold;

   function Get_Prover_Rank (
      Market         : Market_State;
      Prover_Idx     : Prover_Index
   ) return Natural is
      Rank : Natural := 1;
      My_Score : constant Natural :=
         Market.Provers (Prover_Idx).Quality_Score +
         Market.Provers (Prover_Idx).Reputation;
   begin
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         if Prover_Index (I) /= Prover_Idx then
            if Market.Provers (Prover_Index (I)).Quality_Score +
               Market.Provers (Prover_Index (I)).Reputation > My_Score
            then
               Rank := Rank + 1;
            end if;
         end if;
      end loop;
      return Rank;
   end Get_Prover_Rank;

   ---------------------------------------------------------------------------
   --  Diversity Management
   ---------------------------------------------------------------------------

   function Check_Regional_Diversity (
      Market         : Market_State
   ) return Boolean is
      Max_In_Region : Natural := 0;
      Threshold : Natural;
   begin
      if Market.Num_Provers = 0 then
         return True;
      end if;

      for R in Geographic_Region loop
         if Market.Region_Counts (R) > Max_In_Region then
            Max_In_Region := Market.Region_Counts (R);
         end if;
      end loop;

      Threshold := (Market.Num_Provers * Max_Single_Region_Pct) / 100;
      return Max_In_Region <= Threshold;
   end Check_Regional_Diversity;

   function Check_Hardware_Diversity (
      Market         : Market_State
   ) return Boolean is
      Max_Of_Type : Natural := 0;
      Threshold : Natural;
   begin
      if Market.Num_Provers = 0 then
         return True;
      end if;

      for H in Hardware_Type loop
         if Market.Hardware_Counts (H) > Max_Of_Type then
            Max_Of_Type := Market.Hardware_Counts (H);
         end if;
      end loop;

      Threshold := (Market.Num_Provers * Max_Single_Hardware_Pct) / 100;
      return Max_Of_Type <= Threshold;
   end Check_Hardware_Diversity;

   function Get_Diversity_Score (
      Market         : Market_State
   ) return Natural is
      Region_Score : Natural := 0;
      Hardware_Score : Natural := 0;
      Active_Regions : Natural := 0;
      Active_Hardware : Natural := 0;
   begin
      for R in Geographic_Region loop
         if Market.Region_Counts (R) > 0 then
            Active_Regions := Active_Regions + 1;
         end if;
      end loop;

      for H in Hardware_Type loop
         if Market.Hardware_Counts (H) > 0 then
            Active_Hardware := Active_Hardware + 1;
         end if;
      end loop;

      Region_Score := (Active_Regions * 100) / 7;  -- 7 regions
      Hardware_Score := (Active_Hardware * 100) / 7;  -- 7 hardware types

      return (Region_Score + Hardware_Score) / 2;
   end Get_Diversity_Score;

   function Apply_Diversity_Bonus (
      Market         : Market_State;
      Prover_Idx     : Prover_Index;
      Base_Score     : Natural
   ) return Natural is
      Bonus : Natural := 0;
      P : Prover_Info renames Market.Provers (Prover_Idx);
   begin
      --  Bonus for underrepresented regions
      if Market.Num_Provers > 0 then
         if Market.Region_Counts (P.Region) * 100 / Market.Num_Provers < 10 then
            Bonus := Bonus + 500;
         end if;

         if Market.Hardware_Counts (P.Hardware) * 100 / Market.Num_Provers < 10 then
            Bonus := Bonus + 500;
         end if;
      end if;

      return Base_Score + Bonus;
   end Apply_Diversity_Bonus;

   function Would_Improve_Diversity (
      Market         : Market_State;
      Region         : Geographic_Region;
      Hardware       : Hardware_Type
   ) return Boolean is
   begin
      if Market.Num_Provers = 0 then
         return True;
      end if;

      return Market.Region_Counts (Region) * 100 / Market.Num_Provers < 20 or
             Market.Hardware_Counts (Hardware) * 100 / Market.Num_Provers < 20;
   end Would_Improve_Diversity;

   ---------------------------------------------------------------------------
   --  Pricing
   ---------------------------------------------------------------------------

   function Estimate_Price (
      Market         : Market_State;
      Complexity     : Proof_Complexity;
      Priority       : Request_Priority
   ) return Unsigned_64 is
      Base : Unsigned_64;
   begin
      Base := Dynamic_Base_Price (Market);
      return Base * Unsigned_64 (Complexity_Multiplier (Complexity)) *
             Unsigned_64 (Priority_Multiplier (Priority)) / 100;
   end Estimate_Price;

   function Complexity_Multiplier (
      Complexity     : Proof_Complexity
   ) return Natural is
   begin
      case Complexity is
         when Tier_1_Simple   => return 10;
         when Tier_2_Standard => return 25;
         when Tier_3_Complex  => return 50;
         when Tier_4_Heavy    => return 100;
      end case;
   end Complexity_Multiplier;

   function Priority_Multiplier (
      Priority       : Request_Priority
   ) return Natural is
   begin
      case Priority is
         when Low    => return 50;
         when Normal => return 100;
         when High   => return 200;
         when Urgent => return 500;
      end case;
   end Priority_Multiplier;

   function Dynamic_Base_Price (
      Market         : Market_State
   ) return Unsigned_64 is
      Active_Count : Natural := 0;
      Utilization : Natural;
   begin
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         if Market.Provers (Prover_Index (I)).Status = Active then
            Active_Count := Active_Count + 1;
         end if;
      end loop;

      if Active_Count = 0 then
         return 1_000_000;  -- High price when no provers
      end if;

      Utilization := (Market.Num_Pending * 100) / Active_Count;

      if Utilization > 80 then
         return 500_000;  -- High demand
      elsif Utilization > 50 then
         return 200_000;  -- Medium demand
      else
         return 100_000;  -- Low demand
      end if;
   end Dynamic_Base_Price;

   ---------------------------------------------------------------------------
   --  Emergency Procedures
   ---------------------------------------------------------------------------

   procedure Emergency_Pause (
      Market         : in Out Market_State;
      Authority_Sig  : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Authority_Sig);
   begin
      --  Suspend all provers
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         Market.Provers (Prover_Index (I)).Status := Suspended;
      end loop;
      Success := True;
   end Emergency_Pause;

   procedure Activate_Lite_Mode (
      Market         : in Out Market_State
   ) is
   begin
      --  Only keep top provers active
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         if Market.Provers (Prover_Index (I)).Quality_Score < 9000 then
            Market.Provers (Prover_Index (I)).Status := Suspended;
         end if;
      end loop;
   end Activate_Lite_Mode;

   procedure Enable_Validator_Fallback (
      Market         : in Out Market_State
   ) is
   begin
      --  Mark all as suspended to force fallback
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         Market.Provers (Prover_Index (I)).Status := Offline;
      end loop;
   end Enable_Validator_Fallback;

   function Check_Network_Health (
      Market         : Market_State
   ) return Boolean is
      Active_Count : Natural := 0;
   begin
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         if Market.Provers (Prover_Index (I)).Status = Active then
            Active_Count := Active_Count + 1;
         end if;
      end loop;

      --  Healthy if at least 10% of provers are active
      return Active_Count * 10 >= Market.Num_Provers;
   end Check_Network_Health;

   ---------------------------------------------------------------------------
   --  Statistics and Metrics
   ---------------------------------------------------------------------------

   function Get_Metrics (
      Market         : Market_State
   ) return Market_Metrics is
      Active_Count : Natural := 0;
      Avg_Price : Unsigned_64 := 0;
      Avg_Time : Unsigned_64 := 0;
      Total_Time : Unsigned_64 := 0;
      Success_Rate : Natural;
   begin
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         if Market.Provers (Prover_Index (I)).Status = Active then
            Active_Count := Active_Count + 1;
            Total_Time := Total_Time +
               Market.Provers (Prover_Index (I)).Avg_Time_Ms;
         end if;
      end loop;

      if Active_Count > 0 then
         Avg_Time := Total_Time / Unsigned_64 (Active_Count);
      end if;

      if Market.Total_Completed > 0 then
         Avg_Price := Market.Total_Fees / Market.Total_Completed;
      end if;

      if Market.Total_Requests > 0 then
         Success_Rate := Natural ((Market.Total_Completed * 100) /
            Market.Total_Requests);
      else
         Success_Rate := 100;
      end if;

      return (
         Active_Provers   => Active_Count,
         Pending_Requests => Market.Num_Pending,
         Avg_Price        => Avg_Price,
         Avg_Time_Ms      => Avg_Time,
         Success_Rate     => Success_Rate,
         Utilization      => Get_Utilization (Market),
         Diversity_Score  => Get_Diversity_Score (Market)
      );
   end Get_Metrics;

   procedure Get_Prover_Stats (
      Market         : Market_State;
      Prover_Idx     : Prover_Index;
      Total_Proofs   : out Unsigned_64;
      Success_Rate   : out Natural;
      Avg_Time       : out Unsigned_64;
      Total_Earned   : out Unsigned_64
   ) is
      P : Prover_Info renames Market.Provers (Prover_Idx);
   begin
      Total_Proofs := P.Total_Proofs;
      Avg_Time := P.Avg_Time_Ms;

      if P.Total_Proofs > 0 then
         Success_Rate := Natural (
            ((P.Total_Proofs - P.Failed_Proofs) * 100) / P.Total_Proofs);
      else
         Success_Rate := 100;
      end if;

      --  Earnings is approximated from stake changes
      Total_Earned := P.Stake_Amount;
   end Get_Prover_Stats;

   function Get_Utilization (
      Market         : Market_State
   ) return Natural is
      Active_Count : Natural := 0;
      Total_Capacity : Natural := 0;
   begin
      for I in 0 .. Market.Num_Provers - 1 loop
         pragma Loop_Invariant (I < Market.Num_Provers);
         if Market.Provers (Prover_Index (I)).Status = Active then
            Active_Count := Active_Count + 1;
            Total_Capacity := Total_Capacity +
               Market.Provers (Prover_Index (I)).Capacity;
         end if;
      end loop;

      if Total_Capacity = 0 then
         return 0;
      end if;

      return (Market.Num_Pending * 100) / Total_Capacity;
   end Get_Utilization;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Market (
      Market         : Market_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
   begin
      Output := (others => 0);
      Length := 0;

      --  Write number of provers
      Output (0) := Unsigned_8 (Market.Num_Provers mod 256);
      Output (1) := Unsigned_8 ((Market.Num_Provers / 256) mod 256);
      Length := 2;

      --  Write number of pending requests
      Output (2) := Unsigned_8 (Market.Num_Pending mod 256);
      Output (3) := Unsigned_8 ((Market.Num_Pending / 256) mod 256);
      Length := 4;
   end Serialize_Market;

   procedure Deserialize_Market (
      Input          : Byte_Array;
      Market         : out Market_State;
      Success        : out Boolean
   ) is
   begin
      Market := (
         Provers        => (others => (
            Index          => 0,
            Address        => (others => 0),
            PK_Hash        => (others => 0),
            Stake_Amount   => 0,
            Locked_Until   => 0,
            Status         => Offline,
            Last_Active    => 0,
            Registered_At  => 0,
            Hardware       => Unknown,
            Region         => Unknown_Region,
            Capacity       => 0,
            Total_Proofs   => 0,
            Failed_Proofs  => 0,
            Avg_Time_Ms    => 0,
            Quality_Score  => 0,
            Reputation     => 0
         )),
         Num_Provers    => 0,
         Total_Stake    => 0,
         Requests       => (others => (
            Request_ID     => (others => 0),
            Requester      => (others => 0),
            Circuit_Hash   => (others => 0),
            Witness_Hash   => (others => 0),
            Complexity     => Tier_1_Simple,
            Priority       => Normal,
            Max_Fee        => 0,
            Current_Price  => 0,
            Final_Price    => 0,
            Submitted_At   => 0,
            Deadline       => 0,
            Auction_Ends   => 0,
            Assigned_Prover=> 0,
            Assigned       => False,
            Completed      => False,
            Success        => False
         )),
         Num_Pending    => 0,
         Total_Requests => 0,
         Total_Completed=> 0,
         Total_Failed   => 0,
         Total_Fees     => 0,
         Region_Counts  => (others => 0),
         Hardware_Counts=> (others => 0)
      );

      if Input'Length < 4 then
         Success := False;
         return;
      end if;

      Market.Num_Provers := Natural (Input (Input'First)) +
         Natural (Input (Input'First + 1)) * 256;
      Market.Num_Pending := Natural (Input (Input'First + 2)) +
         Natural (Input (Input'First + 3)) * 256;
      Success := True;
   end Deserialize_Market;

   procedure Serialize_Prover (
      Prover         : Prover_Info;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
   begin
      Output := (others => 0);

      --  Write address
      for I in 0 .. 31 loop
         Output (I) := Prover.Address (I);
      end loop;

      --  Write PK hash
      for I in 0 .. 31 loop
         Output (32 + I) := Prover.PK_Hash (I);
      end loop;

      Length := 64;
   end Serialize_Prover;

end Scarab_Prover_Market;
