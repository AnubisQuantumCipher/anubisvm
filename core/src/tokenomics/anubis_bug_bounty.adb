-------------------------------------------------------------------------------
--  ANUBIS Bug Bounty System Implementation Body
--  5% Allocation (50M ANUBIS) for Security Rewards
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Bug_Bounty with
   SPARK_Mode => On
is
   --  Report counter
   Report_Counter : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Bug_Bounty (
      State          : out Bounty_State;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      State.Total_Allocation := Bounty_Allocation;
      State.Remaining_Pool := Bounty_Allocation;
      State.Total_Paid := 0;
      State.Total_Reports := 0;
      State.Reports_Confirmed := 0;
      State.Reports_Rejected := 0;
      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
   end Init_Bug_Bounty;

   ---------------------------------------------------------------------------
   --  Report Submission
   ---------------------------------------------------------------------------

   procedure Submit_Report (
      State          : in Out Bounty_State;
      Reporter       : Byte_Array;
      Category       : Bug_Category;
      Title_Hash     : Byte_Array;
      Description    : Byte_Array;
      POC_Hash       : Byte_Array;
      Report         : out Bug_Report;
      Result         : out Submit_Result
   ) is
   begin
      --  Check pool not exhausted
      if State.Remaining_Pool = 0 then
         Result := Pool_Exhausted;
         Report := (
            ID => 0,
            Reporter => (others => 0),
            Category => Minor_Bug,
            Severity => Low,
            Status => Rejected,
            Title_Hash => (others => 0),
            Description_Hash => (others => 0),
            POC_Hash => (others => 0),
            IPFS_Report => (others => 0),
            Assessed_Severity => Low,
            Final_Reward => 0,
            Fix_Commit => (others => 0),
            Submitted_At => 0,
            Confirmed_At => 0,
            Fixed_At => 0,
            Paid_At => 0
         );
         return;
      end if;

      --  Create report
      Report_Counter := Report_Counter + 1;

      Report.ID := Report_Counter;

      --  Copy reporter
      Report.Reporter := (others => 0);
      for I in Reporter'Range loop
         if I - Reporter'First <= Report.Reporter'Last then
            Report.Reporter (I - Reporter'First) := Reporter (I);
         end if;
      end loop;

      Report.Category := Category;
      Report.Severity := Category_Severity (Category);
      Report.Status := Submitted;

      --  Copy title hash
      Report.Title_Hash := (others => 0);
      for I in Title_Hash'Range loop
         if I - Title_Hash'First <= Report.Title_Hash'Last then
            Report.Title_Hash (I - Title_Hash'First) := Title_Hash (I);
         end if;
      end loop;

      --  Copy description hash
      Report.Description_Hash := (others => 0);
      for I in Description'Range loop
         if I - Description'First <= Report.Description_Hash'Last then
            Report.Description_Hash (I - Description'First) := Description (I);
         end if;
      end loop;

      --  Copy POC hash
      Report.POC_Hash := (others => 0);
      for I in POC_Hash'Range loop
         if I - POC_Hash'First <= Report.POC_Hash'Last then
            Report.POC_Hash (I - POC_Hash'First) := POC_Hash (I);
         end if;
      end loop;

      Report.IPFS_Report := (others => 0);
      Report.Assessed_Severity := Report.Severity;
      Report.Final_Reward := 0;
      Report.Fix_Commit := (others => 0);
      Report.Submitted_At := State.Current_Block;
      Report.Confirmed_At := 0;
      Report.Fixed_At := 0;
      Report.Paid_At := 0;

      State.Total_Reports := State.Total_Reports + 1;

      Result := Submitted;
   end Submit_Report;

   ---------------------------------------------------------------------------
   --  Assessment
   ---------------------------------------------------------------------------

   procedure Assess_Report (
      Report         : in Out Bug_Report;
      Assessor       : Byte_Array;
      Confirmed      : Boolean;
      Final_Severity : Severity_Level;
      Reward         : Unsigned_64;
      Result         : out Assess_Result
   ) is
      pragma Unreferenced (Assessor);
   begin
      --  Check report is in submitted/triaging state
      if Report.Status /= Submitted and then Report.Status /= Triaging then
         Result := Invalid_Report;
         return;
      end if;

      --  Check not already assessed
      if Report.Status = Anubis_Bug_Bounty.Confirmed or else Report.Status = Fixed then
         Result := Already_Assessed;
         return;
      end if;

      if Confirmed then
         Report.Status := Anubis_Bug_Bounty.Confirmed;
         Report.Assessed_Severity := Final_Severity;
         Report.Final_Reward := Reward;
      else
         Report.Status := Rejected;
         Report.Final_Reward := 0;
      end if;

      Result := Assessed;
   end Assess_Report;

   function Calculate_Reward (
      Category       : Bug_Category;
      Impact_Score   : Natural;
      Quality_Score  : Natural
   ) return Unsigned_64 is
      Severity : constant Severity_Level := Category_Severity (Category);
      Range_Rec : constant Reward_Range := Severity_Rewards (Severity);
      Score_Avg : Natural;
      Reward : Unsigned_64;
   begin
      --  Average of impact and quality
      Score_Avg := (Impact_Score + Quality_Score) / 2;

      --  Calculate reward: min + (range * score / 100)
      Reward := Range_Rec.Min_Reward +
         ((Range_Rec.Max_Reward - Range_Rec.Min_Reward) *
          Unsigned_64 (Score_Avg)) / 100;

      --  Cap at max
      if Reward > Range_Rec.Max_Reward then
         Reward := Range_Rec.Max_Reward;
      end if;

      return Reward;
   end Calculate_Reward;

   ---------------------------------------------------------------------------
   --  Payout
   ---------------------------------------------------------------------------

   procedure Process_Payout (
      State          : in Out Bounty_State;
      Report         : in Out Bug_Report;
      Result         : out Payout_Result
   ) is
   begin
      --  Check report is confirmed or fixed
      if Report.Status /= Confirmed and then Report.Status /= Fixed then
         Result := Not_Confirmed;
         return;
      end if;

      --  Check not already paid
      if Report.Paid_At /= 0 then
         Result := Already_Paid;
         return;
      end if;

      --  Check sufficient pool
      if Report.Final_Reward > State.Remaining_Pool then
         Result := Insufficient_Pool;
         return;
      end if;

      --  Process payout
      State.Remaining_Pool := State.Remaining_Pool - Report.Final_Reward;
      State.Total_Paid := State.Total_Paid + Report.Final_Reward;
      State.Reports_Confirmed := State.Reports_Confirmed + 1;

      Report.Status := Paid;
      Report.Paid_At := State.Current_Block;

      Result := Paid;
   end Process_Payout;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Remaining_Pool (State : Bounty_State) return Unsigned_64 is
   begin
      return State.Remaining_Pool;
   end Get_Remaining_Pool;

   function Get_Reward_Range (Severity : Severity_Level) return Reward_Range is
   begin
      return Severity_Rewards (Severity);
   end Get_Reward_Range;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Bounty_State (State : out Bounty_State) is
   begin
      State.Total_Allocation := 0;
      State.Remaining_Pool := 0;
      State.Total_Paid := 0;
      State.Total_Reports := 0;
      State.Reports_Confirmed := 0;
      State.Reports_Rejected := 0;
      State.Genesis_Block := 0;
      State.Current_Block := 0;
   end Zeroize_Bounty_State;

   procedure Zeroize_Report (R : out Bug_Report) is
   begin
      R.ID := 0;
      R.Reporter := (others => 0);
      R.Category := Minor_Bug;
      R.Severity := Low;
      R.Status := Rejected;
      R.Title_Hash := (others => 0);
      R.Description_Hash := (others => 0);
      R.POC_Hash := (others => 0);
      R.IPFS_Report := (others => 0);
      R.Assessed_Severity := Low;
      R.Final_Reward := 0;
      R.Fix_Commit := (others => 0);
      R.Submitted_At := 0;
      R.Confirmed_At := 0;
      R.Fixed_At := 0;
      R.Paid_At := 0;
   end Zeroize_Report;

end Anubis_Bug_Bounty;
