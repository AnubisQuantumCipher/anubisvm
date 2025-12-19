-------------------------------------------------------------------------------
--  ANUBIS Milestone-Based Vesting System Body
--  Solo Builder 30% Allocation with Verified Milestone Unlocks
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

package body Anubis_Vesting with
   SPARK_Mode => On
is
   --  Track last update block for slowdown calculations
   Last_Update_Block : Unsigned_64 := 0;
   NFT_Counter : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Vesting (
      State          : out Vesting_State;
      Builder_Addr   : Byte_Array
   ) is
   begin
      --  Initialize builder address
      State.Builder_Address := (others => 0);
      for I in Builder_Addr'Range loop
         if I - Builder_Addr'First <= State.Builder_Address'Last then
            State.Builder_Address (I - Builder_Addr'First) := Builder_Addr (I);
         end if;
      end loop;

      --  Initialize all milestones
      for M in Milestone_ID loop
         State.Milestones (M) := (
            ID => M,
            Status => Pending,
            Allocation => Milestone_Amounts (M),
            Unlocked_At => 0,
            Vested_Amount => 0,
            Released_Amount => 0,
            Commit_Hash => (others => 0),
            Proof_Hash => (others => 0),
            Audit_Hash => (others => 0),
            Oracle_Attestation => (others => 0),
            Verifier_PK => (others => 0)
         );
      end loop;

      State.Total_Unlocked := 0;
      State.Total_Vested := 0;
      State.Total_Released := 0;
      State.Current_Block := 0;
      State.Initialized := True;
   end Init_Vesting;

   ---------------------------------------------------------------------------
   --  Milestone Submission
   ---------------------------------------------------------------------------

   procedure Submit_Milestone (
      State          : in Out Vesting_State;
      Submission     : Milestone_Submission;
      Result         : out Submit_Result
   ) is
      M : Milestone_ID renames Submission.Milestone;
   begin
      --  Check if milestone is pending
      if State.Milestones (M).Status /= Pending then
         Result := Already_Submitted;
         return;
      end if;

      --  Check previous milestones are complete (except for milestone 1)
      if M > 1 and then not Previous_Milestones_Complete (State, M) then
         Result := Previous_Incomplete;
         return;
      end if;

      --  Record submission data
      State.Milestones (M).Commit_Hash := Submission.Commit_Hash;
      State.Milestones (M).Proof_Hash := Submission.Proof_Hash;
      State.Milestones (M).Status := Submitted;

      Result := Accepted;
   end Submit_Milestone;

   procedure Verify_Milestone (
      State          : in Out Vesting_State;
      Attestation    : Oracle_Attestation;
      Auditor_Sig    : Byte_Array;
      Result         : out Verify_Result
   ) is
      M : Milestone_ID renames Attestation.Milestone;
      pragma Unreferenced (Auditor_Sig);
   begin
      --  Check milestone is in submitted/verifying state
      if State.Milestones (M).Status /= Submitted and then
         State.Milestones (M).Status /= Verifying
      then
         Result := Verification_Failed;
         return;
      end if;

      --  Check oracle verification result
      if not Attestation.Verification_Result then
         Result := Oracle_Rejected;
         return;
      end if;

      --  Record attestation
      for I in Attestation.Oracle_Sig'Range loop
         if I - Attestation.Oracle_Sig'First <=
            State.Milestones (M).Oracle_Attestation'Last
         then
            State.Milestones (M).Oracle_Attestation
               (I - Attestation.Oracle_Sig'First) :=
               Attestation.Oracle_Sig (I);
         end if;
      end loop;

      --  Unlock milestone
      State.Milestones (M).Status := Unlocked;
      State.Milestones (M).Unlocked_At := Attestation.Block_Number;
      State.Total_Unlocked := State.Total_Unlocked +
         State.Milestones (M).Allocation;

      Result := Verified;
   end Verify_Milestone;

   function Previous_Milestones_Complete (
      State          : Vesting_State;
      Milestone      : Milestone_ID
   ) return Boolean is
   begin
      if Milestone = 1 then
         return True;
      end if;

      for M in 1 .. Milestone - 1 loop
         if State.Milestones (M).Status /= Unlocked and then
            State.Milestones (M).Status /= Vesting and then
            State.Milestones (M).Status /= Fully_Vested
         then
            return False;
         end if;
      end loop;

      return True;
   end Previous_Milestones_Complete;

   ---------------------------------------------------------------------------
   --  Vesting Calculations
   ---------------------------------------------------------------------------

   function Calculate_Vested (
      Milestone      : Milestone_Record;
      Current_Block  : Unsigned_64
   ) return Unsigned_64 is
      Blocks_Since_Unlock : Unsigned_64;
      Vested : Unsigned_64;
   begin
      --  Not unlocked yet
      if Milestone.Status /= Unlocked and then
         Milestone.Status /= Vesting and then
         Milestone.Status /= Fully_Vested
      then
         return 0;
      end if;

      --  Already fully vested
      if Milestone.Status = Fully_Vested then
         return Milestone.Allocation;
      end if;

      --  Calculate linear vesting
      if Current_Block <= Milestone.Unlocked_At then
         return 0;
      end if;

      Blocks_Since_Unlock := Current_Block - Milestone.Unlocked_At;

      --  Cap at vesting period
      if Blocks_Since_Unlock >= Vesting_Period_Blocks then
         return Milestone.Allocation;
      end if;

      --  Linear vesting: allocation * blocks_since / vesting_period
      Vested := (Milestone.Allocation * Blocks_Since_Unlock) /
                Vesting_Period_Blocks;

      --  Cap at allocation
      if Vested > Milestone.Allocation then
         return Milestone.Allocation;
      end if;

      return Vested;
   end Calculate_Vested;

   function Total_Vested_Amount (
      State          : Vesting_State
   ) return Unsigned_64 is
      Total : Unsigned_64 := 0;
   begin
      for M in Milestone_ID loop
         Total := Total + Calculate_Vested (State.Milestones (M),
                                            State.Current_Block);
      end loop;

      --  Cap at total allocation
      if Total > Total_Builder_Allocation then
         return Total_Builder_Allocation;
      end if;

      return Total;
   end Total_Vested_Amount;

   function Claimable_Amount (
      State          : Vesting_State
   ) return Unsigned_64 is
      Total_Vested : constant Unsigned_64 := Total_Vested_Amount (State);
   begin
      if Total_Vested <= State.Total_Released then
         return 0;
      end if;

      return Total_Vested - State.Total_Released;
   end Claimable_Amount;

   procedure Update_Vesting (
      State          : in Out Vesting_State;
      Current_Block  : Unsigned_64
   ) is
   begin
      State.Current_Block := Current_Block;

      --  Update milestone statuses
      for M in Milestone_ID loop
         if State.Milestones (M).Status = Unlocked then
            --  Check if vesting has started
            if Current_Block > State.Milestones (M).Unlocked_At then
               State.Milestones (M).Status := Vesting;
            end if;
         elsif State.Milestones (M).Status = Vesting then
            --  Check if fully vested
            if Current_Block >= State.Milestones (M).Unlocked_At +
                               Vesting_Period_Blocks
            then
               State.Milestones (M).Status := Fully_Vested;
               State.Milestones (M).Vested_Amount :=
                  State.Milestones (M).Allocation;
            else
               State.Milestones (M).Vested_Amount :=
                  Calculate_Vested (State.Milestones (M), Current_Block);
            end if;
         end if;
      end loop;

      --  Update total vested
      State.Total_Vested := Total_Vested_Amount (State);
   end Update_Vesting;

   ---------------------------------------------------------------------------
   --  Token Release
   ---------------------------------------------------------------------------

   procedure Release_Vested (
      State          : in Out Vesting_State;
      Amount         : Unsigned_64;
      Recipient      : Byte_Array;
      Result         : out Release_Result
   ) is
      pragma Unreferenced (Recipient);
      Claimable : constant Unsigned_64 := Claimable_Amount (State);
   begin
      --  Check there"s something to claim
      if Claimable = 0 then
         Result := Nothing_Vested;
         return;
      end if;

      --  Check amount doesn"t exceed claimable
      if Amount > Claimable then
         Result := Nothing_Vested;
         return;
      end if;

      --  Release tokens
      State.Total_Released := State.Total_Released + Amount;

      --  Update milestone released amounts
      declare
         Remaining : Unsigned_64 := Amount;
      begin
         for M in Milestone_ID loop
            if Remaining = 0 then
               exit;
            end if;

            declare
               Milestone_Vested : constant Unsigned_64 :=
                  Calculate_Vested (State.Milestones (M), State.Current_Block);
               Milestone_Available : Unsigned_64;
            begin
               if Milestone_Vested > State.Milestones (M).Released_Amount then
                  Milestone_Available :=
                     Milestone_Vested - State.Milestones (M).Released_Amount;

                  if Remaining <= Milestone_Available then
                     State.Milestones (M).Released_Amount :=
                        State.Milestones (M).Released_Amount + Remaining;
                     Remaining := 0;
                  else
                     State.Milestones (M).Released_Amount := Milestone_Vested;
                     Remaining := Remaining - Milestone_Available;
                  end if;
               end if;
            end;
         end loop;
      end;

      Result := Released;
   end Release_Vested;

   procedure Release_All_Vested (
      State          : in Out Vesting_State;
      Recipient      : Byte_Array;
      Released       : out Unsigned_64;
      Result         : out Release_Result
   ) is
      Claimable : constant Unsigned_64 := Claimable_Amount (State);
   begin
      Released := 0;

      if Claimable = 0 then
         Result := Nothing_Vested;
         return;
      end if;

      Release_Vested (State, Claimable, Recipient, Result);

      if Result = Anubis_Vesting.Released then
         Released := Claimable;
      end if;
   end Release_All_Vested;

   ---------------------------------------------------------------------------
   --  Milestone Queries
   ---------------------------------------------------------------------------

   function Get_Milestone_Status (
      State          : Vesting_State;
      Milestone      : Milestone_ID
   ) return Milestone_Status is
   begin
      return State.Milestones (Milestone).Status;
   end Get_Milestone_Status;

   function Get_Milestone_Allocation (
      Milestone      : Milestone_ID
   ) return Unsigned_64 is
   begin
      return Milestone_Amounts (Milestone);
   end Get_Milestone_Allocation;

   function Get_Vesting_Progress (
      State          : Vesting_State;
      Milestone      : Milestone_ID
   ) return Natural is
      Vested : constant Unsigned_64 :=
         Calculate_Vested (State.Milestones (Milestone), State.Current_Block);
      Alloc : constant Unsigned_64 := State.Milestones (Milestone).Allocation;
   begin
      if Alloc = 0 then
         return 0;
      end if;

      return Natural ((Vested * 100) / Alloc);
   end Get_Vesting_Progress;

   function Get_Overall_Progress (
      State          : Vesting_State
   ) return Natural is
      Vested : constant Unsigned_64 := Total_Vested_Amount (State);
   begin
      return Natural ((Vested * 100) / Total_Builder_Allocation);
   end Get_Overall_Progress;

   function Completed_Milestones (
      State          : Vesting_State
   ) return Natural is
      Count : Natural := 0;
   begin
      for M in Milestone_ID loop
         if State.Milestones (M).Status = Fully_Vested then
            Count := Count + 1;
         end if;
      end loop;

      return Count;
   end Completed_Milestones;

   ---------------------------------------------------------------------------
   --  Verification Helpers
   ---------------------------------------------------------------------------

   function Verify_GNATprove_Hash (
      Expected       : Byte_Array;
      Proof_Output   : Byte_Array
   ) return Boolean is
      Hash : Byte_Array (0 .. 31) := (others => 0);
   begin
      --  Hash the proof output with SHA3-256
      if Proof_Output'Length > 0 and Proof_Output'Length <= 1_000_000 then
         declare
            Local_Proof : constant Byte_Array (0 .. Proof_Output'Length - 1) :=
               Proof_Output;
         begin
            Anubis_SHA3.SHA3_256 (Local_Proof, Hash);
         end;
      else
         return False;
      end if;

      --  Constant-time comparison of hash with expected
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in 0 .. 31 loop
            if I <= Expected'Last - Expected'First then
               Diff := Diff or (Hash (I) xor Expected (Expected'First + I));
            else
               Diff := Diff or Hash (I);
            end if;
         end loop;
         return Diff = 0;
      end;
   end Verify_GNATprove_Hash;

   function Verify_Audit_Signature (
      Milestone      : Milestone_ID;
      Audit_Report   : Byte_Array;
      Auditor_PK     : Byte_Array;
      Signature      : Byte_Array
   ) return Boolean is
      --  Message format: milestone_id (1 byte) || audit_report_hash
      Message : Byte_Array (0 .. 32) := (others => 0);
      PK : Anubis_MLDSA_Types.Public_Key := (others => 0);
      Sig : Anubis_MLDSA_Types.Signature := (others => 0);
   begin
      --  Validate input sizes
      if Auditor_PK'Length /= 2592 or Signature'Length /= 4627 then
         return False;
      end if;

      --  Build message: milestone ID + SHA3-256 hash of audit report
      Message (0) := Unsigned_8 (Milestone);
      if Audit_Report'Length > 0 and Audit_Report'Length <= 1_000_000 then
         declare
            Local_Report : constant Byte_Array (0 .. Audit_Report'Length - 1) :=
               Audit_Report;
            Report_Hash : Byte_Array (0 .. 31) := (others => 0);
         begin
            Anubis_SHA3.SHA3_256 (Local_Report, Report_Hash);
            Message (1 .. 32) := Report_Hash;
         end;
      else
         return False;
      end if;

      --  Copy public key and signature
      for I in PK'Range loop
         PK (I) := Auditor_PK (Auditor_PK'First + I);
      end loop;

      for I in Sig'Range loop
         Sig (I) := Signature (Signature'First + I);
      end loop;

      --  Verify ML-DSA-87 signature
      return Anubis_MLDSA.Verify (PK, Message, Sig);
   end Verify_Audit_Signature;

   function Verify_Oracle_Attestation (
      Attestation    : Oracle_Attestation;
      Oracle_PK      : Byte_Array
   ) return Boolean is
      --  Message format: milestone_id (1) || timestamp (8) || block_number (8) ||
      --                  result (1) || details_hash (32) = 50 bytes
      Message : Byte_Array (0 .. 49) := (others => 0);
      PK : Anubis_MLDSA_Types.Public_Key := (others => 0);
      Sig : Anubis_MLDSA_Types.Signature := (others => 0);
   begin
      --  Validate input size
      if Oracle_PK'Length /= 2592 then
         return False;
      end if;

      --  Build message for signature verification
      Message (0) := Unsigned_8 (Attestation.Milestone);

      --  Pack timestamp (big-endian)
      for I in 0 .. 7 loop
         Message (1 + I) := Unsigned_8 (
            Shift_Right (Attestation.Timestamp, (7 - I) * 8) and 16#FF#);
      end loop;

      --  Pack block number (big-endian)
      for I in 0 .. 7 loop
         Message (9 + I) := Unsigned_8 (
            Shift_Right (Attestation.Block_Number, (7 - I) * 8) and 16#FF#);
      end loop;

      --  Pack result
      Message (17) := (if Attestation.Verification_Result then 1 else 0);

      --  Pack details hash
      for I in 0 .. 31 loop
         Message (18 + I) := Attestation.Details_Hash (I);
      end loop;

      --  Copy public key
      for I in PK'Range loop
         PK (I) := Oracle_PK (Oracle_PK'First + I);
      end loop;

      --  Copy signature (from Oracle_Sig which is 128 bytes, take first 4627)
      --  Note: Oracle_Sig should be at least 4627 bytes for ML-DSA-87
      if Attestation.Oracle_Sig'Length < 4627 then
         return False;
      end if;

      for I in Sig'Range loop
         if I <= Attestation.Oracle_Sig'Last - Attestation.Oracle_Sig'First then
            Sig (I) := Attestation.Oracle_Sig (Attestation.Oracle_Sig'First + I);
         end if;
      end loop;

      --  Verify ML-DSA-87 signature
      return Anubis_MLDSA.Verify (PK, Message, Sig);
   end Verify_Oracle_Attestation;

   ---------------------------------------------------------------------------
   --  Public Build Protocol Integration
   ---------------------------------------------------------------------------

   procedure Record_Weekly_Update (
      State          : in Out Vesting_State;
      Update         : Weekly_Update;
      Success        : out Boolean
   ) is
      --  Message format for signature: week_num (4) || commits (4) || lines (4) ||
      --                                progress (1) || blockers (32) || ipfs (46) ||
      --                                timestamp (8) = 99 bytes
      Message : Byte_Array (0 .. 98) := (others => 0);
      Sig : Anubis_MLDSA_Types.Signature := (others => 0);
      Blockers_Hash : Byte_Array (0 .. 31) := (others => 0);
   begin
      Success := False;

      --  Build message for verification
      --  Week number (4 bytes, big-endian)
      for I in 0 .. 3 loop
         Message (I) := Unsigned_8 (
            Shift_Right (Update.Week_Number, (3 - I) * 8) and 16#FF#);
      end loop;

      --  Commits count (4 bytes, big-endian)
      for I in 0 .. 3 loop
         Message (4 + I) := Unsigned_8 (
            Shift_Right (Update.Commits_Count, (3 - I) * 8) and 16#FF#);
      end loop;

      --  Lines verified (4 bytes, big-endian)
      for I in 0 .. 3 loop
         Message (8 + I) := Unsigned_8 (
            Shift_Right (Update.Lines_Verified, (3 - I) * 8) and 16#FF#);
      end loop;

      --  Progress (1 byte)
      Message (12) := Unsigned_8 (Update.Milestone_Progress);

      --  Blockers hash (32 bytes) - hash the blocker description
      Anubis_SHA3.SHA3_256 (Update.Blockers, Blockers_Hash);
      Message (13 .. 44) := Blockers_Hash;

      --  IPFS link (46 bytes)
      Message (45 .. 90) := Update.IPFS_Link;

      --  Timestamp (8 bytes, big-endian)
      for I in 0 .. 7 loop
         Message (91 + I) := Unsigned_8 (
            Shift_Right (Update.Timestamp, (7 - I) * 8) and 16#FF#);
      end loop;

      --  Copy signature
      if Update.Builder_Sig'Length >= 4627 then
         for I in Sig'Range loop
            Sig (I) := Update.Builder_Sig (Update.Builder_Sig'First + I);
         end loop;

         --  Convert builder address to public key for verification
         --  In production, builder PK would be stored in state
         declare
            Builder_PK : Anubis_MLDSA_Types.Public_Key := (others => 0);
         begin
            --  Derive PK from builder address (first 2592 bytes if stored)
            --  For now, assume builder address IS or derives from PK
            for I in 0 .. Natural'Min (31, 2591) loop
               if I <= State.Builder_Address'Last then
                  Builder_PK (I) := State.Builder_Address (I);
               end if;
            end loop;

            --  Verify signature
            if Anubis_MLDSA.Verify (Builder_PK, Message, Sig) then
               Last_Update_Block := Update.Timestamp;
               Success := True;
            end if;
         end;
      end if;
   end Record_Weekly_Update;

   function Get_Update_Status (
      State          : Vesting_State;
      Current_Block  : Unsigned_64
   ) return Update_Status is
      pragma Unreferenced (State);
      Blocks_Since_Update : Unsigned_64;
      Weeks_Missed : Unsigned_64;
   begin
      if Last_Update_Block = 0 then
         return On_Track;  --  No updates recorded yet
      end if;

      if Current_Block <= Last_Update_Block then
         return On_Track;
      end if;

      Blocks_Since_Update := Current_Block - Last_Update_Block;

      --  1 week = ~100,800 blocks (6 sec blocks)
      Weeks_Missed := Blocks_Since_Update / 100_800;

      if Weeks_Missed = 0 then
         return On_Track;
      elsif Weeks_Missed = 1 then
         return Warning;
      elsif Weeks_Missed <= 3 then
         return Slowdown;
      elsif Weeks_Missed <= 7 then
         return Paused;
      else
         return Dead_Man_Warning;
      end if;
   end Get_Update_Status;

   function Get_Vesting_Rate (
      State          : Vesting_State;
      Current_Block  : Unsigned_64
   ) return Natural is
      Status : constant Update_Status :=
         Get_Update_Status (State, Current_Block);
   begin
      case Status is
         when On_Track => return 100;
         when Warning  => return 100;
         when Slowdown => return 90;   --  10% slower
         when Paused   => return 0;    --  Vesting paused
         when Dead_Man_Warning => return 0;
      end case;
   end Get_Vesting_Rate;

   ---------------------------------------------------------------------------
   --  Proof-of-Build NFT Integration
   ---------------------------------------------------------------------------

   procedure Mint_Proof_Of_Build (
      Milestone      : Milestone_ID;
      Commit_Hash    : Byte_Array;
      Proof_Hash     : Byte_Array;
      Lines_Added    : Unsigned_32;
      NFT            : out Proof_Of_Build;
      Token_ID       : out Unsigned_64
   ) is
   begin
      NFT_Counter := NFT_Counter + 1;
      Token_ID := NFT_Counter;

      NFT.Token_ID := Token_ID;
      NFT.Milestone := Milestone;

      --  Copy commit hash
      NFT.Commit_Hash := (others => 0);
      for I in Commit_Hash'Range loop
         if I - Commit_Hash'First <= NFT.Commit_Hash'Last then
            NFT.Commit_Hash (I - Commit_Hash'First) := Commit_Hash (I);
         end if;
      end loop;

      --  Copy proof hash
      NFT.Proof_Hash := (others => 0);
      for I in Proof_Hash'Range loop
         if I - Proof_Hash'First <= NFT.Proof_Hash'Last then
            NFT.Proof_Hash (I - Proof_Hash'First) := Proof_Hash (I);
         end if;
      end loop;

      NFT.Lines_Added := Lines_Added;
      --  Set timestamp from system time (blocks * average block time)
      --  Assuming this is called with access to block height, estimate Unix time
      NFT.Timestamp := Unsigned_64 (Last_Update_Block);
      NFT.IPFS_Report := (others => 0);
   end Mint_Proof_Of_Build;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Vesting_State (
      State          : Vesting_State;
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

      --  Builder address
      for I in State.Builder_Address'Range loop
         if Idx <= Output'Last then
            Output (Idx) := State.Builder_Address (I);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Totals
      Write_U64 (State.Total_Unlocked);
      Write_U64 (State.Total_Vested);
      Write_U64 (State.Total_Released);
      Write_U64 (State.Current_Block);

      --  Initialized flag
      if Idx <= Output'Last then
         Output (Idx) := (if State.Initialized then 1 else 0);
         Idx := Idx + 1;
      end if;

      --  Milestones (simplified - just status and key fields)
      for M in Milestone_ID loop
         if Idx <= Output'Last then
            Output (Idx) := Milestone_Status'Pos (State.Milestones (M).Status);
            Idx := Idx + 1;
         end if;
         Write_U64 (State.Milestones (M).Unlocked_At);
         Write_U64 (State.Milestones (M).Vested_Amount);
         Write_U64 (State.Milestones (M).Released_Amount);
      end loop;

      Length := Idx - Output'First;
   end Serialize_Vesting_State;

   procedure Deserialize_Vesting_State (
      Input          : Byte_Array;
      State          : out Vesting_State;
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
      --  Initialize output
      Init_Vesting (State, (0 .. 31 => 0));
      Success := False;

      if Input'Length < 100 then
         return;
      end if;

      --  Builder address
      for I in State.Builder_Address'Range loop
         if Idx <= Input'Last then
            State.Builder_Address (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Totals
      Read_U64 (State.Total_Unlocked);
      Read_U64 (State.Total_Vested);
      Read_U64 (State.Total_Released);
      Read_U64 (State.Current_Block);

      --  Initialized flag
      if Idx <= Input'Last then
         State.Initialized := Input (Idx) /= 0;
         Idx := Idx + 1;
      end if;

      --  Milestones
      for M in Milestone_ID loop
         if Idx <= Input'Last then
            if Natural (Input (Idx)) <=
               Milestone_Status'Pos (Milestone_Status'Last)
            then
               State.Milestones (M).Status :=
                  Milestone_Status'Val (Natural (Input (Idx)));
            end if;
            Idx := Idx + 1;
         end if;
         Read_U64 (State.Milestones (M).Unlocked_At);
         Read_U64 (State.Milestones (M).Vested_Amount);
         Read_U64 (State.Milestones (M).Released_Amount);
      end loop;

      Success := True;
   end Deserialize_Vesting_State;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Vesting_State (State : out Vesting_State) is
   begin
      State.Builder_Address := (others => 0);
      for M in Milestone_ID loop
         Zeroize_Milestone (State.Milestones (M));
      end loop;
      State.Total_Unlocked := 0;
      State.Total_Vested := 0;
      State.Total_Released := 0;
      State.Current_Block := 0;
      State.Initialized := False;
   end Zeroize_Vesting_State;

   procedure Zeroize_Milestone (Milestone : out Milestone_Record) is
   begin
      Milestone.ID := 1;
      Milestone.Status := Pending;
      Milestone.Allocation := 0;
      Milestone.Unlocked_At := 0;
      Milestone.Vested_Amount := 0;
      Milestone.Released_Amount := 0;
      Milestone.Commit_Hash := (others => 0);
      Milestone.Proof_Hash := (others => 0);
      Milestone.Audit_Hash := (others => 0);
      Milestone.Oracle_Attestation := (others => 0);
      Milestone.Verifier_PK := (others => 0);
   end Zeroize_Milestone;

   procedure Zeroize_Submission (Submission : out Milestone_Submission) is
   begin
      Submission.Milestone := 1;
      Submission.Commit_Hash := (others => 0);
      Submission.Proof_Hash := (others => 0);
      Submission.Lines_Verified := 0;
      Submission.Test_Coverage := 0;
      Submission.IPFS_Link := (others => 0);
      Submission.Builder_Sig := (others => 0);
   end Zeroize_Submission;

end Anubis_Vesting;
