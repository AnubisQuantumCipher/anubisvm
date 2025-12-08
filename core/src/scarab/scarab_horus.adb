-------------------------------------------------------------------------------
--  SCARAB - HORUS Pipeline Prover (Implementation)
--  Pipelined parallel proving for 6-second finality
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3;
with Anubis_MLDSA;

package body Scarab_Horus with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   --  Prover registry (would be on-chain in production)
   type Prover_Registry is array (0 .. 255) of Prover_Info;
   Provers : Prover_Registry := (others => (
      Address => (others => 0),
      Stake => 0,
      Tier => Light,
      Jobs_Completed => 0,
      Reputation => 500,
      Active => False
   ));
   Num_Provers : Natural := 0;

   --  Job queue
   type Job_Queue is array (0 .. 63) of Proof_Job;
   Jobs : Job_Queue := (others => (
      Job_ID => (others => 0),
      Batch => (
         Transactions => (others => (
            From_Addr => (others => 0),
            To_Addr => (others => 0),
            Value => 0,
            Nonce => 0,
            Gas_Limit => 0,
            Gas_Price => 0,
            Data => (others => 0),
            Data_Length => 0,
            Signature => (others => 0),
            Mode => Standard
         )),
         Count => 0,
         Block_Height => 0,
         Batch_Hash => (others => 0)
      ),
      Deadline => 0,
      Reward => 0,
      Claimed_By => (others => 0),
      Status => Expired
   ));
   Num_Jobs : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Compute_Batch_Hash (
      Batch : TX_Batch;
      Hash  : out Byte_Array
   ) is
      Data : Byte_Array (0 .. 255);
   begin
      --  Hash transaction roots
      Data := (others => 0);
      for I in 0 .. Natural'Min (Batch.Count - 1, 7) loop
         for J in 0 .. 31 loop
            Data (I * 32 + J) := Batch.Transactions (I).From_Addr (J);
         end loop;
      end loop;
      Anubis_SHA3.SHA3_256 (Data, Hash (Hash'First .. Hash'First + 31));
   end Compute_Batch_Hash;

   function Verify_Signature (TX : Transaction) return Boolean is
      Msg_Hash : Byte_Array (0 .. 63);
      Tx_Data : Byte_Array (0 .. 255);
   begin
      --  Build transaction hash
      Tx_Data (0 .. 31) := TX.From_Addr;
      Tx_Data (32 .. 63) := TX.To_Addr;
      for I in 0 .. 7 loop
         Tx_Data (64 + I) := Unsigned_8 ((TX.Value / (256 ** I)) mod 256);
         Tx_Data (72 + I) := Unsigned_8 ((TX.Nonce / (256 ** I)) mod 256);
         Tx_Data (80 + I) := Unsigned_8 ((TX.Gas_Limit / (256 ** I)) mod 256);
         Tx_Data (88 + I) := Unsigned_8 ((TX.Gas_Price / (256 ** I)) mod 256);
      end loop;
      Anubis_SHA3.SHA3_512 (Tx_Data, Msg_Hash);

      --  Verify ML-DSA signature (simplified - would recover PK from address)
      return TX.Signature (0) /= 0;  -- Placeholder
   end Verify_Signature;

   ---------------------------------------------------------------------------
   --  Prover Registration
   ---------------------------------------------------------------------------

   procedure Register_Prover (
      Address        : Byte_Array;
      Stake          : Unsigned_64;
      Tier           : Prover_Tier;
      Success        : out Boolean
   ) is
   begin
      Success := False;

      if Stake < Minimum_Prover_Stake then
         return;
      end if;

      if Num_Provers >= 256 then
         return;
      end if;

      --  Check not already registered
      for I in 0 .. Num_Provers - 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. 31 loop
               if Provers (I).Address (J) /= Address (Address'First + J) then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               return;  -- Already registered
            end if;
         end;
      end loop;

      --  Register new prover
      Provers (Num_Provers).Address := Address;
      Provers (Num_Provers).Stake := Stake;
      Provers (Num_Provers).Tier := Tier;
      Provers (Num_Provers).Jobs_Completed := 0;
      Provers (Num_Provers).Reputation := 500;
      Provers (Num_Provers).Active := True;
      Num_Provers := Num_Provers + 1;

      Success := True;
   end Register_Prover;

   procedure Unregister_Prover (
      Address        : Byte_Array;
      Success        : out Boolean
   ) is
   begin
      Success := False;

      for I in 0 .. Num_Provers - 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. 31 loop
               if Provers (I).Address (J) /= Address (Address'First + J) then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               Provers (I).Active := False;
               Success := True;
               return;
            end if;
         end;
      end loop;
   end Unregister_Prover;

   ---------------------------------------------------------------------------
   --  Job Market
   ---------------------------------------------------------------------------

   procedure Post_Proof_Job (
      Batch          : TX_Batch;
      Deadline       : Unsigned_64;
      Reward         : Unsigned_64;
      Job            : out Proof_Job
   ) is
   begin
      Job.Batch := Batch;
      Job.Deadline := Deadline;
      Job.Reward := Reward;
      Job.Claimed_By := (others => 0);
      Job.Status := Posted;

      --  Generate job ID
      Compute_Batch_Hash (Batch, Job.Job_ID);

      --  Add to queue
      if Num_Jobs < 64 then
         Jobs (Num_Jobs) := Job;
         Num_Jobs := Num_Jobs + 1;
      end if;
   end Post_Proof_Job;

   procedure Claim_Job (
      Prover         : Byte_Array;
      Job_ID         : Byte_Array;
      Success        : out Boolean
   ) is
   begin
      Success := False;

      --  Find job
      for I in 0 .. Num_Jobs - 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. 31 loop
               if Jobs (I).Job_ID (J) /= Job_ID (Job_ID'First + J) then
                  Match := False;
                  exit;
               end if;
            end loop;

            if Match and Jobs (I).Status = Posted then
               Jobs (I).Claimed_By := Prover;
               Jobs (I).Status := Claimed;
               Success := True;
               return;
            end if;
         end;
      end loop;
   end Claim_Job;

   procedure Submit_Proof (
      Submission     : Proof_Submission;
      Success        : out Boolean
   ) is
   begin
      Success := False;

      --  Find job
      for I in 0 .. Num_Jobs - 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. 31 loop
               if Jobs (I).Job_ID (J) /= Submission.Job_ID (J) then
                  Match := False;
                  exit;
               end if;
            end loop;

            if Match and Jobs (I).Status = Claimed then
               --  Verify prover matches claimer
               declare
                  Prover_Match : Boolean := True;
               begin
                  for J in 0 .. 31 loop
                     if Jobs (I).Claimed_By (J) /= Submission.Prover (J) then
                        Prover_Match := False;
                        exit;
                     end if;
                  end loop;

                  if Prover_Match then
                     Jobs (I).Status := Submitted;
                     Success := True;
                     return;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Submit_Proof;

   procedure Accept_Proof (
      Job            : in Out Proof_Job;
      Submission     : Proof_Submission;
      Valid          : out Boolean
   ) is
   begin
      Valid := False;

      --  Verify proof
      if Submission.Proof_Length < 32 then
         return;
      end if;

      --  Verify batch proof (simplified)
      if Verify_Batch_Proof (Job.Batch, Submission.Proof (0 .. Submission.Proof_Length - 1)) then
         Job.Status := Verified;
         Valid := True;
      end if;
   end Accept_Proof;

   ---------------------------------------------------------------------------
   --  Pipeline Operations
   ---------------------------------------------------------------------------

   procedure Init_Pipeline (
      State          : out Pipeline_State;
      Start_Height   : Unsigned_64
   ) is
   begin
      State.Current_Height := Start_Height;
      State.Head_Proven := Start_Height;

      for I in State.Stages'Range loop
         State.Stages (I).Block_Height := Start_Height + Unsigned_64 (I);
         State.Stages (I).Stage := Collecting;
         State.Stages (I).Started_At := 0;
         State.Stages (I).Data := (others => 0);
         State.Stages (I).Data_Length := 0;
      end loop;
   end Init_Pipeline;

   procedure Add_Transaction (
      State          : in Out Pipeline_State;
      TX             : Transaction;
      Success        : out Boolean
   ) is
      Collecting_Stage : Natural := 0;
   begin
      Success := False;

      --  Find collecting stage
      for I in State.Stages'Range loop
         if State.Stages (I).Stage = Collecting then
            Collecting_Stage := I;
            exit;
         end if;
      end loop;

      --  Verify signature
      if not Verify_Signature (TX) then
         return;
      end if;

      --  Add to stage data (simplified)
      declare
         Offset : constant Natural := State.Stages (Collecting_Stage).Data_Length;
      begin
         if Offset + 32 <= State.Stages (Collecting_Stage).Data'Length then
            State.Stages (Collecting_Stage).Data (Offset .. Offset + 31) := TX.From_Addr;
            State.Stages (Collecting_Stage).Data_Length := Offset + 32;
            Success := True;
         end if;
      end;
   end Add_Transaction;

   procedure Advance_Pipeline (
      State          : in Out Pipeline_State;
      New_Block      : out Proven_Block;
      Has_Block      : out Boolean
   ) is
   begin
      Has_Block := False;
      New_Block.Height := 0;
      New_Block.Parent_Hash := (others => 0);
      New_Block.State_Root := (others => 0);
      New_Block.TX_Root := (others => 0);
      New_Block.Batch_Proof := (others => 0);
      New_Block.Proof_Length := 0;
      New_Block.Proposer := (others => 0);

      --  Check if any stage is ready to finalize
      for I in State.Stages'Range loop
         if State.Stages (I).Stage = Finalizing then
            --  Emit proven block
            New_Block.Height := State.Stages (I).Block_Height;
            Anubis_SHA3.SHA3_256 (
               State.Stages (I).Data (0 .. State.Stages (I).Data_Length - 1),
               New_Block.TX_Root
            );
            New_Block.Proof_Length := 32;  -- Simplified
            Has_Block := True;

            --  Reset stage
            State.Stages (I).Stage := Collecting;
            State.Stages (I).Block_Height := State.Current_Height + Pipeline_Depth;
            State.Stages (I).Data_Length := 0;

            State.Head_Proven := New_Block.Height;
         end if;
      end loop;

      --  Advance all stages
      for I in State.Stages'Range loop
         case State.Stages (I).Stage is
            when Collecting =>
               if State.Stages (I).Data_Length > 0 then
                  State.Stages (I).Stage := Executing;
               end if;

            when Executing =>
               State.Stages (I).Stage := Proving;

            when Proving =>
               State.Stages (I).Stage := Finalizing;

            when Finalizing =>
               null;  -- Handled above
         end case;
      end loop;

      State.Current_Height := State.Current_Height + 1;
   end Advance_Pipeline;

   function Get_Pipeline_Status (
      State          : Pipeline_State
   ) return Pipeline_Stage is
   begin
      return State.Stages (0);
   end Get_Pipeline_Status;

   ---------------------------------------------------------------------------
   --  Transaction Modes
   ---------------------------------------------------------------------------

   procedure Process_Standard_TX (
      TX             : Transaction;
      State          : in Out Pipeline_State;
      Accepted       : out Boolean
   ) is
   begin
      --  Verify signature and add to pipeline
      Add_Transaction (State, TX, Accepted);
   end Process_Standard_TX;

   procedure Process_PreProven_TX (
      TX             : PreProven_TX;
      State          : in Out Pipeline_State;
      Accepted       : out Boolean
   ) is
   begin
      Accepted := False;

      --  Verify mini-proof immediately
      if TX.Proof_Length < 32 then
         return;
      end if;

      --  Verify proof (simplified)
      declare
         Proof_Valid : Boolean := True;
         Checksum : Unsigned_8 := 0;
      begin
         for I in 0 .. TX.Proof_Length - 1 loop
            Checksum := Checksum xor TX.Mini_Proof (I);
         end loop;
         Proof_Valid := Checksum /= 0;

         if Proof_Valid then
            Add_Transaction (State, TX.TX, Accepted);
         end if;
      end;
   end Process_PreProven_TX;

   procedure Process_Batched_TX (
      TX             : Transaction;
      State          : in Out Pipeline_State;
      Accepted       : out Boolean
   ) is
   begin
      --  Add to batch aggregator (same as standard for now)
      Add_Transaction (State, TX, Accepted);
   end Process_Batched_TX;

   ---------------------------------------------------------------------------
   --  Batch Aggregation
   ---------------------------------------------------------------------------

   procedure Aggregate_Batch (
      Batch          : TX_Batch;
      Proof          : out Byte_Array;
      Proof_Length   : out Natural;
      Success        : out Boolean
   ) is
      Batch_Hash : Byte_Array (0 .. 31);
   begin
      Proof := (others => 0);
      Proof_Length := 0;
      Success := False;

      if Batch.Count = 0 then
         return;
      end if;

      --  Compute batch hash
      Compute_Batch_Hash (Batch, Batch_Hash);

      --  Generate aggregated proof (would use SCARAB circuit)
      declare
         Proof_Input : Byte_Array (0 .. 127);
      begin
         Proof_Input (0 .. 31) := Batch_Hash;
         for I in 0 .. 7 loop
            Proof_Input (32 + I) := Unsigned_8 (Batch.Count / (256 ** I) mod 256);
            Proof_Input (40 + I) := Unsigned_8 (Batch.Block_Height / (256 ** I) mod 256);
         end loop;
         Proof_Input (48 .. 127) := (others => 0);

         Anubis_SHA3.SHAKE256 (Proof_Input, Proof (Proof'First .. Proof'First + 159), 160);
         Proof_Length := 160;
      end;

      Success := True;
   end Aggregate_Batch;

   function Verify_Batch_Proof (
      Batch          : TX_Batch;
      Proof          : Byte_Array
   ) return Boolean is
      Expected_Hash : Byte_Array (0 .. 31);
      Proof_Hash : Byte_Array (0 .. 31);
   begin
      if Proof'Length < 32 then
         return False;
      end if;

      --  Compute expected batch hash
      Compute_Batch_Hash (Batch, Expected_Hash);

      --  Extract proof hash
      Proof_Hash := Proof (Proof'First .. Proof'First + 31);

      --  Compare
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in 0 .. 31 loop
            Diff := Diff or (Expected_Hash (I) xor Proof_Hash (I));
         end loop;
         return Diff = 0;
      end;
   end Verify_Batch_Proof;

   ---------------------------------------------------------------------------
   --  Finality
   ---------------------------------------------------------------------------

   function Get_Finality (
      Height         : Unsigned_64;
      State          : Pipeline_State
   ) return Finality_Status is
   begin
      if Height <= State.Head_Proven then
         if Height + 6 <= State.Current_Height then
            return Final;
         else
            return Hard;
         end if;
      else
         return Soft;
      end if;
   end Get_Finality;

   function Finality_Delay (
      Mode           : TX_Mode
   ) return Natural is
   begin
      case Mode is
         when Pre_Proven =>
            return 1;  -- Instant finality
         when Standard =>
            return Pipeline_Depth;  -- ~18 seconds
         when Batched =>
            return Pipeline_Depth + 1;  -- Slightly longer
      end case;
   end Finality_Delay;

   ---------------------------------------------------------------------------
   --  Rewards and Slashing
   ---------------------------------------------------------------------------

   function Calculate_Reward (
      Job            : Proof_Job;
      Submission_Time: Unsigned_64
   ) return Unsigned_64 is
      Base_Reward : constant Unsigned_64 := Job.Reward;
      Time_Bonus : Unsigned_64 := 0;
   begin
      --  Early submission bonus
      if Submission_Time < Job.Deadline then
         declare
            Early_Blocks : constant Unsigned_64 := Job.Deadline - Submission_Time;
         begin
            Time_Bonus := (Base_Reward * Early_Blocks) / 100;
         end;
      end if;

      return Base_Reward + Time_Bonus;
   end Calculate_Reward;

   procedure Slash_Prover (
      Prover         : in Out Prover_Info;
      Job            : Proof_Job;
      Slash_Amount   : out Unsigned_64
   ) is
   begin
      --  Slash 10% of stake for missed deadline
      Slash_Amount := Prover.Stake / 10;

      if Prover.Stake >= Slash_Amount then
         Prover.Stake := Prover.Stake - Slash_Amount;
      else
         Slash_Amount := Prover.Stake;
         Prover.Stake := 0;
      end if;

      --  Reduce reputation
      if Prover.Reputation >= 50 then
         Prover.Reputation := Prover.Reputation - 50;
      else
         Prover.Reputation := 0;
      end if;

      --  Deactivate if stake too low
      if Prover.Stake < Minimum_Prover_Stake then
         Prover.Active := False;
      end if;
   end Slash_Prover;

   ---------------------------------------------------------------------------
   --  Metrics
   ---------------------------------------------------------------------------

   function Get_Metrics (State : Pipeline_State) return Pipeline_Metrics is
      Metrics : Pipeline_Metrics;
      Active_Count : Natural := 0;
      Pending_Count : Natural := 0;
   begin
      --  Count active provers
      for I in 0 .. Num_Provers - 1 loop
         if Provers (I).Active then
            Active_Count := Active_Count + 1;
         end if;
      end loop;

      --  Count pending jobs
      for I in 0 .. Num_Jobs - 1 loop
         if Jobs (I).Status in Posted | Claimed then
            Pending_Count := Pending_Count + 1;
         end if;
      end loop;

      Metrics.Avg_Proof_Time := Unsigned_64 (Proof_Budget * 1000);  -- ms
      Metrics.Avg_Block_Time := Unsigned_64 (Block_Time * 1000);    -- ms
      Metrics.Proofs_Per_Hour := 3600 / Unsigned_64 (Block_Time);
      Metrics.Active_Provers := Active_Count;
      Metrics.Pending_Jobs := Pending_Count;

      return Metrics;
   end Get_Metrics;

end Scarab_Horus;
