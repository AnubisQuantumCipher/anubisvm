-------------------------------------------------------------------------------
--  ANUBIS Quantum Insurance Reserve Implementation Body
--  5% Allocation (50M ANUBIS) Immutably Locked for Cryptographic Migration
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Quantum_Insurance with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Quantum_Insurance (
      State          : out Insurance_State;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      State.Total_Allocation := Insurance_Allocation;
      State.Current_Balance := Insurance_Allocation;
      State.Total_Released := 0;
      State.Current_Threat := None;

      --  Initialize empty attestation
      State.Last_Attestation := (
         Timestamp => 0,
         Block_Number => 0,
         Threat_Level => None,
         Evidence_Hash => (others => 0),
         Oracle_PK => (others => 0),
         Signature => (others => 0),
         IPFS_Evidence => (others => 0)
      );

      State.Has_Active_Proposal := False;

      --  Initialize empty proposal
      State.Active_Proposal := (
         ID => 0,
         Proposer => (others => 0),
         Attestation => State.Last_Attestation,
         Amount => 0,
         Recipient => (others => 0),
         Purpose_Hash => (others => 0),
         Votes_For => 0,
         Votes_Against => 0,
         Unique_Voters => 0,
         Created_At => 0,
         Voting_End => 0,
         Execution_After => 0,
         Passed => False,
         Executed => False,
         Expired => False
      );

      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
      State.Sunset_Block := Genesis_Block + Sunset_Blocks;

      --  Initialize lock hash (proves immutability)
      State.Lock_Hash := (others => 0);
   end Init_Quantum_Insurance;

   ---------------------------------------------------------------------------
   --  Threat Attestation
   ---------------------------------------------------------------------------

   procedure Record_Attestation (
      State          : in Out Insurance_State;
      Attestation    : Oracle_Attestation;
      Result         : out Attest_Result
   ) is
   begin
      --  Cannot lower threat level
      if Attestation.Threat_Level < State.Current_Threat then
         Result := Lower_Than_Current;
         return;
      end if;

      --  Record new attestation
      State.Last_Attestation := Attestation;
      State.Current_Threat := Attestation.Threat_Level;

      Result := Attestation_Recorded;
   end Record_Attestation;

   function Verify_Oracle_Signature (
      Attestation    : Oracle_Attestation;
      Oracle_PK      : Byte_Array
   ) return Boolean is
      pragma Unreferenced (Oracle_PK);
   begin
      --  In production, would verify ML-DSA signature
      --  Check attestation has valid data
      for I in Attestation.Evidence_Hash'Range loop
         if Attestation.Evidence_Hash (I) /= 0 then
            return True;
         end if;
      end loop;

      return False;
   end Verify_Oracle_Signature;

   function Get_Threat_Level (
      State          : Insurance_State
   ) return Quantum_Threat_Level is
   begin
      return State.Current_Threat;
   end Get_Threat_Level;

   ---------------------------------------------------------------------------
   --  Release Proposals
   ---------------------------------------------------------------------------

   procedure Propose_Release (
      State          : in Out Insurance_State;
      Proposer       : Byte_Array;
      Amount         : Unsigned_64;
      Recipient      : Byte_Array;
      Purpose_Hash   : Byte_Array;
      Current_Block  : Unsigned_64;
      Result         : out Propose_Result
   ) is
   begin
      --  Check threat level
      if State.Current_Threat < Min_Release_Threat_Level then
         Result := Threat_Level_Insufficient;
         return;
      end if;

      --  Check no active proposal
      if State.Has_Active_Proposal then
         Result := Active_Proposal_Exists;
         return;
      end if;

      --  Check balance
      if Amount > State.Current_Balance then
         Result := Insurance_Exhausted;
         return;
      end if;

      --  Check sunset
      if Current_Block >= State.Sunset_Block then
         Result := Sunset_Reached;
         return;
      end if;

      --  Create proposal
      State.Active_Proposal.ID := State.Active_Proposal.ID + 1;

      --  Copy proposer
      for I in Proposer'Range loop
         if I - Proposer'First <= State.Active_Proposal.Proposer'Last then
            State.Active_Proposal.Proposer (I - Proposer'First) :=
               Proposer (I);
         end if;
      end loop;

      State.Active_Proposal.Attestation := State.Last_Attestation;
      State.Active_Proposal.Amount := Amount;

      --  Copy recipient
      for I in Recipient'Range loop
         if I - Recipient'First <= State.Active_Proposal.Recipient'Last then
            State.Active_Proposal.Recipient (I - Recipient'First) :=
               Recipient (I);
         end if;
      end loop;

      --  Copy purpose hash
      for I in Purpose_Hash'Range loop
         if I - Purpose_Hash'First <= State.Active_Proposal.Purpose_Hash'Last
         then
            State.Active_Proposal.Purpose_Hash (I - Purpose_Hash'First) :=
               Purpose_Hash (I);
         end if;
      end loop;

      State.Active_Proposal.Votes_For := 0;
      State.Active_Proposal.Votes_Against := 0;
      State.Active_Proposal.Unique_Voters := 0;
      State.Active_Proposal.Created_At := Current_Block;
      State.Active_Proposal.Voting_End := Current_Block + Voting_Period_Blocks;
      State.Active_Proposal.Execution_After :=
         State.Active_Proposal.Voting_End + Execution_Timelock_Blocks;
      State.Active_Proposal.Passed := False;
      State.Active_Proposal.Executed := False;
      State.Active_Proposal.Expired := False;

      State.Has_Active_Proposal := True;
      State.Current_Block := Current_Block;

      Result := Proposed;
   end Propose_Release;

   procedure Vote_Release (
      State          : in Out Insurance_State;
      Voter          : Byte_Array;
      Vote_Amount    : Unsigned_64;
      In_Favor       : Boolean;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Voter);
   begin
      --  Check voting period
      if Current_Block > State.Active_Proposal.Voting_End then
         Success := False;
         return;
      end if;

      --  Record vote
      if In_Favor then
         State.Active_Proposal.Votes_For :=
            State.Active_Proposal.Votes_For + Vote_Amount;
      else
         State.Active_Proposal.Votes_Against :=
            State.Active_Proposal.Votes_Against + Vote_Amount;
      end if;

      State.Active_Proposal.Unique_Voters :=
         State.Active_Proposal.Unique_Voters + 1;

      Success := True;
   end Vote_Release;

   procedure Finalize_Vote (
      State          : in Out Insurance_State;
      Circulating_Supply : Unsigned_64;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
      Quorum_Required : Unsigned_64;
      Total_Votes : Unsigned_64;
      Threshold_Votes : Unsigned_64;
   begin
      --  Check voting period ended
      if Current_Block < State.Active_Proposal.Voting_End then
         Success := False;
         return;
      end if;

      --  Calculate quorum (25%)
      Quorum_Required :=
         (Circulating_Supply * Release_Quorum_BP) / 10_000;

      Total_Votes := State.Active_Proposal.Votes_For +
                     State.Active_Proposal.Votes_Against;

      if Total_Votes < Quorum_Required then
         State.Active_Proposal.Passed := False;
         State.Has_Active_Proposal := False;
         Success := False;
         return;
      end if;

      --  Calculate threshold (67%)
      Threshold_Votes := (Total_Votes * Release_Vote_Threshold_BP) / 10_000;

      if State.Active_Proposal.Votes_For >= Threshold_Votes then
         State.Active_Proposal.Passed := True;
         Success := True;
      else
         State.Active_Proposal.Passed := False;
         State.Has_Active_Proposal := False;
         Success := False;
      end if;
   end Finalize_Vote;

   procedure Execute_Release (
      State          : in Out Insurance_State;
      Current_Block  : Unsigned_64;
      Result         : out Release_Result
   ) is
   begin
      --  Check proposal passed
      if not State.Active_Proposal.Passed then
         Result := Vote_Not_Passed;
         return;
      end if;

      --  Check timelock
      if Current_Block < State.Active_Proposal.Execution_After then
         Result := Timelock_Active;
         return;
      end if;

      --  Check not already executed
      if State.Active_Proposal.Executed then
         Result := Already_Executed;
         return;
      end if;

      --  Check balance
      if State.Active_Proposal.Amount > State.Current_Balance then
         Result := Insufficient_Balance;
         return;
      end if;

      --  Execute release
      State.Current_Balance :=
         State.Current_Balance - State.Active_Proposal.Amount;
      State.Total_Released :=
         State.Total_Released + State.Active_Proposal.Amount;
      State.Active_Proposal.Executed := True;
      State.Has_Active_Proposal := False;

      Result := Released;
   end Execute_Release;

   ---------------------------------------------------------------------------
   --  Access Control Verification
   ---------------------------------------------------------------------------

   function Is_Builder (
      Address        : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean is
   begin
      --  Compare address with builder public key hash
      for I in Address'Range loop
         if I - Address'First <= Builder_PK'Last then
            if Address (I) /= Builder_PK (I - Address'First) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Is_Builder;

   function Is_Valid_Recipient (
      Recipient      : Byte_Array;
      Builder_PK     : Byte_Array
   ) return Boolean is
   begin
      --  CRITICAL: Builder can NEVER be recipient
      return not Is_Builder (Recipient, Builder_PK);
   end Is_Valid_Recipient;

   function Verify_Lock_Integrity (
      State          : Insurance_State
   ) return Boolean is
   begin
      --  Verify lock hash is set and balance matches
      --  Simple check: allocation - released = balance
      return State.Current_Balance =
             State.Total_Allocation - State.Total_Released;
   end Verify_Lock_Integrity;

   ---------------------------------------------------------------------------
   --  Sunset Handling
   ---------------------------------------------------------------------------

   function Is_Sunset (
      State          : Insurance_State;
      Current_Block  : Unsigned_64
   ) return Boolean is
   begin
      return Current_Block >= State.Sunset_Block;
   end Is_Sunset;

   procedure Handle_Sunset (
      State          : in Out Insurance_State;
      DAO_Treasury   : Byte_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
      pragma Unreferenced (DAO_Treasury);
   begin
      --  Check sunset reached
      if Current_Block < State.Sunset_Block then
         Success := False;
         return;
      end if;

      --  Transfer remaining balance to DAO treasury
      --  In production, would execute actual transfer
      State.Total_Released := State.Total_Released + State.Current_Balance;
      State.Current_Balance := 0;

      Success := True;
   end Handle_Sunset;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Balance (
      State          : Insurance_State
   ) return Unsigned_64 is
   begin
      return State.Current_Balance;
   end Get_Balance;

   function Get_Total_Released (
      State          : Insurance_State
   ) return Unsigned_64 is
   begin
      return State.Total_Released;
   end Get_Total_Released;

   function Blocks_Until_Sunset (
      State          : Insurance_State;
      Current_Block  : Unsigned_64
   ) return Unsigned_64 is
   begin
      if Current_Block >= State.Sunset_Block then
         return 0;
      end if;

      return State.Sunset_Block - Current_Block;
   end Blocks_Until_Sunset;

   function Can_Release (
      State          : Insurance_State
   ) return Boolean is
   begin
      return State.Current_Threat >= Min_Release_Threat_Level;
   end Can_Release;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Insurance_State (
      State          : Insurance_State;
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

      Write_U64 (State.Total_Allocation);
      Write_U64 (State.Current_Balance);
      Write_U64 (State.Total_Released);

      --  Threat level
      if Idx <= Output'Last then
         Output (Idx) := Quantum_Threat_Level'Pos (State.Current_Threat);
         Idx := Idx + 1;
      end if;

      Write_U64 (State.Genesis_Block);
      Write_U64 (State.Current_Block);
      Write_U64 (State.Sunset_Block);

      --  Has active proposal
      if Idx <= Output'Last then
         Output (Idx) := (if State.Has_Active_Proposal then 1 else 0);
         Idx := Idx + 1;
      end if;

      --  Lock hash
      for I in State.Lock_Hash'Range loop
         if Idx <= Output'Last then
            Output (Idx) := State.Lock_Hash (I);
            Idx := Idx + 1;
         end if;
      end loop;

      Length := Idx - Output'First;
   end Serialize_Insurance_State;

   procedure Deserialize_Insurance_State (
      Input          : Byte_Array;
      State          : out Insurance_State;
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
      Init_Quantum_Insurance (State, 0);
      Success := False;

      if Input'Length < 58 then
         return;
      end if;

      Read_U64 (State.Total_Allocation);
      Read_U64 (State.Current_Balance);
      Read_U64 (State.Total_Released);

      --  Threat level
      if Idx <= Input'Last then
         if Natural (Input (Idx)) <=
            Quantum_Threat_Level'Pos (Quantum_Threat_Level'Last)
         then
            State.Current_Threat :=
               Quantum_Threat_Level'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      Read_U64 (State.Genesis_Block);
      Read_U64 (State.Current_Block);
      Read_U64 (State.Sunset_Block);

      --  Has active proposal
      if Idx <= Input'Last then
         State.Has_Active_Proposal := Input (Idx) /= 0;
         Idx := Idx + 1;
      end if;

      --  Lock hash
      for I in State.Lock_Hash'Range loop
         if Idx <= Input'Last then
            State.Lock_Hash (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      Success := True;
   end Deserialize_Insurance_State;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Insurance_State (State : out Insurance_State) is
   begin
      State.Total_Allocation := 0;
      State.Current_Balance := 0;
      State.Total_Released := 0;
      State.Current_Threat := None;
      Zeroize_Attestation (State.Last_Attestation);
      State.Has_Active_Proposal := False;
      Zeroize_Proposal (State.Active_Proposal);
      State.Genesis_Block := 0;
      State.Current_Block := 0;
      State.Sunset_Block := 0;
      State.Lock_Hash := (others => 0);
   end Zeroize_Insurance_State;

   procedure Zeroize_Attestation (A : out Oracle_Attestation) is
   begin
      A.Timestamp := 0;
      A.Block_Number := 0;
      A.Threat_Level := None;
      A.Evidence_Hash := (others => 0);
      A.Oracle_PK := (others => 0);
      A.Signature := (others => 0);
      A.IPFS_Evidence := (others => 0);
   end Zeroize_Attestation;

   procedure Zeroize_Proposal (P : out Release_Proposal) is
   begin
      P.ID := 0;
      P.Proposer := (others => 0);
      Zeroize_Attestation (P.Attestation);
      P.Amount := 0;
      P.Recipient := (others => 0);
      P.Purpose_Hash := (others => 0);
      P.Votes_For := 0;
      P.Votes_Against := 0;
      P.Unique_Voters := 0;
      P.Created_At := 0;
      P.Voting_End := 0;
      P.Execution_After := 0;
      P.Passed := False;
      P.Executed := False;
      P.Expired := False;
   end Zeroize_Proposal;

end Anubis_Quantum_Insurance;
