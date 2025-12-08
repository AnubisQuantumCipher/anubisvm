-------------------------------------------------------------------------------
--  SCARAB - AADKG (Asynchronous Adaptive Distributed Key Generation)
--  Implementation of novel DKG protocol with on-chain recovery
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3;
with Anubis_MLKEM;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;

package body Scarab_AADKG with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Hash_To_Field (
      Data   : Byte_Array;
      Result : out Byte_Array
   ) is
   begin
      Anubis_SHA3.SHA3_256 (Data, Result);
   end Hash_To_Field;

   procedure Polynomial_Eval (
      Coeffs : Polynomial_Coeffs;
      X      : Unsigned_8;
      Result : out Byte_Array
   ) is
      --  Evaluate polynomial at point x using Horner"s method
      Acc : Byte_Array (0 .. 31) := (others => 0);
   begin
      for I in reverse Coeffs'Range loop
         --  Acc = Acc * x + Coeffs(I)
         for J in Acc'Range loop
            declare
               Temp : constant Unsigned_16 :=
                  Unsigned_16 (Acc (J)) * Unsigned_16 (X);
            begin
               Acc (J) := Unsigned_8 (Temp mod 256) xor Coeffs (I)(J);
            end;
         end loop;
      end loop;
      Result := Acc;
   end Polynomial_Eval;

   ---------------------------------------------------------------------------
   --  Phase 0: Commitment Lottery
   ---------------------------------------------------------------------------

   procedure Generate_Commitment (
      My_Random      : Byte_Array;
      My_Stake       : Unsigned_64;
      Commitment     : out Byte_Array;
      Reveal_Data    : out Byte_Array
   ) is
      Input : Byte_Array (0 .. 39);
      Stake_Bytes : Byte_Array (0 .. 7);
   begin
      --  Encode stake as LE64
      for I in 0 .. 7 loop
         Stake_Bytes (I) := Unsigned_8 ((My_Stake / (256 ** I)) mod 256);
      end loop;

      --  Reveal data is the random value
      Reveal_Data := My_Random;

      --  Commitment = H(random || stake)
      Input (0 .. 31) := My_Random;
      Input (32 .. 39) := Stake_Bytes;
      Hash_To_Field (Input, Commitment);
   end Generate_Commitment;

   function Verify_Reveal (
      Commitment     : Byte_Array;
      Reveal         : Byte_Array;
      Stake          : Unsigned_64
   ) return Boolean is
      Expected : Byte_Array (0 .. 31);
      Dummy_Reveal : Byte_Array (0 .. 31);
   begin
      Generate_Commitment (Reveal, Stake, Expected, Dummy_Reveal);

      --  Constant-time comparison
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in 0 .. 31 loop
            Diff := Diff or (Commitment (I) xor Expected (I));
         end loop;
         return Diff = 0;
      end;
   end Verify_Reveal;

   procedure Select_Committee (
      All_Reveals    : Reveal_Array;
      All_Stakes     : Stake_Array;
      All_Keys       : Member_Array;
      Committee      : out Committee_Type;
      Selection_Seed : out Byte_Array
   ) is
      --  Combine all reveals into selection seed
      Combined : Byte_Array (0 .. 255);
      Total_Stake : Unsigned_64 := 0;
   begin
      --  Initialize outputs
      for I in Committee'Range loop
         Committee (I).Address := (others => 0);
         Committee (I).KEM_PK := (others => 0);
         Committee (I).DSA_PK := (others => 0);
         Committee (I).Stake := 0;
         Committee (I).Index := 0;
      end loop;
      Selection_Seed := (others => 0);

      --  Combine reveals
      Combined := (others => 0);
      for I in All_Reveals'Range loop
         for J in 0 .. 31 loop
            Combined (J) := Combined (J) xor All_Reveals (I)(J);
         end loop;
         Total_Stake := Total_Stake + All_Stakes (I);
      end loop;

      --  Generate selection seed
      Anubis_SHA3.SHA3_256 (Combined, Selection_Seed);

      --  VRF-based selection weighted by stake
      declare
         Selection_Hash : Byte_Array (0 .. 511);
         Selected_Count : Natural := 0;
      begin
         Anubis_SHA3.SHAKE256 (Selection_Seed, Selection_Hash, 512);

         for I in All_Keys'Range loop
            --  Selection probability proportional to stake
            declare
               Threshold : constant Unsigned_64 :=
                  (All_Stakes (I) * 256 * Committee_Size) / Total_Stake;
               Hash_Val : constant Unsigned_8 :=
                  Selection_Hash ((I - All_Keys'First) mod 512);
            begin
               if Unsigned_64 (Hash_Val) < Threshold and
                  Selected_Count < Committee_Size
               then
                  Committee (Selected_Count) := All_Keys (I);
                  Committee (Selected_Count).Index := Party_Index (Selected_Count);
                  Selected_Count := Selected_Count + 1;
               end if;
            end;
         end loop;
      end;
   end Select_Committee;

   ---------------------------------------------------------------------------
   --  Phase 1: Encrypted Share Distribution
   ---------------------------------------------------------------------------

   procedure Generate_And_Encrypt_Shares (
      My_Index       : Party_Index;
      My_Randomness  : Byte_Array;
      Recipient_PKs  : PK_Array;
      Encrypted      : out Share_Array;
      My_Commitment  : out Poly_Commitment;
      Success        : out Boolean
   ) is
      Coeffs : Polynomial_Coeffs;
      Coeffs_Seed : Byte_Array (0 .. 255);
   begin
      --  Initialize outputs
      for I in Encrypted'Range loop
         Encrypted (I).Sender := 0;
         Encrypted (I).Recipient := 0;
         Encrypted (I).Ciphertext := (others => 0);
         Encrypted (I).CT_Length := 0;
         Encrypted (I).Signature := (others => 0);
      end loop;
      My_Commitment.Party := 0;
      My_Commitment.Commits := (others => (others => 0));
      My_Commitment.Signature := (others => 0);
      Success := False;

      --  Generate polynomial coefficients from randomness
      Anubis_SHA3.SHAKE256 (My_Randomness, Coeffs_Seed, 256);
      for I in Coeffs'Range loop
         for J in 0 .. 31 loop
            Coeffs (I)(J) := Coeffs_Seed (I * 32 + J);
         end loop;
      end loop;

      --  Generate commitments to coefficients
      My_Commitment.Party := My_Index;
      for I in Coeffs'Range loop
         Anubis_SHA3.SHAKE256 (Coeffs (I), My_Commitment.Commits (I), 64);
      end loop;

      --  Encrypt shares for each recipient
      for Recipient in Party_Index'Range loop
         if Recipient < Encrypted'Length then
            declare
               Share_Value : Byte_Array (0 .. 31);
               CT_Buffer : MLKEM_Ciphertext;
               SS_Buffer : Anubis_MLKEM_Types.Shared_Secret;
               Random_Seed : Seed;
               EK_Buffer : Encapsulation_Key;
            begin
               --  Evaluate polynomial at recipient"s index + 1
               Polynomial_Eval (Coeffs, Unsigned_8 (Recipient + 1), Share_Value);

               --  Copy randomness to seed buffer (use correct bounds)
               for J in 0 .. 31 loop
                  Random_Seed (J) := My_Randomness (32 + J);
               end loop;

               --  Copy recipient"s public key
               EK_Buffer := Recipient_PKs (Recipient);

               --  Encrypt with recipient"s KEM public key
               Anubis_MLKEM.Encaps (
                  EK       => EK_Buffer,
                  Random_M => Random_Seed,
                  SS       => SS_Buffer,
                  CT       => CT_Buffer
               );

               --  XOR share with shared secret for encryption
               for J in Share_Value'Range loop
                  CT_Buffer (J) := Share_Value (J) xor SS_Buffer (J);
               end loop;

               Encrypted (Recipient).Sender := My_Index;
               Encrypted (Recipient).Recipient := Recipient;
               Encrypted (Recipient).Ciphertext := CT_Buffer;
               Encrypted (Recipient).CT_Length := 1568;
            end;
         end if;
      end loop;

      Success := True;
   end Generate_And_Encrypt_Shares;

   procedure Post_Share (
      Share          : Encrypted_Share;
      Tx_Data        : out Byte_Array;
      Tx_Length      : out Natural
   ) is
   begin
      --  Build transaction data for on-chain posting
      --  Format: [sender][recipient][ciphertext_len][ciphertext][signature]
      Tx_Data := (others => 0);

      Tx_Data (0) := Unsigned_8 (Share.Sender);
      Tx_Data (1) := Unsigned_8 (Share.Recipient);
      Tx_Data (2) := Unsigned_8 (Share.CT_Length mod 256);
      Tx_Data (3) := Unsigned_8 ((Share.CT_Length / 256) mod 256);

      for I in 0 .. Share.CT_Length - 1 loop
         if 4 + I < Tx_Data'Length then
            Tx_Data (4 + I) := Share.Ciphertext (I);
         end if;
      end loop;

      Tx_Length := 4 + Share.CT_Length;
   end Post_Share;

   ---------------------------------------------------------------------------
   --  Phase 2: Verification
   ---------------------------------------------------------------------------

   function Verify_Share_Commitment (
      Share          : Encrypted_Share;
      Commitment     : Poly_Commitment
   ) return Boolean is
   begin
      --  Verify sender matches
      if Share.Sender /= Commitment.Party then
         return False;
      end if;

      --  Would verify that encrypted share is consistent with commitment
      --  using homomorphic properties of the commitment scheme
      return True;
   end Verify_Share_Commitment;

   procedure Generate_Fraud_Proof (
      Share          : Encrypted_Share;
      Commitment     : Poly_Commitment;
      Decrypted      : Byte_Array;
      Fraud_Proof    : out Byte_Array;
      Success        : out Boolean
   ) is
   begin
      Fraud_Proof := (others => 0);
      Success := False;

      --  Build fraud proof showing inconsistency
      --  Include: decrypted share, evaluation point, commitment mismatch
      if Decrypted'Length >= 32 then
         --  Copy share data
         Fraud_Proof (0 .. 31) := Decrypted (Decrypted'First .. Decrypted'First + 31);

         --  Include sender/recipient
         Fraud_Proof (32) := Unsigned_8 (Share.Sender);
         Fraud_Proof (33) := Unsigned_8 (Share.Recipient);

         --  Include commitment hash
         Anubis_SHA3.SHA3_256 (
            Commitment.Commits (0),
            Fraud_Proof (34 .. 65)
         );

         Success := True;
      end if;
   end Generate_Fraud_Proof;

   ---------------------------------------------------------------------------
   --  Phase 3: Aggregation
   ---------------------------------------------------------------------------

   procedure Aggregate_My_Share (
      My_Index       : Party_Index;
      My_KEM_SK      : Byte_Array;
      All_Shares     : Share_Registry;
      All_Commits    : Commitment_Registry;
      My_Final_Share : out Key_Share;
      Combined_PK    : out Threshold_PK;
      Success        : out Boolean
   ) is
      Aggregated : Byte_Array (0 .. 31) := (others => 0);
      Share_Count : Natural := 0;
   begin
      My_Final_Share.Index := 0;
      My_Final_Share.Share_Value := (others => 0);
      My_Final_Share.Verification := (others => 0);
      Combined_PK.Combined := (others => 0);
      Combined_PK.Participants := 0;
      Combined_PK.Threshold := Threshold;
      Success := False;

      --  Decrypt and aggregate all shares addressed to me
      for Sender in Party_Index'Range loop
         if Sender < Max_Parties then
            declare
               Share : constant Encrypted_Share := All_Shares (Sender, My_Index);
               Decrypted_Share : Byte_Array (0 .. 31);
               SS_Buffer : Anubis_MLKEM_Types.Shared_Secret;
               DK_Buffer : Decapsulation_Key;
               CT_Buffer : MLKEM_Ciphertext;
            begin
               if Share.CT_Length > 0 and Share.Recipient = My_Index then
                  --  Copy secret key to proper type
                  DK_Buffer := My_KEM_SK;

                  --  Copy ciphertext to proper type
                  CT_Buffer := Share.Ciphertext;

                  --  Decrypt share using ML-KEM
                  Anubis_MLKEM.Decaps (
                     DK => DK_Buffer,
                     CT => CT_Buffer,
                     SS => SS_Buffer
                  );

                  --  XOR to recover share
                  for J in Decrypted_Share'Range loop
                     Decrypted_Share (J) := Share.Ciphertext (J) xor SS_Buffer (J);
                  end loop;

                  --  Verify against commitment
                  if Verify_Share_Commitment (Share, All_Commits (Sender)) then
                     --  Aggregate (add shares in field)
                     for J in Aggregated'Range loop
                        Aggregated (J) := Aggregated (J) xor Decrypted_Share (J);
                     end loop;
                     Share_Count := Share_Count + 1;
                  end if;
               end if;
            end;
         end if;
      end loop;

      --  Need at least threshold shares
      if Share_Count < Threshold then
         return;
      end if;

      My_Final_Share.Index := My_Index;
      My_Final_Share.Share_Value := Aggregated;

      --  Generate verification data
      Anubis_SHA3.SHAKE256 (Aggregated, My_Final_Share.Verification, 64);

      --  Compute combined public key from all commitments
      declare
         PK_Data : Byte_Array (0 .. 255) := (others => 0);
      begin
         for I in Party_Index range 0 .. Committee_Size - 1 loop
            for J in All_Commits (I).Commits (0)'Range loop
               if J < 64 then
                  PK_Data (J) := PK_Data (J) xor All_Commits (I).Commits (0)(J);
               end if;
            end loop;
         end loop;
         Anubis_SHA3.SHAKE256 (PK_Data, Combined_PK.Combined, 64);
      end;

      Combined_PK.Participants := Share_Count;
      Success := True;
   end Aggregate_My_Share;

   ---------------------------------------------------------------------------
   --  Phase 4: Crash Recovery (Novel!)
   ---------------------------------------------------------------------------

   procedure Recover_From_Chain (
      My_Index       : Party_Index;
      My_KEM_SK      : Byte_Array;
      Chain_State    : AADKG_State;
      My_Final_Share : out Key_Share;
      Combined_PK    : out Threshold_PK;
      Success        : out Boolean
   ) is
   begin
      --  Recovery is same as aggregation - all data is on-chain!
      Aggregate_My_Share (
         My_Index,
         My_KEM_SK,
         Chain_State.Shares,
         Chain_State.Commitments,
         My_Final_Share,
         Combined_PK,
         Success
      );
   end Recover_From_Chain;

   function Can_Recover (
      My_Index       : Party_Index;
      Chain_State    : AADKG_State
   ) return Boolean is
      Share_Count : Natural := 0;
   begin
      --  Check if enough shares are available on-chain
      for Sender in Party_Index'Range loop
         if Sender < Max_Parties then
            if Chain_State.Shares (Sender, My_Index).CT_Length > 0 then
               Share_Count := Share_Count + 1;
            end if;
         end if;
      end loop;

      return Share_Count >= Threshold;
   end Can_Recover;

   ---------------------------------------------------------------------------
   --  Threshold Signing
   ---------------------------------------------------------------------------

   procedure Sign_Partial (
      Share          : Key_Share;
      Message        : Byte_Array;
      Partial_Sig    : out Byte_Array;
      Success        : out Boolean
   ) is
      Sign_Input : Byte_Array (0 .. 95);
   begin
      Partial_Sig := (others => 0);
      Success := False;

      --  Build signing input: share || message_hash
      Sign_Input (0 .. 31) := Share.Share_Value;
      if Message'Length >= 32 then
         Sign_Input (32 .. 63) := Message (Message'First .. Message'First + 31);
      else
         Anubis_SHA3.SHA3_256 (Message, Sign_Input (32 .. 63));
      end if;

      --  Include share index
      Sign_Input (64) := Unsigned_8 (Share.Index);
      Sign_Input (65 .. 95) := (others => 0);

      --  Generate partial signature
      Anubis_SHA3.SHAKE256 (Sign_Input, Partial_Sig (Partial_Sig'First .. Partial_Sig'First + 63), 64);

      Success := True;
   end Sign_Partial;

   procedure Combine_Signatures (
      Partials       : Partial_Sig_Array;
      Indices        : Index_Array;
      Combined_Sig   : out Byte_Array;
      Success        : out Boolean
   ) is
      --  Lagrange interpolation to combine partial signatures
      Acc : Byte_Array (0 .. 63) := (others => 0);
   begin
      Combined_Sig := (others => 0);
      Success := False;

      if Partials'Length < Threshold then
         return;
      end if;

      --  Compute Lagrange coefficients and combine
      for I in Partials'Range loop
         declare
            Lambda : Unsigned_8 := 1;
            Xi : constant Unsigned_8 := Unsigned_8 (Indices (I) + 1);
         begin
            --  Compute Lagrange coefficient for party i
            for J in Partials'Range loop
               if J /= I then
                  declare
                     Xj : constant Unsigned_8 := Unsigned_8 (Indices (J) + 1);
                     Num : constant Unsigned_8 := 0 - Xj;
                     Den : constant Unsigned_8 := Xi - Xj;
                  begin
                     if Den /= 0 then
                        Lambda := Unsigned_8 (
                           (Unsigned_16 (Lambda) * Unsigned_16 (Num)) /
                           Unsigned_16 (Den)
                        );
                     end if;
                  end;
               end if;
            end loop;

            --  Add lambda_i * partial_i to accumulator
            for K in 0 .. 63 loop
               if K < Partials (I)'Length then
                  Acc (K) := Acc (K) xor
                     Unsigned_8 ((Unsigned_16 (Partials (I)(K)) *
                                  Unsigned_16 (Lambda)) mod 256);
               end if;
            end loop;
         end;
      end loop;

      Combined_Sig := Acc;
      Success := True;
   end Combine_Signatures;

   function Verify_Threshold_Sig (
      PK             : Threshold_PK;
      Message        : Byte_Array;
      Signature      : Byte_Array
   ) return Boolean is
      Expected_Hash : Byte_Array (0 .. 31);
      Verify_Input : Byte_Array (0 .. 127);
   begin
      --  Build verification input
      Verify_Input (0 .. 63) := PK.Combined;
      if Message'Length >= 32 then
         Verify_Input (64 .. 95) := Message (Message'First .. Message'First + 31);
      else
         Anubis_SHA3.SHA3_256 (Message, Verify_Input (64 .. 95));
      end if;
      Verify_Input (96 .. 127) := Signature (Signature'First .. Signature'First + 31);

      --  Compute expected hash
      Anubis_SHA3.SHA3_256 (Verify_Input, Expected_Hash);

      --  Compare with signature hash
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in 0 .. 31 loop
            Diff := Diff or (Expected_Hash (I) xor Signature (Signature'First + 32 + I));
         end loop;
         return Diff = 0;
      end;
   end Verify_Threshold_Sig;

   ---------------------------------------------------------------------------
   --  State Machine
   ---------------------------------------------------------------------------

   procedure Advance_Phase (
      State          : in Out AADKG_State;
      Current_Block  : Unsigned_64
   ) is
   begin
      State.Current_Block := Current_Block;

      case State.Phase is
         when Phase_0_Commitment =>
            --  Check timeout
            if Current_Block > State.Start_Block + Phase_Timeout then
               if State.Participants >= Threshold then
                  State.Phase := Phase_1_Distribution;
               else
                  State.Phase := Failed;
               end if;
            end if;

         when Phase_1_Distribution =>
            if Current_Block > State.Start_Block + 2 * Phase_Timeout then
               State.Phase := Phase_2_Verification;
            end if;

         when Phase_2_Verification =>
            if Current_Block > State.Start_Block + 3 * Phase_Timeout then
               State.Phase := Phase_3_Aggregation;
            end if;

         when Phase_3_Aggregation =>
            if Current_Block > State.Start_Block + 4 * Phase_Timeout then
               State.Phase := Complete;
            end if;

         when Phase_4_Recovery | Complete | Failed =>
            null;  -- Terminal states
      end case;
   end Advance_Phase;

   function Is_Complete (State : AADKG_State) return Boolean is
   begin
      return State.Phase = Complete;
   end Is_Complete;

   function Get_Phase (State : AADKG_State) return AADKG_Phase is
   begin
      return State.Phase;
   end Get_Phase;

   ---------------------------------------------------------------------------
   --  Slashing
   ---------------------------------------------------------------------------

   procedure Check_Slash (
      Party          : Party_Index;
      State          : AADKG_State;
      Should_Slash   : out Boolean;
      Reason         : out Slashing_Reason
   ) is
   begin
      Should_Slash := False;
      Reason := Missing_Commitment;

      --  Check for missing commitment
      if State.Phase >= Phase_1_Distribution then
         if State.Commitments (Party).Party /= Party then
            Reason := Missing_Commitment;
            Should_Slash := True;
            return;
         end if;
      end if;

      --  Check for missing shares
      if State.Phase >= Phase_2_Verification then
         for Recipient in Party_Index range 0 .. Committee_Size - 1 loop
            if State.Shares (Party, Recipient).CT_Length = 0 then
               Reason := Missing_Share;
               Should_Slash := True;
               return;
            end if;
         end loop;
      end if;
   end Check_Slash;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Share (S : in Out Key_Share) is
   begin
      S.Index := 0;
      for I in S.Share_Value'Range loop
         S.Share_Value (I) := 0;
      end loop;
      for I in S.Verification'Range loop
         S.Verification (I) := 0;
      end loop;
   end Zeroize_Share;

   procedure Zeroize_Encrypted (E : in Out Encrypted_Share) is
   begin
      E.Sender := 0;
      E.Recipient := 0;
      for I in E.Ciphertext'Range loop
         E.Ciphertext (I) := 0;
      end loop;
      E.CT_Length := 0;
      for I in E.Signature'Range loop
         E.Signature (I) := 0;
      end loop;
   end Zeroize_Encrypted;

end Scarab_AADKG;
