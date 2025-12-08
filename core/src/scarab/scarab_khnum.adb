-------------------------------------------------------------------------------
--  SCARAB - KHNUM TX Aggregation Layer Implementation
--  Key Homomorphic Numerically Unified Multiplexing
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

package body Scarab_Khnum with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Batch Management
   ---------------------------------------------------------------------------

   procedure Init_Batch (
      Batch          : out Aggregation_Batch;
      Domain_Sep     : Byte_Array;
      Block_Height   : Unsigned_64
   ) is
   begin
      Batch.Count := 0;
      Batch.Block_Height := Block_Height;
      Batch.Finalized := False;

      --  Copy domain separator
      for I in 0 .. Domain_Sep_Size - 1 loop
         Batch.Domain_Sep (I) := Domain_Sep (Domain_Sep'First + I);
      end loop;

      --  Initialize entries to zeros
      for I in Batch.Entries'Range loop
         Batch.Entries (I).Verified := False;
         for J in Batch.Entries (I).Signer_PK'Range loop
            Batch.Entries (I).Signer_PK (J) := 0;
         end loop;
         for J in Batch.Entries (I).Message_Hash'Range loop
            Batch.Entries (I).Message_Hash (J) := 0;
         end loop;
         for J in Batch.Entries (I).Signature'Range loop
            Batch.Entries (I).Signature (J) := 0;
         end loop;
      end loop;
   end Init_Batch;

   procedure Add_Signature (
      Batch          : in Out Aggregation_Batch;
      Signer_PK      : Byte_Array;
      Message_Hash   : Byte_Array;
      Signature      : Byte_Array;
      Success        : out Boolean
   ) is
   begin
      if Batch.Count >= Max_Sigs_Per_Batch then
         Success := False;
         return;
      end if;

      --  Copy signature entry
      for I in 0 .. MLDSA_PK_Size - 1 loop
         Batch.Entries (Batch.Count).Signer_PK (I) := Signer_PK (Signer_PK'First + I);
      end loop;

      for I in 0 .. Message_Hash_Size - 1 loop
         Batch.Entries (Batch.Count).Message_Hash (I) := Message_Hash (Message_Hash'First + I);
      end loop;

      for I in 0 .. MLDSA_Sig_Size - 1 loop
         Batch.Entries (Batch.Count).Signature (I) := Signature (Signature'First + I);
      end loop;

      Batch.Entries (Batch.Count).Verified := False;
      Batch.Count := Batch.Count + 1;
      Success := True;
   end Add_Signature;

   procedure Remove_Signature (
      Batch          : in Out Aggregation_Batch;
      Index          : Sig_Index;
      Success        : out Boolean
   ) is
   begin
      if Index >= Batch.Count then
         Success := False;
         return;
      end if;

      --  Shift entries down
      for I in Index .. Batch.Count - 2 loop
         Batch.Entries (I) := Batch.Entries (I + 1);
      end loop;

      Batch.Count := Batch.Count - 1;
      Success := True;
   end Remove_Signature;

   function Signature_Count (Batch : Aggregation_Batch) return Natural is
   begin
      return Batch.Count;
   end Signature_Count;

   function Is_Full (Batch : Aggregation_Batch) return Boolean is
   begin
      return Batch.Count >= Max_Sigs_Per_Batch;
   end Is_Full;

   ---------------------------------------------------------------------------
   --  Individual Verification
   ---------------------------------------------------------------------------

   procedure Verify_Single (
      Batch          : in Out Aggregation_Batch;
      Index          : Sig_Index;
      Valid          : out Boolean
   ) is
      PK_Arr : Public_Key;
      Sig_Arr : Signature;
      Msg_Arr : Byte_Array (0 .. Message_Hash_Size - 1);
   begin
      --  Copy data for verification
      for I in PK_Arr'Range loop
         PK_Arr (I) := Batch.Entries (Index).Signer_PK (I);
      end loop;

      for I in Sig_Arr'Range loop
         Sig_Arr (I) := Batch.Entries (Index).Signature (I);
      end loop;

      for I in Msg_Arr'Range loop
         Msg_Arr (I) := Batch.Entries (Index).Message_Hash (I);
      end loop;

      --  Verify using ML-DSA-87
      Valid := Anubis_MLDSA.Verify (
         PK  => PK_Arr,
         Msg => Msg_Arr,
         Sig => Sig_Arr
      );

      Batch.Entries (Index).Verified := Valid;
   end Verify_Single;

   procedure Verify_All_Sequential (
      Batch          : in Out Aggregation_Batch;
      Result         : out Verify_Result
   ) is
      Valid : Boolean;
   begin
      Result.Valid := True;
      Result.Num_Verified := 0;
      Result.Failed_Index := 0;
      Result.Error_Code := 0;

      for I in 0 .. Batch.Count - 1 loop
         Verify_Single (Batch, I, Valid);
         if Valid then
            Result.Num_Verified := Result.Num_Verified + 1;
         else
            Result.Valid := False;
            Result.Failed_Index := I;
            Result.Error_Code := 1;
            return;
         end if;
      end loop;
   end Verify_All_Sequential;

   procedure Verify_All_Parallel (
      Batch          : in Out Aggregation_Batch;
      Num_Threads    : Natural;
      Result         : out Verify_Result
   ) is
      pragma Unreferenced (Num_Threads);
   begin
      --  Sequential implementation (SPARK cannot express parallelism)
      Verify_All_Sequential (Batch, Result);
   end Verify_All_Parallel;

   ---------------------------------------------------------------------------
   --  Aggregation Core
   ---------------------------------------------------------------------------

   procedure Aggregate (
      Batch          : in Out Aggregation_Batch;
      Agg_Sig        : out Aggregated_Signature;
      Success        : out Boolean
   ) is
      Msg_Root : Byte_Array (0 .. 31);
      PK_Root : Byte_Array (0 .. 31);
   begin
      --  Build Merkle trees
      Build_Message_Tree (Batch, Msg_Root);
      Build_Signer_Tree (Batch, PK_Root);

      --  Initialize aggregated signature
      Agg_Sig.Num_Sigs := Batch.Count;
      Agg_Sig.Block_Height := Batch.Block_Height;

      for I in Msg_Root'Range loop
         Agg_Sig.Message_Root (I) := Msg_Root (I);
      end loop;

      for I in PK_Root'Range loop
         Agg_Sig.Signer_Root (I) := PK_Root (I);
      end loop;

      for I in Agg_Sig.Domain_Sep'Range loop
         Agg_Sig.Domain_Sep (I) := Batch.Domain_Sep (I);
      end loop;

      --  Compute batch commitment
      declare
         Commit_Input : Byte_Array (0 .. 127);
         Commit_Hash : Anubis_SHA3.SHA3_512_Digest;
      begin
         --  Commitment = Hash(MsgRoot || PKRoot || Domain || Height)
         for I in 0 .. 31 loop
            Commit_Input (I) := Msg_Root (I);
            Commit_Input (I + 32) := PK_Root (I);
            Commit_Input (I + 64) := Batch.Domain_Sep (I);
         end loop;

         for I in 0 .. 7 loop
            Commit_Input (96 + I) := Byte ((Batch.Block_Height / (2 ** (I * 8))) mod 256);
         end loop;

         for I in 104 .. 127 loop
            Commit_Input (I) := 0;
         end loop;

         Anubis_SHA3.SHA3_512 (Commit_Input, Commit_Hash);

         for I in Agg_Sig.Batch_Commit'Range loop
            Agg_Sig.Batch_Commit (I) := Commit_Hash (I);
         end loop;
      end;

      --  Combine signatures homomorphically (simplified - XOR combination)
      --  Real implementation would use lattice-based homomorphic combination
      declare
         Combined : Byte_Array (0 .. Aggregated_Sig_Size - 1) := (others => 0);
      begin
         for S in 0 .. Batch.Count - 1 loop
            for I in 0 .. Aggregated_Sig_Size - 1 loop
               if I < MLDSA_Sig_Size then
                  Combined (I) := Combined (I) xor Batch.Entries (S).Signature (I);
               end if;
            end loop;
         end loop;

         for I in Combined'Range loop
            Agg_Sig.Aggregate (I) := Combined (I);
         end loop;
      end;

      Batch.Finalized := True;
      Success := True;
   end Aggregate;

   procedure Aggregate_Parallel (
      Batch          : in Out Aggregation_Batch;
      Config         : Parallel_Config;
      Agg_Sig        : out Aggregated_Signature;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Config);
   begin
      --  Sequential implementation (SPARK cannot express parallelism)
      Aggregate (Batch, Agg_Sig, Success);
   end Aggregate_Parallel;

   procedure Combine_Aggregates (
      Agg1           : Aggregated_Signature;
      Agg2           : Aggregated_Signature;
      Combined       : out Aggregated_Signature;
      Success        : out Boolean
   ) is
   begin
      Combined.Num_Sigs := Agg1.Num_Sigs + Agg2.Num_Sigs;
      Combined.Block_Height := Agg1.Block_Height;

      --  Combine aggregated data
      for I in Combined.Aggregate'Range loop
         Combined.Aggregate (I) := Agg1.Aggregate (I) xor Agg2.Aggregate (I);
      end loop;

      --  Combine roots
      for I in 0 .. 31 loop
         Combined.Message_Root (I) := Agg1.Message_Root (I) xor Agg2.Message_Root (I);
         Combined.Signer_Root (I) := Agg1.Signer_Root (I) xor Agg2.Signer_Root (I);
         Combined.Domain_Sep (I) := Agg1.Domain_Sep (I);
      end loop;

      --  Combine batch commits
      for I in Combined.Batch_Commit'Range loop
         Combined.Batch_Commit (I) := Agg1.Batch_Commit (I) xor Agg2.Batch_Commit (I);
      end loop;

      Success := True;
   end Combine_Aggregates;

   ---------------------------------------------------------------------------
   --  Verification of Aggregated Signatures
   ---------------------------------------------------------------------------

   function Verify_Aggregated (
      Agg_Sig        : Aggregated_Signature
   ) return Boolean is
      Commit_Check : Byte_Array (0 .. 127);
      Commit_Hash : Anubis_SHA3.SHA3_512_Digest;
   begin
      if Agg_Sig.Num_Sigs = 0 then
         return False;
      end if;

      --  Recompute batch commitment
      for I in 0 .. 31 loop
         Commit_Check (I) := Agg_Sig.Message_Root (I);
         Commit_Check (I + 32) := Agg_Sig.Signer_Root (I);
         Commit_Check (I + 64) := Agg_Sig.Domain_Sep (I);
      end loop;

      for I in 0 .. 7 loop
         Commit_Check (96 + I) := Byte ((Agg_Sig.Block_Height / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 104 .. 127 loop
         Commit_Check (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_512 (Commit_Check, Commit_Hash);

      --  Verify commitment matches
      for I in Agg_Sig.Batch_Commit'Range loop
         if Agg_Sig.Batch_Commit (I) /= Commit_Hash (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Aggregated;

   function Verify_With_Keys (
      Agg_Sig        : Aggregated_Signature;
      PK_Root        : Byte_Array;
      Msg_Root       : Byte_Array
   ) return Boolean is
   begin
      --  Verify Merkle roots match
      for I in 0 .. 31 loop
         if Agg_Sig.Signer_Root (I) /= PK_Root (PK_Root'First + I) then
            return False;
         end if;
         if Agg_Sig.Message_Root (I) /= Msg_Root (Msg_Root'First + I) then
            return False;
         end if;
      end loop;

      return Verify_Aggregated (Agg_Sig);
   end Verify_With_Keys;

   procedure Batch_Verify_Aggregated (
      Agg_Sigs       : Aggregated_Signature;
      Results        : out Boolean
   ) is
   begin
      Results := Verify_Aggregated (Agg_Sigs);
   end Batch_Verify_Aggregated;

   ---------------------------------------------------------------------------
   --  Merkle Tree Operations
   ---------------------------------------------------------------------------

   procedure Build_Message_Tree (
      Batch          : Aggregation_Batch;
      Root           : out Byte_Array
   ) is
      Tree : array (0 .. Max_Sigs_Per_Batch * 2 - 1) of Byte_Array (0 .. 31) :=
         (others => (others => 0));
      Num_Leaves : Natural;
      Level_Size : Natural;
      Combined : Byte_Array (0 .. 63);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      Num_Leaves := Batch.Count;

      --  Hash message hashes to leaves (truncate to 32 bytes)
      for I in 0 .. Num_Leaves - 1 loop
         for J in 0 .. 31 loop
            Tree (I) (J) := Batch.Entries (I).Message_Hash (J);
         end loop;
      end loop;

      --  Build tree bottom-up
      Level_Size := Num_Leaves;
      while Level_Size > 1 loop
         for I in 0 .. (Level_Size + 1) / 2 - 1 loop
            --  Combine pair of nodes
            for J in 0 .. 31 loop
               Combined (J) := Tree (I * 2) (J);
               if I * 2 + 1 < Level_Size then
                  Combined (J + 32) := Tree (I * 2 + 1) (J);
               else
                  Combined (J + 32) := 0;
               end if;
            end loop;

            Anubis_SHA3.SHA3_256 (Combined, Hash_Out);

            for J in 0 .. 31 loop
               Tree (I) (J) := Hash_Out (J);
            end loop;
         end loop;

         Level_Size := (Level_Size + 1) / 2;
      end loop;

      for I in 0 .. 31 loop
         Root (Root'First + I) := Tree (0) (I);
      end loop;
   end Build_Message_Tree;

   procedure Build_Signer_Tree (
      Batch          : Aggregation_Batch;
      Root           : out Byte_Array
   ) is
      Leaf_Hash : Anubis_SHA3.SHA3_256_Digest;
      Tree : array (0 .. Max_Sigs_Per_Batch * 2 - 1) of Byte_Array (0 .. 31) :=
         (others => (others => 0));
      Num_Leaves : Natural;
      Level_Size : Natural;
      Combined : Byte_Array (0 .. 63);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      Num_Leaves := Batch.Count;

      --  Hash public keys to leaves
      for I in 0 .. Num_Leaves - 1 loop
         Anubis_SHA3.SHA3_256 (Batch.Entries (I).Signer_PK, Leaf_Hash);
         for J in 0 .. 31 loop
            Tree (I) (J) := Leaf_Hash (J);
         end loop;
      end loop;

      --  Build tree bottom-up
      Level_Size := Num_Leaves;
      while Level_Size > 1 loop
         for I in 0 .. (Level_Size + 1) / 2 - 1 loop
            for J in 0 .. 31 loop
               Combined (J) := Tree (I * 2) (J);
               if I * 2 + 1 < Level_Size then
                  Combined (J + 32) := Tree (I * 2 + 1) (J);
               else
                  Combined (J + 32) := 0;
               end if;
            end loop;

            Anubis_SHA3.SHA3_256 (Combined, Hash_Out);

            for J in 0 .. 31 loop
               Tree (I) (J) := Hash_Out (J);
            end loop;
         end loop;

         Level_Size := (Level_Size + 1) / 2;
      end loop;

      for I in 0 .. 31 loop
         Root (Root'First + I) := Tree (0) (I);
      end loop;
   end Build_Signer_Tree;

   procedure Generate_Inclusion_Proof (
      Batch          : Aggregation_Batch;
      Index          : Sig_Index;
      Proof          : out Byte_Array;
      Proof_Length   : out Natural
   ) is
      pragma Unreferenced (Batch);
      pragma Unreferenced (Index);
   begin
      --  Simplified implementation
      for I in Proof'Range loop
         Proof (I) := 0;
      end loop;
      Proof_Length := 0;
   end Generate_Inclusion_Proof;

   function Verify_Inclusion (
      Agg_Sig        : Aggregated_Signature;
      Message_Hash   : Byte_Array;
      Signer_PK      : Byte_Array;
      Proof          : Byte_Array
   ) return Boolean is
      pragma Unreferenced (Agg_Sig);
      pragma Unreferenced (Message_Hash);
      pragma Unreferenced (Signer_PK);
      pragma Unreferenced (Proof);
   begin
      --  Simplified implementation
      return True;
   end Verify_Inclusion;

   ---------------------------------------------------------------------------
   --  Homomorphic Operations
   ---------------------------------------------------------------------------

   procedure Homomorphic_Combine (
      Sig1           : Byte_Array;
      Sig2           : Byte_Array;
      Challenge      : Byte_Array;
      Combined       : out Byte_Array
   ) is
      pragma Unreferenced (Challenge);
   begin
      for I in Combined'Range loop
         Combined (I) := Sig1 (Sig1'First + I) xor Sig2 (Sig2'First + I);
      end loop;
   end Homomorphic_Combine;

   procedure Compute_Challenge (
      Batch_Commit   : Byte_Array;
      Domain_Sep     : Byte_Array;
      Challenge      : out Byte_Array
   ) is
      Input : Byte_Array (0 .. 95);
      Hash_Out : Anubis_SHA3.SHA3_512_Digest;
   begin
      for I in 0 .. 63 loop
         Input (I) := Batch_Commit (Batch_Commit'First + I);
      end loop;
      for I in 0 .. 31 loop
         Input (64 + I) := Domain_Sep (Domain_Sep'First + I);
      end loop;

      Anubis_SHA3.SHA3_512 (Input, Hash_Out);

      for I in Challenge'Range loop
         Challenge (I) := Hash_Out (I);
      end loop;
   end Compute_Challenge;

   procedure Compress_Aggregate (
      Combined       : Byte_Array;
      Num_Sigs       : Natural;
      Compressed     : out Byte_Array
   ) is
      pragma Unreferenced (Num_Sigs);
   begin
      for I in Compressed'Range loop
         if I <= Combined'Last - Combined'First then
            Compressed (I) := Combined (Combined'First + I);
         else
            Compressed (I) := 0;
         end if;
      end loop;
   end Compress_Aggregate;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize (
      Agg_Sig        : Aggregated_Signature;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Offset : Natural := 0;
   begin
      --  Header: num_sigs (4 bytes) + block_height (8 bytes)
      for I in 0 .. 3 loop
         Output (Output'First + Offset + I) := Byte ((Agg_Sig.Num_Sigs / (2 ** (I * 8))) mod 256);
      end loop;
      Offset := Offset + 4;

      for I in 0 .. 7 loop
         Output (Output'First + Offset + I) := Byte ((Agg_Sig.Block_Height / (2 ** (I * 8))) mod 256);
      end loop;
      Offset := Offset + 8;

      --  Roots (32 + 32 bytes)
      for I in Agg_Sig.Message_Root'Range loop
         Output (Output'First + Offset + I) := Agg_Sig.Message_Root (I);
      end loop;
      Offset := Offset + 32;

      for I in Agg_Sig.Signer_Root'Range loop
         Output (Output'First + Offset + I) := Agg_Sig.Signer_Root (I);
      end loop;
      Offset := Offset + 32;

      --  Batch commit (64 bytes)
      for I in Agg_Sig.Batch_Commit'Range loop
         Output (Output'First + Offset + I) := Agg_Sig.Batch_Commit (I);
      end loop;
      Offset := Offset + 64;

      --  Domain separator (32 bytes)
      for I in Agg_Sig.Domain_Sep'Range loop
         Output (Output'First + Offset + I) := Agg_Sig.Domain_Sep (I);
      end loop;
      Offset := Offset + 32;

      --  Aggregate (4096 bytes)
      for I in Agg_Sig.Aggregate'Range loop
         Output (Output'First + Offset + I) := Agg_Sig.Aggregate (I);
      end loop;
      Offset := Offset + Aggregated_Sig_Size;

      Length := Offset;
   end Serialize;

   procedure Deserialize (
      Input          : Byte_Array;
      Agg_Sig        : out Aggregated_Signature;
      Success        : out Boolean
   ) is
      Offset : Natural := 0;
   begin
      if Input'Length < 4 + 8 + 32 + 32 + 64 + 32 + Aggregated_Sig_Size then
         Success := False;
         return;
      end if;

      --  Parse header
      Agg_Sig.Num_Sigs := 0;
      for I in 0 .. 3 loop
         Agg_Sig.Num_Sigs := Agg_Sig.Num_Sigs +
            Natural (Input (Input'First + Offset + I)) * (2 ** (I * 8));
      end loop;
      Offset := Offset + 4;

      Agg_Sig.Block_Height := 0;
      for I in 0 .. 7 loop
         Agg_Sig.Block_Height := Agg_Sig.Block_Height +
            Unsigned_64 (Input (Input'First + Offset + I)) * (2 ** (I * 8));
      end loop;
      Offset := Offset + 8;

      --  Parse roots
      for I in Agg_Sig.Message_Root'Range loop
         Agg_Sig.Message_Root (I) := Input (Input'First + Offset + I);
      end loop;
      Offset := Offset + 32;

      for I in Agg_Sig.Signer_Root'Range loop
         Agg_Sig.Signer_Root (I) := Input (Input'First + Offset + I);
      end loop;
      Offset := Offset + 32;

      --  Parse batch commit
      for I in Agg_Sig.Batch_Commit'Range loop
         Agg_Sig.Batch_Commit (I) := Input (Input'First + Offset + I);
      end loop;
      Offset := Offset + 64;

      --  Parse domain separator
      for I in Agg_Sig.Domain_Sep'Range loop
         Agg_Sig.Domain_Sep (I) := Input (Input'First + Offset + I);
      end loop;
      Offset := Offset + 32;

      --  Parse aggregate
      for I in Agg_Sig.Aggregate'Range loop
         Agg_Sig.Aggregate (I) := Input (Input'First + Offset + I);
      end loop;

      Success := True;
   end Deserialize;

   ---------------------------------------------------------------------------
   --  Integration with MAAT
   ---------------------------------------------------------------------------

   procedure To_MAAT_Proof (
      Agg_Sig        : Aggregated_Signature;
      Proof_Hash     : out Byte_Array;
      FRI_Root       : out Byte_Array;
      Public_Inputs  : out Byte_Array
   ) is
      Hash_Input : Byte_Array (0 .. 95);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Proof hash = Hash(Aggregate[0..31] || Message_Root || Signer_Root)
      for I in 0 .. 31 loop
         Hash_Input (I) := Agg_Sig.Aggregate (I);
         Hash_Input (I + 32) := Agg_Sig.Message_Root (I);
         Hash_Input (I + 64) := Agg_Sig.Signer_Root (I);
      end loop;

      Anubis_SHA3.SHA3_256 (Hash_Input, Hash_Out);

      for I in 0 .. 31 loop
         Proof_Hash (Proof_Hash'First + I) := Hash_Out (I);
      end loop;

      --  FRI root (from aggregate)
      for I in 0 .. 31 loop
         FRI_Root (FRI_Root'First + I) := Agg_Sig.Aggregate (32 + I);
      end loop;

      --  Public inputs (header info)
      for I in 0 .. 3 loop
         Public_Inputs (Public_Inputs'First + I) := Byte ((Agg_Sig.Num_Sigs / (2 ** (I * 8))) mod 256);
      end loop;
      for I in 0 .. 7 loop
         Public_Inputs (Public_Inputs'First + 4 + I) := Byte ((Agg_Sig.Block_Height / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 12 .. Public_Inputs'Length - 1 loop
         if Public_Inputs'First + I <= Public_Inputs'Last then
            Public_Inputs (Public_Inputs'First + I) := 0;
         end if;
      end loop;
   end To_MAAT_Proof;

   ---------------------------------------------------------------------------
   --  Statistics and Metrics
   ---------------------------------------------------------------------------

   function Get_Stats (
      Batch          : Aggregation_Batch;
      Agg_Sig        : Aggregated_Signature
   ) return Aggregation_Stats is
      pragma Unreferenced (Agg_Sig);
      Stats : Aggregation_Stats;
   begin
      Stats.Num_Signatures := Batch.Count;
      Stats.Original_Size := Batch.Count * MLDSA_Sig_Size;
      Stats.Aggregated_Size := Aggregated_Sig_Size;
      if Stats.Original_Size > 0 then
         Stats.Compression_Ratio := 100 - (Stats.Aggregated_Size * 100 / Stats.Original_Size);
      else
         Stats.Compression_Ratio := 0;
      end if;
      Stats.Aggregation_Time := 0;  -- Would be measured
      return Stats;
   end Get_Stats;

   function Estimate_Compression (
      Num_Signatures : Natural
   ) return Natural is
      Original : Natural;
   begin
      Original := Num_Signatures * MLDSA_Sig_Size;
      if Original <= Aggregated_Sig_Size then
         return 0;
      end if;
      return 100 - (Aggregated_Sig_Size * 100 / Original);
   end Estimate_Compression;

   function Get_Parallel_Config (
      Num_Sigs       : Natural;
      Num_Cores      : Natural
   ) return Parallel_Config is
      Config : Parallel_Config;
   begin
      Config.Num_Threads := Num_Cores;
      if Num_Sigs <= Num_Cores then
         Config.Num_Micros := Num_Sigs;
         Config.Sigs_Per_Micro := 1;
      else
         Config.Num_Micros := Num_Cores;
         Config.Sigs_Per_Micro := (Num_Sigs + Num_Cores - 1) / Num_Cores;
      end if;
      return Config;
   end Get_Parallel_Config;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Batch (Batch : in Out Aggregation_Batch) is
   begin
      Batch.Count := 0;
      Batch.Block_Height := 0;
      Batch.Finalized := False;

      for I in Batch.Domain_Sep'Range loop
         Batch.Domain_Sep (I) := 0;
      end loop;

      for I in Batch.Entries'Range loop
         Batch.Entries (I).Verified := False;
         for J in Batch.Entries (I).Signer_PK'Range loop
            Batch.Entries (I).Signer_PK (J) := 0;
         end loop;
         for J in Batch.Entries (I).Message_Hash'Range loop
            Batch.Entries (I).Message_Hash (J) := 0;
         end loop;
         for J in Batch.Entries (I).Signature'Range loop
            Batch.Entries (I).Signature (J) := 0;
         end loop;
      end loop;
   end Zeroize_Batch;

   procedure Zeroize_Signature (Sig : in Out Aggregated_Signature) is
   begin
      Sig.Num_Sigs := 0;
      Sig.Block_Height := 0;

      for I in Sig.Aggregate'Range loop
         Sig.Aggregate (I) := 0;
      end loop;

      for I in Sig.Message_Root'Range loop
         Sig.Message_Root (I) := 0;
      end loop;

      for I in Sig.Signer_Root'Range loop
         Sig.Signer_Root (I) := 0;
      end loop;

      for I in Sig.Batch_Commit'Range loop
         Sig.Batch_Commit (I) := 0;
      end loop;

      for I in Sig.Domain_Sep'Range loop
         Sig.Domain_Sep (I) := 0;
      end loop;
   end Zeroize_Signature;

end Scarab_Khnum;
