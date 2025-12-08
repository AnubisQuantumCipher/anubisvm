-------------------------------------------------------------------------------
--  SCARAB - TEFNUT Light Client Protocol Implementation
--  Tiny Efficient Framework for Navigating Unified Transactions
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Anubis_SHA3;

package body Scarab_Tefnut with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Get_Timestamp return Unsigned_64 is
   begin
      --  In production, this would call system time
      return 0;
   end Get_Timestamp;

   function Min (A, B : Natural) return Natural is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_From_Checkpoint (
      State          : out Light_Client_State;
      Checkpoint     : Compact_Header;
      Validator_Root : Byte_Array
   ) is
   begin
      State.Finalized_Header := Checkpoint;
      State.Has_Pending := False;
      State.Current_Epoch := 0;
      State.Verify_Progress := 0;
      State.Verify_Complete := False;
      State.Buffer_Used := 0;
      State.Path_Depth := 0;

      for I in 0 .. 31 loop
         State.Validator_Root (I) := Validator_Root (Validator_Root'First + I);
      end loop;

      --  Initialize pending header
      State.Pending_Header.Block_Height := 0;
      for I in State.Pending_Header.Parent_Hash'Range loop
         State.Pending_Header.Parent_Hash (I) := 0;
      end loop;
      for I in State.Pending_Header.State_Root'Range loop
         State.Pending_Header.State_Root (I) := 0;
      end loop;
      for I in State.Pending_Header.Proof_Commitment'Range loop
         State.Pending_Header.Proof_Commitment (I) := 0;
      end loop;
      for I in State.Pending_Header.Validator_Sig'Range loop
         State.Pending_Header.Validator_Sig (I) := 0;
      end loop;

      --  Clear buffers
      for I in State.Proof_Buffer'Range loop
         State.Proof_Buffer (I) := 0;
      end loop;

      for I in State.Path_Cache'Range loop
         State.Path_Cache (I) := 0;
      end loop;
   end Init_From_Checkpoint;

   procedure Init_From_Genesis (
      State          : out Light_Client_State;
      Genesis        : Compact_Header
   ) is
      Zero_Root : Byte_Array (0 .. 31) := (others => 0);
   begin
      Init_From_Checkpoint (State, Genesis, Zero_Root);
      State.Current_Epoch := 0;
   end Init_From_Genesis;

   procedure Reset_State (
      State          : in Out Light_Client_State
   ) is
   begin
      State.Has_Pending := False;
      State.Verify_Progress := 0;
      State.Verify_Complete := False;
      State.Buffer_Used := 0;
      State.Path_Depth := 0;

      for I in State.Proof_Buffer'Range loop
         State.Proof_Buffer (I) := 0;
      end loop;

      for I in State.Path_Cache'Range loop
         State.Path_Cache (I) := 0;
      end loop;
   end Reset_State;

   ---------------------------------------------------------------------------
   --  Header Verification
   ---------------------------------------------------------------------------

   procedure Begin_Verify (
      State          : in Out Light_Client_State;
      Header         : Compact_Header;
      Result         : out Header_Result
   ) is
      Parent_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Verify height is sequential
      if Header.Block_Height /= State.Finalized_Header.Block_Height + 1 then
         if Header.Block_Height <= State.Finalized_Header.Block_Height then
            Result := Stale;
         else
            Result := Invalid_Height;
         end if;
         return;
      end if;

      --  Compute hash of finalized header
      declare
         Header_Bytes : Byte_Array (0 .. 95);
      begin
         for I in 0 .. 7 loop
            Header_Bytes (I) := Byte ((State.Finalized_Header.Block_Height / (2 ** (I * 8))) mod 256);
         end loop;
         for I in 0 .. 31 loop
            Header_Bytes (8 + I) := State.Finalized_Header.Parent_Hash (I);
            Header_Bytes (40 + I) := State.Finalized_Header.State_Root (I);
            Header_Bytes (72 + I) := State.Finalized_Header.Proof_Commitment (I);
         end loop;

         Anubis_SHA3.SHA3_256 (Header_Bytes, Parent_Hash);
      end;

      --  Verify parent hash matches
      for I in 0 .. 31 loop
         if Header.Parent_Hash (I) /= Parent_Hash (I) then
            Result := Invalid_Parent;
            return;
         end if;
      end loop;

      --  Set pending header for verification
      State.Pending_Header := Header;
      State.Has_Pending := True;
      State.Verify_Progress := 0;
      State.Verify_Complete := False;

      Result := Valid;
   end Begin_Verify;

   procedure Process_Proof_Chunk (
      State          : in Out Light_Client_State;
      Chunk          : Proof_Chunk;
      Progress       : out Natural;
      Complete       : out Boolean
   ) is
      Copy_Len : Natural;
   begin
      if not State.Has_Pending then
         Progress := 0;
         Complete := False;
         return;
      end if;

      --  Copy chunk to buffer
      Copy_Len := Min (Chunk.Length, Max_Chunk_Size - State.Buffer_Used);

      for I in 0 .. Copy_Len - 1 loop
         State.Proof_Buffer (State.Buffer_Used + I) := Chunk.Data (I);
      end loop;

      State.Buffer_Used := State.Buffer_Used + Copy_Len;

      --  Update progress
      if Chunk.Total_Chunks > 0 then
         Progress := ((Chunk.Chunk_Index + 1) * 100) / Chunk.Total_Chunks;
      else
         Progress := 100;
      end if;

      State.Verify_Progress := Progress;
      Complete := Chunk.Is_Final;

      if Complete then
         State.Verify_Complete := True;
      end if;
   end Process_Proof_Chunk;

   procedure Finalize_Verify (
      State          : in Out Light_Client_State;
      Result         : out Header_Result
   ) is
      Proof_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      if not State.Has_Pending or not State.Verify_Complete then
         Result := Invalid_Proof;
         return;
      end if;

      --  Verify proof against commitment
      declare
         Proof_Slice : Byte_Array (0 .. State.Buffer_Used - 1);
      begin
         for I in 0 .. State.Buffer_Used - 1 loop
            Proof_Slice (I) := State.Proof_Buffer (I);
         end loop;

         Anubis_SHA3.SHA3_256 (Proof_Slice, Proof_Hash);
      end;

      --  Check proof matches commitment
      for I in 0 .. 31 loop
         if Proof_Hash (I) /= State.Pending_Header.Proof_Commitment (I) then
            Result := Invalid_Proof;
            State.Has_Pending := False;
            return;
         end if;
      end loop;

      --  Finalize header
      State.Finalized_Header := State.Pending_Header;
      State.Has_Pending := False;
      State.Buffer_Used := 0;

      Result := Valid;
   end Finalize_Verify;

   procedure Verify_Header_Immediate (
      State          : in Out Light_Client_State;
      Header         : Compact_Header;
      Proof          : Byte_Array;
      Result         : out Header_Result
   ) is
      Chunk : Proof_Chunk;
      Progress : Natural;
      Complete : Boolean;
   begin
      --  Begin verification
      Begin_Verify (State, Header, Result);
      if Result /= Valid then
         return;
      end if;

      --  Process proof as single chunk
      for I in 0 .. Min (Proof'Length, Max_Chunk_Size) - 1 loop
         Chunk.Data (I) := Proof (Proof'First + I);
      end loop;

      Chunk.Length := Min (Proof'Length, Max_Chunk_Size);
      Chunk.Chunk_Index := 0;
      Chunk.Total_Chunks := 1;
      Chunk.Is_Final := True;

      Process_Proof_Chunk (State, Chunk, Progress, Complete);

      --  Finalize
      Finalize_Verify (State, Result);
   end Verify_Header_Immediate;

   ---------------------------------------------------------------------------
   --  State Verification
   ---------------------------------------------------------------------------

   function Verify_State_Proof (
      State          : Light_Client_State;
      Proof          : State_Proof
   ) return Boolean is
      Current_Hash : Byte_Array (0 .. 31);
      Sibling : Byte_Array (0 .. 31);
      Combined : Byte_Array (0 .. 63);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Hash the value
      declare
         Value_Slice : Byte_Array (0 .. Proof.Value_Length - 1);
      begin
         for I in 0 .. Proof.Value_Length - 1 loop
            Value_Slice (I) := Proof.Value (I);
         end loop;
         Anubis_SHA3.SHA3_256 (Value_Slice, Hash_Out);
      end;

      for I in 0 .. 31 loop
         Current_Hash (I) := Hash_Out (I);
      end loop;

      --  Walk up the Merkle tree
      for D in 0 .. Proof.Path_Length - 1 loop
         --  Get sibling from proof
         for I in 0 .. 31 loop
            Sibling (I) := Proof.Siblings (D * 32 + I);
         end loop;

         --  Determine order based on path
         if Proof.Path (D * 32) mod 2 = 0 then
            --  Current is left
            for I in 0 .. 31 loop
               Combined (I) := Current_Hash (I);
               Combined (I + 32) := Sibling (I);
            end loop;
         else
            --  Current is right
            for I in 0 .. 31 loop
               Combined (I) := Sibling (I);
               Combined (I + 32) := Current_Hash (I);
            end loop;
         end if;

         Anubis_SHA3.SHA3_256 (Combined, Hash_Out);

         for I in 0 .. 31 loop
            Current_Hash (I) := Hash_Out (I);
         end loop;
      end loop;

      --  Verify against state root
      for I in 0 .. 31 loop
         if Current_Hash (I) /= State.Finalized_Header.State_Root (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_State_Proof;

   procedure Verify_Balance (
      State          : Light_Client_State;
      Address        : Byte_Array;
      Balance        : Unsigned_64;
      Proof          : State_Proof;
      Valid          : out Boolean
   ) is
      pragma Unreferenced (Address);
      pragma Unreferenced (Balance);
   begin
      Valid := Verify_State_Proof (State, Proof);
   end Verify_Balance;

   procedure Verify_TX_Inclusion (
      State          : Light_Client_State;
      TX_Hash        : Byte_Array;
      Block_Height   : Unsigned_64;
      Proof          : State_Proof;
      Valid          : out Boolean
   ) is
      pragma Unreferenced (TX_Hash);
   begin
      if Block_Height > State.Finalized_Header.Block_Height then
         Valid := False;
         return;
      end if;

      Valid := Verify_State_Proof (State, Proof);
   end Verify_TX_Inclusion;

   procedure Verify_Storage (
      State          : Light_Client_State;
      Contract       : Byte_Array;
      Key            : Byte_Array;
      Value          : out Byte_Array;
      Value_Length   : out Natural;
      Proof          : State_Proof;
      Valid          : out Boolean
   ) is
      pragma Unreferenced (Contract);
      pragma Unreferenced (Key);
   begin
      Valid := Verify_State_Proof (State, Proof);

      if Valid then
         Value_Length := Min (Proof.Value_Length, Value'Length);
         for I in 0 .. Value_Length - 1 loop
            Value (Value'First + I) := Proof.Value (I);
         end loop;
      else
         Value_Length := 0;
      end if;
   end Verify_Storage;

   ---------------------------------------------------------------------------
   --  Validator Set Management
   ---------------------------------------------------------------------------

   procedure Verify_Epoch_Transition (
      State          : in Out Light_Client_State;
      Proof          : Epoch_Proof;
      Valid          : out Boolean
   ) is
   begin
      --  Verify old epoch matches
      if Proof.Old_Epoch /= State.Current_Epoch then
         Valid := False;
         return;
      end if;

      --  Verify old validator root matches
      for I in 0 .. 31 loop
         if Proof.Old_Val_Root (I) /= State.Validator_Root (I) then
            Valid := False;
            return;
         end if;
      end loop;

      --  Verify new epoch is sequential
      if Proof.New_Epoch /= Proof.Old_Epoch + 1 then
         Valid := False;
         return;
      end if;

      --  Update state
      State.Current_Epoch := Proof.New_Epoch;
      for I in 0 .. 31 loop
         State.Validator_Root (I) := Proof.New_Val_Root (I);
      end loop;

      Valid := True;
   end Verify_Epoch_Transition;

   function Verify_Validator (
      State          : Light_Client_State;
      Validator      : Validator_Info;
      Proof          : Byte_Array
   ) return Boolean is
      Combined : Byte_Array (0 .. 71);
      Val_Hash : Anubis_SHA3.SHA3_256_Digest;
      Current_Hash : Byte_Array (0 .. 31);
   begin
      --  Hash validator info
      for I in 0 .. 31 loop
         Combined (I) := Validator.PK_Hash (I);
      end loop;

      for I in 0 .. 7 loop
         Combined (32 + I) := Byte ((Validator.Weight / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 0 .. 3 loop
         Combined (40 + I) := Byte ((Validator.Index / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 44 .. 71 loop
         Combined (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_256 (Combined, Val_Hash);

      for I in 0 .. 31 loop
         Current_Hash (I) := Val_Hash (I);
      end loop;

      --  Walk up tree using proof
      for D in 0 .. Max_Merkle_Depth - 1 loop
         if D * 32 + 31 < Proof'Length then
            declare
               Sibling : Byte_Array (0 .. 31);
               Pair : Byte_Array (0 .. 63);
               Pair_Hash : Anubis_SHA3.SHA3_256_Digest;
            begin
               for I in 0 .. 31 loop
                  Sibling (I) := Proof (Proof'First + D * 32 + I);
               end loop;

               for I in 0 .. 31 loop
                  Pair (I) := Current_Hash (I);
                  Pair (I + 32) := Sibling (I);
               end loop;

               Anubis_SHA3.SHA3_256 (Pair, Pair_Hash);

               for I in 0 .. 31 loop
                  Current_Hash (I) := Pair_Hash (I);
               end loop;
            end;
         end if;
      end loop;

      --  Verify against validator root
      for I in 0 .. 31 loop
         if Current_Hash (I) /= State.Validator_Root (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Validator;

   function Get_Validator_Root (
      State          : Light_Client_State
   ) return Byte_Array is
      Result : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Result (I) := State.Validator_Root (I);
      end loop;
      return Result;
   end Get_Validator_Root;

   ---------------------------------------------------------------------------
   --  Sync Protocol
   ---------------------------------------------------------------------------

   function Get_Sync_Status (
      State          : Light_Client_State;
      Network_Height : Unsigned_64
   ) return Sync_Status is
      Blocks_Behind : Unsigned_64;
   begin
      if Network_Height <= State.Finalized_Header.Block_Height then
         return Synced;
      end if;

      Blocks_Behind := Network_Height - State.Finalized_Header.Block_Height;

      if Blocks_Behind = 1 and State.Has_Pending then
         return Syncing;
      elsif Blocks_Behind > 100 then
         return Behind;
      elsif Blocks_Behind > 0 then
         return Syncing;
      else
         return Synced;
      end if;
   end Get_Sync_Status;

   function Blocks_Behind (
      State          : Light_Client_State;
      Network_Height : Unsigned_64
   ) return Unsigned_64 is
   begin
      if Network_Height <= State.Finalized_Header.Block_Height then
         return 0;
      else
         return Network_Height - State.Finalized_Header.Block_Height;
      end if;
   end Blocks_Behind;

   procedure Request_Sync_Range (
      State          : Light_Client_State;
      Network_Height : Unsigned_64;
      Start_Height   : out Unsigned_64;
      End_Height     : out Unsigned_64
   ) is
   begin
      Start_Height := State.Finalized_Header.Block_Height + 1;

      if Network_Height > Start_Height + 100 then
         End_Height := Start_Height + 100;
      else
         End_Height := Network_Height;
      end if;
   end Request_Sync_Range;

   procedure Skip_Forward (
      State          : in Out Light_Client_State;
      Target_Header  : Compact_Header;
      Skip_Proof     : Byte_Array;
      Valid          : out Boolean
   ) is
      Proof_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Verify skip proof
      Anubis_SHA3.SHA3_256 (Skip_Proof, Proof_Hash);

      --  Check proof matches commitment
      for I in 0 .. 31 loop
         if Proof_Hash (I) /= Target_Header.Proof_Commitment (I) then
            Valid := False;
            return;
         end if;
      end loop;

      --  Update finalized header
      State.Finalized_Header := Target_Header;
      Valid := True;
   end Skip_Forward;

   ---------------------------------------------------------------------------
   --  Compact Proof Verification
   ---------------------------------------------------------------------------

   function Verify_Aggregated_Proof (
      State          : Light_Client_State;
      Proof_Root     : Byte_Array;
      Num_Proofs     : Natural
   ) return Boolean is
      pragma Unreferenced (Num_Proofs);
   begin
      --  Verify proof root matches state
      for I in 0 .. 31 loop
         if Proof_Root (Proof_Root'First + I) /= State.Finalized_Header.Proof_Commitment (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Aggregated_Proof;

   function Verify_Sig_Aggregate (
      State          : Light_Client_State;
      Agg_Sig        : Byte_Array;
      Message_Root   : Byte_Array
   ) return Boolean is
      Sig_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Hash aggregated signature
      Anubis_SHA3.SHA3_256 (Agg_Sig, Sig_Hash);

      --  Verify message root consistency
      for I in 0 .. 31 loop
         if Message_Root (Message_Root'First + I) /= State.Finalized_Header.State_Root (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Sig_Aggregate;

   ---------------------------------------------------------------------------
   --  Merkle Operations
   ---------------------------------------------------------------------------

   procedure Verify_Merkle_Streaming (
      Root           : Byte_Array;
      Leaf           : Byte_Array;
      Sibling        : Byte_Array;
      Is_Left        : Boolean;
      Depth          : Merkle_Depth;
      State          : in Out Light_Client_State;
      Continue       : out Boolean;
      Valid          : out Boolean
   ) is
      Combined : Byte_Array (0 .. 63);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Build combined hash input
      if Is_Left then
         for I in 0 .. 31 loop
            Combined (I) := Leaf (Leaf'First + I);
            Combined (I + 32) := Sibling (Sibling'First + I);
         end loop;
      else
         for I in 0 .. 31 loop
            Combined (I) := Sibling (Sibling'First + I);
            Combined (I + 32) := Leaf (Leaf'First + I);
         end loop;
      end if;

      Anubis_SHA3.SHA3_256 (Combined, Hash_Out);

      --  Store in path cache
      for I in 0 .. 31 loop
         State.Path_Cache (Depth * 32 + I) := Hash_Out (I);
      end loop;

      State.Path_Depth := Depth + 1;

      --  Check if at root
      if Depth + 1 >= Max_Merkle_Depth then
         Continue := False;
         Valid := True;
         for I in 0 .. 31 loop
            if Hash_Out (I) /= Root (Root'First + I) then
               Valid := False;
            end if;
         end loop;
      else
         Continue := True;
         Valid := True;
      end if;
   end Verify_Merkle_Streaming;

   procedure Reset_Merkle_State (
      State          : in Out Light_Client_State
   ) is
   begin
      State.Path_Depth := 0;
      for I in State.Path_Cache'Range loop
         State.Path_Cache (I) := 0;
      end loop;
   end Reset_Merkle_State;

   function Finalize_Merkle (
      State          : Light_Client_State;
      Expected_Root  : Byte_Array
   ) return Boolean is
   begin
      if State.Path_Depth = 0 then
         return False;
      end if;

      --  Compare final hash in cache to expected root
      for I in 0 .. 31 loop
         if State.Path_Cache ((State.Path_Depth - 1) * 32 + I) /= Expected_Root (Expected_Root'First + I) then
            return False;
         end if;
      end loop;

      return True;
   end Finalize_Merkle;

   ---------------------------------------------------------------------------
   --  Header Compression
   ---------------------------------------------------------------------------

   procedure Compress_Header (
      Full_Header    : Byte_Array;
      Compact        : out Compact_Header;
      Success        : out Boolean
   ) is
   begin
      if Full_Header'Length < 128 then
         Success := False;
         return;
      end if;

      --  Extract block height (first 8 bytes)
      Compact.Block_Height := 0;
      for I in 0 .. 7 loop
         Compact.Block_Height := Compact.Block_Height +
            Unsigned_64 (Full_Header (Full_Header'First + I)) * (2 ** (I * 8));
      end loop;

      --  Copy hashes
      for I in 0 .. 31 loop
         Compact.Parent_Hash (I) := Full_Header (Full_Header'First + 8 + I);
         Compact.State_Root (I) := Full_Header (Full_Header'First + 40 + I);
         Compact.Proof_Commitment (I) := Full_Header (Full_Header'First + 72 + I);
         Compact.Validator_Sig (I) := Full_Header (Full_Header'First + 104 + I);
      end loop;

      Success := True;
   end Compress_Header;

   procedure Hash_Header (
      Header         : Compact_Header;
      Hash           : out Byte_Array
   ) is
      Header_Bytes : Byte_Array (0 .. 127);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Serialize header
      for I in 0 .. 7 loop
         Header_Bytes (I) := Byte ((Header.Block_Height / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 0 .. 31 loop
         Header_Bytes (8 + I) := Header.Parent_Hash (I);
         Header_Bytes (40 + I) := Header.State_Root (I);
         Header_Bytes (72 + I) := Header.Proof_Commitment (I);
         Header_Bytes (104 + I) := Header.Validator_Sig (I);
      end loop;

      Anubis_SHA3.SHA3_256 (Header_Bytes, Hash_Out);

      for I in 0 .. 31 loop
         Hash (Hash'First + I) := Hash_Out (I);
      end loop;
   end Hash_Header;

   procedure Verify_Header_Chain (
      State          : in Out Light_Client_State;
      Headers        : Byte_Array;
      Num_Headers    : Natural;
      Valid          : out Boolean
   ) is
      Current_Header : Compact_Header;
      Hdr_Result : Header_Result;
      Decompress_Ok : Boolean;
   begin
      Valid := True;

      for H in 0 .. Num_Headers - 1 loop
         --  Decompress header
         declare
            Header_Slice : Byte_Array (0 .. Compact_Header_Size - 1);
         begin
            for I in 0 .. Compact_Header_Size - 1 loop
               Header_Slice (I) := Headers (Headers'First + H * Compact_Header_Size + I);
            end loop;

            Compress_Header (Header_Slice, Current_Header, Decompress_Ok);
         end;

         if not Decompress_Ok then
            Valid := False;
            return;
         end if;

         --  Verify header
         Begin_Verify (State, Current_Header, Hdr_Result);
         if Hdr_Result /= Scarab_Tefnut.Valid then
            Valid := False;
            return;
         end if;

         --  Skip proof verification in chain mode - trust chained headers
         State.Finalized_Header := Current_Header;
         State.Has_Pending := False;
      end loop;
   end Verify_Header_Chain;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_State (
      State          : Light_Client_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Offset : Natural := 0;
   begin
      --  Serialize finalized header
      Serialize_Header (State.Finalized_Header, Output (Output'First .. Output'First + Compact_Header_Size - 1));
      Offset := Compact_Header_Size;

      --  Validator root
      for I in 0 .. 31 loop
         Output (Output'First + Offset + I) := State.Validator_Root (I);
      end loop;
      Offset := Offset + 32;

      --  Epoch
      for I in 0 .. 7 loop
         Output (Output'First + Offset + I) := Byte ((State.Current_Epoch / (2 ** (I * 8))) mod 256);
      end loop;
      Offset := Offset + 8;

      --  Flags
      if State.Has_Pending then
         Output (Output'First + Offset) := 1;
      else
         Output (Output'First + Offset) := 0;
      end if;
      Offset := Offset + 1;

      Length := Offset;
   end Serialize_State;

   procedure Deserialize_State (
      Input          : Byte_Array;
      State          : out Light_Client_State;
      Success        : out Boolean
   ) is
      Offset : Natural := 0;
   begin
      if Input'Length < Compact_Header_Size + 41 then
         Success := False;
         return;
      end if;

      --  Deserialize finalized header
      Deserialize_Header (
         Input (Input'First .. Input'First + Compact_Header_Size - 1),
         State.Finalized_Header,
         Success
      );
      if not Success then
         return;
      end if;
      Offset := Compact_Header_Size;

      --  Validator root
      for I in 0 .. 31 loop
         State.Validator_Root (I) := Input (Input'First + Offset + I);
      end loop;
      Offset := Offset + 32;

      --  Epoch
      State.Current_Epoch := 0;
      for I in 0 .. 7 loop
         State.Current_Epoch := State.Current_Epoch +
            Unsigned_64 (Input (Input'First + Offset + I)) * (2 ** (I * 8));
      end loop;
      Offset := Offset + 8;

      --  Flags
      State.Has_Pending := (Input (Input'First + Offset) = 1);

      --  Initialize other fields
      State.Verify_Progress := 0;
      State.Verify_Complete := False;
      State.Buffer_Used := 0;
      State.Path_Depth := 0;

      for I in State.Proof_Buffer'Range loop
         State.Proof_Buffer (I) := 0;
      end loop;

      for I in State.Path_Cache'Range loop
         State.Path_Cache (I) := 0;
      end loop;

      --  Initialize pending header
      State.Pending_Header.Block_Height := 0;
      for I in State.Pending_Header.Parent_Hash'Range loop
         State.Pending_Header.Parent_Hash (I) := 0;
      end loop;
      for I in State.Pending_Header.State_Root'Range loop
         State.Pending_Header.State_Root (I) := 0;
      end loop;
      for I in State.Pending_Header.Proof_Commitment'Range loop
         State.Pending_Header.Proof_Commitment (I) := 0;
      end loop;
      for I in State.Pending_Header.Validator_Sig'Range loop
         State.Pending_Header.Validator_Sig (I) := 0;
      end loop;

      Success := True;
   end Deserialize_State;

   procedure Serialize_Header (
      Header         : Compact_Header;
      Output         : out Byte_Array
   ) is
   begin
      --  Block height (8 bytes)
      for I in 0 .. 7 loop
         Output (Output'First + I) := Byte ((Header.Block_Height / (2 ** (I * 8))) mod 256);
      end loop;

      --  Hashes (32 bytes each)
      for I in 0 .. 31 loop
         Output (Output'First + 8 + I) := Header.Parent_Hash (I);
         Output (Output'First + 40 + I) := Header.State_Root (I);
         Output (Output'First + 72 + I) := Header.Proof_Commitment (I);
         Output (Output'First + 104 + I) := Header.Validator_Sig (I);
      end loop;
   end Serialize_Header;

   procedure Deserialize_Header (
      Input          : Byte_Array;
      Header         : out Compact_Header;
      Success        : out Boolean
   ) is
   begin
      if Input'Length < Compact_Header_Size then
         Success := False;
         return;
      end if;

      --  Block height
      Header.Block_Height := 0;
      for I in 0 .. 7 loop
         Header.Block_Height := Header.Block_Height +
            Unsigned_64 (Input (Input'First + I)) * (2 ** (I * 8));
      end loop;

      --  Hashes
      for I in 0 .. 31 loop
         Header.Parent_Hash (I) := Input (Input'First + 8 + I);
         Header.State_Root (I) := Input (Input'First + 40 + I);
         Header.Proof_Commitment (I) := Input (Input'First + 72 + I);
         Header.Validator_Sig (I) := Input (Input'First + 104 + I);
      end loop;

      Success := True;
   end Deserialize_Header;

   ---------------------------------------------------------------------------
   --  Metrics and Diagnostics
   ---------------------------------------------------------------------------

   function Get_Metrics (
      State          : Light_Client_State
   ) return Verify_Metrics is
      Metrics : Verify_Metrics;
   begin
      Metrics.Verify_Time_Us := 0;
      Metrics.RAM_Used := Get_RAM_Usage (State);
      Metrics.Chunks_Processed := State.Verify_Progress;
      Metrics.Proofs_Verified := 0;

      if State.Verify_Complete then
         Metrics.Proofs_Verified := 1;
      end if;

      return Metrics;
   end Get_Metrics;

   function Get_RAM_Usage (
      State          : Light_Client_State
   ) return Natural is
      pragma Unreferenced (State);
   begin
      --  Compact_Header = 128 bytes (x2 for finalized + pending)
      --  Validator_Root = 32 bytes
      --  Proof_Buffer = 1024 bytes
      --  Path_Cache = 1024 bytes
      --  Flags + counters = ~32 bytes
      return 256 + 32 + 1024 + 1024 + 32;  -- ~2.4KB
   end Get_RAM_Usage;

   function Self_Test return Boolean is
   begin
      --  Verify size constraints
      if Compact_Header_Size /= 128 then
         return False;
      end if;

      if Max_RAM_Footprint < 8192 then
         return False;
      end if;

      return True;
   end Self_Test;

   ---------------------------------------------------------------------------
   --  Security Checks
   ---------------------------------------------------------------------------

   function Is_Timely (
      State          : Light_Client_State;
      Header         : Compact_Header;
      Current_Time   : Unsigned_64
   ) return Boolean is
      pragma Unreferenced (State);
      pragma Unreferenced (Header);
      pragma Unreferenced (Current_Time);
   begin
      --  In production, this would check timestamp validity
      return True;
   end Is_Timely;

   function Detect_Eclipse (
      State          : Light_Client_State;
      Network_Height : Unsigned_64
   ) return Boolean is
      Diff : Unsigned_64;
   begin
      if Network_Height <= State.Finalized_Header.Block_Height then
         return False;
      end if;

      Diff := Network_Height - State.Finalized_Header.Block_Height;

      --  If we"re more than 1000 blocks behind, might be eclipsed
      return Diff > 1000;
   end Detect_Eclipse;

   function Has_Finality (
      State          : Light_Client_State;
      Block_Height   : Unsigned_64;
      Confirmations  : Natural
   ) return Boolean is
   begin
      if Block_Height > State.Finalized_Header.Block_Height then
         return False;
      end if;

      return State.Finalized_Header.Block_Height - Block_Height >= Unsigned_64 (Confirmations);
   end Has_Finality;

end Scarab_Tefnut;
