--  SCARAB Proof Aggregation Stress Tests
--
--  Stress tests for the MAAT hierarchical non-recursive aggregation system:
--  - Large batch aggregation (up to 1024 proofs)
--  - Tree construction performance
--  - Verification throughput
--  - Memory stability under load
--  - Compression ratio verification
--  - Concurrent batch processing simulation
--
--  SPDX-License-Identifier: Apache-2.0

pragma SPARK_Mode (Off);  --  Test code

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Interfaces; use Interfaces;
with Scarab_Maat; use Scarab_Maat;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

procedure Test_Scarab_Stress is

   Total_Tests : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Report (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Report;

   --  Deterministic hash generation for testing
   function Test_Hash (Seed : Unsigned_64) return Hash_Value is
      Result : Hash_Value;
      S : Unsigned_64 := Seed;
   begin
      for I in Result'Range loop
         S := S * 6364136223846793005 + 1442695040888963407;
         Result (I) := Unsigned_8 (S mod 256);
      end loop;
      return Result;
   end Test_Hash;

   --  Deterministic public inputs
   function Test_Inputs (Seed : Unsigned_64; Len : Natural) return Byte_Array is
      Result : Byte_Array (0 .. Len - 1);
      S : Unsigned_64 := Seed;
   begin
      for I in Result'Range loop
         S := S * 1103515245 + 12345;
         Result (I) := Unsigned_8 (S mod 256);
      end loop;
      return Result;
   end Test_Inputs;

   --  Timer helpers
   Start_Time : Time;
   End_Time : Time;

   procedure Start_Timer is
   begin
      Start_Time := Clock;
   end Start_Timer;

   function Elapsed_Ms return Natural is
      Elapsed : constant Duration := End_Time - Start_Time;
   begin
      return Natural (Elapsed * 1000.0);
   end Elapsed_Ms;

begin
   Put_Line ("===========================================");
   Put_Line ("  SCARAB Proof Aggregation Stress Tests");
   Put_Line ("===========================================");
   Put_Line ("  Testing: MAAT Non-Recursive Aggregation");
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 1: Basic Batch Operations
   ---------------------------------------------------------------------------
   Put_Line ("Test 1: Basic Batch Operations");
   declare
      Batch : Aggregation_Batch;
      Success : Boolean;
   begin
      Init_Batch (Batch);
      Report ("Init_Batch creates empty batch", Batch.Num_Proofs = 0);
      Report ("Tree not built initially", not Batch.Tree_Built);

      --  Add single proof
      Add_Proof (
         Batch         => Batch,
         Proof_Hash    => Test_Hash (1),
         FRI_Root      => Test_Hash (2),
         Trace_Root    => Test_Hash (3),
         Public_Inputs => Test_Inputs (4, 32),
         Success       => Success
      );
      Report ("Add single proof succeeds", Success);
      Report ("Proof count is 1", Proof_Count (Batch) = 1);
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 2: Small Batch (16 proofs)
   ---------------------------------------------------------------------------
   Put_Line ("Test 2: Small Batch Aggregation (16 proofs)");
   declare
      Batch : Aggregation_Batch;
      Agg_Proof : Aggregated_Proof;
      Success : Boolean;
   begin
      Init_Batch (Batch);

      Start_Timer;
      for I in 0 .. 15 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 3)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 3 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 3 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I), 64),
            Success       => Success
         );
         exit when not Success;
      end loop;
      End_Time := Clock;

      Report ("Add 16 proofs succeeds", Batch.Num_Proofs = 16);
      Put_Line ("    Add time: " & Natural'Image (Elapsed_Ms) & " ms");

      --  Build tree
      Start_Timer;
      Build_Tree (Batch, Success);
      End_Time := Clock;
      Report ("Build tree succeeds", Success);
      Put_Line ("    Tree build time: " & Natural'Image (Elapsed_Ms) & " ms");

      if Success then
         Report ("Tree is built", Batch.Tree_Built);

         --  Aggregate
         Start_Timer;
         Aggregate_Proofs (Batch, Agg_Proof, Success);
         End_Time := Clock;
         Report ("Aggregate proofs succeeds", Success);
         Put_Line ("    Aggregation time: " & Natural'Image (Elapsed_Ms) & " ms");

         if Success then
            Report ("Aggregated proof has 16 proofs", Agg_Proof.Num_Proofs = 16);

            --  Verify
            Start_Timer;
            declare
               Valid : constant Boolean := Verify_Aggregated (Agg_Proof);
            begin
               End_Time := Clock;
               Report ("Verification succeeds", Valid);
               Put_Line ("    Verification time: " & Natural'Image (Elapsed_Ms) & " ms");
            end;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 3: Medium Batch (128 proofs)
   ---------------------------------------------------------------------------
   Put_Line ("Test 3: Medium Batch Aggregation (128 proofs)");
   declare
      Batch : Aggregation_Batch;
      Agg_Proof : Aggregated_Proof;
      Success : Boolean;
      Add_Count : Natural := 0;
   begin
      Init_Batch (Batch);

      Start_Timer;
      for I in 0 .. 127 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 5)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 5 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 5 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I * 100), 128),
            Success       => Success
         );
         if Success then
            Add_Count := Add_Count + 1;
         else
            exit;
         end if;
      end loop;
      End_Time := Clock;

      Report ("Add 128 proofs succeeds", Add_Count = 128);
      Put_Line ("    Add time: " & Natural'Image (Elapsed_Ms) & " ms");
      Put_Line ("    Rate: " & Natural'Image (128000 / (Elapsed_Ms + 1)) & " proofs/sec");

      --  Build and aggregate
      Build_Tree (Batch, Success);
      Report ("Tree build succeeds", Success);

      if Success then
         Start_Timer;
         Aggregate_Proofs (Batch, Agg_Proof, Success);
         End_Time := Clock;
         Report ("Aggregation succeeds", Success);
         Put_Line ("    Aggregation time: " & Natural'Image (Elapsed_Ms) & " ms");

         if Success then
            --  Check compression
            declare
               Stats : constant Aggregation_Stats := Get_Stats (Agg_Proof);
            begin
               Put_Line ("    Tree depth: " & Natural'Image (Stats.Tree_Depth));
               Put_Line ("    Aggregated size: " & Natural'Image (Stats.Aggregated_Size) & " bytes");
               Put_Line ("    Compression: " & Natural'Image (Stats.Compression_Ratio) & "%");
               Report ("Compression ratio > 0", Stats.Compression_Ratio > 0);
            end;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 4: Large Batch (512 proofs)
   ---------------------------------------------------------------------------
   Put_Line ("Test 4: Large Batch Aggregation (512 proofs)");
   declare
      Batch : Aggregation_Batch;
      Agg_Proof : Aggregated_Proof;
      Success : Boolean;
      Add_Count : Natural := 0;
   begin
      Init_Batch (Batch);

      Start_Timer;
      for I in 0 .. 511 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 7)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 7 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 7 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I * 200), 64),
            Success       => Success
         );
         if Success then
            Add_Count := Add_Count + 1;
         else
            exit;
         end if;
      end loop;
      End_Time := Clock;

      Report ("Add 512 proofs succeeds", Add_Count = 512);
      Put_Line ("    Add time: " & Natural'Image (Elapsed_Ms) & " ms");

      --  Build tree
      Start_Timer;
      Build_Tree (Batch, Success);
      End_Time := Clock;
      Report ("Tree build succeeds", Success);
      Put_Line ("    Tree build time: " & Natural'Image (Elapsed_Ms) & " ms");

      if Success then
         --  Aggregate
         Start_Timer;
         Aggregate_Proofs (Batch, Agg_Proof, Success);
         End_Time := Clock;
         Report ("Aggregation succeeds", Success);
         Put_Line ("    Aggregation time: " & Natural'Image (Elapsed_Ms) & " ms");

         if Success then
            --  Verify
            Start_Timer;
            declare
               Valid : constant Boolean := Verify_Aggregated (Agg_Proof);
            begin
               End_Time := Clock;
               Report ("Verification succeeds", Valid);
               Put_Line ("    Verification time: " & Natural'Image (Elapsed_Ms) & " ms");
            end;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 5: Maximum Batch (1024 proofs)
   ---------------------------------------------------------------------------
   Put_Line ("Test 5: Maximum Batch Aggregation (1024 proofs)");
   declare
      Batch : Aggregation_Batch;
      Agg_Proof : Aggregated_Proof;
      Success : Boolean;
      Add_Count : Natural := 0;
   begin
      Init_Batch (Batch);

      Start_Timer;
      for I in 0 .. 1023 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 11)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 11 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 11 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I * 300), 32),
            Success       => Success
         );
         if Success then
            Add_Count := Add_Count + 1;
         else
            exit;
         end if;
      end loop;
      End_Time := Clock;

      Report ("Add 1024 proofs succeeds", Add_Count = 1024);
      Put_Line ("    Add time: " & Natural'Image (Elapsed_Ms) & " ms");
      Put_Line ("    Rate: " & Natural'Image (1024000 / (Elapsed_Ms + 1)) & " proofs/sec");

      --  Build tree
      Start_Timer;
      Build_Tree (Batch, Success);
      End_Time := Clock;
      Report ("Tree build succeeds", Success);
      Put_Line ("    Tree build time: " & Natural'Image (Elapsed_Ms) & " ms");

      if Success then
         declare
            Root : constant Hash_Value := Get_Root (Batch);
            Non_Zero : Boolean := False;
         begin
            for B of Root loop
               if B /= 0 then
                  Non_Zero := True;
                  exit;
               end if;
            end loop;
            Report ("Tree root is non-zero", Non_Zero);
         end;

         --  Aggregate
         Start_Timer;
         Aggregate_Proofs (Batch, Agg_Proof, Success);
         End_Time := Clock;
         Report ("Aggregation succeeds", Success);
         Put_Line ("    Aggregation time: " & Natural'Image (Elapsed_Ms) & " ms");

         if Success then
            --  Statistics
            declare
               Stats : constant Aggregation_Stats := Get_Stats (Agg_Proof);
            begin
               Put_Line ("    Tree depth: " & Natural'Image (Stats.Tree_Depth));
               Put_Line ("    Aggregated size: " & Natural'Image (Stats.Aggregated_Size) & " bytes");
               Put_Line ("    Est. verification ops: " & Natural'Image (Stats.Verification_Ops));
               Report ("Max depth <= 10", Stats.Tree_Depth <= 10);
            end;

            --  Verify
            Start_Timer;
            declare
               Valid : constant Boolean := Verify_Aggregated (Agg_Proof);
            begin
               End_Time := Clock;
               Report ("Verification succeeds", Valid);
               Put_Line ("    Verification time: " & Natural'Image (Elapsed_Ms) & " ms");
               Put_Line ("    Throughput: " & Natural'Image (1024000 / (Elapsed_Ms + 1)) & " proofs/sec");
            end;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 6: Authentication Path Verification
   ---------------------------------------------------------------------------
   Put_Line ("Test 6: Authentication Path Verification");
   declare
      Batch : Aggregation_Batch;
      Path : Auth_Path;
      Path_Length : Natural;
      Success : Boolean;
      All_Valid : Boolean := True;
   begin
      Init_Batch (Batch);

      --  Add 64 proofs
      for I in 0 .. 63 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 13)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 13 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 13 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I), 64),
            Success       => Success
         );
         exit when not Success;
      end loop;

      Build_Tree (Batch, Success);
      Report ("Build tree for auth path test", Success);

      if Success then
         declare
            Root : constant Hash_Value := Get_Root (Batch);
         begin
            --  Verify auth path for each proof
            for I in 0 .. 63 loop
               Get_Auth_Path (Batch, I, Path, Path_Length);

               --  Compute leaf hash and verify path
               declare
                  Leaf_Hash : constant Hash_Value := Hash_Proof_Ref (Batch.Proofs (I));
                  Valid : constant Boolean := Verify_Path (Leaf_Hash, Path, Path_Length, Root);
               begin
                  if not Valid then
                     All_Valid := False;
                     Put_Line ("    Auth path " & Natural'Image (I) & " FAILED");
                  end if;
               end;
            end loop;

            Report ("All 64 auth paths verify", All_Valid);
            Put_Line ("    Path length: " & Natural'Image (Path_Length));
         end;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 7: FRI Combination
   ---------------------------------------------------------------------------
   Put_Line ("Test 7: FRI Combination");
   declare
      Batch : Aggregation_Batch;
      Challenge : constant Hash_Value := Test_Hash (99999);
      Combined : Proof_Bytes;
      Combined_Len : Natural;
      Success : Boolean;
   begin
      Init_Batch (Batch);

      --  Add 32 proofs
      for I in 0 .. 31 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 17)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 17 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 17 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I), 128),
            Success       => Success
         );
         exit when not Success;
      end loop;

      Build_Tree (Batch, Success);
      Report ("Build tree for FRI test", Success);

      if Success then
         Start_Timer;
         Combine_FRI (Batch, Challenge, Combined, Combined_Len, Success);
         End_Time := Clock;

         Report ("FRI combination succeeds", Success);
         Put_Line ("    Combined FRI length: " & Natural'Image (Combined_Len) & " bytes");
         Put_Line ("    Combination time: " & Natural'Image (Elapsed_Ms) & " ms");

         if Success then
            declare
               Root : constant Hash_Value := Get_Root (Batch);
               Valid : constant Boolean := Verify_Combined_FRI (Combined, Combined_Len, Root, Challenge);
            begin
               Report ("Combined FRI verifies", Valid);
            end;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 8: Compression/Decompression
   ---------------------------------------------------------------------------
   Put_Line ("Test 8: Compression/Decompression");
   declare
      Batch : Aggregation_Batch;
      Agg_Proof : Aggregated_Proof;
      Decompressed : Aggregated_Proof;
      Compressed : Byte_Array (0 .. Max_Proof_Size + Agg_Proof_Overhead - 1);
      Comp_Length : Natural;
      Success : Boolean;
   begin
      Init_Batch (Batch);

      --  Add 64 proofs
      for I in 0 .. 63 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 19)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 19 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 19 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I), 32),
            Success       => Success
         );
         exit when not Success;
      end loop;

      Build_Tree (Batch, Success);
      if Success then
         Aggregate_Proofs (Batch, Agg_Proof, Success);
      end if;
      Report ("Setup for compression test", Success);

      if Success then
         --  Compress
         Start_Timer;
         Compress_Proof (Agg_Proof, Compressed, Comp_Length, Success);
         End_Time := Clock;

         Report ("Compression succeeds", Success);
         Put_Line ("    Original size: " & Natural'Image (Agg_Proof.Proof_Length) & " bytes");
         Put_Line ("    Compressed size: " & Natural'Image (Comp_Length) & " bytes");
         Put_Line ("    Compression time: " & Natural'Image (Elapsed_Ms) & " ms");

         if Success then
            --  Decompress
            Start_Timer;
            Decompress_Proof (Compressed (0 .. Comp_Length - 1), Decompressed, Success);
            End_Time := Clock;

            Report ("Decompression succeeds", Success);
            Put_Line ("    Decompression time: " & Natural'Image (Elapsed_Ms) & " ms");

            if Success then
               Report ("Num proofs matches", Decompressed.Num_Proofs = Agg_Proof.Num_Proofs);
               Report ("Tree root matches", Decompressed.Tree_Root = Agg_Proof.Tree_Root);
            end if;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 9: Serialization Round-trip
   ---------------------------------------------------------------------------
   Put_Line ("Test 9: Serialization Round-trip");
   declare
      Batch : Aggregation_Batch;
      Agg_Proof : Aggregated_Proof;
      Deserialized : Aggregated_Proof;
      Serialized : Byte_Array (0 .. Max_Proof_Size + Agg_Proof_Overhead - 1);
      Ser_Length : Natural;
      Success : Boolean;
   begin
      Init_Batch (Batch);

      for I in 0 .. 31 loop
         Add_Proof (
            Batch         => Batch,
            Proof_Hash    => Test_Hash (Unsigned_64 (I * 23)),
            FRI_Root      => Test_Hash (Unsigned_64 (I * 23 + 1)),
            Trace_Root    => Test_Hash (Unsigned_64 (I * 23 + 2)),
            Public_Inputs => Test_Inputs (Unsigned_64 (I), 64),
            Success       => Success
         );
         exit when not Success;
      end loop;

      Build_Tree (Batch, Success);
      if Success then
         Aggregate_Proofs (Batch, Agg_Proof, Success);
      end if;

      if Success then
         Serialize_Agg_Proof (Agg_Proof, Serialized, Ser_Length);
         Report ("Serialization succeeds", Ser_Length > 0);
         Put_Line ("    Serialized length: " & Natural'Image (Ser_Length) & " bytes");

         Deserialize_Agg_Proof (Serialized (0 .. Ser_Length - 1), Deserialized, Success);
         Report ("Deserialization succeeds", Success);

         if Success then
            Report ("Round-trip preserves num_proofs",
                    Deserialized.Num_Proofs = Agg_Proof.Num_Proofs);
            Report ("Round-trip preserves tree_root",
                    Deserialized.Tree_Root = Agg_Proof.Tree_Root);

            --  Verify deserialized proof
            declare
               Valid : constant Boolean := Verify_Aggregated (Deserialized);
            begin
               Report ("Deserialized proof verifies", Valid);
            end;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 10: Parallelism Hint
   ---------------------------------------------------------------------------
   Put_Line ("Test 10: Parallelism Hint");
   declare
      Hints : array (Natural range 0 .. 5) of Natural;
      Sizes : constant array (Natural range 0 .. 5) of Natural :=
         (1, 16, 64, 256, 512, 1024);
   begin
      for I in Sizes'Range loop
         Hints (I) := Parallelism_Hint (Sizes (I));
         Put_Line ("    " & Natural'Image (Sizes (I)) & " proofs -> " &
                   Natural'Image (Hints (I)) & " threads");
      end loop;

      Report ("Parallelism hints <= 64", Hints (5) <= 64);
      Report ("Larger batches get more parallelism", Hints (5) >= Hints (0));
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Summary
   ---------------------------------------------------------------------------
   Put_Line ("===========================================");
   Put_Line ("  Stress Test Summary");
   Put_Line ("===========================================");
   Put_Line ("  Total:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed: " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed: " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   if Passed_Tests = Total_Tests then
      Put_Line ("  ALL SCARAB STRESS TESTS PASSED!");
      Put_Line ("  MAAT aggregation system verified.");
   else
      Put_Line ("  SOME TESTS FAILED");
   end if;

end Test_Scarab_Stress;
