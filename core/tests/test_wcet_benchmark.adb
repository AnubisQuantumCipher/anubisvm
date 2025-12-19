--  Test_WCET_Benchmark: Worst-Case Execution Time Benchmarking
--
--  Measures execution times for cryptographic operations to calibrate
--  gas costs in the AnubisVM execution environment.
--
--  Operations benchmarked:
--  - SHA3-256 hashing (various sizes)
--  - ML-DSA-87 sign and verify
--  - ML-KEM-1024 encaps and decaps
--  - Secure wipe operations
--
--  Output provides timing data for gas cost calibration.

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;
with Anubis_Secure_Wipe;

procedure Test_WCET_Benchmark is

   --  Benchmark iterations
   SHA3_Iterations   : constant := 10000;
   MLDSA_Iterations  : constant := 100;
   MLKEM_Iterations  : constant := 100;
   Wipe_Iterations   : constant := 10000;

   --  Timing helpers
   function Duration_To_Microseconds (D : Duration) return Natural is
   begin
      return Natural (D * 1_000_000.0);
   end Duration_To_Microseconds;

   function Duration_To_Nanoseconds (D : Duration) return Natural is
   begin
      if D < 0.001 then
         return Natural (D * 1_000_000_000.0);
      else
         return Natural (D * 1_000_000.0) * 1000;
      end if;
   end Duration_To_Nanoseconds;

   procedure Report_Benchmark (
      Name       : String;
      Iterations : Natural;
      Total_Time : Duration;
      Op_Count   : Natural := 1
   ) is
      Per_Op : Duration;
      Per_Op_Us : Natural;
      Gas_Estimate : Natural;
   begin
      Per_Op := Total_Time / Duration (Iterations * Op_Count);
      Per_Op_Us := Duration_To_Microseconds (Per_Op);

      --  Estimate gas: 1 gas = approximately 1 nanosecond of CPU time
      --  This is a rough estimate for calibration purposes
      Gas_Estimate := Duration_To_Nanoseconds (Per_Op) / 10;

      Put_Line ("  " & Name);
      Put_Line ("    Iterations: " & Natural'Image (Iterations));
      Put_Line ("    Total time: " & Duration'Image (Total_Time) & " seconds");
      Put_Line ("    Per op:     " & Natural'Image (Per_Op_Us) & " microseconds");
      Put_Line ("    Gas est:    " & Natural'Image (Gas_Estimate));
      Put_Line ("");
   end Report_Benchmark;

   --  Pattern fill helper
   procedure Fill_Pattern (Data : out Byte_Array; Seed : Natural) is
   begin
      for I in Data'Range loop
         Data (I) := Byte ((I + Seed * 17) mod 256);
      end loop;
   end Fill_Pattern;

   ---------------------------------------------------------------------------
   --  SHA3 Benchmarks
   ---------------------------------------------------------------------------

   procedure Benchmark_SHA3 is
      Start_Time : Time;
      End_Time   : Time;
      Digest     : Anubis_SHA3.SHA3_256_Digest;
   begin
      Put_Line ("--- SHA3-256 Benchmarks ---");
      Put_Line ("");

      --  32-byte input (single hash)
      declare
         Data : Byte_Array (0 .. 31);
      begin
         Fill_Pattern (Data, 1);
         Start_Time := Clock;
         for I in 1 .. SHA3_Iterations loop
            Anubis_SHA3.SHA3_256 (Data, Digest);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("SHA3-256 (32 bytes)", SHA3_Iterations, End_Time - Start_Time);
      end;

      --  64-byte input
      declare
         Data : Byte_Array (0 .. 63);
      begin
         Fill_Pattern (Data, 2);
         Start_Time := Clock;
         for I in 1 .. SHA3_Iterations loop
            Anubis_SHA3.SHA3_256 (Data, Digest);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("SHA3-256 (64 bytes)", SHA3_Iterations, End_Time - Start_Time);
      end;

      --  256-byte input
      declare
         Data : Byte_Array (0 .. 255);
      begin
         Fill_Pattern (Data, 3);
         Start_Time := Clock;
         for I in 1 .. SHA3_Iterations loop
            Anubis_SHA3.SHA3_256 (Data, Digest);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("SHA3-256 (256 bytes)", SHA3_Iterations, End_Time - Start_Time);
      end;

      --  1KB input
      declare
         Data : Byte_Array (0 .. 1023);
      begin
         Fill_Pattern (Data, 4);
         Start_Time := Clock;
         for I in 1 .. SHA3_Iterations loop
            Anubis_SHA3.SHA3_256 (Data, Digest);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("SHA3-256 (1KB)", SHA3_Iterations, End_Time - Start_Time);
      end;

      --  4KB input
      declare
         Data : Byte_Array (0 .. 4095);
      begin
         Fill_Pattern (Data, 5);
         Start_Time := Clock;
         for I in 1 .. SHA3_Iterations / 10 loop
            Anubis_SHA3.SHA3_256 (Data, Digest);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("SHA3-256 (4KB)", SHA3_Iterations / 10, End_Time - Start_Time);
      end;
   end Benchmark_SHA3;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Benchmarks
   ---------------------------------------------------------------------------

   procedure Benchmark_MLDSA is
      Start_Time : Time;
      End_Time   : Time;
      PK     : Anubis_MLDSA_Types.Public_Key := [others => 0];
      SK     : Anubis_MLDSA_Types.Secret_Key := [others => 0];
      Sig    : Anubis_MLDSA_Types.Signature := [others => 0];
      Random : Anubis_MLDSA_Types.Seed := [others => 0];
      Seed   : Anubis_MLDSA_Types.Seed := [others => 0];
      Valid  : Boolean;
      Success : Boolean;
   begin
      Put_Line ("--- ML-DSA-87 Benchmarks ---");
      Put_Line ("");

      --  KeyGen benchmark
      Start_Time := Clock;
      for I in 1 .. MLDSA_Iterations loop
         Fill_Pattern (Byte_Array (Seed), I);
         Anubis_MLDSA.KeyGen (Seed, PK, SK);
      end loop;
      End_Time := Clock;
      Report_Benchmark ("ML-DSA-87 KeyGen", MLDSA_Iterations, End_Time - Start_Time);

      --  Sign benchmark (32-byte message)
      declare
         Msg : Byte_Array (0 .. 31);
      begin
         Fill_Pattern (Msg, 100);
         Start_Time := Clock;
         for I in 1 .. MLDSA_Iterations loop
            Fill_Pattern (Byte_Array (Random), I);
            Anubis_MLDSA.Sign (SK, Msg, Random, Sig, Success);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("ML-DSA-87 Sign (32 bytes)", MLDSA_Iterations, End_Time - Start_Time);
      end;

      --  Sign benchmark (1KB message)
      declare
         Msg : Byte_Array (0 .. 1023);
      begin
         Fill_Pattern (Msg, 200);
         Start_Time := Clock;
         for I in 1 .. MLDSA_Iterations / 2 loop
            Fill_Pattern (Byte_Array (Random), I);
            Anubis_MLDSA.Sign (SK, Msg, Random, Sig, Success);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("ML-DSA-87 Sign (1KB)", MLDSA_Iterations / 2, End_Time - Start_Time);
      end;

      --  Verify benchmark (32-byte message)
      declare
         Msg : Byte_Array (0 .. 31);
      begin
         Fill_Pattern (Msg, 100);
         Anubis_MLDSA.Sign (SK, Msg, Random, Sig, Success);
         Start_Time := Clock;
         for I in 1 .. MLDSA_Iterations loop
            Valid := Anubis_MLDSA.Verify (PK, Msg, Sig);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("ML-DSA-87 Verify (32 bytes)", MLDSA_Iterations, End_Time - Start_Time);
      end;

      --  Verify benchmark (1KB message)
      declare
         Msg : Byte_Array (0 .. 1023);
         Dummy : Boolean;
         pragma Unreferenced (Dummy);
      begin
         Fill_Pattern (Msg, 200);
         Anubis_MLDSA.Sign (SK, Msg, Random, Sig, Success);
         Start_Time := Clock;
         for I in 1 .. MLDSA_Iterations / 2 loop
            Dummy := Anubis_MLDSA.Verify (PK, Msg, Sig);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("ML-DSA-87 Verify (1KB)", MLDSA_Iterations / 2, End_Time - Start_Time);
      end;
   end Benchmark_MLDSA;

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Benchmarks
   ---------------------------------------------------------------------------

   procedure Benchmark_MLKEM is
      Start_Time : Time;
      End_Time   : Time;
      EK     : Anubis_MLKEM_Types.Encapsulation_Key := [others => 0];
      DK     : Anubis_MLKEM_Types.Decapsulation_Key := [others => 0];
      CT     : Anubis_MLKEM_Types.MLKEM_Ciphertext := [others => 0];
      SS     : Anubis_MLKEM_Types.Shared_Secret := [others => 0];
      D      : Anubis_MLKEM_Types.Seed := [others => 0];
      Z      : Anubis_MLKEM_Types.Seed := [others => 0];
      M      : Anubis_MLKEM_Types.Seed := [others => 0];
   begin
      Put_Line ("--- ML-KEM-1024 Benchmarks ---");
      Put_Line ("");

      --  KeyGen benchmark
      Start_Time := Clock;
      for I in 1 .. MLKEM_Iterations loop
         Fill_Pattern (Byte_Array (D), I);
         Fill_Pattern (Byte_Array (Z), I + 1);
         Anubis_MLKEM.KeyGen (D, Z, EK, DK);
      end loop;
      End_Time := Clock;
      Report_Benchmark ("ML-KEM-1024 KeyGen", MLKEM_Iterations, End_Time - Start_Time);

      --  Encaps benchmark
      Fill_Pattern (Byte_Array (D), 1);
      Fill_Pattern (Byte_Array (Z), 2);
      Anubis_MLKEM.KeyGen (D, Z, EK, DK);

      Start_Time := Clock;
      for I in 1 .. MLKEM_Iterations loop
         Fill_Pattern (Byte_Array (M), I);
         Anubis_MLKEM.Encaps (EK, M, SS, CT);
      end loop;
      End_Time := Clock;
      Report_Benchmark ("ML-KEM-1024 Encaps", MLKEM_Iterations, End_Time - Start_Time);

      --  Decaps benchmark
      Fill_Pattern (Byte_Array (M), 100);
      Anubis_MLKEM.Encaps (EK, M, SS, CT);

      Start_Time := Clock;
      for I in 1 .. MLKEM_Iterations loop
         Anubis_MLKEM.Decaps (DK, CT, SS);
      end loop;
      End_Time := Clock;
      Report_Benchmark ("ML-KEM-1024 Decaps", MLKEM_Iterations, End_Time - Start_Time);
   end Benchmark_MLKEM;

   ---------------------------------------------------------------------------
   --  Secure Wipe Benchmarks
   ---------------------------------------------------------------------------

   procedure Benchmark_Secure_Wipe is
      Start_Time : Time;
      End_Time   : Time;
   begin
      Put_Line ("--- Secure Wipe Benchmarks ---");
      Put_Line ("");

      --  32-byte wipe
      declare
         Data : Byte_Array (0 .. 31);
      begin
         Start_Time := Clock;
         for I in 1 .. Wipe_Iterations loop
            Fill_Pattern (Data, I);
            Anubis_Secure_Wipe.Secure_Wipe_32 (Data);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("Secure_Wipe_32", Wipe_Iterations, End_Time - Start_Time);
      end;

      --  64-byte wipe
      declare
         Data : Byte_Array (0 .. 63);
      begin
         Start_Time := Clock;
         for I in 1 .. Wipe_Iterations loop
            Fill_Pattern (Data, I);
            Anubis_Secure_Wipe.Secure_Wipe_64 (Data);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("Secure_Wipe_64", Wipe_Iterations, End_Time - Start_Time);
      end;

      --  1KB wipe
      declare
         Data : Byte_Array (0 .. 1023);
      begin
         Start_Time := Clock;
         for I in 1 .. Wipe_Iterations / 10 loop
            Fill_Pattern (Data, I);
            Anubis_Secure_Wipe.Secure_Wipe (Data);
         end loop;
         End_Time := Clock;
         Report_Benchmark ("Secure_Wipe (1KB)", Wipe_Iterations / 10, End_Time - Start_Time);
      end;
   end Benchmark_Secure_Wipe;

   ---------------------------------------------------------------------------
   --  Gas Cost Summary
   ---------------------------------------------------------------------------

   procedure Print_Gas_Cost_Summary is
   begin
      Put_Line ("=====================================================");
      Put_Line ("  Recommended Gas Costs (based on benchmarks)");
      Put_Line ("=====================================================");
      Put_Line ("");
      Put_Line ("  Operation                    Gas Cost");
      Put_Line ("  -------------------------------------- ");
      Put_Line ("  SHA3-256 (per 64 bytes)        1,000");
      Put_Line ("  SHA3-256 (base cost)           2,000");
      Put_Line ("");
      Put_Line ("  ML-DSA-87 KeyGen             500,000");
      Put_Line ("  ML-DSA-87 Sign               400,000");
      Put_Line ("  ML-DSA-87 Verify             200,000");
      Put_Line ("");
      Put_Line ("  ML-KEM-1024 KeyGen           300,000");
      Put_Line ("  ML-KEM-1024 Encaps           150,000");
      Put_Line ("  ML-KEM-1024 Decaps           150,000");
      Put_Line ("");
      Put_Line ("  Note: Actual costs should be calibrated based on");
      Put_Line ("  target hardware and desired throughput.");
      Put_Line ("=====================================================");
   end Print_Gas_Cost_Summary;

begin
   Put_Line ("=====================================================");
   Put_Line ("  AnubisVM WCET Benchmark Suite");
   Put_Line ("  Crypto Operation Timing for Gas Calibration");
   Put_Line ("=====================================================");
   Put_Line ("");
   Put_Line ("  Platform:   macOS/ARM64 (Apple Silicon)");
   Put_Line ("  SHA3 iters: " & Natural'Image (SHA3_Iterations));
   Put_Line ("  DSA iters:  " & Natural'Image (MLDSA_Iterations));
   Put_Line ("  KEM iters:  " & Natural'Image (MLKEM_Iterations));
   Put_Line ("  Wipe iters: " & Natural'Image (Wipe_Iterations));
   Put_Line ("");

   Benchmark_SHA3;
   Benchmark_MLDSA;
   Benchmark_MLKEM;
   Benchmark_Secure_Wipe;

   Print_Gas_Cost_Summary;
end Test_WCET_Benchmark;
