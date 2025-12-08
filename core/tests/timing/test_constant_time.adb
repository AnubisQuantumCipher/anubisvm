-------------------------------------------------------------------------------
--  Constant-Time Verification Test Harness
--
--  This test suite verifies that cryptographic operations execute in
--  constant time, regardless of input values. It uses statistical analysis
--  (Welch's t-test) to detect timing variations.
--
--  Methodology:
--  1. Execute operation with "class A" inputs (e.g., all zeros)
--  2. Execute operation with "class B" inputs (e.g., random values)
--  3. Compare timing distributions using Welch's t-test
--  4. Fail if p-value < 0.05 (indicates timing leak)
--
--  Reference: "A Practical Methodology for Measuring the Side-Channel
--  Resistance of In-Silicon Implementations" - Reparaz et al.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Real_Time;       use Ada.Real_Time;
with Ada.Numerics.Float_Random;
with Interfaces;          use Interfaces;
with System;

procedure Test_Constant_Time is

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------

   --  Number of iterations per test
   Iterations : constant := 100_000;

   --  Minimum iterations for warm-up
   Warmup_Iterations : constant := 10_000;

   --  Significance threshold (p-value)
   Alpha : constant Float := 0.05;

   --  T-test threshold for constant-time claim (2-sided, df=large)
   --  For df > 100, critical value at alpha=0.05 is ~1.96
   T_Critical : constant Float := 1.96;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Byte is mod 256;
   type Byte_Array is array (Natural range <>) of Byte;

   type Timing_Array is array (1 .. Iterations) of Long_Float;

   type Test_Result is record
      Name        : String (1 .. 40);
      Name_Len    : Natural;
      T_Statistic : Float;
      P_Value     : Float;
      Mean_A      : Long_Float;
      Mean_B      : Long_Float;
      Std_A       : Long_Float;
      Std_B       : Long_Float;
      Passed      : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Statistical Functions
   ---------------------------------------------------------------------------

   function Mean (Arr : Timing_Array) return Long_Float is
      Sum : Long_Float := 0.0;
   begin
      for I in Arr'Range loop
         Sum := Sum + Arr (I);
      end loop;
      return Sum / Long_Float (Arr'Length);
   end Mean;

   function Variance (Arr : Timing_Array; M : Long_Float) return Long_Float is
      Sum_Sq : Long_Float := 0.0;
      Diff   : Long_Float;
   begin
      for I in Arr'Range loop
         Diff := Arr (I) - M;
         Sum_Sq := Sum_Sq + Diff * Diff;
      end loop;
      return Sum_Sq / Long_Float (Arr'Length - 1);
   end Variance;

   function Std_Dev (Arr : Timing_Array; M : Long_Float) return Long_Float is
      V : constant Long_Float := Variance (Arr, M);
   begin
      if V > 0.0 then
         return Long_Float'Max (V ** 0.5, 0.0);
      else
         return 0.0;
      end if;
   end Std_Dev;

   --  Welch's t-test (unequal variances)
   function Welch_T_Test (
      A, B   : Timing_Array;
      T_Stat : out Float;
      P_Val  : out Float
   ) return Boolean is
      Mean_A  : constant Long_Float := Mean (A);
      Mean_B  : constant Long_Float := Mean (B);
      Var_A   : constant Long_Float := Variance (A, Mean_A);
      Var_B   : constant Long_Float := Variance (B, Mean_B);
      N_A     : constant Long_Float := Long_Float (A'Length);
      N_B     : constant Long_Float := Long_Float (B'Length);
      SE      : Long_Float;
      T       : Long_Float;
   begin
      --  Standard error of the difference
      SE := ((Var_A / N_A) + (Var_B / N_B)) ** 0.5;

      if SE > 0.0 then
         T := (Mean_A - Mean_B) / SE;
         T_Stat := Float (abs (T));
         --  Approximate p-value for large df (normal approximation)
         --  For |t| > 1.96, p < 0.05
         if T_Stat > T_Critical then
            P_Val := 0.01;  -- Significant difference
            return False;
         else
            P_Val := 0.5;   -- No significant difference
            return True;
         end if;
      else
         T_Stat := 0.0;
         P_Val := 1.0;
         return True;  -- No variance = constant time
      end if;
   end Welch_T_Test;

   ---------------------------------------------------------------------------
   --  Timing Measurement
   ---------------------------------------------------------------------------

   function Get_Time_Ns return Long_Float is
      Now : constant Time := Clock;
      Ts  : constant Time_Span := Now - Time_First;
      Ns  : constant Long_Integer := Long_Integer (To_Duration (Ts) * 1_000_000_000.0);
   begin
      return Long_Float (Ns);
   end Get_Time_Ns;

   ---------------------------------------------------------------------------
   --  Test Functions to Verify
   ---------------------------------------------------------------------------

   --  Constant-time byte comparison (correct implementation)
   function CT_Compare (A, B : Byte_Array) return Boolean is
      Diff : Byte := 0;
   begin
      pragma Assert (A'Length = B'Length);
      for I in A'Range loop
         Diff := Diff or (A (I) xor B (A'First + (I - A'First)));
      end loop;
      return Diff = 0;
   end CT_Compare;

   --  Non-constant-time comparison (intentionally bad - for testing)
   function Bad_Compare (A, B : Byte_Array) return Boolean is
   begin
      pragma Assert (A'Length = B'Length);
      for I in A'Range loop
         if A (I) /= B (A'First + (I - A'First)) then
            return False;  -- Early exit - timing leak!
         end if;
      end loop;
      return True;
   end Bad_Compare;

   --  Constant-time select (correct implementation)
   function CT_Select (Cond : Boolean; A, B : Byte) return Byte is
      Mask : Byte;
   begin
      if Cond then
         Mask := 16#FF#;
      else
         Mask := 16#00#;
      end if;
      return (A and Mask) or (B and (not Mask));
   end CT_Select;

   --  Non-constant-time select (bad - for testing)
   function Bad_Select (Cond : Boolean; A, B : Byte) return Byte is
   begin
      if Cond then
         return A;
      else
         return B;
      end if;
   end Bad_Select;

   --  Constant-time array copy (correct)
   procedure CT_Copy (Src : Byte_Array; Dst : out Byte_Array) is
   begin
      pragma Assert (Src'Length = Dst'Length);
      for I in 0 .. Src'Length - 1 loop
         Dst (Dst'First + I) := Src (Src'First + I);
      end loop;
   end CT_Copy;

   --  Constant-time conditional swap (correct)
   procedure CT_Swap (Cond : Boolean; A, B : in out Byte) is
      Mask : Byte;
      Diff : Byte;
   begin
      if Cond then
         Mask := 16#FF#;
      else
         Mask := 16#00#;
      end if;
      Diff := (A xor B) and Mask;
      A := A xor Diff;
      B := B xor Diff;
   end CT_Swap;

   ---------------------------------------------------------------------------
   --  Test Procedures
   ---------------------------------------------------------------------------

   procedure Run_Compare_Test (
      Name       : String;
      Is_CT      : Boolean;
      Result     : out Test_Result
   ) is
      Times_A : Timing_Array;
      Times_B : Timing_Array;
      A_Data  : Byte_Array (0 .. 31) := (others => 0);
      B_Data  : Byte_Array (0 .. 31) := (others => 0);
      B_Rand  : Byte_Array (0 .. 31);
      Dummy   : Boolean;
      T_Start : Long_Float;
      T_End   : Long_Float;
      Gen     : Ada.Numerics.Float_Random.Generator;
   begin
      --  Initialize result
      Result.Name_Len := Natural'Min (Name'Length, 40);
      Result.Name (1 .. Result.Name_Len) := Name (Name'First .. Name'First + Result.Name_Len - 1);
      for I in Result.Name_Len + 1 .. 40 loop
         Result.Name (I) := ' ';
      end loop;

      Ada.Numerics.Float_Random.Reset (Gen);

      --  Generate random data for class B
      for I in B_Rand'Range loop
         B_Rand (I) := Byte (Integer (Ada.Numerics.Float_Random.Random (Gen) * 255.0));
      end loop;

      --  Warm-up phase
      for I in 1 .. Warmup_Iterations loop
         if Is_CT then
            Dummy := CT_Compare (A_Data, B_Data);
         else
            Dummy := Bad_Compare (A_Data, B_Data);
         end if;
      end loop;

      --  Class A: Compare identical data (all zeros)
      for I in Times_A'Range loop
         T_Start := Get_Time_Ns;
         if Is_CT then
            Dummy := CT_Compare (A_Data, B_Data);
         else
            Dummy := Bad_Compare (A_Data, B_Data);
         end if;
         T_End := Get_Time_Ns;
         Times_A (I) := T_End - T_Start;
      end loop;

      --  Class B: Compare data that differs at position 0 (early)
      B_Data (0) := 16#FF#;
      for I in Times_B'Range loop
         T_Start := Get_Time_Ns;
         if Is_CT then
            Dummy := CT_Compare (A_Data, B_Data);
         else
            Dummy := Bad_Compare (A_Data, B_Data);
         end if;
         T_End := Get_Time_Ns;
         Times_B (I) := T_End - T_Start;
      end loop;

      --  Statistical analysis
      Result.Mean_A := Mean (Times_A);
      Result.Mean_B := Mean (Times_B);
      Result.Std_A := Std_Dev (Times_A, Result.Mean_A);
      Result.Std_B := Std_Dev (Times_B, Result.Mean_B);

      Result.Passed := Welch_T_Test (
         Times_A, Times_B,
         Result.T_Statistic, Result.P_Value
      );
   end Run_Compare_Test;

   procedure Run_Select_Test (
      Name       : String;
      Is_CT      : Boolean;
      Result     : out Test_Result
   ) is
      Times_A : Timing_Array;
      Times_B : Timing_Array;
      Dummy   : Byte;
      T_Start : Long_Float;
      T_End   : Long_Float;
   begin
      --  Initialize result
      Result.Name_Len := Natural'Min (Name'Length, 40);
      Result.Name (1 .. Result.Name_Len) := Name (Name'First .. Name'First + Result.Name_Len - 1);
      for I in Result.Name_Len + 1 .. 40 loop
         Result.Name (I) := ' ';
      end loop;

      --  Warm-up
      for I in 1 .. Warmup_Iterations loop
         if Is_CT then
            Dummy := CT_Select (True, 16#AA#, 16#55#);
         else
            Dummy := Bad_Select (True, 16#AA#, 16#55#);
         end if;
      end loop;

      --  Class A: Condition = True
      for I in Times_A'Range loop
         T_Start := Get_Time_Ns;
         if Is_CT then
            Dummy := CT_Select (True, 16#AA#, 16#55#);
         else
            Dummy := Bad_Select (True, 16#AA#, 16#55#);
         end if;
         T_End := Get_Time_Ns;
         Times_A (I) := T_End - T_Start;
      end loop;

      --  Class B: Condition = False
      for I in Times_B'Range loop
         T_Start := Get_Time_Ns;
         if Is_CT then
            Dummy := CT_Select (False, 16#AA#, 16#55#);
         else
            Dummy := Bad_Select (False, 16#AA#, 16#55#);
         end if;
         T_End := Get_Time_Ns;
         Times_B (I) := T_End - T_Start;
      end loop;

      --  Statistical analysis
      Result.Mean_A := Mean (Times_A);
      Result.Mean_B := Mean (Times_B);
      Result.Std_A := Std_Dev (Times_A, Result.Mean_A);
      Result.Std_B := Std_Dev (Times_B, Result.Mean_B);

      Result.Passed := Welch_T_Test (
         Times_A, Times_B,
         Result.T_Statistic, Result.P_Value
      );
   end Run_Select_Test;

   ---------------------------------------------------------------------------
   --  Main Test Runner
   ---------------------------------------------------------------------------

   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;
   Result       : Test_Result;

   procedure Print_Result (R : Test_Result) is
   begin
      Put (R.Name (1 .. R.Name_Len));
      for I in R.Name_Len + 1 .. 35 loop
         Put (' ');
      end loop;

      if R.Passed then
         Put ("PASS");
         Passed_Tests := Passed_Tests + 1;
      else
         Put ("FAIL");
         Failed_Tests := Failed_Tests + 1;
      end if;

      Put ("  t=");
      Put (R.T_Statistic, Fore => 1, Aft => 3, Exp => 0);

      Put ("  mean_a=");
      Put (Float (R.Mean_A), Fore => 1, Aft => 1, Exp => 0);

      Put ("  mean_b=");
      Put (Float (R.Mean_B), Fore => 1, Aft => 1, Exp => 0);

      New_Line;
      Total_Tests := Total_Tests + 1;
   end Print_Result;

begin
   Put_Line ("===============================================================");
   Put_Line ("       AegisVM Constant-Time Verification Test Suite           ");
   Put_Line ("===============================================================");
   Put_Line ("");
   Put_Line ("Methodology: Welch's t-test on timing distributions");
   Put_Line ("Iterations:  " & Natural'Image (Iterations));
   Put_Line ("Threshold:   t > " & Float'Image (T_Critical) & " => timing leak");
   Put_Line ("");
   Put_Line ("---------------------------------------------------------------");
   Put_Line ("Test Name                           Result  Stats");
   Put_Line ("---------------------------------------------------------------");

   --  Test 1: Constant-time comparison (should PASS)
   Run_Compare_Test ("CT_Compare (should pass)", True, Result);
   Print_Result (Result);

   --  Test 2: Non-constant-time comparison (should FAIL)
   Run_Compare_Test ("Bad_Compare (should fail)", False, Result);
   Print_Result (Result);

   --  Test 3: Constant-time select (should PASS)
   Run_Select_Test ("CT_Select (should pass)", True, Result);
   Print_Result (Result);

   --  Test 4: Non-constant-time select (should FAIL)
   Run_Select_Test ("Bad_Select (should fail)", False, Result);
   Print_Result (Result);

   Put_Line ("---------------------------------------------------------------");
   Put_Line ("");
   Put_Line ("Summary:");
   Put_Line ("  Total:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed: " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed: " & Natural'Image (Failed_Tests));
   Put_Line ("");

   --  For the test harness itself, we expect:
   --  - CT_Compare to PASS (constant time)
   --  - Bad_Compare to FAIL (not constant time)
   --  - CT_Select to PASS
   --  - Bad_Select to FAIL
   --
   --  So "success" means detecting the timing leaks in Bad_* functions

   if Failed_Tests = 2 and Passed_Tests = 2 then
      Put_Line ("HARNESS OK: Correctly identified constant-time and leaky implementations");
   else
      Put_Line ("HARNESS ISSUE: Unexpected results - review test methodology");
   end if;

   Put_Line ("");
   Put_Line ("Note: Timing measurements can be affected by system load.");
   Put_Line ("For production, run in isolated environment with CPU pinning.");

end Test_Constant_Time;
