pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Field; use Anubis_Field;

package body Anubis_MLKEM_Compress with
   SPARK_Mode => On
is

   --  Compress_Du: Compress to 11 bits
   --  Compress_11(x) = round((2^11 * x) / q) mod 2^11
   --  = round((2048 * x) / 3329) mod 2048
   --  Using: round(a/b) = (a + b/2) / b
   --  Result is masked with 0x7FF (11 bits), so always < 2^11 = 2048
   procedure Compress_Du (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) is
      Val : Unsigned_32;
   begin
      Result := (others => 0);
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant
            (for all J in Poly_Index'First .. I - 1 => Result (J) < 2**Du);
         --  Compute round((2048 * x) / 3329) mod 2048
         --  = ((2048 * x + 3329/2) / 3329) mod 2048
         --  = ((2048 * x + 1664) / 3329) mod 2048
         Val := Unsigned_32 (F (I));
         Val := ((Shift_Left (Val, 11) + Q / 2) / Q) and 16#7FF#;
         --  After masking with 0x7FF, Val < 2048 = 2^11 = 2^Du
         pragma Assert (Val < 2**Du);
         Result (I) := Field_Element (Val);
      end loop;
   end Compress_Du;

   --  Compress_Dv: Compress to 5 bits
   --  Compress_5(x) = round((32 * x) / 3329) mod 32
   --  Result is masked with 0x1F (5 bits), so always < 2^5 = 32
   procedure Compress_Dv (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) is
      Val : Unsigned_32;
   begin
      Result := (others => 0);
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant
            (for all J in Poly_Index'First .. I - 1 => Result (J) < 2**Dv);
         --  Compute round((32 * x) / 3329) mod 32
         --  = ((32 * x + 1664) / 3329) mod 32
         Val := Unsigned_32 (F (I));
         Val := ((Shift_Left (Val, 5) + Q / 2) / Q) and 16#1F#;
         --  After masking with 0x1F, Val < 32 = 2^5 = 2^Dv
         pragma Assert (Val < 2**Dv);
         Result (I) := Field_Element (Val);
      end loop;
   end Compress_Dv;

   --  Decompress_Du: Decompress from 11 bits
   --  Decompress_11(y) = round((3329 * y) / 2048)
   --  Bound proof: For y < 2048: (y * 3329 + 1024) / 2048
   --  Max: (2047 * 3329 + 1024) / 2048 = (6814913 + 1024) / 2048 = 3328 < Q
   procedure Decompress_Du (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) is
      Val : Unsigned_32;
   begin
      --  Initialize Result for SPARK
      Result := (others => 0);

      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant
            (for all J in Poly_Index'First .. I - 1 => Result (J) < Q);
         --  Compute round((3329 * y) / 2048)
         --  = (3329 * y + 1024) / 2048
         --  Precondition ensures F(I) < 2^11 = 2048
         Val := Unsigned_32 (F (I));
         --  Val < 2048, so Val * Q < 2048 * 3329 = 6818816
         --  (Val * Q + 1024) < 6819840
         --  (Val * Q + 1024) / 2048 < 3330, but actually <= 3328 (proven below)
         Val := (Val * Q + 1024) / 2048;
         --  Assert bound: Val <= (2047 * 3329 + 1024) / 2048 = 3328 < Q
         pragma Assert (Val < Q);
         Result (I) := Field_Element (Val);
      end loop;
   end Decompress_Du;

   --  Decompress_Dv: Decompress from 5 bits
   --  Decompress_5(y) = round((3329 * y) / 32)
   --  Bound proof: For y < 32: (y * 3329 + 16) / 32
   --  Max: (31 * 3329 + 16) / 32 = (103199 + 16) / 32 = 3225 < Q
   procedure Decompress_Dv (
      F      : in     Polynomial;
      Result : out    Polynomial
   ) is
      Val : Unsigned_32;
   begin
      Result := (others => 0);
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant
            (for all J in Poly_Index'First .. I - 1 => Result (J) < Q);
         --  Compute round((3329 * y) / 32)
         --  = (3329 * y + 16) / 32
         --  Precondition ensures F(I) < 2^5 = 32
         Val := Unsigned_32 (F (I));
         Val := (Val * Q + 16) / 32;
         --  Assert bound: Val <= (31 * 3329 + 16) / 32 = 3225 < Q
         pragma Assert (Val < Q);
         Result (I) := Field_Element (Val);
      end loop;
   end Decompress_Dv;

   --  Compress vector with Du
   procedure Compress_Vector_Du (
      V      : in     Poly_Vector;
      Result : out    Poly_Vector
   ) is
   begin
      Result := (others => (others => 0));
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         pragma Loop_Invariant
            (for all K in Vec_Index'First .. I - 1 =>
               (for all J in Poly_Index => Result (K) (J) < 2**Du));
         Compress_Du (V (I), Result (I));
      end loop;
   end Compress_Vector_Du;

   --  Decompress vector with Du
   procedure Decompress_Vector_Du (
      V      : in     Poly_Vector;
      Result : out    Poly_Vector
   ) is
   begin
      --  Initialize Result (for SPARK initialization proof)
      Result := (others => (others => 0));

      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         --  From precondition: all V(J) polynomials have coefficients < 2**Du
         pragma Loop_Invariant (for all K in I .. Vec_Index'Last =>
                                   (for all J in Poly_Index => V (K) (J) < 2**Du));
         --  Accumulate postcondition: processed elements satisfy Result < Q
         pragma Loop_Invariant
            (for all K in Vec_Index'First .. I - 1 =>
               (for all J in Poly_Index => Result (K) (J) < Q));
         Decompress_Du (V (I), Result (I));
      end loop;
   end Decompress_Vector_Du;

end Anubis_MLKEM_Compress;
