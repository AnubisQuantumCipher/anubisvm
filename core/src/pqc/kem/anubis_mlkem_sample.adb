pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_CBD;

package body Anubis_MLKEM_Sample with
   SPARK_Mode => On
is

   --  XOF buffer size for rejection sampling
   --  Need approximately 3 * 256 / 2 = 384 bytes average case
   --  Use 504 bytes (168 * 3) to handle worst case
   XOF_Buffer_Size : constant := 504;

   --  CBD input size for eta=2: 64 * 2 = 128 bytes
   CBD_Input_Size : constant := 128;

   --  Sample polynomial in NTT domain using rejection sampling
   procedure Sample_NTT (
      XOF_Input : in  Byte_Array;
      Poly      : out Polynomial
   ) is
      XOF_Output : Byte_Array (0 .. XOF_Buffer_Size - 1);
      Coef_Idx   : Natural := 0;
      Buf_Idx    : Natural := 0;
      D1, D2     : Unsigned_16;
   begin
      --  Initialize polynomial to zero
      Poly := (others => 0);

      --  From precondition: XOF_Input'Last < Natural'Last
      pragma Assert (XOF_Input'Last < Natural'Last);

      --  Generate XOF output using SHAKE128
      SHAKE128 (XOF_Input, XOF_Output, XOF_Buffer_Size);

      --  Rejection sampling: sample coefficients until we have 256
      while Coef_Idx < N loop
         pragma Loop_Invariant (Coef_Idx <= N);
         pragma Loop_Invariant (Buf_Idx <= XOF_Buffer_Size);
         pragma Loop_Invariant (for all I in Poly_Index => Poly (I) < Q);

         --  Need at least 3 bytes for 2 coefficient candidates
         if Buf_Idx + 2 >= XOF_Buffer_Size then
            --  Regenerate XOF output if buffer exhausted
            --  This is rare in practice
            SHAKE128 (XOF_Input, XOF_Output, XOF_Buffer_Size);
            Buf_Idx := 0;
         end if;

         --  Extract two 12-bit values from 3 bytes
         D1 := Unsigned_16 (XOF_Output (Buf_Idx)) or
               Shift_Left (Unsigned_16 (XOF_Output (Buf_Idx + 1)) and 16#0F#, 8);
         D2 := Shift_Right (Unsigned_16 (XOF_Output (Buf_Idx + 1)), 4) or
               Shift_Left (Unsigned_16 (XOF_Output (Buf_Idx + 2)), 4);

         Buf_Idx := Buf_Idx + 3;

         --  Accept if < q (rejection sampling)
         if D1 < Q and Coef_Idx < N then
            Poly (Coef_Idx) := Field_Element (D1);
            Coef_Idx := Coef_Idx + 1;
         end if;

         if D2 < Q and Coef_Idx < N then
            Poly (Coef_Idx) := Field_Element (D2);
            Coef_Idx := Coef_Idx + 1;
         end if;
      end loop;
   end Sample_NTT;

   --  Generate matrix A from seed ρ
   --  A[i,j] = SampleNTT(SHAKE128(ρ ‖ j ‖ i))
   procedure Generate_Matrix (
      Rho    : in  Seed;
      Matrix : out Poly_Matrix
   ) is
      XOF_Input : Byte_Array (0 .. 33) := (others => 0);
   begin
      --  Copy seed to XOF input
      for B in 0 .. 31 loop
         pragma Loop_Invariant (B in 0 .. 31);
         XOF_Input (B) := Rho (B);
      end loop;

      --  Generate each matrix element
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         for J in Vec_Index loop
            pragma Loop_Invariant (J in Vec_Index);

            --  Set indices: ρ ‖ j ‖ i
            XOF_Input (32) := Byte (J);
            XOF_Input (33) := Byte (I);

            Sample_NTT (XOF_Input, Matrix (I, J));
         end loop;
      end loop;
   end Generate_Matrix;

   --  Generate matrix A^T (transpose) from seed ρ
   procedure Generate_Matrix_Transpose (
      Rho    : in  Seed;
      Matrix : out Poly_Matrix
   ) is
      XOF_Input : Byte_Array (0 .. 33) := (others => 0);
   begin
      --  Copy seed to XOF input
      for B in 0 .. 31 loop
         pragma Loop_Invariant (B in 0 .. 31);
         XOF_Input (B) := Rho (B);
      end loop;

      --  Generate each matrix element (transposed indices)
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         for J in Vec_Index loop
            pragma Loop_Invariant (J in Vec_Index);

            --  Set indices: ρ ‖ i ‖ j (swapped for transpose)
            XOF_Input (32) := Byte (I);
            XOF_Input (33) := Byte (J);

            Sample_NTT (XOF_Input, Matrix (I, J));
         end loop;
      end loop;
   end Generate_Matrix_Transpose;

   --  Sample noise polynomial using CBD_2
   --  Uses PRF(σ, N) = SHAKE256(σ ‖ N) truncated to 128 bytes
   procedure Sample_Poly_CBD (
      Sigma  : in  Seed;
      Nonce  : in  Byte;
      Poly   : out Polynomial
   ) is
      PRF_Input  : Byte_Array (0 .. 32) := (others => 0);
      PRF_Output : Byte_Array (0 .. CBD_Input_Size - 1) := (others => 0);
      CBD_Poly   : Anubis_CBD.Polynomial := (others => 0);
   begin
      --  Build PRF input: σ ‖ N
      for B in 0 .. 31 loop
         pragma Loop_Invariant (B in 0 .. 31);
         PRF_Input (B) := Sigma (B);
      end loop;
      PRF_Input (32) := Nonce;

      --  Generate PRF output using SHAKE256
      SHAKE256 (PRF_Input, PRF_Output, CBD_Input_Size);

      --  Sample using CBD_2
      Anubis_CBD.CBD_2 (PRF_Output, CBD_Poly);

      --  Initialize output polynomial (required for SPARK initialization proof)
      Poly := (others => 0);

      --  Copy result to output polynomial
      --  CBD_2 guarantees: for all I in 0 .. N - 1 => CBD_Poly (I) < Q
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant
            (for all J in Poly_Index'First .. I - 1 => Poly (J) < Q);
         pragma Loop_Invariant
            (for all J in Poly_Index => CBD_Poly (J) < Q);
         Poly (I) := CBD_Poly (I);
      end loop;
   end Sample_Poly_CBD;

   --  Sample noise vector (k polynomials)
   procedure Sample_Vector_CBD (
      Sigma  : in  Seed;
      Offset : in  Natural;
      Vec    : out Poly_Vector
   ) is
   begin
      --  Initialize Vec to ensure all paths write it
      Vec := (others => (others => 0));
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         pragma Loop_Invariant
            (for all J in Vec_Index'First .. I - 1 =>
               (for all K in Poly_Index => Vec (J) (K) < Q));
         Sample_Poly_CBD (Sigma, Byte (Offset + I), Vec (I));
      end loop;
   end Sample_Vector_CBD;

end Anubis_MLKEM_Sample;
