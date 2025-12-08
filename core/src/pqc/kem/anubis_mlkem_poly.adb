pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Field; use Anubis_Field;
with Anubis_NTT;
with Anubis_MLKEM_Encoding; use Anubis_MLKEM_Encoding;

package body Anubis_MLKEM_Poly with
   SPARK_Mode => On
is

   --  Half of q, used for message encoding
   Q_Half : constant := (Q + 1) / 2;  -- 1665

   --  Polynomial addition
   procedure Poly_Add (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) is
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         Result (I) := Add (Valid_Field (A (I) mod Q), Valid_Field (B (I) mod Q));
      end loop;
   end Poly_Add;

   --  Polynomial subtraction
   procedure Poly_Sub (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) is
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         Result (I) := Sub (Valid_Field (A (I) mod Q), Valid_Field (B (I) mod Q));
      end loop;
   end Poly_Sub;

   --  Forward NTT - convert Polynomial type to NTT type
   procedure Poly_NTT (
      F : in Out Polynomial
   ) is
      NTT_Poly : Anubis_NTT.Polynomial;
   begin
      --  Copy to NTT polynomial type, preserving < Q property
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant (for all J in 0 .. I - 1 => NTT_Poly (J) < Q);
         NTT_Poly (I) := F (I);
      end loop;

      pragma Assert (for all I in Poly_Index => NTT_Poly (I) < Q);

      --  Apply NTT
      Anubis_NTT.NTT (NTT_Poly);

      --  Copy back
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         F (I) := NTT_Poly (I);
      end loop;
   end Poly_NTT;

   --  Inverse NTT
   procedure Poly_INTT (
      F : in Out Polynomial
   ) is
      NTT_Poly : Anubis_NTT.Polynomial;
   begin
      --  Copy to NTT polynomial type
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         NTT_Poly (I) := F (I);
      end loop;

      --  Apply inverse NTT
      Anubis_NTT.INTT (NTT_Poly);

      --  Copy back
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         F (I) := NTT_Poly (I);
      end loop;
   end Poly_INTT;

   --  Point-wise multiplication in NTT domain
   procedure Poly_Mul_NTT (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) is
      NTT_A, NTT_B, NTT_R : Anubis_NTT.Polynomial;
   begin
      --  Copy to NTT polynomial type
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         NTT_A (I) := A (I);
         NTT_B (I) := B (I);
      end loop;

      --  Multiply in NTT domain
      Anubis_NTT.NTT_Mul (NTT_A, NTT_B, NTT_R);

      --  Copy back
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         Result (I) := NTT_R (I);
      end loop;
   end Poly_Mul_NTT;

   --  Reduce polynomial coefficients
   procedure Poly_Reduce (
      F : in Out Polynomial
   ) is
      NTT_Poly : Anubis_NTT.Polynomial;
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         NTT_Poly (I) := F (I);
      end loop;

      Anubis_NTT.Normalize (NTT_Poly);

      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         F (I) := NTT_Poly (I);
      end loop;
   end Poly_Reduce;

   --  Convert message bits to polynomial
   --  Each bit m_i maps to round(q/2) * m_i
   procedure Msg_To_Poly (
      Msg    : in  Message;
      Result : out Polynomial
   ) is
      Bit : Field_Element;
   begin
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I in 0 .. 31);
         for J in 0 .. 7 loop
            pragma Loop_Invariant (J in 0 .. 7);
            Bit := Field_Element (Shift_Right (Unsigned_8 (Msg (I)), J) and 1);
            Result (8 * I + J) := Bit * Q_Half;
         end loop;
      end loop;
   end Msg_To_Poly;

   --  Vector addition
   procedure Vec_Add (
      A, B   : in  Poly_Vector;
      Result : out Poly_Vector
   ) is
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         Poly_Add (A (I), B (I), Result (I));
      end loop;
   end Vec_Add;

   --  Vector subtraction
   procedure Vec_Sub (
      A, B   : in  Poly_Vector;
      Result : out Poly_Vector
   ) is
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         Poly_Sub (A (I), B (I), Result (I));
      end loop;
   end Vec_Sub;

   --  Forward NTT on vector
   procedure Vec_NTT (
      V : in Out Poly_Vector
   ) is
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         --  From precondition: V(I) coefficients < Q
         pragma Loop_Invariant (for all K in I .. Vec_Index'Last =>
                                   (for all J in Poly_Index => V (K) (J) < Q));
         Poly_NTT (V (I));
      end loop;
   end Vec_NTT;

   --  Inverse NTT on vector
   procedure Vec_INTT (
      V : in Out Poly_Vector
   ) is
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         Poly_INTT (V (I));
      end loop;
   end Vec_INTT;

   --  Dot product of two vectors in NTT domain
   procedure Vec_Dot_Product_NTT (
      A, B   : in  Poly_Vector;
      Result : out Polynomial
   ) is
      Temp  : Polynomial;
      Accum : Polynomial;  --  Avoid aliasing: separate accumulator
   begin
      --  Initialize result to zero
      Result := (others => 0);

      --  Accumulate products
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         Poly_Mul_NTT (A (I), B (I), Temp);
         Poly_Add (Result, Temp, Accum);
         Result := Accum;
      end loop;
   end Vec_Dot_Product_NTT;

   --  Matrix-vector multiplication in NTT domain
   procedure Matrix_Vec_Mul_NTT (
      M      : in  Poly_Matrix;
      V      : in  Poly_Vector;
      Result : out Poly_Vector
   ) is
      Temp      : Polynomial;
      Row_Accum : Polynomial;  --  Avoid aliasing: separate accumulator
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);

         --  Result[i] = sum(M[i,j] * V[j])
         Result (I) := (others => 0);
         for J in Vec_Index loop
            pragma Loop_Invariant (J in Vec_Index);
            Poly_Mul_NTT (M (I, J), V (J), Temp);
            Poly_Add (Result (I), Temp, Row_Accum);
            Result (I) := Row_Accum;
         end loop;
      end loop;
   end Matrix_Vec_Mul_NTT;

   --  Transpose matrix-vector multiplication
   procedure Matrix_T_Vec_Mul_NTT (
      M      : in  Poly_Matrix;
      V      : in  Poly_Vector;
      Result : out Poly_Vector
   ) is
      Temp      : Polynomial;
      Row_Accum : Polynomial;  --  Avoid aliasing: separate accumulator
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);

         --  Result[i] = sum(M[j,i] * V[j]) (transpose)
         Result (I) := (others => 0);
         for J in Vec_Index loop
            pragma Loop_Invariant (J in Vec_Index);
            Poly_Mul_NTT (M (J, I), V (J), Temp);
            Poly_Add (Result (I), Temp, Row_Accum);
            Result (I) := Row_Accum;
         end loop;
      end loop;
   end Matrix_T_Vec_Mul_NTT;

end Anubis_MLKEM_Poly;
