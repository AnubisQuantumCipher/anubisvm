pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;

package body Anubis_MLDSA_NTT with
   SPARK_Mode => On
is

   --  Normalization constant for INTT
   --  F = 256^{-1} * R mod Q = 16382
   F_Const : constant Field_Element := 16382;

   --  Butterfly operation for NTT
   procedure Butterfly (
      A, B : in out Field_Element;
      Zeta : Field_Element
   ) with
      Inline_Always,
      Global => null
   is
      T : constant Field_Element := Mont_Mul (Zeta, B);
   begin
      B := Field_Element (
         (Unsigned_64 (A) + Unsigned_64 (Q) - Unsigned_64 (T)) mod Unsigned_64 (Q));
      A := Field_Element (
         (Unsigned_64 (A) + Unsigned_64 (T)) mod Unsigned_64 (Q));
   end Butterfly;

   --  Inverse butterfly operation for INTT
   procedure Inv_Butterfly (
      A, B : in Out Field_Element;
      Neg_Zeta : Field_Element
   ) with
      Inline_Always,
      Global => null
   is
      T : constant Field_Element := A;
   begin
      A := Field_Element (
         (Unsigned_64 (T) + Unsigned_64 (B)) mod Unsigned_64 (Q));
      B := Field_Element (
         (Unsigned_64 (Q) + Unsigned_64 (T) - Unsigned_64 (B)) mod Unsigned_64 (Q));
      B := Mont_Mul (Neg_Zeta, B);
   end Inv_Butterfly;

   --  Forward NTT using Cooley-Tukey butterfly
   --  Unrolled for provability with explicit bounds
   procedure NTT (F : in Out Polynomial) is
      K : Natural := 0;
   begin
      --  Layer 0: Len=128, 1 group
      K := 1;
      for J in 0 .. 127 loop
         pragma Loop_Invariant (J in 0 .. 127);
         Butterfly (F (J), F (J + 128), Zetas (K));
      end loop;

      --  Layer 1: Len=64, 2 groups
      for G in 0 .. 1 loop
         pragma Loop_Invariant (G in 0 .. 1);
         pragma Loop_Invariant (K in 1 .. 2);
         K := K + 1;
         for J in 0 .. 63 loop
            pragma Loop_Invariant (J in 0 .. 63);
            pragma Loop_Invariant (G * 128 + J + 64 < N);
            Butterfly (F (G * 128 + J), F (G * 128 + J + 64), Zetas (K));
         end loop;
      end loop;

      --  Layer 2: Len=32, 4 groups
      for G in 0 .. 3 loop
         pragma Loop_Invariant (G in 0 .. 3);
         pragma Loop_Invariant (K in 3 .. 6);
         K := K + 1;
         for J in 0 .. 31 loop
            pragma Loop_Invariant (J in 0 .. 31);
            pragma Loop_Invariant (G * 64 + J + 32 < N);
            Butterfly (F (G * 64 + J), F (G * 64 + J + 32), Zetas (K));
         end loop;
      end loop;

      --  Layer 3: Len=16, 8 groups
      for G in 0 .. 7 loop
         pragma Loop_Invariant (G in 0 .. 7);
         pragma Loop_Invariant (K in 7 .. 14);
         K := K + 1;
         for J in 0 .. 15 loop
            pragma Loop_Invariant (J in 0 .. 15);
            pragma Loop_Invariant (G * 32 + J + 16 < N);
            Butterfly (F (G * 32 + J), F (G * 32 + J + 16), Zetas (K));
         end loop;
      end loop;

      --  Layer 4: Len=8, 16 groups
      for G in 0 .. 15 loop
         pragma Loop_Invariant (G in 0 .. 15);
         pragma Loop_Invariant (K in 15 .. 30);
         K := K + 1;
         for J in 0 .. 7 loop
            pragma Loop_Invariant (J in 0 .. 7);
            pragma Loop_Invariant (G * 16 + J + 8 < N);
            Butterfly (F (G * 16 + J), F (G * 16 + J + 8), Zetas (K));
         end loop;
      end loop;

      --  Layer 5: Len=4, 32 groups
      for G in 0 .. 31 loop
         pragma Loop_Invariant (G in 0 .. 31);
         pragma Loop_Invariant (K in 31 .. 62);
         K := K + 1;
         for J in 0 .. 3 loop
            pragma Loop_Invariant (J in 0 .. 3);
            pragma Loop_Invariant (G * 8 + J + 4 < N);
            Butterfly (F (G * 8 + J), F (G * 8 + J + 4), Zetas (K));
         end loop;
      end loop;

      --  Layer 6: Len=2, 64 groups
      for G in 0 .. 63 loop
         pragma Loop_Invariant (G in 0 .. 63);
         pragma Loop_Invariant (K in 63 .. 126);
         K := K + 1;
         for J in 0 .. 1 loop
            pragma Loop_Invariant (J in 0 .. 1);
            pragma Loop_Invariant (G * 4 + J + 2 < N);
            Butterfly (F (G * 4 + J), F (G * 4 + J + 2), Zetas (K));
         end loop;
      end loop;

      --  Layer 7: Len=1, 128 groups
      for G in 0 .. 127 loop
         pragma Loop_Invariant (G in 0 .. 127);
         pragma Loop_Invariant (K in 127 .. 254);
         K := K + 1;
         Butterfly (F (G * 2), F (G * 2 + 1), Zetas (K));
      end loop;
   end NTT;

   --  Inverse NTT using Gentleman-Sande butterfly
   procedure INTT (F : in Out Polynomial) is
      K : Natural := 256;
      Neg_Zeta : Field_Element;
   begin
      --  Layer 0: Len=1, 128 groups
      for G in 0 .. 127 loop
         pragma Loop_Invariant (G in 0 .. 127);
         pragma Loop_Invariant (K in 129 .. 256);
         K := K - 1;
         if Zetas (K) = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zetas (K);
         end if;
         Inv_Butterfly (F (G * 2), F (G * 2 + 1), Neg_Zeta);
      end loop;

      --  Layer 1: Len=2, 64 groups
      for G in 0 .. 63 loop
         pragma Loop_Invariant (G in 0 .. 63);
         pragma Loop_Invariant (K in 65 .. 128);
         K := K - 1;
         if Zetas (K) = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zetas (K);
         end if;
         for J in 0 .. 1 loop
            pragma Loop_Invariant (J in 0 .. 1);
            pragma Loop_Invariant (G * 4 + J + 2 < N);
            Inv_Butterfly (F (G * 4 + J), F (G * 4 + J + 2), Neg_Zeta);
         end loop;
      end loop;

      --  Layer 2: Len=4, 32 groups
      for G in 0 .. 31 loop
         pragma Loop_Invariant (G in 0 .. 31);
         pragma Loop_Invariant (K in 33 .. 64);
         K := K - 1;
         if Zetas (K) = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zetas (K);
         end if;
         for J in 0 .. 3 loop
            pragma Loop_Invariant (J in 0 .. 3);
            pragma Loop_Invariant (G * 8 + J + 4 < N);
            Inv_Butterfly (F (G * 8 + J), F (G * 8 + J + 4), Neg_Zeta);
         end loop;
      end loop;

      --  Layer 3: Len=8, 16 groups
      for G in 0 .. 15 loop
         pragma Loop_Invariant (G in 0 .. 15);
         pragma Loop_Invariant (K in 17 .. 32);
         K := K - 1;
         if Zetas (K) = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zetas (K);
         end if;
         for J in 0 .. 7 loop
            pragma Loop_Invariant (J in 0 .. 7);
            pragma Loop_Invariant (G * 16 + J + 8 < N);
            Inv_Butterfly (F (G * 16 + J), F (G * 16 + J + 8), Neg_Zeta);
         end loop;
      end loop;

      --  Layer 4: Len=16, 8 groups
      for G in 0 .. 7 loop
         pragma Loop_Invariant (G in 0 .. 7);
         pragma Loop_Invariant (K in 9 .. 16);
         K := K - 1;
         if Zetas (K) = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zetas (K);
         end if;
         for J in 0 .. 15 loop
            pragma Loop_Invariant (J in 0 .. 15);
            pragma Loop_Invariant (G * 32 + J + 16 < N);
            Inv_Butterfly (F (G * 32 + J), F (G * 32 + J + 16), Neg_Zeta);
         end loop;
      end loop;

      --  Layer 5: Len=32, 4 groups
      for G in 0 .. 3 loop
         pragma Loop_Invariant (G in 0 .. 3);
         pragma Loop_Invariant (K in 5 .. 8);
         K := K - 1;
         if Zetas (K) = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zetas (K);
         end if;
         for J in 0 .. 31 loop
            pragma Loop_Invariant (J in 0 .. 31);
            pragma Loop_Invariant (G * 64 + J + 32 < N);
            Inv_Butterfly (F (G * 64 + J), F (G * 64 + J + 32), Neg_Zeta);
         end loop;
      end loop;

      --  Layer 6: Len=64, 2 groups
      for G in 0 .. 1 loop
         pragma Loop_Invariant (G in 0 .. 1);
         pragma Loop_Invariant (K in 3 .. 4);
         K := K - 1;
         if Zetas (K) = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zetas (K);
         end if;
         for J in 0 .. 63 loop
            pragma Loop_Invariant (J in 0 .. 63);
            pragma Loop_Invariant (G * 128 + J + 64 < N);
            Inv_Butterfly (F (G * 128 + J), F (G * 128 + J + 64), Neg_Zeta);
         end loop;
      end loop;

      --  Layer 7: Len=128, 1 group
      K := K - 1;
      if Zetas (K) = 0 then
         Neg_Zeta := 0;
      else
         Neg_Zeta := Q - Zetas (K);
      end if;
      for J in 0 .. 127 loop
         pragma Loop_Invariant (J in 0 .. 127);
         Inv_Butterfly (F (J), F (J + 128), Neg_Zeta);
      end loop;

      --  Final normalization
      for I in Poly_Index loop
         F (I) := Mont_Reduce (Unsigned_64 (F_Const) * Unsigned_64 (F (I)));
      end loop;
   end INTT;

   --  Point-wise multiplication in NTT domain
   procedure NTT_Mul (
      F : in Polynomial;
      G : in Polynomial;
      H : out Polynomial
   ) is
   begin
      for I in Poly_Index loop
         H (I) := Mul (Freeze (F (I)), Freeze (G (I)));
      end loop;
   end NTT_Mul;

   --  Vector NTT operations
   procedure Vec_NTT_L (V : in Out Poly_Vector_L) is
   begin
      for I in L_Index loop
         NTT (V (I));
      end loop;
   end Vec_NTT_L;

   procedure Vec_INTT_L (V : in Out Poly_Vector_L) is
   begin
      for I in L_Index loop
         INTT (V (I));
      end loop;
   end Vec_INTT_L;

   procedure Vec_NTT_K (V : in Out Poly_Vector_K) is
   begin
      for I in K_Index loop
         NTT (V (I));
      end loop;
   end Vec_NTT_K;

   procedure Vec_INTT_K (V : in Out Poly_Vector_K) is
   begin
      for I in K_Index loop
         INTT (V (I));
      end loop;
   end Vec_INTT_K;

   procedure Matrix_NTT (M : in out Poly_Matrix) is
   begin
      for I in K_Index loop
         for J in L_Index loop
            NTT (M (I, J));
         end loop;
      end loop;
   end Matrix_NTT;

   --  Matrix-vector multiplication: result = A * v
   procedure Matrix_Vec_Mul (
      A      : in  Poly_Matrix;
      V      : in  Poly_Vector_L;
      Result : out Poly_Vector_K
   ) is
      Temp : Polynomial;
   begin
      for I in K_Index loop
         Result (I) := (others => 0);
         for J in L_Index loop
            NTT_Mul (A (I, J), V (J), Temp);
            for C in Poly_Index loop
               Result (I) (C) := Field_Element (
                  (Unsigned_64 (Result (I) (C)) + Unsigned_64 (Temp (C))) mod Unsigned_64 (Q));
            end loop;
         end loop;
      end loop;
   end Matrix_Vec_Mul;

   --  Dot product of l-vectors
   procedure Vec_Dot_Product_L (
      V1, V2 : in  Poly_Vector_L;
      Result : out Polynomial
   ) is
      Temp : Polynomial;
   begin
      Result := (others => 0);
      for I in L_Index loop
         NTT_Mul (V1 (I), V2 (I), Temp);
         for C in Poly_Index loop
            Result (C) := Field_Element (
               (Unsigned_64 (Result (C)) + Unsigned_64 (Temp (C))) mod Unsigned_64 (Q));
         end loop;
      end loop;
   end Vec_Dot_Product_L;

   --  Dot product of k-vectors
   procedure Vec_Dot_Product_K (
      V1, V2 : in  Poly_Vector_K;
      Result : out Polynomial
   ) is
      Temp : Polynomial;
   begin
      Result := (others => 0);
      for I in K_Index loop
         NTT_Mul (V1 (I), V2 (I), Temp);
         for C in Poly_Index loop
            Result (C) := Field_Element (
               (Unsigned_64 (Result (C)) + Unsigned_64 (Temp (C))) mod Unsigned_64 (Q));
         end loop;
      end loop;
   end Vec_Dot_Product_K;

end Anubis_MLDSA_NTT;
