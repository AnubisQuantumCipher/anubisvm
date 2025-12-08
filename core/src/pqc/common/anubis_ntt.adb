pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_NTT with
   SPARK_Mode => On
is

   --  Bit-reverse 8-bit index (for 256-element array)
   function Bit_Reverse (X : Poly_Index) return Poly_Index is
      Result : Natural := 0;
      Val : Natural := X;
   begin
      --  Reverse 8 bits
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         pragma Loop_Invariant (Result < 256);
         pragma Loop_Invariant (Val < 512);

         Result := Result * 2;
         if Val mod 2 = 1 then
            Result := Result + 1;
         end if;
         Val := Val / 2;
      end loop;

      return Result;
   end Bit_Reverse;

   --  Forward NTT using Cooley-Tukey butterfly
   --  7 layers with stride pattern: 128, 64, 32, 16, 8, 4, 2
   procedure NTT (F : in out Polynomial) is
      Len : Positive := 128;
      K : Natural := 1;
      Start : Natural;
      J : Natural;
      Zeta : Field_Element;
      T : Field_Element;
   begin
      --  7 layers of butterflies (log2(256) = 8, but we pair coefficients)
      for Layer in 0 .. 6 loop
         pragma Loop_Invariant (Layer in 0 .. 6);
         pragma Loop_Invariant (Len in 1 .. 128);
         pragma Loop_Invariant (K in 1 .. 128);
         pragma Loop_Invariant (K = 2**Layer);
         pragma Loop_Invariant (Len = 128 / K);

         Start := 0;
         while Start < N loop
            pragma Loop_Invariant (Start >= 0);
            pragma Loop_Invariant (Start <= N);
            pragma Loop_Invariant (Start mod (2 * Len) = 0);
            pragma Loop_Invariant (K < 256);

            Zeta := Zetas (K);
            K := K + 1;

            J := Start;
            while J < Start + Len loop
               pragma Loop_Invariant (J >= Start);
               pragma Loop_Invariant (J < Start + Len);
               pragma Loop_Invariant (J < N);
               pragma Loop_Invariant (J + Len < N);
               pragma Loop_Invariant (K <= 256);

               --  Butterfly: (a, b) → (a + ζ*b, a - ζ*b)
               --  Cast to Unsigned_32 BEFORE arithmetic to avoid Field_Element wraparound
               T := Mont_Mul (Zeta, F (J + Len));
               F (J + Len) := Barrett_Reduce (Unsigned_32 (F (J)) + 2 * Q - Unsigned_32 (T));
               F (J) := Barrett_Reduce (Unsigned_32 (F (J)) + Unsigned_32 (T));

               J := J + 1;
            end loop;

            Start := Start + 2 * Len;
         end loop;

         Len := Len / 2;
      end loop;
   end NTT;

   --  Inverse NTT using Gentleman-Sande butterfly
   --  7 layers with stride pattern: 2, 4, 8, 16, 32, 64, 128
   --  Uses Zetas array BACKWARDS like Kyber reference (zetas[127] down to zetas[0])
   procedure INTT (F : in out Polynomial) is
      Len : Positive := 2;
      K : Natural := 127;  -- Start from 127 and decrement (Kyber style)
      Start : Natural;
      J : Natural;
      Zeta : Field_Element;
      T : Field_Element;
   begin
      --  7 layers of inverse butterflies
      for Layer in 0 .. 6 loop
         pragma Loop_Invariant (Layer in 0 .. 6);
         pragma Loop_Invariant (Len in 2 .. 128);
         pragma Loop_Invariant (K in 0 .. 127);
         pragma Loop_Invariant (Len = 2**(Layer + 1));

         Start := 0;
         while Start < N loop
            pragma Loop_Invariant (Start >= 0);
            pragma Loop_Invariant (Start <= N);
            pragma Loop_Invariant (Start mod (2 * Len) = 0);
            pragma Loop_Invariant (K < 256);

            --  Kyber uses positive zetas[k--] for inverse NTT
            Zeta := Zetas (K);
            K := K - 1;          -- Decrement AFTER access (like Kyber"s k--)

            J := Start;
            while J < Start + Len loop
               pragma Loop_Invariant (J >= Start);
               pragma Loop_Invariant (J < Start + Len);
               pragma Loop_Invariant (J < N);
               pragma Loop_Invariant (J + Len < N);
               pragma Loop_Invariant (K <= 256);

               --  Inverse butterfly matching Kyber exactly:
               --  t = r[j]; r[j] = t + r[j+len]; r[j+len] = zeta * (r[j+len] - t)
               T := F (J);
               F (J) := Barrett_Reduce (Unsigned_32 (T) + Unsigned_32 (F (J + Len)));
               --  Compute (F(J+Len) - T) mod Q = (2*Q + F(J+Len) - T) mod Q
               F (J + Len) := Barrett_Reduce (2 * Q + Unsigned_32 (F (J + Len)) - Unsigned_32 (T));
               F (J + Len) := Mont_Mul (Zeta, F (J + Len));

               J := J + 1;
            end loop;

            Start := Start + 2 * Len;
         end loop;

         Len := Len * 2;
      end loop;

      --  Normalize: pre-normalization value is coeff * 128
      --  Multiply by 128^-1 = 3303 to recover original coefficient
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         F (I) := Mul (F (I), 3303);
      end loop;

      --  Final reduction to ensure all coefficients are in [0, Q-1]
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         F (I) := Barrett_Reduce (Unsigned_32 (F (I)));
      end loop;
   end INTT;

   --  Basemul: Multiply two degree-1 polynomials in NTT domain
   --  (a0 + a1*X) * (b0 + b1*X) mod (X^2 - zeta)
   --  = (a0*b0 + a1*b1*zeta) + (a0*b1 + a1*b0)*X
   procedure Basemul (
      A0, A1, B0, B1 : in Field_Element;
      Zeta           : in Field_Element;
      R0, R1         : out Field_Element
   ) is
      T0, T1, T2 : Field_Element;
   begin
      --  r0 = a0*b0 + a1*b1*zeta
      T0 := Mont_Mul (A1, B1);
      T1 := Mont_Mul (T0, Zeta);
      T2 := Mont_Mul (A0, B0);
      R0 := Barrett_Reduce (Unsigned_32 (T1) + Unsigned_32 (T2));

      --  r1 = a0*b1 + a1*b0
      T0 := Mont_Mul (A0, B1);
      T1 := Mont_Mul (A1, B0);
      R1 := Barrett_Reduce (Unsigned_32 (T0) + Unsigned_32 (T1));
   end Basemul;

   --  Point-wise multiplication in NTT domain using basemul
   --  Operates on 128 pairs of coefficients (groups of 4)
   --  Following Kyber reference: basemul with zetas[64+i] for first pair,
   --  -zetas[64+i] for second pair in each group of 4
   --
   --  Note: Basemul introduces an extra R^(-1) factor from Montgomery multiplication.
   --  We compensate by multiplying each result coefficient by R^2 = 1353.
   --  Mont_Mul(x, R^2) = x * R^2 * R^(-1) = x * R, canceling the extra R^(-1).
   R_Squared : constant Field_Element := 1353;  -- R^2 mod Q = 2285^2 mod 3329

   procedure NTT_Mul (
      F : in Polynomial;
      G : in Polynomial;
      H : out Polynomial
   ) is
      Zeta, Neg_Zeta : Field_Element;
   begin
      --  Process 64 groups of 4 coefficients (2 pairs each)
      for I in 0 .. 63 loop
         pragma Loop_Invariant (I in 0 .. 63);

         --  Get zeta for this group
         Zeta := Zetas (64 + I);
         --  Compute -zeta mod Q = Q - zeta (for zeta > 0)
         if Zeta = 0 then
            Neg_Zeta := 0;
         else
            Neg_Zeta := Q - Zeta;
         end if;

         --  First pair in group uses +zeta
         Basemul (
            F (4 * I), F (4 * I + 1),
            G (4 * I), G (4 * I + 1),
            Zeta,
            H (4 * I), H (4 * I + 1)
         );

         --  Second pair in group uses -zeta
         Basemul (
            F (4 * I + 2), F (4 * I + 3),
            G (4 * I + 2), G (4 * I + 3),
            Neg_Zeta,
            H (4 * I + 2), H (4 * I + 3)
         );
      end loop;

      --  Compensate for the extra R^(-1) factor introduced by basemul
      --  Multiply each coefficient by R using Mont_Mul(coef, R^2)
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         H (I) := Mont_Mul (H (I), R_Squared);
      end loop;
   end NTT_Mul;

   --  Normalize all coefficients to [0, Q-1]
   procedure Normalize (F : in out Polynomial) is
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         F (I) := Barrett_Reduce (Unsigned_32 (F (I)));
      end loop;
   end Normalize;

end Anubis_NTT;
