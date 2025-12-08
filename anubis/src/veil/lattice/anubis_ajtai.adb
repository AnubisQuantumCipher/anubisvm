--  ANUBIS Ajtai Commitment Scheme Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_Ajtai with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  NTT Parameters (aligned with ML-KEM q=3329)
   ---------------------------------------------------------------------------

   --  Primitive 512th root of unity mod q
   --  zeta^256 = -1 mod q (for negacyclic NTT)
   Zeta : constant Field_Element := 17;

   --  Precomputed powers of zeta for NTT
   --  zetas(i) = zeta^(bit_rev_7(i)) for i in 0..127
   type Zeta_Array is array (0 .. 127) of Field_Element;

   Zetas : constant Zeta_Array := (
      1, 1729, 2580, 3289, 2642, 630, 1897, 848,
      1062, 1919, 193, 797, 2786, 3260, 569, 1746,
      296, 2447, 1339, 1476, 3046, 56, 2240, 1333,
      1426, 2094, 535, 2882, 2393, 2879, 1974, 821,
      289, 331, 3253, 1756, 1197, 2304, 2277, 2055,
      650, 1977, 2513, 632, 2865, 33, 1320, 1915,
      2319, 1435, 807, 452, 1438, 2868, 1534, 2402,
      2647, 2617, 1481, 648, 2474, 3110, 1227, 910,
      17, 2761, 583, 2649, 1637, 723, 2288, 1100,
      1409, 2662, 3281, 233, 756, 2156, 3015, 3050,
      1703, 1651, 2789, 1789, 1847, 952, 1461, 2687,
      939, 2308, 2437, 2388, 733, 2337, 268, 641,
      1584, 2298, 2037, 3220, 375, 2549, 2090, 1645,
      1063, 319, 2773, 757, 2099, 561, 2466, 2594,
      2804, 1092, 403, 1026, 1143, 2150, 2775, 886,
      1722, 1212, 1874, 1029, 2110, 2935, 885, 2154
   );

   --  Inverse zetas for inverse NTT
   Inv_Zetas : constant Zeta_Array := (
      1175, 2444, 394, 1219, 2300, 1455, 2117, 1607,
      2443, 554, 1179, 2186, 2303, 2926, 2237, 525,
      735, 863, 2768, 1230, 2572, 556, 3010, 2266,
      1684, 1239, 780, 2954, 109, 1292, 1031, 1745,
      2688, 3061, 992, 2596, 941, 892, 1021, 2390,
      642, 1868, 2377, 1482, 1540, 540, 1678, 1626,
      279, 1173, 1573, 76, 3040, 2998, 2132, 1573,
      2009, 2009, 1894, 522, 877, 2891, 1010, 1010,
      2319, 219, 855, 2681, 2103, 712, 2682, 1903,
      2794, 447, 936, 450, 1235, 794, 2794, 1235,
      1903, 2682, 712, 2103, 2681, 855, 219, 2319,
      1010, 1010, 2891, 877, 522, 1894, 2009, 2009,
      1573, 2132, 2998, 3040, 76, 1573, 1173, 279,
      1626, 1678, 540, 1540, 1482, 2377, 1868, 642,
      2390, 1021, 892, 941, 2596, 992, 3061, 2688,
      1745, 1031, 1292, 109, 2954, 780, 1239, 1684
   );

   --  Montgomery reduction constant: R = 2^16
   R_Inv : constant := 169;  --  (2^16)^-1 mod q

   ---------------------------------------------------------------------------
   --  Modular Arithmetic Helpers
   ---------------------------------------------------------------------------

   function Mod_Add (A, B : Field_Element) return Field_Element is
   begin
      return (A + B) mod Q;
   end Mod_Add;

   function Mod_Sub (A, B : Field_Element) return Field_Element is
   begin
      if A >= B then
         return A - B;
      else
         return Q - (B - A);
      end if;
   end Mod_Sub;

   function Mod_Mul (A, B : Field_Element) return Field_Element is
      Product : constant Unsigned_32 := Unsigned_32 (A) * Unsigned_32 (B);
   begin
      return Field_Element (Product mod Unsigned_32 (Q));
   end Mod_Mul;

   ---------------------------------------------------------------------------
   --  Polynomial Arithmetic
   ---------------------------------------------------------------------------

   function Add_Poly (A, B : Polynomial) return Polynomial is
      Result : Polynomial;
   begin
      for I in 0 .. N - 1 loop
         Result (I) := Mod_Add (A (I), B (I));
      end loop;
      return Result;
   end Add_Poly;

   function Sub_Poly (A, B : Polynomial) return Polynomial is
      Result : Polynomial;
   begin
      for I in 0 .. N - 1 loop
         Result (I) := Mod_Sub (A (I), B (I));
      end loop;
      return Result;
   end Sub_Poly;

   --  NTT: Convert polynomial to NTT domain
   procedure NTT (F : in out Polynomial) is
      Len : Natural := 128;
      Start, J : Natural;
      Zeta_Val, T : Field_Element;
      K_Idx : Natural := 1;
   begin
      while Len >= 2 loop
         Start := 0;
         while Start < N loop
            Zeta_Val := Zetas (K_Idx);
            K_Idx := K_Idx + 1;
            J := Start;
            while J < Start + Len loop
               T := Mod_Mul (Zeta_Val, F (J + Len));
               F (J + Len) := Mod_Sub (F (J), T);
               F (J) := Mod_Add (F (J), T);
               J := J + 1;
            end loop;
            Start := Start + 2 * Len;
         end loop;
         Len := Len / 2;
      end loop;
   end NTT;

   --  Inverse NTT
   procedure Inv_NTT (F : in Out Polynomial) is
      Len : Natural := 2;
      Start, J : Natural;
      Zeta_Val, T : Field_Element;
      K_Idx : Natural := 127;
   begin
      while Len <= 128 loop
         Start := 0;
         while Start < N loop
            Zeta_Val := Inv_Zetas (K_Idx);
            K_Idx := K_Idx - 1;
            J := Start;
            while J < Start + Len loop
               T := F (J);
               F (J) := Mod_Add (T, F (J + Len));
               F (J + Len) := Mod_Mul (Zeta_Val, Mod_Sub (F (J + Len), T));
               J := J + 1;
            end loop;
            Start := Start + 2 * Len;
         end loop;
         Len := Len * 2;
      end loop;

      --  Multiply by n^-1 mod q
      --  n = 256, n^-1 mod 3329 = 3303
      for I in 0 .. N - 1 loop
         F (I) := Mod_Mul (F (I), 3303);
      end loop;
   end Inv_NTT;

   --  Polynomial multiplication using NTT
   function Mul_Poly (A, B : Polynomial) return Polynomial is
      A_NTT : Polynomial := A;
      B_NTT : Polynomial := B;
      Result : Polynomial;
   begin
      NTT (A_NTT);
      NTT (B_NTT);

      --  Pointwise multiplication in NTT domain
      for I in 0 .. N - 1 loop
         Result (I) := Mod_Mul (A_NTT (I), B_NTT (I));
      end loop;

      Inv_NTT (Result);
      return Result;
   end Mul_Poly;

   ---------------------------------------------------------------------------
   --  Vector Operations
   ---------------------------------------------------------------------------

   function Add_Vector (A, B : Module_Vector) return Module_Vector is
      Result : Module_Vector;
   begin
      for I in 0 .. K - 1 loop
         Result (I) := Add_Poly (A (I), B (I));
      end loop;
      return Result;
   end Add_Vector;

   function Sub_Vector (A, B : Module_Vector) return Module_Vector is
      Result : Module_Vector;
   begin
      for I in 0 .. K - 1 loop
         Result (I) := Sub_Poly (A (I), B (I));
      end loop;
      return Result;
   end Sub_Vector;

   function Mat_Vec_Mul (A : Module_Matrix; V : Module_Vector)
      return Module_Vector
   is
      Result : Module_Vector := Zero_Vector;
      Term   : Polynomial;
   begin
      for I in 0 .. K - 1 loop
         for J in 0 .. K - 1 loop
            Term := Mul_Poly (A (I, J), V (J));
            Result (I) := Add_Poly (Result (I), Term);
         end loop;
      end loop;
      return Result;
   end Mat_Vec_Mul;

   ---------------------------------------------------------------------------
   --  Key Generation
   ---------------------------------------------------------------------------

   procedure Generate_Key (
      Seed : Byte_Array;
      CK   : out Commitment_Key
   ) is
      --  Use SHAKE256 to expand seed into matrix elements
      --  This is deterministic and reproducible
      Counter : Natural := 0;
   begin
      --  Initialize to zero
      for I in 0 .. K - 1 loop
         for J in 0 .. K - 1 loop
            CK.A1 (I, J) := Zero_Poly;
            CK.A2 (I, J) := Zero_Poly;
         end loop;
      end loop;

      --  For now, simple deterministic generation
      --  Real implementation would use SHAKE256
      for I in 0 .. K - 1 loop
         for J in 0 .. K - 1 loop
            for Coef in 0 .. N - 1 loop
               --  Simple PRNG based on seed
               Counter := (Counter * 1103515245 + 12345) mod (2**31);
               Counter := Counter xor Natural (Seed (Coef mod 32));
               CK.A1 (I, J) (Coef) := Field_Element (Counter mod Natural (Q));

               Counter := (Counter * 1103515245 + 12345) mod (2**31);
               CK.A2 (I, J) (Coef) := Field_Element (Counter mod Natural (Q));
            end loop;
         end loop;
      end loop;
   end Generate_Key;

   ---------------------------------------------------------------------------
   --  Commitment Operations
   ---------------------------------------------------------------------------

   procedure Commit (
      CK         : Commitment_Key;
      Message    : Module_Vector;
      Randomness : Module_Vector;
      Result     : out Ajtai_Commitment
   ) is
      A1_m : Module_Vector;
      A2_r : Module_Vector;
   begin
      --  C = A1*m + A2*r
      A1_m := Mat_Vec_Mul (CK.A1, Message);
      A2_r := Mat_Vec_Mul (CK.A2, Randomness);
      Result.Value := Add_Vector (A1_m, A2_r);
   end Commit;

   procedure Commit_Bytes (
      CK         : Commitment_Key;
      Data       : Byte_Array;
      Randomness : Byte_Array;
      Result     : out Ajtai_Commitment;
      Opening    : out Commitment_Opening;
      Success    : out Boolean
   ) is
      Msg_Vec : Module_Vector := Zero_Vector;
      Rand_Vec : Module_Vector := Zero_Vector;
      Counter : Natural := 0;
   begin
      Success := True;

      --  Convert data to message vector (binary encoding)
      for I in Data'Range loop
         declare
            Byte_Val : constant Byte := Data (I);
            Bit_Offset : Natural := (I - Data'First) * 8;
            Poly_Idx : Natural;
            Coef_Idx : Natural;
         begin
            for Bit in 0 .. 7 loop
               if Bit_Offset + Bit < K * N then
                  Poly_Idx := (Bit_Offset + Bit) / N;
                  Coef_Idx := (Bit_Offset + Bit) mod N;
                  if Poly_Idx < K then
                     Msg_Vec (Poly_Idx) (Coef_Idx) :=
                        Field_Element ((Shift_Right (Byte_Val, Bit)) and 1);
                  end if;
               end if;
            end loop;
         end;
      end loop;

      --  Generate randomness vector from seed
      for I in 0 .. K - 1 loop
         for J in 0 .. N - 1 loop
            Counter := (Counter * 1103515245 + 12345) mod (2**31);
            Counter := Counter xor Natural (Randomness (J mod 32));
            --  Small coefficients: {-2, -1, 0, 1, 2}
            declare
               Val : constant Integer := (Counter mod 5) - 2;
            begin
               if Val >= 0 then
                  Rand_Vec (I) (J) := Field_Element (Val);
               else
                  Rand_Vec (I) (J) := Field_Element (Natural (Q) + Val);
               end if;
            end;
         end loop;
      end loop;

      --  Compute commitment
      Commit (CK, Msg_Vec, Rand_Vec, Result);

      --  Store opening
      Opening := (
         Message    => Msg_Vec,
         Randomness => Rand_Vec,
         Norm_Bound => Norm_Bound_Randomness
      );
   end Commit_Bytes;

   function Verify_Opening (
      CK         : Commitment_Key;
      Commitment : Ajtai_Commitment;
      Opening    : Commitment_Opening
   ) return Boolean
   is
      Recomputed : Ajtai_Commitment;
   begin
      --  Check norm bounds
      if not Has_Small_Norm (Opening.Message, Norm_Bound_Message) then
         return False;
      end if;
      if not Has_Small_Norm (Opening.Randomness, Opening.Norm_Bound) then
         return False;
      end if;

      --  Recompute commitment
      Commit (CK, Opening.Message, Opening.Randomness, Recomputed);

      --  Compare
      for I in 0 .. K - 1 loop
         for J in 0 .. N - 1 loop
            if Recomputed.Value (I) (J) /= Commitment.Value (I) (J) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Verify_Opening;

   ---------------------------------------------------------------------------
   --  Homomorphic Operations
   ---------------------------------------------------------------------------

   procedure Add_Commitment (
      C1     : Ajtai_Commitment;
      C2     : Ajtai_Commitment;
      Result : out Ajtai_Commitment
   ) is
   begin
      Result.Value := Add_Vector (C1.Value, C2.Value);
   end Add_Commitment;

   procedure Sub_Commitment (
      C1     : Ajtai_Commitment;
      C2     : Ajtai_Commitment;
      Result : out Ajtai_Commitment
   ) is
   begin
      Result.Value := Sub_Vector (C1.Value, C2.Value);
   end Sub_Commitment;

   procedure Scalar_Mult (
      Scalar : Field_Element;
      C      : Ajtai_Commitment;
      Result : out Ajtai_Commitment
   ) is
   begin
      for I in 0 .. K - 1 loop
         for J in 0 .. N - 1 loop
            Result.Value (I) (J) := Mod_Mul (Scalar, C.Value (I) (J));
         end loop;
      end loop;
   end Scalar_Mult;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   procedure Bytes_To_Vector (
      Data   : Byte_Array;
      Result : out Module_Vector
   ) is
   begin
      Result := Zero_Vector;
      for I in Data'Range loop
         declare
            Byte_Val : constant Byte := Data (I);
            Bit_Offset : Natural := (I - Data'First) * 8;
            Poly_Idx : Natural;
            Coef_Idx : Natural;
         begin
            for Bit in 0 .. 7 loop
               if Bit_Offset + Bit < K * N then
                  Poly_Idx := (Bit_Offset + Bit) / N;
                  Coef_Idx := (Bit_Offset + Bit) mod N;
                  if Poly_Idx < K then
                     Result (Poly_Idx) (Coef_Idx) :=
                        Field_Element ((Shift_Right (Byte_Val, Bit)) and 1);
                  end if;
               end if;
            end loop;
         end;
      end loop;
   end Bytes_To_Vector;

   procedure Vector_To_Bytes (
      Vec    : Module_Vector;
      Result : out Byte_Array;
      Length : out Natural
   ) is
      Byte_Val : Byte;
      Bit_Idx : Natural;
   begin
      Result := (others => 0);
      Length := K * N / 8;

      for B in 0 .. Length - 1 loop
         Byte_Val := 0;
         for Bit in 0 .. 7 loop
            Bit_Idx := B * 8 + Bit;
            declare
               Poly_Idx : constant Natural := Bit_Idx / N;
               Coef_Idx : constant Natural := Bit_Idx mod N;
            begin
               if Poly_Idx < K and then Vec (Poly_Idx) (Coef_Idx) mod 2 = 1 then
                  Byte_Val := Byte_Val or Shift_Left (1, Bit);
               end if;
            end;
         end loop;
         Result (Result'First + B) := Byte_Val;
      end loop;
   end Vector_To_Bytes;

   function Has_Small_Norm (
      Vec   : Module_Vector;
      Bound : Natural
   ) return Boolean
   is
   begin
      for I in 0 .. K - 1 loop
         for J in 0 .. N - 1 loop
            declare
               Val : constant Field_Element := Vec (I) (J);
               Centered : Integer;
            begin
               --  Center coefficient: map to [-q/2, q/2)
               if Val > Q / 2 then
                  Centered := Integer (Val) - Integer (Q);
               else
                  Centered := Integer (Val);
               end if;
               if abs (Centered) > Bound then
                  return False;
               end if;
            end;
         end loop;
      end loop;
      return True;
   end Has_Small_Norm;

   procedure Serialize_Commitment (
      C      : Ajtai_Commitment;
      Output : out Byte_Array;
      Length : out Natural
   ) is
      Pos : Natural := 0;
   begin
      Output := (others => 0);

      --  Serialize each polynomial coefficient as 12 bits
      for I in 0 .. K - 1 loop
         for J in 0 .. N - 1 loop
            declare
               Val : constant Unsigned_16 := Unsigned_16 (C.Value (I) (J));
               Byte_Idx : constant Natural := (Pos * 12) / 8;
               Bit_Offset : constant Natural := (Pos * 12) mod 8;
            begin
               if Byte_Idx + 1 < Output'Length then
                  Output (Output'First + Byte_Idx) :=
                     Output (Output'First + Byte_Idx) or
                     Byte (Shift_Left (Val, Bit_Offset) and 16#FF#);
                  Output (Output'First + Byte_Idx + 1) :=
                     Byte (Shift_Right (Val, 8 - Bit_Offset) and 16#FF#);
               end if;
               Pos := Pos + 1;
            end;
         end loop;
      end loop;

      Length := (K * N * 12 + 7) / 8;
   end Serialize_Commitment;

   procedure Deserialize_Commitment (
      Data   : Byte_Array;
      C      : out Ajtai_Commitment;
      Success : out Boolean
   ) is
      Pos : Natural := 0;
      Required_Len : constant Natural := (K * N * 12 + 7) / 8;
   begin
      C := Zero_Commitment;
      Success := False;

      if Data'Length < Required_Len then
         return;
      end if;

      --  Deserialize each 12-bit coefficient
      for I in 0 .. K - 1 loop
         for J in 0 .. N - 1 loop
            declare
               Byte_Idx : constant Natural := (Pos * 12) / 8;
               Bit_Offset : constant Natural := (Pos * 12) mod 8;
               Val : Unsigned_16;
            begin
               if Byte_Idx + 1 < Data'Length then
                  Val := Shift_Right (Unsigned_16 (Data (Data'First + Byte_Idx)),
                                      Bit_Offset);
                  Val := Val or Shift_Left (
                     Unsigned_16 (Data (Data'First + Byte_Idx + 1)),
                     8 - Bit_Offset);
                  Val := Val and 16#FFF#;
                  if Val >= Unsigned_16 (Q) then
                     return;  -- Invalid coefficient
                  end if;
                  C.Value (I) (J) := Field_Element (Val);
               end if;
               Pos := Pos + 1;
            end;
         end loop;
      end loop;

      Success := True;
   end Deserialize_Commitment;

end Anubis_Ajtai;
