pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_MLDSA_Poly with
   SPARK_Mode => On
is

   --  Polynomial addition
   procedure Poly_Add (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) is
   begin
      --  Initialize all to 0 first to satisfy prover
      Result := (others => 0);
      for I in Poly_Index loop
         --  Use Freeze to ensure inputs are in Valid_Field range
         --  Add returns Valid_Field which is < Q (by postcondition)
         Result (I) := Add (Freeze (A (I)), Freeze (B (I)));
         pragma Loop_Invariant
           (for all J in Poly_Index => Result (J) < Q);
      end loop;
   end Poly_Add;

   --  Polynomial subtraction
   procedure Poly_Sub (
      A, B   : in  Polynomial;
      Result : out Polynomial
   ) is
   begin
      --  Initialize all to 0 first to satisfy prover
      Result := (others => 0);
      for I in Poly_Index loop
         --  Use Freeze to ensure inputs are in Valid_Field range
         --  Sub returns Valid_Field which is < Q (by postcondition)
         Result (I) := Sub (Freeze (A (I)), Freeze (B (I)));
         pragma Loop_Invariant
           (for all J in Poly_Index => Result (J) < Q);
      end loop;
   end Poly_Sub;

   --  Polynomial coefficient reduction
   procedure Poly_Reduce (F : in Out Polynomial) is
   begin
      for I in Poly_Index loop
         --  Freeze returns Valid_Field which is < Q (by postcondition)
         F (I) := Freeze (F (I));
         pragma Loop_Invariant
           (for all J in 0 .. I => F (J) < Q);
      end loop;
   end Poly_Reduce;

   --  Convert polynomial from Montgomery form to standard form
   procedure Poly_From_Mont (F : in out Polynomial) is
   begin
      for I in Poly_Index loop
         F (I) := From_Mont (F (I));
      end loop;
   end Poly_From_Mont;

   --  Polynomial left shift
   --  Shifts each coefficient left by Shift bits, reducing mod Q
   procedure Poly_ShiftL (
      A      : in  Polynomial;
      Shift  : in  Natural;
      Result : out Polynomial
   ) is
      --  Use Shift_Left on 64-bit value to avoid 2**Shift overflow
      Mult : constant Unsigned_64 := Shift_Left (Unsigned_64 (1), Shift);
      R : Unsigned_64;
   begin
      --  Initialize all to 0 first to satisfy prover
      Result := (others => 0);
      for I in Poly_Index loop
         --  Compute (A(I) * 2^Shift) mod Q
         R := (Unsigned_64 (A (I)) * Mult) mod Unsigned_64 (Q);
         pragma Assert (R < Unsigned_64 (Q));
         Result (I) := Field_Element (R);
         pragma Loop_Invariant
           (for all J in Poly_Index => Result (J) < Q);
      end loop;
   end Poly_ShiftL;

   --  Check polynomial norm bound (infinity norm)
   function Poly_Chk_Norm (F : Polynomial; Bound : Natural) return Boolean is
      Centered : Integer;
   begin
      for I in Poly_Index loop
         Centered := Center (Valid_Field (Freeze (F (I))));
         if abs (Centered) >= Bound then
            return False;
         end if;
      end loop;
      return True;
   end Poly_Chk_Norm;

   --  L-vector operations
   procedure Vec_Add_L (
      A, B   : in  Poly_Vector_L;
      Result : out Poly_Vector_L
   ) is
   begin
      for I in L_Index loop
         Poly_Add (A (I), B (I), Result (I));
      end loop;
   end Vec_Add_L;

   procedure Vec_Sub_L (
      A, B   : in  Poly_Vector_L;
      Result : out Poly_Vector_L
   ) is
   begin
      for I in L_Index loop
         Poly_Sub (A (I), B (I), Result (I));
      end loop;
   end Vec_Sub_L;

   --  K-vector operations
   procedure Vec_Add_K (
      A, B   : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) is
   begin
      for I in K_Index loop
         Poly_Add (A (I), B (I), Result (I));
      end loop;
   end Vec_Add_K;

   procedure Vec_Sub_K (
      A, B   : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) is
   begin
      for I in K_Index loop
         Poly_Sub (A (I), B (I), Result (I));
      end loop;
   end Vec_Sub_K;

   procedure Vec_Reduce_K (V : in Out Poly_Vector_K) is
   begin
      for I in K_Index loop
         Poly_Reduce (V (I));
      end loop;
   end Vec_Reduce_K;

   procedure Vec_Reduce_L (V : in Out Poly_Vector_L) is
   begin
      for I in L_Index loop
         Poly_Reduce (V (I));
      end loop;
   end Vec_Reduce_L;

   procedure Vec_From_Mont_K (V : in out Poly_Vector_K) is
   begin
      for I in K_Index loop
         Poly_From_Mont (V (I));
      end loop;
   end Vec_From_Mont_K;

   procedure Vec_From_Mont_L (V : in out Poly_Vector_L) is
   begin
      for I in L_Index loop
         Poly_From_Mont (V (I));
      end loop;
   end Vec_From_Mont_L;

   --  Center polynomial coefficients to [-Q/2, Q/2)
   procedure Poly_Center (F : in out Polynomial) is
      Half_Q : constant := (Q - 1) / 2;
   begin
      for I in Poly_Index loop
         if F (I) > Half_Q then
            F (I) := F (I) - Q;
         end if;
      end loop;
   end Poly_Center;

   procedure Vec_Center_K (V : in out Poly_Vector_K) is
   begin
      for I in K_Index loop
         Poly_Center (V (I));
      end loop;
   end Vec_Center_K;

   procedure Vec_Center_L (V : in out Poly_Vector_L) is
   begin
      for I in L_Index loop
         Poly_Center (V (I));
      end loop;
   end Vec_Center_L;

   function Vec_Chk_Norm_L (V : Poly_Vector_L; Bound : Natural) return Boolean is
   begin
      for I in L_Index loop
         if not Poly_Chk_Norm (V (I), Bound) then
            return False;
         end if;
      end loop;
      return True;
   end Vec_Chk_Norm_L;

   function Vec_Chk_Norm_K (V : Poly_Vector_K; Bound : Natural) return Boolean is
   begin
      for I in K_Index loop
         if not Poly_Chk_Norm (V (I), Bound) then
            return False;
         end if;
      end loop;
      return True;
   end Vec_Chk_Norm_K;

   --  Apply Power2Round to polynomial
   procedure Poly_Power2Round (
      A  : in  Polynomial;
      A1 : out Polynomial;
      A0 : out Polynomial
   ) is
      High : Valid_Field;
      Low : Integer;
   begin
      for I in Poly_Index loop
         Power2Round (Valid_Field (Freeze (A (I))), High, Low);
         A1 (I) := High;
         --  Convert centered low to unsigned
         if Low >= 0 then
            A0 (I) := Field_Element (Low);
         else
            A0 (I) := Field_Element (Q + Low);
         end if;
      end loop;
   end Poly_Power2Round;

   --  Apply Power2Round to K-vector
   procedure Vec_Power2Round_K (
      V  : in  Poly_Vector_K;
      V1 : out Poly_Vector_K;
      V0 : out Poly_Vector_K
   ) is
   begin
      for I in K_Index loop
         Poly_Power2Round (V (I), V1 (I), V0 (I));
      end loop;
   end Vec_Power2Round_K;

   --  Apply Decompose to polynomial
   procedure Poly_Decompose (
      A  : in  Polynomial;
      A1 : out Polynomial;
      A0 : out Polynomial
   ) is
      High : Valid_Field;
      Low : Integer;
   begin
      for I in Poly_Index loop
         Decompose (Valid_Field (Freeze (A (I))), High, Low);
         A1 (I) := High;
         if Low >= 0 then
            A0 (I) := Field_Element (Low);
         else
            A0 (I) := Field_Element (Q + Low);
         end if;
      end loop;
   end Poly_Decompose;

   --  Compute HighBits of K-vector
   procedure Vec_HighBits_K (
      V      : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) is
      Dummy : Polynomial;
   begin
      for I in K_Index loop
         Poly_Decompose (V (I), Result (I), Dummy);
      end loop;
   end Vec_HighBits_K;

   --  Compute LowBits of K-vector
   procedure Vec_LowBits_K (
      V      : in  Poly_Vector_K;
      Result : out Poly_Vector_K
   ) is
      Dummy : Polynomial;
   begin
      for I in K_Index loop
         Poly_Decompose (V (I), Dummy, Result (I));
      end loop;
   end Vec_LowBits_K;

   --  Make hint for K-vector pair
   procedure Vec_MakeHint_K (
      Z, R   : in  Poly_Vector_K;
      Hint   : out Poly_Vector_K;
      Count  : out Natural
   ) is
      H : Boolean;
      --  Maximum possible hints is K * N = 8 * 256 = 2048
      Max_Count : constant := K * N;
   begin
      Count := 0;
      --  Initialize all hint values to 0 first
      Hint := (others => (others => 0));

      for I in K_Index loop
         for J in Poly_Index loop
            H := MakeHint (Freeze (Z (I) (J)), Freeze (R (I) (J)));
            if H then
               Hint (I) (J) := 1;
               --  Guard against overflow (shouldn"t happen but proves correctness)
               if Count < Max_Count then
                  Count := Count + 1;
               end if;
            end if;
         end loop;
      end loop;
   end Vec_MakeHint_K;

   --  Use hint for K-vector
   procedure Vec_UseHint_K (
      Hint, R : in  Poly_Vector_K;
      Result  : out Poly_Vector_K
   ) is
   begin
      --  Initialize result first to ensure all elements are set
      Result := (others => (others => 0));

      for I in K_Index loop
         for J in Poly_Index loop
            Result (I) (J) := UseHint (Hint (I) (J) = 1, Freeze (R (I) (J)));
         end loop;
      end loop;
   end Vec_UseHint_K;

end Anubis_MLDSA_Poly;
