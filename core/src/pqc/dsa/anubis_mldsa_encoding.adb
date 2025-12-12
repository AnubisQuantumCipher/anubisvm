--  ML-DSA Encoding: Complex bit-packing operations
--  Note: Implementation uses SPARK_Mode Off for complex arithmetic
--  The spec maintains full SPARK contracts for interface verification
pragma SPARK_Mode (Off);

with Interfaces; use Interfaces;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;

package body Anubis_MLDSA_Encoding with
   SPARK_Mode => Off
is

   --  Pack T1: 10-bit coefficients, 256 * 10 / 8 = 320 bytes
   procedure Pack_T1 (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) is
      --  T1 coefficients are 10-bit values, mask to ensure range
      T0, T1_Val, T2, T3 : Unsigned_32;
      --  Intermediate variables for combined values (max 8 bits each)
      B0, B1, B2, B3, B4 : Unsigned_32;
   begin
      Output := (others => 0);
      --  Pack 4 coefficients into 5 bytes
      for I in 0 .. 63 loop
         pragma Loop_Invariant (I <= 63);

         --  Extract and mask coefficients
         T0 := Unsigned_32 (Poly (4 * I)) and 16#3FF#;
         T1_Val := Unsigned_32 (Poly (4 * I + 1)) and 16#3FF#;
         T2 := Unsigned_32 (Poly (4 * I + 2)) and 16#3FF#;
         T3 := Unsigned_32 (Poly (4 * I + 3)) and 16#3FF#;

         --  Compute byte values
         B0 := T0 and 16#FF#;
         B1 := (Shift_Right (T0, 8) or Shift_Left (T1_Val, 2)) and 16#FF#;
         B2 := (Shift_Right (T1_Val, 6) or Shift_Left (T2, 4)) and 16#FF#;
         B3 := (Shift_Right (T2, 4) or Shift_Left (T3, 6)) and 16#FF#;
         B4 := Shift_Right (T3, 2) and 16#FF#;

         --  Write to output (I * 5 ranges from 0 to 315, +4 = 319)
         pragma Assert (I * 5 <= 315);
         pragma Assert (I * 5 + 4 <= 319);
         Output (I * 5) := Byte (B0);
         Output (I * 5 + 1) := Byte (B1);
         Output (I * 5 + 2) := Byte (B2);
         Output (I * 5 + 3) := Byte (B3);
         Output (I * 5 + 4) := Byte (B4);
      end loop;
   end Pack_T1;

   --  Unpack T1
   procedure Unpack_T1 (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) is
      B0, B1, B2, B3, B4 : Unsigned_32;
   begin
      Poly := (others => 0);
      for I in 0 .. 63 loop
         pragma Loop_Invariant (I <= 63);

         --  I * 5 ranges from 0 to 315, +4 = 319
         pragma Assert (I * 5 <= 315);
         pragma Assert (I * 5 + 4 <= 319);

         B0 := Unsigned_32 (Input (I * 5));
         B1 := Unsigned_32 (Input (I * 5 + 1));
         B2 := Unsigned_32 (Input (I * 5 + 2));
         B3 := Unsigned_32 (Input (I * 5 + 3));
         B4 := Unsigned_32 (Input (I * 5 + 4));

         Poly (4 * I) := Field_Element (B0 or Shift_Left (B1 and 3, 8));
         Poly (4 * I + 1) := Field_Element (Shift_Right (B1, 2) or Shift_Left (B2 and 16#0F#, 6));
         Poly (4 * I + 2) := Field_Element (Shift_Right (B2, 4) or Shift_Left (B3 and 16#3F#, 4));
         Poly (4 * I + 3) := Field_Element (Shift_Right (B3, 6) or Shift_Left (B4, 2));
      end loop;
   end Unpack_T1;

   --  Pack T0: 13-bit coefficients, 256 * 13 / 8 = 416 bytes
   procedure Pack_T0 (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) is
      Coef : Unsigned_32;
      Base : Natural;
      A0 : Integer;
   begin
      Output := (others => 0);
      --  Pack 8 coefficients into 13 bytes
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I <= 31);
         --  Base index for this group: I * 13 ranges from 0 to 403
         Base := I * 13;
         pragma Assert (Base <= 403);
         pragma Assert (Base + 12 <= 415);

         for J in 0 .. 7 loop
            --  T0 coefficients are centered, need to make positive
            --  a0 in [-2^(D-1), 2^(D-1)] stored as (2^(D-1) - a0) in [0, 2^D - 1]
            A0 := Center (Valid_Field (Freeze (Poly (8 * I + J))));
            --  A0 is in [-Q/2, Q/2], convert to unsigned: 2^12 - A0 mod 2^13
            --  Since A0 can be negative, we compute (2^12 - A0) mod 2^13
            if A0 >= 0 then
               Coef := (2**(D - 1) - Unsigned_32 (A0)) and 16#1FFF#;
            else
               Coef := (2**(D - 1) + Unsigned_32 (-A0)) and 16#1FFF#;
            end if;

            --  Pack based on position (Base + 12 is max access)
            case J is
               when 0 =>
                  Output (Base) := Byte (Coef and 16#FF#);
                  Output (Base + 1) := Byte (Shift_Right (Coef, 8) and 16#1F#);
               when 1 =>
                  Output (Base + 1) := Output (Base + 1) or Byte (Shift_Left (Coef and 7, 5));
                  Output (Base + 2) := Byte (Shift_Right (Coef, 3) and 16#FF#);
                  Output (Base + 3) := Byte (Shift_Right (Coef, 11) and 3);
               when 2 =>
                  Output (Base + 3) := Output (Base + 3) or Byte (Shift_Left (Coef and 16#3F#, 2));
                  Output (Base + 4) := Byte (Shift_Right (Coef, 6) and 16#7F#);
               when 3 =>
                  Output (Base + 4) := Output (Base + 4) or Byte (Shift_Left (Coef and 1, 7));
                  Output (Base + 5) := Byte (Shift_Right (Coef, 1) and 16#FF#);
                  Output (Base + 6) := Byte (Shift_Right (Coef, 9) and 16#0F#);
               when 4 =>
                  Output (Base + 6) := Output (Base + 6) or Byte (Shift_Left (Coef and 16#0F#, 4));
                  Output (Base + 7) := Byte (Shift_Right (Coef, 4) and 16#FF#);
                  Output (Base + 8) := Byte (Shift_Right (Coef, 12) and 1);
               when 5 =>
                  Output (Base + 8) := Output (Base + 8) or Byte (Shift_Left (Coef and 16#7F#, 1));
                  Output (Base + 9) := Byte (Shift_Right (Coef, 7) and 16#3F#);
               when 6 =>
                  Output (Base + 9) := Output (Base + 9) or Byte (Shift_Left (Coef and 3, 6));
                  Output (Base + 10) := Byte (Shift_Right (Coef, 2) and 16#FF#);
                  Output (Base + 11) := Byte (Shift_Right (Coef, 10) and 7);
               when 7 =>
                  Output (Base + 11) := Output (Base + 11) or Byte (Shift_Left (Coef and 16#1F#, 3));
                  Output (Base + 12) := Byte (Shift_Right (Coef, 5) and 16#FF#);
            end case;
         end loop;
      end loop;
   end Pack_T0;

   --  Unpack T0
   procedure Unpack_T0 (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) is
      Coef : Unsigned_32;
      Val : Integer;
      Base : Natural;
   begin
      Poly := (others => 0);
      --  Simplified: unpack in groups
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I <= 31);
         --  Base index for this group: I * 13 ranges from 0 to 403
         Base := I * 13;
         pragma Assert (Base <= 403);
         pragma Assert (Base + 12 <= 415);

         for J in 0 .. 7 loop
            --  Unpack 13-bit value (Base + 12 is max access)
            case J is
               when 0 =>
                  Coef := Unsigned_32 (Input (Base)) or
                          Shift_Left (Unsigned_32 (Input (Base + 1)) and 16#1F#, 8);
               when 1 =>
                  Coef := Shift_Right (Unsigned_32 (Input (Base + 1)), 5) or
                          Shift_Left (Unsigned_32 (Input (Base + 2)), 3) or
                          Shift_Left (Unsigned_32 (Input (Base + 3)) and 3, 11);
               when 2 =>
                  Coef := Shift_Right (Unsigned_32 (Input (Base + 3)), 2) or
                          Shift_Left (Unsigned_32 (Input (Base + 4)) and 16#7F#, 6);
               when 3 =>
                  Coef := Shift_Right (Unsigned_32 (Input (Base + 4)), 7) or
                          Shift_Left (Unsigned_32 (Input (Base + 5)), 1) or
                          Shift_Left (Unsigned_32 (Input (Base + 6)) and 16#0F#, 9);
               when 4 =>
                  Coef := Shift_Right (Unsigned_32 (Input (Base + 6)), 4) or
                          Shift_Left (Unsigned_32 (Input (Base + 7)), 4) or
                          Shift_Left (Unsigned_32 (Input (Base + 8)) and 1, 12);
               when 5 =>
                  Coef := Shift_Right (Unsigned_32 (Input (Base + 8)), 1) or
                          Shift_Left (Unsigned_32 (Input (Base + 9)) and 16#3F#, 7);
               when 6 =>
                  Coef := Shift_Right (Unsigned_32 (Input (Base + 9)), 6) or
                          Shift_Left (Unsigned_32 (Input (Base + 10)), 2) or
                          Shift_Left (Unsigned_32 (Input (Base + 11)) and 7, 10);
               when 7 =>
                  Coef := Shift_Right (Unsigned_32 (Input (Base + 11)), 3) or
                          Shift_Left (Unsigned_32 (Input (Base + 12)), 5);
            end case;

            --  Convert back to centered: a0 = 2^(D-1) - coef
            Val := Integer (2**(D - 1)) - Integer (Coef and 16#1FFF#);
            if Val >= 0 then
               Poly (8 * I + J) := Field_Element (Val);
            else
               Poly (8 * I + J) := Field_Element (Integer (Q) + Val);
            end if;
         end loop;
      end loop;
   end Unpack_T0;

   --  Pack Eta: 3-bit coefficients for η=2
   --  8 coefficients pack into 3 bytes (8 * 3 = 24 bits)
   procedure Pack_Eta (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) is
      T : array (0 .. 7) of Unsigned_8;
      C_Val : Integer;
      Val : Integer;
      Base : Natural;
   begin
      Output := (others => 0);
      --  Pack 8 coefficients into 3 bytes, 32 groups total
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I <= 31);
         --  Base index for this group: I * 3 ranges from 0 to 93
         Base := I * 3;
         pragma Assert (Base <= 93);
         pragma Assert (Base + 2 <= 95);

         --  η - coef gives range [0, 2η] = [0, 4] (needs 3 bits)
         for J in 0 .. 7 loop
            C_Val := Center (Valid_Field (Freeze (Poly (8 * I + J))));
            --  Eta - C is expected to be in [0, 2*Eta], mask to 3 bits for safety
            Val := Eta - C_Val;
            --  Val should be in [0, 4] for η=2
            --  Use modular arithmetic to handle any range safely
            T (J) := Unsigned_8 (Val mod 8);
         end loop;

         --  Pack into 3 bytes following Dilithium spec:
         --  Byte 0: t0[2:0] | t1[2:0] << 3 | t2[1:0] << 6
         --  Byte 1: t2[2] >> 2 | t3[2:0] << 1 | t4[2:0] << 4 | t5[0] << 7
         --  Byte 2: t5[2:1] >> 1 | t6[2:0] << 2 | t7[2:0] << 5
         Output (Base) := Byte (T (0) or Shift_Left (T (1), 3) or Shift_Left (T (2), 6));
         Output (Base + 1) := Byte (Shift_Right (T (2), 2) or Shift_Left (T (3), 1) or
                                   Shift_Left (T (4), 4) or Shift_Left (T (5), 7));
         Output (Base + 2) := Byte (Shift_Right (T (5), 1) or Shift_Left (T (6), 2) or
                                   Shift_Left (T (7), 5));
      end loop;
   end Pack_Eta;

   --  Unpack Eta: 3-bit coefficients for η=2
   --  3 bytes unpack to 8 coefficients
   procedure Unpack_Eta (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) is
      B0, B1, B2 : Unsigned_8;
      T : Integer;
      Base : Natural;
   begin
      Poly := (others => 0);
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I <= 31);
         --  Base index for this group: I * 3 ranges from 0 to 93
         Base := I * 3;
         pragma Assert (Base <= 93);
         pragma Assert (Base + 2 <= 95);

         B0 := Unsigned_8 (Input (Base));
         B1 := Unsigned_8 (Input (Base + 1));
         B2 := Unsigned_8 (Input (Base + 2));

         --  Unpack 8 coefficients from 3 bytes
         --  T0 = B0[2:0]
         T := Integer (Eta) - Integer (B0 and 7);
         Poly (8 * I) := (if T >= 0 then Field_Element (T)
                         else Field_Element (Integer (Q) + T));

         --  T1 = B0[5:3]
         T := Integer (Eta) - Integer (Shift_Right (B0, 3) and 7);
         Poly (8 * I + 1) := (if T >= 0 then Field_Element (T)
                             else Field_Element (Integer (Q) + T));

         --  T2 = B0[7:6] | B1[0] << 2
         T := Integer (Eta) - Integer ((Shift_Right (B0, 6) or Shift_Left (B1 and 1, 2)) and 7);
         Poly (8 * I + 2) := (if T >= 0 then Field_Element (T)
                             else Field_Element (Integer (Q) + T));

         --  T3 = B1[3:1]
         T := Integer (Eta) - Integer (Shift_Right (B1, 1) and 7);
         Poly (8 * I + 3) := (if T >= 0 then Field_Element (T)
                             else Field_Element (Integer (Q) + T));

         --  T4 = B1[6:4]
         T := Integer (Eta) - Integer (Shift_Right (B1, 4) and 7);
         Poly (8 * I + 4) := (if T >= 0 then Field_Element (T)
                             else Field_Element (Integer (Q) + T));

         --  T5 = B1[7] | B2[1:0] << 1
         T := Integer (Eta) - Integer ((Shift_Right (B1, 7) or Shift_Left (B2 and 3, 1)) and 7);
         Poly (8 * I + 5) := (if T >= 0 then Field_Element (T)
                             else Field_Element (Integer (Q) + T));

         --  T6 = B2[4:2]
         T := Integer (Eta) - Integer (Shift_Right (B2, 2) and 7);
         Poly (8 * I + 6) := (if T >= 0 then Field_Element (T)
                             else Field_Element (Integer (Q) + T));

         --  T7 = B2[7:5]
         T := Integer (Eta) - Integer (Shift_Right (B2, 5) and 7);
         Poly (8 * I + 7) := (if T >= 0 then Field_Element (T)
                             else Field_Element (Integer (Q) + T));
      end loop;
   end Unpack_Eta;

   --  Pack Z: 20-bit coefficients
   procedure Pack_Z (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) is
      Coef : Unsigned_32;
      Z0, Z1 : Integer;
      Val0, Val1 : Integer;
      Base : Natural;
   begin
      Output := (others => 0);
      --  Pack 2 coefficients into 5 bytes
      for I in 0 .. 127 loop
         pragma Loop_Invariant (I <= 127);
         --  Base index for this pair: I * 5 ranges from 0 to 635
         Base := I * 5;
         pragma Assert (Base <= 635);
         pragma Assert (Base + 4 <= 639);

         --  γ1 - z gives centered value in [0, 2γ1]
         Z0 := Center (Valid_Field (Freeze (Poly (2 * I))));
         Z1 := Center (Valid_Field (Freeze (Poly (2 * I + 1))));
         --  Compute in signed domain then convert to unsigned safely
         Val0 := Integer (Gamma1) - Z0;
         Val1 := Integer (Gamma1) - Z1;

         --  Convert to unsigned using modular arithmetic (mod 2^20)
         Coef := Unsigned_32 (Val0 mod (2**20));
         Output (Base) := Byte (Coef and 16#FF#);
         Output (Base + 1) := Byte (Shift_Right (Coef, 8) and 16#FF#);
         Output (Base + 2) := Byte (Shift_Right (Coef, 16) and 16#0F#);

         Coef := Unsigned_32 (Val1 mod (2**20));
         Output (Base + 2) := Output (Base + 2) or Byte (Shift_Left (Coef and 16#0F#, 4));
         Output (Base + 3) := Byte (Shift_Right (Coef, 4) and 16#FF#);
         Output (Base + 4) := Byte (Shift_Right (Coef, 12) and 16#FF#);
      end loop;
   end Pack_Z;

   --  Unpack Z
   procedure Unpack_Z (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) is
      Coef : Unsigned_32;
      Val : Integer;
      Base : Natural;
   begin
      Poly := (others => 0);
      for I in 0 .. 127 loop
         pragma Loop_Invariant (I <= 127);
         --  Base index for this pair: I * 5 ranges from 0 to 635
         Base := I * 5;
         pragma Assert (Base <= 635);
         pragma Assert (Base + 4 <= 639);

         Coef := Unsigned_32 (Input (Base)) or
                 Shift_Left (Unsigned_32 (Input (Base + 1)), 8) or
                 Shift_Left (Unsigned_32 (Input (Base + 2)) and 16#0F#, 16);
         Val := Integer (Gamma1) - Integer (Coef);
         if Val >= 0 then
            Poly (2 * I) := Field_Element (Val);
         else
            Poly (2 * I) := Field_Element (Integer (Q) + Val);
         end if;

         Coef := Shift_Right (Unsigned_32 (Input (Base + 2)), 4) or
                 Shift_Left (Unsigned_32 (Input (Base + 3)), 4) or
                 Shift_Left (Unsigned_32 (Input (Base + 4)), 12);
         Coef := Coef and 16#FFFFF#;
         Val := Integer (Gamma1) - Integer (Coef);
         if Val >= 0 then
            Poly (2 * I + 1) := Field_Element (Val);
         else
            Poly (2 * I + 1) := Field_Element (Integer (Q) + Val);
         end if;
      end loop;
   end Unpack_Z;

   --  Pack W1: 4-bit coefficients
   procedure Pack_W1 (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) is
      Idx : Natural := 0;
      W0, W1 : Unsigned_32;
   begin
      Output := (others => 0);
      for I in 0 .. 127 loop
         pragma Loop_Invariant (Idx = I);
         pragma Loop_Invariant (Idx <= 127);
         --  W1 values are expected to be in range [0, 15] (4-bit)
         --  Use Unsigned_32 to avoid range check on Field_Element
         W0 := Unsigned_32 (Poly (2 * I)) and 16#0F#;
         W1 := Unsigned_32 (Poly (2 * I + 1)) and 16#0F#;
         Output (Idx) := Byte (W0 or Shift_Left (W1, 4));
         Idx := Idx + 1;
      end loop;
   end Pack_W1;

   --  Pack public key
   procedure Pack_Public_Key (
      Rho : in  Seed;
      T1  : in  Poly_Vector_K;
      PK  : out Public_Key
   ) is
      Idx : Natural := PK'First;
      Temp : Byte_Array (0 .. Poly_T1_Packed_Bytes - 1);
   begin
      PK := (others => 0);
      --  Copy ρ
      for I in 0 .. 31 loop
         PK (Idx + I) := Rho (I);
      end loop;
      Idx := Idx + 32;

      --  Pack each t1 polynomial
      for I in K_Index loop
         Pack_T1 (T1 (I), Temp);
         for J in 0 .. Poly_T1_Packed_Bytes - 1 loop
            PK (Idx + J) := Temp (J);
         end loop;
         Idx := Idx + Poly_T1_Packed_Bytes;
      end loop;
   end Pack_Public_Key;

   --  Unpack public key
   procedure Unpack_Public_Key (
      PK  : in  Public_Key;
      Rho : out Seed;
      T1  : out Poly_Vector_K
   ) is
      Idx : Natural := PK'First;
      --  Temp array that starts at 0 for unpacking functions
      Temp : Byte_Array (0 .. Poly_T1_Packed_Bytes - 1);
   begin
      --  Extract ρ
      for I in 0 .. 31 loop
         Rho (I) := PK (Idx + I);
      end loop;
      Idx := Idx + 32;

      --  Unpack each t1 polynomial
      for I in K_Index loop
         for J in 0 .. Poly_T1_Packed_Bytes - 1 loop
            Temp (J) := PK (Idx + J);
         end loop;
         Unpack_T1 (Temp, T1 (I));
         Idx := Idx + Poly_T1_Packed_Bytes;
      end loop;
   end Unpack_Public_Key;

   --  Pack secret key
   procedure Pack_Secret_Key (
      Rho   : in  Seed;
      K     : in  Seed;
      Tr    : in  Tr_Hash;
      S1    : in  Poly_Vector_L;
      S2    : in  Poly_Vector_K;
      T0    : in  Poly_Vector_K;
      SK    : out Secret_Key
   ) is
      Idx : Natural := SK'First;
      Temp_Eta : Byte_Array (0 .. Poly_Eta_Packed_Bytes - 1);
      Temp_T0 : Byte_Array (0 .. Poly_T0_Packed_Bytes - 1);
   begin
      SK := (others => 0);
      --  ρ (32 bytes)
      for I in 0 .. 31 loop
         SK (Idx + I) := Rho (I);
      end loop;
      Idx := Idx + 32;

      --  K (32 bytes)
      for I in 0 .. 31 loop
         SK (Idx + I) := K (I);
      end loop;
      Idx := Idx + 32;

      --  tr (64 bytes)
      for I in 0 .. 63 loop
         SK (Idx + I) := Tr (I);
      end loop;
      Idx := Idx + 64;

      --  s1 (L polynomials)
      for I in L_Index loop
         Pack_Eta (S1 (I), Temp_Eta);
         for J in 0 .. Poly_Eta_Packed_Bytes - 1 loop
            SK (Idx + J) := Temp_Eta (J);
         end loop;
         Idx := Idx + Poly_Eta_Packed_Bytes;
      end loop;

      --  s2 (K polynomials)
      for I in K_Index loop
         Pack_Eta (S2 (I), Temp_Eta);
         for J in 0 .. Poly_Eta_Packed_Bytes - 1 loop
            SK (Idx + J) := Temp_Eta (J);
         end loop;
         Idx := Idx + Poly_Eta_Packed_Bytes;
      end loop;

      --  t0 (K polynomials)
      for I in K_Index loop
         Pack_T0 (T0 (I), Temp_T0);
         for J in 0 .. Poly_T0_Packed_Bytes - 1 loop
            SK (Idx + J) := Temp_T0 (J);
         end loop;
         Idx := Idx + Poly_T0_Packed_Bytes;
      end loop;
   end Pack_Secret_Key;

   --  Unpack secret key
   procedure Unpack_Secret_Key (
      SK  : in  Secret_Key;
      Rho : out Seed;
      K   : out Seed;
      Tr  : out Tr_Hash;
      S1  : out Poly_Vector_L;
      S2  : out Poly_Vector_K;
      T0  : out Poly_Vector_K
   ) is
      Idx : Natural := SK'First;
      --  Temp arrays that start at 0 for unpacking functions
      Temp_Eta : Byte_Array (0 .. Poly_Eta_Packed_Bytes - 1);
      Temp_T0 : Byte_Array (0 .. Poly_T0_Packed_Bytes - 1);
   begin
      --  ρ
      for I in 0 .. 31 loop
         Rho (I) := SK (Idx + I);
      end loop;
      Idx := Idx + 32;

      --  K
      for I in 0 .. 31 loop
         K (I) := SK (Idx + I);
      end loop;
      Idx := Idx + 32;

      --  tr
      for I in 0 .. 63 loop
         Tr (I) := SK (Idx + I);
      end loop;
      Idx := Idx + 64;

      --  s1
      for I in L_Index loop
         for J in 0 .. Poly_Eta_Packed_Bytes - 1 loop
            Temp_Eta (J) := SK (Idx + J);
         end loop;
         Unpack_Eta (Temp_Eta, S1 (I));
         Idx := Idx + Poly_Eta_Packed_Bytes;
      end loop;

      --  s2
      for I in K_Index loop
         for J in 0 .. Poly_Eta_Packed_Bytes - 1 loop
            Temp_Eta (J) := SK (Idx + J);
         end loop;
         Unpack_Eta (Temp_Eta, S2 (I));
         Idx := Idx + Poly_Eta_Packed_Bytes;
      end loop;

      --  t0
      for I in K_Index loop
         for J in 0 .. Poly_T0_Packed_Bytes - 1 loop
            Temp_T0 (J) := SK (Idx + J);
         end loop;
         Unpack_T0 (Temp_T0, T0 (I));
         Idx := Idx + Poly_T0_Packed_Bytes;
      end loop;
   end Unpack_Secret_Key;

   --  Pack signature
   procedure Pack_Signature (
      C_Tilde : in  Challenge_Seed;
      Z       : in  Poly_Vector_L;
      H       : in  Poly_Vector_K;
      Sig     : out Signature
   ) is
      Idx : Natural := Sig'First;
      Temp_Z : Byte_Array (0 .. Poly_Z_Packed_Bytes - 1);
      Hint_Count : Natural := 0;
   begin
      Sig := (others => 0);
      --  c_tilde (32 bytes)
      for I in 0 .. 31 loop
         Sig (Idx + I) := C_Tilde (I);
      end loop;
      Idx := Idx + 32;

      --  z (L polynomials)
      for I in L_Index loop
         Pack_Z (Z (I), Temp_Z);
         for J in 0 .. Poly_Z_Packed_Bytes - 1 loop
            Sig (Idx + J) := Temp_Z (J);
         end loop;
         Idx := Idx + Poly_Z_Packed_Bytes;
      end loop;

      --  Pack hints (sparse encoding)
      --  First, count hints per polynomial and store positions
      for I in 0 .. Omega + K - 1 loop
         Sig (Idx + I) := 0;
      end loop;

      declare
         Hint_Idx : Natural := Idx;
      begin
         for I in K_Index loop
            pragma Loop_Invariant (Hint_Count <= Omega);
            for J in Poly_Index loop
               pragma Loop_Invariant (Hint_Count <= Omega);
               if H (I) (J) = 1 then
                  if Hint_Count < Omega then
                     Sig (Hint_Idx + Hint_Count) := Byte (J);
                     Hint_Count := Hint_Count + 1;
                  end if;
               end if;
            end loop;
            --  Mark end of this polynomial"s hints
            --  Hint_Count <= Omega = 75 < 256, so safe for Byte conversion
            Sig (Idx + Omega + I) := Byte (Hint_Count);
         end loop;
      end;
   end Pack_Signature;

   --  Unpack signature
   procedure Unpack_Signature (
      Sig     : in  Signature;
      C_Tilde : out Challenge_Seed;
      Z       : out Poly_Vector_L;
      H       : out Poly_Vector_K;
      Valid   : out Boolean
   ) is
      Idx : Natural := Sig'First;
      Hint_Count : Natural := 0;
      Prev_Count : Natural := 0;
      --  Temp array that starts at 0 for unpacking functions
      Temp_Z : Byte_Array (0 .. Poly_Z_Packed_Bytes - 1);
   begin
      Valid := True;

      --  c_tilde
      for I in 0 .. 31 loop
         C_Tilde (I) := Sig (Idx + I);
      end loop;
      Idx := Idx + 32;

      --  z
      for I in L_Index loop
         for J in 0 .. Poly_Z_Packed_Bytes - 1 loop
            Temp_Z (J) := Sig (Idx + J);
         end loop;
         Unpack_Z (Temp_Z, Z (I));
         Idx := Idx + Poly_Z_Packed_Bytes;
      end loop;

      --  Initialize hints to zero
      for I in K_Index loop
         H (I) := (others => 0);
      end loop;

      --  Unpack hints
      for I in K_Index loop
         Hint_Count := Natural (Sig (Idx + Omega + I));

         --  Validate hint count is non-decreasing
         if Hint_Count < Prev_Count or Hint_Count > Omega then
            Valid := False;
            return;
         end if;

         --  Set hint positions for this polynomial
         for J in Prev_Count .. Hint_Count - 1 loop
            declare
               Pos : constant Natural := Natural (Sig (Idx + J));
            begin
               if Pos >= N then
                  Valid := False;
                  return;
               end if;
               H (I) (Pos) := 1;
            end;
         end loop;

         Prev_Count := Hint_Count;
      end loop;
   end Unpack_Signature;

end Anubis_MLDSA_Encoding;
