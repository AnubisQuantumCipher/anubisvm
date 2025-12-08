--  Debug test for ML-DSA-87 signing

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;
with Anubis_MLDSA_NTT; use Anubis_MLDSA_NTT;
with Anubis_MLDSA_Poly; use Anubis_MLDSA_Poly;
with Anubis_MLDSA_Sample; use Anubis_MLDSA_Sample;
with Anubis_MLDSA_Encoding; use Anubis_MLDSA_Encoding;
with Anubis_SHA3; use Anubis_SHA3;

procedure Test_MLDSA_Debug is

   --  Manual NTT layer 1 only (len=128) for debugging
   procedure NTT_Layer1_Debug (F : in out Polynomial) is
      Len : constant := 128;
      K : Natural := 0;
      Start, J : Natural;
      Zeta : Field_Element;
      T : Field_Element;
   begin
      Start := 0;
      while Start < N loop
         K := K + 1;
         Zeta := Zetas (K);
         Put_Line ("    Layer1: K=" & Natural'Image (K) &
                   " Zeta=" & Field_Element'Image (Zeta) &
                   " Start=" & Natural'Image (Start));

         J := Start;
         while J < Start + Len loop
            if J < 4 then
               Put_Line ("      j=" & Natural'Image (J) &
                         " F(j)=" & Field_Element'Image (F (J)) &
                         " F(j+len)=" & Field_Element'Image (F (J + Len)));
            end if;
            T := Mont_Mul (Zeta, F (J + Len));
            F (J + Len) := Field_Element (
               (Unsigned_64 (F (J)) + Unsigned_64 (Q) - Unsigned_64 (T)) mod Unsigned_64 (Q));
            F (J) := Field_Element (
               (Unsigned_64 (F (J)) + Unsigned_64 (T)) mod Unsigned_64 (Q));
            if J < 4 then
               Put_Line ("        T=" & Field_Element'Image (T) &
                         " -> F(j)=" & Field_Element'Image (F (J)) &
                         " F(j+len)=" & Field_Element'Image (F (J + Len)));
            end if;
            J := J + 1;
         end loop;

         Start := Start + 2 * Len;
      end loop;
   end NTT_Layer1_Debug;

   procedure Test_NTT_Roundtrip is
      Poly_In : Polynomial := (others => 0);
      Poly_Out : Polynomial;
      All_Match : Boolean := True;
   begin
      Put_Line ("Testing NTT round-trip...");

      --  Detailed trace of [0, 1, 0, 0, ...] through first NTT layer
      Put_Line ("  Detailed NTT trace for X=[0,1,0,0,...]:");
      Poly_In := (others => 0);
      Poly_In (1) := 1;
      Poly_Out := Poly_In;
      Put_Line ("    Input[0..4,128..132]:");
      Put_Line ("      " & Field_Element'Image (Poly_Out (0)) &
                Field_Element'Image (Poly_Out (1)) &
                Field_Element'Image (Poly_Out (2)) &
                Field_Element'Image (Poly_Out (3)) &
                Field_Element'Image (Poly_Out (4)));
      Put_Line ("      " & Field_Element'Image (Poly_Out (128)) &
                Field_Element'Image (Poly_Out (129)) &
                Field_Element'Image (Poly_Out (130)) &
                Field_Element'Image (Poly_Out (131)) &
                Field_Element'Image (Poly_Out (132)));

      NTT_Layer1_Debug (Poly_Out);

      Put_Line ("    After layer1[0..4,128..132]:");
      Put_Line ("      " & Field_Element'Image (Poly_Out (0)) &
                Field_Element'Image (Poly_Out (1)) &
                Field_Element'Image (Poly_Out (2)) &
                Field_Element'Image (Poly_Out (3)) &
                Field_Element'Image (Poly_Out (4)));
      Put_Line ("      " & Field_Element'Image (Poly_Out (128)) &
                Field_Element'Image (Poly_Out (129)) &
                Field_Element'Image (Poly_Out (130)) &
                Field_Element'Image (Poly_Out (131)) &
                Field_Element'Image (Poly_Out (132)));

      --  Test 1: Constant polynomial [1, 0, 0, ...]
      Put_Line ("  Test 1: Constant [1, 0, 0, ...]");
      Poly_In := (others => 0);
      Poly_In (0) := 1;
      Poly_Out := Poly_In;
      NTT (Poly_Out);
      Put_Line ("    After NTT[0..3]: " & Field_Element'Image (Poly_Out (0)) &
                ", " & Field_Element'Image (Poly_Out (1)) &
                ", " & Field_Element'Image (Poly_Out (2)) &
                ", " & Field_Element'Image (Poly_Out (3)));
      INTT (Poly_Out);
      Put_Line ("    After INTT[0..3]: " & Field_Element'Image (Poly_Out (0)) &
                ", " & Field_Element'Image (Poly_Out (1)) &
                ", " & Field_Element'Image (Poly_Out (2)) &
                ", " & Field_Element'Image (Poly_Out (3)));
      if Poly_Out (0) /= 1 then
         Put_Line ("    FAIL: Coef 0 wrong");
         All_Match := False;
      end if;
      for I in 1 .. N - 1 loop
         if Poly_Out (I) /= 0 then
            Put_Line ("    FAIL: Coef" & Natural'Image (I) & " = " &
                      Field_Element'Image (Poly_Out (I)) & " (expected 0)");
            All_Match := False;
            exit;
         end if;
      end loop;

      --  Test 2: [0, 1, 0, 0, ...]
      Put_Line ("  Test 2: X polynomial [0, 1, 0, 0, ...]");
      Poly_In := (others => 0);
      Poly_In (1) := 1;
      Poly_Out := Poly_In;
      NTT (Poly_Out);
      INTT (Poly_Out);
      Put_Line ("    After round-trip[0..3]: " & Field_Element'Image (Poly_Out (0)) &
                ", " & Field_Element'Image (Poly_Out (1)) &
                ", " & Field_Element'Image (Poly_Out (2)) &
                ", " & Field_Element'Image (Poly_Out (3)));
      if Poly_Out (0) /= 0 or Poly_Out (1) /= 1 then
         Put_Line ("    FAIL");
         All_Match := False;
      end if;

      --  Test 3: [1, 2, 3, 0, 0, ...]
      Put_Line ("  Test 3: [1, 2, 3, 0, ...]");
      Poly_In := (others => 0);
      Poly_In (0) := 1;
      Poly_In (1) := 2;
      Poly_In (2) := 3;
      Poly_Out := Poly_In;
      NTT (Poly_Out);
      INTT (Poly_Out);
      Put_Line ("    After round-trip[0..3]: " & Field_Element'Image (Poly_Out (0)) &
                ", " & Field_Element'Image (Poly_Out (1)) &
                ", " & Field_Element'Image (Poly_Out (2)) &
                ", " & Field_Element'Image (Poly_Out (3)));
      if Poly_Out (0) /= 1 or Poly_Out (1) /= 2 or Poly_Out (2) /= 3 then
         Put_Line ("    FAIL");
         All_Match := False;
      end if;

      if All_Match then
         Put_Line ("  NTT round-trip: PASS");
      else
         Put_Line ("  NTT round-trip: FAIL");
      end if;
   end Test_NTT_Roundtrip;

   procedure Test_Sign_Step_By_Step is
      Seed_KG : Seed := (others => 0);
      Random_Sign : Seed := (others => 0);

      Rho     : Seed;
      K_Seed  : Seed;
      Tr      : Tr_Hash;
      S1      : Poly_Vector_L;
      S2      : Poly_Vector_K;
      T0      : Poly_Vector_K;

      PK : Public_Key;
      SK : Secret_Key;

      A_Hat   : Poly_Matrix;
      S1_Hat  : Poly_Vector_L;
      S2_Hat  : Poly_Vector_K;
      T0_Hat  : Poly_Vector_K;

      Msg : constant Byte_Array (0 .. 31) := (others => 16#42#);
      Mu : Byte_Array (0 .. 63);
      Mu_Input : Byte_Array (0 .. Tr_Bytes + Msg'Length - 1);

      Rho_PP : Seed;

      Y : Poly_Vector_L;
      Y_Hat : Poly_Vector_L;
      W : Poly_Vector_K;
      W1 : Poly_Vector_K;
      W0 : Poly_Vector_K;

      C_Tilde : Challenge_Seed;
      C : Polynomial;
      C_Hat : Polynomial;

      Z : Poly_Vector_L;
      R : Poly_Vector_K;
      Ct0 : Poly_Vector_K;

      Max_Coef : Integer;
      Kappa : Natural := 0;
   begin
      Put_Line ("Step-by-step signing debug...");

      --  Set up seed
      for I in 0 .. 31 loop
         Seed_KG (I) := Byte (I);
      end loop;

      --  Generate key pair (using existing implementation)
      Put_Line ("  Generating keys...");
      declare
         Expanded : Byte_Array (0 .. 127);
         T, T_Sum : Poly_Vector_K;
         T1, T0_Decomp : Poly_Vector_K;
      begin
         SHAKE256 (Byte_Array (Seed_KG), Expanded, 128);

         for I in 0 .. 31 loop
            Rho (I) := Expanded (I);
            K_Seed (I) := Expanded (64 + I);
         end loop;

         ExpandA (Rho, A_Hat);  --  Already in NTT domain
         ExpandS (Seed (Expanded (32 .. 63)), S1, S2);

         --  Compute t = A*s1 + s2
         S1_Hat := S1;
         Vec_NTT_L (S1_Hat);
         Matrix_Vec_Mul (A_Hat, S1_Hat, T);
         Vec_INTT_K (T);
         Vec_Add_K (T, S2, T_Sum);
         Vec_Reduce_K (T_Sum);

         Vec_Power2Round_K (T_Sum, T1, T0);

         Pack_Public_Key (Rho, T1, PK);
         SHAKE256 (Byte_Array (PK), Tr, 64);
         Pack_Secret_Key (Rho, K_Seed, Tr, S1, S2, T0, SK);
      end;

      --  Unpack secret key
      Unpack_Secret_Key (SK, Rho, K_Seed, Tr, S1, S2, T0);

      Put_Line ("  S1[0] first coefs: " &
                Field_Element'Image (S1 (0) (0)) & ", " &
                Field_Element'Image (S1 (0) (1)) & ", " &
                Field_Element'Image (S1 (0) (2)));

      --  Check S1 coefficients are in range [-eta, eta]
      Max_Coef := 0;
      for I in L_Index loop
         for J in Poly_Index loop
            declare
               C : constant Integer := Center (Valid_Field (Freeze (S1 (I) (J))));
            begin
               if abs (C) > Max_Coef then
                  Max_Coef := abs (C);
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("  Max |S1| coef: " & Integer'Image (Max_Coef) &
                " (should be <= " & Natural'Image (Eta) & ")");

      --  Expand A (already in NTT domain) and convert secret vectors to NTT
      ExpandA (Rho, A_Hat);
      S1_Hat := S1;
      Vec_NTT_L (S1_Hat);
      S2_Hat := S2;
      Vec_NTT_K (S2_Hat);
      T0_Hat := T0;
      Vec_NTT_K (T0_Hat);

      --  Compute mu
      for I in 0 .. Tr_Bytes - 1 loop
         Mu_Input (I) := Tr (I);
      end loop;
      for I in Msg'Range loop
         Mu_Input (Tr_Bytes + I - Msg'First) := Msg (I);
      end loop;
      SHAKE256 (Mu_Input, Mu, 64);

      --  Compute rho''
      declare
         Rho_PP_Input : Byte_Array (0 .. 127);
      begin
         for I in 0 .. 31 loop
            Rho_PP_Input (I) := K_Seed (I);
            Rho_PP_Input (32 + I) := Random_Sign (I);
         end loop;
         for I in 0 .. 63 loop
            Rho_PP_Input (64 + I) := Mu (I);
         end loop;
         SHAKE256 (Rho_PP_Input, Byte_Array (Rho_PP), 32);
      end;

      --  Generate masking vector y
      ExpandMask (Rho_PP, Kappa, Y);
      Kappa := Kappa + L;

      --  Check y coefficients are in range [-gamma1+1, gamma1]
      Max_Coef := 0;
      for I in L_Index loop
         for J in Poly_Index loop
            declare
               C : constant Integer := Center (Valid_Field (Freeze (Y (I) (J))));
            begin
               if abs (C) > Max_Coef then
                  Max_Coef := abs (C);
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("  Max |Y| coef: " & Integer'Image (Max_Coef) &
                " (should be <= " & Natural'Image (Gamma1) & ")");

      --  Compute w = A*y
      Y_Hat := Y;
      Vec_NTT_L (Y_Hat);
      Matrix_Vec_Mul (A_Hat, Y_Hat, W);
      Vec_Reduce_K (W);        --  Reduce before INTT to minimize overflow
      Vec_INTT_K (W);
      Vec_Reduce_K (W);        --  Reduce to [0, Q-1]
      --  Note: INTT already produces standard form (no From_Mont needed)

      --  Debug: Print first few raw W coefficients
      Put_Line ("  First 5 raw W[0] coefs: " &
                Field_Element'Image (W (0) (0)) & "," &
                Field_Element'Image (W (0) (1)) & "," &
                Field_Element'Image (W (0) (2)) & "," &
                Field_Element'Image (W (0) (3)) & "," &
                Field_Element'Image (W (0) (4)));

      --  Check w range
      Max_Coef := 0;
      for I in K_Index loop
         for J in Poly_Index loop
            declare
               C : constant Integer := Center (Valid_Field (Freeze (W (I) (J))));
            begin
               if abs (C) > Max_Coef then
                  Max_Coef := abs (C);
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("  Max |W| coef: " & Integer'Image (Max_Coef));

      --  Decompose w
      Vec_HighBits_K (W, W1);

      --  Sample challenge
      declare
         W1_Packed : Byte_Array (0 .. K * Poly_W1_Packed_Bytes - 1);
         C_Input : Byte_Array (0 .. 64 + K * Poly_W1_Packed_Bytes - 1);
         Idx : Natural := 0;
      begin
         for I in K_Index loop
            Pack_W1 (W1 (I), W1_Packed (Idx .. Idx + Poly_W1_Packed_Bytes - 1));
            Idx := Idx + Poly_W1_Packed_Bytes;
         end loop;

         for I in 0 .. 63 loop
            C_Input (I) := Mu (I);
         end loop;
         for I in W1_Packed'Range loop
            C_Input (64 + I) := W1_Packed (I);
         end loop;

         SHAKE256 (C_Input, Byte_Array (C_Tilde), 32);
      end;

      SampleInBall (Byte_Array (C_Tilde), C);

      --  Count non-zero coefficients in c
      declare
         Count : Natural := 0;
      begin
         for I in Poly_Index loop
            if C (I) /= 0 then
               Count := Count + 1;
            end if;
         end loop;
         Put_Line ("  Challenge c has " & Natural'Image (Count) &
                   " non-zero coeffs (should be " & Natural'Image (Tau) & ")");
      end;

      --  Compute z = y + c*s1
      C_Hat := C;
      NTT (C_Hat);

      for I in L_Index loop
         declare
            Cs1 : Polynomial;
         begin
            NTT_Mul (C_Hat, S1_Hat (I), Cs1);
            INTT (Cs1);
            --  INTT already produces standard form (no From_Mont needed)
            Poly_Add (Y (I), Cs1, Z (I));
         end;
      end loop;
      Vec_Reduce_L (Z);

      --  Check ||z||_inf < gamma1 - beta
      Max_Coef := 0;
      for I in L_Index loop
         for J in Poly_Index loop
            declare
               Co : constant Integer := Center (Valid_Field (Freeze (Z (I) (J))));
            begin
               if abs (Co) > Max_Coef then
                  Max_Coef := abs (Co);
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("  Max |Z| coef: " & Integer'Image (Max_Coef) &
                " (bound: " & Natural'Image (Gamma1 - Beta) & ")");

      if Max_Coef >= Gamma1 - Beta then
         Put_Line ("  REJECTION: ||z||_inf >= gamma1 - beta");
      else
         Put_Line ("  z check: PASS");
      end if;

      --  Compute r = w - c*s2
      for I in K_Index loop
         declare
            Cs2 : Polynomial;
         begin
            NTT_Mul (C_Hat, S2_Hat (I), Cs2);
            INTT (Cs2);
            --  INTT already produces standard form (no From_Mont needed)
            Poly_Sub (W (I), Cs2, R (I));
         end;
      end loop;
      Vec_Reduce_K (R);

      --  Get low bits
      Vec_LowBits_K (R, W0);

      --  Check ||LowBits(r)||_inf < gamma2 - beta
      Max_Coef := 0;
      for I in K_Index loop
         for J in Poly_Index loop
            declare
               Co : constant Integer := Center (Valid_Field (Freeze (W0 (I) (J))));
            begin
               if abs (Co) > Max_Coef then
                  Max_Coef := abs (Co);
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("  Max |LowBits(r)| coef: " & Integer'Image (Max_Coef) &
                " (bound: " & Natural'Image (Gamma2 - Beta) & ")");

      if Max_Coef >= Gamma2 - Beta then
         Put_Line ("  REJECTION: ||LowBits(r)||_inf >= gamma2 - beta");
      else
         Put_Line ("  LowBits(r) check: PASS");
      end if;

      --  Compute c*t0
      for I in K_Index loop
         declare
            Ct0_I : Polynomial;
         begin
            NTT_Mul (C_Hat, T0_Hat (I), Ct0_I);
            INTT (Ct0_I);
            --  INTT already produces standard form (no From_Mont needed)
            Ct0 (I) := Ct0_I;
         end;
      end loop;
      Vec_Reduce_K (Ct0);

      --  Check ||c*t0||_inf < gamma2
      Max_Coef := 0;
      for I in K_Index loop
         for J in Poly_Index loop
            declare
               Co : constant Integer := Center (Valid_Field (Freeze (Ct0 (I) (J))));
            begin
               if abs (Co) > Max_Coef then
                  Max_Coef := abs (Co);
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("  Max |c*t0| coef: " & Integer'Image (Max_Coef) &
                " (bound: " & Natural'Image (Gamma2) & ")");

      if Max_Coef >= Gamma2 then
         Put_Line ("  REJECTION: ||c*t0||_inf >= gamma2");
      else
         Put_Line ("  c*t0 check: PASS");
      end if;

   end Test_Sign_Step_By_Step;

   procedure Test_Montgomery is
      A, B, C : Field_Element;
   begin
      Put_Line ("Testing Montgomery multiplication...");

      --  Test 1: Mont_Mul(1, 1) should give R^(-1) mod Q
      --  Because Mont_Mul(a, b) = a * b * R^(-1) mod Q
      A := 1;
      B := 1;
      C := Mont_Mul (A, B);
      Put_Line ("  Mont_Mul(1, 1) = " & Field_Element'Image (C));
      --  R^(-1) mod Q = 8265825

      --  Test 2: Mont_Mul(R, R) should give R (where R = 4193792)
      --  Mont_Mul(R, R) = R * R * R^(-1) = R
      A := 4193792;  --  R mod Q
      B := 4193792;
      C := Mont_Mul (A, B);
      Put_Line ("  Mont_Mul(R, R) = " & Field_Element'Image (C) &
                " (expected R = 4193792)");

      --  Test 3: Convert 5 to Montgomery form and back
      --  To_Mont(5) = 5 * R mod Q
      --  From_Mont(To_Mont(5)) should give 5
      A := To_Mont (5);
      Put_Line ("  To_Mont(5) = " & Field_Element'Image (A));
      B := From_Mont (A);
      Put_Line ("  From_Mont(To_Mont(5)) = " & Field_Element'Image (B));

      if B = 5 then
         Put_Line ("  Montgomery round-trip: PASS");
      else
         Put_Line ("  Montgomery round-trip: FAIL");
      end if;

      --  Test 4: Verify Mont_Reduce(a) = a * R^(-1) mod Q
      --  Mont_Reduce(1) should give R^(-1) mod Q = 8265825
      A := Mont_Reduce (1);
      Put_Line ("  Mont_Reduce(1) = " & Field_Element'Image (A) &
                " (expected R^(-1) = 8265825)");

      --  Test 5: Mont_Reduce(R) should give 1
      A := Mont_Reduce (4193792);
      Put_Line ("  Mont_Reduce(R) = " & Field_Element'Image (A) &
                " (expected 1)");
   end Test_Montgomery;

   procedure Verify_Zetas is
   begin
      Put_Line ("Verifying Zetas table (first 32):");
      for I in 0 .. 31 loop
         Put_Line ("  Zetas(" & Natural'Image (I) & ") = " &
                   Field_Element'Image (Zetas (I)));
      end loop;

      --  Expected from reference (converted to unsigned mod Q)
      Put_Line ("Expected (first 8): 0, 25847, 5771523, 7861508, 237124, 7602457, 7504169, 466468");
   end Verify_Zetas;

   procedure Test_SampleInBall is
      Seed : Byte_Array (0 .. 31) := (others => 16#42#);
      Buffer : Byte_Array (0 .. 399);
      C : Polynomial;
      Count : Natural := 0;
      Plus_Count : Natural := 0;
      Minus_Count : Natural := 0;
   begin
      Put_Line ("Testing SampleInBall...");
      Put_Line ("  N=" & Natural'Image (N) & " Tau=" & Natural'Image (Tau));
      Put_Line ("  Loop range: " & Natural'Image (N - Tau) & " .. " & Natural'Image (N - 1));

      --  Check what SHAKE256 produces
      SHAKE256 (Seed, Buffer, 400);
      Put_Line ("  First 16 bytes of SHAKE256 output:");
      Put ("    ");
      for I in 0 .. 15 loop
         Put (Byte'Image (Buffer (I)));
      end loop;
      New_Line;
      Put_Line ("  Bytes 8-23 (used for sampling):");
      Put ("    ");
      for I in 8 .. 23 loop
         Put (Byte'Image (Buffer (I)));
      end loop;
      New_Line;

      SampleInBall (Seed, C);

      --  Count non-zero coefficients and show positions
      Put_Line ("  Non-zero positions:");
      for I in Poly_Index loop
         if C (I) /= 0 then
            Count := Count + 1;
            if C (I) = 1 then
               Plus_Count := Plus_Count + 1;
               if Count <= 20 then
                  Put_Line ("    C(" & Natural'Image (I) & ") = +1");
               end if;
            elsif C (I) = Q - 1 then
               Minus_Count := Minus_Count + 1;
               if Count <= 20 then
                  Put_Line ("    C(" & Natural'Image (I) & ") = -1");
               end if;
            else
               Put_Line ("  ERROR: Invalid coefficient at" & Natural'Image (I) &
                         " = " & Field_Element'Image (C (I)));
            end if;
         end if;
      end loop;

      Put_Line ("  Non-zero coeffs: " & Natural'Image (Count) &
                " (expected " & Natural'Image (Tau) & ")");
      Put_Line ("  +1 coeffs: " & Natural'Image (Plus_Count));
      Put_Line ("  -1 coeffs: " & Natural'Image (Minus_Count));

      if Count = Tau then
         Put_Line ("  SampleInBall: PASS");
      else
         Put_Line ("  SampleInBall: FAIL");
      end if;
   end Test_SampleInBall;

begin
   Put_Line ("ML-DSA-87 Debug Tests");
   Put_Line ("=====================");
   New_Line;

   Test_Montgomery;
   New_Line;

   Test_NTT_Roundtrip;
   New_Line;

   Test_SampleInBall;
   New_Line;

   Test_Sign_Step_By_Step;
   New_Line;

   Put_Line ("Debug complete.");
end Test_MLDSA_Debug;
