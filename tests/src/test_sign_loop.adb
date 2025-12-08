with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;
with Anubis_MLDSA_NTT; use Anubis_MLDSA_NTT;
with Anubis_MLDSA_Poly; use Anubis_MLDSA_Poly;
with Anubis_MLDSA_Sample; use Anubis_MLDSA_Sample;
with Anubis_MLDSA_Encoding; use Anubis_MLDSA_Encoding;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;
with Anubis_SHA3; use Anubis_SHA3;

procedure Test_Sign_Loop is
   --  Test multiple signing iterations to see rejection pattern

   Seed_KG : Seed := (others => 0);
   Rho : Seed;
   K_Seed : Seed;
   Rho_PP : Seed;

   A_Hat : Poly_Matrix;
   S1 : Poly_Vector_L;
   S2 : Poly_Vector_K;
   T0 : Poly_Vector_K;
   S1_Hat : Poly_Vector_L;
   S2_Hat : Poly_Vector_K;
   T0_Hat : Poly_Vector_K;

   Y : Poly_Vector_L;
   Y_Hat : Poly_Vector_L;
   W : Poly_Vector_K;
   W1 : Poly_Vector_K;
   Z : Poly_Vector_L;
   R : Poly_Vector_K;
   W0 : Poly_Vector_K;

   C_Tilde : Challenge_Seed;
   C : Polynomial;
   C_Hat : Polynomial;

   Kappa : Natural := 0;
   Max_Iterations : constant := 100;

   Z_Pass : Natural := 0;
   LowBits_Pass : Natural := 0;
   Ct0_Pass : Natural := 0;
   All_Pass : Natural := 0;

   Max_LowBits_Seen : Integer := 0;
begin
   Put_Line ("Testing signing loop distribution...");

   --  Set up seed
   for I in 0 .. 31 loop
      Seed_KG (I) := Byte (I);
   end loop;

   --  Generate keys
   declare
      Expanded : Byte_Array (0 .. 127);
      T, T_Sum : Poly_Vector_K;
      T1 : Poly_Vector_K;
   begin
      SHAKE256 (Byte_Array (Seed_KG), Expanded, 128);

      for I in 0 .. 31 loop
         Rho (I) := Expanded (I);
         K_Seed (I) := Expanded (64 + I);
      end loop;

      ExpandA (Rho, A_Hat);
      ExpandS (Seed (Expanded (32 .. 63)), S1, S2);

      S1_Hat := S1;
      Vec_NTT_L (S1_Hat);
      Matrix_Vec_Mul (A_Hat, S1_Hat, T);
      Vec_INTT_K (T);
      Vec_Reduce_K (T);
      for I in K_Index loop
         for J in Poly_Index loop
            T_Sum (I) (J) := Field_Element (
               (Unsigned_64 (T (I) (J)) + Unsigned_64 (S2 (I) (J))) mod Unsigned_64 (Q));
         end loop;
      end loop;

      Vec_Power2Round_K (T_Sum, T1, T0);
   end;

   --  Convert to NTT
   S1_Hat := S1;
   Vec_NTT_L (S1_Hat);
   S2_Hat := S2;
   Vec_NTT_K (S2_Hat);
   T0_Hat := T0;
   Vec_NTT_K (T0_Hat);

   --  Compute rho'' (just use a fixed one for testing)
   for I in 0 .. 31 loop
      Rho_PP (I) := Byte (I + 100);
   end loop;

   --  Run multiple iterations
   for Iter in 1 .. Max_Iterations loop
      --  Generate Y
      ExpandMask (Rho_PP, Kappa, Y);
      Kappa := Kappa + L;

      --  Compute W = A*Y
      Y_Hat := Y;
      Vec_NTT_L (Y_Hat);
      Matrix_Vec_Mul (A_Hat, Y_Hat, W);
      Vec_Reduce_K (W);
      Vec_INTT_K (W);
      Vec_Reduce_K (W);

      --  Get W1 and sample challenge
      Vec_HighBits_K (W, W1);

      declare
         W1_Packed : Byte_Array (0 .. K * Poly_W1_Packed_Bytes - 1);
         C_Input : Byte_Array (0 .. 64 + K * Poly_W1_Packed_Bytes - 1);
         Idx : Natural := 0;
         Mu : constant Byte_Array (0 .. 63) := (others => 0);
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

      --  Compute Z = Y + c*s1
      C_Hat := C;
      NTT (C_Hat);

      for I in L_Index loop
         declare
            Cs1 : Polynomial;
         begin
            NTT_Mul (C_Hat, S1_Hat (I), Cs1);
            INTT (Cs1);
            Poly_Add (Y (I), Cs1, Z (I));
         end;
      end loop;
      Vec_Reduce_L (Z);

      --  Check ||z||_inf < gamma1 - beta
      declare
         Z_Ok : constant Boolean := Vec_Chk_Norm_L (Z, Gamma1 - Beta);
      begin
         if Z_Ok then
            Z_Pass := Z_Pass + 1;
         end if;
      end;

      --  Compute R = W - c*s2
      for I in K_Index loop
         declare
            Cs2 : Polynomial;
         begin
            NTT_Mul (C_Hat, S2_Hat (I), Cs2);
            INTT (Cs2);
            Poly_Sub (W (I), Cs2, R (I));
         end;
      end loop;
      Vec_Reduce_K (R);

      --  Get LowBits and check
      Vec_LowBits_K (R, W0);

      declare
         Max_This : Integer := 0;
         C_Val : Integer;
      begin
         for I in K_Index loop
            for J in Poly_Index loop
               C_Val := Center (Valid_Field (Freeze (W0 (I) (J))));
               if abs (C_Val) > Max_This then
                  Max_This := abs (C_Val);
               end if;
            end loop;
         end loop;

         if Max_This > Max_LowBits_Seen then
            Max_LowBits_Seen := Max_This;
         end if;

         if Max_This < Gamma2 - Beta then
            LowBits_Pass := LowBits_Pass + 1;

            --  Also check c*t0
            declare
               Ct0 : Poly_Vector_K;
               Ct0_Ok : Boolean;
            begin
               for I in K_Index loop
                  declare
                     Ct0_I : Polynomial;
                  begin
                     NTT_Mul (C_Hat, T0_Hat (I), Ct0_I);
                     INTT (Ct0_I);
                     Ct0 (I) := Ct0_I;
                  end;
               end loop;
               Vec_Reduce_K (Ct0);

               Ct0_Ok := Vec_Chk_Norm_K (Ct0, Gamma2);
               if Ct0_Ok then
                  Ct0_Pass := Ct0_Pass + 1;

                  --  Check hint count using FIPS 204: MakeHint(-ct0, r + ct0)
                  declare
                     R_Plus_Ct0 : Poly_Vector_K;
                     Neg_Ct0 : Poly_Vector_K;
                     H : Poly_Vector_K;
                     Hint_Count : Natural;
                  begin
                     for II in K_Index loop
                        for JJ in Poly_Index loop
                           R_Plus_Ct0 (II) (JJ) := Field_Element (
                              (Unsigned_64 (R (II) (JJ)) + Unsigned_64 (Ct0 (II) (JJ))) mod Unsigned_64 (Q));
                           --  Compute -ct0 mod Q
                           if Ct0 (II) (JJ) = 0 then
                              Neg_Ct0 (II) (JJ) := 0;
                           else
                              Neg_Ct0 (II) (JJ) := Q - Ct0 (II) (JJ);
                           end if;
                        end loop;
                     end loop;
                     Vec_MakeHint_K (Neg_Ct0, R_Plus_Ct0, H, Hint_Count);

                     if Hint_Count <= Omega then
                        All_Pass := All_Pass + 1;
                     end if;

                     if Iter <= 10 then
                        Put ("     Hint count:" & Natural'Image (Hint_Count));
                        if Hint_Count <= Omega then
                           Put_Line (" <= " & Natural'Image (Omega) & " PASS");
                        else
                           Put_Line (" > " & Natural'Image (Omega) & " FAIL");
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;

         --  Print first 10 iterations
         if Iter <= 10 then
            Put_Line ("  Iter" & Natural'Image (Iter) &
                      ": max |LowBits| =" & Integer'Image (Max_This) &
                      " (bound:" & Natural'Image (Gamma2 - Beta) & ")");
         end if;
      end;
   end loop;

   Put_Line ("");
   Put_Line ("Results after " & Natural'Image (Max_Iterations) & " iterations:");
   Put_Line ("  Z check passed:       " & Natural'Image (Z_Pass));
   Put_Line ("  LowBits check passed: " & Natural'Image (LowBits_Pass));
   Put_Line ("  Ct0 check passed:     " & Natural'Image (Ct0_Pass));
   Put_Line ("  All checks passed:    " & Natural'Image (All_Pass));
   Put_Line ("  Max |LowBits| seen:   " & Integer'Image (Max_LowBits_Seen) &
             " (bound: " & Natural'Image (Gamma2 - Beta) & ")");

end Test_Sign_Loop;
