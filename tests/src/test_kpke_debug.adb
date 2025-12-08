--  Debug K-PKE encrypt/decrypt

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;
with Anubis_MLKEM_Poly; use Anubis_MLKEM_Poly;
with Anubis_MLKEM_Encoding; use Anubis_MLKEM_Encoding;
with Anubis_MLKEM_Compress; use Anubis_MLKEM_Compress;
with Anubis_MLKEM_Sample; use Anubis_MLKEM_Sample;
with Anubis_SHA3; use Anubis_SHA3;

procedure Test_KPKE_Debug is

   procedure Print_Poly_Sample (Name : String; P : Polynomial) is
   begin
      Put (Name & ": [");
      for I in 0 .. 7 loop
         Put (Field_Element'Image (P (I)));
         if I < 7 then Put (","); end if;
      end loop;
      Put_Line ("...]");
   end Print_Poly_Sample;

   D_Seed : Seed := (others => 0);
   Z_Seed : Seed := (others => 0);
   Coins : Seed := (others => 0);

   --  Initialize with pattern
   Rho, Sigma : Seed;

   --  Matrix and vectors
   A_Hat : Poly_Matrix;
   S, E : Poly_Vector;
   S_Hat, E_Hat, T_Hat : Poly_Vector;

   --  For encryption
   R, E1 : Poly_Vector;
   E2 : Polynomial;
   R_Hat : Poly_Vector;
   A_Hat_T : Poly_Matrix;
   U : Poly_Vector;
   V : Polynomial;
   M_Poly : Polynomial;

   --  For decryption
   W : Polynomial;
   W2 : Polynomial;

   --  Test message: all zeros (should decrypt to zeros)
   Msg_In : Message := (others => 0);
   Msg_Out : Message;

   Q_Half : constant := (Q + 1) / 2;

begin
   Put_Line ("K-PKE Debug Test");
   Put_Line ("================");
   New_Line;

   --  Set up seeds
   for I in 0 .. 31 loop
      D_Seed (I) := Byte (I);
   end loop;

   --  G(d) -> (rho, sigma)
   declare
      Digest : SHA3_512_Digest;
   begin
      SHA3_512 (Byte_Array (D_Seed), Digest);
      for I in 0 .. 31 loop
         Rho (I) := Digest (I);
         Sigma (I) := Digest (32 + I);
      end loop;
   end;

   Put_Line ("1. Generate matrix A...");
   Generate_Matrix (Rho, A_Hat);
   Print_Poly_Sample ("  A_Hat(0,0)", A_Hat (0, 0));

   Put_Line ("2. Sample s, e...");
   Sample_Vector_CBD (Sigma, 0, S);
   Sample_Vector_CBD (Sigma, K, E);
   Print_Poly_Sample ("  S(0)", S (0));
   Print_Poly_Sample ("  E(0)", E (0));

   Put_Line ("3. NTT(s), NTT(e)...");
   S_Hat := S;
   Vec_NTT (S_Hat);
   E_Hat := E;
   Vec_NTT (E_Hat);
   Print_Poly_Sample ("  S_Hat(0)", S_Hat (0));

   Put_Line ("4. t_hat = A * s_hat + e_hat...");
   Matrix_Vec_Mul_NTT (A_Hat, S_Hat, T_Hat);
   Vec_Add (T_Hat, E_Hat, T_Hat);
   Print_Poly_Sample ("  T_Hat(0)", T_Hat (0));

   Put_Line ("5. Check s_hat encoding round-trip...");
   declare
      S_Encoded : Encoded_Vector;
      S_Hat_Decoded : Poly_Vector;
      Match : Boolean := True;
   begin
      Encode_Vector_12 (S_Hat, S_Encoded);
      Decode_Vector_12 (S_Encoded, S_Hat_Decoded);
      for I in Vec_Index loop
         for J in Poly_Index loop
            if S_Hat (I) (J) /= S_Hat_Decoded (I) (J) then
               Match := False;
            end if;
         end loop;
      end loop;
      if Match then
         Put_Line ("  s_hat encode/decode: MATCH");
      else
         Put_Line ("  s_hat encode/decode: MISMATCH!");
      end if;
   end;

   New_Line;
   Put_Line ("=== ENCRYPTION (with zero message) ===");

   Put_Line ("6. Generate A^T and sample r, e1, e2...");
   Generate_Matrix_Transpose (Rho, A_Hat_T);
   Coins := (others => 16#AA#);  -- Different seed
   Sample_Vector_CBD (Coins, 0, R);
   Sample_Vector_CBD (Coins, K, E1);
   Sample_Poly_CBD (Coins, Byte (2 * K), E2);
   Print_Poly_Sample ("  R(0)", R (0));

   Put_Line ("7. r_hat = NTT(r)...");
   R_Hat := R;
   Vec_NTT (R_Hat);
   Print_Poly_Sample ("  R_Hat(0)", R_Hat (0));

   Put_Line ("8. u = INTT(A^T * r_hat) + e1...");
   Matrix_Vec_Mul_NTT (A_Hat_T, R_Hat, U);
   Print_Poly_Sample ("  A^T*r_hat(0) before INTT", U (0));
   Vec_INTT (U);
   Print_Poly_Sample ("  A^T*r_hat(0) after INTT", U (0));
   Vec_Add (U, E1, U);
   Print_Poly_Sample ("  U(0) = ... + e1", U (0));

   Put_Line ("9. v = INTT(t_hat^T * r_hat) + e2 + m...");
   Vec_Dot_Product_NTT (T_Hat, R_Hat, V);
   Print_Poly_Sample ("  t^T*r before INTT", V);
   Poly_INTT (V);
   Print_Poly_Sample ("  t^T*r after INTT", V);
   Poly_Add (V, E2, V);
   Msg_To_Poly (Msg_In, M_Poly);
   Print_Poly_Sample ("  M_Poly (should be zeros)", M_Poly);
   Poly_Add (V, M_Poly, V);
   Print_Poly_Sample ("  V final", V);

   New_Line;
   Put_Line ("=== DECRYPTION ===");

   Put_Line ("10. Convert u to NTT domain...");
   declare
      U_NTT : Poly_Vector := U;
   begin
      Vec_NTT (U_NTT);
      Print_Poly_Sample ("  U_NTT(0)", U_NTT (0));

      Put_Line ("11. w = INTT(s_hat^T * u_NTT)...");
      Vec_Dot_Product_NTT (S_Hat, U_NTT, W);
      Print_Poly_Sample ("  s^T*u before INTT", W);
      Poly_INTT (W);
      Print_Poly_Sample ("  s^T*u after INTT", W);

      Put_Line ("12. w2 = v - w...");
      Poly_Sub (V, W, W2);
      Print_Poly_Sample ("  W2 = v - s^T*u", W2);

      --  Decode message from w2
      for I in 0 .. 31 loop
         Msg_Out (I) := 0;
         for J in 0 .. 7 loop
            declare
               Coef : constant Unsigned_32 := Unsigned_32 (W2 (8 * I + J));
               Bit : constant Unsigned_8 :=
                  Unsigned_8 (((Coef * 2 + Q / 2) / Q) and 1);
            begin
               Msg_Out (I) := Msg_Out (I) or Shift_Left (Bit, J);
            end;
         end loop;
      end loop;
   end;

   New_Line;
   Put_Line ("Input message:  all zeros");
   Put ("Output message: ");
   for I in 0 .. 7 loop
      Put (Byte'Image (Msg_Out (I)));
   end loop;
   Put_Line ("...");

   if Msg_In = Msg_Out then
      Put_Line ("");
      Put_Line ("RESULT: PASS");
   else
      Put_Line ("");
      Put_Line ("RESULT: FAIL");
   end if;

end Test_KPKE_Debug;
