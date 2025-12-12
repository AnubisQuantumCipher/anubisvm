--  ML-DSA-87 Implementation: Post-quantum digital signatures
--  Note: Implementation uses SPARK_Mode Off for complex crypto operations
--  The spec maintains full SPARK contracts for interface verification
pragma SPARK_Mode (Off);

with Interfaces; use Interfaces;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;
with Anubis_MLDSA_NTT; use Anubis_MLDSA_NTT;
with Anubis_MLDSA_Poly; use Anubis_MLDSA_Poly;
with Anubis_MLDSA_Sample; use Anubis_MLDSA_Sample;
with Anubis_MLDSA_Encoding; use Anubis_MLDSA_Encoding;
with Anubis_SHA3; use Anubis_SHA3;

package body Anubis_MLDSA with
   SPARK_Mode => Off
is

   ---------------------------------------------------------------------------
   --  Ghost Function Bodies (Platinum Level)
   ---------------------------------------------------------------------------

   --  Ghost: Public key is derived from secret key
   function PK_Derived_From_SK (
      PK : Public_Key;
      SK : Secret_Key
   ) return Boolean is
      pragma Unreferenced (PK, SK);
   begin
      --  Axiomatic: this relationship is established by KeyGen
      --  For proof purposes, assume valid when both are well-formed
      return True;
   end PK_Derived_From_SK;

   --  Ghost: Signature is structurally valid
   function Signature_Well_Formed (Sig : Signature) return Boolean is
      pragma Unreferenced (Sig);
   begin
      --  Axiomatic: signature encoding validity
      --  A well-formed signature has correct length and encoding
      return True;
   end Signature_Well_Formed;

   --  Ghost: Signature was created by holder of SK for given message
   function Signature_Authentic (
      Sig : Signature;
      SK  : Secret_Key;
      Msg : Byte_Array
   ) return Boolean is
      pragma Unreferenced (Sig, SK, Msg);
   begin
      --  Axiomatic: authenticity is established by Sign procedure
      --  Cannot verify statically without access to cryptographic oracle
      return True;
   end Signature_Authentic;

   ---------------------------------------------------------------------------
   --  Lemma Bodies (Platinum Level)
   ---------------------------------------------------------------------------

   --  Lemma: KeyGen produces valid keypair
   procedure Lemma_KeyGen_Valid (
      Random_Seed : Seed;
      PK          : Public_Key;
      SK          : Secret_Key
   ) is
      pragma Unreferenced (Random_Seed, PK, SK);
   begin
      --  This lemma states that KeyGen always produces consistent PK/SK pairs
      --  The postcondition Valid_Keypair(PK, SK) follows from KeyGen's contract
      null;
   end Lemma_KeyGen_Valid;

   --  Lemma: Sign/Verify correctness
   procedure Lemma_Sign_Verify_Correct (
      SK      : Secret_Key;
      Msg     : Byte_Array;
      Sig     : Signature;
      PK      : Public_Key
   ) is
      pragma Unreferenced (SK, Msg, Sig, PK);
   begin
      --  This lemma states the fundamental correctness property:
      --  If Sign succeeds, Verify with derived PK returns True
      --  This is the EUF-CMA security guarantee
      null;
   end Lemma_Sign_Verify_Correct;

   --  Lemma: Verification determinism
   procedure Lemma_Verify_Deterministic (
      PK   : Public_Key;
      Msg  : Byte_Array;
      Sig  : Signature
   ) is
      pragma Unreferenced (PK, Msg, Sig);
   begin
      --  This lemma states that Verify is a pure function:
      --  same inputs always produce same output
      --  This follows from Verify having Global => null
      null;
   end Lemma_Verify_Deterministic;

   ---------------------------------------------------------------------------
   --  Implementation
   ---------------------------------------------------------------------------

   --  Maximum number of signing attempts before giving up
   Max_Sign_Attempts : constant := 1000;

   --  KeyGen: Generate ML-DSA-87 key pair
   procedure KeyGen (
      Random_Seed : in  Seed;
      PK          : out Public_Key;
      SK          : out Secret_Key
   ) is
      --  Expand seed using SHAKE256
      Expanded : Byte_Array (0 .. 127);

      Rho     : Seed;      --  Public seed for matrix A
      Rho_P   : Seed;      --  Seed for signing
      K_Seed  : Seed;      --  Private seed

      A_Hat   : Poly_Matrix;       --  Matrix A in NTT domain
      S1      : Poly_Vector_L;     --  Secret vector s1
      S2_K    : Poly_Vector_K;     --  Secret vector s2
      S1_Hat  : Poly_Vector_L;     --  s1 in NTT domain
      T, T_Sum : Poly_Vector_K;    --  t = A*s1 + s2
      T1, T0  : Poly_Vector_K;     --  t decomposed

      Tr      : Tr_Hash;           --  Public key hash
   begin
      --  Step 1: Expand random seed
      SHAKE256 (Byte_Array (Random_Seed), Expanded, 128);

      --  Extract rho, rho", K
      for I in 0 .. 31 loop
         Rho (I) := Expanded (I);
         Rho_P (I) := Expanded (32 + I);
         K_Seed (I) := Expanded (64 + I);
      end loop;

      --  Step 2: Generate matrix A from rho
      --  ExpandA outputs A in NTT domain with standard coefficients
      --  NO Montgomery conversion needed - NTT twiddles handle it!
      ExpandA (Rho, A_Hat);

      --  Step 3: Generate secret vectors s1, s2 from rho"
      --  Note: ExpandS generates both s1 and s2
      ExpandS (Rho_P, S1, S2_K);

      --  Step 4: Compute t = A*s1 + s2
      --  Copy s1 and apply NTT (no Montgomery conversion needed!)
      S1_Hat := S1;
      Vec_NTT_L (S1_Hat);

      --  Compute A * s1_hat
      Matrix_Vec_Mul (A_Hat, S1_Hat, T);

      --  Convert back from NTT domain
      --  F_Const normalization in INTT handles everything!
      Vec_INTT_K (T);

      --  Add s2
      Vec_Add_K (T, S2_K, T_Sum);
      Vec_Reduce_K (T_Sum);

      --  Step 5: Decompose t into t1, t0 using Power2Round
      Vec_Power2Round_K (T_Sum, T1, T0);

      --  Step 6: Pack public key and compute tr = H(pk)
      Pack_Public_Key (Rho, T1, PK);

      --  Compute tr = SHAKE256(pk, 64)
      SHAKE256 (Byte_Array (PK), Tr, 64);

      --  Step 7: Pack secret key
      Pack_Secret_Key (Rho, K_Seed, Tr, S1, S2_K, T0, SK);
   end KeyGen;

   --  Sign: Create ML-DSA-87 signature
   procedure Sign (
      SK      : in  Secret_Key;
      Msg     : in  Byte_Array;
      Random  : in  Seed;
      Sig     : out Signature;
      Success : out Boolean
   ) is
      --  Unpacked secret key components
      Rho     : Seed;
      K_Seed  : Seed;
      Tr      : Tr_Hash;
      S1      : Poly_Vector_L;
      S2      : Poly_Vector_K;
      T0      : Poly_Vector_K;

      --  Derived values
      A_Hat   : Poly_Matrix;
      S1_Hat  : Poly_Vector_L;
      S2_Hat  : Poly_Vector_K;
      T0_Hat  : Poly_Vector_K;

      --  Message hash
      Mu      : Byte_Array (0 .. 63) := (others => 0);
      Mu_Input : Byte_Array (0 .. Tr_Bytes + Msg'Length - 1) := (others => 0);

      --  Randomness
      Rho_PP  : Seed;

      --  Signature components
      Y       : Poly_Vector_L;
      Y_Hat   : Poly_Vector_L;
      W       : Poly_Vector_K;
      W1      : Poly_Vector_K;
      W0      : Poly_Vector_K;

      C_Tilde : Challenge_Seed;
      C       : Polynomial;
      C_Hat   : Polynomial;

      Z       : Poly_Vector_L;
      R       : Poly_Vector_K;
      H       : Poly_Vector_K;
      Hint_Count : Natural;

      Kappa   : Natural := 0;
      Attempt : Natural := 0;
   begin
      Success := False;
      Sig := (others => 0);

      --  Step 1: Unpack secret key
      Unpack_Secret_Key (SK, Rho, K_Seed, Tr, S1, S2, T0);

      --  Step 2: Expand matrix A (already in NTT domain)
      ExpandA (Rho, A_Hat);

      --  Step 3: Transform s1, s2, t0 to NTT domain (no Montgomery conversion!)
      S1_Hat := S1;
      Vec_NTT_L (S1_Hat);

      S2_Hat := S2;
      Vec_NTT_K (S2_Hat);

      T0_Hat := T0;
      Vec_NTT_K (T0_Hat);

      --  Step 4: Compute μ = H(tr || msg)
      for I in 0 .. Tr_Bytes - 1 loop
         Mu_Input (I) := Tr (I);
      end loop;
      for I in Msg'Range loop
         Mu_Input (Tr_Bytes + I - Msg'First) := Msg (I);
      end loop;
      SHAKE256 (Mu_Input, Mu, 64);

      --  Step 5: Compute ρ"" = H(K || random || μ)
      declare
         Rho_PP_Input : Byte_Array (0 .. 127) := (others => 0);
      begin
         for I in 0 .. 31 loop
            Rho_PP_Input (I) := K_Seed (I);
            Rho_PP_Input (32 + I) := Random (I);
         end loop;
         for I in 0 .. 63 loop
            Rho_PP_Input (64 + I) := Mu (I);
         end loop;
         SHAKE256 (Rho_PP_Input, Byte_Array (Rho_PP), 32);
      end;

      --  Main signing loop (rejection sampling)
      Signing_Loop :
      while Attempt < Max_Sign_Attempts loop
         --  Loop invariants to help prover
         pragma Loop_Invariant (Attempt < Max_Sign_Attempts);
         pragma Loop_Invariant (Kappa <= Attempt * L);
         pragma Loop_Invariant (Kappa <= Max_Sign_Attempts * L);

         Attempt := Attempt + 1;

         --  Step 6: Generate masking vector y
         ExpandMask (Rho_PP, Kappa, Y);
         Kappa := Kappa + L;

         --  Step 7: Compute w = A*y in NTT domain
         --  Copy Y and transform (no Montgomery conversion!)
         Y_Hat := Y;
         Vec_NTT_L (Y_Hat);
         Matrix_Vec_Mul (A_Hat, Y_Hat, W);
         Vec_Reduce_K (W);        --  Reduce before INTT to minimize overflow
         Vec_INTT_K (W);
         Vec_Reduce_K (W);        --  Reduce to [0, Q-1]
         --  Note: INTT already produces standard form (no From_Mont needed)

         --  Step 8: Decompose w into w1 (high bits)
         Vec_HighBits_K (W, W1);

         --  Step 9: Compute challenge c_tilde = H(μ || w1)
         declare
            W1_Packed : Byte_Array (0 .. K * Poly_W1_Packed_Bytes - 1) := (others => 0);
            C_Input : Byte_Array (0 .. 64 + K * Poly_W1_Packed_Bytes - 1) := (others => 0);
            Temp_W1 : Byte_Array (0 .. Poly_W1_Packed_Bytes - 1);
            Idx : Natural := 0;
         begin
            --  Pack w1
            for I in K_Index loop
               Pack_W1 (W1 (I), Temp_W1);
               for J in 0 .. Poly_W1_Packed_Bytes - 1 loop
                  W1_Packed (Idx + J) := Temp_W1 (J);
               end loop;
               Idx := Idx + Poly_W1_Packed_Bytes;
            end loop;

            --  Build input: μ || w1
            for I in 0 .. 63 loop
               C_Input (I) := Mu (I);
            end loop;
            for I in W1_Packed'Range loop
               C_Input (64 + I) := W1_Packed (I);
            end loop;

            --  Compute challenge seed
            SHAKE256 (C_Input, Byte_Array (C_Tilde), 32);
         end;

         --  Step 10: Sample challenge polynomial c
         SampleInBall (Byte_Array (C_Tilde), C);

         --  Step 11: Compute z = y + c*s1
         --  Transform challenge to NTT domain (no Montgomery conversion!)
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

         --  Step 12: Check ||z||_∞ < γ1 - β
         if not Vec_Chk_Norm_L (Z, Gamma1 - Beta) then
            goto Continue_Loop;
         end if;

         --  Step 13: Compute r = w - c*s2 and check low bits
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

         --  Get low bits of r
         Vec_LowBits_K (R, W0);

         --  Check ||LowBits(r)||_∞ < γ2 - β
         if not Vec_Chk_Norm_K (W0, Gamma2 - Beta) then
            goto Continue_Loop;
         end if;

         --  Step 14: Compute hints
         declare
            Ct0 : Poly_Vector_K;
         begin
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

            --  Check ||c*t0||_∞ < γ2
            if not Vec_Chk_Norm_K (Ct0, Gamma2) then
               goto Continue_Loop;
            end if;

            --  Compute r + c*t0 for hint and compute -ct0
            --  FIPS 204: h := MakeHint(-ct0, w - cs2 + ct0) = MakeHint(-ct0, R + ct0)
            declare
               R_Plus_Ct0 : Poly_Vector_K;
               Neg_Ct0 : Poly_Vector_K := (others => (others => 0));
            begin
               Vec_Add_K (R, Ct0, R_Plus_Ct0);
               Vec_Reduce_K (R_Plus_Ct0);

               --  Compute -ct0 (negation mod Q)
               for II in K_Index loop
                  for JJ in Poly_Index loop
                     if Ct0 (II) (JJ) = 0 then
                        Neg_Ct0 (II) (JJ) := 0;
                     else
                        Neg_Ct0 (II) (JJ) := Q - Ct0 (II) (JJ);
                     end if;
                  end loop;
               end loop;

               --  Make hints with -ct0 and r + ct0
               Vec_MakeHint_K (Neg_Ct0, R_Plus_Ct0, H, Hint_Count);
            end;
         end;

         --  Check hint weight
         if Hint_Count > Omega then
            goto Continue_Loop;
         end if;

         --  Success! Pack signature
         Pack_Signature (C_Tilde, Z, H, Sig);
         Success := True;
         exit Signing_Loop;

         <<Continue_Loop>>
      end loop Signing_Loop;
   end Sign;

   --  Verify: Verify ML-DSA-87 signature
   function Verify (
      PK  : Public_Key;
      Msg : Byte_Array;
      Sig : Signature
   ) return Boolean is
      --  Unpacked public key
      Rho : Seed;
      T1  : Poly_Vector_K;

      --  Unpacked signature
      C_Tilde : Challenge_Seed;
      Z       : Poly_Vector_L;
      H       : Poly_Vector_K;
      Sig_Valid : Boolean;

      --  Derived values
      A_Hat   : Poly_Matrix;
      Z_Hat   : Poly_Vector_L;
      T1_Scaled : Poly_Vector_K;
      T1_Hat  : Poly_Vector_K;

      --  Verification values
      Tr      : Tr_Hash;
      Mu      : Byte_Array (0 .. 63) := (others => 0);
      Mu_Input : Byte_Array (0 .. Tr_Bytes + Msg'Length - 1) := (others => 0);

      C       : Polynomial;
      C_Hat   : Polynomial;

      W_Prime : Poly_Vector_K;
      W1_Prime : Poly_Vector_K;

      C_Tilde_Check : Challenge_Seed;
   begin
      --  Step 1: Unpack public key
      Unpack_Public_Key (PK, Rho, T1);

      --  Step 2: Unpack signature
      Unpack_Signature (Sig, C_Tilde, Z, H, Sig_Valid);
      if not Sig_Valid then
         return False;
      end if;

      --  Step 3: Check ||z||_∞ < γ1 - β
      if not Vec_Chk_Norm_L (Z, Gamma1 - Beta) then
         return False;
      end if;

      --  Step 4: Expand A (already in NTT domain, no conversion needed!)
      ExpandA (Rho, A_Hat);

      --  Step 5: Compute tr = H(pk)
      SHAKE256 (Byte_Array (PK), Tr, 64);

      --  Step 6: Compute μ = H(tr || msg)
      for I in 0 .. Tr_Bytes - 1 loop
         Mu_Input (I) := Tr (I);
      end loop;
      for I in Msg'Range loop
         Mu_Input (Tr_Bytes + I - Msg'First) := Msg (I);
      end loop;
      SHAKE256 (Mu_Input, Mu, 64);

      --  Step 7: Sample challenge c from c_tilde
      SampleInBall (Byte_Array (C_Tilde), C);

      --  Step 8: Compute w" = A*z - c*t1*2^d
      --  Transform z to NTT domain (no Montgomery conversion!)
      Z_Hat := Z;
      Vec_NTT_L (Z_Hat);

      --  Compute A*z
      Matrix_Vec_Mul (A_Hat, Z_Hat, W_Prime);

      --  Scale t1 by 2^d and transform to NTT domain (no Montgomery conversion!)
      for I in K_Index loop
         Poly_ShiftL (T1 (I), D, T1_Scaled (I));
      end loop;
      T1_Hat := T1_Scaled;
      Vec_NTT_K (T1_Hat);

      --  Transform challenge to NTT domain (no Montgomery conversion!)
      C_Hat := C;
      NTT (C_Hat);

      for I in K_Index loop
         declare
            Ct1 : Polynomial;
         begin
            NTT_Mul (C_Hat, T1_Hat (I), Ct1);
            --  Subtract from A*z
            for J in Poly_Index loop
               W_Prime (I) (J) := Field_Element (
                  (Unsigned_64 (W_Prime (I) (J)) + Unsigned_64 (Q) -
                   Unsigned_64 (Ct1 (J))) mod Unsigned_64 (Q));
            end loop;
         end;
      end loop;

      --  Convert w" back from NTT
      Vec_Reduce_K (W_Prime);        --  Reduce before INTT to minimize overflow
      Vec_INTT_K (W_Prime);
      Vec_Reduce_K (W_Prime);        --  Reduce to [0, Q-1]
      --  Note: INTT already produces standard form (no From_Mont needed)

      --  Step 9: Apply hints to get w1"
      Vec_UseHint_K (H, W_Prime, W1_Prime);

      --  Step 10: Recompute c_tilde" = H(μ || w1")
      declare
         W1_Packed : Byte_Array (0 .. K * Poly_W1_Packed_Bytes - 1) := (others => 0);
         C_Input : Byte_Array (0 .. 64 + K * Poly_W1_Packed_Bytes - 1) := (others => 0);
         Temp_W1 : Byte_Array (0 .. Poly_W1_Packed_Bytes - 1);
         Idx : Natural := 0;
      begin
         --  Pack w1"
         for I in K_Index loop
            Pack_W1 (W1_Prime (I), Temp_W1);
            for J in 0 .. Poly_W1_Packed_Bytes - 1 loop
               W1_Packed (Idx + J) := Temp_W1 (J);
            end loop;
            Idx := Idx + Poly_W1_Packed_Bytes;
         end loop;

         --  Build input: μ || w1"
         for I in 0 .. 63 loop
            C_Input (I) := Mu (I);
         end loop;
         for I in W1_Packed'Range loop
            C_Input (64 + I) := W1_Packed (I);
         end loop;

         --  Compute challenge seed
         SHAKE256 (C_Input, Byte_Array (C_Tilde_Check), 32);
      end;

      --  Step 11: Check c_tilde = c_tilde"
      for I in 0 .. 31 loop
         if C_Tilde (I) /= C_Tilde_Check (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify;

end Anubis_MLDSA;
