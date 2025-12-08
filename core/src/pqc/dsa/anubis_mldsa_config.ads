pragma SPARK_Mode (On);

--  Anubis_MLDSA_Config: Configuration constants for ML-DSA-87
--
--  ML-DSA-87 is the highest security level of NIST FIPS 204
--  (Module-Lattice Digital Signature Algorithm)
--
--  Security: NIST Level 5 (equivalent to AES-256)
--
--  References:
--  - NIST FIPS 204 (ML-DSA)
--  - "CRYSTALS-Dilithium" specification

package Anubis_MLDSA_Config with
   Pure,
   SPARK_Mode => On
is

   --  Field modulus (different from ML-KEM!)
   --  q = 2^23 - 2^13 + 1 = 8380417
   Q : constant := 8380417;

   --  Polynomial degree
   N : constant := 256;

   --  ML-DSA-87 parameters
   K : constant := 8;   --  Number of rows in matrix A
   L : constant := 7;   --  Number of columns in matrix A
   D : constant := 13;  --  Dropped bits from t (for t1 = t >> d)

   --  Challenge parameters
   Tau : constant := 60;  --  Number of +/-1 coefficients in challenge c

   --  Masking and rounding parameters
   Gamma1 : constant := 2**19;          --  524288 (masking range)
   Gamma2 : constant := (Q - 1) / 32;   --  261888 (low-order rounding)

   --  Secret key range
   Eta : constant := 2;  --  Coefficients in [-η, η]

   --  Derived bounds
   Beta : constant := Tau * Eta;  --  120 (bound on ||c*s||_∞)
   Omega : constant := 75;        --  Max number of 1s in hint

   --  Encoding sizes (bytes)
   --  Polynomial encodings
   Poly_T1_Packed_Bytes : constant := 320;   --  10 bits per coef, 256 coefs
   Poly_T0_Packed_Bytes : constant := 416;   --  13 bits per coef
   Poly_Z_Packed_Bytes  : constant := 640;   --  20 bits per coef (γ1 = 2^19)
   Poly_W1_Packed_Bytes : constant := 128;   --  4 bits per coef
   Poly_Eta_Packed_Bytes : constant := 96;   --  3 bits per coef for η=2

   --  Key and signature sizes for ML-DSA-87
   Seed_Bytes : constant := 32;
   Tr_Bytes   : constant := 64;  --  Public key hash (SHAKE256)

   Public_Key_Bytes : constant := Seed_Bytes + K * Poly_T1_Packed_Bytes;
   --  = 32 + 8*320 = 2592

   Secret_Key_Bytes : constant := 2 * Seed_Bytes + Tr_Bytes +
                                  L * Poly_Eta_Packed_Bytes +
                                  K * Poly_Eta_Packed_Bytes +
                                  K * Poly_T0_Packed_Bytes;
   --  rho (32) + K (32) + Tr (64) + s1 (7*96) + s2 (8*96) + t0 (8*416)
   --  = 64 + 64 + 672 + 768 + 3328 = 4896

   --  Challenge hash c_tilde length (lambda / 4 per FIPS 204 Section 8.1)
   C_Tilde_Bytes : constant := 64;

   Signature_Bytes : constant := C_Tilde_Bytes +
                                 L * Poly_Z_Packed_Bytes +
                                 Omega + K;
   --  c_tilde (64) + z (7*640) + hints (75+8) = 64 + 4480 + 83 = 4627

   --  NTT parameters
   --  Root of unity for q=8380417: ζ = 1753
   Zeta : constant := 1753;

   --  2^(-1) mod q for INTT butterfly scaling
   Inv2 : constant := 4190209;  --  2^{-1} mod 8380417

   --  Montgomery parameters for q=8380417
   --  R = 2^32 mod q (using 32-bit Montgomery for larger modulus)
   Mont_R : constant := 4193792;  --  2^32 mod 8380417

   --  R^(-1) mod q for converting from Montgomery form
   --  Verified: (Mont_R * Mont_R_Inv) mod Q = 1
   Mont_R_Inv : constant := 8265825;  --  inverse(4193792, 8380417)

   --  INTT normalization: 2^(-16) mod q
   --  This is the missing factor after INTT to complete the scaling
   Two_Pow_Minus_16 : constant := 4164865;  --  2^(-16) mod 8380417

end Anubis_MLDSA_Config;
