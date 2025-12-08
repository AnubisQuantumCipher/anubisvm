pragma SPARK_Mode (On);

package Anubis_Config with
   Pure,
   SPARK_Mode => On
is
   --  AnubisVM Global Configuration
   --  These constants define system-wide parameters

   --  === Post-Quantum Security Level ===
   --  NIST Level 5 (ML-KEM-1024, ML-DSA-87)
   PQ_Security_Level : constant := 5;

   --  === Cryptographic Parameters ===

   --  ML-KEM-1024 (NIST FIPS 203)
   MLKEM_K : constant := 4;           -- Dimension
   MLKEM_Eta1 : constant := 2;        -- CBD parameter for secret
   MLKEM_Eta2 : constant := 2;        -- CBD parameter for error
   MLKEM_Du : constant := 11;         -- Ciphertext compression
   MLKEM_Dv : constant := 5;          -- Ciphertext compression
   MLKEM_Public_Key_Bytes : constant := 1568;
   MLKEM_Secret_Key_Bytes : constant := 3168;
   MLKEM_Ciphertext_Bytes : constant := 1568;
   MLKEM_Shared_Secret_Bytes : constant := 32;

   --  ML-DSA-87 (NIST FIPS 204)
   MLDSA_K : constant := 8;           -- Dimension k
   MLDSA_L : constant := 7;           -- Dimension l
   MLDSA_Public_Key_Bytes : constant := 2592;
   MLDSA_Secret_Key_Bytes : constant := 4896;
   MLDSA_Signature_Bytes : constant := 4627;

   --  Field modulus for ML-KEM/ML-DSA
   Q : constant := 3329;              -- Prime modulus

   --  Polynomial degree
   N : constant := 256;               -- Degree of polynomials

   --  === Hash Function Parameters ===

   --  SHA3 variants (FIPS 202)
   SHA3_256_Rate : constant := 1088;  -- bits
   SHA3_384_Rate : constant := 832;
   SHA3_512_Rate : constant := 576;

   --  SHAKE XOF (for matrix generation)
   SHAKE128_Rate : constant := 1344;  -- bits
   SHAKE256_Rate : constant := 1088;

   --  Keccak state
   Keccak_State_Bits : constant := 1600;
   Keccak_Rounds : constant := 24;

   --  === VM Parameters ===

   --  Stack
   Max_Stack_Depth : constant := 1024;

   --  Memory
   Max_Memory_Words : constant := 32768;  -- 1MB / 32 bytes

   --  Storage
   Max_Storage_Keys : constant := 1_000_000;

   --  Code size
   Max_Contract_Size : constant := 24 * 1024;  -- 24KB

   --  === Gas Parameters ===

   --  Base costs
   Gas_Zero : constant := 0;
   Gas_Base : constant := 2;
   Gas_VeryLow : constant := 3;
   Gas_Low : constant := 5;
   Gas_Mid : constant := 8;
   Gas_High : constant := 10;

   --  Crypto precompile costs (realistic for PQ crypto)
   Gas_SHA3_Base : constant := 30;
   Gas_SHA3_Per_Word : constant := 6;
   Gas_MLKEM_Keygen : constant := 50_000;
   Gas_MLKEM_Encaps : constant := 50_000;
   Gas_MLKEM_Decaps : constant := 50_000;
   Gas_MLDSA_Keygen : constant := 80_000;
   Gas_MLDSA_Sign : constant := 100_000;
   Gas_MLDSA_Verify : constant := 60_000;

   --  Storage costs
   Gas_SLoad : constant := 2_100;
   Gas_SStore_Set : constant := 20_000;
   Gas_SStore_Reset : constant := 5_000;

   --  === Governance Parameters ===

   --  Threshold signature
   Max_Signers : constant := 5;       -- 5 signers
   Signature_Threshold : constant := 3; -- 3-of-5 required

   --  Proposal timelock
   Proposal_Delay_Blocks : constant := 1000;  -- ~1000 blocks delay

   --  === Proof Targets ===

   --  Target proof level: Gold
   --  - 100% NRTE (No Runtime Errors)
   --  - Key functional properties proven
   --  - Zeroization postconditions
   --  - Constant-time guarantees (via manual audit + timing tests)
   Target_Proof_Level : constant String := "Gold";

end Anubis_Config;
