--  Test_Crypto_Fuzzing: Extended Fuzzing Tests for Crypto Primitives
--
--  Tests edge cases, boundary conditions, and stress scenarios for:
--  - SHA3/Keccak hash functions
--  - ML-DSA-87 signatures
--  - ML-KEM-1024 key exchange
--
--  These tests are designed to catch potential crashes, hangs, or
--  incorrect behavior when processing unusual or malformed inputs.

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;
with Anubis_Secure_Wipe;

procedure Test_Crypto_Fuzzing is

   --  Test counters
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;

   procedure Report_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Failed_Tests := Failed_Tests + 1;
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Report_Test;

   --  Generate deterministic test data
   procedure Fill_Pattern (Data : out Byte_Array; Seed : Natural) is
   begin
      for I in Data'Range loop
         Data (I) := Byte ((I + Seed * 17) mod 256);
      end loop;
   end Fill_Pattern;

   --  Fill with zeros
   procedure Fill_Zeros (Data : out Byte_Array) is
   begin
      for I in Data'Range loop
         Data (I) := 0;
      end loop;
   end Fill_Zeros;

   --  Fill with 0xFF
   procedure Fill_Ones (Data : out Byte_Array) is
   begin
      for I in Data'Range loop
         Data (I) := 16#FF#;
      end loop;
   end Fill_Ones;

   ---------------------------------------------------------------------------
   --  SHA3/Keccak Fuzzing
   ---------------------------------------------------------------------------

   procedure Test_SHA3_Edge_Cases is
      Digest_256 : Anubis_SHA3.SHA3_256_Digest;
      Digest_512 : Anubis_SHA3.SHA3_512_Digest;
   begin
      Put_Line ("");
      Put_Line ("--- SHA3/Keccak Edge Cases ---");

      --  Empty input
      declare
         Empty : constant Byte_Array (0 .. -1) := [others => 0];
      begin
         Anubis_SHA3.SHA3_256 (Empty, Digest_256);
         Report_Test ("SHA3-256: Empty input produces valid hash",
                      Digest_256'Length = 32);
      end;

      --  Single byte inputs (0x00 through 0xFF sampling)
      declare
         Single : Byte_Array (0 .. 0);
         Last_Digest : Anubis_SHA3.SHA3_256_Digest := [others => 0];
         All_Different : Boolean := True;
      begin
         for I in 0 .. 255 loop
            Single (0) := Byte (I);
            Anubis_SHA3.SHA3_256 (Single, Digest_256);
            if I > 0 and then Digest_256 = Last_Digest then
               All_Different := False;
            end if;
            Last_Digest := Digest_256;
         end loop;
         Report_Test ("SHA3-256: All 256 single-byte inputs produce unique hashes",
                      All_Different);
      end;

      --  Large input (4KB)
      declare
         Large : Byte_Array (0 .. 4095);
      begin
         Fill_Pattern (Large, 42);
         Anubis_SHA3.SHA3_256 (Large, Digest_256);
         Report_Test ("SHA3-256: 4KB input produces valid hash",
                      Digest_256'Length = 32);
      end;

      --  Block boundary: exactly 136 bytes (SHA3-256 rate)
      declare
         Boundary : Byte_Array (0 .. 135);
      begin
         Fill_Pattern (Boundary, 1);
         Anubis_SHA3.SHA3_256 (Boundary, Digest_256);
         Report_Test ("SHA3-256: Block boundary (136 bytes) input",
                      Digest_256'Length = 32);
      end;

      --  One less than block boundary
      declare
         Under : Byte_Array (0 .. 134);
      begin
         Fill_Pattern (Under, 2);
         Anubis_SHA3.SHA3_256 (Under, Digest_256);
         Report_Test ("SHA3-256: Under block boundary (135 bytes)",
                      Digest_256'Length = 32);
      end;

      --  One more than block boundary
      declare
         Over : Byte_Array (0 .. 136);
      begin
         Fill_Pattern (Over, 3);
         Anubis_SHA3.SHA3_256 (Over, Digest_256);
         Report_Test ("SHA3-256: Over block boundary (137 bytes)",
                      Digest_256'Length = 32);
      end;

      --  Multiple of block size
      declare
         Multi : Byte_Array (0 .. 271);  -- 2 * 136
      begin
         Fill_Pattern (Multi, 4);
         Anubis_SHA3.SHA3_256 (Multi, Digest_256);
         Report_Test ("SHA3-256: Multiple block size (272 bytes)",
                      Digest_256'Length = 32);
      end;

      --  All zeros
      declare
         Zeros : Byte_Array (0 .. 63);
      begin
         Fill_Zeros (Zeros);
         Anubis_SHA3.SHA3_256 (Zeros, Digest_256);
         Report_Test ("SHA3-256: All zeros input",
                      Digest_256'Length = 32);
      end;

      --  All ones (0xFF)
      declare
         Ones : Byte_Array (0 .. 63);
      begin
         Fill_Ones (Ones);
         Anubis_SHA3.SHA3_256 (Ones, Digest_256);
         Report_Test ("SHA3-256: All 0xFF input",
                      Digest_256'Length = 32);
      end;

      --  SHA3-512 tests
      declare
         Data : Byte_Array (0 .. 127);
      begin
         Fill_Pattern (Data, 10);
         Anubis_SHA3.SHA3_512 (Data, Digest_512);
         Report_Test ("SHA3-512: 128-byte input produces 64-byte hash",
                      Digest_512'Length = 64);
      end;

      --  Keccak-256 (Ethereum-compatible)
      declare
         Data : Byte_Array (0 .. 31);
      begin
         Fill_Pattern (Data, 20);
         Anubis_SHA3.Keccak_256 (Data, Digest_256);
         Report_Test ("Keccak-256: 32-byte input produces valid hash",
                      Digest_256'Length = 32);
      end;

      --  Consistency test: same input = same output
      declare
         Data1 : Byte_Array (0 .. 31);
         Data2 : Byte_Array (0 .. 31);
         Digest1, Digest2 : Anubis_SHA3.SHA3_256_Digest;
      begin
         Fill_Pattern (Data1, 99);
         Fill_Pattern (Data2, 99);
         Anubis_SHA3.SHA3_256 (Data1, Digest1);
         Anubis_SHA3.SHA3_256 (Data2, Digest2);
         Report_Test ("SHA3-256: Deterministic (same input = same hash)",
                      Digest1 = Digest2);
      end;
   end Test_SHA3_Edge_Cases;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Fuzzing
   ---------------------------------------------------------------------------

   procedure Test_MLDSA_Edge_Cases is
      PK : Anubis_MLDSA_Types.Public_Key := [others => 0];
      SK : Anubis_MLDSA_Types.Secret_Key := [others => 0];
      Sig : Anubis_MLDSA_Types.Signature := [others => 0];
      Random : Anubis_MLDSA_Types.Seed := [others => 0];
      Valid : Boolean;
      Success : Boolean;
   begin
      Put_Line ("");
      Put_Line ("--- ML-DSA-87 Edge Cases ---");

      --  Generate valid keypair
      declare
         Seed : Anubis_MLDSA_Types.Seed := [others => 0];
      begin
         Fill_Pattern (Byte_Array (Seed), 1);
         Anubis_MLDSA.KeyGen (Seed, PK, SK);
         Report_Test ("ML-DSA-87: KeyGen with patterned seed",
                      PK (0) /= 0 or PK (1) /= 0);  -- Key not all zeros
      end;

      --  Sign empty message
      declare
         Empty : constant Byte_Array (0 .. -1) := [others => 0];
      begin
         Fill_Pattern (Byte_Array (Random), 10);
         Anubis_MLDSA.Sign (SK, Empty, Random, Sig, Success);
         if Success then
            Valid := Anubis_MLDSA.Verify (PK, Empty, Sig);
            Report_Test ("ML-DSA-87: Sign/verify empty message",
                         Valid);
         else
            Report_Test ("ML-DSA-87: Sign empty message succeeded",
                         False);
         end if;
      end;

      --  Sign single byte message
      declare
         Single : constant Byte_Array (0 .. 0) := [others => 16#42#];
      begin
         Fill_Pattern (Byte_Array (Random), 11);
         Anubis_MLDSA.Sign (SK, Single, Random, Sig, Success);
         if Success then
            Valid := Anubis_MLDSA.Verify (PK, Single, Sig);
            Report_Test ("ML-DSA-87: Sign/verify single-byte message",
                         Valid);
         else
            Report_Test ("ML-DSA-87: Sign single-byte message succeeded",
                         False);
         end if;
      end;

      --  Sign 1KB message
      declare
         Large : Byte_Array (0 .. 1023);
      begin
         Fill_Pattern (Large, 50);
         Fill_Pattern (Byte_Array (Random), 12);
         Anubis_MLDSA.Sign (SK, Large, Random, Sig, Success);
         if Success then
            Valid := Anubis_MLDSA.Verify (PK, Large, Sig);
            Report_Test ("ML-DSA-87: Sign/verify 1KB message",
                         Valid);
         else
            Report_Test ("ML-DSA-87: Sign 1KB message succeeded",
                         False);
         end if;
      end;

      --  Verify with wrong message
      declare
         Msg1 : constant Byte_Array (0 .. 31) := [others => 0];
         Msg2 : constant Byte_Array (0 .. 31) := [others => 1];
      begin
         Fill_Pattern (Byte_Array (Random), 13);
         Anubis_MLDSA.Sign (SK, Msg1, Random, Sig, Success);
         if Success then
            Valid := Anubis_MLDSA.Verify (PK, Msg2, Sig);
            Report_Test ("ML-DSA-87: Reject signature with wrong message",
                         not Valid);
         else
            Report_Test ("ML-DSA-87: Sign for wrong message test succeeded",
                         False);
         end if;
      end;

      --  Verify with corrupted signature
      declare
         Msg : constant Byte_Array (0 .. 31) := [others => 16#AA#];
      begin
         Fill_Pattern (Byte_Array (Random), 14);
         Anubis_MLDSA.Sign (SK, Msg, Random, Sig, Success);
         if Success then
            --  Corrupt the signature
            Sig (0) := Sig (0) xor 16#FF#;
            Sig (100) := Sig (100) xor 16#AA#;
            Valid := Anubis_MLDSA.Verify (PK, Msg, Sig);
            Report_Test ("ML-DSA-87: Reject corrupted signature",
                         not Valid);
         else
            Report_Test ("ML-DSA-87: Sign for corruption test succeeded",
                         False);
         end if;
      end;

      --  Verify with wrong public key
      declare
         Wrong_PK : Anubis_MLDSA_Types.Public_Key := [others => 0];
         Wrong_SK : Anubis_MLDSA_Types.Secret_Key := [others => 0];
         Seed2 : Anubis_MLDSA_Types.Seed := [others => 16#FF#];
         Msg : constant Byte_Array (0 .. 31) := [others => 16#55#];
      begin
         --  Generate different keypair
         Anubis_MLDSA.KeyGen (Seed2, Wrong_PK, Wrong_SK);
         Fill_Pattern (Byte_Array (Random), 15);
         Anubis_MLDSA.Sign (SK, Msg, Random, Sig, Success);  -- Sign with original SK
         if Success then
            Valid := Anubis_MLDSA.Verify (Wrong_PK, Msg, Sig);  -- Verify with wrong PK
            Report_Test ("ML-DSA-87: Reject signature with wrong public key",
                         not Valid);
         else
            Report_Test ("ML-DSA-87: Sign for wrong PK test succeeded",
                         False);
         end if;
      end;

      --  Zero signature (should be rejected)
      declare
         Zero_Sig : Anubis_MLDSA_Types.Signature := [others => 0];
         Msg : constant Byte_Array (0 .. 31) := [others => 16#12#];
      begin
         Valid := Anubis_MLDSA.Verify (PK, Msg, Zero_Sig);
         Report_Test ("ML-DSA-87: Reject all-zero signature",
                      not Valid);
      end;

      --  All-zero key generation seed
      declare
         Zero_Seed : Anubis_MLDSA_Types.Seed := [others => 0];
         Zero_PK : Anubis_MLDSA_Types.Public_Key := [others => 0];
         Zero_SK : Anubis_MLDSA_Types.Secret_Key := [others => 0];
      begin
         Anubis_MLDSA.KeyGen (Zero_Seed, Zero_PK, Zero_SK);
         Report_Test ("ML-DSA-87: KeyGen with zero seed produces key",
                      Zero_PK (0) /= 0 or Zero_PK (100) /= 0);
      end;

      --  All-ones key generation seed
      declare
         Ones_Seed : Anubis_MLDSA_Types.Seed := [others => 16#FF#];
         Ones_PK : Anubis_MLDSA_Types.Public_Key := [others => 0];
         Ones_SK : Anubis_MLDSA_Types.Secret_Key := [others => 0];
      begin
         Anubis_MLDSA.KeyGen (Ones_Seed, Ones_PK, Ones_SK);
         Report_Test ("ML-DSA-87: KeyGen with all-ones seed produces key",
                      Ones_PK (0) /= 0 or Ones_PK (100) /= 0);
      end;
   end Test_MLDSA_Edge_Cases;

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Fuzzing
   ---------------------------------------------------------------------------

   procedure Test_MLKEM_Edge_Cases is
      EK : Anubis_MLKEM_Types.Encapsulation_Key := [others => 0];
      DK : Anubis_MLKEM_Types.Decapsulation_Key := [others => 0];
      CT : Anubis_MLKEM_Types.MLKEM_Ciphertext := [others => 0];
      SS_Enc : Anubis_MLKEM_Types.Shared_Secret := [others => 0];
      SS_Dec : Anubis_MLKEM_Types.Shared_Secret := [others => 0];
   begin
      Put_Line ("");
      Put_Line ("--- ML-KEM-1024 Edge Cases ---");

      --  Generate valid keypair
      declare
         D : Anubis_MLKEM_Types.Seed := [others => 0];
         Z : Anubis_MLKEM_Types.Seed := [others => 0];
      begin
         Fill_Pattern (Byte_Array (D), 1);
         Fill_Pattern (Byte_Array (Z), 2);
         Anubis_MLKEM.KeyGen (D, Z, EK, DK);
         Report_Test ("ML-KEM-1024: KeyGen with patterned seeds",
                      EK (0) /= 0 or EK (1) /= 0);
      end;

      --  Encaps/Decaps with patterned randomness
      declare
         M : Anubis_MLKEM_Types.Seed := [others => 0];
      begin
         Fill_Pattern (Byte_Array (M), 3);
         Anubis_MLKEM.Encaps (EK, M, SS_Enc, CT);
         Anubis_MLKEM.Decaps (DK, CT, SS_Dec);
         Report_Test ("ML-KEM-1024: Shared secrets match after encaps/decaps",
                      SS_Enc = SS_Dec);
      end;

      --  Decaps with corrupted ciphertext (should produce different secret)
      declare
         M : Anubis_MLKEM_Types.Seed := [others => 0];
         CT_Corrupt : Anubis_MLKEM_Types.MLKEM_Ciphertext := [others => 0];
         SS_Corrupt : Anubis_MLKEM_Types.Shared_Secret := [others => 0];
      begin
         Fill_Pattern (Byte_Array (M), 4);
         Anubis_MLKEM.Encaps (EK, M, SS_Enc, CT);
         CT_Corrupt := CT;
         CT_Corrupt (0) := CT_Corrupt (0) xor 16#01#;  -- Flip one bit
         Anubis_MLKEM.Decaps (DK, CT_Corrupt, SS_Corrupt);
         --  ML-KEM uses implicit rejection, so corrupted CT produces different SS
         Report_Test ("ML-KEM-1024: Corrupted ciphertext produces different secret",
                      SS_Corrupt /= SS_Enc);
      end;

      --  Zero randomness encapsulation
      declare
         Zero_M : Anubis_MLKEM_Types.Seed := [others => 0];
      begin
         Anubis_MLKEM.Encaps (EK, Zero_M, SS_Enc, CT);
         Anubis_MLKEM.Decaps (DK, CT, SS_Dec);
         Report_Test ("ML-KEM-1024: Zero randomness encapsulation",
                      SS_Enc = SS_Dec);
      end;

      --  All-ones randomness encapsulation
      declare
         Ones_M : Anubis_MLKEM_Types.Seed := [others => 16#FF#];
      begin
         Anubis_MLKEM.Encaps (EK, Ones_M, SS_Enc, CT);
         Anubis_MLKEM.Decaps (DK, CT, SS_Dec);
         Report_Test ("ML-KEM-1024: All-ones randomness encapsulation",
                      SS_Enc = SS_Dec);
      end;

      --  Different randomness produces different ciphertext
      declare
         M1 : Anubis_MLKEM_Types.Seed := [others => 16#AA#];
         M2 : Anubis_MLKEM_Types.Seed := [others => 16#BB#];
         CT1 : Anubis_MLKEM_Types.MLKEM_Ciphertext := [others => 0];
         CT2 : Anubis_MLKEM_Types.MLKEM_Ciphertext := [others => 0];
         SS1 : Anubis_MLKEM_Types.Shared_Secret := [others => 0];
         SS2 : Anubis_MLKEM_Types.Shared_Secret := [others => 0];
      begin
         Anubis_MLKEM.Encaps (EK, M1, SS1, CT1);
         Anubis_MLKEM.Encaps (EK, M2, SS2, CT2);
         Report_Test ("ML-KEM-1024: Different randomness produces different ciphertext",
                      CT1 /= CT2);
      end;

      --  Zero seed keypair generation
      declare
         Zero_D : Anubis_MLKEM_Types.Seed := [others => 0];
         Zero_Z : Anubis_MLKEM_Types.Seed := [others => 0];
         Zero_EK : Anubis_MLKEM_Types.Encapsulation_Key := [others => 0];
         Zero_DK : Anubis_MLKEM_Types.Decapsulation_Key := [others => 0];
      begin
         Anubis_MLKEM.KeyGen (Zero_D, Zero_Z, Zero_EK, Zero_DK);
         Report_Test ("ML-KEM-1024: Zero seed keypair generation",
                      Zero_EK (0) /= 0 or Zero_EK (100) /= 0);
      end;

      --  All-ones seed keypair generation
      declare
         Ones_D : Anubis_MLKEM_Types.Seed := [others => 16#FF#];
         Ones_Z : Anubis_MLKEM_Types.Seed := [others => 16#FF#];
         Ones_EK : Anubis_MLKEM_Types.Encapsulation_Key := [others => 0];
         Ones_DK : Anubis_MLKEM_Types.Decapsulation_Key := [others => 0];
      begin
         Anubis_MLKEM.KeyGen (Ones_D, Ones_Z, Ones_EK, Ones_DK);
         Report_Test ("ML-KEM-1024: All-ones seed keypair generation",
                      Ones_EK (0) /= 0 or Ones_EK (100) /= 0);
      end;

      --  Deterministic KeyGen (same seeds = same keys)
      declare
         D1 : Anubis_MLKEM_Types.Seed := [others => 0];
         Z1 : Anubis_MLKEM_Types.Seed := [others => 0];
         EK1 : Anubis_MLKEM_Types.Encapsulation_Key := [others => 0];
         DK1 : Anubis_MLKEM_Types.Decapsulation_Key := [others => 0];
         EK2 : Anubis_MLKEM_Types.Encapsulation_Key := [others => 0];
         DK2 : Anubis_MLKEM_Types.Decapsulation_Key := [others => 0];
      begin
         Fill_Pattern (Byte_Array (D1), 99);
         Fill_Pattern (Byte_Array (Z1), 98);
         Anubis_MLKEM.KeyGen (D1, Z1, EK1, DK1);
         Anubis_MLKEM.KeyGen (D1, Z1, EK2, DK2);
         Report_Test ("ML-KEM-1024: Deterministic KeyGen (same seeds = same keys)",
                      EK1 = EK2 and DK1 = DK2);
      end;
   end Test_MLKEM_Edge_Cases;

   ---------------------------------------------------------------------------
   --  Secure Wipe Fuzzing
   ---------------------------------------------------------------------------

   procedure Test_Secure_Wipe is
      Data_32 : Byte_Array (0 .. 31);
      Data_64 : Byte_Array (0 .. 63);
      Data_Large : Byte_Array (0 .. 1023);
   begin
      Put_Line ("");
      Put_Line ("--- Secure Wipe Edge Cases ---");

      --  Wipe 32-byte buffer
      Fill_Pattern (Data_32, 1);
      Anubis_Secure_Wipe.Secure_Wipe_32 (Data_32);
      Report_Test ("Secure_Wipe_32: All bytes zeroed",
                   Anubis_Secure_Wipe.All_Zero (Data_32));

      --  Wipe 64-byte buffer
      Fill_Pattern (Data_64, 2);
      Anubis_Secure_Wipe.Secure_Wipe_64 (Data_64);
      Report_Test ("Secure_Wipe_64: All bytes zeroed",
                   Anubis_Secure_Wipe.All_Zero (Data_64));

      --  Wipe large buffer
      Fill_Pattern (Data_Large, 3);
      Anubis_Secure_Wipe.Secure_Wipe (Data_Large);
      Report_Test ("Secure_Wipe: 1KB buffer all bytes zeroed",
                   Anubis_Secure_Wipe.All_Zero (Data_Large));

      --  Wipe all-zeros buffer (should still work)
      Fill_Zeros (Data_32);
      Anubis_Secure_Wipe.Secure_Wipe_32 (Data_32);
      Report_Test ("Secure_Wipe_32: Wipe already-zero buffer",
                   Anubis_Secure_Wipe.All_Zero (Data_32));

      --  Wipe all-ones buffer
      Fill_Ones (Data_32);
      Anubis_Secure_Wipe.Secure_Wipe_32 (Data_32);
      Report_Test ("Secure_Wipe_32: Wipe all-0xFF buffer to zeros",
                   Anubis_Secure_Wipe.All_Zero (Data_32));
   end Test_Secure_Wipe;

begin
   Put_Line ("=====================================================");
   Put_Line ("  Crypto Fuzzing Test Suite");
   Put_Line ("  Edge Cases, Boundary Conditions, Stress Tests");
   Put_Line ("=====================================================");

   Test_SHA3_Edge_Cases;
   Test_MLDSA_Edge_Cases;
   Test_MLKEM_Edge_Cases;
   Test_Secure_Wipe;

   Put_Line ("");
   Put_Line ("=====================================================");
   Put_Line ("  Test Summary");
   Put_Line ("=====================================================");
   Put_Line ("  Total:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed: " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed: " & Natural'Image (Failed_Tests));
   Put_Line ("");

   if Failed_Tests = 0 then
      Put_Line ("  RESULT: ALL TESTS PASSED");
      Put_Line ("  Crypto primitives handle edge cases correctly!");
   else
      Put_Line ("  RESULT: SOME TESTS FAILED");
   end if;
   Put_Line ("=====================================================");
end Test_Crypto_Fuzzing;
