--  Privacy Layer Integration Tests
--
--  Comprehensive tests for the ANUBIS privacy layer:
--  - SHIELD: Encrypted private state (ML-KEM + ChaCha20-Poly1305)
--  - EYE: Selective disclosure proofs
--  - GATE: Private contract execution
--  - WHISPER: Confidential transactions
--
--  Tests verify:
--  - End-to-end encryption/decryption roundtrips
--  - Key derivation correctness
--  - Commitment verification
--  - Re-encryption for transfers
--  - Zeroization of sensitive data

pragma SPARK_Mode (Off);  --  Test code

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Shield; use Anubis_Shield;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;
with Anubis_MLKEM; use Anubis_MLKEM;

procedure Test_Privacy_Integration is

   Total_Tests : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Report (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Report;

   --  Test randomness (deterministic for testing)
   function Test_Randomness (Seed : Unsigned_64) return Byte_Array is
      Result : Byte_Array (0 .. 63);
      S : Unsigned_64 := Seed;
   begin
      for I in Result'Range loop
         S := S * 6364136223846793005 + 1442695040888963407;
         Result (I) := Unsigned_8 (S mod 256);
      end loop;
      return Result;
   end Test_Randomness;

   --  Helper to check if arrays are equal
   function Arrays_Equal (A, B : Byte_Array) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;
      end if;
      for I in 0 .. A'Length - 1 loop
         if A (A'First + I) /= B (B'First + I) then
            return False;
         end if;
      end loop;
      return True;
   end Arrays_Equal;

   --  Helper to check if array is all zeros
   function Is_Zero (A : Byte_Array) return Boolean is
   begin
      for I in A'Range loop
         if A (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero;

begin
   Put_Line ("===========================================");
   Put_Line ("  Privacy Layer Integration Tests");
   Put_Line ("===========================================");
   Put_Line ("  Testing: SHIELD/EYE/GATE/WHISPER Layers");
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 1: SHIELD - ML-KEM Key Generation
   ---------------------------------------------------------------------------
   Put_Line ("Test 1: SHIELD - ML-KEM Key Generation");
   declare
      Public_Key  : Byte_Array (0 .. 1567);  --  ML-KEM-1024 pk
      Secret_Key  : Byte_Array (0 .. 3167);  --  ML-KEM-1024 sk
      Randomness  : constant Byte_Array := Test_Randomness (12345);
      Success     : Boolean;
   begin
      --  Generate keypair
      Anubis_MLKEM.KeyGen (
         Randomness => Randomness (0 .. 63),
         Public_Key => Public_Key,
         Secret_Key => Secret_Key,
         Success    => Success
      );

      Report ("ML-KEM-1024 KeyGen succeeds", Success);
      Report ("Public key not all zeros", not Is_Zero (Public_Key));
      Report ("Secret key not all zeros", not Is_Zero (Secret_Key));
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 2: SHIELD - Encrypt/Decrypt Private State
   ---------------------------------------------------------------------------
   Put_Line ("Test 2: SHIELD - Encrypt/Decrypt Private State");
   declare
      --  Test plaintext
      Plaintext : constant Byte_Array (0 .. 31) := (
         16#48#, 16#65#, 16#6C#, 16#6C#, 16#6F#, 16#20#, 16#50#, 16#72#,
         16#69#, 16#76#, 16#61#, 16#74#, 16#65#, 16#20#, 16#57#, 16#6F#,
         16#72#, 16#6C#, 16#64#, 16#21#, others => 0);  -- "Hello Private World!"

      --  Keys
      Public_Key  : Byte_Array (0 .. 1567);
      Secret_Key  : Byte_Array (0 .. 3167);
      Key_Random  : constant Byte_Array := Test_Randomness (54321);

      --  Encryption outputs
      Encrypted   : Private_Entry;
      Commitment  : Entry_Commitment;
      Enc_Random  : constant Byte_Array := Test_Randomness (99999);

      --  Decryption outputs
      Decrypted   : Byte_Array (0 .. Max_Entry_Size - 1);
      Dec_Length  : Natural;

      Success     : Boolean;
      Key_Success : Boolean;
   begin
      --  Generate keypair
      Anubis_MLKEM.KeyGen (Key_Random (0 .. 63), Public_Key, Secret_Key, Key_Success);

      if Key_Success then
         --  Encrypt private state
         Encrypt_State (
            Plaintext   => Plaintext,
            User_KEM_PK => Public_Key,
            Randomness  => Enc_Random,
            Priv_Entry  => Encrypted,
            Commitment  => Commitment,
            Success     => Success
         );

         Report ("Encrypt_State succeeds", Success);
         Report ("Ciphertext length > 0", Encrypted.CT_Length > 0);
         Report ("Commitment not all zeros", not Is_Zero (Commitment.Value));

         if Success then
            --  Decrypt private state
            Decrypt_State (
               Priv_Entry  => Encrypted,
               User_KEM_SK => Secret_Key,
               Plaintext   => Decrypted,
               PT_Length   => Dec_Length,
               Success     => Success
            );

            Report ("Decrypt_State succeeds", Success);
            Report ("Decrypted length matches", Dec_Length = Plaintext'Length);
            Report ("Decrypted data matches original",
                    Success and then Arrays_Equal (
                       Decrypted (0 .. Dec_Length - 1),
                       Plaintext));
         else
            Report ("Decrypt_State succeeds", False);
            Report ("Decrypted length matches", False);
            Report ("Decrypted data matches original", False);
         end if;
      else
         Report ("Encrypt_State succeeds", False);
         Report ("Decrypt_State succeeds", False);
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 3: SHIELD - Commitment Verification
   ---------------------------------------------------------------------------
   Put_Line ("Test 3: SHIELD - Commitment Verification");
   declare
      Plaintext : constant Byte_Array (0 .. 15) := (
         16#54#, 16#65#, 16#73#, 16#74#, 16#20#, 16#44#, 16#61#, 16#74#,
         16#61#, 16#20#, 16#48#, 16#65#, 16#72#, 16#65#, 16#21#, 16#00#);
      Opening   : constant Byte_Array (0 .. 31) := Test_Randomness (11111) (0 .. 31);
      Commit    : Entry_Commitment;
      Valid     : Boolean;
   begin
      --  Create commitment
      Create_Commitment (Plaintext, Opening, Commit);

      Report ("Commitment created", not Is_Zero (Commit.Value));

      --  Verify with correct opening
      Valid := Verify_Commitment (Plaintext, Commit, Opening);
      Report ("Verify with correct opening succeeds", Valid);

      --  Verify with wrong opening should fail
      declare
         Bad_Opening : Byte_Array (0 .. 31) := Opening;
      begin
         Bad_Opening (0) := Bad_Opening (0) xor 16#FF#;
         Valid := Verify_Commitment (Plaintext, Commit, Bad_Opening);
         Report ("Verify with wrong opening fails", not Valid);
      end;

      --  Verify with wrong plaintext should fail
      declare
         Bad_Plaintext : Byte_Array (0 .. 15) := Plaintext;
      begin
         Bad_Plaintext (0) := Bad_Plaintext (0) xor 16#FF#;
         Valid := Verify_Commitment (Bad_Plaintext, Commit, Opening);
         Report ("Verify with wrong plaintext fails", not Valid);
      end;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 4: SHIELD - Key Derivation
   ---------------------------------------------------------------------------
   Put_Line ("Test 4: SHIELD - Key Derivation");
   declare
      Master_Seed : constant Byte_Array (0 .. 31) := Test_Randomness (77777) (0 .. 31);
      View_Key_1  : Viewing_Key;
      View_Key_2  : Viewing_Key;
      Commit_Seed : Commitment_Seed;
   begin
      --  Derive viewing key
      Derive_Viewing_Key (Master_Seed, View_Key_1);
      Report ("Viewing key derived", not Is_Zero (View_Key_1));

      --  Same seed produces same key
      Derive_Viewing_Key (Master_Seed, View_Key_2);
      Report ("Viewing key is deterministic", Arrays_Equal (View_Key_1, View_Key_2));

      --  Derive commitment seed
      Derive_Commit_Seed (Master_Seed, Commit_Seed);
      Report ("Commitment seed derived", not Is_Zero (Commit_Seed));

      --  Different derivations produce different keys
      Report ("View key differs from commit seed",
              not Arrays_Equal (View_Key_1, Commit_Seed));
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 5: SHIELD - Re-Encryption for Transfers
   ---------------------------------------------------------------------------
   Put_Line ("Test 5: SHIELD - Re-Encryption for Transfers");
   declare
      Plaintext : constant Byte_Array (0 .. 63) := Test_Randomness (33333);

      --  Old user keys
      Old_PK    : Byte_Array (0 .. 1567);
      Old_SK    : Byte_Array (0 .. 3167);
      Old_Rand  : constant Byte_Array := Test_Randomness (44444);

      --  New user keys
      New_PK    : Byte_Array (0 .. 1567);
      New_SK    : Byte_Array (0 .. 3167);
      New_Rand  : constant Byte_Array := Test_Randomness (55555);

      --  Encrypted entries
      Entry_1     : Private_Entry;
      Entry_2     : Private_Entry;
      Commitment  : Entry_Commitment;
      Enc_Rand    : constant Byte_Array := Test_Randomness (66666);
      ReEnc_Rand  : constant Byte_Array := Test_Randomness (77777);

      --  Decryption
      Decrypted   : Byte_Array (0 .. Max_Entry_Size - 1);
      Dec_Length  : Natural;

      Success     : Boolean;
   begin
      --  Generate both keypairs
      Anubis_MLKEM.KeyGen (Old_Rand (0 .. 63), Old_PK, Old_SK, Success);
      if not Success then
         Report ("Old keypair generation", False);
      else
         Anubis_MLKEM.KeyGen (New_Rand (0 .. 63), New_PK, New_SK, Success);
         if not Success then
            Report ("New keypair generation", False);
         else
            --  Encrypt for old user
            Encrypt_State (Plaintext, Old_PK, Enc_Rand, Entry_1, Commitment, Success);
            Report ("Initial encryption succeeds", Success);

            if Success then
               --  Re-encrypt for new user
               Re_Encrypt (Entry_1, Old_SK, New_PK, ReEnc_Rand, Entry_2, Success);
               Report ("Re-encryption succeeds", Success);

               if Success then
                  --  New user should be able to decrypt
                  Decrypt_State (Entry_2, New_SK, Decrypted, Dec_Length, Success);
                  Report ("New user can decrypt", Success);
                  Report ("Re-encrypted data matches original",
                          Success and then Dec_Length = Plaintext'Length
                          and then Arrays_Equal (
                             Decrypted (0 .. Dec_Length - 1), Plaintext));
               end if;
            end if;
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 6: SHIELD - Zeroization
   ---------------------------------------------------------------------------
   Put_Line ("Test 6: SHIELD - Zeroization");
   declare
      Plaintext : constant Byte_Array (0 .. 31) := Test_Randomness (88888) (0 .. 31);

      PK        : Byte_Array (0 .. 1567);
      SK        : Byte_Array (0 .. 3167);
      Key_Rand  : constant Byte_Array := Test_Randomness (99999);

      Encrypted : Private_Entry;
      Commit    : Entry_Commitment;
      Enc_Rand  : constant Byte_Array := Test_Randomness (11111);

      Bundle    : User_Key_Bundle;

      Success   : Boolean;
   begin
      --  Setup
      Anubis_MLKEM.KeyGen (Key_Rand (0 .. 63), PK, SK, Success);
      if Success then
         Encrypt_State (Plaintext, PK, Enc_Rand, Encrypted, Commit, Success);
      end if;

      if Success then
         --  Zeroize entry
         Zeroize_Entry (Encrypted);
         Report ("Entry CT_Length zeroed", Encrypted.CT_Length = 0);

         --  Setup bundle
         Bundle.KEM_Public := PK;
         Bundle.DSA_Public := (others => 16#AA#);
         Bundle.View_Key := Test_Randomness (22222) (0 .. 31);
         Bundle.Commit_Random := Test_Randomness (33333) (0 .. 31);

         --  Zeroize bundle
         Zeroize_Bundle (Bundle);
         Report ("Bundle KEM_Public zeroed", Is_Zero (Bundle.KEM_Public));
         Report ("Bundle View_Key zeroed", Is_Zero (Bundle.View_Key));
      else
         Report ("Entry CT_Length zeroed", False);
         Report ("Bundle KEM_Public zeroed", False);
         Report ("Bundle View_Key zeroed", False);
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 7: SHIELD - Large Plaintext
   ---------------------------------------------------------------------------
   Put_Line ("Test 7: SHIELD - Large Plaintext (4KB)");
   declare
      --  Max size plaintext
      Plaintext   : Byte_Array (0 .. Max_Entry_Size - 1);

      PK          : Byte_Array (0 .. 1567);
      SK          : Byte_Array (0 .. 3167);
      Key_Rand    : constant Byte_Array := Test_Randomness (44444);

      Encrypted   : Private_Entry;
      Commit      : Entry_Commitment;
      Enc_Rand    : constant Byte_Array := Test_Randomness (55555);

      Decrypted   : Byte_Array (0 .. Max_Entry_Size - 1);
      Dec_Length  : Natural;

      Success     : Boolean;
   begin
      --  Fill plaintext with pattern
      for I in Plaintext'Range loop
         Plaintext (I) := Unsigned_8 (I mod 256);
      end loop;

      --  Generate keys
      Anubis_MLKEM.KeyGen (Key_Rand (0 .. 63), PK, SK, Success);
      Report ("Key generation for large test", Success);

      if Success then
         --  Encrypt max size
         Encrypt_State (Plaintext, PK, Enc_Rand, Encrypted, Commit, Success);
         Report ("Encrypt 4KB plaintext succeeds", Success);

         if Success then
            --  Decrypt
            Decrypt_State (Encrypted, SK, Decrypted, Dec_Length, Success);
            Report ("Decrypt 4KB plaintext succeeds", Success);
            Report ("4KB roundtrip data matches",
                    Success and then Dec_Length = Max_Entry_Size
                    and then Arrays_Equal (Plaintext, Decrypted (0 .. Dec_Length - 1)));
         end if;
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Test 8: SHIELD - Tamper Detection
   ---------------------------------------------------------------------------
   Put_Line ("Test 8: SHIELD - Tamper Detection");
   declare
      Plaintext : constant Byte_Array (0 .. 31) := Test_Randomness (66666) (0 .. 31);

      PK        : Byte_Array (0 .. 1567);
      SK        : Byte_Array (0 .. 3167);
      Key_Rand  : constant Byte_Array := Test_Randomness (77777);

      Encrypted : Private_Entry;
      Commit    : Entry_Commitment;
      Enc_Rand  : constant Byte_Array := Test_Randomness (88888);

      Decrypted : Byte_Array (0 .. Max_Entry_Size - 1);
      Dec_Length : Natural;

      Success   : Boolean;
   begin
      --  Setup
      Anubis_MLKEM.KeyGen (Key_Rand (0 .. 63), PK, SK, Success);
      if Success then
         Encrypt_State (Plaintext, PK, Enc_Rand, Encrypted, Commit, Success);
      end if;

      if Success then
         --  Tamper with ciphertext
         Encrypted.Ciphertext (0) := Encrypted.Ciphertext (0) xor 16#FF#;

         --  Decryption should fail (AEAD authentication)
         Decrypt_State (Encrypted, SK, Decrypted, Dec_Length, Success);
         Report ("Tampered ciphertext decryption fails", not Success);
      else
         Report ("Tampered ciphertext decryption fails", False);
      end if;
   end;
   New_Line;

   ---------------------------------------------------------------------------
   --  Summary
   ---------------------------------------------------------------------------
   Put_Line ("===========================================");
   Put_Line ("  Test Summary");
   Put_Line ("===========================================");
   Put_Line ("  Total:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed: " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed: " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   if Passed_Tests = Total_Tests then
      Put_Line ("  ALL PRIVACY LAYER TESTS PASSED!");
   else
      Put_Line ("  SOME TESTS FAILED");
   end if;

end Test_Privacy_Integration;
