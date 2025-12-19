--  Integration Test: Crypto Syscalls End-to-End
--
--  Tests the full path from VM execution context through Aegis_Crypto_API
--  to the SPARK-verified post-quantum cryptographic implementations.
--
--  Tested paths:
--  1. Execution_Context → Aegis_Crypto_API.MLDSA87_Verify → Anubis_MLDSA.Verify
--  2. Execution_Context → Aegis_Crypto_API.MLKEM1024_Decaps → Anubis_MLKEM.Decaps
--
--  All tests use real cryptographic operations with proper key generation,
--  no stubs or placeholders.

pragma SPARK_Mode (Off);  --  Test harness doesn't need SPARK

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;            use Interfaces;

--  VM types and execution context
with Aegis_VM_Types;
with Aegis_Execution;       use Aegis_Execution;
with Aegis_Crypto_API;      use Aegis_Crypto_API;

--  Post-quantum crypto implementations
with Anubis_Types;
with Anubis_MLDSA_Types;
with Anubis_MLDSA;
with Anubis_MLKEM_Types;
with Anubis_MLKEM;

procedure Test_Crypto_Syscall_Integration is

   --  Type aliases for clarity
   subtype VM_Byte is Aegis_VM_Types.Byte;
   subtype Crypto_Byte is Anubis_Types.Byte;
   subtype VM_Byte_Array is Aegis_VM_Types.Byte_Array;
   subtype Crypto_Byte_Array is Anubis_Types.Byte_Array;

   ---------------------------------------------------------------------------
   --  Test Result Tracking
   ---------------------------------------------------------------------------

   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;

   procedure Report (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Failed_Tests := Failed_Tests + 1;
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Report;

   procedure Section (Name : String) is
   begin
      New_Line;
      Put_Line ("--- " & Name & " ---");
   end Section;

   ---------------------------------------------------------------------------
   --  Hex Utilities
   ---------------------------------------------------------------------------

   function Bytes_To_Hex (Bytes : Crypto_Byte_Array; Max_Len : Natural := 16) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Actual_Len : constant Natural := Natural'Min (Bytes'Length, Max_Len);
      Result : String (1 .. Actual_Len * 2);
      Pos : Natural := 1;
   begin
      for I in 0 .. Actual_Len - 1 loop
         Result (Pos) := Hex_Chars (Natural (Bytes (Bytes'First + I) / 16) + 1);
         Result (Pos + 1) := Hex_Chars (Natural (Bytes (Bytes'First + I) mod 16) + 1);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Bytes_To_Hex;

   ---------------------------------------------------------------------------
   --  Test: ML-DSA-87 Signature Verification via VM Context
   ---------------------------------------------------------------------------

   procedure Test_MLDSA87_Verify_Valid is
      use Aegis_VM_Types;

      --  Create execution context with sufficient gas
      Ctx : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 100_000_000,  --  100M gas for PQ crypto
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Gold
      );

      --  ML-DSA-87 keys and signature
      Seed_KG     : Anubis_MLDSA_Types.Seed := [others => 0];
      Random_Sign : Anubis_MLDSA_Types.Seed := [others => 0];
      PK          : Anubis_MLDSA_Types.Public_Key;
      SK          : Anubis_MLDSA_Types.Secret_Key;
      Sig         : Anubis_MLDSA_Types.Signature;
      Sign_OK     : Boolean;

      --  Message to sign
      Test_Msg : constant Crypto_Byte_Array (0 .. 31) := [
         16#48#, 16#65#, 16#6c#, 16#6c#, 16#6f#, 16#20#, 16#41#, 16#6e#,
         16#75#, 16#62#, 16#69#, 16#73#, 16#56#, 16#4d#, 16#21#, 16#00#,
         16#54#, 16#65#, 16#73#, 16#74#, 16#20#, 16#4d#, 16#73#, 16#67#,
         16#20#, 16#66#, 16#6f#, 16#72#, 16#20#, 16#50#, 16#51#, 16#43#
      ];  -- "Hello AnubisVM!" + padding

      --  API buffers
      Msg_Buffer  : Hash_Input_Buffer := [others => 0];
      Sig_Buffer  : MLDSA87_Signature := [others => 0];
      PK_Buffer   : MLDSA87_Public_Key := [others => 0];
      Valid       : Boolean;
      Result      : Crypto_Result;
      Gas_Before  : Gas_Amount;
      Gas_After   : Gas_Amount;

   begin
      Section ("ML-DSA-87 Verification (Valid Signature)");

      --  1. Generate real ML-DSA-87 keypair
      for I in 0 .. 31 loop
         Seed_KG (I) := Crypto_Byte (I * 7 + 13);  --  Deterministic seed
      end loop;

      Put_Line ("  Generating ML-DSA-87 keypair...");
      Anubis_MLDSA.KeyGen (Seed_KG, PK, SK);
      Put_Line ("    PK[0..7]: " & Bytes_To_Hex (Crypto_Byte_Array (PK), 8));

      --  2. Sign the message
      Put_Line ("  Signing message...");
      Anubis_MLDSA.Sign (SK, Test_Msg, Random_Sign, Sig, Sign_OK);
      Report ("Signature generation succeeded", Sign_OK);
      if not Sign_OK then
         Put_Line ("    ERROR: Could not generate signature");
         return;
      end if;
      Put_Line ("    Sig[0..7]: " & Bytes_To_Hex (Crypto_Byte_Array (Sig), 8));

      --  3. Copy to API buffers
      for I in Test_Msg'Range loop
         Msg_Buffer (I) := VM_Byte (Test_Msg (I));
      end loop;

      for I in 0 .. Anubis_MLDSA.Signature_Bytes - 1 loop
         Sig_Buffer (I) := VM_Byte (Sig (I));
      end loop;

      for I in 0 .. Anubis_MLDSA.Public_Key_Bytes - 1 loop
         PK_Buffer (I) := VM_Byte (PK (I));
      end loop;

      --  4. Verify through VM context
      Put_Line ("  Verifying via Aegis_Crypto_API...");
      Gas_Before := Gas_Remaining (Ctx);

      MLDSA87_Verify (
         Ctx        => Ctx,
         Message    => Msg_Buffer,
         Msg_Len    => Test_Msg'Length,
         Signature  => Sig_Buffer,
         Public_Key => PK_Buffer,
         Valid      => Valid,
         Result     => Result
      );

      Gas_After := Gas_Remaining (Ctx);

      --  5. Check results
      Report ("Crypto result is OK", Result = Crypto_OK);
      Report ("Signature is valid", Valid);
      Report ("Gas was consumed", Gas_After < Gas_Before);

      Put_Line ("    Gas used: " & Gas_Amount'Image (Gas_Before - Gas_After));

   end Test_MLDSA87_Verify_Valid;

   ---------------------------------------------------------------------------
   --  Test: ML-DSA-87 Invalid Signature Rejection
   ---------------------------------------------------------------------------

   procedure Test_MLDSA87_Verify_Invalid is
      use Aegis_VM_Types;

      Ctx : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 100_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Gold
      );

      --  Generate real keypair
      Seed_KG     : Anubis_MLDSA_Types.Seed := [others => 0];
      Random_Sign : Anubis_MLDSA_Types.Seed := [others => 0];
      PK          : Anubis_MLDSA_Types.Public_Key;
      SK          : Anubis_MLDSA_Types.Secret_Key;
      Sig         : Anubis_MLDSA_Types.Signature;
      Sign_OK     : Boolean;

      Msg1 : constant Crypto_Byte_Array (0 .. 15) := [others => 16#AA#];
      Msg2 : constant Crypto_Byte_Array (0 .. 15) := [others => 16#BB#];  --  Different msg

      Msg_Buffer  : Hash_Input_Buffer := [others => 0];
      Sig_Buffer  : MLDSA87_Signature := [others => 0];
      PK_Buffer   : MLDSA87_Public_Key := [others => 0];
      Valid       : Boolean;
      Result      : Crypto_Result;

   begin
      Section ("ML-DSA-87 Verification (Invalid Signature)");

      --  Generate keys and sign Msg1
      for I in 0 .. 31 loop
         Seed_KG (I) := Crypto_Byte (I * 3 + 17);
      end loop;

      Anubis_MLDSA.KeyGen (Seed_KG, PK, SK);
      Anubis_MLDSA.Sign (SK, Msg1, Random_Sign, Sig, Sign_OK);

      if not Sign_OK then
         Put_Line ("  ERROR: Could not generate signature");
         return;
      end if;

      --  Copy signature for Msg1, but verify against Msg2
      for I in Msg2'Range loop
         Msg_Buffer (I) := VM_Byte (Msg2 (I));
      end loop;

      for I in 0 .. Anubis_MLDSA.Signature_Bytes - 1 loop
         Sig_Buffer (I) := VM_Byte (Sig (I));
      end loop;

      for I in 0 .. Anubis_MLDSA.Public_Key_Bytes - 1 loop
         PK_Buffer (I) := VM_Byte (PK (I));
      end loop;

      --  Verify with wrong message - should fail
      Put_Line ("  Verifying signature against wrong message...");
      MLDSA87_Verify (
         Ctx        => Ctx,
         Message    => Msg_Buffer,
         Msg_Len    => Msg2'Length,
         Signature  => Sig_Buffer,
         Public_Key => PK_Buffer,
         Valid      => Valid,
         Result     => Result
      );

      Report ("Crypto result is OK (verification completed)", Result = Crypto_OK);
      Report ("Signature correctly rejected as invalid", not Valid);

   end Test_MLDSA87_Verify_Invalid;

   ---------------------------------------------------------------------------
   --  Test: ML-KEM-1024 Decapsulation via VM Context
   ---------------------------------------------------------------------------

   procedure Test_MLKEM1024_Decaps_Valid is
      use Aegis_VM_Types;

      Ctx : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 100_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Gold
      );

      --  ML-KEM-1024 keys
      Random_D : Anubis_MLKEM_Types.Seed := [others => 0];
      Random_Z : Anubis_MLKEM_Types.Seed := [others => 0];
      Random_M : Anubis_MLKEM_Types.Seed := [others => 0];
      EK       : Anubis_MLKEM_Types.Encapsulation_Key;
      DK       : Anubis_MLKEM_Types.Decapsulation_Key;
      CT       : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS_Enc   : Anubis_MLKEM_Types.Shared_Secret;
      SS_Dec   : MLKEM1024_Shared_Secret;

      --  API buffers
      CT_Buffer : MLKEM1024_Ciphertext := [others => 0];
      DK_Buffer : MLKEM1024_Decaps_Key := [others => 0];
      Result    : Crypto_Result;
      Gas_Before : Gas_Amount;
      Gas_After  : Gas_Amount;
      Match     : Boolean;

   begin
      Section ("ML-KEM-1024 Decapsulation (Valid)");

      --  1. Generate deterministic seeds
      for I in 0 .. 31 loop
         Random_D (I) := Crypto_Byte ((I * 5 + 7) mod 256);
         Random_Z (I) := Crypto_Byte ((I * 11 + 3) mod 256);
         Random_M (I) := Crypto_Byte ((I * 13 + 19) mod 256);
      end loop;

      --  2. Generate keypair
      Put_Line ("  Generating ML-KEM-1024 keypair...");
      Anubis_MLKEM.KeyGen (Random_D, Random_Z, EK, DK);
      Put_Line ("    EK[0..7]: " & Bytes_To_Hex (Crypto_Byte_Array (EK), 8));

      --  3. Encapsulate to get ciphertext and shared secret
      Put_Line ("  Encapsulating...");
      Anubis_MLKEM.Encaps (EK, Random_M, SS_Enc, CT);
      Put_Line ("    CT[0..7]: " & Bytes_To_Hex (Crypto_Byte_Array (CT), 8));
      Put_Line ("    SS_Enc[0..7]: " & Bytes_To_Hex (Crypto_Byte_Array (SS_Enc), 8));

      --  4. Copy to API buffers
      for I in 0 .. Anubis_MLKEM.Ciphertext_Bytes - 1 loop
         CT_Buffer (I) := VM_Byte (CT (I));
      end loop;

      for I in 0 .. Anubis_MLKEM.Decapsulation_Key_Bytes - 1 loop
         DK_Buffer (I) := VM_Byte (DK (I));
      end loop;

      --  5. Decapsulate through VM context
      Put_Line ("  Decapsulating via Aegis_Crypto_API...");
      Gas_Before := Gas_Remaining (Ctx);

      MLKEM1024_Decaps (
         Ctx           => Ctx,
         Ciphertext    => CT_Buffer,
         Decaps_Key    => DK_Buffer,
         Shared_Secret => SS_Dec,
         Result        => Result
      );

      Gas_After := Gas_Remaining (Ctx);

      --  6. Verify shared secrets match
      Put_Line ("    SS_Dec[0..7]: " & Bytes_To_Hex (Crypto_Byte_Array (SS_Dec), 8));

      Match := True;
      for I in 0 .. 31 loop
         if Crypto_Byte (SS_Dec (I)) /= SS_Enc (I) then
            Match := False;
            exit;
         end if;
      end loop;

      Report ("Crypto result is OK", Result = Crypto_OK);
      Report ("Shared secrets match (encaps = decaps)", Match);
      Report ("Gas was consumed", Gas_After < Gas_Before);

      Put_Line ("    Gas used: " & Gas_Amount'Image (Gas_Before - Gas_After));

   end Test_MLKEM1024_Decaps_Valid;

   ---------------------------------------------------------------------------
   --  Test: Gas Exhaustion for Crypto Operations
   ---------------------------------------------------------------------------

   procedure Test_Crypto_Gas_Exhaustion is
      use Aegis_VM_Types;

      --  Context with minimal gas
      Ctx_DSA : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 100,  --  Far too little for ML-DSA-87
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Bronze
      );

      Ctx_KEM : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 100,  --  Far too little for ML-KEM-1024
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Bronze
      );

      Msg_Buffer  : Hash_Input_Buffer := [others => 0];
      Sig_Buffer  : MLDSA87_Signature := [others => 0];
      PK_Buffer   : MLDSA87_Public_Key := [others => 0];
      CT_Buffer   : MLKEM1024_Ciphertext := [others => 0];
      DK_Buffer   : MLKEM1024_Decaps_Key := [others => 0];
      SS_Buffer   : MLKEM1024_Shared_Secret;
      Valid       : Boolean;
      Result_DSA  : Crypto_Result;
      Result_KEM  : Crypto_Result;

   begin
      Section ("Gas Exhaustion");

      --  Try ML-DSA-87 verify with insufficient gas
      MLDSA87_Verify (
         Ctx        => Ctx_DSA,
         Message    => Msg_Buffer,
         Msg_Len    => 32,
         Signature  => Sig_Buffer,
         Public_Key => PK_Buffer,
         Valid      => Valid,
         Result     => Result_DSA
      );

      Report ("ML-DSA-87 verify with 100 gas returns out-of-gas",
              Result_DSA = Crypto_Out_Of_Gas);

      --  Try ML-KEM-1024 decaps with insufficient gas
      MLKEM1024_Decaps (
         Ctx           => Ctx_KEM,
         Ciphertext    => CT_Buffer,
         Decaps_Key    => DK_Buffer,
         Shared_Secret => SS_Buffer,
         Result        => Result_KEM
      );

      Report ("ML-KEM-1024 decaps with 100 gas returns out-of-gas",
              Result_KEM = Crypto_Out_Of_Gas);

   end Test_Crypto_Gas_Exhaustion;

   ---------------------------------------------------------------------------
   --  Test: Certification Level Gas Discount
   ---------------------------------------------------------------------------

   procedure Test_Certification_Discount_PQC is
      use Aegis_VM_Types;

      --  Bronze: no discount
      Ctx_Bronze : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 200_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Bronze
      );

      --  Platinum: 30% discount
      Ctx_Platinum : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 200_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Platinum
      );

      --  Generate real keypair for test
      Seed_KG     : Anubis_MLDSA_Types.Seed := [others => 0];
      Random_Sign : Anubis_MLDSA_Types.Seed := [others => 0];
      PK          : Anubis_MLDSA_Types.Public_Key;
      SK          : Anubis_MLDSA_Types.Secret_Key;
      Sig         : Anubis_MLDSA_Types.Signature;
      Sign_OK     : Boolean;
      Test_Msg    : constant Crypto_Byte_Array (0 .. 31) := [others => 16#55#];

      Msg_Buffer  : Hash_Input_Buffer := [others => 0];
      Sig_Buffer  : MLDSA87_Signature := [others => 0];
      PK_Buffer   : MLDSA87_Public_Key := [others => 0];
      Valid       : Boolean;
      Result      : Crypto_Result;

      Gas_Bronze, Gas_Platinum : Gas_Amount;

   begin
      Section ("Certification Gas Discount (PQC)");

      --  Generate keypair and signature
      for I in 0 .. 31 loop
         Seed_KG (I) := Crypto_Byte ((I * 23 + 5) mod 256);
      end loop;

      Anubis_MLDSA.KeyGen (Seed_KG, PK, SK);
      Anubis_MLDSA.Sign (SK, Test_Msg, Random_Sign, Sig, Sign_OK);

      if not Sign_OK then
         Put_Line ("  ERROR: Could not generate signature");
         return;
      end if;

      --  Copy to buffers
      for I in Test_Msg'Range loop
         Msg_Buffer (I) := VM_Byte (Test_Msg (I));
      end loop;

      for I in 0 .. Anubis_MLDSA.Signature_Bytes - 1 loop
         Sig_Buffer (I) := VM_Byte (Sig (I));
      end loop;

      for I in 0 .. Anubis_MLDSA.Public_Key_Bytes - 1 loop
         PK_Buffer (I) := VM_Byte (PK (I));
      end loop;

      --  Verify with Bronze
      MLDSA87_Verify (Ctx_Bronze, Msg_Buffer, 32, Sig_Buffer, PK_Buffer, Valid, Result);
      Gas_Bronze := 200_000_000 - Gas_Remaining (Ctx_Bronze);

      --  Verify with Platinum
      MLDSA87_Verify (Ctx_Platinum, Msg_Buffer, 32, Sig_Buffer, PK_Buffer, Valid, Result);
      Gas_Platinum := 200_000_000 - Gas_Remaining (Ctx_Platinum);

      Put_Line ("    Bronze gas:   " & Gas_Amount'Image (Gas_Bronze));
      Put_Line ("    Platinum gas: " & Gas_Amount'Image (Gas_Platinum));

      Report ("Platinum uses less gas than Bronze",
              Gas_Platinum < Gas_Bronze);

   end Test_Certification_Discount_PQC;

   ---------------------------------------------------------------------------
   --  Main
   ---------------------------------------------------------------------------

begin
   Put_Line ("=====================================================");
   Put_Line ("  Crypto Syscall Integration Test");
   Put_Line ("  ML-DSA-87 (FIPS 204) + ML-KEM-1024 (FIPS 203)");
   Put_Line ("=====================================================");
   Put_Line ("  Path: Execution_Context -> Aegis_Crypto_API -> SPARK Crypto");

   Test_MLDSA87_Verify_Valid;
   Test_MLDSA87_Verify_Invalid;
   Test_MLKEM1024_Decaps_Valid;
   Test_Crypto_Gas_Exhaustion;
   Test_Certification_Discount_PQC;

   --  Summary
   New_Line;
   Put_Line ("=====================================================");
   Put_Line ("  Test Summary");
   Put_Line ("=====================================================");
   Put_Line ("  Total:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed: " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed: " & Natural'Image (Failed_Tests));
   New_Line;

   if Failed_Tests > 0 then
      Put_Line ("  RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("  RESULT: ALL TESTS PASSED");
      Put_Line ("  Post-quantum crypto syscalls verified end-to-end!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_Crypto_Syscall_Integration;
