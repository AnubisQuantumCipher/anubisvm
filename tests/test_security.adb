--  Security Tests for AnubisVM
--
--  Tests:
--  1. Secure wipe verification (volatile memory clearing)
--  2. Timing attack resistance (constant-time operations)
--  3. Memory isolation (buffer overflow protection)
--  4. Key zeroization after use
--  5. No information leakage on errors

pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Secure_Wipe;
with Anubis_KMAC; use Anubis_KMAC;
with Anubis_AEAD; use Anubis_AEAD;
with Anubis_MLDSA;
with Anubis_MLKEM;

procedure Test_Security is

   Test_Failed : Boolean := False;
   Test_Count  : Natural := 0;
   Pass_Count  : Natural := 0;

   procedure Report_Test (Name : String; Passed : Boolean) is
   begin
      Test_Count := Test_Count + 1;
      Put ("  Test " & Natural'Image (Test_Count) & ": " & Name & "... ");
      if Passed then
         Put_Line ("PASS");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL");
         Test_Failed := True;
      end if;
   end Report_Test;

   --  Check if array is all zeros
   function Is_All_Zero (A : Byte_Array) return Boolean is
   begin
      for B of A loop
         if B /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_All_Zero;

   --  Test 1: Secure wipe of byte arrays
   procedure Test_Secure_Wipe is
      Buffer : Byte_Array (0 .. 255) := (others => 16#AA#);
   begin
      Anubis_Secure_Wipe.Wipe (Buffer);
      Report_Test ("Secure wipe clears all bytes", Is_All_Zero (Buffer));
   end Test_Secure_Wipe;

   --  Test 2: KMAC key zeroization
   procedure Test_KMAC_Key_Zeroization is
      Key : KMAC_Key := (others => 16#FF#);
   begin
      Zeroize_Key (Key);
      Report_Test ("KMAC key zeroization", Is_All_Zero (Byte_Array (Key)));
   end Test_KMAC_Key_Zeroization;

   --  Test 3: AEAD key and nonce zeroization
   procedure Test_AEAD_Zeroization is
      Key   : AEAD_Key := (others => 16#EE#);
      Nonce : AEAD_Nonce := (others => 16#DD#);
   begin
      Zeroize_Key (Key);
      Zeroize_Nonce (Nonce);
      Report_Test ("AEAD key/nonce zeroization",
                   Is_All_Zero (Byte_Array (Key)) and Is_All_Zero (Byte_Array (Nonce)));
   end Test_AEAD_Zeroization;

   --  Test 4: Failed decryption zeros output buffer
   procedure Test_Failed_Decrypt_Zeros_Output is
      Key        : AEAD_Key := (others => 16#42#);
      Nonce      : AEAD_Nonce := (others => 16#24#);
      Plaintext  : Byte_Array (0 .. 31) := (others => 16#AA#);
      AAD        : Byte_Array (0 .. 7) := (others => 16#BB#);
      Ciphertext : Byte_Array (0 .. 31);
      Decrypted  : Byte_Array (0 .. 31) := (others => 16#FF#);  --  Pre-fill
      Tag        : AEAD_Tag;
      Bad_Tag    : AEAD_Tag := (others => 16#00#);
      Success    : Boolean;
   begin
      --  Encrypt
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Decrypt with wrong tag (should fail and zero output)
      AEAD_Decrypt (Key, Nonce, Ciphertext, AAD, Bad_Tag, Decrypted, Success);

      Report_Test ("Failed decryption zeros plaintext buffer",
                   not Success and Is_All_Zero (Decrypted));
   end Test_Failed_Decrypt_Zeros_Output;

   --  Test 5: Timing attack resistance - constant time comparison
   --  This test verifies that KMAC verification takes constant time
   --  regardless of where the mismatch occurs in the tag
   procedure Test_Constant_Time_Verification is
      Key     : KMAC_Key := (others => 16#42#);
      Message : Byte_Array (0 .. 31) := (others => 16#AA#);
      Tag     : KMAC256_Tag;

      Bad_Tag_Early  : KMAC256_Tag;  --  Mismatch in first byte
      Bad_Tag_Late   : KMAC256_Tag;  --  Mismatch in last byte

      Iterations : constant := 1000;

      Start_Time, End_Time : Time;
      Early_Duration, Late_Duration : Time_Span;

      Dummy_Result : Boolean;  --  To prevent optimization
   begin
      --  Compute correct tag
      KMAC256 (Key, Message, "test", Tag);

      --  Create bad tags with mismatches at different positions
      Bad_Tag_Early := Tag;
      Bad_Tag_Early (0) := Bad_Tag_Early (0) xor 16#FF#;

      Bad_Tag_Late := Tag;
      Bad_Tag_Late (31) := Bad_Tag_Late (31) xor 16#FF#;

      --  Time verification with early mismatch
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         Dummy_Result := Verify_KMAC256 (Key, Message, "test", Bad_Tag_Early);
      end loop;
      End_Time := Clock;
      Early_Duration := End_Time - Start_Time;

      --  Time verification with late mismatch
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         Dummy_Result := Verify_KMAC256 (Key, Message, "test", Bad_Tag_Late);
      end loop;
      End_Time := Clock;
      Late_Duration := End_Time - Start_Time;

      --  Calculate timing difference as percentage
      declare
         Early_Ms : constant Long_Float := Long_Float (To_Duration (Early_Duration)) * 1000.0;
         Late_Ms  : constant Long_Float := Long_Float (To_Duration (Late_Duration)) * 1000.0;
         Diff_Pct : constant Long_Float := abs ((Early_Ms - Late_Ms) / Early_Ms) * 100.0;
      begin
         Put_Line ("    Early mismatch: " & Long_Float'Image (Early_Ms) & " ms");
         Put_Line ("    Late mismatch:  " & Long_Float'Image (Late_Ms) & " ms");
         Put_Line ("    Difference:     " & Long_Float'Image (Diff_Pct) & " %");

         --  Timing difference should be less than 5% for constant-time operation
         --  (allowing some noise from OS scheduling, cache effects, etc.)
         Report_Test ("Constant-time MAC verification (< 5% timing variance)",
                      Diff_Pct < 5.0);
      end;

      --  Use dummy result to prevent optimization
      if Dummy_Result then
         null;
      end if;
   end Test_Constant_Time_Verification;

   --  Test 6: Memory isolation - out of bounds access should not corrupt adjacent memory
   procedure Test_Memory_Isolation is
      --  Create two adjacent buffers with guard patterns
      Guard_Pattern : constant Byte := 16#CC#;

      Buffer1 : Byte_Array (0 .. 31) := (others => 16#AA#);
      Guard   : Byte_Array (0 .. 7)  := (others => Guard_Pattern);
      Buffer2 : Byte_Array (0 .. 31) := (others => 16#BB#);

      Guard_Intact : Boolean := True;
   begin
      --  Perform operations on Buffer1 and Buffer2
      --  The guard should remain unchanged

      Anubis_Secure_Wipe.Wipe (Buffer1);
      Anubis_Secure_Wipe.Wipe (Buffer2);

      --  Check guard integrity
      for B of Guard loop
         if B /= Guard_Pattern then
            Guard_Intact := False;
            exit;
         end if;
      end loop;

      Report_Test ("Memory isolation (no buffer overruns)", Guard_Intact);
   end Test_Memory_Isolation;

   --  Test 7: No information leakage on authentication failure
   procedure Test_No_Info_Leakage_On_Auth_Failure is
      Key        : AEAD_Key := (others => 16#11#);
      Nonce      : AEAD_Nonce := (others => 16#22#);
      Plaintext  : Byte_Array (0 .. 15) := (16#48#, 16#65#, 16#6C#, 16#6C#,  --  "Hell"
                                            16#6F#, 16#20#, 16#53#, 16#65#,  --  "o Se"
                                            16#63#, 16#72#, 16#65#, 16#74#,  --  "cret"
                                            others => 16#00#);
      AAD        : Byte_Array (0 .. 7) := (others => 16#33#);
      Ciphertext : Byte_Array (0 .. 15);
      Decrypted  : Byte_Array (0 .. 15);
      Tag        : AEAD_Tag;
      Bad_Tag    : AEAD_Tag := (others => 16#00#);
      Success    : Boolean;

      No_Leak    : Boolean := True;
   begin
      --  Encrypt
      AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Ciphertext, Tag);

      --  Attempt decrypt with wrong tag
      AEAD_Decrypt (Key, Nonce, Ciphertext, AAD, Bad_Tag, Decrypted, Success);

      --  Decrypted buffer should be all zeros (no partial plaintext leaked)
      if not Is_All_Zero (Decrypted) then
         No_Leak := False;
      end if;

      --  Verify none of the original plaintext bytes are present
      for I in Decrypted'Range loop
         if Decrypted (I) = Plaintext (I) then
            No_Leak := False;
            exit;
         end if;
      end loop;

      Report_Test ("No plaintext leakage on authentication failure",
                   not Success and No_Leak);
   end Test_No_Info_Leakage_On_Auth_Failure;

   --  Test 8: Large buffer secure wipe
   procedure Test_Large_Buffer_Wipe is
      Large_Buffer : Byte_Array (0 .. 8191) := (others => 16#5A#);  --  8KB
   begin
      Anubis_Secure_Wipe.Wipe (Large_Buffer);
      Report_Test ("Large buffer (8KB) secure wipe", Is_All_Zero (Large_Buffer));
   end Test_Large_Buffer_Wipe;

   --  Test 9: Multiple wipes are idempotent
   procedure Test_Multiple_Wipes_Idempotent is
      Buffer : Byte_Array (0 .. 63) := (others => 16#99#);
   begin
      --  Wipe multiple times
      for I in 1 .. 5 loop
         Anubis_Secure_Wipe.Wipe (Buffer);
      end loop;

      Report_Test ("Multiple wipes are idempotent", Is_All_Zero (Buffer));
   end Test_Multiple_Wipes_Idempotent;

begin
   Put_Line ("Security Tests for AnubisVM");
   Put_Line ("============================");
   New_Line;

   Put_Line ("Secure Wipe Tests:");
   Test_Secure_Wipe;
   Test_Large_Buffer_Wipe;
   Test_Multiple_Wipes_Idempotent;
   New_Line;

   Put_Line ("Zeroization Tests:");
   Test_KMAC_Key_Zeroization;
   Test_AEAD_Zeroization;
   Test_Failed_Decrypt_Zeros_Output;
   New_Line;

   Put_Line ("Timing Attack Resistance:");
   Test_Constant_Time_Verification;
   New_Line;

   Put_Line ("Memory Isolation Tests:");
   Test_Memory_Isolation;
   New_Line;

   Put_Line ("Information Leakage Tests:");
   Test_No_Info_Leakage_On_Auth_Failure;
   New_Line;

   Put_Line ("============================");
   Put_Line ("Tests run:    " & Natural'Image (Test_Count));
   Put_Line ("Tests passed: " & Natural'Image (Pass_Count));
   Put_Line ("============================");

   if Test_Failed or Pass_Count /= Test_Count then
      Put_Line ("RESULT: SOME TESTS FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("RESULT: ALL TESTS PASSED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;
end Test_Security;
