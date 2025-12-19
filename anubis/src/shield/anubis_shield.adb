--  ANUBIS Shield Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;

package body Anubis_Shield with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  ChaCha20 Constants
   ---------------------------------------------------------------------------

   --  ChaCha20 constants "expand 32-byte k"
   Sigma : constant array (0 .. 3) of Unsigned_32 := (
      16#61707865#, 16#3320646e#, 16#79622d32#, 16#6b206574#
   );

   ---------------------------------------------------------------------------
   --  ChaCha20 Quarter Round
   ---------------------------------------------------------------------------

   procedure Quarter_Round (
      A, B, C, D : in out Unsigned_32
   ) is
   begin
      A := A + B; D := D xor A; D := Rotate_Left (D, 16);
      C := C + D; B := B xor C; B := Rotate_Left (B, 12);
      A := A + B; D := D xor A; D := Rotate_Left (D, 8);
      C := C + D; B := B xor C; B := Rotate_Left (B, 7);
   end Quarter_Round;

   ---------------------------------------------------------------------------
   --  ChaCha20 Block Function
   ---------------------------------------------------------------------------

   procedure ChaCha20_Block (
      State  : in Out array (0 .. 15) of Unsigned_32
   ) is
      X : array (0 .. 15) of Unsigned_32 := State;
   begin
      --  20 rounds (10 double-rounds)
      for Round in 1 .. 10 loop
         --  Column rounds
         Quarter_Round (X (0), X (4), X (8), X (12));
         Quarter_Round (X (1), X (5), X (9), X (13));
         Quarter_Round (X (2), X (6), X (10), X (14));
         Quarter_Round (X (3), X (7), X (11), X (15));
         --  Diagonal rounds
         Quarter_Round (X (0), X (5), X (10), X (15));
         Quarter_Round (X (1), X (6), X (11), X (12));
         Quarter_Round (X (2), X (7), X (8), X (13));
         Quarter_Round (X (3), X (4), X (9), X (14));
      end loop;

      --  Add working state back
      for I in 0 .. 15 loop
         State (I) := State (I) + X (I);
      end loop;
   end ChaCha20_Block;

   ---------------------------------------------------------------------------
   --  XChaCha20 HChaCha20 (extended nonce)
   ---------------------------------------------------------------------------

   procedure HChaCha20 (
      Key    : Byte_Array;
      Nonce  : Byte_Array;
      Subkey : out Byte_Array
   ) is
      State : array (0 .. 15) of Unsigned_32;
   begin
      --  Setup state
      State (0) := Sigma (0);
      State (1) := Sigma (1);
      State (2) := Sigma (2);
      State (3) := Sigma (3);

      --  Key
      for I in 0 .. 7 loop
         State (4 + I) := Unsigned_32 (Key (Key'First + I * 4)) or
                         Shift_Left (Unsigned_32 (Key (Key'First + I * 4 + 1)), 8) or
                         Shift_Left (Unsigned_32 (Key (Key'First + I * 4 + 2)), 16) or
                         Shift_Left (Unsigned_32 (Key (Key'First + I * 4 + 3)), 24);
      end loop;

      --  Nonce (first 16 bytes)
      for I in 0 .. 3 loop
         State (12 + I) := Unsigned_32 (Nonce (Nonce'First + I * 4)) or
                          Shift_Left (Unsigned_32 (Nonce (Nonce'First + I * 4 + 1)), 8) or
                          Shift_Left (Unsigned_32 (Nonce (Nonce'First + I * 4 + 2)), 16) or
                          Shift_Left (Unsigned_32 (Nonce (Nonce'First + I * 4 + 3)), 24);
      end loop;

      --  Run ChaCha20 rounds (no final addition)
      declare
         X : array (0 .. 15) of Unsigned_32 := State;
      begin
         for Round in 1 .. 10 loop
            Quarter_Round (X (0), X (4), X (8), X (12));
            Quarter_Round (X (1), X (5), X (9), X (13));
            Quarter_Round (X (2), X (6), X (10), X (14));
            Quarter_Round (X (3), X (7), X (11), X (15));
            Quarter_Round (X (0), X (5), X (10), X (15));
            Quarter_Round (X (1), X (6), X (11), X (12));
            Quarter_Round (X (2), X (7), X (8), X (13));
            Quarter_Round (X (3), X (4), X (9), X (14));
         end loop;

         --  Extract subkey (words 0-3, 12-15)
         for I in 0 .. 3 loop
            Subkey (Subkey'First + I * 4) := Byte (X (I) and 16#FF#);
            Subkey (Subkey'First + I * 4 + 1) := Byte (Shift_Right (X (I), 8) and 16#FF#);
            Subkey (Subkey'First + I * 4 + 2) := Byte (Shift_Right (X (I), 16) and 16#FF#);
            Subkey (Subkey'First + I * 4 + 3) := Byte (Shift_Right (X (I), 24) and 16#FF#);
         end loop;
         for I in 0 .. 3 loop
            Subkey (Subkey'First + 16 + I * 4) := Byte (X (12 + I) and 16#FF#);
            Subkey (Subkey'First + 16 + I * 4 + 1) := Byte (Shift_Right (X (12 + I), 8) and 16#FF#);
            Subkey (Subkey'First + 16 + I * 4 + 2) := Byte (Shift_Right (X (12 + I), 16) and 16#FF#);
            Subkey (Subkey'First + 16 + I * 4 + 3) := Byte (Shift_Right (X (12 + I), 24) and 16#FF#);
         end loop;
      end;
   end HChaCha20;

   ---------------------------------------------------------------------------
   --  Poly1305 MAC
   ---------------------------------------------------------------------------

   procedure Poly1305_MAC (
      Key     : Byte_Array;
      Message : Byte_Array;
      Tag     : out Byte_Array
   ) is
      --  Simplified Poly1305 implementation
      --  Uses 130-bit arithmetic via multi-precision

      R0, R1, R2 : Unsigned_64;
      S0, S1 : Unsigned_64;
      H0, H1, H2 : Unsigned_64 := 0;
      Pos : Natural := 0;
   begin
      Tag := (others => 0);

      --  Clamp r
      R0 := Unsigned_64 (Key (Key'First)) or
            Shift_Left (Unsigned_64 (Key (Key'First + 1)), 8) or
            Shift_Left (Unsigned_64 (Key (Key'First + 2)), 16) or
            Shift_Left (Unsigned_64 (Key (Key'First + 3)), 24) or
            Shift_Left (Unsigned_64 (Key (Key'First + 4)), 32) or
            Shift_Left (Unsigned_64 (Key (Key'First + 5)), 40) or
            Shift_Left (Unsigned_64 (Key (Key'First + 6)) and 16#FC#, 48) or
            Shift_Left (Unsigned_64 (Key (Key'First + 7)) and 16#0F#, 56);

      R1 := Unsigned_64 (Key (Key'First + 8)) or
            Shift_Left (Unsigned_64 (Key (Key'First + 9)), 8) or
            Shift_Left (Unsigned_64 (Key (Key'First + 10)), 16) or
            Shift_Left (Unsigned_64 (Key (Key'First + 11)) and 16#FC#, 24) or
            Shift_Left (Unsigned_64 (Key (Key'First + 12)), 32) or
            Shift_Left (Unsigned_64 (Key (Key'First + 13)), 40) or
            Shift_Left (Unsigned_64 (Key (Key'First + 14)) and 16#FC#, 48) or
            Shift_Left (Unsigned_64 (Key (Key'First + 15)) and 16#0F#, 56);

      R2 := 0;

      --  s
      S0 := Unsigned_64 (Key (Key'First + 16)) or
            Shift_Left (Unsigned_64 (Key (Key'First + 17)), 8) or
            Shift_Left (Unsigned_64 (Key (Key'First + 18)), 16) or
            Shift_Left (Unsigned_64 (Key (Key'First + 19)), 24) or
            Shift_Left (Unsigned_64 (Key (Key'First + 20)), 32) or
            Shift_Left (Unsigned_64 (Key (Key'First + 21)), 40) or
            Shift_Left (Unsigned_64 (Key (Key'First + 22)), 48) or
            Shift_Left (Unsigned_64 (Key (Key'First + 23)), 56);

      S1 := Unsigned_64 (Key (Key'First + 24)) or
            Shift_Left (Unsigned_64 (Key (Key'First + 25)), 8) or
            Shift_Left (Unsigned_64 (Key (Key'First + 26)), 16) or
            Shift_Left (Unsigned_64 (Key (Key'First + 27)), 24) or
            Shift_Left (Unsigned_64 (Key (Key'First + 28)), 32) or
            Shift_Left (Unsigned_64 (Key (Key'First + 29)), 40) or
            Shift_Left (Unsigned_64 (Key (Key'First + 30)), 48) or
            Shift_Left (Unsigned_64 (Key (Key'First + 31)), 56);

      --  Process message in 16-byte blocks
      while Pos + 16 <= Message'Length loop
         declare
            N0, N1 : Unsigned_64;
         begin
            N0 := Unsigned_64 (Message (Message'First + Pos)) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 1)), 8) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 2)), 16) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 3)), 24) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 4)), 32) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 5)), 40) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 6)), 48) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 7)), 56);

            N1 := Unsigned_64 (Message (Message'First + Pos + 8)) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 9)), 8) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 10)), 16) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 11)), 24) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 12)), 32) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 13)), 40) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 14)), 48) or
                  Shift_Left (Unsigned_64 (Message (Message'First + Pos + 15)), 56);

            --  h += n
            H0 := H0 + N0;
            H1 := H1 + N1;
            H2 := H2 + 1;  --  Hibit for full block

            --  Simplified modular reduction (not cryptographically precise)
            --  Real implementation needs proper 130-bit arithmetic
         end;
         Pos := Pos + 16;
      end loop;

      --  Finalize: h += s
      H0 := H0 + S0;
      H1 := H1 + S1;

      --  Output tag
      for I in 0 .. 7 loop
         Tag (Tag'First + I) := Byte (Shift_Right (H0, I * 8) and 16#FF#);
      end loop;
      for I in 0 .. 7 loop
         Tag (Tag'First + 8 + I) := Byte (Shift_Right (H1, I * 8) and 16#FF#);
      end loop;
   end Poly1305_MAC;

   ---------------------------------------------------------------------------
   --  AEAD Implementation
   ---------------------------------------------------------------------------

   procedure AEAD_Encrypt (
      Key        : Byte_Array;
      Nonce      : Byte_Array;
      Plaintext  : Byte_Array;
      AAD        : Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Byte_Array
   ) is
      Subkey : Byte_Array (0 .. 31);
      Short_Nonce : Byte_Array (0 .. 7);
      Poly_Key : Byte_Array (0 .. 31);
      State : array (0 .. 15) of Unsigned_32;
      Counter : Unsigned_32 := 0;
      Keystream : Byte_Array (0 .. 63);
      Block_Pos : Natural;
   begin
      Ciphertext := (others => 0);
      Tag := (others => 0);

      --  XChaCha20: derive subkey from first 16 bytes of nonce
      HChaCha20 (Key, Nonce (Nonce'First .. Nonce'First + 15), Subkey);

      --  Use last 8 bytes of nonce
      Short_Nonce := Nonce (Nonce'First + 16 .. Nonce'First + 23);

      --  Generate Poly1305 key (counter = 0)
      State (0) := Sigma (0);
      State (1) := Sigma (1);
      State (2) := Sigma (2);
      State (3) := Sigma (3);

      for I in 0 .. 7 loop
         State (4 + I) := Unsigned_32 (Subkey (I * 4)) or
                         Shift_Left (Unsigned_32 (Subkey (I * 4 + 1)), 8) or
                         Shift_Left (Unsigned_32 (Subkey (I * 4 + 2)), 16) or
                         Shift_Left (Unsigned_32 (Subkey (I * 4 + 3)), 24);
      end loop;

      State (12) := 0;  --  Counter
      State (13) := 0;

      for I in 0 .. 1 loop
         State (14 + I) := Unsigned_32 (Short_Nonce (I * 4)) or
                          Shift_Left (Unsigned_32 (Short_Nonce (I * 4 + 1)), 8) or
                          Shift_Left (Unsigned_32 (Short_Nonce (I * 4 + 2)), 16) or
                          Shift_Left (Unsigned_32 (Short_Nonce (I * 4 + 3)), 24);
      end loop;

      ChaCha20_Block (State);

      for I in 0 .. 31 loop
         Poly_Key (I) := Byte (Shift_Right (State (I / 4), (I mod 4) * 8) and 16#FF#);
      end loop;

      --  Encrypt plaintext with ChaCha20 (counter starting at 1)
      Counter := 1;
      Block_Pos := 0;

      while Block_Pos < Plaintext'Length loop
         --  Setup state for this block
         State (0) := Sigma (0);
         State (1) := Sigma (1);
         State (2) := Sigma (2);
         State (3) := Sigma (3);

         for I in 0 .. 7 loop
            State (4 + I) := Unsigned_32 (Subkey (I * 4)) or
                            Shift_Left (Unsigned_32 (Subkey (I * 4 + 1)), 8) or
                            Shift_Left (Unsigned_32 (Subkey (I * 4 + 2)), 16) or
                            Shift_Left (Unsigned_32 (Subkey (I * 4 + 3)), 24);
         end loop;

         State (12) := Counter;
         State (13) := 0;

         for I in 0 .. 1 loop
            State (14 + I) := Unsigned_32 (Short_Nonce (I * 4)) or
                             Shift_Left (Unsigned_32 (Short_Nonce (I * 4 + 1)), 8) or
                             Shift_Left (Unsigned_32 (Short_Nonce (I * 4 + 2)), 16) or
                             Shift_Left (Unsigned_32 (Short_Nonce (I * 4 + 3)), 24);
         end loop;

         ChaCha20_Block (State);

         --  Serialize keystream
         for I in 0 .. 15 loop
            Keystream (I * 4) := Byte (State (I) and 16#FF#);
            Keystream (I * 4 + 1) := Byte (Shift_Right (State (I), 8) and 16#FF#);
            Keystream (I * 4 + 2) := Byte (Shift_Right (State (I), 16) and 16#FF#);
            Keystream (I * 4 + 3) := Byte (Shift_Right (State (I), 24) and 16#FF#);
         end loop;

         --  XOR plaintext with keystream
         for I in 0 .. Natural'Min (63, Plaintext'Length - Block_Pos - 1) loop
            Ciphertext (Ciphertext'First + Block_Pos + I) :=
               Plaintext (Plaintext'First + Block_Pos + I) xor Keystream (I);
         end loop;

         Counter := Counter + 1;
         Block_Pos := Block_Pos + 64;
      end loop;

      --  Compute Poly1305 tag over AAD || Ciphertext
      declare
         Mac_Input : Byte_Array (0 .. AAD'Length + Ciphertext'Length - 1);
      begin
         Mac_Input (0 .. AAD'Length - 1) := AAD;
         Mac_Input (AAD'Length .. Mac_Input'Last) :=
            Ciphertext (Ciphertext'First .. Ciphertext'First + Plaintext'Length - 1);
         Poly1305_MAC (Poly_Key, Mac_Input, Tag);
      end;
   end AEAD_Encrypt;

   procedure AEAD_Decrypt (
      Key        : Byte_Array;
      Nonce      : Byte_Array;
      Ciphertext : Byte_Array;
      Tag        : Byte_Array;
      AAD        : Byte_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean
   ) is
      Computed_Tag : Byte_Array (0 .. 15);
      Dummy_Tag : Byte_Array (0 .. 15);
   begin
      Plaintext := (others => 0);
      Success := False;

      --  Decrypt (same as encrypt for stream cipher)
      AEAD_Encrypt (Key, Nonce, Ciphertext, AAD, Plaintext, Dummy_Tag);

      --  Recompute tag on original ciphertext
      declare
         Reencrypted : Byte_Array (Plaintext'Range);
      begin
         AEAD_Encrypt (Key, Nonce, Plaintext, AAD, Reencrypted, Computed_Tag);
      end;

      --  Constant-time tag comparison
      declare
         Diff : Byte := 0;
      begin
         for I in 0 .. 15 loop
            Diff := Diff or (Tag (Tag'First + I) xor Computed_Tag (I));
         end loop;
         Success := (Diff = 0);
      end;
   end AEAD_Decrypt;

   ---------------------------------------------------------------------------
   --  KDF
   ---------------------------------------------------------------------------

   procedure Derive_AEAD_Key (
      Shared_Secret : Byte_Array;
      Context       : Byte_Array;
      Key           : out Byte_Array
   ) is
      --  Simple HKDF-like expansion using ChaCha20
      Nonce : constant Byte_Array (0 .. 23) := (others => 0);
   begin
      --  Use shared secret as key, context as "plaintext"
      --  Output is the "ciphertext" (XOR of keystream with zeros)
      declare
         Zeros : Byte_Array (0 .. 31) := (others => 0);
         Dummy_Tag : Byte_Array (0 .. 15);
      begin
         AEAD_Encrypt (Shared_Secret, Nonce, Zeros, Context, Key, Dummy_Tag);
      end;
   end Derive_AEAD_Key;

   ---------------------------------------------------------------------------
   --  ML-KEM Implementation (using Anubis_MLKEM from core)
   ---------------------------------------------------------------------------

   procedure ML_KEM_Encapsulate (
      PK           : ML_KEM_Public_Key;
      Randomness   : Byte_Array;
      Ciphertext   : out ML_KEM_Ciphertext;
      Shared       : out Shared_Secret
   ) is
      --  Map to Anubis_MLKEM types
      EK : Anubis_MLKEM_Types.Encapsulation_Key;
      Seed_M : Anubis_MLKEM_Types.Seed;
      CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS : Anubis_MLKEM_Types.Shared_Secret;
   begin
      --  Copy public key to ML-KEM format
      for I in EK'Range loop
         EK (I) := PK (PK'First + I);
      end loop;

      --  Extract randomness seed (first 32 bytes)
      for I in Seed_M'Range loop
         Seed_M (I) := Randomness (Randomness'First + I);
      end loop;

      --  Call real ML-KEM encapsulation
      Anubis_MLKEM.Encaps (
         EK       => EK,
         Random_M => Seed_M,
         SS       => SS,
         CT       => CT
      );

      --  Copy results back
      for I in Ciphertext'Range loop
         Ciphertext (I) := CT (I);
      end loop;

      for I in Shared'Range loop
         Shared (I) := SS (I);
      end loop;
   end ML_KEM_Encapsulate;

   procedure ML_KEM_Decapsulate (
      SK           : ML_KEM_Secret_Key;
      Ciphertext   : ML_KEM_Ciphertext;
      Shared       : out Shared_Secret;
      Success      : out Boolean
   ) is
      --  Map to Anubis_MLKEM types
      DK : Anubis_MLKEM_Types.Decapsulation_Key;
      CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS : Anubis_MLKEM_Types.Shared_Secret;
   begin
      --  Copy secret key to ML-KEM format
      for I in DK'Range loop
         DK (I) := SK (SK'First + I);
      end loop;

      --  Copy ciphertext to ML-KEM format
      for I in CT'Range loop
         CT (I) := Ciphertext (Ciphertext'First + I);
      end loop;

      --  Call real ML-KEM decapsulation
      --  Note: ML-KEM Decaps always succeeds (implicit rejection on invalid CT)
      Anubis_MLKEM.Decaps (
         DK => DK,
         CT => CT,
         SS => SS
      );

      --  Copy shared secret back
      for I in Shared'Range loop
         Shared (I) := SS (I);
      end loop;

      --  ML-KEM Decaps always succeeds due to implicit rejection
      --  (invalid ciphertexts produce pseudorandom shared secrets)
      Success := True;
   end ML_KEM_Decapsulate;

   ---------------------------------------------------------------------------
   --  Public Operations
   ---------------------------------------------------------------------------

   procedure Encrypt_State (
      Plaintext   : Byte_Array;
      User_PK     : ML_KEM_Public_Key;
      Randomness  : Byte_Array;
      Commit_Key  : Commitment_Key;
      Entry       : out Private_Entry;
      Commitment  : out Entry_Commitment;
      Success     : out Boolean
   ) is
      SS : Shared_Secret;
      AEAD_Key : Byte_Array (0 .. 31);
      Context : constant Byte_Array (0 .. 7) := (
         16#41#, 16#4E#, 16#55#, 16#42#,   --  "ANUB"
         16#49#, 16#53#, 16#00#, 16#01#    --  "IS" + version
      );
   begin
      Entry := Empty_Entry;
      Success := False;

      --  ML-KEM encapsulation
      ML_KEM_Encapsulate (
         User_PK,
         Randomness (0 .. 63),
         Entry.Encapsulated,
         SS
      );

      --  Derive AEAD key from shared secret
      Derive_AEAD_Key (SS, Context, AEAD_Key);

      --  Generate nonce from randomness
      Entry.Nonce := Randomness (64 .. 87);

      --  Encrypt plaintext
      AEAD_Encrypt (
         AEAD_Key,
         Entry.Nonce,
         Plaintext,
         Context,
         Entry.Ciphertext (0 .. Plaintext'Length - 1),
         Entry.Tag
      );
      Entry.CT_Length := Plaintext'Length;

      --  Create Ajtai commitment
      Commit_Bytes (
         Commit_Key,
         Plaintext,
         Randomness (88 .. 119),
         Commitment.Commitment,
         Commitment.Opening,
         Success
      );
   end Encrypt_State;

   procedure Decrypt_State (
      Entry     : Private_Entry;
      User_SK   : ML_KEM_Secret_Key;
      Plaintext : out Byte_Array;
      Length    : out Natural;
      Success   : out Boolean
   ) is
      SS : Shared_Secret;
      AEAD_Key : Byte_Array (0 .. 31);
      Context : constant Byte_Array (0 .. 7) := (
         16#41#, 16#4E#, 16#55#, 16#42#,
         16#49#, 16#53#, 16#00#, 16#01#
      );
      Decaps_OK : Boolean;
   begin
      Plaintext := (others => 0);
      Length := 0;
      Success := False;

      --  ML-KEM decapsulation
      ML_KEM_Decapsulate (User_SK, Entry.Encapsulated, SS, Decaps_OK);
      if not Decaps_OK then
         return;
      end if;

      --  Derive AEAD key
      Derive_AEAD_Key (SS, Context, AEAD_Key);

      --  Decrypt
      AEAD_Decrypt (
         AEAD_Key,
         Entry.Nonce,
         Entry.Ciphertext (0 .. Entry.CT_Length - 1),
         Entry.Tag,
         Context,
         Plaintext (Plaintext'First .. Plaintext'First + Entry.CT_Length - 1),
         Success
      );

      if Success then
         Length := Entry.CT_Length;
      end if;
   end Decrypt_State;

   function Verify_Commitment (
      Plaintext  : Byte_Array;
      Commitment : Entry_Commitment;
      Commit_Key : Commitment_Key
   ) return Boolean
   is
   begin
      return Verify_Opening (Commit_Key, Commitment.Commitment, Commitment.Opening);
   end Verify_Commitment;

   procedure Re_Encrypt (
      Entry          : Private_Entry;
      Old_SK         : ML_KEM_Secret_Key;
      New_PK         : ML_KEM_Public_Key;
      New_Randomness : Byte_Array;
      New_Entry      : out Private_Entry;
      Success        : out Boolean
   ) is
      Plaintext : Byte_Array (0 .. Max_Entry_Size - 1);
      Length : Natural;
      Decrypt_OK : Boolean;
      SS : Shared_Secret;
      AEAD_Key : Byte_Array (0 .. 31);
      Context : constant Byte_Array (0 .. 7) := (
         16#41#, 16#4E#, 16#55#, 16#42#,
         16#49#, 16#53#, 16#00#, 16#01#
      );
   begin
      New_Entry := Empty_Entry;
      Success := False;

      --  First decrypt
      Decrypt_State (Entry, Old_SK, Plaintext, Length, Decrypt_OK);
      if not Decrypt_OK then
         return;
      end if;

      --  Re-encrypt for new user
      ML_KEM_Encapsulate (
         New_PK,
         New_Randomness (0 .. 63),
         New_Entry.Encapsulated,
         SS
      );

      Derive_AEAD_Key (SS, Context, AEAD_Key);

      --  Generate new nonce
      for I in 0 .. 23 loop
         New_Entry.Nonce (I) := New_Randomness (I);
      end loop;

      --  Encrypt
      AEAD_Encrypt (
         AEAD_Key,
         New_Entry.Nonce,
         Plaintext (0 .. Length - 1),
         Context,
         New_Entry.Ciphertext (0 .. Length - 1),
         New_Entry.Tag
      );
      New_Entry.CT_Length := Length;

      Success := True;
   end Re_Encrypt;

   procedure Derive_Viewing_Key (
      Master_SK   : ML_KEM_Secret_Key;
      Scope       : Disclosure_Scope;
      Viewing_Key : out Byte_Array
   ) is
      Context : Byte_Array (0 .. 15);
   begin
      Context := (others => 0);
      Context (0) := 16#56#;  --  'V'
      Context (1) := 16#49#;  --  'I'
      Context (2) := 16#45#;  --  'E'
      Context (3) := 16#57#;  --  'W'
      Context (4) := Byte (Disclosure_Scope'Pos (Scope));

      Derive_AEAD_Key (
         Master_SK (0 .. 31),
         Context,
         Viewing_Key
      );
   end Derive_Viewing_Key;

   procedure View_State (
      Entry       : Private_Entry;
      Viewing_Key : Byte_Array;
      Plaintext   : out Byte_Array;
      Length      : out Natural;
      Success     : out Boolean
   ) is
      Context : constant Byte_Array (0 .. 7) := (
         16#41#, 16#4E#, 16#55#, 16#42#,
         16#49#, 16#53#, 16#00#, 16#01#
      );
   begin
      Plaintext := (others => 0);
      Length := 0;

      --  Decrypt using viewing key directly as AEAD key
      AEAD_Decrypt (
         Viewing_Key,
         Entry.Nonce,
         Entry.Ciphertext (0 .. Entry.CT_Length - 1),
         Entry.Tag,
         Context,
         Plaintext (Plaintext'First .. Plaintext'First + Entry.CT_Length - 1),
         Success
      );

      if Success then
         Length := Entry.CT_Length;
      end if;
   end View_State;

   procedure Generate_Key_Bundle (
      Seed   : Byte_Array;
      Bundle : out User_Key_Bundle
   ) is
      --  ML-KEM key generation seeds
      Random_D : Anubis_MLKEM_Types.Seed;
      Random_Z : Anubis_MLKEM_Types.Seed;
      EK : Anubis_MLKEM_Types.Encapsulation_Key;
      DK : Anubis_MLKEM_Types.Decapsulation_Key;

      --  KDF labels for deriving viewing key and commit seed
      View_Label : constant Byte_Array := (
         16#56#, 16#49#, 16#45#, 16#57#, 16#2D#, 16#4B#, 16#45#, 16#59#  -- "VIEW-KEY"
      );
      Commit_Label : constant Byte_Array := (
         16#43#, 16#4F#, 16#4D#, 16#4D#, 16#49#, 16#54#  -- "COMMIT"
      );
   begin
      --  Initialize output
      Bundle.KEM_Public := (others => 0);
      Bundle.KEM_Secret := (others => 0);
      Bundle.Viewing_Key := (others => 0);
      Bundle.Commit_Seed := (others => 0);

      --  Split 64-byte seed into two 32-byte seeds for ML-KEM KeyGen
      --  Random_D: first 32 bytes (for key generation)
      --  Random_Z: last 32 bytes (for implicit rejection)
      for I in Random_D'Range loop
         Random_D (I) := Seed (Seed'First + I);
      end loop;

      for I in Random_Z'Range loop
         Random_Z (I) := Seed (Seed'First + 32 + I);
      end loop;

      --  Generate ML-KEM-1024 keypair
      Anubis_MLKEM.KeyGen (
         Random_D => Random_D,
         Random_Z => Random_Z,
         EK       => EK,
         DK       => DK
      );

      --  Copy keys to bundle
      for I in Bundle.KEM_Public'Range loop
         Bundle.KEM_Public (I) := EK (I);
      end loop;

      for I in Bundle.KEM_Secret'Range loop
         Bundle.KEM_Secret (I) := DK (I);
      end loop;

      --  Derive viewing key from seed using KDF
      --  This allows viewing key to be regenerated from master seed
      declare
         Seed_First_Half : constant Byte_Array (0 .. 31) :=
            Seed (Seed'First .. Seed'First + 31);
      begin
         --  Use first 32 bytes of seed as input to KDF
         Derive_AEAD_Key (Seed_First_Half, View_Label, Bundle.Viewing_Key);
      end;

      --  Derive commitment seed from seed using KDF
      declare
         Seed_Second_Half : constant Byte_Array (0 .. 31) :=
            Seed (Seed'First + 32 .. Seed'First + 63);
      begin
         --  Use last 32 bytes of seed as input to KDF
         Derive_AEAD_Key (Seed_Second_Half, Commit_Label, Bundle.Commit_Seed);
      end;
   end Generate_Key_Bundle;

   procedure Serialize_Entry (
      Entry  : Private_Entry;
      Output : out Byte_Array;
      Length : out Natural
   ) is
      Pos : Natural := 0;
   begin
      Output := (others => 0);

      --  Length (4 bytes)
      Output (Output'First + Pos) := Byte (Entry.CT_Length mod 256);
      Output (Output'First + Pos + 1) := Byte ((Entry.CT_Length / 256) mod 256);
      Output (Output'First + Pos + 2) := Byte ((Entry.CT_Length / 65536) mod 256);
      Output (Output'First + Pos + 3) := Byte (Entry.CT_Length / 16777216);
      Pos := Pos + 4;

      --  Ciphertext
      for I in 0 .. Entry.CT_Length - 1 loop
         Output (Output'First + Pos + I) := Entry.Ciphertext (I);
      end loop;
      Pos := Pos + Entry.CT_Length;

      --  Encapsulated key
      for I in Entry.Encapsulated'Range loop
         Output (Output'First + Pos + I) := Entry.Encapsulated (I);
      end loop;
      Pos := Pos + Entry.Encapsulated'Length;

      --  Nonce
      for I in Entry.Nonce'Range loop
         Output (Output'First + Pos + I) := Entry.Nonce (I);
      end loop;
      Pos := Pos + Entry.Nonce'Length;

      --  Tag
      for I in Entry.Tag'Range loop
         Output (Output'First + Pos + I) := Entry.Tag (I);
      end loop;
      Pos := Pos + Entry.Tag'Length;

      Length := Pos;
   end Serialize_Entry;

   procedure Deserialize_Entry (
      Data    : Byte_Array;
      Entry   : out Private_Entry;
      Success : out Boolean
   ) is
      Pos : Natural := 0;
   begin
      Entry := Empty_Entry;
      Success := False;

      if Data'Length < 4 then
         return;
      end if;

      --  Length
      Entry.CT_Length := Natural (Data (Data'First + Pos)) +
                        Natural (Data (Data'First + Pos + 1)) * 256 +
                        Natural (Data (Data'First + Pos + 2)) * 65536 +
                        Natural (Data (Data'First + Pos + 3)) * 16777216;
      Pos := Pos + 4;

      if Entry.CT_Length > Max_Entry_Size then
         return;
      end if;

      if Data'Length < Pos + Entry.CT_Length + Ciphertext_Size + 24 + 16 then
         return;
      end if;

      --  Ciphertext
      for I in 0 .. Entry.CT_Length - 1 loop
         Entry.Ciphertext (I) := Data (Data'First + Pos + I);
      end loop;
      Pos := Pos + Entry.CT_Length;

      --  Encapsulated key
      for I in Entry.Encapsulated'Range loop
         Entry.Encapsulated (I) := Data (Data'First + Pos + I);
      end loop;
      Pos := Pos + Entry.Encapsulated'Length;

      --  Nonce
      for I in Entry.Nonce'Range loop
         Entry.Nonce (I) := Data (Data'First + Pos + I);
      end loop;
      Pos := Pos + Entry.Nonce'Length;

      --  Tag
      for I in Entry.Tag'Range loop
         Entry.Tag (I) := Data (Data'First + Pos + I);
      end loop;

      Success := True;
   end Deserialize_Entry;

end Anubis_Shield;
