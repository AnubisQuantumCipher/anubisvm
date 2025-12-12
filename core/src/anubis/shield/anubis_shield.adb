-------------------------------------------------------------------------------
--  ANUBIS SHIELD - Encrypted Private State (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces;   use Interfaces;
with Anubis_SHA3;
with Anubis_ChaCha20_Poly1305; use Anubis_ChaCha20_Poly1305;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;

package body Anubis_Shield with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Helper: SHAKE256-based key derivation
   ---------------------------------------------------------------------------

   --  Maximum KDF input size (label + input combined)
   KDF_Max_Size : constant := 128;

   procedure SHAKE256_KDF (
      Input  : Byte_Array;
      Label  : Byte_Array;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre => Input'Length > 0
             and then Label'Length > 0
             and then Output'Length > 0
             and then Output'Length <= 65535  -- SHAKE256 constraint
             and then Output'Last < Natural'Last  -- SHAKE256 constraint
             and then Input'Length <= KDF_Max_Size
             and then Label'Length <= KDF_Max_Size
             and then Input'Length + Label'Length <= KDF_Max_Size
   is
      Combined_Len : constant Natural := Input'Length + Label'Length;
      Combined : Byte_Array (0 .. KDF_Max_Size - 1) := (others => 0);
   begin
      --  Copy label then input into combined buffer
      for I in 0 .. Label'Length - 1 loop
         Combined (I) := Label (Label'First + I);
         pragma Loop_Invariant (I < Label'Length);
      end loop;

      for I in 0 .. Input'Length - 1 loop
         Combined (Label'Length + I) := Input (Input'First + I);
         pragma Loop_Invariant (I < Input'Length);
         pragma Loop_Invariant (Label'Length + I < KDF_Max_Size);
      end loop;

      --  Hash only the actual data portion
      Anubis_SHA3.SHAKE256 (Combined (0 .. Combined_Len - 1), Output, Output'Length);
   end SHAKE256_KDF;

   ---------------------------------------------------------------------------
   --  Encrypt_State
   ---------------------------------------------------------------------------

   procedure Encrypt_State (
      Plaintext      : Byte_Array;
      User_KEM_PK    : Byte_Array;
      Randomness     : Byte_Array;
      Priv_Entry     : out Private_Entry;
      Commitment     : out Entry_Commitment;
      Success        : out Boolean
   ) is
      SS_Buffer : Byte_Array (0 .. 31);
      AEAD_Key  : Byte_Array (0 .. 31);
      KDF_Label     : constant Byte_Array := (
         16#41#, 16#4E#, 16#55#, 16#42#, 16#49#, 16#53#, 16#2D#, 16#53#,
         16#48#, 16#49#, 16#45#, 16#4C#, 16#44#  -- "ANUBIS-SHIELD"
      );
      EK        : Anubis_MLKEM_Types.Encapsulation_Key;
      CT        : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS        : Anubis_MLKEM_Types.Shared_Secret;
      Seed_M    : Anubis_MLKEM_Types.Seed;
   begin
      --  Initialize output
      Priv_Entry.Ciphertext := (others => 0);
      Priv_Entry.CT_Length := 0;
      Priv_Entry.Encapsulated := (others => 0);
      Priv_Entry.Nonce := (others => 0);
      Priv_Entry.Tag := (others => 0);
      Commitment.Value := (others => 0);
      Success := False;

      --  Generate nonce from randomness
      Priv_Entry.Nonce := Randomness (0 .. AEAD_Nonce_Size - 1);

      --  ML-KEM encapsulation: derive shared secret using receiver's public key
      --  Map User_KEM_PK into ML-KEM Encapsulation_Key
      for I in EK'Range loop
         EK (I) := User_KEM_PK (User_KEM_PK'First + I);
      end loop;

      --  Derive encapsulation randomness from Randomness (first 32 bytes)
      for I in Seed_M'Range loop
         Seed_M (I) := Randomness (Randomness'First + I);
      end loop;

      --  Encapsulate shared secret and ciphertext
      Anubis_MLKEM.Encaps (
         EK       => EK,
         Random_M => Seed_M,
         SS       => SS,
         CT       => CT
      );

      --  Copy ciphertext into entry
      for I in Priv_Entry.Encapsulated'Range loop
         Priv_Entry.Encapsulated (I) := CT (I);
      end loop;

      --  Derive symmetric shared secret buffer from ML-KEM shared secret
      for I in SS_Buffer'Range loop
         SS_Buffer (I) := SS (I);
      end loop;

      --  Derive AEAD key from shared secret
      declare
         Key_Label : constant Byte_Array := (
            16#41#, 16#45#, 16#41#, 16#44#, 16#2D#, 16#4B#, 16#45#, 16#59#
         );  -- "AEAD-KEY"
      begin
         SHAKE256_KDF (
            Input  => SS_Buffer,
            Label  => Key_Label,
            Output => AEAD_Key
         );
      end;

      --  ChaCha20-Poly1305 AEAD encryption
      declare
         Nonce_For_AEAD : AEAD_Nonce;
         Key_For_AEAD   : Anubis_ChaCha20_Poly1305.AEAD_Key;
         Tag_Out        : AEAD_Tag;
         CT_Out         : Byte_Array (0 .. Plaintext'Length - 1);
         --  Empty AAD for now (could include context/address)
         Empty_AAD      : constant Byte_Array (0 .. 0) := (others => 0);
      begin
         --  Copy key and nonce to correct types
         for I in Key_For_AEAD'Range loop
            Key_For_AEAD (I) := AEAD_Key (I);
         end loop;
         for I in Nonce_For_AEAD'Range loop
            Nonce_For_AEAD (I) := Priv_Entry.Nonce (I);
         end loop;

         --  Perform AEAD encryption
         Anubis_ChaCha20_Poly1305.AEAD_Encrypt (
            Ciphertext => CT_Out,
            Tag        => Tag_Out,
            Plaintext  => Plaintext,
            AAD        => Empty_AAD (0 .. -1),  --  Empty slice
            Nonce      => Nonce_For_AEAD,
            Key        => Key_For_AEAD
         );

         --  Copy results to output
         for I in CT_Out'Range loop
            Priv_Entry.Ciphertext (I) := CT_Out (I);
         end loop;
         --  Copy tag bytes (type conversion)
         for I in Priv_Entry.Tag'Range loop
            Priv_Entry.Tag (I) := Tag_Out (I);
         end loop;
         Priv_Entry.CT_Length := Plaintext'Length;

         --  Zeroize local key
         Anubis_ChaCha20_Poly1305.Sanitize (Key_For_AEAD);
      end;

      --  Create commitment
      Create_Commitment (Plaintext, Randomness (0 .. 31), Commitment);

      --  Zeroize sensitive intermediates
      SS_Buffer := (others => 0);
      AEAD_Key := (others => 0);

      Success := True;
   end Encrypt_State;

   ---------------------------------------------------------------------------
   --  Decrypt_State
   ---------------------------------------------------------------------------

   procedure Decrypt_State (
      Priv_Entry     : Private_Entry;
      User_KEM_SK    : Byte_Array;
      Plaintext      : out Byte_Array;
      PT_Length      : out Natural;
      Success        : out Boolean
   ) is
      SS_Buffer     : Byte_Array (0 .. 31);
      AEAD_Key      : Byte_Array (0 .. 31);
      Key_Label     : constant Byte_Array := (
         16#41#, 16#45#, 16#41#, 16#44#, 16#2D#, 16#4B#, 16#45#, 16#59#
      );  -- "AEAD-KEY"
      DK            : Anubis_MLKEM_Types.Decapsulation_Key;
      CT            : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS            : Anubis_MLKEM_Types.Shared_Secret;
   begin
      Plaintext := (others => 0);
      PT_Length := 0;
      Success := False;

      --  ML-KEM decapsulation: recover shared secret from ciphertext
      for I in DK'Range loop
         DK (I) := User_KEM_SK (User_KEM_SK'First + I);
      end loop;

      for I in CT'Range loop
         CT (I) := Priv_Entry.Encapsulated (I);
      end loop;

      Anubis_MLKEM.Decaps (
         DK => DK,
         CT => CT,
         SS => SS
      );

      --  Derive symmetric shared secret buffer from ML-KEM shared secret
      for I in SS_Buffer'Range loop
         SS_Buffer (I) := SS (I);
      end loop;

      --  Derive AEAD key
      SHAKE256_KDF (
         Input  => SS_Buffer,
         Label  => Key_Label,
         Output => AEAD_Key
      );

      --  ChaCha20-Poly1305 AEAD decryption with integrated authentication
      if Priv_Entry.CT_Length > 0 and then Priv_Entry.CT_Length <= Max_Entry_Size then
         declare
            Nonce_For_AEAD : AEAD_Nonce;
            Key_For_AEAD   : Anubis_ChaCha20_Poly1305.AEAD_Key;
            Tag_For_AEAD   : Anubis_ChaCha20_Poly1305.AEAD_Tag;
            CT_In          : Byte_Array (0 .. Priv_Entry.CT_Length - 1);
            PT_Out         : Byte_Array (0 .. Priv_Entry.CT_Length - 1);
            Empty_AAD      : constant Byte_Array (0 .. 0) := (others => 0);
            Decrypt_OK     : Boolean;
         begin
            --  Copy key, nonce, and tag to correct types
            for I in Key_For_AEAD'Range loop
               Key_For_AEAD (I) := AEAD_Key (I);
            end loop;
            for I in Nonce_For_AEAD'Range loop
               Nonce_For_AEAD (I) := Priv_Entry.Nonce (I);
            end loop;
            for I in Tag_For_AEAD'Range loop
               Tag_For_AEAD (I) := Priv_Entry.Tag (I);
            end loop;

            --  Copy ciphertext
            for I in 0 .. Priv_Entry.CT_Length - 1 loop
               pragma Loop_Invariant (I < Priv_Entry.CT_Length);
               CT_In (I) := Priv_Entry.Ciphertext (I);
            end loop;

            --  Perform AEAD decryption (verifies tag internally)
            Anubis_ChaCha20_Poly1305.AEAD_Decrypt (
               Plaintext  => PT_Out,
               Success    => Decrypt_OK,
               Ciphertext => CT_In,
               Tag        => Tag_For_AEAD,
               AAD        => Empty_AAD (0 .. -1),  --  Empty slice
               Nonce      => Nonce_For_AEAD,
               Key        => Key_For_AEAD
            );

            if Decrypt_OK then
               --  Copy plaintext to output
               for I in 0 .. Priv_Entry.CT_Length - 1 loop
                  pragma Loop_Invariant (I < Priv_Entry.CT_Length);
                  Plaintext (I) := PT_Out (I);
               end loop;
               PT_Length := Priv_Entry.CT_Length;
               Success := True;
            else
               Plaintext := (others => 0);
               PT_Length := 0;
               Success := False;
            end if;

            --  Zeroize local key
            Anubis_ChaCha20_Poly1305.Sanitize (Key_For_AEAD);
         end;
      else
         PT_Length := 0;
         Success := False;
      end if;

      --  Zeroize intermediates
      SS_Buffer := (others => 0);
      AEAD_Key := (others => 0);
   end Decrypt_State;

   ---------------------------------------------------------------------------
   --  Verify_Commitment
   ---------------------------------------------------------------------------

   function Verify_Commitment (
      Plaintext      : Byte_Array;
      Commitment     : Entry_Commitment;
      Opening        : Byte_Array
   ) return Boolean is
      Computed : Entry_Commitment;
   begin
      Create_Commitment (Plaintext, Opening, Computed);

      --  Constant-time comparison
      declare
         Diff : Byte := 0;
      begin
         for I in Commitment.Value'Range loop
            Diff := Diff or (Commitment.Value (I) xor Computed.Value (I));
         end loop;
         return Diff = 0;
      end;
   end Verify_Commitment;

   ---------------------------------------------------------------------------
   --  Re_Encrypt
   ---------------------------------------------------------------------------

   procedure Re_Encrypt (
      Priv_Entry     : Private_Entry;
      Old_KEM_SK     : Byte_Array;
      New_KEM_PK     : Byte_Array;
      New_Randomness : Byte_Array;
      New_Entry      : out Private_Entry;
      Success        : out Boolean
   ) is
      Plaintext : Byte_Array (0 .. Max_Entry_Size - 1);
      PT_Length : Natural;
      Decrypt_OK : Boolean;
      Dummy_Commit : Entry_Commitment;
   begin
      --  Decrypt with old key
      Decrypt_State (Priv_Entry, Old_KEM_SK, Plaintext, PT_Length, Decrypt_OK);

      if not Decrypt_OK or PT_Length = 0 then
         New_Entry := (
            Ciphertext   => (others => 0),
            CT_Length    => 0,
            Encapsulated => (others => 0),
            Nonce        => (others => 0),
            Tag          => (others => 0)
         );
         Success := False;
         return;
      end if;

      --  Re-encrypt with new key (PT_Length > 0 guaranteed here)
      pragma Assert (PT_Length > 0);
      pragma Assert (PT_Length <= Max_Entry_Size);
      Encrypt_State (
         Plaintext   => Plaintext (0 .. PT_Length - 1),
         User_KEM_PK => New_KEM_PK,
         Randomness  => New_Randomness,
         Priv_Entry  => New_Entry,
         Commitment  => Dummy_Commit,
         Success     => Success
      );

      --  Zeroize plaintext
      Plaintext := (others => 0);
   end Re_Encrypt;

   ---------------------------------------------------------------------------
   --  Key Derivation
   ---------------------------------------------------------------------------

   procedure Derive_Viewing_Key (
      Master_Seed    : Byte_Array;
      View_Key       : out Viewing_Key
   ) is
      Label : constant Byte_Array := (
         16#56#, 16#49#, 16#45#, 16#57#, 16#2D#, 16#4B#, 16#45#, 16#59#
      );  -- "VIEW-KEY"
   begin
      SHAKE256_KDF (Master_Seed, Label, View_Key);
   end Derive_Viewing_Key;

   procedure Derive_Commit_Seed (
      Master_Seed    : Byte_Array;
      Seed           : out Commitment_Seed
   ) is
      Label : constant Byte_Array := (
         16#43#, 16#4F#, 16#4D#, 16#4D#, 16#49#, 16#54#  -- "COMMIT"
      );
   begin
      SHAKE256_KDF (Master_Seed, Label, Seed);
   end Derive_Commit_Seed;

   ---------------------------------------------------------------------------
   --  Commitment Operations
   ---------------------------------------------------------------------------

   --  Maximum commitment buffer size (randomness + max plaintext)
   Commit_Max_Size : constant := Max_Entry_Size + 64;

   procedure Create_Commitment (
      Plaintext      : Byte_Array;
      Randomness     : Byte_Array;
      Commitment     : out Entry_Commitment
   ) is
      Combined_Len : constant Natural := Plaintext'Length + Randomness'Length;
      Combined : Byte_Array (0 .. Commit_Max_Size - 1) := (others => 0);
      Hash     : Byte_Array (0 .. 63);
   begin
      --  Ajtai-style commitment: C = H(r || m)
      --  Copy randomness
      for I in 0 .. Randomness'Length - 1 loop
         Combined (I) := Randomness (Randomness'First + I);
         pragma Loop_Invariant (I < Randomness'Length);
      end loop;

      --  Copy plaintext after randomness
      --  Note: Randomness'Length = 32, Plaintext'Length <= Max_Entry_Size = 4096
      --  So max index is 32 + 4095 = 4127 < Commit_Max_Size = 4160
      for I in 0 .. Plaintext'Length - 1 loop
         pragma Loop_Invariant (I < Plaintext'Length);
         pragma Loop_Invariant (Plaintext'Length <= Max_Entry_Size);
         pragma Loop_Invariant (Randomness'Length = 32);
         pragma Loop_Invariant (32 + I < Commit_Max_Size);
         Combined (Randomness'Length + I) := Plaintext (Plaintext'First + I);
      end loop;

      --  Use SHAKE256 for 512-bit output (hash only actual data)
      Anubis_SHA3.SHAKE256 (Combined (0 .. Combined_Len - 1), Hash, Hash'Length);
      Commitment.Value := Hash;
   end Create_Commitment;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Entry (Priv_Entry : in out Private_Entry) is
   begin
      Priv_Entry.Ciphertext := (others => 0);
      Priv_Entry.CT_Length := 0;
      Priv_Entry.Encapsulated := (others => 0);
      Priv_Entry.Nonce := (others => 0);
      Priv_Entry.Tag := (others => 0);
   end Zeroize_Entry;

   procedure Zeroize_Bundle (Bundle : in out User_Key_Bundle) is
   begin
      Bundle.KEM_Public := (others => 0);
      Bundle.DSA_Public := (others => 0);
      Bundle.View_Key := (others => 0);
      Bundle.Commit_Random := (others => 0);
   end Zeroize_Bundle;

end Anubis_Shield;
