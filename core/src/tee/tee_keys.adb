pragma SPARK_Mode (On);

with Anubis_SHA3; use Anubis_SHA3;
with Anubis_KDF; use Anubis_KDF;
with Anubis_AEAD; use Anubis_AEAD;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;

package body TEE_Keys with
   SPARK_Mode => On
is

   --  Domain separators for key derivation
   Domain_Storage  : constant String := "tee-storage";
   Domain_CVM      : constant String := "tee-cvm";
   Domain_Session  : constant String := "tee-session";
   Domain_Attest   : constant String := "tee-attest";
   Domain_KEM      : constant String := "tee-kem";

   --  Initialize TEE with fresh keys
   procedure Initialize_Keys (
      Entropy : Byte_Array;
      Bundle  : out TEE_Key_Bundle;
      Success : out Boolean
   ) is
      Salt : KDF_Salt := Zero_Salt;
      Attest_Seed : Byte_Array (0 .. 31);
      KEM_Seed : Byte_Array (0 .. 63);
   begin
      Bundle := Empty_Bundle;
      Success := False;

      --  Extract MSK from entropy using SHAKE256
      declare
         MSK_Input : constant Byte_Array := Entropy;
         MSK_Out : Byte_Array (0 .. 31);
      begin
         SHAKE256 (MSK_Input, MSK_Out, 32);
         Bundle.MSK := MSK_Out;
      end;

      --  Derive storage key
      Derive_Storage_Key (Bundle.MSK, Bundle.Storage);

      --  Derive attestation key seed
      Derive_Key (
         Master  => Bundle.MSK,
         Salt    => Salt,
         Info    => Entropy (32 .. 47),  -- Use part of entropy as info
         Context => Domain_Attest,
         Key     => Attest_Seed
      );

      --  Generate attestation keys
      declare
         Gen_Success : Boolean;
      begin
         Generate_Attestation_Keys (
            Attest_Seed, Bundle.Attest_PK, Bundle.Attest_SK, Gen_Success
         );
         if not Gen_Success then
            Zeroize_Bundle (Bundle);
            return;
         end if;
      end;

      --  Derive KEM key seed
      declare
         KEM_Seed_1 : Byte_Array (0 .. 31);
         KEM_Seed_2 : Byte_Array (0 .. 31);
      begin
         Derive_Key (
            Master  => Bundle.MSK,
            Salt    => Salt,
            Info    => Entropy (48 .. 63),
            Context => Domain_KEM,
            Key     => KEM_Seed_1
         );
         SHAKE256 (Entropy, KEM_Seed_2, 32);
         KEM_Seed (0 .. 31) := KEM_Seed_1;
         KEM_Seed (32 .. 63) := KEM_Seed_2;
      end;

      --  Generate KEM keys
      declare
         Gen_Success : Boolean;
      begin
         Generate_KEM_Keys (KEM_Seed, Bundle.KEM_EK, Bundle.KEM_DK, Gen_Success);
         if not Gen_Success then
            Zeroize_Bundle (Bundle);
            return;
         end if;
      end;

      Bundle.Initialized := True;
      Success := True;
   end Initialize_Keys;

   --  Derive all keys from MSK
   procedure Derive_All_Keys (
      MSK    : Master_Seal_Key;
      Bundle : out TEE_Key_Bundle
   ) is
   begin
      Bundle := Empty_Bundle;
      Bundle.MSK := MSK;

      --  Derive storage key
      Derive_Storage_Key (MSK, Bundle.Storage);

      --  Note: Attestation and KEM keys require seeds/randomness
      --  and should be regenerated separately if needed

      Bundle.Initialized := True;
   end Derive_All_Keys;

   --  Generate attestation keypair
   procedure Generate_Attestation_Keys (
      Seed     : Byte_Array;
      PK       : out DSA_Public_Key;
      SK       : out DSA_Secret_Key;
      Success  : out Boolean
   ) is
      DSA_Seed : Anubis_MLDSA_Types.Seed;
      DSA_PK : Anubis_MLDSA_Types.Public_Key;
      DSA_SK : Anubis_MLDSA_Types.Secret_Key;
   begin
      PK := (others => 0);
      SK := (others => 0);
      Success := False;

      --  Copy seed
      for I in DSA_Seed'Range loop
         pragma Loop_Invariant (I >= DSA_Seed'First);
         DSA_Seed (I) := Seed (Seed'First + I);
      end loop;

      --  Generate ML-DSA-87 keypair
      Anubis_MLDSA.KeyGen (DSA_Seed, DSA_PK, DSA_SK);

      --  Copy keys to output
      for I in DSA_PK'Range loop
         pragma Loop_Invariant (I >= DSA_PK'First);
         PK (I) := DSA_PK (I);
      end loop;

      for I in DSA_SK'Range loop
         pragma Loop_Invariant (I >= DSA_SK'First);
         SK (I) := DSA_SK (I);
      end loop;

      Success := True;
   end Generate_Attestation_Keys;

   --  Generate KEM keypair
   procedure Generate_KEM_Keys (
      Seed     : Byte_Array;
      EK       : out KEM_Encaps_Key;
      DK       : out KEM_Decaps_Key;
      Success  : out Boolean
   ) is
      KEM_D : Anubis_MLKEM_Types.Seed;
      KEM_Z : Anubis_MLKEM_Types.Seed;
      KEM_EK_Out : Anubis_MLKEM_Types.Encapsulation_Key;
      KEM_DK_Out : Anubis_MLKEM_Types.Decapsulation_Key;
   begin
      EK := (others => 0);
      DK := (others => 0);
      Success := False;

      --  Copy seeds
      for I in KEM_D'Range loop
         pragma Loop_Invariant (I >= KEM_D'First);
         KEM_D (I) := Seed (Seed'First + I);
      end loop;

      for I in KEM_Z'Range loop
         pragma Loop_Invariant (I >= KEM_Z'First);
         KEM_Z (I) := Seed (Seed'First + 32 + I);
      end loop;

      --  Generate ML-KEM-1024 keypair
      Anubis_MLKEM.KeyGen (KEM_D, KEM_Z, KEM_EK_Out, KEM_DK_Out);

      --  Copy keys to output
      for I in KEM_EK_Out'Range loop
         pragma Loop_Invariant (I >= KEM_EK_Out'First);
         EK (I) := KEM_EK_Out (I);
      end loop;

      for I in KEM_DK_Out'Range loop
         pragma Loop_Invariant (I >= KEM_DK_Out'First);
         DK (I) := KEM_DK_Out (I);
      end loop;

      Success := True;
   end Generate_KEM_Keys;

   --  Derive storage key
   procedure Derive_Storage_Key (
      MSK : Master_Seal_Key;
      Key : out Storage_Key
   ) is
      Salt : constant KDF_Salt := Zero_Salt;
      Info : constant Byte_Array (0 .. 0) := (others => 0);
   begin
      Derive_Key (
         Master  => MSK,
         Salt    => Salt,
         Info    => Info,
         Context => Domain_Storage,
         Key     => Key
      );
   end Derive_Storage_Key;

   --  Derive CVM key
   procedure Derive_CVM_Key (
      MSK    : Master_Seal_Key;
      CVM_ID : Byte_Array;
      Key    : out Session_Key
   ) is
      Salt : constant KDF_Salt := Zero_Salt;
   begin
      Derive_Key (
         Master  => MSK,
         Salt    => Salt,
         Info    => CVM_ID,
         Context => Domain_CVM,
         Key     => Key
      );
   end Derive_CVM_Key;

   --  Derive connection key
   procedure Derive_Connection_Key (
      MSK        : Master_Seal_Key;
      Session_ID : Byte_Array;
      Key        : out Session_Key
   ) is
      Salt : constant KDF_Salt := Zero_Salt;
   begin
      Derive_Key (
         Master  => MSK,
         Salt    => Salt,
         Info    => Session_ID,
         Context => Domain_Session,
         Key     => Key
      );
   end Derive_Connection_Key;

   --  Seal data
   procedure Seal_Data (
      Key        : Storage_Key;
      Plaintext  : Byte_Array;
      Nonce      : Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Byte_Array;
      Success    : out Boolean
   ) is
      Local_Key : Anubis_AEAD.AEAD_Key;
      Local_Nonce : Anubis_AEAD.AEAD_Nonce;
      Local_Tag : Anubis_AEAD.AEAD_Tag;
      Empty_AAD : constant Byte_Array (0 .. 0) := (others => 0);
   begin
      Ciphertext := (others => 0);
      Tag := (others => 0);
      Success := False;

      --  Copy key
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         Local_Key (I - Key'First) := Key (I);
      end loop;

      --  Copy nonce
      for I in Nonce'Range loop
         pragma Loop_Invariant (I >= Nonce'First);
         Local_Nonce (I - Nonce'First) := Nonce (I);
      end loop;

      --  Encrypt
      Anubis_AEAD.AEAD_Encrypt (
         Key        => Local_Key,
         Nonce      => Local_Nonce,
         Plaintext  => Plaintext,
         AAD        => Empty_AAD (0 .. -1),  -- Empty
         Ciphertext => Ciphertext,
         Tag        => Local_Tag
      );

      --  Copy tag
      for I in Local_Tag'Range loop
         pragma Loop_Invariant (I >= Local_Tag'First);
         Tag (Tag'First + I) := Local_Tag (I);
      end loop;

      Success := True;
   end Seal_Data;

   --  Unseal data
   procedure Unseal_Data (
      Key        : Storage_Key;
      Ciphertext : Byte_Array;
      Nonce      : Byte_Array;
      Tag        : Byte_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean
   ) is
      Local_Key : Anubis_AEAD.AEAD_Key;
      Local_Nonce : Anubis_AEAD.AEAD_Nonce;
      Local_Tag : Anubis_AEAD.AEAD_Tag;
      Empty_AAD : constant Byte_Array (0 .. 0) := (others => 0);
   begin
      Plaintext := (others => 0);
      Success := False;

      --  Copy key
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         Local_Key (I - Key'First) := Key (I);
      end loop;

      --  Copy nonce
      for I in Nonce'Range loop
         pragma Loop_Invariant (I >= Nonce'First);
         Local_Nonce (I - Nonce'First) := Nonce (I);
      end loop;

      --  Copy tag
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I >= 0);
         Local_Tag (I) := Tag (Tag'First + I);
      end loop;

      --  Decrypt
      Anubis_AEAD.AEAD_Decrypt (
         Key        => Local_Key,
         Nonce      => Local_Nonce,
         Ciphertext => Ciphertext,
         AAD        => Empty_AAD (0 .. -1),  -- Empty
         Tag        => Local_Tag,
         Plaintext  => Plaintext,
         Success    => Success
      );
   end Unseal_Data;

   --  Decapsulate
   procedure Decapsulate (
      DK : KEM_Decaps_Key;
      CT : KEM_Ciphertext;
      SS : out KEM_Shared_Secret
   ) is
      KEM_DK : Anubis_MLKEM_Types.Decapsulation_Key;
      KEM_CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      KEM_SS : Anubis_MLKEM_Types.Shared_Secret;
   begin
      SS := (others => 0);

      --  Copy inputs
      for I in KEM_DK'Range loop
         pragma Loop_Invariant (I >= KEM_DK'First);
         KEM_DK (I) := DK (I);
      end loop;

      for I in KEM_CT'Range loop
         pragma Loop_Invariant (I >= KEM_CT'First);
         KEM_CT (I) := CT (I);
      end loop;

      --  Decapsulate
      Anubis_MLKEM.Decaps (KEM_DK, KEM_CT, KEM_SS);

      --  Copy output
      for I in KEM_SS'Range loop
         pragma Loop_Invariant (I >= KEM_SS'First);
         SS (I) := KEM_SS (I);
      end loop;
   end Decapsulate;

   --  Encapsulate
   procedure Encapsulate (
      EK     : KEM_Encaps_Key;
      Random : Byte_Array;
      CT     : out KEM_Ciphertext;
      SS     : out KEM_Shared_Secret
   ) is
      KEM_EK : Anubis_MLKEM_Types.Encapsulation_Key;
      KEM_M : Anubis_MLKEM_Types.Seed;
      KEM_CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      KEM_SS : Anubis_MLKEM_Types.Shared_Secret;
   begin
      CT := (others => 0);
      SS := (others => 0);

      --  Copy inputs
      for I in KEM_EK'Range loop
         pragma Loop_Invariant (I >= KEM_EK'First);
         KEM_EK (I) := EK (I);
      end loop;

      for I in KEM_M'Range loop
         pragma Loop_Invariant (I >= KEM_M'First);
         KEM_M (I) := Random (Random'First + I);
      end loop;

      --  Encapsulate
      Anubis_MLKEM.Encaps (KEM_EK, KEM_M, KEM_SS, KEM_CT);

      --  Copy outputs
      for I in KEM_CT'Range loop
         pragma Loop_Invariant (I >= KEM_CT'First);
         CT (I) := KEM_CT (I);
      end loop;

      for I in KEM_SS'Range loop
         pragma Loop_Invariant (I >= KEM_SS'First);
         SS (I) := KEM_SS (I);
      end loop;
   end Encapsulate;

   --  Zeroize bundle
   procedure Zeroize_Bundle (Bundle : in Out TEE_Key_Bundle) is
   begin
      Zeroize_MSK (Bundle.MSK);
      Zeroize_Session (Bundle.Storage);
      Zeroize_DSA_SK (Bundle.Attest_SK);
      Zeroize_KEM_DK (Bundle.KEM_DK);

      --  Public keys don't need secure zeroization but clear anyway
      Bundle.Attest_PK := (others => 0);
      Bundle.KEM_EK := (others => 0);

      Bundle.Initialized := False;
   end Zeroize_Bundle;

   --  Zeroize MSK
   procedure Zeroize_MSK (Key : in Out Master_Seal_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_MSK;

   --  Zeroize session key
   procedure Zeroize_Session (Key : in Out Session_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_Session;

   --  Zeroize DSA secret key
   procedure Zeroize_DSA_SK (Key : in Out DSA_Secret_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_DSA_SK;

   --  Zeroize KEM decapsulation key
   procedure Zeroize_KEM_DK (Key : in Out KEM_Decaps_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_KEM_DK;

end TEE_Keys;
