pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_KMAC; use Anubis_KMAC;

package body Anubis_AEAD with
   SPARK_Mode => On
is

   --  Domain separator for AEAD authentication
   AEAD_Domain : constant String := "aead";

   --  Generate keystream using SHAKE256(key || nonce)
   --  This produces a cryptographically secure pseudorandom byte sequence
   procedure Generate_Keystream (
      Key       : AEAD_Key;
      Nonce     : AEAD_Nonce;
      Keystream : out Byte_Array
   ) is
      --  Input: key || nonce
      Input_Len : constant Natural := Key_Size + Nonce_Size;
      Input : Byte_Array (0 .. Input_Len - 1);
   begin
      --  Initialize keystream to zeros
      Keystream := (others => 0);

      --  Build input: key || nonce
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         Input (I) := Key (I);
      end loop;

      for I in Nonce'Range loop
         pragma Loop_Invariant (I >= Nonce'First);
         Input (Key_Size + I) := Nonce (I);
      end loop;

      --  Generate keystream using SHAKE256
      if Keystream'Length > 0 and then Keystream'Length <= 65535 then
         SHAKE256 (Input, Keystream, Keystream'Length);
      end if;
   end Generate_Keystream;

   --  Compute authentication tag: KMAC256(key, nonce || AAD || C || lengths, "aead")
   procedure Compute_Tag (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      AAD        : Byte_Array;
      Ciphertext : Byte_Array;
      Tag        : out AEAD_Tag
   ) is
      --  Message for KMAC: nonce || AAD || ciphertext || len(AAD) || len(C)
      --  Using 8 bytes each for lengths (LE64)
      Msg_Len : constant Natural :=
         Nonce_Size + AAD'Length + Ciphertext'Length + 16;
      Msg : Byte_Array (0 .. Msg_Len - 1) := (others => 0);
      Pos : Natural := 0;
      KMAC_Key_Copy : KMAC_Key;
   begin
      Tag := (others => 0);

      --  Copy nonce
      for I in Nonce'Range loop
         pragma Loop_Invariant (I >= Nonce'First);
         pragma Loop_Invariant (Pos <= Nonce_Size);
         if Pos <= Msg'Last then
            Msg (Pos) := Nonce (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy AAD
      for I in AAD'Range loop
         pragma Loop_Invariant (Pos <= Msg'Last);
         if Pos <= Msg'Last then
            Msg (Pos) := AAD (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy ciphertext
      for I in Ciphertext'Range loop
         pragma Loop_Invariant (Pos <= Msg'Last);
         if Pos <= Msg'Last then
            Msg (Pos) := Ciphertext (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Encode AAD length as LE64
      declare
         AAD_Len_U64 : constant Unsigned_64 := Unsigned_64 (AAD'Length);
      begin
         for I in 0 .. 7 loop
            pragma Loop_Invariant (I in 0 .. 7);
            pragma Loop_Invariant (Pos <= Msg'Last);
            if Pos <= Msg'Last then
               Msg (Pos) := Byte (Shift_Right (AAD_Len_U64, I * 8) and 16#FF#);
               Pos := Pos + 1;
            end if;
         end loop;
      end;

      --  Encode ciphertext length as LE64
      declare
         CT_Len_U64 : constant Unsigned_64 := Unsigned_64 (Ciphertext'Length);
      begin
         for I in 0 .. 7 loop
            pragma Loop_Invariant (I in 0 .. 7);
            pragma Loop_Invariant (Pos <= Msg'Last);
            if Pos <= Msg'Last then
               Msg (Pos) := Byte (Shift_Right (CT_Len_U64, I * 8) and 16#FF#);
               Pos := Pos + 1;
            end if;
         end loop;
      end;

      --  Convert AEAD_Key to KMAC_Key (same size)
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         KMAC_Key_Copy (I) := Key (I);
      end loop;

      --  Compute KMAC256 tag
      KMAC256 (KMAC_Key_Copy, Msg, AEAD_Domain, Tag);
   end Compute_Tag;

   --  Constant-time comparison to prevent timing attacks
   function Constant_Time_Equal (
      A : AEAD_Tag;
      B : AEAD_Tag
   ) return Boolean is
      Diff : Byte := 0;
   begin
      for I in A'Range loop
         pragma Loop_Invariant (I >= A'First);
         Diff := Diff or (A (I) xor B (I));
      end loop;
      return Diff = 0;
   end Constant_Time_Equal;

   --  AEAD Encryption
   procedure AEAD_Encrypt (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      Plaintext  : Byte_Array;
      AAD        : Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out AEAD_Tag
   ) is
      Keystream : Byte_Array (Plaintext'Range);
   begin
      --  Initialize outputs
      Ciphertext := (others => 0);
      Tag := (others => 0);

      --  Generate keystream
      Generate_Keystream (Key, Nonce, Keystream);

      --  XOR plaintext with keystream to get ciphertext
      for I in Plaintext'Range loop
         pragma Loop_Invariant (I >= Plaintext'First);
         declare
            KS_Index : constant Natural := I - Plaintext'First + Keystream'First;
            CT_Index : constant Natural := I - Plaintext'First + Ciphertext'First;
         begin
            if KS_Index <= Keystream'Last and then CT_Index <= Ciphertext'Last then
               Ciphertext (CT_Index) := Plaintext (I) xor Keystream (KS_Index);
            end if;
         end;
      end loop;

      --  Compute authentication tag over nonce || AAD || ciphertext
      Compute_Tag (Key, Nonce, AAD, Ciphertext, Tag);
   end AEAD_Encrypt;

   --  AEAD Decryption
   procedure AEAD_Decrypt (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      Ciphertext : Byte_Array;
      AAD        : Byte_Array;
      Tag        : AEAD_Tag;
      Plaintext  : out Byte_Array;
      Success    : out Boolean
   ) is
      Expected_Tag : AEAD_Tag;
      Keystream : Byte_Array (Ciphertext'Range);
   begin
      --  Initialize plaintext to zeros (secure default)
      Plaintext := (others => 0);

      --  Compute expected tag
      Compute_Tag (Key, Nonce, AAD, Ciphertext, Expected_Tag);

      --  Verify tag (constant-time)
      Success := Constant_Time_Equal (Tag, Expected_Tag);

      if Success then
         --  Generate keystream
         Generate_Keystream (Key, Nonce, Keystream);

         --  XOR ciphertext with keystream to get plaintext
         for I in Ciphertext'Range loop
            pragma Loop_Invariant (I >= Ciphertext'First);
            declare
               KS_Index : constant Natural := I - Ciphertext'First + Keystream'First;
               PT_Index : constant Natural := I - Ciphertext'First + Plaintext'First;
            begin
               if KS_Index <= Keystream'Last and then PT_Index <= Plaintext'Last then
                  Plaintext (PT_Index) := Ciphertext (I) xor Keystream (KS_Index);
               end if;
            end;
         end loop;
      end if;
      --  On failure, Plaintext remains zeroed (initialized above)
   end AEAD_Decrypt;

   --  Secure key zeroization
   procedure Zeroize_Key (Key : in Out AEAD_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_Key;

   --  Secure nonce zeroization
   procedure Zeroize_Nonce (Nonce : in Out AEAD_Nonce) is
   begin
      for I in Nonce'Range loop
         pragma Loop_Invariant (I >= Nonce'First);
         pragma Loop_Invariant (for all J in Nonce'First .. I - 1 => Nonce (J) = 0);
         Nonce (I) := 0;
      end loop;
   end Zeroize_Nonce;

end Anubis_AEAD;
