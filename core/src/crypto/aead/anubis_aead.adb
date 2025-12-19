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
      Input : Byte_Array (0 .. Input_Len - 1) := (others => 0);
   begin
      --  Initialize keystream to zeros
      Keystream := (others => 0);

      --  Build input: key || nonce
      --  Key'Range = 0..31, Input'Range = 0..55
      for I in Key'Range loop
         pragma Loop_Invariant (I in Key'Range);
         Input (I) := Key (I);
      end loop;

      --  Nonce'Range = 0..23, write to Input(32..55)
      for I in Nonce'Range loop
         pragma Loop_Invariant (I in Nonce'Range);
         pragma Loop_Invariant (Key_Size + I <= Input'Last);
         Input (Key_Size + I) := Nonce (I);
      end loop;

      --  Generate keystream using SHAKE256
      --  Precondition guarantees: Keystream'Length <= 65535 and Keystream'First = 0
      if Keystream'Length > 0 then
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
      KMAC_Key_Copy : KMAC_Key := (others => 0);
   begin
      Tag := (others => 0);

      --  Copy nonce (Nonce'Range = 0..23)
      for I in Nonce'Range loop
         pragma Loop_Invariant (I in Nonce'Range);
         pragma Loop_Invariant (Pos = I);
         if Pos <= Msg'Last then
            Msg (Pos) := Nonce (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy AAD (may be empty)
      if AAD'Length > 0 then
         for I in AAD'Range loop
            pragma Loop_Invariant (I in AAD'Range);
            pragma Loop_Invariant (Pos <= Msg'Length);
            if Pos <= Msg'Last then
               Msg (Pos) := AAD (I);
               Pos := Pos + 1;
            end if;
         end loop;
      end if;

      --  Copy ciphertext (may be empty)
      if Ciphertext'Length > 0 then
         for I in Ciphertext'Range loop
            pragma Loop_Invariant (I in Ciphertext'Range);
            pragma Loop_Invariant (Pos <= Msg'Length);
            if Pos <= Msg'Last then
               Msg (Pos) := Ciphertext (I);
               Pos := Pos + 1;
            end if;
         end loop;
      end if;

      --  Encode AAD length as LE64
      declare
         AAD_Len_U64 : constant Unsigned_64 := Unsigned_64 (AAD'Length);
      begin
         for I in 0 .. 7 loop
            pragma Loop_Invariant (I in 0 .. 7);
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
            if Pos <= Msg'Last then
               Msg (Pos) := Byte (Shift_Right (CT_Len_U64, I * 8) and 16#FF#);
               Pos := Pos + 1;
            end if;
         end loop;
      end;

      --  Convert AEAD_Key to KMAC_Key (same size, same range 0..31)
      for I in Key'Range loop
         pragma Loop_Invariant (I in Key'Range);
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
         pragma Loop_Invariant (I in A'Range);
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
   begin
      --  Initialize outputs
      Ciphertext := (others => 0);
      Tag := (others => 0);

      --  Handle empty plaintext
      if Plaintext'Length = 0 then
         --  Just compute tag over empty ciphertext
         Compute_Tag (Key, Nonce, AAD, Ciphertext, Tag);
         return;
      end if;

      --  Generate keystream and encrypt
      declare
         Keystream_Len : constant Natural := Plaintext'Length;
         Keystream : Byte_Array (0 .. Keystream_Len - 1) := (others => 0);
      begin
         Generate_Keystream (Key, Nonce, Keystream);

         --  XOR plaintext with keystream to get ciphertext
         for I in Plaintext'Range loop
            pragma Loop_Invariant (I in Plaintext'Range);
            declare
               Offset : constant Natural := I - Plaintext'First;
            begin
               if Offset <= Keystream'Last and then
                  Ciphertext'First + Offset <= Ciphertext'Last
               then
                  Ciphertext (Ciphertext'First + Offset) :=
                     Plaintext (I) xor Keystream (Offset);
               end if;
            end;
         end loop;
      end;

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
      Expected_Tag : AEAD_Tag := (others => 0);
   begin
      --  Initialize plaintext to zeros (secure default)
      Plaintext := (others => 0);
      Success := False;

      --  Compute expected tag
      Compute_Tag (Key, Nonce, AAD, Ciphertext, Expected_Tag);

      --  Verify tag (constant-time)
      Success := Constant_Time_Equal (Tag, Expected_Tag);

      --  Handle empty ciphertext
      if Ciphertext'Length = 0 then
         return;
      end if;

      if Success then
         --  Generate keystream and decrypt
         declare
            Keystream_Len : constant Natural := Ciphertext'Length;
            Keystream : Byte_Array (0 .. Keystream_Len - 1) := (others => 0);
         begin
            Generate_Keystream (Key, Nonce, Keystream);

            --  XOR ciphertext with keystream to get plaintext
            for I in Ciphertext'Range loop
               pragma Loop_Invariant (I in Ciphertext'Range);
               declare
                  Offset : constant Natural := I - Ciphertext'First;
               begin
                  if Offset <= Keystream'Last and then
                     Plaintext'First + Offset <= Plaintext'Last
                  then
                     Plaintext (Plaintext'First + Offset) :=
                        Ciphertext (I) xor Keystream (Offset);
                  end if;
               end;
            end loop;
         end;
      end if;
      --  On failure, Plaintext remains zeroed (initialized above)
   end AEAD_Decrypt;

   --  Secure key zeroization using volatile writes
   --  The loop with Volatile aspect prevents dead-store elimination
   procedure Zeroize_Key (Key : in Out AEAD_Key) is
      type Volatile_Byte is mod 2**8 with
         Size => 8,
         Volatile_Full_Access => True;
      type Volatile_Key_Array is array (AEAD_Key'Range) of Volatile_Byte;

      --  Create a volatile view of the key
      Volatile_View : Volatile_Key_Array with
         Address => Key'Address,
         Import;
   begin
      for I in Volatile_View'Range loop
         pragma Loop_Invariant (I >= Volatile_View'First);
         pragma Loop_Invariant (for all J in Volatile_View'First .. I - 1 =>
                                Volatile_View (J) = 0);
         Volatile_View (I) := 0;
      end loop;
   end Zeroize_Key;

   --  Secure nonce zeroization using volatile writes
   procedure Zeroize_Nonce (Nonce : in Out AEAD_Nonce) is
      type Volatile_Byte is mod 2**8 with
         Size => 8,
         Volatile_Full_Access => True;
      type Volatile_Nonce_Array is array (AEAD_Nonce'Range) of Volatile_Byte;

      --  Create a volatile view of the nonce
      Volatile_View : Volatile_Nonce_Array with
         Address => Nonce'Address,
         Import;
   begin
      for I in Volatile_View'Range loop
         pragma Loop_Invariant (I >= Volatile_View'First);
         pragma Loop_Invariant (for all J in Volatile_View'First .. I - 1 =>
                                Volatile_View (J) = 0);
         Volatile_View (I) := 0;
      end loop;
   end Zeroize_Nonce;

end Anubis_AEAD;
