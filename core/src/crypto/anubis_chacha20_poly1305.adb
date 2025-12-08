-------------------------------------------------------------------------------
--  ANUBIS ChaCha20-Poly1305 AEAD (Implementation)
--
--  Following SPARKNaCl platinum-level patterns for full proof discharge.
--  RFC 8439 compliant authenticated encryption.
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_ChaCha20_Poly1305 with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Helper: Store 64-bit little-endian
   ---------------------------------------------------------------------------

   procedure Store_LE64 (Data : in Out Byte_Array; Offset : Natural; Value : Unsigned_64)
   with
      Global => null,
      Pre    => Offset + 7 <= Data'Last - Data'First
   is
   begin
      Data (Data'First + Offset)     := Byte (Value and 16#FF#);
      Data (Data'First + Offset + 1) := Byte (Shift_Right (Value, 8) and 16#FF#);
      Data (Data'First + Offset + 2) := Byte (Shift_Right (Value, 16) and 16#FF#);
      Data (Data'First + Offset + 3) := Byte (Shift_Right (Value, 24) and 16#FF#);
      Data (Data'First + Offset + 4) := Byte (Shift_Right (Value, 32) and 16#FF#);
      Data (Data'First + Offset + 5) := Byte (Shift_Right (Value, 40) and 16#FF#);
      Data (Data'First + Offset + 6) := Byte (Shift_Right (Value, 48) and 16#FF#);
      Data (Data'First + Offset + 7) := Byte (Shift_Right (Value, 56) and 16#FF#);
   end Store_LE64;

   ---------------------------------------------------------------------------
   --  Compute Poly1305 MAC data (AAD || pad || ciphertext || pad || lengths)
   ---------------------------------------------------------------------------

   procedure Compute_Mac_Data (
      Mac_Data   :    out Byte_Array;
      AAD        : in     Byte_Array;
      Ciphertext : in     Byte_Array
   ) with
      Global => null,
      Pre    => Mac_Data'First = 0
                and then Mac_Data'Length >= AAD'Length + Ciphertext'Length + 32
   is
      Pos      : Natural := 0;
      AAD_Len  : constant Natural := AAD'Length;
      CT_Len   : constant Natural := Ciphertext'Length;
      AAD_Pad  : constant Natural := (16 - (AAD_Len mod 16)) mod 16;
      CT_Pad   : constant Natural := (16 - (CT_Len mod 16)) mod 16;
   begin
      Mac_Data := (others => 0);

      --  Copy AAD
      for I in 0 .. AAD_Len - 1 loop
         pragma Loop_Invariant (I < AAD_Len);
         pragma Loop_Invariant (Pos = I);
         Mac_Data (Pos) := AAD (AAD'First + I);
         Pos := Pos + 1;
      end loop;

      --  Pad AAD to 16-byte boundary (already zeroed)
      Pos := Pos + AAD_Pad;

      --  Copy ciphertext
      for I in 0 .. CT_Len - 1 loop
         pragma Loop_Invariant (I < CT_Len);
         pragma Loop_Invariant (Pos = AAD_Len + AAD_Pad + I);
         Mac_Data (Pos) := Ciphertext (Ciphertext'First + I);
         Pos := Pos + 1;
      end loop;

      --  Pad ciphertext to 16-byte boundary (already zeroed)
      Pos := Pos + CT_Pad;

      --  Store lengths as little-endian 64-bit values
      Store_LE64 (Mac_Data, Pos, Unsigned_64 (AAD_Len));
      Store_LE64 (Mac_Data, Pos + 8, Unsigned_64 (CT_Len));
   end Compute_Mac_Data;

   ---------------------------------------------------------------------------
   --  AEAD Encryption
   ---------------------------------------------------------------------------

   procedure AEAD_Encrypt (
      Ciphertext :    out Byte_Array;
      Tag        :    out AEAD_Tag;
      Plaintext  : in     Byte_Array;
      AAD        : in     Byte_Array;
      Nonce      : in     AEAD_Nonce;
      Key        : in     AEAD_Key
   ) is
      --  One-time Poly1305 key (first 32 bytes of ChaCha20 block 0)
      Poly_Key_Block : ChaCha20_Block;
      Poly_Key       : Poly1305_Key;

      --  MAC input length calculation
      AAD_Len   : constant Natural := AAD'Length;
      PT_Len    : constant Natural := Plaintext'Length;
      AAD_Pad   : constant Natural := (16 - (AAD_Len mod 16)) mod 16;
      CT_Pad    : constant Natural := (16 - (PT_Len mod 16)) mod 16;
      Mac_Len   : constant Natural := AAD_Len + AAD_Pad + PT_Len + CT_Pad + 16;
   begin
      Ciphertext := (others => 0);
      Tag := (others => 0);

      --  Step 1: Generate one-time Poly1305 key using ChaCha20 block 0
      Block_Function (Key, 0, Nonce, Poly_Key_Block);

      --  Extract first 32 bytes as Poly1305 key
      for I in Poly1305_Key'Range loop
         pragma Loop_Invariant (True);
         Poly_Key (I) := Poly_Key_Block (I);
      end loop;

      --  Step 2: Encrypt plaintext with ChaCha20 starting at counter 1
      if PT_Len > 0 then
         ChaCha20_XOR (Ciphertext, Plaintext, Nonce, Key, 1);
      end if;

      --  Step 3: Compute Poly1305 tag over (AAD || pad || ciphertext || pad || lengths)
      declare
         Mac_Data : Byte_Array (0 .. Mac_Len - 1);
      begin
         Compute_Mac_Data (Mac_Data, AAD, Ciphertext);
         Onetimeauth (Tag, Mac_Data, Poly_Key);
      end;

      --  Sanitize one-time key
      Anubis_ChaCha20.Sanitize_Block (Poly_Key_Block);
      Anubis_Poly1305.Sanitize (Poly_Key);
   end AEAD_Encrypt;

   ---------------------------------------------------------------------------
   --  AEAD Decryption
   ---------------------------------------------------------------------------

   procedure AEAD_Decrypt (
      Plaintext  :    out Byte_Array;
      Success    :    out Boolean;
      Ciphertext : in     Byte_Array;
      Tag        : in     AEAD_Tag;
      AAD        : in     Byte_Array;
      Nonce      : in     AEAD_Nonce;
      Key        : in     AEAD_Key
   ) is
      --  One-time Poly1305 key
      Poly_Key_Block : ChaCha20_Block;
      Poly_Key       : Poly1305_Key;

      --  MAC input length calculation
      AAD_Len   : constant Natural := AAD'Length;
      CT_Len    : constant Natural := Ciphertext'Length;
      AAD_Pad   : constant Natural := (16 - (AAD_Len mod 16)) mod 16;
      CT_Pad    : constant Natural := (16 - (CT_Len mod 16)) mod 16;
      Mac_Len   : constant Natural := AAD_Len + AAD_Pad + CT_Len + CT_Pad + 16;
   begin
      Plaintext := (others => 0);
      Success := False;

      --  Step 1: Generate one-time Poly1305 key
      Block_Function (Key, 0, Nonce, Poly_Key_Block);

      for I in Poly1305_Key'Range loop
         pragma Loop_Invariant (True);
         Poly_Key (I) := Poly_Key_Block (I);
      end loop;

      --  Step 2: Verify tag BEFORE decryption
      declare
         Mac_Data : Byte_Array (0 .. Mac_Len - 1);
      begin
         Compute_Mac_Data (Mac_Data, AAD, Ciphertext);
         Success := Onetimeauth_Verify (Tag, Mac_Data, Poly_Key);
      end;

      --  Step 3: Only decrypt if verification succeeded
      if Success and then CT_Len > 0 then
         ChaCha20_XOR (Plaintext, Ciphertext, Nonce, Key, 1);
      end if;

      --  Sanitize one-time key
      Anubis_ChaCha20.Sanitize_Block (Poly_Key_Block);
      Anubis_Poly1305.Sanitize (Poly_Key);
   end AEAD_Decrypt;

   ---------------------------------------------------------------------------
   --  Sanitization
   ---------------------------------------------------------------------------

   procedure Sanitize (Key : out AEAD_Key) is
   begin
      Anubis_ChaCha20.Sanitize (Key);
   end Sanitize;

end Anubis_ChaCha20_Poly1305;
