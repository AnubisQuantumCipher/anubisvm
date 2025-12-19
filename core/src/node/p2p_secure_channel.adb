-------------------------------------------------------------------------------
--  P2P_Secure_Channel: Post-Quantum Secure Communication Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  Implementation uses non-SPARK crypto operations

with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_SHA3;
with Anubis_Types;
with Ada.Text_IO;

package body P2P_Secure_Channel is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Derive channel key from shared secret using SHAKE256
   procedure Derive_Channel_Key (
      Secret        : in  Shared_Secret;
      Transcript    : in  Byte_Array;
      Channel_Key   : out AEAD_Key
   ) is
      Input_Len : constant Natural := Secret'Length + Transcript'Length;
      Input : Anubis_Types.Byte_Array (0 .. Input_Len - 1);
      Output : Anubis_Types.Byte_Array (0 .. 31);
   begin
      --  Concatenate shared secret and transcript
      for I in Secret'Range loop
         Input (I) := Anubis_Types.Byte (Secret (I));
      end loop;
      if Transcript'Length > 0 then
         for I in Transcript'Range loop
            Input (32 + I - Transcript'First) := Anubis_Types.Byte (Transcript (I));
         end loop;
      end if;

      --  Derive key with SHAKE256
      Anubis_SHA3.SHAKE256 (Input, Output, 32);

      --  Copy to channel key
      for I in Channel_Key'Range loop
         Channel_Key (I) := Aegis_VM_Types.Byte (Output (I));
      end loop;
   end Derive_Channel_Key;

   --  Zeroize byte array
   procedure Zeroize (Data : in out Byte_Array) is
   begin
      for I in Data'Range loop
         Data (I) := 0;
      end loop;
   end Zeroize;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize_Channel (
      Chan       : out Secure_Channel;
      Node_ID    : in  Hash256;
      Direction  : in  Channel_Direction
   ) is
   begin
      Chan.State := Channel_Uninitialized;
      Chan.Direction := Direction;
      Chan.Channel_Key := (others => 0);
      Chan.Send_Nonce := 0;
      Chan.Recv_Nonce := 0;
      Chan.Peer_Node_ID := Node_ID;
      Chan.Is_Encrypted := False;
   end Initialize_Channel;

   ---------------------------------------------------------------------------
   --  Handshake Operations
   ---------------------------------------------------------------------------

   procedure Create_Handshake_Init (
      Chan       : in out Secure_Channel;
      My_SK      : in     Anubis_MLDSA_Types.Secret_Key;
      Random_D   : in     Anubis_MLKEM_Types.Seed;
      Random_Z   : in     Anubis_MLKEM_Types.Seed;
      Msg        : out    Handshake_Init_Msg;
      DK_Eph     : out    Decapsulation_Key;
      Result     : out    Channel_Result
   ) is
      EK_Eph : Encapsulation_Key;
      To_Sign : Anubis_Types.Byte_Array (0 .. Encapsulation_Key_Bytes - 1);
      Signature : Anubis_MLDSA_Types.Signature;
      Sign_Success : Boolean;
      Sign_Random : constant Anubis_MLDSA_Types.Seed := (others => 0);
   begin
      Result := Channel_Invalid_State;

      --  Generate ephemeral ML-KEM keypair
      begin
         Anubis_MLKEM.KeyGen (Random_D, Random_Z, EK_Eph, DK_Eph);
      exception
         when others =>
            Result := Channel_Handshake_Failed;
            return;
      end;

      --  Sign the ephemeral public key
      for I in EK_Eph'Range loop
         To_Sign (I) := Anubis_Types.Byte (EK_Eph (I));
      end loop;

      Anubis_MLDSA.Sign (
         SK      => My_SK,
         Msg     => To_Sign,
         Random  => Sign_Random,
         Sig     => Signature,
         Success => Sign_Success
      );

      if not Sign_Success then
         Anubis_MLKEM.Zeroize_DK (DK_Eph);
         Result := Channel_Signature_Invalid;
         return;
      end if;

      --  Store in handshake message
      Msg.EK_Ephemeral := EK_Eph;
      Msg.Signature := Signature;

      --  Update state
      Chan.State := Channel_Handshaking;
      Result := Channel_OK;

      --  DK_Eph is returned to caller who MUST:
      --  1. Store it securely until Process_Handshake_Resp completes
      --  2. Pass it to Process_Handshake_Resp to derive the shared secret
      --  3. Call Anubis_MLKEM.Zeroize_DK(DK_Eph) after handshake completion
   end Create_Handshake_Init;

   procedure Process_Handshake_Init (
      Chan       : in out Secure_Channel;
      Msg        : in     Handshake_Init_Msg;
      Peer_PK    : in     Anubis_MLDSA_Types.Public_Key;
      My_SK      : in     Anubis_MLDSA_Types.Secret_Key;
      Random_M   : in     Anubis_MLKEM_Types.Seed;
      Resp       : out    Handshake_Resp_Msg;
      Result     : out    Channel_Result
   ) is
      To_Verify : Anubis_Types.Byte_Array (0 .. Encapsulation_Key_Bytes - 1);
      Verify_Success : Boolean;
      SS : Shared_Secret;
      CT : MLKEM_Ciphertext;
      To_Sign : Anubis_Types.Byte_Array (0 .. Anubis_MLKEM_Types.Ciphertext_Bytes - 1);
      Signature : Anubis_MLDSA_Types.Signature;
      Sign_Success : Boolean;
      Transcript : Byte_Array (0 .. Encapsulation_Key_Bytes + Anubis_MLKEM_Types.Ciphertext_Bytes - 1);
      Sign_Random : constant Anubis_MLDSA_Types.Seed := (others => 0);
   begin
      Result := Channel_Invalid_State;

      --  Verify peer's signature on ephemeral key
      for I in Msg.EK_Ephemeral'Range loop
         To_Verify (I) := Anubis_Types.Byte (Msg.EK_Ephemeral (I));
      end loop;

      Verify_Success := Anubis_MLDSA.Verify (
         PK  => Peer_PK,
         Msg => To_Verify,
         Sig => Msg.Signature
      );

      if not Verify_Success then
         Chan.State := Channel_Error;
         Result := Channel_Signature_Invalid;
         return;
      end if;

      --  Encapsulate shared secret
      begin
         Anubis_MLKEM.Encaps (
            EK       => Msg.EK_Ephemeral,
            Random_M => Random_M,
            SS       => SS,
            CT       => CT
         );
      exception
         when others =>
            Chan.State := Channel_Error;
            Result := Channel_Handshake_Failed;
            return;
      end;

      --  Sign the ciphertext
      for I in CT'Range loop
         To_Sign (I) := Anubis_Types.Byte (CT (I));
      end loop;

      Anubis_MLDSA.Sign (
         SK      => My_SK,
         Msg     => To_Sign,
         Random  => Sign_Random,
         Sig     => Signature,
         Success => Sign_Success
      );

      if not Sign_Success then
         Anubis_MLKEM.Zeroize_SS (SS);
         Chan.State := Channel_Error;
         Result := Channel_Signature_Invalid;
         return;
      end if;

      --  Build transcript: EK || CT
      for I in Msg.EK_Ephemeral'Range loop
         Transcript (I) := Aegis_VM_Types.Byte (Msg.EK_Ephemeral (I));
      end loop;
      for I in CT'Range loop
         Transcript (Encapsulation_Key_Bytes + I) := Aegis_VM_Types.Byte (CT (I));
      end loop;

      --  Derive channel key
      Derive_Channel_Key (SS, Transcript, Chan.Channel_Key);

      --  Zeroize shared secret
      Anubis_MLKEM.Zeroize_SS (SS);
      Zeroize (Transcript);

      --  Store in response
      Resp.Ciphertext := CT;
      Resp.Signature := Signature;

      --  Update state
      Chan.State := Channel_Established;
      Chan.Is_Encrypted := True;
      Result := Channel_OK;

      Ada.Text_IO.Put_Line ("P2P_Secure_Channel: Handshake completed (responder)");
   end Process_Handshake_Init;

   procedure Process_Handshake_Resp (
      Chan       : in out Secure_Channel;
      Resp       : in     Handshake_Resp_Msg;
      Peer_PK    : in     Anubis_MLDSA_Types.Public_Key;
      DK_Eph     : in     Decapsulation_Key;
      Result     : out    Channel_Result
   ) is
      To_Verify : Anubis_Types.Byte_Array (0 .. Anubis_MLKEM_Types.Ciphertext_Bytes - 1);
      Verify_Success : Boolean;
      SS : Shared_Secret;
      Transcript : Byte_Array (0 .. Anubis_MLKEM_Types.Ciphertext_Bytes - 1);
   begin
      Result := Channel_Invalid_State;

      --  Verify peer's signature on ciphertext
      for I in Resp.Ciphertext'Range loop
         To_Verify (I) := Anubis_Types.Byte (Resp.Ciphertext (I));
      end loop;

      Verify_Success := Anubis_MLDSA.Verify (
         PK  => Peer_PK,
         Msg => To_Verify,
         Sig => Resp.Signature
      );

      if not Verify_Success then
         Chan.State := Channel_Error;
         Result := Channel_Signature_Invalid;
         return;
      end if;

      --  Decapsulate shared secret
      begin
         Anubis_MLKEM.Decaps (
            DK => DK_Eph,
            CT => Resp.Ciphertext,
            SS => SS
         );
      exception
         when others =>
            Chan.State := Channel_Error;
            Result := Channel_Handshake_Failed;
            return;
      end;

      --  Build transcript (simplified: just CT for now)
      for I in Resp.Ciphertext'Range loop
         Transcript (I) := Aegis_VM_Types.Byte (Resp.Ciphertext (I));
      end loop;

      --  Derive channel key
      Derive_Channel_Key (SS, Transcript, Chan.Channel_Key);

      --  Zeroize shared secret
      Anubis_MLKEM.Zeroize_SS (SS);
      Zeroize (Transcript);

      --  Update state
      Chan.State := Channel_Established;
      Chan.Is_Encrypted := True;
      Result := Channel_OK;

      Ada.Text_IO.Put_Line ("P2P_Secure_Channel: Handshake completed (initiator)");
   end Process_Handshake_Resp;

   ---------------------------------------------------------------------------
   --  Encrypted Communication
   ---------------------------------------------------------------------------

   procedure Encrypt_Message (
      Chan       : in out Secure_Channel;
      Plaintext  : in     Byte_Array;
      Plain_Len  : in     Natural;
      Ciphertext : out    Byte_Array;
      Cipher_Len : out    Natural;
      Tag        : out    AEAD_Tag;
      Result     : out    Channel_Result
   ) is
      Nonce : AEAD_Nonce;
      AAD : Anubis_Types.Byte_Array (0 .. -1);  --  Empty AAD
      CT_Buf : Anubis_Types.Byte_Array (0 .. Plain_Len - 1);
      PT_Buf : Anubis_Types.Byte_Array (0 .. Plain_Len - 1);
      Key_Conv : AEAD_Key;
   begin
      Cipher_Len := 0;
      Result := Channel_Invalid_State;

      --  Check state
      if not Is_Established (Chan) then
         Result := Channel_Not_Established;
         return;
      end if;

      --  Check nonce exhaustion
      if Nonce_Exhausted (Chan) then
         Result := Channel_Nonce_Exhausted;
         return;
      end if;

      --  Convert nonce counter to bytes
      Nonce_To_Bytes (Chan.Send_Nonce, Nonce);

      --  Copy plaintext to properly indexed buffer with type conversion
      for I in 0 .. Plain_Len - 1 loop
         PT_Buf (I) := Anubis_Types.Byte (Plaintext (Plaintext'First + I));
      end loop;

      --  Copy channel key
      for I in Chan.Channel_Key'Range loop
         Key_Conv (I) := Chan.Channel_Key (I);
      end loop;

      --  Encrypt with ChaCha20-Poly1305
      begin
         Anubis_ChaCha20_Poly1305.AEAD_Encrypt (
            Ciphertext => CT_Buf,
            Tag        => Tag,
            Plaintext  => PT_Buf,
            AAD        => AAD,
            Nonce      => Nonce,
            Key        => Key_Conv
         );
      exception
         when others =>
            Result := Channel_Encryption_Failed;
            return;
      end;

      --  Copy to output with type conversion
      for I in 0 .. Plain_Len - 1 loop
         Ciphertext (Ciphertext'First + I) := Aegis_VM_Types.Byte (CT_Buf (I));
      end loop;
      Cipher_Len := Plain_Len;

      --  Increment nonce
      Chan.Send_Nonce := Chan.Send_Nonce + 1;
      Result := Channel_OK;
   end Encrypt_Message;

   procedure Decrypt_Message (
      Chan       : in out Secure_Channel;
      Ciphertext : in     Byte_Array;
      Cipher_Len : in     Natural;
      Tag        : in     AEAD_Tag;
      Plaintext  : out    Byte_Array;
      Plain_Len  : out    Natural;
      Result     : out    Channel_Result
   ) is
      Nonce : AEAD_Nonce;
      AAD : Anubis_Types.Byte_Array (0 .. -1);  --  Empty AAD
      PT_Buf : Anubis_Types.Byte_Array (0 .. Cipher_Len - 1);
      CT_Buf : Anubis_Types.Byte_Array (0 .. Cipher_Len - 1);
      Key_Conv : AEAD_Key;
      Success : Boolean;
   begin
      Plain_Len := 0;
      Result := Channel_Invalid_State;

      --  Check state
      if not Is_Established (Chan) then
         Result := Channel_Not_Established;
         return;
      end if;

      --  Convert nonce counter to bytes
      Nonce_To_Bytes (Chan.Recv_Nonce, Nonce);

      --  Copy ciphertext to properly indexed buffer with type conversion
      for I in 0 .. Cipher_Len - 1 loop
         CT_Buf (I) := Anubis_Types.Byte (Ciphertext (Ciphertext'First + I));
      end loop;

      --  Copy channel key
      for I in Chan.Channel_Key'Range loop
         Key_Conv (I) := Chan.Channel_Key (I);
      end loop;

      --  Decrypt with ChaCha20-Poly1305
      begin
         Anubis_ChaCha20_Poly1305.AEAD_Decrypt (
            Plaintext  => PT_Buf,
            Success    => Success,
            Ciphertext => CT_Buf,
            Tag        => Tag,
            AAD        => AAD,
            Nonce      => Nonce,
            Key        => Key_Conv
         );
      exception
         when others =>
            Result := Channel_Decryption_Failed;
            return;
      end;

      if not Success then
         Result := Channel_Decryption_Failed;
         return;
      end if;

      --  Copy to output with type conversion
      for I in 0 .. Cipher_Len - 1 loop
         Plaintext (Plaintext'First + I) := Aegis_VM_Types.Byte (PT_Buf (I));
      end loop;
      Plain_Len := Cipher_Len;

      --  Increment nonce
      Chan.Recv_Nonce := Chan.Recv_Nonce + 1;
      Result := Channel_OK;
   end Decrypt_Message;

   ---------------------------------------------------------------------------
   --  Channel Management
   ---------------------------------------------------------------------------

   procedure Close_Channel (
      Chan : in out Secure_Channel
   ) is
   begin
      --  Zeroize channel key
      Anubis_ChaCha20_Poly1305.Sanitize (Chan.Channel_Key);

      --  Reset state
      Chan.State := Channel_Closed;
      Chan.Is_Encrypted := False;
      Chan.Send_Nonce := 0;
      Chan.Recv_Nonce := 0;
   end Close_Channel;

   ---------------------------------------------------------------------------
   --  Nonce Management
   ---------------------------------------------------------------------------

   procedure Nonce_To_Bytes (
      Nonce_Counter : in  Unsigned_64;
      Nonce_Bytes   : out AEAD_Nonce
   ) is
      Counter : Unsigned_64 := Nonce_Counter;
   begin
      --  Convert 64-bit counter to 96-bit nonce (little-endian)
      --  First 8 bytes: counter, last 4 bytes: zero
      for I in 0 .. 7 loop
         Nonce_Bytes (I) := Byte (Counter and 16#FF#);
         Counter := Shift_Right (Counter, 8);
      end loop;
      --  Pad with zeros
      for I in 8 .. 11 loop
         Nonce_Bytes (I) := 0;
      end loop;
   end Nonce_To_Bytes;

end P2P_Secure_Channel;
