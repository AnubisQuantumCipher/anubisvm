-------------------------------------------------------------------------------
--  P2P_Secure_Channel: Post-Quantum Secure Communication Channels
--
--  This package implements secure P2P channels using:
--  - ML-KEM-1024 for post-quantum key encapsulation
--  - ChaCha20-Poly1305 for authenticated encryption
--  - Forward secrecy with ephemeral keys
--  - Mutual authentication via ML-DSA-87 signatures
--
--  Protocol Flow:
--  1. Initiator sends: EK_eph || ML-DSA signature
--  2. Responder generates: (SS, CT) = ML-KEM.Encaps(EK_eph)
--  3. Responder sends: CT || ML-DSA signature
--  4. Initiator computes: SS = ML-KEM.Decaps(DK_eph, CT)
--  5. Both derive: Channel_Key = SHAKE256(SS || Transcript)
--  6. All messages encrypted with ChaCha20-Poly1305(Channel_Key)
--
--  Security Properties:
--  - Post-quantum secure key exchange
--  - Forward secrecy (ephemeral ML-KEM keys)
--  - Mutual authentication (ML-DSA-87 signatures)
--  - Confidentiality (ChaCha20-Poly1305 AEAD)
--  - Integrity (Poly1305 MAC)
--
--  SPARK Verification Level: Gold
--  ==============================
--  - NRTE proven for all public operations
--  - Key properties proven: channel establishment, encryption correctness
--  - Resource bounds proven (no heap allocation)
--  - Zeroization postconditions proven
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_MLKEM; use Anubis_MLKEM;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_MLDSA_Types;
with Anubis_MLDSA_Config;
with Anubis_ChaCha20_Poly1305; use Anubis_ChaCha20_Poly1305;

package P2P_Secure_Channel with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Protocol version
   Protocol_Version : constant := 1;

   --  Handshake message sizes
   Handshake_Init_Size : constant :=
      Encapsulation_Key_Bytes + Anubis_MLDSA_Config.Signature_Bytes;
   Handshake_Resp_Size : constant :=
      Anubis_MLKEM_Types.Ciphertext_Bytes + Anubis_MLDSA_Config.Signature_Bytes;

   --  Maximum encrypted message size (1 MB)
   Max_Plaintext_Size : constant := 1024 * 1024;
   Max_Ciphertext_Size : constant := Max_Plaintext_Size + Tag_Size;

   --  Nonce management
   Max_Nonce_Value : constant := 2**48 - 1;  --  48-bit nonce counter

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Channel state
   type Channel_State is (
      Channel_Uninitialized,
      Channel_Handshaking,
      Channel_Established,
      Channel_Error,
      Channel_Closed
   );

   --  Channel direction
   type Channel_Direction is (
      Direction_Initiator,    --  We initiated the connection
      Direction_Responder     --  We accepted the connection
   );

   --  Handshake message types
   type Handshake_Init_Msg is record
      EK_Ephemeral : Encapsulation_Key;
      Signature    : Anubis_MLDSA_Types.Signature;
   end record;

   type Handshake_Resp_Msg is record
      Ciphertext   : MLKEM_Ciphertext;
      Signature    : Anubis_MLDSA_Types.Signature;
   end record;

   --  Secure channel context
   type Secure_Channel is record
      State         : Channel_State;
      Direction     : Channel_Direction;
      Channel_Key   : AEAD_Key;
      Send_Nonce    : Unsigned_64;
      Recv_Nonce    : Unsigned_64;
      Peer_Node_ID  : Hash256;
      Is_Encrypted  : Boolean;
   end record;

   --  Result codes
   type Channel_Result is (
      Channel_OK,
      Channel_Invalid_State,
      Channel_Handshake_Failed,
      Channel_Encryption_Failed,
      Channel_Decryption_Failed,
      Channel_Signature_Invalid,
      Channel_Nonce_Exhausted,
      Channel_Message_Too_Large,
      Channel_Not_Established
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize a new secure channel (initiator side)
   procedure Initialize_Channel (
      Chan       : out Secure_Channel;
      Node_ID    : in  Hash256;
      Direction  : in  Channel_Direction
   ) with
      Global => null,
      Post   => Chan.State = Channel_Uninitialized and then
                Chan.Direction = Direction and then
                Chan.Send_Nonce = 0 and then
                Chan.Recv_Nonce = 0 and then
                not Chan.Is_Encrypted;

   ---------------------------------------------------------------------------
   --  Handshake Operations
   ---------------------------------------------------------------------------

   --  Create handshake initiation message (initiator)
   --  IMPORTANT: Caller must store DK_Eph securely and provide it to
   --  Process_Handshake_Resp. DK_Eph should be zeroized after use.
   procedure Create_Handshake_Init (
      Chan       : in out Secure_Channel;
      My_SK      : in     Anubis_MLDSA_Types.Secret_Key;
      Random_D   : in     Anubis_MLKEM_Types.Seed;
      Random_Z   : in     Anubis_MLKEM_Types.Seed;
      Msg        : out    Handshake_Init_Msg;
      DK_Eph     : out    Decapsulation_Key;
      Result     : out    Channel_Result
   ) with
      Global => null,
      Pre    => Chan.State = Channel_Uninitialized and then
                Chan.Direction = Direction_Initiator,
      Post   => (Result = Channel_OK and then
                   Chan.State = Channel_Handshaking)
                or else (Result /= Channel_OK and then
                   Chan.State = Chan.State'Old);

   --  Process handshake initiation and create response (responder)
   procedure Process_Handshake_Init (
      Chan       : in out Secure_Channel;
      Msg        : in     Handshake_Init_Msg;
      Peer_PK    : in     Anubis_MLDSA_Types.Public_Key;
      My_SK      : in     Anubis_MLDSA_Types.Secret_Key;
      Random_M   : in     Anubis_MLKEM_Types.Seed;
      Resp       : out    Handshake_Resp_Msg;
      Result     : out    Channel_Result
   ) with
      Global => null,
      Pre    => Chan.State = Channel_Uninitialized and then
                Chan.Direction = Direction_Responder,
      Post   => (Result = Channel_OK and then
                   Chan.State = Channel_Established and then
                   Chan.Is_Encrypted)
                or else (Result /= Channel_OK and then
                   Chan.State = Channel_Error);

   --  Process handshake response and establish channel (initiator)
   procedure Process_Handshake_Resp (
      Chan       : in out Secure_Channel;
      Resp       : in     Handshake_Resp_Msg;
      Peer_PK    : in     Anubis_MLDSA_Types.Public_Key;
      DK_Eph     : in     Decapsulation_Key;
      Result     : out    Channel_Result
   ) with
      Global => null,
      Pre    => Chan.State = Channel_Handshaking and then
                Chan.Direction = Direction_Initiator,
      Post   => (Result = Channel_OK and then
                   Chan.State = Channel_Established and then
                   Chan.Is_Encrypted)
                or else (Result /= Channel_OK and then
                   Chan.State = Channel_Error);

   ---------------------------------------------------------------------------
   --  Encrypted Communication
   ---------------------------------------------------------------------------

   --  Encrypt and authenticate message
   procedure Encrypt_Message (
      Chan       : in out Secure_Channel;
      Plaintext  : in     Byte_Array;
      Plain_Len  : in     Natural;
      Ciphertext : out    Byte_Array;
      Cipher_Len : out    Natural;
      Tag        : out    AEAD_Tag;
      Result     : out    Channel_Result
   ) with
      Global => null,
      Pre    => Chan.State = Channel_Established and then
                Chan.Is_Encrypted and then
                Plain_Len <= Max_Plaintext_Size and then
                Plaintext'Length >= Plain_Len and then
                Ciphertext'Length >= Plain_Len + Tag_Size,
      Post   => (Result = Channel_OK and then
                   Cipher_Len = Plain_Len and then
                   Chan.Send_Nonce = Chan.Send_Nonce'Old + 1)
                or else (Result /= Channel_OK and then
                   Cipher_Len = 0);

   --  Decrypt and verify message
   procedure Decrypt_Message (
      Chan       : in out Secure_Channel;
      Ciphertext : in     Byte_Array;
      Cipher_Len : in     Natural;
      Tag        : in     AEAD_Tag;
      Plaintext  : out    Byte_Array;
      Plain_Len  : out    Natural;
      Result     : out    Channel_Result
   ) with
      Global => null,
      Pre    => Chan.State = Channel_Established and then
                Chan.Is_Encrypted and then
                Cipher_Len <= Max_Plaintext_Size and then
                Ciphertext'Length >= Cipher_Len and then
                Plaintext'Length >= Cipher_Len,
      Post   => (Result = Channel_OK and then
                   Plain_Len = Cipher_Len and then
                   Chan.Recv_Nonce = Chan.Recv_Nonce'Old + 1)
                or else (Result /= Channel_OK and then
                   Plain_Len = 0);

   ---------------------------------------------------------------------------
   --  Channel Management
   ---------------------------------------------------------------------------

   --  Check if channel is established
   function Is_Established (Chan : Secure_Channel) return Boolean is
      (Chan.State = Channel_Established and then Chan.Is_Encrypted)
   with
      Global => null;

   --  Get channel state
   function Get_State (Chan : Secure_Channel) return Channel_State is
      (Chan.State)
   with
      Global => null;

   --  Close channel and zeroize keys
   procedure Close_Channel (
      Chan : in out Secure_Channel
   ) with
      Global => null,
      Post   => Chan.State = Channel_Closed and then
                not Chan.Is_Encrypted and then
                (for all I in Chan.Channel_Key'Range =>
                   Chan.Channel_Key (I) = 0);

   ---------------------------------------------------------------------------
   --  Nonce Management
   ---------------------------------------------------------------------------

   --  Convert nonce counter to ChaCha20 nonce (96 bits)
   procedure Nonce_To_Bytes (
      Nonce_Counter : in  Unsigned_64;
      Nonce_Bytes   : out AEAD_Nonce
   ) with
      Global => null,
      Post   => Nonce_Bytes'Length = Nonce_Size;

   --  Check if nonce is exhausted
   function Nonce_Exhausted (Chan : Secure_Channel) return Boolean is
      (Chan.Send_Nonce >= Max_Nonce_Value)
   with
      Global => null;

end P2P_Secure_Channel;
