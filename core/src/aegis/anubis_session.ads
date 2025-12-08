pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;

--  Anubis_Session: Post-Quantum Session Key Establishment
--
--  Uses ML-KEM-1024 (FIPS 203) for quantum-resistant key encapsulation.
--  Provides session keys for:
--  - Encrypted state storage (ANUBIS SHIELD)
--  - Secure channel establishment
--  - Private transaction data
--
--  Session Establishment Flow:
--  1. Recipient generates ML-KEM keypair
--  2. Sender encapsulates using recipient"s public key
--  3. Sender derives session key from shared secret
--  4. Recipient decapsulates to recover shared secret
--  5. Both parties derive identical session keys
--
--  Key Derivation:
--  session_key = SHAKE256("anubis-session-v1" || shared_secret || context, 32)
--
--  Security Properties:
--  - NIST Level 5 (256-bit post-quantum security)
--  - IND-CCA2 secure key encapsulation
--  - Domain-separated key derivation
--
--  References:
--  - NIST FIPS 203 (ML-KEM)
--  - ANUBIS SHIELD (Privacy Layer)

package Anubis_Session with
   SPARK_Mode => On
is

   --  Session key size: 256 bits (32 bytes)
   Session_Key_Size : constant := 32;
   subtype Session_Key_Index is Natural range 0 .. Session_Key_Size - 1;
   type Session_Key is array (Session_Key_Index) of Unsigned_8;

   --  Context string for key derivation (max 64 bytes)
   Max_Context_Size : constant := 64;
   subtype Context_Size is Natural range 0 .. Max_Context_Size;
   subtype Context_Index is Natural range 0 .. Max_Context_Size - 1;
   type Context_Bytes is array (Context_Index range <>) of Unsigned_8;

   --  Nonce for AEAD (192 bits = 24 bytes, for XChaCha20-Poly1305 or AES-GCM-SIV)
   Nonce_Size : constant := 24;
   subtype Nonce_Index is Natural range 0 .. Nonce_Size - 1;
   type Session_Nonce is array (Nonce_Index) of Unsigned_8;

   --  Session establishment result
   type Session_Result is record
      Session_Key      : Anubis_Session.Session_Key;
      Encapsulated_Key : MLKEM_Ciphertext;  --  ML-KEM ciphertext (1568 bytes)
      Success          : Boolean;
   end record;

   --  Encapsulation context for sender
   type Sender_Context is record
      Recipient_PK : Encapsulation_Key;
      Session      : Session_Result;
      Established  : Boolean;
   end record;

   --  Decapsulation context for recipient
   type Recipient_Context is record
      KEM_SK      : Decapsulation_Key;
      KEM_PK      : Encapsulation_Key;
      Session_Key : Anubis_Session.Session_Key;
      Established : Boolean;
   end record;

   --  Generate a new ML-KEM-1024 keypair for session establishment
   --  Called by the recipient who will receive encrypted data
   procedure Generate_Recipient_Keys (
      Seed_D : in  Seed;  --  Random seed for key generation
      Seed_Z : in  Seed;  --  Random seed for implicit rejection
      Ctx    : out Recipient_Context
   ) with
      Global => null;

   --  Establish session as sender (encapsulation)
   --  Uses recipient"s public key to create shared secret
   --  Context is optional additional data for key derivation
   procedure Establish_Sender_Session (
      Recipient_PK : in  Encapsulation_Key;
      Rand_Seed    : in  Seed;
      Context      : in  Context_Bytes;
      Result       : out Session_Result
   ) with
      Global => null,
      Pre => Context'Length <= Max_Context_Size;

   --  Recover session as recipient (decapsulation)
   --  Uses encapsulated key and secret key to derive same session key
   procedure Recover_Recipient_Session (
      Ctx               : in out Recipient_Context;
      Encapsulated_Key  : in     MLKEM_Ciphertext;
      Context           : in     Context_Bytes;
      Session_Key       : out    Anubis_Session.Session_Key;
      Success           : out    Boolean
   ) with
      Global => null,
      Pre => Context'Length <= Max_Context_Size;

   --  Derive session key from shared secret with domain separation
   --  Key = SHAKE256("anubis-session-v1" || shared_secret || context, 32)
   procedure Derive_Session_Key (
      SS      : in  Shared_Secret;
      Context : in  Context_Bytes;
      Key     : out Session_Key
   ) with
      Global => null,
      Pre => Context'Length <= Max_Context_Size;

   --  Generate a random nonce for AEAD operations
   --  Must be unique per message under the same session key
   procedure Generate_Nonce (
      Rand  : in  Byte_Array;
      Nonce : out Session_Nonce
   ) with
      Global => null,
      Pre => Rand'Length >= Nonce_Size and Rand'First = 0;

   --  Zeroize session key (constant-time)
   procedure Zeroize_Session_Key (Key : in out Session_Key) with
      Global => null,
      Post => (for all I in Key'Range => Key (I) = 0);

   --  Zeroize recipient context (including secret key)
   procedure Zeroize_Recipient_Context (Ctx : in Out Recipient_Context) with
      Global => null;

private

   --  Domain separator for session key derivation
   Domain_Prefix : constant String := "anubis-session-v1";

end Anubis_Session;
