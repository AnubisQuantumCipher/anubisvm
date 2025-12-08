pragma SPARK_Mode (On);

with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_Types; use Anubis_Types;

--  Anubis_MLKEM: ML-KEM-1024 Key Encapsulation Mechanism
--
--  Implements the ML-KEM-1024 (NIST FIPS 203) key encapsulation mechanism
--  providing NIST Security Level 5 (256-bit classical security).
--
--  Operations:
--  - KeyGen: Generate encapsulation/decapsulation keypair
--  - Encaps: Encapsulate shared secret with public key
--  - Decaps: Decapsulate ciphertext with secret key
--
--  Security Properties:
--  - IND-CCA2 security against classical and quantum adversaries
--  - Constant-time implementation (no secret-dependent branches)
--  - Implicit rejection on decapsulation failure
--
--  Key Sizes (ML-KEM-1024):
--  - Encapsulation key (public): 1,568 bytes
--  - Decapsulation key (private): 3,168 bytes
--  - Ciphertext: 1,568 bytes
--  - Shared secret: 32 bytes
--
--  References:
--  - NIST FIPS 203 (ML-KEM)
--  - https://csrc.nist.gov/pubs/fips/203/final

package Anubis_MLKEM with
   SPARK_Mode => On
is

   --  ML-KEM.KeyGen: Generate encapsulation/decapsulation keypair
   --
   --  Generates a fresh ML-KEM-1024 keypair using provided randomness.
   --
   --  Input:
   --  - Random_D: 32 bytes of randomness for key generation
   --  - Random_Z: 32 bytes of randomness for implicit rejection
   --
   --  Output:
   --  - EK: Encapsulation key (public key), 1568 bytes
   --  - DK: Decapsulation key (private key), 3168 bytes
   procedure KeyGen (
      Random_D : in  Seed;
      Random_Z : in  Seed;
      EK       : out Encapsulation_Key;
      DK       : out Decapsulation_Key
   ) with
      Global => null,
      Relaxed_Initialization => (EK, DK),
      Post => EK'Initialized and DK'Initialized;

   --  ML-KEM.Encaps: Encapsulate shared secret
   --
   --  Generates a shared secret and encapsulates it using the public key.
   --
   --  Input:
   --  - EK: Encapsulation key (public key)
   --  - Random_M: 32 bytes of randomness for encapsulation
   --
   --  Output:
   --  - SS: Shared secret, 32 bytes
   --  - CT: Ciphertext, 1568 bytes
   procedure Encaps (
      EK       : in  Encapsulation_Key;
      Random_M : in  Seed;
      SS       : out Shared_Secret;
      CT       : out MLKEM_Ciphertext
   ) with
      Global => null;

   --  ML-KEM.Decaps: Decapsulate ciphertext
   --
   --  Recovers the shared secret from the ciphertext using the private key.
   --  Uses implicit rejection: returns pseudorandom value on failure.
   --
   --  Input:
   --  - DK: Decapsulation key (private key)
   --  - CT: Ciphertext
   --
   --  Output:
   --  - SS: Shared secret, 32 bytes
   --       (real secret on success, pseudorandom on failure)
   procedure Decaps (
      DK : in  Decapsulation_Key;
      CT : in  MLKEM_Ciphertext;
      SS : out Shared_Secret
   ) with
      Global => null;

   --  K-PKE.Encrypt: Internal encryption (deterministic)
   --
   --  Encrypts a message using K-PKE (used internally by Encaps).
   --
   --  Input:
   --  - EK: Encapsulation key
   --  - Msg: 32-byte message to encrypt
   --  - Coins: 32-byte randomness for encryption
   --
   --  Output:
   --  - CT: Ciphertext
   procedure K_PKE_Encrypt (
      EK    : in  Encapsulation_Key;
      Msg   : in  Message;
      Coins : in  Seed;
      CT    : out MLKEM_Ciphertext
   ) with
      Global => null,
      Relaxed_Initialization => CT,
      Post => CT'Initialized;

   --  K-PKE.Decrypt: Internal decryption
   --
   --  Decrypts a ciphertext using K-PKE (used internally by Decaps).
   --
   --  Input:
   --  - DK_PKE: K-PKE decryption key (first 1536 bytes of DK)
   --  - CT: Ciphertext
   --
   --  Output:
   --  - Msg: Decrypted 32-byte message
   procedure K_PKE_Decrypt (
      DK_PKE : in  Byte_Array;
      CT     : in  MLKEM_Ciphertext;
      Msg    : out Message
   ) with
      Global => null,
      Pre => DK_PKE'Length = K * Encoded_Poly_Bytes;  -- 1536 bytes

end Anubis_MLKEM;
