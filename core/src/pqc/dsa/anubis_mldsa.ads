pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

--  Anubis_MLDSA: ML-DSA-87 Digital Signature Algorithm
--
--  Implementation of NIST FIPS 204 ML-DSA at security level 5.
--  Based on the Module-LWE problem.
--
--  Key sizes:
--  - Public key: 2592 bytes
--  - Secret key: 4896 bytes
--  - Signature: 4627 bytes
--
--  Security: NIST Level 5 (equivalent to AES-256)
--
--  References:
--  - NIST FIPS 204 (ML-DSA)
--  - CRYSTALS-Dilithium specification

package Anubis_MLDSA with
   SPARK_Mode => On
is

   --  Generate a new ML-DSA-87 key pair
   --
   --  Parameters:
   --  - Seed: 32-byte random seed
   --  - PK: Output public key (2592 bytes)
   --  - SK: Output secret key (4896 bytes)
   procedure KeyGen (
      Random_Seed : in  Seed;
      PK          : out Public_Key;
      SK          : out Secret_Key
   ) with
      Global => null;

   --  Maximum message length to prevent overflow
   --  Tr_Bytes = 64, so max message is about 2^30 bytes
   Max_Msg_Length : constant := 2**30;

   --  Sign a message
   --
   --  Parameters:
   --  - SK: Secret key
   --  - Msg: Message to sign (bounded length to prevent overflow)
   --  - Random: 32 bytes of randomness (can be all zeros for deterministic)
   --  - Sig: Output signature (4627 bytes)
   --  - Success: True if signing succeeded (can fail if too many rejections)
   procedure Sign (
      SK      : in  Secret_Key;
      Msg     : in  Byte_Array;
      Random  : in  Seed;
      Sig     : out Signature;
      Success : out Boolean
   ) with
      Global => null,
      Pre => Msg'Length <= Max_Msg_Length and Msg'First = 0;

   --  Verify a signature
   --
   --  Parameters:
   --  - PK: Public key
   --  - Msg: Original message (bounded length to prevent overflow)
   --  - Sig: Signature to verify
   --  Returns: True if signature is valid
   function Verify (
      PK  : Public_Key;
      Msg : Byte_Array;
      Sig : Signature
   ) return Boolean with
      Global => null,
      Pre => Msg'Length <= Max_Msg_Length and Msg'First = 0;

end Anubis_MLDSA;
