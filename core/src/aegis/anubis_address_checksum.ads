pragma SPARK_Mode (On);

with Anubis_Address_Types; use Anubis_Address_Types;

--  Anubis_Address_Checksum: Checksum computation per AAS-001 v3.1
--
--  SINGLE ALGORITHM: ML-DSA-87 (mldsa87)
--
--  Checksum Algorithm (AAS-001 v3.1 Section 2.3.5):
--  1. Build preimage: algorithm ":" network ":" type ":" payload_base32_no_dashes
--     Example: "mldsa87:main:u:qr7zy5kxmjgpv4wc8h2d6nft3se09ax7k5n2p8rvwd4h7mcqj9f3"
--  2. Hash: digest = SHA3-256(preimage)
--  3. Extract first 24 bits (3 bytes): checksum = digest[0..2]
--  4. Encode to Base32: 3 bytes -> 5 Base32 characters
--
--  Security Properties:
--  - 24-bit checksum provides 1 in 16.7M false positive rate
--  - Detects: single char errors, transpositions, network/type mismatch
--  - Checksum covers algo + network + type + payload (full identity)
--
--  References:
--  - AAS-001 v3.1 Section 2.3.5 (Checksum)

package Anubis_Address_Checksum with
   SPARK_Mode => On
is

   --  Compute checksum for address per AAS-001 v3.1
   --  Preimage: "mldsa87:network:type:payload"
   --  Returns first 24 bits (3 bytes) of SHA3-256(preimage)
   procedure Compute_Checksum (
      Network   : in  Network_Type;
      Entity    : in  Entity_Type;
      Payload   : in  Payload_Base32;
      Checksum  : out Checksum_Bytes
   ) with
      Global => null;

   --  Verify checksum matches expected value
   --  Returns True if checksums match
   function Verify_Checksum (
      Network           : Network_Type;
      Entity            : Entity_Type;
      Payload           : Payload_Base32;
      Expected_Checksum : Checksum_Bytes
   ) return Boolean with
      Global => null;

private

   --  Build preimage string for checksum computation
   --  Format: "mldsa87:network:type:payload" (no dashes in payload)
   --  Max length: 7 + 1 + 7 + 1 + 1 + 1 + 52 = 70 (for staging)
   Max_Preimage_Length : constant := 70;

   procedure Build_Preimage (
      Network   : in  Network_Type;
      Entity    : in  Entity_Type;
      Payload   : in  Payload_Base32;
      Preimage  : out String;
      Length    : out Natural
   ) with
      Global => null,
      Pre    => Preimage'Length >= Max_Preimage_Length,
      Post   => Length <= Preimage'Length;

end Anubis_Address_Checksum;
