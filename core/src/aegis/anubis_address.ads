pragma SPARK_Mode (On);

with Anubis_Address_Types; use Anubis_Address_Types;
with Anubis_Address_Derive; use Anubis_Address_Derive;

--  Anubis_Address: Aegis Address Standard (AAS-001 v3.1) Implementation
--
--  SINGLE ALGORITHM: ML-DSA-87 (NIST FIPS 204 Level 5)
--
--  High-level API for Aegis address operations:
--  - Create address from ML-DSA-87 public key
--  - Format address to string
--  - Parse and validate address from string
--
--  Canonical Address Format (AAS-001 v3.1):
--    mldsa87:network:type:chunked_payload-checksum
--
--  Example:
--    mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3-v8k4n
--
--  Components:
--  - Algorithm: mldsa87 (fixed, identifies ML-DSA-87)
--  - Network: main, test, dev, lab, staging
--  - Type: u (user), c (contract), v (validator), s (system)
--  - Payload: 52 Base32 chars (256-bit account ID), chunked 8-8-8-8-8-8-8-4
--  - Checksum: 5 Base32 chars (24-bit SHA3-256 checksum)
--
--  Security:
--  - Domain-separated key derivation prevents cross-type attacks
--  - Checksums detect typos and transcription errors
--  - Constant-time operations where applicable
--
--  References:
--  - AAS-001 v3.1 (Aegis Address Standard)
--  - NIST FIPS 204 (ML-DSA)
--  - NIST FIPS 202 (SHA-3)

package Anubis_Address with
   SPARK_Mode => On
is

   --  Create address from ML-DSA-87 public key
   --  Derives account ID using domain-separated SHA3-256
   function Create_Address (
      Network    : Network_Type;
      Entity     : Entity_Type;
      Public_Key : Public_Key_Bytes
   ) return Address with
      Global => null,
      Post   => Create_Address'Result.Valid = True;

   --  Format address to string representation per AAS-001 v3.1
   --  Returns: "mldsa87:network:type:chunked_payload-checksum"
   --  Returns actual string length used
   procedure Format_Address (
      Addr   : in  Address;
      Output : out Address_String;
      Length : out Natural
   ) with
      Global => null,
      Pre    => Addr.Valid,
      Post   => Length <= Max_Address_Length;

   --  Parse address from string
   --  Validates format, checksums, and ranges
   --  Sets Valid = True on success, False on any error
   procedure Parse_Address (
      Input  : in  String;
      Addr   : out Address
   ) with
      Global => null,
      Pre    => Input'First >= 1 and Input'Last < Natural'Last - 100;

   --  Validate address format and checksum
   --  Returns True if address is well-formed and checksum matches
   function Validate_Address (Addr : Address) return Boolean with
      Global => null;

   --  Extract chunked payload string from address
   --  Format: "qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3"
   procedure Get_Chunked_Payload (
      Addr    : in  Address;
      Payload : out String;
      Length  : out Natural
   ) with
      Global => null,
      Pre    => Addr.Valid and Payload'Length >= Chunked_Payload_Length,
      Post   => Length = Chunked_Payload_Length;

   --  Extract checksum string from address (5 Base32 characters)
   procedure Get_Checksum_String (
      Addr     : in  Address;
      Checksum : out String;
      Length   : out Natural
   ) with
      Global => null,
      Pre    => Addr.Valid and Checksum'Length >= 5,
      Post   => Length = 5;

end Anubis_Address;
