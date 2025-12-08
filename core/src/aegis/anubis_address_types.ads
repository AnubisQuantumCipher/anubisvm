pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

--  Anubis_Address_Types: AAS-001 Aegis Address Standard v3.1
--
--  SINGLE ALGORITHM: ML-DSA-87 (NIST FIPS 204 Level 5)
--
--  Canonical Format:
--  mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3-v8k4n
--  |-----| |--| |-| |--------------------------------------------------| |---|
--   algo   net  t                 payload (52 chars, chunked)          checksum
--
--  Components:
--  - Algorithm: mldsa87 (fixed, 7 chars)
--  - Network: main/test/dev/lab/staging (3-7 chars)
--  - Type: u/c/v/s (1 char)
--  - Payload: 52 Base32 chars (256-bit account ID), chunked 8-8-8-8-8-8-8-4
--  - Checksum: 5 Base32 chars (24-bit SHA3-256)
--
--  Address Length: 79 characters (for mldsa87:main:u:...)
--
--  References:
--  - AAS-001 v3.1 (Aegis Address Standard)
--  - NIST FIPS 204 (ML-DSA)
--  - NIST FIPS 202 (SHA3)

package Anubis_Address_Types with
   SPARK_Mode => On,
   Pure
is

   --  Algorithm tag (fixed to mldsa87)
   Algorithm_Tag : constant String := "mldsa87";

   --  Network identifiers
   type Network_Type is (
      Main,      --  Production mainnet
      Test,      --  Public testnet
      Dev,       --  Development network
      Lab,       --  Laboratory/experimental
      Staging    --  Pre-production staging
   );

   --  Address entity types
   type Entity_Type is (
      User,       --  "u" - Externally owned account
      Contract,   --  "c" - Smart contract
      Validator,  --  "v" - Consensus participant
      System      --  "s" - Protocol module
   );

   --  Account ID: 256-bit (32 bytes) identifier
   --  Derived as: SHA3-256(domain_separator || public_key)
   --  Domain separator: "aegis-v1-mldsa87-" || type
   subtype Account_ID_Index is Natural range 0 .. 31;
   type Account_ID is array (Account_ID_Index) of Unsigned_8;

   --  Checksum: 24-bit (3 bytes)
   subtype Checksum_Index is Natural range 0 .. 2;
   type Checksum_Bytes is array (Checksum_Index) of Unsigned_8;

   --  Base32 character (Crockford alphabet, lowercase canonical)
   --  0-9, a-z excluding i, l, o, u (32 chars total)
   subtype Base32_Char is Character with
      Static_Predicate => Base32_Char in '0' .. '9' | 'a' .. 'h' | 'j' | 'k' |
                                         'm' | 'n' | 'p' .. 't' | 'v' .. 'z';

   --  Account ID payload in Base32: 32 bytes -> 52 Base32 chars
   subtype Payload_Index is Natural range 0 .. 51;
   type Payload_Base32 is array (Payload_Index) of Base32_Char;

   --  Chunked payload with hyphens: 8-8-8-8-8-8-4 = 52 chars + 6 hyphens = 58 chars
   Chunked_Payload_Length : constant := 58;
   subtype Chunked_Payload_Index is Natural range 0 .. Chunked_Payload_Length - 1;
   type Chunked_Payload is array (Chunked_Payload_Index) of Character;

   --  Checksum in Base32: 3 bytes -> 5 Base32 chars
   subtype Checksum_B32_Index is Natural range 0 .. 4;
   type Checksum_Base32 is array (Checksum_B32_Index) of Base32_Char;

   --  Full address string: "mldsa87:network:type:chunked_payload-checksum"
   --  Max length: 7 + 1 + 7 + 1 + 1 + 1 + 58 + 1 + 5 = 82 (for staging)
   --  Typical: 7 + 1 + 4 + 1 + 1 + 1 + 58 + 1 + 5 = 79 (for main)
   Max_Address_Length : constant := 82;
   subtype Address_String is String (1 .. Max_Address_Length);

   --  Address record containing all components
   type Address is record
      Network   : Network_Type;
      Entity    : Entity_Type;
      Account   : Account_ID;
      Valid     : Boolean;  --  True when checksum validated
   end record;

   --  String representations for networks
   function Network_String (Net : Network_Type) return String is
      (case Net is
         when Main    => "main",
         when Test    => "test",
         when Dev     => "dev",
         when Lab     => "lab",
         when Staging => "staging")
   with
      Post => Network_String'Result'Length in 3 .. 7;

   --  Character representations for entity types
   function Entity_Char (Ent : Entity_Type) return Character is
      (case Ent is
         when User      => 'u',
         when Contract  => 'c',
         when Validator => 'v',
         when System    => 's')
   with
      Post => Entity_Char'Result in 'c' | 's' | 'u' | 'v';

   --  String representation for entity types (for domain separator)
   function Entity_String (Ent : Entity_Type) return String is
      (case Ent is
         when User      => "u",
         when Contract  => "c",
         when Validator => "v",
         when System    => "s")
   with
      Post => Entity_String'Result'Length = 1;

   --  Constants for address format
   Separator_Colon : constant Character := ':';
   Separator_Dash  : constant Character := '-';

   --  Crockford Base32 alphabet (excludes i, l, o, u to avoid confusion)
   type Base32_Alphabet_Array is array (Natural range 0 .. 31) of Character;
   Base32_Alphabet : constant Base32_Alphabet_Array :=
      "0123456789abcdefghjkmnpqrstvwxyz";

end Anubis_Address_Types;
