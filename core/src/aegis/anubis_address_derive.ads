pragma SPARK_Mode (On);

with Anubis_Address_Types; use Anubis_Address_Types;
with Interfaces; use Interfaces;

--  Anubis_Address_Derive: Account ID derivation per AAS-001 v3.1
--
--  SINGLE ALGORITHM: ML-DSA-87 (NIST FIPS 204 Level 5)
--
--  Derivation Algorithm:
--  1. Construct domain separator: "aegis-v1-mldsa87-" || type
--  2. Hash: account_id = SHA3-256(domain_separator || public_key)
--
--  Domain Separator Examples:
--  - User account: "aegis-v1-mldsa87-u"
--  - Contract: "aegis-v1-mldsa87-c"
--  - Validator: "aegis-v1-mldsa87-v"
--  - System: "aegis-v1-mldsa87-s"
--
--  Security Properties:
--  - Domain separation prevents cross-type attacks
--  - SHA3-256 provides 256-bit collision resistance
--  - Deterministic derivation (same key -> same address)
--
--  References:
--  - AAS-001 v3.1 Section 2.3.4 (Payload Derivation)
--  - NIST FIPS 202 (SHA-3)
--  - NIST FIPS 204 (ML-DSA)

package Anubis_Address_Derive with
   SPARK_Mode => On
is

   --  ML-DSA-87 public key size: 2,592 bytes (fixed)
   MLDSA87_Public_Key_Size : constant := 2592;
   subtype Public_Key_Index is Natural range 0 .. MLDSA87_Public_Key_Size - 1;
   type Public_Key_Bytes is array (Public_Key_Index) of Unsigned_8;

   --  Derive account ID from ML-DSA-87 public key
   --  Uses SHA3-256 with domain separator per AAS-001 v3.1
   procedure Derive_Account_ID (
      Entity     : in  Entity_Type;
      Public_Key : in  Public_Key_Bytes;
      Account    : out Account_ID
   ) with
      Global => null;

   --  Domain separator per AAS-001 v3.1
   --  Format: "aegis-v1-mldsa87-" || type
   --  Max length: "aegis-v1-mldsa87-v" = 18 chars
   Max_Domain_Sep_Length : constant := 18;
   subtype Domain_Separator_String is String (1 .. Max_Domain_Sep_Length);

   --  Build domain separator string
   procedure Build_Domain_Separator (
      Entity    : in  Entity_Type;
      Separator : out Domain_Separator_String;
      Length    : out Natural
   ) with
      Global => null,
      Post   => Length <= Max_Domain_Sep_Length;

private

   --  Domain separator prefix: "aegis-v1-mldsa87-"
   Domain_Prefix : constant String := "aegis-v1-mldsa87-";

end Anubis_Address_Derive;
