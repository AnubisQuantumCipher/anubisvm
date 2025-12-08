pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  KHEPRI Crypto: Contract Cryptographic API
--
--  This package provides the cryptographic primitives available to
--  KHEPRI smart contracts. All operations are gas-metered and executed
--  through the ANKH crypto layer.
--
--  Key Features:
--  - Post-Quantum Cryptography (ML-DSA-87, ML-KEM-1024)
--  - Hash Functions (SHA3-256, Keccak-256)
--  - Signature Verification
--  - Key Encapsulation
--
--  Security:
--  - All operations are constant-time where applicable
--  - Keys are zeroized after use
--  - SPARK contracts ensure correct usage
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 4: ANKH Crypto Layer
--  - FIPS 203: ML-KEM (Module-Lattice Key Encapsulation)
--  - FIPS 204: ML-DSA (Module-Lattice Digital Signature)
--  - FIPS 202: SHA-3 Standard

package Khepri_Crypto with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Cryptographic Types
   ---------------------------------------------------------------------------

   --  Hash output sizes
   subtype Hash_256 is Bytes32;
   subtype Hash_512 is Bytes64;

   --  ML-DSA-87 sizes (from FIPS 204)
   MLDSA_Public_Key_Size  : constant := 2592;
   MLDSA_Secret_Key_Size  : constant := 4896;
   MLDSA_Signature_Size   : constant := 4627;

   --  ML-KEM-1024 sizes (from FIPS 203)
   MLKEM_Public_Key_Size  : constant := 1568;
   MLKEM_Secret_Key_Size  : constant := 3168;
   MLKEM_Ciphertext_Size  : constant := 1568;
   MLKEM_Shared_Key_Size  : constant := 32;

   --  Key types
   type MLDSA_Public_Key is array (0 .. MLDSA_Public_Key_Size - 1) of Byte;
   type MLDSA_Secret_Key is array (0 .. MLDSA_Secret_Key_Size - 1) of Byte;
   type MLDSA_Signature is array (0 .. MLDSA_Signature_Size - 1) of Byte;

   type MLKEM_Public_Key is array (0 .. MLKEM_Public_Key_Size - 1) of Byte;
   type MLKEM_Secret_Key is array (0 .. MLKEM_Secret_Key_Size - 1) of Byte;
   type MLKEM_Ciphertext is array (0 .. MLKEM_Ciphertext_Size - 1) of Byte;
   type MLKEM_Shared_Key is array (0 .. MLKEM_Shared_Key_Size - 1) of Byte;

   --  Crypto operation result
   type Crypto_Result is (
      Crypto_Success,
      Crypto_Invalid_Key,
      Crypto_Invalid_Signature,
      Crypto_Decapsulation_Failure,
      Crypto_Out_Of_Gas,
      Crypto_Error
   );

   ---------------------------------------------------------------------------
   --  Hash Functions
   ---------------------------------------------------------------------------

   --  SHA3-256 hash of arbitrary data
   --  Gas: 30 base + 6 per 32-byte word
   function SHA3_256 (Data : Byte_Array) return Hash_256 with
      Global => null,
      Pre    => Data'Length <= 2**16;

   --  SHA3-256 hash of fixed bytes
   function SHA3_256_Bytes32 (Data : Bytes32) return Hash_256 with
      Global => null;

   --  SHA3-256 hash of U256
   function SHA3_256_U256 (Value : Uint256) return Hash_256 with
      Global => null;

   --  Keccak-256 hash (Ethereum-compatible)
   --  Gas: 30 base + 6 per 32-byte word
   function Keccak_256 (Data : Byte_Array) return Hash_256 with
      Global => null,
      Pre    => Data'Length <= 2**16;

   --  SHA3-512 hash
   function SHA3_512 (Data : Byte_Array) return Hash_512 with
      Global => null,
      Pre    => Data'Length <= 2**16;

   ---------------------------------------------------------------------------
   --  Address Derivation
   ---------------------------------------------------------------------------

   --  Derive contract address from ML-DSA-87 public key
   --  Address = SHA3-256(public_key)
   function Derive_Address (PK : MLDSA_Public_Key) return Address with
      Global => null;

   --  Check if address is valid (non-zero)
   function Is_Valid_Address (Addr : Address) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Operations (Post-Quantum Signatures)
   ---------------------------------------------------------------------------

   --  Verify ML-DSA-87 signature
   --  Gas: 30,000
   function MLDSA_Verify (
      Message   : Byte_Array;
      Signature : MLDSA_Signature;
      Public_Key : MLDSA_Public_Key
   ) return Crypto_Result with
      Global => null,
      Pre    => Message'Length <= 2**16;

   --  Verify signature with pre-hashed message
   function MLDSA_Verify_Hash (
      Message_Hash : Hash_256;
      Signature    : MLDSA_Signature;
      Public_Key   : MLDSA_Public_Key
   ) return Crypto_Result with
      Global => null;

   --  Recover signer address from signature (if deterministic context)
   --  Note: ML-DSA signatures don"t support ECDSA-style recovery
   --  This verifies and returns the address if signature is valid
   function Verify_And_Get_Signer (
      Message    : Byte_Array;
      Signature  : MLDSA_Signature;
      Public_Key : MLDSA_Public_Key
   ) return Address with
      Global => null,
      Pre    => Message'Length <= 2**16;

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Operations (Post-Quantum Key Encapsulation)
   ---------------------------------------------------------------------------

   --  Encapsulate: generate shared secret and ciphertext
   --  Gas: 25,000
   --  Used by contracts that need to establish shared secrets
   procedure MLKEM_Encaps (
      Public_Key  : in  MLKEM_Public_Key;
      Ciphertext  : out MLKEM_Ciphertext;
      Shared_Key  : out MLKEM_Shared_Key;
      Result      : out Crypto_Result
   ) with
      Global => null;

   --  Decapsulate: recover shared secret from ciphertext
   --  Gas: 25,000
   procedure MLKEM_Decaps (
      Secret_Key  : in  MLKEM_Secret_Key;
      Ciphertext  : in  MLKEM_Ciphertext;
      Shared_Key  : out MLKEM_Shared_Key;
      Result      : out Crypto_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Compare two hashes (constant-time)
   function Hash_Equal (A, B : Hash_256) return Boolean with
      Global => null;

   --  Compare two addresses (constant-time)
   function Address_Equal (A, B : Address) return Boolean with
      Global => null;

   --  Calculate function selector/signature hash from declaration string
   --  e.g., "transfer(address,uint256)" -> first 4 bytes of keccak256
   function Calculate_Signature (
      Declaration : String
   ) return Hash_256 with
      Global => null,
      Pre    => Declaration'Length <= 256;

   --  Zero out sensitive data (compiler-safe zeroization)
   procedure Zeroize_Hash (H : out Hash_256) with
      Global => null,
      Post   => (for all I in H'Range => H (I) = 0);

   procedure Zeroize_Shared_Key (K : out MLKEM_Shared_Key) with
      Global => null,
      Post   => (for all I in K'Range => K (I) = 0);

   ---------------------------------------------------------------------------
   --  Message Construction Helpers
   ---------------------------------------------------------------------------

   --  Encode address as bytes for hashing
   function Address_To_Bytes (Addr : Address) return Bytes32 with
      Global => null;

   --  Encode U256 as big-endian bytes for hashing
   function U256_To_Bytes_BE (Value : Uint256) return Bytes32 with
      Global => null;

   --  Pack multiple values for structured hashing (EIP-712 style)
   type Encoded_Data_Index is range 0 .. 1023;
   type Encoded_Data is array (Encoded_Data_Index) of Byte;

   --  Encode domain separator
   function Encode_Domain (
      Name        : Bounded_String;
      Version     : Bounded_String;
      Chain_ID    : Uint256;
      Contract    : Address
   ) return Hash_256 with
      Global => null;

   --  Encode structured message
   function Encode_Struct (
      Type_Hash : Hash_256;
      Fields    : Byte_Array
   ) return Hash_256 with
      Global => null,
      Pre    => Fields'Length <= 1024;

private

   --  Internal constant-time comparison
   function Constant_Time_Compare (
      A, B   : Byte_Array;
      Length : Natural
   ) return Boolean with
      Global => null,
      Pre    => A'Length >= Length and B'Length >= Length;

end Khepri_Crypto;
