pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Execution; use Aegis_Execution;

--  AEGIS Crypto API: ANKH Cryptographic Interface for Contracts
--
--  This package provides the ANKH cryptographic API for KHEPRI contracts.
--  It wraps the existing post-quantum cryptographic primitives (ML-DSA-87,
--  ML-KEM-1024, SHA3) into syscall-compatible interfaces.
--
--  Key Features:
--  - SHA3-256/512 hashing
--  - SHAKE128/256 XOF
--  - ML-DSA-87 signature verification
--  - ML-KEM-1024 key encapsulation
--  - Constant-time operations where applicable
--
--  All operations consume gas based on the WCET schedule.
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 5.3: ANKH Crypto
--  - FIPS 202 (SHA3/SHAKE)
--  - FIPS 204 (ML-DSA)
--  - FIPS 203 (ML-KEM)

package Aegis_Crypto_API with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Hash Types
   ---------------------------------------------------------------------------

   --  Maximum input size for single-call hashing
   Max_Hash_Input : constant := 32 * 1024;  -- 32 KB

   subtype Hash_Input_Index is Natural range 0 .. Max_Hash_Input - 1;
   type Hash_Input_Buffer is array (Hash_Input_Index) of Byte;

   --  SHAKE output sizes
   type SHAKE_Variant is (SHAKE_128, SHAKE_256);

   --  Maximum SHAKE output
   Max_SHAKE_Output : constant := 1024;

   subtype SHAKE_Output_Index is Natural range 0 .. Max_SHAKE_Output - 1;
   type SHAKE_Output_Buffer is array (SHAKE_Output_Index) of Byte;

   ---------------------------------------------------------------------------
   --  Signature Types
   ---------------------------------------------------------------------------

   --  ML-DSA-87 signature size: 4627 bytes
   MLDSA87_Signature_Size : constant := 4627;

   subtype MLDSA87_Signature_Index is Natural range 0 .. MLDSA87_Signature_Size - 1;
   type MLDSA87_Signature is array (MLDSA87_Signature_Index) of Byte;

   --  ML-DSA-87 public key size: 2592 bytes
   MLDSA87_Public_Key_Size : constant := 2592;

   subtype MLDSA87_Public_Key_Index is Natural range 0 .. MLDSA87_Public_Key_Size - 1;
   type MLDSA87_Public_Key is array (MLDSA87_Public_Key_Index) of Byte;

   ---------------------------------------------------------------------------
   --  Key Encapsulation Types
   ---------------------------------------------------------------------------

   --  ML-KEM-1024 ciphertext size: 1568 bytes
   MLKEM1024_Ciphertext_Size : constant := 1568;

   subtype MLKEM1024_Ciphertext_Index is Natural range 0 .. MLKEM1024_Ciphertext_Size - 1;
   type MLKEM1024_Ciphertext is array (MLKEM1024_Ciphertext_Index) of Byte;

   --  ML-KEM-1024 shared secret size: 32 bytes
   MLKEM1024_Secret_Size : constant := 32;

   subtype MLKEM1024_Secret_Index is Natural range 0 .. MLKEM1024_Secret_Size - 1;
   type MLKEM1024_Shared_Secret is array (MLKEM1024_Secret_Index) of Byte;

   --  ML-KEM-1024 decapsulation key size: 3168 bytes
   MLKEM1024_Decaps_Key_Size : constant := 3168;

   subtype MLKEM1024_Decaps_Key_Index is Natural range 0 .. MLKEM1024_Decaps_Key_Size - 1;
   type MLKEM1024_Decaps_Key is array (MLKEM1024_Decaps_Key_Index) of Byte;

   ---------------------------------------------------------------------------
   --  Crypto Operation Results
   ---------------------------------------------------------------------------

   type Crypto_Result is (
      Crypto_OK,
      Crypto_Invalid_Input,
      Crypto_Invalid_Signature,
      Crypto_Decaps_Failure,
      Crypto_Out_Of_Gas
   );

   ---------------------------------------------------------------------------
   --  SHA3 Hashing
   ---------------------------------------------------------------------------

   --  SHA3-256 hash
   procedure SHA3_256_Hash (
      Ctx      : in Out Execution_Context;
      Input    : in     Hash_Input_Buffer;
      Length   : in     Natural;
      Output   : out    Hash256;
      Result   : out    Crypto_Result
   ) with
      Global => null,
      Pre    => Length <= Max_Hash_Input;

   --  SHA3-512 hash
   procedure SHA3_512_Hash (
      Ctx      : in Out Execution_Context;
      Input    : in     Hash_Input_Buffer;
      Length   : in     Natural;
      Output   : out    Hash512;
      Result   : out    Crypto_Result
   ) with
      Global => null,
      Pre    => Length <= Max_Hash_Input;

   --  Keccak-256 hash (Ethereum-compatible)
   procedure Keccak256_Hash (
      Ctx      : in Out Execution_Context;
      Input    : in     Hash_Input_Buffer;
      Length   : in     Natural;
      Output   : out    Hash256;
      Result   : out    Crypto_Result
   ) with
      Global => null,
      Pre    => Length <= Max_Hash_Input;

   ---------------------------------------------------------------------------
   --  SHAKE XOF
   ---------------------------------------------------------------------------

   --  SHAKE128/256 extendable output function
   procedure SHAKE_XOF (
      Ctx           : in Out Execution_Context;
      Variant       : in     SHAKE_Variant;
      Input         : in     Hash_Input_Buffer;
      Input_Length  : in     Natural;
      Output        : out    SHAKE_Output_Buffer;
      Output_Length : in     Natural;
      Result        : out    Crypto_Result
   ) with
      Global => null,
      Pre    => Input_Length <= Max_Hash_Input and
                Output_Length <= Max_SHAKE_Output;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Signature Verification
   ---------------------------------------------------------------------------

   --  Verify ML-DSA-87 signature
   --  This is the primary signature verification for KHEPRI contracts
   procedure MLDSA87_Verify (
      Ctx       : in Out Execution_Context;
      Message   : in     Hash_Input_Buffer;
      Msg_Len   : in     Natural;
      Signature : in     MLDSA87_Signature;
      Public_Key : in    MLDSA87_Public_Key;
      Valid     : out    Boolean;
      Result    : out    Crypto_Result
   ) with
      Global => null,
      Pre    => Msg_Len <= Max_Hash_Input;

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Key Decapsulation
   ---------------------------------------------------------------------------

   --  Decapsulate ML-KEM-1024 ciphertext
   --  Used for quantum-resistant key exchange in contracts
   procedure MLKEM1024_Decaps (
      Ctx          : in Out Execution_Context;
      Ciphertext   : in     MLKEM1024_Ciphertext;
      Decaps_Key   : in     MLKEM1024_Decaps_Key;
      Shared_Secret : out   MLKEM1024_Shared_Secret;
      Result       : out    Crypto_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Address Derivation
   ---------------------------------------------------------------------------

   --  Derive contract address from ML-DSA-87 public key
   --  Uses domain separator: "aegis-v1-mldsa87-<type>"
   procedure Derive_Address (
      Ctx        : in Out Execution_Context;
      Public_Key : in     MLDSA87_Public_Key;
      Entity     : in     Byte;  -- "u", "c", "v", "s"
      Address    : out    Contract_Address;
      Result     : out    Crypto_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Constant-time comparison (for signature verification)
   function Constant_Time_Equal (
      A, B   : Hash256
   ) return Boolean with
      Global => null;

   --  Zero memory (for secret cleanup)
   procedure Zeroize_Secret (
      Secret : in Out MLKEM1024_Shared_Secret
   ) with
      Global => null,
      Post   => (for all I in MLKEM1024_Secret_Index => Secret (I) = 0);

end Aegis_Crypto_API;
