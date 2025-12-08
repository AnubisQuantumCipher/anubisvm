--  ANUBIS Types: Shared types and constants for the privacy layer
--
--  This package defines the fundamental types used across all Anubis
--  components, aligned with ML-KEM-1024 parameters for efficiency.
--
--  References:
--  - FIPS 203 (ML-KEM)
--  - Ajtai commitment scheme parameters
--  - KHEPRI Blueprint v1.0, Anubis Privacy Layer Spec

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package Anubis_Types with
   SPARK_Mode => On,
   Pure
is

   ---------------------------------------------------------------------------
   --  Basic Types
   ---------------------------------------------------------------------------

   subtype Byte is Unsigned_8;
   type Byte_Array is array (Natural range <>) of Byte;

   --  32-byte hash (Keccak-256 output)
   subtype Hash_256 is Byte_Array (0 .. 31);

   --  64-byte extended hash
   subtype Hash_512 is Byte_Array (0 .. 63);

   --  Timestamp for expiry/validity
   type Timestamp is new Unsigned_64;

   ---------------------------------------------------------------------------
   --  Lattice Parameters (aligned with ML-KEM-1024)
   ---------------------------------------------------------------------------

   --  Module parameters
   Q : constant := 3329;           --  Modulus (same as ML-KEM)
   N : constant := 256;            --  Polynomial degree
   K : constant := 4;              --  Module rank (NIST L5)

   --  Field element modulo Q
   type Field_Element is mod Q;

   --  Polynomial in R_q = Z_q[X]/(X^N + 1)
   type Polynomial is array (0 .. N - 1) of Field_Element;

   --  Module vector (k polynomials)
   type Module_Vector is array (0 .. K - 1) of Polynomial;

   --  Module matrix (k x k polynomials)
   type Module_Matrix is array (0 .. K - 1, 0 .. K - 1) of Polynomial;

   --  Zero polynomial
   Zero_Poly : constant Polynomial := (others => 0);

   --  Zero module vector
   Zero_Vector : constant Module_Vector := (others => Zero_Poly);

   ---------------------------------------------------------------------------
   --  Commitment Parameters
   ---------------------------------------------------------------------------

   --  Norm bounds for commitments
   Norm_Bound_Message    : constant := 1;    --  Binary coefficients
   Norm_Bound_Randomness : constant := 2;    --  Small coefficients

   --  Commitment size in bytes (4 polynomials * 256 coefficients * ~12 bits)
   Commitment_Size : constant := K * N * 2;  --  ~2KB

   --  Opening size in bytes
   Opening_Size : constant := 2 * K * N * 2;  --  ~4KB

   ---------------------------------------------------------------------------
   --  STARK Parameters
   ---------------------------------------------------------------------------

   --  Goldilocks prime field: p = 2^64 - 2^32 + 1
   P_Goldilocks : constant := 2**64 - 2**32 + 1;

   --  STARK field element
   type STARK_Field is mod P_Goldilocks;

   --  STARK configuration
   Blowup_Factor  : constant := 8;
   Num_Queries    : constant := 30;
   Folding_Factor : constant := 8;
   Security_Bits  : constant := 128;

   ---------------------------------------------------------------------------
   --  Proof Sizes (bytes)
   ---------------------------------------------------------------------------

   Max_STARK_Proof_Size  : constant := 200 * 1024;  --  ~200KB
   Max_Range_Proof_Size  : constant := 20 * 1024;   --  ~20KB
   Max_Linear_Proof_Size : constant := 16 * 1024;   --  ~16KB
   Max_Opening_Proof_Size : constant := 10 * 1024;  --  ~10KB

   ---------------------------------------------------------------------------
   --  Private State Limits
   ---------------------------------------------------------------------------

   Max_Entry_Size : constant := 4096;     --  Maximum encrypted entry
   AEAD_Overhead  : constant := 40;       --  Nonce (24) + Tag (16)
   Max_Note_Size  : constant := 256;      --  Maximum note plaintext

   ---------------------------------------------------------------------------
   --  Transaction Limits
   ---------------------------------------------------------------------------

   Max_Inputs  : constant := 16;
   Max_Outputs : constant := 16;

   ---------------------------------------------------------------------------
   --  Disclosure Types
   ---------------------------------------------------------------------------

   type Disclosure_Scope is (
      Full_History,        --  All transactions
      Time_Range,          --  Specific time period
      Specific_Contracts,  --  Specific contract interactions
      Balance_Only         --  Only current balance
   );

   type Disclosure_Property is (
      Balance_Greater_Than,   --  Balance > threshold
      Balance_Less_Than,      --  Balance < threshold
      Balance_In_Range,       --  Balance in [min, max]
      Is_Verified_User,       --  Membership in verified set
      Has_Permission,         --  Has specific permission
      Transaction_Count       --  Number of transactions
   );

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type Anubis_Error is (
      Error_None,
      Error_Invalid_Input,
      Error_Invalid_Commitment,
      Error_Invalid_Proof,
      Error_Invalid_Signature,
      Error_Decryption_Failed,
      Error_Verification_Failed,
      Error_Expired,
      Error_Unauthorized,
      Error_Double_Spend,
      Error_Insufficient_Balance,
      Error_Buffer_Too_Small,
      Error_Internal
   );

   ---------------------------------------------------------------------------
   --  Gas Costs for Privacy Operations
   ---------------------------------------------------------------------------

   Gas_Private_Load    : constant := 5_000;
   Gas_Private_Store   : constant := 25_000;
   Gas_Shield          : constant := 50_000;
   Gas_Unshield        : constant := 100_000;
   Gas_Commit          : constant := 10_000;
   Gas_Prove_Range     : constant := 200_000;
   Gas_Prove_Linear    : constant := 150_000;
   Gas_Verify_Proof    : constant := 50_000;
   Gas_Nullify         : constant := 5_000;
   Gas_Encrypt         : constant := 50_000;
   Gas_Decrypt         : constant := 50_000;

end Anubis_Types;
