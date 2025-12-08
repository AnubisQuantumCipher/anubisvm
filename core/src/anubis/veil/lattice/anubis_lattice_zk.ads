-------------------------------------------------------------------------------
--  ANUBIS VEIL - Lattice-Based Zero-Knowledge Proofs
--  Post-quantum ZK primitives using lattice problems (SIS/LWE)
--
--  Provides:
--  - Ajtai hash commitments (binding under SIS assumption)
--  - Linear relation proofs
--  - Range proofs for confidential values
--  - Opening proofs for committed values
--
--  Security based on:
--  - Short Integer Solution (SIS) problem
--  - Learning With Errors (LWE) problem
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Anubis_Lattice_ZK with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Lattice Parameters (NIST Level 5 security)
   ---------------------------------------------------------------------------

   --  Ring dimension (power of 2)
   N : constant := 1024;

   --  Modulus for Ajtai hash (prime, NTT-friendly)
   Q : constant := 12289;  -- Matches ML-KEM q for code reuse

   --  Commitment randomness bound
   Beta : constant := 1;  -- Coefficients in {-1, 0, 1}

   --  Number of rows in Ajtai matrix
   M : constant := 8;

   --  Soundness security parameter
   Lambda : constant := 128;

   --  Challenge space (Hamming weight)
   Challenge_Weight : constant := 60;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Ring_Coeff is Integer range -(Q / 2) .. (Q / 2);
   type Ring_Element is array (0 .. N - 1) of Ring_Coeff;

   type Ring_Vector is array (0 .. M - 1) of Ring_Element;

   --  Ajtai matrix A (m x n over R_q)
   type Ajtai_Matrix is array (0 .. M - 1, 0 .. M - 1) of Ring_Element;

   --  Commitment (t = A * r mod q)
   type Commitment is record
      Value : Ring_Vector;
   end record;

   --  Opening (randomness r)
   type Opening is record
      Randomness : Ring_Vector;
   end record;

   --  Challenge (sparse ternary polynomial)
   type Challenge is record
      Coeffs : Ring_Element;
      Weight : Natural;
   end record;

   --  Proof of knowledge of opening
   type Opening_Proof is record
      Response   : Ring_Vector;
      Challenge  : Anubis_Lattice_ZK.Challenge;
   end record;

   --  Linear relation proof: proves a * x + b * y = c for committed x, y
   type Linear_Proof is record
      Z1, Z2     : Ring_Vector;
      Challenge  : Anubis_Lattice_ZK.Challenge;
   end record;

   --  Range proof: proves committed value in [0, 2^bits - 1]
   Max_Range_Bits : constant := 64;
   type Bit_Commitments is array (0 .. Max_Range_Bits - 1) of Commitment;
   type Bit_Opening_Proofs is array (0 .. Max_Range_Bits - 1) of Opening_Proof;

   type Range_Proof is record
      Bit_Commits    : Bit_Commitments;
      Num_Bits       : Natural;
      Bit_Openings   : Bit_Opening_Proofs;
      Sum_Proof      : Linear_Proof;
   end record;

   --  Equality proof: proves two commitments hide same value
   type Equality_Proof is record
      Response       : Ring_Vector;
      Challenge      : Anubis_Lattice_ZK.Challenge;
   end record;

   ---------------------------------------------------------------------------
   --  Setup
   ---------------------------------------------------------------------------

   --  Public parameters
   type Public_Params is record
      A              : Ajtai_Matrix;
      Hash_Seed      : Byte_Array (0 .. 31);
   end record;

   --  Generate public parameters (deterministic from seed)
   procedure Setup (
      Seed           : Byte_Array;
      Params         : out Public_Params
   ) with
      Global => null,
      Pre => Seed'Length = 32;

   --  Verify parameters are well-formed
   function Verify_Params (Params : Public_Params) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Ajtai Commitment
   ---------------------------------------------------------------------------

   --  Commit to ring element: t = A * r where r is short
   procedure Commit (
      Params         : Public_Params;
      Message        : Ring_Element;
      Randomness     : Byte_Array;
      Com            : out Commitment;
      Open           : out Opening
   ) with
      Global => null,
      Pre => Randomness'Length = 64;

   --  Commit to byte array (hashed to ring element first)
   procedure Commit_Bytes (
      Params         : Public_Params;
      Data           : Byte_Array;
      Randomness     : Byte_Array;
      Com            : out Commitment;
      Open           : out Opening
   ) with
      Global => null,
      Pre => Randomness'Length = 64;

   --  Verify opening
   function Verify_Opening (
      Params         : Public_Params;
      Com            : Commitment;
      Message        : Ring_Element;
      Open           : Opening
   ) return Boolean with
      Global => null;

   --  Homomorphic addition of commitments
   function Add_Commitments (
      A, B           : Commitment
   ) return Commitment with
      Global => null;

   --  Homomorphic scalar multiplication
   function Scale_Commitment (
      C              : Commitment;
      Scalar         : Ring_Coeff
   ) return Commitment with
      Global => null;

   ---------------------------------------------------------------------------
   --  Opening Proofs (Sigma Protocol)
   ---------------------------------------------------------------------------

   --  Prove knowledge of opening
   procedure Prove_Opening (
      Params         : Public_Params;
      Com            : Commitment;
      Open           : Opening;
      Transcript     : Byte_Array;
      Proof          : out Opening_Proof
   ) with
      Global => null,
      Pre => Transcript'Length > 0;

   --  Verify opening proof
   function Verify_Opening_Proof (
      Params         : Public_Params;
      Com            : Commitment;
      Proof          : Opening_Proof;
      Transcript     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Transcript'Length > 0;

   ---------------------------------------------------------------------------
   --  Linear Relation Proofs
   ---------------------------------------------------------------------------

   --  Prove: a * x + b * y = c (mod q) for committed x, y
   procedure Prove_Linear (
      Params         : Public_Params;
      Com_X, Com_Y   : Commitment;
      Open_X, Open_Y : Opening;
      A_Coeff        : Ring_Coeff;
      B_Coeff        : Ring_Coeff;
      C_Result       : Ring_Element;
      Transcript     : Byte_Array;
      Proof          : out Linear_Proof
   ) with
      Global => null,
      Pre => Transcript'Length > 0;

   --  Verify linear relation proof
   function Verify_Linear (
      Params         : Public_Params;
      Com_X, Com_Y   : Commitment;
      A_Coeff        : Ring_Coeff;
      B_Coeff        : Ring_Coeff;
      C_Result       : Ring_Element;
      Proof          : Linear_Proof;
      Transcript     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Transcript'Length > 0;

   ---------------------------------------------------------------------------
   --  Range Proofs
   ---------------------------------------------------------------------------

   --  Prove value in [0, 2^bits - 1]
   procedure Prove_Range (
      Params         : Public_Params;
      Value          : Unsigned_64;
      Num_Bits       : Natural;
      Randomness     : Byte_Array;
      Transcript     : Byte_Array;
      Com            : out Commitment;
      Proof          : out Range_Proof
   ) with
      Global => null,
      Pre => Num_Bits in 1 .. Max_Range_Bits
             and Value < 2 ** Num_Bits
             and Randomness'Length = 64
             and Transcript'Length > 0;

   --  Verify range proof
   function Verify_Range (
      Params         : Public_Params;
      Com            : Commitment;
      Num_Bits       : Natural;
      Proof          : Range_Proof;
      Transcript     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Num_Bits in 1 .. Max_Range_Bits
             and Transcript'Length > 0;

   ---------------------------------------------------------------------------
   --  Equality Proofs
   ---------------------------------------------------------------------------

   --  Prove two commitments hide the same value
   procedure Prove_Equality (
      Params         : Public_Params;
      Com_1, Com_2   : Commitment;
      Open_1, Open_2 : Opening;
      Transcript     : Byte_Array;
      Proof          : out Equality_Proof
   ) with
      Global => null,
      Pre => Transcript'Length > 0;

   --  Verify equality proof
   function Verify_Equality (
      Params         : Public_Params;
      Com_1, Com_2   : Commitment;
      Proof          : Equality_Proof;
      Transcript     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Transcript'Length > 0;

   ---------------------------------------------------------------------------
   --  Challenge Generation (Fiat-Shamir)
   ---------------------------------------------------------------------------

   --  Generate challenge from transcript
   procedure Generate_Challenge (
      Transcript     : Byte_Array;
      Chal           : out Challenge
   ) with
      Global => null,
      Pre => Transcript'Length > 0,
      Post => Chal.Weight = Challenge_Weight;

   --  Verify challenge is well-formed
   function Is_Valid_Challenge (Chal : Challenge) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Ring Arithmetic
   ---------------------------------------------------------------------------

   --  Zero ring element
   Zero_Ring : constant Ring_Element := (others => 0);

   --  One ring element (1, 0, 0, ..., 0)
   One_Ring : constant Ring_Element := (0 => 1, others => 0);

   --  Add ring elements (coefficient-wise mod q)
   function Ring_Add (A, B : Ring_Element) return Ring_Element with
      Global => null;

   --  Subtract ring elements
   function Ring_Sub (A, B : Ring_Element) return Ring_Element with
      Global => null;

   --  Multiply ring elements (polynomial multiplication mod X^n + 1)
   function Ring_Mul (A, B : Ring_Element) return Ring_Element with
      Global => null;

   --  Scalar multiply
   function Ring_Scale (A : Ring_Element; S : Ring_Coeff) return Ring_Element with
      Global => null;

   --  Negate ring element
   function Ring_Neg (A : Ring_Element) return Ring_Element with
      Global => null;

   --  Reduce coefficient mod q
   function Reduce_Coeff (X : Integer) return Ring_Coeff with
      Global => null;

   ---------------------------------------------------------------------------
   --  Vector Operations
   ---------------------------------------------------------------------------

   --  Zero vector
   Zero_Vector : constant Ring_Vector := (others => Zero_Ring);

   --  Add vectors
   function Vector_Add (A, B : Ring_Vector) return Ring_Vector with
      Global => null;

   --  Subtract vectors
   function Vector_Sub (A, B : Ring_Vector) return Ring_Vector with
      Global => null;

   --  Scalar multiply vector
   function Vector_Scale (V : Ring_Vector; S : Ring_Coeff) return Ring_Vector with
      Global => null;

   --  Matrix-vector multiply
   function Matrix_Vector_Mul (
      Mat : Ajtai_Matrix;
      Vec : Ring_Vector
   ) return Ring_Vector with
      Global => null;

   ---------------------------------------------------------------------------
   --  Norm Bounds
   ---------------------------------------------------------------------------

   --  Compute infinity norm of ring element
   function Inf_Norm (A : Ring_Element) return Natural with
      Global => null;

   --  Compute L2 norm squared
   function L2_Norm_Sq (A : Ring_Element) return Unsigned_64 with
      Global => null;

   --  Check if vector is short (within bounds)
   function Is_Short (V : Ring_Vector; Bound : Natural) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Commitment size in bytes
   Commitment_Bytes : constant := M * N * 2;  -- 2 bytes per coeff

   --  Serialize commitment
   procedure Serialize_Commitment (
      Com            : Commitment;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= Commitment_Bytes;

   --  Deserialize commitment
   procedure Deserialize_Commitment (
      Input          : Byte_Array;
      Com            : out Commitment;
      Success        : out Boolean
   ) with
      Global => null;

   --  Serialize opening proof
   procedure Serialize_Opening_Proof (
      Proof          : Opening_Proof;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 2 * Commitment_Bytes;

   --  Deserialize opening proof
   procedure Deserialize_Opening_Proof (
      Input          : Byte_Array;
      Proof          : out Opening_Proof;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Opening (Open : in Out Opening) with
      Global => null;

   procedure Zeroize_Ring (R : in Out Ring_Element) with
      Global => null,
      Post => R = Zero_Ring;

end Anubis_Lattice_ZK;
