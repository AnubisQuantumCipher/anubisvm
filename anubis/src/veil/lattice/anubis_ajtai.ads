--  ANUBIS Ajtai Commitment Scheme
--
--  Implements lattice-based commitments using the Ajtai construction:
--    C = A * (m || r) where A is public, m is message, r is randomness
--
--  Properties:
--  - Computationally hiding (under MLWE assumption)
--  - Statistically binding (under Module-SIS assumption)
--  - Post-quantum secure
--
--  Aligned with ML-KEM-1024 parameters for efficiency.
--
--  References:
--  - Ajtai (1996): "Generating Hard Instances of Lattice Problems"
--  - Peikert (2016): "A Decade of Lattice Cryptography"
--  - KHEPRI Blueprint v1.0, Anubis Privacy Layer Spec

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Anubis_Ajtai with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Commitment Key (public parameters)
   ---------------------------------------------------------------------------

   --  The commitment key is a k x 2k matrix of polynomials in R_q
   --  This allows committing to a k-vector message with k-vector randomness
   type Commitment_Key is record
      --  Matrix A is k rows by 2k columns
      --  A = [A1 | A2] where A1, A2 are k x k
      A1 : Module_Matrix;
      A2 : Module_Matrix;
   end record;

   ---------------------------------------------------------------------------
   --  Commitment Types
   ---------------------------------------------------------------------------

   --  Ajtai commitment C = A1*m + A2*r
   type Ajtai_Commitment is record
      Value : Module_Vector;
   end record;

   --  Commitment opening (witness)
   type Commitment_Opening is record
      Message    : Module_Vector;      --  The committed value
      Randomness : Module_Vector;      --  The blinding factor
      Norm_Bound : Natural;            --  Max coefficient norm
   end record;

   --  Zero commitment
   Zero_Commitment : constant Ajtai_Commitment := (
      Value => Zero_Vector
   );

   ---------------------------------------------------------------------------
   --  Key Generation
   ---------------------------------------------------------------------------

   --  Generate a random commitment key
   --  Uses SHAKE256 for deterministic expansion from seed
   procedure Generate_Key (
      Seed : Byte_Array;
      CK   : out Commitment_Key
   ) with
      Global => null,
      Pre    => Seed'Length = 32;

   ---------------------------------------------------------------------------
   --  Commitment Operations
   ---------------------------------------------------------------------------

   --  Commit to a message with given randomness
   --  C = A1*m + A2*r mod q
   procedure Commit (
      CK         : Commitment_Key;
      Message    : Module_Vector;
      Randomness : Module_Vector;
      Result     : out Ajtai_Commitment
   ) with
      Global => null;

   --  Commit to a byte array (converts to module vector)
   procedure Commit_Bytes (
      CK         : Commitment_Key;
      Data       : Byte_Array;
      Randomness : Byte_Array;
      Result     : out Ajtai_Commitment;
      Opening    : out Commitment_Opening;
      Success    : out Boolean
   ) with
      Global => null,
      Pre    => Data'Length <= K * N / 8 and Randomness'Length = 32;

   --  Verify a commitment opening
   function Verify_Opening (
      CK         : Commitment_Key;
      Commitment : Ajtai_Commitment;
      Opening    : Commitment_Opening
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Homomorphic Operations
   ---------------------------------------------------------------------------

   --  Add two commitments: C1 + C2
   --  Commit(m1, r1) + Commit(m2, r2) = Commit(m1+m2, r1+r2)
   procedure Add_Commitment (
      C1     : Ajtai_Commitment;
      C2     : Ajtai_Commitment;
      Result : out Ajtai_Commitment
   ) with
      Global => null;

   --  Subtract commitments: C1 - C2
   procedure Sub_Commitment (
      C1     : Ajtai_Commitment;
      C2     : Ajtai_Commitment;
      Result : out Ajtai_Commitment
   ) with
      Global => null;

   --  Scalar multiply: a * C
   procedure Scalar_Mult (
      Scalar : Field_Element;
      C      : Ajtai_Commitment;
      Result : out Ajtai_Commitment
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Convert bytes to module vector
   procedure Bytes_To_Vector (
      Data   : Byte_Array;
      Result : out Module_Vector
   ) with
      Global => null,
      Pre    => Data'Length <= K * N / 8;

   --  Convert module vector to bytes
   procedure Vector_To_Bytes (
      Vec    : Module_Vector;
      Result : out Byte_Array;
      Length : out Natural
   ) with
      Global => null,
      Pre    => Result'Length >= K * N / 8;

   --  Check if vector has small norm
   function Has_Small_Norm (
      Vec   : Module_Vector;
      Bound : Natural
   ) return Boolean with
      Global => null;

   --  Serialize commitment to bytes
   procedure Serialize_Commitment (
      C      : Ajtai_Commitment;
      Output : out Byte_Array;
      Length : out Natural
   ) with
      Global => null,
      Pre    => Output'Length >= Commitment_Size;

   --  Deserialize commitment from bytes
   procedure Deserialize_Commitment (
      Data   : Byte_Array;
      C      : out Ajtai_Commitment;
      Success : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  NTT Operations (for efficient polynomial arithmetic)
   ---------------------------------------------------------------------------

private

   --  Polynomial addition mod q
   function Add_Poly (A, B : Polynomial) return Polynomial with
      Global => null;

   --  Polynomial subtraction mod q
   function Sub_Poly (A, B : Polynomial) return Polynomial with
      Global => null;

   --  Polynomial multiplication mod (q, X^N + 1) using NTT
   function Mul_Poly (A, B : Polynomial) return Polynomial with
      Global => null;

   --  Vector addition
   function Add_Vector (A, B : Module_Vector) return Module_Vector with
      Global => null;

   --  Vector subtraction
   function Sub_Vector (A, B : Module_Vector) return Module_Vector with
      Global => null;

   --  Matrix-vector multiplication A * v
   function Mat_Vec_Mul (A : Module_Matrix; V : Module_Vector)
      return Module_Vector with
      Global => null;

end Anubis_Ajtai;
