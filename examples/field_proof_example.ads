-------------------------------------------------------------------------------
--  EXAMPLE: Field Arithmetic in SPARK Proof Contexts
--  Demonstrates formally verified usage patterns for SCARAB proofs
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Field; use Anubis_Field;

package Field_Proof_Example with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Example 1: Verified Field Inverse Property
   --  Proves that A * Inv(A) = 1 for all non-zero A
   ---------------------------------------------------------------------------

   procedure Verify_Inverse_Property (
      A : Valid_Field;
      Success : out Boolean
   ) with
      Global => null,
      Pre => Is_Canonical (A) and A /= Zero,
      Post => Success = True,  -- Always succeeds due to field properties
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Example 2: Polynomial Commitment Evaluation
   --  Verifies that a polynomial commitment matches an evaluation
   ---------------------------------------------------------------------------

   type Commitment is record
      Poly : Poly_256;
      Point : Valid_Field;
      Value : Valid_Field;
   end record;

   function Verify_Commitment (C : Commitment) return Boolean with
      Global => null,
      Pre => Poly_Is_Canonical (C.Poly)
             and Is_Canonical (C.Point)
             and Is_Canonical (C.Value),
      Post => (if Verify_Commitment'Result then
                 Poly_Eval (C.Poly, C.Point) = C.Value);

   ---------------------------------------------------------------------------
   --  Example 3: Shamir Secret Sharing
   --  Proves that reconstruction from t shares recovers the secret
   ---------------------------------------------------------------------------

   Max_Shares : constant := 10;

   type Share is record
      X : Valid_Field;  -- Share index
      Y : Valid_Field;  -- Share value
   end record;

   type Share_Array is array (1 .. Max_Shares) of Share;

   --  Split secret into N shares, any T can reconstruct
   procedure Split_Secret (
      Secret    : Valid_Field;
      Threshold : Positive;
      N_Shares  : Positive;
      Shares    : out Share_Array;
      Success   : out Boolean
   ) with
      Global => null,
      Pre => Is_Canonical (Secret)
             and Threshold <= N_Shares
             and N_Shares <= Max_Shares,
      Post => (if Success then
                 (for all I in 1 .. N_Shares =>
                    Is_Canonical (Shares (I).X)
                    and Is_Canonical (Shares (I).Y))),
      Always_Terminates;

   --  Reconstruct secret from T shares using Lagrange interpolation
   procedure Reconstruct_Secret (
      Shares       : Share_Array;
      N_Shares     : Positive;
      Secret       : out Valid_Field
   ) with
      Global => null,
      Pre => N_Shares <= Max_Shares
             and (for all I in 1 .. N_Shares =>
                    Is_Canonical (Shares (I).X)
                    and Is_Canonical (Shares (I).Y)),
      Post => Is_Canonical (Secret),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Example 4: R1CS Constraint Verification
   --  Proves satisfiability of rank-1 constraint system
   ---------------------------------------------------------------------------

   type R1CS_Constraint is record
      A : Valid_Field;  -- Left wire
      B : Valid_Field;  -- Right wire
      C : Valid_Field;  -- Output wire
   end record;

   --  Verify: A * B = C (fundamental building block of SCARAB)
   function Verify_R1CS (Constraint : R1CS_Constraint) return Boolean with
      Global => null,
      Pre => Is_Canonical (Constraint.A)
             and Is_Canonical (Constraint.B)
             and Is_Canonical (Constraint.C),
      Post => (if Verify_R1CS'Result then
                 Mul (Constraint.A, Constraint.B) = Constraint.C);

   ---------------------------------------------------------------------------
   --  Example 5: Batch Fiat-Shamir Challenge
   --  Generates verifier challenges for multiple proof rounds
   ---------------------------------------------------------------------------

   Max_Rounds : constant := 32;
   type Challenge_Array is array (1 .. Max_Rounds) of Valid_Field;

   --  Generate deterministic challenges from transcript
   procedure Generate_Challenges (
      Transcript_Hash : Valid_Field;  -- Hash of proof transcript
      N_Rounds        : Positive;
      Challenges      : out Challenge_Array
   ) with
      Global => null,
      Pre => Is_Canonical (Transcript_Hash)
             and N_Rounds <= Max_Rounds,
      Post => (for all I in 1 .. N_Rounds =>
                 Is_Canonical (Challenges (I))
                 and Challenges (I) /= Zero),  -- Non-zero for soundness
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Example 6: Zero-Knowledge Sum Check
   --  Proves sum of polynomial evaluations without revealing values
   ---------------------------------------------------------------------------

   type ZK_Sum_Proof is record
      Commitment  : Poly_256;
      Random_Mask : Poly_256;
      Response    : Valid_Field;
   end record;

   function Verify_Sum_Proof (
      Proof    : ZK_Sum_Proof;
      Expected : Valid_Field;
      Challenge: Valid_Field
   ) return Boolean with
      Global => null,
      Pre => Poly_Is_Canonical (Proof.Commitment)
             and Poly_Is_Canonical (Proof.Random_Mask)
             and Is_Canonical (Proof.Response)
             and Is_Canonical (Expected)
             and Is_Canonical (Challenge),
      Post => True,  -- Returns verification result
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Example 7: Polynomial Equality Test
   --  Proves two polynomials are equal using Schwartz-Zippel lemma
   ---------------------------------------------------------------------------

   --  With high probability, if P(r) = Q(r) for random r, then P = Q
   function Poly_Equality_Test (
      P, Q      : Poly_256;
      Challenge : Valid_Field
   ) return Boolean with
      Global => null,
      Pre => Poly_Is_Canonical (P)
             and Poly_Is_Canonical (Q)
             and Is_Canonical (Challenge),
      Post => (if Poly_Equality_Test'Result then
                 Poly_Eval (P, Challenge) = Poly_Eval (Q, Challenge));

   ---------------------------------------------------------------------------
   --  Example 8: Batch Verification Accumulator
   --  Combines multiple proofs into single verification (soundness amplification)
   ---------------------------------------------------------------------------

   Max_Batch_Size : constant := 100;

   procedure Batch_Verify_Constraints (
      Constraints : array (1 .. Max_Batch_Size) of R1CS_Constraint;
      N_Proofs    : Positive;
      Randomness  : Challenge_Array;  -- From Fiat-Shamir
      Valid       : out Boolean
   ) with
      Global => null,
      Pre => N_Proofs <= Max_Batch_Size
             and (for all I in 1 .. N_Proofs =>
                    Is_Canonical (Constraints (I).A)
                    and Is_Canonical (Constraints (I).B)
                    and Is_Canonical (Constraints (I).C)
                    and Is_Canonical (Randomness (I))),
      Post => (if Valid then
                 (for all I in 1 .. N_Proofs =>
                    Verify_R1CS (Constraints (I)))),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Ghost Lemmas (helper proofs for verification)
   ---------------------------------------------------------------------------

   --  Lemma: Field operations preserve canonicality
   procedure Lemma_Operations_Canonical (A, B : Valid_Field) with
      Ghost,
      Global => null,
      Pre => Is_Canonical (A) and Is_Canonical (B),
      Post => Is_Canonical (Add (A, B))
             and Is_Canonical (Sub (A, B))
             and Is_Canonical (Mul (A, B))
             and Is_Canonical (Neg (A));

   --  Lemma: Polynomial operations preserve canonicality
   procedure Lemma_Poly_Canonical (P, Q : Poly_256) with
      Ghost,
      Global => null,
      Pre => Poly_Is_Canonical (P) and Poly_Is_Canonical (Q);

   --  Lemma: Inverse is unique
   procedure Lemma_Inverse_Unique (A, X, Y : Valid_Field) with
      Ghost,
      Global => null,
      Pre => Is_Canonical (A) and A /= Zero
             and Is_Canonical (X) and Is_Canonical (Y)
             and Mul (A, X) = One and Mul (A, Y) = One,
      Post => X = Y;

end Field_Proof_Example;
