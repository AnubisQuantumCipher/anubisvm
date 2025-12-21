-------------------------------------------------------------------------------
--  ANUBIS VEIL - FRI Protocol (Fast Reed-Solomon IOP of Proximity)
--  Core component for STARK proof succinctness
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types;       use Anubis_Types;
with Anubis_STARK_Field; use Anubis_STARK_Field;
with Anubis_STARK_Poly;  use Anubis_STARK_Poly;

package Anubis_STARK_FRI with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  FRI Parameters
   ---------------------------------------------------------------------------

   --  Security level (bits)
   Security_Level : constant := 128;

   --  Folding factor (reduce degree by this factor each round)
   Folding_Factor : constant := 2;

   --  Number of queries per round
   Num_Queries : constant := 50;

   --  Maximum FRI rounds
   Max_FRI_Rounds : constant := 20;

   --  Final polynomial degree threshold
   Final_Degree : constant := 16;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Hash_Value is Byte_Array (0 .. 31);

   --  Named array types for SPARK compliance
   type Hash_Array is array (0 .. Max_FRI_Rounds - 1) of Hash_Value;
   type Alpha_Array is array (0 .. Max_FRI_Rounds - 1) of Field_Element;
   type Natural_Array is array (Natural range <>) of Natural;

   --  Merkle authentication path
   type Merkle_Path is record
      Siblings : Hash_Array;
      Length   : Natural;
   end record;

   --  FRI layer commitment
   type Layer_Commitment is record
      Root       : Hash_Value;
      Dom        : Anubis_STARK_Poly.Domain;
      Evals      : Anubis_STARK_Poly.Evaluations;
   end record;

   --  Named array type for layer commitments (0 .. Max_FRI_Rounds inclusive)
   type Layer_Array is array (0 .. Max_FRI_Rounds) of Layer_Commitment;

   --  Query response for a single layer
   type Query_Response is record
      Index      : Natural;
      Value      : Field_Element;
      Sibling    : Field_Element;  -- For folding verification
      Path       : Merkle_Path;
   end record;

   --  Named array type for query responses
   type Query_Response_Array is array (0 .. Max_FRI_Rounds - 1) of Query_Response;

   --  Full query (all layers)
   type Full_Query is record
      Responses  : Query_Response_Array;
      Num_Layers : Natural;
   end record;

   --  Named array type for queries
   type Full_Query_Array is array (0 .. Num_Queries - 1) of Full_Query;

   --  FRI Proof
   type FRI_Proof is record
      --  Layer commitments (Merkle roots)
      Commits        : Hash_Array;
      Num_Rounds     : Natural;

      --  Initial domain (for verification)
      Initial_Domain : Anubis_STARK_Poly.Domain;

      --  Final polynomial (small, sent in clear)
      Final_Poly     : Polynomial;

      --  Query responses
      Queries        : Full_Query_Array;

      --  Folding randomness (Fiat-Shamir)
      Alphas         : Alpha_Array;
   end record;

   --  FRI Prover state
   type FRI_Prover is record
      Layers         : Layer_Array;
      Num_Layers     : Natural;
      Initial_Domain : Anubis_STARK_Poly.Domain;
      Current_Poly   : Polynomial;
   end record;

   ---------------------------------------------------------------------------
   --  FRI Commit Phase
   ---------------------------------------------------------------------------

   --  Initialize FRI prover with polynomial
   procedure FRI_Init (
      Prover     : out FRI_Prover;
      Poly       : Polynomial;
      Dom        : Anubis_STARK_Poly.Domain;
      Blowup     : Positive
   ) with
      Global => null,
      Pre => Dom.Log_Size <= Anubis_STARK_Poly.Max_Log_Degree
             and then Blowup in 1 .. 16,
      Post => Prover.Num_Layers = 0,
      Always_Terminates;

   --  Commit to current layer, get root
   procedure FRI_Commit_Layer (
      Prover     : in out FRI_Prover;
      Root       : out Hash_Value
   ) with
      Global => null,
      Pre => Prover.Num_Layers <= Max_FRI_Rounds,
      Always_Terminates;

   --  Fold polynomial with random alpha
   procedure FRI_Fold (
      Prover     : in out FRI_Prover;
      Alpha      : Field_Element
   ) with
      Global => null,
      Pre => Prover.Num_Layers < Max_FRI_Rounds
             and then Prover.Layers (Prover.Num_Layers).Dom.Size >= 2,
      Post => Prover.Num_Layers = Prover.Num_Layers'Old + 1,
      Always_Terminates;

   --  Complete commit phase, return all roots and folding alphas
   procedure FRI_Complete_Commit (
      Prover     : in out FRI_Prover;
      Transcript : Byte_Array;  -- For Fiat-Shamir
      Roots      : out Hash_Array;
      Alphas     : out Alpha_Array;
      Num_Roots  : out Natural
   ) with
      Global => null,
      Pre => Transcript'Length > 0,
      Post => Num_Roots <= Max_FRI_Rounds,
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  FRI Query Phase
   ---------------------------------------------------------------------------

   --  Named array type for query indices
   type Query_Index_Array is array (0 .. Num_Queries - 1) of Natural;

   --  Generate query indices from transcript
   procedure FRI_Generate_Queries (
      Transcript   : Byte_Array;
      Domain_Size  : Natural;
      Indices      : out Query_Index_Array
   ) with
      Global => null,
      Pre => Transcript'Length > 0 and Domain_Size > 0,
      Post => (for all I in Indices'Range => Indices (I) < Domain_Size),
      Always_Terminates;

   --  Answer a single query
   procedure FRI_Answer_Query (
      Prover     : FRI_Prover;
      Index      : Natural;
      Response   : out Full_Query
   ) with
      Global => null,
      Pre => Prover.Num_Layers > 0
             and then Index < Prover.Layers (0).Dom.Size,
      Post => Response.Num_Layers = Prover.Num_Layers,
      Always_Terminates;

   --  Generate complete FRI proof
   procedure FRI_Prove (
      Poly       : Polynomial;
      Dom        : Anubis_STARK_Poly.Domain;
      Blowup     : Positive;
      Transcript : Byte_Array;
      Proof      : out FRI_Proof;
      Success    : out Boolean
   ) with
      Global => null,
      Pre => Transcript'Length > 0
             and then Dom.Log_Size <= Anubis_STARK_Poly.Max_Log_Degree
             and then Blowup in 1 .. 16,
      Post => (if Success then Proof.Num_Rounds <= Max_FRI_Rounds),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  FRI Verification
   ---------------------------------------------------------------------------

   --  Verify FRI proof
   function FRI_Verify (
      Proof      : FRI_Proof;
      Commitment : Hash_Value;  -- Initial commitment
      Transcript : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Transcript'Length > 0
             and then Proof.Num_Rounds <= Max_FRI_Rounds;

   --  Verify single query consistency
   function Verify_Query (
      Query        : Full_Query;
      Commits      : Hash_Array;
      Alphas       : Alpha_Array;
      Num_Rounds   : Natural;
      Initial_Dom  : Anubis_STARK_Poly.Domain;
      Final_Poly   : Polynomial
   ) return Boolean with
      Global => null,
      Pre => Num_Rounds <= Max_FRI_Rounds
             and then Initial_Dom.Log_Size <= Anubis_STARK_Poly.Max_Log_Degree;

   --  Verify Merkle path
   function Verify_Merkle_Path (
      Leaf       : Field_Element;
      Index      : Natural;
      Path       : Merkle_Path;
      Root       : Hash_Value
   ) return Boolean with
      Global => null,
      Pre => Path.Length <= Max_FRI_Rounds;

   ---------------------------------------------------------------------------
   --  Folding Operations
   ---------------------------------------------------------------------------

   --  Fold evaluation at index with its pair
   --  f'(x^2) = f_even(x^2) + alpha * f_odd(x^2)
   --  where f_even contains even coefficients, f_odd odd coefficients
   function Fold_Evaluations (
      F_X        : Field_Element;  -- f(x)
      F_Neg_X    : Field_Element;  -- f(-x)
      Alpha      : Field_Element
   ) return Field_Element with
      Global => null,
      Pre => Anubis_STARK_Field.Is_Canonical (F_X)
             and then Anubis_STARK_Field.Is_Canonical (F_Neg_X)
             and then Anubis_STARK_Field.Is_Canonical (Alpha),
      Post => Anubis_STARK_Field.Is_Canonical (Fold_Evaluations'Result);

   --  Compute folded domain
   function Fold_Domain (D : Anubis_STARK_Poly.Domain) return Anubis_STARK_Poly.Domain with
      Global => null,
      Pre => D.Log_Size > 0,
      Post => Fold_Domain'Result.Size = D.Size / 2
              and then Fold_Domain'Result.Log_Size = D.Log_Size - 1;

   ---------------------------------------------------------------------------
   --  Utility
   ---------------------------------------------------------------------------

   --  Compute number of FRI rounds needed
   function Compute_Num_Rounds (
      Degree : Natural;
      Final  : Natural := Final_Degree
   ) return Natural with
      Global => null,
      Pre => Final > 0,
      Post => Compute_Num_Rounds'Result <= Max_FRI_Rounds;

   --  Hash two field elements (for Merkle tree)
   function Hash_Pair (
      Left  : Field_Element;
      Right : Field_Element
   ) return Hash_Value with
      Global => null,
      Pre => Anubis_STARK_Field.Is_Canonical (Left)
             and then Anubis_STARK_Field.Is_Canonical (Right);

   --  Hash layer for commitment
   function Hash_Layer (
      Layer_Evals : Anubis_STARK_Poly.Evaluations
   ) return Hash_Value with
      Global => null,
      Pre => Layer_Evals.Count > 0
             and then Layer_Evals.Count <= Anubis_STARK_Poly.Max_Degree;

end Anubis_STARK_FRI;
