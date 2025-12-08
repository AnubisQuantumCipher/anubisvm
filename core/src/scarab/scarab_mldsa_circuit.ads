-------------------------------------------------------------------------------
--  SCARAB - ML-DSA-87 STARK Circuit
--  Arithmetization of ML-DSA-87 signature verification for STARK proofs
--
--  This enables aggregating 100+ ML-DSA signatures into a single ~150KB proof
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces;         use Interfaces;
with Anubis_Types;       use Anubis_Types;
with Anubis_STARK_Field; use Anubis_STARK_Field;
with Anubis_STARK_Poly;  use Anubis_STARK_Poly;

package Scarab_MLDSA_Circuit with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Circuit Parameters (ML-DSA-87)
   ---------------------------------------------------------------------------

   --  ML-DSA-87 parameters
   DSA_N          : constant := 256;    -- Polynomial degree
   DSA_Q          : constant := 8380417; -- Modulus
   DSA_K          : constant := 8;      -- Rows in A
   DSA_L          : constant := 7;      -- Columns in A
   DSA_Gamma1     : constant := 2 ** 19;
   DSA_Gamma2     : constant := (DSA_Q - 1) / 32;
   DSA_Beta       : constant := 120;
   DSA_Omega      : constant := 75;

   --  STARK trace parameters
   Trace_Width    : constant := 64;     -- Columns in execution trace
   Max_Trace_Len  : constant := 2 ** 20; -- 1M rows max

   ---------------------------------------------------------------------------
   --  Named Array Types (SPARK requires named types in records)
   ---------------------------------------------------------------------------

   --  AIR constraint term arrays
   type Term_Array is array (0 .. 15) of Field_Element;
   type Col_Index_Array is array (0 .. 15) of Natural;
   type Row_Offset_Array is array (0 .. 15) of Integer;

   ---------------------------------------------------------------------------
   --  AIR Constraint Types
   ---------------------------------------------------------------------------

   --  Algebraic Intermediate Representation (AIR) constraint
   type AIR_Constraint is record
      --  Constraint polynomial coefficients (sparse representation)
      Terms      : Term_Array;
      Col_Indices: Col_Index_Array;
      Row_Offsets: Row_Offset_Array;
      Num_Terms  : Natural;
      Degree     : Natural;
   end record;

   --  Constraint arrays for MLDSA
   type Boundary_Constraint_Array is array (0 .. 31) of AIR_Constraint;
   type Transition_Constraint_Array is array (0 .. 63) of AIR_Constraint;
   type Periodic_Constraint_Array is array (0 .. 15) of AIR_Constraint;

   --  Constraint system for ML-DSA verification
   type MLDSA_Constraints is record
      --  Boundary constraints (first/last row)
      Boundary     : Boundary_Constraint_Array;
      Num_Boundary : Natural;

      --  Transition constraints (between consecutive rows)
      Transition   : Transition_Constraint_Array;
      Num_Transition : Natural;

      --  Periodic constraints (for NTT butterflies)
      Periodic     : Periodic_Constraint_Array;
      Num_Periodic : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Execution Trace Types
   ---------------------------------------------------------------------------

   --  Single row of execution trace
   type Trace_Row is array (0 .. Trace_Width - 1) of Field_Element;

   --  Trace rows array (named type for SPARK)
   type Trace_Rows_Array is array (0 .. Max_Trace_Len - 1) of Trace_Row;

   --  Full execution trace
   type Execution_Trace is record
      Rows       : Trace_Rows_Array;
      Num_Rows   : Natural;
      Width      : Natural;
   end record;

   --  Witness auxiliary columns (named type for SPARK)
   type Aux_Cols_Array is array (0 .. Trace_Width - 1, 0 .. Max_Trace_Len - 1)
                          of Field_Element;

   --  Witness (auxiliary columns)
   type Witness is record
      Aux_Cols   : Aux_Cols_Array;
      Num_Aux    : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  ML-DSA Named Array Types (SPARK requires named types in records)
   ---------------------------------------------------------------------------

   --  Polynomial vector types
   type PK_T1_Array is array (0 .. DSA_K - 1, 0 .. DSA_N - 1) of Field_Element;
   type Z_Array is array (0 .. DSA_L - 1, 0 .. DSA_N - 1) of Field_Element;
   type Hint_Array is array (0 .. DSA_K - 1) of Unsigned_64;
   type Challenge_Poly is array (0 .. DSA_N - 1) of Field_Element;
   type A_Matrix is array (0 .. DSA_K - 1, 0 .. DSA_L - 1, 0 .. DSA_N - 1)
                    of Field_Element;
   type NTT_Temps_Array is array (0 .. 15, 0 .. DSA_N - 1) of Field_Element;
   type W_Prime_Array is array (0 .. DSA_K - 1, 0 .. DSA_N - 1) of Field_Element;

   ---------------------------------------------------------------------------
   --  ML-DSA Verification Circuit
   ---------------------------------------------------------------------------

   --  Input to ML-DSA verification circuit
   type MLDSA_Input is record
      --  Public key components (in NTT form)
      PK_Seed    : Byte_Array (0 .. 31);
      PK_T1      : PK_T1_Array;

      --  Message hash
      Mu         : Byte_Array (0 .. 63);

      --  Signature components
      C_Tilde    : Byte_Array (0 .. 31);
      Z          : Z_Array;
      Hint       : Hint_Array;
   end record;

   --  Circuit witness (private computation trace)
   type MLDSA_Witness is record
      --  Expanded challenge polynomial c
      C          : Challenge_Poly;

      --  Matrix A (generated from seed)
      A          : A_Matrix;

      --  Intermediate NTT values
      NTT_Temps  : NTT_Temps_Array;

      --  w" computation
      W_Prime    : W_Prime_Array;
   end record;

   ---------------------------------------------------------------------------
   --  Circuit Construction
   ---------------------------------------------------------------------------

   --  Build constraint system for ML-DSA-87 verification
   procedure Build_MLDSA_Constraints (
      Constraints : out MLDSA_Constraints
   ) with
      Global => null;

   --  Generate execution trace for ML-DSA verification
   procedure Generate_MLDSA_Trace (
      Input      : MLDSA_Input;
      Witness    : MLDSA_Witness;
      Trace      : out Execution_Trace;
      Success    : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  NTT Constraints (Core of ML-DSA)
   ---------------------------------------------------------------------------

   --  Array types for NTT layer parameters (named for SPARK)
   type Natural_Array is array (Natural range <>) of Natural;
   type Constraint_Array is array (Natural range <>) of AIR_Constraint;
   type Alpha_Array is array (Natural range <>) of Field_Element;
   type Poly_Array is array (Natural range <>) of Polynomial;

   --  Build NTT butterfly constraint
   function NTT_Butterfly_Constraint (
      In1_Col, In2_Col : Natural;
      Out1_Col, Out2_Col : Natural;
      Twiddle : Field_Element
   ) return AIR_Constraint with
      Global => null;

   --  Build full NTT layer constraints
   procedure NTT_Layer_Constraints (
      Layer      : Natural;
      Input_Cols : Natural_Array;
      Output_Cols: Natural_Array;
      Constraints: out Constraint_Array
   ) with
      Global => null,
      Pre => Input_Cols'Length = Output_Cols'Length;

   ---------------------------------------------------------------------------
   --  Polynomial Arithmetic Constraints
   ---------------------------------------------------------------------------

   --  Constraint for coefficient addition mod q
   function Coeff_Add_Constraint (
      A_Col, B_Col, Out_Col : Natural
   ) return AIR_Constraint with
      Global => null;

   --  Constraint for coefficient multiplication mod q
   function Coeff_Mul_Constraint (
      A_Col, B_Col, Out_Col : Natural
   ) return AIR_Constraint with
      Global => null;

   --  Constraint for range check (coefficient in valid range)
   function Range_Check_Constraint (
      Col : Natural;
      Max_Value : Field_Element
   ) return AIR_Constraint with
      Global => null;

   ---------------------------------------------------------------------------
   --  Batch Verification
   ---------------------------------------------------------------------------

   --  Batch arrays (named types for SPARK)
   type Batch_Inputs_Array is array (0 .. 127) of MLDSA_Input;
   type Batch_Witnesses_Array is array (0 .. 127) of MLDSA_Witness;

   --  Input for batch verification (multiple signatures)
   type Batch_Input is record
      Inputs     : Batch_Inputs_Array;
      Witnesses  : Batch_Witnesses_Array;
      Num_Sigs   : Natural;
   end record;

   --  Build batch verification circuit
   procedure Build_Batch_Circuit (
      Batch        : Batch_Input;
      Constraints  : out MLDSA_Constraints;
      Trace        : out Execution_Trace;
      Success      : out Boolean
   ) with
      Global => null;

   --  Estimate trace size for batch
   function Estimate_Trace_Size (Num_Sigs : Natural) return Natural with
      Global => null,
      Post => Estimate_Trace_Size'Result <= Max_Trace_Len;

   ---------------------------------------------------------------------------
   --  Constraint Evaluation
   ---------------------------------------------------------------------------

   --  Evaluate constraint at specific trace row
   function Eval_Constraint (
      Constraint : AIR_Constraint;
      Trace      : Execution_Trace;
      Row        : Natural
   ) return Field_Element with
      Global => null,
      Pre => Row < Trace.Num_Rows;

   --  Check if constraint is satisfied at all rows
   function Check_Constraint (
      Constraint : AIR_Constraint;
      Trace      : Execution_Trace
   ) return Boolean with
      Global => null;

   --  Check all constraints
   function Verify_Trace (
      Constraints : MLDSA_Constraints;
      Trace       : Execution_Trace
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Constraint Composition (for STARK)
   ---------------------------------------------------------------------------

   --  Compose constraints into single polynomial
   procedure Compose_Constraints (
      Constraints : MLDSA_Constraints;
      Alphas      : Alpha_Array;
      Composed    : out Polynomial
   ) with
      Global => null;

   --  Compute quotient polynomial
   procedure Compute_Quotient (
      Composed    : Polynomial;
      Trace_Poly  : Poly_Array;
      Domain      : Anubis_STARK_Poly.Domain;
      Quotient    : out Polynomial
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Integration with STARK Prover
   ---------------------------------------------------------------------------

   --  Byte array types for verification (named for SPARK)
   type Byte_Array_32 is array (0 .. 31) of Unsigned_8;
   type Public_Key_Array is array (Natural range <>) of Byte_Array_32;
   type Message_Array is array (Natural range <>) of Byte_Array_32;

   --  Generate full STARK proof for ML-DSA batch verification
   procedure Generate_MLDSA_Proof (
      Batch       : Batch_Input;
      Proof       : out Byte_Array;
      Proof_Len   : out Natural;
      Success     : out Boolean
   ) with
      Global => null;

   --  Verify STARK proof for ML-DSA batch
   function Verify_MLDSA_Proof (
      Public_Keys : Public_Key_Array;
      Messages    : Message_Array;
      Proof       : Byte_Array
   ) return Boolean with
      Global => null;

end Scarab_MLDSA_Circuit;
