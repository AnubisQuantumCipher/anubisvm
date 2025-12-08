-------------------------------------------------------------------------------
--  SCARAB - THOTH Verified AIR Compiler
--  Transforms High-level Operations To Homogeneous constraints
--
--  Compiles a verified DSL into Algebraic Intermediate Representation (AIR)
--  for STARK proofs. All constraint generation is formally verified.
--
--  Key Features:
--  - Type-safe constraint generation
--  - Automatic degree reduction
--  - Verified boundary constraint synthesis
--  - Memory-efficient trace layout
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;
with Anubis_STARK_Field; use Anubis_STARK_Field;

package Scarab_Thoth with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  AIR Parameters
   ---------------------------------------------------------------------------

   --  Maximum trace width (columns)
   Max_Trace_Width    : constant := 256;

   --  Maximum constraint degree
   Max_Constraint_Deg : constant := 8;

   --  Maximum number of constraints
   Max_Constraints    : constant := 1024;

   --  Maximum trace length (rows, power of 2)
   Max_Trace_Log_Len  : constant := 24;  -- 2^24 = 16M rows
   Max_Trace_Length   : constant := 2 ** Max_Trace_Log_Len;

   --  Maximum periodic columns
   Max_Periodic_Cols  : constant := 32;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Column_Index is Natural range 0 .. Max_Trace_Width - 1;
   subtype Row_Index is Natural range 0 .. Max_Trace_Length - 1;
   subtype Constraint_Index is Natural range 0 .. Max_Constraints - 1;
   subtype Degree_Range is Natural range 0 .. Max_Constraint_Deg;

   --  Register reference (column + row offset)
   type Register_Ref is record
      Column     : Column_Index;
      Row_Offset : Integer;  -- -1, 0, +1, etc.
   end record;

   --  Expression node types
   type Expr_Kind is (
      Expr_Const,      -- Field constant
      Expr_Register,   -- Trace register reference
      Expr_Periodic,   -- Periodic column value
      Expr_Add,        -- Addition
      Expr_Sub,        -- Subtraction
      Expr_Mul,        -- Multiplication
      Expr_Neg,        -- Negation
      Expr_Exp         -- Exponentiation (small exponent)
   );

   --  Expression AST (simplified for SPARK)
   Max_Expr_Nodes : constant := 256;
   subtype Expr_Node_Index is Natural range 0 .. Max_Expr_Nodes - 1;

   type Expr_Node is record
      Kind       : Expr_Kind;
      Value      : Field_Element;     -- For Expr_Const
      Reg        : Register_Ref;      -- For Expr_Register
      Periodic_ID: Natural;           -- For Expr_Periodic
      Left       : Expr_Node_Index;   -- For binary ops
      Right      : Expr_Node_Index;   -- For binary ops
      Exponent   : Natural;           -- For Expr_Exp
   end record;

   type Expr_Array is array (Expr_Node_Index) of Expr_Node;

   --  Expression tree
   type Expression is record
      Nodes      : Expr_Array;
      Root       : Expr_Node_Index;
      Node_Count : Natural;
      Degree     : Degree_Range;
   end record;

   --  Constraint kinds
   type Constraint_Kind is (
      Transition,    -- Must hold for all rows (except boundaries)
      Boundary,      -- Must hold at specific rows
      Periodic       -- Must hold at periodic intervals
   );

   --  Boundary specification
   type Boundary_Type is (
      First_Row,     -- Row 0
      Last_Row,      -- Row n-1
      Specific_Row   -- Specific row index
   );

   type Boundary_Spec is record
      Kind       : Boundary_Type;
      Row        : Row_Index;
   end record;

   --  Constraint definition
   type Constraint is record
      Kind       : Constraint_Kind;
      Expr       : Expression;        -- Must equal zero
      Boundary   : Boundary_Spec;     -- For boundary constraints
      Period     : Natural;           -- For periodic constraints
      Degree     : Degree_Range;
      Name       : String (1 .. 32);
      Name_Len   : Natural;
   end record;

   type Constraint_Array is array (Constraint_Index) of Constraint;

   --  Periodic column
   type Periodic_Column is record
      ID         : Natural;
      Values     : Field_Array (0 .. 1023);  -- Period values
      Period     : Natural;
      Active     : Boolean;
   end record;

   type Periodic_Array is array (0 .. Max_Periodic_Cols - 1) of Periodic_Column;

   --  Trace layout
   type Column_Type is (
      Main,          -- Main execution trace
      Auxiliary,     -- Auxiliary columns for degree reduction
      Public_Input,  -- Public input columns
      Random         -- Randomness columns (for ZK)
   );

   type Column_Def is record
      Index      : Column_Index;
      Col_Type   : Column_Type;
      Name       : String (1 .. 16);
      Name_Len   : Natural;
   end record;

   type Column_Def_Array is array (Column_Index) of Column_Def;

   --  Complete AIR definition
   type AIR_Definition is record
      --  Trace structure
      Num_Columns    : Natural;
      Columns        : Column_Def_Array;
      Trace_Length   : Natural;
      Log_Trace_Len  : Natural;

      --  Constraints
      Num_Constraints: Natural;
      Constraints    : Constraint_Array;

      --  Periodic columns
      Num_Periodic   : Natural;
      Periodics      : Periodic_Array;

      --  Metadata
      Max_Degree     : Degree_Range;
      Blowup_Factor  : Positive;
   end record;

   ---------------------------------------------------------------------------
   --  Expression Builder
   ---------------------------------------------------------------------------

   --  Create constant expression
   function Const_Expr (Value : Field_Element) return Expression with
      Global => null,
      Post => Const_Expr'Result.Degree = 0;

   --  Create register reference expression
   function Reg_Expr (Col : Column_Index; Offset : Integer := 0) return Expression with
      Global => null,
      Pre => Offset in -16 .. 16,
      Post => Reg_Expr'Result.Degree = 1;

   --  Create periodic column reference
   function Periodic_Expr (Periodic_ID : Natural) return Expression with
      Global => null,
      Post => Periodic_Expr'Result.Degree = 0;

   --  Add two expressions
   function Add_Expr (A, B : Expression) return Expression with
      Global => null,
      Post => Add_Expr'Result.Degree = Natural'Max (A.Degree, B.Degree);

   --  Subtract expressions
   function Sub_Expr (A, B : Expression) return Expression with
      Global => null,
      Post => Sub_Expr'Result.Degree = Natural'Max (A.Degree, B.Degree);

   --  Multiply expressions
   function Mul_Expr (A, B : Expression) return Expression with
      Global => null,
      Pre => A.Degree + B.Degree <= Max_Constraint_Deg,
      Post => Mul_Expr'Result.Degree = A.Degree + B.Degree;

   --  Negate expression
   function Neg_Expr (E : Expression) return Expression with
      Global => null,
      Post => Neg_Expr'Result.Degree = E.Degree;

   --  Exponentiate expression (small exponent)
   function Exp_Expr (E : Expression; N : Natural) return Expression with
      Global => null,
      Pre => E.Degree * N <= Max_Constraint_Deg and N <= 8,
      Post => Exp_Expr'Result.Degree = E.Degree * N;

   ---------------------------------------------------------------------------
   --  AIR Builder
   ---------------------------------------------------------------------------

   --  Initialize empty AIR
   procedure Init_AIR (
      AIR            : out AIR_Definition;
      Trace_Log_Len  : Natural;
      Blowup         : Positive
   ) with
      Global => null,
      Pre => Trace_Log_Len <= Max_Trace_Log_Len and Blowup in 2 .. 16,
      Post => AIR.Num_Columns = 0 and AIR.Num_Constraints = 0;

   --  Add column to AIR
   procedure Add_Column (
      AIR            : in out AIR_Definition;
      Col_Type       : Column_Type;
      Name           : String;
      Index          : out Column_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Name'Length <= 16,
      Post => (if Success then AIR.Num_Columns = AIR.Num_Columns'Old + 1);

   --  Add transition constraint
   procedure Add_Transition_Constraint (
      AIR            : in Out AIR_Definition;
      Expr           : Expression;
      Name           : String;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Name'Length <= 32,
      Post => (if Success then AIR.Num_Constraints = AIR.Num_Constraints'Old + 1);

   --  Add boundary constraint
   procedure Add_Boundary_Constraint (
      AIR            : in out AIR_Definition;
      Expr           : Expression;
      Boundary       : Boundary_Spec;
      Name           : String;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Name'Length <= 32,
      Post => (if Success then AIR.Num_Constraints = AIR.Num_Constraints'Old + 1);

   --  Add periodic column
   procedure Add_Periodic_Column (
      AIR            : in Out AIR_Definition;
      Values         : Field_Array;
      Period         : Natural;
      ID             : out Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Values'Length <= 1024 and Period > 0,
      Post => (if Success then AIR.Num_Periodic = AIR.Num_Periodic'Old + 1);

   ---------------------------------------------------------------------------
   --  Standard Gadgets (Verified Building Blocks)
   ---------------------------------------------------------------------------

   --  Boolean constraint: x * (1 - x) = 0
   function Boolean_Gadget (Col : Column_Index) return Expression with
      Global => null,
      Post => Boolean_Gadget'Result.Degree = 2;

   --  Range check: x in [0, 2^n - 1] via bit decomposition
   procedure Range_Check_Gadget (
      AIR            : in Out AIR_Definition;
      Value_Col      : Column_Index;
      Bit_Cols       : Column_Index;  -- Starting column for bits
      Num_Bits       : Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Num_Bits in 1 .. 64;

   --  Conditional select: result = cond * a + (1 - cond) * b
   function Conditional_Gadget (
      Cond_Col       : Column_Index;
      A_Col          : Column_Index;
      B_Col          : Column_Index;
      Result_Col     : Column_Index
   ) return Expression with
      Global => null,
      Post => Conditional_Gadget'Result.Degree = 2;

   --  Memory read/write consistency
   procedure Memory_Gadget (
      AIR            : in Out AIR_Definition;
      Addr_Col       : Column_Index;
      Value_Col      : Column_Index;
      Time_Col       : Column_Index;
      Is_Write_Col   : Column_Index;
      Success        : out Boolean
   ) with
      Global => null;

   --  Hash chain constraint
   procedure Hash_Chain_Gadget (
      AIR            : in Out AIR_Definition;
      Input_Col      : Column_Index;
      Output_Col     : Column_Index;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Degree Reduction
   ---------------------------------------------------------------------------

   --  Reduce high-degree constraint using auxiliary columns
   procedure Reduce_Degree (
      AIR            : in Out AIR_Definition;
      Constraint_Idx : Constraint_Index;
      Target_Degree  : Degree_Range;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Target_Degree >= 2;

   --  Automatically reduce all constraints to max degree
   procedure Auto_Reduce_All (
      AIR            : in Out AIR_Definition;
      Max_Degree     : Degree_Range;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Max_Degree >= 2;

   ---------------------------------------------------------------------------
   --  Compilation
   ---------------------------------------------------------------------------

   type Compilation_Error is (
      No_Error,
      Degree_Too_High,
      Too_Many_Columns,
      Too_Many_Constraints,
      Invalid_Reference,
      Cyclic_Dependency,
      Out_Of_Memory
   );

   type Compilation_Result is record
      Success        : Boolean;
      Error          : Compilation_Error;
      Error_Details  : String (1 .. 128);
      Detail_Len     : Natural;
   end record;

   --  Compile AIR definition (verify and optimize)
   procedure Compile_AIR (
      AIR            : in Out AIR_Definition;
      Result         : out Compilation_Result
   ) with
      Global => null;

   --  Verify AIR is well-formed
   function Verify_AIR (AIR : AIR_Definition) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Trace Generation Helpers
   ---------------------------------------------------------------------------

   --  Evaluate expression at specific row
   function Eval_Expr (
      Expr           : Expression;
      Trace          : Field_Array;
      Trace_Width    : Natural;
      Row            : Row_Index;
      Periodics      : Periodic_Array
   ) return Field_Element with
      Global => null,
      Pre => Trace'Length = Trace_Width * Max_Trace_Length
             and Trace_Width <= Max_Trace_Width;

   --  Check if constraint is satisfied at row
   function Check_Constraint (
      C              : Constraint;
      Trace          : Field_Array;
      Trace_Width    : Natural;
      Row            : Row_Index;
      Trace_Length   : Natural;
      Periodics      : Periodic_Array
   ) return Boolean with
      Global => null;

   --  Verify entire trace satisfies AIR
   function Verify_Trace (
      AIR            : AIR_Definition;
      Trace          : Field_Array
   ) return Boolean with
      Global => null,
      Pre => Trace'Length = AIR.Num_Columns * AIR.Trace_Length;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize AIR to bytes
   procedure Serialize_AIR (
      AIR            : AIR_Definition;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 1024 * 1024;  -- 1MB buffer

   --  Deserialize AIR from bytes
   procedure Deserialize_AIR (
      Input          : Byte_Array;
      AIR            : out AIR_Definition;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Debugging
   ---------------------------------------------------------------------------

   --  Get constraint as human-readable string
   procedure Constraint_To_String (
      C              : Constraint;
      Output         : out String;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 256;

   --  Dump AIR statistics
   type AIR_Stats is record
      Num_Main_Cols     : Natural;
      Num_Aux_Cols      : Natural;
      Num_Transition    : Natural;
      Num_Boundary      : Natural;
      Max_Degree        : Natural;
      Est_Proof_Size    : Natural;  -- Bytes
   end record;

   function Get_AIR_Stats (AIR : AIR_Definition) return AIR_Stats with
      Global => null;

end Scarab_Thoth;
