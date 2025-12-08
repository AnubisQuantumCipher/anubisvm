pragma SPARK_Mode (On);

package Anubis_Word with
   Pure,
   SPARK_Mode => On
is
   --  Constant-Time 64-bit Word Operations
   --
   --  CRITICAL: NO BRANCHES ON SECRET DATA
   --
   --  All operations here are timing-invariant. They use only:
   --  - Arithmetic operations
   --  - Bitwise operations
   --  - Shifts/rotates
   --
   --  NO:
   --  - Conditional branches (if/case on secrets)
   --  - Table lookups with secret indices
   --  - Variable-time operations

   type Word is mod 2**64 with Size => 64;
   type Word_Array is array (Natural range <>) of Word;

   --  Constant-time conditional selection
   --  Returns A if Cond=1, B if Cond=0
   --  NO BRANCH - pure arithmetic masking
   function CT_Select (Cond : Word; A, B : Word) return Word with
      Inline_Always,
      Pre => Cond in 0 | 1,
      Post => (if Cond = 1 then CT_Select'Result = A else CT_Select'Result = B);

   --  Constant-time equality test
   --  Returns 1 if A = B, 0 otherwise
   function CT_Eq (A, B : Word) return Word with
      Inline_Always,
      Post => CT_Eq'Result in 0 | 1 and then
              (CT_Eq'Result = 1) = (A = B);

   --  Constant-time less-than
   --  Returns 1 if A < B, 0 otherwise
   function CT_Lt (A, B : Word) return Word with
      Inline_Always,
      Post => CT_Lt'Result in 0 | 1 and then
              (CT_Lt'Result = 1) = (A < B);

   --  Constant-time greater-than
   --  Returns 1 if A > B, 0 otherwise
   function CT_Gt (A, B : Word) return Word with
      Inline_Always,
      Post => CT_Gt'Result in 0 | 1 and then
              (CT_Gt'Result = 1) = (A > B);

   --  Constant-time is-zero test
   --  Returns 1 if A = 0, 0 otherwise
   function CT_Is_Zero (A : Word) return Word with
      Inline_Always,
      Post => CT_Is_Zero'Result in 0 | 1 and then
              (CT_Is_Zero'Result = 1) = (A = 0);

   --  Constant-time byte extraction
   --  Returns byte at Index (0..7) from word
   --  NO TABLE LOOKUP - arithmetic only
   function CT_Byte (W : Word; Index : Natural) return Word with
      Inline_Always,
      Pre => Index < 8,
      Post => CT_Byte'Result < 256;

   --  Constant-time conditional swap
   --  If Cond=1, swap A and B; if Cond=0, no change
   procedure CT_Swap (Cond : Word; A, B : in out Word) with
      Inline_Always,
      Pre => Cond in 0 | 1,
      Post => (if Cond = 1 then (A = B'Old and B = A'Old)
               else (A = A'Old and B = B'Old));

   --  Constant-time minimum
   --  Returns min(A, B) without branching
   function CT_Min (A, B : Word) return Word with
      Inline_Always,
      Post => (CT_Min'Result = A or CT_Min'Result = B) and then
              CT_Min'Result <= A and then
              CT_Min'Result <= B;

   --  Constant-time maximum
   --  Returns max(A, B) without branching
   function CT_Max (A, B : Word) return Word with
      Inline_Always,
      Post => (CT_Max'Result = A or CT_Max'Result = B) and then
              CT_Max'Result >= A and then
              CT_Max'Result >= B;

   --  Rotate left
   function Rotate_Left (Value : Word; Amount : Natural) return Word with
      Inline_Always,
      Pre => Amount < 64;

   --  Rotate right
   function Rotate_Right (Value : Word; Amount : Natural) return Word with
      Inline_Always,
      Pre => Amount < 64;

end Anubis_Word;
