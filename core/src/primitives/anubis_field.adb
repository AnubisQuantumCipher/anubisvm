pragma SPARK_Mode (On);

package body Anubis_Field with
   SPARK_Mode => On
is

   --  Reduce: Conditional subtraction
   --  If A >= Q, return A - Q; else return A
   --  This is constant-time via masking
   function Reduce (A : Field_Element) return Valid_Field is
      --  Compute A - Q
      Diff : constant Field_Element := A - Q;

      --  Extract the borrow bit (sign bit at position 15)
      --  If A >= Q, no borrow (Borrow = 0); if A < Q, borrow (Borrow = 1)
      Borrow : constant Field_Element :=
         Field_Element (Shift_Right (Unsigned_16 (Diff), 15));

      --  Mask: 0xFFFF if borrow, 0x0000 if no borrow
      Mask : constant Field_Element := 0 - Borrow;
   begin
      --  If A >= Q: select Diff (A - Q)
      --  If A < Q: select A
      return Valid_Field ((A and Mask) or (Diff and not Mask));
   end Reduce;

   --  Barrett_Reduce: Fast modular reduction using Barrett"s method
   --  For input A < 2^24, computes A mod Q
   function Barrett_Reduce (A : Unsigned_32) return Valid_Field is
      --  Compute quotient estimate: q = floor((A * m) / 2^26)
      --  where m = floor(2^26 / Q) = 20159
      --  Use Unsigned_64 to avoid overflow in multiplication
      Q_Est : constant Unsigned_32 :=
         Unsigned_32 (Shift_Right (Unsigned_64 (A) * Unsigned_64 (Barrett_Const), Barrett_Shift));

      --  Compute remainder: r = A - q * Q
      --  Use modular arithmetic: if subtraction underflows, it wraps in 64-bit space
      --  We mask to get the lower 32 bits which gives us the correct result mod 2^32
      R : constant Unsigned_32 :=
         Unsigned_32 ((Unsigned_64 (A) - Unsigned_64 (Q_Est) * Unsigned_64 (Q)) and 16#FFFFFFFF#);

      --  R can be larger than Q due to quotient underestimate
      --  Use constant-time iterative reduction
      R_Reduced : Unsigned_32 := R;

      --  Helper function for constant-time conditional subtraction
      function CT_Sub_If_GE_Q (Val : Unsigned_32) return Unsigned_32 is
         Diff : constant Unsigned_32 := Val - Q;
         --  If Val >= Q, Diff won"t borrow, so high bit is 0
         --  If Val < Q, Diff borrows, high bit is 1
         Is_Less : constant Unsigned_32 := Shift_Right (Diff, 31);
         Mask : constant Unsigned_32 := 0 - Is_Less;  -- All 1s if Val < Q, all 0s otherwise
      begin
         --  Return Val if Val < Q, else return Diff
         return (Val and Mask) or (Diff and not Mask);
      end CT_Sub_If_GE_Q;
   begin
      --  Apply multiple constant-time reductions
      --  Maximum R ≈ 11*Q for A < 2^24, so 15 iterations to guarantee < Q
      --  (Extra iterations to handle edge cases)
      for I in 1 .. 15 loop
         pragma Loop_Invariant (R_Reduced <= R);
         R_Reduced := CT_Sub_If_GE_Q (R_Reduced);
      end loop;

      return Valid_Field (R_Reduced);
   end Barrett_Reduce;

   --  Add: (A + B) mod Q
   function Add (A, B : Valid_Field) return Valid_Field is
      Sum : constant Field_Element := Field_Element (A) + Field_Element (B);
   begin
      return Reduce (Sum);
   end Add;

   --  Sub: (A - B) mod Q
   --  Add Q first to avoid underflow, then reduce
   function Sub (A, B : Valid_Field) return Valid_Field is
      Diff : constant Field_Element := Field_Element (A) + (Q - Field_Element (B));
   begin
      return Reduce (Diff);
   end Sub;

   --  Mul: (A * B) mod Q using Barrett reduction
   function Mul (A, B : Valid_Field) return Valid_Field is
      Product : constant Unsigned_32 := Unsigned_32 (A) * Unsigned_32 (B);
   begin
      --  Product < Q^2 < 2^24, so Barrett reduction is valid
      return Barrett_Reduce (Product);
   end Mul;

   --  To_Mont: Convert to Montgomery form
   --  Result = (A * R) mod Q
   function To_Mont (A : Valid_Field) return Field_Element is
      Product : constant Unsigned_32 := Unsigned_32 (A) * Mont_R;
   begin
      return Field_Element (Barrett_Reduce (Product));
   end To_Mont;

   --  From_Mont: Convert from Montgomery form
   --  Result = (A * R^(-1)) mod Q
   function From_Mont (A : Field_Element) return Valid_Field is
      Product : constant Unsigned_32 := Unsigned_32 (A) * Mont_R_Inv;
   begin
      return Barrett_Reduce (Product);
   end From_Mont;

   --  Mont_Reduce: Montgomery reduction
   --  Computes (A * R^(-1)) mod Q
   --  Uses the REDC algorithm
   function Mont_Reduce (A : Unsigned_32) return Field_Element is
      --  m = (A * Q_Inv) mod R (where R = 2^16)
      M : constant Unsigned_32 := (A * Mont_Q_Inv) and 16#FFFF#;

      --  t = (A + m * Q) / R
      --  This division by R is just a right shift by 16 bits
      T : constant Unsigned_32 := (A + M * Q) / 2**16;

      --  T can be much larger than Q, need constant-time iterative reduction
      T_Reduced : Unsigned_32 := T;

      --  Helper function for constant-time conditional subtraction
      function CT_Sub_If_GE_Q (Val : Unsigned_32) return Unsigned_32 with
         Inline_Always
      is
         Diff : constant Unsigned_32 := Val - Q;
         Is_Less : constant Unsigned_32 := Shift_Right (Diff, 31);
         Mask : constant Unsigned_32 := 0 - Is_Less;
      begin
         return (Val and Mask) or (Diff and not Mask);
      end CT_Sub_If_GE_Q;
   begin
      --  Apply constant-time reductions
      --  Maximum T ≈ 20*Q, so 22 iterations to guarantee < Q (not <= Q)
      for I in 1 .. 22 loop
         pragma Loop_Invariant (T_Reduced <= T);
         T_Reduced := CT_Sub_If_GE_Q (T_Reduced);
      end loop;

      --  Now T_Reduced < Q (not equal)
      return Field_Element (T_Reduced);
   end Mont_Reduce;

   --  Mont_Mul: Montgomery multiplication
   --  Computes (A * B * R^(-1)) mod Q
   function Mont_Mul (A, B : Field_Element) return Field_Element is
      Product : constant Unsigned_32 := Unsigned_32 (A) * Unsigned_32 (B);
   begin
      return Mont_Reduce (Product);
   end Mont_Mul;

   --  CT_Select: Constant-time conditional selection
   function CT_Select (Cond : Field_Element; A, B : Field_Element) return Field_Element is
      Mask : constant Field_Element := 0 - Cond;
   begin
      return (A and Mask) or (B and not Mask);
   end CT_Select;

end Anubis_Field;
