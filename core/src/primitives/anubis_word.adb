pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_Word with
   SPARK_Mode => On
is

   --  CT_Select: mask = 0 - Cond = all-1s if Cond=1, all-0s if Cond=0
   --  Then: (A and mask) or (B and not mask)
   function CT_Select (Cond : Word; A, B : Word) return Word is
      Mask : constant Word := 0 - Cond;  -- 0xFFFFFFFFFFFFFFFF or 0x0000000000000000
   begin
      return (A and Mask) or (B and not Mask);
   end CT_Select;

   --  CT_Eq: XOR A and B, then fold all bits down to LSB
   --  If any bit differs, result > 0, so NOT equal
   function CT_Eq (A, B : Word) return Word is
      X : Word := A xor B;
      X_U64 : Unsigned_64;
   begin
      --  Fold X down: if any bit is set, X > 0
      --  We fold 64 bits down to 1 bit in log2(64) = 6 steps
      X_U64 := Unsigned_64 (X);
      X_U64 := X_U64 or Shift_Right (X_U64, 32);
      X_U64 := X_U64 or Shift_Right (X_U64, 16);
      X_U64 := X_U64 or Shift_Right (X_U64, 8);
      X_U64 := X_U64 or Shift_Right (X_U64, 4);
      X_U64 := X_U64 or Shift_Right (X_U64, 2);
      X_U64 := X_U64 or Shift_Right (X_U64, 1);
      X := Word (X_U64);

      --  Now X"s LSB is 1 if any bit differed, 0 if all equal
      --  Return 1 - LSB to get: 1 if equal, 0 if not equal
      return 1 - (X and 1);
   end CT_Eq;

   --  CT_Lt: Correct constant-time unsigned less-than
   --  Uses the formula: lt = (A XOR ((A XOR B) OR ((A - B) XOR A))) >> 63
   --  This correctly handles unsigned comparison without branches
   function CT_Lt (A, B : Word) return Word is
      A_U64 : constant Unsigned_64 := Unsigned_64 (A);
      B_U64 : constant Unsigned_64 := Unsigned_64 (B);
      XOR_AB : constant Unsigned_64 := A_U64 xor B_U64;
      Diff : constant Unsigned_64 := A_U64 - B_U64;
      XOR_Diff_A : constant Unsigned_64 := Diff xor A_U64;
      Result_Bits : constant Unsigned_64 := A_U64 xor (XOR_AB or XOR_Diff_A);
   begin
      --  Extract bit 63: 1 if A < B, 0 otherwise
      return Word (Shift_Right (Result_Bits, 63));
   end CT_Lt;

   --  CT_Gt: A > B is equivalent to B < A
   function CT_Gt (A, B : Word) return Word is
   begin
      return CT_Lt (B, A);
   end CT_Gt;

   --  CT_Is_Zero: Use CT_Eq with 0
   function CT_Is_Zero (A : Word) return Word is
   begin
      return CT_Eq (A, 0);
   end CT_Is_Zero;

   --  CT_Byte: Extract byte at Index using shift and mask
   --  NO branch, NO table lookup
   function CT_Byte (W : Word; Index : Natural) return Word is
      Shift_Amount : constant Natural := Index * 8;
      W_U64 : constant Unsigned_64 := Unsigned_64 (W);
   begin
      return Word (Shift_Right (W_U64, Shift_Amount) and 16#FF#);
   end CT_Byte;

   --  CT_Swap: XOR-based swap trick
   --  If Cond=1: T = (A xor B) and 0xFF..FF = A xor B, so A := A xor T = B, B := B xor T = A
   --  If Cond=0: T = (A xor B) and 0x00..00 = 0, so A := A xor 0 = A, B := B xor 0 = B
   procedure CT_Swap (Cond : Word; A, B : in out Word) is
      Mask : constant Word := 0 - Cond;
      T : constant Word := (A xor B) and Mask;
   begin
      A := A xor T;
      B := B xor T;
   end CT_Swap;

   --  CT_Min: Use CT_Lt to select min without branching
   --  If A < B, return A, else return B
   function CT_Min (A, B : Word) return Word is
      Is_A_Less : constant Word := CT_Lt (A, B);
   begin
      return CT_Select (Is_A_Less, A, B);
   end CT_Min;

   --  CT_Max: Use CT_Gt to select max without branching
   --  If A > B, return A, else return B
   function CT_Max (A, B : Word) return Word is
      Is_A_Greater : constant Word := CT_Gt (A, B);
   begin
      return CT_Select (Is_A_Greater, A, B);
   end CT_Max;

   --  Rotate_Left: For modular types, use operators
   function Rotate_Left (Value : Word; Amount : Natural) return Word is
      Amt : constant Natural := Amount mod 64;
      Val_U64 : constant Unsigned_64 := Unsigned_64 (Value);
   begin
      if Amt = 0 then
         return Value;
      else
         return Word (Shift_Left (Val_U64, Amt) or Shift_Right (Val_U64, 64 - Amt));
      end if;
   end Rotate_Left;

   --  Rotate_Right: For modular types, use operators
   function Rotate_Right (Value : Word; Amount : Natural) return Word is
      Amt : constant Natural := Amount mod 64;
      Val_U64 : constant Unsigned_64 := Unsigned_64 (Value);
   begin
      if Amt = 0 then
         return Value;
      else
         return Word (Shift_Right (Val_U64, Amt) or Shift_Left (Val_U64, 64 - Amt));
      end if;
   end Rotate_Right;

end Anubis_Word;
