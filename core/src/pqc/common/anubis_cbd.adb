pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_CBD with
   SPARK_Mode => On
is

   --  Constant-time population count for 8 bits
   --  Counts number of 1s in the byte
   function Popcount_8 (X : Byte) return Natural is
      --  Use SWAR (SIMD Within A Register) technique
      V : Unsigned_8 := X;
   begin
      --  Pairwise sum of bits
      V := (V and 16#55#) + (Shift_Right (V, 1) and 16#55#);
      --  Sum of nibbles
      V := (V and 16#33#) + (Shift_Right (V, 2) and 16#33#);
      --  Sum of bytes (upper nibble + lower nibble)
      V := (V and 16#0F#) + Shift_Right (V, 4);

      return Natural (V);
   end Popcount_8;

   --  CBD with η = 2 (FIPS 203 Algorithm 7)
   --  For each coefficient i:
   --  - x = sum of bits at positions 4i and 4i+1
   --  - y = sum of bits at positions 4i+2 and 4i+3
   --  - coefficient[i] = x - y (result in {-2, -1, 0, 1, 2})
   --
   --  Layout: 4 bits per coefficient, 2 coefficients per byte
   --  Byte 0: coef 0 (bits 0-3), coef 1 (bits 4-7)
   --  Byte 1: coef 2 (bits 0-3), coef 3 (bits 4-7)
   --  ...continuing for 128 bytes → 256 coefficients
   procedure CBD_2 (
      Input : CBD_Input_2;
      Poly  : out Polynomial
   ) is
      B : Unsigned_8;
      X, Y : Integer;
   begin
      --  Process 1 byte per 2 coefficients (128 bytes → 256 coefficients)
      for I in 0 .. 127 loop
         pragma Loop_Invariant (I in 0 .. 127);

         B := Input (I);

         --  Coefficient 2*I: bits [0:1] for x, bits [2:3] for y
         X := Popcount_8 (B and 16#03#);                        -- bits 0,1
         Y := Popcount_8 (Shift_Right (B, 2) and 16#03#);       -- bits 2,3
         Poly (2 * I) := Field_Element ((X - Y + Q) mod Q);

         --  Coefficient 2*I+1: bits [4:5] for x, bits [6:7] for y
         X := Popcount_8 (Shift_Right (B, 4) and 16#03#);       -- bits 4,5
         Y := Popcount_8 (Shift_Right (B, 6) and 16#03#);       -- bits 6,7
         Poly (2 * I + 1) := Field_Element ((X - Y + Q) mod Q);
      end loop;
   end CBD_2;

   --  CBD with η = 3
   --  For each coefficient:
   --  - Take 3 bits (a0, a1, a2) and 3 bits (b0, b1, b2)
   --  - Compute (a0 + a1 + a2) - (b0 + b1 + b2)
   --  - Result in {-3, -2, -1, 0, 1, 2, 3}
   procedure CBD_3 (
      Input : CBD_Input_3;
      Poly  : out Polynomial
   ) is
      Buf : array (0 .. 23) of Unsigned_8;
      A, B : Unsigned_32;
      T : Integer;
      Idx : Natural;
   begin
      --  Process in blocks of 32 coefficients (96 bytes)
      for Block in 0 .. 7 loop
         pragma Loop_Invariant (Block in 0 .. 7);

         --  Load 24 bytes for this block
         for J in 0 .. 23 loop
            pragma Loop_Invariant (J in 0 .. 23);
            Buf (J) := Input (Block * 24 + J);
         end loop;

         --  Process 4 coefficients at a time
         for Chunk in 0 .. 7 loop
            pragma Loop_Invariant (Chunk in 0 .. 7);

            Idx := Chunk * 3;

            --  Extract 24 bits: 12 bits for A, 12 bits for B
            A := Unsigned_32 (Buf (Idx)) or
                 Shift_Left (Unsigned_32 (Buf (Idx + 1)), 8) or
                 (Shift_Left (Unsigned_32 (Buf (Idx + 2)), 16) and 16#F0000#);

            B := Shift_Right (Unsigned_32 (Buf (Idx + 2)), 4) or
                 Shift_Left (Unsigned_32 (Buf (Idx + 3)), 4);

            --  Extract 4 coefficients
            for K in 0 .. 3 loop
               pragma Loop_Invariant (K in 0 .. 3);
               pragma Loop_Invariant (Block in 0 .. 7);
               pragma Loop_Invariant (Chunk in 0 .. 7);

               declare
                  Coef_Idx : constant Natural := Block * 32 + Chunk * 4 + K;
                  Shift_Amount : constant Natural := K * 3;
                  Mask : constant Unsigned_32 := 16#07#;  -- 3 bits
                  A_Bits : constant Unsigned_32 := Shift_Right (A, Shift_Amount) and Mask;
                  B_Bits : constant Unsigned_32 := Shift_Right (B, Shift_Amount) and Mask;
               begin
                  --  Count bits in each 3-bit value
                  T := Integer (Popcount_8 (Byte (A_Bits))) -
                       Integer (Popcount_8 (Byte (B_Bits)));
                  Poly (Coef_Idx) := Field_Element ((T + Q) mod Q);
               end;
            end loop;
         end loop;
      end loop;
   end CBD_3;

end Anubis_CBD;
