pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;

package body Anubis_MLKEM_Encoding with
   SPARK_Mode => On
is

   --  ByteEncode_12: Pack 256 12-bit coefficients into 384 bytes
   --  Groups of 2 coefficients (24 bits) → 3 bytes
   procedure ByteEncode_12 (
      F      : in  Polynomial;
      Output : out Encoded_Poly
   ) is
      Idx : Natural := 0;
      C0, C1 : Unsigned_16;
   begin
      --  Process 2 coefficients at a time
      --  Encoded_Poly has 384 bytes (0..383), so Idx ranges 0..381 (I*3)
      for I in 0 .. 127 loop
         pragma Loop_Invariant (I in 0 .. 127);
         pragma Loop_Invariant (Idx = I * 3);
         pragma Loop_Invariant (Idx <= 381);
         pragma Loop_Invariant (if Idx > 0 then
            (for all K in 0 .. Idx - 1 => Output (K)'Initialized));

         --  Mask to 12 bits (ByteEncode_12 expects coefficients < 4096)
         C0 := Unsigned_16 (F (2 * I)) and 16#FFF#;
         C1 := Unsigned_16 (F (2 * I + 1)) and 16#FFF#;

         --  Pack 24 bits into 3 bytes (little-endian)
         --  Idx + 2 <= 383 is always valid since Idx <= 381
         pragma Assert (Idx + 2 <= 383);
         pragma Assert (C0 <= 4095 and C1 <= 4095);
         pragma Assert (Shift_Right (C1, 4) <= 255);
         Output (Idx)     := Byte (C0 and 16#FF#);
         Output (Idx + 1) := Byte (Shift_Right (C0, 8) or Shift_Left (C1 and 16#F#, 4));
         Output (Idx + 2) := Byte (Shift_Right (C1, 4));

         Idx := Idx + 3;
      end loop;
   end ByteEncode_12;

   --  ByteDecode_12: Unpack 384 bytes to 256 12-bit coefficients
   procedure ByteDecode_12 (
      Input : in  Encoded_Poly;
      F     : out Polynomial
   ) is
      Idx : Natural := 0;
      B0, B1, B2 : Unsigned_16;
      Val : Unsigned_16;
   begin
      --  Process 3 bytes at a time → 2 coefficients
      for I in 0 .. 127 loop
         pragma Loop_Invariant (I in 0 .. 127);
         pragma Loop_Invariant (Idx = I * 3);
         pragma Loop_Invariant (for all K in 0 .. 2 * I - 1 =>
            F (K)'Initialized and F (K) < Q);

         B0 := Unsigned_16 (Input (Idx));
         B1 := Unsigned_16 (Input (Idx + 1));
         B2 := Unsigned_16 (Input (Idx + 2));

         --  Extract 12-bit coefficients
         Val := B0 or Shift_Left (B1 and 16#0F#, 8);
         F (2 * I) := Field_Element (Val mod Q);

         Val := Shift_Right (B1, 4) or Shift_Left (B2, 4);
         F (2 * I + 1) := Field_Element (Val mod Q);

         Idx := Idx + 3;
      end loop;
   end ByteDecode_12;

   --  ByteEncode_11: Pack 256 11-bit coefficients into 352 bytes
   --  Groups of 8 coefficients (88 bits) → 11 bytes
   procedure ByteEncode_11 (
      F      : in  Polynomial;
      Output : out Compressed_Poly_Du
   ) is
      Idx : Natural := 0;
      C : array (0 .. 7) of Unsigned_16;
   begin
      --  Process 8 coefficients at a time
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I in 0 .. 31);
         pragma Loop_Invariant (Idx = I * 11);
         pragma Loop_Invariant (for all K in 0 .. Idx - 1 => Output (K)'Initialized);

         for J in 0 .. 7 loop
            pragma Loop_Invariant (J in 0 .. 7);
            C (J) := Unsigned_16 (F (8 * I + J) and 16#7FF#);
         end loop;

         --  Pack 88 bits into 11 bytes (mask all results to 8 bits)
         Output (Idx)      := Byte (C (0) and 16#FF#);
         Output (Idx + 1)  := Byte ((Shift_Right (C (0), 8) or Shift_Left (C (1), 3)) and 16#FF#);
         Output (Idx + 2)  := Byte ((Shift_Right (C (1), 5) or Shift_Left (C (2), 6)) and 16#FF#);
         Output (Idx + 3)  := Byte (Shift_Right (C (2), 2) and 16#FF#);
         Output (Idx + 4)  := Byte ((Shift_Right (C (2), 10) or Shift_Left (C (3), 1)) and 16#FF#);
         Output (Idx + 5)  := Byte ((Shift_Right (C (3), 7) or Shift_Left (C (4), 4)) and 16#FF#);
         Output (Idx + 6)  := Byte ((Shift_Right (C (4), 4) or Shift_Left (C (5), 7)) and 16#FF#);
         Output (Idx + 7)  := Byte (Shift_Right (C (5), 1) and 16#FF#);
         Output (Idx + 8)  := Byte ((Shift_Right (C (5), 9) or Shift_Left (C (6), 2)) and 16#FF#);
         Output (Idx + 9)  := Byte ((Shift_Right (C (6), 6) or Shift_Left (C (7), 5)) and 16#FF#);
         Output (Idx + 10) := Byte (Shift_Right (C (7), 3) and 16#FF#);

         Idx := Idx + 11;
      end loop;
   end ByteEncode_11;

   --  ByteDecode_11: Unpack 352 bytes to 256 11-bit coefficients
   procedure ByteDecode_11 (
      Input : in  Compressed_Poly_Du;
      F     : out Polynomial
   ) is
      Idx : Natural := 0;
      B : array (0 .. 10) of Unsigned_16;
   begin
      --  Process 11 bytes at a time → 8 coefficients
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I in 0 .. 31);
         pragma Loop_Invariant (Idx = I * 11);
         pragma Loop_Invariant (for all K in 0 .. 8 * I - 1 =>
            F (K)'Initialized and F (K) < 2**Du);

         for J in 0 .. 10 loop
            pragma Loop_Invariant (J in 0 .. 10);
            B (J) := Unsigned_16 (Input (Idx + J));
         end loop;

         --  Extract 8 11-bit coefficients (all masked with 16#7FF# = 2047, so < 2048 = 2^11)
         F (8 * I)     := Field_Element ((B (0) or Shift_Left (B (1), 8)) and 16#7FF#);
         F (8 * I + 1) := Field_Element ((Shift_Right (B (1), 3) or Shift_Left (B (2), 5)) and 16#7FF#);
         F (8 * I + 2) := Field_Element ((Shift_Right (B (2), 6) or Shift_Left (B (3), 2) or Shift_Left (B (4), 10)) and 16#7FF#);
         F (8 * I + 3) := Field_Element ((Shift_Right (B (4), 1) or Shift_Left (B (5), 7)) and 16#7FF#);
         F (8 * I + 4) := Field_Element ((Shift_Right (B (5), 4) or Shift_Left (B (6), 4)) and 16#7FF#);
         F (8 * I + 5) := Field_Element ((Shift_Right (B (6), 7) or Shift_Left (B (7), 1) or Shift_Left (B (8), 9)) and 16#7FF#);
         F (8 * I + 6) := Field_Element ((Shift_Right (B (8), 2) or Shift_Left (B (9), 6)) and 16#7FF#);
         F (8 * I + 7) := Field_Element ((Shift_Right (B (9), 5) or Shift_Left (B (10), 3)) and 16#7FF#);

         Idx := Idx + 11;
      end loop;
   end ByteDecode_11;

   --  ByteEncode_5: Pack 256 5-bit coefficients into 160 bytes
   --  Groups of 8 coefficients (40 bits) → 5 bytes
   procedure ByteEncode_5 (
      F      : in  Polynomial;
      Output : out Compressed_Poly_Dv
   ) is
      Idx : Natural := 0;
      C : array (0 .. 7) of Unsigned_8;
   begin
      --  Process 8 coefficients at a time
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I in 0 .. 31);
         pragma Loop_Invariant (Idx = I * 5);
         pragma Loop_Invariant (for all K in 0 .. Idx - 1 => Output (K)'Initialized);

         for J in 0 .. 7 loop
            pragma Loop_Invariant (J in 0 .. 7);
            C (J) := Unsigned_8 (F (8 * I + J) and 16#1F#);
         end loop;

         --  Pack 40 bits into 5 bytes
         Output (Idx)     := Byte (C (0) or Shift_Left (C (1), 5));
         Output (Idx + 1) := Byte (Shift_Right (C (1), 3) or Shift_Left (C (2), 2) or Shift_Left (C (3), 7));
         Output (Idx + 2) := Byte (Shift_Right (C (3), 1) or Shift_Left (C (4), 4));
         Output (Idx + 3) := Byte (Shift_Right (C (4), 4) or Shift_Left (C (5), 1) or Shift_Left (C (6), 6));
         Output (Idx + 4) := Byte (Shift_Right (C (6), 2) or Shift_Left (C (7), 3));

         Idx := Idx + 5;
      end loop;
   end ByteEncode_5;

   --  ByteDecode_5: Unpack 160 bytes to 256 5-bit coefficients
   procedure ByteDecode_5 (
      Input : in  Compressed_Poly_Dv;
      F     : out Polynomial
   ) is
      Idx : Natural := 0;
      B : array (0 .. 4) of Unsigned_8;
   begin
      --  Process 5 bytes at a time → 8 coefficients
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I in 0 .. 31);
         pragma Loop_Invariant (Idx = I * 5);
         pragma Loop_Invariant (for all K in 0 .. 8 * I - 1 =>
            F (K)'Initialized and F (K) < 2**Dv);

         for J in 0 .. 4 loop
            pragma Loop_Invariant (J in 0 .. 4);
            B (J) := Unsigned_8 (Input (Idx + J));
         end loop;

         --  Extract 8 5-bit coefficients (all masked with 16#1F# = 31, so < 32 = 2^5)
         F (8 * I)     := Field_Element (B (0) and 16#1F#);
         F (8 * I + 1) := Field_Element ((Shift_Right (B (0), 5) or Shift_Left (B (1), 3)) and 16#1F#);
         F (8 * I + 2) := Field_Element ((Shift_Right (B (1), 2)) and 16#1F#);
         F (8 * I + 3) := Field_Element ((Shift_Right (B (1), 7) or Shift_Left (B (2), 1)) and 16#1F#);
         F (8 * I + 4) := Field_Element ((Shift_Right (B (2), 4) or Shift_Left (B (3), 4)) and 16#1F#);
         F (8 * I + 5) := Field_Element ((Shift_Right (B (3), 1)) and 16#1F#);
         F (8 * I + 6) := Field_Element ((Shift_Right (B (3), 6) or Shift_Left (B (4), 2)) and 16#1F#);
         --  Last coefficient: shift right by 3 from 8-bit value gives max 31 (5 bits)
         F (8 * I + 7) := Field_Element (Shift_Right (B (4), 3) and 16#1F#);

         Idx := Idx + 5;
      end loop;
   end ByteDecode_5;

   --  ByteEncode_1: Pack 256 1-bit coefficients into 32 bytes
   procedure ByteEncode_1 (
      F      : in  Polynomial;
      Output : out Message
   ) is
      Acc : Unsigned_8;
   begin
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I in 0 .. 31);
         Acc := Unsigned_8 (F (8 * I) and 1);
         Acc := Acc or Shift_Left (Unsigned_8 (F (8 * I + 1) and 1), 1);
         Acc := Acc or Shift_Left (Unsigned_8 (F (8 * I + 2) and 1), 2);
         Acc := Acc or Shift_Left (Unsigned_8 (F (8 * I + 3) and 1), 3);
         Acc := Acc or Shift_Left (Unsigned_8 (F (8 * I + 4) and 1), 4);
         Acc := Acc or Shift_Left (Unsigned_8 (F (8 * I + 5) and 1), 5);
         Acc := Acc or Shift_Left (Unsigned_8 (F (8 * I + 6) and 1), 6);
         Acc := Acc or Shift_Left (Unsigned_8 (F (8 * I + 7) and 1), 7);
         Output (I) := Byte (Acc);
      end loop;
   end ByteEncode_1;

   --  ByteDecode_1: Unpack 32 bytes to 256 1-bit coefficients
   procedure ByteDecode_1 (
      Input : in  Message;
      F     : out Polynomial
   ) is
      B : Unsigned_8;
   begin
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I in 0 .. 31);
         pragma Loop_Invariant (for all K in 0 .. 8 * I - 1 =>
            F (K)'Initialized and F (K) in 0 | 1);
         B := Unsigned_8 (Input (I));
         for J in 0 .. 7 loop
            pragma Loop_Invariant (J in 0 .. 7);
            pragma Loop_Invariant (for all K in 0 .. 8 * I + J - 1 =>
               F (K)'Initialized and F (K) in 0 | 1);
            F (8 * I + J) := Field_Element (Shift_Right (B, J) and 1);
         end loop;
      end loop;
   end ByteDecode_1;

   --  Encode polynomial vector (k polynomials)
   procedure Encode_Vector_12 (
      V      : in  Poly_Vector;
      Output : out Encoded_Vector
   ) is
      Offset : Natural;
      Buf : Encoded_Poly;
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         pragma Loop_Invariant (for all K in 0 .. I * Encoded_Poly_Bytes - 1 =>
            Output (K)'Initialized);
         Offset := I * Encoded_Poly_Bytes;
         ByteEncode_12 (V (I), Buf);
         for J in Encoded_Poly'Range loop
            pragma Loop_Invariant (J in Encoded_Poly'Range);
            pragma Loop_Invariant (for all K in 0 .. Offset - 1 =>
               Output (K)'Initialized);
            pragma Loop_Invariant (for all K in Offset .. Offset + J - 1 =>
               Output (K)'Initialized);
            Output (Offset + J) := Buf (J);
         end loop;
      end loop;
   end Encode_Vector_12;

   --  Decode polynomial vector
   procedure Decode_Vector_12 (
      Input : in  Encoded_Vector;
      V     : out Poly_Vector
   ) is
      Offset : Natural;
      Buf : Encoded_Poly;
   begin
      for I in Vec_Index loop
         pragma Loop_Invariant (I in Vec_Index);
         Offset := I * Encoded_Poly_Bytes;
         for J in Encoded_Poly'Range loop
            pragma Loop_Invariant (J in Encoded_Poly'Range);
            Buf (J) := Input (Offset + J);
         end loop;
         ByteDecode_12 (Buf, V (I));
      end loop;
   end Decode_Vector_12;

end Anubis_MLKEM_Encoding;
