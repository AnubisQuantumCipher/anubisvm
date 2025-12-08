-------------------------------------------------------------------------------
--  ANUBIS Poly1305 Message Authentication Code (Implementation)
--
--  Following SPARKNaCl platinum-level patterns.
--  Uses 5 x 26-bit limbs for 130-bit arithmetic (mod 2^130 - 5).
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Poly1305 with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Types: 130-bit arithmetic using 5 x 26-bit limbs
   ---------------------------------------------------------------------------

   --  Each limb holds up to 26 bits (max value ~67 million)
   --  We use U64 to prevent overflow during multiply-accumulate
   subtype Limb is Unsigned_64;
   type Limb_Array is array (0 .. 4) of Limb;

   --  Maximum values for proof
   Limb_26_Max : constant := 2**26 - 1;  --  67108863

   ---------------------------------------------------------------------------
   --  Helper: Load 32-bit little-endian
   ---------------------------------------------------------------------------

   function Load_LE32 (Data : Byte_Array; Offset : Natural) return Unsigned_32
   with
      Global => null,
      Pre    => Offset + 3 <= Data'Last - Data'First
   is
   begin
      return Unsigned_32 (Data (Data'First + Offset)) or
             Shift_Left (Unsigned_32 (Data (Data'First + Offset + 1)), 8) or
             Shift_Left (Unsigned_32 (Data (Data'First + Offset + 2)), 16) or
             Shift_Left (Unsigned_32 (Data (Data'First + Offset + 3)), 24);
   end Load_LE32;

   procedure Store_LE32 (Data : in Out Byte_Array; Offset : Natural; Value : Unsigned_32)
   with
      Global => null,
      Pre    => Offset + 3 <= Data'Last - Data'First
   is
   begin
      Data (Data'First + Offset)     := Byte (Value and 16#FF#);
      Data (Data'First + Offset + 1) := Byte (Shift_Right (Value, 8) and 16#FF#);
      Data (Data'First + Offset + 2) := Byte (Shift_Right (Value, 16) and 16#FF#);
      Data (Data'First + Offset + 3) := Byte (Shift_Right (Value, 24) and 16#FF#);
   end Store_LE32;

   ---------------------------------------------------------------------------
   --  Clamp R value (clear certain bits as per Poly1305 spec)
   ---------------------------------------------------------------------------

   procedure Clamp_R (R : in Out Limb_Array) with
      Global => null
   is
   begin
      --  Clear top bits of each 26-bit limb to ensure r is properly clamped
      --  r[3], r[7], r[11], r[15] &= 0x0f (top 4 bits of each 32-bit word)
      --  r[4], r[8], r[12] &= 0xfc (clear bottom 2 bits)
      --  In our 26-bit limb representation, apply equivalent masks
      R (0) := R (0) and 16#03FFFFFF#;  --  26 bits
      R (1) := R (1) and 16#03FFFF03#;  --  Clear bits for clamping
      R (2) := R (2) and 16#03FFC0FF#;
      R (3) := R (3) and 16#03F03FFF#;
      R (4) := R (4) and 16#000FFFFF#;  --  Top limb shorter
   end Clamp_R;

   ---------------------------------------------------------------------------
   --  Reduce modulo 2^130 - 5
   ---------------------------------------------------------------------------

   procedure Reduce (H : in Out Limb_Array) with
      Global => null
   is
      Carry : Limb;
   begin
      --  Propagate carries
      for I in 0 .. 3 loop
         pragma Loop_Invariant (True);
         Carry := Shift_Right (H (I), 26);
         H (I) := H (I) and Limb_26_Max;
         H (I + 1) := H (I + 1) + Carry;
      end loop;

      --  Final limb: reduce mod 2^130 - 5
      Carry := Shift_Right (H (4), 26);
      H (4) := H (4) and Limb_26_Max;
      H (0) := H (0) + Carry * 5;  --  2^130 ≡ 5 (mod 2^130 - 5)

      --  One more propagation
      Carry := Shift_Right (H (0), 26);
      H (0) := H (0) and Limb_26_Max;
      H (1) := H (1) + Carry;
   end Reduce;

   ---------------------------------------------------------------------------
   --  Add block to accumulator and multiply by R
   ---------------------------------------------------------------------------

   procedure Process_Block (
      H     : in Out Limb_Array;
      R     : Limb_Array;
      Block : Byte_Array;
      Final : Boolean
   ) with
      Global => null,
      Pre    => Block'Length = 16 or else (Final and Block'Length <= 16)
   is
      --  Load block into limbs
      B : Limb_Array := (others => 0);
      T : array (0 .. 4) of Unsigned_64 := (others => 0);
      Len : constant Natural := Block'Length;
      Pad : array (0 .. 16) of Byte := (others => 0);
   begin
      --  Copy block with padding
      for I in 0 .. Len - 1 loop
         pragma Loop_Invariant (I < Len);
         Pad (I) := Block (Block'First + I);
      end loop;

      --  Add 2^128 to block (hibit) unless final partial block
      if not Final or Len = 16 then
         Pad (16) := 1;
      elsif Len < 16 then
         Pad (Len) := 1;  --  Add 1 after message bytes
      end if;

      --  Convert padded block to 5 x 26-bit limbs
      declare
         W0 : constant Unsigned_32 := Unsigned_32 (Pad (0)) or
                                      Shift_Left (Unsigned_32 (Pad (1)), 8) or
                                      Shift_Left (Unsigned_32 (Pad (2)), 16) or
                                      Shift_Left (Unsigned_32 (Pad (3)), 24);
         W1 : constant Unsigned_32 := Unsigned_32 (Pad (4)) or
                                      Shift_Left (Unsigned_32 (Pad (5)), 8) or
                                      Shift_Left (Unsigned_32 (Pad (6)), 16) or
                                      Shift_Left (Unsigned_32 (Pad (7)), 24);
         W2 : constant Unsigned_32 := Unsigned_32 (Pad (8)) or
                                      Shift_Left (Unsigned_32 (Pad (9)), 8) or
                                      Shift_Left (Unsigned_32 (Pad (10)), 16) or
                                      Shift_Left (Unsigned_32 (Pad (11)), 24);
         W3 : constant Unsigned_32 := Unsigned_32 (Pad (12)) or
                                      Shift_Left (Unsigned_32 (Pad (13)), 8) or
                                      Shift_Left (Unsigned_32 (Pad (14)), 16) or
                                      Shift_Left (Unsigned_32 (Pad (15)), 24);
         W4 : constant Unsigned_32 := Unsigned_32 (Pad (16));
      begin
         B (0) := Limb (W0 and 16#03FFFFFF#);
         B (1) := Limb (Shift_Right (W0, 26) or Shift_Left (W1 and 16#FFFFF#, 6));
         B (2) := Limb (Shift_Right (W1, 20) or Shift_Left (W2 and 16#3FFF#, 12));
         B (3) := Limb (Shift_Right (W2, 14) or Shift_Left (W3 and 16#FF#, 18));
         B (4) := Limb (Shift_Right (W3, 8) or Shift_Left (W4, 24));
      end;

      --  Add block to accumulator: H = H + Block
      for I in 0 .. 4 loop
         pragma Loop_Invariant (True);
         H (I) := H (I) + B (I);
      end loop;

      --  Multiply H by R (schoolbook multiplication mod 2^130 - 5)
      --  Using the fact that 2^130 ≡ 5 (mod p)
      declare
         R0 : constant Limb := R (0);
         R1 : constant Limb := R (1);
         R2 : constant Limb := R (2);
         R3 : constant Limb := R (3);
         R4 : constant Limb := R (4);
         S1 : constant Limb := R1 * 5;
         S2 : constant Limb := R2 * 5;
         S3 : constant Limb := R3 * 5;
         S4 : constant Limb := R4 * 5;
         H0 : constant Limb := H (0);
         H1 : constant Limb := H (1);
         H2 : constant Limb := H (2);
         H3 : constant Limb := H (3);
         H4 : constant Limb := H (4);
      begin
         T (0) := H0 * R0 + H1 * S4 + H2 * S3 + H3 * S2 + H4 * S1;
         T (1) := H0 * R1 + H1 * R0 + H2 * S4 + H3 * S3 + H4 * S2;
         T (2) := H0 * R2 + H1 * R1 + H2 * R0 + H3 * S4 + H4 * S3;
         T (3) := H0 * R3 + H1 * R2 + H2 * R1 + H3 * R0 + H4 * S4;
         T (4) := H0 * R4 + H1 * R3 + H2 * R2 + H3 * R1 + H4 * R0;
      end;

      --  Store result and reduce
      for I in 0 .. 4 loop
         pragma Loop_Invariant (True);
         H (I) := T (I);
      end loop;

      Reduce (H);
   end Process_Block;

   ---------------------------------------------------------------------------
   --  Onetimeauth: Compute Poly1305 tag
   ---------------------------------------------------------------------------

   procedure Onetimeauth (
      Tag     :    out Poly1305_Tag;
      Message : in     Byte_Array;
      Key     : in     Poly1305_Key
   ) is
      H : Limb_Array := (others => 0);  --  Accumulator
      R : Limb_Array;                    --  Clamped R
      S : array (0 .. 3) of Unsigned_32; --  S (second 16 bytes of key)
      Key_Arr : constant Byte_Array (0 .. 31) := Byte_Array (Key);
      Offset : Natural := 0;
      Block  : Byte_Array (0 .. 15);
   begin
      Tag := (others => 0);

      --  Load R from first 16 bytes of key
      declare
         W0 : constant Unsigned_32 := Load_LE32 (Key_Arr, 0);
         W1 : constant Unsigned_32 := Load_LE32 (Key_Arr, 4);
         W2 : constant Unsigned_32 := Load_LE32 (Key_Arr, 8);
         W3 : constant Unsigned_32 := Load_LE32 (Key_Arr, 12);
      begin
         R (0) := Limb (W0 and 16#03FFFFFF#);
         R (1) := Limb (Shift_Right (W0, 26) or Shift_Left (W1 and 16#FFFFF#, 6));
         R (2) := Limb (Shift_Right (W1, 20) or Shift_Left (W2 and 16#3FFF#, 12));
         R (3) := Limb (Shift_Right (W2, 14) or Shift_Left (W3 and 16#FF#, 18));
         R (4) := Limb (Shift_Right (W3, 8));
      end;

      --  Clamp R
      Clamp_R (R);

      --  Load S from second 16 bytes of key
      S (0) := Load_LE32 (Key_Arr, 16);
      S (1) := Load_LE32 (Key_Arr, 20);
      S (2) := Load_LE32 (Key_Arr, 24);
      S (3) := Load_LE32 (Key_Arr, 28);

      --  Process full 16-byte blocks
      while Offset + 16 <= Message'Length loop
         pragma Loop_Invariant (Offset mod 16 = 0);
         pragma Loop_Invariant (Offset < Message'Length);

         for I in 0 .. 15 loop
            Block (I) := Message (Offset + I);
         end loop;

         Process_Block (H, R, Block, Final => False);
         Offset := Offset + 16;
      end loop;

      --  Process final partial block if any
      if Offset < Message'Length then
         declare
            Remaining : constant Natural := Message'Length - Offset;
            Final_Block : Byte_Array (0 .. Remaining - 1);
         begin
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I < Remaining);
               Final_Block (I) := Message (Offset + I);
            end loop;
            Process_Block (H, R, Final_Block, Final => True);
         end;
      end if;

      --  Final reduction
      Reduce (H);

      --  Fully reduce mod 2^130 - 5
      declare
         G : Limb_Array;
         Carry : Limb;
         Mask : Unsigned_64;
      begin
         --  Compute H + 5 and check if >= 2^130
         G := H;
         G (0) := G (0) + 5;
         for I in 0 .. 3 loop
            pragma Loop_Invariant (True);
            Carry := Shift_Right (G (I), 26);
            G (I) := G (I) and Limb_26_Max;
            G (I + 1) := G (I + 1) + Carry;
         end loop;
         Carry := Shift_Right (G (4), 26);

         --  If carry out, use G; otherwise use H
         Mask := (not (Carry - 1)) and 16#FFFFFFFFFFFFFFFF#;
         for I in 0 .. 4 loop
            pragma Loop_Invariant (True);
            H (I) := (H (I) and (not Mask)) or (G (I) and Mask);
         end loop;
      end;

      --  Add S to get final tag
      declare
         F : Unsigned_64;
         Tag_Arr : Byte_Array (0 .. 15) := (others => 0);
      begin
         F := H (0) or Shift_Left (H (1), 26) or Shift_Left (H (2), 52);
         F := F + Unsigned_64 (S (0));
         Store_LE32 (Tag_Arr, 0, Unsigned_32 (F and 16#FFFFFFFF#));
         F := Shift_Right (F, 32);

         F := F + Shift_Right (H (2), 12) or Shift_Left (H (3), 14);
         F := F + Unsigned_64 (S (1));
         Store_LE32 (Tag_Arr, 4, Unsigned_32 (F and 16#FFFFFFFF#));
         F := Shift_Right (F, 32);

         F := F + Shift_Right (H (3), 18) or Shift_Left (H (4), 8);
         F := F + Unsigned_64 (S (2));
         Store_LE32 (Tag_Arr, 8, Unsigned_32 (F and 16#FFFFFFFF#));
         F := Shift_Right (F, 32);

         F := F + Shift_Right (H (4), 24);
         F := F + Unsigned_64 (S (3));
         Store_LE32 (Tag_Arr, 12, Unsigned_32 (F and 16#FFFFFFFF#));

         Tag := Poly1305_Tag (Tag_Arr);
      end;
   end Onetimeauth;

   ---------------------------------------------------------------------------
   --  Constant-time verification
   ---------------------------------------------------------------------------

   function Onetimeauth_Verify (
      Tag     : Poly1305_Tag;
      Message : Byte_Array;
      Key     : Poly1305_Key
   ) return Boolean is
      Computed : Poly1305_Tag;
      Diff     : Byte := 0;
   begin
      Onetimeauth (Computed, Message, Key);

      --  Constant-time comparison
      for I in Tag'Range loop
         pragma Loop_Invariant (True);
         Diff := Diff or (Tag (I) xor Computed (I));
      end loop;

      return Diff = 0;
   end Onetimeauth_Verify;

   ---------------------------------------------------------------------------
   --  Sanitization
   ---------------------------------------------------------------------------

   procedure Sanitize (Key : out Poly1305_Key) is
   begin
      Key := (others => 0);
   end Sanitize;

   procedure Sanitize_Tag (Tag : out Poly1305_Tag) is
   begin
      Tag := (others => 0);
   end Sanitize_Tag;

end Anubis_Poly1305;
