pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Keccak; use Anubis_Keccak;

package body Anubis_KMAC with
   SPARK_Mode => On
is

   --  cSHAKE256 domain separator suffix (0x04 instead of SHAKE's 0x1F)
   cSHAKE256_Suffix : constant Byte := 16#04#;

   --  SHAKE256 suffix for fallback when N="" and S=""
   SHAKE256_Suffix : constant Byte := 16#1F#;

   --  Convert 8 bytes (little-endian) to Lane
   function Bytes_To_Lane (B : Byte_Array) return Lane with
      Global => null,
      Pre => B'Length = 8
   is
      Result : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         declare
            Shift_Amount : constant Natural := I * 8;
            Byte_As_U64 : constant Unsigned_64 := Unsigned_64 (B (B'First + I));
         begin
            Result := Result or Shift_Left (Byte_As_U64, Shift_Amount);
         end;
      end loop;
      return Lane (Result);
   end Bytes_To_Lane;

   --  Convert Lane to 8 bytes (little-endian)
   procedure Lane_To_Bytes (L : Lane; B : out Byte_Array) with
      Global => null,
      Pre => B'Length = 8
   is
      L_U64 : constant Unsigned_64 := Unsigned_64 (L);
   begin
      B := (others => 0);
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         declare
            Shift_Amount : constant Natural := I * 8;
            Shifted : constant Unsigned_64 := Shift_Right (L_U64, Shift_Amount);
         begin
            B (B'First + I) := Byte (Shifted and 16#FF#);
         end;
      end loop;
   end Lane_To_Bytes;

   --  left_encode: Encode integer with length prefix (big-endian)
   function Left_Encode (X : Natural) return Encoded_Int is
      Result : Encoded_Int := (Data => (others => 0), Length => 0);
      Value : Natural := X;
      Temp : Byte_Array (0 .. 7) := (others => 0);
      N : Natural := 0;
   begin
      if X = 0 then
         --  Special case: 0 encodes as [1, 0]
         Result.Data (0) := 1;
         Result.Data (1) := 0;
         Result.Length := 2;
         return Result;
      end if;

      --  Encode value in big-endian into temp buffer
      while Value > 0 and N < 8 loop
         pragma Loop_Invariant (N <= 7);
         pragma Loop_Invariant (N < 8);
         pragma Loop_Variant (Decreases => Value);
         Temp (7 - N) := Byte (Value mod 256);
         Value := Value / 256;
         N := N + 1;
      end loop;

      --  Result: [byte_count] || [big-endian bytes]
      Result.Data (0) := Byte (N);
      for I in 0 .. N - 1 loop
         pragma Loop_Invariant (I >= 0 and I < N);
         Result.Data (1 + I) := Temp (8 - N + I);
      end loop;
      Result.Length := N + 1;

      return Result;
   end Left_Encode;

   --  right_encode: Encode integer with length suffix (big-endian)
   function Right_Encode (X : Natural) return Encoded_Int is
      Result : Encoded_Int := (Data => (others => 0), Length => 0);
      Value : Natural := X;
      Temp : Byte_Array (0 .. 7) := (others => 0);
      N : Natural := 0;
   begin
      if X = 0 then
         --  Special case: 0 encodes as [0, 1]
         Result.Data (0) := 0;
         Result.Data (1) := 1;
         Result.Length := 2;
         return Result;
      end if;

      --  Encode value in big-endian into temp buffer
      while Value > 0 and N < 8 loop
         pragma Loop_Invariant (N <= 7);
         pragma Loop_Invariant (N < 8);
         pragma Loop_Variant (Decreases => Value);
         Temp (7 - N) := Byte (Value mod 256);
         Value := Value / 256;
         N := N + 1;
      end loop;

      --  Result: [big-endian bytes] || [byte_count]
      for I in 0 .. N - 1 loop
         pragma Loop_Invariant (I >= 0 and I < N);
         Result.Data (I) := Temp (8 - N + I);
      end loop;
      Result.Data (N) := Byte (N);
      Result.Length := N + 1;

      return Result;
   end Right_Encode;

   --  Absorb block into Keccak state
   procedure Absorb_Block (
      State : in out State_Array;
      Block : Byte_Array;
      Rate  : Positive
   ) with
      Global => null,
      Pre => Rate >= 8 and then Rate <= 200 and then Rate mod 8 = 0
             and then Block'Last < Natural'Last
             and then Block'Length = Rate
   is
      Num_Lanes : constant Positive := Rate / 8;
   begin
      for I in 0 .. Num_Lanes - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Num_Lanes);
         declare
            X : constant Lane_Index := Lane_Index (I mod 5);
            Y : constant Lane_Index := Lane_Index (I / 5);
            Lane_Bytes : constant Byte_Array :=
               Block (Block'First + I * 8 .. Block'First + I * 8 + 7);
            Block_Lane : constant Lane := Bytes_To_Lane (Lane_Bytes);
         begin
            State (X, Y) := State (X, Y) xor Block_Lane;
         end;
      end loop;
      Keccak_F (State);
   end Absorb_Block;

   --  Squeeze bytes from Keccak state
   procedure Squeeze_Block (
      State  : State_Array;
      Output : out Byte_Array;
      Rate   : Positive
   ) with
      Global => null,
      Pre => Output'Last < Natural'Last and then
             Output'Length = Rate and then Rate >= 8 and then
             Rate <= 200 and then Rate mod 8 = 0
   is
      Num_Lanes : constant Positive := Rate / 8;
   begin
      Output := (others => 0);
      for I in 0 .. Num_Lanes - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Num_Lanes);
         declare
            X : constant Lane_Index := Lane_Index (I mod 5);
            Y : constant Lane_Index := Lane_Index (I / 5);
            State_Lane : constant Lane := State (X, Y);
            Out_Slice : Byte_Array (0 .. 7);
         begin
            Lane_To_Bytes (State_Lane, Out_Slice);
            for J in 0 .. 7 loop
               pragma Loop_Invariant (J in 0 .. 7);
               Output (Output'First + I * 8 + J) := Out_Slice (J);
            end loop;
         end;
      end loop;
   end Squeeze_Block;

   --  Internal: cSHAKE256 with bytepad prefix and right_encode suffix
   --  This is the core of KMAC256
   procedure cSHAKE256_KMAC (
      Key           : KMAC_Key;
      Message       : Byte_Array;
      Custom        : String;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) with
      Global => null,
      Pre => Message'Length <= Max_Message_Length and then
             Custom'Length <= Max_Custom_Length and then
             Output'Last < Natural'Last and then
             Output'Length = Output_Length and then
             Output_Length <= 64
   is
      State : State_Array := (others => (others => 0));

      --  KMAC function name "KMAC" as bytes
      KMAC_Name : constant Byte_Array (0 .. 3) := (75, 77, 65, 67);  -- "KMAC"

      --  Bytepad block for absorbing prefix
      Bytepad_Block : Byte_Array (0 .. Rate_256 - 1) := (others => 0);
      Pad_Pos : Natural := 0;

      --  Encoded values
      Enc_Rate : constant Encoded_Int := Left_Encode (Rate_256);
      Enc_Name_Len : constant Encoded_Int := Left_Encode (KMAC_Name'Length * 8);
      Enc_Custom_Len : Encoded_Int;
      Enc_Key_Len : constant Encoded_Int := Left_Encode (Key'Length * 8);
      Enc_Output_Bits : constant Encoded_Int := Right_Encode (Output_Length * 8);

   begin
      Output := (others => 0);

      --  ============================================================
      --  Phase 1: Absorb bytepad(encode_string("KMAC") || encode_string(S), 136)
      --  This is the cSHAKE256 prefix
      --  ============================================================

      --  Start bytepad with left_encode(136)
      for I in 0 .. Enc_Rate.Length - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Enc_Rate.Length);
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := Enc_Rate.Data (I);
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      --  encode_string("KMAC"): left_encode(32) || "KMAC"
      for I in 0 .. Enc_Name_Len.Length - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Enc_Name_Len.Length);
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := Enc_Name_Len.Data (I);
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      for I in KMAC_Name'Range loop
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := KMAC_Name (I);
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      --  encode_string(S): left_encode(|S|*8) || S
      Enc_Custom_Len := Left_Encode (Custom'Length * 8);

      for I in 0 .. Enc_Custom_Len.Length - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Enc_Custom_Len.Length);
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := Enc_Custom_Len.Data (I);
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      --  Copy customization string bytes
      for I in Custom'Range loop
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := Byte (Character'Pos (Custom (I)));
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      --  Pad to rate boundary (already zero-filled, just absorb)
      Absorb_Block (State, Bytepad_Block, Rate_256);

      --  ============================================================
      --  Phase 2: Absorb bytepad(encode_string(K), 136) - the key
      --  ============================================================

      Bytepad_Block := (others => 0);
      Pad_Pos := 0;

      --  left_encode(136)
      for I in 0 .. Enc_Rate.Length - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Enc_Rate.Length);
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := Enc_Rate.Data (I);
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      --  encode_string(K): left_encode(|K|*8) || K
      for I in 0 .. Enc_Key_Len.Length - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Enc_Key_Len.Length);
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := Enc_Key_Len.Data (I);
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      for I in Key'Range loop
         pragma Loop_Invariant (Pad_Pos <= Rate_256);
         if Pad_Pos <= Rate_256 - 1 then
            Bytepad_Block (Pad_Pos) := Key (I);
            Pad_Pos := Pad_Pos + 1;
         end if;
      end loop;

      --  Pad to rate boundary and absorb
      Absorb_Block (State, Bytepad_Block, Rate_256);

      --  ============================================================
      --  Phase 3: Absorb message in rate-sized blocks
      --  ============================================================

      declare
         Msg_Pos : Natural := 0;
         Full_Block : Byte_Array (0 .. Rate_256 - 1);
      begin
         --  Absorb full blocks
         while Message'Length >= Rate_256 and then
               Msg_Pos <= Message'Length - Rate_256 loop
            pragma Loop_Invariant (Msg_Pos <= Message'Length);
            pragma Loop_Variant (Decreases => Message'Length - Msg_Pos);

            for I in 0 .. Rate_256 - 1 loop
               pragma Loop_Invariant (I >= 0 and I < Rate_256);
               Full_Block (I) := Message (Message'First + Msg_Pos + I);
            end loop;
            Absorb_Block (State, Full_Block, Rate_256);
            Msg_Pos := Msg_Pos + Rate_256;
         end loop;

         --  Final block with remaining message + right_encode(L) + padding
         declare
            Remaining : constant Natural := Message'Length - Msg_Pos;
            Final_Block : Byte_Array (0 .. Rate_256 - 1) := (others => 0);
            Final_Pos : Natural := 0;
         begin
            --  Copy remaining message bytes
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I >= 0 and I < Remaining);
               pragma Loop_Invariant (Final_Pos <= Rate_256);
               if Final_Pos <= Rate_256 - 1 then
                  Final_Block (Final_Pos) := Message (Message'First + Msg_Pos + I);
                  Final_Pos := Final_Pos + 1;
               end if;
            end loop;

            --  Append right_encode(L) where L = output length in bits
            for I in 0 .. Enc_Output_Bits.Length - 1 loop
               pragma Loop_Invariant (I >= 0 and I < Enc_Output_Bits.Length);
               pragma Loop_Invariant (Final_Pos <= Rate_256);
               if Final_Pos <= Rate_256 - 1 then
                  Final_Block (Final_Pos) := Enc_Output_Bits.Data (I);
                  Final_Pos := Final_Pos + 1;
               end if;
            end loop;

            --  Apply cSHAKE padding: 0x04 domain separator
            if Final_Pos <= Rate_256 - 1 then
               Final_Block (Final_Pos) := cSHAKE256_Suffix;
            end if;

            --  Set final bit of padding (multi-rate padding)
            Final_Block (Rate_256 - 1) := Final_Block (Rate_256 - 1) or 16#80#;

            Absorb_Block (State, Final_Block, Rate_256);
         end;
      end;

      --  ============================================================
      --  Phase 4: Squeeze output
      --  ============================================================

      declare
         Squeezed : Byte_Array (0 .. Rate_256 - 1);
      begin
         Squeeze_Block (State, Squeezed, Rate_256);
         for I in 0 .. Output_Length - 1 loop
            pragma Loop_Invariant (I >= 0 and I < Output_Length);
            Output (Output'First + I) := Squeezed (I);
         end loop;
      end;
   end cSHAKE256_KMAC;

   --  KMAC256: 256-bit output
   procedure KMAC256 (
      Key     : KMAC_Key;
      Message : Byte_Array;
      Custom  : String;
      Tag     : out KMAC256_Tag
   ) is
   begin
      cSHAKE256_KMAC (Key, Message, Custom, Tag, 32);
   end KMAC256;

   --  KMAC256_XOF: 512-bit output
   procedure KMAC256_XOF (
      Key     : KMAC_Key;
      Message : Byte_Array;
      Custom  : String;
      Tag     : out KMAC256_Tag_512
   ) is
   begin
      cSHAKE256_KMAC (Key, Message, Custom, Tag, 64);
   end KMAC256_XOF;

   --  Constant-time comparison to prevent timing attacks
   function Constant_Time_Equal (
      A : Byte_Array;
      B : Byte_Array
   ) return Boolean is
      Diff : Byte := 0;
   begin
      if A'Length = 0 then
         return True;  -- Both must be empty since A'Length = B'Length (precondition)
      end if;
      for I in A'Range loop
         pragma Loop_Invariant (I in A'Range);
         declare
            Offset : constant Natural := I - A'First;
         begin
            if B'First + Offset <= B'Last then
               Diff := Diff or (A (I) xor B (B'First + Offset));
            end if;
         end;
      end loop;
      return Diff = 0;
   end Constant_Time_Equal;

   --  Verify KMAC256 tag
   function Verify_KMAC256 (
      Key          : KMAC_Key;
      Message      : Byte_Array;
      Custom       : String;
      Expected_Tag : KMAC256_Tag
   ) return Boolean is
      Computed_Tag : KMAC256_Tag;
   begin
      KMAC256 (Key, Message, Custom, Computed_Tag);
      return Constant_Time_Equal (Computed_Tag, Expected_Tag);
   end Verify_KMAC256;

   --  Secure key zeroization using volatile writes
   --  The loop with Volatile aspect prevents dead-store elimination
   procedure Zeroize_Key (Key : in Out KMAC_Key) is
      type Volatile_Byte is mod 2**8 with
         Size => 8,
         Volatile_Full_Access => True;
      type Volatile_Key_Array is array (KMAC_Key'Range) of Volatile_Byte;

      --  Create a volatile view of the key
      Volatile_View : Volatile_Key_Array with
         Address => Key'Address,
         Import;
   begin
      for I in Volatile_View'Range loop
         pragma Loop_Invariant (I >= Volatile_View'First);
         pragma Loop_Invariant (for all J in Volatile_View'First .. I - 1 =>
                                Volatile_View (J) = 0);
         Volatile_View (I) := 0;
      end loop;
   end Zeroize_Key;

end Anubis_KMAC;
