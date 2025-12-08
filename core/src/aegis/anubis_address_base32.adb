pragma SPARK_Mode (On);

package body Anubis_Address_Base32 with
   SPARK_Mode => On
is

   --  Encode 5 bits to Base32 character
   function Encode_5bits (Value : Unsigned_8) return Base32_Char is
   begin
      return Encode_Table (Integer (Value));
   end Encode_5bits;

   --  Decode Base32 character to 5 bits (returns 255 on error)
   function Decode_Char (C : Character) return Unsigned_8 is
   begin
      case C is
         when '0' => return 0;
         when '1' => return 1;
         when '2' => return 2;
         when '3' => return 3;
         when '4' => return 4;
         when '5' => return 5;
         when '6' => return 6;
         when '7' => return 7;
         when '8' => return 8;
         when '9' => return 9;
         when 'A' | 'a' => return 10;
         when 'B' | 'b' => return 11;
         when 'C' | 'c' => return 12;
         when 'D' | 'd' => return 13;
         when 'E' | 'e' => return 14;
         when 'F' | 'f' => return 15;
         when 'G' | 'g' => return 16;
         when 'H' | 'h' => return 17;
         when 'J' | 'j' => return 18;
         when 'K' | 'k' => return 19;
         when 'M' | 'm' => return 20;
         when 'N' | 'n' => return 21;
         when 'P' | 'p' => return 22;
         when 'Q' | 'q' => return 23;
         when 'R' | 'r' => return 24;
         when 'S' | 's' => return 25;
         when 'T' | 't' => return 26;
         when 'V' | 'v' => return 27;
         when 'W' | 'w' => return 28;
         when 'X' | 'x' => return 29;
         when 'Y' | 'y' => return 30;
         when 'Z' | 'z' => return 31;
         when others => return 255;  --  Invalid character
      end case;
   end Decode_Char;

   --  Encode 32 bytes (256 bits) to 52 Base32 characters
   --  Process 5 bytes (40 bits) at a time → 8 Base32 chars
   --  6 full rounds (30 bytes → 48 chars) + 1 partial (2 bytes → 4 chars)
   procedure Encode_Account_ID (
      Input  : in  Account_ID;
      Output : out Payload_Base32
   ) is
      In_Idx  : Natural := 0;
      Out_Idx : Natural := 0;
      Buffer  : Unsigned_64;
      Bits    : Natural;
   begin
      --  Process 5 bytes at a time (40 bits → 8 chars)
      while In_Idx + 4 < 32 loop
         pragma Loop_Invariant (In_Idx mod 5 = 0);
         pragma Loop_Invariant (Out_Idx = (In_Idx / 5) * 8);
         pragma Loop_Invariant (In_Idx <= 30);
         pragma Loop_Invariant (Out_Idx <= 48);
         pragma Loop_Variant (Increases => In_Idx);

         --  Pack 5 bytes into 40-bit buffer
         Buffer := Shift_Left (Unsigned_64 (Input (In_Idx)), 32) or
                   Shift_Left (Unsigned_64 (Input (In_Idx + 1)), 24) or
                   Shift_Left (Unsigned_64 (Input (In_Idx + 2)), 16) or
                   Shift_Left (Unsigned_64 (Input (In_Idx + 3)), 8) or
                   Unsigned_64 (Input (In_Idx + 4));

         --  Extract 8 × 5-bit chunks (MSB first)
         for I in 0 .. 7 loop
            pragma Loop_Invariant (Out_Idx + I <= 55);
            Output (Out_Idx + I) := Encode_5bits (
               Unsigned_8 (Shift_Right (Buffer, 35 - I * 5) and 16#1F#)
            );
         end loop;

         In_Idx := In_Idx + 5;
         Out_Idx := Out_Idx + 8;
      end loop;

      --  Handle remaining 2 bytes (16 bits → 4 chars with 4-bit padding)
      --  In_Idx = 30, Out_Idx = 48
      Buffer := Shift_Left (Unsigned_64 (Input (30)), 8) or
                Unsigned_64 (Input (31));

      --  Extract 3 complete 5-bit chunks + 1 partial (1 bit + 4-bit padding)
      Output (48) := Encode_5bits (Unsigned_8 (Shift_Right (Buffer, 11) and 16#1F#));
      Output (49) := Encode_5bits (Unsigned_8 (Shift_Right (Buffer, 6) and 16#1F#));
      Output (50) := Encode_5bits (Unsigned_8 (Shift_Right (Buffer, 1) and 16#1F#));
      Output (51) := Encode_5bits (Unsigned_8 (Shift_Left (Buffer and 1, 4)));
   end Encode_Account_ID;

   --  Encode 3 bytes (24 bits) to 5 Base32 characters
   procedure Encode_Checksum (
      Input  : in  Checksum_Bytes;
      Output : out Checksum_Base32
   ) is
      Buffer : Unsigned_32;
   begin
      --  Pack 3 bytes into 24-bit buffer
      Buffer := Shift_Left (Unsigned_32 (Input (0)), 16) or
                Shift_Left (Unsigned_32 (Input (1)), 8) or
                Unsigned_32 (Input (2));

      --  Extract 5 × 5-bit chunks (4 complete + 1 with 1-bit padding)
      Output (0) := Encode_5bits (Unsigned_8 (Shift_Right (Buffer, 19) and 16#1F#));
      Output (1) := Encode_5bits (Unsigned_8 (Shift_Right (Buffer, 14) and 16#1F#));
      Output (2) := Encode_5bits (Unsigned_8 (Shift_Right (Buffer, 9) and 16#1F#));
      Output (3) := Encode_5bits (Unsigned_8 (Shift_Right (Buffer, 4) and 16#1F#));
      Output (4) := Encode_5bits (Unsigned_8 (Shift_Left (Buffer and 16#F#, 1)));
   end Encode_Checksum;

   --  Decode 52 Base32 characters to 32 bytes
   procedure Decode_Account_ID (
      Input   : in  Payload_Base32;
      Output  : out Account_ID;
      Success : out Boolean
   ) is
      In_Idx  : Natural := 0;
      Out_Idx : Natural := 0;
      Buffer  : Unsigned_64;
      Val     : Unsigned_8;
   begin
      Success := True;

      --  Process 8 chars at a time (40 bits → 5 bytes)
      while In_Idx + 7 < 52 and Success loop
         pragma Loop_Invariant (In_Idx mod 8 = 0);
         pragma Loop_Invariant (Out_Idx = (In_Idx / 8) * 5);
         pragma Loop_Invariant (In_Idx <= 48);
         pragma Loop_Invariant (Out_Idx <= 30);
         pragma Loop_Variant (Increases => In_Idx);

         Buffer := 0;

         --  Decode 8 Base32 chars into 40-bit buffer
         for I in 0 .. 7 loop
            pragma Loop_Invariant (In_Idx + I <= 55);
            Val := Decode_Char (Input (In_Idx + I));
            if Val = 255 then
               Success := False;
               return;
            end if;
            Buffer := Buffer or Shift_Left (Unsigned_64 (Val), 35 - I * 5);
         end loop;

         --  Extract 5 bytes
         Output (Out_Idx)     := Unsigned_8 (Shift_Right (Buffer, 32) and 16#FF#);
         Output (Out_Idx + 1) := Unsigned_8 (Shift_Right (Buffer, 24) and 16#FF#);
         Output (Out_Idx + 2) := Unsigned_8 (Shift_Right (Buffer, 16) and 16#FF#);
         Output (Out_Idx + 3) := Unsigned_8 (Shift_Right (Buffer, 8) and 16#FF#);
         Output (Out_Idx + 4) := Unsigned_8 (Buffer and 16#FF#);

         In_Idx := In_Idx + 8;
         Out_Idx := Out_Idx + 5;
      end loop;

      --  Handle remaining 4 chars (16 bits with 4-bit padding → 2 bytes)
      --  In_Idx = 48, Out_Idx = 30
      --  Char layout: bits 15-11 | bits 10-6 | bits 5-1 | bit 0 (at position 4)
      Buffer := 0;

      --  First 3 chars: normal 5-bit decode
      for I in 0 .. 2 loop
         pragma Loop_Invariant (I <= 2);
         Val := Decode_Char (Input (48 + I));
         if Val = 255 then
            Success := False;
            return;
         end if;
         Buffer := Buffer or Shift_Left (Unsigned_64 (Val), 11 - I * 5);
      end loop;

      --  Last char: bit 0 was stored at position 4, extract and shift right
      Val := Decode_Char (Input (51));
      if Val = 255 then
         Success := False;
         return;
      end if;
      Buffer := Buffer or Shift_Right (Unsigned_64 (Val), 4);

      --  Extract 2 bytes (padding already handled by shift)
      Output (30) := Unsigned_8 (Shift_Right (Buffer, 8) and 16#FF#);
      Output (31) := Unsigned_8 (Buffer and 16#FF#);
   end Decode_Account_ID;

   --  Decode 5 Base32 characters to 3 bytes
   procedure Decode_Checksum (
      Input   : in  Checksum_Base32;
      Output  : out Checksum_Bytes;
      Success : out Boolean
   ) is
      Buffer : Unsigned_32 := 0;
      Val    : Unsigned_8;
   begin
      Success := True;

      --  Decode first 4 Base32 chars into 24-bit buffer
      --  Char layout: bits 23-19 | bits 18-14 | bits 13-9 | bits 8-4 | bits 3-0 (at pos 4-1)
      for I in 0 .. 3 loop
         pragma Loop_Invariant (I <= 3);
         Val := Decode_Char (Input (I));
         if Val = 255 then
            Success := False;
            return;
         end if;
         Buffer := Buffer or Shift_Left (Unsigned_32 (Val), 19 - I * 5);
      end loop;

      --  Last char: bits 3-0 were stored at positions 4-1, shift right by 1
      Val := Decode_Char (Input (4));
      if Val = 255 then
         Success := False;
         return;
      end if;
      Buffer := Buffer or Shift_Right (Unsigned_32 (Val), 1);

      --  Extract 3 bytes (padding already handled)
      Output (0) := Unsigned_8 (Shift_Right (Buffer, 16) and 16#FF#);
      Output (1) := Unsigned_8 (Shift_Right (Buffer, 8) and 16#FF#);
      Output (2) := Unsigned_8 (Buffer and 16#FF#);
   end Decode_Checksum;

   --  Chunk payload into 8-8-8-8-8-8-8-4 format
   procedure Chunk_Payload (
      Input  : in  Payload_Base32;
      Output : out Chunked_Payload
   ) is
      In_Pos  : Natural := 0;
      Out_Pos : Natural := 0;
   begin
      --  Format: 8-8-8-8-8-8-4 (52 chars → 59 chars with 6 dashes)
      --  Copy 8 chars, add dash, repeat 6 times (groups 0-5)
      for Chunk in 0 .. 5 loop
         pragma Loop_Invariant (In_Pos = Chunk * 8);
         pragma Loop_Invariant (Out_Pos = Chunk * 9);

         for I in 0 .. 7 loop
            pragma Loop_Invariant (In_Pos + I <= 47);
            pragma Loop_Invariant (Out_Pos + I <= 53);
            Output (Out_Pos + I) := Input (In_Pos + I);
         end loop;

         In_Pos := In_Pos + 8;
         Out_Pos := Out_Pos + 8;

         Output (Out_Pos) := '-';
         Out_Pos := Out_Pos + 1;
      end loop;

      --  Copy final 4 chars (group 6, positions 48-51 → 54-57)
      for I in 0 .. 3 loop
         pragma Loop_Invariant (In_Pos = 48);
         pragma Loop_Invariant (Out_Pos = 54);
         Output (Out_Pos + I) := Input (In_Pos + I);
      end loop;
   end Chunk_Payload;

   --  Unchunk payload: verify dashes and extract chars
   --  Format: 8-8-8-8-8-8-4 (58 chars input → 52 chars output)
   procedure Unchunk_Payload (
      Input   : in  Chunked_Payload;
      Output  : out Payload_Base32;
      Success : out Boolean
   ) is
      In_Pos  : Natural := 0;
      Out_Pos : Natural := 0;
   begin
      Success := True;

      --  Extract 8 chars, verify dash, repeat 6 times (chunks 0-5)
      for Chunk in 0 .. 5 loop
         pragma Loop_Invariant (In_Pos = Chunk * 9);
         pragma Loop_Invariant (Out_Pos = Chunk * 8);
         pragma Loop_Invariant (In_Pos <= 45);
         pragma Loop_Invariant (Out_Pos <= 40);

         for I in 0 .. 7 loop
            pragma Loop_Invariant (In_Pos + I <= 52);
            pragma Loop_Invariant (Out_Pos + I <= 47);
            Output (Out_Pos + I) := Input (In_Pos + I);
         end loop;

         In_Pos := In_Pos + 8;
         Out_Pos := Out_Pos + 8;

         --  Verify dash after each of the 6 chunks
         if Input (In_Pos) /= '-' then
            Success := False;
            return;
         end if;
         In_Pos := In_Pos + 1;
      end loop;

      --  Extract final 4 chars (positions 54-57 → 48-51)
      --  In_Pos = 54, Out_Pos = 48 after 6 iterations
      for I in 0 .. 3 loop
         pragma Loop_Invariant (I in 0 .. 3);
         Output (48 + I) := Input (54 + I);
      end loop;
   end Unchunk_Payload;

end Anubis_Address_Base32;
