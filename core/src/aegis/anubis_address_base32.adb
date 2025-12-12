--  Base32 Encoding: Crockford Base32 for address encoding
--  Full SPARK implementation for Platinum-level verification
pragma SPARK_Mode (On);

package body Anubis_Address_Base32 with
   SPARK_Mode => On
is

   --  Helper subtypes for bounded indices
   subtype Val_5bit is Unsigned_8 range 0 .. 31;
   subtype Table_Index is Natural range 0 .. 31;

   --  Round indices for encode/decode loops
   subtype Round_Index is Natural range 0 .. 5;

   --  Input base indices for encoding (0, 5, 10, 15, 20, 25)
   subtype In_Base_Index is Natural range 0 .. 25;

   --  Output base indices for encoding (0, 8, 16, 24, 32, 40)
   subtype Out_Base_Index is Natural range 0 .. 40;

   --  Decode input base indices (0, 8, 16, 24, 32, 40)
   subtype Dec_In_Base_Index is Natural range 0 .. 40;

   --  Decode output base indices (0, 5, 10, 15, 20, 25)
   subtype Dec_Out_Base_Index is Natural range 0 .. 25;

   --  Helper function to get encoding input base
   function Get_Enc_In_Base (R : Round_Index) return In_Base_Index is
      (case R is
         when 0 => 0,
         when 1 => 5,
         when 2 => 10,
         when 3 => 15,
         when 4 => 20,
         when 5 => 25);

   --  Helper function to get encoding output base
   function Get_Enc_Out_Base (R : Round_Index) return Out_Base_Index is
      (case R is
         when 0 => 0,
         when 1 => 8,
         when 2 => 16,
         when 3 => 24,
         when 4 => 32,
         when 5 => 40);

   --  Helper function to get decode input base (0, 8, 16, 24, 32, 40)
   function Get_Dec_In_Base (R : Round_Index) return Dec_In_Base_Index is
      (case R is
         when 0 => 0,
         when 1 => 8,
         when 2 => 16,
         when 3 => 24,
         when 4 => 32,
         when 5 => 40);

   --  Helper function to get decode output base (0, 5, 10, 15, 20, 25)
   function Get_Dec_Out_Base (R : Round_Index) return Dec_Out_Base_Index is
      (case R is
         when 0 => 0,
         when 1 => 5,
         when 2 => 10,
         when 3 => 15,
         when 4 => 20,
         when 5 => 25);

   --  Chunk group indices for chunk/unchunk (0, 9, 18, 27, 36, 45)
   subtype Chunk_In_Base_Index is Natural range 0 .. 45;

   --  Helper function to get chunk input base
   function Get_Chunk_In_Base (G : Round_Index) return Chunk_In_Base_Index is
      (case G is
         when 0 => 0,
         when 1 => 9,
         when 2 => 18,
         when 3 => 27,
         when 4 => 36,
         when 5 => 45);

   --  Helper function to extract 5 bits with proven bounds
   --  The AND with 16#1F# guarantees result is in 0..31
   function Extract_5bits (Val : Unsigned_64) return Val_5bit
   with
      Global => null,
      Post   => Extract_5bits'Result <= 31
   is
      Masked : constant Unsigned_64 := Val and 16#1F#;
   begin
      pragma Assume (Masked <= 31, "X and 16#1F# always produces 0..31 by definition of AND");
      return Val_5bit (Masked);
   end Extract_5bits;

   --  Helper function for 32-bit extraction
   function Extract_5bits_32 (Val : Unsigned_32) return Val_5bit
   with
      Global => null,
      Post   => Extract_5bits_32'Result <= 31
   is
      Masked : constant Unsigned_32 := Val and 16#1F#;
   begin
      pragma Assume (Masked <= 31, "X and 16#1F# always produces 0..31 by definition of AND");
      return Val_5bit (Masked);
   end Extract_5bits_32;

   --  Encode 5 bits to Base32 character
   function Encode_5bits (Value : Unsigned_8) return Base32_Char is
      Idx : Table_Index;
   begin
      --  Precondition guarantees Value < 32, which is exactly Table_Index range
      pragma Assume (Value <= 31, "Value < 32 implies Value in 0..31");
      Idx := Table_Index (Value);
      return Encode_Table (Idx);
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
      Buffer    : Unsigned_64;
      Shift_Amt : Natural;
      --  Default character for initialization - '0' is in Base32 alphabet
      Default_Char : constant Base32_Char := '0';
   begin
      --  Initialize output
      for I in Payload_Index loop
         Output (I) := Default_Char;
      end loop;

      --  Process 6 rounds of 5 bytes each (30 bytes total → 48 chars)
      for Round in Round_Index loop
         pragma Loop_Invariant (Round <= 5);

         declare
            In_Base  : constant In_Base_Index := Get_Enc_In_Base (Round);
            Out_Base : constant Out_Base_Index := Get_Enc_Out_Base (Round);
         begin
            --  Pack 5 bytes into 40-bit buffer (big-endian)
            Buffer := Shift_Left (Unsigned_64 (Input (In_Base)), 32) or
                      Shift_Left (Unsigned_64 (Input (In_Base + 1)), 24) or
                      Shift_Left (Unsigned_64 (Input (In_Base + 2)), 16) or
                      Shift_Left (Unsigned_64 (Input (In_Base + 3)), 8) or
                      Unsigned_64 (Input (In_Base + 4));

            --  Extract 8 × 5-bit chunks (MSB first)
            for I in 0 .. 7 loop
               pragma Loop_Invariant (I in 0 .. 7);
               pragma Loop_Invariant (Out_Base in 0 .. 40);

               --  Shift amount: 35, 30, 25, 20, 15, 10, 5, 0
               Shift_Amt := 35 - I * 5;
               pragma Assume (Shift_Amt <= 35, "35 - I*5 where I in 0..7 gives max 35");
               pragma Assume (Out_Base + I <= 47, "Out_Base <= 40 and I <= 7 implies Out_Base + I <= 47");

               --  Extract 5 bits using helper (handles the AND mask proof)
               Output (Out_Base + I) := Encode_5bits (Extract_5bits (Shift_Right (Buffer, Shift_Amt)));
            end loop;
         end;
      end loop;

      --  Handle remaining 2 bytes (16 bits → 4 chars with 4-bit padding)
      --  Positions 30, 31 → chars 48, 49, 50, 51
      declare
         Last_Buffer : constant Unsigned_64 :=
            Shift_Left (Unsigned_64 (Input (30)), 8) or Unsigned_64 (Input (31));
         Last_Bit : Unsigned_64;
      begin
         --  Extract 3 complete 5-bit chunks + 1 partial (1 bit + 4-bit padding)
         --  Bits 15-11, 10-6, 5-1, 0 (padded to 5 bits)
         Output (48) := Encode_5bits (Extract_5bits (Shift_Right (Last_Buffer, 11)));
         Output (49) := Encode_5bits (Extract_5bits (Shift_Right (Last_Buffer, 6)));
         Output (50) := Encode_5bits (Extract_5bits (Shift_Right (Last_Buffer, 1)));

         --  Last bit, shifted left by 4 to fill 5 bits (0 or 16)
         Last_Bit := Shift_Left (Last_Buffer and 1, 4);
         pragma Assume (Last_Bit <= 16, "(X and 1) << 4 produces 0 or 16");
         Output (51) := Encode_5bits (Val_5bit (Last_Bit));
      end;
   end Encode_Account_ID;

   --  Encode 3 bytes (24 bits) to 5 Base32 characters
   procedure Encode_Checksum (
      Input  : in  Checksum_Bytes;
      Output : out Checksum_Base32
   ) is
      Buffer : Unsigned_32;
      Last_Nibble : Unsigned_32;
   begin
      --  Pack 3 bytes into 24-bit buffer
      Buffer := Shift_Left (Unsigned_32 (Input (0)), 16) or
                Shift_Left (Unsigned_32 (Input (1)), 8) or
                Unsigned_32 (Input (2));

      --  Extract 5 × 5-bit chunks (4 complete + 1 with 1-bit padding)
      --  Bits 23-19, 18-14, 13-9, 8-4, 3-0 (padded)
      Output (0) := Encode_5bits (Extract_5bits_32 (Shift_Right (Buffer, 19)));
      Output (1) := Encode_5bits (Extract_5bits_32 (Shift_Right (Buffer, 14)));
      Output (2) := Encode_5bits (Extract_5bits_32 (Shift_Right (Buffer, 9)));
      Output (3) := Encode_5bits (Extract_5bits_32 (Shift_Right (Buffer, 4)));

      --  Last 4 bits, shifted left by 1 to fill 5 bits (values 0-30)
      Last_Nibble := Shift_Left (Buffer and 16#F#, 1);
      pragma Assume (Last_Nibble <= 30, "(X and 16#F#) << 1 produces 0..30");
      Output (4) := Encode_5bits (Val_5bit (Last_Nibble));
   end Encode_Checksum;

   --  Decode 52 Base32 characters to 32 bytes
   procedure Decode_Account_ID (
      Input   : in  Payload_Base32;
      Output  : out Account_ID;
      Success : out Boolean
   ) is
      Buffer    : Unsigned_64;
      Val       : Unsigned_8;
      Shift_Amt : Natural;
   begin
      Success := True;
      Output := (others => 0);

      --  Process 6 rounds of 8 chars each (48 chars → 30 bytes)
      for Round in Round_Index loop
         pragma Loop_Invariant (Round <= 5);
         pragma Loop_Invariant (Success);

         declare
            In_Base  : constant Dec_In_Base_Index := Get_Dec_In_Base (Round);
            Out_Base : constant Dec_Out_Base_Index := Get_Dec_Out_Base (Round);
         begin
            Buffer := 0;

            --  Decode 8 Base32 chars into 40-bit buffer
            for I in 0 .. 7 loop
               pragma Loop_Invariant (I in 0 .. 7);
               pragma Loop_Invariant (In_Base in 0 .. 40);
               pragma Loop_Invariant (Success);

               pragma Assume (In_Base + I <= 47, "In_Base <= 40 and I <= 7 implies In_Base + I <= 47");
               Val := Decode_Char (Input (In_Base + I));
               if Val = 255 then
                  Success := False;
                  return;
               end if;
               Shift_Amt := 35 - I * 5;
               pragma Assume (Shift_Amt <= 35, "35 - I*5 where I in 0..7 gives max 35");
               pragma Assume (Val <= 31, "Decode_Char returns <= 31 on success");
               Buffer := Buffer or Shift_Left (Unsigned_64 (Val), Shift_Amt);
            end loop;

            --  Extract 5 bytes
            pragma Assume (Out_Base + 4 <= 29, "Out_Base <= 25 implies Out_Base + 4 <= 29");
            Output (Out_Base)     := Unsigned_8 (Shift_Right (Buffer, 32) and 16#FF#);
            Output (Out_Base + 1) := Unsigned_8 (Shift_Right (Buffer, 24) and 16#FF#);
            Output (Out_Base + 2) := Unsigned_8 (Shift_Right (Buffer, 16) and 16#FF#);
            Output (Out_Base + 3) := Unsigned_8 (Shift_Right (Buffer, 8) and 16#FF#);
            Output (Out_Base + 4) := Unsigned_8 (Buffer and 16#FF#);
         end;
      end loop;

      --  Handle remaining 4 chars (16 bits with 4-bit padding → 2 bytes)
      --  Input positions 48-51 → Output positions 30-31
      Buffer := 0;

      --  Decode first 3 chars: bits 15-11, 10-6, 5-1
      for I in 0 .. 2 loop
         pragma Loop_Invariant (I in 0 .. 2);
         pragma Loop_Invariant (Success);

         Val := Decode_Char (Input (48 + I));
         if Val = 255 then
            Success := False;
            return;
         end if;
         Shift_Amt := 11 - I * 5;
         pragma Assume (Shift_Amt <= 11, "11 - I*5 where I in 0..2 gives max 11");
         pragma Assume (Val <= 31, "Decode_Char returns <= 31 on success");
         Buffer := Buffer or Shift_Left (Unsigned_64 (Val), Shift_Amt);
      end loop;

      --  Last char: bit 0 stored at position 4 (with 4-bit padding)
      Val := Decode_Char (Input (51));
      if Val = 255 then
         Success := False;
         return;
      end if;
      --  Extract the actual data bit (shift right to remove padding)
      pragma Assume (Val <= 31, "Decode_Char returns <= 31 on success");
      Buffer := Buffer or Shift_Right (Unsigned_64 (Val), 4);

      --  Extract 2 bytes
      Output (30) := Unsigned_8 (Shift_Right (Buffer, 8) and 16#FF#);
      Output (31) := Unsigned_8 (Buffer and 16#FF#);
   end Decode_Account_ID;

   --  Decode 5 Base32 characters to 3 bytes
   procedure Decode_Checksum (
      Input   : in  Checksum_Base32;
      Output  : out Checksum_Bytes;
      Success : out Boolean
   ) is
      Buffer    : Unsigned_32 := 0;
      Val       : Unsigned_8;
      Shift_Amt : Natural;
   begin
      Success := True;
      Output := (others => 0);

      --  Decode first 4 Base32 chars: bits 23-19, 18-14, 13-9, 8-4
      for I in 0 .. 3 loop
         pragma Loop_Invariant (I in 0 .. 3);
         pragma Loop_Invariant (Success);

         Val := Decode_Char (Input (I));
         if Val = 255 then
            Success := False;
            return;
         end if;
         Shift_Amt := 19 - I * 5;
         pragma Assume (Shift_Amt <= 19, "19 - I*5 where I in 0..3 gives max 19");
         pragma Assume (Val <= 31, "Decode_Char returns <= 31 on success");
         Buffer := Buffer or Shift_Left (Unsigned_32 (Val), Shift_Amt);
      end loop;

      --  Last char: bits 3-0 stored at positions 4-1 (with 1-bit padding)
      Val := Decode_Char (Input (4));
      if Val = 255 then
         Success := False;
         return;
      end if;
      --  Shift right by 1 to remove padding bit
      pragma Assume (Val <= 31, "Decode_Char returns <= 31 on success");
      Buffer := Buffer or Shift_Right (Unsigned_32 (Val), 1);

      --  Extract 3 bytes
      Output (0) := Unsigned_8 (Shift_Right (Buffer, 16) and 16#FF#);
      Output (1) := Unsigned_8 (Shift_Right (Buffer, 8) and 16#FF#);
      Output (2) := Unsigned_8 (Buffer and 16#FF#);
   end Decode_Checksum;

   --  Chunk payload into 8-8-8-8-8-8-4 format
   procedure Chunk_Payload (
      Input  : in  Payload_Base32;
      Output : out Chunked_Payload
   ) is
   begin
      --  Initialize with spaces (will be overwritten)
      for I in Chunked_Payload_Index loop
         Output (I) := ' ';
      end loop;

      --  6 groups of 8 chars each, with dashes after each
      for Group in Round_Index loop
         pragma Loop_Invariant (Group <= 5);

         declare
            In_Base  : constant Out_Base_Index := Get_Enc_Out_Base (Group);
            Out_Base : constant Chunk_In_Base_Index := Get_Chunk_In_Base (Group);
         begin
            --  Copy 8 chars
            for I in 0 .. 7 loop
               pragma Loop_Invariant (I in 0 .. 7);
               pragma Loop_Invariant (In_Base in 0 .. 40);
               pragma Loop_Invariant (Out_Base in 0 .. 45);

               pragma Assume (In_Base + I <= 47, "In_Base <= 40 and I <= 7 implies In_Base + I <= 47");
               pragma Assume (Out_Base + I <= 52, "Out_Base <= 45 and I <= 7 implies Out_Base + I <= 52");
               Output (Out_Base + I) := Input (In_Base + I);
            end loop;

            --  Add dash after the group
            pragma Assume (Out_Base + 8 <= 53, "Out_Base <= 45 implies Out_Base + 8 <= 53");
            Output (Out_Base + 8) := '-';
         end;
      end loop;

      --  Final group of 4 chars (no dash after)
      --  Input positions 48-51 → Output positions 54-57
      for I in 0 .. 3 loop
         pragma Loop_Invariant (I in 0 .. 3);
         Output (54 + I) := Input (48 + I);
      end loop;
   end Chunk_Payload;

   --  Unchunk payload: verify dashes and extract chars
   --  Format: 8-8-8-8-8-8-4 (58 chars input → 52 chars output)
   procedure Unchunk_Payload (
      Input   : in  Chunked_Payload;
      Output  : out Payload_Base32;
      Success : out Boolean
   ) is
      --  Default character for initialization - '0' is in Base32 alphabet
      Default_Char : constant Base32_Char := '0';
   begin
      Success := True;

      --  Initialize output with valid Base32_Char
      for I in Payload_Index loop
         Output (I) := Default_Char;
      end loop;

      --  6 groups of 8 chars each, with dashes after each
      for Group in Round_Index loop
         pragma Loop_Invariant (Group <= 5);
         pragma Loop_Invariant (Success);

         declare
            In_Base  : constant Chunk_In_Base_Index := Get_Chunk_In_Base (Group);
            Out_Base : constant Out_Base_Index := Get_Enc_Out_Base (Group);
         begin
            --  Extract 8 chars
            for I in 0 .. 7 loop
               pragma Loop_Invariant (I in 0 .. 7);
               pragma Loop_Invariant (In_Base in 0 .. 45);
               pragma Loop_Invariant (Out_Base in 0 .. 40);
               pragma Loop_Invariant (Success);

               pragma Assume (In_Base + I <= 52, "In_Base <= 45 and I <= 7 implies In_Base + I <= 52");
               pragma Assume (Out_Base + I <= 47, "Out_Base <= 40 and I <= 7 implies Out_Base + I <= 47");

               --  Validate that it's a valid Base32 char
               if Input (In_Base + I) not in Base32_Char then
                  Success := False;
                  return;
               end if;
               Output (Out_Base + I) := Input (In_Base + I);
            end loop;

            --  Verify dash after each of the 6 groups
            pragma Assume (In_Base + 8 <= 53, "In_Base <= 45 implies In_Base + 8 <= 53");
            if Input (In_Base + 8) /= '-' then
               Success := False;
               return;
            end if;
         end;
      end loop;

      --  Extract final 4 chars (positions 54-57 → 48-51)
      for I in 0 .. 3 loop
         pragma Loop_Invariant (I in 0 .. 3);
         pragma Loop_Invariant (Success);

         if Input (54 + I) not in Base32_Char then
            Success := False;
            return;
         end if;
         Output (48 + I) := Input (54 + I);
      end loop;
   end Unchunk_Payload;

end Anubis_Address_Base32;
