pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_SHA3 with
   SPARK_Mode => On
is

   --  Convert 8 bytes (little-endian) to Lane
   function Bytes_To_Lane (B : Byte_Array) return Lane is
      Result_U64 : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         declare
            Shift_Amount : constant Natural := I * 8;
            Byte_As_U64 : constant Unsigned_64 := Unsigned_64 (B (B'First + I));
         begin
            Result_U64 := Result_U64 or Shift_Left (Byte_As_U64, Shift_Amount);
         end;
      end loop;
      return Lane (Result_U64);
   end Bytes_To_Lane;

   --  Convert Lane to 8 bytes (little-endian)
   procedure Lane_To_Bytes (L : Lane; B : out Byte_Array) is
      L_U64 : constant Unsigned_64 := Unsigned_64 (L);
      B_First_Copy : constant Natural := B'First;
      B_Last_Copy : constant Natural := B'Last;
   begin
      --  Initialize output to ensure all bytes are written
      B := (others => 0);
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         pragma Loop_Invariant (B'First = B_First_Copy);
         pragma Loop_Invariant (B'Last = B_Last_Copy);
         declare
            Shift_Amount : constant Natural := I * 8;
            Shifted : constant Unsigned_64 := Shift_Right (L_U64, Shift_Amount);
         begin
            B (B'First + I) := Byte (Shifted and 16#FF#);
         end;
      end loop;
   end Lane_To_Bytes;

   --  Absorb one rate-sized block into state
   procedure Absorb_Block (
      State : in out State_Array;
      Block : Byte_Array;
      Rate  : Positive
   ) is
      Num_Lanes : constant Positive := Rate / 8;
   begin
      --  XOR block into state as lanes (little-endian)
      for I in 0 .. Num_Lanes - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Num_Lanes);
         declare
            X : constant Lane_Index := Lane_Index (I mod 5);
            Y : constant Lane_Index := Lane_Index (I / 5);
            Lane_Bytes : constant Byte_Array := Block (Block'First + I * 8 .. Block'First + I * 8 + 7);
            Block_Lane : constant Lane := Bytes_To_Lane (Lane_Bytes);
         begin
            State (X, Y) := State (X, Y) xor Block_Lane;
         end;
      end loop;

      --  Apply Keccak-f permutation
      Keccak_F (State);
   end Absorb_Block;

   --  Squeeze Rate bytes from state
   procedure Squeeze_Block (
      State  : State_Array;
      Output : out Byte_Array;
      Rate   : Positive
   ) is
      Num_Lanes : constant Positive := Rate / 8;
      Output_First_Copy : constant Natural := Output'First;
      Output_Last_Copy : constant Natural := Output'Last;
   begin
      --  Initialize output to ensure all bytes are written
      Output := (others => 0);
      --  Extract lanes from state to output (little-endian)
      for I in 0 .. Num_Lanes - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Num_Lanes);
         pragma Loop_Invariant (Output'First = Output_First_Copy);
         pragma Loop_Invariant (Output'Last = Output_Last_Copy);
         declare
            X : constant Lane_Index := Lane_Index (I mod 5);
            Y : constant Lane_Index := Lane_Index (I / 5);
            State_Lane : constant Lane := State (X, Y);
            Out_Slice : Byte_Array (0 .. 7);
         begin
            Lane_To_Bytes (State_Lane, Out_Slice);
            --  Copy 8 bytes to output
            for J in 0 .. 7 loop
               pragma Loop_Invariant (J in 0 .. 7);
               pragma Loop_Invariant (Output'First = Output_First_Copy);
               pragma Loop_Invariant (Output'Last = Output_Last_Copy);
               Output (Output'First + I * 8 + J) := Out_Slice (J);
            end loop;
         end;
      end loop;
   end Squeeze_Block;

   --  Generic SHA3 sponge implementation
   procedure SHA3_Sponge (
      Message    : Byte_Array;
      Digest     : out Byte_Array;
      Rate       : Positive;
      Suffix     : Byte
   ) with
      Always_Terminates => True,
      Pre => Rate <= 200 and then Rate mod 8 = 0 and then
             Digest'Last < Natural'Last and then  -- Prevents Digest'Length overflow
             Message'Last < Natural'Last and then  -- Prevents Message'Length overflow
             Digest'First <= Digest'Last and then  -- Ensures non-empty (Length > 0)
             Digest'Last - Digest'First < Rate  -- Length - 1 < Rate means Length <= Rate
   is
      State : State_Array := (others => (others => 0));
      Pos : Natural := 0;
   begin
      --  Initialize Digest to ensure all paths write it
      Digest := (others => 0);

      --  Absorb phase: process full rate-sized blocks
      --  Use subtraction to avoid overflow: Pos <= Message'Length - Rate
      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Message'Length >= Rate);
         pragma Loop_Variant (Decreases => Message'Length - Pos);
         declare
            Block_First : constant Natural := Message'First + Pos;
            Block_Last  : constant Natural := Block_First + Rate - 1;
            Block : constant Byte_Array := Message (Block_First .. Block_Last);
         begin
            Absorb_Block (State, Block, Rate);
            Pos := Pos + Rate;
         end;
      end loop;

      --  Pad and absorb final block
      declare
         Remaining : constant Natural := Message'Length - Pos;
         Final_Block : Byte_Array (0 .. Rate - 1) := (others => 0);
      begin
         --  Copy remaining message bytes
         for I in 0 .. Remaining - 1 loop
            pragma Loop_Invariant (I >= 0 and I < Remaining);
            Final_Block (I) := Message (Message'First + Pos + I);
         end loop;

         --  Apply SHA3 padding: 10*1
         --  Append domain separator (0x06 for SHA3)
         Final_Block (Remaining) := Suffix;

         --  Set final bit of padding (multi-rate padding)
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;

         Absorb_Block (State, Final_Block, Rate);
      end;

      --  Squeeze phase: extract digest
      if Digest'Length <= Rate then
         --  Single squeeze sufficient
         declare
            Squeezed : Byte_Array (0 .. Rate - 1);
         begin
            Squeeze_Block (State, Squeezed, Rate);
            Digest := Squeezed (0 .. Digest'Length - 1);
         end;
      else
         --  Multiple squeezes needed (not typical for SHA3, but handle it)
         declare
            Output_Pos : Natural := 0;
         begin
            while Output_Pos < Digest'Length loop
               pragma Loop_Invariant (Output_Pos >= 0);
               pragma Loop_Invariant (Output_Pos <= Digest'Length);
               pragma Loop_Variant (Decreases => Digest'Length - Output_Pos);
               declare
                  Squeezed : Byte_Array (0 .. Rate - 1);
                  Copy_Len : constant Natural := Natural'Min (Rate, Digest'Length - Output_Pos);
               begin
                  Squeeze_Block (State, Squeezed, Rate);
                  for I in 0 .. Copy_Len - 1 loop
                     pragma Loop_Invariant (I >= 0 and I < Copy_Len);
                     Digest (Digest'First + Output_Pos + I) := Squeezed (I);
                  end loop;
                  Output_Pos := Output_Pos + Copy_Len;
                  if Output_Pos < Digest'Length then
                     Keccak_F (State);
                  end if;
               end;
            end loop;
         end;
      end if;
   end SHA3_Sponge;

   --  SHA3-256
   procedure SHA3_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) is
   begin
      SHA3_Sponge (Message, Digest, Rate_256, SHA3_Suffix);
   end SHA3_256;

   --  SHA3-384
   procedure SHA3_384 (
      Message : Byte_Array;
      Digest  : out SHA3_384_Digest
   ) is
   begin
      SHA3_Sponge (Message, Digest, Rate_384, SHA3_Suffix);
   end SHA3_384;

   --  SHA3-512
   procedure SHA3_512 (
      Message : Byte_Array;
      Digest  : out SHA3_512_Digest
   ) is
   begin
      SHA3_Sponge (Message, Digest, Rate_512, SHA3_Suffix);
   end SHA3_512;

   --  Keccak-256 (Ethereum-compatible)
   --  Uses domain separator 0x01 instead of SHA3"s 0x06
   procedure Keccak_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) is
   begin
      SHA3_Sponge (Message, Digest, Rate_256, Keccak_Suffix);
   end Keccak_256;

   --  SHAKE sponge implementation (XOF variant)
   procedure SHAKE_Sponge (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Rate          : Positive;
      Suffix        : Byte;
      Output_Length : Positive
   ) with
      Always_Terminates => True,
      Pre => Rate <= 200 and then Rate mod 8 = 0 and then
             Output'Last < Natural'Last and then  -- Prevents Output'Length overflow
             Message'Last < Natural'Last and then  -- Prevents Message'Length overflow
             Output'First <= Output'Last and then  -- Non-empty output
             Output'Last - Output'First + 1 = Output_Length  -- Avoids 'Length
   is
      State : State_Array := (others => (others => 0));
      Pos : Natural := 0;
      Output_Pos : Natural := 0;
      Output_First_Copy : constant Natural := Output'First;
      Output_Last_Copy : constant Natural := Output'Last;
   begin
      --  Initialize Output to ensure all paths write it
      Output := (others => 0);

      --  Absorb phase: process full rate-sized blocks
      --  Use subtraction to avoid overflow
      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Message'Length >= Rate);
         pragma Loop_Invariant (Output'First = Output_First_Copy);
         pragma Loop_Invariant (Output'Last = Output_Last_Copy);
         pragma Loop_Variant (Decreases => Message'Length - Pos);
         declare
            Block_First : constant Natural := Message'First + Pos;
            Block_Last  : constant Natural := Block_First + Rate - 1;
            Block : constant Byte_Array := Message (Block_First .. Block_Last);
         begin
            Absorb_Block (State, Block, Rate);
            Pos := Pos + Rate;
         end;
      end loop;

      --  Pad and absorb final block
      declare
         Remaining : constant Natural := Message'Length - Pos;
         Final_Block : Byte_Array (0 .. Rate - 1) := (others => 0);
      begin
         --  Copy remaining message bytes
         for I in 0 .. Remaining - 1 loop
            pragma Loop_Invariant (I >= 0 and I < Remaining);
            Final_Block (I) := Message (Message'First + Pos + I);
         end loop;

         --  Apply SHAKE padding: 10*1 with domain separator 0x1F
         Final_Block (Remaining) := Suffix;

         --  Set final bit of padding
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;

         Absorb_Block (State, Final_Block, Rate);
      end;

      --  Squeeze phase: extract arbitrary-length output
      while Output_Pos < Output_Length loop
         pragma Loop_Invariant (Output_Pos >= 0);
         pragma Loop_Invariant (Output_Pos <= Output_Length);
         pragma Loop_Invariant (Output'First = Output_First_Copy);
         pragma Loop_Invariant (Output'Last = Output_Last_Copy);
         pragma Loop_Variant (Decreases => Output_Length - Output_Pos);

         declare
            Squeezed : Byte_Array (0 .. Rate - 1);
            Copy_Len : constant Natural :=
               Natural'Min (Rate, Output_Length - Output_Pos);
         begin
            Squeeze_Block (State, Squeezed, Rate);

            --  Copy bytes to output
            for I in 0 .. Copy_Len - 1 loop
               pragma Loop_Invariant (I >= 0 and I < Copy_Len);
               pragma Loop_Invariant (Output'First = Output_First_Copy);
               pragma Loop_Invariant (Output'Last = Output_Last_Copy);
               Output (Output'First + Output_Pos + I) := Squeezed (I);
            end loop;

            Output_Pos := Output_Pos + Copy_Len;

            --  Apply Keccak-f for next squeeze (if needed)
            if Output_Pos < Output_Length then
               Keccak_F (State);
            end if;
         end;
      end loop;
   end SHAKE_Sponge;

   --  SHAKE128
   procedure SHAKE128 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) is
   begin
      SHAKE_Sponge (Message, Output, Rate_SHAKE128, SHAKE_Suffix, Output_Length);
   end SHAKE128;

   --  SHAKE256
   procedure SHAKE256 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) is
   begin
      SHAKE_Sponge (Message, Output, Rate_SHAKE256, SHAKE_Suffix, Output_Length);
   end SHAKE256;

end Anubis_SHA3;
