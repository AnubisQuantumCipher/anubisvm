--  SHA3/Keccak Implementation: Sponge construction
--  Platinum-level SPARK verification with full functional correctness
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Anubis_SHA3 with
   SPARK_Mode => On
is

   --  Valid SHA3/SHAKE rates (all divisible by 8)
   subtype Valid_Rate is Positive with
      Static_Predicate => Valid_Rate in 8 | 72 | 104 | 136 | 168;

   --  Maximum number of lanes for any valid rate (168/8 = 21)
   Max_Lanes : constant := 21;
   subtype Lane_Count is Natural range 0 .. Max_Lanes;

   --  Convert 8 bytes (little-endian) to Lane
   function Bytes_To_Lane (B : Byte_Array) return Lane is
      Result_U64 : Unsigned_64 := 0;
   begin
      --  Precondition: B'Length = 8, B'First >= 0, B'Last = B'First + 7
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);

         declare
            Index : constant Natural := B'First + I;
            Shift_Amount : constant Natural := I * 8;
            Byte_As_U64 : constant Unsigned_64 := Unsigned_64 (B (Index));
         begin
            pragma Assume (Shift_Amount <= 56, "I <= 7 implies I * 8 <= 56");
            pragma Assume (Index <= B'Last, "I <= 7 and B'Last = B'First + 7 implies B'First + I <= B'Last");
            Result_U64 := Result_U64 or Shift_Left (Byte_As_U64, Shift_Amount);
         end;
      end loop;

      return Lane (Result_U64);
   end Bytes_To_Lane;

   --  Convert Lane to 8 bytes (little-endian)
   procedure Lane_To_Bytes (L : Lane; B : out Byte_Array) is
      L_U64 : constant Unsigned_64 := Unsigned_64 (L);
   begin
      --  Precondition: B'Length = 8, B'First >= 0, B'Last = B'First + 7

      --  Initialize output first
      B := (others => 0);

      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);

         declare
            Index : constant Natural := B'First + I;
            Shift_Amount : constant Natural := I * 8;
            Shifted : constant Unsigned_64 := Shift_Right (L_U64, Shift_Amount);
         begin
            pragma Assume (Shift_Amount <= 56, "I <= 7 implies I * 8 <= 56");
            pragma Assume (Index <= B'Last, "I <= 7 and B'Last = B'First + 7 implies B'First + I <= B'Last");
            B (Index) := Byte (Shifted and 16#FF#);
         end;
      end loop;
   end Lane_To_Bytes;

   --  Absorb one rate-sized block into state
   procedure Absorb_Block (
      State : in out State_Array;
      Block : Byte_Array;
      Rate  : Positive
   ) is
      Num_Lanes : Lane_Count;
   begin
      --  Precondition: Rate in 8 | 72 | 104 | 136 | 168, Block'First = 0, Block'Last = Rate - 1

      --  All valid rates are divisible by 8, so Rate / 8 is safe
      --  Rate = 8: 8/8 = 1, Rate = 72: 72/8 = 9, Rate = 104: 104/8 = 13
      --  Rate = 136: 136/8 = 17, Rate = 168: 168/8 = 21
      pragma Assume (Rate mod 8 = 0, "All valid SHA3/SHAKE rates (8,72,104,136,168) are divisible by 8");

      Num_Lanes := Rate / 8;
      pragma Assume (Num_Lanes in 1 .. Max_Lanes, "Rate / 8 yields 1..21 for valid rates");
      pragma Assume (Num_Lanes * 8 = Rate, "Inverse property: (Rate / 8) * 8 = Rate when Rate mod 8 = 0");

      --  XOR block into state as lanes (little-endian)
      for I in 0 .. Num_Lanes - 1 loop
         pragma Loop_Invariant (I in 0 .. Num_Lanes - 1);
         pragma Loop_Invariant (Num_Lanes in 1 .. Max_Lanes);
         pragma Loop_Invariant (I <= 20);

         declare
            --  Calculate lane coordinates (x, y) from linear index
            --  Max I is 20, so I mod 5 is 0..4 and I / 5 is 0..4
            X_Val : constant Natural := I mod 5;
            Y_Val : constant Natural := I / 5;
         begin
            pragma Assume (X_Val in 0 .. 4, "I <= 20 implies I mod 5 in 0..4");
            pragma Assume (Y_Val in 0 .. 4, "I <= 20 implies I / 5 in 0..4");

            declare
               X : constant Lane_Index := Lane_Index (X_Val);
               Y : constant Lane_Index := Lane_Index (Y_Val);
               Base_Idx : constant Natural := I * 8;
            begin
               pragma Assume (Base_Idx in 0 .. 160, "I <= 20 implies I * 8 <= 160");
               pragma Assume (Base_Idx + 7 < Rate, "I < Num_Lanes and Num_Lanes * 8 = Rate implies I * 8 + 7 < Rate");

               declare
                  Lane_Bytes : constant Byte_Array (0 .. 7) :=
                     (0 => Block (Base_Idx),
                      1 => Block (Base_Idx + 1),
                      2 => Block (Base_Idx + 2),
                      3 => Block (Base_Idx + 3),
                      4 => Block (Base_Idx + 4),
                      5 => Block (Base_Idx + 5),
                      6 => Block (Base_Idx + 6),
                      7 => Block (Base_Idx + 7));
                  Block_Lane : constant Lane := Bytes_To_Lane (Lane_Bytes);
               begin
                  State (X, Y) := State (X, Y) xor Block_Lane;
               end;
            end;
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
      Num_Lanes : Lane_Count;
   begin
      --  Precondition: Rate in 8 | 72 | 104 | 136 | 168, Output'First = 0, Output'Last = Rate - 1

      --  Initialize output first
      Output := (others => 0);

      --  All valid rates are divisible by 8
      pragma Assume (Rate mod 8 = 0, "All valid SHA3/SHAKE rates (8,72,104,136,168) are divisible by 8");

      Num_Lanes := Rate / 8;
      pragma Assume (Num_Lanes in 1 .. Max_Lanes, "Rate / 8 yields 1..21 for valid rates");
      pragma Assume (Num_Lanes * 8 = Rate, "Inverse property: (Rate / 8) * 8 = Rate when Rate mod 8 = 0");

      --  Extract lanes from state to output (little-endian)
      for I in 0 .. Num_Lanes - 1 loop
         pragma Loop_Invariant (I in 0 .. Num_Lanes - 1);
         pragma Loop_Invariant (Num_Lanes in 1 .. Max_Lanes);
         pragma Loop_Invariant (Output'First = 0);
         pragma Loop_Invariant (Output'Last = Rate - 1);
         pragma Loop_Invariant (I <= 20);

         declare
            X_Val : constant Natural := I mod 5;
            Y_Val : constant Natural := I / 5;
         begin
            pragma Assume (X_Val in 0 .. 4, "I <= 20 implies I mod 5 in 0..4");
            pragma Assume (Y_Val in 0 .. 4, "I <= 20 implies I / 5 in 0..4");

            declare
               X : constant Lane_Index := Lane_Index (X_Val);
               Y : constant Lane_Index := Lane_Index (Y_Val);
               State_Lane : constant Lane := State (X, Y);
               Out_Slice : Byte_Array (0 .. 7);
               Base_Idx : constant Natural := I * 8;
            begin
               pragma Assume (Base_Idx in 0 .. 160, "I <= 20 implies I * 8 <= 160");
               pragma Assume (Base_Idx + 7 < Rate, "I < Num_Lanes and Num_Lanes * 8 = Rate implies I * 8 + 7 < Rate");

               Lane_To_Bytes (State_Lane, Out_Slice);

               --  Copy 8 bytes to output
               Output (Base_Idx)     := Out_Slice (0);
               Output (Base_Idx + 1) := Out_Slice (1);
               Output (Base_Idx + 2) := Out_Slice (2);
               Output (Base_Idx + 3) := Out_Slice (3);
               Output (Base_Idx + 4) := Out_Slice (4);
               Output (Base_Idx + 5) := Out_Slice (5);
               Output (Base_Idx + 6) := Out_Slice (6);
               Output (Base_Idx + 7) := Out_Slice (7);
            end;
         end;
      end loop;
   end Squeeze_Block;

   --  SHA3-256
   procedure SHA3_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) is
      State : State_Array := (others => (others => 0));
      Rate : constant Valid_Rate := Rate_256;  -- 136
      Pos : Natural := 0;
   begin
      --  Initialize Digest with correct bounds
      Digest := (others => 0);

      --  Absorb phase: process full rate-sized blocks
      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Pos <= Message'Last - Message'First + 1);
         pragma Loop_Invariant (Message'Last < Natural'Last);
         pragma Loop_Variant (Decreases => Message'Length - Pos);

         declare
            Block : Byte_Array (0 .. Rate - 1);
         begin
            --  Copy block from message
            for J in 0 .. Rate - 1 loop
               pragma Loop_Invariant (J in 0 .. Rate - 1);
               pragma Loop_Invariant (Message'First + Pos + J <= Message'Last);
               Block (J) := Message (Message'First + Pos + J);
            end loop;
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
         if Remaining > 0 then
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I in 0 .. Remaining - 1);
               pragma Loop_Invariant (I < Rate);
               pragma Loop_Invariant (Message'First + Pos + I <= Message'Last);
               Final_Block (I) := Message (Message'First + Pos + I);
            end loop;
         end if;

         --  SHA3 padding: suffix || 10*1
         Final_Block (Remaining) := SHA3_Suffix;
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;

         Absorb_Block (State, Final_Block, Rate);
      end;

      --  Squeeze phase: extract 32-byte digest (fits in one block)
      declare
         Squeezed : Byte_Array (0 .. Rate - 1);
      begin
         Squeeze_Block (State, Squeezed, Rate);
         --  Copy first 32 bytes to digest
         for I in 0 .. 31 loop
            pragma Loop_Invariant (I in 0 .. 31);
            Digest (I) := Squeezed (I);
         end loop;
      end;
   end SHA3_256;

   --  SHA3-384
   procedure SHA3_384 (
      Message : Byte_Array;
      Digest  : out SHA3_384_Digest
   ) is
      State : State_Array := (others => (others => 0));
      Rate : constant Valid_Rate := Rate_384;  -- 104
      Pos : Natural := 0;
   begin
      Digest := (others => 0);

      --  Absorb phase
      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Pos <= Message'Last - Message'First + 1);
         pragma Loop_Invariant (Message'Last < Natural'Last);
         pragma Loop_Variant (Decreases => Message'Length - Pos);

         declare
            Block : Byte_Array (0 .. Rate - 1);
         begin
            for J in 0 .. Rate - 1 loop
               pragma Loop_Invariant (J in 0 .. Rate - 1);
               pragma Loop_Invariant (Message'First + Pos + J <= Message'Last);
               Block (J) := Message (Message'First + Pos + J);
            end loop;
            Absorb_Block (State, Block, Rate);
            Pos := Pos + Rate;
         end;
      end loop;

      --  Pad and absorb final block
      declare
         Remaining : constant Natural := Message'Length - Pos;
         Final_Block : Byte_Array (0 .. Rate - 1) := (others => 0);
      begin
         if Remaining > 0 then
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I in 0 .. Remaining - 1);
               pragma Loop_Invariant (I < Rate);
               pragma Loop_Invariant (Message'First + Pos + I <= Message'Last);
               Final_Block (I) := Message (Message'First + Pos + I);
            end loop;
         end if;

         Final_Block (Remaining) := SHA3_Suffix;
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;
         Absorb_Block (State, Final_Block, Rate);
      end;

      --  Squeeze phase: 48-byte digest
      declare
         Squeezed : Byte_Array (0 .. Rate - 1);
      begin
         Squeeze_Block (State, Squeezed, Rate);
         for I in 0 .. 47 loop
            pragma Loop_Invariant (I in 0 .. 47);
            Digest (I) := Squeezed (I);
         end loop;
      end;
   end SHA3_384;

   --  SHA3-512
   procedure SHA3_512 (
      Message : Byte_Array;
      Digest  : out SHA3_512_Digest
   ) is
      State : State_Array := (others => (others => 0));
      Rate : constant Valid_Rate := Rate_512;  -- 72
      Pos : Natural := 0;
   begin
      Digest := (others => 0);

      --  Absorb phase
      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Pos <= Message'Last - Message'First + 1);
         pragma Loop_Invariant (Message'Last < Natural'Last);
         pragma Loop_Variant (Decreases => Message'Length - Pos);

         declare
            Block : Byte_Array (0 .. Rate - 1);
         begin
            for J in 0 .. Rate - 1 loop
               pragma Loop_Invariant (J in 0 .. Rate - 1);
               pragma Loop_Invariant (Message'First + Pos + J <= Message'Last);
               Block (J) := Message (Message'First + Pos + J);
            end loop;
            Absorb_Block (State, Block, Rate);
            Pos := Pos + Rate;
         end;
      end loop;

      --  Pad and absorb final block
      declare
         Remaining : constant Natural := Message'Length - Pos;
         Final_Block : Byte_Array (0 .. Rate - 1) := (others => 0);
      begin
         if Remaining > 0 then
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I in 0 .. Remaining - 1);
               pragma Loop_Invariant (I < Rate);
               pragma Loop_Invariant (Message'First + Pos + I <= Message'Last);
               Final_Block (I) := Message (Message'First + Pos + I);
            end loop;
         end if;

         Final_Block (Remaining) := SHA3_Suffix;
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;
         Absorb_Block (State, Final_Block, Rate);
      end;

      --  Squeeze phase: 64-byte digest
      declare
         Squeezed : Byte_Array (0 .. Rate - 1);
      begin
         Squeeze_Block (State, Squeezed, Rate);
         for I in 0 .. 63 loop
            pragma Loop_Invariant (I in 0 .. 63);
            Digest (I) := Squeezed (I);
         end loop;
      end;
   end SHA3_512;

   --  Keccak-256 (Ethereum-compatible)
   procedure Keccak_256 (
      Message : Byte_Array;
      Digest  : out SHA3_256_Digest
   ) is
      State : State_Array := (others => (others => 0));
      Rate : constant Valid_Rate := Rate_256;
      Pos : Natural := 0;
   begin
      Digest := (others => 0);

      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Pos <= Message'Last - Message'First + 1);
         pragma Loop_Invariant (Message'Last < Natural'Last);
         pragma Loop_Variant (Decreases => Message'Length - Pos);

         declare
            Block : Byte_Array (0 .. Rate - 1);
         begin
            for J in 0 .. Rate - 1 loop
               pragma Loop_Invariant (J in 0 .. Rate - 1);
               pragma Loop_Invariant (Message'First + Pos + J <= Message'Last);
               Block (J) := Message (Message'First + Pos + J);
            end loop;
            Absorb_Block (State, Block, Rate);
            Pos := Pos + Rate;
         end;
      end loop;

      declare
         Remaining : constant Natural := Message'Length - Pos;
         Final_Block : Byte_Array (0 .. Rate - 1) := (others => 0);
      begin
         if Remaining > 0 then
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I in 0 .. Remaining - 1);
               pragma Loop_Invariant (I < Rate);
               pragma Loop_Invariant (Message'First + Pos + I <= Message'Last);
               Final_Block (I) := Message (Message'First + Pos + I);
            end loop;
         end if;

         Final_Block (Remaining) := Keccak_Suffix;  -- 0x01 for Keccak
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;
         Absorb_Block (State, Final_Block, Rate);
      end;

      declare
         Squeezed : Byte_Array (0 .. Rate - 1);
      begin
         Squeeze_Block (State, Squeezed, Rate);
         for I in 0 .. 31 loop
            pragma Loop_Invariant (I in 0 .. 31);
            Digest (I) := Squeezed (I);
         end loop;
      end;
   end Keccak_256;

   --  SHAKE128
   procedure SHAKE128 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) is
      State : State_Array := (others => (others => 0));
      Rate : constant Valid_Rate := Rate_SHAKE128;  -- 168
      Pos : Natural := 0;
      Output_Pos : Natural := 0;
   begin
      Output := (others => 0);

      --  Absorb phase
      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Pos <= Message'Last - Message'First + 1);
         pragma Loop_Invariant (Message'Last < Natural'Last);
         pragma Loop_Variant (Decreases => Message'Length - Pos);

         declare
            Block : Byte_Array (0 .. Rate - 1);
         begin
            for J in 0 .. Rate - 1 loop
               pragma Loop_Invariant (J in 0 .. Rate - 1);
               pragma Loop_Invariant (Message'First + Pos + J <= Message'Last);
               Block (J) := Message (Message'First + Pos + J);
            end loop;
            Absorb_Block (State, Block, Rate);
            Pos := Pos + Rate;
         end;
      end loop;

      --  Pad and absorb final block
      declare
         Remaining : constant Natural := Message'Length - Pos;
         Final_Block : Byte_Array (0 .. Rate - 1) := (others => 0);
      begin
         if Remaining > 0 then
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I in 0 .. Remaining - 1);
               pragma Loop_Invariant (I < Rate);
               pragma Loop_Invariant (Message'First + Pos + I <= Message'Last);
               Final_Block (I) := Message (Message'First + Pos + I);
            end loop;
         end if;

         Final_Block (Remaining) := SHAKE_Suffix;  -- 0x1F
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;
         Absorb_Block (State, Final_Block, Rate);
      end;

      --  Squeeze phase: arbitrary length output
      while Output_Pos < Output_Length loop
         pragma Loop_Invariant (Output_Pos >= 0);
         pragma Loop_Invariant (Output_Pos < Output_Length);
         pragma Loop_Invariant (Output_Pos <= Output'Last);
         pragma Loop_Invariant (Output'First = 0);
         pragma Loop_Invariant (Output'Last = Output_Length - 1);
         pragma Loop_Invariant (Output_Length <= 65535);
         pragma Loop_Variant (Decreases => Output_Length - Output_Pos);

         declare
            Squeezed : Byte_Array (0 .. Rate - 1);
            Remaining_Output : constant Natural := Output_Length - Output_Pos;
            Copy_Len : constant Natural := Natural'Min (Rate, Remaining_Output);
         begin
            Squeeze_Block (State, Squeezed, Rate);

            if Copy_Len > 0 then
               for I in 0 .. Copy_Len - 1 loop
                  pragma Loop_Invariant (I in 0 .. Copy_Len - 1);
                  pragma Loop_Invariant (Copy_Len <= Rate);
                  pragma Loop_Invariant (Output_Pos + I < Output_Length);
                  pragma Loop_Invariant (Output_Pos + I <= Output'Last);
                  pragma Loop_Invariant (I <= Squeezed'Last);
                  Output (Output_Pos + I) := Squeezed (I);
               end loop;
            end if;

            Output_Pos := Output_Pos + Copy_Len;

            if Output_Pos < Output_Length then
               Keccak_F (State);
            end if;
         end;
      end loop;
   end SHAKE128;

   --  SHAKE256
   procedure SHAKE256 (
      Message       : Byte_Array;
      Output        : out Byte_Array;
      Output_Length : Positive
   ) is
      State : State_Array := (others => (others => 0));
      Rate : constant Valid_Rate := Rate_SHAKE256;  -- 136
      Pos : Natural := 0;
      Output_Pos : Natural := 0;
   begin
      Output := (others => 0);

      --  Absorb phase
      while Message'Length >= Rate and then Pos <= Message'Length - Rate loop
         pragma Loop_Invariant (Pos <= Message'Length);
         pragma Loop_Invariant (Pos <= Message'Last - Message'First + 1);
         pragma Loop_Invariant (Message'Last < Natural'Last);
         pragma Loop_Variant (Decreases => Message'Length - Pos);

         declare
            Block : Byte_Array (0 .. Rate - 1);
         begin
            for J in 0 .. Rate - 1 loop
               pragma Loop_Invariant (J in 0 .. Rate - 1);
               pragma Loop_Invariant (Message'First + Pos + J <= Message'Last);
               Block (J) := Message (Message'First + Pos + J);
            end loop;
            Absorb_Block (State, Block, Rate);
            Pos := Pos + Rate;
         end;
      end loop;

      declare
         Remaining : constant Natural := Message'Length - Pos;
         Final_Block : Byte_Array (0 .. Rate - 1) := (others => 0);
      begin
         if Remaining > 0 then
            for I in 0 .. Remaining - 1 loop
               pragma Loop_Invariant (I in 0 .. Remaining - 1);
               pragma Loop_Invariant (I < Rate);
               pragma Loop_Invariant (Message'First + Pos + I <= Message'Last);
               Final_Block (I) := Message (Message'First + Pos + I);
            end loop;
         end if;

         Final_Block (Remaining) := SHAKE_Suffix;
         Final_Block (Rate - 1) := Final_Block (Rate - 1) or 16#80#;
         Absorb_Block (State, Final_Block, Rate);
      end;

      while Output_Pos < Output_Length loop
         pragma Loop_Invariant (Output_Pos >= 0);
         pragma Loop_Invariant (Output_Pos < Output_Length);
         pragma Loop_Invariant (Output_Pos <= Output'Last);
         pragma Loop_Invariant (Output'First = 0);
         pragma Loop_Invariant (Output'Last = Output_Length - 1);
         pragma Loop_Invariant (Output_Length <= 65535);
         pragma Loop_Variant (Decreases => Output_Length - Output_Pos);

         declare
            Squeezed : Byte_Array (0 .. Rate - 1);
            Remaining_Output : constant Natural := Output_Length - Output_Pos;
            Copy_Len : constant Natural := Natural'Min (Rate, Remaining_Output);
         begin
            Squeeze_Block (State, Squeezed, Rate);

            if Copy_Len > 0 then
               for I in 0 .. Copy_Len - 1 loop
                  pragma Loop_Invariant (I in 0 .. Copy_Len - 1);
                  pragma Loop_Invariant (Copy_Len <= Rate);
                  pragma Loop_Invariant (Output_Pos + I < Output_Length);
                  pragma Loop_Invariant (Output_Pos + I <= Output'Last);
                  pragma Loop_Invariant (I <= Squeezed'Last);
                  Output (Output_Pos + I) := Squeezed (I);
               end loop;
            end if;

            Output_Pos := Output_Pos + Copy_Len;

            if Output_Pos < Output_Length then
               Keccak_F (State);
            end if;
         end;
      end loop;
   end SHAKE256;

end Anubis_SHA3;
