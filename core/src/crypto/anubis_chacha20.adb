-------------------------------------------------------------------------------
--  ANUBIS ChaCha20 Stream Cipher (Implementation)
--
--  Following SPARKNaCl platinum-level patterns for full proof discharge.
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_ChaCha20 with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants: "expand 32-byte k" in little-endian
   ---------------------------------------------------------------------------

   Sigma_0 : constant Unsigned_32 := 16#61707865#;  -- "expa"
   Sigma_1 : constant Unsigned_32 := 16#3320646e#;  -- "nd 3"
   Sigma_2 : constant Unsigned_32 := 16#79622d32#;  -- "2-by"
   Sigma_3 : constant Unsigned_32 := 16#6b206574#;  -- "te k"

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Rotate left 32-bit (following SPARKNaCl naming)
   function RL32 (X : Unsigned_32; N : Natural) return Unsigned_32 is
     (Rotate_Left (X, N))
   with Inline, Global => null;

   --  Load 32-bit word from byte array (little-endian)
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

   --  Store 32-bit word to byte array (little-endian)
   procedure Store_LE32 (
      Data   : in Out Byte_Array;
      Offset : Natural;
      Value  : Unsigned_32
   ) with
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
   --  Quarter Round (RFC 8439 Section 2.1)
   --  Uses modular U32 arithmetic - overflow wraps naturally
   ---------------------------------------------------------------------------

   procedure Quarter_Round (
      A, B, C, D : in Out Unsigned_32
   ) is
   begin
      A := A + B;
      D := RL32 (D xor A, 16);
      C := C + D;
      B := RL32 (B xor C, 12);
      A := A + B;
      D := RL32 (D xor A, 8);
      C := C + D;
      B := RL32 (B xor C, 7);
   end Quarter_Round;

   ---------------------------------------------------------------------------
   --  Double Round: Column + Diagonal (10 iterations = 20 rounds total)
   ---------------------------------------------------------------------------

   procedure Double_Round (S : in Out State_Matrix) with
      Global => null
   is
   begin
      --  Column round
      Quarter_Round (S (0), S (4), S (8),  S (12));
      Quarter_Round (S (1), S (5), S (9),  S (13));
      Quarter_Round (S (2), S (6), S (10), S (14));
      Quarter_Round (S (3), S (7), S (11), S (15));

      --  Diagonal round
      Quarter_Round (S (0), S (5), S (10), S (15));
      Quarter_Round (S (1), S (6), S (11), S (12));
      Quarter_Round (S (2), S (7), S (8),  S (13));
      Quarter_Round (S (3), S (4), S (9),  S (14));
   end Double_Round;

   ---------------------------------------------------------------------------
   --  Block Function (RFC 8439 Section 2.3)
   ---------------------------------------------------------------------------

   procedure Block_Function (
      Key     : in     ChaCha20_Key;
      Counter : in     Unsigned_32;
      Nonce   : in     ChaCha20_Nonce;
      Output  :    out ChaCha20_Block
   ) is
      Initial : State_Matrix;
      Working : State_Matrix;
      Key_Arr : constant Byte_Array (0 .. 31) := Byte_Array (Key);
      Nonce_Arr : constant Byte_Array (0 .. 11) := Byte_Array (Nonce);
   begin
      --  Initialize state matrix
      --  Words 0-3: constants
      Initial (0) := Sigma_0;
      Initial (1) := Sigma_1;
      Initial (2) := Sigma_2;
      Initial (3) := Sigma_3;

      --  Words 4-11: key (8 words, 32 bytes)
      for I in 0 .. 7 loop
         pragma Loop_Invariant (True);
         Initial (4 + I) := Load_LE32 (Key_Arr, I * 4);
      end loop;

      --  Word 12: counter
      Initial (12) := Counter;

      --  Words 13-15: nonce (3 words, 12 bytes)
      for I in 0 .. 2 loop
         pragma Loop_Invariant (True);
         Initial (13 + I) := Load_LE32 (Nonce_Arr, I * 4);
      end loop;

      --  Copy to working state
      Working := Initial;

      --  Apply 10 double-rounds (= 20 rounds)
      for R in 1 .. 10 loop
         pragma Loop_Invariant (True);
         Double_Round (Working);
      end loop;

      --  Add initial state to working state (modular addition)
      for I in State_Index loop
         pragma Loop_Invariant (True);
         Working (I) := Working (I) + Initial (I);
      end loop;

      --  Serialize to output (little-endian)
      Output := (others => 0);
      declare
         Out_Arr : Byte_Array (0 .. 63) := Byte_Array (Output);
      begin
         for I in State_Index loop
            pragma Loop_Invariant (True);
            Store_LE32 (Out_Arr, I * 4, Working (I));
         end loop;
         Output := ChaCha20_Block (Out_Arr);
      end;
   end Block_Function;

   ---------------------------------------------------------------------------
   --  ChaCha20: Generate keystream only
   ---------------------------------------------------------------------------

   procedure ChaCha20 (
      Output  :    out Byte_Array;
      Nonce   : in     ChaCha20_Nonce;
      Key     : in     ChaCha20_Key;
      Counter : in     Unsigned_32
   ) is
      Block_Out : ChaCha20_Block;
      Curr_Ctr  : Unsigned_32 := Counter;
      Offset    : Natural := 0;
      Remaining : Natural;
   begin
      Output := (others => 0);

      if Output'Length = 0 then
         return;
      end if;

      --  Process full 64-byte blocks
      while Offset + Block_Size <= Output'Length loop
         pragma Loop_Invariant (Offset mod Block_Size = 0);
         pragma Loop_Invariant (Offset < Output'Length);
         pragma Loop_Invariant (Curr_Ctr = Counter + Unsigned_32 (Offset / Block_Size));

         Block_Function (Key, Curr_Ctr, Nonce, Block_Out);

         for I in Block_Index loop
            pragma Loop_Invariant (True);
            Output (Offset + I) := Block_Out (I);
         end loop;

         Offset := Offset + Block_Size;
         Curr_Ctr := Curr_Ctr + 1;
      end loop;

      --  Process remaining bytes (partial block)
      Remaining := Output'Length - Offset;
      if Remaining > 0 then
         Block_Function (Key, Curr_Ctr, Nonce, Block_Out);

         for I in 0 .. Remaining - 1 loop
            pragma Loop_Invariant (I < Remaining);
            pragma Loop_Invariant (Offset + I < Output'Length);
            Output (Offset + I) := Block_Out (I);
         end loop;
      end if;
   end ChaCha20;

   ---------------------------------------------------------------------------
   --  ChaCha20_XOR: Encrypt/decrypt via XOR
   ---------------------------------------------------------------------------

   procedure ChaCha20_XOR (
      Output  :    out Byte_Array;
      Input   : in     Byte_Array;
      Nonce   : in     ChaCha20_Nonce;
      Key     : in     ChaCha20_Key;
      Counter : in     Unsigned_32
   ) is
      Block_Out : ChaCha20_Block;
      Curr_Ctr  : Unsigned_32 := Counter;
      Offset    : Natural := 0;
      Remaining : Natural;
   begin
      Output := (others => 0);

      if Input'Length = 0 then
         return;
      end if;

      --  Process full 64-byte blocks
      while Offset + Block_Size <= Input'Length loop
         pragma Loop_Invariant (Offset mod Block_Size = 0);
         pragma Loop_Invariant (Offset < Input'Length);
         pragma Loop_Invariant (Curr_Ctr = Counter + Unsigned_32 (Offset / Block_Size));

         Block_Function (Key, Curr_Ctr, Nonce, Block_Out);

         for I in Block_Index loop
            pragma Loop_Invariant (True);
            Output (Offset + I) := Input (Offset + I) xor Block_Out (I);
         end loop;

         Offset := Offset + Block_Size;
         Curr_Ctr := Curr_Ctr + 1;
      end loop;

      --  Process remaining bytes (partial block)
      Remaining := Input'Length - Offset;
      if Remaining > 0 then
         Block_Function (Key, Curr_Ctr, Nonce, Block_Out);

         for I in 0 .. Remaining - 1 loop
            pragma Loop_Invariant (I < Remaining);
            pragma Loop_Invariant (Offset + I < Input'Length);
            Output (Offset + I) := Input (Offset + I) xor Block_Out (I);
         end loop;
      end if;
   end ChaCha20_XOR;

   ---------------------------------------------------------------------------
   --  Sanitization
   ---------------------------------------------------------------------------

   procedure Sanitize (Key : out ChaCha20_Key) is
   begin
      Key := (others => 0);
   end Sanitize;

   procedure Sanitize_Block (Block : out ChaCha20_Block) is
   begin
      Block := (others => 0);
   end Sanitize_Block;

end Anubis_ChaCha20;
