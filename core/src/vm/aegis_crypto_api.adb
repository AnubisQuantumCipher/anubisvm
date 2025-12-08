pragma SPARK_Mode (On);

with Aegis_Gas; use Aegis_Gas;
with Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;

package body Aegis_Crypto_API with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal Helpers: Type Conversion
   ---------------------------------------------------------------------------

   --  Convert Hash_Input_Buffer slice to Byte_Array for SHA3 call
   function To_Input_Slice (
      Input  : Hash_Input_Buffer;
      Length : Natural
   ) return Anubis_Types.Byte_Array
   with
      Pre  => Length <= Max_Hash_Input and Length > 0,
      Post => To_Input_Slice'Result'First = 0
              and To_Input_Slice'Result'Last = Length - 1
              and To_Input_Slice'Result'Length = Length
   is
      Result : Anubis_Types.Byte_Array (0 .. Length - 1);
   begin
      for I in 0 .. Length - 1 loop
         pragma Loop_Invariant (Result'First = 0);
         pragma Loop_Invariant (Result'Last = Length - 1);
         Result (I) := Anubis_Types.Byte (Input (I));
      end loop;
      return Result;
   end To_Input_Slice;

   ---------------------------------------------------------------------------
   --  SHA3 Hashing
   ---------------------------------------------------------------------------

   procedure SHA3_256_Hash (
      Ctx      : in out Execution_Context;
      Input    : in     Hash_Input_Buffer;
      Length   : in     Natural;
      Output   : out    Aegis_VM_Types.Hash256;
      Result   : out    Crypto_Result
   ) is
      Success : Boolean;
      Gas_Cost : constant Gas_Amount := Gas_Hash (Length);
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Charge gas
      Use_Gas (Ctx, Gas_Cost, Success);
      if not Success then
         Output := Hash256_Zero;
         Result := Crypto_Out_Of_Gas;
         return;
      end if;

      --  Call real SPARK-verified SHA3-256
      if Length = 0 then
         declare
            Empty_Msg : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
         begin
            Anubis_SHA3.SHA3_256 (Empty_Msg, Digest);
         end;
      else
         declare
            Msg : constant Anubis_Types.Byte_Array := To_Input_Slice (Input, Length);
         begin
            --  Prove precondition: Length <= Max_Hash_Input = 32768, so Msg'Last <= 32767 < Natural'Last
            pragma Assert (Msg'Last = Length - 1);
            pragma Assert (Length <= Max_Hash_Input);
            pragma Assert (Msg'Last < Natural'Last);
            Anubis_SHA3.SHA3_256 (Msg, Digest);
         end;
      end if;

      --  Copy digest to output
      for I in 0 .. 31 loop
         Output (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;
      Result := Crypto_OK;
   end SHA3_256_Hash;

   procedure SHA3_512_Hash (
      Ctx      : in Out Execution_Context;
      Input    : in     Hash_Input_Buffer;
      Length   : in     Natural;
      Output   : out    Aegis_VM_Types.Hash512;
      Result   : out    Crypto_Result
   ) is
      Success : Boolean;
      Gas_Cost : constant Gas_Amount := Gas_Hash (Length);
      Digest : Anubis_SHA3.SHA3_512_Digest;
   begin
      Use_Gas (Ctx, Gas_Cost, Success);
      if not Success then
         Output := Hash512_Zero;
         Result := Crypto_Out_Of_Gas;
         return;
      end if;

      --  Call real SPARK-verified SHA3-512
      if Length = 0 then
         declare
            Empty_Msg : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
         begin
            Anubis_SHA3.SHA3_512 (Empty_Msg, Digest);
         end;
      else
         declare
            Msg : constant Anubis_Types.Byte_Array := To_Input_Slice (Input, Length);
         begin
            pragma Assert (Msg'Last = Length - 1);
            pragma Assert (Length <= Max_Hash_Input);
            pragma Assert (Msg'Last < Natural'Last);
            Anubis_SHA3.SHA3_512 (Msg, Digest);
         end;
      end if;

      --  Copy digest to output
      for I in 0 .. 63 loop
         Output (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;
      Result := Crypto_OK;
   end SHA3_512_Hash;

   procedure Keccak256_Hash (
      Ctx      : in Out Execution_Context;
      Input    : in     Hash_Input_Buffer;
      Length   : in     Natural;
      Output   : out    Aegis_VM_Types.Hash256;
      Result   : out    Crypto_Result
   ) is
      Success : Boolean;
      Gas_Cost : constant Gas_Amount := Gas_Hash (Length);
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      Use_Gas (Ctx, Gas_Cost, Success);
      if not Success then
         Output := Hash256_Zero;
         Result := Crypto_Out_Of_Gas;
         return;
      end if;

      --  Use proper Keccak-256 (Ethereum-compatible)
      --  Keccak-256 uses domain separator 0x01 instead of SHA3"s 0x06
      if Length = 0 then
         declare
            Empty_Msg : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
         begin
            Anubis_SHA3.Keccak_256 (Empty_Msg, Digest);
         end;
      else
         declare
            Msg : constant Anubis_Types.Byte_Array := To_Input_Slice (Input, Length);
         begin
            pragma Assert (Msg'Last = Length - 1);
            pragma Assert (Length <= Max_Hash_Input);
            pragma Assert (Msg'Last < Natural'Last);
            Anubis_SHA3.Keccak_256 (Msg, Digest);
         end;
      end if;

      for I in 0 .. 31 loop
         Output (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;
      Result := Crypto_OK;
   end Keccak256_Hash;

   ---------------------------------------------------------------------------
   --  SHAKE XOF
   ---------------------------------------------------------------------------

   procedure SHAKE_XOF (
      Ctx           : in Out Execution_Context;
      Variant       : in     SHAKE_Variant;
      Input         : in     Hash_Input_Buffer;
      Input_Length  : in     Natural;
      Output        : out    SHAKE_Output_Buffer;
      Output_Length : in     Natural;
      Result        : out    Crypto_Result
   ) is
      Success : Boolean;
      --  Compute gas cost with overflow protection
      --  Output_Length is bounded by Max_SHAKE_Output (1024)
      --  So (Output_Length + 31) / 32 <= 33, and 33 * 6 = 198
      Output_Blocks : constant Natural := (Output_Length + 31) / 32;
      Output_Gas : constant Gas_Amount := Gas_Amount (Output_Blocks) * 6;
      Base_Gas : constant Gas_Amount := Gas_Hash (Input_Length);
      --  Output_Gas <= 198, Base_Gas bounded by Gas_Hash postcondition (<= Max_Safe_Base_Gas)
      Gas_Cost : Gas_Amount;
   begin
      --  Prove overflow safety: Base_Gas is bounded by Gas_Hash"s postcondition
      --  Base_Gas <= Max_Safe_Base_Gas (from Gas_Hash postcondition)
      --  Output_Gas <= 198
      pragma Assert (Output_Gas <= 198);
      pragma Assert (Base_Gas <= Aegis_Gas.Max_Safe_Base_Gas);

      --  Compute total cost, capping at Max_Safe_Base_Gas for Use_Gas precondition
      --  Note: In practice, Base_Gas from Gas_Hash is << Max_Safe_Base_Gas (at most ~400M),
      --  so this cap will never actually trigger, but it satisfies the prover.
      if Base_Gas <= Aegis_Gas.Max_Safe_Base_Gas - Output_Gas then
         Gas_Cost := Base_Gas + Output_Gas;
      else
         --  Cap at Max_Safe_Base_Gas (defensive, should never happen in practice)
         Gas_Cost := Aegis_Gas.Max_Safe_Base_Gas;
      end if;
      pragma Assert (Gas_Cost <= Aegis_Gas.Max_Safe_Base_Gas);
      Use_Gas (Ctx, Gas_Cost, Success);
      if not Success then
         Output := (others => 0);
         Result := Crypto_Out_Of_Gas;
         return;
      end if;

      --  Initialize output to zero
      Output := (others => 0);

      if Output_Length = 0 then
         Result := Crypto_OK;
         return;
      end if;

      --  Call real SPARK-verified SHAKE
      declare
         Shake_Out : Anubis_Types.Byte_Array (0 .. Output_Length - 1);
      begin
         if Input_Length = 0 then
            declare
               Empty_Msg : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
            begin
               case Variant is
                  when SHAKE_128 =>
                     Anubis_SHA3.SHAKE128 (Empty_Msg, Shake_Out, Output_Length);
                  when SHAKE_256 =>
                     Anubis_SHA3.SHAKE256 (Empty_Msg, Shake_Out, Output_Length);
               end case;
            end;
         else
            declare
               Msg : constant Anubis_Types.Byte_Array := To_Input_Slice (Input, Input_Length);
            begin
               --  Prove SHAKE preconditions
               pragma Assert (Msg'Last = Input_Length - 1);
               pragma Assert (Input_Length <= Max_Hash_Input);
               pragma Assert (Msg'Last < Natural'Last);
               pragma Assert (Shake_Out'Last = Output_Length - 1);
               pragma Assert (Output_Length <= Max_SHAKE_Output);
               pragma Assert (Shake_Out'Last < Natural'Last);
               pragma Assert (Shake_Out'Length = Output_Length);
               case Variant is
                  when SHAKE_128 =>
                     Anubis_SHA3.SHAKE128 (Msg, Shake_Out, Output_Length);
                  when SHAKE_256 =>
                     Anubis_SHA3.SHAKE256 (Msg, Shake_Out, Output_Length);
               end case;
            end;
         end if;

         --  Copy result to output buffer
         for I in 0 .. Output_Length - 1 loop
            Output (I) := Aegis_VM_Types.Byte (Shake_Out (I));
         end loop;
      end;

      Result := Crypto_OK;
   end SHAKE_XOF;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Signature Verification
   ---------------------------------------------------------------------------

   procedure MLDSA87_Verify (
      Ctx       : in Out Execution_Context;
      Message   : in     Hash_Input_Buffer;
      Msg_Len   : in     Natural;
      Signature : in     MLDSA87_Signature;
      Public_Key : in    MLDSA87_Public_Key;
      Valid     : out    Boolean;
      Result    : out    Crypto_Result
   ) is
      Success : Boolean;
   begin
      --  Charge gas for signature verification
      Use_Gas (Ctx, Gas_MLDSA_Verify, Success);
      if not Success then
         Valid := False;
         Result := Crypto_Out_Of_Gas;
         return;
      end if;

      --  Convert types and call real SPARK-verified ML-DSA-87
      declare
         --  Initialize to zero to prove full initialization before use
         PK : Anubis_MLDSA_Types.Public_Key := (others => 0);
         Sig : Anubis_MLDSA_Types.Signature := (others => 0);
      begin
         --  Copy public key
         for I in 0 .. MLDSA87_Public_Key_Size - 1 loop
            pragma Loop_Invariant (I <= MLDSA87_Public_Key_Size - 1);
            PK (I) := Anubis_Types.Byte (Public_Key (I));
         end loop;

         --  Copy signature (use Sig'Range to prove index validity)
         pragma Assert (Sig'First = 0);
         pragma Assert (Sig'Last = MLDSA87_Signature_Size - 1);
         for I in Sig'Range loop
            pragma Loop_Invariant (I >= Sig'First and I <= Sig'Last);
            Sig (I) := Anubis_Types.Byte (Signature (I));
         end loop;

         --  Call real verification
         if Msg_Len = 0 then
            declare
               Empty_Msg : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
            begin
               Valid := Anubis_MLDSA.Verify (PK, Empty_Msg, Sig);
            end;
         else
            declare
               Msg : constant Anubis_Types.Byte_Array := To_Input_Slice (Message, Msg_Len);
            begin
               Valid := Anubis_MLDSA.Verify (PK, Msg, Sig);
            end;
         end if;
      end;

      Result := Crypto_OK;
   end MLDSA87_Verify;

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Key Decapsulation
   ---------------------------------------------------------------------------

   procedure MLKEM1024_Decaps (
      Ctx          : in Out Execution_Context;
      Ciphertext   : in     MLKEM1024_Ciphertext;
      Decaps_Key   : in     MLKEM1024_Decaps_Key;
      Shared_Secret : out   MLKEM1024_Shared_Secret;
      Result       : out    Crypto_Result
   ) is
      Success : Boolean;
   begin
      --  Charge gas for decapsulation
      Use_Gas (Ctx, Gas_MLKEM_Decaps, Success);
      if not Success then
         Shared_Secret := (others => 0);
         Result := Crypto_Out_Of_Gas;
         return;
      end if;

      --  Convert types and call real SPARK-verified ML-KEM-1024
      declare
         DK : Anubis_MLKEM_Types.Decapsulation_Key;
         CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
         SS : Anubis_MLKEM_Types.Shared_Secret;
      begin
         --  Copy decapsulation key
         for I in 0 .. MLKEM1024_Decaps_Key_Size - 1 loop
            DK (I) := Anubis_Types.Byte (Decaps_Key (I));
         end loop;

         --  Copy ciphertext
         for I in 0 .. MLKEM1024_Ciphertext_Size - 1 loop
            CT (I) := Anubis_Types.Byte (Ciphertext (I));
         end loop;

         --  Call real decapsulation
         Anubis_MLKEM.Decaps (DK, CT, SS);

         --  Copy shared secret to output
         for I in 0 .. MLKEM1024_Secret_Size - 1 loop
            Shared_Secret (I) := Aegis_VM_Types.Byte (SS (I));
         end loop;
      end;

      Result := Crypto_OK;
   end MLKEM1024_Decaps;

   ---------------------------------------------------------------------------
   --  Address Derivation
   ---------------------------------------------------------------------------

   procedure Derive_Address (
      Ctx        : in Out Execution_Context;
      Public_Key : in     MLDSA87_Public_Key;
      Entity     : in     Aegis_VM_Types.Byte;
      Address    : out    Contract_Address;
      Result     : out    Crypto_Result
   ) is
      Success : Boolean;
      Gas_Cost : constant Gas_Amount := Gas_Hash (MLDSA87_Public_Key_Size);

      --  Domain separator: "aegis-v1-mldsa87-" = 17 bytes + 1 byte entity
      Domain_Prefix : constant Anubis_Types.Byte_Array (0 .. 16) :=
        (16#61#, 16#65#, 16#67#, 16#69#, 16#73#, 16#2D#, 16#76#, 16#31#,
         16#2D#, 16#6D#, 16#6C#, 16#64#, 16#73#, 16#61#, 16#38#, 16#37#,
         16#2D#);

      --  Full preimage: domain_separator (18 bytes) + public_key (2592 bytes)
      Preimage_Len : constant := 18 + MLDSA87_Public_Key_Size;
      --  Initialize to zero to prove full initialization for SPARK
      Preimage : Anubis_Types.Byte_Array (0 .. Preimage_Len - 1) := (others => 0);
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      Use_Gas (Ctx, Gas_Cost, Success);
      if not Success then
         Address := Address_Zero;
         Result := Crypto_Out_Of_Gas;
         return;
      end if;

      --  Build preimage: domain_separator || public_key
      for I in 0 .. 16 loop
         Preimage (I) := Domain_Prefix (I);
      end loop;
      Preimage (17) := Anubis_Types.Byte (Entity);
      for I in 0 .. MLDSA87_Public_Key_Size - 1 loop
         Preimage (18 + I) := Anubis_Types.Byte (Public_Key (I));
      end loop;

      --  Hash to derive address
      Anubis_SHA3.SHA3_256 (Preimage, Digest);

      --  Copy digest to address
      for I in 0 .. 31 loop
         Address (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;

      Result := Crypto_OK;
   end Derive_Address;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Constant_Time_Equal (
      A, B   : Aegis_VM_Types.Hash256
   ) return Boolean is
      Diff : Aegis_VM_Types.Byte := 0;
   begin
      for I in 0 .. 31 loop
         Diff := Diff or (A (I) xor B (I));
      end loop;
      return Diff = 0;
   end Constant_Time_Equal;

   procedure Zeroize_Secret (
      Secret : in Out MLKEM1024_Shared_Secret
   ) is
   begin
      for I in MLKEM1024_Secret_Index loop
         Secret (I) := 0;
         pragma Loop_Invariant (for all J in 0 .. I => Secret (J) = 0);
      end loop;
   end Zeroize_Secret;

end Aegis_Crypto_API;
