pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;
with Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;

package body Khepri_Crypto is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Constant_Time_Compare (
      A, B   : Byte_Array;
      Length : Natural
   ) return Boolean is
      Diff : Byte := 0;
   begin
      for I in 0 .. Length - 1 loop
         Diff := Diff or (A (A'First + I) xor B (B'First + I));
      end loop;
      return Diff = 0;
   end Constant_Time_Compare;

   --  Convert SDK Byte_Array to Anubis Byte_Array
   function To_Anubis_Bytes (Data : Byte_Array) return Anubis_Types.Byte_Array is
      Result : Anubis_Types.Byte_Array (0 .. Data'Length - 1);
   begin
      for I in 0 .. Data'Length - 1 loop
         Result (I) := Anubis_Types.Byte (Data (Data'First + I));
      end loop;
      return Result;
   end To_Anubis_Bytes;

   ---------------------------------------------------------------------------
   --  Hash Functions - Now wired to real SPARK crypto
   ---------------------------------------------------------------------------

   function SHA3_256 (Data : Byte_Array) return Hash_256 is
      Result : Hash_256 := (others => 0);
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      if Data'Length = 0 then
         declare
            Empty : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
         begin
            Anubis_SHA3.SHA3_256 (Empty, Digest);
         end;
      else
         declare
            Input : constant Anubis_Types.Byte_Array := To_Anubis_Bytes (Data);
         begin
            Anubis_SHA3.SHA3_256 (Input, Digest);
         end;
      end if;

      --  Copy result
      for I in 0 .. 31 loop
         Result (I) := Byte (Digest (I));
      end loop;
      return Result;
   end SHA3_256;

   function SHA3_256_Bytes32 (Data : Bytes32) return Hash_256 is
      Temp : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Temp (I) := Data (I);
      end loop;
      return SHA3_256 (Temp);
   end SHA3_256_Bytes32;

   function SHA3_256_U256 (Value : Uint256) return Hash_256 is
      Data : constant Bytes32 := To_Bytes_BE (Value);
   begin
      return SHA3_256_Bytes32 (Data);
   end SHA3_256_U256;

   function Keccak_256 (Data : Byte_Array) return Hash_256 is
      Result : Hash_256 := (others => 0);
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Use proper Keccak-256 (Ethereum-compatible)
      --  Keccak-256 uses domain separator 0x01 instead of SHA3"s 0x06
      if Data'Length = 0 then
         declare
            Empty : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
         begin
            Anubis_SHA3.Keccak_256 (Empty, Digest);
         end;
      else
         declare
            Input : constant Anubis_Types.Byte_Array := To_Anubis_Bytes (Data);
         begin
            Anubis_SHA3.Keccak_256 (Input, Digest);
         end;
      end if;

      for I in 0 .. 31 loop
         Result (I) := Byte (Digest (I));
      end loop;
      return Result;
   end Keccak_256;

   function SHA3_512 (Data : Byte_Array) return Hash_512 is
      Result : Hash_512 := (others => 0);
      Digest : Anubis_SHA3.SHA3_512_Digest;
   begin
      if Data'Length = 0 then
         declare
            Empty : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
         begin
            Anubis_SHA3.SHA3_512 (Empty, Digest);
         end;
      else
         declare
            Input : constant Anubis_Types.Byte_Array := To_Anubis_Bytes (Data);
         begin
            Anubis_SHA3.SHA3_512 (Input, Digest);
         end;
      end if;

      for I in 0 .. 63 loop
         Result (I) := Byte (Digest (I));
      end loop;
      return Result;
   end SHA3_512;

   ---------------------------------------------------------------------------
   --  Address Derivation
   ---------------------------------------------------------------------------

   function Derive_Address (PK : MLDSA_Public_Key) return Address is
      Temp : Byte_Array (0 .. MLDSA_Public_Key_Size - 1);
      Hash : Hash_256;
      Addr : Address;
   begin
      for I in PK'Range loop
         Temp (I) := PK (I);
      end loop;
      Hash := SHA3_256 (Temp);
      for I in 0 .. 31 loop
         Addr (I) := Hash (I);
      end loop;
      return Addr;
   end Derive_Address;

   function Is_Valid_Address (Addr : Address) return Boolean is
   begin
      for I in Addr'Range loop
         if Addr (I) /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Is_Valid_Address;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Operations - Now wired to real SPARK crypto
   ---------------------------------------------------------------------------

   function MLDSA_Verify (
      Message    : Byte_Array;
      Signature  : MLDSA_Signature;
      Public_Key : MLDSA_Public_Key
   ) return Crypto_Result is
      --  Convert to Anubis types
      PK  : Anubis_MLDSA_Types.Public_Key;
      Sig : Anubis_MLDSA_Types.Signature;
      Valid : Boolean;
   begin
      --  Copy public key
      for I in 0 .. MLDSA_Public_Key_Size - 1 loop
         PK (I) := Anubis_Types.Byte (Public_Key (I));
      end loop;

      --  Copy signature
      for I in 0 .. MLDSA_Signature_Size - 1 loop
         Sig (I) := Anubis_Types.Byte (Signature (I));
      end loop;

      --  Call real verification
      if Message'Length = 0 then
         declare
            Empty : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
         begin
            Valid := Anubis_MLDSA.Verify (PK, Empty, Sig);
         end;
      else
         declare
            Msg : constant Anubis_Types.Byte_Array := To_Anubis_Bytes (Message);
         begin
            Valid := Anubis_MLDSA.Verify (PK, Msg, Sig);
         end;
      end if;

      if Valid then
         return Crypto_Success;
      else
         return Crypto_Invalid_Signature;
      end if;
   end MLDSA_Verify;

   function MLDSA_Verify_Hash (
      Message_Hash : Hash_256;
      Signature    : MLDSA_Signature;
      Public_Key   : MLDSA_Public_Key
   ) return Crypto_Result is
      Msg : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Msg (I) := Message_Hash (I);
      end loop;
      return MLDSA_Verify (Msg, Signature, Public_Key);
   end MLDSA_Verify_Hash;

   function Verify_And_Get_Signer (
      Message    : Byte_Array;
      Signature  : MLDSA_Signature;
      Public_Key : MLDSA_Public_Key
   ) return Address is
      Result : Crypto_Result;
   begin
      Result := MLDSA_Verify (Message, Signature, Public_Key);
      if Result = Crypto_Success then
         return Derive_Address (Public_Key);
      else
         return Null_Address;
      end if;
   end Verify_And_Get_Signer;

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Operations - Now wired to real SPARK crypto
   ---------------------------------------------------------------------------

   --  Simple entropy counter for deterministic testing
   --  Real implementation would use hardware RNG or DRBG
   Entropy_Counter : Interfaces.Unsigned_64 := 16#DEADBEEF_CAFEBABE#;

   procedure Get_Random_Seed (Seed : out Anubis_MLKEM_Types.Seed) is
      Input  : Anubis_Types.Byte_Array (0 .. 7);
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Mix counter into seed material
      Entropy_Counter := Entropy_Counter + 1;
      for I in 0 .. 7 loop
         Input (I) := Anubis_Types.Byte (
            Interfaces.Shift_Right (Entropy_Counter, I * 8) and 16#FF#);
      end loop;

      --  Hash to produce seed
      Anubis_SHA3.SHA3_256 (Input, Digest);
      for I in 0 .. 31 loop
         Seed (I) := Digest (I);
      end loop;
   end Get_Random_Seed;

   procedure MLKEM_Encaps (
      Public_Key  : in  MLKEM_Public_Key;
      Ciphertext  : out MLKEM_Ciphertext;
      Shared_Key  : out MLKEM_Shared_Key;
      Result      : out Crypto_Result
   ) is
      --  Convert to Anubis types
      EK : Anubis_MLKEM_Types.Encapsulation_Key;
      CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS : Anubis_MLKEM_Types.Shared_Secret;
      Random_M : Anubis_MLKEM_Types.Seed;
   begin
      --  Generate random seed for encapsulation
      Get_Random_Seed (Random_M);
      --  Copy encapsulation key
      for I in 0 .. MLKEM_Public_Key_Size - 1 loop
         EK (I) := Anubis_Types.Byte (Public_Key (I));
      end loop;

      --  Call real encapsulation
      Anubis_MLKEM.Encaps (EK, Random_M, SS, CT);

      --  Copy outputs
      for I in 0 .. MLKEM_Ciphertext_Size - 1 loop
         Ciphertext (I) := Byte (CT (I));
      end loop;
      for I in 0 .. MLKEM_Shared_Key_Size - 1 loop
         Shared_Key (I) := Byte (SS (I));
      end loop;

      Result := Crypto_Success;
   end MLKEM_Encaps;

   procedure MLKEM_Decaps (
      Secret_Key  : in  MLKEM_Secret_Key;
      Ciphertext  : in  MLKEM_Ciphertext;
      Shared_Key  : out MLKEM_Shared_Key;
      Result      : out Crypto_Result
   ) is
      --  Convert to Anubis types
      DK : Anubis_MLKEM_Types.Decapsulation_Key;
      CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS : Anubis_MLKEM_Types.Shared_Secret;
   begin
      --  Copy decapsulation key
      for I in 0 .. MLKEM_Secret_Key_Size - 1 loop
         DK (I) := Anubis_Types.Byte (Secret_Key (I));
      end loop;

      --  Copy ciphertext
      for I in 0 .. MLKEM_Ciphertext_Size - 1 loop
         CT (I) := Anubis_Types.Byte (Ciphertext (I));
      end loop;

      --  Call real decapsulation
      Anubis_MLKEM.Decaps (DK, CT, SS);

      --  Copy shared secret
      for I in 0 .. MLKEM_Shared_Key_Size - 1 loop
         Shared_Key (I) := Byte (SS (I));
      end loop;

      Result := Crypto_Success;
   end MLKEM_Decaps;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Hash_Equal (A, B : Hash_256) return Boolean is
      Temp_A : Byte_Array (0 .. 31);
      Temp_B : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Temp_A (I) := A (I);
         Temp_B (I) := B (I);
      end loop;
      return Constant_Time_Compare (Temp_A, Temp_B, 32);
   end Hash_Equal;

   function Address_Equal (A, B : Address) return Boolean is
      Temp_A : Byte_Array (0 .. 31);
      Temp_B : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Temp_A (I) := A (I);
         Temp_B (I) := B (I);
      end loop;
      return Constant_Time_Compare (Temp_A, Temp_B, 32);
   end Address_Equal;

   function Calculate_Signature (
      Declaration : String
   ) return Hash_256 is
      Data : Byte_Array (0 .. Declaration'Length - 1);
   begin
      for I in Declaration'Range loop
         Data (I - Declaration'First) := Byte (Character'Pos (Declaration (I)));
      end loop;
      return Keccak_256 (Data);
   end Calculate_Signature;

   procedure Zeroize_Hash (H : out Hash_256) is
   begin
      for I in H'Range loop
         H (I) := 0;
      end loop;
   end Zeroize_Hash;

   procedure Zeroize_Shared_Key (K : out MLKEM_Shared_Key) is
   begin
      for I in K'Range loop
         K (I) := 0;
      end loop;
   end Zeroize_Shared_Key;

   ---------------------------------------------------------------------------
   --  Message Construction Helpers
   ---------------------------------------------------------------------------

   function Address_To_Bytes (Addr : Address) return Bytes32 is
      Result : Bytes32;
   begin
      for I in 0 .. 31 loop
         Result (I) := Addr (I);
      end loop;
      return Result;
   end Address_To_Bytes;

   function U256_To_Bytes_BE (Value : Uint256) return Bytes32 is
   begin
      return To_Bytes_BE (Value);
   end U256_To_Bytes_BE;

   --  EIP-712 Domain Separator type hash
   --  keccak256("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)")
   EIP712_Domain_Type_Hash : constant Hash_256 := (
      16#8B#, 16#73#, 16#C3#, 16#C6#, 16#9B#, 16#B8#, 16#FE#, 16#3D#,
      16#51#, 16#2E#, 16#CC#, 16#40#, 16#37#, 16#05#, 16#59#, 16#E6#,
      16#3B#, 16#AB#, 16#E7#, 16#94#, 16#D6#, 16#11#, 16#BE#, 16#5F#,
      16#17#, 16#9C#, 16#6A#, 16#A3#, 16#69#, 16#3E#, 16#D4#, 16#50#
   );

   function Encode_Domain (
      Name        : Bounded_String;
      Version     : Bounded_String;
      Chain_ID    : Uint256;
      Contract    : Address
   ) return Hash_256 is
      Buffer : Byte_Array (0 .. 255) := (others => 0);
      Idx    : Natural := 0;
      Hash_Result : Hash_256;
   begin
      --  EIP-712 Domain type hash prefix
      for I in 0 .. 31 loop
         Buffer (Idx) := EIP712_Domain_Type_Hash (I);
         Idx := Idx + 1;
      end loop;

      --  Name hash
      for I in 0 .. Name.Length - 1 loop
         if Idx < 256 then
            Buffer (Idx) := Byte (Character'Pos (Name.Data (I)));
            Idx := Idx + 1;
         end if;
      end loop;

      --  Version hash
      for I in 0 .. Version.Length - 1 loop
         if Idx < 256 then
            Buffer (Idx) := Byte (Character'Pos (Version.Data (I)));
            Idx := Idx + 1;
         end if;
      end loop;

      --  Chain ID
      declare
         Chain_Bytes : constant Bytes32 := To_Bytes_BE (Chain_ID);
      begin
         for I in 0 .. 31 loop
            if Idx < 256 then
               Buffer (Idx) := Chain_Bytes (I);
               Idx := Idx + 1;
            end if;
         end loop;
      end;

      --  Contract address
      for I in 0 .. 31 loop
         if Idx < 256 then
            Buffer (Idx) := Contract (I);
            Idx := Idx + 1;
         end if;
      end loop;

      Hash_Result := SHA3_256 (Buffer (0 .. Idx - 1));
      return Hash_Result;
   end Encode_Domain;

   function Encode_Struct (
      Type_Hash : Hash_256;
      Fields    : Byte_Array
   ) return Hash_256 is
      Buffer : Byte_Array (0 .. 1055) := (others => 0);
      Idx    : Natural := 0;
   begin
      --  Type hash first
      for I in 0 .. 31 loop
         Buffer (Idx) := Type_Hash (I);
         Idx := Idx + 1;
      end loop;

      --  Then fields
      for I in Fields'Range loop
         Buffer (Idx) := Fields (I);
         Idx := Idx + 1;
      end loop;

      return SHA3_256 (Buffer (0 .. Idx - 1));
   end Encode_Struct;

end Khepri_Crypto;
