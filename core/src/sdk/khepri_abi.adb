pragma SPARK_Mode (On);

with Khepri_Crypto; use Khepri_Crypto;
with Aegis_U256; use Aegis_U256;

package body Khepri_ABI with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Align size to next 32-byte boundary
   function Align_To_Word (Size : Natural) return Natural with
      Global => null,
      Post   => Align_To_Word'Result >= Size and
                Align_To_Word'Result mod Word_Size = 0
   is
      Remainder : constant Natural := Size mod Word_Size;
   begin
      if Remainder = 0 then
         return Size;
      else
         return Size + (Word_Size - Remainder);
      end if;
   end Align_To_Word;

   ---------------------------------------------------------------------------
   --  Encoder Implementation
   ---------------------------------------------------------------------------

   function New_Encoder return ABI_Encoder is
   begin
      return Empty_Encoder;
   end New_Encoder;

   function Encoder_Size (Encoder : ABI_Encoder) return Natural is
   begin
      return Encoder.Offset;
   end Encoder_Size;

   procedure Encode_Uint256 (
      Encoder : in out ABI_Encoder;
      Value   : in     Uint256;
      Success : out    Boolean
   ) is
      Bytes : constant Bytes32 := U256_To_Bytes (Value);
   begin
      if Encoder.Offset + Word_Size > Max_ABI_Size then
         Success := False;
         return;
      end if;

      --  Copy big-endian bytes
      for I in Bytes'Range loop
         pragma Loop_Invariant (Encoder.Offset + Word_Size <= Max_ABI_Size);
         pragma Loop_Invariant (I in Bytes'Range);
         Encoder.Data (Encoder.Offset + I) := Bytes (I);
      end loop;

      Encoder.Offset := Encoder.Offset + Word_Size;
      Success := True;
   end Encode_Uint256;

   procedure Encode_Address (
      Encoder : in out ABI_Encoder;
      Addr    : in     Address;
      Success : out    Boolean
   ) is
   begin
      if Encoder.Offset + Word_Size > Max_ABI_Size then
         Success := False;
         return;
      end if;

      --  Left-pad with 12 zeros
      for I in 0 .. 11 loop
         pragma Loop_Invariant (Encoder.Offset + Word_Size <= Max_ABI_Size);
         pragma Loop_Invariant (I <= 11);
         Encoder.Data (Encoder.Offset + I) := 0;
      end loop;

      --  Copy address (20 bytes)
      for I in Addr'Range loop
         pragma Loop_Invariant (Encoder.Offset + Word_Size <= Max_ABI_Size);
         pragma Loop_Invariant (I in Addr'Range);
         Encoder.Data (Encoder.Offset + 12 + I) := Addr (I);
      end loop;

      Encoder.Offset := Encoder.Offset + Word_Size;
      Success := True;
   end Encode_Address;

   procedure Encode_Bool (
      Encoder : in out ABI_Encoder;
      Value   : in     Boolean;
      Success : out    Boolean
   ) is
      U256_Value : constant Uint256 := (if Value then One else Zero);
   begin
      Encode_Uint256 (Encoder, U256_Value, Success);
   end Encode_Bool;

   procedure Encode_Bytes32 (
      Encoder : in out ABI_Encoder;
      Value   : in     Bytes32;
      Success : out    Boolean
   ) is
   begin
      if Encoder.Offset + Word_Size > Max_ABI_Size then
         Success := False;
         return;
      end if;

      --  Copy bytes directly
      for I in Value'Range loop
         pragma Loop_Invariant (Encoder.Offset + Word_Size <= Max_ABI_Size);
         pragma Loop_Invariant (I in Value'Range);
         Encoder.Data (Encoder.Offset + I) := Value (I);
      end loop;

      Encoder.Offset := Encoder.Offset + Word_Size;
      Success := True;
   end Encode_Bytes32;

   procedure Encode_Bytes4 (
      Encoder : in out ABI_Encoder;
      Value   : in     Bytes4;
      Success : out    Boolean
   ) is
   begin
      if Encoder.Offset + Word_Size > Max_ABI_Size then
         Success := False;
         return;
      end if;

      --  Copy 4 bytes
      for I in Value'Range loop
         pragma Loop_Invariant (Encoder.Offset + Word_Size <= Max_ABI_Size);
         pragma Loop_Invariant (I in Value'Range);
         Encoder.Data (Encoder.Offset + I) := Value (I);
      end loop;

      --  Right-pad with zeros
      for I in 4 .. 31 loop
         pragma Loop_Invariant (Encoder.Offset + Word_Size <= Max_ABI_Size);
         pragma Loop_Invariant (I >= 4 and I <= 31);
         Encoder.Data (Encoder.Offset + I) := 0;
      end loop;

      Encoder.Offset := Encoder.Offset + Word_Size;
      Success := True;
   end Encode_Bytes4;

   procedure Encode_Bytes (
      Encoder : in out ABI_Encoder;
      Data    : in     Byte_Array;
      Length  : in     Natural;
      Success : out    Boolean
   ) is
      Length_U256 : constant Uint256 := From_Word64 (Word64 (Length));
      Aligned     : constant Natural := Align_To_Word (Length);
      Temp_Success : Boolean;
   begin
      if Length > Data'Length or
         Encoder.Offset + Word_Size + Aligned > Max_ABI_Size
      then
         Success := False;
         return;
      end if;

      --  Encode length
      Encode_Uint256 (Encoder, Length_U256, Temp_Success);
      if not Temp_Success then
         Success := False;
         return;
      end if;

      --  Copy data
      for I in 0 .. Length - 1 loop
         pragma Loop_Invariant (Encoder.Offset + Aligned <= Max_ABI_Size);
         pragma Loop_Invariant (I < Length);
         Encoder.Data (Encoder.Offset + I) := Data (Data'First + I);
      end loop;

      --  Pad to word boundary
      for I in Length .. Aligned - 1 loop
         pragma Loop_Invariant (Encoder.Offset + Aligned <= Max_ABI_Size);
         pragma Loop_Invariant (I >= Length and I < Aligned);
         Encoder.Data (Encoder.Offset + I) := 0;
      end loop;

      Encoder.Offset := Encoder.Offset + Aligned;
      Success := True;
   end Encode_Bytes;

   procedure Encode_String (
      Encoder : in out ABI_Encoder;
      Str     : in     String;
      Success : out    Boolean
   ) is
      Bytes : Byte_Array (0 .. Str'Length - 1);
   begin
      --  Convert string to bytes
      for I in Str'Range loop
         pragma Loop_Invariant (I - Str'First >= 0 and I - Str'First < Str'Length);
         Bytes (I - Str'First) := Character'Pos (Str (I));
      end loop;

      Encode_Bytes (Encoder, Bytes, Str'Length, Success);
   end Encode_String;

   procedure Encode_Bounded_String (
      Encoder : in out ABI_Encoder;
      Str     : in     Bounded_String;
      Success : out    Boolean
   ) is
      Bytes : Byte_Array (0 .. Str.Length - 1);
   begin
      if Str.Length = 0 then
         Encode_Bytes (Encoder, Bytes, 0, Success);
         return;
      end if;

      --  Convert to bytes
      for I in 0 .. Str.Length - 1 loop
         pragma Loop_Invariant (I < Str.Length);
         Bytes (I) := Character'Pos (Str.Data (I));
      end loop;

      Encode_Bytes (Encoder, Bytes, Str.Length, Success);
   end Encode_Bounded_String;

   procedure Finalize_Encoding (
      Encoder : in     ABI_Encoder;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
   begin
      if Encoder.Offset > Output'Length then
         Success := False;
         Size := 0;
         return;
      end if;

      --  Copy encoded data
      for I in 0 .. Encoder.Offset - 1 loop
         pragma Loop_Invariant (I < Encoder.Offset);
         pragma Loop_Invariant (Encoder.Offset <= Output'Length);
         Output (Output'First + I) := Encoder.Data (I);
      end loop;

      Size := Encoder.Offset;
      Success := True;
   end Finalize_Encoding;

   ---------------------------------------------------------------------------
   --  Decoder Implementation
   ---------------------------------------------------------------------------

   function New_Decoder (Data : Byte_Array) return ABI_Decoder is
      Decoder : ABI_Decoder;
   begin
      Decoder.Size := Data'Length;
      Decoder.Offset := 0;

      --  Copy input data
      for I in Data'Range loop
         pragma Loop_Invariant (I - Data'First >= 0 and I - Data'First <= Data'Length);
         Decoder.Data (I - Data'First) := Data (I);
      end loop;

      return Decoder;
   end New_Decoder;

   function Decoder_Remaining (Decoder : ABI_Decoder) return Natural is
   begin
      if Decoder.Offset >= Decoder.Size then
         return 0;
      else
         return Decoder.Size - Decoder.Offset;
      end if;
   end Decoder_Remaining;

   function Has_Remaining (
      Decoder : ABI_Decoder;
      Count   : Natural
   ) return Boolean is
   begin
      return Decoder_Remaining (Decoder) >= Count;
   end Has_Remaining;

   procedure Decode_Uint256 (
      Decoder : in out ABI_Decoder;
      Value   : out    Uint256;
      Success : out    Boolean
   ) is
      Bytes : Bytes32;
   begin
      if not Has_Remaining (Decoder, Word_Size) then
         Success := False;
         Value := Zero;
         return;
      end if;

      --  Extract 32 bytes
      for I in 0 .. Word_Size - 1 loop
         pragma Loop_Invariant (Decoder.Offset + Word_Size <= Decoder.Size);
         pragma Loop_Invariant (I < Word_Size);
         Bytes (I) := Decoder.Data (Decoder.Offset + I);
      end loop;

      Value := Bytes_To_U256 (Bytes);
      Decoder.Offset := Decoder.Offset + Word_Size;
      Success := True;
   end Decode_Uint256;

   procedure Decode_Address (
      Decoder : in out ABI_Decoder;
      Addr    : out    Address;
      Success : out    Boolean
   ) is
   begin
      if not Has_Remaining (Decoder, Word_Size) then
         Success := False;
         Addr := Null_Address;
         return;
      end if;

      --  Skip 12 padding bytes, extract 20-byte address
      for I in Addr'Range loop
         pragma Loop_Invariant (Decoder.Offset + Word_Size <= Decoder.Size);
         pragma Loop_Invariant (I in Addr'Range);
         Addr (I) := Decoder.Data (Decoder.Offset + 12 + I);
      end loop;

      Decoder.Offset := Decoder.Offset + Word_Size;
      Success := True;
   end Decode_Address;

   procedure Decode_Bool (
      Decoder : in out ABI_Decoder;
      Value   : out    Boolean;
      Success : out    Boolean
   ) is
      U256_Value : Uint256;
      Temp_Success : Boolean;
   begin
      Decode_Uint256 (Decoder, U256_Value, Temp_Success);
      if not Temp_Success then
         Success := False;
         Value := False;
         return;
      end if;

      --  Check if 0 or 1
      if Equal (U256_Value, Zero) then
         Value := False;
         Success := True;
      elsif Equal (U256_Value, One) then
         Value := True;
         Success := True;
      else
         --  Invalid boolean encoding
         Value := False;
         Success := False;
      end if;
   end Decode_Bool;

   procedure Decode_Bytes32 (
      Decoder : in out ABI_Decoder;
      Value   : out    Bytes32;
      Success : out    Boolean
   ) is
   begin
      if not Has_Remaining (Decoder, Word_Size) then
         Success := False;
         Value := Bytes32_Zero;
         return;
      end if;

      --  Extract 32 bytes
      for I in Value'Range loop
         pragma Loop_Invariant (Decoder.Offset + Word_Size <= Decoder.Size);
         pragma Loop_Invariant (I in Value'Range);
         Value (I) := Decoder.Data (Decoder.Offset + I);
      end loop;

      Decoder.Offset := Decoder.Offset + Word_Size;
      Success := True;
   end Decode_Bytes32;

   procedure Decode_Bytes4 (
      Decoder : in out ABI_Decoder;
      Value   : out    Bytes4;
      Success : out    Boolean
   ) is
   begin
      if not Has_Remaining (Decoder, Word_Size) then
         Success := False;
         Value := (others => 0);
         return;
      end if;

      --  Extract first 4 bytes
      for I in Value'Range loop
         pragma Loop_Invariant (Decoder.Offset + Word_Size <= Decoder.Size);
         pragma Loop_Invariant (I in Value'Range);
         Value (I) := Decoder.Data (Decoder.Offset + I);
      end loop;

      Decoder.Offset := Decoder.Offset + Word_Size;
      Success := True;
   end Decode_Bytes4;

   procedure Decode_Bytes (
      Decoder : in out ABI_Decoder;
      Output  : out    Byte_Array;
      Length  : out    Natural;
      Success : out    Boolean
   ) is
      Length_U256 : Uint256;
      Temp_Success : Boolean;
      Byte_Length : Natural;
   begin
      --  Decode length
      Decode_Uint256 (Decoder, Length_U256, Temp_Success);
      if not Temp_Success then
         Success := False;
         Length := 0;
         return;
      end if;

      --  Convert to natural (assuming it fits)
      Byte_Length := Natural (To_Word64 (Length_U256));

      if Byte_Length > Output'Length or
         not Has_Remaining (Decoder, Align_To_Word (Byte_Length))
      then
         Success := False;
         Length := 0;
         return;
      end if;

      --  Extract data
      for I in 0 .. Byte_Length - 1 loop
         pragma Loop_Invariant (I < Byte_Length);
         pragma Loop_Invariant (Decoder.Offset + Align_To_Word (Byte_Length) <= Decoder.Size);
         Output (Output'First + I) := Decoder.Data (Decoder.Offset + I);
      end loop;

      Decoder.Offset := Decoder.Offset + Align_To_Word (Byte_Length);
      Length := Byte_Length;
      Success := True;
   end Decode_Bytes;

   procedure Skip_Words (
      Decoder : in out ABI_Decoder;
      Count   : in     Natural;
      Success : out    Boolean
   ) is
      Skip_Amount : constant Natural := Count * Word_Size;
   begin
      if not Has_Remaining (Decoder, Skip_Amount) then
         Success := False;
         return;
      end if;

      Decoder.Offset := Decoder.Offset + Skip_Amount;
      Success := True;
   end Skip_Words;

   ---------------------------------------------------------------------------
   --  Selector Calculation
   ---------------------------------------------------------------------------

   function Calculate_Selector (Signature : String) return Bytes4 is
      Hash : constant Bytes32 := Calculate_Selector_Hash (Signature);
      Result : Bytes4;
   begin
      --  Take first 4 bytes of keccak256
      for I in Result'Range loop
         pragma Loop_Invariant (I in Result'Range);
         Result (I) := Hash (I);
      end loop;
      return Result;
   end Calculate_Selector;

   function Calculate_Selector_Hash (Signature : String) return Bytes32 is
      Sig_Bytes : Byte_Array (0 .. Signature'Length - 1);
   begin
      --  Convert to bytes
      for I in Signature'Range loop
         pragma Loop_Invariant (I - Signature'First >= 0 and I - Signature'First < Signature'Length);
         Sig_Bytes (I - Signature'First) := Character'Pos (Signature (I));
      end loop;

      --  Use Keccak-256 (Ethereum-compatible)
      return Keccak_256 (Sig_Bytes);
   end Calculate_Selector_Hash;

   ---------------------------------------------------------------------------
   --  Event Signature
   ---------------------------------------------------------------------------

   function Calculate_Event_Signature (Declaration : String) return Bytes32 is
   begin
      --  Same as function signature
      return Calculate_Selector_Hash (Declaration);
   end Calculate_Event_Signature;

   ---------------------------------------------------------------------------
   --  EIP-712 Implementation
   ---------------------------------------------------------------------------

   function Encode_Domain_Separator (Domain : Domain_Separator) return Bytes32 is
      --  EIP-712 domain separator:
      --  keccak256(abi.encode(
      --      TYPE_HASH,
      --      keccak256(bytes(name)),
      --      keccak256(bytes(version)),
      --      chainId,
      --      verifyingContract
      --  ))

      --  Build the ABI-encoded data for hashing
      Encoded_Data : Byte_Array (0 .. 159);  -- 5 * 32 bytes
      Pos : Natural := 0;

      --  Helper to convert bounded string to bytes for hashing
      function Hash_Bounded_String (S : Bounded_String) return Bytes32 is
         Str_Bytes : Byte_Array (0 .. S.Length - 1);
      begin
         if S.Length = 0 then
            --  keccak256("") = specific hash
            return Keccak_256 ((1 .. 0 => 0));
         end if;

         for I in 0 .. S.Length - 1 loop
            Str_Bytes (I) := Character'Pos (S.Data (I));
         end loop;
         return Keccak_256 (Str_Bytes);
      end Hash_Bounded_String;

      Name_Hash    : constant Bytes32 := Hash_Bounded_String (Domain.Name);
      Version_Hash : constant Bytes32 := Hash_Bounded_String (Domain.Version);
   begin
      --  1. Type hash (32 bytes)
      for I in EIP712_Domain_Type_Hash'Range loop
         Encoded_Data (Pos) := EIP712_Domain_Type_Hash (I);
         Pos := Pos + 1;
      end loop;

      --  2. Name hash (32 bytes)
      for I in Name_Hash'Range loop
         Encoded_Data (Pos) := Name_Hash (I);
         Pos := Pos + 1;
      end loop;

      --  3. Version hash (32 bytes)
      for I in Version_Hash'Range loop
         Encoded_Data (Pos) := Version_Hash (I);
         Pos := Pos + 1;
      end loop;

      --  4. Chain ID (32 bytes, big-endian)
      for I in 0 .. 31 loop
         Encoded_Data (Pos) := Domain.Chain_ID (I);
         Pos := Pos + 1;
      end loop;

      --  5. Verifying contract address (32 bytes, left-padded)
      for I in 0 .. 11 loop
         Encoded_Data (Pos) := 0;  -- Left padding
         Pos := Pos + 1;
      end loop;
      for I in Domain.Verifying_Contract'Range loop
         Encoded_Data (Pos) := Domain.Verifying_Contract (I);
         Pos := Pos + 1;
      end loop;

      --  Hash the encoded data
      return Keccak_256 (Encoded_Data);
   end Encode_Domain_Separator;

   function Calculate_Type_Hash (Type_String : String) return Bytes32 is
   begin
      return Calculate_Selector_Hash (Type_String);
   end Calculate_Type_Hash;

   function Encode_Struct_Hash (
      Type_Hash : Bytes32;
      Fields    : Byte_Array
   ) return Bytes32 is
      Combined : Byte_Array (0 .. 31 + Fields'Length);
   begin
      --  Copy type hash
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I <= 31);
         Combined (I) := Type_Hash (I);
      end loop;

      --  Copy fields
      for I in Fields'Range loop
         pragma Loop_Invariant (I - Fields'First >= 0 and I - Fields'First <= Fields'Length);
         Combined (32 + I - Fields'First) := Fields (I);
      end loop;

      return Keccak_256 (Combined);
   end Encode_Struct_Hash;

   function Calculate_EIP712_Hash (
      Domain        : Domain_Separator;
      Struct_Hash   : Bytes32
   ) return Bytes32 is
      Domain_Sep : constant Bytes32 := Encode_Domain_Separator (Domain);
      Combined : Byte_Array (0 .. 65);
   begin
      --  EIP-712 format: 0x1901 || domainSeparator || structHash
      Combined (0) := 16#19#;
      Combined (1) := 16#01#;

      for I in 0 .. 31 loop
         pragma Loop_Invariant (I <= 31);
         Combined (2 + I) := Domain_Sep (I);
         Combined (34 + I) := Struct_Hash (I);
      end loop;

      return Keccak_256 (Combined);
   end Calculate_EIP712_Hash;

   ---------------------------------------------------------------------------
   --  Convenience Functions
   ---------------------------------------------------------------------------

   function Encode_Function_Call (
      Selector : Bytes4;
      Args     : Byte_Array
   ) return Byte_Array is
      Result : Byte_Array (0 .. 3 + Args'Length);
   begin
      --  Copy selector
      for I in Selector'Range loop
         pragma Loop_Invariant (I in Selector'Range);
         Result (I) := Selector (I);
      end loop;

      --  Copy args
      for I in Args'Range loop
         pragma Loop_Invariant (I - Args'First >= 0 and I - Args'First <= Args'Length);
         Result (4 + I - Args'First) := Args (I);
      end loop;

      return Result;
   end Encode_Function_Call;

   procedure Encode_Transfer_Call (
      To      : in  Address;
      Amount  : in  Uint256;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) is
      Transfer_Selector : constant Bytes4 := Calculate_Selector ("transfer(address,uint256)");
      Encoder : ABI_Encoder := New_Encoder;
      Temp_Success : Boolean;
   begin
      --  Encode selector
      for I in Transfer_Selector'Range loop
         pragma Loop_Invariant (I in Transfer_Selector'Range);
         Output (Output'First + I) := Transfer_Selector (I);
      end loop;

      --  Encode arguments
      Encode_Address (Encoder, To, Temp_Success);
      if not Temp_Success then
         Success := False;
         Size := 0;
         return;
      end if;

      Encode_Uint256 (Encoder, Amount, Temp_Success);
      if not Temp_Success then
         Success := False;
         Size := 0;
         return;
      end if;

      --  Copy encoded data after selector
      declare
         Encoded_Size : Natural;
      begin
         Finalize_Encoding (Encoder, Output (Output'First + 4 .. Output'Last), Encoded_Size, Temp_Success);
         if not Temp_Success then
            Success := False;
            Size := 0;
            return;
         end if;

         Size := 4 + Encoded_Size;
         Success := True;
      end;
   end Encode_Transfer_Call;

   procedure Encode_Approve_Call (
      Spender : in  Address;
      Amount  : in  Uint256;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) is
      Approve_Selector : constant Bytes4 := Calculate_Selector ("approve(address,uint256)");
      Encoder : ABI_Encoder := New_Encoder;
      Temp_Success : Boolean;
   begin
      for I in Approve_Selector'Range loop
         pragma Loop_Invariant (I in Approve_Selector'Range);
         Output (Output'First + I) := Approve_Selector (I);
      end loop;

      Encode_Address (Encoder, Spender, Temp_Success);
      if not Temp_Success then
         Success := False;
         Size := 0;
         return;
      end if;

      Encode_Uint256 (Encoder, Amount, Temp_Success);
      if not Temp_Success then
         Success := False;
         Size := 0;
         return;
      end if;

      declare
         Encoded_Size : Natural;
      begin
         Finalize_Encoding (Encoder, Output (Output'First + 4 .. Output'Last), Encoded_Size, Temp_Success);
         if not Temp_Success then
            Success := False;
            Size := 0;
            return;
         end if;

         Size := 4 + Encoded_Size;
         Success := True;
      end;
   end Encode_Approve_Call;

   procedure Encode_BalanceOf_Call (
      Account : in  Address;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) is
      BalanceOf_Selector : constant Bytes4 := Calculate_Selector ("balanceOf(address)");
      Encoder : ABI_Encoder := New_Encoder;
      Temp_Success : Boolean;
   begin
      for I in BalanceOf_Selector'Range loop
         pragma Loop_Invariant (I in BalanceOf_Selector'Range);
         Output (Output'First + I) := BalanceOf_Selector (I);
      end loop;

      Encode_Address (Encoder, Account, Temp_Success);
      if not Temp_Success then
         Success := False;
         Size := 0;
         return;
      end if;

      declare
         Encoded_Size : Natural;
      begin
         Finalize_Encoding (Encoder, Output (Output'First + 4 .. Output'Last), Encoded_Size, Temp_Success);
         if not Temp_Success then
            Success := False;
            Size := 0;
            return;
         end if;

         Size := 4 + Encoded_Size;
         Success := True;
      end;
   end Encode_BalanceOf_Call;

   ---------------------------------------------------------------------------
   --  Padding Utilities
   ---------------------------------------------------------------------------

   function Pad_Right (Data : Byte_Array; Target_Size : Positive) return Byte_Array is
      Result : Byte_Array (0 .. Target_Size - 1) := (others => 0);
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (I - Data'First >= 0 and I - Data'First < Data'Length);
         pragma Loop_Invariant (Data'Length <= Target_Size);
         Result (I - Data'First) := Data (I);
      end loop;
      return Result;
   end Pad_Right;

   function Pad_Left (Data : Byte_Array; Target_Size : Positive) return Byte_Array is
      Result : Byte_Array (0 .. Target_Size - 1) := (others => 0);
      Offset : constant Natural := Target_Size - Data'Length;
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (I - Data'First >= 0 and I - Data'First < Data'Length);
         pragma Loop_Invariant (Data'Length <= Target_Size);
         Result (Offset + I - Data'First) := Data (I);
      end loop;
      return Result;
   end Pad_Left;

   ---------------------------------------------------------------------------
   --  Conversion Helpers
   ---------------------------------------------------------------------------

   function U256_To_Bytes (Value : Uint256) return Bytes32 is
      Result : Bytes32;
      Limbs : constant U256_Limbs := Value.Limbs;
   begin
      --  Convert to big-endian bytes
      for I in 0 .. 3 loop
         pragma Loop_Invariant (I <= 3);
         declare
            Limb : constant Word64 := Limbs (3 - I);  --  Reverse order for big-endian
            Offset : constant Natural := I * 8;
         begin
            for J in 0 .. 7 loop
               pragma Loop_Invariant (J <= 7);
               pragma Loop_Invariant (Offset + J <= 31);
               Result (Offset + J) := Byte (Shift_Right (Limb, (7 - J) * 8) and 16#FF#);
            end loop;
         end;
      end loop;
      return Result;
   end U256_To_Bytes;

   function Bytes_To_U256 (Data : Bytes32) return Uint256 is
      Limbs : U256_Limbs := (0, 0, 0, 0);
   begin
      --  Convert from big-endian bytes
      for I in 0 .. 3 loop
         pragma Loop_Invariant (I <= 3);
         declare
            Limb : Word64 := 0;
            Offset : constant Natural := I * 8;
         begin
            for J in 0 .. 7 loop
               pragma Loop_Invariant (J <= 7);
               pragma Loop_Invariant (Offset + J <= 31);
               Limb := Limb or Shift_Left (Word64 (Data (Offset + J)), (7 - J) * 8);
            end loop;
            Limbs (3 - I) := Limb;  --  Reverse order for big-endian
         end;
      end loop;
      return (Limbs => Limbs);
   end Bytes_To_U256;

   function Address_To_Bytes (Addr : Address) return Byte_Array is
      Result : Byte_Array (0 .. 31) := (others => 0);
   begin
      --  Left-pad to 32 bytes
      for I in Addr'Range loop
         pragma Loop_Invariant (I in Addr'Range);
         Result (12 + I) := Addr (I);
      end loop;
      return Result;
   end Address_To_Bytes;

   function Bytes_To_Address (Data : Bytes32) return Address is
      Result : Address;
   begin
      --  Extract 20 bytes starting at offset 12
      for I in Result'Range loop
         pragma Loop_Invariant (I in Result'Range);
         Result (I) := Data (12 + I);
      end loop;
      return Result;
   end Bytes_To_Address;

end Khepri_ABI;
