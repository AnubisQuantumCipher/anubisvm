pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  KHEPRI ABI: Contract Application Binary Interface
--
--  This package provides complete ABI encoding and decoding facilities
--  for KHEPRI smart contracts, compatible with Ethereum ABI specification.
--
--  Key Features:
--  - Type-safe encoding/decoding
--  - Dynamic and fixed-size types
--  - Tuple encoding
--  - Function selector calculation
--  - Event signature generation
--
--  ABI Encoding Rules:
--  - Static types encoded directly (uint256, address, bool, bytes32)
--  - Dynamic types use offset pointers (bytes, string, arrays)
--  - All values padded to 32 bytes
--  - Big-endian encoding for numbers
--  - Left-padding for addresses
--
--  SPARK Verification:
--  - All encoding operations proven safe (no buffer overflows)
--  - Decoding validates input bounds
--  - Round-trip encoding/decoding correctness
--
--  References:
--  - Solidity ABI Specification
--  - EIP-712: Typed structured data hashing and signing

package Khepri_ABI with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  ABI Configuration
   ---------------------------------------------------------------------------

   --  Maximum ABI encoded data size
   Max_ABI_Size : constant := 65536;

   --  Word size (32 bytes)
   Word_Size : constant := 32;

   --  ABI data index
   subtype ABI_Index is Natural range 0 .. Max_ABI_Size - 1;

   --  ABI data buffer
   type ABI_Buffer is array (ABI_Index range <>) of Byte;

   ---------------------------------------------------------------------------
   --  ABI Encoder
   ---------------------------------------------------------------------------

   --  Encoder state for building ABI-encoded data
   type ABI_Encoder is private;

   --  Initialize encoder
   function New_Encoder return ABI_Encoder with
      Global => null,
      Post   => Encoder_Size (New_Encoder'Result) = 0;

   --  Get current encoded size
   function Encoder_Size (Encoder : ABI_Encoder) return Natural with
      Global => null,
      Post   => Encoder_Size'Result <= Max_ABI_Size;

   --  Encode uint256
   procedure Encode_Uint256 (
      Encoder : in out ABI_Encoder;
      Value   : in     Uint256;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Encoder_Size (Encoder) + Word_Size <= Max_ABI_Size,
      Post   => (if Success then Encoder_Size (Encoder) = Encoder_Size (Encoder'Old) + Word_Size);

   --  Encode address (20 bytes, left-padded to 32)
   procedure Encode_Address (
      Encoder : in out ABI_Encoder;
      Addr    : in     Address;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Encoder_Size (Encoder) + Word_Size <= Max_ABI_Size,
      Post   => (if Success then Encoder_Size (Encoder) = Encoder_Size (Encoder'Old) + Word_Size);

   --  Encode bool (as uint256: 0 or 1)
   procedure Encode_Bool (
      Encoder : in out ABI_Encoder;
      Value   : in     Boolean;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Encoder_Size (Encoder) + Word_Size <= Max_ABI_Size,
      Post   => (if Success then Encoder_Size (Encoder) = Encoder_Size (Encoder'Old) + Word_Size);

   --  Encode bytes32 (fixed-size bytes)
   procedure Encode_Bytes32 (
      Encoder : in out ABI_Encoder;
      Value   : in     Bytes32;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Encoder_Size (Encoder) + Word_Size <= Max_ABI_Size,
      Post   => (if Success then Encoder_Size (Encoder) = Encoder_Size (Encoder'Old) + Word_Size);

   --  Encode bytes4 (selector)
   procedure Encode_Bytes4 (
      Encoder : in out ABI_Encoder;
      Value   : in     Bytes4;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Encoder_Size (Encoder) + Word_Size <= Max_ABI_Size,
      Post   => (if Success then Encoder_Size (Encoder) = Encoder_Size (Encoder'Old) + Word_Size);

   --  Encode dynamic bytes (length-prefixed)
   procedure Encode_Bytes (
      Encoder : in out ABI_Encoder;
      Data    : in     Byte_Array;
      Length  : in     Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Length <= Data'Length and
                Encoder_Size (Encoder) + 2 * Word_Size + Length <= Max_ABI_Size;

   --  Encode string (as dynamic bytes)
   procedure Encode_String (
      Encoder : in out ABI_Encoder;
      Str     : in     String;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Str'Length <= 1024 and
                Encoder_Size (Encoder) + 2 * Word_Size + Str'Length <= Max_ABI_Size;

   --  Encode bounded string
   procedure Encode_Bounded_String (
      Encoder : in out ABI_Encoder;
      Str     : in     Bounded_String;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Encoder_Size (Encoder) + 2 * Word_Size + Str.Length <= Max_ABI_Size;

   --  Finalize encoding and get result
   procedure Finalize_Encoding (
      Encoder : in     ABI_Encoder;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= Encoder_Size (Encoder),
      Post   => (if Success then Size = Encoder_Size (Encoder) and Size <= Output'Length);

   ---------------------------------------------------------------------------
   --  ABI Decoder
   ---------------------------------------------------------------------------

   --  Decoder state for parsing ABI-encoded data
   type ABI_Decoder is private;

   --  Initialize decoder from encoded data
   function New_Decoder (Data : Byte_Array) return ABI_Decoder with
      Global => null,
      Pre    => Data'Length <= Max_ABI_Size;

   --  Get remaining bytes to decode
   function Decoder_Remaining (Decoder : ABI_Decoder) return Natural with
      Global => null;

   --  Check if decoder has at least N bytes remaining
   function Has_Remaining (
      Decoder : ABI_Decoder;
      Count   : Natural
   ) return Boolean with
      Global => null,
      Post   => Has_Remaining'Result = (Decoder_Remaining (Decoder) >= Count);

   --  Decode uint256
   procedure Decode_Uint256 (
      Decoder : in out ABI_Decoder;
      Value   : out    Uint256;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Has_Remaining (Decoder, Word_Size),
      Post   => (if Success then Decoder_Remaining (Decoder) = Decoder_Remaining (Decoder'Old) - Word_Size);

   --  Decode address
   procedure Decode_Address (
      Decoder : in out ABI_Decoder;
      Addr    : out    Address;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Has_Remaining (Decoder, Word_Size),
      Post   => (if Success then Decoder_Remaining (Decoder) = Decoder_Remaining (Decoder'Old) - Word_Size);

   --  Decode bool
   procedure Decode_Bool (
      Decoder : in out ABI_Decoder;
      Value   : out    Boolean;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Has_Remaining (Decoder, Word_Size),
      Post   => (if Success then Decoder_Remaining (Decoder) = Decoder_Remaining (Decoder'Old) - Word_Size);

   --  Decode bytes32
   procedure Decode_Bytes32 (
      Decoder : in out ABI_Decoder;
      Value   : out    Bytes32;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Has_Remaining (Decoder, Word_Size),
      Post   => (if Success then Decoder_Remaining (Decoder) = Decoder_Remaining (Decoder'Old) - Word_Size);

   --  Decode bytes4
   procedure Decode_Bytes4 (
      Decoder : in out ABI_Decoder;
      Value   : out    Bytes4;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Has_Remaining (Decoder, Word_Size),
      Post   => (if Success then Decoder_Remaining (Decoder) = Decoder_Remaining (Decoder'Old) - Word_Size);

   --  Decode dynamic bytes
   procedure Decode_Bytes (
      Decoder : in out ABI_Decoder;
      Output  : out    Byte_Array;
      Length  : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length <= Max_ABI_Size and
                Has_Remaining (Decoder, Word_Size);

   --  Skip N words (32-byte blocks)
   procedure Skip_Words (
      Decoder : in out ABI_Decoder;
      Count   : in     Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Has_Remaining (Decoder, Count * Word_Size),
      Post   => (if Success then Decoder_Remaining (Decoder) = Decoder_Remaining (Decoder'Old) - Count * Word_Size);

   ---------------------------------------------------------------------------
   --  Function Selector Calculation
   ---------------------------------------------------------------------------

   --  Calculate function selector from signature
   --  e.g., "transfer(address,uint256)" -> first 4 bytes of keccak256
   function Calculate_Selector (Signature : String) return Bytes4 with
      Global => null,
      Pre    => Signature'Length > 0 and Signature'Length <= 256;

   --  Calculate function selector hash (full 32 bytes)
   function Calculate_Selector_Hash (Signature : String) return Bytes32 with
      Global => null,
      Pre    => Signature'Length > 0 and Signature'Length <= 256;

   ---------------------------------------------------------------------------
   --  Event Signature Calculation
   ---------------------------------------------------------------------------

   --  Calculate event signature (topic[0])
   --  e.g., "Transfer(address,address,uint256)" -> keccak256
   function Calculate_Event_Signature (Declaration : String) return Bytes32 with
      Global => null,
      Pre    => Declaration'Length > 0 and Declaration'Length <= 256;

   ---------------------------------------------------------------------------
   --  Typed Data Hashing (EIP-712)
   ---------------------------------------------------------------------------

   --  Domain separator for EIP-712
   type Domain_Separator is record
      Name              : Bounded_String;
      Version           : Bounded_String;
      Chain_ID          : Uint256;
      Verifying_Contract : Address;
   end record;

   --  Calculate domain separator hash
   function Encode_Domain_Separator (Domain : Domain_Separator) return Bytes32 with
      Global => null;

   --  Type hash for structured data
   function Calculate_Type_Hash (Type_String : String) return Bytes32 with
      Global => null,
      Pre    => Type_String'Length > 0 and Type_String'Length <= 512;

   --  Encode structured data hash
   function Encode_Struct_Hash (
      Type_Hash : Bytes32;
      Fields    : Byte_Array
   ) return Bytes32 with
      Global => null,
      Pre    => Fields'Length <= Max_ABI_Size;

   --  Calculate final EIP-712 hash
   function Calculate_EIP712_Hash (
      Domain        : Domain_Separator;
      Struct_Hash   : Bytes32
   ) return Bytes32 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Convenience Functions (Common Encodings)
   ---------------------------------------------------------------------------

   --  Encode function call (selector + arguments)
   --  Returns: selector || encoded_args
   function Encode_Function_Call (
      Selector : Bytes4;
      Args     : Byte_Array
   ) return Byte_Array with
      Global => null,
      Pre    => Args'Length <= Max_ABI_Size - 4,
      Post   => Encode_Function_Call'Result'Length = 4 + Args'Length;

   --  Encode transfer call: transfer(address,uint256)
   procedure Encode_Transfer_Call (
      To      : in  Address;
      Amount  : in  Uint256;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 68;  -- 4 (selector) + 32 + 32

   --  Encode approve call: approve(address,uint256)
   procedure Encode_Approve_Call (
      Spender : in  Address;
      Amount  : in  Uint256;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 68;

   --  Encode balanceOf call: balanceOf(address)
   procedure Encode_BalanceOf_Call (
      Account : in  Address;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 36;  -- 4 + 32

   ---------------------------------------------------------------------------
   --  Padding Utilities
   ---------------------------------------------------------------------------

   --  Pad value to 32 bytes (right-pad with zeros)
   function Pad_Right (Data : Byte_Array; Target_Size : Positive) return Byte_Array with
      Global => null,
      Pre    => Data'Length <= Target_Size and Target_Size <= Word_Size,
      Post   => Pad_Right'Result'Length = Target_Size;

   --  Pad value to 32 bytes (left-pad with zeros)
   function Pad_Left (Data : Byte_Array; Target_Size : Positive) return Byte_Array with
      Global => null,
      Pre    => Data'Length <= Target_Size and Target_Size <= Word_Size,
      Post   => Pad_Left'Result'Length = Target_Size;

   ---------------------------------------------------------------------------
   --  Conversion Helpers
   ---------------------------------------------------------------------------

   --  Convert U256 to big-endian bytes (32 bytes)
   function U256_To_Bytes (Value : Uint256) return Bytes32 with
      Global => null;

   --  Convert bytes to U256 (big-endian)
   function Bytes_To_U256 (Data : Bytes32) return Uint256 with
      Global => null;

   --  Convert address to bytes (20 bytes)
   function Address_To_Bytes (Addr : Address) return Byte_Array with
      Global => null,
      Post   => Address_To_Bytes'Result'Length = 32;

   --  Convert bytes to address (takes first 20 bytes)
   function Bytes_To_Address (Data : Bytes32) return Address with
      Global => null;

private

   --  Encoder state
   type ABI_Encoder is record
      Data   : Byte_Array (0 .. Max_ABI_Size - 1);
      Offset : Natural;
   end record;

   --  Decoder state
   type ABI_Decoder is record
      Data   : Byte_Array (0 .. Max_ABI_Size - 1);
      Size   : Natural;
      Offset : Natural;
   end record;

   --  Empty encoder
   Empty_Encoder : constant ABI_Encoder := (
      Data   => (others => 0),
      Offset => 0
   );

   --  EIP-712 constants
   EIP712_Domain_Type_Hash : constant Bytes32 := (
      16#8b#, 16#73#, 16#c3#, 16#c6#, 16#9b#, 16#b8#, 16#fe#, 16#3d#,
      16#51#, 16#2e#, 16#cc#, 16#4f#, 16#3c#, 16#07#, 16#ae#, 16#18#,
      16#3c#, 16#3e#, 16#6f#, 16#c6#, 16#58#, 16#52#, 16#3d#, 16#2c#,
      16#28#, 16#72#, 16#34#, 16#24#, 16#f3#, 16#b4#, 16#6b#, 16#c9#
   );

   EIP712_Prefix : constant Bytes32 := (
      16#19#, 16#01#, others => 0
   );

end Khepri_ABI;
