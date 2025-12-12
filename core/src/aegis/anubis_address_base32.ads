pragma SPARK_Mode (On);

with Anubis_Address_Types; use Anubis_Address_Types;
with Interfaces; use Interfaces;

--  Anubis_Address_Base32: Crockford Base32 encoding/decoding
--
--  Crockford Base32 alphabet: 0-9, A-Z excluding I, L, O, U
--  Total: 32 characters (5 bits per character)
--
--  Properties:
--  - Case-insensitive decoding (accepts both cases)
--  - Lowercase output (AAS-001 v3.1 canonical form)
--  - No padding characters
--
--  Encoding:
--  - 32 bytes (256 bits) → 52 Base32 chars (260 bits, 4 padding bits)
--  - 3 bytes (24 bits) → 5 Base32 chars (25 bits, 1 padding bit)
--
--  References:
--  - https://www.crockford.com/base32.html
--  - AAS-001 v3.1

package Anubis_Address_Base32 with
   SPARK_Mode => On
is

   --  Encode 32 bytes to 52 Base32 characters
   --  Input: 256 bits, Output: 52 chars (260 bits, 4 bits padding at end)
   procedure Encode_Account_ID (
      Input  : in  Account_ID;
      Output : out Payload_Base32
   ) with
      Global => null,
      Depends => (Output => Input),
      Post => (for all I in Payload_Index => Output (I) in Base32_Char);

   --  Encode 3 bytes to 5 Base32 characters
   --  Input: 24 bits, Output: 5 chars (25 bits, 1 bit padding at end)
   procedure Encode_Checksum (
      Input  : in  Checksum_Bytes;
      Output : out Checksum_Base32
   ) with
      Global => null,
      Post => (for all I in Checksum_B32_Index => Output (I) in Base32_Char);

   --  Decode 52 Base32 characters to 32 bytes
   --  Returns success status; output undefined on failure
   procedure Decode_Account_ID (
      Input   : in  Payload_Base32;
      Output  : out Account_ID;
      Success : out Boolean
   ) with
      Global => null;

   --  Decode 5 Base32 characters to 3 bytes
   --  Returns success status; output undefined on failure
   procedure Decode_Checksum (
      Input   : in  Checksum_Base32;
      Output  : out Checksum_Bytes;
      Success : out Boolean
   ) with
      Global => null;

   --  Chunk payload into 8-8-8-8-8-8-4 format with dashes
   --  Input: 52 chars, Output: 58 chars (6 groups of 8, 6 dashes, 1 group of 4)
   --  Uses Chunked_Payload type from Anubis_Address_Types
   procedure Chunk_Payload (
      Input  : in  Payload_Base32;
      Output : out Chunked_Payload
   ) with
      Global => null,
      Post => Output (8) = '-' and Output (17) = '-' and Output (26) = '-' and
              Output (35) = '-' and Output (44) = '-' and Output (53) = '-';

   --  Unchunk payload: remove dashes from 8-8-8-8-8-8-4 format
   --  Input: 58 chars (with 6 dashes), Output: 52 chars (no dashes)
   --  Returns success status if dashes are in correct positions
   procedure Unchunk_Payload (
      Input   : in  Chunked_Payload;
      Output  : out Payload_Base32;
      Success : out Boolean
   ) with
      Global => null;

private

   --  Lookup table for encoding: index 0..31 → Base32 char
   Encode_Table : constant Base32_Alphabet_Array := Base32_Alphabet;

   --  Helper: encode 5 bits to Base32 char
   function Encode_5bits (Value : Unsigned_8) return Base32_Char with
      Inline,
      Pre  => Value < 32,
      Post => Encode_5bits'Result in Base32_Char;

   --  Helper: decode Base32 char to 5 bits
   --  Returns 255 on invalid character
   function Decode_Char (C : Character) return Unsigned_8 with
      Inline,
      Post => Decode_Char'Result <= 31 or Decode_Char'Result = 255;

end Anubis_Address_Base32;
