--  JSON Parser: RPC parameter parsing
--  Note: Implementation uses SPARK_Mode Off for complex string operations
--  The spec maintains full SPARK contracts for interface verification
pragma SPARK_Mode (Off);

with Anubis_Address_Types; use Anubis_Address_Types;
with Anubis_Address_Base32; use Anubis_Address_Base32;
with Anubis_Address_Checksum; use Anubis_Address_Checksum;
with Interfaces; use Interfaces;

package body Node_JSON_Parser with
   SPARK_Mode => Off,
   Refined_State => (
      Deploy_Buffer_State => Deploy_Hex_Buffer,
      Invoke_Buffer_State => Args_Hex_Buffer
   )
is

   ---------------------------------------------------------------------------
   --  Package-level buffers (avoid stack overflow with large contracts)
   ---------------------------------------------------------------------------

   --  Static buffer for hex string during deploy parsing (512KB)
   Deploy_Hex_Buffer : String (1 .. Max_Hex_Length) := (others => ' ');

   --  Static buffer for args hex parsing (16KB for up to 8KB binary)
   Args_Hex_Buffer : String (1 .. 16384) := (others => ' ');

   --  Hex character to nibble
   function Hex_Char_To_Nibble (C : Character) return Natural is
   begin
      case C is
         when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
         when others     => return 16;  -- Invalid
      end case;
   end Hex_Char_To_Nibble;

   --  Nibble to hex character
   function Nibble_To_Hex_Char (N : Natural) return Character is
      Hex_Chars : constant String := "0123456789abcdef";
   begin
      if N < 16 then
         return Hex_Chars (N + 1);
      else
         return '0';
      end if;
   end Nibble_To_Hex_Char;

   ---------------------------------------------------------------------------
   --  Hex Conversion
   ---------------------------------------------------------------------------

   procedure Hex_To_Bytes (
      Hex       : in     String;
      Bytes     : out    Node_Code_Buffer;
      Byte_Len  : out    Natural;
      Success   : out    Boolean
   ) is
      High, Low : Natural;
      Idx       : Natural := 0;
   begin
      --  Zero Bytes with loop to avoid 256KB stack temporary from aggregate
      for I in Bytes'Range loop
         Bytes (I) := 0;
      end loop;
      Byte_Len := 0;
      Success := False;

      --  Must be even length
      if Hex'Length mod 2 /= 0 then
         return;
      end if;

      --  Check length fits
      if Hex'Length / 2 > Node_Max_Code_Size then
         return;
      end if;

      --  Convert pairs
      declare
         I : Natural := Hex'First;
      begin
         while I < Hex'Last loop
            High := Hex_Char_To_Nibble (Hex (I));
            Low := Hex_Char_To_Nibble (Hex (I + 1));

            if High > 15 or Low > 15 then
               return;  -- Invalid hex
            end if;

            Bytes (Node_Code_Index (Idx)) := Byte (High * 16 + Low);
            Idx := Idx + 1;
            I := I + 2;
         end loop;
      end;

      Byte_Len := Idx;
      Success := True;
   end Hex_To_Bytes;

   procedure Bytes_To_Hex (
      Bytes     : in     Hash256;
      Hex       : out    String;
      Hex_Len   : out    Natural
   ) is
      Idx : Natural := Hex'First;
   begin
      Hex := (others => '0');

      for I in 0 .. 31 loop
         Hex (Idx) := Nibble_To_Hex_Char (Natural (Bytes (I)) / 16);
         Hex (Idx + 1) := Nibble_To_Hex_Char (Natural (Bytes (I)) mod 16);
         Idx := Idx + 2;
      end loop;

      Hex_Len := 64;
   end Bytes_To_Hex;

   ---------------------------------------------------------------------------
   --  JSON Field Extraction
   ---------------------------------------------------------------------------

   procedure Extract_String (
      Json      : in     String;
      Key       : in     String;
      Value     : out    String;
      Value_Len : out    Natural;
      Found     : out    Boolean
   ) is
      Key_With_Quotes : String (1 .. Key'Length + 2);
      Key_Pos         : Natural := 0;
      Start, Stop     : Natural;
   begin
      --  Note: Don't initialize Value - (others => ' ') creates stack temporary
      --  equal to Value's size which can be 512KB for elf_hex extraction.
      --  Value_Len tracks the valid portion instead.
      Value_Len := 0;
      Found := False;

      --  Build "key" pattern
      Key_With_Quotes (1) := '"';
      Key_With_Quotes (2 .. Key'Length + 1) := Key;
      Key_With_Quotes (Key'Length + 2) := '"';

      --  Find key in JSON
      for I in Json'First .. Json'Last - Key_With_Quotes'Length + 1 loop
         if Json (I .. I + Key_With_Quotes'Length - 1) = Key_With_Quotes then
            Key_Pos := I + Key_With_Quotes'Length;
            exit;
         end if;
      end loop;

      if Key_Pos = 0 then
         return;  -- Key not found
      end if;

      --  Find opening quote after colon
      Start := Key_Pos;
      while Start <= Json'Last and then Json (Start) /= '"' loop
         Start := Start + 1;
      end loop;
      Start := Start + 1;  -- Skip opening quote

      if Start > Json'Last then
         return;
      end if;

      --  Find closing quote
      Stop := Start;
      while Stop <= Json'Last and then Json (Stop) /= '"' loop
         Stop := Stop + 1;
      end loop;

      if Stop > Json'Last or Stop <= Start then
         return;
      end if;

      --  Copy value
      declare
         Len : constant Natural := Stop - Start;
      begin
         if Len <= Value'Length then
            Value (Value'First .. Value'First + Len - 1) := Json (Start .. Stop - 1);
            Value_Len := Len;
            Found := True;
         end if;
      end;
   end Extract_String;

   procedure Extract_Integer (
      Json      : in     String;
      Key       : in     String;
      Value     : out    Natural;
      Found     : out    Boolean
   ) is
      Key_With_Quotes : String (1 .. Key'Length + 2);
      Key_Pos         : Natural := 0;
      Start           : Natural;
      Acc             : Natural := 0;
   begin
      Value := 0;
      Found := False;

      --  Build "key" pattern
      Key_With_Quotes (1) := '"';
      Key_With_Quotes (2 .. Key'Length + 1) := Key;
      Key_With_Quotes (Key'Length + 2) := '"';

      --  Find key in JSON
      for I in Json'First .. Json'Last - Key_With_Quotes'Length + 1 loop
         if Json (I .. I + Key_With_Quotes'Length - 1) = Key_With_Quotes then
            Key_Pos := I + Key_With_Quotes'Length;
            exit;
         end if;
      end loop;

      if Key_Pos = 0 then
         return;  -- Key not found
      end if;

      --  Skip to first digit (past colon and whitespace)
      Start := Key_Pos;
      while Start <= Json'Last and then
            not (Json (Start) in '0' .. '9') loop
         Start := Start + 1;
      end loop;

      if Start > Json'Last then
         return;
      end if;

      --  Parse digits
      while Start <= Json'Last and then
            (Json (Start) in '0' .. '9') loop
         Acc := Acc * 10 + (Character'Pos (Json (Start)) - Character'Pos ('0'));
         Start := Start + 1;
      end loop;

      Value := Acc;
      Found := True;
   end Extract_Integer;

   ---------------------------------------------------------------------------
   --  Deploy Request Parsing
   ---------------------------------------------------------------------------

   procedure Parse_Deploy_Params (
      Params_Json : in     String;
      From_Addr   : out    Contract_Address;
      Code        : out    Node_Code_Buffer;
      Code_Size   : out    Natural;
      Manifest    : out    Node_Contract_Manifest;
      Gas_Limit   : out    Gas_Amount;
      Success     : out    Boolean
   ) is
      --  Use package-level Deploy_Hex_Buffer instead of local 512KB allocation
      Hex_Len   : Natural;
      Name_Str  : String (1 .. Max_Contract_Name_Length);
      Name_Len  : Natural;
      Cert_Str  : String (1 .. 16);
      Cert_Len  : Natural;
      Gas_Val   : Natural;
      Found     : Boolean;
   begin
      From_Addr := (others => 0);
      --  Note: Don't initialize Code with (others => 0) - creates 256KB stack temp
      --  Hex_To_Bytes will initialize it via loop instead
      Code_Size := 0;
      Manifest := (
         Name          => (others => ' '),
         Name_Len      => 0,
         Version_Major => 0,
         Version_Minor => 0,
         Version_Patch => 0,
         Cert          => Cert_None
      );
      Gas_Limit := 0;
      Success := False;

      --  Extract elf_hex (using static buffer)
      --  Note: Don't zero Deploy_Hex_Buffer - (others => ' ') creates 512KB
      --  stack temporary. Hex_Len tracks the valid portion instead.
      Extract_String (Params_Json, "elf_hex", Deploy_Hex_Buffer, Hex_Len, Found);
      if not Found or Hex_Len = 0 then
         return;
      end if;

      --  Convert hex to bytes
      Hex_To_Bytes (Deploy_Hex_Buffer (1 .. Hex_Len), Code, Code_Size, Found);
      if not Found or Code_Size = 0 then
         return;
      end if;

      --  Extract manifest.name
      Extract_String (Params_Json, "name", Name_Str, Name_Len, Found);
      if Found and Name_Len > 0 then
         Manifest.Name (1 .. Name_Len) := Name_Str (1 .. Name_Len);
         Manifest.Name_Len := Name_Len;
      end if;

      --  Extract version fields
      Extract_Integer (Params_Json, "version_major", Manifest.Version_Major, Found);
      Extract_Integer (Params_Json, "version_minor", Manifest.Version_Minor, Found);
      Extract_Integer (Params_Json, "version_patch", Manifest.Version_Patch, Found);

      --  Extract cert level
      Extract_String (Params_Json, "cert", Cert_Str, Cert_Len, Found);
      if Found and Cert_Len > 0 then
         if Cert_Str (1 .. Cert_Len) = "bronze" then
            Manifest.Cert := Cert_Bronze;
         elsif Cert_Str (1 .. Cert_Len) = "silver" then
            Manifest.Cert := Cert_Silver;
         elsif Cert_Str (1 .. Cert_Len) = "gold" then
            Manifest.Cert := Cert_Gold;
         elsif Cert_Str (1 .. Cert_Len) = "platinum" then
            Manifest.Cert := Cert_Platinum;
         else
            Manifest.Cert := Cert_None;
         end if;
      end if;

      --  Extract gas_limit
      Extract_Integer (Params_Json, "gas_limit", Gas_Val, Found);
      if Found then
         Gas_Limit := Gas_Amount (Gas_Val);
      else
         Gas_Limit := 1_000_000;  -- Default
      end if;

      Success := True;
   end Parse_Deploy_Params;

   ---------------------------------------------------------------------------
   --  Invoke Request Parsing
   ---------------------------------------------------------------------------

   procedure Parse_Invoke_Params (
      Params_Json : in     String;
      Request     : out    Invoke_Request;
      Success     : out    Boolean
   ) is
      Entry_Str : String (1 .. Max_Entry_Name_Length);
      Entry_Len : Natural;
      To_Str    : String (1 .. 128);
      To_Len    : Natural;
      Args_Hex_Len : Natural;
      Gas_Val   : Natural;
      Found     : Boolean;
   begin
      Request := (
         From         => (others => 0),
         To           => (others => 0),
         Entry_Point  => (others => ' '),
         Entry_Len    => 0,
         Args         => (others => 0),
         Args_Size    => 0,
         Gas_Limit    => 0,
         Value        => (Limbs => (0, 0, 0, 0))
      );
      Success := False;

      --  Extract entry_point
      Extract_String (Params_Json, "entry_point", Entry_Str, Entry_Len, Found);
      if Found and Entry_Len > 0 then
         Request.Entry_Point (1 .. Entry_Len) := Entry_Str (1 .. Entry_Len);
         Request.Entry_Len := Entry_Len;
      end if;

      --  Extract to (contract address - AAS-001 or hex format)
      Extract_String (Params_Json, "to", To_Str, To_Len, Found);
      if Found and To_Len > 0 then
         --  Check if AAS-001 format (starts with "mldsa87:")
         if To_Len >= 78 and To_Str (1 .. 7) = "mldsa87" then
            --  AAS-001 format: mldsa87:net:t:chunked-checksum
            --  Minimum: mldsa87:dev:c:58chars-5chars = 78 chars
            declare
               --  Find payload start (after "mldsa87:net:t:")
               Payload_Start : Natural := 0;
               Colon_Count   : Natural := 0;
               Chunked       : Chunked_Payload;
               Payload_B32   : Payload_Base32;
               Account       : Account_ID;
               Unchunk_OK    : Boolean;
               Decode_OK     : Boolean;
            begin
               --  Find the third colon to locate payload
               for I in 1 .. To_Len loop
                  if To_Str (I) = ':' then
                     Colon_Count := Colon_Count + 1;
                     if Colon_Count = 3 then
                        Payload_Start := I + 1;
                        exit;
                     end if;
                  end if;
               end loop;

               --  Extract chunked payload (58 chars before the dash-checksum)
               if Payload_Start > 0 and Payload_Start + 57 <= To_Len then
                  for I in Chunked_Payload_Index loop
                     Chunked (I) := To_Str (Payload_Start + I);
                  end loop;

                  --  Unchunk to get raw Base32
                  Unchunk_Payload (Chunked, Payload_B32, Unchunk_OK);
                  if Unchunk_OK then
                     --  Decode Base32 to bytes
                     Decode_Account_ID (Payload_B32, Account, Decode_OK);
                     if Decode_OK then
                        --  Copy to Contract_Address
                        for I in Account_ID_Index loop
                           Request.To (I) := Byte (Account (I));
                        end loop;
                     end if;
                  end if;
               end if;
            end;
         elsif To_Len >= 64 then
            --  Legacy hex format: 64 hex chars = 32 bytes
            for I in 0 .. 31 loop
               declare
                  High : constant Natural := Hex_Char_To_Nibble (To_Str (I * 2 + 1));
                  Low  : constant Natural := Hex_Char_To_Nibble (To_Str (I * 2 + 2));
               begin
                  if High <= 15 and Low <= 15 then
                     Request.To (I) := Byte (High * 16 + Low);
                  end if;
               end;
            end loop;
         end if;
      end if;

      --  Extract args (hex-encoded bytes)
      Extract_String (Params_Json, "args", Args_Hex_Buffer, Args_Hex_Len, Found);
      if Found and Args_Hex_Len > 0 and Args_Hex_Len mod 2 = 0 then
         --  Parse hex to bytes
         declare
            Byte_Len : constant Natural := Args_Hex_Len / 2;
         begin
            if Byte_Len <= Max_Args_Size then
               for I in 0 .. Byte_Len - 1 loop
                  declare
                     High : constant Natural := Hex_Char_To_Nibble (Args_Hex_Buffer (I * 2 + 1));
                     Low  : constant Natural := Hex_Char_To_Nibble (Args_Hex_Buffer (I * 2 + 2));
                  begin
                     if High <= 15 and Low <= 15 then
                        Request.Args (Args_Index (I)) := Byte (High * 16 + Low);
                     end if;
                  end;
               end loop;
               Request.Args_Size := Byte_Len;
            end if;
         end;
      end if;

      --  Extract gas_limit
      Extract_Integer (Params_Json, "gas_limit", Gas_Val, Found);
      if Found then
         Request.Gas_Limit := Gas_Amount (Gas_Val);
      else
         Request.Gas_Limit := 200_000;  -- Default
      end if;

      Success := True;
   end Parse_Invoke_Params;

   ---------------------------------------------------------------------------
   --  Response Formatting
   ---------------------------------------------------------------------------

   procedure Format_Deploy_Result (
      Contract_ID : in     Contract_Address;
      Code_Hash   : in     Hash256;
      Gas_Used    : in     Gas_Amount;
      Result_Str  : out    String;
      Result_Len  : out    Natural
   ) is
      Hash_Hex  : String (1 .. 64);
      Hex_Len   : Natural;
      Pos       : Natural;

      --  AAS-001 address components
      Account     : Account_ID;
      Payload_B32 : Payload_Base32;
      Chunked     : Chunked_Payload;
      Checksum_Val : Checksum_Bytes;
      Checksum_B32 : Checksum_Base32;

      --  Full AAS-001 address: mldsa87:dev:c:payload-checksum
      --  mldsa87 (7) + : (1) + dev (3) + : (1) + c (1) + : (1) + chunked (58) + - (1) + checksum (5) = 78
      AAS_Addr : String (1 .. 78);
   begin
      Result_Str := (others => ' ');

      --  Convert Contract_Address (32 bytes) to Account_ID
      for I in Account_ID_Index loop
         Account (I) := Unsigned_8 (Contract_ID (I));
      end loop;

      --  Encode account ID to Base32 (52 chars)
      Encode_Account_ID (Account, Payload_B32);

      --  Compute checksum per AAS-001 v3.1 (dev network, contract type)
      Compute_Checksum (Dev, Contract, Payload_B32, Checksum_Val);

      --  Encode checksum to Base32 (5 chars)
      Encode_Checksum (Checksum_Val, Checksum_B32);

      --  Chunk payload for readability (8-8-8-8-8-8-4 with dashes = 58 chars)
      Chunk_Payload (Payload_B32, Chunked);

      --  Build AAS-001 address string: mldsa87:dev:c:chunked-checksum
      AAS_Addr := (others => ' ');
      AAS_Addr (1 .. 7) := "mldsa87";
      AAS_Addr (8) := ':';
      AAS_Addr (9 .. 11) := "dev";
      AAS_Addr (12) := ':';
      AAS_Addr (13) := 'c';
      AAS_Addr (14) := ':';
      --  Copy chunked payload (58 chars)
      for I in Chunked_Payload_Index loop
         AAS_Addr (15 + I) := Chunked (I);
      end loop;
      AAS_Addr (73) := '-';
      --  Copy checksum (5 chars)
      for I in Checksum_B32_Index loop
         AAS_Addr (74 + I) := Checksum_B32 (I);
      end loop;

      --  Also convert code hash to hex for reference
      Bytes_To_Hex (Code_Hash, Hash_Hex, Hex_Len);

      --  Build JSON response with AAS-001 contract_id
      declare
         Json : constant String :=
            "{""contract_id"":""" & AAS_Addr &
            """,""code_hash"":""" & Hash_Hex &
            """,""gas_used"":" & Gas_Amount'Image (Gas_Used) & "}";
      begin
         Pos := Natural'Min (Json'Length, Result_Str'Length);
         Result_Str (Result_Str'First .. Result_Str'First + Pos - 1) :=
            Json (Json'First .. Json'First + Pos - 1);
         Result_Len := Pos;
      end;
   end Format_Deploy_Result;

   procedure Format_Invoke_Result (
      Success_Flag : in     Boolean;
      Gas_Used     : in     Gas_Amount;
      Return_Data  : in     Return_Buffer;
      Return_Size  : in     Natural;
      Error_Msg    : in     String;
      Result_Str   : out    String;
      Result_Len   : out    Natural
   ) is
      Pos : Natural;
   begin
      Result_Str := (others => ' ');

      if Success_Flag then
         --  Convert return data to hex string
         --  First 32 bytes is a U256 in big-endian
         declare
            Ret_Hex : String (1 .. 64) := (others => '0');
            Actual_Size : constant Natural := Natural'Min (Return_Size, 32);
         begin
            for I in 0 .. Actual_Size - 1 loop
               Ret_Hex (I * 2 + 1) := Nibble_To_Hex_Char (
                  Natural (Return_Data (Return_Index (I))) / 16);
               Ret_Hex (I * 2 + 2) := Nibble_To_Hex_Char (
                  Natural (Return_Data (Return_Index (I))) mod 16);
            end loop;

            declare
               Json : constant String :=
                  "{""success"":true,""gas_used"":" & Gas_Amount'Image (Gas_Used) &
                  ",""return_value"":""0x" & Ret_Hex & """}";
            begin
               Pos := Natural'Min (Json'Length, Result_Str'Length);
               Result_Str (Result_Str'First .. Result_Str'First + Pos - 1) :=
                  Json (Json'First .. Json'First + Pos - 1);
               Result_Len := Pos;
            end;
         end;
      else
         declare
            Json : constant String :=
               "{""success"":false,""error"":""" & Error_Msg & """}";
         begin
            Pos := Natural'Min (Json'Length, Result_Str'Length);
            Result_Str (Result_Str'First .. Result_Str'First + Pos - 1) :=
               Json (Json'First .. Json'First + Pos - 1);
            Result_Len := Pos;
         end;
      end if;
   end Format_Invoke_Result;

end Node_JSON_Parser;
