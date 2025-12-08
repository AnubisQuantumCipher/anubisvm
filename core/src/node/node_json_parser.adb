pragma SPARK_Mode (On);

package body Node_JSON_Parser with
   SPARK_Mode => On
is

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
      Bytes := (others => 0);
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
      Value := (others => ' ');
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
      Hex_Str   : String (1 .. Max_Hex_Length);
      Hex_Len   : Natural;
      Name_Str  : String (1 .. Max_Contract_Name_Length);
      Name_Len  : Natural;
      Cert_Str  : String (1 .. 16);
      Cert_Len  : Natural;
      Gas_Val   : Natural;
      Found     : Boolean;
   begin
      From_Addr := (others => 0);
      Code := (others => 0);
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

      --  Extract elf_hex
      Extract_String (Params_Json, "elf_hex", Hex_Str, Hex_Len, Found);
      if not Found or Hex_Len = 0 then
         return;
      end if;

      --  Convert hex to bytes
      Hex_To_Bytes (Hex_Str (1 .. Hex_Len), Code, Code_Size, Found);
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

      --  Extract to (contract address as hex)
      Extract_String (Params_Json, "to", To_Str, To_Len, Found);
      if Found and To_Len >= 64 then
         --  Parse hex to address (first 32 bytes)
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
      ID_Hex    : String (1 .. 64);
      Hash_Hex  : String (1 .. 64);
      Hex_Len   : Natural;
      Pos       : Natural;
   begin
      Result_Str := (others => ' ');

      --  Convert contract ID to hex
      Bytes_To_Hex (Hash256 (Contract_ID), ID_Hex, Hex_Len);
      Bytes_To_Hex (Code_Hash, Hash_Hex, Hex_Len);

      --  Build JSON response
      declare
         Json : constant String :=
            "{""contract_id"":""" & ID_Hex &
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
      pragma Unreferenced (Return_Data, Return_Size);
      Pos : Natural;
   begin
      Result_Str := (others => ' ');

      if Success_Flag then
         declare
            Json : constant String :=
               "{""success"":true,""gas_used"":" & Gas_Amount'Image (Gas_Used) &
               ",""return_value"":""0""}";
         begin
            Pos := Natural'Min (Json'Length, Result_Str'Length);
            Result_Str (Result_Str'First .. Result_Str'First + Pos - 1) :=
               Json (Json'First .. Json'First + Pos - 1);
            Result_Len := Pos;
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
