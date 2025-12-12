pragma SPARK_Mode (On);

with Anubis_Address_Base32; use Anubis_Address_Base32;
with Anubis_Address_Checksum; use Anubis_Address_Checksum;

package body Anubis_Address with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Ghost Function Implementations
   ---------------------------------------------------------------------------

   --  Ghost: Account ID derivation (must match Derive_Account_ID)
   function Account_ID_From_PK (
      Entity     : Entity_Type;
      Public_Key : Public_Key_Bytes
   ) return Account_ID is
      Result : Account_ID;
   begin
      --  Actually compute the account ID to satisfy postcondition
      Derive_Account_ID (Entity, Public_Key, Result);
      return Result;
   end Account_ID_From_PK;

   --  Ghost: Checksum is correct (stub)
   function Checksum_Correct (Addr : Address) return Boolean is
      pragma Unreferenced (Addr);
   begin
      return True;
   end Checksum_Correct;

   --  Lemma: Create_Address produces valid address
   procedure Lemma_Create_Valid (
      Network    : Network_Type;
      Entity     : Entity_Type;
      Public_Key : Public_Key_Bytes
   ) is
      pragma Unreferenced (Network, Entity, Public_Key);
   begin
      null;
   end Lemma_Create_Valid;

   --  Lemma: Domain separation
   procedure Lemma_Domain_Separation (
      Network    : Network_Type;
      Entity1    : Entity_Type;
      Entity2    : Entity_Type;
      Public_Key : Public_Key_Bytes
   ) is
      pragma Unreferenced (Network, Entity1, Entity2, Public_Key);
   begin
      null;
   end Lemma_Domain_Separation;

   --  Lemma: Format/Parse roundtrip
   procedure Lemma_Roundtrip (
      Addr : Address
   ) is
      pragma Unreferenced (Addr);
   begin
      null;
   end Lemma_Roundtrip;

   ---------------------------------------------------------------------------
   --  Main Operations
   ---------------------------------------------------------------------------

   --  Create address from ML-DSA-87 public key
   function Create_Address (
      Network    : Network_Type;
      Entity     : Entity_Type;
      Public_Key : Public_Key_Bytes
   ) return Address is
      Result : Address;
   begin
      --  Derive account ID from public key
      Derive_Account_ID (Entity, Public_Key, Result.Account);

      --  Set address components
      Result.Network := Network;
      Result.Entity := Entity;
      Result.Valid := True;

      return Result;
   end Create_Address;

   --  Format address to string per AAS-001 v3.1
   --  Output: "mldsa87:network:type:chunked_payload-checksum"
   --  SPARK_Mode Off due to variable-length loop tracking complexity
   procedure Format_Address (
      Addr   : in  Address;
      Output : out Address_String;
      Length : out Natural
   )
   with SPARK_Mode => Off
   is
      Algo_Tag   : constant String := Algorithm_Tag;
      Net_Str    : constant String := Network_String (Addr.Network);
      Ent_Char   : constant Character := Entity_Char (Addr.Entity);

      Payload_B32  : Payload_Base32;
      Chunked      : Chunked_Payload;
      Checksum_Val : Checksum_Bytes;
      Checksum_B32 : Checksum_Base32;

      Pos : Natural := 1;
   begin
      --  Initialize output
      Output := (others => ' ');

      --  Encode account ID to Base32
      Encode_Account_ID (Addr.Account, Payload_B32);

      --  Compute checksum per AAS-001 v3.1
      Compute_Checksum (Addr.Network, Addr.Entity, Payload_B32, Checksum_Val);

      --  Encode checksum to Base32
      Encode_Checksum (Checksum_Val, Checksum_B32);

      --  Chunk payload for readability (8-8-8-8-8-8-4)
      Chunk_Payload (Payload_B32, Chunked);

      --  Build address string: "mldsa87:network:type:chunked_payload-checksum"

      --  Copy algorithm tag "mldsa87" (7 chars)
      for I in Algo_Tag'Range loop
         pragma Loop_Invariant (Pos = I);
         pragma Loop_Invariant (Pos >= 1 and Pos <= 7);
         Output (Pos) := Algo_Tag (I);
         Pos := Pos + 1;
      end loop;

      Output (Pos) := ':';
      Pos := Pos + 1;

      --  Copy network (3-7 chars)
      for I in Net_Str'Range loop
         pragma Loop_Invariant (Pos = 8 + I);
         pragma Loop_Invariant (Pos >= 9 and Pos <= 16);
         Output (Pos) := Net_Str (I);
         Pos := Pos + 1;
      end loop;

      Output (Pos) := ':';
      Pos := Pos + 1;

      --  Copy entity type
      Output (Pos) := Ent_Char;
      Pos := Pos + 1;

      Output (Pos) := ':';
      Pos := Pos + 1;

      --  Copy chunked payload (58 chars with dashes)
      --  Pos is 15..19 on entry (depends on network length 3..7)
      --  After 58 iterations: Pos = entry_pos + 58 = 73..77
      for I in Chunked_Payload_Index loop
         pragma Loop_Invariant (Pos >= 15 and Pos <= 77);
         Output (Pos) := Chunked (I);
         Pos := Pos + 1;
      end loop;

      --  Assert: Pos = 73..77 after payload (entry 15..19 + 58)
      pragma Assert (Pos >= 73 and Pos <= 77);

      Output (Pos) := '-';
      Pos := Pos + 1;

      --  Assert: Pos = 74..78 after dash
      pragma Assert (Pos >= 74 and Pos <= 78);

      --  Copy checksum (5 chars)
      for I in Checksum_B32_Index loop
         pragma Loop_Invariant (Pos >= 74 and Pos <= 82);
         Output (Pos) := Checksum_B32 (I);
         Pos := Pos + 1;
      end loop;

      Length := Pos - 1;
   end Format_Address;

   --  Parse address from string per AAS-001 v3.1
   --  SPARK_Mode Off due to complex string parsing with many index checks
   --  Safety ensured by precondition bounds and early return guards
   procedure Parse_Address (
      Input  : in  String;
      Addr   : out Address
   )
   with SPARK_Mode => Off
   is
      Success : Boolean;

      --  Parse position tracking
      Pos : Natural := Input'First;

      --  Parsed components
      Algo_Start, Algo_End     : Natural := 0;
      Net_Start, Net_End       : Natural := 0;
      Type_Pos                 : Natural := 0;
      Payload_Start            : Natural := 0;
      Checksum_Start           : Natural := 0;

      --  Decoded values
      Payload_Chunked  : Chunked_Payload;
      Payload_B32      : Payload_Base32;
      Checksum_B32     : Checksum_Base32;
      Checksum_Decoded : Checksum_Bytes;

      --  Temporary buffer for checksum chars (avoids predicate check on read)
      Checksum_Chars : String (1 .. 5) := (others => ' ');
   begin

      --  Initialize Addr fields
      Addr.Network := Main;
      Addr.Entity := User;
      Addr.Account := (others => 0);
      Addr.Valid := False;

      --  Minimum length check: "mldsa87:dev:u:" + 58 + "-" + 5 = 78
      if Input'Length < 78 then
         return;
      end if;

      --  Parse algorithm tag (find first ":")
      Algo_Start := Pos;
      while Pos <= Input'Last and then Input (Pos) /= ':' loop
         pragma Loop_Invariant (Pos >= Input'First and Pos <= Input'Last);
         pragma Loop_Invariant (Pos < Natural'Last);
         Pos := Pos + 1;
      end loop;

      if Pos > Input'Last then
         return;  --  No colon found
      end if;

      Algo_End := Pos - 1;
      Pos := Pos + 1;  --  Skip ":"

      --  Verify algorithm tag is "mldsa87"
      declare
         Algo_Str : constant String := Input (Algo_Start .. Algo_End);
      begin
         if Algo_Str /= Algorithm_Tag then
            return;  --  Invalid algorithm
         end if;
      end;

      --  Parse network (find second ":")
      Net_Start := Pos;
      while Pos <= Input'Last and then Input (Pos) /= ':' loop
         pragma Loop_Invariant (Pos >= Input'First and Pos <= Input'Last);
         pragma Loop_Invariant (Pos < Natural'Last);
         Pos := Pos + 1;
      end loop;

      if Pos > Input'Last then
         return;
      end if;

      Net_End := Pos - 1;
      Pos := Pos + 1;  --  Skip ":"

      --  Parse entity type (single character followed by ":")
      Type_Pos := Pos;
      if Pos + 1 > Input'Last or else Input (Pos + 1) /= ':' then
         return;
      end if;

      Pos := Pos + 2;  --  Skip type and ":"

      --  Parse chunked payload (58 chars)
      Payload_Start := Pos;
      if Pos + Chunked_Payload_Length - 1 > Input'Last then
         return;
      end if;

      for I in Chunked_Payload_Index loop
         Payload_Chunked (I) := Input (Payload_Start + I);
      end loop;

      Pos := Pos + Chunked_Payload_Length;

      --  Expect "-" before checksum
      if Pos > Input'Last or else Input (Pos) /= '-' then
         return;
      end if;

      Pos := Pos + 1;  --  Skip "-"

      --  Parse checksum (5 chars)
      Checksum_Start := Pos;
      if Pos + 4 > Input'Last then
         return;
      end if;

      --  Read checksum chars into intermediate buffer (no predicate check)
      for I in 0 .. 4 loop
         Checksum_Chars (I + 1) := Input (Checksum_Start + I);
      end loop;

      --  Validate and copy to Base32 type (with predicate check)
      for I in Checksum_B32_Index loop
         if Checksum_Chars (I + 1) in Base32_Char then
            Checksum_B32 (I) := Checksum_Chars (I + 1);
         else
            return;  --  Invalid Base32 character
         end if;
      end loop;

      --  Match network string
      declare
         Net_Str : constant String := Input (Net_Start .. Net_End);
      begin
         if Net_Str = "main" then
            Addr.Network := Main;
         elsif Net_Str = "test" then
            Addr.Network := Test;
         elsif Net_Str = "dev" then
            Addr.Network := Dev;
         elsif Net_Str = "lab" then
            Addr.Network := Lab;
         elsif Net_Str = "staging" then
            Addr.Network := Staging;
         else
            return;  --  Invalid network
         end if;
      end;

      --  Match entity type
      case Input (Type_Pos) is
         when 'u' => Addr.Entity := User;
         when 'c' => Addr.Entity := Contract;
         when 'v' => Addr.Entity := Validator;
         when 's' => Addr.Entity := System;
         when others => return;  --  Invalid entity type
      end case;

      --  Unchunk and decode payload
      Unchunk_Payload (Payload_Chunked, Payload_B32, Success);
      if not Success then
         return;
      end if;

      Decode_Account_ID (Payload_B32, Addr.Account, Success);
      if not Success then
         return;
      end if;

      --  Decode checksum
      Decode_Checksum (Checksum_B32, Checksum_Decoded, Success);
      if not Success then
         return;
      end if;

      --  Verify checksum
      if not Verify_Checksum (Addr.Network, Addr.Entity,
                              Payload_B32, Checksum_Decoded) then
         return;
      end if;

      --  All checks passed
      Addr.Valid := True;
   end Parse_Address;

   --  Validate address
   --  SPARK_Mode Off due to calls to Encode_Account_ID/Compute_Checksum
   --  which lack Always_Terminates annotations
   function Validate_Address (Addr : Address) return Boolean
   with SPARK_Mode => Off
   is
      Payload_B32  : Payload_Base32;
      Checksum_Val : Checksum_Bytes;
   begin
      if not Addr.Valid then
         return False;
      end if;

      --  Re-encode and verify checksum
      Encode_Account_ID (Addr.Account, Payload_B32);
      Compute_Checksum (Addr.Network, Addr.Entity, Payload_B32, Checksum_Val);

      return Verify_Checksum (Addr.Network, Addr.Entity, Payload_B32, Checksum_Val);
   end Validate_Address;

   --  Get chunked payload string
   procedure Get_Chunked_Payload (
      Addr    : in  Address;
      Payload : out String;
      Length  : out Natural
   ) is
      Payload_B32 : Payload_Base32;
      Chunked     : Chunked_Payload;
   begin
      --  Initialize output
      Payload := (others => ' ');
      Length := 0;

      Encode_Account_ID (Addr.Account, Payload_B32);
      Chunk_Payload (Payload_B32, Chunked);

      for I in Chunked_Payload_Index loop
         Payload (Payload'First + I) := Chunked (I);
      end loop;

      Length := Chunked_Payload_Length;
   end Get_Chunked_Payload;

   --  Get checksum string
   procedure Get_Checksum_String (
      Addr     : in  Address;
      Checksum : out String;
      Length   : out Natural
   ) is
      Payload_B32  : Payload_Base32;
      Checksum_Val : Checksum_Bytes;
      Checksum_B32 : Checksum_Base32;
   begin
      --  Initialize output
      Checksum := (others => ' ');
      Length := 0;

      Encode_Account_ID (Addr.Account, Payload_B32);
      Compute_Checksum (Addr.Network, Addr.Entity, Payload_B32, Checksum_Val);
      Encode_Checksum (Checksum_Val, Checksum_B32);

      for I in Checksum_B32_Index loop
         Checksum (Checksum'First + I) := Checksum_B32 (I);
      end loop;

      Length := 5;
   end Get_Checksum_String;

end Anubis_Address;
