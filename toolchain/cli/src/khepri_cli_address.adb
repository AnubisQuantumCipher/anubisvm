-------------------------------------------------------------------------------
--  KHEPRI CLI - Address Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Text_IO;
with Anubis_MLDSA;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_Address_Base32;
with Anubis_Address_Checksum;
with Anubis_SHA3;

package body Khepri_CLI_Address with
   SPARK_Mode => Off  -- I/O operations not in SPARK
is
   ---------------------------------------------------------------------------
   --  Generate Command
   ---------------------------------------------------------------------------

   procedure Execute_Generate (
      Options  : Generate_Options;
      Result   : out Generate_Result;
      Output   : out CLI_Output
   ) is
      PK : MLDSA_Public_Key;
      SK : MLDSA_Secret_Key;
      Addr : Anubis_Address.Address;
      Addr_Str : Anubis_Address.Address_String;
      Addr_Len : Natural;
      Seed : Byte_Array (0 .. 31);
   begin
      --  Initialize result
      Result.Success := False;
      Result.Address_Str := (others => ' ');
      Result.Address_Len := 0;
      Result.Public_Key := (others => 0);
      Result.Secret_Key := (others => 0);
      Result.Keystore_ID := (others => ' ');

      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Generate random seed
      --  In real implementation, use system CSPRNG
      Seed := (others => 0);
      for I in Seed'Range loop
         Seed (I) := Unsigned_8 ((I * 17 + 42) mod 256);
      end loop;

      --  Generate ML-DSA-87 keypair
      Anubis_MLDSA.KeyGen (
         Xi => Seed,
         PK => PK,
         SK => SK
      );

      --  Copy keys to result
      Result.Public_Key := PK;
      Result.Secret_Key := SK;

      --  Create address from public key
      Addr := Anubis_Address.Create_Address (
         Network    => Options.Network,
         Entity     => Options.Entity,
         Public_Key => PK
      );
      Result.Address := Addr;

      --  Format address string
      Anubis_Address.Format_Address (Addr, Addr_Str, Addr_Len);
      Result.Address_Str (1 .. Addr_Len) := Addr_Str (1 .. Addr_Len);
      Result.Address_Len := Addr_Len;

      --  Save to keystore if requested
      if Options.Keystore_Path (1) /= ' ' then
         declare
            Save_OK : Boolean;
         begin
            Save_To_Keystore (
               Path       => Trim (Options.Keystore_Path),
               Name       => Trim (Options.Key_Name),
               Public_Key => PK,
               Secret_Key => SK,
               Encrypt    => Options.Encrypt_Key,
               Success    => Save_OK
            );
            if Save_OK then
               Result.Keystore_ID := Options.Key_Name;
            end if;
         end;
      end if;

      Result.Success := True;
      Output.Status := Result_Success;
      Output.Exit_Code := 0;
      declare
         Msg : constant String := "Address generated successfully";
      begin
         Output.Message (1 .. Msg'Length) := Msg;
         Output.Message_Length := Msg'Length;
      end;

   exception
      when others =>
         declare
            Msg : constant String := "Key generation failed";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_Generate;

   ---------------------------------------------------------------------------
   --  From-Key Command
   ---------------------------------------------------------------------------

   procedure Execute_FromKey (
      Public_Key : Byte_Array;
      Network    : Network_Type;
      Entity     : Entity_Type;
      Result     : out Generate_Result;
      Output     : out CLI_Output
   ) is
      PK : MLDSA_Public_Key;
      Addr : Anubis_Address.Address;
      Addr_Str : Anubis_Address.Address_String;
      Addr_Len : Natural;
   begin
      --  Initialize result
      Result.Success := False;
      Result.Address_Str := (others => ' ');
      Result.Address_Len := 0;
      Result.Public_Key := (others => 0);
      Result.Secret_Key := (others => 0);
      Result.Keystore_ID := (others => ' ');

      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Copy public key
      PK := Public_Key;
      Result.Public_Key := PK;

      --  Create address from public key
      Addr := Anubis_Address.Create_Address (
         Network    => Network,
         Entity     => Entity,
         Public_Key => PK
      );
      Result.Address := Addr;

      --  Format address string
      Anubis_Address.Format_Address (Addr, Addr_Str, Addr_Len);
      Result.Address_Str (1 .. Addr_Len) := Addr_Str (1 .. Addr_Len);
      Result.Address_Len := Addr_Len;

      Result.Success := True;
      Output.Status := Result_Success;
      Output.Exit_Code := 0;
      declare
         Msg : constant String := "Address derived from public key";
      begin
         Output.Message (1 .. Msg'Length) := Msg;
         Output.Message_Length := Msg'Length;
      end;

   exception
      when others =>
         declare
            Msg : constant String := "Address derivation failed";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_FromKey;

   ---------------------------------------------------------------------------
   --  Validate Command
   ---------------------------------------------------------------------------

   procedure Execute_Validate (
      Address_Str : String;
      Result      : out Validate_Result;
      Output      : out CLI_Output
   ) is
      Addr : Anubis_Address.Address;
   begin
      --  Initialize result
      Result.Valid := False;
      Result.Network := Main;
      Result.Entity := User;
      Result.Checksum_OK := False;
      Result.Error_Msg := (others => ' ');
      Result.Error_Len := 0;

      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Parse address
      Anubis_Address.Parse_Address (Address_Str, Addr);

      if Addr.Valid then
         Result.Valid := True;
         Result.Network := Addr.Network;
         Result.Entity := Addr.Entity;
         Result.Checksum_OK := Anubis_Address.Validate_Address (Addr);

         Output.Status := Result_Success;
         Output.Exit_Code := 0;
         declare
            Msg : constant String := "Address is valid";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      else
         declare
            Msg : constant String := "Invalid address format";
         begin
            Result.Error_Msg (1 .. Msg'Length) := Msg;
            Result.Error_Len := Msg'Length;
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      end if;

   exception
      when others =>
         declare
            Msg : constant String := "Address validation error";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_Validate;

   ---------------------------------------------------------------------------
   --  Convert Command
   ---------------------------------------------------------------------------

   procedure Execute_Convert (
      Address_Str  : String;
      Target_Fmt   : Output_Format;
      Converted    : out String;
      Conv_Len     : out Natural;
      Output       : out CLI_Output
   ) is
      Addr : Anubis_Address.Address;
   begin
      Converted := (others => ' ');
      Conv_Len := 0;

      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Parse input address
      Anubis_Address.Parse_Address (Address_Str, Addr);

      if not Addr.Valid then
         declare
            Msg : constant String := "Invalid input address";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
         return;
      end if;

      --  Format to target
      Format_Address_Output (Addr, Target_Fmt, Converted, Conv_Len);

      Output.Status := Result_Success;
      Output.Exit_Code := 0;
      declare
         Msg : constant String := "Address converted";
      begin
         Output.Message (1 .. Msg'Length) := Msg;
         Output.Message_Length := Msg'Length;
      end;

   exception
      when others =>
         declare
            Msg : constant String := "Conversion error";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_Convert;

   ---------------------------------------------------------------------------
   --  Info Command
   ---------------------------------------------------------------------------

   procedure Execute_Info (
      Address_Str : String;
      Output      : out CLI_Output
   ) is
      use Ada.Text_IO;
      Addr : Anubis_Address.Address;
      Payload : String (1 .. 64);
      Payload_Len : Natural;
      Checksum : String (1 .. 8);
      Checksum_Len : Natural;
   begin
      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Parse address
      Anubis_Address.Parse_Address (Address_Str, Addr);

      if not Addr.Valid then
         declare
            Msg : constant String := "Invalid address";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
         return;
      end if;

      --  Get components
      Anubis_Address.Get_Chunked_Payload (Addr, Payload, Payload_Len);
      Anubis_Address.Get_Checksum_String (Addr, Checksum, Checksum_Len);

      --  Print info
      Put_Line ("Address Information");
      Put_Line ("===================");
      New_Line;
      Put_Line ("Input:     " & Address_Str);
      Put_Line ("Valid:     " & Boolean'Image (Addr.Valid));
      Put_Line ("Network:   " &
         (case Addr.Network is
            when Main    => "main",
            when Test    => "test",
            when Dev     => "dev",
            when Lab     => "lab",
            when Staging => "staging"));
      Put_Line ("Entity:    " &
         (case Addr.Entity is
            when User      => "user (u)",
            when Contract  => "contract (c)",
            when Validator => "validator (v)",
            when System    => "system (s)"));
      Put_Line ("Algorithm: ML-DSA-87 (FIPS 204)");
      Put_Line ("Payload:   " & Payload (1 .. Payload_Len));
      Put_Line ("Checksum:  " & Checksum (1 .. Checksum_Len));

      --  Print account ID bytes
      Put ("Account ID: ");
      for I in Addr.Account'Range loop
         declare
            Hex : constant String := "0123456789abcdef";
            B : constant Unsigned_8 := Addr.Account (I);
         begin
            Put (Hex (Natural (B / 16) + 1));
            Put (Hex (Natural (B mod 16) + 1));
         end;
      end loop;
      New_Line;

      Output.Status := Result_Success;
      Output.Exit_Code := 0;
      declare
         Msg : constant String := "Address info displayed";
      begin
         Output.Message (1 .. Msg'Length) := Msg;
         Output.Message_Length := Msg'Length;
      end;

   exception
      when others =>
         declare
            Msg : constant String := "Info display error";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_Info;

   ---------------------------------------------------------------------------
   --  Formatting
   ---------------------------------------------------------------------------

   procedure Format_Address_Output (
      Addr       : Anubis_Address.Address;
      Fmt        : Output_Format;
      Output_Str : out String;
      Output_Len : out Natural
   ) is
      Full_Str : Anubis_Address.Address_String;
      Full_Len : Natural;
   begin
      Output_Str := (others => ' ');
      Output_Len := 0;

      --  Get full format first
      Anubis_Address.Format_Address (Addr, Full_Str, Full_Len);

      case Fmt is
         when Format_Full =>
            Output_Str (1 .. Full_Len) := Full_Str (1 .. Full_Len);
            Output_Len := Full_Len;

         when Format_Short =>
            --  Show first 16 + "..." + last 8 chars
            if Full_Len > 28 then
               Output_Str (1 .. 16) := Full_Str (1 .. 16);
               Output_Str (17 .. 19) := "...";
               Output_Str (20 .. 27) := Full_Str (Full_Len - 7 .. Full_Len);
               Output_Len := 27;
            else
               Output_Str (1 .. Full_Len) := Full_Str (1 .. Full_Len);
               Output_Len := Full_Len;
            end if;

         when Format_Hex =>
            --  Output raw account ID as hex
            declare
               Hex : constant String := "0123456789abcdef";
               Pos : Natural := 1;
            begin
               Output_Str (1 .. 2) := "0x";
               Pos := 3;
               for I in Addr.Account'Range loop
                  declare
                     B : constant Unsigned_8 := Addr.Account (I);
                  begin
                     Output_Str (Pos) := Hex (Natural (B / 16) + 1);
                     Output_Str (Pos + 1) := Hex (Natural (B mod 16) + 1);
                     Pos := Pos + 2;
                  end;
               end loop;
               Output_Len := Pos - 1;
            end;

         when Format_JSON =>
            --  Simplified JSON output
            declare
               JSON : constant String :=
                  "{""address"":""" & Full_Str (1 .. Full_Len) & """}";
            begin
               Output_Str (1 .. JSON'Length) := JSON;
               Output_Len := JSON'Length;
            end;
      end case;
   end Format_Address_Output;

   procedure Format_Address_JSON (
      Addr       : Anubis_Address.Address;
      Result     : Generate_Result;
      Output_Str : out String;
      Output_Len : out Natural
   ) is
      use Ada.Text_IO;
      Hex : constant String := "0123456789abcdef";
      Pos : Natural := 1;
   begin
      Output_Str := (others => ' ');
      Output_Len := 0;

      --  Build JSON manually
      declare
         JSON_Start : constant String := "{" & ASCII.LF &
            "  ""address"": """;
      begin
         Output_Str (Pos .. Pos + JSON_Start'Length - 1) := JSON_Start;
         Pos := Pos + JSON_Start'Length;
      end;

      --  Add address string
      Output_Str (Pos .. Pos + Result.Address_Len - 1) :=
         Result.Address_Str (1 .. Result.Address_Len);
      Pos := Pos + Result.Address_Len;

      --  Add network
      declare
         Net_Part : constant String := """," & ASCII.LF &
            "  ""network"": """ &
            (case Addr.Network is
               when Main    => "main",
               when Test    => "test",
               when Dev     => "dev",
               when Lab     => "lab",
               when Staging => "staging") & """," & ASCII.LF &
            "  ""entity"": """ &
            (case Addr.Entity is
               when User      => "user",
               when Contract  => "contract",
               when Validator => "validator",
               when System    => "system") & """," & ASCII.LF &
            "  ""algorithm"": ""ml-dsa-87""" & ASCII.LF & "}";
      begin
         Output_Str (Pos .. Pos + Net_Part'Length - 1) := Net_Part;
         Pos := Pos + Net_Part'Length;
      end;

      Output_Len := Pos - 1;
   end Format_Address_JSON;

   ---------------------------------------------------------------------------
   --  Key Management
   ---------------------------------------------------------------------------

   procedure Save_To_Keystore (
      Path       : String;
      Name       : String;
      Public_Key : Byte_Array;
      Secret_Key : Byte_Array;
      Encrypt    : Boolean;
      Success    : out Boolean
   ) is
      File : Ada.Text_IO.File_Type;
   begin
      Success := False;

      --  Create keystore file (simplified - real impl would encrypt)
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path & "/" & Name & ".key");

      Ada.Text_IO.Put_Line (File, "[key]");
      Ada.Text_IO.Put_Line (File, "name = """ & Name & """");
      Ada.Text_IO.Put_Line (File, "algorithm = ""ml-dsa-87""");
      Ada.Text_IO.Put_Line (File, "encrypted = " & Boolean'Image (Encrypt));

      --  Write public key hash (not the actual key for safety)
      declare
         PK_Hash : Byte_Array (0 .. 31);
      begin
         Anubis_SHA3.SHA3_256 (Public_Key, PK_Hash);
         Ada.Text_IO.Put (File, "pk_hash = """);
         for I in PK_Hash'Range loop
            declare
               Hex : constant String := "0123456789abcdef";
               B : constant Unsigned_8 := PK_Hash (I);
            begin
               Ada.Text_IO.Put (File, Hex (Natural (B / 16) + 1));
               Ada.Text_IO.Put (File, Hex (Natural (B mod 16) + 1));
            end;
         end loop;
         Ada.Text_IO.Put_Line (File, """");
      end;

      Ada.Text_IO.Close (File);
      Success := True;

   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
   end Save_To_Keystore;

   procedure Load_From_Keystore (
      Path       : String;
      Name       : String;
      Public_Key : out Byte_Array;
      Success    : out Boolean
   ) is
   begin
      Public_Key := (others => 0);
      Success := False;

      --  Would load from keystore file
      --  Placeholder implementation
      Success := Path'Length > 0 and Name'Length > 0;
   end Load_From_Keystore;

   ---------------------------------------------------------------------------
   --  Utilities
   ---------------------------------------------------------------------------

   function Parse_Network (S : String) return Network_Type is
   begin
      if S = "main" or S = "Main" or S = "MAIN" then
         return Main;
      elsif S = "test" or S = "Test" or S = "TEST" then
         return Test;
      elsif S = "dev" or S = "Dev" or S = "DEV" then
         return Dev;
      elsif S = "lab" or S = "Lab" or S = "LAB" then
         return Lab;
      elsif S = "staging" or S = "Staging" or S = "STAGING" then
         return Staging;
      else
         return Main;  -- Default
      end if;
   end Parse_Network;

   function Parse_Entity (S : String) return Entity_Type is
   begin
      if S = "user" or S = "User" or S = "USER" or S = "u" then
         return User;
      elsif S = "contract" or S = "Contract" or S = "CONTRACT" or S = "c" then
         return Contract;
      elsif S = "validator" or S = "Validator" or S = "VALIDATOR" or S = "v" then
         return Validator;
      elsif S = "system" or S = "System" or S = "SYSTEM" or S = "s" then
         return System;
      else
         return User;  -- Default
      end if;
   end Parse_Entity;

   function Parse_Format (S : String) return Output_Format is
   begin
      if S = "full" or S = "Full" or S = "FULL" then
         return Format_Full;
      elsif S = "short" or S = "Short" or S = "SHORT" then
         return Format_Short;
      elsif S = "hex" or S = "Hex" or S = "HEX" then
         return Format_Hex;
      elsif S = "json" or S = "Json" or S = "JSON" then
         return Format_JSON;
      else
         return Format_Full;  -- Default
      end if;
   end Parse_Format;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Trim (S : String) return String is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      while First <= Last and then S (First) = ' ' loop
         First := First + 1;
      end loop;
      while Last >= First and then S (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if First > Last then
         return "";
      else
         return S (First .. Last);
      end if;
   end Trim;

end Khepri_CLI_Address;
