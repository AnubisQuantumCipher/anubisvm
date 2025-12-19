-------------------------------------------------------------------------------
--  KHEPRI CLI - Verify Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  Network and file I/O

with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Anubis_SHA3;
with Khepri_RPC_Client;

package body Khepri_CLI_Verify is

   use Ada.Text_IO;
   use Anubis_SHA3;
   use Khepri_RPC_Client;

   ---------------------------------------------------------------------------
   --  Bytecode Fetching
   ---------------------------------------------------------------------------

   procedure Fetch_Deployed_Code (
      RPC_URL  : String;
      Address  : Khepri_Types.Address;
      Code     : out Byte_Array;
      Size     : out Natural;
      Success  : out Boolean
   ) is
      Endpoint : RPC_Endpoint;
      RPC_Err  : RPC_Error;
   begin
      Code := (others => 0);
      Size := 0;
      Success := False;

      --  Parse endpoint
      Endpoint := Parse_Endpoint (RPC_URL);

      --  Fetch code via RPC
      Get_Code (
         Endpoint      => Endpoint,
         Contract_Addr => Address,
         Code          => Code,
         Code_Size     => Size,
         Error         => RPC_Err,
         Success       => Success
      );

      if not Success then
         Put_Line ("RPC Error: " & RPC_Error_Code'Image (RPC_Err.Code));
      end if;
   end Fetch_Deployed_Code;

   ---------------------------------------------------------------------------
   --  Bytecode Comparison
   ---------------------------------------------------------------------------

   function Compare_Bytecode (
      Deployed : Byte_Array;
      Expected : Byte_Array
   ) return Boolean is
      Deployed_Hash : SHA3_256_Digest;
      Expected_Hash : SHA3_256_Digest;
   begin
      --  Compute hashes
      SHA3_256 (Deployed, Deployed_Hash);
      SHA3_256 (Expected, Expected_Hash);

      --  Compare
      for I in Deployed_Hash'Range loop
         if Deployed_Hash (I) /= Expected_Hash (I) then
            return False;
         end if;
      end loop;

      return True;
   end Compare_Bytecode;

   ---------------------------------------------------------------------------
   --  Manifest Verification
   ---------------------------------------------------------------------------

   procedure Verify_Manifest_File (
      Manifest_Path : String;
      Valid         : out Boolean;
      Cert_Level    : out String;
      Error         : out String
   ) is
      use Ada.Streams.Stream_IO;

      File   : File_Type;
      Stream : Stream_Access;
      Magic  : Byte_Array (0 .. 18);  --  "KHEPRI-MANIFEST-V1"
   begin
      Valid := False;
      Cert_Level := (others => ' ');
      Error := (others => ' ');

      begin
         --  Open manifest file
         Open (File, In_File, Manifest_Path);
         Stream := Stream_IO.Stream (File);

         --  Read magic header
         for I in Magic'Range loop
            Byte'Read (Stream, Magic (I));
         end loop;

         --  Verify magic
         declare
            Expected_Magic : constant String := "KHEPRI-MANIFEST-V1";
         begin
            for I in Expected_Magic'Range loop
               if Magic (I - Expected_Magic'First) /=
                  Byte (Character'Pos (Expected_Magic (I)))
               then
                  Error (1 .. 21) := "Invalid manifest magic";
                  Close (File);
                  return;
               end if;
            end loop;
         end;

         --  Read null terminator
         declare
            Null_Byte : Byte;
         begin
            Byte'Read (Stream, Null_Byte);
         end;

         --  Read metadata section
         declare
            Section_ID : Byte;
         begin
            Byte'Read (Stream, Section_ID);
            if Section_ID /= 16#01# then
               Error (1 .. 29) := "Invalid metadata section ID";
               Close (File);
               return;
            end if;
         end;

         --  Skip to certification level (after source/function counts/bytecode size)
         declare
            Skip_Bytes : Byte_Array (0 .. 29);  --  Skip version string + counts
         begin
            for I in Skip_Bytes'Range loop
               Byte'Read (Stream, Skip_Bytes (I));
            end loop;
         end;

         --  Read certification level
         declare
            Level : Byte;
         begin
            Byte'Read (Stream, Level);

            case Level is
               when 0 =>
                  Cert_Level (1 .. 4) := "None";
               when 1 =>
                  Cert_Level (1 .. 6) := "Bronze";
               when 2 =>
                  Cert_Level (1 .. 6) := "Silver";
               when 3 =>
                  Cert_Level (1 .. 4) := "Gold";
               when 4 =>
                  Cert_Level (1 .. 8) := "Platinum";
               when others =>
                  Error (1 .. 28) := "Unknown certification level";
                  Close (File);
                  return;
            end case;
         end;

         Close (File);
         Valid := True;

      exception
         when others =>
            if Is_Open (File) then
               Close (File);
            end if;
            Error (1 .. 29) := "Error reading manifest file";
            Valid := False;
      end;
   end Verify_Manifest_File;

   ---------------------------------------------------------------------------
   --  Command Execution
   ---------------------------------------------------------------------------

   procedure Execute_Verify (
      Options : Verify_Options;
      Result  : out Verify_Result;
      Output  : out CLI_Output
   ) is
      Deployed_Code  : Byte_Array (0 .. 65535);
      Deployed_Size  : Natural;
      Expected_Code  : Byte_Array (0 .. 65535);
      Expected_Size  : Natural;
      Fetch_Success  : Boolean;
      Manifest_Valid : Boolean;
      Error_Msg      : String (1 .. 256);
   begin
      --  Initialize result
      Result := (
         Status            => Verify_Error,
         Bytecode_Match    => False,
         Manifest_Match    => False,
         Cert_Valid        => False,
         Deployed_Hash     => (others => 0),
         Expected_Hash     => (others => 0),
         Cert_Level        => (others => ' '),
         Cert_Level_Length => 0,
         Error_Message     => (others => ' '),
         Error_Length      => 0
      );

      --  Step 1: Fetch deployed bytecode
      if Options.Verbose then
         Put_Line ("Fetching deployed code from network...");
      end if;

      Fetch_Deployed_Code (
         RPC_URL => Options.RPC_Endpoint (1 .. Options.Endpoint_Length),
         Address => Options.Contract_Address,
         Code    => Deployed_Code,
         Size    => Deployed_Size,
         Success => Fetch_Success
      );

      if not Fetch_Success or Deployed_Size = 0 then
         Result.Status := Verify_Not_Found;
         Output.Status := Result_Not_Found;
         Output.Exit_Code := 1;
         declare
            Msg : constant String := "Contract not found at address";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
            Result.Error_Message (1 .. Msg'Length) := Msg;
            Result.Error_Length := Msg'Length;
         end;
         return;
      end if;

      --  Compute deployed hash
      declare
         Hash_Digest : SHA3_256_Digest;
      begin
         SHA3_256 (Deployed_Code (0 .. Deployed_Size - 1), Hash_Digest);
         for I in Hash_Digest'Range loop
            Result.Deployed_Hash (I) := Hash_Digest (I);
         end loop;
      end;

      if Options.Verbose then
         Put_Line ("Deployed code size: " & Natural'Image (Deployed_Size) & " bytes");
      end if;

      --  Step 2: Load expected bytecode
      if Options.Bytecode_Length > 0 then
         declare
            use Ada.Streams.Stream_IO;
            File   : File_Type;
            Stream : Stream_Access;
            Bytecode_Path : constant String :=
               Options.Bytecode_Path (1 .. Options.Bytecode_Length);
         begin
            Open (File, In_File, Bytecode_Path);
            Stream := Stream_IO.Stream (File);

            Expected_Size := Natural (Size (File));
            if Expected_Size > Expected_Code'Length then
               Expected_Size := Expected_Code'Length;
            end if;

            for I in 0 .. Expected_Size - 1 loop
               Byte'Read (Stream, Expected_Code (I));
            end loop;

            Close (File);

            --  Compute expected hash
            declare
               Hash_Digest : SHA3_256_Digest;
            begin
               SHA3_256 (Expected_Code (0 .. Expected_Size - 1), Hash_Digest);
               for I in Hash_Digest'Range loop
                  Result.Expected_Hash (I) := Hash_Digest (I);
               end loop;
            end;

         exception
            when others =>
               Expected_Size := 0;
         end;
      else
         Expected_Size := 0;
      end if;

      --  Step 3: Compare bytecode
      if Expected_Size > 0 then
         Result.Bytecode_Match :=
            Compare_Bytecode (
               Deployed_Code (0 .. Deployed_Size - 1),
               Expected_Code (0 .. Expected_Size - 1)
            );

         if Options.Verbose then
            if Result.Bytecode_Match then
               Put_Line ("Bytecode verification: MATCH");
            else
               Put_Line ("Bytecode verification: MISMATCH");
            end if;
         end if;
      else
         Result.Bytecode_Match := False;
      end if;

      --  Step 4: Verify manifest
      if Options.Manifest_Length > 0 and Options.Verify_Manifest then
         declare
            Cert_Level : String (1 .. 32);
         begin
            Verify_Manifest_File (
               Manifest_Path => Options.Manifest_Path (1 .. Options.Manifest_Length),
               Valid         => Manifest_Valid,
               Cert_Level    => Cert_Level,
               Error         => Error_Msg
            );

            Result.Manifest_Match := Manifest_Valid;
            if Manifest_Valid then
               Result.Cert_Level := Cert_Level;
               --  Calculate length
               for I in Cert_Level'Range loop
                  if Cert_Level (I) = ' ' then
                     Result.Cert_Level_Length := I - 1;
                     exit;
                  end if;
               end loop;
               Result.Cert_Valid := True;

               if Options.Verbose then
                  Put_Line ("Manifest verification: VALID");
                  Put_Line ("Certification level: " &
                           Cert_Level (1 .. Result.Cert_Level_Length));
               end if;
            else
               if Options.Verbose then
                  Put_Line ("Manifest verification: INVALID");
               end if;
            end if;
         end;
      end if;

      --  Step 5: Determine overall status
      if Result.Bytecode_Match and Result.Manifest_Match then
         Result.Status := Verify_Success;
         Output.Status := Result_Success;
         Output.Exit_Code := 0;
         declare
            Msg : constant String := "Contract verified successfully";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      elsif not Result.Bytecode_Match then
         Result.Status := Verify_Mismatch;
         Output.Status := Result_Error;
         Output.Exit_Code := 1;
         declare
            Msg : constant String := "Bytecode mismatch - contract may be different";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
            Result.Error_Message (1 .. Msg'Length) := Msg;
            Result.Error_Length := Msg'Length;
         end;
      elsif not Result.Cert_Valid then
         Result.Status := Verify_Invalid_Cert;
         Output.Status := Result_Error;
         Output.Exit_Code := 1;
         declare
            Msg : constant String := "Invalid certification";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
            Result.Error_Message (1 .. Msg'Length) := Msg;
            Result.Error_Length := Msg'Length;
         end;
      end if;

   exception
      when E : others =>
         Result.Status := Verify_Error;
         Output.Status := Result_Error;
         Output.Exit_Code := 1;
         declare
            Msg : constant String := "Error during verification";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
            Result.Error_Message (1 .. Msg'Length) := Msg;
            Result.Error_Length := Msg'Length;
         end;
   end Execute_Verify;

end Khepri_CLI_Verify;
