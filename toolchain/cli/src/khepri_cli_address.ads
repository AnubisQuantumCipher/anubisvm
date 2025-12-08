-------------------------------------------------------------------------------
--  KHEPRI CLI - Address Command Specification
--
--  Commands:
--    khepri address generate         - Generate new ML-DSA-87 keypair and address
--    khepri address from-key <pk>    - Derive address from existing public key
--    khepri address validate <addr>  - Validate address format and checksum
--    khepri address convert <addr>   - Convert between formats
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_Address; use Anubis_Address;
with Khepri_CLI; use Khepri_CLI;

package Khepri_CLI_Address with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Address_Command is (
      Cmd_Generate,      -- Generate new keypair
      Cmd_FromKey,       -- Derive from public key
      Cmd_Validate,      -- Validate address
      Cmd_Convert,       -- Convert formats
      Cmd_Info           -- Show address info
   );

   type Output_Format is (
      Format_Full,       -- Full canonical format
      Format_Short,      -- Shortened for display
      Format_Hex,        -- Raw hex bytes
      Format_JSON        -- JSON object
   );

   type Generate_Options is record
      Network       : Network_Type;
      Entity        : Entity_Type;
      Output_Format : Output_Format;
      Keystore_Path : String (1 .. 256);
      Key_Name      : String (1 .. 64);
      Encrypt_Key   : Boolean;
   end record;

   Default_Generate_Options : constant Generate_Options := (
      Network       => Main,
      Entity        => User,
      Output_Format => Format_Full,
      Keystore_Path => (others => ' '),
      Key_Name      => (others => ' '),
      Encrypt_Key   => True
   );

   type Generate_Result is record
      Success      : Boolean;
      Address      : Anubis_Address.Address;
      Address_Str  : String (1 .. 128);
      Address_Len  : Natural;
      Public_Key   : Byte_Array (0 .. 2591);   -- ML-DSA-87 PK
      Secret_Key   : Byte_Array (0 .. 4895);   -- ML-DSA-87 SK (if not encrypted)
      Keystore_ID  : String (1 .. 64);
   end record;

   type Validate_Result is record
      Valid        : Boolean;
      Network      : Network_Type;
      Entity       : Entity_Type;
      Checksum_OK  : Boolean;
      Error_Msg    : String (1 .. 256);
      Error_Len    : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Commands
   ---------------------------------------------------------------------------

   --  Generate new ML-DSA-87 keypair and derive address
   procedure Execute_Generate (
      Options  : Generate_Options;
      Result   : out Generate_Result;
      Output   : out CLI_Output
   ) with
      Global => null;

   --  Derive address from existing ML-DSA-87 public key
   procedure Execute_FromKey (
      Public_Key : Byte_Array;
      Network    : Network_Type;
      Entity     : Entity_Type;
      Result     : out Generate_Result;
      Output     : out CLI_Output
   ) with
      Global => null,
      Pre    => Public_Key'Length = 2592;

   --  Validate address string
   procedure Execute_Validate (
      Address_Str : String;
      Result      : out Validate_Result;
      Output      : out CLI_Output
   ) with
      Global => null;

   --  Convert address between formats
   procedure Execute_Convert (
      Address_Str  : String;
      Target_Fmt   : Output_Format;
      Converted    : out String;
      Conv_Len     : out Natural;
      Output       : out CLI_Output
   ) with
      Global => null;

   --  Show detailed address information
   procedure Execute_Info (
      Address_Str : String;
      Output      : out CLI_Output
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Formatting
   ---------------------------------------------------------------------------

   --  Format address for display
   procedure Format_Address_Output (
      Addr       : Anubis_Address.Address;
      Fmt        : Output_Format;
      Output_Str : out String;
      Output_Len : out Natural
   ) with
      Global => null;

   --  Format address as JSON
   procedure Format_Address_JSON (
      Addr       : Anubis_Address.Address;
      Result     : Generate_Result;
      Output_Str : out String;
      Output_Len : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Key Management
   ---------------------------------------------------------------------------

   --  Save keypair to keystore
   procedure Save_To_Keystore (
      Path       : String;
      Name       : String;
      Public_Key : Byte_Array;
      Secret_Key : Byte_Array;
      Encrypt    : Boolean;
      Success    : out Boolean
   ) with
      Global => null;

   --  Load public key from keystore
   procedure Load_From_Keystore (
      Path       : String;
      Name       : String;
      Public_Key : out Byte_Array;
      Success    : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Utilities
   ---------------------------------------------------------------------------

   --  Parse network string
   function Parse_Network (S : String) return Network_Type with
      Global => null;

   --  Parse entity string
   function Parse_Entity (S : String) return Entity_Type with
      Global => null;

   --  Parse output format string
   function Parse_Format (S : String) return Output_Format with
      Global => null;

end Khepri_CLI_Address;
