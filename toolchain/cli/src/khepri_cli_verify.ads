-------------------------------------------------------------------------------
--  KHEPRI CLI - Verify Command (Contract Verification)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Khepri_CLI; use Khepri_CLI;
with Khepri_Types; use Khepri_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

package Khepri_CLI_Verify with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Verify Command Options
   ---------------------------------------------------------------------------

   type Verify_Options is record
      Contract_Address : Address;               --  Deployed contract address
      RPC_Endpoint     : String (1 .. 256);    --  RPC URL
      Endpoint_Length  : Natural;
      Source_Path      : String (1 .. 256);    --  Source file path
      Source_Length    : Natural;
      Bytecode_Path    : String (1 .. 256);    --  Expected bytecode file
      Bytecode_Length  : Natural;
      Manifest_Path    : String (1 .. 256);    --  Manifest file
      Manifest_Length  : Natural;
      Verify_Hash      : Boolean;               --  Verify bytecode hash
      Verify_Manifest  : Boolean;               --  Verify manifest matches
      Check_Cert       : Boolean;               --  Check certification level
      Verbose          : Boolean;
   end record;

   Default_Verify_Options : constant Verify_Options := (
      Contract_Address => (others => 0),
      RPC_Endpoint     => (others => ' '),
      Endpoint_Length  => 0,
      Source_Path      => (others => ' '),
      Source_Length    => 0,
      Bytecode_Path    => (others => ' '),
      Bytecode_Length  => 0,
      Manifest_Path    => (others => ' '),
      Manifest_Length  => 0,
      Verify_Hash      => True,
      Verify_Manifest  => True,
      Check_Cert       => True,
      Verbose          => False
   );

   ---------------------------------------------------------------------------
   --  Verification Result
   ---------------------------------------------------------------------------

   type Verification_Status is (
      Verify_Success,        --  Contract verified successfully
      Verify_Mismatch,       --  Bytecode doesn't match
      Verify_Not_Found,      --  Contract not deployed
      Verify_Invalid_Cert,   --  Certification invalid
      Verify_Error           --  Error during verification
   );

   type Verify_Result is record
      Status            : Verification_Status;
      Bytecode_Match    : Boolean;
      Manifest_Match    : Boolean;
      Cert_Valid        : Boolean;
      Deployed_Hash     : Hash256;
      Expected_Hash     : Hash256;
      Cert_Level        : String (1 .. 32);  --  "Bronze", "Silver", etc.
      Cert_Level_Length : Natural;
      Error_Message     : String (1 .. 256);
      Error_Length      : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Command Execution
   ---------------------------------------------------------------------------

   --  Execute 'khepri verify' command
   procedure Execute_Verify (
      Options : Verify_Options;
      Result  : out Verify_Result;
      Output  : out CLI_Output
   ) with
      Global => null;

   --  Fetch deployed bytecode from network
   procedure Fetch_Deployed_Code (
      RPC_URL  : String;
      Address  : Khepri_Types.Address;
      Code     : out Byte_Array;
      Size     : out Natural;
      Success  : out Boolean
   ) with
      Global => null,
      Pre    => RPC_URL'Length > 0 and Code'Length >= 65536;

   --  Compare bytecode hashes
   function Compare_Bytecode (
      Deployed : Byte_Array;
      Expected : Byte_Array
   ) return Boolean with
      Global => null;

   --  Verify manifest signature and integrity
   procedure Verify_Manifest_File (
      Manifest_Path : String;
      Valid         : out Boolean;
      Cert_Level    : out String;
      Error         : out String
   ) with
      Global => null,
      Pre    => Manifest_Path'Length > 0 and
                Cert_Level'Length >= 32 and
                Error'Length >= 256;

end Khepri_CLI_Verify;
