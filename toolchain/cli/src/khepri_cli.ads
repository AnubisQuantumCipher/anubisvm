-------------------------------------------------------------------------------
--  KHEPRI CLI - Command Line Interface for Contract Development
--
--  Commands:
--    khepri init    - Initialize new contract project
--    khepri build   - Build contract to native binary
--    khepri prove   - Run GNATprove for formal verification
--    khepri certify - Generate certification manifest
--    khepri deploy  - Deploy contract to chain
--    khepri test    - Run contract tests
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Khepri_CLI with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  CLI Command Types
   ---------------------------------------------------------------------------

   type CLI_Command is (
      Cmd_Init,      -- Initialize project
      Cmd_New,       -- Create new project from template (alias for init)
      Cmd_Build,     -- Build contract
      Cmd_Prove,     -- Run proofs
      Cmd_Certify,   -- Generate certification
      Cmd_Deploy,    -- Deploy to chain
      Cmd_Test,      -- Run tests
      Cmd_Verify,    -- Verify deployed contract
      Cmd_Address,   -- Address utilities
      Cmd_Help,      -- Show help
      Cmd_Version,   -- Show version
      Cmd_Unknown    -- Unknown command
   );

   --  Build target
   type Build_Target is (
      Target_Native,    -- Native binary for current platform
      Target_Debug,     -- Debug build with symbols
      Target_Release,   -- Optimized release build
      Target_Embedded   -- Embedded/IoT target
   );

   --  Proof level
   type Proof_Level is (
      Level_Flow,    -- Flow analysis only (fast)
      Level_Bronze,  -- AoRTE proofs
      Level_Silver,  -- Full proof coverage
      Level_Gold,    -- With WCET analysis
      Level_Max      -- Maximum proof level
   );

   --  Proof target (aliasing for compatibility)
   subtype Proof_Target is Proof_Level;
   Proof_Flow   : constant Proof_Target := Level_Flow;
   Proof_Bronze : constant Proof_Target := Level_Bronze;
   Proof_Silver : constant Proof_Target := Level_Silver;
   Proof_Gold   : constant Proof_Target := Level_Gold;
   Proof_Max    : constant Proof_Target := Level_Max;

   --  Certification target
   type Cert_Target is (
      Cert_Bronze,
      Cert_Silver,
      Cert_Gold,
      Cert_Platinum
   );

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------

   type CLI_Config is record
      --  Project settings
      Project_Name   : String (1 .. 64);
      Project_Dir    : String (1 .. 256);
      Build_Dir      : String (1 .. 256);

      --  Build settings
      Target         : Build_Target;
      Optimization   : Natural range 0 .. 3;
      Debug_Info     : Boolean;

      --  Proof settings
      Proof_Level    : Proof_Level;
      Timeout        : Natural;  -- seconds
      Parallel_Jobs  : Natural;

      --  Deploy settings
      RPC_Endpoint   : String (1 .. 256);
      Chain_ID       : Unsigned_64;
      Gas_Limit      : Unsigned_64;
   end record;

   Default_Config : constant CLI_Config := (
      Project_Name   => (others => ' '),
      Project_Dir    => (others => ' '),
      Build_Dir      => (others => ' '),
      Target         => Target_Native,
      Optimization   => 2,
      Debug_Info     => False,
      Proof_Level    => Level_Silver,
      Timeout        => 60,
      Parallel_Jobs  => 0,  -- Auto-detect
      RPC_Endpoint   => (others => ' '),
      Chain_ID       => 1,
      Gas_Limit      => 1_000_000
   );

   ---------------------------------------------------------------------------
   --  Command Results
   ---------------------------------------------------------------------------

   type Command_Result is (
      Result_Success,
      Result_Error,
      Result_Not_Found,
      Result_Invalid_Args,
      Result_Build_Failed,
      Result_Proof_Failed,
      Result_Deploy_Failed
   );

   type CLI_Output is record
      Status         : Command_Result;
      Message        : String (1 .. 1024);
      Message_Length : Natural;
      Exit_Code      : Integer;
   end record;

   ---------------------------------------------------------------------------
   --  Main Entry Point
   ---------------------------------------------------------------------------

   --  Parse command from arguments
   function Parse_Command (Args : String) return CLI_Command with
      Global => null;

   --  Execute CLI command
   procedure Execute (
      Command  : CLI_Command;
      Args     : String;
      Config   : CLI_Config;
      Output   : out CLI_Output
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Individual Commands
   ---------------------------------------------------------------------------

   --  khepri init [name] - Initialize new project
   procedure Cmd_Init_Execute (
      Project_Name : String;
      Output_Dir   : String;
      Output       : out CLI_Output
   ) with
      Global => null;

   --  khepri build - Build contract
   procedure Cmd_Build_Execute (
      Config       : CLI_Config;
      Output       : out CLI_Output
   ) with
      Global => null;

   --  khepri prove - Run GNATprove
   procedure Cmd_Prove_Execute (
      Config       : CLI_Config;
      Level        : Proof_Level;
      Output       : out CLI_Output
   ) with
      Global => null;

   --  khepri certify - Generate certification
   procedure Cmd_Certify_Execute (
      Config       : CLI_Config;
      Target_Level : Cert_Target;
      Output       : out CLI_Output
   ) with
      Global => null;

   --  khepri deploy - Deploy to chain
   procedure Cmd_Deploy_Execute (
      Config       : CLI_Config;
      Contract_Path: String;
      Output       : out CLI_Output
   ) with
      Global => null;

   --  khepri test - Run tests
   procedure Cmd_Test_Execute (
      Config       : CLI_Config;
      Test_Filter  : String;
      Output       : out CLI_Output
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Configuration File
   ---------------------------------------------------------------------------

   --  Load config from khepri.toml
   procedure Load_Config (
      Path         : String;
      Config       : out CLI_Config;
      Success      : out Boolean
   ) with
      Global => null;

   --  Save config to khepri.toml
   procedure Save_Config (
      Path         : String;
      Config       : CLI_Config;
      Success      : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Project Templates
   ---------------------------------------------------------------------------

   type Template_Type is (
      Template_Token,     -- ERC20-style token
      Template_NFT,       -- NFT contract
      Template_Escrow,    -- Escrow/multisig
      Template_Oracle,    -- Price oracle
      Template_Empty      -- Empty contract
   );

   --  Generate project from template
   procedure Generate_From_Template (
      Template     : Template_Type;
      Project_Name : String;
      Output_Dir   : String;
      Success      : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Build System Integration
   ---------------------------------------------------------------------------

   --  Generate GPR project file
   procedure Generate_GPR (
      Project_Name : String;
      Output_Path  : String;
      Config       : CLI_Config;
      Success      : out Boolean
   ) with
      Global => null;

   --  Invoke gprbuild
   procedure Run_GPRbuild (
      Project_Path : String;
      Config       : CLI_Config;
      Output       : out CLI_Output
   ) with
      Global => null;

   --  Invoke gnatprove
   procedure Run_GNATprove (
      Project_Path : String;
      Level        : Proof_Level;
      Timeout      : Natural;
      Output       : out CLI_Output
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Help Text
   ---------------------------------------------------------------------------

   procedure Print_Help (Command : CLI_Command) with
      Global => null;

   procedure Print_Version with
      Global => null;

   procedure Print_Usage with
      Global => null;

end Khepri_CLI;
