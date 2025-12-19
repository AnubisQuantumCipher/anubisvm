--  KHEPRI CLI: Command-Line Interface for SPARK Contract Development
--
--  This package provides the main entry point and command dispatcher for
--  the KHEPRI toolchain. All contract development, certification, and
--  deployment operations are accessed through this CLI.
--
--  Usage: khepri <command> [options] [arguments]
--
--  Commands:
--    Project:     init, build, clean
--    Certify:     certify, prove, wcet, audit
--    Test:        test, coverage, fuzz
--    Deploy:      deploy, verify, upgrade
--    Interact:    call, send, encode, decode
--    Keys:        keys new, keys list, keys show, keys export
--    Network:     node, block, tx, account

pragma SPARK_Mode (Off);  --  CLI uses full Ada for I/O

with Ada.Strings.Bounded;

package Khepri_CLI is

   ---------------------------------------------------------------------------
   --  Version Information
   ---------------------------------------------------------------------------

   Version_Major    : constant := 1;
   Version_Minor    : constant := 0;
   Version_Patch    : constant := 0;
   Version_String   : constant String := "1.0.0";

   Program_Name     : constant String := "khepri";
   Program_Desc     : constant String :=
      "KHEPRI - Native SPARK Contract Development Toolkit";

   ---------------------------------------------------------------------------
   --  Exit Codes
   ---------------------------------------------------------------------------

   Exit_Success          : constant := 0;
   Exit_Error            : constant := 1;
   Exit_Usage_Error      : constant := 2;
   Exit_Build_Error      : constant := 3;
   Exit_Proof_Error      : constant := 4;
   Exit_Deploy_Error     : constant := 5;
   Exit_Network_Error    : constant := 6;
   Exit_Key_Error        : constant := 7;

   ---------------------------------------------------------------------------
   --  Command Types
   ---------------------------------------------------------------------------

   type Command_Kind is (
      --  No command specified
      Cmd_None,
      Cmd_Help,
      Cmd_Version,

      --  Project management
      Cmd_Init,
      Cmd_Build,
      Cmd_Clean,

      --  Certification
      Cmd_Certify,
      Cmd_Prove,
      Cmd_WCET,
      Cmd_Audit,

      --  Testing
      Cmd_Test,
      Cmd_Coverage,
      Cmd_Fuzz,

      --  Deployment
      Cmd_Deploy,
      Cmd_Verify,
      Cmd_Upgrade,

      --  Interaction
      Cmd_Call,
      Cmd_Send,
      Cmd_Encode,
      Cmd_Decode,

      --  Key management
      Cmd_Keys,

      --  Address operations
      Cmd_Address,

      --  Native contract management
      Cmd_Contract,

      --  Network
      Cmd_Node,
      Cmd_Block,
      Cmd_TX,
      Cmd_Account
   );

   ---------------------------------------------------------------------------
   --  Certification Levels
   ---------------------------------------------------------------------------

   type Certification_Level is (
      Level_None,
      Level_Bronze,
      Level_Silver,
      Level_Gold,
      Level_Platinum
   );

   ---------------------------------------------------------------------------
   --  Network Configuration
   ---------------------------------------------------------------------------

   type Network_Kind is (
      Network_Local,
      Network_Testnet,
      Network_Mainnet,
      Network_Custom
   );

   ---------------------------------------------------------------------------
   --  Bounded String Types
   ---------------------------------------------------------------------------

   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (1024);
   subtype Path_String is Path_Strings.Bounded_String;

   package Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (256);
   subtype Name_String is Name_Strings.Bounded_String;

   package Arg_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (4096);
   subtype Arg_String is Arg_Strings.Bounded_String;

   ---------------------------------------------------------------------------
   --  Command Options
   ---------------------------------------------------------------------------

   type Common_Options is record
      Verbose      : Boolean := False;
      Quiet        : Boolean := False;
      No_Color     : Boolean := False;
      Config_File  : Path_String := Path_Strings.Null_Bounded_String;
      Working_Dir  : Path_String := Path_Strings.Null_Bounded_String;
   end record;

   type Build_Options is record
      Target       : Name_String := Name_Strings.Null_Bounded_String;
      Optimization : Natural := 2;
      Debug_Info   : Boolean := False;
      Profile      : Name_String := Name_Strings.Null_Bounded_String;
   end record;

   type Certify_Options is record
      Level        : Certification_Level := Level_Gold;
      Timeout      : Natural := 60;
      Prover_Steps : Natural := 100;
      Force        : Boolean := False;
   end record;

   type Deploy_Options is record
      Contract_Name : Name_String := Name_Strings.Null_Bounded_String;
      Network       : Network_Kind := Network_Testnet;
      Key_Name      : Name_String := Name_Strings.Null_Bounded_String;
      Gas_Limit     : Natural := 1_000_000;
      Gas_Price     : Natural := 1;
      Args          : Arg_String := Arg_Strings.Null_Bounded_String;
      Dry_Run       : Boolean := False;
   end record;

   type Test_Options is record
      Pattern      : Name_String := Name_Strings.Null_Bounded_String;
      Coverage     : Boolean := False;
      Verbose      : Boolean := False;
      Timeout      : Natural := 30;
   end record;

   ---------------------------------------------------------------------------
   --  Main Entry Point
   ---------------------------------------------------------------------------

   procedure Main;
   --  Main entry point for the CLI. Parses command-line arguments
   --  and dispatches to the appropriate command handler.

   ---------------------------------------------------------------------------
   --  Command Handlers
   ---------------------------------------------------------------------------

   procedure Handle_Help (Topic : String := "");
   --  Display help information for a command or general help

   procedure Handle_Version;
   --  Display version information

   procedure Handle_Init (
      Project_Name : String;
      Template     : String := "token"
   );
   --  Initialize a new KHEPRI contract project

   procedure Handle_Build (Options : Build_Options);
   --  Build the contract to ELF binary

   procedure Handle_Clean;
   --  Remove build artifacts

   procedure Handle_Certify (Options : Certify_Options);
   --  Run full certification (proofs + WCET)

   procedure Handle_Prove (Options : Certify_Options);
   --  Run GNATprove only

   procedure Handle_WCET (Options : Certify_Options);
   --  Run WCET analysis only

   procedure Handle_Audit;
   --  Prepare audit package for Platinum certification

   procedure Handle_Test (Options : Test_Options);
   --  Run unit and integration tests

   procedure Handle_Coverage;
   --  Generate coverage report

   procedure Handle_Fuzz (Iterations : Natural := 10000);
   --  Run property-based fuzzing

   procedure Handle_Deploy (Options : Deploy_Options);
   --  Deploy contract to network

   procedure Handle_Verify (Contract_Address : String);
   --  Verify deployed contract

   procedure Handle_Call (
      Contract : String;
      Function_Name : String;
      Args : String := ""
   );
   --  Call contract function (read-only)

   procedure Handle_Send (
      Contract : String;
      Function_Name : String;
      Args : String := "";
      Options : Deploy_Options
   );
   --  Send transaction to contract

   procedure Handle_Keys (Subcommand : String; Args : String := "");
   --  Key management operations

   procedure Handle_Address (Subcommand : String; Args : String := "");
   --  Address operations: generate, validate, info, from-key

   procedure Handle_Contract (Subcommand : String; Args : String := "");
   --  Native contract management: new, build, deploy, call, list

   ---------------------------------------------------------------------------
   --  Output Formatting
   ---------------------------------------------------------------------------

   procedure Print_Banner;
   --  Print the KHEPRI banner

   procedure Print_Success (Msg : String);
   --  Print success message in green

   procedure Print_Error (Msg : String);
   --  Print error message in red

   procedure Print_Warning (Msg : String);
   --  Print warning message in yellow

   procedure Print_Info (Msg : String);
   --  Print info message in blue

   procedure Print_Progress (Step : Natural; Total : Natural; Msg : String);
   --  Print progress indicator

   procedure Print_Table_Header (Columns : String);
   --  Print table header with separator

   procedure Print_Table_Row (Values : String);
   --  Print table row

   ---------------------------------------------------------------------------
   --  Interactive Prompts
   ---------------------------------------------------------------------------

   function Prompt_Confirm (Msg : String; Default : Boolean := False) return Boolean;
   --  Prompt user for yes/no confirmation

   function Prompt_Password (Msg : String) return String;
   --  Prompt for password (hidden input)

   function Prompt_Input (Msg : String; Default : String := "") return String;
   --  Prompt for text input

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Parse_Command (Arg : String) return Command_Kind;
   --  Parse command string to Command_Kind

   function Command_To_String (Cmd : Command_Kind) return String;
   --  Convert Command_Kind to string

   function Level_To_String (Level : Certification_Level) return String;
   --  Convert Certification_Level to string

   function Network_To_String (Net : Network_Kind) return String;
   --  Convert Network_Kind to string

   function File_Exists (Path : String) return Boolean;
   --  Check if file exists

   function Dir_Exists (Path : String) return Boolean;
   --  Check if directory exists

   function Get_Current_Dir return String;
   --  Get current working directory

   function Find_Project_Root return String;
   --  Find khepri.toml in current or parent directories

end Khepri_CLI;
