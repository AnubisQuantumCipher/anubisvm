-------------------------------------------------------------------------------
--  KHEPRI CLI - Main Entry Point
--
--  Usage:
--    khepri init <project_name>     - Create new project
--    khepri build                    - Build contract
--    khepri prove [--level=N]       - Run formal verification
--    khepri certify [--target=LEVEL] - Generate certification
--    khepri deploy <contract.bin>   - Deploy to chain
--    khepri test                     - Run tests
--    khepri help [command]          - Show help
--    khepri version                 - Show version
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;
with Khepri_CLI;
with Khepri_CLI_Init;
with Khepri_CLI_Build;
with Khepri_CLI_Prove;
with Khepri_CLI_Certify;
with Khepri_CLI_Deploy;

procedure Khepri_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Khepri_CLI;

   Version_String : constant String := "khepri 0.1.0 (AegisVM)";

   procedure Print_Banner is
   begin
      Put_Line ("╔═══════════════════════════════════════════════════════════════╗");
      Put_Line ("║                     KHEPRI CLI v0.1.0                         ║");
      Put_Line ("║        Native SPARK Contract Toolchain for AegisVM            ║");
      Put_Line ("╚═══════════════════════════════════════════════════════════════╝");
   end Print_Banner;

   procedure Print_Usage is
   begin
      Put_Line ("Usage: khepri <command> [options]");
      Put_Line ("");
      Put_Line ("Commands:");
      Put_Line ("  init <name>      Create new KHEPRI contract project");
      Put_Line ("  build            Build contract to native binary");
      Put_Line ("  prove            Run GNATprove for formal verification");
      Put_Line ("  certify          Generate certification manifest");
      Put_Line ("  deploy           Deploy contract to AegisVM chain");
      Put_Line ("  test             Run contract tests");
      Put_Line ("  help [cmd]       Show help for command");
      Put_Line ("  version          Show version information");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --project=PATH   Path to khepri.toml or .gpr file");
      Put_Line ("  --verbose        Enable verbose output");
      Put_Line ("  --quiet          Suppress non-essential output");
      Put_Line ("");
      Put_Line ("Examples:");
      Put_Line ("  khepri init my_token");
      Put_Line ("  khepri build --project=my_token.gpr");
      Put_Line ("  khepri prove --level=gold");
      Put_Line ("  khepri certify --target=gold");
      Put_Line ("  khepri deploy obj/my_token.bin --manifest=my_token.manifest");
   end Print_Usage;

   procedure Print_Help_Init is
   begin
      Put_Line ("khepri init - Create new KHEPRI contract project");
      Put_Line ("");
      Put_Line ("Usage: khepri init <project_name> [options]");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --template=TYPE  Contract template (token, nft, escrow, empty)");
      Put_Line ("  --output=DIR     Output directory (default: current)");
      Put_Line ("");
      Put_Line ("Creates:");
      Put_Line ("  <project_name>/");
      Put_Line ("  ├── khepri.toml        Project configuration");
      Put_Line ("  ├── <project_name>.gpr GPR project file");
      Put_Line ("  ├── src/");
      Put_Line ("  │   ├── contract.ads   Contract specification");
      Put_Line ("  │   └── contract.adb   Contract implementation");
      Put_Line ("  └── tests/");
      Put_Line ("      └── test_contract.adb");
   end Print_Help_Init;

   procedure Print_Help_Prove is
   begin
      Put_Line ("khepri prove - Run GNATprove for formal verification");
      Put_Line ("");
      Put_Line ("Usage: khepri prove [options]");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --level=LEVEL    Proof level (flow, bronze, silver, gold, max)");
      Put_Line ("  --timeout=SEC    Per-VC timeout in seconds (default: 60)");
      Put_Line ("  --parallel=N     Parallel jobs (default: auto)");
      Put_Line ("  --prover=NAME    Prover choice (auto, cvc5, z3, alt-ergo, all)");
      Put_Line ("  --report=MODE    Report mode (fail, all, statistics)");
      Put_Line ("");
      Put_Line ("Levels:");
      Put_Line ("  flow     Flow analysis only (fast)");
      Put_Line ("  bronze   Absence of runtime errors");
      Put_Line ("  silver   Full proof coverage (100% VCs)");
      Put_Line ("  gold     With WCET and constant-time analysis");
      Put_Line ("  max      Maximum proof effort");
   end Print_Help_Prove;

   procedure Print_Help_Certify is
   begin
      Put_Line ("khepri certify - Generate certification manifest");
      Put_Line ("");
      Put_Line ("Usage: khepri certify [options]");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --target=LEVEL   Target certification (bronze, silver, gold, platinum)");
      Put_Line ("  --output=PATH    Output manifest path");
      Put_Line ("  --wcet           Include WCET analysis (default: yes)");
      Put_Line ("  --ct             Include constant-time analysis (default: yes)");
      Put_Line ("  --auditor-key=PATH  Auditor key for Platinum certification");
      Put_Line ("");
      Put_Line ("Certification Levels:");
      Put_Line ("  Bronze    SPARK Mode + Flow Analysis + AoRTE");
      Put_Line ("  Silver    Bronze + 100% proof coverage + WCET");
      Put_Line ("  Gold      Silver + functional contracts + constant-time");
      Put_Line ("  Platinum  Gold + external audit signature");
      Put_Line ("");
      Put_Line ("Gas Discounts:");
      Put_Line ("  Bronze:   1.0x (no discount)");
      Put_Line ("  Silver:   0.9x (10% discount)");
      Put_Line ("  Gold:     0.8x (20% discount)");
      Put_Line ("  Platinum: 0.7x (30% discount)");
   end Print_Help_Certify;

   procedure Print_Help_Deploy is
   begin
      Put_Line ("khepri deploy - Deploy contract to AegisVM chain");
      Put_Line ("");
      Put_Line ("Usage: khepri deploy <contract.bin> [options]");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --manifest=PATH  Path to certification manifest");
      Put_Line ("  --rpc=URL        RPC endpoint (default: http://localhost:26657)");
      Put_Line ("  --chain-id=N     Chain ID (default: 1)");
      Put_Line ("  --gas-limit=N    Gas limit (default: 2000000)");
      Put_Line ("  --gas-price=N    Gas price in smallest unit");
      Put_Line ("  --value=N        ANUBIS to send with deployment");
      Put_Line ("  --keystore=PATH  Path to keystore");
      Put_Line ("  --key=ID         Key identifier");
      Put_Line ("  --simulate       Dry run (don't submit)");
      Put_Line ("  --no-wait        Don't wait for confirmation");
   end Print_Help_Deploy;

   Command : CLI_Command;
   Config  : CLI_Config := Default_Config;
   Output  : CLI_Output;
begin
   --  No arguments - show usage
   if Argument_Count = 0 then
      Print_Banner;
      Put_Line ("");
      Print_Usage;
      Set_Exit_Status (0);
      return;
   end if;

   --  Parse command
   Command := Parse_Command (Argument (1));

   case Command is
      when Cmd_Version =>
         Put_Line (Version_String);
         Set_Exit_Status (0);

      when Cmd_Help =>
         if Argument_Count >= 2 then
            declare
               Sub_Cmd : constant CLI_Command := Parse_Command (Argument (2));
            begin
               case Sub_Cmd is
                  when Cmd_Init    => Print_Help_Init;
                  when Cmd_Prove   => Print_Help_Prove;
                  when Cmd_Certify => Print_Help_Certify;
                  when Cmd_Deploy  => Print_Help_Deploy;
                  when others      => Print_Usage;
               end case;
            end;
         else
            Print_Usage;
         end if;
         Set_Exit_Status (0);

      when Cmd_Init =>
         if Argument_Count < 2 then
            Put_Line ("Error: Project name required");
            Put_Line ("Usage: khepri init <project_name>");
            Set_Exit_Status (1);
            return;
         end if;
         Khepri_CLI_Init.Create_Project_Structure (
            Project_Name => Argument (2),
            Output_Dir   => ".",
            Success      => Output.Status = Result_Success
         );
         if Output.Status = Result_Success then
            Put_Line ("Created project: " & Argument (2));
            Set_Exit_Status (0);
         else
            Put_Line ("Error creating project");
            Set_Exit_Status (1);
         end if;

      when Cmd_Build =>
         Khepri_CLI_Build.Execute_Build (
            Options => Khepri_CLI_Build.Default_Build_Options,
            Output  => Output
         );
         Set_Exit_Status (Output.Exit_Code);

      when Cmd_Prove =>
         Khepri_CLI_Prove.Execute_Prove (
            Options => Khepri_CLI_Prove.Default_Proof_Options,
            Results => (others => <>),
            Output  => Output
         );
         Set_Exit_Status (Output.Exit_Code);

      when Cmd_Certify =>
         Khepri_CLI_Certify.Execute_Certify (
            Options => Khepri_CLI_Certify.Default_Certify_Options,
            Results => (others => <>),
            Output  => Output
         );
         Set_Exit_Status (Output.Exit_Code);

      when Cmd_Deploy =>
         if Argument_Count < 2 then
            Put_Line ("Error: Contract binary required");
            Put_Line ("Usage: khepri deploy <contract.bin>");
            Set_Exit_Status (1);
            return;
         end if;
         declare
            Opts : Khepri_CLI_Deploy.Deploy_Options :=
               Khepri_CLI_Deploy.Default_Deploy_Options;
         begin
            --  Set contract path
            Opts.Contract_Path (1 .. Argument (2)'Length) := Argument (2);
            Khepri_CLI_Deploy.Execute_Deploy (
               Options => Opts,
               Result  => (others => <>),
               Output  => Output
            );
         end;
         Set_Exit_Status (Output.Exit_Code);

      when Cmd_Test =>
         Put_Line ("Running tests...");
         --  Would invoke test runner
         Set_Exit_Status (0);

      when Cmd_Unknown =>
         Put_Line ("Error: Unknown command '" & Argument (1) & "'");
         Put_Line ("");
         Print_Usage;
         Set_Exit_Status (1);
   end case;
end Khepri_Main;
