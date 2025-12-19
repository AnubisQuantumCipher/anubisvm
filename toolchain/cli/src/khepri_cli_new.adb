-------------------------------------------------------------------------------
--  KHEPRI CLI - New Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  File I/O and template generation

with Ada.Text_IO;
with Ada.Directories;
with Khepri_CLI_Init;

package body Khepri_CLI_New is

   use Ada.Text_IO;
   use Ada.Directories;

   ---------------------------------------------------------------------------
   --  Template Descriptions
   ---------------------------------------------------------------------------

   function Get_Template_Description (
      Template : Project_Template
   ) return String is
   begin
      case Template is
         when Template_Token =>
            return "ERC20-style fungible token with transfer and approval";
         when Template_NFT =>
            return "ERC721-style NFT collection with minting and metadata";
         when Template_Escrow =>
            return "Multi-party escrow with milestone-based releases";
         when Template_Oracle =>
            return "Price oracle with multiple data feed aggregation";
         when Template_Staking =>
            return "Token staking contract with reward distribution";
         when Template_DAO =>
            return "Governance DAO with proposal voting and execution";
         when Template_AMM =>
            return "Automated market maker with liquidity pools";
         when Template_Minimal =>
            return "Minimal contract scaffold with basic structure";
      end case;
   end Get_Template_Description;

   function Get_Recommended_Proof_Level (
      Template : Project_Template
   ) return Proof_Level is
   begin
      case Template is
         when Template_Token | Template_Staking | Template_AMM =>
            return Level_Gold;  --  Financial contracts need highest assurance
         when Template_DAO | Template_Oracle =>
            return Level_Silver;  --  Governance needs solid proofs
         when Template_NFT | Template_Escrow =>
            return Level_Silver;  --  Asset management
         when Template_Minimal =>
            return Level_Bronze;  --  Minimal baseline
      end case;
   end Get_Recommended_Proof_Level;

   ---------------------------------------------------------------------------
   --  Validation
   ---------------------------------------------------------------------------

   procedure Validate_Options (
      Options : New_Options;
      Valid   : out Boolean;
      Error   : out String
   ) is
   begin
      Valid := False;
      Error := (others => ' ');

      --  Check project name
      if Options.Name_Length = 0 then
         Error (1 .. 27) := "Project name cannot be empty";
         return;
      end if;

      --  Check project name validity (alphanumeric + underscore)
      for I in 1 .. Options.Name_Length loop
         declare
            C : constant Character := Options.Project_Name (I);
         begin
            if not (C in 'a' .. 'z' or C in 'A' .. 'Z' or
                    C in '0' .. '9' or C = '_' or C = '-')
            then
               Error (1 .. 40) := "Invalid character in project name";
               return;
            end if;
         end;
      end loop;

      --  Check if output directory exists
      if Options.Dir_Length > 0 then
         declare
            Dir : constant String :=
               Options.Output_Dir (1 .. Options.Dir_Length);
         begin
            if not Exists (Dir) then
               Error (1 .. 28) := "Output directory not found";
               return;
            end if;
         end;
      end if;

      Valid := True;
   end Validate_Options;

   ---------------------------------------------------------------------------
   --  Template File Generation
   ---------------------------------------------------------------------------

   procedure Generate_Contract_Spec (
      Project_Name : String;
      Template     : Project_Template;
      Output_Dir   : String
   ) is
      File : File_Type;
      Spec_Path : constant String :=
         Output_Dir & "/" & Project_Name & ".ads";
   begin
      Create (File, Out_File, Spec_Path);

      --  Package header
      Put_Line (File, "pragma SPARK_Mode (On);");
      New_Line (File);
      Put_Line (File, "with Khepri_Types; use Khepri_Types;");
      Put_Line (File, "with Khepri_Runtime; use Khepri_Runtime;");
      New_Line (File);

      --  Package declaration
      Put_Line (File, "package " & Project_Name & " with");
      Put_Line (File, "   SPARK_Mode => On");
      Put_Line (File, "is");
      New_Line (File);

      --  Template-specific interface
      case Template is
         when Template_Token =>
            Put_Line (File, "   --  ERC20 Token Interface");
            New_Line (File);
            Put_Line (File, "   function Total_Supply return Uint256 with");
            Put_Line (File, "      Global => null;");
            New_Line (File);
            Put_Line (File, "   function Balance_Of (Owner : Address) return Uint256 with");
            Put_Line (File, "      Global => null;");
            New_Line (File);
            Put_Line (File, "   procedure Transfer (");
            Put_Line (File, "      To     : in Address;");
            Put_Line (File, "      Amount : in Uint256");
            Put_Line (File, "   ) with");
            Put_Line (File, "      Global => null,");
            Put_Line (File, "      Pre    => Amount <= Balance_Of (Get_Sender);");

         when Template_NFT =>
            Put_Line (File, "   --  ERC721 NFT Interface");
            New_Line (File);
            Put_Line (File, "   function Owner_Of (Token_Id : Uint256) return Address with");
            Put_Line (File, "      Global => null;");
            New_Line (File);
            Put_Line (File, "   procedure Mint (");
            Put_Line (File, "      To       : in Address;");
            Put_Line (File, "      Token_Id : in Uint256");
            Put_Line (File, "   ) with");
            Put_Line (File, "      Global => null;");

         when Template_Minimal =>
            Put_Line (File, "   --  Contract State");
            New_Line (File);
            Put_Line (File, "   procedure Initialize with");
            Put_Line (File, "      Global => null;");
            New_Line (File);
            Put_Line (File, "   function Get_Value return Uint256 with");
            Put_Line (File, "      Global => null;");

         when others =>
            Put_Line (File, "   --  Contract interface for " &
                      Project_Template'Image (Template));
      end case;

      New_Line (File);
      Put_Line (File, "end " & Project_Name & ";");

      Close (File);
   end Generate_Contract_Spec;

   procedure Generate_Contract_Body (
      Project_Name : String;
      Template     : Project_Template;
      Output_Dir   : String
   ) is
      File : File_Type;
      Body_Path : constant String :=
         Output_Dir & "/" & Project_Name & ".adb";
   begin
      Create (File, Out_File, Body_Path);

      Put_Line (File, "pragma SPARK_Mode (On);");
      New_Line (File);
      Put_Line (File, "package body " & Project_Name & " with");
      Put_Line (File, "   SPARK_Mode => On");
      Put_Line (File, "is");
      New_Line (File);

      --  Template-specific implementation stubs
      case Template is
         when Template_Token =>
            Put_Line (File, "   Storage_Total_Supply : Uint256 := U256_Zero;");
            New_Line (File);
            Put_Line (File, "   function Total_Supply return Uint256 is");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Storage_Total_Supply;");
            Put_Line (File, "   end Total_Supply;");
            New_Line (File);
            Put_Line (File, "   function Balance_Of (Owner : Address) return Uint256 is");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Read_Balance (Owner);");
            Put_Line (File, "   end Balance_Of;");
            New_Line (File);
            Put_Line (File, "   procedure Transfer (To : in Address; Amount : in Uint256) is");
            Put_Line (File, "      Sender : constant Address := Get_Sender;");
            Put_Line (File, "   begin");
            Put_Line (File, "      --  Implementation: Deduct from sender, add to recipient");
            Put_Line (File, "      null;");
            Put_Line (File, "   end Transfer;");

         when Template_Minimal =>
            Put_Line (File, "   Storage_Value : Uint256 := U256_Zero;");
            New_Line (File);
            Put_Line (File, "   procedure Initialize is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Storage_Value := U256_Zero;");
            Put_Line (File, "   end Initialize;");
            New_Line (File);
            Put_Line (File, "   function Get_Value return Uint256 is");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Storage_Value;");
            Put_Line (File, "   end Get_Value;");

         when others =>
            Put_Line (File, "   --  TODO: Implement " &
                      Project_Template'Image (Template) & " logic");
      end case;

      New_Line (File);
      Put_Line (File, "end " & Project_Name & ";");

      Close (File);
   end Generate_Contract_Body;

   procedure Generate_Test_File (
      Project_Name : String;
      Output_Dir   : String
   ) is
      File : File_Type;
      Test_Path : constant String :=
         Output_Dir & "/test_" & Project_Name & ".adb";
   begin
      Create (File, Out_File, Test_Path);

      Put_Line (File, "pragma SPARK_Mode (Off);  --  Tests use I/O");
      New_Line (File);
      Put_Line (File, "with Ada.Text_IO;");
      Put_Line (File, "with " & Project_Name & ";");
      Put_Line (File, "with Khepri_Test_Runner; use Khepri_Test_Runner;");
      New_Line (File);
      Put_Line (File, "procedure Test_" & Project_Name & " is");
      Put_Line (File, "   use Ada.Text_IO;");
      New_Line (File);
      Put_Line (File, "   Suite : Test_Suite;");
      Put_Line (File, "begin");
      Put_Line (File, "   Put_Line (""Running tests for " & Project_Name & "..."");");
      New_Line (File);
      Put_Line (File, "   --  TODO: Add test cases");
      Put_Line (File, "   Add_Test (Suite, ""Basic test"", null);");
      New_Line (File);
      Put_Line (File, "   Run_Suite (Suite);");
      Put_Line (File, "end Test_" & Project_Name & ";");

      Close (File);
   end Generate_Test_File;

   ---------------------------------------------------------------------------
   --  Command Execution
   ---------------------------------------------------------------------------

   procedure Execute_New (
      Options  : New_Options;
      Result   : out New_Result;
      Output   : out CLI_Output
   ) is
      Valid : Boolean;
      Error_Msg : String (1 .. 256);
      Project_Name : constant String := Options.Project_Name (1 .. Options.Name_Length);
      Output_Dir : constant String :=
         (if Options.Dir_Length > 0 then
             Options.Output_Dir (1 .. Options.Dir_Length)
          else
             ".");
      Project_Dir : constant String := Output_Dir & "/" & Project_Name;
      Files_Created : Natural := 0;
   begin
      --  Initialize result
      Result := (
         Success       => False,
         Project_Path  => (others => ' '),
         Path_Length   => 0,
         Files_Created => 0,
         Error_Message => (others => ' '),
         Error_Length  => 0
      );

      --  Validate options
      Validate_Options (Options, Valid, Error_Msg);
      if not Valid then
         declare
            Msg_Len : constant Natural := 256;
         begin
            Result.Error_Message := Error_Msg;
            Result.Error_Length := Msg_Len;
            Output.Status := Result_Invalid_Args;
            Output.Exit_Code := 1;
            Output.Message (1 .. Error_Msg'Length) := Error_Msg;
            Output.Message_Length := Error_Msg'Length;
         end;
         return;
      end if;

      --  Create project structure
      begin
         --  Create main project directory
         if not Exists (Project_Dir) then
            Create_Directory (Project_Dir);
         end if;

         --  Create src directory
         declare
            Src_Dir : constant String := Project_Dir & "/src";
         begin
            if not Exists (Src_Dir) then
               Create_Directory (Src_Dir);
            end if;

            --  Generate contract files
            Generate_Contract_Spec (Project_Name, Options.Template, Src_Dir);
            Files_Created := Files_Created + 1;

            Generate_Contract_Body (Project_Name, Options.Template, Src_Dir);
            Files_Created := Files_Created + 1;
         end;

         --  Create tests directory
         declare
            Test_Dir : constant String := Project_Dir & "/tests";
         begin
            if not Exists (Test_Dir) then
               Create_Directory (Test_Dir);
            end if;

            Generate_Test_File (Project_Name, Test_Dir);
            Files_Created := Files_Created + 1;
         end;

         --  Use init command to generate GPR and configuration
         declare
            Init_Success : Boolean;
         begin
            Khepri_CLI_Init.Create_Project_Structure (
               Project_Name => Project_Name,
               Output_Dir   => Output_Dir,
               Success      => Init_Success
            );

            if Init_Success then
               Files_Created := Files_Created + 2;  --  .gpr + khepri.toml
            end if;
         end;

         --  Git init if requested
         if Options.Git_Init then
            declare
               use Ada.Directories;
               Git_Dir : constant String := Project_Dir & "/.git";
            begin
               if not Exists (Git_Dir) then
                  --  Would execute: git init
                  null;
               end if;
            end;
         end if;

         --  Success
         Result.Success := True;
         Result.Project_Path (1 .. Project_Dir'Length) := Project_Dir;
         Result.Path_Length := Project_Dir'Length;
         Result.Files_Created := Files_Created;

         Output.Status := Result_Success;
         Output.Exit_Code := 0;
         declare
            Msg : constant String :=
               "Created " & Project_Name & " with " &
               Natural'Image (Files_Created) & " files from " &
               Get_Template_Description (Options.Template) & " template";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;

      exception
         when E : others =>
            Result.Success := False;
            declare
               Msg : constant String := "Error creating project structure";
            begin
               Result.Error_Message (1 .. Msg'Length) := Msg;
               Result.Error_Length := Msg'Length;
               Output.Status := Result_Error;
               Output.Exit_Code := 1;
               Output.Message (1 .. Msg'Length) := Msg;
               Output.Message_Length := Msg'Length;
            end;
      end;
   end Execute_New;

end Khepri_CLI_New;
