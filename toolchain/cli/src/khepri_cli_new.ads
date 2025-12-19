-------------------------------------------------------------------------------
--  KHEPRI CLI - New Command (Project Creation with Templates)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Khepri_CLI; use Khepri_CLI;

package Khepri_CLI_New with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Template Types
   ---------------------------------------------------------------------------

   type Project_Template is (
      Template_Token,     --  ERC20-style fungible token
      Template_NFT,       --  ERC721-style NFT collection
      Template_Escrow,    --  Multi-party escrow contract
      Template_Oracle,    --  Price oracle with feeds
      Template_Staking,   --  Token staking with rewards
      Template_DAO,       --  Governance DAO
      Template_AMM,       --  Automated market maker
      Template_Minimal    --  Minimal contract scaffold
   );

   ---------------------------------------------------------------------------
   --  New Command Options
   ---------------------------------------------------------------------------

   type New_Options is record
      Project_Name   : String (1 .. 64);
      Name_Length    : Natural;
      Template       : Project_Template;
      Output_Dir     : String (1 .. 256);
      Dir_Length     : Natural;
      Git_Init       : Boolean;     --  Initialize git repository
      Open_Editor    : Boolean;     --  Open in editor after creation
      Verbose        : Boolean;
   end record;

   Default_New_Options : constant New_Options := (
      Project_Name   => (others => ' '),
      Name_Length    => 0,
      Template       => Template_Minimal,
      Output_Dir     => (others => ' '),
      Dir_Length     => 0,
      Git_Init       => True,
      Open_Editor    => False,
      Verbose        => False
   );

   ---------------------------------------------------------------------------
   --  New Command Result
   ---------------------------------------------------------------------------

   type New_Result is record
      Success        : Boolean;
      Project_Path   : String (1 .. 512);
      Path_Length    : Natural;
      Files_Created  : Natural;
      Error_Message  : String (1 .. 256);
      Error_Length   : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Command Execution
   ---------------------------------------------------------------------------

   --  Execute 'khepri new' command
   procedure Execute_New (
      Options  : New_Options;
      Result   : out New_Result;
      Output   : out CLI_Output
   ) with
      Global => null;

   --  Validate new command options
   procedure Validate_Options (
      Options : New_Options;
      Valid   : out Boolean;
      Error   : out String
   ) with
      Global => null,
      Pre    => Error'Length >= 256;

   ---------------------------------------------------------------------------
   --  Template Descriptions
   ---------------------------------------------------------------------------

   --  Get human-readable description of template
   function Get_Template_Description (
      Template : Project_Template
   ) return String with
      Global => null;

   --  Get recommended proof level for template
   function Get_Recommended_Proof_Level (
      Template : Project_Template
   ) return Proof_Level with
      Global => null;

end Khepri_CLI_New;
