-------------------------------------------------------------------------------
--  KHEPRI CLI - Init Command
--  Creates new contract project with proper structure
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Khepri_CLI_Init with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Project Structure
   ---------------------------------------------------------------------------

   --  Create project directory structure:
   --    project_name/
   --    ├── khepri.toml          -- Project configuration
   --    ├── project_name.gpr     -- GPR project file
   --    ├── src/
   --    │   ├── contract.ads     -- Contract specification
   --    │   └── contract.adb     -- Contract implementation
   --    ├── tests/
   --    │   └── test_contract.adb
   --    ├── obj/                  -- Build output
   --    └── proofs/               -- Proof artifacts

   procedure Create_Project_Structure (
      Project_Name : String;
      Output_Dir   : String;
      Success      : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  File Generation
   ---------------------------------------------------------------------------

   --  Generate khepri.toml configuration file
   procedure Generate_Config_File (
      Project_Name : String;
      Output_Path  : String;
      Success      : out Boolean
   ) with
      Global => null;

   --  Generate GPR project file
   procedure Generate_Project_File (
      Project_Name : String;
      Output_Path  : String;
      Success      : out Boolean
   ) with
      Global => null;

   --  Generate contract template
   procedure Generate_Contract_Template (
      Contract_Name : String;
      Template_Type : Template_Kind;
      Output_Path   : String;
      Success       : out Boolean
   ) with
      Global => null;

   --  Generate test template
   procedure Generate_Test_Template (
      Contract_Name : String;
      Output_Path   : String;
      Success       : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Template Types
   ---------------------------------------------------------------------------

   type Template_Kind is (
      Token_Contract,     -- ERC20-style fungible token
      NFT_Contract,       -- Non-fungible token
      Escrow_Contract,    -- Multi-signature escrow
      Oracle_Contract,    -- Price feed oracle
      Empty_Contract      -- Minimal empty contract
   );

   ---------------------------------------------------------------------------
   --  Template Content
   ---------------------------------------------------------------------------

   --  Get template specification content
   function Get_Template_Spec (
      Template : Template_Kind;
      Name     : String
   ) return String with
      Global => null;

   --  Get template body content
   function Get_Template_Body (
      Template : Template_Kind;
      Name     : String
   ) return String with
      Global => null;

   ---------------------------------------------------------------------------
   --  Configuration Templates
   ---------------------------------------------------------------------------

   --  Default khepri.toml content
   Default_Config_Template : constant String :=
      "[project]" & ASCII.LF &
      "name = ""%NAME%""" & ASCII.LF &
      "version = ""0.1.0""" & ASCII.LF &
      "authors = []" & ASCII.LF &
      "" & ASCII.LF &
      "[build]" & ASCII.LF &
      "target = ""native""" & ASCII.LF &
      "optimization = 2" & ASCII.LF &
      "debug = false" & ASCII.LF &
      "" & ASCII.LF &
      "[proof]" & ASCII.LF &
      "level = ""silver""" & ASCII.LF &
      "timeout = 60" & ASCII.LF &
      "parallel = 0" & ASCII.LF &
      "" & ASCII.LF &
      "[certification]" & ASCII.LF &
      "target = ""gold""" & ASCII.LF &
      "" & ASCII.LF &
      "[deploy]" & ASCII.LF &
      "rpc = ""http://localhost:26657""" & ASCII.LF &
      "chain_id = 1" & ASCII.LF &
      "gas_limit = 1000000" & ASCII.LF;

   --  Default GPR template
   Default_GPR_Template : constant String :=
      "project %NAME% is" & ASCII.LF &
      "   for Source_Dirs use (""src"");" & ASCII.LF &
      "   for Object_Dir use ""obj"";" & ASCII.LF &
      "   for Main use (""%MAIN%"");" & ASCII.LF &
      "" & ASCII.LF &
      "   package Compiler is" & ASCII.LF &
      "      for Default_Switches (""Ada"") use" & ASCII.LF &
      "         (""-gnatwa"", ""-gnatyy"", ""-gnat2012"", ""-gnata"");" & ASCII.LF &
      "   end Compiler;" & ASCII.LF &
      "" & ASCII.LF &
      "   package Prove is" & ASCII.LF &
      "      for Proof_Switches (""Ada"") use (""--level=4"", ""--prover=all"");" & ASCII.LF &
      "   end Prove;" & ASCII.LF &
      "" & ASCII.LF &
      "end %NAME%;" & ASCII.LF;

end Khepri_CLI_Init;
