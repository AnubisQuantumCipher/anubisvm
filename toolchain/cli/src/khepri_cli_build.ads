-------------------------------------------------------------------------------
--  KHEPRI CLI - Build Command
--  Compiles SPARK contracts to native binaries
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Khepri_CLI; use Khepri_CLI;

package Khepri_CLI_Build with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Build Configuration
   ---------------------------------------------------------------------------

   type Build_Options is record
      Project_File   : String (1 .. 256);
      GPR_File       : String (1 .. 256);   -- Alias for Project_File
      Output_Dir     : String (1 .. 256);
      Target         : Build_Target;
      Optimization   : Natural range 0 .. 3;
      Debug_Symbols  : Boolean;
      Runtime        : Runtime_Profile;
      Link_Static    : Boolean;
      Warnings_Error : Boolean;
      Jobs           : Natural;             -- Parallel build jobs
      Verbose        : Boolean;             -- Verbose output
      Force_Rebuild  : Boolean;             -- Force full rebuild
   end record;

   type Runtime_Profile is (
      Profile_Native,      -- Full Ada runtime
      Profile_Embedded,    -- Ravenscar-style restricted
      Profile_Minimal,     -- Minimal runtime (no heap)
      Profile_Khepri       -- KHEPRI contract profile
   );

   Default_Build_Options : constant Build_Options := (
      Project_File   => (others => ' '),
      GPR_File       => (others => ' '),
      Output_Dir     => (others => ' '),
      Target         => Target_Native,
      Optimization   => 2,
      Debug_Symbols  => False,
      Runtime        => Profile_Khepri,
      Link_Static    => True,
      Warnings_Error => True,
      Jobs           => 0,        -- Auto-detect
      Verbose        => False,
      Force_Rebuild  => False
   );

   ---------------------------------------------------------------------------
   --  Build Execution
   ---------------------------------------------------------------------------

   --  Execute build
   procedure Execute_Build (
      Options  : Build_Options;
      Output   : out CLI_Output
   ) with
      Global => null;

   --  Check prerequisites (gprbuild, gnat)
   procedure Check_Prerequisites (
      Available : out Boolean;
      Message   : out String
   ) with
      Global => null;

   --  Parse project file
   procedure Parse_Project (
      Path     : String;
      Options  : out Build_Options;
      Success  : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  GPRbuild Invocation
   ---------------------------------------------------------------------------

   --  Build command line for gprbuild
   function Build_GPRbuild_Command (
      Options : Build_Options
   ) return String with
      Global => null;

   --  Run gprbuild and capture output
   procedure Run_GPRbuild (
      Options  : Build_Options;
      Exit_Code: out Integer;
      Output   : out String;
      Output_Len : out Natural
   ) with
      Global => null;

   --  Parse gprbuild output for errors
   procedure Parse_Build_Output (
      Output   : String;
      Errors   : out Natural;
      Warnings : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Binary Processing
   ---------------------------------------------------------------------------

   --  Post-process binary (strip, compress, sign)
   procedure Process_Binary (
      Binary_Path  : String;
      Options      : Build_Options;
      Success      : out Boolean
   ) with
      Global => null;

   --  Generate contract metadata
   procedure Generate_Metadata (
      Binary_Path  : String;
      Metadata     : out Contract_Metadata;
      Success      : out Boolean
   ) with
      Global => null;

   type Contract_Metadata is record
      Name           : String (1 .. 64);
      Version        : String (1 .. 32);
      Binary_Hash    : Byte_Array (0 .. 31);
      Source_Hash    : Byte_Array (0 .. 31);
      Binary_Size    : Natural;
      Entry_Points   : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Error Handling
   ---------------------------------------------------------------------------

   type Build_Error is (
      Error_None,
      Error_No_Project,
      Error_Parse_Failed,
      Error_Compile_Failed,
      Error_Link_Failed,
      Error_Missing_Tool,
      Error_Invalid_Config
   );

   function Error_Message (E : Build_Error) return String with
      Global => null;

end Khepri_CLI_Build;
