-------------------------------------------------------------------------------
--  KHEPRI CLI - Init Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Text_IO;
with Ada.Directories;

package body Khepri_CLI_Init with
   SPARK_Mode => Off  -- I/O operations not in SPARK
is
   ---------------------------------------------------------------------------
   --  Templates
   ---------------------------------------------------------------------------

   GPR_Template : constant String :=
      "project %NAME% is" & ASCII.LF &
      "   for Source_Dirs use (""src"");" & ASCII.LF &
      "   for Object_Dir use ""obj"";" & ASCII.LF &
      "   for Main use (""%NAME%.adb"");" & ASCII.LF &
      "" & ASCII.LF &
      "   package Compiler is" & ASCII.LF &
      "      for Default_Switches (""Ada"") use" & ASCII.LF &
      "         (""-gnatwa"", ""-gnatyg"", ""-gnat2022"", ""-gnata"");" & ASCII.LF &
      "   end Compiler;" & ASCII.LF &
      "" & ASCII.LF &
      "   package Prove is" & ASCII.LF &
      "      for Proof_Switches (""Ada"") use" & ASCII.LF &
      "         (""--level=2"", ""--timeout=60"");" & ASCII.LF &
      "   end Prove;" & ASCII.LF &
      "end %NAME%;" & ASCII.LF;

   ADS_Template : constant String :=
      "-------------------------------------------------------------------------------" & ASCII.LF &
      "--  %NAME% - KHEPRI Smart Contract" & ASCII.LF &
      "--  SPDX-License-Identifier: Apache-2.0" & ASCII.LF &
      "-------------------------------------------------------------------------------" & ASCII.LF &
      "" & ASCII.LF &
      "pragma SPARK_Mode (On);" & ASCII.LF &
      "" & ASCII.LF &
      "with Anubis_Types; use Anubis_Types;" & ASCII.LF &
      "" & ASCII.LF &
      "package %NAME% with" & ASCII.LF &
      "   SPARK_Mode => On" & ASCII.LF &
      "is" & ASCII.LF &
      "   --  Contract storage" & ASCII.LF &
      "   type Storage is record" & ASCII.LF &
      "      Owner   : Byte_Array (0 .. 31);" & ASCII.LF &
      "      Balance : Unsigned_64;" & ASCII.LF &
      "   end record;" & ASCII.LF &
      "" & ASCII.LF &
      "   --  Initialize contract" & ASCII.LF &
      "   procedure Initialize (" & ASCII.LF &
      "      State  : out Storage;" & ASCII.LF &
      "      Owner  : Byte_Array" & ASCII.LF &
      "   ) with" & ASCII.LF &
      "      Global => null," & ASCII.LF &
      "      Pre    => Owner'Length = 32;" & ASCII.LF &
      "" & ASCII.LF &
      "   --  Get balance" & ASCII.LF &
      "   function Get_Balance (State : Storage) return Unsigned_64 with" & ASCII.LF &
      "      Global => null;" & ASCII.LF &
      "" & ASCII.LF &
      "end %NAME%;" & ASCII.LF;

   ADB_Template : constant String :=
      "-------------------------------------------------------------------------------" & ASCII.LF &
      "--  %NAME% - Implementation" & ASCII.LF &
      "-------------------------------------------------------------------------------" & ASCII.LF &
      "" & ASCII.LF &
      "pragma SPARK_Mode (On);" & ASCII.LF &
      "" & ASCII.LF &
      "package body %NAME% with" & ASCII.LF &
      "   SPARK_Mode => On" & ASCII.LF &
      "is" & ASCII.LF &
      "   procedure Initialize (" & ASCII.LF &
      "      State  : out Storage;" & ASCII.LF &
      "      Owner  : Byte_Array" & ASCII.LF &
      "   ) is" & ASCII.LF &
      "   begin" & ASCII.LF &
      "      State.Owner := Owner;" & ASCII.LF &
      "      State.Balance := 0;" & ASCII.LF &
      "   end Initialize;" & ASCII.LF &
      "" & ASCII.LF &
      "   function Get_Balance (State : Storage) return Unsigned_64 is" & ASCII.LF &
      "   begin" & ASCII.LF &
      "      return State.Balance;" & ASCII.LF &
      "   end Get_Balance;" & ASCII.LF &
      "" & ASCII.LF &
      "end %NAME%;" & ASCII.LF;

   TOML_Template : constant String :=
      "[package]" & ASCII.LF &
      "name = ""%NAME%""" & ASCII.LF &
      "version = ""0.1.0""" & ASCII.LF &
      "authors = [""Anonymous""]" & ASCII.LF &
      "" & ASCII.LF &
      "[contract]" & ASCII.LF &
      "entrypoint = ""%NAME%""" & ASCII.LF &
      "target_level = ""gold""" & ASCII.LF &
      "" & ASCII.LF &
      "[build]" & ASCII.LF &
      "gpr_file = ""%NAME%.gpr""" & ASCII.LF &
      "" & ASCII.LF &
      "[prove]" & ASCII.LF &
      "level = ""silver""" & ASCII.LF &
      "timeout = 60" & ASCII.LF &
      "" & ASCII.LF &
      "[deploy]" & ASCII.LF &
      "chain_id = 1" & ASCII.LF &
      "gas_limit = 2000000" & ASCII.LF;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Replace_Name (Template : String; Name : String) return String is
      Result : String (1 .. Template'Length + Name'Length * 10);
      Src_Idx : Natural := Template'First;
      Dst_Idx : Natural := Result'First;
   begin
      while Src_Idx <= Template'Last loop
         if Src_Idx + 5 <= Template'Last and then
            Template (Src_Idx .. Src_Idx + 5) = "%NAME%"
         then
            --  Replace with name
            Result (Dst_Idx .. Dst_Idx + Name'Length - 1) := Name;
            Dst_Idx := Dst_Idx + Name'Length;
            Src_Idx := Src_Idx + 6;
         else
            Result (Dst_Idx) := Template (Src_Idx);
            Dst_Idx := Dst_Idx + 1;
            Src_Idx := Src_Idx + 1;
         end if;
      end loop;

      return Result (Result'First .. Dst_Idx - 1);
   end Replace_Name;

   procedure Write_File (Path : String; Content : String; Success : out Boolean) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Put (File, Content);
      Ada.Text_IO.Close (File);
      Success := True;
   exception
      when others =>
         Success := False;
   end Write_File;

   ---------------------------------------------------------------------------
   --  Project Creation
   ---------------------------------------------------------------------------

   procedure Create_Project_Structure (
      Project_Name : String;
      Output_Dir   : String;
      Success      : out Boolean
   ) is
      Project_Path : constant String :=
         (if Output_Dir = "." then Project_Name
          else Output_Dir & "/" & Project_Name);
      Write_OK : Boolean;
   begin
      Success := False;

      --  Create directories
      begin
         Ada.Directories.Create_Directory (Project_Path);
         Ada.Directories.Create_Directory (Project_Path & "/src");
         Ada.Directories.Create_Directory (Project_Path & "/obj");
         Ada.Directories.Create_Directory (Project_Path & "/tests");
      exception
         when others =>
            return;
      end;

      --  Write GPR file
      Write_File (
         Project_Path & "/" & Project_Name & ".gpr",
         Replace_Name (GPR_Template, Project_Name),
         Write_OK
      );
      if not Write_OK then
         return;
      end if;

      --  Write specification
      Write_File (
         Project_Path & "/src/" & Project_Name & ".ads",
         Replace_Name (ADS_Template, Project_Name),
         Write_OK
      );
      if not Write_OK then
         return;
      end if;

      --  Write body
      Write_File (
         Project_Path & "/src/" & Project_Name & ".adb",
         Replace_Name (ADB_Template, Project_Name),
         Write_OK
      );
      if not Write_OK then
         return;
      end if;

      --  Write khepri.toml
      Write_File (
         Project_Path & "/khepri.toml",
         Replace_Name (TOML_Template, Project_Name),
         Write_OK
      );
      if not Write_OK then
         return;
      end if;

      Success := True;
   end Create_Project_Structure;

   procedure Generate_Template (
      Template_Type : Template_Kind;
      Project_Name  : String;
      Output_Path   : String;
      Success       : out Boolean
   ) is
      Content : constant String := (case Template_Type is
         when Template_Token  => Replace_Name (ADS_Template, Project_Name),
         when Template_NFT    => Replace_Name (ADS_Template, Project_Name),
         when Template_Escrow => Replace_Name (ADS_Template, Project_Name),
         when Template_Empty  => Replace_Name (ADS_Template, Project_Name)
      );
      Write_OK : Boolean;
   begin
      Write_File (Output_Path, Content, Write_OK);
      Success := Write_OK;
   end Generate_Template;

end Khepri_CLI_Init;
