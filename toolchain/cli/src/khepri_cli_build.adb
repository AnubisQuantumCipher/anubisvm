-------------------------------------------------------------------------------
--  KHEPRI CLI - Build Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with GNAT.OS_Lib;
with Khepri_Compiler; use Khepri_Compiler;
with Khepri_Types; use Khepri_Types;

package body Khepri_CLI_Build with
   SPARK_Mode => Off  -- OS operations not in SPARK
is
   ---------------------------------------------------------------------------
   --  Build Execution
   ---------------------------------------------------------------------------

   procedure Execute_Build (
      Options  : Build_Options;
      Output   : out CLI_Output
   ) is
      Project_Path : constant String := Trim (Options.Project_File);
      Compiler_Config : Khepri_Compiler.Compiler_Config;
      Session : Compilation_Session;
      Result : Compilation_Result;
      Stats : Compilation_Stats;
      Success : Boolean;
   begin
      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Validate project file exists
      if Project_Path'Length = 0 or else
         (Project_Path'Length > 0 and then Project_Path (Project_Path'First) = ' ')
      then
         declare
            Msg : constant String := "Error: No project file specified";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
            Output.Exit_Code := 1;
            return;
         end;
      end if;

      if not Ada.Directories.Exists (Project_Path) then
         declare
            Msg : constant String := "Error: Project file not found: " & Project_Path;
            Len : constant Natural := Natural'Min (Msg'Length, Output.Message'Length);
         begin
            Output.Message (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
            Output.Message_Length := Len;
            Output.Exit_Code := 1;
            return;
         end;
      end if;

      --  Configure compiler based on build options
      Compiler_Config := Default_Config;

      --  Map build target to optimization level
      Compiler_Config.Opt_Level :=
         (case Options.Target is
            when Target_Debug    => Opt_Debug,
            when Target_Release  => Opt_Full,
            when Target_Native   =>
               (case Options.Optimization is
                  when 0      => Opt_None,
                  when 1      => Opt_Debug,
                  when 2      => Opt_Standard,
                  when 3      => Opt_Full,
                  when others => Opt_Standard),
            when Target_Embedded => Opt_Size);

      --  Set debug info
      Compiler_Config.Debug_Info := Options.Debug_Symbols;
      Compiler_Config.Verbose := Options.Verbose;

      --  Configure certification level based on target
      Compiler_Config.Target_Level :=
         (case Options.Target is
            when Target_Debug    => Target_Bronze,
            when Target_Native   => Target_Silver,
            when Target_Release  => Target_Gold,
            when Target_Embedded => Target_Silver);

      --  Enable proofs for non-debug builds
      Compiler_Config.Run_Proofs :=
         (Options.Target /= Target_Debug);

      --  Create compilation session
      Session := Create_Session (Compiler_Config);

      --  Add source files from project
      --  For now, we'll search for .ads and .adb files in the project directory
      declare
         Project_Dir : constant String :=
            Ada.Directories.Containing_Directory (Project_Path);
      begin
         --  Add main source file (derived from project name)
         --  This is simplified; real implementation would parse GPR
         Add_Source (Session, Project_Path, Success);

         if not Success then
            declare
               Msg : constant String :=
                  "Error: Failed to add source file";
            begin
               Output.Message (1 .. Msg'Length) := Msg;
               Output.Message_Length := Msg'Length;
               Output.Exit_Code := 1;
               return;
            end;
         end if;
      end;

      --  Compile the contract
      if Options.Verbose then
         Ada.Text_IO.Put_Line ("Compiling SPARK contract...");
      end if;

      Compile (Session, Result);

      --  Get compilation statistics
      Stats := Get_Stats (Session);

      --  Report results
      case Result is
         when Result_Success =>
            Output.Status := Result_Success;
            Output.Exit_Code := 0;
            declare
               Msg : constant String :=
                  "Build successful: " &
                  Natural'Image (Stats.Functions_Compiled) & " functions, " &
                  Natural'Image (Stats.Bytecode_Size) & " bytes bytecode";
               Len : constant Natural := Natural'Min (Msg'Length, Output.Message'Length);
            begin
               Output.Message (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
               Output.Message_Length := Len;
            end;

            if Options.Verbose then
               Ada.Text_IO.Put_Line ("  Files processed: " &
                  Natural'Image (Stats.Files_Processed));
               Ada.Text_IO.Put_Line ("  Lines compiled:  " &
                  Natural'Image (Stats.Lines_Compiled));
               Ada.Text_IO.Put_Line ("  Functions:       " &
                  Natural'Image (Stats.Functions_Compiled));
               Ada.Text_IO.Put_Line ("  Bytecode size:   " &
                  Natural'Image (Stats.Bytecode_Size) & " bytes");

               if Compiler_Config.Run_Proofs then
                  Ada.Text_IO.Put_Line ("  Proof VCs:       " &
                     Natural'Image (Stats.Proof_VCs_Passed) & "/" &
                     Natural'Image (Stats.Proof_VCs_Total) & " passed");
               end if;
            end if;

         when Result_Warnings =>
            Output.Status := Result_Success;
            Output.Exit_Code := 0;
            declare
               Msg : constant String := "Build succeeded with warnings";
            begin
               Output.Message (1 .. Msg'Length) := Msg;
               Output.Message_Length := Msg'Length;
            end;

         when Result_Errors =>
            Output.Status := Result_Error;
            Output.Exit_Code := 1;
            declare
               Msg : constant String := "Build failed: compilation errors";
            begin
               Output.Message (1 .. Msg'Length) := Msg;
               Output.Message_Length := Msg'Length;
            end;

            --  Report errors
            declare
               Error_Array : Khepri_Compiler.Error_Array;
               Error_Count : Natural;
            begin
               Get_Errors (Session, Error_Array, Error_Count);

               if Options.Verbose and Error_Count > 0 then
                  Ada.Text_IO.Put_Line ("Compilation errors:");
                  for I in 0 .. Natural'Min (Error_Count - 1, 10) loop
                     Ada.Text_IO.Put_Line ("  " &
                        Extract_Error_Message (Error_Array (Error_Index (I))));
                  end loop;
               end if;
            end;

         when Result_Proof_Failed =>
            Output.Status := Result_Error;
            Output.Exit_Code := 1;
            declare
               Msg : constant String :=
                  "Build failed: proof failures (" &
                  Natural'Image (Stats.Proof_VCs_Total - Stats.Proof_VCs_Passed) &
                  " VCs failed)";
               Len : constant Natural := Natural'Min (Msg'Length, Output.Message'Length);
            begin
               Output.Message (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
               Output.Message_Length := Len;
            end;

         when Result_Internal_Error =>
            Output.Status := Result_Error;
            Output.Exit_Code := 1;
            declare
               Msg : constant String := "Internal compiler error";
            begin
               Output.Message (1 .. Msg'Length) := Msg;
               Output.Message_Length := Msg'Length;
            end;
      end case;

   exception
      when E : others =>
         declare
            Msg : constant String := "Build execution error: " &
               Ada.Exceptions.Exception_Name (E);
            Len : constant Natural := Natural'Min (Msg'Length, Output.Message'Length);
         begin
            Output.Message (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
            Output.Message_Length := Len;
            Output.Status := Result_Error;
            Output.Exit_Code := 1;
         end;
   end Execute_Build;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Trim whitespace from string
   function Trim (S : String) return String is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      while First <= Last and then S (First) = ' ' loop
         First := First + 1;
      end loop;
      while Last >= First and then S (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if First > Last then
         return "";
      else
         return S (First .. Last);
      end if;
   end Trim;

   --  Extract error message from compiler error
   function Extract_Error_Message (Err : Compiler_Error) return String is
      File_Str : constant String := Extract_String (Err.File);
      Msg_Str  : constant String := Extract_String (Err.Message);
   begin
      if File_Str'Length > 0 then
         return File_Str & ":" &
            Natural'Image (Err.Line) & ":" &
            Natural'Image (Err.Column) & ": " &
            Msg_Str;
      else
         return Msg_Str;
      end if;
   end Extract_Error_Message;

   --  Extract string from Bounded_String (from Khepri_Types)
   function Extract_String (Source : Bounded_String) return String is
   begin
      if Source.Length = 0 then
         return "";
      end if;

      declare
         Result : String (1 .. Source.Length);
      begin
         for I in 1 .. Source.Length loop
            Result (I) := Source.Data (String_Index (I - 1));
         end loop;
         return Result;
      end;
   end Extract_String;

   ---------------------------------------------------------------------------
   --  Additional Procedures (Stubs)
   ---------------------------------------------------------------------------

   procedure Check_Prerequisites (
      Available : out Boolean;
      Message   : out String
   ) is
   begin
      --  Check for gprbuild
      declare
         GPRBuild : GNAT.OS_Lib.String_Access :=
            GNAT.OS_Lib.Locate_Exec_On_Path ("gprbuild");
         GNATProve : GNAT.OS_Lib.String_Access :=
            GNAT.OS_Lib.Locate_Exec_On_Path ("gnatprove");
      begin
         if GPRBuild = null then
            Available := False;
            declare
               Msg : constant String := "gprbuild not found in PATH";
            begin
               Message (1 .. Msg'Length) := Msg;
            end;
         elsif GNATProve = null then
            Available := False;
            declare
               Msg : constant String := "gnatprove not found in PATH";
            begin
               Message (1 .. Msg'Length) := Msg;
            end;
         else
            Available := True;
            declare
               Msg : constant String := "Build tools available";
            begin
               Message (1 .. Msg'Length) := Msg;
            end;
         end if;

         if GPRBuild /= null then
            GNAT.OS_Lib.Free (GPRBuild);
         end if;
         if GNATProve /= null then
            GNAT.OS_Lib.Free (GNATProve);
         end if;
      end;
   end Check_Prerequisites;

   procedure Parse_Project (
      Path     : String;
      Options  : out Build_Options;
      Success  : out Boolean
   ) is
   begin
      Options := Default_Build_Options;

      if not Ada.Directories.Exists (Path) then
         Success := False;
         return;
      end if;

      --  Simple implementation: just set the project file path
      declare
         Trimmed : constant String := Trim (Path);
         Len : constant Natural := Natural'Min (Trimmed'Length, Options.Project_File'Length);
      begin
         Options.Project_File := (others => ' ');
         Options.Project_File (1 .. Len) := Trimmed (Trimmed'First .. Trimmed'First + Len - 1);
         Options.GPR_File := Options.Project_File;
         Success := True;
      end;
   end Parse_Project;

   function Build_GPRbuild_Command (
      Options : Build_Options
   ) return String is
      Trimmed_Path : constant String := Trim (Options.Project_File);
   begin
      return "gprbuild -P " & Trimmed_Path &
         " -j" & Natural'Image (Options.Jobs) &
         (if Options.Verbose then " -v" else "") &
         (if Options.Force_Rebuild then " -f" else "");
   end Build_GPRbuild_Command;

   procedure Run_GPRbuild (
      Options    : Build_Options;
      Exit_Code  : out Integer;
      Output     : out String;
      Output_Len : out Natural
   ) is
      Args : GNAT.OS_Lib.Argument_List (1 .. 10);
      Arg_Count : Natural := 0;
      Success : Boolean;
      Trimmed_Path : constant String := Trim (Options.Project_File);
   begin
      Exit_Code := -1;
      Output := (others => ' ');
      Output_Len := 0;

      --  Build argument list
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'("-P");
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'(Trimmed_Path);

      if Options.Jobs > 0 then
         Arg_Count := Arg_Count + 1;
         Args (Arg_Count) := new String'("-j" & Natural'Image (Options.Jobs));
      end if;

      if Options.Verbose then
         Arg_Count := Arg_Count + 1;
         Args (Arg_Count) := new String'("-v");
      end if;

      if Options.Force_Rebuild then
         Arg_Count := Arg_Count + 1;
         Args (Arg_Count) := new String'("-f");
      end if;

      --  Execute gprbuild
      GNAT.OS_Lib.Spawn (
         Program_Name => "gprbuild",
         Args => Args (1 .. Arg_Count),
         Success => Success
      );

      Exit_Code := (if Success then 0 else 1);

      --  Free argument strings
      for I in 1 .. Arg_Count loop
         GNAT.OS_Lib.Free (Args (I));
      end loop;

      if Success then
         declare
            Msg : constant String := "Build completed successfully";
            Len : constant Natural := Natural'Min (Msg'Length, Output'Length);
         begin
            Output (Output'First .. Output'First + Len - 1) := Msg (Msg'First .. Msg'First + Len - 1);
            Output_Len := Len;
         end;
      else
         declare
            Msg : constant String := "Build failed";
            Len : constant Natural := Natural'Min (Msg'Length, Output'Length);
         begin
            Output (Output'First .. Output'First + Len - 1) := Msg (Msg'First .. Msg'First + Len - 1);
            Output_Len := Len;
         end;
      end if;
   end Run_GPRbuild;

   procedure Parse_Build_Output (
      Output   : String;
      Errors   : out Natural;
      Warnings : out Natural
   ) is
   begin
      Errors := 0;
      Warnings := 0;

      --  Simple parsing: count "error:" and "warning:" strings
      for I in Output'Range loop
         if I + 5 <= Output'Last and then
            Output (I .. I + 5) = "error:"
         then
            Errors := Errors + 1;
         elsif I + 7 <= Output'Last and then
            Output (I .. I + 7) = "warning:"
         then
            Warnings := Warnings + 1;
         end if;
      end loop;
   end Parse_Build_Output;

   procedure Process_Binary (
      Binary_Path  : String;
      Options      : Build_Options;
      Success      : out Boolean
   ) is
   begin
      --  Stub implementation
      Success := Ada.Directories.Exists (Binary_Path);
   end Process_Binary;

   procedure Generate_Metadata (
      Binary_Path  : String;
      Metadata     : out Contract_Metadata;
      Success      : out Boolean
   ) is
   begin
      Metadata := (
         Name         => (others => ' '),
         Version      => (others => ' '),
         Binary_Hash  => (others => 0),
         Source_Hash  => (others => 0),
         Binary_Size  => 0,
         Entry_Points => 0
      );

      if not Ada.Directories.Exists (Binary_Path) then
         Success := False;
         return;
      end if;

      Metadata.Binary_Size := Natural (Ada.Directories.Size (Binary_Path));
      Success := True;
   end Generate_Metadata;

   function Error_Message (E : Build_Error) return String is
   begin
      case E is
         when Error_None =>
            return "No error";
         when Error_No_Project =>
            return "No project file specified";
         when Error_Parse_Failed =>
            return "Failed to parse project file";
         when Error_Compile_Failed =>
            return "Compilation failed";
         when Error_Link_Failed =>
            return "Linking failed";
         when Error_Missing_Tool =>
            return "Required build tool not found";
         when Error_Invalid_Config =>
            return "Invalid build configuration";
      end case;
   end Error_Message;

end Khepri_CLI_Build;
