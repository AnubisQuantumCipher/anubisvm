-------------------------------------------------------------------------------
--  KHEPRI CLI - Build Command Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Text_IO;
with GNAT.OS_Lib;

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
      GPRBuild_Path : constant String := "/usr/bin/gprbuild";
      Args : GNAT.OS_Lib.Argument_List (1 .. 10);
      Arg_Count : Natural := 0;
      Status : Integer;
      Success : Boolean;
   begin
      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message := (others => ' ');
      Output.Message_Length := 0;

      --  Build argument list
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'("-P");
      Arg_Count := Arg_Count + 1;
      Args (Arg_Count) := new String'(Trim (Options.GPR_File));

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
         Program_Name => GPRBuild_Path,
         Args => Args (1 .. Arg_Count),
         Success => Success
      );

      --  Free argument strings
      for I in 1 .. Arg_Count loop
         GNAT.OS_Lib.Free (Args (I));
      end loop;

      if Success then
         Output.Status := Result_Success;
         Output.Exit_Code := 0;
         declare
            Msg : constant String := "Build completed successfully";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      else
         declare
            Msg : constant String := "Build failed";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
      end if;

   exception
      when others =>
         declare
            Msg : constant String := "Build execution error";
         begin
            Output.Message (1 .. Msg'Length) := Msg;
            Output.Message_Length := Msg'Length;
         end;
   end Execute_Build;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Find_GPR_File (Directory : String) return String is
      Search : Ada.Directories.Search_Type;
      Item   : Ada.Directories.Directory_Entry_Type;
   begin
      Ada.Directories.Start_Search (
         Search,
         Directory,
         "*.gpr",
         (Ada.Directories.Ordinary_File => True, others => False)
      );

      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Item);
         Ada.Directories.End_Search (Search);
         return Ada.Directories.Full_Name (Item);
      end loop;

      Ada.Directories.End_Search (Search);
      return "";
   exception
      when others =>
         return "";
   end Find_GPR_File;

   function Get_Object_Dir (GPR_File : String) return String is
   begin
      --  Simple heuristic: object dir is usually "obj" relative to GPR
      declare
         Dir : constant String := Ada.Directories.Containing_Directory (GPR_File);
      begin
         return Dir & "/obj";
      end;
   end Get_Object_Dir;

   procedure Clean_Build (
      GPR_File : String;
      Success  : out Boolean
   ) is
      Obj_Dir : constant String := Get_Object_Dir (GPR_File);
   begin
      --  Remove object files
      if Ada.Directories.Exists (Obj_Dir) then
         declare
            Search : Ada.Directories.Search_Type;
            Item   : Ada.Directories.Directory_Entry_Type;
         begin
            Ada.Directories.Start_Search (
               Search,
               Obj_Dir,
               "*",
               (Ada.Directories.Ordinary_File => True, others => False)
            );

            while Ada.Directories.More_Entries (Search) loop
               Ada.Directories.Get_Next_Entry (Search, Item);
               Ada.Directories.Delete_File (Ada.Directories.Full_Name (Item));
            end loop;

            Ada.Directories.End_Search (Search);
         exception
            when others =>
               null;
         end;
      end if;

      Success := True;
   end Clean_Build;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

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

end Khepri_CLI_Build;
