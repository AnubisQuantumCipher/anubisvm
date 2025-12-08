-------------------------------------------------------------------------------
--  KHEPRI CLI - Main Package Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Khepri_CLI with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Command Parsing
   ---------------------------------------------------------------------------

   function Parse_Command (Arg : String) return CLI_Command is
   begin
      if Arg'Length = 0 then
         return Cmd_Unknown;
      end if;

      --  Match command strings
      if Arg = "init" then
         return Cmd_Init;
      elsif Arg = "build" then
         return Cmd_Build;
      elsif Arg = "prove" then
         return Cmd_Prove;
      elsif Arg = "certify" then
         return Cmd_Certify;
      elsif Arg = "deploy" then
         return Cmd_Deploy;
      elsif Arg = "test" then
         return Cmd_Test;
      elsif Arg = "help" or Arg = "--help" or Arg = "-h" then
         return Cmd_Help;
      elsif Arg = "version" or Arg = "--version" or Arg = "-v" then
         return Cmd_Version;
      else
         return Cmd_Unknown;
      end if;
   end Parse_Command;

   function Parse_Cert_Level (Arg : String) return Cert_Target is
   begin
      if Arg = "bronze" or Arg = "Bronze" or Arg = "BRONZE" then
         return Cert_Bronze;
      elsif Arg = "silver" or Arg = "Silver" or Arg = "SILVER" then
         return Cert_Silver;
      elsif Arg = "gold" or Arg = "Gold" or Arg = "GOLD" then
         return Cert_Gold;
      elsif Arg = "platinum" or Arg = "Platinum" or Arg = "PLATINUM" then
         return Cert_Platinum;
      else
         return Cert_Bronze;  -- Default
      end if;
   end Parse_Cert_Level;

   function Parse_Proof_Level (Arg : String) return Proof_Target is
   begin
      if Arg = "flow" or Arg = "Flow" or Arg = "FLOW" then
         return Proof_Flow;
      elsif Arg = "bronze" or Arg = "Bronze" or Arg = "BRONZE" then
         return Proof_Bronze;
      elsif Arg = "silver" or Arg = "Silver" or Arg = "SILVER" then
         return Proof_Silver;
      elsif Arg = "gold" or Arg = "Gold" or Arg = "GOLD" then
         return Proof_Gold;
      elsif Arg = "max" or Arg = "Max" or Arg = "MAX" then
         return Proof_Max;
      else
         return Proof_Silver;  -- Default
      end if;
   end Parse_Proof_Level;

   ---------------------------------------------------------------------------
   --  Option Parsing
   ---------------------------------------------------------------------------

   procedure Parse_Option (
      Arg    : String;
      Key    : out String;
      Value  : out String;
      Valid  : out Boolean
   ) is
      Eq_Pos : Natural := 0;
   begin
      Key := (others => ' ');
      Value := (others => ' ');
      Valid := False;

      --  Check for --key=value format
      if Arg'Length < 3 or else Arg (Arg'First .. Arg'First + 1) /= "--" then
         return;
      end if;

      --  Find equals sign
      for I in Arg'First + 2 .. Arg'Last loop
         if Arg (I) = '=' then
            Eq_Pos := I;
            exit;
         end if;
      end loop;

      if Eq_Pos = 0 then
         --  Boolean flag (no value)
         declare
            Key_Len : constant Natural := Arg'Length - 2;
         begin
            if Key_Len <= Key'Length then
               Key (Key'First .. Key'First + Key_Len - 1) :=
                  Arg (Arg'First + 2 .. Arg'Last);
            end if;
         end;
         Valid := True;
      else
         --  Key=value pair
         declare
            Key_Len : constant Natural := Eq_Pos - Arg'First - 2;
            Val_Len : constant Natural := Arg'Last - Eq_Pos;
         begin
            if Key_Len > 0 and Key_Len <= Key'Length then
               Key (Key'First .. Key'First + Key_Len - 1) :=
                  Arg (Arg'First + 2 .. Eq_Pos - 1);
            end if;
            if Val_Len > 0 and Val_Len <= Value'Length then
               Value (Value'First .. Value'First + Val_Len - 1) :=
                  Arg (Eq_Pos + 1 .. Arg'Last);
            end if;
         end;
         Valid := True;
      end if;
   end Parse_Option;

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------

   procedure Load_Config (
      Path    : String;
      Config  : out CLI_Config;
      Success : out Boolean
   ) is
   begin
      --  Initialize with defaults
      Config := Default_Config;
      Success := True;

      --  Would load from khepri.toml file
      --  For now, just use defaults
      if Path'Length > 0 then
         Config.Project_File (1 .. Path'Length) := Path;
      end if;
   end Load_Config;

   procedure Save_Config (
      Path    : String;
      Config  : CLI_Config;
      Success : out Boolean
   ) is
   begin
      --  Would save to khepri.toml file
      Success := Path'Length > 0;
   end Save_Config;

   ---------------------------------------------------------------------------
   --  Output Formatting
   ---------------------------------------------------------------------------

   procedure Format_Error (
      Message : String;
      Output  : out CLI_Output
   ) is
   begin
      Output.Status := Result_Error;
      Output.Exit_Code := 1;
      Output.Message_Length := Natural'Min (Message'Length, 1024);
      Output.Message (1 .. Output.Message_Length) :=
         Message (Message'First .. Message'First + Output.Message_Length - 1);
   end Format_Error;

   procedure Format_Success (
      Message : String;
      Output  : out CLI_Output
   ) is
   begin
      Output.Status := Result_Success;
      Output.Exit_Code := 0;
      Output.Message_Length := Natural'Min (Message'Length, 1024);
      Output.Message (1 .. Output.Message_Length) :=
         Message (Message'First .. Message'First + Output.Message_Length - 1);
   end Format_Success;

   procedure Format_Warning (
      Message : String;
      Output  : out CLI_Output
   ) is
   begin
      Output.Status := Result_Warning;
      Output.Exit_Code := 0;
      Output.Message_Length := Natural'Min (Message'Length, 1024);
      Output.Message (1 .. Output.Message_Length) :=
         Message (Message'First .. Message'First + Output.Message_Length - 1);
   end Format_Warning;

   ---------------------------------------------------------------------------
   --  Utilities
   ---------------------------------------------------------------------------

   function Trim (S : String) return String is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      --  Skip leading spaces
      while First <= Last and then S (First) = ' ' loop
         First := First + 1;
      end loop;

      --  Skip trailing spaces
      while Last >= First and then S (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      if First > Last then
         return "";
      else
         return S (First .. Last);
      end if;
   end Trim;

   function To_Lower (S : String) return String is
      Result : String := S;
   begin
      for I in Result'Range loop
         if Result (I) in 'A' .. 'Z' then
            Result (I) :=
               Character'Val (Character'Pos (Result (I)) + 32);
         end if;
      end loop;
      return Result;
   end To_Lower;

   function Contains (S : String; Sub : String) return Boolean is
   begin
      if Sub'Length > S'Length then
         return False;
      end if;

      for I in S'First .. S'Last - Sub'Length + 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in Sub'Range loop
               if S (I + J - Sub'First) /= Sub (J) then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               return True;
            end if;
         end;
      end loop;

      return False;
   end Contains;

end Khepri_CLI;
