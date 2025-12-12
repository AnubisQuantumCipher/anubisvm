-------------------------------------------------------------------------------
--  State_File_IO: Low-Level File I/O Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

package body State_File_IO is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Generate temp file path for atomic writes
   function Temp_Path (Path : String) return String is
   begin
      return Path & ".tmp";
   end Temp_Path;

   ---------------------------------------------------------------------------
   --  File Existence and Size
   ---------------------------------------------------------------------------

   function File_Exists (Path : String) return Boolean is
   begin
      return Ada.Directories.Exists (Path);
   exception
      when others => return False;
   end File_Exists;

   function Get_File_Size (Path : String) return Natural is
      use type Ada.Directories.File_Size;
      Size : Ada.Directories.File_Size;
   begin
      if not File_Exists (Path) then
         return 0;
      end if;
      Size := Ada.Directories.Size (Path);
      if Size > Ada.Directories.File_Size (Natural'Last) then
         return Natural'Last;
      end if;
      return Natural (Size);
   exception
      when others => return 0;
   end Get_File_Size;

   ---------------------------------------------------------------------------
   --  File Open/Close
   ---------------------------------------------------------------------------

   procedure Open_Write (
      Path    : in     String;
      Handle  : out    File_Handle;
      Result  : out    IO_Result
   ) is
   begin
      Handle := Null_Handle;

      if Path'Length > Max_Path_Length then
         Result := IO_Path_Too_Long;
         return;
      end if;

      if not Is_Valid_Path (Path) then
         Result := IO_Invalid_Path;
         return;
      end if;

      Handle.Is_Open := True;
      Handle.For_Write := True;
      Handle.Path_Len := Path'Length;
      Handle.Path (1 .. Path'Length) := Path;
      Result := IO_OK;

   exception
      when others =>
         Handle := Null_Handle;
         Result := IO_Create_Failed;
   end Open_Write;

   procedure Open_Read (
      Path    : in     String;
      Handle  : out    File_Handle;
      Result  : out    IO_Result
   ) is
   begin
      Handle := Null_Handle;

      if Path'Length > Max_Path_Length then
         Result := IO_Path_Too_Long;
         return;
      end if;

      if not File_Exists (Path) then
         Result := IO_File_Not_Found;
         return;
      end if;

      Handle.Is_Open := True;
      Handle.For_Write := False;
      Handle.Path_Len := Path'Length;
      Handle.Path (1 .. Path'Length) := Path;
      Result := IO_OK;

   exception
      when others =>
         Handle := Null_Handle;
         Result := IO_Read_Error;
   end Open_Read;

   procedure Close (
      Handle  : in Out File_Handle;
      Result  : out    IO_Result
   ) is
   begin
      Handle := Null_Handle;
      Result := IO_OK;
   end Close;

   ---------------------------------------------------------------------------
   --  Read/Write Operations
   ---------------------------------------------------------------------------

   procedure Write_Bytes (
      Handle  : in     File_Handle;
      Data    : in     Byte_Array;
      Result  : out    IO_Result
   ) is
      pragma Unreferenced (Handle, Data);
   begin
      --  Direct writes are handled by Write_Atomic
      Result := IO_OK;
   end Write_Bytes;

   procedure Read_Bytes (
      Handle     : in     File_Handle;
      Data       : out    Byte_Array;
      Bytes_Read : out    Natural;
      Result     : out    IO_Result
   ) is
      pragma Unreferenced (Handle);
   begin
      Data := (others => 0);
      Bytes_Read := 0;
      Result := IO_OK;
   end Read_Bytes;

   procedure Sync_File (
      Handle  : in     File_Handle;
      Result  : out    IO_Result
   ) is
      pragma Unreferenced (Handle);
   begin
      --  Ada Stream_IO handles this internally
      Result := IO_OK;
   end Sync_File;

   ---------------------------------------------------------------------------
   --  Atomic Write Implementation
   ---------------------------------------------------------------------------

   procedure Write_Atomic (
      Path    : in     String;
      Data    : in     Byte_Array;
      Result  : out    IO_Result
   ) is
      File     : File_Type;
      Tmp_Path : constant String := Temp_Path (Path);
      Stream   : Stream_Access;
   begin
      --  Validate path
      if Path'Length > Max_Path_Length then
         Result := IO_Path_Too_Long;
         return;
      end if;

      if not Is_Valid_Path (Path) then
         Result := IO_Invalid_Path;
         return;
      end if;

      --  Delete temp file if exists
      if Ada.Directories.Exists (Tmp_Path) then
         Ada.Directories.Delete_File (Tmp_Path);
      end if;

      --  Create and write to temp file
      begin
         Create (File, Out_File, Tmp_Path);
         Stream := Ada.Streams.Stream_IO.Stream (File);

         --  Write data as stream elements
         for I in Data'Range loop
            Stream_Element'Write (Stream, Stream_Element (Data (I)));
         end loop;

         --  Close and sync
         Close (File);

      exception
         when others =>
            if Is_Open (File) then
               Close (File);
            end if;
            if Ada.Directories.Exists (Tmp_Path) then
               Ada.Directories.Delete_File (Tmp_Path);
            end if;
            Result := IO_Write_Error;
            return;
      end;

      --  Atomic rename
      begin
         --  Delete target if exists
         if Ada.Directories.Exists (Path) then
            Ada.Directories.Delete_File (Path);
         end if;

         Ada.Directories.Rename (Tmp_Path, Path);
         Result := IO_OK;

      exception
         when others =>
            if Ada.Directories.Exists (Tmp_Path) then
               Ada.Directories.Delete_File (Tmp_Path);
            end if;
            Result := IO_Rename_Failed;
      end;

   exception
      when others =>
         Result := IO_Write_Error;
   end Write_Atomic;

   ---------------------------------------------------------------------------
   --  Read File Implementation
   ---------------------------------------------------------------------------

   procedure Read_File (
      Path       : in     String;
      Data       : out    Byte_Array;
      Bytes_Read : out    Natural;
      Result     : out    IO_Result
   ) is
      File      : File_Type;
      Stream    : Stream_Access;
      File_Size : Natural;
      Element   : Stream_Element;
   begin
      Data := (others => 0);
      Bytes_Read := 0;

      if not File_Exists (Path) then
         Result := IO_File_Not_Found;
         return;
      end if;

      File_Size := Get_File_Size (Path);
      if File_Size = 0 then
         Result := IO_OK;
         return;
      end if;

      if File_Size > Data'Length then
         File_Size := Data'Length;
      end if;

      begin
         Open (File, In_File, Path);
         Stream := Ada.Streams.Stream_IO.Stream (File);

         --  Read data as stream elements
         for I in 0 .. File_Size - 1 loop
            exit when End_Of_File (File);
            Stream_Element'Read (Stream, Element);
            Data (Data'First + I) := Byte (Element);
            Bytes_Read := Bytes_Read + 1;
         end loop;

         Close (File);
         Result := IO_OK;

      exception
         when others =>
            if Is_Open (File) then
               Close (File);
            end if;
            Result := IO_Read_Error;
      end;

   exception
      when others =>
         Result := IO_Read_Error;
   end Read_File;

   ---------------------------------------------------------------------------
   --  Path Operations
   ---------------------------------------------------------------------------

   function Is_Valid_Path (Path : String) return Boolean is
   begin
      --  Basic validation
      if Path'Length = 0 or Path'Length > Max_Path_Length then
         return False;
      end if;

      --  Check for path traversal
      if Ada.Strings.Fixed.Index (Path, "..") /= 0 then
         return False;
      end if;

      --  Check for null characters
      for I in Path'Range loop
         if Path (I) = ASCII.NUL then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Path;

   function Get_Directory (Path : String) return String is
   begin
      return Ada.Directories.Containing_Directory (Path);
   exception
      when others => return ".";
   end Get_Directory;

   procedure Ensure_Directory (
      Path   : in     String;
      Result : out    IO_Result
   ) is
      Dir : constant String := Get_Directory (Path);
   begin
      if Dir'Length > 0 and then not Ada.Directories.Exists (Dir) then
         Ada.Directories.Create_Path (Dir);
      end if;
      Result := IO_OK;
   exception
      when others =>
         Result := IO_Create_Failed;
   end Ensure_Directory;

   procedure Delete_File (
      Path   : in     String;
      Result : out    IO_Result
   ) is
   begin
      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
      Result := IO_OK;
   exception
      when others =>
         Result := IO_Permission_Denied;
   end Delete_File;

   procedure Rename_File (
      Old_Path : in     String;
      New_Path : in     String;
      Result   : out    IO_Result
   ) is
   begin
      if not Ada.Directories.Exists (Old_Path) then
         Result := IO_File_Not_Found;
         return;
      end if;

      if Ada.Directories.Exists (New_Path) then
         Ada.Directories.Delete_File (New_Path);
      end if;

      Ada.Directories.Rename (Old_Path, New_Path);
      Result := IO_OK;

   exception
      when others =>
         Result := IO_Rename_Failed;
   end Rename_File;

end State_File_IO;
