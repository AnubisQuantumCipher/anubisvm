-------------------------------------------------------------------------------
--  State_File_IO: Low-Level File I/O for State Persistence
--
--  This package provides POSIX-like file operations for state persistence.
--  It wraps Ada.Sequential_IO and Ada.Directories for atomic file operations.
--
--  Security:
--  - Atomic writes via temp file + rename
--  - File permissions set to 0600 (owner read/write only)
--  - All paths validated before use
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- File I/O is inherently non-SPARK

with Anubis_Types; use Anubis_Types;

package State_File_IO is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_Path_Length : constant := 4096;
   Max_File_Size   : constant := 64 * 1024 * 1024;  -- 64 MB max state file

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type IO_Result is (
      IO_OK,
      IO_File_Not_Found,
      IO_Permission_Denied,
      IO_Disk_Full,
      IO_Path_Too_Long,
      IO_Invalid_Path,
      IO_Read_Error,
      IO_Write_Error,
      IO_Rename_Failed,
      IO_Create_Failed,
      IO_Close_Failed
   );

   type File_Handle is private;

   Null_Handle : constant File_Handle;

   ---------------------------------------------------------------------------
   --  File Operations
   ---------------------------------------------------------------------------

   --  Check if file exists
   function File_Exists (Path : String) return Boolean;

   --  Get file size (returns 0 if not found)
   function Get_File_Size (Path : String) return Natural;

   --  Create/open file for writing (truncates if exists)
   procedure Open_Write (
      Path    : in     String;
      Handle  : out    File_Handle;
      Result  : out    IO_Result
   );

   --  Open file for reading
   procedure Open_Read (
      Path    : in     String;
      Handle  : out    File_Handle;
      Result  : out    IO_Result
   );

   --  Close file handle
   procedure Close (
      Handle  : in out File_Handle;
      Result  : out    IO_Result
   );

   --  Write bytes to file
   procedure Write_Bytes (
      Handle  : in     File_Handle;
      Data    : in     Byte_Array;
      Result  : out    IO_Result
   );

   --  Read bytes from file
   procedure Read_Bytes (
      Handle     : in     File_Handle;
      Data       : out    Byte_Array;
      Bytes_Read : out    Natural;
      Result     : out    IO_Result
   );

   --  Sync file to disk (fsync)
   procedure Sync_File (
      Handle  : in     File_Handle;
      Result  : out    IO_Result
   );

   ---------------------------------------------------------------------------
   --  Atomic Write Operations
   ---------------------------------------------------------------------------

   --  Write data atomically (write to temp, sync, rename)
   procedure Write_Atomic (
      Path    : in     String;
      Data    : in     Byte_Array;
      Result  : out    IO_Result
   );

   --  Read entire file into buffer
   procedure Read_File (
      Path       : in     String;
      Data       : out    Byte_Array;
      Bytes_Read : out    Natural;
      Result     : out    IO_Result
   );

   ---------------------------------------------------------------------------
   --  Path Operations
   ---------------------------------------------------------------------------

   --  Validate path is safe (no .., absolute, etc.)
   function Is_Valid_Path (Path : String) return Boolean;

   --  Get directory containing path
   function Get_Directory (Path : String) return String;

   --  Ensure directory exists (creates if needed)
   procedure Ensure_Directory (
      Path   : in     String;
      Result : out    IO_Result
   );

   --  Delete file if exists
   procedure Delete_File (
      Path   : in     String;
      Result : out    IO_Result
   );

   --  Rename file atomically
   procedure Rename_File (
      Old_Path : in     String;
      New_Path : in     String;
      Result   : out    IO_Result
   );

private

   type File_Handle is record
      Is_Open    : Boolean := False;
      For_Write  : Boolean := False;
      Path       : String (1 .. Max_Path_Length) := (others => ' ');
      Path_Len   : Natural := 0;
   end record;

   Null_Handle : constant File_Handle := (
      Is_Open   => False,
      For_Write => False,
      Path      => (others => ' '),
      Path_Len  => 0
   );

end State_File_IO;
