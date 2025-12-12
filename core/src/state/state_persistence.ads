pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Node_Contract_Registry; use Node_Contract_Registry;

--  State_Persistence: Disk Storage for AnubisVM State
--
--  This package provides atomic file-based persistence for the contract
--  registry and node state. Uses a simple binary format with integrity
--  checking via SHA3-256 hashes.
--
--  File Format (registry.anubis):
--  +------------------+------------------+------------------+
--  | Magic (8 bytes)  | Version (4 bytes)| Count (4 bytes)  |
--  +------------------+------------------+------------------+
--  | Contract Entry 1 (variable size)                      |
--  +-------------------------------------------------------+
--  | Contract Entry N (variable size)                      |
--  +------------------+------------------+------------------+
--  | State Root Hash  | File Hash        | EOF Magic        |
--  | (32 bytes)       | (32 bytes)       | (8 bytes)        |
--  +------------------+------------------+------------------+
--
--  Contract Entry Format:
--  +------------------+------------------+------------------+
--  | Contract ID      | Code Hash        | Name (64 bytes)  |
--  | (32 bytes)       | (32 bytes)       | + Name_Len (4)   |
--  +------------------+------------------+------------------+
--  | Version (12)     | Cert (4 bytes)   | Code_Size (4)    |
--  +------------------+------------------+------------------+
--  | Code (Code_Size bytes, padded to 4-byte boundary)     |
--  +-------------------------------------------------------+
--
--  Security:
--  - Atomic writes via temp file + rename
--  - File hash covers all data before it
--  - Magic numbers prevent accidental corruption
--  - Version field for format evolution
--
--  References:
--  - POSIX atomic rename semantics
--  - Write-ahead logging concepts

package State_Persistence with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Magic numbers for file format validation
   File_Magic_Header : constant String := "ANUBIS01";  -- 8 bytes
   File_Magic_Footer : constant String := "ENDANUB1";  -- 8 bytes

   --  Format version (increment on breaking changes)
   Format_Version : constant := 1;

   --  Maximum path length
   Max_Path_Len : constant := 256;
   subtype Path_Index is Natural range 1 .. Max_Path_Len;
   subtype Path_String is String (Path_Index);

   ---------------------------------------------------------------------------
   --  Persistence Result Types
   ---------------------------------------------------------------------------

   type Persist_Error is (
      Persist_OK,
      Persist_File_Error,        -- Could not open/create file
      Persist_Write_Error,       -- Write failed
      Persist_Read_Error,        -- Read failed
      Persist_Format_Error,      -- Invalid file format
      Persist_Version_Error,     -- Unsupported version
      Persist_Hash_Mismatch,     -- Integrity check failed
      Persist_Magic_Error,       -- Magic number mismatch
      Persist_Size_Error         -- Unexpected file size
   );

   type Persist_Result is record
      Success      : Boolean;
      Error        : Persist_Error;
      Bytes_Written : Natural;
      Bytes_Read    : Natural;
   end record;

   Success_Result : constant Persist_Result := (
      Success       => True,
      Error         => Persist_OK,
      Bytes_Written => 0,
      Bytes_Read    => 0
   );

   ---------------------------------------------------------------------------
   --  File Header Types
   ---------------------------------------------------------------------------

   --  File header structure (fixed size: 16 bytes)
   type File_Header is record
      Magic         : String (1 .. 8);     -- "ANUBIS01"
      Version       : Natural;              -- Format version
      Contract_Count : Natural;             -- Number of contracts
   end record;

   --  File footer structure (fixed size: 72 bytes)
   type File_Footer is record
      State_Root    : Hash256;              -- Merkle root of state
      File_Hash     : Hash256;              -- SHA3-256 of file content before footer
      Magic         : String (1 .. 8);      -- "ENDANUB1"
   end record;

   ---------------------------------------------------------------------------
   --  Persistence Operations
   ---------------------------------------------------------------------------

   --  Save registry to file
   --  Uses atomic write: writes to temp file, then renames
   procedure Save_Registry (
      Registry  : in     Registry_State;
      File_Path : in     String;
      Path_Len  : in     Natural;
      Result    : out    Persist_Result
   ) with
      Global => null,
      Pre => Path_Len > 0 and Path_Len <= Max_Path_Len and
             Registry.Is_Initialized;

   --  Load registry from file
   --  Validates magic numbers and hash before accepting data
   procedure Load_Registry (
      File_Path : in     String;
      Path_Len  : in     Natural;
      Registry  : out    Registry_State;
      Result    : out    Persist_Result
   ) with
      Global => null,
      Pre => Path_Len > 0 and Path_Len <= Max_Path_Len,
      Post => (if Result.Success then Registry.Is_Initialized);

   ---------------------------------------------------------------------------
   --  Block State Persistence
   ---------------------------------------------------------------------------

   --  Block checkpoint for recovery
   type Block_Checkpoint is record
      Block_Number  : U256;
      Block_Hash    : Hash256;
      State_Root    : Hash256;
      Timestamp     : U256;
   end record;

   --  Save block checkpoint
   procedure Save_Checkpoint (
      Checkpoint : in     Block_Checkpoint;
      File_Path  : in     String;
      Path_Len   : in     Natural;
      Result     : out    Persist_Result
   ) with
      Global => null,
      Pre => Path_Len > 0 and Path_Len <= Max_Path_Len;

   --  Load block checkpoint
   procedure Load_Checkpoint (
      File_Path  : in     String;
      Path_Len   : in     Natural;
      Checkpoint : out    Block_Checkpoint;
      Result     : out    Persist_Result
   ) with
      Global => null,
      Pre => Path_Len > 0 and Path_Len <= Max_Path_Len;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Compute state root hash from registry
   procedure Compute_State_Root (
      Registry   : in     Registry_State;
      State_Root : out    Hash256
   ) with
      Global => null,
      Pre => Registry.Is_Initialized;

   --  Check if a registry file exists and is valid
   function Registry_File_Valid (
      File_Path : String;
      Path_Len  : Natural
   ) return Boolean with
      Global => null,
      Pre => Path_Len > 0 and Path_Len <= Max_Path_Len;

end State_Persistence;
