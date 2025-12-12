-------------------------------------------------------------------------------
--  Privacy_State_Persistence: SHIELD Layer State Storage
--
--  This package persists encrypted private state entries from the SHIELD
--  privacy layer. Each entry is keyed by (contract_address, entry_key) and
--  contains encrypted ciphertext that can only be decrypted by the owner.
--
--  File Format (privacy_state.anubis):
--  +------------------+------------------+------------------+
--  | Magic (8 bytes)  | Version (4 bytes)| Entry_Count (4)  |
--  +------------------+------------------+------------------+
--  | Private Entry 1 (variable size)                       |
--  +-------------------------------------------------------+
--  | Private Entry N (variable size)                       |
--  +------------------+------------------+------------------+
--  | State Root Hash  | File Hash        | EOF Magic        |
--  | (32 bytes)       | (32 bytes)       | (8 bytes)        |
--  +------------------+------------------+------------------+
--
--  Private Entry Format:
--  +------------------+------------------+------------------+
--  | Contract ID (32) | Entry Key (32)   | CT_Length (4)    |
--  +------------------+------------------+------------------+
--  | Ciphertext (CT_Length bytes, padded to 4-byte boundary)|
--  +-------------------------------------------------------+
--  | Encapsulated (1568 bytes ML-KEM ciphertext)           |
--  +-------------------------------------------------------+
--  | Nonce (12 bytes) | Tag (16 bytes)   | Commitment (64)  |
--  +------------------+------------------+------------------+
--
--  Security:
--  - Private entries remain encrypted at rest
--  - Only the owner with the ML-KEM secret key can decrypt
--  - Commitment allows ZK proofs without revealing content
--  - Atomic writes prevent corruption
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Shield;  use Anubis_Shield;
with State_Persistence;

package Privacy_State_Persistence with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Magic number for privacy state file
   Privacy_Magic_Header : constant String := "ANUBPRIV";  -- 8 bytes
   Privacy_Magic_Footer : constant String := "ENDPRIV1";  -- 8 bytes

   --  Format version
   Privacy_Format_Version : constant := 1;

   --  Maximum stored private entries (devnet limit)
   Max_Private_Entries : constant := 256;
   subtype Private_Entry_Index is Natural range 0 .. Max_Private_Entries - 1;

   ---------------------------------------------------------------------------
   --  Stored Private Entry
   ---------------------------------------------------------------------------

   --  A stored private entry with its key and commitment
   type Stored_Private_Entry is record
      Is_Valid       : Boolean;
      Contract_ID    : Contract_Address;
      Entry_Key      : Hash256;
      Priv_Entry     : Private_Entry;
      Commitment     : Entry_Commitment;
   end record;

   --  Storage array for private entries
   type Private_Entry_Storage is array (Private_Entry_Index) of Stored_Private_Entry;

   --  Privacy state registry
   type Privacy_State is record
      Entries        : Private_Entry_Storage;
      Entry_Count    : Natural;
      Is_Initialized : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize empty privacy state
   procedure Initialize (State : out Privacy_State) with
      Global => null,
      Post   => State.Is_Initialized and State.Entry_Count = 0;

   ---------------------------------------------------------------------------
   --  Entry Operations
   ---------------------------------------------------------------------------

   --  Store a private entry
   procedure Store_Private_Entry (
      State       : in out Privacy_State;
      Contract_ID : in     Contract_Address;
      Key         : in     Hash256;
      Priv_Entry  : in     Private_Entry;
      Commitment  : in     Entry_Commitment;
      Index       : out    Private_Entry_Index;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => State.Is_Initialized and State.Entry_Count < Max_Private_Entries,
      Post   => (if Success then State.Entry_Count >= State.Entry_Count'Old);

   --  Find a private entry by contract and key
   procedure Find_Private_Entry (
      State       : in     Privacy_State;
      Contract_ID : in     Contract_Address;
      Key         : in     Hash256;
      Index       : out    Private_Entry_Index;
      Found       : out    Boolean
   ) with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Delete a private entry
   procedure Delete_Private_Entry (
      State       : in Out Privacy_State;
      Contract_ID : in     Contract_Address;
      Key         : in     Hash256;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Check if entry exists
   function Entry_Exists (
      State       : Privacy_State;
      Contract_ID : Contract_Address;
      Key         : Hash256
   ) return Boolean with
      Global => null,
      Pre    => State.Is_Initialized;

   ---------------------------------------------------------------------------
   --  Persistence Operations
   ---------------------------------------------------------------------------

   --  Save privacy state to file
   procedure Save_Privacy_State (
      State     : in     Privacy_State;
      File_Path : in     String;
      Path_Len  : in     Natural;
      Result    : out    State_Persistence.Persist_Result
   ) with
      Global => null,
      Pre    => Path_Len > 0 and Path_Len <= State_Persistence.Max_Path_Len and
                State.Is_Initialized;

   --  Load privacy state from file
   procedure Load_Privacy_State (
      File_Path : in     String;
      Path_Len  : in     Natural;
      State     : out    Privacy_State;
      Result    : out    State_Persistence.Persist_Result
   ) with
      Global => null,
      Pre    => Path_Len > 0 and Path_Len <= State_Persistence.Max_Path_Len,
      Post   => (if Result.Success then State.Is_Initialized);

   ---------------------------------------------------------------------------
   --  State Root Computation
   ---------------------------------------------------------------------------

   --  Compute privacy state root (hash of all commitments)
   procedure Compute_Privacy_Root (
      State      : in     Privacy_State;
      State_Root : out    Hash256
   ) with
      Global => null,
      Pre    => State.Is_Initialized;

   --  Check if privacy state file is valid
   function Privacy_File_Valid (
      File_Path : String;
      Path_Len  : Natural
   ) return Boolean with
      Global => null,
      Pre    => Path_Len > 0 and Path_Len <= State_Persistence.Max_Path_Len;

end Privacy_State_Persistence;
