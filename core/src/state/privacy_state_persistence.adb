-------------------------------------------------------------------------------
--  Privacy_State_Persistence: SHIELD Layer State Storage Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_SHA3;
with Anubis_Types;
with Interfaces; use Interfaces;
with State_File_IO;

package body Privacy_State_Persistence with
   SPARK_Mode => On
is

   --  Type conversion helpers
   subtype Local_Byte is Anubis_Types.Byte;

   function To_Local (B : Byte) return Local_Byte is
      (Local_Byte (B)) with Inline;

   function To_VM (B : Local_Byte) return Byte is
      (Byte (B)) with Inline;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Encode Natural to 4 bytes (little-endian)
   procedure Encode_Natural_LE (
      Value : in     Natural;
      Bytes : out    Anubis_Types.Byte_Array
   ) with
      Pre => Bytes'Length >= 4 and Bytes'First = 0
   is
      V : constant Unsigned_32 := Unsigned_32 (Value);
   begin
      Bytes (0) := Local_Byte (V and 16#FF#);
      Bytes (1) := Local_Byte (Shift_Right (V, 8) and 16#FF#);
      Bytes (2) := Local_Byte (Shift_Right (V, 16) and 16#FF#);
      Bytes (3) := Local_Byte (Shift_Right (V, 24) and 16#FF#);
   end Encode_Natural_LE;

   --  Decode 4 bytes to Natural (little-endian)
   function Decode_Natural_LE (
      Bytes : Anubis_Types.Byte_Array
   ) return Natural with
      Pre => Bytes'Length >= 4 and Bytes'First = 0
   is
      V : Unsigned_32;
   begin
      V := Unsigned_32 (Bytes (0)) or
           Shift_Left (Unsigned_32 (Bytes (1)), 8) or
           Shift_Left (Unsigned_32 (Bytes (2)), 16) or
           Shift_Left (Unsigned_32 (Bytes (3)), 24);
      return Natural (V);
   end Decode_Natural_LE;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Privacy_State) is
   begin
      State.Entry_Count := 0;
      State.Is_Initialized := True;
      for I in State.Entries'Range loop
         State.Entries (I).Is_Valid := False;
         State.Entries (I).Contract_ID := (others => 0);
         State.Entries (I).Entry_Key := (others => 0);
         State.Entries (I).Priv_Entry := (
            Ciphertext   => (others => 0),
            CT_Length    => 0,
            Encapsulated => (others => 0),
            Nonce        => (others => 0),
            Tag          => (others => 0)
         );
         State.Entries (I).Commitment := (Value => (others => 0));
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Entry Operations
   ---------------------------------------------------------------------------

   procedure Store_Private_Entry (
      State       : in Out Privacy_State;
      Contract_ID : in     Contract_Address;
      Key         : in     Hash256;
      Priv_Entry  : in     Private_Entry;
      Commitment  : in     Entry_Commitment;
      Index       : out    Private_Entry_Index;
      Success     : out    Boolean
   ) is
      Existing_Idx : Private_Entry_Index;
      Found        : Boolean;
   begin
      --  Check if entry already exists (update case)
      Find_Private_Entry (State, Contract_ID, Key, Existing_Idx, Found);

      if Found then
         --  Update existing entry
         State.Entries (Existing_Idx).Priv_Entry := Priv_Entry;
         State.Entries (Existing_Idx).Commitment := Commitment;
         Index := Existing_Idx;
         Success := True;
         return;
      end if;

      --  Find first free slot
      for I in State.Entries'Range loop
         if not State.Entries (I).Is_Valid then
            State.Entries (I).Is_Valid := True;
            State.Entries (I).Contract_ID := Contract_ID;
            State.Entries (I).Entry_Key := Key;
            State.Entries (I).Priv_Entry := Priv_Entry;
            State.Entries (I).Commitment := Commitment;
            State.Entry_Count := State.Entry_Count + 1;
            Index := I;
            Success := True;
            return;
         end if;
      end loop;

      --  No free slot found
      Index := 0;
      Success := False;
   end Store_Private_Entry;

   procedure Find_Private_Entry (
      State       : in     Privacy_State;
      Contract_ID : in     Contract_Address;
      Key         : in     Hash256;
      Index       : out    Private_Entry_Index;
      Found       : out    Boolean
   ) is
   begin
      Index := 0;
      Found := False;

      for I in State.Entries'Range loop
         if State.Entries (I).Is_Valid then
            declare
               Match : Boolean := True;
            begin
               --  Check contract ID
               for J in Contract_ID'Range loop
                  if State.Entries (I).Contract_ID (J) /= Contract_ID (J) then
                     Match := False;
                     exit;
                  end if;
               end loop;

               --  Check key if contract matched
               if Match then
                  for J in Key'Range loop
                     if State.Entries (I).Entry_Key (J) /= Key (J) then
                        Match := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if Match then
                  Index := I;
                  Found := True;
                  return;
               end if;
            end;
         end if;
      end loop;
   end Find_Private_Entry;

   procedure Delete_Private_Entry (
      State       : in Out Privacy_State;
      Contract_ID : in     Contract_Address;
      Key         : in     Hash256;
      Success     : out    Boolean
   ) is
      Index : Private_Entry_Index;
      Found : Boolean;
   begin
      Find_Private_Entry (State, Contract_ID, Key, Index, Found);

      if Found then
         --  Clear the entry (zeroize sensitive data)
         State.Entries (Index).Is_Valid := False;
         State.Entries (Index).Contract_ID := (others => 0);
         State.Entries (Index).Entry_Key := (others => 0);
         State.Entries (Index).Priv_Entry := (
            Ciphertext   => (others => 0),
            CT_Length    => 0,
            Encapsulated => (others => 0),
            Nonce        => (others => 0),
            Tag          => (others => 0)
         );
         State.Entries (Index).Commitment := (Value => (others => 0));

         if State.Entry_Count > 0 then
            State.Entry_Count := State.Entry_Count - 1;
         end if;

         Success := True;
      else
         Success := False;
      end if;
   end Delete_Private_Entry;

   function Entry_Exists (
      State       : Privacy_State;
      Contract_ID : Contract_Address;
      Key         : Hash256
   ) return Boolean is
      Index : Private_Entry_Index;
      Found : Boolean;
   begin
      Find_Private_Entry (State, Contract_ID, Key, Index, Found);
      return Found;
   end Entry_Exists;

   ---------------------------------------------------------------------------
   --  State Root Computation
   ---------------------------------------------------------------------------

   procedure Compute_Privacy_Root (
      State      : in     Privacy_State;
      State_Root : out    Hash256
   ) is
      --  Hash all commitments together
      Hash_Input_Max : constant := 4 + 64 * Max_Private_Entries;
      Hash_Input : Anubis_Types.Byte_Array (0 .. Hash_Input_Max - 1) := (others => 0);
      Hash_Pos : Natural := 0;
      Count_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      Hash_Output : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Encode entry count
      Encode_Natural_LE (State.Entry_Count, Count_Bytes);
      for I in 0 .. 3 loop
         Hash_Input (Hash_Pos) := Count_Bytes (I);
         Hash_Pos := Hash_Pos + 1;
      end loop;

      --  Add each commitment
      for I in State.Entries'Range loop
         if State.Entries (I).Is_Valid then
            for J in State.Entries (I).Commitment.Value'Range loop
               if Hash_Pos < Hash_Input_Max then
                  Hash_Input (Hash_Pos) := State.Entries (I).Commitment.Value (J);
                  Hash_Pos := Hash_Pos + 1;
               end if;
            end loop;
         end if;
      end loop;

      --  Compute SHA3-256
      if Hash_Pos > 0 then
         Anubis_SHA3.SHA3_256 (Hash_Input (0 .. Hash_Pos - 1), Hash_Output);
      else
         Hash_Output := (others => 0);
      end if;

      --  Convert to output
      for I in State_Root'Range loop
         State_Root (I) := To_VM (Hash_Output (I));
      end loop;
   end Compute_Privacy_Root;

   ---------------------------------------------------------------------------
   --  Save Privacy State Implementation
   ---------------------------------------------------------------------------

   procedure Save_Privacy_State (
      State     : in     Privacy_State;
      File_Path : in     String;
      Path_Len  : in     Natural;
      Result    : out    State_Persistence.Persist_Result
   )
      with SPARK_Mode => Off  -- File I/O operations
   is
      use State_File_IO;
      use State_Persistence;

      --  Calculate maximum buffer size
      --  Header: 16 bytes (magic 8 + version 4 + count 4)
      --  Per entry: 32 (contract) + 32 (key) + 4 (ct_len) + 4124 (ciphertext max)
      --             + 1568 (encapsulated) + 12 (nonce) + 16 (tag) + 64 (commitment)
      --  Footer: 72 bytes (state_root 32 + file_hash 32 + magic 8)
      Entry_Max_Size : constant := 32 + 32 + 4 + 4124 + 1568 + 12 + 16 + 64;
      Max_Buffer_Size : constant := 16 + Max_Private_Entries * Entry_Max_Size + 72;

      Buffer : Anubis_Types.Byte_Array (0 .. Max_Buffer_Size - 1) := (others => 0);
      Pos    : Natural := 0;
      State_Root_Val : Hash256;
      File_Hash : Anubis_SHA3.SHA3_256_Digest;
      IO_Result_Val : IO_Result;
      Actual_Path : constant String := File_Path (File_Path'First .. File_Path'First + Path_Len - 1);
   begin
      --  Compute privacy state root
      Compute_Privacy_Root (State, State_Root_Val);

      --  Write header magic
      for I in Privacy_Magic_Header'Range loop
         Buffer (Pos) := Local_Byte (Character'Pos (Privacy_Magic_Header (I)));
         Pos := Pos + 1;
      end loop;

      --  Write format version
      declare
         Version_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      begin
         Encode_Natural_LE (Privacy_Format_Version, Version_Bytes);
         for I in 0 .. 3 loop
            Buffer (Pos) := Version_Bytes (I);
            Pos := Pos + 1;
         end loop;
      end;

      --  Write entry count
      declare
         Count_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      begin
         Encode_Natural_LE (State.Entry_Count, Count_Bytes);
         for I in 0 .. 3 loop
            Buffer (Pos) := Count_Bytes (I);
            Pos := Pos + 1;
         end loop;
      end;

      --  Write each valid entry
      for I in State.Entries'Range loop
         if State.Entries (I).Is_Valid then
            declare
               E : Stored_Private_Entry renames State.Entries (I);
            begin
               --  Write contract ID (32 bytes)
               for J in E.Contract_ID'Range loop
                  Buffer (Pos) := To_Local (E.Contract_ID (J));
                  Pos := Pos + 1;
               end loop;

               --  Write entry key (32 bytes)
               for J in E.Entry_Key'Range loop
                  Buffer (Pos) := To_Local (E.Entry_Key (J));
                  Pos := Pos + 1;
               end loop;

               --  Write CT_Length (4 bytes)
               declare
                  Len_Bytes : Anubis_Types.Byte_Array (0 .. 3);
               begin
                  Encode_Natural_LE (E.Priv_Entry.CT_Length, Len_Bytes);
                  for J in 0 .. 3 loop
                     Buffer (Pos) := Len_Bytes (J);
                     Pos := Pos + 1;
                  end loop;
               end;

               --  Write ciphertext (CT_Length bytes, padded to 4-byte boundary)
               for J in 0 .. E.Priv_Entry.CT_Length - 1 loop
                  Buffer (Pos) := E.Priv_Entry.Ciphertext (J);
                  Pos := Pos + 1;
               end loop;

               --  Pad to 4-byte boundary
               while (Pos mod 4) /= 0 loop
                  Buffer (Pos) := 0;
                  Pos := Pos + 1;
               end loop;

               --  Write encapsulated key (1568 bytes)
               for J in E.Priv_Entry.Encapsulated'Range loop
                  Buffer (Pos) := E.Priv_Entry.Encapsulated (J);
                  Pos := Pos + 1;
               end loop;

               --  Write nonce (12 bytes)
               for J in E.Priv_Entry.Nonce'Range loop
                  Buffer (Pos) := E.Priv_Entry.Nonce (J);
                  Pos := Pos + 1;
               end loop;

               --  Write tag (16 bytes)
               for J in E.Priv_Entry.Tag'Range loop
                  Buffer (Pos) := E.Priv_Entry.Tag (J);
                  Pos := Pos + 1;
               end loop;

               --  Write commitment (64 bytes)
               for J in E.Commitment.Value'Range loop
                  Buffer (Pos) := E.Commitment.Value (J);
                  Pos := Pos + 1;
               end loop;
            end;
         end if;
      end loop;

      --  Compute file hash (everything before footer)
      declare
         Hash_Input : Anubis_Types.Byte_Array (0 .. Pos - 1);
      begin
         Hash_Input := Buffer (0 .. Pos - 1);
         Anubis_SHA3.SHA3_256 (Hash_Input, File_Hash);
      end;

      --  Write state root (32 bytes)
      for I in State_Root_Val'Range loop
         Buffer (Pos) := To_Local (State_Root_Val (I));
         Pos := Pos + 1;
      end loop;

      --  Write file hash (32 bytes)
      for I in File_Hash'Range loop
         Buffer (Pos) := File_Hash (I);
         Pos := Pos + 1;
      end loop;

      --  Write footer magic
      for I in Privacy_Magic_Footer'Range loop
         Buffer (Pos) := Local_Byte (Character'Pos (Privacy_Magic_Footer (I)));
         Pos := Pos + 1;
      end loop;

      --  Ensure directory exists
      State_File_IO.Ensure_Directory (Actual_Path, IO_Result_Val);
      if IO_Result_Val /= IO_OK then
         Result := (
            Success       => False,
            Error         => Persist_File_Error,
            Bytes_Written => 0,
            Bytes_Read    => 0
         );
         return;
      end if;

      --  Write atomically
      State_File_IO.Write_Atomic (Actual_Path, Buffer (0 .. Pos - 1), IO_Result_Val);
      if IO_Result_Val /= IO_OK then
         Result := (
            Success       => False,
            Error         => Persist_Write_Error,
            Bytes_Written => 0,
            Bytes_Read    => 0
         );
         return;
      end if;

      Result := (
         Success       => True,
         Error         => Persist_OK,
         Bytes_Written => Pos,
         Bytes_Read    => 0
      );
   end Save_Privacy_State;

   ---------------------------------------------------------------------------
   --  Load Privacy State Implementation
   ---------------------------------------------------------------------------

   procedure Load_Privacy_State (
      File_Path : in     String;
      Path_Len  : in     Natural;
      State     : out    Privacy_State;
      Result    : out    State_Persistence.Persist_Result
   )
      with SPARK_Mode => Off  -- File I/O operations
   is
      use State_File_IO;
      use State_Persistence;

      Entry_Max_Size : constant := 32 + 32 + 4 + 4124 + 1568 + 12 + 16 + 64;
      Max_Buffer_Size : constant := 16 + Max_Private_Entries * Entry_Max_Size + 72;
      Buffer : Anubis_Types.Byte_Array (0 .. Max_Buffer_Size - 1) := (others => 0);
      Bytes_Read : Natural := 0;
      Pos : Natural := 0;
      IO_Result_Val : IO_Result;
      Actual_Path : constant String := File_Path (File_Path'First .. File_Path'First + Path_Len - 1);
      Entry_Count : Natural;
      Stored_State_Root : Hash256;
      Stored_File_Hash : Hash256;
      Computed_State_Root : Hash256;
      Computed_File_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Initialize empty state first
      Initialize (State);

      --  Check if file exists
      if not State_File_IO.File_Exists (Actual_Path) then
         --  Return empty state if file doesn't exist
         Result := (
            Success       => True,
            Error         => Persist_OK,
            Bytes_Written => 0,
            Bytes_Read    => 0
         );
         return;
      end if;

      --  Read file
      State_File_IO.Read_File (Actual_Path, Buffer, Bytes_Read, IO_Result_Val);
      if IO_Result_Val /= IO_OK then
         Result := (
            Success       => False,
            Error         => Persist_Read_Error,
            Bytes_Written => 0,
            Bytes_Read    => 0
         );
         return;
      end if;

      if Bytes_Read < 88 then  -- Minimum: header (16) + footer (72)
         Result := (
            Success       => False,
            Error         => Persist_Size_Error,
            Bytes_Written => 0,
            Bytes_Read    => Bytes_Read
         );
         return;
      end if;

      --  Verify header magic
      for I in Privacy_Magic_Header'Range loop
         if Buffer (Pos) /= Local_Byte (Character'Pos (Privacy_Magic_Header (I))) then
            Result := (
               Success       => False,
               Error         => Persist_Magic_Error,
               Bytes_Written => 0,
               Bytes_Read    => Bytes_Read
            );
            return;
         end if;
         Pos := Pos + 1;
      end loop;

      --  Read and verify version
      declare
         Version_Bytes : Anubis_Types.Byte_Array (0 .. 3);
         File_Version : Natural;
      begin
         for I in 0 .. 3 loop
            Version_Bytes (I) := Buffer (Pos);
            Pos := Pos + 1;
         end loop;
         File_Version := Decode_Natural_LE (Version_Bytes);
         if File_Version /= Privacy_Format_Version then
            Result := (
               Success       => False,
               Error         => Persist_Version_Error,
               Bytes_Written => 0,
               Bytes_Read    => Bytes_Read
            );
            return;
         end if;
      end;

      --  Read entry count
      declare
         Count_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      begin
         for I in 0 .. 3 loop
            Count_Bytes (I) := Buffer (Pos);
            Pos := Pos + 1;
         end loop;
         Entry_Count := Decode_Natural_LE (Count_Bytes);
      end;

      if Entry_Count > Max_Private_Entries then
         Result := (
            Success       => False,
            Error         => Persist_Format_Error,
            Bytes_Written => 0,
            Bytes_Read    => Bytes_Read
         );
         return;
      end if;

      --  Read each entry
      for I in 0 .. Entry_Count - 1 loop
         declare
            Contract_ID : Contract_Address;
            Entry_Key   : Hash256;
            CT_Length   : Natural;
            Priv_Entry  : Private_Entry;
            Commitment  : Entry_Commitment;
            Store_Idx   : Private_Entry_Index;
            Store_OK    : Boolean;
         begin
            --  Read contract ID
            for J in Contract_ID'Range loop
               Contract_ID (J) := To_VM (Buffer (Pos));
               Pos := Pos + 1;
            end loop;

            --  Read entry key
            for J in Entry_Key'Range loop
               Entry_Key (J) := To_VM (Buffer (Pos));
               Pos := Pos + 1;
            end loop;

            --  Read CT_Length
            declare
               Len_Bytes : Anubis_Types.Byte_Array (0 .. 3);
            begin
               for J in 0 .. 3 loop
                  Len_Bytes (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;
               CT_Length := Decode_Natural_LE (Len_Bytes);
            end;

            if CT_Length > Anubis_Shield.Max_Entry_Size + Anubis_Shield.AEAD_Overhead then
               Result := (
                  Success       => False,
                  Error         => Persist_Format_Error,
                  Bytes_Written => 0,
                  Bytes_Read    => Bytes_Read
               );
               return;
            end if;

            --  Initialize private entry
            Priv_Entry := (
               Ciphertext   => (others => 0),
               CT_Length    => CT_Length,
               Encapsulated => (others => 0),
               Nonce        => (others => 0),
               Tag          => (others => 0)
            );

            --  Read ciphertext
            for J in 0 .. CT_Length - 1 loop
               Priv_Entry.Ciphertext (J) := Buffer (Pos);
               Pos := Pos + 1;
            end loop;

            --  Skip padding
            while (Pos mod 4) /= 0 loop
               Pos := Pos + 1;
            end loop;

            --  Read encapsulated key
            for J in Priv_Entry.Encapsulated'Range loop
               Priv_Entry.Encapsulated (J) := Buffer (Pos);
               Pos := Pos + 1;
            end loop;

            --  Read nonce
            for J in Priv_Entry.Nonce'Range loop
               Priv_Entry.Nonce (J) := Buffer (Pos);
               Pos := Pos + 1;
            end loop;

            --  Read tag
            for J in Priv_Entry.Tag'Range loop
               Priv_Entry.Tag (J) := Buffer (Pos);
               Pos := Pos + 1;
            end loop;

            --  Read commitment
            for J in Commitment.Value'Range loop
               Commitment.Value (J) := Buffer (Pos);
               Pos := Pos + 1;
            end loop;

            --  Store the entry
            Store_Private_Entry (
               State       => State,
               Contract_ID => Contract_ID,
               Key         => Entry_Key,
               Priv_Entry  => Priv_Entry,
               Commitment  => Commitment,
               Index       => Store_Idx,
               Success     => Store_OK
            );

            if not Store_OK then
               Result := (
                  Success       => False,
                  Error         => Persist_Format_Error,
                  Bytes_Written => 0,
                  Bytes_Read    => Bytes_Read
               );
               return;
            end if;
         end;
      end loop;

      --  Read footer
      --  State root (32 bytes)
      for I in Stored_State_Root'Range loop
         Stored_State_Root (I) := To_VM (Buffer (Pos));
         Pos := Pos + 1;
      end loop;

      --  File hash (32 bytes)
      for I in Stored_File_Hash'Range loop
         Stored_File_Hash (I) := To_VM (Buffer (Pos));
         Pos := Pos + 1;
      end loop;

      --  Verify footer magic
      for I in Privacy_Magic_Footer'Range loop
         if Buffer (Pos) /= Local_Byte (Character'Pos (Privacy_Magic_Footer (I))) then
            Result := (
               Success       => False,
               Error         => Persist_Magic_Error,
               Bytes_Written => 0,
               Bytes_Read    => Bytes_Read
            );
            return;
         end if;
         Pos := Pos + 1;
      end loop;

      --  Verify state root
      Compute_Privacy_Root (State, Computed_State_Root);
      for I in Computed_State_Root'Range loop
         if Computed_State_Root (I) /= Stored_State_Root (I) then
            Result := (
               Success       => False,
               Error         => Persist_Hash_Mismatch,
               Bytes_Written => 0,
               Bytes_Read    => Bytes_Read
            );
            return;
         end if;
      end loop;

      --  Verify file hash
      Anubis_SHA3.SHA3_256 (Buffer (0 .. Bytes_Read - 73), Computed_File_Hash);
      for I in Computed_File_Hash'Range loop
         if Computed_File_Hash (I) /= To_Local (Stored_File_Hash (I)) then
            Result := (
               Success       => False,
               Error         => Persist_Hash_Mismatch,
               Bytes_Written => 0,
               Bytes_Read    => Bytes_Read
            );
            return;
         end if;
      end loop;

      Result := (
         Success       => True,
         Error         => Persist_OK,
         Bytes_Written => 0,
         Bytes_Read    => Bytes_Read
      );
   end Load_Privacy_State;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Privacy_File_Valid (
      File_Path : String;
      Path_Len  : Natural
   ) return Boolean
      with SPARK_Mode => Off
   is
      use State_File_IO;
      Actual_Path : constant String := File_Path (File_Path'First .. File_Path'First + Path_Len - 1);
      Buffer : Anubis_Types.Byte_Array (0 .. 15);
      Bytes_Read : Natural;
      IO_Result_Val : IO_Result;
   begin
      if not State_File_IO.File_Exists (Actual_Path) then
         return False;
      end if;

      --  Read header
      State_File_IO.Read_File (Actual_Path, Buffer, Bytes_Read, IO_Result_Val);
      if IO_Result_Val /= IO_OK or Bytes_Read < 16 then
         return False;
      end if;

      --  Check magic
      for I in Privacy_Magic_Header'Range loop
         if Buffer (I - 1) /= Local_Byte (Character'Pos (Privacy_Magic_Header (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Privacy_File_Valid;

end Privacy_State_Persistence;
