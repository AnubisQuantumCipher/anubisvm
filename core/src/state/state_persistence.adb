pragma SPARK_Mode (On);

with Anubis_Node_Types; use Anubis_Node_Types;
with Anubis_SHA3;
with Anubis_Types;
with Interfaces; use Interfaces;
with State_File_IO;

package body State_Persistence with
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

   --  Encode U256 to 32 bytes (little-endian)
   procedure Encode_U256_LE (
      Value : in     U256;
      Bytes : out    Anubis_Types.Byte_Array
   ) with
      Pre => Bytes'Length >= 32 and Bytes'First = 0
   is
   begin
      for Limb_Idx in 0 .. 3 loop
         for Byte_Idx in 0 .. 7 loop
            Bytes (Limb_Idx * 8 + Byte_Idx) := Local_Byte (
               Shift_Right (Value.Limbs (Limb_Idx), Byte_Idx * 8) and 16#FF#);
         end loop;
      end loop;
   end Encode_U256_LE;

   --  Decode 32 bytes to U256 (little-endian)
   function Decode_U256_LE (
      Bytes : Anubis_Types.Byte_Array
   ) return U256 with
      Pre => Bytes'Length >= 32 and Bytes'First = 0
   is
      Result : U256 := U256_Zero;
   begin
      for Limb_Idx in 0 .. 3 loop
         for Byte_Idx in 0 .. 7 loop
            Result.Limbs (Limb_Idx) := Result.Limbs (Limb_Idx) or
               Shift_Left (Word64 (Bytes (Limb_Idx * 8 + Byte_Idx)), Byte_Idx * 8);
         end loop;
      end loop;
      return Result;
   end Decode_U256_LE;

   ---------------------------------------------------------------------------
   --  State Root Computation
   ---------------------------------------------------------------------------

   procedure Compute_State_Root (
      Registry   : in     Registry_State;
      State_Root : out    Hash256
   ) is
      --  For simplicity, hash the count + all contract IDs
      --  A real implementation would use a Merkle Patricia Trie
      Hash_Input_Max : constant := 4 + 32 * Max_Stored_Contracts;
      Hash_Input : Anubis_Types.Byte_Array (0 .. Hash_Input_Max - 1) := (others => 0);
      Hash_Pos : Natural := 0;
      Count_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      Hash_Output : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Encode contract count
      Encode_Natural_LE (Registry.Contract_Count, Count_Bytes);
      for I in 0 .. 3 loop
         Hash_Input (Hash_Pos) := Count_Bytes (I);
         Hash_Pos := Hash_Pos + 1;
      end loop;

      --  Add each contract ID
      for I in 0 .. Registry.Contract_Count - 1 loop
         if Registry.Contracts (Stored_Contract_Index (I)).Is_Valid then
            for J in Contract_Address'Range loop
               if Hash_Pos < Hash_Input_Max then
                  Hash_Input (Hash_Pos) := To_Local (
                     Registry.Contracts (Stored_Contract_Index (I)).Contract_ID (J));
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
   end Compute_State_Root;

   ---------------------------------------------------------------------------
   --  Save Registry Implementation
   ---------------------------------------------------------------------------

   procedure Save_Registry (
      Registry  : in     Registry_State;
      File_Path : in     String;
      Path_Len  : in     Natural;
      Result    : out    Persist_Result
   )
      with SPARK_Mode => Off  -- File I/O operations
   is
      use State_File_IO;

      --  Calculate buffer size needed
      --  Header: 16 bytes (magic 8 + version 4 + count 4)
      --  Per contract: 32 (ID) + 32 (hash) + 68 (name) + 12 (version) + 4 (cert)
      --                + 4 (code_size) + code_size + padding
      --  Footer: 72 bytes (state_root 32 + file_hash 32 + magic 8)
      Max_Buffer_Size : constant := 16 + Max_Stored_Contracts * (152 + Node_Max_Code_Size) + 72;

      Buffer : Anubis_Types.Byte_Array (0 .. Max_Buffer_Size - 1) := (others => 0);
      Pos    : Natural := 0;
      State_Root_Val : Hash256;
      File_Hash_Input : Anubis_Types.Byte_Array (0 .. Max_Buffer_Size - 73);
      File_Hash : Anubis_SHA3.SHA3_256_Digest;
      IO_Result_Val : IO_Result;
      Actual_Path : constant String := File_Path (File_Path'First .. File_Path'First + Path_Len - 1);
   begin
      --  Compute state root
      Compute_State_Root (Registry, State_Root_Val);

      --  Write header magic
      for I in File_Magic_Header'Range loop
         Buffer (Pos) := Local_Byte (Character'Pos (File_Magic_Header (I)));
         Pos := Pos + 1;
      end loop;

      --  Write format version
      declare
         Version_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      begin
         Encode_Natural_LE (Format_Version, Version_Bytes);
         for I in 0 .. 3 loop
            Buffer (Pos) := Version_Bytes (I);
            Pos := Pos + 1;
         end loop;
      end;

      --  Write contract count
      declare
         Count_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      begin
         Encode_Natural_LE (Registry.Contract_Count, Count_Bytes);
         for I in 0 .. 3 loop
            Buffer (Pos) := Count_Bytes (I);
            Pos := Pos + 1;
         end loop;
      end;

      --  Write each contract entry
      for I in 0 .. Registry.Contract_Count - 1 loop
         if Registry.Contracts (Stored_Contract_Index (I)).Is_Valid then
            declare
               C : Stored_Contract renames Registry.Contracts (Stored_Contract_Index (I));
            begin
               --  Write contract ID (32 bytes)
               for J in C.Contract_ID'Range loop
                  Buffer (Pos) := To_Local (C.Contract_ID (J));
                  Pos := Pos + 1;
               end loop;

               --  Write code hash (32 bytes)
               for J in C.Code_Hash'Range loop
                  Buffer (Pos) := To_Local (C.Code_Hash (J));
                  Pos := Pos + 1;
               end loop;

               --  Write name (64 bytes + 4 bytes length)
               declare
                  Name_Len_Bytes : Anubis_Types.Byte_Array (0 .. 3);
               begin
                  Encode_Natural_LE (C.Manifest.Name_Len, Name_Len_Bytes);
                  for J in 0 .. 3 loop
                     Buffer (Pos) := Name_Len_Bytes (J);
                     Pos := Pos + 1;
                  end loop;
                  for J in 1 .. 64 loop
                     if J <= C.Manifest.Name_Len then
                        Buffer (Pos) := Local_Byte (Character'Pos (C.Manifest.Name (J)));
                     else
                        Buffer (Pos) := 0;
                     end if;
                     Pos := Pos + 1;
                  end loop;
               end;

               --  Write version (12 bytes: 3 x 4 bytes)
               declare
                  V : Anubis_Types.Byte_Array (0 .. 3);
               begin
                  Encode_Natural_LE (C.Manifest.Version_Major, V);
                  for J in 0 .. 3 loop
                     Buffer (Pos) := V (J);
                     Pos := Pos + 1;
                  end loop;
                  Encode_Natural_LE (C.Manifest.Version_Minor, V);
                  for J in 0 .. 3 loop
                     Buffer (Pos) := V (J);
                     Pos := Pos + 1;
                  end loop;
                  Encode_Natural_LE (C.Manifest.Version_Patch, V);
                  for J in 0 .. 3 loop
                     Buffer (Pos) := V (J);
                     Pos := Pos + 1;
                  end loop;
               end;

               --  Write certification level (4 bytes)
               declare
                  Cert_Bytes : Anubis_Types.Byte_Array (0 .. 3);
               begin
                  Encode_Natural_LE (Cert_Level'Pos (C.Manifest.Cert), Cert_Bytes);
                  for J in 0 .. 3 loop
                     Buffer (Pos) := Cert_Bytes (J);
                     Pos := Pos + 1;
                  end loop;
               end;

               --  Write code size (4 bytes)
               declare
                  Size_Bytes : Anubis_Types.Byte_Array (0 .. 3);
               begin
                  Encode_Natural_LE (C.Code_Size, Size_Bytes);
                  for J in 0 .. 3 loop
                     Buffer (Pos) := Size_Bytes (J);
                     Pos := Pos + 1;
                  end loop;
               end;

               --  Write code (Code_Size bytes)
               for J in 0 .. C.Code_Size - 1 loop
                  Buffer (Pos) := To_Local (C.Code (J));
                  Pos := Pos + 1;
               end loop;

               --  Pad to 4-byte boundary
               while (Pos mod 4) /= 0 loop
                  Buffer (Pos) := 0;
                  Pos := Pos + 1;
               end loop;
            end;
         end if;
      end loop;

      --  Compute file hash (everything before footer)
      File_Hash_Input (0 .. Pos - 1) := Buffer (0 .. Pos - 1);
      Anubis_SHA3.SHA3_256 (File_Hash_Input (0 .. Pos - 1), File_Hash);

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
      for I in File_Magic_Footer'Range loop
         Buffer (Pos) := Local_Byte (Character'Pos (File_Magic_Footer (I)));
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
   end Save_Registry;

   ---------------------------------------------------------------------------
   --  Load Registry Implementation
   ---------------------------------------------------------------------------

   procedure Load_Registry (
      File_Path : in     String;
      Path_Len  : in     Natural;
      Registry  : out    Registry_State;
      Result    : out    Persist_Result
   )
      with SPARK_Mode => Off  -- File I/O operations
   is
      use State_File_IO;

      Max_Buffer_Size : constant := 16 + Max_Stored_Contracts * (152 + Node_Max_Code_Size) + 72;
      Buffer : Anubis_Types.Byte_Array (0 .. Max_Buffer_Size - 1) := (others => 0);
      Bytes_Read : Natural := 0;
      Pos : Natural := 0;
      IO_Result_Val : IO_Result;
      Actual_Path : constant String := File_Path (File_Path'First .. File_Path'First + Path_Len - 1);
      Contract_Count : Natural;
      Stored_State_Root : Hash256;
      Stored_File_Hash : Hash256;
      Computed_State_Root : Hash256;
      Computed_File_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Initialize empty registry first
      Node_Contract_Registry.Initialize (Registry);

      --  Check if file exists
      if not State_File_IO.File_Exists (Actual_Path) then
         --  Return empty registry if file doesn't exist
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
      for I in File_Magic_Header'Range loop
         if Buffer (Pos) /= Local_Byte (Character'Pos (File_Magic_Header (I))) then
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
         if File_Version /= Format_Version then
            Result := (
               Success       => False,
               Error         => Persist_Version_Error,
               Bytes_Written => 0,
               Bytes_Read    => Bytes_Read
            );
            return;
         end if;
      end;

      --  Read contract count
      declare
         Count_Bytes : Anubis_Types.Byte_Array (0 .. 3);
      begin
         for I in 0 .. 3 loop
            Count_Bytes (I) := Buffer (Pos);
            Pos := Pos + 1;
         end loop;
         Contract_Count := Decode_Natural_LE (Count_Bytes);
      end;

      if Contract_Count > Max_Stored_Contracts then
         Result := (
            Success       => False,
            Error         => Persist_Format_Error,
            Bytes_Written => 0,
            Bytes_Read    => Bytes_Read
         );
         return;
      end if;

      --  Read each contract entry
      for I in 0 .. Contract_Count - 1 loop
         declare
            Contract_ID : Contract_Address;
            Code_Hash_Val : Hash256;
            Name_Len : Natural;
            Name_Buf : String (1 .. 64) := (others => ' ');
            Version_Major, Version_Minor, Version_Patch : Natural;
            Cert_Val : Natural;
            Code_Size : Natural;
            Manifest : Node_Contract_Manifest;
            Code_Buf : Node_Code_Buffer := (others => 0);
            Index : Stored_Contract_Index;
            Success : Boolean;
         begin
            --  Read contract ID
            for J in Contract_ID'Range loop
               Contract_ID (J) := To_VM (Buffer (Pos));
               Pos := Pos + 1;
            end loop;

            --  Read code hash
            for J in Code_Hash_Val'Range loop
               Code_Hash_Val (J) := To_VM (Buffer (Pos));
               Pos := Pos + 1;
            end loop;

            --  Read name
            declare
               Len_Bytes : Anubis_Types.Byte_Array (0 .. 3);
            begin
               for J in 0 .. 3 loop
                  Len_Bytes (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;
               Name_Len := Decode_Natural_LE (Len_Bytes);
               if Name_Len > 64 then
                  Name_Len := 64;
               end if;
            end;
            for J in 1 .. 64 loop
               if J <= Name_Len then
                  Name_Buf (J) := Character'Val (Buffer (Pos));
               end if;
               Pos := Pos + 1;
            end loop;

            --  Read version
            declare
               V : Anubis_Types.Byte_Array (0 .. 3);
            begin
               for J in 0 .. 3 loop
                  V (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;
               Version_Major := Decode_Natural_LE (V);

               for J in 0 .. 3 loop
                  V (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;
               Version_Minor := Decode_Natural_LE (V);

               for J in 0 .. 3 loop
                  V (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;
               Version_Patch := Decode_Natural_LE (V);
            end;

            --  Read certification
            declare
               Cert_Bytes : Anubis_Types.Byte_Array (0 .. 3);
            begin
               for J in 0 .. 3 loop
                  Cert_Bytes (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;
               Cert_Val := Decode_Natural_LE (Cert_Bytes);
            end;

            --  Read code size
            declare
               Size_Bytes : Anubis_Types.Byte_Array (0 .. 3);
            begin
               for J in 0 .. 3 loop
                  Size_Bytes (J) := Buffer (Pos);
                  Pos := Pos + 1;
               end loop;
               Code_Size := Decode_Natural_LE (Size_Bytes);
            end;

            if Code_Size > Node_Max_Code_Size then
               Result := (
                  Success       => False,
                  Error         => Persist_Format_Error,
                  Bytes_Written => 0,
                  Bytes_Read    => Bytes_Read
               );
               return;
            end if;

            --  Read code
            for J in 0 .. Code_Size - 1 loop
               Code_Buf (J) := To_VM (Buffer (Pos));
               Pos := Pos + 1;
            end loop;

            --  Skip padding
            while (Pos mod 4) /= 0 loop
               Pos := Pos + 1;
            end loop;

            --  Build manifest (Name is 64 chars per Contract_Name in anubis_node_types.ads)
            Manifest := (
               Name          => Name_Buf,
               Name_Len      => Name_Len,
               Version_Major => Version_Major,
               Version_Minor => Version_Minor,
               Version_Patch => Version_Patch,
               Cert          => Cert_Level'Val (Cert_Val mod 5)
            );

            --  Register contract
            Node_Contract_Registry.Register_Contract (
               Registry  => Registry,
               Manifest  => Manifest,
               Code      => Code_Buf,
               Code_Size => Code_Size,
               Block_Num => U256_Zero,
               Index     => Index,
               Success   => Success
            );

            if not Success then
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
      for I in File_Magic_Footer'Range loop
         if Buffer (Pos) /= Local_Byte (Character'Pos (File_Magic_Footer (I))) then
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
      Compute_State_Root (Registry, Computed_State_Root);
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
   end Load_Registry;

   ---------------------------------------------------------------------------
   --  Block Checkpoint Operations
   ---------------------------------------------------------------------------

   procedure Save_Checkpoint (
      Checkpoint : in     Block_Checkpoint;
      File_Path  : in     String;
      Path_Len   : in     Natural;
      Result     : out    Persist_Result
   )
      with SPARK_Mode => Off
   is
      use State_File_IO;

      --  Checkpoint: 8 (magic) + 32 (block_number) + 32 (block_hash) +
      --              32 (state_root) + 32 (timestamp) + 8 (magic) = 144 bytes
      Buffer : Anubis_Types.Byte_Array (0 .. 143) := (others => 0);
      Pos : Natural := 0;
      IO_Result_Val : IO_Result;
      Actual_Path : constant String := File_Path (File_Path'First .. File_Path'First + Path_Len - 1);
      U256_Bytes : Anubis_Types.Byte_Array (0 .. 31);
   begin
      --  Write header magic
      for I in File_Magic_Header'Range loop
         Buffer (Pos) := Local_Byte (Character'Pos (File_Magic_Header (I)));
         Pos := Pos + 1;
      end loop;

      --  Write block number (32 bytes)
      Encode_U256_LE (Checkpoint.Block_Number, U256_Bytes);
      for I in 0 .. 31 loop
         Buffer (Pos) := U256_Bytes (I);
         Pos := Pos + 1;
      end loop;

      --  Write block hash (32 bytes)
      for I in Checkpoint.Block_Hash'Range loop
         Buffer (Pos) := To_Local (Checkpoint.Block_Hash (I));
         Pos := Pos + 1;
      end loop;

      --  Write state root (32 bytes)
      for I in Checkpoint.State_Root'Range loop
         Buffer (Pos) := To_Local (Checkpoint.State_Root (I));
         Pos := Pos + 1;
      end loop;

      --  Write timestamp (32 bytes)
      Encode_U256_LE (Checkpoint.Timestamp, U256_Bytes);
      for I in 0 .. 31 loop
         Buffer (Pos) := U256_Bytes (I);
         Pos := Pos + 1;
      end loop;

      --  Write footer magic
      for I in File_Magic_Footer'Range loop
         Buffer (Pos) := Local_Byte (Character'Pos (File_Magic_Footer (I)));
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
      State_File_IO.Write_Atomic (Actual_Path, Buffer, IO_Result_Val);
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
   end Save_Checkpoint;

   procedure Load_Checkpoint (
      File_Path  : in     String;
      Path_Len   : in     Natural;
      Checkpoint : out    Block_Checkpoint;
      Result     : out    Persist_Result
   )
      with SPARK_Mode => Off
   is
      use State_File_IO;

      Buffer : Anubis_Types.Byte_Array (0 .. 143) := (others => 0);
      Bytes_Read : Natural := 0;
      Pos : Natural := 0;
      IO_Result_Val : IO_Result;
      Actual_Path : constant String := File_Path (File_Path'First .. File_Path'First + Path_Len - 1);
      U256_Bytes : Anubis_Types.Byte_Array (0 .. 31);
   begin
      --  Initialize checkpoint
      Checkpoint := (
         Block_Number => U256_Zero,
         Block_Hash   => (others => 0),
         State_Root   => (others => 0),
         Timestamp    => U256_Zero
      );

      --  Check if file exists
      if not State_File_IO.File_Exists (Actual_Path) then
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
      if IO_Result_Val /= IO_OK or Bytes_Read /= 144 then
         Result := (
            Success       => False,
            Error         => Persist_Read_Error,
            Bytes_Written => 0,
            Bytes_Read    => Bytes_Read
         );
         return;
      end if;

      --  Verify header magic
      for I in File_Magic_Header'Range loop
         if Buffer (Pos) /= Local_Byte (Character'Pos (File_Magic_Header (I))) then
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

      --  Read block number
      for I in 0 .. 31 loop
         U256_Bytes (I) := Buffer (Pos);
         Pos := Pos + 1;
      end loop;
      Checkpoint.Block_Number := Decode_U256_LE (U256_Bytes);

      --  Read block hash
      for I in Checkpoint.Block_Hash'Range loop
         Checkpoint.Block_Hash (I) := To_VM (Buffer (Pos));
         Pos := Pos + 1;
      end loop;

      --  Read state root
      for I in Checkpoint.State_Root'Range loop
         Checkpoint.State_Root (I) := To_VM (Buffer (Pos));
         Pos := Pos + 1;
      end loop;

      --  Read timestamp
      for I in 0 .. 31 loop
         U256_Bytes (I) := Buffer (Pos);
         Pos := Pos + 1;
      end loop;
      Checkpoint.Timestamp := Decode_U256_LE (U256_Bytes);

      --  Verify footer magic
      for I in File_Magic_Footer'Range loop
         if Buffer (Pos) /= Local_Byte (Character'Pos (File_Magic_Footer (I))) then
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

      Result := (
         Success       => True,
         Error         => Persist_OK,
         Bytes_Written => 0,
         Bytes_Read    => Bytes_Read
      );
   end Load_Checkpoint;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Registry_File_Valid (
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
      for I in File_Magic_Header'Range loop
         if Buffer (I - 1) /= Local_Byte (Character'Pos (File_Magic_Header (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Registry_File_Valid;

end State_Persistence;
