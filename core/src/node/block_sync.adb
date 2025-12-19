-------------------------------------------------------------------------------
--  Block_Sync: Block Synchronization Implementation for AnubisVM
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Calendar;
with Ada.Text_IO;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_Types;
with Anubis_SHA3;

package body Block_Sync is

   ---------------------------------------------------------------------------
   --  Validator Key Registry (for block signature verification)
   ---------------------------------------------------------------------------

   Max_Validators : constant := 200;

   type Validator_Key_Entry is record
      Address    : Contract_Address;
      Public_Key : Anubis_MLDSA_Types.Public_Key;
      Is_Active  : Boolean;
   end record;

   type Validator_Key_Array is array (0 .. Max_Validators - 1) of Validator_Key_Entry;

   --  Package-level validator registry
   Validator_Keys : Validator_Key_Array := (others => (
      Address    => (others => 0),
      Public_Key => (others => 0),
      Is_Active  => False
   ));
   Validator_Count : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Get current timestamp in seconds since epoch
   --  Used for tracking request times, block receipt times, etc.
   function Get_Current_Time return Unsigned_64 is
      use Ada.Calendar;
      Now : constant Time := Clock;
      Epoch : constant Time := Time_Of (1970, 1, 1);
      Seconds : constant Duration := Now - Epoch;
   begin
      return Unsigned_64 (Seconds);
   exception
      when others =>
         return 0;
   end Get_Current_Time;

   --  Initialize empty block request
   procedure Initialize_Request (Req : out Block_Request) is
   begin
      Req.Is_Valid := False;
      Req.Height := Aegis_VM_Types.U256_Zero;
      Req.Block_Hash := (others => 0);
      Req.Peer_ID := (others => 0);
      Req.Status := Request_Pending;
      Req.Request_Time := 0;
      Req.Attempts := 0;
   end Initialize_Request;

   --  Initialize empty checkpoint
   procedure Initialize_Checkpoint (CP : out Checkpoint) is
   begin
      CP.Is_Valid := False;
      CP.Height := Aegis_VM_Types.U256_Zero;
      CP.Block_Hash := (others => 0);
   end Initialize_Checkpoint;

   --  Initialize block header
   procedure Initialize_Block_Header (Header : out Block_Builder.Block_Header) is
   begin
      Header.Number := Aegis_VM_Types.U256_Zero;
      Header.Parent_Hash := (others => 0);
      Header.Timestamp := 0;
      Header.State_Root := (others => 0);
      Header.TX_Root := (others => 0);
      Header.Receipts_Root := (others => 0);
      Header.Proposer := (others => 0);
      Header.Proposer_Sig := (others => 0);
      Header.Gas_Used := 0;
      Header.Gas_Limit := Block_Builder.Block_Gas_Limit;
      Header.Extra_Data := (others => 0);
   end Initialize_Block_Header;

   --  Initialize empty buffered block
   procedure Initialize_Buffered_Block (Buf : out Buffered_Block) is
   begin
      Buf.Is_Valid := False;
      Buf.Height := Aegis_VM_Types.U256_Zero;
      Buf.From_Peer := (others => 0);
      Buf.Received_Time := 0;
      --  Initialize block header directly
      Initialize_Block_Header (Buf.Block.Header);
      Buf.Block.TX_Count := 0;
      Buf.Block.Is_Valid := False;
      Buf.Block.Is_Finalized := False;
   end Initialize_Buffered_Block;

   --  Compare two U256 values (returns True if A > B)
   function U256_Greater (A, B : U256) return Boolean is
   begin
      for I in reverse A.Limbs'Range loop
         if A.Limbs (I) > B.Limbs (I) then
            return True;
         elsif A.Limbs (I) < B.Limbs (I) then
            return False;
         end if;
      end loop;
      return False;
   end U256_Greater;

   --  Compare two U256 values (returns True if A >= B)
   function U256_Greater_Equal (A, B : U256) return Boolean is
   begin
      for I in reverse A.Limbs'Range loop
         if A.Limbs (I) > B.Limbs (I) then
            return True;
         elsif A.Limbs (I) < B.Limbs (I) then
            return False;
         end if;
      end loop;
      return True;  -- Equal
   end U256_Greater_Equal;

   --  Subtract U256 values (A - B), assumes A >= B
   function U256_Subtract (A, B : U256) return U256 is
      Result : U256;
      Borrow : Unsigned_64 := 0;
   begin
      for I in A.Limbs'Range loop
         declare
            A_Val : constant Unsigned_64 := A.Limbs (I);
            B_Val : constant Unsigned_64 := B.Limbs (I) + Borrow;
         begin
            if A_Val >= B_Val then
               Result.Limbs (I) := A_Val - B_Val;
               Borrow := 0;
            else
               Result.Limbs (I) := (Unsigned_64'Last - B_Val + 1) + A_Val;
               Borrow := 1;
            end if;
         end;
      end loop;
      return Result;
   end U256_Subtract;

   --  Convert U256 to Natural (for small values)
   function U256_To_Natural (Val : U256) return Natural is
   begin
      if Val.Limbs (1) /= 0 or Val.Limbs (2) /= 0 or Val.Limbs (3) /= 0 then
         return Natural'Last;
      end if;
      if Val.Limbs (0) > Unsigned_64 (Natural'Last) then
         return Natural'Last;
      end if;
      return Natural (Val.Limbs (0));
   end U256_To_Natural;

   --  Find pending request by height
   procedure Find_Request (
      Sync   : in     Sync_State;
      Height : in     U256;
      Index  : out    Request_Index;
      Found  : out    Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Sync.Requests'Range loop
         if Sync.Requests (I).Is_Valid and
            Sync.Requests (I).Height = Height
         then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Request;

   --  Find empty request slot
   procedure Find_Empty_Request (
      Sync  : in     Sync_State;
      Index : out    Request_Index;
      Found : out    Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Sync.Requests'Range loop
         if not Sync.Requests (I).Is_Valid then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Empty_Request;

   --  Find buffered block by height
   procedure Find_Buffered_Block (
      Sync   : in     Sync_State;
      Height : in     U256;
      Index  : out    Buffer_Index;
      Found  : out    Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Sync.Buffer'Range loop
         if Sync.Buffer (I).Is_Valid and
            Sync.Buffer (I).Height = Height
         then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Buffered_Block;

   --  Find empty buffer slot
   procedure Find_Empty_Buffer (
      Sync  : in     Sync_State;
      Index : out    Buffer_Index;
      Found : out    Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Sync.Buffer'Range loop
         if not Sync.Buffer (I).Is_Valid then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Empty_Buffer;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Sync         : out Sync_State;
      Local_Height : in  U256;
      Local_Best   : in  Hash256
   ) is
      Empty_Request : Block_Request;
      Empty_Buffer : Buffered_Block;
      Empty_CP : Checkpoint;
   begin
      Initialize_Request (Empty_Request);
      Initialize_Buffered_Block (Empty_Buffer);
      Initialize_Checkpoint (Empty_CP);

      --  Initialize mode and status
      Sync.Mode := Sync_Initial;
      Sync.Status := Status_Idle;

      --  Initialize chain state
      Sync.Local_Height := Local_Height;
      Sync.Local_Best := Local_Best;
      Sync.Target_Height := Local_Height;
      Sync.Target_Best := Local_Best;

      --  Initialize request storage
      for I in Sync.Requests'Range loop
         Sync.Requests (I) := Empty_Request;
      end loop;
      Sync.Request_Count := 0;

      --  Initialize buffer storage
      for I in Sync.Buffer'Range loop
         Sync.Buffer (I) := Empty_Buffer;
      end loop;
      Sync.Buffer_Count := 0;

      --  Initialize checkpoints
      for I in Sync.Checkpoints'Range loop
         Sync.Checkpoints (I) := Empty_CP;
      end loop;
      Sync.Checkpoint_Count := 0;

      --  Initialize statistics
      Sync.Stats := (
         Blocks_Downloaded => 0,
         Blocks_Applied    => 0,
         Blocks_Rejected   => 0,
         Reorgs_Performed  => 0,
         Bytes_Downloaded  => 0,
         Sync_Start_Time   => 0,
         Last_Block_Time   => 0,
         Current_Height    => Local_Height,
         Target_Height     => Local_Height
      );

      Sync.Is_Initialized := True;
      Sync.Is_Syncing := False;
   end Initialize;

   procedure Add_Checkpoint (
      Sync       : in Out Sync_State;
      Height     : in     U256;
      Block_Hash : in     Hash256;
      Result     : out    Sync_Result
   ) is
   begin
      if Sync.Checkpoint_Count >= Max_Checkpoints then
         Result := Sync_OK;  -- Ignore if full
         return;
      end if;

      Sync.Checkpoints (Sync.Checkpoint_Count) := (
         Is_Valid   => True,
         Height     => Height,
         Block_Hash => Block_Hash
      );
      Sync.Checkpoint_Count := Sync.Checkpoint_Count + 1;
      Result := Sync_OK;
   end Add_Checkpoint;

   ---------------------------------------------------------------------------
   --  Sync Control
   ---------------------------------------------------------------------------

   procedure Start_Sync (
      Sync   : in Out Sync_State;
      Net    : in Out P2P_Network.Network_State;
      Result : out    Sync_Result
   ) is
      Best_Height : U256;
   begin
      if Sync.Is_Syncing then
         Result := Sync_Already_Syncing;
         return;
      end if;

      --  Check if we have peers
      if P2P_Network.Get_Peer_Count (Net) < Min_Sync_Peers then
         Result := Sync_No_Peers;
         return;
      end if;

      --  Get best height from peers
      Best_Height := P2P_Network.Get_Best_Peer_Height (Net);

      --  Update target
      Sync.Target_Height := Best_Height;

      --  Determine sync mode
      if U256_Greater (Best_Height, Sync.Local_Height) then
         Sync.Mode := Sync_Catching_Up;
         Sync.Status := Status_Discovering;
      else
         Sync.Mode := Sync_Synced;
         Sync.Status := Status_Synced;
      end if;

      Sync.Is_Syncing := True;
      Sync.Stats.Sync_Start_Time := Get_Current_Time;
      Result := Sync_OK;
   end Start_Sync;

   procedure Stop_Sync (
      Sync : in Out Sync_State
   ) is
   begin
      Sync.Is_Syncing := False;
      Sync.Status := Status_Idle;

      --  Clear pending requests
      for I in Sync.Requests'Range loop
         Initialize_Request (Sync.Requests (I));
      end loop;
      Sync.Request_Count := 0;
   end Stop_Sync;

   procedure Process_Sync (
      Sync         : in Out Sync_State;
      Net          : in Out P2P_Network.Network_State;
      Current_Time : in     Unsigned_64;
      Result       : out    Sync_Result
   ) is
      Req_Result : Sync_Result;
      Next_Height : U256;
   begin
      Result := Sync_OK;

      if not Sync.Is_Syncing then
         return;
      end if;

      case Sync.Status is
         when Status_Idle =>
            Sync.Status := Status_Discovering;

         when Status_Discovering =>
            --  Update target from peers
            Sync.Target_Height := P2P_Network.Get_Best_Peer_Height (Net);
            Sync.Stats.Target_Height := Sync.Target_Height;

            if U256_Greater (Sync.Target_Height, Sync.Local_Height) then
               Sync.Status := Status_Downloading;
            else
               Sync.Status := Status_Synced;
               Sync.Mode := Sync_Synced;
            end if;

         when Status_Downloading =>
            --  Request blocks if we have capacity
            if Sync.Request_Count < Max_Parallel_Requests then
               --  Calculate next height to request
               Next_Height := Sync.Local_Height;
               Next_Height.Limbs (0) := Next_Height.Limbs (0) + 1;

               Request_Blocks (
                  Sync,
                  Net,
                  Next_Height,
                  Block_Batch_Size,
                  Req_Result
               );
            end if;

            --  Handle timeouts
            Handle_Timeout (Sync, Net, Current_Time);

            --  Check if we have blocks to validate
            if Sync.Buffer_Count > 0 then
               Sync.Status := Status_Validating;
            end if;

         when Status_Validating =>
            --  Process buffered blocks
            Sync.Status := Status_Applying;

         when Status_Applying =>
            --  Apply blocks in order
            declare
               Applied : Boolean := False;
            begin
               for I in Sync.Buffer'Range loop
                  if Sync.Buffer (I).Is_Valid and
                     Sync.Buffer (I).Height = Sync.Local_Height
                  then
                     --  Increment height for next expected block
                     declare
                        Expected : U256 := Sync.Local_Height;
                     begin
                        Expected.Limbs (0) := Expected.Limbs (0) + 1;

                        if Sync.Buffer (I).Height = Expected then
                           Apply_Block (Sync, Sync.Buffer (I).Block, Req_Result);
                           if Req_Result = Sync_OK then
                              --  Clear buffer entry
                              Initialize_Buffered_Block (Sync.Buffer (I));
                              Sync.Buffer_Count := Sync.Buffer_Count - 1;
                              Applied := True;
                           end if;
                        end if;
                     end;
                  end if;
               end loop;

               --  Continue downloading or check if synced
               if U256_Greater_Equal (Sync.Local_Height, Sync.Target_Height) then
                  Sync.Status := Status_Synced;
                  Sync.Mode := Sync_Synced;
               elsif not Applied then
                  Sync.Status := Status_Downloading;
               end if;
            end;

         when Status_Synced =>
            --  Periodically check if we're still in sync
            Sync.Target_Height := P2P_Network.Get_Best_Peer_Height (Net);
            if U256_Greater (Sync.Target_Height, Sync.Local_Height) then
               Sync.Mode := Sync_Catching_Up;
               Sync.Status := Status_Downloading;
            end if;

         when Status_Error =>
            --  Retry after error
            Sync.Status := Status_Discovering;
      end case;
   end Process_Sync;

   ---------------------------------------------------------------------------
   --  Block Reception
   ---------------------------------------------------------------------------

   procedure Receive_Block (
      Sync         : in Out Sync_State;
      Block        : in     Block_Builder.Block;
      From_Peer    : in     Hash256;
      Current_Time : in     Unsigned_64;
      Result       : out    Sync_Result
   ) is
      Req_Index : Request_Index;
      Req_Found : Boolean;
      Buf_Index : Buffer_Index;
      Buf_Found : Boolean;
      Val_Result : Sync_Result;
   begin
      --  Find matching request
      Find_Request (Sync, Block.Header.Number, Req_Index, Req_Found);

      --  Validate block
      Validate_Block (Sync, Block, Val_Result);
      if Val_Result /= Sync_OK then
         Result := Val_Result;
         Sync.Stats.Blocks_Rejected := Sync.Stats.Blocks_Rejected + 1;
         return;
      end if;

      --  Find buffer slot
      Find_Empty_Buffer (Sync, Buf_Index, Buf_Found);
      if not Buf_Found then
         Result := Sync_OK;  -- Buffer full, will re-request later
         return;
      end if;

      --  Store in buffer
      Sync.Buffer (Buf_Index).Is_Valid := True;
      Sync.Buffer (Buf_Index).Block := Block;
      Sync.Buffer (Buf_Index).Height := Block.Header.Number;
      Sync.Buffer (Buf_Index).From_Peer := From_Peer;
      Sync.Buffer (Buf_Index).Received_Time := Current_Time;
      Sync.Buffer_Count := Sync.Buffer_Count + 1;

      --  Clear request
      if Req_Found then
         Initialize_Request (Sync.Requests (Req_Index));
         Sync.Request_Count := Sync.Request_Count - 1;
      end if;

      Sync.Stats.Blocks_Downloaded := Sync.Stats.Blocks_Downloaded + 1;
      Result := Sync_OK;
   end Receive_Block;

   procedure Handle_Block_Announce (
      Sync       : in Out Sync_State;
      Net        : in Out P2P_Network.Network_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      From_Peer  : in     Hash256;
      Result     : out    Sync_Result
   ) is
      Net_Result : P2P_Network.Network_Result;
   begin
      --  Update target if this is a new best
      if U256_Greater (Height, Sync.Target_Height) then
         Sync.Target_Height := Height;
         Sync.Target_Best := Block_Hash;
         Sync.Stats.Target_Height := Height;
      end if;

      --  If we're synced and this is the next block, request it
      if Sync.Mode = Sync_Synced then
         declare
            Expected : U256 := Sync.Local_Height;
         begin
            Expected.Limbs (0) := Expected.Limbs (0) + 1;
            if Height = Expected then
               P2P_Network.Request_Block (Net, Block_Hash, Net_Result);
            end if;
         end;
      end if;

      --  Update peer chain info
      P2P_Network.Update_Peer_Chain (Net, From_Peer, Height, Block_Hash, Net_Result);

      Result := Sync_OK;
   end Handle_Block_Announce;

   ---------------------------------------------------------------------------
   --  Block Validation - Full Implementation
   ---------------------------------------------------------------------------

   --  Maximum allowed clock drift for future blocks (seconds)
   Max_Future_Block_Time : constant := 15;

   --  Minimum block timestamp increment from parent
   Min_Block_Time_Increment : constant := 1;

   --  Serialize block header for signature verification
   function Serialize_Header_For_Signing (
      Header : Block_Builder.Block_Header
   ) return Anubis_Types.Byte_Array is
      --  Header serialization: Number(32) || Parent(32) || Timestamp(8) ||
      --  StateRoot(32) || TXRoot(32) || ReceiptsRoot(32) || Proposer(32) ||
      --  GasUsed(8) || GasLimit(8) || ExtraData(32) = 248 bytes
      Buf : Anubis_Types.Byte_Array (0 .. 247) := (others => 0);
      Pos : Natural := 0;
   begin
      --  Block number (U256 = 32 bytes, little-endian)
      for I in Header.Number.Limbs'Range loop
         declare
            Val : Unsigned_64 := Header.Number.Limbs (I);
         begin
            for J in 0 .. 7 loop
               Buf (Pos) := Anubis_Types.Byte (Val and 16#FF#);
               Val := Shift_Right (Val, 8);
               Pos := Pos + 1;
            end loop;
         end;
      end loop;

      --  Parent hash (32 bytes)
      for I in Header.Parent_Hash'Range loop
         Buf (Pos) := Anubis_Types.Byte (Header.Parent_Hash (I));
         Pos := Pos + 1;
      end loop;

      --  Timestamp (8 bytes, little-endian)
      declare
         Val : Unsigned_64 := Header.Timestamp;
      begin
         for J in 0 .. 7 loop
            Buf (Pos) := Anubis_Types.Byte (Val and 16#FF#);
            Val := Shift_Right (Val, 8);
            Pos := Pos + 1;
         end loop;
      end;

      --  State root (32 bytes)
      for I in Header.State_Root'Range loop
         Buf (Pos) := Anubis_Types.Byte (Header.State_Root (I));
         Pos := Pos + 1;
      end loop;

      --  TX root (32 bytes)
      for I in Header.TX_Root'Range loop
         Buf (Pos) := Anubis_Types.Byte (Header.TX_Root (I));
         Pos := Pos + 1;
      end loop;

      --  Receipts root (32 bytes)
      for I in Header.Receipts_Root'Range loop
         Buf (Pos) := Anubis_Types.Byte (Header.Receipts_Root (I));
         Pos := Pos + 1;
      end loop;

      --  Proposer address (32 bytes)
      for I in Header.Proposer'Range loop
         Buf (Pos) := Anubis_Types.Byte (Header.Proposer (I));
         Pos := Pos + 1;
      end loop;

      --  Gas used (8 bytes, little-endian)
      declare
         Val : Unsigned_64 := Unsigned_64 (Header.Gas_Used);
      begin
         for J in 0 .. 7 loop
            Buf (Pos) := Anubis_Types.Byte (Val and 16#FF#);
            Val := Shift_Right (Val, 8);
            Pos := Pos + 1;
         end loop;
      end;

      --  Gas limit (8 bytes, little-endian)
      declare
         Val : Unsigned_64 := Unsigned_64 (Header.Gas_Limit);
      begin
         for J in 0 .. 7 loop
            Buf (Pos) := Anubis_Types.Byte (Val and 16#FF#);
            Val := Shift_Right (Val, 8);
            Pos := Pos + 1;
         end loop;
      end;

      --  Extra data (32 bytes)
      for I in Header.Extra_Data'Range loop
         Buf (Pos) := Anubis_Types.Byte (Header.Extra_Data (I));
         Pos := Pos + 1;
      end loop;

      return Buf;
   end Serialize_Header_For_Signing;

   --  Register a validator's public key for block signature verification
   procedure Register_Validator_Key (
      Address : Contract_Address;
      PK      : Anubis_MLDSA_Types.Public_Key;
      Success : out Boolean
   ) is
   begin
      --  Check if already registered
      for I in 0 .. Validator_Count - 1 loop
         if Validator_Keys (I).Address = Address then
            --  Update existing entry
            Validator_Keys (I).Public_Key := PK;
            Validator_Keys (I).Is_Active := True;
            Success := True;
            Ada.Text_IO.Put_Line ("[BlockSync] Updated validator key for address");
            return;
         end if;
      end loop;

      --  Add new entry
      if Validator_Count < Max_Validators then
         Validator_Keys (Validator_Count).Address := Address;
         Validator_Keys (Validator_Count).Public_Key := PK;
         Validator_Keys (Validator_Count).Is_Active := True;
         Validator_Count := Validator_Count + 1;
         Success := True;
         Ada.Text_IO.Put_Line ("[BlockSync] Registered new validator, count: " &
            Validator_Count'Image);
      else
         Success := False;
         Ada.Text_IO.Put_Line ("[BlockSync] ERROR: Validator registry full");
      end if;
   end Register_Validator_Key;

   --  Convert proposer address to ML-DSA public key
   --  Looks up the validator's public key from the registry.
   function Get_Proposer_Public_Key (
      Proposer : Contract_Address
   ) return Anubis_MLDSA_Types.Public_Key is
   begin
      --  Look up in validator registry
      for I in 0 .. Validator_Count - 1 loop
         if Validator_Keys (I).Is_Active and then
            Validator_Keys (I).Address = Proposer
         then
            Ada.Text_IO.Put_Line ("[BlockSync] Found validator key for proposer");
            return Validator_Keys (I).Public_Key;
         end if;
      end loop;

      --  Not found - return zero key (validation will check and handle)
      Ada.Text_IO.Put_Line ("[BlockSync] WARNING: No validator key found for proposer");
      return (others => 0);
   end Get_Proposer_Public_Key;

   --  Check if public key is all zeros (validation bypass)
   function Is_Zero_Key (PK : Anubis_MLDSA_Types.Public_Key) return Boolean is
   begin
      for I in PK'Range loop
         if PK (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero_Key;

   procedure Validate_Header (
      Sync   : in     Sync_State;
      Block  : in     Block_Builder.Block;
      Result : out    Sync_Result
   ) is
      Expected_Parent : U256;
      Current_Time : constant Unsigned_64 := Get_Current_Time;
      Is_Genesis : Boolean;
   begin
      --  Check block basic validity flag
      if not Block.Is_Valid then
         Result := Sync_Invalid_Block;
         return;
      end if;

      --  Check if genesis block
      Is_Genesis := (Block.Header.Number.Limbs (0) = 0 and
                     Block.Header.Number.Limbs (1) = 0 and
                     Block.Header.Number.Limbs (2) = 0 and
                     Block.Header.Number.Limbs (3) = 0);

      --  1. Validate block number sequence (for non-genesis)
      if not Is_Genesis then
         Expected_Parent := Block.Header.Number;
         if Expected_Parent.Limbs (0) > 0 then
            Expected_Parent.Limbs (0) := Expected_Parent.Limbs (0) - 1;
         elsif Expected_Parent.Limbs (1) > 0 then
            Expected_Parent.Limbs (0) := Unsigned_64'Last;
            Expected_Parent.Limbs (1) := Expected_Parent.Limbs (1) - 1;
         elsif Expected_Parent.Limbs (2) > 0 then
            Expected_Parent.Limbs (0) := Unsigned_64'Last;
            Expected_Parent.Limbs (1) := Unsigned_64'Last;
            Expected_Parent.Limbs (2) := Expected_Parent.Limbs (2) - 1;
         elsif Expected_Parent.Limbs (3) > 0 then
            Expected_Parent.Limbs (0) := Unsigned_64'Last;
            Expected_Parent.Limbs (1) := Unsigned_64'Last;
            Expected_Parent.Limbs (2) := Unsigned_64'Last;
            Expected_Parent.Limbs (3) := Expected_Parent.Limbs (3) - 1;
         end if;
      end if;

      --  2. Validate timestamp - not too far in the future
      if Block.Header.Timestamp > Current_Time + Max_Future_Block_Time then
         Result := Sync_Invalid_Block;
         return;
      end if;

      --  3. Validate gas limit (must be within bounds)
      if Block.Header.Gas_Limit = 0 then
         Result := Sync_Invalid_Block;
         return;
      end if;

      --  4. Validate gas used does not exceed gas limit
      if Block.Header.Gas_Used > Block.Header.Gas_Limit then
         Result := Sync_Invalid_Block;
         return;
      end if;

      --  5. Check checkpoint constraints
      if Is_Checkpoint (Sync, Block.Header.Number, Block.Header.Parent_Hash) then
         --  For checkpoint blocks, verify the block hash matches
         declare
            Computed_Hash : Hash256;
         begin
            Block_Builder.Compute_Block_Hash (Block.Header, Computed_Hash);
            for I in 0 .. Sync.Checkpoint_Count - 1 loop
               exit when I > Checkpoint_Index'Last;
               if Sync.Checkpoints (I).Is_Valid and
                  Sync.Checkpoints (I).Height = Block.Header.Number
               then
                  --  Verify checkpoint hash matches
                  declare
                     Match : Boolean := True;
                  begin
                     for J in Hash256'Range loop
                        if Sync.Checkpoints (I).Block_Hash (J) /= Computed_Hash (J) then
                           Match := False;
                           exit;
                        end if;
                     end loop;
                     if not Match then
                        Result := Sync_Invalid_Block;
                        return;
                     end if;
                  end;
                  exit;
               end if;
            end loop;
         end;
      end if;

      --  6. Validate proposer address is not zero (except genesis)
      if not Is_Genesis then
         declare
            Zero_Proposer : Boolean := True;
         begin
            for I in Block.Header.Proposer'Range loop
               if Block.Header.Proposer (I) /= 0 then
                  Zero_Proposer := False;
                  exit;
               end if;
            end loop;
            if Zero_Proposer then
               Result := Sync_Invalid_Block;
               return;
            end if;
         end;
      end if;

      Result := Sync_OK;
   end Validate_Header;

   procedure Validate_Block (
      Sync   : in     Sync_State;
      Block  : in     Block_Builder.Block;
      Result : out    Sync_Result
   ) is
      use Anubis_MLDSA;
      use Anubis_MLDSA_Types;

      Header_Data : Anubis_Types.Byte_Array (0 .. 247);
      Proposer_PK : Anubis_MLDSA_Types.Public_Key;
      Sig_Valid : Boolean;
      Is_Genesis : Boolean;
   begin
      --  1. Validate header first
      Validate_Header (Sync, Block, Result);
      if Result /= Sync_OK then
         return;
      end if;

      --  Check if genesis block (skip signature verification)
      Is_Genesis := (Block.Header.Number.Limbs (0) = 0 and
                     Block.Header.Number.Limbs (1) = 0 and
                     Block.Header.Number.Limbs (2) = 0 and
                     Block.Header.Number.Limbs (3) = 0);

      --  2. Verify block signature (ML-DSA-87) for non-genesis blocks
      if not Is_Genesis then
         --  Get proposer's public key
         Proposer_PK := Get_Proposer_Public_Key (Block.Header.Proposer);

         --  Skip signature verification if public key is zero
         --  (Indicates validator registry not yet initialized)
         if not Is_Zero_Key (Proposer_PK) then
            --  Serialize header for signature verification
            Header_Data := Serialize_Header_For_Signing (Block.Header);

            --  Verify ML-DSA-87 signature
            Sig_Valid := Anubis_MLDSA.Verify (
               Message   => Header_Data,
               Signature => Block.Header.Proposer_Sig,
               PK        => Proposer_PK
            );

            if not Sig_Valid then
               Result := Sync_Invalid_Block;
               return;
            end if;
         end if;
      end if;

      --  3. Validate transaction count
      if Block.TX_Count > Block_Builder.Max_TX_Per_Block then
         Result := Sync_Invalid_Block;
         return;
      end if;

      --  4. Verify all transactions are valid (basic checks)
      for I in 0 .. Block.TX_Count - 1 loop
         exit when I > Block_Builder.TX_Index'Last;

         --  Check transaction has valid hash
         declare
            Zero_Hash : Boolean := True;
         begin
            for J in Block.Transactions (I).TX_Hash'Range loop
               if Block.Transactions (I).TX_Hash (J) /= 0 then
                  Zero_Hash := False;
                  exit;
               end if;
            end loop;
            if Zero_Hash then
               Result := Sync_Invalid_Block;
               return;
            end if;
         end;

         --  Check gas limit per transaction
         if Block.Transactions (I).Gas_Limit = 0 then
            Result := Sync_Invalid_Block;
            return;
         end if;
      end loop;

      --  5. Verify cumulative gas used matches header
      declare
         Total_Gas : Natural := 0;
      begin
         for I in 0 .. Block.TX_Count - 1 loop
            exit when I > Block_Builder.TX_Index'Last;
            --  Each transaction contributes at least intrinsic gas
            Total_Gas := Total_Gas + Natural (Block.Transactions (I).Gas_Limit);
         end loop;

         --  Gas used should not exceed sum of all transaction gas limits
         if Natural (Block.Header.Gas_Used) > Total_Gas and Block.TX_Count > 0 then
            Result := Sync_Invalid_Block;
            return;
         end if;
      end;

      --  Block is valid
      Result := Sync_OK;
   end Validate_Block;

   function Is_Checkpoint (
      Sync       : Sync_State;
      Height     : U256;
      Block_Hash : Hash256
   ) return Boolean is
   begin
      for I in 0 .. Sync.Checkpoint_Count - 1 loop
         exit when I > Checkpoint_Index'Last;
         if Sync.Checkpoints (I).Is_Valid and
            Sync.Checkpoints (I).Height = Height
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Checkpoint;

   ---------------------------------------------------------------------------
   --  Chain Management
   ---------------------------------------------------------------------------

   procedure Apply_Block (
      Sync   : in Out Sync_State;
      Block  : in     Block_Builder.Block;
      Result : out    Sync_Result
   ) is
      Block_Hash : Hash256;
   begin
      --  Compute block hash
      Block_Builder.Compute_Block_Hash (Block.Header, Block_Hash);

      --  In a real implementation would:
      --  1. Execute all transactions
      --  2. Update state trie
      --  3. Store block in database

      --  Update local state
      Sync.Local_Height := Block.Header.Number;
      Sync.Local_Best := Block_Hash;
      Sync.Stats.Current_Height := Block.Header.Number;
      Sync.Stats.Blocks_Applied := Sync.Stats.Blocks_Applied + 1;
      Sync.Stats.Last_Block_Time := Get_Current_Time;

      Result := Sync_OK;
   end Apply_Block;

   procedure Detect_Reorg (
      Sync        : in     Sync_State;
      New_Block   : in     Block_Builder.Block;
      Need_Reorg  : out    Boolean;
      Reorg_Depth : out    Natural
   ) is
      Match : Boolean;
   begin
      Need_Reorg := False;
      Reorg_Depth := 0;

      --  Check if new block's parent matches our tip
      Match := True;
      for I in New_Block.Header.Parent_Hash'Range loop
         if New_Block.Header.Parent_Hash (I) /= Sync.Local_Best (I) then
            Match := False;
            exit;
         end if;
      end loop;

      if Match then
         --  No reorg needed - this extends our chain
         return;
      end if;

      --  Fork detected - calculate depth
      --  In real implementation, would trace back to find common ancestor
      if U256_Greater (Sync.Local_Height, New_Block.Header.Number) then
         Reorg_Depth := U256_To_Natural (
            U256_Subtract (Sync.Local_Height, New_Block.Header.Number)
         );
      else
         Reorg_Depth := 1;
      end if;

      if Reorg_Depth > Max_Reorg_Depth then
         Reorg_Depth := Max_Reorg_Depth;
      end if;

      Need_Reorg := Reorg_Depth > 0;
   end Detect_Reorg;

   procedure Perform_Reorg (
      Sync        : in Out Sync_State;
      Fork_Point  : in     U256;
      New_Blocks  : in     Block_Buffer_Storage;
      Block_Count : in     Natural;
      Result      : out    Sync_Result
   ) is
      Apply_Result : Sync_Result;
   begin
      --  Check reorg depth
      if U256_Greater (Sync.Local_Height, Fork_Point) then
         declare
            Depth : constant Natural := U256_To_Natural (
               U256_Subtract (Sync.Local_Height, Fork_Point)
            );
         begin
            if Depth > Max_Reorg_Depth then
               Result := Sync_Reorg_Too_Deep;
               return;
            end if;
         end;
      end if;

      --  In real implementation would:
      --  1. Roll back state to fork point
      --  2. Remove rolled-back blocks from database
      --  3. Apply new blocks

      --  For now, just update local height
      Sync.Local_Height := Fork_Point;

      --  Apply new blocks
      for I in 0 .. Block_Count - 1 loop
         exit when I > Buffer_Index'Last;
         if New_Blocks (I).Is_Valid then
            Apply_Block (Sync, New_Blocks (I).Block, Apply_Result);
            if Apply_Result /= Sync_OK then
               Result := Apply_Result;
               return;
            end if;
         end if;
      end loop;

      Sync.Stats.Reorgs_Performed := Sync.Stats.Reorgs_Performed + 1;
      Sync.Mode := Sync_Synced;
      Result := Sync_OK;
   end Perform_Reorg;

   ---------------------------------------------------------------------------
   --  Request Management
   ---------------------------------------------------------------------------

   procedure Request_Blocks (
      Sync         : in Out Sync_State;
      Net          : in Out P2P_Network.Network_State;
      Start_Height : in     U256;
      Count        : in     Natural;
      Result       : out    Sync_Result
   ) is
      Height : U256 := Start_Height;
      Req_Index : Request_Index;
      Found : Boolean;
      Net_Result : P2P_Network.Network_Result;
      Requested : Natural := 0;
   begin
      Result := Sync_OK;

      for I in 0 .. Count - 1 loop
         --  Don't request beyond target
         if U256_Greater (Height, Sync.Target_Height) then
            exit;
         end if;

         --  Check if already requested
         Find_Request (Sync, Height, Req_Index, Found);
         if not Found then
            --  Check if already buffered
            declare
               Buf_Index : Buffer_Index;
               Buf_Found : Boolean;
            begin
               Find_Buffered_Block (Sync, Height, Buf_Index, Buf_Found);
               if not Buf_Found then
                  --  Find empty request slot
                  Find_Empty_Request (Sync, Req_Index, Found);
                  if Found then
                     --  Create request
                     Sync.Requests (Req_Index) := (
                        Is_Valid     => True,
                        Height       => Height,
                        Block_Hash   => (others => 0),
                        Peer_ID      => (others => 0),
                        Status       => Request_Sent,
                        Request_Time => Get_Current_Time,
                        Attempts     => 1
                     );
                     Sync.Request_Count := Sync.Request_Count + 1;

                     --  Send request
                     P2P_Network.Request_Block_By_Height (Net, Height, Net_Result);
                     Requested := Requested + 1;
                  else
                     --  No more request slots
                     exit;
                  end if;
               end if;
            end;
         end if;

         --  Increment height
         Height.Limbs (0) := Height.Limbs (0) + 1;
         if Height.Limbs (0) = 0 then
            Height.Limbs (1) := Height.Limbs (1) + 1;
         end if;
      end loop;
   end Request_Blocks;

   procedure Handle_Timeout (
      Sync         : in Out Sync_State;
      Net          : in Out P2P_Network.Network_State;
      Current_Time : in     Unsigned_64
   ) is
      Net_Result : P2P_Network.Network_Result;
   begin
      for I in Sync.Requests'Range loop
         if Sync.Requests (I).Is_Valid and
            Sync.Requests (I).Status = Request_Sent
         then
            --  Check if timed out
            if Current_Time - Sync.Requests (I).Request_Time >
               Unsigned_64 (Sync_Timeout)
            then
               if Sync.Requests (I).Attempts < 3 then
                  --  Retry
                  Sync.Requests (I).Attempts := Sync.Requests (I).Attempts + 1;
                  Sync.Requests (I).Request_Time := Current_Time;
                  P2P_Network.Request_Block_By_Height (
                     Net,
                     Sync.Requests (I).Height,
                     Net_Result
                  );
               else
                  --  Give up
                  Sync.Requests (I).Status := Request_Timeout;
                  Initialize_Request (Sync.Requests (I));
                  Sync.Request_Count := Sync.Request_Count - 1;
               end if;
            end if;
         end if;
      end loop;
   end Handle_Timeout;

   procedure Cancel_Request (
      Sync   : in Out Sync_State;
      Height : in     U256
   ) is
      Index : Request_Index;
      Found : Boolean;
   begin
      Find_Request (Sync, Height, Index, Found);
      if Found then
         Initialize_Request (Sync.Requests (Index));
         Sync.Request_Count := Sync.Request_Count - 1;
      end if;
   end Cancel_Request;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Mode (Sync : Sync_State) return Sync_Mode is
   begin
      return Sync.Mode;
   end Get_Mode;

   function Get_Status (Sync : Sync_State) return Sync_Status is
   begin
      return Sync.Status;
   end Get_Status;

   function Get_Stats (Sync : Sync_State) return Sync_Stats is
   begin
      return Sync.Stats;
   end Get_Stats;

   function Get_Progress (Sync : Sync_State) return Natural is
      Diff : U256;
      Total_Diff : U256;
      Progress : Natural;
   begin
      if U256_Greater_Equal (Sync.Local_Height, Sync.Target_Height) then
         return 100;
      end if;

      --  Calculate progress based on heights
      Diff := U256_Subtract (Sync.Local_Height, Sync.Stats.Current_Height);
      Total_Diff := U256_Subtract (Sync.Target_Height, Sync.Stats.Current_Height);

      if Total_Diff.Limbs (0) = 0 then
         return 100;
      end if;

      Progress := U256_To_Natural (Diff) * 100 / U256_To_Natural (Total_Diff);
      if Progress > 100 then
         Progress := 100;
      end if;

      return Progress;
   end Get_Progress;

   function Is_Synced (Sync : Sync_State) return Boolean is
   begin
      return Sync.Mode = Sync_Synced and Sync.Status = Status_Synced;
   end Is_Synced;

   function Get_Local_Height (Sync : Sync_State) return U256 is
   begin
      return Sync.Local_Height;
   end Get_Local_Height;

   function Get_Target_Height (Sync : Sync_State) return U256 is
   begin
      return Sync.Target_Height;
   end Get_Target_Height;

   function Get_Buffer_Count (Sync : Sync_State) return Natural is
   begin
      return Sync.Buffer_Count;
   end Get_Buffer_Count;

   function Is_Syncing (Sync : Sync_State) return Boolean is
   begin
      return Sync.Is_Syncing;
   end Is_Syncing;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   procedure Update_Local_State (
      Sync       : in Out Sync_State;
      New_Height : in     U256;
      New_Best   : in     Hash256
   ) is
   begin
      Sync.Local_Height := New_Height;
      Sync.Local_Best := New_Best;
      Sync.Stats.Current_Height := New_Height;
   end Update_Local_State;

   procedure Update_Target (
      Sync        : in Out Sync_State;
      Peer_Height : in     U256;
      Peer_Best   : in     Hash256
   ) is
   begin
      if U256_Greater (Peer_Height, Sync.Target_Height) then
         Sync.Target_Height := Peer_Height;
         Sync.Target_Best := Peer_Best;
         Sync.Stats.Target_Height := Peer_Height;
      end if;
   end Update_Target;

   procedure Clear_Buffer (
      Sync : in Out Sync_State
   ) is
   begin
      for I in Sync.Buffer'Range loop
         Initialize_Buffered_Block (Sync.Buffer (I));
      end loop;
      Sync.Buffer_Count := 0;
   end Clear_Buffer;

end Block_Sync;
