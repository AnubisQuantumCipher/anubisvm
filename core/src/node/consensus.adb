-------------------------------------------------------------------------------
--  Consensus: Proof-of-Authority Consensus Engine for AnubisVM
--
--  Implementation of the PoA consensus mechanism with round-robin
--  validator selection and ML-DSA-87 signed blocks.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_SHA3;
with Anubis_Types;
with Anubis_MLDSA;

package body Consensus with
   SPARK_Mode => On
is
   use Block_Builder;  --  Make Builder_Result operators visible

   ---------------------------------------------------------------------------
   --  Local Helper Functions
   ---------------------------------------------------------------------------

   --  Initialize an authority record
   procedure Initialize_Authority (Auth : out Authority) with
      Global => null
   is
   begin
      Auth.Is_Valid := False;
      Auth.Public_Key := (others => 0);
      Auth.Key_Hash := (others => 0);
      Auth.Name := (others => ' ');
      Auth.Name_Len := 0;
      Auth.Blocks_Produced := 0;
      Auth.Last_Block := U256_Zero;
      Auth.Is_Active := False;
   end Initialize_Authority;

   --  Compute public key hash (SHA3-256 of public key bytes)
   procedure Compute_Key_Hash (
      Pub_Key  : in     Anubis_MLDSA_Types.Public_Key;
      Key_Hash : out    Hash256
   ) with
      Global => null
   is
      Input : Anubis_Types.Byte_Array (0 .. Pub_Key'Length - 1);
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Copy public key to input buffer
      for I in Pub_Key'Range loop
         Input (I - Pub_Key'First) := Pub_Key (I);
      end loop;

      --  Compute SHA3-256 hash
      Anubis_SHA3.SHA3_256 (Input, Digest);

      --  Copy to output
      for I in Key_Hash'Range loop
         Key_Hash (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;
   end Compute_Key_Hash;

   --  Compare two Hash256 values for equality
   function Hash_Equal (A, B : Hash256) return Boolean with
      Global => null
   is
   begin
      for I in A'Range loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Hash_Equal;

   --  Serialize block header for signature verification
   --  Returns the message bytes that were signed (header without signature)
   --  Buffer size: number(32) + parent(32) + timestamp(8) + roots(96) +
   --               proposer(32) + gas(16) + extra(32) = 248 bytes
   Header_Sign_Buffer_Size : constant := 248;

   procedure Serialize_Header_For_Signing (
      Header : in     Block_Builder.Block_Header;
      Buffer : out    Anubis_Types.Byte_Array;
      Length : out    Natural
   ) with
      Global => null,
      Pre => Buffer'Length >= Header_Sign_Buffer_Size and Buffer'First = 0
   is
      Len : Natural := 0;
   begin
      --  Block number (32 bytes, little-endian)
      for I in Header.Number.Limbs'Range loop
         declare
            Limb : constant Unsigned_64 := Header.Number.Limbs (I);
         begin
            Buffer (Len) := Anubis_Types.Byte (Limb and 16#FF#);
            Buffer (Len + 1) := Anubis_Types.Byte ((Limb / 256) and 16#FF#);
            Buffer (Len + 2) := Anubis_Types.Byte ((Limb / 65536) and 16#FF#);
            Buffer (Len + 3) := Anubis_Types.Byte ((Limb / 16777216) and 16#FF#);
            Buffer (Len + 4) := Anubis_Types.Byte ((Limb / 2**24 / 256) and 16#FF#);
            Buffer (Len + 5) := Anubis_Types.Byte ((Limb / 2**24 / 65536) and 16#FF#);
            Buffer (Len + 6) := Anubis_Types.Byte ((Limb / 2**24 / 16777216) and 16#FF#);
            Buffer (Len + 7) := Anubis_Types.Byte (Limb / 2**56);
            Len := Len + 8;
         end;
      end loop;

      --  Parent hash (32 bytes)
      for I in Header.Parent_Hash'Range loop
         Buffer (Len) := Anubis_Types.Byte (Header.Parent_Hash (I));
         Len := Len + 1;
      end loop;

      --  Timestamp (8 bytes, little-endian)
      declare
         TS : constant Unsigned_64 := Header.Timestamp;
      begin
         Buffer (Len) := Anubis_Types.Byte (TS and 16#FF#);
         Buffer (Len + 1) := Anubis_Types.Byte ((TS / 256) and 16#FF#);
         Buffer (Len + 2) := Anubis_Types.Byte ((TS / 65536) and 16#FF#);
         Buffer (Len + 3) := Anubis_Types.Byte ((TS / 16777216) and 16#FF#);
         Buffer (Len + 4) := Anubis_Types.Byte ((TS / 2**24 / 256) and 16#FF#);
         Buffer (Len + 5) := Anubis_Types.Byte ((TS / 2**24 / 65536) and 16#FF#);
         Buffer (Len + 6) := Anubis_Types.Byte ((TS / 2**24 / 16777216) and 16#FF#);
         Buffer (Len + 7) := Anubis_Types.Byte (TS / 2**56);
         Len := Len + 8;
      end;

      --  State root (32 bytes)
      for I in Header.State_Root'Range loop
         Buffer (Len) := Anubis_Types.Byte (Header.State_Root (I));
         Len := Len + 1;
      end loop;

      --  TX root (32 bytes)
      for I in Header.TX_Root'Range loop
         Buffer (Len) := Anubis_Types.Byte (Header.TX_Root (I));
         Len := Len + 1;
      end loop;

      --  Receipts root (32 bytes)
      for I in Header.Receipts_Root'Range loop
         Buffer (Len) := Anubis_Types.Byte (Header.Receipts_Root (I));
         Len := Len + 1;
      end loop;

      --  Proposer address (32 bytes)
      for I in Header.Proposer'Range loop
         Buffer (Len) := Anubis_Types.Byte (Header.Proposer (I));
         Len := Len + 1;
      end loop;

      --  Gas used (8 bytes, little-endian)
      declare
         Gas : constant Unsigned_64 := Unsigned_64 (Header.Gas_Used);
      begin
         Buffer (Len) := Anubis_Types.Byte (Gas and 16#FF#);
         Buffer (Len + 1) := Anubis_Types.Byte ((Gas / 256) and 16#FF#);
         Buffer (Len + 2) := Anubis_Types.Byte ((Gas / 65536) and 16#FF#);
         Buffer (Len + 3) := Anubis_Types.Byte ((Gas / 16777216) and 16#FF#);
         Buffer (Len + 4) := Anubis_Types.Byte ((Gas / 2**24 / 256) and 16#FF#);
         Buffer (Len + 5) := Anubis_Types.Byte ((Gas / 2**24 / 65536) and 16#FF#);
         Buffer (Len + 6) := Anubis_Types.Byte ((Gas / 2**24 / 16777216) and 16#FF#);
         Buffer (Len + 7) := Anubis_Types.Byte (Gas / 2**56);
         Len := Len + 8;
      end;

      --  Gas limit (8 bytes, little-endian)
      declare
         Gas : constant Unsigned_64 := Unsigned_64 (Header.Gas_Limit);
      begin
         Buffer (Len) := Anubis_Types.Byte (Gas and 16#FF#);
         Buffer (Len + 1) := Anubis_Types.Byte ((Gas / 256) and 16#FF#);
         Buffer (Len + 2) := Anubis_Types.Byte ((Gas / 65536) and 16#FF#);
         Buffer (Len + 3) := Anubis_Types.Byte ((Gas / 16777216) and 16#FF#);
         Buffer (Len + 4) := Anubis_Types.Byte ((Gas / 2**24 / 256) and 16#FF#);
         Buffer (Len + 5) := Anubis_Types.Byte ((Gas / 2**24 / 65536) and 16#FF#);
         Buffer (Len + 6) := Anubis_Types.Byte ((Gas / 2**24 / 16777216) and 16#FF#);
         Buffer (Len + 7) := Anubis_Types.Byte (Gas / 2**56);
         Len := Len + 8;
      end;

      --  Extra data (32 bytes)
      for I in Header.Extra_Data'Range loop
         Buffer (Len) := Anubis_Types.Byte (Header.Extra_Data (I));
         Len := Len + 1;
      end loop;

      Length := Len;
   end Serialize_Header_For_Signing;

   --  Sign a block header with our secret key
   procedure Sign_Block_Header (
      Cons      : in     Consensus_State;
      Header    : in Out Block_Builder.Block_Header;
      Success   : out    Boolean
   ) with
      Global => null
   is
      Header_Buf : Anubis_Types.Byte_Array (0 .. Header_Sign_Buffer_Size - 1) :=
                     (others => 0);
      Header_Len : Natural;
      Zero_Random : constant Anubis_MLDSA_Types.Seed := (others => 0);
   begin
      --  Serialize the header (without signature)
      Serialize_Header_For_Signing (Header, Header_Buf, Header_Len);

      --  Sign with ML-DSA-87 (deterministic signing with zero randomness)
      Anubis_MLDSA.Sign (
         SK      => Cons.Our_Secret_Key,
         Msg     => Header_Buf (0 .. Header_Len - 1),
         Random  => Zero_Random,
         Sig     => Header.Proposer_Sig,
         Success => Success
      );
   end Sign_Block_Header;

   --  Find authority by key hash
   procedure Find_Authority_By_Hash (
      Cons  : in     Consensus_State;
      Key_Hash : in  Hash256;
      Index : out    Authority_Index;
      Found : out    Boolean
   ) with
      Global => null
   is
   begin
      Found := False;
      Index := 0;

      for I in Authority_Index loop
         if Cons.Authorities (I).Is_Valid and then
            Hash_Equal (Cons.Authorities (I).Key_Hash, Key_Hash)
         then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Authority_By_Hash;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Cons          : out Consensus_State;
      Mode          : in  Consensus_Mode;
      Our_Key       : in  Anubis_MLDSA_Types.Public_Key;
      Our_Secret    : in  Anubis_MLDSA_Types.Secret_Key
   ) is
   begin
      --  Initialize authority storage
      for I in Authority_Index loop
         Initialize_Authority (Cons.Authorities (I));
      end loop;
      Cons.Authority_Count := 0;

      --  Initialize our authority info
      Cons.Our_Index := 0;
      Cons.Our_Key := Our_Key;
      Cons.Our_Secret_Key := Our_Secret;
      Compute_Key_Hash (Our_Key, Cons.Our_Key_Hash);
      Cons.Is_Authority := False;

      --  Initialize slot tracking
      Cons.Current_Slot := 0;
      Cons.Last_Slot_Time := 0;

      --  Initialize chain state
      Cons.Chain_Height := U256_Zero;
      Cons.Last_Block_Hash := (others => 0);
      Cons.Last_Block_Slot := 0;

      --  Set mode and initialize stats
      Cons.Mode := Mode;
      Cons.Stats := Consensus_Stats'(
         Blocks_Proposed   => 0,
         Blocks_Validated  => 0,
         Blocks_Rejected   => 0,
         Slots_Missed      => 0,
         Current_Slot      => 0,
         Current_Epoch     => 0
      );

      --  Set flags
      Cons.Is_Initialized := True;
      Cons.Is_Running := False;
   end Initialize;

   procedure Add_Authority (
      Cons     : in Out Consensus_State;
      Pub_Key  : in     Anubis_MLDSA_Types.Public_Key;
      Name     : in     String;
      Name_Len : in     Natural;
      Result   : out    Consensus_Result
   ) is
      Key_Hash : Hash256;
      Index    : Authority_Index;
      Found    : Boolean;
   begin
      --  Check capacity
      if Cons.Authority_Count >= Max_Authorities then
         Result := Consensus_Invalid_Authority;
         return;
      end if;

      --  Compute key hash
      Compute_Key_Hash (Pub_Key, Key_Hash);

      --  Check for duplicates
      Find_Authority_By_Hash (Cons, Key_Hash, Index, Found);
      if Found then
         Result := Consensus_Invalid_Authority;
         return;
      end if;

      --  Find empty slot
      for I in Authority_Index loop
         if not Cons.Authorities (I).Is_Valid then
            Cons.Authorities (I).Is_Valid := True;
            Cons.Authorities (I).Public_Key := Pub_Key;
            Cons.Authorities (I).Key_Hash := Key_Hash;

            --  Copy name
            Cons.Authorities (I).Name := (others => ' ');
            for J in 1 .. Name_Len loop
               if J <= 32 then
                  Cons.Authorities (I).Name (J) := Name (Name'First + J - 1);
               end if;
            end loop;
            Cons.Authorities (I).Name_Len := Name_Len;

            Cons.Authorities (I).Blocks_Produced := 0;
            Cons.Authorities (I).Last_Block := U256_Zero;
            Cons.Authorities (I).Is_Active := True;

            Cons.Authority_Count := Cons.Authority_Count + 1;

            --  Check if this is our key
            if Hash_Equal (Key_Hash, Cons.Our_Key_Hash) then
               Cons.Our_Index := I;
               Cons.Is_Authority := True;
            end if;

            Result := Consensus_OK;
            return;
         end if;
      end loop;

      Result := Consensus_Invalid_Authority;
   end Add_Authority;

   procedure Remove_Authority (
      Cons     : in Out Consensus_State;
      Key_Hash : in     Hash256;
      Result   : out    Consensus_Result
   ) is
      Index : Authority_Index;
      Found : Boolean;
   begin
      Find_Authority_By_Hash (Cons, Key_Hash, Index, Found);

      if not Found then
         Result := Consensus_Invalid_Authority;
         return;
      end if;

      --  Clear the authority
      Initialize_Authority (Cons.Authorities (Index));
      Cons.Authority_Count := Cons.Authority_Count - 1;

      --  Check if we removed ourselves
      if Hash_Equal (Key_Hash, Cons.Our_Key_Hash) then
         Cons.Is_Authority := False;
      end if;

      Result := Consensus_OK;
   end Remove_Authority;

   ---------------------------------------------------------------------------
   --  Consensus Control
   ---------------------------------------------------------------------------

   procedure Start (
      Cons   : in Out Consensus_State;
      Result : out    Consensus_Result
   ) is
   begin
      if not Cons.Is_Initialized then
         Result := Consensus_Not_Initialized;
         return;
      end if;

      --  Need at least one authority
      if Cons.Authority_Count = 0 then
         Result := Consensus_Invalid_Authority;
         return;
      end if;

      Cons.Is_Running := True;
      Result := Consensus_OK;
   end Start;

   procedure Stop (
      Cons : in Out Consensus_State
   ) is
   begin
      Cons.Is_Running := False;
   end Stop;

   procedure Process_Slot (
      Cons         : in Out Consensus_State;
      Builder      : in Out Block_Builder.Builder_State;
      Pool         : in Out Mempool.Mempool_State;
      Net          : in Out P2P_Network.Network_State;
      Current_Time : in     Unsigned_64;
      Result       : out    Consensus_Result
   ) is
      New_Slot : Slot_Number;
      Blk      : Block_Builder.Block;
   begin
      --  Calculate current slot from timestamp
      New_Slot := Slot_From_Timestamp (Current_Time);

      --  Check if we've moved to a new slot
      if New_Slot <= Cons.Current_Slot then
         Result := Consensus_Already_Proposed;
         return;
      end if;

      --  Update slot tracking
      Cons.Current_Slot := New_Slot;
      Cons.Last_Slot_Time := Current_Time;
      Cons.Stats.Current_Slot := New_Slot;
      Cons.Stats.Current_Epoch := Natural (New_Slot / Unsigned_64 (Blocks_Per_Epoch));

      --  Check if it's our turn
      if not Is_Our_Turn (Cons, New_Slot) then
         Result := Consensus_Not_Our_Turn;
         return;
      end if;

      --  Produce a block
      Produce_Block (Cons, Builder, Pool, Net, Blk, Result);
   end Process_Slot;

   ---------------------------------------------------------------------------
   --  Block Production
   ---------------------------------------------------------------------------

   function Is_Our_Turn (
      Cons : Consensus_State;
      Slot : Slot_Number
   ) return Boolean is
      Authority_Idx : Authority_Index;
   begin
      if not Cons.Is_Authority then
         return False;
      end if;

      if Cons.Authority_Count = 0 then
         return False;
      end if;

      --  Single authority mode: always our turn
      if Cons.Mode = Mode_Single_Authority then
         return True;
      end if;

      --  Round-robin selection
      Authority_Idx := Get_Slot_Authority (Cons, Slot);
      return Authority_Idx = Cons.Our_Index;
   end Is_Our_Turn;

   function Get_Slot_Authority (
      Cons : Consensus_State;
      Slot : Slot_Number
   ) return Authority_Index is
      Idx : Natural;
      Count : Natural := 0;
   begin
      if Cons.Authority_Count = 0 then
         return 0;
      end if;

      --  Calculate which authority should produce this slot's block
      Idx := Natural (Slot mod Unsigned_64 (Cons.Authority_Count));

      --  Map to actual authority index (skipping invalid entries)
      for I in Authority_Index loop
         if Cons.Authorities (I).Is_Valid and then
            Cons.Authorities (I).Is_Active
         then
            if Count = Idx then
               return I;
            end if;
            Count := Count + 1;
         end if;
      end loop;

      return 0;
   end Get_Slot_Authority;

   procedure Produce_Block (
      Cons    : in Out Consensus_State;
      Builder : in Out Block_Builder.Builder_State;
      Pool    : in Out Mempool.Mempool_State;
      Net     : in Out P2P_Network.Network_State;
      Block   : out    Block_Builder.Block;
      Result  : out    Consensus_Result
   ) is
      Build_Result : Block_Builder.Builder_Result;
      Added        : Natural;
      State_Root   : constant Hash256 := (others => 0);
      Block_Hash   : Hash256;
      Dummy_Sent   : Natural;
      Sign_Success : Boolean;
      Our_Address  : Contract_Address;
   begin
      --  Initialize output block
      Block.Is_Valid := False;
      Block.Is_Finalized := False;
      Block.TX_Count := 0;

      --  Derive proposer address from our key hash
      for I in Our_Address'Range loop
         Our_Address (I) := Cons.Our_Key_Hash (I);
      end loop;

      --  Start building the block
      Block_Builder.Start_Block (
         Builder     => Builder,
         Parent_Hash => Cons.Last_Block_Hash,
         Parent_Num  => Cons.Chain_Height,
         Timestamp   => Cons.Last_Slot_Time,
         Proposer    => Our_Address,
         Result      => Build_Result
      );

      if Build_Result /= Builder_OK then
         Result := Consensus_Block_Build_Failed;
         return;
      end if;

      --  Add transactions from mempool
      Block_Builder.Add_Transactions (
         Builder => Builder,
         Pool    => Pool,
         Max_TX  => Block_Builder.Max_TX_Per_Block,
         Added   => Added,
         Result  => Build_Result
      );

      --  Allow empty blocks in devnet (for liveness)
      if Build_Result /= Builder_OK and then
         Build_Result /= Builder_No_Transactions
      then
         Block_Builder.Abort_Block (Builder);
         Result := Consensus_Block_Build_Failed;
         return;
      end if;

      --  Seal the block
      Block_Builder.Seal_Block (
         Builder      => Builder,
         State_Root   => State_Root,
         Result       => Build_Result,
         Sealed_Block => Block
      );

      if Build_Result /= Builder_OK then
         Result := Consensus_Block_Build_Failed;
         return;
      end if;

      --  Sign the block header with our ML-DSA-87 secret key
      Sign_Block_Header (Cons, Block.Header, Sign_Success);

      if not Sign_Success then
         Result := Consensus_Block_Build_Failed;
         return;
      end if;

      --  Compute block hash for gossiping
      Block_Builder.Compute_Block_Hash (Block.Header, Block_Hash);

      --  Update our chain state
      Cons.Last_Block_Hash := Block_Hash;
      Cons.Last_Block_Slot := Cons.Current_Slot;

      --  Increment chain height
      if Cons.Chain_Height.Limbs (0) < Unsigned_64'Last then
         Cons.Chain_Height.Limbs (0) := Cons.Chain_Height.Limbs (0) + 1;
      end if;

      --  Update authority stats
      if Cons.Is_Authority then
         Cons.Authorities (Cons.Our_Index).Blocks_Produced :=
            Cons.Authorities (Cons.Our_Index).Blocks_Produced + 1;
         Cons.Authorities (Cons.Our_Index).Last_Block := Cons.Chain_Height;
      end if;

      --  Update consensus stats
      Cons.Stats.Blocks_Proposed := Cons.Stats.Blocks_Proposed + 1;

      --  Gossip the new block to peers
      P2P_Network.Gossip_Block (Net, Block_Hash, Cons.Chain_Height, Dummy_Sent);

      Result := Consensus_OK;
   end Produce_Block;

   ---------------------------------------------------------------------------
   --  Block Validation
   ---------------------------------------------------------------------------

   procedure Validate_Block (
      Cons   : in     Consensus_State;
      Block  : in     Block_Builder.Block;
      Result : out    Consensus_Result
   ) is
      Valid_Sig : Boolean;
      Proposer_Hash : Hash256;
   begin
      --  Check block validity flag
      if not Block.Is_Valid then
         Result := Consensus_Invalid_Signature;
         return;
      end if;

      --  Verify block signature
      Verify_Block_Signature (Cons, Block, Valid_Sig);
      if not Valid_Sig then
         Result := Consensus_Invalid_Signature;
         return;
      end if;

      --  Convert proposer address to hash for validation
      for I in Proposer_Hash'Range loop
         Proposer_Hash (I) := Block.Header.Proposer (I);
      end loop;

      --  Check authority is valid
      if not Is_Valid_Authority (Cons, Proposer_Hash) then
         Result := Consensus_Invalid_Authority;
         return;
      end if;

      Result := Consensus_OK;
   end Validate_Block;

   procedure Verify_Block_Signature (
      Cons   : in     Consensus_State;
      Block  : in     Block_Builder.Block;
      Valid  : out    Boolean
   ) is
      Proposer_Hash : Hash256;
      Auth_Index    : Authority_Index;
      Found         : Boolean;
      Header_Buf    : Anubis_Types.Byte_Array (0 .. Header_Sign_Buffer_Size - 1) :=
                        (others => 0);
      Header_Len    : Natural;
   begin
      Valid := False;

      --  Extract proposer address (which is the key hash)
      for I in Proposer_Hash'Range loop
         Proposer_Hash (I) := Block.Header.Proposer (I);
      end loop;

      --  Find the authority by their key hash (proposer address)
      Find_Authority_By_Hash (Cons, Proposer_Hash, Auth_Index, Found);

      if not Found then
         --  Proposer is not a valid authority
         return;
      end if;

      --  Serialize the block header (without signature) for verification
      Serialize_Header_For_Signing (Block.Header, Header_Buf, Header_Len);

      --  Verify the ML-DSA-87 signature using the authority's public key
      Valid := Anubis_MLDSA.Verify (
         PK  => Cons.Authorities (Auth_Index).Public_Key,
         Msg => Header_Buf (0 .. Header_Len - 1),
         Sig => Block.Header.Proposer_Sig
      );
   end Verify_Block_Signature;

   function Is_Valid_Authority (
      Cons     : Consensus_State;
      Key_Hash : Hash256
   ) return Boolean is
      Index : Authority_Index;
      Found : Boolean;
   begin
      Find_Authority_By_Hash (Cons, Key_Hash, Index, Found);
      return Found and then Cons.Authorities (Index).Is_Active;
   end Is_Valid_Authority;

   ---------------------------------------------------------------------------
   --  Finality
   ---------------------------------------------------------------------------

   function Is_Finalized (
      Cons         : Consensus_State;
      Block_Height : U256
   ) return Boolean is
   begin
      --  Single authority mode: immediate finality
      if Cons.Mode = Mode_Single_Authority then
         --  Compare heights (simplified for U256)
         return Block_Height.Limbs (0) <= Cons.Chain_Height.Limbs (0);
      end if;

      --  Multi-authority: finalized if enough blocks built on top
      --  Simple heuristic: finalized after 2/3 of authorities have produced
      declare
         Confirmations : constant Unsigned_64 :=
            Cons.Chain_Height.Limbs (0) - Block_Height.Limbs (0);
      begin
         return Confirmations >= Unsigned_64 (Finality_Threshold);
      end;
   end Is_Finalized;

   function Get_Finalized_Height (
      Cons : Consensus_State
   ) return U256 is
      Result : U256 := Cons.Chain_Height;
   begin
      --  Single authority: all blocks are finalized
      if Cons.Mode = Mode_Single_Authority then
         return Result;
      end if;

      --  Multi-authority: subtract finality threshold
      if Result.Limbs (0) >= Unsigned_64 (Finality_Threshold) then
         Result.Limbs (0) := Result.Limbs (0) - Unsigned_64 (Finality_Threshold);
      else
         Result := U256_Zero;
      end if;

      return Result;
   end Get_Finalized_Height;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Current_Slot (Cons : Consensus_State) return Slot_Number is
   begin
      return Cons.Current_Slot;
   end Get_Current_Slot;

   function Get_Current_Epoch (Cons : Consensus_State) return Natural is
   begin
      return Natural (Cons.Current_Slot / Unsigned_64 (Blocks_Per_Epoch));
   end Get_Current_Epoch;

   procedure Get_Slot_Info (
      Cons         : in     Consensus_State;
      Slot         : in     Slot_Number;
      Info         : out    Slot_Info
   ) is
   begin
      Info.Slot := Slot;
      Info.Epoch := Natural (Slot / Unsigned_64 (Blocks_Per_Epoch));
      Info.Slot_In_Epoch := Natural (Slot mod Unsigned_64 (Blocks_Per_Epoch));
      Info.Authority_Idx := Get_Slot_Authority (Cons, Slot);
      Info.Timestamp := Timestamp_From_Slot (Slot);
   end Get_Slot_Info;

   function Get_Authority_Count (Cons : Consensus_State) return Natural is
   begin
      return Cons.Authority_Count;
   end Get_Authority_Count;

   procedure Get_Authority (
      Cons  : in     Consensus_State;
      Index : in     Authority_Index;
      Auth  : out    Authority;
      Found : out    Boolean
   ) is
   begin
      --  Index is already constrained by Authority_Index (0 .. Max_Authorities - 1)
      if Cons.Authorities (Index).Is_Valid then
         Auth := Cons.Authorities (Index);
         Found := True;
      else
         Initialize_Authority (Auth);
         Found := False;
      end if;
   end Get_Authority;

   function Get_Stats (Cons : Consensus_State) return Consensus_Stats is
   begin
      return Cons.Stats;
   end Get_Stats;

   function Is_Authority (Cons : Consensus_State) return Boolean is
   begin
      return Cons.Is_Authority;
   end Is_Authority;

   function Is_Running (Cons : Consensus_State) return Boolean is
   begin
      return Cons.Is_Running;
   end Is_Running;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Slot_From_Timestamp (
      Timestamp : Unsigned_64
   ) return Slot_Number is
   begin
      if Timestamp = Unsigned_64 (Genesis_Timestamp) then
         return 0;
      end if;

      return Slot_Number ((Timestamp - Unsigned_64 (Genesis_Timestamp)) /
                          Unsigned_64 (Slot_Duration));
   end Slot_From_Timestamp;

   function Timestamp_From_Slot (
      Slot : Slot_Number
   ) return Unsigned_64 is
   begin
      return Unsigned_64 (Genesis_Timestamp) +
             (Unsigned_64 (Slot) * Unsigned_64 (Slot_Duration));
   end Timestamp_From_Slot;

   procedure Update_Chain_State (
      Cons        : in Out Consensus_State;
      Block_Hash  : in     Hash256;
      Block_Slot  : in     Slot_Number;
      Height      : in     U256
   ) is
   begin
      Cons.Last_Block_Hash := Block_Hash;
      Cons.Last_Block_Slot := Block_Slot;
      Cons.Chain_Height := Height;
   end Update_Chain_State;

end Consensus;
