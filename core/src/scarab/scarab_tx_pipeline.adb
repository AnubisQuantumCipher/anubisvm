pragma SPARK_Mode (On);

with Anubis_MLDSA;
with Scarab_Maat;

package body Scarab_TX_Pipeline with
   SPARK_Mode => On
is

   --  Type conversion helper
   subtype Local_Byte is Anubis_Types.Byte;

   function To_Local (B : Aegis_VM_Types.Byte) return Local_Byte is
      (Local_Byte (B)) with Inline;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      State          : out Pipeline_State
   ) is
      Empty_Item : TX_Item;
      Domain_Sep : constant TX_Hash32 := (others => 0);
   begin
      --  Initialize empty item template
      Empty_Item := (
         TX_Hash      => (others => 0),
         Signature    => (others => 0),
         Public_Key   => (others => 0),
         Mode         => Mode_Standard,
         Gas_Used     => 0,
         Verified     => False,
         In_Batch     => False,
         Batch_Index  => 0,
         Block_Height => 0
      );

      --  Initialize batch
      State.Current_Batch.Items := (others => Empty_Item);
      State.Current_Batch.Count := 0;
      State.Current_Batch.Block_Height := 0;
      State.Current_Batch.Finalized := False;
      State.Current_Batch.Has_Proof := False;

      --  Initialize KHNUM batch with domain separator
      Scarab_Khnum.Init_Batch (
         Batch        => State.Current_Batch.KHNUM_Batch,
         Domain_Sep   => Domain_Sep,
         Block_Height => 0
      );

      --  Initialize statistics
      State.Stats := (
         TX_Processed     => 0,
         TX_Batched       => 0,
         TX_Pre_Proven    => 0,
         TX_Standard      => 0,
         Total_Gas_Saved  => 0,
         Avg_Verify_Time  => 0,
         Blocks_Processed => 0
      );

      State.Pending_Proofs := 0;
      State.Is_Initialized := True;
   end Initialize;

   procedure Reset_For_Block (
      State          : in Out Pipeline_State;
      Block_Height   : Unsigned_64
   ) is
      Domain_Sep : TX_Hash32 := (others => 0);
      Empty_Item : TX_Item;
   begin
      --  Initialize empty item template
      Empty_Item := (
         TX_Hash      => (others => 0),
         Signature    => (others => 0),
         Public_Key   => (others => 0),
         Mode         => Mode_Standard,
         Gas_Used     => 0,
         Verified     => False,
         In_Batch     => False,
         Batch_Index  => 0,
         Block_Height => 0
      );

      --  Reset batch
      State.Current_Batch.Items := (others => Empty_Item);
      State.Current_Batch.Count := 0;
      State.Current_Batch.Block_Height := Block_Height;
      State.Current_Batch.Finalized := False;
      State.Current_Batch.Has_Proof := False;

      --  Encode block height in domain separator for uniqueness
      for I in 0 .. 7 loop
         Domain_Sep (I) := Local_Byte (Shift_Right (Block_Height, I * 8) and 16#FF#);
      end loop;

      --  Initialize new KHNUM batch
      Scarab_Khnum.Init_Batch (
         Batch        => State.Current_Batch.KHNUM_Batch,
         Domain_Sep   => Domain_Sep,
         Block_Height => Block_Height
      );

      --  Update statistics
      State.Stats.Blocks_Processed := State.Stats.Blocks_Processed + 1;
   end Reset_For_Block;

   ---------------------------------------------------------------------------
   --  Transaction Submission
   ---------------------------------------------------------------------------

   procedure Submit_TX (
      State          : in Out Pipeline_State;
      TX_Hash        : TX_Hash_Buffer;
      Signature      : TX_Sig_Buffer;
      Public_Key     : TX_PK_Buffer;
      Mode           : TX_Mode;
      Gas_Cost       : out Gas_Amount;
      Success        : out Boolean
   ) is
      Idx : Natural;
   begin
      --  Check capacity
      if State.Current_Batch.Count >= Max_TX_Per_Batch then
         Gas_Cost := 0;
         Success := False;
         return;
      end if;

      Idx := State.Current_Batch.Count;

      --  Copy TX data
      State.Current_Batch.Items (TX_Index (Idx)).TX_Hash := TX_Hash;
      State.Current_Batch.Items (TX_Index (Idx)).Signature := Signature;
      State.Current_Batch.Items (TX_Index (Idx)).Public_Key := Public_Key;

      --  Set metadata
      State.Current_Batch.Items (TX_Index (Idx)).Mode := Mode;
      State.Current_Batch.Items (TX_Index (Idx)).Block_Height := State.Current_Batch.Block_Height;
      State.Current_Batch.Items (TX_Index (Idx)).Verified := False;
      State.Current_Batch.Items (TX_Index (Idx)).In_Batch := False;

      --  Calculate gas cost based on mode
      Gas_Cost := Calculate_Gas (Mode, State.Current_Batch.Count + 1);
      State.Current_Batch.Items (TX_Index (Idx)).Gas_Used := Gas_Cost;

      --  Update statistics
      State.Stats.TX_Processed := State.Stats.TX_Processed + 1;
      case Mode is
         when Mode_Standard =>
            State.Stats.TX_Standard := State.Stats.TX_Standard + 1;
         when Mode_Batched =>
            State.Stats.TX_Batched := State.Stats.TX_Batched + 1;
         when Mode_Pre_Proven =>
            State.Stats.TX_Pre_Proven := State.Stats.TX_Pre_Proven + 1;
      end case;

      State.Current_Batch.Count := State.Current_Batch.Count + 1;
      Success := True;
   end Submit_TX;

   procedure Submit_Pre_Proven (
      State          : in Out Pipeline_State;
      TX_Hash        : TX_Hash_Buffer;
      Signature      : TX_Sig_Buffer;
      Public_Key     : TX_PK_Buffer;
      Proof          : Pre_Proof;
      Gas_Cost       : out Gas_Amount;
      Success        : out Boolean
   ) is
   begin
      --  Verify the pre-provided proof
      if not Verify_Pre_Proof (Proof, TX_Hash, Public_Key) then
         Gas_Cost := 0;
         Success := False;
         return;
      end if;

      --  Submit with pre-proven mode
      Submit_TX (
         State      => State,
         TX_Hash    => TX_Hash,
         Signature  => Signature,
         Public_Key => Public_Key,
         Mode       => Mode_Pre_Proven,
         Gas_Cost   => Gas_Cost,
         Success    => Success
      );

      --  Mark as already verified
      if Success and State.Current_Batch.Count > 0 then
         State.Current_Batch.Items (TX_Index (State.Current_Batch.Count - 1)).Verified := True;
      end if;
   end Submit_Pre_Proven;

   procedure Submit_Signed_TX (
      State          : in Out Pipeline_State;
      Signed_TX      : Signed_Transaction;
      Public_Key     : Anubis_MLDSA_Types.Public_Key;
      Mode           : TX_Mode;
      Gas_Cost       : out Gas_Amount;
      Success        : out Boolean
   ) is
      TX_Hash_Val    : Hash256;
      TX_Hash_Bytes  : TX_Hash_Buffer := (others => 0);
      Sig_Bytes      : TX_Sig_Buffer := (others => 0);
      PK_Bytes       : TX_PK_Buffer := (others => 0);
   begin
      --  Compute transaction hash
      Quantum_Transaction_Auth.Compute_Tx_Hash (Signed_TX, TX_Hash_Val);

      --  Convert hash (with padding for 64-byte message hash)
      for I in TX_Hash_Val'Range loop
         TX_Hash_Bytes (I) := To_Local (TX_Hash_Val (I));
      end loop;

      --  Convert signature (Anubis_Types.Byte -> Local_Byte is same)
      for I in Signed_TX.Signature'Range loop
         Sig_Bytes (I) := Signed_TX.Signature (I);
      end loop;

      --  Convert public key
      for I in Public_Key'Range loop
         PK_Bytes (I) := Public_Key (I);
      end loop;

      --  Submit to pipeline
      Submit_TX (
         State      => State,
         TX_Hash    => TX_Hash_Bytes,
         Signature  => Sig_Bytes,
         Public_Key => PK_Bytes,
         Mode       => Mode,
         Gas_Cost   => Gas_Cost,
         Success    => Success
      );
   end Submit_Signed_TX;

   ---------------------------------------------------------------------------
   --  Batch Processing
   ---------------------------------------------------------------------------

   procedure Add_To_Batch (
      State          : in Out Pipeline_State;
      Item_Index     : Natural;
      Success        : out Boolean
   ) is
      Itm : TX_Item renames State.Current_Batch.Items (TX_Index (Item_Index));
   begin
      if Itm.In_Batch or not Itm.Verified then
         Success := False;
         return;
      end if;

      --  Add to KHNUM batch
      Scarab_Khnum.Add_Signature (
         Batch        => State.Current_Batch.KHNUM_Batch,
         Signer_PK    => Itm.Public_Key,
         Message_Hash => Itm.TX_Hash,
         Signature    => Itm.Signature,
         Success      => Success
      );

      if Success then
         Itm.In_Batch := True;
         Itm.Batch_Index := Scarab_Khnum.Signature_Count (State.Current_Batch.KHNUM_Batch) - 1;
      end if;
   end Add_To_Batch;

   procedure Process_Pending (
      State          : in Out Pipeline_State;
      Processed      : out Natural;
      Failed         : out Natural
   ) is
      Valid : Boolean;
   begin
      Processed := 0;
      Failed := 0;

      for I in 0 .. State.Current_Batch.Count - 1 loop
         declare
            Itm : TX_Item renames State.Current_Batch.Items (TX_Index (I));
         begin
            if not Itm.Verified then
               --  Verify signature
               Valid := Verify_Single (
                  TX_Hash    => Itm.TX_Hash,
                  Signature  => Itm.Signature,
                  Public_Key => Itm.Public_Key
               );

               Itm.Verified := Valid;

               if Valid then
                  Processed := Processed + 1;

                  --  Add to batch if batched mode
                  if Itm.Mode = Mode_Batched then
                     declare
                        Add_Success : Boolean;
                     begin
                        Add_To_Batch (State, I, Add_Success);
                        if Add_Success then
                           --  Calculate gas savings
                           State.Stats.Total_Gas_Saved := State.Stats.Total_Gas_Saved +
                              Unsigned_64 (Individual_Verify_Gas - Batched_Verify_Gas);
                        end if;
                     end;
                  end if;
               else
                  Failed := Failed + 1;
               end if;
            end if;
         end;
      end loop;
   end Process_Pending;

   procedure Finalize_Batch (
      State          : in Out Pipeline_State;
      Aggregate      : out Scarab_Khnum.Aggregated_Signature;
      Success        : out Boolean
   ) is
   begin
      if State.Current_Batch.Finalized then
         Success := False;
         return;
      end if;

      --  Generate KHNUM aggregate
      Scarab_Khnum.Aggregate (
         Batch   => State.Current_Batch.KHNUM_Batch,
         Agg_Sig => Aggregate,
         Success => Success
      );

      if Success then
         State.Current_Batch.Finalized := True;
      end if;
   end Finalize_Batch;

   ---------------------------------------------------------------------------
   --  Proof Generation (HORUS Pipeline)
   ---------------------------------------------------------------------------

   procedure Generate_Block_Proof (
      State          : in     Pipeline_State;
      Block_Height   : Unsigned_64;
      Proof          : out    Block_Proof
   ) is
      pragma Unreferenced (State);
   begin
      --  Initialize proof
      Proof.Block_Height := Block_Height;
      Proof.TX_Count := 0;
      Proof.TX_Root := (others => 0);
      Proof.Sig_Root := (others => 0);
      Proof.Proof_Data := (others => 0);
      Proof.Proof_Len := 0;
      Proof.FRI_Root := (others => 0);
      Proof.Valid := False;

      --  Note: Actual STARK proof generation would call HORUS pipeline
      --  This is a stub that would be completed with:
      --  1. Convert KHNUM aggregate to circuit witness
      --  2. Run HORUS pipeline prover
      --  3. Extract FRI commitment and proof data
      --  4. Set Valid := True on success

      Proof.Valid := True;  -- Stub: assume success
   end Generate_Block_Proof;

   function Verify_Pre_Proof (
      Proof          : Pre_Proof;
      TX_Hash        : TX_Hash_Buffer;
      Public_Key     : TX_PK_Buffer
   ) return Boolean is
      pragma Unreferenced (Public_Key);
   begin
      --  Verify proof is valid and matches TX
      if not Proof.Valid or Proof.Proof_Len = 0 then
         return False;
      end if;

      --  Check TX hash matches
      for I in TX_Hash'Range loop
         if TX_Hash (I) /= Proof.TX_Hash (I) then
            return False;
         end if;
      end loop;

      --  Note: Actual STARK proof verification would call HORUS verifier
      --  This stub assumes valid proofs
      return True;
   end Verify_Pre_Proof;

   procedure Submit_To_MAAT (
      Proof          : Block_Proof;
      Success        : out Boolean
   ) is
      Batch : Scarab_Maat.Aggregation_Batch;
      Agg_Proof : Scarab_Maat.Aggregated_Proof;
      pragma Unreferenced (Agg_Proof);
      Public_Inputs : constant Anubis_Types.Byte_Array (0 .. 255) := (others => 0);
   begin
      if not Proof.Valid then
         Success := False;
         return;
      end if;

      --  Initialize MAAT batch
      Scarab_Maat.Init_Batch (Batch);

      --  Add block proof as a MAAT proof reference
      Scarab_Maat.Add_Proof (
         Batch         => Batch,
         Proof_Hash    => Proof.TX_Root,
         FRI_Root      => Proof.FRI_Root,
         Trace_Root    => Proof.Sig_Root,
         Public_Inputs => Public_Inputs (0 .. 63),
         Success       => Success
      );

      if not Success then
         return;
      end if;

      --  Build MAAT tree and aggregate
      Scarab_Maat.Build_Tree (Batch, Success);
      if not Success then
         return;
      end if;

      Scarab_Maat.Aggregate_Proofs (Batch, Agg_Proof, Success);
   end Submit_To_MAAT;

   ---------------------------------------------------------------------------
   --  Verification Operations
   ---------------------------------------------------------------------------

   function Verify_Single (
      TX_Hash        : TX_Hash_Buffer;
      Signature      : TX_Sig_Buffer;
      Public_Key     : TX_PK_Buffer
   ) return Boolean is
      Msg_32 : Anubis_Types.Byte_Array (0 .. 31);
      Sig    : Anubis_MLDSA_Types.Signature;
      PK     : Anubis_MLDSA_Types.Public_Key;
   begin
      --  Copy message (first 32 bytes of TX hash)
      for I in 0 .. 31 loop
         Msg_32 (I) := TX_Hash (I);
      end loop;

      --  Copy signature
      for I in Signature'Range loop
         Sig (I) := Signature (I);
      end loop;

      --  Copy public key
      for I in Public_Key'Range loop
         PK (I) := Public_Key (I);
      end loop;

      --  Verify ML-DSA-87 signature
      return Anubis_MLDSA.Verify (
         PK  => PK,
         Msg => Msg_32,
         Sig => Sig
      );
   end Verify_Single;

   function Verify_Batch (
      Aggregate      : Scarab_Khnum.Aggregated_Signature
   ) return Boolean is
   begin
      return Scarab_Khnum.Verify_Aggregated (Aggregate);
   end Verify_Batch;

   function Verify_Block_Proof (
      Proof          : Block_Proof
   ) return Boolean is
   begin
      --  Note: Actual STARK verification would call HORUS verifier
      --  This stub checks basic validity
      return Proof.Valid and Proof.TX_Count > 0;
   end Verify_Block_Proof;

   ---------------------------------------------------------------------------
   --  Gas Calculation
   ---------------------------------------------------------------------------

   function Calculate_Gas (
      Mode           : TX_Mode;
      Batch_Size     : Natural
   ) return Gas_Amount is
      pragma Unreferenced (Batch_Size);
   begin
      case Mode is
         when Mode_Standard =>
            return Individual_Verify_Gas;
         when Mode_Batched =>
            return Batched_Verify_Gas;
         when Mode_Pre_Proven =>
            return Pre_Proven_Gas;
      end case;
   end Calculate_Gas;

   function Calculate_Savings (
      Batch_Size     : Natural
   ) return Unsigned_64 is
   begin
      return Unsigned_64 (Batch_Size) *
             (Unsigned_64 (Individual_Verify_Gas) - Unsigned_64 (Batched_Verify_Gas));
   end Calculate_Savings;

   ---------------------------------------------------------------------------
   --  Statistics and Metrics
   ---------------------------------------------------------------------------

   function Get_Stats (
      State          : Pipeline_State
   ) return Pipeline_Stats is
   begin
      return State.Stats;
   end Get_Stats;

   function Get_Batch_Count (
      State          : Pipeline_State
   ) return Natural is
   begin
      return State.Current_Batch.Count;
   end Get_Batch_Count;

   function Is_Batch_Full (
      State          : Pipeline_State
   ) return Boolean is
   begin
      return State.Current_Batch.Count >= Max_TX_Per_Batch;
   end Is_Batch_Full;

   ---------------------------------------------------------------------------
   --  HORUS Pipeline Integration
   ---------------------------------------------------------------------------

   procedure Register_Prover (
      Tier           : Scarab_Horus.Prover_Tier;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Tier);
   begin
      --  Note: Actual prover registration would:
      --  1. Check hardware requirements for tier
      --  2. Register with HORUS coordinator
      --  3. Stake required tokens
      Success := True;  -- Stub
   end Register_Prover;

   procedure Start_Pipeline (
      State          : in Out Pipeline_State;
      Block_Height   : Unsigned_64;
      Success        : out Boolean
   ) is
   begin
      if not State.Is_Initialized then
         Success := False;
         return;
      end if;

      Reset_For_Block (State, Block_Height);
      Success := True;
   end Start_Pipeline;

   function Pipeline_Ready (
      State          : Pipeline_State
   ) return Boolean is
   begin
      return State.Is_Initialized and not State.Current_Batch.Finalized;
   end Pipeline_Ready;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Batch (
      State          : in Out Pipeline_State
   ) is
   begin
      for I in TX_Index loop
         Zeroize_Item (State.Current_Batch.Items (I));
      end loop;
      State.Current_Batch.Count := 0;
      State.Current_Batch.Finalized := False;
      Scarab_Khnum.Zeroize_Batch (State.Current_Batch.KHNUM_Batch);
   end Zeroize_Batch;

   procedure Zeroize_Item (
      Item           : in Out TX_Item
   ) is
   begin
      Item.TX_Hash := (others => 0);
      Item.Signature := (others => 0);
      Item.Public_Key := (others => 0);
      Item.Mode := Mode_Standard;
      Item.Gas_Used := 0;
      Item.Verified := False;
      Item.In_Batch := False;
      Item.Batch_Index := 0;
      Item.Block_Height := 0;
   end Zeroize_Item;

   procedure Zeroize_Proof (
      Proof          : in Out Block_Proof
   ) is
   begin
      Proof.Block_Height := 0;
      Proof.TX_Count := 0;
      Proof.TX_Root := (others => 0);
      Proof.Sig_Root := (others => 0);
      Proof.Proof_Data := (others => 0);
      Proof.Proof_Len := 0;
      Proof.FRI_Root := (others => 0);
      Proof.Valid := False;
   end Zeroize_Proof;

end Scarab_TX_Pipeline;
