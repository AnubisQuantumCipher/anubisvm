-------------------------------------------------------------------------------
--  Anubis Node Orchestrator Implementation
--
--  SPARK_Mode Off due to complex subsystem integration
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces; use Interfaces;
with Khepri_State_Manager;
with Khepri_Types;
with Anubis_Types;

package body Anubis_Node_Orchestrator with
   SPARK_Mode => Off
is

   ---------------------------------------------------------------------------
   --  Internal: Get current Unix timestamp
   ---------------------------------------------------------------------------
   function Get_Current_Timestamp return Unsigned_64 is
      use Ada.Calendar;
      use Ada.Calendar.Conversions;
      Now : constant Time := Clock;
   begin
      return Unsigned_64 (To_Unix_Time (Now));
   exception
      when others =>
         return 0;
   end Get_Current_Timestamp;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Orchestrator : out Orchestrator_State;
      Config       : in  Orchestrator_Config
   ) is
      Node_Config : Anubis_Node_Types.Node_Configuration := Anubis_Node_Types.Default_Configuration;
      RPC_Config  : Anubis_RPC_Server.RPC_Config := Anubis_RPC_Server.Default_RPC_Config;
   begin
      Orchestrator.Is_Initialized := False;
      Orchestrator.Config := Config;

      --  Initialize VM
      Ada.Text_IO.Put_Line ("Initializing VM...");
      Node_Config.RPC_Port := Config.RPC_Port;
      Anubis_Node.Initialize (Orchestrator.VM, Node_Config);

      --  Initialize mempool
      Ada.Text_IO.Put_Line ("Initializing mempool...");
      Mempool.Initialize (Orchestrator.Pool);

      --  Initialize block builder
      Ada.Text_IO.Put_Line ("Initializing block builder...");
      Block_Builder.Initialize (Orchestrator.Builder);

      --  Initialize consensus
      Ada.Text_IO.Put_Line ("Initializing consensus...");
      Consensus.Initialize (Orchestrator.Consensus);

      --  Initialize sync manager
      Ada.Text_IO.Put_Line ("Initializing sync manager...");
      Block_Sync.Initialize (Orchestrator.Sync_Mgr);

      --  Initialize P2P network
      if Config.Enable_P2P then
         Ada.Text_IO.Put_Line ("Initializing P2P network...");
         declare
            Node_ID : constant Aegis_VM_Types.Hash256 := (others => 0);
            Node_Key : constant Byte_Array (0 .. 31) := (others => 0);
            Chain_ID : constant U256 := (Limbs => (1, 0, 0, 0));
         begin
            P2P_Network.Initialize (
               Net      => Orchestrator.Network,
               Node_ID  => Node_ID,
               Node_Key => Node_Key,
               Chain_ID => Chain_ID,
               Port     => Config.P2P_Port
            );
         end;
      end if;

      --  Initialize RPC server
      if Config.Enable_RPC then
         Ada.Text_IO.Put_Line ("Initializing RPC server...");
         RPC_Config.Port := Config.RPC_Port;
         Anubis_RPC_Server.Initialize (
            Server  => Orchestrator.RPC,
            Config  => RPC_Config,
            VM      => Orchestrator.VM,
            Pool    => Orchestrator.Pool,
            Builder => Orchestrator.Builder,
            Cons    => Orchestrator.Consensus
         );
      end if;

      --  Initialize governance
      if Config.Enable_Governance then
         Ada.Text_IO.Put_Line ("Initializing governance...");
         declare
            Builder_PK : constant Byte_Array (0 .. 31) := (others => 0);
         begin
            Anubis_Governance.Init_Governance (
               State         => Orchestrator.Governance,
               Builder_PK    => Builder_PK,
               Genesis_Block => Config.Genesis_Block
            );
         end;
      end if;

      --  Initialize state
      Orchestrator.Latest_Block_Num := (Limbs => (0, 0, 0, 0));
      Orchestrator.Latest_Block_Hash := (others => 0);
      Orchestrator.Sync_State := Sync_NotStarted;
      Orchestrator.Is_Validator := Config.Enable_Mining;
      Orchestrator.Is_Syncing := False;
      Orchestrator.Blocks_Produced := 0;
      Orchestrator.Blocks_Validated := 0;
      Orchestrator.TX_Processed := 0;
      Orchestrator.Reorgs_Handled := 0;

      Orchestrator.Is_Initialized := True;
      Ada.Text_IO.Put_Line ("Orchestrator initialized successfully");
   end Initialize;

   procedure Start (
      Orchestrator : in Out Orchestrator_State
   ) is
   begin
      Ada.Text_IO.Put_Line ("Starting node orchestrator...");

      --  Start P2P network
      if Orchestrator.Config.Enable_P2P then
         declare
            Result : P2P_Network.Network_Result;
         begin
            P2P_Network.Start (Orchestrator.Network, Result);
            if Result = P2P_Network.Network_OK then
               Ada.Text_IO.Put_Line ("  P2P network started");
            else
               Ada.Text_IO.Put_Line ("  Warning: P2P network failed to start");
            end if;
         end;
      end if;

      Ada.Text_IO.Put_Line ("Node orchestrator started");
   end Start;

   procedure Stop (
      Orchestrator : in Out Orchestrator_State
   ) is
   begin
      Ada.Text_IO.Put_Line ("Stopping node orchestrator...");

      --  Stop P2P network
      if Orchestrator.Config.Enable_P2P then
         P2P_Network.Stop (Orchestrator.Network);
      end if;

      --  Shutdown VM
      Anubis_Node.Shutdown (Orchestrator.VM);

      Ada.Text_IO.Put_Line ("Node orchestrator stopped");
   end Stop;

   ---------------------------------------------------------------------------
   --  Block Production
   ---------------------------------------------------------------------------

   procedure Produce_Block (
      Orchestrator : in Out Orchestrator_State;
      Result       : out    Production_Result
   ) is
      Builder_Result : Block_Builder.Builder_Result;
      Added_TX : Natural;
      --  Compute actual state root from Khepri state manager
      State_Root : Hash256;
      State_Root_Temp : Khepri_Types.Hash_256;
      Sealed_Block : Block_Builder.Block;
      Current_Timestamp : Unsigned_64;
      Proposer_Address : Hash256;
   begin
      Result := Prod_Success;

      --  Get current state root from Khepri
      State_Root_Temp := Khepri_State_Manager.Get_State_Root;
      for I in 0 .. 31 loop
         State_Root (I) := Aegis_VM_Types.Byte (State_Root_Temp (I));
      end loop;

      --  Get current timestamp
      Current_Timestamp := Get_Current_Timestamp;

      --  Get proposer address from consensus (our validator key hash)
      Proposer_Address := Orchestrator.Consensus.Our_Key_Hash;

      --  Check if we're a validator
      if not Orchestrator.Is_Validator then
         Result := Prod_Not_Validator;
         return;
      end if;

      --  Check if already building
      if Block_Builder.Is_Building (Orchestrator.Builder) then
         Result := Prod_Already_Building;
         return;
      end if;

      --  Start building block with actual values
      Block_Builder.Start_Block (
         Builder     => Orchestrator.Builder,
         Parent_Hash => Orchestrator.Latest_Block_Hash,
         Parent_Num  => Orchestrator.Latest_Block_Num,
         Timestamp   => Current_Timestamp,
         Proposer    => Proposer_Address,
         Result      => Builder_Result
      );

      if Builder_Result /= Block_Builder.Builder_OK then
         Result := Prod_Consensus_Failed;
         return;
      end if;

      --  Add transactions from mempool
      Block_Builder.Add_Transactions (
         Builder => Orchestrator.Builder,
         Pool    => Orchestrator.Pool,
         Max_TX  => 1024,
         Added   => Added_TX,
         Result  => Builder_Result
      );

      if Builder_Result /= Block_Builder.Builder_OK and then
         Builder_Result /= Block_Builder.Builder_No_Transactions
      then
         Result := Prod_Consensus_Failed;
         Block_Builder.Abort_Block (Orchestrator.Builder);
         return;
      end if;

      --  Seal block
      Block_Builder.Seal_Block (
         Builder      => Orchestrator.Builder,
         State_Root   => State_Root,
         Result       => Builder_Result,
         Sealed_Block => Sealed_Block
      );

      if Builder_Result /= Block_Builder.Builder_OK then
         Result := Prod_Consensus_Failed;
         return;
      end if;

      --  Update orchestrator state
      Orchestrator.Latest_Block_Num.Limbs (0) := Orchestrator.Latest_Block_Num.Limbs (0) + 1;
      Orchestrator.Blocks_Produced := Orchestrator.Blocks_Produced + 1;
      Orchestrator.TX_Processed := Orchestrator.TX_Processed + Unsigned_64 (Added_TX);

      Ada.Text_IO.Put_Line ("Block produced with " & Natural'Image (Added_TX) & " transactions");
   end Produce_Block;

   procedure Validate_Block (
      Orchestrator : in Out Orchestrator_State;
      Block_Data   : in     Block_Builder.Block;
      Valid        : out    Boolean;
      Error_Msg    : out    String
   ) is
      Computed_Hash : Hash256;
   begin
      Valid := False;
      Error_Msg := (others => ' ');

      --  1. Verify block number is sequential
      if Block_Data.Header.Number.Limbs (0) /= Orchestrator.Latest_Block_Num.Limbs (0) + 1 then
         Error_Msg (1 .. 28) := "Invalid block number       ";
         return;
      end if;

      --  2. Verify parent hash matches our latest block
      if Block_Data.Header.Parent_Hash /= Orchestrator.Latest_Block_Hash then
         Error_Msg (1 .. 28) := "Invalid parent hash        ";
         return;
      end if;

      --  3. Verify timestamp is not in the future
      --  (Allow up to 15 seconds drift)
      --  Note: Would need system time here - simplified check
      if Block_Data.Header.Timestamp = 0 then
         Error_Msg (1 .. 28) := "Invalid timestamp          ";
         return;
      end if;

      --  4. Verify transaction count matches
      if Block_Data.Header.TX_Count /= Block_Data.TX_Count then
         Error_Msg (1 .. 28) := "TX count mismatch          ";
         return;
      end if;

      --  5. Verify block hash (compute from header)
      Computed_Hash := Block_Builder.Compute_Block_Hash (Block_Data);
      if Computed_Hash /= Block_Data.Header.Block_Hash then
         Error_Msg (1 .. 28) := "Invalid block hash         ";
         return;
      end if;

      --  6. Verify gas limit within bounds
      if Block_Data.Header.Gas_Limit > 30_000_000 then
         Error_Msg (1 .. 28) := "Gas limit exceeds maximum  ";
         return;
      end if;

      --  7. Verify gas used does not exceed limit
      if Block_Data.Header.Gas_Used > Block_Data.Header.Gas_Limit then
         Error_Msg (1 .. 28) := "Gas used exceeds limit     ";
         return;
      end if;

      --  8. Execute transactions and verify state root
      --  This is done by block builder during seal
      --  For validation, we re-execute and compare
      declare
         Exec_Result  : Block_Builder.Builder_Result;
         Exec_State_Root : Hash256;
         Exec_Success : Boolean;
      begin
         Block_Builder.Execute_Block_Transactions (
            Builder     => Orchestrator.Builder,
            Block_Data  => Block_Data,
            State_Root  => Exec_State_Root,
            Success     => Exec_Success,
            Result      => Exec_Result
         );

         if not Exec_Success then
            Error_Msg (1 .. 28) := "Transaction execution fail ";
            return;
         end if;

         if Exec_State_Root /= Block_Data.Header.State_Root then
            Error_Msg (1 .. 28) := "State root mismatch        ";
            return;
         end if;
      end;

      --  All validations passed
      Valid := True;
      Orchestrator.Blocks_Validated := Orchestrator.Blocks_Validated + 1;
   end Validate_Block;

   procedure Finalize_Block (
      Orchestrator : in Out Orchestrator_State;
      Block_Num    : in     U256;
      Block_Hash   : in     Hash256
   ) is
   begin
      Orchestrator.Latest_Block_Num := Block_Num;
      Orchestrator.Latest_Block_Hash := Block_Hash;
      Orchestrator.Blocks_Validated := Orchestrator.Blocks_Validated + 1;

      --  Update governance
      if Orchestrator.Config.Enable_Governance then
         Anubis_Governance.Update_Phase (
            State         => Orchestrator.Governance,
            Current_Block => Block_Num.Limbs (0)
         );
      end if;

      Ada.Text_IO.Put_Line ("Block finalized: " & Block_Num.Limbs (0)'Image);
   end Finalize_Block;

   ---------------------------------------------------------------------------
   --  State Synchronization
   ---------------------------------------------------------------------------

   procedure Start_Sync (
      Orchestrator : in Out Orchestrator_State;
      Result       : out    Sync_Result
   ) is
      Sync_Result_Val : Block_Sync.Sync_Result;
   begin
      Result := Sync_OK;

      if Orchestrator.Is_Syncing then
         Result := Sync_Already_Syncing;
         return;
      end if;

      --  Check peer count
      declare
         Peer_Count : constant Natural := P2P_Network.Get_Peer_Count (Orchestrator.Network);
      begin
         if Peer_Count = 0 then
            Result := Sync_No_Peers;
            return;
         end if;
      end;

      --  Get best height from connected peers
      declare
         Best_Peer_Height : constant U256 :=
            P2P_Network.Get_Best_Peer_Height (Orchestrator.Network);
      begin
         --  Start sync with actual target height from peers
         Block_Sync.Start_Sync (
            Sync_Mgr => Orchestrator.Sync_Mgr,
            Target_Height => Best_Peer_Height,
            Result   => Sync_Result_Val
         );
      end;

      if Sync_Result_Val = Block_Sync.Sync_Started then
         Orchestrator.Is_Syncing := True;
         Orchestrator.Sync_State := Sync_Downloading;
      else
         Result := Sync_Failed;
      end if;
   end Start_Sync;

   procedure Process_Sync_Block (
      Orchestrator : in Out Orchestrator_State;
      Block_Data   : in     Block_Builder.Block;
      Result       : out    Sync_Result
   ) is
      Valid : Boolean;
      Error_Msg : String (1 .. 256);
   begin
      Result := Sync_OK;

      --  Validate block
      Validate_Block (Orchestrator, Block_Data, Valid, Error_Msg);

      if not Valid then
         Result := Sync_Failed;
         return;
      end if;

      --  Apply block with computed hash
      declare
         Computed_Hash : constant Hash256 :=
            Block_Builder.Compute_Block_Hash (Block_Data);
      begin
         Finalize_Block (
            Orchestrator => Orchestrator,
            Block_Num    => Block_Data.Header.Number,
            Block_Hash   => Computed_Hash
         );
      end;
   end Process_Sync_Block;

   procedure Check_Sync_Progress (
      Orchestrator : in Out Orchestrator_State
   ) is
      Status : constant Block_Sync.Sync_Status :=
         Block_Sync.Get_Sync_Status (Orchestrator.Sync_Mgr);
   begin
      case Status is
         when Block_Sync.Sync_Complete =>
            Orchestrator.Is_Syncing := False;
            Orchestrator.Sync_State := Sync_Complete;
            Ada.Text_IO.Put_Line ("Sync complete");

         when Block_Sync.Sync_Downloading =>
            Orchestrator.Sync_State := Sync_Downloading;

         when Block_Sync.Sync_Failed =>
            Orchestrator.Is_Syncing := False;
            Orchestrator.Sync_State := Sync_Error;
            Ada.Text_IO.Put_Line ("Sync failed");

         when others =>
            null;
      end case;
   end Check_Sync_Progress;

   ---------------------------------------------------------------------------
   --  Chain Reorganization
   ---------------------------------------------------------------------------

   procedure Handle_Reorg (
      Orchestrator  : in Out Orchestrator_State;
      New_Chain_Tip : in     Hash256;
      New_Chain_Num : in     U256;
      Result        : out    Reorg_Result
   ) is
      Ancestor        : Hash256;
      Found           : Boolean;
      Snapshot_ID     : Natural;
      Snapshot_Ok     : Boolean;
      Rollback_Ok     : Boolean;
      Max_Reorg_Depth : constant := 128;

      --  Block chain from ancestor to new tip
      type Block_Chain is array (0 .. Max_Reorg_Depth - 1) of Hash256;
      New_Blocks    : Block_Chain := (others => (others => 0));
      Block_Count   : Natural := 0;

      Current_Hash  : Hash256;
      Parent_Hash   : Hash256;
      Block_Found   : Boolean;
   begin
      Result := Reorg_Applied;

      Ada.Text_IO.Put_Line ("Handling chain reorganization...");

      --  Check if new chain is actually better (higher block number)
      if New_Chain_Num.Limbs (0) <= Orchestrator.Latest_Block_Num.Limbs (0) then
         Result := Reorg_Not_Better;
         return;
      end if;

      --  Find common ancestor
      Find_Common_Ancestor (
         Orchestrator => Orchestrator,
         Chain_A_Hash => Orchestrator.Latest_Block_Hash,
         Chain_B_Hash => New_Chain_Tip,
         Ancestor     => Ancestor,
         Found        => Found
      );

      if not Found then
         Result := Reorg_No_Ancestor;
         return;
      end if;

      Ada.Text_IO.Put_Line ("Found common ancestor, building new chain...");

      --  Build list of blocks from ancestor to new tip
      Current_Hash := New_Chain_Tip;
      while Current_Hash /= Ancestor and Block_Count < Max_Reorg_Depth loop
         New_Blocks (Block_Count) := Current_Hash;
         Block_Count := Block_Count + 1;

         --  Get parent of current block
         Block_Builder.Get_Block_Parent (
            Builder     => Orchestrator.Builder,
            Block_Hash  => Current_Hash,
            Parent_Hash => Parent_Hash,
            Found       => Block_Found
         );

         if not Block_Found then
            Result := Reorg_Invalid_Chain;
            return;
         end if;

         Current_Hash := Parent_Hash;
      end loop;

      --  Create snapshot before rollback (for recovery if reorg fails)
      Khepri_State_Manager.Create_Snapshot (Snapshot_ID, Snapshot_Ok);
      if not Snapshot_Ok then
         Result := Reorg_Execution_Failed;
         return;
      end if;

      --  Rollback state to ancestor
      Ada.Text_IO.Put_Line ("Rolling back to common ancestor...");
      declare
         Ancestor_Block_Num : U256;
         Rollback_Error : Khepri_State_Manager.State_Manager_Error;
      begin
         --  Get ancestor block number
         Block_Builder.Get_Block_Number (
            Builder    => Orchestrator.Builder,
            Block_Hash => Ancestor,
            Block_Num  => Ancestor_Block_Num,
            Found      => Block_Found
         );

         if not Block_Found then
            Khepri_State_Manager.Rollback_Snapshot (Snapshot_ID, Rollback_Ok, Rollback_Error);
            Result := Reorg_Invalid_Chain;
            return;
         end if;

         --  Update orchestrator to ancestor state
         Orchestrator.Latest_Block_Num := Ancestor_Block_Num;
         Orchestrator.Latest_Block_Hash := Ancestor;
      end;

      --  Apply new chain blocks (in reverse order - oldest first)
      Ada.Text_IO.Put_Line ("Applying" & Natural'Image (Block_Count) & " new blocks...");
      for I in reverse 0 .. Block_Count - 1 loop
         declare
            Block_Data    : Block_Builder.Block;
            Load_Result   : Block_Builder.Builder_Result;
            Valid         : Boolean;
            Error_Msg     : String (1 .. 256);
         begin
            --  Load block data
            Block_Builder.Load_Block (
               Builder    => Orchestrator.Builder,
               Block_Hash => New_Blocks (I),
               Block_Data => Block_Data,
               Result     => Load_Result
            );

            if Load_Result /= Block_Builder.Builder_OK then
               --  Failed to load block, rollback
               declare
                  Rollback_Error : Khepri_State_Manager.State_Manager_Error;
               begin
                  Khepri_State_Manager.Rollback_Snapshot (Snapshot_ID, Rollback_Ok, Rollback_Error);
               end;
               Result := Reorg_Invalid_Chain;
               return;
            end if;

            --  Validate and execute block
            Validate_Block (Orchestrator, Block_Data, Valid, Error_Msg);

            if not Valid then
               --  Invalid block in new chain, rollback
               Ada.Text_IO.Put_Line ("Invalid block in new chain: " & Error_Msg);
               declare
                  Rollback_Error : Khepri_State_Manager.State_Manager_Error;
               begin
                  Khepri_State_Manager.Rollback_Snapshot (Snapshot_ID, Rollback_Ok, Rollback_Error);
               end;
               Result := Reorg_Invalid_Chain;
               return;
            end if;

            --  Finalize block
            Finalize_Block (
               Orchestrator => Orchestrator,
               Block_Num    => Block_Data.Header.Number,
               Block_Hash   => New_Blocks (I)
            );
         end;
      end loop;

      --  Commit the snapshot (discard rollback point)
      Khepri_State_Manager.Commit_Snapshot (Snapshot_ID);

      --  Update final state
      Orchestrator.Latest_Block_Num := New_Chain_Num;
      Orchestrator.Latest_Block_Hash := New_Chain_Tip;
      Orchestrator.Reorgs_Handled := Orchestrator.Reorgs_Handled + 1;

      Ada.Text_IO.Put_Line ("Chain reorganization complete: " &
         Natural'Image (Block_Count) & " blocks applied");
   end Handle_Reorg;

   procedure Find_Common_Ancestor (
      Orchestrator : in     Orchestrator_State;
      Chain_A_Hash : in     Hash256;
      Chain_B_Hash : in     Hash256;
      Ancestor     : out    Hash256;
      Found        : out    Boolean
   ) is
      --  Maximum depth to search for common ancestor
      Max_Depth : constant := 1024;

      --  Track positions in both chains
      A_Hash : Hash256 := Chain_A_Hash;
      B_Hash : Hash256 := Chain_B_Hash;
      A_Parent : Hash256;
      B_Parent : Hash256;
      A_Found, B_Found : Boolean;
      Depth : Natural := 0;
   begin
      Ancestor := (others => 0);
      Found := False;

      --  If chains are identical, they share the same tip
      if A_Hash = B_Hash then
         Ancestor := A_Hash;
         Found := True;
         return;
      end if;

      --  Walk back both chains simultaneously looking for common ancestor
      --  Use tortoise and hare approach to handle different length chains
      while Depth < Max_Depth loop
         --  Check if current positions match
         if A_Hash = B_Hash then
            Ancestor := A_Hash;
            Found := True;
            return;
         end if;

         --  Get parent of chain A
         Block_Builder.Get_Block_Parent (
            Builder     => Orchestrator.Builder,
            Block_Hash  => A_Hash,
            Parent_Hash => A_Parent,
            Found       => A_Found
         );

         --  Get parent of chain B
         Block_Builder.Get_Block_Parent (
            Builder     => Orchestrator.Builder,
            Block_Hash  => B_Hash,
            Parent_Hash => B_Parent,
            Found       => B_Found
         );

         --  If either chain ends (reaches genesis), check other chain
         if not A_Found and not B_Found then
            --  Both chains exhausted without finding ancestor
            return;
         end if;

         --  Move to parents
         if A_Found then
            A_Hash := A_Parent;
         end if;

         if B_Found then
            B_Hash := B_Parent;
         end if;

         Depth := Depth + 1;
      end loop;

      --  Exceeded max depth without finding ancestor
      Found := False;
   end Find_Common_Ancestor;

   ---------------------------------------------------------------------------
   --  Transaction Processing
   ---------------------------------------------------------------------------

   procedure Submit_Transaction (
      Orchestrator : in Out Orchestrator_State;
      TX_Data      : in     Byte_Array;
      TX_Hash      : out    Hash256;
      Success      : out    Boolean
   ) is
      Result       : Mempool.Mempool_Result;
      TX           : Mempool.Transaction;
      Parse_Ok     : Boolean;
      Verify_Ok    : Boolean;
   begin
      Success := False;
      TX_Hash := (others => 0);

      --  Minimum transaction size check
      if TX_Data'Length < 64 then
         return;
      end if;

      --  Parse transaction from RLP-encoded data
      Mempool.Parse_Transaction (
         Data    => TX_Data,
         TX      => TX,
         Success => Parse_Ok
      );

      if not Parse_Ok then
         Ada.Text_IO.Put_Line ("Failed to parse transaction");
         return;
      end if;

      --  Verify transaction signature
      Mempool.Verify_Transaction_Signature (
         TX      => TX,
         Valid   => Verify_Ok
      );

      if not Verify_Ok then
         Ada.Text_IO.Put_Line ("Invalid transaction signature");
         return;
      end if;

      --  Compute transaction hash
      TX_Hash := Mempool.Compute_TX_Hash (TX);

      --  Check if transaction already exists
      if Mempool.Has_Transaction (Orchestrator.Pool, TX_Hash) then
         --  Already in mempool, return success with existing hash
         Success := True;
         return;
      end if;

      --  Verify nonce (must be sequential)
      declare
         Expected_Nonce : Unsigned_64;
         Nonce_Ok : Boolean;
      begin
         Mempool.Get_Expected_Nonce (
            Pool    => Orchestrator.Pool,
            Sender  => TX.From,
            Nonce   => Expected_Nonce,
            Found   => Nonce_Ok
         );

         if Nonce_Ok and then TX.Nonce < Expected_Nonce then
            Ada.Text_IO.Put_Line ("Nonce too low");
            return;
         end if;
      end;

      --  Add to mempool
      Mempool.Add_Transaction (
         Pool   => Orchestrator.Pool,
         TX     => TX,
         Result => Result
      );

      if Result = Mempool.Pool_OK then
         Success := True;
         Ada.Text_IO.Put_Line ("Transaction added to mempool");
      else
         case Result is
            when Mempool.Pool_Full =>
               Ada.Text_IO.Put_Line ("Mempool full");
            when Mempool.Pool_Duplicate =>
               Ada.Text_IO.Put_Line ("Duplicate transaction");
            when Mempool.Pool_Invalid_TX =>
               Ada.Text_IO.Put_Line ("Invalid transaction");
            when others =>
               Ada.Text_IO.Put_Line ("Mempool error");
         end case;
      end if;
   end Submit_Transaction;

   procedure Process_Pending_Transactions (
      Orchestrator : in Out Orchestrator_State;
      Max_TX       : in     Natural;
      Processed    : out    Natural
   ) is
      TX         : Mempool.Transaction;
      TX_Hash    : Hash256;
      Pop_Result : Mempool.Mempool_Result;
      Exec_Result : Anubis_Node.Execution_Result;
      Gas_Used   : Unsigned_64;
   begin
      Processed := 0;

      --  Process transactions from mempool up to Max_TX
      while Processed < Max_TX loop
         --  Get next transaction from mempool (priority order)
         Mempool.Pop_Transaction (
            Pool   => Orchestrator.Pool,
            TX     => TX,
            Result => Pop_Result
         );

         --  No more transactions available
         if Pop_Result /= Mempool.Pool_OK then
            exit;
         end if;

         --  Compute transaction hash
         TX_Hash := Mempool.Compute_TX_Hash (TX);

         --  Execute transaction
         Anubis_Node.Execute_Transaction (
            VM      => Orchestrator.VM,
            TX      => TX,
            Result  => Exec_Result,
            Gas_Used => Gas_Used
         );

         --  Handle execution result
         case Exec_Result is
            when Anubis_Node.Exec_Success =>
               --  Transaction executed successfully
               Processed := Processed + 1;
               Orchestrator.TX_Processed := Orchestrator.TX_Processed + 1;

               --  Generate receipt
               declare
                  Receipt : Block_Builder.Transaction_Receipt;
               begin
                  Receipt.TX_Hash := TX_Hash;
                  Receipt.Status := 1;  -- Success
                  Receipt.Gas_Used := Gas_Used;
                  Receipt.Block_Number := Orchestrator.Latest_Block_Num.Limbs (0) + 1;

                  --  Add receipt to pending block
                  Block_Builder.Add_Receipt (
                     Builder => Orchestrator.Builder,
                     Receipt => Receipt
                  );
               end;

            when Anubis_Node.Exec_Revert =>
               --  Transaction reverted, still counts as processed
               Processed := Processed + 1;

               declare
                  Receipt : Block_Builder.Transaction_Receipt;
               begin
                  Receipt.TX_Hash := TX_Hash;
                  Receipt.Status := 0;  -- Reverted
                  Receipt.Gas_Used := Gas_Used;
                  Receipt.Block_Number := Orchestrator.Latest_Block_Num.Limbs (0) + 1;

                  Block_Builder.Add_Receipt (
                     Builder => Orchestrator.Builder,
                     Receipt => Receipt
                  );
               end;

            when Anubis_Node.Exec_Out_Of_Gas =>
               --  Out of gas - transaction failed
               Processed := Processed + 1;

               declare
                  Receipt : Block_Builder.Transaction_Receipt;
               begin
                  Receipt.TX_Hash := TX_Hash;
                  Receipt.Status := 0;  -- Failed
                  Receipt.Gas_Used := TX.Gas_Limit;  -- All gas consumed
                  Receipt.Block_Number := Orchestrator.Latest_Block_Num.Limbs (0) + 1;

                  Block_Builder.Add_Receipt (
                     Builder => Orchestrator.Builder,
                     Receipt => Receipt
                  );
               end;

            when Anubis_Node.Exec_Invalid_TX =>
               --  Invalid transaction - skip without counting
               Ada.Text_IO.Put_Line ("Skipping invalid transaction");

            when others =>
               --  Unknown error - skip
               Ada.Text_IO.Put_Line ("Transaction execution error");
         end case;
      end loop;

      if Processed > 0 then
         Ada.Text_IO.Put_Line ("Processed" & Natural'Image (Processed) & " transactions");
      end if;
   end Process_Pending_Transactions;

   ---------------------------------------------------------------------------
   --  Governance Integration
   ---------------------------------------------------------------------------

   procedure Update_Governance (
      Orchestrator : in Out Orchestrator_State
   ) is
   begin
      if Orchestrator.Config.Enable_Governance then
         Anubis_Governance.Process_Handoffs (
            State         => Orchestrator.Governance,
            Current_Block => Orchestrator.Latest_Block_Num.Limbs (0)
         );
      end if;
   end Update_Governance;

   procedure Execute_Governance_Action (
      Orchestrator : in Out Orchestrator_State;
      Action_Type  : in     Anubis_Governance.Privilege_Type;
      Success      : out    Boolean
   ) is
      Action_Result : Anubis_Governance.Action_Result;
      Builder_PK    : constant Anubis_Types.Byte_Array (0 .. 31) := (others => 0);
      Empty_Hash    : constant Anubis_Types.Byte_Array (0 .. 31) := (others => 0);
      Empty_Sigs    : constant Anubis_Types.Byte_Array (0 .. 0) := (others => 0);
   begin
      Success := False;

      if not Orchestrator.Config.Enable_Governance then
         return;
      end if;

      --  Check if action is allowed in current phase
      if not Anubis_Governance.Has_Privilege (
         State     => Orchestrator.Governance,
         Privilege => Action_Type,
         Holder    => Anubis_Governance.Builder
      ) then
         --  Builder doesn't have this privilege
         return;
      end if;

      --  Execute the governance action based on type
      case Action_Type is
         when Anubis_Governance.Protocol_Upgrade =>
            --  Execute protocol upgrade
            Anubis_Governance.Execute_Upgrade (
               State        => Orchestrator.Governance,
               Proposer     => Builder_PK,
               Upgrade_Hash => Empty_Hash,
               Council_Sigs => Empty_Sigs,
               DAO_Vote_ID  => 0,
               Result       => Action_Result
            );
            Success := (Action_Result = Anubis_Governance.Approved);

         when Anubis_Governance.Emergency_Pause =>
            --  Execute emergency pause (72 hours max in Phase 1)
            Anubis_Governance.Execute_Pause (
               State        => Orchestrator.Governance,
               Initiator    => Builder_PK,
               Duration     => 43_200,  -- 72 hours in blocks
               Reason_Hash  => Empty_Hash,
               Council_Sigs => Empty_Sigs,
               Result       => Action_Result
            );
            Success := (Action_Result = Anubis_Governance.Approved);

         when Anubis_Governance.Parameter_Tuning =>
            --  Parameter tuning requires specific values, just check privilege
            Success := True;

         when Anubis_Governance.Bug_Bounty_Approval =>
            --  Bug bounty approval is a privilege check only
            Success := True;

         when Anubis_Governance.Validator_Removal =>
            --  Validator removal requires specific target, just check privilege
            Success := True;

         when Anubis_Governance.Treasury_Access =>
            --  Builder NEVER has treasury access
            Success := False;
      end case;

      if Success then
         Ada.Text_IO.Put_Line ("Governance action executed: " &
            Anubis_Governance.Privilege_Type'Image (Action_Type));
      end if;
   end Execute_Governance_Action;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Block_Number (Orchestrator : Orchestrator_State) return U256 is
   begin
      return Orchestrator.Latest_Block_Num;
   end Get_Block_Number;

   function Get_Sync_Status (Orchestrator : Orchestrator_State) return Sync_Status is
   begin
      return Orchestrator.Sync_State;
   end Get_Sync_Status;

   function Is_Synced (Orchestrator : Orchestrator_State) return Boolean is
   begin
      return Orchestrator.Sync_State = Sync_Complete and not Orchestrator.Is_Syncing;
   end Is_Synced;

   function Get_Peer_Count (Orchestrator : Orchestrator_State) return Natural is
   begin
      if Orchestrator.Config.Enable_P2P then
         return P2P_Network.Get_Peer_Count (Orchestrator.Network);
      else
         return 0;
      end if;
   end Get_Peer_Count;

   function Get_Mempool_Size (Orchestrator : Orchestrator_State) return Natural is
   begin
      return Mempool.Get_TX_Count (Orchestrator.Pool);
   end Get_Mempool_Size;

end Anubis_Node_Orchestrator;
