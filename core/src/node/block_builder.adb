-------------------------------------------------------------------------------
--  Block_Builder: Block Construction Implementation for AnubisVM
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Anubis_SHA3;
with Anubis_Types;

package body Block_Builder is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Initialize empty block header
   procedure Initialize_Header (Header : out Block_Header) is
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
      Header.Gas_Limit := Block_Gas_Limit;
      Header.Extra_Data := (others => 0);
   end Initialize_Header;

   --  Initialize empty TX entry
   procedure Initialize_TX_Entry (TX_Entry : out Block_TX_Entry) is
      Empty_TX : Signed_Transaction;
   begin
      Empty_TX.Tx_Type := Tx_Invoke;
      Empty_TX.Nonce := Aegis_VM_Types.U256_Zero;
      Empty_TX.Chain_ID := Aegis_VM_Types.U256_Zero;
      Empty_TX.From := (others => 0);
      Empty_TX.To := (others => 0);
      Empty_TX.Entry_Point := (others => ' ');
      Empty_TX.Entry_Len := 0;
      Empty_TX.Args := (others => 0);
      Empty_TX.Args_Size := 0;
      Empty_TX.Gas_Limit := 0;
      Empty_TX.Value := Aegis_VM_Types.U256_Zero;
      Empty_TX.Signature := (others => 0);
      Empty_TX.Sig_Valid := False;
      Empty_TX.PubKey_Hash := (others => 0);

      TX_Entry.Is_Valid := False;
      TX_Entry.TX_Hash := (others => 0);
      TX_Entry.TX := Empty_TX;
      TX_Entry.Public_Key := (others => 0);
   end Initialize_TX_Entry;

   --  Initialize empty receipt
   procedure Initialize_Receipt (Receipt : out TX_Receipt) is
   begin
      Receipt.Is_Valid := False;
      Receipt.TX_Hash := (others => 0);
      Receipt.Success := False;
      Receipt.Gas_Used := 0;
      Receipt.Return_Data := (others => 0);
      Receipt.Return_Size := 0;
      Receipt.Error_Code := 0;
   end Initialize_Receipt;

   --  Increment U256 by 1
   function U256_Increment (Value : U256) return U256 is
      Result : U256 := Value;
      Carry  : Unsigned_64 := 1;
   begin
      for I in Result.Limbs'Range loop
         declare
            Old : constant Unsigned_64 := Result.Limbs (I);
         begin
            Result.Limbs (I) := Old + Carry;
            if Result.Limbs (I) >= Old then
               Carry := 0;
            else
               Carry := 1;
            end if;
         end;
         exit when Carry = 0;
      end loop;
      return Result;
   end U256_Increment;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (Builder : out Builder_State) is
      Empty_Entry : Block_TX_Entry;
      Empty_Receipt : TX_Receipt;
   begin
      Initialize_TX_Entry (Empty_Entry);
      Initialize_Receipt (Empty_Receipt);

      Builder.Is_Building := False;
      Builder.Parent_Hash := (others => 0);
      Builder.Parent_Number := Aegis_VM_Types.U256_Zero;
      Builder.Start_Time := 0;
      Builder.Is_Initialized := True;

      --  Initialize stats
      Builder.Stats := (
         Blocks_Built => 0,
         TX_Included  => 0,
         TX_Failed    => 0,
         Gas_Used     => 0
      );

      --  Initialize current block
      Initialize_Header (Builder.Current_Block.Header);
      Builder.Current_Block.TX_Count := 0;
      Builder.Current_Block.Is_Valid := False;
      Builder.Current_Block.Is_Finalized := False;

      for I in Builder.Current_Block.Transactions'Range loop
         Builder.Current_Block.Transactions (I) := Empty_Entry;
      end loop;

      for I in Builder.Current_Block.Receipts'Range loop
         Builder.Current_Block.Receipts (I) := Empty_Receipt;
      end loop;

      --  Initialize VM execution state
      Node_Contract_Registry.Initialize (Builder.Registry);
      Node_Contract_Executor.Initialize (Builder.Exec);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Block Building
   ---------------------------------------------------------------------------

   procedure Start_Block (
      Builder      : in Out Builder_State;
      Parent_Hash  : in     Hash256;
      Parent_Num   : in     U256;
      Timestamp    : in     Unsigned_64;
      Proposer     : in     Contract_Address;
      Result       : out    Builder_Result
   ) is
      Empty_Entry : Block_TX_Entry;
      Empty_Receipt : TX_Receipt;
   begin
      if Builder.Is_Building then
         Result := Builder_Already_Building;
         return;
      end if;

      Initialize_TX_Entry (Empty_Entry);
      Initialize_Receipt (Empty_Receipt);

      Builder.Is_Building := True;
      Builder.Parent_Hash := Parent_Hash;
      Builder.Parent_Number := Parent_Num;
      Builder.Start_Time := Timestamp;

      --  Initialize new block header
      Initialize_Header (Builder.Current_Block.Header);
      Builder.Current_Block.Header.Number := U256_Increment (Parent_Num);
      Builder.Current_Block.Header.Parent_Hash := Parent_Hash;
      Builder.Current_Block.Header.Timestamp := Timestamp;
      Builder.Current_Block.Header.Proposer := Proposer;
      Builder.Current_Block.Header.Gas_Limit := Block_Gas_Limit;
      Builder.Current_Block.Header.Gas_Used := 0;

      --  Clear block contents
      Builder.Current_Block.TX_Count := 0;
      Builder.Current_Block.Is_Valid := True;
      Builder.Current_Block.Is_Finalized := False;

      for I in Builder.Current_Block.Transactions'Range loop
         Builder.Current_Block.Transactions (I) := Empty_Entry;
      end loop;

      for I in Builder.Current_Block.Receipts'Range loop
         Builder.Current_Block.Receipts (I) := Empty_Receipt;
      end loop;

      Result := Builder_OK;
   end Start_Block;

   procedure Add_Transactions (
      Builder      : in Out Builder_State;
      Pool         : in Out Mempool.Mempool_State;
      Max_TX       : in     Natural;
      Added        : out    Natural;
      Result       : out    Builder_Result
   ) is
      Receipt    : TX_Receipt;
      TX_Result  : Builder_Result;
      Remove_OK  : Boolean;
      Target_TX  : Natural;
   begin
      Added := 0;

      if not Builder.Is_Building then
         Result := Builder_OK;
         return;
      end if;

      --  Determine how many TXs to try to add
      if Max_TX > Max_TX_Per_Block - Builder.Current_Block.TX_Count then
         Target_TX := Max_TX_Per_Block - Builder.Current_Block.TX_Count;
      else
         Target_TX := Max_TX;
      end if;

      --  Add transactions from mempool using Get_Best_N_TX to get entries
      --  Note: Since Pool.Pool is not directly accessible, we use the
      --  Get_Best_N_TX procedure which returns entries
      declare
         Best_Entries : Mempool.TX_Pool_Storage;
         Best_Count   : Natural;
      begin
         Mempool.Get_Best_N_TX (Pool, Target_TX, Best_Entries, Best_Count);

         for I in 0 .. Best_Count - 1 loop
            exit when I > Mempool.TX_Pool_Index'Last;

            declare
               Pool_Entry : constant Mempool.Mempool_Entry := Best_Entries (I);
            begin
               exit when not Pool_Entry.Is_Valid;

               --  Check if we have gas capacity
               exit when not Can_Add_TX (Builder, Pool_Entry.TX.Gas_Limit);

               --  Execute transaction
               Execute_Transaction (
                  Builder,
                  Pool_Entry.TX,
                  Pool_Entry.Public_Key,
                  Pool_Entry.TX_Hash,
                  Receipt,
                  TX_Result
               );

               --  Remove from mempool (whether success or failure)
               Mempool.Remove_TX (Pool, Pool_Entry.TX_Hash, Remove_OK);

               if TX_Result = Builder_OK then
                  Added := Added + 1;
               end if;
            end;
         end loop;
      end;

      if Added = 0 and Mempool.Get_TX_Count (Pool) > 0 then
         Result := Builder_Gas_Limit_Exceeded;
      else
         Result := Builder_OK;
      end if;
   end Add_Transactions;

   procedure Execute_Transaction (
      Builder      : in Out Builder_State;
      TX           : in     Signed_Transaction;
      Public_Key   : in     Anubis_MLDSA_Types.Public_Key;
      TX_Hash      : in     Hash256;
      Receipt      : out    TX_Receipt;
      Result       : out    Builder_Result
   ) is
      use Node_Contract_Registry;
      use Node_Contract_Executor;

      TX_Index     : Block_TX_Index;
      EP_Name      : Entry_Name := (others => ' ');
      EP_Name_Len  : Natural := 0;
      Contract_Idx : Stored_Contract_Index := 0;
      VM_Result    : Invoke_Result;
   begin
      Initialize_Receipt (Receipt);

      --  Check capacity
      if Builder.Current_Block.TX_Count >= Max_TX_Per_Block then
         Receipt.Success := False;
         Receipt.Error_Code := 1;  -- Block full
         Result := Builder_Gas_Limit_Exceeded;
         return;
      end if;

      --  Check gas limit
      if Builder.Current_Block.Header.Gas_Used + TX.Gas_Limit > Block_Gas_Limit then
         Receipt.Success := False;
         Receipt.Error_Code := 2;  -- Gas limit exceeded
         Result := Builder_Gas_Limit_Exceeded;
         return;
      end if;

      TX_Index := Builder.Current_Block.TX_Count;

      --  Add transaction to block
      Builder.Current_Block.Transactions (TX_Index) := (
         Is_Valid   => True,
         TX_Hash    => TX_Hash,
         TX         => TX,
         Public_Key => Public_Key
      );

      --  Prepare entry point name from transaction
      EP_Name_Len := TX.Entry_Len;
      if EP_Name_Len > Max_Entry_Name_Length then
         EP_Name_Len := Max_Entry_Name_Length;
      end if;
      for I in 1 .. EP_Name_Len loop
         EP_Name (I) := TX.Entry_Point (I);
      end loop;

      --  Look up contract by destination address (To field)
      --  Use contract index 0 as default for now (built-ins use address matching)
      Contract_Idx := 0;

      --  Execute the transaction via the VM
      Node_Contract_Executor.Execute (
         Exec         => Builder.Exec,
         Registry     => Builder.Registry,
         Contract_Idx => Contract_Idx,
         From         => TX.From,
         EP_Name      => EP_Name,
         EP_Name_Len  => EP_Name_Len,
         Args         => TX.Args,
         Args_Size    => TX.Args_Size,
         Gas_Limit    => TX.Gas_Limit,
         Value        => TX.Value,
         Is_View      => False,
         Ret          => VM_Result
      );

      --  Build receipt from VM execution result
      Receipt.Is_Valid := True;
      Receipt.TX_Hash := TX_Hash;
      Receipt.Success := VM_Result.Success;
      Receipt.Gas_Used := VM_Result.Gas_Used;

      --  Copy return data
      Receipt.Return_Size := VM_Result.Return_Size;
      if VM_Result.Return_Size > 0 then
         for I in 0 .. VM_Result.Return_Size - 1 loop
            if I < Receipt.Return_Data'Length then
               Receipt.Return_Data (Args_Buffer'First + I) :=
                 VM_Result.Return_Data (Return_Index (I));
            end if;
         end loop;
      else
         Receipt.Return_Data := (others => 0);
      end if;

      if VM_Result.Success then
         Receipt.Error_Code := 0;
      else
         Receipt.Error_Code := 3;  -- VM execution failed
      end if;

      Builder.Current_Block.Receipts (TX_Index) := Receipt;

      --  Update counters
      Builder.Current_Block.TX_Count := Builder.Current_Block.TX_Count + 1;
      Builder.Current_Block.Header.Gas_Used :=
         Builder.Current_Block.Header.Gas_Used + Receipt.Gas_Used;

      --  Update stats
      Builder.Stats.TX_Included := Builder.Stats.TX_Included + 1;
      Builder.Stats.Gas_Used := Builder.Stats.Gas_Used + Receipt.Gas_Used;

      if VM_Result.Success then
         Result := Builder_OK;
      else
         Builder.Stats.TX_Failed := Builder.Stats.TX_Failed + 1;
         Result := Builder_Execution_Failed;
      end if;
   end Execute_Transaction;

   procedure Seal_Block (
      Builder      : in Out Builder_State;
      State_Root   : in     Hash256;
      Result       : out    Builder_Result;
      Sealed_Block : out    Block
   ) is
      TX_Root      : Hash256;
      Receipts_Root : Hash256;
   begin
      --  Initialize output
      Initialize_Header (Sealed_Block.Header);
      Sealed_Block.TX_Count := 0;
      Sealed_Block.Is_Valid := False;
      Sealed_Block.Is_Finalized := False;

      if not Builder.Is_Building then
         Result := Builder_OK;  -- Nothing to seal
         return;
      end if;

      --  Compute roots
      Compute_TX_Root (Builder, TX_Root);
      Compute_Receipts_Root (Builder, Receipts_Root);

      --  Update header
      Builder.Current_Block.Header.State_Root := State_Root;
      Builder.Current_Block.Header.TX_Root := TX_Root;
      Builder.Current_Block.Header.Receipts_Root := Receipts_Root;

      --  Mark as finalized
      Builder.Current_Block.Is_Finalized := True;

      --  Copy to output
      Sealed_Block := Builder.Current_Block;

      --  Update stats
      Builder.Stats.Blocks_Built := Builder.Stats.Blocks_Built + 1;

      --  Reset builder state
      Builder.Is_Building := False;

      Result := Builder_OK;
   end Seal_Block;

   procedure Abort_Block (
      Builder : in Out Builder_State
   ) is
   begin
      Builder.Is_Building := False;
      Builder.Current_Block.Is_Valid := False;
      Builder.Current_Block.Is_Finalized := False;
   end Abort_Block;

   ---------------------------------------------------------------------------
   --  Root Computation
   ---------------------------------------------------------------------------

   procedure Compute_TX_Root (
      Builder  : in     Builder_State;
      TX_Root  : out    Hash256
   ) is
      Buffer : Anubis_Types.Byte_Array (0 .. 32 * Max_TX_Per_Block - 1);
      Len    : Natural := 0;
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Concatenate all TX hashes
      for I in 0 .. Builder.Current_Block.TX_Count - 1 loop
         exit when I > Block_TX_Index'Last;
         if Builder.Current_Block.Transactions (I).Is_Valid then
            for J in Builder.Current_Block.Transactions (I).TX_Hash'Range loop
               Buffer (Len) := Anubis_Types.Byte (
                  Builder.Current_Block.Transactions (I).TX_Hash (J)
               );
               Len := Len + 1;
            end loop;
         end if;
      end loop;

      --  Hash concatenation
      if Len > 0 then
         Anubis_SHA3.SHA3_256 (Buffer (0 .. Len - 1), Digest);
      else
         Anubis_SHA3.SHA3_256 (Buffer (0 .. 0), Digest);
      end if;

      for I in TX_Root'Range loop
         TX_Root (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;
   end Compute_TX_Root;

   procedure Compute_Receipts_Root (
      Builder       : in     Builder_State;
      Receipts_Root : out    Hash256
   ) is
      --  Buffer for receipt data: (success_byte + gas_used_8bytes + tx_hash_32) * max_tx
      Buffer : Anubis_Types.Byte_Array (0 .. 41 * Max_TX_Per_Block - 1);
      Len    : Natural := 0;
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Build receipt hash input
      for I in 0 .. Builder.Current_Block.TX_Count - 1 loop
         exit when I > TX_Receipt_Index'Last;
         if Builder.Current_Block.Receipts (I).Is_Valid then
            --  Success flag
            if Builder.Current_Block.Receipts (I).Success then
               Buffer (Len) := 1;
            else
               Buffer (Len) := 0;
            end if;
            Len := Len + 1;

            --  Gas used (8 bytes LE) - convert to Unsigned_64 for bitwise ops
            declare
               Gas : constant Unsigned_64 := Unsigned_64 (
                  Builder.Current_Block.Receipts (I).Gas_Used
               );
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

            --  TX hash
            for J in Builder.Current_Block.Receipts (I).TX_Hash'Range loop
               Buffer (Len) := Anubis_Types.Byte (
                  Builder.Current_Block.Receipts (I).TX_Hash (J)
               );
               Len := Len + 1;
            end loop;
         end if;
      end loop;

      --  Hash
      if Len > 0 then
         Anubis_SHA3.SHA3_256 (Buffer (0 .. Len - 1), Digest);
      else
         Anubis_SHA3.SHA3_256 (Buffer (0 .. 0), Digest);
      end if;

      for I in Receipts_Root'Range loop
         Receipts_Root (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;
   end Compute_Receipts_Root;

   procedure Compute_Block_Hash (
      Header     : in     Block_Header;
      Block_Hash : out    Hash256
   ) is
      --  Buffer for header hash: number(32) + parent(32) + timestamp(8) + roots(96) + proposer(32) + gas(16)
      Buffer : Anubis_Types.Byte_Array (0 .. 255);
      Len    : Natural := 0;
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Block number (32 bytes)
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

      --  Timestamp (8 bytes)
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

      --  Proposer (32 bytes)
      for I in Header.Proposer'Range loop
         Buffer (Len) := Anubis_Types.Byte (Header.Proposer (I));
         Len := Len + 1;
      end loop;

      --  Hash
      Anubis_SHA3.SHA3_256 (Buffer (0 .. Len - 1), Digest);

      for I in Block_Hash'Range loop
         Block_Hash (I) := Aegis_VM_Types.Byte (Digest (I));
      end loop;
   end Compute_Block_Hash;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Is_Building (Builder : Builder_State) return Boolean is
   begin
      return Builder.Is_Building;
   end Is_Building;

   function Get_TX_Count (Builder : Builder_State) return Natural is
   begin
      return Builder.Current_Block.TX_Count;
   end Get_TX_Count;

   function Get_Gas_Used (Builder : Builder_State) return Gas_Amount is
   begin
      return Builder.Current_Block.Header.Gas_Used;
   end Get_Gas_Used;

   function Get_Gas_Remaining (Builder : Builder_State) return Gas_Amount is
   begin
      if Block_Gas_Limit >= Builder.Current_Block.Header.Gas_Used then
         return Block_Gas_Limit - Builder.Current_Block.Header.Gas_Used;
      else
         return 0;
      end if;
   end Get_Gas_Remaining;

   function Get_Stats (Builder : Builder_State) return Builder_Stats is
   begin
      return Builder.Stats;
   end Get_Stats;

   function Can_Add_TX (
      Builder   : Builder_State;
      Gas_Limit : Gas_Amount
   ) return Boolean is
   begin
      if Builder.Current_Block.TX_Count >= Max_TX_Per_Block then
         return False;
      end if;

      if Builder.Current_Block.Header.Gas_Used + Gas_Limit > Block_Gas_Limit then
         return False;
      end if;

      return True;
   end Can_Add_TX;

end Block_Builder;
