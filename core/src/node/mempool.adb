-------------------------------------------------------------------------------
--  Mempool: Transaction Pool Implementation for AnubisVM
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Anubis_SHA3;
with Anubis_Types;

package body Mempool is

   --  Use types from Aegis_VM_Types (via spec's use clause)
   subtype Local_Byte is Aegis_VM_Types.Byte;
   subtype Local_Byte_Array is Aegis_VM_Types.Byte_Array;
   subtype Local_Hash256 is Aegis_VM_Types.Hash256;
   subtype Local_U256 is Aegis_VM_Types.U256;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Hash comparison for equality check
   function Hash_Equal (A, B : Local_Hash256) return Boolean is
   begin
      for I in A'Range loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Hash_Equal;

   --  Address comparison for equality check
   function Address_Equal (A, B : Contract_Address) return Boolean is
   begin
      for I in A'Range loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Address_Equal;

   --  U256 comparison for equality check
   function U256_Equal (A, B : Local_U256) return Boolean is
   begin
      for I in A.Limbs'Range loop
         if A.Limbs (I) /= B.Limbs (I) then
            return False;
         end if;
      end loop;
      return True;
   end U256_Equal;

   --  Compute transaction hash from signed transaction
   procedure Compute_TX_Hash_Internal (
      TX   : Signed_Transaction;
      Hash : out Local_Hash256
   ) is
      --  Create a buffer for hashing using Anubis_Types (SHA3 compatible)
      --  nonce(32) + to(32) + entry(64) + args(up to 1024)
      Buffer : Anubis_Types.Byte_Array (0 .. 32 + 32 + 64 + 1024 - 1);
      Len    : Natural := 0;
      Digest : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Add nonce (from U256 limbs - 32 bytes)
      for I in TX.Nonce.Limbs'Range loop
         declare
            Limb : constant Unsigned_64 := TX.Nonce.Limbs (I);
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

      --  Add recipient address (32 bytes)
      for I in TX.To'Range loop
         Buffer (Len) := Anubis_Types.Byte (TX.To (I));
         Len := Len + 1;
      end loop;

      --  Add entry point (up to 64 bytes)
      for I in 0 .. TX.Entry_Len - 1 loop
         exit when I > Entry_Name'Last;
         Buffer (Len) := Anubis_Types.Byte (Character'Pos (TX.Entry_Point (I)));
         Len := Len + 1;
      end loop;

      --  Add args (variable)
      for I in 0 .. TX.Args_Size - 1 loop
         exit when Len >= Buffer'Last;
         Buffer (Len) := Anubis_Types.Byte (TX.Args (I));
         Len := Len + 1;
      end loop;

      --  Hash the buffer
      if Len > 0 then
         Anubis_SHA3.SHA3_256 (Buffer (0 .. Len - 1), Digest);
      else
         Anubis_SHA3.SHA3_256 (Buffer (0 .. 0), Digest);
      end if;

      --  Copy to output (convert from Anubis_Types.Byte to Aegis_VM_Types.Byte)
      for I in Hash'Range loop
         Hash (I) := Local_Byte (Digest (I));
      end loop;
   end Compute_TX_Hash_Internal;

   --  Find free slot in pool
   procedure Find_Free_Slot (
      Pool  : Mempool_State;
      Index : out TX_Pool_Index;
      Found : out Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Pool.Pool'Range loop
         if not Pool.Pool (I).Is_Valid then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Free_Slot;

   --  Find sender in nonce table
   procedure Find_Sender (
      Pool   : Mempool_State;
      Sender : Contract_Address;
      Index  : out Sender_Index;
      Found  : out Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Pool.Senders'Range loop
         if Pool.Senders (I).Is_Valid and then
            Address_Equal (Pool.Senders (I).Sender, Sender)
         then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Sender;

   --  Find free slot in sender table
   procedure Find_Free_Sender_Slot (
      Pool  : Mempool_State;
      Index : out Sender_Index;
      Found : out Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Pool.Senders'Range loop
         if not Pool.Senders (I).Is_Valid then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Free_Sender_Slot;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (Pool : out Mempool_State) is
      Empty_TX : Signed_Transaction;
      Empty_Entry : Mempool_Entry;
      Empty_Sender : Sender_Nonce_Entry;
   begin
      --  Initialize empty TX
      Empty_TX.Tx_Type := Quantum_Transaction_Auth.Tx_Invoke;
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

      --  Initialize empty entry
      Empty_Entry.Is_Valid := False;
      Empty_Entry.TX := Empty_TX;
      Empty_Entry.Public_Key := (others => 0);
      Empty_Entry.TX_Hash := (others => 0);
      Empty_Entry.Gas_Price_Val := Min_Gas_Price;
      Empty_Entry.Received_Time := 0;
      Empty_Entry.Priority := 0;

      --  Initialize empty sender
      Empty_Sender.Is_Valid := False;
      Empty_Sender.Sender := (others => 0);
      Empty_Sender.Pending_Nonce := Aegis_VM_Types.U256_Zero;
      Empty_Sender.TX_Count := 0;

      Pool.Is_Initialized := True;
      Pool.TX_Count := 0;
      Pool.Total_Gas := 0;
      Pool.Sender_Count := 0;
      Pool.Current_Time := 0;

      --  Initialize pool storage
      for I in Pool.Pool'Range loop
         Pool.Pool (I) := Empty_Entry;
      end loop;

      --  Initialize sender table
      for I in Pool.Senders'Range loop
         Pool.Senders (I) := Empty_Sender;
      end loop;

      --  Initialize stats
      Pool.Stats := (
         TX_Count   => 0,
         Total_Gas  => 0,
         Senders    => 0,
         Evictions  => 0,
         Expired    => 0,
         Duplicates => 0
      );
   end Initialize;

   procedure Reset (Pool : in Out Mempool_State) is
   begin
      Initialize (Pool);
   end Reset;

   ---------------------------------------------------------------------------
   --  Transaction Submission
   ---------------------------------------------------------------------------

   procedure Submit_TX (
      Pool          : in Out Mempool_State;
      TX            : in     Signed_Transaction;
      Public_Key    : in     Anubis_MLDSA_Types.Public_Key;
      Gas_Price_Val : in     Gas_Price;
      Timestamp     : in     Unsigned_64;
      Expected_Nonce : in    U256;
      Chain_ID      : in     U256;
      Result        : out    Mempool_Result;
      TX_Hash       : out    Hash256
   ) is
      Verify_Result : Quantum_Transaction_Auth.Verify_Result;
   begin
      --  Verify the transaction signature first
      Quantum_Transaction_Auth.Verify_Transaction (
         Tx               => TX,
         Public_Key       => Public_Key,
         Expected_Nonce   => Expected_Nonce,
         Current_Chain_ID => Chain_ID,
         Result           => Verify_Result
      );

      if not Verify_Result.Valid then
         case Verify_Result.Error_Code is
            when Quantum_Transaction_Auth.Error_Invalid_Signature =>
               Result := Mempool_Invalid_Signature;
            when Quantum_Transaction_Auth.Error_Invalid_Nonce =>
               Result := Mempool_Invalid_Nonce;
            when others =>
               Result := Mempool_Invalid_Signature;
         end case;
         TX_Hash := (others => 0);
         return;
      end if;

      --  If signature valid, submit without re-checking
      Submit_TX_Unchecked (Pool, TX, Public_Key, Gas_Price_Val, Timestamp, Result, TX_Hash);
   end Submit_TX;

   procedure Submit_TX_Unchecked (
      Pool          : in Out Mempool_State;
      TX            : in     Signed_Transaction;
      Public_Key    : in     Anubis_MLDSA_Types.Public_Key;
      Gas_Price_Val : in     Gas_Price;
      Timestamp     : in     Unsigned_64;
      Result        : out    Mempool_Result;
      TX_Hash       : out    Hash256
   ) is
      Slot_Index   : TX_Pool_Index;
      Slot_Found   : Boolean;
      Sender_Idx   : Sender_Index;
      Sender_Found : Boolean;
      New_Sender   : Sender_Index;
      New_Found    : Boolean;
      Computed_Hash : Local_Hash256;
   begin
      --  Compute transaction hash
      Compute_TX_Hash_Internal (TX, Computed_Hash);
      TX_Hash := Computed_Hash;

      --  Check for duplicate
      if TX_Exists (Pool, Computed_Hash) then
         Result := Mempool_Duplicate;
         Pool.Stats.Duplicates := Pool.Stats.Duplicates + 1;
         return;
      end if;

      --  Check gas price
      if Gas_Price_Val < Min_Gas_Price then
         Result := Mempool_Gas_Price_Too_Low;
         return;
      end if;

      --  Check if pool is full
      if Pool.TX_Count >= Max_Pending_TX then
         Result := Mempool_Full;
         return;
      end if;

      --  Check total gas capacity
      if Pool.Total_Gas + TX.Gas_Limit > Max_Pending_Gas then
         Result := Mempool_Full;
         return;
      end if;

      --  Find or create sender entry
      Find_Sender (Pool, TX.From, Sender_Idx, Sender_Found);

      if Sender_Found then
         --  Check per-sender limit
         if Pool.Senders (Sender_Idx).TX_Count >= Max_TX_Per_Sender then
            Result := Mempool_Sender_Limit;
            return;
         end if;
      else
         --  Need to create new sender entry
         if Pool.Sender_Count >= Max_Tracked_Senders then
            Result := Mempool_Full;  -- Sender table full
            return;
         end if;

         Find_Free_Sender_Slot (Pool, New_Sender, New_Found);
         if not New_Found then
            Result := Mempool_Full;
            return;
         end if;

         Pool.Senders (New_Sender) := (
            Is_Valid      => True,
            Sender        => TX.From,
            Pending_Nonce => TX.Nonce,
            TX_Count      => 0
         );
         Pool.Sender_Count := Pool.Sender_Count + 1;
         Pool.Stats.Senders := Pool.Sender_Count;
         Sender_Idx := New_Sender;
      end if;

      --  Find free slot
      Find_Free_Slot (Pool, Slot_Index, Slot_Found);
      if not Slot_Found then
         Result := Mempool_Full;
         return;
      end if;

      --  Add transaction to pool
      Pool.Pool (Slot_Index) := (
         Is_Valid      => True,
         TX            => TX,
         Public_Key    => Public_Key,
         TX_Hash       => Computed_Hash,
         Gas_Price_Val => Gas_Price_Val,
         Received_Time => Timestamp,
         Priority      => Calculate_Priority (Gas_Price_Val, Timestamp, Pool.Current_Time)
      );

      --  Update counters
      Pool.TX_Count := Pool.TX_Count + 1;
      Pool.Total_Gas := Pool.Total_Gas + TX.Gas_Limit;
      Pool.Senders (Sender_Idx).TX_Count := Pool.Senders (Sender_Idx).TX_Count + 1;

      --  Update stats
      Pool.Stats.TX_Count := Pool.TX_Count;
      Pool.Stats.Total_Gas := Pool.Total_Gas;

      Result := Mempool_OK;
   end Submit_TX_Unchecked;

   ---------------------------------------------------------------------------
   --  Transaction Retrieval
   ---------------------------------------------------------------------------

   procedure Get_Best_TX (
      Pool    : in     Mempool_State;
      Index   : out    TX_Pool_Index;
      Found   : out    Boolean
   ) is
      Best_Priority : Natural := 0;
      Best_Index    : TX_Pool_Index := 0;
   begin
      Found := False;
      Index := 0;

      for I in Pool.Pool'Range loop
         if Pool.Pool (I).Is_Valid then
            if not Found or else Pool.Pool (I).Priority > Best_Priority then
               Best_Priority := Pool.Pool (I).Priority;
               Best_Index := I;
               Found := True;
            end if;
         end if;
      end loop;

      if Found then
         Index := Best_Index;
      end if;
   end Get_Best_TX;

   procedure Get_TX_By_Hash (
      Pool    : in     Mempool_State;
      TX_Hash : in     Hash256;
      Index   : out    TX_Pool_Index;
      Found   : out    Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Pool.Pool'Range loop
         if Pool.Pool (I).Is_Valid and then
            Hash_Equal (Pool.Pool (I).TX_Hash, TX_Hash)
         then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Get_TX_By_Hash;

   procedure Get_TX_For_Sender (
      Pool         : in     Mempool_State;
      Sender       : in     Contract_Address;
      Indices      : out    TX_Pool_Storage;
      Count        : out    Natural
   ) is
      Empty_TX : Signed_Transaction;
      Empty_Entry : Mempool_Entry;
   begin
      --  Initialize empty TX
      Empty_TX.Tx_Type := Quantum_Transaction_Auth.Tx_Invoke;
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

      Empty_Entry.Is_Valid := False;
      Empty_Entry.TX := Empty_TX;
      Empty_Entry.Public_Key := (others => 0);
      Empty_Entry.TX_Hash := (others => 0);
      Empty_Entry.Gas_Price_Val := Min_Gas_Price;
      Empty_Entry.Received_Time := 0;
      Empty_Entry.Priority := 0;

      Count := 0;

      --  Initialize output
      for I in Indices'Range loop
         Indices (I) := Empty_Entry;
      end loop;

      --  Collect transactions for this sender
      for I in Pool.Pool'Range loop
         if Pool.Pool (I).Is_Valid and then
            Address_Equal (Pool.Pool (I).TX.From, Sender)
         then
            if Count < Max_Pending_TX then
               Indices (Count) := Pool.Pool (I);
               Count := Count + 1;
            end if;
         end if;
      end loop;
   end Get_TX_For_Sender;

   procedure Get_Best_N_TX (
      Pool    : in     Mempool_State;
      N       : in     Natural;
      Indices : out    TX_Pool_Storage;
      Count   : out    Natural
   ) is
      Empty_TX : Signed_Transaction;
      Empty_Entry : Mempool_Entry;
      Collected    : array (TX_Pool_Index) of Boolean := (others => False);
      Best_Idx     : TX_Pool_Index;
      Best_Priority : Natural;
   begin
      --  Initialize empty TX
      Empty_TX.Tx_Type := Quantum_Transaction_Auth.Tx_Invoke;
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

      Empty_Entry.Is_Valid := False;
      Empty_Entry.TX := Empty_TX;
      Empty_Entry.Public_Key := (others => 0);
      Empty_Entry.TX_Hash := (others => 0);
      Empty_Entry.Gas_Price_Val := Min_Gas_Price;
      Empty_Entry.Received_Time := 0;
      Empty_Entry.Priority := 0;

      Count := 0;

      --  Initialize output
      for I in Indices'Range loop
         Indices (I) := Empty_Entry;
      end loop;

      --  Collect N highest priority transactions
      while Count < N loop
         Best_Priority := 0;
         Best_Idx := 0;

         --  Find best uncollected transaction
         for I in Pool.Pool'Range loop
            if Pool.Pool (I).Is_Valid and then
               not Collected (I) and then
               Pool.Pool (I).Priority > Best_Priority
            then
               Best_Priority := Pool.Pool (I).Priority;
               Best_Idx := I;
            end if;
         end loop;

         exit when Best_Priority = 0;  -- No more valid transactions

         Indices (Count) := Pool.Pool (Best_Idx);
         Collected (Best_Idx) := True;
         Count := Count + 1;
      end loop;
   end Get_Best_N_TX;

   ---------------------------------------------------------------------------
   --  Transaction Removal
   ---------------------------------------------------------------------------

   procedure Remove_TX (
      Pool    : in Out Mempool_State;
      TX_Hash : in     Hash256;
      Success : out    Boolean
   ) is
      Index : TX_Pool_Index;
      Found : Boolean;
   begin
      Get_TX_By_Hash (Pool, TX_Hash, Index, Found);

      if Found then
         Remove_TX_By_Index (Pool, Index, Success);
      else
         Success := False;
      end if;
   end Remove_TX;

   procedure Remove_TX_By_Index (
      Pool    : in Out Mempool_State;
      Index   : in     TX_Pool_Index;
      Success : out    Boolean
   ) is
      TX_Entry   : Mempool_Entry renames Pool.Pool (Index);
      Sender_Idx : Sender_Index;
      Found      : Boolean;
   begin
      if not TX_Entry.Is_Valid then
         Success := False;
         return;
      end if;

      --  Update sender tracking
      Find_Sender (Pool, TX_Entry.TX.From, Sender_Idx, Found);
      if Found then
         if Pool.Senders (Sender_Idx).TX_Count > 0 then
            Pool.Senders (Sender_Idx).TX_Count :=
               Pool.Senders (Sender_Idx).TX_Count - 1;
         end if;

         --  Remove sender entry if no more transactions
         if Pool.Senders (Sender_Idx).TX_Count = 0 then
            Pool.Senders (Sender_Idx).Is_Valid := False;
            if Pool.Sender_Count > 0 then
               Pool.Sender_Count := Pool.Sender_Count - 1;
            end if;
         end if;
      end if;

      --  Update pool counters
      if Pool.TX_Count > 0 then
         Pool.TX_Count := Pool.TX_Count - 1;
      end if;
      if Pool.Total_Gas >= TX_Entry.TX.Gas_Limit then
         Pool.Total_Gas := Pool.Total_Gas - TX_Entry.TX.Gas_Limit;
      else
         Pool.Total_Gas := 0;
      end if;

      --  Clear the slot
      TX_Entry.Is_Valid := False;

      --  Update stats
      Pool.Stats.TX_Count := Pool.TX_Count;
      Pool.Stats.Total_Gas := Pool.Total_Gas;
      Pool.Stats.Senders := Pool.Sender_Count;

      Success := True;
   end Remove_TX_By_Index;

   procedure Remove_Sender_TX (
      Pool    : in Out Mempool_State;
      Sender  : in     Contract_Address;
      Removed : out    Natural
   ) is
      Success : Boolean;
   begin
      Removed := 0;

      for I in Pool.Pool'Range loop
         if Pool.Pool (I).Is_Valid and then
            Address_Equal (Pool.Pool (I).TX.From, Sender)
         then
            Remove_TX_By_Index (Pool, I, Success);
            if Success then
               Removed := Removed + 1;
            end if;
         end if;
      end loop;
   end Remove_Sender_TX;

   ---------------------------------------------------------------------------
   --  Maintenance Operations
   ---------------------------------------------------------------------------

   procedure Expire_Old_TX (
      Pool         : in Out Mempool_State;
      Current_Time : in     Unsigned_64;
      Expired      : out    Natural
   ) is
      Success : Boolean;
      Age     : Unsigned_64;
   begin
      Expired := 0;
      Pool.Current_Time := Current_Time;

      for I in Pool.Pool'Range loop
         if Pool.Pool (I).Is_Valid then
            if Current_Time >= Pool.Pool (I).Received_Time then
               Age := Current_Time - Pool.Pool (I).Received_Time;
               if Age > Unsigned_64 (TX_Expiration_Seconds) then
                  Remove_TX_By_Index (Pool, I, Success);
                  if Success then
                     Expired := Expired + 1;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      Pool.Stats.Expired := Pool.Stats.Expired + Expired;
   end Expire_Old_TX;

   procedure Evict_Lowest_Priority (
      Pool    : in Out Mempool_State;
      Count   : in     Natural;
      Evicted : out    Natural
   ) is
      Success       : Boolean;
      Lowest_Priority : Natural;
      Lowest_Idx      : TX_Pool_Index;
      To_Evict        : Natural := Count;
   begin
      Evicted := 0;

      while To_Evict > 0 loop
         Lowest_Priority := Natural'Last;
         Lowest_Idx := 0;

         --  Find lowest priority transaction
         for I in Pool.Pool'Range loop
            if Pool.Pool (I).Is_Valid and then
               Pool.Pool (I).Priority < Lowest_Priority
            then
               Lowest_Priority := Pool.Pool (I).Priority;
               Lowest_Idx := I;
            end if;
         end loop;

         exit when Lowest_Priority = Natural'Last;  -- No valid transactions

         Remove_TX_By_Index (Pool, Lowest_Idx, Success);
         if Success then
            Evicted := Evicted + 1;
            To_Evict := To_Evict - 1;
         else
            exit;  -- Something went wrong
         end if;
      end loop;

      Pool.Stats.Evictions := Pool.Stats.Evictions + Evicted;
   end Evict_Lowest_Priority;

   procedure Update_Sender_Nonce (
      Pool       : in Out Mempool_State;
      Sender     : in     Contract_Address;
      New_Nonce  : in     U256
   ) is
      Sender_Idx : Sender_Index;
      Found      : Boolean;
   begin
      Find_Sender (Pool, Sender, Sender_Idx, Found);

      if Found then
         Pool.Senders (Sender_Idx).Pending_Nonce := New_Nonce;
      end if;
   end Update_Sender_Nonce;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function TX_Exists (
      Pool    : Mempool_State;
      TX_Hash : Hash256
   ) return Boolean is
   begin
      for I in Pool.Pool'Range loop
         if Pool.Pool (I).Is_Valid and then
            Hash_Equal (Pool.Pool (I).TX_Hash, TX_Hash)
         then
            return True;
         end if;
      end loop;
      return False;
   end TX_Exists;

   function Get_Pending_Nonce (
      Pool   : Mempool_State;
      Sender : Contract_Address
   ) return U256 is
   begin
      for I in Pool.Senders'Range loop
         if Pool.Senders (I).Is_Valid and then
            Address_Equal (Pool.Senders (I).Sender, Sender)
         then
            return Pool.Senders (I).Pending_Nonce;
         end if;
      end loop;

      --  Default: return zero nonce
      return Aegis_VM_Types.U256_Zero;
   end Get_Pending_Nonce;

   function Get_TX_Count (Pool : Mempool_State) return Natural is
   begin
      return Pool.TX_Count;
   end Get_TX_Count;

   function Get_Total_Gas (Pool : Mempool_State) return Gas_Amount is
   begin
      return Pool.Total_Gas;
   end Get_Total_Gas;

   function Get_Stats (Pool : Mempool_State) return Mempool_Stats is
   begin
      return Pool.Stats;
   end Get_Stats;

   function Is_Full (Pool : Mempool_State) return Boolean is
   begin
      return Pool.TX_Count >= Max_Pending_TX;
   end Is_Full;

   function Has_Gas_Capacity (
      Pool      : Mempool_State;
      Gas_Limit : Gas_Amount
   ) return Boolean is
   begin
      return Pool.Total_Gas + Gas_Limit <= Max_Pending_Gas;
   end Has_Gas_Capacity;

   ---------------------------------------------------------------------------
   --  Priority Calculation
   ---------------------------------------------------------------------------

   function Calculate_Priority (
      Gas_Price_Val : Gas_Price;
      Received_Time : Unsigned_64;
      Current_Time  : Unsigned_64
   ) return Natural is
      Age_Seconds   : Unsigned_64;
      Age_Bonus     : Natural;
      Base_Priority : Natural;
   begin
      --  Base priority from gas price (higher gas = higher priority)
      --  Scale down to fit in Natural
      if Gas_Price_Val > 1_000_000 then
         Base_Priority := Natural (Gas_Price_Val / 1_000_000);
      else
         Base_Priority := Natural (Gas_Price_Val);
      end if;

      --  Age bonus: older transactions get slight priority boost
      --  This prevents starvation
      if Current_Time >= Received_Time then
         Age_Seconds := Current_Time - Received_Time;
         if Age_Seconds > 3600 then
            Age_Bonus := 100;  -- Max bonus for old transactions
         else
            Age_Bonus := Natural (Age_Seconds / 36);  -- 1 point per 36 seconds
         end if;
      else
         Age_Bonus := 0;
      end if;

      --  Combine: gas price dominates, age is tiebreaker
      if Base_Priority <= Natural'Last - Age_Bonus then
         return Base_Priority + Age_Bonus;
      else
         return Natural'Last;
      end if;
   end Calculate_Priority;

   function Compare_Priority (
      A, B : Mempool_Entry
   ) return Boolean is
   begin
      --  Higher priority first
      if A.Priority /= B.Priority then
         return A.Priority > B.Priority;
      end if;

      --  Same priority: prefer higher gas price
      if A.Gas_Price_Val /= B.Gas_Price_Val then
         return A.Gas_Price_Val > B.Gas_Price_Val;
      end if;

      --  Same gas price: prefer older transaction
      return A.Received_Time < B.Received_Time;
   end Compare_Priority;

   ---------------------------------------------------------------------------
   --  Validation
   ---------------------------------------------------------------------------

   procedure Validate_TX (
      Pool    : in     Mempool_State;
      TX      : in     Signed_Transaction;
      Gas_Val : in     Gas_Price;
      Result  : out    Mempool_Result
   ) is
   begin
      --  Check gas price
      if Gas_Val < Min_Gas_Price then
         Result := Mempool_Gas_Price_Too_Low;
         return;
      end if;

      --  Check gas limit
      if TX.Gas_Limit = 0 then
         Result := Mempool_Gas_Too_Low;
         return;
      end if;

      --  Check if pool is full
      if Pool.TX_Count >= Max_Pending_TX then
         Result := Mempool_Full;
         return;
      end if;

      --  Check gas capacity
      if not Has_Gas_Capacity (Pool, TX.Gas_Limit) then
         Result := Mempool_Full;
         return;
      end if;

      Result := Mempool_OK;
   end Validate_TX;

end Mempool;
