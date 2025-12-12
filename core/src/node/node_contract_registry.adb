pragma SPARK_Mode (On);

with Anubis_Types;
with Anubis_SHA3;

package body Node_Contract_Registry with
   SPARK_Mode => On,
   Refined_State => (Hash_Buffer_State => Hash_Code_Buffer)
is

   ---------------------------------------------------------------------------
   --  Package-level buffers (avoid stack overflow with large contracts)
   ---------------------------------------------------------------------------

   --  Static buffer for code hashing (256KB) - avoids stack allocation
   Hash_Code_Buffer : Anubis_Types.Byte_Array (0 .. Node_Max_Code_Size - 1) :=
      (others => 0);

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   procedure Initialize (Registry : out Registry_State) is
   begin
      Registry.Is_Initialized := True;
      Registry.Contract_Count := 0;

      --  Clear all slots using field-by-field init to avoid 256KB stack temps
      for I in Stored_Contract_Index loop
         pragma Loop_Invariant (for all K in Stored_Contract_Index'First .. I - 1 =>
            Registry.Contracts (K)'Initialized);
         Registry.Contracts (I).Is_Valid := False;
         Registry.Contracts (I).Contract_ID := (others => 0);
         Registry.Contracts (I).Code_Hash := (others => 0);
         Registry.Contracts (I).Manifest := (
            Name          => (others => ' '),
            Name_Len      => 0,
            Version_Major => 0,
            Version_Minor => 0,
            Version_Patch => 0,
            Cert          => Cert_None
         );
         --  Zero Code buffer with loop to avoid 256KB stack temporary
         for J in Node_Code_Index loop
            pragma Loop_Invariant (for all L in Node_Code_Index'First .. J - 1 =>
               Registry.Contracts (I).Code (L)'Initialized);
            Registry.Contracts (I).Code (J) := 0;
         end loop;
         Registry.Contracts (I).Code_Size := 0;
         Registry.Contracts (I).Deploy_Block := (Limbs => (0, 0, 0, 0));
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Register Contract
   ---------------------------------------------------------------------------

   procedure Register_Contract (
      Registry  : in out Registry_State;
      Manifest  : in     Node_Contract_Manifest;
      Code      : in     Node_Code_Buffer;
      Code_Size : in     Natural;
      Block_Num : in     U256;
      Index     : out    Stored_Contract_Index;
      Success   : out    Boolean
   ) with
      Refined_Global => (In_Out => Hash_Code_Buffer)
   is
      Code_Hash   : Hash256;
      Contract_ID : Contract_Address;
      Slot        : Stored_Contract_Index;
   begin
      --  Default failure
      Index := 0;
      Success := False;

      if Registry.Contract_Count >= Max_Stored_Contracts then
         return;
      end if;

      Slot := Stored_Contract_Index (Registry.Contract_Count);

      --  Hash the code to create contract ID
      --  Use package-level Hash_Code_Buffer to avoid stack overflow
      declare
         Digest : Anubis_SHA3.SHA3_256_Digest;
      begin
         --  Convert code buffer to byte array for hashing (using static buffer)
         for I in 0 .. Code_Size - 1 loop
            Hash_Code_Buffer (I) :=
               Anubis_Types.Byte (Code (Node_Code_Index (I)));
         end loop;

         Anubis_SHA3.SHA3_256 (Hash_Code_Buffer (0 .. Code_Size - 1), Digest);

         --  Copy digest to code hash
         for I in 0 .. 31 loop
            Code_Hash (I) := Byte (Digest (I));
         end loop;
      end;

      --  Generate contract address from code hash
      for I in 0 .. 31 loop
         Contract_ID (I) := Code_Hash (I);
      end loop;

      --  Initialize the slot IN PLACE (no big local, no aggregate)
      --  Note: Using direct indexing instead of rename to satisfy SPARK flow analysis
      Registry.Contracts (Slot).Is_Valid := True;
      Registry.Contracts (Slot).Contract_ID := Contract_ID;
      Registry.Contracts (Slot).Code_Hash := Code_Hash;
      Registry.Contracts (Slot).Manifest := Manifest;
      Registry.Contracts (Slot).Code_Size := Code_Size;
      Registry.Contracts (Slot).Deploy_Block := Block_Num;

      --  Copy code via loop (no array aggregate temp)
      for I in 0 .. Code_Size - 1 loop
         Registry.Contracts (Slot).Code (Node_Code_Index (I)) :=
            Code (Node_Code_Index (I));
      end loop;

      Registry.Contract_Count := Registry.Contract_Count + 1;

      Index := Slot;
      Success := True;
   end Register_Contract;

   ---------------------------------------------------------------------------
   --  Find Contract
   ---------------------------------------------------------------------------

   procedure Find_Contract (
      Registry    : in  Registry_State;
      Contract_ID : in  Contract_Address;
      Index       : out Stored_Contract_Index;
      Found       : out Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      --  Guard against empty or out-of-range contract count
      if Registry.Contract_Count = 0 or else
         Registry.Contract_Count > Max_Stored_Contracts
      then
         return;
      end if;

      for I in 0 .. Stored_Contract_Index (Registry.Contract_Count - 1) loop
         pragma Loop_Invariant (Registry.Contract_Count >= 1);
         pragma Loop_Invariant (Registry.Contract_Count <= Max_Stored_Contracts);
         if Registry.Contracts (I).Is_Valid and then
            Registry.Contracts (I).Contract_ID = Contract_ID
         then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Contract;

   ---------------------------------------------------------------------------
   --  Contract Exists
   ---------------------------------------------------------------------------

   function Contract_Exists (
      Registry    : Registry_State;
      Contract_ID : Contract_Address
   ) return Boolean is
   begin
      --  Guard against empty or out-of-range contract count
      if Registry.Contract_Count = 0 or else
         Registry.Contract_Count > Max_Stored_Contracts
      then
         return False;
      end if;

      for I in 0 .. Stored_Contract_Index (Registry.Contract_Count - 1) loop
         pragma Loop_Invariant (Registry.Contract_Count >= 1);
         pragma Loop_Invariant (Registry.Contract_Count <= Max_Stored_Contracts);
         if Registry.Contracts (I).Is_Valid and then
            Registry.Contracts (I).Contract_ID = Contract_ID then
            return True;
         end if;
      end loop;

      return False;
   end Contract_Exists;

   ---------------------------------------------------------------------------
   --  Count
   ---------------------------------------------------------------------------

   function Count (Registry : Registry_State) return Natural is
   begin
      return Registry.Contract_Count;
   end Count;

end Node_Contract_Registry;
