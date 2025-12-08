pragma SPARK_Mode (On);

with Anubis_Types;
with Anubis_SHA3;

package body Node_Contract_Registry with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   procedure Initialize (Registry : out Registry_State) is
   begin
      Registry.Is_Initialized := True;
      Registry.Contract_Count := 0;

      --  Clear all slots
      for I in Stored_Contract_Index loop
         Registry.Contracts (I) := Empty_Stored_Contract;
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Register Contract
   ---------------------------------------------------------------------------

   procedure Register_Contract (
      Registry  : in Out Registry_State;
      Manifest  : in     Node_Contract_Manifest;
      Code      : in     Node_Code_Buffer;
      Code_Size : in     Natural;
      Block_Num : in     U256;
      Result    : out    Stored_Contract;
      Success   : out    Boolean
   ) is
      Code_Hash    : Hash256;
      Contract_ID  : Contract_Address;
      Slot         : Stored_Contract_Index;
   begin
      Result := Empty_Stored_Contract;
      Success := False;

      --  Hash the code to create contract ID
      declare
         Code_Bytes : Anubis_Types.Byte_Array (0 .. Code_Size - 1);
         Digest     : Anubis_SHA3.SHA3_256_Digest;
      begin
         --  Convert code buffer to byte array for hashing
         for I in 0 .. Code_Size - 1 loop
            Code_Bytes (I) := Anubis_Types.Byte (Code (Node_Code_Index (I)));
         end loop;

         Anubis_SHA3.SHA3_256 (Code_Bytes, Digest);

         --  Copy digest to code hash
         for I in 0 .. 31 loop
            Code_Hash (I) := Byte (Digest (I));
         end loop;
      end;

      --  Generate contract address from code hash
      for I in 0 .. 31 loop
         Contract_ID (I) := Code_Hash (I);
      end loop;

      --  Find empty slot
      Slot := Stored_Contract_Index (Registry.Contract_Count);

      --  Store contract
      Registry.Contracts (Slot) := (
         Is_Valid     => True,
         Contract_ID  => Contract_ID,
         Code_Hash    => Code_Hash,
         Manifest     => Manifest,
         Code         => Code,
         Code_Size    => Code_Size,
         Deploy_Block => Block_Num
      );

      Registry.Contract_Count := Registry.Contract_Count + 1;

      --  Return the stored contract
      Result := Registry.Contracts (Slot);
      Success := True;
   end Register_Contract;

   ---------------------------------------------------------------------------
   --  Get Contract
   ---------------------------------------------------------------------------

   function Get_Contract (
      Registry    : Registry_State;
      Contract_ID : Contract_Address
   ) return Stored_Contract is
   begin
      for I in 0 .. Stored_Contract_Index (Registry.Contract_Count - 1) loop
         if Registry.Contracts (I).Is_Valid and then
            Registry.Contracts (I).Contract_ID = Contract_ID then
            return Registry.Contracts (I);
         end if;
      end loop;

      return Empty_Stored_Contract;
   end Get_Contract;

   ---------------------------------------------------------------------------
   --  Contract Exists
   ---------------------------------------------------------------------------

   function Contract_Exists (
      Registry    : Registry_State;
      Contract_ID : Contract_Address
   ) return Boolean is
   begin
      for I in 0 .. Stored_Contract_Index (Registry.Contract_Count - 1) loop
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
