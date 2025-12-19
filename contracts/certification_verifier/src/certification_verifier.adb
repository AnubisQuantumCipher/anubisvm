pragma SPARK_Mode (On);

package body Certification_Verifier is

   ---------------------------------------------------------------------------
   --  Constructor
   ---------------------------------------------------------------------------

   procedure Constructor is
   begin
      --  Initialize admin address to contract deployer
      Store (Slot_Admin_Address, Address_To_U256 (Caller));

      --  Initialize total contracts to 0
      Store (Slot_Total_Contracts, Zero);
   end Constructor;

   ---------------------------------------------------------------------------
   --  Administrative Functions
   ---------------------------------------------------------------------------

   procedure Register_Certification (
      Contract_Addr : in Address;
      Proof_Hash    : in Hash_256;
      Code_Hash     : in Hash_256;
      Level         : in Uint256
   ) is
      Admin : constant Address := U256_To_Address (Load (Slot_Admin_Address));
      Slot_Base : Uint256;
      Total : Uint256;
      Proof_U256 : Uint256;
      Code_U256 : Uint256;
   begin
      --  Check authorization
      pragma Assert (Caller = Admin, "Only admin can register certifications");

      --  Calculate storage slot for this contract
      Slot_Base := Get_Contract_Slot (Contract_Addr, Zero);

      --  Store certification data
      Store (Slot_Base, Level);                                    --  slot+0: Level
      Proof_U256 := Bytes_To_U256 (Proof_Hash);
      Store (Slot_Base + One, Proof_U256);                         --  slot+1: Proof hash
      Code_U256 := Bytes_To_U256 (Code_Hash);
      Store (Slot_Base + From_Natural (2), Code_U256);             --  slot+2: Code hash
      Store (Slot_Base + From_Natural (3), Block_Timestamp);       --  slot+3: Timestamp
      Store (Slot_Base + From_Natural (4), Address_To_U256 (Caller)); --  slot+4: Certifier
      Store (Slot_Base + From_Natural (5), Zero);                  --  slot+5: Flags (not revoked)

      --  Increment total count
      Total := Load (Slot_Total_Contracts);
      Store (Slot_Total_Contracts, Total + One);

      --  Emit event: CertificationRegistered(contract, level, proofHash)
      Emit_3 (
         Event_Sig => Event_Certification_Registered,
         Topic1    => Address_To_Bytes32 (Contract_Addr),
         Topic2    => U256_To_Bytes_BE (Level),
         Topic3    => Proof_Hash,
         Data      => Empty_Data
      );
   end Register_Certification;

   procedure Revoke_Certification (
      Contract_Addr : in Address
   ) is
      Admin : constant Address := U256_To_Address (Load (Slot_Admin_Address));
      Slot_Base : Uint256;
      Flags : Uint256;
   begin
      --  Check authorization
      pragma Assert (Caller = Admin, "Only admin can revoke certifications");

      --  Calculate storage slot
      Slot_Base := Get_Contract_Slot (Contract_Addr, Zero);

      --  Set revoked flag (bit 0 of flags)
      Flags := Load (Slot_Base + From_Natural (5));
      Store (Slot_Base + From_Natural (5), Flags or One);

      --  Emit event
      Emit_1 (
         Event_Sig => Event_Certification_Revoked,
         Topic1    => Address_To_Bytes32 (Contract_Addr),
         Data      => Empty_Data
      );
   end Revoke_Certification;

   procedure Set_Admin (
      New_Admin : in Address
   ) is
   begin
      Store (Slot_Admin_Address, Address_To_U256 (New_Admin));
   end Set_Admin;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Level (
      Contract_Addr : Address
   ) return Uint256 is
      Slot_Base : constant Uint256 := Get_Contract_Slot (Contract_Addr, Zero);
      Level : constant Uint256 := Load (Slot_Base);
      Flags : constant Uint256 := Load (Slot_Base + From_Natural (5));
      Is_Revoked : constant Boolean := (Flags and One) /= Zero;
   begin
      --  If revoked, return Level_None
      if Is_Revoked then
         return Level_None;
      end if;

      return Level;
   end Get_Level;

   function Get_Proof_Hash (
      Contract_Addr : Address
   ) return Hash_256 is
      Slot_Base : constant Uint256 := Get_Contract_Slot (Contract_Addr, Zero);
      Proof_U256 : constant Uint256 := Load (Slot_Base + One);
   begin
      return U256_To_Bytes_BE (Proof_U256);
   end Get_Proof_Hash;

   function Get_Code_Hash (
      Contract_Addr : Address
   ) return Hash_256 is
      Slot_Base : constant Uint256 := Get_Contract_Slot (Contract_Addr, Zero);
      Code_U256 : constant Uint256 := Load (Slot_Base + From_Natural (2));
   begin
      return U256_To_Bytes_BE (Code_U256);
   end Get_Code_Hash;

   function Get_Discount (
      Contract_Addr : Address
   ) return Uint256 is
      Level : constant Uint256 := Get_Level (Contract_Addr);
   begin
      return Level_To_Discount (Level);
   end Get_Discount;

   function Has_Minimum_Level (
      Contract_Addr : Address;
      Min_Level     : Uint256
   ) return Boolean is
      Level : constant Uint256 := Get_Level (Contract_Addr);
   begin
      return Level >= Min_Level;
   end Has_Minimum_Level;

   function Get_Total_Contracts return Uint256 is
   begin
      return Load (Slot_Total_Contracts);
   end Get_Total_Contracts;

   function Get_Admin return Address is
   begin
      return U256_To_Address (Load (Slot_Admin_Address));
   end Get_Admin;

   ---------------------------------------------------------------------------
   --  Verification Functions
   ---------------------------------------------------------------------------

   function Verify_Proof (
      Contract_Addr : Address;
      Proof_Data    : Hash_256
   ) return Boolean is
      Stored_Hash : constant Hash_256 := Get_Proof_Hash (Contract_Addr);
   begin
      return Hash_Equal (Stored_Hash, Proof_Data);
   end Verify_Proof;

   function Get_Contract_Slot (
      Contract_Addr : Address;
      Offset        : Uint256
   ) return Uint256 is
      --  Hash contract address to get unique slot base
      --  slot = keccak256(address || base_slot) + offset
      Addr_Bytes : constant Bytes32 := Address_To_Bytes (Contract_Addr);
      Base_Bytes : constant Bytes32 := U256_To_Bytes_BE (Slot_Entry_Base);
      Combined : Byte_Array (0 .. 63);
      Hash : Hash_256;
      Slot : Uint256;
   begin
      --  Combine address and base slot
      for I in 0 .. 31 loop
         Combined (I) := Addr_Bytes (I);
         Combined (32 + I) := Base_Bytes (I);
      end loop;

      --  Hash to get slot base
      Hash := Keccak_256 (Combined);
      Slot := Bytes_To_U256 (Hash);

      --  Add offset
      return Slot + Offset;
   end Get_Contract_Slot;

   ---------------------------------------------------------------------------
   --  Private Helpers
   ---------------------------------------------------------------------------

   function Level_To_Discount (Level : Uint256) return Uint256 is
   begin
      if Level = Level_Platinum then
         return Discount_Platinum;
      elsif Level = Level_Gold then
         return Discount_Gold;
      elsif Level = Level_Silver then
         return Discount_Silver;
      elsif Level = Level_Bronze then
         return Discount_Bronze;
      else
         return Discount_None;
      end if;
   end Level_To_Discount;

   function Is_Initialized return Boolean is
      Admin : constant Uint256 := Load (Slot_Admin_Address);
   begin
      --  Contract is initialized if admin address is not zero
      return Admin /= Zero;
   end Is_Initialized;

   function Is_Constructor_Call return Boolean is
   begin
      --  In AnubisVM, constructor is identified by Code_Size = 0 at deploy time
      --  For now, we check if admin is not yet set
      return Load (Slot_Admin_Address) = Zero;
   end Is_Constructor_Call;

   --  Helper functions for type conversions
   function Address_To_U256 (Addr : Address) return Uint256 is
      Bytes : constant Bytes32 := Address_To_Bytes (Addr);
   begin
      return Bytes_To_U256 (Bytes);
   end Address_To_U256;

   function U256_To_Address (Value : Uint256) return Address is
      Bytes : constant Bytes32 := U256_To_Bytes_BE (Value);
      Addr : Address;
   begin
      for I in 0 .. 31 loop
         Addr (I) := Bytes (I);
      end loop;
      return Addr;
   end U256_To_Address;

   function Bytes_To_U256 (Bytes : Bytes32) return Uint256 is
      Result : Uint256 := Zero;
   begin
      --  Convert big-endian bytes to U256
      for I in 0 .. 31 loop
         Result := Result * From_Natural (256) + From_Natural (Natural (Bytes (I)));
      end loop;
      return Result;
   end Bytes_To_U256;

   function Address_To_Bytes32 (Addr : Address) return Bytes32 is
      Result : Bytes32;
   begin
      for I in 0 .. 31 loop
         Result (I) := Addr (I);
      end loop;
      return Result;
   end Address_To_Bytes32;

   --  Empty data for events
   Empty_Data_Index : constant := 0;
   Empty_Data_Array : constant Byte_Array (0 .. Empty_Data_Index - 1) := (others => 0);
   Empty_Data : Byte_Array renames Empty_Data_Array;

end Certification_Verifier;
