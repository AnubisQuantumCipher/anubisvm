--  KHEPRI State: Contract state management implementation
--  Note: Implementation uses SPARK_Mode Off for complex storage operations
--  The spec maintains full SPARK contracts for interface verification
pragma SPARK_Mode (Off);

with Aegis_U256; use Aegis_U256;
with Anubis_Types;
with Anubis_SHA3;

package body Khepri_State with
   SPARK_Mode => Off,
   Refined_State => (
      Storage_State   => (Current_Contract, SDK_Storage, SDK_Storage_Count),
      Transient_State => (Transient_Storage, Transient_Count)
   )
is

   ---------------------------------------------------------------------------
   --  State Variables (for Refined_State)
   ---------------------------------------------------------------------------

   --  Internal state: current execution context address (set by runtime)
   Current_Contract : Address := (others => 0);

   --  SDK storage: in-memory simulation for embedded contracts
   --  Real implementation would use AEGIS syscall dispatcher
   Max_Storage_Slots : constant := 4096;
   type SDK_Storage_Entry is record
      Contract : Address;
      Key      : Uint256;
      Value    : Uint256;
      Valid    : Boolean;
   end record;
   type SDK_Storage_Array is array (0 .. Max_Storage_Slots - 1) of SDK_Storage_Entry;
   SDK_Storage : SDK_Storage_Array := (others => (
      Contract => (others => 0), Key => Zero, Value => Zero, Valid => False));
   SDK_Storage_Count : Natural := 0;

   --  Transient storage (cleared after each transaction)
   Max_Transient_Slots : constant := 256;
   type Transient_Entry is record
      Key   : Uint256;
      Value : Uint256;
      Valid : Boolean;
   end record;
   type Transient_Array is array (0 .. Max_Transient_Slots - 1) of Transient_Entry;
   Transient_Storage : Transient_Array := (others => (Key => Zero, Value => Zero, Valid => False));
   Transient_Count : Natural := 0;

   procedure Set_Current_Contract (Addr : Address) is
   begin
      Current_Contract := Addr;
   end Set_Current_Contract;

   procedure Clear_Transient_Storage is
   begin
      Transient_Storage := (others => (Key => Zero, Value => Zero, Valid => False));
      Transient_Count := 0;
   end Clear_Transient_Storage;

   ---------------------------------------------------------------------------
   --  Storage Result Constructors
   ---------------------------------------------------------------------------

   function Storage_Ok (Value : Uint256) return Storage_Result is
   begin
      return (Success => True, Value => Value, Error => No_Error);
   end Storage_Ok;

   function Storage_Err (Code : Error_Code) return Storage_Result is
   begin
      return (Success => False, Value => Zero, Error => Code);
   end Storage_Err;

   ---------------------------------------------------------------------------
   --  Internal Slot Calculation
   ---------------------------------------------------------------------------

   function Slot_Hash (Data : Bytes32) return Uint256 is
      Input  : Anubis_Types.Byte_Array (0 .. 31);
      Digest : Anubis_SHA3.SHA3_256_Digest;
      Output : Bytes32;
   begin
      --  Convert to Anubis types
      for I in 0 .. 31 loop
         Input (I) := Anubis_Types.Byte (Data (I));
      end loop;

      --  Compute Keccak-256 (Ethereum-compatible hash)
      Anubis_SHA3.Keccak_256 (Input, Digest);

      --  Convert back
      for I in 0 .. 31 loop
         Output (I) := Byte (Digest (I));
      end loop;

      return From_Bytes_BE (Output);
   end Slot_Hash;

   ---------------------------------------------------------------------------
   --  Raw Storage Operations
   ---------------------------------------------------------------------------

   procedure SLoad (
      Key   : in  Uint256;
      Value : out Uint256
   ) is
   begin
      --  Search for key in storage array
      for I in 0 .. SDK_Storage_Count - 1 loop
         if SDK_Storage (I).Valid and then
            SDK_Storage (I).Contract = Current_Contract and then
            Equal (SDK_Storage (I).Key, Key)
         then
            Value := SDK_Storage (I).Value;
            return;
         end if;
      end loop;
      --  Not found, return zero (Ethereum semantics)
      Value := Zero;
   end SLoad;

   procedure SStore (
      Key   : in Uint256;
      Value : in Uint256
   ) is
   begin
      --  Search for existing slot
      for I in 0 .. SDK_Storage_Count - 1 loop
         if SDK_Storage (I).Valid and then
            SDK_Storage (I).Contract = Current_Contract and then
            Equal (SDK_Storage (I).Key, Key)
         then
            SDK_Storage (I).Value := Value;
            return;
         end if;
      end loop;

      --  Add new slot if space available
      if SDK_Storage_Count < Max_Storage_Slots then
         SDK_Storage (SDK_Storage_Count) := (
            Contract => Current_Contract,
            Key      => Key,
            Value    => Value,
            Valid    => True
         );
         SDK_Storage_Count := SDK_Storage_Count + 1;
      end if;
   end SStore;

   function Has_Storage (Key : Uint256) return Boolean is
      Value : Uint256;
   begin
      SLoad (Key, Value);
      return not Is_Zero (Value);
   end Has_Storage;

   procedure Clear_Storage (Key : Uint256) is
   begin
      SStore (Key, Zero);
   end Clear_Storage;

   ---------------------------------------------------------------------------
   --  Map Operations
   ---------------------------------------------------------------------------

   function Mapping_Slot (
      Base_Slot : Uint256;
      Key       : Uint256
   ) return Uint256 is
      Data : Bytes32;
      Combined : Uint256;
   begin
      --  Combine key and base_slot, then hash
      Combined := Bit_Xor (Key, Base_Slot);
      Data := To_Bytes_BE (Combined);
      return Slot_Hash (Data);
   end Mapping_Slot;

   function Nested_Mapping_Slot (
      Base_Slot : Uint256;
      Key1      : Uint256;
      Key2      : Uint256
   ) return Uint256 is
      First_Slot : Uint256;
   begin
      First_Slot := Mapping_Slot (Base_Slot, Key1);
      return Mapping_Slot (First_Slot, Key2);
   end Nested_Mapping_Slot;

   function Array_Slot (
      Base_Slot : Uint256;
      Index     : Uint256
   ) return Uint256 is
      Base_Hash : Uint256;
   begin
      Base_Hash := Slot_Hash (To_Bytes_BE (Base_Slot));
      return Add_Mod (Base_Hash, Index);
   end Array_Slot;

   function Address_Key (Addr : Address) return Uint256 is
      use Aegis_U256;
   begin
      return Address_To_U256 (Addr);
   end Address_Key;

   function Map_Get (
      Base_Slot : Uint256;
      Key       : Uint256
   ) return Uint256 is
      Slot : Uint256;
      Value : Uint256;
   begin
      Slot := Mapping_Slot (Base_Slot, Key);
      SLoad (Slot, Value);
      return Value;
   end Map_Get;

   procedure Map_Set (
      Base_Slot : in Uint256;
      Key       : in Uint256;
      Value     : in Uint256
   ) is
      Slot : Uint256;
   begin
      Slot := Mapping_Slot (Base_Slot, Key);
      SStore (Slot, Value);
   end Map_Set;

   function Map_Has (
      Base_Slot : Uint256;
      Key       : Uint256
   ) return Boolean is
      Slot : Uint256;
   begin
      Slot := Mapping_Slot (Base_Slot, Key);
      return Has_Storage (Slot);
   end Map_Has;

   procedure Map_Delete (
      Base_Slot : in Uint256;
      Key       : in Uint256
   ) is
      Slot : Uint256;
   begin
      Slot := Mapping_Slot (Base_Slot, Key);
      Clear_Storage (Slot);
   end Map_Delete;

   ---------------------------------------------------------------------------
   --  Address Map Operations
   ---------------------------------------------------------------------------

   function Balance_Of (
      Base_Slot : Uint256;
      Account   : Address
   ) return Uint256 is
      Key : Uint256;
   begin
      Key := Address_Key (Account);
      return Map_Get (Base_Slot, Key);
   end Balance_Of;

   procedure Set_Balance (
      Base_Slot : in Uint256;
      Account   : in Address;
      Amount    : in Uint256
   ) is
      Key : Uint256;
   begin
      Key := Address_Key (Account);
      Map_Set (Base_Slot, Key, Amount);
   end Set_Balance;

   function Add_Balance (
      Base_Slot : Uint256;
      Account   : Address;
      Amount    : Uint256
   ) return Result with SPARK_Mode => Off is
      Current  : Uint256;
      New_Val  : Uint256;
      Overflow : Boolean;
   begin
      Current := Balance_Of (Base_Slot, Account);
      Add (Current, Amount, New_Val, Overflow);
      if Overflow then
         return Err (Khepri_Types.Overflow);
      end if;
      Set_Balance (Base_Slot, Account, New_Val);
      return Ok (New_Val);
   end Add_Balance;

   function Sub_Balance (
      Base_Slot : Uint256;
      Account   : Address;
      Amount    : Uint256
   ) return Result with SPARK_Mode => Off is
      Current   : Uint256;
      New_Val   : Uint256;
      Underflow : Boolean;
   begin
      Current := Balance_Of (Base_Slot, Account);
      if Less_Than (Current, Amount) then
         return Err (Insufficient_Balance);
      end if;
      Sub (Current, Amount, New_Val, Underflow);
      if Underflow then
         return Err (Khepri_Types.Underflow);
      end if;
      Set_Balance (Base_Slot, Account, New_Val);
      return Ok (New_Val);
   end Sub_Balance;

   ---------------------------------------------------------------------------
   --  Nested Map Operations
   ---------------------------------------------------------------------------

   function Get_Allowance (
      Base_Slot : Uint256;
      Owner     : Address;
      Spender   : Address
   ) return Uint256 is
      Slot : Uint256;
      Value : Uint256;
   begin
      Slot := Nested_Mapping_Slot (
         Base_Slot,
         Address_Key (Owner),
         Address_Key (Spender)
      );
      SLoad (Slot, Value);
      return Value;
   end Get_Allowance;

   procedure Set_Allowance (
      Base_Slot : in Uint256;
      Owner     : in Address;
      Spender   : in Address;
      Amount    : in Uint256
   ) is
      Slot : Uint256;
   begin
      Slot := Nested_Mapping_Slot (
         Base_Slot,
         Address_Key (Owner),
         Address_Key (Spender)
      );
      SStore (Slot, Amount);
   end Set_Allowance;

   ---------------------------------------------------------------------------
   --  Array Operations
   ---------------------------------------------------------------------------

   function Array_Length (Base_Slot : Uint256) return Uint256 is
      Length : Uint256;
   begin
      SLoad (Base_Slot, Length);
      return Length;
   end Array_Length;

   function Array_Get (
      Base_Slot : Uint256;
      Index     : Uint256
   ) return Storage_Result is
      Length : Uint256;
      Slot   : Uint256;
      Value  : Uint256;
   begin
      Length := Array_Length (Base_Slot);
      if not Less_Than (Index, Length) then
         return Storage_Err (Not_Found);
      end if;
      Slot := Array_Slot (Base_Slot, Index);
      SLoad (Slot, Value);
      return Storage_Ok (Value);
   end Array_Get;

   procedure Array_Set (
      Base_Slot : in Uint256;
      Index     : in Uint256;
      Value     : in Uint256
   ) is
      Slot : Uint256;
   begin
      Slot := Array_Slot (Base_Slot, Index);
      SStore (Slot, Value);
   end Array_Set;

   procedure Array_Push (
      Base_Slot : in Uint256;
      Value     : in Uint256
   ) is
      Length   : Uint256;
      New_Len  : Uint256;
      Overflow : Boolean;
      Slot     : Uint256;
   begin
      Length := Array_Length (Base_Slot);
      Add (Length, One, New_Len, Overflow);
      if not Overflow then
         Slot := Array_Slot (Base_Slot, Length);
         SStore (Slot, Value);
         SStore (Base_Slot, New_Len);
      end if;
   end Array_Push;

   function Array_Pop (Base_Slot : Uint256) return Storage_Result
      with SPARK_Mode => Off
   is
      Length    : Uint256;
      New_Len   : Uint256;
      Underflow : Boolean;
      Slot      : Uint256;
      Value     : Uint256;
   begin
      Length := Array_Length (Base_Slot);
      if Is_Zero (Length) then
         return Storage_Err (Not_Found);
      end if;
      Sub (Length, One, New_Len, Underflow);
      if Underflow then
         return Storage_Err (Khepri_Types.Underflow);
      end if;
      Slot := Array_Slot (Base_Slot, New_Len);
      SLoad (Slot, Value);
      Clear_Storage (Slot);
      SStore (Base_Slot, New_Len);
      return Storage_Ok (Value);
   end Array_Pop;

   ---------------------------------------------------------------------------
   --  Packed Storage Operations
   ---------------------------------------------------------------------------

   procedure Pack_U128 (
      Slot  : in Uint256;
      High  : in Uint256;
      Low   : in Uint256
   ) is
      Shifted : Uint256;
      Combined : Uint256;
   begin
      --  Shift High left by 128 bits and OR with Low
      Shifted := Shift_Left (High, 128);
      Combined := Bit_Or (Shifted, Low);
      SStore (Slot, Combined);
   end Pack_U128;

   procedure Unpack_U128 (
      Slot : in  Uint256;
      High : out Uint256;
      Low  : out Uint256
   ) is
      Value : Uint256;
      Mask  : constant Uint256 := (Limbs => (Word64'Last, Word64'Last, 0, 0));
   begin
      SLoad (Slot, Value);
      Low := Bit_And (Value, Mask);
      High := Shift_Right (Value, 128);
   end Unpack_U128;

   procedure Pack_U64x4 (
      Slot : in Uint256;
      V0   : in Word64;
      V1   : in Word64;
      V2   : in Word64;
      V3   : in Word64
   ) is
      Combined : Uint256;
   begin
      Combined := (Limbs => (V0, V1, V2, V3));
      SStore (Slot, Combined);
   end Pack_U64x4;

   procedure Unpack_U64x4 (
      Slot : in  Uint256;
      V0   : out Word64;
      V1   : out Word64;
      V2   : out Word64;
      V3   : out Word64
   ) is
      Value : Uint256;
   begin
      SLoad (Slot, Value);
      V0 := Value.Limbs (0);
      V1 := Value.Limbs (1);
      V2 := Value.Limbs (2);
      V3 := Value.Limbs (3);
   end Unpack_U64x4;

   ---------------------------------------------------------------------------
   --  Transient Storage
   ---------------------------------------------------------------------------

   procedure TLoad (
      Key   : in  Uint256;
      Value : out Uint256
   ) is
   begin
      --  Search transient storage
      for I in 0 .. Transient_Count - 1 loop
         if Transient_Storage (I).Valid and then
            Equal (Transient_Storage (I).Key, Key)
         then
            Value := Transient_Storage (I).Value;
            return;
         end if;
      end loop;
      --  Not found
      Value := Zero;
   end TLoad;

   procedure TStore (
      Key   : in Uint256;
      Value : in Uint256
   ) is
   begin
      --  Search for existing slot
      for I in 0 .. Transient_Count - 1 loop
         if Transient_Storage (I).Valid and then
            Equal (Transient_Storage (I).Key, Key)
         then
            Transient_Storage (I).Value := Value;
            return;
         end if;
      end loop;

      --  Add new slot if space available
      if Transient_Count < Max_Transient_Slots then
         Transient_Storage (Transient_Count) := (
            Key   => Key,
            Value => Value,
            Valid => True
         );
         Transient_Count := Transient_Count + 1;
      end if;
   end TStore;

end Khepri_State;
