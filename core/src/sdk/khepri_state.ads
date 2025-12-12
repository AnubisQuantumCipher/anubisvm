pragma SPARK_Mode (On);

with Khepri_Types; use Khepri_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  KHEPRI State: Contract State Management API
--
--  This package provides the interface for contracts to manage persistent
--  storage on the KHEPRI blockchain. All state operations are mediated
--  through the THOTH storage layer.
--
--  Key Features:
--  - Type-safe storage access
--  - Gas-metered operations
--  - Map and array abstractions
--  - Atomic state transitions
--
--  Storage Model:
--  - Each contract has isolated 256-bit key-value storage
--  - Keys and values are both 256 bits (U256)
--  - Storage costs follow EVM pricing (warm/cold access patterns)
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 5: THOTH State Layer
--  - EIP-2929: Gas cost increases for state access opcodes

package Khepri_State with
   SPARK_Mode => On,
   Abstract_State => (Storage_State, Transient_State),
   Initializes => (Storage_State, Transient_State)
is

   ---------------------------------------------------------------------------
   --  Storage Context
   ---------------------------------------------------------------------------

   --  Storage operation result
   type Storage_Result is record
      Success : Boolean;
      Value   : Uint256;
      Error   : Error_Code;
   end record;

   --  Successful storage result
   function Storage_Ok (Value : Uint256) return Storage_Result with
      Global => null,
      Post   => Storage_Ok'Result.Success and
                Khepri_Types."=" (Storage_Ok'Result.Value, Value);

   --  Failed storage result
   function Storage_Err (Code : Error_Code) return Storage_Result with
      Global => null,
      Pre    => Code /= No_Error,
      Post   => not Storage_Err'Result.Success and Storage_Err'Result.Error = Code;

   ---------------------------------------------------------------------------
   --  Raw Storage Operations
   ---------------------------------------------------------------------------

   --  Read from storage
   --  Returns Zero if slot has never been written
   procedure SLoad (
      Key   : in  Uint256;
      Value : out Uint256
   ) with
      Global => (Input => Storage_State);

   --  Write to storage
   --  Gas cost depends on current value (zero/non-zero transitions)
   procedure SStore (
      Key   : in Uint256;
      Value : in Uint256
   ) with
      Global => (In_Out => Storage_State);

   --  Check if storage slot has been written
   function Has_Storage (Key : Uint256) return Boolean with
      Global => Storage_State;

   --  Clear storage slot (sets to zero, may trigger gas refund)
   procedure Clear_Storage (Key : Uint256) with
      Global => (In_Out => Storage_State);

   ---------------------------------------------------------------------------
   --  Map Operations (High-Level Abstraction)
   ---------------------------------------------------------------------------

   --  Mapping slot calculation: keccak256(key || slot)
   --  This matches Solidity"s mapping storage layout

   --  Get value from mapping at base slot
   function Map_Get (
      Base_Slot : Uint256;
      Key       : Uint256
   ) return Uint256 with
      Global => Storage_State;

   --  Set value in mapping at base slot
   procedure Map_Set (
      Base_Slot : in Uint256;
      Key       : in Uint256;
      Value     : in Uint256
   ) with
      Global => (In_Out => Storage_State);

   --  Check if key exists in mapping
   function Map_Has (
      Base_Slot : Uint256;
      Key       : Uint256
   ) return Boolean with
      Global => Storage_State;

   --  Delete key from mapping (set to zero)
   procedure Map_Delete (
      Base_Slot : in Uint256;
      Key       : in Uint256
   ) with
      Global => (In_Out => Storage_State);

   ---------------------------------------------------------------------------
   --  Address Map Operations (Balances, Allowances, etc.)
   ---------------------------------------------------------------------------

   --  Get balance for address
   function Balance_Of (
      Base_Slot : Uint256;
      Account   : Address
   ) return Uint256 with
      Global => Storage_State;

   --  Set balance for address
   procedure Set_Balance (
      Base_Slot : in Uint256;
      Account   : in Address;
      Amount    : in Uint256
   ) with
      Global => (In_Out => Storage_State);

   --  Add to balance (with overflow check)
   --  Note: SPARK_Mode Off because function has side effects (writes storage)
   function Add_Balance (
      Base_Slot : Uint256;
      Account   : Address;
      Amount    : Uint256
   ) return Result with
      SPARK_Mode => Off;

   --  Subtract from balance (with underflow check)
   --  Note: SPARK_Mode Off because function has side effects (writes storage)
   function Sub_Balance (
      Base_Slot : Uint256;
      Account   : Address;
      Amount    : Uint256
   ) return Result with
      SPARK_Mode => Off;

   ---------------------------------------------------------------------------
   --  Nested Map Operations (Allowances: owner -> spender -> amount)
   ---------------------------------------------------------------------------

   --  Get allowance: mapping(owner => mapping(spender => amount))
   function Get_Allowance (
      Base_Slot : Uint256;
      Owner     : Address;
      Spender   : Address
   ) return Uint256 with
      Global => Storage_State;

   --  Set allowance
   procedure Set_Allowance (
      Base_Slot : in Uint256;
      Owner     : in Address;
      Spender   : in Address;
      Amount    : in Uint256
   ) with
      Global => (In_Out => Storage_State);

   ---------------------------------------------------------------------------
   --  Array Operations (Dynamic Arrays in Storage)
   ---------------------------------------------------------------------------

   --  Array length is stored at base_slot
   --  Array elements at keccak256(base_slot) + index

   --  Get array length
   function Array_Length (Base_Slot : Uint256) return Uint256 with
      Global => Storage_State;

   --  Get array element (0-indexed)
   function Array_Get (
      Base_Slot : Uint256;
      Index     : Uint256
   ) return Storage_Result with
      Global => Storage_State;

   --  Set array element
   procedure Array_Set (
      Base_Slot : in Uint256;
      Index     : in Uint256;
      Value     : in Uint256
   ) with
      Global => (In_Out => Storage_State);

   --  Push element to array end
   procedure Array_Push (
      Base_Slot : in Uint256;
      Value     : in Uint256
   ) with
      Global => (In_Out => Storage_State);

   --  Pop element from array end (returns popped value)
   --  Note: SPARK_Mode Off because function has side effects (writes storage)
   function Array_Pop (Base_Slot : Uint256) return Storage_Result with
      SPARK_Mode => Off;

   ---------------------------------------------------------------------------
   --  Packed Storage Operations (Multiple values in one slot)
   ---------------------------------------------------------------------------

   --  Pack two U128 values into one U256 slot
   procedure Pack_U128 (
      Slot  : in Uint256;
      High  : in Uint256;  -- Upper 128 bits
      Low   : in Uint256   -- Lower 128 bits
   ) with
      Global => (In_Out => Storage_State);

   --  Unpack U256 into two U128 values
   procedure Unpack_U128 (
      Slot : in  Uint256;
      High : out Uint256;
      Low  : out Uint256
   ) with
      Global => (Input => Storage_State);

   --  Pack four U64 values into one U256 slot
   procedure Pack_U64x4 (
      Slot : in Uint256;
      V0   : in Word64;
      V1   : in Word64;
      V2   : in Word64;
      V3   : in Word64
   ) with
      Global => (In_Out => Storage_State);

   --  Unpack U256 into four U64 values
   procedure Unpack_U64x4 (
      Slot : in  Uint256;
      V0   : out Word64;
      V1   : out Word64;
      V2   : out Word64;
      V3   : out Word64
   ) with
      Global => (Input => Storage_State);

   ---------------------------------------------------------------------------
   --  Storage Slot Calculation Helpers
   ---------------------------------------------------------------------------

   --  Calculate mapping slot: keccak256(key || base_slot)
   function Mapping_Slot (
      Base_Slot : Uint256;
      Key       : Uint256
   ) return Uint256 with
      Global => null;

   --  Calculate nested mapping slot
   function Nested_Mapping_Slot (
      Base_Slot : Uint256;
      Key1      : Uint256;
      Key2      : Uint256
   ) return Uint256 with
      Global => null;

   --  Calculate array element slot
   function Array_Slot (
      Base_Slot : Uint256;
      Index     : Uint256
   ) return Uint256 with
      Global => null;

   --  Convert address to U256 key
   function Address_Key (Addr : Address) return Uint256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Transient Storage (EIP-1153 style)
   ---------------------------------------------------------------------------

   --  Transient storage is cleared at end of transaction
   --  Lower gas cost than persistent storage

   --  Read from transient storage
   procedure TLoad (
      Key   : in  Uint256;
      Value : out Uint256
   ) with
      Global => (Input => Transient_State);

   --  Write to transient storage
   procedure TStore (
      Key   : in Uint256;
      Value : in Uint256
   ) with
      Global => (In_Out => Transient_State);

private

   --  Internal: Hash for slot calculation (uses Keccak256)
   function Slot_Hash (Data : Bytes32) return Uint256 with
      Global => null;

end Khepri_State;
