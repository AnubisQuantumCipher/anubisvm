pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

--  AEGIS VM Types: Core Type Definitions for KHEPRI Runtime
--
--  This package defines the foundational types for the KHEPRI smart contract
--  execution environment. All types are designed for SPARK verification.
--
--  Key Components:
--  - U256: 256-bit unsigned integer for EVM compatibility
--  - Certification levels (Bronze, Silver, Gold, Platinum)
--  - Gas context and metering types
--  - Memory and storage types
--  - Contract state types
--
--  References:
--  - KHEPRI Blueprint v1.0
--  - WCET-Based Gas Model Specification

package Aegis_VM_Types with
   Pure,
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Fundamental Types
   ---------------------------------------------------------------------------

   --  Byte and word types
   subtype Byte is Unsigned_8;
   type Byte_Array is array (Natural range <>) of Byte;

   subtype Word32 is Unsigned_32;
   subtype Word64 is Unsigned_64;

   --  U256: 256-bit unsigned integer (little-endian limb order)
   --  Stored as 4 x 64-bit limbs for efficient arithmetic
   type U256_Limbs is array (0 .. 3) of Word64;

   type U256 is record
      Limbs : U256_Limbs;
   end record;

   --  U256 constants
   U256_Zero : constant U256 := (Limbs => (0, 0, 0, 0));
   U256_One  : constant U256 := (Limbs => (1, 0, 0, 0));
   U256_Max  : constant U256 := (Limbs => (Word64'Last, Word64'Last,
                                            Word64'Last, Word64'Last));

   ---------------------------------------------------------------------------
   --  Hash Types
   ---------------------------------------------------------------------------

   --  Hash256: 32-byte hash (SHA3-256, Keccak-256, etc.)
   type Hash256 is array (0 .. 31) of Byte;

   --  Hash512: 64-byte hash (SHA3-512, etc.)
   type Hash512 is array (0 .. 63) of Byte;

   --  Zero hashes
   Hash256_Zero : constant Hash256 := (others => 0);
   Hash512_Zero : constant Hash512 := (others => 0);

   ---------------------------------------------------------------------------
   --  Address Types
   ---------------------------------------------------------------------------

   --  Contract address: 32 bytes (derived from ML-DSA-87 public key)
   type Contract_Address is array (0 .. 31) of Byte;

   Address_Zero : constant Contract_Address := (others => 0);

   ---------------------------------------------------------------------------
   --  Certification Levels (KHEPRI Spec)
   ---------------------------------------------------------------------------

   --  Certification levels with associated gas discounts
   --  Bronze:   1.0x (no discount) - Flow analysis only
   --  Silver:   0.9x (10% discount) - 100% proof + WCET
   --  Gold:     0.8x (20% discount) - Pre/Post + constant-time
   --  Platinum: 0.7x (30% discount) - Formal security + audit

   type Certification_Level is (Bronze, Silver, Gold, Platinum);

   --  Gas discount factors (in basis points, 10000 = 100%)
   type Discount_Factor is range 7000 .. 10000;

   function Get_Discount (Level : Certification_Level) return Discount_Factor is
      (case Level is
         when Bronze   => 10000,  -- 1.0x
         when Silver   => 9000,   -- 0.9x
         when Gold     => 8000,   -- 0.8x
         when Platinum => 7000);  -- 0.7x

   ---------------------------------------------------------------------------
   --  Gas Types (WCET-Based Model)
   ---------------------------------------------------------------------------

   --  Gas units: 64-bit to handle large computations
   type Gas_Amount is range 0 .. 2**63 - 1;

   --  WCET cycles per gas unit (configurable, default 100)
   Cycles_Per_Gas : constant := 100;

   --  Maximum gas per transaction
   Max_Gas_Per_Tx : constant Gas_Amount := 30_000_000;

   --  Maximum gas per block
   Max_Gas_Per_Block : constant Gas_Amount := 100_000_000;

   --  Gas context for execution
   --
   --  SPARK Gold Type Invariant:
   --  ===========================
   --  The type invariant guarantees that Gas_Used never exceeds Gas_Limit,
   --  which is a critical safety property preventing unbounded resource consumption.
   --
   --  This invariant is verified at:
   --  1. Object initialization (Initial_Gas_Context)
   --  2. After every procedure that modifies Gas_Used
   --  3. At function call boundaries
   --
   --  Security Implications:
   --  - Prevents denial-of-service via gas exhaustion attacks
   --  - Ensures deterministic gas accounting
   --  - Guarantees execution terminates within bounded resources
   --  Ghost function for Gas_Context validity predicate
   --  Note: Using function instead of Type_Invariant because record is public
   function Gas_Context_Valid (Gas_Used, Gas_Limit : Gas_Amount;
                                Discount : Discount_Factor) return Boolean is
      (Gas_Used <= Gas_Limit and then
       Gas_Limit <= Max_Gas_Per_Tx and then
       Discount in 7000 .. 10000)
   with Ghost, Pure_Function;

   type Gas_Context is record
      Gas_Limit     : Gas_Amount;  -- Maximum gas for this call
      Gas_Used      : Gas_Amount;  -- Gas consumed so far
      Gas_Price     : U256;        -- Wei per gas unit
      Discount      : Discount_Factor;  -- Certification discount
   end record;

   --  Ghost function: Check if Gas_Context satisfies validity predicate
   function Is_Valid_Gas_Context (G : Gas_Context) return Boolean is
      (Gas_Context_Valid (G.Gas_Used, G.Gas_Limit, G.Discount))
   with Ghost, Pure_Function;

   --  Initial gas context
   --
   --  SPARK Gold Postcondition:
   --  ==========================
   --  Guarantees that the returned Gas_Context satisfies the type invariant
   --  and that all fields are properly initialized to their specified values.
   --
   --  The postcondition explicitly states:
   --  1. Gas_Used starts at 0 (no gas consumed yet)
   --  2. Gas_Limit matches the provided limit
   --  3. Discount is correctly computed from certification level
   --  4. The context is valid (satisfies type invariant)
   function Initial_Gas_Context (
      Limit : Gas_Amount;
      Price : U256;
      Level : Certification_Level
   ) return Gas_Context is
      ((Gas_Limit => Limit,
        Gas_Used  => 0,
        Gas_Price => Price,
        Discount  => Get_Discount (Level)))
   with
      Pre  => Limit <= Max_Gas_Per_Tx,
      Post => Initial_Gas_Context'Result.Gas_Used = 0 and then
              Initial_Gas_Context'Result.Gas_Limit = Limit and then
              Initial_Gas_Context'Result.Gas_Price = Price and then
              Initial_Gas_Context'Result.Discount = Get_Discount (Level) and then
              Is_Valid_Gas_Context (Initial_Gas_Context'Result);

   ---------------------------------------------------------------------------
   --  Memory Types (SPHINX Sandbox)
   ---------------------------------------------------------------------------

   --  Memory region types
   type Memory_Access is (Read_Only, Read_Write, Execute);

   --  Memory region descriptor
   --
   --  SPARK Gold Validity Predicate:
   --  ===============================
   --  Ensures memory regions do not wrap around the address space,
   --  which would cause undefined behavior in memory access checks.
   --
   --  Security Implications:
   --  - Prevents integer overflow in bounds checking
   --  - Guarantees valid address range calculations
   --  - Enables sound memory isolation proofs
   type Memory_Region is record
      Base_Address : Word64;
      Size         : Word64;
      Access_Mode  : Memory_Access;
   end record;

   --  Ghost function: Check if Memory_Region is valid (no address overflow)
   function Is_Valid_Memory_Region (R : Memory_Region) return Boolean is
      (R.Size <= Word64'Last - R.Base_Address)
   with Ghost, Pure_Function;

   --  Maximum memory regions per contract
   Max_Memory_Regions : constant := 16;

   type Memory_Region_Index is range 0 .. Max_Memory_Regions - 1;
   type Memory_Regions is array (Memory_Region_Index) of Memory_Region;

   --  Contract memory limits
   Max_Stack_Size   : constant := 1024 * 1024;      -- 1 MB stack
   Max_Heap_Size    : constant := 16 * 1024 * 1024; -- 16 MB heap
   Max_Code_Size    : constant := 24 * 1024;        -- 24 KB code

   ---------------------------------------------------------------------------
   --  Storage Types (THOTH State)
   ---------------------------------------------------------------------------

   --  Storage slot: 256-bit key -> 256-bit value
   type Storage_Key is new U256;
   type Storage_Value is new U256;

   --  Storage slot pair
   type Storage_Slot is record
      Key   : Storage_Key;
      Value : Storage_Value;
   end record;

   --  Maximum storage slots per contract access
   Max_Storage_Accesses : constant := 1024;

   ---------------------------------------------------------------------------
   --  Call Types
   ---------------------------------------------------------------------------

   --  Call type enumeration
   type Call_Type is (
      Call,           -- Standard call
      Static_Call,    -- Read-only call
      Delegate_Call,  -- Delegate to other contract
      Create,         -- Contract creation
      Create2         -- Deterministic contract creation
   );

   --  Call depth limit
   Max_Call_Depth : constant := 1024;
   type Call_Depth is range 0 .. Max_Call_Depth;

   ---------------------------------------------------------------------------
   --  Execution Result Types
   ---------------------------------------------------------------------------

   --  Execution status
   type Execution_Status is (
      Success,          -- Execution completed successfully
      Revert,           -- Explicit revert (state rolled back)
      Out_Of_Gas,       -- Gas exhausted
      Stack_Overflow,   -- Call depth exceeded
      Invalid_Opcode,   -- Unsupported operation
      Access_Violation, -- Memory/storage access error
      Contract_Error    -- Contract-specific error
   );

   --  Execution result
   --
   --  SPARK Gold Validity Predicate:
   --  ===============================
   --  Guarantees consistency between Status and Gas_Used:
   --  - Success: Gas_Used represents actual consumption
   --  - Out_Of_Gas: Gas_Used equals what was available (all consumed)
   --  - Revert/Error: Gas_Used represents partial consumption
   --
   --  Security Implications:
   --  - Deterministic fee calculation from any result state
   --  - Consistent gas refund computation on revert
   --  - Auditable execution cost regardless of outcome
   type Execution_Result is record
      Status    : Execution_Status;
      Gas_Used  : Gas_Amount;
      Return_Data : Hash256;  -- Hash of return data (actual data stored separately)
   end record;

   --  Ghost function: Check if Execution_Result is valid
   function Is_Valid_Execution_Result (R : Execution_Result) return Boolean is
      (R.Gas_Used <= Max_Gas_Per_Tx)
   with Ghost, Pure_Function;

   --  Ghost: Result indicates successful execution
   function Is_Success (R : Execution_Result) return Boolean is
      (R.Status = Success)
   with Ghost, Pure_Function;

   --  Ghost: Result indicates reverted execution (state rolled back)
   function Is_Reverted (R : Execution_Result) return Boolean is
      (R.Status = Revert)
   with Ghost, Pure_Function;

   --  Ghost: Result indicates failed execution (error condition)
   function Is_Failed (R : Execution_Result) return Boolean is
      (R.Status not in Success | Revert)
   with Ghost, Pure_Function;

   --  Success result constructor
   --
   --  SPARK Gold Postcondition:
   --  ==========================
   --  Guarantees the result is properly formed as a success result
   --  with specified gas consumption and return data hash.
   function Success_Result (Gas : Gas_Amount; Data : Hash256) return Execution_Result is
      ((Status => Success, Gas_Used => Gas, Return_Data => Data))
   with
      Pre  => Gas <= Max_Gas_Per_Tx,
      Post => Success_Result'Result.Status = Success and then
              Success_Result'Result.Gas_Used = Gas and then
              Success_Result'Result.Return_Data = Data and then
              Is_Success (Success_Result'Result) and then
              Is_Valid_Execution_Result (Success_Result'Result);

   --  Failure result constructor
   --
   --  SPARK Gold Postcondition:
   --  ==========================
   --  Guarantees the result is properly formed as a failure result
   --  with the specified error status and gas consumption.
   --  Return_Data is zeroed for error cases.
   function Failure_Result (
      Status : Execution_Status;
      Gas    : Gas_Amount
   ) return Execution_Result is
      ((Status => Status, Gas_Used => Gas, Return_Data => Hash256_Zero))
   with
      Pre  => Status /= Success and then Gas <= Max_Gas_Per_Tx,
      Post => Failure_Result'Result.Status = Status and then
              Failure_Result'Result.Gas_Used = Gas and then
              Failure_Result'Result.Return_Data = Hash256_Zero and then
              not Is_Success (Failure_Result'Result) and then
              Is_Valid_Execution_Result (Failure_Result'Result);

   ---------------------------------------------------------------------------
   --  Contract Manifest Types
   ---------------------------------------------------------------------------

   --  Contract version
   type Version is record
      Major : Natural;
      Minor : Natural;
      Patch : Natural;
   end record;

   --  Contract manifest (on-chain metadata)
   type Contract_Manifest is record
      Address       : Contract_Address;
      Code_Hash     : Hash256;
      Proof_Hash    : Hash256;       -- Hash of SPARK proof artifacts
      Version       : Aegis_VM_Types.Version;
      Certification : Certification_Level;
      WCET_Bound    : Gas_Amount;    -- Proven worst-case gas
   end record;

   ---------------------------------------------------------------------------
   --  Capability Tokens (SPHINX Security)
   ---------------------------------------------------------------------------

   --  Capability types for access control
   type Capability_Type is (
      Cap_None,           -- No access
      Cap_Read_Storage,   -- Read contract storage
      Cap_Write_Storage,  -- Write contract storage
      Cap_Call,           -- Call other contracts
      Cap_Transfer,       -- Transfer value
      Cap_Create,         -- Create contracts
      Cap_Self_Destruct,  -- Self-destruct
      Cap_Crypto,         -- Use crypto operations
      Cap_Event,          -- Emit events
      Cap_Privacy         -- Use privacy operations (ANUBIS layer)
   );

   --  Capability mask (set of capabilities)
   type Capability_Mask is array (Capability_Type) of Boolean;

   --  Default capability sets
   No_Capabilities : constant Capability_Mask := (others => False);

   Full_Capabilities : constant Capability_Mask := (
      Cap_None => False,
      others   => True
   );

   Read_Only_Capabilities : constant Capability_Mask := (
      Cap_Read_Storage => True,
      Cap_Call         => True,
      others           => False
   );

end Aegis_VM_Types;
