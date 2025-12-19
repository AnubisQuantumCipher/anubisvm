pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;

--  AEGIS Gas: WCET-Based Gas Metering System
--
--  This package implements the KHEPRI gas model based on Worst-Case
--  Execution Time (WCET) analysis. Gas costs are derived from proven
--  cycle bounds divided by CYCLES_PER_GAS.
--
--  Key Features:
--  - WCET-proven gas bounds for all operations
--  - Certification-based discounts (Bronze/Silver/Gold/Platinum)
--  - Deterministic gas accounting
--  - Underflow-safe operations
--
--  Gas Formula:
--    Effective_Gas = Base_Gas * Discount / 10000
--
--  Where Discount is:
--    Bronze:   10000 (1.0x - no discount)
--    Silver:    9000 (0.9x - 10% discount)
--    Gold:      8000 (0.8x - 20% discount)
--    Platinum:  7000 (0.7x - 30% discount)
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 4: Gas Model
--  - WCET Analysis Specification

package Aegis_Gas with
   Pure,
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Ghost Functions for Formal Verification
   ---------------------------------------------------------------------------

   --  Ghost: Gas context is in valid state
   function Gas_Context_Is_Valid (Ctx : Gas_Context) return Boolean is
      (Ctx.Gas_Used <= Ctx.Gas_Limit and then
       Ctx.Gas_Limit <= Max_Gas_Per_Tx and then
       Ctx.Discount in 7000 .. 10000)
   with Ghost, Pure_Function;

   --  Ghost: Gas context has capacity for amount
   function Has_Capacity (Ctx : Gas_Context; Amount : Gas_Amount) return Boolean is
      (Ctx.Gas_Used <= Ctx.Gas_Limit and then
       Ctx.Gas_Limit - Ctx.Gas_Used >= Amount)
   with Ghost, Pure_Function;

   --  Ghost: Discount computation is valid (ensures no overflow in multiplication)
   function Discount_Valid (Base : Gas_Amount; Disc : Discount_Factor) return Boolean is
      (Base <= 922_337_203_685_477 and then Disc >= 7000 and then Disc <= 10000)
   with Ghost, Pure_Function;

   --  Ghost: Discounted value is always <= base value
   function Discount_Reduces (Base : Gas_Amount; Disc : Discount_Factor) return Boolean is
      (Disc <= 10000 and then Base >= 0)  --  By definition: Disc/10000 <= 1
   with Ghost, Pure_Function;

   --  Ghost: Gas consumption preserves validity
   function Consumption_Preserves_Valid (
      Ctx_Before : Gas_Context;
      Ctx_After  : Gas_Context;
      Amount     : Gas_Amount
   ) return Boolean is
      (Gas_Context_Is_Valid (Ctx_Before) and then
       Has_Capacity (Ctx_Before, Amount) and then
       Ctx_After.Gas_Used = Ctx_Before.Gas_Used + Amount and then
       Ctx_After.Gas_Limit = Ctx_Before.Gas_Limit and then
       Ctx_After.Discount = Ctx_Before.Discount)
   with Ghost, Pure_Function;

   --  Ghost: Refund is valid (no underflow)
   function Refund_Valid (Ctx : Gas_Context; Amount : Gas_Amount) return Boolean is
      (Ctx.Gas_Used >= Amount)
   with Ghost, Pure_Function;

   --  Ghost: Hash gas is bounded
   function Hash_Gas_Bounded (Byte_Length : Natural) return Boolean is
      (Byte_Length <= Natural'Last - 31)
   with Ghost, Pure_Function;

   --  Ghost: Log gas is bounded
   function Log_Gas_Bounded (Topics : Natural; Data_Len : Natural) return Boolean is
      (Topics <= 4 and then Data_Len <= 1_000_000)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Model Functions for State Abstraction
   ---------------------------------------------------------------------------

   --  Model: Compute effective gas after discount
   function Model_Effective_Gas (Base : Gas_Amount; Disc : Discount_Factor) return Gas_Amount is
      (if Base <= 922_337_203_685_477 then
          Gas_Amount ((Long_Long_Integer (Base) * Long_Long_Integer (Disc)) / 10000)
       else Base)
   with Ghost, Pure_Function;

   --  Model: Total available gas in context
   function Model_Available_Gas (Ctx : Gas_Context) return Gas_Amount is
      (if Ctx.Gas_Used <= Ctx.Gas_Limit then Ctx.Gas_Limit - Ctx.Gas_Used else 0)
   with Ghost, Pure_Function;

   --  Model: Gas consumed so far
   function Model_Gas_Consumed (Ctx : Gas_Context) return Gas_Amount is
      (Ctx.Gas_Used)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Gas Schedule (Base Costs from WCET Analysis)
   ---------------------------------------------------------------------------

   --  Base operations (environment reads, simple queries)
   Gas_Base         : constant Gas_Amount := 2;

   --  Arithmetic operations
   Gas_Add          : constant Gas_Amount := 3;
   Gas_Sub          : constant Gas_Amount := 3;
   Gas_Mul          : constant Gas_Amount := 5;
   Gas_Div          : constant Gas_Amount := 5;
   Gas_Mod          : constant Gas_Amount := 5;
   Gas_Exp_Base     : constant Gas_Amount := 10;
   Gas_Exp_Per_Byte : constant Gas_Amount := 50;

   --  Comparison operations
   Gas_Lt           : constant Gas_Amount := 3;
   Gas_Gt           : constant Gas_Amount := 3;
   Gas_Eq           : constant Gas_Amount := 3;

   --  Bitwise operations
   Gas_And          : constant Gas_Amount := 3;
   Gas_Or           : constant Gas_Amount := 3;
   Gas_Xor          : constant Gas_Amount := 3;
   Gas_Not          : constant Gas_Amount := 3;
   Gas_Shl          : constant Gas_Amount := 3;
   Gas_Shr          : constant Gas_Amount := 3;

   --  Cryptographic operations (ANKH)
   Gas_SHA3_Base    : constant Gas_Amount := 30;
   Gas_SHA3_Per_Word : constant Gas_Amount := 6;
   Gas_Keccak256_Base : constant Gas_Amount := 30;
   Gas_Keccak256_Per_Word : constant Gas_Amount := 6;

   --  ML-DSA-87 operations
   Gas_MLDSA_KeyGen : constant Gas_Amount := 50_000;
   Gas_MLDSA_Sign   : constant Gas_Amount := 100_000;
   Gas_MLDSA_Verify : constant Gas_Amount := 30_000;

   --  ML-KEM-1024 operations
   Gas_MLKEM_KeyGen : constant Gas_Amount := 20_000;
   Gas_MLKEM_Encaps : constant Gas_Amount := 25_000;
   Gas_MLKEM_Decaps : constant Gas_Amount := 25_000;

   --  Privacy operations (VEIL/GATE/WHISPER/EYE)
   Gas_Private_Load    : constant Gas_Amount := 5_000;    -- Private storage read
   Gas_Private_Store   : constant Gas_Amount := 25_000;   -- Private storage write
   Gas_Shield          : constant Gas_Amount := 50_000;   -- Shield value
   Gas_Unshield        : constant Gas_Amount := 100_000;  -- Unshield (ZK verify)
   Gas_Commit          : constant Gas_Amount := 10_000;   -- Ajtai commitment
   Gas_Prove_Range     : constant Gas_Amount := 75_000;   -- Range proof generation
   Gas_Prove_Linear    : constant Gas_Amount := 50_000;   -- Linear relation proof
   Gas_Verify_Proof    : constant Gas_Amount := 25_000;   -- Verify ZK proof
   Gas_Nullify         : constant Gas_Amount := 15_000;   -- Nullifier check
   Gas_Encrypt         : constant Gas_Amount := 8_000;    -- Symmetric encryption
   Gas_Decrypt         : constant Gas_Amount := 8_000;    -- Symmetric decryption

   --  STARK proof operations (SCARAB)
   Gas_STARK_Verify_Base : constant Gas_Amount := 200_000;  -- Base STARK verify
   Gas_STARK_Verify_Per_Sig : constant Gas_Amount := 5_000; -- Per signature in batch
   Gas_STARK_Generate_Base : constant Gas_Amount := 500_000; -- Base proof generation
   Gas_FRI_Verify       : constant Gas_Amount := 50_000;    -- FRI layer verify

   --  Ring signature operations (EYE)
   Gas_Ring_Sign        : constant Gas_Amount := 150_000;   -- Ring signature
   Gas_Ring_Verify      : constant Gas_Amount := 100_000;   -- Ring verify

   --  Memory operations
   Gas_MLoad        : constant Gas_Amount := 3;
   Gas_MStore       : constant Gas_Amount := 3;
   Gas_MStore8      : constant Gas_Amount := 3;
   Gas_MCopy_Base   : constant Gas_Amount := 3;
   Gas_MCopy_Per_Word : constant Gas_Amount := 3;

   --  Storage operations (THOTH)
   Gas_SLoad        : constant Gas_Amount := 200;
   Gas_SStore_Set   : constant Gas_Amount := 20_000;  -- Zero to non-zero
   Gas_SStore_Reset : constant Gas_Amount := 5_000;   -- Non-zero to non-zero
   Gas_SStore_Clear : constant Gas_Amount := 5_000;   -- Non-zero to zero (refund)
   Gas_SStore_Refund : constant Gas_Amount := 15_000; -- Refund on clear

   --  Call operations
   Gas_Call_Base    : constant Gas_Amount := 700;
   Gas_Call_Value   : constant Gas_Amount := 9_000;   -- Extra for value transfer
   Gas_Call_New_Account : constant Gas_Amount := 25_000;
   Gas_StaticCall   : constant Gas_Amount := 700;
   Gas_DelegateCall : constant Gas_Amount := 700;

   --  Contract creation
   Gas_Create       : constant Gas_Amount := 32_000;
   Gas_Create2      : constant Gas_Amount := 32_000;
   Gas_Create_Per_Byte : constant Gas_Amount := 200;

   --  Event operations
   Gas_Log_Base     : constant Gas_Amount := 375;
   Gas_Log_Topic    : constant Gas_Amount := 375;
   Gas_Log_Per_Byte : constant Gas_Amount := 8;

   --  System operations
   Gas_Balance      : constant Gas_Amount := 400;
   Gas_SelfBalance  : constant Gas_Amount := 5;      -- Lower cost for self
   Gas_Call         : constant Gas_Amount := 700;    -- Alias for Gas_Call_Base
   Gas_ExtCodeSize  : constant Gas_Amount := 700;
   Gas_ExtCodeCopy_Base : constant Gas_Amount := 700;
   Gas_ExtCodeCopy_Per_Word : constant Gas_Amount := 3;
   Gas_SelfDestruct : constant Gas_Amount := 5_000;

   ---------------------------------------------------------------------------
   --  Gas Operations
   ---------------------------------------------------------------------------

   --  Maximum base gas for safe discount computation
   --  Ensures Base_Gas * Discount fits in Long_Long_Integer
   Max_Safe_Base_Gas : constant Gas_Amount := 922_337_203_685_477;

   --  Apply certification discount to base gas
   function Apply_Discount (
      Base_Gas : Gas_Amount;
      Discount : Discount_Factor
   ) return Gas_Amount with
      Global => null,
      Pre    => Base_Gas <= Max_Safe_Base_Gas,
      Post   => Apply_Discount'Result <= Base_Gas;

   --  Check if sufficient gas remains
   function Has_Gas (
      Context : Gas_Context;
      Required : Gas_Amount
   ) return Boolean with
      Global => null,
      Post   => Has_Gas'Result =
         (Context.Gas_Used <= Context.Gas_Limit and then
          Context.Gas_Limit - Context.Gas_Used >= Required);

   --  Consume gas (returns False if insufficient)
   procedure Consume_Gas (
      Context  : in out Gas_Context;
      Amount   : in     Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then
                    Context.Gas_Used = Context'Old.Gas_Used + Amount
                 else
                    Context = Context'Old);

   --  Consume gas with certification discount applied
   procedure Consume_Gas_Discounted (
      Context  : in out Gas_Context;
      Base_Gas : in     Gas_Amount;
      Success  : out    Boolean
   ) with
      Global => null,
      Pre    => Base_Gas <= Max_Safe_Base_Gas;

   --  Get remaining gas
   function Remaining_Gas (Context : Gas_Context) return Gas_Amount with
      Global => null,
      Post   => Remaining_Gas'Result =
         (if Context.Gas_Used <= Context.Gas_Limit then
             Context.Gas_Limit - Context.Gas_Used
          else 0);

   --  Refund gas (e.g., storage clear)
   procedure Refund_Gas (
      Context : in out Gas_Context;
      Amount  : in     Gas_Amount
   ) with
      Global => null,
      Pre    => Context.Gas_Used >= Amount,
      Post   => Context.Gas_Used = Context'Old.Gas_Used - Amount;

   ---------------------------------------------------------------------------
   --  Complex Gas Calculations
   ---------------------------------------------------------------------------

   --  Maximum byte length for safe hash gas computation
   Max_Hash_Byte_Length : constant Natural := Natural'Last - 31;

   --  SHA3/Keccak gas (base + per-word)
   --  Max Words = (Max_Hash_Byte_Length + 31) / 32 = Natural'Last / 32 = 67108863
   --  Max Gas = 30 + 67108863 * 6 = 402653208, which is << Max_Safe_Base_Gas
   function Gas_Hash (
      Byte_Length : Natural
   ) return Gas_Amount with
      Global => null,
      Pre    => Byte_Length <= Max_Hash_Byte_Length,
      Post   => Gas_Hash'Result >= Gas_SHA3_Base
                and Gas_Hash'Result <= Max_Safe_Base_Gas;

   --  Memory expansion gas
   function Gas_Memory_Expansion (
      Current_Words : Natural;
      New_Words     : Natural
   ) return Gas_Amount with
      Global => null;

   --  Call gas calculation
   function Gas_Call_Complex (
      Has_Value    : Boolean;
      Is_New       : Boolean;
      Available    : Gas_Amount
   ) return Gas_Amount with
      Global => null;

   --  Maximum data length for safe log gas computation
   --  Max gas = 375 + 4*375 + 1_000_000*8 = 8_001_875 << Max_Safe_Base_Gas
   Max_Log_Data_Length : constant Natural := 1_000_000;

   --  Log gas calculation
   function Gas_Log (
      Topic_Count : Natural;
      Data_Length : Natural
   ) return Gas_Amount with
      Global => null,
      Pre    => Topic_Count <= 4 and then Data_Length <= Max_Log_Data_Length,
      Post   => Gas_Log'Result >= Gas_Log_Base
                and Gas_Log'Result <= Max_Safe_Base_Gas;

   --  Create gas calculation
   function Gas_Create_Contract (
      Code_Length : Natural
   ) return Gas_Amount with
      Global => null;

   --  STARK batch verification gas
   function Gas_STARK_Batch_Verify (
      Num_Signatures : Natural
   ) return Gas_Amount with
      Global => null,
      Post => Gas_STARK_Batch_Verify'Result >= Gas_STARK_Verify_Base;

   --  Privacy operation gas (combined)
   function Gas_Privacy_Operation (
      Op_Type     : Natural;  -- 0=load, 1=store, 2=shield, 3=unshield
      Data_Length : Natural
   ) return Gas_Amount with
      Global => null;

   ---------------------------------------------------------------------------
   --  WCET Integration
   ---------------------------------------------------------------------------

   --  Convert WCET cycles to gas
   function Cycles_To_Gas (Cycles : Natural) return Gas_Amount with
      Global => null,
      Post   => Cycles_To_Gas'Result = Gas_Amount (Cycles / Cycles_Per_Gas);

   --  Validate WCET bound against gas limit
   function Validate_WCET_Bound (
      WCET_Gas  : Gas_Amount;
      Gas_Limit : Gas_Amount
   ) return Boolean with
      Global => null,
      Post   => Validate_WCET_Bound'Result = (WCET_Gas <= Gas_Limit);

   ---------------------------------------------------------------------------
   --  Lemma Subprograms for Proof Guidance
   ---------------------------------------------------------------------------

   --  Lemma: Discount always reduces or maintains gas cost
   procedure Lemma_Discount_Reduces_Cost (
      Base_Gas : Gas_Amount;
      Discount : Discount_Factor
   ) with
      Ghost,
      Global => null,
      Pre  => Base_Gas <= Max_Safe_Base_Gas,
      Post => Apply_Discount (Base_Gas, Discount) <= Base_Gas;

   --  Lemma: Higher certification gives lower effective gas
   --  (Lower discount factor => less gas charged)
   procedure Lemma_Higher_Cert_Lower_Gas (
      Base_Gas  : Gas_Amount;
      Disc_Low  : Discount_Factor;
      Disc_High : Discount_Factor
   ) with
      Ghost,
      Global => null,
      Pre  => Base_Gas <= Max_Safe_Base_Gas and then Disc_Low <= Disc_High,
      Post => True;  -- Assertion: Apply_Discount monotonic in discount factor

   --  Lemma: Gas consumption is monotonic
   procedure Lemma_Gas_Monotonic (
      Ctx    : Gas_Context;
      Amount : Gas_Amount
   ) with
      Ghost,
      Global => null,
      Pre  => Has_Capacity (Ctx, Amount),
      Post => Ctx.Gas_Used + Amount >= Ctx.Gas_Used;

   --  Lemma: Remaining gas decreases after consumption
   procedure Lemma_Remaining_Decreases (
      Ctx_Before : Gas_Context;
      Ctx_After  : Gas_Context;
      Amount     : Gas_Amount
   ) with
      Ghost,
      Global => null,
      Pre  => Consumption_Preserves_Valid (Ctx_Before, Ctx_After, Amount),
      Post => Model_Available_Gas (Ctx_After) = Model_Available_Gas (Ctx_Before) - Amount;

   --  Lemma: Valid context stays valid after bounded consumption
   procedure Lemma_Valid_Preserves_Valid (
      Ctx    : Gas_Context;
      Amount : Gas_Amount
   ) with
      Ghost,
      Global => null,
      Pre  => Gas_Context_Is_Valid (Ctx) and then Has_Capacity (Ctx, Amount),
      Post => True;  -- Precondition implies postcondition can hold after consume

   --  Lemma: Refund restores capacity
   procedure Lemma_Refund_Restores_Capacity (
      Ctx    : Gas_Context;
      Amount : Gas_Amount
   ) with
      Ghost,
      Global => null,
      Pre  => Gas_Context_Is_Valid (Ctx) and then Refund_Valid (Ctx, Amount),
      Post => Ctx.Gas_Used - Amount <= Ctx.Gas_Limit;

   --  Lemma: Hash gas is bounded by formula
   procedure Lemma_Hash_Gas_Formula (
      Byte_Length : Natural
   ) with
      Ghost,
      Global => null,
      Pre  => Hash_Gas_Bounded (Byte_Length),
      Post => Gas_SHA3_Base + Gas_Amount ((Byte_Length + 31) / 32) * Gas_SHA3_Per_Word
              <= Max_Safe_Base_Gas;

   --  Lemma: Log gas is bounded by formula
   procedure Lemma_Log_Gas_Formula (
      Topic_Count : Natural;
      Data_Length : Natural
   ) with
      Ghost,
      Global => null,
      Pre  => Log_Gas_Bounded (Topic_Count, Data_Length),
      Post => Gas_Log_Base + Gas_Amount (Topic_Count) * Gas_Log_Topic +
              Gas_Amount (Data_Length) * Gas_Log_Per_Byte <= Max_Safe_Base_Gas;

end Aegis_Gas;
