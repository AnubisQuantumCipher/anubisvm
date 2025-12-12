pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;

--  Gas_Estimation: WCET-Based Gas Cost Calculation for AnubisVM
--
--  This package provides gas cost estimation based on Worst-Case Execution
--  Time (WCET) analysis. All gas costs are derived from proven WCET bounds
--  for SPARK-verified code paths.
--
--  Gas Cost Model:
--  - Base costs for each operation type
--  - Size-dependent costs (per-byte, per-word)
--  - Certification discounts (Bronze through Platinum)
--  - Storage cold/warm access patterns
--
--  References:
--  - KHEPRI Blueprint v1.0 (WCET-based gas model)
--  - FIPS 203 (ML-KEM) / FIPS 204 (ML-DSA) operation costs

package Gas_Estimation with
   SPARK_Mode => On,
   Pure
is

   ---------------------------------------------------------------------------
   --  Base Operation Costs
   ---------------------------------------------------------------------------

   --  Transaction base costs
   Tx_Base_Cost       : constant Gas_Amount := 21_000;    -- Base transaction
   Tx_Data_Zero_Cost  : constant Gas_Amount := 4;         -- Zero byte in calldata
   Tx_Data_Nonzero_Cost : constant Gas_Amount := 16;      -- Non-zero byte
   Tx_Create_Cost     : constant Gas_Amount := 32_000;    -- Contract creation

   --  Memory operations
   Memory_Cost_Per_Word : constant Gas_Amount := 3;       -- Memory expansion
   Memory_Copy_Per_Word : constant Gas_Amount := 3;       -- MCOPY per word
   Memory_Quadratic_Divisor : constant := 512;            -- Quadratic term divisor

   --  Storage operations (EIP-2200 style)
   Storage_Set_Cost     : constant Gas_Amount := 20_000;  -- SSTORE (new value)
   Storage_Update_Cost  : constant Gas_Amount := 5_000;   -- SSTORE (existing)
   Storage_Clear_Refund : constant Gas_Amount := 15_000;  -- Refund for clearing
   Storage_Load_Cold    : constant Gas_Amount := 2_100;   -- SLOAD cold
   Storage_Load_Warm    : constant Gas_Amount := 100;     -- SLOAD warm

   --  Account access (EIP-2929 style)
   Account_Cold_Access  : constant Gas_Amount := 2_600;   -- Cold account
   Account_Warm_Access  : constant Gas_Amount := 100;     -- Warm account

   --  Call operations
   Call_Base_Cost       : constant Gas_Amount := 100;     -- Base call cost
   Call_Value_Transfer  : constant Gas_Amount := 9_000;   -- Non-zero value
   Call_New_Account     : constant Gas_Amount := 25_000;  -- Create new account
   Call_Stipend         : constant Gas_Amount := 2_300;   -- Call stipend

   --  Cryptographic operations (post-quantum)
   SHA3_Base_Cost       : constant Gas_Amount := 30;      -- SHA3 base
   SHA3_Word_Cost       : constant Gas_Amount := 6;       -- SHA3 per word

   --  ML-DSA-87 (NIST FIPS 204) costs - based on WCET analysis
   MLDSA_Sign_Cost      : constant Gas_Amount := 100_000; -- Signature generation
   MLDSA_Verify_Cost    : constant Gas_Amount := 50_000;  -- Signature verification
   MLDSA_KeyGen_Cost    : constant Gas_Amount := 75_000;  -- Key generation

   --  ML-KEM-1024 (NIST FIPS 203) costs
   MLKEM_Encap_Cost     : constant Gas_Amount := 40_000;  -- Encapsulation
   MLKEM_Decap_Cost     : constant Gas_Amount := 35_000;  -- Decapsulation
   MLKEM_KeyGen_Cost    : constant Gas_Amount := 50_000;  -- Key generation

   --  Event emission (from contract_events)
   Event_Base_Cost      : constant Gas_Amount := 375;     -- LOG0 base
   Event_Topic_Cost     : constant Gas_Amount := 375;     -- Per topic
   Event_Data_Byte_Cost : constant Gas_Amount := 8;       -- Per data byte

   ---------------------------------------------------------------------------
   --  Gas Estimation Functions
   ---------------------------------------------------------------------------

   --  Estimate calldata cost
   function Estimate_Calldata_Gas (
      Zero_Bytes    : Natural;
      NonZero_Bytes : Natural
   ) return Gas_Amount with
      Post => Estimate_Calldata_Gas'Result >=
              Gas_Amount (Zero_Bytes) * Tx_Data_Zero_Cost;

   --  Estimate memory expansion cost
   function Estimate_Memory_Gas (
      Old_Size : Natural;
      New_Size : Natural
   ) return Gas_Amount with
      Pre => New_Size >= Old_Size;

   --  Estimate storage operation cost
   function Estimate_Storage_Gas (
      Is_Cold     : Boolean;
      Is_New_Slot : Boolean;
      Clear_Slot  : Boolean
   ) return Gas_Amount;

   --  Estimate SHA3 hash cost
   function Estimate_SHA3_Gas (
      Data_Size : Natural
   ) return Gas_Amount;

   --  Estimate event emission cost
   function Estimate_Event_Gas (
      Topic_Count : Natural;
      Data_Size   : Natural
   ) return Gas_Amount with
      Pre => Topic_Count <= 4;

   --  Estimate ML-DSA verification (per signature)
   function Estimate_MLDSA_Verify_Gas return Gas_Amount is
      (MLDSA_Verify_Cost) with Inline;

   --  Estimate ML-KEM encapsulation
   function Estimate_MLKEM_Encap_Gas return Gas_Amount is
      (MLKEM_Encap_Cost) with Inline;

   ---------------------------------------------------------------------------
   --  Certification-Adjusted Costs
   ---------------------------------------------------------------------------

   --  Apply certification discount to gas amount
   function Apply_Discount (
      Base_Cost : Gas_Amount;
      Level     : Certification_Level
   ) return Gas_Amount with
      Post => Apply_Discount'Result <= Base_Cost;

   --  Get effective gas for a contract call
   function Effective_Gas (
      Base_Gas : Gas_Amount;
      Level    : Certification_Level
   ) return Gas_Amount is
      (Apply_Discount (Base_Gas, Level)) with Inline;

   ---------------------------------------------------------------------------
   --  Gas Estimation for Contract Operations
   ---------------------------------------------------------------------------

   --  Estimate deployment gas
   function Estimate_Deploy_Gas (
      Code_Size : Natural;
      Init_Storage_Slots : Natural
   ) return Gas_Amount;

   --  Estimate invocation gas (excluding execution)
   function Estimate_Invoke_Overhead (
      Args_Size     : Natural;
      Is_Cold_Call  : Boolean;
      Has_Value     : Boolean
   ) return Gas_Amount;

   ---------------------------------------------------------------------------
   --  Intrinsic Gas Calculation
   ---------------------------------------------------------------------------

   --  Calculate intrinsic gas for a transaction
   --  This is the minimum gas required before execution begins
   function Intrinsic_Gas (
      Data_Size     : Natural;
      Is_Create     : Boolean;
      Zero_Bytes    : Natural;
      NonZero_Bytes : Natural
   ) return Gas_Amount with
      Pre => Zero_Bytes + NonZero_Bytes = Data_Size;

end Gas_Estimation;
