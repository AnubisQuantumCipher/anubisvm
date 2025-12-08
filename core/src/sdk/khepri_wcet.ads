--  KHEPRI WCET (Worst-Case Execution Time) Analysis
--
--  Provides static analysis for execution cost bounds, enabling
--  mathematically proven gas limits for KHEPRI contracts.
--
--  Features:
--  - Instruction cycle counting
--  - Memory access cost modeling
--  - Cryptographic operation costs
--  - Loop bound analysis
--  - Gas estimation with safety margins
--
--  This enables the unique KHEPRI feature of "proven gas bounds"
--  where contracts can guarantee they will never exceed gas limits.
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 4: Gas & WCET
--  - SPARK User"s Guide, Chapter on WCET Analysis
--  - aiT WCET Analysis documentation
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;

package Khepri_WCET with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Basic Cost Types
   ---------------------------------------------------------------------------

   --  Gas unit (smallest execution cost unit)
   subtype Gas is Unsigned_64;

   --  CPU cycles (for hardware-level analysis)
   subtype Cycles is Unsigned_64;

   --  Memory operations count
   subtype Mem_Ops is Unsigned_32;

   --  Zero gas constant
   Gas_Zero : constant Gas := 0;

   --  Maximum gas for a single transaction
   Max_Transaction_Gas : constant Gas := 30_000_000;

   --  Maximum gas for a block
   Max_Block_Gas : constant Gas := 1_000_000_000;

   ---------------------------------------------------------------------------
   --  Instruction Costs
   ---------------------------------------------------------------------------

   --  Base costs for primitive operations (in gas units)
   type Instruction_Costs is record
      --  Arithmetic
      Add_Cost         : Gas;    --  Integer addition
      Sub_Cost         : Gas;    --  Integer subtraction
      Mul_Cost         : Gas;    --  Integer multiplication
      Div_Cost         : Gas;    --  Integer division
      Mod_Cost         : Gas;    --  Modulo operation
      Exp_Cost         : Gas;    --  Exponentiation (base)
      Exp_Per_Byte     : Gas;    --  Exponentiation per byte of exponent

      --  Bitwise
      And_Cost         : Gas;
      Or_Cost          : Gas;
      Xor_Cost         : Gas;
      Not_Cost         : Gas;
      Shift_Cost       : Gas;

      --  Comparison
      Cmp_Cost         : Gas;    --  Any comparison

      --  Memory
      Load_Cost        : Gas;    --  Memory load
      Store_Cost       : Gas;    --  Memory store
      Copy_Per_Word    : Gas;    --  Memory copy per 32 bytes

      --  Control flow
      Jump_Cost        : Gas;    --  Unconditional jump
      Branch_Cost      : Gas;    --  Conditional branch
      Call_Cost        : Gas;    --  Function call overhead
      Return_Cost      : Gas;    --  Function return
   end record;

   --  Standard instruction costs (EVM-compatible)
   Standard_Costs : constant Instruction_Costs := (
      Add_Cost       => 3,
      Sub_Cost       => 3,
      Mul_Cost       => 5,
      Div_Cost       => 5,
      Mod_Cost       => 5,
      Exp_Cost       => 10,
      Exp_Per_Byte   => 50,
      And_Cost       => 3,
      Or_Cost        => 3,
      Xor_Cost       => 3,
      Not_Cost       => 3,
      Shift_Cost     => 3,
      Cmp_Cost       => 3,
      Load_Cost      => 3,
      Store_Cost     => 3,
      Copy_Per_Word  => 3,
      Jump_Cost      => 8,
      Branch_Cost    => 10,
      Call_Cost      => 700,
      Return_Cost    => 0
   );

   ---------------------------------------------------------------------------
   --  Storage Costs
   ---------------------------------------------------------------------------

   --  Storage operation costs
   type Storage_Costs is record
      SLOAD_Cold     : Gas;    --  First load of a slot
      SLOAD_Warm     : Gas;    --  Subsequent loads
      SSTORE_Set     : Gas;    --  Set non-zero from zero
      SSTORE_Reset   : Gas;    --  Change non-zero value
      SSTORE_Clear   : Gas;    --  Set to zero (refund eligible)
      SSTORE_Refund  : Gas;    --  Refund for clearing
   end record;

   --  Standard storage costs
   Standard_Storage : constant Storage_Costs := (
      SLOAD_Cold   => 2100,
      SLOAD_Warm   => 100,
      SSTORE_Set   => 20000,
      SSTORE_Reset => 2900,
      SSTORE_Clear => 2900,
      SSTORE_Refund => 4800
   );

   ---------------------------------------------------------------------------
   --  Cryptographic Operation Costs
   ---------------------------------------------------------------------------

   type Crypto_Costs is record
      --  Hashing
      SHA3_Base        : Gas;    --  SHA3/Keccak base cost
      SHA3_Per_Word    : Gas;    --  Per 32-byte word
      BLAKE2_Base      : Gas;    --  BLAKE2b base cost
      BLAKE2_Per_Word  : Gas;    --  Per 32-byte word

      --  Signatures
      ECDSA_Verify     : Gas;    --  secp256k1 signature verify
      ED25519_Verify   : Gas;    --  Ed25519 signature verify
      MLDSA_Verify     : Gas;    --  ML-DSA-87 signature verify
      MLDSA_Sign       : Gas;    --  ML-DSA-87 signing (off-chain typically)

      --  Key encapsulation
      MLKEM_Encaps     : Gas;    --  ML-KEM-1024 encapsulation
      MLKEM_Decaps     : Gas;    --  ML-KEM-1024 decapsulation

      --  Pairing operations (for ZK proofs)
      EC_Add           : Gas;    --  Elliptic curve addition
      EC_Mul           : Gas;    --  Elliptic curve scalar multiplication
      Pairing_Base     : Gas;    --  Pairing base cost
      Pairing_Per_Pair : Gas;    --  Cost per additional pair
   end record;

   --  Standard cryptographic costs
   Standard_Crypto : constant Crypto_Costs := (
      SHA3_Base       => 30,
      SHA3_Per_Word   => 6,
      BLAKE2_Base     => 15,
      BLAKE2_Per_Word => 3,
      ECDSA_Verify    => 3000,
      ED25519_Verify  => 2000,
      MLDSA_Verify    => 50000,
      MLDSA_Sign      => 100000,
      MLKEM_Encaps    => 30000,
      MLKEM_Decaps    => 30000,
      EC_Add          => 150,
      EC_Mul          => 6000,
      Pairing_Base    => 45000,
      Pairing_Per_Pair => 34000
   );

   ---------------------------------------------------------------------------
   --  External Call Costs
   ---------------------------------------------------------------------------

   type Call_Costs is record
      Call_Base        : Gas;    --  Base call cost
      Call_Value       : Gas;    --  Additional if value transfer
      Call_New_Account : Gas;    --  If calling new account
      Call_Stipend     : Gas;    --  Gas stipend for calls

      --  Contract creation
      Create_Base      : Gas;    --  CREATE base cost
      Create_Per_Byte  : Gas;    --  Per byte of deployed code
      Create2_Base     : Gas;    --  CREATE2 base cost
   end record;

   --  Standard call costs
   Standard_Call : constant Call_Costs := (
      Call_Base        => 100,
      Call_Value       => 9000,
      Call_New_Account => 25000,
      Call_Stipend     => 2300,
      Create_Base      => 32000,
      Create_Per_Byte  => 200,
      Create2_Base     => 32000
   );

   ---------------------------------------------------------------------------
   --  Complete Cost Model
   ---------------------------------------------------------------------------

   type Cost_Model is record
      Instructions : Instruction_Costs;
      Storage      : Storage_Costs;
      Crypto       : Crypto_Costs;
      Calls        : Call_Costs;
   end record;

   --  Standard cost model (EVM-compatible with PQC extensions)
   Standard_Model : constant Cost_Model := (
      Instructions => Standard_Costs,
      Storage      => Standard_Storage,
      Crypto       => Standard_Crypto,
      Calls        => Standard_Call
   );

   ---------------------------------------------------------------------------
   --  WCET Bounds Types
   ---------------------------------------------------------------------------

   --  Execution bounds for a code segment
   type Execution_Bound is record
      Min_Gas     : Gas;        --  Best case gas usage
      Max_Gas     : Gas;        --  Worst case gas usage
      Avg_Gas     : Gas;        --  Average case (estimated)
      Confidence  : Natural;    --  Confidence level (0-100%)
   end record;

   --  Unknown/unanalyzed bound
   Unknown_Bound : constant Execution_Bound := (
      Min_Gas    => 0,
      Max_Gas    => Gas'Last,
      Avg_Gas    => 0,
      Confidence => 0
   );

   --  Exact bound (when min = max)
   function Is_Exact (Bound : Execution_Bound) return Boolean is
      (Bound.Min_Gas = Bound.Max_Gas) with
      Global => null;

   --  Proven bound (high confidence, reasonable spread)
   function Is_Proven (Bound : Execution_Bound) return Boolean is
      (Bound.Confidence >= 95 and then Bound.Max_Gas < Gas'Last) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Loop Analysis Types
   ---------------------------------------------------------------------------

   --  Loop bound annotation
   type Loop_Bound is record
      Min_Iterations  : Natural;    --  Minimum loop iterations
      Max_Iterations  : Natural;    --  Maximum loop iterations
      Body_Cost       : Gas;        --  Cost per iteration
      Proven          : Boolean;    --  Whether bound is proven
   end record;

   --  Calculate loop gas cost
   function Loop_Gas (Bound : Loop_Bound) return Execution_Bound with
      Global => null,
      Pre    => Bound.Max_Iterations <= Natural'Last / Natural (Bound.Body_Cost + 1);

   ---------------------------------------------------------------------------
   --  Function Analysis Types
   ---------------------------------------------------------------------------

   --  Function WCET annotation
   type Function_WCET is record
      Name        : Bounded_String;
      Base_Cost   : Gas;            --  Fixed overhead
      Per_Byte    : Gas;            --  Cost per input byte (if applicable)
      Max_Input   : Natural;        --  Maximum input size
      Bound       : Execution_Bound;
   end record;

   ---------------------------------------------------------------------------
   --  Contract Analysis Types
   ---------------------------------------------------------------------------

   --  Entry point WCET summary
   Max_Entry_Points : constant := 256;
   type Entry_Index is range 0 .. Max_Entry_Points - 1;

   type Entry_WCET is record
      Func_Selector : Selector;       --  Function selector (4 bytes)
      Bound         : Execution_Bound;
      May_Revert    : Boolean;        --  Can this entry point revert?
      State_Write   : Boolean;        --  Does it modify state?
   end record;

   type Entry_WCET_Array is array (Entry_Index) of Entry_WCET;

   --  Complete contract analysis
   type Contract_Analysis is record
      Entry_Points    : Entry_WCET_Array;
      Entry_Count     : Natural;
      Total_Code_Size : Natural;
      Storage_Slots   : Natural;
      Max_Stack_Depth : Natural;
      Proven_Safe     : Boolean;   --  All entry points have proven bounds
   end record;

   ---------------------------------------------------------------------------
   --  Gas Estimation Functions
   ---------------------------------------------------------------------------

   --  Estimate gas for a hash operation
   function Hash_Gas (
      Algorithm  : Natural;    --  0=SHA3, 1=BLAKE2
      Input_Size : Natural;
      Model      : Cost_Model
   ) return Gas with
      Global => null;

   --  Estimate gas for signature verification
   function Verify_Gas (
      Algorithm : Natural;     --  0=ECDSA, 1=Ed25519, 2=ML-DSA
      Model     : Cost_Model
   ) return Gas with
      Global => null;

   --  Estimate gas for memory copy
   function Copy_Gas (
      Bytes : Natural;
      Model : Cost_Model
   ) return Gas with
      Global => null,
      Pre   => Bytes <= Natural'Last - 31;

   --  Estimate gas for storage operations
   function Storage_Read_Gas (
      Slot_Count : Natural;
      Cold       : Boolean;
      Model      : Cost_Model
   ) return Gas with
      Global => null,
      Pre   => Slot_Count <= Natural'Last / Natural (Model.Storage.SLOAD_Cold + 1);

   function Storage_Write_Gas (
      Slot_Count : Natural;
      Is_Set     : Boolean;   --  Setting vs resetting
      Model      : Cost_Model
   ) return Gas with
      Global => null,
      Pre   => Slot_Count <= Natural'Last / Natural (Model.Storage.SSTORE_Set + 1);

   ---------------------------------------------------------------------------
   --  Bound Combination Functions
   ---------------------------------------------------------------------------

   --  Combine sequential bounds (add)
   function Sequence (A, B : Execution_Bound) return Execution_Bound with
      Global => null,
      Pre    => A.Max_Gas <= Gas'Last - B.Max_Gas;

   --  Combine branch bounds (max)
   function Branch (A, B : Execution_Bound) return Execution_Bound with
      Global => null;

   --  Scale bound by factor
   function Scale (Bound : Execution_Bound; Factor : Natural) return Execution_Bound with
      Global => null,
      Pre    => Factor = 0 or else Bound.Max_Gas <= Gas'Last / Gas (Factor);

   ---------------------------------------------------------------------------
   --  Safety Margin Functions
   ---------------------------------------------------------------------------

   --  Add safety margin to bound (percentage)
   function With_Margin (
      Bound   : Execution_Bound;
      Percent : Natural
   ) return Execution_Bound with
      Global => null,
      Pre    => Percent <= 100 and then
                Bound.Max_Gas <= Gas'Last - (Bound.Max_Gas * Gas (Percent)) / 100;

   --  Check if transaction is safe with given gas limit
   function Is_Safe (
      Bound     : Execution_Bound;
      Gas_Limit : Gas
   ) return Boolean is
      (Bound.Max_Gas <= Gas_Limit) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Reporting Types
   ---------------------------------------------------------------------------

   type WCET_Report is record
      Contract_Name   : Bounded_String;
      Analysis        : Contract_Analysis;
      Model_Used      : Cost_Model;
      Generated_At    : Unsigned_64;     --  Unix timestamp
      Analyzer_Version : Natural;
   end record;

   --  Empty report constant
   Empty_Report : constant WCET_Report := (
      Contract_Name => Empty_String,
      Analysis      => (
         Entry_Points    => (others => (
            Func_Selector => (0, 0, 0, 0),
            Bound         => Unknown_Bound,
            May_Revert  => True,
            State_Write => False
         )),
         Entry_Count     => 0,
         Total_Code_Size => 0,
         Storage_Slots   => 0,
         Max_Stack_Depth => 0,
         Proven_Safe     => False
      ),
      Model_Used       => Standard_Model,
      Generated_At     => 0,
      Analyzer_Version => 1
   );

end Khepri_WCET;
