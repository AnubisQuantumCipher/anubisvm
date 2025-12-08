pragma SPARK_Mode (On);

with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;

--  KHEPRI Fuzzer: Property-Based and Fuzz Testing
--
--  This package provides fuzzing capabilities for KHEPRI contracts:
--  - Random input generation
--  - Property-based testing
--  - Mutation-based fuzzing
--  - Coverage-guided fuzzing
--  - Crash detection and triage
--
--  Fuzzing Strategies:
--  - Random: Pure random input generation
--  - Mutation: Mutate known-good inputs
--  - Grammar: Generate inputs based on ABI grammar
--  - Coverage: Guide fuzzing by code coverage
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.3: Fuzz Testing

package Khepri_Fuzzer with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Fuzzer Configuration
   ---------------------------------------------------------------------------

   --  Fuzzing strategy
   type Fuzz_Strategy is (
      Strategy_Random,      --  Pure random generation
      Strategy_Mutation,    --  Mutate existing inputs
      Strategy_Grammar,     --  ABI-aware generation
      Strategy_Coverage     --  Coverage-guided
   );

   --  Mutation type
   type Mutation_Type is (
      Mut_Bit_Flip,         --  Flip random bits
      Mut_Byte_Flip,        --  Flip random bytes
      Mut_Arithmetic,       --  Add/subtract small values
      Mut_Interesting,      --  Insert interesting values
      Mut_Block_Delete,     --  Delete blocks of bytes
      Mut_Block_Insert,     --  Insert blocks of bytes
      Mut_Block_Shuffle     --  Shuffle blocks
   );

   --  Fuzzer configuration
   type Fuzzer_Config is record
      --  Strategy
      Strategy        : Fuzz_Strategy;
      Max_Iterations  : Natural;
      Max_Input_Size  : Natural;
      Timeout_Ms      : Natural;

      --  Coverage
      Track_Coverage  : Boolean;
      Coverage_Goal   : Natural;  --  Target coverage %

      --  Mutation settings
      Mutation_Rate   : Natural;  --  0-100%
      Max_Mutations   : Natural;

      --  Seed corpus
      Use_Corpus      : Boolean;
      Corpus_Path     : Bounded_String;

      --  Output
      Save_Crashes    : Boolean;
      Save_Corpus     : Boolean;
      Verbose         : Boolean;
   end record;

   Default_Fuzzer_Config : constant Fuzzer_Config := (
      Strategy        => Strategy_Random,
      Max_Iterations  => 10000,
      Max_Input_Size  => 4096,
      Timeout_Ms      => 1000,
      Track_Coverage  => True,
      Coverage_Goal   => 80,
      Mutation_Rate   => 10,
      Max_Mutations   => 5,
      Use_Corpus      => False,
      Corpus_Path     => Empty_String,
      Save_Crashes    => True,
      Save_Corpus     => True,
      Verbose         => False
   );

   ---------------------------------------------------------------------------
   --  Input Generation
   ---------------------------------------------------------------------------

   --  Random number generator state
   type RNG_State is private;

   --  Initialize RNG with seed
   function Init_RNG (Seed : Word64) return RNG_State with
      Global => null;

   --  Generate random bytes
   procedure Generate_Bytes (
      State  : in Out RNG_State;
      Output : out    Byte_Array;
      Count  : in     Natural
   ) with
      Global => null,
      Pre    => Count <= Output'Length;

   --  Generate random Uint256
   procedure Generate_U256 (
      State  : in Out RNG_State;
      Result : out    Uint256
   ) with
      Global => null;

   --  Generate random address
   procedure Generate_Address (
      State  : in Out RNG_State;
      Result : out    Khepri_Types.Address
   ) with
      Global => null;

   --  Generate random bounded integer
   procedure Generate_Bounded (
      State  : in Out RNG_State;
      Min    : in     Natural;
      Max    : in     Natural;
      Result : out    Natural
   ) with
      Global => null,
      Pre    => Min <= Max,
      Post   => Result >= Min and Result <= Max;

   ---------------------------------------------------------------------------
   --  Interesting Values
   ---------------------------------------------------------------------------

   --  Interesting byte values for fuzzing
   Max_Interesting_Bytes : constant := 16;
   type Interesting_Byte_Array is array (0 .. Max_Interesting_Bytes - 1) of Byte;

   Interesting_Bytes : constant Interesting_Byte_Array := (
      16#00#, 16#01#, 16#7F#, 16#80#, 16#FF#,
      16#FE#, 16#7E#, 16#81#, 16#F0#, 16#0F#,
      16#55#, 16#AA#, 16#CC#, 16#33#, 16#F8#, 16#07#
   );

   --  Interesting Word64 values
   Max_Interesting_U64 : constant := 16;
   type Interesting_U64_Array is array (0 .. Max_Interesting_U64 - 1) of Word64;

   Interesting_U64 : constant Interesting_U64_Array := (
      0, 1, 16#7FFFFFFF#, 16#80000000#, 16#FFFFFFFF#,
      16#7FFFFFFFFFFFFFFF#, 16#8000000000000000#, 16#FFFFFFFFFFFFFFFF#,
      2, 255, 256, 65535, 65536, 16#DEADBEEF#,
      16#CAFEBABE#, 16#12345678#
   );

   ---------------------------------------------------------------------------
   --  Mutation Operations
   ---------------------------------------------------------------------------

   --  Mutate byte array
   procedure Mutate_Input (
      State     : in Out RNG_State;
      Input     : in Out Byte_Array;
      Input_Len : in Out Natural;
      Max_Size  : in     Natural;
      Mut_Type  : in     Mutation_Type
   ) with
      Global => null,
      Pre    => Max_Size <= Input'Length;

   --  Apply multiple mutations
   procedure Mutate_Multi (
      State       : in Out RNG_State;
      Input       : in Out Byte_Array;
      Input_Len   : in Out Natural;
      Max_Size    : in     Natural;
      Num_Mutates : in     Natural
   ) with
      Global => null,
      Pre    => Max_Size <= Input'Length;

   ---------------------------------------------------------------------------
   --  Fuzzing Session
   ---------------------------------------------------------------------------

   type Fuzzer_Session is private;

   --  Fuzzing statistics
   type Fuzz_Stats is record
      Iterations      : Natural;
      Unique_Crashes  : Natural;
      Unique_Hangs    : Natural;
      Coverage_Percent: Natural;
      Corpus_Size     : Natural;
      Exec_Speed      : Natural;  --  Executions per second
      Total_Time_Ms   : Natural;
   end record;

   --  Crash info
   type Crash_Info is record
      Input        : Byte_Array (0 .. 4095);
      Input_Size   : Natural;
      Crash_Type   : Natural;  --  0=crash, 1=hang, 2=assert
      Error_Msg    : Bounded_String;
      Coverage     : Natural;
   end record;

   Max_Crashes : constant := 256;
   type Crash_Index is range 0 .. Max_Crashes - 1;
   type Crash_Array is array (Crash_Index) of Crash_Info;

   --  Create fuzzer session
   function Create_Session (
      Config : Fuzzer_Config
   ) return Fuzzer_Session with
      Global => null;

   --  Set target contract
   procedure Set_Target (
      Session       : in Out Fuzzer_Session;
      Contract_Path : in     String;
      Selector      : in     Bytes4;
      Success       : out    Boolean
   ) with
      Global => null,
      Pre    => Contract_Path'Length <= 256;

   --  Run fuzzing campaign
   procedure Run_Fuzzing (
      Session : in Out Fuzzer_Session;
      Stats   : out    Fuzz_Stats
   ) with
      Global => null;

   --  Get fuzzing statistics
   function Get_Stats (
      Session : Fuzzer_Session
   ) return Fuzz_Stats with
      Global => null;

   --  Get crash reports
   procedure Get_Crashes (
      Session     : in  Fuzzer_Session;
      Crashes     : out Crash_Array;
      Crash_Count : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Corpus Management
   ---------------------------------------------------------------------------

   --  Corpus entry
   type Corpus_Entry is record
      Input       : Byte_Array (0 .. 4095);
      Input_Size  : Natural;
      Coverage    : Natural;
      Exec_Time   : Natural;
      Is_Favored  : Boolean;
   end record;

   Max_Corpus : constant := 1024;
   type Corpus_Index is range 0 .. Max_Corpus - 1;
   type Corpus_Array is array (Corpus_Index) of Corpus_Entry;

   --  Add input to corpus
   procedure Add_To_Corpus (
      Session : in Out Fuzzer_Session;
      Input   : in     Byte_Array;
      Size    : in     Natural;
      Success : out    Boolean
   ) with
      Global => null;

   --  Get corpus entry
   function Get_Corpus_Entry (
      Session : Fuzzer_Session;
      Index   : Corpus_Index
   ) return Corpus_Entry with
      Global => null;

   --  Select corpus entry for mutation
   procedure Select_From_Corpus (
      Session : in     Fuzzer_Session;
      State   : in Out RNG_State;
      Entry_Idx: out    Corpus_Index;
      Success : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Property Testing
   ---------------------------------------------------------------------------

   --  Property result
   type Property_Result is (
      Property_Holds,       --  Property satisfied
      Property_Violated,    --  Counterexample found
      Property_Error,       --  Error during check
      Property_Timeout      --  Check timed out
   );

   --  Property check result
   type Property_Check is record
      Result       : Property_Result;
      Iterations   : Natural;
      Counter_Ex   : Byte_Array (0 .. 255);
      Counter_Size : Natural;
      Message      : Bounded_String;
   end record;

   --  Test property with random inputs
   procedure Test_Property (
      Session    : in Out Fuzzer_Session;
      Iterations : in     Natural;
      Check      : out    Property_Check
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   --  Generate fuzz report
   procedure Generate_Report (
      Session : in     Fuzzer_Session;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 65536;

   --  Summary line
   function Summary_Line (
      Session : Fuzzer_Session
   ) return String with
      Global => null;

private

   type RNG_State is record
      State : Word64;
   end record;

   type Fuzzer_Session is record
      Config        : Fuzzer_Config;
      RNG           : RNG_State;
      Target_Path   : Bounded_String;
      Target_Sel    : Bytes4;
      Corpus        : Corpus_Array;
      Corpus_Size   : Natural;
      Crashes       : Crash_Array;
      Crash_Count   : Natural;
      Stats         : Fuzz_Stats;
      Is_Running    : Boolean;
      Has_Target    : Boolean;
   end record;

end Khepri_Fuzzer;
