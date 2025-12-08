pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;

--  KHEPRI Coverage: Code Coverage Analysis for Smart Contracts
--
--  This package provides coverage analysis for KHEPRI contracts:
--  - Instruction-level coverage tracking
--  - Branch coverage analysis
--  - Path coverage analysis
--  - Coverage-guided feedback for fuzzing
--
--  Coverage Types:
--  - Line coverage: Which lines were executed
--  - Branch coverage: Which branches were taken
--  - Path coverage: Which execution paths were followed
--  - Opcode coverage: Which VM opcodes were executed
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.4: Coverage Analysis

package Khepri_Coverage with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Coverage Types
   ---------------------------------------------------------------------------

   --  Coverage granularity
   type Coverage_Type is (
      Coverage_Line,        --  Line-level coverage
      Coverage_Branch,      --  Branch coverage
      Coverage_Path,        --  Path coverage
      Coverage_Opcode       --  Opcode-level coverage
   );

   --  Coverage entry state
   type Coverage_State is (
      State_Not_Covered,    --  Never executed
      State_Covered,        --  Executed at least once
      State_Hot,            --  Executed frequently
      State_Cold            --  Executed rarely
   );

   ---------------------------------------------------------------------------
   --  Coverage Map
   ---------------------------------------------------------------------------

   --  Maximum coverage points per contract
   Max_Coverage_Points : constant := 4096;
   type Coverage_Index is range 0 .. Max_Coverage_Points - 1;

   --  Coverage point information
   type Coverage_Point is record
      PC           : Natural;      --  Program counter / offset
      Point_Type   : Coverage_Type;
      Hit_Count    : Natural;
      State        : Coverage_State;
      Is_Branch    : Boolean;
      Branch_Taken : Boolean;      --  For branch points
   end record;

   type Coverage_Point_Array is array (Coverage_Index) of Coverage_Point;

   --  Coverage map for a contract
   type Coverage_Map is record
      Points      : Coverage_Point_Array;
      Point_Count : Natural;
      Total_Hits  : Natural;
      Total_Lines : Natural;
      Total_Branches : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Branch Coverage
   ---------------------------------------------------------------------------

   --  Maximum branches per contract
   Max_Branches : constant := 1024;
   type Branch_Index is range 0 .. Max_Branches - 1;

   --  Branch information
   type Branch_Info is record
      PC            : Natural;        --  Branch instruction location
      Target_True   : Natural;        --  Target if condition true
      Target_False  : Natural;        --  Target if condition false
      Taken_True    : Boolean;        --  Was true branch taken?
      Taken_False   : Boolean;        --  Was false branch taken?
      Hit_Count_T   : Natural;        --  True branch hit count
      Hit_Count_F   : Natural;        --  False branch hit count
   end record;

   type Branch_Array is array (Branch_Index) of Branch_Info;

   --  Branch coverage map
   type Branch_Map is record
      Branches     : Branch_Array;
      Branch_Count : Natural;
      Covered_Both : Natural;  --  Branches with both paths covered
      Covered_One  : Natural;  --  Branches with one path covered
      Not_Covered  : Natural;  --  Branches not covered
   end record;

   ---------------------------------------------------------------------------
   --  Path Coverage
   ---------------------------------------------------------------------------

   --  Path representation (simplified as hash)
   subtype Path_Hash is Hash256;

   --  Maximum unique paths to track
   Max_Paths : constant := 512;
   type Path_Index is range 0 .. Max_Paths - 1;

   --  Path information
   type Path_Info is record
      Hash      : Path_Hash;
      Hit_Count : Natural;
      Length    : Natural;    --  Number of branches in path
      Is_Hot    : Boolean;
   end record;

   type Path_Array is array (Path_Index) of Path_Info;

   --  Path coverage map
   type Path_Map is record
      Paths       : Path_Array;
      Path_Count  : Natural;
      Total_Paths : Natural;  --  Theoretical total paths
   end record;

   ---------------------------------------------------------------------------
   --  Coverage Session
   ---------------------------------------------------------------------------

   type Coverage_Session is private;

   --  Coverage statistics
   type Coverage_Stats is record
      Line_Coverage    : Natural;  --  Percentage 0-100
      Branch_Coverage  : Natural;  --  Percentage 0-100
      Path_Coverage    : Natural;  --  Percentage 0-100
      Opcode_Coverage  : Natural;  --  Percentage 0-100
      Total_Points     : Natural;
      Covered_Points   : Natural;
      Hot_Points       : Natural;
      Cold_Points      : Natural;
   end record;

   --  Initialize coverage session
   function Create_Session return Coverage_Session with
      Global => null;

   --  Set target contract for coverage
   procedure Set_Target (
      Session       : in out Coverage_Session;
      Contract_Code : in     Byte_Array;
      Code_Size     : in     Natural;
      Success       : out    Boolean
   ) with
      Global => null;

   --  Start coverage collection
   procedure Start_Collection (
      Session : in Out Coverage_Session
   ) with
      Global => null;

   --  Stop coverage collection
   procedure Stop_Collection (
      Session : in Out Coverage_Session
   ) with
      Global => null;

   --  Reset coverage data
   procedure Reset_Coverage (
      Session : in Out Coverage_Session
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Coverage Recording
   ---------------------------------------------------------------------------

   --  Record instruction execution
   procedure Record_Instruction (
      Session : in Out Coverage_Session;
      PC      : in     Natural
   ) with
      Global => null;

   --  Record branch execution
   procedure Record_Branch (
      Session    : in Out Coverage_Session;
      PC         : in     Natural;
      Taken      : in     Boolean;
      Target     : in     Natural
   ) with
      Global => null;

   --  Record path hash
   procedure Record_Path (
      Session : in Out Coverage_Session;
      Hash    : in     Path_Hash
   ) with
      Global => null;

   --  Record opcode execution
   procedure Record_Opcode (
      Session : in Out Coverage_Session;
      Opcode  : in     Byte
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Coverage Queries
   ---------------------------------------------------------------------------

   --  Get coverage statistics
   function Get_Stats (
      Session : Coverage_Session
   ) return Coverage_Stats with
      Global => null;

   --  Get line coverage map
   procedure Get_Line_Coverage (
      Session  : in  Coverage_Session;
      Map      : out Coverage_Map;
      Success  : out Boolean
   ) with
      Global => null;

   --  Get branch coverage map
   procedure Get_Branch_Coverage (
      Session : in  Coverage_Session;
      Map     : out Branch_Map;
      Success : out Boolean
   ) with
      Global => null;

   --  Get path coverage map
   procedure Get_Path_Coverage (
      Session : in  Coverage_Session;
      Map     : out Path_Map;
      Success : out Boolean
   ) with
      Global => null;

   --  Check if specific PC is covered
   function Is_Covered (
      Session : Coverage_Session;
      PC      : Natural
   ) return Boolean with
      Global => null;

   --  Get hit count for PC
   function Get_Hit_Count (
      Session : Coverage_Session;
      PC      : Natural
   ) return Natural with
      Global => null;

   ---------------------------------------------------------------------------
   --  Coverage Analysis
   ---------------------------------------------------------------------------

   --  Find uncovered regions
   procedure Find_Uncovered (
      Session   : in  Coverage_Session;
      Uncovered : out Coverage_Point_Array;
      Count     : out Natural
   ) with
      Global => null;

   --  Find hot spots (frequently executed)
   procedure Find_Hot_Spots (
      Session  : in  Coverage_Session;
      Hot      : out Coverage_Point_Array;
      Count    : out Natural;
      Min_Hits : in  Natural := 100
   ) with
      Global => null;

   --  Find cold spots (rarely executed)
   procedure Find_Cold_Spots (
      Session  : in  Coverage_Session;
      Cold     : out Coverage_Point_Array;
      Count    : out Natural;
      Max_Hits : in  Natural := 5
   ) with
      Global => null;

   --  Calculate coverage delta (new coverage from test)
   function Coverage_Delta (
      Before : Coverage_Stats;
      After  : Coverage_Stats
   ) return Coverage_Stats with
      Global => null;

   ---------------------------------------------------------------------------
   --  Coverage-Guided Feedback
   ---------------------------------------------------------------------------

   --  Feedback for coverage-guided fuzzing
   type Coverage_Feedback is record
      New_Coverage    : Boolean;     --  Found new coverage
      New_Branches    : Natural;     --  Number of new branches
      New_Paths       : Natural;     --  Number of new paths
      Interest_Score  : Natural;     --  0-100 interest score
      Should_Save     : Boolean;     --  Input should be saved to corpus
   end record;

   --  Calculate feedback from coverage
   function Calculate_Feedback (
      Session : Coverage_Session;
      Prev_Stats : Coverage_Stats
   ) return Coverage_Feedback with
      Global => null;

   --  Merge coverage from another session
   procedure Merge_Coverage (
      Target : in Out Coverage_Session;
      Source : in     Coverage_Session
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Opcode Coverage
   ---------------------------------------------------------------------------

   --  Maximum opcodes (256 possible byte values)
   Max_Opcodes : constant := 256;
   type Opcode_Index is range 0 .. Max_Opcodes - 1;

   type Opcode_Hit_Array is array (Opcode_Index) of Natural;

   --  Opcode coverage information
   type Opcode_Coverage is record
      Hits         : Opcode_Hit_Array;
      Covered      : Natural;  --  Number of unique opcodes hit
      Total_Valid  : Natural;  --  Total valid opcodes in instruction set
   end record;

   --  Get opcode coverage
   function Get_Opcode_Coverage (
      Session : Coverage_Session
   ) return Opcode_Coverage with
      Global => null;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   --  Report format
   type Coverage_Report_Format is (
      Format_Text,          --  Plain text
      Format_HTML,          --  HTML with highlighting
      Format_LCOV,          --  LCOV format
      Format_JSON           --  JSON format
   );

   --  Generate coverage report
   procedure Generate_Report (
      Session : in     Coverage_Session;
      Format  : in     Coverage_Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 65536;

   --  Summary line for display
   function Summary_Line (
      Session : Coverage_Session
   ) return String with
      Global => null;

private

   --  Execution trace for path coverage
   Max_Trace_Length : constant := 256;
   type Trace_Index is range 0 .. Max_Trace_Length - 1;
   type Trace_Array is array (Trace_Index) of Natural;

   type Execution_Trace is record
      Branches   : Trace_Array;
      Length     : Natural;
   end record;

   type Coverage_Session is record
      --  Target contract
      Has_Target   : Boolean;
      Code_Size    : Natural;

      --  Collection state
      Is_Collecting : Boolean;

      --  Coverage maps
      Line_Map     : Coverage_Map;
      Branch_Info  : Branch_Map;
      Path_Info    : Path_Map;
      Opcode_Hits  : Opcode_Hit_Array;

      --  Current execution trace
      Current_Trace : Execution_Trace;

      --  Statistics
      Stats        : Coverage_Stats;
   end record;

end Khepri_Coverage;
