pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;

--  KHEPRI Inspector: Contract Inspection Toolchain
--
--  This package provides inspection capabilities for KHEPRI
--  smart contracts. It enables:
--  - Contract metadata retrieval
--  - Bytecode disassembly
--  - ABI extraction
--  - Storage layout inspection
--  - Event log parsing
--  - Call trace analysis
--
--  Inspection Operations:
--  1. Fetch contract from chain
--  2. Disassemble bytecode
--  3. Extract ABI and metadata
--  4. Analyze storage layout
--  5. Generate inspection report
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 7.4: Inspector

package Khepri_Inspector with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Contract Metadata
   ---------------------------------------------------------------------------

   --  Contract basic info
   type Contract_Info is record
      Address           : Khepri_Types.Address;
      Bytecode_Hash     : Hash256;
      Bytecode_Size     : Natural;
      Balance           : Uint256;
      Nonce             : Word64;
      Creator           : Khepri_Types.Address;
      Creation_Block    : Word64;
      Cert_Level        : Khepri_Certification.Certification_Level;
      Is_Proxy          : Boolean;
      Implementation    : Khepri_Types.Address;  --  If proxy
   end record;

   --  Contract version info
   type Version_Info is record
      Name              : Bounded_String;
      Version           : Bounded_String;
      Compiler_Version  : Bounded_String;
      SPARK_Mode        : Boolean;
      Optimization      : Natural;  --  0-3
   end record;

   ---------------------------------------------------------------------------
   --  ABI Types
   ---------------------------------------------------------------------------

   --  Parameter type
   type Param_Type is (
      Type_Uint256,
      Type_Int256,
      Type_Address,
      Type_Bool,
      Type_Bytes,
      Type_String,
      Type_Bytes32,
      Type_Array,
      Type_Tuple,
      Type_Unknown
   );

   --  Function parameter
   type ABI_Param is record
      Name       : Bounded_String;
      P_Type     : Param_Type;
      Indexed    : Boolean;  --  For events
   end record;

   Max_Params : constant := 16;
   type Param_Index is range 0 .. Max_Params - 1;
   type Param_Array is array (Param_Index) of ABI_Param;

   --  Function entry
   type ABI_Function is record
      Name         : Bounded_String;
      Selector     : Bytes4;
      Inputs       : Param_Array;
      Input_Count  : Natural;
      Outputs      : Param_Array;
      Output_Count : Natural;
      Mutability   : Natural;  --  0=pure, 1=view, 2=nonpayable, 3=payable
      Is_External  : Boolean;
   end record;

   --  Event entry
   type ABI_Event is record
      Name          : Bounded_String;
      Signature     : Hash256;
      Params        : Param_Array;
      Param_Count   : Natural;
      Is_Anonymous  : Boolean;
   end record;

   Max_Functions : constant := 128;
   type Function_Index is range 0 .. Max_Functions - 1;
   type Function_Array is array (Function_Index) of ABI_Function;

   Max_Events : constant := 64;
   type Event_Index is range 0 .. Max_Events - 1;
   type Event_Array is array (Event_Index) of ABI_Event;

   ---------------------------------------------------------------------------
   --  Disassembly Types
   ---------------------------------------------------------------------------

   --  Opcode information
   type Opcode_Info is record
      Offset     : Natural;
      Opcode     : Byte;
      Mnemonic   : Bounded_String;
      Operand    : Byte_Array (0 .. 31);
      Op_Size    : Natural;
      Gas_Cost   : Natural;
      Stack_In   : Natural;
      Stack_Out  : Natural;
   end record;

   Max_Instructions : constant := 65536;
   type Instruction_Index is range 0 .. Max_Instructions - 1;
   type Instruction_Array is array (Instruction_Index) of Opcode_Info;

   --  Disassembly result
   type Disassembly is record
      Instructions      : Instruction_Array;
      Instruction_Count : Natural;
      Jump_Targets      : Natural;
      Total_Gas         : Word64;
      Has_Errors        : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Storage Layout
   ---------------------------------------------------------------------------

   --  Storage slot info
   type Storage_Slot is record
      Slot        : Hash256;
      Offset      : Natural;  --  Offset within slot
      Size        : Natural;  --  Size in bytes
      Name        : Bounded_String;
      Type_Name   : Bounded_String;
      Value       : Uint256;
   end record;

   Max_Slots : constant := 256;
   type Slot_Index is range 0 .. Max_Slots - 1;
   type Slot_Array is array (Slot_Index) of Storage_Slot;

   --  Storage layout
   type Storage_Layout is record
      Slots      : Slot_Array;
      Slot_Count : Natural;
      Total_Size : Natural;  --  Total storage bytes used
   end record;

   ---------------------------------------------------------------------------
   --  Inspection Session
   ---------------------------------------------------------------------------

   type Inspection_Session is private;

   --  Inspector configuration
   type Inspector_Config is record
      --  Network
      RPC_Endpoint      : Bounded_String;
      Chain_ID          : Word64;

      --  Options
      Fetch_Storage     : Boolean;
      Disassemble       : Boolean;
      Extract_ABI       : Boolean;
      Parse_Events      : Boolean;

      --  Output
      Output_Format     : Natural;  --  0=text, 1=JSON, 2=HTML
   end record;

   Default_Inspector_Config : constant Inspector_Config := (
      RPC_Endpoint      => Empty_String,
      Chain_ID          => 1,
      Fetch_Storage     => True,
      Disassemble       => True,
      Extract_ABI       => True,
      Parse_Events      => True,
      Output_Format     => 0
   );

   --  Create inspection session
   function Create_Session (
      Config : Inspector_Config
   ) return Inspection_Session with
      Global => null;

   --  Inspect contract by address
   procedure Inspect_Contract (
      Session  : in Out Inspection_Session;
      Address  : in     Khepri_Types.Address;
      Success  : out    Boolean
   ) with
      Global => null;

   --  Inspect contract from bytecode
   procedure Inspect_Bytecode (
      Session  : in Out Inspection_Session;
      Bytecode : in     Byte_Array;
      Success  : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Metadata Retrieval
   ---------------------------------------------------------------------------

   --  Get contract info
   function Get_Contract_Info (
      Session : Inspection_Session
   ) return Contract_Info with
      Global => null;

   --  Get version info
   function Get_Version_Info (
      Session : Inspection_Session
   ) return Version_Info with
      Global => null;

   --  Get certification level
   function Get_Cert_Level (
      Session : Inspection_Session
   ) return Khepri_Certification.Certification_Level with
      Global => null;

   ---------------------------------------------------------------------------
   --  ABI Operations
   ---------------------------------------------------------------------------

   --  Get contract ABI
   procedure Get_ABI (
      Session        : in  Inspection_Session;
      Functions      : out Function_Array;
      Function_Count : out Natural;
      Events         : out Event_Array;
      Event_Count    : out Natural
   ) with
      Global => null;

   --  Get function by selector
   function Get_Function (
      Session  : Inspection_Session;
      Selector : Bytes4
   ) return ABI_Function with
      Global => null;

   --  Get event by signature
   function Get_Event (
      Session   : Inspection_Session;
      Signature : Hash256
   ) return ABI_Event with
      Global => null;

   --  Encode function call
   procedure Encode_Call (
      Func    : in     ABI_Function;
      Args    : in     Byte_Array;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 65536;

   --  Decode function result
   procedure Decode_Result (
      Func    : in     ABI_Function;
      Data    : in     Byte_Array;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 65536;

   ---------------------------------------------------------------------------
   --  Disassembly Operations
   ---------------------------------------------------------------------------

   --  Disassemble bytecode
   procedure Disassemble_Bytecode (
      Session : in     Inspection_Session;
      Result  : out    Disassembly;
      Success : out    Boolean
   ) with
      Global => null;

   --  Get instruction at offset
   function Get_Instruction (
      Session : Inspection_Session;
      Offset  : Natural
   ) return Opcode_Info with
      Global => null;

   --  Format disassembly as text
   procedure Format_Disassembly (
      Disasm  : in     Disassembly;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 1048576;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   --  Get storage layout
   procedure Get_Storage_Layout (
      Session : in     Inspection_Session;
      Layout  : out    Storage_Layout;
      Success : out    Boolean
   ) with
      Global => null;

   --  Read storage slot
   function Read_Slot (
      Session : Inspection_Session;
      Slot    : Hash256
   ) return Uint256 with
      Global => null;

   --  Get storage at key (for mappings)
   function Get_Mapping_Slot (
      Base_Slot : Hash256;
      Key       : Byte_Array
   ) return Hash256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Event Parsing
   ---------------------------------------------------------------------------

   --  Decoded event
   type Decoded_Event is record
      Event_Type    : ABI_Event;
      Block_Number  : Word64;
      Tx_Hash       : Hash256;
      Log_Index     : Natural;
      Data          : Byte_Array (0 .. 1023);
      Data_Size     : Natural;
   end record;

   Max_Decoded_Events : constant := 256;
   type Decoded_Event_Index is range 0 .. Max_Decoded_Events - 1;
   type Decoded_Event_Array is array (Decoded_Event_Index) of Decoded_Event;

   --  Parse event logs
   procedure Parse_Events (
      Session     : in     Inspection_Session;
      From_Block  : in     Word64;
      To_Block    : in     Word64;
      Events      : out    Decoded_Event_Array;
      Event_Count : out    Natural;
      Success     : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Call Analysis
   ---------------------------------------------------------------------------

   --  Call trace entry
   type Call_Trace is record
      Depth        : Natural;
      Call_Type    : Natural;  --  0=call, 1=staticcall, 2=delegatecall
      From_Addr    : Khepri_Types.Address;
      To_Addr      : Khepri_Types.Address;
      Selector     : Bytes4;
      Gas_Used     : Word64;
      Success_Flag : Boolean;
   end record;

   Max_Traces : constant := 256;
   type Trace_Index is range 0 .. Max_Traces - 1;
   type Trace_Array is array (Trace_Index) of Call_Trace;

   --  Trace function call
   procedure Trace_Call (
      Session     : in     Inspection_Session;
      Tx_Hash     : in     Hash256;
      Traces      : out    Trace_Array;
      Trace_Count : out    Natural;
      Success     : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   --  Report format
   type Report_Format is (
      Format_Text,
      Format_JSON,
      Format_HTML,
      Format_Markdown
   );

   --  Generate inspection report
   procedure Generate_Report (
      Session : in     Inspection_Session;
      Format  : in     Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 1048576;

   --  Generate summary
   function Summary_Line (
      Session : Inspection_Session
   ) return String with
      Global => null;

private

   type Inspection_Session is record
      Config         : Inspector_Config;
      Contract       : Contract_Info;
      Version        : Version_Info;
      Functions      : Function_Array;
      Function_Count : Natural;
      Events         : Event_Array;
      Event_Count    : Natural;
      Bytecode       : Byte_Array (0 .. 1048575);
      Bytecode_Size  : Natural;
      Is_Inspected   : Boolean;
   end record;

end Khepri_Inspector;
