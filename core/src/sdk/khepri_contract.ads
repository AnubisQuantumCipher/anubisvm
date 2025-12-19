pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;
with Khepri_Crypto; use Khepri_Crypto;
with Khepri_ABI; use Khepri_ABI;
with Khepri_Transaction; use Khepri_Transaction;
with Aegis_VM_Types; use Aegis_VM_Types;

--  KHEPRI Contract: High-Level Contract Interaction
--
--  This package provides high-level abstractions for interacting with
--  KHEPRI smart contracts, building on top of the ABI and Transaction
--  layers.
--
--  Key Features:
--  - Contract instance abstraction
--  - Type-safe function calls
--  - Event monitoring
--  - Deployment helpers
--  - Common contract patterns (ERC-20, ERC-721, etc.)
--
--  Usage Pattern:
--  1. Create contract instance with address and ABI
--  2. Build function calls using type-safe builders
--  3. Execute calls and parse results
--  4. Monitor events
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 9: Contract Interactions
--  - ERC-20: Token Standard
--  - ERC-721: NFT Standard

package Khepri_Contract with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Contract Configuration
   ---------------------------------------------------------------------------

   --  Maximum ABI entries per contract
   Max_ABI_Entries : constant := 128;

   --  Maximum contract name length
   Max_Contract_Name : constant := 64;

   ---------------------------------------------------------------------------
   --  Contract Instance
   ---------------------------------------------------------------------------

   --  Contract function descriptor
   type Function_Descriptor is record
      Name      : Bounded_String;
      Selector  : Bytes4;
      Is_View   : Boolean;   -- Read-only (doesn't modify state)
      Is_Payable : Boolean;  -- Accepts value
   end record;

   --  Contract ABI entry index
   subtype ABI_Entry_Index is Natural range 0 .. Max_ABI_Entries - 1;

   --  Contract ABI table
   type ABI_Table is array (ABI_Entry_Index) of Function_Descriptor;

   --  Contract instance
   type Contract is record
      Address      : Khepri_Types.Address;
      Name         : Bounded_String;
      ABI          : ABI_Table;
      ABI_Count    : Natural;
      Initialized  : Boolean;
   end record;

   --  Null contract
   Null_Contract : constant Contract := (
      Address      => Null_Address,
      Name         => Empty_String,
      ABI          => (others => (Empty_String, (others => 0), False, False)),
      ABI_Count    => 0,
      Initialized  => False
   );

   ---------------------------------------------------------------------------
   --  Contract Creation
   ---------------------------------------------------------------------------

   --  Create contract instance from address
   function New_Contract (
      Addr : Khepri_Types.Address;
      Name : Bounded_String
   ) return Contract with
      Global => null,
      Post   => New_Contract'Result.Initialized and
                Equal (New_Contract'Result.Address, Addr);

   --  Add function to contract ABI
   procedure Add_Function (
      C          : in out Contract;
      Name       : in     Bounded_String;
      Signature  : in     String;
      Is_View    : in     Boolean;
      Is_Payable : in     Boolean;
      Success    : out    Boolean
   ) with
      Global => null,
      Pre    => C.Initialized and
                Signature'Length > 0 and Signature'Length <= 256 and
                C.ABI_Count < Max_ABI_Entries,
      Post   => (if Success then C.ABI_Count = C.ABI_Count'Old + 1);

   --  Find function by name
   procedure Find_Function (
      C        : in  Contract;
      Name     : in  Bounded_String;
      Index    : out Natural;
      Found    : out Boolean
   ) with
      Global => null,
      Pre    => C.Initialized,
      Post   => (if Found then Index < C.ABI_Count);

   --  Find function by selector
   procedure Find_Function_By_Selector (
      C        : in  Contract;
      Selector : in  Bytes4;
      Index    : out Natural;
      Found    : out Boolean
   ) with
      Global => null,
      Pre    => C.Initialized,
      Post   => (if Found then Index < C.ABI_Count);

   ---------------------------------------------------------------------------
   --  Function Call Builder
   ---------------------------------------------------------------------------

   --  Call builder for constructing function calls
   type Call_Builder is private;

   --  Create new call builder
   function New_Call (
      C        : Contract;
      Func_Name : Bounded_String
   ) return Call_Builder with
      Global => null,
      Pre    => C.Initialized;

   --  Add uint256 argument
   function Add_Uint256_Arg (
      Builder : Call_Builder;
      Value   : Uint256
   ) return Call_Builder with
      Global => null;

   --  Add address argument
   function Add_Address_Arg (
      Builder : Call_Builder;
      Addr    : Khepri_Types.Address
   ) return Call_Builder with
      Global => null;

   --  Add bool argument
   function Add_Bool_Arg (
      Builder : Call_Builder;
      Value   : Boolean
   ) return Call_Builder with
      Global => null;

   --  Add bytes32 argument
   function Add_Bytes32_Arg (
      Builder : Call_Builder;
      Value   : Bytes32
   ) return Call_Builder with
      Global => null;

   --  Add string argument
   function Add_String_Arg (
      Builder : Call_Builder;
      Value   : Bounded_String
   ) return Call_Builder with
      Global => null;

   --  Set call value (for payable functions)
   function Set_Call_Value (
      Builder : Call_Builder;
      Value   : Wei
   ) return Call_Builder with
      Global => null;

   --  Set gas limit
   function Set_Call_Gas_Limit (
      Builder   : Call_Builder;
      Gas_Limit : Uint256
   ) return Call_Builder with
      Global => null;

   --  Build transaction from call
   procedure Build_Call (
      Builder   : in  Call_Builder;
      From      : in  Khepri_Types.Address;
      Nonce     : in  Uint256;
      Chain_ID  : in  Uint256;
      Gas_Price : in  Uint256;
      TX        : out Transaction;
      Success   : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Call Result Parsing
   ---------------------------------------------------------------------------

   --  Call result (return data)
   type Call_Result is record
      Success    : Boolean;
      Data       : Byte_Array (0 .. Max_ABI_Size - 1);
      Data_Size  : Natural;
      Gas_Used   : Uint256;
   end record;

   --  Empty result
   Empty_Result : constant Call_Result := (
      Success   => False,
      Data      => (others => 0),
      Data_Size => 0,
      Gas_Used  => Zero
   );

   --  Decode uint256 from result
   procedure Decode_Uint256_Result (
      Result  : in  Call_Result;
      Value   : out Uint256;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Result.Success;

   --  Decode address from result
   procedure Decode_Address_Result (
      Result  : in  Call_Result;
      Addr    : out Khepri_Types.Address;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Result.Success;

   --  Decode bool from result
   procedure Decode_Bool_Result (
      Result  : in  Call_Result;
      Value   : out Boolean;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Result.Success;

   ---------------------------------------------------------------------------
   --  ERC-20 Token Interface
   ---------------------------------------------------------------------------

   --  ERC-20 token contract helper
   type ERC20_Contract is record
      Base         : Contract;
      Total_Supply : Uint256;
      Decimals     : Natural;
   end record;

   --  Create ERC-20 contract instance
   function New_ERC20_Contract (
      Token_Address : Khepri_Types.Address;
      Name          : Bounded_String
   ) return ERC20_Contract with
      Global => null;

   --  Build transfer call: transfer(address,uint256)
   procedure Build_Transfer (
      Token     : in  ERC20_Contract;
      To        : in  Khepri_Types.Address;
      Amount    : in  Uint256;
      From      : in  Khepri_Types.Address;
      Nonce     : in  Uint256;
      Chain_ID  : in  Uint256;
      Gas_Price : in  Uint256;
      TX        : out Transaction;
      Success   : out Boolean
   ) with
      Global => null;

   --  Build approve call: approve(address,uint256)
   procedure Build_Approve (
      Token     : in  ERC20_Contract;
      Spender   : in  Khepri_Types.Address;
      Amount    : in  Uint256;
      From      : in  Khepri_Types.Address;
      Nonce     : in  Uint256;
      Chain_ID  : in  Uint256;
      Gas_Price : in  Uint256;
      TX        : out Transaction;
      Success   : out Boolean
   ) with
      Global => null;

   --  Build balanceOf call: balanceOf(address)
   procedure Build_BalanceOf (
      Token     : in  ERC20_Contract;
      Account   : in  Khepri_Types.Address;
      From      : in  Khepri_Types.Address;
      Nonce     : in  Uint256;
      Chain_ID  : in  Uint256;
      TX        : out Transaction;
      Success   : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Contract Deployment
   ---------------------------------------------------------------------------

   --  Deployment parameters
   type Deploy_Params is record
      Bytecode      : Byte_Array (0 .. Max_Tx_Data_Size - 1);
      Bytecode_Size : Natural;
      Constructor_Args : Byte_Array (0 .. Max_ABI_Size - 1);
      Args_Size     : Natural;
      Value         : Wei;
      Gas_Limit     : Uint256;
   end record;

   --  Empty deploy params
   Empty_Deploy_Params : constant Deploy_Params := (
      Bytecode      => (others => 0),
      Bytecode_Size => 0,
      Constructor_Args => (others => 0),
      Args_Size     => 0,
      Value         => Zero,
      Gas_Limit     => From_Word64 (1_000_000)
   );

   --  Build deployment transaction
   procedure Build_Deploy (
      Params    : in  Deploy_Params;
      From      : in  Khepri_Types.Address;
      Nonce     : in  Uint256;
      Chain_ID  : in  Uint256;
      Gas_Price : in  Uint256;
      TX        : out Transaction;
      Success   : out Boolean
   ) with
      Global => null,
      Pre    => Params.Bytecode_Size <= Max_Tx_Data_Size and
                Params.Args_Size <= Max_ABI_Size;

   --  Calculate deployment address (CREATE2 style)
   function Calculate_Deploy_Address (
      Deployer : Khepri_Types.Address;
      Salt     : Bytes32;
      Bytecode_Hash : Bytes32
   ) return Khepri_Types.Address with
      Global => null;

   ---------------------------------------------------------------------------
   --  Event Monitoring
   ---------------------------------------------------------------------------

   --  Event filter
   type Event_Filter is record
      Contract_Addr : Khepri_Types.Address;
      Event_Sig     : Bytes32;
      From_Block    : Uint256;
      To_Block      : Uint256;
      Active        : Boolean;
   end record;

   --  Null filter
   Null_Filter : constant Event_Filter := (
      Contract_Addr => Null_Address,
      Event_Sig     => Bytes32_Zero,
      From_Block    => Zero,
      To_Block      => Max,
      Active        => False
   );

   --  Create event filter
   function New_Event_Filter (
      Contract_Addr : Khepri_Types.Address;
      Event_Name    : String
   ) return Event_Filter with
      Global => null,
      Pre    => Event_Name'Length > 0 and Event_Name'Length <= 256;

   --  Decoded event
   type Decoded_Event is record
      Contract_Addr : Khepri_Types.Address;
      Event_Sig     : Bytes32;
      Topics        : Event_Topics;
      Topic_Count   : Natural;
      Data          : Byte_Array (0 .. Max_Event_Data - 1);
      Data_Size     : Natural;
      Block_Number  : Uint256;
      TX_Hash       : Bytes32;
      Valid         : Boolean;
   end record;

   --  Empty event
   Empty_Decoded_Event : constant Decoded_Event := (
      Contract_Addr => Null_Address,
      Event_Sig     => Bytes32_Zero,
      Topics        => (others => Bytes32_Zero),
      Topic_Count   => 0,
      Data          => (others => 0),
      Data_Size     => 0,
      Block_Number  => Zero,
      TX_Hash       => Bytes32_Zero,
      Valid         => False
   );

   ---------------------------------------------------------------------------
   --  Common Contract Patterns
   ---------------------------------------------------------------------------

   --  Ownable pattern: check ownership
   function Is_Owner (
      C     : Contract;
      Addr  : Khepri_Types.Address
   ) return Boolean with
      SPARK_Mode => Off;

   --  Pausable pattern: check if paused
   function Is_Paused (C : Contract) return Boolean with
      SPARK_Mode => Off;

   --  Access control: check role
   function Has_Role (
      C    : Contract;
      Role : Bytes32;
      Addr : Khepri_Types.Address
   ) return Boolean with
      SPARK_Mode => Off;

private

   --  Call builder state
   type Call_Builder is record
      Contract_Addr : Khepri_Types.Address;
      Selector      : Bytes4;
      Encoder       : ABI_Encoder;
      Value         : Wei;
      Gas_Limit     : Uint256;
      Valid         : Boolean;
   end record;

   --  Empty call builder
   Empty_Call_Builder : constant Call_Builder := (
      Contract_Addr => Null_Address,
      Selector      => (others => 0),
      Encoder       => New_Encoder,
      Value         => Zero,
      Gas_Limit     => From_Word64 (100_000),
      Valid         => False
   );

end Khepri_Contract;
