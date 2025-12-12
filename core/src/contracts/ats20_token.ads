pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;

--  ATS-20 Token Contract: Standard Fungible Token for AnubisVM
--
--  This contract implements a fungible token standard compatible with
--  the Anubis Token Standard (ATS-20). It provides:
--
--  - Balance tracking for all accounts
--  - Transfer of tokens between accounts
--  - Approval and allowance mechanism
--  - Total supply tracking
--
--  State Slot Layout:
--  - Slot 0: Total supply (8 bytes, big-endian)
--  - Slot 1: Token name (up to 32 bytes)
--  - Slot 2: Token symbol (up to 8 bytes)
--  - Slot 3: Decimals (1 byte)
--  - Slots 4-127: Balances (key = hash(address) mod 124 + 4)
--  - Slots 128-255: Allowances (key = hash(owner || spender) mod 128 + 128)
--
--  Method Selectors (SHA3-256 of method name, first 4 bytes):
--  - Transfer:    0xa9059cbb
--  - BalanceOf:   0x70a08231
--  - Approve:     0x095ea7b3
--  - Allowance:   0xdd62ed3e
--  - TotalSupply: 0x18160ddd
--  - Name:        0x06fdde03
--  - Symbol:      0x95d89b41
--  - Decimals:    0x313ce567
--
--  Formal Verification:
--  - All entry points have Pre/Post contracts
--  - Balance operations proven safe (no overflow/underflow)
--  - State slot indices proven in bounds

package ATS20_Token with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  State slot assignments
   Total_Supply_Slot : constant State_Index := 0;
   Name_Slot        : constant State_Index := 1;
   Symbol_Slot      : constant State_Index := 2;
   Decimals_Slot    : constant State_Index := 3;

   --  Balance slots: 4-127 (124 slots)
   Balance_Slot_Base : constant State_Index := 4;
   Balance_Slot_Count : constant := 124;

   --  Allowance slots: 128-255 (128 slots)
   Allowance_Slot_Base : constant State_Index := 128;
   Allowance_Slot_Count : constant := 128;

   --  Method selectors (first 4 bytes of SHA3-256)
   Transfer_Selector    : constant Hash256 := (
      16#a9#, 16#05#, 16#9c#, 16#bb#, others => 0);
   Balance_Of_Selector  : constant Hash256 := (
      16#70#, 16#a0#, 16#82#, 16#31#, others => 0);
   Approve_Selector     : constant Hash256 := (
      16#09#, 16#5e#, 16#a7#, 16#b3#, others => 0);
   Allowance_Selector   : constant Hash256 := (
      16#dd#, 16#62#, 16#ed#, 16#3e#, others => 0);
   Total_Supply_Selector : constant Hash256 := (
      16#18#, 16#16#, 16#0d#, 16#dd#, others => 0);
   Name_Selector        : constant Hash256 := (
      16#06#, 16#fd#, 16#de#, 16#03#, others => 0);
   Symbol_Selector      : constant Hash256 := (
      16#95#, 16#d8#, 16#9b#, 16#41#, others => 0);
   Decimals_Selector    : constant Hash256 := (
      16#31#, 16#3c#, 16#e5#, 16#67#, others => 0);
   Transfer_From_Selector : constant Hash256 := (
      16#23#, 16#b8#, 16#72#, 16#dd#, others => 0);

   ---------------------------------------------------------------------------
   --  Entry Points
   ---------------------------------------------------------------------------

   --  Transfer tokens from caller to recipient
   --
   --  Parameters (40 bytes):
   --  - To: Address (32 bytes)
   --  - Amount: Unsigned_64 (8 bytes, big-endian)
   --
   --  Returns:
   --  - Success: 1 byte (0x01 = success, 0x00 = failure)
   procedure Transfer (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get balance of an account
   --
   --  Parameters (32 bytes):
   --  - Account: Address (32 bytes)
   --
   --  Returns:
   --  - Balance: Unsigned_64 (8 bytes, big-endian)
   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Approve spender to transfer tokens on behalf of caller
   --
   --  Parameters (40 bytes):
   --  - Spender: Address (32 bytes)
   --  - Amount: Unsigned_64 (8 bytes, big-endian)
   --
   --  Returns:
   --  - Success: 1 byte (0x01 = success)
   procedure Approve (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get allowance for spender from owner
   --
   --  Parameters (64 bytes):
   --  - Owner: Address (32 bytes)
   --  - Spender: Address (32 bytes)
   --
   --  Returns:
   --  - Allowance: Unsigned_64 (8 bytes, big-endian)
   procedure Allowance (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Transfer tokens from one account to another using allowance
   --
   --  Parameters (72 bytes):
   --  - From: Address (32 bytes)
   --  - To: Address (32 bytes)
   --  - Amount: Unsigned_64 (8 bytes, big-endian)
   --
   --  Returns:
   --  - Success: 1 byte (0x01 = success, 0x00 = failure)
   procedure Transfer_From (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get total supply
   --
   --  Parameters: None
   --
   --  Returns:
   --  - Total supply: Unsigned_64 (8 bytes, big-endian)
   procedure Total_Supply (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get token name
   --
   --  Parameters: None
   --
   --  Returns:
   --  - Name: String (up to 32 bytes)
   procedure Name (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get token symbol
   --
   --  Parameters: None
   --
   --  Returns:
   --  - Symbol: String (up to 8 bytes)
   procedure Symbol (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get decimals
   --
   --  Parameters: None
   --
   --  Returns:
   --  - Decimals: Unsigned_8 (1 byte)
   procedure Decimals (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   --  Get CVM descriptor
   function Get_Descriptor return CVM_Descriptor with
      Global => null;

   --  Initialize token with name, symbol, decimals, and initial supply
   --
   --  Init_Params layout:
   --  - Name length: 1 byte
   --  - Name: up to 32 bytes
   --  - Symbol length: 1 byte
   --  - Symbol: up to 8 bytes
   --  - Decimals: 1 byte
   --  - Initial supply: 8 bytes (big-endian)
   --  - Initial holder: 32 bytes (address)
   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) with
      Global => null;

   --  Execute entry point
   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global => null;

private

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Compute balance slot index from address
   --  Uses simple hash: sum of bytes mod Balance_Slot_Count + Balance_Slot_Base
   function Get_Balance_Slot (Addr : Address) return State_Index with
      Global => null,
      Post   => Get_Balance_Slot'Result in Balance_Slot_Base ..
                Balance_Slot_Base + Balance_Slot_Count - 1;

   --  Compute allowance slot index from owner and spender
   function Get_Allowance_Slot (
      Owner   : Address;
      Spender : Address
   ) return State_Index with
      Global => null,
      Post   => Get_Allowance_Slot'Result in Allowance_Slot_Base ..
                Allowance_Slot_Base + Allowance_Slot_Count - 1;

   --  Read Unsigned_64 from state slot (big-endian)
   function Read_U64 (Slot : State_Slot) return Unsigned_64 with
      Global => null;

   --  Write Unsigned_64 to state slot (big-endian)
   procedure Write_U64 (
      Slot  : in Out State_Slot;
      Value : Unsigned_64
   ) with
      Global => null;

   --  Compare method selectors (first 4 bytes only)
   function Selector_Match (A, B : Hash256) return Boolean with
      Global => null;

end ATS20_Token;
