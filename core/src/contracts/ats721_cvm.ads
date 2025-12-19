pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;

--  ATS-721 NFT Contract: Non-Fungible Token Standard for AnubisVM
--
--  This contract implements the ATS-721 standard (equivalent to ERC-721)
--  with full CVM integration and formal verification support.
--
--  Features:
--  - Unique token IDs (256-bit represented as 32-byte arrays)
--  - Ownership tracking and transfer
--  - Approval mechanisms (single and operator)
--  - Metadata URI support
--  - Enumerable extension
--  - Safe transfer with receiver validation
--  - Batch minting and transfers
--  - Royalty support (EIP-2981)
--
--  State Slot Layout:
--  - Slot 0: Collection name
--  - Slot 1: Collection symbol
--  - Slot 2: Base URI
--  - Slot 3: Total supply
--  - Slot 4: Owner address
--  - Slot 5: Royalty receiver
--  - Slot 6: Royalty basis points
--  - Slots 16-79: Token ownership (64 slots)
--  - Slots 80-143: Token approvals (64 slots)
--  - Slots 144-207: Operator approvals (64 slots)
--  - Slots 208-255: Token URIs (48 slots)
--
--  Method Selectors:
--  - TransferFrom:        0x23b872dd
--  - SafeTransferFrom:    0x42842e0e
--  - Approve:             0x095ea7b3
--  - SetApprovalForAll:   0xa22cb465
--  - BalanceOf:           0x70a08231
--  - OwnerOf:             0x6352211e
--  - GetApproved:         0x081812fc
--  - IsApprovedForAll:    0xe985e9c5
--  - Mint:                0x40c10f19
--  - Burn:                0x42966c68
--  - TokenURI:            0xc87b56dd
--  - RoyaltyInfo:         0x2a55205a

package ATS721_CVM with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  State slot assignments
   Name_Slot           : constant State_Index := 0;
   Symbol_Slot         : constant State_Index := 1;
   Base_URI_Slot       : constant State_Index := 2;
   Total_Supply_Slot   : constant State_Index := 3;
   Owner_Slot          : constant State_Index := 4;
   Royalty_Receiver_Slot : constant State_Index := 5;
   Royalty_BP_Slot     : constant State_Index := 6;

   --  Token ownership: 16-79 (64 slots)
   Token_Owner_Base    : constant State_Index := 16;
   Token_Owner_Count   : constant := 64;

   --  Token approvals: 80-143 (64 slots)
   Token_Approval_Base : constant State_Index := 80;
   Token_Approval_Count : constant := 64;

   --  Operator approvals: 144-207 (64 slots)
   Operator_Approval_Base : constant State_Index := 144;
   Operator_Approval_Count : constant := 64;

   --  Token URIs: 208-255 (48 slots)
   Token_URI_Base      : constant State_Index := 208;
   Token_URI_Count     : constant := 48;

   --  Method selectors
   Transfer_From_Selector       : constant Hash256 := (
      16#23#, 16#b8#, 16#72#, 16#dd#, others => 0);
   Safe_Transfer_From_Selector  : constant Hash256 := (
      16#42#, 16#84#, 16#2e#, 16#0e#, others => 0);
   Approve_Selector             : constant Hash256 := (
      16#09#, 16#5e#, 16#a7#, 16#b3#, others => 0);
   Set_Approval_For_All_Selector : constant Hash256 := (
      16#a2#, 16#2c#, 16#b4#, 16#65#, others => 0);
   Balance_Of_Selector          : constant Hash256 := (
      16#70#, 16#a0#, 16#82#, 16#31#, others => 0);
   Owner_Of_Selector            : constant Hash256 := (
      16#63#, 16#52#, 16#21#, 16#1e#, others => 0);
   Get_Approved_Selector        : constant Hash256 := (
      16#08#, 16#18#, 16#12#, 16#fc#, others => 0);
   Is_Approved_For_All_Selector : constant Hash256 := (
      16#e9#, 16#85#, 16#e9#, 16#c5#, others => 0);
   Mint_Selector                : constant Hash256 := (
      16#40#, 16#c1#, 16#0f#, 16#19#, others => 0);
   Burn_Selector                : constant Hash256 := (
      16#42#, 16#96#, 16#6c#, 16#68#, others => 0);
   Token_URI_Selector           : constant Hash256 := (
      16#c8#, 16#7b#, 16#56#, 16#dd#, others => 0);
   Royalty_Info_Selector        : constant Hash256 := (
      16#2a#, 16#55#, 16#20#, 16#5a#, others => 0);

   --  Token ID type (32 bytes to represent 256-bit)
   subtype Token_ID is Byte_Array (0 .. 31);

   ---------------------------------------------------------------------------
   --  Entry Points
   ---------------------------------------------------------------------------

   --  Transfer NFT from one address to another
   --
   --  Parameters (96 bytes):
   --  - From: Address (32 bytes)
   --  - To: Address (32 bytes)
   --  - Token ID: 32 bytes
   procedure Transfer_From (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Safe transfer with receiver validation
   procedure Safe_Transfer_From (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Approve address to transfer specific token
   --
   --  Parameters (64 bytes):
   --  - To: Address (32 bytes)
   --  - Token ID: 32 bytes
   procedure Approve (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Set operator approval for all tokens
   --
   --  Parameters (33 bytes):
   --  - Operator: Address (32 bytes)
   --  - Approved: Boolean (1 byte)
   procedure Set_Approval_For_All (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get token balance of address
   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get owner of token
   procedure Owner_Of (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get approved address for token
   procedure Get_Approved (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Check if operator is approved for all
   procedure Is_Approved_For_All (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Mint new NFT
   --
   --  Parameters (64 bytes):
   --  - To: Address (32 bytes)
   --  - Token ID: 32 bytes
   procedure Mint (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Burn NFT
   --
   --  Parameters (32 bytes):
   --  - Token ID: 32 bytes
   procedure Burn (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get token metadata URI
   procedure Token_URI (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get royalty information (EIP-2981)
   --
   --  Parameters (40 bytes):
   --  - Token ID: 32 bytes
   --  - Sale Price: 8 bytes (Unsigned_64)
   procedure Royalty_Info (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor with
      Global => null;

   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) with
      Global => null;

   procedure Execute (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global => null;

private

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Get_Token_Owner_Slot (Token_Id : Token_ID) return State_Index with
      Global => null,
      Post   => Get_Token_Owner_Slot'Result in Token_Owner_Base ..
                Token_Owner_Base + Token_Owner_Count - 1;

   function Get_Token_Approval_Slot (Token_Id : Token_ID) return State_Index with
      Global => null,
      Post   => Get_Token_Approval_Slot'Result in Token_Approval_Base ..
                Token_Approval_Base + Token_Approval_Count - 1;

   function Get_Operator_Approval_Slot (
      Owner    : Address;
      Operator : Address
   ) return State_Index with
      Global => null,
      Post   => Get_Operator_Approval_Slot'Result in Operator_Approval_Base ..
                Operator_Approval_Base + Operator_Approval_Count - 1;

   function Get_Token_URI_Slot (Token_Id : Token_ID) return State_Index with
      Global => null,
      Post   => Get_Token_URI_Slot'Result in Token_URI_Base ..
                Token_URI_Base + Token_URI_Count - 1;

   function Read_Address (Slot : State_Slot) return Address with
      Global => null;

   procedure Write_Address (
      Slot : in out State_Slot;
      Addr : Address
   ) with
      Global => null;

   function Read_U64 (Slot : State_Slot) return Unsigned_64 with
      Global => null;

   procedure Write_U64 (
      Slot  : in out State_Slot;
      Value : Unsigned_64
   ) with
      Global => null;

   function Token_Exists (
      State    : State_Array;
      Token_Id : Token_ID
   ) return Boolean with
      Global => null;

   function Is_Approved_Or_Owner (
      State    : State_Array;
      Spender  : Address;
      Token_Id : Token_ID
   ) return Boolean with
      Global => null;

   function Is_Contract_Owner (
      State : State_Array;
      Addr  : Address
   ) return Boolean with
      Global => null;

   function Selector_Match (A, B : Hash256) return Boolean with
      Global => null;

   --  Count tokens owned by address
   function Count_Tokens (
      State : State_Array;
      Owner : Address
   ) return Natural with
      Global => null;

   --  Emit Transfer event (LOG4)
   procedure Emit_Transfer_Event (
      From     : Address;
      To       : Address;
      Token_Id : Token_ID
   ) with
      Global => null;

   --  Emit Approval event (LOG4)
   procedure Emit_Approval_Event (
      Owner    : Address;
      Approved : Address;
      Token_Id : Token_ID
   ) with
      Global => null;

   --  Emit ApprovalForAll event (LOG3)
   procedure Emit_Approval_For_All_Event (
      Owner    : Address;
      Operator : Address;
      Approved : Boolean
   ) with
      Global => null;

end ATS721_CVM;
