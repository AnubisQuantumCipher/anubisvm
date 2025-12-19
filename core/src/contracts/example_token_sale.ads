pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;

--  Example: Token Sale Contract
--
--  Demonstrates integration of multiple tokenomics features:
--  - Uses ATS-20 for payment token
--  - Mints ATS-721 NFTs as receipts
--  - Implements access control
--  - Pausable during emergencies
--  - Time-locked sale periods
--  - Whitelist support
--  - Refund mechanism
--
--  This is an educational example showing how to build complex
--  DeFi contracts using AnubisVM's tokenomics primitives.
--
--  Sale Mechanics:
--  - Users deposit payment tokens (e.g., ANUBIS)
--  - Contract mints NFT receipt to user
--  - NFT metadata records purchase details
--  - Owner can withdraw proceeds
--  - Users can claim refund if sale fails
--
--  State Slots:
--  - 0: Owner address
--  - 1: Payment token address
--  - 2: NFT contract address
--  - 3: Sale price per NFT
--  - 4: Sale start time
--  - 5: Sale end time
--  - 6: Max supply
--  - 7: Total sold
--  - 8: Paused flag
--  - 9: Sale succeeded flag
--  - 16-79: Whitelist (64 slots)
--  - 80-143: Purchases (64 slots)

package Example_Token_Sale with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  State slots
   Owner_Slot          : constant State_Index := 0;
   Payment_Token_Slot  : constant State_Index := 1;
   NFT_Contract_Slot   : constant State_Index := 2;
   Price_Slot          : constant State_Index := 3;
   Start_Time_Slot     : constant State_Index := 4;
   End_Time_Slot       : constant State_Index := 5;
   Max_Supply_Slot     : constant State_Index := 6;
   Total_Sold_Slot     : constant State_Index := 7;
   Paused_Slot         : constant State_Index := 8;
   Success_Slot        : constant State_Index := 9;

   Whitelist_Base      : constant State_Index := 16;
   Whitelist_Count     : constant := 64;

   Purchase_Base       : constant State_Index := 80;
   Purchase_Count      : constant := 64;

   --  Method selectors
   Buy_Selector        : constant Hash256 := (
      16#a6#, 16#f2#, 16#ae#, 16#3a#, others => 0);
   Claim_Refund_Selector : constant Hash256 := (
      16#6a#, 16#1c#, 16#05#, 16#ae#, others => 0);
   Withdraw_Selector   : constant Hash256 := (
      16#3c#, 16#cd#, 16#8c#, 16#e#, others => 0);
   Add_To_Whitelist_Selector : constant Hash256 := (
      16#b5#, 16#29#, 16#e9#, 16#5b#, others => 0);
   Set_Success_Selector : constant Hash256 := (
      16#24#, 16#d5#, 16#73#, 16#f3#, others => 0);
   Pause_Selector      : constant Hash256 := (
      16#84#, 16#56#, 16#cb#, 16#59#, others => 0);

   ---------------------------------------------------------------------------
   --  Entry Points
   ---------------------------------------------------------------------------

   --  Buy NFT with payment tokens
   --
   --  Parameters (8 bytes):
   --  - Quantity: Unsigned_64 (number of NFTs to buy)
   --
   --  Process:
   --  1. Check sale is active (not paused, within time window)
   --  2. Check caller is whitelisted (if whitelist enabled)
   --  3. Check supply available
   --  4. Calculate total cost
   --  5. Transfer payment tokens from caller
   --  6. Mint NFT(s) to caller
   --  7. Record purchase for refunds
   procedure Buy (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Claim refund if sale failed
   --
   --  Parameters: None
   --
   --  Process:
   --  1. Check sale has ended
   --  2. Check sale failed (not marked successful)
   --  3. Look up caller's purchase amount
   --  4. Transfer payment tokens back to caller
   --  5. Clear purchase record
   procedure Claim_Refund (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Withdraw proceeds (owner only)
   --
   --  Parameters: None
   --
   --  Process:
   --  1. Check caller is owner
   --  2. Check sale succeeded
   --  3. Transfer all payment tokens to owner
   procedure Withdraw (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Add address to whitelist (owner only)
   --
   --  Parameters (32 bytes):
   --  - Address: 32 bytes
   procedure Add_To_Whitelist (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Mark sale as successful (owner only)
   --
   --  Parameters (1 byte):
   --  - Success: Boolean
   procedure Set_Success (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Pause sale (owner only)
   procedure Pause (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor with
      Global => null;

   --  Initialize token sale
   --
   --  Params (112 bytes):
   --  - Owner: Address (32 bytes)
   --  - Payment token: Address (32 bytes)
   --  - NFT contract: Address (32 bytes)
   --  - Price: Unsigned_64 (8 bytes)
   --  - Start time: Unsigned_64 (8 bytes)
   --  - End time: Unsigned_64 (8 bytes)
   --  - Max supply: Unsigned_64 (8 bytes)
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

   function Is_Owner (State : State_Array; Addr : Address) return Boolean with
      Global => null;

   function Is_Paused (State : State_Array) return Boolean with
      Global => null;

   function Is_Sale_Active (
      State       : State_Array;
      Current_Time : Unsigned_64
   ) return Boolean with
      Global => null;

   function Is_Whitelisted (State : State_Array; Addr : Address) return Boolean with
      Global => null;

   function Get_Whitelist_Slot (Addr : Address) return State_Index with
      Global => null,
      Post => Get_Whitelist_Slot'Result in Whitelist_Base ..
              Whitelist_Base + Whitelist_Count - 1;

   function Get_Purchase_Slot (Addr : Address) return State_Index with
      Global => null,
      Post => Get_Purchase_Slot'Result in Purchase_Base ..
              Purchase_Base + Purchase_Count - 1;

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

   function Selector_Match (A, B : Hash256) return Boolean with
      Global => null;

end Example_Token_Sale;
