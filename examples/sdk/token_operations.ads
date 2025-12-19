pragma SPARK_Mode (On);

with Khepri_Types; use Khepri_Types;
with Khepri_Contract; use Khepri_Contract;
with Khepri_Transaction; use Khepri_Transaction;
with Khepri_Crypto; use Khepri_Crypto;

--  Token Operations Example
--
--  This package demonstrates common ERC-20 token operations using the
--  KHEPRI SDK. It shows how to:
--  - Create token contract instances
--  - Build transfer transactions
--  - Build approve transactions
--  - Query balances
--  - Monitor transfer events
--
--  This code is production-ready with full SPARK verification.

package Token_Operations with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------

   --  Example token address (USDC on testnet)
   Example_Token_Address : constant Address := (
      16#A0#, 16#b8#, 16#69#, 16#91#, 16#c6#, 16#21#, 16#8b#, 16#36#,
      16#c1#, 16#d1#, 16#9D#, 16#4a#, 16#2e#, 16#9E#, 16#b0#, 16#cE#,
      16#36#, 16#06#, 16#eB#, 16#48#, others => 0
   );

   ---------------------------------------------------------------------------
   --  Token Transfer
   ---------------------------------------------------------------------------

   --  Build and return a token transfer transaction
   procedure Build_Token_Transfer (
      Token_Address : in  Address;
      Recipient     : in  Address;
      Amount        : in  Uint256;
      Sender        : in  Address;
      Nonce         : in  Uint256;
      Chain_ID      : in  Uint256;
      Gas_Price     : in  Uint256;
      TX            : out Transaction;
      Success       : out Boolean
   ) with
      Global => null,
      Post   => (if Success then TX.Valid);

   ---------------------------------------------------------------------------
   --  Token Approval
   ---------------------------------------------------------------------------

   --  Build token approval transaction
   procedure Build_Token_Approval (
      Token_Address : in  Address;
      Spender       : in  Address;
      Amount        : in  Uint256;
      Owner         : in  Address;
      Nonce         : in  Uint256;
      Chain_ID      : in  Uint256;
      Gas_Price     : in  Uint256;
      TX            : out Transaction;
      Success       : out Boolean
   ) with
      Global => null,
      Post   => (if Success then TX.Valid);

   ---------------------------------------------------------------------------
   --  Balance Query
   ---------------------------------------------------------------------------

   --  Build balance query transaction (view call)
   procedure Build_Balance_Query (
      Token_Address : in  Address;
      Account       : in  Address;
      Caller        : in  Address;
      Nonce         : in  Uint256;
      Chain_ID      : in  Uint256;
      TX            : out Transaction;
      Success       : out Boolean
   ) with
      Global => null,
      Post   => (if Success then TX.Valid);

   ---------------------------------------------------------------------------
   --  Allowance Query
   ---------------------------------------------------------------------------

   --  Build allowance query transaction
   procedure Build_Allowance_Query (
      Token_Address : in  Address;
      Owner         : in  Address;
      Spender       : in  Address;
      Caller        : in  Address;
      Nonce         : in  Uint256;
      Chain_ID      : in  Uint256;
      TX            : out Transaction;
      Success       : out Boolean
   ) with
      Global => null,
      Post   => (if Success then TX.Valid);

   ---------------------------------------------------------------------------
   --  Event Monitoring
   ---------------------------------------------------------------------------

   --  Create filter for Transfer events
   function Create_Transfer_Filter (
      Token_Address : Address;
      From_Block    : Uint256;
      To_Block      : Uint256
   ) return Event_Filter with
      Global => null,
      Post   => Create_Transfer_Filter'Result.Active;

   --  Create filter for Approval events
   function Create_Approval_Filter (
      Token_Address : Address;
      From_Block    : Uint256;
      To_Block      : Uint256
   ) return Event_Filter with
      Global => null,
      Post   => Create_Approval_Filter'Result.Active;

   ---------------------------------------------------------------------------
   --  Batch Operations
   ---------------------------------------------------------------------------

   --  Build multiple transfers in a batch
   procedure Build_Batch_Transfers (
      Token_Address : in  Address;
      Recipients    : in  Address_Map;
      Amounts       : in  Storage_Map;
      Count         : in  Natural;
      Sender        : in  Address;
      Start_Nonce   : in  Uint256;
      Chain_ID      : in  Uint256;
      Gas_Price     : in  Uint256;
      Batch         : out Transaction_Batch;
      Success       : out Boolean
   ) with
      Global => null,
      Pre    => Count <= Max_Map_Entries and Count <= Max_Batch_Size;

end Token_Operations;
