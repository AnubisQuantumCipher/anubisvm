-------------------------------------------------------------------------------
--  ANUBIS Token Core Implementation
--  Native Token of the AnubisVM/KHEPRI Ecosystem
--
--  Symbol: ANUBIS | Decimals: 18 | Fixed Supply: 1,000,000,000
--
--  Key Features:
--  - Fixed total supply (no inflation, ever)
--  - Deflationary through burn mechanisms
--  - Milestone-based vesting for builder allocation
--  - Certification-based gas discounts
--
--  Token Distribution (v4.0):
--  - Solo Builder:       30% (300M) - Milestone-vested over 6 years
--  - Protocol Treasury:  30% (300M) - DAO-controlled from day 1
--  - Genesis Validators: 15% (150M) - Earned via block production
--  - Genesis Provers:     8%  (80M) - Earned via proof generation
--  - Developer Ecosystem: 7%  (70M) - Earned via verified commits
--  - Quantum Insurance:   5%  (50M) - Immutably locked
--  - Bug Bounties/Audits: 5%  (50M) - On-chain claim system
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Token with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Token Constants (Immutable from Genesis)
   ---------------------------------------------------------------------------

   --  Token identity
   Token_Symbol           : constant String := "ANUBIS";
   Token_Name             : constant String := "Anubis Token";
   Token_Decimals         : constant := 18;

   --  Total supply: 1 billion tokens with 18 decimals
   --  Represented as raw units (wei equivalent)
   Total_Supply_Raw       : constant := 1_000_000_000;
   Wei_Per_Token          : constant := 10 ** 18;

   --  Distribution allocations (in raw tokens, multiply by Wei_Per_Token for wei)
   Alloc_Solo_Builder     : constant := 300_000_000;  -- 30%
   Alloc_Protocol_Treasury: constant := 300_000_000;  -- 30%
   Alloc_Genesis_Validators: constant := 150_000_000; -- 15%
   Alloc_Genesis_Provers  : constant := 80_000_000;   -- 8%
   Alloc_Developer_Ecosystem: constant := 70_000_000; -- 7%
   Alloc_Quantum_Insurance: constant := 50_000_000;   -- 5%
   Alloc_Bug_Bounties     : constant := 50_000_000;   -- 5%

   --  Burn rates (basis points, 1 = 0.01%)
   Base_Fee_Burn_BP       : constant := 8000;  -- 80% of base fee burned
   Failed_Proposal_Burn_BP: constant := 10000; -- 100% of bond burned
   Slashing_Burn_BP       : constant := 5000;  -- 50% of slash burned
   Privacy_Fee_Burn_BP    : constant := 500;   -- 5% of privacy fee burned

   --  Certification discount levels (basis points off gas)
   Discount_Bronze        : constant := 0;     -- 0% (SPARK Mode only)
   Discount_Silver        : constant := 1000;  -- 10% (All VCs proven)
   Discount_Gold          : constant := 2000;  -- 20% (Constant-time)
   Discount_Platinum      : constant := 3000;  -- 30% (Third-party audit)

   --  Certification deposit requirements
   Deposit_Bronze         : constant := 1_000;    -- 1,000 ANUBIS
   Deposit_Silver         : constant := 10_000;   -- 10,000 ANUBIS
   Deposit_Gold           : constant := 50_000;   -- 50,000 ANUBIS
   Deposit_Platinum       : constant := 100_000;  -- 100,000 ANUBIS

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Token amount (256-bit for full precision)
   --  In wei-equivalent units (10^-18 ANUBIS)
   subtype Token_Amount is Unsigned_64;  -- Simplified for now
   subtype Token_Amount_256 is Byte_Array (0 .. 31);  -- Full U256

   --  Allocation categories
   type Allocation_Category is (
      Solo_Builder,
      Protocol_Treasury,
      Genesis_Validators,
      Genesis_Provers,
      Developer_Ecosystem,
      Quantum_Insurance,
      Bug_Bounties
   );

   --  Certification levels
   type Certification_Level is (
      None,       -- No certification
      Bronze,     -- SPARK Mode enabled
      Silver,     -- All VCs proven
      Gold,       -- Constant-time verified
      Platinum    -- Third-party audited
   );

   --  Token transfer result
   type Transfer_Result is (
      Success,
      Insufficient_Balance,
      Invalid_Recipient,
      Amount_Overflow,
      Sender_Frozen,
      Recipient_Frozen
   );

   --  Burn result
   type Burn_Result is (
      Burned,
      Insufficient_Balance,
      Amount_Zero,
      Burn_Forbidden
   );

   ---------------------------------------------------------------------------
   --  Account State
   ---------------------------------------------------------------------------

   type Account_State is record
      Address        : Byte_Array (0 .. 31);
      Balance        : Token_Amount;
      Nonce          : Unsigned_64;
      Frozen         : Boolean;
      Cert_Level     : Certification_Level;
      Cert_Deposit   : Token_Amount;
      Last_Activity  : Unsigned_64;
   end record;

   type Account_Array is array (Natural range <>) of Account_State;

   ---------------------------------------------------------------------------
   --  Token State (Global Ledger)
   ---------------------------------------------------------------------------

   type Token_State is record
      Total_Supply       : Token_Amount;   -- Current supply (decreases with burns)
      Total_Burned       : Token_Amount;   -- Cumulative burned amount
      Genesis_Block      : Unsigned_64;    -- Block 0 timestamp
      Current_Block      : Unsigned_64;
      --  Allocation tracking
      Builder_Vested     : Token_Amount;
      Builder_Released   : Token_Amount;
      Treasury_Released  : Token_Amount;
      Validators_Earned  : Token_Amount;
      Provers_Earned     : Token_Amount;
      Developers_Earned  : Token_Amount;
      Insurance_Locked   : Token_Amount;
      Bounties_Paid      : Token_Amount;
   end record;

   ---------------------------------------------------------------------------
   --  Token Operations
   ---------------------------------------------------------------------------

   --  Transfer tokens between accounts
   procedure Transfer (
      From           : in Out Account_State;
      To             : in Out Account_State;
      Amount         : Token_Amount;
      Result         : out Transfer_Result
   ) with
      Global => null,
      Pre => Amount > 0,
      Post => (if Result = Success then
                  From.Balance = From.Balance'Old - Amount
                  and To.Balance = To.Balance'Old + Amount);

   --  Burn tokens (deflationary mechanism)
   procedure Burn (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Amount         : Token_Amount;
      Result         : out Burn_Result
   ) with
      Global => null,
      Pre => Amount > 0,
      Post => (if Result = Burned then
                  Account.Balance = Account.Balance'Old - Amount
                  and State.Total_Burned = State.Total_Burned'Old + Amount
                  and State.Total_Supply = State.Total_Supply'Old - Amount);

   --  Mint tokens (only for initial distribution, never after genesis)
   --  This is called exactly once during genesis block creation
   procedure Genesis_Mint (
      State          : in Out Token_State;
      Category       : Allocation_Category;
      Recipient      : in Out Account_State;
      Amount         : Token_Amount;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => State.Current_Block = 0  -- Only at genesis
             and Amount > 0;

   --  Get balance of account
   function Balance_Of (Account : Account_State) return Token_Amount with
      Global => null,
      Post => Balance_Of'Result = Account.Balance;

   --  Get total supply
   function Get_Total_Supply (State : Token_State) return Token_Amount with
      Global => null,
      Post => Get_Total_Supply'Result = State.Total_Supply;

   --  Get total burned
   function Get_Total_Burned (State : Token_State) return Token_Amount with
      Global => null,
      Post => Get_Total_Burned'Result = State.Total_Burned;

   ---------------------------------------------------------------------------
   --  Certification Operations
   ---------------------------------------------------------------------------

   --  Apply for certification level
   procedure Apply_Certification (
      Account        : in Out Account_State;
      Level          : Certification_Level;
      Deposit        : Token_Amount;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Level /= None
             and Deposit >= Get_Required_Deposit (Level);

   --  Upgrade certification level
   procedure Upgrade_Certification (
      Account        : in Out Account_State;
      New_Level      : Certification_Level;
      Additional_Deposit : Token_Amount;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => New_Level > Account.Cert_Level;

   --  Revoke certification (slashing if fraudulent)
   procedure Revoke_Certification (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Slash          : Boolean;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Account.Cert_Level /= None;

   --  Get gas discount for certification level
   function Get_Gas_Discount (Level : Certification_Level) return Natural with
      Global => null,
      Post => Get_Gas_Discount'Result <= 3000;  -- Max 30%

   --  Get required deposit for certification level
   function Get_Required_Deposit (Level : Certification_Level) return Token_Amount with
      Global => null;

   ---------------------------------------------------------------------------
   --  Burn Mechanisms
   ---------------------------------------------------------------------------

   --  Burn base fee (called by transaction processor)
   procedure Burn_Base_Fee (
      State          : in Out Token_State;
      Fee_Amount     : Token_Amount
   ) with
      Global => null;

   --  Burn failed proposal bond
   procedure Burn_Proposal_Bond (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Bond_Amount    : Token_Amount
   ) with
      Global => null;

   --  Burn slashing amount
   procedure Burn_Slashing (
      Account        : in Out Account_State;
      State          : in Out Token_State;
      Slash_Amount   : Token_Amount
   ) with
      Global => null;

   --  Burn privacy fee
   procedure Burn_Privacy_Fee (
      State          : in Out Token_State;
      Fee_Amount     : Token_Amount
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Allocation Queries
   ---------------------------------------------------------------------------

   --  Get allocation for category
   function Get_Allocation (Category : Allocation_Category) return Token_Amount with
      Global => null;

   --  Get remaining allocation
   function Get_Remaining (
      State          : Token_State;
      Category       : Allocation_Category
   ) return Token_Amount with
      Global => null;

   --  Check if allocation is exhausted
   function Is_Exhausted (
      State          : Token_State;
      Category       : Allocation_Category
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Token Utilities
   ---------------------------------------------------------------------------

   --  Convert raw amount to display string (with decimals)
   procedure To_Display_String (
      Amount         : Token_Amount;
      Output         : out String;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 32;

   --  Convert display string to raw amount
   procedure From_Display_String (
      Input          : String;
      Amount         : out Token_Amount;
      Success        : out Boolean
   ) with
      Global => null;

   --  Check if amount is dust (too small to matter)
   function Is_Dust (Amount : Token_Amount) return Boolean with
      Global => null;

   --  Minimum transferable amount (anti-spam)
   Min_Transfer_Amount    : constant := 1_000_000_000_000;  -- 0.000001 ANUBIS

   ---------------------------------------------------------------------------
   --  Account Operations
   ---------------------------------------------------------------------------

   --  Initialize account
   procedure Init_Account (
      Account        : out Account_State;
      Address        : Byte_Array
   ) with
      Global => null,
      Pre => Address'Length = 32,
      Post => Account.Balance = 0
              and Account.Nonce = 0
              and not Account.Frozen
              and Account.Cert_Level = None;

   --  Freeze account (emergency measure)
   procedure Freeze_Account (
      Account        : in Out Account_State
   ) with
      Global => null,
      Post => Account.Frozen;

   --  Unfreeze account
   procedure Unfreeze_Account (
      Account        : in Out Account_State
   ) with
      Global => null,
      Post => not Account.Frozen;

   --  Increment nonce
   procedure Increment_Nonce (
      Account        : in Out Account_State
   ) with
      Global => null,
      Post => Account.Nonce = Account.Nonce'Old + 1;

   ---------------------------------------------------------------------------
   --  State Initialization
   ---------------------------------------------------------------------------

   --  Initialize token state at genesis
   procedure Init_Token_State (
      State          : out Token_State;
      Genesis_Time   : Unsigned_64
   ) with
      Global => null,
      Post => State.Total_Supply = Total_Supply_Raw * Wei_Per_Token
              and State.Total_Burned = 0
              and State.Genesis_Block = Genesis_Time
              and State.Current_Block = 0;

   --  Advance to next block
   procedure Advance_Block (
      State          : in Out Token_State
   ) with
      Global => null,
      Post => State.Current_Block = State.Current_Block'Old + 1;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize account state
   procedure Serialize_Account (
      Account        : Account_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 128;

   --  Deserialize account state
   procedure Deserialize_Account (
      Input          : Byte_Array;
      Account        : out Account_State;
      Success        : out Boolean
   ) with
      Global => null;

   --  Serialize token state
   procedure Serialize_Token_State (
      State          : Token_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 256;

   --  Deserialize token state
   procedure Deserialize_Token_State (
      Input          : Byte_Array;
      State          : out Token_State;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Account (Account : out Account_State) with
      Global => null;

   procedure Zeroize_Token_State (State : out Token_State) with
      Global => null;

end Anubis_Token;
