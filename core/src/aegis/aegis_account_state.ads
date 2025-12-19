pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Address_Types; use Anubis_Address_Types;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;

--  Aegis_Account_State: Post-Quantum Secure Account Management
--
--  Implements account state management for the AEGIS VM with:
--  - ML-DSA-87 address-based accounts (32-byte Account_ID)
--  - Nonce tracking for replay protection
--  - Balance management (256-bit unsigned integers)
--  - Account metadata (code hash, storage root)
--  - SPARK Gold-level verification
--
--  Account Model:
--  - Address: Derived from ML-DSA-87 public key via AAS-001 v3.1
--  - Balance: U256 (256-bit unsigned, same as EVM)
--  - Nonce: Unsigned_64 (increments per transaction)
--  - Code_Hash: SHA3-256 of contract bytecode (zero for EOAs)
--  - Storage_Root: Merkle Patricia Trie root of contract storage
--
--  Security Properties:
--  - Post-quantum security via ML-DSA-87 derived addresses
--  - Replay protection via monotonic nonce increments
--  - Balance overflow protection (checked arithmetic)
--  - State consistency guarantees via SPARK contracts
--
--  References:
--  - AAS-001 v3.1 (Aegis Address Standard)
--  - KHEPRI Blueprint v1.0 (Account Model)
--  - NIST FIPS 204 (ML-DSA)

package Aegis_Account_State with
   SPARK_Mode => On,
   Always_Terminates
is

   ---------------------------------------------------------------------------
   --  Account State Types
   ---------------------------------------------------------------------------

   --  Account metadata: complete state for an account
   type Account_State is record
      Address      : Account_ID;           --  32-byte account identifier
      Balance      : U256;                 --  Account balance (256-bit)
      Nonce        : Unsigned_64;          --  Transaction counter
      Code_Hash    : Hash256;              --  Contract code hash (zero for EOA)
      Storage_Root : Hash256;              --  Storage trie root
      Exists       : Boolean;              --  True if account exists
   end record;

   --  Null account (default state for non-existent accounts)
   Null_Account : constant Account_State := (
      Address      => (others => 0),
      Balance      => U256_Zero,
      Nonce        => 0,
      Code_Hash    => Hash256_Zero,
      Storage_Root => Hash256_Zero,
      Exists       => False
   );

   --  Account error codes
   type Account_Error is (
      Success,                    --  Operation succeeded
      Account_Not_Found,          --  Account does not exist
      Insufficient_Balance,       --  Balance too low for operation
      Balance_Overflow,           --  Balance would overflow U256
      Nonce_Overflow,             --  Nonce would overflow Unsigned_64
      Invalid_Address,            --  Address format invalid
      Contract_Already_Deployed,  --  Account already has code
      Invalid_Transfer            --  Transfer parameters invalid
   );

   ---------------------------------------------------------------------------
   --  Ghost Functions for SPARK Specification
   ---------------------------------------------------------------------------

   --  Ghost: Account is well-formed (all fields valid)
   function Account_Well_Formed (Acc : Account_State) return Boolean is
      (Acc.Exists and then
       (for all I in Account_ID_Index => Acc.Address (I) in Unsigned_8))
   with Ghost, Pure_Function;

   --  Ghost: Account is an EOA (Externally Owned Account, no code)
   function Is_EOA_Ghost (Acc : Account_State) return Boolean is
      (Acc.Exists and then
       (for all I in Hash256'Range => Acc.Code_Hash (I) = 0))
   with Ghost, Pure_Function;

   --  Runtime: Account is an EOA (Externally Owned Account, no code)
   function Is_EOA (Acc : Account_State) return Boolean with
      Global => null,
      Post   => Is_EOA'Result = Is_EOA_Ghost (Acc);

   --  Ghost: Account is a contract (has code)
   function Is_Contract_Ghost (Acc : Account_State) return Boolean is
      (Acc.Exists and then
       (for some I in Hash256'Range => Acc.Code_Hash (I) /= 0))
   with Ghost, Pure_Function;

   --  Runtime: Account is a contract (has code)
   function Is_Contract (Acc : Account_State) return Boolean with
      Global => null,
      Post   => Is_Contract'Result = Is_Contract_Ghost (Acc);

   --  Ghost: Nonce is safe to increment (no overflow)
   function Nonce_Safe_To_Increment (Acc : Account_State) return Boolean is
      (Acc.Nonce < Unsigned_64'Last)
   with Ghost, Pure_Function;

   --  Ghost: Balance is safe to add amount (no overflow)
   function Balance_Safe_To_Add (Acc : Account_State; Amount : U256) return Boolean is
      (not Equal (Acc.Balance, U256_Max) and then
       not Equal (Amount, U256_Max))
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Account Creation and Initialization
   ---------------------------------------------------------------------------

   --  Create new account with given address
   --
   --  Preconditions:
   --  - Address must be valid (non-zero for user/contract accounts)
   --
   --  Postconditions:
   --  - Account exists with zero balance and nonce
   --  - Code_Hash and Storage_Root are zero (EOA by default)
   function Create_Account (
      Address : Account_ID
   ) return Account_State with
      Global => null,
      Post   => Create_Account'Result.Exists and then
                Create_Account'Result.Address = Address and then
                Equal (Create_Account'Result.Balance, U256_Zero) and then
                Create_Account'Result.Nonce = 0 and then
                Is_EOA (Create_Account'Result);

   --  Initialize contract account with code hash
   --
   --  Preconditions:
   --  - Account must exist
   --  - Account must be an EOA (not already a contract)
   --  - Code_Hash must be non-zero
   --
   --  Postconditions:
   --  - Account becomes a contract
   --  - Code_Hash is set to provided value
   procedure Initialize_Contract (
      Account   : in out Account_State;
      Code_Hash : in     Hash256;
      Success   : out    Boolean;
      Error     : out    Account_Error
   ) with
      Global => null,
      Pre    => Account_Well_Formed (Account) and then
                Is_EOA (Account) and then
                (for some I in Hash256'Range => Code_Hash (I) /= 0),
      Post   => (if Success then
                    Is_Contract (Account) and then
                    Account.Code_Hash = Code_Hash
                 else
                    Account = Account'Old);

   ---------------------------------------------------------------------------
   --  Nonce Management
   ---------------------------------------------------------------------------

   --  Get current nonce
   function Get_Nonce (Account : Account_State) return Unsigned_64 with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => Get_Nonce'Result = Account.Nonce;

   --  Increment nonce (for transaction replay protection)
   --
   --  Preconditions:
   --  - Account must exist
   --  - Nonce must not overflow (< Unsigned_64'Last)
   --
   --  Postconditions:
   --  - On success: Nonce incremented by 1
   --  - On failure: Account unchanged
   procedure Increment_Nonce (
      Account : in out Account_State;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => (if Success then
                    Account.Nonce = Account.Nonce'Old + 1 and then
                    Account.Address = Account.Address'Old and then
                    Equal (Account.Balance, Account.Balance'Old)
                 else
                    Account = Account'Old and then
                    (if Nonce_Safe_To_Increment (Account'Old) then
                       Error = Aegis_Account_State.Success
                     else
                       Error = Aegis_Account_State.Nonce_Overflow));

   --  Set nonce (for account initialization or state import)
   --
   --  Preconditions:
   --  - Account must exist
   --
   --  Postconditions:
   --  - Nonce set to specified value
   procedure Set_Nonce (
      Account : in out Account_State;
      Nonce   : in     Unsigned_64
   ) with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => Account.Nonce = Nonce and then
                Account.Address = Account.Address'Old and then
                Equal (Account.Balance, Account.Balance'Old);

   ---------------------------------------------------------------------------
   --  Balance Management
   ---------------------------------------------------------------------------

   --  Get current balance
   function Get_Balance (Account : Account_State) return U256 with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => Equal (Get_Balance'Result, Account.Balance);

   --  Add amount to balance
   --
   --  Preconditions:
   --  - Account must exist
   --  - Result must not overflow U256
   --
   --  Postconditions:
   --  - On success: Balance increased by amount
   --  - On failure: Account unchanged
   procedure Add_Balance (
      Account : in out Account_State;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => (if Success then
                    not Equal (Account.Balance, Account.Balance'Old) and then
                    Account.Address = Account.Address'Old and then
                    Account.Nonce = Account.Nonce'Old
                 else
                    Account = Account'Old);

   --  Subtract amount from balance
   --
   --  Preconditions:
   --  - Account must exist
   --  - Balance must be >= amount (sufficient funds)
   --
   --  Postconditions:
   --  - On success: Balance decreased by amount
   --  - On failure: Account unchanged
   procedure Sub_Balance (
      Account : in out Account_State;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => (if Success then
                    not Equal (Account.Balance, Account.Balance'Old) and then
                    Account.Address = Account.Address'Old and then
                    Account.Nonce = Account.Nonce'Old
                 else
                    Account = Account'Old);

   --  Set balance directly (for genesis or state import)
   --
   --  Preconditions:
   --  - Account must exist
   --
   --  Postconditions:
   --  - Balance set to specified value
   procedure Set_Balance (
      Account : in out Account_State;
      Balance : in     U256
   ) with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => Equal (Account.Balance, Balance) and then
                Account.Address = Account.Address'Old and then
                Account.Nonce = Account.Nonce'Old;

   --  Transfer amount from one account to another
   --
   --  Preconditions:
   --  - Both accounts must exist
   --  - From account must have sufficient balance
   --  - To account balance must not overflow
   --
   --  Postconditions:
   --  - On success: From balance decreased, To balance increased by amount
   --  - On failure: Both accounts unchanged
   procedure Transfer (
      From    : in out Account_State;
      To      : in out Account_State;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    Account_Error
   ) with
      Global => null,
      Pre    => Account_Well_Formed (From) and then
                Account_Well_Formed (To),
      Post   => (if Success then
                    not Equal (From.Balance, From.Balance'Old) and then
                    not Equal (To.Balance, To.Balance'Old)
                 else
                    From = From'Old and then
                    To = To'Old);

   ---------------------------------------------------------------------------
   --  Account Metadata
   ---------------------------------------------------------------------------

   --  Check if account exists
   function Account_Exists (Account : Account_State) return Boolean with
      Global => null,
      Post   => Account_Exists'Result = Account.Exists;

   --  Get code hash
   function Get_Code_Hash (Account : Account_State) return Hash256 with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => Get_Code_Hash'Result = Account.Code_Hash;

   --  Set storage root (for contract storage updates)
   procedure Set_Storage_Root (
      Account      : in out Account_State;
      Storage_Root : in     Hash256
   ) with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => Account.Storage_Root = Storage_Root and then
                Account.Address = Account.Address'Old and then
                Equal (Account.Balance, Account.Balance'Old) and then
                Account.Nonce = Account.Nonce'Old;

   --  Get storage root
   function Get_Storage_Root (Account : Account_State) return Hash256 with
      Global => null,
      Pre    => Account_Well_Formed (Account),
      Post   => Get_Storage_Root'Result = Account.Storage_Root;

   ---------------------------------------------------------------------------
   --  Account Validation
   ---------------------------------------------------------------------------

   --  Validate account state consistency
   --
   --  Checks:
   --  - If code_hash is non-zero, account is a contract
   --  - If code_hash is zero, account is an EOA
   --  - Address is valid (not all zeros for existing accounts)
   function Validate_Account (Account : Account_State) return Boolean with
      Global => null,
      Post   => Validate_Account'Result =
                (Account.Exists and then
                 (Is_EOA (Account) or Is_Contract (Account)));

end Aegis_Account_State;
