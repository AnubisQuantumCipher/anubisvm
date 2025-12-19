pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;
with Khepri_Crypto; use Khepri_Crypto;
with Aegis_VM_Types; use Aegis_VM_Types;

--  KHEPRI Transaction: Transaction Building and Signing
--
--  This package provides facilities for building, signing, and encoding
--  KHEPRI blockchain transactions with ML-DSA-87 post-quantum signatures.
--
--  Key Features:
--  - Transaction builder pattern
--  - ML-DSA-87 signing
--  - RLP-style encoding
--  - Gas estimation helpers
--  - Nonce management
--
--  Transaction Types:
--  - Transfer: Simple value transfer
--  - Contract Call: Call contract function
--  - Contract Deploy: Deploy new contract
--
--  Security:
--  - All signing operations use ML-DSA-87 (FIPS 204)
--  - Transaction hashes verified
--  - Replay protection via chain_id
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8: Transaction Format
--  - FIPS 204: ML-DSA Digital Signature Algorithm

package Khepri_Transaction with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Transaction Configuration
   ---------------------------------------------------------------------------

   --  Maximum transaction data size
   Max_Tx_Data_Size : constant := 131072;  -- 128 KB

   --  Maximum transaction size (encoded)
   Max_Tx_Size : constant := 262144;  -- 256 KB

   ---------------------------------------------------------------------------
   --  Transaction Types
   ---------------------------------------------------------------------------

   --  Transaction type identifier
   type Transaction_Type is (
      TX_Transfer,         -- Simple value transfer
      TX_Contract_Call,    -- Call existing contract
      TX_Contract_Deploy   -- Deploy new contract
   );

   --  Transaction data payload
   subtype TX_Data_Index is Natural range 0 .. Max_Tx_Data_Size - 1;
   type TX_Data_Array is array (TX_Data_Index range <>) of Byte;

   ---------------------------------------------------------------------------
   --  Transaction Structure
   ---------------------------------------------------------------------------

   --  Complete transaction (unsigned)
   type Transaction is record
      --  Transaction metadata
      TX_Type    : Transaction_Type;
      Chain_ID   : Uint256;
      Nonce      : Uint256;

      --  Sender and recipient
      From       : Address;
      To         : Address;  -- Null_Address for contract deploy

      --  Value and gas
      Value      : Wei;
      Gas_Limit  : Uint256;
      Gas_Price  : Uint256;

      --  Data payload
      Data       : TX_Data_Array (0 .. Max_Tx_Data_Size - 1);
      Data_Size  : Natural;

      --  Validity
      Valid      : Boolean;
   end record;

   --  Empty transaction
   Null_Transaction : constant Transaction := (
      TX_Type    => TX_Transfer,
      Chain_ID   => Zero,
      Nonce      => Zero,
      From       => Null_Address,
      To         => Null_Address,
      Value      => Zero,
      Gas_Limit  => Zero,
      Gas_Price  => Zero,
      Data       => (others => 0),
      Data_Size  => 0,
      Valid      => False
   );

   ---------------------------------------------------------------------------
   --  Signed Transaction
   ---------------------------------------------------------------------------

   --  Signed transaction with ML-DSA-87 signature
   type Signed_Transaction is record
      TX         : Transaction;
      Signature  : MLDSA_Signature;
      Public_Key : MLDSA_Public_Key;
      TX_Hash    : Bytes32;
      Signed     : Boolean;
   end record;

   --  Empty signed transaction
   Null_Signed_Transaction : constant Signed_Transaction := (
      TX         => Null_Transaction,
      Signature  => (others => 0),
      Public_Key => (others => 0),
      TX_Hash    => Bytes32_Zero,
      Signed     => False
   );

   ---------------------------------------------------------------------------
   --  Transaction Builder
   ---------------------------------------------------------------------------

   --  Transaction builder for constructing transactions
   type TX_Builder is private;

   --  Create new transaction builder
   function New_Builder return TX_Builder with
      Global => null,
      Post   => not Is_Valid (New_Builder'Result);

   --  Set transaction type
   function Set_Type (
      Builder : TX_Builder;
      TX_Type : Transaction_Type
   ) return TX_Builder with
      Global => null;

   --  Set chain ID
   function Set_Chain_ID (
      Builder  : TX_Builder;
      Chain_ID : Uint256
   ) return TX_Builder with
      Global => null;

   --  Set nonce
   function Set_Nonce (
      Builder : TX_Builder;
      Nonce   : Uint256
   ) return TX_Builder with
      Global => null;

   --  Set sender address
   function Set_From (
      Builder : TX_Builder;
      From    : Address
   ) return TX_Builder with
      Global => null;

   --  Set recipient address
   function Set_To (
      Builder : TX_Builder;
      To      : Address
   ) return TX_Builder with
      Global => null;

   --  Set value
   function Set_Value (
      Builder : TX_Builder;
      Value   : Wei
   ) return TX_Builder with
      Global => null;

   --  Set gas limit
   function Set_Gas_Limit (
      Builder   : TX_Builder;
      Gas_Limit : Uint256
   ) return TX_Builder with
      Global => null;

   --  Set gas price
   function Set_Gas_Price (
      Builder   : TX_Builder;
      Gas_Price : Uint256
   ) return TX_Builder with
      Global => null;

   --  Set data payload
   function Set_Data (
      Builder : TX_Builder;
      Data    : Byte_Array;
      Size    : Natural
   ) return TX_Builder with
      Global => null,
      Pre    => Size <= Data'Length and Size <= Max_Tx_Data_Size;

   --  Check if builder is valid (all required fields set)
   function Is_Valid (Builder : TX_Builder) return Boolean with
      Global => null;

   --  Build transaction from builder
   procedure Build (
      Builder : in  TX_Builder;
      TX      : out Transaction;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Is_Valid (Builder),
      Post   => (if Success then TX.Valid);

   ---------------------------------------------------------------------------
   --  Transaction Signing
   ---------------------------------------------------------------------------

   --  Sign transaction with ML-DSA-87
   procedure Sign_Transaction (
      TX         : in  Transaction;
      Secret_Key : in  MLDSA_Secret_Key;
      Public_Key : in  MLDSA_Public_Key;
      Signed_TX  : out Signed_Transaction;
      Result     : out Crypto_Result
   ) with
      Global => null,
      Pre    => TX.Valid,
      Post   => (if Result = Crypto_Success then Signed_TX.Signed);

   --  Verify signed transaction
   function Verify_Transaction (Signed_TX : Signed_Transaction) return Crypto_Result with
      Global => null,
      Pre    => Signed_TX.Signed;

   --  Recover signer address from signed transaction
   function Get_Signer (Signed_TX : Signed_Transaction) return Address with
      Global => null,
      Pre    => Signed_TX.Signed;

   ---------------------------------------------------------------------------
   --  Transaction Hashing
   ---------------------------------------------------------------------------

   --  Calculate transaction hash (for signing)
   function Calculate_TX_Hash (TX : Transaction) return Bytes32 with
      Global => null,
      Pre    => TX.Valid;

   --  Calculate signed transaction hash (includes signature)
   function Calculate_Signed_Hash (Signed_TX : Signed_Transaction) return Bytes32 with
      Global => null,
      Pre    => Signed_TX.Signed;

   ---------------------------------------------------------------------------
   --  Transaction Encoding
   ---------------------------------------------------------------------------

   --  Encode transaction (unsigned) to bytes
   procedure Encode_Transaction (
      TX      : in  Transaction;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => TX.Valid and Output'Length >= Max_Tx_Size;

   --  Encode signed transaction to bytes
   procedure Encode_Signed_Transaction (
      Signed_TX : in  Signed_Transaction;
      Output    : out Byte_Array;
      Size      : out Natural;
      Success   : out Boolean
   ) with
      Global => null,
      Pre    => Signed_TX.Signed and Output'Length >= Max_Tx_Size;

   --  Decode transaction from bytes
   procedure Decode_Transaction (
      Data    : in  Byte_Array;
      TX      : out Transaction;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Data'Length <= Max_Tx_Size;

   --  Decode signed transaction from bytes
   procedure Decode_Signed_Transaction (
      Data      : in  Byte_Array;
      Signed_TX : out Signed_Transaction;
      Success   : out Boolean
   ) with
      Global => null,
      Pre    => Data'Length <= Max_Tx_Size;

   ---------------------------------------------------------------------------
   --  Gas Estimation
   ---------------------------------------------------------------------------

   --  Estimate gas for transaction
   function Estimate_Gas (TX : Transaction) return Uint256 with
      Global => null,
      Pre    => TX.Valid;

   --  Calculate intrinsic gas (base cost)
   function Calculate_Intrinsic_Gas (TX : Transaction) return Uint256 with
      Global => null,
      Pre    => TX.Valid;

   --  Gas constants
   Gas_TX_Base          : constant := 21000;   -- Base transaction cost
   Gas_TX_Data_Zero     : constant := 4;       -- Per zero byte in data
   Gas_TX_Data_NonZero  : constant := 16;      -- Per non-zero byte in data
   Gas_TX_Create        : constant := 32000;   -- Contract creation

   ---------------------------------------------------------------------------
   --  Transaction Helpers
   ---------------------------------------------------------------------------

   --  Create simple transfer transaction
   function Create_Transfer (
      From      : Address;
      To        : Address;
      Value     : Wei;
      Nonce     : Uint256;
      Chain_ID  : Uint256;
      Gas_Price : Uint256
   ) return Transaction with
      Global => null,
      Post   => Create_Transfer'Result.Valid and
                Create_Transfer'Result.TX_Type = TX_Transfer;

   --  Create contract call transaction
   function Create_Contract_Call (
      From      : Address;
      Contract  : Address;
      Value     : Wei;
      Data      : Byte_Array;
      Data_Size : Natural;
      Nonce     : Uint256;
      Chain_ID  : Uint256;
      Gas_Limit : Uint256;
      Gas_Price : Uint256
   ) return Transaction with
      Global => null,
      Pre    => Data_Size <= Data'Length and Data_Size <= Max_Tx_Data_Size,
      Post   => Create_Contract_Call'Result.Valid and
                Create_Contract_Call'Result.TX_Type = TX_Contract_Call;

   --  Create contract deployment transaction
   function Create_Contract_Deploy (
      From       : Address;
      Bytecode   : Byte_Array;
      Code_Size  : Natural;
      Value      : Wei;
      Nonce      : Uint256;
      Chain_ID   : Uint256;
      Gas_Limit  : Uint256;
      Gas_Price  : Uint256
   ) return Transaction with
      Global => null,
      Pre    => Code_Size <= Bytecode'Length and Code_Size <= Max_Tx_Data_Size,
      Post   => Create_Contract_Deploy'Result.Valid and
                Create_Contract_Deploy'Result.TX_Type = TX_Contract_Deploy and
                Equal (Create_Contract_Deploy'Result.To, Null_Address);

   ---------------------------------------------------------------------------
   --  Nonce Management
   ---------------------------------------------------------------------------

   --  Nonce cache for accounts (simple in-memory cache)
   Max_Nonce_Cache : constant := 256;

   --  Get cached nonce for address
   function Get_Nonce (Addr : Address) return Uint256 with
      SPARK_Mode => Off;

   --  Set cached nonce for address
   procedure Set_Nonce (Addr : Address; Nonce : Uint256) with
      SPARK_Mode => Off;

   --  Increment cached nonce for address
   procedure Increment_Nonce (Addr : Address) with
      SPARK_Mode => Off;

   --  Clear nonce cache
   procedure Clear_Nonce_Cache with
      SPARK_Mode => Off;

   ---------------------------------------------------------------------------
   --  Batch Transactions
   ---------------------------------------------------------------------------

   --  Maximum transactions in a batch
   Max_Batch_Size : constant := 64;

   --  Transaction batch
   type TX_Batch_Index is range 0 .. Max_Batch_Size - 1;
   type TX_Batch is array (TX_Batch_Index) of Signed_Transaction;

   --  Batch descriptor
   type Transaction_Batch is record
      Transactions : TX_Batch;
      Count        : Natural;
   end record;

   --  Empty batch
   Empty_Batch : constant Transaction_Batch := (
      Transactions => (others => Null_Signed_Transaction),
      Count        => 0
   );

   --  Add transaction to batch
   procedure Add_To_Batch (
      Batch     : in out Transaction_Batch;
      Signed_TX : in     Signed_Transaction;
      Success   : out    Boolean
   ) with
      Global => null,
      Pre    => Signed_TX.Signed and Batch.Count < Max_Batch_Size,
      Post   => (if Success then Batch.Count = Batch.Count'Old + 1);

   --  Encode transaction batch
   procedure Encode_Batch (
      Batch   : in  Transaction_Batch;
      Output  : out Byte_Array;
      Size    : out Natural;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= Max_Batch_Size * Max_Tx_Size;

private

   --  Builder internal state
   type TX_Builder is record
      TX           : Transaction;
      Has_Type     : Boolean;
      Has_Chain_ID : Boolean;
      Has_Nonce    : Boolean;
      Has_From     : Boolean;
      Has_Gas      : Boolean;
   end record;

   --  Empty builder
   Empty_Builder : constant TX_Builder := (
      TX           => Null_Transaction,
      Has_Type     => False,
      Has_Chain_ID => False,
      Has_Nonce    => False,
      Has_From     => False,
      Has_Gas      => False
   );

end Khepri_Transaction;
