pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;
with Khepri_Certification; use Khepri_Certification;
with Khepri_Crypto; use Khepri_Crypto;

--  KHEPRI Deployer: Contract Deployment Toolchain
--
--  This package provides deployment infrastructure for KHEPRI
--  smart contracts. It handles:
--  - Deployment transaction creation
--  - Contract address computation
--  - Initialization parameter encoding
--  - Gas estimation for deployment
--  - Multi-network deployment
--  - Upgrade management
--
--  Deployment Pipeline:
--  1. Verify contract is deployable
--  2. Compute deployment address
--  3. Create deployment transaction
--  4. Sign transaction
--  5. Submit to network
--  6. Verify deployment
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 7.3: Deployer

package Khepri_Deployer with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Network Types
   ---------------------------------------------------------------------------

   --  Target network
   type Network_Type is (
      Network_Local,      --  Local development
      Network_Testnet,    --  Public testnet
      Network_Mainnet,    --  Production mainnet
      Network_Custom      --  Custom network
   );

   --  Network configuration
   type Network_Config is record
      Network          : Network_Type;
      Chain_ID         : Word64;
      RPC_Endpoint     : Bounded_String;
      Block_Explorer   : Bounded_String;
      Gas_Price_Gwei   : Natural;
      Max_Gas_Limit    : Word64;
      Confirmation_Blocks : Natural;
   end record;

   Local_Network : constant Network_Config := (
      Network          => Network_Local,
      Chain_ID         => 31337,
      RPC_Endpoint     => Empty_String,
      Block_Explorer   => Empty_String,
      Gas_Price_Gwei   => 1,
      Max_Gas_Limit    => 30_000_000,
      Confirmation_Blocks => 1
   );

   ---------------------------------------------------------------------------
   --  Deployment Configuration
   ---------------------------------------------------------------------------

   --  Deployment mode
   type Deploy_Mode is (
      Mode_Create,        --  New deployment
      Mode_Create2,       --  Deterministic address
      Mode_Upgrade,       --  Upgrade existing
      Mode_Clone          --  Clone existing
   );

   --  Deployer configuration
   type Deployer_Config is record
      --  Network
      Network_Cfg        : Network_Config;

      --  Deployment options
      Deployment_Mode    : Deploy_Mode;
      Salt               : Hash256;         --  For CREATE2
      Verify_Deployment  : Boolean;
      Wait_Confirmations : Boolean;

      --  Gas options
      Gas_Limit          : Word64;
      Gas_Price_Gwei     : Natural;
      Max_Fee_Per_Gas    : Word64;
      Max_Priority_Fee   : Word64;

      --  Signing
      Use_Hardware_Wallet: Boolean;
      Keystore_Path      : Bounded_String;

      --  Output
      Save_Receipt       : Boolean;
      Receipt_Path       : Bounded_String;
   end record;

   Default_Deploy_Config : constant Deployer_Config := (
      Network_Cfg        => Local_Network,
      Deployment_Mode    => Mode_Create,
      Salt               => (others => 0),
      Verify_Deployment  => True,
      Wait_Confirmations => True,
      Gas_Limit          => 5_000_000,
      Gas_Price_Gwei     => 20,
      Max_Fee_Per_Gas    => 100_000_000_000,
      Max_Priority_Fee   => 2_000_000_000,
      Use_Hardware_Wallet=> False,
      Keystore_Path      => Empty_String,
      Save_Receipt       => True,
      Receipt_Path       => Empty_String
   );

   ---------------------------------------------------------------------------
   --  Deployment Session
   ---------------------------------------------------------------------------

   type Deployment_Session is private;

   --  Deployment result
   type Deployment_Result is (
      Deploy_Success,         --  Contract deployed
      Deploy_Pending,         --  Waiting for confirmation
      Deploy_Failed,          --  Deployment failed
      Deploy_Verification_Failed,  --  Deployed but verification failed
      Deploy_Error            --  Error during deployment
   );

   --  Deployment receipt
   type Deployment_Receipt is record
      Contract_Address   : Khepri_Types.Address;
      Transaction_Hash   : Hash256;
      Block_Number       : Word64;
      Gas_Used           : Word64;
      Effective_Gas_Price: Word64;
      Status             : Deployment_Result;
      Error_Message      : Bounded_String;
      Timestamp          : Word64;
   end record;

   --  Create deployment session
   function Create_Session (
      Config : Deployer_Config
   ) return Deployment_Session with
      Global => null;

   --  Load contract for deployment
   procedure Load_Contract (
      Session       : in Out Deployment_Session;
      Manifest_Path : in     String;
      Bytecode_Path : in     String;
      Success       : out    Boolean
   ) with
      Global => null,
      Pre    => Manifest_Path'Length <= 256 and Bytecode_Path'Length <= 256;

   --  Set constructor arguments
   procedure Set_Constructor_Args (
      Session : in Out Deployment_Session;
      Args    : in     Byte_Array;
      Success : out    Boolean
   ) with
      Global => null;

   --  Compute deployment address
   function Compute_Address (
      Session : Deployment_Session
   ) return Khepri_Types.Address with
      Global => null;

   --  Estimate deployment gas
   function Estimate_Gas (
      Session : Deployment_Session
   ) return Word64 with
      Global => null;

   --  Deploy contract (requires signing)
   procedure Deploy (
      Session : in Out Deployment_Session;
      Signer  : in     MLDSA_Secret_Key;
      Receipt : out    Deployment_Receipt
   ) with
      Global => null;

   --  Get deployment receipt
   function Get_Receipt (
      Session : Deployment_Session
   ) return Deployment_Receipt with
      Global => null;

   ---------------------------------------------------------------------------
   --  Address Computation
   ---------------------------------------------------------------------------

   --  Compute CREATE address
   function Compute_Create_Address (
      Deployer : Khepri_Types.Address;
      Nonce    : Word64
   ) return Khepri_Types.Address with
      Global => null;

   --  Compute CREATE2 address
   function Compute_Create2_Address (
      Deployer      : Khepri_Types.Address;
      Salt          : Hash256;
      Init_Code_Hash: Hash256
   ) return Khepri_Types.Address with
      Global => null;

   ---------------------------------------------------------------------------
   --  Transaction Building
   ---------------------------------------------------------------------------

   --  Deployment transaction
   type Deploy_Transaction is record
      Nonce          : Word64;
      Gas_Price      : Word64;
      Gas_Limit      : Word64;
      To_Address     : Khepri_Types.Address;  --  Zero for creation
      Value          : Uint256;
      Data           : Byte_Array (0 .. 1048575);  --  1MB max
      Data_Size      : Natural;
      Chain_ID       : Word64;
      Is_Signed      : Boolean;
      Signature      : MLDSA_Signature;
   end record;

   --  Build deployment transaction
   procedure Build_Transaction (
      Session     : in     Deployment_Session;
      Transaction : out    Deploy_Transaction;
      Success     : out    Boolean
   ) with
      Global => null;

   --  Sign transaction
   procedure Sign_Transaction (
      Transaction : in Out Deploy_Transaction;
      Signer      : in     MLDSA_Secret_Key;
      Success     : out    Boolean
   ) with
      Global => null;

   --  Serialize signed transaction
   procedure Serialize_Transaction (
      Transaction : in     Deploy_Transaction;
      Output      : out    Byte_Array;
      Size        : out    Natural;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= 1048576;

   ---------------------------------------------------------------------------
   --  Network Interaction
   ---------------------------------------------------------------------------

   --  Submit transaction to network
   procedure Submit_Transaction (
      Session       : in     Deployment_Session;
      Transaction   : in     Deploy_Transaction;
      Tx_Hash       : out    Hash256;
      Success       : out    Boolean
   ) with
      Global => null;

   --  Wait for confirmation
   procedure Wait_Confirmation (
      Session       : in     Deployment_Session;
      Tx_Hash       : in     Hash256;
      Confirmations : in     Natural;
      Receipt       : out    Deployment_Receipt;
      Success       : out    Boolean
   ) with
      Global => null;

   --  Get transaction status
   function Get_Tx_Status (
      Session : Deployment_Session;
      Tx_Hash : Hash256
   ) return Deployment_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Upgrade Management
   ---------------------------------------------------------------------------

   --  Upgrade proxy type
   type Proxy_Type is (
      Proxy_UUPS,           --  Universal Upgradeable Proxy
      Proxy_Transparent,    --  Transparent proxy
      Proxy_Beacon,         --  Beacon proxy
      Proxy_Minimal         --  Minimal proxy (clone)
   );

   --  Deploy upgradeable contract
   procedure Deploy_Upgradeable (
      Session    : in Out Deployment_Session;
      P_Type     : in     Proxy_Type;
      Signer     : in     MLDSA_Secret_Key;
      Impl_Addr  : out    Khepri_Types.Address;
      Proxy_Addr : out    Khepri_Types.Address;
      Receipt    : out    Deployment_Receipt
   ) with
      Global => null;

   --  Upgrade existing proxy
   procedure Upgrade_Proxy (
      Session     : in Out Deployment_Session;
      Proxy_Addr  : in     Khepri_Types.Address;
      New_Impl    : in     Byte_Array;
      Signer      : in     MLDSA_Secret_Key;
      Receipt     : out    Deployment_Receipt
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Deployment Verification
   ---------------------------------------------------------------------------

   --  Verify deployed contract matches expected
   procedure Verify_Deployment (
      Session        : in     Deployment_Session;
      Contract_Addr  : in     Khepri_Types.Address;
      Expected_Hash  : in     Hash256;
      Is_Verified    : out    Boolean
   ) with
      Global => null;

   --  Get deployed bytecode
   procedure Get_Deployed_Code (
      Session       : in     Deployment_Session;
      Contract_Addr : in     Khepri_Types.Address;
      Code          : out    Byte_Array;
      Code_Size     : out    Natural;
      Success       : out    Boolean
   ) with
      Global => null,
      Pre    => Code'Length >= 1048576;

private

   type Deployment_Session is record
      Config           : Deployer_Config;
      Bytecode         : Byte_Array (0 .. 1048575);
      Bytecode_Size    : Natural;
      Constructor_Args : Byte_Array (0 .. 65535);
      Args_Size        : Natural;
      Manifest_Hash    : Hash256;
      Is_Loaded        : Boolean;
      Receipt          : Deployment_Receipt;
   end record;

end Khepri_Deployer;
