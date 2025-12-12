pragma SPARK_Mode (On);

with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;
with Node_Contract_Registry; use Node_Contract_Registry;
with Sphinx_Native;

--  Node Contract Executor: Dispatches contract calls by entry point name
--
--  KHEPRI contracts are native ELF binaries, NOT bytecode. This executor
--  supports two modes:
--
--  1. ELF Native Execution (production):
--     - Contract code is compiled Ada/SPARK to native ARM/x86/RISC-V ELF
--     - ELF is loaded via Sphinx_Native.Load_ELF
--     - Syscalls are intercepted via the SPHINX runtime
--     - Gas is metered based on WCET analysis
--
--  2. Table-Driven Built-in Contracts (development):
--     - Pre-registered contracts with hardcoded handlers
--     - Used for testing and bootstrapping before full ELF flow
--
--  Built-in Contracts:
--  1. HelloCounter - Simple counter with increment/reset
--  2. SimpleToken  - ERC20-style fungible token
--  3. SimpleVault  - Deposit/withdraw with balance tracking
--
--  Storage: Uses Khepri_State for persistent key-value storage

package Node_Contract_Executor with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Execution Context
   ---------------------------------------------------------------------------

   --  Maximum contracts that can be registered for native execution
   Max_Native_Contracts : constant := 32;

   --  Maximum entry points per contract
   Max_Entry_Points : constant := 64;

   --  Entry point handler type
   --  Takes: args buffer, args size
   --  Returns: result value, success flag
   type EP_Handler is access procedure (
      Args      : in     Args_Buffer;
      Args_Size : in     Natural;
      Ret       : out    U256;
      Success   : out    Boolean
   );

   --  Entry point registration
   type Entry_Point_Def is record
      Name     : Entry_Name;
      Name_Len : Natural;
      Handler  : EP_Handler;
      Is_View  : Boolean;  -- True for read-only calls
   end record;

   subtype EP_Index is Natural range 0 .. Max_Entry_Points - 1;
   type EP_Table is array (EP_Index) of Entry_Point_Def;

   --  Contract type enumeration
   type Contract_Type is (
      Contract_HelloCounter,
      Contract_SimpleToken,
      Contract_SimpleVault,
      Contract_QuantumDID,       -- Post-quantum decentralized identity
      Contract_Staking,          -- Proof-of-stake staking
      Contract_Governance,       -- On-chain governance/voting
      Contract_NativeELF,        -- Native ELF binary (production)
      Contract_Unknown
   );

   --  Native contract definition
   type Native_Contract is record
      Code_Hash_Val  : Hash256;
      Contract_Addr  : Contract_Address;
      Kind           : Contract_Type;
      Entries        : EP_Table;
      Entry_Count    : Natural;
      Is_Registered  : Boolean;
      --  ELF Native fields (used when Kind = Contract_NativeELF)
      ELF_Contract   : Sphinx_Native.Loaded_Contract;
      Is_ELF_Valid   : Boolean;
   end record;

   subtype Native_Index is Natural range 0 .. Max_Native_Contracts - 1;
   type Native_Registry is array (Native_Index) of Native_Contract;

   ---------------------------------------------------------------------------
   --  Executor State
   ---------------------------------------------------------------------------

   type Executor_State is record
      Natives         : Native_Registry;
      Native_Count    : Natural;
      Is_Initialized  : Boolean;
      --  Current execution context (accessible by handlers)
      Current_Sender  : Contract_Address;
      Current_Self    : Contract_Address;
      Current_Value   : U256;
      Current_Gas     : Gas_Amount;
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (Exec : out Executor_State) with
      Global => null,
      Post   => Exec.Is_Initialized and Exec.Native_Count = 0;

   ---------------------------------------------------------------------------
   --  Native Contract Registration
   ---------------------------------------------------------------------------

   procedure Register_Native (
      Exec        : in Out Executor_State;
      Code_Hash   : in     Hash256;
      Entries     : in     EP_Table;
      Entry_Count : in     Natural;
      Success     : out    Boolean
   ) with
      Global => null,
      Pre    => Exec.Is_Initialized and Entry_Count <= Max_Entry_Points;

   ---------------------------------------------------------------------------
   --  Contract Type Detection
   ---------------------------------------------------------------------------

   --  Detect contract type from manifest name
   function Detect_Contract_Type (
      Manifest_Name : String
   ) return Contract_Type with
      Global => null;

   ---------------------------------------------------------------------------
   --  Execution
   ---------------------------------------------------------------------------

   procedure Execute (
      Exec         : in Out Executor_State;
      Registry     : in     Registry_State;
      Contract_Idx : in     Stored_Contract_Index;
      From         : in     Contract_Address;
      EP_Name      : in     Entry_Name;
      EP_Name_Len  : in     Natural;
      Args         : in     Args_Buffer;
      Args_Size    : in     Natural;
      Gas_Limit    : in     Gas_Amount;
      Value        : in     U256;
      Is_View      : in     Boolean;
      Ret          : out    Invoke_Result
   ) with
      Global => null,
      Pre    => Exec.Is_Initialized and Registry.Is_Initialized and
                EP_Name_Len <= Max_Entry_Name_Length;

   ---------------------------------------------------------------------------
   --  Built-in Contract Entry Tables
   ---------------------------------------------------------------------------

   --  HelloCounter: Simple counter
   procedure Get_HelloCounter_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) with
      Global => null;

   --  SimpleToken: ERC20-style token
   --  Entry points: Initialize, Transfer, Approve, TransferFrom,
   --                BalanceOf, TotalSupply, Allowance, Name, Symbol
   procedure Get_SimpleToken_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) with
      Global => null;

   --  SimpleVault: Deposit/Withdraw vault
   --  Entry points: Initialize, Deposit, Withdraw, BalanceOf, TotalDeposits
   procedure Get_SimpleVault_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) with
      Global => null;

   --  QuantumDID: Post-quantum decentralized identity registry
   --  Entry points: Register, AddCredential, RevokeCredential, GetIdentity,
   --                IsRegistered, GetCredentialCount
   procedure Get_QuantumDID_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) with
      Global => null;

   --  Staking: Proof-of-stake staking contract
   --  Entry points: Stake, Unstake, ClaimRewards, GetStake, GetTotalStaked,
   --                GetRewards, GetValidatorCount
   procedure Get_Staking_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) with
      Global => null;

   --  Governance: On-chain governance and voting
   --  Entry points: CreateProposal, Vote, Execute, GetProposal, GetVotes,
   --                GetProposalCount, GetQuorum
   procedure Get_Governance_Entries (
      Entries     : out EP_Table;
      Entry_Count : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Storage Context Management
   ---------------------------------------------------------------------------

   --  Set current contract for storage operations
   procedure Set_Storage_Context (
      Contract_Addr : in Contract_Address
   ) with
      Global => null;

   --  Get current execution context (for handlers)
   function Get_Current_Sender return Contract_Address with
      Global => null;

   function Get_Current_Self return Contract_Address with
      Global => null;

   function Get_Current_Value return U256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Native ELF Contract Loading
   ---------------------------------------------------------------------------

   --  Try to load code buffer as ELF binary
   --  Sets Success to True if code is a valid ELF that was successfully loaded
   procedure Try_Load_ELF (
      Code      : in     Node_Code_Buffer;
      Code_Size : in     Natural;
      Contract  :    out Sphinx_Native.Loaded_Contract;
      Success   :    out Boolean
   ) with
      Global => null;

   --  Check if code buffer looks like an ELF binary (magic number check)
   function Is_ELF_Binary (
      Code      : Node_Code_Buffer;
      Code_Size : Natural
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Native ELF Contract Execution
   ---------------------------------------------------------------------------

   --  Execute native ELF contract function
   --  Entry point is resolved by name using symbol table
   procedure Execute_ELF (
      Exec         : in Out Executor_State;
      Native_Idx   : in     Native_Index;
      EP_Name      : in     Entry_Name;
      EP_Name_Len  : in     Natural;
      Args         : in     Args_Buffer;
      Args_Size    : in     Natural;
      Gas_Limit    : in     Gas_Amount;
      Ret          : out    Invoke_Result
   ) with
      Global => null;

end Node_Contract_Executor;
