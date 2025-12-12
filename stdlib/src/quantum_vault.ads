pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Khepri_Types;
with Anubis_Address_Types;

--  QUANTUM VAULT: NIST Level 5 Secure Multi-Signature Treasury
--
--  This contract demonstrates the power of ML-DSA-87 addresses by implementing
--  a quantum-resistant multi-signature vault with advanced security features.

package Quantum_Vault with
   SPARK_Mode => On,
   Abstract_State => Vault_State
is

   --  Basic type aliases
   subtype Address      is Khepri_Types.Address;
   subtype Network_Type is Anubis_Address_Types.Network_Type;

   type Address_Array is array (Natural range <>) of Address;
   type Natural_Array is array (Natural range <>) of Natural;

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_Signers   : constant := 20;   -- Maximum number of authorized signers
   Max_Guardians : constant := 10;   -- Maximum number of recovery guardians
   Max_Pending   : constant := 50;   -- Maximum pending transactions
   
   Recovery_Delay : constant := 100_000;  -- Blocks before recovery can execute
   
   --  ML-DSA-87 signature size (bytes, per Anubis_MLDSA)
   Signature_Size : constant := 4627;

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Error_Code is (
      Error_None,
      Error_Invalid_Signer,
      Error_Invalid_Threshold,
      Error_Unauthorized,
      Error_Invalid_Transaction,
      Error_Recovery_Not_Active,
      Error_Internal
   );

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Signer role with quantum-resistant address
   type Signer_Info is record
      Signer_Address : Address;                 -- Must be ML-DSA-87-derived
      Public_Key     : Byte_Array (0 .. 2591);  -- ML-DSA-87 public key
      Added_At       : Natural;                 -- Block number when added
      Active         : Boolean;                 -- Is this signer active?
      Weight         : Natural;                 -- Voting weight
   end record;

   Empty_Signer : constant Signer_Info := (
      Signer_Address => (others => 0),
      Public_Key     => (others => 0),
      Added_At       => 0,
      Active         => False,
      Weight         => 0
   );

   --  Transaction proposal
   type Transaction_Proposal is record
      ID           : Natural;                   -- Unique transaction ID
      Destination  : Address;                   -- Where to send funds
      Amount       : Natural;                   -- How much to send (demo units)
      Data         : Byte_Array (0 .. 1023);    -- Call data
      Data_Length  : Natural;
      Proposed_At  : Natural;                   -- Block number
      Executed     : Boolean;
      
      --  Signature tracking
      Approvals    : Natural;                   -- Number of approvals
      Total_Weight : Natural;                   -- Total weight of approvals
      
      --  Aggregated proof placeholder
      Signature_Proof : Byte_Array (0 .. 255);
      Proof_Valid     : Boolean;
   end record;

   Empty_Transaction : constant Transaction_Proposal := (
      ID              => 0,
      Destination     => (others => 0),
      Amount          => 0,
      Data            => (others => 0),
      Data_Length     => 0,
      Proposed_At     => 0,
      Executed        => False,
      Approvals       => 0,
      Total_Weight    => 0,
      Signature_Proof => (others => 0),
      Proof_Valid     => False
   );

   --  Recovery guardian
   type Guardian_Info is record
      Guardian_Address : Address;
      Added_At         : Natural;
      Active           : Boolean;
   end record;

   --  Vault configuration
   type Vault_Config is record
      Threshold        : Natural;     -- Required signatures (M in M-of-N)
      Total_Signers    : Natural;     -- Total signers (N in M-of-N)
      Total_Guardians  : Natural;
      Balance          : Natural;     -- Demo balance
      Next_TX_ID       : Natural;     -- Next transaction ID
      Recovery_Active  : Boolean;     -- Is recovery mode active?
      Recovery_Started : Natural;     -- Block when recovery started
   end record;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Signers   : Address_Array;
      Weights   : Natural_Array;
      Threshold : Natural;
      Success   : out Boolean;
      Error     : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Signers'Length >= 1 and then
                Signers'Length <= Max_Signers and then
                Weights'Length >= 1 and then
                Weights'Length <= Max_Signers and then
                Signers'Length = Weights'Length and then
                Threshold >= 1 and then
                Threshold <= Max_Signers,
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   ---------------------------------------------------------------------------
   --  Address Verification (NIST Level 5 Enforcement)
   ---------------------------------------------------------------------------

   function Verify_MLDSA87_Address (
      Addr    : Address;
      Network : Network_Type
   ) return Boolean with
      Global => null;

   procedure Extract_Public_Key (
      Addr       : Address;
      Public_Key : out Byte_Array;
      Success    : out Boolean
   ) with
      Global => Vault_State,
      Pre    => Public_Key'Length = 2592;

   function Verify_Signature (
      Message    : Byte_Array;
      Signature  : Byte_Array;
      Public_Key : Byte_Array
   ) return Boolean with
      Global => null,
      Pre    => Signature'Length = Signature_Size
                and Public_Key'Length = 2592;

   ---------------------------------------------------------------------------
   --  Multi-Signature Operations
   ---------------------------------------------------------------------------

   procedure Propose_Transaction (
      Proposer    : Address;
      Destination : Address;
      Amount      : Natural;
      Data        : Byte_Array;
      Block_Num   : Natural;
      TX_ID       : out Natural;
      Success     : out Boolean;
      Error       : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Data'Length <= 1024,
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   procedure Approve_Transaction (
      TX_ID      : Natural;
      Signer     : Address;
      Signature  : Byte_Array;
      Block_Num  : Natural;
      Success    : out Boolean;
      Error      : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Signature'Length = Signature_Size,
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   procedure Execute_Transaction (
      TX_ID     : Natural;
      Caller    : Address;
      Block_Num : Natural;
      Success   : out Boolean;
      Error     : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   ---------------------------------------------------------------------------
   --  Signer Management
   ---------------------------------------------------------------------------

   procedure Add_Signer (
      New_Signer : Address;
      Public_Key : Byte_Array;
      Weight     : Natural;
      Approvals  : Byte_Array;
      Success    : out Boolean;
      Error      : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Pre    => Public_Key'Length = 2592,
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   procedure Remove_Signer (
      Signer    : Address;
      Approvals : Byte_Array;
      Success   : out Boolean;
      Error     : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   procedure Rotate_Key (
      Old_Address   : Address;
      New_Address   : Address;
      New_Public_Key : Byte_Array;
      Signature     : Byte_Array;
      Success       : out Boolean;
      Error         : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Pre    => New_Public_Key'Length = 2592
                and Signature'Length = Signature_Size,
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   ---------------------------------------------------------------------------
   --  Recovery
   ---------------------------------------------------------------------------

   procedure Initiate_Recovery (
      Guardian_Signatures : Byte_Array;
      Block_Num          : Natural;
      Success            : out Boolean;
      Error              : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   procedure Execute_Recovery (
      New_Signers : Address_Array;
      New_Weights : Natural_Array;
      Block_Num   : Natural;
      Success     : out Boolean;
      Error       : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Pre    => New_Signers'Length > 0
                and New_Signers'Length = New_Weights'Length,
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   procedure Cancel_Recovery (
      Approvals : Byte_Array;
      Success   : out Boolean;
      Error     : out Error_Code
   ) with
      Global => (In_Out => Vault_State),
      Post   => (if Success then Error = Error_None else Error /= Error_None);

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Threshold return Natural with
      Global => Vault_State;

   function Get_Total_Signers return Natural with
      Global => Vault_State,
      Post   => Get_Total_Signers'Result <= Max_Signers;

   function Get_Balance return Natural with
      Global => Vault_State;

   function Is_Signer (Addr : Address) return Boolean with
      Global => Vault_State;

   function Get_Signer_Weight (Addr : Address) return Natural with
      Global => Vault_State;

   function Get_Transaction_Status (TX_ID : Natural) return Boolean with
      Global => Vault_State;

   function Get_Approvals (TX_ID : Natural) return Natural with
      Global => Vault_State;

   function Get_Config return Vault_Config with
      Global => Vault_State;

   ---------------------------------------------------------------------------
   --  Security Statistics
   ---------------------------------------------------------------------------

   function Get_Total_Signatures_Verified return Natural with
      Global => Vault_State;

   function Get_Signature_Verification_Gas return Natural with
      Global => Vault_State;

   function Get_STARK_Batches return Natural with
      Global => Vault_State;

end Quantum_Vault;
