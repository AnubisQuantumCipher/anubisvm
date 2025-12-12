-------------------------------------------------------------------------------
--  AEGIS PRIVACY - Privacy Precompiles for AegisVM
--  Bridges VM syscalls with Anubis Privacy Layer
--
--  Provides privacy-preserving smart contract capabilities:
--  - Encrypted private state (SHIELD)
--  - Confidential transactions (WHISPER)
--  - Private execution with ZK proofs (GATE)
--  - Selective disclosure and viewing keys (EYE)
--  - Ring signatures for anonymity (VEIL)
--
--  Syscall Interface:
--  - PRIVATE_STORE:     Store encrypted private state
--  - PRIVATE_LOAD:      Load and decrypt private state
--  - COMMIT_AMOUNT:     Create confidential amount commitment
--  - VERIFY_RANGE:      Verify range proof for commitment
--  - PRIVATE_CALL:      Execute contract privately with ZK proof
--  - CREATE_DISCLOSURE: Create selective disclosure proof
--  - VERIFY_RING_SIG:   Verify ring signature
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types;

package Aegis_Privacy with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Type Aliases (use Aegis_VM_Types exclusively for consistency)
   ---------------------------------------------------------------------------

   subtype VM_Byte is Aegis_VM_Types.Byte;
   subtype VM_Byte_Array is Aegis_VM_Types.Byte_Array;
   subtype VM_Hash256 is Aegis_VM_Types.Hash256;
   subtype VM_Contract_Address is Aegis_VM_Types.Contract_Address;
   subtype VM_Gas_Amount is Aegis_VM_Types.Gas_Amount;

   --  Syscall number type (0-255 range)
   subtype Syscall_Number is VM_Byte;

   ---------------------------------------------------------------------------
   --  Privacy Syscall Numbers (0x80-0x9F range reserved for privacy)
   ---------------------------------------------------------------------------

   Syscall_Private_Store      : constant Syscall_Number := 16#80#;
   Syscall_Private_Load       : constant Syscall_Number := 16#81#;
   Syscall_Private_Delete     : constant Syscall_Number := 16#82#;

   Syscall_Commit_Amount      : constant Syscall_Number := 16#83#;
   Syscall_Verify_Range       : constant Syscall_Number := 16#84#;
   Syscall_Add_Commitments    : constant Syscall_Number := 16#85#;
   Syscall_Verify_Balance     : constant Syscall_Number := 16#86#;

   Syscall_Private_Call       : constant Syscall_Number := 16#87#;
   Syscall_Verify_Execution   : constant Syscall_Number := 16#88#;
   Syscall_Create_Session     : constant Syscall_Number := 16#89#;
   Syscall_Close_Session      : constant Syscall_Number := 16#8A#;

   Syscall_Create_Disclosure  : constant Syscall_Number := 16#8B#;
   Syscall_Verify_Disclosure  : constant Syscall_Number := 16#8C#;
   Syscall_Derive_View_Key    : constant Syscall_Number := 16#8D#;
   Syscall_Generate_Stealth   : constant Syscall_Number := 16#8E#;

   Syscall_Ring_Sign          : constant Syscall_Number := 16#8F#;
   Syscall_Verify_Ring_Sig    : constant Syscall_Number := 16#90#;
   Syscall_Compute_Key_Image  : constant Syscall_Number := 16#91#;
   Syscall_Check_Spent        : constant Syscall_Number := 16#92#;

   Syscall_ZK_Prove_Range     : constant Syscall_Number := 16#93#;
   Syscall_ZK_Verify_Range    : constant Syscall_Number := 16#94#;
   Syscall_ZK_Prove_Linear    : constant Syscall_Number := 16#95#;
   Syscall_ZK_Verify_Linear   : constant Syscall_Number := 16#96#;

   --  Confidential transfer syscalls (WHISPER extended)
   Syscall_Confidential_Transfer : constant Syscall_Number := 16#97#;
   Syscall_Create_Transfer_Proof : constant Syscall_Number := 16#98#;
   Syscall_Verify_Transfer       : constant Syscall_Number := 16#99#;
   Syscall_Scan_Output           : constant Syscall_Number := 16#9A#;

   ---------------------------------------------------------------------------
   --  Gas Costs for Privacy Operations
   ---------------------------------------------------------------------------

   --  Private state operations (SHIELD)
   Gas_Private_Store          : constant VM_Gas_Amount := 50_000;
   Gas_Private_Load           : constant VM_Gas_Amount := 25_000;
   Gas_Private_Delete         : constant VM_Gas_Amount := 5_000;

   --  Confidential transactions (WHISPER)
   Gas_Commit_Amount          : constant VM_Gas_Amount := 10_000;
   Gas_Verify_Range           : constant VM_Gas_Amount := 100_000;
   Gas_Add_Commitments        : constant VM_Gas_Amount := 2_000;
   Gas_Verify_Balance         : constant VM_Gas_Amount := 50_000;

   --  Private execution (GATE)
   Gas_Private_Call_Base      : constant VM_Gas_Amount := 200_000;
   Gas_Verify_Execution       : constant VM_Gas_Amount := 500_000;
   Gas_Create_Session         : constant VM_Gas_Amount := 100_000;
   Gas_Close_Session          : constant VM_Gas_Amount := 5_000;

   --  Selective disclosure (EYE)
   Gas_Create_Disclosure      : constant VM_Gas_Amount := 50_000;
   Gas_Verify_Disclosure      : constant VM_Gas_Amount := 75_000;
   Gas_Derive_View_Key        : constant VM_Gas_Amount := 10_000;
   Gas_Generate_Stealth       : constant VM_Gas_Amount := 15_000;

   --  Ring signatures (VEIL)
   Gas_Ring_Sign_Base         : constant VM_Gas_Amount := 100_000;
   Gas_Ring_Sign_Per_Member   : constant VM_Gas_Amount := 10_000;
   Gas_Verify_Ring_Sig_Base   : constant VM_Gas_Amount := 150_000;
   Gas_Verify_Ring_Per_Member : constant VM_Gas_Amount := 15_000;
   Gas_Compute_Key_Image      : constant VM_Gas_Amount := 20_000;
   Gas_Check_Spent            : constant VM_Gas_Amount := 5_000;

   --  Lattice ZK proofs
   Gas_ZK_Prove_Range         : constant VM_Gas_Amount := 200_000;
   Gas_ZK_Verify_Range        : constant VM_Gas_Amount := 100_000;
   Gas_ZK_Prove_Linear        : constant VM_Gas_Amount := 150_000;
   Gas_ZK_Verify_Linear       : constant VM_Gas_Amount := 75_000;

   --  Confidential transfers (WHISPER extended)
   Gas_Confidential_Transfer  : constant VM_Gas_Amount := 300_000;
   Gas_Create_Transfer_Proof  : constant VM_Gas_Amount := 250_000;
   Gas_Verify_Transfer        : constant VM_Gas_Amount := 200_000;
   Gas_Scan_Output            : constant VM_Gas_Amount := 50_000;

   ---------------------------------------------------------------------------
   --  Privacy Context (extends Execution_Context)
   ---------------------------------------------------------------------------

   --  Maximum concurrent private sessions
   Max_Private_Sessions : constant := 4;

   --  Private session identifier
   subtype Session_ID is VM_Byte_Array (0 .. 31);

   --  Private state entry reference
   type Private_State_Ref is record
      Contract_Addr  : VM_Contract_Address;
      Entry_Key      : VM_Hash256;
      Is_Valid       : Boolean;
   end record;

   --  Privacy execution mode
   type Privacy_Mode is (
      Privacy_Off,           -- Standard transparent execution
      Privacy_Encrypted,     -- Encrypted state only
      Privacy_Confidential,  -- Confidential amounts
      Privacy_Private,       -- Full private execution with ZK proofs
      Privacy_Anonymous      -- Anonymous with ring signatures
   );

   --  Privacy context for execution
   type Privacy_Context is record
      Mode           : Privacy_Mode;
      User_KEM_PK    : VM_Byte_Array (0 .. 1567);  -- ML-KEM-1024 public key
      View_Key       : VM_Byte_Array (0 .. 31);    -- Viewing key for auditing
      Session_Active : Boolean;
      Session_ID     : Aegis_Privacy.Session_ID;
   end record;

   ---------------------------------------------------------------------------
   --  Result Types
   ---------------------------------------------------------------------------

   type Privacy_Result is record
      Success        : Boolean;
      Gas_Used       : VM_Gas_Amount;
      Error_Code     : Natural;
      Return_Hash    : VM_Hash256;  -- Hash of return data
   end record;

   Privacy_Result_OK : constant Privacy_Result := (
      Success     => True,
      Gas_Used    => 0,
      Error_Code  => 0,
      Return_Hash => (others => 0)
   );

   --  Error codes
   Privacy_Error_None             : constant := 0;
   Privacy_Error_Invalid_Key      : constant := 1;
   Privacy_Error_Decryption_Failed : constant := 2;
   Privacy_Error_Invalid_Proof    : constant := 3;
   Privacy_Error_Session_Expired  : constant := 4;
   Privacy_Error_Permission       : constant := 5;
   Privacy_Error_Out_Of_Gas       : constant := 6;
   Privacy_Error_Invalid_Commitment : constant := 7;
   Privacy_Error_Range_Violation  : constant := 8;
   Privacy_Error_Key_Image_Spent  : constant := 9;
   Privacy_Error_Ring_Too_Small   : constant := 10;

   ---------------------------------------------------------------------------
   --  Private State Operations (SHIELD)
   ---------------------------------------------------------------------------

   --  Store encrypted private state
   procedure Private_Store (
      Contract       : VM_Contract_Address;
      Key            : VM_Hash256;
      Plaintext      : VM_Byte_Array;
      User_KEM_PK    : VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Plaintext'Length <= 4096
             and then User_KEM_PK'Length = 1568
             and then Randomness'Length = 64;

   --  Load and decrypt private state
   procedure Private_Load (
      Contract       : VM_Contract_Address;
      Key            : VM_Hash256;
      User_KEM_SK    : VM_Byte_Array;
      Plaintext      : out VM_Byte_Array;
      Plaintext_Len  : out Natural;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => User_KEM_SK'Length = 3168
             and then Plaintext'Length >= 4096;

   --  Delete private state entry
   procedure Private_Delete (
      Contract       : VM_Contract_Address;
      Key            : VM_Hash256;
      Result         : out Privacy_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Confidential Transaction Operations (WHISPER)
   ---------------------------------------------------------------------------

   --  Create amount commitment with blinding factor
   procedure Commit_Amount (
      Value          : Unsigned_64;
      Blinding       : VM_Byte_Array;
      Commitment     : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Blinding'Length = 32
             and then Commitment'Length = 64;

   --  Verify range proof for commitment
   procedure Verify_Range_Proof (
      Commitment     : VM_Byte_Array;
      Proof          : VM_Byte_Array;
      Bits           : Natural;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Commitment'Length = 64
             and then Proof'Length = 2048
             and then Bits <= 64;

   --  Add two commitments homomorphically
   procedure Add_Commitments (
      A, B           : VM_Byte_Array;
      Sum            : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => A'Length = 64
             and then B'Length = 64
             and then Sum'Length = 64;

   --  Verify balance equation: inputs = outputs + fee
   procedure Verify_Balance (
      Input_Commits  : VM_Byte_Array;
      Output_Commits : VM_Byte_Array;
      Fee            : Unsigned_64;
      Proof          : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Private Execution Operations (GATE)
   ---------------------------------------------------------------------------

   --  Execute contract privately with ZK proof generation
   procedure Private_Call (
      Target         : VM_Contract_Address;
      Function_Sel   : Unsigned_32;
      Private_Args   : VM_Byte_Array;
      Public_Args    : VM_Byte_Array;
      Gas_Limit      : VM_Gas_Amount;
      Mode           : Privacy_Mode;
      Proof          : out VM_Byte_Array;
      Output         : out VM_Byte_Array;
      Output_Len     : out Natural;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Private_Args'Length <= 4096
             and then Public_Args'Length <= 1024
             and then Proof'Length >= 8192
             and then Output'Length >= 1024;

   --  Verify execution proof
   procedure Verify_Execution_Proof (
      Proof          : VM_Byte_Array;
      Contract       : VM_Contract_Address;
      Old_State_Hash : VM_Hash256;
      New_State_Hash : VM_Hash256;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Proof'Length = 8192;

   --  Create private session with contract
   procedure Create_Private_Session (
      Contract       : VM_Contract_Address;
      User_KEM_SK    : VM_Byte_Array;
      Contract_KEM_PK: VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Session        : out Session_ID;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => User_KEM_SK'Length = 3168
             and then Contract_KEM_PK'Length = 1568
             and then Randomness'Length = 64;

   --  Close private session (zeroize secrets)
   procedure Close_Private_Session (
      Session        : in out Session_ID;
      Result         : out Privacy_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Selective Disclosure Operations (EYE)
   ---------------------------------------------------------------------------

   --  Create selective disclosure proof
   procedure Create_Disclosure (
      Credential     : VM_Byte_Array;
      Holder_Secret  : VM_Byte_Array;
      Disclose_Mask  : Unsigned_32;
      Challenge      : VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Holder_Secret'Length = 32
             and then Challenge'Length = 32
             and then Proof'Length >= 512;

   --  Verify selective disclosure proof
   procedure Verify_Disclosure (
      Proof          : VM_Byte_Array;
      Disclosed_Attrs: VM_Byte_Array;
      Issuer_PK      : VM_Byte_Array;
      Challenge      : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Issuer_PK'Length = 2592
             and then Challenge'Length = 32;

   --  Derive viewing key from master seed
   procedure Derive_Viewing_Key (
      Master_Seed    : VM_Byte_Array;
      View_Key       : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Master_Seed'Length = 32
             and then View_Key'Length = 32;

   --  Generate stealth address for receiving
   procedure Generate_Stealth_Address (
      View_Key       : VM_Byte_Array;
      Spend_Key      : VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Stealth_Addr   : out VM_Byte_Array;
      Tx_Public_Key  : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => View_Key'Length = 32
             and then Spend_Key'Length = 32
             and then Randomness'Length = 32
             and then Stealth_Addr'Length = 32
             and then Tx_Public_Key'Length = 32;

   ---------------------------------------------------------------------------
   --  Ring Signature Operations (VEIL)
   ---------------------------------------------------------------------------

   --  Sign with ring signature (anonymity set)
   procedure Ring_Sign (
      Ring_PKs       : VM_Byte_Array;  -- Concatenated public keys
      Ring_Size      : Natural;
      Signer_Index   : Natural;
      Signer_SK      : VM_Byte_Array;
      Message        : VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Signature      : out VM_Byte_Array;
      Key_Image      : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Ring_Size >= 2
             and then Signer_Index < Ring_Size
             and then Randomness'Length >= 64
             and then Key_Image'Length = 64;

   --  Verify ring signature
   procedure Verify_Ring_Signature (
      Ring_PKs       : VM_Byte_Array;
      Ring_Size      : Natural;
      Message        : VM_Byte_Array;
      Signature      : VM_Byte_Array;
      Key_Image      : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Ring_Size >= 2
             and then Key_Image'Length = 64;

   --  Compute key image for linkability
   procedure Compute_Key_Image (
      Secret_Key     : VM_Byte_Array;
      Key_Image      : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Key_Image'Length = 64;

   --  Check if key image has been spent
   procedure Check_Key_Image_Spent (
      Key_Image      : VM_Byte_Array;
      Spent_Images   : VM_Byte_Array;  -- Concatenated spent images
      Num_Spent      : Natural;
      Is_Spent       : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Key_Image'Length = 64;

   ---------------------------------------------------------------------------
   --  Confidential Transfer Operations (WHISPER Extended)
   ---------------------------------------------------------------------------

   --  Execute confidential value transfer
   procedure Confidential_Transfer (
      Sender_Commit  : VM_Byte_Array;
      Recv_Commit    : VM_Byte_Array;
      Transfer_Proof : VM_Byte_Array;
      Recv_Addr      : VM_Contract_Address;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Sender_Commit'Length = 64
             and then Recv_Commit'Length = 64
             and then Transfer_Proof'Length >= 2048;

   --  Create balance proof for transfer
   procedure Create_Transfer_Proof (
      Input_Value    : Unsigned_64;
      Output_Value   : Unsigned_64;
      Input_Blinding : VM_Byte_Array;
      Output_Blinding: VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Input_Blinding'Length = 32
             and then Output_Blinding'Length = 32
             and then Randomness'Length = 64
             and then Proof'Length >= 2048;

   --  Verify confidential transfer
   procedure Verify_Confidential_Transfer (
      Sender_Commit  : VM_Byte_Array;
      Recv_Commit    : VM_Byte_Array;
      Fee_Commit     : VM_Byte_Array;
      Transfer_Proof : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Sender_Commit'Length = 64
             and then Recv_Commit'Length = 64
             and then Fee_Commit'Length = 64
             and then Transfer_Proof'Length >= 2048;

   --  Scan for owned confidential outputs
   procedure Scan_Confidential_Output (
      Tx_Public_Key  : VM_Byte_Array;
      Output_Commit  : VM_Byte_Array;
      View_Key       : VM_Byte_Array;
      Spend_Key      : VM_Byte_Array;
      Is_Owned       : out Boolean;
      Decrypted_Value: out Unsigned_64;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Tx_Public_Key'Length = 32
             and then Output_Commit'Length = 64
             and then View_Key'Length = 32
             and then Spend_Key'Length = 32;

   ---------------------------------------------------------------------------
   --  Lattice ZK Proof Operations (VEIL/Lattice)
   ---------------------------------------------------------------------------

   --  Create lattice-based range proof
   procedure ZK_Prove_Range (
      Value          : Unsigned_64;
      Num_Bits       : Natural;
      Randomness     : VM_Byte_Array;
      Commitment     : out VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Num_Bits in 1 .. 64
             and then Randomness'Length = 64;

   --  Verify lattice-based range proof
   procedure ZK_Verify_Range (
      Commitment     : VM_Byte_Array;
      Num_Bits       : Natural;
      Proof          : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Num_Bits in 1 .. 64;

   --  Create linear relation proof: a*x + b*y = c
   procedure ZK_Prove_Linear (
      X_Value        : VM_Byte_Array;
      Y_Value        : VM_Byte_Array;
      A_Coeff        : Integer;
      B_Coeff        : Integer;
      Randomness     : VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) with
      Global => null,
      Pre => Randomness'Length = 64;

   --  Verify linear relation proof
   procedure ZK_Verify_Linear (
      Com_X, Com_Y   : VM_Byte_Array;
      A_Coeff        : Integer;
      B_Coeff        : Integer;
      C_Result       : VM_Byte_Array;
      Proof          : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Syscall Dispatcher Integration
   ---------------------------------------------------------------------------

   --  Check if syscall is a privacy operation
   function Is_Privacy_Syscall (Syscall : Syscall_Number) return Boolean with
      Global => null,
      Post => Is_Privacy_Syscall'Result =
              (Syscall in 16#80# .. 16#9A#);

   --  Get gas cost for privacy syscall
   function Get_Privacy_Gas_Cost (
      Syscall   : Syscall_Number;
      Data_Size : Natural := 0;
      Ring_Size : Natural := 0
   ) return VM_Gas_Amount with
      Global => null,
      Pre => Is_Privacy_Syscall (Syscall);

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Privacy_Context (Ctx : in Out Privacy_Context) with
      Global => null;

   procedure Zeroize_Session (Session : in Out Session_ID) with
      Global => null,
      Post => (for all I in Session'Range => Session (I) = 0);

end Aegis_Privacy;
