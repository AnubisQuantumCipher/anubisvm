--  AegisVM Foreign Function Interface Exports
--
--  This package exports AegisVM cryptographic and VM functions to C.
--  It provides the implementation backing the aegisvm.h header file.
--
--  All functions use C calling convention and are designed for
--  integration with Cosmos SDK via CGO.
--
--  Memory Model:
--  - All buffers are caller-allocated
--  - Functions do not allocate memory
--  - Zeroization is caller"s responsibility for sensitive data
--
--  Thread Safety: All functions are reentrant.
--
--  References:
--  - aegisvm.h C header specification
--  - Cosmos SDK x/aegisvm module

pragma SPARK_Mode (Off);  --  FFI layer uses C convention

with Interfaces;          use Interfaces;
with Interfaces.C;        use Interfaces.C;
with Interfaces.C.Strings;
with System;

package Aegis_FFI is

   ---------------------------------------------------------------------------
   --  Result Codes (match aegisvm.h enum)
   ---------------------------------------------------------------------------

   AEGIS_OK                      : constant := 0;
   AEGIS_ERROR_INVALID_INPUT     : constant := 1;
   AEGIS_ERROR_INVALID_SIGNATURE : constant := 2;
   AEGIS_ERROR_DECAPS_FAILURE    : constant := 3;
   AEGIS_ERROR_OUT_OF_GAS        : constant := 4;
   AEGIS_ERROR_OUT_OF_MEMORY     : constant := 5;
   AEGIS_ERROR_INVALID_STATE     : constant := 6;
   AEGIS_ERROR_ACCESS_VIOLATION  : constant := 7;
   AEGIS_ERROR_STACK_OVERFLOW    : constant := 8;
   AEGIS_ERROR_INVALID_OPCODE    : constant := 9;
   AEGIS_ERROR_REVERTED          : constant := 10;
   AEGIS_ERROR_INVALID_BYTECODE  : constant := 11;
   AEGIS_ERROR_UNKNOWN           : constant := 255;

   type Result_Code is new Interfaces.C.int;

   ---------------------------------------------------------------------------
   --  Certification Levels
   ---------------------------------------------------------------------------

   AEGIS_CERT_BRONZE   : constant := 0;
   AEGIS_CERT_SILVER   : constant := 1;
   AEGIS_CERT_GOLD     : constant := 2;
   AEGIS_CERT_PLATINUM : constant := 3;

   type Cert_Level is new Interfaces.C.int;

   ---------------------------------------------------------------------------
   --  Execution Status
   ---------------------------------------------------------------------------

   AEGIS_EXEC_SUCCESS          : constant := 0;
   AEGIS_EXEC_REVERT           : constant := 1;
   AEGIS_EXEC_OUT_OF_GAS       : constant := 2;
   AEGIS_EXEC_STACK_OVERFLOW   : constant := 3;
   AEGIS_EXEC_INVALID_OPCODE   : constant := 4;
   AEGIS_EXEC_ACCESS_VIOLATION : constant := 5;
   AEGIS_EXEC_CONTRACT_ERROR   : constant := 6;

   type Exec_Status is new Interfaces.C.int;

   ---------------------------------------------------------------------------
   --  Cryptographic Constants
   ---------------------------------------------------------------------------

   MLDSA87_PUBLIC_KEY_SIZE  : constant := 2592;
   MLDSA87_SECRET_KEY_SIZE  : constant := 4896;
   MLDSA87_SIGNATURE_SIZE   : constant := 4627;

   MLKEM1024_PUBLIC_KEY_SIZE  : constant := 1568;
   MLKEM1024_SECRET_KEY_SIZE  : constant := 3168;
   MLKEM1024_CIPHERTEXT_SIZE  : constant := 1568;
   MLKEM1024_SHARED_SECRET_SIZE : constant := 32;

   SHA3_256_SIZE : constant := 32;
   SHA3_512_SIZE : constant := 64;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Byte array for raw data
   type Byte_Array is array (Natural range <>) of Interfaces.C.unsigned_char;
   pragma Convention (C, Byte_Array);

   --  256-bit hash
   type Hash256 is array (0 .. 31) of Interfaces.C.unsigned_char;
   pragma Convention (C, Hash256);

   --  512-bit hash
   type Hash512 is array (0 .. 63) of Interfaces.C.unsigned_char;
   pragma Convention (C, Hash512);

   --  256-bit unsigned integer (4 x 64-bit limbs)
   type U256_Limbs is array (0 .. 3) of Interfaces.C.unsigned_long;
   pragma Convention (C, U256_Limbs);

   type U256 is record
      Limbs : U256_Limbs;
   end record;
   pragma Convention (C, U256);

   --  VM context handle
   type VM_Context is limited private;
   type VM_Handle is access all VM_Context;
   pragma Convention (C, VM_Handle);

   --  Execution context for VM calls (matches aegis_exec_context_t)
   type Exec_Context is record
      Origin       : Hash256;                          --  Transaction origin
      Caller       : Hash256;                          --  Immediate caller
      Gas_Limit    : Interfaces.C.unsigned_long;       --  Maximum gas
      Gas_Price    : Interfaces.C.unsigned_long;       --  Gas price in uaegis
      Value        : Interfaces.C.unsigned_long;       --  Value transferred
      Block_Height : Interfaces.C.long;                --  Current block height
      Block_Time   : Interfaces.C.long;                --  Block timestamp
      Cert_Lvl     : Cert_Level;                       --  Certification level
      Is_Static    : Interfaces.C.int;                 --  Read-only call flag
   end record;
   pragma Convention (C, Exec_Context);

   ---------------------------------------------------------------------------
   --  Library Initialization
   ---------------------------------------------------------------------------

   function Aegis_Init return Result_Code;
   pragma Export (C, Aegis_Init, "aegis_init");

   procedure Aegis_Cleanup;
   pragma Export (C, Aegis_Cleanup, "aegis_cleanup");

   function Aegis_Version return Interfaces.C.Strings.chars_ptr;
   pragma Export (C, Aegis_Version, "aegis_version");

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Digital Signatures
   ---------------------------------------------------------------------------

   function Aegis_MLDSA87_Keygen (
      Public_Key : System.Address;
      Secret_Key : System.Address;
      Seed       : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_MLDSA87_Keygen, "aegis_mldsa87_keygen");

   function Aegis_MLDSA87_Sign (
      Signature  : System.Address;
      Sig_Len    : access Interfaces.C.size_t;
      Message    : System.Address;
      Msg_Len    : Interfaces.C.size_t;
      Secret_Key : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_MLDSA87_Sign, "aegis_mldsa87_sign");

   function Aegis_MLDSA87_Verify (
      Signature  : System.Address;
      Sig_Len    : Interfaces.C.size_t;
      Message    : System.Address;
      Msg_Len    : Interfaces.C.size_t;
      Public_Key : System.Address;
      Valid      : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_MLDSA87_Verify, "aegis_mldsa87_verify");

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Key Encapsulation
   ---------------------------------------------------------------------------

   function Aegis_MLKEM1024_Keygen (
      Encaps_Key : System.Address;
      Decaps_Key : System.Address;
      Seed       : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_MLKEM1024_Keygen, "aegis_mlkem1024_keygen");

   function Aegis_MLKEM1024_Encaps (
      Ciphertext    : System.Address;
      Shared_Secret : System.Address;
      Encaps_Key    : System.Address;
      Seed          : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_MLKEM1024_Encaps, "aegis_mlkem1024_encaps");

   function Aegis_MLKEM1024_Decaps (
      Shared_Secret : System.Address;
      Ciphertext    : System.Address;
      Decaps_Key    : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_MLKEM1024_Decaps, "aegis_mlkem1024_decaps");

   ---------------------------------------------------------------------------
   --  SHA3/SHAKE Hash Functions
   ---------------------------------------------------------------------------

   function Aegis_SHA3_256 (
      Output : System.Address;
      Input  : System.Address;
      Len    : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_SHA3_256, "aegis_sha3_256");

   function Aegis_SHA3_512 (
      Output : System.Address;
      Input  : System.Address;
      Len    : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_SHA3_512, "aegis_sha3_512");

   function Aegis_Keccak256 (
      Output : System.Address;
      Input  : System.Address;
      Len    : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Keccak256, "aegis_keccak256");

   function Aegis_SHAKE128 (
      Output     : System.Address;
      Output_Len : Interfaces.C.size_t;
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_SHAKE128, "aegis_shake128");

   function Aegis_SHAKE256 (
      Output     : System.Address;
      Output_Len : Interfaces.C.size_t;
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_SHAKE256, "aegis_shake256");

   ---------------------------------------------------------------------------
   --  Address Derivation (AAS-001 v3.1)
   ---------------------------------------------------------------------------

   --  Entity types
   AEGIS_ADDR_USER      : constant := Character'Pos ('u');
   AEGIS_ADDR_CONTRACT  : constant := Character'Pos ('c');
   AEGIS_ADDR_VALIDATOR : constant := Character'Pos ('v');
   AEGIS_ADDR_SYSTEM    : constant := Character'Pos ('s');

   --  Network types
   AEGIS_NET_MAIN    : constant := 0;
   AEGIS_NET_TEST    : constant := 1;
   AEGIS_NET_DEV     : constant := 2;
   AEGIS_NET_LAB     : constant := 3;
   AEGIS_NET_STAGING : constant := 4;

   --  Maximum address string length
   AEGIS_MAX_ADDRESS_STRING : constant := 96;

   function Aegis_Derive_Address (
      Address    : System.Address;
      Public_Key : System.Address;
      Addr_Type  : Interfaces.C.int
   ) return Result_Code;
   pragma Export (C, Aegis_Derive_Address, "aegis_derive_address");

   --  Format address to canonical AAS-001 v3.1 string
   function Aegis_Format_Address (
      Output     : System.Address;
      Length     : access Interfaces.C.size_t;
      Public_Key : System.Address;
      Network    : Interfaces.C.int;
      Addr_Type  : Interfaces.C.int
   ) return Result_Code;
   pragma Export (C, Aegis_Format_Address, "aegis_format_address");

   --  Parse canonical AAS-001 v3.1 address string
   function Aegis_Parse_Address (
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t;
      Account_ID : System.Address;
      Network    : access Interfaces.C.int;
      Addr_Type  : access Interfaces.C.int
   ) return Result_Code;
   pragma Export (C, Aegis_Parse_Address, "aegis_parse_address");

   ---------------------------------------------------------------------------
   --  VM Context Management
   ---------------------------------------------------------------------------

   --  Simplified create (no initial params)
   function Aegis_VM_Create (
      Handle : access VM_Handle
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Create, "aegis_vm_create");

   --  Create with options
   function Aegis_VM_Create_With_Options (
      Handle    : access VM_Handle;
      Gas_Limit : Interfaces.C.unsigned_long;
      Cert_Lvl  : Cert_Level
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Create_With_Options, "aegis_vm_create_with_options");

   function Aegis_VM_Set_Context (
      Handle       : VM_Handle;
      Origin       : System.Address;
      Caller       : System.Address;
      Address      : System.Address;
      Value        : System.Address;
      Block_Number : Interfaces.C.unsigned_long;
      Timestamp    : Interfaces.C.unsigned_long;
      Chain_ID     : Interfaces.C.unsigned_long
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Set_Context, "aegis_vm_set_context");

   --  Load bytecode into VM
   function Aegis_VM_Load_Code (
      Handle   : VM_Handle;
      Code     : System.Address;
      Code_Len : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Load_Code, "aegis_vm_load_code");

   --  Standard execute
   function Aegis_VM_Execute (
      Handle     : VM_Handle;
      Code       : System.Address;
      Code_Len   : Interfaces.C.size_t;
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t;
      Output     : System.Address;
      Output_Len : access Interfaces.C.size_t;
      Gas_Used   : access Interfaces.C.unsigned_long;
      Status     : access Exec_Status
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Execute, "aegis_vm_execute");

   --  Execute with full context
   function Aegis_VM_Execute_With_Context (
      Handle        : VM_Handle;
      Ctx           : access constant Exec_Context;
      Func          : System.Address;
      Func_Len      : Interfaces.C.size_t;
      Args          : System.Address;
      Args_Len      : Interfaces.C.size_t;
      State         : System.Address;
      State_Len     : Interfaces.C.size_t;
      Output        : System.Address;
      Output_Len    : access Interfaces.C.size_t;
      New_State     : System.Address;
      New_State_Len : access Interfaces.C.size_t;
      Gas_Used      : access Interfaces.C.unsigned_long
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Execute_With_Context, "aegis_vm_execute_with_context");

   --  Set state key-value
   function Aegis_VM_Set_State (
      Handle    : VM_Handle;
      Key       : System.Address;
      Key_Len   : Interfaces.C.size_t;
      Value     : System.Address;
      Value_Len : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Set_State, "aegis_vm_set_state");

   --  Get state value
   function Aegis_VM_Get_State (
      Handle    : VM_Handle;
      Key       : System.Address;
      Key_Len   : Interfaces.C.size_t;
      Value     : System.Address;
      Value_Len : access Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Get_State, "aegis_vm_get_state");

   --  Reset VM for reuse
   function Aegis_VM_Reset (Handle : VM_Handle) return Result_Code;
   pragma Export (C, Aegis_VM_Reset, "aegis_vm_reset");

   function Aegis_VM_Gas_Remaining (
      Handle : VM_Handle;
      Gas    : access Interfaces.C.unsigned_long
   ) return Result_Code;
   pragma Export (C, Aegis_VM_Gas_Remaining, "aegis_vm_gas_remaining");

   function Aegis_VM_Destroy (Handle : VM_Handle) return Result_Code;
   pragma Export (C, Aegis_VM_Destroy, "aegis_vm_destroy");

   --  Validate bytecode without execution
   function Aegis_Validate_Bytecode (
      Code     : System.Address;
      Code_Len : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Validate_Bytecode, "aegis_validate_bytecode");

   --  Estimate gas for execution
   function Aegis_Estimate_Gas (
      Code     : System.Address;
      Code_Len : Interfaces.C.size_t;
      Func     : System.Address;
      Func_Len : Interfaces.C.size_t;
      Args     : System.Address;
      Args_Len : Interfaces.C.size_t;
      Estimate : access Interfaces.C.unsigned_long
   ) return Result_Code;
   pragma Export (C, Aegis_Estimate_Gas, "aegis_estimate_gas");

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   function Aegis_Storage_Load (
      Handle  : VM_Handle;
      Address : System.Address;
      Key     : System.Address;
      Value   : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Storage_Load, "aegis_storage_load");

   function Aegis_Storage_Store (
      Handle  : VM_Handle;
      Address : System.Address;
      Key     : System.Address;
      Value   : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Storage_Store, "aegis_storage_store");

   ---------------------------------------------------------------------------
   --  State Root and Merkle Proofs
   ---------------------------------------------------------------------------

   function Aegis_Get_State_Root (
      Handle : VM_Handle;
      Root   : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Get_State_Root, "aegis_get_state_root");

   function Aegis_Get_Storage_Proof (
      Handle    : VM_Handle;
      Address   : System.Address;
      Key       : System.Address;
      Proof     : System.Address;
      Proof_Len : access Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Get_Storage_Proof, "aegis_get_storage_proof");

   function Aegis_Verify_Storage_Proof (
      Root      : System.Address;
      Address   : System.Address;
      Key       : System.Address;
      Value     : System.Address;
      Proof     : System.Address;
      Proof_Len : Interfaces.C.size_t;
      Valid     : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Verify_Storage_Proof, "aegis_verify_storage_proof");

   ---------------------------------------------------------------------------
   --  KHEPRI Registry Integration
   ---------------------------------------------------------------------------

   function Aegis_Get_Cert_Level (
      Handle  : VM_Handle;
      Address : System.Address;
      Level   : access Cert_Level
   ) return Result_Code;
   pragma Export (C, Aegis_Get_Cert_Level, "aegis_get_cert_level");

   function Aegis_Apply_Gas_Discount (
      Base_Gas : Interfaces.C.unsigned_long;
      Level    : Cert_Level;
      Result   : access Interfaces.C.unsigned_long
   ) return Result_Code;
   pragma Export (C, Aegis_Apply_Gas_Discount, "aegis_apply_gas_discount");

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   procedure Aegis_Secure_Zero (
      Ptr : System.Address;
      Len : Interfaces.C.size_t
   );
   pragma Export (C, Aegis_Secure_Zero, "aegis_secure_zero");

   function Aegis_Constant_Time_Compare (
      A   : System.Address;
      B   : System.Address;
      Len : Interfaces.C.size_t
   ) return Interfaces.C.int;
   pragma Export (C, Aegis_Constant_Time_Compare, "aegis_constant_time_compare");

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Shield (Encrypted State)
   ---------------------------------------------------------------------------

   function Aegis_Shield_Encrypt_State (
      Ciphertext     : System.Address;
      Ciphertext_Len : access Interfaces.C.size_t;
      Plaintext      : System.Address;
      Plaintext_Len  : Interfaces.C.size_t;
      Encaps_Key     : System.Address;
      KEM_Ciphertext : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Shield_Encrypt_State, "aegis_shield_encrypt_state");

   function Aegis_Shield_Decrypt_State (
      Plaintext      : System.Address;
      Plaintext_Len  : access Interfaces.C.size_t;
      Ciphertext     : System.Address;
      Ciphertext_Len : Interfaces.C.size_t;
      KEM_Ciphertext : System.Address;
      Decaps_Key     : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Shield_Decrypt_State, "aegis_shield_decrypt_state");

   function Aegis_Shield_Create_Commitment (
      Commitment : System.Address;
      Data       : System.Address;
      Data_Len   : Interfaces.C.size_t;
      Randomness : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Shield_Create_Commitment, "aegis_shield_create_commitment");

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Whisper (Confidential Transactions)
   ---------------------------------------------------------------------------

   function Aegis_Whisper_Create_Note (
      Note     : System.Address;
      Value    : Interfaces.C.unsigned_long;
      Blinding : System.Address;
      Owner_PK : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Whisper_Create_Note, "aegis_whisper_create_note");

   function Aegis_Whisper_Compute_Nullifier (
      Nullifier : System.Address;
      Note      : System.Address;
      Owner_SK  : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Whisper_Compute_Nullifier, "aegis_whisper_compute_nullifier");

   function Aegis_Whisper_Create_Range_Proof (
      Proof      : System.Address;
      Proof_Len  : access Interfaces.C.size_t;
      Commitment : System.Address;
      Value      : Interfaces.C.unsigned_long;
      Blinding   : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Whisper_Create_Range_Proof, "aegis_whisper_create_range_proof");

   function Aegis_Whisper_Verify_Range_Proof (
      Commitment : System.Address;
      Proof      : System.Address;
      Proof_Len  : Interfaces.C.size_t;
      Valid      : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Whisper_Verify_Range_Proof, "aegis_whisper_verify_range_proof");

   function Aegis_Whisper_Create_Balance_Proof (
      Proof              : System.Address;
      Proof_Len          : access Interfaces.C.size_t;
      Input_Commitments  : System.Address;
      Input_Count        : Interfaces.C.size_t;
      Output_Commitments : System.Address;
      Output_Count       : Interfaces.C.size_t;
      Fee                : Interfaces.C.unsigned_long;
      Input_Values       : System.Address;
      Input_Blindings    : System.Address;
      Output_Values      : System.Address;
      Output_Blindings   : System.Address
   ) return Result_Code;
   pragma Export (C, Aegis_Whisper_Create_Balance_Proof, "aegis_whisper_create_balance_proof");

   function Aegis_Whisper_Verify_Balance_Proof (
      Input_Commitments  : System.Address;
      Input_Count        : Interfaces.C.size_t;
      Output_Commitments : System.Address;
      Output_Count       : Interfaces.C.size_t;
      Fee                : Interfaces.C.unsigned_long;
      Proof              : System.Address;
      Proof_Len          : Interfaces.C.size_t;
      Valid              : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Whisper_Verify_Balance_Proof, "aegis_whisper_verify_balance_proof");

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Eye (Selective Disclosure)
   ---------------------------------------------------------------------------

   function Aegis_Eye_Derive_Viewing_Key (
      Viewing_Key : System.Address;
      Master_SK   : System.Address;
      View_Type   : Interfaces.C.int;
      Valid_Until : Interfaces.C.unsigned_long
   ) return Result_Code;
   pragma Export (C, Aegis_Eye_Derive_Viewing_Key, "aegis_eye_derive_viewing_key");

   function Aegis_Eye_Create_Disclosure_Proof (
      Proof          : System.Address;
      Proof_Len      : access Interfaces.C.size_t;
      Viewing_Key    : System.Address;
      Attribute_Mask : Interfaces.C.unsigned_long;
      Attributes     : System.Address;
      Attr_Count     : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Eye_Create_Disclosure_Proof, "aegis_eye_create_disclosure_proof");

   function Aegis_Eye_Verify_Disclosure (
      Owner_PK       : System.Address;
      Attribute_Mask : Interfaces.C.unsigned_long;
      Attributes     : System.Address;
      Attr_Count     : Interfaces.C.size_t;
      Proof          : System.Address;
      Proof_Len      : Interfaces.C.size_t;
      Valid          : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Eye_Verify_Disclosure, "aegis_eye_verify_disclosure");

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Gate (Private Execution)
   ---------------------------------------------------------------------------

   function Aegis_Gate_Init_Session (
      Session       : System.Address;
      Contract_Addr : System.Address;
      Initiator_PK  : System.Address;
      Contract_PK   : System.Address;
      Mode          : Interfaces.C.int;
      Max_Calls     : Interfaces.C.unsigned_long;
      TTL_Blocks    : Interfaces.C.unsigned_long
   ) return Result_Code;
   pragma Export (C, Aegis_Gate_Init_Session, "aegis_gate_init_session");

   function Aegis_Gate_Encrypt_Inputs (
      Encrypted     : System.Address;
      Encrypted_Len : access Interfaces.C.size_t;
      Session       : System.Address;
      Func          : System.Address;
      Args          : System.Address;
      Args_Len      : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Gate_Encrypt_Inputs, "aegis_gate_encrypt_inputs");

   function Aegis_Gate_Decrypt_Outputs (
      Plaintext     : System.Address;
      Plaintext_Len : access Interfaces.C.size_t;
      Session       : System.Address;
      Encrypted     : System.Address;
      Encrypted_Len : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Gate_Decrypt_Outputs, "aegis_gate_decrypt_outputs");

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Veil (ZK Proofs)
   ---------------------------------------------------------------------------

   function Aegis_Veil_Verify_STARK_Proof (
      Code_Hash      : System.Address;
      Old_State_Root : System.Address;
      New_State_Root : System.Address;
      Public_Inputs  : System.Address;
      Inputs_Len     : Interfaces.C.size_t;
      Public_Outputs : System.Address;
      Outputs_Len    : Interfaces.C.size_t;
      Proof          : System.Address;
      Proof_Len      : Interfaces.C.size_t;
      Valid          : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Veil_Verify_STARK_Proof, "aegis_veil_verify_stark_proof");

   function Aegis_Veil_Verify_Lattice_Proof (
      Statement : System.Address;
      Stmt_Len  : Interfaces.C.size_t;
      Proof     : System.Address;
      Proof_Len : Interfaces.C.size_t;
      Valid     : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Veil_Verify_Lattice_Proof, "aegis_veil_verify_lattice_proof");

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - KHNUM Signature Aggregation
   ---------------------------------------------------------------------------

   function Aegis_Khnum_Aggregate_Signatures (
      Proof      : System.Address;
      Proof_Len  : access Interfaces.C.size_t;
      Signatures : System.Address;
      Public_Keys : System.Address;
      Messages   : System.Address;
      Count      : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Khnum_Aggregate_Signatures, "aegis_khnum_aggregate_signatures");

   function Aegis_Khnum_Verify_Aggregated (
      Proof      : System.Address;
      Proof_Len  : Interfaces.C.size_t;
      Public_Keys : System.Address;
      Messages   : System.Address;
      Count      : Interfaces.C.size_t;
      Valid      : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Khnum_Verify_Aggregated, "aegis_khnum_verify_aggregated");

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - SEBEK Threshold Signatures
   ---------------------------------------------------------------------------

   --  SEBEK key share type
   type Aegis_Sebek_Share_T is record
      Share      : Byte_Array (0 .. 255);
      Index      : Interfaces.C.unsigned;
      Commitment : Hash256;
   end record;
   pragma Convention (C, Aegis_Sebek_Share_T);

   function Aegis_Sebek_Sign_Partial (
      Partial_Sig : System.Address;
      Sig_Len     : access Interfaces.C.size_t;
      Share       : System.Address;
      Message     : System.Address;
      Msg_Len     : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Sebek_Sign_Partial, "aegis_sebek_sign_partial");

   function Aegis_Sebek_Combine_Signatures (
      Combined_Sig : System.Address;
      Sig_Len      : access Interfaces.C.size_t;
      Partials     : System.Address;
      Indices      : System.Address;
      Count        : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Sebek_Combine_Signatures, "aegis_sebek_combine_signatures");

   function Aegis_Sebek_Verify (
      Signature   : System.Address;
      Sig_Len     : Interfaces.C.size_t;
      Message     : System.Address;
      Msg_Len     : Interfaces.C.size_t;
      Combined_PK : System.Address;
      Valid       : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Sebek_Verify, "aegis_sebek_verify");

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - MAAT Hierarchical Aggregation
   ---------------------------------------------------------------------------

   --  MAAT array types
   type Batch_Commit_Array is array (0 .. 15) of Hash256;
   pragma Convention (C, Batch_Commit_Array);

   type Block_Root_Array is array (0 .. 99) of Hash256;
   pragma Convention (C, Block_Root_Array);

   type Aggregated_Proof_Array is array (0 .. 32767) of Interfaces.C.unsigned_char;
   pragma Convention (C, Aggregated_Proof_Array);

   type Epoch_Proof_Array is array (0 .. 4095) of Interfaces.C.unsigned_char;
   pragma Convention (C, Epoch_Proof_Array);

   --  MAAT block proof type
   type Aegis_Maat_Block_Proof_T is record
      Batch_Commits    : Batch_Commit_Array;
      Aggregated_Proof : Aggregated_Proof_Array;
      Proof_Len        : Interfaces.C.size_t;
      Block_Height     : Interfaces.C.unsigned_long;
   end record;
   pragma Convention (C, Aegis_Maat_Block_Proof_T);

   --  MAAT epoch proof type
   type Aegis_Maat_Epoch_Proof_T is record
      Block_Roots  : Block_Root_Array;
      Epoch_Root   : Hash256;
      State_Root   : Hash256;
      Epoch_Number : Interfaces.C.unsigned_long;
      Proof_Data   : Epoch_Proof_Array;
      Proof_Len    : Interfaces.C.size_t;
   end record;
   pragma Convention (C, Aegis_Maat_Epoch_Proof_T);

   function Aegis_Maat_Generate_Block_Proof (
      Proof        : System.Address;
      Signatures   : System.Address;
      Public_Keys  : System.Address;
      TX_Hashes    : System.Address;
      Block_Height : Interfaces.Unsigned_64
   ) return Result_Code;
   pragma Export (C, Aegis_Maat_Generate_Block_Proof, "aegis_maat_generate_block_proof");

   function Aegis_Maat_Verify_Block_Proof (
      Proof       : System.Address;
      Public_Keys : System.Address;
      Tx_Hashes   : System.Address;
      Valid       : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Maat_Verify_Block_Proof, "aegis_maat_verify_block_proof");

   function Aegis_Maat_Verify_Epoch_Proof (
      Proof          : System.Address;
      Prev_State     : System.Address;
      Expected_State : System.Address;
      Valid          : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Maat_Verify_Epoch_Proof, "aegis_maat_verify_epoch_proof");

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - TEFNUT Light Client Proofs
   ---------------------------------------------------------------------------

   --  Light client profile type
   type Aegis_Tefnut_Profile_T is new Interfaces.C.int;

   --  Tefnut proof data array type
   type Tefnut_Proof_Array is array (0 .. 1023) of Interfaces.C.unsigned_char;
   pragma Convention (C, Tefnut_Proof_Array);

   --  Light client checkpoint type
   type Aegis_Tefnut_Checkpoint_T is record
      Block_Height   : Interfaces.C.unsigned_long;
      State_Root     : Hash256;
      Committee_Root : Hash256;
      Proof_Data     : Tefnut_Proof_Array;
      Proof_Len      : Interfaces.C.size_t;
   end record;
   pragma Convention (C, Aegis_Tefnut_Checkpoint_T);

   function Aegis_Tefnut_Generate_Proof (
      Proof        : System.Address;
      Proof_Len    : access Interfaces.C.size_t;
      Profile      : Interfaces.C.int;
      Checkpoint   : System.Address;
      Query_Key    : System.Address;
      Query_Value  : System.Address;
      Value_Len    : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Tefnut_Generate_Proof, "aegis_tefnut_generate_proof");

   function Aegis_Tefnut_Verify_Proof (
      Checkpoint  : System.Address;
      Query_Key   : System.Address;
      Query_Value : System.Address;
      Value_Len   : Interfaces.C.size_t;
      Proof       : System.Address;
      Proof_Len   : Interfaces.C.size_t;
      Valid       : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Tefnut_Verify_Proof, "aegis_tefnut_verify_proof");

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - SEKHMET SIMD Acceleration
   ---------------------------------------------------------------------------

   function Aegis_Sekhmet_Detect_Backend return Interfaces.C.int;
   pragma Export (C, Aegis_Sekhmet_Detect_Backend, "aegis_sekhmet_detect_backend");

   function Aegis_Sekhmet_NTT (
      Data    : System.Address;
      Size    : Interfaces.C.size_t;
      Inverse : Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Sekhmet_NTT, "aegis_sekhmet_ntt");

   function Aegis_Sekhmet_Poly_Mul (
      Result_Arr : System.Address;
      A          : System.Address;
      B          : System.Address;
      Size       : Interfaces.C.size_t
   ) return Result_Code;
   pragma Export (C, Aegis_Sekhmet_Poly_Mul, "aegis_sekhmet_poly_mul");

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - KHEPRI Native Contract Deployment
   ---------------------------------------------------------------------------

   --  KHEPRI deploy result type
   type Aegis_Khepri_Deploy_Result_T is record
      Contract_Addr  : Hash256;
      Code_Hash      : Hash256;
      Cert_Lvl       : Cert_Level;
      Gas_Used       : Interfaces.C.unsigned_long;
   end record;
   pragma Convention (C, Aegis_Khepri_Deploy_Result_T);

   function Aegis_Khepri_Deploy (
      Result_Ptr : System.Address;
      ELF_Binary : System.Address;
      ELF_Len    : Interfaces.C.size_t;
      Proof_Hash : System.Address;
      Cert_Level : Interfaces.C.int;
      Deployer   : System.Address;
      Gas_Limit  : Interfaces.Unsigned_64
   ) return Result_Code;
   pragma Export (C, Aegis_Khepri_Deploy, "aegis_khepri_deploy");

   function Aegis_Khepri_Verify_Certification (
      Code_Hash     : System.Address;
      Proof_Hash    : System.Address;
      Claimed_Level : Interfaces.C.int;
      Verified      : access Interfaces.C.C_bool
   ) return Result_Code;
   pragma Export (C, Aegis_Khepri_Verify_Certification, "aegis_khepri_verify_certification");

private

   --  Maximum bytecode size (512KB)
   Max_Bytecode_Size : constant := 512 * 1024;

   --  Maximum state entries
   Max_State_Entries : constant := 1024;

   --  State entry record
   type State_Entry is record
      Key       : Byte_Array (0 .. 255);
      Key_Len   : Natural := 0;
      Value     : Byte_Array (0 .. 32 * 1024 - 1);  --  32KB max value
      Value_Len : Natural := 0;
      Used      : Boolean := False;
   end record;
   pragma Convention (C, State_Entry);

   type State_Table is array (0 .. Max_State_Entries - 1) of State_Entry;

   --  Bytecode storage
   type Bytecode_Buffer is array (0 .. Max_Bytecode_Size - 1)
     of Interfaces.C.unsigned_char;

   --  VM context internal structure
   type VM_Context is record
      --  Transaction info
      Origin       : Hash256;
      Caller       : Hash256;
      Address      : Hash256;
      Value        : U256;
      Block_Number : Interfaces.C.unsigned_long;
      Timestamp    : Interfaces.C.unsigned_long;
      Chain_ID     : Interfaces.C.unsigned_long;

      --  Gas metering
      Gas_Limit     : Interfaces.C.unsigned_long;
      Gas_Used      : Interfaces.C.unsigned_long;
      Certification : Cert_Level;

      --  Bytecode
      Bytecode     : access Bytecode_Buffer;
      Bytecode_Len : Interfaces.C.size_t := 0;

      --  State storage
      State        : access State_Table;

      --  Flags
      Initialized   : Boolean := False;
      Code_Loaded   : Boolean := False;
   end record;
   pragma Convention (C, VM_Context);

end Aegis_FFI;
