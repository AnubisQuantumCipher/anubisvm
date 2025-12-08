/**
 * AegisVM C Foreign Function Interface
 *
 * This header provides the C interface to the SPARK/Ada AegisVM implementation.
 * It enables integration with the Cosmos SDK via CGO while maintaining the
 * formal verification guarantees of the underlying SPARK code.
 *
 * Components:
 * - Post-quantum cryptography (ML-DSA-87, ML-KEM-1024)
 * - SHA3/SHAKE hashing
 * - VM execution context management
 * - Contract state operations
 *
 * References:
 * - FIPS 203 (ML-KEM)
 * - FIPS 204 (ML-DSA)
 * - FIPS 202 (SHA3/SHAKE)
 * - KHEPRI Blueprint v1.0
 *
 * Thread Safety: All functions are reentrant and thread-safe.
 * Memory: Caller manages all buffers; functions do not allocate.
 */

#ifndef AEGISVM_H
#define AEGISVM_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================
 * Type Definitions
 *===========================================================================*/

/* Result codes for all operations */
typedef enum {
    AEGIS_OK = 0,
    AEGIS_ERROR_INVALID_INPUT = 1,
    AEGIS_ERROR_INVALID_SIGNATURE = 2,
    AEGIS_ERROR_DECAPS_FAILURE = 3,
    AEGIS_ERROR_OUT_OF_GAS = 4,
    AEGIS_ERROR_OUT_OF_MEMORY = 5,
    AEGIS_ERROR_INVALID_STATE = 6,
    AEGIS_ERROR_ACCESS_VIOLATION = 7,
    AEGIS_ERROR_STACK_OVERFLOW = 8,
    AEGIS_ERROR_INVALID_OPCODE = 9,
    AEGIS_ERROR_REVERTED = 10,
    AEGIS_ERROR_INVALID_BYTECODE = 11,
    AEGIS_ERROR_UNKNOWN = 255
} aegis_result_t;

/* Certification levels for gas discounts */
typedef enum {
    AEGIS_CERT_BRONZE = 0,    /* No discount */
    AEGIS_CERT_SILVER = 1,    /* 10% discount */
    AEGIS_CERT_GOLD = 2,      /* 20% discount */
    AEGIS_CERT_PLATINUM = 3   /* 30% discount */
} aegis_cert_level_t;

/* Execution status */
typedef enum {
    AEGIS_EXEC_SUCCESS = 0,
    AEGIS_EXEC_REVERT = 1,
    AEGIS_EXEC_OUT_OF_GAS = 2,
    AEGIS_EXEC_STACK_OVERFLOW = 3,
    AEGIS_EXEC_INVALID_OPCODE = 4,
    AEGIS_EXEC_ACCESS_VIOLATION = 5,
    AEGIS_EXEC_CONTRACT_ERROR = 6
} aegis_exec_status_t;

/* 256-bit unsigned integer (little-endian limbs) */
typedef struct {
    uint64_t limbs[4];
} aegis_u256_t;

/* 32-byte hash (SHA3-256, addresses, etc.) */
typedef struct {
    uint8_t bytes[32];
} aegis_hash256_t;

/* 64-byte hash (SHA3-512, etc.) */
typedef struct {
    uint8_t bytes[64];
} aegis_hash512_t;

/* Contract address (32 bytes, derived from ML-DSA-87 public key) */
typedef aegis_hash256_t aegis_address_t;

/* Opaque VM context handle */
typedef struct aegis_vm_context* aegis_vm_handle_t;

/* Alternate name for compatibility */
typedef aegis_vm_handle_t aegis_vm_t;

/* Execution context for VM calls */
typedef struct {
    uint8_t origin[32];         /* Transaction origin */
    uint8_t caller[32];         /* Immediate caller */
    uint64_t gas_limit;         /* Maximum gas */
    uint64_t gas_price;         /* Gas price in uaegis */
    uint64_t value;             /* Value transferred */
    int64_t block_height;       /* Current block height */
    int64_t block_time;         /* Block timestamp */
    aegis_cert_level_t cert_level; /* Certification level */
    int is_static;              /* Read-only call flag */
} aegis_exec_context_t;

/*===========================================================================
 * Cryptographic Constants
 *===========================================================================*/

/* ML-DSA-87 (FIPS 204) sizes */
#define AEGIS_MLDSA87_PUBLIC_KEY_SIZE   2592
#define AEGIS_MLDSA87_SECRET_KEY_SIZE   4896
#define AEGIS_MLDSA87_SIGNATURE_SIZE    4627

/* ML-KEM-1024 (FIPS 203) sizes */
#define AEGIS_MLKEM1024_PUBLIC_KEY_SIZE   1568
#define AEGIS_MLKEM1024_SECRET_KEY_SIZE   3168
#define AEGIS_MLKEM1024_CIPHERTEXT_SIZE   1568
#define AEGIS_MLKEM1024_SHARED_SECRET_SIZE 32

/* Hash sizes */
#define AEGIS_SHA3_256_SIZE  32
#define AEGIS_SHA3_512_SIZE  64

/*===========================================================================
 * ML-DSA-87 Digital Signatures (FIPS 204)
 *===========================================================================*/

/**
 * Generate ML-DSA-87 key pair.
 *
 * @param[out] public_key  Buffer for public key (2592 bytes)
 * @param[out] secret_key  Buffer for secret key (4896 bytes)
 * @param[in]  seed        32-byte random seed (optional, NULL for internal RNG)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_mldsa87_keygen(
    uint8_t public_key[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    uint8_t secret_key[AEGIS_MLDSA87_SECRET_KEY_SIZE],
    const uint8_t seed[32]
);

/**
 * Sign message with ML-DSA-87.
 *
 * @param[out] signature   Buffer for signature (4627 bytes)
 * @param[out] sig_len     Actual signature length
 * @param[in]  message     Message to sign
 * @param[in]  msg_len     Message length
 * @param[in]  secret_key  Secret key (4896 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_mldsa87_sign(
    uint8_t signature[AEGIS_MLDSA87_SIGNATURE_SIZE],
    size_t* sig_len,
    const uint8_t* message,
    size_t msg_len,
    const uint8_t secret_key[AEGIS_MLDSA87_SECRET_KEY_SIZE]
);

/**
 * Verify ML-DSA-87 signature.
 *
 * @param[in]  signature   Signature to verify (4627 bytes)
 * @param[in]  sig_len     Signature length
 * @param[in]  message     Original message
 * @param[in]  msg_len     Message length
 * @param[in]  public_key  Public key (2592 bytes)
 * @param[out] valid       True if signature is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_mldsa87_verify(
    const uint8_t signature[AEGIS_MLDSA87_SIGNATURE_SIZE],
    size_t sig_len,
    const uint8_t* message,
    size_t msg_len,
    const uint8_t public_key[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    bool* valid
);

/*===========================================================================
 * ML-KEM-1024 Key Encapsulation (FIPS 203)
 *===========================================================================*/

/**
 * Generate ML-KEM-1024 key pair.
 *
 * @param[out] encaps_key  Buffer for encapsulation key (1568 bytes)
 * @param[out] decaps_key  Buffer for decapsulation key (3168 bytes)
 * @param[in]  seed        64-byte random seed (optional, NULL for internal RNG)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_mlkem1024_keygen(
    uint8_t encaps_key[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE],
    uint8_t decaps_key[AEGIS_MLKEM1024_SECRET_KEY_SIZE],
    const uint8_t seed[64]
);

/**
 * Encapsulate shared secret with ML-KEM-1024.
 *
 * @param[out] ciphertext     Buffer for ciphertext (1568 bytes)
 * @param[out] shared_secret  Buffer for shared secret (32 bytes)
 * @param[in]  encaps_key     Encapsulation key (1568 bytes)
 * @param[in]  seed           32-byte random seed (optional, NULL for internal RNG)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_mlkem1024_encaps(
    uint8_t ciphertext[AEGIS_MLKEM1024_CIPHERTEXT_SIZE],
    uint8_t shared_secret[AEGIS_MLKEM1024_SHARED_SECRET_SIZE],
    const uint8_t encaps_key[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE],
    const uint8_t seed[32]
);

/**
 * Decapsulate shared secret with ML-KEM-1024.
 *
 * @param[out] shared_secret  Buffer for shared secret (32 bytes)
 * @param[in]  ciphertext     Ciphertext (1568 bytes)
 * @param[in]  decaps_key     Decapsulation key (3168 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_mlkem1024_decaps(
    uint8_t shared_secret[AEGIS_MLKEM1024_SHARED_SECRET_SIZE],
    const uint8_t ciphertext[AEGIS_MLKEM1024_CIPHERTEXT_SIZE],
    const uint8_t decaps_key[AEGIS_MLKEM1024_SECRET_KEY_SIZE]
);

/*===========================================================================
 * SHA3/SHAKE Hash Functions (FIPS 202)
 *===========================================================================*/

/**
 * Compute SHA3-256 hash.
 *
 * @param[out] output  Buffer for hash output (32 bytes)
 * @param[in]  input   Input data
 * @param[in]  len     Input length in bytes
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_sha3_256(
    uint8_t output[AEGIS_SHA3_256_SIZE],
    const uint8_t* input,
    size_t len
);

/**
 * Compute SHA3-512 hash.
 *
 * @param[out] output  Buffer for hash output (64 bytes)
 * @param[in]  input   Input data
 * @param[in]  len     Input length in bytes
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_sha3_512(
    uint8_t output[AEGIS_SHA3_512_SIZE],
    const uint8_t* input,
    size_t len
);

/**
 * Compute Keccak-256 hash (Ethereum-compatible).
 *
 * @param[out] output  Buffer for hash output (32 bytes)
 * @param[in]  input   Input data
 * @param[in]  len     Input length in bytes
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_keccak256(
    uint8_t output[AEGIS_SHA3_256_SIZE],
    const uint8_t* input,
    size_t len
);

/**
 * Compute SHAKE128 XOF.
 *
 * @param[out] output      Buffer for output
 * @param[in]  output_len  Desired output length
 * @param[in]  input       Input data
 * @param[in]  input_len   Input length in bytes
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_shake128(
    uint8_t* output,
    size_t output_len,
    const uint8_t* input,
    size_t input_len
);

/**
 * Compute SHAKE256 XOF.
 *
 * @param[out] output      Buffer for output
 * @param[in]  output_len  Desired output length
 * @param[in]  input       Input data
 * @param[in]  input_len   Input length in bytes
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_shake256(
    uint8_t* output,
    size_t output_len,
    const uint8_t* input,
    size_t input_len
);

/*===========================================================================
 * Address Derivation
 *===========================================================================*/

/* Address entity types */
typedef enum {
    AEGIS_ADDR_USER = 'u',      /* User account */
    AEGIS_ADDR_CONTRACT = 'c',  /* Contract */
    AEGIS_ADDR_VALIDATOR = 'v', /* Validator */
    AEGIS_ADDR_SYSTEM = 's'     /* System/precompile */
} aegis_addr_type_t;

/* Network types for AAS-001 canonical addresses */
typedef enum {
    AEGIS_NET_MAIN = 0,     /* Main network */
    AEGIS_NET_TEST = 1,     /* Test network */
    AEGIS_NET_DEV = 2,      /* Development network */
    AEGIS_NET_LAB = 3,      /* Lab network */
    AEGIS_NET_STAGING = 4   /* Staging network */
} aegis_network_t;

/* Maximum length for canonical address string (AAS-001 v3.1) */
/* Format: mldsa87:staging:u:xxxxxxxx-xxxxxxxx-xxxxxxxx-xxxxxxxx-xxxxxxxx-xxxxxxxx-xxxx-xxxxx */
#define AEGIS_MAX_ADDRESS_STRING 96

/**
 * Derive address from ML-DSA-87 public key.
 *
 * Uses domain separator: "aegis-v1-mldsa87-<type>"
 *
 * @param[out] address     Buffer for address (32 bytes)
 * @param[in]  public_key  ML-DSA-87 public key (2592 bytes)
 * @param[in]  type        Address entity type
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_derive_address(
    aegis_address_t* address,
    const uint8_t public_key[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    aegis_addr_type_t type
);

/**
 * Format address to canonical AAS-001 v3.1 string.
 *
 * Output format: mldsa87:network:type:chunked_payload-checksum
 * Example: mldsa87:main:u:dztd939y-b16ybkqa-12a55nb1-zyb1f50k-nn08yc11-fskf8sfs-0vsg-bg0bc
 *
 * @param[out] output      Buffer for address string (at least AEGIS_MAX_ADDRESS_STRING bytes)
 * @param[out] length      Actual length of the address string
 * @param[in]  public_key  ML-DSA-87 public key (2592 bytes)
 * @param[in]  network     Network type (main, test, dev, lab, staging)
 * @param[in]  type        Address entity type (u, c, v, s)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_format_address(
    char* output,
    size_t* length,
    const uint8_t public_key[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    aegis_network_t network,
    aegis_addr_type_t type
);

/**
 * Parse canonical AAS-001 v3.1 address string.
 *
 * Validates format, checksums, and extracts components.
 *
 * @param[in]  input       Address string to parse
 * @param[in]  input_len   Length of address string
 * @param[out] account_id  Extracted 32-byte account ID
 * @param[out] network     Extracted network type
 * @param[out] type        Extracted entity type
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_parse_address(
    const char* input,
    size_t input_len,
    aegis_address_t* account_id,
    aegis_network_t* network,
    aegis_addr_type_t* type
);

/*===========================================================================
 * VM Context Management
 *===========================================================================*/

/**
 * Create a new VM execution context.
 *
 * @param[out] handle  Pointer to receive VM handle
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_create(
    aegis_vm_handle_t* handle
);

/**
 * Create a new VM execution context with options.
 *
 * @param[out] handle       Pointer to receive VM handle
 * @param[in]  gas_limit    Maximum gas for execution
 * @param[in]  cert_level   Certification level for gas discounts
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_create_with_options(
    aegis_vm_handle_t* handle,
    uint64_t gas_limit,
    aegis_cert_level_t cert_level
);

/**
 * Set VM context parameters.
 *
 * @param[in] handle        VM handle
 * @param[in] origin        Transaction origin address
 * @param[in] caller        Current caller address
 * @param[in] address       Current contract address
 * @param[in] value         Call value
 * @param[in] block_number  Current block number
 * @param[in] timestamp     Current timestamp
 * @param[in] chain_id      Chain ID
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_set_context(
    aegis_vm_handle_t handle,
    const aegis_address_t* origin,
    const aegis_address_t* caller,
    const aegis_address_t* address,
    const aegis_u256_t* value,
    uint64_t block_number,
    uint64_t timestamp,
    uint64_t chain_id
);

/**
 * Execute contract bytecode.
 *
 * @param[in]     handle      VM handle
 * @param[in]     code        Contract bytecode
 * @param[in]     code_len    Bytecode length
 * @param[in]     input       Call input data
 * @param[in]     input_len   Input length
 * @param[out]    output      Buffer for return data
 * @param[in,out] output_len  In: buffer size, Out: actual output length
 * @param[out]    gas_used    Gas consumed
 * @param[out]    status      Execution status
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_execute(
    aegis_vm_handle_t handle,
    const uint8_t* code,
    size_t code_len,
    const uint8_t* input,
    size_t input_len,
    uint8_t* output,
    size_t* output_len,
    uint64_t* gas_used,
    aegis_exec_status_t* status
);

/**
 * Get remaining gas in VM context.
 *
 * @param[in]  handle  VM handle
 * @param[out] gas     Remaining gas
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_gas_remaining(
    aegis_vm_handle_t handle,
    uint64_t* gas
);

/**
 * Destroy VM context and free resources.
 *
 * @param[in] handle  VM handle
 * @return AEGIS_OK on success
 */
aegis_result_t aegis_vm_destroy(
    aegis_vm_handle_t handle
);

/**
 * Load bytecode into VM for execution.
 *
 * @param[in] handle    VM handle
 * @param[in] code      Contract bytecode
 * @param[in] code_len  Bytecode length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_load_code(
    aegis_vm_handle_t handle,
    const uint8_t* code,
    size_t code_len
);

/**
 * Execute loaded bytecode with context.
 *
 * @param[in]     handle       VM handle
 * @param[in]     ctx          Execution context
 * @param[in]     function     Function selector (4 bytes)
 * @param[in]     func_len     Function selector length
 * @param[in]     args         Function arguments
 * @param[in]     args_len     Arguments length
 * @param[in]     state        Current state data
 * @param[in]     state_len    State length
 * @param[out]    output       Return data buffer
 * @param[in,out] output_len   In: buffer size, Out: actual output length
 * @param[out]    new_state    New state buffer
 * @param[in,out] new_state_len In: buffer size, Out: actual state length
 * @param[out]    gas_used     Gas consumed
 * @return Execution status code
 */
aegis_result_t aegis_vm_execute_with_context(
    aegis_vm_handle_t handle,
    const aegis_exec_context_t* ctx,
    const uint8_t* function,
    size_t func_len,
    const uint8_t* args,
    size_t args_len,
    const uint8_t* state,
    size_t state_len,
    uint8_t* output,
    size_t* output_len,
    uint8_t* new_state,
    size_t* new_state_len,
    uint64_t* gas_used
);

/**
 * Set state for VM execution.
 *
 * @param[in] handle     VM handle
 * @param[in] key        State key
 * @param[in] key_len    Key length
 * @param[in] value      State value
 * @param[in] value_len  Value length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_set_state(
    aegis_vm_handle_t handle,
    const uint8_t* key,
    size_t key_len,
    const uint8_t* value,
    size_t value_len
);

/**
 * Get state from VM.
 *
 * @param[in]     handle     VM handle
 * @param[in]     key        State key
 * @param[in]     key_len    Key length
 * @param[out]    value      Value buffer
 * @param[in,out] value_len  In: buffer size, Out: actual value length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_get_state(
    aegis_vm_handle_t handle,
    const uint8_t* key,
    size_t key_len,
    uint8_t* value,
    size_t* value_len
);

/**
 * Reset VM state for reuse.
 *
 * @param[in] handle  VM handle
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_vm_reset(
    aegis_vm_handle_t handle
);

/**
 * Validate bytecode without executing.
 *
 * @param[in] code      Bytecode to validate
 * @param[in] code_len  Bytecode length
 * @return AEGIS_OK if valid, error code otherwise
 */
aegis_result_t aegis_validate_bytecode(
    const uint8_t* code,
    size_t code_len
);

/**
 * Estimate gas consumption for execution.
 *
 * @param[in]  code        Contract bytecode
 * @param[in]  code_len    Bytecode length
 * @param[in]  function    Function selector
 * @param[in]  func_len    Function selector length
 * @param[in]  args        Function arguments
 * @param[in]  args_len    Arguments length
 * @param[out] gas_estimate Estimated gas consumption
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_estimate_gas(
    const uint8_t* code,
    size_t code_len,
    const uint8_t* function,
    size_t func_len,
    const uint8_t* args,
    size_t args_len,
    uint64_t* gas_estimate
);

/*===========================================================================
 * Storage Operations
 *===========================================================================*/

/**
 * Load value from contract storage.
 *
 * @param[in]  handle   VM handle
 * @param[in]  address  Contract address
 * @param[in]  key      Storage key (256-bit)
 * @param[out] value    Storage value (256-bit)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_storage_load(
    aegis_vm_handle_t handle,
    const aegis_address_t* address,
    const aegis_u256_t* key,
    aegis_u256_t* value
);

/**
 * Store value to contract storage.
 *
 * @param[in] handle   VM handle
 * @param[in] address  Contract address
 * @param[in] key      Storage key (256-bit)
 * @param[in] value    Storage value (256-bit)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_storage_store(
    aegis_vm_handle_t handle,
    const aegis_address_t* address,
    const aegis_u256_t* key,
    const aegis_u256_t* value
);

/*===========================================================================
 * State Root and Merkle Proofs
 *===========================================================================*/

/**
 * Compute current state root.
 *
 * @param[in]  handle  VM handle
 * @param[out] root    State root hash (32 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_get_state_root(
    aegis_vm_handle_t handle,
    aegis_hash256_t* root
);

/**
 * Generate Merkle proof for storage slot.
 *
 * @param[in]     handle     VM handle
 * @param[in]     address    Contract address
 * @param[in]     key        Storage key
 * @param[out]    proof      Buffer for proof data
 * @param[in,out] proof_len  In: buffer size, Out: actual proof length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_get_storage_proof(
    aegis_vm_handle_t handle,
    const aegis_address_t* address,
    const aegis_u256_t* key,
    uint8_t* proof,
    size_t* proof_len
);

/**
 * Verify Merkle proof for storage slot.
 *
 * @param[in]  root       Expected state root
 * @param[in]  address    Contract address
 * @param[in]  key        Storage key
 * @param[in]  value      Expected value
 * @param[in]  proof      Proof data
 * @param[in]  proof_len  Proof length
 * @param[out] valid      True if proof is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_verify_storage_proof(
    const aegis_hash256_t* root,
    const aegis_address_t* address,
    const aegis_u256_t* key,
    const aegis_u256_t* value,
    const uint8_t* proof,
    size_t proof_len,
    bool* valid
);

/*===========================================================================
 * KHEPRI Registry Integration
 *===========================================================================*/

/**
 * Get contract certification level.
 *
 * @param[in]  handle   VM handle
 * @param[in]  address  Contract address
 * @param[out] level    Certification level
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_get_cert_level(
    aegis_vm_handle_t handle,
    const aegis_address_t* address,
    aegis_cert_level_t* level
);

/**
 * Calculate gas with certification discount.
 *
 * @param[in]  base_gas  Base gas amount
 * @param[in]  level     Certification level
 * @param[out] result    Discounted gas amount
 * @return AEGIS_OK on success
 */
aegis_result_t aegis_apply_gas_discount(
    uint64_t base_gas,
    aegis_cert_level_t level,
    uint64_t* result
);

/*===========================================================================
 * ANUBIS Privacy Layer - Shield (Encrypted State)
 *===========================================================================*/

/* Shield constants */
#define AEGIS_SHIELD_COMMITMENT_SIZE     64
#define AEGIS_SHIELD_ENCRYPTED_STATE_OVERHEAD 48  /* 12-byte nonce + 16-byte tag + 20-byte header */
#define AEGIS_SHIELD_MAX_STATE_SIZE      65536

/**
 * Encrypt contract state using ML-KEM derived key.
 *
 * @param[out] ciphertext     Buffer for encrypted state
 * @param[out] ciphertext_len Actual ciphertext length
 * @param[in]  plaintext      State data to encrypt
 * @param[in]  plaintext_len  Plaintext length
 * @param[in]  encaps_key     ML-KEM-1024 encapsulation key (1568 bytes)
 * @param[out] kem_ciphertext Buffer for KEM ciphertext (1568 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_shield_encrypt_state(
    uint8_t* ciphertext,
    size_t* ciphertext_len,
    const uint8_t* plaintext,
    size_t plaintext_len,
    const uint8_t encaps_key[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE],
    uint8_t kem_ciphertext[AEGIS_MLKEM1024_CIPHERTEXT_SIZE]
);

/**
 * Decrypt contract state using ML-KEM secret key.
 *
 * @param[out] plaintext      Buffer for decrypted state
 * @param[out] plaintext_len  Actual plaintext length
 * @param[in]  ciphertext     Encrypted state data
 * @param[in]  ciphertext_len Ciphertext length
 * @param[in]  kem_ciphertext KEM ciphertext (1568 bytes)
 * @param[in]  decaps_key     ML-KEM-1024 decapsulation key (3168 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_shield_decrypt_state(
    uint8_t* plaintext,
    size_t* plaintext_len,
    const uint8_t* ciphertext,
    size_t ciphertext_len,
    const uint8_t kem_ciphertext[AEGIS_MLKEM1024_CIPHERTEXT_SIZE],
    const uint8_t decaps_key[AEGIS_MLKEM1024_SECRET_KEY_SIZE]
);

/**
 * Create Ajtai commitment to state.
 *
 * @param[out] commitment  Buffer for commitment (64 bytes)
 * @param[in]  data        Data to commit to
 * @param[in]  data_len    Data length
 * @param[in]  randomness  Blinding factor (32 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_shield_create_commitment(
    uint8_t commitment[AEGIS_SHIELD_COMMITMENT_SIZE],
    const uint8_t* data,
    size_t data_len,
    const uint8_t randomness[32]
);

/*===========================================================================
 * ANUBIS Privacy Layer - Whisper (Confidential Transactions)
 *===========================================================================*/

/* Whisper constants */
#define AEGIS_WHISPER_NOTE_SIZE          160
#define AEGIS_WHISPER_NULLIFIER_SIZE     32
#define AEGIS_WHISPER_RANGE_PROOF_SIZE   1024
#define AEGIS_WHISPER_BALANCE_PROOF_SIZE 2048

/* Confidential note structure */
typedef struct {
    uint8_t commitment[64];      /* Ajtai commitment to value */
    uint8_t encrypted_data[64];  /* ML-KEM encrypted value+blinding */
    uint8_t owner_pk_hash[32];   /* SHA3(owner's ML-KEM public key) */
} aegis_whisper_note_t;

/**
 * Create a confidential note.
 *
 * @param[out] note         Output note structure
 * @param[in]  value        Value to hide (64-bit)
 * @param[in]  blinding     Blinding factor (32 bytes)
 * @param[in]  owner_pk     Owner's ML-KEM public key (1568 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_whisper_create_note(
    aegis_whisper_note_t* note,
    uint64_t value,
    const uint8_t blinding[32],
    const uint8_t owner_pk[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE]
);

/**
 * Compute nullifier for a note (prevents double-spend).
 *
 * @param[out] nullifier    Output nullifier (32 bytes)
 * @param[in]  note         Note to nullify
 * @param[in]  owner_sk     Owner's ML-KEM secret key (3168 bytes)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_whisper_compute_nullifier(
    uint8_t nullifier[AEGIS_WHISPER_NULLIFIER_SIZE],
    const aegis_whisper_note_t* note,
    const uint8_t owner_sk[AEGIS_MLKEM1024_SECRET_KEY_SIZE]
);

/**
 * Create range proof (proves value is in [0, 2^64)).
 *
 * @param[out] proof        Output proof buffer
 * @param[out] proof_len    Actual proof length
 * @param[in]  commitment   Value commitment (64 bytes)
 * @param[in]  value        Actual value (witness)
 * @param[in]  blinding     Blinding factor (witness)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_whisper_create_range_proof(
    uint8_t* proof,
    size_t* proof_len,
    const uint8_t commitment[64],
    uint64_t value,
    const uint8_t blinding[32]
);

/**
 * Verify range proof.
 *
 * @param[in]  commitment   Value commitment (64 bytes)
 * @param[in]  proof        Proof data
 * @param[in]  proof_len    Proof length
 * @param[out] valid        True if proof is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_whisper_verify_range_proof(
    const uint8_t commitment[64],
    const uint8_t* proof,
    size_t proof_len,
    bool* valid
);

/**
 * Create balance proof (proves sum(inputs) = sum(outputs) + fee).
 *
 * @param[out] proof              Output proof buffer
 * @param[out] proof_len          Actual proof length
 * @param[in]  input_commitments  Array of input commitments
 * @param[in]  input_count        Number of inputs
 * @param[in]  output_commitments Array of output commitments
 * @param[in]  output_count       Number of outputs
 * @param[in]  fee                Public fee amount
 * @param[in]  input_values       Input values (witnesses)
 * @param[in]  input_blindings    Input blinding factors (witnesses)
 * @param[in]  output_values      Output values (witnesses)
 * @param[in]  output_blindings   Output blinding factors (witnesses)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_whisper_create_balance_proof(
    uint8_t* proof,
    size_t* proof_len,
    const uint8_t (*input_commitments)[64],
    size_t input_count,
    const uint8_t (*output_commitments)[64],
    size_t output_count,
    uint64_t fee,
    const uint64_t* input_values,
    const uint8_t (*input_blindings)[32],
    const uint64_t* output_values,
    const uint8_t (*output_blindings)[32]
);

/**
 * Verify balance proof.
 *
 * @param[in]  input_commitments  Array of input commitments
 * @param[in]  input_count        Number of inputs
 * @param[in]  output_commitments Array of output commitments
 * @param[in]  output_count       Number of outputs
 * @param[in]  fee                Public fee amount
 * @param[in]  proof              Proof data
 * @param[in]  proof_len          Proof length
 * @param[out] valid              True if proof is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_whisper_verify_balance_proof(
    const uint8_t (*input_commitments)[64],
    size_t input_count,
    const uint8_t (*output_commitments)[64],
    size_t output_count,
    uint64_t fee,
    const uint8_t* proof,
    size_t proof_len,
    bool* valid
);

/*===========================================================================
 * ANUBIS Privacy Layer - Eye (Selective Disclosure)
 *===========================================================================*/

/* Eye constants */
#define AEGIS_EYE_VIEWING_KEY_SIZE       64
#define AEGIS_EYE_DISCLOSURE_PROOF_SIZE  512

/* Viewing key types */
typedef enum {
    AEGIS_VIEW_FULL = 0,       /* Full balance visibility */
    AEGIS_VIEW_BALANCE = 1,    /* Balance only (no tx details) */
    AEGIS_VIEW_EXISTENCE = 2,  /* Only proves account exists */
    AEGIS_VIEW_AUDIT = 3,      /* Time-limited audit access */
    AEGIS_VIEW_CUSTOM = 4      /* Custom attribute set */
} aegis_viewing_type_t;

/**
 * Derive viewing key from master key.
 *
 * @param[out] viewing_key  Output viewing key (64 bytes)
 * @param[in]  master_sk    Master ML-KEM secret key (3168 bytes)
 * @param[in]  view_type    Type of viewing key
 * @param[in]  valid_until  Block height until key is valid (0 = unlimited)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_eye_derive_viewing_key(
    uint8_t viewing_key[AEGIS_EYE_VIEWING_KEY_SIZE],
    const uint8_t master_sk[AEGIS_MLKEM1024_SECRET_KEY_SIZE],
    aegis_viewing_type_t view_type,
    uint64_t valid_until
);

/**
 * Create selective disclosure proof.
 *
 * @param[out] proof          Output proof buffer
 * @param[out] proof_len      Actual proof length
 * @param[in]  viewing_key    Viewing key (64 bytes)
 * @param[in]  attribute_mask Bitmask of attributes to disclose
 * @param[in]  attributes     Attribute values to prove
 * @param[in]  attr_count     Number of attributes
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_eye_create_disclosure_proof(
    uint8_t* proof,
    size_t* proof_len,
    const uint8_t viewing_key[AEGIS_EYE_VIEWING_KEY_SIZE],
    uint64_t attribute_mask,
    const uint64_t* attributes,
    size_t attr_count
);

/**
 * Verify selective disclosure.
 *
 * @param[in]  owner_pk       Owner's ML-DSA public key (2592 bytes)
 * @param[in]  attribute_mask Bitmask of disclosed attributes
 * @param[in]  attributes     Claimed attribute values
 * @param[in]  attr_count     Number of attributes
 * @param[in]  proof          Proof data
 * @param[in]  proof_len      Proof length
 * @param[out] valid          True if disclosure is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_eye_verify_disclosure(
    const uint8_t owner_pk[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    uint64_t attribute_mask,
    const uint64_t* attributes,
    size_t attr_count,
    const uint8_t* proof,
    size_t proof_len,
    bool* valid
);

/*===========================================================================
 * ANUBIS Privacy Layer - Gate (Private Execution)
 *===========================================================================*/

/* Gate constants */
#define AEGIS_GATE_SESSION_ID_SIZE       32
#define AEGIS_GATE_MAX_ENCRYPTED_INPUT   4096
#define AEGIS_GATE_EXECUTION_PROOF_SIZE  8192

/* Execution modes */
typedef enum {
    AEGIS_EXEC_FULL_PRIVATE = 0,    /* All inputs/outputs encrypted */
    AEGIS_EXEC_PUBLIC_RESULT = 1,   /* Inputs encrypted, result public */
    AEGIS_EXEC_PUBLIC_FUNCTION = 2, /* Function public, args encrypted */
    AEGIS_EXEC_AUDITABLE = 3        /* Encrypted but auditor can view */
} aegis_exec_mode_t;

/* Private execution session */
typedef struct {
    uint8_t session_id[32];
    uint8_t contract_addr[32];
    uint8_t initiator_pk[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE];
    uint8_t shared_secret[32];
    uint64_t expires_at;
    uint64_t call_count;
    uint64_t max_calls;
    aegis_exec_mode_t mode;
} aegis_gate_session_t;

/**
 * Initialize private execution session.
 *
 * @param[out] session       Output session structure
 * @param[in]  contract_addr Contract address (32 bytes)
 * @param[in]  initiator_pk  Initiator's ML-KEM public key (1568 bytes)
 * @param[in]  contract_pk   Contract's ML-KEM public key (1568 bytes)
 * @param[in]  mode          Execution mode
 * @param[in]  max_calls     Maximum calls in session
 * @param[in]  ttl_blocks    Session lifetime in blocks
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_gate_init_session(
    aegis_gate_session_t* session,
    const uint8_t contract_addr[32],
    const uint8_t initiator_pk[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE],
    const uint8_t contract_pk[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE],
    aegis_exec_mode_t mode,
    uint64_t max_calls,
    uint64_t ttl_blocks
);

/**
 * Encrypt inputs for private execution.
 *
 * @param[out] encrypted     Output encrypted buffer
 * @param[out] encrypted_len Actual encrypted length
 * @param[in]  session       Active session
 * @param[in]  function      Function selector (4 bytes)
 * @param[in]  args          Function arguments
 * @param[in]  args_len      Arguments length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_gate_encrypt_inputs(
    uint8_t* encrypted,
    size_t* encrypted_len,
    const aegis_gate_session_t* session,
    const uint8_t function[4],
    const uint8_t* args,
    size_t args_len
);

/**
 * Decrypt outputs from private execution.
 *
 * @param[out] plaintext     Output buffer for decrypted result
 * @param[out] plaintext_len Actual plaintext length
 * @param[in]  session       Active session
 * @param[in]  encrypted     Encrypted result
 * @param[in]  encrypted_len Encrypted length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_gate_decrypt_outputs(
    uint8_t* plaintext,
    size_t* plaintext_len,
    const aegis_gate_session_t* session,
    const uint8_t* encrypted,
    size_t encrypted_len
);

/*===========================================================================
 * ANUBIS Privacy Layer - Veil (ZK Proofs)
 *===========================================================================*/

/* Veil constants */
#define AEGIS_VEIL_STARK_PROOF_MAX_SIZE  65536
#define AEGIS_VEIL_LATTICE_PROOF_SIZE    4096

/**
 * Verify STARK execution proof.
 *
 * @param[in]  code_hash       Contract code hash (32 bytes)
 * @param[in]  old_state_root  Previous state root (32 bytes)
 * @param[in]  new_state_root  New state root (32 bytes)
 * @param[in]  public_inputs   Public inputs
 * @param[in]  inputs_len      Public inputs length
 * @param[in]  public_outputs  Public outputs
 * @param[in]  outputs_len     Public outputs length
 * @param[in]  proof           STARK proof data
 * @param[in]  proof_len       Proof length
 * @param[out] valid           True if proof is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_veil_verify_stark_proof(
    const uint8_t code_hash[32],
    const uint8_t old_state_root[32],
    const uint8_t new_state_root[32],
    const uint8_t* public_inputs,
    size_t inputs_len,
    const uint8_t* public_outputs,
    size_t outputs_len,
    const uint8_t* proof,
    size_t proof_len,
    bool* valid
);

/**
 * Verify lattice-based ZK proof.
 *
 * @param[in]  statement    Statement being proved
 * @param[in]  stmt_len     Statement length
 * @param[in]  proof        Lattice ZK proof data
 * @param[in]  proof_len    Proof length
 * @param[out] valid        True if proof is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_veil_verify_lattice_proof(
    const uint8_t* statement,
    size_t stmt_len,
    const uint8_t* proof,
    size_t proof_len,
    bool* valid
);

/*===========================================================================
 * Utility Functions
 *===========================================================================*/

/**
 * Securely zero memory (resistant to compiler optimization).
 *
 * @param[out] ptr   Pointer to memory
 * @param[in]  len   Length in bytes
 */
void aegis_secure_zero(
    void* ptr,
    size_t len
);

/**
 * Constant-time memory comparison.
 *
 * @param[in] a    First buffer
 * @param[in] b    Second buffer
 * @param[in] len  Length to compare
 * @return 0 if equal, non-zero otherwise (constant time)
 */
int aegis_constant_time_compare(
    const void* a,
    const void* b,
    size_t len
);

/**
 * Get library version string.
 *
 * @return Version string (e.g., "1.0.0")
 */
const char* aegis_version(void);

/**
 * Initialize the AegisVM library.
 * Must be called once before any other functions.
 *
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_init(void);

/**
 * Cleanup the AegisVM library.
 * Call before program exit.
 */
void aegis_cleanup(void);

/*===========================================================================
 * SCARAB v2.0 - Proof Aggregation System
 *===========================================================================*/

/*---------------------------------------------------------------------------
 * KHNUM - Transaction Signature Aggregation
 *---------------------------------------------------------------------------*/

/* KHNUM constants */
#define AEGIS_KHNUM_MAX_BATCH_SIZE          4096
#define AEGIS_KHNUM_AGGREGATED_PROOF_SIZE   32768

/**
 * Create aggregated proof for batch of ML-DSA-87 signatures.
 *
 * @param[out] proof         Output aggregated proof
 * @param[out] proof_len     Actual proof length
 * @param[in]  signatures    Array of signatures
 * @param[in]  public_keys   Array of public keys
 * @param[in]  messages      Array of message hashes
 * @param[in]  count         Number of signatures (max 4096)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_khnum_aggregate_signatures(
    uint8_t* proof,
    size_t* proof_len,
    const uint8_t (*signatures)[AEGIS_MLDSA87_SIGNATURE_SIZE],
    const uint8_t (*public_keys)[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    const uint8_t (*messages)[32],
    size_t count
);

/**
 * Verify aggregated signature proof.
 *
 * @param[in]  proof         Aggregated proof
 * @param[in]  proof_len     Proof length
 * @param[in]  public_keys   Array of public keys (commitments)
 * @param[in]  messages      Array of message hashes
 * @param[in]  count         Number of signatures
 * @param[out] valid         True if all signatures valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_khnum_verify_aggregated(
    const uint8_t* proof,
    size_t proof_len,
    const uint8_t (*public_keys)[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    const uint8_t (*messages)[32],
    size_t count,
    bool* valid
);

/*---------------------------------------------------------------------------
 * SEBEK - Threshold Signature Scheme
 *---------------------------------------------------------------------------*/

/* SEBEK constants */
#define AEGIS_SEBEK_MAX_COMMITTEE_SIZE      67
#define AEGIS_SEBEK_THRESHOLD              45
#define AEGIS_SEBEK_SHARE_SIZE             256
#define AEGIS_SEBEK_COMBINED_SIG_SIZE      4800

/* Key share for threshold signing */
typedef struct {
    uint8_t share[AEGIS_SEBEK_SHARE_SIZE];
    uint32_t index;
    uint8_t commitment[32];
} aegis_sebek_share_t;

/**
 * Generate partial signature using key share.
 *
 * @param[out] partial_sig   Output partial signature
 * @param[out] sig_len       Actual signature length
 * @param[in]  share         Key share
 * @param[in]  message       Message to sign
 * @param[in]  msg_len       Message length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_sebek_sign_partial(
    uint8_t* partial_sig,
    size_t* sig_len,
    const aegis_sebek_share_t* share,
    const uint8_t* message,
    size_t msg_len
);

/**
 * Combine partial signatures into threshold signature.
 *
 * @param[out] combined_sig  Output combined signature
 * @param[out] sig_len       Actual signature length
 * @param[in]  partials      Array of partial signatures
 * @param[in]  indices       Array of signer indices
 * @param[in]  count         Number of partial signatures (must >= threshold)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_sebek_combine_signatures(
    uint8_t combined_sig[AEGIS_SEBEK_COMBINED_SIG_SIZE],
    size_t* sig_len,
    const uint8_t (*partials)[AEGIS_SEBEK_SHARE_SIZE],
    const uint32_t* indices,
    size_t count
);

/**
 * Verify threshold signature against combined public key.
 *
 * @param[in]  signature     Combined signature
 * @param[in]  sig_len       Signature length
 * @param[in]  message       Original message
 * @param[in]  msg_len       Message length
 * @param[in]  combined_pk   Combined public key (32 bytes)
 * @param[out] valid         True if signature is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_sebek_verify(
    const uint8_t* signature,
    size_t sig_len,
    const uint8_t* message,
    size_t msg_len,
    const uint8_t combined_pk[32],
    bool* valid
);

/*---------------------------------------------------------------------------
 * AADKG - Asynchronous Adaptive Distributed Key Generation
 *---------------------------------------------------------------------------*/

/* AADKG phase states */
typedef enum {
    AEGIS_AADKG_PHASE_COMMITMENT = 0,
    AEGIS_AADKG_PHASE_DISTRIBUTION = 1,
    AEGIS_AADKG_PHASE_VERIFICATION = 2,
    AEGIS_AADKG_PHASE_AGGREGATION = 3,
    AEGIS_AADKG_PHASE_RECOVERY = 4,
    AEGIS_AADKG_PHASE_COMPLETE = 5,
    AEGIS_AADKG_PHASE_FAILED = 6
} aegis_aadkg_phase_t;

/* Encrypted share for on-chain storage */
typedef struct {
    uint32_t sender;
    uint32_t recipient;
    uint8_t ciphertext[AEGIS_MLKEM1024_CIPHERTEXT_SIZE + 256];
    size_t ct_len;
} aegis_aadkg_encrypted_share_t;

/**
 * Generate and encrypt shares for all committee members.
 *
 * @param[out] shares        Output array of encrypted shares
 * @param[out] commitment    Polynomial commitment (for verification)
 * @param[in]  my_index      This party's index
 * @param[in]  randomness    64-byte randomness for polynomial
 * @param[in]  recipient_pks Array of recipient ML-KEM public keys
 * @param[in]  committee_size Committee size
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_aadkg_generate_shares(
    aegis_aadkg_encrypted_share_t* shares,
    uint8_t commitment[32],
    uint32_t my_index,
    const uint8_t randomness[64],
    const uint8_t (*recipient_pks)[AEGIS_MLKEM1024_PUBLIC_KEY_SIZE],
    size_t committee_size
);

/**
 * Aggregate received shares into final key share.
 *
 * @param[out] my_share      Output final key share
 * @param[out] combined_pk   Combined threshold public key
 * @param[in]  my_index      This party's index
 * @param[in]  my_kem_sk     This party's ML-KEM secret key
 * @param[in]  all_shares    All encrypted shares for this party
 * @param[in]  share_count   Number of shares
 * @param[in]  commitments   All polynomial commitments
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_aadkg_aggregate_shares(
    aegis_sebek_share_t* my_share,
    uint8_t combined_pk[32],
    uint32_t my_index,
    const uint8_t my_kem_sk[AEGIS_MLKEM1024_SECRET_KEY_SIZE],
    const aegis_aadkg_encrypted_share_t* all_shares,
    size_t share_count,
    const uint8_t (*commitments)[32]
);

/**
 * Recover DKG state from on-chain data after crash.
 *
 * @param[out] my_share      Recovered key share
 * @param[out] combined_pk   Combined threshold public key
 * @param[in]  my_index      This party's index
 * @param[in]  my_kem_sk     This party's ML-KEM secret key
 * @param[in]  chain_shares  Encrypted shares from blockchain
 * @param[in]  share_count   Number of shares
 * @param[in]  chain_commits Commitments from blockchain
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_aadkg_recover_from_chain(
    aegis_sebek_share_t* my_share,
    uint8_t combined_pk[32],
    uint32_t my_index,
    const uint8_t my_kem_sk[AEGIS_MLKEM1024_SECRET_KEY_SIZE],
    const aegis_aadkg_encrypted_share_t* chain_shares,
    size_t share_count,
    const uint8_t (*chain_commits)[32]
);

/*---------------------------------------------------------------------------
 * HORUS - Pipelined Parallel Prover
 *---------------------------------------------------------------------------*/

/* Prover tiers */
typedef enum {
    AEGIS_PROVER_LIGHT = 0,      /* 16GB RAM */
    AEGIS_PROVER_STANDARD = 1,   /* 64GB RAM */
    AEGIS_PROVER_HEAVY = 2,      /* 256GB RAM */
    AEGIS_PROVER_ENTERPRISE = 3  /* 1TB+ RAM */
} aegis_prover_tier_t;

/* Transaction modes */
typedef enum {
    AEGIS_TX_STANDARD = 0,      /* Normal, prover aggregates */
    AEGIS_TX_PRE_PROVEN = 1,    /* User provides proof */
    AEGIS_TX_BATCHED = 2        /* Batch aggregator handles */
} aegis_tx_mode_t;

/* Proof job for prover market */
typedef struct {
    uint8_t job_id[32];
    uint64_t block_height;
    size_t batch_size;
    uint64_t deadline;
    uint64_t reward;
    uint8_t claimed_by[32];
} aegis_horus_job_t;

/**
 * Submit transaction with specified mode.
 *
 * @param[out] tx_id         Transaction ID
 * @param[in]  mode          Transaction mode
 * @param[in]  tx_data       Transaction data
 * @param[in]  tx_len        Transaction length
 * @param[in]  signature     ML-DSA signature (for STANDARD mode)
 * @param[in]  proof         User proof (for PRE_PROVEN mode)
 * @param[in]  proof_len     Proof length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_horus_submit_tx(
    uint8_t tx_id[32],
    aegis_tx_mode_t mode,
    const uint8_t* tx_data,
    size_t tx_len,
    const uint8_t signature[AEGIS_MLDSA87_SIGNATURE_SIZE],
    const uint8_t* proof,
    size_t proof_len
);

/**
 * Claim proof job from market.
 *
 * @param[in]  prover_addr   Prover's address
 * @param[in]  job_id        Job ID to claim
 * @param[out] job           Job details
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_horus_claim_job(
    const uint8_t prover_addr[32],
    const uint8_t job_id[32],
    aegis_horus_job_t* job
);

/**
 * Submit completed proof for job.
 *
 * @param[in]  job_id        Job ID
 * @param[in]  prover_addr   Prover's address
 * @param[in]  proof         Completed proof
 * @param[in]  proof_len     Proof length
 * @param[out] reward        Reward paid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_horus_submit_proof(
    const uint8_t job_id[32],
    const uint8_t prover_addr[32],
    const uint8_t* proof,
    size_t proof_len,
    uint64_t* reward
);

/*---------------------------------------------------------------------------
 * TEFNUT - Light Client Proofs
 *---------------------------------------------------------------------------*/

/* Light client profiles */
typedef enum {
    AEGIS_TEFNUT_STANDARD = 0,  /* 64KB RAM, 100ms verify */
    AEGIS_TEFNUT_ULTRA = 1,     /* 8KB RAM, 50ms verify */
    AEGIS_TEFNUT_IOT = 2        /* ESP32/nRF52 optimized */
} aegis_tefnut_profile_t;

/* Light client checkpoint */
typedef struct {
    uint64_t block_height;
    uint8_t state_root[32];
    uint8_t committee_root[32];
    uint8_t proof[1024];
    size_t proof_len;
} aegis_tefnut_checkpoint_t;

/**
 * Generate light client proof for state query.
 *
 * @param[out] proof         Output proof
 * @param[out] proof_len     Actual proof length
 * @param[in]  profile       Target device profile
 * @param[in]  checkpoint    Latest checkpoint
 * @param[in]  query_key     State key being queried
 * @param[in]  query_value   State value
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_tefnut_generate_proof(
    uint8_t* proof,
    size_t* proof_len,
    aegis_tefnut_profile_t profile,
    const aegis_tefnut_checkpoint_t* checkpoint,
    const uint8_t query_key[32],
    const uint8_t* query_value,
    size_t value_len
);

/**
 * Verify light client proof (optimized for constrained devices).
 *
 * @param[in]  checkpoint    Trusted checkpoint
 * @param[in]  query_key     State key
 * @param[in]  query_value   Claimed value
 * @param[in]  value_len     Value length
 * @param[in]  proof         Proof data
 * @param[in]  proof_len     Proof length
 * @param[out] valid         True if proof is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_tefnut_verify_proof(
    const aegis_tefnut_checkpoint_t* checkpoint,
    const uint8_t query_key[32],
    const uint8_t* query_value,
    size_t value_len,
    const uint8_t* proof,
    size_t proof_len,
    bool* valid
);

/*---------------------------------------------------------------------------
 * MAAT - Hierarchical Non-Recursive Aggregation
 *---------------------------------------------------------------------------*/

/* Aggregation levels */
typedef enum {
    AEGIS_MAAT_LEVEL_INDIVIDUAL = 0,  /* Individual signatures */
    AEGIS_MAAT_LEVEL_BATCH = 1,       /* 256 sigs per batch */
    AEGIS_MAAT_LEVEL_BLOCK = 2,       /* 16 batches = 4096 sigs */
    AEGIS_MAAT_LEVEL_EPOCH = 3        /* 100 blocks */
} aegis_maat_level_t;

/* Block proof structure */
typedef struct {
    uint8_t batch_commits[16][32];    /* 16 batch commitments */
    uint8_t aggregated_proof[32768];  /* STARK proof */
    size_t proof_len;
    uint64_t block_height;
} aegis_maat_block_proof_t;

/* Epoch proof for light clients */
typedef struct {
    uint8_t block_roots[100][32];     /* 100 block roots */
    uint8_t epoch_root[32];
    uint8_t state_root[32];
    uint64_t epoch_number;
    uint8_t epoch_proof[65536];
    size_t proof_len;
} aegis_maat_epoch_proof_t;

/**
 * Generate block proof for batch of transactions.
 *
 * @param[out] proof         Output block proof
 * @param[in]  signatures    Array of 4096 signatures
 * @param[in]  public_keys   Array of 4096 public keys
 * @param[in]  tx_hashes     Array of 4096 transaction hashes
 * @param[in]  block_height  Block height
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_maat_generate_block_proof(
    aegis_maat_block_proof_t* proof,
    const uint8_t (*signatures)[AEGIS_MLDSA87_SIGNATURE_SIZE],
    const uint8_t (*public_keys)[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    const uint8_t (*tx_hashes)[32],
    uint64_t block_height
);

/**
 * Verify block proof (O(1) verification).
 *
 * @param[in]  proof         Block proof
 * @param[in]  public_keys   Array of 4096 public keys
 * @param[in]  tx_hashes     Array of 4096 transaction hashes
 * @param[out] valid         True if all signatures valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_maat_verify_block_proof(
    const aegis_maat_block_proof_t* proof,
    const uint8_t (*public_keys)[AEGIS_MLDSA87_PUBLIC_KEY_SIZE],
    const uint8_t (*tx_hashes)[32],
    bool* valid
);

/**
 * Verify epoch proof for light client sync.
 *
 * @param[in]  proof          Epoch proof
 * @param[in]  prev_state     Previous state root
 * @param[in]  expected_state Expected new state root
 * @param[out] valid          True if epoch is valid
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_maat_verify_epoch_proof(
    const aegis_maat_epoch_proof_t* proof,
    const uint8_t prev_state[32],
    const uint8_t expected_state[32],
    bool* valid
);

/*---------------------------------------------------------------------------
 * THOTH - Verified AIR Compiler
 *---------------------------------------------------------------------------*/

/* Circuit types */
typedef enum {
    AEGIS_THOTH_MLDSA_VERIFY = 0,     /* ML-DSA-87 signature verification */
    AEGIS_THOTH_MERKLE_VERIFY = 1,    /* Merkle membership proof */
    AEGIS_THOTH_SHA3_256 = 2,         /* SHA3-256 hash */
    AEGIS_THOTH_BATCH_VERIFY = 3,     /* Batch signature verification */
    AEGIS_THOTH_CUSTOM = 4            /* Custom circuit */
} aegis_thoth_circuit_t;

/* Compiled AIR program */
typedef struct {
    uint8_t program_hash[32];
    size_t trace_width;
    size_t trace_length;
    size_t constraint_count;
    uint8_t* constraints;
    size_t constraints_len;
} aegis_thoth_air_t;

/**
 * Get pre-compiled circuit for standard operations.
 *
 * @param[out] air           Output AIR program
 * @param[in]  circuit_type  Type of circuit
 * @param[in]  params        Circuit parameters (circuit-specific)
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_thoth_get_circuit(
    aegis_thoth_air_t* air,
    aegis_thoth_circuit_t circuit_type,
    const void* params
);

/**
 * Compile custom circuit from THOTH DSL.
 *
 * @param[out] air           Output AIR program
 * @param[in]  source        THOTH DSL source code
 * @param[in]  source_len    Source length
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_thoth_compile(
    aegis_thoth_air_t* air,
    const char* source,
    size_t source_len
);

/**
 * Free AIR program resources.
 *
 * @param[in] air            AIR program to free
 */
void aegis_thoth_free_air(aegis_thoth_air_t* air);

/*---------------------------------------------------------------------------
 * SEKHMET - SIMD Acceleration
 *---------------------------------------------------------------------------*/

/* SIMD backends */
typedef enum {
    AEGIS_SIMD_SCALAR = 0,    /* Pure Ada fallback */
    AEGIS_SIMD_AVX2 = 1,      /* x86-64 256-bit */
    AEGIS_SIMD_AVX512 = 2,    /* x86-64 512-bit */
    AEGIS_SIMD_NEON = 3,      /* ARM 128-bit */
    AEGIS_SIMD_SVE = 4        /* ARM Scalable Vector */
} aegis_simd_backend_t;

/**
 * Detect best available SIMD backend.
 *
 * @return Detected backend type
 */
aegis_simd_backend_t aegis_sekhmet_detect_backend(void);

/**
 * Perform vectorized NTT (Number Theoretic Transform).
 *
 * @param[in,out] data       Data array (modified in place)
 * @param[in]     size       Array size (must be power of 2)
 * @param[in]     inverse    True for inverse NTT
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_sekhmet_ntt(
    uint64_t* data,
    size_t size,
    bool inverse
);

/**
 * Perform vectorized polynomial multiplication.
 *
 * @param[out] result        Output polynomial
 * @param[in]  a             First polynomial
 * @param[in]  b             Second polynomial
 * @param[in]  size          Polynomial size
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_sekhmet_poly_mul(
    uint64_t* result,
    const uint64_t* a,
    const uint64_t* b,
    size_t size
);

/*---------------------------------------------------------------------------
 * KHEPRI - Native SPARK Contract System
 *---------------------------------------------------------------------------*/

/* Contract deployment result */
typedef struct {
    uint8_t contract_addr[32];
    uint8_t code_hash[32];
    aegis_cert_level_t cert_level;
    uint64_t gas_used;
} aegis_khepri_deploy_result_t;

/**
 * Deploy native SPARK contract.
 *
 * @param[out] result        Deployment result
 * @param[in]  elf_binary    ELF binary of compiled SPARK contract
 * @param[in]  elf_len       Binary length
 * @param[in]  proof_hash    Hash of SPARK proof artifacts
 * @param[in]  cert_level    Claimed certification level
 * @param[in]  deployer      Deployer's address
 * @param[in]  gas_limit     Maximum gas
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_khepri_deploy(
    aegis_khepri_deploy_result_t* result,
    const uint8_t* elf_binary,
    size_t elf_len,
    const uint8_t proof_hash[32],
    aegis_cert_level_t cert_level,
    const uint8_t deployer[32],
    uint64_t gas_limit
);

/**
 * Execute native SPARK contract function.
 *
 * @param[out]    output       Return data buffer
 * @param[in,out] output_len   In: buffer size, Out: actual length
 * @param[out]    gas_used     Gas consumed
 * @param[in]     contract     Contract address
 * @param[in]     function     Function selector
 * @param[in]     func_len     Function selector length
 * @param[in]     args         Function arguments
 * @param[in]     args_len     Arguments length
 * @param[in]     ctx          Execution context
 * @return Execution status
 */
aegis_result_t aegis_khepri_execute(
    uint8_t* output,
    size_t* output_len,
    uint64_t* gas_used,
    const uint8_t contract[32],
    const uint8_t* function,
    size_t func_len,
    const uint8_t* args,
    size_t args_len,
    const aegis_exec_context_t* ctx
);

/**
 * Verify contract certification level.
 *
 * @param[in]  code_hash     Contract code hash
 * @param[in]  proof_hash    SPARK proof artifacts hash
 * @param[in]  claimed_level Claimed certification level
 * @param[out] verified      True if certification verified
 * @return AEGIS_OK on success, error code otherwise
 */
aegis_result_t aegis_khepri_verify_certification(
    const uint8_t code_hash[32],
    const uint8_t proof_hash[32],
    aegis_cert_level_t claimed_level,
    bool* verified
);

#ifdef __cplusplus
}
#endif

#endif /* AEGISVM_H */
