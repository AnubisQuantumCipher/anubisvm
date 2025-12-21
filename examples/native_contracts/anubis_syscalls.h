/*
 * AnubisVM Syscall Interface for Native Contracts
 *
 * This header defines the syscall table that the VM provides to native
 * contracts. Contracts call these functions for all VM operations.
 *
 * PRODUCTION MODE: All operations go through the VM syscalls.
 * No local implementations - everything uses VM power.
 */

#ifndef ANUBIS_SYSCALLS_H
#define ANUBIS_SYSCALLS_H

#include <stdint.h>
#include <stddef.h>

/*===========================================================================
 *  Address Types (matches Anubis_Address_Types)
 *===========================================================================*/

/* 256-bit Account ID (32 bytes) */
typedef struct {
    uint8_t data[32];
} Account_ID;

/* Full address with network and entity type */
typedef struct {
    uint8_t  network;   /* 0=main, 1=test, 2=dev, 3=lab, 4=staging */
    uint8_t  entity;    /* 0=user, 1=contract, 2=validator, 3=system */
    Account_ID account;
    uint8_t  valid;
} Address;

/* 256-bit hash/storage key */
typedef struct {
    uint8_t data[32];
} Hash256;

/*===========================================================================
 *  Syscall Function Types
 *===========================================================================*/

/* THOTH Storage */
typedef void (*syscall_sload_t)(const Hash256* slot, Hash256* value);
typedef void (*syscall_sstore_t)(const Hash256* slot, const Hash256* value);

/* ANKH Crypto */
typedef void (*syscall_sha3_t)(const uint8_t* input, size_t len, Hash256* output);
typedef int  (*syscall_mldsa_verify_t)(
    const uint8_t* message, size_t msg_len,
    const uint8_t* signature, size_t sig_len,
    const uint8_t* pubkey
);
typedef int  (*syscall_mlkem_decaps_t)(
    const uint8_t* ciphertext, size_t ct_len,
    const uint8_t* secret_key, size_t sk_len,
    uint8_t* shared_secret
);

/* Environment */
typedef void     (*syscall_get_caller_t)(Account_ID* addr);
typedef void     (*syscall_get_self_t)(Account_ID* addr);
typedef uint64_t (*syscall_get_timestamp_t)(void);
typedef uint64_t (*syscall_get_block_number_t)(void);
typedef uint64_t (*syscall_get_gas_remaining_t)(void);

/*===========================================================================
 *  Syscall Table (provided by VM)
 *===========================================================================*/

typedef struct {
    /* THOTH Storage */
    syscall_sload_t  sload;
    syscall_sstore_t sstore;

    /* ANKH Crypto */
    syscall_sha3_t         sha3;
    syscall_mldsa_verify_t mldsa_verify;
    syscall_mlkem_decaps_t mlkem_decaps;

    /* Environment */
    syscall_get_caller_t       get_caller;
    syscall_get_self_t         get_self;
    syscall_get_timestamp_t    get_timestamp;
    syscall_get_block_number_t get_block_number;
    syscall_get_gas_remaining_t get_gas_remaining;
} Syscall_Table;

/*===========================================================================
 *  Execution Context (provided by VM)
 *===========================================================================*/

typedef struct {
    Account_ID   caller;
    Account_ID   self;
    uint64_t     call_value;
    uint64_t     gas_limit;
    uint64_t     gas_used;
    uint64_t     block_number;
    uint64_t     timestamp;
    Syscall_Table* syscalls;
} Execution_Context;

/*===========================================================================
 *  Global Context (set by VM before execution)
 *===========================================================================*/

extern Execution_Context* vm_context;

/*===========================================================================
 *  High-Level Syscall Wrappers
 *===========================================================================*/

/* Storage */
static inline void vm_sload(const Hash256* slot, Hash256* value) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->sload) {
        vm_context->syscalls->sload(slot, value);
    } else {
        for (int i = 0; i < 32; i++) value->data[i] = 0;
    }
}

static inline void vm_sstore(const Hash256* slot, const Hash256* value) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->sstore) {
        vm_context->syscalls->sstore(slot, value);
    }
}

/* Crypto */
static inline void vm_sha3(const uint8_t* input, size_t len, Hash256* output) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->sha3) {
        vm_context->syscalls->sha3(input, len, output);
    } else {
        for (int i = 0; i < 32; i++) output->data[i] = 0;
    }
}

static inline int vm_mldsa_verify(
    const uint8_t* msg, size_t msg_len,
    const uint8_t* sig, size_t sig_len,
    const uint8_t* pubkey
) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->mldsa_verify) {
        return vm_context->syscalls->mldsa_verify(msg, msg_len, sig, sig_len, pubkey);
    }
    return 0;  /* Verification fails if no syscall */
}

/* Environment */
static inline void vm_get_caller(Account_ID* addr) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->get_caller) {
        vm_context->syscalls->get_caller(addr);
    } else if (vm_context) {
        *addr = vm_context->caller;
    } else {
        for (int i = 0; i < 32; i++) addr->data[i] = 0;
    }
}

static inline void vm_get_self(Account_ID* addr) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->get_self) {
        vm_context->syscalls->get_self(addr);
    } else if (vm_context) {
        *addr = vm_context->self;
    } else {
        for (int i = 0; i < 32; i++) addr->data[i] = 0;
    }
}

static inline uint64_t vm_get_timestamp(void) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->get_timestamp) {
        return vm_context->syscalls->get_timestamp();
    } else if (vm_context) {
        return vm_context->timestamp;
    }
    return 0;
}

static inline uint64_t vm_get_block_number(void) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->get_block_number) {
        return vm_context->syscalls->get_block_number();
    } else if (vm_context) {
        return vm_context->block_number;
    }
    return 0;
}

static inline uint64_t vm_get_gas_remaining(void) {
    if (vm_context && vm_context->syscalls && vm_context->syscalls->get_gas_remaining) {
        return vm_context->syscalls->get_gas_remaining();
    } else if (vm_context) {
        return vm_context->gas_limit - vm_context->gas_used;
    }
    return 0;
}

/*===========================================================================
 *  Gas Costs
 *===========================================================================*/

#define GAS_BASE            50
#define GAS_SLOAD_COLD      2100
#define GAS_SLOAD_WARM      100
#define GAS_SSTORE_SET      20000
#define GAS_SSTORE_RESET    2900
#define GAS_SHA3_BASE       30
#define GAS_SHA3_WORD       6
#define GAS_MLDSA_VERIFY    50000
#define GAS_MLKEM_DECAPS    10000

/*===========================================================================
 *  Error Codes
 *===========================================================================*/

#define ERR_SUCCESS             0
#define ERR_OUT_OF_GAS          1
#define ERR_INVALID_OPCODE      2
#define ERR_INVALID_INPUT       3
#define ERR_UNAUTHORIZED        11
#define ERR_VERIFICATION_FAILED 10

/*===========================================================================
 *  Utility Macros
 *===========================================================================*/

/* Check gas and return if insufficient */
#define REQUIRE_GAS(amount, gas_used_ptr, gas_limit) \
    do { \
        *(gas_used_ptr) += (amount); \
        if (*(gas_used_ptr) > (gas_limit)) { \
            return ERR_OUT_OF_GAS; \
        } \
    } while(0)

/* Compare two Account_IDs */
static inline int account_id_eq(const Account_ID* a, const Account_ID* b) {
    for (int i = 0; i < 32; i++) {
        if (a->data[i] != b->data[i]) return 0;
    }
    return 1;
}

/* Zero an Account_ID */
static inline void account_id_zero(Account_ID* a) {
    for (int i = 0; i < 32; i++) a->data[i] = 0;
}

/* Check if Account_ID is zero */
static inline int account_id_is_zero(const Account_ID* a) {
    for (int i = 0; i < 32; i++) {
        if (a->data[i] != 0) return 0;
    }
    return 1;
}

#endif /* ANUBIS_SYSCALLS_H */
