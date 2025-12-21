/*
 * AnubisVM Native Contract SDK
 *
 * This header provides the complete syscall interface for native contracts.
 * Contracts link against this interface to access VM services.
 */

#ifndef ANUBIS_VM_H
#define ANUBIS_VM_H

#include <stdint.h>
#include <stddef.h>

/*===========================================================================
 *  Basic Types
 *===========================================================================*/

typedef uint8_t  Byte;
typedef uint64_t Gas_Amount;
typedef uint64_t U256[4];  /* 256-bit unsigned integer (little-endian) */

/* 256-bit hash/address type */
typedef struct {
    uint8_t data[32];
} Hash256;

/* Contract address (32 bytes) */
typedef Hash256 Address;

/* Storage slot key/value */
typedef Hash256 Storage_Key;
typedef Hash256 Storage_Value;

/*===========================================================================
 *  Syscall Numbers - THOTH (Storage)
 *===========================================================================*/

#define SYS_SLOAD           0x01
#define SYS_SSTORE          0x02

/*===========================================================================
 *  Syscall Numbers - ANKH (Crypto)
 *===========================================================================*/

#define SYS_SHA3            0x10
#define SYS_KECCAK256       0x11
#define SYS_MLDSA_VERIFY    0x12
#define SYS_MLKEM_DECAPS    0x13

/*===========================================================================
 *  Syscall Numbers - Environment
 *===========================================================================*/

#define SYS_CALLER          0x20
#define SYS_ADDRESS         0x21
#define SYS_CALLVALUE       0x22
#define SYS_CALLDATA        0x23
#define SYS_CALLDATASIZE    0x24
#define SYS_BLOCKNUMBER     0x25
#define SYS_TIMESTAMP       0x26
#define SYS_GASPRICE        0x27
#define SYS_GASREMAINING    0x28
#define SYS_BALANCE         0x30
#define SYS_SELFBALANCE     0x31

/*===========================================================================
 *  Syscall Numbers - Call Operations
 *===========================================================================*/

#define SYS_CALL            0x40
#define SYS_STATICCALL      0x41
#define SYS_DELEGATECALL    0x42
#define SYS_CREATE          0x43
#define SYS_CREATE2         0x44

/*===========================================================================
 *  Syscall Numbers - Control
 *===========================================================================*/

#define SYS_RETURN          0x50
#define SYS_REVERT          0x51
#define SYS_STOP            0x52
#define SYS_SELFDESTRUCT    0x53

/*===========================================================================
 *  Syscall Numbers - Events
 *===========================================================================*/

#define SYS_LOG0            0x60
#define SYS_LOG1            0x61
#define SYS_LOG2            0x62
#define SYS_LOG3            0x63
#define SYS_LOG4            0x64

/*===========================================================================
 *  Syscall Numbers - ANUBIS Privacy
 *===========================================================================*/

/* SHIELD - Encrypted State */
#define SYS_PRIVATE_STORE   0x80
#define SYS_PRIVATE_LOAD    0x81
#define SYS_PRIVATE_DELETE  0x82

/* WHISPER - Confidential Values */
#define SYS_COMMIT_AMOUNT   0x83
#define SYS_VERIFY_RANGE    0x84
#define SYS_ADD_COMMITMENTS 0x85
#define SYS_VERIFY_BALANCE  0x86

/* GATE - Private Execution */
#define SYS_PRIVATE_CALL    0x87
#define SYS_VERIFY_EXEC     0x88
#define SYS_CREATE_SESSION  0x89
#define SYS_CLOSE_SESSION   0x8A

/* EYE - Selective Disclosure */
#define SYS_CREATE_DISCLOSURE 0x8B
#define SYS_VERIFY_DISCLOSURE 0x8C
#define SYS_DERIVE_VIEW_KEY   0x8D
#define SYS_GENERATE_STEALTH  0x8E

/* VEIL - Ring Signatures */
#define SYS_RING_SIGN         0x8F
#define SYS_VERIFY_RING_SIG   0x90
#define SYS_COMPUTE_KEY_IMAGE 0x91
#define SYS_CHECK_SPENT       0x92

/* Lattice ZK Proofs */
#define SYS_ZK_PROVE_RANGE    0x93
#define SYS_ZK_VERIFY_RANGE   0x94
#define SYS_ZK_PROVE_LINEAR   0x95
#define SYS_ZK_VERIFY_LINEAR  0x96

/* Confidential Transfers */
#define SYS_CONFIDENTIAL_TRANSFER 0x97
#define SYS_CREATE_TRANSFER_PROOF 0x98
#define SYS_VERIFY_TRANSFER       0x99
#define SYS_SCAN_CONFIDENTIAL     0x9A

/*===========================================================================
 *  Gas Costs
 *===========================================================================*/

#define GAS_BASE            50
#define GAS_SLOAD_COLD      2100
#define GAS_SLOAD_WARM      100
#define GAS_SSTORE_SET      20000
#define GAS_SSTORE_RESET    2900
#define GAS_SSTORE_CLEAR    4800  /* Refund for clearing */
#define GAS_SHA3_BASE       30
#define GAS_SHA3_WORD       6
#define GAS_MLDSA_VERIFY    50000
#define GAS_MLKEM_DECAPS    10000
#define GAS_CALL            100
#define GAS_LOG_BASE        375
#define GAS_LOG_TOPIC       375
#define GAS_LOG_DATA        8

/*===========================================================================
 *  Error Codes
 *===========================================================================*/

#define ERR_SUCCESS         0
#define ERR_OUT_OF_GAS      1
#define ERR_INVALID_OPCODE  2
#define ERR_INVALID_INPUT   3
#define ERR_STACK_OVERFLOW  4
#define ERR_STACK_UNDERFLOW 5
#define ERR_INVALID_JUMP    6
#define ERR_WRITE_PROTECT   7
#define ERR_CALL_DEPTH      8
#define ERR_INSUFFICIENT_BALANCE 9
#define ERR_VERIFICATION_FAILED 10
#define ERR_UNAUTHORIZED    11

/*===========================================================================
 *  Method Selectors (first 4 bytes of SHA3(method_name))
 *===========================================================================*/

/* Compute at runtime or define common ones */
#define SELECTOR_TRANSFER     0xa9059cbb
#define SELECTOR_BALANCE_OF   0x70a08231
#define SELECTOR_APPROVE      0x095ea7b3
#define SELECTOR_ALLOWANCE    0xdd62ed3e
#define SELECTOR_TOTAL_SUPPLY 0x18160ddd

/*===========================================================================
 *  VM Context (passed to syscalls)
 *===========================================================================*/

typedef struct {
    Address   caller;
    Address   self;
    U256      value;
    uint64_t  gas_remaining;
    uint64_t  block_number;
    uint64_t  timestamp;
} VM_Context;

/*===========================================================================
 *  Syscall Interface
 *
 *  These are stubs that would be implemented by the VM runtime.
 *  For testing, we provide software implementations.
 *===========================================================================*/

/* Storage callbacks - set by VM before execution */
typedef void (*storage_load_fn)(const Storage_Key* slot, Storage_Value* value);
typedef void (*storage_store_fn)(const Storage_Key* slot, const Storage_Value* value);

extern storage_load_fn  vm_sload;
extern storage_store_fn vm_sstore;

/* Inline helpers for storage */
static inline void sload(const Hash256* slot, Hash256* value) {
    if (vm_sload) vm_sload(slot, value);
}

static inline void sstore(const Hash256* slot, const Hash256* value) {
    if (vm_sstore) vm_sstore(slot, value);
}

/*===========================================================================
 *  Utility Functions
 *===========================================================================*/

/* Hash256 operations */
static inline void hash256_zero(Hash256* h) {
    for (int i = 0; i < 32; i++) h->data[i] = 0;
}

static inline int hash256_is_zero(const Hash256* h) {
    for (int i = 0; i < 32; i++) {
        if (h->data[i] != 0) return 0;
    }
    return 1;
}

static inline int hash256_eq(const Hash256* a, const Hash256* b) {
    for (int i = 0; i < 32; i++) {
        if (a->data[i] != b->data[i]) return 0;
    }
    return 1;
}

static inline void hash256_copy(Hash256* dst, const Hash256* src) {
    for (int i = 0; i < 32; i++) dst->data[i] = src->data[i];
}

/* Little-endian conversions */
static inline uint64_t read_le64(const uint8_t* buf) {
    return (uint64_t)buf[0] | ((uint64_t)buf[1] << 8) |
           ((uint64_t)buf[2] << 16) | ((uint64_t)buf[3] << 24) |
           ((uint64_t)buf[4] << 32) | ((uint64_t)buf[5] << 40) |
           ((uint64_t)buf[6] << 48) | ((uint64_t)buf[7] << 56);
}

static inline void write_le64(uint8_t* buf, uint64_t val) {
    buf[0] = val & 0xFF;
    buf[1] = (val >> 8) & 0xFF;
    buf[2] = (val >> 16) & 0xFF;
    buf[3] = (val >> 24) & 0xFF;
    buf[4] = (val >> 32) & 0xFF;
    buf[5] = (val >> 40) & 0xFF;
    buf[6] = (val >> 48) & 0xFF;
    buf[7] = (val >> 56) & 0xFF;
}

static inline uint32_t read_le32(const uint8_t* buf) {
    return (uint32_t)buf[0] | ((uint32_t)buf[1] << 8) |
           ((uint32_t)buf[2] << 16) | ((uint32_t)buf[3] << 24);
}

static inline void write_le32(uint8_t* buf, uint32_t val) {
    buf[0] = val & 0xFF;
    buf[1] = (val >> 8) & 0xFF;
    buf[2] = (val >> 16) & 0xFF;
    buf[3] = (val >> 24) & 0xFF;
}

/* Method selector from first 4 bytes of calldata */
static inline uint32_t get_selector(const uint8_t* calldata, size_t len) {
    if (len < 4) return 0;
    return ((uint32_t)calldata[0] << 24) | ((uint32_t)calldata[1] << 16) |
           ((uint32_t)calldata[2] << 8) | (uint32_t)calldata[3];
}

#endif /* ANUBIS_VM_H */
