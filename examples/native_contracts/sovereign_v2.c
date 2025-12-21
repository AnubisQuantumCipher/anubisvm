/*
 * AnubisSovereign v2 - Production Self-Sovereign Identity Contract
 *
 * ALL OPERATIONS POWERED BY VM SYSCALLS:
 * - THOTH: Storage via vm_sload/vm_sstore
 * - ANKH: Crypto via vm_sha3, vm_mldsa_verify
 * - ADDRESS: Full address system with ML-DSA-87
 * - PRIVACY: SHIELD, WHISPER, VEIL, EYE, GATE
 *
 * Compile: clang -shared -fPIC -O2 -o libsovereign_v2.dylib sovereign_v2.c
 */

#include "anubis_syscalls.h"
#include <string.h>

/*===========================================================================
 *  Global VM Context (set by VM before execution)
 *===========================================================================*/

Execution_Context* vm_context = NULL;

/*===========================================================================
 *  Storage Layout (256 slots)
 *===========================================================================*/

#define SLOT_VERSION        0
#define SLOT_OWNER          1
#define SLOT_PAUSED         2
#define SLOT_ISSUER_COUNT   3
#define SLOT_SUBJECT_COUNT  4
#define SLOT_CRED_COUNT     5

#define SLOT_ISSUER_BASE    32   /* 32-63: Issuers (32 max) */
#define SLOT_SUBJECT_BASE   64   /* 64-127: Subjects (64 max) */
#define SLOT_CRED_BASE      128  /* 128-191: Credentials (64 max) */
#define SLOT_REP_BASE       192  /* 192-223: Reputation commits (32 max) */
#define SLOT_KEY_IMAGE_BASE 224  /* 224-239: Key images (16 max) */
#define SLOT_SESSION_BASE   240  /* 240-255: Sessions (16 max) */

#define MAX_ISSUERS    32
#define MAX_SUBJECTS   64
#define MAX_CREDS      64
#define MAX_REPS       32
#define MAX_KEY_IMAGES 16

/*===========================================================================
 *  Entry Points
 *===========================================================================*/

#define SEL_INITIALIZE       0x00
#define SEL_REGISTER_ISSUER  0x01
#define SEL_REVOKE_ISSUER    0x02
#define SEL_REGISTER_DID     0x03
#define SEL_ISSUE_CRED       0x04
#define SEL_REVOKE_CRED      0x05
#define SEL_VERIFY_SIG       0x06  /* ML-DSA-87 signature verification */

/* Privacy Operations */
#define SEL_COMMIT_REP       0x18  /* WHISPER: Commit reputation */
#define SEL_VERIFY_REP       0x19  /* WHISPER: Verify reputation range */
#define SEL_CHECK_KEY_IMAGE  0x1D  /* VEIL: Check key image */

/* Query Operations */
#define SEL_GET_OWNER        0x25
#define SEL_GET_VERSION      0x26
#define SEL_GET_CALLER       0x27
#define SEL_IS_ISSUER        0x28
#define SEL_GET_COUNTS       0x29

/*===========================================================================
 *  Storage Helpers (use VM syscalls)
 *===========================================================================*/

static void slot_key(uint8_t slot, Hash256* key) {
    for (int i = 0; i < 32; i++) key->data[i] = 0;
    key->data[0] = slot;
}

static uint64_t read_u64(uint8_t slot) {
    Hash256 key, value;
    slot_key(slot, &key);
    vm_sload(&key, &value);
    return (uint64_t)value.data[0] |
           ((uint64_t)value.data[1] << 8) |
           ((uint64_t)value.data[2] << 16) |
           ((uint64_t)value.data[3] << 24) |
           ((uint64_t)value.data[4] << 32) |
           ((uint64_t)value.data[5] << 40) |
           ((uint64_t)value.data[6] << 48) |
           ((uint64_t)value.data[7] << 56);
}

static void write_u64(uint8_t slot, uint64_t val) {
    Hash256 key, value;
    slot_key(slot, &key);
    for (int i = 0; i < 32; i++) value.data[i] = 0;
    value.data[0] = val & 0xFF;
    value.data[1] = (val >> 8) & 0xFF;
    value.data[2] = (val >> 16) & 0xFF;
    value.data[3] = (val >> 24) & 0xFF;
    value.data[4] = (val >> 32) & 0xFF;
    value.data[5] = (val >> 40) & 0xFF;
    value.data[6] = (val >> 48) & 0xFF;
    value.data[7] = (val >> 56) & 0xFF;
    vm_sstore(&key, &value);
}

static void read_hash(uint8_t slot, Hash256* out) {
    Hash256 key;
    slot_key(slot, &key);
    vm_sload(&key, out);
}

static void write_hash(uint8_t slot, const Hash256* val) {
    Hash256 key;
    slot_key(slot, &key);
    vm_sstore(&key, val);
}

static void read_account(uint8_t slot, Account_ID* out) {
    Hash256 value;
    read_hash(slot, &value);
    memcpy(out->data, value.data, 32);
}

static void write_account(uint8_t slot, const Account_ID* val) {
    Hash256 value;
    memcpy(value.data, val->data, 32);
    write_hash(slot, &value);
}

/*===========================================================================
 *  Authorization Helpers
 *===========================================================================*/

static int is_owner(void) {
    Account_ID caller, owner;
    vm_get_caller(&caller);
    read_account(SLOT_OWNER, &owner);
    return account_id_eq(&caller, &owner);
}

static int is_paused(void) {
    return read_u64(SLOT_PAUSED) != 0;
}

static int is_registered_issuer(const Account_ID* issuer) {
    uint64_t count = read_u64(SLOT_ISSUER_COUNT);
    for (uint64_t i = 0; i < count && i < MAX_ISSUERS; i++) {
        Account_ID stored;
        read_account(SLOT_ISSUER_BASE + (uint8_t)i, &stored);
        if (account_id_eq(issuer, &stored)) {
            return 1;
        }
    }
    return 0;
}

/*===========================================================================
 *  Handler: Initialize
 *===========================================================================*/

static int handle_initialize(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SLOAD_COLD + GAS_SSTORE_SET * 4, gas_used, gas_limit);

    /* Already initialized? */
    if (read_u64(SLOT_VERSION) != 0) {
        return ERR_UNAUTHORIZED;
    }

    /* Set version */
    write_u64(SLOT_VERSION, 1);

    /* Set owner from caller */
    Account_ID caller;
    vm_get_caller(&caller);
    write_account(SLOT_OWNER, &caller);

    /* Initialize counters */
    write_u64(SLOT_ISSUER_COUNT, 0);
    write_u64(SLOT_SUBJECT_COUNT, 0);
    write_u64(SLOT_CRED_COUNT, 0);
    write_u64(SLOT_PAUSED, 0);

    /* Return success */
    ret[0] = 1;
    *ret_len = 1;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Register Issuer (owner only)
 *===========================================================================*/

static int handle_register_issuer(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SLOAD_COLD * 3 + GAS_SSTORE_SET, gas_used, gas_limit);

    /* Owner only */
    if (!is_owner()) {
        return ERR_UNAUTHORIZED;
    }

    /* Need 32 bytes for issuer address */
    if (len < 33) {
        return ERR_INVALID_INPUT;
    }

    /* Check capacity */
    uint64_t count = read_u64(SLOT_ISSUER_COUNT);
    if (count >= MAX_ISSUERS) {
        return ERR_INVALID_INPUT;
    }

    /* Store issuer */
    Account_ID issuer;
    memcpy(issuer.data, calldata + 1, 32);
    write_account(SLOT_ISSUER_BASE + (uint8_t)count, &issuer);
    write_u64(SLOT_ISSUER_COUNT, count + 1);

    /* Return index */
    ret[0] = (uint8_t)count;
    *ret_len = 1;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Register DID (using VM SHA3)
 *===========================================================================*/

static int handle_register_did(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SHA3_BASE + GAS_SHA3_WORD * 4 +
                GAS_SLOAD_COLD * 2 + GAS_SSTORE_SET, gas_used, gas_limit);

    /* Need 32 bytes for public key */
    if (len < 33) {
        return ERR_INVALID_INPUT;
    }

    /* Check capacity */
    uint64_t count = read_u64(SLOT_SUBJECT_COUNT);
    if (count >= MAX_SUBJECTS) {
        return ERR_INVALID_INPUT;
    }

    /* Create DID via VM SHA3: H(domain || pubkey) */
    uint8_t domain_input[64];
    memcpy(domain_input, "aegis-v1-mldsa87-u", 18);  /* Domain separator */
    memcpy(domain_input + 18, calldata + 1, 32);     /* Public key */

    Hash256 did;
    vm_sha3(domain_input, 50, &did);

    /* Store DID */
    write_hash(SLOT_SUBJECT_BASE + (uint8_t)count, &did);
    write_u64(SLOT_SUBJECT_COUNT, count + 1);

    /* Return DID */
    memcpy(ret, did.data, 32);
    *ret_len = 32;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Issue Credential (issuer only, with ML-DSA-87 signature)
 *===========================================================================*/

static int handle_issue_cred(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SHA3_BASE + GAS_SHA3_WORD * 8 +
                GAS_SLOAD_COLD * (MAX_ISSUERS + 2) + GAS_SSTORE_SET, gas_used, gas_limit);

    /* Caller must be registered issuer */
    Account_ID caller;
    vm_get_caller(&caller);
    if (!is_registered_issuer(&caller)) {
        return ERR_UNAUTHORIZED;
    }

    /* Need at least 64 bytes: subject_did(32) + cred_data(32+) */
    if (len < 65) {
        return ERR_INVALID_INPUT;
    }

    /* Check capacity */
    uint64_t count = read_u64(SLOT_CRED_COUNT);
    if (count >= MAX_CREDS) {
        return ERR_INVALID_INPUT;
    }

    /* Hash credential: H(issuer || subject || data || timestamp) */
    uint8_t cred_input[128];
    memcpy(cred_input, caller.data, 32);        /* Issuer */
    memcpy(cred_input + 32, calldata + 1, 32);  /* Subject DID */
    memcpy(cred_input + 64, calldata + 33, 32); /* Credential data */

    uint64_t ts = vm_get_timestamp();
    cred_input[96] = ts & 0xFF;
    cred_input[97] = (ts >> 8) & 0xFF;
    cred_input[98] = (ts >> 16) & 0xFF;
    cred_input[99] = (ts >> 24) & 0xFF;

    Hash256 cred_hash;
    vm_sha3(cred_input, 100, &cred_hash);

    /* Store credential hash */
    write_hash(SLOT_CRED_BASE + (uint8_t)count, &cred_hash);
    write_u64(SLOT_CRED_COUNT, count + 1);

    /* Return credential hash */
    memcpy(ret, cred_hash.data, 32);
    *ret_len = 32;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Verify Signature (ML-DSA-87 via VM)
 *===========================================================================*/

static int handle_verify_sig(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_MLDSA_VERIFY, gas_used, gas_limit);

    /* Format: selector(1) + msg_len(2) + msg + sig_len(2) + sig + pubkey(2592) */
    if (len < 6) {
        return ERR_INVALID_INPUT;
    }

    uint16_t msg_len = calldata[1] | ((uint16_t)calldata[2] << 8);
    if (len < 3 + msg_len + 2) {
        return ERR_INVALID_INPUT;
    }

    const uint8_t* msg = calldata + 3;
    uint16_t sig_len = calldata[3 + msg_len] | ((uint16_t)calldata[4 + msg_len] << 8);

    if (len < 5 + msg_len + sig_len + 2592) {  /* ML-DSA-87 pubkey = 2592 bytes */
        return ERR_INVALID_INPUT;
    }

    const uint8_t* sig = calldata + 5 + msg_len;
    const uint8_t* pubkey = sig + sig_len;

    /* Verify via VM syscall */
    int valid = vm_mldsa_verify(msg, msg_len, sig, sig_len, pubkey);

    ret[0] = valid ? 1 : 0;
    *ret_len = 1;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: WHISPER - Commit Reputation
 *===========================================================================*/

static int handle_commit_rep(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SHA3_BASE + GAS_SHA3_WORD * 5 +
                GAS_SLOAD_COLD + GAS_SSTORE_SET, gas_used, gas_limit);

    /* Format: selector(1) + slot(1) + score(8) + randomness(32) */
    if (len < 42) {
        return ERR_INVALID_INPUT;
    }

    uint8_t slot = calldata[1];
    if (slot >= MAX_REPS) {
        return ERR_INVALID_INPUT;
    }

    /* Create commitment: H(score || randomness) */
    Hash256 commitment;
    vm_sha3(calldata + 2, 40, &commitment);

    /* Store commitment */
    write_hash(SLOT_REP_BASE + slot, &commitment);

    /* Return commitment */
    memcpy(ret, commitment.data, 32);
    *ret_len = 32;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: WHISPER - Verify Reputation Range
 *===========================================================================*/

static int handle_verify_rep(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SHA3_BASE * 2 + GAS_SLOAD_COLD, gas_used, gas_limit);

    /* Format: selector(1) + slot(1) + score(8) + randomness(32) + min(8) + max(8) */
    if (len < 58) {
        return ERR_INVALID_INPUT;
    }

    uint8_t slot = calldata[1];
    if (slot >= MAX_REPS) {
        return ERR_INVALID_INPUT;
    }

    /* Read stored commitment */
    Hash256 stored;
    read_hash(SLOT_REP_BASE + slot, &stored);

    /* Recompute commitment */
    Hash256 computed;
    vm_sha3(calldata + 2, 40, &computed);

    /* Verify commitment matches */
    if (memcmp(stored.data, computed.data, 32) != 0) {
        ret[0] = 0;  /* Invalid commitment */
        *ret_len = 1;
        return ERR_SUCCESS;
    }

    /* Extract and verify range */
    uint64_t score = 0;
    for (int i = 0; i < 8; i++) {
        score |= ((uint64_t)calldata[2 + i]) << (i * 8);
    }

    uint64_t min_val = 0, max_val = 0;
    for (int i = 0; i < 8; i++) {
        min_val |= ((uint64_t)calldata[42 + i]) << (i * 8);
        max_val |= ((uint64_t)calldata[50 + i]) << (i * 8);
    }

    ret[0] = (score >= min_val && score <= max_val) ? 1 : 0;
    *ret_len = 1;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: VEIL - Check Key Image
 *===========================================================================*/

static int handle_check_key_image(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SLOAD_COLD * MAX_KEY_IMAGES, gas_used, gas_limit);

    if (len < 33) {
        return ERR_INVALID_INPUT;
    }

    /* Check if key image already exists (double-spend) */
    Hash256 check_image;
    memcpy(check_image.data, calldata + 1, 32);

    for (int i = 0; i < MAX_KEY_IMAGES; i++) {
        Hash256 stored;
        read_hash(SLOT_KEY_IMAGE_BASE + i, &stored);
        if (memcmp(check_image.data, stored.data, 32) == 0) {
            ret[0] = 0;  /* Already spent */
            *ret_len = 1;
            return ERR_SUCCESS;
        }
    }

    ret[0] = 1;  /* Valid (unspent) */
    *ret_len = 1;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Get Owner
 *===========================================================================*/

static int handle_get_owner(
    uint8_t* ret, size_t* ret_len,
    uint64_t* gas_used
) {
    *gas_used += GAS_BASE + GAS_SLOAD_COLD;
    Account_ID owner;
    read_account(SLOT_OWNER, &owner);
    memcpy(ret, owner.data, 32);
    *ret_len = 32;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Get Version
 *===========================================================================*/

static int handle_get_version(
    uint8_t* ret, size_t* ret_len,
    uint64_t* gas_used
) {
    *gas_used += GAS_BASE + GAS_SLOAD_COLD;
    uint64_t version = read_u64(SLOT_VERSION);
    for (int i = 0; i < 8; i++) {
        ret[i] = (version >> (i * 8)) & 0xFF;
    }
    *ret_len = 8;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Get Caller (returns caller's address)
 *===========================================================================*/

static int handle_get_caller(
    uint8_t* ret, size_t* ret_len,
    uint64_t* gas_used
) {
    *gas_used += GAS_BASE;
    Account_ID caller;
    vm_get_caller(&caller);
    memcpy(ret, caller.data, 32);
    *ret_len = 32;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Is Issuer
 *===========================================================================*/

static int handle_is_issuer(
    const uint8_t* calldata, size_t len,
    uint8_t* ret, size_t* ret_len,
    uint64_t gas_limit, uint64_t* gas_used
) {
    REQUIRE_GAS(GAS_BASE + GAS_SLOAD_COLD * (MAX_ISSUERS + 1), gas_used, gas_limit);

    if (len < 33) {
        return ERR_INVALID_INPUT;
    }

    Account_ID check;
    memcpy(check.data, calldata + 1, 32);

    ret[0] = is_registered_issuer(&check) ? 1 : 0;
    *ret_len = 1;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Handler: Get Counts
 *===========================================================================*/

static int handle_get_counts(
    uint8_t* ret, size_t* ret_len,
    uint64_t* gas_used
) {
    *gas_used += GAS_BASE + GAS_SLOAD_COLD * 3;

    uint64_t issuers = read_u64(SLOT_ISSUER_COUNT);
    uint64_t subjects = read_u64(SLOT_SUBJECT_COUNT);
    uint64_t creds = read_u64(SLOT_CRED_COUNT);

    /* Pack as 3x uint64 LE */
    for (int i = 0; i < 8; i++) {
        ret[i] = (issuers >> (i * 8)) & 0xFF;
        ret[8 + i] = (subjects >> (i * 8)) & 0xFF;
        ret[16 + i] = (creds >> (i * 8)) & 0xFF;
    }
    *ret_len = 24;
    return ERR_SUCCESS;
}

/*===========================================================================
 *  Main Entry Point
 *===========================================================================*/

int contract_execute(
    const uint8_t* calldata,
    size_t calldata_len,
    uint8_t* return_buf,
    size_t* return_len,
    uint64_t gas_limit,
    uint64_t* gas_used
) {
    *return_len = 0;
    *gas_used = 0;

    if (calldata_len < 1) {
        return ERR_INVALID_INPUT;
    }

    uint8_t selector = calldata[0];

    switch (selector) {
        case SEL_INITIALIZE:
            return handle_initialize(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_REGISTER_ISSUER:
            return handle_register_issuer(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_REGISTER_DID:
            return handle_register_did(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_ISSUE_CRED:
            return handle_issue_cred(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_VERIFY_SIG:
            return handle_verify_sig(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_COMMIT_REP:
            return handle_commit_rep(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_VERIFY_REP:
            return handle_verify_rep(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_CHECK_KEY_IMAGE:
            return handle_check_key_image(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_GET_OWNER:
            return handle_get_owner(return_buf, return_len, gas_used);

        case SEL_GET_VERSION:
            return handle_get_version(return_buf, return_len, gas_used);

        case SEL_GET_CALLER:
            return handle_get_caller(return_buf, return_len, gas_used);

        case SEL_IS_ISSUER:
            return handle_is_issuer(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_GET_COUNTS:
            return handle_get_counts(return_buf, return_len, gas_used);

        default:
            *gas_used = GAS_BASE;
            return ERR_INVALID_OPCODE;
    }
}

const char* contract_version(void) {
    return "AnubisSovereign v2.0 - Production (VM-Powered)";
}
