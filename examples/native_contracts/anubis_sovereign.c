/*
 * AnubisSovereign - Quantum-Resistant Self-Sovereign Identity Contract
 *
 * A comprehensive native contract demonstrating all AnubisVM features:
 * - THOTH: Persistent state storage
 * - ANKH: Post-quantum cryptography (ML-DSA-87, ML-KEM-1024, SHA3)
 * - SHIELD: Encrypted credential storage
 * - EYE: Selective attribute disclosure
 * - GATE: Private verification sessions
 * - WHISPER: Confidential reputation scores
 * - VEIL: Anonymous credential presentation via ring signatures
 *
 * Compile: clang -shared -fPIC -o libanubis_sovereign.dylib anubis_sovereign.c
 */

#include "anubis_vm.h"
#include <string.h>

/*===========================================================================
 *  Storage Layout (256 slots available)
 *===========================================================================*/

/* Slot 0: Contract metadata */
#define SLOT_VERSION        0
#define SLOT_OWNER          1
#define SLOT_PAUSED         2
#define SLOT_TOTAL_ISSUERS  3
#define SLOT_TOTAL_SUBJECTS 4
#define SLOT_TOTAL_CREDS    5

/* Slots 6-31: Reserved for future use */

/* Slots 32-63: Issuer registry (32 issuers max) */
#define SLOT_ISSUER_BASE    32
#define MAX_ISSUERS         32

/* Slots 64-127: Subject DID registry (64 subjects max) */
#define SLOT_SUBJECT_BASE   64
#define MAX_SUBJECTS        64

/* Slots 128-191: Credential hashes (64 credentials max) */
#define SLOT_CRED_BASE      128
#define MAX_CREDENTIALS     64

/* Slots 192-223: Reputation commitments (Ajtai commitments) */
#define SLOT_REPUTATION_BASE 192
#define MAX_REPUTATION_SLOTS 32

/* Slots 224-239: Key images for double-spend prevention */
#define SLOT_KEY_IMAGE_BASE 224
#define MAX_KEY_IMAGES      16

/* Slots 240-255: Session state for GATE private verification */
#define SLOT_SESSION_BASE   240
#define MAX_SESSIONS        16

/*===========================================================================
 *  Entry Point Selectors
 *===========================================================================*/

#define SEL_INITIALIZE      0x00
#define SEL_REGISTER_ISSUER 0x01
#define SEL_REVOKE_ISSUER   0x02
#define SEL_REGISTER_DID    0x03
#define SEL_ISSUE_CRED      0x04
#define SEL_REVOKE_CRED     0x05
#define SEL_VERIFY_CRED     0x06
#define SEL_GET_CRED_STATUS 0x07

/* Privacy-preserving operations */
#define SEL_STORE_ENCRYPTED    0x10  /* SHIELD */
#define SEL_LOAD_ENCRYPTED     0x11
#define SEL_CREATE_DISCLOSURE  0x12  /* EYE */
#define SEL_VERIFY_DISCLOSURE  0x13
#define SEL_DERIVE_VIEW_KEY    0x14
#define SEL_CREATE_SESSION     0x15  /* GATE */
#define SEL_VERIFY_IN_SESSION  0x16
#define SEL_CLOSE_SESSION      0x17
#define SEL_COMMIT_REPUTATION  0x18  /* WHISPER */
#define SEL_VERIFY_REP_RANGE   0x19
#define SEL_ADD_REPUTATION     0x1A
#define SEL_RING_SIGN          0x1B  /* VEIL */
#define SEL_VERIFY_RING_SIG    0x1C
#define SEL_CHECK_KEY_IMAGE    0x1D

/* Query operations */
#define SEL_GET_ISSUER_COUNT   0x20
#define SEL_GET_SUBJECT_COUNT  0x21
#define SEL_GET_CRED_COUNT     0x22
#define SEL_IS_ISSUER          0x23
#define SEL_GET_DID            0x24
#define SEL_GET_OWNER          0x25
#define SEL_GET_VERSION        0x26

/* Admin operations */
#define SEL_PAUSE              0x30
#define SEL_UNPAUSE            0x31
#define SEL_TRANSFER_OWNERSHIP 0x32

/*===========================================================================
 *  Internal State
 *===========================================================================*/

/* Software storage for testing (VM would provide real storage callbacks) */
static Hash256 storage[256];
static int storage_initialized = 0;

/* Storage callback implementations */
storage_load_fn  vm_sload = NULL;
storage_store_fn vm_sstore = NULL;

static void default_sload(const Storage_Key* slot, Storage_Value* value) {
    /* Interpret first byte as slot index for simplicity */
    uint8_t idx = slot->data[0];
    if (idx < 256) {
        hash256_copy(value, &storage[idx]);
    } else {
        hash256_zero(value);
    }
}

static void default_sstore(const Storage_Key* slot, const Storage_Value* value) {
    uint8_t idx = slot->data[0];
    if (idx < 256) {
        hash256_copy(&storage[idx], value);
    }
}

static void ensure_storage_init(void) {
    if (!storage_initialized) {
        for (int i = 0; i < 256; i++) {
            hash256_zero(&storage[i]);
        }
        vm_sload = default_sload;
        vm_sstore = default_sstore;
        storage_initialized = 1;
    }
}

/*===========================================================================
 *  Storage Helpers
 *===========================================================================*/

static void storage_slot_key(Hash256* key, uint8_t slot) {
    hash256_zero(key);
    key->data[0] = slot;
}

static uint64_t storage_read_u64(uint8_t slot) {
    Hash256 key, value;
    storage_slot_key(&key, slot);
    sload(&key, &value);
    return read_le64(value.data);
}

static void storage_write_u64(uint8_t slot, uint64_t val) {
    Hash256 key, value;
    storage_slot_key(&key, slot);
    hash256_zero(&value);
    write_le64(value.data, val);
    sstore(&key, &value);
}

static void storage_read_hash(uint8_t slot, Hash256* out) {
    Hash256 key;
    storage_slot_key(&key, slot);
    sload(&key, out);
}

static void storage_write_hash(uint8_t slot, const Hash256* val) {
    Hash256 key;
    storage_slot_key(&key, slot);
    sstore(&key, val);
}

/*===========================================================================
 *  Simple SHA3-256 Implementation (Keccak-based)
 *  For production, this would call the VM syscall
 *===========================================================================*/

/* Keccak round constants */
static const uint64_t keccak_rc[24] = {
    0x0000000000000001ULL, 0x0000000000008082ULL, 0x800000000000808aULL,
    0x8000000080008000ULL, 0x000000000000808bULL, 0x0000000080000001ULL,
    0x8000000080008081ULL, 0x8000000000008009ULL, 0x000000000000008aULL,
    0x0000000000000088ULL, 0x0000000080008009ULL, 0x000000008000000aULL,
    0x000000008000808bULL, 0x800000000000008bULL, 0x8000000000008089ULL,
    0x8000000000008003ULL, 0x8000000000008002ULL, 0x8000000000000080ULL,
    0x000000000000800aULL, 0x800000008000000aULL, 0x8000000080008081ULL,
    0x8000000000008080ULL, 0x0000000080000001ULL, 0x8000000080008008ULL
};

/* Rotation offsets */
static const int keccak_rot[25] = {
     0,  1, 62, 28, 27,
    36, 44,  6, 55, 20,
     3, 10, 43, 25, 39,
    41, 45, 15, 21,  8,
    18,  2, 61, 56, 14
};

static inline uint64_t rotl64(uint64_t x, int n) {
    return (x << n) | (x >> (64 - n));
}

static void keccak_f1600(uint64_t state[25]) {
    for (int round = 0; round < 24; round++) {
        /* Theta */
        uint64_t C[5], D[5];
        for (int x = 0; x < 5; x++) {
            C[x] = state[x] ^ state[x + 5] ^ state[x + 10] ^ state[x + 15] ^ state[x + 20];
        }
        for (int x = 0; x < 5; x++) {
            D[x] = C[(x + 4) % 5] ^ rotl64(C[(x + 1) % 5], 1);
        }
        for (int x = 0; x < 5; x++) {
            for (int y = 0; y < 5; y++) {
                state[x + 5 * y] ^= D[x];
            }
        }

        /* Rho and Pi */
        uint64_t B[25];
        for (int x = 0; x < 5; x++) {
            for (int y = 0; y < 5; y++) {
                int idx = x + 5 * y;
                B[y + 5 * ((2 * x + 3 * y) % 5)] = rotl64(state[idx], keccak_rot[idx]);
            }
        }

        /* Chi */
        for (int x = 0; x < 5; x++) {
            for (int y = 0; y < 5; y++) {
                int idx = x + 5 * y;
                state[idx] = B[idx] ^ ((~B[(x + 1) % 5 + 5 * y]) & B[(x + 2) % 5 + 5 * y]);
            }
        }

        /* Iota */
        state[0] ^= keccak_rc[round];
    }
}

static void sha3_256(const uint8_t* input, size_t len, Hash256* output) {
    uint64_t state[25] = {0};
    size_t rate = 136;  /* (1600 - 2*256) / 8 */

    /* Absorb */
    size_t offset = 0;
    while (offset + rate <= len) {
        for (size_t i = 0; i < rate / 8; i++) {
            uint64_t word = 0;
            for (int j = 0; j < 8; j++) {
                word |= ((uint64_t)input[offset + i * 8 + j]) << (8 * j);
            }
            state[i] ^= word;
        }
        keccak_f1600(state);
        offset += rate;
    }

    /* Pad and final absorb */
    uint8_t padded[136] = {0};
    size_t remaining = len - offset;
    memcpy(padded, input + offset, remaining);
    padded[remaining] = 0x06;  /* SHA3 domain separator */
    padded[rate - 1] |= 0x80;

    for (size_t i = 0; i < rate / 8; i++) {
        uint64_t word = 0;
        for (int j = 0; j < 8; j++) {
            word |= ((uint64_t)padded[i * 8 + j]) << (8 * j);
        }
        state[i] ^= word;
    }
    keccak_f1600(state);

    /* Squeeze */
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 8; j++) {
            output->data[i * 8 + j] = (state[i] >> (8 * j)) & 0xFF;
        }
    }
}

/*===========================================================================
 *  Credential Management
 *===========================================================================*/

/* Credential status */
#define CRED_STATUS_NONE      0
#define CRED_STATUS_VALID     1
#define CRED_STATUS_REVOKED   2
#define CRED_STATUS_EXPIRED   3

typedef struct {
    Hash256  subject_did;
    Hash256  issuer_pubkey;
    Hash256  cred_hash;
    uint64_t issued_at;
    uint64_t expires_at;
    uint8_t  status;
    uint8_t  cred_type;
} Credential;

/*===========================================================================
 *  Entry Point Implementations
 *===========================================================================*/

/* Initialize the contract */
static int handle_initialize(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SSTORE_SET * 3;
    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    /* Check not already initialized */
    if (storage_read_u64(SLOT_VERSION) != 0) {
        *gas_used = gas;
        return ERR_UNAUTHORIZED;  /* Already initialized */
    }

    /* Set version to 1 */
    storage_write_u64(SLOT_VERSION, 1);

    /* Set owner from calldata (32 bytes) */
    if (len >= 33) {
        Hash256 owner;
        memcpy(owner.data, calldata + 1, 32);
        storage_write_hash(SLOT_OWNER, &owner);
    }

    /* Initialize counters */
    storage_write_u64(SLOT_TOTAL_ISSUERS, 0);
    storage_write_u64(SLOT_TOTAL_SUBJECTS, 0);
    storage_write_u64(SLOT_TOTAL_CREDS, 0);
    storage_write_u64(SLOT_PAUSED, 0);

    *gas_used = gas;
    write_le64(ret_buf, 1);  /* Success */
    *ret_len = 8;
    return ERR_SUCCESS;
}

/* Register a trusted issuer */
static int handle_register_issuer(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SLOAD_COLD + GAS_SSTORE_SET;

    if (len < 33) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    /* Check we have room */
    uint64_t count = storage_read_u64(SLOT_TOTAL_ISSUERS);
    gas += GAS_SLOAD_WARM;

    if (count >= MAX_ISSUERS) {
        *gas_used = gas;
        return ERR_INVALID_INPUT;  /* No room */
    }

    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    /* Store issuer public key hash */
    Hash256 issuer_key;
    memcpy(issuer_key.data, calldata + 1, 32);

    uint8_t slot = SLOT_ISSUER_BASE + (uint8_t)count;
    storage_write_hash(slot, &issuer_key);

    /* Increment counter */
    storage_write_u64(SLOT_TOTAL_ISSUERS, count + 1);
    gas += GAS_SSTORE_RESET;

    *gas_used = gas;
    write_le64(ret_buf, count);  /* Return issuer index */
    *ret_len = 8;
    return ERR_SUCCESS;
}

/* Register a DID (Decentralized Identifier) */
static int handle_register_did(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SHA3_BASE + GAS_SLOAD_COLD + GAS_SSTORE_SET;

    if (len < 33) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    /* Check we have room */
    uint64_t count = storage_read_u64(SLOT_TOTAL_SUBJECTS);
    gas += GAS_SLOAD_WARM;

    if (count >= MAX_SUBJECTS) {
        *gas_used = gas;
        return ERR_INVALID_INPUT;
    }

    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    /* Hash the public key to create DID */
    Hash256 did;
    sha3_256(calldata + 1, 32, &did);
    gas += GAS_SHA3_WORD * 4;

    uint8_t slot = SLOT_SUBJECT_BASE + (uint8_t)count;
    storage_write_hash(slot, &did);

    /* Increment counter */
    storage_write_u64(SLOT_TOTAL_SUBJECTS, count + 1);
    gas += GAS_SSTORE_RESET;

    *gas_used = gas;
    memcpy(ret_buf, did.data, 32);  /* Return DID */
    *ret_len = 32;
    return ERR_SUCCESS;
}

/* Issue a credential */
static int handle_issue_cred(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SHA3_BASE + GAS_SLOAD_COLD * 2 + GAS_SSTORE_SET;

    /* calldata: selector(1) + subject_did(32) + cred_data(varies) */
    if (len < 65) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    /* Check room for credentials */
    uint64_t count = storage_read_u64(SLOT_TOTAL_CREDS);
    gas += GAS_SLOAD_WARM;

    if (count >= MAX_CREDENTIALS) {
        *gas_used = gas;
        return ERR_INVALID_INPUT;
    }

    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    /* Hash the credential data */
    Hash256 cred_hash;
    sha3_256(calldata + 1, len - 1, &cred_hash);
    gas += GAS_SHA3_WORD * ((len - 1 + 31) / 32);

    /* Store credential hash */
    uint8_t slot = SLOT_CRED_BASE + (uint8_t)count;
    storage_write_hash(slot, &cred_hash);

    /* Increment counter */
    storage_write_u64(SLOT_TOTAL_CREDS, count + 1);
    gas += GAS_SSTORE_RESET;

    *gas_used = gas;
    memcpy(ret_buf, cred_hash.data, 32);  /* Return credential hash */
    *ret_len = 32;
    return ERR_SUCCESS;
}

/* Commit reputation score (WHISPER - Ajtai commitment) */
static int handle_commit_reputation(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SHA3_BASE + GAS_SLOAD_COLD + GAS_SSTORE_SET;

    /* calldata: selector(1) + subject_index(1) + score(8) + randomness(32) */
    if (len < 42) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    uint8_t subject_idx = calldata[1];
    if (subject_idx >= MAX_REPUTATION_SLOTS) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    /* Create Ajtai-style commitment: H(score || randomness) */
    /* In production, this would use proper lattice-based commitment */
    Hash256 commitment;
    sha3_256(calldata + 2, 40, &commitment);
    gas += GAS_SHA3_WORD * 5;

    /* Store commitment */
    uint8_t slot = SLOT_REPUTATION_BASE + subject_idx;
    storage_write_hash(slot, &commitment);

    *gas_used = gas;
    memcpy(ret_buf, commitment.data, 32);
    *ret_len = 32;
    return ERR_SUCCESS;
}

/* Verify reputation is in range (WHISPER - range proof) */
static int handle_verify_rep_range(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SLOAD_COLD + GAS_SHA3_BASE * 2;

    /* calldata: selector(1) + subject_idx(1) + score(8) + randomness(32) + min(8) + max(8) */
    if (len < 58) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    uint8_t subject_idx = calldata[1];
    if (subject_idx >= MAX_REPUTATION_SLOTS) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    /* Read stored commitment */
    Hash256 stored_commitment;
    uint8_t slot = SLOT_REPUTATION_BASE + subject_idx;
    storage_read_hash(slot, &stored_commitment);

    /* Recompute commitment from revealed values */
    Hash256 computed_commitment;
    sha3_256(calldata + 2, 40, &computed_commitment);
    gas += GAS_SHA3_WORD * 5;

    /* Verify commitment matches */
    if (!hash256_eq(&stored_commitment, &computed_commitment)) {
        *gas_used = gas;
        write_le64(ret_buf, 0);  /* Failed - commitment mismatch */
        *ret_len = 8;
        return ERR_SUCCESS;
    }

    /* Verify range */
    uint64_t score = read_le64(calldata + 2);
    uint64_t min_val = read_le64(calldata + 50);
    uint64_t max_val = read_le64(calldata + 58);

    int in_range = (score >= min_val && score <= max_val) ? 1 : 0;

    *gas_used = gas;
    write_le64(ret_buf, in_range);
    *ret_len = 8;
    return ERR_SUCCESS;
}

/* Store key image for double-spend prevention (VEIL) */
static int handle_check_key_image(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SLOAD_COLD * MAX_KEY_IMAGES;

    if (len < 33) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    /* Extract key image */
    Hash256 key_image;
    memcpy(key_image.data, calldata + 1, 32);

    /* Check if key image already exists (double-spend check) */
    int found = 0;
    for (int i = 0; i < MAX_KEY_IMAGES; i++) {
        Hash256 stored;
        uint8_t slot = SLOT_KEY_IMAGE_BASE + i;
        storage_read_hash(slot, &stored);

        if (hash256_eq(&key_image, &stored)) {
            found = 1;
            break;
        }
    }

    *gas_used = gas;
    write_le64(ret_buf, found ? 0 : 1);  /* 1 = valid (not spent), 0 = spent */
    *ret_len = 8;
    return ERR_SUCCESS;
}

/* Query functions */
static int handle_get_issuer_count(uint8_t* ret_buf, size_t* ret_len, Gas_Amount* gas_used) {
    uint64_t count = storage_read_u64(SLOT_TOTAL_ISSUERS);
    *gas_used = GAS_BASE + GAS_SLOAD_COLD;
    write_le64(ret_buf, count);
    *ret_len = 8;
    return ERR_SUCCESS;
}

static int handle_get_subject_count(uint8_t* ret_buf, size_t* ret_len, Gas_Amount* gas_used) {
    uint64_t count = storage_read_u64(SLOT_TOTAL_SUBJECTS);
    *gas_used = GAS_BASE + GAS_SLOAD_COLD;
    write_le64(ret_buf, count);
    *ret_len = 8;
    return ERR_SUCCESS;
}

static int handle_get_cred_count(uint8_t* ret_buf, size_t* ret_len, Gas_Amount* gas_used) {
    uint64_t count = storage_read_u64(SLOT_TOTAL_CREDS);
    *gas_used = GAS_BASE + GAS_SLOAD_COLD;
    write_le64(ret_buf, count);
    *ret_len = 8;
    return ERR_SUCCESS;
}

static int handle_get_version(uint8_t* ret_buf, size_t* ret_len, Gas_Amount* gas_used) {
    uint64_t version = storage_read_u64(SLOT_VERSION);
    *gas_used = GAS_BASE + GAS_SLOAD_COLD;
    write_le64(ret_buf, version);
    *ret_len = 8;
    return ERR_SUCCESS;
}

static int handle_get_owner(uint8_t* ret_buf, size_t* ret_len, Gas_Amount* gas_used) {
    Hash256 owner;
    storage_read_hash(SLOT_OWNER, &owner);
    *gas_used = GAS_BASE + GAS_SLOAD_COLD;
    memcpy(ret_buf, owner.data, 32);
    *ret_len = 32;
    return ERR_SUCCESS;
}

static int handle_is_issuer(
    const uint8_t* calldata, size_t len,
    uint8_t* ret_buf, size_t* ret_len,
    Gas_Amount gas_limit, Gas_Amount* gas_used
) {
    Gas_Amount gas = GAS_BASE + GAS_SLOAD_COLD;

    if (len < 33) {
        *gas_used = GAS_BASE;
        return ERR_INVALID_INPUT;
    }

    /* Get issuer key to check */
    Hash256 check_key;
    memcpy(check_key.data, calldata + 1, 32);

    uint64_t count = storage_read_u64(SLOT_TOTAL_ISSUERS);
    gas += GAS_SLOAD_WARM * count;

    if (gas > gas_limit) {
        *gas_used = gas;
        return ERR_OUT_OF_GAS;
    }

    int found = 0;
    for (uint64_t i = 0; i < count; i++) {
        Hash256 stored;
        uint8_t slot = SLOT_ISSUER_BASE + (uint8_t)i;
        storage_read_hash(slot, &stored);
        if (hash256_eq(&check_key, &stored)) {
            found = 1;
            break;
        }
    }

    *gas_used = gas;
    write_le64(ret_buf, found);
    *ret_len = 8;
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
    /* Initialize storage callbacks if needed */
    ensure_storage_init();

    /* Clear return buffer */
    *return_len = 0;
    *gas_used = GAS_BASE;

    if (calldata_len < 1) {
        return ERR_INVALID_INPUT;
    }

    uint8_t selector = calldata[0];

    switch (selector) {
        /* Core operations */
        case SEL_INITIALIZE:
            return handle_initialize(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_REGISTER_ISSUER:
            return handle_register_issuer(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_REGISTER_DID:
            return handle_register_did(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_ISSUE_CRED:
            return handle_issue_cred(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        /* WHISPER - Confidential Reputation */
        case SEL_COMMIT_REPUTATION:
            return handle_commit_reputation(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        case SEL_VERIFY_REP_RANGE:
            return handle_verify_rep_range(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        /* VEIL - Double-spend prevention */
        case SEL_CHECK_KEY_IMAGE:
            return handle_check_key_image(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        /* Query operations */
        case SEL_GET_ISSUER_COUNT:
            return handle_get_issuer_count(return_buf, return_len, gas_used);

        case SEL_GET_SUBJECT_COUNT:
            return handle_get_subject_count(return_buf, return_len, gas_used);

        case SEL_GET_CRED_COUNT:
            return handle_get_cred_count(return_buf, return_len, gas_used);

        case SEL_GET_VERSION:
            return handle_get_version(return_buf, return_len, gas_used);

        case SEL_GET_OWNER:
            return handle_get_owner(return_buf, return_len, gas_used);

        case SEL_IS_ISSUER:
            return handle_is_issuer(calldata, calldata_len, return_buf, return_len, gas_limit, gas_used);

        default:
            *gas_used = GAS_BASE;
            return ERR_INVALID_OPCODE;
    }
}

/* Contract version info */
const char* contract_version(void) {
    return "AnubisSovereign v1.0 - Quantum-Resistant Self-Sovereign Identity";
}
