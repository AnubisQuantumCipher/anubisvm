/*
 * Counter Contract - Sample Native Contract for AnubisVM
 *
 * This is a simple counter contract that demonstrates the native
 * contract interface. It exports the standard contract_execute function.
 *
 * Compile with:
 *   clang -shared -fPIC -o libcounter.dylib counter_contract.c
 *
 * Entry points (via calldata[0]):
 *   0x00 = Get counter value
 *   0x01 = Increment counter
 *   0x02 = Set counter (value in calldata[1..4] as LE32)
 */

#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* Internal state - for demo purposes only */
/* In real contracts, this would use storage callbacks */
static uint64_t counter_value = 0;

/* Gas costs */
#define GAS_READ   100
#define GAS_WRITE  1000
#define GAS_BASE   50

/*
 * Contract entry point
 *
 * Returns: 0 = success, non-zero = error code
 *   1 = out of gas
 *   2 = invalid entry point
 *   3 = invalid calldata length
 */
int contract_execute(
    const uint8_t* calldata,
    size_t calldata_len,
    uint8_t* return_buf,
    size_t* return_len,
    uint64_t gas_limit,
    uint64_t* gas_used
) {
    uint64_t gas = GAS_BASE;

    /* Clear return buffer */
    *return_len = 0;

    /* Need at least 1 byte for entry point */
    if (calldata_len < 1) {
        *gas_used = gas;
        return 3;  /* Invalid calldata */
    }

    uint8_t entry_point = calldata[0];

    switch (entry_point) {
        case 0x00:  /* Get counter value */
            gas += GAS_READ;
            if (gas > gas_limit) {
                *gas_used = gas;
                return 1;  /* Out of gas */
            }

            /* Return counter as 8 bytes LE */
            if (return_buf != NULL) {
                return_buf[0] = (counter_value >> 0) & 0xFF;
                return_buf[1] = (counter_value >> 8) & 0xFF;
                return_buf[2] = (counter_value >> 16) & 0xFF;
                return_buf[3] = (counter_value >> 24) & 0xFF;
                return_buf[4] = (counter_value >> 32) & 0xFF;
                return_buf[5] = (counter_value >> 40) & 0xFF;
                return_buf[6] = (counter_value >> 48) & 0xFF;
                return_buf[7] = (counter_value >> 56) & 0xFF;
                *return_len = 8;
            }
            break;

        case 0x01:  /* Increment counter */
            gas += GAS_WRITE;
            if (gas > gas_limit) {
                *gas_used = gas;
                return 1;  /* Out of gas */
            }

            counter_value++;

            /* Return new value */
            if (return_buf != NULL) {
                return_buf[0] = (counter_value >> 0) & 0xFF;
                return_buf[1] = (counter_value >> 8) & 0xFF;
                return_buf[2] = (counter_value >> 16) & 0xFF;
                return_buf[3] = (counter_value >> 24) & 0xFF;
                return_buf[4] = (counter_value >> 32) & 0xFF;
                return_buf[5] = (counter_value >> 40) & 0xFF;
                return_buf[6] = (counter_value >> 48) & 0xFF;
                return_buf[7] = (counter_value >> 56) & 0xFF;
                *return_len = 8;
            }
            break;

        case 0x02:  /* Set counter */
            gas += GAS_WRITE;
            if (gas > gas_limit) {
                *gas_used = gas;
                return 1;  /* Out of gas */
            }

            /* Need 5 bytes: entry_point + 4-byte value */
            if (calldata_len < 5) {
                *gas_used = gas;
                return 3;  /* Invalid calldata length */
            }

            /* Read LE32 value from calldata[1..4] */
            counter_value = (uint64_t)calldata[1] |
                           ((uint64_t)calldata[2] << 8) |
                           ((uint64_t)calldata[3] << 16) |
                           ((uint64_t)calldata[4] << 24);

            /* Return new value */
            if (return_buf != NULL) {
                return_buf[0] = (counter_value >> 0) & 0xFF;
                return_buf[1] = (counter_value >> 8) & 0xFF;
                return_buf[2] = (counter_value >> 16) & 0xFF;
                return_buf[3] = (counter_value >> 24) & 0xFF;
                return_buf[4] = (counter_value >> 32) & 0xFF;
                return_buf[5] = (counter_value >> 40) & 0xFF;
                return_buf[6] = (counter_value >> 48) & 0xFF;
                return_buf[7] = (counter_value >> 56) & 0xFF;
                *return_len = 8;
            }
            break;

        default:
            *gas_used = gas;
            return 2;  /* Invalid entry point */
    }

    *gas_used = gas;
    return 0;  /* Success */
}

/* Version info for debugging */
const char* contract_version(void) {
    return "Counter Contract v1.0";
}
