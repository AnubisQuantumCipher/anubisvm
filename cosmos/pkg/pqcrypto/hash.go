// SHA3 and SHAKE hash functions
//
// This file provides post-quantum secure hash functions via the SPARK/Ada
// implementation of FIPS 202.

package pqcrypto

/*
#cgo CFLAGS: -I${SRCDIR}/../../../core/include
#cgo LDFLAGS: -L${SRCDIR}/../../../core/lib -laegisvm -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib -lgnat -lgnarl

#include "aegisvm.h"
*/
import "C"

import (
	"errors"
	"unsafe"
)

// Hash sizes
const (
	SHA3_256_Size = 32
	SHA3_512_Size = 64
)

// Hash errors
var (
	ErrHashFailed = errors.New("pqcrypto: hash operation failed")
)

// SHA3_256 computes the SHA3-256 hash of the input.
func SHA3_256(data []byte) ([SHA3_256_Size]byte, error) {
	var hash [SHA3_256_Size]byte

	var dataPtr *C.uint8_t
	if len(data) > 0 {
		dataPtr = (*C.uint8_t)(unsafe.Pointer(&data[0]))
	}

	result := C.aegis_sha3_256(
		(*C.uint8_t)(unsafe.Pointer(&hash[0])),
		dataPtr,
		C.size_t(len(data)),
	)

	if result != C.AEGIS_OK {
		return hash, ErrHashFailed
	}

	return hash, nil
}

// SHA3_512 computes the SHA3-512 hash of the input.
func SHA3_512(data []byte) ([SHA3_512_Size]byte, error) {
	var hash [SHA3_512_Size]byte

	var dataPtr *C.uint8_t
	if len(data) > 0 {
		dataPtr = (*C.uint8_t)(unsafe.Pointer(&data[0]))
	}

	result := C.aegis_sha3_512(
		(*C.uint8_t)(unsafe.Pointer(&hash[0])),
		dataPtr,
		C.size_t(len(data)),
	)

	if result != C.AEGIS_OK {
		return hash, ErrHashFailed
	}

	return hash, nil
}

// Keccak256 computes the Keccak-256 hash (Ethereum-compatible).
func Keccak256(data []byte) ([SHA3_256_Size]byte, error) {
	var hash [SHA3_256_Size]byte

	var dataPtr *C.uint8_t
	if len(data) > 0 {
		dataPtr = (*C.uint8_t)(unsafe.Pointer(&data[0]))
	}

	result := C.aegis_keccak256(
		(*C.uint8_t)(unsafe.Pointer(&hash[0])),
		dataPtr,
		C.size_t(len(data)),
	)

	if result != C.AEGIS_OK {
		return hash, ErrHashFailed
	}

	return hash, nil
}

// SHAKE128 computes SHAKE128 XOF output of arbitrary length.
func SHAKE128(data []byte, outputLen int) ([]byte, error) {
	if outputLen <= 0 {
		return nil, errors.New("pqcrypto: invalid output length")
	}

	output := make([]byte, outputLen)

	var dataPtr *C.uint8_t
	if len(data) > 0 {
		dataPtr = (*C.uint8_t)(unsafe.Pointer(&data[0]))
	}

	result := C.aegis_shake128(
		(*C.uint8_t)(unsafe.Pointer(&output[0])),
		C.size_t(outputLen),
		dataPtr,
		C.size_t(len(data)),
	)

	if result != C.AEGIS_OK {
		return nil, ErrHashFailed
	}

	return output, nil
}

// SHAKE256 computes SHAKE256 XOF output of arbitrary length.
func SHAKE256(data []byte, outputLen int) ([]byte, error) {
	if outputLen <= 0 {
		return nil, errors.New("pqcrypto: invalid output length")
	}

	output := make([]byte, outputLen)

	var dataPtr *C.uint8_t
	if len(data) > 0 {
		dataPtr = (*C.uint8_t)(unsafe.Pointer(&data[0]))
	}

	result := C.aegis_shake256(
		(*C.uint8_t)(unsafe.Pointer(&output[0])),
		C.size_t(outputLen),
		dataPtr,
		C.size_t(len(data)),
	)

	if result != C.AEGIS_OK {
		return nil, ErrHashFailed
	}

	return output, nil
}
