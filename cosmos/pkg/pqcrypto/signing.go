// Package pqcrypto provides post-quantum cryptographic primitives for AegisVM.
//
// This package wraps the SPARK/Ada ML-DSA-87 implementation via CGO,
// providing Go-native interfaces for digital signatures.
//
// All operations are formally verified in the underlying SPARK code.
package pqcrypto

/*
#cgo CFLAGS: -I${SRCDIR}/../../../core/include
#cgo LDFLAGS: -L${SRCDIR}/../../../core/lib -laegisvm -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib -lgnat -lgnarl
#include "aegisvm.h"
#include <stdlib.h>
*/
import "C"

import (
	"crypto"
	"errors"
	"io"
	"unsafe"
)

// ML-DSA-87 key sizes (FIPS 204)
const (
	PublicKeySize  = 2592 // ML-DSA-87 public key
	SecretKeySize  = 4896 // ML-DSA-87 secret key
	SignatureSize  = 4627 // ML-DSA-87 signature (max)
	SeedSize       = 32   // Random seed for keygen
)

// Errors
var (
	ErrKeyGenFailed    = errors.New("pqcrypto: key generation failed")
	ErrSignFailed      = errors.New("pqcrypto: signing failed")
	ErrVerifyFailed    = errors.New("pqcrypto: verification failed")
	ErrInvalidKey      = errors.New("pqcrypto: invalid key")
	ErrInvalidSeed     = errors.New("pqcrypto: invalid seed")
)

// PublicKey represents an ML-DSA-87 public key.
type PublicKey [PublicKeySize]byte

// SecretKey represents an ML-DSA-87 secret key.
type SecretKey [SecretKeySize]byte

// Signature represents an ML-DSA-87 signature.
type Signature []byte

// GenerateKey generates an ML-DSA-87 keypair using internal RNG.
func GenerateKey(rand io.Reader) (PublicKey, SecretKey, error) {
	var pk PublicKey
	var sk SecretKey
	var seed [SeedSize]byte

	// Read random seed
	if rand != nil {
		if _, err := io.ReadFull(rand, seed[:]); err != nil {
			return pk, sk, err
		}
	}

	var seedPtr *C.uint8_t
	if rand != nil {
		seedPtr = (*C.uint8_t)(unsafe.Pointer(&seed[0]))
	}

	result := C.aegis_mldsa87_keygen(
		(*C.uint8_t)(unsafe.Pointer(&pk[0])),
		(*C.uint8_t)(unsafe.Pointer(&sk[0])),
		seedPtr,
	)

	if result != C.AEGIS_OK {
		return pk, sk, ErrKeyGenFailed
	}

	return pk, sk, nil
}

// Sign signs a message with ML-DSA-87.
func Sign(sk SecretKey, message []byte) (Signature, error) {
	if len(message) == 0 {
		return nil, errors.New("pqcrypto: empty message")
	}

	// Debug: Check if secret key looks valid (non-zero)
	allZero := true
	for i := 0; i < 32 && i < len(sk); i++ {
		if sk[i] != 0 {
			allZero = false
			break
		}
	}
	if allZero {
		return nil, errors.New("pqcrypto: secret key appears to be all zeros - key not loaded correctly")
	}

	sig := make([]byte, SignatureSize)
	var sigLen C.size_t

	result := C.aegis_mldsa87_sign(
		(*C.uint8_t)(unsafe.Pointer(&sig[0])),
		&sigLen,
		(*C.uint8_t)(unsafe.Pointer(&message[0])),
		C.size_t(len(message)),
		(*C.uint8_t)(unsafe.Pointer(&sk[0])),
	)

	if result != C.AEGIS_OK {
		return nil, ErrSignFailed
	}

	return sig[:sigLen], nil
}

// Verify verifies an ML-DSA-87 signature.
func Verify(pk PublicKey, message []byte, sig Signature) bool {
	if len(message) == 0 || len(sig) == 0 {
		return false
	}

	var valid C.bool

	result := C.aegis_mldsa87_verify(
		(*C.uint8_t)(unsafe.Pointer(&sig[0])),
		C.size_t(len(sig)),
		(*C.uint8_t)(unsafe.Pointer(&message[0])),
		C.size_t(len(message)),
		(*C.uint8_t)(unsafe.Pointer(&pk[0])),
		&valid,
	)

	return result == C.AEGIS_OK && bool(valid)
}

// Bytes returns the public key as a byte slice.
func (pk PublicKey) Bytes() []byte {
	return pk[:]
}

// Bytes returns the secret key as a byte slice.
func (sk SecretKey) Bytes() []byte {
	return sk[:]
}

// Public extracts the public key from a secret key.
// ML-DSA-87 secret keys contain the public key at a fixed offset.
func (sk SecretKey) Public() PublicKey {
	var pk PublicKey
	// The public key is embedded in the secret key
	// For ML-DSA-87, it starts at offset 64
	copy(pk[:], sk[64:64+PublicKeySize])
	return pk
}

// Sign implements crypto.Signer interface.
func (sk SecretKey) Sign(rand io.Reader, digest []byte, opts crypto.SignerOpts) ([]byte, error) {
	return Sign(sk, digest)
}

// PublicKeyFromBytes creates a PublicKey from a byte slice.
func PublicKeyFromBytes(data []byte) (PublicKey, error) {
	var pk PublicKey
	if len(data) != PublicKeySize {
		return pk, ErrInvalidKey
	}
	copy(pk[:], data)
	return pk, nil
}

// SecretKeyFromBytes creates a SecretKey from a byte slice.
func SecretKeyFromBytes(data []byte) (SecretKey, error) {
	var sk SecretKey
	if len(data) != SecretKeySize {
		return sk, ErrInvalidKey
	}
	copy(sk[:], data)
	return sk, nil
}

// GenerateKeyDeterministic generates an ML-DSA-87 keypair from a fixed 32-byte seed.
func GenerateKeyDeterministic(seed [SeedSize]byte) (PublicKey, SecretKey, error) {
	var pk PublicKey
	var sk SecretKey

	result := C.aegis_mldsa87_keygen(
		(*C.uint8_t)(unsafe.Pointer(&pk[0])),
		(*C.uint8_t)(unsafe.Pointer(&sk[0])),
		(*C.uint8_t)(unsafe.Pointer(&seed[0])),
	)

	if result != C.AEGIS_OK {
		return pk, sk, ErrKeyGenFailed
	}

	return pk, sk, nil
}
