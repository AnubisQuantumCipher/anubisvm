// ML-KEM-1024 Key Encapsulation Mechanism
//
// This file provides post-quantum key encapsulation using ML-KEM-1024 (FIPS 203).
// Used for encrypted P2P communication and confidential transactions.

package pqcrypto

/*
#cgo CFLAGS: -I${SRCDIR}/../../../core/include
#cgo LDFLAGS: -L${SRCDIR}/../../../core/lib -laegisvm -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib -lgnat -lgnarl
#include "aegisvm.h"
*/
import "C"

import (
	"errors"
	"io"
	"unsafe"
)

// ML-KEM-1024 sizes (FIPS 203)
const (
	KEMPublicKeySize    = 1568 // ML-KEM-1024 encapsulation key
	KEMSecretKeySize    = 3168 // ML-KEM-1024 decapsulation key
	KEMCiphertextSize   = 1568 // ML-KEM-1024 ciphertext
	KEMSharedSecretSize = 32   // Shared secret
	KEMSeedSize         = 64   // Random seed for keygen
	KEMEncapsSeedSize   = 32   // Random seed for encaps
)

// KEM errors
var (
	ErrKEMKeyGenFailed = errors.New("pqcrypto: KEM key generation failed")
	ErrEncapsFailed    = errors.New("pqcrypto: encapsulation failed")
	ErrDecapsFailed    = errors.New("pqcrypto: decapsulation failed")
	ErrInvalidKEMKey   = errors.New("pqcrypto: invalid KEM key")
)

// KEMPublicKey represents an ML-KEM-1024 encapsulation key.
type KEMPublicKey [KEMPublicKeySize]byte

// KEMSecretKey represents an ML-KEM-1024 decapsulation key.
type KEMSecretKey [KEMSecretKeySize]byte

// KEMCiphertext represents an ML-KEM-1024 ciphertext.
type KEMCiphertext [KEMCiphertextSize]byte

// SharedSecret represents the shared secret from key encapsulation.
type SharedSecret [KEMSharedSecretSize]byte

// GenerateKEMKey generates an ML-KEM-1024 keypair.
func GenerateKEMKey(rand io.Reader) (KEMPublicKey, KEMSecretKey, error) {
	var pk KEMPublicKey
	var sk KEMSecretKey
	var seed [KEMSeedSize]byte

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

	result := C.aegis_mlkem1024_keygen(
		(*C.uint8_t)(unsafe.Pointer(&pk[0])),
		(*C.uint8_t)(unsafe.Pointer(&sk[0])),
		seedPtr,
	)

	if result != C.AEGIS_OK {
		return pk, sk, ErrKEMKeyGenFailed
	}

	return pk, sk, nil
}

// Encapsulate generates a shared secret and ciphertext for the given public key.
func Encapsulate(pk KEMPublicKey, rand io.Reader) (SharedSecret, KEMCiphertext, error) {
	var ss SharedSecret
	var ct KEMCiphertext
	var seed [KEMEncapsSeedSize]byte

	// Read random seed
	if rand != nil {
		if _, err := io.ReadFull(rand, seed[:]); err != nil {
			return ss, ct, err
		}
	}

	var seedPtr *C.uint8_t
	if rand != nil {
		seedPtr = (*C.uint8_t)(unsafe.Pointer(&seed[0]))
	}

	result := C.aegis_mlkem1024_encaps(
		(*C.uint8_t)(unsafe.Pointer(&ct[0])),
		(*C.uint8_t)(unsafe.Pointer(&ss[0])),
		(*C.uint8_t)(unsafe.Pointer(&pk[0])),
		seedPtr,
	)

	if result != C.AEGIS_OK {
		return ss, ct, ErrEncapsFailed
	}

	return ss, ct, nil
}

// Decapsulate recovers the shared secret from a ciphertext.
func Decapsulate(sk KEMSecretKey, ct KEMCiphertext) (SharedSecret, error) {
	var ss SharedSecret

	result := C.aegis_mlkem1024_decaps(
		(*C.uint8_t)(unsafe.Pointer(&ss[0])),
		(*C.uint8_t)(unsafe.Pointer(&ct[0])),
		(*C.uint8_t)(unsafe.Pointer(&sk[0])),
	)

	if result != C.AEGIS_OK {
		return ss, ErrDecapsFailed
	}

	return ss, nil
}

// Bytes returns the public key as a byte slice.
func (pk KEMPublicKey) Bytes() []byte {
	return pk[:]
}

// Bytes returns the secret key as a byte slice.
func (sk KEMSecretKey) Bytes() []byte {
	return sk[:]
}

// Bytes returns the ciphertext as a byte slice.
func (ct KEMCiphertext) Bytes() []byte {
	return ct[:]
}

// Bytes returns the shared secret as a byte slice.
func (ss SharedSecret) Bytes() []byte {
	return ss[:]
}

// KEMPublicKeyFromBytes creates a KEMPublicKey from a byte slice.
func KEMPublicKeyFromBytes(data []byte) (KEMPublicKey, error) {
	var pk KEMPublicKey
	if len(data) != KEMPublicKeySize {
		return pk, ErrInvalidKEMKey
	}
	copy(pk[:], data)
	return pk, nil
}

// KEMSecretKeyFromBytes creates a KEMSecretKey from a byte slice.
func KEMSecretKeyFromBytes(data []byte) (KEMSecretKey, error) {
	var sk KEMSecretKey
	if len(data) != KEMSecretKeySize {
		return sk, ErrInvalidKEMKey
	}
	copy(sk[:], data)
	return sk, nil
}

// KEMCiphertextFromBytes creates a KEMCiphertext from a byte slice.
func KEMCiphertextFromBytes(data []byte) (KEMCiphertext, error) {
	var ct KEMCiphertext
	if len(data) != KEMCiphertextSize {
		return ct, errors.New("pqcrypto: invalid ciphertext size")
	}
	copy(ct[:], data)
	return ct, nil
}
