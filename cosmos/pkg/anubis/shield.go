// Shield provides encrypted state operations for ANUBIS.
//
// Shield encrypts contract state using ML-KEM-1024 for key encapsulation
// and ChaCha20-Poly1305 for symmetric encryption. State is also committed
// using Ajtai commitments for ZK proofs.

package anubis

/*
#include "aegisvm.h"
*/
import "C"

import (
	"unsafe"
)

// Commitment represents a 64-byte Ajtai commitment
type Commitment [ShieldCommitmentSize]byte

// EncryptState encrypts contract state using ML-KEM.
// Returns the ciphertext and the KEM ciphertext needed for decryption.
func EncryptState(plaintext []byte, encapsKey [MLKEMPublicKeySize]byte) (ciphertext []byte, kemCiphertext [MLKEMCiphertextSize]byte, err error) {
	if len(plaintext) == 0 || len(plaintext) > ShieldMaxStateSize {
		return nil, kemCiphertext, ErrInvalidInput
	}

	// Allocate output buffer (plaintext + overhead)
	ciphertext = make([]byte, len(plaintext)+ShieldEncryptedStateOverhead)
	var ciphertextLen C.size_t

	result := C.aegis_shield_encrypt_state(
		(*C.uint8_t)(unsafe.Pointer(&ciphertext[0])),
		&ciphertextLen,
		(*C.uint8_t)(unsafe.Pointer(&plaintext[0])),
		C.size_t(len(plaintext)),
		(*C.uint8_t)(unsafe.Pointer(&encapsKey[0])),
		(*C.uint8_t)(unsafe.Pointer(&kemCiphertext[0])),
	)

	if err := checkResult(result, ErrEncryptionFailed); err != nil {
		return nil, kemCiphertext, err
	}

	return ciphertext[:ciphertextLen], kemCiphertext, nil
}

// DecryptState decrypts contract state using ML-KEM secret key.
func DecryptState(ciphertext []byte, kemCiphertext [MLKEMCiphertextSize]byte, decapsKey [MLKEMSecretKeySize]byte) ([]byte, error) {
	if len(ciphertext) < ShieldEncryptedStateOverhead {
		return nil, ErrInvalidInput
	}

	// Allocate output buffer
	plaintext := make([]byte, len(ciphertext))
	var plaintextLen C.size_t

	result := C.aegis_shield_decrypt_state(
		(*C.uint8_t)(unsafe.Pointer(&plaintext[0])),
		&plaintextLen,
		(*C.uint8_t)(unsafe.Pointer(&ciphertext[0])),
		C.size_t(len(ciphertext)),
		(*C.uint8_t)(unsafe.Pointer(&kemCiphertext[0])),
		(*C.uint8_t)(unsafe.Pointer(&decapsKey[0])),
	)

	if err := checkResult(result, ErrDecryptionFailed); err != nil {
		return nil, err
	}

	return plaintext[:plaintextLen], nil
}

// CreateCommitment creates an Ajtai commitment to data.
// The randomness provides hiding; the commitment is binding.
func CreateCommitment(data []byte, randomness [32]byte) (Commitment, error) {
	var commitment Commitment

	if len(data) == 0 {
		return commitment, ErrInvalidInput
	}

	result := C.aegis_shield_create_commitment(
		(*C.uint8_t)(unsafe.Pointer(&commitment[0])),
		(*C.uint8_t)(unsafe.Pointer(&data[0])),
		C.size_t(len(data)),
		(*C.uint8_t)(unsafe.Pointer(&randomness[0])),
	)

	if err := checkResult(result, ErrCommitmentFailed); err != nil {
		return commitment, err
	}

	return commitment, nil
}

// EncryptedState represents encrypted contract state
type EncryptedState struct {
	Ciphertext    []byte
	KEMCiphertext [MLKEMCiphertextSize]byte
	Commitment    Commitment
}

// EncryptStateWithCommitment encrypts state and creates a commitment.
// This is the typical flow for storing private contract state.
func EncryptStateWithCommitment(plaintext []byte, encapsKey [MLKEMPublicKeySize]byte, randomness [32]byte) (*EncryptedState, error) {
	// Create commitment first
	commitment, err := CreateCommitment(plaintext, randomness)
	if err != nil {
		return nil, err
	}

	// Encrypt state
	ciphertext, kemCiphertext, err := EncryptState(plaintext, encapsKey)
	if err != nil {
		return nil, err
	}

	return &EncryptedState{
		Ciphertext:    ciphertext,
		KEMCiphertext: kemCiphertext,
		Commitment:    commitment,
	}, nil
}
