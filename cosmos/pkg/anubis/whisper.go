// Whisper provides confidential transaction operations for ANUBIS.
//
// Whisper uses Ajtai commitments to hide transaction values while proving
// balance conservation. Notes are encrypted to recipients using ML-KEM,
// and nullifiers prevent double-spending.

package anubis

/*
#include "aegisvm.h"
*/
import "C"

import (
	"unsafe"
)

// Note represents a confidential value (similar to UTXO)
type Note struct {
	Commitment    [64]byte                  // Ajtai commitment to value
	EncryptedData [64]byte                  // ML-KEM encrypted value+blinding
	OwnerPKHash   [32]byte                  // SHA3(owner's ML-KEM public key)
}

// Nullifier represents a spent note identifier
type Nullifier [WhisperNullifierSize]byte

// CreateNote creates a new confidential note.
// The value is hidden in the commitment, encrypted to the owner.
func CreateNote(value uint64, blinding [32]byte, ownerPK [MLKEMPublicKeySize]byte) (*Note, error) {
	var cNote C.aegis_whisper_note_t

	result := C.aegis_whisper_create_note(
		&cNote,
		C.uint64_t(value),
		(*C.uint8_t)(unsafe.Pointer(&blinding[0])),
		(*C.uint8_t)(unsafe.Pointer(&ownerPK[0])),
	)

	if err := checkResult(result, ErrCommitmentFailed); err != nil {
		return nil, err
	}

	// Convert C struct to Go struct
	note := &Note{}
	copy(note.Commitment[:], C.GoBytes(unsafe.Pointer(&cNote.commitment[0]), 64))
	copy(note.EncryptedData[:], C.GoBytes(unsafe.Pointer(&cNote.encrypted_data[0]), 64))
	copy(note.OwnerPKHash[:], C.GoBytes(unsafe.Pointer(&cNote.owner_pk_hash[0]), 32))

	return note, nil
}

// ComputeNullifier computes a nullifier for spending a note.
// The nullifier is deterministic: same note + key = same nullifier.
// This prevents double-spending without revealing which note was spent.
func ComputeNullifier(note *Note, ownerSK [MLKEMSecretKeySize]byte) (Nullifier, error) {
	var nullifier Nullifier

	// Convert Go Note to C struct
	var cNote C.aegis_whisper_note_t
	copy((*[64]byte)(unsafe.Pointer(&cNote.commitment[0]))[:], note.Commitment[:])
	copy((*[64]byte)(unsafe.Pointer(&cNote.encrypted_data[0]))[:], note.EncryptedData[:])
	copy((*[32]byte)(unsafe.Pointer(&cNote.owner_pk_hash[0]))[:], note.OwnerPKHash[:])

	result := C.aegis_whisper_compute_nullifier(
		(*C.uint8_t)(unsafe.Pointer(&nullifier[0])),
		&cNote,
		(*C.uint8_t)(unsafe.Pointer(&ownerSK[0])),
	)

	if err := checkResult(result, ErrNullifierCompute); err != nil {
		return nullifier, err
	}

	return nullifier, nil
}

// CreateRangeProof creates a ZK proof that a committed value is in [0, 2^64).
// This prevents negative values or overflow attacks.
func CreateRangeProof(commitment [64]byte, value uint64, blinding [32]byte) ([]byte, error) {
	proof := make([]byte, WhisperRangeProofSize)
	var proofLen C.size_t

	result := C.aegis_whisper_create_range_proof(
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		&proofLen,
		(*C.uint8_t)(unsafe.Pointer(&commitment[0])),
		C.uint64_t(value),
		(*C.uint8_t)(unsafe.Pointer(&blinding[0])),
	)

	if err := checkResult(result, ErrProofCreation); err != nil {
		return nil, err
	}

	return proof[:proofLen], nil
}

// VerifyRangeProof verifies a range proof for a commitment.
func VerifyRangeProof(commitment [64]byte, proof []byte) (bool, error) {
	var valid C.bool

	result := C.aegis_whisper_verify_range_proof(
		(*C.uint8_t)(unsafe.Pointer(&commitment[0])),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if err := checkResult(result, ErrProofVerification); err != nil {
		return false, err
	}

	return bool(valid), nil
}

// BalanceWitness contains the private data for creating a balance proof
type BalanceWitness struct {
	InputValues     []uint64
	InputBlindings  [][32]byte
	OutputValues    []uint64
	OutputBlindings [][32]byte
}

// CreateBalanceProof creates a ZK proof that inputs = outputs + fee.
// This proves value conservation without revealing amounts.
func CreateBalanceProof(
	inputCommitments [][64]byte,
	outputCommitments [][64]byte,
	fee uint64,
	witness *BalanceWitness,
) ([]byte, error) {
	if len(inputCommitments) != len(witness.InputValues) ||
		len(inputCommitments) != len(witness.InputBlindings) ||
		len(outputCommitments) != len(witness.OutputValues) ||
		len(outputCommitments) != len(witness.OutputBlindings) {
		return nil, ErrInvalidInput
	}

	proof := make([]byte, WhisperBalanceProofSize)
	var proofLen C.size_t

	// Convert slices to C arrays
	var inputCommsPtr, outputCommsPtr *[64]C.uint8_t
	var inputBlindingsPtr, outputBlindingsPtr *[32]C.uint8_t
	var inputValsPtr, outputValsPtr *C.uint64_t

	if len(inputCommitments) > 0 {
		inputCommsPtr = (*[64]C.uint8_t)(unsafe.Pointer(&inputCommitments[0]))
		inputBlindingsPtr = (*[32]C.uint8_t)(unsafe.Pointer(&witness.InputBlindings[0]))
		inputValsPtr = (*C.uint64_t)(unsafe.Pointer(&witness.InputValues[0]))
	}
	if len(outputCommitments) > 0 {
		outputCommsPtr = (*[64]C.uint8_t)(unsafe.Pointer(&outputCommitments[0]))
		outputBlindingsPtr = (*[32]C.uint8_t)(unsafe.Pointer(&witness.OutputBlindings[0]))
		outputValsPtr = (*C.uint64_t)(unsafe.Pointer(&witness.OutputValues[0]))
	}

	result := C.aegis_whisper_create_balance_proof(
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		&proofLen,
		inputCommsPtr,
		C.size_t(len(inputCommitments)),
		outputCommsPtr,
		C.size_t(len(outputCommitments)),
		C.uint64_t(fee),
		inputValsPtr,
		inputBlindingsPtr,
		outputValsPtr,
		outputBlindingsPtr,
	)

	if err := checkResult(result, ErrProofCreation); err != nil {
		return nil, err
	}

	return proof[:proofLen], nil
}

// VerifyBalanceProof verifies a balance conservation proof.
func VerifyBalanceProof(
	inputCommitments [][64]byte,
	outputCommitments [][64]byte,
	fee uint64,
	proof []byte,
) (bool, error) {
	var valid C.bool

	var inputCommsPtr, outputCommsPtr *[64]C.uint8_t
	if len(inputCommitments) > 0 {
		inputCommsPtr = (*[64]C.uint8_t)(unsafe.Pointer(&inputCommitments[0]))
	}
	if len(outputCommitments) > 0 {
		outputCommsPtr = (*[64]C.uint8_t)(unsafe.Pointer(&outputCommitments[0]))
	}

	result := C.aegis_whisper_verify_balance_proof(
		inputCommsPtr,
		C.size_t(len(inputCommitments)),
		outputCommsPtr,
		C.size_t(len(outputCommitments)),
		C.uint64_t(fee),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if err := checkResult(result, ErrProofVerification); err != nil {
		return false, err
	}

	return bool(valid), nil
}

// ConfidentialTransfer represents a complete confidential transfer
type ConfidentialTransfer struct {
	InputNullifiers   []Nullifier
	OutputNotes       []*Note
	InputProofs       [][]byte // Ownership proofs
	OutputRangeProofs [][]byte
	BalanceProof      []byte
	Fee               uint64
}
