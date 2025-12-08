// Privacy Bridge - Interface for ANUBIS privacy features
//
// Provides Go wrappers for Shield, Whisper, Eye, Gate, and Veil FFI calls.

package vmbridge

/*
#cgo CFLAGS: -I${SRCDIR}/../../../core/include
#cgo LDFLAGS: -L${SRCDIR}/../../../core/lib -laegisvm -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib -lgnat -lgnarl -lm

#include "aegisvm.h"
#include <stdlib.h>
#include <string.h>
*/
import "C"

import (
	"errors"
	"unsafe"
)

// Privacy error definitions
var (
	ErrShieldEncrypt        = errors.New("privacy: state encryption failed")
	ErrShieldDecrypt        = errors.New("privacy: state decryption failed")
	ErrWhisperNote          = errors.New("privacy: note creation failed")
	ErrWhisperNullifier     = errors.New("privacy: nullifier computation failed")
	ErrWhisperRangeProof    = errors.New("privacy: range proof failed")
	ErrWhisperBalanceProof  = errors.New("privacy: balance proof failed")
	ErrEyeViewingKey        = errors.New("privacy: viewing key derivation failed")
	ErrEyeDisclosure        = errors.New("privacy: disclosure proof failed")
	ErrGateSession          = errors.New("privacy: session initialization failed")
	ErrGateEncrypt          = errors.New("privacy: input encryption failed")
	ErrGateDecrypt          = errors.New("privacy: output decryption failed")
	ErrVeilSTARK            = errors.New("privacy: STARK verification failed")
	ErrVeilLattice          = errors.New("privacy: lattice proof verification failed")
)

// =============================================================================
// Shield (Encrypted State)
// =============================================================================

// ShieldEncryptState encrypts contract state using ML-KEM
func ShieldEncryptState(plaintext []byte, encapsKey [1568]byte) (ciphertext []byte, kemCT [1568]byte, err error) {
	if len(plaintext) == 0 {
		return nil, kemCT, errors.New("privacy: empty plaintext")
	}

	// Allocate output buffers
	maxCTLen := len(plaintext) + 48 // overhead for nonce + tag + header
	ctBuf := make([]byte, maxCTLen)
	var ctLen C.size_t

	result := C.aegis_shield_encrypt_state(
		(*C.uint8_t)(unsafe.Pointer(&ctBuf[0])),
		&ctLen,
		(*C.uint8_t)(unsafe.Pointer(&plaintext[0])),
		C.size_t(len(plaintext)),
		(*C.uint8_t)(unsafe.Pointer(&encapsKey[0])),
		(*C.uint8_t)(unsafe.Pointer(&kemCT[0])),
	)

	if result != C.AEGIS_OK {
		return nil, kemCT, ErrShieldEncrypt
	}

	ciphertext = make([]byte, ctLen)
	copy(ciphertext, ctBuf[:ctLen])
	return ciphertext, kemCT, nil
}

// ShieldDecryptState decrypts contract state using ML-KEM secret key
func ShieldDecryptState(ciphertext []byte, kemCT [1568]byte, decapsKey [3168]byte) (plaintext []byte, err error) {
	if len(ciphertext) == 0 {
		return nil, errors.New("privacy: empty ciphertext")
	}

	// Allocate output buffer
	ptBuf := make([]byte, len(ciphertext))
	var ptLen C.size_t

	result := C.aegis_shield_decrypt_state(
		(*C.uint8_t)(unsafe.Pointer(&ptBuf[0])),
		&ptLen,
		(*C.uint8_t)(unsafe.Pointer(&ciphertext[0])),
		C.size_t(len(ciphertext)),
		(*C.uint8_t)(unsafe.Pointer(&kemCT[0])),
		(*C.uint8_t)(unsafe.Pointer(&decapsKey[0])),
	)

	if result != C.AEGIS_OK {
		return nil, ErrShieldDecrypt
	}

	plaintext = make([]byte, ptLen)
	copy(plaintext, ptBuf[:ptLen])
	return plaintext, nil
}

// ShieldCreateCommitment creates an Ajtai commitment to data
func ShieldCreateCommitment(data []byte, randomness [32]byte) ([64]byte, error) {
	var commitment [64]byte

	if len(data) == 0 {
		return commitment, errors.New("privacy: empty data")
	}

	result := C.aegis_shield_create_commitment(
		(*C.uint8_t)(unsafe.Pointer(&commitment[0])),
		(*C.uint8_t)(unsafe.Pointer(&data[0])),
		C.size_t(len(data)),
		(*C.uint8_t)(unsafe.Pointer(&randomness[0])),
	)

	if result != C.AEGIS_OK {
		return commitment, errors.New("privacy: commitment creation failed")
	}

	return commitment, nil
}

// =============================================================================
// Whisper (Confidential Transactions)
// =============================================================================

// WhisperNote represents a confidential note
type WhisperNote struct {
	Commitment   [64]byte
	EncryptedData [64]byte
	OwnerPKHash  [32]byte
}

// WhisperCreateNote creates a confidential note
func WhisperCreateNote(value uint64, blinding [32]byte, ownerPK [1568]byte) (*WhisperNote, error) {
	var cNote C.aegis_whisper_note_t

	result := C.aegis_whisper_create_note(
		&cNote,
		C.uint64_t(value),
		(*C.uint8_t)(unsafe.Pointer(&blinding[0])),
		(*C.uint8_t)(unsafe.Pointer(&ownerPK[0])),
	)

	if result != C.AEGIS_OK {
		return nil, ErrWhisperNote
	}

	note := &WhisperNote{}
	copy(note.Commitment[:], (*[64]byte)(unsafe.Pointer(&cNote.commitment[0]))[:])
	copy(note.EncryptedData[:], (*[64]byte)(unsafe.Pointer(&cNote.encrypted_data[0]))[:])
	copy(note.OwnerPKHash[:], (*[32]byte)(unsafe.Pointer(&cNote.owner_pk_hash[0]))[:])

	return note, nil
}

// WhisperComputeNullifier computes the nullifier for a note
func WhisperComputeNullifier(note *WhisperNote, ownerSK [3168]byte) ([32]byte, error) {
	var nullifier [32]byte

	cNote := C.aegis_whisper_note_t{}
	copy((*[64]byte)(unsafe.Pointer(&cNote.commitment[0]))[:], note.Commitment[:])
	copy((*[64]byte)(unsafe.Pointer(&cNote.encrypted_data[0]))[:], note.EncryptedData[:])
	copy((*[32]byte)(unsafe.Pointer(&cNote.owner_pk_hash[0]))[:], note.OwnerPKHash[:])

	result := C.aegis_whisper_compute_nullifier(
		(*C.uint8_t)(unsafe.Pointer(&nullifier[0])),
		&cNote,
		(*C.uint8_t)(unsafe.Pointer(&ownerSK[0])),
	)

	if result != C.AEGIS_OK {
		return nullifier, ErrWhisperNullifier
	}

	return nullifier, nil
}

// WhisperCreateRangeProof creates a range proof for a value
func WhisperCreateRangeProof(commitment [64]byte, value uint64, blinding [32]byte) ([]byte, error) {
	proofBuf := make([]byte, 1024)
	var proofLen C.size_t

	result := C.aegis_whisper_create_range_proof(
		(*C.uint8_t)(unsafe.Pointer(&proofBuf[0])),
		&proofLen,
		(*C.uint8_t)(unsafe.Pointer(&commitment[0])),
		C.uint64_t(value),
		(*C.uint8_t)(unsafe.Pointer(&blinding[0])),
	)

	if result != C.AEGIS_OK {
		return nil, ErrWhisperRangeProof
	}

	proof := make([]byte, proofLen)
	copy(proof, proofBuf[:proofLen])
	return proof, nil
}

// WhisperVerifyRangeProof verifies a range proof
func WhisperVerifyRangeProof(commitment [64]byte, proof []byte) (bool, error) {
	var valid C.bool

	result := C.aegis_whisper_verify_range_proof(
		(*C.uint8_t)(unsafe.Pointer(&commitment[0])),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if result != C.AEGIS_OK {
		return false, ErrWhisperRangeProof
	}

	return bool(valid), nil
}

// WhisperVerifyBalanceProof verifies that inputs equal outputs + fee
func WhisperVerifyBalanceProof(
	inputCommitments [][64]byte,
	outputCommitments [][64]byte,
	fee uint64,
	proof []byte,
) (bool, error) {
	if len(inputCommitments) == 0 || len(outputCommitments) == 0 {
		return false, errors.New("privacy: empty commitments")
	}

	var valid C.bool

	// Flatten commitment arrays
	inCommits := make([]byte, len(inputCommitments)*64)
	for i, c := range inputCommitments {
		copy(inCommits[i*64:], c[:])
	}

	outCommits := make([]byte, len(outputCommitments)*64)
	for i, c := range outputCommitments {
		copy(outCommits[i*64:], c[:])
	}

	result := C.aegis_whisper_verify_balance_proof(
		(*[64]C.uint8_t)(unsafe.Pointer(&inCommits[0])),
		C.size_t(len(inputCommitments)),
		(*[64]C.uint8_t)(unsafe.Pointer(&outCommits[0])),
		C.size_t(len(outputCommitments)),
		C.uint64_t(fee),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if result != C.AEGIS_OK {
		return false, ErrWhisperBalanceProof
	}

	return bool(valid), nil
}

// =============================================================================
// Eye (Selective Disclosure)
// =============================================================================

// ViewingType represents the type of viewing key
type ViewingType uint8

const (
	ViewFull      ViewingType = 0
	ViewBalance   ViewingType = 1
	ViewExistence ViewingType = 2
	ViewAudit     ViewingType = 3
	ViewCustom    ViewingType = 4
)

// EyeDeriveViewingKey derives a viewing key from master secret key
func EyeDeriveViewingKey(masterSK [3168]byte, viewType ViewingType, validUntil uint64) ([64]byte, error) {
	var viewingKey [64]byte

	result := C.aegis_eye_derive_viewing_key(
		(*C.uint8_t)(unsafe.Pointer(&viewingKey[0])),
		(*C.uint8_t)(unsafe.Pointer(&masterSK[0])),
		C.aegis_viewing_type_t(viewType),
		C.uint64_t(validUntil),
	)

	if result != C.AEGIS_OK {
		return viewingKey, ErrEyeViewingKey
	}

	return viewingKey, nil
}

// EyeCreateDisclosureProof creates a selective disclosure proof
func EyeCreateDisclosureProof(viewingKey [64]byte, attributeMask uint64, attributes []uint64) ([]byte, error) {
	proofBuf := make([]byte, 512)
	var proofLen C.size_t

	var attrsPtr *C.uint64_t
	if len(attributes) > 0 {
		attrsPtr = (*C.uint64_t)(unsafe.Pointer(&attributes[0]))
	}

	result := C.aegis_eye_create_disclosure_proof(
		(*C.uint8_t)(unsafe.Pointer(&proofBuf[0])),
		&proofLen,
		(*C.uint8_t)(unsafe.Pointer(&viewingKey[0])),
		C.uint64_t(attributeMask),
		attrsPtr,
		C.size_t(len(attributes)),
	)

	if result != C.AEGIS_OK {
		return nil, ErrEyeDisclosure
	}

	proof := make([]byte, proofLen)
	copy(proof, proofBuf[:proofLen])
	return proof, nil
}

// EyeVerifyDisclosure verifies a selective disclosure
func EyeVerifyDisclosure(ownerPK [2592]byte, attributeMask uint64, attributes []uint64, proof []byte) (bool, error) {
	var valid C.bool

	var attrsPtr *C.uint64_t
	if len(attributes) > 0 {
		attrsPtr = (*C.uint64_t)(unsafe.Pointer(&attributes[0]))
	}

	result := C.aegis_eye_verify_disclosure(
		(*C.uint8_t)(unsafe.Pointer(&ownerPK[0])),
		C.uint64_t(attributeMask),
		attrsPtr,
		C.size_t(len(attributes)),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if result != C.AEGIS_OK {
		return false, ErrEyeDisclosure
	}

	return bool(valid), nil
}

// =============================================================================
// Gate (Private Execution)
// =============================================================================

// ExecutionMode represents private execution modes
type ExecutionMode uint8

const (
	ExecFullPrivate   ExecutionMode = 0
	ExecPublicResult  ExecutionMode = 1
	ExecPublicFunc    ExecutionMode = 2
	ExecAuditable     ExecutionMode = 3
)

// GateSession represents a private execution session
type GateSession struct {
	SessionID    [32]byte
	ContractAddr [32]byte
	InitiatorPK  [1568]byte
	SharedSecret [32]byte
	ExpiresAt    uint64
	CallCount    uint64
	MaxCalls     uint64
	Mode         ExecutionMode
}

// GateInitSession initializes a private execution session
func GateInitSession(
	contractAddr [32]byte,
	initiatorPK [1568]byte,
	contractPK [1568]byte,
	mode ExecutionMode,
	maxCalls uint64,
	ttlBlocks uint64,
) (*GateSession, error) {
	var cSession C.aegis_gate_session_t

	result := C.aegis_gate_init_session(
		&cSession,
		(*C.uint8_t)(unsafe.Pointer(&contractAddr[0])),
		(*C.uint8_t)(unsafe.Pointer(&initiatorPK[0])),
		(*C.uint8_t)(unsafe.Pointer(&contractPK[0])),
		C.aegis_exec_mode_t(mode),
		C.uint64_t(maxCalls),
		C.uint64_t(ttlBlocks),
	)

	if result != C.AEGIS_OK {
		return nil, ErrGateSession
	}

	session := &GateSession{
		ExpiresAt: uint64(cSession.expires_at),
		CallCount: uint64(cSession.call_count),
		MaxCalls:  uint64(cSession.max_calls),
		Mode:      ExecutionMode(cSession.mode),
	}
	copy(session.SessionID[:], (*[32]byte)(unsafe.Pointer(&cSession.session_id[0]))[:])
	copy(session.ContractAddr[:], (*[32]byte)(unsafe.Pointer(&cSession.contract_addr[0]))[:])
	copy(session.InitiatorPK[:], (*[1568]byte)(unsafe.Pointer(&cSession.initiator_pk[0]))[:])
	copy(session.SharedSecret[:], (*[32]byte)(unsafe.Pointer(&cSession.shared_secret[0]))[:])

	return session, nil
}

// GateEncryptInputs encrypts function call inputs for private execution
func GateEncryptInputs(session *GateSession, function [4]byte, args []byte) ([]byte, error) {
	encBuf := make([]byte, 4096)
	var encLen C.size_t

	cSession := C.aegis_gate_session_t{
		expires_at: C.uint64_t(session.ExpiresAt),
		call_count: C.uint64_t(session.CallCount),
		max_calls:  C.uint64_t(session.MaxCalls),
		mode:       C.aegis_exec_mode_t(session.Mode),
	}
	copy((*[32]byte)(unsafe.Pointer(&cSession.session_id[0]))[:], session.SessionID[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.contract_addr[0]))[:], session.ContractAddr[:])
	copy((*[1568]byte)(unsafe.Pointer(&cSession.initiator_pk[0]))[:], session.InitiatorPK[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.shared_secret[0]))[:], session.SharedSecret[:])

	var argsPtr *C.uint8_t
	argsLen := C.size_t(0)
	if len(args) > 0 {
		argsPtr = (*C.uint8_t)(unsafe.Pointer(&args[0]))
		argsLen = C.size_t(len(args))
	}

	result := C.aegis_gate_encrypt_inputs(
		(*C.uint8_t)(unsafe.Pointer(&encBuf[0])),
		&encLen,
		&cSession,
		(*C.uint8_t)(unsafe.Pointer(&function[0])),
		argsPtr,
		argsLen,
	)

	if result != C.AEGIS_OK {
		return nil, ErrGateEncrypt
	}

	encrypted := make([]byte, encLen)
	copy(encrypted, encBuf[:encLen])
	return encrypted, nil
}

// GateDecryptOutputs decrypts execution results
func GateDecryptOutputs(session *GateSession, encrypted []byte) ([]byte, error) {
	ptBuf := make([]byte, len(encrypted))
	var ptLen C.size_t

	cSession := C.aegis_gate_session_t{
		expires_at: C.uint64_t(session.ExpiresAt),
		call_count: C.uint64_t(session.CallCount),
		max_calls:  C.uint64_t(session.MaxCalls),
		mode:       C.aegis_exec_mode_t(session.Mode),
	}
	copy((*[32]byte)(unsafe.Pointer(&cSession.session_id[0]))[:], session.SessionID[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.contract_addr[0]))[:], session.ContractAddr[:])
	copy((*[1568]byte)(unsafe.Pointer(&cSession.initiator_pk[0]))[:], session.InitiatorPK[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.shared_secret[0]))[:], session.SharedSecret[:])

	result := C.aegis_gate_decrypt_outputs(
		(*C.uint8_t)(unsafe.Pointer(&ptBuf[0])),
		&ptLen,
		&cSession,
		(*C.uint8_t)(unsafe.Pointer(&encrypted[0])),
		C.size_t(len(encrypted)),
	)

	if result != C.AEGIS_OK {
		return nil, ErrGateDecrypt
	}

	plaintext := make([]byte, ptLen)
	copy(plaintext, ptBuf[:ptLen])
	return plaintext, nil
}

// =============================================================================
// Veil (ZK Proofs)
// =============================================================================

// VeilVerifySTARKProof verifies a STARK execution proof
func VeilVerifySTARKProof(
	codeHash [32]byte,
	oldStateRoot [32]byte,
	newStateRoot [32]byte,
	publicInputs []byte,
	publicOutputs []byte,
	proof []byte,
) (bool, error) {
	var valid C.bool

	var inputsPtr, outputsPtr *C.uint8_t
	if len(publicInputs) > 0 {
		inputsPtr = (*C.uint8_t)(unsafe.Pointer(&publicInputs[0]))
	}
	if len(publicOutputs) > 0 {
		outputsPtr = (*C.uint8_t)(unsafe.Pointer(&publicOutputs[0]))
	}

	result := C.aegis_veil_verify_stark_proof(
		(*C.uint8_t)(unsafe.Pointer(&codeHash[0])),
		(*C.uint8_t)(unsafe.Pointer(&oldStateRoot[0])),
		(*C.uint8_t)(unsafe.Pointer(&newStateRoot[0])),
		inputsPtr,
		C.size_t(len(publicInputs)),
		outputsPtr,
		C.size_t(len(publicOutputs)),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if result != C.AEGIS_OK {
		return false, ErrVeilSTARK
	}

	return bool(valid), nil
}

// VeilVerifyLatticeProof verifies a lattice-based ZK proof
func VeilVerifyLatticeProof(statement []byte, proof []byte) (bool, error) {
	var valid C.bool

	result := C.aegis_veil_verify_lattice_proof(
		(*C.uint8_t)(unsafe.Pointer(&statement[0])),
		C.size_t(len(statement)),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if result != C.AEGIS_OK {
		return false, ErrVeilLattice
	}

	return bool(valid), nil
}
