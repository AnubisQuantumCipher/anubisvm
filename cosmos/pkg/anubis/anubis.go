// Package anubis provides Go bindings for the ANUBIS privacy layer.
//
// ANUBIS provides five pillars of privacy for AegisVM:
// - Shield: Encrypted state storage using ML-KEM
// - Whisper: Confidential transactions with Ajtai commitments
// - Eye: Selective disclosure with viewing keys
// - Gate: Private smart contract execution
// - Veil: ZK proof verification (STARK + Lattice)
//
// All operations use post-quantum cryptography (NIST Level 5).
package anubis

/*
#cgo CFLAGS: -I${SRCDIR}/../../../core/include
#cgo LDFLAGS: -L${SRCDIR}/../../../core/lib -laegisvm

#include "aegisvm.h"
#include <stdlib.h>
*/
import "C"

import (
	"errors"
)

// Error codes
var (
	ErrInvalidInput       = errors.New("anubis: invalid input")
	ErrEncryptionFailed   = errors.New("anubis: encryption failed")
	ErrDecryptionFailed   = errors.New("anubis: decryption failed")
	ErrCommitmentFailed   = errors.New("anubis: commitment creation failed")
	ErrProofCreation      = errors.New("anubis: proof creation failed")
	ErrProofVerification  = errors.New("anubis: proof verification failed")
	ErrSessionInit        = errors.New("anubis: session initialization failed")
	ErrNullifierCompute   = errors.New("anubis: nullifier computation failed")
	ErrViewingKeyDerive   = errors.New("anubis: viewing key derivation failed")
)

// Constants from C header
const (
	// Shield constants
	ShieldCommitmentSize         = C.AEGIS_SHIELD_COMMITMENT_SIZE
	ShieldEncryptedStateOverhead = C.AEGIS_SHIELD_ENCRYPTED_STATE_OVERHEAD
	ShieldMaxStateSize           = C.AEGIS_SHIELD_MAX_STATE_SIZE

	// Whisper constants
	WhisperNoteSize         = C.AEGIS_WHISPER_NOTE_SIZE
	WhisperNullifierSize    = C.AEGIS_WHISPER_NULLIFIER_SIZE
	WhisperRangeProofSize   = C.AEGIS_WHISPER_RANGE_PROOF_SIZE
	WhisperBalanceProofSize = C.AEGIS_WHISPER_BALANCE_PROOF_SIZE

	// Eye constants
	EyeViewingKeySize       = C.AEGIS_EYE_VIEWING_KEY_SIZE
	EyeDisclosureProofSize  = C.AEGIS_EYE_DISCLOSURE_PROOF_SIZE

	// Gate constants
	GateSessionIDSize        = C.AEGIS_GATE_SESSION_ID_SIZE
	GateMaxEncryptedInput    = C.AEGIS_GATE_MAX_ENCRYPTED_INPUT
	GateExecutionProofSize   = C.AEGIS_GATE_EXECUTION_PROOF_SIZE

	// Veil constants
	VeilSTARKProofMaxSize   = C.AEGIS_VEIL_STARK_PROOF_MAX_SIZE
	VeilLatticeProofSize    = C.AEGIS_VEIL_LATTICE_PROOF_SIZE

	// Key sizes (from pqcrypto)
	MLKEMPublicKeySize  = 1568
	MLKEMSecretKeySize  = 3168
	MLKEMCiphertextSize = 1568
	MLDSAPublicKeySize  = 2592
)

// ViewingType represents the type of viewing key
type ViewingType int

const (
	ViewFull      ViewingType = C.AEGIS_VIEW_FULL
	ViewBalance   ViewingType = C.AEGIS_VIEW_BALANCE
	ViewExistence ViewingType = C.AEGIS_VIEW_EXISTENCE
	ViewAudit     ViewingType = C.AEGIS_VIEW_AUDIT
	ViewCustom    ViewingType = C.AEGIS_VIEW_CUSTOM
)

// ExecMode represents the private execution mode
type ExecMode int

const (
	ExecFullPrivate    ExecMode = C.AEGIS_EXEC_FULL_PRIVATE
	ExecPublicResult   ExecMode = C.AEGIS_EXEC_PUBLIC_RESULT
	ExecPublicFunction ExecMode = C.AEGIS_EXEC_PUBLIC_FUNCTION
	ExecAuditable      ExecMode = C.AEGIS_EXEC_AUDITABLE
)

// checkResult converts C result code to Go error
func checkResult(result C.aegis_result_t, defaultErr error) error {
	if result == C.AEGIS_OK {
		return nil
	}
	return defaultErr
}
