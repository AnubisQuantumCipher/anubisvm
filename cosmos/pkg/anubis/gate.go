// Gate provides private smart contract execution for ANUBIS.
//
// Gate enables encrypted inputs/outputs for contract calls. Sessions
// provide efficient multi-call interactions using a shared secret
// derived from ML-KEM key exchange.

package anubis

/*
#include "aegisvm.h"
*/
import "C"

import (
	"unsafe"
)

// Session represents a private execution session
type Session struct {
	SessionID    [GateSessionIDSize]byte
	ContractAddr [32]byte
	InitiatorPK  [MLKEMPublicKeySize]byte
	SharedSecret [32]byte
	ExpiresAt    uint64
	CallCount    uint64
	MaxCalls     uint64
	Mode         ExecMode
}

// InitSession initializes a new private execution session.
// The session allows multiple encrypted calls to the same contract.
func InitSession(
	contractAddr [32]byte,
	initiatorPK [MLKEMPublicKeySize]byte,
	contractPK [MLKEMPublicKeySize]byte,
	mode ExecMode,
	maxCalls uint64,
	ttlBlocks uint64,
) (*Session, error) {
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

	if err := checkResult(result, ErrSessionInit); err != nil {
		return nil, err
	}

	// Convert C struct to Go struct
	session := &Session{
		ExpiresAt: uint64(cSession.expires_at),
		CallCount: uint64(cSession.call_count),
		MaxCalls:  uint64(cSession.max_calls),
		Mode:      ExecMode(cSession.mode),
	}
	copy(session.SessionID[:], C.GoBytes(unsafe.Pointer(&cSession.session_id[0]), 32))
	copy(session.ContractAddr[:], C.GoBytes(unsafe.Pointer(&cSession.contract_addr[0]), 32))
	copy(session.InitiatorPK[:], C.GoBytes(unsafe.Pointer(&cSession.initiator_pk[0]), MLKEMPublicKeySize))
	copy(session.SharedSecret[:], C.GoBytes(unsafe.Pointer(&cSession.shared_secret[0]), 32))

	return session, nil
}

// EncryptInputs encrypts function call inputs for private execution.
func (s *Session) EncryptInputs(function [4]byte, args []byte) ([]byte, error) {
	if len(args) > GateMaxEncryptedInput {
		return nil, ErrInvalidInput
	}

	// Convert session back to C struct
	var cSession C.aegis_gate_session_t
	copy((*[32]byte)(unsafe.Pointer(&cSession.session_id[0]))[:], s.SessionID[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.contract_addr[0]))[:], s.ContractAddr[:])
	copy((*[MLKEMPublicKeySize]byte)(unsafe.Pointer(&cSession.initiator_pk[0]))[:], s.InitiatorPK[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.shared_secret[0]))[:], s.SharedSecret[:])
	cSession.expires_at = C.uint64_t(s.ExpiresAt)
	cSession.call_count = C.uint64_t(s.CallCount)
	cSession.max_calls = C.uint64_t(s.MaxCalls)
	cSession.mode = C.aegis_exec_mode_t(s.Mode)

	encrypted := make([]byte, len(args)+64) // Add room for overhead
	var encryptedLen C.size_t

	var argsPtr *C.uint8_t
	if len(args) > 0 {
		argsPtr = (*C.uint8_t)(unsafe.Pointer(&args[0]))
	}

	result := C.aegis_gate_encrypt_inputs(
		(*C.uint8_t)(unsafe.Pointer(&encrypted[0])),
		&encryptedLen,
		&cSession,
		(*C.uint8_t)(unsafe.Pointer(&function[0])),
		argsPtr,
		C.size_t(len(args)),
	)

	if err := checkResult(result, ErrEncryptionFailed); err != nil {
		return nil, err
	}

	s.CallCount++
	return encrypted[:encryptedLen], nil
}

// DecryptOutputs decrypts the results from private execution.
func (s *Session) DecryptOutputs(encrypted []byte) ([]byte, error) {
	if len(encrypted) == 0 {
		return nil, ErrInvalidInput
	}

	// Convert session back to C struct
	var cSession C.aegis_gate_session_t
	copy((*[32]byte)(unsafe.Pointer(&cSession.session_id[0]))[:], s.SessionID[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.contract_addr[0]))[:], s.ContractAddr[:])
	copy((*[MLKEMPublicKeySize]byte)(unsafe.Pointer(&cSession.initiator_pk[0]))[:], s.InitiatorPK[:])
	copy((*[32]byte)(unsafe.Pointer(&cSession.shared_secret[0]))[:], s.SharedSecret[:])
	cSession.expires_at = C.uint64_t(s.ExpiresAt)
	cSession.call_count = C.uint64_t(s.CallCount)
	cSession.max_calls = C.uint64_t(s.MaxCalls)
	cSession.mode = C.aegis_exec_mode_t(s.Mode)

	plaintext := make([]byte, len(encrypted))
	var plaintextLen C.size_t

	result := C.aegis_gate_decrypt_outputs(
		(*C.uint8_t)(unsafe.Pointer(&plaintext[0])),
		&plaintextLen,
		&cSession,
		(*C.uint8_t)(unsafe.Pointer(&encrypted[0])),
		C.size_t(len(encrypted)),
	)

	if err := checkResult(result, ErrDecryptionFailed); err != nil {
		return nil, err
	}

	return plaintext[:plaintextLen], nil
}

// IsExpired checks if the session has expired
func (s *Session) IsExpired(currentBlock uint64) bool {
	return currentBlock >= s.ExpiresAt
}

// CanCall checks if another call can be made in this session
func (s *Session) CanCall() bool {
	return s.CallCount < s.MaxCalls
}

// PrivateCall represents an encrypted contract call
type PrivateCall struct {
	SessionID        [32]byte
	EncryptedInputs  []byte
	PublicInputs     []byte
	CallerPK         [MLKEMPublicKeySize]byte
	GasLimit         uint64
	Nonce            [32]byte
	Signature        []byte // ML-DSA signature
}

// PrivateResult represents the result of private execution
type PrivateResult struct {
	SessionID         [32]byte
	Success           bool
	EncryptedOutputs  []byte
	PublicOutputs     []byte
	NewStateRoot      [32]byte
	ExecutionProof    []byte // STARK proof
	GasUsed           uint64
}
