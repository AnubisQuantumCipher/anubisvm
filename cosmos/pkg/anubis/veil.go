// Veil provides ZK proof verification for ANUBIS.
//
// Veil supports two proof systems:
// - STARK: Transparent, quantum-resistant validity proofs using Goldilocks field
// - Lattice ZK: Post-quantum ZK proofs based on SIS/LWE hardness

package anubis

/*
#include "aegisvm.h"
*/
import "C"

import (
	"unsafe"
)

// VerifySTARKProof verifies a STARK execution proof.
// This proves correct execution of a smart contract.
func VerifySTARKProof(
	codeHash [32]byte,
	oldStateRoot [32]byte,
	newStateRoot [32]byte,
	publicInputs []byte,
	publicOutputs []byte,
	proof []byte,
) (bool, error) {
	if len(proof) == 0 || len(proof) > VeilSTARKProofMaxSize {
		return false, ErrInvalidInput
	}

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

	if err := checkResult(result, ErrProofVerification); err != nil {
		return false, err
	}

	return bool(valid), nil
}

// VerifyLatticeProof verifies a lattice-based ZK proof.
// Used for post-quantum secure proofs of knowledge.
func VerifyLatticeProof(statement []byte, proof []byte) (bool, error) {
	if len(statement) == 0 || len(proof) == 0 {
		return false, ErrInvalidInput
	}

	var valid C.bool

	result := C.aegis_veil_verify_lattice_proof(
		(*C.uint8_t)(unsafe.Pointer(&statement[0])),
		C.size_t(len(statement)),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if err := checkResult(result, ErrProofVerification); err != nil {
		return false, err
	}

	return bool(valid), nil
}

// STARKProof represents a STARK execution proof
type STARKProof struct {
	CodeHash      [32]byte
	OldStateRoot  [32]byte
	NewStateRoot  [32]byte
	PublicInputs  []byte
	PublicOutputs []byte
	Proof         []byte
}

// Verify verifies this STARK proof
func (p *STARKProof) Verify() (bool, error) {
	return VerifySTARKProof(
		p.CodeHash,
		p.OldStateRoot,
		p.NewStateRoot,
		p.PublicInputs,
		p.PublicOutputs,
		p.Proof,
	)
}

// ExecutionProof represents a proof of private execution
type ExecutionProof struct {
	RequestID       [32]byte
	ProverAddress   [32]byte
	STARKProof      *STARKProof
	ProverSignature []byte // ML-DSA signature over proof
}

// LatticeProof represents a lattice-based ZK proof
type LatticeProof struct {
	Statement []byte
	Proof     []byte
}

// Verify verifies this lattice proof
func (p *LatticeProof) Verify() (bool, error) {
	return VerifyLatticeProof(p.Statement, p.Proof)
}

// ProofSystem identifies the ZK proof system used
type ProofSystem string

const (
	ProofSystemSTARK   ProofSystem = "stark"
	ProofSystemLattice ProofSystem = "lattice"
)

// ZKProof is a generic ZK proof interface
type ZKProof interface {
	Verify() (bool, error)
	System() ProofSystem
}

// Implement ZKProof for STARKProof
func (p *STARKProof) System() ProofSystem {
	return ProofSystemSTARK
}

// Implement ZKProof for LatticeProof
func (p *LatticeProof) System() ProofSystem {
	return ProofSystemLattice
}

// ProofMetadata contains metadata about a proof
type ProofMetadata struct {
	System      ProofSystem
	ProofSize   int
	VerifyGas   uint64
	SecurityBits int
}

// GetSTARKMetadata returns metadata for STARK proofs
func GetSTARKMetadata() ProofMetadata {
	return ProofMetadata{
		System:       ProofSystemSTARK,
		ProofSize:    VeilSTARKProofMaxSize,
		VerifyGas:    500000, // Gas cost for STARK verification
		SecurityBits: 128,    // 128-bit security level
	}
}

// GetLatticeMetadata returns metadata for lattice proofs
func GetLatticeMetadata() ProofMetadata {
	return ProofMetadata{
		System:       ProofSystemLattice,
		ProofSize:    VeilLatticeProofSize,
		VerifyGas:    200000, // Gas cost for lattice verification
		SecurityBits: 256,    // NIST Level 5 (256-bit equivalent)
	}
}
