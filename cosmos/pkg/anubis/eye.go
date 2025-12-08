// Eye provides selective disclosure operations for ANUBIS.
//
// Eye allows users to prove properties about their accounts without
// revealing full details. Viewing keys can be derived with different
// permission levels and time limits.

package anubis

/*
#include "aegisvm.h"
*/
import "C"

import (
	"unsafe"
)

// ViewingKey represents a derived viewing key for selective disclosure
type ViewingKey [EyeViewingKeySize]byte

// DeriveViewingKey derives a viewing key from the master secret key.
// The viewing key can be shared with auditors, exchanges, etc.
func DeriveViewingKey(masterSK [MLKEMSecretKeySize]byte, viewType ViewingType, validUntil uint64) (ViewingKey, error) {
	var viewingKey ViewingKey

	result := C.aegis_eye_derive_viewing_key(
		(*C.uint8_t)(unsafe.Pointer(&viewingKey[0])),
		(*C.uint8_t)(unsafe.Pointer(&masterSK[0])),
		C.aegis_viewing_type_t(viewType),
		C.uint64_t(validUntil),
	)

	if err := checkResult(result, ErrViewingKeyDerive); err != nil {
		return viewingKey, err
	}

	return viewingKey, nil
}

// CreateDisclosureProof creates a ZK proof of disclosed attributes.
// The attribute mask specifies which attributes are being disclosed.
func CreateDisclosureProof(viewingKey ViewingKey, attributeMask uint64, attributes []uint64) ([]byte, error) {
	if len(attributes) == 0 {
		return nil, ErrInvalidInput
	}

	proof := make([]byte, EyeDisclosureProofSize)
	var proofLen C.size_t

	result := C.aegis_eye_create_disclosure_proof(
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		&proofLen,
		(*C.uint8_t)(unsafe.Pointer(&viewingKey[0])),
		C.uint64_t(attributeMask),
		(*C.uint64_t)(unsafe.Pointer(&attributes[0])),
		C.size_t(len(attributes)),
	)

	if err := checkResult(result, ErrProofCreation); err != nil {
		return nil, err
	}

	return proof[:proofLen], nil
}

// VerifyDisclosure verifies a selective disclosure proof.
// Returns true if the claimed attributes are valid for the owner.
func VerifyDisclosure(ownerPK [MLDSAPublicKeySize]byte, attributeMask uint64, attributes []uint64, proof []byte) (bool, error) {
	if len(attributes) == 0 || len(proof) == 0 {
		return false, ErrInvalidInput
	}

	var valid C.bool

	result := C.aegis_eye_verify_disclosure(
		(*C.uint8_t)(unsafe.Pointer(&ownerPK[0])),
		C.uint64_t(attributeMask),
		(*C.uint64_t)(unsafe.Pointer(&attributes[0])),
		C.size_t(len(attributes)),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if err := checkResult(result, ErrProofVerification); err != nil {
		return false, err
	}

	return bool(valid), nil
}

// Attribute masks for common disclosure scenarios
const (
	AttrMaskBalance    uint64 = 1 << 0  // Prove balance >= X
	AttrMaskAge        uint64 = 1 << 1  // Prove account age
	AttrMaskTxCount    uint64 = 1 << 2  // Prove transaction count
	AttrMaskVerified   uint64 = 1 << 3  // Prove KYC status
	AttrMaskJurisdiction uint64 = 1 << 4  // Prove jurisdiction
	AttrMaskInstitution uint64 = 1 << 5  // Prove institutional status
)

// Disclosure represents a selective disclosure request
type Disclosure struct {
	OwnerPK       [MLDSAPublicKeySize]byte
	AttributeMask uint64
	Attributes    []uint64
	Proof         []byte
	ValidUntil    uint64
}

// DisclosureBuilder helps construct disclosure proofs
type DisclosureBuilder struct {
	viewingKey    ViewingKey
	attributeMask uint64
	attributes    []uint64
}

// NewDisclosureBuilder creates a new disclosure builder
func NewDisclosureBuilder(viewingKey ViewingKey) *DisclosureBuilder {
	return &DisclosureBuilder{
		viewingKey:    viewingKey,
		attributeMask: 0,
		attributes:    make([]uint64, 0),
	}
}

// ProveBalanceGT proves balance is greater than threshold
func (b *DisclosureBuilder) ProveBalanceGT(threshold uint64) *DisclosureBuilder {
	b.attributeMask |= AttrMaskBalance
	b.attributes = append(b.attributes, threshold)
	return b
}

// ProveAccountAge proves account was created before block height
func (b *DisclosureBuilder) ProveAccountAge(minAge uint64) *DisclosureBuilder {
	b.attributeMask |= AttrMaskAge
	b.attributes = append(b.attributes, minAge)
	return b
}

// ProveVerified proves KYC verification status
func (b *DisclosureBuilder) ProveVerified(level uint64) *DisclosureBuilder {
	b.attributeMask |= AttrMaskVerified
	b.attributes = append(b.attributes, level)
	return b
}

// Build creates the disclosure proof
func (b *DisclosureBuilder) Build() ([]byte, error) {
	return CreateDisclosureProof(b.viewingKey, b.attributeMask, b.attributes)
}
