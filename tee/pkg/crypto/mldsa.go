// Package crypto provides post-quantum cryptographic operations for AegisVM TEE.
//
// This package provides ML-DSA-87 signature verification and ML-KEM-1024
// key encapsulation. It can operate in two modes:
//
// 1. Process-isolated mode: Calls out to a verified SPARK binary
// 2. Embedded mode: Uses CGO to call SPARK library (weaker guarantees)
//
// For production use with full verification guarantees, use process isolation.
package crypto

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
)

const (
	// ML-DSA-87 sizes (NIST FIPS 204)
	MLDSAPublicKeySize  = 2592
	MLDSASecretKeySize  = 4896
	MLDSASignatureSize  = 4627

	// ML-KEM-1024 sizes (NIST FIPS 203)
	MLKEMPublicKeySize  = 1568
	MLKEMSecretKeySize  = 3168
	MLKEMCiphertextSize = 1568
	MLKEMSharedKeySize  = 32
)

var (
	ErrInvalidPublicKey  = errors.New("invalid public key size")
	ErrInvalidSecretKey  = errors.New("invalid secret key size")
	ErrInvalidSignature  = errors.New("invalid signature size")
	ErrVerificationFailed = errors.New("signature verification failed")
)

// MLDSAPublicKey represents an ML-DSA-87 public key
type MLDSAPublicKey [MLDSAPublicKeySize]byte

// MLDSASecretKey represents an ML-DSA-87 secret key
type MLDSASecretKey [MLDSASecretKeySize]byte

// MLDSASignature represents an ML-DSA-87 signature
type MLDSASignature []byte

// Address computes the 20-byte address from a public key (CometBFT compatible)
func (pk *MLDSAPublicKey) Address() []byte {
	hash := sha256.Sum256(pk[:])
	return hash[:20]
}

// AddressHex returns the address as a hex string
func (pk *MLDSAPublicKey) AddressHex() string {
	return hex.EncodeToString(pk.Address())
}

// AccountID computes the full 32-byte AAS-001 account ID
func (pk *MLDSAPublicKey) AccountID() []byte {
	hash := sha256.Sum256(pk[:])
	return hash[:]
}

// Bytes returns the public key bytes
func (pk *MLDSAPublicKey) Bytes() []byte {
	return pk[:]
}

// MLDSAPublicKeyFromBytes creates a public key from bytes
func MLDSAPublicKeyFromBytes(data []byte) (*MLDSAPublicKey, error) {
	if len(data) != MLDSAPublicKeySize {
		return nil, fmt.Errorf("%w: got %d, expected %d", ErrInvalidPublicKey, len(data), MLDSAPublicKeySize)
	}
	var pk MLDSAPublicKey
	copy(pk[:], data)
	return &pk, nil
}

// MLDSASecretKeyFromBytes creates a secret key from bytes
func MLDSASecretKeyFromBytes(data []byte) (*MLDSASecretKey, error) {
	if len(data) != MLDSASecretKeySize {
		return nil, fmt.Errorf("%w: got %d, expected %d", ErrInvalidSecretKey, len(data), MLDSASecretKeySize)
	}
	var sk MLDSASecretKey
	copy(sk[:], data)
	return &sk, nil
}

// =============================================================================
// Verification Interface
// =============================================================================

// Verifier provides signature verification
type Verifier interface {
	Verify(pk *MLDSAPublicKey, message []byte, sig MLDSASignature) (bool, error)
}

// Signer provides signature creation
type Signer interface {
	Sign(sk *MLDSASecretKey, message []byte) (MLDSASignature, error)
}

// KeyGenerator provides key generation
type KeyGenerator interface {
	GenerateKey() (*MLDSAPublicKey, *MLDSASecretKey, error)
}

// =============================================================================
// Placeholder Implementation (for testing without SPARK)
// =============================================================================

// PlaceholderVerifier is a testing verifier that accepts all signatures
type PlaceholderVerifier struct{}

func (v *PlaceholderVerifier) Verify(pk *MLDSAPublicKey, message []byte, sig MLDSASignature) (bool, error) {
	// WARNING: This is for testing only - accepts all signatures
	if len(sig) == 0 {
		return false, ErrInvalidSignature
	}
	// In production, this would call the SPARK verifier
	return true, nil
}

// =============================================================================
// Process-Isolated Verifier (maintains verification guarantees)
// =============================================================================

// ProcessVerifier calls an external SPARK-verified binary for verification
type ProcessVerifier struct {
	binaryPath string
}

// NewProcessVerifier creates a verifier that uses process isolation
func NewProcessVerifier(binaryPath string) *ProcessVerifier {
	return &ProcessVerifier{binaryPath: binaryPath}
}

func (v *ProcessVerifier) Verify(pk *MLDSAPublicKey, message []byte, sig MLDSASignature) (bool, error) {
	// TODO: Implement IPC call to SPARK binary
	// The binary would:
	// 1. Read pk, message, sig from stdin (JSON or binary protocol)
	// 2. Run verified verification code
	// 3. Return result to stdout
	return false, errors.New("process verifier not yet implemented")
}

// =============================================================================
// AAS-001 Address Formatting
// =============================================================================

// FormatAddress formats an address according to AAS-001 v3.1
// Format: mldsa87:network:type:payload-checksum
func FormatAddress(pk *MLDSAPublicKey, network string, addrType string) string {
	accountID := pk.AccountID()
	checksum := sha256.Sum256(accountID)
	return fmt.Sprintf("mldsa87:%s:%s:%s-%s",
		network,
		addrType,
		hex.EncodeToString(accountID[:28]),
		hex.EncodeToString(checksum[:4]),
	)
}

// FormatUserAddress returns a mainnet user address
func FormatUserAddress(pk *MLDSAPublicKey) string {
	return FormatAddress(pk, "main", "u")
}

// FormatContractAddress returns a mainnet contract address
func FormatContractAddress(pk *MLDSAPublicKey) string {
	return FormatAddress(pk, "main", "c")
}
