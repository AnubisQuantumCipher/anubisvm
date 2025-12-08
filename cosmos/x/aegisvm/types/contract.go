// Contract types for x/aegisvm

package types

import (
	"fmt"
	"time"
)

// Proto interface implementations for Cosmos SDK v0.50 compatibility
func (m *Contract) Reset()         { *m = Contract{} }
func (m *Contract) String() string { return fmt.Sprintf("%+v", *m) }
func (m *Contract) ProtoMessage()  {}

func (m *ContractMetadata) Reset()         { *m = ContractMetadata{} }
func (m *ContractMetadata) String() string { return fmt.Sprintf("%+v", *m) }
func (m *ContractMetadata) ProtoMessage()  {}

func (m *ExecutionResult) Reset()         { *m = ExecutionResult{} }
func (m *ExecutionResult) String() string { return fmt.Sprintf("%+v", *m) }
func (m *ExecutionResult) ProtoMessage()  {}

// SecurityProfile defines the required cryptographic profile
const (
	ProfilePQL5Strict = "PQ-L5-Strict"
)

// Contract represents a deployed smart contract
type Contract struct {
	// Address is the contract's unique identifier (20 bytes)
	Address []byte `json:"address"`

	// Creator is the address that deployed the contract
	Creator []byte `json:"creator"`

	// CodeHash is the SHA3-256 hash of the bytecode
	CodeHash []byte `json:"code_hash"`

	// Metadata contains contract certification info
	Metadata ContractMetadata `json:"metadata"`

	// CreatedAt is the block height of deployment
	CreatedAt int64 `json:"created_at"`

	// UpdatedAt is the last update block height
	UpdatedAt int64 `json:"updated_at"`
}

// ContractMetadata contains certification and verification metadata
type ContractMetadata struct {
	// Profile must be "PQ-L5-Strict" for quantum resistance
	Profile string `json:"profile"`

	// ProofHash is the SHA3 of SPARK proof artifacts
	ProofHash []byte `json:"proof_hash"`

	// KATHash is the SHA3 of Known Answer Test vectors
	KATHash []byte `json:"kat_hash"`

	// CertificationLevel (Bronze, Silver, Gold, Platinum)
	CertificationLevel string `json:"certification_level"`

	// WCETBound is the proven worst-case execution time in gas
	WCETBound uint64 `json:"wcet_bound"`

	// SupportsPrivate indicates if contract supports private execution
	SupportsPrivate bool `json:"supports_private"`

	// Auditors lists addresses of independent auditors
	Auditors [][]byte `json:"auditors,omitempty"`

	// AuditTimestamp is when the contract was audited
	AuditTimestamp time.Time `json:"audit_timestamp,omitempty"`
}

// ExecutionResult represents the result of contract execution
type ExecutionResult struct {
	// Success indicates if execution completed without error
	Success bool `json:"success"`

	// ReturnData is the return value from the contract
	ReturnData []byte `json:"return_data"`

	// GasUsed is the amount of gas consumed
	GasUsed uint64 `json:"gas_used"`

	// State is the new contract state
	State []byte `json:"state"`

	// Logs are events emitted during execution
	Logs [][]byte `json:"logs"`

	// StateRoot is the Merkle root of the new state
	StateRoot []byte `json:"state_root"`
}

// CertificationLevel constants
const (
	CertLevelNone     = ""
	CertLevelBronze   = "bronze"
	CertLevelSilver   = "silver"
	CertLevelGold     = "gold"
	CertLevelPlatinum = "platinum"
)

// GasDiscount returns the gas discount percentage for a certification level
func GasDiscount(level string) uint64 {
	switch level {
	case CertLevelBronze:
		return 5 // 5% discount
	case CertLevelSilver:
		return 15 // 15% discount
	case CertLevelGold:
		return 25 // 25% discount
	case CertLevelPlatinum:
		return 35 // 35% discount
	default:
		return 0 // No discount
	}
}

// CertLevelToUint8 converts a certification level string to uint8 for VM bridge
func CertLevelToUint8(level string) uint8 {
	switch level {
	case CertLevelBronze:
		return 1
	case CertLevelSilver:
		return 2
	case CertLevelGold:
		return 3
	case CertLevelPlatinum:
		return 4
	default:
		return 0
	}
}

// ValidateMetadata validates contract metadata
func ValidateMetadata(m ContractMetadata) error {
	if m.Profile != ProfilePQL5Strict {
		return ErrInvalidProfile
	}
	if len(m.ProofHash) != 32 {
		return ErrMissingProofHash
	}
	return nil
}
