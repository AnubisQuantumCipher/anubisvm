// Module parameters for x/anubis

package types

import (
	"fmt"
)

// Default parameter values
const (
	DefaultPrivateExecEnabled   = false
	DefaultMaxEncryptedCallSize = uint64(64 * 1024) // 64 KB
	DefaultMaxSessionDuration   = int64(1000)       // 1000 blocks
	DefaultMaxSessionCalls      = uint64(100)
	DefaultZKProofTimeout       = int64(50) // 50 blocks
	DefaultFHEEnabled           = false
	DefaultMinGasForPrivate     = uint64(100000)

	// ANUBIS Prover Network defaults
	DefaultMinProverStake   = uint64(1000000000) // 1000 AEGIS (in uaegis)
	DefaultUnbondingPeriod  = int64(14400)       // ~1 day at 6s blocks
	DefaultMaxProvers       = uint32(100)
	DefaultProverCommission = uint32(1000)       // 10% default (basis points)
)

// Params defines the module parameters
type Params struct {
	// PrivateExecEnabled enables private execution globally
	PrivateExecEnabled bool `json:"private_exec_enabled"`

	// MaxEncryptedCallSize is the maximum size of encrypted call data
	MaxEncryptedCallSize uint64 `json:"max_encrypted_call_size"`

	// MaxSessionDuration is the maximum session duration in blocks
	MaxSessionDuration int64 `json:"max_session_duration"`

	// MaxSessionCalls is the maximum calls per session
	MaxSessionCalls uint64 `json:"max_session_calls"`

	// ZKProofTimeout is the timeout for ZK proof submission in blocks
	ZKProofTimeout int64 `json:"zk_proof_timeout"`

	// FHEEnabled enables fully homomorphic encryption operations
	FHEEnabled bool `json:"fhe_enabled"`

	// MinGasForPrivate is the minimum gas required for private calls
	MinGasForPrivate uint64 `json:"min_gas_for_private"`

	// ANUBIS Prover Network parameters
	// MinProverStake is the minimum stake required to be a prover
	MinProverStake uint64 `json:"min_prover_stake"`

	// UnbondingPeriod is the number of blocks for unbonding
	UnbondingPeriod int64 `json:"unbonding_period"`

	// MaxProvers is the maximum number of active provers
	MaxProvers uint32 `json:"max_provers"`
}

// DefaultParams returns the default module parameters
func DefaultParams() Params {
	return Params{
		PrivateExecEnabled:   DefaultPrivateExecEnabled,
		MaxEncryptedCallSize: DefaultMaxEncryptedCallSize,
		MaxSessionDuration:   DefaultMaxSessionDuration,
		MaxSessionCalls:      DefaultMaxSessionCalls,
		ZKProofTimeout:       DefaultZKProofTimeout,
		FHEEnabled:           DefaultFHEEnabled,
		MinGasForPrivate:     DefaultMinGasForPrivate,
		MinProverStake:       DefaultMinProverStake,
		UnbondingPeriod:      DefaultUnbondingPeriod,
		MaxProvers:           DefaultMaxProvers,
	}
}

// Validate validates the parameters
func (p Params) Validate() error {
	if p.MaxEncryptedCallSize == 0 {
		return fmt.Errorf("max_encrypted_call_size must be positive")
	}
	if p.MaxSessionDuration <= 0 {
		return fmt.Errorf("max_session_duration must be positive")
	}
	if p.MaxSessionCalls == 0 {
		return fmt.Errorf("max_session_calls must be positive")
	}
	if p.ZKProofTimeout <= 0 {
		return fmt.Errorf("zk_proof_timeout must be positive")
	}
	if p.MinGasForPrivate == 0 {
		return fmt.Errorf("min_gas_for_private must be positive")
	}
	return nil
}
