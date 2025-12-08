// Module parameters for x/aegisvm

package types

import (
	"fmt"
)

// Proto interface implementations for Cosmos SDK v0.50 compatibility
func (m *Params) Reset()         { *m = Params{} }
func (m *Params) String() string { return fmt.Sprintf("%+v", *m) }
func (m *Params) ProtoMessage()  {}

// Default parameter values
const (
	DefaultMaxBytecodeSize    = uint64(512 * 1024) // 512 KB
	DefaultMaxGasLimit        = uint64(10_000_000)
	DefaultRequireGovernance  = true
	DefaultMinCertLevel       = CertLevelBronze
	DefaultBaseGasPrice       = uint64(1)
	DefaultPrivateExecEnabled = false
	DefaultWCETEnforcement    = true
)

// Params defines the module parameters
type Params struct {
	// MaxBytecodeSize is the maximum allowed bytecode size in bytes
	MaxBytecodeSize uint64 `json:"max_bytecode_size"`

	// MaxGasLimit is the maximum gas limit per transaction
	MaxGasLimit uint64 `json:"max_gas_limit"`

	// RequireGovernance requires code hash approval before deployment
	RequireGovernance bool `json:"require_governance"`

	// MinCertLevel is the minimum required certification level
	MinCertLevel string `json:"min_cert_level"`

	// BaseGasPrice is the base gas price in uaegis
	BaseGasPrice uint64 `json:"base_gas_price"`

	// PrivateExecEnabled enables private execution mode
	PrivateExecEnabled bool `json:"private_exec_enabled"`

	// WCETEnforcement enables worst-case execution time enforcement
	WCETEnforcement bool `json:"wcet_enforcement"`
}

// DefaultParams returns the default module parameters
func DefaultParams() Params {
	return Params{
		MaxBytecodeSize:    DefaultMaxBytecodeSize,
		MaxGasLimit:        DefaultMaxGasLimit,
		RequireGovernance:  DefaultRequireGovernance,
		MinCertLevel:       DefaultMinCertLevel,
		BaseGasPrice:       DefaultBaseGasPrice,
		PrivateExecEnabled: DefaultPrivateExecEnabled,
		WCETEnforcement:    DefaultWCETEnforcement,
	}
}

// Validate validates the parameters
func (p Params) Validate() error {
	if err := validateMaxBytecodeSize(p.MaxBytecodeSize); err != nil {
		return err
	}
	if err := validateMaxGasLimit(p.MaxGasLimit); err != nil {
		return err
	}
	if err := validateCertLevel(p.MinCertLevel); err != nil {
		return err
	}
	if err := validateBaseGasPrice(p.BaseGasPrice); err != nil {
		return err
	}
	return nil
}

func validateMaxBytecodeSize(v uint64) error {
	if v == 0 {
		return fmt.Errorf("max bytecode size cannot be zero")
	}
	if v > 10*1024*1024 { // 10 MB hard limit
		return fmt.Errorf("max bytecode size too large: %d", v)
	}
	return nil
}

func validateMaxGasLimit(v uint64) error {
	if v == 0 {
		return fmt.Errorf("max gas limit cannot be zero")
	}
	return nil
}

func validateCertLevel(v string) error {
	switch v {
	case CertLevelNone, CertLevelBronze, CertLevelSilver, CertLevelGold, CertLevelPlatinum:
		return nil
	default:
		return fmt.Errorf("invalid certification level: %s", v)
	}
}

func validateBaseGasPrice(v uint64) error {
	if v == 0 {
		return fmt.Errorf("base gas price cannot be zero")
	}
	return nil
}
