// Package ante provides custom ante handlers for AegisVM.
//
// This implements post-quantum signature verification using ML-DSA-87
// and custom gas metering per the AegisVM Cosmos Chain blueprint v1.0.
package ante

import (
	storetypes "cosmossdk.io/store/types"

	"github.com/cosmos/cosmos-sdk/types/tx/signing"
	"github.com/cosmos/cosmos-sdk/x/auth/ante"
	authtypes "github.com/cosmos/cosmos-sdk/x/auth/types"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
)

// ============================================================================
// Gas Costs (per AegisVM blueprint Part VII.2)
// ============================================================================

const (
	// Signature verification costs
	PQSigVerifyCost = storetypes.Gas(6000) // ML-DSA-87 verify

	// Crypto precompile costs
	GasCostMLDSASign   = storetypes.Gas(10000)
	GasCostMLDSAVerify = storetypes.Gas(6000)
	GasCostMLKEMEncaps = storetypes.Gas(5000)
	GasCostMLKEMDecaps = storetypes.Gas(5000)
	GasCostSHA3Base    = storetypes.Gas(30)
	GasCostSHA3PerWord = storetypes.Gas(6)

	// Contract costs
	GasCostContractCall   = storetypes.Gas(21000)
	GasCostContractCreate = storetypes.Gas(500000)
	GasCostBytecodePerByte = storetypes.Gas(200)

	// Privacy operation costs
	GasCostConfidentialTransfer = storetypes.Gas(500000)
	GasCostPrivateExecution     = storetypes.Gas(1000000)
	GasCostCreateDisclosure     = storetypes.Gas(100000)
	GasCostRangeProofVerify     = storetypes.Gas(200000)
	GasCostSTARKVerify          = storetypes.Gas(500000)
	GasCostShieldDeposit        = storetypes.Gas(150000)
	GasCostUnshieldWithdraw     = storetypes.Gas(200000)
)

// DefaultSigVerificationGasConsumer is the default implementation of SignatureVerificationGasConsumer.
// It consumes gas for signature verification based on the public key type.
func DefaultSigVerificationGasConsumer(meter storetypes.GasMeter, sig signing.SignatureV2, params authtypes.Params) error {
	pubkey := sig.PubKey

	switch pubkey.(type) {
	case *pqcrypto.SDKPubKey:
		// ML-DSA-87 signature verification cost
		meter.ConsumeGas(PQSigVerifyCost, "ante verify: mldsa87")
		return nil
	default:
		// Also check by type string for ML-DSA-87 keys
		if pubkey != nil && pubkey.Type() == pqcrypto.KeyType {
			meter.ConsumeGas(PQSigVerifyCost, "ante verify: mldsa87")
			return nil
		}
		// Use default gas consumption for other key types
		return ante.DefaultSigVerificationGasConsumer(meter, sig, params)
	}
}

// ============================================================================
// Gas Calculation Helpers
// ============================================================================

// CalculateSHA3Gas calculates gas for SHA3 hashing based on input size
func CalculateSHA3Gas(inputBytes int) storetypes.Gas {
	words := storetypes.Gas((inputBytes + 31) / 32)
	return GasCostSHA3Base + GasCostSHA3PerWord*words
}

// CalculateDeployGas calculates gas for contract deployment
func CalculateDeployGas(bytecodeLen int) storetypes.Gas {
	return GasCostContractCreate + storetypes.Gas(bytecodeLen)*GasCostBytecodePerByte
}

// ============================================================================
// Certification Level Gas Discounts
// ============================================================================

// CertificationLevel represents KHEPRI contract certification
type CertificationLevel uint8

const (
	CertNone     CertificationLevel = 0 // No certification
	CertBronze   CertificationLevel = 1 // Type-safe, bounds-checked
	CertSilver   CertificationLevel = 2 // SPARK-proved (no runtime errors)
	CertGold     CertificationLevel = 3 // Functional correctness
	CertPlatinum CertificationLevel = 4 // Full security properties
)

// GasDiscountBP returns the gas discount in basis points for a certification level
func GasDiscountBP(level CertificationLevel) uint64 {
	switch level {
	case CertBronze:
		return 0 // 0% discount
	case CertSilver:
		return 1000 // 10% discount
	case CertGold:
		return 2000 // 20% discount
	case CertPlatinum:
		return 3000 // 30% discount
	default:
		return 0
	}
}

// ApplyGasDiscount applies certification-based gas discount
func ApplyGasDiscount(gas storetypes.Gas, level CertificationLevel) storetypes.Gas {
	discount := GasDiscountBP(level)
	if discount == 0 {
		return gas
	}
	return storetypes.Gas(uint64(gas) * (10000 - discount) / 10000)
}

// ============================================================================
// Fee Validation
// ============================================================================

// MinGasPriceUAegis returns minimum gas price in uaegis (0.001 = 1/1000)
func MinGasPriceUAegis() uint64 {
	return 1 // 0.001 uaegis per gas (effectively 1 uaegis per 1000 gas)
}

// CalculateMinFee calculates minimum fee for given gas
func CalculateMinFee(gas uint64) uint64 {
	// 0.001 uaegis per gas = gas / 1000
	return (gas + 999) / 1000 // Round up
}
