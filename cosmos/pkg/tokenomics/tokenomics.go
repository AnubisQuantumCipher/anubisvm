// Package tokenomics defines the AEGIS token economic parameters.
//
// This file contains all token economics constants as specified in the
// AegisVM Cosmos Chain blueprint v1.0.
package tokenomics

import (
	"time"

	sdk "github.com/cosmos/cosmos-sdk/types"
)

// ============================================================================
// Token Parameters
// ============================================================================

const (
	// TokenDenom is the base denomination (micro AEGIS = 10^-6)
	TokenDenom = "uaegis"

	// TokenName is the human-readable token name
	TokenName = "AEGIS"

	// TokenSymbol is the token symbol
	TokenSymbol = "AEGIS"

	// TokenDecimals is the number of decimal places
	TokenDecimals = 6

	// Supply (in AEGIS, multiply by 10^6 for uaegis)

	// InitialSupply is 1 billion AEGIS
	InitialSupply = 1_000_000_000

	// MaxSupply is 10 billion AEGIS (with inflation)
	MaxSupply = 10_000_000_000

	// InitialSupplyUAegis is initial supply in micro units
	InitialSupplyUAegis = InitialSupply * 1_000_000

	// MaxSupplyUAegis is max supply in micro units
	MaxSupplyUAegis = MaxSupply * 1_000_000
)

// ============================================================================
// Inflation Parameters
// ============================================================================

const (
	// InitialInflation is 7% initial annual inflation
	InitialInflationBP = 700 // Basis points (700 = 7%)

	// MinInflation is 2% floor
	MinInflationBP = 200 // Basis points (200 = 2%)

	// InflationDecay is 10% reduction per year
	InflationDecayBP = 1000 // Basis points (1000 = 10%)

	// BlocksPerYear estimates (5 second blocks)
	BlocksPerYear = 6_307_200 // ~365.25 * 24 * 60 * 12
)

// InflationDistribution defines how inflation is distributed
type InflationDistribution struct {
	StakingRewards uint64 // Basis points (4000 = 40%)
	ProverRewards  uint64 // Basis points (3000 = 30%)
	Treasury       uint64 // Basis points (2000 = 20%)
	Burn           uint64 // Basis points (1000 = 10%)
}

// DefaultInflationDistribution per blueprint
var DefaultInflationDistribution = InflationDistribution{
	StakingRewards: 4000, // 40%
	ProverRewards:  3000, // 30%
	Treasury:       2000, // 20%
	Burn:           1000, // 10%
}

// ============================================================================
// Gas Costs (per blueprint Part VII.2)
// ============================================================================

const (
	// MinGasPrice is 0.001 uaegis per gas unit
	MinGasPriceNumerator   = 1
	MinGasPriceDenominator = 1000

	// Standard operations
	GasTransfer       uint64 = 65_000
	GasContractDeploy uint64 = 500_000 // Plus bytecode_len * 200
	GasContractCall   uint64 = 21_000  // Plus execution gas

	// Bytecode gas multiplier
	GasPerBytecodeBytes uint64 = 200

	// Privacy operations (more expensive due to proofs)
	GasConfidentialTransfer uint64 = 500_000
	GasPrivateExecution     uint64 = 1_000_000 // Plus execution gas
	GasCreateDisclosure     uint64 = 100_000
	GasRangeProofVerify     uint64 = 200_000
	GasSTARKVerify          uint64 = 500_000

	// Crypto precompiles
	GasMLKEMEncaps  uint64 = 50_000
	GasMLKEMDecaps  uint64 = 50_000
	GasMLDSASign    uint64 = 100_000
	GasMLDSAVerify  uint64 = 60_000
	GasSHA3Base     uint64 = 30
	GasSHA3PerWord  uint64 = 6

	// Shield operations
	GasShieldDeposit    uint64 = 150_000
	GasUnshieldWithdraw uint64 = 200_000
)

// CalculateSHA3Gas calculates gas for SHA3 based on input size
func CalculateSHA3Gas(inputBytes int) uint64 {
	words := uint64((inputBytes + 31) / 32) // 32-byte words
	return GasSHA3Base + GasSHA3PerWord*words
}

// CalculateDeployGas calculates gas for contract deployment
func CalculateDeployGas(bytecodeLen int) uint64 {
	return GasContractDeploy + uint64(bytecodeLen)*GasPerBytecodeBytes
}

// ============================================================================
// Staking Parameters
// ============================================================================

const (
	// Validator requirements
	MinValidatorStakeAegis = 100_000          // 100,000 AEGIS
	MinValidatorStake      = 100_000_000_000  // In uaegis

	// MaxValidators is the maximum number of validators
	MaxValidators = 150

	// UnbondingTime is 21 days
	UnbondingTime = 21 * 24 * time.Hour

	// Slashing rates (basis points, 10000 = 100%)
	SlashDoubleSignBP   = 500  // 5% for double signing
	SlashDowntimeBP     = 100  // 1% for extended downtime
	SlashInvalidProofBP = 1000 // 10% for submitting invalid ZK proof

	// Prover staking
	MinProverStakeAegis = 10_000            // 10,000 AEGIS
	MinProverStake      = 10_000_000_000    // In uaegis
	ProverSlashInvalidBP = 2000             // 20% for invalid proof
)

// ============================================================================
// Governance Parameters
// ============================================================================

const (
	// Proposals
	MinDepositAegis = 1_000          // 1,000 AEGIS
	MinDeposit      = 1_000_000_000  // In uaegis

	// MaxDepositPeriod is 14 days
	MaxDepositPeriod = 14 * 24 * time.Hour

	// VotingPeriod is 7 days
	VotingPeriod = 7 * 24 * time.Hour

	// Thresholds (basis points)
	QuorumBP        = 3340 // 33.4% participation
	ThresholdBP     = 5000 // 50% to pass
	VetoThresholdBP = 3340 // 33.4% to veto

	// PQ threshold for critical changes
	CriticalQuorumBP    = 6000 // 60% participation
	CriticalThresholdBP = 8000 // 80% to pass
	CriticalVetoBP      = 2000 // 20% to veto

	// ML-DSA threshold signatures for emergency actions
	EmergencySigners   = 5
	EmergencyThreshold = 3 // 3/5 ML-DSA signatures
)

// ============================================================================
// Token Allocation (Genesis Distribution)
// ============================================================================

// GenesisAllocation defines token distribution at genesis
type GenesisAllocation struct {
	Name       string
	Percentage uint64 // Basis points (10000 = 100%)
	Amount     uint64 // In uaegis
	Address    string // Bech32 address (set at genesis)
}

// DefaultGenesisAllocations per blueprint
var DefaultGenesisAllocations = []GenesisAllocation{
	{
		Name:       "Treasury",
		Percentage: 2000, // 20%
		Amount:     200_000_000_000_000, // 200M AEGIS in uaegis
	},
	{
		Name:       "Foundation",
		Percentage: 1000, // 10%
		Amount:     100_000_000_000_000, // 100M AEGIS in uaegis
	},
	{
		Name:       "Community Pool",
		Percentage: 1500, // 15%
		Amount:     150_000_000_000_000,
	},
	{
		Name:       "Ecosystem Development",
		Percentage: 1500, // 15%
		Amount:     150_000_000_000_000,
	},
	{
		Name:       "Initial Validators",
		Percentage: 1000, // 10%
		Amount:     100_000_000_000_000,
	},
	{
		Name:       "Prover Incentives",
		Percentage: 1000, // 10%
		Amount:     100_000_000_000_000,
	},
	{
		Name:       "Team & Advisors",
		Percentage: 1000, // 10% (vested)
		Amount:     100_000_000_000_000,
	},
	{
		Name:       "Public Sale",
		Percentage: 1000, // 10%
		Amount:     100_000_000_000_000,
	},
}

// ============================================================================
// Helper Functions
// ============================================================================

// NewCoin creates a new uaegis coin
func NewCoin(amount int64) sdk.Coin {
	return sdk.NewInt64Coin(TokenDenom, amount)
}

// NewCoins creates a new uaegis coin set
func NewCoins(amount int64) sdk.Coins {
	return sdk.NewCoins(NewCoin(amount))
}

// AegisToUAegis converts AEGIS to uaegis (micro AEGIS)
func AegisToUAegis(aegis uint64) uint64 {
	return aegis * 1_000_000
}

// UAegisToAegis converts uaegis to AEGIS
func UAegisToAegis(uaegis uint64) uint64 {
	return uaegis / 1_000_000
}

// ============================================================================
// Denom Metadata
// ============================================================================

// DenomMetadata returns bank module denom metadata for AEGIS
func DenomMetadata() []DenomUnit {
	return []DenomUnit{
		{Denom: "uaegis", Exponent: 0, Aliases: []string{"microaegis"}},
		{Denom: "maegis", Exponent: 3, Aliases: []string{"milliaegis"}},
		{Denom: "aegis", Exponent: 6, Aliases: []string{}},
	}
}

// DenomUnit represents a denomination unit
type DenomUnit struct {
	Denom    string
	Exponent uint32
	Aliases  []string
}
