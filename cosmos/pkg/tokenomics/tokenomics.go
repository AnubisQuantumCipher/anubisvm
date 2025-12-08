// Package tokenomics defines the ANUBIS token economic parameters.
//
// This file contains all token economics constants as specified in the
// ANUBIS Token Specification v4.0 (anubis_token.ads).
//
// Key Properties:
// - Fixed supply: 1,000,000,000 ANUBIS (no inflation, ever)
// - Deflationary: 80% of base fees burned
// - 18 decimals (like ETH)
// - Milestone-based vesting for builder allocation
package tokenomics

import (
	"time"

	sdk "github.com/cosmos/cosmos-sdk/types"
)

// ============================================================================
// Token Parameters (ANUBIS Spec v4.0)
// ============================================================================

const (
	// TokenDenom is the base denomination (atto ANUBIS = 10^-18)
	TokenDenom = "aanubis"

	// TokenName is the human-readable token name
	TokenName = "ANUBIS"

	// TokenSymbol is the token symbol
	TokenSymbol = "ANUBIS"

	// TokenDecimals is the number of decimal places (like ETH)
	TokenDecimals = 18

	// TotalSupply is 1 billion ANUBIS - FIXED, NO INFLATION
	TotalSupply = 1_000_000_000

	// TotalSupplyAtto is total supply in smallest units (10^18)
	// Note: Go doesn't support 10^18 as a constant, use string math
	TotalSupplyAtto = "1000000000000000000000000000" // 1B * 10^18
)

// ============================================================================
// NO INFLATION - Fixed Supply Economics
// ============================================================================

// The ANUBIS token has NO inflation. Supply is fixed at 1 billion tokens.
// Deflation occurs through burn mechanisms:
// - 80% of base transaction fees are burned
// - 100% of failed proposal bonds are burned
// - 50% of slashing amounts are burned
// - 5% of privacy operation fees are burned

// ============================================================================
// Burn Rates (Basis Points, 10000 = 100%)
// ============================================================================

const (
	// BaseFee burn - 80% of all base fees are burned
	BaseFeeeBurnBP = 8000

	// Failed proposal bond - 100% burned
	FailedProposalBurnBP = 10000

	// Slashing burn - 50% of slashed amount burned
	SlashingBurnBP = 5000

	// Privacy fee burn - 5% of privacy operation fees
	PrivacyFeeBurnBP = 500
)

// ============================================================================
// Gas Costs (per ANUBIS Spec)
// ============================================================================

const (
	// MinGasPrice is 0.001 aanubis per gas unit
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
// Certification Gas Discounts
// ============================================================================

const (
	// Discount for Bronze certification (SPARK Mode enabled)
	DiscountBronzeBP = 0 // 0%

	// Discount for Silver certification (All VCs proven)
	DiscountSilverBP = 1000 // 10%

	// Discount for Gold certification (Constant-time verified)
	DiscountGoldBP = 2000 // 20%

	// Discount for Platinum certification (Third-party audited)
	DiscountPlatinumBP = 3000 // 30%
)

// Certification deposit requirements (in ANUBIS tokens)
const (
	DepositBronze   = 1_000   // 1,000 ANUBIS
	DepositSilver   = 10_000  // 10,000 ANUBIS
	DepositGold     = 50_000  // 50,000 ANUBIS
	DepositPlatinum = 100_000 // 100,000 ANUBIS
)

// ============================================================================
// Staking Parameters
// ============================================================================

const (
	// Validator requirements
	MinValidatorStakeANUBIS = 100_000 // 100,000 ANUBIS

	// MaxValidators is the maximum number of validators
	MaxValidators = 150

	// UnbondingTime is 21 days
	UnbondingTime = 21 * 24 * time.Hour

	// Slashing rates (basis points, 10000 = 100%)
	SlashDoubleSignBP   = 500  // 5% for double signing
	SlashDowntimeBP     = 100  // 1% for extended downtime
	SlashInvalidProofBP = 1000 // 10% for submitting invalid ZK proof

	// Prover staking
	MinProverStakeANUBIS = 10_000 // 10,000 ANUBIS
	ProverSlashInvalidBP = 2000   // 20% for invalid proof
)

// ============================================================================
// Governance Parameters
// ============================================================================

const (
	// Proposals
	MinDepositANUBIS = 1_000 // 1,000 ANUBIS

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
// Token Allocation (Genesis Distribution) - ANUBIS v4.0
// ============================================================================

// AllocationCategory represents the token allocation categories
type AllocationCategory string

const (
	AllocSoloBuilder       AllocationCategory = "solo_builder"
	AllocProtocolTreasury  AllocationCategory = "protocol_treasury"
	AllocGenesisValidators AllocationCategory = "genesis_validators"
	AllocGenesisProvers    AllocationCategory = "genesis_provers"
	AllocDeveloperEcosystem AllocationCategory = "developer_ecosystem"
	AllocQuantumInsurance  AllocationCategory = "quantum_insurance"
	AllocBugBounties       AllocationCategory = "bug_bounties"
)

// GenesisAllocation defines token distribution at genesis
type GenesisAllocation struct {
	Category    AllocationCategory
	Name        string
	Percentage  uint64 // Basis points (10000 = 100%)
	Amount      uint64 // In ANUBIS tokens (not smallest units)
	Vesting     string // Vesting schedule description
	Description string
}

// DefaultGenesisAllocations per ANUBIS Token Spec v4.0
var DefaultGenesisAllocations = []GenesisAllocation{
	{
		Category:    AllocSoloBuilder,
		Name:        "Solo Builder",
		Percentage:  3000, // 30%
		Amount:      300_000_000,
		Vesting:     "Milestone-based over 6 years",
		Description: "Builder allocation, vested via verified milestone delivery",
	},
	{
		Category:    AllocProtocolTreasury,
		Name:        "Protocol Treasury",
		Percentage:  3000, // 30%
		Amount:      300_000_000,
		Vesting:     "DAO-controlled from day 1",
		Description: "DAO treasury, builder has ZERO access",
	},
	{
		Category:    AllocGenesisValidators,
		Name:        "Genesis Validators",
		Percentage:  1500, // 15%
		Amount:      150_000_000,
		Vesting:     "Earned via block production",
		Description: "Rewards for genesis validator set",
	},
	{
		Category:    AllocGenesisProvers,
		Name:        "Genesis Provers",
		Percentage:  800, // 8%
		Amount:      80_000_000,
		Vesting:     "Earned via proof generation over 3 years",
		Description: "Rewards for ZK proof miners",
	},
	{
		Category:    AllocDeveloperEcosystem,
		Name:        "Developer Ecosystem",
		Percentage:  700, // 7%
		Amount:      70_000_000,
		Vesting:     "Earned via verified code commits",
		Description: "Code mining rewards for contributors",
	},
	{
		Category:    AllocQuantumInsurance,
		Name:        "Quantum Insurance Reserve",
		Percentage:  500, // 5%
		Amount:      50_000_000,
		Vesting:     "Immutably locked for 20 years",
		Description: "Emergency fund for post-quantum migration",
	},
	{
		Category:    AllocBugBounties,
		Name:        "Bug Bounties & Audits",
		Percentage:  500, // 5%
		Amount:      50_000_000,
		Vesting:     "On-chain claim system",
		Description: "Security rewards and audit funding",
	},
}

// ============================================================================
// Builder Milestone Vesting (30% = 300M ANUBIS)
// ============================================================================

// Milestone represents a vesting milestone for the builder allocation
type Milestone struct {
	ID           uint8
	Name         string
	Allocation   uint64 // ANUBIS tokens
	Percentage   uint64 // Basis points of builder allocation
	Verification string
}

// BuilderMilestones defines the 9 vesting milestones
var BuilderMilestones = []Milestone{
	{1, "ML-DSA-87 SPARK Implementation", 50_000_000, 1667, "GNATprove + Audit"},
	{2, "ML-KEM-1024 SPARK Implementation", 50_000_000, 1667, "GNATprove + Audit"},
	{3, "Core VM with WCET Gas Model", 50_000_000, 1667, "Test Suite + Audit"},
	{4, "Cosmos SDK PQ Integration", 50_000_000, 1667, "Testnet Launch"},
	{5, "zk-STARK Prover (Basic)", 30_000_000, 1000, "Proof Verification"},
	{6, "Privacy Layer (Shield, Whisper)", 30_000_000, 1000, "Testnet Demo"},
	{7, "Mainnet Launch", 20_000_000, 667, "Block 1 Produced"},
	{8, "1 Year Mainnet Stability", 10_000_000, 333, "99.9% Uptime"},
	{9, "2 Year Mainnet Stability", 10_000_000, 333, "No Critical Bugs"},
}

// ============================================================================
// Helper Functions
// ============================================================================

// NewCoin creates a new aanubis coin
func NewCoin(amount int64) sdk.Coin {
	return sdk.NewInt64Coin(TokenDenom, amount)
}

// NewCoins creates a new aanubis coin set
func NewCoins(amount int64) sdk.Coins {
	return sdk.NewCoins(NewCoin(amount))
}

// ANUBISToAtto converts ANUBIS to aanubis (atto ANUBIS, 10^-18)
func ANUBISToAtto(anubis uint64) string {
	// Since Go can't handle 10^18 as uint64, return as string
	// In production, use big.Int
	return sdk.NewIntFromUint64(anubis).Mul(sdk.NewIntFromUint64(1_000_000_000_000_000_000)).String()
}

// GetAllocationAmount returns the allocation for a category
func GetAllocationAmount(category AllocationCategory) uint64 {
	for _, alloc := range DefaultGenesisAllocations {
		if alloc.Category == category {
			return alloc.Amount
		}
	}
	return 0
}

// GetMilestoneAllocation returns allocation for a specific milestone
func GetMilestoneAllocation(milestoneID uint8) uint64 {
	for _, m := range BuilderMilestones {
		if m.ID == milestoneID {
			return m.Allocation
		}
	}
	return 0
}

// ============================================================================
// Denom Metadata
// ============================================================================

// DenomMetadata returns bank module denom metadata for ANUBIS
func DenomMetadata() []DenomUnit {
	return []DenomUnit{
		{Denom: "aanubis", Exponent: 0, Aliases: []string{"attoanubis"}},
		{Denom: "uanubis", Exponent: 12, Aliases: []string{"microanubis"}},
		{Denom: "manubis", Exponent: 15, Aliases: []string{"millianubis"}},
		{Denom: "anubis", Exponent: 18, Aliases: []string{"ANUBIS"}},
	}
}

// DenomUnit represents a denomination unit
type DenomUnit struct {
	Denom    string
	Exponent uint32
	Aliases  []string
}

// ============================================================================
// Verification
// ============================================================================

// VerifyTotalAllocation ensures all allocations sum to 100%
func VerifyTotalAllocation() bool {
	var total uint64
	for _, alloc := range DefaultGenesisAllocations {
		total += alloc.Percentage
	}
	return total == 10000 // 100%
}

// VerifyBuilderMilestones ensures milestone allocations sum to 300M
func VerifyBuilderMilestones() bool {
	var total uint64
	for _, m := range BuilderMilestones {
		total += m.Allocation
	}
	return total == 300_000_000
}

func init() {
	if !VerifyTotalAllocation() {
		panic("tokenomics: genesis allocations do not sum to 100%")
	}
	if !VerifyBuilderMilestones() {
		panic("tokenomics: builder milestones do not sum to 300M")
	}
}
