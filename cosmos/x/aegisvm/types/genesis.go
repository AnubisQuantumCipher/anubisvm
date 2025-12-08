// Genesis state for x/aegisvm

package types

import (
	"encoding/hex"
	"fmt"
)

// GenesisState defines the aegisvm module's genesis state
type GenesisState struct {
	// Params are the module parameters
	Params Params `json:"params"`

	// Contracts is the list of deployed contracts
	Contracts []GenesisContract `json:"contracts,omitempty"`

	// ApprovedCodes is the list of governance-approved code hashes
	ApprovedCodes []ApprovedCode `json:"approved_codes,omitempty"`
}

// GenesisContract represents a contract in genesis state
type GenesisContract struct {
	// Address is the contract address (hex encoded)
	Address string `json:"address"`

	// Creator is the creator address (bech32)
	Creator string `json:"creator"`

	// Bytecode is the contract bytecode (hex encoded)
	Bytecode string `json:"bytecode"`

	// State is the initial contract state (hex encoded)
	State string `json:"state,omitempty"`

	// Metadata contains contract certification info
	Metadata ContractMetadata `json:"metadata"`

	// Admin is the contract admin address (bech32)
	Admin string `json:"admin,omitempty"`

	// Label is a human-readable label
	Label string `json:"label,omitempty"`
}

// ApprovedCode represents a governance-approved code hash
type ApprovedCode struct {
	// CodeHash is the SHA3-256 of the bytecode (hex encoded)
	CodeHash string `json:"code_hash"`

	// Proposer is the address that proposed approval
	Proposer string `json:"proposer"`

	// Description is a human-readable description
	Description string `json:"description,omitempty"`

	// ProposalID is the governance proposal ID
	ProposalID uint64 `json:"proposal_id"`
}

// DefaultGenesisState returns the default genesis state
func DefaultGenesisState() *GenesisState {
	return &GenesisState{
		Params:        DefaultParams(),
		Contracts:     []GenesisContract{},
		ApprovedCodes: []ApprovedCode{},
	}
}

// ValidateGenesis validates the genesis state
func ValidateGenesis(gs GenesisState) error {
	if err := gs.Params.Validate(); err != nil {
		return fmt.Errorf("invalid params: %w", err)
	}

	// Validate contracts
	seenAddresses := make(map[string]bool)
	for i, contract := range gs.Contracts {
		if contract.Address == "" {
			return fmt.Errorf("contract %d: empty address", i)
		}
		if seenAddresses[contract.Address] {
			return fmt.Errorf("contract %d: duplicate address %s", i, contract.Address)
		}
		seenAddresses[contract.Address] = true

		if contract.Bytecode == "" {
			return fmt.Errorf("contract %d: empty bytecode", i)
		}
		if _, err := hex.DecodeString(contract.Bytecode); err != nil {
			return fmt.Errorf("contract %d: invalid bytecode hex: %w", i, err)
		}

		if err := ValidateMetadata(contract.Metadata); err != nil {
			return fmt.Errorf("contract %d: %w", i, err)
		}
	}

	// Validate approved codes
	seenCodes := make(map[string]bool)
	for i, code := range gs.ApprovedCodes {
		if code.CodeHash == "" {
			return fmt.Errorf("approved code %d: empty code hash", i)
		}
		if seenCodes[code.CodeHash] {
			return fmt.Errorf("approved code %d: duplicate code hash %s", i, code.CodeHash)
		}
		seenCodes[code.CodeHash] = true

		hashBytes, err := hex.DecodeString(code.CodeHash)
		if err != nil {
			return fmt.Errorf("approved code %d: invalid code hash hex: %w", i, err)
		}
		if len(hashBytes) != 32 {
			return fmt.Errorf("approved code %d: code hash must be 32 bytes", i)
		}
	}

	return nil
}
