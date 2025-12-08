// Expected keeper interfaces for x/anubis

package types

import (
	"context"

	sdk "github.com/cosmos/cosmos-sdk/types"

	aegisvmtypes "github.com/aegisvm/cosmos/x/aegisvm/types"
)

// AegisVMKeeper defines the expected aegisvm keeper interface
type AegisVMKeeper interface {
	// GetContract returns a contract by address
	GetContract(ctx context.Context, addr []byte) (aegisvmtypes.Contract, bool)

	// GetContractState returns contract state
	GetContractState(ctx context.Context, addr []byte) ([]byte, bool)

	// SetContractState stores contract state
	SetContractState(ctx context.Context, addr, state []byte)

	// ExecuteContract executes a contract
	ExecuteContract(ctx context.Context, contractAddr, caller, function, args []byte, gasLimit uint64) (*aegisvmtypes.ExecutionResult, error)
}

// BankKeeper defines the expected bank keeper interface
type BankKeeper interface {
	// SpendableCoins returns the spendable coins for an account
	SpendableCoins(ctx context.Context, addr sdk.AccAddress) sdk.Coins

	// SendCoins transfers coins from one account to another
	SendCoins(ctx context.Context, from, to sdk.AccAddress, amt sdk.Coins) error

	// SendCoinsFromAccountToModule transfers coins from an account to a module
	SendCoinsFromAccountToModule(ctx context.Context, senderAddr sdk.AccAddress, recipientModule string, amt sdk.Coins) error

	// SendCoinsFromModuleToAccount transfers coins from a module to an account
	SendCoinsFromModuleToAccount(ctx context.Context, senderModule string, recipientAddr sdk.AccAddress, amt sdk.Coins) error

	// MintCoins mints new coins to a module account
	MintCoins(ctx context.Context, moduleName string, amt sdk.Coins) error

	// BurnCoins burns coins from a module account
	BurnCoins(ctx context.Context, moduleName string, amt sdk.Coins) error
}
