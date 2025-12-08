// Expected keeper interfaces for x/aegisvm

package types

import (
	"context"

	sdk "github.com/cosmos/cosmos-sdk/types"
)

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
}

// AccountKeeper defines the expected account keeper interface
type AccountKeeper interface {
	// GetAccount returns an account or nil if not found
	GetAccount(ctx context.Context, addr sdk.AccAddress) sdk.AccountI

	// SetAccount stores an account
	SetAccount(ctx context.Context, acc sdk.AccountI)

	// GetModuleAddress returns the module account address
	GetModuleAddress(moduleName string) sdk.AccAddress

	// GetModuleAccount returns the module account
	GetModuleAccount(ctx context.Context, moduleName string) sdk.ModuleAccountI
}

// GovKeeper defines the expected governance keeper interface
type GovKeeper interface {
	// GetProposal returns a proposal by ID
	GetProposal(ctx context.Context, proposalID uint64) (interface{}, bool)
}
