// Provers keeper operations for prover network management

package keeper

import (
	"context"
	"strconv"

	storetypes "cosmossdk.io/store/types"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/x/anubis/types"
)

// RegisterProver registers a new ZK prover.
func (k Keeper) RegisterProver(
	ctx context.Context,
	msg *types.MsgRegisterProver,
) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	operatorAddr, err := sdk.AccAddressFromBech32(msg.Operator)
	if err != nil {
		return err
	}

	// Check if prover already exists
	if _, found := k.GetProver(ctx, operatorAddr); found {
		return types.ErrProverAlreadyExists
	}

	// Verify stake amount meets minimum
	params, _ := k.GetParams(ctx)
	if msg.StakeAmount < params.MinProverStake {
		return types.ErrInsufficientStake
	}

	// Transfer stake to module
	stakeCoins := sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(msg.StakeAmount)))
	if err := k.bankKeeper.SendCoinsFromAccountToModule(
		sdkCtx, operatorAddr, types.ModuleName, stakeCoins,
	); err != nil {
		return err
	}

	// Create prover record
	prover := &types.Prover{
		Operator:    msg.Operator,
		PK:          msg.ProverPK,
		Stake:       msg.StakeAmount,
		Commission:  msg.Commission,
		Status:      types.ProverStatusActive,
		JoinedAt:    sdkCtx.BlockHeight(),
		ProofsCount: 0,
		Slashed:     0,
	}

	k.SetProver(ctx, prover)

	// Emit event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeProverRegistered,
			sdk.NewAttribute(types.AttributeKeyOperator, msg.Operator),
			sdk.NewAttribute(types.AttributeKeyStake, strconv.FormatUint(msg.StakeAmount, 10)),
		),
	)

	return nil
}

// GetProver retrieves a prover by operator address.
func (k Keeper) GetProver(ctx context.Context, operatorAddr sdk.AccAddress) (*types.Prover, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetProverKey(operatorAddr)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}

	var prover types.Prover
	k.cdc.MustUnmarshal(bz, &prover)
	return &prover, true
}

// SetProver stores a prover record.
func (k Keeper) SetProver(ctx context.Context, prover *types.Prover) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)

	operatorAddr, _ := sdk.AccAddressFromBech32(prover.Operator)
	key := types.GetProverKey(operatorAddr)
	bz := k.cdc.MustMarshal(prover)
	store.Set(key, bz)
}

// SlashProver slashes a prover for misbehavior.
func (k Keeper) SlashProver(ctx context.Context, operatorAddr sdk.AccAddress, slashType types.SlashType) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	prover, found := k.GetProver(ctx, operatorAddr)
	if !found {
		return types.ErrProverNotFound
	}

	// Calculate slash amount based on type
	var slashPercent uint64
	switch slashType {
	case types.SlashInvalidProof:
		slashPercent = 20 // 20% for invalid proof
	case types.SlashDowntime:
		slashPercent = 5 // 5% for downtime
	case types.SlashDoubleProve:
		slashPercent = 50 // 50% for double proving
	default:
		slashPercent = 10
	}

	slashAmount := prover.Stake * slashPercent / 100
	prover.Stake -= slashAmount
	prover.Slashed += slashAmount

	// If stake falls below minimum, deactivate prover
	params, _ := k.GetParams(ctx)
	if prover.Stake < params.MinProverStake {
		prover.Status = types.ProverStatusInactive
	}

	k.SetProver(ctx, prover)

	// Burn slashed tokens
	slashCoins := sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(slashAmount)))
	if err := k.bankKeeper.BurnCoins(sdkCtx, types.ModuleName, slashCoins); err != nil {
		k.Logger(ctx).Error("failed to burn slashed tokens", "error", err)
	}

	// Emit event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeProverSlashed,
			sdk.NewAttribute(types.AttributeKeyOperator, prover.Operator),
			sdk.NewAttribute(types.AttributeKeySlashAmount, strconv.FormatUint(slashAmount, 10)),
			sdk.NewAttribute(types.AttributeKeySlashType, string(slashType)),
		),
	)

	return nil
}

// UnbondProver initiates unbonding for a prover.
func (k Keeper) UnbondProver(ctx context.Context, operatorAddr sdk.AccAddress) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	prover, found := k.GetProver(ctx, operatorAddr)
	if !found {
		return types.ErrProverNotFound
	}

	if prover.Status != types.ProverStatusActive {
		return types.ErrProverNotActive
	}

	// Set to unbonding
	prover.Status = types.ProverStatusUnbonding
	prover.UnbondingAt = sdkCtx.BlockHeight()
	k.SetProver(ctx, prover)

	// Emit event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeProverUnbonding,
			sdk.NewAttribute(types.AttributeKeyOperator, prover.Operator),
		),
	)

	return nil
}

// CompleteUnbonding completes the unbonding process for provers.
func (k Keeper) CompleteUnbonding(ctx context.Context) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	currentHeight := sdkCtx.BlockHeight()

	params, _ := k.GetParams(ctx)
	unbondingPeriod := params.UnbondingPeriod

	iterator := storetypes.KVStorePrefixIterator(store, types.ProverKeyPrefix)
	defer iterator.Close()

	for ; iterator.Valid(); iterator.Next() {
		var prover types.Prover
		k.cdc.MustUnmarshal(iterator.Value(), &prover)

		if prover.Status == types.ProverStatusUnbonding &&
			currentHeight >= prover.UnbondingAt+unbondingPeriod {
			// Return stake to operator
			operatorAddr, _ := sdk.AccAddressFromBech32(prover.Operator)
			stakeCoins := sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(prover.Stake)))
			if err := k.bankKeeper.SendCoinsFromModuleToAccount(
				sdkCtx, types.ModuleName, operatorAddr, stakeCoins,
			); err != nil {
				k.Logger(ctx).Error("failed to return stake", "error", err)
				continue
			}

			// Remove prover
			store.Delete(iterator.Key())

			k.Logger(ctx).Info("prover unbonding complete", "operator", prover.Operator)
		}
	}
}

// GetActiveProvers returns all active provers.
func (k Keeper) GetActiveProvers(ctx context.Context) []*types.Prover {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)

	var provers []*types.Prover
	iterator := storetypes.KVStorePrefixIterator(store, types.ProverKeyPrefix)
	defer iterator.Close()

	for ; iterator.Valid(); iterator.Next() {
		var prover types.Prover
		k.cdc.MustUnmarshal(iterator.Value(), &prover)
		if prover.Status == types.ProverStatusActive {
			provers = append(provers, &prover)
		}
	}

	return provers
}

// IncrementProverStats increments proof count for a prover.
func (k Keeper) IncrementProverStats(ctx context.Context, operatorAddr sdk.AccAddress) {
	prover, found := k.GetProver(ctx, operatorAddr)
	if !found {
		return
	}
	prover.ProofsCount++
	k.SetProver(ctx, prover)
}
