// Gate keeper operations for private execution

package keeper

import (
	"context"
	"encoding/hex"
	"strconv"

	storetypes "cosmossdk.io/store/types"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/anubis"
	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// RequestPrivateExecution queues a private execution request.
func (k Keeper) RequestPrivateExecution(
	ctx context.Context,
	msg *types.MsgPrivateExecute,
) ([]byte, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Get encrypted call data
	ec := msg.EncryptedCall

	// 1. Verify contract supports private execution
	pc, found := k.GetPrivateContract(ctx, ec.ContractAddress)
	if !found {
		return nil, types.ErrContractNotFound
	}
	if !pc.Enabled {
		return nil, types.ErrPrivateNotEnabled
	}

	// 2. Verify sender is authorized
	senderAddr, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, err
	}
	if !k.IsCallerAuthorized(ctx, ec.ContractAddress, senderAddr.Bytes()) {
		return nil, types.ErrUnauthorizedCaller
	}

	// 3. Generate call ID if not already set
	var callID []byte
	if len(ec.CallID) == 0 {
		callID, err = k.GenerateCallID(ctx, senderAddr.Bytes(), ec.Nonce)
		if err != nil {
			return nil, err
		}
	} else {
		callID = ec.CallID
	}

	// 4. Lock gas payment
	gasCost := k.CalculatePrivateGasCost(ctx, ec.GasLimit)
	err = k.bankKeeper.SendCoinsFromAccountToModule(
		sdkCtx, senderAddr, types.ModuleName,
		sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(gasCost))),
	)
	if err != nil {
		return nil, err
	}

	// 5. Store encrypted call
	storedCall := types.EncryptedCall{
		CallID:            callID,
		ContractAddress:   ec.ContractAddress,
		Sender:            senderAddr.Bytes(),
		EncryptedFunction: ec.EncryptedFunction,
		EncryptedArgs:     ec.EncryptedArgs,
		KEMCiphertext:     ec.KEMCiphertext,
		GasLimit:          ec.GasLimit,
		Nonce:             ec.Nonce,
		Signature:         ec.Signature,
	}
	k.SetEncryptedCall(ctx, storedCall)

	// 6. Emit event for provers
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypePrivateExecutionRequest,
			sdk.NewAttribute(types.AttributeKeyRequestID, hex.EncodeToString(callID)),
			sdk.NewAttribute(types.AttributeKeyContract, hex.EncodeToString(ec.ContractAddress)),
			sdk.NewAttribute("gas_limit", strconv.FormatUint(ec.GasLimit, 10)),
		),
	)

	return callID, nil
}

// SubmitPrivateResult processes a prover's result submission.
func (k Keeper) SubmitPrivateResult(
	ctx context.Context,
	msg *types.MsgSubmitProof,
) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// 1. Get request
	request, found := k.GetPrivateRequest(ctx, msg.RequestID)
	if !found {
		return types.ErrRequestNotFound
	}
	if request.Status != types.StatusPending {
		return types.ErrRequestNotPending
	}
	if sdkCtx.BlockHeight() > request.Deadline {
		return types.ErrRequestExpired
	}

	// 2. Verify prover is registered and staked
	proverAddr, err := sdk.AccAddressFromBech32(msg.Prover)
	if err != nil {
		return err
	}
	prover, found := k.GetProver(ctx, proverAddr)
	if !found || prover.Status != types.ProverStatusActive {
		return types.ErrInvalidProver
	}

	// 3. Get contract code hash
	contractAddr, _ := sdk.AccAddressFromBech32(request.Contract)
	pc, _ := k.GetPrivateContract(ctx, contractAddr.Bytes())

	// 4. Verify STARK execution proof
	var codeHash, oldStateRoot, newStateRoot [32]byte
	copy(codeHash[:], pc.CodeHash)
	// Get old state root from current encrypted state
	encState, _ := k.GetEncryptedState(ctx, contractAddr.Bytes())
	if encState != nil {
		copy(oldStateRoot[:], encState.Commitment[:32])
	}
	copy(newStateRoot[:], msg.NewStateRoot)

	valid, err := anubis.VerifySTARKProof(
		codeHash,
		oldStateRoot,
		newStateRoot,
		request.PublicInputs,
		msg.PublicOutputs,
		msg.Proof.Proof,
	)
	if err != nil || !valid {
		// Slash prover for invalid proof
		k.SlashProver(ctx, proverAddr, types.SlashInvalidProof)
		return types.ErrInvalidExecutionProof
	}

	// 5. Verify prover signature
	resultHash, _ := pqcrypto.SHA3_256(msg.Proof.Proof)
	var proverPK pqcrypto.PublicKey
	copy(proverPK[:], prover.PK)
	if !pqcrypto.Verify(proverPK, resultHash[:], msg.Signature) {
		return types.ErrInvalidSignature
	}

	// 6. Update contract state
	newEncState := &types.EncryptedState{
		Commitment:    msg.NewStateRoot,
		Ciphertext:    msg.EncryptedState,
		KEMCiphertext: msg.KEMCiphertext,
	}
	k.SetEncryptedState(ctx, contractAddr.Bytes(), newEncState)

	// 7. Store encrypted outputs for caller to retrieve
	k.SetEncryptedResult(ctx, msg.RequestID, msg.EncryptedOutputs)

	// 8. Update request status
	request.Status = types.StatusCompleted
	request.CompletedAt = sdkCtx.BlockHeight()
	request.Prover = msg.Prover
	k.SetPrivateRequest(ctx, request)

	// 9. Pay prover from locked gas
	proverReward := k.CalculateProverReward(request.LockedGas, prover.Commission)
	err = k.bankKeeper.SendCoinsFromModuleToAccount(
		sdkCtx, types.ModuleName, proverAddr,
		sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(proverReward))),
	)
	if err != nil {
		return err
	}

	// 10. Emit completion event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypePrivateExecutionComplete,
			sdk.NewAttribute(types.AttributeKeyRequestID, hex.EncodeToString(msg.RequestID)),
			sdk.NewAttribute(types.AttributeKeyProver, msg.Prover),
			sdk.NewAttribute(types.AttributeKeyGasUsed, strconv.FormatUint(msg.GasUsed, 10)),
		),
	)

	return nil
}

// GetPrivateRequest retrieves a private execution request.
func (k Keeper) GetPrivateRequest(ctx context.Context, requestID []byte) (*types.PrivateExecutionRequest, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetPrivateRequestKey(requestID)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}

	var request types.PrivateExecutionRequest
	k.cdc.MustUnmarshal(bz, &request)
	return &request, true
}

// SetPrivateRequest stores a private execution request.
func (k Keeper) SetPrivateRequest(ctx context.Context, request *types.PrivateExecutionRequest) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetPrivateRequestKey(request.ID)
	bz := k.cdc.MustMarshal(request)
	store.Set(key, bz)
}

// GetEncryptedResult retrieves encrypted result for a request.
func (k Keeper) GetEncryptedResult(ctx context.Context, requestID []byte) ([]byte, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetEncryptedResultKey(requestID)
	bz := store.Get(key)
	return bz, bz != nil
}

// SetEncryptedResult stores encrypted result for a request.
func (k Keeper) SetEncryptedResult(ctx context.Context, requestID []byte, result []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetEncryptedResultKey(requestID)
	store.Set(key, result)
}

// CalculatePrivateGasCost calculates the gas cost for private execution.
func (k Keeper) CalculatePrivateGasCost(ctx context.Context, gasLimit uint64) uint64 {
	// Base cost + per-gas cost
	// Private execution is more expensive due to proof verification
	baseCost := uint64(1000000) // 1 AEGIS base
	perGasCost := gasLimit * 2  // 2x multiplier for private execution
	return baseCost + perGasCost
}

// CalculateProverReward calculates the reward for a prover.
func (k Keeper) CalculateProverReward(lockedGas uint64, commission uint32) uint64 {
	// Prover gets locked gas minus commission to protocol
	protocolFee := lockedGas * uint64(commission) / 10000 // commission in basis points
	return lockedGas - protocolFee
}

// CleanupExpiredRequests cleans up expired private execution requests.
func (k Keeper) CleanupExpiredRequests(ctx context.Context) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	currentHeight := sdkCtx.BlockHeight()

	var expired [][]byte
	iterator := storetypes.KVStorePrefixIterator(store, types.PrivateRequestKeyPrefix)
	defer iterator.Close()

	for ; iterator.Valid(); iterator.Next() {
		var request types.PrivateExecutionRequest
		k.cdc.MustUnmarshal(iterator.Value(), &request)
		if request.Status == types.StatusPending && currentHeight > request.Deadline {
			expired = append(expired, request.ID)
		}
	}

	// Refund locked gas for expired requests
	for _, id := range expired {
		request, _ := k.GetPrivateRequest(ctx, id)
		if request != nil {
			senderAddr, _ := sdk.AccAddressFromBech32(request.Sender)
			_ = k.bankKeeper.SendCoinsFromModuleToAccount(
				sdkCtx, types.ModuleName, senderAddr,
				sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(request.LockedGas))),
			)
			request.Status = types.StatusExpired
			k.SetPrivateRequest(ctx, request)
		}
	}

	if len(expired) > 0 {
		k.Logger(ctx).Info("cleaned up expired requests", "count", len(expired))
	}
}
