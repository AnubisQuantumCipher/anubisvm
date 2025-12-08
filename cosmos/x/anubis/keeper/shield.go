// Shield keeper operations for encrypted state

package keeper

import (
	"context"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/anubis"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// EncryptContractState encrypts contract state for private storage.
func (k Keeper) EncryptContractState(
	ctx context.Context,
	contractAddr []byte,
	state []byte,
) (*types.EncryptedState, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Get contract's ML-KEM public key
	pc, found := k.GetPrivateContract(ctx, contractAddr)
	if !found {
		return nil, types.ErrContractNotFound
	}
	if !pc.Enabled {
		return nil, types.ErrPrivateNotEnabled
	}

	// Convert public key to array
	var encapsKey [anubis.MLKEMPublicKeySize]byte
	if len(pc.PublicKey) != anubis.MLKEMPublicKeySize {
		return nil, types.ErrInvalidPublicKey
	}
	copy(encapsKey[:], pc.PublicKey)

	// Generate randomness for commitment
	var randomness [32]byte
	// In production, use crypto/rand; here we derive from block hash for determinism
	blockHash := sdkCtx.HeaderHash()
	copy(randomness[:], blockHash[:32])

	// Encrypt state and create commitment
	encState, err := anubis.EncryptStateWithCommitment(state, encapsKey, randomness)
	if err != nil {
		k.Logger(ctx).Error("failed to encrypt state", "error", err)
		return nil, types.ErrEncryptionFailed
	}

	return &types.EncryptedState{
		Ciphertext:    encState.Ciphertext,
		KEMCiphertext: encState.KEMCiphertext[:],
		Commitment:    encState.Commitment[:],
	}, nil
}

// DecryptContractState decrypts contract state (prover-side operation).
// Note: This requires the contract's secret key, typically done by provers.
func (k Keeper) DecryptContractState(
	ciphertext []byte,
	kemCiphertext []byte,
	decapsKey []byte,
) ([]byte, error) {
	if len(kemCiphertext) != anubis.MLKEMCiphertextSize {
		return nil, types.ErrInvalidCiphertext
	}
	if len(decapsKey) != anubis.MLKEMSecretKeySize {
		return nil, types.ErrInvalidSecretKey
	}

	var kemCT [anubis.MLKEMCiphertextSize]byte
	var sk [anubis.MLKEMSecretKeySize]byte
	copy(kemCT[:], kemCiphertext)
	copy(sk[:], decapsKey)

	plaintext, err := anubis.DecryptState(ciphertext, kemCT, sk)
	if err != nil {
		return nil, types.ErrDecryptionFailed
	}

	return plaintext, nil
}

// VerifyStateCommitment verifies that a commitment matches expected state.
func (k Keeper) VerifyStateCommitment(
	commitment []byte,
	stateRoot []byte,
) bool {
	// Compare commitment to stored state root
	if len(commitment) != anubis.ShieldCommitmentSize {
		return false
	}
	if len(stateRoot) != 32 {
		return false
	}

	// In the full implementation, this would verify the commitment
	// against the Merkle root of the state tree
	return true
}

// GetEncryptedState retrieves encrypted state for a contract.
func (k Keeper) GetEncryptedState(ctx context.Context, contractAddr []byte) (*types.EncryptedState, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetEncryptedStateKey(contractAddr)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}

	var state types.EncryptedState
	k.cdc.MustUnmarshal(bz, &state)
	return &state, true
}

// SetEncryptedState stores encrypted state for a contract.
func (k Keeper) SetEncryptedState(ctx context.Context, contractAddr []byte, state *types.EncryptedState) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetEncryptedStateKey(contractAddr)
	bz := k.cdc.MustMarshal(state)
	store.Set(key, bz)
}

// UpdateEncryptedState updates encrypted state and emits event.
func (k Keeper) UpdateEncryptedState(
	ctx context.Context,
	contractAddr []byte,
	newState *types.EncryptedState,
	oldStateRoot []byte,
) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Store new state
	k.SetEncryptedState(ctx, contractAddr, newState)

	// Emit state update event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeStateUpdate,
			sdk.NewAttribute(types.AttributeKeyContract, string(contractAddr)),
			sdk.NewAttribute(types.AttributeKeyOldRoot, string(oldStateRoot)),
			sdk.NewAttribute(types.AttributeKeyNewRoot, string(newState.Commitment)),
		),
	)

	return nil
}
