// Eye keeper operations for selective disclosure

package keeper

import (
	"context"
	"encoding/hex"

	storetypes "cosmossdk.io/store/types"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/anubis"
	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// ProcessCreateDisclosure processes a disclosure creation request.
func (k Keeper) ProcessCreateDisclosure(
	ctx context.Context,
	msg *types.MsgCreateDisclosure,
) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// 1. Verify the disclosure proof
	var ownerPK [anubis.MLDSAPublicKeySize]byte
	if len(msg.OwnerPK) != anubis.MLDSAPublicKeySize {
		return types.ErrInvalidPublicKey
	}
	copy(ownerPK[:], msg.OwnerPK)

	valid, err := anubis.VerifyDisclosure(
		ownerPK,
		msg.AttributeMask,
		msg.Attributes,
		msg.Proof,
	)
	if err != nil || !valid {
		return types.ErrInvalidDisclosureProof
	}

	// 2. Store the disclosure
	disclosureID := k.generateDisclosureID(ctx, msg.Owner, msg.RecipientPKHash)
	disclosure := &types.Disclosure{
		ID:              disclosureID,
		Owner:           msg.Owner,
		RecipientPKHash: msg.RecipientPKHash,
		AttributeMask:   msg.AttributeMask,
		Attributes:      msg.Attributes,
		ValidUntil:      msg.ValidUntil,
		CreatedAt:       sdkCtx.BlockHeight(),
	}
	k.SetDisclosure(ctx, disclosure)

	// 3. Emit event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeDisclosureCreated,
			sdk.NewAttribute(types.AttributeKeyDisclosureID, hex.EncodeToString(disclosureID)),
			sdk.NewAttribute(types.AttributeKeyOwner, msg.Owner),
		),
	)

	return nil
}

// VerifyDisclosure verifies a selective disclosure.
func (k Keeper) VerifyDisclosure(
	ctx context.Context,
	ownerPK []byte,
	attributeMask uint64,
	attributes []uint64,
	proof []byte,
) (bool, error) {
	if len(ownerPK) != anubis.MLDSAPublicKeySize {
		return false, types.ErrInvalidPublicKey
	}

	var pk [anubis.MLDSAPublicKeySize]byte
	copy(pk[:], ownerPK)

	return anubis.VerifyDisclosure(pk, attributeMask, attributes, proof)
}

// GetDisclosure retrieves a disclosure by ID.
func (k Keeper) GetDisclosure(ctx context.Context, disclosureID []byte) (*types.Disclosure, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetDisclosureKey(disclosureID)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}

	var disclosure types.Disclosure
	k.cdc.MustUnmarshal(bz, &disclosure)
	return &disclosure, true
}

// SetDisclosure stores a disclosure.
func (k Keeper) SetDisclosure(ctx context.Context, disclosure *types.Disclosure) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetDisclosureKey(disclosure.ID)
	bz := k.cdc.MustMarshal(disclosure)
	store.Set(key, bz)
}

// DeleteDisclosure removes a disclosure.
func (k Keeper) DeleteDisclosure(ctx context.Context, disclosureID []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetDisclosureKey(disclosureID)
	store.Delete(key)
}

// IsDisclosureValid checks if a disclosure is still valid.
func (k Keeper) IsDisclosureValid(ctx context.Context, disclosure *types.Disclosure) bool {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Check expiration
	if disclosure.ValidUntil > 0 && sdkCtx.BlockHeight() > disclosure.ValidUntil {
		return false
	}

	return true
}

// generateDisclosureID generates a unique disclosure ID.
func (k Keeper) generateDisclosureID(ctx context.Context, owner string, recipientPKHash []byte) []byte {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	input := make([]byte, 0, 100)
	input = append(input, []byte(owner)...)
	input = append(input, recipientPKHash...)
	input = append(input, uint64ToBytes(uint64(sdkCtx.BlockHeight()))...)

	hash, _ := pqcrypto.SHA3_256(input)
	return hash[:]
}

// GetDisclosuresForOwner returns all disclosures created by an owner.
func (k Keeper) GetDisclosuresForOwner(ctx context.Context, owner string) []*types.Disclosure {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)

	var disclosures []*types.Disclosure
	iterator := storetypes.KVStorePrefixIterator(store, types.DisclosureKeyPrefix)
	defer iterator.Close()

	for ; iterator.Valid(); iterator.Next() {
		var disclosure types.Disclosure
		k.cdc.MustUnmarshal(iterator.Value(), &disclosure)
		if disclosure.Owner == owner {
			disclosures = append(disclosures, &disclosure)
		}
	}

	return disclosures
}

// CleanupExpiredDisclosures removes expired disclosures.
func (k Keeper) CleanupExpiredDisclosures(ctx context.Context) int {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	currentHeight := sdkCtx.BlockHeight()

	var expired [][]byte
	iterator := storetypes.KVStorePrefixIterator(store, types.DisclosureKeyPrefix)
	defer iterator.Close()

	for ; iterator.Valid(); iterator.Next() {
		var disclosure types.Disclosure
		k.cdc.MustUnmarshal(iterator.Value(), &disclosure)
		if disclosure.ValidUntil > 0 && currentHeight > disclosure.ValidUntil {
			expired = append(expired, disclosure.ID)
		}
	}

	for _, id := range expired {
		k.DeleteDisclosure(ctx, id)
	}

	if len(expired) > 0 {
		k.Logger(ctx).Info("cleaned up expired disclosures", "count", len(expired))
	}

	return len(expired)
}
