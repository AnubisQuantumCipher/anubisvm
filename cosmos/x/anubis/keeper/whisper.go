// Whisper keeper operations for confidential transactions

package keeper

import (
	"context"
	"encoding/hex"
	"strconv"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/anubis"
	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// ProcessConfidentialTransfer processes a confidential transfer transaction.
// Returns the generated note IDs for the output notes.
func (k Keeper) ProcessConfidentialTransfer(
	ctx context.Context,
	msg *types.MsgConfidentialTransfer,
) ([][]byte, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// 1. Verify no double-spend (check nullifiers)
	for _, nullifier := range msg.InputNullifiers {
		if k.HasNullifier(ctx, nullifier) {
			return nil, types.ErrDoubleSpend
		}
	}

	// 2. Verify input ownership proofs
	for i, proof := range msg.InputOwnershipProofs {
		if !k.VerifyOwnershipProof(ctx, msg.InputNullifiers[i], proof) {
			return nil, types.ErrInvalidOwnershipProof
		}
	}

	// 3. Verify output range proofs
	for i, note := range msg.OutputNotes {
		var commitment [64]byte
		copy(commitment[:], note.Commitment)

		valid, err := anubis.VerifyRangeProof(commitment, msg.OutputRangeProofs[i])
		if err != nil || !valid {
			return nil, types.ErrInvalidRangeProof
		}
	}

	// 4. Verify balance proof
	inputCommitments := make([][64]byte, len(msg.InputNullifiers))
	for i, nullifier := range msg.InputNullifiers {
		comm := k.GetCommitmentFromNullifier(ctx, nullifier)
		if comm == nil {
			return nil, types.ErrNullifierNotFound
		}
		copy(inputCommitments[i][:], comm)
	}

	outputCommitments := make([][64]byte, len(msg.OutputNotes))
	for i, note := range msg.OutputNotes {
		copy(outputCommitments[i][:], note.Commitment)
	}

	valid, err := anubis.VerifyBalanceProof(
		inputCommitments,
		outputCommitments,
		msg.Fee,
		msg.BalanceProof,
	)
	if err != nil || !valid {
		return nil, types.ErrInvalidBalanceProof
	}

	// 5. Mark nullifiers as spent
	for _, nullifier := range msg.InputNullifiers {
		k.SetNullifier(ctx, nullifier, sdkCtx.BlockHeight())
	}

	// 6. Store output notes and collect note IDs
	noteIDs := make([][]byte, len(msg.OutputNotes))
	for i, note := range msg.OutputNotes {
		noteID := k.generateNoteID(ctx, sdkCtx.BlockHeight(), i)
		noteIDs[i] = noteID
		k.SetNote(ctx, noteID, note)
	}

	// 7. Charge fee from sender's public balance
	senderAddr, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, err
	}
	feeCoins := sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(msg.Fee)))
	if err := k.bankKeeper.SendCoinsFromAccountToModule(
		sdkCtx, senderAddr, types.ModuleName, feeCoins,
	); err != nil {
		return nil, err
	}

	// 8. Burn fee
	if err := k.bankKeeper.BurnCoins(sdkCtx, types.ModuleName, feeCoins); err != nil {
		return nil, err
	}

	// 9. Emit events
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeConfidentialTransfer,
			sdk.NewAttribute(types.AttributeKeyNullifierCount, strconv.Itoa(len(msg.InputNullifiers))),
			sdk.NewAttribute(types.AttributeKeyNoteCount, strconv.Itoa(len(msg.OutputNotes))),
			sdk.NewAttribute(types.AttributeKeyFee, strconv.FormatUint(msg.Fee, 10)),
		),
	)

	return noteIDs, nil
}

// VerifyOwnershipProof verifies that the sender owns the note being spent.
func (k Keeper) VerifyOwnershipProof(ctx context.Context, nullifier []byte, proof []byte) bool {
	// The ownership proof is a lattice ZK proof that the sender knows
	// the secret key corresponding to the note's owner public key hash
	valid, err := anubis.VerifyLatticeProof(nullifier, proof)
	if err != nil {
		k.Logger(ctx).Error("ownership proof verification failed", "error", err)
		return false
	}
	return valid
}

// GetCommitmentFromNullifier retrieves the original commitment for a nullifier.
// This requires maintaining a nullifier -> commitment mapping.
func (k Keeper) GetCommitmentFromNullifier(ctx context.Context, nullifier []byte) []byte {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNullifierCommitmentKey(nullifier)
	return store.Get(key)
}

// SetNullifierCommitment stores the commitment associated with a nullifier.
func (k Keeper) SetNullifierCommitment(ctx context.Context, nullifier []byte, commitment []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNullifierCommitmentKey(nullifier)
	store.Set(key, commitment)
}

// HasNullifier checks if a nullifier has been spent.
func (k Keeper) HasNullifier(ctx context.Context, nullifier []byte) bool {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNullifierKey(nullifier)
	return store.Has(key)
}

// SetNullifier marks a nullifier as spent.
func (k Keeper) SetNullifier(ctx context.Context, nullifier []byte, blockHeight int64) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNullifierKey(nullifier)
	// Store block height when nullifier was spent
	bz := make([]byte, 8)
	for i := 0; i < 8; i++ {
		bz[i] = byte(blockHeight >> (56 - 8*i))
	}
	store.Set(key, bz)

	// Emit nullifier event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeNullifierSpent,
			sdk.NewAttribute(types.AttributeKeyNullifier, hex.EncodeToString(nullifier)),
			sdk.NewAttribute(types.AttributeKeyBlockHeight, strconv.FormatInt(blockHeight, 10)),
		),
	)
}

// GetNote retrieves a note by ID.
func (k Keeper) GetNote(ctx context.Context, noteID []byte) (*types.Note, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNoteKey(noteID)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}

	var note types.Note
	k.cdc.MustUnmarshal(bz, &note)
	return &note, true
}

// SetNote stores a note.
func (k Keeper) SetNote(ctx context.Context, noteID []byte, note *types.Note) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNoteKey(noteID)
	bz := k.cdc.MustMarshal(note)
	store.Set(key, bz)
}

// generateNoteID generates a unique note ID.
func (k Keeper) generateNoteID(ctx context.Context, blockHeight int64, index int) []byte {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	// Combine block height, index, and block hash for uniqueness
	input := make([]byte, 0, 48)
	input = append(input, uint64ToBytes(uint64(blockHeight))...)
	input = append(input, uint64ToBytes(uint64(index))...)
	input = append(input, sdkCtx.HeaderHash()...)

	hash, _ := pqcrypto.SHA3_256(input)
	return hash[:]
}

// ShieldToWhisper converts public tokens to confidential notes.
func (k Keeper) ShieldToWhisper(
	ctx context.Context,
	sender sdk.AccAddress,
	amount uint64,
	recipientPK []byte,
	blinding [32]byte,
) (*types.Note, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// 1. Burn public tokens
	coins := sdk.NewCoins(sdk.NewInt64Coin("uaegis", int64(amount)))
	if err := k.bankKeeper.SendCoinsFromAccountToModule(
		sdkCtx, sender, types.ModuleName, coins,
	); err != nil {
		return nil, err
	}
	if err := k.bankKeeper.BurnCoins(sdkCtx, types.ModuleName, coins); err != nil {
		return nil, err
	}

	// 2. Create confidential note
	var ownerPK [anubis.MLKEMPublicKeySize]byte
	if len(recipientPK) != anubis.MLKEMPublicKeySize {
		return nil, types.ErrInvalidPublicKey
	}
	copy(ownerPK[:], recipientPK)

	note, err := anubis.CreateNote(amount, blinding, ownerPK)
	if err != nil {
		return nil, err
	}

	// 3. Store note
	noteID := k.generateNoteID(ctx, sdkCtx.BlockHeight(), 0)
	typesNote := &types.Note{
		Commitment:    note.Commitment[:],
		EncryptedData: note.EncryptedData[:],
		OwnerPKHash:   note.OwnerPKHash[:],
	}
	k.SetNote(ctx, noteID, typesNote)

	// 4. Emit event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeShieldDeposit,
			sdk.NewAttribute(types.AttributeKeySender, sender.String()),
			sdk.NewAttribute(types.AttributeKeyNoteID, hex.EncodeToString(noteID)),
		),
	)

	return typesNote, nil
}

// ProcessShieldDeposit processes a MsgShieldDeposit message.
// Returns the note ID for the newly created confidential note.
func (k Keeper) ProcessShieldDeposit(
	ctx context.Context,
	msg *types.MsgShieldDeposit,
) ([]byte, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Validate sender
	senderAddr, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, err
	}

	// Convert blinding factor to fixed size
	var blinding [32]byte
	if len(msg.Blinding) != 32 {
		return nil, types.ErrInvalidNote
	}
	copy(blinding[:], msg.Blinding)

	// Burn public tokens
	denom := msg.Denom
	if denom == "" {
		denom = "uaegis"
	}
	coins := sdk.NewCoins(sdk.NewInt64Coin(denom, int64(msg.Amount)))
	if err := k.bankKeeper.SendCoinsFromAccountToModule(
		sdkCtx, senderAddr, types.ModuleName, coins,
	); err != nil {
		return nil, err
	}
	if err := k.bankKeeper.BurnCoins(sdkCtx, types.ModuleName, coins); err != nil {
		return nil, err
	}

	// Create confidential note
	var ownerPK [anubis.MLKEMPublicKeySize]byte
	if len(msg.RecipientPK) != anubis.MLKEMPublicKeySize {
		return nil, types.ErrInvalidPublicKey
	}
	copy(ownerPK[:], msg.RecipientPK)

	note, err := anubis.CreateNote(msg.Amount, blinding, ownerPK)
	if err != nil {
		return nil, err
	}

	// Generate note ID
	noteID := k.generateNoteID(ctx, sdkCtx.BlockHeight(), 0)

	// Store note
	typesNote := &types.Note{
		Commitment:    note.Commitment[:],
		EncryptedData: note.EncryptedData[:],
		OwnerPKHash:   note.OwnerPKHash[:],
	}
	k.SetNote(ctx, noteID, typesNote)

	// Emit event
	sdkCtx.EventManager().EmitEvent(
		sdk.NewEvent(
			types.EventTypeShieldDeposit,
			sdk.NewAttribute(types.AttributeKeySender, msg.Sender),
			sdk.NewAttribute(types.AttributeKeyNoteID, hex.EncodeToString(noteID)),
		),
	)

	return noteID, nil
}
