// Keeper for x/anubis module

package keeper

import (
	"context"
	"fmt"

	"cosmossdk.io/log"
	storetypes "cosmossdk.io/store/types"

	"github.com/cosmos/cosmos-sdk/codec"
	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// Keeper maintains the state and provides methods for the anubis module
type Keeper struct {
	cdc           codec.BinaryCodec
	storeKey      storetypes.StoreKey
	memKey        storetypes.StoreKey
	aegisvmKeeper types.AegisVMKeeper
	bankKeeper    types.BankKeeper
	authority     string
}

// NewKeeper creates a new anubis Keeper
func NewKeeper(
	cdc codec.BinaryCodec,
	storeKey, memKey storetypes.StoreKey,
	aegisvmKeeper types.AegisVMKeeper,
	bankKeeper types.BankKeeper,
	authority string,
) Keeper {
	return Keeper{
		cdc:           cdc,
		storeKey:      storeKey,
		memKey:        memKey,
		aegisvmKeeper: aegisvmKeeper,
		bankKeeper:    bankKeeper,
		authority:     authority,
	}
}

// GetAuthority returns the module authority address
func (k Keeper) GetAuthority() string {
	return k.authority
}

// Logger returns a module-specific logger
func (k Keeper) Logger(ctx context.Context) log.Logger {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	return sdkCtx.Logger().With("module", fmt.Sprintf("x/%s", types.ModuleName))
}

// GetParams returns the module parameters
func (k Keeper) GetParams(ctx context.Context) (types.Params, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	bz := store.Get(types.ParamsKey)
	if bz == nil {
		return types.DefaultParams(), nil
	}

	var params types.Params
	if err := k.cdc.Unmarshal(bz, &params); err != nil {
		return types.Params{}, err
	}
	return params, nil
}

// SetParams sets the module parameters
func (k Keeper) SetParams(ctx context.Context, params types.Params) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	bz, err := k.cdc.Marshal(&params)
	if err != nil {
		return err
	}
	store.Set(types.ParamsKey, bz)
	return nil
}

// GetPrivateContract returns a private contract configuration
func (k Keeper) GetPrivateContract(ctx context.Context, addr []byte) (types.PrivateContract, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetPrivateContractKey(addr)

	bz := store.Get(key)
	if bz == nil {
		return types.PrivateContract{}, false
	}

	var pc types.PrivateContract
	k.cdc.MustUnmarshal(bz, &pc)
	return pc, true
}

// SetPrivateContract stores a private contract configuration
func (k Keeper) SetPrivateContract(ctx context.Context, pc types.PrivateContract) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetPrivateContractKey(pc.Address)
	bz := k.cdc.MustMarshal(&pc)
	store.Set(key, bz)
}

// GetEncryptedCall returns an encrypted call by ID
func (k Keeper) GetEncryptedCall(ctx context.Context, callID []byte) (types.EncryptedCall, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetEncryptedCallKey(callID)

	bz := store.Get(key)
	if bz == nil {
		return types.EncryptedCall{}, false
	}

	var ec types.EncryptedCall
	k.cdc.MustUnmarshal(bz, &ec)
	return ec, true
}

// SetEncryptedCall stores an encrypted call
func (k Keeper) SetEncryptedCall(ctx context.Context, ec types.EncryptedCall) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetEncryptedCallKey(ec.CallID)
	bz := k.cdc.MustMarshal(&ec)
	store.Set(key, bz)
}

// DeleteEncryptedCall removes an encrypted call
func (k Keeper) DeleteEncryptedCall(ctx context.Context, callID []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetEncryptedCallKey(callID)
	store.Delete(key)
}

// GetSession returns a session by ID
func (k Keeper) GetSession(ctx context.Context, sessionID []byte) (types.SessionKey, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetSessionKey(sessionID)

	bz := store.Get(key)
	if bz == nil {
		return types.SessionKey{}, false
	}

	var sk types.SessionKey
	k.cdc.MustUnmarshal(bz, &sk)
	return sk, true
}

// SetSession stores a session
func (k Keeper) SetSession(ctx context.Context, sk types.SessionKey) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetSessionKey(sk.SessionID)
	bz := k.cdc.MustMarshal(&sk)
	store.Set(key, bz)
}

// DeleteSession removes a session
func (k Keeper) DeleteSession(ctx context.Context, sessionID []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetSessionKey(sessionID)
	store.Delete(key)
}

// GetZKProof returns a ZK proof by hash
func (k Keeper) GetZKProof(ctx context.Context, proofHash []byte) (types.ZKStateProof, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetZKProofKey(proofHash)

	bz := store.Get(key)
	if bz == nil {
		return types.ZKStateProof{}, false
	}

	var proof types.ZKStateProof
	k.cdc.MustUnmarshal(bz, &proof)
	return proof, true
}

// SetZKProof stores a ZK proof
func (k Keeper) SetZKProof(ctx context.Context, proof types.ZKStateProof) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetZKProofKey(proof.ProofHash)
	bz := k.cdc.MustMarshal(&proof)
	store.Set(key, bz)
}

// GenerateCallID generates a unique call ID
func (k Keeper) GenerateCallID(ctx context.Context, sender []byte, nonce []byte) ([]byte, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	// Combine sender, block height, and nonce
	input := append(sender, uint64ToBytes(uint64(sdkCtx.BlockHeight()))...)
	input = append(input, nonce...)

	hash, err := pqcrypto.SHA3_256(input)
	if err != nil {
		return nil, err
	}

	return hash[:], nil
}

// GenerateSessionID generates a unique session ID
func (k Keeper) GenerateSessionID(ctx context.Context, initiator []byte, contract []byte) ([]byte, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	// Combine initiator, contract, and block height
	input := append(initiator, contract...)
	input = append(input, uint64ToBytes(uint64(sdkCtx.BlockHeight()))...)

	hash, err := pqcrypto.SHA3_256(input)
	if err != nil {
		return nil, err
	}

	return hash[:], nil
}

// IsCallerAuthorized checks if a caller is authorized for private calls
func (k Keeper) IsCallerAuthorized(ctx context.Context, contractAddr, caller []byte) bool {
	pc, found := k.GetPrivateContract(ctx, contractAddr)
	if !found || !pc.Enabled {
		return false
	}

	// If no authorized callers specified, anyone can call
	if len(pc.AuthorizedCallers) == 0 {
		return true
	}

	// Check if caller is in the authorized list
	for _, auth := range pc.AuthorizedCallers {
		if bytesEqual(auth, caller) {
			return true
		}
	}

	return false
}

// VerifyZKProof verifies a ZK proof
func (k Keeper) VerifyZKProof(ctx context.Context, proof types.ZKStateProof) error {
	// TODO: Implement actual ZK proof verification
	// This would call into the appropriate verifier based on proof.ProofSystem

	switch proof.ProofSystem {
	case types.ProofSystemPlonk:
		// Verify Plonk proof
		return nil
	case types.ProofSystemGroth16:
		// Verify Groth16 proof
		return nil
	case types.ProofSystemSTARK:
		// Verify STARK proof
		return nil
	default:
		return types.ErrZKProofInvalid
	}
}

// CleanupExpiredSessions removes expired sessions
func (k Keeper) CleanupExpiredSessions(ctx context.Context) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	iterator := storetypes.KVStorePrefixIterator(store, types.SessionKeyPrefix)
	defer iterator.Close()

	currentHeight := sdkCtx.BlockHeight()
	var expiredSessions [][]byte

	for ; iterator.Valid(); iterator.Next() {
		var sk types.SessionKey
		k.cdc.MustUnmarshal(iterator.Value(), &sk)
		if sk.ExpiresAt <= currentHeight {
			expiredSessions = append(expiredSessions, sk.SessionID)
		}
	}

	// Delete expired sessions
	for _, sessionID := range expiredSessions {
		k.DeleteSession(ctx, sessionID)
	}

	if len(expiredSessions) > 0 {
		k.Logger(ctx).Info("cleaned up expired sessions", "count", len(expiredSessions))
	}
}

// Helper functions
func uint64ToBytes(n uint64) []byte {
	bz := make([]byte, 8)
	for i := 0; i < 8; i++ {
		bz[i] = byte(n >> (56 - 8*i))
	}
	return bz
}

func bytesEqual(a, b []byte) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
