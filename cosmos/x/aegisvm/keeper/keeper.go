// Keeper for x/aegisvm module

package keeper

import (
	"context"
	"fmt"

	"cosmossdk.io/log"
	storetypes "cosmossdk.io/store/types"

	"github.com/cosmos/cosmos-sdk/codec"
	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/x/aegisvm/types"
)

// Keeper maintains the state and provides methods for the aegisvm module
type Keeper struct {
	cdc        codec.BinaryCodec
	storeKey   storetypes.StoreKey
	memKey     storetypes.StoreKey
	bankKeeper types.BankKeeper
	authority  string
}

// NewKeeper creates a new aegisvm Keeper
func NewKeeper(
	cdc codec.BinaryCodec,
	storeKey, memKey storetypes.StoreKey,
	bankKeeper types.BankKeeper,
	authority string,
) Keeper {
	return Keeper{
		cdc:        cdc,
		storeKey:   storeKey,
		memKey:     memKey,
		bankKeeper: bankKeeper,
		authority:  authority,
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

// GetContract returns a contract by address
func (k Keeper) GetContract(ctx context.Context, addr []byte) (types.Contract, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetContractKey(addr)

	bz := store.Get(key)
	if bz == nil {
		return types.Contract{}, false
	}

	var contract types.Contract
	k.cdc.MustUnmarshal(bz, &contract)
	return contract, true
}

// SetContract stores a contract
func (k Keeper) SetContract(ctx context.Context, contract types.Contract) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetContractKey(contract.Address)
	bz := k.cdc.MustMarshal(&contract)
	store.Set(key, bz)
}

// DeleteContract removes a contract
func (k Keeper) DeleteContract(ctx context.Context, addr []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetContractKey(addr)
	store.Delete(key)
}

// GetBytecode returns bytecode by hash
func (k Keeper) GetBytecode(ctx context.Context, codeHash []byte) ([]byte, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetCodeKey(codeHash)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}
	return bz, true
}

// SetBytecode stores bytecode
func (k Keeper) SetBytecode(ctx context.Context, codeHash, bytecode []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetCodeKey(codeHash)
	store.Set(key, bytecode)
}

// GetContractState returns contract state
func (k Keeper) GetContractState(ctx context.Context, addr []byte) ([]byte, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetStateKey(addr)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}
	return bz, true
}

// SetContractState stores contract state
func (k Keeper) SetContractState(ctx context.Context, addr, state []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetStateKey(addr)
	store.Set(key, state)
}

// IsCodeApproved checks if a code hash is approved by governance
func (k Keeper) IsCodeApproved(ctx context.Context, codeHash []byte) bool {
	params, err := k.GetParams(ctx)
	if err != nil || !params.RequireGovernance {
		return true
	}

	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetApprovedCodeKey(codeHash)
	return store.Has(key)
}

// ApproveCode marks a code hash as approved
func (k Keeper) ApproveCode(ctx context.Context, codeHash []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetApprovedCodeKey(codeHash)
	store.Set(key, []byte{1})
}

// RevokeCodeApproval removes code approval
func (k Keeper) RevokeCodeApproval(ctx context.Context, codeHash []byte) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetApprovedCodeKey(codeHash)
	store.Delete(key)
}

// DeriveContractAddress derives a contract address from deployer and nonce
func (k Keeper) DeriveContractAddress(ctx context.Context, deployer sdk.AccAddress) ([]byte, error) {
	// Get and increment nonce
	nonce := k.GetNonce(ctx, deployer)
	k.SetNonce(ctx, deployer, nonce+1)

	// Create input: deployer || nonce
	input := append(deployer.Bytes(), uint64ToBytes(nonce)...)

	// Hash to derive address
	hash, err := pqcrypto.SHA3_256(input)
	if err != nil {
		return nil, err
	}

	// Take first 20 bytes as address
	return hash[:20], nil
}

// GetNonce returns the nonce for an address
func (k Keeper) GetNonce(ctx context.Context, addr sdk.AccAddress) uint64 {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNonceKey(addr.Bytes())

	bz := store.Get(key)
	if bz == nil {
		return 0
	}

	return bytesToUint64(bz)
}

// SetNonce sets the nonce for an address
func (k Keeper) SetNonce(ctx context.Context, addr sdk.AccAddress, nonce uint64) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetNonceKey(addr.Bytes())
	store.Set(key, uint64ToBytes(nonce))
}

// IterateContracts iterates over all contracts
func (k Keeper) IterateContracts(ctx context.Context, cb func(contract types.Contract) bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	iterator := storetypes.KVStorePrefixIterator(store, types.ContractKeyPrefix)
	defer iterator.Close()

	for ; iterator.Valid(); iterator.Next() {
		var contract types.Contract
		k.cdc.MustUnmarshal(iterator.Value(), &contract)
		if cb(contract) {
			break
		}
	}
}

// Helper functions for uint64 encoding
func uint64ToBytes(n uint64) []byte {
	bz := make([]byte, 8)
	for i := 0; i < 8; i++ {
		bz[i] = byte(n >> (56 - 8*i))
	}
	return bz
}

func bytesToUint64(bz []byte) uint64 {
	var n uint64
	for i := 0; i < 8 && i < len(bz); i++ {
		n |= uint64(bz[i]) << (56 - 8*i)
	}
	return n
}
