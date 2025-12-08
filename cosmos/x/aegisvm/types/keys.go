// Module keys and store prefixes for x/aegisvm

package types

const (
	// ModuleName is the name of the aegisvm module
	ModuleName = "aegisvm"

	// StoreKey is the store key string for aegisvm
	StoreKey = ModuleName

	// RouterKey is the message route for aegisvm
	RouterKey = ModuleName

	// QuerierRoute is the querier route for aegisvm
	QuerierRoute = ModuleName

	// MemStoreKey defines the in-memory store key
	MemStoreKey = "mem_aegisvm"
)

// Store key prefixes
var (
	// ContractKeyPrefix is the prefix for contract storage
	ContractKeyPrefix = []byte{0x01}

	// CodeKeyPrefix is the prefix for bytecode storage
	CodeKeyPrefix = []byte{0x02}

	// StateKeyPrefix is the prefix for contract state storage
	StateKeyPrefix = []byte{0x03}

	// ApprovedCodeKeyPrefix is the prefix for approved code hashes
	ApprovedCodeKeyPrefix = []byte{0x04}

	// NonceKeyPrefix is the prefix for account nonces
	NonceKeyPrefix = []byte{0x05}

	// ParamsKey is the key for module parameters
	ParamsKey = []byte{0x06}
)

// GetContractKey returns the store key for a contract
func GetContractKey(addr []byte) []byte {
	return append(ContractKeyPrefix, addr...)
}

// GetCodeKey returns the store key for bytecode
func GetCodeKey(codeHash []byte) []byte {
	return append(CodeKeyPrefix, codeHash...)
}

// GetStateKey returns the store key for contract state
func GetStateKey(addr []byte) []byte {
	return append(StateKeyPrefix, addr...)
}

// GetApprovedCodeKey returns the store key for an approved code hash
func GetApprovedCodeKey(codeHash []byte) []byte {
	return append(ApprovedCodeKeyPrefix, codeHash...)
}

// GetNonceKey returns the store key for an account nonce
func GetNonceKey(addr []byte) []byte {
	return append(NonceKeyPrefix, addr...)
}
