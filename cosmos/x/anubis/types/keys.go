// Module keys and store prefixes for x/anubis

package types

const (
	// ModuleName is the name of the anubis module
	ModuleName = "anubis"

	// StoreKey is the store key string for anubis
	StoreKey = ModuleName

	// RouterKey is the message route for anubis
	RouterKey = ModuleName

	// QuerierRoute is the querier route for anubis
	QuerierRoute = ModuleName

	// MemStoreKey defines the in-memory store key
	MemStoreKey = "mem_anubis"
)

// Store key prefixes
var (
	// PrivateStateKeyPrefix is the prefix for encrypted contract state
	PrivateStateKeyPrefix = []byte{0x01}

	// ZKProofKeyPrefix is the prefix for ZK proof storage
	ZKProofKeyPrefix = []byte{0x02}

	// EncryptedCallKeyPrefix is the prefix for encrypted call data
	EncryptedCallKeyPrefix = []byte{0x03}

	// KeyEncapsulationPrefix is the prefix for KEM ciphertexts
	KeyEncapsulationPrefix = []byte{0x04}

	// PrivateContractKeyPrefix is the prefix for private contract configs
	PrivateContractKeyPrefix = []byte{0x05}

	// SessionKeyPrefix is the prefix for encrypted session keys
	SessionKeyPrefix = []byte{0x06}

	// ParamsKey is the key for module parameters
	ParamsKey = []byte{0x07}

	// NullifierKeyPrefix is the prefix for spent nullifiers
	NullifierKeyPrefix = []byte{0x08}

	// NullifierCommitmentKeyPrefix maps nullifiers to their original commitments
	NullifierCommitmentKeyPrefix = []byte{0x09}

	// NoteKeyPrefix is the prefix for confidential notes
	NoteKeyPrefix = []byte{0x0A}

	// DisclosureKeyPrefix is the prefix for selective disclosures
	DisclosureKeyPrefix = []byte{0x0B}

	// PrivateRequestKeyPrefix is the prefix for private execution requests
	PrivateRequestKeyPrefix = []byte{0x0C}

	// EncryptedResultKeyPrefix is the prefix for encrypted execution results
	EncryptedResultKeyPrefix = []byte{0x0D}

	// EncryptedStateKeyPrefix is the prefix for encrypted state storage
	EncryptedStateKeyPrefix = []byte{0x0E}

	// ProverKeyPrefix is the prefix for prover registrations
	ProverKeyPrefix = []byte{0x0F}

	// ProofJobKeyPrefix is the prefix for HORUS proof jobs
	ProofJobKeyPrefix = []byte{0x10}

	// ThresholdKeySharePrefix is the prefix for SEBEK threshold key shares
	ThresholdKeySharePrefix = []byte{0x11}

	// BlockProofKeyPrefix is the prefix for MAAT block proofs
	BlockProofKeyPrefix = []byte{0x12}

	// EpochProofKeyPrefix is the prefix for MAAT epoch proofs
	EpochProofKeyPrefix = []byte{0x13}

	// CheckpointKeyPrefix is the prefix for TEFNUT light client checkpoints
	CheckpointKeyPrefix = []byte{0x14}

	// AADKGStateKeyPrefix is the prefix for AADKG distributed key generation state
	AADKGStateKeyPrefix = []byte{0x15}
)

// GetPrivateStateKey returns the store key for encrypted contract state
func GetPrivateStateKey(contractAddr []byte, stateKey []byte) []byte {
	key := append(PrivateStateKeyPrefix, contractAddr...)
	return append(key, stateKey...)
}

// GetZKProofKey returns the store key for a ZK proof
func GetZKProofKey(proofHash []byte) []byte {
	return append(ZKProofKeyPrefix, proofHash...)
}

// GetEncryptedCallKey returns the store key for encrypted call data
func GetEncryptedCallKey(callID []byte) []byte {
	return append(EncryptedCallKeyPrefix, callID...)
}

// GetKeyEncapsulationKey returns the store key for a KEM ciphertext
func GetKeyEncapsulationKey(txHash []byte) []byte {
	return append(KeyEncapsulationPrefix, txHash...)
}

// GetPrivateContractKey returns the store key for private contract config
func GetPrivateContractKey(contractAddr []byte) []byte {
	return append(PrivateContractKeyPrefix, contractAddr...)
}

// GetSessionKey returns the store key for an encrypted session key
func GetSessionKey(sessionID []byte) []byte {
	return append(SessionKeyPrefix, sessionID...)
}

// GetNullifierKey returns the store key for a nullifier
func GetNullifierKey(nullifier []byte) []byte {
	return append(NullifierKeyPrefix, nullifier...)
}

// GetNullifierCommitmentKey returns the store key for nullifier to commitment mapping
func GetNullifierCommitmentKey(nullifier []byte) []byte {
	return append(NullifierCommitmentKeyPrefix, nullifier...)
}

// GetNoteKey returns the store key for a note
func GetNoteKey(noteID []byte) []byte {
	return append(NoteKeyPrefix, noteID...)
}

// GetDisclosureKey returns the store key for a disclosure
func GetDisclosureKey(disclosureID []byte) []byte {
	return append(DisclosureKeyPrefix, disclosureID...)
}

// GetPrivateRequestKey returns the store key for a private execution request
func GetPrivateRequestKey(requestID []byte) []byte {
	return append(PrivateRequestKeyPrefix, requestID...)
}

// GetEncryptedResultKey returns the store key for encrypted execution result
func GetEncryptedResultKey(requestID []byte) []byte {
	return append(EncryptedResultKeyPrefix, requestID...)
}

// GetEncryptedStateKey returns the store key for encrypted contract state
func GetEncryptedStateKey(contractAddr []byte) []byte {
	return append(EncryptedStateKeyPrefix, contractAddr...)
}

// GetProverKey returns the store key for a prover
func GetProverKey(operatorAddr []byte) []byte {
	return append(ProverKeyPrefix, operatorAddr...)
}

// GetProofJobKey returns the store key for a HORUS proof job
func GetProofJobKey(jobID []byte) []byte {
	return append(ProofJobKeyPrefix, jobID...)
}

// GetThresholdKeyShareKey returns the store key for a SEBEK threshold key share
func GetThresholdKeyShareKey(partyIndex uint32) []byte {
	key := make([]byte, len(ThresholdKeySharePrefix)+4)
	copy(key, ThresholdKeySharePrefix)
	key[len(ThresholdKeySharePrefix)] = byte(partyIndex >> 24)
	key[len(ThresholdKeySharePrefix)+1] = byte(partyIndex >> 16)
	key[len(ThresholdKeySharePrefix)+2] = byte(partyIndex >> 8)
	key[len(ThresholdKeySharePrefix)+3] = byte(partyIndex)
	return key
}

// GetBlockProofKey returns the store key for a MAAT block proof
func GetBlockProofKey(blockHeight uint64) []byte {
	key := make([]byte, len(BlockProofKeyPrefix)+8)
	copy(key, BlockProofKeyPrefix)
	for i := 0; i < 8; i++ {
		key[len(BlockProofKeyPrefix)+i] = byte(blockHeight >> (56 - 8*i))
	}
	return key
}

// GetEpochProofKey returns the store key for a MAAT epoch proof
func GetEpochProofKey(epochNumber uint64) []byte {
	key := make([]byte, len(EpochProofKeyPrefix)+8)
	copy(key, EpochProofKeyPrefix)
	for i := 0; i < 8; i++ {
		key[len(EpochProofKeyPrefix)+i] = byte(epochNumber >> (56 - 8*i))
	}
	return key
}

// GetCheckpointKey returns the store key for a TEFNUT light client checkpoint
func GetCheckpointKey(blockHeight uint64) []byte {
	key := make([]byte, len(CheckpointKeyPrefix)+8)
	copy(key, CheckpointKeyPrefix)
	for i := 0; i < 8; i++ {
		key[len(CheckpointKeyPrefix)+i] = byte(blockHeight >> (56 - 8*i))
	}
	return key
}

// GetAADKGStateKey returns the store key for AADKG state
func GetAADKGStateKey(epoch uint64) []byte {
	key := make([]byte, len(AADKGStateKeyPrefix)+8)
	copy(key, AADKGStateKeyPrefix)
	for i := 0; i < 8; i++ {
		key[len(AADKGStateKeyPrefix)+i] = byte(epoch >> (56 - 8*i))
	}
	return key
}
