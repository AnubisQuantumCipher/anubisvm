// Private execution types for x/anubis

package types

// PrivateContract represents a contract with private execution capability
type PrivateContract struct {
	// Address is the contract address
	Address []byte `json:"address"`

	// PublicKey is the ML-KEM-1024 public key for key encapsulation
	PublicKey []byte `json:"public_key"`

	// EncryptionScheme defines the encryption method used
	EncryptionScheme string `json:"encryption_scheme"`

	// AuthorizedCallers are addresses allowed to make private calls
	AuthorizedCallers [][]byte `json:"authorized_callers,omitempty"`

	// MaxPrivateCalls is the maximum concurrent private calls
	MaxPrivateCalls uint32 `json:"max_private_calls"`

	// Enabled indicates if private execution is enabled
	Enabled bool `json:"enabled"`

	// CodeHash is the SHA3-256 hash of the contract code
	CodeHash []byte `json:"code_hash"`
}

// EncryptedCall represents an encrypted contract call
type EncryptedCall struct {
	// CallID is the unique identifier for this call
	CallID []byte `json:"call_id"`

	// ContractAddress is the target contract
	ContractAddress []byte `json:"contract_address"`

	// Sender is the caller address
	Sender []byte `json:"sender"`

	// EncryptedFunction is the encrypted function selector
	EncryptedFunction []byte `json:"encrypted_function"`

	// EncryptedArgs is the encrypted call arguments
	EncryptedArgs []byte `json:"encrypted_args"`

	// KEMCiphertext is the ML-KEM-1024 encapsulated key
	KEMCiphertext []byte `json:"kem_ciphertext"`

	// GasLimit is the maximum gas for execution
	GasLimit uint64 `json:"gas_limit"`

	// Nonce prevents replay attacks
	Nonce []byte `json:"nonce"`

	// Signature is the ML-DSA-87 signature over the call
	Signature []byte `json:"signature"`
}

// EncryptedResult represents the result of a private execution
type EncryptedResult struct {
	// CallID references the original call
	CallID []byte `json:"call_id"`

	// Success indicates if execution succeeded
	Success bool `json:"success"`

	// EncryptedReturnData is the encrypted return value
	EncryptedReturnData []byte `json:"encrypted_return_data"`

	// EncryptedState is the encrypted new state
	EncryptedState []byte `json:"encrypted_state,omitempty"`

	// GasUsed is the amount of gas consumed
	GasUsed uint64 `json:"gas_used"`

	// StateProof is a ZK proof of correct state transition
	StateProof []byte `json:"state_proof,omitempty"`

	// KEMCiphertext is used for decrypting the result
	KEMCiphertext []byte `json:"kem_ciphertext"`
}

// ZKStateProof represents a zero-knowledge proof of state transition
type ZKStateProof struct {
	// ProofHash is the SHA3-256 hash of the proof
	ProofHash []byte `json:"proof_hash"`

	// OldStateRoot is the Merkle root of the old state
	OldStateRoot []byte `json:"old_state_root"`

	// NewStateRoot is the Merkle root of the new state
	NewStateRoot []byte `json:"new_state_root"`

	// Proof is the ZK proof data
	Proof []byte `json:"proof"`

	// ProofSystem is the ZK proof system used (e.g., "plonk", "groth16")
	ProofSystem string `json:"proof_system"`

	// CircuitHash identifies the circuit used
	CircuitHash []byte `json:"circuit_hash"`
}

// SessionKey represents an encrypted session key for multi-call sessions
type SessionKey struct {
	// SessionID is the unique session identifier
	SessionID []byte `json:"session_id"`

	// ContractAddress is the target contract
	ContractAddress []byte `json:"contract_address"`

	// Initiator is the session initiator
	Initiator []byte `json:"initiator"`

	// EncryptedKey is the encrypted session key
	EncryptedKey []byte `json:"encrypted_key"`

	// ExpiresAt is the block height when the session expires
	ExpiresAt int64 `json:"expires_at"`

	// CallCount is the number of calls made in this session
	CallCount uint64 `json:"call_count"`

	// MaxCalls is the maximum calls allowed in this session
	MaxCalls uint64 `json:"max_calls"`
}

// Encryption schemes
const (
	EncryptionSchemeFHE    = "fhe-cggi"       // CGGI-based FHE
	EncryptionSchemeAESGCM = "aes-256-gcm"    // AES-256-GCM for symmetric
	EncryptionSchemeKEM    = "ml-kem-1024"    // ML-KEM-1024 for key exchange
)

// ZK proof systems
const (
	ProofSystemPlonk    = "plonk"
	ProofSystemGroth16  = "groth16"
	ProofSystemSTARK    = "stark"
)

// ValidatePrivateContract validates a private contract configuration
func ValidatePrivateContract(pc PrivateContract) error {
	if len(pc.Address) != 20 {
		return ErrInvalidAddress
	}
	if len(pc.PublicKey) != 1568 { // ML-KEM-1024 public key size
		return ErrInvalidPublicKey
	}
	if pc.EncryptionScheme == "" {
		return ErrInvalidEncryption
	}
	return nil
}

// ValidateEncryptedCall validates an encrypted call
func ValidateEncryptedCall(ec EncryptedCall) error {
	if len(ec.CallID) != 32 {
		return ErrInvalidCallID
	}
	if len(ec.ContractAddress) != 20 {
		return ErrInvalidAddress
	}
	if len(ec.KEMCiphertext) != 1568 { // ML-KEM-1024 ciphertext size
		return ErrInvalidCiphertext
	}
	if len(ec.Signature) != 4627 { // ML-DSA-87 signature size
		return ErrInvalidSignature
	}
	return nil
}
