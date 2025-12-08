// Additional types for ANUBIS privacy layer

package types

// Status constants for private execution
type RequestStatus int

const (
	StatusPending   RequestStatus = 0
	StatusCompleted RequestStatus = 1
	StatusExpired   RequestStatus = 2
	StatusFailed    RequestStatus = 3
)

// Prover status constants
type ProverStatus int

const (
	ProverStatusActive    ProverStatus = 0
	ProverStatusInactive  ProverStatus = 1
	ProverStatusUnbonding ProverStatus = 2
	ProverStatusSlashed   ProverStatus = 3
)

// Slash type constants
type SlashType string

const (
	SlashInvalidProof SlashType = "invalid_proof"
	SlashDowntime     SlashType = "downtime"
	SlashDoubleProve  SlashType = "double_prove"
)

// PrivateExecDeadline is the number of blocks before a request expires
const PrivateExecDeadline int64 = 100

// Note represents a confidential value (UTXO-like)
type Note struct {
	Commitment    []byte `json:"commitment"`
	EncryptedData []byte `json:"encrypted_data"`
	OwnerPKHash   []byte `json:"owner_pk_hash"`
}

// Proto interface implementations
func (m *Note) Reset()         { *m = Note{} }
func (m *Note) String() string { return "Note{...}" }
func (m *Note) ProtoMessage()  {}

// Disclosure represents a selective disclosure
type Disclosure struct {
	ID              []byte   `json:"id"`
	Owner           string   `json:"owner"`
	RecipientPKHash []byte   `json:"recipient_pk_hash"`
	AttributeMask   uint64   `json:"attribute_mask"`
	Attributes      []uint64 `json:"attributes"`
	ValidUntil      int64    `json:"valid_until"`
	CreatedAt       int64    `json:"created_at"`
}

// Proto interface implementations
func (m *Disclosure) Reset()         { *m = Disclosure{} }
func (m *Disclosure) String() string { return "Disclosure{...}" }
func (m *Disclosure) ProtoMessage()  {}

// PrivateExecutionRequest represents a pending private execution
type PrivateExecutionRequest struct {
	ID              []byte        `json:"id"`
	Contract        string        `json:"contract"`
	Sender          string        `json:"sender"`
	EncryptedInputs []byte        `json:"encrypted_inputs"`
	PublicInputs    []byte        `json:"public_inputs"`
	CallerPK        []byte        `json:"caller_pk"`
	GasLimit        uint64        `json:"gas_limit"`
	RequestedAt     int64         `json:"requested_at"`
	Deadline        int64         `json:"deadline"`
	Status          RequestStatus `json:"status"`
	LockedGas       uint64        `json:"locked_gas"`
	CompletedAt     int64         `json:"completed_at"`
	Prover          string        `json:"prover"`
}

// Proto interface implementations
func (m *PrivateExecutionRequest) Reset()         { *m = PrivateExecutionRequest{} }
func (m *PrivateExecutionRequest) String() string { return "PrivateExecutionRequest{...}" }
func (m *PrivateExecutionRequest) ProtoMessage()  {}

// EncryptedState represents encrypted contract state
type EncryptedState struct {
	Ciphertext    []byte `json:"ciphertext"`
	KEMCiphertext []byte `json:"kem_ciphertext"`
	Commitment    []byte `json:"commitment"`
}

// Proto interface implementations
func (m *EncryptedState) Reset()         { *m = EncryptedState{} }
func (m *EncryptedState) String() string { return "EncryptedState{...}" }
func (m *EncryptedState) ProtoMessage()  {}

// Prover represents a ZK prover in the network
type Prover struct {
	Operator    string       `json:"operator"`
	PK          []byte       `json:"pk"`
	Stake       uint64       `json:"stake"`
	Commission  uint32       `json:"commission"`
	Status      ProverStatus `json:"status"`
	JoinedAt    int64        `json:"joined_at"`
	UnbondingAt int64        `json:"unbonding_at"`
	ProofsCount uint64       `json:"proofs_count"`
	Slashed     uint64       `json:"slashed"`
}

// Proto interface implementations
func (m *Prover) Reset()         { *m = Prover{} }
func (m *Prover) String() string { return "Prover{...}" }
func (m *Prover) ProtoMessage()  {}

// MsgConfidentialTransfer is the message for confidential transfers
type MsgConfidentialTransfer struct {
	Sender               string   `json:"sender"`
	InputNullifiers      [][]byte `json:"input_nullifiers"`
	OutputNotes          []*Note  `json:"output_notes"`
	InputOwnershipProofs [][]byte `json:"input_ownership_proofs"`
	OutputRangeProofs    [][]byte `json:"output_range_proofs"`
	BalanceProof         []byte   `json:"balance_proof"`
	Fee                  uint64   `json:"fee"`
	Signature            []byte   `json:"signature"`
}

// Proto interface implementations
func (m *MsgConfidentialTransfer) Reset()         { *m = MsgConfidentialTransfer{} }
func (m *MsgConfidentialTransfer) String() string { return "MsgConfidentialTransfer{...}" }
func (m *MsgConfidentialTransfer) ProtoMessage()  {}

// MsgCreateDisclosure is the message for creating disclosures
type MsgCreateDisclosure struct {
	Owner           string   `json:"owner"`
	OwnerPK         []byte   `json:"owner_pk"`
	RecipientPKHash []byte   `json:"recipient_pk_hash"`
	AttributeMask   uint64   `json:"attribute_mask"`
	Attributes      []uint64 `json:"attributes"`
	ValidUntil      int64    `json:"valid_until"`
	Proof           []byte   `json:"proof"`
	Signature       []byte   `json:"signature"`
}

// Proto interface implementations
func (m *MsgCreateDisclosure) Reset()         { *m = MsgCreateDisclosure{} }
func (m *MsgCreateDisclosure) String() string { return "MsgCreateDisclosure{...}" }
func (m *MsgCreateDisclosure) ProtoMessage()  {}

// MsgRegisterProver is the message for registering a prover
type MsgRegisterProver struct {
	Operator    string `json:"operator"`
	ProverPK    []byte `json:"prover_pk"`
	StakeAmount uint64 `json:"stake_amount"`
	Commission  uint32 `json:"commission"`
	Metadata    string `json:"metadata"`
}

// Proto interface implementations
func (m *MsgRegisterProver) Reset()         { *m = MsgRegisterProver{} }
func (m *MsgRegisterProver) String() string { return "MsgRegisterProver{...}" }
func (m *MsgRegisterProver) ProtoMessage()  {}

// Event types
const (
	EventTypeConfidentialTransfer    = "confidential_transfer"
	EventTypeStateUpdate             = "state_update"
	EventTypeNullifierSpent          = "nullifier_spent"
	EventTypeShieldDeposit           = "shield_deposit"
	EventTypeDisclosureCreated       = "disclosure_created"
	EventTypePrivateExecutionRequest = "private_execution_request"
	EventTypePrivateExecutionComplete = "private_execution_complete"
	EventTypeProverRegistered        = "prover_registered"
	EventTypeProverSlashed           = "prover_slashed"
	EventTypeProverUnbonding         = "prover_unbonding"
)

// Attribute keys
const (
	AttributeKeyContract        = "contract"
	AttributeKeyOldRoot         = "old_root"
	AttributeKeyNewRoot         = "new_root"
	AttributeKeyNullifier       = "nullifier"
	AttributeKeyBlockHeight     = "block_height"
	AttributeKeyNullifierCount  = "nullifier_count"
	AttributeKeyNoteCount       = "note_count"
	AttributeKeyFee             = "fee"
	AttributeKeySender          = "sender"
	AttributeKeyNoteID          = "note_id"
	AttributeKeyDisclosureID    = "disclosure_id"
	AttributeKeyOwner           = "owner"
	AttributeKeyRequestID       = "request_id"
	AttributeKeyDeadline        = "deadline"
	AttributeKeyProver          = "prover"
	AttributeKeyGasUsed         = "gas_used"
	AttributeKeyOperator        = "operator"
	AttributeKeyStake           = "stake"
	AttributeKeySlashAmount     = "slash_amount"
	AttributeKeySlashType       = "slash_type"
)
