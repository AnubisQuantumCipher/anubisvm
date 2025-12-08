// SCARAB v2.0 types for x/anubis module
//
// This file defines types for the SCARAB proof aggregation system:
// - KHNUM: TX signature aggregation
// - SEBEK: Threshold signatures
// - MAAT: Hierarchical proof aggregation
// - HORUS: Pipelined prover market
// - TEFNUT: Light client proofs
// - AADKG: Distributed key generation

package types

import (
	"cosmossdk.io/errors"
)

// ============================================================================
// HORUS - Proof Job Types
// ============================================================================

// Proof job status constants
const (
	ProofJobStatusPending   = 0
	ProofJobStatusClaimed   = 1
	ProofJobStatusSubmitted = 2
	ProofJobStatusVerified  = 3
	ProofJobStatusExpired   = 4
)

// ProofJobInfo represents a HORUS proof job in the prover market
type ProofJobInfo struct {
	JobID       []byte `json:"job_id"`
	BatchHash   []byte `json:"batch_hash"`
	BatchSize   uint64 `json:"batch_size"`
	BlockHeight uint64 `json:"block_height"`
	Deadline    uint64 `json:"deadline"`
	Reward      uint64 `json:"reward"`
	Status      int    `json:"status"`
	ClaimedBy   []byte `json:"claimed_by,omitempty"`
	ClaimedAt   uint64 `json:"claimed_at,omitempty"`
	Proof       []byte `json:"proof,omitempty"`
	SubmittedAt uint64 `json:"submitted_at,omitempty"`
}

// ProverInfoProto is the protobuf-compatible prover info type
type ProverInfoProto struct {
	Address       []byte `protobuf:"bytes,1,opt,name=address" json:"address,omitempty"`
	Stake         uint64 `protobuf:"varint,2,opt,name=stake" json:"stake,omitempty"`
	Tier          uint32 `protobuf:"varint,3,opt,name=tier" json:"tier,omitempty"`
	JobsCompleted uint64 `protobuf:"varint,4,opt,name=jobs_completed,json=jobsCompleted" json:"jobs_completed,omitempty"`
	Reputation    uint64 `protobuf:"varint,5,opt,name=reputation" json:"reputation,omitempty"`
	Active        bool   `protobuf:"varint,6,opt,name=active" json:"active,omitempty"`
}

func (m *ProverInfoProto) Reset()         { *m = ProverInfoProto{} }
func (m *ProverInfoProto) String() string { return "" }
func (*ProverInfoProto) ProtoMessage()    {}

// ============================================================================
// SEBEK - Threshold Signature Types
// ============================================================================

// ThresholdKeyShare represents a party's share in threshold signing
type ThresholdKeyShare struct {
	Index      uint32 `json:"index"`
	Share      []byte `json:"share"`
	Commitment []byte `json:"commitment"`
	Epoch      uint64 `json:"epoch"`
}

// ThresholdPublicKey represents the combined public key for threshold signing
type ThresholdPublicKey struct {
	CombinedPK []byte `json:"combined_pk"`
	Threshold  uint32 `json:"threshold"`
	NumSigners uint32 `json:"num_signers"`
	Epoch      uint64 `json:"epoch"`
}

// ============================================================================
// MAAT - Block/Epoch Proof Types
// ============================================================================

// BlockProofMeta contains metadata about a MAAT block proof
type BlockProofMeta struct {
	BlockHeight   uint64     `json:"block_height"`
	BatchCommits  [16][32]byte `json:"batch_commits"`
	ProofHash     []byte     `json:"proof_hash"`
	TxCount       uint64     `json:"tx_count"`
	GeneratedAt   uint64     `json:"generated_at"`
	ProverAddress []byte     `json:"prover_address"`
}

// EpochProofMeta contains metadata about a MAAT epoch proof
type EpochProofMeta struct {
	EpochNumber    uint64   `json:"epoch_number"`
	StartBlock     uint64   `json:"start_block"`
	EndBlock       uint64   `json:"end_block"`
	StateRoot      []byte   `json:"state_root"`
	PrevStateRoot  []byte   `json:"prev_state_root"`
	ProofHash      []byte   `json:"proof_hash"`
	BlockCount     uint64   `json:"block_count"`
}

// ============================================================================
// TEFNUT - Light Client Types
// ============================================================================

// LightClientCheckpoint represents a verified checkpoint for light clients
type LightClientCheckpoint struct {
	BlockHeight   uint64 `json:"block_height"`
	StateRoot     []byte `json:"state_root"`
	CommitteeRoot []byte `json:"committee_root"`
	Proof         []byte `json:"proof"`
	Timestamp     uint64 `json:"timestamp"`
}

// ============================================================================
// AADKG - Distributed Key Generation Types
// ============================================================================

// AADKGPhase represents the current phase of DKG
type AADKGPhase int

const (
	AADKGPhaseCommitment AADKGPhase = iota
	AADKGPhaseDistribution
	AADKGPhaseVerification
	AADKGPhaseAggregation
	AADKGPhaseRecovery
	AADKGPhaseComplete
	AADKGPhaseFailed
)

// AADKGState represents the on-chain DKG state
type AADKGState struct {
	Epoch          uint64     `json:"epoch"`
	Phase          AADKGPhase `json:"phase"`
	Participants   uint32     `json:"participants"`
	Threshold      uint32     `json:"threshold"`
	StartBlock     uint64     `json:"start_block"`
	CommitteeRoot  []byte     `json:"committee_root"`
	CombinedPK     []byte     `json:"combined_pk,omitempty"`
}

// EncryptedShare represents an ML-KEM encrypted share for AADKG
type EncryptedShare struct {
	Sender     uint32 `json:"sender"`
	Recipient  uint32 `json:"recipient"`
	Ciphertext []byte `json:"ciphertext"`
	Signature  []byte `json:"signature"`
}

// ============================================================================
// SCARAB Error Codes
// ============================================================================

var (
	// KHNUM errors
	ErrAggregationFailed    = errors.Register(ModuleName, 100, "signature aggregation failed")
	ErrVerificationFailed   = errors.Register(ModuleName, 101, "proof verification failed")
	ErrInvalidBatchSize     = errors.Register(ModuleName, 102, "invalid batch size")

	// SEBEK errors
	ErrThresholdSignFailed   = errors.Register(ModuleName, 110, "threshold partial sign failed")
	ErrThresholdCombineFailed = errors.Register(ModuleName, 111, "threshold signature combine failed")
	ErrThresholdVerifyFailed = errors.Register(ModuleName, 112, "threshold signature verification failed")
	ErrThresholdNotMet       = errors.Register(ModuleName, 113, "threshold not met")

	// MAAT errors
	ErrProofGenerationFailed   = errors.Register(ModuleName, 120, "proof generation failed")
	ErrProofVerificationFailed = errors.Register(ModuleName, 121, "proof verification failed")
	ErrInvalidBlockProof       = errors.Register(ModuleName, 122, "invalid block proof")
	ErrInvalidEpochProof       = errors.Register(ModuleName, 123, "invalid epoch proof")

	// HORUS errors (using aliases where already defined in errors.go)
	ErrProverNotRegistered      = errors.Register(ModuleName, 130, "prover not registered")
	// ErrProverAlreadyRegistered - use ErrProverAlreadyExists from errors.go
	// ErrInsufficientStake - use existing from errors.go
	ErrProofJobNotFound         = errors.Register(ModuleName, 133, "proof job not found")
	ErrProofJobNotAvailable     = errors.Register(ModuleName, 134, "proof job not available")
	ErrNotJobOwner              = errors.Register(ModuleName, 135, "not proof job owner")
	ErrProofDeadlinePassed      = errors.Register(ModuleName, 136, "proof deadline passed")
	ErrInvalidProof             = errors.Register(ModuleName, 137, "invalid proof")

	// TEFNUT errors
	ErrLightClientProofFailed   = errors.Register(ModuleName, 140, "light client proof generation failed")
	ErrLightClientVerifyFailed  = errors.Register(ModuleName, 141, "light client proof verification failed")
	ErrCheckpointNotFound       = errors.Register(ModuleName, 142, "checkpoint not found")
	ErrStaleCheckpoint          = errors.Register(ModuleName, 143, "checkpoint is stale")

	// AADKG errors
	ErrDKGNotInitialized        = errors.Register(ModuleName, 150, "DKG not initialized")
	ErrDKGWrongPhase            = errors.Register(ModuleName, 151, "DKG in wrong phase")
	ErrDKGShareVerificationFailed = errors.Register(ModuleName, 152, "DKG share verification failed")
	ErrDKGRecoveryFailed        = errors.Register(ModuleName, 153, "DKG recovery failed")
)

// Aliases for backwards compatibility with errors.go
var (
	ErrProverAlreadyRegistered = ErrProverAlreadyExists
)
