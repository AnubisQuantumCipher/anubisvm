// Types for prover daemon

package prover

import (
	"time"

	"cosmossdk.io/log"
	"github.com/aegisvm/cosmos/pkg/pqcrypto"
)

// PrivateExecutionRequest represents a request for private execution
type PrivateExecutionRequest struct {
	// ID is the unique request identifier (32 bytes)
	ID []byte

	// Contract is the contract address being called
	Contract string

	// Caller is the transaction sender
	Caller string

	// CallerPK is the caller's ML-KEM public key for response encryption
	CallerPK []byte

	// Function is the function selector
	Function []byte

	// PublicInputs are visible on-chain
	PublicInputs []byte

	// EncryptedInputs are encrypted to the prover's key
	EncryptedInputs []byte

	// GasLimit is the maximum gas for execution
	GasLimit uint64

	// Deadline is the block height by which proof must be submitted
	Deadline int64

	// Fee is the fee offered for proof generation
	Fee uint64

	// BlockHeight when request was submitted
	BlockHeight int64
}

// ExecutionResult holds the result of local execution
type ExecutionResult struct {
	Success  bool
	Output   []byte
	NewState []byte
	GasUsed  uint64
}

// ContractInfo holds contract metadata
type ContractInfo struct {
	Address  string
	Code     []byte
	CodeHash []byte
	Admin    string
}

// Event represents a chain event
type Event struct {
	Type       string
	Attributes map[string]string
	TxHash     string
	BlockHeight int64
}

// TxResponse represents a broadcast transaction response
type TxResponse struct {
	TxHash  string
	Code    uint32
	GasUsed uint64
}

// ProofResultMessage is the message signed by the prover
type ProofResultMessage struct {
	RequestID        []byte
	NewStateRoot     []byte
	EncryptedOutputs []byte
	Proof            []byte
	GasUsed          uint64
}

// Hash computes the SHA3-256 hash of the message for signing
func (m *ProofResultMessage) Hash() []byte {
	// Concatenate all fields
	data := make([]byte, 0, len(m.RequestID)+len(m.NewStateRoot)+len(m.EncryptedOutputs)+len(m.Proof)+8)
	data = append(data, m.RequestID...)
	data = append(data, m.NewStateRoot...)
	data = append(data, m.EncryptedOutputs...)
	data = append(data, m.Proof...)
	// Append gas used
	for i := 0; i < 8; i++ {
		data = append(data, byte(m.GasUsed>>(56-8*i)))
	}

	hash, _ := pqcrypto.SHA3_256(data)
	return hash[:]
}

// ChainClient interface for chain interactions
type ChainClient interface {
	// SubscribeToEvents subscribes to chain events
	SubscribeToEvents(ctx interface{}, query string) (chan Event, error)

	// GetLatestBlockHeight returns the current block height
	GetLatestBlockHeight() (int64, error)

	// GetContract fetches contract info
	GetContract(address string) (*ContractInfo, error)

	// GetEncryptedState fetches encrypted contract state
	GetEncryptedState(address string) ([]byte, error)

	// BroadcastTx broadcasts a transaction
	BroadcastTx(msg interface{}) (*TxResponse, error)
}

// STARKProver interface for STARK proof generation
type STARKProver interface {
	// Prove generates a STARK proof of execution
	Prove(
		codeHash []byte,
		oldState []byte,
		inputs []byte,
		publicInputs []byte,
		newState []byte,
		output []byte,
	) ([]byte, error)
}

// LatticeProver interface for lattice-based proofs
type LatticeProver interface {
	// Prove generates a lattice-based ZK proof
	Prove(
		statement []byte,
		witness []byte,
	) ([]byte, error)
}

// NewSTARKProver creates a new STARK prover instance
func NewSTARKProver() STARKProver {
	return &starkProverImpl{}
}

// NewLatticeProver creates a new lattice prover instance
func NewLatticeProver() LatticeProver {
	return &latticeProverImpl{}
}

// starkProverImpl implements STARKProver
type starkProverImpl struct{}

func (p *starkProverImpl) Prove(codeHash, oldState, inputs, publicInputs, newState, output []byte) ([]byte, error) {
	// TODO: Call SPARK/Ada STARK prover via CGO
	// This will use the THOTH AIR compiler from SCARAB
	return make([]byte, 4096), nil
}

// latticeProverImpl implements LatticeProver
type latticeProverImpl struct{}

func (p *latticeProverImpl) Prove(statement, witness []byte) ([]byte, error) {
	// TODO: Call SPARK/Ada lattice prover via CGO
	return make([]byte, 2048), nil
}

// ProverMetrics tracks prover performance
type ProverMetrics struct {
	proofsGenerated uint64
	proofsSubmitted uint64
	proofsFailed    uint64
	totalProofTime  time.Duration
	totalProofBytes uint64
	totalGasUsed    uint64
	errors          map[string]uint64
}

// NewProverMetrics creates new metrics tracker
func NewProverMetrics() *ProverMetrics {
	return &ProverMetrics{
		errors: make(map[string]uint64),
	}
}

// RecordProof records a successful proof
func (m *ProverMetrics) RecordProof(duration time.Duration, proofSize int, gasUsed uint64) {
	m.proofsGenerated++
	m.proofsSubmitted++
	m.totalProofTime += duration
	m.totalProofBytes += uint64(proofSize)
	m.totalGasUsed += gasUsed
}

// RecordError records an error
func (m *ProverMetrics) RecordError(errorType string) {
	m.proofsFailed++
	m.errors[errorType]++
}

// Log outputs metrics to logger
func (m *ProverMetrics) Log(logger log.Logger) {
	var avgTime time.Duration
	if m.proofsGenerated > 0 {
		avgTime = m.totalProofTime / time.Duration(m.proofsGenerated)
	}

	logger.Info("prover metrics",
		"generated", m.proofsGenerated,
		"submitted", m.proofsSubmitted,
		"failed", m.proofsFailed,
		"avg_time", avgTime,
		"total_gas", m.totalGasUsed,
	)
}

// ParsePrivateExecutionRequest parses an event into a request
func ParsePrivateExecutionRequest(event Event) (*PrivateExecutionRequest, error) {
	// TODO: Implement full parsing
	return &PrivateExecutionRequest{
		ID:          []byte(event.Attributes["request_id"]),
		Contract:    event.Attributes["contract"],
		Caller:      event.Attributes["caller"],
		BlockHeight: event.BlockHeight,
	}, nil
}
