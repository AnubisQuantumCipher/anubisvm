// Package prover implements the off-chain prover daemon for AegisVM.
//
// The prover daemon listens for private execution requests, generates
// ZK proofs (STARK or lattice-based), and submits results to the chain.
//
// Per AegisVM Cosmos Chain blueprint v1.0, Part V: Prover Network
package prover

import (
	"context"
	"encoding/hex"
	"errors"
	"sync"
	"time"

	"cosmossdk.io/log"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/pkg/vmbridge"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// Daemon configuration constants
const (
	DefaultNumWorkers     = 4
	DefaultQueueSize      = 1000
	DefaultPollInterval   = 100 * time.Millisecond
	DefaultProofTimeout   = 5 * time.Minute
	DefaultRetryAttempts  = 3
	DefaultRetryDelay     = 1 * time.Second
)

// ProverDaemon runs the off-chain prover service.
// It listens for proof requests, generates proofs, and submits results.
type ProverDaemon struct {
	// Configuration
	config DaemonConfig

	// Chain connection
	client ChainClient

	// Prover identity
	operatorAddr string
	signingKey   pqcrypto.SecretKey    // ML-DSA for signing proofs
	decryptKey   pqcrypto.KEMSecretKey // ML-KEM for decrypting inputs

	// Prover engines
	starkProver   STARKProver
	latticeProver LatticeProver

	// VM for local execution
	vm *vmbridge.VMInstance

	// Work queue
	queue *ProofQueue

	// Metrics
	metrics *ProverMetrics

	// Lifecycle
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup
	logger log.Logger
}

// DaemonConfig holds prover daemon configuration
type DaemonConfig struct {
	// OperatorAddress is the prover's bech32 address
	OperatorAddress string

	// SigningKeyPath is the path to the ML-DSA secret key
	SigningKeyPath string

	// DecryptKeyPath is the path to the ML-KEM secret key
	DecryptKeyPath string

	// NumWorkers is the number of concurrent proof workers
	NumWorkers int

	// QueueSize is the maximum pending requests
	QueueSize int

	// MaxGas is the maximum gas to spend on proofs
	MaxGas uint64

	// ChainRPC is the CometBFT RPC endpoint
	ChainRPC string

	// ChainGRPC is the gRPC endpoint
	ChainGRPC string

	// ProofTimeout is the maximum time to generate a proof
	ProofTimeout time.Duration
}

// DefaultConfig returns default daemon configuration
func DefaultConfig() DaemonConfig {
	return DaemonConfig{
		NumWorkers:   DefaultNumWorkers,
		QueueSize:    DefaultQueueSize,
		MaxGas:       10_000_000,
		ProofTimeout: DefaultProofTimeout,
	}
}

// NewProverDaemon creates a new prover daemon
func NewProverDaemon(
	config DaemonConfig,
	client ChainClient,
	signingKey pqcrypto.SecretKey,
	decryptKey pqcrypto.KEMSecretKey,
	logger log.Logger,
) (*ProverDaemon, error) {
	// Create VM instance
	vm, err := vmbridge.NewVM()
	if err != nil {
		return nil, err
	}

	return &ProverDaemon{
		config:       config,
		client:       client,
		operatorAddr: config.OperatorAddress,
		signingKey:   signingKey,
		decryptKey:   decryptKey,
		starkProver:  NewSTARKProver(),
		latticeProver: NewLatticeProver(),
		vm:           vm,
		queue:        NewProofQueue(config.QueueSize),
		metrics:      NewProverMetrics(),
		logger:       logger,
	}, nil
}

// Start begins the prover daemon
func (d *ProverDaemon) Start(ctx context.Context) error {
	d.ctx, d.cancel = context.WithCancel(ctx)

	d.logger.Info("starting prover daemon",
		"operator", d.operatorAddr,
		"workers", d.config.NumWorkers,
	)

	// Subscribe to proof request events
	eventCh, err := d.client.SubscribeToEvents(
		d.ctx,
		"tm.event='Tx' AND anubis.event_type='private_execution_request'",
	)
	if err != nil {
		return err
	}

	// Start workers
	for i := 0; i < d.config.NumWorkers; i++ {
		d.wg.Add(1)
		go d.worker(i)
	}

	// Start metrics reporter
	d.wg.Add(1)
	go d.metricsReporter()

	// Main event loop
	go func() {
		for {
			select {
			case event := <-eventCh:
				d.handleEvent(event)
			case <-d.ctx.Done():
				return
			}
		}
	}()

	return nil
}

// Stop gracefully stops the daemon
func (d *ProverDaemon) Stop() error {
	d.logger.Info("stopping prover daemon")
	d.cancel()
	d.wg.Wait()

	if d.vm != nil {
		d.vm.Close()
	}

	return nil
}

// handleEvent processes a proof request event
func (d *ProverDaemon) handleEvent(event Event) {
	request, err := ParsePrivateExecutionRequest(event)
	if err != nil {
		d.metrics.RecordError("parse_error")
		d.logger.Error("failed to parse request", "error", err)
		return
	}

	// Check if we should prove this (based on strategy)
	if !d.shouldProve(request) {
		d.logger.Debug("skipping request", "request_id", hex.EncodeToString(request.ID))
		return
	}

	// Add to queue
	if err := d.queue.Push(request); err != nil {
		d.metrics.RecordError("queue_full")
		d.logger.Error("queue full, dropping request", "request_id", hex.EncodeToString(request.ID))
		return
	}

	d.logger.Info("queued proof request",
		"request_id", hex.EncodeToString(request.ID),
		"contract", request.Contract,
	)
}

// shouldProve determines if this prover should handle the request
func (d *ProverDaemon) shouldProve(request *PrivateExecutionRequest) bool {
	// Check gas limit
	if request.GasLimit > d.config.MaxGas {
		return false
	}

	// Check deadline
	currentBlock, err := d.client.GetLatestBlockHeight()
	if err != nil {
		return false
	}
	if currentBlock >= request.Deadline {
		return false
	}

	// TODO: Add more sophisticated selection criteria:
	// - Stake-weighted probability
	// - Hardware capabilities
	// - Historical success rate

	return true
}

// worker processes proof requests from the queue
func (d *ProverDaemon) worker(id int) {
	defer d.wg.Done()

	d.logger.Debug("worker started", "worker_id", id)

	for {
		select {
		case <-d.ctx.Done():
			return
		default:
			request := d.queue.Pop()
			if request == nil {
				time.Sleep(DefaultPollInterval)
				continue
			}

			d.processRequest(id, request)
		}
	}
}

// processRequest generates proof for a request
func (d *ProverDaemon) processRequest(workerID int, request *PrivateExecutionRequest) {
	start := time.Now()
	requestID := hex.EncodeToString(request.ID)

	d.logger.Info("processing request",
		"worker", workerID,
		"request_id", requestID,
	)

	// Create timeout context
	ctx, cancel := context.WithTimeout(d.ctx, d.config.ProofTimeout)
	defer cancel()

	// 1. Fetch contract state and code
	contract, err := d.client.GetContract(request.Contract)
	if err != nil {
		d.metrics.RecordError("fetch_contract")
		d.logger.Error("failed to fetch contract", "error", err)
		return
	}

	state, err := d.client.GetEncryptedState(request.Contract)
	if err != nil {
		d.metrics.RecordError("fetch_state")
		d.logger.Error("failed to fetch state", "error", err)
		return
	}

	// 2. Decrypt inputs using our KEM key
	decryptedInputs, err := d.decryptInputs(request.EncryptedInputs)
	if err != nil {
		d.metrics.RecordError("decrypt_inputs")
		d.logger.Error("failed to decrypt inputs", "error", err)
		return
	}

	// 3. Decrypt state
	decryptedState, err := d.decryptState(state)
	if err != nil {
		d.metrics.RecordError("decrypt_state")
		d.logger.Error("failed to decrypt state", "error", err)
		return
	}

	// 4. Execute locally to get new state
	result, err := d.executeLocally(ctx, contract.Code, decryptedState, decryptedInputs, request.PublicInputs, request.GasLimit)
	if err != nil {
		d.metrics.RecordError("execution")
		d.logger.Error("local execution failed", "error", err)
		return
	}

	// 5. Generate ZK proof of execution
	proof, err := d.generateProof(ctx, contract, decryptedState, decryptedInputs, request.PublicInputs, result)
	if err != nil {
		d.metrics.RecordError("proof_generation")
		d.logger.Error("proof generation failed", "error", err)
		return
	}

	// 6. Encrypt outputs for caller
	encryptedOutputs, err := d.encryptOutputs(result.Output, request.CallerPK)
	if err != nil {
		d.metrics.RecordError("encrypt_outputs")
		d.logger.Error("failed to encrypt outputs", "error", err)
		return
	}

	// 7. Compute new state root
	newStateRoot := d.computeStateRoot(result.NewState)

	// 8. Sign the result
	resultMsg := &ProofResultMessage{
		RequestID:        request.ID,
		NewStateRoot:     newStateRoot,
		EncryptedOutputs: encryptedOutputs,
		Proof:            proof,
		GasUsed:          result.GasUsed,
	}

	signature, err := pqcrypto.Sign(d.signingKey, resultMsg.Hash())
	if err != nil {
		d.metrics.RecordError("sign")
		d.logger.Error("failed to sign result", "error", err)
		return
	}

	// 9. Submit to chain
	msg := &types.MsgSubmitProof{
		Sender:           d.operatorAddr,
		Prover:           d.operatorAddr,
		RequestID:        request.ID,
		Proof:            types.ZKStateProof{Proof: proof},
		NewStateRoot:     newStateRoot,
		EncryptedOutputs: encryptedOutputs,
		GasUsed:          result.GasUsed,
		Signature:        signature,
	}

	txResp, err := d.client.BroadcastTx(msg)
	if err != nil {
		d.metrics.RecordError("broadcast")
		d.logger.Error("failed to broadcast tx", "error", err)
		return
	}

	// 10. Record metrics
	duration := time.Since(start)
	d.metrics.RecordProof(duration, len(proof), txResp.GasUsed)

	d.logger.Info("proof submitted successfully",
		"request_id", requestID,
		"duration", duration,
		"proof_size", len(proof),
		"gas_used", txResp.GasUsed,
	)
}

// decryptInputs decrypts the encrypted inputs using our KEM key
func (d *ProverDaemon) decryptInputs(encryptedInputs []byte) ([]byte, error) {
	if len(encryptedInputs) < pqcrypto.KEMCiphertextSize {
		return nil, errors.New("encrypted inputs too short")
	}

	// Extract ciphertext and encrypted data
	ct, err := pqcrypto.KEMCiphertextFromBytes(encryptedInputs[:pqcrypto.KEMCiphertextSize])
	if err != nil {
		return nil, err
	}

	// Decapsulate to get shared secret
	ss, err := pqcrypto.Decapsulate(d.decryptKey, ct)
	if err != nil {
		return nil, err
	}

	// Use shared secret to decrypt the rest
	// TODO: Implement AES-GCM decryption with derived key
	_ = ss
	return encryptedInputs[pqcrypto.KEMCiphertextSize:], nil
}

// decryptState decrypts the encrypted contract state
func (d *ProverDaemon) decryptState(encryptedState []byte) ([]byte, error) {
	// Similar to decryptInputs
	// TODO: Implement full decryption
	return encryptedState, nil
}

// executeLocally runs the contract locally
func (d *ProverDaemon) executeLocally(
	ctx context.Context,
	code []byte,
	state []byte,
	inputs []byte,
	publicInputs []byte,
	gasLimit uint64,
) (*ExecutionResult, error) {
	// Load bytecode
	if err := d.vm.LoadBytecode(code); err != nil {
		return nil, err
	}

	// Create execution context
	execCtx := vmbridge.ExecutionContext{
		GasLimit:     gasLimit,
		IsStaticCall: false,
	}

	// Execute
	result, err := d.vm.Execute(execCtx, nil, inputs, state)
	if err != nil {
		return nil, err
	}

	return &ExecutionResult{
		Success:  result.Success,
		Output:   result.ReturnData,
		NewState: d.applyStateChanges(state, result.StateChanges),
		GasUsed:  result.GasUsed,
	}, nil
}

// applyStateChanges applies state changes to get new state
func (d *ProverDaemon) applyStateChanges(oldState []byte, changes []vmbridge.StateChange) []byte {
	// TODO: Implement proper state merkle tree updates
	return oldState
}

// generateProof generates a ZK proof of correct execution
func (d *ProverDaemon) generateProof(
	ctx context.Context,
	contract *ContractInfo,
	state []byte,
	inputs []byte,
	publicInputs []byte,
	result *ExecutionResult,
) ([]byte, error) {
	// Generate STARK proof using execution trace
	return d.starkProver.Prove(
		contract.CodeHash,
		state,
		inputs,
		publicInputs,
		result.NewState,
		result.Output,
	)
}

// encryptOutputs encrypts outputs for the caller
func (d *ProverDaemon) encryptOutputs(outputs []byte, callerPK []byte) ([]byte, error) {
	if len(callerPK) != pqcrypto.KEMPublicKeySize {
		return nil, errors.New("invalid caller public key")
	}

	var pk pqcrypto.KEMPublicKey
	copy(pk[:], callerPK)

	// Encapsulate to get shared secret
	ss, ct, err := pqcrypto.Encapsulate(pk, nil)
	if err != nil {
		return nil, err
	}

	// Use shared secret to encrypt outputs
	// TODO: Implement AES-GCM encryption with derived key
	_ = ss

	// Return ciphertext || encrypted data
	result := make([]byte, 0, pqcrypto.KEMCiphertextSize+len(outputs))
	result = append(result, ct[:]...)
	result = append(result, outputs...)
	return result, nil
}

// computeStateRoot computes the Merkle root of the new state
func (d *ProverDaemon) computeStateRoot(state []byte) []byte {
	hash, _ := pqcrypto.SHA3_256(state)
	return hash[:]
}

// metricsReporter periodically logs metrics
func (d *ProverDaemon) metricsReporter() {
	defer d.wg.Done()

	ticker := time.NewTicker(1 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-d.ctx.Done():
			return
		case <-ticker.C:
			d.metrics.Log(d.logger)
		}
	}
}
