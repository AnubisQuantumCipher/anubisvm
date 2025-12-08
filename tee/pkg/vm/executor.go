// Package vm provides the interface to the AegisVM SPARK execution engine.
//
// This uses process isolation to maintain formal verification guarantees.
// The SPARK-verified VM runs as a separate process, communicating via
// a well-defined protocol. This prevents FFI boundary issues from
// compromising the verification.
package vm

import (
	"bufio"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"sync"
)

// Executor manages the AegisVM execution process
type Executor struct {
	cmd    *exec.Cmd
	stdin  io.WriteCloser
	stdout *bufio.Reader
	mu     sync.Mutex
}

// ExecutionRequest is sent to the VM process
type ExecutionRequest struct {
	Type     string `json:"type"`      // "execute", "deploy", "query"
	From     string `json:"from"`      // Caller address
	To       string `json:"to"`        // Contract address (empty for deploy)
	Code     string `json:"code"`      // Hex-encoded bytecode
	Input    string `json:"input"`     // Hex-encoded call data
	Gas      uint64 `json:"gas"`       // Gas limit
	Value    uint64 `json:"value"`     // Transfer value
	StateIn  string `json:"state_in"`  // Hex-encoded state root
}

// ExecutionResult is returned from the VM process
type ExecutionResult struct {
	Success  bool   `json:"success"`
	GasUsed  uint64 `json:"gas_used"`
	Output   string `json:"output"`     // Hex-encoded return data
	StateOut string `json:"state_out"`  // New state root
	Error    string `json:"error,omitempty"`
	Logs     []Log  `json:"logs,omitempty"`
}

// Log represents an event emitted during execution
type Log struct {
	Address string   `json:"address"`
	Topics  []string `json:"topics"`
	Data    string   `json:"data"`
}

// NewExecutor creates a new VM executor
// vmPath is the path to the aegis-vm executable (the SPARK verified binary)
func NewExecutor(vmPath string) (*Executor, error) {
	cmd := exec.Command(vmPath, "--ipc-mode")

	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, fmt.Errorf("failed to create stdin pipe: %w", err)
	}

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("failed to create stdout pipe: %w", err)
	}

	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("failed to start VM process: %w", err)
	}

	return &Executor{
		cmd:    cmd,
		stdin:  stdin,
		stdout: bufio.NewReader(stdout),
	}, nil
}

// Execute runs a contract call
func (e *Executor) Execute(req ExecutionRequest) (*ExecutionResult, error) {
	e.mu.Lock()
	defer e.mu.Unlock()

	req.Type = "execute"
	return e.sendRequest(req)
}

// Deploy deploys a new contract
func (e *Executor) Deploy(req ExecutionRequest) (*ExecutionResult, error) {
	e.mu.Lock()
	defer e.mu.Unlock()

	req.Type = "deploy"
	return e.sendRequest(req)
}

// Query performs a read-only state query
func (e *Executor) Query(req ExecutionRequest) (*ExecutionResult, error) {
	e.mu.Lock()
	defer e.mu.Unlock()

	req.Type = "query"
	return e.sendRequest(req)
}

func (e *Executor) sendRequest(req ExecutionRequest) (*ExecutionResult, error) {
	// Encode request as JSON line
	data, err := json.Marshal(req)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %w", err)
	}

	// Send to VM process
	if _, err := e.stdin.Write(append(data, '\n')); err != nil {
		return nil, fmt.Errorf("failed to write to VM: %w", err)
	}

	// Read response
	line, err := e.stdout.ReadBytes('\n')
	if err != nil {
		return nil, fmt.Errorf("failed to read from VM: %w", err)
	}

	var result ExecutionResult
	if err := json.Unmarshal(line, &result); err != nil {
		return nil, fmt.Errorf("failed to unmarshal result: %w", err)
	}

	return &result, nil
}

// Close shuts down the VM process
func (e *Executor) Close() error {
	e.stdin.Close()
	return e.cmd.Wait()
}

// =============================================================================
// Embedded Executor (for when process isolation isn't needed)
// =============================================================================

// EmbeddedExecutor runs the VM in-process (uses CGO, loses some verification guarantees)
type EmbeddedExecutor struct {
	state map[string][]byte
	mu    sync.RWMutex
}

// NewEmbeddedExecutor creates an in-process executor
func NewEmbeddedExecutor() *EmbeddedExecutor {
	return &EmbeddedExecutor{
		state: make(map[string][]byte),
	}
}

// Execute runs a contract in-process
func (e *EmbeddedExecutor) Execute(req ExecutionRequest) (*ExecutionResult, error) {
	// For now, return a placeholder result
	// TODO: Integrate with SPARK VM via CGO (with the understanding that
	// verification guarantees are weaker at the FFI boundary)

	return &ExecutionResult{
		Success: true,
		GasUsed: 21000,
		Output:  "",
	}, nil
}

// Deploy deploys a contract in-process
func (e *EmbeddedExecutor) Deploy(req ExecutionRequest) (*ExecutionResult, error) {
	code, err := hex.DecodeString(req.Code)
	if err != nil {
		return nil, errors.New("invalid bytecode hex")
	}

	// Generate contract address (simplified)
	contractAddr := fmt.Sprintf("aegis1contract%x", len(e.state))

	e.mu.Lock()
	e.state[contractAddr] = code
	e.mu.Unlock()

	return &ExecutionResult{
		Success: true,
		GasUsed: uint64(len(code) * 200), // Gas per byte
		Output:  contractAddr,
	}, nil
}

// Query performs a read-only query
func (e *EmbeddedExecutor) Query(req ExecutionRequest) (*ExecutionResult, error) {
	e.mu.RLock()
	defer e.mu.RUnlock()

	if code, ok := e.state[req.To]; ok {
		return &ExecutionResult{
			Success: true,
			Output:  hex.EncodeToString(code),
		}, nil
	}

	return &ExecutionResult{
		Success: false,
		Error:   "contract not found",
	}, nil
}

func (e *EmbeddedExecutor) Close() error {
	return nil
}
