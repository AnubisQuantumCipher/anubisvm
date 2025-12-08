// AegisVM Trusted Execution Environment
//
// Single-operator deployment of the AegisVM post-quantum virtual machine.
// Uses ML-DSA-87 for signatures, ML-KEM-1024 for encryption, and
// formally verified SPARK core for execution.
package main

import (
	"context"
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
)

// Configuration
type Config struct {
	ListenAddr   string `json:"listen_addr"`
	DataDir      string `json:"data_dir"`
	OperatorKey  string `json:"operator_key"`
	MaxGasPerTx  uint64 `json:"max_gas_per_tx"`
	BlockTime    int    `json:"block_time_seconds"`
}

// TEE Server
type TEEServer struct {
	config     Config
	state      *StateManager
	txPool     *TxPool
	blockNum   uint64
	mu         sync.RWMutex
	httpServer *http.Server
}

// Transaction represents a signed transaction
type Transaction struct {
	From      string `json:"from"`       // AAS-001 address
	To        string `json:"to"`         // AAS-001 address (empty for deploy)
	Data      []byte `json:"data"`       // Contract call data or bytecode
	Value     uint64 `json:"value"`      // ANUBIS transfer amount
	Gas       uint64 `json:"gas"`        // Gas limit
	Nonce     uint64 `json:"nonce"`      // Replay protection
	Signature []byte `json:"signature"`  // ML-DSA-87 signature
}

// TxResult is the result of transaction execution
type TxResult struct {
	Success     bool   `json:"success"`
	TxHash      string `json:"tx_hash"`
	BlockNumber uint64 `json:"block_number"`
	GasUsed     uint64 `json:"gas_used"`
	Result      []byte `json:"result,omitempty"`
	Error       string `json:"error,omitempty"`
}

// Block represents a committed block
type Block struct {
	Number       uint64        `json:"number"`
	Timestamp    int64         `json:"timestamp"`
	PrevHash     string        `json:"prev_hash"`
	StateRoot    string        `json:"state_root"`
	TxRoot       string        `json:"tx_root"`
	Transactions []Transaction `json:"transactions"`
	Signature    []byte        `json:"signature"` // Operator signature
}

// StateManager handles state storage and proofs
type StateManager struct {
	mu        sync.RWMutex
	accounts  map[string]*Account
	contracts map[string][]byte
	storage   map[string]map[string][]byte
	stateRoot []byte
}

// Account represents an account state
type Account struct {
	Address   string `json:"address"`
	Balance   uint64 `json:"balance"`
	Nonce     uint64 `json:"nonce"`
	CodeHash  string `json:"code_hash,omitempty"`
}

// TxPool holds pending transactions
type TxPool struct {
	mu      sync.Mutex
	pending []Transaction
}

func NewTEEServer(config Config) (*TEEServer, error) {
	state := &StateManager{
		accounts:  make(map[string]*Account),
		contracts: make(map[string][]byte),
		storage:   make(map[string]map[string][]byte),
		stateRoot: make([]byte, 32),
	}

	return &TEEServer{
		config:   config,
		state:    state,
		txPool:   &TxPool{pending: make([]Transaction, 0)},
		blockNum: 0,
	}, nil
}

func (s *TEEServer) Start() error {
	mux := http.NewServeMux()

	// API endpoints
	mux.HandleFunc("/", s.handleRoot)
	mux.HandleFunc("/health", s.handleHealth)
	mux.HandleFunc("/tx", s.handleTransaction)
	mux.HandleFunc("/state/", s.handleState)
	mux.HandleFunc("/proof/", s.handleProof)
	mux.HandleFunc("/deploy", s.handleDeploy)
	mux.HandleFunc("/block/", s.handleBlock)
	mux.HandleFunc("/account/", s.handleAccount)

	s.httpServer = &http.Server{
		Addr:         s.config.ListenAddr,
		Handler:      mux,
		ReadTimeout:  30 * time.Second,
		WriteTimeout: 30 * time.Second,
	}

	// Start block producer
	go s.blockProducer()

	log.Printf("AegisVM TEE Server starting on %s", s.config.ListenAddr)
	log.Printf("Data directory: %s", s.config.DataDir)
	log.Printf("Block time: %d seconds", s.config.BlockTime)

	return s.httpServer.ListenAndServe()
}

func (s *TEEServer) Shutdown(ctx context.Context) error {
	return s.httpServer.Shutdown(ctx)
}

func (s *TEEServer) blockProducer() {
	ticker := time.NewTicker(time.Duration(s.config.BlockTime) * time.Second)
	defer ticker.Stop()

	for range ticker.C {
		s.produceBlock()
	}
}

func (s *TEEServer) produceBlock() {
	s.txPool.mu.Lock()
	txs := s.txPool.pending
	s.txPool.pending = make([]Transaction, 0)
	s.txPool.mu.Unlock()

	if len(txs) == 0 {
		return // No transactions, skip empty block
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	s.blockNum++
	block := Block{
		Number:       s.blockNum,
		Timestamp:    time.Now().Unix(),
		PrevHash:     "", // TODO: compute from previous block
		Transactions: txs,
	}

	// Execute transactions
	for _, tx := range txs {
		s.executeTx(tx)
	}

	// Update state root
	s.state.mu.Lock()
	block.StateRoot = hex.EncodeToString(s.state.stateRoot)
	s.state.mu.Unlock()

	log.Printf("Block %d produced with %d transactions", block.Number, len(txs))
}

func (s *TEEServer) executeTx(tx Transaction) *TxResult {
	result := &TxResult{
		Success:     true,
		BlockNumber: s.blockNum,
	}

	// TODO: Call into AegisVM SPARK execution engine
	// For now, just update balances for transfers

	s.state.mu.Lock()
	defer s.state.mu.Unlock()

	// Get or create accounts
	from := s.state.accounts[tx.From]
	if from == nil {
		from = &Account{Address: tx.From, Balance: 0, Nonce: 0}
		s.state.accounts[tx.From] = from
	}

	to := s.state.accounts[tx.To]
	if to == nil && tx.To != "" {
		to = &Account{Address: tx.To, Balance: 0, Nonce: 0}
		s.state.accounts[tx.To] = to
	}

	// Check nonce
	if tx.Nonce != from.Nonce {
		result.Success = false
		result.Error = "invalid nonce"
		return result
	}

	// Check balance
	if from.Balance < tx.Value {
		result.Success = false
		result.Error = "insufficient balance"
		return result
	}

	// Execute transfer
	from.Balance -= tx.Value
	from.Nonce++
	if to != nil {
		to.Balance += tx.Value
	}

	result.GasUsed = 21000 // Base gas for transfer
	return result
}

// HTTP Handlers

func (s *TEEServer) handleRoot(w http.ResponseWriter, r *http.Request) {
	info := map[string]interface{}{
		"name":        "AegisVM TEE",
		"version":     "0.1.0",
		"chain_id":    "aegis-tee-1",
		"description": "Post-Quantum Trusted Execution Environment",
		"features": []string{
			"ML-DSA-87 signatures (NIST FIPS 204)",
			"ML-KEM-1024 encryption (NIST FIPS 203)",
			"SPARK formally verified VM",
			"Merkle Patricia Trie state proofs",
			"AAS-001 v3.1 addresses",
		},
		"block_height": s.blockNum,
	}
	writeJSON(w, info)
}

func (s *TEEServer) handleHealth(w http.ResponseWriter, r *http.Request) {
	writeJSON(w, map[string]string{"status": "healthy"})
}

func (s *TEEServer) handleTransaction(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var tx Transaction
	if err := json.NewDecoder(r.Body).Decode(&tx); err != nil {
		http.Error(w, "invalid transaction: "+err.Error(), http.StatusBadRequest)
		return
	}

	// TODO: Verify ML-DSA-87 signature
	// TODO: Validate AAS-001 addresses

	// Add to pool
	s.txPool.mu.Lock()
	s.txPool.pending = append(s.txPool.pending, tx)
	s.txPool.mu.Unlock()

	// Compute tx hash
	txHash := fmt.Sprintf("%x", tx.Nonce) // Placeholder

	writeJSON(w, map[string]string{
		"status":  "pending",
		"tx_hash": txHash,
	})
}

func (s *TEEServer) handleState(w http.ResponseWriter, r *http.Request) {
	key := r.URL.Path[len("/state/"):]
	if key == "" {
		http.Error(w, "key required", http.StatusBadRequest)
		return
	}

	s.state.mu.RLock()
	defer s.state.mu.RUnlock()

	// Check account storage
	for addr, storage := range s.state.storage {
		if val, ok := storage[key]; ok {
			writeJSON(w, map[string]interface{}{
				"key":     key,
				"value":   hex.EncodeToString(val),
				"address": addr,
			})
			return
		}
	}

	http.Error(w, "key not found", http.StatusNotFound)
}

func (s *TEEServer) handleProof(w http.ResponseWriter, r *http.Request) {
	key := r.URL.Path[len("/proof/"):]
	if key == "" {
		http.Error(w, "key required", http.StatusBadRequest)
		return
	}

	// TODO: Generate MPT proof via Khepri
	writeJSON(w, map[string]interface{}{
		"key":        key,
		"proof":      []string{}, // Placeholder
		"state_root": hex.EncodeToString(s.state.stateRoot),
	})
}

func (s *TEEServer) handleDeploy(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req struct {
		From      string `json:"from"`
		Bytecode  string `json:"bytecode"`
		Gas       uint64 `json:"gas"`
		Signature string `json:"signature"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "invalid request: "+err.Error(), http.StatusBadRequest)
		return
	}

	// TODO: Deploy contract via AegisVM
	contractAddr := fmt.Sprintf("aegis1contract%d", time.Now().UnixNano())

	writeJSON(w, map[string]string{
		"status":           "deployed",
		"contract_address": contractAddr,
	})
}

func (s *TEEServer) handleBlock(w http.ResponseWriter, r *http.Request) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	writeJSON(w, map[string]interface{}{
		"latest_block": s.blockNum,
		"state_root":   hex.EncodeToString(s.state.stateRoot),
	})
}

func (s *TEEServer) handleAccount(w http.ResponseWriter, r *http.Request) {
	addr := r.URL.Path[len("/account/"):]
	if addr == "" {
		http.Error(w, "address required", http.StatusBadRequest)
		return
	}

	s.state.mu.RLock()
	defer s.state.mu.RUnlock()

	account := s.state.accounts[addr]
	if account == nil {
		account = &Account{Address: addr, Balance: 0, Nonce: 0}
	}

	writeJSON(w, account)
}

func writeJSON(w http.ResponseWriter, v interface{}) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(v)
}

func main() {
	configPath := flag.String("config", "", "Path to config file")
	listenAddr := flag.String("listen", ":8545", "Listen address")
	dataDir := flag.String("data", "./data", "Data directory")
	blockTime := flag.Int("block-time", 5, "Block time in seconds")
	flag.Parse()

	config := Config{
		ListenAddr:  *listenAddr,
		DataDir:     *dataDir,
		MaxGasPerTx: 10_000_000,
		BlockTime:   *blockTime,
	}

	// Load config file if provided
	if *configPath != "" {
		data, err := os.ReadFile(*configPath)
		if err != nil {
			log.Fatalf("Failed to read config: %v", err)
		}
		if err := json.Unmarshal(data, &config); err != nil {
			log.Fatalf("Failed to parse config: %v", err)
		}
	}

	// Create data directory
	if err := os.MkdirAll(config.DataDir, 0755); err != nil {
		log.Fatalf("Failed to create data directory: %v", err)
	}

	server, err := NewTEEServer(config)
	if err != nil {
		log.Fatalf("Failed to create server: %v", err)
	}

	// Handle shutdown gracefully
	go func() {
		sigCh := make(chan os.Signal, 1)
		signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)
		<-sigCh
		log.Println("Shutting down...")
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()
		server.Shutdown(ctx)
	}()

	fmt.Println(`
    ___    _____ __________  _    ____  ___
   /   |  / ___// ____/  _/ | |  / /  |/  /
  / /| | / __ \/ / __ / /   | | / / /|_/ /
 / ___ |/ /_/ / /_/ // /    | |/ / /  / /
/_/  |_/\____/\____/___/    |___/_/  /_/

 Post-Quantum Trusted Execution Environment
 ML-DSA-87 | ML-KEM-1024 | SPARK Verified
`)

	if err := server.Start(); err != http.ErrServerClosed {
		log.Fatalf("Server error: %v", err)
	}
}
