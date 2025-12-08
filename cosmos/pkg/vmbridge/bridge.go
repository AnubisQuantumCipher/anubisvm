// VM Bridge - Interface between Cosmos SDK and SPARK/Ada VM
//
// This package provides the bridge between Go (Cosmos SDK) and the
// formally verified SPARK/Ada AegisVM implementation via CGO.

package vmbridge

/*
#cgo CFLAGS: -I${SRCDIR}/../../../core/include
#cgo LDFLAGS: -L${SRCDIR}/../../../core/lib -laegisvm -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib -lgnat -lgnarl -lm

#include "aegisvm.h"
#include <stdlib.h>
#include <string.h>
*/
import "C"

import (
	"errors"
	"sync"
	"unsafe"
)

// Error definitions
var (
	ErrVMCreate         = errors.New("vmbridge: failed to create VM instance")
	ErrVMDestroy        = errors.New("vmbridge: failed to destroy VM instance")
	ErrVMExecute        = errors.New("vmbridge: execution failed")
	ErrVMLoadCode       = errors.New("vmbridge: failed to load bytecode")
	ErrVMInvalidContext = errors.New("vmbridge: invalid execution context")
	ErrVMOutOfGas       = errors.New("vmbridge: out of gas")
	ErrVMStackOverflow  = errors.New("vmbridge: stack overflow")
	ErrVMInvalidOpcode  = errors.New("vmbridge: invalid opcode")
	ErrVMReverted       = errors.New("vmbridge: execution reverted")
	ErrVMStateAccess    = errors.New("vmbridge: state access violation")
)

// CertificationLevel represents contract certification
type CertificationLevel uint8

const (
	CertNone     CertificationLevel = 0
	CertBronze   CertificationLevel = 1
	CertSilver   CertificationLevel = 2
	CertGold     CertificationLevel = 3
	CertPlatinum CertificationLevel = 4
)

// ExecutionContext contains the context for contract execution
type ExecutionContext struct {
	// Origin is the original transaction sender
	Origin [32]byte

	// Caller is the immediate caller (could be another contract)
	Caller [32]byte

	// ContractAddress is the address of the contract being executed
	ContractAddress [20]byte

	// GasLimit is the maximum gas available
	GasLimit uint64

	// GasPrice is the gas price in uaegis
	GasPrice uint64

	// Value is the amount of tokens sent with the call
	Value uint64

	// BlockHeight is the current block height
	BlockHeight int64

	// BlockTime is the current block timestamp
	BlockTime int64

	// ChainID is the chain identifier
	ChainID string

	// CertLevel is the contract's certification level
	CertLevel CertificationLevel

	// IsStaticCall indicates if this is a read-only call
	IsStaticCall bool
}

// ExecutionResult contains the result of contract execution
type ExecutionResult struct {
	// Success indicates if execution completed without error
	Success bool

	// ReturnData is the return value from the contract
	ReturnData []byte

	// GasUsed is the amount of gas consumed
	GasUsed uint64

	// GasRefund is the gas to be refunded
	GasRefund uint64

	// Logs are events emitted during execution
	Logs []Log

	// StateChanges are the state modifications
	StateChanges []StateChange

	// Error contains any error message
	Error string
}

// Log represents an event emitted during execution
type Log struct {
	// Address is the contract that emitted the log
	Address [20]byte

	// Topics are indexed log parameters (up to 4)
	Topics [][32]byte

	// Data is the non-indexed log data
	Data []byte
}

// StateChange represents a state modification
type StateChange struct {
	// Key is the state key
	Key []byte

	// Value is the new value (nil for deletion)
	Value []byte

	// IsDelete indicates if this is a deletion
	IsDelete bool
}

// VMInstance represents an active VM instance
type VMInstance struct {
	handle unsafe.Pointer
	mu     sync.Mutex
}

// vmPool manages VM instances for reuse
var vmPool = sync.Pool{
	New: func() interface{} {
		return &VMInstance{}
	},
}

// NewVM creates a new VM instance
func NewVM() (*VMInstance, error) {
	vm := vmPool.Get().(*VMInstance)

	var handle C.aegis_vm_t
	result := C.aegis_vm_create(&handle)
	if result != C.AEGIS_OK {
		vmPool.Put(vm)
		return nil, ErrVMCreate
	}

	vm.handle = unsafe.Pointer(handle)
	return vm, nil
}

// Close releases the VM instance back to the pool
func (vm *VMInstance) Close() error {
	vm.mu.Lock()
	defer vm.mu.Unlock()

	if vm.handle != nil {
		result := C.aegis_vm_destroy(C.aegis_vm_t(vm.handle))
		vm.handle = nil
		vmPool.Put(vm)
		if result != C.AEGIS_OK {
			return ErrVMDestroy
		}
	}
	return nil
}

// LoadBytecode loads contract bytecode into the VM
func (vm *VMInstance) LoadBytecode(bytecode []byte) error {
	vm.mu.Lock()
	defer vm.mu.Unlock()

	if vm.handle == nil {
		return ErrVMInvalidContext
	}

	if len(bytecode) == 0 {
		return ErrVMLoadCode
	}

	result := C.aegis_vm_load_code(
		C.aegis_vm_t(vm.handle),
		(*C.uint8_t)(unsafe.Pointer(&bytecode[0])),
		C.size_t(len(bytecode)),
	)

	if result != C.AEGIS_OK {
		return ErrVMLoadCode
	}
	return nil
}

// Execute runs the loaded bytecode with the given context
func (vm *VMInstance) Execute(ctx ExecutionContext, function []byte, args []byte, state []byte) (*ExecutionResult, error) {
	vm.mu.Lock()
	defer vm.mu.Unlock()

	if vm.handle == nil {
		return nil, ErrVMInvalidContext
	}

	// Prepare C context
	var cCtx C.aegis_exec_context_t
	copy((*[32]byte)(unsafe.Pointer(&cCtx.origin[0]))[:], ctx.Origin[:])
	copy((*[32]byte)(unsafe.Pointer(&cCtx.caller[0]))[:], ctx.Caller[:])
	cCtx.gas_limit = C.uint64_t(ctx.GasLimit)
	cCtx.gas_price = C.uint64_t(ctx.GasPrice)
	cCtx.value = C.uint64_t(ctx.Value)
	cCtx.block_height = C.int64_t(ctx.BlockHeight)
	cCtx.block_time = C.int64_t(ctx.BlockTime)
	cCtx.cert_level = C.aegis_cert_level_t(ctx.CertLevel)
	if ctx.IsStaticCall {
		cCtx.is_static = C.int(1)
	} else {
		cCtx.is_static = C.int(0)
	}

	// Prepare input buffers
	var funcPtr *C.uint8_t
	if len(function) > 0 {
		funcPtr = (*C.uint8_t)(unsafe.Pointer(&function[0]))
	}

	var argsPtr *C.uint8_t
	if len(args) > 0 {
		argsPtr = (*C.uint8_t)(unsafe.Pointer(&args[0]))
	}

	var statePtr *C.uint8_t
	if len(state) > 0 {
		statePtr = (*C.uint8_t)(unsafe.Pointer(&state[0]))
	}

	// Prepare output buffers
	const maxOutputSize = 1024 * 1024 // 1 MB max output
	outputBuf := make([]byte, maxOutputSize)
	var outputLen C.size_t

	stateBuf := make([]byte, maxOutputSize)
	var stateLen C.size_t

	var gasUsed C.uint64_t

	// Execute
	result := C.aegis_vm_execute_with_context(
		C.aegis_vm_t(vm.handle),
		&cCtx,
		funcPtr, C.size_t(len(function)),
		argsPtr, C.size_t(len(args)),
		statePtr, C.size_t(len(state)),
		(*C.uint8_t)(unsafe.Pointer(&outputBuf[0])), &outputLen,
		(*C.uint8_t)(unsafe.Pointer(&stateBuf[0])), &stateLen,
		&gasUsed,
	)

	execResult := &ExecutionResult{
		GasUsed: uint64(gasUsed),
	}

	// Handle result
	switch result {
	case C.AEGIS_OK:
		execResult.Success = true
		if outputLen > 0 {
			execResult.ReturnData = make([]byte, outputLen)
			copy(execResult.ReturnData, outputBuf[:outputLen])
		}
		if stateLen > 0 {
			// Parse state changes
			execResult.StateChanges = parseStateChanges(stateBuf[:stateLen])
		}
	case C.AEGIS_ERROR_OUT_OF_GAS:
		execResult.Success = false
		execResult.Error = "out of gas"
		return execResult, ErrVMOutOfGas
	case C.AEGIS_ERROR_STACK_OVERFLOW:
		execResult.Success = false
		execResult.Error = "stack overflow"
		return execResult, ErrVMStackOverflow
	case C.AEGIS_ERROR_INVALID_OPCODE:
		execResult.Success = false
		execResult.Error = "invalid opcode"
		return execResult, ErrVMInvalidOpcode
	case C.AEGIS_ERROR_REVERTED:
		execResult.Success = false
		if outputLen > 0 {
			execResult.ReturnData = make([]byte, outputLen)
			copy(execResult.ReturnData, outputBuf[:outputLen])
		}
		execResult.Error = "execution reverted"
		return execResult, ErrVMReverted
	default:
		execResult.Success = false
		execResult.Error = "execution failed"
		return execResult, ErrVMExecute
	}

	return execResult, nil
}

// SetState sets the contract state
func (vm *VMInstance) SetState(key, value []byte) error {
	vm.mu.Lock()
	defer vm.mu.Unlock()

	if vm.handle == nil {
		return ErrVMInvalidContext
	}

	var keyPtr, valuePtr *C.uint8_t
	if len(key) > 0 {
		keyPtr = (*C.uint8_t)(unsafe.Pointer(&key[0]))
	}
	if len(value) > 0 {
		valuePtr = (*C.uint8_t)(unsafe.Pointer(&value[0]))
	}

	result := C.aegis_vm_set_state(
		C.aegis_vm_t(vm.handle),
		keyPtr, C.size_t(len(key)),
		valuePtr, C.size_t(len(value)),
	)

	if result != C.AEGIS_OK {
		return ErrVMStateAccess
	}
	return nil
}

// GetState retrieves contract state
func (vm *VMInstance) GetState(key []byte) ([]byte, error) {
	vm.mu.Lock()
	defer vm.mu.Unlock()

	if vm.handle == nil {
		return nil, ErrVMInvalidContext
	}

	var keyPtr *C.uint8_t
	if len(key) > 0 {
		keyPtr = (*C.uint8_t)(unsafe.Pointer(&key[0]))
	}

	const maxValueSize = 32 * 1024 // 32 KB max value
	valueBuf := make([]byte, maxValueSize)
	var valueLen C.size_t

	result := C.aegis_vm_get_state(
		C.aegis_vm_t(vm.handle),
		keyPtr, C.size_t(len(key)),
		(*C.uint8_t)(unsafe.Pointer(&valueBuf[0])), &valueLen,
	)

	if result != C.AEGIS_OK {
		return nil, ErrVMStateAccess
	}

	if valueLen == 0 {
		return nil, nil
	}

	value := make([]byte, valueLen)
	copy(value, valueBuf[:valueLen])
	return value, nil
}

// Reset resets the VM state for reuse
func (vm *VMInstance) Reset() error {
	vm.mu.Lock()
	defer vm.mu.Unlock()

	if vm.handle == nil {
		return ErrVMInvalidContext
	}

	result := C.aegis_vm_reset(C.aegis_vm_t(vm.handle))
	if result != C.AEGIS_OK {
		return ErrVMExecute
	}
	return nil
}

// parseStateChanges parses state changes from the output buffer
func parseStateChanges(data []byte) []StateChange {
	if len(data) == 0 {
		return nil
	}

	var changes []StateChange
	offset := 0

	for offset < len(data) {
		// Each state change: [1 byte delete flag][4 bytes key len][key][4 bytes value len][value]
		if offset+1 > len(data) {
			break
		}
		isDelete := data[offset] == 1
		offset++

		if offset+4 > len(data) {
			break
		}
		keyLen := int(data[offset])<<24 | int(data[offset+1])<<16 | int(data[offset+2])<<8 | int(data[offset+3])
		offset += 4

		if offset+keyLen > len(data) {
			break
		}
		key := make([]byte, keyLen)
		copy(key, data[offset:offset+keyLen])
		offset += keyLen

		var value []byte
		if !isDelete {
			if offset+4 > len(data) {
				break
			}
			valueLen := int(data[offset])<<24 | int(data[offset+1])<<16 | int(data[offset+2])<<8 | int(data[offset+3])
			offset += 4

			if offset+valueLen > len(data) {
				break
			}
			value = make([]byte, valueLen)
			copy(value, data[offset:offset+valueLen])
			offset += valueLen
		}

		changes = append(changes, StateChange{
			Key:      key,
			Value:    value,
			IsDelete: isDelete,
		})
	}

	return changes
}

// ValidateBytecode validates contract bytecode before loading
func ValidateBytecode(bytecode []byte) error {
	if len(bytecode) == 0 {
		return errors.New("vmbridge: empty bytecode")
	}

	result := C.aegis_validate_bytecode(
		(*C.uint8_t)(unsafe.Pointer(&bytecode[0])),
		C.size_t(len(bytecode)),
	)

	if result != C.AEGIS_OK {
		return errors.New("vmbridge: invalid bytecode format")
	}
	return nil
}

// EstimateGas estimates gas consumption for an execution
func EstimateGas(bytecode []byte, function []byte, args []byte) (uint64, error) {
	if len(bytecode) == 0 {
		return 0, errors.New("vmbridge: empty bytecode")
	}

	var funcPtr, argsPtr *C.uint8_t
	if len(function) > 0 {
		funcPtr = (*C.uint8_t)(unsafe.Pointer(&function[0]))
	}
	if len(args) > 0 {
		argsPtr = (*C.uint8_t)(unsafe.Pointer(&args[0]))
	}

	var gasEstimate C.uint64_t
	result := C.aegis_estimate_gas(
		(*C.uint8_t)(unsafe.Pointer(&bytecode[0])),
		C.size_t(len(bytecode)),
		funcPtr, C.size_t(len(function)),
		argsPtr, C.size_t(len(args)),
		&gasEstimate,
	)

	if result != C.AEGIS_OK {
		return 0, errors.New("vmbridge: gas estimation failed")
	}
	return uint64(gasEstimate), nil
}

// GasDiscount returns the gas discount percentage for a certification level
// Aligned with KHEPRI Blueprint v1.0, Section 4: Gas Model
//
// Discount factors:
//   Bronze:   0% discount (1.0x multiplier)
//   Silver:  10% discount (0.9x multiplier)
//   Gold:    20% discount (0.8x multiplier)
//   Platinum: 30% discount (0.7x multiplier)
func GasDiscount(level CertificationLevel) uint64 {
	switch level {
	case CertBronze:
		return 0 // 0% discount - baseline
	case CertSilver:
		return 10 // 10% discount
	case CertGold:
		return 20 // 20% discount
	case CertPlatinum:
		return 30 // 30% discount
	default:
		return 0
	}
}

// ApplyGasDiscount applies the certification discount to a gas amount
func ApplyGasDiscount(gas uint64, level CertificationLevel) uint64 {
	discount := GasDiscount(level)
	return gas * (100 - discount) / 100
}
