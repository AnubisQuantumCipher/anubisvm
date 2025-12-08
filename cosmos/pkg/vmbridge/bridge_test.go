// VM Bridge Integration Tests
//
// Tests for the CGO bridge between Go and SPARK/Ada VM implementation.
// These tests require the libaegisvm shared library to be built and available.

package vmbridge

import (
	"testing"
)

// TestNewVM tests VM instance creation
func TestNewVM(t *testing.T) {
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("NewVM() failed: %v", err)
	}
	defer vm.Close()

	if vm.handle == nil {
		t.Fatal("VM handle is nil after creation")
	}
}

// TestVMReset tests VM reset functionality
func TestVMReset(t *testing.T) {
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("NewVM() failed: %v", err)
	}
	defer vm.Close()

	err = vm.Reset()
	if err != nil {
		t.Fatalf("Reset() failed: %v", err)
	}
}

// TestLoadBytecode tests bytecode loading
func TestLoadBytecode(t *testing.T) {
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("NewVM() failed: %v", err)
	}
	defer vm.Close()

	// Sample bytecode
	bytecode := []byte{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07}

	err = vm.LoadBytecode(bytecode)
	if err != nil {
		t.Fatalf("LoadBytecode() failed: %v", err)
	}
}

// TestLoadEmptyBytecode tests that empty bytecode is rejected
func TestLoadEmptyBytecode(t *testing.T) {
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("NewVM() failed: %v", err)
	}
	defer vm.Close()

	err = vm.LoadBytecode([]byte{})
	if err != ErrVMLoadCode {
		t.Fatalf("LoadBytecode() should fail with empty bytecode, got: %v", err)
	}
}

// TestSetGetState tests state management
func TestSetGetState(t *testing.T) {
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("NewVM() failed: %v", err)
	}
	defer vm.Close()

	key := []byte("test_key")
	value := []byte("test_value_12345")

	// Set state
	err = vm.SetState(key, value)
	if err != nil {
		t.Fatalf("SetState() failed: %v", err)
	}

	// Get state
	gotValue, err := vm.GetState(key)
	if err != nil {
		t.Fatalf("GetState() failed: %v", err)
	}

	if string(gotValue) != string(value) {
		t.Fatalf("GetState() returned %q, want %q", gotValue, value)
	}
}

// TestGetNonExistentState tests getting non-existent key
func TestGetNonExistentState(t *testing.T) {
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("NewVM() failed: %v", err)
	}
	defer vm.Close()

	value, err := vm.GetState([]byte("nonexistent_key"))
	if err != nil {
		t.Fatalf("GetState() failed: %v", err)
	}

	if value != nil {
		t.Fatalf("GetState() should return nil for non-existent key, got: %v", value)
	}
}

// TestValidateBytecode tests bytecode validation
func TestValidateBytecode(t *testing.T) {
	bytecode := []byte{0x00, 0x01, 0x02, 0x03}

	err := ValidateBytecode(bytecode)
	if err != nil {
		t.Fatalf("ValidateBytecode() failed: %v", err)
	}
}

// TestValidateEmptyBytecode tests that empty bytecode is rejected
func TestValidateEmptyBytecode(t *testing.T) {
	err := ValidateBytecode([]byte{})
	if err == nil {
		t.Fatal("ValidateBytecode() should fail with empty bytecode")
	}
}

// TestEstimateGas tests gas estimation
func TestEstimateGas(t *testing.T) {
	bytecode := make([]byte, 100)

	gas, err := EstimateGas(bytecode, nil, nil)
	if err != nil {
		t.Fatalf("EstimateGas() failed: %v", err)
	}

	// Expected: 21000 + (100 * 16) = 22600
	if gas != 22600 {
		t.Fatalf("EstimateGas() returned %d, want 22600", gas)
	}
}

// TestGasDiscount tests certification gas discounts
func TestGasDiscount(t *testing.T) {
	tests := []struct {
		level    CertificationLevel
		expected uint64
	}{
		{CertNone, 0},
		{CertBronze, 5},
		{CertSilver, 15},
		{CertGold, 25},
		{CertPlatinum, 35},
	}

	for _, tt := range tests {
		got := GasDiscount(tt.level)
		if got != tt.expected {
			t.Errorf("GasDiscount(%d) = %d, want %d", tt.level, got, tt.expected)
		}
	}
}

// TestApplyGasDiscount tests gas discount application
func TestApplyGasDiscount(t *testing.T) {
	baseGas := uint64(100000)

	tests := []struct {
		level    CertificationLevel
		expected uint64
	}{
		{CertNone, 100000},     // 0% discount
		{CertBronze, 95000},    // 5% discount
		{CertSilver, 85000},    // 15% discount
		{CertGold, 75000},      // 25% discount
		{CertPlatinum, 65000},  // 35% discount
	}

	for _, tt := range tests {
		got := ApplyGasDiscount(baseGas, tt.level)
		if got != tt.expected {
			t.Errorf("ApplyGasDiscount(%d, %d) = %d, want %d",
				baseGas, tt.level, got, tt.expected)
		}
	}
}

// TestExecutionContext tests execution context creation
func TestExecutionContext(t *testing.T) {
	ctx := ExecutionContext{
		GasLimit:    10000000,
		GasPrice:    1,
		Value:       0,
		BlockHeight: 12345,
		BlockTime:   1699999999,
		ChainID:     "aegis-1",
		CertLevel:   CertGold,
		IsStaticCall: false,
	}

	// Just verify the struct can be created without issues
	if ctx.GasLimit != 10000000 {
		t.Fatal("ExecutionContext field not set correctly")
	}
}

// TestExecutionResult tests execution result structure
func TestExecutionResult(t *testing.T) {
	result := ExecutionResult{
		Success:    true,
		ReturnData: []byte{0x01, 0x02},
		GasUsed:    1000,
		GasRefund:  0,
		Error:      "",
	}

	if !result.Success {
		t.Fatal("ExecutionResult.Success should be true")
	}

	if result.GasUsed != 1000 {
		t.Fatal("ExecutionResult.GasUsed not set correctly")
	}
}

// TestVMPooling tests VM instance pooling
func TestVMPooling(t *testing.T) {
	// Create and close multiple VMs to test pooling
	for i := 0; i < 10; i++ {
		vm, err := NewVM()
		if err != nil {
			t.Fatalf("NewVM() failed on iteration %d: %v", i, err)
		}

		err = vm.Close()
		if err != nil {
			t.Fatalf("Close() failed on iteration %d: %v", i, err)
		}
	}
}

// TestConcurrentVMAccess tests concurrent VM access
func TestConcurrentVMAccess(t *testing.T) {
	vm, err := NewVM()
	if err != nil {
		t.Fatalf("NewVM() failed: %v", err)
	}
	defer vm.Close()

	done := make(chan bool, 10)

	// Concurrent state operations
	for i := 0; i < 10; i++ {
		go func(id int) {
			key := []byte{byte(id)}
			value := []byte{byte(id * 2)}

			_ = vm.SetState(key, value)
			_, _ = vm.GetState(key)

			done <- true
		}(i)
	}

	// Wait for all goroutines
	for i := 0; i < 10; i++ {
		<-done
	}
}
