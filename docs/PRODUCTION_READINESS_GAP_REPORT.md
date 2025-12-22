# AnubisVM Production Readiness Gap Report

Generated: 2025-12-22
Status: **PROTOTYPE** (not production-ready)

---

## Executive Summary

AnubisVM is a native ELF contract execution environment with two execution modes:
- **Exec_Mode_InProcess**: Direct native execution (fast, requires trusted contracts)
- **Exec_Mode_Sandboxed**: Fork+sandbox subprocess with syscall IPC (secure)

The architecture is sound and the security mindset is present. However, several gaps
must be closed before production deployment.

---

## Execution Model Decision

**CHOSEN: (B) Native ELF out-of-process (Exec_Mode_Sandboxed)**

Rationale:
- Native ELF provides maximum performance for SPARK-verified contracts
- Process isolation provides defense-in-depth against contract escape
- Syscall IPC enables proper gas metering and state management
- Seatbelt (macOS) / seccomp-bpf (Linux) provide OS-level enforcement

---

## Priority 1: CRITICAL (Must Fix Before Any Deployment)

### 1.1 Complete Sandbox Enforcement
**Files**: `sphinx_subprocess.adb`
**Status**: COMPLETED (macOS), PENDING (Linux)

| Item | Current | Required |
|------|---------|----------|
| Fork isolation | Yes | Yes |
| Seatbelt profile (macOS) | **DONE - Strict deny-by-default** | Comprehensive deny-by-default |
| seccomp-bpf (Linux) | Stubbed | Full implementation |
| Resource limits | Yes (rlimit) | Yes |
| Syscall allowlist | **DONE - Explicit in Seatbelt** | Explicit minimal set |

**Completed** (commit 68799b3):
- Strict Seatbelt profile with DENY-BY-DEFAULT
- Explicit denies: process-exec, process-fork, network*, signal, ipc-*, system*
- Minimal allows: sysctl-read, /dev/urandom, /dev/null, pipe I/O, process-exit

**Remaining**:
- Implement seccomp-bpf filter for Linux

### 1.2 Symbol Resolution Policy Enforcement
**Files**: `sphinx_native.adb`, `sphinx_elf_loader.adb`
**Status**: COMPLETED

The policy is now enforced: **contracts MUST be statically linked**.

**Completed** (commit 68799b3):
- PT_DYNAMIC program headers scanned for DT_NEEDED entries
- SHT_DYNAMIC sections rejected
- SHT_DYNSYM (dynamic symbol table) rejected
- Contracts with external dependencies fail at load time

### 1.3 Entry Function ABI Validation
**Files**: `sphinx_elf_loader.adb:~849`
**Status**: Assumes correct ABI

The contract entry function has a specific ABI:
```c
int entry(void* calldata, size_t len, void* ret_buf, size_t* ret_len,
          uint64_t gas_limit, uint64_t* gas_used);
```

**Action Items**:
- Validate entry function signature via symbol table analysis
- Reject contracts where entry doesn't match expected signature
- Add runtime guard for return buffer overflow (4096 byte limit)

---

## Priority 2: HIGH (Required Before Beta)

### 2.1 Syscall Handler Completeness
**Files**: `sphinx_subprocess.adb`
**Status**: Core syscalls implemented, some gaps

| Syscall | Code | Status |
|---------|------|--------|
| SLOAD | 0x01 | Implemented |
| SSTORE | 0x02 | Implemented |
| SHA3 | 0x10 | Implemented |
| KECCAK256 | 0x11 | Missing |
| ML_DSA_Sign | 0x20 | Implemented |
| ML_DSA_Vrfy | 0x21 | Implemented |
| ML_KEM_Enc | 0x30 | Implemented |
| ML_KEM_Dec | 0x31 | Implemented |
| LOG0-4 | 0x40-0x44 | Partial |
| REVERT | 0x50 | Implemented |
| RETURN | 0x51 | **Implemented** |
| CALL | 0x60 | Missing (cross-contract) |
| DELEGATECALL | 0x61 | Missing |
| STATICCALL | 0x62 | Missing |
| CREATE | 0x70 | Missing |
| CREATE2 | 0x71 | Missing |
| SELFBALANCE | 0x80 | Missing |
| BALANCE | 0x81 | Missing |
| BLOCKHASH | 0x90 | Missing |
| TIMESTAMP | 0x91 | Missing |
| CHAINID | 0x92 | Missing |

**Action Items**:
```ada
-- Add missing syscall handlers to Handle_Syscall_Request
-- Prioritize: RETURN, CALL, BALANCE, TIMESTAMP, CHAINID
```

### 2.2 Gas Metering Accuracy
**Files**: `aegis_gas.ads/adb`, `sphinx_subprocess.adb`
**Status**: Basic metering exists

**Gaps**:
- Gas costs for syscalls need calibration against actual execution time
- Memory expansion costs not implemented
- Cross-contract call gas forwarding not implemented

**Action Items**:
- Benchmark syscall handlers and set accurate gas costs
- Implement memory expansion gas formula: `gas = 3 * words + words^2 / 512`
- Add gas stipend handling for CALL/DELEGATECALL

### 2.3 State Commitment
**Files**: `aegis_storage.ads/adb`, `khepri_mpt.ads/adb`
**Status**: MPT exists but integration incomplete

**Action Items**:
- Wire storage operations through MPT for state root computation
- Implement storage proof generation for light clients
- Add state snapshot/restore for transaction rollback

---

## Priority 3: MEDIUM (Required Before Mainnet)

### 3.1 Privacy Layer Implementation
**Files**: `aegis_privacy.ads/adb`
**Status**: API defined, implementation has placeholders

**Known Placeholders**:
- XOR-based "homomorphic addition" at line ~466 (not real crypto)
- Ring signature verification incomplete
- Stealth address derivation needs validation

**Action Items**:
- Replace placeholder crypto with real implementations
- Integrate with Anubis_Whisper for confidential transactions
- Add formal verification of privacy properties

### 3.2 Error Recovery and Logging
**Files**: `execution_errors.ads/adb`
**Status**: Error codes exist, recovery paths incomplete

**Action Items**:
- Add structured logging with correlation IDs
- Implement graceful degradation for sandbox failures
- Add crash dump generation for debugging

### 3.3 Test Harness
**Status**: Basic tests exist, coverage insufficient

**Required Tests**:
- [ ] ELF parsing fuzzing
- [ ] mprotect layout verification
- [ ] Gas accounting accuracy tests
- [ ] Malicious contract tests (attempt syscall, overflow, etc.)
- [ ] Sandbox escape tests
- [ ] IPC protocol fuzzing
- [ ] Cross-contract call tests

---

## Priority 4: LOW (Nice to Have)

### 4.1 Performance Optimization
- Implement connection pooling for RPC handlers
- Add contract bytecode caching
- Optimize IPC serialization

### 4.2 Observability
- Add Prometheus metrics export
- Implement distributed tracing
- Add contract execution profiling

### 4.3 Documentation
- API reference generation
- Contract development guide
- Security audit preparation docs

---

## File-by-File Status

| File | LOC | Status | Notes |
|------|-----|--------|-------|
| sphinx_native.ads | 732 | Fixed | Spec/body mismatch corrected |
| sphinx_native.adb | 2007 | OK | Syscall scanning integrated |
| sphinx_subprocess.ads | ~200 | OK | IPC protocol defined |
| sphinx_subprocess.adb | ~500 | OK | ML-DSA/ML-KEM implemented |
| sphinx_elf_loader.ads | 456 | OK | - |
| sphinx_elf_loader.adb | 1123 | Fixed | Symbol resolution clarified |
| aegis_vm_types.ads | 448 | OK | - |
| aegis_syscall.adb | 3816 | OK | - |
| aegis_privacy.adb | 2356 | WARN | Has placeholders |
| aegis_storage.adb | 125 | WARN | Body smaller than expected |

---

## Minimum Viable Security Checklist

Before deploying even for testing with untrusted contracts:

- [x] Syscall scanning at load time
- [x] W^X enforcement on memory pages
- [x] Process isolation (fork)
- [x] Resource limits (rlimit)
- [x] Strict syscall allowlist (Seatbelt on macOS) **DONE**
- [x] Dynamic symbol rejection **DONE**
- [ ] Entry ABI validation
- [x] Execution timeout
- [x] Gas metering (basic)
- [ ] State rollback on failure
- [ ] seccomp-bpf (Linux only)

---

## Recommended Next Steps

1. **Immediate** (COMPLETED 2025-12-22):
   - ~~Add DT_NEEDED / dynamic symbol rejection to Load_ELF~~ DONE
   - ~~Implement strict Seatbelt profile for macOS~~ DONE
   - ~~Add RETURN syscall handler~~ DONE

2. **Short-term** (next priority):
   - Implement seccomp-bpf for Linux (critical for Linux deployment)
   - Add CALL/STATICCALL syscalls for cross-contract calls
   - Create malicious contract test suite
   - Add entry function ABI validation

3. **Medium-term**:
   - Replace privacy layer placeholders
   - Implement full state commitment via MPT
   - Add remaining blockchain syscalls (BALANCE, TIMESTAMP, CHAINID)
   - Prepare for security audit

---

## Conclusion

AnubisVM has made significant progress on security hardening. The subprocess
sandbox with syscall IPC is the correct design choice.

**Completed (Priority 1)**:
1. Sandbox hardening (macOS) - Strict deny-by-default Seatbelt profile
2. Contract validation - Static linking enforced at load time
3. RETURN syscall - Contracts can now return data properly

**Remaining gaps**:
1. **Linux support** - seccomp-bpf filter not yet implemented
2. **Syscall completeness** - Cross-contract calls and balance queries missing
3. **Privacy layer** - Current implementation has placeholders
4. **Entry ABI validation** - Contract entry signature not yet validated

The VM is now suitable for controlled testing on macOS with statically-linked
contracts. Production deployment requires completing Linux support and passing
a security audit.
