# AnubisVM

**Formally Verified Post-Quantum Smart Contract Virtual Machine**

AnubisVM is a formally verified virtual machine for executing smart contracts with post-quantum cryptographic security. Built using SPARK/Ada with mathematical proof of correctness.

## 🎯 Design Goals

1. **NIST Level 5 Security**: ML-KEM-1024 and ML-DSA-87 only
2. **Formal Verification**: 100% SPARK proof coverage (Gold standard)
3. **Constant-Time Cryptography**: No timing side-channels
4. **Reproducible Builds**: Docker-pinned toolchains with hash verification
5. **Governance by PQ Threshold Signatures**: 3-of-5 ML-DSA multisig

## 🏗️ Architecture

```
AnubisVM/
├── core/           # Core VM and cryptographic primitives
│   ├── primitives/ # Constant-time word operations
│   ├── hash/       # Keccak, SHA3, SHAKE
│   ├── pqc/        # ML-KEM-1024, ML-DSA-87
│   ├── vm/         # Bytecode interpreter
│   ├── storage/    # Merkle Patricia Trie
│   └── governance/ # Threshold signatures
├── tests/          # KAT, unit, and integration tests
└── docs/           # Specifications and proofs
```

## 🚀 Quick Start

### Prerequisites

- **GNAT FSF 14.x** or **GNAT Pro 25.x**
- **SPARK 2014** (included with GNAT Pro or via community edition)
- **Alire** (Ada package manager)

### Build

```bash
# Development build
make build

# Build for formal verification
make build-prove

# Run SPARK proofs
make prove

# Quick proof check (level 2)
make prove-quick
```

## 📊 Project Status

**Current Phase**: Phase 1 - Constant-Time Primitives

### Phase Completion

- [x] Phase 0: Project structure and build system
- [ ] Phase 1: Constant-time primitives (in progress)
- [ ] Phase 2: Keccak/SHA3/SHAKE
- [ ] Phase 3: NTT with verified twiddles
- [ ] Phase 4: Constant-time CBD sampling
- [ ] Phase 5: Complete ML-KEM-1024
- [ ] Phase 6: VM architecture
- [ ] Phase 7: Storage layer
- [ ] Phase 8: Governance
- [ ] Phase 9: Testing infrastructure
- [ ] Phase 10: CI/CD
- [ ] Phase 11: Documentation

## 🔒 Security Model

### Cryptographic Primitives

- **Key Encapsulation**: ML-KEM-1024 (NIST FIPS 203)
- **Digital Signatures**: ML-DSA-87 (NIST FIPS 204)
- **Hashing**: SHA3-256/384/512, SHAKE128/256 (NIST FIPS 202)

### Formal Verification Target: Gold

- **NRTE**: No runtime errors (bounds, overflow, division by zero)
- **Functional Properties**: Key cryptographic invariants proven
- **Zeroization**: Sensitive data cleared (proven via postconditions)
- **Constant-Time**: Manual audit + statistical timing tests

## 📖 Documentation

- [Bytecode Specification](docs/spec/bytecode_spec.md) (coming in Phase 6)
- [Gas Schedule](docs/spec/gas_schedule.md) (coming in Phase 6)
- [Security Model](docs/spec/security_model.md) (coming in Phase 11)
- [Mathematical Derivations](docs/math/) (coming in Phase 3)

## 🤝 Contributing

AnubisVM is part of the Anubis Project ecosystem. Contributions welcome!

1. All code must be in SPARK/Ada with `pragma SPARK_Mode (On)`
2. All public subprograms must have Pre/Post conditions
3. All loops must have invariants
4. All changes must pass `make prove` at level 4
5. KAT tests required for all cryptographic code

## 📜 License

Apache 2.0

## 🔗 Related Projects

- [Anubis ML-KEM](https://github.com/AnubisQuantumCipher/mlkem_1024)
- [Anubis Argon2id](https://github.com/AnubisQuantumCipher/spark_argon2id)

---

**Status**: Active Development | **Target**: Production Ready v1.0 (30 weeks)
