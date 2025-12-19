# AnubisVM

Post-quantum smart contract virtual machine with SPARK formal verification.

Contracts compile to native ELF binaries. All cryptography proven memory-safe via GNATprove.

## Verification Status

| Component | Subprograms | Status |
|-----------|-------------|--------|
| VM Core (aegis_*) | 149 | Platinum - 100% proven |
| Cryptographic Primitives | 89 | Platinum - 100% proven |
| CVM Runtime | 56 | Platinum - 100% proven |
| State Management | 34 | Platinum - 100% proven |
| **Total** | **493** | **100% verified** |

Platinum certification includes: ghost functions, model abstractions, lemma subprograms, biconditional postconditions, and full NRTE (No Run-Time Errors) proofs.

## Installation

### Requirements

- **Alire** 2.0+ (Ada package manager)
- **macOS** 12+ or **Linux** (x86_64, aarch64)

### Install from Source

```bash
# Clone
git clone https://github.com/AnubisQuantumCipher/anubisvm.git
cd anubisvm

# Install Alire if not present
# macOS:
brew install alire
# Linux (Debian/Ubuntu):
wget https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-x86_64-linux.zip
unzip alr-2.0.1-bin-x86_64-linux.zip && sudo mv bin/alr /usr/local/bin/

# Build
make setup
make build

# Install CLI tools to ~/.local/bin
make install

# Add to PATH (add to ~/.bashrc or ~/.zshrc)
export PATH="$HOME/.local/bin:$PATH"

# Verify
khepri version
```

### Binaries Installed

| Binary | Purpose |
|--------|---------|
| `khepri` | Contract development CLI |
| `anubis-node` | Full blockchain node |
| `khepri-local` | Local development node |

Data directory: `~/.anubisvm/`

## CLI Reference

### Key Management

```bash
# Generate ML-DSA-87 keypair
khepri keys new <name>

# List keys
khepri keys list

# Show key details
khepri keys show <name>

# Derive address from key
khepri address from-key <name>
```

### Contract Operations

```bash
# Deploy contract
khepri deploy <path> --from <key> --gas <limit>

# Call read-only method
khepri call <address> <method> --args "<args>"

# Send transaction
khepri send <address> <method> --args "<args>" --from <key> --gas <limit>
```

### Node Operations

```bash
# Start local development node
khepri-local

# Initialize full node
anubis-node init --home ~/.anubisvm

# Start full node
anubis-node start --home ~/.anubisvm
```

### Encoding/Decoding

```bash
# Encode value
khepri encode <type> <value>
# Types: uint256, address, string, bytes

# Decode hex
khepri decode <type> <hex>
```

## Building Contracts

Contracts are SPARK/Ada packages compiled to native ELF binaries (CVM - Cryptographically Verified Modules).

### Contract Structure

```
my_contract/
  my_contract.ads    # Specification (interface)
  my_contract.adb    # Implementation
  my_contract.gpr    # GNAT project file
```

### Build and Verify

```bash
cd contracts/my_contract

# Build
alr build

# Verify with SPARK (level 2)
alr exec -- gnatprove -P my_contract.gpr --level=2

# Full verification (level 4)
alr exec -- gnatprove -P my_contract.gpr --level=4
```

### Deploy

```bash
khepri keys new deployer
khepri deploy contracts/my_contract/obj/my_contract --from deployer --gas 1000000
```

## Cryptographic Primitives

| Primitive | Standard | Implementation |
|-----------|----------|----------------|
| ML-DSA-87 | NIST FIPS 204 | Pure SPARK, KAT verified |
| ML-KEM-1024 | NIST FIPS 203 | Pure SPARK, KAT verified |
| SHA3-256/512 | NIST FIPS 202 | Pure SPARK, KAT verified |
| SHAKE128/256 | NIST FIPS 202 | Pure SPARK, KAT verified |
| KMAC256 | NIST SP 800-185 | Pure SPARK, Platinum proven |
| AEAD | ChaCha20-Poly1305 | Platinum proven |
| KDF | HKDF-SHA3-256 | Platinum proven |

All cryptographic code passes NIST Known Answer Tests (KAT) for interoperability.

## Address Format (AAS-001 v3.1)

```
mldsa87:network:type:payload-checksum
```

Components:
- `mldsa87` - Signature algorithm identifier
- `network` - `main`, `test`, or `staging`
- `type` - `u` (user), `c` (contract), `v` (validator)
- `payload` - Base32-encoded public key hash (chunked with `-`)
- `checksum` - 5-character verification code

Example:
```
mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-abc12
```

## Build Targets

```bash
make                  # Build all (development)
make build-release    # Optimized release build
make test             # Run all tests
make prove            # SPARK verification (level 2)
make prove-full       # Full SPARK verification (level 4)
make prove-report     # Show proof statistics
make install          # Install to ~/.local/bin
make uninstall        # Remove installed binaries
make clean            # Remove build artifacts
make help             # Show all targets
```

## Test Suite

```bash
# Run all tests
make test

# Run specific test categories
core/bin/test_kmac      # KMAC primitives (9 tests)
core/bin/test_aead      # AEAD encryption (10 tests)
core/bin/test_kdf       # Key derivation (12 tests)
core/bin/test_cvm       # CVM runtime (56 tests)
core/bin/test_anubis    # Core VM (67 tests)
core/bin/test_state     # State management
core/bin/test_tee       # TEE operations

# KAT tests
tests/bin/test_sha3_kat    # SHA3 NIST vectors
tests/bin/test_mldsa       # ML-DSA-87 NIST vectors
tests/bin/test_mlkem_kat   # ML-KEM-1024 NIST vectors
tests/bin/test_address     # Address encoding
```

## Architecture

```
anubisvm/
  core/
    src/
      aegis/         # Address encoding (AAS-001)
      cvm/           # CVM types, interface, registry, dispatch
      crypto/        # KMAC, AEAD, KDF
        aead/        # Authenticated encryption
        kdf/         # Key derivation
        kmac/        # Keyed MAC
      hash/          # SHA3, Keccak, SHAKE
      pqc/           # Post-quantum cryptography
        dsa/         # ML-DSA-87 signatures
        kem/         # ML-KEM-1024 encapsulation
      state/         # Khepri MPT, persistence
      tee/           # TEE keys, attestation, runtime
      vm/            # Execution engine
        aegis_execution.ads   # Context management
        aegis_gas.ads         # Gas metering
        aegis_sandbox.ads     # Syscall validation
        aegis_storage.ads     # State storage
        aegis_syscall.ads     # Syscall dispatch
    bin/             # Compiled binaries
  cli/               # CLI source
  contracts/         # Example contracts
  tests/             # Test suite
```

## VM Execution Model

### Gas Metering

| Operation | Base Cost | Notes |
|-----------|-----------|-------|
| Storage read | 200 | +2100 cold access |
| Storage write | 5000 | +15000 new slot |
| Contract call | 700 | +2600 cold account |
| Memory expand | 3 | Per 32-byte word |

### Certification Discounts

| Level | Discount | Requirements |
|-------|----------|--------------|
| Bronze | 5% | Basic SPARK mode |
| Silver | 10% | Flow analysis clean |
| Gold | 20% | Level 2 proofs |
| Platinum | 30% | Level 4 proofs, functional contracts |

### Syscall Interface

Contracts interact with the VM through numbered syscalls:

| Number | Name | Description |
|--------|------|-------------|
| 0x01 | SLOAD | Load storage slot |
| 0x02 | SSTORE | Store to slot |
| 0x10 | CALL | Call contract |
| 0x11 | DELEGATECALL | Delegate call |
| 0x20 | LOG0-LOG4 | Emit events |
| 0x30 | RETURN | Return data |
| 0x31 | REVERT | Revert execution |

## SPARK Verification

### Running Proofs

```bash
# Quick verification (recommended during development)
make prove

# Full verification (CI/release)
make prove-full

# View detailed report
cat obj/gnatprove/gnatprove.out
```

### Verification Levels

| Level | Description | Use Case |
|-------|-------------|----------|
| 1 | Flow analysis | Fast iteration |
| 2 | Standard proofs | Development |
| 3 | Extended proofs | Pre-release |
| 4 | Full proofs | Production/audit |

### Platinum Contracts

VM core packages with Platinum certification:

- `aegis_execution.ads` - Execution context management
- `aegis_gas.ads` - Gas metering with lemma subprograms
- `aegis_sandbox.ads` - Syscall validation with ghost functions
- `aegis_storage.ads` - State storage with model abstractions
- `aegis_syscall.ads` - Syscall dispatch
- `aegis_contract.ads` - Contract interface
- `aegis_vm_types.ads` - Core type definitions

## Development

### Running Tests After Changes

```bash
make build && make test
```

### Adding New Contracts

```bash
mkdir -p contracts/my_contract
# Create .ads, .adb, .gpr files
# See contracts/sovereign_token/ for example
```

### Debugging

```bash
# Build with debug symbols
make build

# Run individual test with output
DYLD_LIBRARY_PATH=core/lib:~/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib \
  core/bin/test_cvm
```

## License

Apache 2.0
