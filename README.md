# AnubisVM

Formally verified post-quantum smart contract virtual machine built in SPARK/Ada.

## Overview

AnubisVM executes Cryptographically Verified Modules (CVMs) - native SPARK packages that are mathematically proven correct at compile time. Unlike bytecode interpreters, CVMs are statically linked into the TEE binary and execute with full hardware speed while maintaining formal verification guarantees.

## Verification Status

- **Level**: SPARK Platinum (bodies verified)
- **High Issues**: 0
- **Medium Issues**: 4 (expected initialization proofs in postconditions)
- **Provers**: Z3, CVC5, Alt-Ergo

## Cryptographic Primitives

| Primitive | Standard | Security Level |
|-----------|----------|----------------|
| ML-KEM-1024 | NIST FIPS 203 | Level 5 |
| ML-DSA-87 | NIST FIPS 204 | Level 5 |
| SHA3-256/384/512 | NIST FIPS 202 | - |
| SHAKE128/256 | NIST FIPS 202 | - |

## Architecture

```
anubisvm/
├── core/
│   ├── src/
│   │   ├── aegis/          # Address encoding (AAS-001 v3.1)
│   │   ├── cvm/            # CVM types, interface, registry, dispatch
│   │   ├── hash/           # SHA3, Keccak, SHAKE
│   │   ├── pqc/            # ML-KEM-1024, ML-DSA-87
│   │   ├── node/           # Consensus, mempool, P2P
│   │   ├── state/          # Merkle Patricia Trie, persistence
│   │   ├── tee/            # TEE keys, attestation, runtime
│   │   └── vm/             # Execution, syscalls, gas estimation
│   └── bin/                # Built executables
├── contracts/              # Example CVM contracts
├── tests/                  # KAT and integration tests
└── docs/                   # Technical documentation
```

## Build

### Prerequisites

- GNAT FSF 14.x or GNAT Pro 25.x
- SPARK 2014 (included with GNAT Pro or community edition)
- Alire package manager

### Commands

```bash
# Build all binaries
alr build

# Run SPARK verification (Platinum level)
alr exec -- gnatprove -P anubisvm.gpr --level=2 --prover=z3,cvc5,altergo --timeout=60 -j0

# Clean build artifacts
alr clean
```

### Build Outputs

| Binary | Description |
|--------|-------------|
| `anubis_main` | Full node with TEE support |
| `khepri_main` | Contract runtime |
| `khepri_local_main` | Local contract executor for testing |

## CVM Development

Cryptographically Verified Modules are native SPARK packages that implement the CVM interface. See `docs/CVM.md` for complete documentation.

### Quick Start

1. Create a SPARK package implementing `CVM_Interface`
2. Register entry points with method selectors
3. Compile with `alr build`
4. Verify with `gnatprove`
5. Deploy to registry

### Example CVM

```ada
pragma SPARK_Mode (On);

with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;

package My_Token with
   SPARK_Mode => On
is
   --  Entry points
   procedure Transfer (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  : out    Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Is_Valid_Exec_Result (Result);

   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Is_Valid_Exec_Result (Result);

   --  CVM registration
   function Get_Descriptor return CVM_Descriptor;

end My_Token;
```

### CVM Capabilities

| Capability | Description |
|------------|-------------|
| `Cap_Read_State` | Read own state slots |
| `Cap_Write_State` | Modify own state slots |
| `Cap_Call_CVM` | Call other CVMs |
| `Cap_Shield_State` | Encrypted state (TEE) |
| `Cap_Eye_View` | Create viewing keys (TEE) |
| `Cap_Gate_Session` | Private sessions (TEE) |
| `Cap_Whisper_Value` | Confidential values (TEE) |
| `Cap_Veil_Proof` | ZK proof generation (TEE) |

## Address Format

AnubisVM uses the AAS-001 v3.1 address specification:

```
mldsa87:network:type:chunked_payload-checksum
```

| Field | Description |
|-------|-------------|
| `mldsa87` | Algorithm identifier (ML-DSA-87) |
| `network` | Network identifier (`main`, `test`, `staging`) |
| `type` | Entity type (`u` = user, `c` = contract, `v` = validator) |
| `chunked_payload` | Base32-encoded payload with dashes every 8 chars |
| `checksum` | 5-character Base32 checksum |

Example:
```
mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3-abc12
```

## Node Operation

### Configuration

Create `config/node.toml`:

```toml
[network]
listen_addr = "0.0.0.0:26656"
seeds = []

[consensus]
timeout_propose = "3s"
timeout_prevote = "1s"
timeout_precommit = "1s"

[state]
db_path = "data/state"
```

### Starting a Node

```bash
# Initialize node
./core/bin/anubis_main init --home ~/.anubisvm

# Start node
./core/bin/anubis_main start --home ~/.anubisvm
```

## SPARK Verification

AnubisVM uses SPARK formal verification to prove:

1. **Memory Safety**: No buffer overflows, use-after-free, or null dereferences
2. **Type Safety**: All operations are type-checked at compile time
3. **Termination**: All loops proven to terminate
4. **Functional Correctness**: Postconditions verified for critical operations

### Verification Levels

| Level | Description | Time |
|-------|-------------|------|
| 1 | Basic flow analysis | Fast |
| 2 | Standard proofs (development) | Medium |
| 3 | Extended proofs | Slow |
| 4 | Full proofs (release) | Very slow |

### Running Verification

```bash
# Quick check (level 2)
alr exec -- gnatprove -P anubisvm.gpr --level=2 -j0

# Full verification (level 4)
alr exec -- gnatprove -P anubisvm.gpr --level=4 --timeout=120 -j0 --report=statistics
```

## Testing

### Known Answer Tests (KAT)

```bash
# Run SHA3 KAT
./tests/bin/test_sha3_kat

# Run ML-DSA KAT
./tests/bin/test_mldsa

# Run ML-KEM KAT
./tests/bin/test_mlkem_kat

# Run address encoding tests
./tests/bin/test_address
```

### Integration Tests

```bash
# Local contract execution
./core/bin/khepri_local_main --contract my_contract.cvm --method Transfer --params hex:...
```

## Project Structure

### Core Packages

| Package | Purpose |
|---------|---------|
| `Anubis_Types` | Fundamental type definitions |
| `Anubis_SHA3` | SHA3/SHAKE hash functions |
| `Anubis_MLDSA` | ML-DSA-87 signatures |
| `Anubis_MLKEM` | ML-KEM-1024 key encapsulation |
| `CVM_Types` | CVM type definitions |
| `CVM_Interface` | CVM abstract interface |
| `CVM_Registry` | CVM deployment and lookup |
| `CVM_Dispatch` | Syscall dispatch |

### State Management

| Package | Purpose |
|---------|---------|
| `Khepri_State` | State slot operations |
| `Khepri_MPT` | Merkle Patricia Trie |
| `State_Persistence` | Durable storage |

### TEE Support

| Package | Purpose |
|---------|---------|
| `TEE_Keys` | Key management |
| `TEE_Attestation` | Hardware attestation |
| `TEE_Runtime` | TEE lifecycle |
| `Aegis_Privacy` | Privacy primitives |

## Security Considerations

1. **Constant-Time Operations**: Critical cryptographic operations are constant-time to prevent timing attacks
2. **Zeroization**: Sensitive data is cleared from memory after use
3. **State Isolation**: Each CVM has isolated state slots
4. **Capability System**: Fine-grained access control for CVM operations
5. **Formal Verification**: Mathematical proofs of correctness

## License

Apache 2.0
