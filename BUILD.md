# Building Anubis Node on M4 Max

## Prerequisites
- GNAT 14.2+ for ARM64 macOS (via Alire)
- Alire package manager

## Quick Start

```bash
# One-click build
./scripts/build.sh

# Run the node
./core/bin/anubis-node --port 26659

# Test (from another terminal)
echo '{"jsonrpc":"2.0","method":"health","id":"1"}' | nc localhost 26659
```

## Architecture

```
anubis-node (Pure Ada/SPARK binary)
    |
    +-- GNAT.Sockets (TCP server)
    |
    +-- Anubis_Node (VM instance, RPC processing)
    |
    +-- Aegis_VM (SPARK verified execution)
    |
    +-- Anubis_SHA3, Anubis_MLDSA, Anubis_MLKEM (PQC crypto)
    |
    v
syscall -> kernel -> M4 Max hardware
```

No Go. No Rust. Just proven Ada/SPARK from entry point to syscall.

## Formal Verification

```bash
# Run SPARK proofs (requires ~30min on M4 Max)
alr exec -- gnatprove -P anubisvm.gpr --level=4 --proof=all
```

## JSON-RPC API

Port: 26659 (default)

### Methods

| Method | Description |
|--------|-------------|
| `health` | Health check, returns "ok" |
| `getBlockNumber` | Get latest block number |
| `execute` | Execute contract call |
| `query` | Read-only state query |
| `deploy` | Deploy new contract |

### Example

```bash
# Health check
echo '{"jsonrpc":"2.0","method":"health","id":"1"}' | nc localhost 26659
# Response: {"jsonrpc":"2.0","id":"1","result":"ok"}
```

## Backup

```bash
# Push to all configured remotes
./scripts/backup.sh
```

## Security

- ML-DSA-87 signatures (NIST FIPS 204) - NIST Level 5
- ML-KEM-1024 encryption (NIST FIPS 203) - NIST Level 5
- SPARK formally verified core
- No memory safety vulnerabilities (proven)
- Constant-time cryptographic operations

## License

Proprietary - All rights reserved
