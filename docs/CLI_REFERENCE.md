# AnubisVM CLI Reference

Complete command-line interface documentation for the AnubisVM smart contract development tools.

## Overview

AnubisVM provides three main CLI tools:

| Tool | Description |
|------|-------------|
| `khepri` | Smart contract development and interaction |
| `khepri-local` | Local development node |
| `anubis-node` | Full blockchain node |

---

## khepri

The primary CLI for smart contract development.

### Global Options

```
--help, -h       Show help information
--version, -v    Show version information
--verbose        Enable verbose output
--quiet          Suppress non-essential output
```

---

## Contract Commands

### contract new

Create a new smart contract project.

```bash
khepri contract new <name>
```

**Arguments:**
- `<name>` - Contract name (alphanumeric, underscores allowed)

**Creates:**
```
contracts/<name>/
├── <name>.gpr           # GPR project file
├── khepri.toml          # Contract manifest
└── src/
    ├── <name>.ads       # SPARK specification
    ├── <name>.adb       # Implementation
    └── <name>_main.adb  # Test harness
```

**Example:**
```bash
khepri contract new my_token
# Creates contracts/my_token/
```

---

### contract build

Build a smart contract.

```bash
khepri contract build <name>
```

**Arguments:**
- `<name>` - Contract name to build

**Output:**
- Compiles contract to `contracts/<name>/bin/<name>_main`
- Object files in `contracts/<name>/obj/`

**Example:**
```bash
khepri contract build my_token
# Output: Build successful: contracts/my_token/bin/my_token_main
```

---

### contract run

Execute a contract's test harness.

```bash
khepri contract run <name>
```

**Arguments:**
- `<name>` - Contract name to run

**Example:**
```bash
khepri contract run my_token
# Output:
# AnubisVM Contract Test
# ======================
# Contract initialized
# Owner verification passed
# All tests passed!
```

---

### contract prove

Run SPARK formal verification on a contract.

```bash
khepri contract prove <name> [--level=<1-4>]
```

**Arguments:**
- `<name>` - Contract name to verify

**Options:**
- `--level=<1-4>` - Proof level (default: 2)

**Proof Levels:**
| Level | Description |
|-------|-------------|
| 1 | Flow analysis only |
| 2 | Standard proofs (recommended) |
| 3 | Extended proofs |
| 4 | Full proofs (slow) |

**Example:**
```bash
khepri contract prove my_token --level=2
# Output: gnatprove results...
```

---

## Key Management Commands

### keys new

Generate a new ML-DSA-87 keypair.

```bash
khepri keys new <name>
```

**Arguments:**
- `<name>` - Key identifier

**Output:**
- Creates keypair in `~/.anubisvm/keys/<name>/`
- Public key: `<name>.pub`
- Secret key: `<name>.key` (encrypted)

**Example:**
```bash
khepri keys new deployer
# Output:
# Generated ML-DSA-87 keypair: deployer
# Public key: ~/.anubisvm/keys/deployer/deployer.pub
# Address: mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-abc12
```

---

### keys list

List all available keys.

```bash
khepri keys list
```

**Output:**
```
NAME        TYPE       ADDRESS
deployer    ML-DSA-87  mldsa87:main:u:qr7zy5kx-mjgpv4wc...
validator   ML-DSA-87  mldsa87:main:v:abc123xy-def456zw...
```

---

### keys show

Display key details.

```bash
khepri keys show <name>
```

**Arguments:**
- `<name>` - Key identifier

**Output:**
```
Name:       deployer
Type:       ML-DSA-87 (FIPS 204 Level 5)
Address:    mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-abc12
Public Key: (2592 bytes)
Created:    2024-12-13T10:30:00Z
```

---

### keys export

Export a key to file.

```bash
khepri keys export <name> [--format=<pem|raw>] [--output=<file>]
```

**Arguments:**
- `<name>` - Key identifier

**Options:**
- `--format` - Export format: `pem` (default) or `raw`
- `--output` - Output file path

**Example:**
```bash
khepri keys export deployer --format=pem --output=deployer.pem
```

---

## Address Commands

### address from-key

Derive address from an existing key.

```bash
khepri address from-key <name>
```

**Arguments:**
- `<name>` - Key identifier

**Output:**
```
mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-abc12
```

---

### address info

Display address details.

```bash
khepri address info <address>
```

**Arguments:**
- `<address>` - Full address string

**Output:**
```
Address:    mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-abc12
Algorithm:  ML-DSA-87 (FIPS 204)
Network:    main
Type:       user (u)
Payload:    qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7
Checksum:   abc12 (valid)
```

---

## Encoding Commands

### encode

Encode a value to hex.

```bash
khepri encode <type> <value>
```

**Types:**
- `uint256` - 256-bit unsigned integer
- `address` - 32-byte address
- `string` - UTF-8 string
- `bytes` - Raw bytes (hex input)

**Examples:**
```bash
khepri encode uint256 1000000
# Output: 0x00000000000000000000000000000000000000000000000000000000000f4240

khepri encode address mldsa87:main:u:qr7zy5kx...
# Output: 0x...

khepri encode string "Hello"
# Output: 0x48656c6c6f
```

---

### decode

Decode hex to a value.

```bash
khepri decode <type> <hex>
```

**Examples:**
```bash
khepri decode uint256 0x000000000000000000000000000000000000000000000000000000000000000f4240
# Output: 1000000

khepri decode string 0x48656c6c6f
# Output: Hello
```

---

## Utility Commands

### version

Show version information.

```bash
khepri version
```

**Output:**
```
AnubisVM Khepri CLI v1.0.0
SPARK/Ada Runtime
Post-Quantum: ML-DSA-87, ML-KEM-1024
Build: 2024-12-13
```

---

### help

Show help information.

```bash
khepri help [command]
```

**Examples:**
```bash
khepri help
khepri help contract
khepri help keys new
```

---

## khepri-local

Local development node for testing contracts.

### Starting the Node

```bash
khepri-local [--port=<port>] [--data=<dir>]
```

**Options:**
- `--port` - RPC port (default: 26657)
- `--data` - Data directory (default: `~/.anubisvm-local`)

**Example:**
```bash
khepri-local --port=26657
# Output:
# AnubisVM Local Node
# RPC: http://localhost:26657
# Ready for connections...
```

### Connecting to Local Node

```bash
khepri --node=localhost:26657 <command>
```

---

## anubis-node

Full blockchain node.

### init

Initialize a new node.

```bash
anubis-node init [--home=<dir>] [--chain-id=<id>]
```

**Options:**
- `--home` - Node home directory (default: `~/.anubisvm`)
- `--chain-id` - Chain identifier

**Example:**
```bash
anubis-node init --home ~/.anubisvm --chain-id anubis-1
```

---

### start

Start the node.

```bash
anubis-node start [--home=<dir>]
```

**Options:**
- `--home` - Node home directory

**Example:**
```bash
anubis-node start --home ~/.anubisvm
```

---

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `ANUBISVM_HOME` | Default home directory | `~/.anubisvm` |
| `ANUBISVM_NODE` | Default node URL | `localhost:26657` |
| `ANUBISVM_KEYS` | Keys directory | `~/.anubisvm/keys` |
| `DYLD_LIBRARY_PATH` | macOS library path | - |
| `LD_LIBRARY_PATH` | Linux library path | - |

---

## Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | General error |
| 2 | Invalid arguments |
| 3 | File not found |
| 4 | Build error |
| 5 | Proof failure |
| 6 | Network error |
| 7 | Key error |

---

## Examples

### Complete Workflow

```bash
# 1. Create a new contract
khepri contract new my_token

# 2. Edit the contract
# (modify contracts/my_token/src/my_token.ads and .adb)

# 3. Build the contract
khepri contract build my_token

# 4. Run tests
khepri contract run my_token

# 5. Verify with SPARK
khepri contract prove my_token --level=2

# 6. Generate deployment key
khepri keys new deployer

# 7. Start local node (in another terminal)
khepri-local

# 8. Deploy contract
khepri deploy contracts/my_token/bin/my_token_main --from deployer
```

### Key Management Workflow

```bash
# Generate keys
khepri keys new alice
khepri keys new bob

# List keys
khepri keys list

# Get addresses
khepri address from-key alice
khepri address from-key bob

# Export for backup
khepri keys export alice --output=alice-backup.pem
```

---

## Troubleshooting

### "Command not found"

Ensure CLI is in PATH:
```bash
export PATH="$HOME/.local/bin:$PATH"
```

Or use full path:
```bash
./core/bin/khepri_main contract new my_token
```

### "Library not found"

Set library path:
```bash
# macOS
export DYLD_LIBRARY_PATH=/path/to/anubisvm/core/lib

# Linux
export LD_LIBRARY_PATH=/path/to/anubisvm/core/lib
```

### "Build failed"

Check GPR file has correct SDK path:
```ada
SDK_Root := "/correct/path/to/anubisvm/core/src";
```

### "Proof timeout"

Reduce proof level or increase timeout:
```bash
khepri contract prove my_token --level=1
```
