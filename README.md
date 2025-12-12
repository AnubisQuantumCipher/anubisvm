# AnubisVM

Post-quantum smart contract virtual machine with formal verification.

**Build ELF smart contracts** that are mathematically proven correct and quantum-resistant.

## Quick Start

```bash
# Clone the repository
git clone https://github.com/AnubisQuantumCipher/anubisvm.git
cd anubisvm

# Setup (installs Alire if needed)
make setup

# Build
make

# Install CLI tools
make install

# Verify installation
khepri version
```

## Features

- **Post-Quantum Cryptography**: ML-DSA-87 (FIPS 204) signatures, ML-KEM-1024 (FIPS 203) key encapsulation
- **Formal Verification**: SPARK/Ada with 93% proven (core crypto ~99%)
- **Native Performance**: Contracts compile to native ELF binaries, not interpreted bytecode
- **TEE Support**: Trusted Execution Environment with hardware attestation
- **335 Tests**: Comprehensive test suite including NIST KAT vectors

## Requirements

| Dependency | Version | Installation |
|------------|---------|--------------|
| Alire | 2.0+ | `brew install alire` (macOS) or [download](https://alire.ada.dev/) |
| GNAT | 14.x+ | Installed automatically by Alire |
| GNATprove | 14.x+ | Installed automatically by Alire |

### Installing Alire

**macOS:**
```bash
brew install alire
```

**Linux (Debian/Ubuntu):**
```bash
# Download latest release
wget https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-x86_64-linux.zip
unzip alr-2.0.1-bin-x86_64-linux.zip
sudo mv bin/alr /usr/local/bin/
```

**Linux (Arch):**
```bash
yay -S alire
```

**Windows:**
Download installer from [alire.ada.dev](https://alire.ada.dev/)

## Building

```bash
# Development build
make build

# Release build (optimized)
make build-release

# Run tests
make test

# Run SPARK verification
make prove
```

## Installation

```bash
# Install to ~/.local/bin
make install

# Add to PATH (add to ~/.bashrc or ~/.zshrc)
export PATH="$HOME/.local/bin:$PATH"

# Verify
khepri version
```

This installs:
- `khepri` - Smart contract development CLI
- `anubis-node` - Full blockchain node
- `khepri-local` - Local development/testing node

## Smart Contract Development

### Contract Structure

AnubisVM contracts are **Cryptographically Verified Modules (CVMs)** - native SPARK/Ada packages that compile to ELF binaries.

```
my_contract/
  my_contract.ads    -- Contract specification (interface)
  my_contract.adb    -- Contract implementation
  my_contract.gpr    -- GNAT project file
```

### Creating a Contract

```bash
# Create from template
mkdir -p contracts/my_token
```

**my_token.ads** (specification):
```ada
pragma SPARK_Mode (On);

with CVM_Types;     use CVM_Types;
with CVM_Interface; use CVM_Interface;

package My_Token with SPARK_Mode => On is

   --  Initialize token with total supply
   procedure Initialize (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  :    out Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Result.Success or Result.Error_Code /= 0;

   --  Transfer tokens between accounts
   procedure Transfer (
      Context : in     Call_Context;
      State   : in out State_Array;
      Result  :    out Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Result.Success or Result.Error_Code /= 0;

   --  Get balance of an account
   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Result.Success;

   --  CVM descriptor for registry
   function Get_Descriptor return CVM_Descriptor;

end My_Token;
```

**my_token.adb** (implementation):
```ada
pragma SPARK_Mode (On);

with Khepri_State; use Khepri_State;

package body My_Token with SPARK_Mode => On is

   --  Storage slot constants
   Total_Supply_Slot : constant := 0;
   Balance_Slot_Base : constant := 1;

   procedure Initialize (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) is
      Supply : constant U256 := Decode_U256 (Context.Calldata);
   begin
      --  Store total supply
      Store_U256 (State, Total_Supply_Slot, Supply);

      --  Give all tokens to deployer
      Store_U256 (State, Balance_Slot_Base + Hash_Address (Context.Sender), Supply);

      Result := (Success => True, Gas_Used => 50_000, Return_Data => (others => 0), others => <>);
   end Initialize;

   procedure Transfer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) is
      To     : constant Address := Decode_Address (Context.Calldata (0 .. 31));
      Amount : constant U256    := Decode_U256 (Context.Calldata (32 .. 63));

      From_Slot : constant Natural := Balance_Slot_Base + Hash_Address (Context.Sender);
      To_Slot   : constant Natural := Balance_Slot_Base + Hash_Address (To);

      From_Balance : U256;
      To_Balance   : U256;
   begin
      From_Balance := Load_U256 (State, From_Slot);
      To_Balance   := Load_U256 (State, To_Slot);

      if From_Balance < Amount then
         Result := (Success => False, Error_Code => 1, others => <>);  --  Insufficient balance
         return;
      end if;

      Store_U256 (State, From_Slot, From_Balance - Amount);
      Store_U256 (State, To_Slot, To_Balance + Amount);

      Result := (Success => True, Gas_Used => 30_000, others => <>);
   end Transfer;

   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) is
      Account : constant Address := Decode_Address (Context.Calldata);
      Slot    : constant Natural := Balance_Slot_Base + Hash_Address (Account);
      Balance : constant U256    := Load_U256 (State, Slot);
   begin
      Result := (
         Success     => True,
         Gas_Used    => 5_000,
         Return_Data => Encode_U256 (Balance),
         others      => <>
      );
   end Balance_Of;

   function Get_Descriptor return CVM_Descriptor is
   begin
      return (
         Name       => "My_Token",
         Version    => 1,
         Methods    => (
            (Selector => 16#00000001#, Handler => Initialize'Access),
            (Selector => 16#00000002#, Handler => Transfer'Access),
            (Selector => 16#00000003#, Handler => Balance_Of'Access)
         ),
         Capability => Cap_Read_State or Cap_Write_State
      );
   end Get_Descriptor;

end My_Token;
```

### Building Contracts

```bash
# Build contract
cd contracts/my_token
alr build

# Verify with SPARK
alr exec -- gnatprove -P my_token.gpr --level=2

# The compiled ELF is in obj/my_token
```

### Deploying Contracts

```bash
# Generate deployment keypair
khepri keys new deployer

# Deploy contract
khepri deploy contracts/my_token/obj/my_token \
  --from deployer \
  --gas 1000000

# Returns contract address:
# Contract deployed: mldsa87:main:c:abc123...
```

### Interacting with Contracts

```bash
# Call read-only method
khepri call <contract-address> Balance_Of \
  --args "address:<account-address>"

# Send transaction
khepri send <contract-address> Transfer \
  --args "address:<to-address>,uint256:1000000" \
  --from deployer \
  --gas 100000
```

## CLI Reference

### khepri

```
khepri - AnubisVM Smart Contract CLI

COMMANDS:
  version              Show version information
  help                 Show help

KEY MANAGEMENT:
  keys new <name>      Generate new ML-DSA-87 keypair
  keys list            List all keys
  keys show <name>     Show key details
  keys export <name>   Export key to file

ADDRESS:
  address from-key <n> Derive address from key
  address info <addr>  Show address details

CONTRACT:
  deploy <path>        Deploy contract
  call <addr> <method> Call contract (read-only)
  send <addr> <method> Send transaction

ENCODING:
  encode <type> <val>  Encode value (uint256, address, string)
  decode <type> <hex>  Decode hex to value

NETWORK:
  node status          Show node status
  block <number>       Get block info
  tx <hash>            Get transaction info
  account <address>    Get account info
```

### khepri-local

Local development node for testing:

```bash
# Start local node
khepri-local

# In another terminal, deploy and test
khepri deploy my_contract --node localhost:26657
```

### anubis-node

Full blockchain node:

```bash
# Initialize node
anubis-node init --home ~/.anubisvm

# Start node
anubis-node start --home ~/.anubisvm
```

## Architecture

```
anubisvm/
  core/
    src/
      aegis/       # Address encoding (AAS-001 v3.1)
      cvm/         # CVM types, interface, registry, dispatch
      crypto/      # KMAC, AEAD, KDF primitives
      hash/        # SHA3, Keccak, SHAKE
      pqc/         # ML-KEM-1024, ML-DSA-87
      state/       # Khepri MPT, persistence
      tee/         # TEE keys, attestation, runtime
      vm/          # Execution engine
    bin/           # Compiled binaries
  cli/             # CLI source code
  contracts/       # Example contracts
  tests/           # Test suite
  docs/            # Documentation
```

## Cryptographic Primitives

| Primitive | Standard | Use Case |
|-----------|----------|----------|
| ML-DSA-87 | NIST FIPS 204 | Transaction signatures |
| ML-KEM-1024 | NIST FIPS 203 | Key encapsulation |
| SHA3-256/512 | NIST FIPS 202 | Hashing |
| SHAKE128/256 | NIST FIPS 202 | XOF |
| KMAC256 | NIST SP 800-185 | Keyed hashing |

## Address Format (AAS-001 v3.1)

```
mldsa87:network:type:payload-checksum
```

Example:
```
mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-abc12
         ^    ^  ^                              ^
         |    |  |                              checksum
         |    |  base32 payload (chunked)
         |    type (u=user, c=contract, v=validator)
         network (main, test, staging)
```

## SPARK Verification

```bash
# Quick verification (level 2)
make prove

# Full verification (level 4)
make prove-full

# View proof report
make prove-report
```

### Verification Levels

| Level | Description | Speed |
|-------|-------------|-------|
| 1 | Flow analysis only | Fast |
| 2 | Standard proofs | Medium |
| 3 | Extended proofs | Slow |
| 4 | Full proofs | Very slow |

## Testing

```bash
# Run all tests (335 tests)
make test

# Run only KAT tests
make test-kat

# Quick CLI test
make test-quick
```

## Project Status

- Core VM: Complete
- CLI: Complete
- SPARK Proofs: 93% (core crypto ~99%)
- Tests: 335 passing
- Documentation: In progress

## License

Apache 2.0

## Contributing

1. Fork the repository
2. Create a feature branch
3. Write SPARK-verified code
4. Run `make prove` to verify
5. Run `make test` to test
6. Submit pull request
