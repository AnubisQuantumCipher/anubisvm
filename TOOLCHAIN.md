# Toolchain for anubis-node v0.1.0

## Target Platform
- **OS**: macOS 26.0 (Tahoe) / Apple Silicon
- **Architecture**: aarch64 (ARM64, M4 Max)

## Build Toolchain
- **Alire**: 2.1.0
- **GNAT**: 14.2.0 (FSF, via Alire)
- **GPRbuild**: 24.0.0 (2024-03-14)
- **SPARK**: GNATprove included with GNAT toolchain

## Alire Toolchain Paths
```
GNAT:     ~/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/
GPRbuild: ~/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/
```

## Build Commands
```bash
# One-click build
./scripts/build.sh

# Manual build
alr build --profiles='*=release'
alr exec -- gprbuild -P core/anubis_node.gpr -XBUILD_MODE=release

# Run SPARK proofs (requires ~30min on M4 Max)
alr exec -- gnatprove -P anubisvm.gpr --level=4 --proof=all
```

## Output Binary
```
core/bin/anubis-node
Architecture: Mach-O 64-bit arm64
```

## Dependencies (Ada/SPARK only)
- GNAT.Sockets (TCP networking)
- Interfaces (modular arithmetic)
- Ada.Streams (byte I/O)

No external C libraries. No Go. No Rust.

## Reproducibility

To rebuild from source on a fresh M4 Mac:

```bash
# 1. Install Alire
curl -L https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-aarch64-macos.zip -o alr.zip
unzip alr.zip && mv bin/alr ~/.local/bin/

# 2. Clone and build
git clone https://github.com/AnubisQuantumCipher/anubisvm.git
cd anubisvm
alr toolchain --select gnat_native=14.2.1
./scripts/build.sh
```

## Verification

```bash
# Check binary
file core/bin/anubis-node
# Expected: Mach-O 64-bit executable arm64

# SHA256 (v0.1.0)
shasum -a 256 core/bin/anubis-node
```
