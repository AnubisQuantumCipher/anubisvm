#!/bin/bash
#
# AnubisVM Installation Script
# Installs khepri CLI, anubis-node, and khepri-local
#
# Usage:
#   ./scripts/install.sh              # Standard install
#   ./scripts/install.sh --with-tests # Include test binaries
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
INSTALL_DIR="${HOME}/.local/bin"
DATA_DIR="${HOME}/.anubisvm"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "AnubisVM Installer"
echo "=================="
echo ""

# Detect OS
OS="$(uname -s)"
ARCH="$(uname -m)"
echo "Platform: $OS ($ARCH)"

# Find Alire
ALR=""
if command -v alr &> /dev/null; then
    ALR="$(command -v alr)"
elif [ -x "${HOME}/.local/bin/alr" ]; then
    ALR="${HOME}/.local/bin/alr"
elif [ -x "/usr/local/bin/alr" ]; then
    ALR="/usr/local/bin/alr"
fi

if [ -z "$ALR" ]; then
    echo ""
    echo -e "${RED}Error: Alire (alr) not found${NC}"
    echo ""
    echo "Please install Alire first:"
    echo ""
    case "$OS" in
        Darwin)
            echo "  brew install alire"
            ;;
        Linux)
            echo "  # Download from: https://github.com/alire-project/alire/releases"
            echo "  wget https://github.com/alire-project/alire/releases/latest/download/alr-linux.zip"
            echo "  unzip alr-linux.zip && sudo mv bin/alr /usr/local/bin/"
            ;;
        *)
            echo "  Download from: https://alire.ada.dev/"
            ;;
    esac
    echo ""
    exit 1
fi

echo "Alire: $ALR ($($ALR --version 2>/dev/null | head -1))"
echo ""

# Build project
echo "[1/4] Building AnubisVM..."
cd "$PROJECT_DIR"
$ALR build

# Create directories
echo "[2/4] Creating directories..."
mkdir -p "$INSTALL_DIR"
mkdir -p "$DATA_DIR/keys"
mkdir -p "$DATA_DIR/state"
mkdir -p "$DATA_DIR/contracts"

# Fix RPATH on macOS
if [ "$OS" = "Darwin" ]; then
    echo "[3/4] Fixing macOS RPATH..."
    GNAT_LIB=$($ALR exec -- gnatls -v 2>/dev/null | grep adalib | head -1 | sed 's|/adalib||' || echo "")
    if [ -n "$GNAT_LIB" ]; then
        for bin in "$PROJECT_DIR/core/bin"/*; do
            if [ -f "$bin" ] && [ -x "$bin" ]; then
                install_name_tool -delete_rpath "$GNAT_LIB" "$bin" 2>/dev/null || true
            fi
        done
    fi
else
    echo "[3/4] Skipping RPATH fix (not macOS)"
fi

# Install binaries
echo "[4/4] Installing binaries..."

install_binary() {
    local src="$1"
    local dst="$2"
    local name="$3"

    if [ -f "$src" ]; then
        rm -f "$dst"
        cp "$src" "$dst"
        chmod +x "$dst"
        echo -e "  ${GREEN}Installed${NC}: $dst"
    else
        echo -e "  ${YELLOW}Skipped${NC}: $name (not found)"
    fi
}

# Main binaries
install_binary "$PROJECT_DIR/core/bin/khepri_main" "$INSTALL_DIR/khepri" "khepri"
install_binary "$PROJECT_DIR/core/bin/anubis_main" "$INSTALL_DIR/anubis-node" "anubis-node"
install_binary "$PROJECT_DIR/core/bin/khepri_local_main" "$INSTALL_DIR/khepri-local" "khepri-local"

# Test binaries (optional)
if [ "$1" = "--with-tests" ]; then
    echo ""
    echo "Installing test binaries..."
    for test_bin in "$PROJECT_DIR/core/bin/test_"*; do
        if [ -f "$test_bin" ]; then
            name=$(basename "$test_bin")
            install_binary "$test_bin" "$INSTALL_DIR/$name" "$name"
        fi
    done
fi

echo ""
echo -e "${GREEN}Installation complete!${NC}"
echo ""
echo "Installed binaries:"
echo "  $INSTALL_DIR/khepri        - Smart Contract CLI"
echo "  $INSTALL_DIR/anubis-node   - Full Node"
echo "  $INSTALL_DIR/khepri-local  - Local Development Node"
echo ""
echo "Data directory: $DATA_DIR"
echo ""

# Check if ~/.local/bin is in PATH
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo -e "${YELLOW}Note${NC}: Add ~/.local/bin to your PATH:"
    echo ""
    echo "  # Add to ~/.bashrc or ~/.zshrc:"
    echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo ""
fi

echo "Run 'khepri help' to get started."
echo ""
echo "Quick start:"
echo "  khepri keys new mykey      # Generate ML-DSA-87 keypair"
echo "  khepri version             # Show version info"
echo "  khepri test                # Run self-tests"
