#!/bin/bash
#
# AnubisVM Installation Script
# Installs khepri CLI and anubis-node to ~/.local/bin
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
INSTALL_DIR="${HOME}/.local/bin"
GNAT_LIB="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib"

echo "AnubisVM Installer"
echo "=================="
echo ""

# Check if alr is available
if ! command -v /Users/sicarii/bin/alr &> /dev/null; then
    echo "Error: Alire (alr) not found"
    exit 1
fi

# Build project
echo "[1/4] Building AnubisVM..."
cd "$PROJECT_DIR"
/Users/sicarii/bin/alr build

# Create install directory
echo "[2/4] Creating install directory..."
mkdir -p "$INSTALL_DIR"

# Fix RPATH and copy binaries
echo "[3/4] Installing binaries..."

install_binary() {
    local src="$1"
    local dst="$2"

    if [ -f "$src" ]; then
        # Remove duplicate RPATH if present
        install_name_tool -delete_rpath "$GNAT_LIB" "$src" 2>/dev/null || true

        # Copy to install directory
        cp "$src" "$dst"
        chmod +x "$dst"
        echo "  Installed: $dst"
    fi
}

# Main binaries
install_binary "$PROJECT_DIR/core/bin/khepri_main" "$INSTALL_DIR/khepri"
install_binary "$PROJECT_DIR/core/bin/anubis_main" "$INSTALL_DIR/anubis-node"
install_binary "$PROJECT_DIR/core/bin/khepri_local_main" "$INSTALL_DIR/khepri-local"

# Test binaries (optional)
if [ "$1" == "--with-tests" ]; then
    for test_bin in "$PROJECT_DIR/core/bin/test_"*; do
        if [ -f "$test_bin" ]; then
            name=$(basename "$test_bin")
            install_binary "$test_bin" "$INSTALL_DIR/$name"
        fi
    done
fi

# Create data directory
echo "[4/4] Creating data directory..."
mkdir -p "$HOME/.anubisvm/keys"
mkdir -p "$HOME/.anubisvm/state"

echo ""
echo "Installation complete!"
echo ""
echo "Installed binaries:"
echo "  $INSTALL_DIR/khepri       - SPARK Contract Development CLI"
echo "  $INSTALL_DIR/anubis-node  - TEE Node Server"
echo "  $INSTALL_DIR/khepri-local - Local Development Node"
echo ""

# Check if ~/.local/bin is in PATH
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo "Note: Add ~/.local/bin to your PATH:"
    echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo ""
fi

echo "Run 'khepri help' to get started."
