#!/bin/bash
# Anubis VM Build Script
# Pure Ada/SPARK - No Go - No Rust - Just Math

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

echo "Building AnubisVM..."

# Build all binaries
alr build --profiles='*=release'

# Fix duplicate RPATH entries on macOS (required for macOS 15.4+)
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo ""
    echo "Fixing RPATH entries for macOS..."
    "$SCRIPT_DIR/fix_rpath.sh" core/bin
fi

echo ""
echo "Build complete!"
echo ""
echo "Binaries available in core/bin/:"
ls -la core/bin/

echo ""
echo "Run with:"
echo "  ./core/bin/khepri_main --help          # CLI tool"
echo "  ./core/bin/khepri_local_main --help    # Local executor"
echo "  ./core/bin/anubis_main                 # Full node"
