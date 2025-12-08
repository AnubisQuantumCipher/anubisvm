#!/bin/bash
# Anubis VM Build Script
# Pure Ada/SPARK - No Go - No Rust - Just Math

set -e

cd "$(dirname "$0")/.."

echo "Building Anubis Node..."

# Build library first
alr build --profiles='*=release'

# Build node executable
alr exec -- gprbuild -P core/anubis_node.gpr -XBUILD_MODE=release

# Fix rpath issue on macOS
install_name_tool -delete_rpath /Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib core/bin/anubis-node 2>/dev/null || true

SIZE=$(stat -f%z core/bin/anubis-node 2>/dev/null || stat -c%s core/bin/anubis-node)
echo ""
echo "Build complete: core/bin/anubis-node ($SIZE bytes)"
echo ""
echo "Run with: ./core/bin/anubis-node --port 26659"
