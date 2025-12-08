#!/bin/bash
#===============================================================================
# ANUBIS Testnet Start Script
#
# Starts the ANUBIS testnet node with proper library paths and configuration.
#===============================================================================

set -e

# Configuration
HOME_DIR="${HOME_DIR:-$HOME/.anubis}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${GREEN}===============================================================================${NC}"
echo -e "${GREEN}                    STARTING ANUBIS TESTNET NODE${NC}"
echo -e "${GREEN}===============================================================================${NC}"
echo ""

# Check binary
AEGISD="$PROJECT_ROOT/cosmos/aegisd"
if [ ! -f "$AEGISD" ]; then
    echo "ERROR: aegisd not found. Run ./scripts/init_testnet.sh first."
    exit 1
fi

# Set library paths for SPARK/Ada runtime
export DYLD_LIBRARY_PATH="$PROJECT_ROOT/core/lib:$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib:$DYLD_LIBRARY_PATH"

# Linux equivalent
export LD_LIBRARY_PATH="$PROJECT_ROOT/core/lib:$LD_LIBRARY_PATH"

echo -e "Home directory: ${YELLOW}$HOME_DIR${NC}"
echo -e "Binary:         ${YELLOW}$AEGISD${NC}"
echo ""

# Start the node
echo -e "${GREEN}Starting node...${NC}"
echo ""

exec $AEGISD start \
    --home "$HOME_DIR" \
    --log_level "info" \
    --minimum-gas-prices "0.001aanubis" \
    "$@"
