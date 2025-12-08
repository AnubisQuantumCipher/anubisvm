#!/bin/bash
#===============================================================================
# ANUBIS Testnet Initialization Script
#
# This script initializes a new ANUBIS testnet node with:
# - ML-DSA-87 validator keypair
# - Genesis configuration with ANUBIS Token v4.0 allocations
# - Proper chain ID and consensus parameters
#===============================================================================

set -e

# Configuration
CHAIN_ID="${CHAIN_ID:-anubis-testnet-1}"
MONIKER="${MONIKER:-anubis-validator-1}"
HOME_DIR="${HOME_DIR:-$HOME/.anubis}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}===============================================================================${NC}"
echo -e "${GREEN}                    ANUBIS TESTNET INITIALIZATION${NC}"
echo -e "${GREEN}===============================================================================${NC}"
echo ""
echo -e "Chain ID:    ${YELLOW}${CHAIN_ID}${NC}"
echo -e "Moniker:     ${YELLOW}${MONIKER}${NC}"
echo -e "Home Dir:    ${YELLOW}${HOME_DIR}${NC}"
echo ""

# Check if aegisd binary exists
AEGISD="$PROJECT_ROOT/cosmos/aegisd"
if [ ! -f "$AEGISD" ]; then
    echo -e "${YELLOW}Building aegisd...${NC}"
    cd "$PROJECT_ROOT/cosmos"

    # Set CGO environment for SPARK/Ada integration
    export CGO_ENABLED=1
    export CGO_CFLAGS="-I$PROJECT_ROOT/core/include"
    export CGO_LDFLAGS="-L$PROJECT_ROOT/core/lib -laegisvm -L$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib -lgnat -lgnarl -lm"

    go build -o aegisd ./cmd/aegisd/
    cd "$SCRIPT_DIR"
fi

if [ ! -f "$AEGISD" ]; then
    echo -e "${RED}ERROR: Failed to build aegisd${NC}"
    exit 1
fi

# Set library path for runtime
export DYLD_LIBRARY_PATH="$PROJECT_ROOT/core/lib:$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib:$DYLD_LIBRARY_PATH"

# Clean previous installation if requested
if [ "$1" == "--clean" ]; then
    echo -e "${YELLOW}Cleaning previous installation...${NC}"
    rm -rf "$HOME_DIR"
fi

# Initialize the node
echo -e "${GREEN}Initializing node...${NC}"
$AEGISD init "$MONIKER" --chain-id "$CHAIN_ID" --home "$HOME_DIR" 2>/dev/null || true

# Copy genesis configuration
echo -e "${GREEN}Configuring genesis...${NC}"
cp "$PROJECT_ROOT/cosmos/config/genesis_anubis.json" "$HOME_DIR/config/genesis.json"

# Update chain ID in genesis
sed -i '' "s/\"chain_id\": \"anubis-1\"/\"chain_id\": \"$CHAIN_ID\"/" "$HOME_DIR/config/genesis.json"

# Generate validator key if not exists
if [ ! -f "$HOME_DIR/config/priv_validator_key.json" ]; then
    echo -e "${GREEN}Generating ML-DSA-87 validator key...${NC}"
    $AEGISD keys add validator --keyring-backend test --home "$HOME_DIR" 2>/dev/null || true
fi

# Show validator address
echo ""
echo -e "${GREEN}===============================================================================${NC}"
echo -e "${GREEN}                    INITIALIZATION COMPLETE${NC}"
echo -e "${GREEN}===============================================================================${NC}"
echo ""
echo -e "Home directory: ${YELLOW}$HOME_DIR${NC}"
echo -e "Genesis file:   ${YELLOW}$HOME_DIR/config/genesis.json${NC}"
echo ""
echo -e "To start the node:"
echo -e "  ${YELLOW}./scripts/start_testnet.sh${NC}"
echo ""
echo -e "To add genesis accounts:"
echo -e "  ${YELLOW}$AEGISD genesis add-genesis-account <address> <amount>aanubis --home $HOME_DIR${NC}"
echo ""
