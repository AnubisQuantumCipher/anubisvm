#!/bin/bash
#===============================================================================
# ANUBIS Testnet Complete Setup Script
#
# This script performs a complete testnet setup including:
# 1. Building the aegisd binary
# 2. Initializing the node
# 3. Creating a validator account
# 4. Adding genesis allocations
# 5. Generating gentx for validator
# 6. Starting the node
#===============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Configuration
export CHAIN_ID="${CHAIN_ID:-anubis-testnet-1}"
export MONIKER="${MONIKER:-anubis-validator-1}"
export HOME_DIR="${HOME_DIR:-$HOME/.anubis}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_step() {
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}Step $1: $2${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
}

echo -e "${GREEN}"
echo "╔═══════════════════════════════════════════════════════════════════════════╗"
echo "║                                                                           ║"
echo "║     █████╗ ███╗   ██╗██╗   ██╗██████╗ ██╗███████╗                         ║"
echo "║    ██╔══██╗████╗  ██║██║   ██║██╔══██╗██║██╔════╝                         ║"
echo "║    ███████║██╔██╗ ██║██║   ██║██████╔╝██║███████╗                         ║"
echo "║    ██╔══██║██║╚██╗██║██║   ██║██╔══██╗██║╚════██║                         ║"
echo "║    ██║  ██║██║ ╚████║╚██████╔╝██████╔╝██║███████║                         ║"
echo "║    ╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝ ╚═╝╚══════╝                         ║"
echo "║                                                                           ║"
echo "║              Post-Quantum Secure Smart Contract Platform                  ║"
echo "║                        Testnet Setup v1.0                                 ║"
echo "║                                                                           ║"
echo "╚═══════════════════════════════════════════════════════════════════════════╝"
echo -e "${NC}"
echo ""
echo -e "Chain ID:    ${YELLOW}$CHAIN_ID${NC}"
echo -e "Moniker:     ${YELLOW}$MONIKER${NC}"
echo -e "Home Dir:    ${YELLOW}$HOME_DIR${NC}"
echo ""

# Set library paths
export DYLD_LIBRARY_PATH="$PROJECT_ROOT/core/lib:$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib:$DYLD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$PROJECT_ROOT/core/lib:$LD_LIBRARY_PATH"

AEGISD="$PROJECT_ROOT/cosmos/aegisd"

#-------------------------------------------------------------------------------
print_step 1 "Building aegisd binary"
#-------------------------------------------------------------------------------

if [ ! -f "$AEGISD" ] || [ "$1" == "--rebuild" ]; then
    echo "Building aegisd with SPARK/Ada integration..."
    cd "$PROJECT_ROOT/cosmos"

    export CGO_ENABLED=1
    export CGO_CFLAGS="-I$PROJECT_ROOT/core/include"
    export CGO_LDFLAGS="-L$PROJECT_ROOT/core/lib -laegisvm -L$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib -lgnat -lgnarl -lm"

    go build -o aegisd ./cmd/aegisd/
    echo -e "${GREEN}✓ aegisd built successfully${NC}"
else
    echo -e "${YELLOW}aegisd already exists, skipping build (use --rebuild to force)${NC}"
fi

#-------------------------------------------------------------------------------
print_step 2 "Initializing node"
#-------------------------------------------------------------------------------

rm -rf "$HOME_DIR"
$AEGISD init "$MONIKER" --chain-id "$CHAIN_ID" --home "$HOME_DIR"
echo -e "${GREEN}✓ Node initialized${NC}"

#-------------------------------------------------------------------------------
print_step 3 "Configuring genesis with ANUBIS Token v4.0"
#-------------------------------------------------------------------------------

cp "$PROJECT_ROOT/cosmos/config/genesis_anubis.json" "$HOME_DIR/config/genesis.json"
sed -i '' "s/\"chain_id\": \"anubis-1\"/\"chain_id\": \"$CHAIN_ID\"/" "$HOME_DIR/config/genesis.json"
echo -e "${GREEN}✓ Genesis configured with ANUBIS allocations${NC}"
echo ""
echo "Token Allocations:"
echo "  • Solo Builder:          300,000,000 ANUBIS (30%)"
echo "  • Protocol Treasury:     300,000,000 ANUBIS (30%)"
echo "  • Genesis Validators:    150,000,000 ANUBIS (15%)"
echo "  • Genesis Provers:        80,000,000 ANUBIS (8%)"
echo "  • Developer Ecosystem:    70,000,000 ANUBIS (7%)"
echo "  • Quantum Insurance:      50,000,000 ANUBIS (5%)"
echo "  • Bug Bounties:           50,000,000 ANUBIS (5%)"
echo "  ─────────────────────────────────────────────"
echo "  • Total:               1,000,000,000 ANUBIS (FIXED, NO INFLATION)"

#-------------------------------------------------------------------------------
print_step 4 "Creating validator account"
#-------------------------------------------------------------------------------

$AEGISD keys add validator --keyring-backend test --home "$HOME_DIR" 2>&1 | tee /tmp/validator_key.txt
VALIDATOR_ADDR=$($AEGISD keys show validator -a --keyring-backend test --home "$HOME_DIR")
echo ""
echo -e "${GREEN}✓ Validator account created${NC}"
echo -e "Address: ${YELLOW}$VALIDATOR_ADDR${NC}"

#-------------------------------------------------------------------------------
print_step 5 "Adding genesis account"
#-------------------------------------------------------------------------------

# Add validator with stake from genesis validators pool
$AEGISD genesis add-genesis-account "$VALIDATOR_ADDR" 100000000000000000000000aanubis --keyring-backend test --home "$HOME_DIR"
echo -e "${GREEN}✓ Genesis account added with 100,000 ANUBIS stake${NC}"

#-------------------------------------------------------------------------------
print_step 6 "Creating gentx"
#-------------------------------------------------------------------------------

$AEGISD genesis gentx validator 100000000000000000000000aanubis \
    --chain-id "$CHAIN_ID" \
    --keyring-backend test \
    --home "$HOME_DIR" \
    --moniker "$MONIKER" \
    --commission-rate "0.10" \
    --commission-max-rate "0.20" \
    --commission-max-change-rate "0.01"
echo -e "${GREEN}✓ Gentx created${NC}"

#-------------------------------------------------------------------------------
print_step 7 "Collecting gentxs"
#-------------------------------------------------------------------------------

$AEGISD genesis collect-gentxs --home "$HOME_DIR"
echo -e "${GREEN}✓ Gentxs collected${NC}"

#-------------------------------------------------------------------------------
print_step 8 "Validating genesis"
#-------------------------------------------------------------------------------

$AEGISD genesis validate-genesis --home "$HOME_DIR"
echo -e "${GREEN}✓ Genesis validated${NC}"

#-------------------------------------------------------------------------------
echo ""
echo -e "${GREEN}╔═══════════════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║                    TESTNET SETUP COMPLETE                                 ║${NC}"
echo -e "${GREEN}╚═══════════════════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "Home directory: ${YELLOW}$HOME_DIR${NC}"
echo -e "Chain ID:       ${YELLOW}$CHAIN_ID${NC}"
echo -e "Validator:      ${YELLOW}$VALIDATOR_ADDR${NC}"
echo ""
echo -e "To start the testnet:"
echo -e "  ${YELLOW}./scripts/start_testnet.sh${NC}"
echo ""
echo -e "Or manually:"
echo -e "  ${YELLOW}$AEGISD start --home $HOME_DIR${NC}"
echo ""
