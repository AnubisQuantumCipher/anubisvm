#!/bin/bash
# setup.sh - Setup AnubisVM environment
#
# This script:
# 1. Creates ~/.anubisvm directory structure
# 2. Creates symlinks to binaries in /usr/local/bin (or ~/bin)
# 3. Sets up environment variables
# 4. Optionally adds to shell profile

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ANUBISVM_ROOT="$SCRIPT_DIR/.."
BIN_DIR="$ANUBISVM_ROOT/core/bin"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}AnubisVM Setup${NC}"
echo "=============="
echo ""

# 1. Create directory structure
echo -e "${BLUE}Creating ~/.anubisvm directory structure...${NC}"
mkdir -p ~/.anubisvm/{state,contracts,keys,config,logs}
echo -e "${GREEN}✓${NC} Created ~/.anubisvm/"

# 2. Fix RPATH issues on macOS
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo ""
    echo -e "${BLUE}Fixing RPATH entries for macOS...${NC}"
    "$SCRIPT_DIR/fix_rpath.sh" "$BIN_DIR"
fi

# 3. Create symlinks
echo ""
echo -e "${BLUE}Creating symlinks...${NC}"

# Determine target directory
if [ -w /usr/local/bin ]; then
    LINK_DIR="/usr/local/bin"
else
    mkdir -p "$HOME/bin"
    LINK_DIR="$HOME/bin"
fi

# Create symlinks
ln -sf "$BIN_DIR/khepri_main" "$LINK_DIR/khepri" 2>/dev/null && \
    echo -e "${GREEN}✓${NC} Created $LINK_DIR/khepri" || \
    echo -e "${YELLOW}!${NC} Could not create $LINK_DIR/khepri"

ln -sf "$BIN_DIR/khepri_local_main" "$LINK_DIR/khepri-local" 2>/dev/null && \
    echo -e "${GREEN}✓${NC} Created $LINK_DIR/khepri-local" || \
    echo -e "${YELLOW}!${NC} Could not create $LINK_DIR/khepri-local"

ln -sf "$BIN_DIR/anubis_main" "$LINK_DIR/anubis" 2>/dev/null && \
    echo -e "${GREEN}✓${NC} Created $LINK_DIR/anubis" || \
    echo -e "${YELLOW}!${NC} Could not create $LINK_DIR/anubis"

# 4. Generate shell profile snippet
echo ""
echo -e "${BLUE}Environment configuration:${NC}"
echo ""
cat << 'PROFILE_EOF'
# Add to your ~/.zshrc or ~/.bashrc:

# AnubisVM Configuration
export ANUBISVM_HOME="$HOME/.anubisvm"
export ANUBISVM_STATE="$ANUBISVM_HOME/state"
export ANUBISVM_KEYS="$ANUBISVM_HOME/keys"

PROFILE_EOF

if [ "$LINK_DIR" = "$HOME/bin" ]; then
    echo "# Add ~/bin to PATH (if not already)"
    echo 'export PATH="$HOME/bin:$PATH"'
    echo ""
fi

# 5. Ask to add to profile
echo ""
read -p "Add to shell profile (~/.zshrc)? [y/N] " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    PROFILE_FILE="$HOME/.zshrc"
    if [ ! -f "$PROFILE_FILE" ]; then
        PROFILE_FILE="$HOME/.bashrc"
    fi

    # Check if already configured
    if grep -q "ANUBISVM_HOME" "$PROFILE_FILE" 2>/dev/null; then
        echo -e "${YELLOW}!${NC} AnubisVM already configured in $PROFILE_FILE"
    else
        cat >> "$PROFILE_FILE" << 'EOF'

# AnubisVM Configuration
export ANUBISVM_HOME="$HOME/.anubisvm"
export ANUBISVM_STATE="$ANUBISVM_HOME/state"
export ANUBISVM_KEYS="$ANUBISVM_HOME/keys"
EOF

        if [ "$LINK_DIR" = "$HOME/bin" ]; then
            echo 'export PATH="$HOME/bin:$PATH"' >> "$PROFILE_FILE"
        fi

        echo -e "${GREEN}✓${NC} Added to $PROFILE_FILE"
        echo ""
        echo -e "${YELLOW}Run: source $PROFILE_FILE${NC}"
    fi
fi

echo ""
echo -e "${GREEN}Setup complete!${NC}"
echo ""
echo "Verify installation:"
echo "  khepri --version"
echo "  khepri-local HelloCounter Increment"
echo "  khepri address help"
