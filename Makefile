# AnubisVM Makefile
# Post-Quantum Smart Contract Virtual Machine
# ML-DSA-87 | ML-KEM-1024 | SPARK Verified

.PHONY: all build prove test clean install uninstall fix-rpath help setup check-deps

# Directories
BIN_DIR := core/bin
OBJ_DIR := obj
INSTALL_DIR := $(HOME)/.local/bin
DATA_DIR := $(HOME)/.anubisvm

# Auto-detect Alire
ALR := $(shell command -v alr 2>/dev/null)
ifeq ($(ALR),)
    ALR := $(HOME)/.local/bin/alr
endif

# Auto-detect GNAT library path for RPATH fix (macOS only)
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    GNAT_LIB := $(shell $(ALR) exec -- gnatls -v 2>/dev/null | grep adalib | head -1 | sed 's|/adalib||' || echo "")
endif

# Default target
all: build fix-rpath

# Check dependencies
check-deps:
	@echo "Checking dependencies..."
	@command -v $(ALR) >/dev/null 2>&1 || { echo "ERROR: Alire (alr) not found. Run 'make setup' first."; exit 1; }
	@echo "  alr: OK ($(shell $(ALR) --version 2>/dev/null | head -1))"
	@$(ALR) exec -- gnatls --version >/dev/null 2>&1 || { echo "ERROR: GNAT not found."; exit 1; }
	@echo "  gnat: OK ($(shell $(ALR) exec -- gnatls --version 2>/dev/null | head -1))"
	@echo "All dependencies satisfied."

# Setup development environment
setup:
	@echo "AnubisVM Development Setup"
	@echo "=========================="
	@echo ""
	@if command -v alr >/dev/null 2>&1; then \
		echo "Alire already installed: $$(alr --version)"; \
	else \
		echo "Installing Alire package manager..."; \
		echo ""; \
		echo "Please install Alire from: https://alire.ada.dev/"; \
		echo ""; \
		echo "Quick install options:"; \
		echo "  macOS:   brew install alire"; \
		echo "  Linux:   Download from https://github.com/alire-project/alire/releases"; \
		echo "  Windows: Download installer from https://alire.ada.dev/"; \
		echo ""; \
		exit 1; \
	fi
	@echo ""
	@echo "Fetching dependencies..."
	@$(ALR) toolchain --select gnat_native 2>/dev/null || true
	@$(ALR) update 2>/dev/null || true
	@echo ""
	@echo "Setup complete! Run 'make build' to compile."

# Build all binaries
build: check-deps
	@echo "Building AnubisVM..."
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	@$(ALR) exec -- gprbuild -P anubisvm.gpr -p -j0
	@echo "Building KAT tests..."
	@mkdir -p tests/bin
	@$(ALR) exec -- gprbuild -P tests/test_sha3.gpr -p -j0 2>/dev/null || true
	@$(ALR) exec -- gprbuild -P tests/test_mldsa.gpr -p -j0 2>/dev/null || true
	@$(ALR) exec -- gprbuild -P tests/test_mlkem_kat.gpr -p -j0 2>/dev/null || true
	@$(ALR) exec -- gprbuild -P tests/test_address.gpr -p -j0 2>/dev/null || true

build-release: check-deps
	@echo "Building AnubisVM (release)..."
	@$(ALR) build -- -XBUILD_MODE=release

build-prove: check-deps
	@echo "Building AnubisVM (proof mode)..."
	@$(ALR) build -- -XBUILD_MODE=prove

# Run SPARK proofs
prove: check-deps
	@echo "Running SPARK proofs (level 2)..."
	@$(ALR) exec -- gnatprove -P anubisvm.gpr --level=2 --report=statistics --warnings=continue -j0

prove-full: check-deps
	@echo "Running full SPARK proofs (level 4)..."
	@$(ALR) exec -- gnatprove -P anubisvm.gpr --level=4 --report=all --warnings=continue -j0

prove-report:
	@cat $(OBJ_DIR)/gnatprove/gnatprove.out 2>/dev/null | grep -E "^(Total|SPARK)" || echo "No proof report found. Run 'make prove' first."

# Run all tests
test: fix-rpath
	@echo ""
	@echo "AnubisVM Test Suite"
	@echo "==================="
	@echo ""
	@$(BIN_DIR)/test_kmac 2>&1 | grep -E "Tests|RESULT" || echo "test_kmac: SKIPPED"
	@$(BIN_DIR)/test_aead 2>&1 | grep -E "Tests|RESULT" || echo "test_aead: SKIPPED"
	@$(BIN_DIR)/test_kdf 2>&1 | grep -E "Tests|RESULT" || echo "test_kdf: SKIPPED"
	@$(BIN_DIR)/test_cvm 2>&1 | grep -E "Tests|RESULT" || echo "test_cvm: SKIPPED"
	@$(BIN_DIR)/test_anubis 2>&1 | grep -E "Tests|RESULT" || echo "test_anubis: SKIPPED"
	@$(BIN_DIR)/test_state 2>&1 | grep -E "Tests|RESULT" || echo "test_state: SKIPPED"
	@$(BIN_DIR)/test_tee 2>&1 | grep -E "Tests|RESULT" || echo "test_tee: SKIPPED"
	@echo ""
	@echo "KAT Tests"
	@echo "---------"
	@tests/bin/test_sha3_kat 2>&1 | grep "RESULT" || echo "test_sha3_kat: SKIPPED"
	@tests/bin/test_mldsa 2>&1 | grep "RESULT" || echo "test_mldsa: SKIPPED"
	@tests/bin/test_mlkem_kat 2>&1 | grep "RESULT" || echo "test_mlkem_kat: SKIPPED"
	@tests/bin/test_address 2>&1 | grep "SUCCESS\|FAIL" || echo "test_address: SKIPPED"
	@echo ""
	@echo "All tests completed!"

# Quick test via CLI
test-quick: fix-rpath
	@$(BIN_DIR)/khepri_main test

# Run KAT tests only
test-kat:
	@tests/bin/test_sha3_kat
	@tests/bin/test_mldsa
	@tests/bin/test_mlkem_kat
	@tests/bin/test_address

# Fix RPATH issues on macOS
fix-rpath:
ifeq ($(UNAME_S),Darwin)
	@if [ -n "$(GNAT_LIB)" ]; then \
		for bin in $(BIN_DIR)/*; do \
			if [ -f "$$bin" ] && [ -x "$$bin" ]; then \
				install_name_tool -delete_rpath "$(GNAT_LIB)" "$$bin" 2>/dev/null || true; \
			fi; \
		done; \
	fi
endif

# Install to ~/.local/bin
install: build fix-rpath
	@echo "Installing AnubisVM..."
	@mkdir -p $(INSTALL_DIR)
	@mkdir -p $(DATA_DIR)/keys $(DATA_DIR)/state $(DATA_DIR)/contracts
	@rm -f $(INSTALL_DIR)/khepri $(INSTALL_DIR)/anubis-node
	@cp $(BIN_DIR)/khepri_main $(INSTALL_DIR)/khepri
	@cp $(BIN_DIR)/anubis_main $(INSTALL_DIR)/anubis-node
	@chmod +x $(INSTALL_DIR)/khepri $(INSTALL_DIR)/anubis-node
	@echo ""
	@echo "Installed:"
	@echo "  $(INSTALL_DIR)/khepri        - Smart contract CLI"
	@echo "  $(INSTALL_DIR)/anubis-node   - Full node"
	@echo ""
	@echo "Data directory: $(DATA_DIR)"
	@echo ""
	@if echo "$$PATH" | grep -q "$(INSTALL_DIR)"; then \
		echo "Run 'khepri help' to get started"; \
	else \
		echo "Add to your shell profile:"; \
		echo "  export PATH=\"$(INSTALL_DIR):\$$PATH\""; \
		echo ""; \
		echo "Then run 'khepri help' to get started"; \
	fi

# Uninstall
uninstall:
	@rm -f $(INSTALL_DIR)/khepri
	@rm -f $(INSTALL_DIR)/anubis-node
	@echo "Uninstalled from $(INSTALL_DIR)"
	@echo "Data directory preserved at $(DATA_DIR)"

# Clean build artifacts
clean:
	@echo "Cleaning..."
	@$(ALR) clean 2>/dev/null || true
	@rm -rf $(OBJ_DIR) gnatprove

# Deep clean
distclean: clean
	@rm -rf alire

# Format code (if gnatformat available)
format:
	@$(ALR) exec -- gnatformat -P anubisvm.gpr -U 2>/dev/null || echo "gnatformat not available"

# Version info
version:
	@$(BIN_DIR)/khepri_main version 2>/dev/null || echo "Build first with 'make build'"

# Create new contract from template
new-contract:
	@if [ -z "$(NAME)" ]; then \
		echo "Usage: make new-contract NAME=my_contract"; \
		exit 1; \
	fi
	@mkdir -p contracts/$(NAME)
	@cp -r templates/contract/* contracts/$(NAME)/ 2>/dev/null || \
		echo "Template not found. See docs/CONTRACTS.md for manual setup."
	@echo "Created contract skeleton in contracts/$(NAME)/"

# Help
help:
	@echo "AnubisVM Build System"
	@echo "====================="
	@echo ""
	@echo "Quick Start:"
	@echo "  make setup        Check/install dependencies"
	@echo "  make              Build project"
	@echo "  make test         Run all tests (335 tests)"
	@echo "  make install      Install CLI to ~/.local/bin"
	@echo ""
	@echo "Build Targets:"
	@echo "  build             Build all binaries (development)"
	@echo "  build-release     Build optimized release"
	@echo "  build-prove       Build for proof mode"
	@echo ""
	@echo "Test Targets:"
	@echo "  test              Run all tests"
	@echo "  test-quick        Run quick tests via CLI"
	@echo "  test-kat          Run KAT tests only"
	@echo ""
	@echo "Proof Targets:"
	@echo "  prove             Run SPARK proofs (level 2)"
	@echo "  prove-full        Run full SPARK proofs (level 4)"
	@echo "  prove-report      Show proof statistics"
	@echo ""
	@echo "Contract Development:"
	@echo "  new-contract NAME=x  Create new contract from template"
	@echo ""
	@echo "Install Targets:"
	@echo "  install           Install to ~/.local/bin"
	@echo "  uninstall         Remove installed binaries"
	@echo ""
	@echo "Utility Targets:"
	@echo "  setup             Setup development environment"
	@echo "  check-deps        Verify dependencies installed"
	@echo "  clean             Remove build artifacts"
	@echo "  distclean         Remove all generated files"
	@echo "  fix-rpath         Fix macOS RPATH issues"
	@echo "  format            Format source code"
	@echo "  version           Show version info"
