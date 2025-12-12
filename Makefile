# AnubisVM Makefile
# Post-Quantum Trusted Execution Environment
# ML-DSA-87 | ML-KEM-1024 | SPARK Verified

.PHONY: all build prove test clean install uninstall fix-rpath help

# Directories
BIN_DIR := core/bin
OBJ_DIR := obj
INSTALL_DIR := $(HOME)/.local/bin
GNAT_LIB := /Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib

# Alire
ALR := /Users/sicarii/bin/alr

# Default target
all: build fix-rpath

# Build all binaries
build:
	@echo "Building AnubisVM..."
	@$(ALR) build

build-release:
	@echo "Building AnubisVM (release)..."
	@$(ALR) build -- -XBUILD_MODE=release

build-prove:
	@echo "Building AnubisVM (proof mode)..."
	@$(ALR) build -- -XBUILD_MODE=prove

# Run SPARK proofs
prove:
	@echo "Running SPARK proofs (level 2)..."
	@$(ALR) exec -- gnatprove -P anubisvm.gpr --level=2 --report=statistics --warnings=continue -j0

prove-full:
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
	@$(BIN_DIR)/test_kmac 2>&1 | grep -E "Tests|RESULT"
	@$(BIN_DIR)/test_aead 2>&1 | grep -E "Tests|RESULT"
	@$(BIN_DIR)/test_kdf 2>&1 | grep -E "Tests|RESULT"
	@$(BIN_DIR)/test_cvm 2>&1 | grep -E "Tests|RESULT"
	@$(BIN_DIR)/test_anubis 2>&1 | grep -E "Tests|RESULT"
	@$(BIN_DIR)/test_state 2>&1 | grep -E "Tests|RESULT"
	@$(BIN_DIR)/test_tee 2>&1 | grep -E "Tests|RESULT"
	@echo ""
	@echo "KAT Tests"
	@echo "---------"
	@tests/bin/test_sha3_kat 2>&1 | grep "RESULT"
	@tests/bin/test_mldsa 2>&1 | grep "RESULT"
	@tests/bin/test_mlkem_kat 2>&1 | grep "RESULT"
	@tests/bin/test_address 2>&1 | grep "SUCCESS\|FAIL"
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
	@for bin in $(BIN_DIR)/*; do \
		if [ -f "$$bin" ] && [ -x "$$bin" ]; then \
			install_name_tool -delete_rpath "$(GNAT_LIB)" "$$bin" 2>/dev/null || true; \
		fi; \
	done

# Install to ~/.local/bin
install: build fix-rpath
	@echo "Installing AnubisVM..."
	@mkdir -p $(INSTALL_DIR)
	@rm -f $(INSTALL_DIR)/khepri $(INSTALL_DIR)/anubis-node $(INSTALL_DIR)/khepri-local
	@cp $(BIN_DIR)/khepri_main $(INSTALL_DIR)/khepri
	@cp $(BIN_DIR)/anubis_main $(INSTALL_DIR)/anubis-node
	@cp $(BIN_DIR)/khepri_local_main $(INSTALL_DIR)/khepri-local
	@chmod +x $(INSTALL_DIR)/khepri $(INSTALL_DIR)/anubis-node $(INSTALL_DIR)/khepri-local
	@mkdir -p $(HOME)/.anubisvm/keys $(HOME)/.anubisvm/state
	@echo ""
	@echo "Installed:"
	@echo "  $(INSTALL_DIR)/khepri"
	@echo "  $(INSTALL_DIR)/anubis-node"
	@echo "  $(INSTALL_DIR)/khepri-local"
	@echo ""
	@echo "Run 'khepri help' to get started"

# Uninstall
uninstall:
	@rm -f $(INSTALL_DIR)/khepri
	@rm -f $(INSTALL_DIR)/anubis-node
	@rm -f $(INSTALL_DIR)/khepri-local
	@echo "Uninstalled from $(INSTALL_DIR)"

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

# Help
help:
	@echo "AnubisVM Build System"
	@echo "====================="
	@echo ""
	@echo "Build Targets:"
	@echo "  build         Build all binaries (development)"
	@echo "  build-release Build optimized release"
	@echo "  build-prove   Build for proof mode"
	@echo ""
	@echo "Test Targets:"
	@echo "  test          Run all tests (335 tests)"
	@echo "  test-quick    Run quick tests via CLI"
	@echo "  test-kat      Run KAT tests only"
	@echo ""
	@echo "Proof Targets:"
	@echo "  prove         Run SPARK proofs (level 2)"
	@echo "  prove-full    Run full SPARK proofs (level 4)"
	@echo "  prove-report  Show proof statistics"
	@echo ""
	@echo "Install Targets:"
	@echo "  install       Install to ~/.local/bin"
	@echo "  uninstall     Remove installed binaries"
	@echo ""
	@echo "Utility Targets:"
	@echo "  clean         Remove build artifacts"
	@echo "  distclean     Remove all generated files"
	@echo "  fix-rpath     Fix macOS RPATH issues"
	@echo "  format        Format source code"
	@echo "  version       Show version info"
	@echo ""
	@echo "Examples:"
	@echo "  make                  Build project"
	@echo "  make test             Run all tests"
	@echo "  make install          Install CLI tools"
	@echo "  make prove            Verify SPARK contracts"
