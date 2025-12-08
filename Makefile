.PHONY: all build prove test clean format help

# Default target
all: build

help:
	@echo "AnubisVM Build System"
	@echo "===================="
	@echo ""
	@echo "Targets:"
	@echo "  build        - Build project (development mode)"
	@echo "  build-prove  - Build for proof mode"
	@echo "  build-release- Build optimized release"
	@echo "  prove        - Run SPARK proofs (level 4)"
	@echo "  prove-quick  - Run quick proofs (level 2)"
	@echo "  test         - Run all tests"
	@echo "  test-kat     - Run Known Answer Tests"
	@echo "  clean        - Clean build artifacts"
	@echo "  format       - Format source code"
	@echo ""

# Build targets
build:
	gprbuild -P anubisvm.gpr -XBUILD_MODE=development -j0

build-prove:
	gprbuild -P anubisvm.gpr -XBUILD_MODE=prove -j0

build-release:
	gprbuild -P anubisvm.gpr -XBUILD_MODE=release -j0

# Proof targets
prove: build-prove
	gnatprove -P anubisvm.gpr --level=4 --proof=all --timeout=60 --report=all

prove-quick: build-prove
	gnatprove -P anubisvm.gpr --level=2 --proof=per_path --timeout=10

prove-report:
	gnatprove -P anubisvm.gpr --report=statistics --output-header

# Test targets
test: build
	@echo "Running test suite..."
	# Will add test runners as we build them

test-kat: build
	@echo "Running Known Answer Tests..."
	# Will add KAT tests in Phase 5

# Utility targets
clean:
	gnatclean -P anubisvm.gpr
	rm -rf obj/ gnatprove/

format:
	@echo "Code formatting (will add gnatpp when ready)"

# Development helpers
check-syntax:
	gcc -c -gnatc -gnatwa core/src/**/*.adb

list-units:
	@find core/src -name "*.ads" | sort
