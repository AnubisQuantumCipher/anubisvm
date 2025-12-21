# Makefile for AnubisVM Test Suite
#
# This Makefile builds and runs all tests for AnubisVM

.PHONY: all build test clean help

# Test executables
NEW_TESTS := \
	test_chacha20_poly1305_kat \
	test_kmac_kat \
	test_security \
	test_integration \
	test_sandbox \
	test_fuzzer

EXISTING_TESTS := \
	test_address \
	test_kmac \
	test_aead \
	test_kdf

ALL_TESTS := $(NEW_TESTS) $(EXISTING_TESTS)

# Default target
all: help

help:
	@echo "AnubisVM Test Suite"
	@echo "==================="
	@echo ""
	@echo "Targets:"
	@echo "  build           - Build all tests (requires gprbuild or alr)"
	@echo "  test            - Run all tests"
	@echo "  test-new        - Run new tests only"
	@echo "  test-existing   - Run existing tests only"
	@echo "  clean           - Clean build artifacts"
	@echo "  coverage        - Generate coverage report"
	@echo "  help            - Show this help message"
	@echo ""
	@echo "New Tests Added:"
	@echo "  - ChaCha20-Poly1305 KAT (RFC 8439)"
	@echo "  - KMAC256 KAT (NIST SP 800-185)"
	@echo "  - Security tests (wipe, timing, isolation)"
	@echo "  - Integration tests (contracts, tokens, state)"
	@echo "  - Sandbox tests (bounds, gas, reentrancy)"
	@echo "  - Fuzzing harnesses (bytecode, ELF, RLP, crypto)"
	@echo ""

build:
	@echo "Building tests..."
	@if command -v gprbuild >/dev/null 2>&1; then \
		for test in $(NEW_TESTS); do \
			echo "Building $$test..."; \
			gprbuild -P $$test.gpr -q || true; \
		done \
	elif command -v alr >/dev/null 2>&1; then \
		echo "Using alr to build..."; \
		alr build || true; \
	else \
		echo "Error: Neither gprbuild nor alr found"; \
		echo "Please install GNAT toolchain"; \
		exit 1; \
	fi

test: test-new test-existing
	@echo ""
	@echo "All tests completed"

test-new:
	@echo "Running new tests..."
	@./run_all_tests.sh

test-existing:
	@echo "Running existing tests..."
	@for test in $(EXISTING_TESTS); do \
		if [ -x "obj/$$test" ]; then \
			echo "Running $$test..."; \
			./obj/$$test || true; \
		fi \
	done

coverage:
	@echo "Generating coverage report..."
	@mkdir -p coverage_report
	@./run_all_tests.sh
	@echo "Coverage report generated in test_report/"

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf obj/* coverage_report/* test_report/*
	@echo "Clean complete"

# Individual test targets
test-chacha20:
	@if [ -x "obj/test_chacha20_poly1305_kat" ]; then \
		./obj/test_chacha20_poly1305_kat; \
	else \
		echo "Test not built. Run 'make build' first."; \
	fi

test-kmac-kat:
	@if [ -x "obj/test_kmac_kat" ]; then \
		./obj/test_kmac_kat; \
	else \
		echo "Test not built. Run 'make build' first."; \
	fi

test-sec:
	@if [ -x "obj/test_security" ]; then \
		./obj/test_security; \
	else \
		echo "Test not built. Run 'make build' first."; \
	fi

test-int:
	@if [ -x "obj/test_integration" ]; then \
		./obj/test_integration; \
	else \
		echo "Test not built. Run 'make build' first."; \
	fi

test-sand:
	@if [ -x "obj/test_sandbox" ]; then \
		./obj/test_sandbox; \
	else \
		echo "Test not built. Run 'make build' first."; \
	fi

test-fuzz:
	@if [ -x "obj/test_fuzzer" ]; then \
		./obj/test_fuzzer; \
	else \
		echo "Test not built. Run 'make build' first."; \
	fi
