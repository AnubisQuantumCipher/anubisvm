# AegisVM Core Library Makefile
#
# Builds the SPARK/Ada core as a shared library for CGO integration.
# Produces libaegisvm.so (Linux), libaegisvm.dylib (macOS), or aegisvm.dll (Windows)

# Platform detection
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    PLATFORM := linux
    LIB_EXT := .so
    SHARED_FLAGS := -shared -fPIC
endif
ifeq ($(UNAME_S),Darwin)
    PLATFORM := darwin
    LIB_EXT := .dylib
    SHARED_FLAGS := -dynamiclib -fPIC
endif

# Directories
ROOT_DIR := $(shell pwd)
SRC_DIR := $(ROOT_DIR)/src
OBJ_DIR := $(ROOT_DIR)/obj
LIB_DIR := $(ROOT_DIR)/lib
INCLUDE_DIR := $(ROOT_DIR)/include

# GNAT/GPRbuild configuration
GPRBUILD := gprbuild
GNATPROVE := gnatprove
GNATMAKE := gnatmake
GNAT_FLAGS := -j0 -p

# Library name
LIB_NAME := aegisvm
LIB_FILE := $(LIB_DIR)/lib$(LIB_NAME)$(LIB_EXT)

# GPR project file
GPR_FILE := $(ROOT_DIR)/aegisvm.gpr

# Alire integration (if available)
ALR := $(shell command -v alr 2> /dev/null)
ifdef ALR
    GPRBUILD := alr exec -- gprbuild
    GNATPROVE := alr exec -- gnatprove
endif

# Default target
.PHONY: all
all: lib

# Create directories
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

# Build the shared library
.PHONY: lib
lib: $(OBJ_DIR) $(LIB_DIR)
	$(GPRBUILD) $(GNAT_FLAGS) -P $(GPR_FILE) -XLIBRARY_TYPE=relocatable
	@echo "Built: $(LIB_FILE)"

# Build static library
.PHONY: lib-static
lib-static: $(OBJ_DIR) $(LIB_DIR)
	$(GPRBUILD) $(GNAT_FLAGS) -P $(GPR_FILE) -XLIBRARY_TYPE=static
	@echo "Built static library"

# Build debug version
.PHONY: debug
debug: $(OBJ_DIR) $(LIB_DIR)
	$(GPRBUILD) $(GNAT_FLAGS) -P $(GPR_FILE) -XLIBRARY_TYPE=relocatable -XBUILD_MODE=debug
	@echo "Built debug library"

# Run SPARK proofs
.PHONY: prove
prove:
	$(GNATPROVE) -P $(GPR_FILE) --level=2 --report=all

# Run full SPARK proofs (takes longer)
.PHONY: prove-full
prove-full:
	$(GNATPROVE) -P $(GPR_FILE) --level=4 --proof=all --report=all --timeout=120

# Run unit tests
.PHONY: test
test: lib test-ffi test-e2e test-privacy test-scarab

# Build and run FFI tests
.PHONY: test-ffi
test-ffi: $(OBJ_DIR) $(LIB_DIR)
	mkdir -p $(OBJ_DIR)/tests $(ROOT_DIR)/bin
	$(GPRBUILD) $(GNAT_FLAGS) -P $(ROOT_DIR)/tests/test_ffi.gpr
	LD_LIBRARY_PATH=$(LIB_DIR):$$LD_LIBRARY_PATH DYLD_LIBRARY_PATH=$(LIB_DIR):$$DYLD_LIBRARY_PATH $(ROOT_DIR)/bin/test_aegis_ffi

# Build and run E2E contract tests
.PHONY: test-e2e
test-e2e: $(OBJ_DIR) $(LIB_DIR)
	mkdir -p $(OBJ_DIR)/tests $(ROOT_DIR)/bin
	$(GPRBUILD) $(GNAT_FLAGS) -P $(ROOT_DIR)/tests/test_contract_e2e.gpr
	LD_LIBRARY_PATH=$(LIB_DIR):$$LD_LIBRARY_PATH DYLD_LIBRARY_PATH=$(LIB_DIR):$$DYLD_LIBRARY_PATH $(ROOT_DIR)/bin/test_contract_e2e

# Build and run privacy layer integration tests
.PHONY: test-privacy
test-privacy: $(OBJ_DIR) $(LIB_DIR)
	mkdir -p $(OBJ_DIR)/tests $(ROOT_DIR)/bin
	$(GPRBUILD) $(GNAT_FLAGS) -P $(ROOT_DIR)/tests/test_privacy.gpr
	LD_LIBRARY_PATH=$(LIB_DIR):$$LD_LIBRARY_PATH DYLD_LIBRARY_PATH=$(LIB_DIR):$$DYLD_LIBRARY_PATH $(ROOT_DIR)/bin/test_privacy_integration

# Build and run SCARAB proof aggregation stress tests
.PHONY: test-scarab
test-scarab: $(OBJ_DIR) $(LIB_DIR)
	mkdir -p $(OBJ_DIR)/tests $(ROOT_DIR)/bin
	$(GPRBUILD) $(GNAT_FLAGS) -P $(ROOT_DIR)/tests/test_scarab.gpr
	LD_LIBRARY_PATH=$(LIB_DIR):$$LD_LIBRARY_PATH DYLD_LIBRARY_PATH=$(LIB_DIR):$$DYLD_LIBRARY_PATH timeout 300 $(ROOT_DIR)/bin/test_scarab_stress

# Build and run constant-time verification tests
.PHONY: test-timing
test-timing: $(OBJ_DIR) $(LIB_DIR)
	mkdir -p $(ROOT_DIR)/tests/timing/obj $(ROOT_DIR)/bin
	$(GPRBUILD) $(GNAT_FLAGS) -P $(ROOT_DIR)/tests/timing/test_ct.gpr
	$(ROOT_DIR)/bin/test_constant_time

# Run all integration tests
.PHONY: test-integration
test-integration: test-e2e test-privacy test-scarab test-timing
	@echo "All integration tests passed!"

# Clean build artifacts
.PHONY: clean
clean:
	rm -rf $(OBJ_DIR)/*.o $(OBJ_DIR)/*.ali
	rm -rf gnatprove

# Clean everything
.PHONY: distclean
distclean: clean
	rm -rf $(OBJ_DIR) $(LIB_DIR)

# Install library and headers
.PHONY: install
install: lib
	install -d $(DESTDIR)/usr/local/lib
	install -d $(DESTDIR)/usr/local/include
	install -m 644 $(LIB_FILE) $(DESTDIR)/usr/local/lib/
	install -m 644 $(INCLUDE_DIR)/aegisvm.h $(DESTDIR)/usr/local/include/

# Format Ada source code
.PHONY: format
format:
	find $(SRC_DIR) -name "*.ads" -o -name "*.adb" | xargs gnatformat -q

# Generate documentation
.PHONY: docs
docs:
	gnatdoc -P $(GPR_FILE)

# Check compilation without linking
.PHONY: check
check:
	$(GPRBUILD) $(GNAT_FLAGS) -P $(GPR_FILE) -c

# Show help
.PHONY: help
help:
	@echo "AegisVM Core Library Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all              Build shared library (default)"
	@echo "  lib              Build shared library"
	@echo "  lib-static       Build static library"
	@echo "  debug            Build debug version"
	@echo "  prove            Run SPARK proofs (level 2)"
	@echo "  prove-full       Run full SPARK proofs (level 4)"
	@echo "  test             Build and run all tests"
	@echo "  test-ffi         Build and run FFI integration tests"
	@echo "  test-e2e         Build and run E2E contract tests"
	@echo "  test-privacy     Build and run privacy layer tests"
	@echo "  test-scarab      Build and run SCARAB aggregation tests"
	@echo "  test-timing      Build and run constant-time verification tests"
	@echo "  test-integration Run all integration tests"
	@echo "  clean            Clean object files"
	@echo "  distclean        Clean all generated files"
	@echo "  install          Install library and headers"
	@echo "  format           Format Ada source code"
	@echo "  docs             Generate documentation"
	@echo "  check            Check compilation"
	@echo "  help             Show this help"
	@echo ""
	@echo "Variables:"
	@echo "  DESTDIR     Installation prefix (default: empty)"
	@echo ""
	@echo "Platform: $(PLATFORM)"
	@echo "Library:  $(LIB_FILE)"
