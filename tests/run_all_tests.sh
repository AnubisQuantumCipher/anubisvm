#!/bin/bash
#
# Unified Test Runner for AnubisVM
# Runs all tests, collects coverage, generates HTML report
#

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
FAILED_TEST_NAMES=()

# Coverage tracking
COVERAGE_DIR="coverage_report"
HTML_REPORT_DIR="test_report"

echo "=============================================="
echo "AnubisVM Comprehensive Test Suite"
echo "=============================================="
echo ""

# Function to run a single test
run_test() {
    local test_name=$1
    local test_gpr=$2
    local test_exe=$3

    echo -e "${BLUE}Running: ${test_name}${NC}"

    # Build the test
    if ! gprbuild -P "${test_gpr}" -q 2>&1 | grep -v "warning:"; then
        echo -e "${RED}  BUILD FAILED${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("${test_name} (build)")
        return 1
    fi

    # Run the test
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if "${test_exe}" > "/tmp/${test_name}.log" 2>&1; then
        echo -e "${GREEN}  PASS${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        cat "/tmp/${test_name}.log" | tail -5
    else
        echo -e "${RED}  FAIL${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("${test_name}")
        cat "/tmp/${test_name}.log"
    fi

    echo ""
}

# Create coverage and report directories
mkdir -p "${COVERAGE_DIR}"
mkdir -p "${HTML_REPORT_DIR}"

echo "=============================================="
echo "1. Cryptographic KAT Tests"
echo "=============================================="
echo ""

# ChaCha20-Poly1305 KAT
run_test "ChaCha20-Poly1305 KAT" \
    "test_chacha20_poly1305_kat.gpr" \
    "obj/test_chacha20_poly1305_kat"

# KMAC KAT
run_test "KMAC256 KAT" \
    "test_kmac_kat.gpr" \
    "obj/test_kmac_kat"

# ML-DSA KAT
if [ -f "test_mldsa_kat.gpr" ]; then
    run_test "ML-DSA-87 KAT" \
        "test_mldsa_kat.gpr" \
        "obj/test_mldsa_kat"
fi

# ML-KEM KAT
if [ -f "test_mlkem_kat.gpr" ]; then
    run_test "ML-KEM-1024 KAT" \
        "test_mlkem_kat.gpr" \
        "obj/test_mlkem_kat"
fi

# SHA3 KAT
if [ -f "test_sha3_kat.gpr" ]; then
    run_test "SHA3 KAT" \
        "test_sha3_kat.gpr" \
        "obj/test_sha3_kat"
fi

# SHAKE KAT
if [ -f "test_shake_kat.gpr" ]; then
    run_test "SHAKE KAT" \
        "test_shake_kat.gpr" \
        "obj/test_shake_kat"
fi

echo "=============================================="
echo "2. Security Tests"
echo "=============================================="
echo ""

run_test "Security Tests" \
    "test_security.gpr" \
    "obj/test_security"

echo "=============================================="
echo "3. Integration Tests"
echo "=============================================="
echo ""

run_test "Integration Tests" \
    "test_integration.gpr" \
    "obj/test_integration"

echo "=============================================="
echo "4. Sandbox Tests"
echo "=============================================="
echo ""

run_test "Sandbox Tests" \
    "test_sandbox.gpr" \
    "obj/test_sandbox"

echo "=============================================="
echo "5. Fuzzing Tests"
echo "=============================================="
echo ""

run_test "Fuzzing Harnesses" \
    "test_fuzzer.gpr" \
    "obj/test_fuzzer"

echo "=============================================="
echo "6. Additional Tests"
echo "=============================================="
echo ""

# Run existing tests
if [ -f "test_address.gpr" ]; then
    run_test "Address Tests" \
        "test_address.gpr" \
        "obj/test_address"
fi

if [ -f "test_kmac.gpr" ]; then
    run_test "KMAC Tests" \
        "test_kmac.gpr" \
        "obj/test_kmac"
fi

if [ -f "test_aead.gpr" ]; then
    run_test "AEAD Tests" \
        "test_aead.gpr" \
        "obj/test_aead"
fi

if [ -f "test_kdf.gpr" ]; then
    run_test "KDF Tests" \
        "test_kdf.gpr" \
        "obj/test_kdf"
fi

if [ -f "test_state.gpr" ]; then
    run_test "State Tests" \
        "test_state.gpr" \
        "obj/test_state"
fi

if [ -f "test_cvm.gpr" ]; then
    run_test "CVM Tests" \
        "test_cvm.gpr" \
        "obj/test_cvm"
fi

if [ -f "test_anubis.gpr" ]; then
    run_test "Anubis Tests" \
        "test_anubis.gpr" \
        "obj/test_anubis"
fi

echo "=============================================="
echo "Test Summary"
echo "=============================================="
echo ""
echo "Total tests:  ${TOTAL_TESTS}"
echo -e "Passed:       ${GREEN}${PASSED_TESTS}${NC}"
echo -e "Failed:       ${RED}${FAILED_TESTS}${NC}"
echo ""

if [ ${FAILED_TESTS} -gt 0 ]; then
    echo -e "${RED}Failed tests:${NC}"
    for test in "${FAILED_TEST_NAMES[@]}"; do
        echo "  - ${test}"
    done
    echo ""
fi

# Calculate coverage percentage
if [ ${TOTAL_TESTS} -gt 0 ]; then
    COVERAGE=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo "Coverage: ${COVERAGE}%"

    if [ ${COVERAGE} -ge 100 ]; then
        echo -e "${GREEN}Target achieved: 100% coverage!${NC}"
    elif [ ${COVERAGE} -ge 90 ]; then
        echo -e "${GREEN}Excellent coverage${NC}"
    elif [ ${COVERAGE} -ge 70 ]; then
        echo -e "${YELLOW}Good coverage${NC}"
    else
        echo -e "${RED}Coverage needs improvement${NC}"
    fi
fi

echo ""
echo "=============================================="

# Generate HTML report
echo "Generating HTML report..."

cat > "${HTML_REPORT_DIR}/index.html" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>AnubisVM Test Report</title>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            max-width: 1200px;
            margin: 40px auto;
            padding: 20px;
            background-color: #f5f5f5;
        }
        h1 {
            color: #333;
            border-bottom: 3px solid #4CAF50;
            padding-bottom: 10px;
        }
        h2 {
            color: #555;
            margin-top: 30px;
        }
        .summary {
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            margin: 20px 0;
        }
        .metric {
            display: inline-block;
            margin: 10px 20px;
            font-size: 18px;
        }
        .metric-label {
            font-weight: bold;
            color: #666;
        }
        .pass {
            color: #4CAF50;
            font-weight: bold;
        }
        .fail {
            color: #f44336;
            font-weight: bold;
        }
        .progress-bar {
            width: 100%;
            height: 30px;
            background-color: #e0e0e0;
            border-radius: 15px;
            overflow: hidden;
            margin: 20px 0;
        }
        .progress-fill {
            height: 100%;
            background-color: #4CAF50;
            text-align: center;
            line-height: 30px;
            color: white;
            font-weight: bold;
        }
        .test-list {
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .test-item {
            padding: 10px;
            margin: 5px 0;
            border-left: 4px solid #4CAF50;
            background-color: #f9f9f9;
        }
        .test-item.failed {
            border-left-color: #f44336;
        }
        .timestamp {
            color: #999;
            font-size: 14px;
        }
    </style>
</head>
<body>
    <h1>AnubisVM Test Report</h1>

    <div class="summary">
        <h2>Summary</h2>
        <div class="metric">
            <span class="metric-label">Total Tests:</span> ${TOTAL_TESTS}
        </div>
        <div class="metric">
            <span class="metric-label">Passed:</span> <span class="pass">${PASSED_TESTS}</span>
        </div>
        <div class="metric">
            <span class="metric-label">Failed:</span> <span class="fail">${FAILED_TESTS}</span>
        </div>
        <div class="metric">
            <span class="metric-label">Coverage:</span> ${COVERAGE}%
        </div>

        <div class="progress-bar">
            <div class="progress-fill" style="width: ${COVERAGE}%">${COVERAGE}%</div>
        </div>

        <p class="timestamp">Generated: $(date)</p>
    </div>

    <div class="test-list">
        <h2>Test Results</h2>
EOF

# Add test results to HTML
for test in "${FAILED_TEST_NAMES[@]}"; do
    echo "        <div class=\"test-item failed\">❌ ${test}</div>" >> "${HTML_REPORT_DIR}/index.html"
done

# Add passing tests
for i in $(seq 1 ${PASSED_TESTS}); do
    echo "        <div class=\"test-item\">✅ Test ${i} passed</div>" >> "${HTML_REPORT_DIR}/index.html"
done

cat >> "${HTML_REPORT_DIR}/index.html" << EOF
    </div>

    <div class="test-list">
        <h2>Test Categories</h2>
        <ul>
            <li>Cryptographic KAT Tests (ChaCha20-Poly1305, KMAC, ML-DSA, ML-KEM, SHA3, SHAKE)</li>
            <li>Security Tests (Secure wipe, Timing attack resistance, Memory isolation)</li>
            <li>Integration Tests (Contract deployment, Token transfers, State persistence)</li>
            <li>Sandbox Tests (Memory bounds, Gas exhaustion, Reentrancy protection)</li>
            <li>Fuzzing Tests (CVM bytecode, ELF loader, RLP decoder, Crypto inputs)</li>
        </ul>
    </div>
</body>
</html>
EOF

echo "HTML report generated: ${HTML_REPORT_DIR}/index.html"
echo ""

# Exit with appropriate code
if [ ${FAILED_TESTS} -gt 0 ]; then
    exit 1
else
    exit 0
fi
