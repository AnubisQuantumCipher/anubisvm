# AnubisVM Security Audit Report

Generated: 2025-12-12

## Summary

- **SPARK Coverage**: See flow analysis above
- **Post-Quantum Cryptography**: ML-DSA-87, ML-KEM-1024
- **Address Format**: AAS-001 v3.1 compliant

## Recommendations

1. Review any Unchecked_Conversion usage for safety
2. Ensure all public APIs have SPARK contracts
3. Run GNATprove at level 4 for full verification

## Certification Status

Run `khepri certify --level=gold` for formal certification.
