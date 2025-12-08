# AegisVM Gas Schedule

**Version:** 1.0.0
**Status:** Draft
**Date:** 2025-12-07

## 1. Overview

Gas is the unit of computation in AegisVM. Every operation consumes gas proportional to its computational complexity and resource usage. This document specifies the gas costs for all operations.

### 1.1 Design Principles

1. **Proportionality**: Gas cost reflects actual computational work
2. **DoS Prevention**: Expensive operations have high gas costs
3. **PQ Premium**: Post-quantum cryptographic operations have additional costs due to larger key/signature sizes
4. **Privacy Premium**: Privacy-preserving operations include overhead for encryption/ZK

### 1.2 Gas Constants

| Constant | Value | Description |
|----------|-------|-------------|
| GAS_LIMIT_TX | 30,000,000 | Maximum gas per transaction |
| GAS_LIMIT_BLOCK | 100,000,000 | Maximum gas per block |
| GAS_PRICE_MIN | 1 gwei | Minimum gas price |
| BASE_FEE | Dynamic | EIP-1559 style base fee |

## 2. Base Operation Costs

### 2.1 Stack Operations

| Operation | Gas | Notes |
|-----------|-----|-------|
| PUSH1-PUSH32 | 3 | Push to stack |
| POP | 2 | Pop from stack |
| DUP1-DUP16 | 3 | Duplicate stack item |
| SWAP1-SWAP16 | 3 | Swap stack items |

### 2.2 Arithmetic Operations

| Operation | Gas | Notes |
|-----------|-----|-------|
| ADD, SUB | 3 | Basic arithmetic |
| MUL, DIV, MOD | 5 | Multiplication/division |
| SDIV, SMOD | 5 | Signed operations |
| ADDMOD, MULMOD | 8 | Modular arithmetic |
| EXP | 10 + 50 * exp_bytes | Exponentiation |
| SIGNEXTEND | 5 | Sign extension |
| LT, GT, SLT, SGT | 3 | Comparisons |
| EQ, ISZERO | 3 | Equality checks |

### 2.3 Bitwise Operations

| Operation | Gas | Notes |
|-----------|-----|-------|
| AND, OR, XOR | 3 | Bitwise logic |
| NOT | 3 | Bitwise negation |
| BYTE | 3 | Extract byte |
| SHL, SHR, SAR | 3 | Shift operations |

### 2.4 Memory Operations

| Operation | Base Gas | Additional | Notes |
|-----------|----------|------------|-------|
| MLOAD | 3 | Memory expansion | Load 32 bytes |
| MSTORE | 3 | Memory expansion | Store 32 bytes |
| MSTORE8 | 3 | Memory expansion | Store 1 byte |
| MSIZE | 2 | - | Get memory size |
| MCOPY | 3 | 3 * words | Copy memory |
| MZERO | 3 | 3 * words | Zero memory |

### 2.5 Memory Expansion Cost

```
expansion_cost(new_words) = 3 * new_words + (new_words^2 / 512)
```

Example costs:
| Memory Size | Words | Cost |
|-------------|-------|------|
| 32 bytes | 1 | 3 |
| 1 KB | 32 | 98 |
| 32 KB | 1024 | 5,120 |
| 1 MB | 32768 | 2,195,456 |

### 2.6 Storage Operations

| Operation | Cold | Warm | Notes |
|-----------|------|------|-------|
| SLOAD | 2,100 | 100 | Read storage |
| SSTORE (0 -> non-0) | 22,100 | 20,000 | Create storage |
| SSTORE (non-0 -> non-0) | 5,000 | 2,900 | Modify storage |
| SSTORE (non-0 -> 0) | 5,000 | 2,900 | +15,000 refund |
| SLOAD_ENC | 5,000 | 500 | Encrypted read |
| SSTORE_ENC | 25,000 | 22,000 | Encrypted write |

### 2.7 Control Flow

| Operation | Gas | Notes |
|-----------|-----|-------|
| JUMP | 8 | Unconditional jump |
| JUMPI | 10 | Conditional jump |
| JUMPDEST | 1 | Jump marker |
| PC | 2 | Get program counter |
| STOP | 0 | Halt execution |
| RETURN | 0 | Return data |
| REVERT | 0 | Revert with data |
| INVALID | All gas | Invalid opcode |

### 2.8 Call Operations

| Operation | Base | Additional | Notes |
|-----------|------|------------|-------|
| CALL | 100 | (see below) | External call |
| CALLCODE | 100 | (see below) | Call with context |
| DELEGATECALL | 100 | (see below) | Delegate call |
| STATICCALL | 100 | (see below) | Read-only call |

Call additional costs:
- Cold account access: +2,600
- Value transfer: +9,000
- New account creation: +25,000
- Memory expansion: (as above)

### 2.9 Environment Operations

| Operation | Gas | Notes |
|-----------|-----|-------|
| ADDRESS | 2 | Get contract address |
| BALANCE | 100 (warm) / 2,600 (cold) | Get balance |
| ORIGIN | 2 | Transaction origin |
| CALLER | 2 | Direct caller |
| CALLVALUE | 2 | Call value |
| CALLDATALOAD | 3 | Load call data |
| CALLDATASIZE | 2 | Call data size |
| CALLDATACOPY | 3 + 3*words | Copy call data |
| CODESIZE | 2 | Code size |
| CODECOPY | 3 + 3*words | Copy code |
| GASPRICE | 2 | Gas price |
| EXTCODESIZE | 100/2,600 | External code size |
| EXTCODECOPY | 100/2,600 + 3*words | Copy external code |
| EXTCODEHASH | 100/2,600 | Code hash |

### 2.10 Block Information

| Operation | Gas | Notes |
|-----------|-----|-------|
| BLOCKHASH | 20 | Get block hash |
| COINBASE | 2 | Block producer |
| TIMESTAMP | 2 | Block time |
| NUMBER | 2 | Block number |
| PREVRANDAO | 2 | Random beacon |
| GASLIMIT | 2 | Gas limit |
| CHAINID | 2 | Chain ID |
| SELFBALANCE | 5 | Own balance |
| BASEFEE | 2 | Base fee |

## 3. Cryptographic Operation Costs

### 3.1 Hash Functions

| Operation | Base | Per Word | Notes |
|-----------|------|----------|-------|
| SHA3-256 | 30 | 6 | Keccak-256 |
| SHA3-512 | 45 | 9 | Keccak-512 |
| SHAKE128 | 35 | 6 + 2*out_words | XOF |
| SHAKE256 | 40 | 7 + 2*out_words | XOF |

### 3.2 ML-KEM (FIPS 203) - Post-Quantum KEM

| Operation | Gas | Notes |
|-----------|-----|-------|
| MLKEM_KEYGEN | 50,000 | Generate key pair (1568+3168 bytes) |
| MLKEM_ENCAP | 35,000 | Encapsulate (1568 byte ciphertext) |
| MLKEM_DECAP | 40,000 | Decapsulate |

Cost breakdown:
- Key sizes: pk=1568, sk=3168 bytes (ML-KEM-1024)
- Ciphertext: 1568 bytes
- Shared secret: 32 bytes

### 3.3 ML-DSA (FIPS 204) - Post-Quantum Signatures

| Operation | Gas | Notes |
|-----------|-----|-------|
| MLDSA_KEYGEN | 75,000 | Generate key pair (2592+4896 bytes) |
| MLDSA_SIGN | 100,000 | Sign message (4627 byte sig) |
| MLDSA_VERIFY | 45,000 | Verify signature |

Cost breakdown:
- Key sizes: pk=2592, sk=4896 bytes (ML-DSA-87)
- Signature: 4627 bytes
- Security level: 256-bit quantum

### 3.4 Lattice Commitments

| Operation | Base | Per Element | Notes |
|-----------|------|-------------|-------|
| AJTAI_COMMIT | 10,000 | 500 | Commit to data |
| AJTAI_OPEN | 15,000 | 600 | Open commitment |
| AJTAI_VERIFY | 12,000 | 400 | Verify opening |

### 3.5 ZK-STARK Verification

| Operation | Base | Per Public Input | Notes |
|-----------|------|------------------|-------|
| ZKVERIFY (small) | 200,000 | 1,000 | < 1KB proof |
| ZKVERIFY (medium) | 500,000 | 1,500 | < 10KB proof |
| ZKVERIFY (large) | 1,000,000 | 2,000 | < 100KB proof |

### 3.6 Symmetric Encryption

| Operation | Base | Per KB | Notes |
|-----------|------|--------|-------|
| CHACHA_ENC | 500 | 200 | Encrypt with Poly1305 |
| CHACHA_DEC | 500 | 200 | Decrypt and verify |

## 4. Privacy Operation Costs

### 4.1 SHIELD Layer

| Operation | Gas | Notes |
|-----------|-----|-------|
| SHIELD_WRAP | 2,000 + 100/KB | Encrypt state |
| SHIELD_UNWRAP | 2,500 + 100/KB | Decrypt state |
| SHIELD_REKEY | 50,000 | Rotate encryption key |

### 4.2 Ring Signatures

| Operation | Base | Per Member | Notes |
|-----------|------|------------|-------|
| RING_SIGN | 20,000 | 5,000 | Lattice ring signature |
| RING_VERIFY | 15,000 | 3,500 | Verify ring signature |

### 4.3 Confidential Messaging (WHISPER)

| Operation | Base | Per KB | Notes |
|-----------|------|--------|-------|
| WHISPER_SEND | 5,000 | 300 | Send encrypted message |
| WHISPER_RECV | 3,000 | 200 | Receive message |

### 4.4 Selective Disclosure (EYE)

| Operation | Gas | Notes |
|-----------|-----|-------|
| EYE_REVEAL | 10,000 + 1,000*fields | Create view token |
| EYE_VERIFY | 8,000 + 500*fields | Verify disclosure |

### 4.5 Private Execution (GATE)

| Operation | Gas | Notes |
|-----------|-----|-------|
| GATE_ENTER | 100,000 | Enter private context |
| GATE_EXIT | 150,000 | Exit with proof |
| GATE_VERIFY | 200,000 | Verify execution proof |

## 5. Logging Operations

| Operation | Base | Per Topic | Per Byte | Notes |
|-----------|------|-----------|----------|-------|
| LOG0 | 375 | - | 8 | No topics |
| LOG1 | 375 | 375 | 8 | 1 topic |
| LOG2 | 375 | 750 | 8 | 2 topics |
| LOG3 | 375 | 1125 | 8 | 3 topics |
| LOG4 | 375 | 1500 | 8 | 4 topics |

## 6. Contract Creation

| Operation | Base | Per Byte | Notes |
|-----------|------|----------|-------|
| CREATE | 32,000 | 200 | Create contract |
| CREATE2 | 32,000 | 200 + 6*words | Create with salt |

Additional costs:
- Code deposit: 200 gas per byte
- Max code size: 24,576 bytes

## 7. Refunds

| Action | Refund | Max Refund |
|--------|--------|------------|
| SSTORE (clear) | 15,000 | 50% of gas used |
| SELFDESTRUCT | 0 | Deprecated |

## 8. Precompiles

### 8.1 Standard Precompiles

| Address | Operation | Base | Per Word |
|---------|-----------|------|----------|
| 0x01 | ECRECOVER | 3,000 | - |
| 0x02 | SHA256 | 60 | 12 |
| 0x03 | RIPEMD160 | 600 | 120 |
| 0x04 | IDENTITY | 15 | 3 |
| 0x05 | MODEXP | Dynamic | - |

### 8.2 Post-Quantum Precompiles

| Address | Operation | Gas |
|---------|-----------|-----|
| 0x100 | MLKEM_FULL | 125,000 |
| 0x101 | MLDSA_FULL | 220,000 |
| 0x102 | STARK_VERIFY | 500,000 |
| 0x103 | AJTAI_BATCH | 100,000 |

### 8.3 Privacy Precompiles

| Address | Operation | Gas |
|---------|-----------|-----|
| 0x200 | SHIELD_BATCH | 50,000 |
| 0x201 | RING_BATCH | 100,000 |
| 0x202 | WHISPER_ROUTE | 30,000 |
| 0x203 | EYE_BATCH | 40,000 |

## 9. Gas Price Mechanism

### 9.1 EIP-1559 Style Pricing

```
gas_price = base_fee + priority_fee
effective_gas_price = min(gas_price, max_fee_per_gas)
```

### 9.2 Base Fee Adjustment

```
if gas_used > target_gas:
    base_fee_next = base_fee * (1 + 0.125 * (gas_used - target_gas) / target_gas)
else:
    base_fee_next = base_fee * (1 - 0.125 * (target_gas - gas_used) / target_gas)
```

### 9.3 Priority Fee

- Minimum: 0
- Recommended: 1-2 gwei for normal
- High: 10+ gwei for urgent

## 10. Gas Estimation

### 10.1 Estimation Formula

```
estimated_gas = base_cost + operation_costs + memory_expansion + call_overhead
safety_margin = estimated_gas * 1.2  -- 20% buffer
```

### 10.2 Common Patterns

| Pattern | Typical Gas |
|---------|-------------|
| Simple transfer | 21,000 |
| Token transfer (warm) | 35,000 |
| Token transfer (cold) | 50,000 |
| NFT mint | 80,000 |
| DEX swap | 150,000 |
| Complex DeFi | 300,000+ |
| ZK verification | 500,000+ |
| Private execution | 500,000+ |

## 11. Intrinsic Gas

Transaction intrinsic costs:
| Component | Gas |
|-----------|-----|
| Base | 21,000 |
| Per zero byte | 4 |
| Per non-zero byte | 16 |
| Contract creation | +32,000 |
| Access list entry | +2,400 |
| Access list storage | +1,900 |

## 12. Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-12-07 | Initial specification |
