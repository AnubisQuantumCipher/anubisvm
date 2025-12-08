# AegisVM Bytecode Specification

**Version:** 1.0.0
**Status:** Draft
**Date:** 2025-12-07

## 1. Overview

AegisVM uses a stack-based bytecode architecture with 256 opcodes. The VM is designed for:
- Post-quantum security (all cryptographic operations use PQ algorithms)
- Formal verification (SPARK/Ada core with proven memory safety)
- Deterministic execution (identical results across all nodes)
- Privacy-preserving computation (SHIELD encrypted state)

## 2. Instruction Format

### 2.1 Basic Format

```
+--------+--------+--------+--------+
| Opcode |  Arg1  |  Arg2  |  Arg3  |
+--------+--------+--------+--------+
  1 byte   1-32 bytes (variable)
```

### 2.2 Opcode Encoding

| Bits 7-6 | Category |
|----------|----------|
| 00 | Stack/Memory |
| 01 | Arithmetic |
| 10 | Control Flow |
| 11 | Crypto/System |

### 2.3 Argument Types

| Type | Size | Description |
|------|------|-------------|
| IMM8 | 1 byte | Immediate 8-bit value |
| IMM16 | 2 bytes | Immediate 16-bit value (LE) |
| IMM32 | 4 bytes | Immediate 32-bit value (LE) |
| IMM256 | 32 bytes | Immediate 256-bit value (LE) |
| ADDR | 32 bytes | Account address |
| OFFSET | 2 bytes | Jump offset (signed) |

## 3. Opcode Reference

### 3.1 Stack Operations (0x00-0x1F)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0x00 | STOP | - | [] -> [] | Halt execution |
| 0x01 | PUSH1 | IMM8 | [] -> [v] | Push 1-byte value |
| 0x02 | PUSH2 | IMM16 | [] -> [v] | Push 2-byte value |
| 0x03 | PUSH4 | IMM32 | [] -> [v] | Push 4-byte value |
| 0x04 | PUSH32 | IMM256 | [] -> [v] | Push 32-byte value |
| 0x05 | POP | - | [a] -> [] | Pop top of stack |
| 0x06 | DUP1 | - | [a] -> [a,a] | Duplicate top |
| 0x07 | DUP2 | - | [a,b] -> [a,b,a] | Duplicate 2nd item |
| 0x08-0x0F | DUP3-DUP10 | - | ... | Duplicate Nth item |
| 0x10 | SWAP1 | - | [a,b] -> [b,a] | Swap top 2 |
| 0x11-0x1F | SWAP2-SWAP16 | - | ... | Swap top with Nth |

### 3.2 Memory Operations (0x20-0x3F)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0x20 | MLOAD | - | [addr] -> [v] | Load 32 bytes from memory |
| 0x21 | MSTORE | - | [addr,v] -> [] | Store 32 bytes to memory |
| 0x22 | MSTORE8 | - | [addr,v] -> [] | Store 1 byte to memory |
| 0x23 | MSIZE | - | [] -> [size] | Get memory size |
| 0x24 | MCOPY | - | [dst,src,len] -> [] | Copy memory region |
| 0x25 | MZERO | - | [addr,len] -> [] | Zero memory region |
| 0x30 | SLOAD | - | [key] -> [v] | Load from storage |
| 0x31 | SSTORE | - | [key,v] -> [] | Store to storage |
| 0x32 | SLOAD_ENC | - | [key] -> [v] | Load encrypted (SHIELD) |
| 0x33 | SSTORE_ENC | - | [key,v] -> [] | Store encrypted (SHIELD) |

### 3.3 Arithmetic Operations (0x40-0x5F)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0x40 | ADD | - | [a,b] -> [a+b] | Addition |
| 0x41 | SUB | - | [a,b] -> [a-b] | Subtraction |
| 0x42 | MUL | - | [a,b] -> [a*b] | Multiplication |
| 0x43 | DIV | - | [a,b] -> [a/b] | Division |
| 0x44 | MOD | - | [a,b] -> [a%b] | Modulo |
| 0x45 | SDIV | - | [a,b] -> [a/b] | Signed division |
| 0x46 | SMOD | - | [a,b] -> [a%b] | Signed modulo |
| 0x47 | ADDMOD | - | [a,b,N] -> [(a+b)%N] | Modular addition |
| 0x48 | MULMOD | - | [a,b,N] -> [(a*b)%N] | Modular multiplication |
| 0x49 | EXP | - | [a,b] -> [a^b] | Exponentiation |
| 0x4A | SIGNEXTEND | - | [b,x] -> [y] | Sign extend |
| 0x50 | LT | - | [a,b] -> [a<b] | Less than |
| 0x51 | GT | - | [a,b] -> [a>b] | Greater than |
| 0x52 | SLT | - | [a,b] -> [a<b] | Signed less than |
| 0x53 | SGT | - | [a,b] -> [a>b] | Signed greater than |
| 0x54 | EQ | - | [a,b] -> [a==b] | Equality |
| 0x55 | ISZERO | - | [a] -> [a==0] | Is zero |

### 3.4 Bitwise Operations (0x60-0x6F)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0x60 | AND | - | [a,b] -> [a&b] | Bitwise AND |
| 0x61 | OR | - | [a,b] -> [a\|b] | Bitwise OR |
| 0x62 | XOR | - | [a,b] -> [a^b] | Bitwise XOR |
| 0x63 | NOT | - | [a] -> [~a] | Bitwise NOT |
| 0x64 | BYTE | - | [i,x] -> [x[i]] | Get byte |
| 0x65 | SHL | - | [s,v] -> [v<<s] | Shift left |
| 0x66 | SHR | - | [s,v] -> [v>>s] | Shift right (logical) |
| 0x67 | SAR | - | [s,v] -> [v>>s] | Shift right (arithmetic) |

### 3.5 Control Flow (0x80-0x9F)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0x80 | JUMP | - | [dst] -> [] | Unconditional jump |
| 0x81 | JUMPI | - | [dst,cond] -> [] | Conditional jump |
| 0x82 | JUMPDEST | - | [] -> [] | Jump destination marker |
| 0x83 | PC | - | [] -> [pc] | Get program counter |
| 0x84 | CALL | - | [gas,addr,val,in,insz,out,outsz] -> [success] | Call contract |
| 0x85 | CALLCODE | - | [...] -> [success] | Call with caller's context |
| 0x86 | DELEGATECALL | - | [...] -> [success] | Delegate call |
| 0x87 | STATICCALL | - | [...] -> [success] | Read-only call |
| 0x88 | RETURN | - | [offset,len] -> [] | Return from execution |
| 0x89 | REVERT | - | [offset,len] -> [] | Revert with data |
| 0x8A | INVALID | - | [] -> [] | Invalid opcode (consumes all gas) |

### 3.6 Environment Operations (0xA0-0xAF)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0xA0 | ADDRESS | - | [] -> [addr] | Get contract address |
| 0xA1 | BALANCE | - | [addr] -> [bal] | Get account balance |
| 0xA2 | ORIGIN | - | [] -> [addr] | Get transaction origin |
| 0xA3 | CALLER | - | [] -> [addr] | Get direct caller |
| 0xA4 | CALLVALUE | - | [] -> [val] | Get call value |
| 0xA5 | CALLDATALOAD | - | [i] -> [data] | Get input data |
| 0xA6 | CALLDATASIZE | - | [] -> [size] | Get input data size |
| 0xA7 | CALLDATACOPY | - | [dst,src,len] -> [] | Copy input data |
| 0xA8 | CODESIZE | - | [] -> [size] | Get code size |
| 0xA9 | CODECOPY | - | [dst,src,len] -> [] | Copy code |
| 0xAA | GASPRICE | - | [] -> [price] | Get gas price |
| 0xAB | EXTCODESIZE | - | [addr] -> [size] | Get external code size |
| 0xAC | EXTCODECOPY | - | [addr,dst,src,len] -> [] | Copy external code |
| 0xAD | EXTCODEHASH | - | [addr] -> [hash] | Get code hash |

### 3.7 Block Information (0xB0-0xBF)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0xB0 | BLOCKHASH | - | [num] -> [hash] | Get block hash |
| 0xB1 | COINBASE | - | [] -> [addr] | Get block producer |
| 0xB2 | TIMESTAMP | - | [] -> [ts] | Get block timestamp |
| 0xB3 | NUMBER | - | [] -> [num] | Get block number |
| 0xB4 | PREVRANDAO | - | [] -> [rand] | Get random value |
| 0xB5 | GASLIMIT | - | [] -> [limit] | Get gas limit |
| 0xB6 | CHAINID | - | [] -> [id] | Get chain ID |
| 0xB7 | SELFBALANCE | - | [] -> [bal] | Get own balance |
| 0xB8 | BASEFEE | - | [] -> [fee] | Get base fee |

### 3.8 Cryptographic Operations (0xC0-0xDF)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0xC0 | SHA3 | - | [offset,len] -> [hash] | SHA3-256 hash |
| 0xC1 | SHA3_512 | - | [offset,len] -> [hash] | SHA3-512 hash |
| 0xC2 | SHAKE128 | - | [offset,len,outlen] -> [hash] | SHAKE128 XOF |
| 0xC3 | SHAKE256 | - | [offset,len,outlen] -> [hash] | SHAKE256 XOF |
| 0xC4 | MLKEM_KEYGEN | - | [] -> [pk,sk] | ML-KEM-1024 key generation |
| 0xC5 | MLKEM_ENCAP | - | [pk] -> [ct,ss] | ML-KEM encapsulation |
| 0xC6 | MLKEM_DECAP | - | [sk,ct] -> [ss] | ML-KEM decapsulation |
| 0xC7 | MLDSA_KEYGEN | - | [] -> [pk,sk] | ML-DSA-87 key generation |
| 0xC8 | MLDSA_SIGN | - | [sk,msg] -> [sig] | ML-DSA signing |
| 0xC9 | MLDSA_VERIFY | - | [pk,msg,sig] -> [valid] | ML-DSA verification |
| 0xCA | AJTAI_COMMIT | - | [data] -> [comm] | Ajtai commitment |
| 0xCB | AJTAI_OPEN | - | [comm,data,open] -> [valid] | Ajtai opening |
| 0xCC | ZKVERIFY | - | [proof,inputs] -> [valid] | ZK-STARK verification |
| 0xCD | CHACHA_ENC | - | [key,nonce,data] -> [ct] | ChaCha20-Poly1305 encrypt |
| 0xCE | CHACHA_DEC | - | [key,nonce,ct] -> [data] | ChaCha20-Poly1305 decrypt |

### 3.9 Privacy Operations (0xE0-0xEF)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0xE0 | SHIELD_WRAP | - | [data] -> [enc] | Encrypt with SHIELD |
| 0xE1 | SHIELD_UNWRAP | - | [enc] -> [data] | Decrypt with SHIELD |
| 0xE2 | RING_SIGN | - | [ring,sk,msg] -> [sig] | Ring signature |
| 0xE3 | RING_VERIFY | - | [ring,msg,sig] -> [valid] | Ring signature verify |
| 0xE4 | WHISPER_SEND | - | [to,msg] -> [] | Confidential message |
| 0xE5 | WHISPER_RECV | - | [ch] -> [msg] | Receive confidential |
| 0xE6 | EYE_REVEAL | - | [data,selector] -> [view] | Selective disclosure |
| 0xE7 | GATE_ENTER | - | [proof] -> [token] | Enter private execution |
| 0xE8 | GATE_EXIT | - | [token,result] -> [] | Exit private execution |

### 3.10 Logging Operations (0xF0-0xFF)

| Opcode | Mnemonic | Args | Stack | Description |
|--------|----------|------|-------|-------------|
| 0xF0 | LOG0 | - | [offset,len] -> [] | Log with 0 topics |
| 0xF1 | LOG1 | - | [offset,len,t1] -> [] | Log with 1 topic |
| 0xF2 | LOG2 | - | [offset,len,t1,t2] -> [] | Log with 2 topics |
| 0xF3 | LOG3 | - | [offset,len,t1,t2,t3] -> [] | Log with 3 topics |
| 0xF4 | LOG4 | - | [offset,len,t1-t4] -> [] | Log with 4 topics |
| 0xFE | SELFDESTRUCT | - | [addr] -> [] | Destroy contract |
| 0xFF | CREATE2 | - | [val,off,len,salt] -> [addr] | Create with salt |

## 4. Stack Limits

| Limit | Value |
|-------|-------|
| Max stack depth | 1024 |
| Max item size | 32 bytes (256 bits) |
| Max memory | 4 MB |
| Max code size | 24 KB |
| Max call depth | 1024 |

## 5. Memory Model

### 5.1 Memory Layout

```
+------------------+ 0x0000
|    Call Data     |
+------------------+
|   Return Data    |
+------------------+
|      Stack       |
+------------------+
|      Heap        |
+------------------+ 0x400000 (4MB limit)
```

### 5.2 Memory Expansion Cost

Memory expansion gas cost:
```
cost = 3 * words + (words^2 / 512)
```

## 6. Contract Storage

### 6.1 Storage Model

- Key-value store with 256-bit keys and values
- SLOAD: Read from storage
- SSTORE: Write to storage
- Cold/warm access tracking for gas calculation

### 6.2 Encrypted Storage (SHIELD)

- SLOAD_ENC: Load and decrypt using contract's SHIELD key
- SSTORE_ENC: Encrypt and store using contract's SHIELD key
- Keys derived per-contract using HKDF from master key

## 7. Execution Context

### 7.1 Transaction Context

| Field | Size | Description |
|-------|------|-------------|
| origin | 32 | Original sender |
| gasprice | 32 | Gas price |
| gasLimit | 32 | Total gas limit |
| value | 32 | Transferred value |
| data | var | Input data |

### 7.2 Block Context

| Field | Size | Description |
|-------|------|-------------|
| coinbase | 32 | Block producer |
| timestamp | 8 | Block time |
| number | 8 | Block height |
| gaslimit | 8 | Block gas limit |
| basefee | 32 | Base fee |
| prevrandao | 32 | Randomness beacon |

## 8. Exception Handling

### 8.1 Exception Types

| Type | Description |
|------|-------------|
| OUT_OF_GAS | Gas exhausted |
| STACK_UNDERFLOW | Not enough stack items |
| STACK_OVERFLOW | Stack full |
| INVALID_JUMP | Jump to non-JUMPDEST |
| INVALID_OPCODE | Unknown opcode |
| WRITE_PROTECTION | Write in static context |
| OUT_OF_BOUNDS | Memory/code access OOB |

### 8.2 Exception Behavior

- All state changes reverted on exception
- Gas consumed up to point of exception
- Return data empty (or revert data if REVERT)

## 9. Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-12-07 | Initial specification |
