# AegisVM Security Model

**Version:** 1.0.0
**Status:** Draft
**Date:** 2025-12-07

## 1. Security Architecture Overview

AegisVM is designed with defense-in-depth, combining multiple security layers:

```
+------------------------------------------------------------------+
|                    Application Layer                              |
|  (Smart Contracts, DApps)                                        |
+------------------------------------------------------------------+
|                    Privacy Layer (ANUBIS)                        |
|  SHIELD | VEIL | WHISPER | EYE | GATE                           |
+------------------------------------------------------------------+
|                    Consensus Layer (SCARAB)                      |
|  AADKG | HORUS | THOTH | SEKHMET | MAAT | KHNUM | TEFNUT | SEBEK|
+------------------------------------------------------------------+
|                    Execution Layer (VM)                          |
|  Stack VM | Memory Model | Storage | Cryptographic Primitives    |
+------------------------------------------------------------------+
|                    Formal Verification Layer                     |
|  SPARK/Ada Core | Proven Memory Safety | Contract Verification  |
+------------------------------------------------------------------+
|                    Cryptographic Foundation                      |
|  ML-KEM-1024 | ML-DSA-87 | SHA3/SHAKE | ChaCha20-Poly1305       |
+------------------------------------------------------------------+
```

## 2. Threat Model

### 2.1 Adversary Capabilities

| Threat Level | Capabilities |
|--------------|--------------|
| **L1: Script Kiddie** | Known exploits, automated tools |
| **L2: Professional** | Custom exploits, 0-days, $100K budget |
| **L3: Nation State** | Quantum computers, unlimited resources |
| **L4: Future Adversary** | CRQC (Cryptographically Relevant Quantum Computer) |

### 2.2 Attack Surface

| Surface | Threats | Mitigations |
|---------|---------|-------------|
| Network | DDoS, Eclipse, MITM | Rate limiting, peer diversity, TLS 1.3 |
| Consensus | 51%, Long-range | BFT with t-of-n signatures, finality |
| Smart Contracts | Reentrancy, overflow | Formal verification, gas limits |
| Cryptography | Quantum attacks | Post-quantum algorithms |
| Privacy | Metadata leaks | SHIELD encryption, ring signatures |
| Side Channels | Timing, power | Constant-time code, SPARK proofs |

### 2.3 Security Assumptions

1. **Cryptographic hardness**: ML-KEM/ML-DSA are secure against quantum adversaries
2. **BFT threshold**: Fewer than n/3 validators are malicious
3. **Honest majority**: At least 50%+1 stake is honest for liveness
4. **Network synchrony**: Bounded message delivery time (for finality)
5. **Formal verification**: SPARK proofs are sound

## 3. Post-Quantum Cryptography

### 3.1 Algorithm Selection

| Purpose | Algorithm | Standard | Security Level |
|---------|-----------|----------|----------------|
| Key Exchange | ML-KEM-1024 | FIPS 203 | Category 5 (256-bit quantum) |
| Signatures | ML-DSA-87 | FIPS 204 | Category 5 (256-bit quantum) |
| Hashing | SHA3-256/512 | FIPS 202 | 256/512-bit classical |
| XOF | SHAKE128/256 | FIPS 202 | 128/256-bit security |
| Symmetric | ChaCha20-Poly1305 | RFC 8439 | 256-bit classical |

### 3.2 Key Sizes and Performance

| Algorithm | Public Key | Secret Key | Signature/CT | Ops/sec |
|-----------|------------|------------|--------------|---------|
| ML-KEM-1024 | 1,568 B | 3,168 B | 1,568 B | 10,000 |
| ML-DSA-87 | 2,592 B | 4,896 B | 4,627 B | 2,000 |
| SHA3-256 | - | - | 32 B | 500 MB/s |
| ChaCha20 | 32 B | 32 B | +16 B tag | 2 GB/s |

### 3.3 Quantum Security Analysis

**Lattice Security (ML-KEM/ML-DSA)**:
- Based on Module-LWE and Module-SIS problems
- Best known quantum attack: Grover + BKZ lattice reduction
- Category 5 security: 256 quantum bits equivalent
- No known polynomial-time quantum algorithm

**Hash Security (SHA3)**:
- Grover's algorithm reduces security by factor of 2
- SHA3-256: 128-bit post-quantum security
- SHA3-512: 256-bit post-quantum security

### 3.4 Crypto Agility

The system supports algorithm migration:
1. Version field in all signatures/keys
2. Hybrid mode: classical + PQ during transition
3. Key rotation protocol for epoch changes
4. Backward compatibility with verification

## 4. Formal Verification

### 4.1 SPARK/Ada Verification Levels

| Level | What's Proven | Coverage |
|-------|---------------|----------|
| Stone | No runtime errors (bounds, overflow) | 100% |
| Bronze | + Data initialization | 100% |
| Silver | + Absence of certain errors | 95% |
| **Gold** | + Key functional properties | 90% |
| Platinum | + Full functional correctness | Target |

### 4.2 Proven Properties

**Memory Safety**:
- No buffer overflows
- No use-after-free
- No null dereferences
- No uninitialized reads
- No aliasing violations

**Arithmetic Safety**:
- No integer overflow
- No division by zero
- Modular arithmetic correct

**Functional Properties**:
- Cryptographic operations produce correct output
- State transitions are deterministic
- Storage operations are atomic

### 4.3 Verification Toolchain

```
Source Code (Ada/SPARK)
        |
        v
+------------------+
|    gnatprove     |  SPARK analyzer
+------------------+
        |
        v
+------------------+
|    Why3/Alt-Ergo |  SMT solvers
|    CVC5/Z3       |
+------------------+
        |
        v
Proof Obligations (VCs)
        |
        v
+------------------+
|  Manual Proofs   |  For complex lemmas
|  (Coq/Isabelle)  |
+------------------+
```

### 4.4 Waiver System

For unproven code sections:
1. Documented justification required
2. Alternative mitigations (tests, audits)
3. On-chain recording for auditability
4. Time-limited approval (max 1 year)
5. Quarterly review process

## 5. Side-Channel Resistance

### 5.1 Constant-Time Requirements

| Operation | Requirement | Verification |
|-----------|-------------|--------------|
| Key comparison | CT | Timing tests + review |
| Password check | CT | Timing tests + review |
| Secret indexing | CT | SPARK contracts |
| Modular operations | CT | Assembly review |

### 5.2 Implementation Guidelines

```ada
--  WRONG: Early exit leaks timing
function Compare_Bad (A, B : Byte_Array) return Boolean is
begin
   for I in A'Range loop
      if A(I) /= B(I) then
         return False;  -- Timing leak!
      end if;
   end loop;
   return True;
end Compare_Bad;

--  CORRECT: Constant-time comparison
function Compare_CT (A, B : Byte_Array) return Boolean is
   Diff : Unsigned_8 := 0;
begin
   for I in A'Range loop
      Diff := Diff or (A(I) xor B(I));
   end loop;
   return Diff = 0;
end Compare_CT;
```

### 5.3 Timing Test Methodology

1. **Statistical analysis**: 100,000+ iterations
2. **T-test**: Compare distributions for different inputs
3. **Threshold**: p-value > 0.05 for CT claim
4. **CI integration**: Automated timing tests

## 6. Privacy Model (ANUBIS)

### 6.1 Privacy Guarantees

| Layer | Guarantee | Mechanism |
|-------|-----------|-----------|
| SHIELD | State confidentiality | ChaCha20-Poly1305 |
| VEIL | Computation privacy | ZK-STARK proofs |
| WHISPER | Message unlinkability | Ring signatures |
| EYE | Selective disclosure | Attribute proofs |
| GATE | Execution privacy | TEE + ZK |

### 6.2 Threat Scenarios

**Scenario 1: Curious Validator**
- Cannot read encrypted state (SHIELD)
- Cannot link transactions (WHISPER)
- Can see: block timing, gas usage, public state

**Scenario 2: Network Observer**
- Cannot decrypt traffic (TLS 1.3)
- Cannot trace transactions (ring sigs)
- Can see: IP addresses, message sizes

**Scenario 3: Compromised Node**
- Cannot forge signatures (ML-DSA)
- Cannot decrypt other nodes' state
- Limited to own node's data

### 6.3 Metadata Leakage

| Leaked | Not Leaked | Mitigation |
|--------|------------|------------|
| Block timing | Sender identity | Ring signatures |
| Gas usage | Transaction amounts | Confidential TX |
| Contract address | Internal state | SHIELD encryption |
| Message size | Message content | Padding + batching |

## 7. Consensus Security (SCARAB)

### 7.1 BFT Security Properties

| Property | Guarantee | Threshold |
|----------|-----------|-----------|
| Safety | No conflicting finalization | f < n/3 |
| Liveness | Progress under async | f < n/3 |
| Accountability | Detect misbehavior | Any f |

### 7.2 Distributed Key Generation (AADKG)

- Asynchronous: Works under network delays
- Verifiable: All parties can verify shares
- Robust: Tolerates f < n/3 malicious
- Proactive: Key resharing for long-term security

### 7.3 Threshold Signatures (SEBEK)

- t-of-n ML-DSA-87 signatures
- Constant verification time
- No single point of failure
- Audit trail for all operations

### 7.4 Attack Resistance

| Attack | Defense |
|--------|---------|
| 51% attack | BFT requires only n/3+1 honest |
| Long-range attack | Finality + checkpointing |
| Eclipse attack | Peer diversity + reputation |
| Nothing-at-stake | Slashing conditions |
| Selfish mining | Not applicable (BFT) |

## 8. Smart Contract Security

### 8.1 Common Vulnerabilities Mitigated

| Vulnerability | Mitigation |
|--------------|------------|
| Reentrancy | Checks-effects-interactions pattern |
| Integer overflow | Checked arithmetic (SPARK) |
| Access control | Formal verification of permissions |
| Front-running | Confidential transactions |
| Flash loan attacks | Minimum confirmation requirements |

### 8.2 Gas Limits and DoS

- Per-transaction gas limit: 30M
- Per-block gas limit: 100M
- Minimum gas price enforced
- Expensive ops have proportional gas

### 8.3 Formal Verification for Contracts

```
Contract Source
      |
      v
+-------------+
| SPARK/Ada   |  Or verified compiler
| Compiler    |
+-------------+
      |
      v
+-------------+
| gnatprove   |  Property verification
+-------------+
      |
      v
Bytecode (proven safe)
```

## 9. Network Security

### 9.1 P2P Layer

- TLS 1.3 for all connections
- Peer authentication via ML-DSA
- Rate limiting per peer
- Reputation scoring

### 9.2 DDoS Mitigation

| Layer | Protection |
|-------|------------|
| Network | Rate limiting, connection caps |
| Protocol | Message size limits, PoW puzzles |
| Application | Gas limits, spam filtering |

### 9.3 Eclipse Attack Prevention

- Minimum peer diversity: 8 distinct ASNs
- DNS seed diversity
- Outbound connection rotation
- Peer exchange protocol

## 10. Operational Security

### 10.1 Key Management

| Key Type | Storage | Rotation |
|----------|---------|----------|
| Validator keys | HSM | Annual |
| Node keys | Encrypted disk | Epoch-based |
| User keys | Hardware wallet | User-controlled |

### 10.2 Incident Response

1. **Detection**: Monitoring + alerting
2. **Containment**: Isolate affected nodes
3. **Eradication**: Patch and redeploy
4. **Recovery**: State recovery from backups
5. **Lessons**: Post-mortem and improvements

### 10.3 Security Monitoring

- Real-time anomaly detection
- Consensus health metrics
- Cryptographic operation timing
- Memory and resource usage

## 11. Audit and Compliance

### 11.1 Security Audits

| Type | Frequency | Scope |
|------|-----------|-------|
| Code audit | Per release | All changes |
| Formal verification | Continuous | SPARK code |
| Penetration test | Annual | Full system |
| Economic audit | Semi-annual | Incentive mechanisms |

### 11.2 Bug Bounty Program

| Severity | Reward | Examples |
|----------|--------|----------|
| Critical | $100K+ | Consensus break, fund theft |
| High | $25K | Privacy leak, DoS |
| Medium | $5K | Minor vulnerabilities |
| Low | $1K | Best practices violations |

### 11.3 Compliance

- SOC 2 Type II (in progress)
- ISO 27001 (planned)
- NIST Cybersecurity Framework

## 12. Security Assumptions and Limitations

### 12.1 Trust Assumptions

1. SPARK/gnatprove toolchain is correct
2. Underlying hardware is not compromised
3. Cryptographic assumptions hold
4. At least n/3+1 validators are honest

### 12.2 Known Limitations

1. Side-channel resistance not formally proven (mitigated by testing)
2. FFI code not SPARK-verified (mitigated by isolation)
3. Quantum computers don't exist yet (using PQ-safe algorithms)

### 12.3 Future Work

- Full Platinum-level SPARK proofs
- Hardware security module integration
- Post-quantum zero-knowledge proofs
- Decentralized key ceremony

## 13. References

1. NIST FIPS 203 (ML-KEM)
2. NIST FIPS 204 (ML-DSA)
3. NIST FIPS 202 (SHA-3)
4. RFC 8439 (ChaCha20-Poly1305)
5. SPARK 2014 Reference Manual
6. Boneh & Shoup, "A Graduate Course in Applied Cryptography"

## 14. Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-12-07 | Initial specification |
