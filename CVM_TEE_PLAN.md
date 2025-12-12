# AnubisVM: Post-Quantum Cryptographically Verified Module TEE

## Core Principle

**Every identity, every signature, every encryption is post-quantum resistant by default.**

The system is built on mathematical certainty: SPARK proofs guarantee that code does exactly what it's programmed to do - no buffer overflows, no undefined behavior, no surprises.

---

## Post-Quantum Foundation (AAS-001 v3.1)

### Single Algorithm Policy

| Operation | Algorithm | Standard | Security Level |
|-----------|-----------|----------|----------------|
| **Digital Signatures** | ML-DSA-87 | NIST FIPS 204 | Level 5 (256-bit) |
| **Key Encapsulation** | ML-KEM-1024 | NIST FIPS 203 | Level 5 (256-bit) |
| **Hashing** | SHA3-256/512, SHAKE128/256 | NIST FIPS 202 | 256-bit |
| **MAC** | KMAC256 | NIST SP 800-185 | 256-bit |

**No classical crypto fallbacks. No ECDSA. No RSA. No secp256k1.**

### AAS-001 v3.1 Address Standard

All identities in the system use the AAS-001 address format:

```
mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3-v8k4n
|-----| |--| |-| |--------------------------------------------------| |---|
 algo   net  t                 payload (52 chars)                   checksum
```

**Components:**
- **Algorithm**: `mldsa87` (fixed - identifies ML-DSA-87)
- **Network**: `main`, `test`, `dev`, `lab`, `staging`
- **Entity Type**:
  - `u` = User (externally owned account)
  - `c` = Contract (CVM module)
  - `v` = Validator (not used in TEE mode)
  - `s` = System (protocol modules)
- **Payload**: 52 Base32 chars = SHA3-256(domain_separator || ML-DSA-87-PublicKey)
- **Checksum**: 5 Base32 chars = truncated SHA3-256 of payload

**Domain Separation:**
```
Account_ID = SHA3-256("aegis-v1-mldsa87-" || entity_type || public_key)
```

This prevents cross-type attacks (a user key cannot become a contract key).

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        AnubisVM TEE                                     │
│                   (Single SPARK-Proven Binary)                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                    CVM Registry                                 │   │
│   │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐             │   │
│   │  │ CVM: Token  │  │ CVM: Vault  │  │ CVM: Auth   │  ...        │   │
│   │  │ addr: c:... │  │ addr: c:... │  │ addr: c:... │             │   │
│   │  └─────────────┘  └─────────────┘  └─────────────┘             │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                    Identity Layer (AAS-001)                     │   │
│   │  - All addresses: mldsa87:network:type:payload-checksum         │   │
│   │  - All signatures: ML-DSA-87 (FIPS 204)                         │   │
│   │  - All encryption: ML-KEM-1024 (FIPS 203)                       │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                    Privacy Layer (ANUBIS)                       │   │
│   │  ┌─────────┐ ┌───────┐ ┌────────┐ ┌─────────┐ ┌──────┐         │   │
│   │  │ Shield  │ │  Eye  │ │  Gate  │ │ Whisper │ │ Veil │         │   │
│   │  │ (state) │ │(view) │ │(exec)  │ │ (conf)  │ │ (zk) │         │   │
│   │  └─────────┘ └───────┘ └────────┘ └─────────┘ └──────┘         │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                    Cryptographic Core                           │   │
│   │  ┌──────────────┐ ┌───────────────┐ ┌────────────────┐         │   │
│   │  │  SHA3/SHAKE  │ │   ML-DSA-87   │ │  ML-KEM-1024   │         │   │
│   │  │  Keccak-256  │ │   (sign)      │ │  (encrypt)     │         │   │
│   │  │  KMAC256     │ │   FIPS 204    │ │  FIPS 203      │         │   │
│   │  └──────────────┘ └───────────────┘ └────────────────┘         │   │
│   │                                                                 │   │
│   │  ┌──────────────────────────────────────────────────┐          │   │
│   │  │  AEAD: SHAKE256-CTR + KMAC256 (authenticated)   │          │   │
│   │  └──────────────────────────────────────────────────┘          │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                    State Layer                                  │   │
│   │  ┌─────────────────────────┐  ┌─────────────────────────┐      │   │
│   │  │      Khepri MPT         │  │   Encrypted Storage     │      │   │
│   │  │   (Merkle Patricia)     │  │   (AEAD + ML-KEM)       │      │   │
│   │  └─────────────────────────┘  └─────────────────────────┘      │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Project Structure

```
anubisvm/
├── anubisvm.gpr                    -- Master GPRbuild project
├── alire.toml                      -- Alire package manifest
├── Makefile                        -- Build/prove/test targets
│
├── src/
│   ├── core/                       -- Foundation (100% SPARK)
│   │   ├── anubis_types.ads/adb        -- Byte, Word64, U256
│   │   └── anubis_config.ads           -- TEE configuration
│   │
│   ├── crypto/                     -- Post-Quantum Primitives (100% SPARK)
│   │   ├── keccak/
│   │   │   ├── anubis_keccak.ads/adb   -- Keccak-f[1600]
│   │   ├── sha3/
│   │   │   ├── anubis_sha3.ads/adb     -- SHA3-256/384/512, SHAKE
│   │   ├── kmac/
│   │   │   ├── anubis_kmac.ads/adb     -- KMAC128/256 (NEW)
│   │   ├── aead/
│   │   │   ├── anubis_aead.ads/adb     -- Authenticated encryption (NEW)
│   │   ├── kdf/
│   │   │   ├── anubis_kdf.ads/adb      -- Key derivation (NEW)
│   │   ├── mldsa/
│   │   │   └── [existing ML-DSA-87 implementation]
│   │   └── mlkem/
│   │       └── [existing ML-KEM-1024 implementation]
│   │
│   ├── identity/                   -- AAS-001 Identity (100% SPARK)
│   │   ├── anubis_address_types.ads    -- Address types [EXISTS]
│   │   ├── anubis_address_derive.ads/adb -- Derivation [EXISTS]
│   │   ├── anubis_address.ads/adb      -- High-level API [EXISTS]
│   │   ├── anubis_address_base32.ads/adb -- Encoding [EXISTS]
│   │   └── anubis_address_checksum.ads/adb -- Checksums [EXISTS]
│   │
│   ├── cvm/                        -- CVM Runtime (100% SPARK)
│   │   ├── anubis_cvm_types.ads        -- Module types
│   │   ├── anubis_cvm_interface.ads    -- Generic module interface
│   │   ├── anubis_cvm_registry.ads/adb -- Module registry
│   │   ├── anubis_cvm_context.ads/adb  -- Execution context
│   │   └── anubis_cvm_dispatch.ads/adb -- Entry point dispatch
│   │
│   ├── privacy/                    -- ANUBIS Privacy (100% SPARK)
│   │   ├── anubis_shield.ads/adb       -- Encrypted state
│   │   ├── anubis_eye.ads/adb          -- Viewing keys
│   │   ├── anubis_gate.ads/adb         -- Private sessions
│   │   ├── anubis_whisper.ads/adb      -- Confidential values
│   │   └── anubis_veil.ads/adb         -- Zero-knowledge proofs
│   │
│   ├── state/                      -- State Management (100% SPARK)
│   │   ├── anubis_khepri.ads/adb       -- Merkle Patricia Trie
│   │   └── anubis_store.ads/adb        -- Encrypted persistence
│   │
│   ├── tee/                        -- TEE Core (100% SPARK)
│   │   ├── anubis_tee_keys.ads/adb     -- Key management
│   │   ├── anubis_tee_attest.ads/adb   -- Attestation
│   │   └── anubis_tee_runtime.ads/adb  -- Main runtime
│   │
│   └── io/                         -- I/O Boundary (minimal)
│       ├── anubis_io.ads/adb           -- File/stdio (SPARK_Mode Off)
│       └── anubis_json.ads/adb         -- JSON parsing
│
├── modules/                        -- Cryptographically Verified Modules
│   ├── cvm_token/                  -- Example: Fungible token
│   ├── cvm_vault/                  -- Example: Encrypted vault
│   └── cvm_identity/               -- Example: Identity attestation
│
├── main/
│   └── anubis_main.adb             -- Entry point
│
└── tests/
    ├── test_sha3_kat.adb           -- SHA3 Known Answer Tests [EXISTS]
    ├── test_mldsa.adb              -- ML-DSA-87 tests [EXISTS]
    ├── test_mlkem_kat.adb          -- ML-KEM-1024 tests [EXISTS]
    ├── test_address.adb            -- AAS-001 tests [EXISTS]
    ├── test_kmac.adb               -- KMAC tests (NEW)
    ├── test_aead.adb               -- AEAD tests (NEW)
    └── test_cvm.adb                -- CVM integration tests (NEW)
```

---

## What Gets Deleted

Remove all non-SPARK code:

```bash
# Remove Go code (Cosmos SDK, CometBFT)
rm -rf cosmos/
rm -rf cosmos-sdk-pq/
rm -rf cometbft-pq/
rm -rf tee/                    # Go TEE code (rewriting in SPARK)

# Remove blockchain-specific modules
rm -rf core/src/tokenomics/    # No tokens in TEE mode
rm -rf core/src/scarab/        # Distributed consensus features

# Remove unused
rm -rf toolchain/              # CLI tools will be rebuilt
```

**Keep:**
- `core/src/hash/` - SHA3, Keccak (proven)
- `core/src/pqc/` - ML-DSA-87, ML-KEM-1024 (proven)
- `core/src/aegis/` - AAS-001 address system (proven)
- `core/src/anubis/` - Privacy layer specs (will complete)
- `tests/` - KAT tests (expand)

---

## Implementation Phases

### Phase 1: Foundation Cleanup

**Goal**: Pure SPARK codebase with working crypto primitives.

**Tasks**:
1. Delete all Go code directories
2. Restructure into new layout
3. Create unified `anubisvm.gpr`
4. Verify all existing tests pass:
   - `test_sha3_kat` - SHA3 Known Answer Tests
   - `test_mldsa` - ML-DSA-87 sign/verify
   - `test_mlkem_kat` - ML-KEM-1024 encaps/decaps
   - `test_address` - AAS-001 address derivation

**Verification**:
```bash
alr build
./bin/test_sha3_kat
./bin/test_mldsa
./bin/test_mlkem_kat
./bin/test_address
```

### Phase 2: Complete Cryptographic Stack

**Goal**: All crypto building blocks for the TEE.

#### 2.1 KMAC256 (Keccak Message Authentication Code)

Per NIST SP 800-185:

```ada
package Anubis_KMAC with
   SPARK_Mode => On
is
   subtype KMAC_Key is Byte_Array (0 .. 31);  -- 256-bit key
   subtype KMAC256_Tag is Byte_Array (0 .. 31);  -- 256-bit tag

   --  KMAC256(K, X, L, S) = cSHAKE256(bytepad(encode_string(K), 136) || X || right_encode(L), L, "KMAC", S)
   procedure KMAC256 (
      Key        : KMAC_Key;
      Message    : Byte_Array;
      Custom     : String;
      Tag        : out KMAC256_Tag
   ) with
      Global => null,
      Pre => Message'Length <= 2**32 - 1;

   --  Constant-time verification
   function Verify_KMAC256 (
      Key      : KMAC_Key;
      Message  : Byte_Array;
      Custom   : String;
      Expected : KMAC256_Tag
   ) return Boolean with
      Global => null;

end Anubis_KMAC;
```

**Implementation**: Built on existing cSHAKE (customizable SHAKE) from SHA3.

#### 2.2 Authenticated Encryption (AEAD)

SHAKE256-based authenticated encryption:

```ada
package Anubis_AEAD with
   SPARK_Mode => On
is
   subtype AEAD_Key is Byte_Array (0 .. 31);    -- 256-bit
   subtype AEAD_Nonce is Byte_Array (0 .. 15);  -- 128-bit
   subtype AEAD_Tag is Byte_Array (0 .. 31);    -- 256-bit

   procedure Encrypt (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      Plaintext  : Byte_Array;
      Assoc_Data : Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out AEAD_Tag
   ) with
      Global => null,
      Pre => Ciphertext'Length = Plaintext'Length;

   procedure Decrypt (
      Key        : AEAD_Key;
      Nonce      : AEAD_Nonce;
      Ciphertext : Byte_Array;
      Assoc_Data : Byte_Array;
      Tag        : AEAD_Tag;
      Plaintext  : out Byte_Array;
      Valid      : out Boolean
   ) with
      Global => null,
      Post => (if not Valid then (for all I in Plaintext'Range => Plaintext (I) = 0));
      --  Plaintext zeroed on auth failure (proven!)

end Anubis_AEAD;
```

**Construction**:
1. `K_enc = SHAKE256(Key || "enc" || Nonce, 32)`
2. `K_mac = SHAKE256(Key || "mac" || Nonce, 32)`
3. `C = P XOR SHAKE256(K_enc, |P|)` (stream cipher)
4. `Tag = KMAC256(K_mac, AD || C, "AnubisAEAD")`

#### 2.3 Key Derivation Function

```ada
package Anubis_KDF with
   SPARK_Mode => On
is
   --  HKDF-like using SHAKE256

   procedure Derive_Key (
      Input_Key  : Byte_Array;
      Salt       : Byte_Array;
      Info       : String;
      Output_Key : out Byte_Array
   ) with
      Global => null;

   --  Password-based KDF (memory-hard)
   procedure Password_KDF (
      Password   : String;
      Salt       : Byte_Array;
      Iterations : Positive;
      Memory_KB  : Positive;
      Output     : out Byte_Array
   ) with
      Global => null,
      Pre => Iterations >= 1000 and Memory_KB >= 64;

end Anubis_KDF;
```

### Phase 3: CVM Runtime

**Goal**: Framework for executing Cryptographically Verified Modules.

#### 3.1 CVM Identity

Every CVM gets an AAS-001 address:

```ada
--  CVM address = SHA3-256("aegis-v1-mldsa87-c" || code_hash)
--  Format: mldsa87:main:c:payload-checksum

procedure Register_CVM (
   Code_Hash : Hash256;
   Name      : String;
   Address   : out Address  --  type = Contract
) with
   Global => null,
   Post => Address.Entity = Contract and Address.Valid;
```

#### 3.2 CVM Interface

```ada
generic
   CVM_Name : String;
package Anubis_CVM_Interface with
   SPARK_Mode => On
is
   --  Every CVM must implement these

   procedure Initialize (
      Ctx     : CVM_Context;
      State   : in out CVM_State;
      Success : out Boolean
   ) with
      Global => null;

   procedure Execute (
      Ctx         : CVM_Context;
      Entry_Point : String;
      Args        : Byte_Array;
      State       : in Out CVM_State;
      Result      : out Byte_Array;
      Result_Len  : out Natural;
      Status      : out Exec_Status
   ) with
      Global => null,
      Pre => Args'Length <= Max_Args_Size,
      Post => Result_Len <= Max_Result_Size;

   procedure Query (
      Ctx         : CVM_Context;
      Entry_Point : String;
      Args        : Byte_Array;
      State       : CVM_State;  -- Read-only
      Result      : out Byte_Array;
      Result_Len  : out Natural;
      Status      : out Exec_Status
   ) with
      Global => null;

end Anubis_CVM_Interface;
```

#### 3.3 Execution Context

```ada
type CVM_Context is record
   --  Caller identity (AAS-001 address)
   Caller      : Address;      -- mldsa87:main:u:...
   Caller_PK   : ML_DSA_Public_Key;

   --  Target module (AAS-001 address)
   Module      : Address;      -- mldsa87:main:c:...

   --  Execution limits (not gas - resource budgets)
   Budget      : Execution_Budget;

   --  Timing
   Timestamp   : Word64;
   Counter     : Word64;       -- Monotonic counter
end record;

type Execution_Budget is record
   Max_Iterations    : Positive := 10_000_000;
   Max_Memory_KB     : Positive := 16_384;      -- 16 MB
   Max_Storage_Ops   : Positive := 1_000;
   Max_Crypto_Ops    : Positive := 100;
end record;
```

### Phase 4: Privacy Layer (ANUBIS)

All privacy operations use post-quantum crypto.

#### 4.1 Shield (Encrypted State)

```ada
package Anubis_Shield with
   SPARK_Mode => On
is
   --  Hybrid encryption: ML-KEM-1024 + AEAD

   type Encrypted_State is record
      KEM_Ciphertext : ML_KEM_Ciphertext;  -- 1568 bytes
      AEAD_Nonce     : AEAD_Nonce;         -- 16 bytes
      Ciphertext     : Byte_Array (0 .. Max_State_Size - 1);
      Ciphertext_Len : Natural;
      Tag            : AEAD_Tag;           -- 32 bytes
   end record;

   procedure Encrypt_For_Address (
      Plaintext       : Byte_Array;
      Recipient_Addr  : Address;      -- mldsa87:...:u:...
      Recipient_PK    : ML_KEM_Encaps_Key;
      Randomness      : Byte_Array;   -- 64 bytes
      Encrypted       : out Encrypted_State;
      Success         : out Boolean
   ) with
      Global => null,
      Pre => Recipient_Addr.Valid and Recipient_Addr.Entity = User;

   procedure Decrypt_With_Key (
      Encrypted   : Encrypted_State;
      Secret_Key  : ML_KEM_Decaps_Key;
      Plaintext   : out Byte_Array;
      Len         : out Natural;
      Success     : out Boolean
   ) with
      Global => null;

end Anubis_Shield;
```

#### 4.2 Eye (Selective Disclosure with AAS-001)

```ada
package Anubis_Eye with
   SPARK_Mode => On
is
   type Viewing_Key is record
      Owner        : Address;        -- AAS-001 address of owner
      Viewer       : Address;        -- AAS-001 address of authorized viewer
      Permission   : View_Permission;
      Valid_Until  : Word64;
      Key_Material : Byte_Array (0 .. 31);
      Signature    : ML_DSA_Signature;  -- Signed by owner
   end record;

   procedure Create_Viewing_Key (
      Owner_Addr    : Address;
      Owner_SK      : ML_DSA_Secret_Key;
      Viewer_Addr   : Address;
      Permission    : View_Permission;
      Valid_Until   : Word64;
      VK            : out Viewing_Key;
      Success       : out Boolean
   ) with
      Global => null,
      Pre => Owner_Addr.Valid and Owner_Addr.Entity = User and
             Viewer_Addr.Valid;

   function Verify_Viewing_Key (VK : Viewing_Key) return Boolean with
      Global => null;
      --  Verifies ML-DSA-87 signature

end Anubis_Eye;
```

#### 4.3 Gate (Private Execution Sessions)

```ada
package Anubis_Gate with
   SPARK_Mode => On
is
   type Session is record
      ID             : Session_ID;
      Initiator      : Address;       -- User address (mldsa87:...:u:...)
      Target_Module  : Address;       -- Contract address (mldsa87:...:c:...)
      Shared_Key     : AEAD_Key;      -- Derived from ML-KEM
      Call_Count     : Natural;
      Max_Calls      : Positive;
      Is_Active      : Boolean;
   end record;

   procedure Create_Session (
      Initiator_Addr : Address;
      Initiator_SK   : ML_KEM_Decaps_Key;
      Module_Addr    : Address;
      Module_PK      : ML_KEM_Encaps_Key;
      Max_Calls      : Positive;
      Session        : out Session;
      KEM_CT         : out ML_KEM_Ciphertext;  -- Send to module
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Initiator_Addr.Entity = User and Module_Addr.Entity = Contract;

end Anubis_Gate;
```

#### 4.4 Whisper (Confidential Values)

```ada
package Anubis_Whisper with
   SPARK_Mode => On
is
   --  Lattice-based Pedersen commitments

   type Commitment is record
      Value : Byte_Array (0 .. 63);  -- 512-bit commitment
   end record;

   procedure Commit (
      Value    : Word64;
      Blinding : Byte_Array;
      Result   : out Commitment
   ) with
      Global => null;

   --  Homomorphic addition
   procedure Add_Commitments (
      A, B   : Commitment;
      Result : out Commitment
   ) with
      Global => null;

   --  Range proof (prove value in [0, 2^64) without revealing)
   type Range_Proof is private;

   procedure Create_Range_Proof (
      Value    : Word64;
      Blinding : Byte_Array;
      Commit   : Commitment;
      Proof    : out Range_Proof;
      Success  : out Boolean
   ) with
      Global => null;

   function Verify_Range_Proof (
      Commit : Commitment;
      Proof  : Range_Proof
   ) return Boolean with
      Global => null;

end Anubis_Whisper;
```

#### 4.5 Veil (Zero-Knowledge Proofs)

```ada
package Anubis_Veil with
   SPARK_Mode => On
is
   --  Lattice-based ZK proofs (post-quantum)

   type Statement is record
      Public_Inputs : Byte_Array (0 .. 1023);
      Input_Len     : Natural;
      Claim         : Claim_Kind;
   end record;

   type Claim_Kind is (
      Claim_Knowledge,    -- "I know x s.t. f(x) = y"
      Claim_Range,        -- "Value in [a,b]"
      Claim_Membership,   -- "Value in set S"
      Claim_Equality      -- "Encrypted equals public"
   );

   procedure Generate_Proof (
      Stmt    : Statement;
      Witness : Byte_Array;
      Proof   : out ZK_Proof;
      Success : out Boolean
   ) with
      Global => null;

   function Verify_Proof (
      Stmt  : Statement;
      Proof : ZK_Proof
   ) return Boolean with
      Global => null;

end Anubis_Veil;
```

### Phase 5: State Management

#### 5.1 Khepri MPT (Merkle Patricia Trie)

```ada
package Anubis_Khepri with
   SPARK_Mode => On
is
   type Trie is limited private;

   --  Keys are derived from AAS-001 addresses
   procedure Insert (
      T       : in Out Trie;
      Address : Anubis_Address_Types.Address;  -- AAS-001
      Key     : Hash256;
      Value   : Byte_Array;
      Success : out Boolean
   ) with
      Global => null,
      Pre => Address.Valid;

   procedure Lookup (
      T       : Trie;
      Address : Anubis_Address_Types.Address;
      Key     : Hash256;
      Value   : out Byte_Array;
      Len     : out Natural;
      Found   : out Boolean
   ) with
      Global => null;

   function Root_Hash (T : Trie) return Hash256 with
      Global => null;

   --  Merkle proof
   procedure Generate_Proof (
      T       : Trie;
      Address : Anubis_Address_Types.Address;
      Key     : Hash256;
      Proof   : out Merkle_Proof
   ) with
      Global => null;

   function Verify_Proof (
      Root    : Hash256;
      Address : Anubis_Address_Types.Address;
      Key     : Hash256;
      Value   : Byte_Array;
      Proof   : Merkle_Proof
   ) return Boolean with
      Global => null;

end Anubis_Khepri;
```

#### 5.2 Encrypted Persistence

```ada
package Anubis_Store with
   SPARK_Mode => On
is
   --  All state encrypted at rest with ML-KEM + AEAD

   type Store is limited private;

   procedure Open_Store (
      S           : out Store;
      Path        : String;
      Master_PK   : ML_KEM_Encaps_Key;
      Master_SK   : ML_KEM_Decaps_Key;
      Success     : out Boolean
   ) with
      Global => null;

   procedure Save_Trie (
      S       : in Out Store;
      T       : Trie;
      Success : out Boolean
   ) with
      Global => null;

   procedure Load_Trie (
      S       : Store;
      T       : out Trie;
      Success : out Boolean
   ) with
      Global => null;

end Anubis_Store;
```

### Phase 6: TEE Core

#### 6.1 Key Management

```ada
package Anubis_TEE_Keys with
   SPARK_Mode => On
is
   type Key_Store is limited private;
   type Key_Handle is new Positive;

   --  Generate new identity (ML-DSA-87 + ML-KEM-1024 keypair)
   procedure Generate_Identity (
      Store     : in Out Key_Store;
      Passphrase : String;
      Label     : String;
      Handle    : out Key_Handle;
      Address   : out Anubis_Address_Types.Address;  -- AAS-001 format
      Success   : out Boolean
   ) with
      Global => null,
      Post => (if Success then Address.Valid and Address.Entity = User);

   --  Sign message (returns ML-DSA-87 signature)
   procedure Sign (
      Store     : Key_Store;
      Handle    : Key_Handle;
      Message   : Byte_Array;
      Signature : out ML_DSA_Signature;
      Success   : out Boolean
   ) with
      Global => null;

   --  Verify signature against AAS-001 address
   function Verify (
      Address   : Anubis_Address_Types.Address;
      Public_Key : ML_DSA_Public_Key;
      Message   : Byte_Array;
      Signature : ML_DSA_Signature
   ) return Boolean with
      Global => null,
      Pre => Address.Valid;
      --  Verifies that public key matches address derivation

end Anubis_TEE_Keys;
```

#### 6.2 Attestation

```ada
package Anubis_TEE_Attest with
   SPARK_Mode => On
is
   type Attestation_Report is record
      --  TEE identity (AAS-001 system address)
      TEE_Address    : Anubis_Address_Types.Address;  -- mldsa87:...:s:...

      --  Measurements
      Code_Hash      : Hash256;   -- SHA3-256 of TEE binary
      Config_Hash    : Hash256;   -- SHA3-256 of configuration
      State_Root     : Hash256;   -- Khepri trie root

      --  Timing (anti-replay)
      Timestamp      : Word64;
      Counter        : Word64;

      --  Signature (ML-DSA-87)
      Signature      : ML_DSA_Signature;
   end record;

   procedure Generate_Report (
      Keys    : Key_Store;
      Handle  : Key_Handle;  -- TEE's signing key
      State   : Trie;
      Report  : out Attestation_Report;
      Success : out Boolean
   ) with
      Global => null,
      Post => (if Success then Report.TEE_Address.Entity = System);

   function Verify_Report (
      Report    : Attestation_Report;
      TEE_PK    : ML_DSA_Public_Key
   ) return Boolean with
      Global => null;

end Anubis_TEE_Attest;
```

#### 6.3 Main Runtime

```ada
package Anubis_TEE_Runtime with
   SPARK_Mode => On
is
   type TEE_State is limited private;

   procedure Initialize (
      State      : out TEE_State;
      Passphrase : String;
      Data_Dir   : String;
      Network    : Network_Type;  -- main/test/dev/lab/staging
      Success    : out Boolean
   ) with
      Global => null;

   procedure Process_Request (
      State    : in Out TEE_State;
      Request  : Byte_Array;
      Response : out Byte_Array;
      Resp_Len : out Natural
   ) with
      Global => null;

   procedure Shutdown (State : in Out TEE_State) with
      Global => null;

private
   type TEE_State is record
      Keys         : Key_Store;
      State_Trie   : Trie;
      Registry     : Module_Registry;
      Network      : Network_Type;
      Counter      : Word64;
      Is_Running   : Boolean;
   end record;

end Anubis_TEE_Runtime;
```

---

## Request/Response Format

All communication uses AAS-001 addresses:

```json
{
  "method": "cvm_execute",
  "params": {
    "caller": "mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3-v8k4n",
    "module": "mldsa87:main:c:abc123...-checksum",
    "entry": "transfer",
    "args": "0x...",
    "signature": "0x..."
  },
  "id": 1
}
```

Response:
```json
{
  "result": {
    "status": "success",
    "data": "0x...",
    "state_root": "0x...",
    "counter": 12345
  },
  "id": 1
}
```

---

## Build System

### anubisvm.gpr

```ada
project Anubisvm is
   for Source_Dirs use (
      "src/core",
      "src/crypto/keccak",
      "src/crypto/sha3",
      "src/crypto/kmac",
      "src/crypto/aead",
      "src/crypto/kdf",
      "src/crypto/mldsa",
      "src/crypto/mlkem",
      "src/identity",
      "src/cvm",
      "src/privacy",
      "src/state",
      "src/tee",
      "src/io",
      "modules/cvm_token",
      "modules/cvm_vault",
      "main"
   );

   for Object_Dir use "obj";
   for Exec_Dir use "bin";
   for Main use ("anubis_main.adb");

   package Compiler is
      for Default_Switches ("Ada") use (
         "-gnat2022",
         "-gnatwa",
         "-gnatyg",
         "-gnato13",
         "-gnatVa",
         "-gnata"
      );
   end Compiler;

   package Prove is
      for Proof_Switches ("Ada") use (
         "--level=4",
         "--proof=all",
         "--timeout=120",
         "--memlimit=8000",
         "--warnings=continue"
      );
   end Prove;

end Anubisvm;
```

### Makefile

```makefile
.PHONY: all build prove test clean

all: prove build test

build:
	alr build

prove:
	alr exec -- gnatprove -P anubisvm.gpr --level=4 --proof=all --report=all

test: build
	./bin/test_sha3_kat
	./bin/test_mldsa
	./bin/test_mlkem_kat
	./bin/test_address
	./bin/test_kmac
	./bin/test_aead
	./bin/test_cvm

clean:
	rm -rf obj bin gnatprove

# Run TEE
run:
	./bin/anubis_main --network main --data ~/.anubis
```

---

## Security Properties (Mathematically Proven)

| Property | Guarantee | Mechanism |
|----------|-----------|-----------|
| Post-quantum signatures | 256-bit security vs quantum | ML-DSA-87 (FIPS 204) |
| Post-quantum encryption | 256-bit security vs quantum | ML-KEM-1024 (FIPS 203) |
| No buffer overflows | Proven at compile time | SPARK range analysis |
| No null dereferences | Proven at compile time | No access types in proven code |
| No uninitialized reads | Proven at compile time | SPARK flow analysis |
| No integer overflows | Proven at compile time | Modular types + checks |
| Deterministic execution | Same input = same output | `Depends` contracts |
| Memory zeroization | Sensitive data cleared | `Post` contracts |
| Address collision resistance | 256-bit | SHA3-256 domain separation |
| Constant-time comparison | No timing leaks | No early exits |

---

## Success Criteria

```
$ alr exec -- gnatprove -P anubisvm.gpr --level=4 --proof=all

Phase 1 of 2: generation of Global contracts ...
Phase 2 of 2: flow analysis and proof ...

Summary of SPARK analysis
=========================

 SPARK Analysis results        Total    Flow   Interval  CodePeer   Provers
 -------------------------------------------------------------------------
 Data dependencies                XX      XX          .         .         .
 Flow dependencies                XX      XX          .         .         .
 Initialization                   XX      XX          .         .         .
 Non-aliasing                     XX      XX          .         .         .
 Run-time checks                  XX       .         XX         .        XX
 Assertions                       XX       .          .         .        XX
 Functional contracts             XX       .          .         .        XX
 -------------------------------------------------------------------------
 Total                           XXX   XX (100%)   XX (100%)    .    XX (100%)

 Proved: 100%
```

All tests passing:
```
$ make test
test_sha3_kat:    PASSED (all vectors)
test_mldsa:       PASSED (keygen/sign/verify)
test_mlkem_kat:   PASSED (all vectors)
test_address:     PASSED (AAS-001 derivation/format/parse)
test_kmac:        PASSED (NIST vectors)
test_aead:        PASSED (encrypt/decrypt/tamper-detect)
test_cvm:         PASSED (module execution)
```

---

## Timeline

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| 1. Cleanup | 3 days | Pure SPARK codebase, existing tests pass |
| 2. Crypto | 1 week | KMAC, AEAD, KDF proven |
| 3. CVM | 1 week | Module runtime with AAS-001 identity |
| 4. Privacy | 2 weeks | Shield, Eye, Gate, Whisper, Veil |
| 5. State | 1 week | Khepri MPT, encrypted persistence |
| 6. Integration | 1 week | Full TEE, CLI, attestation |

**Total: ~6 weeks to fully proven, post-quantum TEE**
