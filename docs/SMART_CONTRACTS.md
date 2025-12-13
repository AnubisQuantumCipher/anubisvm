# AnubisVM Smart Contract Development Guide

This guide covers everything you need to build, deploy, and interact with smart contracts on AnubisVM using the command-line interface.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Creating Contracts](#creating-contracts)
3. [Contract Structure](#contract-structure)
4. [Building Contracts](#building-contracts)
5. [Running Contracts](#running-contracts)
6. [SDK Reference](#sdk-reference)
7. [Cryptographic Operations](#cryptographic-operations)
8. [State Management](#state-management)
9. [Advanced Features](#advanced-features)

---

## Prerequisites

### Required Tools

```bash
# Verify GNAT toolchain is installed
which gprbuild
# Should return: ~/.local/share/alire/toolchains/.../bin/gprbuild

# Verify AnubisVM is built
ls -la core/bin/khepri_main
```

### Build AnubisVM

```bash
cd /path/to/anubisvm
make build
```

---

## Creating Contracts

### Using the CLI

Create a new contract project with all required files:

```bash
./core/bin/khepri_main contract new <contract_name>
```

**Example:**
```bash
./core/bin/khepri_main contract new my_token
```

This creates:
```
contracts/my_token/
├── my_token.gpr           # GPR project file
├── khepri.toml            # Contract manifest
└── src/
    ├── my_token.ads       # SPARK specification
    ├── my_token.adb       # Implementation
    └── my_token_main.adb  # Test harness
```

---

## Contract Structure

### Specification File (`.ads`)

```ada
--  My_Token - AnubisVM SPARK Contract
--  Features: KHEPRI SDK, THOTH Storage, ANKH Crypto
pragma SPARK_Mode (On);

with Interfaces;      use Interfaces;
with Khepri_Types;    use Khepri_Types;
with Khepri_Crypto;   use Khepri_Crypto;
with Aegis_VM_Types;

package My_Token with SPARK_Mode => On is

   --  Contract version (AAS-001 compliant)
   Contract_Version : constant Uint256 := One;

   --  State record with quantum-safe types
   type Contract_State is record
      Version      : Uint256;
      Owner        : Address;
      Total_Supply : Balance;
      Initialized  : Boolean;
   end record;

   Empty_State : constant Contract_State := (
      Version      => Zero,
      Owner        => Null_Address,
      Total_Supply => Zero,
      Initialized  => False
   );

   --  Initialize contract with owner (ML-DSA-87 derived address)
   procedure Initialize (
      State  : out Contract_State;
      Owner  : Address;
      Supply : Balance
   ) with
      Global => null,
      Pre    => Is_Valid_Address (Owner),
      Post   => State.Initialized and State.Version = One;

   --  Check if caller is owner
   function Is_Owner (State : Contract_State; Caller : Address)
      return Boolean with Global => null;

   --  Transfer tokens (requires ownership)
   procedure Transfer (
      State  : in out Contract_State;
      To     : Address;
      Amount : Balance;
      Caller : Address;
      Status : out Error_Code
   ) with
      Global => null,
      Pre    => State.Initialized and Is_Valid_Address (To);

   --  Query total supply
   function Get_Supply (State : Contract_State) return Balance
      with Global => null;

   --  Query owner address
   function Get_Owner (State : Contract_State) return Address
      with Global => null;

   --  Compute state hash (SHA3-256)
   function State_Hash (State : Contract_State) return Hash_256
      with Global => null;

end My_Token;
```

### Implementation File (`.adb`)

```ada
--  My_Token - AnubisVM Contract Implementation
pragma SPARK_Mode (On);

package body My_Token with SPARK_Mode => On is

   procedure Initialize (
      State  : out Contract_State;
      Owner  : Address;
      Supply : Balance
   ) is
   begin
      State := (
         Version      => One,
         Owner        => Owner,
         Total_Supply => Supply,
         Initialized  => True
      );
   end Initialize;

   function Is_Owner (State : Contract_State; Caller : Address)
      return Boolean is
   begin
      return Address_Equal (State.Owner, Caller);
   end Is_Owner;

   procedure Transfer (
      State  : in Out Contract_State;
      To     : Address;
      Amount : Balance;
      Caller : Address;
      Status : out Error_Code
   ) is
      pragma Unreferenced (To);
   begin
      if not Is_Owner (State, Caller) then
         Status := Unauthorized;
         return;
      end if;
      if State.Total_Supply < Amount then
         Status := Insufficient_Balance;
         return;
      end if;
      State.Total_Supply := State.Total_Supply - Amount;
      Status := No_Error;
   end Transfer;

   function Get_Supply (State : Contract_State) return Balance is
      (State.Total_Supply);

   function Get_Owner (State : Contract_State) return Address is
      (State.Owner);

   function State_Hash (State : Contract_State) return Hash_256 is
      Bytes : Aegis_VM_Types.Byte_Array (0 .. 31) := [others => 0];
      pragma Unreferenced (State);
   begin
      return SHA3_256 (Bytes);
   end State_Hash;

end My_Token;
```

### Test Harness (`.._main.adb`)

```ada
--  My_Token - AnubisVM Contract Entry Point
with My_Token;      use My_Token;
with Khepri_Types;  use Khepri_Types;
with Khepri_Crypto; use Khepri_Crypto;
with Ada.Text_IO;   use Ada.Text_IO;

procedure My_Token_Main is
   State  : Contract_State := Empty_State;
   Owner  : Address := [others => 1];  --  Test owner address
   Supply : Balance := From_Natural (1000);
begin
   Put_Line ("AnubisVM Contract Test");
   Put_Line ("======================");

   --  Initialize contract with owner and supply
   Initialize (State, Owner, Supply);
   Put_Line ("Contract initialized");

   --  Verify owner check
   pragma Assert (Is_Owner (State, Owner));
   Put_Line ("Owner verification passed");

   Put_Line ("All tests passed!");
end My_Token_Main;
```

### GPR Project File

```ada
project My_Token is

   --  AnubisVM SDK paths
   SDK_Root := "/path/to/anubisvm/core/src";

   for Source_Dirs use (
      "src",
      SDK_Root & "/**"
   );
   for Object_Dir use "obj";
   for Exec_Dir use "bin";
   for Main use ("my_token_main.adb");

   package Compiler is
      for Default_Switches ("Ada") use
         ("-gnatwa", "-gnat2022", "-gnata", "-O2");
   end Compiler;

   package Prove is
      for Proof_Switches ("Ada") use ("--level=2");
   end Prove;

end My_Token;
```

---

## Building Contracts

### Using the CLI

```bash
./core/bin/khepri_main contract build <contract_name>
```

**Example:**
```bash
./core/bin/khepri_main contract build my_token
```

### Manual Build

```bash
cd contracts/my_token
gprbuild -P my_token.gpr -j0
```

### Build Output

```
contracts/my_token/
├── bin/
│   └── my_token_main     # Executable binary
└── obj/
    └── *.o, *.ali        # Object files
```

---

## Running Contracts

### Using the CLI

```bash
./core/bin/khepri_main contract run <contract_name>
```

**Example:**
```bash
./core/bin/khepri_main contract run my_token
```

### Manual Execution

```bash
# Set library path for runtime dependencies
export DYLD_LIBRARY_PATH=/path/to/anubisvm/core/lib

# Run the contract
./contracts/my_token/bin/my_token_main
```

**Expected Output:**
```
AnubisVM Contract Test
======================
Contract initialized
Owner verification passed
All tests passed!
```

---

## SDK Reference

### Khepri_Types Package

Core types for contract development:

| Type | Description |
|------|-------------|
| `Uint256` | 256-bit unsigned integer |
| `Address` | 32-byte contract/account address |
| `Balance` | Token balance (alias for Uint256) |
| `Error_Code` | Contract execution error codes |
| `Result` | Result type for fallible operations |
| `Storage_Map` | Key-value storage abstraction |

**Constants:**
```ada
Zero : constant Uint256;          -- 0
One  : constant Uint256;          -- 1
Max  : constant Uint256;          -- 2^256 - 1
Null_Address : constant Address;  -- All zeros
```

**Error Codes:**
```ada
type Error_Code is (
   No_Error,              -- Success
   Insufficient_Balance,  -- Not enough tokens
   Unauthorized,          -- Caller not authorized
   Invalid_Input,         -- Bad parameters
   Overflow,              -- Arithmetic overflow
   Underflow,             -- Arithmetic underflow
   Not_Found,             -- Key/entry not found
   Already_Exists,        -- Entry already exists
   Out_Of_Gas,            -- Gas exhausted
   Contract_Error         -- Generic error
);
```

**Arithmetic Operations:**
```ada
function "+" (Left, Right : Uint256) return Uint256;
function "-" (Left, Right : Uint256) return Uint256;
function "*" (Left, Right : Uint256) return Uint256;
function "/" (Left, Right : Uint256) return Uint256;
function "mod" (Left, Right : Uint256) return Uint256;
```

**Comparison Operations:**
```ada
function "=" (Left, Right : Uint256) return Boolean;
function "<" (Left, Right : Uint256) return Boolean;
function "<=" (Left, Right : Uint256) return Boolean;
function ">" (Left, Right : Uint256) return Boolean;
function ">=" (Left, Right : Uint256) return Boolean;
```

**Conversion Helpers:**
```ada
function From_Natural (N : Natural) return Uint256;
function To_U256 (V : Word64) return Uint256;
function To_U64 (V : Uint256) return Word64;
```

### Khepri_Crypto Package

Cryptographic primitives:

**Hash Functions:**
```ada
function SHA3_256 (Data : Byte_Array) return Hash_256;
function SHA3_512 (Data : Byte_Array) return Hash_512;
function Keccak_256 (Data : Byte_Array) return Hash_256;
```

**Address Operations:**
```ada
function Derive_Address (PK : MLDSA_Public_Key) return Address;
function Is_Valid_Address (Addr : Address) return Boolean;
function Address_Equal (A, B : Address) return Boolean;
```

**Post-Quantum Signatures (ML-DSA-87):**
```ada
function MLDSA_Verify (
   Message    : Byte_Array;
   Signature  : MLDSA_Signature;
   Public_Key : MLDSA_Public_Key
) return Crypto_Result;
```

**Post-Quantum Key Encapsulation (ML-KEM-1024):**
```ada
procedure MLKEM_Encaps (
   Public_Key  : in  MLKEM_Public_Key;
   Ciphertext  : out MLKEM_Ciphertext;
   Shared_Key  : out MLKEM_Shared_Key;
   Result      : out Crypto_Result
);

procedure MLKEM_Decaps (
   Secret_Key  : in  MLKEM_Secret_Key;
   Ciphertext  : in  MLKEM_Ciphertext;
   Shared_Key  : out MLKEM_Shared_Key;
   Result      : out Crypto_Result
);
```

**Secure Comparison:**
```ada
function Hash_Equal (A, B : Hash_256) return Boolean;
function Address_Equal (A, B : Address) return Boolean;
```

**Zeroization:**
```ada
procedure Zeroize_Hash (H : out Hash_256);
procedure Zeroize_Shared_Key (K : out MLKEM_Shared_Key);
```

---

## Cryptographic Operations

### Hashing Data

```ada
with Khepri_Crypto; use Khepri_Crypto;
with Aegis_VM_Types; use Aegis_VM_Types;

procedure Hash_Example is
   Data : Byte_Array (0 .. 31) := (others => 16#AB#);
   Hash : Hash_256;
begin
   Hash := SHA3_256 (Data);
end Hash_Example;
```

### Verifying Signatures

```ada
with Khepri_Crypto; use Khepri_Crypto;

procedure Verify_Example is
   Message   : Byte_Array (0 .. 63) := (others => 0);
   Signature : MLDSA_Signature;
   Public_Key : MLDSA_Public_Key;
   Result    : Crypto_Result;
begin
   Result := MLDSA_Verify (Message, Signature, Public_Key);
   if Result = Crypto_Success then
      -- Signature is valid
      null;
   end if;
end Verify_Example;
```

### Key Encapsulation

```ada
with Khepri_Crypto; use Khepri_Crypto;

procedure KEM_Example is
   Public_Key  : MLKEM_Public_Key;
   Ciphertext  : MLKEM_Ciphertext;
   Shared_Key  : MLKEM_Shared_Key;
   Result      : Crypto_Result;
begin
   MLKEM_Encaps (Public_Key, Ciphertext, Shared_Key, Result);
   if Result = Crypto_Success then
      -- Use Shared_Key for encryption
      null;
   end if;
end KEM_Example;
```

---

## State Management

### Using Storage Maps

```ada
with Khepri_Types; use Khepri_Types;

procedure Storage_Example is
   Balances : Storage_Map := Empty_Map;
   Key      : Uint256 := From_Natural (42);
   Value    : Uint256 := From_Natural (1000);
begin
   -- Store value
   Balances (0) := (Key => Key, Value => Value, Valid => True);

   -- Read value
   if Balances (0).Valid and Balances (0).Key = Key then
      Value := Balances (0).Value;
   end if;
end Storage_Example;
```

### Using Address Maps

```ada
with Khepri_Types; use Khepri_Types;

procedure Address_Map_Example is
   Balances : Address_Map := Empty_Address_Map;
   User     : Address := (others => 1);
   Balance  : Uint256 := From_Natural (1000);
begin
   -- Store balance for address
   Balances (0) := (Addr => User, Value => Balance, Valid => True);
end Address_Map_Example;
```

---

## Advanced Features

### TEE Integration

For contracts requiring confidential computing:

```ada
with TEE_Keys; use TEE_Keys;

procedure TEE_Example is
   Bundle  : TEE_Key_Bundle;
   Entropy : Byte_Array (0 .. 63) := (others => 0);
   Success : Boolean;
begin
   Initialize_Keys (Entropy, Bundle, Success);
   if Success then
      -- Use Bundle.Storage for encryption
      null;
   end if;
end TEE_Example;
```

### CVM Interface

For advanced contract patterns:

```ada
with CVM_Interface; use CVM_Interface;
with CVM_Types;     use CVM_Types;

procedure CVM_Example is
   Context : Call_Context;
   State   : State_Array (0 .. 1023);
   Result  : Exec_Result;
begin
   -- Entry point dispatch
   if Is_Valid_Context (Context) then
      -- Process call
      Result := Success_Result (State (0 .. 31), 32);
   end if;
end CVM_Example;
```

### State Trie Operations

For Merkle Patricia Trie access:

```ada
with Khepri_State_Trie; use Khepri_State_Trie;

procedure Trie_Example is
   Addr    : Address := (others => 1);
   Account : Account_State;
   Found   : Boolean;
   Error   : State_Error;
begin
   Get_Account (Addr, Account, Found, Error);
   if Found then
      -- Use Account.Balance, Account.Nonce, etc.
      null;
   end if;
end Trie_Example;
```

---

## CLI Command Reference

### Contract Commands

| Command | Description |
|---------|-------------|
| `contract new <name>` | Create new contract project |
| `contract build <name>` | Build contract |
| `contract run <name>` | Run contract test harness |
| `contract prove <name>` | Run SPARK verification |

### Key Management

| Command | Description |
|---------|-------------|
| `keys new <name>` | Generate ML-DSA-87 keypair |
| `keys list` | List all keys |
| `keys show <name>` | Show key details |

### Address Operations

| Command | Description |
|---------|-------------|
| `address from-key <name>` | Derive address from key |
| `address info <addr>` | Show address details |

### Utility Commands

| Command | Description |
|---------|-------------|
| `version` | Show version information |
| `help` | Show help |

---

## SPARK Verification

### Running Proofs

```bash
# Via CLI
./core/bin/khepri_main contract prove my_token

# Manual
cd contracts/my_token
gnatprove -P my_token.gpr --level=2
```

### Proof Levels

| Level | Description | Use Case |
|-------|-------------|----------|
| 1 | Flow analysis | Quick checks |
| 2 | Standard proofs | Development |
| 3 | Extended proofs | Pre-release |
| 4 | Full proofs | Production |

### Common SPARK Contracts

```ada
-- Precondition
procedure Foo (X : Integer) with
   Pre => X > 0;

-- Postcondition
function Bar (X : Integer) return Integer with
   Post => Bar'Result >= X;

-- Global state annotation
procedure Baz with
   Global => null;  -- No global state access

-- Depends annotation
procedure Qux (A : in Integer; B : out Integer) with
   Depends => (B => A);
```

---

## Troubleshooting

### Build Errors

**"SDK_Root not found"**
- Ensure GPR file has correct path to AnubisVM SDK

**"Khepri_Types not found"**
- Ensure GPR includes `SDK_Root & "/**"` in Source_Dirs

**"Ambiguous operator"**
- Remove `use Aegis_VM_Types` and use qualified names

### Runtime Errors

**"Library not found"**
- Set `DYLD_LIBRARY_PATH` (macOS) or `LD_LIBRARY_PATH` (Linux)

**"Assertion failed"**
- Check contract preconditions are satisfied

### SPARK Proof Failures

**"Precondition might fail"**
- Add assertions before the call to help the prover

**"Range check might fail"**
- Add subtype constraints or explicit range checks

---

## Examples

See `contracts/` directory for complete examples:

- `sovereign_token/` - Basic token contract
- `quantum_vault_v2/` - Advanced vault with TEE
- `ats20_token/` - Full ATS-20 token standard

---

## Support

- GitHub Issues: https://github.com/AnubisQuantumCipher/anubisvm/issues
- Documentation: https://github.com/AnubisQuantumCipher/anubisvm/docs
