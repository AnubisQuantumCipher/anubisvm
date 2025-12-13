# AnubisVM SDK Reference

Complete API reference for the KHEPRI Smart Contract SDK.

## Overview

The AnubisVM SDK consists of several packages:

| Package | Description |
|---------|-------------|
| `Khepri_Types` | Core types (Uint256, Address, Balance) |
| `Khepri_Crypto` | Cryptographic primitives |
| `Aegis_VM_Types` | Low-level VM types |
| `CVM_Types` | CVM execution types |
| `CVM_Interface` | CVM entry point interface |
| `TEE_Keys` | TEE key management |
| `Khepri_State_Trie` | State storage |

---

## Khepri_Types

Core types for contract development.

### Numeric Types

```ada
--  256-bit unsigned integer
subtype Uint256 is U256;

--  Token balance
subtype Balance is Uint256;

--  Wei (smallest unit)
subtype Wei is Uint256;

--  Token ID for NFTs
subtype Token_ID is Uint256;
```

### Constants

```ada
Zero : constant Uint256 := (Limbs => (0, 0, 0, 0));
One  : constant Uint256 := (Limbs => (1, 0, 0, 0));
Max  : constant Uint256 := (Limbs => (Word64'Last, Word64'Last,
                                       Word64'Last, Word64'Last));
```

### Address Types

```ada
--  Contract address (32 bytes, derived from ML-DSA-87 public key)
subtype Address is Contract_Address;

--  Zero address constant
Null_Address : constant Address := (others => 0);
```

### Hash Types

```ada
subtype Bytes32 is Hash256;
subtype Bytes64 is Hash512;

Bytes32_Zero : constant Bytes32 := (others => 0);
```

### Conversion Functions

```ada
--  Convert Word64 to Uint256
function To_U256 (V : Word64) return Uint256;

--  Convert Uint256 to Word64 (truncates)
function To_U64 (V : Uint256) return Word64;

--  From natural (up to 64 bits)
function From_Natural (N : Natural) return Uint256;
```

### Arithmetic Operations

```ada
function "+" (Left, Right : Uint256) return Uint256;
function "-" (Left, Right : Uint256) return Uint256;
function "*" (Left, Right : Uint256) return Uint256;
function "/" (Left, Right : Uint256) return Uint256
   with Pre => not Is_Zero (Right);
function "mod" (Left, Right : Uint256) return Uint256
   with Pre => not Is_Zero (Right);
```

### Comparison Operations

```ada
function "=" (Left, Right : Uint256) return Boolean;
function "<" (Left, Right : Uint256) return Boolean;
function "<=" (Left, Right : Uint256) return Boolean;
function ">" (Left, Right : Uint256) return Boolean;
function ">=" (Left, Right : Uint256) return Boolean;
```

### Bitwise Operations

```ada
function "and" (Left, Right : Uint256) return Uint256;
function "or" (Left, Right : Uint256) return Uint256;
function "xor" (Left, Right : Uint256) return Uint256;
function "not" (Value : Uint256) return Uint256;
```

### Storage Map Types

```ada
Max_Map_Entries : constant := 256;

type Map_Entry is record
   Key   : Uint256;
   Value : Uint256;
   Valid : Boolean;
end record;

subtype Map_Index is Natural range 0 .. Max_Map_Entries - 1;
type Storage_Map is array (Map_Index) of Map_Entry;

Empty_Map : constant Storage_Map;
```

### Address Map Types

```ada
type Address_Entry is record
   Addr  : Address;
   Value : Uint256;
   Valid : Boolean;
end record;

type Address_Map is array (Map_Index) of Address_Entry;

Empty_Address_Map : constant Address_Map;
```

### Error Codes

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

### Result Types

```ada
type Result (Success : Boolean := False) is record
   case Success is
      when True =>
         Value : Uint256;
      when False =>
         Error : Error_Code;
   end case;
end record;

function Ok (Value : Uint256) return Result
   with Post => Ok'Result.Success and Ok'Result.Value = Value;

function Err (Code : Error_Code) return Result
   with Pre => Code /= No_Error,
        Post => not Err'Result.Success and Err'Result.Error = Code;
```

### Context Types

```ada
type Msg_Context is record
   Sender : Address;      -- Transaction origin
   Value  : Wei;          -- Value sent with call
   Data   : Bytes32;      -- Hash of call data
end record;

type Block_Context is record
   Number     : Uint256;    -- Block number
   Timestamp  : Uint256;    -- Unix timestamp
   Coinbase   : Address;    -- Block producer
   Gas_Limit  : Uint256;    -- Block gas limit
   Base_Fee   : Uint256;    -- Base fee per gas
   Chain_ID   : Uint256;    -- Chain identifier
end record;

type Tx_Context is record
   Origin    : Address;    -- Original sender
   Gas_Price : Uint256;    -- Gas price in wei
   Gas_Limit : Uint256;    -- Gas limit for tx
end record;
```

### String Types

```ada
Max_String_Length : constant := 32;

type Bounded_String is record
   Data   : Contract_String;
   Length : Natural;
end record;

Empty_String : constant Bounded_String;
```

---

## Khepri_Crypto

Cryptographic primitives.

### Key Sizes

```ada
--  ML-DSA-87 (FIPS 204)
MLDSA_Public_Key_Size  : constant := 2592;
MLDSA_Secret_Key_Size  : constant := 4896;
MLDSA_Signature_Size   : constant := 4627;

--  ML-KEM-1024 (FIPS 203)
MLKEM_Public_Key_Size  : constant := 1568;
MLKEM_Secret_Key_Size  : constant := 3168;
MLKEM_Ciphertext_Size  : constant := 1568;
MLKEM_Shared_Key_Size  : constant := 32;
```

### Key Types

```ada
type MLDSA_Public_Key is array (0 .. MLDSA_Public_Key_Size - 1) of Byte;
type MLDSA_Secret_Key is array (0 .. MLDSA_Secret_Key_Size - 1) of Byte;
type MLDSA_Signature is array (0 .. MLDSA_Signature_Size - 1) of Byte;

type MLKEM_Public_Key is array (0 .. MLKEM_Public_Key_Size - 1) of Byte;
type MLKEM_Secret_Key is array (0 .. MLKEM_Secret_Key_Size - 1) of Byte;
type MLKEM_Ciphertext is array (0 .. MLKEM_Ciphertext_Size - 1) of Byte;
type MLKEM_Shared_Key is array (0 .. MLKEM_Shared_Key_Size - 1) of Byte;
```

### Crypto Result

```ada
type Crypto_Result is (
   Crypto_Success,
   Crypto_Invalid_Key,
   Crypto_Invalid_Signature,
   Crypto_Decapsulation_Failure,
   Crypto_Out_Of_Gas,
   Crypto_Error
);
```

### Hash Functions

```ada
--  SHA3-256 (Gas: 30 + 6 per 32 bytes)
function SHA3_256 (Data : Byte_Array) return Hash_256
   with Pre => Data'Length <= 2**16;

function SHA3_256_Bytes32 (Data : Bytes32) return Hash_256;

function SHA3_256_U256 (Value : Uint256) return Hash_256;

--  Keccak-256 (Ethereum-compatible)
function Keccak_256 (Data : Byte_Array) return Hash_256
   with Pre => Data'Length <= 2**16;

--  SHA3-512
function SHA3_512 (Data : Byte_Array) return Hash_512
   with Pre => Data'Length <= 2**16;
```

### Address Operations

```ada
--  Derive address from ML-DSA-87 public key
function Derive_Address (PK : MLDSA_Public_Key) return Address;

--  Check if address is valid (non-zero)
function Is_Valid_Address (Addr : Address) return Boolean;
```

### ML-DSA-87 Signatures

```ada
--  Verify signature (Gas: 30,000)
function MLDSA_Verify (
   Message    : Byte_Array;
   Signature  : MLDSA_Signature;
   Public_Key : MLDSA_Public_Key
) return Crypto_Result
   with Pre => Message'Length <= 2**16;

--  Verify with pre-hashed message
function MLDSA_Verify_Hash (
   Message_Hash : Hash_256;
   Signature    : MLDSA_Signature;
   Public_Key   : MLDSA_Public_Key
) return Crypto_Result;

--  Verify and get signer address
function Verify_And_Get_Signer (
   Message    : Byte_Array;
   Signature  : MLDSA_Signature;
   Public_Key : MLDSA_Public_Key
) return Address
   with Pre => Message'Length <= 2**16;
```

### ML-KEM-1024 Key Encapsulation

```ada
--  Encapsulate (Gas: 25,000)
procedure MLKEM_Encaps (
   Public_Key  : in  MLKEM_Public_Key;
   Ciphertext  : out MLKEM_Ciphertext;
   Shared_Key  : out MLKEM_Shared_Key;
   Result      : out Crypto_Result
);

--  Decapsulate (Gas: 25,000)
procedure MLKEM_Decaps (
   Secret_Key  : in  MLKEM_Secret_Key;
   Ciphertext  : in  MLKEM_Ciphertext;
   Shared_Key  : out MLKEM_Shared_Key;
   Result      : out Crypto_Result
);
```

### Utility Functions

```ada
--  Constant-time hash comparison
function Hash_Equal (A, B : Hash_256) return Boolean;

--  Constant-time address comparison
function Address_Equal (A, B : Address) return Boolean;

--  Calculate function selector
function Calculate_Signature (
   Declaration : String
) return Hash_256
   with Pre => Declaration'Length <= 256;

--  Zeroization
procedure Zeroize_Hash (H : out Hash_256)
   with Post => (for all I in H'Range => H (I) = 0);

procedure Zeroize_Shared_Key (K : out MLKEM_Shared_Key)
   with Post => (for all I in K'Range => K (I) = 0);
```

### Encoding Helpers

```ada
function Address_To_Bytes (Addr : Address) return Bytes32;

function U256_To_Bytes_BE (Value : Uint256) return Bytes32;

function Encode_Domain (
   Name     : Bounded_String;
   Version  : Bounded_String;
   Chain_ID : Uint256;
   Contract : Address
) return Hash_256;

function Encode_Struct (
   Type_Hash : Hash_256;
   Fields    : Byte_Array
) return Hash_256
   with Pre => Fields'Length <= 1024;
```

---

## TEE_Keys

Trusted Execution Environment key management.

### Key Types

```ada
Key_Size : constant := 32;

subtype Master_Seal_Key is Byte_Array (0 .. Key_Size - 1);
subtype Storage_Key is Byte_Array (0 .. Key_Size - 1);
subtype Session_Key is Byte_Array (0 .. Key_Size - 1);

subtype DSA_Public_Key is Byte_Array (0 .. 2591);
subtype DSA_Secret_Key is Byte_Array (0 .. 4895);
subtype DSA_Signature is Byte_Array (0 .. 4626);

subtype KEM_Encaps_Key is Byte_Array (0 .. 1567);
subtype KEM_Decaps_Key is Byte_Array (0 .. 3167);
subtype KEM_Ciphertext is Byte_Array (0 .. 1567);
subtype KEM_Shared_Secret is Byte_Array (0 .. 31);
```

### Key Bundle

```ada
type TEE_Key_Bundle is record
   MSK         : Master_Seal_Key;
   Storage     : Storage_Key;
   Attest_PK   : DSA_Public_Key;
   Attest_SK   : DSA_Secret_Key;
   KEM_EK      : KEM_Encaps_Key;
   KEM_DK      : KEM_Decaps_Key;
   Initialized : Boolean;
end record;

Empty_Bundle : constant TEE_Key_Bundle;
```

### Key Generation

```ada
procedure Initialize_Keys (
   Entropy : Byte_Array;
   Bundle  : out TEE_Key_Bundle;
   Success : out Boolean
) with
   Pre => Entropy'Length = 64,
   Post => (if Success then Bundle.Initialized);

procedure Derive_All_Keys (
   MSK    : Master_Seal_Key;
   Bundle : out TEE_Key_Bundle
) with
   Post => Bundle.Initialized;

procedure Generate_Attestation_Keys (
   Seed    : Byte_Array;
   PK      : out DSA_Public_Key;
   SK      : out DSA_Secret_Key;
   Success : out Boolean
) with
   Pre => Seed'Length = 32;

procedure Generate_KEM_Keys (
   Seed    : Byte_Array;
   EK      : out KEM_Encaps_Key;
   DK      : out KEM_Decaps_Key;
   Success : out Boolean
) with
   Pre => Seed'Length = 64;
```

### Key Derivation

```ada
procedure Derive_Storage_Key (
   MSK : Master_Seal_Key;
   Key : out Storage_Key
);

procedure Derive_CVM_Key (
   MSK    : Master_Seal_Key;
   CVM_ID : Byte_Array;
   Key    : out Session_Key
) with
   Pre => CVM_ID'Length = 32;

procedure Derive_Connection_Key (
   MSK        : Master_Seal_Key;
   Session_ID : Byte_Array;
   Key        : out Session_Key
) with
   Pre => Session_ID'Length = 32;
```

### Data Sealing

```ada
procedure Seal_Data (
   Key        : Storage_Key;
   Plaintext  : Byte_Array;
   Nonce      : Byte_Array;
   Ciphertext : out Byte_Array;
   Tag        : out Byte_Array;
   Success    : out Boolean
) with
   Pre => Nonce'Length = 24 and
          Ciphertext'Length = Plaintext'Length and
          Tag'Length = 32;

procedure Unseal_Data (
   Key        : Storage_Key;
   Ciphertext : Byte_Array;
   Nonce      : Byte_Array;
   Tag        : Byte_Array;
   Plaintext  : out Byte_Array;
   Success    : out Boolean
) with
   Pre => Nonce'Length = 24 and
          Plaintext'Length = Ciphertext'Length and
          Tag'Length = 32;
```

### Key Exchange

```ada
procedure Decapsulate (
   DK : KEM_Decaps_Key;
   CT : KEM_Ciphertext;
   SS : out KEM_Shared_Secret
);

procedure Encapsulate (
   EK     : KEM_Encaps_Key;
   Random : Byte_Array;
   CT     : out KEM_Ciphertext;
   SS     : out KEM_Shared_Secret
) with
   Pre => Random'Length = 32;
```

### Zeroization

```ada
procedure Zeroize_Bundle (Bundle : in Out TEE_Key_Bundle)
   with Post => not Bundle.Initialized;

procedure Zeroize_MSK (Key : in Out Master_Seal_Key)
   with Post => (for all I in Key'Range => Key (I) = 0);

procedure Zeroize_Session (Key : in Out Session_Key)
   with Post => (for all I in Key'Range => Key (I) = 0);

procedure Zeroize_DSA_SK (Key : in Out DSA_Secret_Key)
   with Post => (for all I in Key'Range => Key (I) = 0);

procedure Zeroize_KEM_DK (Key : in Out KEM_Decaps_Key)
   with Post => (for all I in Key'Range => Key (I) = 0);
```

---

## Khepri_State_Trie

Merkle Patricia Trie state storage.

### Account State

```ada
type Account_State is record
   Nonce        : Word64;
   Balance      : U256;
   Storage_Root : Hash_256;
   Code_Hash    : Hash_256;
end record;

Empty_Account : constant Account_State;
```

### State Errors

```ada
type State_Error is (
   Error_None,
   Error_Account_Not_Found,
   Error_Invalid_State,
   Error_Trie_Error,
   Error_Overflow
);
```

### Initialization

```ada
procedure Initialize;

procedure Initialize_From_Root (
   Root    : Hash_256;
   Success : out Boolean
);
```

### Account Operations

```ada
procedure Get_Account (
   Addr    : Address;
   Account : out Account_State;
   Found   : out Boolean;
   Error   : out State_Error
);

procedure Set_Account (
   Addr    : Address;
   Account : Account_State;
   Success : out Boolean;
   Error   : out State_Error
);

procedure Delete_Account (
   Addr    : Address;
   Success : out Boolean;
   Error   : out State_Error
);

function Account_Exists (Addr : Address) return Boolean;
```

### Balance Operations

```ada
function Get_Balance (Addr : Address) return U256;

procedure Add_Balance (
   Addr    : Address;
   Amount  : U256;
   Success : out Boolean;
   Error   : out State_Error
);

procedure Sub_Balance (
   Addr    : Address;
   Amount  : U256;
   Success : out Boolean;
   Error   : out State_Error
);

procedure Transfer (
   From    : Address;
   To      : Address;
   Amount  : U256;
   Success : out Boolean;
   Error   : out State_Error
);
```

### Nonce Operations

```ada
function Get_Nonce (Addr : Address) return Word64;

procedure Increment_Nonce (
   Addr    : Address;
   Success : out Boolean;
   Error   : out State_Error
);

procedure Set_Nonce (
   Addr  : Address;
   Nonce : Word64;
   Success : out Boolean;
   Error   : out State_Error
);
```

### Code Operations

```ada
function Get_Code_Hash (Addr : Address) return Hash_256;

procedure Set_Code_Hash (
   Addr      : Address;
   Code_Hash : Hash_256;
   Success   : out Boolean;
   Error     : out State_Error
);

function Is_Contract (Addr : Address) return Boolean;
```

### State Root

```ada
function State_Root return Hash_256;

procedure Commit (New_Root : out Hash_256);
```

### Snapshots

```ada
procedure Create_Snapshot (
   Snapshot_ID : out Natural;
   Success     : out Boolean
);

procedure Revert_To_Snapshot (
   Snapshot_ID : Natural;
   Success     : out Boolean
);

procedure Discard_Snapshot (Snapshot_ID : Natural);
```

### Proofs

```ada
procedure Generate_Account_Proof (
   Addr    : Address;
   Proof   : out Merkle_Proof;
   Success : out Boolean
);

procedure Verify_Account_Proof (
   Root    : Hash_256;
   Addr    : Address;
   Proof   : Merkle_Proof;
   Account : out Account_State;
   Valid   : out Boolean
);
```

---

## CVM_Interface

Cryptographically Verified Module interface.

### Entry Point Types

```ada
type CVM_Entry_Point is access procedure (
   Context : in     Call_Context;
   State   : in Out State_Array;
   Result  : out    Exec_Result
);

Max_Entry_Points : constant := 32;
subtype Entry_Point_Index is Natural range 0 .. Max_Entry_Points - 1;

type Entry_Point_Desc is record
   Selector : Method_Selector;
   Handler  : CVM_Entry_Point;
   Public   : Boolean;
   ReadOnly : Boolean;
end record;
```

### CVM Descriptor

```ada
type CVM_Descriptor is record
   Info        : CVM_Info;
   Entry_Count : Natural;
   Entries     : Entry_Point_Table;
end record;
```

### Interface Procedures

```ada
type CVM_Init_Proc is access procedure (
   Init_Params : Byte_Array;
   State       : out State_Array;
   Success     : out Boolean
);

type CVM_Execute_Proc is access procedure (
   Context : Call_Context;
   State   : in Out State_Array;
   Result  : out Exec_Result
);

type CVM_Get_Descriptor_Func is access function return CVM_Descriptor;
```

### Registration

```ada
type CVM_Registration is record
   Get_Descriptor : CVM_Get_Descriptor_Func;
   Init           : CVM_Init_Proc;
   Execute        : CVM_Execute_Proc;
end record;
```

### Helper Functions

```ada
procedure Find_Entry_Point (
   Desc     : CVM_Descriptor;
   Selector : Method_Selector;
   Index    : out Natural;
   Found    : out Boolean
) with
   Pre  => Is_Valid_Descriptor (Desc),
   Post => (if Found then Valid_Entry_Index (Desc, Index));

function Has_Capability (
   Caps : Capability_Set;
   Cap  : CVM_Capability
) return Boolean;

function Is_Authorized (
   Context : Call_Context;
   EP_Desc : Entry_Point_Desc
) return Boolean
   with Pre => Is_Valid_Context (Context);

function Success_Result (
   Data : Byte_Array;
   Len  : Natural
) return Exec_Result
   with Pre => Len <= Max_Return_Size;
```

---

## Usage Examples

### Basic Token Contract

```ada
with Khepri_Types;  use Khepri_Types;
with Khepri_Crypto; use Khepri_Crypto;

package My_Token is
   type State is record
      Owner   : Address;
      Supply  : Balance;
   end record;

   procedure Initialize (S : out State; Owner : Address; Supply : Balance);
   procedure Transfer (S : in out State; To : Address; Amount : Balance;
                       Caller : Address; Status : out Error_Code);
end My_Token;
```

### Secure Vault with TEE

```ada
with TEE_Keys; use TEE_Keys;

package Secure_Vault is
   procedure Store_Secret (
      Bundle : TEE_Key_Bundle;
      Data   : Byte_Array;
      Nonce  : Byte_Array;
      Result : out Byte_Array
   );
end Secure_Vault;
```

### State Management

```ada
with Khepri_State_Trie; use Khepri_State_Trie;

procedure Update_Balance is
   Success : Boolean;
   Error   : State_Error;
begin
   Add_Balance (Some_Address, Amount, Success, Error);
   if not Success then
      -- Handle error
   end if;
end Update_Balance;
```
