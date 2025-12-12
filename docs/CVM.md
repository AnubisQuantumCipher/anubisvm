# Cryptographically Verified Modules (CVM)

Technical specification for developing and deploying CVMs on AnubisVM.

## Overview

Cryptographically Verified Modules (CVMs) are native SPARK/Ada packages that implement smart contract logic. Unlike bytecode virtual machines, CVMs are:

1. **Compiled natively**: Execute at full hardware speed
2. **Formally verified**: Mathematical proofs of correctness at compile time
3. **Type-safe**: No runtime type errors
4. **Memory-safe**: No buffer overflows or use-after-free

## Architecture

### CVM Lifecycle

```
1. Development
   └── Write SPARK package implementing CVM_Interface
   └── Add Pre/Post contracts for all entry points
   └── Run gnatprove to verify all proofs pass

2. Compilation
   └── alr build compiles CVM into native code
   └── Code hash computed: SHA3-256(compiled_object)

3. Deployment
   └── CVM registered in CVM_Registry
   └── Address derived: SHA3-256("aegis-v1-mldsa87-c" || code_hash)
   └── Initial state set via Init procedure

4. Execution
   └── Transaction targets CVM address
   └── CVM_Dispatch routes to entry point by selector
   └── Entry point executes with state access
   └── Result returned to caller
```

### Core Types

#### CVM_Types Package

```ada
--  Address types
subtype Code_Hash is Hash256;      -- SHA3-256 of compiled code
subtype CVM_Address is Address;    -- 32-byte contract address
subtype Caller_Address is Address; -- 32-byte caller address

--  Buffer limits
Max_Param_Size   : constant := 4096;  -- 4 KB parameter buffer
Max_Return_Size  : constant := 4096;  -- 4 KB return buffer
Max_State_Size   : constant := 1024;  -- 1 KB per state slot
Max_State_Slots  : constant := 256;   -- 256 slots per CVM

--  Execution status
type Exec_Status is (
   Success,           -- Completed successfully
   Invalid_Caller,    -- Authorization failed
   Invalid_Params,    -- Parameter validation failed
   Invalid_State,     -- State inconsistency
   Out_Of_Gas,        -- Resource limit exceeded
   Proof_Failed,      -- Runtime assertion (should never happen)
   Unknown_Method,    -- Entry point not found
   Internal_Error     -- Unexpected error
);

--  Execution result
type Exec_Result is record
   Status      : Exec_Status;
   Return_Len  : Natural;
   Return_Data : Return_Buffer;
end record;
```

#### Call Context

```ada
type Call_Context is record
   Caller      : Caller_Address;  -- Transaction sender
   Target      : CVM_Address;     -- Target CVM
   Entry_Point : Hash256;         -- Method selector
   Param_Len   : Natural;         -- Parameter length
   Params      : Param_Buffer;    -- Parameter data
   Height      : Natural;         -- Block height
   Internal    : Boolean;         -- True if CVM-to-CVM call
end record;
```

#### State Slots

```ada
type State_Slot is record
   Index    : State_Index;   -- Slot number (0..255)
   Length   : Natural;       -- Value length
   Value    : State_Value;   -- Up to 1 KB data
   Modified : Boolean;       -- True if changed
end record;

type State_Array is array (State_Index) of State_Slot;
```

## CVM Interface

### Required Procedures

Every CVM must implement three procedures:

#### 1. Get_Descriptor

Returns CVM metadata and entry point table.

```ada
function Get_Descriptor return CVM_Descriptor;
```

#### 2. Init

Called once on deployment to initialize state.

```ada
procedure Init (
   Init_Params : in     Byte_Array;
   State       : out    State_Array;
   Success     : out    Boolean
);
```

#### 3. Execute

Called for each transaction. Dispatches to entry points.

```ada
procedure Execute (
   Context : in     Call_Context;
   State   : in Out State_Array;
   Result  : out    Exec_Result
);
```

### Entry Point Signature

All entry points must conform to:

```ada
procedure Entry_Point (
   Context : in     Call_Context;
   State   : in Out State_Array;
   Result  : out    Exec_Result
) with
   Pre  => Is_Valid_Context (Context),
   Post => Is_Valid_Exec_Result (Result);
```

### Method Selectors

Entry points are identified by method selectors:

```ada
Selector := SHA3_256 (Method_Name)
```

Example:
```
"Transfer" -> 0xa9059cbb...
"BalanceOf" -> 0x70a08231...
```

## Example Implementation

### Token Contract

```ada
pragma SPARK_Mode (On);

with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;
with Anubis_SHA3;

package ATS20_Token with
   SPARK_Mode => On
is
   --  State slot assignments
   Total_Supply_Slot : constant State_Index := 0;
   --  Slots 1-255: Balance[address_hash mod 255]

   --  Entry points
   procedure Transfer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Is_Valid_Exec_Result (Result);

   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Is_Valid_Exec_Result (Result);

   procedure Total_Supply (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Pre  => Is_Valid_Context (Context),
      Post => Is_Valid_Exec_Result (Result);

   --  CVM interface
   function Get_Descriptor return CVM_Descriptor;

   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   );

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   );

private
   --  Method selectors (SHA3-256 of method names)
   Transfer_Selector     : constant Method_Selector := (16#a9#, 16#05#, ...);
   Balance_Of_Selector   : constant Method_Selector := (16#70#, 16#a0#, ...);
   Total_Supply_Selector : constant Method_Selector := (16#18#, 16#16#, ...);

end ATS20_Token;
```

### Implementation Body

```ada
pragma SPARK_Mode (On);

package body ATS20_Token with
   SPARK_Mode => On
is
   procedure Transfer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      --  Parse parameters: To (32 bytes) + Amount (8 bytes)
      To_Addr    : Address;
      Amount     : Unsigned_64;
      From_Slot  : State_Index;
      To_Slot    : State_Index;
      From_Bal   : Unsigned_64;
      To_Bal     : Unsigned_64;
   begin
      --  Validate parameter length
      if Context.Param_Len /= 40 then
         Result := Error_Result (Invalid_Params);
         return;
      end if;

      --  Extract parameters
      To_Addr := Context.Params (0 .. 31);
      Amount := Unpack_U64 (Context.Params (32 .. 39));

      --  Compute state slot indices
      From_Slot := State_Index (Hash_To_Index (Context.Caller) mod 255 + 1);
      To_Slot := State_Index (Hash_To_Index (To_Addr) mod 255 + 1);

      --  Read balances
      From_Bal := Read_U64 (State (From_Slot));
      To_Bal := Read_U64 (State (To_Slot));

      --  Check sufficient balance
      if From_Bal < Amount then
         Result := Error_Result (Invalid_State);
         return;
      end if;

      --  Update balances (proven safe by precondition check)
      Write_U64 (State (From_Slot), From_Bal - Amount);
      Write_U64 (State (To_Slot), To_Bal + Amount);

      --  Return success
      Result := Empty_Result;
   end Transfer;

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
   begin
      --  Dispatch by method selector
      if Context.Entry_Point = Transfer_Selector then
         Transfer (Context, State, Result);
      elsif Context.Entry_Point = Balance_Of_Selector then
         Balance_Of (Context, State, Result);
      elsif Context.Entry_Point = Total_Supply_Selector then
         Total_Supply (Context, State, Result);
      else
         Result := Error_Result (Unknown_Method);
      end if;
   end Execute;

end ATS20_Token;
```

## Capabilities

CVMs operate under a capability system:

| Capability | Bit | Description |
|------------|-----|-------------|
| `Cap_Read_State` | 0 | Read own state slots |
| `Cap_Write_State` | 1 | Modify own state slots |
| `Cap_Call_CVM` | 2 | Invoke other CVMs |
| `Cap_Shield_State` | 3 | Use encrypted state (TEE) |
| `Cap_Eye_View` | 4 | Create viewing keys (TEE) |
| `Cap_Gate_Session` | 5 | Create private sessions (TEE) |
| `Cap_Whisper_Value` | 6 | Use confidential values (TEE) |
| `Cap_Veil_Proof` | 7 | Generate ZK proofs (TEE) |

### Default Capabilities

Standard CVMs receive:
- `Cap_Read_State`
- `Cap_Write_State`
- `Cap_Call_CVM`

### Privacy Capabilities (TEE)

Privacy-enabled CVMs additionally receive:
- `Cap_Shield_State`
- `Cap_Eye_View`
- `Cap_Gate_Session`
- `Cap_Whisper_Value`
- `Cap_Veil_Proof`

## State Management

### Slot Indexing

Each CVM has 256 state slots (0-255). Slot 0 is typically reserved for metadata.

### State Encoding

State values are raw byte arrays up to 1 KB. CVMs are responsible for encoding/decoding.

Recommended encodings:
- Unsigned integers: Big-endian
- Addresses: Raw 32 bytes
- Strings: Length-prefixed UTF-8
- Arrays: Length prefix + concatenated elements

### State Persistence

State is committed after successful execution:
1. Modified slots marked with `Modified => True`
2. Merkle Patricia Trie updated with new values
3. State root included in block

## CVM-to-CVM Calls

CVMs can invoke other CVMs via the dispatch mechanism:

```ada
procedure Call_CVM (
   Target      : CVM_Address;
   Entry_Point : Method_Selector;
   Params      : Byte_Array;
   Result      : out Exec_Result
);
```

### Call Restrictions

1. Maximum call depth: 8
2. Re-entrancy allowed (with care)
3. State changes atomic per transaction
4. Failed subcalls can be caught

## Formal Verification

### Required Contracts

Every CVM entry point must have:

```ada
with
   Pre  => Is_Valid_Context (Context),
   Post => Is_Valid_Exec_Result (Result);
```

### Recommended Contracts

Additional contracts for safety:

```ada
with
   Global  => null,  -- No hidden state
   Depends => (Result => (Context, State),
               State  => (Context, State));
```

### Loop Invariants

All loops must have invariants:

```ada
for I in Buffer'Range loop
   pragma Loop_Invariant (I >= Buffer'First);
   pragma Loop_Invariant (I <= Buffer'Last);
   --  ...
end loop;
```

### Running Verification

```bash
# Verify CVM package
alr exec -- gnatprove -P anubisvm.gpr --level=2 -j0 src/contracts/my_cvm.adb

# Full verification with statistics
alr exec -- gnatprove -P anubisvm.gpr --level=4 --timeout=120 --report=statistics
```

## Deployment

### Registration

CVMs are registered via `CVM_Registry`:

```ada
procedure Register_CVM (
   Registration : CVM_Registration;
   Code_Hash    : Hash256;
   Address      : out CVM_Address;
   Success      : out Boolean
);
```

### Address Derivation

CVM addresses follow AAS-001 v3.1:

```
domain := "aegis-v1-mldsa87-c"
payload := SHA3_256 (domain || code_hash)
address := Base32_Encode (payload) with checksum
```

### Initialization

After registration, `Init` is called with deployment parameters:

```ada
Init (Init_Params, Initial_State, Success);
```

## Gas Metering

CVMs optionally support gas metering:

| Operation | Gas Cost |
|-----------|----------|
| State read | 200 |
| State write | 5000 |
| CVM call | 700 + subcall gas |
| SHA3-256 | 30 + 6 per word |
| Parameter access | 3 per byte |

Gas limits are checked in the dispatch layer, not individual CVMs.

## Error Handling

### Status Codes

| Status | Meaning | Recovery |
|--------|---------|----------|
| `Success` | Completed | None |
| `Invalid_Caller` | Not authorized | Check permissions |
| `Invalid_Params` | Bad parameters | Fix input |
| `Invalid_State` | State constraint violated | Check preconditions |
| `Out_Of_Gas` | Resource exhausted | Increase gas limit |
| `Proof_Failed` | Assertion failed | Bug in CVM |
| `Unknown_Method` | Entry point not found | Check selector |
| `Internal_Error` | Unexpected error | Report issue |

### Error Results

```ada
Result := Error_Result (Invalid_Params);
```

Return data is zeroed on error.

## Testing

### Local Execution

```bash
./core/bin/khepri_local_main \
   --contract my_token.cvm \
   --method Transfer \
   --params hex:a1b2c3...
```

### Unit Tests

```ada
with AUnit; use AUnit;
with CVM_Types; use CVM_Types;
with My_Token;

procedure Test_Transfer is
   Context : Call_Context := (
      Caller      => Test_Address_1,
      Target      => Token_Address,
      Entry_Point => Transfer_Selector,
      Param_Len   => 40,
      Params      => Test_Params,
      Height      => 100,
      Internal    => False
   );
   State  : State_Array := Initial_State;
   Result : Exec_Result;
begin
   My_Token.Transfer (Context, State, Result);
   Assert (Result.Status = Success, "Transfer should succeed");
end Test_Transfer;
```

## Reference Implementation

See `core/src/contracts/ats20_token.adb` for a complete token contract implementation.
