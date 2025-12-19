pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;

--  MultiSig Wallet: M-of-N Multi-Signature Wallet with ML-DSA-87
--
--  A secure multi-signature wallet requiring M signatures from N owners
--  to execute transactions. Uses post-quantum ML-DSA-87 signatures.
--
--  FEATURES:
--  - M-of-N signature threshold (configurable, e.g., 3-of-5)
--  - Transaction proposal and approval workflow
--  - ML-DSA-87 post-quantum signatures for all approvals
--  - Owner management (add/remove owners via multisig)
--  - Daily spending limits
--  - Transaction history and audit log
--  - Emergency freeze/unfreeze
--
--  STATE LAYOUT (256 slots):
--  [0]     : Configuration (M, N, frozen flag)
--  [1]     : Total transaction count
--  [2]     : Pending transaction count
--  [3]     : Daily spent amount
--  [4]     : Last reset timestamp
--  [5]     : Daily limit
--  [6-7]   : Reserved
--  [8-23]  : Owner addresses (16 max owners)
--  [24-39] : Owner public key hashes (for ML-DSA-87)
--  [40-103]: Pending transactions (64 slots)
--  [104-167]: Transaction history (64 slots)
--  [168-231]: Approval signatures (64 slots)
--  [232-255]: Audit log

package MultiSig_Wallet with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_Owners              : constant := 16;
   Max_Pending_Txs         : constant := 64;
   Max_History_Entries     : constant := 64;
   Max_Audit_Entries       : constant := 24;

   --  State slot assignments
   Config_Slot             : constant State_Index := 0;
   Tx_Count_Slot           : constant State_Index := 1;
   Pending_Count_Slot      : constant State_Index := 2;
   Daily_Spent_Slot        : constant State_Index := 3;
   Last_Reset_Slot         : constant State_Index := 4;
   Daily_Limit_Slot        : constant State_Index := 5;

   Owner_Slots_Base        : constant State_Index := 8;
   PubKey_Slots_Base       : constant State_Index := 24;
   Pending_Tx_Slots_Base   : constant State_Index := 40;
   History_Slots_Base      : constant State_Index := 104;
   Approval_Slots_Base     : constant State_Index := 168;
   Audit_Slots_Base        : constant State_Index := 232;

   --  Method selectors
   Propose_Transaction_Selector : constant Hash256 := (16#a0#, 16#b1#, 16#c2#, 16#d3#, others => 0);
   Approve_Transaction_Selector : constant Hash256 := (16#a1#, 16#b2#, 16#c3#, 16#d4#, others => 0);
   Execute_Transaction_Selector : constant Hash256 := (16#a2#, 16#b3#, 16#c4#, 16#d5#, others => 0);
   Revoke_Approval_Selector     : constant Hash256 := (16#a3#, 16#b4#, 16#c5#, 16#d6#, others => 0);
   Add_Owner_Selector           : constant Hash256 := (16#a4#, 16#b5#, 16#c6#, 16#d7#, others => 0);
   Remove_Owner_Selector        : constant Hash256 := (16#a5#, 16#b6#, 16#c7#, 16#d8#, others => 0);
   Change_Threshold_Selector    : constant Hash256 := (16#a6#, 16#b7#, 16#c8#, 16#d9#, others => 0);
   Set_Daily_Limit_Selector     : constant Hash256 := (16#a7#, 16#b8#, 16#c9#, 16#da#, others => 0);
   Freeze_Selector              : constant Hash256 := (16#a8#, 16#b9#, 16#ca#, 16#db#, others => 0);
   Unfreeze_Selector            : constant Hash256 := (16#a9#, 16#ba#, 16#cb#, 16#dc#, others => 0);
   Get_Transaction_Selector     : constant Hash256 := (16#aa#, 16#bb#, 16#cc#, 16#dd#, others => 0);
   Get_Owners_Selector          : constant Hash256 := (16#ab#, 16#bc#, 16#cd#, 16#de#, others => 0);

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Transaction status
   type Tx_Status is (Pending, Executed, Revoked);

   --  Transaction record
   type Transaction is record
      To              : Address;
      Amount          : Unsigned_64;
      Data_Hash       : Hash256;
      Approvals       : Unsigned_8;      -- Count of approvals
      Required        : Unsigned_8;      -- Required approvals
      Executed        : Boolean;
      Revoked         : Boolean;
      Proposer        : Address;
      Propose_Time    : Unsigned_64;
      Execute_Time    : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Core Operations
   ---------------------------------------------------------------------------

   --  Propose a new transaction
   --  Params: to (32) + amount (8) + data_len (2) + data (variable)
   procedure Propose_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Approve a pending transaction with ML-DSA-87 signature
   --  Params: tx_id (1) + signature (4627 bytes)
   procedure Approve_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Execute a fully approved transaction
   --  Params: tx_id (1 byte)
   procedure Execute_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Revoke approval from a transaction
   --  Params: tx_id (1 byte)
   procedure Revoke_Approval (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  Owner Management (Requires M-of-N approval)
   ---------------------------------------------------------------------------

   --  Add a new owner (requires multisig approval)
   --  Params: new_owner (32) + pubkey_hash (32)
   procedure Add_Owner (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Remove an owner (requires multisig approval)
   --  Params: owner_to_remove (32 bytes)
   procedure Remove_Owner (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Change signature threshold (requires multisig approval)
   --  Params: new_m (1 byte)
   procedure Change_Threshold (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------

   --  Set daily spending limit
   --  Params: new_limit (8 bytes)
   procedure Set_Daily_Limit (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Freeze all operations (emergency)
   procedure Freeze (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Unfreeze operations
   procedure Unfreeze (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  Query Operations
   ---------------------------------------------------------------------------

   --  Get transaction details
   --  Params: tx_id (1 byte)
   procedure Get_Transaction (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => State, State => State, null => Context),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get list of owners
   procedure Get_Owners (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => State, State => State, null => Context),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor with
      Global => null;

   --  Initialize multisig wallet
   --  Params: M (1) + N (1) + daily_limit (8) + owner1 (32) + ... + ownerN (32)
   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) with
      Global  => null,
      Depends => ((State, Success) => Init_Params);

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => ((Result, State) => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

private

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Check if address is an owner
   function Is_Owner (
      State : State_Array;
      Addr  : Address
   ) return Boolean with
      Global => null;

   --  Get owner count
   function Get_Owner_Count (State : State_Array) return Natural with
      Global => null,
      Post   => Get_Owner_Count'Result <= Max_Owners;

   --  Verify ML-DSA-87 signature
   function Verify_Signature (
      Message   : Byte_Array;
      Signature : Byte_Array;
      PubKey    : Address
   ) return Boolean with
      Global => null;

   --  Check if wallet is frozen
   function Is_Frozen (State : State_Array) return Boolean with
      Global => null;

   --  Check and update daily limit
   function Check_Daily_Limit (
      State        : in Out State_Array;
      Amount       : Unsigned_64;
      Current_Time : Unsigned_64
   ) return Boolean with
      Global => null;

   --  Read U64 from state slot
   function Read_U64 (Slot : State_Slot) return Unsigned_64 with
      Global => null;

   --  Write U64 to state slot
   procedure Write_U64 (
      Slot  : in Out State_Slot;
      Value : Unsigned_64
   ) with
      Global => null;

   --  Match method selectors
   function Match_Selector (A, B : Hash256) return Boolean with
      Global => null;

end MultiSig_Wallet;
