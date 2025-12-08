--  KHEPRI Merkle Patricia Trie
--
--  Core Merkle Patricia Trie implementation for KHEPRI state management.
--  Implements the Modified Merkle Patricia Trie as specified in Ethereum.
--
--  Features:
--    - Insert, Get, Delete operations
--    - State root computation
--    - Merkle proof generation and verification
--    - RLP encoding/decoding
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Interfaces;         use Interfaces;
with Aegis_VM_Types;     use Aegis_VM_Types;
with Khepri_MPT_Types;   use Khepri_MPT_Types;

package Khepri_MPT with
   SPARK_Mode => On,
   Abstract_State => (Trie_State with External => Async_Writers),
   Initializes => Trie_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum nodes in trie (memory limit)
   Max_Nodes : constant := 1_000_000;

   --  RLP constants
   RLP_Short_String : constant := 16#80#;
   RLP_Long_String  : constant := 16#B7#;
   RLP_Short_List   : constant := 16#C0#;
   RLP_Long_List    : constant := 16#F7#;

   ---------------------------------------------------------------------------
   --  Trie Handle
   ---------------------------------------------------------------------------

   --  Opaque trie reference
   type Trie_ID is new Natural range 0 .. 255;
   Null_Trie : constant Trie_ID := 0;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Create a new empty trie
   procedure Create_Trie (
      Trie    : out Trie_ID;
      Success : out Boolean
   ) with
      Global => (In_Out => Trie_State);

   --  Create trie from existing root
   procedure Load_Trie (
      Root    : in     Hash_256;
      Trie    : out    Trie_ID;
      Success : out    Boolean
   ) with
      Global => (In_Out => Trie_State);

   --  Destroy a trie
   procedure Destroy_Trie (
      Trie : in Trie_ID
   ) with
      Global => (In_Out => Trie_State);

   ---------------------------------------------------------------------------
   --  Core Operations
   ---------------------------------------------------------------------------

   --  Insert or update a key-value pair
   procedure Put (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Value   : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) with
      Global => (In_Out => Trie_State),
      Pre    => Key'Length <= Max_Key_Bytes
                and Value'Length <= Max_Value_Size;

   --  Get value for a key
   procedure Get (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Value   : out    Value_Data;
      Found   : out    Boolean;
      Error   : out    MPT_Error
   ) with
      Global => Trie_State,
      Pre    => Key'Length <= Max_Key_Bytes;

   --  Delete a key
   procedure Delete (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) with
      Global => (In_Out => Trie_State),
      Pre    => Key'Length <= Max_Key_Bytes;

   --  Check if key exists
   function Contains (
      Trie : Trie_ID;
      Key  : Byte_Array
   ) return Boolean with
      Global => Trie_State,
      Volatile_Function,
      Pre    => Key'Length <= Max_Key_Bytes;

   ---------------------------------------------------------------------------
   --  State Root
   ---------------------------------------------------------------------------

   --  Compute the current state root
   function Root_Hash (Trie : Trie_ID) return Hash_256 with
      Global => Trie_State,
      Volatile_Function;

   --  Check if trie is empty
   function Is_Empty (Trie : Trie_ID) return Boolean with
      Global => Trie_State,
      Volatile_Function;

   --  Get node count
   function Node_Count (Trie : Trie_ID) return Natural with
      Global => Trie_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Merkle Proofs
   ---------------------------------------------------------------------------

   --  Generate inclusion/exclusion proof for a key
   procedure Generate_Proof (
      Trie    : in     Trie_ID;
      Key     : in     Byte_Array;
      Proof   : out    Merkle_Proof;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) with
      Global => Trie_State,
      Pre    => Key'Length <= Max_Key_Bytes;

   --  Verify a merkle proof against a root
   procedure Verify_Proof (
      Root    : in     Hash_256;
      Proof   : in     Merkle_Proof;
      Valid   : out    Boolean;
      Error   : out    MPT_Error
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Snapshots
   ---------------------------------------------------------------------------

   --  Create a snapshot of current state
   procedure Create_Snapshot (
      Trie        : in     Trie_ID;
      Snapshot_ID : out    Natural;
      Success     : out    Boolean
   ) with
      Global => (In_Out => Trie_State);

   --  Restore trie to a snapshot
   procedure Restore_Snapshot (
      Trie        : in     Trie_ID;
      Snapshot_ID : in     Natural;
      Success     : out    Boolean;
      Error       : out    MPT_Error
   ) with
      Global => (In_Out => Trie_State);

   --  Discard a snapshot
   procedure Discard_Snapshot (
      Trie        : in     Trie_ID;
      Snapshot_ID : in     Natural
   ) with
      Global => (In_Out => Trie_State);

   ---------------------------------------------------------------------------
   --  RLP Encoding
   ---------------------------------------------------------------------------

   --  Encode a node to RLP
   procedure Encode_Node (
      Node    : in     MPT_Node;
      Output  : out    Byte_Array;
      Length  : out    Natural;
      Success : out    Boolean
   ) with
      Global => null,
      Pre    => Output'Length >= Max_Proof_Node_Size;

   --  Decode RLP to a node
   procedure Decode_Node (
      Input   : in     Byte_Array;
      Node    : out    MPT_Node;
      Success : out    Boolean;
      Error   : out    MPT_Error
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Hashing
   ---------------------------------------------------------------------------

   --  Compute Keccak-256 hash of data
   function Keccak_256 (Data : Byte_Array) return Hash_256 with
      Global => null;

   --  Compute hash of an MPT node
   function Hash_Node (Node : MPT_Node) return Hash_256 with
      Global => null;

   ---------------------------------------------------------------------------
   --  Iteration
   ---------------------------------------------------------------------------

   --  Iterator state
   type Iterator is private;

   --  Begin iteration
   procedure Begin_Iteration (
      Trie : in     Trie_ID;
      Iter : out    Iterator
   ) with
      Global => Trie_State;

   --  Get next key-value pair
   procedure Next (
      Iter    : in out Iterator;
      Key     : out    Nibble_Key;
      Value   : out    Value_Data;
      Done    : out    Boolean
   ) with
      Global => Trie_State;

private

   ---------------------------------------------------------------------------
   --  Iterator Implementation
   ---------------------------------------------------------------------------

   type Path_Entry is record
      Node_Idx : Natural;
      Child    : Natural;
   end record;

   type Path_Stack is array (0 .. Max_Trie_Depth - 1) of Path_Entry;

   type Iterator is record
      Trie      : Trie_ID;
      Stack     : Path_Stack;
      Depth     : Natural;
      Current   : Nibble_Key;
      Exhausted : Boolean;
   end record;

end Khepri_MPT;
