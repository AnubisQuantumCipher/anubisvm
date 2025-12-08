--  KHEPRI Merkle Patricia Trie Types
--
--  Type definitions for the Merkle Patricia Trie implementation.
--  Used for state management, storage, and proof generation.
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Interfaces;      use Interfaces;
with Aegis_VM_Types;  use Aegis_VM_Types;

package Khepri_MPT_Types with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Nibble (4-bit) constants
   Nibbles_Per_Byte : constant := 2;
   Max_Key_Bytes    : constant := 32;  --  256-bit keys
   Max_Key_Nibbles  : constant := Max_Key_Bytes * Nibbles_Per_Byte;

   --  Node type constants
   Max_Children     : constant := 16;  --  Hexadecimal branch
   Max_Value_Size   : constant := 32;  --  Hash or small value

   --  Trie depth limits
   Max_Trie_Depth   : constant := 64;  --  Maximum path length

   ---------------------------------------------------------------------------
   --  Nibble Types
   ---------------------------------------------------------------------------

   --  A nibble is 4 bits (0..15)
   subtype Nibble is Unsigned_8 range 0 .. 15;

   --  Nibble path (key broken into nibbles)
   subtype Nibble_Index is Natural range 0 .. Max_Key_Nibbles - 1;
   type Nibble_Path is array (Nibble_Index) of Nibble;

   --  Path with length tracking
   type Nibble_Key is record
      Data   : Nibble_Path;
      Length : Natural;
   end record;

   Empty_Nibble_Key : constant Nibble_Key := (
      Data   => (others => 0),
      Length => 0
   );

   ---------------------------------------------------------------------------
   --  Hash Types
   ---------------------------------------------------------------------------

   --  32-byte hash (Keccak-256)
   subtype Hash_Index is Natural range 0 .. 31;
   type Hash_256 is array (Hash_Index) of Unsigned_8;

   Empty_Hash : constant Hash_256 := (others => 0);

   --  RLP-encoded node hash
   type Node_Hash is record
      Data  : Hash_256;
      Valid : Boolean;
   end record;

   Null_Node_Hash : constant Node_Hash := (
      Data  => Empty_Hash,
      Valid => False
   );

   ---------------------------------------------------------------------------
   --  Node Types
   ---------------------------------------------------------------------------

   --  MPT node types per Ethereum Yellow Paper
   type Node_Kind is (
      Node_Empty,      --  Null/empty node
      Node_Leaf,       --  Leaf node (key-end + value)
      Node_Extension,  --  Extension node (shared prefix)
      Node_Branch      --  Branch node (16 children + value)
   );

   --  Value stored in leaf/branch nodes
   subtype Value_Index is Natural range 0 .. Max_Value_Size - 1;
   type Node_Value is array (Value_Index) of Unsigned_8;

   type Value_Data is record
      Bytes  : Node_Value;
      Length : Natural;
   end record;

   Empty_Value : constant Value_Data := (
      Bytes  => (others => 0),
      Length => 0
   );

   ---------------------------------------------------------------------------
   --  Branch Node Children
   ---------------------------------------------------------------------------

   --  Child reference (hash of child node)
   subtype Child_Index is Natural range 0 .. Max_Children - 1;
   type Child_Array is array (Child_Index) of Node_Hash;

   Empty_Children : constant Child_Array := (others => Null_Node_Hash);

   ---------------------------------------------------------------------------
   --  Node Structure
   ---------------------------------------------------------------------------

   --  Complete MPT node
   type MPT_Node is record
      Kind     : Node_Kind;
      Key      : Nibble_Key;      --  For leaf/extension: remaining path
      Value    : Value_Data;      --  For leaf/branch: stored value
      Children : Child_Array;     --  For branch: 16 child hashes
      Hash     : Node_Hash;       --  Cached hash of this node
   end record;

   Empty_Node : constant MPT_Node := (
      Kind     => Node_Empty,
      Key      => Empty_Nibble_Key,
      Value    => Empty_Value,
      Children => Empty_Children,
      Hash     => Null_Node_Hash
   );

   ---------------------------------------------------------------------------
   --  Proof Types
   ---------------------------------------------------------------------------

   --  Proof node (RLP-encoded node on path)
   Max_Proof_Node_Size : constant := 532;  --  Max RLP size for branch
   subtype Proof_Node_Index is Natural range 0 .. Max_Proof_Node_Size - 1;
   type Proof_Node_Data is array (Proof_Node_Index) of Unsigned_8;

   type Proof_Node is record
      Data   : Proof_Node_Data;
      Length : Natural;
   end record;

   --  Merkle proof (list of nodes from root to target)
   subtype Proof_Depth is Natural range 0 .. Max_Trie_Depth - 1;
   type Proof_Path is array (Proof_Depth) of Proof_Node;

   type Merkle_Proof is record
      Nodes  : Proof_Path;
      Depth  : Natural;
      Key    : Nibble_Key;
      Value  : Value_Data;
      Exists : Boolean;  --  True if key exists, False for non-existence proof
   end record;

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type MPT_Error is (
      Error_None,
      Error_Key_Not_Found,
      Error_Invalid_Node,
      Error_Invalid_Proof,
      Error_Hash_Mismatch,
      Error_Trie_Full,
      Error_Invalid_RLP,
      Error_Overflow
   );

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Convert byte array to nibble path
   function Bytes_To_Nibbles (
      Data   : Byte_Array;
      Length : Natural
   ) return Nibble_Key with
      Pre => Length <= Max_Key_Bytes
             and then Data'First = 0
             and then Data'Last < Natural'Last
             and then Data'Length >= Length;

   --  Compare nibble keys
   function Keys_Equal (A, B : Nibble_Key) return Boolean with
      Pre => A.Length <= Max_Key_Nibbles and B.Length <= Max_Key_Nibbles;

   --  Get common prefix length
   function Common_Prefix_Length (A, B : Nibble_Key) return Natural with
      Pre => A.Length <= Max_Key_Nibbles and B.Length <= Max_Key_Nibbles;

   --  Check if key A is prefix of key B
   function Is_Prefix (Prefix, Key : Nibble_Key) return Boolean with
      Pre => Prefix.Length <= Max_Key_Nibbles and Key.Length <= Max_Key_Nibbles;

   --  Remove prefix from key
   function Remove_Prefix (
      Key    : Nibble_Key;
      Length : Natural
   ) return Nibble_Key with
      Pre => Length <= Key.Length
             and Key.Length <= Max_Key_Nibbles;

   --  Compare hashes
   function Hash_Equal (A, B : Hash_256) return Boolean;

end Khepri_MPT_Types;
