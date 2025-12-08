pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Khepri_Types; use Khepri_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  KHEPRI Events: Contract Event Emission API
--
--  This package provides the interface for contracts to emit events
--  (logs) that are recorded on the blockchain. Events enable:
--  - Off-chain indexing and monitoring
--  - User notification of state changes
--  - Audit trail for contract actions
--
--  Event Model:
--  - Up to 4 indexed topics (32 bytes each)
--  - Variable-length unindexed data
--  - Topic[0] is typically event signature hash
--
--  Gas Costs:
--  - Base: 375 per event
--  - Per topic: 375
--  - Per data byte: 8
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 6: Event System
--  - EVM LOG opcodes (LOG0-LOG4)

package Khepri_Events with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Event Configuration
   ---------------------------------------------------------------------------

   --  Maximum topics (LOG4 equivalent)
   Max_Topics : constant := 4;

   --  Maximum event data size (in bytes)
   Max_Data_Size : constant := 2048;

   --  Event signature type (keccak256 of event declaration)
   subtype Event_Signature is Bytes32;

   ---------------------------------------------------------------------------
   --  Pre-defined Event Signatures
   ---------------------------------------------------------------------------

   --  ERC-20 Transfer: Transfer(address,address,uint256)
   --  keccak256("Transfer(address,address,uint256)")
   Transfer_Sig : constant Event_Signature := (
      16#DD#, 16#F2#, 16#52#, 16#AD#, 16#1B#, 16#E2#, 16#C8#, 16#9B#,
      16#69#, 16#C2#, 16#B0#, 16#68#, 16#FC#, 16#37#, 16#8D#, 16#AA#,
      16#95#, 16#2B#, 16#A7#, 16#F1#, 16#63#, 16#C4#, 16#A1#, 16#16#,
      16#28#, 16#F5#, 16#5A#, 16#4D#, 16#F5#, 16#23#, 16#B3#, 16#EF#
   );

   --  ERC-20 Approval: Approval(address,address,uint256)
   Approval_Sig : constant Event_Signature := (
      16#8C#, 16#5B#, 16#E1#, 16#E5#, 16#EB#, 16#EC#, 16#7D#, 16#5B#,
      16#D1#, 16#4F#, 16#71#, 16#42#, 16#7D#, 16#1E#, 16#84#, 16#F3#,
      16#DD#, 16#03#, 16#14#, 16#C0#, 16#F7#, 16#B2#, 16#29#, 16#1E#,
      16#5B#, 16#20#, 16#0A#, 16#C8#, 16#C7#, 16#C3#, 16#B9#, 16#25#
   );

   --  OwnershipTransferred: OwnershipTransferred(address,address)
   Ownership_Transferred_Sig : constant Event_Signature := (
      16#8B#, 16#E0#, 16#07#, 16#9C#, 16#53#, 16#16#, 16#59#, 16#14#,
      16#13#, 16#44#, 16#CD#, 16#1F#, 16#D0#, 16#A4#, 16#F2#, 16#84#,
      16#19#, 16#49#, 16#7F#, 16#97#, 16#22#, 16#A3#, 16#DA#, 16#AF#,
      16#E3#, 16#B4#, 16#18#, 16#6F#, 16#6B#, 16#64#, 16#57#, 16#E0#
   );

   ---------------------------------------------------------------------------
   --  Basic Event Emission (LOG0-LOG4 equivalents)
   ---------------------------------------------------------------------------

   --  Emit event with no topics (LOG0)
   procedure Emit_Log0 (
      Data       : Byte_Array;
      Data_Size  : Natural
   ) with
      Global => null,
      Pre    => Data_Size <= Max_Data_Size and Data'Length >= Data_Size;

   --  Emit event with 1 topic (LOG1)
   procedure Emit_Log1 (
      Topic0     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) with
      Global => null,
      Pre    => Data_Size <= Max_Data_Size and Data'Length >= Data_Size;

   --  Emit event with 2 topics (LOG2)
   procedure Emit_Log2 (
      Topic0     : Bytes32;
      Topic1     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) with
      Global => null,
      Pre    => Data_Size <= Max_Data_Size and Data'Length >= Data_Size;

   --  Emit event with 3 topics (LOG3)
   procedure Emit_Log3 (
      Topic0     : Bytes32;
      Topic1     : Bytes32;
      Topic2     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) with
      Global => null,
      Pre    => Data_Size <= Max_Data_Size and Data'Length >= Data_Size;

   --  Emit event with 4 topics (LOG4)
   procedure Emit_Log4 (
      Topic0     : Bytes32;
      Topic1     : Bytes32;
      Topic2     : Bytes32;
      Topic3     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) with
      Global => null,
      Pre    => Data_Size <= Max_Data_Size and Data'Length >= Data_Size;

   ---------------------------------------------------------------------------
   --  High-Level Event Helpers (ERC-20 style)
   ---------------------------------------------------------------------------

   --  Emit Transfer event
   --  Transfer(from, to, amount)
   procedure Emit_Transfer (
      From   : Address;
      To     : Address;
      Amount : Uint256
   ) with
      Global => null;

   --  Emit Approval event
   --  Approval(owner, spender, amount)
   procedure Emit_Approval (
      Owner   : Address;
      Spender : Address;
      Amount  : Uint256
   ) with
      Global => null;

   --  Emit OwnershipTransferred event
   procedure Emit_Ownership_Transferred (
      Previous_Owner : Address;
      New_Owner      : Address
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Custom Event Construction
   ---------------------------------------------------------------------------

   --  Event builder type for custom events
   type Event_Builder is private;

   --  Initialize event builder with signature
   function New_Event (Signature : Event_Signature) return Event_Builder with
      Global => null;

   --  Add indexed topic (max 3 additional after signature)
   function Add_Topic (
      Builder : Event_Builder;
      Topic   : Bytes32
   ) return Event_Builder with
      Global => null;

   --  Add indexed address topic
   function Add_Address_Topic (
      Builder : Event_Builder;
      Addr    : Address
   ) return Event_Builder with
      Global => null;

   --  Add indexed U256 topic
   function Add_U256_Topic (
      Builder : Event_Builder;
      Value   : Uint256
   ) return Event_Builder with
      Global => null;

   --  Add unindexed U256 data
   function Add_U256_Data (
      Builder : Event_Builder;
      Value   : Uint256
   ) return Event_Builder with
      Global => null;

   --  Add unindexed address data
   function Add_Address_Data (
      Builder : Event_Builder;
      Addr    : Address
   ) return Event_Builder with
      Global => null;

   --  Add unindexed bytes data
   function Add_Bytes_Data (
      Builder : Event_Builder;
      Data    : Byte_Array
   ) return Event_Builder with
      Global => null,
      Pre    => Data'Length <= Max_Data_Size;

   --  Emit the constructed event
   procedure Emit (Builder : Event_Builder) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Event Signature Calculation
   ---------------------------------------------------------------------------

   --  Calculate event signature from string declaration
   --  e.g., "Transfer(address,address,uint256)"
   function Calculate_Signature (
      Declaration : String
   ) return Event_Signature with
      Global => null,
      Pre    => Declaration'Length <= 256;

   ---------------------------------------------------------------------------
   --  Encoding Helpers
   ---------------------------------------------------------------------------

   --  Encode address as event topic (32 bytes, left-padded)
   function Encode_Address (Addr : Address) return Bytes32 with
      Global => null;

   --  Encode U256 as event topic (32 bytes, big-endian)
   function Encode_U256 (Value : Uint256) return Bytes32 with
      Global => null;

   --  Encode boolean as event topic
   function Encode_Bool (Value : Boolean) return Bytes32 with
      Global => null;

   --  Encode natural as event topic
   function Encode_Natural (Value : Natural) return Bytes32 with
      Global => null;

private

   --  Internal builder state
   type Topic_Array is array (0 .. Max_Topics - 1) of Bytes32;
   type Data_Array is array (0 .. Max_Data_Size - 1) of Byte;

   type Event_Builder is record
      Topics       : Topic_Array;
      Topic_Count  : Natural;
      Data         : Data_Array;
      Data_Size    : Natural;
   end record;

   --  Empty event builder
   Empty_Builder : constant Event_Builder := (
      Topics      => (others => (others => 0)),
      Topic_Count => 0,
      Data        => (others => 0),
      Data_Size   => 0
   );

end Khepri_Events;
