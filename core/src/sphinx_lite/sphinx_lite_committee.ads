--  SPHINX Lite Committee: Checkpoint Committee Management
--
--  Manages the 21-member checkpoint committee for ultra-light clients.
--  Implements committee rotation, member validation, and threshold logic.
--
--  Key Features:
--  - 21-member BFT committee
--  - Committee rotation every epoch
--  - Slashing conditions for misbehavior
--  - Threshold signature aggregation
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 8.2: Committee Protocol
--  - SCARAB v2.0 Immortal Edition

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Sphinx_Lite_Types; use Sphinx_Lite_Types;

package Sphinx_Lite_Committee with
   SPARK_Mode => On,
   Abstract_State => Committee_State,
   Initializes => Committee_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Epoch length (in blocks)
   Epoch_Length : constant := 1000;

   --  Minimum stake for committee membership
   Min_Stake : constant := 100_000;

   ---------------------------------------------------------------------------
   --  Committee Management
   ---------------------------------------------------------------------------

   --  Initialize committee with genesis members
   procedure Initialize (
      Genesis_Members : in Committee_Array;
      Chain_ID        : in Unsigned_32
   ) with
      Global => (Output => Committee_State);

   --  Check if committee is initialized
   function Is_Initialized return Boolean with
      Global => Committee_State;

   --  Get current committee size (returns constant)
   function Get_Committee_Size return Natural with
      Global => null,
      Post   => Get_Committee_Size'Result = Committee_Size;

   --  Get active member count
   function Get_Active_Count return Natural with
      Global => Committee_State;

   ---------------------------------------------------------------------------
   --  Member Operations
   ---------------------------------------------------------------------------

   --  Get member by index
   procedure Get_Member (
      Index  : in  Committee_Index;
      Member : out Committee_Member;
      Found  : out Boolean
   ) with
      Global => Committee_State;

   --  Get member by address
   procedure Find_Member (
      Address : in  Contract_Address;
      Index   : out Committee_Index;
      Found   : out Boolean
   ) with
      Global => Committee_State;

   --  Validate member is active and authorized
   function Is_Valid_Member (Index : Committee_Index) return Boolean with
      Global => Committee_State;

   --  Check if address is a committee member
   function Is_Member (Address : Contract_Address) return Boolean with
      Global => Committee_State;

   ---------------------------------------------------------------------------
   --  Committee Rotation
   ---------------------------------------------------------------------------

   --  Get current epoch number
   function Get_Current_Epoch (Block_Number : Unsigned_64) return Unsigned_64 with
      Global => null,
      Post   => Get_Current_Epoch'Result = Block_Number / Epoch_Length;

   --  Rotate committee at epoch boundary
   procedure Rotate_Committee (
      New_Members : in  Committee_Array;
      Epoch       : in  Unsigned_64;
      Success     : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) with
      Global => (In_Out => Committee_State);

   --  Get committee root hash (for verification)
   function Get_Committee_Root return Hash256 with
      Global => Committee_State;

   ---------------------------------------------------------------------------
   --  Signature Verification
   ---------------------------------------------------------------------------

   --  Verify member signature on checkpoint
   procedure Verify_Member_Signature (
      Index       : in  Committee_Index;
      Checkpoint  : in  Sphinx_Lite_Types.Checkpoint;
      Signature   : in  Hash512;
      Valid       : out Boolean
   ) with
      Global => (In_Out => Committee_State),
      Pre    => Index < Committee_Size;

   --  Aggregate signatures and check threshold
   procedure Verify_Threshold (
      Checkpoint  : in  Sphinx_Lite_Types.Checkpoint;
      Signatures  : in  Signature_Array;
      Result      : out Verification_Result
   ) with
      Global => (In_Out => Committee_State);

   ---------------------------------------------------------------------------
   --  Slashing
   ---------------------------------------------------------------------------

   type Slashing_Reason is (
      Slash_None,
      Slash_Double_Sign,        --  Signed conflicting checkpoints
      Slash_Invalid_Signature,  --  Submitted invalid signature
      Slash_Offline,            --  Failed to sign for extended period
      Slash_Equivocation        --  General equivocation
   );

   --  Slash a committee member
   procedure Slash_Member (
      Index   : in  Committee_Index;
      Reason  : in  Slashing_Reason;
      Success : out Boolean
   ) with
      Global => (In_Out => Committee_State),
      Pre    => Index < Committee_Size;

   --  Check if member is slashed
   function Is_Slashed (Index : Committee_Index) return Boolean with
      Global => Committee_State;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   --  Get total signed checkpoints by member
   function Get_Sign_Count (Index : Committee_Index) return Unsigned_64 with
      Global => Committee_State;

   --  Get last signed block by member
   function Get_Last_Signed (Index : Committee_Index) return Unsigned_64 with
      Global => Committee_State;

end Sphinx_Lite_Committee;
