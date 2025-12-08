-------------------------------------------------------------------------------
--  SCARAB - AADKG (Asynchronous Adaptive Distributed Key Generation)
--  Novel DKG protocol that tolerates arbitrary node crashes
--
--  Key innovation: All DKG state is stored on-chain encrypted, enabling
--  crashed nodes to recover autonomously without restarting the protocol.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Scarab_AADKG with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Protocol Parameters
   ---------------------------------------------------------------------------

   --  Committee size (67 validators for 2/3 majority)
   Committee_Size : constant := 67;

   --  Threshold (45 = 2/3 + 1 for BFT security)
   Threshold      : constant := 45;

   --  Maximum parties (for array sizing)
   Max_Parties    : constant := 128;

   --  Timeout in blocks
   Phase_Timeout  : constant := 100;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Party_Index is Natural range 0 .. Max_Parties - 1;
   subtype Threshold_Range is Natural range 0 .. Threshold - 1;

   --  AADKG phases
   type AADKG_Phase is (
      Phase_0_Commitment,     -- Commit to randomness for committee selection
      Phase_1_Distribution,   -- Post encrypted shares to chain
      Phase_2_Verification,   -- Verify commitments
      Phase_3_Aggregation,    -- Compute final shares
      Phase_4_Recovery,       -- Recovery mode (for crashed nodes)
      Complete,
      Failed
   );

   --  Polynomial over field (for secret sharing)
   type Polynomial_Coeffs is array (Threshold_Range) of Byte_Array (0 .. 31);

   --  Encrypted share (ML-KEM encrypted)
   type Encrypted_Share is record
      Sender         : Party_Index;
      Recipient      : Party_Index;
      Ciphertext     : Byte_Array (0 .. 1567);  -- ML-KEM-1024 ciphertext
      CT_Length      : Natural;
      Signature      : Byte_Array (0 .. 4626);  -- ML-DSA-87 signature
   end record;

   --  Commitment element type
   type Commit_Array is array (Threshold_Range) of Byte_Array (0 .. 63);

   --  Polynomial commitment (Pedersen-style)
   type Poly_Commitment is record
      Party          : Party_Index;
      Commits        : Commit_Array;
      Signature      : Byte_Array (0 .. 4626);  -- Proves authenticity
   end record;

   --  Share registry (on-chain)
   type Share_Registry is array (Party_Index, Party_Index) of Encrypted_Share;

   --  Commitment registry (on-chain)
   type Commitment_Registry is array (Party_Index) of Poly_Commitment;

   --  Committee member
   type Committee_Member is record
      Address        : Byte_Array (0 .. 31);
      KEM_PK         : Byte_Array (0 .. 1567);  -- ML-KEM-1024 public key
      DSA_PK         : Byte_Array (0 .. 2591);  -- ML-DSA-87 public key
      Stake          : Unsigned_64;
      Index          : Party_Index;
   end record;

   --  Committee type
   type Committee_Type is array (Party_Index range 0 .. Committee_Size - 1)
      of Committee_Member;

   --  Key share (result of DKG)
   type Key_Share is record
      Index          : Party_Index;
      Share_Value    : Byte_Array (0 .. 31);
      Verification   : Byte_Array (0 .. 63);  -- For share verification
   end record;

   --  Threshold public key
   type Threshold_PK is record
      Combined       : Byte_Array (0 .. 63);
      Participants   : Natural;
      Threshold      : Natural;
   end record;

   --  Array types for procedure parameters
   type Reveal_Array is array (Natural range <>) of Byte_Array (0 .. 31);
   type Stake_Array is array (Natural range <>) of Unsigned_64;
   type Member_Array is array (Natural range <>) of Committee_Member;
   type PK_Array is array (Party_Index) of Byte_Array (0 .. 1567);
   type Share_Array is array (Party_Index) of Encrypted_Share;
   type Partial_Sig_Array is array (Natural range <>) of Byte_Array (0 .. 63);
   type Index_Array is array (Natural range <>) of Party_Index;

   --  Full AADKG state (stored on-chain)
   type AADKG_State is record
      Phase          : AADKG_Phase;
      Committee      : Committee_Type;
      Shares         : Share_Registry;
      Commitments    : Commitment_Registry;
      Participants   : Natural;
      Start_Block    : Unsigned_64;
      Current_Block  : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Phase 0: Commitment Lottery
   ---------------------------------------------------------------------------

   --  Generate commitment for committee selection
   procedure Generate_Commitment (
      My_Random      : Byte_Array;
      My_Stake       : Unsigned_64;
      Commitment     : out Byte_Array;
      Reveal_Data    : out Byte_Array
   ) with
      Global => null,
      Pre => My_Random'Length = 32
             and Commitment'Length = 32
             and Reveal_Data'Length = 32;

   --  Verify commitment reveal
   function Verify_Reveal (
      Commitment     : Byte_Array;
      Reveal         : Byte_Array;
      Stake          : Unsigned_64
   ) return Boolean with
      Global => null,
      Pre => Commitment'Length = 32 and Reveal'Length = 32;

   --  Select committee from revealed randomness (VRF-based)
   procedure Select_Committee (
      All_Reveals    : Reveal_Array;
      All_Stakes     : Stake_Array;
      All_Keys       : Member_Array;
      Committee      : out Committee_Type;
      Selection_Seed : out Byte_Array
   ) with
      Global => null,
      Pre => All_Reveals'Length = All_Stakes'Length
             and Selection_Seed'Length = 32;

   ---------------------------------------------------------------------------
   --  Phase 1: Encrypted Share Distribution
   ---------------------------------------------------------------------------

   --  Generate polynomial and encrypt shares
   procedure Generate_And_Encrypt_Shares (
      My_Index       : Party_Index;
      My_Randomness  : Byte_Array;
      Recipient_PKs  : PK_Array;
      Encrypted      : out Share_Array;
      My_Commitment  : out Poly_Commitment;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => My_Randomness'Length = 64;

   --  Post share to chain (creates transaction)
   procedure Post_Share (
      Share          : Encrypted_Share;
      Tx_Data        : out Byte_Array;
      Tx_Length      : out Natural
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Phase 2: Verification
   ---------------------------------------------------------------------------

   --  Verify encrypted share matches commitment (without decrypting)
   function Verify_Share_Commitment (
      Share          : Encrypted_Share;
      Commitment     : Poly_Commitment
   ) return Boolean with
      Global => null;

   --  Generate fraud proof for invalid share
   procedure Generate_Fraud_Proof (
      Share          : Encrypted_Share;
      Commitment     : Poly_Commitment;
      Decrypted      : Byte_Array;
      Fraud_Proof    : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Phase 3: Aggregation
   ---------------------------------------------------------------------------

   --  Decrypt and aggregate all shares addressed to me
   procedure Aggregate_My_Share (
      My_Index       : Party_Index;
      My_KEM_SK      : Byte_Array;
      All_Shares     : Share_Registry;
      All_Commits    : Commitment_Registry;
      My_Final_Share : out Key_Share;
      Combined_PK    : out Threshold_PK;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => My_KEM_SK'Length = 3168;  -- ML-KEM-1024 secret key

   ---------------------------------------------------------------------------
   --  Phase 4: Crash Recovery (Novel!)
   ---------------------------------------------------------------------------

   --  Recover DKG state after crash
   --  Key innovation: All data is on-chain, so crashed nodes can recover
   procedure Recover_From_Chain (
      My_Index       : Party_Index;
      My_KEM_SK      : Byte_Array;
      Chain_State    : AADKG_State;
      My_Final_Share : out Key_Share;
      Combined_PK    : out Threshold_PK;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Chain_State.Phase >= Phase_3_Aggregation
             and My_KEM_SK'Length = 3168;

   --  Check if I can recover (enough data on chain)
   function Can_Recover (
      My_Index       : Party_Index;
      Chain_State    : AADKG_State
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Threshold Signing (Using DKG Result)
   ---------------------------------------------------------------------------

   --  Generate partial signature
   procedure Sign_Partial (
      Share          : Key_Share;
      Message        : Byte_Array;
      Partial_Sig    : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Partial_Sig'Length = 64;

   --  Combine partial signatures
   procedure Combine_Signatures (
      Partials       : Partial_Sig_Array;
      Indices        : Index_Array;
      Combined_Sig   : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Partials'Length = Indices'Length
             and Natural (Partials'Length) >= Threshold
             and Combined_Sig'Length = 64;

   --  Verify threshold signature
   function Verify_Threshold_Sig (
      PK             : Threshold_PK;
      Message        : Byte_Array;
      Signature      : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Signature'Length = 64;

   ---------------------------------------------------------------------------
   --  State Machine
   ---------------------------------------------------------------------------

   --  Advance DKG state machine
   procedure Advance_Phase (
      State          : in Out AADKG_State;
      Current_Block  : Unsigned_64
   ) with
      Global => null;

   --  Check if DKG is complete
   function Is_Complete (State : AADKG_State) return Boolean with
      Global => null;

   --  Get current phase
   function Get_Phase (State : AADKG_State) return AADKG_Phase with
      Global => null;

   ---------------------------------------------------------------------------
   --  Slashing
   ---------------------------------------------------------------------------

   type Slashing_Reason is (
      Invalid_Share,        -- Share doesn"t match commitment
      Missing_Commitment,   -- Didn"t post commitment in time
      Missing_Share,        -- Didn"t post share in time
      Equivocation         -- Posted conflicting data
   );

   --  Check if party should be slashed
   procedure Check_Slash (
      Party          : Party_Index;
      State          : AADKG_State;
      Should_Slash   : out Boolean;
      Reason         : out Slashing_Reason
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Share (S : in Out Key_Share) with
      Global => null;

   procedure Zeroize_Encrypted (E : in Out Encrypted_Share) with
      Global => null;

end Scarab_AADKG;
