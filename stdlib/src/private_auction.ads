pragma SPARK_Mode (On);

with Aegis_VM_Types;  use Aegis_VM_Types;
with Khepri_Types;
with Aegis_Privacy;
with Interfaces;

--  PRIVATE AUCTION: Sealed-Bid, Post-Quantum Auction
--
--  This contract implements a sealed-bid auction where:
--  - Bids are stored as encrypted ML-KEM ciphertexts (opaque to the contract)
--  - Amounts are committed using Ajtai commitments (Anubis_Whisper)
--  - Bids include lattice-based range proofs verified via Aegis_Privacy
--  - The winner is determined only after bidders reveal their amounts

package Private_Auction with
   SPARK_Mode => On,
   Abstract_State => Auction_State
is

   ---------------------------------------------------------------------------
   --  Basic Types
   ---------------------------------------------------------------------------

   subtype Address is Khepri_Types.Address;

   --  Opaque encrypted bid (e.g., ML-KEM-1024 ciphertext bytes)
   subtype Encrypted_Amount_Bytes is Byte_Array (0 .. 1567);

   --  Ajtai commitment (Anubis_Whisper.Amount_Commitment layout)
   subtype Commitment_Bytes is Byte_Array (0 .. 63);

   --  Lattice range proof (Anubis_Whisper.Range_Proof layout)
   subtype Range_Proof_Bytes is Byte_Array (0 .. 2047);

   --  Reveal key:
   --    - First 8 bytes: little-endian amount (Unsigned_64)
   --    - Next 32 bytes: blinding factor
   subtype Reveal_Key_Bytes is Byte_Array (0 .. 39);

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Max_Bidders  : constant := 32;
   Range_Bits   : constant := 20;

   Min_Bid_Amount : constant Interfaces.Unsigned_64 := 0;
   Max_Bid_Amount : constant Interfaces.Unsigned_64 :=
     Interfaces.Unsigned_64 (2**Range_Bits - 1);

   ---------------------------------------------------------------------------
   --  Error Codes
   ---------------------------------------------------------------------------

   type Error_Code is (
      Error_None,
      Error_Already_Initialized,
      Error_Not_Initialized,
      Error_Invalid_Params,
      Error_Auction_Not_Open,
      Error_Auction_Closed,
      Error_Not_Owner,
      Error_Too_Early,
      Error_Too_Late,
      Error_Capacity,
      Error_Duplicate_Bid,
      Error_Invalid_Proof,
      Error_Invalid_Commitment,
      Error_Invalid_Reveal,
      Error_No_Bid,
      Error_Out_Of_Range,
      Error_Internal
   );

   ---------------------------------------------------------------------------
   --  Auction Types
   ---------------------------------------------------------------------------

   type Auction_Status is (
      Auction_Uninitialized,
      Auction_Open,
      Auction_Closed
   );

   type Auction_Info is record
      Status       : Auction_Status;
      Owner        : Address;
      Start_Block  : Natural;
      End_Block    : Natural;
      Winner       : Address;
      Winning_Bid  : Interfaces.Unsigned_64;
   end record;

   --  Private sealed bid (amount hidden inside encrypted bytes + commitment)
   type Private_Bid is record
      Encrypted_Amount : Encrypted_Amount_Bytes;
      Commitment       : Commitment_Bytes;
      Range_Proof      : Range_Proof_Bytes;
      Bidder           : Address;
      Timestamp        : Natural;
      Revealed         : Boolean;
      Valid            : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Contract State
   ---------------------------------------------------------------------------

   --  Internal state (refined in body)
   --  - Single auction instance
   --  - Fixed-size bidder array
   --  - Privacy gas accounting
   --  - ZK/STARK verification counter

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize a new sealed-bid auction.
   --
   --  Owner    : Address that controls Close_Auction.
   --  Duration : Number of blocks from start (0) until bidding ends.
   --
   --  After initialization:
   --    - Status      = Auction_Open
   --    - Start_Block = 0
   --    - End_Block   = Duration
   procedure Initialize (
      Owner    : in Address;
      Duration : in Natural;
      Success  : out Boolean;
      Error    : out Error_Code
   ) with
      Global => (In_Out => Auction_State);

   ---------------------------------------------------------------------------
   --  Bid Submission
   ---------------------------------------------------------------------------

   --  Submit a sealed bid into the auction.
   --
   --  Bidder          : Address of the bidder.
   --  Encrypted_Amount: Opaque encrypted amount (e.g., ML-KEM ciphertext).
   --  Commitment      : Ajtai commitment to the bid amount.
   --  Range_Proof     : Lattice-based range proof (amount < 2^Range_Bits).
   --  Block_Number    : Current block height (used for window checks).
   --
   --  Enforcement:
   --    - Auction must be open.
   --    - Block_Number in [Start_Block, End_Block].
   --    - At most one bid per Bidder.
   --    - Capacity <= Max_Bidders.
   --    - Range proof verified via Aegis_Privacy.Verify_Range_Proof.
   procedure Submit_Bid (
      Bidder           : in Address;
      Encrypted_Amount : in Encrypted_Amount_Bytes;
      Commitment       : in Commitment_Bytes;
      Range_Proof      : in Range_Proof_Bytes;
      Block_Number     : in Natural;
      Success          : out Boolean;
      Error            : out Error_Code
   ) with
      Global => (In_Out => Auction_State);

   ---------------------------------------------------------------------------
   --  Auction Control
   ---------------------------------------------------------------------------

   --  Close the auction (stop accepting bids).
   --
   --  Only Owner can close.
   --  Requires:
   --    - Status = Auction_Open
   --    - Block_Number >= End_Block
   procedure Close_Auction (
      Caller       : in Address;
      Block_Number : in Natural;
      Success      : out Boolean;
      Error        : out Error_Code
   ) with
      Global => (In_Out => Auction_State);

   ---------------------------------------------------------------------------
   --  Reveal Phase
   ---------------------------------------------------------------------------

   --  Reveal a bid after the auction is closed.
   --
   --  Caller      : Bidder wishing to reveal their own bid.
   --  Reveal_Key  : 8-byte amount (LE) + 32-byte blinding.
   --  Block_Number: Current block height (for consistency checks).
   --
   --  Behavior:
   --    - Only allowed when Status = Auction_Closed.
   --    - Verifies that a bid from Caller exists.
   --    - Parses amount from Reveal_Key and checks Min/Max bounds.
   --    - Uses Anubis_Whisper.Verify_Commitment to confirm that the
   --      stored commitment opens to (amount, blinding).
   --    - Updates Winner and Winning_Bid if this amount is strictly
   --      greater than current Winning_Bid.
   procedure Reveal_Bid (
      Caller       : in Address;
      Reveal_Key   : in Reveal_Key_Bytes;
      Block_Number : in Natural;
      Success      : out Boolean;
      Error        : out Error_Code
   ) with
      Global => (In_Out => Auction_State);

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Status return Auction_Status with
      Global => Auction_State;

   function Get_Num_Bids return Natural with
      Global => Auction_State;

   function Get_Winner return Address with
      Global => Auction_State;

   function Get_Winning_Bid return Interfaces.Unsigned_64 with
      Global => Auction_State;

   function Has_Bid (Bidder : Address) return Boolean with
      Global => Auction_State;

   function Get_Privacy_Gas_Used return Aegis_Privacy.VM_Gas_Amount with
      Global => Auction_State;

   function Get_STARK_Verifications return Natural with
      Global => Auction_State;

end Private_Auction;
