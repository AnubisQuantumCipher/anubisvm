pragma SPARK_Mode (On);

with Interfaces;       use Interfaces;
with Aegis_VM_Types;   use Aegis_VM_Types;
with Aegis_Privacy;    use Aegis_Privacy;
with Anubis_Whisper;   use Anubis_Whisper;

package body Private_Auction with
   SPARK_Mode => On,
   Refined_State => (Auction_State =>
      (Info, Bids, Privacy_Gas_Used, Stark_Verifications))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   type Bid_Array is array (Natural range 0 .. Max_Bidders - 1) of Private_Bid;

   Info               : Auction_Info := (
      Status      => Auction_Uninitialized,
      Owner       => (others => 0),
      Start_Block => 0,
      End_Block   => 0,
      Winner      => (others => 0),
      Winning_Bid => 0
   );

   Bids               : Bid_Array := (others => (
      Encrypted_Amount => (others => 0),
      Commitment       => (others => 0),
      Range_Proof      => (others => 0),
      Bidder           => (others => 0),
      Timestamp        => 0,
      Revealed         => False,
      Valid            => False
   ));

   Privacy_Gas_Used   : Aegis_Privacy.VM_Gas_Amount := 0;
   Stark_Verifications: Natural := 0;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   function Find_Bid_Index (Bidder : Address) return Natural is
   begin
      for I in Bids'Range loop
         if Bids (I).Valid
           and then Bids (I).Bidder = Bidder
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Bid_Index;

   function Count_Bids return Natural is
      Count : Natural := 0;
   begin
      for I in Bids'Range loop
         if Bids (I).Valid then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Bids;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Owner    : in Address;
      Duration : in Natural;
      Success  : out Boolean;
      Error    : out Error_Code
   ) is
   begin
      if Info.Status /= Auction_Uninitialized then
         Success := False;
         Error   := Error_Already_Initialized;
         return;
      end if;

      if Duration = 0 then
         Success := False;
         Error   := Error_Invalid_Params;
         return;
      end if;

      Info.Status      := Auction_Open;
      Info.Owner       := Owner;
      Info.Start_Block := 0;
      Info.End_Block   := Duration;
      Info.Winner      := (others => 0);
      Info.Winning_Bid := 0;

      for I in Bids'Range loop
         Bids (I).Encrypted_Amount := (others => 0);
         Bids (I).Commitment       := (others => 0);
         Bids (I).Range_Proof      := (others => 0);
         Bids (I).Bidder           := (others => 0);
         Bids (I).Timestamp        := 0;
         Bids (I).Revealed         := False;
         Bids (I).Valid            := False;
      end loop;

      Privacy_Gas_Used    := 0;
      Stark_Verifications := 0;

      Success := True;
      Error   := Error_None;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Bid Submission
   ---------------------------------------------------------------------------

   procedure Submit_Bid (
      Bidder           : in Address;
      Encrypted_Amount : in Encrypted_Amount_Bytes;
      Commitment       : in Commitment_Bytes;
      Range_Proof      : in Range_Proof_Bytes;
      Block_Number     : in Natural;
      Success          : out Boolean;
      Error            : out Error_Code
   ) is
      Valid_Range : Boolean := False;
      Priv_Res    : Privacy_Result;
      Slot        : Natural := Natural'Last;
   begin
      if Info.Status = Auction_Uninitialized then
         Success := False;
         Error   := Error_Not_Initialized;
         return;
      end if;

      if Info.Status /= Auction_Open then
         Success := False;
         Error   := Error_Auction_Not_Open;
         return;
      end if;

      if Block_Number < Info.Start_Block then
         Success := False;
         Error   := Error_Too_Early;
         return;
      end if;

      if Block_Number > Info.End_Block then
         Success := False;
         Error   := Error_Too_Late;
         return;
      end if;

      --  Enforce capacity and uniqueness
      if Count_Bids >= Max_Bidders then
         Success := False;
         Error   := Error_Capacity;
         return;
      end if;

      if Find_Bid_Index (Bidder) /= Natural'Last then
         Success := False;
         Error   := Error_Duplicate_Bid;
         return;
      end if;

      --  Verify range proof via Aegis_Privacy
      Aegis_Privacy.Verify_Range_Proof (
         Commitment => Commitment,
         Proof      => Range_Proof,
         Bits       => Range_Bits,
         Valid      => Valid_Range,
         Result     => Priv_Res
      );

      Privacy_Gas_Used :=
        Privacy_Gas_Used + Priv_Res.Gas_Used;

      if Valid_Range then
         Stark_Verifications := Stark_Verifications + 1;
      end if;

      if (not Priv_Res.Success) or else (not Valid_Range) then
         Success := False;
         Error   := Error_Invalid_Proof;
         return;
      end if;

      --  Find a free slot
      for I in Bids'Range loop
         if not Bids (I).Valid then
            Slot := I;
            exit;
         end if;
      end loop;

      if Slot = Natural'Last then
         Success := False;
         Error   := Error_Capacity;
         return;
      end if;

      Bids (Slot).Encrypted_Amount := Encrypted_Amount;
      Bids (Slot).Commitment       := Commitment;
      Bids (Slot).Range_Proof      := Range_Proof;
      Bids (Slot).Bidder           := Bidder;
      Bids (Slot).Timestamp        := Block_Number;
      Bids (Slot).Revealed         := False;
      Bids (Slot).Valid            := True;

      Success := True;
      Error   := Error_None;
   end Submit_Bid;

   ---------------------------------------------------------------------------
   --  Auction Control
   ---------------------------------------------------------------------------

   procedure Close_Auction (
      Caller       : in Address;
      Block_Number : in Natural;
      Success      : out Boolean;
      Error        : out Error_Code
   ) is
   begin
      if Info.Status = Auction_Uninitialized then
         Success := False;
         Error   := Error_Not_Initialized;
         return;
      end if;

      if Info.Status /= Auction_Open then
         Success := False;
         Error   := Error_Auction_Closed;
         return;
      end if;

      if Caller /= Info.Owner then
         Success := False;
         Error   := Error_Not_Owner;
         return;
      end if;

      if Block_Number < Info.End_Block then
         Success := False;
         Error   := Error_Too_Early;
         return;
      end if;

      Info.Status := Auction_Closed;

      Success := True;
      Error   := Error_None;
   end Close_Auction;

   ---------------------------------------------------------------------------
   --  Reveal Phase
   ---------------------------------------------------------------------------

   procedure Reveal_Bid (
      Caller       : in Address;
      Reveal_Key   : in Reveal_Key_Bytes;
      Block_Number : in Natural;
      Success      : out Boolean;
      Error        : out Error_Code
   ) is
      pragma Unreferenced (Block_Number);

      Bid_Idx : Natural := Natural'Last;
      Amount  : Unsigned_64 := 0;
      Factor  : Unsigned_64 := 1;

      Blinding : Blinding_Factor;
      Commit   : Amount_Commitment;
      OK       : Boolean := False;
   begin
      if Info.Status = Auction_Uninitialized then
         Success := False;
         Error   := Error_Not_Initialized;
         return;
      end if;

      if Info.Status /= Auction_Closed then
         Success := False;
         Error   := Error_Auction_Not_Open;
         return;
      end if;

      Bid_Idx := Find_Bid_Index (Caller);
      if Bid_Idx = Natural'Last then
         Success := False;
         Error   := Error_No_Bid;
         return;
      end if;

      if Bids (Bid_Idx).Revealed then
         Success := False;
         Error   := Error_Invalid_Reveal;
         return;
      end if;

      --  Decode little-endian amount from first 8 bytes
      Amount := 0;
      Factor := 1;
      for I in 0 .. 7 loop
         Amount :=
           Amount +
           Factor * Unsigned_64 (Reveal_Key (I));
         Factor := Shift_Left (Factor, 8);
      end loop;

      if Amount < Min_Bid_Amount or else Amount > Max_Bid_Amount then
         Success := False;
         Error   := Error_Out_Of_Range;
         return;
      end if;

      --  Extract blinding factor
      for I in 0 .. 31 loop
         Blinding (I) := Reveal_Key (8 + I);
      end loop;

      --  Copy commitment into Anubis_Whisper type
      for I in Commit'Range loop
         Commit (I) := Bids (Bid_Idx).Commitment (I);
      end loop;

      OK := Verify_Commitment (
        Commitment => Commit,
        Value      => Amount,
        Blinding   => Blinding
      );

      if not OK then
         Success := False;
         Error   := Error_Invalid_Commitment;
         return;
      end if;

      --  Mark bid as revealed and update winner if needed
      Bids (Bid_Idx).Revealed := True;

      if Amount > Info.Winning_Bid then
         Info.Winning_Bid := Amount;
         Info.Winner      := Bids (Bid_Idx).Bidder;
      end if;

      Success := True;
      Error   := Error_None;
   end Reveal_Bid;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Status return Auction_Status is
   begin
      return Info.Status;
   end Get_Status;

   function Get_Num_Bids return Natural is
   begin
      return Count_Bids;
   end Get_Num_Bids;

   function Get_Winner return Address is
   begin
      return Info.Winner;
   end Get_Winner;

   function Get_Winning_Bid return Unsigned_64 is
   begin
      return Info.Winning_Bid;
   end Get_Winning_Bid;

   function Has_Bid (Bidder : Address) return Boolean is
   begin
      return Find_Bid_Index (Bidder) /= Natural'Last;
   end Has_Bid;

   function Get_Privacy_Gas_Used return Aegis_Privacy.VM_Gas_Amount is
   begin
      return Privacy_Gas_Used;
   end Get_Privacy_Gas_Used;

   function Get_STARK_Verifications return Natural is
   begin
      return Stark_Verifications;
   end Get_STARK_Verifications;

end Private_Auction;

