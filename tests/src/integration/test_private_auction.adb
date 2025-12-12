with Ada.Text_IO;          use Ada.Text_IO;
with Interfaces;           use Interfaces;

with Aegis_VM_Types;       use Aegis_VM_Types;
with Khepri_Types;

with Anubis_Whisper;
with Aegis_Privacy;        use Aegis_Privacy;
with Private_Auction;

procedure Test_Private_Auction is

   subtype Address is Khepri_Types.Address;

   Owner  : Address := (others => 0);
   Bidder : Address := (others => 0);

   Bid_Value : constant Unsigned_64 := 42_000;

   Blinding : Anubis_Whisper.Blinding_Factor := (others => 0);
   Commit   : Anubis_Whisper.Amount_Commitment;
   Proof    : Anubis_Whisper.Range_Proof;
   RP_OK    : Boolean;

   Commit_VM : Private_Auction.Commitment_Bytes := (others => 0);
   Proof_VM  : Private_Auction.Range_Proof_Bytes := (others => 0);
   Enc_Bid   : Private_Auction.Encrypted_Amount_Bytes := (others => 0);

   Init_OK : Boolean;
   Init_E  : Private_Auction.Error_Code;

   Sub_OK  : Boolean;
   Sub_E   : Private_Auction.Error_Code;

   Close_OK : Boolean;
   Close_E  : Private_Auction.Error_Code;

   Reveal_OK : Boolean;
   Reveal_E  : Private_Auction.Error_Code;

   Reveal_Key : Private_Auction.Reveal_Key_Bytes := (others => 0);

   Winner      : Address;
   Winning_Bid : Unsigned_64;
   Gas_Used    : Aegis_Privacy.VM_Gas_Amount;
   Stark_Count : Natural;

begin
   Put_Line ("[Private_Auction] Sealed-bid auction test");

   --  Simple deterministic owner/bidder addresses
   Owner (0)  := 1;
   Bidder (0) := 2;

   --  Build blinding factor
   for I in Blinding'Range loop
      Blinding (I) := Unsigned_8 (I);
   end loop;

   --  Create Ajtai commitment and range proof for Bid_Value
   Put_Line ("[Private_Auction] Creating commitment and range proof...");

   Anubis_Whisper.Create_Commitment (
      Value      => Bid_Value,
      Blinding   => Blinding,
      Commitment => Commit
   );

   Anubis_Whisper.Create_Range_Proof (
      Value      => Bid_Value,
      Bits       => Private_Auction.Range_Bits,
      Blinding   => Blinding,
      Commitment => Commit,
      Proof      => Proof,
      Success    => RP_OK
   );

   if not RP_OK then
      Put_Line ("[Private_Auction] Range proof creation failed");
      return;
   end if;

   --  Map commitment and proof into VM byte arrays
   for I in Commit'Range loop
      Commit_VM (I) := Commit (I);
   end loop;

   for I in Proof'Range loop
      Proof_VM (I) := Proof (I);
   end loop;

   --  Encrypted amount is opaque to the contract; use dummy bytes
   for I in Enc_Bid'Range loop
      Enc_Bid (I) := Byte (I mod 256);
   end loop;

   Put_Line ("[Private_Auction] Initializing auction...");

   Private_Auction.Initialize
     (Owner    => Owner,
      Duration => 10,
      Success  => Init_OK,
      Error    => Init_E);

   Put_Line ("  Initialize: success="
     & Boolean'Image (Init_OK)
     & " error=" & Private_Auction.Error_Code'Image (Init_E));

   if not Init_OK then
      return;
   end if;

   Put_Line ("[Private_Auction] Submitting sealed bid...");

   Private_Auction.Submit_Bid
     (Bidder           => Bidder,
      Encrypted_Amount => Enc_Bid,
      Commitment       => Commit_VM,
      Range_Proof      => Proof_VM,
      Block_Number     => 1,
      Success          => Sub_OK,
      Error            => Sub_E);

   Put_Line ("  Submit_Bid: success="
     & Boolean'Image (Sub_OK)
     & " error=" & Private_Auction.Error_Code'Image (Sub_E));

   if not Sub_OK then
      return;
   end if;

   Put_Line ("[Private_Auction] Closing auction at block 10...");

   Private_Auction.Close_Auction
     (Caller       => Owner,
      Block_Number => 10,
      Success      => Close_OK,
      Error        => Close_E);

   Put_Line ("  Close_Auction: success="
     & Boolean'Image (Close_OK)
     & " error=" & Private_Auction.Error_Code'Image (Close_E));

   if not Close_OK then
      return;
   end if;

   --  Build reveal key: 8-byte LE amount + 32-byte blinding
   declare
      V      : Unsigned_64 := Bid_Value;
      Factor : Unsigned_64 := 1;
   begin
      for I in 0 .. 7 loop
         Reveal_Key (I) := Byte (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;

      for I in 0 .. 31 loop
         Reveal_Key (8 + I) := Blinding (I);
      end loop;
   end;

   Put_Line ("[Private_Auction] Revealing bid...");

   Private_Auction.Reveal_Bid
     (Caller       => Bidder,
      Reveal_Key   => Reveal_Key,
      Block_Number => 10,
      Success      => Reveal_OK,
      Error        => Reveal_E);

   Put_Line ("  Reveal_Bid: success="
     & Boolean'Image (Reveal_OK)
     & " error=" & Private_Auction.Error_Code'Image (Reveal_E));

   if not Reveal_OK then
      return;
   end if;

   Winner      := Private_Auction.Get_Winner;
   Winning_Bid := Private_Auction.Get_Winning_Bid;
   Gas_Used    := Private_Auction.Get_Privacy_Gas_Used;
   Stark_Count := Private_Auction.Get_STARK_Verifications;

   Put_Line ("[Private_Auction] Winner first byte : "
     & Integer'Image (Integer (Winner (0))));
   Put_Line ("[Private_Auction] Winning bid       : "
     & Unsigned_64'Image (Winning_Bid));
   Put_Line ("[Private_Auction] Privacy gas used  : "
     & Aegis_Privacy.VM_Gas_Amount'Image (Gas_Used));
   Put_Line ("[Private_Auction] STARK verifications:"
     & Natural'Image (Stark_Count));

end Test_Private_Auction;

