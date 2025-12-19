pragma SPARK_Mode (On);

package body quantum_escrow with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Account_Equal (A, B : Account_ID) return Boolean is
      Result : Boolean := True;
   begin
      --  Constant-time comparison
      for I in Account_ID_Index loop
         if A (I) /= B (I) then
            Result := False;
         end if;
         pragma Loop_Invariant
           (Result = (for all J in Account_ID_Index'First .. I => A (J) = B (J)));
      end loop;
      return Result;
   end Account_Equal;

   function Calculate_Fee (
      Amount  : U256;
      Fee_Bps : Unsigned_16) return U256
   is
      --  Simple fee calculation: Amount * Fee_Bps / 10000
      --  For simplicity, just use first limb (works for reasonable amounts)
      Fee_Amount : Unsigned_64;
   begin
      --  Zero fee means no fee
      if Fee_Bps = 0 then
         return U256_Zero;
      end if;

      if Amount.Limbs (0) > Unsigned_64'Last / Unsigned_64 (Fee_Bps) then
         --  Overflow protection
         return U256_Zero;
      end if;
      Fee_Amount := (Amount.Limbs (0) * Unsigned_64 (Fee_Bps)) / 10000;
      return (Limbs => (Fee_Amount, 0, 0, 0));
   end Calculate_Fee;

   ---------------------------------------------------------------------------
   --  Core Contract Functions
   ---------------------------------------------------------------------------

   procedure Initialize (
      State : in Out Contract_State;
      Owner : Account_ID;
      Fee   : Unsigned_16)
   is
   begin
      State.Initialized := True;
      State.Owner := Owner;
      State.Total_Escrows := 0;
      State.Active_Escrows := 0;
      State.Total_Volume := U256_Zero;
      State.Fee_Basis_Pts := Fee;
      State.Cert_Level := Platinum;

      for I in Escrow_ID loop
         State.Escrows (I) := Empty_Escrow;
      end loop;
   end Initialize;

   procedure Create_Escrow (
      State        : in Out Contract_State;
      Buyer        : Account_ID;
      Seller       : Account_ID;
      Arbiter      : Account_ID;
      Amount       : U256;
      Release_Time : Unsigned_64;
      Description  : Hash256;
      Eid          : out Escrow_ID;
      Success      : out Boolean)
   is
   begin
      --  Check capacity
      if State.Total_Escrows >= Unsigned_64 (Max_Escrows) then
         Eid := 0;
         Success := False;
         return;
      end if;

      --  Check amount is non-zero
      if Amount = U256_Zero then
         Eid := 0;
         Success := False;
         return;
      end if;

      --  Allocate escrow slot
      Eid := Escrow_ID (State.Total_Escrows);

      State.Escrows (Eid) := (
         Status       => Created,
         Buyer        => Buyer,
         Seller       => Seller,
         Arbiter      => Arbiter,
         Amount       => Amount,
         Release_Time => Release_Time,
         Created_At   => 0,  --  Would come from block timestamp
         Description  => Description);

      State.Total_Escrows := State.Total_Escrows + 1;
      Success := True;
   end Create_Escrow;

   procedure Fund_Escrow (
      State   : in Out Contract_State;
      Eid     : Escrow_ID;
      Caller  : Account_ID;
      Success : out Boolean)
   is
      E : Escrow renames State.Escrows (Eid);
   begin
      --  Must be buyer
      if not Account_Equal (Caller, E.Buyer) then
         Success := False;
         return;
      end if;

      --  Must be in Created state
      if E.Status /= Created then
         Success := False;
         return;
      end if;

      --  Mark as funded
      State.Escrows (Eid).Status := Funded;
      State.Active_Escrows := State.Active_Escrows + 1;

      --  Track volume
      State.Total_Volume.Limbs (0) :=
         State.Total_Volume.Limbs (0) + E.Amount.Limbs (0);

      Success := True;
   end Fund_Escrow;

   procedure Release (
      State   : in Out Contract_State;
      Eid     : Escrow_ID;
      Caller  : Account_ID;
      Success : out Boolean)
   is
      E : Escrow renames State.Escrows (Eid);
   begin
      --  Only buyer can release
      if not Account_Equal (Caller, E.Buyer) then
         Success := False;
         return;
      end if;

      --  Must be funded
      if E.Status /= Funded then
         Success := False;
         return;
      end if;

      --  Release to seller
      State.Escrows (Eid).Status := Released;
      if State.Active_Escrows > 0 then
         State.Active_Escrows := State.Active_Escrows - 1;
      end if;

      Success := True;
   end Release;

   procedure Refund (
      State        : in Out Contract_State;
      Eid          : Escrow_ID;
      Caller       : Account_ID;
      Current_Time : Unsigned_64;
      Success      : out Boolean)
   is
      E : Escrow renames State.Escrows (Eid);
   begin
      --  Must be funded
      if E.Status /= Funded then
         Success := False;
         return;
      end if;

      --  Seller can refund anytime, or anyone after timeout
      if Account_Equal (Caller, E.Seller) or
         (E.Release_Time > 0 and Current_Time >= E.Release_Time)
      then
         State.Escrows (Eid).Status := Refunded;
         if State.Active_Escrows > 0 then
            State.Active_Escrows := State.Active_Escrows - 1;
         end if;
         Success := True;
      else
         Success := False;
      end if;
   end Refund;

   procedure Open_Dispute (
      State   : in Out Contract_State;
      Eid     : Escrow_ID;
      Caller  : Account_ID;
      Success : out Boolean)
   is
      E : Escrow renames State.Escrows (Eid);
   begin
      --  Must be funded
      if E.Status /= Funded then
         Success := False;
         return;
      end if;

      --  Only buyer or seller can open dispute
      if Account_Equal (Caller, E.Buyer) or Account_Equal (Caller, E.Seller) then
         State.Escrows (Eid).Status := Disputed;
         Success := True;
      else
         Success := False;
      end if;
   end Open_Dispute;

   procedure Resolve_Dispute (
      State             : in Out Contract_State;
      Eid               : Escrow_ID;
      Caller            : Account_ID;
      Release_To_Seller : Boolean;
      Success           : out Boolean)
   is
      E : Escrow renames State.Escrows (Eid);
   begin
      --  Must be disputed
      if E.Status /= Disputed then
         Success := False;
         return;
      end if;

      --  Only arbiter can resolve
      if not Account_Equal (Caller, E.Arbiter) then
         Success := False;
         return;
      end if;

      --  Resolve based on arbiter decision
      State.Escrows (Eid).Status := Resolved;
      if State.Active_Escrows > 0 then
         State.Active_Escrows := State.Active_Escrows - 1;
      end if;

      --  Note: In real implementation, would transfer funds here
      --  based on Release_To_Seller flag
      pragma Unreferenced (Release_To_Seller);

      Success := True;
   end Resolve_Dispute;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Escrow_Status (
      State     : Contract_State;
      Eid       : Escrow_ID) return Escrow_Status
   is
   begin
      return State.Escrows (Eid).Status;
   end Get_Escrow_Status;

   function Get_Escrow_Amount (
      State     : Contract_State;
      Eid       : Escrow_ID) return U256
   is
   begin
      return State.Escrows (Eid).Amount;
   end Get_Escrow_Amount;

   function Get_Total_Escrows (State : Contract_State) return Unsigned_64 is
   begin
      return State.Total_Escrows;
   end Get_Total_Escrows;

   function Get_Active_Count (State : Contract_State) return Unsigned_64 is
   begin
      return State.Active_Escrows;
   end Get_Active_Count;

   function Get_Total_Volume (State : Contract_State) return U256 is
   begin
      return State.Total_Volume;
   end Get_Total_Volume;

   function Is_Participant (
      State     : Contract_State;
      Eid       : Escrow_ID;
      Caller    : Account_ID) return Boolean
   is
      E : Escrow renames State.Escrows (Eid);
   begin
      return Account_Equal (Caller, E.Buyer) or
             Account_Equal (Caller, E.Seller) or
             Account_Equal (Caller, E.Arbiter);
   end Is_Participant;

end quantum_escrow;
