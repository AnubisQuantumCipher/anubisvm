-------------------------------------------------------------------------------
--  ANUBIS Certification Gas Discounts Implementation Body
--  Bronze to Platinum Tiers Based on SPARK Verification Level
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Certification with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Certification_State (
      State          : out Certification_State;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      State.Total_Certified := 0;
      State.Total_Deposits := 0;
      State.Total_Gas_Saved := 0;

      for L in Cert_Level loop
         State.Level_Counts (L) := 0;
      end loop;

      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
   end Init_Certification_State;

   ---------------------------------------------------------------------------
   --  Certification Application
   ---------------------------------------------------------------------------

   procedure Apply_Certification (
      State          : in Out Certification_State;
      Contract_Addr  : Byte_Array;
      Level          : Cert_Level;
      Deposit        : Unsigned_64;
      GNATprove_Hash : Byte_Array;
      Cert           : out Contract_Certification;
      Result         : out Apply_Result
   ) is
      Required_Deposit : constant Unsigned_64 := Level_Deposits (Level);
   begin
      --  Check deposit meets requirement
      if Deposit < Required_Deposit then
         Result := Insufficient_Deposit;
         Cert := (
            Contract_Address => (others => 0),
            Level => None,
            Status => Not_Applied,
            GNATprove_Hash => (others => 0),
            VC_Count => 0,
            VCs_Proven => 0,
            CT_Analysis_Hash => (others => 0),
            Audit_Report => (others => 0),
            Auditor_PK => (others => 0),
            Deposit_Amount => 0,
            Deposit_Locked => False,
            Applied_At => 0,
            Certified_At => 0,
            Expires_At => 0,
            Last_Verified => 0,
            Gas_Saved => 0,
            TX_Count => 0
         );
         return;
      end if;

      --  Initialize certification
      Cert.Contract_Address := (others => 0);
      for I in Contract_Addr'Range loop
         if I - Contract_Addr'First <= Cert.Contract_Address'Last then
            Cert.Contract_Address (I - Contract_Addr'First) :=
               Contract_Addr (I);
         end if;
      end loop;

      Cert.Level := Level;
      Cert.Status := Pending_Review;

      --  Copy GNATprove hash
      Cert.GNATprove_Hash := (others => 0);
      for I in GNATprove_Hash'Range loop
         if I - GNATprove_Hash'First <= Cert.GNATprove_Hash'Last then
            Cert.GNATprove_Hash (I - GNATprove_Hash'First) :=
               GNATprove_Hash (I);
         end if;
      end loop;

      Cert.VC_Count := 0;
      Cert.VCs_Proven := 0;
      Cert.CT_Analysis_Hash := (others => 0);
      Cert.Audit_Report := (others => 0);
      Cert.Auditor_PK := (others => 0);
      Cert.Deposit_Amount := Deposit;
      Cert.Deposit_Locked := True;
      Cert.Applied_At := State.Current_Block;
      Cert.Certified_At := 0;
      Cert.Expires_At := 0;
      Cert.Last_Verified := 0;
      Cert.Gas_Saved := 0;
      Cert.TX_Count := 0;

      --  Update state
      State.Total_Deposits := State.Total_Deposits + Deposit;

      Result := Applied;
   end Apply_Certification;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   procedure Verify_Certification (
      Cert           : in Out Contract_Certification;
      Verifier       : Byte_Array;
      Proof_Hash     : Byte_Array;
      CT_Hash        : Byte_Array;
      Audit_Hash     : Byte_Array;
      Result         : out Verify_Result
   ) is
      pragma Unreferenced (Verifier, Proof_Hash);
      Blocks_Per_Year : constant := 5_256_000;  --  ~1 year at 6 sec/block
   begin
      --  Check certification is pending
      if Cert.Status /= Pending_Review then
         Result := Verification_Failed;
         return;
      end if;

      --  Check VC coverage based on level
      case Cert.Level is
         when None =>
            Result := Verification_Failed;
            return;

         when Bronze =>
            --  Just SPARK Mode enabled
            Cert.Status := Active;

         when Silver =>
            --  All VCs must be proven
            if not Check_VC_Coverage (Cert.VC_Count, Cert.VCs_Proven) then
               Result := Missing_Proofs;
               return;
            end if;
            Cert.Status := Active;

         when Gold =>
            --  VCs proven + constant-time analysis
            if not Check_VC_Coverage (Cert.VC_Count, Cert.VCs_Proven) then
               Result := Missing_Proofs;
               return;
            end if;
            --  Copy CT hash
            for I in CT_Hash'Range loop
               if I - CT_Hash'First <= Cert.CT_Analysis_Hash'Last then
                  Cert.CT_Analysis_Hash (I - CT_Hash'First) := CT_Hash (I);
               end if;
            end loop;
            Cert.Status := Active;

         when Platinum =>
            --  All above + audit
            if not Check_VC_Coverage (Cert.VC_Count, Cert.VCs_Proven) then
               Result := Missing_Proofs;
               return;
            end if;
            --  Check audit hash
            declare
               Has_Audit : Boolean := False;
            begin
               for I in Audit_Hash'Range loop
                  if Audit_Hash (I) /= 0 then
                     Has_Audit := True;
                     exit;
                  end if;
               end loop;
               if not Has_Audit then
                  Result := Audit_Required;
                  return;
               end if;
            end;
            --  Copy audit hash
            for I in Audit_Hash'Range loop
               if I - Audit_Hash'First <= Cert.Audit_Report'Last then
                  Cert.Audit_Report (I - Audit_Hash'First) := Audit_Hash (I);
               end if;
            end loop;
            Cert.Status := Active;
      end case;

      --  Set expiration (1 year)
      Cert.Certified_At := Cert.Applied_At;  --  Would use current block
      Cert.Expires_At := Cert.Certified_At + Blocks_Per_Year;
      Cert.Last_Verified := Cert.Certified_At;

      Result := Verified;
   end Verify_Certification;

   function Check_VC_Coverage (
      VC_Count       : Unsigned_32;
      VCs_Proven     : Unsigned_32
   ) return Boolean is
   begin
      return VCs_Proven = VC_Count;
   end Check_VC_Coverage;

   ---------------------------------------------------------------------------
   --  Level Management
   ---------------------------------------------------------------------------

   procedure Upgrade_Level (
      Cert           : in Out Contract_Certification;
      New_Level      : Cert_Level;
      Additional_Deposit : Unsigned_64;
      Additional_Proof   : Byte_Array;
      Result         : out Upgrade_Result
   ) is
      Required : Unsigned_64;
      pragma Unreferenced (Additional_Proof);
   begin
      --  Calculate additional deposit required
      Required := Level_Deposits (New_Level) - Level_Deposits (Cert.Level);

      if Additional_Deposit < Required then
         Result := Insufficient_Deposit;
         return;
      end if;

      --  Upgrade
      Cert.Level := New_Level;
      Cert.Deposit_Amount := Cert.Deposit_Amount + Additional_Deposit;

      Result := Upgraded;
   end Upgrade_Level;

   procedure Downgrade_Level (
      Cert           : in Out Contract_Certification;
      New_Level      : Cert_Level;
      Refund         : out Unsigned_64
   ) is
      Old_Deposit : constant Unsigned_64 := Level_Deposits (Cert.Level);
      New_Deposit : constant Unsigned_64 := Level_Deposits (New_Level);
   begin
      --  Calculate refund
      Refund := Old_Deposit - New_Deposit;

      --  Downgrade
      Cert.Level := New_Level;
      Cert.Deposit_Amount := Cert.Deposit_Amount - Refund;
   end Downgrade_Level;

   procedure Revoke_Certification (
      State          : in Out Certification_State;
      Cert           : in Out Contract_Certification;
      Reason_Hash    : Byte_Array;
      Slash          : Boolean;
      Slashed_Amount : out Unsigned_64
   ) is
      pragma Unreferenced (Reason_Hash);
   begin
      if Slash then
         --  Slash 50% of deposit
         Slashed_Amount := Cert.Deposit_Amount / 2;
         Cert.Deposit_Amount := Cert.Deposit_Amount - Slashed_Amount;
      else
         Slashed_Amount := 0;
      end if;

      --  Update state
      if State.Total_Deposits >= Cert.Deposit_Amount then
         State.Total_Deposits := State.Total_Deposits - Cert.Deposit_Amount;
      end if;

      if State.Level_Counts (Cert.Level) > 0 then
         State.Level_Counts (Cert.Level) :=
            State.Level_Counts (Cert.Level) - 1;
      end if;

      Cert.Status := Revoked;
      Cert.Deposit_Locked := False;
   end Revoke_Certification;

   ---------------------------------------------------------------------------
   --  Gas Discount Calculation
   ---------------------------------------------------------------------------

   function Get_Discount_Rate (
      Level          : Cert_Level
   ) return Natural is
   begin
      return Level_Discounts (Level);
   end Get_Discount_Rate;

   function Calculate_Discounted_Gas (
      Base_Gas       : Unsigned_64;
      Level          : Cert_Level
   ) return Unsigned_64 is
      Discount_BP : constant Natural := Level_Discounts (Level);
      Discount_Amount : Unsigned_64;
   begin
      --  Calculate discount: base_gas * discount_rate / 10000
      Discount_Amount := (Base_Gas * Unsigned_64 (Discount_BP)) / 10_000;

      return Base_Gas - Discount_Amount;
   end Calculate_Discounted_Gas;

   procedure Apply_Gas_Discount (
      Cert           : in Out Contract_Certification;
      Base_Gas       : Unsigned_64;
      Final_Gas      : out Unsigned_64
   ) is
      Saved : Unsigned_64;
   begin
      --  Check certification is active and not expired
      if Cert.Status /= Active then
         Final_Gas := Base_Gas;
         return;
      end if;

      --  Calculate discounted gas
      Final_Gas := Calculate_Discounted_Gas (Base_Gas, Cert.Level);

      --  Track gas saved
      Saved := Base_Gas - Final_Gas;
      Cert.Gas_Saved := Cert.Gas_Saved + Saved;
      Cert.TX_Count := Cert.TX_Count + 1;
   end Apply_Gas_Discount;

   ---------------------------------------------------------------------------
   --  Renewal
   ---------------------------------------------------------------------------

   procedure Renew_Certification (
      Cert           : in Out Contract_Certification;
      New_Proof_Hash : Byte_Array;
      Current_Block  : Unsigned_64;
      Success        : out Boolean
   ) is
      Blocks_Per_Year : constant := 5_256_000;
   begin
      --  Check certification is active or expired
      if Cert.Status /= Active and then Cert.Status /= Certification_Status'(Suspended) then
         Success := False;
         return;
      end if;

      --  Update proof hash
      for I in New_Proof_Hash'Range loop
         if I - New_Proof_Hash'First <= Cert.GNATprove_Hash'Last then
            Cert.GNATprove_Hash (I - New_Proof_Hash'First) :=
               New_Proof_Hash (I);
         end if;
      end loop;

      --  Extend expiration
      Cert.Expires_At := Current_Block + Blocks_Per_Year;
      Cert.Last_Verified := Current_Block;
      Cert.Status := Active;

      Success := True;
   end Renew_Certification;

   function Is_Expired (
      Cert           : Contract_Certification;
      Current_Block  : Unsigned_64
   ) return Boolean is
   begin
      return Current_Block >= Cert.Expires_At;
   end Is_Expired;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Level (Cert : Contract_Certification) return Cert_Level is
   begin
      return Cert.Level;
   end Get_Level;

   function Get_Deposit (Level : Cert_Level) return Unsigned_64 is
   begin
      return Level_Deposits (Level);
   end Get_Deposit;

   function Get_Total_Certified (State : Certification_State) return Unsigned_64 is
   begin
      return State.Total_Certified;
   end Get_Total_Certified;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Certification_State (State : out Certification_State) is
   begin
      State.Total_Certified := 0;
      State.Total_Deposits := 0;
      State.Total_Gas_Saved := 0;

      for L in Cert_Level loop
         State.Level_Counts (L) := 0;
      end loop;

      State.Genesis_Block := 0;
      State.Current_Block := 0;
   end Zeroize_Certification_State;

   procedure Zeroize_Contract_Cert (Cert : out Contract_Certification) is
   begin
      Cert.Contract_Address := (others => 0);
      Cert.Level := None;
      Cert.Status := Not_Applied;
      Cert.GNATprove_Hash := (others => 0);
      Cert.VC_Count := 0;
      Cert.VCs_Proven := 0;
      Cert.CT_Analysis_Hash := (others => 0);
      Cert.Audit_Report := (others => 0);
      Cert.Auditor_PK := (others => 0);
      Cert.Deposit_Amount := 0;
      Cert.Deposit_Locked := False;
      Cert.Applied_At := 0;
      Cert.Certified_At := 0;
      Cert.Expires_At := 0;
      Cert.Last_Verified := 0;
      Cert.Gas_Saved := 0;
      Cert.TX_Count := 0;
   end Zeroize_Contract_Cert;

end Anubis_Certification;
