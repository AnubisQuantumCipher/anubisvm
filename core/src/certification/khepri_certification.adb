pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;

package body Khepri_Certification is

   ---------------------------------------------------------------------------
   --  Gas Discount Calculation
   ---------------------------------------------------------------------------

   function Get_Discount (Level : Certification_Level) return Natural is
   begin
      case Level is
         when Level_None    => return 0;
         when Level_Bronze  => return Bronze_Discount;
         when Level_Silver  => return Silver_Discount;
         when Level_Gold    => return Gold_Discount;
         when Level_Platinum => return Platinum_Discount;
      end case;
   end Get_Discount;

   function Apply_Discount (
      Base_Gas : Gas_Amount;
      Level    : Certification_Level
   ) return Gas_Amount is
      Discount : constant Natural := Get_Discount (Level);
      Reduction : Gas_Amount;
   begin
      if Discount = 0 then
         return Base_Gas;
      end if;

      --  Calculate reduction: base * discount / 10000
      Reduction := Base_Gas * Gas_Amount (Discount) / 10000;

      if Reduction > Base_Gas then
         return 0;
      end if;

      return Base_Gas - Reduction;
   end Apply_Discount;

   function Effective_Gas_Price (
      Base_Price : Uint256;
      Level      : Certification_Level
   ) return Uint256 is
      Discount : constant Natural := Get_Discount (Level);
      Reduction : Uint256;
      Result : Uint256;
      Underflow : Boolean;
   begin
      if Discount = 0 then
         return Base_Price;
      end if;

      --  Calculate reduction
      Reduction := Base_Price * From_Natural (Discount) / From_Natural (10000);
      Sub (Base_Price, Reduction, Result, Underflow);

      if Underflow then
         return Zero;
      end if;

      return Result;
   end Effective_Gas_Price;

   ---------------------------------------------------------------------------
   --  Certification Verification
   ---------------------------------------------------------------------------

   function Check_Bronze (
      Contract_Code : Byte_Array
   ) return Bronze_Requirements is
      Reqs : Bronze_Requirements := (others => False);
   begin
      --  Placeholder: Real implementation would:
      --  1. Parse contract binary
      --  2. Check for SPARK_Mode pragma
      --  3. Run flow analysis
      --  4. Check compilation status

      if Contract_Code'Length > 0 then
         Reqs.SPARK_Mode_Enabled := True;
         Reqs.Compiles_Clean := True;
         Reqs.No_Runtime_Errors := True;
         Reqs.No_Uninitialized_Reads := True;
         Reqs.No_Illegal_Aliasing := True;
      end if;

      return Reqs;
   end Check_Bronze;

   function Check_Silver (
      Contract_Code : Byte_Array;
      Bronze_Status : Bronze_Requirements
   ) return Silver_Requirements is
      Reqs : Silver_Requirements := (others => False);
   begin
      Reqs.Bronze_Met := All_Bronze_Met (Bronze_Status);

      if Reqs.Bronze_Met and Contract_Code'Length > 0 then
         --  Placeholder: Real implementation would run GNATprove
         Reqs.All_Proofs_Discharged := True;
         Reqs.WCET_Analyzed := True;
         Reqs.WCET_Bounded := True;
         Reqs.No_Unchecked_Unions := True;
      end if;

      return Reqs;
   end Check_Silver;

   function Check_Gold (
      Contract_Code : Byte_Array;
      Silver_Status : Silver_Requirements
   ) return Gold_Requirements is
      Reqs : Gold_Requirements := (others => False);
   begin
      Reqs.Silver_Met := All_Silver_Met (Silver_Status);

      if Reqs.Silver_Met and Contract_Code'Length > 0 then
         --  Placeholder: Real implementation would run CT analysis
         Reqs.Constant_Time_Verified := True;
         Reqs.Security_Props_Proven := True;
         Reqs.No_Timing_Channels := True;
         Reqs.Functional_Correctness := True;
      end if;

      return Reqs;
   end Check_Gold;

   function Verify_Certification (
      Contract_Code : Byte_Array
   ) return Certification_Status is
      Status : Certification_Status := Uncertified_Status;
      Bronze : Bronze_Requirements;
      Silver : Silver_Requirements;
      Gold   : Gold_Requirements;
   begin
      Bronze := Check_Bronze (Contract_Code);
      Status.Bronze_Reqs := Bronze;

      if All_Bronze_Met (Bronze) then
         Silver := Check_Silver (Contract_Code, Bronze);
         Status.Silver_Reqs := Silver;

         if All_Silver_Met (Silver) then
            Gold := Check_Gold (Contract_Code, Silver);
            Status.Gold_Reqs := Gold;
         end if;
      end if;

      Status.Level := Calculate_Level (
         Status.Bronze_Reqs,
         Status.Silver_Reqs,
         Status.Gold_Reqs,
         Status.Platinum_Reqs
      );

      return Status;
   end Verify_Certification;

   function Calculate_Level (
      Bronze : Bronze_Requirements;
      Silver : Silver_Requirements;
      Gold   : Gold_Requirements;
      Platinum : Platinum_Requirements
   ) return Certification_Level is
   begin
      if All_Platinum_Met (Platinum) then
         return Level_Platinum;
      elsif All_Gold_Met (Gold) then
         return Level_Gold;
      elsif All_Silver_Met (Silver) then
         return Level_Silver;
      elsif All_Bronze_Met (Bronze) then
         return Level_Bronze;
      else
         return Level_None;
      end if;
   end Calculate_Level;

   ---------------------------------------------------------------------------
   --  Certification Registry
   ---------------------------------------------------------------------------

   --  Internal registry storage (placeholder)
   type Registry_Storage is array (Registry_Index) of Registry_Entry;
   Registry : Registry_Storage := (others => (
      Contract_Address => (others => 0),
      Status           => Uncertified_Status,
      Version          => 0,
      Is_Active        => False,
      Revoked          => False,
      Revoked_Reason   => Empty_String
   ));
   Registry_Count : Natural := 0;

   procedure Register_Certification (
      Contract  : in Address;
      Status    : in Certification_Status;
      Success   : out Boolean
   ) is
   begin
      if Registry_Count >= Max_Registry_Entries then
         Success := False;
         return;
      end if;

      Registry (Registry_Index (Registry_Count)) := (
         Contract_Address => Contract,
         Status           => Status,
         Version          => 1,
         Is_Active        => True,
         Revoked          => False,
         Revoked_Reason   => Empty_String
      );
      Registry_Count := Registry_Count + 1;
      Success := True;
   end Register_Certification;

   function Get_Certification (
      Contract : Address
   ) return Certification_Status is
   begin
      for I in 0 .. Registry_Count - 1 loop
         if Registry (Registry_Index (I)).Contract_Address = Contract and
            Registry (Registry_Index (I)).Is_Active
         then
            return Registry (Registry_Index (I)).Status;
         end if;
      end loop;
      return Uncertified_Status;
   end Get_Certification;

   function Has_Minimum_Level (
      Contract : Address;
      Minimum  : Certification_Level
   ) return Boolean is
      Status : constant Certification_Status := Get_Certification (Contract);
   begin
      return Status.Level >= Minimum;
   end Has_Minimum_Level;

   procedure Revoke_Certification (
      Contract : in Address;
      Reason   : in Bounded_String;
      Success  : out Boolean
   ) is
   begin
      for I in 0 .. Registry_Count - 1 loop
         if Registry (Registry_Index (I)).Contract_Address = Contract and
            Registry (Registry_Index (I)).Is_Active
         then
            Registry (Registry_Index (I)).Revoked := True;
            Registry (Registry_Index (I)).Revoked_Reason := Reason;
            Registry (Registry_Index (I)).Is_Active := False;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Revoke_Certification;

   ---------------------------------------------------------------------------
   --  Certification Badges
   ---------------------------------------------------------------------------

   function Get_Badge (Level : Certification_Level) return Badge_Type is
   begin
      case Level is
         when Level_None    => return Badge_None;
         when Level_Bronze  => return Badge_Bronze;
         when Level_Silver  => return Badge_Silver;
         when Level_Gold    => return Badge_Gold;
         when Level_Platinum => return Badge_Platinum;
      end case;
   end Get_Badge;

   function Badge_Description (Badge : Badge_Type) return String is
   begin
      case Badge is
         when Badge_None    => return "Uncertified";
         when Badge_Bronze  => return "Flow Analysis Clean";
         when Badge_Silver  => return "Fully Proven + WCET";
         when Badge_Gold    => return "CT-Safe + Security";
         when Badge_Platinum => return "Audited";
      end case;
   end Badge_Description;

end Khepri_Certification;
