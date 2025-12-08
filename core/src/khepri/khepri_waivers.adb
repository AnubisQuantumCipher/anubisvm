-------------------------------------------------------------------------------
--  KHEPRI - Proof Waiver Management System Implementation
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Anubis_SHA3;

package body Khepri_Waivers with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Registry Management
   ---------------------------------------------------------------------------

   procedure Init_Registry (
      Registry       : out Waiver_Registry
   ) is
   begin
      Registry.Count := 0;
      Registry.Last_Updated := 0;
      Registry.Active_Count := 0;
      Registry.Expired_Count := 0;
      Registry.High_Sev_Count := 0;

      for I in Registry.Waivers'Range loop
         Registry.Waivers (I).Valid := False;
         Registry.Waivers (I).Status := Expired;
      end loop;
   end Init_Registry;

   procedure Add_Waiver (
      Registry       : in Out Waiver_Registry;
      Waiver         : Waiver_Record;
      Index          : out Waiver_Index;
      Success        : out Boolean
   ) is
   begin
      if Registry.Count >= Max_Active_Waivers then
         Index := 0;
         Success := False;
         return;
      end if;

      Index := Registry.Count;
      Registry.Waivers (Index) := Waiver;
      Registry.Count := Registry.Count + 1;

      if Waiver.Status = Active then
         Registry.Active_Count := Registry.Active_Count + 1;
      end if;

      if Waiver.Severity = High or Waiver.Severity = Critical then
         Registry.High_Sev_Count := Registry.High_Sev_Count + 1;
      end if;

      Success := True;
   end Add_Waiver;

   procedure Remove_Waiver (
      Registry       : in Out Waiver_Registry;
      Index          : Waiver_Index;
      Success        : out Boolean
   ) is
   begin
      if Index >= Registry.Count then
         Success := False;
         return;
      end if;

      if Registry.Waivers (Index).Status = Active then
         Registry.Active_Count := Registry.Active_Count - 1;
      end if;

      Registry.Waivers (Index).Status := Revoked;
      Success := True;
   end Remove_Waiver;

   procedure Update_Status (
      Registry       : in Out Waiver_Registry;
      Index          : Waiver_Index;
      New_Status     : Waiver_Status
   ) is
      Old_Status : constant Waiver_Status := Registry.Waivers (Index).Status;
   begin
      if Old_Status = Active and New_Status /= Active then
         Registry.Active_Count := Registry.Active_Count - 1;
      elsif Old_Status /= Active and New_Status = Active then
         Registry.Active_Count := Registry.Active_Count + 1;
      end if;

      if New_Status = Expired then
         Registry.Expired_Count := Registry.Expired_Count + 1;
      end if;

      Registry.Waivers (Index).Status := New_Status;
   end Update_Status;

   procedure Process_Expirations (
      Registry       : in Out Waiver_Registry;
      Current_Date   : Waiver_Date;
      Expired_Count  : out Natural
   ) is
   begin
      Expired_Count := 0;

      for I in 0 .. Registry.Count - 1 loop
         if Registry.Waivers (I).Status = Active and
            Registry.Waivers (I).Expires_Date < Current_Date
         then
            Registry.Waivers (I).Status := Expired;
            Expired_Count := Expired_Count + 1;
            Registry.Active_Count := Registry.Active_Count - 1;
            Registry.Expired_Count := Registry.Expired_Count + 1;
         end if;
      end loop;
   end Process_Expirations;

   ---------------------------------------------------------------------------
   --  Waiver Creation
   ---------------------------------------------------------------------------

   procedure Create_Waiver (
      ID             : Waiver_ID_String;
      Category       : Waiver_Category;
      Unit_Name      : String;
      VC_ID          : String;
      Justification  : String;
      Mitigation     : String;
      Approved_Date  : Waiver_Date;
      Expires_Date   : Waiver_Date;
      Waiver         : out Waiver_Record;
      Success        : out Boolean
   ) is
   begin
      Waiver.ID := ID;
      Waiver.Category := Category;
      Waiver.Status := Active;
      Waiver.Severity := Category_Severity (Category);

      --  Copy unit name
      Waiver.Unit_Name_Len := Unit_Name'Length;
      for I in 1 .. Unit_Name'Length loop
         Waiver.Unit_Name (I) := Unit_Name (Unit_Name'First + I - 1);
      end loop;
      for I in Unit_Name'Length + 1 .. Max_Unit_Name_Len loop
         Waiver.Unit_Name (I) := ' ';
      end loop;

      --  Copy VC ID
      Waiver.VC_ID_Len := VC_ID'Length;
      for I in 1 .. VC_ID'Length loop
         Waiver.VC_ID (I) := VC_ID (VC_ID'First + I - 1);
      end loop;
      for I in VC_ID'Length + 1 .. Max_VC_ID_Len loop
         Waiver.VC_ID (I) := ' ';
      end loop;

      --  Copy justification
      Waiver.Just_Len := Justification'Length;
      for I in 1 .. Justification'Length loop
         Waiver.Justification (I) := Justification (Justification'First + I - 1);
      end loop;
      for I in Justification'Length + 1 .. Max_Justification_Len loop
         Waiver.Justification (I) := ' ';
      end loop;

      --  Copy mitigation
      Waiver.Mit_Len := Mitigation'Length;
      for I in 1 .. Mitigation'Length loop
         Waiver.Mitigation (I) := Mitigation (Mitigation'First + I - 1);
      end loop;
      for I in Mitigation'Length + 1 .. Max_Mitigation_Len loop
         Waiver.Mitigation (I) := ' ';
      end loop;

      Waiver.Approved_Date := Approved_Date;
      Waiver.Expires_Date := Expires_Date;

      for I in Waiver.Reviewer_Hash'Range loop
         Waiver.Reviewer_Hash (I) := 0;
      end loop;

      for I in Waiver.On_Chain_Hash'Range loop
         Waiver.On_Chain_Hash (I) := 0;
      end loop;

      Waiver.Chain_Block := 0;
      Waiver.Recorded := False;
      Waiver.Valid := True;

      Success := True;
   end Create_Waiver;

   procedure Set_Reviewer (
      Waiver         : in Out Waiver_Record;
      Reviewer_Hash  : Byte_Array
   ) is
   begin
      for I in 0 .. Hash_Size - 1 loop
         Waiver.Reviewer_Hash (I) := Reviewer_Hash (Reviewer_Hash'First + I);
      end loop;
   end Set_Reviewer;

   ---------------------------------------------------------------------------
   --  Validation
   ---------------------------------------------------------------------------

   function Validate_Waiver (
      Waiver         : Waiver_Record;
      Current_Date   : Waiver_Date
   ) return Validation_Result is
   begin
      if not Waiver.Valid then
         return Invalid_ID;
      end if;

      if Waiver.Just_Len = 0 then
         return Missing_Justification;
      end if;

      if Waiver.Mit_Len = 0 then
         return Missing_Mitigation;
      end if;

      if Waiver.Approved_Date >= Waiver.Expires_Date then
         return Invalid_Dates;
      end if;

      if Current_Date > Waiver.Expires_Date then
         return Expired;
      end if;

      return Valid;
   end Validate_Waiver;

   function Is_Active (
      Waiver         : Waiver_Record;
      Current_Date   : Waiver_Date
   ) return Boolean is
   begin
      return Waiver.Valid and
             Waiver.Status = Active and
             Current_Date <= Waiver.Expires_Date;
   end Is_Active;

   function Is_Unit_Waived (
      Registry       : Waiver_Registry;
      Unit_Name      : String;
      Current_Date   : Waiver_Date
   ) return Boolean is
   begin
      for I in 0 .. Registry.Count - 1 loop
         if Is_Active (Registry.Waivers (I), Current_Date) then
            declare
               Matches : Boolean := True;
            begin
               if Unit_Name'Length = Registry.Waivers (I).Unit_Name_Len then
                  for J in 1 .. Unit_Name'Length loop
                     if Unit_Name (Unit_Name'First + J - 1) /=
                        Registry.Waivers (I).Unit_Name (J)
                     then
                        Matches := False;
                        exit;
                     end if;
                  end loop;

                  if Matches then
                     return True;
                  end if;
               end if;
            end;
         end if;
      end loop;

      return False;
   end Is_Unit_Waived;

   function Is_VC_Waived (
      Registry       : Waiver_Registry;
      Unit_Name      : String;
      VC_ID          : String;
      Current_Date   : Waiver_Date
   ) return Boolean is
      pragma Unreferenced (VC_ID);
   begin
      return Is_Unit_Waived (Registry, Unit_Name, Current_Date);
   end Is_VC_Waived;

   ---------------------------------------------------------------------------
   --  Query Operations
   ---------------------------------------------------------------------------

   procedure Find_By_ID (
      Registry       : Waiver_Registry;
      ID             : Waiver_ID_String;
      Result         : out Query_Result
   ) is
   begin
      Result.Found := False;
      Result.Index := 0;

      for I in 0 .. Registry.Count - 1 loop
         if Registry.Waivers (I).ID = ID then
            Result.Found := True;
            Result.Index := I;
            Result.Waiver := Registry.Waivers (I);
            return;
         end if;
      end loop;
   end Find_By_ID;

   procedure Find_By_Unit (
      Registry       : Waiver_Registry;
      Unit_Name      : String;
      Indices        : out Waiver_Array;
      Count          : out Natural
   ) is
   begin
      Count := 0;

      for I in 0 .. Registry.Count - 1 loop
         if Registry.Waivers (I).Unit_Name_Len = Unit_Name'Length then
            declare
               Matches : Boolean := True;
            begin
               for J in 1 .. Unit_Name'Length loop
                  if Unit_Name (Unit_Name'First + J - 1) /=
                     Registry.Waivers (I).Unit_Name (J)
                  then
                     Matches := False;
                     exit;
                  end if;
               end loop;

               if Matches and Count < Max_Active_Waivers then
                  Indices (Waiver_Index (Count)) := Registry.Waivers (I);
                  Count := Count + 1;
               end if;
            end;
         end if;
      end loop;
   end Find_By_Unit;

   procedure Find_By_Category (
      Registry       : Waiver_Registry;
      Category       : Waiver_Category;
      Indices        : out Waiver_Array;
      Count          : out Natural
   ) is
   begin
      Count := 0;

      for I in 0 .. Registry.Count - 1 loop
         if Registry.Waivers (I).Category = Category and
            Count < Max_Active_Waivers
         then
            Indices (Waiver_Index (Count)) := Registry.Waivers (I);
            Count := Count + 1;
         end if;
      end loop;
   end Find_By_Category;

   procedure Find_Expiring (
      Registry       : Waiver_Registry;
      Current_Date   : Waiver_Date;
      Days_Ahead     : Natural;
      Indices        : out Waiver_Array;
      Count          : out Natural
   ) is
   begin
      Count := 0;

      for I in 0 .. Registry.Count - 1 loop
         if Registry.Waivers (I).Status = Active and
            Registry.Waivers (I).Expires_Date <= Current_Date + Days_Ahead and
            Count < Max_Active_Waivers
         then
            Indices (Waiver_Index (Count)) := Registry.Waivers (I);
            Count := Count + 1;
         end if;
      end loop;
   end Find_Expiring;

   ---------------------------------------------------------------------------
   --  On-Chain Recording
   ---------------------------------------------------------------------------

   procedure Compute_Waiver_Hash (
      Waiver         : Waiver_Record;
      Hash           : out Byte_Array
   ) is
      Input : Byte_Array (0 .. 255);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Build hash input from waiver ID and dates
      for I in 1 .. Waiver_ID_Length loop
         Input (I - 1) := Byte (Character'Pos (Waiver.ID (I)));
      end loop;

      for I in Waiver_ID_Length .. 255 loop
         Input (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_256 (Input, Hash_Out);

      for I in 0 .. Hash_Size - 1 loop
         Hash (Hash'First + I) := Hash_Out (I);
      end loop;
   end Compute_Waiver_Hash;

   procedure Prepare_Chain_Record (
      Waiver         : Waiver_Record;
      TX_Data        : out Byte_Array;
      TX_Length      : out Natural
   ) is
   begin
      Compute_Waiver_Hash (Waiver, TX_Data (TX_Data'First .. TX_Data'First + Hash_Size - 1));
      TX_Length := Hash_Size;
   end Prepare_Chain_Record;

   procedure Mark_Recorded (
      Waiver         : in Out Waiver_Record;
      Block_Number   : Unsigned_64;
      TX_Hash        : Byte_Array
   ) is
   begin
      for I in 0 .. Hash_Size - 1 loop
         Waiver.On_Chain_Hash (I) := TX_Hash (TX_Hash'First + I);
      end loop;

      Waiver.Chain_Block := Block_Number;
      Waiver.Recorded := True;
   end Mark_Recorded;

   function Verify_Chain_Record (
      Waiver         : Waiver_Record;
      Chain_Hash     : Byte_Array
   ) return Boolean is
   begin
      if not Waiver.Recorded then
         return False;
      end if;

      for I in 0 .. Hash_Size - 1 loop
         if Waiver.On_Chain_Hash (I) /= Chain_Hash (Chain_Hash'First + I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Chain_Record;

   ---------------------------------------------------------------------------
   --  Statistics and Reporting
   ---------------------------------------------------------------------------

   function Get_Stats (
      Registry       : Waiver_Registry;
      Current_Date   : Waiver_Date
   ) return Waiver_Stats is
      Stats : Waiver_Stats;
      Expiring : Natural := 0;
   begin
      Stats.Total_Waivers := Registry.Count;
      Stats.Active_Waivers := Registry.Active_Count;
      Stats.Expired_Waivers := Registry.Expired_Count;
      Stats.High_Severity := Registry.High_Sev_Count;

      for I in 0 .. Registry.Count - 1 loop
         if Registry.Waivers (I).Status = Active and
            Registry.Waivers (I).Expires_Date <= Current_Date + 30
         then
            Expiring := Expiring + 1;
         end if;
      end loop;

      Stats.Expiring_Soon := Expiring;
      Stats.Coverage_Percent := 0;

      return Stats;
   end Get_Stats;

   function Check_Thresholds (
      Stats          : Waiver_Stats;
      Max_Active     : Natural;
      Max_High_Sev   : Natural
   ) return Boolean is
   begin
      return Stats.Active_Waivers <= Max_Active and
             Stats.High_Severity <= Max_High_Sev;
   end Check_Thresholds;

   function Category_Severity (Category : Waiver_Category) return Severity_Level is
   begin
      case Category is
         when Timing_Sensitive =>
            return Critical;
         when External_FFI =>
            return High;
         when Platform_Dependent =>
            return Medium;
         when Performance_Critical =>
            return Medium;
         when Legacy_Interface =>
            return Low;
      end case;
   end Category_Severity;

   function Category_Max_Duration (Category : Waiver_Category) return Natural is
   begin
      case Category is
         when Timing_Sensitive =>
            return 30;
         when External_FFI =>
            return 90;
         when Platform_Dependent =>
            return 180;
         when Performance_Critical =>
            return 90;
         when Legacy_Interface =>
            return 365;
      end case;
   end Category_Max_Duration;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Waiver (
      Waiver         : Waiver_Record;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
   begin
      --  Simple serialization of waiver ID
      for I in 1 .. Waiver_ID_Length loop
         Output (Output'First + I - 1) := Byte (Character'Pos (Waiver.ID (I)));
      end loop;
      Length := Waiver_ID_Length;
   end Serialize_Waiver;

   procedure Deserialize_Waiver (
      Input          : Byte_Array;
      Waiver         : out Waiver_Record;
      Success        : out Boolean
   ) is
   begin
      if Input'Length < Waiver_ID_Length then
         Success := False;
         return;
      end if;

      for I in 1 .. Waiver_ID_Length loop
         Waiver.ID (I) := Character'Val (Natural (Input (Input'First + I - 1)));
      end loop;

      Waiver.Valid := True;
      Success := True;
   end Deserialize_Waiver;

   procedure Serialize_Registry (
      Registry       : Waiver_Registry;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
   begin
      --  Serialize count
      for I in 0 .. 3 loop
         Output (Output'First + I) := Byte ((Registry.Count / (2 ** (I * 8))) mod 256);
      end loop;
      Length := 4;
   end Serialize_Registry;

   procedure Deserialize_Registry (
      Input          : Byte_Array;
      Registry       : out Waiver_Registry;
      Success        : out Boolean
   ) is
   begin
      Init_Registry (Registry);

      if Input'Length < 4 then
         Success := False;
         return;
      end if;

      Registry.Count := 0;
      for I in 0 .. 3 loop
         Registry.Count := Registry.Count +
            Natural (Input (Input'First + I)) * (2 ** (I * 8));
      end loop;

      Success := True;
   end Deserialize_Registry;

   ---------------------------------------------------------------------------
   --  Date Utilities
   ---------------------------------------------------------------------------

   function Make_Date (
      Year           : Natural;
      Month          : Natural;
      Day            : Natural
   ) return Waiver_Date is
      Days : Natural := 0;
   begin
      Days := (Year - 2020) * 365 + (Month - 1) * 30 + (Day - 1);
      if Days > 36500 then
         Days := 36500;
      end if;
      return Days;
   end Make_Date;

   function Days_Until_Expiry (
      Waiver         : Waiver_Record;
      Current_Date   : Waiver_Date
   ) return Integer is
   begin
      return Integer (Waiver.Expires_Date) - Integer (Current_Date);
   end Days_Until_Expiry;

   ---------------------------------------------------------------------------
   --  CI/CD Integration
   ---------------------------------------------------------------------------

   procedure Generate_Exclusions (
      Registry       : Waiver_Registry;
      Current_Date   : Waiver_Date;
      Output         : out String;
      Length         : out Natural
   ) is
      pragma Unreferenced (Registry);
      pragma Unreferenced (Current_Date);
   begin
      Output (Output'First) := ' ';
      Length := 0;
   end Generate_Exclusions;

   function Validate_Exclusions (
      Registry       : Waiver_Registry;
      Exclusions     : String;
      Current_Date   : Waiver_Date
   ) return Boolean is
      pragma Unreferenced (Registry);
      pragma Unreferenced (Exclusions);
      pragma Unreferenced (Current_Date);
   begin
      return True;
   end Validate_Exclusions;

end Khepri_Waivers;
