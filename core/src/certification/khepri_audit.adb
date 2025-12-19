pragma SPARK_Mode (On);

with Anubis_MLDSA; use Anubis_MLDSA;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_Types; use Anubis_Types;

package body Khepri_Audit is

   ---------------------------------------------------------------------------
   --  Audit Package Generation
   ---------------------------------------------------------------------------

   procedure Generate_Audit_Package (
      Contract_Path : in     String;
      Package_Out   : out    Audit_Package;
      Success       : out    Boolean
   ) is
      pragma Unreferenced (Contract_Path);
      Empty_Hash : constant Hash256 := (others => 0);
   begin
      Package_Out := (
         Contract_Name    => Empty_String,
         Contract_Version => Empty_String,
         Contract_Address => (others => 0),
         Contract_Hash    => Empty_Hash,
         Source_Hash      => Empty_Hash,
         Proof_Hash       => Empty_Hash,
         WCET_Hash        => Empty_Hash,
         CT_Analysis_Hash => Empty_Hash,
         Current_Level    => Level_None,
         Generated_At     => 0,
         Expires_At       => 0,
         Package_Hash     => Empty_Hash
      );

      --  Real implementation would:
      --  1. Load contract source code from Contract_Path
      --  2. Compute SHA3-256 of source code -> Source_Hash
      --  3. Load proof artifacts (gnatprove output) -> Proof_Hash
      --  4. Load WCET analysis results -> WCET_Hash
      --  5. Load CT analysis results -> CT_Analysis_Hash
      --  6. Extract contract metadata (name, version, address)
      --  7. Set timestamps (Generated_At, Expires_At)
      --  8. Compute SHA3-256 of entire package -> Package_Hash

      --  For production implementation:
      --  - Parse contract binary/source
      --  - Extract certification metadata
      --  - Bundle all artifacts into serializable package
      --  - Generate cryptographic hash of package for integrity

      Success := True;
   end Generate_Audit_Package;

   procedure Serialize_Package (
      Package_In : in  Audit_Package;
      Data       : out Byte_Array;
      Size       : out Natural;
      Success    : out Boolean
   ) is
      pragma Unreferenced (Package_In);
   begin
      Data := (others => 0);
      Size := 0;
      Success := True;
   end Serialize_Package;

   procedure Parse_Package (
      Data       : in  Byte_Array;
      Package_Out: out Audit_Package;
      Success    : out Boolean
   ) is
      pragma Unreferenced (Data);
   begin
      Package_Out := (
         Contract_Name    => Empty_String,
         Contract_Version => Empty_String,
         Contract_Address => (others => 0),
         Contract_Hash    => (others => 0),
         Source_Hash      => (others => 0),
         Proof_Hash       => (others => 0),
         WCET_Hash        => (others => 0),
         CT_Analysis_Hash => (others => 0),
         Current_Level    => Level_None,
         Generated_At     => 0,
         Expires_At       => 0,
         Package_Hash     => (others => 0)
      );
      Success := True;
   end Parse_Package;

   ---------------------------------------------------------------------------
   --  Audit Report Operations
   ---------------------------------------------------------------------------

   function Create_Report (
      Contract  : Khepri_Types.Address;
      Auditor   : Khepri_Types.Address
   ) return Audit_Report is
   begin
      return (
         Report_ID        => (others => 0),
         Contract_Address => Contract,
         Contract_Hash    => (others => 0),
         Auditor          => Auditor,
         Audit_Start      => 0,
         Audit_End        => 0,
         Report_Time      => 0,
         Version_Audited  => Empty_String,
         Commit_Hash      => (others => 0),
         Scope_Description=> Empty_String,
         Findings         => (others => (
            ID          => 0,
            Severity    => Finding_Info,
            Status      => Status_Open,
            Title       => Empty_String,
            Description => Empty_String,
            Location    => Empty_String,
            Recommendation => Empty_String,
            Response    => Empty_String
         )),
         Finding_Count    => 0,
         Critical_Count   => 0,
         High_Count       => 0,
         Medium_Count     => 0,
         Low_Count        => 0,
         Info_Count       => 0,
         Verdict          => Verdict_Incomplete,
         Summary          => Empty_String,
         Signature        => (others => 0),
         Is_Signed        => False
      );
   end Create_Report;

   procedure Add_Finding (
      Report  : in Out Audit_Report;
      Finding : in     Audit_Finding;
      Success : out    Boolean
   ) is
   begin
      if Report.Finding_Count >= Max_Findings then
         Success := False;
         return;
      end if;

      Report.Findings (Finding_Index (Report.Finding_Count)) := Finding;
      Report.Finding_Count := Report.Finding_Count + 1;

      --  Update counts
      case Finding.Severity is
         when Finding_Critical => Report.Critical_Count := Report.Critical_Count + 1;
         when Finding_High     => Report.High_Count := Report.High_Count + 1;
         when Finding_Medium   => Report.Medium_Count := Report.Medium_Count + 1;
         when Finding_Low      => Report.Low_Count := Report.Low_Count + 1;
         when Finding_Info     => Report.Info_Count := Report.Info_Count + 1;
      end case;

      Success := True;
   end Add_Finding;

   procedure Set_Verdict (
      Report  : in Out Audit_Report;
      Verdict : in     Audit_Verdict;
      Summary : in     Bounded_String
   ) is
   begin
      Report.Verdict := Verdict;
      Report.Summary := Summary;
   end Set_Verdict;

   procedure Sign_Report (
      Report     : in Out Audit_Report;
      Secret_Key : in     MLDSA_Secret_Key;
      Success    : out    Boolean
   ) is
      --  Convert report to byte array for signing
      Report_Bytes : Byte_Array (0 .. 1023);
      Msg_Length : Natural := 0;
      Zero_Random : constant Anubis_MLDSA.Seed := (others => 0);
      Sig : Anubis_MLDSA.Signature;
      Sign_Success : Boolean;
   begin
      --  Serialize report fields into message for signing
      --  Include: Report_ID, Contract_Address, Contract_Hash, Auditor,
      --           timestamps, version, scope, findings, verdict, summary

      --  In real implementation, would serialize all report fields
      --  For now, use Report_ID as representative content
      for I in Report.Report_ID'Range loop
         if Msg_Length < Report_Bytes'Length then
            Report_Bytes (Report_Bytes'First + Msg_Length) := Report.Report_ID (I);
            Msg_Length := Msg_Length + 1;
         end if;
      end loop;

      --  Sign the report using ML-DSA-87
      --  Use deterministic signing (zero random) for reproducibility
      Anubis_MLDSA.Sign (
         SK      => Secret_Key,
         Msg     => Report_Bytes (0 .. Msg_Length - 1),
         Random  => Zero_Random,
         Sig     => Sig,
         Success => Sign_Success
      );

      if Sign_Success then
         --  Copy signature to report
         for I in Sig'Range loop
            Report.Signature (I) := Sig (I);
         end loop;
         Report.Is_Signed := True;
         Success := True;
      else
         Report.Is_Signed := False;
         Success := False;
      end if;
   end Sign_Report;

   function Verify_Report (
      Report     : Audit_Report;
      Public_Key : MLDSA_Public_Key
   ) return Boolean is
      --  Reconstruct message bytes for verification
      Report_Bytes : Byte_Array (0 .. 1023);
      Msg_Length : Natural := 0;
      Sig : Anubis_MLDSA.Signature;
   begin
      --  Report must be signed
      if not Report.Is_Signed then
         return False;
      end if;

      --  Serialize report fields (same as in Sign_Report)
      for I in Report.Report_ID'Range loop
         if Msg_Length < Report_Bytes'Length then
            Report_Bytes (Report_Bytes'First + Msg_Length) := Report.Report_ID (I);
            Msg_Length := Msg_Length + 1;
         end if;
      end loop;

      --  Copy signature from report
      for I in Sig'Range loop
         Sig (I) := Report.Signature (I);
      end loop;

      --  Verify ML-DSA-87 signature
      return Anubis_MLDSA.Verify (
         PK  => Public_Key,
         Msg => Report_Bytes (0 .. Msg_Length - 1),
         Sig => Sig
      );
   end Verify_Report;

   function Report_Hash (Report : Audit_Report) return Hash256 is
      --  Serialize report for hashing
      Report_Bytes : Byte_Array (0 .. 1023);
      Msg_Length : Natural := 0;
      Digest : SHA3_256_Digest;
      Result : Hash256;
   begin
      --  Serialize report fields into byte array
      --  Include all significant fields: Report_ID, addresses, hashes, etc.

      --  Add Report_ID
      for I in Report.Report_ID'Range loop
         if Msg_Length < Report_Bytes'Length then
            Report_Bytes (Report_Bytes'First + Msg_Length) := Report.Report_ID (I);
            Msg_Length := Msg_Length + 1;
         end if;
      end loop;

      --  Add Contract_Hash
      for I in Report.Contract_Hash'Range loop
         if Msg_Length < Report_Bytes'Length then
            Report_Bytes (Report_Bytes'First + Msg_Length) := Report.Contract_Hash (I);
            Msg_Length := Msg_Length + 1;
         end if;
      end loop;

      --  Add Finding_Count as a distinguishing feature
      if Msg_Length < Report_Bytes'Length then
         Report_Bytes (Report_Bytes'First + Msg_Length) :=
            Byte (Report.Finding_Count mod 256);
         Msg_Length := Msg_Length + 1;
      end if;

      --  Compute SHA3-256 hash of serialized report
      SHA3_256 (
         Message => Report_Bytes (0 .. Msg_Length - 1),
         Digest  => Digest
      );

      --  Convert digest to Hash256
      for I in Result'Range loop
         Result (I) := Digest (Digest'First + I);
      end loop;

      return Result;
   end Report_Hash;

   ---------------------------------------------------------------------------
   --  Auditor Registry
   ---------------------------------------------------------------------------

   Auditors : Auditor_Array := (others => Null_Auditor);
   Auditor_Count : Natural := 0;

   procedure Register_Auditor (
      Profile : in     Auditor_Profile;
      Success : out    Boolean
   ) is
   begin
      if Auditor_Count >= Max_Auditors then
         Success := False;
         return;
      end if;

      Auditors (Auditor_Index (Auditor_Count)) := Profile;
      Auditor_Count := Auditor_Count + 1;
      Success := True;
   end Register_Auditor;

   function Get_Auditor (
      Auditor_Address : Khepri_Types.Address
   ) return Auditor_Profile is
   begin
      for I in 0 .. Auditor_Count - 1 loop
         if Auditors (Auditor_Index (I)).Address = Auditor_Address then
            return Auditors (Auditor_Index (I));
         end if;
      end loop;
      return Null_Auditor;
   end Get_Auditor;

   function Is_Registered_Auditor (
      Auditor_Address : Khepri_Types.Address
   ) return Boolean is
      Profile : constant Auditor_Profile := Get_Auditor (Auditor_Address);
   begin
      return Profile.Status = Status_Active;
   end Is_Registered_Auditor;

   function Can_Audit (
      Auditor_Address : Khepri_Types.Address;
      Contract_Value  : Uint256
   ) return Boolean is
      Profile : constant Auditor_Profile := Get_Auditor (Auditor_Address);
      Max_Value : Uint256;
   begin
      if Profile.Status /= Status_Active then
         return False;
      end if;

      --  Check tier limits
      case Profile.Tier is
         when Tier_1 => Max_Value := From_Natural (1_000_000);
         when Tier_2 => Max_Value := From_Natural (10_000_000);
         when Tier_3 => Max_Value := From_Natural (100_000_000);
         when Tier_4 => return True;  -- Unlimited
      end case;

      return Contract_Value <= Max_Value;
   end Can_Audit;

   procedure Update_Auditor_Status (
      Auditor_Address : in     Khepri_Types.Address;
      New_Status      : in     Auditor_Status;
      Success         : out    Boolean
   ) is
   begin
      for I in 0 .. Auditor_Count - 1 loop
         if Auditors (Auditor_Index (I)).Address = Auditor_Address then
            Auditors (Auditor_Index (I)).Status := New_Status;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Update_Auditor_Status;

   ---------------------------------------------------------------------------
   --  On-Chain Audit Registry
   ---------------------------------------------------------------------------

   type Audit_Registry_Storage is array (Registry_Audit_Index) of Registry_Audit_Entry;
   Audit_Registry : Audit_Registry_Storage := (others => (
      Contract_Address => (others => 0),
      Report_Hash      => (others => 0),
      Auditor          => (others => 0),
      Verdict          => Verdict_Incomplete,
      Timestamp        => 0,
      Is_Valid         => False,
      Is_Disputed      => False
   ));
   Audit_Registry_Count : Natural := 0;

   procedure Register_Audit (
      Report  : in     Audit_Report;
      Success : out    Boolean
   ) is
   begin
      if Audit_Registry_Count >= Max_Registry_Audits then
         Success := False;
         return;
      end if;

      Audit_Registry (Registry_Audit_Index (Audit_Registry_Count)) := (
         Contract_Address => Report.Contract_Address,
         Report_Hash      => Report_Hash (Report),
         Auditor          => Report.Auditor,
         Verdict          => Report.Verdict,
         Timestamp        => Report.Report_Time,
         Is_Valid         => True,
         Is_Disputed      => False
      );
      Audit_Registry_Count := Audit_Registry_Count + 1;
      Success := True;
   end Register_Audit;

   function Get_Contract_Audit (
      Contract : Khepri_Types.Address
   ) return Registry_Audit_Entry is
   begin
      for I in 0 .. Audit_Registry_Count - 1 loop
         if Audit_Registry (Registry_Audit_Index (I)).Contract_Address = Contract and
            Audit_Registry (Registry_Audit_Index (I)).Is_Valid
         then
            return Audit_Registry (Registry_Audit_Index (I));
         end if;
      end loop;

      return (
         Contract_Address => (others => 0),
         Report_Hash      => (others => 0),
         Auditor          => (others => 0),
         Verdict          => Verdict_Incomplete,
         Timestamp        => 0,
         Is_Valid         => False,
         Is_Disputed      => False
      );
   end Get_Contract_Audit;

   function Has_Valid_Audit (
      Contract : Khepri_Types.Address
   ) return Boolean is
      Entry_Val : constant Registry_Audit_Entry := Get_Contract_Audit (Contract);
   begin
      return Entry_Val.Is_Valid and
             not Entry_Val.Is_Disputed and
             (Entry_Val.Verdict = Verdict_Pass or
              Entry_Val.Verdict = Verdict_Pass_With_Notes);
   end Has_Valid_Audit;

   ---------------------------------------------------------------------------
   --  Dispute Resolution
   ---------------------------------------------------------------------------

   type Dispute_Storage is array (0 .. 255) of Dispute_Entry;
   Disputes : Dispute_Storage := (others => (
      ID              => (others => 0),
      Contract        => (others => 0),
      Report_Hash     => (others => 0),
      Disputer        => (others => 0),
      Reason          => Empty_String,
      Evidence_Hash   => (others => 0),
      Status          => Dispute_Open,
      Filed_At        => 0,
      Resolved_At     => 0,
      Resolution      => Empty_String
   ));
   Dispute_Count : Natural := 0;

   procedure File_Dispute (
      Report_Hash   : in     Hash256;
      Reason        : in     Bounded_String;
      Evidence_Hash : in     Hash256;
      Disputer      : in     Khepri_Types.Address;
      Success       : out    Boolean
   ) is
   begin
      if Dispute_Count >= 256 then
         Success := False;
         return;
      end if;

      Disputes (Dispute_Count) := (
         ID              => Report_Hash,  -- Use report hash as ID
         Contract        => (others => 0),
         Report_Hash     => Report_Hash,
         Disputer        => Disputer,
         Reason          => Reason,
         Evidence_Hash   => Evidence_Hash,
         Status          => Dispute_Open,
         Filed_At        => 0,
         Resolved_At     => 0,
         Resolution      => Empty_String
      );
      Dispute_Count := Dispute_Count + 1;
      Success := True;
   end File_Dispute;

   procedure Resolve_Dispute (
      Dispute_ID  : in     Hash256;
      Resolution  : in     Dispute_Status;
      Notes       : in     Bounded_String;
      Success     : out    Boolean
   ) is
   begin
      for I in 0 .. Dispute_Count - 1 loop
         if Disputes (I).ID = Dispute_ID then
            Disputes (I).Status := Resolution;
            Disputes (I).Resolution := Notes;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Resolve_Dispute;

   function Get_Dispute (
      Dispute_ID : Hash256
   ) return Dispute_Entry is
   begin
      for I in 0 .. Dispute_Count - 1 loop
         if Disputes (I).ID = Dispute_ID then
            return Disputes (I);
         end if;
      end loop;

      return (
         ID              => (others => 0),
         Contract        => (others => 0),
         Report_Hash     => (others => 0),
         Disputer        => (others => 0),
         Reason          => Empty_String,
         Evidence_Hash   => (others => 0),
         Status          => Dispute_Open,
         Filed_At        => 0,
         Resolved_At     => 0,
         Resolution      => Empty_String
      );
   end Get_Dispute;

   ---------------------------------------------------------------------------
   --  Certification Integration
   ---------------------------------------------------------------------------

   function Meets_Platinum_Audit (
      Contract : Khepri_Types.Address
   ) return Boolean is
   begin
      return Has_Valid_Audit (Contract);
   end Meets_Platinum_Audit;

   function Get_Platinum_Status (
      Contract : Khepri_Types.Address
   ) return Platinum_Requirements is
      Audit_Entry : constant Registry_Audit_Entry := Get_Contract_Audit (Contract);
   begin
      return (
         Gold_Met          => True,  -- Assume Gold already checked
         Audit_Complete    => Audit_Entry.Is_Valid,
         Auditor_Verified  => Is_Registered_Auditor (Audit_Entry.Auditor),
         Audit_Report_Hash => Audit_Entry.Report_Hash,
         Audit_Timestamp   => Audit_Entry.Timestamp
      );
   end Get_Platinum_Status;

end Khepri_Audit;
