pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_Types;

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
      --  Bronze certification requires flow analysis clean:
      --  The contract bytecode must include embedded SPARK metadata
      --  that certifies the following:
      --  1. SPARK_Mode (On) pragma present in all units
      --  2. GNATprove flow analysis passed (no flow errors)
      --  3. Clean compilation (no warnings/errors)
      --  4. No uninitialized reads
      --  5. No illegal aliasing

      if Contract_Code'Length = 0 then
         return Reqs;
      end if;

      --  Check for SPARK magic marker in bytecode metadata section
      --  Contract binaries embed a metadata section with certification info
      --  Format: [MAGIC:4][VERSION:4][FLAGS:4][CERT_LEVEL:4][PROOF_HASH:32]...
      if Contract_Code'Length >= 16 then
         --  Check magic number "SPRK" = 0x53 50 52 4B
         if Contract_Code (Contract_Code'First) = 16#53# and
            Contract_Code (Contract_Code'First + 1) = 16#50# and
            Contract_Code (Contract_Code'First + 2) = 16#52# and
            Contract_Code (Contract_Code'First + 3) = 16#4B#
         then
            Reqs.SPARK_Mode_Enabled := True;

            --  Extract certification flags from metadata (byte 8)
            --  Bit 0: Flow analysis clean
            --  Bit 1: No runtime errors
            --  Bit 2: No uninitialized reads
            --  Bit 3: No illegal aliasing
            --  Bit 4: Compiles clean
            if Contract_Code'Length >= 12 then
               declare
                  Flags : constant Byte := Contract_Code (Contract_Code'First + 8);
               begin
                  Reqs.No_Runtime_Errors      := (Flags and 16#01#) /= 0;
                  Reqs.No_Uninitialized_Reads := (Flags and 16#02#) /= 0;
                  Reqs.No_Illegal_Aliasing    := (Flags and 16#04#) /= 0;
                  Reqs.Compiles_Clean         := (Flags and 16#08#) /= 0;
               end;
            end if;
         end if;
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

      if Reqs.Bronze_Met and Contract_Code'Length >= 16 then
         --  Silver certification requires full proof discharge:
         --  Extract Silver certification flags from metadata
         --  Metadata format (after Bronze section):
         --  [SILVER_FLAGS:4][VC_TOTAL:4][VC_PROVED:4][WCET_BOUNDS:4]

         if Contract_Code'Length >= 28 then
            declare
               Silver_Flags : constant Byte := Contract_Code (Contract_Code'First + 16);
               VC_Total : Word32;
               VC_Proved : Word32;
            begin
               --  Silver flags:
               --  Bit 0: All proofs discharged
               --  Bit 1: WCET analyzed
               --  Bit 2: WCET bounded
               --  Bit 3: No unchecked unions
               Reqs.All_Proofs_Discharged := (Silver_Flags and 16#01#) /= 0;
               Reqs.WCET_Analyzed         := (Silver_Flags and 16#02#) /= 0;
               Reqs.WCET_Bounded          := (Silver_Flags and 16#04#) /= 0;
               Reqs.No_Unchecked_Unions   := (Silver_Flags and 16#08#) /= 0;

               --  Extract VC statistics (bytes 20-27)
               if Contract_Code'Length >= 28 then
                  VC_Total := Word32 (Contract_Code (Contract_Code'First + 20)) or
                              Word32 (Contract_Code (Contract_Code'First + 21)) * 256 or
                              Word32 (Contract_Code (Contract_Code'First + 22)) * 65536 or
                              Word32 (Contract_Code (Contract_Code'First + 23)) * 16777216;

                  VC_Proved := Word32 (Contract_Code (Contract_Code'First + 24)) or
                               Word32 (Contract_Code (Contract_Code'First + 25)) * 256 or
                               Word32 (Contract_Code (Contract_Code'First + 26)) * 65536 or
                               Word32 (Contract_Code (Contract_Code'First + 27)) * 16777216;

                  --  Verify that all VCs are proved (100%)
                  if VC_Total > 0 and VC_Total = VC_Proved then
                     Reqs.All_Proofs_Discharged := Reqs.All_Proofs_Discharged and True;
                  else
                     Reqs.All_Proofs_Discharged := False;
                  end if;
               end if;
            end;
         end if;
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

      if Reqs.Silver_Met and Contract_Code'Length >= 32 then
         --  Gold certification requires constant-time and security properties:
         --  Extract Gold certification flags from metadata
         --  Metadata format (after Silver section):
         --  [GOLD_FLAGS:4][CT_VIOLATIONS:4][SEC_PROPS:4][FUNC_CORRECT:4]

         if Contract_Code'Length >= 44 then
            declare
               Gold_Flags : constant Byte := Contract_Code (Contract_Code'First + 32);
               CT_Violations : Word32;
               Security_Props_Checked : Word32;
            begin
               --  Gold flags:
               --  Bit 0: Constant-time verified
               --  Bit 1: Security properties proven
               --  Bit 2: No timing channels
               --  Bit 3: Functional correctness proven
               Reqs.Constant_Time_Verified := (Gold_Flags and 16#01#) /= 0;
               Reqs.Security_Props_Proven  := (Gold_Flags and 16#02#) /= 0;
               Reqs.No_Timing_Channels     := (Gold_Flags and 16#04#) /= 0;
               Reqs.Functional_Correctness := (Gold_Flags and 16#08#) /= 0;

               --  Extract CT analysis results (bytes 36-39)
               if Contract_Code'Length >= 40 then
                  CT_Violations := Word32 (Contract_Code (Contract_Code'First + 36)) or
                                   Word32 (Contract_Code (Contract_Code'First + 37)) * 256 or
                                   Word32 (Contract_Code (Contract_Code'First + 38)) * 65536 or
                                   Word32 (Contract_Code (Contract_Code'First + 39)) * 16777216;

                  --  CT must have zero critical violations
                  if CT_Violations /= 0 then
                     Reqs.Constant_Time_Verified := False;
                     Reqs.No_Timing_Channels := False;
                  end if;
               end if;

               --  Extract security properties count (bytes 40-43)
               if Contract_Code'Length >= 44 then
                  Security_Props_Checked := Word32 (Contract_Code (Contract_Code'First + 40)) or
                                            Word32 (Contract_Code (Contract_Code'First + 41)) * 256 or
                                            Word32 (Contract_Code (Contract_Code'First + 42)) * 65536 or
                                            Word32 (Contract_Code (Contract_Code'First + 43)) * 16777216;

                  --  Must have checked at least one security property
                  if Security_Props_Checked = 0 then
                     Reqs.Security_Props_Proven := False;
                  end if;
               end if;
            end;
         end if;
      end if;

      return Reqs;
   end Check_Gold;

   function Verify_Certification (
      Contract_Code : Byte_Array
   ) return Certification_Status is
      use Anubis_SHA3;
      Status : Certification_Status := Uncertified_Status;
      Bronze : Bronze_Requirements;
      Silver : Silver_Requirements;
      Gold   : Gold_Requirements;
      Code_Digest : SHA3_256_Digest;
      Proof_Digest : SHA3_256_Digest;
      Temp_Array : Anubis_Types.Byte_Array (0 .. Contract_Code'Length - 1);
   begin
      --  Step 1: Compute contract hash for proof binding
      for I in 0 .. Contract_Code'Length - 1 loop
         Temp_Array (I) := Anubis_Types.Byte (Contract_Code (Contract_Code'First + I));
      end loop;
      SHA3_256 (Message => Temp_Array, Digest => Code_Digest);

      --  Copy contract hash
      for I in Status.Contract_Hash'Range loop
         Status.Contract_Hash (I) := Code_Digest (I);
      end loop;

      --  Step 2: Check each certification level
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

      --  Step 3: Calculate final certification level
      Status.Level := Calculate_Level (
         Status.Bronze_Reqs,
         Status.Silver_Reqs,
         Status.Gold_Reqs,
         Status.Platinum_Reqs
      );

      --  Step 4: Generate proof artifact hash
      --  The proof artifact combines: contract hash + certification level + flags
      declare
         Proof_Data : Anubis_Types.Byte_Array (0 .. 63);
         Idx : Natural := 0;
      begin
         --  Include contract hash
         for I in 0 .. 31 loop
            Proof_Data (Idx) := Code_Digest (I);
            Idx := Idx + 1;
         end loop;

         --  Include certification level (1 byte)
         Proof_Data (Idx) := Anubis_Types.Byte (Certification_Level'Pos (Status.Level));
         Idx := Idx + 1;

         --  Include Bronze flags
         Proof_Data (Idx) := (if Bronze.SPARK_Mode_Enabled then 16#01# else 0) or
                            (if Bronze.No_Runtime_Errors then 16#02# else 0) or
                            (if Bronze.No_Uninitialized_Reads then 16#04# else 0) or
                            (if Bronze.No_Illegal_Aliasing then 16#08# else 0) or
                            (if Bronze.Compiles_Clean then 16#10# else 0);
         Idx := Idx + 1;

         --  Include Silver flags
         Proof_Data (Idx) := (if Silver.Bronze_Met then 16#01# else 0) or
                            (if Silver.All_Proofs_Discharged then 16#02# else 0) or
                            (if Silver.WCET_Analyzed then 16#04# else 0) or
                            (if Silver.WCET_Bounded then 16#08# else 0);
         Idx := Idx + 1;

         --  Include Gold flags
         Proof_Data (Idx) := (if Gold.Silver_Met then 16#01# else 0) or
                            (if Gold.Constant_Time_Verified then 16#02# else 0) or
                            (if Gold.Security_Props_Proven then 16#04# else 0) or
                            (if Gold.No_Timing_Channels then 16#08# else 0);
         Idx := Idx + 1;

         --  Compute proof artifact hash
         SHA3_256 (Message => Proof_Data (0 .. Idx - 1), Digest => Proof_Digest);

         --  Copy proof hash
         for I in Status.Proof_Artifact'Range loop
            Status.Proof_Artifact (I) := Proof_Digest (I);
         end loop;
      end;

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
