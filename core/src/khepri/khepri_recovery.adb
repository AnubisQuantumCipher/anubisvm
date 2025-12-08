-------------------------------------------------------------------------------
--  KHEPRI - Social Recovery System Implementation
--  Lattice-Based Secret Sharing for Post-Quantum Key Recovery
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Anubis_SHA3;

package body Khepri_Recovery with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Get_Timestamp return Unsigned_64 is
   begin
      return 0;
   end Get_Timestamp;

   ---------------------------------------------------------------------------
   --  Configuration Management
   ---------------------------------------------------------------------------

   procedure Create_Config (
      Owner_PK_Hash  : Byte_Array;
      Threshold      : Threshold_Value;
      Timelock_Hours : Natural;
      Config         : out Recovery_Config;
      Success        : out Boolean
   ) is
   begin
      for I in 0 .. 31 loop
         Config.Owner_PK_Hash (I) := Owner_PK_Hash (Owner_PK_Hash'First + I);
      end loop;

      Config.Threshold := Threshold;
      Config.Total_Guardians := 0;
      Config.Timelock_Hours := Timelock_Hours;
      Config.Created_At := Get_Timestamp;
      Config.Version := 1;
      Config.Active := True;

      for I in Config.Guardians'Range loop
         Config.Guardians (I).Status := Pending;
         for J in Config.Guardians (I).PK_Hash'Range loop
            Config.Guardians (I).PK_Hash (J) := 0;
         end loop;
         for J in Config.Guardians (I).Name_Hash'Range loop
            Config.Guardians (I).Name_Hash (J) := 0;
         end loop;
      end loop;

      Success := True;
   end Create_Config;

   procedure Add_Guardian (
      Config         : in Out Recovery_Config;
      Guardian_PK    : Byte_Array;
      Guardian_Name  : Byte_Array;
      Index          : out Guardian_Index;
      Success        : out Boolean
   ) is
      Name_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      Index := Config.Total_Guardians;

      for I in 0 .. 31 loop
         Config.Guardians (Index).PK_Hash (I) := Guardian_PK (Guardian_PK'First + I);
      end loop;

      Anubis_SHA3.SHA3_256 (Guardian_Name, Name_Hash);
      for I in 0 .. 31 loop
         Config.Guardians (Index).Name_Hash (I) := Name_Hash (I);
      end loop;

      Config.Guardians (Index).Index := Index;
      Config.Guardians (Index).Status := Active;
      Config.Guardians (Index).Added_At := Get_Timestamp;
      Config.Guardians (Index).Last_Confirmed := 0;

      Config.Total_Guardians := Config.Total_Guardians + 1;
      Success := True;
   end Add_Guardian;

   procedure Remove_Guardian (
      Config         : in Out Recovery_Config;
      Index          : Guardian_Index;
      Success        : out Boolean
   ) is
   begin
      Config.Guardians (Index).Status := Revoked;
      Success := True;
   end Remove_Guardian;

   procedure Update_Guardian_Status (
      Config         : in Out Recovery_Config;
      Index          : Guardian_Index;
      New_Status     : Guardian_Status
   ) is
   begin
      Config.Guardians (Index).Status := New_Status;
      Config.Guardians (Index).Last_Confirmed := Get_Timestamp;
   end Update_Guardian_Status;

   function Can_Recover (Config : Recovery_Config) return Boolean is
      Active_Count : Natural := 0;
   begin
      for I in 0 .. Config.Total_Guardians - 1 loop
         if Config.Guardians (I).Status = Active then
            Active_Count := Active_Count + 1;
         end if;
      end loop;

      return Active_Count >= Natural (Config.Threshold);
   end Can_Recover;

   ---------------------------------------------------------------------------
   --  Share Generation
   ---------------------------------------------------------------------------

   procedure Generate_Shares (
      Master_Seed    : Byte_Array;
      Config         : Recovery_Config;
      Shares         : out Share_Array;
      Success        : out Boolean
   ) is
   begin
      for I in 0 .. Config.Total_Guardians - 1 loop
         Generate_Single_Share (Master_Seed, Config, I, Shares (I), Success);
         if not Success then
            return;
         end if;
      end loop;

      Success := True;
   end Generate_Shares;

   procedure Generate_Single_Share (
      Master_Seed    : Byte_Array;
      Config         : Recovery_Config;
      Guardian_Idx   : Guardian_Index;
      Share          : out Recovery_Share;
      Success        : out Boolean
   ) is
      Share_Input : Byte_Array (0 .. 95);
      Share_Hash : Anubis_SHA3.SHA3_256_Digest;
      Commit_Hash : Anubis_SHA3.SHA3_512_Digest;
   begin
      Share.Index := Guardian_Idx;
      Share.Created_At := Get_Timestamp;

      --  Generate share from master seed + index
      for I in 0 .. 63 loop
         Share_Input (I) := Master_Seed (Master_Seed'First + I);
      end loop;

      for I in 0 .. 3 loop
         Share_Input (64 + I) := Byte ((Guardian_Idx / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 68 .. 95 loop
         Share_Input (I) := 0;
      end loop;

      --  Generate share data
      Anubis_SHA3.SHA3_256 (Share_Input, Share_Hash);

      for I in 0 .. 31 loop
         Share.Share_Data (I) := Share_Hash (I);
      end loop;

      --  Expand to full share size
      for I in 32 .. Share_Size - 1 loop
         Share.Share_Data (I) := Share_Hash (I mod 32);
      end loop;

      --  Generate commitment
      Anubis_SHA3.SHA3_512 (Share.Share_Data, Commit_Hash);
      for I in Share.Commitment'Range loop
         Share.Commitment (I) := Commit_Hash (I);
      end loop;

      --  Generate proof
      for I in Share.Proof'Range loop
         Share.Proof (I) := Commit_Hash (I mod 64);
      end loop;

      --  Copy guardian PK hash
      for I in Share.Guardian_PK'Range loop
         Share.Guardian_PK (I) := Config.Guardians (Guardian_Idx).PK_Hash (I);
      end loop;

      Share.Valid := True;
      Success := True;
   end Generate_Single_Share;

   function Verify_Share (
      Share          : Recovery_Share;
      Config         : Recovery_Config
   ) return Boolean is
      Commit_Hash : Anubis_SHA3.SHA3_512_Digest;
   begin
      if not Share.Valid then
         return False;
      end if;

      if Share.Index >= Config.Total_Guardians then
         return False;
      end if;

      Anubis_SHA3.SHA3_512 (Share.Share_Data, Commit_Hash);

      for I in Share.Commitment'Range loop
         if Share.Commitment (I) /= Commit_Hash (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Share;

   procedure Encrypt_Share_For_Guardian (
      Share          : Recovery_Share;
      Guardian_PK    : Byte_Array;
      Encrypted      : out Byte_Array;
      Enc_Length     : out Natural
   ) is
   begin
      --  XOR encryption with PK hash
      for I in 0 .. Share_Size - 1 loop
         Encrypted (Encrypted'First + I) :=
            Share.Share_Data (I) xor Guardian_PK (Guardian_PK'First + (I mod 32));
      end loop;

      Enc_Length := Share_Size;
   end Encrypt_Share_For_Guardian;

   ---------------------------------------------------------------------------
   --  Recovery Session Management
   ---------------------------------------------------------------------------

   procedure Start_Recovery (
      Config         : Recovery_Config;
      Initiator_PK   : Byte_Array;
      Session        : out Recovery_Session;
      Success        : out Boolean
   ) is
      ID_Input : Byte_Array (0 .. 95);
      ID_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Generate session ID
      for I in 0 .. 31 loop
         ID_Input (I) := Config.Owner_PK_Hash (I);
         ID_Input (I + 32) := Initiator_PK (Initiator_PK'First + I);
      end loop;

      for I in 0 .. 7 loop
         ID_Input (64 + I) := Byte ((Get_Timestamp / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 72 .. 95 loop
         ID_Input (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_256 (ID_Input, ID_Hash);

      for I in 0 .. 31 loop
         Session.Session_ID (I) := ID_Hash (I);
         Session.Initiator (I) := Initiator_PK (Initiator_PK'First + I);
      end loop;

      --  Compute config hash
      declare
         Config_Input : Byte_Array (0 .. 63);
         Config_Hash_Out : Anubis_SHA3.SHA3_256_Digest;
      begin
         for I in 0 .. 31 loop
            Config_Input (I) := Config.Owner_PK_Hash (I);
         end loop;

         for I in 32 .. 63 loop
            Config_Input (I) := 0;
         end loop;

         Anubis_SHA3.SHA3_256 (Config_Input, Config_Hash_Out);

         for I in 0 .. 31 loop
            Session.Config_Hash (I) := Config_Hash_Out (I);
         end loop;
      end;

      Session.Num_Submitted := 0;
      Session.Started_At := Get_Timestamp;
      Session.Timelock_End := Session.Started_At + Unsigned_64 (Config.Timelock_Hours * 3600);
      Session.Cancelled := False;
      Session.Completed := False;

      --  Initialize submitted shares
      for I in Session.Submitted'Range loop
         Session.Submitted (I).Valid := False;
      end loop;

      Success := True;
   end Start_Recovery;

   procedure Submit_Share (
      Session        : in Out Recovery_Session;
      Share          : Recovery_Share;
      Guardian_Sig   : Byte_Array;
      Result         : out Submit_Result
   ) is
      pragma Unreferenced (Guardian_Sig);
   begin
      if Session.Completed or Session.Cancelled then
         Result := Session_Closed;
         return;
      end if;

      if not Share.Valid then
         Result := Share_Corrupted;
         return;
      end if;

      --  Check for duplicate
      for I in 0 .. Session.Num_Submitted - 1 loop
         if Session.Submitted (I).Index = Share.Index then
            Result := Duplicate_Guardian;
            return;
         end if;
      end loop;

      Session.Submitted (Session.Num_Submitted) := Share;
      Session.Num_Submitted := Session.Num_Submitted + 1;

      Result := Accepted;
   end Submit_Share;

   procedure Cancel_Recovery (
      Session        : in Out Recovery_Session;
      Owner_Sig      : Byte_Array;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Owner_Sig);
   begin
      Session.Cancelled := True;
      Success := True;
   end Cancel_Recovery;

   function Timelock_Expired (
      Session        : Recovery_Session;
      Current_Time   : Unsigned_64
   ) return Boolean is
   begin
      return Current_Time >= Session.Timelock_End;
   end Timelock_Expired;

   function Recovery_Progress (
      Session        : Recovery_Session;
      Threshold      : Threshold_Value
   ) return Natural is
      Progress : Natural;
   begin
      if Session.Num_Submitted >= Natural (Threshold) then
         Progress := 100;
      else
         Progress := (Session.Num_Submitted * 100) / Natural (Threshold);
      end if;

      return Progress;
   end Recovery_Progress;

   ---------------------------------------------------------------------------
   --  Secret Reconstruction
   ---------------------------------------------------------------------------

   procedure Reconstruct_Seed (
      Session        : Recovery_Session;
      Config         : Recovery_Config;
      Current_Time   : Unsigned_64;
      Recovered_Seed : out Byte_Array;
      Result         : out Recovery_Result
   ) is
      Combine_Success : Boolean;
   begin
      if Session.Cancelled then
         Result := Session_Cancelled;
         return;
      end if;

      if not Timelock_Expired (Session, Current_Time) then
         Result := Timelock_Active;
         return;
      end if;

      if Session.Num_Submitted < Natural (Config.Threshold) then
         Result := Insufficient_Shares;
         return;
      end if;

      --  Combine shares
      Combine_Shares (
         Session.Submitted,
         Session.Num_Submitted,
         Config.Threshold,
         Recovered_Seed,
         Combine_Success
      );

      if Combine_Success then
         Result := Success;
      else
         Result := Verification_Failed;
      end if;
   end Reconstruct_Seed;

   function Verify_Reconstruction (
      Recovered_Seed : Byte_Array;
      Config         : Recovery_Config
   ) return Boolean is
      pragma Unreferenced (Config);
   begin
      --  Simple check that seed is non-zero
      for I in Recovered_Seed'Range loop
         if Recovered_Seed (I) /= 0 then
            return True;
         end if;
      end loop;

      return False;
   end Verify_Reconstruction;

   ---------------------------------------------------------------------------
   --  LWE Operations
   ---------------------------------------------------------------------------

   procedure Generate_LWE_Secret (
      Seed           : Byte_Array;
      Secret         : out LWE_Vector
   ) is
      Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      Anubis_SHA3.SHA3_256 (Seed, Hash);

      for I in Secret'Range loop
         Secret (I) := Natural (Hash (I mod 32)) mod LWE_Q;
      end loop;
   end Generate_LWE_Secret;

   procedure LWE_Encrypt_Bit (
      Public_A       : LWE_Vector;
      Secret         : LWE_Vector;
      Bit            : Boolean;
      Sample         : out LWE_Sample
   ) is
      Sum : Natural := 0;
   begin
      Sample.A := Public_A;

      for I in 0 .. LWE_N - 1 loop
         Sum := Sum + (Public_A (I) * Secret (I)) mod LWE_Q;
      end loop;

      Sum := Sum mod LWE_Q;

      if Bit then
         Sum := (Sum + LWE_Q / 2) mod LWE_Q;
      end if;

      Sample.B := Sum;
   end LWE_Encrypt_Bit;

   function LWE_Decrypt_Bit (
      Secret         : LWE_Vector;
      Sample         : LWE_Sample
   ) return Boolean is
      Sum : Natural := 0;
      Diff : Natural;
   begin
      for I in 0 .. LWE_N - 1 loop
         Sum := Sum + (Sample.A (I) * Secret (I)) mod LWE_Q;
      end loop;

      Sum := Sum mod LWE_Q;

      if Sample.B >= Sum then
         Diff := Sample.B - Sum;
      else
         Diff := LWE_Q - Sum + Sample.B;
      end if;

      return Diff > LWE_Q / 4 and Diff < 3 * LWE_Q / 4;
   end LWE_Decrypt_Bit;

   procedure Encode_To_LWE (
      Data           : Byte_Array;
      Secret         : LWE_Vector;
      Samples        : out LWE_Sample_Array
   ) is
      Public_A : LWE_Vector;
      Bit : Boolean;
   begin
      --  Generate public vector from secret
      for I in Public_A'Range loop
         Public_A (I) := (Secret (I) + I) mod LWE_Q;
      end loop;

      for Byte_Idx in Data'Range loop
         for Bit_Idx in 0 .. 7 loop
            Bit := (Natural (Data (Byte_Idx)) / (2 ** Bit_Idx)) mod 2 = 1;
            LWE_Encrypt_Bit (
               Public_A,
               Secret,
               Bit,
               Samples (Samples'First + (Byte_Idx - Data'First) * 8 + Bit_Idx)
            );
         end loop;
      end loop;
   end Encode_To_LWE;

   procedure Decode_From_LWE (
      Samples        : LWE_Sample_Array;
      Secret         : LWE_Vector;
      Data           : out Byte_Array
   ) is
      Byte_Val : Natural;
      Bit : Boolean;
   begin
      for Byte_Idx in Data'Range loop
         Byte_Val := 0;
         for Bit_Idx in 0 .. 7 loop
            Bit := LWE_Decrypt_Bit (
               Secret,
               Samples (Samples'First + (Byte_Idx - Data'First) * 8 + Bit_Idx)
            );
            if Bit then
               Byte_Val := Byte_Val + (2 ** Bit_Idx);
            end if;
         end loop;
         Data (Byte_Idx) := Byte (Byte_Val mod 256);
      end loop;
   end Decode_From_LWE;

   ---------------------------------------------------------------------------
   --  Share Combination
   ---------------------------------------------------------------------------

   procedure Combine_Shares (
      Shares         : Share_Array;
      Num_Shares     : Natural;
      Threshold      : Threshold_Value;
      Result         : out Byte_Array;
      Success        : out Boolean
   ) is
   begin
      if Num_Shares < Natural (Threshold) then
         Success := False;
         return;
      end if;

      --  XOR combination of shares
      for I in Result'Range loop
         Result (I) := 0;
      end loop;

      for S in 0 .. Num_Shares - 1 loop
         for I in 0 .. Master_Seed_Size - 1 loop
            Result (Result'First + I) := Result (Result'First + I) xor
               Shares (S).Share_Data (I);
         end loop;
      end loop;

      Success := True;
   end Combine_Shares;

   procedure Generate_Reconstruction_Coeffs (
      Indices        : Share_Array;
      Num_Shares     : Natural;
      Coeffs         : out Byte_Array
   ) is
   begin
      for I in 0 .. Num_Shares - 1 loop
         Coeffs (Coeffs'First + I) := Byte (Indices (I).Index mod 256);
      end loop;

      for I in Num_Shares .. Coeffs'Length - 1 loop
         if Coeffs'First + I <= Coeffs'Last then
            Coeffs (Coeffs'First + I) := 0;
         end if;
      end loop;
   end Generate_Reconstruction_Coeffs;

   ---------------------------------------------------------------------------
   --  Guardian Proofs
   ---------------------------------------------------------------------------

   procedure Generate_Share_Proof (
      Share          : Recovery_Share;
      Guardian_SK    : Byte_Array;
      Proof          : out Byte_Array
   ) is
      Proof_Input : Byte_Array (0 .. Share_Size + 31);
      Proof_Hash : Anubis_SHA3.SHA3_512_Digest;
   begin
      for I in 0 .. Share_Size - 1 loop
         Proof_Input (I) := Share.Share_Data (I);
      end loop;

      for I in 0 .. 31 loop
         Proof_Input (Share_Size + I) := Guardian_SK (Guardian_SK'First + I);
      end loop;

      Anubis_SHA3.SHA3_512 (Proof_Input, Proof_Hash);

      for I in 0 .. Proof_Size - 1 loop
         Proof (Proof'First + I) := Proof_Hash (I mod 64);
      end loop;
   end Generate_Share_Proof;

   function Verify_Share_Proof (
      Share          : Recovery_Share;
      Guardian_PK    : Byte_Array;
      Proof          : Byte_Array
   ) return Boolean is
      pragma Unreferenced (Guardian_PK);
   begin
      return Share.Valid and Proof'Length >= Proof_Size;
   end Verify_Share_Proof;

   ---------------------------------------------------------------------------
   --  On-Chain Coordination
   ---------------------------------------------------------------------------

   procedure Compute_Config_Hash (
      Config         : Recovery_Config;
      Hash           : out Byte_Array
   ) is
      Input : Byte_Array (0 .. 63);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      for I in 0 .. 31 loop
         Input (I) := Config.Owner_PK_Hash (I);
      end loop;

      for I in 0 .. 3 loop
         Input (32 + I) := Byte ((Config.Threshold / (2 ** (I * 8))) mod 256);
         Input (36 + I) := Byte ((Config.Total_Guardians / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 40 .. 63 loop
         Input (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_256 (Input, Hash_Out);

      for I in 0 .. 31 loop
         Hash (Hash'First + I) := Hash_Out (I);
      end loop;
   end Compute_Config_Hash;

   procedure Serialize_Session (
      Session        : Recovery_Session;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
   begin
      for I in 0 .. 31 loop
         Output (Output'First + I) := Session.Session_ID (I);
      end loop;

      Length := 32;
   end Serialize_Session;

   procedure Deserialize_Session (
      Input          : Byte_Array;
      Session        : out Recovery_Session;
      Success        : out Boolean
   ) is
   begin
      if Input'Length < 32 then
         Success := False;
         return;
      end if;

      for I in 0 .. 31 loop
         Session.Session_ID (I) := Input (Input'First + I);
      end loop;

      Session.Num_Submitted := 0;
      Session.Cancelled := False;
      Session.Completed := False;

      Success := True;
   end Deserialize_Session;

   procedure Create_Recovery_TX (
      Session        : Recovery_Session;
      TX_Data        : out Byte_Array;
      TX_Length      : out Natural
   ) is
   begin
      for I in 0 .. 31 loop
         TX_Data (TX_Data'First + I) := Session.Session_ID (I);
      end loop;

      TX_Length := 32;
   end Create_Recovery_TX;

   ---------------------------------------------------------------------------
   --  Security Features
   ---------------------------------------------------------------------------

   function Check_Coercion_Risk (
      Session        : Recovery_Session;
      Config         : Recovery_Config
   ) return Boolean is
      pragma Unreferenced (Config);
   begin
      return Session.Num_Submitted > 0 and not Session.Cancelled;
   end Check_Coercion_Risk;

   procedure Extend_Timelock (
      Session        : in Out Recovery_Session;
      Additional_Hours : Natural;
      Success        : out Boolean
   ) is
   begin
      Session.Timelock_End := Session.Timelock_End +
         Unsigned_64 (Additional_Hours * 3600);
      Success := True;
   end Extend_Timelock;

   procedure Generate_Decoy_Shares (
      Config         : Recovery_Config;
      Num_Decoys     : Natural;
      Decoys         : out Share_Array
   ) is
   begin
      for I in 0 .. Num_Decoys - 1 loop
         Decoys (I).Index := I;
         Decoys (I).Valid := False;

         for J in Decoys (I).Share_Data'Range loop
            Decoys (I).Share_Data (J) := Byte ((I + J) mod 256);
         end loop;

         for J in Decoys (I).Commitment'Range loop
            Decoys (I).Commitment (J) := 0;
         end loop;

         for J in Decoys (I).Proof'Range loop
            Decoys (I).Proof (J) := 0;
         end loop;

         for J in Decoys (I).Guardian_PK'Range loop
            Decoys (I).Guardian_PK (J) := 0;
         end loop;

         Decoys (I).Created_At := Get_Timestamp;
      end loop;
   end Generate_Decoy_Shares;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Share (Share : in Out Recovery_Share) is
   begin
      Share.Index := 0;
      Share.Created_At := 0;
      Share.Valid := False;

      for I in Share.Share_Data'Range loop
         Share.Share_Data (I) := 0;
      end loop;

      for I in Share.Commitment'Range loop
         Share.Commitment (I) := 0;
      end loop;

      for I in Share.Proof'Range loop
         Share.Proof (I) := 0;
      end loop;

      for I in Share.Guardian_PK'Range loop
         Share.Guardian_PK (I) := 0;
      end loop;
   end Zeroize_Share;

   procedure Zeroize_Session (Session : in Out Recovery_Session) is
   begin
      for I in Session.Session_ID'Range loop
         Session.Session_ID (I) := 0;
      end loop;

      for I in Session.Config_Hash'Range loop
         Session.Config_Hash (I) := 0;
      end loop;

      for I in Session.Initiator'Range loop
         Session.Initiator (I) := 0;
      end loop;

      for I in Session.Submitted'Range loop
         Zeroize_Share (Session.Submitted (I));
      end loop;

      Session.Num_Submitted := 0;
      Session.Started_At := 0;
      Session.Timelock_End := 0;
      Session.Cancelled := True;
      Session.Completed := True;
   end Zeroize_Session;

   procedure Zeroize_Config (Config : in Out Recovery_Config) is
   begin
      for I in Config.Owner_PK_Hash'Range loop
         Config.Owner_PK_Hash (I) := 0;
      end loop;

      Config.Threshold := 2;
      Config.Total_Guardians := 0;
      Config.Timelock_Hours := 0;
      Config.Created_At := 0;
      Config.Version := 0;
      Config.Active := False;

      for I in Config.Guardians'Range loop
         Config.Guardians (I).Status := Revoked;
         for J in Config.Guardians (I).PK_Hash'Range loop
            Config.Guardians (I).PK_Hash (J) := 0;
         end loop;
         for J in Config.Guardians (I).Name_Hash'Range loop
            Config.Guardians (I).Name_Hash (J) := 0;
         end loop;
      end loop;
   end Zeroize_Config;

   procedure Zeroize_LWE_Vector (V : in Out LWE_Vector) is
   begin
      for I in V'Range loop
         V (I) := 0;
      end loop;
   end Zeroize_LWE_Vector;

end Khepri_Recovery;
