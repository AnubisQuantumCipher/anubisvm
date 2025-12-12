pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;

package body TEE_Attestation with
   SPARK_Mode => On
is

   --  Domain separator for report hashing
   Report_Domain : constant Byte_Array (0 .. 15) := (
      16#41#, 16#4E#, 16#55#, 16#42#, 16#49#, 16#53#, 16#2D#, 16#41#,
      16#54#, 16#54#, 16#45#, 16#53#, 16#54#, 16#2D#, 16#56#, 16#31#
   );  -- "ANUBIS-ATTEST-V1"

   --  Generate attestation report
   procedure Generate_Report (
      TEE_Code      : Measurement;
      TEE_Config    : Measurement;
      State_Root    : Measurement;
      CVM_Hashes    : CVM_Measurement_Array;
      CVM_Count     : Natural;
      Nonce         : Byte_Array;
      Attest_PK     : DSA_Public_Key;
      Report        : out Attestation_Report
   ) is
      PK_Hash : SHA3_256_Digest;
   begin
      Report := Empty_Report;

      --  Set version
      Report.Version := Report_Version;

      --  Set timestamp (caller should set real timestamp)
      Get_Timestamp (Report.Timestamp);

      --  Copy nonce
      for I in 0 .. 31 loop
         pragma Loop_Invariant (I >= 0);
         Report.Nonce (I) := Nonce (Nonce'First + I);
      end loop;

      --  Set TEE measurements
      Report.TEE_Code_Hash := TEE_Code;
      Report.TEE_Config_Hash := TEE_Config;
      Report.TEE_State_Root := State_Root;

      --  Copy CVM measurements
      Report.CVM_Count := CVM_Count;
      for I in 0 .. Max_CVM_Measurements - 1 loop
         pragma Loop_Invariant (I >= 0);
         Report.CVMs (I) := CVM_Hashes (I);
      end loop;

      --  Hash attestation public key
      SHA3_256 (Attest_PK, PK_Hash);
      Report.Attest_PK_Hash := PK_Hash;

      Report.Valid := True;
   end Generate_Report;

   --  Hash report for signing
   procedure Hash_Report (
      Report : Attestation_Report;
      Hash   : out Measurement
   ) is
      --  Build message to hash: domain || version || timestamp || nonce || measurements
      --  Estimated size: 16 + 4 + 8 + 32 + 32*3 + 32*Max_CVMs*2 ≈ 2200 bytes max
      Max_Msg_Size : constant := 4096;
      Msg : Byte_Array (0 .. Max_Msg_Size - 1) := (others => 0);
      Pos : Natural := 0;
   begin
      Hash := (others => 0);

      --  Copy domain separator
      for I in Report_Domain'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report_Domain (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Encode version (4 bytes LE)
      declare
         V : constant Unsigned_32 := Unsigned_32 (Report.Version);
      begin
         if Pos + 4 <= Max_Msg_Size then
            Msg (Pos) := Byte (V and 16#FF#);
            Msg (Pos + 1) := Byte (Shift_Right (V, 8) and 16#FF#);
            Msg (Pos + 2) := Byte (Shift_Right (V, 16) and 16#FF#);
            Msg (Pos + 3) := Byte (Shift_Right (V, 24) and 16#FF#);
            Pos := Pos + 4;
         end if;
      end;

      --  Copy timestamp
      for I in Report.Timestamp'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.Timestamp (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy nonce
      for I in Report.Nonce'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.Nonce (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy TEE measurements
      for I in Report.TEE_Code_Hash'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.TEE_Code_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      for I in Report.TEE_Config_Hash'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.TEE_Config_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      for I in Report.TEE_State_Root'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.TEE_State_Root (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy CVM count (4 bytes LE)
      declare
         C : constant Unsigned_32 := Unsigned_32 (Report.CVM_Count);
      begin
         if Pos + 4 <= Max_Msg_Size then
            Msg (Pos) := Byte (C and 16#FF#);
            Msg (Pos + 1) := Byte (Shift_Right (C, 8) and 16#FF#);
            Msg (Pos + 2) := Byte (Shift_Right (C, 16) and 16#FF#);
            Msg (Pos + 3) := Byte (Shift_Right (C, 24) and 16#FF#);
            Pos := Pos + 4;
         end if;
      end;

      --  Copy CVM measurements (only active ones)
      for J in 0 .. Report.CVM_Count - 1 loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);

         --  Copy address
         for I in Report.CVMs (J).Address'Range loop
            pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
            if Pos <= Max_Msg_Size - 1 then
               Msg (Pos) := Report.CVMs (J).Address (I);
               Pos := Pos + 1;
            end if;
         end loop;

         --  Copy code hash
         for I in Report.CVMs (J).CodeHash'Range loop
            pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
            if Pos <= Max_Msg_Size - 1 then
               Msg (Pos) := Report.CVMs (J).CodeHash (I);
               Pos := Pos + 1;
            end if;
         end loop;
      end loop;

      --  Copy attestation PK hash
      for I in Report.Attest_PK_Hash'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size - 1);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.Attest_PK_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Hash the message
      if Pos > 0 then
         SHA3_256 (Msg (0 .. Pos - 1), Hash);
      end if;
   end Hash_Report;

   --  Generate signed quote
   procedure Generate_Quote (
      Report    : Attestation_Report;
      Attest_SK : DSA_Secret_Key;
      Quote     : out Attestation_Quote;
      Success   : out Boolean
   ) is
      Report_Hash : Measurement;
      DSA_SK : Anubis_MLDSA_Types.Secret_Key;
      DSA_Sig : Anubis_MLDSA_Types.Signature;
      Sign_Seed : Anubis_MLDSA_Types.Seed := (others => 0);
      Sign_Success : Boolean;
   begin
      Quote := Empty_Quote;
      Success := False;

      --  Hash the report
      Hash_Report (Report, Report_Hash);

      --  Copy secret key
      for I in DSA_SK'Range loop
         pragma Loop_Invariant (I >= DSA_SK'First);
         DSA_SK (I) := Attest_SK (I);
      end loop;

      --  Sign the report hash
      Anubis_MLDSA.Sign (DSA_SK, Report_Hash, Sign_Seed, DSA_Sig, Sign_Success);

      if not Sign_Success then
         return;
      end if;

      --  Build quote
      Quote.Report := Report;

      --  Copy signature
      for I in DSA_Sig'Range loop
         pragma Loop_Invariant (I >= DSA_Sig'First);
         Quote.Signature (I) := DSA_Sig (I);
      end loop;

      Quote.Valid := True;
      Success := True;
   end Generate_Quote;

   --  Verify attestation quote
   function Verify_Quote (
      Quote     : Attestation_Quote;
      Attest_PK : DSA_Public_Key;
      Nonce     : Byte_Array
   ) return Boolean is
      Report_Hash : Measurement;
      DSA_PK : Anubis_MLDSA_Types.Public_Key;
      DSA_Sig : Anubis_MLDSA_Types.Signature;
   begin
      --  Check quote validity
      if not Quote.Valid then
         return False;
      end if;

      --  Check nonce matches
      for I in 0 .. 31 loop
         if Quote.Report.Nonce (I) /= Nonce (Nonce'First + I) then
            return False;
         end if;
      end loop;

      --  Hash the report
      Hash_Report (Quote.Report, Report_Hash);

      --  Copy public key
      for I in DSA_PK'Range loop
         pragma Loop_Invariant (I >= DSA_PK'First);
         DSA_PK (I) := Attest_PK (I);
      end loop;

      --  Copy signature
      for I in DSA_Sig'Range loop
         pragma Loop_Invariant (I >= DSA_Sig'First);
         DSA_Sig (I) := Quote.Signature (I);
      end loop;

      --  Verify signature
      return Anubis_MLDSA.Verify (DSA_PK, Report_Hash, DSA_Sig);
   end Verify_Quote;

   --  Verify measurements
   function Verify_Measurements (
      Quote           : Attestation_Quote;
      Expected_Code   : Measurement;
      Expected_Config : Measurement
   ) return Boolean is
   begin
      return Equal_Measurements (Quote.Report.TEE_Code_Hash, Expected_Code)
             and then Equal_Measurements (Quote.Report.TEE_Config_Hash, Expected_Config);
   end Verify_Measurements;

   --  Serialize report
   procedure Serialize_Report (
      Report  : Attestation_Report;
      Output  : out Byte_Array;
      Length  : out Natural;
      Success : out Boolean
   ) is
      Pos : Natural := 0;
   begin
      Output := (others => 0);
      Length := 0;
      Success := False;

      if not Report.Valid then
         return;
      end if;

      --  This is a simplified serialization - in production would use
      --  a proper encoding format like CBOR or Protocol Buffers

      --  Version (4 bytes)
      declare
         V : constant Unsigned_32 := Unsigned_32 (Report.Version);
      begin
         Output (Pos) := Byte (V and 16#FF#);
         Output (Pos + 1) := Byte (Shift_Right (V, 8) and 16#FF#);
         Output (Pos + 2) := Byte (Shift_Right (V, 16) and 16#FF#);
         Output (Pos + 3) := Byte (Shift_Right (V, 24) and 16#FF#);
         Pos := Pos + 4;
      end;

      --  Timestamp (8 bytes)
      for I in Report.Timestamp'Range loop
         Output (Pos) := Report.Timestamp (I);
         Pos := Pos + 1;
      end loop;

      --  Nonce (32 bytes)
      for I in Report.Nonce'Range loop
         Output (Pos) := Report.Nonce (I);
         Pos := Pos + 1;
      end loop;

      --  TEE measurements (32 * 3 = 96 bytes)
      for I in Report.TEE_Code_Hash'Range loop
         Output (Pos) := Report.TEE_Code_Hash (I);
         Pos := Pos + 1;
      end loop;

      for I in Report.TEE_Config_Hash'Range loop
         Output (Pos) := Report.TEE_Config_Hash (I);
         Pos := Pos + 1;
      end loop;

      for I in Report.TEE_State_Root'Range loop
         Output (Pos) := Report.TEE_State_Root (I);
         Pos := Pos + 1;
      end loop;

      --  Attest PK hash (32 bytes)
      for I in Report.Attest_PK_Hash'Range loop
         Output (Pos) := Report.Attest_PK_Hash (I);
         Pos := Pos + 1;
      end loop;

      --  CVM count (4 bytes)
      declare
         C : constant Unsigned_32 := Unsigned_32 (Report.CVM_Count);
      begin
         Output (Pos) := Byte (C and 16#FF#);
         Output (Pos + 1) := Byte (Shift_Right (C, 8) and 16#FF#);
         Output (Pos + 2) := Byte (Shift_Right (C, 16) and 16#FF#);
         Output (Pos + 3) := Byte (Shift_Right (C, 24) and 16#FF#);
         Pos := Pos + 4;
      end;

      --  CVM measurements
      for J in 0 .. Report.CVM_Count - 1 loop
         for I in Report.CVMs (J).Address'Range loop
            Output (Pos) := Report.CVMs (J).Address (I);
            Pos := Pos + 1;
         end loop;

         for I in Report.CVMs (J).CodeHash'Range loop
            Output (Pos) := Report.CVMs (J).CodeHash (I);
            Pos := Pos + 1;
         end loop;
      end loop;

      Length := Pos;
      Success := True;
   end Serialize_Report;

   --  Deserialize report (simplified)
   procedure Deserialize_Report (
      Input   : Byte_Array;
      Report  : out Attestation_Report;
      Success : out Boolean
   ) is
   begin
      Report := Empty_Report;
      Success := False;

      if Input'Length < 180 then  -- Minimum size
         return;
      end if;

      --  Parse version
      Report.Version := Natural (Input (Input'First)) +
                        Natural (Input (Input'First + 1)) * 256 +
                        Natural (Input (Input'First + 2)) * 65536;

      if Report.Version /= Report_Version then
         return;
      end if;

      --  Parse rest (simplified - full implementation would be more robust)
      Report.Valid := True;
      Success := True;
   end Deserialize_Report;

   --  Serialize quote
   procedure Serialize_Quote (
      Quote   : Attestation_Quote;
      Output  : out Byte_Array;
      Length  : out Natural;
      Success : out Boolean
   ) is
      Report_Len : Natural;
      Report_OK : Boolean;
   begin
      Output := (others => 0);
      Length := 0;
      Success := False;

      if not Quote.Valid then
         return;
      end if;

      --  Serialize report
      Serialize_Report (Quote.Report, Output, Report_Len, Report_OK);

      if not Report_OK then
         return;
      end if;

      --  Append signature
      for I in Quote.Signature'Range loop
         if Report_Len + I <= Output'Last then
            Output (Output'First + Report_Len + I) := Quote.Signature (I);
         end if;
      end loop;

      Length := Report_Len + DSA_SIG_Size;
      Success := True;
   end Serialize_Quote;

   --  Deserialize quote
   procedure Deserialize_Quote (
      Input   : Byte_Array;
      Quote   : out Attestation_Quote;
      Success : out Boolean
   ) is
   begin
      Quote := Empty_Quote;
      Success := False;

      if Input'Length < 180 + DSA_SIG_Size then
         return;
      end if;

      --  Deserialize report
      Deserialize_Report (Input, Quote.Report, Success);

      if not Success then
         return;
      end if;

      --  Copy signature (last DSA_SIG_Size bytes)
      declare
         Sig_Start : constant Natural := Input'Length - DSA_SIG_Size;
      begin
         for I in Quote.Signature'Range loop
            Quote.Signature (I) := Input (Input'First + Sig_Start + I);
         end loop;
      end;

      Quote.Valid := True;
      Success := True;
   end Deserialize_Quote;

   --  Get timestamp (placeholder - real implementation would use system time)
   procedure Get_Timestamp (
      Timestamp : out Byte_Array
   ) is
   begin
      --  In a real implementation, this would get the current Unix timestamp
      --  For now, just zero it out (caller should set it)
      Timestamp := (others => 0);
   end Get_Timestamp;

   --  Compare measurements (constant-time)
   function Equal_Measurements (
      A : Measurement;
      B : Measurement
   ) return Boolean is
      Diff : Byte := 0;
   begin
      for I in A'Range loop
         Diff := Diff or (A (I) xor B (I));
      end loop;
      return Diff = 0;
   end Equal_Measurements;

end TEE_Attestation;
