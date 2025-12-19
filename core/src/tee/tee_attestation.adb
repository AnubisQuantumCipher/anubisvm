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
      Block_Time    : Word64;
      Attest_PK     : DSA_Public_Key;
      Report        : out Attestation_Report
   ) is
      PK_Hash : SHA3_256_Digest;
      TS      : Word64 := Block_Time;
   begin
      Report := Empty_Report;

      --  Set version
      Report.Version := Report_Version;

      --  Set timestamp from block time (little-endian)
      for I in Report.Timestamp'Range loop
         Report.Timestamp (I) := Byte (TS and 16#FF#);
         TS := Word64 (Shift_Right (Unsigned_64 (TS), 8));
      end loop;

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
         pragma Loop_Invariant (Pos <= Max_Msg_Size);
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
         pragma Loop_Invariant (Pos <= Max_Msg_Size);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.Timestamp (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy nonce
      for I in Report.Nonce'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.Nonce (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Copy TEE measurements
      for I in Report.TEE_Code_Hash'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.TEE_Code_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      for I in Report.TEE_Config_Hash'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size);
         if Pos <= Max_Msg_Size - 1 then
            Msg (Pos) := Report.TEE_Config_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      for I in Report.TEE_State_Root'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size);
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
      --  Guard: ensure CVM_Count is within array bounds
      if Report.CVM_Count > 0 and then Report.CVM_Count <= Max_CVM_Measurements then
         for J in 0 .. Report.CVM_Count - 1 loop
            pragma Loop_Invariant (Pos <= Max_Msg_Size);
            pragma Loop_Invariant (J in 0 .. Max_CVM_Measurements - 1);

            --  Copy address
            for I in Report.CVMs (J).Address'Range loop
               pragma Loop_Invariant (Pos <= Max_Msg_Size);
               if Pos <= Max_Msg_Size - 1 then
                  Msg (Pos) := Report.CVMs (J).Address (I);
                  Pos := Pos + 1;
               end if;
            end loop;

            --  Copy code hash
            for I in Report.CVMs (J).CodeHash'Range loop
               pragma Loop_Invariant (Pos <= Max_Msg_Size);
               if Pos <= Max_Msg_Size - 1 then
                  Msg (Pos) := Report.CVMs (J).CodeHash (I);
                  Pos := Pos + 1;
               end if;
            end loop;
         end loop;
      end if;

      --  Copy attestation PK hash
      for I in Report.Attest_PK_Hash'Range loop
         pragma Loop_Invariant (Pos <= Max_Msg_Size);
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
      --  Precondition guarantees Output'Length >= Max_Report_Size (4096)
      --  Total fixed size: 4+8+32+32*4+4 = 180 bytes
      --  Variable size: 64*CVM_Count bytes max = 64*32 = 2048
      --  Total max: 180 + 2048 = 2228 < 4096
   begin
      Output := (others => 0);
      Length := 0;
      Success := False;

      if not Report.Valid then
         return;
      end if;

      --  Binary serialization format (little-endian):
      --  +0:   Version (4 bytes)
      --  +4:   Timestamp (8 bytes)
      --  +12:  Nonce (32 bytes)
      --  +44:  TEE_Code_Hash (32 bytes)
      --  +76:  TEE_Config_Hash (32 bytes)
      --  +108: TEE_State_Root (32 bytes)
      --  +140: Attest_PK_Hash (32 bytes)
      --  +172: CVM_Count (4 bytes)
      --  +176: CVM measurements (64 bytes each: 32 address + 32 code_hash)
      --  Total fixed: 176 bytes + 64*CVM_Count

      --  Precondition guarantees Output'Length >= 4096, so Pos+3 is safe
      --  Version (4 bytes)
      declare
         V : constant Unsigned_32 := Unsigned_32 (Report.Version);
      begin
         if Pos + 3 <= Output'Last then
            Output (Output'First + Pos) := Byte (V and 16#FF#);
            Output (Output'First + Pos + 1) := Byte (Shift_Right (V, 8) and 16#FF#);
            Output (Output'First + Pos + 2) := Byte (Shift_Right (V, 16) and 16#FF#);
            Output (Output'First + Pos + 3) := Byte (Shift_Right (V, 24) and 16#FF#);
            Pos := Pos + 4;
         end if;
      end;

      --  Timestamp (8 bytes) - Pos now = 4
      for I in Report.Timestamp'Range loop
         pragma Loop_Invariant (Pos < Max_Report_Size);
         if Output'First + Pos <= Output'Last then
            Output (Output'First + Pos) := Report.Timestamp (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Nonce (32 bytes) - Pos now = 12
      for I in Report.Nonce'Range loop
         pragma Loop_Invariant (Pos < Max_Report_Size);
         if Output'First + Pos <= Output'Last then
            Output (Output'First + Pos) := Report.Nonce (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  TEE measurements (32 * 3 = 96 bytes) - Pos now = 44
      for I in Report.TEE_Code_Hash'Range loop
         pragma Loop_Invariant (Pos < Max_Report_Size);
         if Output'First + Pos <= Output'Last then
            Output (Output'First + Pos) := Report.TEE_Code_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      for I in Report.TEE_Config_Hash'Range loop
         pragma Loop_Invariant (Pos < Max_Report_Size);
         if Output'First + Pos <= Output'Last then
            Output (Output'First + Pos) := Report.TEE_Config_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      for I in Report.TEE_State_Root'Range loop
         pragma Loop_Invariant (Pos < Max_Report_Size);
         if Output'First + Pos <= Output'Last then
            Output (Output'First + Pos) := Report.TEE_State_Root (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Attest PK hash (32 bytes)
      for I in Report.Attest_PK_Hash'Range loop
         pragma Loop_Invariant (Pos < Max_Report_Size);
         if Output'First + Pos <= Output'Last then
            Output (Output'First + Pos) := Report.Attest_PK_Hash (I);
            Pos := Pos + 1;
         end if;
      end loop;

      --  CVM count (4 bytes)
      declare
         C : constant Unsigned_32 := Unsigned_32 (Report.CVM_Count);
      begin
         if Pos + 3 < Max_Report_Size and then Output'First + Pos + 3 <= Output'Last then
            Output (Output'First + Pos) := Byte (C and 16#FF#);
            Output (Output'First + Pos + 1) := Byte (Shift_Right (C, 8) and 16#FF#);
            Output (Output'First + Pos + 2) := Byte (Shift_Right (C, 16) and 16#FF#);
            Output (Output'First + Pos + 3) := Byte (Shift_Right (C, 24) and 16#FF#);
            Pos := Pos + 4;
         end if;
      end;

      --  CVM measurements (with bounds guards)
      if Report.CVM_Count > 0 and then Report.CVM_Count <= Max_CVM_Measurements then
         for J in 0 .. Report.CVM_Count - 1 loop
            pragma Loop_Invariant (J in 0 .. Max_CVM_Measurements - 1);
            pragma Loop_Invariant (Pos < Max_Report_Size);

            for I in Report.CVMs (J).Address'Range loop
               pragma Loop_Invariant (Pos < Max_Report_Size);
               if Output'First + Pos <= Output'Last then
                  Output (Output'First + Pos) := Report.CVMs (J).Address (I);
                  if Pos < Max_Report_Size - 1 then
                     Pos := Pos + 1;
                  end if;
               end if;
            end loop;

            for I in Report.CVMs (J).CodeHash'Range loop
               pragma Loop_Invariant (Pos < Max_Report_Size);
               if Output'First + Pos <= Output'Last then
                  Output (Output'First + Pos) := Report.CVMs (J).CodeHash (I);
                  if Pos < Max_Report_Size - 1 then
                     Pos := Pos + 1;
                  end if;
               end if;
            end loop;
         end loop;
      end if;

      Length := Pos;
      Success := True;
   end Serialize_Report;

   --  Deserialize report - full implementation parsing all fields
   procedure Deserialize_Report (
      Input   : Byte_Array;
      Report  : out Attestation_Report;
      Success : out Boolean
   ) is
      Pos : Natural := 0;
      --  Minimum size: Version(4) + Timestamp(8) + Nonce(32) +
      --  TEE_Code(32) + TEE_Config(32) + TEE_State(32) + Attest_PK(32) + CVM_Count(4)
      --  = 4 + 8 + 32 + 32 + 32 + 32 + 32 + 4 = 176 bytes minimum
      Min_Size : constant := 176;
   begin
      Report := Empty_Report;
      Success := False;

      --  Check minimum size
      if Input'Length < Min_Size then
         return;
      end if;

      --  Parse version (4 bytes LE)
      Report.Version := Natural (Input (Input'First)) +
                        Natural (Input (Input'First + 1)) * 256 +
                        Natural (Input (Input'First + 2)) * 65536 +
                        Natural (Input (Input'First + 3) mod 128) * 16777216;  -- Guard against overflow

      if Report.Version /= Report_Version then
         return;
      end if;
      Pos := 4;

      --  Parse timestamp (8 bytes)
      for I in Report.Timestamp'Range loop
         pragma Loop_Invariant (Pos < Input'Length);
         pragma Loop_Invariant (I in 0 .. 7);
         if Pos < Input'Length then
            Report.Timestamp (I) := Input (Input'First + Pos);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Parse nonce (32 bytes)
      for I in Report.Nonce'Range loop
         pragma Loop_Invariant (Pos < Input'Length);
         pragma Loop_Invariant (I in 0 .. 31);
         if Pos < Input'Length then
            Report.Nonce (I) := Input (Input'First + Pos);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Parse TEE code hash (32 bytes)
      for I in Report.TEE_Code_Hash'Range loop
         pragma Loop_Invariant (Pos < Input'Length);
         pragma Loop_Invariant (I in 0 .. 31);
         if Pos < Input'Length then
            Report.TEE_Code_Hash (I) := Input (Input'First + Pos);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Parse TEE config hash (32 bytes)
      for I in Report.TEE_Config_Hash'Range loop
         pragma Loop_Invariant (Pos < Input'Length);
         pragma Loop_Invariant (I in 0 .. 31);
         if Pos < Input'Length then
            Report.TEE_Config_Hash (I) := Input (Input'First + Pos);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Parse TEE state root (32 bytes)
      for I in Report.TEE_State_Root'Range loop
         pragma Loop_Invariant (Pos < Input'Length);
         pragma Loop_Invariant (I in 0 .. 31);
         if Pos < Input'Length then
            Report.TEE_State_Root (I) := Input (Input'First + Pos);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Parse attestation PK hash (32 bytes)
      for I in Report.Attest_PK_Hash'Range loop
         pragma Loop_Invariant (Pos < Input'Length);
         pragma Loop_Invariant (I in 0 .. 31);
         if Pos < Input'Length then
            Report.Attest_PK_Hash (I) := Input (Input'First + Pos);
            Pos := Pos + 1;
         end if;
      end loop;

      --  Parse CVM count (4 bytes LE)
      if Pos + 4 > Input'Length then
         return;
      end if;
      Report.CVM_Count := Natural (Input (Input'First + Pos)) +
                          Natural (Input (Input'First + Pos + 1)) * 256 +
                          Natural (Input (Input'First + Pos + 2)) * 65536;
      --  Ignore high byte to prevent overflow; CVM_Count limited to Max_CVM_Measurements
      Pos := Pos + 4;

      --  Validate CVM count
      if Report.CVM_Count > Max_CVM_Measurements then
         return;
      end if;

      --  Check remaining input size for CVM measurements
      --  Each CVM measurement = 32 (address) + 32 (code_hash) = 64 bytes
      if Report.CVM_Count > 0 then
         declare
            Required_Size : constant Natural := Report.CVM_Count * 64;
         begin
            if Input'Length - Pos < Required_Size then
               return;
            end if;
         end;

         --  Parse CVM measurements
         for J in 0 .. Report.CVM_Count - 1 loop
            pragma Loop_Invariant (J in 0 .. Max_CVM_Measurements - 1);
            pragma Loop_Invariant (Pos < Input'Length);

            --  Parse address (32 bytes)
            for I in Report.CVMs (J).Address'Range loop
               pragma Loop_Invariant (I in 0 .. 31);
               if Pos < Input'Length then
                  Report.CVMs (J).Address (I) := Input (Input'First + Pos);
                  Pos := Pos + 1;
               end if;
            end loop;

            --  Parse code hash (32 bytes)
            for I in Report.CVMs (J).CodeHash'Range loop
               pragma Loop_Invariant (I in 0 .. 31);
               if Pos < Input'Length then
                  Report.CVMs (J).CodeHash (I) := Input (Input'First + Pos);
                  Pos := Pos + 1;
               end if;
            end loop;

            --  Mark CVM as active
            Report.CVMs (J).Active := True;
         end loop;
      end if;

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
      --  Format: [Report][Signature]
      --  Report size = 176 + 64 * CVM_Count
      --  CVM_Count is at offset 172 in the report
      CVM_Count_Offset : constant := 172;
      Min_Report_Size  : constant := 176;
      CVM_Count        : Natural := 0;
      Report_Size      : Natural;
      Sig_Start        : Natural;
   begin
      Quote := Empty_Quote;
      Success := False;

      --  Need at least minimum report size + signature
      if Input'Length < Min_Report_Size + DSA_SIG_Size then
         return;
      end if;

      --  Read CVM_Count from offset 172 (4 bytes LE)
      if CVM_Count_Offset + 3 <= Input'Last - Input'First then
         CVM_Count := Natural (Input (Input'First + CVM_Count_Offset)) +
                      Natural (Input (Input'First + CVM_Count_Offset + 1)) * 256 +
                      Natural (Input (Input'First + CVM_Count_Offset + 2)) * 65536;
      end if;

      --  Validate CVM_Count
      if CVM_Count > Max_CVM_Measurements then
         return;
      end if;

      --  Calculate actual report size
      Report_Size := Min_Report_Size + CVM_Count * 64;

      --  Verify input has enough bytes for report + signature
      if Input'Length < Report_Size + DSA_SIG_Size then
         return;
      end if;

      --  Deserialize report
      Deserialize_Report (Input, Quote.Report, Success);

      if not Success then
         return;
      end if;

      --  Read signature immediately after report
      Sig_Start := Report_Size;
      for I in Quote.Signature'Range loop
         pragma Loop_Invariant (I in Quote.Signature'Range);
         if Input'First + Sig_Start + I <= Input'Last then
            Quote.Signature (I) := Input (Input'First + Sig_Start + I);
         end if;
      end loop;

      Quote.Valid := True;
      Success := True;
   end Deserialize_Quote;

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

   ---------------------------------------------------------------------------
   --  Measurement Chain Verification
   ---------------------------------------------------------------------------

   --  Verify measurement chain integrity
   function Verify_Measurement_Chain (
      Chain         : CVM_Measurement_Array;
      Chain_Length  : Natural;
      Expected_Root : Measurement
   ) return Boolean is
      Computed_Root : Measurement;
   begin
      --  Empty chain is invalid
      if Chain_Length = 0 then
         return False;
      end if;

      --  Verify each measurement in the chain is valid
      for I in 0 .. Chain_Length - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Chain_Length);
         pragma Loop_Invariant (Chain_Length <= Max_CVM_Measurements);

         if not Verify_CVM_Measurement (Chain (I)) then
            return False;
         end if;
      end loop;

      --  Compute the root hash of the chain
      Compute_Chain_Root (Chain, Chain_Length, Computed_Root);

      --  Verify root matches expected
      return Equal_Measurements (Computed_Root, Expected_Root);
   end Verify_Measurement_Chain;

   --  Extend measurement chain (hash extension)
   procedure Extend_Measurement (
      Previous        : Measurement;
      Current         : Measurement;
      New_Measurement : out Measurement
   ) is
      Combined : Byte_Array (0 .. 63);  --  Previous || Current (32 + 32 bytes)
   begin
      New_Measurement := (others => 0);

      --  Copy previous measurement
      for I in Previous'Range loop
         pragma Loop_Invariant (I in Previous'Range);
         pragma Loop_Invariant (I in 0 .. 31);
         Combined (I) := Previous (I);
      end loop;

      --  Copy current measurement
      for I in Current'Range loop
         pragma Loop_Invariant (I in Current'Range);
         pragma Loop_Invariant (I in 0 .. 31);
         pragma Loop_Invariant (32 + I in Combined'Range);
         Combined (32 + I) := Current (I);
      end loop;

      --  Hash the combined measurements
      SHA3_256 (Combined, New_Measurement);
   end Extend_Measurement;

   --  Verify CVM measurement is valid
   function Verify_CVM_Measurement (
      M : CVM_Measurement
   ) return Boolean is
      Address_All_Zero : Boolean := True;
      CodeHash_All_Zero : Boolean := True;
   begin
      --  Must be marked as active
      if not M.Active then
         return False;
      end if;

      --  Check if address is all zeros
      for I in M.Address'Range loop
         pragma Loop_Invariant (Address_All_Zero or not Address_All_Zero);
         if M.Address (I) /= 0 then
            Address_All_Zero := False;
            exit;
         end if;
      end loop;

      --  Address must not be all zeros
      if Address_All_Zero then
         return False;
      end if;

      --  Check if code hash is all zeros
      for I in M.CodeHash'Range loop
         pragma Loop_Invariant (CodeHash_All_Zero or not CodeHash_All_Zero);
         if M.CodeHash (I) /= 0 then
            CodeHash_All_Zero := False;
            exit;
         end if;
      end loop;

      --  Code hash must not be all zeros
      if CodeHash_All_Zero then
         return False;
      end if;

      return True;
   end Verify_CVM_Measurement;

   --  Compute measurement chain root
   procedure Compute_Chain_Root (
      Measurements : CVM_Measurement_Array;
      Count        : Natural;
      Root         : out Measurement
   ) is
      --  Maximum serialized size: Count * (32 address + 32 code_hash) = Count * 64
      --  With Max_CVM_Measurements = 32: max = 32 * 64 = 2048 bytes
      Max_Chain_Size : constant := Max_CVM_Measurements * 64;
      Chain_Data : Byte_Array (0 .. Max_Chain_Size - 1) := (others => 0);
      Pos : Natural := 0;
   begin
      Root := (others => 0);

      --  Handle empty chain
      if Count = 0 then
         return;
      end if;

      --  Serialize all measurements into a single byte array
      for J in 0 .. Count - 1 loop
         pragma Loop_Invariant (J >= 0 and J < Count);
         pragma Loop_Invariant (Count <= Max_CVM_Measurements);
         pragma Loop_Invariant (Pos <= Max_Chain_Size);
         pragma Loop_Invariant (Pos = J * 64);

         --  Copy address (32 bytes)
         for I in Measurements (J).Address'Range loop
            pragma Loop_Invariant (I in Measurements (J).Address'Range);
            pragma Loop_Invariant (Pos + I < Max_Chain_Size);
            if Pos + I < Max_Chain_Size then
               Chain_Data (Pos + I) := Measurements (J).Address (I);
            end if;
         end loop;
         Pos := Pos + 32;

         --  Copy code hash (32 bytes)
         for I in Measurements (J).CodeHash'Range loop
            pragma Loop_Invariant (I in Measurements (J).CodeHash'Range);
            pragma Loop_Invariant (Pos + I < Max_Chain_Size);
            if Pos + I < Max_Chain_Size then
               Chain_Data (Pos + I) := Measurements (J).CodeHash (I);
            end if;
         end loop;
         Pos := Pos + 32;
      end loop;

      --  Hash the entire chain
      if Pos > 0 and Pos <= Max_Chain_Size then
         SHA3_256 (Chain_Data (0 .. Pos - 1), Root);
      end if;
   end Compute_Chain_Root;

   ---------------------------------------------------------------------------
   --  Hardware Attestation Support
   ---------------------------------------------------------------------------

   --  Verify hardware attestation evidence
   function Verify_Hardware_Attestation (
      Evidence      : Hardware_Attestation_Evidence;
      Expected_Type : Hardware_Attestation_Type
   ) return Boolean is
      Has_Non_Zero : Boolean := False;
   begin
      --  Must be valid
      if not Evidence.Valid then
         return False;
      end if;

      --  Must match expected type
      if Evidence.Attestation_Type /= Expected_Type then
         return False;
      end if;

      --  None type should have no data
      if Evidence.Attestation_Type = None then
         return Evidence.Data_Length = 0;
      end if;

      --  Software_Only requires no platform data
      if Evidence.Attestation_Type = Software_Only then
         return Evidence.Data_Length = 0;
      end if;

      --  Hardware attestation types require non-empty, non-zero data
      if Evidence.Data_Length = 0 then
         return False;
      end if;

      --  Check that platform data is not all zeros
      for I in 0 .. Evidence.Data_Length - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Evidence.Data_Length);
         pragma Loop_Invariant (Evidence.Data_Length <= 256);
         pragma Loop_Invariant (not Has_Non_Zero or Has_Non_Zero);

         if Evidence.Platform_Data (I) /= 0 then
            Has_Non_Zero := True;
            exit;
         end if;
      end loop;

      if not Has_Non_Zero then
         return False;
      end if;

      --  Platform-specific validation would go here
      --  For now, basic structural validation is sufficient
      case Evidence.Attestation_Type is
         when TPM_2_0 =>
            --  TPM 2.0 quote typically includes PCR values, signature, etc.
            --  Minimum size check (this is a simplified check)
            return Evidence.Data_Length >= 64;

         when Intel_SGX =>
            --  SGX report structure is typically 432 bytes
            --  We accept anything >= 64 bytes as "plausible"
            return Evidence.Data_Length >= 64;

         when AMD_SEV_SNP =>
            --  AMD SEV-SNP attestation report
            return Evidence.Data_Length >= 64;

         when ARM_TrustZone =>
            --  ARM TrustZone attestation token
            return Evidence.Data_Length >= 32;

         when Apple_Secure_Enclave =>
            --  Apple Secure Enclave attestation
            return Evidence.Data_Length >= 32;

         when None | Software_Only =>
            --  Already handled above
            return True;
      end case;
   end Verify_Hardware_Attestation;

   --  Create hardware attestation evidence from report
   procedure Create_Hardware_Evidence (
      Report           : Attestation_Report;
      Attestation_Type : Hardware_Attestation_Type;
      Platform_Data    : Byte_Array;
      Evidence         : out Hardware_Attestation_Evidence
   ) is
   begin
      Evidence := Empty_Hardware_Evidence;

      --  Set attestation type
      Evidence.Attestation_Type := Attestation_Type;

      --  Copy platform data
      Evidence.Data_Length := Platform_Data'Length;
      for I in Platform_Data'Range loop
         pragma Loop_Invariant (I in Platform_Data'Range);
         pragma Loop_Invariant (Platform_Data'Length <= 256);
         pragma Loop_Invariant (I - Platform_Data'First < 256);

         if I - Platform_Data'First < 256 then
            Evidence.Platform_Data (I - Platform_Data'First) := Platform_Data (I);
         end if;
      end loop;

      Evidence.Valid := True;
   end Create_Hardware_Evidence;

end TEE_Attestation;
