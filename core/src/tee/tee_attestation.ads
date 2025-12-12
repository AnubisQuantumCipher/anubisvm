pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with TEE_Keys; use TEE_Keys;

--  TEE_Attestation: Remote Attestation for Trusted Execution Environment
--
--  This package implements post-quantum remote attestation:
--  - Attestation Report generation (signed measurements)
--  - Quote generation (signed + encrypted reports)
--  - Verification of remote attestation
--
--  Attestation flow:
--  1. TEE measures its state (code, config, runtime state)
--  2. Measurements are hashed into an Attestation Report
--  3. Report is signed with ML-DSA-87 attestation key
--  4. Verifier checks signature and measurements
--
--  Measurement sources:
--  - Code hash: SHA3-256 of TEE binary
--  - Config hash: SHA3-256 of configuration
--  - State hash: Khepri MPT root hash
--  - CVM hashes: Registered CVM code hashes

package TEE_Attestation with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Attestation Report
   ---------------------------------------------------------------------------

   --  Report version
   Report_Version : constant := 1;

   --  Maximum number of CVM measurements in report
   Max_CVM_Measurements : constant := 32;

   --  Measurement (SHA3-256 hash)
   subtype Measurement is Byte_Array (0 .. 31);

   --  CVM measurement entry
   type CVM_Measurement is record
      Address  : Byte_Array (0 .. 31);  -- CVM address
      CodeHash : Measurement;            -- SHA3-256 of CVM code
      Active   : Boolean;
   end record;

   --  CVM measurement array
   type CVM_Measurement_Array is array (0 .. Max_CVM_Measurements - 1) of CVM_Measurement;

   --  Empty CVM measurement
   Empty_CVM_Measurement : constant CVM_Measurement := (
      Address  => (others => 0),
      CodeHash => (others => 0),
      Active   => False
   );

   --  Attestation Report structure
   type Attestation_Report is record
      --  Report metadata
      Version   : Natural;              -- Report format version
      Timestamp : Byte_Array (0 .. 7);  -- Unix timestamp (64-bit LE)
      Nonce     : Byte_Array (0 .. 31); -- Verifier-provided nonce

      --  TEE measurements
      TEE_Code_Hash   : Measurement;    -- Hash of TEE binary
      TEE_Config_Hash : Measurement;    -- Hash of configuration
      TEE_State_Root  : Measurement;    -- Khepri MPT root hash

      --  CVM measurements
      CVM_Count : Natural;
      CVMs      : CVM_Measurement_Array;

      --  Attestation key info
      Attest_PK_Hash : Measurement;     -- Hash of attestation public key

      --  Report is valid
      Valid : Boolean;
   end record;

   --  Empty report
   Empty_Report : constant Attestation_Report := (
      Version         => 0,
      Timestamp       => (others => 0),
      Nonce           => (others => 0),
      TEE_Code_Hash   => (others => 0),
      TEE_Config_Hash => (others => 0),
      TEE_State_Root  => (others => 0),
      CVM_Count       => 0,
      CVMs            => (others => Empty_CVM_Measurement),
      Attest_PK_Hash  => (others => 0),
      Valid           => False
   );

   ---------------------------------------------------------------------------
   --  Signed Quote
   ---------------------------------------------------------------------------

   --  Quote contains report + signature
   type Attestation_Quote is record
      Report    : Attestation_Report;
      Signature : DSA_Signature;
      Valid     : Boolean;
   end record;

   --  Empty quote
   Empty_Quote : constant Attestation_Quote := (
      Report    => Empty_Report,
      Signature => (others => 0),
      Valid     => False
   );

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   --  Generate attestation report
   --
   --  TEE_Code     : Hash of TEE binary
   --  TEE_Config   : Hash of configuration
   --  State_Root   : Current state root hash
   --  CVM_Hashes   : Array of CVM code hashes
   --  CVM_Addresses: Array of CVM addresses
   --  CVM_Active   : Array of CVM active flags
   --  CVM_Count    : Number of CVMs
   --  Nonce        : Verifier-provided nonce (32 bytes)
   --  Attest_PK    : Attestation public key
   --  Report       : Output attestation report
   procedure Generate_Report (
      TEE_Code      : Measurement;
      TEE_Config    : Measurement;
      State_Root    : Measurement;
      CVM_Hashes    : CVM_Measurement_Array;
      CVM_Count     : Natural;
      Nonce         : Byte_Array;
      Attest_PK     : DSA_Public_Key;
      Report        : out Attestation_Report
   ) with
      Global => null,
      Pre => Nonce'Length = 32 and then CVM_Count <= Max_CVM_Measurements,
      Post => Report.Valid and then Report.Version = Report_Version;

   ---------------------------------------------------------------------------
   --  Quote Generation and Verification
   ---------------------------------------------------------------------------

   --  Generate signed quote
   --
   --  Report    : Attestation report
   --  Attest_SK : Attestation secret key
   --  Quote     : Output signed quote
   --  Success   : True if signing succeeded
   procedure Generate_Quote (
      Report    : Attestation_Report;
      Attest_SK : DSA_Secret_Key;
      Quote     : out Attestation_Quote;
      Success   : out Boolean
   ) with
      Global => null,
      Pre => Report.Valid,
      Post => (if Success then Quote.Valid);

   --  Verify attestation quote
   --
   --  Quote     : Signed quote to verify
   --  Attest_PK : Expected attestation public key
   --  Nonce     : Expected nonce
   --  Valid     : True if quote is valid
   function Verify_Quote (
      Quote     : Attestation_Quote;
      Attest_PK : DSA_Public_Key;
      Nonce     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Nonce'Length = 32;

   --  Verify specific measurements in quote
   --
   --  Quote           : Verified quote
   --  Expected_Code   : Expected TEE code hash
   --  Expected_Config : Expected config hash
   --  Match           : True if measurements match
   function Verify_Measurements (
      Quote           : Attestation_Quote;
      Expected_Code   : Measurement;
      Expected_Config : Measurement
   ) return Boolean with
      Global => null,
      Pre => Quote.Valid;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Maximum serialized report size
   Max_Report_Size : constant := 4096;

   --  Maximum serialized quote size
   Max_Quote_Size : constant := Max_Report_Size + DSA_SIG_Size;

   --  Serialize report to bytes
   procedure Serialize_Report (
      Report  : Attestation_Report;
      Output  : out Byte_Array;
      Length  : out Natural;
      Success : out Boolean
   ) with
      Global => null,
      Pre => Output'Length >= Max_Report_Size,
      Post => (if Success then Length <= Max_Report_Size);

   --  Deserialize report from bytes
   procedure Deserialize_Report (
      Input   : Byte_Array;
      Report  : out Attestation_Report;
      Success : out Boolean
   ) with
      Global => null;

   --  Serialize quote to bytes
   procedure Serialize_Quote (
      Quote   : Attestation_Quote;
      Output  : out Byte_Array;
      Length  : out Natural;
      Success : out Boolean
   ) with
      Global => null,
      Pre => Output'Length >= Max_Quote_Size,
      Post => (if Success then Length <= Max_Quote_Size);

   --  Deserialize quote from bytes
   procedure Deserialize_Quote (
      Input   : Byte_Array;
      Quote   : out Attestation_Quote;
      Success : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Compute report hash (for signing)
   procedure Hash_Report (
      Report : Attestation_Report;
      Hash   : out Measurement
   ) with
      Global => null,
      Pre => Report.Valid;

   --  Get current timestamp
   procedure Get_Timestamp (
      Timestamp : out Byte_Array
   ) with
      Global => null,
      Pre => Timestamp'Length = 8;

   --  Compare measurements (constant-time)
   function Equal_Measurements (
      A : Measurement;
      B : Measurement
   ) return Boolean with
      Global => null;

end TEE_Attestation;
