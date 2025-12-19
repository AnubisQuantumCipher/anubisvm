pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

--  TEE_Platform: Platform Abstraction Interface
--
--  This package defines an abstract interface for platform-specific
--  operations required by the TEE. All verification logic remains in
--  pure SPARK, while platform integration is delegated to implementations
--  outside the SPARK boundary.
--
--  Design Philosophy:
--  - ALL verification logic is pure SPARK and provably correct
--  - Platform operations are parameters injected into TEE functions
--  - Timestamps, hardware measurements, and entropy are provided by caller
--  - TEE never makes OS calls or accesses hardware directly
--
--  This approach enables:
--  - Complete formal verification of TEE logic
--  - Platform portability (Linux, macOS, bare-metal, enclaves)
--  - Test isolation (inject deterministic values for testing)
--  - Hardware abstraction (Apple Secure Enclave, Intel SGX, etc.)

package TEE_Platform with
   SPARK_Mode => On,
   Pure,
   Always_Terminates
is

   ---------------------------------------------------------------------------
   --  Time Abstraction
   ---------------------------------------------------------------------------

   --  Unix timestamp (seconds since 1970-01-01)
   subtype Unix_Timestamp is Word64;

   --  Timestamp source enumeration (for verification)
   type Timestamp_Source is (
      Block_Time,      --  Blockchain block timestamp
      System_Time,     --  OS system time (untrusted)
      Hardware_Clock,  --  Hardware secure clock (trusted)
      Test_Time        --  Fixed timestamp for testing
   );

   --  Timestamp with source metadata
   type Timestamped_Value is record
      Value  : Unix_Timestamp;  --  Timestamp value (seconds)
      Source : Timestamp_Source; --  Where timestamp came from
      Valid  : Boolean;          --  Timestamp is valid
   end record;

   --  Zero timestamp
   Zero_Timestamp : constant Timestamped_Value := (
      Value  => 0,
      Source => Test_Time,
      Valid  => False
   );

   ---------------------------------------------------------------------------
   --  Entropy Abstraction
   ---------------------------------------------------------------------------

   --  Entropy quality enumeration
   type Entropy_Quality is (
      Hardware_RNG,    --  Hardware random number generator (highest quality)
      System_RNG,      --  OS cryptographic RNG (high quality)
      PRNG,            --  Pseudorandom generator (low quality, for testing)
      Test_Fixed       --  Fixed value (testing only)
   );

   --  Maximum entropy size (512 bits)
   Max_Entropy_Size : constant := 64;

   --  Entropy buffer type
   subtype Entropy_Buffer is Byte_Array (0 .. Max_Entropy_Size - 1);

   --  Entropy with quality metadata
   type Entropy_Sample is record
      Data    : Entropy_Buffer;     --  Entropy bytes
      Size    : Natural;             --  Valid entropy bytes (1..64)
      Quality : Entropy_Quality;     --  Entropy source quality
      Valid   : Boolean;             --  Entropy is valid
   end record;

   --  Zero entropy
   Zero_Entropy : constant Entropy_Sample := (
      Data    => (others => 0),
      Size    => 0,
      Quality => Test_Fixed,
      Valid   => False
   );

   ---------------------------------------------------------------------------
   --  Hardware Measurement Abstraction
   ---------------------------------------------------------------------------

   --  Measurement type (SHA3-256 hash)
   subtype Platform_Measurement is Byte_Array (0 .. 31);

   --  Measurement source enumeration
   type Measurement_Source is (
      Software_Hash,    --  Software-computed hash
      Hardware_PCR,     --  Hardware Platform Configuration Register
      Enclave_Report,   --  Enclave attestation report
      Test_Fixture      --  Fixed test value
   );

   --  Hardware measurement with source metadata
   type Hardware_Measurement is record
      Hash   : Platform_Measurement;  --  Measurement hash
      Source : Measurement_Source;    --  Where measurement came from
      Valid  : Boolean;                --  Measurement is valid
   end record;

   --  Zero measurement
   Zero_Measurement : constant Hardware_Measurement := (
      Hash   => (others => 0),
      Source => Test_Fixture,
      Valid  => False
   );

   ---------------------------------------------------------------------------
   --  Platform Configuration
   ---------------------------------------------------------------------------

   --  Platform capabilities flags
   type Platform_Capabilities is record
      Has_Hardware_RNG      : Boolean;  --  Hardware RNG available
      Has_Secure_Clock      : Boolean;  --  Hardware secure clock available
      Has_Hardware_PCR      : Boolean;  --  Hardware PCR measurements
      Has_Enclave_Support   : Boolean;  --  Secure enclave available
      Has_Memory_Isolation  : Boolean;  --  Hardware memory isolation
   end record;

   --  Zero capabilities (minimal platform)
   Zero_Capabilities : constant Platform_Capabilities := (
      Has_Hardware_RNG      => False,
      Has_Secure_Clock      => False,
      Has_Hardware_PCR      => False,
      Has_Enclave_Support   => False,
      Has_Memory_Isolation  => False
   );

   --  Full capabilities (maximum security platform)
   Full_Capabilities : constant Platform_Capabilities := (
      Has_Hardware_RNG      => True,
      Has_Secure_Clock      => True,
      Has_Hardware_PCR      => True,
      Has_Enclave_Support   => True,
      Has_Memory_Isolation  => True
   );

   ---------------------------------------------------------------------------
   --  Platform Information
   ---------------------------------------------------------------------------

   --  Platform type enumeration
   type Platform_Type is (
      Generic_Linux,        --  Generic Linux system
      Generic_MacOS,        --  Generic macOS system
      Apple_Secure_Enclave, --  Apple Secure Enclave
      Intel_SGX,            --  Intel SGX
      AMD_SEV,              --  AMD SEV
      ARM_TrustZone,        --  ARM TrustZone
      RISC_V_Keystone,      --  RISC-V Keystone
      Test_Platform         --  Testing environment
   );

   --  Platform information
   type Platform_Info is record
      Platform_Type : TEE_Platform.Platform_Type;
      Capabilities  : Platform_Capabilities;
      Valid         : Boolean;
   end record;

   --  Test platform (minimal capabilities)
   Test_Platform_Info : constant Platform_Info := (
      Platform_Type => Test_Platform,
      Capabilities  => Zero_Capabilities,
      Valid         => True
   );

   ---------------------------------------------------------------------------
   --  Verification Functions (Pure SPARK)
   ---------------------------------------------------------------------------

   --  Verify timestamp is within acceptable range
   --
   --  Checks that timestamp is:
   --  1. Valid flag is set
   --  2. Not zero (unless Test_Time)
   --  3. Not in the distant future (> current_time + 1 hour)
   --  4. From a trusted source (Block_Time or Hardware_Clock preferred)
   --
   --  Current_Block_Time : Current blockchain timestamp (for bounds checking)
   --  TS                 : Timestamp to verify
   --
   --  Returns: True if timestamp is acceptable
   function Verify_Timestamp (
      Current_Block_Time : Unix_Timestamp;
      TS                 : Timestamped_Value
   ) return Boolean with
      Global => null,
      Post => (if Verify_Timestamp'Result then TS.Valid);

   --  Verify entropy meets minimum quality requirements
   --
   --  Checks that entropy:
   --  1. Valid flag is set
   --  2. Has sufficient size (>= Min_Size)
   --  3. Meets minimum quality level
   --  4. Not all zeros (basic sanity check)
   --
   --  E        : Entropy sample to verify
   --  Min_Size : Minimum required entropy bytes
   --  Min_Qual : Minimum required quality level
   --
   --  Returns: True if entropy meets requirements
   function Verify_Entropy (
      E        : Entropy_Sample;
      Min_Size : Positive;
      Min_Qual : Entropy_Quality
   ) return Boolean with
      Global => null,
      Pre => Min_Size <= Max_Entropy_Size,
      Post => (if Verify_Entropy'Result then E.Valid and E.Size >= Min_Size);

   --  Verify hardware measurement is valid
   --
   --  Checks that measurement:
   --  1. Valid flag is set
   --  2. Hash is not all zeros (unless Test_Fixture)
   --  3. Source is trusted (prefer Hardware_PCR or Enclave_Report)
   --
   --  M : Hardware measurement to verify
   --
   --  Returns: True if measurement is valid
   function Verify_Measurement (
      M : Hardware_Measurement
   ) return Boolean with
      Global => null,
      Post => (if Verify_Measurement'Result then M.Valid);

   --  Verify platform capabilities meet minimum requirements
   --
   --  Checks that platform has required capabilities for secure operation.
   --
   --  Caps          : Platform capabilities
   --  Require_HW_RNG: Require hardware RNG
   --  Require_Enclave: Require enclave support
   --
   --  Returns: True if platform meets requirements
   function Verify_Capabilities (
      Caps            : Platform_Capabilities;
      Require_HW_RNG  : Boolean;
      Require_Enclave : Boolean
   ) return Boolean with
      Global => null,
      Post => (if Verify_Capabilities'Result then
                 (if Require_HW_RNG then Caps.Has_Hardware_RNG) and
                 (if Require_Enclave then Caps.Has_Enclave_Support));

   ---------------------------------------------------------------------------
   --  Comparison Functions (Constant-Time Where Applicable)
   ---------------------------------------------------------------------------

   --  Compare two timestamps (constant-time)
   function Equal_Timestamps (
      A : Timestamped_Value;
      B : Timestamped_Value
   ) return Boolean with
      Global => null;

   --  Compare two measurements (constant-time)
   function Equal_Measurements (
      A : Hardware_Measurement;
      B : Hardware_Measurement
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Convert Word64 timestamp to byte array (little-endian)
   procedure Timestamp_To_Bytes (
      TS    : Unix_Timestamp;
      Bytes : out Byte_Array
   ) with
      Global => null,
      Pre => Bytes'Length = 8,
      Post => Bytes (Bytes'First) = Byte (TS and 16#FF#);

   --  Convert byte array to Word64 timestamp (little-endian)
   function Bytes_To_Timestamp (
      Bytes : Byte_Array
   ) return Unix_Timestamp with
      Global => null,
      Pre => Bytes'Length = 8;

   --  Check if entropy buffer is all zeros
   function Is_Zero_Entropy (
      E : Entropy_Buffer
   ) return Boolean with
      Global => null,
      Post => Is_Zero_Entropy'Result = (for all I in E'Range => E (I) = 0);

   --  Check if measurement is all zeros
   function Is_Zero_Measurement (
      M : Platform_Measurement
   ) return Boolean with
      Global => null,
      Post => Is_Zero_Measurement'Result = (for all I in M'Range => M (I) = 0);

end TEE_Platform;
