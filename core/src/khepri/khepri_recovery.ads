-------------------------------------------------------------------------------
--  KHEPRI - Social Recovery System
--  Lattice-Based Secret Sharing for Post-Quantum Key Recovery
--
--  Implements a quantum-resistant secret sharing scheme based on the
--  Learning With Errors (LWE) problem. Traditional Shamir's Secret Sharing
--  relies on finite field arithmetic vulnerable to quantum attacks.
--
--  Key Features:
--  - K-of-N threshold recovery (configurable)
--  - LWE-based security (quantum-resistant)
--  - Guardian network with time-locks
--  - Anti-coercion protection (72-hour delay)
--  - On-chain recovery coordination
--
--  Security: Based on Module-LWE hardness assumption (same as ML-KEM)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Khepri_Recovery with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum guardians in a recovery setup
   Max_Guardians         : constant := 16;

   --  Default threshold (3-of-5 recommended)
   Default_Threshold     : constant := 3;
   Default_Total         : constant := 5;

   --  Share sizes (based on LWE parameters for 128-bit post-quantum security)
   Share_Size            : constant := 256;   -- Bytes per share
   Commitment_Size       : constant := 64;    -- Commitment to share
   Proof_Size            : constant := 128;   -- Share validity proof

   --  Master seed size (what we're protecting)
   Master_Seed_Size      : constant := 64;

   --  Time-lock parameters
   Min_Timelock_Hours    : constant := 24;
   Default_Timelock_Hours: constant := 72;
   Max_Timelock_Hours    : constant := 168;   -- 7 days

   --  LWE parameters (n=256, q=3329 - same as Kyber)
   LWE_N                 : constant := 256;
   LWE_Q                 : constant := 3329;
   LWE_Eta               : constant := 2;     -- Noise parameter

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Guardian_Index is Natural range 0 .. Max_Guardians - 1;
   subtype Threshold_Value is Natural range 2 .. Max_Guardians;

   --  Share data
   type Recovery_Share is record
      Index          : Guardian_Index;
      Share_Data     : Byte_Array (0 .. Share_Size - 1);
      Commitment     : Byte_Array (0 .. Commitment_Size - 1);
      Proof          : Byte_Array (0 .. Proof_Size - 1);
      Guardian_PK    : Byte_Array (0 .. 31);  -- SHA3-256 of guardian's ML-DSA PK
      Created_At     : Unsigned_64;
      Valid          : Boolean;
   end record;

   type Share_Array is array (Guardian_Index range <>) of Recovery_Share;

   --  Guardian information
   type Guardian_Status is (
      Active,        -- Guardian can participate in recovery
      Pending,       -- Awaiting confirmation
      Revoked,       -- Guardian removed
      Unresponsive   -- Failed to respond to recovery
   );

   type Guardian_Info is record
      Index          : Guardian_Index;
      PK_Hash        : Byte_Array (0 .. 31);
      Name_Hash      : Byte_Array (0 .. 31);  -- Privacy-preserving name
      Status         : Guardian_Status;
      Added_At       : Unsigned_64;
      Last_Confirmed : Unsigned_64;
   end record;

   type Guardian_Array is array (Guardian_Index range <>) of Guardian_Info;

   --  Recovery configuration
   type Recovery_Config is record
      Owner_PK_Hash  : Byte_Array (0 .. 31);
      Threshold      : Threshold_Value;
      Total_Guardians: Natural;
      Guardians      : Guardian_Array (0 .. Max_Guardians - 1);
      Timelock_Hours : Natural;
      Created_At     : Unsigned_64;
      Version        : Natural;
      Active         : Boolean;
   end record;

   --  Recovery session (in-progress recovery)
   type Recovery_Session is record
      Session_ID     : Byte_Array (0 .. 31);
      Config_Hash    : Byte_Array (0 .. 31);
      Initiator      : Byte_Array (0 .. 31);  -- Who started recovery
      Submitted      : Share_Array (0 .. Max_Guardians - 1);
      Num_Submitted  : Natural;
      Started_At     : Unsigned_64;
      Timelock_End   : Unsigned_64;
      Cancelled      : Boolean;
      Completed      : Boolean;
   end record;

   --  Recovery result
   type Recovery_Result is (
      Success,
      Insufficient_Shares,
      Invalid_Share,
      Timelock_Active,
      Session_Cancelled,
      Session_Expired,
      Verification_Failed,
      Already_Recovered
   );

   --  Share submission result
   type Submit_Result is (
      Accepted,
      Duplicate_Guardian,
      Invalid_Proof,
      Session_Closed,
      Not_A_Guardian,
      Share_Corrupted
   );

   ---------------------------------------------------------------------------
   --  LWE Types (for lattice operations)
   ---------------------------------------------------------------------------

   --  LWE vector (coefficients mod q)
   subtype LWE_Coeff is Natural range 0 .. LWE_Q - 1;
   type LWE_Vector is array (0 .. LWE_N - 1) of LWE_Coeff;

   --  LWE sample (a, b = <a,s> + e + m*q/2)
   type LWE_Sample is record
      A : LWE_Vector;
      B : LWE_Coeff;
   end record;

   type LWE_Sample_Array is array (Natural range <>) of LWE_Sample;

   ---------------------------------------------------------------------------
   --  Configuration Management
   ---------------------------------------------------------------------------

   --  Create new recovery configuration
   procedure Create_Config (
      Owner_PK_Hash  : Byte_Array;
      Threshold      : Threshold_Value;
      Timelock_Hours : Natural;
      Config         : out Recovery_Config;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Owner_PK_Hash'Length = 32
             and Timelock_Hours >= Min_Timelock_Hours
             and Timelock_Hours <= Max_Timelock_Hours,
      Post => (if Success then
               Config.Active
               and Config.Threshold = Threshold
               and Config.Total_Guardians = 0);

   --  Add guardian to configuration
   procedure Add_Guardian (
      Config         : in out Recovery_Config;
      Guardian_PK    : Byte_Array;
      Guardian_Name  : Byte_Array;  -- Will be hashed
      Index          : out Guardian_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Guardian_PK'Length = 32
             and Guardian_Name'Length <= 256
             and Config.Total_Guardians < Max_Guardians,
      Post => (if Success then
               Config.Total_Guardians = Config.Total_Guardians'Old + 1);

   --  Remove guardian from configuration
   procedure Remove_Guardian (
      Config         : in Out Recovery_Config;
      Index          : Guardian_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Index < Config.Total_Guardians;

   --  Update guardian status
   procedure Update_Guardian_Status (
      Config         : in Out Recovery_Config;
      Index          : Guardian_Index;
      New_Status     : Guardian_Status
   ) with
      Global => null,
      Pre => Index < Config.Total_Guardians;

   --  Check if recovery is possible (enough active guardians)
   function Can_Recover (Config : Recovery_Config) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Share Generation (LWE-based)
   ---------------------------------------------------------------------------

   --  Generate all shares from master seed
   procedure Generate_Shares (
      Master_Seed    : Byte_Array;
      Config         : Recovery_Config;
      Shares         : out Share_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Master_Seed'Length = Master_Seed_Size
             and Config.Active
             and Config.Total_Guardians >= Natural (Config.Threshold)
             and Shares'Length >= Config.Total_Guardians,
      Post => (if Success then
               (for all I in 0 .. Config.Total_Guardians - 1 =>
                  Shares (Guardian_Index (I)).Valid));

   --  Generate single share for specific guardian
   procedure Generate_Single_Share (
      Master_Seed    : Byte_Array;
      Config         : Recovery_Config;
      Guardian_Idx   : Guardian_Index;
      Share          : out Recovery_Share;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Master_Seed'Length = Master_Seed_Size
             and Guardian_Idx < Config.Total_Guardians;

   --  Verify share is valid (matches commitment)
   function Verify_Share (
      Share          : Recovery_Share;
      Config         : Recovery_Config
   ) return Boolean with
      Global => null;

   --  Encrypt share for guardian (using their public key)
   procedure Encrypt_Share_For_Guardian (
      Share          : Recovery_Share;
      Guardian_PK    : Byte_Array;
      Encrypted      : out Byte_Array;
      Enc_Length     : out Natural
   ) with
      Global => null,
      Pre => Guardian_PK'Length = 2592  -- ML-DSA-87 public key
             and Encrypted'Length >= Share_Size + 128;

   ---------------------------------------------------------------------------
   --  Recovery Session Management
   ---------------------------------------------------------------------------

   --  Start recovery session
   procedure Start_Recovery (
      Config         : Recovery_Config;
      Initiator_PK   : Byte_Array;
      Session        : out Recovery_Session;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Initiator_PK'Length = 32 and Config.Active,
      Post => (if Success then
               not Session.Completed
               and Session.Num_Submitted = 0);

   --  Submit share to recovery session
   procedure Submit_Share (
      Session        : in Out Recovery_Session;
      Share          : Recovery_Share;
      Guardian_Sig   : Byte_Array;
      Result         : out Submit_Result
   ) with
      Global => null,
      Pre => Guardian_Sig'Length = 4627  -- ML-DSA-87 signature
             and not Session.Completed,
      Post => (if Result = Accepted then
               Session.Num_Submitted = Session.Num_Submitted'Old + 1);

   --  Cancel recovery (by original owner)
   procedure Cancel_Recovery (
      Session        : in Out Recovery_Session;
      Owner_Sig      : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Owner_Sig'Length = 4627
             and not Session.Completed,
      Post => (if Success then Session.Cancelled);

   --  Check if timelock has expired
   function Timelock_Expired (
      Session        : Recovery_Session;
      Current_Time   : Unsigned_64
   ) return Boolean with
      Global => null,
      Post => Timelock_Expired'Result = (Current_Time >= Session.Timelock_End);

   --  Get recovery progress
   function Recovery_Progress (
      Session        : Recovery_Session;
      Threshold      : Threshold_Value
   ) return Natural with
      Global => null,
      Post => Recovery_Progress'Result <= 100;

   ---------------------------------------------------------------------------
   --  Secret Reconstruction (LWE-based)
   ---------------------------------------------------------------------------

   --  Reconstruct master seed from shares
   procedure Reconstruct_Seed (
      Session        : Recovery_Session;
      Config         : Recovery_Config;
      Current_Time   : Unsigned_64;
      Recovered_Seed : out Byte_Array;
      Result         : out Recovery_Result
   ) with
      Global => null,
      Pre => Recovered_Seed'Length = Master_Seed_Size,
      Post => (if Result = Success then
               Session.Num_Submitted >= Natural (Config.Threshold));

   --  Verify reconstructed seed matches original (via commitment)
   function Verify_Reconstruction (
      Recovered_Seed : Byte_Array;
      Config         : Recovery_Config
   ) return Boolean with
      Global => null,
      Pre => Recovered_Seed'Length = Master_Seed_Size;

   ---------------------------------------------------------------------------
   --  LWE Operations (Core Lattice Cryptography)
   ---------------------------------------------------------------------------

   --  Generate LWE secret vector
   procedure Generate_LWE_Secret (
      Seed           : Byte_Array;
      Secret         : out LWE_Vector
   ) with
      Global => null,
      Pre => Seed'Length >= 32;

   --  LWE encryption of single bit
   procedure LWE_Encrypt_Bit (
      Public_A       : LWE_Vector;
      Secret         : LWE_Vector;
      Bit            : Boolean;
      Sample         : out LWE_Sample
   ) with
      Global => null;

   --  LWE decryption
   function LWE_Decrypt_Bit (
      Secret         : LWE_Vector;
      Sample         : LWE_Sample
   ) return Boolean with
      Global => null;

   --  Encode bytes as LWE samples
   procedure Encode_To_LWE (
      Data           : Byte_Array;
      Secret         : LWE_Vector;
      Samples        : out LWE_Sample_Array
   ) with
      Global => null,
      Pre => Samples'Length >= Data'Length * 8;

   --  Decode LWE samples to bytes
   procedure Decode_From_LWE (
      Samples        : LWE_Sample_Array;
      Secret         : LWE_Vector;
      Data           : out Byte_Array
   ) with
      Global => null,
      Pre => Data'Length * 8 <= Samples'Length;

   ---------------------------------------------------------------------------
   --  Share Combination (Threshold Reconstruction)
   ---------------------------------------------------------------------------

   --  Combine shares using lattice-based reconstruction
   procedure Combine_Shares (
      Shares         : Share_Array;
      Num_Shares     : Natural;
      Threshold      : Threshold_Value;
      Result         : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Num_Shares >= Natural (Threshold)
             and Result'Length = Master_Seed_Size;

   --  Generate Lagrange-like coefficients for lattice interpolation
   procedure Generate_Reconstruction_Coeffs (
      Indices        : Share_Array;
      Num_Shares     : Natural;
      Coeffs         : out Byte_Array
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Guardian Proofs
   ---------------------------------------------------------------------------

   --  Generate proof that guardian holds valid share
   procedure Generate_Share_Proof (
      Share          : Recovery_Share;
      Guardian_SK    : Byte_Array;
      Proof          : out Byte_Array
   ) with
      Global => null,
      Pre => Guardian_SK'Length = 4896  -- ML-DSA-87 secret key
             and Proof'Length >= Proof_Size;

   --  Verify guardian's share proof
   function Verify_Share_Proof (
      Share          : Recovery_Share;
      Guardian_PK    : Byte_Array;
      Proof          : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Guardian_PK'Length = 2592
             and Proof'Length >= Proof_Size;

   ---------------------------------------------------------------------------
   --  On-Chain Coordination
   ---------------------------------------------------------------------------

   --  Compute commitment for on-chain recording
   procedure Compute_Config_Hash (
      Config         : Recovery_Config;
      Hash           : out Byte_Array
   ) with
      Global => null,
      Pre => Hash'Length = 32;

   --  Serialize recovery session for on-chain storage
   procedure Serialize_Session (
      Session        : Recovery_Session;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 4096;

   --  Deserialize recovery session
   procedure Deserialize_Session (
      Input          : Byte_Array;
      Session        : out Recovery_Session;
      Success        : out Boolean
   ) with
      Global => null;

   --  Generate on-chain recovery initiation transaction
   procedure Create_Recovery_TX (
      Session        : Recovery_Session;
      TX_Data        : out Byte_Array;
      TX_Length      : out Natural
   ) with
      Global => null,
      Pre => TX_Data'Length >= 1024;

   ---------------------------------------------------------------------------
   --  Security Features
   ---------------------------------------------------------------------------

   --  Check for coercion indicators
   function Check_Coercion_Risk (
      Session        : Recovery_Session;
      Config         : Recovery_Config
   ) return Boolean with
      Global => null;

   --  Extend timelock (anti-coercion measure)
   procedure Extend_Timelock (
      Session        : in Out Recovery_Session;
      Additional_Hours : Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Additional_Hours <= 168
             and not Session.Completed;

   --  Generate decoy shares (honeypot)
   procedure Generate_Decoy_Shares (
      Config         : Recovery_Config;
      Num_Decoys     : Natural;
      Decoys         : out Share_Array
   ) with
      Global => null,
      Pre => Num_Decoys <= Max_Guardians
             and Decoys'Length >= Num_Decoys;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Share (Share : in Out Recovery_Share) with
      Global => null,
      Post => not Share.Valid;

   procedure Zeroize_Session (Session : in Out Recovery_Session) with
      Global => null;

   procedure Zeroize_Config (Config : in Out Recovery_Config) with
      Global => null,
      Post => not Config.Active;

   procedure Zeroize_LWE_Vector (V : in Out LWE_Vector) with
      Global => null;

end Khepri_Recovery;
