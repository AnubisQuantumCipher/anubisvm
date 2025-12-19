-------------------------------------------------------------------------------
--  SCARAB - SEBEK Threshold Signature Protocol
--  Secure Efficient Byzantine-tolerant Efficient Key-sharing
--
--  Standalone threshold signature protocol for post-quantum security:
--  - t-of-n threshold ML-DSA-87 signatures
--  - Supports dynamic threshold changes
--  - Efficient partial signature aggregation
--  - Compatible with AADKG key generation
--
--  Key Features:
--  - BFT-secure (tolerates f < n/3 malicious signers)
--  - Constant verification time regardless of signers
--  - Proactive resharing for long-term security
--  - Audit trails for all signing operations
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Scarab_Sebek with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Protocol Parameters
   ---------------------------------------------------------------------------

   --  Maximum signers in a threshold group
   Max_Signers           : constant := 128;

   --  Maximum threshold value
   Max_Threshold         : constant := 86;  -- 2/3 + 1 of 128

   --  ML-DSA-87 key sizes
   MLDSA_PK_Size         : constant := 2592;
   MLDSA_SK_Size         : constant := 4896;
   MLDSA_Sig_Size        : constant := 4627;

   --  Partial signature size
   Partial_Sig_Size      : constant := 64;

   --  Share size
   Share_Size            : constant := 32;

   --  Commitment size
   Commitment_Size       : constant := 64;

   --  Nonce size
   Nonce_Size            : constant := 32;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Signer_Index is Natural range 0 .. Max_Signers - 1;
   subtype Threshold_Value is Natural range 1 .. Max_Threshold;

   --  Signing key share (held by each signer)
   type Key_Share is record
      Index          : Signer_Index;
      Share_Value    : Byte_Array (0 .. Share_Size - 1);
      Verification   : Byte_Array (0 .. Commitment_Size - 1);
      Epoch          : Unsigned_64;
      Valid          : Boolean;
   end record;

   --  Public key share verification data
   type PK_Share_Verification is record
      Index          : Signer_Index;
      Commitment     : Byte_Array (0 .. Commitment_Size - 1);
   end record;

   type PK_Verification_Array is array (Signer_Index range <>) of PK_Share_Verification;

   --  Threshold public key
   type Threshold_Public_Key is record
      Combined_PK    : Byte_Array (0 .. MLDSA_PK_Size - 1);
      Threshold      : Threshold_Value;
      Num_Signers    : Natural;
      Epoch          : Unsigned_64;
      PK_Hash        : Byte_Array (0 .. 31);  -- For quick comparison
   end record;

   --  Partial signature from one signer
   type Partial_Signature is record
      Signer_Idx     : Signer_Index;
      Partial        : Byte_Array (0 .. Partial_Sig_Size - 1);
      Nonce_Commit   : Byte_Array (0 .. Commitment_Size - 1);
      Timestamp      : Unsigned_64;
      Verified       : Boolean;
   end record;

   type Partial_Sig_Array is array (Natural range <>) of Partial_Signature;

   --  Combined threshold signature
   type Threshold_Signature is record
      Signature      : Byte_Array (0 .. MLDSA_Sig_Size - 1);
      Signers_Used   : Natural;
      Threshold_Met  : Boolean;
      Timestamp      : Unsigned_64;
   end record;

   --  Signing session (for coordinating multi-round signing)
   type Signing_Session is record
      Session_ID     : Byte_Array (0 .. 31);
      Message_Hash   : Byte_Array (0 .. 63);
      Partials       : Partial_Sig_Array (0 .. Max_Signers - 1);
      Num_Partials   : Natural;
      Threshold      : Threshold_Value;
      Started_At     : Unsigned_64;
      Expires_At     : Unsigned_64;
      Finalized      : Boolean;
   end record;

   --  Signer status
   type Signer_Status is (
      Active,        -- Participating normally
      Pending,       -- Awaiting share
      Suspended,     -- Temporarily unavailable
      Revoked,       -- Permanently removed
      Resharing      -- In resharing protocol
   );

   --  Individual signer info
   type Signer_Info is record
      Index          : Signer_Index;
      PK_Hash        : Byte_Array (0 .. 31);
      Status         : Signer_Status;
      Sessions       : Natural;  -- Active sessions
      Last_Active    : Unsigned_64;
   end record;

   type Signer_Info_Array is array (Signer_Index range <>) of Signer_Info;

   --  Threshold group configuration
   type Group_Config is record
      Group_ID       : Byte_Array (0 .. 31);
      Threshold      : Threshold_Value;
      Num_Signers    : Natural;
      TPK            : Threshold_Public_Key;
      Signers        : Signer_Info_Array (0 .. Max_Signers - 1);
      Epoch          : Unsigned_64;
      Created_At     : Unsigned_64;
   end record;

   --  Resharing proof
   type Reshare_Proof is record
      Old_Epoch      : Unsigned_64;
      New_Epoch      : Unsigned_64;
      Old_TPK_Hash   : Byte_Array (0 .. 31);
      New_TPK_Hash   : Byte_Array (0 .. 31);
      Proof          : Byte_Array (0 .. 255);
      Proof_Length   : Natural;
   end record;

   --  Verification result
   type Verify_Result is (
      Valid,
      Invalid_Share,
      Invalid_Partial,
      Invalid_Signature,
      Threshold_Not_Met,
      Session_Expired,
      Signer_Revoked,
      Duplicate_Signer,
      Invalid_Epoch
   );

   --  Signing metrics
   type Signing_Metrics is record
      Total_Sessions    : Unsigned_64;
      Successful_Signs  : Unsigned_64;
      Failed_Signs      : Unsigned_64;
      Avg_Sign_Time_Us  : Unsigned_64;
      Active_Sessions   : Natural;
   end record;

   ---------------------------------------------------------------------------
   --  Group Management
   ---------------------------------------------------------------------------

   --  Create new threshold group from DKG output
   procedure Create_Group (
      Group_ID       : Byte_Array;
      Threshold      : Threshold_Value;
      Num_Signers    : Natural;
      Combined_PK    : Byte_Array;
      Config         : out Group_Config;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Group_ID'Length = 32
             and Combined_PK'Length = MLDSA_PK_Size
             and Num_Signers >= Natural (Threshold)
             and Num_Signers <= Max_Signers,
      Post => (if Success then
               Config.Threshold = Threshold
               and Config.Num_Signers = Num_Signers);

   --  Add signer to group
   procedure Add_Signer (
      Config         : in Out Group_Config;
      Signer_PK_Hash : Byte_Array;
      Index          : out Signer_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Signer_PK_Hash'Length = 32
             and Config.Num_Signers < Max_Signers;

   --  Remove signer from group
   procedure Remove_Signer (
      Config         : in Out Group_Config;
      Index          : Signer_Index;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Index < Config.Num_Signers;

   --  Update signer status
   procedure Update_Signer_Status (
      Config         : in Out Group_Config;
      Index          : Signer_Index;
      New_Status     : Signer_Status
   ) with
      Global => null,
      Pre => Index < Config.Num_Signers;

   --  Get active signer count
   function Active_Signers (Config : Group_Config) return Natural with
      Global => null;

   --  Check if threshold can be met
   function Can_Sign (Config : Group_Config) return Boolean with
      Global => null,
      Post => Can_Sign'Result = (Active_Signers (Config) >= Natural (Config.Threshold));

   ---------------------------------------------------------------------------
   --  Key Share Operations
   ---------------------------------------------------------------------------

   --  Import key share from DKG
   procedure Import_Share (
      Share_Data     : Byte_Array;
      Verification   : Byte_Array;
      Index          : Signer_Index;
      Epoch          : Unsigned_64;
      Share          : out Key_Share;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Share_Data'Length = Share_Size
             and Verification'Length = Commitment_Size;

   --  Verify share against group commitment
   function Verify_Share (
      Share          : Key_Share;
      Config         : Group_Config
   ) return Boolean with
      Global => null;

   --  AEAD constants for share encryption (ChaCha20-Poly1305)
   Share_Nonce_Size  : constant := 12;  -- 96-bit nonce
   Share_Tag_Size    : constant := 16;  -- 128-bit auth tag
   --  Encrypted share format:
   --    [Nonce(12)] [Ciphertext(32)] [Tag(16)] [Verification(64)]
   --  Total: 124 bytes minimum
   Share_Export_Size : constant := Share_Nonce_Size + Share_Size +
                                   Share_Tag_Size + Commitment_Size;

   --  Export share (encrypted for backup using ChaCha20-Poly1305 AEAD)
   --  The share value is encrypted; verification data is authenticated but not encrypted
   procedure Export_Share (
      Share          : Key_Share;
      Encryption_Key : Byte_Array;
      Encrypted      : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Encryption_Key'Length = 32
             and Encrypted'Length >= Share_Export_Size;

   ---------------------------------------------------------------------------
   --  Signing Protocol
   ---------------------------------------------------------------------------

   --  Start new signing session
   procedure Start_Session (
      Config         : Group_Config;
      Message        : Byte_Array;
      Session        : out Signing_Session;
      Success        : out Boolean
   ) with
      Global => null,
      Post => (if Success then
               Session.Num_Partials = 0
               and not Session.Finalized);

   --  Generate partial signature
   procedure Sign_Partial (
      Share          : Key_Share;
      Session        : Signing_Session;
      Partial        : out Partial_Signature;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Share.Valid and not Session.Finalized;

   --  Submit partial signature to session
   procedure Submit_Partial (
      Session        : in Out Signing_Session;
      Partial        : Partial_Signature;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => not Session.Finalized,
      Post => (if Result = Valid then
               Session.Num_Partials = Session.Num_Partials'Old + 1);

   --  Check if threshold is met
   function Threshold_Met (Session : Signing_Session) return Boolean with
      Global => null,
      Post => Threshold_Met'Result =
              (Session.Num_Partials >= Natural (Session.Threshold));

   --  Combine partial signatures into threshold signature
   procedure Combine_Partials (
      Session        : in Out Signing_Session;
      Combined       : out Threshold_Signature;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => Threshold_Met (Session),
      Post => (if Result = Valid then
               Session.Finalized and Combined.Threshold_Met);

   --  Single-shot signing (all partials provided at once)
   procedure Sign_Immediate (
      Config         : Group_Config;
      Message        : Byte_Array;
      Partials       : Partial_Sig_Array;
      Combined       : out Threshold_Signature;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => Partials'Length >= Natural (Config.Threshold);

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   --  Verify threshold signature
   function Verify_Threshold_Sig (
      TPK            : Threshold_Public_Key;
      Message        : Byte_Array;
      Signature      : Threshold_Signature
   ) return Boolean with
      Global => null;

   --  Verify partial signature
   function Verify_Partial (
      Config         : Group_Config;
      Partial        : Partial_Signature;
      Message_Hash   : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Message_Hash'Length = 64;

   --  Batch verify multiple signatures
   procedure Batch_Verify (
      TPK            : Threshold_Public_Key;
      Messages       : Byte_Array;
      Msg_Count      : Natural;
      Signatures     : Threshold_Signature;
      Valid          : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Resharing Protocol
   ---------------------------------------------------------------------------

   --  Start resharing to new threshold/signers
   procedure Start_Reshare (
      Config         : Group_Config;
      New_Threshold  : Threshold_Value;
      New_Signers    : Natural;
      Reshare_ID     : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => New_Signers >= Natural (New_Threshold)
             and Reshare_ID'Length = 32;

   --  Generate reshare contribution
   procedure Generate_Reshare_Share (
      Old_Share      : Key_Share;
      Reshare_ID     : Byte_Array;
      New_Index      : Signer_Index;
      New_Share      : out Byte_Array;
      Proof          : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Old_Share.Valid
             and Reshare_ID'Length = 32
             and New_Share'Length = Share_Size
             and Proof'Length >= 128;

   --  Complete resharing
   procedure Complete_Reshare (
      Config         : in Out Group_Config;
      Proof          : Reshare_Proof;
      Success        : out Boolean
   ) with
      Global => null,
      Post => (if Success then
               Config.Epoch = Proof.New_Epoch);

   --  Verify reshare proof
   function Verify_Reshare (
      Old_TPK        : Threshold_Public_Key;
      Proof          : Reshare_Proof
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Nonce Protocol (for Secure Signing)
   ---------------------------------------------------------------------------

   --  Generate nonce commitment (round 1)
   procedure Generate_Nonce_Commit (
      Share          : Key_Share;
      Session_ID     : Byte_Array;
      Nonce          : out Byte_Array;
      Commitment     : out Byte_Array
   ) with
      Global => null,
      Pre => Session_ID'Length = 32
             and Nonce'Length = Nonce_Size
             and Commitment'Length = Commitment_Size;

   --  Reveal nonce (round 2)
   procedure Reveal_Nonce (
      Commitment     : Byte_Array;
      Nonce          : Byte_Array;
      Valid          : out Boolean
   ) with
      Global => null,
      Pre => Commitment'Length = Commitment_Size
             and Nonce'Length = Nonce_Size;

   --  Aggregate nonces
   procedure Aggregate_Nonces (
      Nonces         : Byte_Array;
      Num_Nonces     : Natural;
      Aggregated     : out Byte_Array
   ) with
      Global => null,
      Pre => Nonces'Length = Num_Nonces * Nonce_Size
             and Aggregated'Length = Nonce_Size;

   ---------------------------------------------------------------------------
   --  Audit Trail
   ---------------------------------------------------------------------------

   --  Audit entry for signing operation
   type Audit_Entry is record
      Session_ID     : Byte_Array (0 .. 31);
      Message_Hash   : Byte_Array (0 .. 31);
      Signers        : Byte_Array (0 .. Max_Signers / 8 - 1);  -- Bitmap
      Timestamp      : Unsigned_64;
      Success        : Boolean;
   end record;

   --  Log signing operation
   procedure Log_Sign_Operation (
      Session        : Signing_Session;
      Result         : Verify_Result;
      Audit_Log      : out Audit_Entry
   ) with
      Global => null;

   --  Verify audit trail integrity
   function Verify_Audit_Entry (
      Audit_Log      : Audit_Entry;
      TPK            : Threshold_Public_Key
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   --  Serialize threshold signature
   procedure Serialize_Signature (
      Sig            : Threshold_Signature;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= MLDSA_Sig_Size + 64;

   --  Deserialize threshold signature
   procedure Deserialize_Signature (
      Input          : Byte_Array;
      Sig            : out Threshold_Signature;
      Success        : out Boolean
   ) with
      Global => null;

   --  Serialize group config
   procedure Serialize_Config (
      Config         : Group_Config;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 65536;  -- Max config size

   --  Deserialize group config
   procedure Deserialize_Config (
      Input          : Byte_Array;
      Config         : out Group_Config;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Metrics and Diagnostics
   ---------------------------------------------------------------------------

   --  Get signing metrics
   function Get_Metrics (Config : Group_Config) return Signing_Metrics with
      Global => null;

   --  Estimate signing time
   function Estimate_Sign_Time (
      Num_Signers    : Natural;
      Network_Latency: Natural  -- Milliseconds
   ) return Natural with
      Global => null,
      Pre => Num_Signers <= Max_Signers;

   --  Get session status
   function Session_Status (Session : Signing_Session) return Signer_Status with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Share (Share : in Out Key_Share) with
      Global => null,
      Post => not Share.Valid;

   procedure Zeroize_Session (Session : in Out Signing_Session) with
      Global => null;

   procedure Zeroize_Partial (Partial : in Out Partial_Signature) with
      Global => null;

end Scarab_Sebek;
