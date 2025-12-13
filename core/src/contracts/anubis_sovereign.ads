pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with CVM_Interface; use CVM_Interface;

--  AnubisSovereign: Post-Quantum Self-Sovereign Identity System
--
--  A comprehensive identity contract demonstrating ALL AnubisVM capabilities:
--
--  ============================================================================
--  CRYPTOGRAPHIC PRIMITIVES USED:
--  ============================================================================
--  - ML-DSA-87 (FIPS 204): Sign credentials, attestations, revocations
--  - ML-KEM-1024 (FIPS 203): Secure key exchange with verifiers
--  - SHA3-256: Credential hashes, merkle tree nodes
--  - KMAC256: Domain-separated credential identifiers
--  - ChaCha20-Poly1305 AEAD: Encrypt credentials at rest
--  - KDF (KMAC-based): Derive per-verifier keys
--
--  ============================================================================
--  PRIVACY LAYERS USED:
--  ============================================================================
--  - SHIELD: Encrypted private credential storage
--    * Credentials encrypted with ML-KEM for owner
--    * Ajtai commitments for ZK proofs
--
--  - EYE: Selective Attribute Disclosure
--    * Prove age >= 18 without revealing DOB
--    * Issue viewing keys for auditors
--    * Credential verification without full disclosure
--
--  - GATE: Private Verification Sessions
--    * Encrypted communication with verifiers
--    * ZK proofs of credential validity
--    * Multi-party credential verification
--
--  - WHISPER: Confidential Reputation Scores
--    * Hidden reputation values with range proofs
--    * Homomorphic reputation updates
--    * Balance proofs for reputation transfers
--
--  - VEIL: Anonymous Credential Presentation
--    * Ring signatures for anonymous verification
--    * Key images prevent double-presentation
--    * k-anonymity among credential holders
--
--  ============================================================================
--  TEE FEATURES USED:
--  ============================================================================
--  - Attestation: Prove trusted issuance environment
--  - Key Management: Hierarchical issuer key derivation
--  - Seal/Unseal: Persistent encrypted credential storage
--
--  ============================================================================
--  STATE MANAGEMENT:
--  ============================================================================
--  - CVM: 256 state slots with atomic updates
--  - Khepri MPT: Merkle proofs for credential existence
--  - Snapshots: Rollback on verification failure
--
--  ============================================================================
--  STATE LAYOUT (256 slots):
--  ============================================================================
--  [0]       : Contract configuration hash
--  [1]       : Issuer registry root (MPT hash)
--  [2]       : Credential registry root (MPT hash)
--  [3]       : Revocation registry root (MPT hash)
--  [4]       : Total credentials issued
--  [5]       : Total credentials revoked
--  [6]       : Total verifications performed
--  [7]       : Reputation pool commitment
--  [8-15]    : Trusted issuer public key hashes (8 issuers)
--  [16-31]   : Schema definitions (16 schemas)
--  [32-95]   : Credential entries (64 slots, hashed by ID)
--  [96-127]  : Reputation scores (32 slots, confidential)
--  [128-159] : Verification sessions (32 active sessions)
--  [160-191] : Ring signature key images (spent tracker)
--  [192-223] : Audit log entries
--  [224-255] : Recovery & governance
--
--  ============================================================================
--  CREDENTIAL TYPES:
--  ============================================================================
--  1. Identity Credential: Name, DOB, Photo hash, Nationality
--  2. Age Credential: DOB, Age verification signature
--  3. Employment Credential: Employer, Title, Start date
--  4. Education Credential: Institution, Degree, Year
--  5. Membership Credential: Organization, Role, Expiry
--  6. Reputation Credential: Score (confidential), Category

package Anubis_Sovereign with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Identity parameters
   Max_Issuers          : constant := 8;
   Max_Schemas          : constant := 16;
   Max_Credentials      : constant := 64;
   Max_Reputation_Slots : constant := 32;
   Max_Sessions         : constant := 32;
   Max_Key_Images       : constant := 32;
   Max_Audit_Entries    : constant := 32;
   Max_Attributes       : constant := 16;

   --  Cryptographic sizes
   Credential_ID_Size   : constant := 32;   -- SHA3-256 hash
   DSA_PK_Size          : constant := 2592; -- ML-DSA-87 public key
   DSA_SK_Size          : constant := 4896; -- ML-DSA-87 secret key
   DSA_SIG_Size         : constant := 4627; -- ML-DSA-87 signature
   KEM_EK_Size          : constant := 1568; -- ML-KEM-1024 encaps key
   KEM_DK_Size          : constant := 3168; -- ML-KEM-1024 decaps key
   KEM_CT_Size          : constant := 1568; -- ML-KEM-1024 ciphertext
   KEM_SS_Size          : constant := 32;   -- Shared secret
   Commitment_Size      : constant := 64;   -- Ajtai commitment
   Range_Proof_Size     : constant := 2048; -- WHISPER range proof
   Ring_Sig_Size        : constant := 4096; -- VEIL ring signature
   Key_Image_Size       : constant := 64;   -- Ring sig key image

   --  Time constants
   Credential_Max_Age   : constant := 31536000;  -- 1 year in seconds
   Session_Timeout      : constant := 3600;      -- 1 hour
   Revocation_Delay     : constant := 86400;     -- 24 hours

   --  State slot assignments
   Config_Slot          : constant State_Index := 0;
   Issuer_Root_Slot     : constant State_Index := 1;
   Cred_Root_Slot       : constant State_Index := 2;
   Revoke_Root_Slot     : constant State_Index := 3;
   Total_Issued_Slot    : constant State_Index := 4;
   Total_Revoked_Slot   : constant State_Index := 5;
   Total_Verified_Slot  : constant State_Index := 6;
   Rep_Pool_Slot        : constant State_Index := 7;
   Issuer_Slots_Base    : constant State_Index := 8;
   Schema_Slots_Base    : constant State_Index := 16;
   Cred_Slots_Base      : constant State_Index := 32;
   Rep_Slots_Base       : constant State_Index := 96;
   Session_Slots_Base   : constant State_Index := 128;
   Key_Image_Slots_Base : constant State_Index := 160;
   Audit_Slots_Base     : constant State_Index := 192;
   Recovery_Slots_Base  : constant State_Index := 224;

   ---------------------------------------------------------------------------
   --  Method Selectors (SHA3-256 of method name, first 4 bytes)
   ---------------------------------------------------------------------------

   --  Issuer management
   Register_Issuer_Selector    : constant Hash256 := (16#a1#, 16#b2#, 16#c3#, 16#d4#, others => 0);
   Revoke_Issuer_Selector      : constant Hash256 := (16#e5#, 16#f6#, 16#07#, 16#18#, others => 0);
   Get_Issuer_Selector         : constant Hash256 := (16#29#, 16#3a#, 16#4b#, 16#5c#, others => 0);

   --  Credential lifecycle
   Issue_Credential_Selector   : constant Hash256 := (16#6d#, 16#7e#, 16#8f#, 16#90#, others => 0);
   Present_Credential_Selector : constant Hash256 := (16#a1#, 16#b2#, 16#c3#, 16#d5#, others => 0);
   Revoke_Credential_Selector  : constant Hash256 := (16#e6#, 16#f7#, 16#08#, 16#19#, others => 0);
   Check_Credential_Selector   : constant Hash256 := (16#2a#, 16#3b#, 16#4c#, 16#5d#, others => 0);

   --  Selective disclosure (EYE)
   Create_Disclosure_Selector  : constant Hash256 := (16#6e#, 16#7f#, 16#80#, 16#91#, others => 0);
   Verify_Disclosure_Selector  : constant Hash256 := (16#a2#, 16#b3#, 16#c4#, 16#d6#, others => 0);
   Issue_View_Key_Selector     : constant Hash256 := (16#e7#, 16#f8#, 16#09#, 16#1a#, others => 0);

   --  Private verification (GATE)
   Start_Session_Selector      : constant Hash256 := (16#2b#, 16#3c#, 16#4d#, 16#5e#, others => 0);
   Verify_Private_Selector     : constant Hash256 := (16#6f#, 16#70#, 16#81#, 16#92#, others => 0);
   End_Session_Selector        : constant Hash256 := (16#a3#, 16#b4#, 16#c5#, 16#d7#, others => 0);

   --  Confidential reputation (WHISPER)
   Issue_Rep_Score_Selector    : constant Hash256 := (16#e8#, 16#f9#, 16#0a#, 16#1b#, others => 0);
   Update_Rep_Score_Selector   : constant Hash256 := (16#2c#, 16#3d#, 16#4e#, 16#5f#, others => 0);
   Prove_Rep_Range_Selector    : constant Hash256 := (16#60#, 16#71#, 16#82#, 16#93#, others => 0);

   --  Anonymous presentation (VEIL)
   Anon_Present_Selector       : constant Hash256 := (16#a4#, 16#b5#, 16#c6#, 16#d8#, others => 0);
   Verify_Ring_Sig_Selector    : constant Hash256 := (16#e9#, 16#fa#, 16#0b#, 16#1c#, others => 0);
   Check_Key_Image_Selector    : constant Hash256 := (16#2d#, 16#3e#, 16#4f#, 16#50#, others => 0);

   --  TEE attestation
   Request_Attestation_Selector : constant Hash256 := (16#61#, 16#72#, 16#83#, 16#94#, others => 0);
   Verify_Attestation_Selector  : constant Hash256 := (16#a5#, 16#b6#, 16#c7#, 16#d9#, others => 0);

   --  Query methods
   Get_Credential_Selector     : constant Hash256 := (16#ea#, 16#fb#, 16#0c#, 16#1d#, others => 0);
   Get_Rep_Commitment_Selector : constant Hash256 := (16#2e#, 16#3f#, 16#40#, 16#51#, others => 0);
   Get_Stats_Selector          : constant Hash256 := (16#62#, 16#73#, 16#84#, 16#95#, others => 0);

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Credential schema types
   type Schema_Type is (
      Identity_Schema,      -- Full identity (name, DOB, nationality)
      Age_Schema,           -- Age verification only
      Employment_Schema,    -- Employment credentials
      Education_Schema,     -- Academic credentials
      Membership_Schema,    -- Organization membership
      Reputation_Schema     -- Confidential reputation score
   );

   --  Attribute value (flexible encoding)
   type Attribute_Value is record
      Data     : Byte_Array (0 .. 63);   -- Attribute data
      Length   : Natural;                 -- Actual length
      Encrypted: Boolean;                 -- Is value encrypted?
   end record;

   type Attribute_Array is array (0 .. Max_Attributes - 1) of Attribute_Value;

   --  Credential structure
   type Credential is record
      --  Core identity
      Credential_ID    : Byte_Array (0 .. Credential_ID_Size - 1);
      Schema           : Schema_Type;
      Holder_Address   : Address;

      --  Attributes (some may be encrypted via SHIELD)
      Attributes       : Attribute_Array;
      Attr_Count       : Natural;

      --  Cryptographic proofs
      Issuer_Signature : Byte_Array (0 .. DSA_SIG_Size - 1);  -- ML-DSA-87
      Holder_Commit    : Byte_Array (0 .. Commitment_Size - 1); -- Ajtai

      --  Encrypted version (SHIELD)
      Encrypted_Data   : Byte_Array (0 .. 1023);
      Encrypted_Len    : Natural;
      KEM_Ciphertext   : Byte_Array (0 .. KEM_CT_Size - 1);

      --  Metadata
      Issued_At        : Unsigned_64;
      Expires_At       : Unsigned_64;
      Revoked          : Boolean;
      Active           : Boolean;
   end record;

   --  Trusted issuer record
   type Issuer_Record is record
      Issuer_Address   : Address;
      DSA_Public_Key   : Byte_Array (0 .. DSA_PK_Size - 1);
      KEM_Public_Key   : Byte_Array (0 .. KEM_EK_Size - 1);
      Allowed_Schemas  : Unsigned_16;  -- Bitmask of allowed schemas
      Credential_Count : Natural;
      Active           : Boolean;
   end record;

   --  Selective disclosure proof (EYE)
   type Disclosure_Proof is record
      Credential_Hash  : Byte_Array (0 .. 31);
      Disclosed_Mask   : Unsigned_16;        -- Which attributes revealed
      Proof_Data       : Byte_Array (0 .. 511);
      Issuer_PK_Hash   : Byte_Array (0 .. 31);
   end record;

   --  Private verification session (GATE)
   type Verification_Session is record
      Session_ID       : Byte_Array (0 .. 31);
      Verifier_Address : Address;
      Holder_Address   : Address;
      Shared_Secret    : Byte_Array (0 .. KEM_SS_Size - 1);
      Encrypted_Cred   : Byte_Array (0 .. 2047);
      Encrypted_Len    : Natural;
      Created_At       : Unsigned_64;
      Expires_At       : Unsigned_64;
      Active           : Boolean;
   end record;

   --  Confidential reputation (WHISPER)
   type Reputation_Score is record
      Holder_Address   : Address;
      Commitment       : Byte_Array (0 .. Commitment_Size - 1);
      Range_Proof      : Byte_Array (0 .. Range_Proof_Size - 1);
      Category         : Unsigned_8;
      Last_Updated     : Unsigned_64;
      Active           : Boolean;
   end record;

   --  Anonymous presentation (VEIL)
   type Anonymous_Presentation is record
      Credential_Hash  : Byte_Array (0 .. 31);
      Ring_Signature   : Byte_Array (0 .. Ring_Sig_Size - 1);
      Key_Image        : Byte_Array (0 .. Key_Image_Size - 1);
      Ring_Size        : Natural;
      Timestamp        : Unsigned_64;
   end record;

   --  Attestation quote (TEE)
   type Attestation_Quote is record
      TEE_Code_Hash    : Byte_Array (0 .. 31);
      Config_Hash      : Byte_Array (0 .. 31);
      State_Root       : Byte_Array (0 .. 31);
      Nonce            : Byte_Array (0 .. 31);
      Signature        : Byte_Array (0 .. DSA_SIG_Size - 1);
      Timestamp        : Unsigned_64;
      Valid            : Boolean;
   end record;

   --  Audit log entry
   type Audit_Entry is record
      Operation        : Unsigned_8;
      Actor_Hash       : Unsigned_64;
      Target_Hash      : Unsigned_64;
      Timestamp        : Unsigned_64;
      Block_Height     : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  ISSUER MANAGEMENT
   ---------------------------------------------------------------------------

   --  Register a trusted credential issuer
   --  Requires governance approval (multi-sig)
   --  Params: issuer_address (32) + dsa_pk (2592) + kem_pk (1568) + schemas (2)
   procedure Register_Issuer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Revoke an issuer (their credentials remain valid but no new ones)
   procedure Revoke_Issuer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  CREDENTIAL LIFECYCLE (with SHIELD encryption)
   ---------------------------------------------------------------------------

   --  Issue credential to holder
   --  Uses: ML-DSA-87 signature, SHIELD encryption, Ajtai commitment
   --  Params: schema (1) + holder (32) + attributes (variable) + signature
   procedure Issue_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Present credential (holder proves ownership)
   --  Uses: ML-DSA-87 verification, commitment opening
   procedure Present_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Revoke credential (issuer or holder)
   procedure Revoke_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Check credential validity
   procedure Check_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  SELECTIVE DISCLOSURE (EYE)
   ---------------------------------------------------------------------------

   --  Create selective disclosure proof
   --  Prove specific attributes without revealing others
   --  Example: Prove age >= 18 without revealing exact DOB
   --  Uses: EYE disclosure proofs, viewing keys
   procedure Create_Disclosure (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Verify selective disclosure proof
   procedure Verify_Disclosure (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Issue viewing key to auditor
   --  Uses: EYE key derivation, hierarchical viewing keys
   procedure Issue_View_Key (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  PRIVATE VERIFICATION (GATE)
   ---------------------------------------------------------------------------

   --  Start private verification session
   --  Uses: ML-KEM-1024 key exchange, GATE session management
   procedure Start_Session (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Verify credential privately within session
   --  Uses: GATE ZK execution proofs
   procedure Verify_Private (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  End verification session (cleanup)
   procedure End_Session (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  CONFIDENTIAL REPUTATION (WHISPER)
   ---------------------------------------------------------------------------

   --  Issue confidential reputation score
   --  Uses: WHISPER Ajtai commitments, range proofs
   --  Score is hidden but provably in [0, max_score]
   procedure Issue_Rep_Score (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Update reputation (homomorphic addition)
   --  Uses: WHISPER homomorphic commitment properties
   procedure Update_Rep_Score (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Prove reputation in range [min, max]
   --  Uses: WHISPER range proofs
   procedure Prove_Rep_Range (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  ANONYMOUS PRESENTATION (VEIL)
   ---------------------------------------------------------------------------

   --  Present credential anonymously via ring signature
   --  Uses: VEIL lattice-based ring signatures
   --  Proves membership in holder set without revealing identity
   --  Key image prevents double-presentation
   procedure Anon_Present (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Verify ring signature (public verification)
   procedure Verify_Ring_Sig (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Check if key image was already used
   --  Prevents double-presentation of anonymous credentials
   procedure Check_Key_Image (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  TEE ATTESTATION
   ---------------------------------------------------------------------------

   --  Request TEE attestation for credential issuance environment
   --  Uses: TEE_Attestation report generation, ML-DSA-87 signing
   procedure Request_Attestation (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Verify attestation quote
   procedure Verify_Attestation (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  QUERY METHODS
   ---------------------------------------------------------------------------

   --  Get credential by ID
   procedure Get_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get reputation commitment (public)
   procedure Get_Rep_Commitment (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => State, State => State, null => Context),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   --  Get contract statistics
   procedure Get_Stats (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => (Result => State, State => State, null => Context),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor with
      Global => null;

   --  Initialize contract with initial issuers
   --  Init_Params: config_hash (32) + num_issuers (1) + issuer_data (variable)
   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) with
      Global  => null,
      Depends => ((State, Success) => Init_Params);

   --  Main execution dispatcher
   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) with
      Global  => null,
      Depends => ((Result, State) => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Is_Valid_Exec_Result (Result);

private

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Get credential slot from ID
   function Get_Credential_Slot (Cred_ID : Byte_Array) return State_Index with
      Global => null,
      Pre    => Cred_ID'Length = Credential_ID_Size,
      Post   => Get_Credential_Slot'Result in Cred_Slots_Base ..
                Cred_Slots_Base + Max_Credentials - 1;

   --  Get reputation slot from holder address
   function Get_Rep_Slot (Holder : Address) return State_Index with
      Global => null,
      Post   => Get_Rep_Slot'Result in Rep_Slots_Base ..
                Rep_Slots_Base + Max_Reputation_Slots - 1;

   --  Get session slot from session ID
   function Get_Session_Slot (Session_ID : Byte_Array) return State_Index with
      Global => null,
      Pre    => Session_ID'Length = 32,
      Post   => Get_Session_Slot'Result in Session_Slots_Base ..
                Session_Slots_Base + Max_Sessions - 1;

   --  Get key image slot
   function Get_Key_Image_Slot (Image : Byte_Array) return State_Index with
      Global => null,
      Pre    => Image'Length = Key_Image_Size,
      Post   => Get_Key_Image_Slot'Result in Key_Image_Slots_Base ..
                Key_Image_Slots_Base + Max_Key_Images - 1;

   --  Compute credential ID from content hash
   procedure Compute_Credential_ID (
      Schema      : Schema_Type;
      Holder      : Address;
      Attributes  : Attribute_Array;
      Attr_Count  : Natural;
      Cred_ID     : out Byte_Array
   ) with
      Global => null,
      Pre    => Cred_ID'Length = Credential_ID_Size and
                Attr_Count <= Max_Attributes;

   --  Verify ML-DSA-87 signature
   function Verify_DSA_Signature (
      Message   : Byte_Array;
      Signature : Byte_Array;
      Public_Key: Byte_Array
   ) return Boolean with
      Global => null,
      Pre    => Signature'Length = DSA_SIG_Size and
                Public_Key'Length = DSA_PK_Size;

   --  Record audit entry
   procedure Record_Audit (
      State     : in Out State_Array;
      Operation : Unsigned_8;
      Actor     : Address;
      Target    : Byte_Array;
      Timestamp : Unsigned_64;
      Block     : Unsigned_64
   ) with
      Global => null;

   --  Check if issuer is authorized for schema
   function Issuer_Authorized (
      Issuer_Rec : Issuer_Record;
      Schema     : Schema_Type
   ) return Boolean with
      Global => null;

   --  Constant-time comparison
   function Constant_Time_Compare (
      A : Byte_Array;
      B : Byte_Array
   ) return Boolean with
      Global => null,
      Pre    => A'Length = B'Length;

end Anubis_Sovereign;
