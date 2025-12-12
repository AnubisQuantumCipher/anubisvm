pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

--  Anubis_KDF: Post-Quantum Key Derivation Function
--
--  This package implements HKDF-like key derivation using KMAC256,
--  providing quantum-resistant key derivation for the AnubisVM TEE.
--
--  Design follows NIST SP 800-56C recommendations:
--  1. Extract: Derive PRK from input key material (IKM) + salt
--  2. Expand: Generate output key material from PRK + context info
--
--  The construction uses KMAC256 for both steps, providing:
--  - 256-bit security level (post-quantum resistant)
--  - Built-in domain separation via customization strings
--  - Proven security under standard model assumptions
--
--  Key derivation contexts (domain separation):
--  - "master"   : TEE master key derivation
--  - "shield"   : Shield encrypted state keys
--  - "eye"      : Viewing key derivation
--  - "gate"     : Session key derivation
--  - "whisper"  : Confidential value keys
--  - "signing"  : ML-DSA signing key derivation
--  - "encrypt"  : ML-KEM encryption key derivation
--
--  Formal Verification (SPARK Gold):
--  - All procedures proven free of runtime errors (no overflow, bounds)
--  - Key length invariants verified
--  - Context separation ensures distinct key spaces
--  - Zeroization postconditions proven
--  - Data flow dependencies explicitly declared and verified

package Anubis_KDF with
   SPARK_Mode => On,
   Pure,
   Always_Terminates
is

   ---------------------------------------------------------------------------
   --  Constants (Security Parameters)
   ---------------------------------------------------------------------------

   --  Key sizes: 256 bits (32 bytes) - post-quantum secure
   Key_Size : constant := 32;

   --  Salt size: 256 bits (32 bytes)
   Salt_Size : constant := 32;

   --  Maximum context info length
   Max_Info_Size : constant := 256;

   --  Maximum output key material (255 * 32 bytes)
   Max_Output_Size : constant := 8160;

   --  Maximum context string length
   Max_Context_Length : constant := 32;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Master key type (256-bit)
   subtype Master_Key is Byte_Array (0 .. Key_Size - 1);

   --  Derived key type (256-bit)
   subtype Derived_Key is Byte_Array (0 .. Key_Size - 1);

   --  Salt type (256-bit)
   subtype KDF_Salt is Byte_Array (0 .. Salt_Size - 1);

   --  Pseudorandom Key - intermediate key from extraction (256-bit)
   subtype KDF_PRK is Byte_Array (0 .. Key_Size - 1);

   ---------------------------------------------------------------------------
   --  Ghost Functions for Specification
   ---------------------------------------------------------------------------

   --  Ghost function: Verify master key has correct length
   function Is_Valid_Master (K : Master_Key) return Boolean is
      (K'First = 0 and K'Last = Key_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify derived key has correct length
   function Is_Valid_Derived (K : Derived_Key) return Boolean is
      (K'First = 0 and K'Last = Key_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify PRK has correct length
   function Is_Valid_PRK (K : KDF_PRK) return Boolean is
      (K'First = 0 and K'Last = Key_Size - 1)
   with Ghost, Pure_Function;

   --  Ghost function: Verify info bounds are safe for processing
   function Info_Bounds_Safe (I : Byte_Array) return Boolean is
      (I'Length <= Max_Info_Size and then I'Last < Natural'Last)
   with Ghost, Pure_Function;

   --  Ghost function: Verify context string bounds are safe
   function Context_Bounds_Safe (C : String) return Boolean is
      (C'Length <= Max_Context_Length and then C'Last < Natural'Last)
   with Ghost, Pure_Function;

   --  Ghost function: Verify output bounds are safe for processing
   function Output_Bounds_Safe (O : Byte_Array) return Boolean is
      (O'Length <= Max_Output_Size and then O'Length > 0 and then
       O'Last < Natural'Last)
   with Ghost, Pure_Function;

   --  Ghost function: Verify key is all zeros (for zeroization)
   function Key_Is_Zero (K : Derived_Key) return Boolean is
      (for all I in K'Range => K (I) = 0)
   with Ghost, Pure_Function;

   --  Ghost function: Verify master key is all zeros
   function Master_Is_Zero (K : Master_Key) return Boolean is
      (for all I in K'Range => K (I) = 0)
   with Ghost, Pure_Function;

   ---------------------------------------------------------------------------
   --  Fixed Context Strings for Domain Separation
   ---------------------------------------------------------------------------

   --  These constants ensure consistent domain separation across the TEE.
   --  Using different context strings for different key types prevents
   --  related-key attacks and ensures cryptographic independence.

   Context_Master  : constant String := "master";   -- TEE master key
   Context_Shield  : constant String := "shield";   -- Encrypted state keys
   Context_Eye     : constant String := "eye";      -- Viewing keys
   Context_Gate    : constant String := "gate";     -- Session keys
   Context_Whisper : constant String := "whisper";  -- Confidential values
   Context_Signing : constant String := "signing";  -- ML-DSA keys
   Context_Encrypt : constant String := "encrypt";  -- ML-KEM keys

   ---------------------------------------------------------------------------
   --  Core KDF Operations (Extract-then-Expand)
   ---------------------------------------------------------------------------

   --  Extract: Create PRK from input key material
   --
   --  This is the "extraction" step that takes potentially non-uniform
   --  input key material and produces a uniformly random PRK.
   --
   --  IKM  : Input Key Material (e.g., shared secret from ML-KEM)
   --  Salt : Optional salt (use random value or Zero_Salt)
   --  Prk  : Output Pseudorandom Key (32 bytes)
   --
   --  The extraction uses KMAC256 with customization "extract":
   --    PRK = KMAC256(salt, IKM, "extract")
   --
   --  Pre: IKM bounds safe
   --  Post: PRK has exact length 32 bytes (indices 0..31)
   procedure Extract (
      IKM  : Byte_Array;
      Salt : KDF_Salt;
      Prk  : out KDF_PRK
   ) with
      Global  => null,
      Depends => (Prk => (IKM, Salt)),
      Pre     => Info_Bounds_Safe (IKM),
      Post    => Is_Valid_PRK (Prk),
      Always_Terminates;

   --  Expand: Generate output key material from PRK
   --
   --  This is the "expansion" step that generates arbitrary-length
   --  output key material from the PRK and context info.
   --
   --  Prk     : Pseudorandom Key from Extract
   --  Info    : Context/application-specific info
   --  Context : Domain separation string
   --  OKM     : Output Key Material (variable length, up to 8160 bytes)
   --
   --  The expansion uses KMAC256 in counter mode:
   --    T(0) = empty
   --    T(i) = KMAC256(PRK, T(i-1) || info || i, context)
   --    OKM = T(1) || T(2) || ... (truncated to requested length)
   --
   --  Pre: Info, context, and output bounds safe
   procedure Expand (
      Prk     : KDF_PRK;
      Info    : Byte_Array;
      Context : String;
      OKM     : out Byte_Array
   ) with
      Global  => null,
      Depends => (OKM => (Prk, Info, Context)),
      Pre     => Info_Bounds_Safe (Info) and then
                 Context_Bounds_Safe (Context) and then
                 Output_Bounds_Safe (OKM),
      Always_Terminates;

   --  Derive_Key: One-step key derivation (Extract + Expand)
   --
   --  Convenient function combining extraction and expansion.
   --  Produces a single 256-bit derived key.
   --
   --  Master  : Master key material
   --  Salt    : Optional salt (use Zero_Salt for deterministic)
   --  Info    : Context-specific information
   --  Context : Domain separation string
   --  Key     : Derived 256-bit key
   --
   --  Pre: Info and context bounds safe
   --  Post: Key has exact length 32 bytes (indices 0..31)
   procedure Derive_Key (
      Master  : Master_Key;
      Salt    : KDF_Salt;
      Info    : Byte_Array;
      Context : String;
      Key     : out Derived_Key
   ) with
      Global  => null,
      Depends => (Key => (Master, Salt, Info, Context)),
      Pre     => Info_Bounds_Safe (Info) and then
                 Context_Bounds_Safe (Context),
      Post    => Is_Valid_Derived (Key),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  TEE-Specific Key Derivation Functions
   ---------------------------------------------------------------------------

   --  Derive_Shield_Key: Derive encryption key for Shield (encrypted state)
   --
   --  Shield provides encrypted state storage. Each state slot has
   --  a unique derived encryption key.
   --
   --  Master : TEE master key
   --  Salt   : Random or deterministic salt
   --  Index  : State slot index (0 to Max_State_Slots - 1)
   --  Key    : Derived encryption key
   --
   --  Post: Key has exact length 32 bytes
   procedure Derive_Shield_Key (
      Master : Master_Key;
      Salt   : KDF_Salt;
      Index  : Natural;
      Key    : out Derived_Key
   ) with
      Global  => null,
      Depends => (Key => (Master, Salt, Index)),
      Post    => Is_Valid_Derived (Key),
      Always_Terminates;

   --  Derive_Eye_Key: Derive viewing key for Eye (selective disclosure)
   --
   --  Eye enables selective disclosure of private state. Each viewer
   --  gets a unique derived viewing key.
   --
   --  Master : TEE master key
   --  Salt   : Random or deterministic salt
   --  Viewer : Viewer's public key hash (SHA3-256, 32 bytes)
   --  Key    : Derived viewing key
   --
   --  Pre: Viewer must be 32 bytes (SHA3-256 hash)
   --  Post: Key has exact length 32 bytes
   procedure Derive_Eye_Key (
      Master : Master_Key;
      Salt   : KDF_Salt;
      Viewer : Byte_Array;
      Key    : out Derived_Key
   ) with
      Global  => null,
      Depends => (Key => (Master, Salt, Viewer)),
      Pre     => Viewer'Length = 32,
      Post    => Is_Valid_Derived (Key),
      Always_Terminates;

   --  Derive_Gate_Key: Derive session key for Gate (private channels)
   --
   --  Gate provides secure private communication channels.
   --  Each session gets a unique derived session key.
   --
   --  Master     : TEE master key
   --  Salt       : Random or deterministic salt
   --  Session_ID : Unique session identifier (256-bit)
   --  Key        : Derived session key
   --
   --  Pre: Session_ID must be 32 bytes
   --  Post: Key has exact length 32 bytes
   procedure Derive_Gate_Key (
      Master     : Master_Key;
      Salt       : KDF_Salt;
      Session_ID : Byte_Array;
      Key        : out Derived_Key
   ) with
      Global  => null,
      Depends => (Key => (Master, Salt, Session_ID)),
      Pre     => Session_ID'Length = 32,
      Post    => Is_Valid_Derived (Key),
      Always_Terminates;

   --  Derive_Whisper_Key: Derive key for Whisper (confidential values)
   --
   --  Whisper provides encrypted value storage within contracts.
   --  Each confidential value has a unique derived encryption key.
   --
   --  Master   : TEE master key
   --  Salt     : Random or deterministic salt
   --  Value_ID : Value identifier (256-bit)
   --  Key      : Derived encryption key
   --
   --  Pre: Value_ID must be 32 bytes
   --  Post: Key has exact length 32 bytes
   procedure Derive_Whisper_Key (
      Master   : Master_Key;
      Salt     : KDF_Salt;
      Value_ID : Byte_Array;
      Key      : out Derived_Key
   ) with
      Global  => null,
      Depends => (Key => (Master, Salt, Value_ID)),
      Pre     => Value_ID'Length = 32,
      Post    => Is_Valid_Derived (Key),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   --  Zeroize_Key: Securely clear derived key material
   --
   --  Uses volatile writes to prevent dead-store elimination.
   --  Must be called after derived key is no longer needed.
   --
   --  Post: All bytes of key are zero (proven by postcondition)
   procedure Zeroize_Key (Key : in Out Derived_Key) with
      Global  => null,
      Depends => (Key => null),
      Post    => Key_Is_Zero (Key),
      Always_Terminates;

   --  Zeroize_Master: Securely clear master key material
   --
   --  Uses volatile writes to prevent dead-store elimination.
   --  Must be called after master key is no longer needed.
   --
   --  Post: All bytes of master key are zero (proven by postcondition)
   procedure Zeroize_Master (Key : in Out Master_Key) with
      Global  => null,
      Depends => (Key => null),
      Post    => Master_Is_Zero (Key),
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Zero_Salt: Constant zero salt for deterministic derivation
   --
   --  Use when deterministic key derivation is required.
   --  For maximum security, prefer random salts when possible.
   Zero_Salt : constant KDF_Salt := (others => 0);

end Anubis_KDF;
