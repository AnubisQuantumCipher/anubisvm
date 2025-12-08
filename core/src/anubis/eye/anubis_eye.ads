-------------------------------------------------------------------------------
--  ANUBIS EYE - Selective Disclosure
--  Viewing keys and selective attribute disclosure for privacy
--
--  Enables auditing and compliance without full transparency
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces;   use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Anubis_Eye with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum attributes per credential
   Max_Attributes     : constant := 32;

   --  Disclosure proof size
   Disclosure_Proof_Size : constant := 512;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Viewing key hierarchy
   type Viewing_Key_Type is (
      Full_View,        -- Can see all transaction details
      Balance_View,     -- Can only see balances
      Existence_View,   -- Can only see transaction exists
      Audit_View,       -- Can see amounts but not addresses
      Custom_View       -- Application-defined restrictions
   );

   subtype Viewing_Key is Byte_Array (0 .. 31);
   subtype Incoming_Key is Byte_Array (0 .. 31);  -- Detect incoming
   subtype Outgoing_Key is Byte_Array (0 .. 31);  -- Prove outgoing

   --  Viewing key bundle
   type View_Key_Bundle is record
      Full_Key     : Viewing_Key;
      Incoming     : Incoming_Key;
      Outgoing     : Outgoing_Key;
      Key_Type     : Viewing_Key_Type;
   end record;

   --  Named array types (required for SPARK - no anonymous arrays)
   subtype Attribute_Data is Byte_Array (0 .. 63);
   type Attribute_Array is array (0 .. Max_Attributes - 1) of Attribute_Data;

   --  For unconstrained array parameters
   type Attribute_Input_Array is array (Natural range <>) of Attribute_Data;
   subtype Transaction_Data is Byte_Array (0 .. 255);
   type Transaction_Array is array (Natural range <>) of Transaction_Data;

   --  Attribute disclosure credential
   type Attribute_Credential is record
      Attrs        : Attribute_Array;
      Attr_Count   : Natural;
      Issuer_PK    : Byte_Array (0 .. 2591);  -- ML-DSA-87 public key
      Signature    : Byte_Array (0 .. 4626);  -- ML-DSA-87 signature (4627 bytes per FIPS 204)
      Holder_Commit: Byte_Array (0 .. 63);    -- Commitment to holder
   end record;

   --  Selective disclosure proof
   type Disclosure_Proof is record
      Proof_Data   : Byte_Array (0 .. Disclosure_Proof_Size - 1);
      Disclosed_Mask : Unsigned_32;  -- Bitmask of disclosed attributes
      Credential_Hash: Byte_Array (0 .. 31);
   end record;

   ---------------------------------------------------------------------------
   --  Viewing Key Operations
   ---------------------------------------------------------------------------

   --  Derive viewing keys from master seed
   procedure Derive_View_Keys (
      Master_Seed : Byte_Array;
      Bundle      : out View_Key_Bundle
   ) with
      Global => null,
      Pre => Master_Seed'Length = 32;

   --  Derive child viewing key (for delegation)
   procedure Derive_Child_Key (
      Parent_Key  : Viewing_Key;
      Child_Index : Unsigned_32;
      Key_Type    : Viewing_Key_Type;
      Child_Key   : out Viewing_Key
   ) with
      Global => null;

   --  Check if key can view specific data
   function Can_View (
      Key        : Viewing_Key;
      Key_Type   : Viewing_Key_Type;
      Data_Type  : Viewing_Key_Type
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Transaction Scanning
   ---------------------------------------------------------------------------

   --  Scan transaction output for ownership
   procedure Scan_Transaction (
      View_Key       : Viewing_Key;
      Tx_Commitment  : Byte_Array;
      Encrypted_Data : Byte_Array;
      Is_Mine        : out Boolean;
      Decrypted_Data : out Byte_Array;
      Data_Length    : out Natural
   ) with
      Global => null,
      Pre => Tx_Commitment'Length = 64 and Encrypted_Data'Length <= 4096;

   --  Generate stealth address for receiving
   procedure Generate_Stealth_Address (
      View_Key       : Viewing_Key;
      Spend_Key      : Byte_Array;
      Randomness     : Byte_Array;
      Stealth_Addr   : out Byte_Array;
      Tx_Public_Key  : out Byte_Array
   ) with
      Global => null,
      Pre => Spend_Key'Length = 32 and Randomness'Length = 32
             and Stealth_Addr'Length = 32 and Tx_Public_Key'Length = 32;

   ---------------------------------------------------------------------------
   --  Attribute Credentials
   ---------------------------------------------------------------------------

   --  Issue credential (issuer side)
   procedure Issue_Credential (
      Issuer_SK     : Byte_Array;
      Holder_Commit : Byte_Array;
      Attributes    : Attribute_Input_Array;
      Credential    : out Attribute_Credential;
      Success       : out Boolean
   ) with
      Global => null,
      Pre => Issuer_SK'Length = 4032  -- ML-DSA-87 secret key
             and Holder_Commit'Length = 64
             and Attributes'Length <= Max_Attributes;

   --  Verify credential authenticity
   function Verify_Credential (
      Credential    : Attribute_Credential;
      Issuer_PK     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Issuer_PK'Length = 2592;

   ---------------------------------------------------------------------------
   --  Selective Disclosure
   ---------------------------------------------------------------------------

   --  Create proof disclosing only selected attributes
   procedure Create_Disclosure_Proof (
      Credential      : Attribute_Credential;
      Holder_Secret   : Byte_Array;
      Disclose_Mask   : Unsigned_32;  -- Bitmask of attributes to reveal
      Challenge       : Byte_Array;    -- Verifier"s challenge
      Proof           : out Disclosure_Proof;
      Success         : out Boolean
   ) with
      Global => null,
      Pre => Holder_Secret'Length = 32 and Challenge'Length = 32;

   --  Verify selective disclosure proof
   function Verify_Disclosure (
      Proof          : Disclosure_Proof;
      Disclosed_Attrs: Attribute_Input_Array;
      Issuer_PK      : Byte_Array;
      Challenge      : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Issuer_PK'Length = 2592 and Challenge'Length = 32;

   ---------------------------------------------------------------------------
   --  Audit Support
   ---------------------------------------------------------------------------

   --  Generate audit key for compliance
   procedure Generate_Audit_Key (
      View_Key    : Viewing_Key;
      Audit_Scope : Byte_Array;  -- Definition of what can be audited
      Audit_Key   : out Viewing_Key;
      Expiry      : Unsigned_64  -- Unix timestamp
   ) with
      Global => null;

   --  Verify audit trail
   function Verify_Audit_Trail (
      Audit_Key   : Viewing_Key;
      Transactions: Transaction_Array;
      Expected_Sum: Unsigned_64
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Bundle (Bundle : in out View_Key_Bundle) with
      Global => null;

   procedure Zeroize_Credential (Cred : in Out Attribute_Credential) with
      Global => null;

end Anubis_Eye;
