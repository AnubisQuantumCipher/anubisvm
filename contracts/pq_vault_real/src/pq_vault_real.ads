--  PQ_Vault: Post-Quantum Password Manager with AAS-001 Identity
--
--  WHY AAS-001 ADDRESSES MATTER HERE:
--  1. SHARING: Share secrets with specific addresses (user/contract/validator)
--  2. RECOVERY: Designate emergency contacts by address (checksum prevents typos)
--  3. NETWORK ISOLATION: Dev passwords can't leak to mainnet
--  4. ENTITY-BASED ACCESS: Different rules for users vs contracts vs validators
--  5. AUDIT TRAIL: Every access logged with full AAS-001 identity
--
--  Certification Target: GOLD
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Address_Types; use Anubis_Address_Types;

package PQ_Vault_Real with SPARK_Mode => On is

   Contract_Name    : constant String := "PQ_Vault";
   Contract_Version : constant String := "2.0.0";

   ---------------------------------------------------------------------------
   --  Vault Configuration
   ---------------------------------------------------------------------------

   Max_Entries          : constant := 256;
   Max_Shared_With      : constant := 8;    --  Max addresses to share entry with
   Max_Recovery_Contacts : constant := 3;   --  Emergency access contacts

   subtype Entry_Index is Natural range 0 .. Max_Entries - 1;
   subtype Entry_Count_Type is Natural range 0 .. Max_Entries;
   subtype Share_Index is Natural range 0 .. Max_Shared_With - 1;
   subtype Recovery_Index is Natural range 0 .. Max_Recovery_Contacts - 1;

   ---------------------------------------------------------------------------
   --  Entry Types
   ---------------------------------------------------------------------------

   type Entry_Kind is (
      Kind_Login,      --  Username/password
      Kind_Note,       --  Secure note
      Kind_Card,       --  Payment card
      Kind_Identity,   --  Personal ID
      Kind_Crypto,     --  Wallet seed/key
      Kind_API_Key,    --  API credentials
      Kind_SSH_Key,    --  SSH keys
      Kind_Certificate --  TLS/SSL certs
   );
   for Entry_Kind'Size use 8;

   ---------------------------------------------------------------------------
   --  Access Permissions (WHO can access WHAT)
   ---------------------------------------------------------------------------

   --  Permission levels for shared access
   type Permission_Level is (
      Perm_None,       --  No access
      Perm_View,       --  Can view (read-only)
      Perm_Use,        --  Can use (e.g., sign with key)
      Perm_Edit,       --  Can modify
      Perm_Admin       --  Full control (can share, delete)
   );
   for Permission_Level'Size use 8;

   --  Entity type restrictions
   type Entity_Restriction is (
      Allow_All,       --  Any entity type
      Users_Only,      --  Only 'u' addresses
      Contracts_Only,  --  Only 'c' addresses (for multisig)
      Validators_Only  --  Only 'v' addresses (node operator secrets)
   );
   for Entity_Restriction'Size use 8;

   ---------------------------------------------------------------------------
   --  Shared Access Entry (WHO has access to this secret)
   ---------------------------------------------------------------------------

   type Shared_Access is record
      Active      : Boolean;
      Grantee     : Account_ID;        --  AAS-001 account who has access
      Entity      : Entity_Type;       --  What type of entity (u/c/v/s)
      Network     : Network_Type;      --  What network (must match vault)
      Permission  : Permission_Level;  --  What they can do
      Granted_At  : Unsigned_64;       --  When access was granted
      Expires_At  : Unsigned_64;       --  0 = never expires
      Access_Count : Unsigned_32;      --  How many times accessed
      Last_Access : Unsigned_64;       --  Last access timestamp
   end record;

   type Shared_Access_List is array (Share_Index) of Shared_Access;

   Null_Shared_Access : constant Shared_Access := (
      Active       => False,
      Grantee      => (others => 0),
      Entity       => User,
      Network      => Dev,
      Permission   => Perm_None,
      Granted_At   => 0,
      Expires_At   => 0,
      Access_Count => 0,
      Last_Access  => 0
   );

   ---------------------------------------------------------------------------
   --  Entry Header (metadata for each secret)
   ---------------------------------------------------------------------------

   type Entry_Header is record
      Active       : Boolean;
      Kind         : Entry_Kind;
      Created      : Unsigned_64;
      Modified     : Unsigned_64;
      Data_Hash    : Account_ID;       --  SHA3-256 of encrypted data
      Label_Hash   : Account_ID;       --  SHA3-256 of label (for search)
      Favorite     : Boolean;
      Category     : Unsigned_8;
      --  Sharing configuration
      Shared_With  : Shared_Access_List;
      Restriction  : Entity_Restriction;
      --  Network binding (secret only usable on this network)
      Bound_Network : Network_Type;
   end record;

   Null_Header : constant Entry_Header := (
      Active        => False,
      Kind          => Kind_Login,
      Created       => 0,
      Modified      => 0,
      Data_Hash     => (others => 0),
      Label_Hash    => (others => 0),
      Favorite      => False,
      Category      => 0,
      Shared_With   => (others => Null_Shared_Access),
      Restriction   => Users_Only,
      Bound_Network => Dev
   );

   type Entry_Headers is array (Entry_Index) of Entry_Header;

   ---------------------------------------------------------------------------
   --  Recovery Contact (emergency access)
   ---------------------------------------------------------------------------

   type Recovery_Contact is record
      Active       : Boolean;
      Contact      : Account_ID;       --  AAS-001 account for recovery
      Entity       : Entity_Type;      --  Must be User for recovery
      Network      : Network_Type;     --  Must match vault network
      Added_At     : Unsigned_64;
      Delay_Hours  : Unsigned_32;      --  Wait time before access granted
   end record;

   type Recovery_Contacts is array (Recovery_Index) of Recovery_Contact;

   Null_Recovery : constant Recovery_Contact := (
      Active      => False,
      Contact     => (others => 0),
      Entity      => User,
      Network     => Dev,
      Added_At    => 0,
      Delay_Hours => 168  --  1 week default
   );

   ---------------------------------------------------------------------------
   --  Vault State
   ---------------------------------------------------------------------------

   type Vault_State is record
      Initialized     : Boolean;

      --  Owner identity (full AAS-001 info)
      Owner           : Account_ID;
      Owner_Entity    : Entity_Type;
      Owner_Network   : Network_Type;

      --  Vault metadata
      Name_Hash       : Account_ID;
      Created_At      : Unsigned_64;
      Modified_At     : Unsigned_64;

      --  Entries
      Headers         : Entry_Headers;
      Entry_Count     : Entry_Count_Type;

      --  Recovery configuration
      Recovery        : Recovery_Contacts;
      Recovery_Pending : Boolean;
      Recovery_Requested_At : Unsigned_64;
      Recovery_Requester : Account_ID;

      --  Security
      Version         : Unsigned_64;
      Access_Count    : Unsigned_64;
      Last_Access     : Unsigned_64;
   end record;

   Empty_State : constant Vault_State := (
      Initialized           => False,
      Owner                 => (others => 0),
      Owner_Entity          => User,
      Owner_Network         => Dev,
      Name_Hash             => (others => 0),
      Created_At            => 0,
      Modified_At           => 0,
      Headers               => (others => Null_Header),
      Entry_Count           => 0,
      Recovery              => (others => Null_Recovery),
      Recovery_Pending      => False,
      Recovery_Requested_At => 0,
      Recovery_Requester    => (others => 0),
      Version               => 0,
      Access_Count          => 0,
      Last_Access           => 0
   );

   ---------------------------------------------------------------------------
   --  Result Codes
   ---------------------------------------------------------------------------

   type Result_Code is (
      Success,
      Error_Not_Initialized,
      Error_Already_Initialized,
      Error_Not_Owner,
      Error_Vault_Full,
      Error_Entry_Not_Found,
      Error_Invalid_Index,
      Error_Invalid_Data,
      --  AAS-001 specific errors
      Error_Network_Mismatch,      --  Caller on wrong network
      Error_Entity_Not_Allowed,    --  Entity type not permitted
      Error_Address_Invalid,       --  Checksum failed
      Error_Permission_Denied,     --  Insufficient permission
      Error_Share_Limit_Reached,   --  Max shares exceeded
      Error_Already_Shared,        --  Already shared with this address
      Error_Recovery_Pending,      --  Recovery already in progress
      Error_Recovery_Delay         --  Must wait for delay period
   );
   for Result_Code'Size use 8;

   ---------------------------------------------------------------------------
   --  Vault Management
   ---------------------------------------------------------------------------

   --  Initialize vault with full AAS-001 identity
   procedure Initialize (
      State        : in out Vault_State;
      Owner        : Account_ID;
      Owner_Entity : Entity_Type;
      Owner_Network : Network_Type;
      Name_Hash    : Account_ID;
      Timestamp    : Unsigned_64;
      Result       : out Result_Code)
   with
      Global => null,
      Pre    => not State.Initialized,
      Post   => (if Result = Success then
                    State.Initialized and
                    State.Owner = Owner and
                    State.Owner_Network = Owner_Network);

   --  Check if caller is owner (with network validation)
   function Is_Owner (
      State          : Vault_State;
      Caller         : Account_ID;
      Caller_Network : Network_Type) return Boolean
   with
      Global => null,
      Pre    => State.Initialized;

   --  Check if caller has permission to access entry
   function Has_Permission (
      State          : Vault_State;
      Index          : Entry_Index;
      Caller         : Account_ID;
      Caller_Entity  : Entity_Type;
      Caller_Network : Network_Type;
      Required       : Permission_Level) return Boolean
   with
      Global => null,
      Pre    => State.Initialized;

   ---------------------------------------------------------------------------
   --  Entry Operations
   ---------------------------------------------------------------------------

   procedure Add_Entry (
      State          : in Out Vault_State;
      Caller         : Account_ID;
      Caller_Network : Network_Type;
      Kind           : Entry_Kind;
      Label_Hash     : Account_ID;
      Data_Hash      : Account_ID;
      Category       : Unsigned_8;
      Bound_Network  : Network_Type;  --  Which network can use this secret
      Restriction    : Entity_Restriction;
      Timestamp      : Unsigned_64;
      Index          : out Entry_Index;
      Result         : out Result_Code)
   with
      Global => null,
      Pre    => State.Initialized;

   procedure Get_Entry (
      State          : Vault_State;
      Index          : Entry_Index;
      Caller         : Account_ID;
      Caller_Entity  : Entity_Type;
      Caller_Network : Network_Type;
      Header         : out Entry_Header;
      Result         : out Result_Code)
   with
      Global => null,
      Pre    => State.Initialized;

   ---------------------------------------------------------------------------
   --  Sharing Operations (uses AAS-001 for access control)
   ---------------------------------------------------------------------------

   --  Share an entry with another AAS-001 address
   procedure Share_Entry (
      State           : in Out Vault_State;
      Index           : Entry_Index;
      Caller          : Account_ID;
      Caller_Network  : Network_Type;
      Grantee         : Account_ID;
      Grantee_Entity  : Entity_Type;
      Grantee_Network : Network_Type;
      Permission      : Permission_Level;
      Expires_At      : Unsigned_64;
      Timestamp       : Unsigned_64;
      Result          : out Result_Code)
   with
      Global => null,
      Pre    => State.Initialized;

   --  Revoke shared access
   procedure Revoke_Share (
      State          : in out Vault_State;
      Index          : Entry_Index;
      Caller         : Account_ID;
      Caller_Network : Network_Type;
      Grantee        : Account_ID;
      Timestamp      : Unsigned_64;
      Result         : out Result_Code)
   with
      Global => null,
      Pre    => State.Initialized;

   ---------------------------------------------------------------------------
   --  Recovery Operations (emergency access via trusted contacts)
   ---------------------------------------------------------------------------

   --  Add a recovery contact
   procedure Add_Recovery_Contact (
      State           : in Out Vault_State;
      Caller          : Account_ID;
      Caller_Network  : Network_Type;
      Contact         : Account_ID;
      Contact_Network : Network_Type;
      Delay_Hours     : Unsigned_32;
      Timestamp       : Unsigned_64;
      Result          : out Result_Code)
   with
      Global => null,
      Pre    => State.Initialized;

   --  Request recovery (starts delay timer)
   procedure Request_Recovery (
      State            : in Out Vault_State;
      Requester        : Account_ID;
      Requester_Network : Network_Type;
      Timestamp        : Unsigned_64;
      Result           : out Result_Code)
   with
      Global => null,
      Pre    => State.Initialized;

   --  Complete recovery (after delay period)
   procedure Complete_Recovery (
      State            : in Out Vault_State;
      Requester        : Account_ID;
      Requester_Network : Network_Type;
      Timestamp        : Unsigned_64;
      Result           : out Result_Code)
   with
      Global => null,
      Pre    => State.Initialized;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Entry_Count (State : Vault_State) return Entry_Count_Type
   with Global => null, Pre => State.Initialized;

   function Get_Owner_Network (State : Vault_State) return Network_Type
   with Global => null, Pre => State.Initialized;

   function Get_Owner_Entity (State : Vault_State) return Entity_Type
   with Global => null, Pre => State.Initialized;

end PQ_Vault_Real;
