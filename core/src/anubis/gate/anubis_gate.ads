-------------------------------------------------------------------------------
--  ANUBIS GATE - Private Smart Contract Execution
--  ZK-proven contract execution without revealing inputs/state
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces;   use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Anubis_Gate with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum private contract state size
   Max_Private_State  : constant := 65536;  -- 64KB

   --  Maximum private input size
   Max_Private_Input  : constant := 4096;   -- 4KB

   --  Execution proof size
   Execution_Proof_Size : constant := 8192; -- 8KB

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Private execution mode
   type Execution_Mode is (
      Full_Private,      -- Everything hidden
      Public_Result,     -- Only result visible
      Public_Function,   -- Function name visible, args hidden
      Auditable         -- Viewable with audit key
   );

   --  Encrypted contract state
   type Private_State is record
      Ciphertext     : Byte_Array (0 .. Max_Private_State - 1);
      CT_Length      : Natural;
      State_Hash     : Byte_Array (0 .. 31);
      Version        : Unsigned_64;
   end record;

   --  Encrypted input
   type Private_Input is record
      Ciphertext     : Byte_Array (0 .. Max_Private_Input - 1);
      CT_Length      : Natural;
      Input_Hash     : Byte_Array (0 .. 31);
   end record;

   --  Execution proof (ZK proof of correct execution)
   type Execution_Proof is record
      Proof_Data     : Byte_Array (0 .. Execution_Proof_Size - 1);
      Proof_Length   : Natural;
      Old_State_Hash : Byte_Array (0 .. 31);
      New_State_Hash : Byte_Array (0 .. 31);
      Output_Hash    : Byte_Array (0 .. 31);
   end record;

   --  Private execution request
   type Private_Execution_Request is record
      Contract_Addr  : Byte_Array (0 .. 31);
      Function_Selector : Unsigned_32;
      Private_Args   : Private_Input;
      Public_Args    : Byte_Array (0 .. 1023);
      Public_Args_Len: Natural;
      Mode           : Execution_Mode;
      Gas_Limit      : Unsigned_64;
   end record;

   --  Private execution result
   type Private_Execution_Result is record
      Success        : Boolean;
      New_State      : Private_State;
      Output         : Private_Input;  -- Could be private or public
      Output_Public  : Byte_Array (0 .. 1023);
      Output_Public_Len : Natural;
      Proof          : Execution_Proof;
      Gas_Used       : Unsigned_64;
   end record;

   --  Session for interactive private execution
   type Private_Session is record
      Session_ID     : Byte_Array (0 .. 31);
      Contract_Addr  : Byte_Array (0 .. 31);
      Shared_Secret  : Byte_Array (0 .. 31);  -- ML-KEM derived
      State_Snapshot : Private_State;
      Created_At     : Unsigned_64;
      Expires_At     : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Private State Management
   ---------------------------------------------------------------------------

   --  Initialize private state for contract
   procedure Init_Private_State (
      Contract_Code  : Byte_Array;
      Init_Data      : Byte_Array;
      Owner_PK       : Byte_Array;
      State          : out Private_State;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Owner_PK'Length = 1568;  -- ML-KEM-1024 public key

   --  Encrypt state update
   procedure Encrypt_State (
      Plaintext      : Byte_Array;
      Contract_Key   : Byte_Array;
      Nonce          : Byte_Array;
      State          : out Private_State;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Plaintext'Length <= Max_Private_State
             and Contract_Key'Length = 32
             and Nonce'Length = 24;

   --  Decrypt state (for authorized parties)
   procedure Decrypt_State (
      State          : Private_State;
      Contract_Key   : Byte_Array;
      Plaintext      : out Byte_Array;
      PT_Length      : out Natural;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Contract_Key'Length = 32
             and Plaintext'Length >= Max_Private_State;

   ---------------------------------------------------------------------------
   --  Private Execution
   ---------------------------------------------------------------------------

   --  Execute contract privately (generates ZK proof)
   procedure Execute_Private (
      Request        : Private_Execution_Request;
      Current_State  : Private_State;
      Caller_SK      : Byte_Array;  -- For decrypting state
      Result         : out Private_Execution_Result
   ) with
      Global => null;

   --  Verify private execution proof
   function Verify_Execution (
      Proof          : Execution_Proof;
      Contract_Addr  : Byte_Array;
      Old_State_Hash : Byte_Array;
      New_State_Hash : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Contract_Addr'Length = 32
             and Old_State_Hash'Length = 32
             and New_State_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Session Management
   ---------------------------------------------------------------------------

   --  Create private session with contract
   procedure Create_Session (
      Contract_Addr  : Byte_Array;
      User_KEM_SK    : Byte_Array;
      Contract_KEM_PK: Byte_Array;
      Randomness     : Byte_Array;
      Session        : out Private_Session;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Contract_Addr'Length = 32
             and User_KEM_SK'Length = 3168
             and Contract_KEM_PK'Length = 1568
             and Randomness'Length = 64;

   --  Execute within session
   procedure Session_Execute (
      Session        : in out Private_Session;
      Function_Selector : Unsigned_32;
      Args           : Byte_Array;
      Result         : out Private_Execution_Result
   ) with
      Global => null;

   --  Close session (zeroize secrets)
   procedure Close_Session (Session : in out Private_Session) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Access Control
   ---------------------------------------------------------------------------

   type Access_Policy is (
      Owner_Only,       -- Only owner can execute
      Allowlist,        -- Specific addresses allowed
      Public,           -- Anyone can execute
      Conditional       -- Based on credential verification
   );

   --  Check if caller is authorized
   function Check_Access (
      Contract_Addr  : Byte_Array;
      Caller_Addr    : Byte_Array;
      Policy         : Access_Policy;
      Credential     : Byte_Array  -- Optional credential
   ) return Boolean with
      Global => null;

   --  Update access policy (requires owner signature)
   procedure Update_Policy (
      Contract_Addr  : Byte_Array;
      New_Policy     : Access_Policy;
      Owner_Sig      : Byte_Array;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Privacy-Preserving State Reads
   ---------------------------------------------------------------------------

   --  Read private state with proof of membership (doesn"t reveal value)
   procedure Private_Read_With_Proof (
      State          : Private_State;
      Key            : Byte_Array;
      View_Key       : Byte_Array;
      Value          : out Byte_Array;
      Value_Length   : out Natural;
      Membership_Proof : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null;

   --  Verify membership proof without seeing value
   function Verify_Membership (
      State_Hash     : Byte_Array;
      Key            : Byte_Array;
      Proof          : Byte_Array
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Multi-Party Computation Support
   ---------------------------------------------------------------------------

   type MPC_Share is record
      Share_Data     : Byte_Array (0 .. 1023);
      Share_Length   : Natural;
      Party_Index    : Natural;
      Threshold      : Natural;
      Total_Parties  : Natural;
   end record;

   --  Named array type for shares
   type MPC_Share_Array is array (Natural range <>) of MPC_Share;

   --  Split private input into shares
   procedure Create_Shares (
      Input          : Byte_Array;
      Threshold      : Natural;
      Total          : Natural;
      Shares         : out MPC_Share_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Threshold <= Total and Shares'Length = Total;

   --  Combine shares to recover input
   procedure Combine_Shares (
      Shares         : MPC_Share_Array;
      Output         : out Byte_Array;
      Output_Length  : out Natural;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_State (State : in out Private_State) with
      Global => null;

   procedure Zeroize_Session (Session : in Out Private_Session) with
      Global => null;

   procedure Zeroize_Result (Result : in out Private_Execution_Result) with
      Global => null;

end Anubis_Gate;
