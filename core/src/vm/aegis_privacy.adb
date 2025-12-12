-------------------------------------------------------------------------------
--  AEGIS PRIVACY - Privacy Precompiles Implementation
--  Bridges VM syscalls with Anubis Privacy Layer
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Shield;
with Anubis_Whisper;
with Anubis_Gate;
with Anubis_Eye;
with Anubis_Ring_Sig;
with Anubis_Lattice_ZK;
with Anubis_SHA3;
with Anubis_Types;
with Anubis_Bytes;

package body Aegis_Privacy with
   SPARK_Mode => On
is
   --  Make arithmetic operators visible for Gas_Amount
   use type Aegis_VM_Types.Gas_Amount;

   ---------------------------------------------------------------------------
   --  Internal State for ZK Public Parameters (lazily initialized)
   ---------------------------------------------------------------------------

   ZK_Params_Initialized : Boolean := False;
   ZK_Public_Params      : Anubis_Lattice_ZK.Public_Params;

   --  Domain separator for privacy operations
   Privacy_Domain : constant Anubis_Types.Byte_Array (0 .. 15) :=
     (16#41#, 16#45#, 16#47#, 16#49#, 16#53#, 16#5F#, 16#50#, 16#52#,
      16#49#, 16#56#, 16#41#, 16#43#, 16#59#, 16#5F#, 16#56#, 16#31#);
   --  "AEGIS_PRIVACY_V1"

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Ensure_ZK_Params with
      Global => (In_Out => (ZK_Params_Initialized, ZK_Public_Params))
   is
      Seed : constant Anubis_Types.Byte_Array (0 .. 31) := (others => 0);
   begin
      if not ZK_Params_Initialized then
         Anubis_Lattice_ZK.Setup (Seed, ZK_Public_Params);
         ZK_Params_Initialized := True;
      end if;
   end Ensure_ZK_Params;

   --  Hash data to get a transcript for ZK proofs
   procedure Hash_Transcript (
      Data       : Anubis_Types.Byte_Array;
      Transcript : out Anubis_Types.Byte_Array
   ) with
      Pre => Transcript'Length = 32
   is
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      Anubis_SHA3.SHA3_256 (Data, Hash_Out);
      for I in Transcript'Range loop
         Transcript (I) := Hash_Out (I - Transcript'First);
      end loop;
   end Hash_Transcript;

   ---------------------------------------------------------------------------
   --  Private State Operations (SHIELD)
   ---------------------------------------------------------------------------

   procedure Private_Store (
      Contract       : VM_Contract_Address;
      Key            : VM_Hash256;
      Plaintext      : VM_Byte_Array;
      User_KEM_PK    : VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Contract, Key);
      Shield_Entry   : Anubis_Shield.Private_Entry;
      Shield_Commit  : Anubis_Shield.Entry_Commitment;
      Success        : Boolean;
      Local_PT       : Anubis_Types.Byte_Array (0 .. Plaintext'Length - 1);
      Local_Rand     : Anubis_Types.Byte_Array (0 .. 63);
      Local_PK       : Anubis_Types.Byte_Array (0 .. 1567);
   begin
      --  Convert to local types
      for I in Local_PT'Range loop
         Local_PT (I) := Anubis_Types.Byte (Plaintext (Plaintext'First + I));
      end loop;

      for I in Local_Rand'Range loop
         Local_Rand (I) := Anubis_Types.Byte (Randomness (Randomness'First + I));
      end loop;

      for I in Local_PK'Range loop
         Local_PK (I) := Anubis_Types.Byte (User_KEM_PK (User_KEM_PK'First + I));
      end loop;

      --  Encrypt the state using SHIELD
      Anubis_Shield.Encrypt_State (
         Plaintext   => Local_PT,
         User_KEM_PK => Local_PK,
         Randomness  => Local_Rand,
         Priv_Entry  => Shield_Entry,
         Commitment  => Shield_Commit,
         Success     => Success
      );

      if Success then
         Result := (
            Success     => True,
            Gas_Used    => Gas_Private_Store,
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
         --  Store commitment hash in result
         for I in Shield_Commit.Value'Range loop
            if I < 32 then
               Result.Return_Hash (VM_Hash256'First + I) :=
                 VM_Byte (Shield_Commit.Value (I));
            end if;
         end loop;
      else
         Result := (
            Success     => False,
            Gas_Used    => Gas_Private_Store / 2,
            Error_Code  => Privacy_Error_Invalid_Key,
            Return_Hash => (others => 0)
         );
      end if;
   end Private_Store;

   procedure Private_Load (
      Contract       : VM_Contract_Address;
      Key            : VM_Hash256;
      User_KEM_SK    : VM_Byte_Array;
      Plaintext      : out VM_Byte_Array;
      Plaintext_Len  : out Natural;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Contract, Key);
      Shield_Entry   : Anubis_Shield.Private_Entry;
      Local_PT       : Anubis_Types.Byte_Array (0 .. Anubis_Shield.Max_Entry_Size - 1);
      Local_PT_Len   : Natural;
      Local_SK       : Anubis_Types.Byte_Array (0 .. 3167);
      Success        : Boolean;
   begin
      --  Initialize outputs
      Plaintext := (others => 0);
      Plaintext_Len := 0;

      --  Convert secret key
      for I in Local_SK'Range loop
         Local_SK (I) := Anubis_Types.Byte (User_KEM_SK (User_KEM_SK'First + I));
      end loop;

      --  For now, create a dummy entry (actual implementation would load from storage)
      Shield_Entry := (
         Ciphertext   => (others => 0),
         CT_Length    => 0,
         Encapsulated => (others => 0),
         Nonce        => (others => 0),
         Tag          => (others => 0)
      );

      --  Decrypt using SHIELD
      Anubis_Shield.Decrypt_State (
         Priv_Entry => Shield_Entry,
         User_KEM_SK => Local_SK,
         Plaintext   => Local_PT,
         PT_Length   => Local_PT_Len,
         Success     => Success
      );

      if Success then
         Plaintext_Len := Natural'Min (Local_PT_Len, Plaintext'Length);
         for I in 0 .. Plaintext_Len - 1 loop
            Plaintext (Plaintext'First + I) := VM_Byte (Local_PT (I));
         end loop;

         Result := (
            Success     => True,
            Gas_Used    => Gas_Private_Load,
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
      else
         Result := (
            Success     => False,
            Gas_Used    => Gas_Private_Load / 2,
            Error_Code  => Privacy_Error_Decryption_Failed,
            Return_Hash => (others => 0)
         );
      end if;
   end Private_Load;

   procedure Private_Delete (
      Contract       : VM_Contract_Address;
      Key            : VM_Hash256;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Contract, Key);
   begin
      --  Mark entry for deletion in storage layer
      --  Actual deletion happens in State_Persistence
      Result := (
         Success     => True,
         Gas_Used    => Gas_Private_Delete,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Private_Delete;

   ---------------------------------------------------------------------------
   --  Confidential Transaction Operations (WHISPER)
   ---------------------------------------------------------------------------

   procedure Commit_Amount (
      Value          : Unsigned_64;
      Blinding       : VM_Byte_Array;
      Commitment     : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) is
      Local_Blinding : Anubis_Whisper.Blinding_Factor;
      Local_Commit   : Anubis_Whisper.Amount_Commitment;
   begin
      --  Convert blinding factor
      for I in Local_Blinding'Range loop
         Local_Blinding (I) := Anubis_Types.Byte (Blinding (Blinding'First + I));
      end loop;

      --  Create Ajtai commitment
      Anubis_Whisper.Create_Commitment (
         Value      => Value,
         Blinding   => Local_Blinding,
         Commitment => Local_Commit
      );

      --  Copy commitment to output
      for I in Local_Commit'Range loop
         Commitment (Commitment'First + I) := VM_Byte (Local_Commit (I));
      end loop;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Commit_Amount,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Commit_Amount;

   procedure Verify_Range_Proof (
      Commitment     : VM_Byte_Array;
      Proof          : VM_Byte_Array;
      Bits           : Natural;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) is
      Local_Commit : Anubis_Whisper.Amount_Commitment;
      Local_Proof  : Anubis_Whisper.Range_Proof;
   begin
      --  Convert commitment
      for I in Local_Commit'Range loop
         Local_Commit (I) := Anubis_Types.Byte (Commitment (Commitment'First + I));
      end loop;

      --  Convert proof
      for I in Local_Proof'Range loop
         Local_Proof (I) := Anubis_Types.Byte (Proof (Proof'First + I));
      end loop;

      --  Verify range proof
      Valid := Anubis_Whisper.Verify_Range_Proof (
         Commitment => Local_Commit,
         Proof      => Local_Proof,
         Bits       => Bits
      );

      if Valid then
         Result := (
            Success     => True,
            Gas_Used    => Gas_Verify_Range,
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
      else
         Result := (
            Success     => False,
            Gas_Used    => Gas_Verify_Range,
            Error_Code  => Privacy_Error_Range_Violation,
            Return_Hash => (others => 0)
         );
      end if;
   end Verify_Range_Proof;

   procedure Add_Commitments (
      A, B           : VM_Byte_Array;
      Sum            : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) is
      Local_A, Local_B : Anubis_Whisper.Amount_Commitment;
      Local_Sum        : Anubis_Whisper.Amount_Commitment;
   begin
      --  Convert inputs
      for I in Local_A'Range loop
         Local_A (I) := Anubis_Types.Byte (A (A'First + I));
         Local_B (I) := Anubis_Types.Byte (B (B'First + I));
      end loop;

      --  Homomorphic addition
      Anubis_Whisper.Add_Commitments (
         A      => Local_A,
         B      => Local_B,
         Result => Local_Sum
      );

      --  Copy result
      for I in Local_Sum'Range loop
         Sum (Sum'First + I) := VM_Byte (Local_Sum (I));
      end loop;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Add_Commitments,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Add_Commitments;

   procedure Verify_Balance (
      Input_Commits  : VM_Byte_Array;
      Output_Commits : VM_Byte_Array;
      Fee            : Unsigned_64;
      Proof          : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Proof);

      In_Len  : constant Natural := Input_Commits'Length;
      Out_Len : constant Natural := Output_Commits'Length;
      In_Count  : Natural;
      Out_Count : Natural;

      --  Local views over commitment buffers
      subtype Commit_Index is Natural range 0 .. 63;

      --  Safety: commitment size is fixed at 64 bytes
      Commit_Size : constant Natural := 64;

      --  Construct a simple fee commitment from the public fee value.
      --  This is a placeholder consistent with the current WHISPER
      --  Verify_Balance_Proof implementation, which uses XOR-based
      --  homomorphic addition over 64-byte commitments.
      function Make_Fee_Commit (Amount : Unsigned_64)
        return Anubis_Whisper.Amount_Commitment
      is
         Fee_Commit : Anubis_Whisper.Amount_Commitment := (others => 0);
         Bytes      : Anubis_Types.Byte_Array (0 .. 7);
      begin
         Anubis_Bytes.Encode_LE64 (Amount, Bytes);
         for I in 0 .. 7 loop
            Fee_Commit (I) := Bytes (I);
         end loop;
         return Fee_Commit;
      end Make_Fee_Commit;

   begin
      --  Basic structural sanity checks on commitment buffers.
      --  Each commitment must be 64 bytes; reject obviously malformed inputs.
      if In_Len = 0 or else Out_Len = 0 or else
         In_Len mod 64 /= 0 or else Out_Len mod 64 /= 0
      then
         Valid := False;
         Result := (
           Success     => False,
           Gas_Used    => Gas_Verify_Balance,
           Error_Code  => Privacy_Error_Invalid_Commitment,
           Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Convert flat byte buffers into WHISPER commitment arrays.
      In_Count  := In_Len / Commit_Size;
      Out_Count := Out_Len / Commit_Size;

      declare
         Input_Array  : Anubis_Whisper.Commitment_Array (0 .. In_Count - 1);
         Output_Array : Anubis_Whisper.Commitment_Array (0 .. Out_Count - 1);
         Fee_Commit   : constant Anubis_Whisper.Amount_Commitment :=
           Make_Fee_Commit (Fee);
         Dummy_Proof  : Anubis_Types.Byte_Array (0 .. 255) := (others => 0);
         OK           : Boolean;
      begin
         --  Flatten VM byte arrays into WHISPER commitments
         for C in 0 .. In_Count - 1 loop
            for J in Commit_Index loop
               declare
                  Src_Idx : constant Natural :=
                    Input_Commits'First + C * Commit_Size + Natural (J);
               begin
                  Input_Array (C)(J) :=
                    Anubis_Types.Byte (Input_Commits (Src_Idx));
               end;
            end loop;
         end loop;

         for C in 0 .. Out_Count - 1 loop
            for J in Commit_Index loop
               declare
                  Src_Idx : constant Natural :=
                    Output_Commits'First + C * Commit_Size + Natural (J);
               begin
                  Output_Array (C)(J) :=
                    Anubis_Types.Byte (Output_Commits (Src_Idx));
               end;
            end loop;
         end loop;

         OK := Anubis_Whisper.Verify_Balance_Proof (
           Input_Commits  => Input_Array,
           Output_Commits => Output_Array,
           Fee_Commit     => Fee_Commit,
           Proof          => Dummy_Proof
         );

         Valid := OK;

         Result := (
           Success     => OK,
           Gas_Used    => Gas_Verify_Balance,
           Error_Code  => (if OK then Privacy_Error_None
                           else Privacy_Error_Range_Violation),
           Return_Hash => (others => 0)
         );
      end;
   end Verify_Balance;

   ---------------------------------------------------------------------------
   --  Private Execution Operations (GATE)
   ---------------------------------------------------------------------------

   procedure Private_Call (
      Target         : VM_Contract_Address;
      Function_Sel   : Unsigned_32;
      Private_Args   : VM_Byte_Array;
      Public_Args    : VM_Byte_Array;
      Gas_Limit      : VM_Gas_Amount;
      Mode           : Privacy_Mode;
      Proof          : out VM_Byte_Array;
      Output         : out VM_Byte_Array;
      Output_Len     : out Natural;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Private_Args, Public_Args, Gas_Limit);
      Gate_Request   : Anubis_Gate.Private_Execution_Request;
      Gate_State     : Anubis_Gate.Private_State;
      Gate_Result    : Anubis_Gate.Private_Execution_Result;
      Caller_SK      : Anubis_Types.Byte_Array (0 .. 3167) := (others => 0);
   begin
      --  Initialize outputs
      Proof := (others => 0);
      Output := (others => 0);
      Output_Len := 0;

      --  Build GATE execution request
      for I in Target'Range loop
         Gate_Request.Contract_Addr (I) := Anubis_Types.Byte (Target (I));
      end loop;
      Gate_Request.Function_Selector := Function_Sel;
      Gate_Request.Private_Args := (
         Ciphertext => (others => 0),
         CT_Length  => 0,
         Input_Hash => (others => 0)
      );
      Gate_Request.Public_Args := (others => 0);
      Gate_Request.Public_Args_Len := 0;
      Gate_Request.Mode := (case Mode is
         when Privacy_Off           => Anubis_Gate.Public_Result,
         when Privacy_Encrypted     => Anubis_Gate.Auditable,
         when Privacy_Confidential  => Anubis_Gate.Public_Result,
         when Privacy_Private       => Anubis_Gate.Full_Private,
         when Privacy_Anonymous     => Anubis_Gate.Full_Private);
      Gate_Request.Gas_Limit := Unsigned_64 (Gas_Limit);

      --  Initialize state
      Gate_State := (
         Ciphertext => (others => 0),
         CT_Length  => 0,
         State_Hash => (others => 0),
         Version    => 0
      );

      --  Execute privately
      Anubis_Gate.Execute_Private (
         Request       => Gate_Request,
         Current_State => Gate_State,
         Caller_SK     => Caller_SK,
         Result        => Gate_Result
      );

      if Gate_Result.Success then
         --  Copy proof
         for I in 0 .. Natural'Min (Proof'Length - 1,
                                    Gate_Result.Proof.Proof_Length - 1) loop
            Proof (Proof'First + I) :=
              VM_Byte (Gate_Result.Proof.Proof_Data (I));
         end loop;

         --  Copy output
         Output_Len := Natural'Min (Output'Length,
                                    Gate_Result.Output_Public_Len);
         for I in 0 .. Output_Len - 1 loop
            Output (Output'First + I) :=
              VM_Byte (Gate_Result.Output_Public (I));
         end loop;

         Result := (
            Success     => True,
            Gas_Used    => Gas_Private_Call_Base +
                          VM_Gas_Amount (Gate_Result.Gas_Used),
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
      else
         Result := (
            Success     => False,
            Gas_Used    => Gas_Private_Call_Base,
            Error_Code  => Privacy_Error_Invalid_Proof,
            Return_Hash => (others => 0)
         );
      end if;
   end Private_Call;

   procedure Verify_Execution_Proof (
      Proof          : VM_Byte_Array;
      Contract       : VM_Contract_Address;
      Old_State_Hash : VM_Hash256;
      New_State_Hash : VM_Hash256;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) is
      Gate_Proof     : Anubis_Gate.Execution_Proof;
      Contract_Addr  : Anubis_Types.Byte_Array (0 .. 31);
      Old_Hash       : Anubis_Types.Byte_Array (0 .. 31);
      New_Hash       : Anubis_Types.Byte_Array (0 .. 31);
   begin
      --  Convert proof
      Gate_Proof := (
         Proof_Data     => (others => 0),
         Proof_Length   => Natural'Min (Proof'Length,
                                        Anubis_Gate.Execution_Proof_Size),
         Old_State_Hash => (others => 0),
         New_State_Hash => (others => 0),
         Output_Hash    => (others => 0)
      );

      for I in 0 .. Gate_Proof.Proof_Length - 1 loop
         Gate_Proof.Proof_Data (I) := Anubis_Types.Byte (Proof (Proof'First + I));
      end loop;

      --  Convert addresses
      for I in Contract_Addr'Range loop
         Contract_Addr (I) := Anubis_Types.Byte (Contract (Contract'First + I));
         Old_Hash (I) := Anubis_Types.Byte (Old_State_Hash (Old_State_Hash'First + I));
         New_Hash (I) := Anubis_Types.Byte (New_State_Hash (New_State_Hash'First + I));
      end loop;

      --  Verify using GATE
      Valid := Anubis_Gate.Verify_Execution (
         Proof          => Gate_Proof,
         Contract_Addr  => Contract_Addr,
         Old_State_Hash => Old_Hash,
         New_State_Hash => New_Hash
      );

      Result := (
         Success     => Valid,
         Gas_Used    => Gas_Verify_Execution,
         Error_Code  => (if Valid then Privacy_Error_None
                        else Privacy_Error_Invalid_Proof),
         Return_Hash => (others => 0)
      );
   end Verify_Execution_Proof;

   procedure Create_Private_Session (
      Contract       : VM_Contract_Address;
      User_KEM_SK    : VM_Byte_Array;
      Contract_KEM_PK: VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Session        : out Session_ID;
      Result         : out Privacy_Result
   ) is
      Gate_Session   : Anubis_Gate.Private_Session;
      Contract_Addr  : Anubis_Types.Byte_Array (0 .. 31);
      Local_SK       : Anubis_Types.Byte_Array (0 .. 3167);
      Local_PK       : Anubis_Types.Byte_Array (0 .. 1567);
      Local_Rand     : Anubis_Types.Byte_Array (0 .. 63);
      Success        : Boolean;
   begin
      --  Convert inputs
      for I in Contract_Addr'Range loop
         Contract_Addr (I) := Anubis_Types.Byte (Contract (Contract'First + I));
      end loop;

      for I in Local_SK'Range loop
         Local_SK (I) := Anubis_Types.Byte (User_KEM_SK (User_KEM_SK'First + I));
      end loop;

      for I in Local_PK'Range loop
         Local_PK (I) := Anubis_Types.Byte (Contract_KEM_PK (Contract_KEM_PK'First + I));
      end loop;

      for I in Local_Rand'Range loop
         Local_Rand (I) := Anubis_Types.Byte (Randomness (Randomness'First + I));
      end loop;

      --  Create GATE session
      Anubis_Gate.Create_Session (
         Contract_Addr   => Contract_Addr,
         User_KEM_SK     => Local_SK,
         Contract_KEM_PK => Local_PK,
         Randomness      => Local_Rand,
         Session         => Gate_Session,
         Success         => Success
      );

      if Success then
         --  Copy session ID
         for I in Session'Range loop
            Session (I) := VM_Byte (Gate_Session.Session_ID (I));
         end loop;

         Result := (
            Success     => True,
            Gas_Used    => Gas_Create_Session,
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
      else
         Session := (others => 0);
         Result := (
            Success     => False,
            Gas_Used    => Gas_Create_Session / 2,
            Error_Code  => Privacy_Error_Invalid_Key,
            Return_Hash => (others => 0)
         );
      end if;
   end Create_Private_Session;

   procedure Close_Private_Session (
      Session        : in Out Session_ID;
      Result         : out Privacy_Result
   ) is
   begin
      --  Zeroize session data
      Zeroize_Session (Session);

      Result := (
         Success     => True,
         Gas_Used    => Gas_Close_Session,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Close_Private_Session;

   ---------------------------------------------------------------------------
   --  Selective Disclosure Operations (EYE)
   ---------------------------------------------------------------------------

   procedure Create_Disclosure (
      Credential     : VM_Byte_Array;
      Holder_Secret  : VM_Byte_Array;
      Disclose_Mask  : Unsigned_32;
      Challenge      : VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Credential);
      Eye_Cred       : Anubis_Eye.Attribute_Credential;
      Local_Secret   : Anubis_Types.Byte_Array (0 .. 31);
      Local_Challenge: Anubis_Types.Byte_Array (0 .. 31);
      Eye_Proof      : Anubis_Eye.Disclosure_Proof;
      Success        : Boolean;
   begin
      --  Initialize output
      Proof := (others => 0);

      --  Convert inputs
      for I in Local_Secret'Range loop
         Local_Secret (I) := Anubis_Types.Byte (
           Holder_Secret (Holder_Secret'First + I));
      end loop;

      for I in Local_Challenge'Range loop
         Local_Challenge (I) := Anubis_Types.Byte (
           Challenge (Challenge'First + I));
      end loop;

      --  Initialize credential (would normally be deserialized from Credential)
      Eye_Cred := (
         Attrs        => (others => (others => 0)),
         Attr_Count   => 0,
         Issuer_PK    => (others => 0),
         Signature    => (others => 0),
         Holder_Commit => (others => 0)
      );

      --  Create disclosure proof
      Anubis_Eye.Create_Disclosure_Proof (
         Credential    => Eye_Cred,
         Holder_Secret => Local_Secret,
         Disclose_Mask => Disclose_Mask,
         Challenge     => Local_Challenge,
         Proof         => Eye_Proof,
         Success       => Success
      );

      if Success then
         --  Copy proof data
         for I in 0 .. Natural'Min (Proof'Length - 1,
                                    Anubis_Eye.Disclosure_Proof_Size - 1) loop
            Proof (Proof'First + I) := VM_Byte (Eye_Proof.Proof_Data (I));
         end loop;

         Result := (
            Success     => True,
            Gas_Used    => Gas_Create_Disclosure,
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
      else
         Result := (
            Success     => False,
            Gas_Used    => Gas_Create_Disclosure / 2,
            Error_Code  => Privacy_Error_Invalid_Proof,
            Return_Hash => (others => 0)
         );
      end if;
   end Create_Disclosure;

   procedure Verify_Disclosure (
      Proof          : VM_Byte_Array;
      Disclosed_Attrs: VM_Byte_Array;
      Issuer_PK      : VM_Byte_Array;
      Challenge      : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Proof, Disclosed_Attrs);
      Local_PK       : Anubis_Types.Byte_Array (0 .. 2591);
      Local_Challenge: Anubis_Types.Byte_Array (0 .. 31);
      Eye_Proof      : Anubis_Eye.Disclosure_Proof;
      Empty_Attrs    : Anubis_Eye.Attribute_Input_Array (0 .. -1);
   begin
      --  Convert inputs
      for I in Local_PK'Range loop
         Local_PK (I) := Anubis_Types.Byte (Issuer_PK (Issuer_PK'First + I));
      end loop;

      for I in Local_Challenge'Range loop
         Local_Challenge (I) := Anubis_Types.Byte (
           Challenge (Challenge'First + I));
      end loop;

      --  Initialize proof structure
      Eye_Proof := (
         Proof_Data      => (others => 0),
         Disclosed_Mask  => 0,
         Credential_Hash => (others => 0)
      );

      --  Verify disclosure
      Valid := Anubis_Eye.Verify_Disclosure (
         Proof           => Eye_Proof,
         Disclosed_Attrs => Empty_Attrs,
         Issuer_PK       => Local_PK,
         Challenge       => Local_Challenge
      );

      Result := (
         Success     => Valid,
         Gas_Used    => Gas_Verify_Disclosure,
         Error_Code  => (if Valid then Privacy_Error_None
                        else Privacy_Error_Invalid_Proof),
         Return_Hash => (others => 0)
      );
   end Verify_Disclosure;

   procedure Derive_Viewing_Key (
      Master_Seed    : VM_Byte_Array;
      View_Key       : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) is
      Local_Seed     : Anubis_Types.Byte_Array (0 .. 31);
      Local_View_Key : Anubis_Shield.Viewing_Key;
   begin
      --  Convert seed
      for I in Local_Seed'Range loop
         Local_Seed (I) := Anubis_Types.Byte (Master_Seed (Master_Seed'First + I));
      end loop;

      --  Derive viewing key using SHIELD
      Anubis_Shield.Derive_Viewing_Key (
         Master_Seed => Local_Seed,
         View_Key    => Local_View_Key
      );

      --  Copy result
      for I in Local_View_Key'Range loop
         View_Key (View_Key'First + I) := VM_Byte (Local_View_Key (I));
      end loop;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Derive_View_Key,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Derive_Viewing_Key;

   procedure Generate_Stealth_Address (
      View_Key       : VM_Byte_Array;
      Spend_Key      : VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Stealth_Addr   : out VM_Byte_Array;
      Tx_Public_Key  : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) is
      Local_View     : Anubis_Eye.Viewing_Key;
      Local_Spend    : Anubis_Types.Byte_Array (0 .. 31);
      Local_Rand     : Anubis_Types.Byte_Array (0 .. 31);
      Local_Stealth  : Anubis_Types.Byte_Array (0 .. 31);
      Local_TxPK     : Anubis_Types.Byte_Array (0 .. 31);
   begin
      --  Convert inputs
      for I in Local_View'Range loop
         Local_View (I) := Anubis_Types.Byte (View_Key (View_Key'First + I));
         Local_Spend (I) := Anubis_Types.Byte (Spend_Key (Spend_Key'First + I));
         Local_Rand (I) := Anubis_Types.Byte (Randomness (Randomness'First + I));
      end loop;

      --  Generate stealth address using EYE
      Anubis_Eye.Generate_Stealth_Address (
         View_Key      => Local_View,
         Spend_Key     => Local_Spend,
         Randomness    => Local_Rand,
         Stealth_Addr  => Local_Stealth,
         Tx_Public_Key => Local_TxPK
      );

      --  Copy outputs
      for I in 0 .. 31 loop
         Stealth_Addr (Stealth_Addr'First + I) := VM_Byte (Local_Stealth (I));
         Tx_Public_Key (Tx_Public_Key'First + I) := VM_Byte (Local_TxPK (I));
      end loop;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Generate_Stealth,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Generate_Stealth_Address;

   ---------------------------------------------------------------------------
   --  Ring Signature Operations (VEIL)
   ---------------------------------------------------------------------------

   procedure Ring_Sign (
      Ring_PKs       : VM_Byte_Array;
      Ring_Size      : Natural;
      Signer_Index   : Natural;
      Signer_SK      : VM_Byte_Array;
      Message        : VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Signature      : out VM_Byte_Array;
      Key_Image      : out VM_Byte_Array;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off  -- Complex ring signature implementation
   is
      pragma Unreferenced (Ring_PKs, Message, Randomness);
   begin
      --  Initialize outputs
      Signature := (others => 0);
      Key_Image := (others => 0);

      if Ring_Size < Anubis_Ring_Sig.Min_Ring_Size then
         Result := (
            Success     => False,
            Gas_Used    => Gas_Ring_Sign_Base,
            Error_Code  => Privacy_Error_Ring_Too_Small,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      if Signer_Index >= Ring_Size then
         Result := (
            Success     => False,
            Gas_Used    => Gas_Ring_Sign_Base,
            Error_Code  => Privacy_Error_Invalid_Key,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Compute key image from signer secret key
      for I in 0 .. Natural'Min (63, Key_Image'Length - 1) loop
         if I < Signer_SK'Length then
            Key_Image (Key_Image'First + I) := Signer_SK (Signer_SK'First + I);
         end if;
      end loop;

      --  Signature placeholder (actual implementation would use Anubis_Ring_Sig)
      Result := (
         Success     => True,
         Gas_Used    => Gas_Ring_Sign_Base +
                       Gas_Ring_Sign_Per_Member * VM_Gas_Amount (Ring_Size),
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Ring_Sign;

   procedure Verify_Ring_Signature (
      Ring_PKs       : VM_Byte_Array;
      Ring_Size      : Natural;
      Message        : VM_Byte_Array;
      Signature      : VM_Byte_Array;
      Key_Image      : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Ring_PKs, Message, Signature, Key_Image);
   begin
      if Ring_Size < Anubis_Ring_Sig.Min_Ring_Size then
         Valid := False;
         Result := (
            Success     => False,
            Gas_Used    => Gas_Verify_Ring_Sig_Base,
            Error_Code  => Privacy_Error_Ring_Too_Small,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Verification using Anubis_Ring_Sig would go here
      Valid := True;  -- Stub

      Result := (
         Success     => True,
         Gas_Used    => Gas_Verify_Ring_Sig_Base +
                       Gas_Verify_Ring_Per_Member * VM_Gas_Amount (Ring_Size),
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Verify_Ring_Signature;

   procedure Compute_Key_Image (
      Secret_Key     : VM_Byte_Array;
      Key_Image      : out VM_Byte_Array;
      Result         : out Privacy_Result
   ) is
      Local_SK       : Anubis_Ring_Sig.Secret_Key;
      Local_Image    : Anubis_Ring_Sig.Key_Image;
   begin
      --  Initialize secret key structure
      Local_SK := (
         Secret  => Anubis_Lattice_ZK.Zero_Ring,
         Opening => (Randomness => Anubis_Lattice_ZK.Zero_Vector)
      );

      --  Compute key image using VEIL/Ring_Sig
      Anubis_Ring_Sig.Compute_Key_Image (
         SK    => Local_SK,
         Image => Local_Image
      );

      --  Copy result
      for I in Local_Image'Range loop
         Key_Image (Key_Image'First + I) := VM_Byte (Local_Image (I));
      end loop;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Compute_Key_Image,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );

      pragma Unreferenced (Secret_Key);
   end Compute_Key_Image;

   procedure Check_Key_Image_Spent (
      Key_Image      : VM_Byte_Array;
      Spent_Images   : VM_Byte_Array;
      Num_Spent      : Natural;
      Is_Spent       : out Boolean;
      Result         : out Privacy_Result
   ) is
      Image_Size     : constant := Anubis_Ring_Sig.Key_Image_Size;
   begin
      Is_Spent := False;

      --  Check against all spent images
      for I in 0 .. Num_Spent - 1 loop
         declare
            Match : Boolean := True;
         begin
            for J in 0 .. Image_Size - 1 loop
               if Key_Image (Key_Image'First + J) /=
                  Spent_Images (Spent_Images'First + I * Image_Size + J)
               then
                  Match := False;
                  exit;
               end if;
            end loop;

            if Match then
               Is_Spent := True;
               exit;
            end if;
         end;
      end loop;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Check_Spent,
         Error_Code  => (if Is_Spent then Privacy_Error_Key_Image_Spent
                        else Privacy_Error_None),
         Return_Hash => (others => 0)
      );
   end Check_Key_Image_Spent;

   ---------------------------------------------------------------------------
   --  Confidential Transfer Operations (WHISPER Extended)
   ---------------------------------------------------------------------------

   procedure Confidential_Transfer (
      Sender_Commit  : VM_Byte_Array;
      Recv_Commit    : VM_Byte_Array;
      Transfer_Proof : VM_Byte_Array;
      Recv_Addr      : VM_Contract_Address;
      Result         : out Privacy_Result
   ) is
      pragma Unreferenced (Sender_Commit, Recv_Commit, Transfer_Proof, Recv_Addr);
   begin
      --  Confidential transfer:
      --  1. Verify balance proof (sender_commit = recv_commit + fee_commit)
      --  2. Update sender balance commitment
      --  3. Create receiver output with stealth address
      --  4. Emit encrypted event for receiver

      Result := (
         Success     => True,
         Gas_Used    => Gas_Confidential_Transfer,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Confidential_Transfer;

   procedure Create_Transfer_Proof (
      Input_Value    : Unsigned_64;
      Output_Value   : Unsigned_64;
      Input_Blinding : VM_Byte_Array;
      Output_Blinding: VM_Byte_Array;
      Randomness     : VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off  -- Complex ZK proof generation
   is
      pragma Unreferenced (Input_Blinding, Output_Blinding, Randomness);
   begin
      --  Initialize output
      Proof := (others => 0);

      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Verify balance: input >= output (allows fee)
      if Input_Value < Output_Value then
         Result := (
            Success     => False,
            Gas_Used    => Gas_Create_Transfer_Proof / 2,
            Error_Code  => Privacy_Error_Range_Violation,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Create proof that:
      --  1. Input commitment opens to Input_Value
      --  2. Output commitment opens to Output_Value
      --  3. Fee = Input_Value - Output_Value >= 0
      --  Uses lattice ZK range proofs for each component

      Result := (
         Success     => True,
         Gas_Used    => Gas_Create_Transfer_Proof,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end Create_Transfer_Proof;

   procedure Verify_Confidential_Transfer (
      Sender_Commit  : VM_Byte_Array;
      Recv_Commit    : VM_Byte_Array;
      Fee_Commit     : VM_Byte_Array;
      Transfer_Proof : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off  -- Complex ZK verification
   is
      pragma Unreferenced (Transfer_Proof);
      Local_Sender   : Anubis_Whisper.Amount_Commitment;
      Local_Recv     : Anubis_Whisper.Amount_Commitment;
      Local_Fee      : Anubis_Whisper.Amount_Commitment;
      Sum_Recv_Fee   : Anubis_Whisper.Amount_Commitment;
   begin
      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Convert commitments
      for I in Local_Sender'Range loop
         Local_Sender (I) := Anubis_Types.Byte (Sender_Commit (Sender_Commit'First + I));
         Local_Recv (I) := Anubis_Types.Byte (Recv_Commit (Recv_Commit'First + I));
         Local_Fee (I) := Anubis_Types.Byte (Fee_Commit (Fee_Commit'First + I));
      end loop;

      --  Homomorphically add receiver + fee commitments
      Anubis_Whisper.Add_Commitments (
         A      => Local_Recv,
         B      => Local_Fee,
         Result => Sum_Recv_Fee
      );

      --  Verify: sender_commit = recv_commit + fee_commit
      Valid := True;
      for I in Local_Sender'Range loop
         if Local_Sender (I) /= Sum_Recv_Fee (I) then
            Valid := False;
            exit;
         end if;
      end loop;

      --  Also need to verify range proofs for receiver and fee
      --  (ensures they're non-negative)

      Result := (
         Success     => Valid,
         Gas_Used    => Gas_Verify_Transfer,
         Error_Code  => (if Valid then Privacy_Error_None
                        else Privacy_Error_Invalid_Proof),
         Return_Hash => (others => 0)
      );
   end Verify_Confidential_Transfer;

   procedure Scan_Confidential_Output (
      Tx_Public_Key  : VM_Byte_Array;
      Output_Commit  : VM_Byte_Array;
      View_Key       : VM_Byte_Array;
      Spend_Key      : VM_Byte_Array;
      Is_Owned       : out Boolean;
      Decrypted_Value: out Unsigned_64;
      Result         : out Privacy_Result
   ) is
      Local_TxPK     : Anubis_Types.Byte_Array (0 .. 31);
      Local_ViewKey  : Anubis_Eye.Viewing_Key;
      Local_SpendKey : Anubis_Types.Byte_Array (0 .. 31);
      Stealth_Addr   : Anubis_Types.Byte_Array (0 .. 31);
      Expected_Addr  : Anubis_Types.Byte_Array (0 .. 31);
   begin
      --  Initialize outputs
      Is_Owned := False;
      Decrypted_Value := 0;

      --  Convert inputs
      for I in Local_TxPK'Range loop
         Local_TxPK (I) := Anubis_Types.Byte (Tx_Public_Key (Tx_Public_Key'First + I));
         Local_ViewKey (I) := Anubis_Types.Byte (View_Key (View_Key'First + I));
         Local_SpendKey (I) := Anubis_Types.Byte (Spend_Key (Spend_Key'First + I));
      end loop;

      --  Compute expected stealth address: P = H(r*V)*G + S
      --  where r is transaction randomness (encoded in Tx_Public_Key)
      --  V is view public key, S is spend public key
      Anubis_Eye.Generate_Stealth_Address (
         View_Key      => Local_ViewKey,
         Spend_Key     => Local_SpendKey,
         Randomness    => Local_TxPK,  -- Use tx key as randomness derivation
         Stealth_Addr  => Stealth_Addr,
         Tx_Public_Key => Expected_Addr
      );

      --  Check if this output belongs to us by comparing addresses
      --  In a real implementation, we'd derive the one-time private key
      --  and attempt to decrypt the amount

      --  Simple ownership check (real impl needs key derivation)
      Is_Owned := True;
      for I in 0 .. 31 loop
         if Output_Commit (Output_Commit'First + I) /= 0 then
            --  Has content, assume we can scan it
            null;
         end if;
      end loop;

      --  If owned, try to decrypt value using view key
      if Is_Owned then
         --  Decrypted_Value would be recovered from encrypted amount
         --  using shared secret derived from view key and tx public key
         Decrypted_Value := 0;  -- Stub: actual decryption needed
      end if;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Scan_Output,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );

      pragma Unreferenced (Output_Commit);
   end Scan_Confidential_Output;

   ---------------------------------------------------------------------------
   --  Lattice ZK Proof Operations (VEIL/Lattice)
   ---------------------------------------------------------------------------

   procedure ZK_Prove_Range (
      Value          : Unsigned_64;
      Num_Bits       : Natural;
      Randomness     : VM_Byte_Array;
      Commitment     : out VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off  -- Complex ZK proof
   is
      Local_Rand     : Anubis_Types.Byte_Array (0 .. 63);
      Local_Trans    : Anubis_Types.Byte_Array (0 .. 31);
      ZK_Com         : Anubis_Lattice_ZK.Commitment;
      ZK_Proof       : Anubis_Lattice_ZK.Range_Proof;
      Com_Bytes      : Anubis_Types.Byte_Array (0 .. Anubis_Lattice_ZK.Commitment_Bytes - 1);
      Com_Len        : Natural;
   begin
      --  Initialize outputs
      Commitment := (others => 0);
      Proof := (others => 0);

      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Convert randomness
      for I in Local_Rand'Range loop
         Local_Rand (I) := Anubis_Types.Byte (Randomness (Randomness'First + I));
      end loop;

      --  Generate transcript
      Local_Trans := (others => 0);
      Hash_Transcript (Privacy_Domain, Local_Trans);

      --  Generate range proof using Anubis_Lattice_ZK
      Anubis_Lattice_ZK.Prove_Range (
         Params     => ZK_Public_Params,
         Value      => Value,
         Num_Bits   => Num_Bits,
         Randomness => Local_Rand,
         Transcript => Local_Trans,
         Com        => ZK_Com,
         Proof      => ZK_Proof
      );

      --  Serialize commitment
      Anubis_Lattice_ZK.Serialize_Commitment (ZK_Com, Com_Bytes, Com_Len);
      for I in 0 .. Natural'Min (Commitment'Length - 1, Com_Len - 1) loop
         Commitment (Commitment'First + I) := VM_Byte (Com_Bytes (I));
      end loop;

      Result := (
         Success     => True,
         Gas_Used    => Gas_ZK_Prove_Range,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end ZK_Prove_Range;

   procedure ZK_Verify_Range (
      Commitment     : VM_Byte_Array;
      Num_Bits       : Natural;
      Proof          : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off  -- Complex ZK verification
   is
      pragma Unreferenced (Proof);
      ZK_Com         : Anubis_Lattice_ZK.Commitment;
      ZK_Proof       : Anubis_Lattice_ZK.Range_Proof;
      Local_Trans    : Anubis_Types.Byte_Array (0 .. 31);
      Success        : Boolean;
      Com_Input      : Anubis_Types.Byte_Array (0 .. Commitment'Length - 1);
   begin
      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Convert commitment
      for I in Com_Input'Range loop
         Com_Input (I) := Anubis_Types.Byte (Commitment (Commitment'First + I));
      end loop;

      --  Deserialize commitment
      Anubis_Lattice_ZK.Deserialize_Commitment (Com_Input, ZK_Com, Success);
      if not Success then
         Valid := False;
         Result := (
            Success     => False,
            Gas_Used    => Gas_ZK_Verify_Range / 2,
            Error_Code  => Privacy_Error_Invalid_Commitment,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Initialize proof structure
      ZK_Proof := (
         Bit_Commits  => (others => (Value => Anubis_Lattice_ZK.Zero_Vector)),
         Num_Bits     => Num_Bits,
         Bit_Openings => (others => (
            Response  => Anubis_Lattice_ZK.Zero_Vector,
            Challenge => (
               Coeffs => Anubis_Lattice_ZK.Zero_Ring,
               Weight => 0
            )
         )),
         Sum_Proof    => (
            Z1        => Anubis_Lattice_ZK.Zero_Vector,
            Z2        => Anubis_Lattice_ZK.Zero_Vector,
            Challenge => (
               Coeffs => Anubis_Lattice_ZK.Zero_Ring,
               Weight => 0
            )
         )
      );

      --  Generate transcript
      Local_Trans := (others => 0);
      Hash_Transcript (Privacy_Domain, Local_Trans);

      --  Verify range proof
      Valid := Anubis_Lattice_ZK.Verify_Range (
         Params     => ZK_Public_Params,
         Com        => ZK_Com,
         Num_Bits   => Num_Bits,
         Proof      => ZK_Proof,
         Transcript => Local_Trans
      );

      Result := (
         Success     => Valid,
         Gas_Used    => Gas_ZK_Verify_Range,
         Error_Code  => (if Valid then Privacy_Error_None
                        else Privacy_Error_Range_Violation),
         Return_Hash => (others => 0)
      );
   end ZK_Verify_Range;

   procedure ZK_Prove_Linear (
      X_Value        : VM_Byte_Array;
      Y_Value        : VM_Byte_Array;
      A_Coeff        : Integer;
      B_Coeff        : Integer;
      Randomness     : VM_Byte_Array;
      Proof          : out VM_Byte_Array;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off
   is
      pragma Unreferenced (X_Value, Y_Value, Randomness, A_Coeff, B_Coeff);
   begin
      --  Initialize output
      Proof := (others => 0);

      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Linear proof generation would use Anubis_Lattice_ZK.Prove_Linear

      Result := (
         Success     => True,
         Gas_Used    => Gas_ZK_Prove_Linear,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end ZK_Prove_Linear;

   procedure ZK_Verify_Linear (
      Com_X, Com_Y   : VM_Byte_Array;
      A_Coeff        : Integer;
      B_Coeff        : Integer;
      C_Result       : VM_Byte_Array;
      Proof          : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off
   is
      pragma Unreferenced (Com_X, Com_Y, A_Coeff, B_Coeff, C_Result, Proof);
   begin
      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Linear verification would use Anubis_Lattice_ZK.Verify_Linear
      Valid := True;  -- Stub

      Result := (
         Success     => Valid,
         Gas_Used    => Gas_ZK_Verify_Linear,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );
   end ZK_Verify_Linear;

   ---------------------------------------------------------------------------
   --  Syscall Dispatcher Integration
   ---------------------------------------------------------------------------

   function Is_Privacy_Syscall (Syscall : Syscall_Number) return Boolean is
   begin
      return Syscall in 16#80# .. 16#9A#;
   end Is_Privacy_Syscall;

   function Get_Privacy_Gas_Cost (
      Syscall   : Syscall_Number;
      Data_Size : Natural := 0;
      Ring_Size : Natural := 0
   ) return VM_Gas_Amount is
      pragma Unreferenced (Data_Size);
   begin
      case Syscall is
         --  SHIELD operations
         when Syscall_Private_Store   => return Gas_Private_Store;
         when Syscall_Private_Load    => return Gas_Private_Load;
         when Syscall_Private_Delete  => return Gas_Private_Delete;

         --  WHISPER operations
         when Syscall_Commit_Amount   => return Gas_Commit_Amount;
         when Syscall_Verify_Range    => return Gas_Verify_Range;
         when Syscall_Add_Commitments => return Gas_Add_Commitments;
         when Syscall_Verify_Balance  => return Gas_Verify_Balance;

         --  GATE operations
         when Syscall_Private_Call    => return Gas_Private_Call_Base;
         when Syscall_Verify_Execution=> return Gas_Verify_Execution;
         when Syscall_Create_Session  => return Gas_Create_Session;
         when Syscall_Close_Session   => return Gas_Close_Session;

         --  EYE operations
         when Syscall_Create_Disclosure => return Gas_Create_Disclosure;
         when Syscall_Verify_Disclosure => return Gas_Verify_Disclosure;
         when Syscall_Derive_View_Key   => return Gas_Derive_View_Key;
         when Syscall_Generate_Stealth  => return Gas_Generate_Stealth;

         --  VEIL operations
         when Syscall_Ring_Sign       =>
            return Gas_Ring_Sign_Base +
                   Gas_Ring_Sign_Per_Member * VM_Gas_Amount (Ring_Size);
         when Syscall_Verify_Ring_Sig =>
            return Gas_Verify_Ring_Sig_Base +
                   Gas_Verify_Ring_Per_Member * VM_Gas_Amount (Ring_Size);
         when Syscall_Compute_Key_Image => return Gas_Compute_Key_Image;
         when Syscall_Check_Spent     => return Gas_Check_Spent;

         --  Lattice ZK operations
         when Syscall_ZK_Prove_Range  => return Gas_ZK_Prove_Range;
         when Syscall_ZK_Verify_Range => return Gas_ZK_Verify_Range;
         when Syscall_ZK_Prove_Linear => return Gas_ZK_Prove_Linear;
         when Syscall_ZK_Verify_Linear=> return Gas_ZK_Verify_Linear;

         --  Confidential transfer operations (WHISPER extended)
         when Syscall_Confidential_Transfer => return Gas_Confidential_Transfer;
         when Syscall_Create_Transfer_Proof => return Gas_Create_Transfer_Proof;
         when Syscall_Verify_Transfer       => return Gas_Verify_Transfer;
         when Syscall_Scan_Output           => return Gas_Scan_Output;

         when others                  => return 0;
      end case;
   end Get_Privacy_Gas_Cost;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Privacy_Context (Ctx : in Out Privacy_Context) is
   begin
      Ctx.Mode := Privacy_Off;
      for I in Ctx.User_KEM_PK'Range loop
         Ctx.User_KEM_PK (I) := 0;
      end loop;
      for I in Ctx.View_Key'Range loop
         Ctx.View_Key (I) := 0;
      end loop;
      Ctx.Session_Active := False;
      Zeroize_Session (Ctx.Session_ID);
   end Zeroize_Privacy_Context;

   procedure Zeroize_Session (Session : in Out Session_ID) is
   begin
      for I in Session'Range loop
         Session (I) := 0;
      end loop;
   end Zeroize_Session;

end Aegis_Privacy;
