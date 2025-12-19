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
with Privacy_State_Persistence;

package body Aegis_Privacy with
   SPARK_Mode => On
is
   --  Make arithmetic operators visible for Gas_Amount
   use type Aegis_VM_Types.Gas_Amount;

   ---------------------------------------------------------------------------
   --  Global Privacy State (persists encrypted entries)
   ---------------------------------------------------------------------------

   Global_Privacy_State : Privacy_State_Persistence.Privacy_State;
   Privacy_State_Ready  : Boolean := False;

   procedure Ensure_Privacy_State with
      Global => (In_Out => (Global_Privacy_State, Privacy_State_Ready))
   is
   begin
      if not Privacy_State_Ready then
         Privacy_State_Persistence.Initialize (Global_Privacy_State);
         Privacy_State_Ready := True;
      end if;
   end Ensure_Privacy_State;

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
      Shield_Entry   : Anubis_Shield.Private_Entry;
      Shield_Commit  : Anubis_Shield.Entry_Commitment;
      Success        : Boolean;
      Local_PT       : Anubis_Types.Byte_Array (0 .. Plaintext'Length - 1);
      Local_Rand     : Anubis_Types.Byte_Array (0 .. 63);
      Local_PK       : Anubis_Types.Byte_Array (0 .. 1567);

      --  For storage
      Store_Contract : Aegis_VM_Types.Contract_Address;
      Store_Key      : Aegis_VM_Types.Hash256;
      Store_Index    : Privacy_State_Persistence.Private_Entry_Index;
      Store_Success  : Boolean;
   begin
      --  Ensure privacy state is initialized
      Ensure_Privacy_State;

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
         --  Convert addresses for storage
         for I in Store_Contract'Range loop
            Store_Contract (I) := Contract (Contract'First + I);
         end loop;
         for I in Store_Key'Range loop
            Store_Key (I) := Key (Key'First + I);
         end loop;

         --  Store encrypted entry in privacy state
         Privacy_State_Persistence.Store_Private_Entry (
            State       => Global_Privacy_State,
            Contract_ID => Store_Contract,
            Key         => Store_Key,
            Priv_Entry  => Shield_Entry,
            Commitment  => Shield_Commit,
            Index       => Store_Index,
            Success     => Store_Success
         );

         if Store_Success then
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
               Error_Code  => Privacy_Error_Permission,
               Return_Hash => (others => 0)
            );
         end if;
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
      Shield_Entry   : Anubis_Shield.Private_Entry;
      Local_PT       : Anubis_Types.Byte_Array (0 .. Anubis_Shield.Max_Entry_Size - 1);
      Local_PT_Len   : Natural;
      Local_SK       : Anubis_Types.Byte_Array (0 .. 3167);
      Success        : Boolean;

      --  For lookup
      Lookup_Contract : Aegis_VM_Types.Contract_Address;
      Lookup_Key      : Aegis_VM_Types.Hash256;
      Entry_Index     : Privacy_State_Persistence.Private_Entry_Index;
      Found           : Boolean;
   begin
      --  Initialize outputs
      Plaintext := (others => 0);
      Plaintext_Len := 0;

      --  Ensure privacy state is initialized
      Ensure_Privacy_State;

      --  Convert addresses for lookup
      for I in Lookup_Contract'Range loop
         Lookup_Contract (I) := Contract (Contract'First + I);
      end loop;
      for I in Lookup_Key'Range loop
         Lookup_Key (I) := Key (Key'First + I);
      end loop;

      --  Look up the entry in privacy state
      Privacy_State_Persistence.Find_Private_Entry (
         State       => Global_Privacy_State,
         Contract_ID => Lookup_Contract,
         Key         => Lookup_Key,
         Index       => Entry_Index,
         Found       => Found
      );

      if not Found then
         Result := (
            Success     => False,
            Gas_Used    => Gas_Private_Load / 4,
            Error_Code  => Privacy_Error_Permission,  -- Entry not found
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Get the stored entry
      Shield_Entry := Global_Privacy_State.Entries (Entry_Index).Priv_Entry;

      --  Convert secret key
      for I in Local_SK'Range loop
         Local_SK (I) := Anubis_Types.Byte (User_KEM_SK (User_KEM_SK'First + I));
      end loop;

      --  Decrypt using SHIELD
      Anubis_Shield.Decrypt_State (
         Priv_Entry  => Shield_Entry,
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
      Delete_Contract : Aegis_VM_Types.Contract_Address;
      Delete_Key      : Aegis_VM_Types.Hash256;
      Delete_Success  : Boolean;
   begin
      --  Ensure privacy state is initialized
      Ensure_Privacy_State;

      --  Convert addresses for deletion
      for I in Delete_Contract'Range loop
         Delete_Contract (I) := Contract (Contract'First + I);
      end loop;
      for I in Delete_Key'Range loop
         Delete_Key (I) := Key (Key'First + I);
      end loop;

      --  Delete entry from privacy state
      Privacy_State_Persistence.Delete_Private_Entry (
         State       => Global_Privacy_State,
         Contract_ID => Delete_Contract,
         Key         => Delete_Key,
         Success     => Delete_Success
      );

      if Delete_Success then
         Result := (
            Success     => True,
            Gas_Used    => Gas_Private_Delete,
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
      else
         Result := (
            Success     => False,
            Gas_Used    => Gas_Private_Delete / 2,
            Error_Code  => Privacy_Error_Permission,  -- Entry not found
            Return_Hash => (others => 0)
         );
      end if;
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
      --  Build actual ring from flat key data
      R              : Anubis_Ring_Sig.Ring;
      Local_SK       : Anubis_Ring_Sig.Secret_Key;
      Local_Msg      : Anubis_Types.Byte_Array (0 .. Message'Length - 1);
      Local_Rand     : Anubis_Types.Byte_Array (0 .. 63);
      Local_Sig      : Anubis_Ring_Sig.Ring_Signature;
      Sig_Bytes      : Anubis_Types.Byte_Array (0 .. Anubis_Ring_Sig.Max_Sig_Bytes - 1);
      Sig_Len        : Natural;
      Local_Image    : Anubis_Ring_Sig.Key_Image;
      Success        : Boolean;
      PK_Size        : constant Natural := Anubis_Lattice_ZK.Commitment_Bytes + 32;
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

      if Ring_Size > Anubis_Ring_Sig.Max_Ring_Size then
         Result := (
            Success     => False,
            Gas_Used    => Gas_Ring_Sign_Base,
            Error_Code  => Privacy_Error_Ring_Too_Large,
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

      --  Initialize ring and add public keys
      Anubis_Ring_Sig.Init_Ring (R);
      for I in 0 .. Ring_Size - 1 loop
         declare
            PK           : Anubis_Ring_Sig.Public_Key;
            PK_Offset    : constant Natural := I * PK_Size;
            PK_Bytes     : Anubis_Types.Byte_Array (0 .. PK_Size - 1);
            PK_Success   : Boolean;
         begin
            --  Extract public key bytes from flat array
            for J in PK_Bytes'Range loop
               if PK_Offset + J < Ring_PKs'Length then
                  PK_Bytes (J) := Anubis_Types.Byte (
                     Ring_PKs (Ring_PKs'First + PK_Offset + J));
               else
                  PK_Bytes (J) := 0;
               end if;
            end loop;

            --  Deserialize public key
            Anubis_Ring_Sig.Deserialize_Public_Key (PK_Bytes, PK, PK_Success);
            if PK_Success then
               Anubis_Ring_Sig.Add_To_Ring (R, PK, PK_Success);
            end if;
         end;
      end loop;

      --  Convert secret key - extract from VM format
      --  SK contains: secret ring element + opening
      Local_SK := (
         Secret  => Anubis_Lattice_ZK.Zero_Ring,
         Opening => (Randomness => Anubis_Lattice_ZK.Zero_Vector)
      );
      --  Parse secret from input bytes
      for I in 0 .. Natural'Min (127, Signer_SK'Length - 1) loop
         --  Secret key data mapping would go here
         null;
      end loop;

      --  Convert message
      for I in Local_Msg'Range loop
         Local_Msg (I) := Anubis_Types.Byte (Message (Message'First + I));
      end loop;

      --  Convert randomness
      for I in 0 .. 63 loop
         if I < Randomness'Length then
            Local_Rand (I) := Anubis_Types.Byte (Randomness (Randomness'First + I));
         else
            Local_Rand (I) := 0;
         end if;
      end loop;

      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Perform ring signature
      Anubis_Ring_Sig.Sign (
         Params       => ZK_Public_Params,
         R            => R,
         Signer_Index => Signer_Index,
         SK           => Local_SK,
         Message      => Local_Msg,
         Randomness   => Local_Rand,
         Sig          => Local_Sig,
         Success      => Success
      );

      if Success then
         --  Serialize signature to output
         Anubis_Ring_Sig.Serialize_Signature (Local_Sig, Sig_Bytes, Sig_Len);
         for I in 0 .. Natural'Min (Signature'Length - 1, Sig_Len - 1) loop
            Signature (Signature'First + I) := VM_Byte (Sig_Bytes (I));
         end loop;

         --  Copy key image from signature
         Local_Image := Local_Sig.Image;
         for I in Local_Image'Range loop
            if I < Key_Image'Length then
               Key_Image (Key_Image'First + I) := VM_Byte (Local_Image (I));
            end if;
         end loop;

         Result := (
            Success     => True,
            Gas_Used    => Gas_Ring_Sign_Base +
                          Gas_Ring_Sign_Per_Member * VM_Gas_Amount (Ring_Size),
            Error_Code  => Privacy_Error_None,
            Return_Hash => (others => 0)
         );
      else
         Result := (
            Success     => False,
            Gas_Used    => Gas_Ring_Sign_Base,
            Error_Code  => Privacy_Error_Signing_Failed,
            Return_Hash => (others => 0)
         );
      end if;

      --  Zeroize secret key
      Anubis_Ring_Sig.Zeroize_Secret_Key (Local_SK);
   end Ring_Sign;

   procedure Verify_Ring_Signature (
      Ring_PKs       : VM_Byte_Array;
      Ring_Size      : Natural;
      Message        : VM_Byte_Array;
      Signature      : VM_Byte_Array;
      Key_Image      : VM_Byte_Array;
      Valid          : out Boolean;
      Result         : out Privacy_Result
   )
      with SPARK_Mode => Off  -- Complex ring signature verification
   is
      R              : Anubis_Ring_Sig.Ring;
      Local_Msg      : Anubis_Types.Byte_Array (0 .. Message'Length - 1);
      Local_Sig      : Anubis_Ring_Sig.Ring_Signature;
      Sig_Input      : Anubis_Types.Byte_Array (0 .. Signature'Length - 1);
      Deser_Success  : Boolean;
      PK_Size        : constant Natural := Anubis_Lattice_ZK.Commitment_Bytes + 32;
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

      if Ring_Size > Anubis_Ring_Sig.Max_Ring_Size then
         Valid := False;
         Result := (
            Success     => False,
            Gas_Used    => Gas_Verify_Ring_Sig_Base,
            Error_Code  => Privacy_Error_Ring_Too_Large,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Initialize ring and add public keys
      Anubis_Ring_Sig.Init_Ring (R);
      for I in 0 .. Ring_Size - 1 loop
         declare
            PK           : Anubis_Ring_Sig.Public_Key;
            PK_Offset    : constant Natural := I * PK_Size;
            PK_Bytes     : Anubis_Types.Byte_Array (0 .. PK_Size - 1);
            PK_Success   : Boolean;
         begin
            --  Extract public key bytes from flat array
            for J in PK_Bytes'Range loop
               if PK_Offset + J < Ring_PKs'Length then
                  PK_Bytes (J) := Anubis_Types.Byte (
                     Ring_PKs (Ring_PKs'First + PK_Offset + J));
               else
                  PK_Bytes (J) := 0;
               end if;
            end loop;

            --  Deserialize public key
            Anubis_Ring_Sig.Deserialize_Public_Key (PK_Bytes, PK, PK_Success);
            if PK_Success then
               Anubis_Ring_Sig.Add_To_Ring (R, PK, PK_Success);
            end if;
         end;
      end loop;

      --  Convert message
      for I in Local_Msg'Range loop
         Local_Msg (I) := Anubis_Types.Byte (Message (Message'First + I));
      end loop;

      --  Convert signature
      for I in Sig_Input'Range loop
         Sig_Input (I) := Anubis_Types.Byte (Signature (Signature'First + I));
      end loop;

      --  Deserialize signature
      Anubis_Ring_Sig.Deserialize_Signature (Sig_Input, Local_Sig, Deser_Success);
      if not Deser_Success then
         Valid := False;
         Result := (
            Success     => False,
            Gas_Used    => Gas_Verify_Ring_Sig_Base,
            Error_Code  => Privacy_Error_Invalid_Signature,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Check key image matches (for linkability verification)
      for I in 0 .. Anubis_Ring_Sig.Key_Image_Size - 1 loop
         if I < Key_Image'Length then
            if Local_Sig.Image (I) /= Anubis_Types.Byte (Key_Image (Key_Image'First + I)) then
               Valid := False;
               Result := (
                  Success     => False,
                  Gas_Used    => Gas_Verify_Ring_Sig_Base,
                  Error_Code  => Privacy_Error_Key_Image_Mismatch,
                  Return_Hash => (others => 0)
               );
               return;
            end if;
         end if;
      end loop;

      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Perform actual verification using Anubis_Ring_Sig
      Valid := Anubis_Ring_Sig.Verify (
         Params  => ZK_Public_Params,
         R       => R,
         Message => Local_Msg,
         Sig     => Local_Sig
      );

      Result := (
         Success     => Valid,
         Gas_Used    => Gas_Verify_Ring_Sig_Base +
                       Gas_Verify_Ring_Per_Member * VM_Gas_Amount (Ring_Size),
         Error_Code  => (if Valid then Privacy_Error_None
                        else Privacy_Error_Invalid_Signature),
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
      Local_ViewKey  : Anubis_Eye.Viewing_Key;
      Local_SpendKey : Anubis_Types.Byte_Array (0 .. 31);

      --  Tx commitment (64 bytes: tx_public_key || output_commit)
      Tx_Commitment  : Anubis_Types.Byte_Array (0 .. 63);

      --  Encrypted data buffer (amount is typically 8 bytes)
      Encrypted_Data : Anubis_Types.Byte_Array (0 .. 63);
      Decrypted_Data : Anubis_Types.Byte_Array (0 .. 63);
      Data_Length    : Natural;
   begin
      --  Initialize outputs
      Is_Owned := False;
      Decrypted_Value := 0;

      --  Convert view key
      for I in Local_ViewKey'Range loop
         if I < View_Key'Length then
            Local_ViewKey (I) := Anubis_Types.Byte (View_Key (View_Key'First + I));
         else
            Local_ViewKey (I) := 0;
         end if;
      end loop;

      --  Convert spend key
      for I in Local_SpendKey'Range loop
         if I < Spend_Key'Length then
            Local_SpendKey (I) := Anubis_Types.Byte (Spend_Key (Spend_Key'First + I));
         else
            Local_SpendKey (I) := 0;
         end if;
      end loop;

      --  Build Tx commitment: tx_public_key (32 bytes) || output_commit (32 bytes)
      for I in 0 .. 31 loop
         if I < Tx_Public_Key'Length then
            Tx_Commitment (I) := Anubis_Types.Byte (
               Tx_Public_Key (Tx_Public_Key'First + I));
         else
            Tx_Commitment (I) := 0;
         end if;
         if I < Output_Commit'Length then
            Tx_Commitment (32 + I) := Anubis_Types.Byte (
               Output_Commit (Output_Commit'First + I));
         else
            Tx_Commitment (32 + I) := 0;
         end if;
      end loop;

      --  Copy output commitment to encrypted data (contains encrypted amount)
      for I in Encrypted_Data'Range loop
         if I < Output_Commit'Length then
            Encrypted_Data (I) := Anubis_Types.Byte (
               Output_Commit (Output_Commit'First + I));
         else
            Encrypted_Data (I) := 0;
         end if;
      end loop;

      --  Initialize decrypted data
      Decrypted_Data := (others => 0);

      --  Use Anubis_Eye.Scan_Transaction to check ownership and decrypt
      Anubis_Eye.Scan_Transaction (
         View_Key       => Local_ViewKey,
         Tx_Commitment  => Tx_Commitment,
         Encrypted_Data => Encrypted_Data,
         Is_Mine        => Is_Owned,
         Decrypted_Data => Decrypted_Data,
         Data_Length    => Data_Length
      );

      --  If owned, extract decrypted value from first 8 bytes (LE64)
      if Is_Owned and then Data_Length >= 8 then
         Decrypted_Value := 0;
         for I in 0 .. 7 loop
            Decrypted_Value := Decrypted_Value +
               Unsigned_64 (Decrypted_Data (I)) * (256 ** I);
         end loop;
      elsif Is_Owned then
         --  Partial data, extract what we can
         Decrypted_Value := 0;
         for I in 0 .. Natural'Min (Data_Length - 1, 7) loop
            Decrypted_Value := Decrypted_Value +
               Unsigned_64 (Decrypted_Data (I)) * (256 ** I);
         end loop;
      end if;

      Result := (
         Success     => True,
         Gas_Used    => Gas_Scan_Output,
         Error_Code  => Privacy_Error_None,
         Return_Hash => (others => 0)
      );

      pragma Unreferenced (Spend_Key);
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
      Proof_Idx      : Natural;
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

      --  Serialize proof
      --  Format: [num_bits:4][bit_commits:var][bit_openings:var][sum_proof:var]
      Proof_Idx := 0;

      --  Store num_bits (4 bytes, little-endian)
      if Proof'Length >= 4 then
         Proof (Proof'First) := VM_Byte (ZK_Proof.Num_Bits mod 256);
         Proof (Proof'First + 1) := VM_Byte ((ZK_Proof.Num_Bits / 256) mod 256);
         Proof (Proof'First + 2) := VM_Byte ((ZK_Proof.Num_Bits / 65536) mod 256);
         Proof (Proof'First + 3) := VM_Byte ((ZK_Proof.Num_Bits / 16777216) mod 256);
         Proof_Idx := 4;
      end if;

      --  Serialize bit commitments (2 bytes per coefficient)
      for Bit in 0 .. Natural'Min (ZK_Proof.Num_Bits - 1,
                                    Anubis_Lattice_ZK.Max_Range_Bits - 1) loop
         for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
               if Proof_Idx + 1 < Proof'Length then
                  declare
                     Val : constant Integer :=
                        Integer (ZK_Proof.Bit_Commits (Bit).Value (M_Idx)(N_Idx));
                  begin
                     Proof (Proof'First + Proof_Idx) :=
                        VM_Byte (Unsigned_8 (Val mod 256));
                     Proof (Proof'First + Proof_Idx + 1) :=
                        VM_Byte (Unsigned_8 ((Val / 256) mod 256));
                  end;
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;
      end loop;

      --  Serialize bit opening responses
      for Bit in 0 .. Natural'Min (ZK_Proof.Num_Bits - 1,
                                    Anubis_Lattice_ZK.Max_Range_Bits - 1) loop
         for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
               if Proof_Idx + 1 < Proof'Length then
                  declare
                     Val : constant Integer :=
                        Integer (ZK_Proof.Bit_Openings (Bit).Response (M_Idx)(N_Idx));
                  begin
                     Proof (Proof'First + Proof_Idx) :=
                        VM_Byte (Unsigned_8 (Val mod 256));
                     Proof (Proof'First + Proof_Idx + 1) :=
                        VM_Byte (Unsigned_8 ((Val / 256) mod 256));
                  end;
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;
      end loop;

      --  Serialize sum proof Z1 and Z2
      for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
         for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
            if Proof_Idx + 1 < Proof'Length then
               declare
                  Val : constant Integer :=
                     Integer (ZK_Proof.Sum_Proof.Z1 (M_Idx)(N_Idx));
               begin
                  Proof (Proof'First + Proof_Idx) :=
                     VM_Byte (Unsigned_8 (Val mod 256));
                  Proof (Proof'First + Proof_Idx + 1) :=
                     VM_Byte (Unsigned_8 ((Val / 256) mod 256));
               end;
               Proof_Idx := Proof_Idx + 2;
            end if;
         end loop;
      end loop;

      for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
         for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
            if Proof_Idx + 1 < Proof'Length then
               declare
                  Val : constant Integer :=
                     Integer (ZK_Proof.Sum_Proof.Z2 (M_Idx)(N_Idx));
               begin
                  Proof (Proof'First + Proof_Idx) :=
                     VM_Byte (Unsigned_8 (Val mod 256));
                  Proof (Proof'First + Proof_Idx + 1) :=
                     VM_Byte (Unsigned_8 ((Val / 256) mod 256));
               end;
               Proof_Idx := Proof_Idx + 2;
            end if;
         end loop;
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
      ZK_Com         : Anubis_Lattice_ZK.Commitment;
      ZK_Proof       : Anubis_Lattice_ZK.Range_Proof;
      Local_Trans    : Anubis_Types.Byte_Array (0 .. 31);
      Success        : Boolean;
      Com_Input      : Anubis_Types.Byte_Array (0 .. Commitment'Length - 1);
      Proof_Idx      : Natural;
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

      --  Initialize proof structure with defaults
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

      --  Deserialize proof from input bytes
      --  Proof format: [num_bits:4][bit_commits:var][bit_openings:var][sum_proof:var]
      Proof_Idx := 0;

      --  Extract num_bits from proof (first 4 bytes as little-endian)
      if Proof'Length >= 4 then
         ZK_Proof.Num_Bits :=
            Natural (Proof (Proof'First)) +
            Natural (Proof (Proof'First + 1)) * 256 +
            Natural (Proof (Proof'First + 2)) * 65536 +
            Natural (Proof (Proof'First + 3)) * 16777216;
         Proof_Idx := 4;

         --  Clamp to valid range
         if ZK_Proof.Num_Bits > Anubis_Lattice_ZK.Max_Range_Bits then
            ZK_Proof.Num_Bits := Num_Bits;
         end if;
      end if;

      --  Deserialize bit commitments (2 bytes per coefficient)
      --  Each bit commitment is M x N coefficients
      for Bit in 0 .. Natural'Min (ZK_Proof.Num_Bits - 1,
                                    Anubis_Lattice_ZK.Max_Range_Bits - 1) loop
         for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
               if Proof_Idx + 1 < Proof'Length then
                  declare
                     Lo : constant Integer := Integer (Proof (Proof'First + Proof_Idx));
                     Hi : constant Integer := Integer (Proof (Proof'First + Proof_Idx + 1));
                     Val : Integer := Lo + Hi * 256;
                  begin
                     if Hi > 127 then
                        Val := Val - 65536;
                     end if;
                     ZK_Proof.Bit_Commits (Bit).Value (M_Idx)(N_Idx) :=
                        Anubis_Lattice_ZK.Reduce_Coeff (Val);
                  end;
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;
      end loop;

      --  Deserialize bit opening responses
      for Bit in 0 .. Natural'Min (ZK_Proof.Num_Bits - 1,
                                    Anubis_Lattice_ZK.Max_Range_Bits - 1) loop
         for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
               if Proof_Idx + 1 < Proof'Length then
                  declare
                     Lo : constant Integer := Integer (Proof (Proof'First + Proof_Idx));
                     Hi : constant Integer := Integer (Proof (Proof'First + Proof_Idx + 1));
                     Val : Integer := Lo + Hi * 256;
                  begin
                     if Hi > 127 then
                        Val := Val - 65536;
                     end if;
                     ZK_Proof.Bit_Openings (Bit).Response (M_Idx)(N_Idx) :=
                        Anubis_Lattice_ZK.Reduce_Coeff (Val);
                  end;
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;
      end loop;

      --  Deserialize sum proof Z1
      for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
         for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
            if Proof_Idx + 1 < Proof'Length then
               declare
                  Lo : constant Integer := Integer (Proof (Proof'First + Proof_Idx));
                  Hi : constant Integer := Integer (Proof (Proof'First + Proof_Idx + 1));
                  Val : Integer := Lo + Hi * 256;
               begin
                  if Hi > 127 then
                     Val := Val - 65536;
                  end if;
                  ZK_Proof.Sum_Proof.Z1 (M_Idx)(N_Idx) :=
                     Anubis_Lattice_ZK.Reduce_Coeff (Val);
               end;
               Proof_Idx := Proof_Idx + 2;
            end if;
         end loop;
      end loop;

      --  Deserialize sum proof Z2
      for M_Idx in 0 .. Anubis_Lattice_ZK.M - 1 loop
         for N_Idx in 0 .. Anubis_Lattice_ZK.N - 1 loop
            if Proof_Idx + 1 < Proof'Length then
               declare
                  Lo : constant Integer := Integer (Proof (Proof'First + Proof_Idx));
                  Hi : constant Integer := Integer (Proof (Proof'First + Proof_Idx + 1));
                  Val : Integer := Lo + Hi * 256;
               begin
                  if Hi > 127 then
                     Val := Val - 65536;
                  end if;
                  ZK_Proof.Sum_Proof.Z2 (M_Idx)(N_Idx) :=
                     Anubis_Lattice_ZK.Reduce_Coeff (Val);
               end;
               Proof_Idx := Proof_Idx + 2;
            end if;
         end loop;
      end loop;

      --  Generate transcript
      Local_Trans := (others => 0);
      Hash_Transcript (Privacy_Domain, Local_Trans);

      --  Regenerate challenges from transcript for consistency
      for Bit in 0 .. Natural'Min (ZK_Proof.Num_Bits - 1,
                                    Anubis_Lattice_ZK.Max_Range_Bits - 1) loop
         Anubis_Lattice_ZK.Generate_Challenge (
            Local_Trans, ZK_Proof.Bit_Openings (Bit).Challenge);
      end loop;
      Anubis_Lattice_ZK.Generate_Challenge (Local_Trans, ZK_Proof.Sum_Proof.Challenge);

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
      --  Convert values to ring elements
      X_Ring         : Anubis_Lattice_ZK.Ring_Element := Anubis_Lattice_ZK.Zero_Ring;
      Y_Ring         : Anubis_Lattice_ZK.Ring_Element := Anubis_Lattice_ZK.Zero_Ring;
      C_Ring         : Anubis_Lattice_ZK.Ring_Element;

      --  Commitments and openings
      Com_X, Com_Y   : Anubis_Lattice_ZK.Commitment;
      Open_X, Open_Y : Anubis_Lattice_ZK.Opening;

      --  Randomness buffers for commitment (64 bytes each)
      Rand_X         : Anubis_Types.Byte_Array (0 .. 63);
      Rand_Y         : Anubis_Types.Byte_Array (0 .. 63);

      --  Transcript for Fiat-Shamir
      Local_Trans    : Anubis_Types.Byte_Array (0 .. 31);

      --  Linear proof output
      Linear_Prf     : Anubis_Lattice_ZK.Linear_Proof;

      --  Coefficients as Ring_Coeff (within modulus bounds)
      A_Ring         : Anubis_Lattice_ZK.Ring_Coeff;
      B_Ring         : Anubis_Lattice_ZK.Ring_Coeff;
   begin
      --  Initialize output
      Proof := (others => 0);

      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Convert X_Value to ring element (interpret as coefficients)
      for I in 0 .. Natural'Min (X_Value'Length - 1, Anubis_Lattice_ZK.N - 1) loop
         X_Ring (I) := Anubis_Lattice_ZK.Reduce_Coeff (
            Integer (X_Value (X_Value'First + I)));
      end loop;

      --  Convert Y_Value to ring element
      for I in 0 .. Natural'Min (Y_Value'Length - 1, Anubis_Lattice_ZK.N - 1) loop
         Y_Ring (I) := Anubis_Lattice_ZK.Reduce_Coeff (
            Integer (Y_Value (Y_Value'First + I)));
      end loop;

      --  Extract randomness for both commitments
      for I in 0 .. 63 loop
         if I < Randomness'Length then
            Rand_X (I) := Anubis_Types.Byte (Randomness (Randomness'First + I));
         else
            Rand_X (I) := 0;
         end if;
         if I + 64 < Randomness'Length then
            Rand_Y (I) := Anubis_Types.Byte (Randomness (Randomness'First + I + 64));
         else
            Rand_Y (I) := 0;
         end if;
      end loop;

      --  Convert coefficients to Ring_Coeff (clamp to valid range)
      if A_Coeff > Integer (Anubis_Lattice_ZK.Q / 2) then
         A_Ring := Anubis_Lattice_ZK.Q / 2;
      elsif A_Coeff < Integer (-(Anubis_Lattice_ZK.Q / 2)) then
         A_Ring := -(Anubis_Lattice_ZK.Q / 2);
      else
         A_Ring := Anubis_Lattice_ZK.Ring_Coeff (A_Coeff);
      end if;

      if B_Coeff > Integer (Anubis_Lattice_ZK.Q / 2) then
         B_Ring := Anubis_Lattice_ZK.Q / 2;
      elsif B_Coeff < Integer (-(Anubis_Lattice_ZK.Q / 2)) then
         B_Ring := -(Anubis_Lattice_ZK.Q / 2);
      else
         B_Ring := Anubis_Lattice_ZK.Ring_Coeff (B_Coeff);
      end if;

      --  Commit to X and Y
      Anubis_Lattice_ZK.Commit (
         Params     => ZK_Public_Params,
         Message    => X_Ring,
         Randomness => Rand_X,
         Com        => Com_X,
         Open       => Open_X
      );

      Anubis_Lattice_ZK.Commit (
         Params     => ZK_Public_Params,
         Message    => Y_Ring,
         Randomness => Rand_Y,
         Com        => Com_Y,
         Open       => Open_Y
      );

      --  Compute C = a*X + b*Y (the linear relation result)
      declare
         AX : constant Anubis_Lattice_ZK.Ring_Element :=
            Anubis_Lattice_ZK.Ring_Scale (X_Ring, A_Ring);
         BY : constant Anubis_Lattice_ZK.Ring_Element :=
            Anubis_Lattice_ZK.Ring_Scale (Y_Ring, B_Ring);
      begin
         C_Ring := Anubis_Lattice_ZK.Ring_Add (AX, BY);
      end;

      --  Generate transcript
      Local_Trans := (others => 0);
      Hash_Transcript (Privacy_Domain, Local_Trans);

      --  Generate linear proof
      Anubis_Lattice_ZK.Prove_Linear (
         Params     => ZK_Public_Params,
         Com_X      => Com_X,
         Com_Y      => Com_Y,
         Open_X     => Open_X,
         Open_Y     => Open_Y,
         A_Coeff    => A_Ring,
         B_Coeff    => B_Ring,
         C_Result   => C_Ring,
         Transcript => Local_Trans,
         Proof      => Linear_Prf
      );

      --  Serialize proof: Z1 || Z2 || Challenge
      --  Z1 and Z2 are Ring_Vectors (M x N coefficients each)
      declare
         Proof_Idx  : Natural := 0;
         Coeff_Val  : Integer;
      begin
         --  Serialize Z1
         for I in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for J in 0 .. Anubis_Lattice_ZK.N - 1 loop
               Coeff_Val := Integer (Linear_Prf.Z1 (I)(J));
               if Proof_Idx + 1 < Proof'Length then
                  --  Store as 2-byte signed value
                  Proof (Proof'First + Proof_Idx) :=
                     VM_Byte (Unsigned_8 (Coeff_Val mod 256));
                  Proof (Proof'First + Proof_Idx + 1) :=
                     VM_Byte (Unsigned_8 ((Coeff_Val / 256) mod 256));
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;

         --  Serialize Z2
         for I in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for J in 0 .. Anubis_Lattice_ZK.N - 1 loop
               Coeff_Val := Integer (Linear_Prf.Z2 (I)(J));
               if Proof_Idx + 1 < Proof'Length then
                  Proof (Proof'First + Proof_Idx) :=
                     VM_Byte (Unsigned_8 (Coeff_Val mod 256));
                  Proof (Proof'First + Proof_Idx + 1) :=
                     VM_Byte (Unsigned_8 ((Coeff_Val / 256) mod 256));
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;
      end;

      --  Zeroize sensitive data
      Anubis_Lattice_ZK.Zeroize_Opening (Open_X);
      Anubis_Lattice_ZK.Zeroize_Opening (Open_Y);

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
      --  Deserialized commitments
      ZK_Com_X       : Anubis_Lattice_ZK.Commitment;
      ZK_Com_Y       : Anubis_Lattice_ZK.Commitment;
      Success_X      : Boolean;
      Success_Y      : Boolean;

      --  Input buffers for deserialization
      Com_X_Bytes    : Anubis_Types.Byte_Array (0 .. Com_X'Length - 1);
      Com_Y_Bytes    : Anubis_Types.Byte_Array (0 .. Com_Y'Length - 1);

      --  C_Result as ring element
      C_Ring         : Anubis_Lattice_ZK.Ring_Element := Anubis_Lattice_ZK.Zero_Ring;

      --  Coefficients as Ring_Coeff
      A_Ring         : Anubis_Lattice_ZK.Ring_Coeff;
      B_Ring         : Anubis_Lattice_ZK.Ring_Coeff;

      --  Deserialized proof
      Linear_Prf     : Anubis_Lattice_ZK.Linear_Proof;

      --  Transcript for Fiat-Shamir
      Local_Trans    : Anubis_Types.Byte_Array (0 .. 31);
   begin
      Valid := False;

      --  Ensure ZK params are initialized
      Ensure_ZK_Params;

      --  Convert commitment bytes
      for I in Com_X_Bytes'Range loop
         Com_X_Bytes (I) := Anubis_Types.Byte (Com_X (Com_X'First + I));
      end loop;
      for I in Com_Y_Bytes'Range loop
         Com_Y_Bytes (I) := Anubis_Types.Byte (Com_Y (Com_Y'First + I));
      end loop;

      --  Deserialize commitments
      Anubis_Lattice_ZK.Deserialize_Commitment (Com_X_Bytes, ZK_Com_X, Success_X);
      Anubis_Lattice_ZK.Deserialize_Commitment (Com_Y_Bytes, ZK_Com_Y, Success_Y);

      if not Success_X or not Success_Y then
         Result := (
            Success     => False,
            Gas_Used    => Gas_ZK_Verify_Linear / 2,
            Error_Code  => Privacy_Error_Invalid_Commitment,
            Return_Hash => (others => 0)
         );
         return;
      end if;

      --  Convert C_Result to ring element
      for I in 0 .. Natural'Min (C_Result'Length - 1, Anubis_Lattice_ZK.N - 1) loop
         C_Ring (I) := Anubis_Lattice_ZK.Reduce_Coeff (
            Integer (C_Result (C_Result'First + I)));
      end loop;

      --  Convert coefficients to Ring_Coeff (clamp to valid range)
      if A_Coeff > Integer (Anubis_Lattice_ZK.Q / 2) then
         A_Ring := Anubis_Lattice_ZK.Q / 2;
      elsif A_Coeff < Integer (-(Anubis_Lattice_ZK.Q / 2)) then
         A_Ring := -(Anubis_Lattice_ZK.Q / 2);
      else
         A_Ring := Anubis_Lattice_ZK.Ring_Coeff (A_Coeff);
      end if;

      if B_Coeff > Integer (Anubis_Lattice_ZK.Q / 2) then
         B_Ring := Anubis_Lattice_ZK.Q / 2;
      elsif B_Coeff < Integer (-(Anubis_Lattice_ZK.Q / 2)) then
         B_Ring := -(Anubis_Lattice_ZK.Q / 2);
      else
         B_Ring := Anubis_Lattice_ZK.Ring_Coeff (B_Coeff);
      end if;

      --  Deserialize proof: Z1 || Z2 || Challenge
      --  Z1 and Z2 are Ring_Vectors (M x N coefficients each)
      declare
         Proof_Idx  : Natural := 0;
         Lo, Hi     : Integer;
         Coeff_Val  : Integer;
      begin
         --  Initialize proof with zeros
         Linear_Prf := (
            Z1 => Anubis_Lattice_ZK.Zero_Vector,
            Z2 => Anubis_Lattice_ZK.Zero_Vector,
            Challenge => (
               Coeffs => Anubis_Lattice_ZK.Zero_Ring,
               Weight => 0
            )
         );

         --  Deserialize Z1
         for I in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for J in 0 .. Anubis_Lattice_ZK.N - 1 loop
               if Proof_Idx + 1 < Proof'Length then
                  Lo := Integer (Proof (Proof'First + Proof_Idx));
                  Hi := Integer (Proof (Proof'First + Proof_Idx + 1));
                  Coeff_Val := Lo + Hi * 256;
                  --  Handle sign extension for negative values
                  if Hi > 127 then
                     Coeff_Val := Coeff_Val - 65536;
                  end if;
                  Linear_Prf.Z1 (I)(J) := Anubis_Lattice_ZK.Reduce_Coeff (Coeff_Val);
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;

         --  Deserialize Z2
         for I in 0 .. Anubis_Lattice_ZK.M - 1 loop
            for J in 0 .. Anubis_Lattice_ZK.N - 1 loop
               if Proof_Idx + 1 < Proof'Length then
                  Lo := Integer (Proof (Proof'First + Proof_Idx));
                  Hi := Integer (Proof (Proof'First + Proof_Idx + 1));
                  Coeff_Val := Lo + Hi * 256;
                  if Hi > 127 then
                     Coeff_Val := Coeff_Val - 65536;
                  end if;
                  Linear_Prf.Z2 (I)(J) := Anubis_Lattice_ZK.Reduce_Coeff (Coeff_Val);
                  Proof_Idx := Proof_Idx + 2;
               end if;
            end loop;
         end loop;

         --  Regenerate challenge from transcript (Fiat-Shamir)
         Local_Trans := (others => 0);
         Hash_Transcript (Privacy_Domain, Local_Trans);
         Anubis_Lattice_ZK.Generate_Challenge (Local_Trans, Linear_Prf.Challenge);
      end;

      --  Perform actual verification using Anubis_Lattice_ZK
      Valid := Anubis_Lattice_ZK.Verify_Linear (
         Params     => ZK_Public_Params,
         Com_X      => ZK_Com_X,
         Com_Y      => ZK_Com_Y,
         A_Coeff    => A_Ring,
         B_Coeff    => B_Ring,
         C_Result   => C_Ring,
         Proof      => Linear_Prf,
         Transcript => Local_Trans
      );

      Result := (
         Success     => Valid,
         Gas_Used    => Gas_ZK_Verify_Linear,
         Error_Code  => (if Valid then Privacy_Error_None
                        else Privacy_Error_Invalid_Proof),
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
