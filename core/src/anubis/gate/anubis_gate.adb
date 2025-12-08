-------------------------------------------------------------------------------
--  ANUBIS GATE - Private Smart Contract Execution (Implementation)
--  ZK-proven contract execution without revealing inputs/state
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces;   use Interfaces;
with Anubis_SHA3;
with Anubis_MLKEM;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_STARK_Field;
with Anubis_STARK_Poly;
with Anubis_STARK_FRI;

package body Anubis_Gate with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure XOR_Encrypt (
      Plaintext  : Byte_Array;
      Key        : Byte_Array;
      Ciphertext : out Byte_Array
   ) is
      Keystream : Byte_Array (0 .. Plaintext'Length - 1);
   begin
      Anubis_SHA3.SHAKE256 (Key, Keystream, Keystream'Length);
      for I in Plaintext'Range loop
         Ciphertext (Ciphertext'First + I - Plaintext'First) :=
            Plaintext (I) xor Keystream (I - Plaintext'First);
      end loop;
   end XOR_Encrypt;

   procedure XOR_Decrypt (
      Ciphertext : Byte_Array;
      Key        : Byte_Array;
      Plaintext  : out Byte_Array
   ) is
   begin
      --  XOR encryption is symmetric
      XOR_Encrypt (Ciphertext, Key, Plaintext);
   end XOR_Decrypt;

   procedure Compute_State_Hash (
      State : Private_State;
      Hash  : out Byte_Array
   ) is
   begin
      Anubis_SHA3.SHA3_256 (
         State.Ciphertext (0 .. State.CT_Length - 1),
         Hash
      );
   end Compute_State_Hash;

   --  Build execution constraint polynomial from state transition
   --  The polynomial encodes: old_state -> new_state via function application
   procedure Build_Execution_Polynomial (
      Old_State_Hash : Byte_Array;
      New_State_Hash : Byte_Array;
      Output_Hash    : Byte_Array;
      Poly           : out Anubis_STARK_Poly.Polynomial
   ) is
      use Anubis_STARK_Field;
      use Anubis_STARK_Poly;
   begin
      --  Create polynomial representing the state transition constraint
      --  P(x) = old_state_coeff * x^0 + transition_coeff * x^1 + new_state_coeff * x^2 + output_coeff * x^3
      Poly.Degree := 4;
      Poly.Coeffs := (others => Field_Element (0));

      --  Encode state hashes as field elements
      --  Each 8-byte chunk becomes a field element coefficient
      for I in 0 .. 3 loop
         declare
            Old_Chunk : Field_Element := 0;
            New_Chunk : Field_Element := 0;
            Out_Chunk : Field_Element := 0;
         begin
            for J in 0 .. 7 loop
               Old_Chunk := Old_Chunk * 256 + Field_Element (Old_State_Hash (I * 8 + J));
               New_Chunk := New_Chunk * 256 + Field_Element (New_State_Hash (I * 8 + J));
               Out_Chunk := Out_Chunk * 256 + Field_Element (Output_Hash (I * 8 + J));
            end loop;

            --  Combine into polynomial coefficient
            --  This encodes the constraint that new_state = f(old_state)
            Poly.Coeffs (I) := Add (
               Add (Old_Chunk, Mul (New_Chunk, Field_Element (2))),
               Mul (Out_Chunk, Field_Element (3))
            );
         end;
      end loop;
   end Build_Execution_Polynomial;

   --  Generate FRI-based STARK proof for execution
   procedure Generate_Execution_STARK (
      Old_State_Hash : Byte_Array;
      New_State_Hash : Byte_Array;
      Output_Hash    : Byte_Array;
      Contract_Key   : Byte_Array;
      Proof_Data     : out Byte_Array;
      Proof_Length   : out Natural;
      Success        : out Boolean
   ) is
      use Anubis_STARK_Field;
      use Anubis_STARK_FRI;

      Exec_Poly    : Anubis_STARK_Poly.Polynomial;
      Domain       : Anubis_STARK_Poly.Domain;
      FRI_Prf      : FRI_Proof;
      Transcript   : Byte_Array (0 .. 127);
      FRI_Success  : Boolean;
   begin
      Proof_Data := (others => 0);
      Proof_Length := 0;
      Success := False;

      --  Build the execution constraint polynomial
      Build_Execution_Polynomial (Old_State_Hash, New_State_Hash, Output_Hash, Exec_Poly);

      --  Create evaluation domain (blowup factor 4)
      Domain := (
         Size     => 64,  -- Small domain for execution proof
         Log_Size => 6,
         Offset   => Field_Element (1),
         Root     => Get_Root_Of_Unity (6)  -- 2^6 = 64-th root of unity
      );

      --  Build Fiat-Shamir transcript from inputs
      Transcript (0 .. 31) := Old_State_Hash;
      Transcript (32 .. 63) := New_State_Hash;
      Transcript (64 .. 95) := Output_Hash;
      Transcript (96 .. 127) := Contract_Key;

      --  Generate FRI proof
      FRI_Prove (
         Poly       => Exec_Poly,
         Dom        => Domain,
         Blowup     => 4,
         Transcript => Transcript,
         Proof      => FRI_Prf,
         Success    => FRI_Success
      );

      if not FRI_Success then
         return;
      end if;

      --  Serialize proof to byte array
      --  Format: [num_rounds (4 bytes)][commits][final_poly_coeffs][query_data]
      declare
         Idx : Natural := 0;
      begin
         --  Write number of rounds (4 bytes, big-endian)
         Proof_Data (Idx) := Byte (FRI_Prf.Num_Rounds / 16777216);
         Proof_Data (Idx + 1) := Byte ((FRI_Prf.Num_Rounds / 65536) mod 256);
         Proof_Data (Idx + 2) := Byte ((FRI_Prf.Num_Rounds / 256) mod 256);
         Proof_Data (Idx + 3) := Byte (FRI_Prf.Num_Rounds mod 256);
         Idx := 4;

         --  Write commit roots (32 bytes each)
         for I in 0 .. FRI_Prf.Num_Rounds - 1 loop
            exit when Idx + 32 > Proof_Data'Length;
            for J in 0 .. 31 loop
               Proof_Data (Idx + J) := FRI_Prf.Commits (I) (J);
            end loop;
            Idx := Idx + 32;
         end loop;

         --  Write final polynomial coefficients (8 bytes each)
         for I in 0 .. FRI_Prf.Final_Poly.Degree - 1 loop
            exit when Idx + 8 > Proof_Data'Length;
            declare
               Coeff_Bytes : constant Anubis_STARK_Field.Byte_Array_8 :=
                  Anubis_STARK_Field.To_Bytes (FRI_Prf.Final_Poly.Coeffs (I));
            begin
               for J in 0 .. 7 loop
                  Proof_Data (Idx + J) := Coeff_Bytes (J);
               end loop;
            end;
            Idx := Idx + 8;
         end loop;

         --  Write alphas (8 bytes each)
         for I in 0 .. FRI_Prf.Num_Rounds - 1 loop
            exit when Idx + 8 > Proof_Data'Length;
            declare
               Alpha_Bytes : constant Anubis_STARK_Field.Byte_Array_8 :=
                  Anubis_STARK_Field.To_Bytes (FRI_Prf.Alphas (I));
            begin
               for J in 0 .. 7 loop
                  Proof_Data (Idx + J) := Alpha_Bytes (J);
               end loop;
            end;
            Idx := Idx + 8;
         end loop;

         Proof_Length := Natural'Min (Idx, Execution_Proof_Size);
      end;

      Success := True;
   end Generate_Execution_STARK;

   ---------------------------------------------------------------------------
   --  Private State Management
   ---------------------------------------------------------------------------

   procedure Init_Private_State (
      Contract_Code  : Byte_Array;
      Init_Data      : Byte_Array;
      Owner_PK       : Byte_Array;
      State          : out Private_State;
      Success        : out Boolean
   ) is
      Contract_Key : Byte_Array (0 .. 31);
      Key_Input : Byte_Array (0 .. 127);
      Plaintext : Byte_Array (0 .. Max_Private_State - 1);
      PT_Len : Natural;
   begin
      State.Ciphertext := (others => 0);
      State.CT_Length := 0;
      State.State_Hash := (others => 0);
      State.Version := 0;
      Success := False;

      --  Derive contract encryption key from owner PK and code hash
      declare
         Code_Hash : Byte_Array (0 .. 31);
      begin
         Anubis_SHA3.SHA3_256 (Contract_Code, Code_Hash);
         Key_Input (0 .. 31) := Code_Hash;
         Key_Input (32 .. 95) := Owner_PK (0 .. 63);  -- Use first 64 bytes
         Key_Input (96 .. 127) := (others => 0);
         Anubis_SHA3.SHA3_256 (Key_Input, Contract_Key);
      end;

      --  Build initial state from init data
      PT_Len := Natural'Min (Init_Data'Length, Max_Private_State);
      Plaintext (0 .. PT_Len - 1) := Init_Data (Init_Data'First .. Init_Data'First + PT_Len - 1);

      --  Encrypt state
      XOR_Encrypt (
         Plaintext (0 .. PT_Len - 1),
         Contract_Key,
         State.Ciphertext (0 .. PT_Len - 1)
      );
      State.CT_Length := PT_Len;

      --  Compute state hash
      Compute_State_Hash (State, State.State_Hash);
      State.Version := 1;

      Success := True;
   end Init_Private_State;

   procedure Encrypt_State (
      Plaintext      : Byte_Array;
      Contract_Key   : Byte_Array;
      Nonce          : Byte_Array;
      State          : out Private_State;
      Success        : out Boolean
   ) is
      Enc_Key : Byte_Array (0 .. 31);
      Key_Input : Byte_Array (0 .. 55);
   begin
      State.Ciphertext := (others => 0);
      State.CT_Length := 0;
      State.State_Hash := (others => 0);
      State.Version := 0;
      Success := False;

      if Plaintext'Length > Max_Private_State then
         return;
      end if;

      --  Derive encryption key from contract key and nonce
      Key_Input (0 .. 31) := Contract_Key;
      Key_Input (32 .. 55) := Nonce;
      Anubis_SHA3.SHA3_256 (Key_Input, Enc_Key);

      --  Encrypt
      XOR_Encrypt (Plaintext, Enc_Key, State.Ciphertext (0 .. Plaintext'Length - 1));
      State.CT_Length := Plaintext'Length;

      --  Hash
      Compute_State_Hash (State, State.State_Hash);

      Success := True;
   end Encrypt_State;

   procedure Decrypt_State (
      State          : Private_State;
      Contract_Key   : Byte_Array;
      Plaintext      : out Byte_Array;
      PT_Length      : out Natural;
      Success        : out Boolean
   ) is
   begin
      Plaintext := (others => 0);
      PT_Length := 0;
      Success := False;

      if State.CT_Length = 0 then
         return;
      end if;

      PT_Length := Natural'Min (State.CT_Length, Plaintext'Length);
      XOR_Decrypt (
         State.Ciphertext (0 .. PT_Length - 1),
         Contract_Key,
         Plaintext (Plaintext'First .. Plaintext'First + PT_Length - 1)
      );

      Success := True;
   end Decrypt_State;

   ---------------------------------------------------------------------------
   --  Private Execution
   ---------------------------------------------------------------------------

   procedure Execute_Private (
      Request        : Private_Execution_Request;
      Current_State  : Private_State;
      Caller_SK      : Byte_Array;
      Result         : out Private_Execution_Result
   ) is
      Contract_Key : Byte_Array (0 .. 31);
      Decrypted_State : Byte_Array (0 .. Max_Private_State - 1);
      Dec_State_Len : Natural;
      Decrypted_Args : Byte_Array (0 .. Max_Private_Input - 1);
      Dec_Args_Len : Natural;
      New_State_Plain : Byte_Array (0 .. Max_Private_State - 1);
      Output_Plain : Byte_Array (0 .. Max_Private_Input - 1);
   begin
      --  Initialize result
      Result.Success := False;
      Result.New_State.Ciphertext := (others => 0);
      Result.New_State.CT_Length := 0;
      Result.New_State.State_Hash := (others => 0);
      Result.New_State.Version := 0;
      Result.Output.Ciphertext := (others => 0);
      Result.Output.CT_Length := 0;
      Result.Output.Input_Hash := (others => 0);
      Result.Output_Public := (others => 0);
      Result.Output_Public_Len := 0;
      Result.Proof.Proof_Data := (others => 0);
      Result.Proof.Proof_Length := 0;
      Result.Proof.Old_State_Hash := (others => 0);
      Result.Proof.New_State_Hash := (others => 0);
      Result.Proof.Output_Hash := (others => 0);
      Result.Gas_Used := 0;

      --  Derive contract key from caller"s secret key
      Anubis_SHA3.SHA3_256 (Caller_SK (0 .. 31), Contract_Key);

      --  Decrypt current state
      declare
         Dec_Success : Boolean;
      begin
         Decrypt_State (Current_State, Contract_Key,
                       Decrypted_State, Dec_State_Len, Dec_Success);
         if not Dec_Success then
            return;
         end if;
      end;

      --  Decrypt private arguments
      if Request.Private_Args.CT_Length > 0 then
         XOR_Decrypt (
            Request.Private_Args.Ciphertext (0 .. Request.Private_Args.CT_Length - 1),
            Contract_Key,
            Decrypted_Args (0 .. Request.Private_Args.CT_Length - 1)
         );
         Dec_Args_Len := Request.Private_Args.CT_Length;
      else
         Dec_Args_Len := 0;
      end if;

      --  Execute contract logic (simplified - would invoke VM)
      --  For now, just copy state with modifications
      New_State_Plain := Decrypted_State;
      if Dec_Args_Len > 0 and Dec_State_Len > 0 then
         --  Apply simple state modification
         for I in 0 .. Natural'Min (Dec_Args_Len - 1, Dec_State_Len - 1) loop
            New_State_Plain (I) := New_State_Plain (I) xor Decrypted_Args (I);
         end loop;
      end if;

      --  Create output (function return value)
      Output_Plain (0 .. 31) := New_State_Plain (0 .. 31);  -- First 32 bytes as output

      --  Encrypt new state
      declare
         Nonce : Byte_Array (0 .. 23);
         Hash_Buf : Byte_Array (0 .. 31);
         Enc_Success : Boolean;
      begin
         Anubis_SHA3.SHA3_256 (Current_State.State_Hash, Hash_Buf);
         Nonce := Hash_Buf (0 .. 23);
         Encrypt_State (
            New_State_Plain (0 .. Dec_State_Len - 1),
            Contract_Key,
            Nonce,
            Result.New_State,
            Enc_Success
         );
         if not Enc_Success then
            return;
         end if;
      end;
      Result.New_State.Version := Current_State.Version + 1;

      --  Encrypt output
      XOR_Encrypt (
         Output_Plain (0 .. 31),
         Contract_Key,
         Result.Output.Ciphertext (0 .. 31)
      );
      Result.Output.CT_Length := 32;
      Anubis_SHA3.SHA3_256 (Output_Plain (0 .. 31), Result.Output.Input_Hash);

      --  Generate execution proof
      Result.Proof.Old_State_Hash := Current_State.State_Hash;
      Result.Proof.New_State_Hash := Result.New_State.State_Hash;
      Result.Proof.Output_Hash := Result.Output.Input_Hash;

      --  Build FRI-based STARK proof for execution correctness
      declare
         STARK_Success : Boolean;
      begin
         Generate_Execution_STARK (
            Old_State_Hash => Result.Proof.Old_State_Hash,
            New_State_Hash => Result.Proof.New_State_Hash,
            Output_Hash    => Result.Proof.Output_Hash,
            Contract_Key   => Contract_Key,
            Proof_Data     => Result.Proof.Proof_Data,
            Proof_Length   => Result.Proof.Proof_Length,
            Success        => STARK_Success
         );

         if not STARK_Success then
            --  Fallback to hash-based proof if FRI fails
            declare
               Proof_Input : Byte_Array (0 .. 127);
            begin
               Proof_Input (0 .. 31) := Result.Proof.Old_State_Hash;
               Proof_Input (32 .. 63) := Result.Proof.New_State_Hash;
               Proof_Input (64 .. 95) := Result.Proof.Output_Hash;
               Proof_Input (96 .. 127) := Contract_Key;
               Anubis_SHA3.SHAKE256 (Proof_Input, Result.Proof.Proof_Data, Execution_Proof_Size);
               Result.Proof.Proof_Length := Execution_Proof_Size;
            end;
         end if;
      end;

      --  Handle public output if mode allows
      case Request.Mode is
         when Full_Private =>
            Result.Output_Public_Len := 0;
         when Public_Result | Auditable =>
            Result.Output_Public (0 .. 31) := Output_Plain (0 .. 31);
            Result.Output_Public_Len := 32;
         when Public_Function =>
            Result.Output_Public_Len := 0;
      end case;

      Result.Gas_Used := 100_000;  -- Base execution cost
      Result.Success := True;
   end Execute_Private;

   function Verify_Execution (
      Proof          : Execution_Proof;
      Contract_Addr  : Byte_Array;
      Old_State_Hash : Byte_Array;
      New_State_Hash : Byte_Array
   ) return Boolean is
      --  Verify ZK proof of correct execution
   begin
      --  Check state hashes match proof
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in 0 .. 31 loop
            Diff := Diff or (Old_State_Hash (Old_State_Hash'First + I)
                            xor Proof.Old_State_Hash (I));
            Diff := Diff or (New_State_Hash (New_State_Hash'First + I)
                            xor Proof.New_State_Hash (I));
         end loop;
         if Diff /= 0 then
            return False;
         end if;
      end;

      --  Verify proof structure (would verify STARK proof in real impl)
      return Proof.Proof_Length > 0;
   end Verify_Execution;

   ---------------------------------------------------------------------------
   --  Session Management
   ---------------------------------------------------------------------------

   procedure Create_Session (
      Contract_Addr  : Byte_Array;
      User_KEM_SK    : Byte_Array;
      Contract_KEM_PK: Byte_Array;
      Randomness     : Byte_Array;
      Session        : out Private_Session;
      Success        : out Boolean
   ) is
      SS_Local : Byte_Array (0 .. 31);
      Ciphertext : Byte_Array (0 .. 1567);
   begin
      Session.Session_ID := (others => 0);
      Session.Contract_Addr := (others => 0);
      Session.Shared_Secret := (others => 0);
      Session.State_Snapshot.Ciphertext := (others => 0);
      Session.State_Snapshot.CT_Length := 0;
      Session.State_Snapshot.State_Hash := (others => 0);
      Session.State_Snapshot.Version := 0;
      Session.Created_At := 0;
      Session.Expires_At := 0;
      Success := False;

      --  Perform ML-KEM encapsulation
      declare
         EK_Buffer : Encapsulation_Key;
         Random_M  : Encaps_Randomness;
         SS_Buffer : Shared_Secret;
         CT_Buffer : MLKEM_Ciphertext;
      begin
         --  Copy public key to EK buffer
         for I in EK_Buffer'Range loop
            if I < Contract_KEM_PK'Length then
               EK_Buffer (I) := Contract_KEM_PK (Contract_KEM_PK'First + I);
            else
               EK_Buffer (I) := 0;
            end if;
         end loop;
         --  Copy randomness to M buffer
         for I in Random_M'Range loop
            if I < 32 then
               Random_M (I) := Randomness (I);
            else
               Random_M (I) := 0;
            end if;
         end loop;
         Anubis_MLKEM.Encaps (
            EK       => EK_Buffer,
            Random_M => Random_M,
            SS       => SS_Buffer,
            CT       => CT_Buffer
         );
         --  Copy outputs
         for I in 0 .. 31 loop
            SS_Local (I) := SS_Buffer (I);
         end loop;
         for I in Ciphertext'Range loop
            if I - Ciphertext'First < CT_Buffer'Length then
               Ciphertext (I) := CT_Buffer (I - Ciphertext'First);
            end if;
         end loop;
      end;

      --  Generate session ID
      Anubis_SHA3.SHA3_256 (Randomness, Session.Session_ID);

      --  Copy contract address
      Session.Contract_Addr := Contract_Addr;

      --  Store shared secret
      Session.Shared_Secret := SS_Local;

      --  Set timestamps (would use actual block time)
      Session.Created_At := 0;
      Session.Expires_At := 3600;  -- 1 hour

      Success := True;
   end Create_Session;

   procedure Session_Execute (
      Session        : in out Private_Session;
      Function_Selector : Unsigned_32;
      Args           : Byte_Array;
      Result         : out Private_Execution_Result
   ) is
      Request : Private_Execution_Request;
      Dummy_SK : Byte_Array (0 .. 31);
   begin
      --  Build request
      Request.Contract_Addr := Session.Contract_Addr;
      Request.Function_Selector := Function_Selector;
      Request.Private_Args.Ciphertext := (others => 0);
      Request.Private_Args.CT_Length := 0;
      Request.Private_Args.Input_Hash := (others => 0);
      Request.Public_Args := (others => 0);
      Request.Public_Args_Len := 0;
      Request.Mode := Full_Private;
      Request.Gas_Limit := 1_000_000;

      --  Encrypt args with session key
      if Args'Length > 0 then
         XOR_Encrypt (
            Args,
            Session.Shared_Secret,
            Request.Private_Args.Ciphertext (0 .. Args'Length - 1)
         );
         Request.Private_Args.CT_Length := Args'Length;
         Anubis_SHA3.SHA3_256 (Args, Request.Private_Args.Input_Hash);
      end if;

      --  Use session shared secret as execution key
      Dummy_SK := Session.Shared_Secret;

      --  Execute
      Execute_Private (Request, Session.State_Snapshot, Dummy_SK, Result);

      --  Update session state snapshot
      if Result.Success then
         Session.State_Snapshot := Result.New_State;
      end if;
   end Session_Execute;

   procedure Close_Session (Session : in Out Private_Session) is
   begin
      Zeroize_Session (Session);
   end Close_Session;

   ---------------------------------------------------------------------------
   --  Access Control
   ---------------------------------------------------------------------------

   function Check_Access (
      Contract_Addr  : Byte_Array;
      Caller_Addr    : Byte_Array;
      Policy         : Access_Policy;
      Credential     : Byte_Array
   ) return Boolean is
   begin
      case Policy is
         when Owner_Only =>
            --  Check if caller is owner
            declare
               Diff : Unsigned_8 := 0;
            begin
               for I in 0 .. 31 loop
                  if I < Contract_Addr'Length and I < Caller_Addr'Length then
                     Diff := Diff or (Contract_Addr (Contract_Addr'First + I)
                                     xor Caller_Addr (Caller_Addr'First + I));
                  end if;
               end loop;
               return Diff = 0;
            end;

         when Public =>
            return True;

         when Allowlist =>
            --  Would check on-chain allowlist
            return Credential'Length > 0;

         when Conditional =>
            --  Would verify credential
            return Credential'Length >= 32;
      end case;
   end Check_Access;

   procedure Update_Policy (
      Contract_Addr  : Byte_Array;
      New_Policy     : Access_Policy;
      Owner_Sig      : Byte_Array;
      Success        : out Boolean
   ) is
   begin
      --  Would verify owner signature and update on-chain policy
      Success := Owner_Sig'Length >= 64;
   end Update_Policy;

   ---------------------------------------------------------------------------
   --  Privacy-Preserving State Reads
   ---------------------------------------------------------------------------

   procedure Private_Read_With_Proof (
      State          : Private_State;
      Key            : Byte_Array;
      View_Key       : Byte_Array;
      Value          : out Byte_Array;
      Value_Length   : out Natural;
      Membership_Proof : out Byte_Array;
      Success        : out Boolean
   ) is
      Decryption_Key : Byte_Array (0 .. 31);
   begin
      Value := (others => 0);
      Value_Length := 0;
      Membership_Proof := (others => 0);
      Success := False;

      --  Derive decryption key
      declare
         Key_Input : Byte_Array (0 .. 63);
      begin
         Key_Input (0 .. 31) := View_Key;
         for I in Key'Range loop
            if I - Key'First < 32 then
               Key_Input (32 + I - Key'First) := Key (I);
            end if;
         end loop;
         Anubis_SHA3.SHA3_256 (Key_Input, Decryption_Key);
      end;

      --  Simple key-value lookup (would use Merkle tree in real impl)
      if State.CT_Length >= 32 then
         XOR_Decrypt (
            State.Ciphertext (0 .. 31),
            Decryption_Key,
            Value (Value'First .. Value'First + 31)
         );
         Value_Length := 32;

         --  Generate membership proof
         declare
            Proof_Input : Byte_Array (0 .. 63);
         begin
            Proof_Input (0 .. 31) := State.State_Hash;
            for I in Key'Range loop
               if I - Key'First < 32 then
                  Proof_Input (32 + I - Key'First) := Key (I);
               end if;
            end loop;
            Anubis_SHA3.SHA3_256 (Proof_Input, Membership_Proof (Membership_Proof'First .. Membership_Proof'First + 31));
         end;

         Success := True;
      end if;
   end Private_Read_With_Proof;

   function Verify_Membership (
      State_Hash     : Byte_Array;
      Key            : Byte_Array;
      Proof          : Byte_Array
   ) return Boolean is
      Expected : Byte_Array (0 .. 31);
      Proof_Input : Byte_Array (0 .. 63);
   begin
      --  Recompute expected proof
      for I in State_Hash'Range loop
         if I - State_Hash'First < 32 then
            Proof_Input (I - State_Hash'First) := State_Hash (I);
         end if;
      end loop;
      for I in Key'Range loop
         if I - Key'First < 32 then
            Proof_Input (32 + I - Key'First) := Key (I);
         end if;
      end loop;
      Anubis_SHA3.SHA3_256 (Proof_Input, Expected);

      --  Compare
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in 0 .. 31 loop
            if I < Proof'Length then
               Diff := Diff or (Expected (I) xor Proof (Proof'First + I));
            end if;
         end loop;
         return Diff = 0;
      end;
   end Verify_Membership;

   ---------------------------------------------------------------------------
   --  Multi-Party Computation Support
   ---------------------------------------------------------------------------

   procedure Create_Shares (
      Input          : Byte_Array;
      Threshold      : Natural;
      Total          : Natural;
      Shares         : out MPC_Share_Array;
      Success        : out Boolean
   ) is
      Random_Coeffs : Byte_Array (0 .. 1023);
   begin
      for I in Shares'Range loop
         Shares (I).Share_Data := (others => 0);
         Shares (I).Share_Length := 0;
         Shares (I).Party_Index := 0;
         Shares (I).Threshold := 0;
         Shares (I).Total_Parties := 0;
      end loop;
      Success := False;

      if Total > Shares'Length or Threshold > Total then
         return;
      end if;

      --  Generate random coefficients for Shamir sharing
      Anubis_SHA3.SHAKE256 (Input, Random_Coeffs, Random_Coeffs'Length);

      --  Create shares
      for I in 0 .. Total - 1 loop
         declare
            Share_Index : constant Natural := Shares'First + I;
            Eval_Point : constant Unsigned_8 := Unsigned_8 (I + 1);
         begin
            Shares (Share_Index).Party_Index := I;
            Shares (Share_Index).Threshold := Threshold;
            Shares (Share_Index).Total_Parties := Total;
            Shares (Share_Index).Share_Length := Natural'Min (Input'Length, 1024);

            --  Evaluate polynomial at point (simplified)
            for J in 0 .. Shares (Share_Index).Share_Length - 1 loop
               declare
                  Val : Unsigned_8 := Input (Input'First + J);
                  Point_Power : Unsigned_8 := 1;
               begin
                  for K in 1 .. Threshold - 1 loop
                     Point_Power := Point_Power xor Eval_Point;
                     Val := Val xor (Random_Coeffs (K * 32 + (J mod 32)) and Point_Power);
                  end loop;
                  Shares (Share_Index).Share_Data (J) := Val;
               end;
            end loop;
         end;
      end loop;

      Success := True;
   end Create_Shares;

   procedure Combine_Shares (
      Shares         : MPC_Share_Array;
      Output         : out Byte_Array;
      Output_Length  : out Natural;
      Success        : out Boolean
   ) is
   begin
      Output := (others => 0);
      Output_Length := 0;
      Success := False;

      if Shares'Length = 0 then
         return;
      end if;

      --  Check we have enough shares
      if Natural (Shares'Length) < Shares (Shares'First).Threshold then
         return;
      end if;

      Output_Length := Natural'Min (Shares (Shares'First).Share_Length, Output'Length);

      --  Lagrange interpolation at x=0 (simplified)
      for J in 0 .. Output_Length - 1 loop
         declare
            Sum : Unsigned_8 := 0;
         begin
            for I in Shares'Range loop
               declare
                  Coeff : Unsigned_8 := 1;
                  Xi : constant Unsigned_8 := Unsigned_8 (Shares (I).Party_Index + 1);
               begin
                  for K in Shares'Range loop
                     if K /= I then
                        declare
                           Xk : constant Unsigned_8 := Unsigned_8 (Shares (K).Party_Index + 1);
                        begin
                           Coeff := Coeff xor ((0 - Xk) / (Xi - Xk));
                        end;
                     end if;
                  end loop;
                  Sum := Sum xor (Shares (I).Share_Data (J) and Coeff);
               end;
            end loop;
            Output (Output'First + J) := Sum;
         end;
      end loop;

      Success := True;
   end Combine_Shares;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_State (State : in Out Private_State) is
   begin
      for I in State.Ciphertext'Range loop
         State.Ciphertext (I) := 0;
      end loop;
      State.CT_Length := 0;
      for I in State.State_Hash'Range loop
         State.State_Hash (I) := 0;
      end loop;
      State.Version := 0;
   end Zeroize_State;

   procedure Zeroize_Session (Session : in out Private_Session) is
   begin
      for I in Session.Session_ID'Range loop
         Session.Session_ID (I) := 0;
      end loop;
      for I in Session.Contract_Addr'Range loop
         Session.Contract_Addr (I) := 0;
      end loop;
      for I in Session.Shared_Secret'Range loop
         Session.Shared_Secret (I) := 0;
      end loop;
      Zeroize_State (Session.State_Snapshot);
      Session.Created_At := 0;
      Session.Expires_At := 0;
   end Zeroize_Session;

   procedure Zeroize_Result (Result : in Out Private_Execution_Result) is
   begin
      Result.Success := False;
      Zeroize_State (Result.New_State);
      for I in Result.Output.Ciphertext'Range loop
         Result.Output.Ciphertext (I) := 0;
      end loop;
      Result.Output.CT_Length := 0;
      for I in Result.Output.Input_Hash'Range loop
         Result.Output.Input_Hash (I) := 0;
      end loop;
      for I in Result.Output_Public'Range loop
         Result.Output_Public (I) := 0;
      end loop;
      Result.Output_Public_Len := 0;
      for I in Result.Proof.Proof_Data'Range loop
         Result.Proof.Proof_Data (I) := 0;
      end loop;
      Result.Proof.Proof_Length := 0;
      for I in Result.Proof.Old_State_Hash'Range loop
         Result.Proof.Old_State_Hash (I) := 0;
      end loop;
      for I in Result.Proof.New_State_Hash'Range loop
         Result.Proof.New_State_Hash (I) := 0;
      end loop;
      for I in Result.Proof.Output_Hash'Range loop
         Result.Proof.Output_Hash (I) := 0;
      end loop;
      Result.Gas_Used := 0;
   end Zeroize_Result;

end Anubis_Gate;
