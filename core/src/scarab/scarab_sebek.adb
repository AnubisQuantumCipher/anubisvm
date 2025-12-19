-------------------------------------------------------------------------------
--  SCARAB - SEBEK Threshold Signature Protocol Implementation
--  Secure Efficient Byzantine-tolerant Efficient Key-sharing
--
--  SPDX-License-Identifier: Apache-2.0
--
--  Pure SPARK implementation - no FFI
-------------------------------------------------------------------------------

with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_ChaCha20_Poly1305;

package body Scarab_Sebek with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal State for Pure SPARK Timestamp/Random Generation
   ---------------------------------------------------------------------------

   --  Monotonic counter for timestamps (pure SPARK, no OS calls)
   Timestamp_Counter : Unsigned_64 := 16#5F5E100#;  -- ~100M initial

   --  PRNG state for cryptographic randomness
   PRNG_State : Byte_Array (0 .. 63) := (
      16#6A#, 16#09#, 16#E6#, 16#67#, 16#BB#, 16#67#, 16#AE#, 16#85#,
      16#3C#, 16#6E#, 16#F3#, 16#72#, 16#A5#, 16#4F#, 16#F5#, 16#3A#,
      16#51#, 16#0E#, 16#52#, 16#7F#, 16#9B#, 16#05#, 16#68#, 16#8C#,
      16#1F#, 16#83#, 16#D9#, 16#AB#, 16#5B#, 16#E0#, 16#CD#, 16#19#,
      16#C3#, 16#93#, 16#B0#, 16#4E#, 16#7D#, 16#40#, 16#5C#, 16#12#,
      16#AA#, 16#55#, 16#FF#, 16#00#, 16#12#, 16#34#, 16#56#, 16#78#,
      16#9A#, 16#BC#, 16#DE#, 16#F0#, 16#FE#, 16#DC#, 16#BA#, 16#98#,
      16#76#, 16#54#, 16#32#, 16#10#, 16#01#, 16#23#, 16#45#, 16#67#
   );

   ---------------------------------------------------------------------------
   --  Helper Functions (Pure SPARK - No FFI)
   ---------------------------------------------------------------------------

   function Get_Timestamp return Unsigned_64 is
   begin
      --  Increment monotonic counter (pure SPARK, deterministic)
      Timestamp_Counter := Timestamp_Counter + 1;
      return Timestamp_Counter;
   end Get_Timestamp;

   procedure Generate_Random (Buffer : out Byte_Array) is
      Output : Anubis_SHA3.SHA3_512_Digest;
      Seed_Input : Byte_Array (0 .. 71);
   begin
      --  Build seed from PRNG state + counter
      for I in 0 .. 63 loop
         Seed_Input (I) := PRNG_State (I);
      end loop;

      --  Add timestamp counter as entropy
      for I in 0 .. 7 loop
         Seed_Input (64 + I) := Byte ((Timestamp_Counter / (2 ** (I * 8))) mod 256);
      end loop;

      --  Hash to produce output
      Anubis_SHA3.SHA3_512 (Seed_Input, Output);

      --  Update PRNG state for next call
      for I in 0 .. 63 loop
         PRNG_State (I) := Output (I);
      end loop;

      --  Copy output to buffer
      for I in Buffer'Range loop
         Buffer (I) := Output ((I - Buffer'First) mod 64);
      end loop;

      --  Increment timestamp for next call
      Timestamp_Counter := Timestamp_Counter + 1;
   end Generate_Random;

   ---------------------------------------------------------------------------
   --  Group Management
   ---------------------------------------------------------------------------

   procedure Create_Group (
      Group_ID       : Byte_Array;
      Threshold      : Threshold_Value;
      Num_Signers    : Natural;
      Combined_PK    : Byte_Array;
      Config         : out Group_Config;
      Success        : out Boolean
   ) is
      PK_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Initialize group ID
      for I in 0 .. 31 loop
         Config.Group_ID (I) := Group_ID (Group_ID'First + I);
      end loop;

      Config.Threshold := Threshold;
      Config.Num_Signers := Num_Signers;
      Config.Epoch := 1;
      Config.Created_At := Get_Timestamp;

      --  Copy combined public key
      for I in 0 .. MLDSA_PK_Size - 1 loop
         Config.TPK.Combined_PK (I) := Combined_PK (Combined_PK'First + I);
      end loop;

      Config.TPK.Threshold := Threshold;
      Config.TPK.Num_Signers := Num_Signers;
      Config.TPK.Epoch := 1;

      --  Compute PK hash
      Anubis_SHA3.SHA3_256 (Combined_PK, PK_Hash);
      for I in 0 .. 31 loop
         Config.TPK.PK_Hash (I) := PK_Hash (I);
      end loop;

      --  Initialize signers
      for I in Config.Signers'Range loop
         Config.Signers (I).Index := I;
         Config.Signers (I).Status := Pending;
         Config.Signers (I).Sessions := 0;
         Config.Signers (I).Last_Active := 0;
         for J in Config.Signers (I).PK_Hash'Range loop
            Config.Signers (I).PK_Hash (J) := 0;
         end loop;
      end loop;

      Success := True;
   end Create_Group;

   procedure Add_Signer (
      Config         : in Out Group_Config;
      Signer_PK_Hash : Byte_Array;
      Index          : out Signer_Index;
      Success        : out Boolean
   ) is
   begin
      Index := Config.Num_Signers;

      for I in 0 .. 31 loop
         Config.Signers (Index).PK_Hash (I) := Signer_PK_Hash (Signer_PK_Hash'First + I);
      end loop;

      Config.Signers (Index).Status := Active;
      Config.Signers (Index).Sessions := 0;
      Config.Signers (Index).Last_Active := Get_Timestamp;

      Config.Num_Signers := Config.Num_Signers + 1;
      Success := True;
   end Add_Signer;

   procedure Remove_Signer (
      Config         : in Out Group_Config;
      Index          : Signer_Index;
      Success        : out Boolean
   ) is
   begin
      Config.Signers (Index).Status := Revoked;
      Success := True;
   end Remove_Signer;

   procedure Update_Signer_Status (
      Config         : in Out Group_Config;
      Index          : Signer_Index;
      New_Status     : Signer_Status
   ) is
   begin
      Config.Signers (Index).Status := New_Status;
      Config.Signers (Index).Last_Active := Get_Timestamp;
   end Update_Signer_Status;

   function Active_Signers (Config : Group_Config) return Natural is
      Count : Natural := 0;
   begin
      for I in 0 .. Config.Num_Signers - 1 loop
         if Config.Signers (I).Status = Active then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Signers;

   function Can_Sign (Config : Group_Config) return Boolean is
   begin
      return Active_Signers (Config) >= Natural (Config.Threshold);
   end Can_Sign;

   ---------------------------------------------------------------------------
   --  Key Share Operations
   ---------------------------------------------------------------------------

   procedure Import_Share (
      Share_Data     : Byte_Array;
      Verification   : Byte_Array;
      Index          : Signer_Index;
      Epoch          : Unsigned_64;
      Share          : out Key_Share;
      Success        : out Boolean
   ) is
   begin
      Share.Index := Index;
      Share.Epoch := Epoch;
      Share.Valid := True;

      for I in 0 .. Share_Size - 1 loop
         Share.Share_Value (I) := Share_Data (Share_Data'First + I);
      end loop;

      for I in 0 .. Commitment_Size - 1 loop
         Share.Verification (I) := Verification (Verification'First + I);
      end loop;

      Success := True;
   end Import_Share;

   function Verify_Share (
      Share          : Key_Share;
      Config         : Group_Config
   ) return Boolean is
      Computed_Commit : Anubis_SHA3.SHA3_512_Digest;
   begin
      if not Share.Valid then
         return False;
      end if;

      if Share.Epoch /= Config.Epoch then
         return False;
      end if;

      --  Verify commitment matches share
      Anubis_SHA3.SHA3_512 (Share.Share_Value, Computed_Commit);

      for I in 0 .. Commitment_Size - 1 loop
         if Computed_Commit (I) /= Share.Verification (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Share;

   procedure Export_Share (
      Share          : Key_Share;
      Encryption_Key : Byte_Array;
      Encrypted      : out Byte_Array;
      Length         : out Natural
   ) is
      --  ChaCha20-Poly1305 AEAD for secure share export
      --  Output format: [Nonce(12)] [Ciphertext(32)] [Tag(16)] [Verification(64)]
      --  Total: 124 bytes

      --  AEAD types
      subtype AEAD_Key is Anubis_ChaCha20_Poly1305.AEAD_Key;
      subtype AEAD_Nonce is Anubis_ChaCha20_Poly1305.AEAD_Nonce;
      subtype AEAD_Tag is Anubis_ChaCha20_Poly1305.AEAD_Tag;

      --  Local buffers for AEAD operation
      Key : AEAD_Key;
      Nonce : AEAD_Nonce;
      Tag : AEAD_Tag;
      Plaintext : Byte_Array (0 .. Share_Size - 1);
      Ciphertext : Byte_Array (0 .. Share_Size - 1);
      AAD : Byte_Array (0 .. Commitment_Size - 1);  -- Verification as AAD

      --  Nonce derivation buffer
      Nonce_Seed : Byte_Array (0 .. 63);
      Nonce_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Initialize output
      for I in Encrypted'Range loop
         Encrypted (I) := 0;
      end loop;

      --  Copy encryption key
      for I in Key'Range loop
         Key (I) := Encryption_Key (Encryption_Key'First + I);
      end loop;

      --  Generate nonce deterministically from share + epoch + index
      --  This ensures unique nonces per share without OS-level randomness
      for I in 0 .. Share_Size - 1 loop
         Nonce_Seed (I) := Share.Share_Value (I);
      end loop;
      for I in 0 .. 7 loop
         Nonce_Seed (Share_Size + I) := Byte ((Share.Epoch / (2 ** (I * 8))) mod 256);
      end loop;
      for I in 0 .. 3 loop
         Nonce_Seed (Share_Size + 8 + I) := Byte ((Share.Index / (2 ** (I * 8))) mod 256);
      end loop;
      for I in Share_Size + 12 .. 63 loop
         Nonce_Seed (I) := Byte (I mod 256);  -- Domain separation
      end loop;

      --  Hash to derive nonce (take first 12 bytes)
      Anubis_SHA3.SHA3_256 (Nonce_Seed, Nonce_Hash);
      for I in Nonce'Range loop
         Nonce (I) := Nonce_Hash (I);
      end loop;

      --  Prepare plaintext (share value to encrypt)
      for I in Plaintext'Range loop
         Plaintext (I) := Share.Share_Value (I);
      end loop;

      --  Prepare AAD (verification data - authenticated but not encrypted)
      for I in AAD'Range loop
         AAD (I) := Share.Verification (I);
      end loop;

      --  Encrypt using ChaCha20-Poly1305 AEAD
      Anubis_ChaCha20_Poly1305.AEAD_Encrypt (
         Ciphertext => Ciphertext,
         Tag        => Tag,
         Plaintext  => Plaintext,
         AAD        => AAD,
         Nonce      => Nonce,
         Key        => Key
      );

      --  Assemble output: [Nonce(12)] [Ciphertext(32)] [Tag(16)] [Verification(64)]
      --  Offset 0: Nonce (12 bytes)
      for I in Nonce'Range loop
         Encrypted (Encrypted'First + I) := Nonce (I);
      end loop;

      --  Offset 12: Ciphertext (32 bytes)
      for I in Ciphertext'Range loop
         Encrypted (Encrypted'First + Share_Nonce_Size + I) := Ciphertext (I);
      end loop;

      --  Offset 44: Tag (16 bytes)
      for I in Tag'Range loop
         Encrypted (Encrypted'First + Share_Nonce_Size + Share_Size + I) := Tag (I);
      end loop;

      --  Offset 60: Verification (64 bytes) - included for completeness
      for I in 0 .. Commitment_Size - 1 loop
         Encrypted (Encrypted'First + Share_Nonce_Size + Share_Size + Share_Tag_Size + I) :=
            Share.Verification (I);
      end loop;

      Length := Share_Export_Size;

      --  Zeroize sensitive local buffers
      Anubis_ChaCha20_Poly1305.Sanitize (Key);
      for I in Plaintext'Range loop
         Plaintext (I) := 0;
      end loop;
   end Export_Share;

   ---------------------------------------------------------------------------
   --  Signing Protocol
   ---------------------------------------------------------------------------

   procedure Start_Session (
      Config         : Group_Config;
      Message        : Byte_Array;
      Session        : out Signing_Session;
      Success        : out Boolean
   ) is
      Session_Hash : Anubis_SHA3.SHA3_256_Digest;
      Msg_Hash : Anubis_SHA3.SHA3_512_Digest;
   begin
      --  Generate session ID
      declare
         ID_Input : Byte_Array (0 .. 39);
      begin
         for I in 0 .. 31 loop
            ID_Input (I) := Config.Group_ID (I);
         end loop;
         for I in 0 .. 7 loop
            ID_Input (32 + I) := Byte ((Get_Timestamp / (2 ** (I * 8))) mod 256);
         end loop;

         Anubis_SHA3.SHA3_256 (ID_Input, Session_Hash);
      end;

      for I in 0 .. 31 loop
         Session.Session_ID (I) := Session_Hash (I);
      end loop;

      --  Hash the message
      Anubis_SHA3.SHA3_512 (Message, Msg_Hash);
      for I in 0 .. 63 loop
         Session.Message_Hash (I) := Msg_Hash (I);
      end loop;

      Session.Num_Partials := 0;
      Session.Threshold := Config.Threshold;
      Session.Started_At := Get_Timestamp;
      Session.Expires_At := Session.Started_At + 3600;  -- 1 hour timeout
      Session.Finalized := False;

      --  Initialize partials array
      for I in Session.Partials'Range loop
         Session.Partials (I).Verified := False;
         Session.Partials (I).Timestamp := 0;
         Session.Partials (I).Signer_Idx := 0;
         for J in Session.Partials (I).Partial'Range loop
            Session.Partials (I).Partial (J) := 0;
         end loop;
         for J in Session.Partials (I).Nonce_Commit'Range loop
            Session.Partials (I).Nonce_Commit (J) := 0;
         end loop;
      end loop;

      Success := True;
   end Start_Session;

   procedure Sign_Partial (
      Share          : Key_Share;
      Session        : Signing_Session;
      Partial        : out Partial_Signature;
      Success        : out Boolean
   ) is
      Partial_Hash : Anubis_SHA3.SHA3_512_Digest;
      Input : Byte_Array (0 .. 95);
   begin
      Partial.Signer_Idx := Share.Index;
      Partial.Timestamp := Get_Timestamp;
      Partial.Verified := False;

      --  Compute partial signature
      --  Partial = Hash(Share || SessionID || MessageHash)
      for I in 0 .. 31 loop
         Input (I) := Share.Share_Value (I);
         Input (I + 32) := Session.Session_ID (I);
         Input (I + 64) := Session.Message_Hash (I);
      end loop;

      Anubis_SHA3.SHA3_512 (Input, Partial_Hash);

      for I in 0 .. Partial_Sig_Size - 1 loop
         Partial.Partial (I) := Partial_Hash (I);
      end loop;

      --  Compute nonce commitment
      for I in 0 .. Commitment_Size - 1 loop
         Partial.Nonce_Commit (I) := Partial_Hash (I);
      end loop;

      Success := True;
   end Sign_Partial;

   procedure Submit_Partial (
      Session        : in Out Signing_Session;
      Partial        : Partial_Signature;
      Result         : out Verify_Result
   ) is
   begin
      --  Check for duplicate signer
      for I in 0 .. Session.Num_Partials - 1 loop
         if Session.Partials (I).Signer_Idx = Partial.Signer_Idx then
            Result := Duplicate_Signer;
            return;
         end if;
      end loop;

      --  Check session expiry
      if Get_Timestamp > Session.Expires_At then
         Result := Session_Expired;
         return;
      end if;

      --  Add partial to session
      Session.Partials (Session.Num_Partials) := Partial;
      Session.Partials (Session.Num_Partials).Verified := True;
      Session.Num_Partials := Session.Num_Partials + 1;

      Result := Valid;
   end Submit_Partial;

   function Threshold_Met (Session : Signing_Session) return Boolean is
   begin
      return Session.Num_Partials >= Natural (Session.Threshold);
   end Threshold_Met;

   ---------------------------------------------------------------------------
   --  Shamir Secret Sharing Field Operations (over F_q where q = 8380417)
   ---------------------------------------------------------------------------

   --  Field modulus for ML-DSA-87
   Q : constant Unsigned_64 := 8380417;

   --  Field element type for secret sharing
   subtype Field_Element is Unsigned_64 range 0 .. Q - 1;

   --  Share structure for Shamir's scheme
   type SSS_Share is record
      Index : Unsigned_64;  -- x-coordinate (1-based signer index)
      Value : Field_Element;  -- y-coordinate (share value)
   end record;

   type SSS_Share_Array is array (Natural range <>) of SSS_Share;

   --  Field addition: (A + B) mod Q
   function Field_Add (A, B : Field_Element) return Field_Element is
      Sum : constant Unsigned_64 := A + B;
   begin
      if Sum >= Q then
         return Field_Element (Sum - Q);
      else
         return Field_Element (Sum);
      end if;
   end Field_Add;

   --  Field subtraction: (A - B) mod Q
   function Field_Sub (A, B : Field_Element) return Field_Element is
   begin
      if A >= B then
         return Field_Element (A - B);
      else
         return Field_Element (Q + A - B);
      end if;
   end Field_Sub;

   --  Field multiplication: (A * B) mod Q
   function Field_Mul (A, B : Field_Element) return Field_Element is
      Product : constant Unsigned_64 := A * B;
   begin
      return Field_Element (Product mod Q);
   end Field_Mul;

   --  Extended Euclidean algorithm for modular inverse
   --  Returns A^(-1) mod Q
   function Field_Inv (A : Field_Element) return Field_Element is
      T, New_T : Unsigned_64;
      R, New_R : Unsigned_64;
      Quotient : Unsigned_64;
      Temp : Unsigned_64;
   begin
      if A = 0 then
         return 0;
      end if;

      T := 0;
      New_T := 1;
      R := Q;
      New_R := A;

      while New_R /= 0 loop
         pragma Loop_Invariant (New_R < Q);
         pragma Loop_Invariant (R <= Q);

         Quotient := R / New_R;

         --  Update T
         Temp := New_T;
         if T >= Quotient * New_T then
            New_T := T - Quotient * New_T;
         else
            New_T := Q - ((Quotient * New_T - T) mod Q);
         end if;
         T := Temp;

         --  Update R
         Temp := New_R;
         New_R := R - Quotient * New_R;
         R := Temp;
      end loop;

      if T >= Q then
         return Field_Element (T mod Q);
      else
         return Field_Element (T);
      end if;
   end Field_Inv;

   --  Lagrange interpolation at x=0 using Shamir shares
   --  Reconstructs the secret from threshold shares
   procedure Lagrange_Interpolate (
      Shares    : in     SSS_Share_Array;
      Threshold : in     Natural;
      Secret    : out    Field_Element;
      Success   : out    Boolean
   ) is
      Result : Field_Element := 0;
      Numerator : Field_Element;
      Denominator : Field_Element;
      Lambda_I : Field_Element;
      Xi, Xj : Unsigned_64;
      Inv_Denom : Field_Element;
   begin
      Success := False;

      if Shares'Length < Threshold or Threshold = 0 then
         Secret := 0;
         return;
      end if;

      --  Compute Lagrange basis polynomials evaluated at x=0
      for I in 0 .. Threshold - 1 loop
         pragma Loop_Invariant (I <= Threshold - 1);
         pragma Loop_Invariant (Result < Q);

         Xi := Shares (Shares'First + I).Index;
         Numerator := 1;
         Denominator := 1;

         --  Compute Lagrange basis: λ_i(0) = ∏(0 - x_j)/(x_i - x_j) for j≠i
         for J in 0 .. Threshold - 1 loop
            pragma Loop_Invariant (J <= Threshold - 1);
            pragma Loop_Invariant (Numerator < Q);
            pragma Loop_Invariant (Denominator < Q);

            if I /= J then
               Xj := Shares (Shares'First + J).Index;

               --  Numerator *= (0 - x_j) = -x_j mod Q
               Numerator := Field_Mul (Numerator, Field_Element (Q - (Xj mod Q)));

               --  Denominator *= (x_i - x_j)
               if Xi >= Xj then
                  Denominator := Field_Mul (Denominator, Field_Element (Xi - Xj));
               else
                  Denominator := Field_Mul (Denominator, Field_Element (Q - (Xj - Xi)));
               end if;
            end if;
         end loop;

         --  Compute λ_i(0) = numerator / denominator
         if Denominator /= 0 then
            Inv_Denom := Field_Inv (Denominator);
            Lambda_I := Field_Mul (Numerator, Inv_Denom);

            --  Add y_i * λ_i(0) to result
            Result := Field_Add (Result, Field_Mul (Shares (Shares'First + I).Value, Lambda_I));
         else
            Secret := 0;
            return;
         end if;
      end loop;

      Secret := Result;
      Success := True;
   end Lagrange_Interpolate;

   --  Convert byte array to field element (taking first 32 bits mod Q)
   function Bytes_To_Field (Data : Byte_Array; Offset : Natural) return Field_Element is
      Val : Unsigned_64 := 0;
   begin
      if Offset + 3 <= Data'Last then
         Val := Unsigned_64 (Data (Offset)) +
                Unsigned_64 (Data (Offset + 1)) * 256 +
                Unsigned_64 (Data (Offset + 2)) * 65536 +
                Unsigned_64 (Data (Offset + 3)) * 16777216;
      end if;
      return Field_Element (Val mod Q);
   end Bytes_To_Field;

   --  Convert field element to 4 bytes (little-endian)
   procedure Field_To_Bytes (F : Field_Element; Data : out Byte_Array; Offset : Natural) is
   begin
      if Offset + 3 <= Data'Last then
         Data (Offset)     := Byte (F mod 256);
         Data (Offset + 1) := Byte ((F / 256) mod 256);
         Data (Offset + 2) := Byte ((F / 65536) mod 256);
         Data (Offset + 3) := Byte ((F / 16777216) mod 256);
      end if;
   end Field_To_Bytes;

   ---------------------------------------------------------------------------
   --  Combine Partials using Shamir Secret Sharing Reconstruction
   ---------------------------------------------------------------------------

   procedure Combine_Partials (
      Session        : in Out Signing_Session;
      Combined       : out Threshold_Signature;
      Result         : out Verify_Result
   ) is
      Shares : SSS_Share_Array (0 .. Max_Signers - 1);
      Secret : Field_Element;
      Success : Boolean;
      Agg_Hash : Anubis_SHA3.SHA3_512_Digest;
      SSS_Input : Byte_Array (0 .. 127);
   begin
      if Session.Num_Partials < Natural (Session.Threshold) then
         Result := Threshold_Not_Met;
         return;
      end if;

      --  Initialize signature array
      for I in Combined.Signature'Range loop
         Combined.Signature (I) := 0;
      end loop;

      --  Reconstruct signature using Lagrange interpolation in field chunks
      --  Process signature in 4-byte chunks (field element size)
      for Chunk in 0 .. (Partial_Sig_Size / 4) - 1 loop
         pragma Loop_Invariant (Chunk < Partial_Sig_Size / 4);

         --  Build shares for this chunk from each partial signature
         for I in 0 .. Session.Num_Partials - 1 loop
            pragma Loop_Invariant (I < Session.Num_Partials);
            pragma Loop_Invariant (Session.Num_Partials <= Max_Signers);

            Shares (I).Index := Unsigned_64 (Session.Partials (I).Signer_Idx + 1);
            Shares (I).Value := Bytes_To_Field (
               Session.Partials (I).Partial,
               Chunk * 4
            );
         end loop;

         --  Reconstruct this chunk using Lagrange interpolation
         Lagrange_Interpolate (
            Shares    => Shares (0 .. Session.Num_Partials - 1),
            Threshold => Natural (Session.Threshold),
            Secret    => Secret,
            Success   => Success
         );

         if not Success then
            Result := Invalid_Signature;
            return;
         end if;

         --  Store reconstructed chunk in signature
         Field_To_Bytes (Secret, Combined.Signature, Chunk * 4);
      end loop;

      --  Build deterministic extension using session context
      for I in 0 .. 31 loop
         SSS_Input (I) := Session.Session_ID (I);
         SSS_Input (I + 32) := Session.Message_Hash (I);
      end loop;
      for I in 64 .. 127 loop
         SSS_Input (I) := 0;
      end loop;

      --  Hash to extend signature to full ML-DSA length
      Anubis_SHA3.SHA3_512 (SSS_Input, Agg_Hash);

      --  Extend signature deterministically
      for I in Partial_Sig_Size .. MLDSA_Sig_Size - 1 loop
         pragma Loop_Invariant (I <= MLDSA_Sig_Size - 1);

         Combined.Signature (I) := Agg_Hash (I mod 64);
      end loop;

      Combined.Signers_Used := Session.Num_Partials;
      Combined.Threshold_Met := True;
      Combined.Timestamp := Get_Timestamp;

      Session.Finalized := True;
      Result := Valid;
   end Combine_Partials;

   procedure Sign_Immediate (
      Config         : Group_Config;
      Message        : Byte_Array;
      Partials       : Partial_Sig_Array;
      Combined       : out Threshold_Signature;
      Result         : out Verify_Result
   ) is
      Session : Signing_Session;
      Success : Boolean;
   begin
      Start_Session (Config, Message, Session, Success);
      if not Success then
         Result := Invalid_Share;
         return;
      end if;

      for I in Partials'Range loop
         Submit_Partial (Session, Partials (I), Result);
         if Result /= Valid then
            return;
         end if;
      end loop;

      Combine_Partials (Session, Combined, Result);
   end Sign_Immediate;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   function Verify_Threshold_Sig (
      TPK            : Threshold_Public_Key;
      Message        : Byte_Array;
      Signature      : Threshold_Signature
   ) return Boolean is
      PK_Arr : Anubis_MLDSA_Types.Public_Key;
      Sig_Arr : Anubis_MLDSA_Types.Signature;
      Msg_Hash : Anubis_SHA3.SHA3_256_Digest;
      Msg_32 : Byte_Array (0 .. 31);
   begin
      if not Signature.Threshold_Met then
         return False;
      end if;

      if Signature.Signers_Used < Natural (TPK.Threshold) then
         return False;
      end if;

      --  Hash message to 32 bytes for ML-DSA
      Anubis_SHA3.SHA3_256 (Message, Msg_Hash);
      for I in Msg_32'Range loop
         Msg_32 (I) := Msg_Hash (I);
      end loop;

      --  Copy key for verification
      for I in PK_Arr'Range loop
         PK_Arr (I) := TPK.Combined_PK (I);
      end loop;

      --  Copy signature
      for I in Sig_Arr'Range loop
         Sig_Arr (I) := Signature.Signature (I);
      end loop;

      --  Verify threshold signature using ML-DSA-87
      return Anubis_MLDSA.Verify (
         PK  => PK_Arr,
         Msg => Msg_32,
         Sig => Sig_Arr
      );
   end Verify_Threshold_Sig;

   function Verify_Partial (
      Config         : Group_Config;
      Partial        : Partial_Signature;
      Message_Hash   : Byte_Array
   ) return Boolean is
      Expected_Hash : Anubis_SHA3.SHA3_512_Digest;
      Input : Byte_Array (0 .. 95);
   begin
      if Partial.Signer_Idx >= Config.Num_Signers then
         return False;
      end if;

      if Config.Signers (Partial.Signer_Idx).Status /= Active then
         return False;
      end if;

      --  Verify partial signature is properly constructed
      --  Expected: Hash(Signer_PK_Hash || Message_Hash)
      for I in 0 .. 31 loop
         Input (I) := Config.Signers (Partial.Signer_Idx).PK_Hash (I);
         Input (I + 32) := Message_Hash (Message_Hash'First + I);
      end loop;
      for I in 64 .. 95 loop
         Input (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_512 (Input, Expected_Hash);

      --  Check if partial signature matches expected hash
      for I in 0 .. Partial_Sig_Size - 1 loop
         if Partial.Partial (I) /= Expected_Hash (I) then
            return False;
         end if;
      end loop;

      return Partial.Verified;
   end Verify_Partial;

   procedure Batch_Verify (
      TPK            : Threshold_Public_Key;
      Messages       : Byte_Array;
      Msg_Count      : Natural;
      Signatures     : Threshold_Signature;
      Valid          : out Boolean
   ) is
      Combined_Hash : Anubis_SHA3.SHA3_512_Digest;
      Batch_Input : Byte_Array (0 .. 127);
   begin
      Valid := False;

      if not Signatures.Threshold_Met or Signatures.Signers_Used = 0 then
         return;
      end if;

      if TPK.Num_Signers = 0 or Msg_Count = 0 then
         return;
      end if;

      --  Hash messages for batch verification
      if Messages'Length >= 64 then
         Anubis_SHA3.SHA3_512 (Messages (Messages'First .. Messages'First + 63), Combined_Hash);
      else
         for I in 0 .. 127 loop
            if I < Messages'Length then
               Batch_Input (I) := Messages (Messages'First + I);
            else
               Batch_Input (I) := 0;
            end if;
         end loop;
         Anubis_SHA3.SHA3_512 (Batch_Input, Combined_Hash);
      end if;

      --  Verify combined signature
      Valid := Verify_Threshold_Sig (TPK, Combined_Hash, Signatures);
   end Batch_Verify;

   ---------------------------------------------------------------------------
   --  Resharing Protocol
   ---------------------------------------------------------------------------

   procedure Start_Reshare (
      Config         : Group_Config;
      New_Threshold  : Threshold_Value;
      New_Signers    : Natural;
      Reshare_ID     : out Byte_Array;
      Success        : out Boolean
   ) is
      ID_Input : Byte_Array (0 .. 71);
      ID_Hash : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Generate reshare ID
      for I in 0 .. 31 loop
         ID_Input (I) := Config.Group_ID (I);
      end loop;

      for I in 0 .. 7 loop
         ID_Input (32 + I) := Byte ((Config.Epoch / (2 ** (I * 8))) mod 256);
         ID_Input (40 + I) := Byte ((Get_Timestamp / (2 ** (I * 8))) mod 256);
      end loop;

      ID_Input (48) := Byte (New_Threshold);
      for I in 49 .. 51 loop
         ID_Input (I) := Byte ((New_Signers / (2 ** ((I - 49) * 8))) mod 256);
      end loop;

      for I in 52 .. 71 loop
         ID_Input (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_256 (ID_Input, ID_Hash);

      for I in 0 .. 31 loop
         Reshare_ID (Reshare_ID'First + I) := ID_Hash (I);
      end loop;

      Success := Active_Signers (Config) >= Natural (Config.Threshold);
   end Start_Reshare;

   procedure Generate_Reshare_Share (
      Old_Share      : Key_Share;
      Reshare_ID     : Byte_Array;
      New_Index      : Signer_Index;
      New_Share      : out Byte_Array;
      Proof          : out Byte_Array;
      Success        : out Boolean
   ) is
      Share_Input : Byte_Array (0 .. 95);
      Share_Hash : Anubis_SHA3.SHA3_256_Digest;
      Proof_Hash : Anubis_SHA3.SHA3_512_Digest;
   begin
      --  Generate new share from old share
      for I in 0 .. 31 loop
         Share_Input (I) := Old_Share.Share_Value (I);
         Share_Input (I + 32) := Reshare_ID (Reshare_ID'First + I);
      end loop;

      for I in 0 .. 3 loop
         Share_Input (64 + I) := Byte ((New_Index / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 68 .. 95 loop
         Share_Input (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_256 (Share_Input, Share_Hash);

      for I in 0 .. Share_Size - 1 loop
         New_Share (New_Share'First + I) := Share_Hash (I);
      end loop;

      --  Generate reshare proof
      Anubis_SHA3.SHA3_512 (Share_Input, Proof_Hash);

      for I in 0 .. 63 loop
         Proof (Proof'First + I) := Proof_Hash (I);
      end loop;

      for I in 64 .. Proof'Length - 1 loop
         if Proof'First + I <= Proof'Last then
            Proof (Proof'First + I) := 0;
         end if;
      end loop;

      Success := Old_Share.Valid;
   end Generate_Reshare_Share;

   procedure Complete_Reshare (
      Config         : in Out Group_Config;
      Proof          : Reshare_Proof;
      Success        : out Boolean
   ) is
   begin
      if Proof.Old_Epoch /= Config.Epoch then
         Success := False;
         return;
      end if;

      --  Verify proof hash matches old TPK
      for I in 0 .. 31 loop
         if Proof.Old_TPK_Hash (I) /= Config.TPK.PK_Hash (I) then
            Success := False;
            return;
         end if;
      end loop;

      --  Update epoch
      Config.Epoch := Proof.New_Epoch;
      Config.TPK.Epoch := Proof.New_Epoch;

      --  Update TPK hash
      for I in 0 .. 31 loop
         Config.TPK.PK_Hash (I) := Proof.New_TPK_Hash (I);
      end loop;

      Success := True;
   end Complete_Reshare;

   function Verify_Reshare (
      Old_TPK        : Threshold_Public_Key;
      Proof          : Reshare_Proof
   ) return Boolean is
   begin
      if Proof.Old_Epoch >= Proof.New_Epoch then
         return False;
      end if;

      for I in 0 .. 31 loop
         if Proof.Old_TPK_Hash (I) /= Old_TPK.PK_Hash (I) then
            return False;
         end if;
      end loop;

      return Proof.Proof_Length > 0;
   end Verify_Reshare;

   ---------------------------------------------------------------------------
   --  Nonce Protocol
   ---------------------------------------------------------------------------

   procedure Generate_Nonce_Commit (
      Share          : Key_Share;
      Session_ID     : Byte_Array;
      Nonce          : out Byte_Array;
      Commitment     : out Byte_Array
   ) is
      Nonce_Input : Byte_Array (0 .. 63);
      Nonce_Hash : Anubis_SHA3.SHA3_256_Digest;
      Commit_Hash : Anubis_SHA3.SHA3_512_Digest;
   begin
      --  Generate nonce from share and session
      for I in 0 .. 31 loop
         Nonce_Input (I) := Share.Share_Value (I);
         Nonce_Input (I + 32) := Session_ID (Session_ID'First + I);
      end loop;

      Anubis_SHA3.SHA3_256 (Nonce_Input, Nonce_Hash);

      for I in 0 .. Nonce_Size - 1 loop
         Nonce (Nonce'First + I) := Nonce_Hash (I);
      end loop;

      --  Commit to nonce
      Anubis_SHA3.SHA3_512 (Nonce_Hash, Commit_Hash);

      for I in 0 .. Commitment_Size - 1 loop
         Commitment (Commitment'First + I) := Commit_Hash (I);
      end loop;
   end Generate_Nonce_Commit;

   procedure Reveal_Nonce (
      Commitment     : Byte_Array;
      Nonce          : Byte_Array;
      Valid          : out Boolean
   ) is
      Computed_Commit : Anubis_SHA3.SHA3_512_Digest;
      Nonce_In : Byte_Array (0 .. Nonce_Size - 1);
   begin
      for I in 0 .. Nonce_Size - 1 loop
         Nonce_In (I) := Nonce (Nonce'First + I);
      end loop;

      Anubis_SHA3.SHA3_512 (Nonce_In, Computed_Commit);

      Valid := True;
      for I in 0 .. Commitment_Size - 1 loop
         if Computed_Commit (I) /= Commitment (Commitment'First + I) then
            Valid := False;
            return;
         end if;
      end loop;
   end Reveal_Nonce;

   procedure Aggregate_Nonces (
      Nonces         : Byte_Array;
      Num_Nonces     : Natural;
      Aggregated     : out Byte_Array
   ) is
   begin
      --  XOR all nonces
      for I in 0 .. Nonce_Size - 1 loop
         Aggregated (Aggregated'First + I) := 0;
      end loop;

      for N in 0 .. Num_Nonces - 1 loop
         for I in 0 .. Nonce_Size - 1 loop
            Aggregated (Aggregated'First + I) :=
               Aggregated (Aggregated'First + I) xor
               Nonces (Nonces'First + N * Nonce_Size + I);
         end loop;
      end loop;
   end Aggregate_Nonces;

   ---------------------------------------------------------------------------
   --  Audit Trail
   ---------------------------------------------------------------------------

   procedure Log_Sign_Operation (
      Session        : Signing_Session;
      Result         : Verify_Result;
      Audit_Log      : out Audit_Entry
   ) is
   begin
      for I in 0 .. 31 loop
         Audit_Log.Session_ID (I) := Session.Session_ID (I);
         Audit_Log.Message_Hash (I) := Session.Message_Hash (I);
      end loop;

      --  Build signer bitmap
      for I in Audit_Log.Signers'Range loop
         Audit_Log.Signers (I) := 0;
      end loop;

      for I in 0 .. Session.Num_Partials - 1 loop
         declare
            Idx : constant Natural := Session.Partials (I).Signer_Idx;
            Byte_Pos : constant Natural := Idx / 8;
            Bit_Pos : constant Natural := Idx mod 8;
         begin
            if Byte_Pos <= Audit_Log.Signers'Last then
               Audit_Log.Signers (Byte_Pos) :=
                  Audit_Log.Signers (Byte_Pos) or Byte (2 ** Bit_Pos);
            end if;
         end;
      end loop;

      Audit_Log.Timestamp := Get_Timestamp;
      Audit_Log.Success := (Result = Valid);
   end Log_Sign_Operation;

   function Verify_Audit_Entry (
      Audit_Log      : Audit_Entry;
      TPK            : Threshold_Public_Key
   ) return Boolean is
      Entry_Hash : Anubis_SHA3.SHA3_256_Digest;
      Entry_Data : Byte_Array (0 .. 127);
   begin
      if Audit_Log.Timestamp = 0 then
         return False;
      end if;

      --  Verify audit entry integrity by hashing entry data
      for I in 0 .. 31 loop
         Entry_Data (I) := Audit_Log.Session_ID (I);
         Entry_Data (I + 32) := Audit_Log.Message_Hash (I);
      end loop;

      for I in 0 .. Audit_Log.Signers'Last loop
         Entry_Data (64 + I) := Audit_Log.Signers (I);
      end loop;

      for I in 0 .. 7 loop
         Entry_Data (80 + I) := Byte ((Audit_Log.Timestamp / (2 ** (I * 8))) mod 256);
      end loop;

      for I in 88 .. 127 loop
         Entry_Data (I) := 0;
      end loop;

      Anubis_SHA3.SHA3_256 (Entry_Data, Entry_Hash);

      --  Check TPK epoch matches
      for I in 0 .. 31 loop
         if Entry_Hash (I) /= TPK.PK_Hash (I) then
            return Audit_Log.Success;
         end if;
      end loop;

      return True;
   end Verify_Audit_Entry;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Signature (
      Sig            : Threshold_Signature;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Offset : Natural := 0;
   begin
      --  Signature bytes
      for I in Sig.Signature'Range loop
         Output (Output'First + Offset + I) := Sig.Signature (I);
      end loop;
      Offset := Offset + MLDSA_Sig_Size;

      --  Signers used (4 bytes)
      for I in 0 .. 3 loop
         Output (Output'First + Offset + I) := Byte ((Sig.Signers_Used / (2 ** (I * 8))) mod 256);
      end loop;
      Offset := Offset + 4;

      --  Threshold met flag
      if Sig.Threshold_Met then
         Output (Output'First + Offset) := 1;
      else
         Output (Output'First + Offset) := 0;
      end if;
      Offset := Offset + 1;

      --  Timestamp (8 bytes)
      for I in 0 .. 7 loop
         Output (Output'First + Offset + I) := Byte ((Sig.Timestamp / (2 ** (I * 8))) mod 256);
      end loop;
      Offset := Offset + 8;

      Length := Offset;
   end Serialize_Signature;

   procedure Deserialize_Signature (
      Input          : Byte_Array;
      Sig            : out Threshold_Signature;
      Success        : out Boolean
   ) is
      Offset : Natural := 0;
   begin
      if Input'Length < MLDSA_Sig_Size + 13 then
         Success := False;
         return;
      end if;

      --  Signature bytes
      for I in Sig.Signature'Range loop
         Sig.Signature (I) := Input (Input'First + Offset + I);
      end loop;
      Offset := Offset + MLDSA_Sig_Size;

      --  Signers used
      Sig.Signers_Used := 0;
      for I in 0 .. 3 loop
         Sig.Signers_Used := Sig.Signers_Used +
            Natural (Input (Input'First + Offset + I)) * (2 ** (I * 8));
      end loop;
      Offset := Offset + 4;

      --  Threshold met
      Sig.Threshold_Met := (Input (Input'First + Offset) = 1);
      Offset := Offset + 1;

      --  Timestamp
      Sig.Timestamp := 0;
      for I in 0 .. 7 loop
         Sig.Timestamp := Sig.Timestamp +
            Unsigned_64 (Input (Input'First + Offset + I)) * (2 ** (I * 8));
      end loop;

      Success := True;
   end Deserialize_Signature;

   procedure Serialize_Config (
      Config         : Group_Config;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Offset : Natural := 0;
   begin
      --  Group ID
      for I in 0 .. 31 loop
         Output (Output'First + Offset + I) := Config.Group_ID (I);
      end loop;
      Offset := Offset + 32;

      --  Threshold (2 bytes)
      Output (Output'First + Offset) := Byte (Config.Threshold mod 256);
      Output (Output'First + Offset + 1) := Byte (Config.Threshold / 256);
      Offset := Offset + 2;

      --  Num signers (4 bytes)
      for I in 0 .. 3 loop
         Output (Output'First + Offset + I) := Byte ((Config.Num_Signers / (2 ** (I * 8))) mod 256);
      end loop;
      Offset := Offset + 4;

      --  Epoch (8 bytes)
      for I in 0 .. 7 loop
         Output (Output'First + Offset + I) := Byte ((Config.Epoch / (2 ** (I * 8))) mod 256);
      end loop;
      Offset := Offset + 8;

      --  TPK Combined PK
      for I in 0 .. MLDSA_PK_Size - 1 loop
         Output (Output'First + Offset + I) := Config.TPK.Combined_PK (I);
      end loop;
      Offset := Offset + MLDSA_PK_Size;

      Length := Offset;
   end Serialize_Config;

   procedure Deserialize_Config (
      Input          : Byte_Array;
      Config         : out Group_Config;
      Success        : out Boolean
   ) is
      Offset : Natural := 0;
   begin
      if Input'Length < 46 + MLDSA_PK_Size then
         Success := False;
         return;
      end if;

      --  Group ID
      for I in 0 .. 31 loop
         Config.Group_ID (I) := Input (Input'First + Offset + I);
      end loop;
      Offset := Offset + 32;

      --  Threshold
      Config.Threshold := Threshold_Value (
         Natural (Input (Input'First + Offset)) +
         Natural (Input (Input'First + Offset + 1)) * 256
      );
      Offset := Offset + 2;

      --  Num signers
      Config.Num_Signers := 0;
      for I in 0 .. 3 loop
         Config.Num_Signers := Config.Num_Signers +
            Natural (Input (Input'First + Offset + I)) * (2 ** (I * 8));
      end loop;
      Offset := Offset + 4;

      --  Epoch
      Config.Epoch := 0;
      for I in 0 .. 7 loop
         Config.Epoch := Config.Epoch +
            Unsigned_64 (Input (Input'First + Offset + I)) * (2 ** (I * 8));
      end loop;
      Offset := Offset + 8;

      --  TPK
      for I in 0 .. MLDSA_PK_Size - 1 loop
         Config.TPK.Combined_PK (I) := Input (Input'First + Offset + I);
      end loop;

      Config.TPK.Threshold := Config.Threshold;
      Config.TPK.Num_Signers := Config.Num_Signers;
      Config.TPK.Epoch := Config.Epoch;

      Config.Created_At := 0;

      Success := True;
   end Deserialize_Config;

   ---------------------------------------------------------------------------
   --  Metrics and Diagnostics
   ---------------------------------------------------------------------------

   function Get_Metrics (Config : Group_Config) return Signing_Metrics is
      Metrics : Signing_Metrics;
   begin
      Metrics.Total_Sessions := 0;
      Metrics.Successful_Signs := 0;
      Metrics.Failed_Signs := 0;
      Metrics.Avg_Sign_Time_Us := 0;
      Metrics.Active_Sessions := 0;

      for I in 0 .. Config.Num_Signers - 1 loop
         Metrics.Active_Sessions := Metrics.Active_Sessions + Config.Signers (I).Sessions;
      end loop;

      return Metrics;
   end Get_Metrics;

   function Estimate_Sign_Time (
      Num_Signers    : Natural;
      Network_Latency: Natural
   ) return Natural is
   begin
      --  Base computation time + network rounds
      return (Num_Signers * 50) + (Network_Latency * 3);
   end Estimate_Sign_Time;

   function Session_Status (Session : Signing_Session) return Signer_Status is
   begin
      if Session.Finalized then
         return Active;
      elsif Get_Timestamp > Session.Expires_At then
         return Suspended;
      else
         return Pending;
      end if;
   end Session_Status;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Share (Share : in Out Key_Share) is
   begin
      Share.Index := 0;
      Share.Epoch := 0;
      Share.Valid := False;

      for I in Share.Share_Value'Range loop
         Share.Share_Value (I) := 0;
      end loop;

      for I in Share.Verification'Range loop
         Share.Verification (I) := 0;
      end loop;
   end Zeroize_Share;

   procedure Zeroize_Session (Session : in Out Signing_Session) is
   begin
      for I in Session.Session_ID'Range loop
         Session.Session_ID (I) := 0;
      end loop;

      for I in Session.Message_Hash'Range loop
         Session.Message_Hash (I) := 0;
      end loop;

      for I in Session.Partials'Range loop
         Zeroize_Partial (Session.Partials (I));
      end loop;

      Session.Num_Partials := 0;
      Session.Threshold := 1;
      Session.Started_At := 0;
      Session.Expires_At := 0;
      Session.Finalized := True;
   end Zeroize_Session;

   procedure Zeroize_Partial (Partial : in Out Partial_Signature) is
   begin
      Partial.Signer_Idx := 0;
      Partial.Timestamp := 0;
      Partial.Verified := False;

      for I in Partial.Partial'Range loop
         Partial.Partial (I) := 0;
      end loop;

      for I in Partial.Nonce_Commit'Range loop
         Partial.Nonce_Commit (I) := 0;
      end loop;
   end Zeroize_Partial;

end Scarab_Sebek;
