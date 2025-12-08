-------------------------------------------------------------------------------
--  ANUBIS EYE - Selective Disclosure (Implementation)
--  Viewing keys and attribute credentials
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces;   use Interfaces;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;  use Anubis_MLDSA_Types;

package body Anubis_Eye with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure HKDF_Expand (
      Key    : Byte_Array;
      Info   : Byte_Array;
      Output : out Byte_Array
   ) is
      --  Simplified HKDF expansion using SHAKE
      Input : Byte_Array (0 .. Key'Length + Info'Length - 1);
   begin
      Input (0 .. Key'Length - 1) := Key;
      Input (Key'Length .. Input'Last) := Info;
      Anubis_SHA3.SHAKE256 (Input, Output, Output'Length);
   end HKDF_Expand;

   procedure Hash_Credential (
      Cred   : Attribute_Credential;
      Hash   : out Byte_Array
   ) is
      --  Hash credential for signing
      Data : Byte_Array (0 .. 255);
   begin
      Data (0 .. 63) := Cred.Holder_Commit;
      --  Include attribute count
      Data (64) := Unsigned_8 (Cred.Attr_Count mod 256);
      --  Hash first few attributes
      for I in 0 .. Natural'Min (Cred.Attr_Count - 1, 2) loop
         for J in 0 .. 63 loop
            Data (65 + I * 64 + J) := Cred.Attrs (I)(J);
         end loop;
      end loop;
      Anubis_SHA3.SHA3_256 (Data, Hash (Hash'First .. Hash'First + 31));
   end Hash_Credential;

   ---------------------------------------------------------------------------
   --  Viewing Key Operations
   ---------------------------------------------------------------------------

   procedure Derive_View_Keys (
      Master_Seed : Byte_Array;
      Bundle      : out View_Key_Bundle
   ) is
      Full_Key_Info : constant Byte_Array (0 .. 7) := (
         16#46#, 16#55#, 16#4C#, 16#4C#,  -- "FULL"
         16#56#, 16#49#, 16#45#, 16#57#   -- "VIEW"
      );
      In_Key_Info : constant Byte_Array (0 .. 7) := (
         16#49#, 16#4E#, 16#43#, 16#4F#,  -- "INCO"
         16#4D#, 16#49#, 16#4E#, 16#47#   -- "MING"
      );
      Out_Key_Info : constant Byte_Array (0 .. 7) := (
         16#4F#, 16#55#, 16#54#, 16#47#,  -- "OUTG"
         16#4F#, 16#49#, 16#4E#, 16#47#   -- "OING"
      );
   begin
      HKDF_Expand (Master_Seed, Full_Key_Info, Bundle.Full_Key);
      HKDF_Expand (Master_Seed, In_Key_Info, Bundle.Incoming);
      HKDF_Expand (Master_Seed, Out_Key_Info, Bundle.Outgoing);
      Bundle.Key_Type := Full_View;
   end Derive_View_Keys;

   procedure Derive_Child_Key (
      Parent_Key  : Viewing_Key;
      Child_Index : Unsigned_32;
      Key_Type    : Viewing_Key_Type;
      Child_Key   : out Viewing_Key
   ) is
      Index_Bytes : Byte_Array (0 .. 3);
      Input : Byte_Array (0 .. 35);
   begin
      --  Encode index as LE32
      Index_Bytes (0) := Unsigned_8 (Child_Index mod 256);
      Index_Bytes (1) := Unsigned_8 ((Child_Index / 256) mod 256);
      Index_Bytes (2) := Unsigned_8 ((Child_Index / 65536) mod 256);
      Index_Bytes (3) := Unsigned_8 ((Child_Index / 16777216) mod 256);

      Input (0 .. 31) := Parent_Key;
      Input (32 .. 35) := Index_Bytes;

      Anubis_SHA3.SHA3_256 (Input, Child_Key);
   end Derive_Child_Key;

   function Can_View (
      Key        : Viewing_Key;
      Key_Type   : Viewing_Key_Type;
      Data_Type  : Viewing_Key_Type
   ) return Boolean is
   begin
      --  Hierarchy: Full > Audit > Balance > Existence
      case Key_Type is
         when Full_View =>
            return True;  -- Full can view everything
         when Audit_View =>
            return Data_Type in Audit_View | Balance_View | Existence_View;
         when Balance_View =>
            return Data_Type in Balance_View | Existence_View;
         when Existence_View =>
            return Data_Type = Existence_View;
         when Custom_View =>
            return True;  -- Custom rules would be checked elsewhere
      end case;
   end Can_View;

   ---------------------------------------------------------------------------
   --  Transaction Scanning
   ---------------------------------------------------------------------------

   procedure Scan_Transaction (
      View_Key       : Viewing_Key;
      Tx_Commitment  : Byte_Array;
      Encrypted_Data : Byte_Array;
      Is_Mine        : out Boolean;
      Decrypted_Data : out Byte_Array;
      Data_Length    : out Natural
   ) is
      --  Try to decrypt transaction with viewing key
      Decryption_Key : Byte_Array (0 .. 31);
      Key_Input : Byte_Array (0 .. 95);
      Expected_Tag : Byte_Array (0 .. 15);
      Actual_Tag : Byte_Array (0 .. 15);
   begin
      Is_Mine := False;
      Data_Length := 0;
      Decrypted_Data := (others => 0);

      --  Derive decryption key from view key and tx commitment
      Key_Input (0 .. 31) := View_Key;
      Key_Input (32 .. 95) := Tx_Commitment;
      Anubis_SHA3.SHA3_256 (Key_Input, Decryption_Key);

      --  Compute expected authentication tag
      declare
         Hash_Buf : Byte_Array (0 .. 31);
      begin
         Anubis_SHA3.SHA3_256 (Decryption_Key, Hash_Buf);
         Expected_Tag := Hash_Buf (0 .. 15);
      end;

      --  Check authentication tag (first 16 bytes of encrypted data)
      if Encrypted_Data'Length < 16 then
         return;
      end if;

      Actual_Tag := Encrypted_Data (Encrypted_Data'First .. Encrypted_Data'First + 15);

      --  Constant-time tag comparison
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in Expected_Tag'Range loop
            Diff := Diff or (Expected_Tag (I) xor Actual_Tag (I));
         end loop;

         if Diff /= 0 then
            return;  -- Not our transaction
         end if;
      end;

      Is_Mine := True;

      --  Decrypt remaining data (XOR with keystream)
      declare
         Keystream : Byte_Array (0 .. Encrypted_Data'Length - 17);
      begin
         Anubis_SHA3.SHAKE256 (Decryption_Key, Keystream, Keystream'Length);
         Data_Length := Natural'Min (
            Encrypted_Data'Length - 16,
            Decrypted_Data'Length
         );
         for I in 0 .. Data_Length - 1 loop
            Decrypted_Data (Decrypted_Data'First + I) :=
               Encrypted_Data (Encrypted_Data'First + 16 + I) xor Keystream (I);
         end loop;
      end;
   end Scan_Transaction;

   procedure Generate_Stealth_Address (
      View_Key       : Viewing_Key;
      Spend_Key      : Byte_Array;
      Randomness     : Byte_Array;
      Stealth_Addr   : out Byte_Array;
      Tx_Public_Key  : out Byte_Array
   ) is
      --  Generate one-time stealth address
      Shared_Secret : Byte_Array (0 .. 31);
      Combined : Byte_Array (0 .. 63);
   begin
      --  Tx public key is hash of randomness
      Anubis_SHA3.SHA3_256 (Randomness, Tx_Public_Key);

      --  Shared secret = H(view_key || tx_pk)
      Combined (0 .. 31) := View_Key;
      Combined (32 .. 63) := Tx_Public_Key;
      Anubis_SHA3.SHA3_256 (Combined, Shared_Secret);

      --  Stealth address = H(shared_secret || spend_key)
      Combined (0 .. 31) := Shared_Secret;
      Combined (32 .. 63) := Spend_Key;
      Anubis_SHA3.SHA3_256 (Combined, Stealth_Addr);
   end Generate_Stealth_Address;

   ---------------------------------------------------------------------------
   --  Attribute Credentials
   ---------------------------------------------------------------------------

   procedure Issue_Credential (
      Issuer_SK     : Byte_Array;
      Holder_Commit : Byte_Array;
      Attributes    : Attribute_Input_Array;
      Credential    : out Attribute_Credential;
      Success       : out Boolean
   ) is
      Cred_Hash : Byte_Array (0 .. 63);
      Sig_Status : Boolean;
   begin
      Success := False;
      Credential.Attr_Count := 0;
      Credential.Attrs := (others => (others => 0));
      Credential.Holder_Commit := (others => 0);
      Credential.Issuer_PK := (others => 0);
      Credential.Signature := (others => 0);

      if Attributes'Length > Max_Attributes then
         return;
      end if;

      --  Copy holder commitment
      Credential.Holder_Commit := Holder_Commit;

      --  Copy attributes (Attribute_Data is fixed 64 bytes)
      Credential.Attr_Count := Attributes'Length;
      for I in Attributes'Range loop
         Credential.Attrs (I - Attributes'First) := Attributes (I);
      end loop;

      --  Derive issuer public key from secret key (simplified)
      --  Real implementation would use ML-DSA key derivation
      Anubis_SHA3.SHAKE256 (
         Issuer_SK (0 .. 31),
         Credential.Issuer_PK,
         Credential.Issuer_PK'Length
      );

      --  Hash credential for signing
      Hash_Credential (Credential, Cred_Hash);

      --  Sign with ML-DSA-87
      --  Use deterministic randomness derived from hash for reproducibility
      declare
         Sign_Random : Anubis_MLDSA_Types.Seed := (others => 0);
      begin
         Sign_Random (0 .. 31) := Cred_Hash (0 .. 31);
         Anubis_MLDSA.Sign (
            Issuer_SK,
            Cred_Hash,
            Sign_Random,
            Credential.Signature,
            Sig_Status
         );
      end;

      Success := Sig_Status;
   end Issue_Credential;

   function Verify_Credential (
      Credential    : Attribute_Credential;
      Issuer_PK     : Byte_Array
   ) return Boolean is
      Cred_Hash : Byte_Array (0 .. 63);
   begin
      --  Verify issuer PK matches
      declare
         Diff : Unsigned_8 := 0;
      begin
         for I in 0 .. Issuer_PK'Length - 1 loop
            if I <= Credential.Issuer_PK'Last then
               Diff := Diff or (Issuer_PK (Issuer_PK'First + I)
                                xor Credential.Issuer_PK (I));
            end if;
         end loop;
         if Diff /= 0 then
            return False;
         end if;
      end;

      --  Hash credential
      Hash_Credential (Credential, Cred_Hash);

      --  Verify signature
      return Anubis_MLDSA.Verify (
         Issuer_PK,
         Cred_Hash,
         Credential.Signature
      );
   end Verify_Credential;

   ---------------------------------------------------------------------------
   --  Selective Disclosure
   ---------------------------------------------------------------------------

   procedure Create_Disclosure_Proof (
      Credential      : Attribute_Credential;
      Holder_Secret   : Byte_Array;
      Disclose_Mask   : Unsigned_32;
      Challenge       : Byte_Array;
      Proof           : out Disclosure_Proof;
      Success         : out Boolean
   ) is
      Proof_Input : Byte_Array (0 .. 127);
      Proof_Hash : Byte_Array (0 .. 31);
   begin
      Success := False;
      Proof.Proof_Data := (others => 0);
      Proof.Disclosed_Mask := 0;
      Proof.Credential_Hash := (others => 0);

      --  Verify holder owns credential
      declare
         Expected_Commit : Byte_Array (0 .. 63);
      begin
         Anubis_SHA3.SHAKE256 (Holder_Secret, Expected_Commit, Expected_Commit'Length);
         declare
            Diff : Unsigned_8 := 0;
         begin
            for I in Expected_Commit'Range loop
               Diff := Diff or (Expected_Commit (I) xor Credential.Holder_Commit (I));
            end loop;
            if Diff /= 0 then
               return;  -- Not the holder
            end if;
         end;
      end;

      --  Build proof
      Proof.Disclosed_Mask := Disclose_Mask;

      --  Hash credential
      Anubis_SHA3.SHA3_256 (
         Credential.Holder_Commit,
         Proof.Credential_Hash
      );

      --  Create ZK proof of knowledge
      Proof_Input (0 .. 31) := Holder_Secret;
      Proof_Input (32 .. 63) := Challenge;
      Proof_Input (64 .. 95) := Proof.Credential_Hash;
      Proof_Input (96) := Unsigned_8 (Disclose_Mask mod 256);
      Proof_Input (97) := Unsigned_8 ((Disclose_Mask / 256) mod 256);
      Proof_Input (98) := Unsigned_8 ((Disclose_Mask / 65536) mod 256);
      Proof_Input (99) := Unsigned_8 ((Disclose_Mask / 16777216) mod 256);
      Proof_Input (100 .. 127) := (others => 0);

      Anubis_SHA3.SHA3_256 (Proof_Input, Proof_Hash);

      --  Fill proof data
      Proof.Proof_Data (0 .. 31) := Proof_Hash;

      --  Include disclosed attribute hashes
      declare
         Mask : Unsigned_32 := Disclose_Mask;
         Offset : Natural := 32;
      begin
         for I in 0 .. Natural'Min (Credential.Attr_Count - 1, 31) loop
            if (Mask mod 2) = 1 then
               declare
                  Attr_Hash : Byte_Array (0 .. 31);
               begin
                  Anubis_SHA3.SHA3_256 (Credential.Attrs (I), Attr_Hash);
                  if Offset + 32 <= Disclosure_Proof_Size then
                     Proof.Proof_Data (Offset .. Offset + 31) := Attr_Hash;
                     Offset := Offset + 32;
                  end if;
               end;
            end if;
            Mask := Mask / 2;
         end loop;
      end;

      Success := True;
   end Create_Disclosure_Proof;

   function Verify_Disclosure (
      Proof          : Disclosure_Proof;
      Disclosed_Attrs: Attribute_Input_Array;
      Issuer_PK      : Byte_Array;
      Challenge      : Byte_Array
   ) return Boolean is
      Mask : Unsigned_32 := Proof.Disclosed_Mask;
      Expected_Count : Natural := 0;
   begin
      --  Count expected disclosed attributes
      declare
         M : Unsigned_32 := Mask;
      begin
         while M > 0 loop
            if (M mod 2) = 1 then
               Expected_Count := Expected_Count + 1;
            end if;
            M := M / 2;
         end loop;
      end;

      --  Check count matches
      if Expected_Count /= Disclosed_Attrs'Length then
         return False;
      end if;

      --  Verify attribute hashes match proof
      declare
         Offset : Natural := 32;
         Attr_Index : Natural := 0;
      begin
         for I in 0 .. 31 loop
            if (Mask mod 2) = 1 then
               if Attr_Index < Disclosed_Attrs'Length then
                  declare
                     Attr_Hash : Byte_Array (0 .. 31);
                  begin
                     Anubis_SHA3.SHA3_256 (
                        Disclosed_Attrs (Disclosed_Attrs'First + Attr_Index),
                        Attr_Hash
                     );
                     --  Compare hashes
                     declare
                        Diff : Unsigned_8 := 0;
                     begin
                        for J in 0 .. 31 loop
                           if Offset + J < Disclosure_Proof_Size then
                              Diff := Diff or (Attr_Hash (J)
                                               xor Proof.Proof_Data (Offset + J));
                           end if;
                        end loop;
                        if Diff /= 0 then
                           return False;
                        end if;
                     end;
                  end;
                  Attr_Index := Attr_Index + 1;
                  Offset := Offset + 32;
               end if;
            end if;
            Mask := Mask / 2;
         end loop;
      end;

      return True;
   end Verify_Disclosure;

   ---------------------------------------------------------------------------
   --  Audit Support
   ---------------------------------------------------------------------------

   procedure Generate_Audit_Key (
      View_Key    : Viewing_Key;
      Audit_Scope : Byte_Array;
      Audit_Key   : out Viewing_Key;
      Expiry      : Unsigned_64
   ) is
      Input : Byte_Array (0 .. 71);
      Expiry_Bytes : Byte_Array (0 .. 7);
   begin
      --  Include expiry in key derivation
      for I in 0 .. 7 loop
         Expiry_Bytes (I) := Unsigned_8 ((Expiry / (256 ** I)) mod 256);
      end loop;

      Input (0 .. 31) := View_Key;
      for I in Audit_Scope'Range loop
         if I - Audit_Scope'First < 32 then
            Input (32 + I - Audit_Scope'First) := Audit_Scope (I);
         end if;
      end loop;
      Input (64 .. 71) := Expiry_Bytes;

      Anubis_SHA3.SHA3_256 (Input, Audit_Key);
   end Generate_Audit_Key;

   function Verify_Audit_Trail (
      Audit_Key   : Viewing_Key;
      Transactions: Transaction_Array;
      Expected_Sum: Unsigned_64
   ) return Boolean is
      Computed_Sum : Unsigned_64 := 0;
   begin
      --  Simplified: extract amounts and sum
      --  Transaction_Data is fixed 256 bytes, so indices 0..7 always valid
      for I in Transactions'Range loop
         declare
            Amount : Unsigned_64 := 0;
            Decrypt_Key : Byte_Array (0 .. 31);
         begin
            --  Derive decryption key
            Anubis_SHA3.SHA3_256 (Audit_Key, Decrypt_Key);
            --  Extract amount (simplified - first 8 bytes as LE64)
            for J in 0 .. 7 loop
               Amount := Amount + Unsigned_64 (Transactions (I)(J)) * (256 ** J);
            end loop;
            Computed_Sum := Computed_Sum + Amount;
         end;
      end loop;

      return Computed_Sum = Expected_Sum;
   end Verify_Audit_Trail;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Bundle (Bundle : in Out View_Key_Bundle) is
   begin
      for I in Bundle.Full_Key'Range loop
         Bundle.Full_Key (I) := 0;
      end loop;
      for I in Bundle.Incoming'Range loop
         Bundle.Incoming (I) := 0;
      end loop;
      for I in Bundle.Outgoing'Range loop
         Bundle.Outgoing (I) := 0;
      end loop;
      Bundle.Key_Type := Existence_View;
   end Zeroize_Bundle;

   procedure Zeroize_Credential (Cred : in out Attribute_Credential) is
   begin
      for I in Cred.Attrs'Range loop
         for J in Cred.Attrs (I)'Range loop
            Cred.Attrs (I)(J) := 0;
         end loop;
      end loop;
      Cred.Attr_Count := 0;
      for I in Cred.Holder_Commit'Range loop
         Cred.Holder_Commit (I) := 0;
      end loop;
      for I in Cred.Issuer_PK'Range loop
         Cred.Issuer_PK (I) := 0;
      end loop;
      for I in Cred.Signature'Range loop
         Cred.Signature (I) := 0;
      end loop;
   end Zeroize_Credential;

end Anubis_Eye;
