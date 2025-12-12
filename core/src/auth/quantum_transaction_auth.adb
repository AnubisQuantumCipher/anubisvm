pragma SPARK_Mode (On);

with Anubis_MLDSA;
with Anubis_SHA3;
with Anubis_Types;
with Interfaces; use Interfaces;

package body Quantum_Transaction_Auth with
   SPARK_Mode => On
is

   --  Use Anubis_Types for compatibility with SHA3 and MLDSA
   subtype Byte_Array is Anubis_Types.Byte_Array;
   subtype Local_Byte is Anubis_Types.Byte;

   --  Conversion helper: Aegis_VM_Types.Byte to Anubis_Types.Byte
   function To_Local (B : Aegis_VM_Types.Byte) return Local_Byte is
      (Local_Byte (B)) with Inline;

   --  Conversion helper: Anubis_Types.Byte to Aegis_VM_Types.Byte
   function To_VM (B : Local_Byte) return Aegis_VM_Types.Byte is
      (Aegis_VM_Types.Byte (B)) with Inline;

   ---------------------------------------------------------------------------
   --  Domain Separator Constants
   ---------------------------------------------------------------------------

   --  Domain separators for different contexts
   Domain_Tx      : constant String := "aegis-v1-tx-mldsa87";
   Domain_Address : constant String := "aegis-v1-addr-mldsa87";

   ---------------------------------------------------------------------------
   --  Helper: Encode U256 to Bytes (Big-Endian)
   ---------------------------------------------------------------------------

   procedure U256_To_Bytes (
      Value : in     U256;
      Bytes : out    Byte_Array
   ) with
      Pre => Bytes'Length >= 32 and Bytes'First = 0
   is
   begin
      for I in 0 .. 3 loop
         declare
            Limb : constant Unsigned_64 := Value.Limbs (3 - I);
         begin
            for J in 0 .. 7 loop
               Bytes (I * 8 + J) := Local_Byte (Shift_Right (Limb, (7 - J) * 8) and 16#FF#);
            end loop;
         end;
      end loop;
   end U256_To_Bytes;

   ---------------------------------------------------------------------------
   --  Derive Sender Address
   ---------------------------------------------------------------------------

   procedure Derive_Sender_Address (
      Public_Key : in     Anubis_MLDSA_Types.Public_Key;
      Sender     : out    Contract_Address
   ) is
      --  Hash input: domain_separator || public_key
      Hash_Input_Size : constant Natural := Domain_Address'Length + Public_Key'Length;
      Hash_Input : Byte_Array (0 .. Hash_Input_Size - 1);
      Hash_Output : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Copy domain separator
      for I in Domain_Address'Range loop
         Hash_Input (I - Domain_Address'First) := Local_Byte (Character'Pos (Domain_Address (I)));
      end loop;

      --  Copy public key (already Anubis_Types.Byte)
      for I in Public_Key'Range loop
         Hash_Input (Domain_Address'Length + I) := Public_Key (I);
      end loop;

      --  Compute SHA3-256 hash
      Anubis_SHA3.SHA3_256 (Hash_Input, Hash_Output);

      --  Extract first 32 bytes as sender address (convert to VM bytes)
      for I in Sender'Range loop
         Sender (I) := To_VM (Hash_Output (I));
      end loop;
   end Derive_Sender_Address;

   ---------------------------------------------------------------------------
   --  Compute Transaction Hash
   ---------------------------------------------------------------------------

   procedure Compute_Tx_Hash (
      Tx   : in     Signed_Transaction;
      Hash : out    Hash256
   ) is
      --  Maximum hash input size
      --  domain (20) + nonce (32) + chain_id (32) + to (32) + entry (64) + gas (8) + value (32) + args (8K)
      Max_Hash_Input : constant := 8500;
      Hash_Input : Byte_Array (0 .. Max_Hash_Input - 1) := (others => 0);
      Hash_Pos : Natural := 0;
      Nonce_Bytes : Byte_Array (0 .. 31);
      Chain_Bytes : Byte_Array (0 .. 31);
      Value_Bytes : Byte_Array (0 .. 31);
      Hash_Output : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Copy domain separator
      for I in Domain_Tx'Range loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := Local_Byte (Character'Pos (Domain_Tx (I)));
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Copy nonce (32 bytes)
      U256_To_Bytes (Tx.Nonce, Nonce_Bytes);
      for I in Nonce_Bytes'Range loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := Nonce_Bytes (I);
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Copy chain ID (32 bytes)
      U256_To_Bytes (Tx.Chain_ID, Chain_Bytes);
      for I in Chain_Bytes'Range loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := Chain_Bytes (I);
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Copy 'to' address (32 bytes) - convert from VM bytes
      for I in Tx.To'Range loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := To_Local (Tx.To (I));
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Copy entry point name
      for I in 1 .. Tx.Entry_Len loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := Local_Byte (Character'Pos (Tx.Entry_Point (I)));
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Copy gas limit (8 bytes)
      for I in reverse 0 .. 7 loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := Local_Byte (Shift_Right (Unsigned_64 (Tx.Gas_Limit), I * 8) and 16#FF#);
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Copy value (32 bytes)
      U256_To_Bytes (Tx.Value, Value_Bytes);
      for I in Value_Bytes'Range loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := Value_Bytes (I);
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Copy args - convert from VM bytes
      for I in 0 .. Tx.Args_Size - 1 loop
         if Hash_Pos < Max_Hash_Input then
            Hash_Input (Hash_Pos) := To_Local (Tx.Args (Args_Index (I)));
            Hash_Pos := Hash_Pos + 1;
         end if;
      end loop;

      --  Compute SHA3-256 hash
      Anubis_SHA3.SHA3_256 (Hash_Input (0 .. Hash_Pos - 1), Hash_Output);

      --  Copy to output - convert to VM bytes
      for I in Hash'Range loop
         Hash (I) := To_VM (Hash_Output (I));
      end loop;
   end Compute_Tx_Hash;

   ---------------------------------------------------------------------------
   --  Verify Public Key Hash
   ---------------------------------------------------------------------------

   function Verify_PubKey_Hash (
      Public_Key  : Anubis_MLDSA_Types.Public_Key;
      Hash        : Hash256
   ) return Boolean is
      Derived_Addr : Contract_Address;
   begin
      Derive_Sender_Address (Public_Key, Derived_Addr);

      --  Compare with provided hash (both are Aegis_VM_Types.Byte)
      for I in Hash'Range loop
         if Hash (I) /= Derived_Addr (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_PubKey_Hash;

   ---------------------------------------------------------------------------
   --  Verify Transaction
   ---------------------------------------------------------------------------

   procedure Verify_Transaction (
      Tx              : in     Signed_Transaction;
      Public_Key      : in     Anubis_MLDSA_Types.Public_Key;
      Expected_Nonce  : in     U256;
      Current_Chain_ID : in    U256;
      Result          : out    Verify_Result
   ) is
      Tx_Hash      : Hash256;
      Derived_From : Contract_Address;
      Msg          : Byte_Array (0 .. 31);  -- Anubis_Types.Byte_Array for MLDSA
      Sig_Valid    : Boolean;
   begin
      --  Initialize result
      Result.Valid := False;
      Result.Error_Code := Error_OK;
      Result.Error_Msg := (others => ' ');
      Result.Error_Len := 0;
      Result.Gas_Cost := Sig_Verify_Gas_Cost;

      --  1. Verify chain ID (replay protection)
      if Tx.Chain_ID.Limbs /= Current_Chain_ID.Limbs then
         Result.Error_Code := Error_Invalid_Chain_ID;
         declare
            Msg_Str : constant String := "Invalid chain ID";
         begin
            for I in Msg_Str'Range loop
               Result.Error_Msg (I) := Msg_Str (I);
            end loop;
            Result.Error_Len := Msg_Str'Length;
         end;
         return;
      end if;

      --  2. Verify nonce (replay protection)
      if Tx.Nonce.Limbs /= Expected_Nonce.Limbs then
         Result.Error_Code := Error_Invalid_Nonce;
         declare
            Msg_Str : constant String := "Invalid nonce";
         begin
            for I in Msg_Str'Range loop
               Result.Error_Msg (I) := Msg_Str (I);
            end loop;
            Result.Error_Len := Msg_Str'Length;
         end;
         return;
      end if;

      --  3. Verify public key hash matches the one in transaction
      if not Verify_PubKey_Hash (Public_Key, Tx.PubKey_Hash) then
         Result.Error_Code := Error_Invalid_Sender;
         declare
            Msg_Str : constant String := "Public key hash mismatch";
         begin
            for I in Msg_Str'Range loop
               Result.Error_Msg (I) := Msg_Str (I);
            end loop;
            Result.Error_Len := Msg_Str'Length;
         end;
         return;
      end if;

      --  4. Verify sender address is derived from public key
      Derive_Sender_Address (Public_Key, Derived_From);
      for I in Tx.From'Range loop
         if Tx.From (I) /= Derived_From (I) then
            Result.Error_Code := Error_Invalid_Sender;
            declare
               Msg_Str : constant String := "Sender address mismatch";
            begin
               for I in Msg_Str'Range loop
                  Result.Error_Msg (I) := Msg_Str (I);
               end loop;
               Result.Error_Len := Msg_Str'Length;
            end;
            return;
         end if;
      end loop;

      --  5. Compute transaction hash
      Compute_Tx_Hash (Tx, Tx_Hash);

      --  6. Verify ML-DSA-87 signature
      --  The message is the 32-byte transaction hash (convert to Anubis_Types)
      for I in Tx_Hash'Range loop
         Msg (I) := To_Local (Tx_Hash (I));
      end loop;

      Sig_Valid := Anubis_MLDSA.Verify (
         PK  => Public_Key,
         Msg => Msg,
         Sig => Tx.Signature
      );

      if not Sig_Valid then
         Result.Error_Code := Error_Invalid_Signature;
         declare
            Msg_Str : constant String := "ML-DSA-87 signature verification failed";
         begin
            for I in Msg_Str'Range loop
               Result.Error_Msg (I) := Msg_Str (I);
            end loop;
            Result.Error_Len := Msg_Str'Length;
         end;
         return;
      end if;

      --  All checks passed
      Result.Valid := True;
      Result.Error_Code := Error_OK;
   end Verify_Transaction;

   ---------------------------------------------------------------------------
   --  Sign Transaction
   ---------------------------------------------------------------------------

   procedure Sign_Transaction (
      Tx_Type      : in     Transaction_Type;
      To           : in     Contract_Address;
      Entry_Point  : in     Entry_Name;
      Entry_Len    : in     Natural;
      Args         : in     Args_Buffer;
      Args_Size    : in     Natural;
      Gas_Limit    : in     Gas_Amount;
      Value        : in     U256;
      Nonce        : in     U256;
      Chain_ID     : in     U256;
      Secret_Key   : in     Anubis_MLDSA_Types.Secret_Key;
      Public_Key   : in     Anubis_MLDSA_Types.Public_Key;
      Signed_Tx    : out    Signed_Transaction;
      Success      : out    Boolean
   ) is
      Tx_Hash      : Hash256;
      Msg          : Byte_Array (0 .. 31);  -- Anubis_Types.Byte_Array for MLDSA
      Random_Seed  : constant Anubis_MLDSA_Types.Seed := (others => 0);  -- Deterministic signing
      PK_Hash_Full : Anubis_SHA3.SHA3_256_Digest;
      PK_Hash_Input : Byte_Array (0 .. Domain_Address'Length + Public_Key'Length - 1);
   begin
      --  Initialize output
      Signed_Tx.Tx_Type := Tx_Type;
      Signed_Tx.Nonce := Nonce;
      Signed_Tx.Chain_ID := Chain_ID;
      Signed_Tx.To := To;
      Signed_Tx.Entry_Point := Entry_Point;
      Signed_Tx.Entry_Len := Entry_Len;
      Signed_Tx.Args := Args;
      Signed_Tx.Args_Size := Args_Size;
      Signed_Tx.Gas_Limit := Gas_Limit;
      Signed_Tx.Value := Value;
      Signed_Tx.Sig_Valid := False;
      Signed_Tx.Signature := (others => 0);

      --  Derive sender address from public key
      Derive_Sender_Address (Public_Key, Signed_Tx.From);

      --  Compute public key hash
      for I in Domain_Address'Range loop
         PK_Hash_Input (I - Domain_Address'First) := Local_Byte (Character'Pos (Domain_Address (I)));
      end loop;
      for I in Public_Key'Range loop
         PK_Hash_Input (Domain_Address'Length + I) := Public_Key (I);
      end loop;
      Anubis_SHA3.SHA3_256 (PK_Hash_Input, PK_Hash_Full);
      for I in Signed_Tx.PubKey_Hash'Range loop
         Signed_Tx.PubKey_Hash (I) := To_VM (PK_Hash_Full (I));
      end loop;

      --  Compute transaction hash
      Compute_Tx_Hash (Signed_Tx, Tx_Hash);

      --  Sign the hash with ML-DSA-87 (convert to Anubis_Types)
      for I in Tx_Hash'Range loop
         Msg (I) := To_Local (Tx_Hash (I));
      end loop;

      Anubis_MLDSA.Sign (
         SK      => Secret_Key,
         Msg     => Msg,
         Random  => Random_Seed,
         Sig     => Signed_Tx.Signature,
         Success => Success
      );

      if Success then
         Signed_Tx.Sig_Valid := True;
      end if;
   end Sign_Transaction;

end Quantum_Transaction_Auth;
