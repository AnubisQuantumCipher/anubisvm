pragma SPARK_Mode (On);

with Anubis_MLKEM; use Anubis_MLKEM;
with Anubis_SHA3; use Anubis_SHA3;

package body Anubis_Session with
   SPARK_Mode => On
is

   --  Generate a new ML-KEM-1024 keypair for session establishment
   procedure Generate_Recipient_Keys (
      Seed_D : in  Seed;
      Seed_Z : in  Seed;
      Ctx    : out Recipient_Context
   ) is
   begin
      --  Generate ML-KEM keypair
      KeyGen (Seed_D, Seed_Z, Ctx.KEM_PK, Ctx.KEM_SK);

      --  Clear session key initially
      Ctx.Session_Key := (others => 0);
      Ctx.Established := False;
   end Generate_Recipient_Keys;

   --  Establish session as sender (encapsulation)
   procedure Establish_Sender_Session (
      Recipient_PK : in  Encapsulation_Key;
      Rand_Seed    : in  Seed;
      Context      : in  Context_Bytes;
      Result       : out Session_Result
   ) is
      SS : Shared_Secret;
   begin
      --  Initialize output
      Result.Session_Key := (others => 0);
      Result.Encapsulated_Key := (others => 0);
      Result.Success := False;

      --  Encapsulate to get shared secret and ciphertext
      Encaps (Recipient_PK, Rand_Seed, SS, Result.Encapsulated_Key);

      --  Derive session key from shared secret
      Derive_Session_Key (SS, Context, Result.Session_Key);

      --  Zeroize shared secret
      for I in SS'Range loop
         SS (I) := 0;
      end loop;

      Result.Success := True;
   end Establish_Sender_Session;

   --  Recover session as recipient (decapsulation)
   procedure Recover_Recipient_Session (
      Ctx               : in Out Recipient_Context;
      Encapsulated_Key  : in     MLKEM_Ciphertext;
      Context           : in     Context_Bytes;
      Session_Key       : out    Anubis_Session.Session_Key;
      Success           : out    Boolean
   ) is
      SS : Shared_Secret;
   begin
      Success := False;

      --  Decapsulate to recover shared secret
      Decaps (Ctx.KEM_SK, Encapsulated_Key, SS);

      --  Derive session key from shared secret
      Derive_Session_Key (SS, Context, Session_Key);

      --  Store in context
      Ctx.Session_Key := Session_Key;
      Ctx.Established := True;

      --  Zeroize shared secret
      for I in SS'Range loop
         SS (I) := 0;
      end loop;

      Success := True;
   end Recover_Recipient_Session;

   --  Derive session key from shared secret with domain separation
   procedure Derive_Session_Key (
      SS      : in  Shared_Secret;
      Context : in  Context_Bytes;
      Key     : out Session_Key
   ) is
      --  Fixed sizes
      Prefix_Len : constant := 17;  --  "anubis-session-v1"
      SS_Len     : constant := 32;  --  Shared secret size

      --  Total input size: prefix + SS + context
      Input_Len : constant Natural := Prefix_Len + SS_Len + Context'Length;

      --  Max possible input: 17 + 32 + 64 = 113
      subtype Input_Index is Natural range 0 .. Prefix_Len + SS_Len + Max_Context_Size - 1;
      Input  : Byte_Array (Input_Index) := (others => 0);
      Output : Byte_Array (0 .. Session_Key_Size - 1);
   begin
      --  Copy domain prefix at offset 0
      for I in Natural range 0 .. Prefix_Len - 1 loop
         pragma Loop_Invariant (I <= Prefix_Len - 1);
         Input (I) := Unsigned_8 (Character'Pos (Domain_Prefix (Domain_Prefix'First + I)));
      end loop;

      --  Copy shared secret at offset 17
      for I in SS'Range loop
         pragma Loop_Invariant (I in SS'Range);
         Input (Prefix_Len + I) := SS (I);
      end loop;

      --  Copy context at offset 49 (17 + 32)
      for I in Context'Range loop
         pragma Loop_Invariant (I in Context'Range);
         pragma Loop_Invariant (Prefix_Len + SS_Len + I - Context'First <= Input'Last);
         Input (Prefix_Len + SS_Len + I - Context'First) := Context (I);
      end loop;

      --  Use SHAKE256 to derive key
      SHAKE256 (Input (0 .. Input_Len - 1), Output, Session_Key_Size);

      --  Copy to output
      for I in Key'Range loop
         pragma Loop_Invariant (I in Key'Range);
         Key (I) := Output (I);
      end loop;
   end Derive_Session_Key;

   --  Generate a random nonce for AEAD operations
   procedure Generate_Nonce (
      Rand  : in  Byte_Array;
      Nonce : out Session_Nonce
   ) is
   begin
      --  Copy first 24 bytes as nonce
      for I in Nonce'Range loop
         Nonce (I) := Rand (I);
      end loop;
   end Generate_Nonce;

   --  Zeroize session key (constant-time)
   procedure Zeroize_Session_Key (Key : in Out Session_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_Session_Key;

   --  Zeroize recipient context (including secret key)
   procedure Zeroize_Recipient_Context (Ctx : in Out Recipient_Context) is
   begin
      --  Zeroize secret key
      for I in Ctx.KEM_SK'Range loop
         Ctx.KEM_SK (I) := 0;
      end loop;

      --  Zeroize public key
      for I in Ctx.KEM_PK'Range loop
         Ctx.KEM_PK (I) := 0;
      end loop;

      --  Zeroize session key
      Zeroize_Session_Key (Ctx.Session_Key);

      Ctx.Established := False;
   end Zeroize_Recipient_Context;

end Anubis_Session;
