pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_KMAC; use Anubis_KMAC;

package body Anubis_KDF with
   SPARK_Mode => On
is

   --  Domain separators for KDF operations
   Extract_Domain : constant String := "kdf-extract";
   Expand_Domain_Base : constant String := "kdf-expand-";

   --  Convert Natural to 4-byte big-endian
   procedure Natural_To_BE32 (N : Natural; B : out Byte_Array) with
      Global => null,
      Pre => B'Length = 4
   is
      N_U32 : constant Unsigned_32 := Unsigned_32 (N);
   begin
      B := (others => 0);
      B (B'First)     := Byte (Shift_Right (N_U32, 24) and 16#FF#);
      B (B'First + 1) := Byte (Shift_Right (N_U32, 16) and 16#FF#);
      B (B'First + 2) := Byte (Shift_Right (N_U32, 8) and 16#FF#);
      B (B'First + 3) := Byte (N_U32 and 16#FF#);
   end Natural_To_BE32;

   --  Extract: PRK = KMAC256(Salt, IKM, "kdf-extract")
   procedure Extract (
      IKM  : Byte_Array;
      Salt : KDF_Salt;
      Prk  : out KDF_PRK
   ) is
      KMAC_Key : Anubis_KMAC.KMAC_Key;
   begin
      Prk := (others => 0);

      --  Use salt as KMAC key
      for I in Salt'Range loop
         pragma Loop_Invariant (I >= Salt'First);
         KMAC_Key (I - Salt'First) := Salt (I);
      end loop;

      --  PRK = KMAC256(salt, IKM, "kdf-extract")
      Anubis_KMAC.KMAC256 (KMAC_Key, IKM, Extract_Domain, Prk);
   end Extract;

   --  Expand: Generate OKM from PRK using KMAC256 in counter mode
   --
   --  For each 32-byte block i (1 to ceil(L/32)):
   --    T(i) = KMAC256(PRK, T(i-1) || info || counter || context, "kdf-expand")
   --  OKM = first L bytes of T(1) || T(2) || ...
   procedure Expand (
      Prk     : KDF_PRK;
      Info    : Byte_Array;
      Context : String;
      OKM     : out Byte_Array
   ) is
      KMAC_Key : Anubis_KMAC.KMAC_Key;
      T_Prev : Anubis_KMAC.KMAC256_Tag := (others => 0);
      T_Curr : Anubis_KMAC.KMAC256_Tag;
      Num_Blocks : Natural;
      Pos : Natural := 0;

      --  Message for each iteration: T_prev || info || counter (4 bytes)
      Msg_Len : Natural;
      Counter_Bytes : Byte_Array (0 .. 3);

      --  Build full domain: "kdf-expand-" || context
      Full_Domain : String (1 .. Expand_Domain_Base'Length + Context'Length);
   begin
      OKM := (others => 0);

      --  Copy PRK to KMAC key
      for I in Prk'Range loop
         pragma Loop_Invariant (I >= Prk'First);
         KMAC_Key (I - Prk'First) := Prk (I);
      end loop;

      --  Build full domain string
      Full_Domain (1 .. Expand_Domain_Base'Length) := Expand_Domain_Base;
      Full_Domain (Expand_Domain_Base'Length + 1 .. Full_Domain'Last) := Context;

      --  Calculate number of blocks needed
      Num_Blocks := (OKM'Length + Key_Size - 1) / Key_Size;

      --  Generate blocks
      for Block in 1 .. Num_Blocks loop
         pragma Loop_Invariant (Block >= 1);
         pragma Loop_Invariant (Pos <= OKM'Length);

         --  First block: T(1) = KMAC256(PRK, info || 0x00000001, domain)
         --  Later blocks: T(i) = KMAC256(PRK, T(i-1) || info || counter, domain)

         Natural_To_BE32 (Block, Counter_Bytes);

         if Block = 1 then
            --  First block: message = info || counter
            Msg_Len := Info'Length + 4;
            declare
               Msg : Byte_Array (0 .. Msg_Len - 1) := (others => 0);
               Msg_Pos : Natural := 0;
            begin
               --  Copy info
               for I in Info'Range loop
                  pragma Loop_Invariant (Msg_Pos <= Msg'Last);
                  if Msg_Pos <= Msg'Last then
                     Msg (Msg_Pos) := Info (I);
                     Msg_Pos := Msg_Pos + 1;
                  end if;
               end loop;

               --  Append counter
               for I in Counter_Bytes'Range loop
                  pragma Loop_Invariant (Msg_Pos <= Msg'Last);
                  if Msg_Pos <= Msg'Last then
                     Msg (Msg_Pos) := Counter_Bytes (I);
                     Msg_Pos := Msg_Pos + 1;
                  end if;
               end loop;

               Anubis_KMAC.KMAC256 (KMAC_Key, Msg, Full_Domain, T_Curr);
            end;
         else
            --  Later blocks: message = T_prev || info || counter
            Msg_Len := Key_Size + Info'Length + 4;
            declare
               Msg : Byte_Array (0 .. Msg_Len - 1) := (others => 0);
               Msg_Pos : Natural := 0;
            begin
               --  Copy T_prev
               for I in T_Prev'Range loop
                  pragma Loop_Invariant (Msg_Pos <= Msg'Last);
                  if Msg_Pos <= Msg'Last then
                     Msg (Msg_Pos) := T_Prev (I);
                     Msg_Pos := Msg_Pos + 1;
                  end if;
               end loop;

               --  Copy info
               for I in Info'Range loop
                  pragma Loop_Invariant (Msg_Pos <= Msg'Last);
                  if Msg_Pos <= Msg'Last then
                     Msg (Msg_Pos) := Info (I);
                     Msg_Pos := Msg_Pos + 1;
                  end if;
               end loop;

               --  Append counter
               for I in Counter_Bytes'Range loop
                  pragma Loop_Invariant (Msg_Pos <= Msg'Last);
                  if Msg_Pos <= Msg'Last then
                     Msg (Msg_Pos) := Counter_Bytes (I);
                     Msg_Pos := Msg_Pos + 1;
                  end if;
               end loop;

               Anubis_KMAC.KMAC256 (KMAC_Key, Msg, Full_Domain, T_Curr);
            end;
         end if;

         --  Copy this block's output to OKM
         declare
            Remaining : constant Natural := OKM'Length - Pos;
            Copy_Len : constant Natural := Natural'Min (Key_Size, Remaining);
         begin
            for I in 0 .. Copy_Len - 1 loop
               pragma Loop_Invariant (I >= 0 and I < Copy_Len);
               pragma Loop_Invariant (Pos + I <= OKM'Last);
               if Pos + I <= OKM'Last then
                  OKM (OKM'First + Pos + I) := T_Curr (I);
               end if;
            end loop;
            Pos := Pos + Copy_Len;
         end;

         --  Save current block for next iteration
         T_Prev := T_Curr;
      end loop;
   end Expand;

   --  Derive_Key: One-step derivation
   procedure Derive_Key (
      Master  : Master_Key;
      Salt    : KDF_Salt;
      Info    : Byte_Array;
      Context : String;
      Key     : out Derived_Key
   ) is
      Prk_Value : KDF_PRK;
   begin
      Key := (others => 0);
      Extract (Master, Salt, Prk_Value);
      Expand (Prk_Value, Info, Context, Key);
   end Derive_Key;

   --  Derive_Shield_Key: Key for encrypted state
   procedure Derive_Shield_Key (
      Master : Master_Key;
      Salt   : KDF_Salt;
      Index  : Natural;
      Key    : out Derived_Key
   ) is
      Index_Bytes : Byte_Array (0 .. 3);
   begin
      Natural_To_BE32 (Index, Index_Bytes);
      Derive_Key (Master, Salt, Index_Bytes, Context_Shield, Key);
   end Derive_Shield_Key;

   --  Derive_Eye_Key: Key for viewing
   procedure Derive_Eye_Key (
      Master : Master_Key;
      Salt   : KDF_Salt;
      Viewer : Byte_Array;
      Key    : out Derived_Key
   ) is
   begin
      Derive_Key (Master, Salt, Viewer, Context_Eye, Key);
   end Derive_Eye_Key;

   --  Derive_Gate_Key: Session key
   procedure Derive_Gate_Key (
      Master     : Master_Key;
      Salt       : KDF_Salt;
      Session_ID : Byte_Array;
      Key        : out Derived_Key
   ) is
   begin
      Derive_Key (Master, Salt, Session_ID, Context_Gate, Key);
   end Derive_Gate_Key;

   --  Derive_Whisper_Key: Confidential value key
   procedure Derive_Whisper_Key (
      Master   : Master_Key;
      Salt     : KDF_Salt;
      Value_ID : Byte_Array;
      Key      : out Derived_Key
   ) is
   begin
      Derive_Key (Master, Salt, Value_ID, Context_Whisper, Key);
   end Derive_Whisper_Key;

   --  Secure key zeroization
   procedure Zeroize_Key (Key : in Out Derived_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_Key;

   --  Secure master key zeroization
   procedure Zeroize_Master (Key : in Out Master_Key) is
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I >= Key'First);
         pragma Loop_Invariant (for all J in Key'First .. I - 1 => Key (J) = 0);
         Key (I) := 0;
      end loop;
   end Zeroize_Master;

end Anubis_KDF;
