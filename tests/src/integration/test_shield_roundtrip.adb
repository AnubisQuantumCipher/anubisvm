with Ada.Text_IO;        use Ada.Text_IO;
with Interfaces;         use Interfaces;

with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_Privacy;      use Aegis_Privacy;

with Anubis_Types;       use Anubis_Types;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_MLKEM;       use Anubis_MLKEM;
with Anubis_Shield;

procedure Test_Shield_Roundtrip is

   --  Simple ML-KEM keypair for SHIELD
   Seed_D : Seed := (others => 0);
   Seed_Z : Seed := (others => 1);

   EK : Encapsulation_Key;
   DK : Decapsulation_Key;

   --  Plaintext to protect
   Plain     : VM_Byte_Array (0 .. 31);
   Plain_Out : VM_Byte_Array (0 .. Anubis_Shield.Max_Entry_Size - 1);
   PT_Len    : Natural;

   --  SHIELD parameters
   User_KEM_PK : VM_Byte_Array (0 .. 1567);
   User_KEM_SK : VM_Byte_Array (0 .. 3167);
   Randomness  : VM_Byte_Array (0 .. 63);

   Entry      : Anubis_Shield.Private_Entry;
   Commit     : Anubis_Shield.Entry_Commitment;
   Success    : Boolean;

begin
   Put_Line ("[SHIELD] ML-KEM keygen...");

   --  Derive deterministic seeds
   for I in Seed_D'Range loop
      Seed_D (I) := Byte (I);
      Seed_Z (I) := Byte (Seed_Z'Length - 1 - I);
   end loop;

   KeyGen (Seed_D, Seed_Z, EK, DK);

   --  Map ML-KEM keys into VM byte arrays
   for I in User_KEM_PK'Range loop
      User_KEM_PK (I) := Byte (EK (I));
   end loop;

   for I in User_KEM_SK'Range loop
      User_KEM_SK (I) := Byte (DK (I));
   end loop;

   --  Simple plaintext and randomness
   for I in Plain'Range loop
      Plain (I) := Byte (I);
   end loop;

   for I in Randomness'Range loop
      Randomness (I) := Byte (I);
   end loop;

   Put_Line ("[SHIELD] Encrypt_State...");

   declare
      Local_PT  : Anubis_Types.Byte_Array (0 .. Plain'Length - 1);
      Local_PK  : Anubis_Types.Byte_Array (0 .. 1567);
      Local_Rnd : Anubis_Types.Byte_Array (0 .. 63);
   begin
      for I in Local_PT'Range loop
         Local_PT (I) := Anubis_Types.Byte (Plain (Plain'First + I));
      end loop;

      for I in Local_PK'Range loop
         Local_PK (I) := User_KEM_PK (I);
      end loop;

      for I in Local_Rnd'Range loop
         Local_Rnd (I) := Randomness (I);
      end loop;

      Anubis_Shield.Encrypt_State (
         Plaintext   => Local_PT,
         User_KEM_PK => Local_PK,
         Randomness  => Local_Rnd,
         Priv_Entry  => Entry,
         Commitment  => Commit,
         Success     => Success
      );
   end;

   if not Success then
      Put_Line ("[SHIELD] Encrypt_State failed");
      return;
   end if;

   Put_Line ("[SHIELD] Decrypt_State...");

   declare
      Local_SK  : Anubis_Types.Byte_Array (0 .. 3167);
      Local_PT  : Anubis_Types.Byte_Array (0 .. Anubis_Shield.Max_Entry_Size - 1);
      Local_Len : Natural;
   begin
      for I in Local_SK'Range loop
         Local_SK (I) := User_KEM_SK (I);
      end loop;

      for I in Local_PT'Range loop
         Local_PT (I) := 0;
      end loop;

      Anubis_Shield.Decrypt_State (
         Priv_Entry => Entry,
         User_KEM_SK => Local_SK,
         Plaintext   => Local_PT,
         PT_Length   => Local_Len,
         Success     => Success
      );

      PT_Len := Local_Len;

      if Success and then PT_Len = Plain'Length then
         for I in 0 .. PT_Len - 1 loop
            Plain_Out (I) := VM_Byte (Local_PT (I));
         end loop;
      end if;
   end;

   if not Success then
      Put_Line ("[SHIELD] Decrypt_State failed");
      return;
   end if;

   if PT_Len /= Plain'Length then
      Put_Line ("[SHIELD] Length mismatch after decrypt");
      Put_Line ("  expected: " & Integer'Image (Plain'Length));
      Put_Line ("  actual  : " & Integer'Image (PT_Len));
      return;
   end if;

   for I in 0 .. PT_Len - 1 loop
      if Plain_Out (I) /= Plain (I) then
         Put_Line ("[SHIELD] Mismatch at byte "
           & Integer'Image (I));
         return;
      end if;
   end loop;

   Put_Line ("[SHIELD] Roundtrip OK");

end Test_Shield_Roundtrip;

