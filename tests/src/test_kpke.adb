--  Test K-PKE encrypt/decrypt directly

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_MLKEM; use Anubis_MLKEM;

procedure Test_KPKE is

   --  Hex to byte conversion
   function Hex_To_Byte (C1, C2 : Character) return Byte is
      function Hex_Digit (C : Character) return Byte is
      begin
         case C is
            when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
            when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
            when others => raise Constraint_Error;
         end case;
      end Hex_Digit;
   begin
      return Shift_Left (Hex_Digit (C1), 4) or Hex_Digit (C2);
   end Hex_To_Byte;

   function Hex_To_Bytes (Hex : String) return Byte_Array is
      Result : Byte_Array (0 .. Hex'Length / 2 - 1);
   begin
      for I in Result'Range loop
         Result (I) := Hex_To_Byte (
            Hex (Hex'First + I * 2),
            Hex (Hex'First + I * 2 + 1)
         );
      end loop;
      return Result;
   end Hex_To_Bytes;

   function Bytes_To_Hex (Bytes : Byte_Array) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Result : String (1 .. Bytes'Length * 2);
      Pos : Natural := 1;
   begin
      for B of Bytes loop
         Result (Pos) := Hex_Chars (Natural (Shift_Right (B, 4)) + 1);
         Result (Pos + 1) := Hex_Chars (Natural (B and 16#0F#) + 1);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Bytes_To_Hex;

   D_Seed_Hex : constant String := "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
   Z_Seed_Hex : constant String := "202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f";
   Coins_Hex  : constant String := "404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f";

   D_Bytes : constant Byte_Array := Hex_To_Bytes (D_Seed_Hex);
   Z_Bytes : constant Byte_Array := Hex_To_Bytes (Z_Seed_Hex);
   C_Bytes : constant Byte_Array := Hex_To_Bytes (Coins_Hex);

   Random_D, Random_Z, Coins : Seed;
   EK : Encapsulation_Key;
   DK : Decapsulation_Key;
   CT : MLKEM_Ciphertext;
   Msg_In, Msg_Out : Message;
   DK_PKE : Byte_Array (0 .. Encoded_Vector_Bytes - 1);

begin
   Put_Line ("K-PKE Encrypt/Decrypt Test");
   Put_Line ("==========================");
   New_Line;

   --  Copy seeds
   for I in 0 .. 31 loop
      Random_D (I) := D_Bytes (I);
      Random_Z (I) := Z_Bytes (I);
      Coins (I) := C_Bytes (I);
      Msg_In (I) := Byte (I);  -- Simple test message
   end loop;

   Put_Line ("Input message: " & Bytes_To_Hex (Byte_Array (Msg_In)));

   --  Generate keypair
   KeyGen (Random_D, Random_Z, EK, DK);
   Put_Line ("KeyGen: OK");

   --  Extract DK_PKE from DK
   for I in DK_PKE'Range loop
      DK_PKE (I) := DK (I);
   end loop;

   --  Encrypt
   K_PKE_Encrypt (EK, Msg_In, Coins, CT);
   Put_Line ("K_PKE_Encrypt: OK");

   --  Decrypt
   K_PKE_Decrypt (DK_PKE, CT, Msg_Out);
   Put_Line ("K_PKE_Decrypt: OK");

   Put_Line ("Output message: " & Bytes_To_Hex (Byte_Array (Msg_Out)));

   --  Compare
   if Msg_In = Msg_Out then
      Put_Line ("");
      Put_Line ("RESULT: PASS - Messages match!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Put_Line ("");
      Put_Line ("RESULT: FAIL - Messages differ!");
      Put_Line ("  Input:  " & Bytes_To_Hex (Byte_Array (Msg_In)));
      Put_Line ("  Output: " & Bytes_To_Hex (Byte_Array (Msg_Out)));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Test_KPKE;
