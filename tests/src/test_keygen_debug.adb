--  Debug test for KeyGen determinism

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA; use Anubis_MLDSA;
with Anubis_SHA3; use Anubis_SHA3;

procedure Test_KeyGen_Debug is

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

   Seed1 : constant Seed := (others => 16#01#);
   Seed2 : constant Seed := (others => 16#01#);
   PK1, PK2 : Public_Key;
   SK1, SK2 : Secret_Key;

   --  Test SHAKE256 determinism first
   Output1, Output2 : Byte_Array (0 .. 127);
begin
   Put_Line ("Testing SHAKE256 determinism...");
   SHAKE256 (Byte_Array (Seed1), Output1, 128);
   SHAKE256 (Byte_Array (Seed2), Output2, 128);
   Put_Line ("  Output1: " & Bytes_To_Hex (Output1 (0 .. 15)) & "...");
   Put_Line ("  Output2: " & Bytes_To_Hex (Output2 (0 .. 15)) & "...");

   if Output1 = Output2 then
      Put_Line ("  SHAKE256 is deterministic: PASS");
   else
      Put_Line ("  SHAKE256 is NOT deterministic: FAIL");
   end if;

   New_Line;
   Put_Line ("Testing KeyGen determinism...");
   Put_Line ("  Seed: " & Bytes_To_Hex (Byte_Array (Seed1)));

   Put_Line ("  Calling KeyGen #1...");
   KeyGen (Seed1, PK1, SK1);
   Put_Line ("  PK1[0..15]: " & Bytes_To_Hex (Byte_Array (PK1 (0 .. 15))));

   Put_Line ("  Calling KeyGen #2...");
   KeyGen (Seed2, PK2, SK2);
   Put_Line ("  PK2[0..15]: " & Bytes_To_Hex (Byte_Array (PK2 (0 .. 15))));

   if Byte_Array (PK1) = Byte_Array (PK2) then
      Put_Line ("  KeyGen is deterministic: PASS");
   else
      Put_Line ("  KeyGen is NOT deterministic: FAIL");
      Put_Line ("  First difference at byte:");
      for I in PK1'Range loop
         if PK1 (I) /= PK2 (I) then
            Put_Line ("    Index" & Natural'Image (I) &
                      ": PK1=" & Bytes_To_Hex ((0 => PK1 (I))) &
                      " PK2=" & Bytes_To_Hex ((0 => PK2 (I))));
            exit;
         end if;
      end loop;
   end if;

end Test_KeyGen_Debug;
