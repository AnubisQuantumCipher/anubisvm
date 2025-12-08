--  Test message encoding/decoding for ML-KEM

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLKEM_Types; use Anubis_MLKEM_Types;
with Anubis_MLKEM_Poly;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;

procedure Test_Msg_Encode is

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

   Q_Half : constant := (Q + 1) / 2;  -- 1665

   Msg_In : Message;
   Poly : Polynomial;
   Msg_Out : Message;
   All_Pass : Boolean := True;

begin
   Put_Line ("Message Encode/Decode Test");
   Put_Line ("==========================");
   New_Line;

   Put_Line ("Q = " & Natural'Image (Q) & ", Q_Half = " & Natural'Image (Q_Half));
   New_Line;

   --  Test with simple message
   for I in 0 .. 31 loop
      Msg_In (I) := Byte (I);
   end loop;

   Put_Line ("Input message: " & Bytes_To_Hex (Byte_Array (Msg_In)));

   --  Encode message to polynomial
   Anubis_MLKEM_Poly.Msg_To_Poly (Msg_In, Poly);

   --  Check first few polynomial coefficients
   Put_Line ("First 16 poly coefficients (should be 0 or Q_Half):");
   for I in 0 .. 15 loop
      Put ("  [" & Natural'Image (I) & "] = " & Field_Element'Image (Poly (I)));
      if Poly (I) = 0 or Poly (I) = Q_Half then
         Put_Line (" OK");
      else
         Put_Line (" BAD!");
         All_Pass := False;
      end if;
   end loop;

   --  Now decode back to message
   --  This simulates what K_PKE_Decrypt does
   for I in 0 .. 31 loop
      Msg_Out (I) := 0;
      for J in 0 .. 7 loop
         declare
            Coef : constant Unsigned_32 := Unsigned_32 (Poly (8 * I + J));
            Bit : constant Unsigned_8 :=
               Unsigned_8 (((Coef * 2 + Q / 2) / Q) and 1);
         begin
            Msg_Out (I) := Msg_Out (I) or Shift_Left (Bit, J);
         end;
      end loop;
   end loop;

   Put_Line ("");
   Put_Line ("Output message: " & Bytes_To_Hex (Byte_Array (Msg_Out)));

   if Msg_In = Msg_Out then
      Put_Line ("");
      Put_Line ("RESULT: PASS - Round-trip works!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Put_Line ("");
      Put_Line ("RESULT: FAIL - Messages differ!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Test_Msg_Encode;
