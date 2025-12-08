--  Diagnostic test for ML-DSA-87

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA; use Anubis_MLDSA;

procedure Test_Diagnostic is
   Seed_KG : Seed := (others => 0);
   PK : Public_Key;
   SK : Secret_Key;
begin
   Put_Line ("Starting diagnostic test...");

   --  Set up seed
   for I in 0 .. 31 loop
      Seed_KG (I) := Byte (I);
   end loop;

   Put_Line ("Calling KeyGen...");
   KeyGen (Seed_KG, PK, SK);
   Put_Line ("KeyGen completed!");

   Put_Line ("Done.");
exception
   when E : others =>
      Put_Line ("Exception caught!");
end Test_Diagnostic;
