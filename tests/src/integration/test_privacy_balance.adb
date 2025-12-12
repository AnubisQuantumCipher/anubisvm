with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Privacy;  use Aegis_Privacy;

procedure Test_Privacy_Balance is

   --  Build simple 64-byte commitments with XOR-based balance:
   --    C_in0 xor C_in1 = C_out0 xor C_fee

   subtype Commit_Index is Natural range 0 .. 63;

   function Make_Fee_Commit (Fee : Unsigned_64) return VM_Byte_Array is
      Res   : VM_Byte_Array (0 .. 63) := (others => 0);
      Value : Unsigned_64 := Fee;
   begin
      for I in 0 .. 7 loop
         Res (I) := Byte (Value and 16#FF#);
         Value := Value / 256;
      end loop;
      return Res;
   end Make_Fee_Commit;

   In_Commits  : VM_Byte_Array (0 .. 127);
   Out_Commits : VM_Byte_Array (0 .. 63);
   Proof       : VM_Byte_Array (0 .. 31) := (others => 0);

   Fee     : constant Unsigned_64 := 2;
   Fee_Comm: constant VM_Byte_Array := Make_Fee_Commit (Fee);

   Valid  : Boolean;
   Result : Privacy_Result;

begin
   Put_Line ("[Privacy] Verify_Balance smoke test");

   --  Choose two arbitrary input commitments C0, C1
   for I in Commit_Index loop
      In_Commits (Natural (I)) := Byte (I);
      In_Commits (64 + Natural (I)) := Byte (I * 3);
   end loop;

   --  Compute output commitment satisfying XOR balance
   for I in Commit_Index loop
      declare
         C0 : constant Byte := In_Commits (Natural (I));
         C1 : constant Byte := In_Commits (64 + Natural (I));
         F  : constant Byte := Fee_Comm (Natural (I));
      begin
         Out_Commits (Natural (I)) := C0 xor C1 xor F;
      end;
   end loop;

   Verify_Balance (
      Input_Commits  => In_Commits,
      Output_Commits => Out_Commits,
      Fee            => Fee,
      Proof          => Proof,
      Valid          => Valid,
      Result         => Result
   );

   Put_Line ("  success: " & (if Result.Success then "true" else "false"));
   Put_Line ("  valid  : " & (if Valid then "true" else "false"));
   Put_Line ("  gas    :" & Gas_Amount'Image (Result.Gas_Used));
   Put_Line ("  err    :" & Integer'Image (Integer (Result.Error_Code)));

end Test_Privacy_Balance;

