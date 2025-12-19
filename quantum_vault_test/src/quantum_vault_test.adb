--  quantum_vault_test Implementation
pragma SPARK_Mode (On);

package body quantum_vault_test with
   SPARK_Mode => On
is

   procedure Initialize (State : in Out Contract_State) is
   begin
      State.Initialized := True;
      State.Counter := 0;
   end Initialize;

   procedure Increment (State : in Out Contract_State) is
   begin
      State.Counter := State.Counter + 1;
   end Increment;

   function Get_Counter (State : Contract_State) return Unsigned_64 is
   begin
      return State.Counter;
   end Get_Counter;

end quantum_vault_test;
