--  quantum_vault_test Test Runner
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;
with quantum_vault_test; use quantum_vault_test;

procedure quantum_vault_test_Main is
   State : Contract_State := Empty_State;
begin
   Put_Line ("KHEPRI Contract Test: quantum_vault_test");
   Put_Line ("===========================");
   New_Line;

   Put_Line ("Initializing contract...");
   Initialize (State);
   Put_Line ("  Initialized: " & Boolean'Image (State.Initialized));

   Put_Line ("Incrementing counter 3 times...");
   Increment (State);
   Increment (State);
   Increment (State);
   Put_Line ("  Counter: " & Interfaces.Unsigned_64'Image (Get_Counter (State)));

   New_Line;
   Put_Line ("All tests passed!");
end quantum_vault_test_Main;
