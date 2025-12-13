--  Quantum_Vault_V2 - AnubisVM Contract Entry Point
with Quantum_Vault_V2;  use Quantum_Vault_V2;
with Khepri_Types;   use Khepri_Types;
with Khepri_Crypto;  use Khepri_Crypto;
with Ada.Text_IO;    use Ada.Text_IO;

procedure Quantum_Vault_V2_Main is
   State  : Contract_State := Empty_State;
   Owner  : Address := [others => 1];  --  Test owner address
   Supply : Balance := From_Natural (1000);
begin
   Put_Line ("AnubisVM Contract Test");
   Put_Line ("======================");
   
   --  Initialize contract with owner and supply
   Initialize (State, Owner, Supply);
   Put_Line ("Contract initialized");
   
   --  Verify owner check
   pragma Assert (Is_Owner (State, Owner));
   Put_Line ("Owner verification passed");
   
   Put_Line ("All tests passed!");
end Quantum_Vault_V2_Main;
