--  Sovereign_Token - AnubisVM Contract Entry Point
with Sovereign_Token;  use Sovereign_Token;
with Khepri_Types;   use Khepri_Types;
with Khepri_Crypto;  use Khepri_Crypto;
with Ada.Text_IO;    use Ada.Text_IO;

procedure Sovereign_Token_Main is
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
end Sovereign_Token_Main;
