--  Quantum_Guardian - Entry Point
with Quantum_Guardian; use Quantum_Guardian;
with Interfaces; use Interfaces;

procedure Quantum_Guardian_Main is
   State : Storage := Empty_Storage;
begin
   Initialize (State, 1);
   for I in 1 .. 5 loop
      if State.Count < Max_Count then Increment (State); end if;
   end loop;
   pragma Assert (Get_Count (State) = 5);
end Quantum_Guardian_Main;
