--  Mytest - Entry Point
with Mytest; use Mytest;
with Interfaces; use Interfaces;

procedure Mytest_Main is
   State : Storage := Empty_Storage;
begin
   Initialize (State, 1);
   for I in 1 .. 5 loop
      if State.Count < Max_Count then Increment (State); end if;
   end loop;
   pragma Assert (Get_Count (State) = 5);
end Mytest_Main;
