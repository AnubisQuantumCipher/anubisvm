-------------------------------------------------------------------------------
--  HelloCounter - Entry Point
--
--  This is the contract entry point that gets called by the VM runtime.
--  In production, this would be a runtime dispatch based on function selector.
-------------------------------------------------------------------------------

with Hello_Counter; use Hello_Counter;
with Interfaces; use Interfaces;

procedure Hello_Counter_Main is
   --  Contract state (would be loaded from storage in production)
   State : Hello_Counter.Storage := Hello_Counter.Empty_Storage;
begin
   --  Initialize with owner ID 1
   Hello_Counter.Initialize (State, 1);

   --  Increment a few times
   for I in 1 .. 5 loop
      if State.Count < Max_Count then
         Hello_Counter.Increment (State);
      end if;
   end loop;

   --  Final count should be 5
   pragma Assert (Hello_Counter.Get_Count (State) = 5);
end Hello_Counter_Main;
