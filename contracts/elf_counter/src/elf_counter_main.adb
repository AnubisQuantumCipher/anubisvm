--  Elf_Counter - Entry Point
with Elf_Counter; use Elf_Counter;
with Interfaces; use Interfaces;

procedure Elf_Counter_Main is
   State : Storage := Empty_Storage;
begin
   Initialize (State, 1);
   for I in 1 .. 5 loop
      if State.Count < Max_Count then Increment (State); end if;
   end loop;
   pragma Assert (Get_Count (State) = 5);
end Elf_Counter_Main;
