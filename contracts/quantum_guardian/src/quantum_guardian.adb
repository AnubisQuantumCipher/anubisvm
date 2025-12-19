--  Quantum_Guardian - Implementation
pragma SPARK_Mode (On);

package body Quantum_Guardian with SPARK_Mode => On is

   procedure Initialize (State : out Storage; Owner_ID : Unsigned_64) is
   begin
      State := (Version => 1, Count => 0, Owner => Owner_ID);
   end Initialize;

   procedure Increment (State : in Out Storage) is
   begin
      State.Count := State.Count + 1;
   end Increment;

   procedure Reset (State : in Out Storage) is
   begin
      State.Count := 0;
   end Reset;

   function Get_Count (State : Storage) return Count_Type is
      (State.Count);
   function Get_Owner (State : Storage) return Unsigned_64 is
      (State.Owner);
   function Get_Version (State : Storage) return Unsigned_64 is
      (State.Version);

end Quantum_Guardian;
