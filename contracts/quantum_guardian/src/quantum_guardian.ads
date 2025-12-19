--  Quantum_Guardian - KHEPRI SPARK Contract
pragma SPARK_Mode (On);
with Interfaces; use Interfaces;

package Quantum_Guardian with SPARK_Mode => On is

   Max_Count : constant := 2**32 - 1;
   subtype Count_Type is Unsigned_64 range 0 .. Max_Count;

   type Storage is record
      Version : Unsigned_64;
      Count   : Count_Type;
      Owner   : Unsigned_64;
   end record;

   Empty_Storage : constant Storage := (0, 0, 0);

   procedure Initialize (State : out Storage; Owner_ID : Unsigned_64)
      with Global => null,
           Post => State.Version = 1 and State.Count = 0;

   procedure Increment (State : in out Storage)
      with Global => null,
           Pre => State.Count < Max_Count,
           Post => State.Count = State.Count'Old + 1;

   procedure Reset (State : in Out Storage)
      with Global => null, Post => State.Count = 0;

   function Get_Count (State : Storage) return Count_Type
      with Global => null;
   function Get_Owner (State : Storage) return Unsigned_64
      with Global => null;
   function Get_Version (State : Storage) return Unsigned_64
      with Global => null;

end Quantum_Guardian;
