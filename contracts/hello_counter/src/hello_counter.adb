-------------------------------------------------------------------------------
--  HelloCounter - Implementation
--
--  SPARK-verified counter contract with formal proofs of:
--  - No overflow in Increment
--  - Bounded increment amounts
--  - Correct state transitions
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Hello_Counter with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Entry Point Implementations
   ---------------------------------------------------------------------------

   procedure Initialize (
      State    : out Storage;
      Owner_ID : Unsigned_64
   ) is
   begin
      State := (Count => 0, Owner => Owner_ID);
   end Initialize;

   procedure Increment (
      State : in out Storage
   ) is
   begin
      State.Count := State.Count + 1;
   end Increment;

   procedure Increment_By (
      State  : in out Storage;
      Amount : Count_Type
   ) is
   begin
      State.Count := State.Count + Amount;
   end Increment_By;

   procedure Reset (
      State : in out Storage
   ) is
   begin
      State.Count := 0;
   end Reset;

   ---------------------------------------------------------------------------
   --  View Functions
   ---------------------------------------------------------------------------

   function Get_Count (State : Storage) return Count_Type is
   begin
      return State.Count;
   end Get_Count;

   function Get_Owner (State : Storage) return Unsigned_64 is
   begin
      return State.Owner;
   end Get_Owner;

end Hello_Counter;
