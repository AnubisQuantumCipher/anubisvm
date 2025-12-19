--  token_contract Contract
--  Certification Target: GOLD
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package token_contract with
   SPARK_Mode => On
is
   --  Contract metadata
   Contract_Name    : constant String := "token_contract";
   Contract_Version : constant String := "1.0.0";

   --  Contract state
   type Contract_State is record
      Initialized : Boolean;
      Counter     : Unsigned_64;
   end record;

   Empty_State : constant Contract_State := (
      Initialized => False,
      Counter     => 0);

   --  Initialize the contract
   procedure Initialize (State : in out Contract_State)
      with Global => null,
           Pre    => not State.Initialized,
           Post   => State.Initialized;

   --  Increment counter
   procedure Increment (State : in out Contract_State)
      with Global => null,
           Pre    => State.Initialized and State.Counter < Unsigned_64'Last,
           Post   => State.Counter = State.Counter'Old + 1;

   --  Get counter value
   function Get_Counter (State : Contract_State) return Unsigned_64
      with Global => null;

end token_contract;
