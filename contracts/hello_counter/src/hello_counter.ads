-------------------------------------------------------------------------------
--  HelloCounter - KHEPRI Smart Contract
--
--  A simple counter contract demonstrating SPARK verified state transitions.
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package Hello_Counter with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Contract Storage
   ---------------------------------------------------------------------------

   --  Maximum counter value (prevents overflow)
   Max_Count : constant := 2**32 - 1;
   subtype Count_Type is Unsigned_64 range 0 .. Max_Count;

   --  Contract state
   type Storage is record
      Count : Count_Type;
      Owner : Unsigned_64;  -- Simplified owner ID
   end record;

   --  Initial empty state
   Empty_Storage : constant Storage := (Count => 0, Owner => 0);

   ---------------------------------------------------------------------------
   --  Entry Points (callable via vm_invoke)
   ---------------------------------------------------------------------------

   --  Initialize the contract
   procedure Initialize (
      State    : out Storage;
      Owner_ID : Unsigned_64
   ) with
      Global => null,
      Post   => State.Count = 0 and State.Owner = Owner_ID;

   --  Increment the counter by 1
   procedure Increment (
      State : in out Storage
   ) with
      Global => null,
      Pre    => State.Count < Max_Count,
      Post   => State.Count = State.Count'Old + 1;

   --  Increment the counter by a specific amount
   procedure Increment_By (
      State  : in out Storage;
      Amount : Count_Type
   ) with
      Global => null,
      Pre    => State.Count <= Max_Count - Amount,
      Post   => State.Count = State.Count'Old + Amount;

   --  Reset counter to zero (owner only in production)
   procedure Reset (
      State : in Out Storage
   ) with
      Global => null,
      Post   => State.Count = 0;

   ---------------------------------------------------------------------------
   --  View Functions (callable via vm_call - read-only)
   ---------------------------------------------------------------------------

   --  Get current count
   function Get_Count (State : Storage) return Count_Type with
      Global => null;

   --  Get owner ID
   function Get_Owner (State : Storage) return Unsigned_64 with
      Global => null;

end Hello_Counter;
