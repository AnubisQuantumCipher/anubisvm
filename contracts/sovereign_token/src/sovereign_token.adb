--  Sovereign_Token - AnubisVM Contract Implementation
pragma SPARK_Mode (On);

package body Sovereign_Token with SPARK_Mode => On is

   procedure Initialize (
      State  : out Contract_State;
      Owner  : Address;
      Supply : Balance
   ) is
   begin
      State := (
         Version      => One,
         Owner        => Owner,
         Total_Supply => Supply,
         Initialized  => True
      );
   end Initialize;

   function Is_Owner (State : Contract_State; Caller : Address)
      return Boolean is
   begin
      return Address_Equal (State.Owner, Caller);
   end Is_Owner;

   procedure Transfer (
      State  : in out Contract_State;
      To     : Address;
      Amount : Balance;
      Caller : Address;
      Status : out Error_Code
   ) is
      pragma Unreferenced (To);
   begin
      if not Is_Owner (State, Caller) then
         Status := Unauthorized;
         return;
      end if;
      if State.Total_Supply < Amount then
         Status := Insufficient_Balance;
         return;
      end if;
      State.Total_Supply := State.Total_Supply - Amount;
      Status := No_Error;
   end Transfer;

   function Get_Supply (State : Contract_State) return Balance is
      (State.Total_Supply);

   function Get_Owner (State : Contract_State) return Address is
      (State.Owner);

   function State_Hash (State : Contract_State) return Hash_256 is
      Bytes : Aegis_VM_Types.Byte_Array (0 .. 31) := [others => 0];
      pragma Unreferenced (State);
   begin
      return SHA3_256 (Bytes);
   end State_Hash;

end Sovereign_Token;
