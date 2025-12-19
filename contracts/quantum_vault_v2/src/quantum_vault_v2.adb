--  Quantum_Vault_V2 - AnubisVM Contract Implementation
pragma SPARK_Mode (On);

package body Quantum_Vault_V2 with SPARK_Mode => On is

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
   begin
      --  NOTE: This is a simplified token implementation without per-address balances.
      --  In a production ERC20-style token, you would:
      --  1. Track balances: mapping(address => uint256) using THOTH storage
      --  2. Debit sender's balance, credit recipient's balance
      --  3. Emit Transfer(from, to, amount) event
      --
      --  For this demo, we:
      --  - Verify caller is owner (owns all supply)
      --  - Verify recipient address is valid
      --  - Verify sufficient supply exists
      --  - Deduct from total supply (representing owner's balance)

      if not Is_Owner (State, Caller) then
         Status := Unauthorized;
         return;
      end if;

      --  Validate recipient address (don't send to null address)
      if not Is_Valid_Address (To) then
         Status := Invalid_Input;
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
      --  Serialize state fields into a canonical byte representation
      --  Format: Version (32) || Owner (32) || Total_Supply (32) || Initialized (1)
      --  Total: 97 bytes
      Buffer : Aegis_VM_Types.Byte_Array (0 .. 96);
      Version_Bytes : constant Bytes32 := U256_To_Bytes_BE (State.Version);
      Owner_Bytes : constant Bytes32 := Address_To_Bytes (State.Owner);
      Supply_Bytes : constant Bytes32 := U256_To_Bytes_BE (State.Total_Supply);
   begin
      --  Pack version (bytes 0..31)
      for I in Bytes32'Range loop
         Buffer (Natural (I)) := Version_Bytes (I);
      end loop;

      --  Pack owner (bytes 32..63)
      for I in Bytes32'Range loop
         Buffer (32 + Natural (I)) := Owner_Bytes (I);
      end loop;

      --  Pack total supply (bytes 64..95)
      for I in Bytes32'Range loop
         Buffer (64 + Natural (I)) := Supply_Bytes (I);
      end loop;

      --  Pack initialized flag (byte 96)
      Buffer (96) := (if State.Initialized then 1 else 0);

      --  Hash the serialized state
      return SHA3_256 (Buffer);
   end State_Hash;

end Quantum_Vault_V2;
