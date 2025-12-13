--  Quantum_Vault_V2 - AnubisVM SPARK Contract
--  Features: KHEPRI SDK, THOTH Storage, ANKH Crypto
pragma SPARK_Mode (On);

with Interfaces;      use Interfaces;
with Khepri_Types;    use Khepri_Types;
with Khepri_Crypto;   use Khepri_Crypto;
with Aegis_VM_Types;

package Quantum_Vault_V2 with SPARK_Mode => On is

   --  Contract version (AAS-001 compliant)
   Contract_Version : constant Uint256 := One;

   --  State record with quantum-safe types
   type Contract_State is record
      Version      : Uint256;
      Owner        : Address;
      Total_Supply : Balance;
      Initialized  : Boolean;
   end record;

   Empty_State : constant Contract_State := (
      Version      => Zero,
      Owner        => Null_Address,
      Total_Supply => Zero,
      Initialized  => False
   );

   --  Initialize contract with owner (ML-DSA-87 derived address)
   procedure Initialize (
      State  : out Contract_State;
      Owner  : Address;
      Supply : Balance
   ) with
      Global => null,
      Pre    => Is_Valid_Address (Owner),
      Post   => State.Initialized and State.Version = One;

   --  Check if caller is owner
   function Is_Owner (State : Contract_State; Caller : Address)
      return Boolean with Global => null;

   --  Transfer tokens (requires ownership)
   procedure Transfer (
      State  : in out Contract_State;
      To     : Address;
      Amount : Balance;
      Caller : Address;
      Status : out Error_Code
   ) with
      Global => null,
      Pre    => State.Initialized and Is_Valid_Address (To);

   --  Query total supply
   function Get_Supply (State : Contract_State) return Balance
      with Global => null;

   --  Query owner address
   function Get_Owner (State : Contract_State) return Address
      with Global => null;

   --  Compute state hash (SHA3-256)
   function State_Hash (State : Contract_State) return Hash_256
      with Global => null;

end Quantum_Vault_V2;
