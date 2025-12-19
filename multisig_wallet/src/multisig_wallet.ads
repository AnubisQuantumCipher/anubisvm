--  Multi-Signature Wallet Contract
--  Requires M of N signatures to execute transactions
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package Multisig_Wallet with SPARK_Mode => On is

   Max_Owners : constant := 5;

   type Owner_Index is range 1 .. Max_Owners;
   type Signature_Mask is array (Owner_Index) of Boolean;

   No_Signatures : constant Signature_Mask := (others => False);

   type Wallet_State is record
      Initialized : Boolean;
      Balance : Unsigned_64;
      Required_Sigs : Unsigned_64;
      Owner_Count : Unsigned_64;
      Pending_Amount : Unsigned_64;
      Pending_Sigs : Signature_Mask;
      Pending_Count : Unsigned_64;
   end record;

   Empty_Wallet : constant Wallet_State := (
      Initialized => False,
      Balance => 0,
      Required_Sigs => 0,
      Owner_Count => 0,
      Pending_Amount => 0,
      Pending_Sigs => No_Signatures,
      Pending_Count => 0);

   procedure Initialize (
      State : in out Wallet_State;
      Owners : Unsigned_64;
      Required : Unsigned_64)
   with
      Global => null,
      Pre  => not State.Initialized
              and Owners >= 1 and Owners <= Max_Owners
              and Required >= 1 and Required <= Owners,
      Post => State.Initialized
              and State.Owner_Count = Owners
              and State.Required_Sigs = Required
              and State.Balance = 0;

   procedure Deposit (
      State : in Out Wallet_State;
      Amount : Unsigned_64)
   with
      Global => null,
      Pre  => State.Initialized
              and Amount > 0
              and State.Balance <= Unsigned_64'Last - Amount,
      Post => State.Balance = State.Balance'Old + Amount;

   procedure Propose_Withdrawal (
      State : in Out Wallet_State;
      Amount : Unsigned_64)
   with
      Global => null,
      Pre  => State.Initialized
              and Amount > 0
              and Amount <= State.Balance
              and State.Pending_Amount = 0,
      Post => State.Pending_Amount = Amount
              and State.Pending_Count = 0;

   procedure Sign_Withdrawal (
      State : in Out Wallet_State;
      Owner : Owner_Index)
   with
      Global => null,
      Pre  => State.Initialized
              and State.Pending_Amount > 0
              and Unsigned_64 (Owner) <= State.Owner_Count
              and not State.Pending_Sigs (Owner),
      Post => State.Pending_Sigs (Owner)
              and State.Pending_Count = State.Pending_Count'Old + 1;

   procedure Execute_Withdrawal (State : in Out Wallet_State)
   with
      Global => null,
      Pre  => State.Initialized
              and State.Pending_Amount > 0
              and State.Pending_Count >= State.Required_Sigs,
      Post => State.Balance = State.Balance'Old - State.Pending_Amount'Old
              and State.Pending_Amount = 0
              and State.Pending_Count = 0;

   procedure Cancel_Withdrawal (State : in Out Wallet_State)
   with
      Global => null,
      Pre  => State.Initialized and State.Pending_Amount > 0,
      Post => State.Pending_Amount = 0 and State.Pending_Count = 0;

   function Get_Balance (State : Wallet_State) return Unsigned_64
   with Global => null;

   function Get_Pending (State : Wallet_State) return Unsigned_64
   with Global => null;

   function Get_Sig_Count (State : Wallet_State) return Unsigned_64
   with Global => null;

   function Is_Ready (State : Wallet_State) return Boolean
   with Global => null;

end Multisig_Wallet;
