pragma SPARK_Mode (On);

package body Multisig_Wallet with SPARK_Mode => On is

   procedure Initialize (
      State : in Out Wallet_State;
      Owners : Unsigned_64;
      Required : Unsigned_64) is
   begin
      State := (
         Initialized => True,
         Balance => 0,
         Required_Sigs => Required,
         Owner_Count => Owners,
         Pending_Amount => 0,
         Pending_Sigs => No_Signatures,
         Pending_Count => 0);
   end Initialize;

   procedure Deposit (
      State : in Out Wallet_State;
      Amount : Unsigned_64) is
   begin
      State.Balance := State.Balance + Amount;
   end Deposit;

   procedure Propose_Withdrawal (
      State : in Out Wallet_State;
      Amount : Unsigned_64) is
   begin
      State.Pending_Amount := Amount;
      State.Pending_Sigs := No_Signatures;
      State.Pending_Count := 0;
   end Propose_Withdrawal;

   procedure Sign_Withdrawal (
      State : in Out Wallet_State;
      Owner : Owner_Index) is
   begin
      State.Pending_Sigs (Owner) := True;
      State.Pending_Count := State.Pending_Count + 1;
   end Sign_Withdrawal;

   procedure Execute_Withdrawal (State : in Out Wallet_State) is
   begin
      State.Balance := State.Balance - State.Pending_Amount;
      State.Pending_Amount := 0;
      State.Pending_Sigs := No_Signatures;
      State.Pending_Count := 0;
   end Execute_Withdrawal;

   procedure Cancel_Withdrawal (State : in Out Wallet_State) is
   begin
      State.Pending_Amount := 0;
      State.Pending_Sigs := No_Signatures;
      State.Pending_Count := 0;
   end Cancel_Withdrawal;

   function Get_Balance (State : Wallet_State) return Unsigned_64 is
   begin
      return State.Balance;
   end Get_Balance;

   function Get_Pending (State : Wallet_State) return Unsigned_64 is
   begin
      return State.Pending_Amount;
   end Get_Pending;

   function Get_Sig_Count (State : Wallet_State) return Unsigned_64 is
   begin
      return State.Pending_Count;
   end Get_Sig_Count;

   function Is_Ready (State : Wallet_State) return Boolean is
   begin
      return State.Pending_Count >= State.Required_Sigs;
   end Is_Ready;

end Multisig_Wallet;
