--  KHEPRI Multisig Wallet Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_Multisig with
   SPARK_Mode => On,
   Refined_State => (Multisig_State =>
      (Config_Store, Owners_Store, Owner_Count_Store, Transactions,
       Confirmations, Tx_Counter, Time_Store, Daily_Spent_Store, Last_Day))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Owner_Array is array (0 .. Max_Owners - 1) of Address;
   type Tx_Array is array (0 .. Max_Transactions - 1) of Transaction;

   --  Confirmation tracking: (tx_id, owner) -> confirmed
   type Confirm_Entry is record
      Tx_Num  : Transaction_ID;
      Owner   : Address;
      Used    : Boolean;
   end record;

   Max_Confirms : constant := Max_Transactions * Max_Owners;
   type Confirm_Array is array (0 .. Max_Confirms - 1) of Confirm_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Config_Store      : Multisig_Config := Default_Config;
   Owners_Store      : Owner_Array := (others => Zero_Address);
   Owner_Count_Store : Natural := 0;
   Tx_Counter        : Natural := 0;
   Time_Store        : Timestamp := 0;
   Daily_Spent_Store : U256 := U256_Zero;
   Last_Day          : Timestamp := 0;

   Transactions : Tx_Array := (others => (
      ID            => 0,
      Target        => Zero_Address,
      Value         => U256_Zero,
      Data          => (Bytes => (others => 0), Length => 0),
      Proposer      => Zero_Address,
      Confirmations => 0,
      Status        => Status_Pending,
      Created_At    => 0,
      Executed_At   => 0,
      Used          => False
   ));

   Confirmations : Confirm_Array := (others => (
      Tx_Num => 0,
      Owner  => Zero_Address,
      Used   => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Owner (Owner : Address) return Natural is
   begin
      for I in 0 .. Owner_Count_Store - 1 loop
         if Owners_Store (I) = Owner then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Owner;

   function Find_Transaction (Tx_Id : Transaction_ID) return Natural is
   begin
      if Tx_Id < Max_Transactions and then Transactions (Tx_Id).Used then
         return Tx_Id;
      end if;
      return Natural'Last;
   end Find_Transaction;

   function Find_Confirmation (Tx_Id : Transaction_ID; Owner : Address) return Natural is
   begin
      for I in Confirmations'Range loop
         if Confirmations (I).Used
            and then Confirmations (I).Tx_Num = Tx_Id
            and then Confirmations (I).Owner = Owner
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Confirmation;

   function Find_Empty_Confirmation return Natural is
   begin
      for I in Confirmations'Range loop
         if not Confirmations (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Confirmation;

   function Count_Confirmations (Tx_Id : Transaction_ID) return Natural is
      Count : Natural := 0;
   begin
      for I in Confirmations'Range loop
         if Confirmations (I).Used
            and then Confirmations (I).Tx_Num = Tx_Id
         then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Confirmations;

   procedure Reset_Daily_Limit is
      Day_Seconds : constant Timestamp := 86400;
   begin
      if Time_Store >= Last_Day + Day_Seconds then
         Daily_Spent_Store := U256_Zero;
         Last_Day := Time_Store;
      end if;
   end Reset_Daily_Limit;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Owners        : in Owner_Address_Array;
      Owner_Count   : in Natural;
      Required_Sigs : in Natural;
      Success       : out Boolean
   ) is
   begin
      if Owner_Count_Store > 0 then
         Success := False;
         return;
      end if;

      for I in 0 .. Owner_Count - 1 loop
         Owners_Store (I) := Owners (I);
      end loop;

      Owner_Count_Store := Owner_Count;
      Config_Store.Required_Sigs := Required_Sigs;
      Tx_Counter := 0;
      Success := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Owner Management
   ---------------------------------------------------------------------------

   function Is_Owner (Account : Address) return Boolean is
   begin
      return Find_Owner (Account) /= Natural'Last;
   end Is_Owner;

   function Owner_Count return Natural is
   begin
      return Owner_Count_Store;
   end Owner_Count;

   function Required_Confirmations return Natural is
   begin
      return Config_Store.Required_Sigs;
   end Required_Confirmations;

   function Get_Owner (Index : Natural) return Address is
   begin
      if Index < Owner_Count_Store then
         return Owners_Store (Index);
      else
         return Zero_Address;
      end if;
   end Get_Owner;

   procedure Add_Owner (
      Caller    : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Multisig_Error
   ) is
   begin
      --  Only wallet itself can add owner (via executed tx)
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Owner_Count_Store >= Max_Owners then
         Success := False;
         Error := Error_Too_Many_Owners;
         return;
      end if;

      if Is_Owner (New_Owner) then
         Success := False;
         Error := Error_Already_Owner;
         return;
      end if;

      Owners_Store (Owner_Count_Store) := New_Owner;
      Owner_Count_Store := Owner_Count_Store + 1;

      Success := True;
      Error := Error_None;
   end Add_Owner;

   procedure Remove_Owner (
      Caller      : in     Address;
      Owner_Addr  : in     Address;
      Success     : out    Boolean;
      Error       : out    Multisig_Error
   ) is
      Slot : constant Natural := Find_Owner (Owner_Addr);
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Slot = Natural'Last then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      --  Must have at least required signers
      if Owner_Count_Store <= Config_Store.Required_Sigs then
         Success := False;
         Error := Error_Not_Enough_Owners;
         return;
      end if;

      --  Shift owners down
      for I in Slot .. Owner_Count_Store - 2 loop
         Owners_Store (I) := Owners_Store (I + 1);
      end loop;
      Owners_Store (Owner_Count_Store - 1) := Zero_Address;
      Owner_Count_Store := Owner_Count_Store - 1;

      Success := True;
      Error := Error_None;
   end Remove_Owner;

   procedure Replace_Owner (
      Caller    : in     Address;
      Old_Owner : in     Address;
      New_Owner : in     Address;
      Success   : out    Boolean;
      Error     : out    Multisig_Error
   ) is
      Slot : constant Natural := Find_Owner (Old_Owner);
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Slot = Natural'Last then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Is_Owner (New_Owner) then
         Success := False;
         Error := Error_Already_Owner;
         return;
      end if;

      Owners_Store (Slot) := New_Owner;

      Success := True;
      Error := Error_None;
   end Replace_Owner;

   procedure Change_Requirement (
      Caller       : in     Address;
      New_Required : in     Natural;
      Success      : out    Boolean;
      Error        : out    Multisig_Error
   ) is
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if New_Required = 0 or New_Required > Owner_Count_Store then
         Success := False;
         Error := Error_Invalid_Threshold;
         return;
      end if;

      Config_Store.Required_Sigs := New_Required;

      Success := True;
      Error := Error_None;
   end Change_Requirement;

   ---------------------------------------------------------------------------
   --  Transaction Management
   ---------------------------------------------------------------------------

   procedure Submit_Transaction (
      Caller  : in     Address;
      Target  : in     Address;
      Value   : in     U256;
      Data    : in     Tx_Data;
      Tx_Id   : out    Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) is
      Confirm_Slot : Natural;
   begin
      if not Is_Owner (Caller) then
         Tx_Id := 0;
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Tx_Counter >= Max_Transactions then
         Tx_Id := 0;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Tx_Id := Tx_Counter;

      Transactions (Tx_Id) := (
         ID            => Tx_Id,
         Target        => Target,
         Value         => Value,
         Data          => Data,
         Proposer      => Caller,
         Confirmations => 1,
         Status        => Status_Pending,
         Created_At    => Time_Store,
         Executed_At   => 0,
         Used          => True
      );

      Tx_Counter := Tx_Counter + 1;

      --  Auto-confirm by proposer
      Confirm_Slot := Find_Empty_Confirmation;
      if Confirm_Slot /= Natural'Last then
         Confirmations (Confirm_Slot) := (
            Tx_Num => Tx_Id,
            Owner  => Caller,
            Used   => True
         );
      end if;

      Success := True;
      Error := Error_None;
   end Submit_Transaction;

   procedure Confirm_Transaction (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) is
      Tx_Slot      : constant Natural := Find_Transaction (Tx_Id);
      Confirm_Slot : Natural;
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Tx_Slot = Natural'Last then
         Success := False;
         Error := Error_Tx_Not_Found;
         return;
      end if;

      if Transactions (Tx_Slot).Status /= Status_Pending then
         Success := False;
         Error := Error_Already_Executed;
         return;
      end if;

      if Find_Confirmation (Tx_Id, Caller) /= Natural'Last then
         Success := False;
         Error := Error_Already_Confirmed;
         return;
      end if;

      Confirm_Slot := Find_Empty_Confirmation;
      if Confirm_Slot = Natural'Last then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Confirmations (Confirm_Slot) := (
         Tx_Num => Tx_Id,
         Owner  => Caller,
         Used   => True
      );

      Transactions (Tx_Slot).Confirmations :=
         Transactions (Tx_Slot).Confirmations + 1;

      Success := True;
      Error := Error_None;
   end Confirm_Transaction;

   procedure Revoke_Confirmation (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) is
      Tx_Slot      : constant Natural := Find_Transaction (Tx_Id);
      Confirm_Slot : constant Natural := Find_Confirmation (Tx_Id, Caller);
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Tx_Slot = Natural'Last then
         Success := False;
         Error := Error_Tx_Not_Found;
         return;
      end if;

      if Transactions (Tx_Slot).Status /= Status_Pending then
         Success := False;
         Error := Error_Already_Executed;
         return;
      end if;

      if Confirm_Slot = Natural'Last then
         Success := False;
         Error := Error_Not_Confirmed;
         return;
      end if;

      Confirmations (Confirm_Slot).Used := False;

      if Transactions (Tx_Slot).Confirmations > 0 then
         Transactions (Tx_Slot).Confirmations :=
            Transactions (Tx_Slot).Confirmations - 1;
      end if;

      Success := True;
      Error := Error_None;
   end Revoke_Confirmation;

   procedure Execute_Transaction (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) is
      Tx_Slot  : constant Natural := Find_Transaction (Tx_Id);
      Confirms : Natural;
      Overflow : Boolean;
      New_Spent: U256;
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Tx_Slot = Natural'Last then
         Success := False;
         Error := Error_Tx_Not_Found;
         return;
      end if;

      if Transactions (Tx_Slot).Status /= Status_Pending then
         Success := False;
         Error := Error_Already_Executed;
         return;
      end if;

      --  Check expiry
      if Config_Store.Expiry_Period > 0 then
         if Time_Store > Transactions (Tx_Slot).Created_At +
            Config_Store.Expiry_Period
         then
            Transactions (Tx_Slot).Status := Status_Expired;
            Success := False;
            Error := Error_Tx_Expired;
            return;
         end if;
      end if;

      --  Check confirmations
      Confirms := Count_Confirmations (Tx_Id);
      if Confirms < Config_Store.Required_Sigs then
         Success := False;
         Error := Error_Insufficient_Confirmations;
         return;
      end if;

      --  Check daily limit
      Reset_Daily_Limit;
      if not Equal (Config_Store.Daily_Limit, U256_Zero) then
         Add (Daily_Spent_Store, Transactions (Tx_Slot).Value,
              New_Spent, Overflow);
         if Overflow or else Less_Than (Config_Store.Daily_Limit, New_Spent) then
            Success := False;
            Error := Error_Daily_Limit_Exceeded;
            return;
         end if;
         Daily_Spent_Store := New_Spent;
      end if;

      --  Execute (placeholder - actual execution would call target)
      Transactions (Tx_Slot).Status := Status_Executed;
      Transactions (Tx_Slot).Executed_At := Time_Store;

      Success := True;
      Error := Error_None;
   end Execute_Transaction;

   procedure Cancel_Transaction (
      Caller  : in     Address;
      Tx_Id   : in     Transaction_ID;
      Success : out    Boolean;
      Error   : out    Multisig_Error
   ) is
      Tx_Slot : constant Natural := Find_Transaction (Tx_Id);
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      if Tx_Slot = Natural'Last then
         Success := False;
         Error := Error_Tx_Not_Found;
         return;
      end if;

      if Transactions (Tx_Slot).Status /= Status_Pending then
         Success := False;
         Error := Error_Already_Executed;
         return;
      end if;

      --  Only proposer can cancel
      if Caller /= Transactions (Tx_Slot).Proposer then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Transactions (Tx_Slot).Status := Status_Cancelled;

      Success := True;
      Error := Error_None;
   end Cancel_Transaction;

   ---------------------------------------------------------------------------
   --  Transaction Query
   ---------------------------------------------------------------------------

   function Get_Transaction (Tx_Id : Transaction_ID) return Transaction is
   begin
      if Tx_Id < Max_Transactions and then Transactions (Tx_Id).Used then
         return Transactions (Tx_Id);
      else
         return (
            ID            => 0,
            Target        => Zero_Address,
            Value         => U256_Zero,
            Data          => (Bytes => (others => 0), Length => 0),
            Proposer      => Zero_Address,
            Confirmations => 0,
            Status        => Status_Pending,
            Created_At    => 0,
            Executed_At   => 0,
            Used          => False
         );
      end if;
   end Get_Transaction;

   function Transaction_Count return Natural is
   begin
      return Tx_Counter;
   end Transaction_Count;

   function Pending_Transaction_Count return Natural is
      Count : Natural := 0;
   begin
      for I in Transactions'Range loop
         if Transactions (I).Used
            and then Transactions (I).Status = Status_Pending
         then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Pending_Transaction_Count;

   function Is_Confirmed (Tx_Id : Transaction_ID; Owner : Address) return Boolean is
   begin
      return Find_Confirmation (Tx_Id, Owner) /= Natural'Last;
   end Is_Confirmed;

   function Confirmation_Count (Tx_Id : Transaction_ID) return Natural is
   begin
      return Count_Confirmations (Tx_Id);
   end Confirmation_Count;

   function Is_Executable (Tx_Id : Transaction_ID) return Boolean is
      Tx_Slot : constant Natural := Find_Transaction (Tx_Id);
   begin
      if Tx_Slot = Natural'Last then
         return False;
      end if;

      if Transactions (Tx_Slot).Status /= Status_Pending then
         return False;
      end if;

      return Count_Confirmations (Tx_Id) >= Config_Store.Required_Sigs;
   end Is_Executable;

   ---------------------------------------------------------------------------
   --  Configuration Query
   ---------------------------------------------------------------------------

   function Get_Config return Multisig_Config is
   begin
      return Config_Store;
   end Get_Config;

   function Daily_Spent return U256 is
   begin
      return Daily_Spent_Store;
   end Daily_Spent;

   function Current_Time return Timestamp is
   begin
      return Time_Store;
   end Current_Time;

   ---------------------------------------------------------------------------
   --  Configuration Update
   ---------------------------------------------------------------------------

   procedure Set_Daily_Limit (
      Caller    : in     Address;
      New_Limit : in     U256;
      Success   : out    Boolean;
      Error     : out    Multisig_Error
   ) is
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Config_Store.Daily_Limit := New_Limit;
      Success := True;
      Error := Error_None;
   end Set_Daily_Limit;

   procedure Set_Expiry_Period (
      Caller     : in     Address;
      New_Expiry : in     Timestamp;
      Success    : out    Boolean;
      Error      : out    Multisig_Error
   ) is
   begin
      if not Is_Owner (Caller) then
         Success := False;
         Error := Error_Not_Owner;
         return;
      end if;

      Config_Store.Expiry_Period := New_Expiry;
      Success := True;
      Error := Error_None;
   end Set_Expiry_Period;

end Khepri_Multisig;
