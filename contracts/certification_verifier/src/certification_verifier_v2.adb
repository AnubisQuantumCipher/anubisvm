pragma SPARK_Mode (On);

package body Certification_Verifier_V2 with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  Contract Lifecycle
   ---------------------------------------------------------------------------

   procedure Initialize (
      State : out Contract_State;
      Admin : Address
   ) is
   begin
      State := Empty_State;
      State.Admin := Admin;
      State.Initialized := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Administrative Functions
   ---------------------------------------------------------------------------

   procedure Register_Certification (
      State         : in out Contract_State;
      Contract_Addr : in     Address;
      Level         : in     Cert_Level;
      Proof_Hash    : in     Hash_256;
      Code_Hash     : in     Hash_256;
      Certifier     : in     Address;
      Timestamp     : in     Uint256;
      Status        : out    Error_Code
   ) is
      Index : Contract_Index;
   begin
      --  Check if already registered (update existing entry)
      Index := Find_Entry (State, Contract_Addr);

      --  If not found, find empty slot
      if not State.Entries (Index).Is_Valid then
         --  Check capacity
         if State.Total_Contracts >= Max_Contracts then
            Status := Contract_Error;
            return;
         end if;

         --  Find first empty slot
         for I in Contract_Index loop
            if not State.Entries (I).Is_Valid then
               Index := I;
               exit;
            end if;
         end loop;

         State.Total_Contracts := State.Total_Contracts + 1;
      end if;

      --  Register/update certification
      State.Entries (Index) := (
         Contract_Addr => Contract_Addr,
         Level         => Level,
         Proof_Hash    => Proof_Hash,
         Code_Hash     => Code_Hash,
         Timestamp     => Timestamp,
         Certifier     => Certifier,
         Is_Revoked    => False,
         Is_Valid      => True
      );

      Status := No_Error;
   end Register_Certification;

   procedure Revoke_Certification (
      State         : in out Contract_State;
      Contract_Addr : in     Address;
      Caller        : in     Address;
      Status        : out    Error_Code
   ) is
      Index : Contract_Index;
   begin
      --  Only admin can revoke
      if not Address_Equal (State.Admin, Caller) then
         Status := Unauthorized;
         return;
      end if;

      --  Find entry
      Index := Find_Entry (State, Contract_Addr);
      if not State.Entries (Index).Is_Valid then
         Status := Not_Found;
         return;
      end if;

      --  Mark as revoked
      State.Entries (Index).Is_Revoked := True;
      Status := No_Error;
   end Revoke_Certification;

   procedure Set_Admin (
      State     : in out Contract_State;
      New_Admin : in     Address;
      Caller    : in     Address;
      Status    : out    Error_Code
   ) is
   begin
      --  Only current admin can change admin
      if not Address_Equal (State.Admin, Caller) then
         Status := Unauthorized;
         return;
      end if;

      State.Admin := New_Admin;
      Status := No_Error;
   end Set_Admin;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Level (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Cert_Level is
      Index : constant Contract_Index := Find_Entry (State, Contract_Addr);
   begin
      if State.Entries (Index).Is_Valid and then
         not State.Entries (Index).Is_Revoked
      then
         return State.Entries (Index).Level;
      else
         return Level_None;
      end if;
   end Get_Level;

   function Get_Proof_Hash (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Hash_256 is
      Index : constant Contract_Index := Find_Entry (State, Contract_Addr);
   begin
      if State.Entries (Index).Is_Valid then
         return State.Entries (Index).Proof_Hash;
      else
         return (others => 0);
      end if;
   end Get_Proof_Hash;

   function Get_Code_Hash (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Hash_256 is
      Index : constant Contract_Index := Find_Entry (State, Contract_Addr);
   begin
      if State.Entries (Index).Is_Valid then
         return State.Entries (Index).Code_Hash;
      else
         return (others => 0);
      end if;
   end Get_Code_Hash;

   function Get_Discount (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Uint256 is
      Level : constant Cert_Level := Get_Level (State, Contract_Addr);
   begin
      return Level_To_Discount (Level);
   end Get_Discount;

   function Has_Minimum_Level (
      State         : Contract_State;
      Contract_Addr : Address;
      Min_Level     : Cert_Level
   ) return Boolean is
      Level : constant Cert_Level := Get_Level (State, Contract_Addr);
   begin
      return Level >= Min_Level;
   end Has_Minimum_Level;

   function Verify_Proof (
      State         : Contract_State;
      Contract_Addr : Address;
      Proof_Data    : Hash_256
   ) return Boolean is
      Stored_Hash : constant Hash_256 := Get_Proof_Hash (State, Contract_Addr);
   begin
      return Hash_Equal (Stored_Hash, Proof_Data);
   end Verify_Proof;

   function Get_Admin (State : Contract_State) return Address is
      (State.Admin);

   function Get_Total_Contracts (State : Contract_State) return Natural is
      (State.Total_Contracts);

   ---------------------------------------------------------------------------
   --  Private Helpers
   ---------------------------------------------------------------------------

   function Find_Entry (
      State         : Contract_State;
      Contract_Addr : Address
   ) return Contract_Index is
   begin
      --  Linear search for contract address
      for I in Contract_Index loop
         if State.Entries (I).Is_Valid and then
            Address_Equal (State.Entries (I).Contract_Addr, Contract_Addr)
         then
            return I;
         end if;
      end loop;

      --  Not found, return index 0 (will be invalid)
      return 0;
   end Find_Entry;

   function Level_To_Discount (Level : Cert_Level) return Uint256 is
   begin
      if Khepri_Types."=" (Level, Level_Platinum) then
         return Discount_Platinum;
      elsif Khepri_Types."=" (Level, Level_Gold) then
         return Discount_Gold;
      elsif Khepri_Types."=" (Level, Level_Silver) then
         return Discount_Silver;
      elsif Khepri_Types."=" (Level, Level_Bronze) then
         return Discount_Bronze;
      else
         return Discount_None;
      end if;
   end Level_To_Discount;

end Certification_Verifier_V2;
