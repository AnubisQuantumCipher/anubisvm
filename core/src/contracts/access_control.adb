pragma SPARK_Mode (On);

package body Access_Control with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Roles_Equal (A, B : Role_ID) return Boolean is
   begin
      for I in Role_ID'Range loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Roles_Equal;

   function Addresses_Equal (A, B : Address) return Boolean is
   begin
      for I in Address'Range loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Addresses_Equal;

   ---------------------------------------------------------------------------
   --  Core Functions Implementation
   ---------------------------------------------------------------------------

   function Has_Role (
      State   : AC_State;
      Role    : Role_ID;
      Account : Address
   ) return Boolean is
   begin
      for I in 0 .. State.Count - 1 loop
         if State.Assignments (I).Active
            and then Roles_Equal (State.Assignments (I).Role, Role)
            and then Addresses_Equal (State.Assignments (I).Account, Account)
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Role;

   procedure Grant_Role (
      State   : in out AC_State;
      Caller  : Address;
      Role    : Role_ID;
      Account : Address;
      Success : out Boolean
   ) is
      Admin_Role : constant Role_ID := Get_Role_Admin (State, Role);
   begin
      Success := False;

      --  Check caller has admin permission for this role
      if not Has_Role (State, Admin_Role, Caller) then
         return;
      end if;

      --  Check if already has role
      if Has_Role (State, Role, Account) then
         Success := True;
         return;
      end if;

      --  Check space available
      if State.Count >= Max_Role_Assignments then
         return;
      end if;

      --  Add role assignment
      State.Assignments (State.Count) := (
         Role    => Role,
         Account => Account,
         Active  => True
      );
      State.Count := State.Count + 1;
      Success := True;
   end Grant_Role;

   procedure Revoke_Role (
      State   : in out AC_State;
      Caller  : Address;
      Role    : Role_ID;
      Account : Address;
      Success : out Boolean
   ) is
      Admin_Role : constant Role_ID := Get_Role_Admin (State, Role);
   begin
      Success := False;

      --  Check caller has admin permission for this role
      if not Has_Role (State, Admin_Role, Caller) then
         return;
      end if;

      --  Find and deactivate the role assignment
      for I in 0 .. State.Count - 1 loop
         if State.Assignments (I).Active
            and then Roles_Equal (State.Assignments (I).Role, Role)
            and then Addresses_Equal (State.Assignments (I).Account, Account)
         then
            State.Assignments (I).Active := False;
            Success := True;
            return;
         end if;
      end loop;
   end Revoke_Role;

   procedure Renounce_Role (
      State   : in out AC_State;
      Caller  : Address;
      Role    : Role_ID;
      Success : out Boolean
   ) is
   begin
      Success := False;

      --  Find and deactivate the caller's role assignment
      for I in 0 .. State.Count - 1 loop
         if State.Assignments (I).Active
            and then Roles_Equal (State.Assignments (I).Role, Role)
            and then Addresses_Equal (State.Assignments (I).Account, Caller)
         then
            State.Assignments (I).Active := False;
            Success := True;
            return;
         end if;
      end loop;
   end Renounce_Role;

   function Get_Role_Admin (
      State : AC_State;
      Role  : Role_ID
   ) return Role_ID is
   begin
      --  Look up role admin mapping
      for I in 0 .. State.Admin_Count - 1 loop
         if State.Role_Admins (I).Active
            and then Roles_Equal (State.Role_Admins (I).Role, Role)
         then
            return State.Role_Admins (I).Admin_Role;
         end if;
      end loop;

      --  Default: all roles administered by DEFAULT_ADMIN_ROLE
      return Default_Admin_Role;
   end Get_Role_Admin;

   procedure Set_Role_Admin (
      State     : in out AC_State;
      Caller    : Address;
      Role      : Role_ID;
      Admin_Role : Role_ID;
      Success   : out Boolean
   ) is
   begin
      Success := False;

      --  Only DEFAULT_ADMIN_ROLE holders can set role admins
      if not Has_Role (State, Default_Admin_Role, Caller) then
         return;
      end if;

      --  Check if role admin mapping already exists
      for I in 0 .. State.Admin_Count - 1 loop
         if State.Role_Admins (I).Active
            and then Roles_Equal (State.Role_Admins (I).Role, Role)
         then
            --  Update existing mapping
            State.Role_Admins (I).Admin_Role := Admin_Role;
            Success := True;
            return;
         end if;
      end loop;

      --  Check for inactive slot to reuse
      for I in 0 .. State.Admin_Count - 1 loop
         if not State.Role_Admins (I).Active then
            State.Role_Admins (I) := (
               Role       => Role,
               Admin_Role => Admin_Role,
               Active     => True
            );
            Success := True;
            return;
         end if;
      end loop;

      --  Check space available for new mapping
      if State.Admin_Count >= Max_Role_Admins then
         return;
      end if;

      --  Add new role admin mapping
      State.Role_Admins (State.Admin_Count) := (
         Role       => Role,
         Admin_Role => Admin_Role,
         Active     => True
      );
      State.Admin_Count := State.Admin_Count + 1;
      Success := True;
   end Set_Role_Admin;

   ---------------------------------------------------------------------------
   --  Modifier-Style Helpers Implementation
   ---------------------------------------------------------------------------

   function Only_Owner (
      State  : AC_State;
      Caller : Address
   ) return Boolean is
   begin
      return Has_Role (State, Default_Admin_Role, Caller);
   end Only_Owner;

   function Only_Minter (
      State  : AC_State;
      Caller : Address
   ) return Boolean is
   begin
      return Has_Role (State, Minter_Role, Caller);
   end Only_Minter;

   function Only_Burner (
      State  : AC_State;
      Caller : Address
   ) return Boolean is
   begin
      return Has_Role (State, Burner_Role, Caller);
   end Only_Burner;

   function Only_Pauser (
      State  : AC_State;
      Caller : Address
   ) return Boolean is
   begin
      return Has_Role (State, Pauser_Role, Caller);
   end Only_Pauser;

   ---------------------------------------------------------------------------
   --  Initialization Implementation
   ---------------------------------------------------------------------------

   procedure Init_Access_Control (
      State        : out AC_State;
      Default_Admin : Address
   ) is
   begin
      --  Initialize empty assignments
      State.Assignments := (others => (
         Role    => (others => 0),
         Account => (others => 0),
         Active  => False
      ));
      State.Count := 0;

      --  Initialize empty role admin mappings
      State.Role_Admins := (others => (
         Role       => (others => 0),
         Admin_Role => (others => 0),
         Active     => False
      ));
      State.Admin_Count := 0;

      --  Grant DEFAULT_ADMIN_ROLE to initial admin
      State.Assignments (0) := (
         Role    => Default_Admin_Role,
         Account => Default_Admin,
         Active  => True
      );
      State.Count := 1;
   end Init_Access_Control;

   ---------------------------------------------------------------------------
   --  Utilities Implementation
   ---------------------------------------------------------------------------

   function Count_Roles (
      State   : AC_State;
      Account : Address
   ) return Natural is
      Count : Natural := 0;
   begin
      for I in 0 .. State.Count - 1 loop
         if State.Assignments (I).Active
            and then Addresses_Equal (State.Assignments (I).Account, Account)
         then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Roles;

   procedure Get_Role_By_Index (
      State   : AC_State;
      Account : Address;
      Index   : Natural;
      Role    : out Role_ID;
      Found   : out Boolean
   ) is
      Count : Natural := 0;
   begin
      Role := (others => 0);
      Found := False;

      for I in 0 .. State.Count - 1 loop
         if State.Assignments (I).Active
            and then Addresses_Equal (State.Assignments (I).Account, Account)
         then
            if Count = Index then
               Role := State.Assignments (I).Role;
               Found := True;
               return;
            end if;
            Count := Count + 1;
         end if;
      end loop;
   end Get_Role_By_Index;

end Access_Control;
