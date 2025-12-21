--  PQ_Vault: Post-Quantum Password Manager - Implementation
pragma SPARK_Mode (On);

package body PQ_Vault_Real with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Constant-time account ID comparison
   function Accounts_Equal (A, B : Account_ID) return Boolean
   with Global => null
   is
      Diff : Unsigned_8 := 0;
   begin
      for I in Account_ID_Index loop
         Diff := Diff or (A (I) xor B (I));
      end loop;
      return Diff = 0;
   end Accounts_Equal;

   --  Check if account is zero (invalid)
   function Is_Zero_Account (A : Account_ID) return Boolean
   with Global => null
   is
      Result : Boolean := True;
   begin
      for I in Account_ID_Index loop
         if A (I) /= 0 then
            Result := False;
            exit;
         end if;
      end loop;
      return Result;
   end Is_Zero_Account;

   --  Find first available entry slot
   procedure Find_Free_Slot (
      State : Vault_State;
      Slot  : out Entry_Index;
      Found : out Boolean)
   with Global => null
   is
   begin
      Found := False;
      Slot := 0;
      for I in Entry_Index loop
         if not State.Headers (I).Active then
            Slot := I;
            Found := True;
            exit;
         end if;
      end loop;
   end Find_Free_Slot;

   --  Find first available share slot
   procedure Find_Free_Share_Slot (
      Header : Entry_Header;
      Slot   : out Share_Index;
      Found  : out Boolean)
   with Global => null
   is
   begin
      Found := False;
      Slot := 0;
      for I in Share_Index loop
         if not Header.Shared_With (I).Active then
            Slot := I;
            Found := True;
            exit;
         end if;
      end loop;
   end Find_Free_Share_Slot;

   --  Check if entity type is allowed by restriction
   function Entity_Allowed (
      Restriction : Entity_Restriction;
      Entity      : Entity_Type) return Boolean
   with Global => null
   is
   begin
      case Restriction is
         when Allow_All =>
            return True;
         when Users_Only =>
            return Entity = User;
         when Contracts_Only =>
            return Entity = Contract;
         when Validators_Only =>
            return Entity = Validator;
      end case;
   end Entity_Allowed;

   ---------------------------------------------------------------------------
   --  Vault Management Implementation
   ---------------------------------------------------------------------------

   procedure Initialize (
      State        : in Out Vault_State;
      Owner        : Account_ID;
      Owner_Entity : Entity_Type;
      Owner_Network : Network_Type;
      Name_Hash    : Account_ID;
      Timestamp    : Unsigned_64;
      Result       : out Result_Code)
   is
   begin
      if State.Initialized then
         Result := Error_Already_Initialized;
         return;
      end if;

      --  Validate owner is not zero address
      if Is_Zero_Account (Owner) then
         Result := Error_Invalid_Data;
         return;
      end if;

      State := (
         Initialized           => True,
         Owner                 => Owner,
         Owner_Entity          => Owner_Entity,
         Owner_Network         => Owner_Network,
         Name_Hash             => Name_Hash,
         Created_At            => Timestamp,
         Modified_At           => Timestamp,
         Headers               => (others => Null_Header),
         Entry_Count           => 0,
         Recovery              => (others => Null_Recovery),
         Recovery_Pending      => False,
         Recovery_Requested_At => 0,
         Recovery_Requester    => (others => 0),
         Version               => 1,
         Access_Count          => 0,
         Last_Access           => Timestamp
      );

      Result := Success;
   end Initialize;

   function Is_Owner (
      State          : Vault_State;
      Caller         : Account_ID;
      Caller_Network : Network_Type) return Boolean
   is
   begin
      --  Must match both account AND network
      return Accounts_Equal (State.Owner, Caller) and
             State.Owner_Network = Caller_Network;
   end Is_Owner;

   function Has_Permission (
      State          : Vault_State;
      Index          : Entry_Index;
      Caller         : Account_ID;
      Caller_Entity  : Entity_Type;
      Caller_Network : Network_Type;
      Required       : Permission_Level) return Boolean
   is
      Header : Entry_Header renames State.Headers (Index);
   begin
      --  Owner always has admin permission
      if Is_Owner (State, Caller, Caller_Network) then
         return True;
      end if;

      --  Entry must be active
      if not Header.Active then
         return False;
      end if;

      --  Check network binding
      if Header.Bound_Network /= Caller_Network then
         return False;
      end if;

      --  Check entity type restriction
      if not Entity_Allowed (Header.Restriction, Caller_Entity) then
         return False;
      end if;

      --  Check shared access list
      for I in Share_Index loop
         if Header.Shared_With (I).Active and then
            Accounts_Equal (Header.Shared_With (I).Grantee, Caller) and then
            Header.Shared_With (I).Network = Caller_Network
         then
            --  Check permission level (higher includes lower)
            return Header.Shared_With (I).Permission >= Required;
         end if;
      end loop;

      return False;
   end Has_Permission;

   ---------------------------------------------------------------------------
   --  Entry Operations Implementation
   ---------------------------------------------------------------------------

   procedure Add_Entry (
      State          : in Out Vault_State;
      Caller         : Account_ID;
      Caller_Network : Network_Type;
      Kind           : Entry_Kind;
      Label_Hash     : Account_ID;
      Data_Hash      : Account_ID;
      Category       : Unsigned_8;
      Bound_Network  : Network_Type;
      Restriction    : Entity_Restriction;
      Timestamp      : Unsigned_64;
      Index          : out Entry_Index;
      Result         : out Result_Code)
   is
      Slot  : Entry_Index;
      Found : Boolean;
   begin
      Index := 0;

      --  Verify ownership AND network match
      if not Is_Owner (State, Caller, Caller_Network) then
         Result := Error_Not_Owner;
         return;
      end if;

      --  Check capacity
      if State.Entry_Count >= Max_Entries then
         Result := Error_Vault_Full;
         return;
      end if;

      --  Find free slot
      Find_Free_Slot (State, Slot, Found);
      if not Found then
         Result := Error_Vault_Full;
         return;
      end if;

      --  Create entry with network binding
      State.Headers (Slot) := (
         Active        => True,
         Kind          => Kind,
         Created       => Timestamp,
         Modified      => Timestamp,
         Data_Hash     => Data_Hash,
         Label_Hash    => Label_Hash,
         Favorite      => False,
         Category      => Category,
         Shared_With   => (others => Null_Shared_Access),
         Restriction   => Restriction,
         Bound_Network => Bound_Network
      );

      State.Entry_Count := State.Entry_Count + 1;
      State.Modified_At := Timestamp;
      State.Version := State.Version + 1;

      Index := Slot;
      Result := Success;
   end Add_Entry;

   procedure Get_Entry (
      State          : Vault_State;
      Index          : Entry_Index;
      Caller         : Account_ID;
      Caller_Entity  : Entity_Type;
      Caller_Network : Network_Type;
      Header         : out Entry_Header;
      Result         : out Result_Code)
   is
   begin
      Header := Null_Header;

      --  Check entry exists
      if not State.Headers (Index).Active then
         Result := Error_Entry_Not_Found;
         return;
      end if;

      --  Check permission (view level required)
      if not Has_Permission (State, Index, Caller, Caller_Entity,
                             Caller_Network, Perm_View)
      then
         --  Determine specific error
         if State.Headers (Index).Bound_Network /= Caller_Network then
            Result := Error_Network_Mismatch;
         elsif not Entity_Allowed (State.Headers (Index).Restriction,
                                   Caller_Entity)
         then
            Result := Error_Entity_Not_Allowed;
         else
            Result := Error_Permission_Denied;
         end if;
         return;
      end if;

      Header := State.Headers (Index);
      Result := Success;
   end Get_Entry;

   ---------------------------------------------------------------------------
   --  Sharing Operations Implementation
   ---------------------------------------------------------------------------

   procedure Share_Entry (
      State           : in Out Vault_State;
      Index           : Entry_Index;
      Caller          : Account_ID;
      Caller_Network  : Network_Type;
      Grantee         : Account_ID;
      Grantee_Entity  : Entity_Type;
      Grantee_Network : Network_Type;
      Permission      : Permission_Level;
      Expires_At      : Unsigned_64;
      Timestamp       : Unsigned_64;
      Result          : out Result_Code)
   is
      Slot  : Share_Index;
      Found : Boolean;
   begin
      --  Only owner can share
      if not Is_Owner (State, Caller, Caller_Network) then
         Result := Error_Not_Owner;
         return;
      end if;

      --  Entry must exist
      if not State.Headers (Index).Active then
         Result := Error_Entry_Not_Found;
         return;
      end if;

      --  Grantee network must match entry's bound network
      if Grantee_Network /= State.Headers (Index).Bound_Network then
         Result := Error_Network_Mismatch;
         return;
      end if;

      --  Grantee entity must be allowed
      if not Entity_Allowed (State.Headers (Index).Restriction, Grantee_Entity)
      then
         Result := Error_Entity_Not_Allowed;
         return;
      end if;

      --  Check not already shared with this address
      for I in Share_Index loop
         if State.Headers (Index).Shared_With (I).Active and then
            Accounts_Equal (State.Headers (Index).Shared_With (I).Grantee,
                           Grantee)
         then
            Result := Error_Already_Shared;
            return;
         end if;
      end loop;

      --  Find free share slot
      Find_Free_Share_Slot (State.Headers (Index), Slot, Found);
      if not Found then
         Result := Error_Share_Limit_Reached;
         return;
      end if;

      --  Add share entry with full AAS-001 info
      State.Headers (Index).Shared_With (Slot) := (
         Active       => True,
         Grantee      => Grantee,
         Entity       => Grantee_Entity,
         Network      => Grantee_Network,
         Permission   => Permission,
         Granted_At   => Timestamp,
         Expires_At   => Expires_At,
         Access_Count => 0,
         Last_Access  => 0
      );

      State.Headers (Index).Modified := Timestamp;
      State.Modified_At := Timestamp;
      State.Version := State.Version + 1;

      Result := Success;
   end Share_Entry;

   procedure Revoke_Share (
      State          : in Out Vault_State;
      Index          : Entry_Index;
      Caller         : Account_ID;
      Caller_Network : Network_Type;
      Grantee        : Account_ID;
      Timestamp      : Unsigned_64;
      Result         : out Result_Code)
   is
      Found : Boolean := False;
   begin
      --  Only owner can revoke
      if not Is_Owner (State, Caller, Caller_Network) then
         Result := Error_Not_Owner;
         return;
      end if;

      --  Entry must exist
      if not State.Headers (Index).Active then
         Result := Error_Entry_Not_Found;
         return;
      end if;

      --  Find and remove share
      for I in Share_Index loop
         if State.Headers (Index).Shared_With (I).Active and then
            Accounts_Equal (State.Headers (Index).Shared_With (I).Grantee,
                           Grantee)
         then
            State.Headers (Index).Shared_With (I) := Null_Shared_Access;
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Result := Error_Entry_Not_Found;
         return;
      end if;

      State.Headers (Index).Modified := Timestamp;
      State.Modified_At := Timestamp;
      State.Version := State.Version + 1;

      Result := Success;
   end Revoke_Share;

   ---------------------------------------------------------------------------
   --  Recovery Operations Implementation
   ---------------------------------------------------------------------------

   procedure Add_Recovery_Contact (
      State           : in Out Vault_State;
      Caller          : Account_ID;
      Caller_Network  : Network_Type;
      Contact         : Account_ID;
      Contact_Network : Network_Type;
      Delay_Hours     : Unsigned_32;
      Timestamp       : Unsigned_64;
      Result          : out Result_Code)
   is
      Found : Boolean := False;
      Slot  : Recovery_Index := 0;
   begin
      --  Only owner can add recovery contacts
      if not Is_Owner (State, Caller, Caller_Network) then
         Result := Error_Not_Owner;
         return;
      end if;

      --  Recovery contact must be on same network
      if Contact_Network /= State.Owner_Network then
         Result := Error_Network_Mismatch;
         return;
      end if;

      --  Contact cannot be zero
      if Is_Zero_Account (Contact) then
         Result := Error_Invalid_Data;
         return;
      end if;

      --  Find free slot
      for I in Recovery_Index loop
         if not State.Recovery (I).Active then
            Slot := I;
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Result := Error_Share_Limit_Reached;
         return;
      end if;

      State.Recovery (Slot) := (
         Active      => True,
         Contact     => Contact,
         Entity      => User,  --  Recovery contacts must be users
         Network     => Contact_Network,
         Added_At    => Timestamp,
         Delay_Hours => Delay_Hours
      );

      State.Modified_At := Timestamp;
      State.Version := State.Version + 1;

      Result := Success;
   end Add_Recovery_Contact;

   procedure Request_Recovery (
      State            : in Out Vault_State;
      Requester        : Account_ID;
      Requester_Network : Network_Type;
      Timestamp        : Unsigned_64;
      Result           : out Result_Code)
   is
      Is_Valid_Contact : Boolean := False;
   begin
      --  Cannot request if recovery already pending
      if State.Recovery_Pending then
         Result := Error_Recovery_Pending;
         return;
      end if;

      --  Must be on same network as vault
      if Requester_Network /= State.Owner_Network then
         Result := Error_Network_Mismatch;
         return;
      end if;

      --  Requester must be a registered recovery contact
      for I in Recovery_Index loop
         if State.Recovery (I).Active and then
            Accounts_Equal (State.Recovery (I).Contact, Requester) and then
            State.Recovery (I).Network = Requester_Network
         then
            Is_Valid_Contact := True;
            exit;
         end if;
      end loop;

      if not Is_Valid_Contact then
         Result := Error_Permission_Denied;
         return;
      end if;

      --  Start recovery process
      State.Recovery_Pending := True;
      State.Recovery_Requested_At := Timestamp;
      State.Recovery_Requester := Requester;
      State.Modified_At := Timestamp;
      State.Version := State.Version + 1;

      Result := Success;
   end Request_Recovery;

   procedure Complete_Recovery (
      State            : in Out Vault_State;
      Requester        : Account_ID;
      Requester_Network : Network_Type;
      Timestamp        : Unsigned_64;
      Result           : out Result_Code)
   is
      Delay_Seconds : Unsigned_64 := 0;
      Min_Delay     : Unsigned_32 := Unsigned_32'Last;
   begin
      --  Recovery must be pending
      if not State.Recovery_Pending then
         Result := Error_Not_Initialized;
         return;
      end if;

      --  Must be the same requester
      if not Accounts_Equal (State.Recovery_Requester, Requester) then
         Result := Error_Permission_Denied;
         return;
      end if;

      --  Must be on same network
      if Requester_Network /= State.Owner_Network then
         Result := Error_Network_Mismatch;
         return;
      end if;

      --  Find the delay for this contact
      for I in Recovery_Index loop
         if State.Recovery (I).Active and then
            Accounts_Equal (State.Recovery (I).Contact, Requester)
         then
            if State.Recovery (I).Delay_Hours < Min_Delay then
               Min_Delay := State.Recovery (I).Delay_Hours;
            end if;
         end if;
      end loop;

      --  Check if delay has passed (hours to seconds)
      Delay_Seconds := Unsigned_64 (Min_Delay) * 3600;
      if Timestamp < State.Recovery_Requested_At + Delay_Seconds then
         Result := Error_Recovery_Delay;
         return;
      end if;

      --  Transfer ownership to recovery contact
      State.Owner := Requester;
      State.Owner_Entity := User;
      --  Network stays the same
      State.Recovery_Pending := False;
      State.Recovery_Requested_At := 0;
      State.Recovery_Requester := (others => 0);
      State.Modified_At := Timestamp;
      State.Version := State.Version + 1;

      Result := Success;
   end Complete_Recovery;

   ---------------------------------------------------------------------------
   --  Query Functions Implementation
   ---------------------------------------------------------------------------

   function Get_Entry_Count (State : Vault_State) return Entry_Count_Type is
   begin
      return State.Entry_Count;
   end Get_Entry_Count;

   function Get_Owner_Network (State : Vault_State) return Network_Type is
   begin
      return State.Owner_Network;
   end Get_Owner_Network;

   function Get_Owner_Entity (State : Vault_State) return Entity_Type is
   begin
      return State.Owner_Entity;
   end Get_Owner_Entity;

end PQ_Vault_Real;
