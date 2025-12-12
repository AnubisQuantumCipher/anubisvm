pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body CVM_Registry with
   SPARK_Mode => On
is

   --  Initialize registry to empty state
   procedure Initialize (
      Registry : out Registry_Array
   ) is
   begin
      for I in Registry'Range loop
         pragma Loop_Invariant (I >= Registry'First);
         pragma Loop_Invariant
           (for all J in Registry'First .. I - 1 => not Registry (J).Occupied);
         Registry (I) := Empty_Entry;
      end loop;
   end Initialize;

   --  Compare addresses (constant-time to prevent timing attacks)
   function Address_Equal (
      A : CVM_Address;
      B : CVM_Address
   ) return Boolean is
      Diff : Byte := 0;
   begin
      for I in A'Range loop
         pragma Loop_Invariant (I >= A'First);
         Diff := Diff or (A (I) xor B (I));
      end loop;
      return Diff = 0;
   end Address_Equal;

   --  Compare code hashes (constant-time)
   function Code_Equal (
      A : Code_Hash;
      B : Code_Hash
   ) return Boolean is
      Diff : Byte := 0;
   begin
      for I in A'Range loop
         pragma Loop_Invariant (I >= A'First);
         Diff := Diff or (A (I) xor B (I));
      end loop;
      return Diff = 0;
   end Code_Equal;

   --  Register a new CVM
   procedure Register_CVM (
      Registry : in Out Registry_Array;
      Reg      : CVM_Registration;
      Success  : out Boolean
   ) is
      Desc : CVM_Descriptor;
      Free_Index : Registry_Index := 0;
      Found_Free : Boolean := False;
   begin
      Success := False;

      --  Get CVM descriptor
      if Reg.Get_Descriptor = null then
         return;
      end if;

      Desc := Reg.Get_Descriptor.all;

      --  Check if CVM already registered (by address)
      for I in Registry'Range loop
         pragma Loop_Invariant (I >= Registry'First);
         if Registry (I).Occupied then
            if Address_Equal (Registry (I).Descriptor.Info.Addr, Desc.Info.Addr) then
               --  Already registered
               return;
            end if;
         end if;
      end loop;

      --  Find free slot
      for I in Registry'Range loop
         pragma Loop_Invariant (I >= Registry'First);
         if not Registry (I).Occupied then
            Free_Index := I;
            Found_Free := True;
            exit;
         end if;
      end loop;

      if not Found_Free then
         --  Registry full
         return;
      end if;

      --  Initialize the CVM
      if Reg.Init /= null then
         declare
            Init_Params : constant Byte_Array (0 .. 0) := (others => 0);
            Init_State : State_Array;
            Init_Success : Boolean;
         begin
            Reg.Init.all (Init_Params, Init_State, Init_Success);
            if not Init_Success then
               return;
            end if;
            Registry (Free_Index).State := Init_State;
         end;
      else
         Registry (Free_Index).State := Empty_State;
      end if;

      --  Register the CVM
      Registry (Free_Index).Registration := Reg;
      Registry (Free_Index).Descriptor := Desc;
      Registry (Free_Index).Occupied := True;
      Registry (Free_Index).Exec_Count := 0;

      Success := True;
   end Register_CVM;

   --  Lookup by address
   procedure Lookup_By_Address (
      Registry : Registry_Array;
      Addr     : CVM_Address;
      Index    : out Registry_Index;
      Found    : out Boolean
   ) is
   begin
      Index := 0;
      Found := False;

      for I in Registry'Range loop
         pragma Loop_Invariant (I >= Registry'First);
         pragma Loop_Invariant (not Found);

         if Registry (I).Occupied then
            if Address_Equal (Registry (I).Descriptor.Info.Addr, Addr) then
               Index := I;
               Found := True;
               return;
            end if;
         end if;
      end loop;
   end Lookup_By_Address;

   --  Lookup by code hash
   procedure Lookup_By_Code (
      Registry : Registry_Array;
      Code     : Code_Hash;
      Index    : out Registry_Index;
      Found    : out Boolean
   ) is
   begin
      Index := 0;
      Found := False;

      for I in Registry'Range loop
         pragma Loop_Invariant (I >= Registry'First);
         pragma Loop_Invariant (not Found);

         if Registry (I).Occupied then
            if Code_Equal (Registry (I).Descriptor.Info.Code, Code) then
               Index := I;
               Found := True;
               return;
            end if;
         end if;
      end loop;
   end Lookup_By_Code;

   --  Get CVM descriptor
   procedure Get_Descriptor (
      Registry : Registry_Array;
      Index    : Registry_Index;
      Desc     : out CVM_Descriptor
   ) is
   begin
      Desc := Registry (Index).Descriptor;
   end Get_Descriptor;

   --  Get CVM state
   procedure Get_State (
      Registry : Registry_Array;
      Index    : Registry_Index;
      State    : out State_Array
   ) is
   begin
      State := Registry (Index).State;
   end Get_State;

   --  Set CVM state
   procedure Set_State (
      Registry : in Out Registry_Array;
      Index    : Registry_Index;
      State    : State_Array
   ) is
   begin
      Registry (Index).State := State;
   end Set_State;

   --  Count registered CVMs
   function CVM_Count (
      Registry : Registry_Array
   ) return Natural is
      Count : Natural := 0;
   begin
      for I in Registry'Range loop
         pragma Loop_Invariant (I >= Registry'First);
         pragma Loop_Invariant (Count <= I - Registry'First);

         if Registry (I).Occupied then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end CVM_Count;

   --  Check if CVM is active
   function Is_Active (
      Registry : Registry_Array;
      Index    : Registry_Index
   ) return Boolean is
   begin
      return Registry (Index).Descriptor.Info.Active;
   end Is_Active;

   --  Set CVM active status
   procedure Set_Active (
      Registry : in Out Registry_Array;
      Index    : Registry_Index;
      Active   : Boolean
   ) is
   begin
      Registry (Index).Descriptor.Info.Active := Active;
   end Set_Active;

   --  Increment execution counter
   procedure Inc_Exec_Count (
      Registry : in Out Registry_Array;
      Index    : Registry_Index
   ) is
   begin
      if Registry (Index).Exec_Count < Natural'Last then
         Registry (Index).Exec_Count := Registry (Index).Exec_Count + 1;
      end if;
   end Inc_Exec_Count;

end CVM_Registry;
