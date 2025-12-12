--  KHEPRI Contract Registry Implementation
pragma SPARK_Mode (On);

package body Khepri_Registry with
   SPARK_Mode => On,
   Refined_State => (Registry_State => (
      Entries, Entry_Count, Certifiers, Certifier_Count, Admin_Addr, Current_Time))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   type Entry_Index is range 0 .. Max_Contracts - 1;
   type Entry_Array is array (Entry_Index) of Registry_Entry;

   Max_Certifiers : constant := 100;
   type Certifier_Index is range 0 .. Max_Certifiers - 1;
   type Certifier_Array is array (Certifier_Index) of Contract_Address;

   Entries         : Entry_Array := (others => Null_Entry);
   Entry_Count     : Natural := 0;
   Certifiers      : Certifier_Array := (others => (others => 0));
   Certifier_Count : Natural := 0;
   Admin_Addr      : Contract_Address := (others => 0);

   --  Monotonic timestamp (set by block production, used for registry entries)
   Current_Time    : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Entry (Contract_Addr : Contract_Address) return Natural
      with Post => Find_Entry'Result = Natural'Last or else
                   Find_Entry'Result <= Max_Contracts - 1
   is
   begin
      for I in Entry_Index loop
         if Entries (I).Contract_Addr = Contract_Addr and then
            Entries (I).Registered_At > 0
         then
            return Natural (I);
         end if;
      end loop;
      return Natural'Last;
   end Find_Entry;

   function Find_Empty_Slot return Natural
      with Post => Find_Empty_Slot'Result = Natural'Last or else
                   Find_Empty_Slot'Result <= Max_Contracts - 1
   is
   begin
      for I in Entry_Index loop
         if Entries (I).Registered_At = 0 then
            return Natural (I);
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Slot;

   function Level_To_Discount (Level : Certification_Level) return Natural
      with Post => Level_To_Discount'Result <= 100
   is
   begin
      case Level is
         when Level_None     => return Gas_Discount_None;
         when Level_Bronze   => return Gas_Discount_Bronze;
         when Level_Silver   => return Gas_Discount_Silver;
         when Level_Gold     => return Gas_Discount_Gold;
         when Level_Platinum => return Gas_Discount_Platinum;
      end case;
   end Level_To_Discount;

   ---------------------------------------------------------------------------
   --  Registration Operations
   ---------------------------------------------------------------------------

   procedure Register (
      Contract_Addr : in     Contract_Address;
      Code_Hash     : in     Hash256;
      Deployer      : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) is
      Slot : Natural;
   begin
      Success := False;
      Error := Error_None;

      --  Check if already registered
      if Find_Entry (Contract_Addr) /= Natural'Last then
         Error := Error_Already_Registered;
         return;
      end if;

      --  Find empty slot
      Slot := Find_Empty_Slot;
      if Slot = Natural'Last then
         Error := Error_Registry_Full;
         return;
      end if;

      --  Create entry
      Entries (Entry_Index (Slot)) := (
         Contract_Addr   => Contract_Addr,
         Code_Hash       => Code_Hash,
         Deployer        => Deployer,
         Level           => Level_None,
         Proof_Hash      => (others => 0),
         WCET_Bound      => U256_Zero,
         Auditors        => (others => (
            Auditor   => (others => 0),
            Signature => (others => 0),
            Timestamp => 0,
            Valid     => False
         )),
         Auditor_Count   => 0,
         Registered_At   => Current_Time,
         Updated_At      => Current_Time,
         Revoked         => False,
         Revoked_Reason  => (others => 0),
         Call_Count      => U256_Zero,
         Gas_Saved       => U256_Zero
      );

      --  Safe increment with overflow check
      if Entry_Count < Natural'Last then
         Entry_Count := Entry_Count + 1;
      end if;
      Success := True;
   end Register;

   procedure Set_Certification (
      Contract_Addr : in     Contract_Address;
      Level         : in     Certification_Level;
      Proof_Hash    : in     Hash256;
      WCET_Bound    : in     U256;
      Caller        : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) is
      Idx : Natural;
      Caller_Is_Certifier : Boolean;
   begin
      Success := False;
      Error := Error_None;

      --  Check authorization (extract volatile function result first)
      Caller_Is_Certifier := Is_Certifier (Caller);
      if not Caller_Is_Certifier then
         Error := Error_Unauthorized;
         return;
      end if;

      --  Find entry
      Idx := Find_Entry (Contract_Addr);
      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      --  Check not revoked
      if Entries (Entry_Index (Idx)).Revoked then
         Error := Error_Already_Revoked;
         return;
      end if;

      --  Platinum requires auditors
      if Level = Level_Platinum and then
         Entries (Entry_Index (Idx)).Auditor_Count = 0
      then
         Error := Error_Auditor_Required;
         return;
      end if;

      --  Update certification
      Entries (Entry_Index (Idx)).Level := Level;
      Entries (Entry_Index (Idx)).Proof_Hash := Proof_Hash;
      Entries (Entry_Index (Idx)).WCET_Bound := WCET_Bound;
      Entries (Entry_Index (Idx)).Updated_At := Current_Time;

      Success := True;
   end Set_Certification;

   procedure Add_Auditor (
      Contract_Addr : in     Contract_Address;
      Auditor       : in     Contract_Address;
      Signature     : in     Hash512;
      Caller        : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) is
      Idx : Natural;
      Aud_Idx : Natural;
   begin
      Success := False;
      Error := Error_None;

      --  Find entry
      Idx := Find_Entry (Contract_Addr);
      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      --  Check not revoked
      if Entries (Entry_Index (Idx)).Revoked then
         Error := Error_Already_Revoked;
         return;
      end if;

      --  Check room for auditor
      Aud_Idx := Entries (Entry_Index (Idx)).Auditor_Count;
      if Aud_Idx >= Max_Auditors then
         Error := Error_Registry_Full;
         return;
      end if;

      --  Add auditor attestation
      Entries (Entry_Index (Idx)).Auditors (Aud_Idx) := (
         Auditor   => Auditor,
         Signature => Signature,
         Timestamp => Current_Time,
         Valid     => True
      );
      Entries (Entry_Index (Idx)).Auditor_Count := Aud_Idx + 1;
      Entries (Entry_Index (Idx)).Updated_At := Current_Time;

      Success := True;
   end Add_Auditor;

   procedure Revoke (
      Contract_Addr : in     Contract_Address;
      Reason        : in     Hash256;
      Caller        : in     Contract_Address;
      Success       : out    Boolean;
      Error         : out    Registry_Error
   ) is
      Idx : Natural;
      Caller_Is_Certifier : Boolean;
   begin
      Success := False;
      Error := Error_None;

      --  Check authorization (extract volatile function result first)
      Caller_Is_Certifier := Is_Certifier (Caller);
      if not Caller_Is_Certifier then
         Error := Error_Unauthorized;
         return;
      end if;

      --  Find entry
      Idx := Find_Entry (Contract_Addr);
      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      --  Check not already revoked
      if Entries (Entry_Index (Idx)).Revoked then
         Error := Error_Already_Revoked;
         return;
      end if;

      --  Revoke
      Entries (Entry_Index (Idx)).Revoked := True;
      Entries (Entry_Index (Idx)).Revoked_Reason := Reason;
      Entries (Entry_Index (Idx)).Level := Level_None;
      Entries (Entry_Index (Idx)).Updated_At := Current_Time;

      Success := True;
   end Revoke;

   ---------------------------------------------------------------------------
   --  Query Operations
   ---------------------------------------------------------------------------

   function Is_Registered (Contract_Addr : Contract_Address) return Boolean is
   begin
      return Find_Entry (Contract_Addr) /= Natural'Last;
   end Is_Registered;

   function Get_Level (Contract_Addr : Contract_Address) return Certification_Level is
      Idx : constant Natural := Find_Entry (Contract_Addr);
   begin
      if Idx = Natural'Last then
         return Level_None;
      end if;
      if Entries (Entry_Index (Idx)).Revoked then
         return Level_None;
      end if;
      return Entries (Entry_Index (Idx)).Level;
   end Get_Level;

   procedure Get_Registry_Entry (
      Contract_Addr : in     Contract_Address;
      Reg_Entry     : out    Registry_Entry;
      Found         : out    Boolean
   ) is
      Idx : constant Natural := Find_Entry (Contract_Addr);
   begin
      if Idx = Natural'Last then
         Reg_Entry := Null_Entry;
         Found := False;
      else
         Reg_Entry := Entries (Entry_Index (Idx));
         Found := True;
      end if;
   end Get_Registry_Entry;

   function Is_Revoked (Contract_Addr : Contract_Address) return Boolean is
      Idx : constant Natural := Find_Entry (Contract_Addr);
   begin
      if Idx = Natural'Last then
         return False;
      end if;
      return Entries (Entry_Index (Idx)).Revoked;
   end Is_Revoked;

   function Get_WCET_Bound (Contract_Addr : Contract_Address) return U256 is
      Idx : constant Natural := Find_Entry (Contract_Addr);
   begin
      if Idx = Natural'Last then
         return U256_Zero;
      end if;
      return Entries (Entry_Index (Idx)).WCET_Bound;
   end Get_WCET_Bound;

   ---------------------------------------------------------------------------
   --  Gas Discount Calculation
   ---------------------------------------------------------------------------

   function Get_Gas_Discount (Contract_Addr : Contract_Address) return Natural is
      Level : constant Certification_Level := Get_Level (Contract_Addr);
   begin
      return Level_To_Discount (Level);
   end Get_Gas_Discount;

   function Apply_Discount (
      Gas_Amount    : U256;
      Contract_Addr : Contract_Address
   ) return U256 is
      Discount : constant Natural := Get_Gas_Discount (Contract_Addr);
      Savings  : U256;
      High     : U256;
      Result   : U256;
   begin
      if Discount = 0 then
         return Gas_Amount;
      end if;

      --  Calculate savings = (Gas_Amount * Discount) / 100
      Mul (Gas_Amount, From_Word64 (Word64 (Discount)), High, Savings);
      Savings := Div (Savings, From_Word64 (100));

      --  Return discounted amount
      Result := Sub_Mod (Gas_Amount, Savings);
      return Result;
   end Apply_Discount;

   procedure Record_Gas_Usage (
      Contract_Addr : in Contract_Address;
      Gas_Used      : in U256;
      Gas_Saved     : in U256
   ) is
      Idx : constant Natural := Find_Entry (Contract_Addr);
      Temp : U256;
      High : U256;
      Oflow : Boolean;
   begin
      if Idx = Natural'Last then
         return;
      end if;

      --  Increment call count
      Add (Entries (Entry_Index (Idx)).Call_Count, From_Word64 (1), Temp, Oflow);
      if not Oflow then
         Entries (Entry_Index (Idx)).Call_Count := Temp;
      end if;

      --  Add gas saved
      Add (Entries (Entry_Index (Idx)).Gas_Saved, Gas_Saved, Temp, Oflow);
      if not Oflow then
         Entries (Entry_Index (Idx)).Gas_Saved := Temp;
      end if;
   end Record_Gas_Usage;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   function Verify_Proof (
      Contract_Addr : Contract_Address;
      Proof_Data    : Hash256
   ) return Boolean is
      Idx : constant Natural := Find_Entry (Contract_Addr);
   begin
      if Idx = Natural'Last then
         return False;
      end if;
      return Entries (Entry_Index (Idx)).Proof_Hash = Proof_Data;
   end Verify_Proof;

   function Verify_Auditor (
      Contract_Addr : Contract_Address;
      Auditor       : Contract_Address
   ) return Boolean is
      Idx : constant Natural := Find_Entry (Contract_Addr);
      Aud_Count : Natural;
   begin
      if Idx = Natural'Last then
         return False;
      end if;

      Aud_Count := Entries (Entry_Index (Idx)).Auditor_Count;

      --  Early exit if no auditors or bounds check
      if Aud_Count = 0 or else Aud_Count > Max_Auditors then
         return False;
      end if;

      --  Iterate only over valid auditor indices (0..Aud_Count-1 where Aud_Count <= Max_Auditors)
      for I in 0 .. Aud_Count - 1 loop
         if Entries (Entry_Index (Idx)).Auditors (I).Auditor = Auditor and then
            Entries (Entry_Index (Idx)).Auditors (I).Valid
         then
            return True;
         end if;
      end loop;

      return False;
   end Verify_Auditor;

   function Meets_Level (
      Contract_Addr : Contract_Address;
      Min_Level     : Certification_Level
   ) return Boolean is
      Level : constant Certification_Level := Get_Level (Contract_Addr);
   begin
      return Certification_Level'Pos (Level) >= Certification_Level'Pos (Min_Level);
   end Meets_Level;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   function Total_Registered return Natural is
   begin
      return Entry_Count;
   end Total_Registered;

   function Count_By_Level (Level : Certification_Level) return Natural is
      Count : Natural := 0;
   begin
      for I in Entry_Index loop
         if Entries (I).Registered_At > 0 and then
            not Entries (I).Revoked and then
            Entries (I).Level = Level
         then
            --  Safe increment: Count can be at most Max_Contracts which is < Natural'Last
            if Count < Max_Contracts then
               Count := Count + 1;
            end if;
         end if;
      end loop;
      return Count;
   end Count_By_Level;

   function Total_Gas_Saved return U256 is
      Total : U256 := U256_Zero;
      Temp  : U256;
      Oflow : Boolean;
   begin
      for I in Entry_Index loop
         if Entries (I).Registered_At > 0 then
            Add (Total, Entries (I).Gas_Saved, Temp, Oflow);
            if not Oflow then
               Total := Temp;
            end if;
         end if;
      end loop;
      return Total;
   end Total_Gas_Saved;

   ---------------------------------------------------------------------------
   --  Time Management
   ---------------------------------------------------------------------------

   procedure Set_Current_Time (Timestamp : Unsigned_64) is
   begin
      Current_Time := Timestamp;
   end Set_Current_Time;

   function Get_Current_Time return Unsigned_64 is
   begin
      return Current_Time;
   end Get_Current_Time;

   ---------------------------------------------------------------------------
   --  Administrative
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin
      Entries := (others => Null_Entry);
      Entry_Count := 0;
      Certifiers := (others => (others => 0));
      Certifier_Count := 0;
      Admin_Addr := (others => 0);
      Current_Time := 0;
   end Initialize;

   function Is_Certifier (Addr : Contract_Address) return Boolean is
   begin
      if Addr = Admin_Addr then
         return True;
      end if;

      for I in Certifier_Index loop
         if Certifiers (I) = Addr then
            return True;
         end if;
      end loop;

      return False;
   end Is_Certifier;

   procedure Add_Certifier (
      Addr    : in     Contract_Address;
      Caller  : in     Contract_Address;
      Success : out    Boolean
   ) is
   begin
      Success := False;

      if Caller /= Admin_Addr then
         return;
      end if;

      if Certifier_Count >= Max_Certifiers then
         return;
      end if;

      Certifiers (Certifier_Index (Certifier_Count)) := Addr;
      Certifier_Count := Certifier_Count + 1;
      Success := True;
   end Add_Certifier;

   procedure Remove_Certifier (
      Addr    : in     Contract_Address;
      Caller  : in     Contract_Address;
      Success : out    Boolean
   ) is
   begin
      Success := False;

      if Caller /= Admin_Addr then
         return;
      end if;

      for I in Certifier_Index loop
         if Certifiers (I) = Addr then
            Certifiers (I) := (others => 0);
            Success := True;
            return;
         end if;
      end loop;
   end Remove_Certifier;

end Khepri_Registry;
