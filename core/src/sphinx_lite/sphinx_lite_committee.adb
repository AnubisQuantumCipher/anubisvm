--  SPHINX Lite Committee Implementation

pragma SPARK_Mode (On);

package body Sphinx_Lite_Committee with
   SPARK_Mode => On,
   Refined_State => (Committee_State => (
      Members, Member_Stats_Data, Active_Count, Current_Epoch, Chain,
      Initialized, Committee_Hash))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   type Member_Stats is record
      Sign_Count  : Unsigned_64;
      Last_Signed : Unsigned_64;
      Slashed     : Boolean;
      Slash_Reason : Slashing_Reason;
   end record;

   Null_Stats : constant Member_Stats := (
      Sign_Count   => 0,
      Last_Signed  => 0,
      Slashed      => False,
      Slash_Reason => Slash_None
   );

   type Stats_Array is array (Committee_Index) of Member_Stats;

   Members        : Committee_Array := (others => Null_Member);
   Member_Stats_Data : Stats_Array := (others => Null_Stats);
   Active_Count   : Natural := 0;
   Current_Epoch  : Unsigned_64 := 0;
   Chain          : Unsigned_32 := 0;
   Initialized    : Boolean := False;
   Committee_Hash : Hash256 := (others => 0);

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Compute_Committee_Hash with
      Global => (Input => Members, Output => Committee_Hash)
   is
      --  Simple hash computation (placeholder for real Keccak)
      Acc : Unsigned_8 := 0;
   begin
      for I in Committee_Index loop
         for J in Members (I).Address'Range loop
            Acc := Acc xor Members (I).Address (J);
         end loop;
      end loop;
      Committee_Hash := (others => Acc);
   end Compute_Committee_Hash;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   procedure Initialize (
      Genesis_Members : in Committee_Array;
      Chain_ID        : in Unsigned_32
   ) is
      Count : Natural := 0;
   begin
      Members := Genesis_Members;
      Chain := Chain_ID;
      Current_Epoch := 0;
      Member_Stats_Data := (others => Null_Stats);

      --  Count active members
      for I in Committee_Index loop
         if Members (I).Active then
            if Count < Committee_Size then
               Count := Count + 1;
            end if;
         end if;
      end loop;

      Active_Count := Count;
      Compute_Committee_Hash;
      Initialized := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Is_Initialized
   ---------------------------------------------------------------------------

   function Is_Initialized return Boolean is
   begin
      return Initialized;
   end Is_Initialized;

   ---------------------------------------------------------------------------
   --  Get_Committee_Size
   ---------------------------------------------------------------------------

   function Get_Committee_Size return Natural is
   begin
      return Committee_Size;
   end Get_Committee_Size;

   ---------------------------------------------------------------------------
   --  Get_Active_Count
   ---------------------------------------------------------------------------

   function Get_Active_Count return Natural is
   begin
      return Active_Count;
   end Get_Active_Count;

   ---------------------------------------------------------------------------
   --  Get_Member
   ---------------------------------------------------------------------------

   procedure Get_Member (
      Index  : in  Committee_Index;
      Member : out Committee_Member;
      Found  : out Boolean
   ) is
   begin
      if not Initialized then
         Member := Null_Member;
         Found := False;
      else
         Member := Members (Index);
         Found := Members (Index).Active;
      end if;
   end Get_Member;

   ---------------------------------------------------------------------------
   --  Find_Member
   ---------------------------------------------------------------------------

   procedure Find_Member (
      Address : in  Contract_Address;
      Index   : out Committee_Index;
      Found   : out Boolean
   ) is
   begin
      Index := 0;
      Found := False;

      if not Initialized then
         return;
      end if;

      for I in Committee_Index loop
         if Members (I).Address = Address and then Members (I).Active then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Member;

   ---------------------------------------------------------------------------
   --  Is_Valid_Member
   ---------------------------------------------------------------------------

   function Is_Valid_Member (Index : Committee_Index) return Boolean is
   begin
      if not Initialized then
         return False;
      end if;

      return Members (Index).Active and then
             not Member_Stats_Data (Index).Slashed;
   end Is_Valid_Member;

   ---------------------------------------------------------------------------
   --  Is_Member
   ---------------------------------------------------------------------------

   function Is_Member (Address : Contract_Address) return Boolean is
   begin
      if not Initialized then
         return False;
      end if;

      for I in Committee_Index loop
         if Members (I).Address = Address and then Members (I).Active then
            return True;
         end if;
      end loop;

      return False;
   end Is_Member;

   ---------------------------------------------------------------------------
   --  Get_Current_Epoch
   ---------------------------------------------------------------------------

   function Get_Current_Epoch (Block_Number : Unsigned_64) return Unsigned_64 is
   begin
      return Block_Number / Epoch_Length;
   end Get_Current_Epoch;

   ---------------------------------------------------------------------------
   --  Rotate_Committee
   ---------------------------------------------------------------------------

   procedure Rotate_Committee (
      New_Members : in  Committee_Array;
      Epoch       : in  Unsigned_64;
      Success     : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) is
      Count : Natural := 0;
   begin
      Success := False;
      Error := Error_None;

      --  Validate epoch progression
      if Epoch <= Current_Epoch then
         Error := Error_Invalid_Block_Number;
         return;
      end if;

      --  Update members
      Members := New_Members;
      Current_Epoch := Epoch;
      Member_Stats_Data := (others => Null_Stats);

      --  Count active
      for I in Committee_Index loop
         if Members (I).Active then
            if Count < Committee_Size then
               Count := Count + 1;
            end if;
         end if;
      end loop;

      Active_Count := Count;
      Compute_Committee_Hash;
      Success := True;
   end Rotate_Committee;

   ---------------------------------------------------------------------------
   --  Get_Committee_Root
   ---------------------------------------------------------------------------

   function Get_Committee_Root return Hash256 is
   begin
      return Committee_Hash;
   end Get_Committee_Root;

   ---------------------------------------------------------------------------
   --  Verify_Member_Signature
   ---------------------------------------------------------------------------

   procedure Verify_Member_Signature (
      Index       : in  Committee_Index;
      Checkpoint  : in  Sphinx_Lite_Types.Checkpoint;
      Signature   : in  Hash512;
      Valid       : out Boolean
   ) is
      pragma Unreferenced (Checkpoint, Signature);
   begin
      --  Placeholder for ML-DSA-87 signature verification
      --  In production, this calls anubis_mldsa.Verify
      if not Initialized then
         Valid := False;
         return;
      end if;

      if not Members (Index).Active then
         Valid := False;
         return;
      end if;

      if Member_Stats_Data (Index).Slashed then
         Valid := False;
         return;
      end if;

      --  Assume valid for now (real impl uses crypto)
      Valid := True;

      --  Update stats
      if Member_Stats_Data (Index).Sign_Count < Unsigned_64'Last then
         Member_Stats_Data (Index).Sign_Count :=
            Member_Stats_Data (Index).Sign_Count + 1;
      end if;
   end Verify_Member_Signature;

   ---------------------------------------------------------------------------
   --  Verify_Threshold
   ---------------------------------------------------------------------------

   procedure Verify_Threshold (
      Checkpoint  : in  Sphinx_Lite_Types.Checkpoint;
      Signatures  : in  Signature_Array;
      Result      : out Verification_Result
   ) is
      Valid_Count : Natural := 0;
      Sig_Valid   : Boolean;
   begin
      Result := (Success => False, Error => Error_None);

      if not Initialized then
         Result.Error := Error_Not_Initialized;
         return;
      end if;

      --  Verify each signature
      for I in Committee_Index loop
         if Signatures (I).Valid then
            Verify_Member_Signature (
               Index      => I,
               Checkpoint => Checkpoint,
               Signature  => Signatures (I).Signature,
               Valid      => Sig_Valid
            );

            if Sig_Valid then
               if Valid_Count < Committee_Size then
                  Valid_Count := Valid_Count + 1;
               end if;
            end if;
         end if;
      end loop;

      --  Check threshold
      if Valid_Count >= Threshold then
         Result := Sphinx_Lite_Types.Success_Result;
      else
         Result.Error := Error_Insufficient_Signatures;
      end if;
   end Verify_Threshold;

   ---------------------------------------------------------------------------
   --  Slash_Member
   ---------------------------------------------------------------------------

   procedure Slash_Member (
      Index   : in  Committee_Index;
      Reason  : in  Slashing_Reason;
      Success : out Boolean
   ) is
   begin
      Success := False;

      if not Initialized then
         return;
      end if;

      if not Members (Index).Active then
         return;
      end if;

      --  Already slashed?
      if Member_Stats_Data (Index).Slashed then
         return;
      end if;

      --  Slash the member
      Member_Stats_Data (Index).Slashed := True;
      Member_Stats_Data (Index).Slash_Reason := Reason;
      Members (Index).Active := False;

      --  Update active count
      if Active_Count > 0 then
         Active_Count := Active_Count - 1;
      end if;

      Success := True;
   end Slash_Member;

   ---------------------------------------------------------------------------
   --  Is_Slashed
   ---------------------------------------------------------------------------

   function Is_Slashed (Index : Committee_Index) return Boolean is
   begin
      if not Initialized then
         return False;
      end if;

      return Member_Stats_Data (Index).Slashed;
   end Is_Slashed;

   ---------------------------------------------------------------------------
   --  Get_Sign_Count
   ---------------------------------------------------------------------------

   function Get_Sign_Count (Index : Committee_Index) return Unsigned_64 is
   begin
      if not Initialized then
         return 0;
      end if;

      return Member_Stats_Data (Index).Sign_Count;
   end Get_Sign_Count;

   ---------------------------------------------------------------------------
   --  Get_Last_Signed
   ---------------------------------------------------------------------------

   function Get_Last_Signed (Index : Committee_Index) return Unsigned_64 is
   begin
      if not Initialized then
         return 0;
      end if;

      return Member_Stats_Data (Index).Last_Signed;
   end Get_Last_Signed;

end Sphinx_Lite_Committee;
