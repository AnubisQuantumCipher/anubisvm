pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3; use Anubis_SHA3;
with CVM_Interface; use CVM_Interface;

package body TEE_Runtime with
   SPARK_Mode => On
is

   --  Initialize TEE
   procedure Initialize (
      Entropy     : Byte_Array;
      Code_Hash   : Measurement;
      Config_Hash : Measurement;
      State       : out TEE_State;
      Success     : out Boolean
   ) is
      Key_Success : Boolean;
   begin
      State := Initial_State;
      State.Status := Initializing;
      Success := False;

      --  Store measurements
      State.Code_Hash := Code_Hash;
      State.Config_Hash := Config_Hash;

      --  Initialize keys
      TEE_Keys.Initialize_Keys (Entropy, State.Keys, Key_Success);

      if not Key_Success then
         State.Status := Error;
         State.Error_Code := Key_Generation_Fail;
         return;
      end if;

      --  Initialize CVM registry
      CVM_Registry.Initialize (State.Registry);

      --  Initialize dispatch state
      State.Dispatch := Initial_Dispatch_State;

      State.Status := Running;
      State.Height := 0;
      Success := True;
   end Initialize;

   --  Shutdown TEE
   procedure Shutdown (
      State : in Out TEE_State
   ) is
   begin
      --  Zeroize all keys
      TEE_Keys.Zeroize_Bundle (State.Keys);

      --  Clear registry (not sensitive but clean up)
      CVM_Registry.Initialize (State.Registry);

      --  Reset dispatch
      State.Dispatch := Initial_Dispatch_State;

      --  Clear measurements
      State.Code_Hash := (others => 0);
      State.Config_Hash := (others => 0);

      State.Status := Shutdown;
      State.Error_Code := 0;
   end Shutdown;

   --  Register CVM
   procedure Register_CVM (
      State   : in Out TEE_State;
      Reg     : CVM_Interface.CVM_Registration;
      Success : out Boolean
   ) is
   begin
      CVM_Registry.Register_CVM (State.Registry, Reg, Success);

      if not Success then
         State.Error_Code := CVM_Register_Fail;
      end if;
   end Register_CVM;

   --  Execute CVM call
   procedure Execute (
      State   : in Out TEE_State;
      Context : Call_Context;
      Result  : out Exec_Result
   ) is
   begin
      CVM_Dispatch.Execute_Call (
         State.Registry,
         State.Dispatch,
         Context,
         Result
      );

      if Result.Status /= Success then
         State.Error_Code := Execution_Fail;
      end if;
   end Execute;

   --  Get CVM count
   function Get_CVM_Count (
      State : TEE_State
   ) return Natural is
   begin
      return CVM_Registry.CVM_Count (State.Registry);
   end Get_CVM_Count;

   --  Generate attestation quote
   procedure Generate_Attestation (
      State   : TEE_State;
      Nonce   : Byte_Array;
      Quote   : out Attestation_Quote;
      Success : out Boolean
   ) is
      Report : Attestation_Report;
      CVM_Measurements : CVM_Measurement_Array := (others => Empty_CVM_Measurement);
      CVM_Count_Val : Natural := 0;
      State_Root_Val : Measurement := (others => 0);
   begin
      Quote := Empty_Quote;
      Success := False;

      --  Gather CVM measurements from registry
      for I in State.Registry'Range loop
         if State.Registry (I).Occupied and then CVM_Count_Val < Max_CVM_Measurements then
            declare
               Desc : CVM_Descriptor;
            begin
               CVM_Registry.Get_Descriptor (State.Registry, I, Desc);
               CVM_Measurements (CVM_Count_Val).Address := Desc.Info.Addr;
               CVM_Measurements (CVM_Count_Val).CodeHash := Desc.Info.Code;
               CVM_Measurements (CVM_Count_Val).Active := Desc.Info.Active;
               CVM_Count_Val := CVM_Count_Val + 1;
            end;
         end if;
      end loop;

      --  Get state root (cached from last Khepri MPT commit via Set_State_Root)
      State_Root_Val := Get_State_Root (State);

      --  Generate report with real timestamp from TEE state
      TEE_Attestation.Generate_Report (
         TEE_Code      => State.Code_Hash,
         TEE_Config    => State.Config_Hash,
         State_Root    => State_Root_Val,
         CVM_Hashes    => CVM_Measurements,
         CVM_Count     => CVM_Count_Val,
         Nonce         => Nonce,
         Block_Time    => State.Timestamp,
         Attest_PK     => State.Keys.Attest_PK,
         Report        => Report
      );

      --  Generate signed quote
      TEE_Attestation.Generate_Quote (
         Report,
         State.Keys.Attest_SK,
         Quote,
         Success
      );
   end Generate_Attestation;

   --  Get attestation public key
   function Get_Attestation_PK (
      State : TEE_State
   ) return DSA_Public_Key is
   begin
      return State.Keys.Attest_PK;
   end Get_Attestation_PK;

   --  Get KEM key
   function Get_KEM_Key (
      State : TEE_State
   ) return KEM_Encaps_Key is
   begin
      return State.Keys.KEM_EK;
   end Get_KEM_Key;

   --  Establish session
   procedure Establish_Session (
      State      : TEE_State;
      Ciphertext : KEM_Ciphertext;
      Session_ID : out Byte_Array;
      Key        : out Session_Key;
      Success    : out Boolean
   ) is
      Shared_Secret : KEM_Shared_Secret;
      Session_Hash : SHA3_256_Digest;
   begin
      Session_ID := (others => 0);
      Key := (others => 0);
      Success := False;

      --  Decapsulate to get shared secret
      TEE_Keys.Decapsulate (
         State.Keys.KEM_DK,
         Ciphertext,
         Shared_Secret
      );

      --  Derive session ID from shared secret + ciphertext
      declare
         Input : Byte_Array (0 .. KEM_SS_Size + KEM_CT_Size - 1);
      begin
         for I in Shared_Secret'Range loop
            Input (I) := Shared_Secret (I);
         end loop;
         for I in Ciphertext'Range loop
            Input (KEM_SS_Size + I) := Ciphertext (I);
         end loop;
         SHA3_256 (Input, Session_Hash);
      end;

      --  Session ID is the hash
      for I in Session_ID'Range loop
         Session_ID (I) := Session_Hash (Session_Hash'First + I - Session_ID'First);
      end loop;

      --  Derive session key
      TEE_Keys.Derive_Connection_Key (
         State.Keys.MSK,
         Session_ID,
         Key
      );

      Success := True;
   end Establish_Session;

   --  Get state root
   function Get_State_Root (
      State : TEE_State
   ) return Measurement is
   begin
      --  Return the cached state root (set by Set_State_Root after MPT commits)
      return State.State_Root;
   end Get_State_Root;

   --  Update state root (called after Khepri MPT commits)
   procedure Set_State_Root (
      State     : in Out TEE_State;
      New_Root  : in     Measurement
   ) is
   begin
      State.State_Root := New_Root;
   end Set_State_Root;

   --  Get current timestamp
   function Get_Timestamp (
      State : TEE_State
   ) return Word64 is
   begin
      return State.Timestamp;
   end Get_Timestamp;

   --  Update timestamp
   procedure Set_Timestamp (
      State         : in Out TEE_State;
      New_Timestamp : in     Word64
   ) is
   begin
      State.Timestamp := New_Timestamp;
   end Set_Timestamp;

   --  Advance height
   procedure Advance_Height (
      State : in Out TEE_State
   ) is
   begin
      if State.Height < Natural'Last then
         State.Height := State.Height + 1;
      end if;
   end Advance_Height;

   --  Clear error
   procedure Clear_Error (
      State : in Out TEE_State
   ) is
   begin
      State.Error_Code := 0;
      if State.Status = Error then
         State.Status := Running;
      end if;
   end Clear_Error;

   ---------------------------------------------------------------------------
   --  Memory Isolation Verification
   ---------------------------------------------------------------------------

   --  Verify memory isolation
   function Verify_Memory_Isolation (
      Isolation : Memory_Isolation_State
   ) return Boolean is
   begin
      --  Empty isolation state is invalid
      if Isolation.Region_Count = 0 then
         return False;
      end if;

      --  Check each region
      for I in 0 .. Isolation.Region_Count - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Isolation.Region_Count);
         pragma Loop_Invariant (Isolation.Region_Count <= Max_Memory_Regions);

         declare
            R : constant Memory_Region := Isolation.Regions (I);
         begin
            --  Region must be valid
            if not R.Valid then
               return False;
            end if;

            --  Region must be marked as isolated
            if not R.Isolated then
               return False;
            end if;

            --  Region must have non-zero size
            if R.Size = 0 then
               return False;
            end if;

            --  Executable regions must be read-only (W^X policy)
            if R.Executable and not R.Read_Only then
               return False;
            end if;

            --  Check for overlaps with other regions
            for J in 0 .. Isolation.Region_Count - 1 loop
               pragma Loop_Invariant (J >= 0 and J < Isolation.Region_Count);

               if I /= J and Isolation.Regions (J).Valid then
                  if Regions_Overlap (R, Isolation.Regions (J)) then
                     return False;
                  end if;
               end if;
            end loop;
         end;
      end loop;

      return True;
   end Verify_Memory_Isolation;

   --  Add memory region to isolation state
   procedure Add_Memory_Region (
      Isolation : in out Memory_Isolation_State;
      Region    : Memory_Region;
      Success   : out Boolean
   ) is
   begin
      Success := False;

      --  Check if we have space
      if Isolation.Region_Count >= Max_Memory_Regions then
         return;
      end if;

      --  Check for overlaps with existing regions
      for I in 0 .. Isolation.Region_Count - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Isolation.Region_Count);

         if Isolation.Regions (I).Valid then
            if Regions_Overlap (Region, Isolation.Regions (I)) then
               return;
            end if;
         end if;
      end loop;

      --  Add the region
      Isolation.Regions (Isolation.Region_Count) := Region;
      Isolation.Region_Count := Isolation.Region_Count + 1;
      Success := True;
   end Add_Memory_Region;

   --  Check if two memory regions overlap
   function Regions_Overlap (
      A : Memory_Region;
      B : Memory_Region
   ) return Boolean is
      A_End : Word64;
      B_End : Word64;
   begin
      --  Compute end addresses (handle potential overflow)
      if A.Base_Address <= Word64'Last - A.Size then
         A_End := A.Base_Address + A.Size;
      else
         A_End := Word64'Last;
      end if;

      if B.Base_Address <= Word64'Last - B.Size then
         B_End := B.Base_Address + B.Size;
      else
         B_End := Word64'Last;
      end if;

      --  Check for overlap
      --  Regions overlap if: A.Base < B_End and B.Base < A_End
      return A.Base_Address < B_End and B.Base_Address < A_End;
   end Regions_Overlap;

   ---------------------------------------------------------------------------
   --  Enclave Lifecycle Management
   ---------------------------------------------------------------------------

   --  Verify enclave lifecycle state transition is valid
   function Valid_Lifecycle_Transition (
      Current : Enclave_Lifecycle;
      Next    : Enclave_Lifecycle
   ) return Boolean is
   begin
      --  Self-transition is always valid
      if Current = Next then
         return True;
      end if;

      --  Check valid transitions based on current state
      case Current is
         when Uninitialized =>
            --  Can only transition to Created
            return Next = Created;

         when Created =>
            --  Can transition to Initialized or Destroyed
            return Next = Initialized or Next = Destroyed;

         when Initialized =>
            --  Can transition to Attested or Terminating
            return Next = Attested or Next = Terminating;

         when Attested =>
            --  Can transition to Running or Terminating
            return Next = Running or Next = Terminating;

         when Running =>
            --  Can suspend or terminate
            return Next = Suspended or Next = Terminating;

         when Suspended =>
            --  Can resume to running or terminate
            return Next = Running or Next = Terminating;

         when Terminating =>
            --  Can only go to destroyed
            return Next = Destroyed;

         when Destroyed =>
            --  Terminal state, no transitions allowed
            return False;
      end case;
   end Valid_Lifecycle_Transition;

   --  Update enclave lifecycle state
   procedure Update_Enclave_State (
      Metadata   : in out Enclave_Metadata;
      New_State  : Enclave_Lifecycle;
      Timestamp  : Word64;
      Success    : out Boolean
   ) is
   begin
      Success := False;

      --  Verify transition is valid
      if not Valid_Lifecycle_Transition (Metadata.Lifecycle, New_State) then
         return;
      end if;

      --  Update state
      Metadata.Lifecycle := New_State;

      --  Update attestation time if transitioning to Attested
      if New_State = Attested then
         Metadata.Last_Attest := Timestamp;
      end if;

      Success := True;
   end Update_Enclave_State;

   --  Verify enclave measurement matches expected
   function Verify_Enclave_Measurement (
      Metadata : Enclave_Metadata;
      Expected : Measurement
   ) return Boolean is
      Diff : Byte := 0;
   begin
      --  Constant-time comparison
      for I in Metadata.Measurement'Range loop
         pragma Loop_Invariant (I in Metadata.Measurement'Range);
         Diff := Diff or (Metadata.Measurement (I) xor Expected (I));
      end loop;

      return Diff = 0;
   end Verify_Enclave_Measurement;

   --  Seal enclave state
   procedure Seal_Enclave_State (
      Metadata : in out Enclave_Metadata
   ) is
   begin
      Metadata.Sealed := True;
   end Seal_Enclave_State;

   --  Unseal enclave state
   procedure Unseal_Enclave_State (
      Metadata : in out Enclave_Metadata
   ) is
   begin
      Metadata.Sealed := False;
   end Unseal_Enclave_State;

end TEE_Runtime;
