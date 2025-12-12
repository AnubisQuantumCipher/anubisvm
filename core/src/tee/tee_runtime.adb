pragma SPARK_Mode (On);

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

      --  Get state root (placeholder - would query Khepri MPT)
      State_Root_Val := Get_State_Root (State);

      --  Generate report
      TEE_Attestation.Generate_Report (
         TEE_Code      => State.Code_Hash,
         TEE_Config    => State.Config_Hash,
         State_Root    => State_Root_Val,
         CVM_Hashes    => CVM_Measurements,
         CVM_Count     => CVM_Count_Val,
         Nonce         => Nonce,
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
      Root : Measurement := (others => 0);
   begin
      --  In full implementation, this would query the Khepri MPT
      --  For now, hash the registry state as a placeholder

      --  Compute hash of registry state
      declare
         Count : constant Natural := CVM_Registry.CVM_Count (State.Registry);
         Count_Bytes : Byte_Array (0 .. 3);
      begin
         Count_Bytes (0) := Byte (Count mod 256);
         Count_Bytes (1) := Byte ((Count / 256) mod 256);
         Count_Bytes (2) := Byte ((Count / 65536) mod 256);
         Count_Bytes (3) := 0;
         SHA3_256 (Count_Bytes, Root);
      end;

      return Root;
   end Get_State_Root;

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

end TEE_Runtime;
