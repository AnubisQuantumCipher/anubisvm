with Ada.Text_IO;   use Ada.Text_IO;
with Interfaces;    use Interfaces;

with Aegis_VM_Types;
with Khepri_Types;

with Anubis_Types;
with Anubis_MLDSA_Types;  use Anubis_MLDSA_Types;
with Anubis_MLDSA;
with Anubis_Address_Types;     use Anubis_Address_Types;
with Anubis_Address_Derive;    use Anubis_Address_Derive;
with Anubis_Address;

with Quantum_Vault;

procedure Test_Quantum_Vault is

   subtype Address is Khepri_Types.Address;

   --  Single signer keypair (ML-DSA-87)
   Key_Seed : Seed := (others => 0);
   PK      : Public_Key;
   SK      : Secret_Key;

   --  AAS-001 address view
   AAS_PK   : Public_Key_Bytes;
   AAS_Addr : Anubis_Address_Types.Address;

   --  VM address (32-byte account ID)
   Signer_Address : Address := (others => 0);

   --  Vault initialization
   Signers : Quantum_Vault.Address_Array (0 .. 0);
   Weights : Quantum_Vault.Natural_Array (0 .. 0);

   Init_OK : Boolean;
   Init_E  : Quantum_Vault.Error_Code;

   --  Transaction state
   TX_Data : Aegis_VM_Types.Byte_Array (0 .. 0) := (others => 0);
   TX_ID   : Natural := 0;

   Prop_OK : Boolean;
   Prop_E  : Quantum_Vault.Error_Code;

   --  Approval
   Block_Num : constant Natural := 1;
   Sig_Seed  : Seed := (others => 1);
   Sig       : Signature;
   Sig_OK    : Boolean;

   Approve_Sig : Aegis_VM_Types.Byte_Array
     (0 .. Quantum_Vault.Signature_Size - 1);
   App_OK      : Boolean;
   App_E       : Quantum_Vault.Error_Code;

   Exec_OK : Boolean;
   Exec_E  : Quantum_Vault.Error_Code;

begin
   Put_Line ("[Quantum_Vault] ML-DSA keygen and AAS-001 address...");

   --  Deterministic seed for repeatable TE runs
   for I in Key_Seed'Range loop
      Key_Seed (I) := Anubis_Types.Byte (I);
   end loop;

   Anubis_MLDSA.KeyGen (Key_Seed, PK, SK);

   --  Map public key into AAS-001 Public_Key_Bytes
   for I in AAS_PK'Range loop
      AAS_PK (I) := Unsigned_8 (PK (I));
   end loop;

   --  Derive AAS-001 address (Dev network, user entity)
   AAS_Addr := Anubis_Address.Create_Address
     (Network    => Dev,
      Entity     => User,
      Public_Key => AAS_PK);

   --  Extract 32-byte account ID into VM Address
   for I in 0 .. 31 loop
      Signer_Address (I) :=
        Aegis_VM_Types.Byte (AAS_Addr.Account (I));
   end loop;

   Put_Line ("  Derived VM address from AAS-001 account ID.");

   --  Initialize vault with single signer, threshold 1-of-1
   Signers (0) := Signer_Address;
   Weights (0) := 1;

   Quantum_Vault.Initialize
     (Signers   => Signers,
      Weights   => Weights,
      Threshold => 1,
      Success   => Init_OK,
      Error     => Init_E);

   Put_Line ("[Quantum_Vault] Initialize -> "
     & "success=" & Boolean'Image (Init_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (Init_E));

   if not Init_OK then
      return;
   end if;

   --  Rotate key to inject real ML-DSA public key into vault state
   declare
      PK_Bytes  : Aegis_VM_Types.Byte_Array (0 .. 2591);
      Dummy_Sig : Aegis_VM_Types.Byte_Array
        (0 .. Quantum_Vault.Signature_Size - 1) := (others => 0);
      Rot_OK : Boolean;
      Rot_E  : Quantum_Vault.Error_Code;
   begin
      for I in PK_Bytes'Range loop
         PK_Bytes (I) := Aegis_VM_Types.Byte (PK (I));
      end loop;

      Quantum_Vault.Rotate_Key
        (Old_Address   => Signer_Address,
         New_Address   => Signer_Address,
         New_Public_Key => PK_Bytes,
         Signature     => Dummy_Sig,
         Success       => Rot_OK,
         Error         => Rot_E);

      Put_Line ("[Quantum_Vault] Rotate_Key -> "
        & "success=" & Boolean'Image (Rot_OK)
        & " error=" & Quantum_Vault.Error_Code'Image (Rot_E));

      if not Rot_OK then
         return;
      end if;
   end;

   --  Propose a simple transaction
   TX_Data (0) := 0;

   Quantum_Vault.Propose_Transaction
     (Proposer    => Signer_Address,
      Destination => Signer_Address,
      Amount      => 42,
      Data        => TX_Data,
      Block_Num   => Block_Num,
      TX_ID       => TX_ID,
      Success     => Prop_OK,
      Error       => Prop_E);

   Put_Line ("[Quantum_Vault] Propose_Transaction -> "
     & "success=" & Boolean'Image (Prop_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (Prop_E)
     & " tx_id=" & Natural'Image (TX_ID));

   if not Prop_OK then
      return;
   end if;

   --  Build the exact approval message used by Approve_Transaction:
   --    8 bytes little-endian TX_ID || 8 bytes little-endian Block_Num
   declare
      Msg       : Aegis_VM_Types.Byte_Array (0 .. 15);
      ID_Val    : Unsigned_64 := Unsigned_64 (TX_ID);
      Block_Val : Unsigned_64 := Unsigned_64 (Block_Num);
   begin
      for I in 0 .. 7 loop
         Msg (I) := Aegis_VM_Types.Byte (ID_Val and 16#FF#);
         ID_Val  := Shift_Right (ID_Val, 8);
      end loop;

      for I in 0 .. 7 loop
         Msg (8 + I) := Aegis_VM_Types.Byte (Block_Val and 16#FF#);
         Block_Val   := Shift_Right (Block_Val, 8);
      end loop;

      --  Sign message with ML-DSA
      declare
         Msg_For_Sign : Anubis_Types.Byte_Array (0 .. Msg'Length - 1);
      begin
         for I in Msg_For_Sign'Range loop
            Msg_For_Sign (I) := Anubis_Types.Byte (Msg (I));
         end loop;

         for I in Sig_Seed'Range loop
            Sig_Seed (I) :=
              Anubis_Types.Byte (Sig_Seed'Length - 1 - I);
         end loop;

         Anubis_MLDSA.Sign
           (SK      => SK,
            Msg     => Msg_For_Sign,
            Random  => Sig_Seed,
            Sig     => Sig,
            Success => Sig_OK);
      end;
   end;

   Put_Line ("[Quantum_Vault] ML-DSA Sign -> "
     & "success=" & Boolean'Image (Sig_OK));

   if not Sig_OK then
      return;
   end if;

   --  Map signature into Byte_Array for vault API
   for I in Approve_Sig'Range loop
      Approve_Sig (I) := Aegis_VM_Types.Byte (Sig (I));
   end loop;

   Quantum_Vault.Approve_Transaction
     (TX_ID     => TX_ID,
      Signer    => Signer_Address,
      Signature => Approve_Sig,
      Block_Num => Block_Num,
      Success   => App_OK,
      Error     => App_E);

   Put_Line ("[Quantum_Vault] Approve_Transaction -> "
     & "success=" & Boolean'Image (App_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (App_E));

   if not App_OK then
      return;
   end if;

   --  Execute transaction once approvals meet threshold
   Quantum_Vault.Execute_Transaction
     (TX_ID     => TX_ID,
      Caller    => Signer_Address,
      Block_Num => Block_Num,
      Success   => Exec_OK,
      Error     => Exec_E);

   Put_Line ("[Quantum_Vault] Execute_Transaction -> "
     & "success=" & Boolean'Image (Exec_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (Exec_E));

   if not Exec_OK then
      return;
   end if;

   Put_Line ("[Quantum_Vault] End-to-end flow OK.");

end Test_Quantum_Vault;
