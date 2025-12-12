with Ada.Text_IO;    use Ada.Text_IO;
with Interfaces;     use Interfaces;

with Aegis_VM_Types;
with Khepri_Types;

with Anubis_Types;
with Anubis_MLDSA_Types;  use Anubis_MLDSA_Types;
with Anubis_MLDSA;
with Anubis_Address_Types; use Anubis_Address_Types;
with Anubis_Address_Derive; use Anubis_Address_Derive;
with Anubis_Address;

with Quantum_Vault;

procedure Test_Quantum_Vault_Multisig is

   subtype Address is Khepri_Types.Address;

   --  Simple holder for signer key material
   type Signer_Keypair is record
      PK     : Public_Key;
      SK     : Secret_Key;
      Addr   : Address;
   end record;

   type Signer_Array is array (Natural range <>) of Signer_Keypair;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   procedure Make_Signer
     (Seed_Byte : Interfaces.Unsigned_8;
      Out_Signer : out Signer_Keypair) is
      S_Seed : Seed := (others => 0);
      AAS_PK : Public_Key_Bytes;
      AAS_Addr_Record : Anubis_Address_Types.Address;
   begin
      for I in S_Seed'Range loop
         S_Seed (I) := Anubis_Types.Byte (Seed_Byte + Interfaces.Unsigned_8 (I));
      end loop;

      Anubis_MLDSA.KeyGen (S_Seed, Out_Signer.PK, Out_Signer.SK);

      for I in AAS_PK'Range loop
         AAS_PK (I) := Interfaces.Unsigned_8 (Out_Signer.PK (I));
      end loop;

      AAS_Addr_Record := Anubis_Address.Create_Address
        (Network    => Dev,
         Entity     => User,
         Public_Key => AAS_PK);

      for I in 0 .. 31 loop
         Out_Signer.Addr (I) :=
           Aegis_VM_Types.Byte (AAS_Addr_Record.Account (I));
      end loop;
   end Make_Signer;

   function Sign_Tx_Message
     (TX_ID     : Natural;
      Block_Num : Natural;
      SK        : Secret_Key) return Signature is

      Msg       : Aegis_VM_Types.Byte_Array (0 .. 15);
      ID_Val    : Unsigned_64 := Unsigned_64 (TX_ID);
      Block_Val : Unsigned_64 := Unsigned_64 (Block_Num);
      Sig       : Signature;
      Seed_R    : Seed := (others => 0);
      Msg_For_Sign : Anubis_Types.Byte_Array (0 .. Msg'Length - 1);
      Ok        : Boolean;
   begin
      for I in 0 .. 7 loop
         Msg (I) := Aegis_VM_Types.Byte (ID_Val and 16#FF#);
         ID_Val  := Shift_Right (ID_Val, 8);
      end loop;

      for I in 0 .. 7 loop
         Msg (8 + I) :=
           Aegis_VM_Types.Byte (Block_Val and 16#FF#);
         Block_Val := Shift_Right (Block_Val, 8);
      end loop;

      for I in Msg_For_Sign'Range loop
         Msg_For_Sign (I) := Msg (I);
      end loop;

      for I in Seed_R'Range loop
         Seed_R (I) := Anubis_Types.Byte (Seed_R'Length - 1 - I);
      end loop;

      Anubis_MLDSA.Sign
        (SK      => SK,
         Msg     => Msg_For_Sign,
         Random  => Seed_R,
         Sig     => Sig,
         Success => Ok);

      if not Ok then
         Put_Line ("[Multisig] Sign_Tx_Message: signing failed");
      end if;

      return Sig;
   end Sign_Tx_Message;

   ---------------------------------------------------------------------------
   --  Test body
   ---------------------------------------------------------------------------

   Signers_Keys : Signer_Array (0 .. 2);

   Signers : Quantum_Vault.Address_Array (0 .. 2);
   Weights : Quantum_Vault.Natural_Array (0 .. 2);

   Init_OK : Boolean;
   Init_E  : Quantum_Vault.Error_Code;

   TX_Data : Aegis_VM_Types.Byte_Array (0 .. 0) := (others => 0);
   TX_ID   : Natural := 0;
   Prop_OK : Boolean;
   Prop_E  : Quantum_Vault.Error_Code;

   App_OK  : Boolean;
   App_E   : Quantum_Vault.Error_Code;
   Exec_OK : Boolean;
   Exec_E  : Quantum_Vault.Error_Code;

   Block_Num : constant Natural := 42;

begin
   Put_Line ("[Multisig] Generating 3 ML-DSA signers + AAS-001 addresses...");

   Make_Signer (16#01#, Signers_Keys (0));
   Make_Signer (16#10#, Signers_Keys (1));
   Make_Signer (16#20#, Signers_Keys (2));

   for I in Signers'Range loop
      Signers (I) := Signers_Keys (I).Addr;
      Weights (I) := 1;
   end loop;

   Put_Line ("[Multisig] Initializing Quantum_Vault with 2-of-3 threshold...");

   Quantum_Vault.Initialize
     (Signers   => Signers,
      Weights   => Weights,
      Threshold => 2,
      Success   => Init_OK,
      Error     => Init_E);

   Put_Line ("  Initialize: success="
     & Boolean'Image (Init_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (Init_E));

   if not Init_OK then
      return;
   end if;

   Put_Line ("[Multisig] Rotating keys to install public keys...");

   declare
      PK_Bytes  : Aegis_VM_Types.Byte_Array (0 .. 2591);
      Dummy_Sig : Aegis_VM_Types.Byte_Array
        (0 .. Quantum_Vault.Signature_Size - 1) := (others => 0);
      Rot_OK : Boolean;
      Rot_E  : Quantum_Vault.Error_Code;
   begin
      for I in Signers_Keys'Range loop
         for J in PK_Bytes'Range loop
            PK_Bytes (J) := Aegis_VM_Types.Byte (Signers_Keys (I).PK (J));
         end loop;

         Quantum_Vault.Rotate_Key
           (Old_Address    => Signers_Keys (I).Addr,
            New_Address    => Signers_Keys (I).Addr,
            New_Public_Key => PK_Bytes,
            Signature      => Dummy_Sig,
            Success        => Rot_OK,
            Error          => Rot_E);

         Put_Line ("  Rotate_Key signer"
           & Natural'Image (I)
           & ": success=" & Boolean'Image (Rot_OK)
           & " error=" & Quantum_Vault.Error_Code'Image (Rot_E));

         if not Rot_OK then
            return;
         end if;
      end loop;
   end;

   Put_Line ("[Multisig] Proposing transaction from signer 1...");

   TX_Data (0) := 0;

   Quantum_Vault.Propose_Transaction
     (Proposer    => Signers_Keys (0).Addr,
      Destination => Signers_Keys (2).Addr,
      Amount      => 100,
      Data        => TX_Data,
      Block_Num   => Block_Num,
      TX_ID       => TX_ID,
      Success     => Prop_OK,
      Error       => Prop_E);

   Put_Line ("  Propose_Transaction: success="
     & Boolean'Image (Prop_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (Prop_E)
     & " tx_id=" & Natural'Image (TX_ID));

   if not Prop_OK then
      return;
   end if;

   Put_Line ("[Multisig] Approving with signer 1 (valid)...");

   declare
      Sig1 : constant Signature :=
        Sign_Tx_Message (TX_ID, Block_Num, Signers_Keys (0).SK);
      Approve_Sig : Aegis_VM_Types.Byte_Array
        (0 .. Quantum_Vault.Signature_Size - 1);
   begin
      for I in Approve_Sig'Range loop
         Approve_Sig (I) := Aegis_VM_Types.Byte (Sig1 (I));
      end loop;

      Quantum_Vault.Approve_Transaction
        (TX_ID     => TX_ID,
         Signer    => Signers_Keys (0).Addr,
         Signature => Approve_Sig,
         Block_Num => Block_Num,
         Success   => App_OK,
         Error     => App_E);
   end;

   Put_Line ("  Approve (signer1): success="
     & Boolean'Image (App_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (App_E));

   if not App_OK then
      return;
   end if;

   Put_Line ("[Multisig] Attempting Execute with only 1 approval (should fail)...");

   Quantum_Vault.Execute_Transaction
     (TX_ID     => TX_ID,
      Caller    => Signers_Keys (0).Addr,
      Block_Num => Block_Num,
      Success   => Exec_OK,
      Error     => Exec_E);

   Put_Line ("  Execute (1-of-2): success="
     & Boolean'Image (Exec_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (Exec_E));

   if Exec_OK then
      Put_Line ("[Multisig] ERROR: execute succeeded with insufficient approvals");
      return;
   end if;

   Put_Line ("[Multisig] Approving with signer 2 (valid)...");

   declare
      Sig2 : constant Signature :=
        Sign_Tx_Message (TX_ID, Block_Num, Signers_Keys (1).SK);
      Approve_Sig : Aegis_VM_Types.Byte_Array
        (0 .. Quantum_Vault.Signature_Size - 1);
   begin
      for I in Approve_Sig'Range loop
         Approve_Sig (I) := Aegis_VM_Types.Byte (Sig2 (I));
      end loop;

      Quantum_Vault.Approve_Transaction
        (TX_ID     => TX_ID,
         Signer    => Signers_Keys (1).Addr,
         Signature => Approve_Sig,
         Block_Num => Block_Num,
         Success   => App_OK,
         Error     => App_E);
   end;

   Put_Line ("  Approve (signer2): success="
     & Boolean'Image (App_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (App_E));

   if not App_OK then
      return;
   end if;

   Put_Line ("[Multisig] Executing with 2 valid approvals (should succeed)...");

   Quantum_Vault.Execute_Transaction
     (TX_ID     => TX_ID,
      Caller    => Signers_Keys (0).Addr,
      Block_Num => Block_Num,
      Success   => Exec_OK,
      Error     => Exec_E);

   Put_Line ("  Execute (2-of-2): success="
     & Boolean'Image (Exec_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (Exec_E));

   if not Exec_OK then
      Put_Line ("[Multisig] ERROR: execute failed despite sufficient approvals");
      return;
   end if;

   Put_Line ("[Multisig] Trying invalid signer (not in set)...");

   declare
      -- Reuse signer 2's key but spoof an address not in the signer set.
      Fake_Addr : Address := (others => 0);
      Sig_Fake  : constant Signature :=
        Sign_Tx_Message (TX_ID, Block_Num, Signers_Keys (1).SK);
      Approve_Sig : Aegis_VM_Types.Byte_Array
        (0 .. Quantum_Vault.Signature_Size - 1);
   begin
      for I in Approve_Sig'Range loop
         Approve_Sig (I) := Aegis_VM_Types.Byte (Sig_Fake (I));
      end loop;

      Quantum_Vault.Approve_Transaction
        (TX_ID     => TX_ID,
         Signer    => Fake_Addr,
         Signature => Approve_Sig,
         Block_Num => Block_Num,
         Success   => App_OK,
         Error     => App_E);
   end;

   Put_Line ("  Approve (fake signer): success="
     & Boolean'Image (App_OK)
     & " error=" & Quantum_Vault.Error_Code'Image (App_E));

   Put_Line ("[Multisig] Test completed.");

end Test_Quantum_Vault_Multisig;
