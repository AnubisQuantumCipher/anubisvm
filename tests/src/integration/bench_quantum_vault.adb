with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Calendar;
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

procedure Bench_Quantum_Vault is

   subtype Address is Khepri_Types.Address;

   function Seconds (DT : Ada.Calendar.Time) return Long_Float is
   begin
      return Long_Float (Ada.Calendar.Seconds (DT));
   end Seconds;

   ---------------------------------------------------------------------------
   --  Key material and address
   ---------------------------------------------------------------------------

   Key_Seed : Seed := (others => 0);
   PK       : Public_Key;
   SK       : Secret_Key;

   AAS_PK   : Public_Key_Bytes;
   AAS_Addr : Anubis_Address_Types.Address;

   Signer_Address : Address := (others => 0);

   ---------------------------------------------------------------------------
   --  Vault setup
   ---------------------------------------------------------------------------

   Signers : Quantum_Vault.Address_Array (0 .. 0);
   Weights : Quantum_Vault.Natural_Array (0 .. 0);

   Init_OK : Boolean;
   Init_E  : Quantum_Vault.Error_Code;

   ---------------------------------------------------------------------------
   --  Benchmark parameters
   ---------------------------------------------------------------------------

   Iterations : constant Natural := 50;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   procedure Setup_Key_And_Address is
   begin
      Put_Line ("[Bench] ML-DSA keygen + AAS-001 address...");

      for I in Key_Seed'Range loop
         Key_Seed (I) := Anubis_Types.Byte (I);
      end loop;

      Anubis_MLDSA.KeyGen (Key_Seed, PK, SK);

      for I in AAS_PK'Range loop
         AAS_PK (I) := Unsigned_8 (PK (I));
      end loop;

      AAS_Addr := Anubis_Address.Create_Address
        (Network    => Dev,
         Entity     => User,
         Public_Key => AAS_PK);

      for I in 0 .. 31 loop
         Signer_Address (I) :=
           Aegis_VM_Types.Byte (AAS_Addr.Account (I));
      end loop;
   end Setup_Key_And_Address;

begin
   Setup_Key_And_Address;

   ------------------------------------------------------------------------
   --  Initialize + Rotate_Key once
   ------------------------------------------------------------------------

   Signers (0) := Signer_Address;
   Weights (0) := 1;

   Put_Line ("[Bench] Initialize vault (1-of-1)...");

   declare
      T0, T1 : Ada.Calendar.Time;
   begin
      T0 := Ada.Calendar.Clock;

      Quantum_Vault.Initialize
        (Signers   => Signers,
         Weights   => Weights,
         Threshold => 1,
         Success   => Init_OK,
         Error     => Init_E);

      T1 := Ada.Calendar.Clock;

      Put_Line ("  Initialize: success="
        & Boolean'Image (Init_OK)
        & " error=" & Quantum_Vault.Error_Code'Image (Init_E));
      Put_Line ("  Initialize time (ms):"
        & Long_Float'Image ((Seconds (T1) - Seconds (T0)) * 1_000.0));

      if not Init_OK then
         return;
      end if;
   end;

   Put_Line ("[Bench] Rotate_Key to populate public key...");

   declare
      PK_Bytes  : Aegis_VM_Types.Byte_Array (0 .. 2591);
      Dummy_Sig : Aegis_VM_Types.Byte_Array
        (0 .. Quantum_Vault.Signature_Size - 1) := (others => 0);
      Rot_OK : Boolean;
      Rot_E  : Quantum_Vault.Error_Code;
      T0, T1 : Ada.Calendar.Time;
   begin
      for I in PK_Bytes'Range loop
         PK_Bytes (I) := Aegis_VM_Types.Byte (PK (I));
      end loop;

      T0 := Ada.Calendar.Clock;

      Quantum_Vault.Rotate_Key
        (Old_Address    => Signer_Address,
         New_Address    => Signer_Address,
         New_Public_Key => PK_Bytes,
         Signature      => Dummy_Sig,
         Success        => Rot_OK,
         Error          => Rot_E);

      T1 := Ada.Calendar.Clock;

      Put_Line ("  Rotate_Key: success="
        & Boolean'Image (Rot_OK)
        & " error=" & Quantum_Vault.Error_Code'Image (Rot_E));
      Put_Line ("  Rotate_Key time (ms):"
        & Long_Float'Image ((Seconds (T1) - Seconds (T0)) * 1_000.0));

      if not Rot_OK then
         return;
      end if;
   end;

   ------------------------------------------------------------------------
   --  Benchmark loop: Propose + Sign + Approve + Execute
   ------------------------------------------------------------------------

   Put_Line ("[Bench] Running "
     & Natural'Image (Iterations)
     & " end-to-end iterations...");

   declare
      Total_Elapsed : Long_Float := 0.0;
      TX_ID         : Natural := 0;

      TX_Data : Aegis_VM_Types.Byte_Array (0 .. 0) := (others => 0);

      Sig_Seed  : Seed := (others => 1);
      Sig       : Signature;

      Prop_OK : Boolean;
      Prop_E  : Quantum_Vault.Error_Code;
      App_OK  : Boolean;
      App_E   : Quantum_Vault.Error_Code;
      Exec_OK : Boolean;
      Exec_E  : Quantum_Vault.Error_Code;

   begin
      for Iter in 1 .. Iterations loop
         declare
            T0, T1 : Ada.Calendar.Time;
         begin
            T0 := Ada.Calendar.Clock;

            -- Propose
            TX_Data (0) := 0;

            Quantum_Vault.Propose_Transaction
              (Proposer    => Signer_Address,
               Destination => Signer_Address,
               Amount      => 42,
               Data        => TX_Data,
               Block_Num   => Iter,
               TX_ID       => TX_ID,
               Success     => Prop_OK,
               Error       => Prop_E);

            if not Prop_OK then
               Put_Line ("  Iter" & Natural'Image (Iter)
                 & " Propose failed: "
                 & Quantum_Vault.Error_Code'Image (Prop_E));
               return;
            end if;

            -- Build message TX_ID || Block_Num
            declare
               Msg       : Aegis_VM_Types.Byte_Array (0 .. 15);
               ID_Val    : Unsigned_64 := Unsigned_64 (TX_ID);
               Block_Val : Unsigned_64 := Unsigned_64 (Iter);
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

               -- Sign
               declare
                  Msg_For_Sign :
                    Anubis_Types.Byte_Array (0 .. Msg'Length - 1);
               begin
                  for I in Msg_For_Sign'Range loop
                     Msg_For_Sign (I) := Msg (I);
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
                     Success => App_OK);
               end;
            end;

            if not App_OK then
               Put_Line ("  Iter" & Natural'Image (Iter)
                 & " Sign failed");
               return;
            end if;

            -- Map signature and approve
            declare
               Approve_Sig : Aegis_VM_Types.Byte_Array
                 (0 .. Quantum_Vault.Signature_Size - 1);
            begin
               for I in Approve_Sig'Range loop
                  Approve_Sig (I) :=
                    Aegis_VM_Types.Byte (Sig (I));
               end loop;

               Quantum_Vault.Approve_Transaction
                 (TX_ID     => TX_ID,
                  Signer    => Signer_Address,
                  Signature => Approve_Sig,
                  Block_Num => Iter,
                  Success   => App_OK,
                  Error     => App_E);
            end;

            if not App_OK then
               Put_Line ("  Iter" & Natural'Image (Iter)
                 & " Approve failed: "
                 & Quantum_Vault.Error_Code'Image (App_E));
               return;
            end if;

            -- Execute
            Quantum_Vault.Execute_Transaction
              (TX_ID     => TX_ID,
               Caller    => Signer_Address,
               Block_Num => Iter,
               Success   => Exec_OK,
               Error     => Exec_E);

            if not Exec_OK then
               Put_Line ("  Iter" & Natural'Image (Iter)
                 & " Execute failed: "
                 & Quantum_Vault.Error_Code'Image (Exec_E));
               return;
            end if;

            T1 := Ada.Calendar.Clock;
            Total_Elapsed := Total_Elapsed +
              (Seconds (T1) - Seconds (T0));
         end;
      end loop;

      declare
         Avg_Sec : constant Long_Float :=
           (if Iterations = 0 then 0.0
            else Total_Elapsed / Long_Float (Iterations));
      begin
         Put_Line ("[Bench] Total iterations      :"
           & Natural'Image (Iterations));
         Put_Line ("[Bench] Total elapsed (ms)   :"
           & Long_Float'Image (Total_Elapsed * 1_000.0));
         Put_Line ("[Bench] Avg per iteration(ms):"
           & Long_Float'Image (Avg_Sec * 1_000.0));
         if Avg_Sec > 0.0 then
            Put_Line ("[Bench] Approx TPS (iter/s)  :"
              & Long_Float'Image (1.0 / Avg_Sec));
         end if;
      end;
   end;

   Put_Line ("[Bench] Done.");

end Bench_Quantum_Vault;

