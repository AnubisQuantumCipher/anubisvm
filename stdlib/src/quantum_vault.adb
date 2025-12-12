--  Quantum Vault: Multi-signature wallet with PQ signatures
--  Note: Implementation uses SPARK_Mode Off for complex state management
--  The spec maintains full SPARK contracts for interface verification
pragma SPARK_Mode (Off);

with Interfaces;         use Interfaces;
with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_U256;         use Aegis_U256;
with Anubis_Types;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA;

package body Quantum_Vault with
   SPARK_Mode => Off,
   Refined_State => (Vault_State =>
      (Config_Store, Signer_Set, Guardian_Set,
       Tx_Store, Total_Signatures, Sig_Verif_Gas, Stark_Batches))
is

   ---------------------------------------------------------------------------
   --  Internal State Types
   ---------------------------------------------------------------------------

   type Signer_Array is array (Natural range 0 .. Max_Signers - 1) of Signer_Info;
   type Guardian_Array is array (Natural range 0 .. Max_Guardians - 1) of Guardian_Info;
   type Tx_Array is array (Natural range 0 .. Max_Pending - 1) of Transaction_Proposal;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Config_Store : Vault_Config := (
      Threshold        => 0,
      Total_Signers    => 0,
      Total_Guardians  => 0,
      Balance          => 0,
      Next_TX_ID       => 0,
      Recovery_Active  => False,
      Recovery_Started => 0
   );

   Signer_Set   : Signer_Array   := (others => Empty_Signer);
   Guardian_Set : Guardian_Array := (others => (
      Guardian_Address => (others => 0),
      Added_At         => 0,
      Active           => False
   ));
   Tx_Store     : Tx_Array := (others => Empty_Transaction);

   Total_Signatures : Natural := 0;
   Sig_Verif_Gas    : Natural := 0;
   Stark_Batches    : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Signer_Index (Addr : Address) return Natural is
   begin
      for I in Signer_Set'Range loop
         if Signer_Set (I).Active
           and then Signer_Set (I).Signer_Address = Addr
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Signer_Index;

   function Find_Tx_Index (TX_ID : Natural) return Natural is
   begin
      for I in Tx_Store'Range loop
         if Tx_Store (I).ID = TX_ID then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Tx_Index;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Signers   : Address_Array;
      Weights   : Natural_Array;
      Threshold : Natural;
      Success   : out Boolean;
      Error     : out Error_Code
   ) is
   begin
      --  Basic sanity: enforce preconditions defensively
      if Signers'Length = 0
        or else Signers'Length > Max_Signers
        or else Signers'Length /= Weights'Length
        or else Threshold = 0
        or else Threshold > Signers'Length
      then
         Success := False;
         Error   := Error_Invalid_Threshold;
         return;
      end if;

      --  Initialize configuration
      Config_Store.Threshold        := Threshold;
      Config_Store.Total_Signers    := Signers'Length;
      Config_Store.Total_Guardians  := 0;
      Config_Store.Balance          := 0;
      Config_Store.Next_TX_ID       := 0;
      Config_Store.Recovery_Active  := False;
      Config_Store.Recovery_Started := 0;

      --  Initialize signers
      for I in Signer_Set'Range loop
         if I < Signers'Length then
            Signer_Set (I).Signer_Address :=
              Signers (Signers'First + I);
            Signer_Set (I).Public_Key := (others => 0);
            Signer_Set (I).Added_At   := 0;
            Signer_Set (I).Active     := True;
            Signer_Set (I).Weight     :=
              Weights (Weights'First + I);
         else
            Signer_Set (I) := Empty_Signer;
         end if;
      end loop;

      --  Clear guardians and transactions
      Guardian_Set := (others => (
         Guardian_Address => (others => 0),
         Added_At         => 0,
         Active           => False
      ));
      Tx_Store := (others => Empty_Transaction);

      Success := True;
      Error   := Error_None;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Address Verification
   ---------------------------------------------------------------------------

   function Verify_MLDSA87_Address (
      Addr    : Address;
      Network : Network_Type
   ) return Boolean is
   begin
      --  In this TE VM, all addresses are derived from ML-DSA-87 public keys
      --  via the AAS-001 standard. We enforce that the address is non-zero;
      --  the network parameter is part of higher-level policy.
      pragma Unreferenced (Network);

      for I in Addr'Range loop
         if Addr (I) /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Verify_MLDSA87_Address;

   procedure Extract_Public_Key (
      Addr       : Address;
      Public_Key : out Byte_Array;
      Success    : out Boolean
   ) is
      Signer_Idx : constant Natural := Find_Signer_Index (Addr);
   begin
      if Signer_Idx = Natural'Last then
         Success := False;
         --  Zero output for safety
         Public_Key := (others => 0);
         return;
      end if;

      for I in 0 .. 2591 loop
         Public_Key (Public_Key'First + I) :=
           Signer_Set (Signer_Idx).Public_Key (Public_Key'First + I);
      end loop;

      Success := True;
   end Extract_Public_Key;

   function Verify_Signature (
      Message    : Byte_Array;
      Signature  : Byte_Array;
      Public_Key : Byte_Array
   ) return Boolean is
      --  Bridge to the SPARK-verified ML-DSA implementation.
      Local_Msg  : Anubis_Types.Byte_Array (0 .. Message'Length - 1);
      PK         : Anubis_MLDSA_Types.Public_Key;
      Sig        : Anubis_MLDSA_Types.Signature;
      Valid      : Boolean;
   begin
      --  Copy message into 0-based Anubis byte array
      if Message'Length > 0 then
         for I in 0 .. Message'Length - 1 loop
            Local_Msg (I) :=
              Anubis_Types.Byte (Message (Message'First + I));
         end loop;
      end if;

      --  Copy public key
      for I in 0 .. 2591 loop
         PK (I) := Anubis_Types.Byte (Public_Key (Public_Key'First + I));
      end loop;

      --  Copy signature
      for I in 0 .. Signature_Size - 1 loop
         Sig (I) := Anubis_Types.Byte (Signature (Signature'First + I));
      end loop;

      Valid := Anubis_MLDSA.Verify (PK, Local_Msg, Sig);
      return Valid;
   end Verify_Signature;

   ---------------------------------------------------------------------------
   --  Multi-Signature Operations
   ---------------------------------------------------------------------------

   procedure Propose_Transaction (
      Proposer    : Address;
      Destination : Address;
      Amount      : Natural;
      Data        : Byte_Array;
      Block_Num   : Natural;
      TX_ID       : out Natural;
      Success     : out Boolean;
      Error       : out Error_Code
   ) is
      Idx        : Natural := Natural'Last;
      ProposerIx : constant Natural := Find_Signer_Index (Proposer);
   begin
      --  Require proposer to be an active signer
      if ProposerIx = Natural'Last then
         Success := False;
         Error   := Error_Invalid_Signer;
         TX_ID   := 0;
         return;
      end if;

      --  Find free slot
      for I in Tx_Store'Range loop
         if Tx_Store (I).ID = 0 and then not Tx_Store (I).Executed then
            Idx := I;
            exit;
         end if;
      end loop;

      if Idx = Natural'Last then
         Success := False;
         Error   := Error_Invalid_Transaction;
         TX_ID   := 0;
         return;
      end if;

      --  Allocate TX ID
      Config_Store.Next_TX_ID := Config_Store.Next_TX_ID + 1;
      TX_ID := Config_Store.Next_TX_ID;

      --  Initialize transaction record
      Tx_Store (Idx) := Empty_Transaction;
      Tx_Store (Idx).ID          := TX_ID;
      Tx_Store (Idx).Destination := Destination;
      Tx_Store (Idx).Amount      := Amount;
      Tx_Store (Idx).Proposed_At := Block_Num;
      Tx_Store (Idx).Executed    := False;

      --  Copy data (truncate if necessary)
      declare
         Copy_Len : constant Natural :=
           (if Data'Length <= Tx_Store (Idx).Data'Length then Data'Length
            else Tx_Store (Idx).Data'Length);
      begin
         for I in 0 .. Copy_Len - 1 loop
            Tx_Store (Idx).Data (I) := Data (Data'First + I);
         end loop;
         Tx_Store (Idx).Data_Length := Copy_Len;
      end;

      Success := True;
      Error   := Error_None;
   end Propose_Transaction;

   procedure Approve_Transaction (
      TX_ID      : Natural;
      Signer     : Address;
      Signature  : Byte_Array;
      Block_Num  : Natural;
      Success    : out Boolean;
      Error      : out Error_Code
   ) is
      Tx_Idx    : constant Natural := Find_Tx_Index (TX_ID);
      Signer_Ix : constant Natural := Find_Signer_Index (Signer);
      Msg       : Byte_Array (0 .. 15);
      PK        : Byte_Array (0 .. 2591);
      Sig_Valid : Boolean;
   begin
      if Tx_Idx = Natural'Last or else Signer_Ix = Natural'Last then
         Success := False;
         Error   := Error_Invalid_Transaction;
         return;
      end if;

      if Tx_Store (Tx_Idx).Executed then
         Success := False;
         Error   := Error_Invalid_Transaction;
         return;
      end if;

      --  Build a simple message binding TX_ID and Block_Num into 16 bytes
      declare
         ID_Val    : Unsigned_64 := Unsigned_64 (TX_ID);
         Block_Val : Unsigned_64 := Unsigned_64 (Block_Num);
      begin
         for I in 0 .. 7 loop
            Msg (I) := Byte (ID_Val and 16#FF#);
            ID_Val  := Shift_Right (ID_Val, 8);
         end loop;
         for I in 0 .. 7 loop
            Msg (8 + I) := Byte (Block_Val and 16#FF#);
            Block_Val   := Shift_Right (Block_Val, 8);
         end loop;
      end;

      --  Extract public key for signer
      for I in 0 .. 2591 loop
         PK (I) := Signer_Set (Signer_Ix).Public_Key (I);
      end loop;

      --  Verify signature
      Sig_Valid := Verify_Signature (Msg, Signature, PK);
      Total_Signatures := Total_Signatures + 1;

      if not Sig_Valid then
         Success := False;
         Error   := Error_Unauthorized;
         return;
      end if;

      --  Increment approval totals for valid signature
      Tx_Store (Tx_Idx).Approvals :=
        Tx_Store (Tx_Idx).Approvals + 1;
      Tx_Store (Tx_Idx).Total_Weight :=
        Tx_Store (Tx_Idx).Total_Weight + Signer_Set (Signer_Ix).Weight;

      Tx_Store (Tx_Idx).Proof_Valid := True;

      Success := True;
      Error   := Error_None;
   end Approve_Transaction;

   procedure Execute_Transaction (
      TX_ID     : Natural;
      Caller    : Address;
      Block_Num : Natural;
      Success   : out Boolean;
      Error     : out Error_Code
   ) is
      pragma Unreferenced (Caller, Block_Num);
      Tx_Idx : constant Natural := Find_Tx_Index (TX_ID);
   begin
      if Tx_Idx = Natural'Last then
         Success := False;
         Error   := Error_Invalid_Transaction;
         return;
      end if;

      if Tx_Store (Tx_Idx).Executed then
         Success := False;
         Error   := Error_Invalid_Transaction;
         return;
      end if;

      --  Require threshold of total weight
      if Tx_Store (Tx_Idx).Total_Weight < Config_Store.Threshold then
         Success := False;
         Error   := Error_Unauthorized;
         return;
      end if;

      --  Mark executed (balance changes are out of scope for demo)
      Tx_Store (Tx_Idx).Executed := True;

      Success := True;
      Error   := Error_None;
   end Execute_Transaction;

   ---------------------------------------------------------------------------
   --  Signer Management
   ---------------------------------------------------------------------------

   procedure Add_Signer (
      New_Signer : Address;
      Public_Key : Byte_Array;
      Weight     : Natural;
      Approvals  : Byte_Array;
      Success    : out Boolean;
      Error      : out Error_Code
   ) is
      pragma Unreferenced (Approvals);
      Slot : Natural := Natural'Last;
   begin
      --  Reject duplicates
      if Find_Signer_Index (New_Signer) /= Natural'Last then
         Success := False;
         Error   := Error_Invalid_Signer;
         return;
      end if;

      --  Find empty slot
      for I in Signer_Set'Range loop
         if not Signer_Set (I).Active then
            Slot := I;
            exit;
         end if;
      end loop;

      if Slot = Natural'Last then
         Success := False;
         Error   := Error_Invalid_Signer;
         return;
      end if;

      --  Add signer
      Signer_Set (Slot).Signer_Address := New_Signer;
      Signer_Set (Slot).Added_At := 0;
      Signer_Set (Slot).Active   := True;
      Signer_Set (Slot).Weight   := Weight;

      for I in 0 .. 2591 loop
         Signer_Set (Slot).Public_Key (I) :=
           Public_Key (Public_Key'First + I);
      end loop;

      Config_Store.Total_Signers :=
        Config_Store.Total_Signers + 1;

      Success := True;
      Error   := Error_None;
   end Add_Signer;

   procedure Remove_Signer (
      Signer    : Address;
      Approvals : Byte_Array;
      Success   : out Boolean;
      Error     : out Error_Code
   ) is
      pragma Unreferenced (Approvals);
      Ix : constant Natural := Find_Signer_Index (Signer);
   begin
      if Ix = Natural'Last then
         Success := False;
         Error   := Error_Invalid_Signer;
         return;
      end if;

      Signer_Set (Ix) := Empty_Signer;
      if Config_Store.Total_Signers > 0 then
         Config_Store.Total_Signers :=
           Config_Store.Total_Signers - 1;
      end if;

      Success := True;
      Error   := Error_None;
   end Remove_Signer;

   procedure Rotate_Key (
      Old_Address   : Address;
      New_Address   : Address;
      New_Public_Key : Byte_Array;
      Signature     : Byte_Array;
      Success       : out Boolean;
      Error         : out Error_Code
   ) is
      Ix : constant Natural := Find_Signer_Index (Old_Address);
      Has_Existing_PK : Boolean := False;
      PK_Bytes        : Byte_Array (0 .. 2591);
      Msg             : Byte_Array (0 .. 2591);
      Sig_Valid       : Boolean := True;
   begin
      if Ix = Natural'Last then
         Success := False;
         Error   := Error_Invalid_Signer;
         return;
      end if;

      --  If a public key is already installed for this signer, require a
      --  valid ML-DSA-87 signature under the existing key authorizing the
      --  rotation to New_Public_Key. The message for rotation is simply the
      --  new public key bytes.
      for I in 0 .. 2591 loop
         PK_Bytes (I) := Signer_Set (Ix).Public_Key (I);
         if PK_Bytes (I) /= 0 then
            Has_Existing_PK := True;
         end if;
      end loop;

      if Has_Existing_PK then
         for I in 0 .. 2591 loop
            Msg (I) := New_Public_Key (New_Public_Key'First + I);
         end loop;

         Sig_Valid := Verify_Signature (Msg, Signature, PK_Bytes);
         Total_Signatures := Total_Signatures + 1;

         if not Sig_Valid then
            Success := False;
            Error   := Error_Unauthorized;
            return;
         end if;
      end if;

      Signer_Set (Ix).Signer_Address := New_Address;
      for I in 0 .. 2591 loop
         Signer_Set (Ix).Public_Key (I) :=
           New_Public_Key (New_Public_Key'First + I);
      end loop;

      Success := True;
      Error   := Error_None;
   end Rotate_Key;

   ---------------------------------------------------------------------------
   --  Recovery
   ---------------------------------------------------------------------------

   procedure Initiate_Recovery (
      Guardian_Signatures : Byte_Array;
      Block_Num          : Natural;
      Success            : out Boolean;
      Error              : out Error_Code
   ) is
      pragma Unreferenced (Guardian_Signatures);
   begin
      if Config_Store.Recovery_Active then
         Success := False;
         Error   := Error_Recovery_Not_Active;
         return;
      end if;

      Config_Store.Recovery_Active  := True;
      Config_Store.Recovery_Started := Block_Num;

      Success := True;
      Error   := Error_None;
   end Initiate_Recovery;

   procedure Execute_Recovery (
      New_Signers : Address_Array;
      New_Weights : Natural_Array;
      Block_Num   : Natural;
      Success     : out Boolean;
      Error       : out Error_Code
   ) is
      pragma Unreferenced (Block_Num);
   begin
      if not Config_Store.Recovery_Active then
         Success := False;
         Error   := Error_Recovery_Not_Active;
         return;
      end if;

      if New_Signers'Length = 0
        or else New_Signers'Length > Max_Signers
        or else New_Signers'Length /= New_Weights'Length
      then
         Success := False;
         Error   := Error_Invalid_Threshold;
         return;
      end if;

      --  Replace signer set
      for I in Signer_Set'Range loop
         if I < New_Signers'Length then
            Signer_Set (I).Signer_Address :=
              New_Signers (New_Signers'First + I);
            Signer_Set (I).Public_Key := (others => 0);
            Signer_Set (I).Added_At   := 0;
            Signer_Set (I).Active     := True;
            Signer_Set (I).Weight     := New_Weights (New_Weights'First + I);
         else
            Signer_Set (I) := Empty_Signer;
         end if;
      end loop;

      Config_Store.Total_Signers   := New_Signers'Length;
      Config_Store.Recovery_Active := False;

      Success := True;
      Error   := Error_None;
   end Execute_Recovery;

   procedure Cancel_Recovery (
      Approvals : Byte_Array;
      Success   : out Boolean;
      Error     : out Error_Code
   ) is
      pragma Unreferenced (Approvals);
   begin
      if not Config_Store.Recovery_Active then
         Success := False;
         Error   := Error_Recovery_Not_Active;
         return;
      end if;

      Config_Store.Recovery_Active := False;
      Success := True;
      Error   := Error_None;
   end Cancel_Recovery;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Threshold return Natural is
   begin
      return Config_Store.Threshold;
   end Get_Threshold;

   function Get_Total_Signers return Natural is
   begin
      return Config_Store.Total_Signers;
   end Get_Total_Signers;

   function Get_Balance return Natural is
   begin
      return Config_Store.Balance;
   end Get_Balance;

   function Is_Signer (Addr : Address) return Boolean is
   begin
      return Find_Signer_Index (Addr) /= Natural'Last;
   end Is_Signer;

   function Get_Signer_Weight (Addr : Address) return Natural is
      Ix : constant Natural := Find_Signer_Index (Addr);
   begin
      if Ix = Natural'Last then
         return 0;
      end if;
      return Signer_Set (Ix).Weight;
   end Get_Signer_Weight;

   function Get_Transaction_Status (TX_ID : Natural) return Boolean is
      Ix : constant Natural := Find_Tx_Index (TX_ID);
   begin
      if Ix = Natural'Last then
         return False;
      end if;
      return Tx_Store (Ix).Executed;
   end Get_Transaction_Status;

   function Get_Approvals (TX_ID : Natural) return Natural is
      Ix : constant Natural := Find_Tx_Index (TX_ID);
   begin
      if Ix = Natural'Last then
         return 0;
      end if;
      return Tx_Store (Ix).Approvals;
   end Get_Approvals;

   function Get_Config return Vault_Config is
   begin
      return Config_Store;
   end Get_Config;

   ---------------------------------------------------------------------------
   --  Security Statistics
   ---------------------------------------------------------------------------

   function Get_Total_Signatures_Verified return Natural is
   begin
      return Total_Signatures;
   end Get_Total_Signatures_Verified;

   function Get_Signature_Verification_Gas return Natural is
   begin
      return Sig_Verif_Gas;
   end Get_Signature_Verification_Gas;

   function Get_STARK_Batches return Natural is
   begin
      return Stark_Batches;
   end Get_STARK_Batches;

end Quantum_Vault;
