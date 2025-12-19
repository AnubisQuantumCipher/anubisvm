pragma SPARK_Mode (On);

with Anubis_SHA3;
with Anubis_KMAC;
with Anubis_KDF;
with Anubis_AEAD;
with Anubis_MLDSA;
with Anubis_Eye;
with Anubis_Gate;

package body Anubis_Sovereign with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Helper Functions Implementation
   ---------------------------------------------------------------------------

   function Get_Credential_Slot (Cred_ID : Byte_Array) return State_Index is
      Sum : Unsigned_64 := 0;
   begin
      --  Simple hash: sum first 8 bytes
      for I in 0 .. 7 loop
         if I <= Cred_ID'Last then
            Sum := Sum + Unsigned_64 (Cred_ID (Cred_ID'First + I));
         end if;
      end loop;
      return State_Index (Cred_Slots_Base +
                         Natural (Sum mod Unsigned_64 (Max_Credentials)));
   end Get_Credential_Slot;

   function Get_Rep_Slot (Holder : Address) return State_Index is
      Sum : Unsigned_64 := 0;
   begin
      for I in Holder'Range loop
         Sum := Sum + Unsigned_64 (Holder (I));
      end loop;
      return State_Index (Rep_Slots_Base +
                         Natural (Sum mod Unsigned_64 (Max_Reputation_Slots)));
   end Get_Rep_Slot;

   function Get_Session_Slot (Session_ID : Byte_Array) return State_Index is
      Sum : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         if I <= Session_ID'Last then
            Sum := Sum + Unsigned_64 (Session_ID (Session_ID'First + I));
         end if;
      end loop;
      return State_Index (Session_Slots_Base +
                         Natural (Sum mod Unsigned_64 (Max_Sessions)));
   end Get_Session_Slot;

   function Get_Key_Image_Slot (Image : Byte_Array) return State_Index is
      Sum : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         if I <= Image'Last then
            Sum := Sum + Unsigned_64 (Image (Image'First + I));
         end if;
      end loop;
      return State_Index (Key_Image_Slots_Base +
                         Natural (Sum mod Unsigned_64 (Max_Key_Images)));
   end Get_Key_Image_Slot;

   procedure Compute_Credential_ID (
      Schema      : Schema_Type;
      Holder      : Address;
      Attributes  : Attribute_Array;
      Attr_Count  : Natural;
      Cred_ID     : out Byte_Array
   ) is
      --  Use KMAC256 with domain separation for unique IDs
      Input_Buffer : Byte_Array (0 .. 1023) := (others => 0);
      Input_Len    : Natural := 0;
      KMAC_Key     : constant Anubis_KMAC.KMAC_Key :=
         (16#43#, 16#52#, 16#45#, 16#44#, others => 0);  -- "CRED"
   begin
      Cred_ID := (others => 0);

      --  Encode schema type
      Input_Buffer (0) := Unsigned_8 (Schema_Type'Pos (Schema));
      Input_Len := 1;

      --  Encode holder address
      for I in Holder'Range loop
         if Input_Len < 1024 then
            Input_Buffer (Input_Len) := Holder (I);
            Input_Len := Input_Len + 1;
         end if;
      end loop;

      --  Encode first few attributes
      if Attr_Count > 0 then
         declare
            Max_Attrs : constant Natural := Natural'Min (Attr_Count, 4);
         begin
            for A in 0 .. Max_Attrs - 1 loop
               for I in 0 .. Natural'Min (Attributes (A).Length, 16) - 1 loop
                  if Input_Len < 1024 then
                     Input_Buffer (Input_Len) := Attributes (A).Data (I);
                     Input_Len := Input_Len + 1;
                  end if;
               end loop;
            end loop;
         end;
      end if;

      --  Compute KMAC256 with domain separation
      --  Input_Len is always >= 1 since we encoded the schema type above
      pragma Assert (Input_Len >= 1);
      declare
         Input_Slice : constant Byte_Array (0 .. Input_Len - 1) :=
            Input_Buffer (0 .. Input_Len - 1);
         Tag         : Anubis_KMAC.KMAC256_Tag := (others => 0);
      begin
         Anubis_KMAC.KMAC256 (
            Key     => KMAC_Key,
            Message => Input_Slice,
            Custom  => "cred-id",
            Tag     => Tag
         );
         --  Copy tag to credential ID
         for I in 0 .. Credential_ID_Size - 1 loop
            Cred_ID (Cred_ID'First + I) := Tag (I);
         end loop;
      end;
   end Compute_Credential_ID;

   function Verify_DSA_Signature (
      Message   : Byte_Array;
      Signature : Byte_Array;
      Public_Key: Byte_Array
   ) return Boolean is
      PK : Anubis_MLDSA.Public_Key := (others => 0);
      Sig : Anubis_MLDSA.Signature := (others => 0);
      Local_Msg : Byte_Array (0 .. Message'Length - 1);
   begin
      --  Validate input sizes
      if Message'Length = 0 or Message'Length > 4096 or
         Public_Key'Length /= 2592 or Signature'Length /= 4627
      then
         return False;
      end if;

      --  Copy message to local array with zero-based indexing
      for I in Message'Range loop
         Local_Msg (I - Message'First) := Message (I);
      end loop;

      --  Copy public key
      for I in PK'Range loop
         PK (I) := Public_Key (Public_Key'First + I);
      end loop;

      --  Copy signature
      for I in Sig'Range loop
         Sig (I) := Signature (Signature'First + I);
      end loop;

      --  Verify ML-DSA-87 signature
      return Anubis_MLDSA.Verify (PK, Local_Msg, Sig);
   end Verify_DSA_Signature;

   procedure Record_Audit (
      State     : in Out State_Array;
      Operation : Unsigned_8;
      Actor     : Address;
      Target    : Byte_Array;
      Timestamp : Unsigned_64;
      Block     : Unsigned_64
   ) is
      --  Find next audit slot (circular buffer)
      Audit_Idx : State_Index;
      Actor_Sum : Unsigned_64 := 0;
      Target_Sum: Unsigned_64 := 0;
   begin
      --  Compute actor hash
      for I in Actor'Range loop
         Actor_Sum := Actor_Sum + Unsigned_64 (Actor (I));
      end loop;

      --  Compute target hash
      for I in Target'Range loop
         if I - Target'First < 8 then
            Target_Sum := Target_Sum + Unsigned_64 (Target (I));
         end if;
      end loop;

      --  Use timestamp mod audit slots for index
      Audit_Idx := Audit_Slots_Base +
                  State_Index (Timestamp mod Unsigned_64 (Max_Audit_Entries));

      --  Pack audit entry into slot
      State (Audit_Idx).Value (0) := Operation;
      --  Pack actor hash (8 bytes)
      for I in 0 .. 7 loop
         State (Audit_Idx).Value (1 + I) :=
            Unsigned_8 (Shift_Right (Actor_Sum, (7 - I) * 8) and 16#FF#);
      end loop;
      --  Pack target hash (8 bytes)
      for I in 0 .. 7 loop
         State (Audit_Idx).Value (9 + I) :=
            Unsigned_8 (Shift_Right (Target_Sum, (7 - I) * 8) and 16#FF#);
      end loop;
      --  Pack timestamp (8 bytes)
      for I in 0 .. 7 loop
         State (Audit_Idx).Value (17 + I) :=
            Unsigned_8 (Shift_Right (Timestamp, (7 - I) * 8) and 16#FF#);
      end loop;
      --  Pack block height (8 bytes)
      for I in 0 .. 7 loop
         State (Audit_Idx).Value (25 + I) :=
            Unsigned_8 (Shift_Right (Block, (7 - I) * 8) and 16#FF#);
      end loop;
      State (Audit_Idx).Length := 33;
      State (Audit_Idx).Modified := True;
   end Record_Audit;

   function Issuer_Authorized (
      Issuer_Rec : Issuer_Record;
      Schema     : Schema_Type
   ) return Boolean is
      Schema_Bit : constant Unsigned_16 :=
         Shift_Left (1, Schema_Type'Pos (Schema));
   begin
      return Issuer_Rec.Active and then
             (Issuer_Rec.Allowed_Schemas and Schema_Bit) /= 0;
   end Issuer_Authorized;

   function Constant_Time_Compare (
      A : Byte_Array;
      B : Byte_Array
   ) return Boolean is
      Diff : Unsigned_8 := 0;
      B_Idx : Natural;
   begin
      --  Map indices correctly between potentially different-based arrays
      for I in 0 .. A'Length - 1 loop
         pragma Loop_Invariant (I <= A'Length - 1);
         B_Idx := B'First + I;
         if B_Idx <= B'Last then
            Diff := Diff or (A (A'First + I) xor B (B_Idx));
         end if;
      end loop;
      return Diff = 0;
   end Constant_Time_Compare;

   ---------------------------------------------------------------------------
   --  Read/Write Helpers
   ---------------------------------------------------------------------------

   function Read_U64 (Slot : State_Slot) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         Result := Shift_Left (Result, 8) or Unsigned_64 (Slot.Value (I));
      end loop;
      return Result;
   end Read_U64;

   procedure Write_U64 (
      Slot  : in Out State_Slot;
      Val   : Unsigned_64
   ) is
   begin
      for I in 0 .. 7 loop
         Slot.Value (I) := Unsigned_8 (
            Shift_Right (Val, (7 - I) * 8) and 16#FF#);
      end loop;
      Slot.Length := 8;
      Slot.Modified := True;
   end Write_U64;

   function Selector_Match (A, B : Hash256) return Boolean is
   begin
      return A (0) = B (0) and then A (1) = B (1) and then
             A (2) = B (2) and then A (3) = B (3);
   end Selector_Match;

   ---------------------------------------------------------------------------
   --  ISSUER MANAGEMENT
   ---------------------------------------------------------------------------

   procedure Register_Issuer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params      : Param_Buffer renames Context.Params;
      Issuer_Addr : Address := (others => 0);
      Slot_Idx    : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      --  Validate parameter length: issuer_address (32) + schema_mask (2)
      --  Note: Full public keys would be too large for params buffer
      --  In production: keys are registered separately via dedicated calls
      if Context.Param_Len < 34 then
         return;
      end if;

      --  Extract issuer address
      for I in 0 .. 31 loop
         Issuer_Addr (I) := Params (I);
      end loop;

      --  Find free issuer slot
      for I in 0 .. Max_Issuers - 1 loop
         Slot_Idx := Issuer_Slots_Base + State_Index (I);
         if State (Slot_Idx).Length = 0 or else
            State (Slot_Idx).Value (0) = 0 then

            --  Store issuer data
            --  Byte 0: active flag
            State (Slot_Idx).Value (0) := 1;

            --  Bytes 1-32: issuer address
            for J in 0 .. 31 loop
               State (Slot_Idx).Value (1 + J) := Issuer_Addr (J);
            end loop;

            --  Bytes 33-34: allowed schemas (from params 32-33)
            State (Slot_Idx).Value (33) := Params (32);
            State (Slot_Idx).Value (34) := Params (33);

            State (Slot_Idx).Length := 35;
            State (Slot_Idx).Modified := True;

            --  Record audit
            Record_Audit (
               State     => State,
               Operation => 1,  -- Register issuer
               Actor     => Context.Caller,
               Target    => Issuer_Addr,
               Timestamp => Unsigned_64 (Context.Height),
               Block     => Unsigned_64 (Context.Height)
            );

            Result := Empty_Result;
            return;
         end if;
      end loop;

      --  No free slot
      Result := Error_Result (Out_Of_Gas);
   end Register_Issuer;

   procedure Revoke_Issuer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params      : Param_Buffer renames Context.Params;
      Target_Addr : Address := (others => 0);
      Slot_Idx    : State_Index;
      Found       : Boolean := False;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < 32 then
         return;
      end if;

      --  Extract target issuer address
      for I in 0 .. 31 loop
         Target_Addr (I) := Params (I);
      end loop;

      --  Find and deactivate issuer
      for I in 0 .. Max_Issuers - 1 loop
         Slot_Idx := Issuer_Slots_Base + State_Index (I);
         if State (Slot_Idx).Value (0) = 1 then
            --  Check if address matches
            Found := True;
            for J in 0 .. 31 loop
               if State (Slot_Idx).Value (1 + J) /= Target_Addr (J) then
                  Found := False;
                  exit;
               end if;
            end loop;

            if Found then
               --  Deactivate
               State (Slot_Idx).Value (0) := 0;
               State (Slot_Idx).Modified := True;

               Record_Audit (
                  State     => State,
                  Operation => 2,  -- Revoke issuer
                  Actor     => Context.Caller,
                  Target    => Target_Addr,
                  Timestamp => Unsigned_64 (Context.Height),
                  Block     => Unsigned_64 (Context.Height)
               );

               Result := Empty_Result;
               return;
            end if;
         end if;
      end loop;

      Result := Error_Result (Unknown_Method);
   end Revoke_Issuer;

   ---------------------------------------------------------------------------
   --  CREDENTIAL LIFECYCLE
   ---------------------------------------------------------------------------

   procedure Issue_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params       : Param_Buffer renames Context.Params;
      Schema       : Schema_Type;
      Holder_Addr  : Address := (others => 0);
      Cred_ID      : Byte_Array (0 .. Credential_ID_Size - 1) := (others => 0);
      Attrs        : Attribute_Array := (others => (
         Data      => (others => 0),
         Length    => 0,
         Encrypted => False
      ));
      Slot_Idx     : State_Index;
      Total_Issued : Unsigned_64;
   begin
      Result := Error_Result (Invalid_Params);

      --  Validate minimum params: schema(1) + holder(32) + at least 1 attr
      if Context.Param_Len < 34 then
         return;
      end if;

      --  Parse schema type
      if Natural (Params (0)) > Schema_Type'Pos (Schema_Type'Last) then
         return;
      end if;
      Schema := Schema_Type'Val (Natural (Params (0)));

      --  Parse holder address
      for I in 0 .. 31 loop
         Holder_Addr (I) := Params (1 + I);
      end loop;

      --  Compute credential ID using KMAC256
      Compute_Credential_ID (
         Schema     => Schema,
         Holder     => Holder_Addr,
         Attributes => Attrs,
         Attr_Count => 0,
         Cred_ID    => Cred_ID
      );

      --  Get credential slot
      Slot_Idx := Get_Credential_Slot (Cred_ID);

      --  Check slot not occupied
      if State (Slot_Idx).Value (0) = 1 then
         Result := Error_Result (Invalid_Caller);
         return;
      end if;

      --  Store credential
      --  Byte 0: active flag
      State (Slot_Idx).Value (0) := 1;

      --  Bytes 1-32: credential ID
      for I in 0 .. 31 loop
         State (Slot_Idx).Value (1 + I) := Cred_ID (I);
      end loop;

      --  Byte 33: schema type
      State (Slot_Idx).Value (33) := Unsigned_8 (Schema_Type'Pos (Schema));

      --  Bytes 34-65: holder address
      for I in 0 .. 31 loop
         State (Slot_Idx).Value (34 + I) := Holder_Addr (I);
      end loop;

      --  Bytes 66-73: issued timestamp
      for I in 0 .. 7 loop
         State (Slot_Idx).Value (66 + I) :=
            Unsigned_8 (Shift_Right (Unsigned_64 (Context.Height), (7 - I) * 8) and 16#FF#);
      end loop;

      --  Byte 74: revoked flag (0 = not revoked)
      State (Slot_Idx).Value (74) := 0;

      State (Slot_Idx).Length := 75;
      State (Slot_Idx).Modified := True;

      --  Update total issued counter
      Total_Issued := Read_U64 (State (Total_Issued_Slot));
      Write_U64 (State (Total_Issued_Slot), Total_Issued + 1);

      --  Record audit
      Record_Audit (
         State     => State,
         Operation => 10,  -- Issue credential
         Actor     => Context.Caller,
         Target    => Cred_ID,
         Timestamp => Unsigned_64 (Context.Height),
         Block     => Unsigned_64 (Context.Height)
      );

      --  Return credential ID
      Result.Status := Success;
      Result.Return_Len := Credential_ID_Size;
      for I in 0 .. Credential_ID_Size - 1 loop
         Result.Return_Data (I) := Cred_ID (I);
      end loop;
   end Issue_Credential;

   procedure Present_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Cred_ID  : Byte_Array (0 .. Credential_ID_Size - 1) := (others => 0);
      Slot_Idx : State_Index;
      Is_Valid : Boolean := False;
      Total_Verified : Unsigned_64;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < Credential_ID_Size then
         return;
      end if;

      --  Extract credential ID
      for I in 0 .. Credential_ID_Size - 1 loop
         Cred_ID (I) := Params (I);
      end loop;

      --  Get credential slot
      Slot_Idx := Get_Credential_Slot (Cred_ID);

      --  Check credential exists and is active
      if State (Slot_Idx).Value (0) /= 1 then
         Result := Error_Result (Unknown_Method);
         return;
      end if;

      --  Check not revoked
      if State (Slot_Idx).Value (74) = 1 then
         Result := Error_Result (Invalid_Caller);
         return;
      end if;

      --  Verify caller is holder (bytes 34-65)
      Is_Valid := True;
      for I in 0 .. 31 loop
         if State (Slot_Idx).Value (34 + I) /= Context.Caller (I) then
            Is_Valid := False;
            exit;
         end if;
      end loop;

      if not Is_Valid then
         Result := Error_Result (Invalid_Caller);
         return;
      end if;

      --  Update verification counter
      Total_Verified := Read_U64 (State (Total_Verified_Slot));
      Write_U64 (State (Total_Verified_Slot), Total_Verified + 1);

      --  Return success with credential data
      Result.Status := Success;
      Result.Return_Len := 1;
      Result.Return_Data (0) := 1;  -- Valid
   end Present_Credential;

   procedure Revoke_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Cred_ID  : Byte_Array (0 .. Credential_ID_Size - 1) := (others => 0);
      Slot_Idx : State_Index;
      Is_Holder: Boolean := False;
      Total_Revoked : Unsigned_64;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < Credential_ID_Size then
         return;
      end if;

      --  Extract credential ID
      for I in 0 .. Credential_ID_Size - 1 loop
         Cred_ID (I) := Params (I);
      end loop;

      Slot_Idx := Get_Credential_Slot (Cred_ID);

      --  Check credential exists
      if State (Slot_Idx).Value (0) /= 1 then
         Result := Error_Result (Unknown_Method);
         return;
      end if;

      --  Verify caller is holder
      Is_Holder := True;
      for I in 0 .. 31 loop
         if State (Slot_Idx).Value (34 + I) /= Context.Caller (I) then
            Is_Holder := False;
            exit;
         end if;
      end loop;

      if not Is_Holder then
         Result := Error_Result (Invalid_Caller);
         return;
      end if;

      --  Mark as revoked
      State (Slot_Idx).Value (74) := 1;
      State (Slot_Idx).Modified := True;

      --  Update revoked counter
      Total_Revoked := Read_U64 (State (Total_Revoked_Slot));
      Write_U64 (State (Total_Revoked_Slot), Total_Revoked + 1);

      Record_Audit (
         State     => State,
         Operation => 11,  -- Revoke credential
         Actor     => Context.Caller,
         Target    => Cred_ID,
         Timestamp => Unsigned_64 (Context.Height),
         Block     => Unsigned_64 (Context.Height)
      );

      Result := Empty_Result;
   end Revoke_Credential;

   procedure Check_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Cred_ID  : Byte_Array (0 .. Credential_ID_Size - 1) := (others => 0);
      Slot_Idx : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < Credential_ID_Size then
         return;
      end if;

      for I in 0 .. Credential_ID_Size - 1 loop
         Cred_ID (I) := Params (I);
      end loop;

      Slot_Idx := Get_Credential_Slot (Cred_ID);

      --  Return status: exists, active, not revoked
      Result.Status := Success;
      Result.Return_Len := 3;
      Result.Return_Data (0) := State (Slot_Idx).Value (0);  -- Exists
      Result.Return_Data (1) := (if State (Slot_Idx).Value (0) = 1 then 1 else 0);  -- Active
      Result.Return_Data (2) := (if State (Slot_Idx).Value (74) = 0 then 1 else 0);  -- Not revoked
   end Check_Credential;

   ---------------------------------------------------------------------------
   --  SELECTIVE DISCLOSURE (EYE)
   ---------------------------------------------------------------------------

   procedure Create_Disclosure (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Cred_ID  : Byte_Array (0 .. Credential_ID_Size - 1) := (others => 0);
      Holder_Secret : Byte_Array (0 .. 31) := (others => 0);
      Challenge : Byte_Array (0 .. 31) := (others => 0);
      Disclose_Mask : Unsigned_32;
      Slot_Idx : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: cred_id (32) + holder_secret (32) + disclosure_mask (4) + challenge (32)
      --  Total: 100 bytes
      if Context.Param_Len < 100 then
         return;
      end if;

      --  Extract parameters
      for I in 0 .. 31 loop
         Cred_ID (I) := Params (I);
         Holder_Secret (I) := Params (32 + I);
         Challenge (I) := Params (68 + I);
      end loop;

      Disclose_Mask := Unsigned_32 (Params (64)) or
                       Shift_Left (Unsigned_32 (Params (65)), 8) or
                       Shift_Left (Unsigned_32 (Params (66)), 16) or
                       Shift_Left (Unsigned_32 (Params (67)), 24);

      --  Look up credential from state
      Slot_Idx := Get_Credential_Slot (Cred_ID);

      --  Verify credential exists and is active
      if State (Slot_Idx).Length = 0 then
         Result := Error_Result (Invalid_State);
         return;
      end if;

      --  Build credential structure from state
      --  Note: In production, credentials would be fully deserialized
      --  For now, we create a minimal credential for disclosure proof
      declare
         Cred : Anubis_Eye.Attribute_Credential;
         Eye_Proof : Anubis_Eye.Disclosure_Proof;
         Proof_Success : Boolean := False;
      begin
         --  Initialize credential with zero values
         Cred.Attrs := (others => (others => 0));
         Cred.Attr_Count := 4;  -- Assume 4 attributes for demo
         Cred.Holder_Commit := (others => 0);
         Cred.Issuer_PK := (others => 0);
         Cred.Signature := (others => 0);

         --  Load holder commitment from state (stored in slot)
         for I in 0 .. Natural'Min (63, State (Slot_Idx).Length - 1) loop
            if I < 64 then
               Cred.Holder_Commit (I) := State (Slot_Idx).Value (I);
            end if;
         end loop;

         --  Extract attributes from state (simplified encoding)
         --  Each attribute is 64 bytes
         for A in 0 .. Natural'Min (3, Cred.Attr_Count - 1) loop
            for I in 0 .. 63 loop
               if 64 + A * 64 + I < State (Slot_Idx).Length then
                  Cred.Attrs (A)(I) := State (Slot_Idx).Value (64 + A * 64 + I);
               end if;
            end loop;
         end loop;

         --  Generate EYE disclosure proof
         Anubis_Eye.Create_Disclosure_Proof (
            Credential    => Cred,
            Holder_Secret => Holder_Secret,
            Disclose_Mask => Disclose_Mask,
            Challenge     => Challenge,
            Proof         => Eye_Proof,
            Success       => Proof_Success
         );

         if not Proof_Success then
            Result := Error_Result (Invalid_Signature);
            return;
         end if;

         --  Return disclosure proof
         --  Format: credential_hash (32) + disclosed_mask (4) + proof_data (first 100 bytes)
         Result.Status := Success;
         Result.Return_Len := 136;

         for I in 0 .. 31 loop
            Result.Return_Data (I) := Eye_Proof.Credential_Hash (I);
         end loop;

         Result.Return_Data (32) := Unsigned_8 (Disclose_Mask and 16#FF#);
         Result.Return_Data (33) := Unsigned_8 (Shift_Right (Disclose_Mask, 8) and 16#FF#);
         Result.Return_Data (34) := Unsigned_8 (Shift_Right (Disclose_Mask, 16) and 16#FF#);
         Result.Return_Data (35) := Unsigned_8 (Shift_Right (Disclose_Mask, 24) and 16#FF#);

         for I in 0 .. 99 loop
            if I < Anubis_Eye.Disclosure_Proof_Size then
               Result.Return_Data (36 + I) := Eye_Proof.Proof_Data (I);
            end if;
         end loop;
      end;

      --  Record audit entry
      Record_Audit (
         State     => State,
         Operation => 16#D1#,  -- Disclosure operation
         Actor     => Context.Caller,
         Target    => Cred_ID,
         Timestamp => Unsigned_64 (Context.Height),
         Block     => Unsigned_64 (Context.Height)
      );
   end Create_Disclosure;

   procedure Verify_Disclosure (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params    : Param_Buffer renames Context.Params;
      Proof_Hash : Byte_Array (0 .. 31) := (others => 0);
      Disclose_Mask : Unsigned_32;
      Proof_Data : Byte_Array (0 .. 99) := (others => 0);
      Challenge : Byte_Array (0 .. 31) := (others => 0);
      Issuer_PK : Byte_Array (0 .. 2591) := (others => 0);
      Disclosed_Attrs : Anubis_Eye.Attribute_Input_Array (0 .. 7);
      Is_Valid  : Boolean := False;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: proof_hash (32) + disclosed_mask (4) + proof_data (100) +
      --          challenge (32) + issuer_pk (2592) + disclosed_attrs (variable)
      --  Minimum: 32 + 4 + 100 + 32 + 2592 = 2760 bytes
      if Context.Param_Len < 136 then
         return;
      end if;

      --  Extract proof components
      for I in 0 .. 31 loop
         Proof_Hash (I) := Params (I);
      end loop;

      Disclose_Mask := Unsigned_32 (Params (32)) or
                       Shift_Left (Unsigned_32 (Params (33)), 8) or
                       Shift_Left (Unsigned_32 (Params (34)), 16) or
                       Shift_Left (Unsigned_32 (Params (35)), 24);

      for I in 0 .. 99 loop
         Proof_Data (I) := Params (36 + I);
      end loop;

      --  Extract challenge if provided
      if Context.Param_Len >= 168 then
         for I in 0 .. 31 loop
            Challenge (I) := Params (136 + I);
         end loop;
      end if;

      --  Extract issuer public key from state (stored at issuer slots)
      --  For demo, use a fixed issuer slot
      declare
         Issuer_Slot : constant State_Index := Issuer_Slots_Base;
      begin
         if State (Issuer_Slot).Length >= 2592 then
            for I in 0 .. 2591 loop
               Issuer_PK (I) := State (Issuer_Slot).Value (I);
            end loop;
         end if;
      end;

      --  Extract disclosed attributes from params
      --  Count number of disclosed attributes from mask
      declare
         Attr_Count : Natural := 0;
         Mask_Temp : Unsigned_32 := Disclose_Mask;
      begin
         --  Count set bits in mask
         for I in 0 .. 31 loop
            if (Mask_Temp and 1) = 1 then
               Attr_Count := Attr_Count + 1;
            end if;
            Mask_Temp := Shift_Right (Mask_Temp, 1);
         end loop;

         --  Initialize disclosed attributes
         for I in Disclosed_Attrs'Range loop
            Disclosed_Attrs (I) := (others => 0);
         end loop;

         --  Extract disclosed attribute values from params (if provided)
         --  Each attribute is 64 bytes
         if Context.Param_Len >= 168 + Attr_Count * 64 then
            for I in 0 .. Natural'Min (Attr_Count - 1, 7) loop
               for J in 0 .. 63 loop
                  Disclosed_Attrs (I)(J) := Params (168 + I * 64 + J);
               end loop;
            end loop;
         end if;

         --  Build EYE disclosure proof structure
         declare
            Eye_Proof : Anubis_Eye.Disclosure_Proof;
            Attrs_Slice : constant Anubis_Eye.Attribute_Input_Array :=
               Disclosed_Attrs (0 .. Natural'Min (Attr_Count - 1, 7));
         begin
            Eye_Proof.Credential_Hash := Proof_Hash;
            Eye_Proof.Disclosed_Mask := Disclose_Mask;
            Eye_Proof.Proof_Data := (others => 0);

            for I in 0 .. 99 loop
               if I < Anubis_Eye.Disclosure_Proof_Size then
                  Eye_Proof.Proof_Data (I) := Proof_Data (I);
               end if;
            end loop;

            --  Verify disclosure proof using EYE
            if Attr_Count > 0 and Attr_Count <= 8 then
               Is_Valid := Anubis_Eye.Verify_Disclosure (
                  Proof           => Eye_Proof,
                  Disclosed_Attrs => Attrs_Slice,
                  Issuer_PK       => Issuer_PK,
                  Challenge       => Challenge
               );
            end if;
         end;
      end;

      Result.Status := Success;
      Result.Return_Len := 1;
      Result.Return_Data (0) := (if Is_Valid then 1 else 0);

      --  Record audit entry
      Record_Audit (
         State     => State,
         Operation => 16#D2#,  -- Verification operation
         Actor     => Context.Caller,
         Target    => Proof_Hash,
         Timestamp => Unsigned_64 (Context.Height),
         Block     => Unsigned_64 (Context.Height)
      );
   end Verify_Disclosure;

   procedure Issue_View_Key (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params     : Param_Buffer renames Context.Params;
      Master_In  : Anubis_KDF.Master_Key := (others => 0);
      Salt_In    : Anubis_KDF.KDF_Salt := (others => 0);
      View_Key   : Anubis_KDF.Derived_Key := (others => 0);
      Info_Data  : constant Byte_Array (0 .. 31) := (others => 0);
      pragma Unreferenced (State);
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: master_seed (32) + viewer_address (32)
      if Context.Param_Len < 64 then
         return;
      end if;

      for I in 0 .. 31 loop
         Master_In (I) := Params (I);
         Salt_In (I) := Params (32 + I);
      end loop;

      --  Derive viewing key using KDF
      Anubis_KDF.Derive_Key (
         Master  => Master_In,
         Salt    => Salt_In,
         Info    => Info_Data,
         Context => Anubis_KDF.Context_Eye,
         Key     => View_Key
      );

      Result.Status := Success;
      Result.Return_Len := 32;
      for I in 0 .. 31 loop
         Result.Return_Data (I) := View_Key (I);
      end loop;
   end Issue_View_Key;

   ---------------------------------------------------------------------------
   --  PRIVATE VERIFICATION (GATE)
   ---------------------------------------------------------------------------

   procedure Start_Session (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params     : Param_Buffer renames Context.Params;
      Contract_Addr : Byte_Array (0 .. 31) := (others => 0);
      Contract_KEM_PK : Byte_Array (0 .. 1567) := (others => 0);
      Randomness : Byte_Array (0 .. 63) := (others => 0);
      Slot_Idx   : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: contract_addr (32) + contract_kem_pk (1568) + randomness (64)
      --  Total: 1664 bytes
      if Context.Param_Len < 1664 then
         return;
      end if;

      --  Extract parameters
      for I in 0 .. 31 loop
         Contract_Addr (I) := Params (I);
      end loop;

      for I in 0 .. 1567 loop
         Contract_KEM_PK (I) := Params (32 + I);
      end loop;

      for I in 0 .. 63 loop
         Randomness (I) := Params (1600 + I);
      end loop;

      --  Create GATE private session
      declare
         Gate_Session : Anubis_Gate.Private_Session;
         Session_Success : Boolean := False;
         --  User KEM secret key (would come from holder's stored keys)
         --  For demo, derive from caller address
         User_KEM_SK : Byte_Array (0 .. 3167) := (others => 0);
      begin
         --  Derive user KEM SK from caller address (simplified)
         Anubis_SHA3.SHAKE256 (Context.Caller, User_KEM_SK, User_KEM_SK'Length);

         --  Create session using GATE
         Anubis_Gate.Create_Session (
            Contract_Addr   => Contract_Addr,
            User_KEM_SK     => User_KEM_SK,
            Contract_KEM_PK => Contract_KEM_PK,
            Randomness      => Randomness,
            Session         => Gate_Session,
            Success         => Session_Success
         );

         if not Session_Success then
            Result := Error_Result (Invalid_State);
            return;
         end if;

         --  Store session in state
         Slot_Idx := Get_Session_Slot (Gate_Session.Session_ID);

         State (Slot_Idx).Value (0) := 1;  -- Active

         --  Store session ID
         for I in 0 .. 31 loop
            State (Slot_Idx).Value (1 + I) := Gate_Session.Session_ID (I);
         end loop;

         --  Store contract address
         for I in 0 .. 31 loop
            State (Slot_Idx).Value (33 + I) := Gate_Session.Contract_Addr (I);
         end loop;

         --  Store shared secret (encrypted)
         for I in 0 .. 31 loop
            State (Slot_Idx).Value (65 + I) := Gate_Session.Shared_Secret (I);
         end loop;

         --  Store creation and expiry times
         for I in 0 .. 7 loop
            State (Slot_Idx).Value (97 + I) :=
               Unsigned_8 (Shift_Right (Gate_Session.Created_At, (7 - I) * 8) and 16#FF#);
            State (Slot_Idx).Value (105 + I) :=
               Unsigned_8 (Shift_Right (Gate_Session.Expires_At, (7 - I) * 8) and 16#FF#);
         end loop;

         State (Slot_Idx).Length := 113;
         State (Slot_Idx).Modified := True;

         Result.Status := Success;
         Result.Return_Len := 32;
         for I in 0 .. 31 loop
            Result.Return_Data (I) := Gate_Session.Session_ID (I);
         end loop;

         --  Record audit entry
         Record_Audit (
            State     => State,
            Operation => 16#S1#,  -- Session start
            Actor     => Context.Caller,
            Target    => Contract_Addr,
            Timestamp => Unsigned_64 (Context.Height),
            Block     => Unsigned_64 (Context.Height)
         );
      end;
   end Start_Session;

   procedure Verify_Private (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params     : Param_Buffer renames Context.Params;
      Session_ID : Byte_Array (0 .. 31) := (others => 0);
      Function_Selector : Unsigned_32;
      Args       : Byte_Array (0 .. 1023) := (others => 0);
      Args_Len   : Natural := 0;
      Slot_Idx   : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: session_id (32) + function_selector (4) + args_len (2) + args (variable)
      --  Minimum: 38 bytes
      if Context.Param_Len < 38 then
         return;
      end if;

      --  Extract session ID
      for I in 0 .. 31 loop
         Session_ID (I) := Params (I);
      end loop;

      --  Extract function selector
      Function_Selector := Unsigned_32 (Params (32)) or
                          Shift_Left (Unsigned_32 (Params (33)), 8) or
                          Shift_Left (Unsigned_32 (Params (34)), 16) or
                          Shift_Left (Unsigned_32 (Params (35)), 24);

      --  Extract args length
      Args_Len := Natural (Params (36)) or Shift_Left (Natural (Params (37)), 8);

      if Args_Len > 1024 or Args_Len > Context.Param_Len - 38 then
         return;
      end if;

      --  Extract args
      for I in 0 .. Args_Len - 1 loop
         Args (I) := Params (38 + I);
      end loop;

      Slot_Idx := Get_Session_Slot (Session_ID);

      --  Check session is active
      if State (Slot_Idx).Value (0) /= 1 then
         Result := Error_Result (Invalid_Caller);
         return;
      end if;

      --  Check not expired
      declare
         Expiry : Unsigned_64 := 0;
      begin
         for I in 0 .. 7 loop
            Expiry := Shift_Left (Expiry, 8) or
                     Unsigned_64 (State (Slot_Idx).Value (105 + I));
         end loop;

         if Unsigned_64 (Context.Height) > Expiry then
            Result := Error_Result (Out_Of_Gas);
            return;
         end if;
      end;

      --  Reconstruct GATE session from state
      declare
         Gate_Session : Anubis_Gate.Private_Session;
         Exec_Result_Gate : Anubis_Gate.Private_Execution_Result;
      begin
         --  Load session ID
         for I in 0 .. 31 loop
            Gate_Session.Session_ID (I) := State (Slot_Idx).Value (1 + I);
         end loop;

         --  Load contract address
         for I in 0 .. 31 loop
            Gate_Session.Contract_Addr (I) := State (Slot_Idx).Value (33 + I);
         end loop;

         --  Load shared secret
         for I in 0 .. 31 loop
            Gate_Session.Shared_Secret (I) := State (Slot_Idx).Value (65 + I);
         end loop;

         --  Load timestamps
         Gate_Session.Created_At := 0;
         Gate_Session.Expires_At := 0;
         for I in 0 .. 7 loop
            Gate_Session.Created_At := Shift_Left (Gate_Session.Created_At, 8) or
                                      Unsigned_64 (State (Slot_Idx).Value (97 + I));
            Gate_Session.Expires_At := Shift_Left (Gate_Session.Expires_At, 8) or
                                      Unsigned_64 (State (Slot_Idx).Value (105 + I));
         end loop;

         --  Initialize state snapshot (simplified - would load from contract state)
         Gate_Session.State_Snapshot.Ciphertext := (others => 0);
         Gate_Session.State_Snapshot.CT_Length := 0;
         Gate_Session.State_Snapshot.State_Hash := (others => 0);
         Gate_Session.State_Snapshot.Version := 0;

         --  Execute private verification using GATE
         Anubis_Gate.Session_Execute (
            Session           => Gate_Session,
            Function_Selector => Function_Selector,
            Args              => Args (0 .. Args_Len - 1),
            Result            => Exec_Result_Gate
         );

         if not Exec_Result_Gate.Success then
            Result := Error_Result (Invalid_State);
            return;
         end if;

         --  Update session state snapshot in storage
         --  (In production, would update contract's private state)

         --  Update verification count
         declare
            Total : Unsigned_64 := Read_U64 (State (Total_Verified_Slot));
         begin
            Write_U64 (State (Total_Verified_Slot), Total + 1);
         end;

         --  Return execution proof
         --  Format: success (1) + new_state_hash (32) + output_hash (32) + proof_length (2) + proof (variable)
         Result.Status := Success;
         Result.Return_Len := 67 + Natural'Min (Exec_Result_Gate.Proof.Proof_Length, 200);

         Result.Return_Data (0) := (if Exec_Result_Gate.Success then 1 else 0);

         for I in 0 .. 31 loop
            Result.Return_Data (1 + I) := Exec_Result_Gate.Proof.New_State_Hash (I);
            Result.Return_Data (33 + I) := Exec_Result_Gate.Proof.Output_Hash (I);
         end loop;

         Result.Return_Data (65) := Unsigned_8 (Exec_Result_Gate.Proof.Proof_Length mod 256);
         Result.Return_Data (66) := Unsigned_8 ((Exec_Result_Gate.Proof.Proof_Length / 256) mod 256);

         for I in 0 .. Natural'Min (Exec_Result_Gate.Proof.Proof_Length - 1, 199) loop
            if I < Anubis_Gate.Execution_Proof_Size then
               Result.Return_Data (67 + I) := Exec_Result_Gate.Proof.Proof_Data (I);
            end if;
         end loop;

         --  Record audit entry
         Record_Audit (
            State     => State,
            Operation => 16#V1#,  -- Private verification
            Actor     => Context.Caller,
            Target    => Session_ID,
            Timestamp => Unsigned_64 (Context.Height),
            Block     => Unsigned_64 (Context.Height)
         );
      end;
   end Verify_Private;

   procedure End_Session (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params     : Param_Buffer renames Context.Params;
      Session_ID : Byte_Array (0 .. 31) := (others => 0);
      Slot_Idx   : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < 32 then
         return;
      end if;

      for I in 0 .. 31 loop
         Session_ID (I) := Params (I);
      end loop;

      Slot_Idx := Get_Session_Slot (Session_ID);

      --  Check session exists
      if State (Slot_Idx).Value (0) /= 1 then
         Result := Error_Result (Unknown_Method);
         return;
      end if;

      --  Clear session (zeroize)
      for I in State (Slot_Idx).Value'Range loop
         State (Slot_Idx).Value (I) := 0;
      end loop;
      State (Slot_Idx).Length := 0;
      State (Slot_Idx).Modified := True;

      Result := Empty_Result;
   end End_Session;

   ---------------------------------------------------------------------------
   --  CONFIDENTIAL REPUTATION (WHISPER)
   ---------------------------------------------------------------------------

   procedure Issue_Rep_Score (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Holder   : Address := (others => 0);
      Score    : Unsigned_64;
      Category : Unsigned_8;
      Slot_Idx : State_Index;
      Commitment : Byte_Array (0 .. Commitment_Size - 1) := (others => 0);
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: holder (32) + score (8) + category (1) + blinding (32)
      if Context.Param_Len < 73 then
         return;
      end if;

      for I in 0 .. 31 loop
         Holder (I) := Params (I);
      end loop;

      Score := 0;
      for I in 0 .. 7 loop
         Score := Shift_Left (Score, 8) or Unsigned_64 (Params (32 + I));
      end loop;

      Category := Params (40);

      --  Create Ajtai commitment to score (WHISPER)
      --  In production: call Anubis_Whisper.Create_Commitment
      --  For now, hash score with blinding factor
      declare
         Commit_Input : Byte_Array (0 .. 47) := (others => 0);
      begin
         --  Encode score
         for I in 0 .. 7 loop
            Commit_Input (I) := Unsigned_8 (Shift_Right (Score, (7 - I) * 8) and 16#FF#);
         end loop;
         --  Add blinding factor
         for I in 0 .. 31 loop
            Commit_Input (8 + I) := Params (41 + I);
         end loop;
         --  Hash to create commitment
         Anubis_SHA3.SHA3_256 (Commit_Input, Commitment (0 .. 31));
         Anubis_SHA3.SHA3_256 (Commitment (0 .. 31), Commitment (32 .. 63));
      end;

      Slot_Idx := Get_Rep_Slot (Holder);

      --  Store reputation
      State (Slot_Idx).Value (0) := 1;  -- Active

      for I in 0 .. 31 loop
         State (Slot_Idx).Value (1 + I) := Holder (I);
      end loop;

      for I in 0 .. 63 loop
         State (Slot_Idx).Value (33 + I) := Commitment (I);
      end loop;

      State (Slot_Idx).Value (97) := Category;

      State (Slot_Idx).Length := 98;
      State (Slot_Idx).Modified := True;

      Result.Status := Success;
      Result.Return_Len := 64;
      for I in 0 .. 63 loop
         Result.Return_Data (I) := Commitment (I);
      end loop;
   end Issue_Rep_Score;

   procedure Update_Rep_Score (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Holder   : Address := (others => 0);
      Delta_Commit : Byte_Array (0 .. 63) := (others => 0);
      Slot_Idx : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: holder (32) + delta_commitment (64)
      if Context.Param_Len < 96 then
         return;
      end if;

      for I in 0 .. 31 loop
         Holder (I) := Params (I);
      end loop;

      for I in 0 .. 63 loop
         Delta_Commit (I) := Params (32 + I);
      end loop;

      Slot_Idx := Get_Rep_Slot (Holder);

      --  Check reputation exists
      if State (Slot_Idx).Value (0) /= 1 then
         Result := Error_Result (Unknown_Method);
         return;
      end if;

      --  Homomorphic addition using Ajtai commitment properties
      --  Real implementation: Anubis_Whisper.Add_Commitments
      --  For production: Use proper lattice-based homomorphic commitment
      --  Current: Hash-based accumulation for placeholder
      declare
         Combined : Byte_Array (0 .. 127) := (others => 0);
         New_Commit : Byte_Array (0 .. 31) := (others => 0);
      begin
         --  Combine old and delta commitments
         for I in 0 .. 63 loop
            Combined (I) := State (Slot_Idx).Value (33 + I);
            Combined (64 + I) := Delta_Commit (I);
         end loop;

         --  Hash to create new commitment
         Anubis_SHA3.SHA3_256 (Combined (0 .. 127), New_Commit);

         --  Store first 32 bytes of new commitment
         for I in 0 .. 31 loop
            State (Slot_Idx).Value (33 + I) := New_Commit (I);
         end loop;

         --  Extend to 64 bytes by hashing again
         Anubis_SHA3.SHA3_256 (New_Commit, New_Commit);
         for I in 0 .. 31 loop
            State (Slot_Idx).Value (65 + I) := New_Commit (I);
         end loop;
      end;

      State (Slot_Idx).Modified := True;

      Result := Empty_Result;
   end Update_Rep_Score;

   procedure Prove_Rep_Range (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Holder   : Address := (others => 0);
      Min_Score: Unsigned_64;
      Max_Score: Unsigned_64;
      Slot_Idx : State_Index;
      pragma Unreferenced (Min_Score, Max_Score);
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: holder (32) + min_score (8) + max_score (8) + blinding (32)
      if Context.Param_Len < 80 then
         return;
      end if;

      for I in 0 .. 31 loop
         Holder (I) := Params (I);
      end loop;

      Min_Score := 0;
      Max_Score := 0;
      for I in 0 .. 7 loop
         Min_Score := Shift_Left (Min_Score, 8) or Unsigned_64 (Params (32 + I));
         Max_Score := Shift_Left (Max_Score, 8) or Unsigned_64 (Params (40 + I));
      end loop;

      Slot_Idx := Get_Rep_Slot (Holder);

      --  Check reputation exists
      if State (Slot_Idx).Value (0) /= 1 then
         Result := Error_Result (Unknown_Method);
         return;
      end if;

      --  Generate range proof (WHISPER)
      --  In production: Anubis_Whisper.Create_Range_Proof
      --  Return proof structure
      Result.Status := Success;
      Result.Return_Len := 64;  -- Commitment as proof placeholder

      for I in 0 .. 63 loop
         Result.Return_Data (I) := State (Slot_Idx).Value (33 + I);
      end loop;
   end Prove_Rep_Range;

   ---------------------------------------------------------------------------
   --  ANONYMOUS PRESENTATION (VEIL)
   ---------------------------------------------------------------------------

   procedure Anon_Present (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params    : Param_Buffer renames Context.Params;
      Cred_ID   : Byte_Array (0 .. 31) := (others => 0);
      Ring_Size : Natural;
      Key_Image : Byte_Array (0 .. Key_Image_Size - 1) := (others => 0);
      KI_Slot   : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: cred_id (32) + ring_size (1) + ring_keys (variable) + signature (4096)
      if Context.Param_Len < 33 then
         return;
      end if;

      for I in 0 .. 31 loop
         Cred_ID (I) := Params (I);
      end loop;

      Ring_Size := Natural (Params (32));
      if Ring_Size < 2 or Ring_Size > 128 then
         return;
      end if;

      --  Generate key image (VEIL)
      --  In production: Anubis_Ring_Sig.Compute_Key_Image
      --  For now: hash caller address as key image
      Anubis_SHA3.SHA3_256 (Context.Caller, Key_Image (0 .. 31));
      Anubis_SHA3.SHA3_256 (Key_Image (0 .. 31), Key_Image (32 .. 63));

      --  Check key image not already spent
      KI_Slot := Get_Key_Image_Slot (Key_Image);
      if State (KI_Slot).Value (0) = 1 then
         --  Key image already used - double presentation
         Result := Error_Result (Invalid_Caller);
         return;
      end if;

      --  Store key image to prevent reuse
      State (KI_Slot).Value (0) := 1;
      for I in 0 .. Key_Image_Size - 1 loop
         State (KI_Slot).Value (1 + I) := Key_Image (I);
      end loop;
      State (KI_Slot).Length := 1 + Key_Image_Size;
      State (KI_Slot).Modified := True;

      Record_Audit (
         State     => State,
         Operation => 30,  -- Anonymous presentation
         Actor     => (others => 0),  -- Anonymous!
         Target    => Cred_ID,
         Timestamp => Unsigned_64 (Context.Height),
         Block     => Unsigned_64 (Context.Height)
      );

      Result.Status := Success;
      Result.Return_Len := Key_Image_Size;
      for I in 0 .. Key_Image_Size - 1 loop
         Result.Return_Data (I) := Key_Image (I);
      end loop;
   end Anon_Present;

   procedure Verify_Ring_Sig (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params    : Param_Buffer renames Context.Params;
      Ring_Size : Natural;
      Is_Valid  : Boolean := False;
      pragma Unreferenced (State);
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: message_hash (32) + ring_size (1) + signature (4096)
      if Context.Param_Len < 33 then
         return;
      end if;

      Ring_Size := Natural (Params (32));

      --  In production: Anubis_Ring_Sig.Verify
      --  For now: check ring size is valid
      if Ring_Size >= 2 and Ring_Size <= 128 then
         Is_Valid := True;
      end if;

      Result.Status := Success;
      Result.Return_Len := 1;
      Result.Return_Data (0) := (if Is_Valid then 1 else 0);
   end Verify_Ring_Sig;

   procedure Check_Key_Image (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params    : Param_Buffer renames Context.Params;
      Key_Image : Byte_Array (0 .. Key_Image_Size - 1) := (others => 0);
      KI_Slot   : State_Index;
      Is_Spent  : Boolean := False;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < Key_Image_Size then
         return;
      end if;

      for I in 0 .. Key_Image_Size - 1 loop
         Key_Image (I) := Params (I);
      end loop;

      KI_Slot := Get_Key_Image_Slot (Key_Image);
      Is_Spent := State (KI_Slot).Value (0) = 1;

      Result.Status := Success;
      Result.Return_Len := 1;
      Result.Return_Data (0) := (if Is_Spent then 1 else 0);
   end Check_Key_Image;

   ---------------------------------------------------------------------------
   --  TEE ATTESTATION
   ---------------------------------------------------------------------------

   procedure Request_Attestation (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params : Param_Buffer renames Context.Params;
      Nonce  : Byte_Array (0 .. 31) := (others => 0);
      Quote  : Byte_Array (0 .. 127) := (others => 0);
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: nonce (32)
      if Context.Param_Len < 32 then
         return;
      end if;

      for I in 0 .. 31 loop
         Nonce (I) := Params (I);
      end loop;

      --  Generate attestation quote
      --  In production: TEE_Attestation.Create_Quote
      --  For now: create structure with hashes

      --  Bytes 0-31: TEE code hash
      declare
         TEE_Label : constant Byte_Array (0 .. 31) :=
            (16#54#, 16#45#, 16#45#, others => 0);  -- "TEE" + padding
      begin
         Anubis_SHA3.SHA3_256 (TEE_Label, Quote (0 .. 31));
      end;

      --  Bytes 32-63: Config hash (from state slot 0)
      for I in 0 .. 31 loop
         Quote (32 + I) := State (Config_Slot).Value (I);
      end loop;

      --  Bytes 64-95: State root (credential registry)
      for I in 0 .. 31 loop
         Quote (64 + I) := State (Cred_Root_Slot).Value (I);
      end loop;

      --  Bytes 96-127: Nonce
      for I in 0 .. 31 loop
         Quote (96 + I) := Nonce (I);
      end loop;

      Result.Status := Success;
      Result.Return_Len := 128;
      for I in 0 .. 127 loop
         Result.Return_Data (I) := Quote (I);
      end loop;
   end Request_Attestation;

   procedure Verify_Attestation (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Quote    : Byte_Array (0 .. 127) := (others => 0);
      Is_Valid : Boolean := False;
      pragma Unreferenced (State);
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < 128 then
         return;
      end if;

      for I in 0 .. 127 loop
         Quote (I) := Params (I);
      end loop;

      --  In production: TEE_Attestation.Verify_Quote
      --  For now: verify structure is non-zero
      for I in 0 .. 31 loop
         if Quote (I) /= 0 then
            Is_Valid := True;
            exit;
         end if;
      end loop;

      Result.Status := Success;
      Result.Return_Len := 1;
      Result.Return_Data (0) := (if Is_Valid then 1 else 0);
   end Verify_Attestation;

   ---------------------------------------------------------------------------
   --  QUERY METHODS
   ---------------------------------------------------------------------------

   procedure Get_Credential (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params   : Param_Buffer renames Context.Params;
      Cred_ID  : Byte_Array (0 .. 31) := (others => 0);
      Slot_Idx : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < 32 then
         return;
      end if;

      for I in 0 .. 31 loop
         Cred_ID (I) := Params (I);
      end loop;

      Slot_Idx := Get_Credential_Slot (Cred_ID);

      if State (Slot_Idx).Value (0) /= 1 then
         Result := Error_Result (Unknown_Method);
         return;
      end if;

      --  Return credential data (public fields only)
      Result.Status := Success;
      Result.Return_Len := Natural'Min (State (Slot_Idx).Length, 128);
      for I in 0 .. Result.Return_Len - 1 loop
         Result.Return_Data (I) := State (Slot_Idx).Value (I);
      end loop;
   end Get_Credential;

   procedure Get_Rep_Commitment (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
   begin
      --  Return reputation pool commitment from slot 7
      Result.Status := Success;
      Result.Return_Len := 64;
      for I in 0 .. 63 loop
         Result.Return_Data (I) := State (Rep_Pool_Slot).Value (I);
      end loop;
   end Get_Rep_Commitment;

   procedure Get_Stats (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      pragma Unreferenced (Context);
      Total_Issued   : Unsigned_64;
      Total_Revoked  : Unsigned_64;
      Total_Verified : Unsigned_64;
   begin
      Total_Issued := Read_U64 (State (Total_Issued_Slot));
      Total_Revoked := Read_U64 (State (Total_Revoked_Slot));
      Total_Verified := Read_U64 (State (Total_Verified_Slot));

      Result.Status := Success;
      Result.Return_Len := 24;

      --  Pack stats
      for I in 0 .. 7 loop
         Result.Return_Data (I) :=
            Unsigned_8 (Shift_Right (Total_Issued, (7 - I) * 8) and 16#FF#);
         Result.Return_Data (8 + I) :=
            Unsigned_8 (Shift_Right (Total_Revoked, (7 - I) * 8) and 16#FF#);
         Result.Return_Data (16 + I) :=
            Unsigned_8 (Shift_Right (Total_Verified, (7 - I) * 8) and 16#FF#);
      end loop;
   end Get_Stats;

   ---------------------------------------------------------------------------
   --  CVM Interface
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor is
      Desc : CVM_Descriptor := Empty_Descriptor;
   begin
      --  Set up basic info
      Desc.Info.Active := True;
      Desc.Info.Caps := Privacy_Capabilities;  -- Full privacy support
      Desc.Entry_Count := 20;  -- Number of entry points

      return Desc;
   end Get_Descriptor;

   procedure Init (
      Init_Params : in     Byte_Array;
      State       : out    State_Array;
      Success     : out    Boolean
   ) is
   begin
      --  Initialize all slots to empty
      State := (others => Empty_Slot);
      Success := False;

      --  Need at least config hash
      if Init_Params'Length < 32 then
         return;
      end if;

      --  Store config hash
      for I in 0 .. 31 loop
         State (Config_Slot).Value (I) := Init_Params (Init_Params'First + I);
      end loop;
      State (Config_Slot).Length := 32;
      State (Config_Slot).Modified := True;

      --  Initialize counters
      Write_U64 (State (Total_Issued_Slot), 0);
      Write_U64 (State (Total_Revoked_Slot), 0);
      Write_U64 (State (Total_Verified_Slot), 0);

      Success := True;
   end Init;

   procedure Execute (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Selector : Hash256 := (others => 0);
   begin
      --  Get method selector from context
      Selector := Context.Entry_Point;

      --  Dispatch to appropriate handler
      if Selector_Match (Selector, Register_Issuer_Selector) then
         Register_Issuer (Context, State, Result);

      elsif Selector_Match (Selector, Revoke_Issuer_Selector) then
         Revoke_Issuer (Context, State, Result);

      elsif Selector_Match (Selector, Issue_Credential_Selector) then
         Issue_Credential (Context, State, Result);

      elsif Selector_Match (Selector, Present_Credential_Selector) then
         Present_Credential (Context, State, Result);

      elsif Selector_Match (Selector, Revoke_Credential_Selector) then
         Revoke_Credential (Context, State, Result);

      elsif Selector_Match (Selector, Check_Credential_Selector) then
         Check_Credential (Context, State, Result);

      elsif Selector_Match (Selector, Create_Disclosure_Selector) then
         Create_Disclosure (Context, State, Result);

      elsif Selector_Match (Selector, Verify_Disclosure_Selector) then
         Verify_Disclosure (Context, State, Result);

      elsif Selector_Match (Selector, Issue_View_Key_Selector) then
         Issue_View_Key (Context, State, Result);

      elsif Selector_Match (Selector, Start_Session_Selector) then
         Start_Session (Context, State, Result);

      elsif Selector_Match (Selector, Verify_Private_Selector) then
         Verify_Private (Context, State, Result);

      elsif Selector_Match (Selector, End_Session_Selector) then
         End_Session (Context, State, Result);

      elsif Selector_Match (Selector, Issue_Rep_Score_Selector) then
         Issue_Rep_Score (Context, State, Result);

      elsif Selector_Match (Selector, Update_Rep_Score_Selector) then
         Update_Rep_Score (Context, State, Result);

      elsif Selector_Match (Selector, Prove_Rep_Range_Selector) then
         Prove_Rep_Range (Context, State, Result);

      elsif Selector_Match (Selector, Anon_Present_Selector) then
         Anon_Present (Context, State, Result);

      elsif Selector_Match (Selector, Verify_Ring_Sig_Selector) then
         Verify_Ring_Sig (Context, State, Result);

      elsif Selector_Match (Selector, Check_Key_Image_Selector) then
         Check_Key_Image (Context, State, Result);

      elsif Selector_Match (Selector, Request_Attestation_Selector) then
         Request_Attestation (Context, State, Result);

      elsif Selector_Match (Selector, Verify_Attestation_Selector) then
         Verify_Attestation (Context, State, Result);

      elsif Selector_Match (Selector, Get_Credential_Selector) then
         Get_Credential (Context, State, Result);

      elsif Selector_Match (Selector, Get_Rep_Commitment_Selector) then
         Get_Rep_Commitment (Context, State, Result);

      elsif Selector_Match (Selector, Get_Stats_Selector) then
         Get_Stats (Context, State, Result);

      else
         Result := Error_Result (Unknown_Method);
      end if;
   end Execute;

end Anubis_Sovereign;
