pragma SPARK_Mode (On);

with Anubis_SHA3;
with Anubis_KMAC;
with Anubis_KDF;
with Anubis_AEAD;

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
      pragma Unreferenced (Signature, Public_Key);
      --  Stub: Would call Anubis_MLDSA.Verify
      Hash : Byte_Array (0 .. 31) := (others => 0);
   begin
      --  Compute message hash for verification
      if Message'Length > 0 and Message'Length <= 4096 then
         --  Create local copy with known bounds to satisfy SHA3 precondition
         declare
            Local_Msg : constant Byte_Array (0 .. Message'Length - 1) := Message;
         begin
            Anubis_SHA3.SHA3_256 (Local_Msg, Hash);
         end;
         --  In production: call ML-DSA-87 verify
         return Hash (0) /= 0;  -- Stub: always succeeds if message non-empty
      end if;
      return False;
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
      Mask     : Unsigned_16;
      Proof    : Byte_Array (0 .. 511) := (others => 0);
      pragma Unreferenced (State);
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: cred_id (32) + disclosure_mask (2)
      if Context.Param_Len < 34 then
         return;
      end if;

      for I in 0 .. 31 loop
         Cred_ID (I) := Params (I);
      end loop;

      Mask := Unsigned_16 (Params (32)) or Shift_Left (Unsigned_16 (Params (33)), 8);

      --  Generate disclosure proof using EYE
      --  In production: call Anubis_Eye.Create_Disclosure_Proof
      --  For now, create a simple proof structure
      Proof (0 .. 31) := Cred_ID;
      Proof (32) := Unsigned_8 (Mask and 16#FF#);
      Proof (33) := Unsigned_8 (Shift_Right (Mask, 8) and 16#FF#);

      --  Hash the proof with SHA3
      declare
         Proof_Hash : Byte_Array (0 .. 31) := (others => 0);
      begin
         Anubis_SHA3.SHA3_256 (Proof (0 .. 33), Proof_Hash);
         for I in 0 .. 31 loop
            Proof (34 + I) := Proof_Hash (I);
         end loop;
      end;

      Result.Status := Success;
      Result.Return_Len := 66;
      for I in 0 .. 65 loop
         Result.Return_Data (I) := Proof (I);
      end loop;
   end Create_Disclosure;

   procedure Verify_Disclosure (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params    : Param_Buffer renames Context.Params;
      Proof     : Byte_Array (0 .. 65) := (others => 0);
      Cred_ID   : Byte_Array (0 .. 31) := (others => 0);
      Slot_Idx  : State_Index;
      Is_Valid  : Boolean := False;
   begin
      Result := Error_Result (Invalid_Params);

      if Context.Param_Len < 66 then
         return;
      end if;

      for I in 0 .. 65 loop
         Proof (I) := Params (I);
      end loop;

      --  Extract credential ID from proof
      for I in 0 .. 31 loop
         Cred_ID (I) := Proof (I);
      end loop;

      Slot_Idx := Get_Credential_Slot (Cred_ID);

      --  Check credential exists and not revoked
      if State (Slot_Idx).Value (0) = 1 and then
         State (Slot_Idx).Value (74) = 0 then

         --  Verify proof hash
         declare
            Expected_Hash : Byte_Array (0 .. 31) := (others => 0);
            Actual_Hash   : Byte_Array (0 .. 31) := (others => 0);
         begin
            Anubis_SHA3.SHA3_256 (Proof (0 .. 33), Expected_Hash);
            for I in 0 .. 31 loop
               Actual_Hash (I) := Proof (34 + I);
            end loop;
            Is_Valid := Constant_Time_Compare (Expected_Hash, Actual_Hash);
         end;
      end if;

      Result.Status := Success;
      Result.Return_Len := 1;
      Result.Return_Data (0) := (if Is_Valid then 1 else 0);
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
      Session_ID : Byte_Array (0 .. 31) := (others => 0);
      Slot_Idx   : State_Index;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: verifier_kem_pk (1568) + randomness (32)
      if Context.Param_Len < 64 then
         return;
      end if;

      --  Generate session ID from caller + randomness
      declare
         Input : Byte_Array (0 .. 63) := (others => 0);
      begin
         for I in 0 .. 31 loop
            Input (I) := Context.Caller (I);
            Input (32 + I) := Params (I);
         end loop;
         Anubis_SHA3.SHA3_256 (Input, Session_ID);
      end;

      Slot_Idx := Get_Session_Slot (Session_ID);

      --  Store session
      State (Slot_Idx).Value (0) := 1;  -- Active

      for I in 0 .. 31 loop
         State (Slot_Idx).Value (1 + I) := Session_ID (I);
         State (Slot_Idx).Value (33 + I) := Context.Caller (I);  -- Holder
      end loop;

      --  Store expiry time
      declare
         Expiry : constant Unsigned_64 :=
            Unsigned_64 (Context.Height) + Session_Timeout;
      begin
         for I in 0 .. 7 loop
            State (Slot_Idx).Value (65 + I) :=
               Unsigned_8 (Shift_Right (Expiry, (7 - I) * 8) and 16#FF#);
         end loop;
      end;

      State (Slot_Idx).Length := 73;
      State (Slot_Idx).Modified := True;

      Result.Status := Success;
      Result.Return_Len := 32;
      for I in 0 .. 31 loop
         Result.Return_Data (I) := Session_ID (I);
      end loop;
   end Start_Session;

   procedure Verify_Private (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  : out    Exec_Result
   ) is
      Params     : Param_Buffer renames Context.Params;
      Session_ID : Byte_Array (0 .. 31) := (others => 0);
      Cred_ID    : Byte_Array (0 .. 31) := (others => 0);
      Slot_Idx   : State_Index;
      Cred_Slot  : State_Index;
      Is_Valid   : Boolean := False;
   begin
      Result := Error_Result (Invalid_Params);

      --  Params: session_id (32) + cred_id (32) + encrypted_proof (variable)
      if Context.Param_Len < 64 then
         return;
      end if;

      for I in 0 .. 31 loop
         Session_ID (I) := Params (I);
         Cred_ID (I) := Params (32 + I);
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
                     Unsigned_64 (State (Slot_Idx).Value (65 + I));
         end loop;

         if Unsigned_64 (Context.Height) > Expiry then
            Result := Error_Result (Out_Of_Gas);
            return;
         end if;
      end;

      --  Verify credential exists and is valid
      Cred_Slot := Get_Credential_Slot (Cred_ID);
      if State (Cred_Slot).Value (0) = 1 and then
         State (Cred_Slot).Value (74) = 0 then
         Is_Valid := True;
      end if;

      --  Update verification count
      declare
         Total : Unsigned_64 := Read_U64 (State (Total_Verified_Slot));
      begin
         Write_U64 (State (Total_Verified_Slot), Total + 1);
      end;

      Result.Status := Success;
      Result.Return_Len := 1;
      Result.Return_Data (0) := (if Is_Valid then 1 else 0);
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

      --  Homomorphic addition (WHISPER)
      --  In production: Anubis_Whisper.Add_Commitments
      --  For now: XOR commitments (placeholder)
      for I in 0 .. 63 loop
         State (Slot_Idx).Value (33 + I) :=
            State (Slot_Idx).Value (33 + I) xor Delta_Commit (I);
      end loop;

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
