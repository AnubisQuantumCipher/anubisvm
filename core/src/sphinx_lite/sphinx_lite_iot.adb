--  SPHINX Lite IoT Implementation

pragma SPARK_Mode (On);

package body Sphinx_Lite_IoT with
   SPARK_Mode => On,
   Refined_State => (IoT_State => (
      Current_State, Sync_Count, Bytes_Total, CP_Updates))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Current_State  : IoT_Trusted_State := Null_IoT_State;
   Sync_Count     : Natural := 0;
   Bytes_Total    : Unsigned_64 := 0;
   CP_Updates     : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Compute checkpoint hash (placeholder for Keccak)
   function Compute_Checkpoint_Hash (CP : Checkpoint) return Hash256 is
      Result : Hash256 := (others => 0);
   begin
      --  XOR block hash and state root
      for I in Hash256'Range loop
         Result (I) := CP.Block_Hash (I) xor CP.State_Root (I);
      end loop;

      --  Mix in block number
      for I in 0 .. 7 loop
         Result (I) := Result (I) xor
            Unsigned_8 (Shift_Right (CP.Block_Number, I * 8) and 16#FF#);
      end loop;

      return Result;
   end Compute_Checkpoint_Hash;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   procedure Initialize (
      Genesis_Checkpoint : in Checkpoint;
      Chain_ID           : in Unsigned_32;
      Committee_Root     : in Hash256
   ) is
   begin
      Current_State := (
         Checkpoint_Hash  => Compute_Checkpoint_Hash (Genesis_Checkpoint),
         Block_Number     => Genesis_Checkpoint.Block_Number,
         Chain_ID         => Chain_ID,
         Committee_Epoch  => 0,
         Committee_Root   => Committee_Root,
         Initialized      => True
      );

      Sync_Count := 0;
      Bytes_Total := 0;
      CP_Updates := 1;  -- Count genesis as first update
   end Initialize;

   ---------------------------------------------------------------------------
   --  Is_Initialized
   ---------------------------------------------------------------------------

   function Is_Initialized return Boolean is
   begin
      return Current_State.Initialized;
   end Is_Initialized;

   ---------------------------------------------------------------------------
   --  Reset
   ---------------------------------------------------------------------------

   procedure Reset is
   begin
      Current_State := Null_IoT_State;
      Sync_Count := 0;
      Bytes_Total := 0;
      CP_Updates := 0;
   end Reset;

   ---------------------------------------------------------------------------
   --  Get_Trusted_State
   ---------------------------------------------------------------------------

   function Get_Trusted_State return IoT_Trusted_State is
   begin
      return Current_State;
   end Get_Trusted_State;

   ---------------------------------------------------------------------------
   --  Get_Block_Number
   ---------------------------------------------------------------------------

   function Get_Block_Number return Unsigned_64 is
   begin
      return Current_State.Block_Number;
   end Get_Block_Number;

   ---------------------------------------------------------------------------
   --  Get_Checkpoint_Hash
   ---------------------------------------------------------------------------

   function Get_Checkpoint_Hash return Hash256 is
   begin
      return Current_State.Checkpoint_Hash;
   end Get_Checkpoint_Hash;

   ---------------------------------------------------------------------------
   --  Get_Chain_ID
   ---------------------------------------------------------------------------

   function Get_Chain_ID return Unsigned_32 is
   begin
      return Current_State.Chain_ID;
   end Get_Chain_ID;

   ---------------------------------------------------------------------------
   --  Create_Sync_Request
   ---------------------------------------------------------------------------

   procedure Create_Sync_Request (
      Request_Proof : in  Boolean;
      Proof_Address : in  Contract_Address;
      Request       : out Sync_Request
   ) is
   begin
      Request := (
         Client_Block   => Current_State.Block_Number,
         Client_Epoch   => Current_State.Committee_Epoch,
         Request_Proof  => Request_Proof,
         Proof_Address  => Proof_Address
      );
   end Create_Sync_Request;

   ---------------------------------------------------------------------------
   --  Process_Sync_Response
   ---------------------------------------------------------------------------

   procedure Process_Sync_Response (
      Response : in  Sync_Response;
      Success  : out Boolean;
      Error    : out Sphinx_Lite_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      --  Update sync count
      if Sync_Count < Natural'Last then
         Sync_Count := Sync_Count + 1;
      end if;

      case Response.Status is
         when Sync_OK | Sync_Checkpoint_Available =>
            --  Verify and apply new checkpoint
            if Response.New_Checkpoint.Valid then
               if Response.New_Checkpoint.Block_Number > Current_State.Block_Number then
                  --  Update state
                  Current_State.Block_Number := Response.New_Checkpoint.Block_Number;
                  Current_State.Checkpoint_Hash :=
                     Compute_Checkpoint_Hash (Response.New_Checkpoint);

                  if CP_Updates < Natural'Last then
                     CP_Updates := CP_Updates + 1;
                  end if;

                  Success := True;
               else
                  Error := Error_Stale_Checkpoint;
               end if;
            else
               Error := Error_Invalid_Checkpoint;
            end if;

         when Sync_Already_Current =>
            --  No update needed
            Success := True;

         when Sync_Committee_Rotation =>
            --  Committee needs update - handled separately
            Error := Error_Committee_Changed;

         when Sync_Error_Invalid =>
            Error := Error_Invalid_Checkpoint;

         when Sync_Error_Stale =>
            Error := Error_Stale_Checkpoint;

         when Sync_Error_No_Data =>
            Error := Error_Not_Initialized;
      end case;
   end Process_Sync_Response;

   ---------------------------------------------------------------------------
   --  Needs_Sync
   ---------------------------------------------------------------------------

   function Needs_Sync (Current_Block : Unsigned_64) return Boolean is
   begin
      --  Sync if we"re more than 100 blocks behind
      return Current_Block > Current_State.Block_Number + 100;
   end Needs_Sync;

   ---------------------------------------------------------------------------
   --  Verify_Checkpoint_Update
   ---------------------------------------------------------------------------

   procedure Verify_Checkpoint_Update (
      New_Checkpoint   : in  Checkpoint;
      Expected_Root    : in  Hash256;
      Valid            : out Boolean;
      Error            : out Sphinx_Lite_Error
   ) is
   begin
      Valid := False;
      Error := Error_None;

      --  Block must be newer
      if New_Checkpoint.Block_Number <= Current_State.Block_Number then
         Error := Error_Stale_Checkpoint;
         return;
      end if;

      --  Must have enough signatures
      if New_Checkpoint.Sig_Count < Threshold then
         Error := Error_Insufficient_Signatures;
         return;
      end if;

      --  State root must match expected (from committee verification)
      if New_Checkpoint.State_Root /= Expected_Root then
         Error := Error_State_Root_Mismatch;
         return;
      end if;

      Valid := True;
   end Verify_Checkpoint_Update;

   ---------------------------------------------------------------------------
   --  Apply_Checkpoint
   ---------------------------------------------------------------------------

   procedure Apply_Checkpoint (
      New_Checkpoint : in  Checkpoint;
      Success        : out Boolean;
      Error          : out Sphinx_Lite_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      --  Verify signatures
      if New_Checkpoint.Sig_Count < Threshold then
         Error := Error_Insufficient_Signatures;
         return;
      end if;

      --  Update state
      Current_State.Block_Number := New_Checkpoint.Block_Number;
      Current_State.Checkpoint_Hash := Compute_Checkpoint_Hash (New_Checkpoint);

      if CP_Updates < Natural'Last then
         CP_Updates := CP_Updates + 1;
      end if;

      Success := True;
   end Apply_Checkpoint;

   ---------------------------------------------------------------------------
   --  Needs_Committee_Update
   ---------------------------------------------------------------------------

   function Needs_Committee_Update (New_Epoch : Unsigned_32) return Boolean is
   begin
      return New_Epoch > Current_State.Committee_Epoch;
   end Needs_Committee_Update;

   ---------------------------------------------------------------------------
   --  Update_Committee_Root
   ---------------------------------------------------------------------------

   procedure Update_Committee_Root (
      New_Root    : in  Hash256;
      New_Epoch   : in  Unsigned_32;
      Success     : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      --  Epoch must be strictly greater
      if New_Epoch <= Current_State.Committee_Epoch then
         Error := Error_Invalid_Block_Number;  -- Reusing error type
         return;
      end if;

      --  Update committee state
      Current_State.Committee_Root := New_Root;
      Current_State.Committee_Epoch := New_Epoch;

      Success := True;
   end Update_Committee_Root;

   ---------------------------------------------------------------------------
   --  Get_Sync_Interval
   ---------------------------------------------------------------------------

   function Get_Sync_Interval (Mode : Power_Mode) return Unsigned_32 is
   begin
      case Mode is
         when Power_Normal   => return Default_Sync_Interval;          -- 15 min
         when Power_Low      => return Default_Sync_Interval * 2;      -- 30 min
         when Power_Critical => return Default_Sync_Interval * 4;      -- 60 min
         when Power_Sleep    => return Max_Checkpoint_Age_Seconds;     -- 1 hour
      end case;
   end Get_Sync_Interval;

   ---------------------------------------------------------------------------
   --  Should_Skip_Sync
   ---------------------------------------------------------------------------

   function Should_Skip_Sync (
      Mode           : Power_Mode;
      Seconds_Since  : Unsigned_32
   ) return Boolean is
      Interval : constant Unsigned_32 := Get_Sync_Interval (Mode);
   begin
      --  Skip if not enough time has passed
      return Seconds_Since < Interval;
   end Should_Skip_Sync;

   ---------------------------------------------------------------------------
   --  Serialize_State
   ---------------------------------------------------------------------------

   procedure Serialize_State (
      State : in  IoT_Trusted_State;
      Bytes : out Serialized_IoT_State
   ) is
      Idx : Natural := 0;
   begin
      Bytes := (others => 0);

      --  Checkpoint hash (32 bytes)
      for I in State.Checkpoint_Hash'Range loop
         exit when Idx >= Trusted_State_Size;
         Bytes (Idx) := State.Checkpoint_Hash (I);
         Idx := Idx + 1;
      end loop;

      --  Block number (8 bytes, little-endian)
      for I in 0 .. 7 loop
         exit when Idx >= Trusted_State_Size;
         Bytes (Idx) := Unsigned_8 (
            Shift_Right (State.Block_Number, I * 8) and 16#FF#);
         Idx := Idx + 1;
      end loop;

      --  Chain ID (4 bytes)
      for I in 0 .. 3 loop
         exit when Idx >= Trusted_State_Size;
         Bytes (Idx) := Unsigned_8 (
            Shift_Right (State.Chain_ID, I * 8) and 16#FF#);
         Idx := Idx + 1;
      end loop;

      --  Committee epoch (4 bytes)
      for I in 0 .. 3 loop
         exit when Idx >= Trusted_State_Size;
         Bytes (Idx) := Unsigned_8 (
            Shift_Right (State.Committee_Epoch, I * 8) and 16#FF#);
         Idx := Idx + 1;
      end loop;

      --  Committee root (32 bytes)
      for I in State.Committee_Root'Range loop
         exit when Idx >= Trusted_State_Size;
         Bytes (Idx) := State.Committee_Root (I);
         Idx := Idx + 1;
      end loop;

      --  Initialized flag (1 byte)
      if Idx < Trusted_State_Size then
         Bytes (Idx) := (if State.Initialized then 1 else 0);
      end if;
   end Serialize_State;

   ---------------------------------------------------------------------------
   --  Deserialize_State
   ---------------------------------------------------------------------------

   procedure Deserialize_State (
      Bytes   : in  Serialized_IoT_State;
      State   : out IoT_Trusted_State;
      Success : out Boolean
   ) is
      Idx : Natural := 0;
      Block_Num : Unsigned_64 := 0;
      Chain : Unsigned_32 := 0;
      Epoch : Unsigned_32 := 0;
   begin
      State := Null_IoT_State;
      Success := False;

      --  Checkpoint hash
      for I in State.Checkpoint_Hash'Range loop
         exit when Idx >= Trusted_State_Size;
         State.Checkpoint_Hash (I) := Bytes (Idx);
         Idx := Idx + 1;
      end loop;

      --  Block number
      for I in 0 .. 7 loop
         exit when Idx >= Trusted_State_Size;
         Block_Num := Block_Num or
            Shift_Left (Unsigned_64 (Bytes (Idx)), I * 8);
         Idx := Idx + 1;
      end loop;
      State.Block_Number := Block_Num;

      --  Chain ID
      for I in 0 .. 3 loop
         exit when Idx >= Trusted_State_Size;
         Chain := Chain or
            Shift_Left (Unsigned_32 (Bytes (Idx)), I * 8);
         Idx := Idx + 1;
      end loop;
      State.Chain_ID := Chain;

      --  Committee epoch
      for I in 0 .. 3 loop
         exit when Idx >= Trusted_State_Size;
         Epoch := Epoch or
            Shift_Left (Unsigned_32 (Bytes (Idx)), I * 8);
         Idx := Idx + 1;
      end loop;
      State.Committee_Epoch := Epoch;

      --  Committee root
      for I in State.Committee_Root'Range loop
         exit when Idx >= Trusted_State_Size;
         State.Committee_Root (I) := Bytes (Idx);
         Idx := Idx + 1;
      end loop;

      --  Initialized flag
      if Idx < Trusted_State_Size then
         State.Initialized := Bytes (Idx) /= 0;
      end if;

      Success := True;
   end Deserialize_State;

   ---------------------------------------------------------------------------
   --  Save_State
   ---------------------------------------------------------------------------

   procedure Save_State (
      Bytes : out Serialized_IoT_State
   ) is
   begin
      Serialize_State (Current_State, Bytes);
   end Save_State;

   ---------------------------------------------------------------------------
   --  Load_State
   ---------------------------------------------------------------------------

   procedure Load_State (
      Bytes   : in  Serialized_IoT_State;
      Success : out Boolean
   ) is
   begin
      --  Always initialize all state constituents first
      Sync_Count := 0;
      Bytes_Total := 0;
      CP_Updates := 0;
      Current_State := Null_IoT_State;

      --  Then try to deserialize
      Deserialize_State (Bytes, Current_State, Success);
   end Load_State;

   ---------------------------------------------------------------------------
   --  Total_Syncs
   ---------------------------------------------------------------------------

   function Total_Syncs return Natural is
   begin
      return Sync_Count;
   end Total_Syncs;

   ---------------------------------------------------------------------------
   --  Bytes_Received
   ---------------------------------------------------------------------------

   function Bytes_Received return Unsigned_64 is
   begin
      return Bytes_Total;
   end Bytes_Received;

   ---------------------------------------------------------------------------
   --  Checkpoint_Updates
   ---------------------------------------------------------------------------

   function Checkpoint_Updates return Natural is
   begin
      return CP_Updates;
   end Checkpoint_Updates;

end Sphinx_Lite_IoT;
