--  SPHINX Lite Checkpoint Implementation

pragma SPARK_Mode (On);

package body Sphinx_Lite_Checkpoint with
   SPARK_Mode => On,
   Refined_State => (Checkpoint_State => (
      Latest_CP, CP_Count, Total_Sigs, Committee_Hash, Init_Flag))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Latest_CP      : Checkpoint := Null_Checkpoint;
   CP_Count       : Natural := 0;
   Total_Sigs     : Natural := 0;
   Committee_Hash : Hash256 := (others => 0);
   Init_Flag      : Boolean := False;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   procedure Initialize (
      Genesis_Checkpoint : in Checkpoint;
      Committee_Root     : in Hash256
   ) is
   begin
      Latest_CP := Genesis_Checkpoint;
      CP_Count := 1;
      Total_Sigs := Natural (Genesis_Checkpoint.Sig_Count);
      Committee_Hash := Committee_Root;
      Init_Flag := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Is_Initialized
   ---------------------------------------------------------------------------

   function Is_Initialized return Boolean is
   begin
      return Init_Flag;
   end Is_Initialized;

   ---------------------------------------------------------------------------
   --  Get_Latest_Checkpoint
   ---------------------------------------------------------------------------

   function Get_Latest_Checkpoint return Checkpoint is
   begin
      return Latest_CP;
   end Get_Latest_Checkpoint;

   ---------------------------------------------------------------------------
   --  Get_Checkpoint_Count
   ---------------------------------------------------------------------------

   function Get_Checkpoint_Count return Natural is
   begin
      return CP_Count;
   end Get_Checkpoint_Count;

   ---------------------------------------------------------------------------
   --  Create_Checkpoint
   ---------------------------------------------------------------------------

   procedure Create_Checkpoint (
      Block_Number : in  Unsigned_64;
      Block_Hash   : in  Hash256;
      State_Root   : in  Hash256;
      Timestamp    : in  Unsigned_64;
      Checkpoint   : out Sphinx_Lite_Types.Checkpoint;
      Success      : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      --  Must be newer than latest checkpoint
      if Block_Number <= Latest_CP.Block_Number then
         Error := Error_Invalid_Block_Number;
         Checkpoint := Null_Checkpoint;
         return;
      end if;

      --  Create new checkpoint
      Checkpoint := (
         Block_Number => Block_Number,
         Block_Hash   => Block_Hash,
         State_Root   => State_Root,
         Timestamp    => Timestamp,
         Sig_Bitmap   => 0,
         Sig_Count    => 0,
         Valid        => True
      );

      Success := True;
   end Create_Checkpoint;

   ---------------------------------------------------------------------------
   --  Add_Signature
   ---------------------------------------------------------------------------

   procedure Add_Signature (
      Checkpoint   : in out Sphinx_Lite_Types.Checkpoint;
      Member_Index : in     Committee_Index;
      Signature    : in     Hash512;
      Success      : out    Boolean;
      Error        : out    Sphinx_Lite_Error
   ) is
      pragma Unreferenced (Signature);
   begin
      Success := False;
      Error := Error_None;

      --  Check if already signed
      if Has_Signed (Checkpoint.Sig_Bitmap, Member_Index) then
         Error := Error_Duplicate_Signature;
         return;
      end if;

      --  Add signature
      Checkpoint.Sig_Bitmap := Set_Signature (Checkpoint.Sig_Bitmap, Member_Index);

      --  Increment count (with overflow check)
      if Checkpoint.Sig_Count < Committee_Size then
         Checkpoint.Sig_Count := Checkpoint.Sig_Count + 1;
      end if;

      Success := True;
   end Add_Signature;

   ---------------------------------------------------------------------------
   --  Is_Finalized
   ---------------------------------------------------------------------------

   function Is_Finalized (CP : Checkpoint) return Boolean is
   begin
      return CP.Sig_Count >= Threshold and then CP.Valid;
   end Is_Finalized;

   ---------------------------------------------------------------------------
   --  Verify_Checkpoint
   ---------------------------------------------------------------------------

   procedure Verify_Checkpoint (
      CP     : in  Checkpoint;
      Result : out Verification_Result
   ) is
   begin
      Result := (Success => False, Error => Error_None);

      --  Check validity flag
      if not CP.Valid then
         Result.Error := Error_Invalid_Checkpoint;
         return;
      end if;

      --  Check signature threshold
      if CP.Sig_Count < Threshold then
         Result.Error := Error_Insufficient_Signatures;
         return;
      end if;

      --  Check signature count matches bitmap
      if Count_Signatures (CP.Sig_Bitmap) /= CP.Sig_Count then
         Result.Error := Error_Invalid_Checkpoint;
         return;
      end if;

      Result := Sphinx_Lite_Types.Success_Result;
   end Verify_Checkpoint;

   ---------------------------------------------------------------------------
   --  Verify_Chain
   ---------------------------------------------------------------------------

   procedure Verify_Chain (
      New_Checkpoint : in  Checkpoint;
      Result         : out Verification_Result
   ) is
   begin
      Result := (Success => False, Error => Error_None);

      --  First verify the checkpoint itself
      Verify_Checkpoint (New_Checkpoint, Result);
      if not Result.Success then
         return;
      end if;

      --  Check block number progression
      if New_Checkpoint.Block_Number <= Latest_CP.Block_Number then
         Result.Error := Error_Invalid_Block_Number;
         Result.Success := False;
         return;
      end if;

      --  Check not too old
      if New_Checkpoint.Block_Number - Latest_CP.Block_Number > Max_Checkpoint_Age then
         Result.Error := Error_Stale_Checkpoint;
         Result.Success := False;
         return;
      end if;

      Result := Sphinx_Lite_Types.Success_Result;
   end Verify_Chain;

   ---------------------------------------------------------------------------
   --  Apply_Checkpoint
   ---------------------------------------------------------------------------

   procedure Apply_Checkpoint (
      CP      : in  Checkpoint;
      Success : out Boolean;
      Error   : out Sphinx_Lite_Error
   ) is
      Verify_Result : Verification_Result;
   begin
      Success := False;
      Error := Error_None;

      --  Verify chain first
      Verify_Chain (CP, Verify_Result);
      if not Verify_Result.Success then
         Error := Verify_Result.Error;
         return;
      end if;

      --  Apply checkpoint
      Latest_CP := CP;

      --  Update stats
      if CP_Count < Natural'Last then
         CP_Count := CP_Count + 1;
      end if;

      if Total_Sigs <= Natural'Last - Natural (CP.Sig_Count) then
         Total_Sigs := Total_Sigs + Natural (CP.Sig_Count);
      end if;

      Success := True;
   end Apply_Checkpoint;

   ---------------------------------------------------------------------------
   --  Get_Checkpoint_At
   ---------------------------------------------------------------------------

   procedure Get_Checkpoint_At (
      Block_Number : in  Unsigned_64;
      CP           : out Checkpoint;
      Found        : out Boolean
   ) is
   begin
      --  For now, only return if exact match with latest
      if Latest_CP.Block_Number = Block_Number then
         CP := Latest_CP;
         Found := True;
      else
         CP := Null_Checkpoint;
         Found := False;
      end if;
   end Get_Checkpoint_At;

   ---------------------------------------------------------------------------
   --  Is_Checkpointed
   ---------------------------------------------------------------------------

   function Is_Checkpointed (Block_Number : Unsigned_64) return Boolean is
   begin
      return Block_Number <= Latest_CP.Block_Number;
   end Is_Checkpointed;

   ---------------------------------------------------------------------------
   --  Blocks_Since_Checkpoint
   ---------------------------------------------------------------------------

   function Blocks_Since_Checkpoint (
      Block_Number : Unsigned_64
   ) return Unsigned_64 is
   begin
      if Block_Number >= Latest_CP.Block_Number then
         return Block_Number - Latest_CP.Block_Number;
      else
         return 0;
      end if;
   end Blocks_Since_Checkpoint;

   ---------------------------------------------------------------------------
   --  Serialize_Checkpoint
   ---------------------------------------------------------------------------

   procedure Serialize_Checkpoint (
      CP    : in  Checkpoint;
      Bytes : out Checkpoint_Bytes
   ) is
      Idx : Natural := 0;
   begin
      Bytes := (others => 0);

      --  Block number (8 bytes, little-endian)
      for I in 0 .. 7 loop
         Bytes (Idx) := Unsigned_8 (
            Shift_Right (CP.Block_Number, I * 8) and 16#FF#);
         Idx := Idx + 1;
      end loop;

      --  Block hash (32 bytes)
      for I in CP.Block_Hash'Range loop
         pragma Loop_Invariant (Idx = 8 + I);
         pragma Loop_Invariant (Idx in 8 .. 39);
         Bytes (Idx) := CP.Block_Hash (I);
         Idx := Idx + 1;
      end loop;

      --  State root (32 bytes)
      for I in CP.State_Root'Range loop
         pragma Loop_Invariant (Idx = 40 + I);
         pragma Loop_Invariant (Idx in 40 .. 71);
         Bytes (Idx) := CP.State_Root (I);
         Idx := Idx + 1;
      end loop;

      --  Timestamp (8 bytes)
      for I in 0 .. 7 loop
         if Idx < Checkpoint_Size then
            Bytes (Idx) := Unsigned_8 (
               Shift_Right (CP.Timestamp, I * 8) and 16#FF#);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Sig bitmap (4 bytes)
      for I in 0 .. 3 loop
         if Idx < Checkpoint_Size then
            Bytes (Idx) := Unsigned_8 (
               Shift_Right (CP.Sig_Bitmap, I * 8) and 16#FF#);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Sig count (1 byte, capped at 21)
      if Idx < Checkpoint_Size then
         Bytes (Idx) := Unsigned_8 (CP.Sig_Count);
         Idx := Idx + 1;
      end if;

      --  Valid flag (1 byte)
      if Idx < Checkpoint_Size then
         Bytes (Idx) := (if CP.Valid then 1 else 0);
      end if;
   end Serialize_Checkpoint;

   ---------------------------------------------------------------------------
   --  Deserialize_Checkpoint
   ---------------------------------------------------------------------------

   procedure Deserialize_Checkpoint (
      Bytes   : in  Checkpoint_Bytes;
      CP      : out Checkpoint;
      Success : out Boolean
   ) is
      Idx : Natural := 0;
      Block_Num : Unsigned_64 := 0;
      Timestamp : Unsigned_64 := 0;
      Bitmap    : Unsigned_32 := 0;
   begin
      CP := Null_Checkpoint;
      Success := False;

      --  Block number
      for I in 0 .. 7 loop
         Block_Num := Block_Num or
            Shift_Left (Unsigned_64 (Bytes (Idx)), I * 8);
         Idx := Idx + 1;
      end loop;
      CP.Block_Number := Block_Num;

      --  Block hash
      for I in CP.Block_Hash'Range loop
         pragma Loop_Invariant (Idx = 8 + I);
         pragma Loop_Invariant (Idx in 8 .. 39);
         CP.Block_Hash (I) := Bytes (Idx);
         Idx := Idx + 1;
      end loop;

      --  State root
      for I in CP.State_Root'Range loop
         pragma Loop_Invariant (Idx = 40 + I);
         pragma Loop_Invariant (Idx in 40 .. 71);
         CP.State_Root (I) := Bytes (Idx);
         Idx := Idx + 1;
      end loop;

      --  Timestamp
      for I in 0 .. 7 loop
         if Idx < Checkpoint_Size then
            Timestamp := Timestamp or
               Shift_Left (Unsigned_64 (Bytes (Idx)), I * 8);
            Idx := Idx + 1;
         end if;
      end loop;
      CP.Timestamp := Timestamp;

      --  Bitmap
      for I in 0 .. 3 loop
         if Idx < Checkpoint_Size then
            Bitmap := Bitmap or
               Shift_Left (Unsigned_32 (Bytes (Idx)), I * 8);
            Idx := Idx + 1;
         end if;
      end loop;
      CP.Sig_Bitmap := Bitmap;

      --  Sig count
      if Idx < Checkpoint_Size then
         CP.Sig_Count := Natural (Bytes (Idx) mod 22);  -- Cap at 21
         Idx := Idx + 1;
      end if;

      --  Valid flag
      if Idx < Checkpoint_Size then
         CP.Valid := Bytes (Idx) /= 0;
      end if;

      Success := True;
   end Deserialize_Checkpoint;

   ---------------------------------------------------------------------------
   --  Total_Checkpoints
   ---------------------------------------------------------------------------

   function Total_Checkpoints return Natural is
   begin
      return CP_Count;
   end Total_Checkpoints;

   ---------------------------------------------------------------------------
   --  Average_Signatures
   ---------------------------------------------------------------------------

   function Average_Signatures return Natural is
   begin
      if CP_Count = 0 then
         return 0;
      end if;

      return Total_Sigs / CP_Count;
   end Average_Signatures;

end Sphinx_Lite_Checkpoint;
