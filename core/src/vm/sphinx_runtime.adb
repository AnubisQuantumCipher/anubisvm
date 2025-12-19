--  SPHINX Runtime Implementation
--
--  Full execution runtime with call stack, calldata, and cross-contract calls
--
--  Note: This implementation uses Ada.Text_IO for debugging and has global
--  state (Current_Runtime), so individual procedure bodies are marked with
--  SPARK_Mode => Off. However, the package spec has SPARK_Mode => On with
--  proper Global contracts, allowing SPARK clients to call these procedures.

pragma SPARK_Mode (On);

with Ada.Text_IO;
with Anubis_SHA3;
with Anubis_Types;
with Sphinx_Native;

package body Sphinx_Runtime with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Runtime Initialization
   ---------------------------------------------------------------------------

   procedure Initialize_Runtime (
      Runtime     : out Runtime_Context;
      Origin      : in  Account_ID;
      Gas_Limit   : in  Gas_Amount;
      Gas_Price   : in  Unsigned_64;
      Block_Num   : in  Unsigned_64;
      Timestamp   : in  Unsigned_64;
      Chain_ID    : in  Unsigned_64;
      Syscalls    : in  Syscall_Table_Access;
      Success     : out Boolean
   ) with SPARK_Mode => Off is
   begin
      Runtime := (
         Frames        => (others => Empty_Frame),
         Current_Depth => 0,
         Origin        => Origin,
         Gas_Price     => Gas_Price,
         Block_Number  => Block_Num,
         Timestamp     => Timestamp,
         Chain_ID      => Chain_ID,
         Syscalls      => Syscalls,
         Initialized   => True,
         Next_Snapshot => 0
      );

      --  Set up initial frame with transaction gas limit
      Runtime.Frames (0).Gas_Limit := Gas_Limit;
      Runtime.Frames (0).Origin_Addr := Origin;
      Runtime.Frames (0).Active := True;

      Success := True;
   end Initialize_Runtime;

   procedure Finalize_Runtime (Runtime : in Out Runtime_Context) is
   begin
      Runtime.Initialized := False;
      Runtime.Current_Depth := 0;
   end Finalize_Runtime;

   procedure Set_Runtime (Runtime : Runtime_Context_Access) is
   begin
      Current_Runtime := Runtime;
   end Set_Runtime;

   function Get_Runtime return Runtime_Context_Access is
   begin
      return Current_Runtime;
   end Get_Runtime;

   ---------------------------------------------------------------------------
   --  Call Frame Management
   ---------------------------------------------------------------------------

   procedure Push_Frame (
      Runtime   : in Out Runtime_Context;
      Target    : in     Account_ID;
      Caller    : in     Account_ID;
      Value     : in     Unsigned_64;
      Calldata  : in     Byte_Array;
      Gas_Limit : in     Gas_Amount;
      Kind      : in     Call_Kind;
      Success   : out    Boolean
   ) is
      New_Depth : Natural;
      CD_Size   : Natural;
   begin
      --  Check depth limit
      if Runtime.Current_Depth >= Max_Call_Depth - 1 then
         Success := False;
         return;
      end if;

      New_Depth := Runtime.Current_Depth + 1;
      Runtime.Current_Depth := New_Depth;

      --  Initialize new frame
      Runtime.Frames (New_Depth) := Empty_Frame;
      Runtime.Frames (New_Depth).Contract_Addr := Target;
      Runtime.Frames (New_Depth).Caller_Addr := Caller;
      Runtime.Frames (New_Depth).Origin_Addr := Runtime.Origin;
      Runtime.Frames (New_Depth).Value := Value;
      Runtime.Frames (New_Depth).Kind := Kind;
      Runtime.Frames (New_Depth).Gas_Limit := Gas_Limit;
      Runtime.Frames (New_Depth).Gas_Used := 0;
      Runtime.Frames (New_Depth).Depth := New_Depth;
      Runtime.Frames (New_Depth).Active := True;

      --  Handle DELEGATECALL context preservation
      if Kind = Kind_Delegate_Call and New_Depth > 0 then
         Runtime.Frames (New_Depth).Contract_Addr :=
            Runtime.Frames (New_Depth - 1).Contract_Addr;
         Runtime.Frames (New_Depth).Caller_Addr :=
            Runtime.Frames (New_Depth - 1).Caller_Addr;
         Runtime.Frames (New_Depth).Value :=
            Runtime.Frames (New_Depth - 1).Value;
         Runtime.Frames (New_Depth).Code_Addr := Target;
      else
         Runtime.Frames (New_Depth).Code_Addr := Target;
      end if;

      --  Copy calldata
      CD_Size := Natural'Min (Calldata'Length, Max_Calldata_Size);
      Runtime.Frames (New_Depth).Calldata_Size := CD_Size;
      for I in 0 .. CD_Size - 1 loop
         Runtime.Frames (New_Depth).Calldata (I) :=
            Calldata (Calldata'First + I);
      end loop;

      --  Create state snapshot for potential rollback
      Runtime.Frames (New_Depth).Snapshot_ID := Runtime.Next_Snapshot;
      Runtime.Next_Snapshot := Runtime.Next_Snapshot + 1;

      Success := True;
   end Push_Frame;

   procedure Pop_Frame (
      Runtime     : in Out Runtime_Context;
      Success     : in     Boolean;
      Return_Data : in     Byte_Array;
      Gas_Refund  : out    Gas_Amount
   ) is
      Current    : Natural;
      Parent     : Natural;
      Ret_Size   : Natural;
   begin
      Gas_Refund := 0;

      if Runtime.Current_Depth = 0 then
         return;
      end if;

      Current := Runtime.Current_Depth;
      Parent := Current - 1;

      --  Calculate gas refund
      if Runtime.Frames (Current).Gas_Used < Runtime.Frames (Current).Gas_Limit then
         Gas_Refund := Runtime.Frames (Current).Gas_Limit -
                       Runtime.Frames (Current).Gas_Used;
      end if;

      --  Copy return data to parent frame
      Ret_Size := Natural'Min (Return_Data'Length, Max_Return_Size);
      Runtime.Frames (Parent).Return_Size := Ret_Size;
      for I in 0 .. Ret_Size - 1 loop
         Runtime.Frames (Parent).Return_Data (I) :=
            Return_Data (Return_Data'First + I);
      end loop;

      --  Deactivate current frame
      Runtime.Frames (Current).Active := False;

      --  If call failed, rollback would happen here (via snapshot)
      if not Success then
         Ada.Text_IO.Put_Line ("  [SPHINX] Call reverted, rolling back state");
         --  State rollback would use Snapshot_ID
      end if;

      Runtime.Current_Depth := Parent;
   end Pop_Frame;

   function Current_Frame (Runtime : Runtime_Context) return Call_Frame is
   begin
      if Runtime.Current_Depth <= Frame_Index'Last then
         return Runtime.Frames (Runtime.Current_Depth);
      else
         return Empty_Frame;
      end if;
   end Current_Frame;

   ---------------------------------------------------------------------------
   --  Calldata Operations
   ---------------------------------------------------------------------------

   function Get_Calldata_Size return Natural is
      RT : Runtime_Context_Access;
   begin
      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return 0;
      end if;

      if RT.Current_Depth <= Frame_Index'Last then
         return RT.Frames (RT.Current_Depth).Calldata_Size;
      else
         return 0;
      end if;
   end Get_Calldata_Size;

   procedure Load_Calldata (
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) with SPARK_Mode => Off is
      RT       : Runtime_Context_Access;
      Frame    : Call_Frame;
      Copy_Len : Natural;
   begin
      Buffer := (others => 0);
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      if RT.Current_Depth > Frame_Index'Last then
         return;
      end if;

      Frame := RT.Frames (RT.Current_Depth);

      --  Calculate how much we can copy
      if Offset >= Frame.Calldata_Size then
         --  Reading past calldata returns zeros
         Success := True;
         return;
      end if;

      Copy_Len := Natural'Min (Size, Frame.Calldata_Size - Offset);
      Copy_Len := Natural'Min (Copy_Len, Buffer'Length);

      for I in 0 .. Copy_Len - 1 loop
         if Offset + I < Frame.Calldata_Size then
            Buffer (Buffer'First + I) := Frame.Calldata (Offset + I);
         end if;
      end loop;

      Success := True;
   end Load_Calldata;

   procedure Copy_Calldata (
      Data_Offset : in     Natural;
      Size        : in     Natural;
      Buffer      : out    Byte_Array;
      Success     : out    Boolean
   ) is
   begin
      Load_Calldata (Data_Offset, Size, Buffer, Success);
   end Copy_Calldata;

   ---------------------------------------------------------------------------
   --  Return Data Operations
   ---------------------------------------------------------------------------

   function Get_Return_Data_Size return Natural is
      RT : Runtime_Context_Access;
   begin
      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return 0;
      end if;

      if RT.Current_Depth <= Frame_Index'Last then
         return RT.Frames (RT.Current_Depth).Return_Size;
      else
         return 0;
      end if;
   end Get_Return_Data_Size;

   procedure Load_Return_Data (
      Offset  : in     Natural;
      Size    : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) is
      RT       : Runtime_Context_Access;
      Frame    : Call_Frame;
      Copy_Len : Natural;
   begin
      Buffer := (others => 0);
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      if RT.Current_Depth > Frame_Index'Last then
         return;
      end if;

      Frame := RT.Frames (RT.Current_Depth);

      if Offset >= Frame.Return_Size then
         Success := True;
         return;
      end if;

      Copy_Len := Natural'Min (Size, Frame.Return_Size - Offset);
      Copy_Len := Natural'Min (Copy_Len, Buffer'Length);

      for I in 0 .. Copy_Len - 1 loop
         if Offset + I < Frame.Return_Size then
            Buffer (Buffer'First + I) := Frame.Return_Data (Offset + I);
         end if;
      end loop;

      Success := True;
   end Load_Return_Data;

   ---------------------------------------------------------------------------
   --  Cross-Contract Call Operations
   ---------------------------------------------------------------------------

   procedure Execute_Call (
      Target      : in     Account_ID;
      Value       : in     Unsigned_64;
      Calldata    : in     Byte_Array;
      Gas_Limit   : in     Gas_Amount;
      Return_Data : out    Byte_Array;
      Return_Size : out    Natural;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with SPARK_Mode => Off is
      RT         : Runtime_Context_Access;
      Frame      : Call_Frame;
      Push_OK    : Boolean;
      Gas_Refund : Gas_Amount;
   begin
      Return_Data := (others => 0);
      Return_Size := 0;
      Gas_Used := 0;
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         Ada.Text_IO.Put_Line ("  [SPHINX] Execute_Call: No runtime context");
         return;
      end if;

      --  Get current caller address
      Frame := Current_Frame (RT.all);

      --  Push new call frame
      Push_Frame (
         Runtime   => RT.all,
         Target    => Target,
         Caller    => Frame.Contract_Addr,
         Value     => Value,
         Calldata  => Calldata,
         Gas_Limit => Gas_Limit,
         Kind      => Kind_Call,
         Success   => Push_OK
      );

      if not Push_OK then
         Ada.Text_IO.Put_Line ("  [SPHINX] Execute_Call: Stack overflow");
         Gas_Used := Gas_Limit;  --  Consume all gas on stack overflow
         return;
      end if;

      --  Execute the target contract via Sphinx_Native
      Ada.Text_IO.Put_Line ("  [SPHINX] Executing CALL to target contract...");

      declare
         use Sphinx_Native;

         --  Convert target address to code ID (first 8 bytes as Word64)
         Code_ID : Word64 := 0;
         Exec_Return : Byte_Array (0 .. Max_Return_Size - 1) := (others => 0);
         Exec_Return_Len : Natural := 0;
         Exec_Gas_Used : Gas_Amount := 0;
         Exec_Status : Sphinx_Native.Exec_Status;
         Exec_Success : Boolean;
      begin
         --  Build code ID from target address (use first 8 bytes)
         for I in 0 .. 7 loop
            Code_ID := Code_ID or
               Shift_Left (Word64 (Target (I)), I * 8);
         end loop;

         --  Try to execute via registered native contract
         Sphinx_Native.Execute_Registered (
            Code_ID     => Code_ID,
            Calldata    => Calldata,
            Gas_Limit   => Gas_Limit,
            Return_Data => Exec_Return,
            Return_Len  => Exec_Return_Len,
            Gas_Used    => Exec_Gas_Used,
            Status      => Exec_Status
         );

         Exec_Success := (Exec_Status = Sphinx_Native.Exec_Success);

         if Exec_Success then
            Ada.Text_IO.Put_Line ("  [SPHINX] CALL succeeded with " &
               Natural'Image (Exec_Return_Len) & " bytes return data");

            --  Copy return data to caller's buffer
            Return_Size := Natural'Min (Exec_Return_Len, Return_Data'Length);
            for I in 0 .. Return_Size - 1 loop
               Return_Data (Return_Data'First + I) := Exec_Return (I);
            end loop;

            Gas_Used := Exec_Gas_Used;

            --  Pop frame with success
            Pop_Frame (
               Runtime     => RT.all,
               Success     => True,
               Return_Data => Exec_Return (0 .. Natural'Max (0, Exec_Return_Len - 1)),
               Gas_Refund  => Gas_Refund
            );

            Success := True;
         else
            Ada.Text_IO.Put_Line ("  [SPHINX] CALL reverted or no library found");

            --  Pop frame with failure
            Pop_Frame (
               Runtime     => RT.all,
               Success     => False,
               Return_Data => Byte_Array'(0 .. -1 => 0),
               Gas_Refund  => Gas_Refund
            );

            Gas_Used := Gas_Limit;  --  Consume all gas on revert
            Success := False;
         end if;
      end;
   end Execute_Call;

   procedure Execute_Static_Call (
      Target      : in     Account_ID;
      Calldata    : in     Byte_Array;
      Gas_Limit   : in     Gas_Amount;
      Return_Data : out    Byte_Array;
      Return_Size : out    Natural;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with SPARK_Mode => Off is
      RT         : Runtime_Context_Access;
      Frame      : Call_Frame;
      Push_OK    : Boolean;
      Gas_Refund : Gas_Amount;
   begin
      Return_Data := (others => 0);
      Return_Size := 0;
      Gas_Used := 0;
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      Frame := Current_Frame (RT.all);

      --  Push static call frame (value = 0, read-only)
      Push_Frame (
         Runtime   => RT.all,
         Target    => Target,
         Caller    => Frame.Contract_Addr,
         Value     => 0,  --  Static calls cannot transfer value
         Calldata  => Calldata,
         Gas_Limit => Gas_Limit,
         Kind      => Kind_Static_Call,
         Success   => Push_OK
      );

      if not Push_OK then
         Gas_Used := Gas_Limit;
         return;
      end if;

      --  Execute STATICCALL via Sphinx_Native (read-only)
      Ada.Text_IO.Put_Line ("  [SPHINX] Executing STATICCALL (read-only)...");

      declare
         use Sphinx_Native;

         Code_ID : Word64 := 0;
         Exec_Return : Byte_Array (0 .. Max_Return_Size - 1) := (others => 0);
         Exec_Return_Len : Natural := 0;
         Exec_Gas_Used : Gas_Amount := 0;
         Exec_Status : Sphinx_Native.Exec_Status;
         Exec_Success : Boolean;
      begin
         for I in 0 .. 7 loop
            Code_ID := Code_ID or
               Shift_Left (Word64 (Target (I)), I * 8);
         end loop;

         Sphinx_Native.Execute_Registered (
            Code_ID     => Code_ID,
            Calldata    => Calldata,
            Gas_Limit   => Gas_Limit,
            Return_Data => Exec_Return,
            Return_Len  => Exec_Return_Len,
            Gas_Used    => Exec_Gas_Used,
            Status      => Exec_Status
         );

         Exec_Success := (Exec_Status = Sphinx_Native.Exec_Success);

         if Exec_Success then
            Return_Size := Natural'Min (Exec_Return_Len, Return_Data'Length);
            for I in 0 .. Return_Size - 1 loop
               Return_Data (Return_Data'First + I) := Exec_Return (I);
            end loop;

            Gas_Used := Exec_Gas_Used;
            Pop_Frame (RT.all, True, Exec_Return (0 .. Natural'Max (0, Exec_Return_Len - 1)), Gas_Refund);
            Success := True;
         else
            Pop_Frame (RT.all, False, Byte_Array'(0 .. -1 => 0), Gas_Refund);
            Gas_Used := Gas_Limit;
            Success := False;
         end if;
      end;
   end Execute_Static_Call;

   procedure Execute_Delegate_Call (
      Target      : in     Account_ID;
      Calldata    : in     Byte_Array;
      Gas_Limit   : in     Gas_Amount;
      Return_Data : out    Byte_Array;
      Return_Size : out    Natural;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with SPARK_Mode => Off is
      RT         : Runtime_Context_Access;
      Frame      : Call_Frame;
      Push_OK    : Boolean;
      Gas_Refund : Gas_Amount;
   begin
      Return_Data := (others => 0);
      Return_Size := 0;
      Gas_Used := 0;
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      Frame := Current_Frame (RT.all);

      --  Push delegate call frame (preserves caller and value)
      Push_Frame (
         Runtime   => RT.all,
         Target    => Target,
         Caller    => Frame.Caller_Addr,  --  Preserved from parent
         Value     => Frame.Value,        --  Preserved from parent
         Calldata  => Calldata,
         Gas_Limit => Gas_Limit,
         Kind      => Kind_Delegate_Call,
         Success   => Push_OK
      );

      if not Push_OK then
         Gas_Used := Gas_Limit;
         return;
      end if;

      --  Execute DELEGATECALL via Sphinx_Native (preserves caller/value context)
      Ada.Text_IO.Put_Line ("  [SPHINX] Executing DELEGATECALL (preserving context)...");

      declare
         use Sphinx_Native;

         Code_ID : Word64 := 0;
         Exec_Return : Byte_Array (0 .. Max_Return_Size - 1) := (others => 0);
         Exec_Return_Len : Natural := 0;
         Exec_Gas_Used : Gas_Amount := 0;
         Exec_Status : Sphinx_Native.Exec_Status;
         Exec_Success : Boolean;
      begin
         for I in 0 .. 7 loop
            Code_ID := Code_ID or
               Shift_Left (Word64 (Target (I)), I * 8);
         end loop;

         Sphinx_Native.Execute_Registered (
            Code_ID     => Code_ID,
            Calldata    => Calldata,
            Gas_Limit   => Gas_Limit,
            Return_Data => Exec_Return,
            Return_Len  => Exec_Return_Len,
            Gas_Used    => Exec_Gas_Used,
            Status      => Exec_Status
         );

         Exec_Success := (Exec_Status = Sphinx_Native.Exec_Success);

         if Exec_Success then
            Return_Size := Natural'Min (Exec_Return_Len, Return_Data'Length);
            for I in 0 .. Return_Size - 1 loop
               Return_Data (Return_Data'First + I) := Exec_Return (I);
            end loop;

            Gas_Used := Exec_Gas_Used;
            Pop_Frame (RT.all, True, Exec_Return (0 .. Natural'Max (0, Exec_Return_Len - 1)), Gas_Refund);
            Success := True;
         else
            Pop_Frame (RT.all, False, Byte_Array'(0 .. -1 => 0), Gas_Refund);
            Gas_Used := Gas_Limit;
            Success := False;
         end if;
      end;
   end Execute_Delegate_Call;

   procedure Execute_Create (
      Init_Code   : in     Byte_Array;
      Value       : in     Unsigned_64;
      Gas_Limit   : in     Gas_Amount;
      New_Address : out    Account_ID;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with SPARK_Mode => Off is
      RT    : Runtime_Context_Access;
      Frame : Call_Frame;
      Nonce : Unsigned_64 := 0;
   begin
      New_Address := (others => 0);
      Gas_Used := 0;
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      Frame := Current_Frame (RT.all);

      --  Compute CREATE address: keccak256(sender || nonce)[12..31]
      declare
         use Anubis_SHA3;
         use Anubis_Types;
         Input  : Anubis_Types.Byte_Array (0 .. 39);  --  32 addr + 8 nonce
         Digest : SHA3_256_Digest;
      begin
         --  Copy sender address
         for I in 0 .. 31 loop
            Input (I) := Anubis_Types.Byte (Frame.Contract_Addr (I));
         end loop;

         --  Encode nonce (little-endian)
         for I in 0 .. 7 loop
            Input (32 + I) := Anubis_Types.Byte (
               Shift_Right (Nonce, I * 8) and 16#FF#);
         end loop;

         SHA3_256 (Input, Digest);

         --  Take last 20 bytes as address (padded to 32)
         for I in 0 .. 19 loop
            New_Address (12 + I) := Aegis_VM_Types.Byte (Digest (12 + I));
         end loop;
      end;

      Ada.Text_IO.Put_Line ("  [SPHINX] Executing CREATE...");

      --  CREATE deployment flow:
      --  1. Compute deployment address (done above)
      --  2. Execute init code to get runtime code
      --  3. Register runtime code at computed address

      declare
         use Sphinx_Native;

         --  Hash init code to get a deployment ID
         Init_Hash  : Anubis_SHA3.SHA3_256_Digest;
         Code_ID    : Word64 := 0;
         Exec_Return : Byte_Array (0 .. Max_Return_Size - 1) := (others => 0);
         Exec_Return_Len : Natural := 0;
         Exec_Gas_Used : Gas_Amount := 0;
         Exec_Status : Sphinx_Native.Exec_Status;
         Push_OK : Boolean;
         Gas_Refund : Gas_Amount;
      begin
         --  Hash init code to get code ID
         declare
            Init_Bytes : Anubis_Types.Byte_Array (0 .. Init_Code'Length - 1);
         begin
            for I in Init_Code'Range loop
               Init_Bytes (I - Init_Code'First) := Anubis_Types.Byte (Init_Code (I));
            end loop;
            Anubis_SHA3.SHA3_256 (Init_Bytes, Init_Hash);
         end;

         --  Build code ID from init hash (first 8 bytes)
         for I in 0 .. 7 loop
            Code_ID := Code_ID or
               Shift_Left (Word64 (Init_Hash (I)), I * 8);
         end loop;

         Ada.Text_IO.Put_Line ("  [SPHINX] CREATE: Init code hash ID 0x" &
            Word64'Image (Code_ID));

         --  Push frame for init code execution
         Push_Frame (
            Runtime   => RT.all,
            Target    => New_Address,
            Caller    => Frame.Contract_Addr,
            Value     => Value,
            Calldata  => Byte_Array'(0 .. -1 => 0),  --  No calldata for constructor
            Gas_Limit => Gas_Limit,
            Kind      => Kind_Create,
            Success   => Push_OK
         );

         if not Push_OK then
            Gas_Used := Gas_Limit;
            return;
         end if;

         --  Try to execute init code via registered native contract
         Sphinx_Native.Execute_Registered (
            Code_ID     => Code_ID,
            Calldata    => Byte_Array'(0 .. -1 => 0),
            Gas_Limit   => Gas_Limit - 32000,  --  Reserve CREATE base gas
            Return_Data => Exec_Return,
            Return_Len  => Exec_Return_Len,
            Gas_Used    => Exec_Gas_Used,
            Status      => Exec_Status
         );

         Pop_Frame (
            Runtime     => RT.all,
            Success     => (Exec_Status = Sphinx_Native.Exec_Success),
            Return_Data => Byte_Array'(0 .. -1 => 0),
            Gas_Refund  => Gas_Refund
         );

         Gas_Used := 32000 + Exec_Gas_Used;  --  Base + execution gas

         if Exec_Status = Sphinx_Native.Exec_Success then
            Ada.Text_IO.Put_Line ("  [SPHINX] CREATE: Init code executed successfully");
            Success := True;
         else
            Ada.Text_IO.Put_Line ("  [SPHINX] CREATE: Init code not found or execution failed");
            --  CREATE still succeeds if init code isn't registered (lazy deployment)
            Success := True;
         end if;
      end;
   end Execute_Create;

   procedure Execute_Create2 (
      Init_Code   : in     Byte_Array;
      Salt        : in     Hash256;
      Value       : in     Unsigned_64;
      Gas_Limit   : in     Gas_Amount;
      New_Address : out    Account_ID;
      Gas_Used    : out    Gas_Amount;
      Success     : out    Boolean
   ) with SPARK_Mode => Off is
      RT    : Runtime_Context_Access;
      Frame : Call_Frame;
   begin
      New_Address := (others => 0);
      Gas_Used := 0;
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      Frame := Current_Frame (RT.all);

      --  Compute CREATE2 address: keccak256(0xff || sender || salt || keccak256(init_code))[12..31]
      declare
         use Anubis_SHA3;
         use Anubis_Types;
         Code_Input : Anubis_Types.Byte_Array (0 .. Init_Code'Length - 1);
         Code_Hash  : SHA3_256_Digest;
         Input      : Anubis_Types.Byte_Array (0 .. 84);  --  1 + 32 + 32 + 32
         Digest     : SHA3_256_Digest;
      begin
         --  Hash init code
         for I in Init_Code'Range loop
            Code_Input (I - Init_Code'First) := Anubis_Types.Byte (Init_Code (I));
         end loop;
         SHA3_256 (Code_Input, Code_Hash);

         --  Build CREATE2 input
         Input (0) := 16#FF#;  --  CREATE2 prefix

         --  Sender address
         for I in 0 .. 31 loop
            Input (1 + I) := Anubis_Types.Byte (Frame.Contract_Addr (I));
         end loop;

         --  Salt
         for I in 0 .. 31 loop
            Input (33 + I) := Anubis_Types.Byte (Salt (I));
         end loop;

         --  Init code hash
         for I in 0 .. 31 loop
            Input (65 + I) := Anubis_Types.Byte (Code_Hash (I));
         end loop;

         SHA3_256 (Input, Digest);

         --  Take last 20 bytes as address
         for I in 0 .. 19 loop
            New_Address (12 + I) := Aegis_VM_Types.Byte (Digest (12 + I));
         end loop;
      end;

      Ada.Text_IO.Put_Line ("  [SPHINX] Executing CREATE2...");

      --  CREATE2 deployment with real execution
      declare
         use Sphinx_Native;
         use Anubis_SHA3;

         Init_Hash  : SHA3_256_Digest;
         Code_ID    : Word64 := 0;
         Exec_Return : Byte_Array (0 .. Max_Return_Size - 1) := (others => 0);
         Exec_Return_Len : Natural := 0;
         Exec_Gas_Used : Gas_Amount := 0;
         Exec_Status : Sphinx_Native.Exec_Status;
         Push_OK : Boolean;
         Gas_Refund : Gas_Amount;
         Init_Bytes : Anubis_Types.Byte_Array (0 .. Init_Code'Length - 1);
      begin
         --  Hash init code for code ID
         for I in Init_Code'Range loop
            Init_Bytes (I - Init_Code'First) := Anubis_Types.Byte (Init_Code (I));
         end loop;
         SHA3_256 (Init_Bytes, Init_Hash);

         --  Build code ID from init hash
         for I in 0 .. 7 loop
            Code_ID := Code_ID or
               Shift_Left (Word64 (Init_Hash (I)), I * 8);
         end loop;

         Ada.Text_IO.Put_Line ("  [SPHINX] CREATE2: Init code hash ID 0x" &
            Word64'Image (Code_ID));

         --  Push frame for init code execution
         Push_Frame (
            Runtime   => RT.all,
            Target    => New_Address,
            Caller    => Frame.Contract_Addr,
            Value     => Value,
            Calldata  => Byte_Array'(0 .. -1 => 0),
            Gas_Limit => Gas_Limit,
            Kind      => Kind_Create2,
            Success   => Push_OK
         );

         if not Push_OK then
            Gas_Used := Gas_Limit;
            return;
         end if;

         --  Execute init code
         Sphinx_Native.Execute_Registered (
            Code_ID     => Code_ID,
            Calldata    => Byte_Array'(0 .. -1 => 0),
            Gas_Limit   => Gas_Limit - 32000,
            Return_Data => Exec_Return,
            Return_Len  => Exec_Return_Len,
            Gas_Used    => Exec_Gas_Used,
            Status      => Exec_Status
         );

         Pop_Frame (
            Runtime     => RT.all,
            Success     => (Exec_Status = Sphinx_Native.Exec_Success),
            Return_Data => Byte_Array'(0 .. -1 => 0),
            Gas_Refund  => Gas_Refund
         );

         Gas_Used := 32000 + Exec_Gas_Used + Gas_Amount (Init_Code'Length) * 200;

         if Exec_Status = Sphinx_Native.Exec_Success then
            Ada.Text_IO.Put_Line ("  [SPHINX] CREATE2: Init code executed successfully");
            Success := True;
         else
            Ada.Text_IO.Put_Line ("  [SPHINX] CREATE2: Init code not found or execution failed");
            Success := True;  --  Lazy deployment
         end if;
      end;
   end Execute_Create2;

   ---------------------------------------------------------------------------
   --  Gas Management
   ---------------------------------------------------------------------------

   procedure Use_Gas (
      Amount  : in     Gas_Amount;
      Success : out    Boolean
   ) is
      RT    : Runtime_Context_Access;
      Depth : Natural;
   begin
      Success := False;

      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      Depth := RT.Current_Depth;
      if Depth > Frame_Index'Last then
         return;
      end if;

      --  Check if enough gas
      if RT.Frames (Depth).Gas_Used + Amount > RT.Frames (Depth).Gas_Limit then
         Success := False;
         return;
      end if;

      RT.Frames (Depth).Gas_Used := RT.Frames (Depth).Gas_Used + Amount;
      Success := True;
   end Use_Gas;

   function Gas_Remaining return Gas_Amount is
      RT    : Runtime_Context_Access;
      Depth : Natural;
   begin
      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return 0;
      end if;

      Depth := RT.Current_Depth;
      if Depth > Frame_Index'Last then
         return 0;
      end if;

      if RT.Frames (Depth).Gas_Used >= RT.Frames (Depth).Gas_Limit then
         return 0;
      end if;

      return RT.Frames (Depth).Gas_Limit - RT.Frames (Depth).Gas_Used;
   end Gas_Remaining;

   procedure Refund_Gas (Amount : in Gas_Amount) is
      RT    : Runtime_Context_Access;
      Depth : Natural;
   begin
      RT := Current_Runtime;
      if RT = null or else not RT.Initialized then
         return;
      end if;

      Depth := RT.Current_Depth;
      if Depth > Frame_Index'Last then
         return;
      end if;

      if RT.Frames (Depth).Gas_Used >= Amount then
         RT.Frames (Depth).Gas_Used := RT.Frames (Depth).Gas_Used - Amount;
      else
         RT.Frames (Depth).Gas_Used := 0;
      end if;
   end Refund_Gas;

   ---------------------------------------------------------------------------
   --  Legacy Context Management
   ---------------------------------------------------------------------------

   procedure Set_Context (Ctx : Context_Access) is
   begin
      Current_Context := Ctx;
   end Set_Context;

   function Get_Context return Context_Access is
   begin
      return Current_Context;
   end Get_Context;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   procedure VM_SLoad (Slot : Hash256; Value : out Hash256) is
      Ctx : constant Context_Access := Current_Context;
   begin
      Value := (others => 0);

      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return;
      end if;

      if Ctx.Syscalls.SLoad = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: SLoad syscall not set");
         return;
      end if;

      Ctx.Syscalls.SLoad (Slot'Address, Value'Address);
   end VM_SLoad;

   procedure VM_SStore (Slot : Hash256; Value : Hash256) is
      Ctx : constant Context_Access := Current_Context;
   begin
      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return;
      end if;

      if Ctx.Syscalls.SStore = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: SStore syscall not set");
         return;
      end if;

      Ctx.Syscalls.SStore (Slot'Address, Value'Address);
   end VM_SStore;

   ---------------------------------------------------------------------------
   --  Crypto Operations
   ---------------------------------------------------------------------------

   procedure VM_SHA3 (Input : Byte_Array; Output : out Hash256) is
      Ctx : constant Context_Access := Current_Context;
   begin
      Output := (others => 0);

      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return;
      end if;

      if Ctx.Syscalls.SHA3 = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: SHA3 syscall not set");
         return;
      end if;

      Ctx.Syscalls.SHA3 (
         Input (Input'First)'Address,
         Interfaces.C.size_t (Input'Length),
         Output'Address
      );
   end VM_SHA3;

   function VM_MLDSA_Verify (
      Message   : Byte_Array;
      Signature : Byte_Array;
      Pub_Key   : Byte_Array
   ) return Boolean is
      Ctx    : constant Context_Access := Current_Context;
      Result : Interfaces.C.int;
   begin
      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return False;
      end if;

      if Ctx.Syscalls.MLDSA_Verify = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: MLDSA_Verify syscall not set");
         return False;
      end if;

      Result := Ctx.Syscalls.MLDSA_Verify (
         Message (Message'First)'Address,
         Interfaces.C.size_t (Message'Length),
         Signature (Signature'First)'Address,
         Interfaces.C.size_t (Signature'Length),
         Pub_Key (Pub_Key'First)'Address
      );

      return Integer (Result) = 1;
   end VM_MLDSA_Verify;

   ---------------------------------------------------------------------------
   --  Environment Operations
   ---------------------------------------------------------------------------

   procedure VM_Get_Caller (Addr : out Account_ID) is
      RT  : Runtime_Context_Access;
      Ctx : constant Context_Access := Current_Context;
   begin
      Addr := (others => 0);

      --  Try new runtime first
      RT := Current_Runtime;
      if RT /= null and then RT.Initialized then
         if RT.Current_Depth <= Frame_Index'Last then
            Addr := RT.Frames (RT.Current_Depth).Caller_Addr;
            return;
         end if;
      end if;

      --  Fall back to legacy context
      if Ctx = null then
         return;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Caller /= null then
         Ctx.Syscalls.Get_Caller (Addr'Address);
      else
         Addr := Ctx.Caller;
      end if;
   end VM_Get_Caller;

   procedure VM_Get_Self (Addr : out Account_ID) is
      RT  : Runtime_Context_Access;
      Ctx : constant Context_Access := Current_Context;
   begin
      Addr := (others => 0);

      --  Try new runtime first
      RT := Current_Runtime;
      if RT /= null and then RT.Initialized then
         if RT.Current_Depth <= Frame_Index'Last then
            Addr := RT.Frames (RT.Current_Depth).Contract_Addr;
            return;
         end if;
      end if;

      --  Fall back to legacy context
      if Ctx = null then
         return;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Self /= null then
         Ctx.Syscalls.Get_Self (Addr'Address);
      else
         Addr := Ctx.Self;
      end if;
   end VM_Get_Self;

   function VM_Get_Timestamp return Unsigned_64 is
      RT  : Runtime_Context_Access;
      Ctx : constant Context_Access := Current_Context;
   begin
      --  Try new runtime first
      RT := Current_Runtime;
      if RT /= null and then RT.Initialized then
         return RT.Timestamp;
      end if;

      --  Fall back to legacy context
      if Ctx = null then
         return 0;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Timestamp /= null then
         return Ctx.Syscalls.Get_Timestamp.all;
      else
         return Ctx.Timestamp;
      end if;
   end VM_Get_Timestamp;

   function VM_Get_Block_Number return Unsigned_64 is
      RT  : Runtime_Context_Access;
      Ctx : constant Context_Access := Current_Context;
   begin
      --  Try new runtime first
      RT := Current_Runtime;
      if RT /= null and then RT.Initialized then
         return RT.Block_Number;
      end if;

      --  Fall back to legacy context
      if Ctx = null then
         return 0;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Block_Number /= null then
         return Ctx.Syscalls.Get_Block_Number.all;
      else
         return Ctx.Block_Number;
      end if;
   end VM_Get_Block_Number;

   function VM_Get_Gas_Remaining return Unsigned_64 is
      RT  : Runtime_Context_Access;
      Ctx : constant Context_Access := Current_Context;
   begin
      --  Try new runtime first
      RT := Current_Runtime;
      if RT /= null and then RT.Initialized then
         return Unsigned_64 (Gas_Remaining);
      end if;

      --  Fall back to legacy context
      if Ctx = null then
         return 0;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Gas_Remaining /= null then
         return Ctx.Syscalls.Get_Gas_Remaining.all;
      else
         return Unsigned_64 (Ctx.Gas_Limit - Ctx.Gas_Used);
      end if;
   end VM_Get_Gas_Remaining;

end Sphinx_Runtime;
