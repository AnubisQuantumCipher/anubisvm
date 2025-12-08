pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;
with Khepri_Crypto;

package body Khepri_Runtime is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   --  Current execution context (set by VM before call)
   Current_Context : Execution_Context := (
      Msg_Sender      => Null_Address,
      Msg_Value       => Zero,
      Origin          => Null_Address,
      Block_Number    => Zero,
      Block_Timestamp => Zero,
      Block_Coinbase  => Null_Address,
      Chain_ID        => Zero,
      Self_Address    => Null_Address,
      Code_Hash       => Bytes32_Zero,
      Gas_Limit       => Zero,
      Gas_Used        => Zero,
      Gas_Price       => Zero
   );

   --  Constructor state
   Constructor_Complete : Boolean := False;

   --  Reentrancy state
   Currently_Entered : Boolean := False;

   --  Execution result state (set by Revert/Return)
   Execution_Reverted : Boolean := False;
   Execution_Returned : Boolean := False;
   Revert_Reason_Buf  : Byte_Array (0 .. 255) := (others => 0);
   Revert_Reason_Len  : Natural := 0;
   Return_Data_Buf    : Byte_Array (0 .. Max_Return_Size - 1) := (others => 0);
   Return_Data_Len    : Natural := 0;

   ---------------------------------------------------------------------------
   --  Context Accessors
   ---------------------------------------------------------------------------

   function Get_Context return Execution_Context is
   begin
      return Current_Context;
   end Get_Context;

   function Msg_Sender return Address is
   begin
      return Current_Context.Msg_Sender;
   end Msg_Sender;

   function Msg_Value return Wei is
   begin
      return Current_Context.Msg_Value;
   end Msg_Value;

   function Block_Number return Uint256 is
   begin
      return Current_Context.Block_Number;
   end Block_Number;

   function Block_Timestamp return Uint256 is
   begin
      return Current_Context.Block_Timestamp;
   end Block_Timestamp;

   function Chain_ID return Uint256 is
   begin
      return Current_Context.Chain_ID;
   end Chain_ID;

   function Self return Address is
   begin
      return Current_Context.Self_Address;
   end Self;

   function Gas_Left return Uint256 is
      Used  : Uint256;
      Limit : Uint256;
      Left  : Uint256;
      Underflow : Boolean;
   begin
      Used := Current_Context.Gas_Used;
      Limit := Current_Context.Gas_Limit;
      Sub (Limit, Used, Left, Underflow);
      if Underflow then
         return Zero;
      end if;
      return Left;
   end Gas_Left;

   ---------------------------------------------------------------------------
   --  Selector Calculation
   ---------------------------------------------------------------------------

   function Calculate_Selector (Signature : String) return Selector is
      Hash : Bytes32;
      Sel  : Selector;
   begin
      Hash := Khepri_Crypto.Calculate_Signature (Signature);
      for I in 0 .. 3 loop
         Sel (I) := Hash (I);
      end loop;
      return Sel;
   end Calculate_Selector;

   ---------------------------------------------------------------------------
   --  Calldata Reader
   ---------------------------------------------------------------------------

   function Make_Reader (Data : Calldata_Buffer) return Calldata_Reader is
      Reader : Calldata_Reader;
   begin
      Reader.Size := Data'Length;
      Reader.Offset := 0;
      Reader.Data := (others => 0);
      for I in Data'Range loop
         Reader.Data (Calldata_Index (I - Data'First)) := Data (I);
      end loop;
      return Reader;
   end Make_Reader;

   function Get_Selector (Reader : Calldata_Reader) return Selector is
      Sel : Selector := (others => 0);
   begin
      if Reader.Size >= 4 then
         for I in 0 .. 3 loop
            Sel (I) := Reader.Data (Calldata_Index (I));
         end loop;
      end if;
      return Sel;
   end Get_Selector;

   function Calldata_Size (Reader : Calldata_Reader) return Natural is
   begin
      return Reader.Size;
   end Calldata_Size;

   ---------------------------------------------------------------------------
   --  ABI Decoding
   ---------------------------------------------------------------------------

   procedure Decode_U256 (
      Reader : in out Calldata_Reader;
      Value  :    out Uint256
   ) is
      Data : Bytes32 := (others => 0);
   begin
      if Reader.Offset + 32 <= Reader.Size then
         for I in 0 .. 31 loop
            Data (I) := Reader.Data (Calldata_Index (Reader.Offset + I));
         end loop;
         Reader.Offset := Reader.Offset + 32;
      end if;
      Value := From_Bytes_BE (Data);
   end Decode_U256;

   procedure Decode_Address (
      Reader : in out Calldata_Reader;
      Addr   :    out Address
   ) is
   begin
      Addr := Null_Address;
      if Reader.Offset + 32 <= Reader.Size then
         --  Address is in last 20 bytes of 32-byte word (left-padded)
         --  But KHEPRI uses 32-byte addresses
         for I in 0 .. 31 loop
            Addr (I) := Reader.Data (Calldata_Index (Reader.Offset + I));
         end loop;
         Reader.Offset := Reader.Offset + 32;
      end if;
   end Decode_Address;

   procedure Decode_Bool (
      Reader : in out Calldata_Reader;
      Value  :    out Boolean
   ) is
      U_Value : Uint256;
   begin
      Decode_U256 (Reader, U_Value);
      Value := not Is_Zero (U_Value);
   end Decode_Bool;

   procedure Decode_Bytes32 (
      Reader : in out Calldata_Reader;
      Value  :    out Bytes32
   ) is
   begin
      Value := (others => 0);
      if Reader.Offset + 32 <= Reader.Size then
         for I in 0 .. 31 loop
            Value (I) := Reader.Data (Calldata_Index (Reader.Offset + I));
         end loop;
         Reader.Offset := Reader.Offset + 32;
      end if;
   end Decode_Bytes32;

   procedure Skip_Bytes (
      Reader : in Out Calldata_Reader;
      Count  : in     Natural
   ) is
   begin
      if Reader.Offset + Count <= Reader.Size then
         Reader.Offset := Reader.Offset + Count;
      else
         Reader.Offset := Reader.Size;
      end if;
   end Skip_Bytes;

   ---------------------------------------------------------------------------
   --  ABI Encoding
   ---------------------------------------------------------------------------

   function Make_Writer return Return_Writer is
   begin
      return (Data => (others => 0), Size => 0);
   end Make_Writer;

   procedure Encode_U256 (
      Writer : in Out Return_Writer;
      Value  : in     Uint256
   ) is
      Data : constant Bytes32 := To_Bytes_BE (Value);
   begin
      if Writer.Size + 32 <= Max_Return_Size then
         for I in 0 .. 31 loop
            Writer.Data (Writer.Size + I) := Data (I);
         end loop;
         Writer.Size := Writer.Size + 32;
      end if;
   end Encode_U256;

   procedure Encode_Address (
      Writer : in Out Return_Writer;
      Addr   : in     Address
   ) is
   begin
      if Writer.Size + 32 <= Max_Return_Size then
         for I in 0 .. 31 loop
            Writer.Data (Writer.Size + I) := Addr (I);
         end loop;
         Writer.Size := Writer.Size + 32;
      end if;
   end Encode_Address;

   procedure Encode_Bool (
      Writer : in Out Return_Writer;
      Value  : in     Boolean
   ) is
   begin
      if Writer.Size + 32 <= Max_Return_Size then
         for I in 0 .. 30 loop
            Writer.Data (Writer.Size + I) := 0;
         end loop;
         if Value then
            Writer.Data (Writer.Size + 31) := 1;
         else
            Writer.Data (Writer.Size + 31) := 0;
         end if;
         Writer.Size := Writer.Size + 32;
      end if;
   end Encode_Bool;

   procedure Encode_Bytes32 (
      Writer : in Out Return_Writer;
      Value  : in     Bytes32
   ) is
   begin
      if Writer.Size + 32 <= Max_Return_Size then
         for I in 0 .. 31 loop
            Writer.Data (Writer.Size + I) := Value (I);
         end loop;
         Writer.Size := Writer.Size + 32;
      end if;
   end Encode_Bytes32;

   function Return_Size (Writer : Return_Writer) return Natural is
   begin
      return Writer.Size;
   end Return_Size;

   procedure Finalize_Return (
      Writer : in     Return_Writer;
      Data   :    out Byte_Array;
      Size   :    out Natural
   ) is
   begin
      Size := Writer.Size;
      for I in 0 .. Size - 1 loop
         Data (Data'First + I) := Writer.Data (I);
      end loop;
   end Finalize_Return;

   ---------------------------------------------------------------------------
   --  Contract Dispatch
   ---------------------------------------------------------------------------

   procedure Dispatch (
      Handlers      : in     Handler_Table;
      Handler_Count : in     Natural;
      Calldata      : in     Calldata_Buffer;
      Return_Data   : out    Byte_Array;
      Return_Size   : out    Natural;
      Result        : out    Dispatch_Result
   ) is
      Reader   : Calldata_Reader;
      Writer   : Return_Writer;
      Sel      : Selector;
      Found    : Boolean := False;
      Success  : Boolean := False;
   begin
      Return_Size := 0;
      Return_Data := (others => 0);

      --  Need at least 4 bytes for selector
      if Calldata'Length < 4 then
         Result := Dispatch_Invalid_Calldata;
         return;
      end if;

      Reader := Make_Reader (Calldata);
      Sel := Get_Selector (Reader);
      Skip_Bytes (Reader, 4);  -- Move past selector

      Writer := Make_Writer;

      --  Find matching handler
      for I in 0 .. Handler_Count - 1 loop
         if Handlers (Handler_Index (I)).Selector = Sel then
            Found := True;
            if Handlers (Handler_Index (I)).Handler /= null then
               Handlers (Handler_Index (I)).Handler.all (Reader, Writer, Success);
            end if;
            exit;
         end if;
      end loop;

      if not Found then
         Result := Dispatch_Selector_Not_Found;
         return;
      end if;

      if not Success then
         Result := Dispatch_Handler_Error;
         return;
      end if;

      Finalize_Return (Writer, Return_Data, Return_Size);
      Result := Dispatch_Success;
   end Dispatch;

   ---------------------------------------------------------------------------
   --  Contract Lifecycle
   ---------------------------------------------------------------------------

   --  Contract initialization tracking (set by VM when deploying)
   Is_Deploying : Boolean := False;

   procedure Set_Deploying (Value : Boolean) is
   begin
      Is_Deploying := Value;
   end Set_Deploying;

   function Is_Constructor_Call return Boolean is
   begin
      --  During deployment, first call is constructor
      --  After deployment, all calls are regular function calls
      return Is_Deploying and not Constructor_Complete;
   end Is_Constructor_Call;

   procedure Complete_Constructor is
   begin
      Constructor_Complete := True;
   end Complete_Constructor;

   function Is_Initialized return Boolean is
   begin
      return Constructor_Complete;
   end Is_Initialized;

   ---------------------------------------------------------------------------
   --  Control Flow
   ---------------------------------------------------------------------------

   procedure Revert (Reason : String) is
   begin
      --  Set revert state with reason
      Execution_Reverted := True;
      Execution_Returned := False;
      Revert_Reason_Len := Natural'Min (Reason'Length, 256);

      --  Copy reason to buffer
      for I in 0 .. Revert_Reason_Len - 1 loop
         Revert_Reason_Buf (I) := Byte (Character'Pos (Reason (Reason'First + I)));
      end loop;
   end Revert;

   procedure Revert_Code (Code : Error_Code) is
      Code_Val : constant Unsigned_32 := Unsigned_32 (Error_Code'Pos (Code));
   begin
      --  Set revert state with error code as 4-byte selector
      Execution_Reverted := True;
      Execution_Returned := False;
      Revert_Reason_Len := 4;

      --  Encode error code position in big-endian
      Revert_Reason_Buf (0) := Byte (Shift_Right (Code_Val, 24) and 16#FF#);
      Revert_Reason_Buf (1) := Byte (Shift_Right (Code_Val, 16) and 16#FF#);
      Revert_Reason_Buf (2) := Byte (Shift_Right (Code_Val, 8) and 16#FF#);
      Revert_Reason_Buf (3) := Byte (Code_Val and 16#FF#);
   end Revert_Code;

   procedure Return_Void is
   begin
      --  Set return state with empty data
      Execution_Returned := True;
      Execution_Reverted := False;
      Return_Data_Len := 0;
   end Return_Void;

   procedure Return_Data (Data : Byte_Array) is
   begin
      --  Set return state with data
      Execution_Returned := True;
      Execution_Reverted := False;
      Return_Data_Len := Natural'Min (Data'Length, Max_Return_Size);

      --  Copy return data to buffer
      for I in 0 .. Return_Data_Len - 1 loop
         Return_Data_Buf (I) := Data (Data'First + I);
      end loop;
   end Return_Data;

   ---------------------------------------------------------------------------
   --  Modifier-style Checks
   ---------------------------------------------------------------------------

   procedure Require (
      Condition : Boolean;
      Message   : String
   ) is
   begin
      if not Condition then
         Revert (Message);
      end if;
   end Require;

   procedure Only_Owner (Owner_Address : Address) is
   begin
      if not Khepri_Crypto.Address_Equal (Msg_Sender, Owner_Address) then
         Revert ("Only owner");
      end if;
   end Only_Owner;

   procedure Enter_Non_Reentrant is
   begin
      if Currently_Entered then
         Revert ("Reentrant call");
      end if;
      Currently_Entered := True;
   end Enter_Non_Reentrant;

   procedure Exit_Non_Reentrant is
   begin
      Currently_Entered := False;
   end Exit_Non_Reentrant;

   procedure Require_Payment is
   begin
      if Is_Zero (Msg_Value) then
         Revert ("Payment required");
      end if;
   end Require_Payment;

   procedure Require_No_Payment is
   begin
      if not Is_Zero (Msg_Value) then
         Revert ("Non-payable");
      end if;
   end Require_No_Payment;

   ---------------------------------------------------------------------------
   --  Execution State Query (for VM integration)
   ---------------------------------------------------------------------------

   function Was_Reverted return Boolean is
   begin
      return Execution_Reverted;
   end Was_Reverted;

   function Was_Returned return Boolean is
   begin
      return Execution_Returned;
   end Was_Returned;

   procedure Get_Revert_Data (
      Data   : out Byte_Array;
      Length : out Natural
   ) is
   begin
      Length := Revert_Reason_Len;
      Data := (others => 0);
      for I in 0 .. Revert_Reason_Len - 1 loop
         Data (Data'First + I) := Revert_Reason_Buf (I);
      end loop;
   end Get_Revert_Data;

   procedure Get_Return_Data (
      Data   : out Byte_Array;
      Length : out Natural
   ) is
   begin
      Length := Return_Data_Len;
      Data := (others => 0);
      for I in 0 .. Return_Data_Len - 1 loop
         Data (Data'First + I) := Return_Data_Buf (I);
      end loop;
   end Get_Return_Data;

   procedure Reset_Execution_State is
   begin
      Execution_Reverted := False;
      Execution_Returned := False;
      Revert_Reason_Len := 0;
      Return_Data_Len := 0;
   end Reset_Execution_State;

end Khepri_Runtime;
