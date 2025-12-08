pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;
with Khepri_Crypto;

package body Khepri_Events is

   ---------------------------------------------------------------------------
   --  Internal Event Emission
   ---------------------------------------------------------------------------

   --  Event log storage for SDK embedded contracts
   Max_Logs : constant := 256;
   type Log_Record is record
      Topics      : Topic_Array;
      Topic_Count : Natural;
      Data        : Data_Array;
      Data_Size   : Natural;
      Valid       : Boolean;
   end record;
   type Log_Array_Type is array (0 .. Max_Logs - 1) of Log_Record;
   Event_Logs : Log_Array_Type := (others => (
      Topics => (others => (others => 0)),
      Topic_Count => 0,
      Data => (others => 0),
      Data_Size => 0,
      Valid => False
   ));
   Event_Count : Natural := 0;

   procedure Clear_Events is
   begin
      Event_Count := 0;
      Event_Logs := (others => (
         Topics => (others => (others => 0)),
         Topic_Count => 0,
         Data => (others => 0),
         Data_Size => 0,
         Valid => False
      ));
   end Clear_Events;

   function Get_Event_Count return Natural is
   begin
      return Event_Count;
   end Get_Event_Count;

   procedure Internal_Emit (
      Topics      : Topic_Array;
      Topic_Count : Natural;
      Data        : Data_Array;
      Data_Size   : Natural
   ) with
      Global => (In_Out => (Event_Logs, Event_Count))
   is
   begin
      --  Record log entry for SDK embedded contracts
      --  In full VM, this would call AEGIS syscall layer:
      --  1. Validate topic count (0-4)
      --  2. Charge gas (375 base + 375*topics + 8*data_bytes)
      --  3. Check capabilities (Cap_Event required)
      --  4. Add to transaction receipt

      if Topic_Count <= Max_Topics and Data_Size <= Max_Data_Size and
         Event_Count < Max_Logs
      then
         Event_Logs (Event_Count) := (
            Topics      => Topics,
            Topic_Count => Topic_Count,
            Data        => Data,
            Data_Size   => Data_Size,
            Valid       => True
         );
         Event_Count := Event_Count + 1;
      end if;
   end Internal_Emit;

   ---------------------------------------------------------------------------
   --  Basic Event Emission
   ---------------------------------------------------------------------------

   procedure Emit_Log0 (
      Data       : Byte_Array;
      Data_Size  : Natural
   ) is
      Topics : Topic_Array := (others => (others => 0));
      Log_Data : Data_Array := (others => 0);
   begin
      for I in 0 .. Data_Size - 1 loop
         Log_Data (I) := Data (Data'First + I);
      end loop;
      Internal_Emit (Topics, 0, Log_Data, Data_Size);
   end Emit_Log0;

   procedure Emit_Log1 (
      Topic0     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) is
      Topics : Topic_Array := (others => (others => 0));
      Log_Data : Data_Array := (others => 0);
   begin
      Topics (0) := Topic0;
      for I in 0 .. Data_Size - 1 loop
         Log_Data (I) := Data (Data'First + I);
      end loop;
      Internal_Emit (Topics, 1, Log_Data, Data_Size);
   end Emit_Log1;

   procedure Emit_Log2 (
      Topic0     : Bytes32;
      Topic1     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) is
      Topics : Topic_Array := (others => (others => 0));
      Log_Data : Data_Array := (others => 0);
   begin
      Topics (0) := Topic0;
      Topics (1) := Topic1;
      for I in 0 .. Data_Size - 1 loop
         Log_Data (I) := Data (Data'First + I);
      end loop;
      Internal_Emit (Topics, 2, Log_Data, Data_Size);
   end Emit_Log2;

   procedure Emit_Log3 (
      Topic0     : Bytes32;
      Topic1     : Bytes32;
      Topic2     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) is
      Topics : Topic_Array := (others => (others => 0));
      Log_Data : Data_Array := (others => 0);
   begin
      Topics (0) := Topic0;
      Topics (1) := Topic1;
      Topics (2) := Topic2;
      for I in 0 .. Data_Size - 1 loop
         Log_Data (I) := Data (Data'First + I);
      end loop;
      Internal_Emit (Topics, 3, Log_Data, Data_Size);
   end Emit_Log3;

   procedure Emit_Log4 (
      Topic0     : Bytes32;
      Topic1     : Bytes32;
      Topic2     : Bytes32;
      Topic3     : Bytes32;
      Data       : Byte_Array;
      Data_Size  : Natural
   ) is
      Topics : Topic_Array := (others => (others => 0));
      Log_Data : Data_Array := (others => 0);
   begin
      Topics (0) := Topic0;
      Topics (1) := Topic1;
      Topics (2) := Topic2;
      Topics (3) := Topic3;
      for I in 0 .. Data_Size - 1 loop
         Log_Data (I) := Data (Data'First + I);
      end loop;
      Internal_Emit (Topics, 4, Log_Data, Data_Size);
   end Emit_Log4;

   ---------------------------------------------------------------------------
   --  High-Level Event Helpers
   ---------------------------------------------------------------------------

   procedure Emit_Transfer (
      From   : Address;
      To     : Address;
      Amount : Uint256
   ) is
      Amount_Bytes : constant Bytes32 := Encode_U256 (Amount);
      Data : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Data (I) := Amount_Bytes (I);
      end loop;
      Emit_Log3 (
         Topic0 => Transfer_Sig,
         Topic1 => Encode_Address (From),
         Topic2 => Encode_Address (To),
         Data   => Data,
         Data_Size => 32
      );
   end Emit_Transfer;

   procedure Emit_Approval (
      Owner   : Address;
      Spender : Address;
      Amount  : Uint256
   ) is
      Amount_Bytes : constant Bytes32 := Encode_U256 (Amount);
      Data : Byte_Array (0 .. 31);
   begin
      for I in 0 .. 31 loop
         Data (I) := Amount_Bytes (I);
      end loop;
      Emit_Log3 (
         Topic0 => Approval_Sig,
         Topic1 => Encode_Address (Owner),
         Topic2 => Encode_Address (Spender),
         Data   => Data,
         Data_Size => 32
      );
   end Emit_Approval;

   procedure Emit_Ownership_Transferred (
      Previous_Owner : Address;
      New_Owner      : Address
   ) is
      Empty_Data : constant Byte_Array (0 .. 0) := (others => 0);
   begin
      Emit_Log3 (
         Topic0 => Ownership_Transferred_Sig,
         Topic1 => Encode_Address (Previous_Owner),
         Topic2 => Encode_Address (New_Owner),
         Data   => Empty_Data,
         Data_Size => 0
      );
   end Emit_Ownership_Transferred;

   ---------------------------------------------------------------------------
   --  Custom Event Construction
   ---------------------------------------------------------------------------

   function New_Event (Signature : Event_Signature) return Event_Builder is
      Builder : Event_Builder := Empty_Builder;
   begin
      Builder.Topics (0) := Signature;
      Builder.Topic_Count := 1;
      return Builder;
   end New_Event;

   function Add_Topic (
      Builder : Event_Builder;
      Topic   : Bytes32
   ) return Event_Builder is
      Result : Event_Builder := Builder;
   begin
      if Result.Topic_Count < Max_Topics then
         Result.Topics (Result.Topic_Count) := Topic;
         Result.Topic_Count := Result.Topic_Count + 1;
      end if;
      return Result;
   end Add_Topic;

   function Add_Address_Topic (
      Builder : Event_Builder;
      Addr    : Address
   ) return Event_Builder is
   begin
      return Add_Topic (Builder, Encode_Address (Addr));
   end Add_Address_Topic;

   function Add_U256_Topic (
      Builder : Event_Builder;
      Value   : Uint256
   ) return Event_Builder is
   begin
      return Add_Topic (Builder, Encode_U256 (Value));
   end Add_U256_Topic;

   function Add_U256_Data (
      Builder : Event_Builder;
      Value   : Uint256
   ) return Event_Builder is
      Result : Event_Builder := Builder;
      Bytes  : constant Bytes32 := Encode_U256 (Value);
   begin
      if Result.Data_Size + 32 <= Max_Data_Size then
         for I in 0 .. 31 loop
            Result.Data (Result.Data_Size + I) := Bytes (I);
         end loop;
         Result.Data_Size := Result.Data_Size + 32;
      end if;
      return Result;
   end Add_U256_Data;

   function Add_Address_Data (
      Builder : Event_Builder;
      Addr    : Address
   ) return Event_Builder is
      Result : Event_Builder := Builder;
      Bytes  : constant Bytes32 := Encode_Address (Addr);
   begin
      if Result.Data_Size + 32 <= Max_Data_Size then
         for I in 0 .. 31 loop
            Result.Data (Result.Data_Size + I) := Bytes (I);
         end loop;
         Result.Data_Size := Result.Data_Size + 32;
      end if;
      return Result;
   end Add_Address_Data;

   function Add_Bytes_Data (
      Builder : Event_Builder;
      Data    : Byte_Array
   ) return Event_Builder is
      Result : Event_Builder := Builder;
   begin
      if Result.Data_Size + Data'Length <= Max_Data_Size then
         for I in Data'Range loop
            Result.Data (Result.Data_Size + I - Data'First) := Data (I);
         end loop;
         Result.Data_Size := Result.Data_Size + Data'Length;
      end if;
      return Result;
   end Add_Bytes_Data;

   procedure Emit (Builder : Event_Builder) is
   begin
      Internal_Emit (
         Builder.Topics,
         Builder.Topic_Count,
         Builder.Data,
         Builder.Data_Size
      );
   end Emit;

   ---------------------------------------------------------------------------
   --  Event Signature Calculation
   ---------------------------------------------------------------------------

   function Calculate_Signature (
      Declaration : String
   ) return Event_Signature is
      Data : Byte_Array (0 .. Declaration'Length - 1);
   begin
      for I in Declaration'Range loop
         Data (I - Declaration'First) := Byte (Character'Pos (Declaration (I)));
      end loop;
      return Khepri_Crypto.Keccak_256 (Data);
   end Calculate_Signature;

   ---------------------------------------------------------------------------
   --  Encoding Helpers
   ---------------------------------------------------------------------------

   function Encode_Address (Addr : Address) return Bytes32 is
      Result : Bytes32 := (others => 0);
   begin
      --  Address is 32 bytes, same as Bytes32
      for I in 0 .. 31 loop
         Result (I) := Addr (I);
      end loop;
      return Result;
   end Encode_Address;

   function Encode_U256 (Value : Uint256) return Bytes32 is
   begin
      return To_Bytes_BE (Value);
   end Encode_U256;

   function Encode_Bool (Value : Boolean) return Bytes32 is
      Result : Bytes32 := (others => 0);
   begin
      if Value then
         Result (31) := 1;
      end if;
      return Result;
   end Encode_Bool;

   function Encode_Natural (Value : Natural) return Bytes32 is
   begin
      return Encode_U256 (From_Word64 (Word64 (Value)));
   end Encode_Natural;

end Khepri_Events;
