pragma SPARK_Mode (On);

with Anubis_SHA3;
with Anubis_Types;
with Interfaces; use Interfaces;

package body Contract_Events with
   SPARK_Mode => On
is

   --  Type conversion helpers
   subtype Local_Byte is Anubis_Types.Byte;

   function To_VM (B : Local_Byte) return Aegis_VM_Types.Byte is
      (Aegis_VM_Types.Byte (B)) with Inline;

   ---------------------------------------------------------------------------
   --  Event Buffer Operations
   ---------------------------------------------------------------------------

   procedure Initialize_Buffer (Buffer : out Event_Buffer) is
   begin
      Buffer.Count := 0;
      --  No need to initialize Events array, Count controls valid entries
   end Initialize_Buffer;

   --  Internal: Compute gas cost for an event
   function Compute_Event_Gas (
      Topic_Count : Natural;
      Data_Size   : Natural
   ) return Gas_Amount is
   begin
      return Event_Base_Gas +
             Gas_Amount (Topic_Count) * Event_Topic_Gas +
             Gas_Amount (Data_Size) * Event_Data_Byte_Gas;
   end Compute_Event_Gas;

   procedure Emit_Event_0 (
      Buffer    : in Out Event_Buffer;
      Contract  : in     Contract_Address;
      Data      : in     Args_Buffer;
      Data_Size : in     Natural;
      Gas_Used  : out    Gas_Amount;
      Success   : out    Boolean
   ) is
   begin
      Gas_Used := Compute_Event_Gas (0, Data_Size);

      if Buffer.Count >= Max_Events_Per_Tx then
         Success := False;
         return;
      end if;

      Buffer.Events (Event_Index (Buffer.Count)).Contract := Contract;
      Buffer.Events (Event_Index (Buffer.Count)).Topic_Count := 0;
      Buffer.Events (Event_Index (Buffer.Count)).Topics := (others => (others => 0));
      Buffer.Events (Event_Index (Buffer.Count)).Data := Data;
      Buffer.Events (Event_Index (Buffer.Count)).Data_Size := Data_Size;
      Buffer.Events (Event_Index (Buffer.Count)).Block_Number := U256_Zero;
      Buffer.Events (Event_Index (Buffer.Count)).Tx_Index := 0;
      Buffer.Events (Event_Index (Buffer.Count)).Log_Index := Buffer.Count;
      Buffer.Count := Buffer.Count + 1;
      Success := True;
   end Emit_Event_0;

   procedure Emit_Event_1 (
      Buffer    : in Out Event_Buffer;
      Contract  : in     Contract_Address;
      Topic0    : in     Hash256;
      Data      : in     Args_Buffer;
      Data_Size : in     Natural;
      Gas_Used  : out    Gas_Amount;
      Success   : out    Boolean
   ) is
   begin
      Gas_Used := Compute_Event_Gas (1, Data_Size);

      if Buffer.Count >= Max_Events_Per_Tx then
         Success := False;
         return;
      end if;

      Buffer.Events (Event_Index (Buffer.Count)).Contract := Contract;
      Buffer.Events (Event_Index (Buffer.Count)).Topic_Count := 1;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (0) := Topic0;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (1) := (others => 0);
      Buffer.Events (Event_Index (Buffer.Count)).Topics (2) := (others => 0);
      Buffer.Events (Event_Index (Buffer.Count)).Topics (3) := (others => 0);
      Buffer.Events (Event_Index (Buffer.Count)).Data := Data;
      Buffer.Events (Event_Index (Buffer.Count)).Data_Size := Data_Size;
      Buffer.Events (Event_Index (Buffer.Count)).Block_Number := U256_Zero;
      Buffer.Events (Event_Index (Buffer.Count)).Tx_Index := 0;
      Buffer.Events (Event_Index (Buffer.Count)).Log_Index := Buffer.Count;
      Buffer.Count := Buffer.Count + 1;
      Success := True;
   end Emit_Event_1;

   procedure Emit_Event_2 (
      Buffer    : in Out Event_Buffer;
      Contract  : in     Contract_Address;
      Topic0    : in     Hash256;
      Topic1    : in     Hash256;
      Data      : in     Args_Buffer;
      Data_Size : in     Natural;
      Gas_Used  : out    Gas_Amount;
      Success   : out    Boolean
   ) is
   begin
      Gas_Used := Compute_Event_Gas (2, Data_Size);

      if Buffer.Count >= Max_Events_Per_Tx then
         Success := False;
         return;
      end if;

      Buffer.Events (Event_Index (Buffer.Count)).Contract := Contract;
      Buffer.Events (Event_Index (Buffer.Count)).Topic_Count := 2;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (0) := Topic0;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (1) := Topic1;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (2) := (others => 0);
      Buffer.Events (Event_Index (Buffer.Count)).Topics (3) := (others => 0);
      Buffer.Events (Event_Index (Buffer.Count)).Data := Data;
      Buffer.Events (Event_Index (Buffer.Count)).Data_Size := Data_Size;
      Buffer.Events (Event_Index (Buffer.Count)).Block_Number := U256_Zero;
      Buffer.Events (Event_Index (Buffer.Count)).Tx_Index := 0;
      Buffer.Events (Event_Index (Buffer.Count)).Log_Index := Buffer.Count;
      Buffer.Count := Buffer.Count + 1;
      Success := True;
   end Emit_Event_2;

   procedure Emit_Event_3 (
      Buffer    : in Out Event_Buffer;
      Contract  : in     Contract_Address;
      Topic0    : in     Hash256;
      Topic1    : in     Hash256;
      Topic2    : in     Hash256;
      Data      : in     Args_Buffer;
      Data_Size : in     Natural;
      Gas_Used  : out    Gas_Amount;
      Success   : out    Boolean
   ) is
   begin
      Gas_Used := Compute_Event_Gas (3, Data_Size);

      if Buffer.Count >= Max_Events_Per_Tx then
         Success := False;
         return;
      end if;

      Buffer.Events (Event_Index (Buffer.Count)).Contract := Contract;
      Buffer.Events (Event_Index (Buffer.Count)).Topic_Count := 3;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (0) := Topic0;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (1) := Topic1;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (2) := Topic2;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (3) := (others => 0);
      Buffer.Events (Event_Index (Buffer.Count)).Data := Data;
      Buffer.Events (Event_Index (Buffer.Count)).Data_Size := Data_Size;
      Buffer.Events (Event_Index (Buffer.Count)).Block_Number := U256_Zero;
      Buffer.Events (Event_Index (Buffer.Count)).Tx_Index := 0;
      Buffer.Events (Event_Index (Buffer.Count)).Log_Index := Buffer.Count;
      Buffer.Count := Buffer.Count + 1;
      Success := True;
   end Emit_Event_3;

   procedure Emit_Event_4 (
      Buffer    : in Out Event_Buffer;
      Contract  : in     Contract_Address;
      Topic0    : in     Hash256;
      Topic1    : in     Hash256;
      Topic2    : in     Hash256;
      Topic3    : in     Hash256;
      Data      : in     Args_Buffer;
      Data_Size : in     Natural;
      Gas_Used  : out    Gas_Amount;
      Success   : out    Boolean
   ) is
   begin
      Gas_Used := Compute_Event_Gas (4, Data_Size);

      if Buffer.Count >= Max_Events_Per_Tx then
         Success := False;
         return;
      end if;

      Buffer.Events (Event_Index (Buffer.Count)).Contract := Contract;
      Buffer.Events (Event_Index (Buffer.Count)).Topic_Count := 4;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (0) := Topic0;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (1) := Topic1;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (2) := Topic2;
      Buffer.Events (Event_Index (Buffer.Count)).Topics (3) := Topic3;
      Buffer.Events (Event_Index (Buffer.Count)).Data := Data;
      Buffer.Events (Event_Index (Buffer.Count)).Data_Size := Data_Size;
      Buffer.Events (Event_Index (Buffer.Count)).Block_Number := U256_Zero;
      Buffer.Events (Event_Index (Buffer.Count)).Tx_Index := 0;
      Buffer.Events (Event_Index (Buffer.Count)).Log_Index := Buffer.Count;
      Buffer.Count := Buffer.Count + 1;
      Success := True;
   end Emit_Event_4;

   ---------------------------------------------------------------------------
   --  Event Topic Computation
   ---------------------------------------------------------------------------

   procedure Compute_Event_Signature (
      Event_Sig : in     String;
      Topic     : out    Hash256
   ) is
      Input  : Anubis_Types.Byte_Array (0 .. Event_Sig'Length - 1);
      Output : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Convert string to byte array
      for I in Event_Sig'Range loop
         Input (I - Event_Sig'First) := Local_Byte (Character'Pos (Event_Sig (I)));
      end loop;

      --  Compute SHA3-256 hash
      Anubis_SHA3.SHA3_256 (Input, Output);

      --  Convert to VM bytes
      for I in Topic'Range loop
         Topic (I) := To_VM (Output (I));
      end loop;
   end Compute_Event_Signature;

   procedure Address_To_Topic (
      Addr  : in     Contract_Address;
      Topic : out    Hash256
   ) is
   begin
      --  Address is already 32 bytes, just copy
      for I in Addr'Range loop
         Topic (I) := Addr (I);
      end loop;
   end Address_To_Topic;

   procedure U256_To_Topic (
      Value : in     U256;
      Topic : out    Hash256
   ) is
   begin
      --  Convert U256 to big-endian 32 bytes
      for I in 0 .. 3 loop
         declare
            Limb : constant Unsigned_64 := Value.Limbs (3 - I);
         begin
            for J in 0 .. 7 loop
               Topic (I * 8 + J) := Aegis_VM_Types.Byte (
                  Shift_Right (Limb, (7 - J) * 8) and 16#FF#);
            end loop;
         end;
      end loop;
   end U256_To_Topic;

   ---------------------------------------------------------------------------
   --  Event Filtering
   ---------------------------------------------------------------------------

   function Matches_Filter (
      Event  : Contract_Event;
      Filter : Event_Filter
   ) return Boolean is
   begin
      --  Check contract filter
      if Filter.Filter_Contract then
         for I in Contract_Address'Range loop
            if Event.Contract (I) /= Filter.Contract (I) then
               return False;
            end if;
         end loop;
      end if;

      --  Check topic filters
      for T in Topic_Index loop
         if Filter.Topic_Masks (T) then
            if T >= Event.Topic_Count then
               return False;  -- Event doesn't have this topic
            end if;

            for I in Hash256'Range loop
               if Event.Topics (T) (I) /= Filter.Topics (T) (I) then
                  return False;
               end if;
            end loop;
         end if;
      end loop;

      --  Block range check would go here (requires block number in event)

      return True;
   end Matches_Filter;

   ---------------------------------------------------------------------------
   --  Bloom Filter for Events
   ---------------------------------------------------------------------------

   --  Internal: Add a 32-byte value to the Bloom filter
   --  Uses 3 hash indices, each from different parts of the hash
   procedure Add_Value_To_Bloom (
      Bloom : in Out Bloom_Filter;
      Value : in     Hash256
   ) is
      --  Extract 3 indices from the hash (11 bits each = 0-2047, mapped to bytes)
      Idx1 : constant Natural := (Natural (Value (0)) * 256 + Natural (Value (1))) mod 2048;
      Idx2 : constant Natural := (Natural (Value (2)) * 256 + Natural (Value (3))) mod 2048;
      Idx3 : constant Natural := (Natural (Value (4)) * 256 + Natural (Value (5))) mod 2048;

      --  Convert bit index to byte index and bit mask
      Byte1 : constant Natural := Idx1 / 8;
      Bit1  : constant Natural := Idx1 mod 8;
      Byte2 : constant Natural := Idx2 / 8;
      Bit2  : constant Natural := Idx2 mod 8;
      Byte3 : constant Natural := Idx3 / 8;
      Bit3  : constant Natural := Idx3 mod 8;
   begin
      Bloom (Byte1) := Bloom (Byte1) or Shift_Left (1, Bit1);
      Bloom (Byte2) := Bloom (Byte2) or Shift_Left (1, Bit2);
      Bloom (Byte3) := Bloom (Byte3) or Shift_Left (1, Bit3);
   end Add_Value_To_Bloom;

   --  Internal: Check if a value might be in the Bloom filter
   function Check_Value_In_Bloom (
      Bloom : Bloom_Filter;
      Value : Hash256
   ) return Boolean is
      Idx1 : constant Natural := (Natural (Value (0)) * 256 + Natural (Value (1))) mod 2048;
      Idx2 : constant Natural := (Natural (Value (2)) * 256 + Natural (Value (3))) mod 2048;
      Idx3 : constant Natural := (Natural (Value (4)) * 256 + Natural (Value (5))) mod 2048;

      Byte1 : constant Natural := Idx1 / 8;
      Bit1  : constant Natural := Idx1 mod 8;
      Byte2 : constant Natural := Idx2 / 8;
      Bit2  : constant Natural := Idx2 mod 8;
      Byte3 : constant Natural := Idx3 / 8;
      Bit3  : constant Natural := Idx3 mod 8;
   begin
      return (Bloom (Byte1) and Shift_Left (1, Bit1)) /= 0 and then
             (Bloom (Byte2) and Shift_Left (1, Bit2)) /= 0 and then
             (Bloom (Byte3) and Shift_Left (1, Bit3)) /= 0;
   end Check_Value_In_Bloom;

   procedure Add_To_Bloom (
      Bloom    : in Out Bloom_Filter;
      Contract : in     Contract_Address;
      Topics   : in     Topic_Array;
      Count    : in     Natural
   ) is
      Addr_Hash : Hash256;
   begin
      --  Add contract address
      Address_To_Topic (Contract, Addr_Hash);
      Add_Value_To_Bloom (Bloom, Addr_Hash);

      --  Add each topic
      for T in 0 .. Natural'Min (Count, Max_Topics) - 1 loop
         Add_Value_To_Bloom (Bloom, Topics (Topic_Index (T)));
      end loop;
   end Add_To_Bloom;

   function Maybe_In_Bloom (
      Bloom    : Bloom_Filter;
      Contract : Contract_Address;
      Topics   : Topic_Array;
      Count    : Natural
   ) return Boolean is
      Addr_Hash : Hash256;
   begin
      --  Check contract address
      Address_To_Topic (Contract, Addr_Hash);
      if not Check_Value_In_Bloom (Bloom, Addr_Hash) then
         return False;
      end if;

      --  Check each topic
      for T in 0 .. Natural'Min (Count, Max_Topics) - 1 loop
         if not Check_Value_In_Bloom (Bloom, Topics (Topic_Index (T))) then
            return False;
         end if;
      end loop;

      return True;
   end Maybe_In_Bloom;

   ---------------------------------------------------------------------------
   --  Conversion to Node Types
   ---------------------------------------------------------------------------

   procedure To_Log_Entry (
      Event : in     Contract_Event;
      Log   : out    Log_Entry
   ) is
   begin
      Log.Contract := Event.Contract;
      --  Use topic0 as the topic hash
      if Event.Topic_Count > 0 then
         Log.Topic_Hash := Event.Topics (0);
      else
         Log.Topic_Hash := (others => 0);
      end if;
      Log.Data := Event.Data;
      Log.Data_Size := Event.Data_Size;
   end To_Log_Entry;

   procedure To_Log_Array (
      Buffer    : in     Event_Buffer;
      Logs      : out    Log_Array;
      Log_Count : out    Natural
   ) is
      Max_Copy : constant Natural := Natural'Min (Buffer.Count, Max_Logs);
   begin
      Log_Count := Max_Copy;

      for I in 0 .. Max_Copy - 1 loop
         To_Log_Entry (Buffer.Events (Event_Index (I)), Logs (Log_Index (I)));
      end loop;

      --  Initialize remaining entries
      for I in Max_Copy .. Max_Logs - 1 loop
         Logs (Log_Index (I)).Contract := (others => 0);
         Logs (Log_Index (I)).Topic_Hash := (others => 0);
         Logs (Log_Index (I)).Data := (others => 0);
         Logs (Log_Index (I)).Data_Size := 0;
      end loop;
   end To_Log_Array;

end Contract_Events;
