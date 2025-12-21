--  SPHINX RV32 Storage: Implementation

pragma SPARK_Mode (On);

package body Sphinx_RV32_Storage with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper: Compare two storage keys
   ---------------------------------------------------------------------------

   function Keys_Equal (A, B : Storage_Key_Bytes) return Boolean is
   begin
      for I in A'Range loop
         if A (I) /= B (I) then
            return False;
         end if;
         pragma Loop_Invariant (for all J in A'First .. I => A (J) = B (J));
      end loop;
      return True;
   end Keys_Equal;

   ---------------------------------------------------------------------------
   --  Helper: Find key index in storage
   ---------------------------------------------------------------------------

   procedure Find_Key (
      S     : in     Storage_State;
      Key   : in     Storage_Key_Bytes;
      Index : out    Storage_Slot_Index;
      Found : out    Boolean
   ) with
      Pre  => Storage_Valid (S),
      Post => (if Found then Index < S.Count)
   is
   begin
      Found := False;
      Index := 0;

      if S.Count = 0 then
         return;
      end if;

      for I in 0 .. S.Count - 1 loop
         if S.Entries (I).Written and then Keys_Equal (S.Entries (I).Key, Key) then
            Index := I;
            Found := True;
            return;
         end if;
         pragma Loop_Invariant (not Found);
         pragma Loop_Invariant (I < S.Count);
      end loop;
   end Find_Key;

   ---------------------------------------------------------------------------
   --  SLoad: Load value from storage
   ---------------------------------------------------------------------------

   procedure SLoad (
      S       : in     Storage_State;
      Key     : in     Storage_Key_Bytes;
      Value   : out    Storage_Value_Bytes;
      Found   : out    Boolean
   )
   is
      Index : Storage_Slot_Index;
   begin
      Find_Key (S, Key, Index, Found);

      if Found then
         Value := S.Entries (Index).Value;
      else
         Value := Zero_Value;
      end if;
   end SLoad;

   ---------------------------------------------------------------------------
   --  SStore: Store value to storage
   ---------------------------------------------------------------------------

   procedure SStore (
      S       : in Out Storage_State;
      Key     : in     Storage_Key_Bytes;
      Value   : in     Storage_Value_Bytes;
      Success : out    Boolean
   )
   is
      Index : Storage_Slot_Index;
      Found : Boolean;
   begin
      Find_Key (S, Key, Index, Found);

      if Found then
         --  Update existing entry
         S.Entries (Index).Value := Value;
         Success := True;
      elsif S.Count < Max_Storage_Slots then
         --  Add new entry
         S.Entries (S.Count) := (
            Key     => Key,
            Value   => Value,
            Written => True
         );
         S.Count := S.Count + 1;
         Success := True;
      else
         --  Storage is full
         Success := False;
      end if;
   end SStore;

   ---------------------------------------------------------------------------
   --  Contains: Check if key exists
   ---------------------------------------------------------------------------

   function Contains (
      S   : Storage_State;
      Key : Storage_Key_Bytes
   ) return Boolean
   is
      Index : Storage_Slot_Index;
      Found : Boolean;
   begin
      Find_Key (S, Key, Index, Found);
      return Found;
   end Contains;

end Sphinx_RV32_Storage;
