--  KHEPRI Merkle Patricia Trie Types Implementation
pragma SPARK_Mode (On);

package body Khepri_MPT_Types with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Bytes_To_Nibbles
   ---------------------------------------------------------------------------

   function Bytes_To_Nibbles (
      Data   : Byte_Array;
      Length : Natural
   ) return Nibble_Key
   is
      Result : Nibble_Key := Empty_Nibble_Key;
      Idx    : Natural := 0;
   begin
      if Length = 0 then
         return Result;
      end if;

      for I in 0 .. Length - 1 loop
         pragma Loop_Invariant (Idx = I * 2);
         pragma Loop_Invariant (Idx <= Max_Key_Nibbles - 2);
         pragma Loop_Invariant (I < Length);
         pragma Loop_Invariant (Data'First + I <= Data'Last);

         --  High nibble
         Result.Data (Idx) := Nibble (Shift_Right (Data (Data'First + I), 4));
         Idx := Idx + 1;
         --  Low nibble
         Result.Data (Idx) := Nibble (Data (Data'First + I) and 16#0F#);
         Idx := Idx + 1;
      end loop;
      Result.Length := Length * 2;
      return Result;
   end Bytes_To_Nibbles;

   ---------------------------------------------------------------------------
   --  Keys_Equal
   ---------------------------------------------------------------------------

   function Keys_Equal (A, B : Nibble_Key) return Boolean is
   begin
      if A.Length /= B.Length then
         return False;
      end if;

      if A.Length = 0 then
         return True;
      end if;

      for I in 0 .. A.Length - 1 loop
         pragma Loop_Invariant (I < A.Length);
         pragma Loop_Invariant (I < B.Length);
         pragma Loop_Invariant (I <= Max_Key_Nibbles - 1);

         if A.Data (I) /= B.Data (I) then
            return False;
         end if;
      end loop;

      return True;
   end Keys_Equal;

   ---------------------------------------------------------------------------
   --  Common_Prefix_Length
   ---------------------------------------------------------------------------

   function Common_Prefix_Length (A, B : Nibble_Key) return Natural is
      Min_Len : constant Natural := Natural'Min (A.Length, B.Length);
      Prefix  : Natural := 0;
   begin
      if Min_Len = 0 then
         return 0;
      end if;

      for I in 0 .. Min_Len - 1 loop
         pragma Loop_Invariant (I < Min_Len);
         pragma Loop_Invariant (I <= Max_Key_Nibbles - 1);
         pragma Loop_Invariant (Prefix = I);
         pragma Loop_Invariant (Prefix < Max_Key_Nibbles);

         if A.Data (I) = B.Data (I) then
            Prefix := Prefix + 1;
         else
            exit;
         end if;
      end loop;
      return Prefix;
   end Common_Prefix_Length;

   ---------------------------------------------------------------------------
   --  Is_Prefix
   ---------------------------------------------------------------------------

   function Is_Prefix (Prefix, Key : Nibble_Key) return Boolean is
   begin
      if Prefix.Length > Key.Length then
         return False;
      end if;

      if Prefix.Length = 0 then
         return True;
      end if;

      for I in 0 .. Prefix.Length - 1 loop
         pragma Loop_Invariant (I < Prefix.Length);
         pragma Loop_Invariant (I < Key.Length);
         pragma Loop_Invariant (I <= Max_Key_Nibbles - 1);

         if Prefix.Data (I) /= Key.Data (I) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Prefix;

   ---------------------------------------------------------------------------
   --  Remove_Prefix
   ---------------------------------------------------------------------------

   function Remove_Prefix (
      Key    : Nibble_Key;
      Length : Natural
   ) return Nibble_Key
   is
      Result : Nibble_Key := Empty_Nibble_Key;
      New_Len : constant Natural := Key.Length - Length;
   begin
      if New_Len = 0 then
         return Result;
      end if;

      for I in 0 .. New_Len - 1 loop
         pragma Loop_Invariant (I < New_Len);
         pragma Loop_Invariant (I <= Max_Key_Nibbles - 1);
         pragma Loop_Invariant (Length + I < Key.Length);
         pragma Loop_Invariant (Length + I <= Max_Key_Nibbles - 1);

         Result.Data (I) := Key.Data (Length + I);
      end loop;
      Result.Length := New_Len;
      return Result;
   end Remove_Prefix;

   ---------------------------------------------------------------------------
   --  Hash_Equal
   ---------------------------------------------------------------------------

   function Hash_Equal (A, B : Hash_256) return Boolean is
   begin
      for I in Hash_Index loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Hash_Equal;

end Khepri_MPT_Types;
