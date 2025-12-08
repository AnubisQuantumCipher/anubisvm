pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_U256; use Aegis_U256;

package body Aegis_Contract with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  ABI Encoding/Decoding
   ---------------------------------------------------------------------------

   function Encode_U256 (Value : U256) return Parameter_Slot is
      Result : Parameter_Slot;
      Bytes : constant Hash256 := To_Bytes_BE (Value);
   begin
      for I in 0 .. 31 loop
         Result (I) := Bytes (I);
      end loop;
      return Result;
   end Encode_U256;

   function Decode_U256 (Slot : Parameter_Slot) return U256 is
      Bytes : Hash256;
   begin
      for I in 0 .. 31 loop
         Bytes (I) := Slot (I);
      end loop;
      return From_Bytes_BE (Bytes);
   end Decode_U256;

   function Encode_Address (Addr : Contract_Address) return Parameter_Slot is
   begin
      --  Address is left-padded with zeros (12 bytes) then 20 bytes address
      --  For 32-byte addresses, we just copy directly
      return Result : Parameter_Slot do
         for I in 0 .. 31 loop
            Result (I) := Addr (I);
         end loop;
      end return;
   end Encode_Address;

   function Decode_Address (Slot : Parameter_Slot) return Contract_Address is
      Result : Contract_Address;
   begin
      for I in 0 .. 31 loop
         Result (I) := Slot (I);
      end loop;
      return Result;
   end Decode_Address;

   function Encode_Bool (Value : Boolean) return Parameter_Slot is
      Result : Parameter_Slot := (others => 0);
   begin
      if Value then
         Result (31) := 1;
      end if;
      return Result;
   end Encode_Bool;

   function Decode_Bool (Slot : Parameter_Slot) return Boolean is
   begin
      --  Any non-zero value is true
      for I in 0 .. 31 loop
         if Slot (I) /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end Decode_Bool;

   ---------------------------------------------------------------------------
   --  Function Selector Computation
   ---------------------------------------------------------------------------

   function Compute_Selector (Signature_Hash : Hash256) return Function_Selector is
      Result : Function_Selector;
   begin
      for I in 0 .. 3 loop
         Result (I) := Signature_Hash (I);
      end loop;
      return Result;
   end Compute_Selector;

   function Find_Function (
      Table    : Function_Table;
      Count    : Natural;
      Selector : Function_Selector
   ) return Natural is
   begin
      for I in 0 .. Count - 1 loop
         if Table (Function_Index (I)).Selector = Selector then
            return I;
         end if;
      end loop;
      return Count;  -- Not found
   end Find_Function;

   ---------------------------------------------------------------------------
   --  Contract Validation
   ---------------------------------------------------------------------------

   function Validate_Manifest (M : Contract_Manifest) return Boolean is
   begin
      --  Check basic consistency
      if M.Function_Count > Max_Functions then
         return False;
      end if;

      if M.Event_Count > Max_Events then
         return False;
      end if;

      if M.Name_Length > Max_Name_Length then
         return False;
      end if;

      --  Check code hash is non-zero
      for I in 0 .. 31 loop
         if M.Code_Hash (I) /= 0 then
            return True;
         end if;
      end loop;

      return False;  -- All zeros code hash is invalid
   end Validate_Manifest;

   function Can_Call_Function (
      Func      : Function_Entry;
      Is_Static : Boolean;
      Has_Value : Boolean
   ) return Boolean is
   begin
      --  Check visibility
      if Func.Visibility = Vis_Private or Func.Visibility = Vis_Internal then
         return False;
      end if;

      --  Static calls cannot modify state
      if Is_Static then
         case Func.Mutability is
            when Mut_Pure | Mut_View =>
               null;  -- OK
            when Mut_Nonpayable | Mut_Payable =>
               return False;
         end case;
      end if;

      --  Only payable functions can receive value
      if Has_Value and Func.Mutability /= Mut_Payable then
         return False;
      end if;

      return True;
   end Can_Call_Function;

end Aegis_Contract;
