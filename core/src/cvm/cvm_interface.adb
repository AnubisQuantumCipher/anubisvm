pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body CVM_Interface with
   SPARK_Mode => On
is

   --  Find entry point by selector
   procedure Find_Entry_Point (
      Desc     : CVM_Descriptor;
      Selector : Method_Selector;
      Index    : out Natural;
      Found    : out Boolean
   ) is
   begin
      Index := 0;
      Found := False;

      for I in 0 .. Desc.Entry_Count - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Desc.Entry_Count);
         pragma Loop_Invariant (not Found);

         --  Compare selector byte-by-byte
         declare
            Match : Boolean := True;
         begin
            for J in Selector'Range loop
               pragma Loop_Invariant (J >= Selector'First);
               if Selector (J) /= Desc.Entries (I).Selector (J) then
                  Match := False;
               end if;
            end loop;

            if Match then
               Index := I;
               Found := True;
               return;
            end if;
         end;
      end loop;
   end Find_Entry_Point;

   --  Trust Domain Size: First 8 bytes of address define trust domain
   Trust_Domain_Prefix_Size : constant := 8;

   --  Check if two addresses are in the same trust domain
   --  Trust domain is defined by the first 8 bytes of the CVM address
   --  CVMs deployed by the same deployer or in the same security context
   --  share the same trust domain prefix
   function Same_Trust_Domain (
      Addr_A : Caller_Address;
      Addr_B : CVM_Address
   ) return Boolean with
      Global => null
   is
      Match : Boolean := True;
   begin
      --  Compare first 8 bytes (trust domain prefix)
      for I in 0 .. Trust_Domain_Prefix_Size - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Trust_Domain_Prefix_Size);
         if Addr_A (I) /= Addr_B (I) then
            Match := False;
         end if;
      end loop;
      return Match;
   end Same_Trust_Domain;

   --  Check caller authorization
   function Is_Authorized (
      Context : Call_Context;
      EP_Desc : Entry_Point_Desc
   ) return Boolean is
   begin
      --  Public entry points are always callable
      if EP_Desc.Public then
         return True;
      end if;

      --  Internal calls (from other CVMs) can access non-public entries
      --  if caller is in the same trust domain
      if Context.Internal then
         --  Trust domain check: caller and target must share same prefix
         --  This ensures only CVMs from the same deployment context
         --  can call each other's internal methods
         if Same_Trust_Domain (Context.Caller, Context.Target) then
            return True;
         end if;
         --  Different trust domains cannot access internal methods
         return False;
      end if;

      --  External calls to non-public entries are denied
      return False;
   end Is_Authorized;

   --  Create success result
   function Success_Result (
      Data : Byte_Array;
      Len  : Natural
   ) return Exec_Result is
      Result : Exec_Result := Empty_Result;
   begin
      Result.Status := Success;
      Result.Return_Len := Len;

      for I in 0 .. Len - 1 loop
         pragma Loop_Invariant (I >= 0 and I < Len);
         if I <= Result.Return_Data'Last then
            Result.Return_Data (I) := Data (Data'First + I);
         end if;
      end loop;

      return Result;
   end Success_Result;

   --  Pack natural as 4-byte big-endian
   procedure Pack_Natural (
      Value  : Natural;
      Buffer : out Byte_Array;
      Offset : Natural
   ) is
      V : constant Unsigned_32 := Unsigned_32 (Value);
   begin
      Buffer (Buffer'First + Offset)     := Byte (Shift_Right (V, 24) and 16#FF#);
      Buffer (Buffer'First + Offset + 1) := Byte (Shift_Right (V, 16) and 16#FF#);
      Buffer (Buffer'First + Offset + 2) := Byte (Shift_Right (V, 8) and 16#FF#);
      Buffer (Buffer'First + Offset + 3) := Byte (V and 16#FF#);
   end Pack_Natural;

   --  Unpack natural from 4-byte big-endian
   function Unpack_Natural (
      Buffer : Byte_Array;
      Offset : Natural
   ) return Natural is
      B0 : constant Unsigned_32 := Unsigned_32 (Buffer (Buffer'First + Offset));
      B1 : constant Unsigned_32 := Unsigned_32 (Buffer (Buffer'First + Offset + 1));
      B2 : constant Unsigned_32 := Unsigned_32 (Buffer (Buffer'First + Offset + 2));
      B3 : constant Unsigned_32 := Unsigned_32 (Buffer (Buffer'First + Offset + 3));
      Result : constant Unsigned_32 :=
         Shift_Left (B0, 24) or
         Shift_Left (B1, 16) or
         Shift_Left (B2, 8) or
         B3;
   begin
      return Natural (Result);
   end Unpack_Natural;

end CVM_Interface;
