--  SPHINX Lite Types Implementation

pragma SPARK_Mode (On);

package body Sphinx_Lite_Types with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Count_Signatures
   ---------------------------------------------------------------------------

   function Count_Signatures (Bitmap : Unsigned_32) return Natural is
      Count : Natural := 0;
      Temp  : Unsigned_32 := Bitmap;
   begin
      --  Population count (Hamming weight)
      --  Only count first 21 bits
      for I in 0 .. Committee_Size - 1 loop
         if (Temp and 1) = 1 then
            if Count < Committee_Size then
               Count := Count + 1;
            end if;
         end if;
         Temp := Shift_Right (Temp, 1);

         pragma Loop_Invariant (Count <= I + 1);
         pragma Loop_Invariant (Count <= Committee_Size);
      end loop;

      return Count;
   end Count_Signatures;

   ---------------------------------------------------------------------------
   --  Has_Signed
   ---------------------------------------------------------------------------

   function Has_Signed (
      Bitmap : Unsigned_32;
      Index  : Committee_Index
   ) return Boolean is
      Mask : constant Unsigned_32 := Shift_Left (1, Index);
   begin
      return (Bitmap and Mask) /= 0;
   end Has_Signed;

   ---------------------------------------------------------------------------
   --  Set_Signature
   ---------------------------------------------------------------------------

   function Set_Signature (
      Bitmap : Unsigned_32;
      Index  : Committee_Index
   ) return Unsigned_32 is
      Mask : constant Unsigned_32 := Shift_Left (1, Index);
   begin
      return Bitmap or Mask;
   end Set_Signature;

   ---------------------------------------------------------------------------
   --  Threshold_Reached
   ---------------------------------------------------------------------------

   function Threshold_Reached (Sig_Count : Signature_Count) return Boolean is
   begin
      return Sig_Count >= Threshold;
   end Threshold_Reached;

end Sphinx_Lite_Types;
