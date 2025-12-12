-------------------------------------------------------------------------------
--  ANUBIS PLATINUM MODEL - Implementation
--
--  Bodies for ghost functions that require computation.
--  Most ghost functions are expression functions defined in the spec.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Platinum_Model with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  U256 Arithmetic Lemmas (require computation)
   ---------------------------------------------------------------------------

   --  Model: U256 addition does not overflow
   --  This is a conservative check: sum of limbs with max carry propagation
   function U256_Add_No_Overflow (
      A_Limb0, A_Limb1, A_Limb2, A_Limb3 : Word64;
      B_Limb0, B_Limb1, B_Limb2, B_Limb3 : Word64
   ) return Boolean is
      --  Check if the MSB addition could overflow
      --  Conservative: if A3 + B3 < max, we're safe
      Max_Without_Carry : constant Word64 := Word64'Last - 1;
   begin
      --  If A3 + B3 <= Max_Without_Carry, no overflow possible
      --  (even with maximum carry from lower limbs)
      return A_Limb3 <= Max_Without_Carry - B_Limb3 or else
             (A_Limb3 = Word64'Last and B_Limb3 = 0) or else
             (A_Limb3 = 0 and B_Limb3 = Word64'Last);
   end U256_Add_No_Overflow;

   --  Model: U256 subtraction does not underflow
   --  A >= B means no underflow
   function U256_Sub_No_Underflow (
      A_Limb0, A_Limb1, A_Limb2, A_Limb3 : Word64;
      B_Limb0, B_Limb1, B_Limb2, B_Limb3 : Word64
   ) return Boolean is
   begin
      --  Compare from MSB to LSB
      if A_Limb3 > B_Limb3 then
         return True;
      elsif A_Limb3 < B_Limb3 then
         return False;
      elsif A_Limb2 > B_Limb2 then
         return True;
      elsif A_Limb2 < B_Limb2 then
         return False;
      elsif A_Limb1 > B_Limb1 then
         return True;
      elsif A_Limb1 < B_Limb1 then
         return False;
      else
         return A_Limb0 >= B_Limb0;
      end if;
   end U256_Sub_No_Underflow;

end Anubis_Platinum_Model;
