pragma SPARK_Mode (On);

with System;

--  Anubis_Secure_Wipe: Implementation
--
--  Uses volatile writes to prevent dead-store elimination.
--  The approach creates a volatile overlay and writes through it,
--  then uses a memory barrier to ensure completion.

package body Anubis_Secure_Wipe with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal Volatile Write (cannot be eliminated)
   ---------------------------------------------------------------------------

   --  Write single byte through volatile access
   procedure Write_Volatile_Byte (
      Addr  : System.Address;
      Value : Byte
   ) with
      SPARK_Mode => Off,
      Inline => False,
      Global => null
   is
      Target : Volatile_Byte with
         Import => True,
         Convention => Ada,
         Address => Addr;
   begin
      Target := Volatile_Byte (Value);
   end Write_Volatile_Byte;

   ---------------------------------------------------------------------------
   --  Secure Wipe Implementation
   ---------------------------------------------------------------------------

   procedure Secure_Wipe (Data : out Byte_Array) is
   begin
      --  Write zeros through volatile access
      for I in Data'Range loop
         Write_Volatile_Byte (Data (I)'Address, 0);
         pragma Loop_Invariant (for all J in Data'First .. I => Data (J) = 0);
      end loop;

      --  Memory barrier to ensure writes complete
      Memory_Barrier;
   end Secure_Wipe;

   ---------------------------------------------------------------------------
   --  Fixed-Size Secure Wipe (32 bytes)
   ---------------------------------------------------------------------------

   procedure Secure_Wipe_32 (Data : out Byte_Array) is
   begin
      for I in Data'Range loop
         Write_Volatile_Byte (Data (I)'Address, 0);
         pragma Loop_Invariant (for all J in Data'First .. I => Data (J) = 0);
      end loop;

      Memory_Barrier;
   end Secure_Wipe_32;

   ---------------------------------------------------------------------------
   --  Fixed-Size Secure Wipe (64 bytes)
   ---------------------------------------------------------------------------

   procedure Secure_Wipe_64 (Data : out Byte_Array) is
   begin
      for I in Data'Range loop
         Write_Volatile_Byte (Data (I)'Address, 0);
         pragma Loop_Invariant (for all J in Data'First .. I => Data (J) = 0);
      end loop;

      Memory_Barrier;
   end Secure_Wipe_64;

   ---------------------------------------------------------------------------
   --  Memory Barrier
   ---------------------------------------------------------------------------

   procedure Memory_Barrier is
      --  Volatile variable forces compiler to complete pending writes
      Barrier : Volatile_Byte := 0 with Volatile;
      pragma Unreferenced (Barrier);
   begin
      --  Reading/writing a volatile variable acts as a compiler barrier
      Barrier := 0;
   end Memory_Barrier;

   ---------------------------------------------------------------------------
   --  Verification Helper
   ---------------------------------------------------------------------------

   function All_Zero (Data : Byte_Array) return Boolean is
   begin
      for I in Data'Range loop
         if Data (I) /= 0 then
            return False;
         end if;
         pragma Loop_Invariant (for all J in Data'First .. I => Data (J) = 0);
      end loop;
      return True;
   end All_Zero;

end Anubis_Secure_Wipe;
