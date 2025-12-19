pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

--  Anubis_Secure_Wipe: Secure Memory Zeroization
--
--  This package provides secure zeroization primitives that are
--  resistant to dead-store elimination by compilers.
--
--  Key features:
--  1. Volatile_Full_Access prevents optimization of writes
--  2. Memory barriers ensure writes complete before return
--  3. Postconditions prove all bytes are zero
--
--  Use these procedures instead of direct loop zeroing for
--  any security-sensitive data (keys, secrets, etc.)

package Anubis_Secure_Wipe with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Volatile Byte Type for Secure Writes
   ---------------------------------------------------------------------------

   --  Volatile byte type (fresh modular type, not derived)
   type Volatile_Byte is mod 2**8 with
      Size => 8,
      Volatile_Full_Access => True;

   --  Volatile byte array for secure wiping
   type Volatile_Byte_Array is array (Natural range <>) of Volatile_Byte;

   ---------------------------------------------------------------------------
   --  Secure Wipe Procedures
   ---------------------------------------------------------------------------

   --  Securely wipe a byte array
   --
   --  This procedure zeros all bytes in the array using volatile writes
   --  that cannot be eliminated by the compiler.
   --
   --  Data  : Array to wipe
   --
   --  Post: All bytes in Data are zero
   procedure Secure_Wipe (Data : out Byte_Array) with
      Global => null,
      Always_Terminates => True,
      Post => (for all I in Data'Range => Data (I) = 0);

   --  Securely wipe a fixed-size 32-byte buffer (keys, hashes)
   procedure Secure_Wipe_32 (Data : out Byte_Array) with
      Global => null,
      Always_Terminates => True,
      Pre => Data'Length = 32,
      Post => (for all I in Data'Range => Data (I) = 0);

   --  Securely wipe a fixed-size 64-byte buffer (seeds, extended hashes)
   procedure Secure_Wipe_64 (Data : out Byte_Array) with
      Global => null,
      Always_Terminates => True,
      Pre => Data'Length = 64,
      Post => (for all I in Data'Range => Data (I) = 0);

   ---------------------------------------------------------------------------
   --  Memory Barrier
   ---------------------------------------------------------------------------

   --  Compiler memory barrier to ensure writes are not reordered
   --
   --  This procedure acts as an optimization barrier, preventing
   --  the compiler from moving writes across this point.
   procedure Memory_Barrier with
      Global => null,
      Always_Terminates => True,
      Inline => True;

   ---------------------------------------------------------------------------
   --  Verification Helper
   ---------------------------------------------------------------------------

   --  Check if all bytes in array are zero (for assertions/tests)
   function All_Zero (Data : Byte_Array) return Boolean with
      Global => null,
      Post => All_Zero'Result = (for all I in Data'Range => Data (I) = 0);

end Anubis_Secure_Wipe;
