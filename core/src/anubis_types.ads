pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package Anubis_Types with
   Pure,
   SPARK_Mode => On
is
   --  Fundamental types for AnubisVM
   --  All crypto operations use these base types

   --  Byte type and arrays
   subtype Byte is Unsigned_8;
   type Byte_Array is array (Natural range <>) of Byte;

   --  Word types
   type Word32 is mod 2**32 with Size => 32;
   type Word64 is mod 2**64 with Size => 64;

   --  256-bit word for VM operations
   type Word256 is array (0 .. 3) of Word64;

   --  Address type (32 bytes)
   subtype Address is Byte_Array (0 .. 31);

   --  Hash type (32 bytes for SHA3-256)
   subtype Hash256 is Byte_Array (0 .. 31);

   --  Zero constants
   Zero_Byte : constant Byte := 0;
   Zero_Word32 : constant Word32 := 0;
   Zero_Word64 : constant Word64 := 0;
   Zero_Hash : constant Hash256 := (others => 0);

   --  Maximum sizes for bounded structures
   Max_Bytecode_Size : constant := 24 * 1024;  -- 24KB max contract size
   Max_Storage_Value_Size : constant := 1024;   -- 1KB max storage value
   Max_Memory_Size : constant := 1024 * 1024;   -- 1MB max runtime memory

end Anubis_Types;
