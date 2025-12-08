pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Word; use Anubis_Word;

package Anubis_Bytes with
   SPARK_Mode => On
is
   --  Constant-Time Byte Array Operations
   --
   --  All operations here are timing-invariant
   --  Critical for comparing MACs, signatures, etc.

   --  Constant-time array equality
   --  Returns True if all bytes equal, False otherwise
   --  Timing independent of position of first difference
   function CT_Equal (A, B : Byte_Array) return Boolean with
      Pre => A'Length = B'Length,
      Post => CT_Equal'Result = (A = B);

   --  Constant-time array comparison (for sorting/ordering)
   --  Returns True if A < B lexicographically
   function CT_Less_Than (A, B : Byte_Array) return Boolean with
      Pre => A'Length = B'Length;

   --  Constant-time conditional copy
   --  If Cond=1, copies Src to Dest; if Cond=0, Dest unchanged
   procedure CT_Copy (Cond : Word; Dest : in out Byte_Array; Src : Byte_Array) with
      Pre => Cond in 0 | 1 and then Dest'Length = Src'Length,
      Post => (if Cond = 1 then Dest = Src else Dest = Dest'Old);

   --  Zero a byte array (for zeroization)
   --  Uses volatile semantics to prevent optimization
   procedure Secure_Zero (Data : out Byte_Array) with
      Post => (for all I in Data'Range => Data (I) = 0);

   --  Constant-time check if array is all zeros
   function CT_Is_Zero (Data : Byte_Array) return Boolean;

   --  XOR two byte arrays
   procedure XOR_Arrays (Result : in out Byte_Array; A, B : Byte_Array) with
      Pre => Result'Length = A'Length and A'Length = B'Length,
      Post => (for all I in Result'Range =>
                 Result (I) = (A (A'First + (I - Result'First)) xor
                               B (B'First + (I - Result'First))));

   --  AND two byte arrays
   procedure AND_Arrays (Result : in out Byte_Array; A, B : Byte_Array) with
      Pre => Result'Length = A'Length and A'Length = B'Length;

end Anubis_Bytes;
