-------------------------------------------------------------------------------
--  ANUBIS Poly1305 Message Authentication Code (RFC 8439)
--
--  Pure SPARK implementation following SPARKNaCl platinum-level patterns.
--  Uses 130-bit arithmetic via five 26-bit limbs for proof-friendly math.
--
--  References:
--  - RFC 8439: ChaCha20 and Poly1305 for IETF Protocols
--  - SPARKNaCl by Rod Chapman (platinum-level formal verification)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Poly1305 with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Key_Size : constant := 32;   --  256-bit key (r || s)
   Tag_Size : constant := 16;   --  128-bit authentication tag

   ---------------------------------------------------------------------------
   --  Types (following SPARKNaCl limited-type pattern)
   ---------------------------------------------------------------------------

   subtype Key_Index is Natural range 0 .. Key_Size - 1;
   subtype Tag_Index is Natural range 0 .. Tag_Size - 1;

   type Poly1305_Key is array (Key_Index) of Byte;
   type Poly1305_Tag is array (Tag_Index) of Byte;

   ---------------------------------------------------------------------------
   --  One-shot MAC computation
   ---------------------------------------------------------------------------

   procedure Onetimeauth (
      Tag     :    out Poly1305_Tag;
      Message : in     Byte_Array;
      Key     : in     Poly1305_Key
   ) with
      Global => null,
      Pre    => Message'First = 0,
      Always_Terminates;

   ---------------------------------------------------------------------------
   --  Verification (constant-time)
   ---------------------------------------------------------------------------

   function Onetimeauth_Verify (
      Tag     : Poly1305_Tag;
      Message : Byte_Array;
      Key     : Poly1305_Key
   ) return Boolean with
      Global => null,
      Pre    => Message'First = 0;

   ---------------------------------------------------------------------------
   --  Sanitization
   ---------------------------------------------------------------------------

   procedure Sanitize (Key : out Poly1305_Key) with
      Global => null,
      Post   => (for all I in Key'Range => Key (I) = 0);

   procedure Sanitize_Tag (Tag : out Poly1305_Tag) with
      Global => null,
      Post   => (for all I in Tag'Range => Tag (I) = 0);

end Anubis_Poly1305;
