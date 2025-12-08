pragma SPARK_Mode (On);

with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;
with Anubis_Types; use Anubis_Types;

--  Anubis_CBD: Centered Binomial Distribution Sampling
--
--  Implements constant-time CBD sampling for ML-KEM noise generation.
--  The CBD distribution is used to sample small errors in lattice-based crypto.
--
--  CBD_η(seed): samples from centered binomial distribution with parameter η
--  Output coefficients are in range [0, 2η]
--
--  Security: Constant-time implementation (no data-dependent branches)
--  All bit operations are performed uniformly regardless of input values.
--
--  References:
--  - NIST FIPS 203 (ML-KEM) Section 4.2
--  - "CRYSTALS-Kyber Algorithm Specifications"

package Anubis_CBD with
   SPARK_Mode => On
is

   --  Sample polynomial from CBD_η using input bytes
   --  η = 2: requires 128 bytes (2 * 256 bits)
   --  η = 3: requires 192 bytes (3 * 256 bits)
   subtype CBD_Input_2 is Byte_Array (0 .. 127);  -- For η=2
   subtype CBD_Input_3 is Byte_Array (0 .. 191);  -- For η=3

   type Polynomial is array (0 .. N - 1) of Field_Element;

   --  CBD with η = 2 (ML-KEM-512, ML-KEM-768, ML-KEM-1024)
   --  Samples coefficients from {-2, -1, 0, 1, 2}
   --  Uses 2 bits per coefficient, 64 bytes total for 256 coefficients
   procedure CBD_2 (
      Input : CBD_Input_2;
      Poly  : out Polynomial
   ) with
      Global => null,
      Post => (for all I in 0 .. N - 1 => Poly (I) < Q);

   --  CBD with η = 3 (higher security variants)
   --  Samples coefficients from {-3, -2, -1, 0, 1, 2, 3}
   --  Uses 3 bits per coefficient, 96 bytes total for 256 coefficients
   procedure CBD_3 (
      Input : CBD_Input_3;
      Poly  : out Polynomial
   ) with
      Global => null,
      Post => (for all I in 0 .. N - 1 => Poly (I) < Q);

private

   --  Count number of 1 bits in a byte (population count)
   --  Constant-time implementation using bit manipulation
   function Popcount_8 (X : Byte) return Natural with
      Inline_Always,
      Post => Popcount_8'Result <= 8;

end Anubis_CBD;
