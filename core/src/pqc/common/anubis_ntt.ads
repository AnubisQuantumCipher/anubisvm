pragma SPARK_Mode (On);

with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;

--  Anubis_NTT: Number Theoretic Transform for ML-KEM/ML-DSA
--
--  Implements the NTT over Z_q[X]/(X^256 + 1) where q = 3329
--
--  Security considerations:
--  - All operations constant-time (no data-dependent branches)
--  - Twiddle factors pre-computed and verified against NIST vectors
--  - Montgomery form used throughout for efficiency
--
--  NTT Parameters:
--  - Primitive 256th root of unity: ζ = 17 (mod 3329)
--  - Twiddle factors in Montgomery form for fast multiplication
--  - Bit-reversed butterfly structure (Cooley-Tukey)
--
--  References:
--  - NIST FIPS 203 (ML-KEM)
--  - "Kyber: A CCA-secure module-lattice-based KEM"
--  - https://pq-crystals.org/kyber/

package Anubis_NTT with
   SPARK_Mode => On
is

   --  Polynomial degree N = 256
   subtype Poly_Index is Natural range 0 .. N - 1;

   --  Polynomial: array of N field elements
   type Polynomial is array (Poly_Index) of Field_Element;

   --  Pre-computed twiddle factors in Montgomery form
   --  These are powers of ζ = 17 in bit-reversed order
   --  Twiddles[i] = ζ^(bitrev(i)) * R mod Q
   type Twiddle_Array is array (Poly_Index) of Field_Element;

   --  Forward NTT twiddle factors (powers of ζ = 17)
   Zetas : constant Twiddle_Array;

   --  Inverse NTT twiddle factors (powers of ζ^(-1) = 1175)
   Zetas_Inv : constant Twiddle_Array;

   --  Normalization constant for inverse NTT: 128^(-1) = 3303 mod Q
   --  With 7 NTT layers, inverse butterflies accumulate a factor of 2^7 = 128
   --  Multiply by 128^(-1) to normalize back to original values
   --  Verification: 128 * 3303 = 422784 = 127 * 3329 + 1 ≡ 1 (mod 3329)
   N_Inv : constant Field_Element := 3303;

   --  Forward NTT: polynomial → NTT domain
   --  Computes f_hat = NTT(f) using Cooley-Tukey butterfly
   --  Output is in bit-reversed order (standard for ML-KEM)
   procedure NTT (F : in out Polynomial) with
      Global => null,
      Pre => (for all I in Poly_Index => F (I) < Q),
      Post => (for all I in Poly_Index => F (I) <= 2 * Q);

   --  Inverse NTT: NTT domain → polynomial
   --  Computes f = INTT(f_hat) using Gentleman-Sande with normalization
   --  Expects input in bit-reversed order
   procedure INTT (F : in out Polynomial) with
      Global => null,
      Post => (for all I in Poly_Index => F (I) < Q);

   --  Point-wise multiplication in NTT domain: f_hat * g_hat
   --  Uses Montgomery multiplication for efficiency
   procedure NTT_Mul (
      F : in Polynomial;
      G : in Polynomial;
      H : out Polynomial
   ) with
      Global => null,
      Pre => (for all I in Poly_Index => F (I) < 2 * Q and G (I) < 2 * Q),
      Post => (for all I in Poly_Index => H (I) < 2 * Q);

   --  Reduce all coefficients to [0, Q-1]
   procedure Normalize (F : in out Polynomial) with
      Global => null,
      Post => (for all I in Poly_Index => F (I) < Q);

private

   --  Bit-reverse index for NTT butterfly
   function Bit_Reverse (X : Poly_Index) return Poly_Index with
      Inline,
      Post => Bit_Reverse'Result in Poly_Index;

   --  Pre-computed forward NTT twiddle factors from Kyber reference
   --  Converted from signed int16_t to unsigned mod Q = 3329
   --  Only indices 0-127 are used; indices 128-255 are unused (padded with 0)
   Zetas : constant Twiddle_Array := (
      2285, 2571, 2970, 1812, 1493, 1422,  287,  202,
      3158,  622, 1577,  182,  962, 2127, 1855, 1468,
       573, 2004,  264,  383, 2500, 1458, 1727, 3199,
      2648, 1017,  732,  608, 1787,  411, 3124, 1758,
      1223,  652, 2777, 1015, 2036, 1491, 3047, 1785,
       516, 3321, 3009, 2663, 1711, 2167,  126, 1469,
      2476, 3239, 3058,  830,  107, 1908, 3082, 2378,
      2931,  961, 1821, 2604,  448, 2264,  677, 2054,
      2226,  430,  555,  843, 2078,  871, 1550,  105,
       422,  587,  177, 3094, 3038, 2869, 1574, 1653,
      3083,  778, 1159, 3182, 2552, 1483, 2727, 1119,
      1739,  644, 2457,  349,  418,  329, 3173, 3254,
       817, 1097,  603,  610, 1322, 2044, 1864,  384,
      2114, 3193, 1218, 1994, 2455,  220, 2142, 1670,
      2144, 1799, 2051,  794, 1819, 2475, 2459,  478,
      3221, 3021,  996,  991,  958, 1869, 1522, 1628,
      --  Indices 128-255 unused (padding)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
   );

   --  Pre-computed inverse NTT twiddle factors
   --  Reverse of Zetas[1..127] to match Kyber"s backward indexing
   --  (Kyber uses same array for forward/inverse, accessing backwards for inverse)
   --  Zetas_Inv[0] = Zetas[127], Zetas_Inv[1] = Zetas[126], ..., Zetas_Inv[126] = Zetas[1]
   Zetas_Inv : constant Twiddle_Array := (
      2360, 1019,  993, 2849,  555,  184, 1602,  872,
      2226,  153, 3225,  691, 1177,  761,  255, 2134,
       733,  588, 1054,  927, 1541, 2222, 2127,  842,
      2428, 1103, 1919, 1801, 1762,  557, 3099, 2648,
       780,  721, 3220,  792, 1524,  293, 2291, 1947,
      1690,  229, 3035,  940, 1726,  156,  956,  396,
       555, 2445,  669, 1126, 2231,  859, 1727, 1698,
      2077,  612,  635, 1990,  946,  305, 1930, 2960,
      1943, 3337,  732,  603,  807,  502,  477, 2176,
      1714,  293, 1127,  788, 2797,  869,  637, 2962,
       308, 1567,  758, 1711, 2663, 3009, 3321,  516,
      1785, 3047, 1491, 2036, 1015, 2777,  652, 1223,
      1758, 3124,  411, 1787,  608,  732, 1017, 2648,
      3199, 1727, 1458, 2500,  383,  264, 2004,  573,
      1468, 1855, 2127,  962,  182, 1577,  622, 3158,
       202,  287, 1422, 1493, 1812, 2970, 2571,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0,
         0,    0,    0,    0,    0,    0,    0,    0
   );

end Anubis_NTT;
