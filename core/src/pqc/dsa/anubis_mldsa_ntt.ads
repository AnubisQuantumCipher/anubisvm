pragma SPARK_Mode (On);

with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

--  Anubis_MLDSA_NTT: Number Theoretic Transform for ML-DSA
--
--  Implements the NTT over Z_q[X]/(X^256 + 1) where q = 8380417
--
--  The primitive 512th root of unity is ζ = 1753
--  NTT twiddles are powers of ζ in bit-reversed order, in Montgomery form
--
--  References:
--  - NIST FIPS 204 (ML-DSA)
--  - CRYSTALS-Dilithium specification: https://pq-crystals.org/dilithium/
--  - Reference implementation: https://github.com/pq-crystals/dilithium

package Anubis_MLDSA_NTT with
   SPARK_Mode => On
is

   --  Pre-computed twiddle factors for NTT
   --  Zetas[i] = ζ^(bitrev(i)) mod Q in Montgomery form
   type Twiddle_Array is array (0 .. 255) of Field_Element;

   Zetas : constant Twiddle_Array;

   --  Forward NTT: polynomial → NTT domain
   procedure NTT (F : in out Polynomial) with
      Global => null;

   --  Inverse NTT: NTT domain → polynomial
   procedure INTT (F : in out Polynomial) with
      Global => null;

   --  Point-wise multiplication in NTT domain
   procedure NTT_Mul (
      F : in Polynomial;
      G : in Polynomial;
      H : out Polynomial
   ) with
      Global => null;

   --  Vector operations (for l-length vectors)
   procedure Vec_NTT_L (V : in out Poly_Vector_L) with
      Global => null;

   procedure Vec_INTT_L (V : in out Poly_Vector_L) with
      Global => null;

   --  Vector operations (for k-length vectors)
   procedure Vec_NTT_K (V : in out Poly_Vector_K) with
      Global => null;

   procedure Vec_INTT_K (V : in out Poly_Vector_K) with
      Global => null;

   --  Matrix operations
   procedure Matrix_NTT (M : in out Poly_Matrix) with
      Global => null;

   --  Matrix-vector multiplication: result = A * v (in NTT domain)
   procedure Matrix_Vec_Mul (
      A      : in  Poly_Matrix;
      V      : in  Poly_Vector_L;
      Result : out Poly_Vector_K
   ) with
      Global => null;

   --  Dot product of vectors in NTT domain
   procedure Vec_Dot_Product_L (
      V1, V2 : in  Poly_Vector_L;
      Result : out Polynomial
   ) with
      Global => null;

   procedure Vec_Dot_Product_K (
      V1, V2 : in  Poly_Vector_K;
      Result : out Polynomial
   ) with
      Global => null;

private

   --  Dilithium NTT twiddle factors in Montgomery form
   --  Converted from signed ref impl values: negative → Q + value
   --  Source: https://github.com/pq-crystals/dilithium (CC0/Apache 2.0)
   Zetas : constant Twiddle_Array := (
      --  Row 0 (indices 0-7)
            0,   25847, 5771523, 7861508,  237124, 7602457, 7504169,  466468,
      --  Row 1 (indices 8-15)
      1826347, 2353451, 8021166, 6288512, 3119733, 5495562, 3111497, 2680103,
      --  Row 2 (indices 16-23)
      2725464, 1024112, 7300517, 3585928, 7830929, 7260833, 2619752, 6271868,
      --  Row 3 (indices 24-31)
      6262231, 4520680, 6980856, 5102745, 1757237, 8360995, 4010497,  280005,
      --  Row 4 (indices 32-39)
      2706023,   95776, 3077325, 3530437, 6718724, 4788269, 5842901, 3915439,
      --  Row 5 (indices 40-47)
      4519302, 5336701, 3574422, 5512770, 3539968, 8079950, 2348700, 7841118,
      --  Row 6 (indices 48-55)
      6681150, 6736599, 3505694, 4558682, 3507263, 6239768, 6779997, 3699596,
      --  Row 7 (indices 56-63)
       811944,  531354,  954230, 3881043, 3900724, 5823537, 2071892, 5582638,
      --  Row 8 (indices 64-71)
      4450022, 6851714, 4702672, 5339162, 6927966, 3475950, 2176455, 6795196,
      --  Row 9 (indices 72-79)
      7122806, 1939314, 4296819, 7380215, 5190273, 5223087, 4747489,  126922,
      --  Row 10 (indices 80-87)
      3412210, 7396998, 2147896, 2715295, 5412772, 4686924, 7969390, 5903370,
      --  Row 11 (indices 88-95)
      7709315, 7151892, 8357436, 7072248, 7998430, 1349076, 1852771, 6949987,
      --  Row 12 (indices 96-103)
      5037034,  264944,  508951, 3097992,   44288, 7280319,  904516, 3958618,
      --  Row 13 (indices 104-111)
      4656075, 8371839, 1653064, 5130689, 2389356, 8169440,  759969, 7063561,
      --  Row 14 (indices 112-119)
       189548, 4827145, 3159746, 6529015, 5971092, 8202977, 1315589, 1341330,
      --  Row 15 (indices 120-127)
      1285669, 6795489, 7567685, 6940675, 5361315, 4499357, 4751448, 3839961,
      --  Row 16 (indices 128-135)
      2091667, 3407706, 2316500, 3817976, 5037939, 2244091, 5933984, 4817955,
      --  Row 17 (indices 136-143)
       266997, 2434439, 7144689, 3513181, 4860065, 4621053, 7183191, 5187039,
      --  Row 18 (indices 144-151)
       900702, 1859098,  909542,  819034,  495491, 6767243, 8337157, 7857917,
      --  Row 19 (indices 152-159)
      7725090, 5257975, 2031748, 3207046, 4823422, 7855319, 7611795, 4784579,
      --  Row 20 (indices 160-167)
       342297,  286988, 5942594, 4108315, 3437287, 5038140, 1735879,  203044,
      --  Row 21 (indices 168-175)
      2842341, 2691481, 5790267, 1265009, 4055324, 1247620, 2486353, 1595974,
      --  Row 22 (indices 176-183)
      4613401, 1250494, 2635921, 4832145, 5386378, 1869119, 1903435, 7329447,
      --  Row 23 (indices 184-191)
      7047359, 1237275, 5062207, 6950192, 7929317, 1312455, 3306115, 6417775,
      --  Row 24 (indices 192-199)
      7100756, 1917081, 5834105, 7005614, 1500165,  777191, 2235880, 3406031,
      --  Row 25 (indices 200-207)
      7838005, 5548557, 6709241, 6533464, 5796124, 4656147,  594136, 4603424,
      --  Row 26 (indices 208-215)
      6366809, 2432395, 2454455, 8215696, 1957272, 3369112,  185531, 7173032,
      --  Row 27 (indices 216-223)
      5196991,  162844, 1616392, 3014001,  810149, 1652634, 4686184, 6581310,
      --  Row 28 (indices 224-231)
      5341501, 3523897, 3866901,  269760, 2213111, 7404533, 1717735,  472078,
      --  Row 29 (indices 232-239)
      7953734, 1723600, 6577327, 1910376, 6712985, 7276084, 8119771, 4546524,
      --  Row 30 (indices 240-247)
      5441381, 6144432, 7959518, 6094090,  183443, 7403526, 1612842, 4834730,
      --  Row 31 (indices 248-255)
      7826001, 3919660, 8332111, 7018208, 3937738, 1400424, 7534263, 1976782
   );

end Anubis_MLDSA_NTT;
