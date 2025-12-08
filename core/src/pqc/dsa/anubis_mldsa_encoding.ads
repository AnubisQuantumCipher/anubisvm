pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;

--  Anubis_MLDSA_Encoding: Byte encoding/decoding for ML-DSA
--
--  Provides packing/unpacking of polynomials, vectors, keys, and signatures

package Anubis_MLDSA_Encoding with
   SPARK_Mode => On
is

   --  Pack polynomial with d-bit coefficients (for t1, d=10)
   procedure Pack_T1 (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre => Output'Length = Poly_T1_Packed_Bytes and Output'First = 0;

   --  Unpack polynomial with 10-bit coefficients
   procedure Unpack_T1 (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) with
      Global => null,
      Pre => Input'Length = Poly_T1_Packed_Bytes and Input'First = 0;

   --  Pack polynomial with 13-bit coefficients (for t0)
   procedure Pack_T0 (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre => Output'Length = Poly_T0_Packed_Bytes and Output'First = 0;

   --  Unpack polynomial with 13-bit coefficients
   procedure Unpack_T0 (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) with
      Global => null,
      Pre => Input'Length = Poly_T0_Packed_Bytes and Input'First = 0;

   --  Pack polynomial with η-bit coefficients (η=2, 3 bits)
   procedure Pack_Eta (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre => Output'Length = Poly_Eta_Packed_Bytes and Output'First = 0;

   --  Unpack polynomial with η-bit coefficients
   procedure Unpack_Eta (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) with
      Global => null,
      Pre => Input'Length = Poly_Eta_Packed_Bytes and Input'First = 0;

   --  Pack polynomial z (20-bit coefficients for γ1 = 2^19)
   procedure Pack_Z (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre => Output'Length = Poly_Z_Packed_Bytes and Output'First = 0;

   --  Unpack polynomial z
   procedure Unpack_Z (
      Input : in  Byte_Array;
      Poly  : out Polynomial
   ) with
      Global => null,
      Pre => Input'Length = Poly_Z_Packed_Bytes and Input'First = 0;

   --  Pack w1 (4-bit coefficients)
   procedure Pack_W1 (
      Poly   : in  Polynomial;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre => Output'Length = Poly_W1_Packed_Bytes and Output'First = 0;

   --  Pack public key: ρ ‖ t1
   procedure Pack_Public_Key (
      Rho : in  Seed;
      T1  : in  Poly_Vector_K;
      PK  : out Public_Key
   ) with
      Global => null;

   --  Unpack public key
   procedure Unpack_Public_Key (
      PK  : in  Public_Key;
      Rho : out Seed;
      T1  : out Poly_Vector_K
   ) with
      Global => null;

   --  Pack secret key: ρ ‖ K ‖ tr ‖ s1 ‖ s2 ‖ t0
   procedure Pack_Secret_Key (
      Rho   : in  Seed;
      K     : in  Seed;
      Tr    : in  Tr_Hash;
      S1    : in  Poly_Vector_L;
      S2    : in  Poly_Vector_K;
      T0    : in  Poly_Vector_K;
      SK    : out Secret_Key
   ) with
      Global => null;

   --  Unpack secret key
   procedure Unpack_Secret_Key (
      SK  : in  Secret_Key;
      Rho : out Seed;
      K   : out Seed;
      Tr  : out Tr_Hash;
      S1  : out Poly_Vector_L;
      S2  : out Poly_Vector_K;
      T0  : out Poly_Vector_K
   ) with
      Global => null;

   --  Pack signature: c_tilde ‖ z ‖ h
   procedure Pack_Signature (
      C_Tilde : in  Challenge_Seed;
      Z       : in  Poly_Vector_L;
      H       : in  Poly_Vector_K;
      Sig     : out Signature
   ) with
      Global => null;

   --  Unpack signature
   procedure Unpack_Signature (
      Sig     : in  Signature;
      C_Tilde : out Challenge_Seed;
      Z       : out Poly_Vector_L;
      H       : out Poly_Vector_K;
      Valid   : out Boolean
   ) with
      Global => null;

end Anubis_MLDSA_Encoding;
