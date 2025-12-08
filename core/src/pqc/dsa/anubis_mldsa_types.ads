pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;

--  Anubis_MLDSA_Types: Type definitions for ML-DSA-87
--
--  Defines polynomials, vectors, and matrices for the signature scheme.
--  ML-DSA-87 uses:
--  - k = 8 rows in matrix A (public key dimension)
--  - l = 7 columns in matrix A (secret key dimension)

package Anubis_MLDSA_Types with
   Pure,
   SPARK_Mode => On
is

   --  Polynomial index type
   subtype Poly_Index is Natural range 0 .. N - 1;

   --  Polynomial: array of N field elements
   type Polynomial is array (Poly_Index) of Field_Element;

   --  Vector index types
   subtype K_Index is Natural range 0 .. K - 1;  --  0..7
   subtype L_Index is Natural range 0 .. L - 1;  --  0..6

   --  Polynomial vectors
   type Poly_Vector_K is array (K_Index) of Polynomial;  --  Length k=8
   type Poly_Vector_L is array (L_Index) of Polynomial;  --  Length l=7

   --  Matrix A: k rows x l columns
   type Poly_Matrix is array (K_Index, L_Index) of Polynomial;

   --  Seed types (32 bytes each)
   subtype Seed is Byte_Array (0 .. Seed_Bytes - 1);

   --  Expanded seeds from keygen
   type Seeds_Record is record
      Rho   : Seed;  --  Public seed for matrix A
      Rho_P : Seed;  --  Seed for signing randomness
      K_Seed : Seed;  --  Key for PRF in signing
   end record;

   --  Public key hash (64 bytes)
   subtype Tr_Hash is Byte_Array (0 .. Tr_Bytes - 1);

   --  Challenge seed (32 bytes)
   subtype Challenge_Seed is Byte_Array (0 .. 31);

   --  Hint vector: one bit per coefficient per polynomial
   --  Packed representation: omega + k bytes
   subtype Hint_Bytes is Byte_Array (0 .. Omega + K - 1);

   --  Key types (packed byte arrays)
   subtype Public_Key is Byte_Array (0 .. Public_Key_Bytes - 1);
   subtype Secret_Key is Byte_Array (0 .. Secret_Key_Bytes - 1);
   subtype Signature is Byte_Array (0 .. Signature_Bytes - 1);

   --  Message type (variable length)
   subtype Message is Byte_Array;

end Anubis_MLDSA_Types;
