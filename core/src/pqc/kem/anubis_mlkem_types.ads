pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Config; use Anubis_Config;
with Anubis_Field; use Anubis_Field;
with Anubis_Types; use Anubis_Types;

--  Anubis_MLKEM_Types: Type definitions for ML-KEM-1024
--
--  Defines polynomial vectors, key structures, and array types
--  for the ML-KEM-1024 (NIST FIPS 203) implementation.
--
--  ML-KEM-1024 Parameters:
--  - k = 4 (dimension of module)
--  - n = 256 (polynomial degree)
--  - q = 3329 (field modulus)
--  - eta1 = 2 (CBD parameter for secret)
--  - eta2 = 2 (CBD parameter for error)
--  - du = 11 (ciphertext compression for u)
--  - dv = 5 (ciphertext compression for v)
--
--  References:
--  - NIST FIPS 203 (ML-KEM)
--  - https://csrc.nist.gov/pubs/fips/203/final

package Anubis_MLKEM_Types with
   SPARK_Mode => On,
   Pure
is

   --  ML-KEM-1024 specific parameters
   K : constant := MLKEM_K;         -- 4
   Eta1 : constant := MLKEM_Eta1;   -- 2
   Eta2 : constant := MLKEM_Eta2;   -- 2
   Du : constant := MLKEM_Du;       -- 11
   Dv : constant := MLKEM_Dv;       -- 5

   --  Polynomial: array of N field elements
   subtype Poly_Index is Natural range 0 .. N - 1;
   type Polynomial is array (Poly_Index) of Field_Element;

   --  Zero polynomial
   Zero_Poly : constant Polynomial := (others => 0);

   --  Polynomial vector: k polynomials
   subtype Vec_Index is Natural range 0 .. K - 1;
   type Poly_Vector is array (Vec_Index) of Polynomial;

   --  Polynomial matrix: k x k polynomials
   type Poly_Matrix is array (Vec_Index, Vec_Index) of Polynomial;

   --  Key sizes (bytes) per FIPS 203
   Encaps_Key_Bytes : constant := MLKEM_Public_Key_Bytes;   -- 1568
   Decaps_Key_Bytes : constant := MLKEM_Secret_Key_Bytes;   -- 3168
   Ciphertext_Bytes : constant := MLKEM_Ciphertext_Bytes;   -- 1568
   Shared_Secret_Bytes : constant := MLKEM_Shared_Secret_Bytes; -- 32

   --  Internal sizes
   Encoded_Poly_Bytes : constant := 12 * N / 8;  -- 384 bytes (12 bits per coeff)
   Encoded_Vector_Bytes : constant := K * Encoded_Poly_Bytes;  -- 1536 bytes

   --  Compressed sizes
   Compressed_Du_Bytes : constant := Du * N / 8;  -- 352 bytes (11 bits per coeff)
   Compressed_Dv_Bytes : constant := Dv * N / 8;  -- 160 bytes (5 bits per coeff)
   Compressed_U_Bytes : constant := K * Compressed_Du_Bytes;  -- 1408 bytes

   --  Seed sizes
   Seed_Bytes : constant := 32;
   subtype Seed is Byte_Array (0 .. Seed_Bytes - 1);

   --  Shared secret type
   subtype Shared_Secret is Byte_Array (0 .. Shared_Secret_Bytes - 1);

   --  Key types
   subtype Encapsulation_Key is Byte_Array (0 .. Encaps_Key_Bytes - 1);
   subtype Decapsulation_Key is Byte_Array (0 .. Decaps_Key_Bytes - 1);
   subtype MLKEM_Ciphertext is Byte_Array (0 .. Ciphertext_Bytes - 1);

   --  Message type (32 bytes = 256 bits)
   subtype Message is Byte_Array (0 .. 31);

   --  Randomness type for encapsulation
   subtype Encaps_Randomness is Byte_Array (0 .. 31);

   --  Internal encoding buffer types
   subtype Encoded_Poly is Byte_Array (0 .. Encoded_Poly_Bytes - 1);
   subtype Encoded_Vector is Byte_Array (0 .. Encoded_Vector_Bytes - 1);

   --  Compressed buffer types
   subtype Compressed_Poly_Du is Byte_Array (0 .. Compressed_Du_Bytes - 1);
   subtype Compressed_Poly_Dv is Byte_Array (0 .. Compressed_Dv_Bytes - 1);
   subtype Compressed_U is Byte_Array (0 .. Compressed_U_Bytes - 1);

end Anubis_MLKEM_Types;
