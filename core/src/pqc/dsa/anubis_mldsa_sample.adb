pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3; use Anubis_SHA3;

package body Anubis_MLDSA_Sample with
   SPARK_Mode => On
is

   --  Buffer size for rejection sampling
   XOF_Buffer_Size : constant := 840;  --  ~3 * 256 coeffs with margin

   --  RejNTTPoly: Sample polynomial using rejection sampling
   --  Accepts 23-bit values < Q
   procedure RejNTTPoly (
      Seed_Bytes : in  Byte_Array;
      Poly       : out Polynomial
   ) is
      --  Max valid index after check: XOF_Buffer_Size - 3 = 837
      subtype Buf_Index is Natural range 0 .. XOF_Buffer_Size - 1;
      Buffer : Byte_Array (Buf_Index);
      Coef_Idx : Natural := 0;
      Buf_Idx : Natural := 0;
      Val : Unsigned_32;
   begin
      Poly := (others => 0);

      --  From precondition: Seed_Bytes'Last < Natural'Last
      pragma Assert (Seed_Bytes'Last < Natural'Last);

      --  Generate XOF output
      SHAKE128 (Seed_Bytes, Buffer, XOF_Buffer_Size);

      --  Rejection sampling
      while Coef_Idx < N loop
         pragma Loop_Invariant (Coef_Idx < N);
         pragma Loop_Invariant (Buf_Idx <= XOF_Buffer_Size);

         --  Check if we need at least 3 more bytes
         if Buf_Idx > XOF_Buffer_Size - 3 then
            --  Regenerate buffer if exhausted
            SHAKE128 (Seed_Bytes, Buffer, XOF_Buffer_Size);
            Buf_Idx := 0;
         end if;

         --  At this point, Buf_Idx <= XOF_Buffer_Size - 3 = 837
         pragma Assert (Buf_Idx <= XOF_Buffer_Size - 3);
         pragma Assert (Buf_Idx + 2 <= XOF_Buffer_Size - 1);

         --  Extract 3 bytes, form 24-bit value, mask to 23 bits
         Val := Unsigned_32 (Buffer (Buf_Idx)) or
                Shift_Left (Unsigned_32 (Buffer (Buf_Idx + 1)), 8) or
                Shift_Left (Unsigned_32 (Buffer (Buf_Idx + 2)) and 16#7F#, 16);

         Buf_Idx := Buf_Idx + 3;

         --  Accept if < Q
         if Val < Q then
            Poly (Coef_Idx) := Field_Element (Val);
            Coef_Idx := Coef_Idx + 1;
         end if;
      end loop;
   end RejNTTPoly;

   --  RejBoundedPoly: Sample polynomial with coefficients in [-η, η]
   --  For ML-DSA-87, η = 2
   procedure RejBoundedPoly (
      Seed_Bytes : in  Byte_Array;
      Poly       : out Polynomial
   ) is
      Buffer : Byte_Array (0 .. 255);  --  256 bytes for 256 coefficients
      Coef_Idx : Natural := 0;
      Buf_Idx : Natural := 0;
      B : Byte;
      T0, T1 : Integer;
   begin
      Poly := (others => 0);

      --  From precondition: Seed_Bytes'Last < Natural'Last
      pragma Assert (Seed_Bytes'Last < Natural'Last);

      --  Generate SHAKE256 output
      SHAKE256 (Seed_Bytes, Buffer, 256);

      --  Sample coefficients using rejection
      while Coef_Idx < N loop
         if Buf_Idx >= 256 then
            SHAKE256 (Seed_Bytes, Buffer, 256);
            Buf_Idx := 0;
         end if;

         B := Buffer (Buf_Idx);
         Buf_Idx := Buf_Idx + 1;

         --  Each byte gives two coefficient candidates (4 bits each)
         --  For η=2: valid range is 0..4, representing -2..2
         T0 := Integer (B and 16#0F#);
         T1 := Integer (Shift_Right (B, 4));

         if T0 < 5 and Coef_Idx < N then
            --  Map 0,1,2,3,4 to 2,1,0,-1,-2 which is η - t
            if Eta - T0 >= 0 then
               Poly (Coef_Idx) := Field_Element (Eta - T0);
            else
               Poly (Coef_Idx) := Field_Element (Q + (Eta - T0));
            end if;
            Coef_Idx := Coef_Idx + 1;
         end if;

         if T1 < 5 and Coef_Idx < N then
            if Eta - T1 >= 0 then
               Poly (Coef_Idx) := Field_Element (Eta - T1);
            else
               Poly (Coef_Idx) := Field_Element (Q + (Eta - T1));
            end if;
            Coef_Idx := Coef_Idx + 1;
         end if;
      end loop;
   end RejBoundedPoly;

   --  ExpandA: Generate matrix A from seed ρ
   procedure ExpandA (
      Rho    : in  Seed;
      A_Hat  : out Poly_Matrix
   ) is
      XOF_Seed : Byte_Array (0 .. 33) := (others => 0);  --  Initialize fully
   begin
      --  Copy ρ to XOF seed
      for I in 0 .. 31 loop
         XOF_Seed (I) := Rho (I);
      end loop;

      --  Generate each matrix element
      for I in K_Index loop
         for J in L_Index loop
            --  XOF input: ρ ‖ j ‖ i (little endian indices)
            XOF_Seed (32) := Byte (J);
            XOF_Seed (33) := Byte (I);

            RejNTTPoly (XOF_Seed, A_Hat (I, J));
         end loop;
      end loop;
   end ExpandA;

   --  ExpandS: Generate secret vectors s1, s2
   procedure ExpandS (
      Rho_Prime : in  Seed;
      S1        : out Poly_Vector_L;
      S2        : out Poly_Vector_K
   ) is
      XOF_Seed : Byte_Array (0 .. 65) := (others => 0);  --  Initialize fully
      Counter : Natural := 0;
   begin
      --  Copy ρ" to XOF seed (using full 32 bytes)
      for I in 0 .. 31 loop
         XOF_Seed (I) := Rho_Prime (I);
      end loop;

      --  Generate s1 (L polynomials)
      for I in L_Index loop
         XOF_Seed (64) := Byte (Counter mod 256);
         XOF_Seed (65) := Byte (Counter / 256);
         Counter := Counter + 1;

         RejBoundedPoly (XOF_Seed (0 .. 65), S1 (I));
      end loop;

      --  Generate s2 (K polynomials)
      for I in K_Index loop
         XOF_Seed (64) := Byte (Counter mod 256);
         XOF_Seed (65) := Byte (Counter / 256);
         Counter := Counter + 1;

         RejBoundedPoly (XOF_Seed (0 .. 65), S2 (I));
      end loop;
   end ExpandS;

   --  ExpandMask: Generate masking vector y
   --  Each coefficient is in [-γ1+1, γ1] represented mod Q
   procedure ExpandMask (
      Rho_Prime : in  Seed;
      Kappa     : in  Natural;
      Y         : out Poly_Vector_L
   ) is
      XOF_Seed : Byte_Array (0 .. 65) := (others => 0);  --  Initialize fully
      Buffer : Byte_Array (0 .. 639);  --  5 bytes * 256 / 2 = 640 bytes
      Buf_Idx : Natural;
      Val : Unsigned_32;
      Centered : Integer;
      Two_Gamma1 : constant := 2 * Gamma1;  --  2^20
   begin
      --  Initialize Y to avoid "might not be initialized" warning
      Y := (others => (others => 0));

      --  Copy ρ" to seed
      for I in 0 .. 31 loop
         XOF_Seed (I) := Rho_Prime (I);
      end loop;

      for I in L_Index loop
         --  Counter = κ + i (per FIPS 204)
         --  Encode as 2 bytes (little endian), masking to ensure Byte range
         declare
            Counter : constant Natural := Kappa + I;
         begin
            XOF_Seed (64) := Byte (Counter mod 256);
            XOF_Seed (65) := Byte ((Counter / 256) mod 256);  --  Mask to Byte range
         end;

         --  Generate 640 bytes for 256 coefficients (20 bits each)
         SHAKE256 (XOF_Seed (0 .. 65), Buffer, 640);

         Buf_Idx := 0;
         for J in Poly_Index loop
            --  Loop invariant: Buf_Idx advances by 5 for every 2 coefficients
            --  After J coefficients, Buf_Idx = (J / 2) * 5
            --  Maximum is (255 / 2) * 5 = 127 * 5 = 635
            pragma Loop_Invariant (Buf_Idx = (J / 2) * 5);
            pragma Loop_Invariant (Buf_Idx <= 635);

            --  Extract 20-bit value from 5 bytes (2 coefficients per 5 bytes)
            if J mod 2 = 0 then
               --  At even J, we read from Buf_Idx, Buf_Idx+1, Buf_Idx+2
               --  Buf_Idx + 2 <= 635 + 2 = 637 < 640
               Val := Unsigned_32 (Buffer (Buf_Idx)) or
                      Shift_Left (Unsigned_32 (Buffer (Buf_Idx + 1)), 8) or
                      Shift_Left (Unsigned_32 (Buffer (Buf_Idx + 2)) and 16#0F#, 16);
            else
               --  At odd J, we read from Buf_Idx+2, Buf_Idx+3, Buf_Idx+4
               --  Buf_Idx + 4 <= 635 + 4 = 639 < 640
               Val := Shift_Right (Unsigned_32 (Buffer (Buf_Idx + 2)), 4) or
                      Shift_Left (Unsigned_32 (Buffer (Buf_Idx + 3)), 4) or
                      Shift_Left (Unsigned_32 (Buffer (Buf_Idx + 4)), 12);
               Val := Val and 16#FFFFF#;  --  Mask to 20 bits
               Buf_Idx := Buf_Idx + 5;
            end if;

            --  Convert to centered: γ1 - val gives range [γ1-2^20+1, γ1]
            --  which is [-γ1+1, γ1] for γ1 = 2^19
            Centered := Integer (Gamma1) - Integer (Val and (Two_Gamma1 - 1));

            if Centered >= 0 then
               Y (I) (J) := Field_Element (Centered);
            else
               Y (I) (J) := Field_Element (Integer (Q) + Centered);
            end if;
         end loop;
      end loop;
   end ExpandMask;

   --  SampleInBall: Generate challenge polynomial c
   --  c has exactly τ non-zero coefficients, each ±1
   --  Uses Fisher-Yates style sampling with SHAKE256 XOF
   procedure SampleInBall (
      Seed_Bytes : in  Byte_Array;
      C          : out Polynomial
   ) is
      --  FIPS 204 specifies 272 bytes, but we use more for rejection safety
      Buffer : Byte_Array (0 .. 399);
      Signs : Unsigned_64;
      Buf_Idx : Natural := 8;  --  Start after sign bits
      J : Natural := 0;  --  Initialize to avoid garbage
      B : Byte;
   begin
      --  Initialize c to zero
      C := (others => 0);

      --  From precondition: Seed_Bytes'Last < Natural'Last
      pragma Assert (Seed_Bytes'Last < Natural'Last);

      --  Generate SHAKE256 output (single continuous stream)
      SHAKE256 (Seed_Bytes, Buffer, 400);

      --  Extract sign bits (first 8 bytes = 64 bits, we use τ = 60)
      Signs := Unsigned_64 (Buffer (0)) or
               Shift_Left (Unsigned_64 (Buffer (1)), 8) or
               Shift_Left (Unsigned_64 (Buffer (2)), 16) or
               Shift_Left (Unsigned_64 (Buffer (3)), 24) or
               Shift_Left (Unsigned_64 (Buffer (4)), 32) or
               Shift_Left (Unsigned_64 (Buffer (5)), 40) or
               Shift_Left (Unsigned_64 (Buffer (6)), 48) or
               Shift_Left (Unsigned_64 (Buffer (7)), 56);

      --  Fisher-Yates style sampling for τ positions
      --  FIPS 204: For i from 256-τ to 255 (increasing order!)
      for I in N - Tau .. N - 1 loop
         --  Sample J uniformly from [0, I] using rejection sampling
         loop
            --  Safety check - should never run out with 400 byte buffer
            if Buf_Idx >= 400 then
               --  Emergency: use I as fallback (shouldn"t happen normally)
               J := I;
               exit;
            end if;

            B := Buffer (Buf_Idx);
            Buf_Idx := Buf_Idx + 1;

            J := Natural (B);
            exit when J <= I;
         end loop;

         --  c[I] ← c[J] (copy value at J to I)
         C (I) := C (J);

         --  c[J] ← (−1)^h (set J to ±1 based on sign bit)
         if (Signs and 1) = 1 then
            C (J) := Q - 1;  --  -1 mod Q
         else
            C (J) := 1;
         end if;

         Signs := Shift_Right (Signs, 1);
      end loop;
   end SampleInBall;

end Anubis_MLDSA_Sample;
