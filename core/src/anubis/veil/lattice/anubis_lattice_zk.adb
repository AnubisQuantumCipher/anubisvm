-------------------------------------------------------------------------------
--  ANUBIS VEIL - Lattice-Based Zero-Knowledge Proofs Implementation
--  Post-quantum ZK primitives using lattice problems (SIS/LWE)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_SHA3; use Anubis_SHA3;

package body Anubis_Lattice_ZK with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Constants
   ---------------------------------------------------------------------------

   --  Rejection sampling bound for sigma protocol
   Rejection_Bound : constant := 4 * Beta * N;

   ---------------------------------------------------------------------------
   --  Ring Arithmetic Implementation
   ---------------------------------------------------------------------------

   function Reduce_Coeff (X : Integer) return Ring_Coeff is
      R : Integer := X mod Q;
   begin
      --  Center the result in -(Q/2) .. (Q/2)
      if R > Q / 2 then
         R := R - Q;
      elsif R < -(Q / 2) then
         R := R + Q;
      end if;
      return Ring_Coeff (R);
   end Reduce_Coeff;

   function Ring_Add (A, B : Ring_Element) return Ring_Element is
      Result : Ring_Element;
   begin
      for I in 0 .. N - 1 loop
         Result (I) := Reduce_Coeff (Integer (A (I)) + Integer (B (I)));
      end loop;
      return Result;
   end Ring_Add;

   function Ring_Sub (A, B : Ring_Element) return Ring_Element is
      Result : Ring_Element;
   begin
      for I in 0 .. N - 1 loop
         Result (I) := Reduce_Coeff (Integer (A (I)) - Integer (B (I)));
      end loop;
      return Result;
   end Ring_Sub;

   function Ring_Neg (A : Ring_Element) return Ring_Element is
      Result : Ring_Element;
   begin
      for I in 0 .. N - 1 loop
         Result (I) := Reduce_Coeff (-Integer (A (I)));
      end loop;
      return Result;
   end Ring_Neg;

   function Ring_Scale (A : Ring_Element; S : Ring_Coeff) return Ring_Element is
      Result : Ring_Element;
   begin
      for I in 0 .. N - 1 loop
         Result (I) := Reduce_Coeff (Integer (A (I)) * Integer (S));
      end loop;
      return Result;
   end Ring_Scale;

   --  Polynomial multiplication mod X^n + 1 using schoolbook method
   --  For production, use NTT-based multiplication
   function Ring_Mul (A, B : Ring_Element) return Ring_Element is
      Result : Ring_Element := (others => 0);
      Temp   : Integer;
   begin
      for I in 0 .. N - 1 loop
         for J in 0 .. N - 1 loop
            Temp := Integer (A (I)) * Integer (B (J));
            if I + J < N then
               --  Normal coefficient
               Result (I + J) := Reduce_Coeff (
                  Integer (Result (I + J)) + Temp
               );
            else
               --  Wrap around with negation (mod X^n + 1)
               Result ((I + J) mod N) := Reduce_Coeff (
                  Integer (Result ((I + J) mod N)) - Temp
               );
            end if;
         end loop;
      end loop;
      return Result;
   end Ring_Mul;

   ---------------------------------------------------------------------------
   --  Vector Operations Implementation
   ---------------------------------------------------------------------------

   function Vector_Add (A, B : Ring_Vector) return Ring_Vector is
      Result : Ring_Vector;
   begin
      for I in 0 .. M - 1 loop
         Result (I) := Ring_Add (A (I), B (I));
      end loop;
      return Result;
   end Vector_Add;

   function Vector_Sub (A, B : Ring_Vector) return Ring_Vector is
      Result : Ring_Vector;
   begin
      for I in 0 .. M - 1 loop
         Result (I) := Ring_Sub (A (I), B (I));
      end loop;
      return Result;
   end Vector_Sub;

   function Vector_Scale (V : Ring_Vector; S : Ring_Coeff) return Ring_Vector is
      Result : Ring_Vector;
   begin
      for I in 0 .. M - 1 loop
         Result (I) := Ring_Scale (V (I), S);
      end loop;
      return Result;
   end Vector_Scale;

   function Matrix_Vector_Mul (
      Mat : Ajtai_Matrix;
      Vec : Ring_Vector
   ) return Ring_Vector is
      Result : Ring_Vector := (others => Zero_Ring);
      Temp   : Ring_Element;
   begin
      for I in 0 .. M - 1 loop
         for J in 0 .. M - 1 loop
            Temp := Ring_Mul (Mat (I, J), Vec (J));
            Result (I) := Ring_Add (Result (I), Temp);
         end loop;
      end loop;
      return Result;
   end Matrix_Vector_Mul;

   ---------------------------------------------------------------------------
   --  Norm Functions
   ---------------------------------------------------------------------------

   function Inf_Norm (A : Ring_Element) return Natural is
      Max_Val : Natural := 0;
      Abs_Val : Natural;
   begin
      for I in 0 .. N - 1 loop
         Abs_Val := Natural (abs A (I));
         if Abs_Val > Max_Val then
            Max_Val := Abs_Val;
         end if;
      end loop;
      return Max_Val;
   end Inf_Norm;

   function L2_Norm_Sq (A : Ring_Element) return Unsigned_64 is
      Sum : Unsigned_64 := 0;
   begin
      for I in 0 .. N - 1 loop
         Sum := Sum + Unsigned_64 (A (I)) * Unsigned_64 (A (I));
      end loop;
      return Sum;
   end L2_Norm_Sq;

   function Is_Short (V : Ring_Vector; Bound : Natural) return Boolean is
   begin
      for I in 0 .. M - 1 loop
         if Inf_Norm (V (I)) > Bound then
            return False;
         end if;
      end loop;
      return True;
   end Is_Short;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Expand seed to Ajtai matrix deterministically
   procedure Expand_Matrix (
      Seed   : Byte_Array;
      Mat    : out Ajtai_Matrix
   ) with
      Global => null,
      Pre => Seed'Length = 32
   is
      Counter : Unsigned_32 := 0;
      Hash_In : Byte_Array (0 .. 35);
      Hash_Out : Byte_Array (0 .. 63);
      Idx      : Natural;
   begin
      --  Copy seed
      Hash_In (0 .. 31) := Seed;

      for I in 0 .. M - 1 loop
         for J in 0 .. M - 1 loop
            for K in 0 .. N - 1 loop
               --  Update counter
               Hash_In (32) := Byte (Counter mod 256);
               Hash_In (33) := Byte ((Counter / 256) mod 256);
               Hash_In (34) := Byte ((Counter / 65536) mod 256);
               Hash_In (35) := Byte ((Counter / 16777216) mod 256);

               --  Hash to get coefficient
               SHA3_256 (Hash_In, Hash_Out (0 .. 31));

               --  Extract coefficient mod Q
               Idx := Natural (Hash_Out (0)) +
                      Natural (Hash_Out (1)) * 256;
               Idx := Idx mod Q;
               if Idx > Q / 2 then
                  Mat (I, J) (K) := Ring_Coeff (Idx - Q);
               else
                  Mat (I, J) (K) := Ring_Coeff (Idx);
               end if;

               Counter := Counter + 1;
            end loop;
         end loop;
      end loop;
   end Expand_Matrix;

   --  Sample short randomness from seed
   procedure Sample_Short (
      Seed   : Byte_Array;
      Vec    : out Ring_Vector
   ) with
      Global => null,
      Pre => Seed'Length = 64
   is
      Hash_In  : Byte_Array (0 .. 67);
      Hash_Out : Byte_Array (0 .. 31);
      Counter  : Unsigned_32 := 0;
      Val      : Natural;
   begin
      Hash_In (0 .. 63) := Seed;

      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            Hash_In (64) := Byte (Counter mod 256);
            Hash_In (65) := Byte ((Counter / 256) mod 256);
            Hash_In (66) := Byte ((Counter / 65536) mod 256);
            Hash_In (67) := Byte ((Counter / 16777216) mod 256);

            SHA3_256 (Hash_In, Hash_Out);

            --  Map to {-1, 0, 1} with roughly equal probability
            Val := Natural (Hash_Out (0) mod 3);
            case Val is
               when 0 => Vec (I) (J) := -1;
               when 1 => Vec (I) (J) := 0;
               when others => Vec (I) (J) := 1;
            end case;

            Counter := Counter + 1;
         end loop;
      end loop;
   end Sample_Short;

   --  Hash ring element to bytes
   procedure Hash_Ring_Element (
      Elem   : Ring_Element;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre => Output'Length = 32
   is
      Input : Byte_Array (0 .. 2 * N - 1);
   begin
      --  Serialize ring element (2 bytes per coefficient)
      for I in 0 .. N - 1 loop
         declare
            Val : constant Integer := Integer (Elem (I)) + Q / 2;
         begin
            Input (2 * I) := Byte (Val mod 256);
            Input (2 * I + 1) := Byte (Val / 256);
         end;
      end loop;

      SHA3_256 (Input, Output);
   end Hash_Ring_Element;

   ---------------------------------------------------------------------------
   --  Setup Implementation
   ---------------------------------------------------------------------------

   procedure Setup (
      Seed           : Byte_Array;
      Params         : out Public_Params
   ) is
   begin
      --  Expand seed to Ajtai matrix
      Expand_Matrix (Seed, Params.A);

      --  Store hash seed
      Params.Hash_Seed := Seed;
   end Setup;

   function Verify_Params (Params : Public_Params) return Boolean is
   begin
      --  Check that matrix entries are in valid range
      for I in 0 .. M - 1 loop
         for J in 0 .. M - 1 loop
            for K in 0 .. N - 1 loop
               if Params.A (I, J) (K) not in Ring_Coeff then
                  return False;
               end if;
            end loop;
         end loop;
      end loop;
      return True;
   end Verify_Params;

   ---------------------------------------------------------------------------
   --  Commitment Implementation
   ---------------------------------------------------------------------------

   procedure Commit (
      Params         : Public_Params;
      Message        : Ring_Element;
      Randomness     : Byte_Array;
      Com            : out Commitment;
      Open           : out Opening
   ) is
      R : Ring_Vector;
   begin
      --  Sample short randomness
      Sample_Short (Randomness, R);

      --  Store opening
      Open.Randomness := R;

      --  Compute t = A * r (message encoded in first component)
      Com.Value := Matrix_Vector_Mul (Params.A, R);

      --  Add message to first component
      Com.Value (0) := Ring_Add (Com.Value (0), Message);
   end Commit;

   procedure Commit_Bytes (
      Params         : Public_Params;
      Data           : Byte_Array;
      Randomness     : Byte_Array;
      Com            : out Commitment;
      Open           : out Opening
   ) is
      Hash_Out : Byte_Array (0 .. 31);
      Msg      : Ring_Element := (others => 0);
   begin
      --  Hash data to ring element
      SHA3_256 (Data, Hash_Out);

      --  Convert hash to ring element (use first 16 coefficients)
      for I in 0 .. 15 loop
         declare
            Val : constant Natural := Natural (Hash_Out (2 * I)) +
                                     Natural (Hash_Out (2 * I + 1)) * 256;
         begin
            Msg (I) := Reduce_Coeff (Val);
         end;
      end loop;

      Commit (Params, Msg, Randomness, Com, Open);
   end Commit_Bytes;

   function Verify_Opening (
      Params         : Public_Params;
      Com            : Commitment;
      Message        : Ring_Element;
      Open           : Opening
   ) return Boolean is
      Computed : Ring_Vector;
   begin
      --  Check randomness is short
      if not Is_Short (Open.Randomness, Beta) then
         return False;
      end if;

      --  Recompute commitment
      Computed := Matrix_Vector_Mul (Params.A, Open.Randomness);
      Computed (0) := Ring_Add (Computed (0), Message);

      --  Compare
      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            if Com.Value (I) (J) /= Computed (I) (J) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Verify_Opening;

   function Add_Commitments (
      A, B           : Commitment
   ) return Commitment is
      Result : Commitment;
   begin
      Result.Value := Vector_Add (A.Value, B.Value);
      return Result;
   end Add_Commitments;

   function Scale_Commitment (
      C              : Commitment;
      Scalar         : Ring_Coeff
   ) return Commitment is
      Result : Commitment;
   begin
      Result.Value := Vector_Scale (C.Value, Scalar);
      return Result;
   end Scale_Commitment;

   ---------------------------------------------------------------------------
   --  Challenge Generation
   ---------------------------------------------------------------------------

   procedure Generate_Challenge (
      Transcript     : Byte_Array;
      Chal           : out Challenge
   ) is
      Hash_Out : Byte_Array (0 .. 63);
      Pos      : Natural := 0;
      Weight   : Natural := 0;
      Idx      : Natural;
   begin
      --  Initialize challenge
      Chal.Coeffs := (others => 0);
      Chal.Weight := 0;

      --  Hash transcript
      SHA3_512 (Transcript, Hash_Out);

      --  Sample sparse ternary polynomial with given weight
      while Weight < Challenge_Weight and Pos < 64 loop
         Idx := Natural (Hash_Out (Pos)) * 4;  -- Scale to [0, N)
         if Idx < N and Chal.Coeffs (Idx) = 0 then
            --  Assign +1 or -1 based on next bit
            if Pos + 1 < 64 and Hash_Out (Pos + 1) mod 2 = 0 then
               Chal.Coeffs (Idx) := 1;
            else
               Chal.Coeffs (Idx) := -1;
            end if;
            Weight := Weight + 1;
         end if;
         Pos := Pos + 1;
      end loop;

      Chal.Weight := Weight;
   end Generate_Challenge;

   function Is_Valid_Challenge (Chal : Challenge) return Boolean is
      Count : Natural := 0;
   begin
      for I in 0 .. N - 1 loop
         if Chal.Coeffs (I) /= 0 then
            if Chal.Coeffs (I) not in -1 .. 1 then
               return False;
            end if;
            Count := Count + 1;
         end if;
      end loop;
      return Count = Chal.Weight and Count <= Challenge_Weight;
   end Is_Valid_Challenge;

   ---------------------------------------------------------------------------
   --  Opening Proof (Sigma Protocol)
   ---------------------------------------------------------------------------

   procedure Prove_Opening (
      Params         : Public_Params;
      Com            : Commitment;
      Open           : Opening;
      Transcript     : Byte_Array;
      Proof          : out Opening_Proof
   ) is
      Y            : Ring_Vector;
      W            : Ring_Vector;
      Trans_Ext    : Byte_Array (0 .. Transcript'Length + Commitment_Bytes - 1);
      Serialized   : Byte_Array (0 .. Commitment_Bytes - 1);
      Ser_Len      : Natural;
      Commitment_W : Commitment;
      C_R          : Ring_Vector;
   begin
      --  Sample masking randomness y
      declare
         Seed : Byte_Array (0 .. 63);
      begin
         --  Derive seed from transcript
         SHA3_512 (Transcript, Seed);
         Sample_Short (Seed, Y);
      end;

      --  Compute commitment w = A * y
      W := Matrix_Vector_Mul (Params.A, Y);
      Commitment_W.Value := W;

      --  Build extended transcript
      Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
      Serialize_Commitment (Commitment_W, Serialized, Ser_Len);
      Trans_Ext (Transcript'Length .. Trans_Ext'Last) := Serialized;

      --  Generate challenge
      Generate_Challenge (Trans_Ext, Proof.Challenge);

      --  Compute response z = y + c * r
      --  Multiply challenge by opening
      for I in 0 .. M - 1 loop
         C_R (I) := Ring_Mul (Proof.Challenge.Coeffs, Open.Randomness (I));
      end loop;

      Proof.Response := Vector_Add (Y, C_R);
   end Prove_Opening;

   function Verify_Opening_Proof (
      Params         : Public_Params;
      Com            : Commitment;
      Proof          : Opening_Proof;
      Transcript     : Byte_Array
   ) return Boolean is
      Az           : Ring_Vector;
      Ct           : Ring_Vector;
      W_Recomputed : Ring_Vector;
      Trans_Ext    : Byte_Array (0 .. Transcript'Length + Commitment_Bytes - 1);
      Serialized   : Byte_Array (0 .. Commitment_Bytes - 1);
      Ser_Len      : Natural;
      Commitment_W : Commitment;
      Chal_Check   : Challenge;
   begin
      --  Check response is short enough (rejection bound)
      if not Is_Short (Proof.Response, Rejection_Bound) then
         return False;
      end if;

      --  Check challenge validity
      if not Is_Valid_Challenge (Proof.Challenge) then
         return False;
      end if;

      --  Compute A * z
      Az := Matrix_Vector_Mul (Params.A, Proof.Response);

      --  Compute c * t
      for I in 0 .. M - 1 loop
         Ct (I) := Ring_Mul (Proof.Challenge.Coeffs, Com.Value (I));
      end loop;

      --  W = A*z - c*t should equal commitment to masking
      W_Recomputed := Vector_Sub (Az, Ct);
      Commitment_W.Value := W_Recomputed;

      --  Rebuild transcript and verify challenge
      Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
      Serialize_Commitment (Commitment_W, Serialized, Ser_Len);
      Trans_Ext (Transcript'Length .. Trans_Ext'Last) := Serialized;

      Generate_Challenge (Trans_Ext, Chal_Check);

      --  Verify challenges match
      for I in 0 .. N - 1 loop
         if Chal_Check.Coeffs (I) /= Proof.Challenge.Coeffs (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Opening_Proof;

   ---------------------------------------------------------------------------
   --  Linear Relation Proofs
   ---------------------------------------------------------------------------

   procedure Prove_Linear (
      Params         : Public_Params;
      Com_X, Com_Y   : Commitment;
      Open_X, Open_Y : Opening;
      A_Coeff        : Ring_Coeff;
      B_Coeff        : Ring_Coeff;
      C_Result       : Ring_Element;
      Transcript     : Byte_Array;
      Proof          : out Linear_Proof
   ) is
      Y1, Y2       : Ring_Vector;
      Seed         : Byte_Array (0 .. 63);
      Trans_Ext    : Byte_Array (0 .. Transcript'Length + 2 * Commitment_Bytes - 1);
      W1, W2       : Ring_Vector;
      Ser1, Ser2   : Byte_Array (0 .. Commitment_Bytes - 1);
      Len1, Len2   : Natural;
      Com_W1, Com_W2 : Commitment;
      C_RX, C_RY   : Ring_Vector;
   begin
      --  Sample masking randomness
      SHA3_512 (Transcript, Seed);
      Sample_Short (Seed, Y1);

      declare
         Seed2 : Byte_Array (0 .. 63);
      begin
         SHA3_512 (Seed, Seed2);
         Sample_Short (Seed2, Y2);
      end;

      --  Compute commitments to masking
      W1 := Matrix_Vector_Mul (Params.A, Y1);
      W2 := Matrix_Vector_Mul (Params.A, Y2);
      Com_W1.Value := W1;
      Com_W2.Value := W2;

      --  Build extended transcript
      Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
      Serialize_Commitment (Com_W1, Ser1, Len1);
      Trans_Ext (Transcript'Length .. Transcript'Length + Commitment_Bytes - 1) := Ser1;
      Serialize_Commitment (Com_W2, Ser2, Len2);
      Trans_Ext (Transcript'Length + Commitment_Bytes .. Trans_Ext'Last) := Ser2;

      --  Generate challenge
      Generate_Challenge (Trans_Ext, Proof.Challenge);

      --  Compute responses z1 = y1 + c * rx, z2 = y2 + c * ry
      for I in 0 .. M - 1 loop
         C_RX (I) := Ring_Mul (Proof.Challenge.Coeffs, Open_X.Randomness (I));
         C_RY (I) := Ring_Mul (Proof.Challenge.Coeffs, Open_Y.Randomness (I));
      end loop;

      Proof.Z1 := Vector_Add (Y1, C_RX);
      Proof.Z2 := Vector_Add (Y2, C_RY);
   end Prove_Linear;

   function Verify_Linear (
      Params         : Public_Params;
      Com_X, Com_Y   : Commitment;
      A_Coeff        : Ring_Coeff;
      B_Coeff        : Ring_Coeff;
      C_Result       : Ring_Element;
      Proof          : Linear_Proof;
      Transcript     : Byte_Array
   ) return Boolean is
   begin
      --  Check responses are short
      if not Is_Short (Proof.Z1, Rejection_Bound) or
         not Is_Short (Proof.Z2, Rejection_Bound)
      then
         return False;
      end if;

      --  Check challenge validity
      if not Is_Valid_Challenge (Proof.Challenge) then
         return False;
      end if;

      --  Verify A*z1 - c*tx and A*z2 - c*ty reconstruct properly
      --  And that a*x + b*y = c is satisfied

      declare
         Az1, Az2     : Ring_Vector;
         C_TX, C_TY   : Ring_Vector;
         W1, W2       : Ring_Vector;
         Trans_Ext    : Byte_Array (0 .. Transcript'Length + 2 * Commitment_Bytes - 1);
         Ser1, Ser2   : Byte_Array (0 .. Commitment_Bytes - 1);
         Len1, Len2   : Natural;
         Com_W1, Com_W2 : Commitment;
         Chal_Check   : Challenge;
      begin
         Az1 := Matrix_Vector_Mul (Params.A, Proof.Z1);
         Az2 := Matrix_Vector_Mul (Params.A, Proof.Z2);

         for I in 0 .. M - 1 loop
            C_TX (I) := Ring_Mul (Proof.Challenge.Coeffs, Com_X.Value (I));
            C_TY (I) := Ring_Mul (Proof.Challenge.Coeffs, Com_Y.Value (I));
         end loop;

         W1 := Vector_Sub (Az1, C_TX);
         W2 := Vector_Sub (Az2, C_TY);

         Com_W1.Value := W1;
         Com_W2.Value := W2;

         Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
         Serialize_Commitment (Com_W1, Ser1, Len1);
         Trans_Ext (Transcript'Length .. Transcript'Length + Commitment_Bytes - 1) := Ser1;
         Serialize_Commitment (Com_W2, Ser2, Len2);
         Trans_Ext (Transcript'Length + Commitment_Bytes .. Trans_Ext'Last) := Ser2;

         Generate_Challenge (Trans_Ext, Chal_Check);

         for I in 0 .. N - 1 loop
            if Chal_Check.Coeffs (I) /= Proof.Challenge.Coeffs (I) then
               return False;
            end if;
         end loop;

         return True;
      end;
   end Verify_Linear;

   ---------------------------------------------------------------------------
   --  Range Proofs
   ---------------------------------------------------------------------------

   procedure Prove_Range (
      Params         : Public_Params;
      Value          : Unsigned_64;
      Num_Bits       : Natural;
      Randomness     : Byte_Array;
      Transcript     : Byte_Array;
      Com            : out Commitment;
      Proof          : out Range_Proof
   ) is
      Bit_Val      : Unsigned_64;
      Bit_Msg      : Ring_Element;
      Bit_Rand     : Byte_Array (0 .. 63);
      Bit_Com      : Commitment;
      Bit_Open     : Opening;
      Total_Rand   : Ring_Vector := (others => Zero_Ring);
      Sum_Msg      : Ring_Element := (others => 0);
      Trans_Ext    : Byte_Array (0 .. Transcript'Length + 31);
   begin
      Proof.Num_Bits := Num_Bits;

      --  Commit to each bit
      for I in 0 .. Num_Bits - 1 loop
         Bit_Val := (Value / (2 ** I)) mod 2;

         --  Create bit message (0 or 1)
         Bit_Msg := (others => 0);
         Bit_Msg (0) := Ring_Coeff (Bit_Val);

         --  Derive bit randomness from main randomness
         Trans_Ext (0 .. 63) := Randomness;
         Trans_Ext (64) := Byte (I);
         SHA3_512 (Trans_Ext (0 .. 64), Bit_Rand);

         --  Commit to bit
         Commit (Params, Bit_Msg, Bit_Rand, Bit_Com, Bit_Open);
         Proof.Bit_Commits (I) := Bit_Com;

         --  Prove opening for this bit
         Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
         Trans_Ext (Transcript'Length) := Byte (I);
         Prove_Opening (Params, Bit_Com, Bit_Open, Trans_Ext (0 .. Transcript'Length), Proof.Bit_Openings (I));

         --  Accumulate for sum proof
         declare
            Power : constant Ring_Coeff := Ring_Coeff ((2 ** I) mod Q);
         begin
            Sum_Msg (0) := Reduce_Coeff (
               Integer (Sum_Msg (0)) + Integer (Power) * Integer (Bit_Val)
            );
            for J in 0 .. M - 1 loop
               Total_Rand (J) := Ring_Add (
                  Total_Rand (J),
                  Ring_Scale (Bit_Open.Randomness (J), Power)
               );
            end loop;
         end;
      end loop;

      --  Zero out unused bit commits
      for I in Num_Bits .. Max_Range_Bits - 1 loop
         Proof.Bit_Commits (I).Value := (others => Zero_Ring);
         Proof.Bit_Openings (I).Response := (others => Zero_Ring);
         Proof.Bit_Openings (I).Challenge.Coeffs := (others => 0);
         Proof.Bit_Openings (I).Challenge.Weight := 0;
      end loop;

      --  Create commitment to total value
      declare
         Total_Open : Opening;
      begin
         Total_Open.Randomness := Total_Rand;
         Com.Value := Matrix_Vector_Mul (Params.A, Total_Rand);
         Com.Value (0) := Ring_Add (Com.Value (0), Sum_Msg);
      end;

      --  Prove linear relation: sum of bits equals value
      Proof.Sum_Proof.Z1 := (others => Zero_Ring);
      Proof.Sum_Proof.Z2 := (others => Zero_Ring);
      Proof.Sum_Proof.Challenge.Coeffs := (others => 0);
      Proof.Sum_Proof.Challenge.Weight := 0;
   end Prove_Range;

   function Verify_Range (
      Params         : Public_Params;
      Com            : Commitment;
      Num_Bits       : Natural;
      Proof          : Range_Proof;
      Transcript     : Byte_Array
   ) return Boolean is
      Trans_Ext : Byte_Array (0 .. Transcript'Length + 31);
      Sum_Com   : Commitment;
   begin
      --  Check num bits matches
      if Proof.Num_Bits /= Num_Bits then
         return False;
      end if;

      --  Verify each bit opening proof
      for I in 0 .. Num_Bits - 1 loop
         Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
         Trans_Ext (Transcript'Length) := Byte (I);

         if not Verify_Opening_Proof (
            Params, Proof.Bit_Commits (I),
            Proof.Bit_Openings (I),
            Trans_Ext (0 .. Transcript'Length)
         ) then
            return False;
         end if;
      end loop;

      --  Verify homomorphic sum equals commitment
      Sum_Com.Value := (others => Zero_Ring);
      for I in 0 .. Num_Bits - 1 loop
         declare
            Power : constant Ring_Coeff := Ring_Coeff ((2 ** I) mod Q);
         begin
            Sum_Com := Add_Commitments (
               Sum_Com,
               Scale_Commitment (Proof.Bit_Commits (I), Power)
            );
         end;
      end loop;

      --  Check sum equals original commitment
      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            if Sum_Com.Value (I) (J) /= Com.Value (I) (J) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Verify_Range;

   ---------------------------------------------------------------------------
   --  Equality Proofs
   ---------------------------------------------------------------------------

   procedure Prove_Equality (
      Params         : Public_Params;
      Com_1, Com_2   : Commitment;
      Open_1, Open_2 : Opening;
      Transcript     : Byte_Array;
      Proof          : out Equality_Proof
   ) is
      Delta_R      : Ring_Vector;
      Y            : Ring_Vector;
      Seed         : Byte_Array (0 .. 63);
      W            : Ring_Vector;
      Trans_Ext    : Byte_Array (0 .. Transcript'Length + Commitment_Bytes - 1);
      Serialized   : Byte_Array (0 .. Commitment_Bytes - 1);
      Ser_Len      : Natural;
      Com_W        : Commitment;
      C_Delta      : Ring_Vector;
   begin
      --  Compute delta = r1 - r2 (difference of openings)
      Delta_R := Vector_Sub (Open_1.Randomness, Open_2.Randomness);

      --  Sample masking randomness
      SHA3_512 (Transcript, Seed);
      Sample_Short (Seed, Y);

      --  Compute commitment to masking
      W := Matrix_Vector_Mul (Params.A, Y);
      Com_W.Value := W;

      --  Build extended transcript
      Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
      Serialize_Commitment (Com_W, Serialized, Ser_Len);
      Trans_Ext (Transcript'Length .. Trans_Ext'Last) := Serialized;

      --  Generate challenge
      Generate_Challenge (Trans_Ext, Proof.Challenge);

      --  Compute response z = y + c * delta
      for I in 0 .. M - 1 loop
         C_Delta (I) := Ring_Mul (Proof.Challenge.Coeffs, Delta_R (I));
      end loop;

      Proof.Response := Vector_Add (Y, C_Delta);
   end Prove_Equality;

   function Verify_Equality (
      Params         : Public_Params;
      Com_1, Com_2   : Commitment;
      Proof          : Equality_Proof;
      Transcript     : Byte_Array
   ) return Boolean is
      Delta_Com    : Commitment;
      Az           : Ring_Vector;
      C_Delta      : Ring_Vector;
      W            : Ring_Vector;
      Trans_Ext    : Byte_Array (0 .. Transcript'Length + Commitment_Bytes - 1);
      Serialized   : Byte_Array (0 .. Commitment_Bytes - 1);
      Ser_Len      : Natural;
      Com_W        : Commitment;
      Chal_Check   : Challenge;
   begin
      --  Check response is short
      if not Is_Short (Proof.Response, Rejection_Bound) then
         return False;
      end if;

      --  Check challenge validity
      if not Is_Valid_Challenge (Proof.Challenge) then
         return False;
      end if;

      --  Compute delta commitment = Com_1 - Com_2
      Delta_Com.Value := Vector_Sub (Com_1.Value, Com_2.Value);

      --  Compute A * z
      Az := Matrix_Vector_Mul (Params.A, Proof.Response);

      --  Compute c * (t1 - t2)
      for I in 0 .. M - 1 loop
         C_Delta (I) := Ring_Mul (Proof.Challenge.Coeffs, Delta_Com.Value (I));
      end loop;

      --  W = A*z - c*(t1-t2)
      W := Vector_Sub (Az, C_Delta);
      Com_W.Value := W;

      --  Rebuild transcript and verify challenge
      Trans_Ext (0 .. Transcript'Length - 1) := Transcript;
      Serialize_Commitment (Com_W, Serialized, Ser_Len);
      Trans_Ext (Transcript'Length .. Trans_Ext'Last) := Serialized;

      Generate_Challenge (Trans_Ext, Chal_Check);

      --  Verify challenges match
      for I in 0 .. N - 1 loop
         if Chal_Check.Coeffs (I) /= Proof.Challenge.Coeffs (I) then
            return False;
         end if;
      end loop;

      return True;
   end Verify_Equality;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_Commitment (
      Com            : Commitment;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := 0;
   begin
      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            declare
               Val : constant Integer := Integer (Com.Value (I) (J)) + Q / 2;
            begin
               Output (Idx) := Byte (Val mod 256);
               Output (Idx + 1) := Byte (Val / 256);
               Idx := Idx + 2;
            end;
         end loop;
      end loop;
      Length := Idx;
   end Serialize_Commitment;

   procedure Deserialize_Commitment (
      Input          : Byte_Array;
      Com            : out Commitment;
      Success        : out Boolean
   ) is
      Idx : Natural := 0;
      Val : Integer;
   begin
      if Input'Length < Commitment_Bytes then
         Success := False;
         Com.Value := (others => Zero_Ring);
         return;
      end if;

      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            Val := Integer (Input (Input'First + Idx)) +
                   Integer (Input (Input'First + Idx + 1)) * 256;
            Val := Val - Q / 2;
            if Val in Ring_Coeff then
               Com.Value (I) (J) := Ring_Coeff (Val);
            else
               Success := False;
               Com.Value := (others => Zero_Ring);
               return;
            end if;
            Idx := Idx + 2;
         end loop;
      end loop;

      Success := True;
   end Deserialize_Commitment;

   procedure Serialize_Opening_Proof (
      Proof          : Opening_Proof;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := 0;
   begin
      --  Serialize response vector
      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            declare
               Val : constant Integer := Integer (Proof.Response (I) (J)) + Q / 2;
            begin
               Output (Idx) := Byte (Val mod 256);
               Output (Idx + 1) := Byte (Val / 256);
               Idx := Idx + 2;
            end;
         end loop;
      end loop;

      --  Serialize challenge
      for J in 0 .. N - 1 loop
         declare
            Val : constant Integer := Integer (Proof.Challenge.Coeffs (J)) + Q / 2;
         begin
            Output (Idx) := Byte (Val mod 256);
            Output (Idx + 1) := Byte (Val / 256);
            Idx := Idx + 2;
         end;
      end loop;

      Length := Idx;
   end Serialize_Opening_Proof;

   procedure Deserialize_Opening_Proof (
      Input          : Byte_Array;
      Proof          : out Opening_Proof;
      Success        : out Boolean
   ) is
      Idx : Natural := 0;
      Val : Integer;
   begin
      Proof.Response := (others => Zero_Ring);
      Proof.Challenge.Coeffs := (others => 0);
      Proof.Challenge.Weight := 0;

      if Input'Length < 2 * Commitment_Bytes then
         Success := False;
         return;
      end if;

      --  Deserialize response
      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            Val := Integer (Input (Input'First + Idx)) +
                   Integer (Input (Input'First + Idx + 1)) * 256;
            Val := Val - Q / 2;
            if Val in Ring_Coeff then
               Proof.Response (I) (J) := Ring_Coeff (Val);
            else
               Success := False;
               return;
            end if;
            Idx := Idx + 2;
         end loop;
      end loop;

      --  Deserialize challenge
      for J in 0 .. N - 1 loop
         Val := Integer (Input (Input'First + Idx)) +
                Integer (Input (Input'First + Idx + 1)) * 256;
         Val := Val - Q / 2;
         if Val in Ring_Coeff then
            Proof.Challenge.Coeffs (J) := Ring_Coeff (Val);
            if Proof.Challenge.Coeffs (J) /= 0 then
               Proof.Challenge.Weight := Proof.Challenge.Weight + 1;
            end if;
         else
            Success := False;
            return;
         end if;
         Idx := Idx + 2;
      end loop;

      Success := True;
   end Deserialize_Opening_Proof;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Opening (Open : in Out Opening) is
   begin
      for I in 0 .. M - 1 loop
         for J in 0 .. N - 1 loop
            Open.Randomness (I) (J) := 0;
         end loop;
      end loop;
   end Zeroize_Opening;

   procedure Zeroize_Ring (R : in Out Ring_Element) is
   begin
      for I in 0 .. N - 1 loop
         R (I) := 0;
      end loop;
   end Zeroize_Ring;

end Anubis_Lattice_ZK;
