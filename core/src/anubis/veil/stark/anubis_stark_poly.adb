-------------------------------------------------------------------------------
--  ANUBIS VEIL - STARK Polynomial Operations (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3;
with Anubis_Types; use Anubis_Types;

package body Anubis_STARK_Poly with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Domain Operations
   ---------------------------------------------------------------------------

   function Create_Domain (Log_Size : Natural) return Domain is
   begin
      return (
         Size     => 2 ** Log_Size,
         Log_Size => Log_Size,
         Offset   => One,
         Root     => Get_Root_Of_Unity (Log_Size)
      );
   end Create_Domain;

   function Create_Coset (
      Log_Size : Natural;
      Offset   : Field_Element
   ) return Domain is
   begin
      return (
         Size     => 2 ** Log_Size,
         Log_Size => Log_Size,
         Offset   => Offset,
         Root     => Get_Root_Of_Unity (Log_Size)
      );
   end Create_Coset;

   function Domain_Element (D : Domain; Index : Natural) return Field_Element is
   begin
      return Mul (D.Offset, Exp (D.Root, Unsigned_64 (Index)));
   end Domain_Element;

   ---------------------------------------------------------------------------
   --  FFT (Cooley-Tukey)
   ---------------------------------------------------------------------------

   procedure FFT (
      Coeffs : Field_Array;
      Dom    : Domain;
      Evals  : out Field_Array
   ) is
      N      : constant Natural := Dom.Size;
      Log_N  : constant Natural := Dom.Log_Size;
      Temp   : Field_Array (0 .. N - 1);
   begin
      --  Copy input
      Temp := Coeffs;

      --  Bit-reversal permutation
      for I in 0 .. N - 1 loop
         declare
            Rev : Natural := 0;
            Idx : Natural := I;
         begin
            for J in 0 .. Log_N - 1 loop
               Rev := Rev * 2 + (Idx mod 2);
               Idx := Idx / 2;
            end loop;
            if Rev > I then
               declare
                  T : constant Field_Element := Temp (I);
               begin
                  Temp (I) := Temp (Rev);
                  Temp (Rev) := T;
               end;
            end if;
         end;
      end loop;

      --  Cooley-Tukey butterfly
      declare
         M : Natural := 1;
      begin
         for S in 1 .. Log_N loop
            M := M * 2;
            declare
               Omega_M : constant Field_Element :=
                  Get_Root_Of_Unity (S);
            begin
               for K in 0 .. (N / M) - 1 loop
                  declare
                     Omega : Field_Element := One;
                  begin
                     for J in 0 .. (M / 2) - 1 loop
                        declare
                           Idx1 : constant Natural := K * M + J;
                           Idx2 : constant Natural := Idx1 + M / 2;
                           T    : constant Field_Element :=
                              Mul (Omega, Temp (Idx2));
                           U    : constant Field_Element := Temp (Idx1);
                        begin
                           Temp (Idx1) := Add (U, T);
                           Temp (Idx2) := Sub (U, T);
                        end;
                        Omega := Mul (Omega, Omega_M);
                     end loop;
                  end;
               end loop;
            end;
         end loop;
      end;

      Evals := Temp;
   end FFT;

   ---------------------------------------------------------------------------
   --  IFFT
   ---------------------------------------------------------------------------

   procedure IFFT (
      Evals  : Field_Array;
      Dom    : Domain;
      Coeffs : out Field_Array
   ) is
      N       : constant Natural := Dom.Size;
      N_Inv   : constant Field_Element := Inv (Field_Element (N));
      Temp    : Field_Array (0 .. N - 1);
      Inv_Dom : Domain;
   begin
      --  Create domain with inverse root
      Inv_Dom := (
         Size     => Dom.Size,
         Log_Size => Dom.Log_Size,
         Offset   => Dom.Offset,
         Root     => Inv (Dom.Root)
      );

      --  Forward FFT with inverse root
      FFT (Evals, Inv_Dom, Temp);

      --  Scale by 1/N
      for I in Temp'Range loop
         Coeffs (I) := Mul (Temp (I), N_Inv);
      end loop;
   end IFFT;

   ---------------------------------------------------------------------------
   --  Coset FFT
   ---------------------------------------------------------------------------

   procedure Coset_FFT (
      Coeffs : Field_Array;
      Dom    : Domain;
      Evals  : out Field_Array
   ) is
      Scaled : Field_Array (0 .. Dom.Size - 1);
      Power  : Field_Element := One;
   begin
      --  Scale coefficients by offset^i
      for I in Coeffs'Range loop
         Scaled (I) := Mul (Coeffs (I), Power);
         Power := Mul (Power, Dom.Offset);
      end loop;

      --  Zero-pad if needed
      for I in Coeffs'Length .. Dom.Size - 1 loop
         Scaled (I) := Zero;
      end loop;

      --  Standard FFT
      declare
         Std_Dom : constant Domain := Create_Domain (Dom.Log_Size);
      begin
         FFT (Scaled, Std_Dom, Evals);
      end;
   end Coset_FFT;

   procedure Coset_IFFT (
      Evals  : Field_Array;
      Dom    : Domain;
      Coeffs : out Field_Array
   ) is
      Temp   : Field_Array (0 .. Dom.Size - 1);
      Power  : Field_Element;
      Offset_Inv : constant Field_Element := Inv (Dom.Offset);
   begin
      --  Standard IFFT
      declare
         Std_Dom : constant Domain := Create_Domain (Dom.Log_Size);
      begin
         IFFT (Evals, Std_Dom, Temp);
      end;

      --  Unscale by offset^(-i)
      Power := One;
      for I in Temp'Range loop
         Coeffs (I) := Mul (Temp (I), Power);
         Power := Mul (Power, Offset_Inv);
      end loop;
   end Coset_IFFT;

   ---------------------------------------------------------------------------
   --  Polynomial Operations
   ---------------------------------------------------------------------------

   function From_Coeffs (Coeffs : Field_Array) return Polynomial is
      P : Polynomial;
   begin
      P.Coeffs := (others => Zero);
      for I in Coeffs'Range loop
         P.Coeffs (I) := Coeffs (I);
      end loop;

      --  Find actual degree
      P.Degree := 0;
      for I in reverse Coeffs'Range loop
         if Coeffs (I) /= Zero then
            P.Degree := I;
            exit;
         end if;
      end loop;

      return P;
   end From_Coeffs;

   function Zero_Poly return Polynomial is
      P : Polynomial;
   begin
      P.Coeffs := (others => Zero);
      P.Degree := 0;
      return P;
   end Zero_Poly;

   function Poly_Add (A, B : Polynomial) return Polynomial is
      P      : Polynomial;
      Max_D  : constant Natural := Natural'Max (A.Degree, B.Degree);
   begin
      P.Coeffs := (others => Zero);

      for I in 0 .. Max_D loop
         P.Coeffs (I) := Add (A.Coeffs (I), B.Coeffs (I));
      end loop;

      --  Find actual degree
      P.Degree := 0;
      for I in reverse 0 .. Max_D loop
         if P.Coeffs (I) /= Zero then
            P.Degree := I;
            exit;
         end if;
      end loop;

      return P;
   end Poly_Add;

   function Poly_Sub (A, B : Polynomial) return Polynomial is
      P      : Polynomial;
      Max_D  : constant Natural := Natural'Max (A.Degree, B.Degree);
   begin
      P.Coeffs := (others => Zero);

      for I in 0 .. Max_D loop
         P.Coeffs (I) := Sub (A.Coeffs (I), B.Coeffs (I));
      end loop;

      P.Degree := 0;
      for I in reverse 0 .. Max_D loop
         if P.Coeffs (I) /= Zero then
            P.Degree := I;
            exit;
         end if;
      end loop;

      return P;
   end Poly_Sub;

   function Poly_Scale (P : Polynomial; S : Field_Element) return Polynomial is
      Result : Polynomial;
   begin
      Result.Coeffs := (others => Zero);
      for I in 0 .. P.Degree loop
         Result.Coeffs (I) := Mul (P.Coeffs (I), S);
      end loop;
      Result.Degree := P.Degree;
      return Result;
   end Poly_Scale;

   function Poly_Mul (A, B : Polynomial) return Polynomial is
      Result : Polynomial;
      N      : constant Natural := A.Degree + B.Degree + 1;
   begin
      Result.Coeffs := (others => Zero);

      --  Simple O(n^2) multiplication for now
      --  Would use FFT for large polynomials
      for I in 0 .. A.Degree loop
         for J in 0 .. B.Degree loop
            Result.Coeffs (I + J) :=
               Add (Result.Coeffs (I + J), Mul (A.Coeffs (I), B.Coeffs (J)));
         end loop;
      end loop;

      Result.Degree := A.Degree + B.Degree;
      return Result;
   end Poly_Mul;

   procedure Poly_Div (
      A        : Polynomial;
      B        : Polynomial;
      Quotient : out Polynomial;
      Remainder: out Polynomial
   ) is
      R : Polynomial := A;
      Q : Polynomial;
      D : Natural := A.Degree;
   begin
      Q.Coeffs := (others => Zero);
      Q.Degree := 0;

      while D >= B.Degree and R.Coeffs (D) /= Zero loop
         declare
            Coeff : constant Field_Element :=
               Div_F (R.Coeffs (D), B.Coeffs (B.Degree));
            Shift : constant Natural := D - B.Degree;
         begin
            Q.Coeffs (Shift) := Coeff;
            if Shift > Q.Degree then
               Q.Degree := Shift;
            end if;

            for I in 0 .. B.Degree loop
               R.Coeffs (I + Shift) :=
                  Sub (R.Coeffs (I + Shift), Mul (Coeff, B.Coeffs (I)));
            end loop;
         end;

         if D = 0 then
            exit;
         end if;
         D := D - 1;
      end loop;

      Quotient := Q;
      Remainder := R;
   end Poly_Div;

   function Poly_Eval (P : Polynomial; X : Field_Element) return Field_Element is
      Result : Field_Element := Zero;
   begin
      --  Horner"s method
      for I in reverse 0 .. P.Degree loop
         Result := Add (Mul (Result, X), P.Coeffs (I));
      end loop;
      return Result;
   end Poly_Eval;

   procedure Poly_Multi_Eval (
      P      : Polynomial;
      Points : Field_Array;
      Values : out Field_Array
   ) is
   begin
      for I in Points'Range loop
         Values (I) := Poly_Eval (P, Points (I));
      end loop;
   end Poly_Multi_Eval;

   ---------------------------------------------------------------------------
   --  Interpolation
   ---------------------------------------------------------------------------

   function Interpolate (Evals : Evaluations) return Polynomial is
      Coeffs : Field_Array (0 .. Evals.Count - 1);
   begin
      IFFT (Evals.Values (0 .. Evals.Count - 1), Evals.Dom, Coeffs);
      return From_Coeffs (Coeffs);
   end Interpolate;

   function Interpolate_Points (
      Points : Field_Array;
      Values : Field_Array
   ) return Polynomial is
      N      : constant Natural := Points'Length;
      Result : Polynomial;
   begin
      Result.Coeffs := (others => Zero);
      Result.Degree := 0;

      --  Lagrange interpolation
      for I in Points'Range loop
         declare
            Term : Polynomial;
            Denom : Field_Element := One;
         begin
            Term.Coeffs := (others => Zero);
            Term.Coeffs (0) := One;
            Term.Degree := 0;

            for J in Points'Range loop
               if J /= I then
                  --  Multiply by (x - Points(j))
                  declare
                     Factor : Polynomial;
                  begin
                     Factor.Coeffs := (others => Zero);
                     Factor.Coeffs (0) := Neg (Points (J));
                     Factor.Coeffs (1) := One;
                     Factor.Degree := 1;
                     Term := Poly_Mul (Term, Factor);
                  end;
                  Denom := Mul (Denom, Sub (Points (I), Points (J)));
               end if;
            end loop;

            --  Scale by Values(i) / denom
            Term := Poly_Scale (Term, Div_F (Values (I), Denom));
            Result := Poly_Add (Result, Term);
         end;
      end loop;

      return Result;
   end Interpolate_Points;

   ---------------------------------------------------------------------------
   --  Reed-Solomon LDE
   ---------------------------------------------------------------------------

   procedure LDE (
      Evals    : Field_Array;
      Blowup   : Positive;
      Extended : out Field_Array
   ) is
      N         : constant Natural := Evals'Length;
      Log_N     : Natural := 0;
      Coeffs    : Field_Array (0 .. N - 1);
      Dom       : Domain;
      Ext_Dom   : Domain;
      Temp      : Natural := N;
   begin
      --  Compute log2(N)
      while Temp > 1 loop
         Log_N := Log_N + 1;
         Temp := Temp / 2;
      end loop;

      --  Interpolate
      Dom := Create_Domain (Log_N);
      IFFT (Evals, Dom, Coeffs);

      --  Pad coefficients to extended size
      declare
         Ext_N     : constant Natural := N * Blowup;
         Ext_Log_N : Natural := 0;
         Ext_Coeffs: Field_Array (0 .. Ext_N - 1);
      begin
         Temp := Ext_N;
         while Temp > 1 loop
            Ext_Log_N := Ext_Log_N + 1;
            Temp := Temp / 2;
         end loop;

         Ext_Coeffs := (others => Zero);
         Ext_Coeffs (0 .. N - 1) := Coeffs;

         --  Evaluate on extended domain
         Ext_Dom := Create_Coset (Ext_Log_N, Generator);
         Coset_FFT (Ext_Coeffs, Ext_Dom, Extended);
      end;
   end LDE;

   ---------------------------------------------------------------------------
   --  Quotient Polynomial
   ---------------------------------------------------------------------------

   function Compute_Quotient (
      P : Polynomial;
      Z : Field_Element
   ) return Polynomial is
      --  Q(x) = (P(x) - P(z)) / (x - z)
      Numerator : Polynomial := P;
      Divisor   : Polynomial;
      P_Z       : constant Field_Element := Poly_Eval (P, Z);
      Quotient  : Polynomial;
      Remainder : Polynomial;
   begin
      --  Subtract P(z) from constant term
      Numerator.Coeffs (0) := Sub (Numerator.Coeffs (0), P_Z);

      --  Build divisor (x - z)
      Divisor.Coeffs := (others => Zero);
      Divisor.Coeffs (0) := Neg (Z);
      Divisor.Coeffs (1) := One;
      Divisor.Degree := 1;

      --  Divide
      Poly_Div (Numerator, Divisor, Quotient, Remainder);

      return Quotient;
   end Compute_Quotient;

   ---------------------------------------------------------------------------
   --  Vanishing Polynomial
   ---------------------------------------------------------------------------

   function Vanishing_Poly (Dom : Domain) return Polynomial is
      P : Polynomial;
   begin
      --  Z_H(x) = x^n - 1
      P.Coeffs := (others => Zero);
      P.Coeffs (0) := Neg (One);
      P.Coeffs (Dom.Size) := One;
      P.Degree := Dom.Size;
      return P;
   end Vanishing_Poly;

   function Eval_Vanishing (Dom : Domain; X : Field_Element) return Field_Element is
   begin
      --  Z_H(x) = x^n - 1
      return Sub (Exp (X, Unsigned_64 (Dom.Size)), One);
   end Eval_Vanishing;

   ---------------------------------------------------------------------------
   --  Commitment
   ---------------------------------------------------------------------------

   function Commit_Evaluations (
      Evals : Evaluations
   ) return Byte_Array_32 is
      Hash : Byte_Array_32;
      Data : Byte_Array (0 .. Evals.Count * 8 - 1);
   begin
      --  Convert evaluations to bytes
      for I in 0 .. Evals.Count - 1 loop
         declare
            E_Bytes : constant Byte_Array_8 := To_Bytes (Evals.Values (I));
         begin
            Data (I * 8 .. I * 8 + 7) := E_Bytes;
         end;
      end loop;

      Anubis_SHA3.SHA3_256 (Data, Hash);
      return Hash;
   end Commit_Evaluations;

end Anubis_STARK_Poly;
