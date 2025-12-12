pragma SPARK_Mode (On);

package body Anubis_Keccak with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Theta: Linear mixing of columns (FIPS 202 Section 3.2.1)
   --
   --  Algorithm:
   --    C[x] = A[x,0] XOR A[x,1] XOR A[x,2] XOR A[x,3] XOR A[x,4]
   --    D[x] = C[x-1 mod 5] XOR ROL(C[x+1 mod 5], 1)
   --    A'[x,y] = A[x,y] XOR D[x]
   --
   --  Cryptographic properties:
   --    - Linear transformation preserving XOR relationships
   --    - Each output bit depends on 11 input bits (full diffusion)
   --    - Constant-time: no data-dependent branches or memory accesses
   ---------------------------------------------------------------------------
   procedure Theta (State : in out State_Array) is
      C : array (Lane_Index) of Lane;
      D : array (Lane_Index) of Lane := (others => 0);
   begin
      --  Step 1: Compute column parity C[x] = XOR of all lanes in column x
      for X in Lane_Index loop
         pragma Loop_Invariant (X >= Lane_Index'First);
         pragma Loop_Invariant (State_Is_Valid (State));
         C (X) := State (X, 0) xor State (X, 1) xor State (X, 2) xor
                  State (X, 3) xor State (X, 4);
      end loop;

      --  Step 2: Compute D[x] and apply to all lanes in column
      for X in Lane_Index loop
         pragma Loop_Invariant (X >= Lane_Index'First);
         pragma Loop_Invariant (State_Is_Valid (State));

         --  D[x] = C[x-1 mod 5] XOR ROL(C[x+1 mod 5], 1)
         --  Indices wrap modulo 5 (Lane_Index range 0..4)
         declare
            X_Minus_1 : constant Lane_Index := (if X = 0 then 4 else X - 1);
            X_Plus_1  : constant Lane_Index := (if X = 4 then 0 else X + 1);
            C_As_U64  : constant Unsigned_64 := Unsigned_64 (C (X_Plus_1));
            C_Rotated : constant Lane := Lane (Rotate_Left (C_As_U64, 1));
         begin
            D (X) := C (X_Minus_1) xor C_Rotated;

            --  Apply D[x] to all lanes in this column (all y values)
            for Y in Lane_Index loop
               pragma Loop_Invariant (Y >= Lane_Index'First);
               pragma Loop_Invariant (State_Is_Valid (State));
               State (X, Y) := State (X, Y) xor D (X);
            end loop;
         end;
      end loop;
   end Theta;

   ---------------------------------------------------------------------------
   --  Rho: Lane rotations (FIPS 202 Section 3.2.2)
   --
   --  Algorithm: A'[x,y] = ROL(A[x,y], Rho_Offsets[x,y])
   --
   --  Cryptographic properties:
   --    - Bit permutation within each lane
   --    - Rotation amounts are compile-time constants
   --    - Constant-time: fixed rotation per lane position
   --    - Invertible: rotate right by same amount
   ---------------------------------------------------------------------------
   procedure Rho (State : in Out State_Array) is
   begin
      for X in Lane_Index loop
         pragma Loop_Invariant (X >= Lane_Index'First);
         pragma Loop_Invariant (State_Is_Valid (State));
         for Y in Lane_Index loop
            pragma Loop_Invariant (Y >= Lane_Index'First);
            pragma Loop_Invariant (State_Is_Valid (State));
            declare
               Lane_As_U64 : constant Unsigned_64 := Unsigned_64 (State (X, Y));
               Rotated : constant Unsigned_64 := Rotate_Left (Lane_As_U64, Rho_Offsets (X, Y));
            begin
               State (X, Y) := Lane (Rotated);
            end;
         end loop;
      end loop;
   end Rho;

   ---------------------------------------------------------------------------
   --  Pi: Lane permutation (FIPS 202 Section 3.2.3)
   --
   --  Algorithm: A'[x,y] = A[(x + 3*y) mod 5, x]
   --
   --  Cryptographic properties:
   --    - Bijective permutation of lane positions
   --    - Spreads lanes across rows for better diffusion
   --    - Constant-time: computed from constant indices
   ---------------------------------------------------------------------------
   procedure Pi (State : in Out State_Array) is
      Temp : State_Array := State;
   begin
      for X in Lane_Index loop
         pragma Loop_Invariant (X >= Lane_Index'First);
         pragma Loop_Invariant (State_Is_Valid (State));
         for Y in Lane_Index loop
            pragma Loop_Invariant (Y >= Lane_Index'First);
            pragma Loop_Invariant (State_Is_Valid (State));

            --  Compute source coordinates
            --  x' = (x + 3*y) mod 5
            --  y' = x
            declare
               Three_Y : constant Natural := 3 * Natural (Y);
               X_Prime_Nat : constant Natural := (Natural (X) + Three_Y) mod 5;
               X_Prime : constant Lane_Index := Lane_Index (X_Prime_Nat);
               Y_Prime : constant Lane_Index := X;
            begin
               State (X, Y) := Temp (X_Prime, Y_Prime);
            end;
         end loop;
      end loop;
   end Pi;

   ---------------------------------------------------------------------------
   --  Chi: Nonlinear transformation (FIPS 202 Section 3.2.4)
   --
   --  Algorithm (per row):
   --    A'[x,y] = A[x,y] XOR ((NOT A[x+1 mod 5, y]) AND A[x+2 mod 5, y])
   --
   --  CRITICAL: This is the ONLY nonlinear operation in Keccak!
   --
   --  Cryptographic properties:
   --    - Algebraic degree 2 (quadratic)
   --    - Provides resistance to linear and differential cryptanalysis
   --    - Row-independent: each row transformed separately
   --    - Constant-time: same operations regardless of input values
   --
   --  WARNING: Common implementation mistake is to compute:
   --    A XOR (NOT B)  -- This is WRONG! Missing the AND with C
   --    Correct: A XOR ((NOT B) AND C)
   ---------------------------------------------------------------------------
   procedure Chi (State : in Out State_Array) is
      Row : array (Lane_Index) of Lane;
   begin
      for Y in Lane_Index loop
         pragma Loop_Invariant (Y >= Lane_Index'First);
         pragma Loop_Invariant (State_Is_Valid (State));

         --  Copy row to temporary
         for X in Lane_Index loop
            pragma Loop_Invariant (X >= Lane_Index'First);
            Row (X) := State (X, Y);
         end loop;

         --  Apply Chi transformation
         --  For each position x in the row:
         --    new[x] = old[x] XOR ((NOT old[x+1]) AND old[x+2])
         for X in Lane_Index loop
            pragma Loop_Invariant (X >= Lane_Index'First);
            pragma Loop_Invariant (State_Is_Valid (State));

            declare
               X_Plus_1 : constant Lane_Index := (if X = 4 then 0 else X + 1);
               X_Plus_2 : constant Lane_Index := (case X is
                  when 0 => 2, when 1 => 3, when 2 => 4, when 3 => 0, when 4 => 1);

               --  Compute (NOT A[x+1]) AND A[x+2]
               Not_Next : constant Lane := not Row (X_Plus_1);
               Next_Next : constant Lane := Row (X_Plus_2);
               Chi_Term : constant Lane := Not_Next and Next_Next;
            begin
               --  A'[x] = A[x] XOR Chi_Term
               State (X, Y) := Row (X) xor Chi_Term;
            end;
         end loop;
      end loop;
   end Chi;

   ---------------------------------------------------------------------------
   --  Iota: Round constant injection (FIPS 202 Section 3.2.5)
   --
   --  Algorithm: A'[0,0] = A[0,0] XOR RC[round]
   --
   --  Cryptographic properties:
   --    - Breaks rotational symmetry of the permutation
   --    - Only modifies lane (0,0); all other lanes unchanged
   --    - Round constants derived from degree-8 LFSR
   --    - Prevents slide attacks and rotational cryptanalysis
   ---------------------------------------------------------------------------
   procedure Iota (State : in Out State_Array; Round : Round_Index) is
   begin
      State (0, 0) := State (0, 0) xor Round_Constants (Round);
   end Iota;

   ---------------------------------------------------------------------------
   --  Keccak-f[1600]: 24-round cryptographic permutation
   --
   --  Algorithm: For round = 0 to 23:
   --               State := Iota(Chi(Pi(Rho(Theta(State)))))
   --
   --  Security Properties (FIPS 202):
   --    - 1600-bit state provides 800-bit security (birthday bound)
   --    - Differential probability: <= 2^-509 (24 rounds)
   --    - Linear probability: <= 2^-510 (24 rounds)
   --    - No known practical attacks
   --
   --  Implementation Properties:
   --    - Constant-time: no data-dependent branches or memory accesses
   --    - Deterministic: identical inputs always produce identical outputs
   --    - Memory-safe: no dynamic allocation, bounded stack usage
   --    - Formally verified: SPARK proven free of runtime errors
   ---------------------------------------------------------------------------
   procedure Keccak_F (State : in Out State_Array) is
   begin
      for Round in Round_Index loop
         pragma Loop_Invariant (Round >= Round_Index'First);
         pragma Loop_Invariant (State_Is_Valid (State));

         Theta (State);
         Rho (State);
         Pi (State);
         Chi (State);
         Iota (State, Round);
      end loop;
   end Keccak_F;

end Anubis_Keccak;
