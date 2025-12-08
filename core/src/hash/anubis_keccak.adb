pragma SPARK_Mode (On);

package body Anubis_Keccak with
   SPARK_Mode => On
is

   --  Theta: Linear mixing of columns
   --  C[x] = A[x,0] ⊕ A[x,1] ⊕ A[x,2] ⊕ A[x,3] ⊕ A[x,4]
   --  D[x] = C[x-1] ⊕ ROT(C[x+1], 1)
   --  A"[x,y] = A[x,y] ⊕ D[x]
   procedure Theta (State : in out State_Array) is
      C : array (Lane_Index) of Lane;
      D : array (Lane_Index) of Lane := (others => 0);
   begin
      --  Compute column parity
      for X in Lane_Index loop
         pragma Loop_Invariant (X in Lane_Index);
         C (X) := State (X, 0) xor State (X, 1) xor State (X, 2) xor
                  State (X, 3) xor State (X, 4);
      end loop;

      --  Compute diffusion and apply
      for X in Lane_Index loop
         pragma Loop_Invariant (X in Lane_Index);

         --  D[x] = C[x-1] ⊕ ROT(C[x+1], 1)
         --  Indices wrap modulo 5
         declare
            X_Minus_1 : constant Lane_Index := (if X = 0 then 4 else X - 1);
            X_Plus_1  : constant Lane_Index := (if X = 4 then 0 else X + 1);
            C_As_U64  : constant Unsigned_64 := Unsigned_64 (C (X_Plus_1));
            C_Rotated : constant Lane := Lane (Rotate_Left (C_As_U64, 1));
         begin
            D (X) := C (X_Minus_1) xor C_Rotated;

            --  Apply to all lanes in this column
            for Y in Lane_Index loop
               pragma Loop_Invariant (Y in Lane_Index);
               State (X, Y) := State (X, Y) xor D (X);
            end loop;
         end;
      end loop;
   end Theta;

   --  Rho: Rotate each lane by fixed offset
   procedure Rho (State : in out State_Array) is
   begin
      for X in Lane_Index loop
         pragma Loop_Invariant (X in Lane_Index);
         for Y in Lane_Index loop
            pragma Loop_Invariant (Y in Lane_Index);
            declare
               Lane_As_U64 : constant Unsigned_64 := Unsigned_64 (State (X, Y));
               Rotated : constant Unsigned_64 := Rotate_Left (Lane_As_U64, Rho_Offsets (X, Y));
            begin
               State (X, Y) := Lane (Rotated);
            end;
         end loop;
      end loop;
   end Rho;

   --  Pi: Permute lane positions
   --  A"[x,y] = A[x",y"] where x = y" and y = (2x" + 3y") mod 5
   --  Inverse: x" = (x + 3y) mod 5, y" = x
   procedure Pi (State : in out State_Array) is
      Temp : State_Array := State;
   begin
      for X in Lane_Index loop
         pragma Loop_Invariant (X in Lane_Index);
         for Y in Lane_Index loop
            pragma Loop_Invariant (Y in Lane_Index);

            --  Compute source coordinates
            --  x" = (x + 3y) mod 5
            --  y" = x
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

   --  Chi: Nonlinear layer - THE CRITICAL STEP!
   --  A"[x,y] = A[x,y] ⊕ ((¬A[x+1,y]) ∧ A[x+2,y])
   --
   --  This is NOT: A[x,y] ∧ ¬B ∨ ¬A[x,y] ∧ B (that"s XOR)
   --  This is: A[x,y] ⊕ (¬A[x+1,y] ∧ A[x+2,y])
   --
   --  Process each row independently
   procedure Chi (State : in out State_Array) is
      Row : array (Lane_Index) of Lane;
   begin
      for Y in Lane_Index loop
         pragma Loop_Invariant (Y in Lane_Index);

         --  Copy row to temporary
         for X in Lane_Index loop
            pragma Loop_Invariant (X in Lane_Index);
            Row (X) := State (X, Y);
         end loop;

         --  Apply Chi transformation
         --  For each position x in the row:
         --    new[x] = old[x] ⊕ (¬old[x+1] ∧ old[x+2])
         for X in Lane_Index loop
            pragma Loop_Invariant (X in Lane_Index);

            declare
               X_Plus_1 : constant Lane_Index := (if X = 4 then 0 else X + 1);
               X_Plus_2 : constant Lane_Index := (case X is
                  when 0 => 2, when 1 => 3, when 2 => 4, when 3 => 0, when 4 => 1);

               --  Compute (NOT A[x+1]) AND A[x+2]
               Not_Next : constant Lane := not Row (X_Plus_1);
               Next_Next : constant Lane := Row (X_Plus_2);
               Chi_Term : constant Lane := Not_Next and Next_Next;
            begin
               --  A"[x] = A[x] XOR Chi_Term
               State (X, Y) := Row (X) xor Chi_Term;
            end;
         end loop;
      end loop;
   end Chi;

   --  Iota: Add round constant to break symmetry
   procedure Iota (State : in out State_Array; Round : Round_Index) is
   begin
      State (0, 0) := State (0, 0) xor Round_Constants (Round);
   end Iota;

   --  Keccak-f[1600]: 24 rounds of θ ρ π χ ι
   procedure Keccak_F (State : in out State_Array) is
   begin
      for Round in Round_Index loop
         pragma Loop_Invariant (Round in Round_Index);

         Theta (State);
         Rho (State);
         Pi (State);
         Chi (State);
         Iota (State, Round);
      end loop;
   end Keccak_F;

end Anubis_Keccak;
